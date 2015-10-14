package view;
import haxe.Json;
import js.html.Event;
import js.Browser;
import js.html.Element;
import js.html.InputElement;
import js.html.TextAreaElement;
import js.html.DOMCoreException;
import js.html.Document;
import js.html.ButtonElement;
import js.html.DivElement;
import js.html.UListElement;
import js.html.LIElement;
import js.html.SelectElement;
import js.html.KeyboardEvent;
import js.html.OptionElement;
import js.html.TableElement;
import js.html.TableCellElement;
import js.html.TableRowElement;
import js.html.HTMLCollection;
import js.html.FileReader;
import haxe.ds.ObjectMap;
import haxe.ds.StringMap;
import promhx.Stream;
import promhx.Deferred;
import promhx.base.EventLoop;
import model.Portfolio;
import model.PortfolioSymbol;
import model.Company;
import js.Lib.*;
import util.*;
import model.Entitlement;
import view.ListManager;

/*
* Client --------------------------------------------> Server
* Save entitlement clicked
* 				send manage entitlement command -----> 
													 	Process manage entitlement command
	<---------------------------------------------------
	Singleton routes the command to stream
	View handles incoming stream event 
			Clears fields
	Updates entitlement list.
*/
class Entitlement {

	private static var TAB_NAME : String = "entitlementTabName";
	private static var SECTION_NAME : String= "entitlementSectionName";
	private static var ENTITLEMENT_LIST  : String = "entitlementsList";
	private static var ADD_ENTITLEMENT  : String = "addEntitlement" ;
	private static var UPDATE_ENTITLEMENT : String = "updateEntitlement";
	private static var REMOVE_ENTITLEMENT : String = "removeEntitlement";
	private static var MANAGE_ENTITLEMENTS_COMMAND : String = "ManageEntitlements";
	private var tabNameElement (default, null) : InputElement;
	private var tabName (default, null): String;
	private var sectionNameElement (default, null) : InputElement;
	private var sectionName (default, null) : String;
	private var addEntitlementButton : ButtonElement;
	private var updateEntitlementButton : ButtonElement;
	private var deleteEntitlementButton : ButtonElement;
	private var entitlementsList : SelectElement;
	public var queryEntitlementResponse(default, null) : Deferred<model.QueryEntitlement>;
	public var modelResponseStream(default, null) : Deferred<model.EntitlementT>;
	public var modelStream(default, null) : Deferred<model.EntitlementT>;
	public var view (default, null) : Deferred<model.EntitlementT>;
	
	private var modelObject : model.Entitlement;
	private var textFields : List<InputElement>;
	private var entitlementMap : Map<String, model.EntitlementT>;
	public function new(){
		trace("Creating Entitlement view");
		tabNameElement = cast Browser.document.getElementById(TAB_NAME);
		if(tabNameElement == null){
			throw ("Element not found " + TAB_NAME) ;
		}
		tabName = tabNameElement.value;
		sectionNameElement = cast Browser.document.getElementById(SECTION_NAME);
		if(sectionNameElement == null){
			throw ("Element not found " + SECTION_NAME);
		}
		entitlementMap = new Map<String, model.EntitlementT>();
		sectionName = sectionNameElement.value;
		textFields = new List<InputElement>();
		textFields.add(sectionNameElement);
		textFields.add(tabNameElement);

		entitlementsList = cast Browser.document.getElementById(ENTITLEMENT_LIST);
		if(entitlementsList == null){
			throw ("Element not found  " + ENTITLEMENT_LIST);
		}

		//changes from external source.
		modelStream = new Deferred<model.EntitlementT>();
		//changes, external sources are interested in 
		//listening to.
		view = new Deferred<EntitlementT>();
		modelResponseStream = new Deferred<model.EntitlementT>();
		modelResponseStream.then(handleModelResponse);
		modelObject = new model.Entitlement(modelStream);

		queryEntitlementResponse = new Deferred<model.QueryEntitlement>();
		queryEntitlementResponse.then(handleQueryEntitlementResponse);
		addEntitlementButton = cast (Browser.document.getElementById(ADD_ENTITLEMENT));
		updateEntitlementButton = cast (Browser.document.getElementById(UPDATE_ENTITLEMENT));
		deleteEntitlementButton = cast (Browser.document.getElementById(REMOVE_ENTITLEMENT));
		setupStreams();
	}	

	public function queryAllEntitlements(){
		modelObject.queryAllEntitlements();
	}

	//TODO: Refactor.
	public function setupStreams(){
		addEntitlementButton.addEventListener("click", addEntitlementEvent);
		updateEntitlementButton.addEventListener("click", updateEntitlementEvent);
		deleteEntitlementButton.addEventListener("click", deleteEntitlementEvent);
	}


	private function getModelEntitlement(aCrudType){
		var change : model.EntitlementT = {
			crudType : aCrudType
			, commandType  : MANAGE_ENTITLEMENTS_COMMAND
			, tabName : tabNameElement.value
			, sectionName  : sectionNameElement.value
			, nickName : MBooks_im.getSingleton().getNickName()
		};
		return change;
	}	
	private function addEntitlementEvent(ev : Event){
		trace("Add entitlement clicked");
		var change : model.EntitlementT = getModelEntitlement("Create");
		modelStream.resolve(change);
	}
	private function updateEntitlementEvent(ev : Event){
		trace("Update entitlement clicked");
		var change = getModelEntitlement("C_Update");
		modelStream.resolve(change);

	}
	private function deleteEntitlementEvent(ev : Event){
		trace("Deleting entitlement ");
		var change  = getModelEntitlement("Delete");
		modelStream.resolve(change);
	}

	private function setTabName(aTabName : String) {
		tabName = aTabName;
	}
	public function setSectionName(aName : String){
		sectionName = aName;
	}

	private function incomingMessageNull(source : String)  {
		MBooks_im.getSingleton().incomingMessageNull(source);
	}
	private function handleQueryEntitlementResponse(incoming : Dynamic){
		if(incoming == null){
			incomingMessageNull("QueryEntitlement");
			return;
		}if(incoming.Left != null){
			MBooks_im.getSingleton().applicationErrorStream.resolve(incoming);
		}else if(incoming.Right != null){
			updateEntitlementList(incoming.Right);
		}		
	}

	private function handleModelResponse(incoming : Dynamic) {
		trace("handling model response");
		if(incoming == null){
			incomingMessageNull("ModelResponse");
			return;
		}
		if(incoming.Left != null){
			MBooks_im.getSingleton().applicationErrorStream.resolve(incoming);
		}else if(incoming.Right != null){
			updateSelf(incoming.Right);
		}
	}
	

	private function updateEntitlementList(queryEntitlement : model.QueryEntitlement){
		//Delete all the elements in the list and then add
		//the result.
		var entitlementList : Array<EntitlementT> = queryEntitlement.resultSet;
		deleteFromEntitlementList();
		for(entitlement in entitlementList){
			updateIntoView(entitlement);
		}
	}

	private function updateSelf(entitlement : model.EntitlementT){
		trace("Updating view " +  entitlement);
		//If the crud type is Delete, then remove the element
		//from the list. Pick the next element,
		//replace the values with the values in that element.
		//If the crud type is update, replace the values on the view
		//with the values in the model.
		if (entitlement.crudType == "Delete") {
			deleteFromView(entitlement);
		}else {
			updateIntoView(entitlement);
		}
	}

	private function deleteFromView(entitlement : model.EntitlementT){
		//delete from the view
		//clear text fields
		clearTextFields();
		//update the selection element.
		removeFromList(entitlement);
	}

	private function updateIntoView(entitlement : model.EntitlementT) {
		//update into the view
		//or insert into the view.
		clearTextFields();
		updateList(entitlement);
		updateTextFields(entitlement);
	}

	private function clearTextFields(){
		for (i in textFields){
			i.value = "";
		}
	}
	
	private function updateTextFields(entitlement : model.EntitlementT){
		sectionNameElement.value = entitlement.sectionName;
		tabNameElement.value = entitlement.tabName;		
	}

	private function getOptionElementKey(entitlement : model.EntitlementT){
		trace("Creating an option element key");
		var optionElementKey : String = 
				MANAGE_ENTITLEMENTS_COMMAND + entitlement.tabName + entitlement.sectionName;
		return optionElementKey;
	}

	private function printListText(entitlement){
		return entitlement.tabName + "->" + entitlement.sectionName;
	}

	private function updateList(entitlement : model.EntitlementT){
		trace("Adding element to list");
		var optionElementKey = getOptionElementKey(entitlement);
		var optionElement : OptionElement = cast (Browser.document.getElementById(optionElementKey));

		if(optionElement == null){
			entitlementMap[optionElementKey] = entitlement;
			optionElement = cast (Browser.document.createOptionElement());
			optionElement.id = optionElementKey;
			optionElement.text = printListText(entitlement);
			var stream = 
				MBooks_im.getSingleton().initializeElementStream(
					cast optionElement
					, "click"
				);
			stream.then(handleEntitlementSelected);
			entitlementsList.appendChild(optionElement);		

		}else {
			optionElement.text = printListText(entitlement);
		}
		optionElement.selected = true;

	}
	
	private function removeFromList(entitlement : model.EntitlementT){
		trace("Removing element from list");
		var optionElementKey = getOptionElementKey(entitlement);
		removeElementFromList(optionElementKey);
	}

	private function removeElementFromList(id : String){

		var optionElement :OptionElement = cast (Browser.document.getElementById(id));
		if(optionElement == null){
			throw ("Nothing to delete " + id);
		}
		optionElement.parentNode.removeChild(optionElement);
		trace("The above code should most likely work");
		for(entitlement in entitlementMap) {
			var optionElementKey = getOptionElementKey(entitlement);
			var optionElement : OptionElement = cast (Browser.document.getElementById(optionElementKey));
			if(optionElement != null){
				optionElement.selected = true;
				updateTextFields(entitlement);
				break;
			}
		}

	}
	private function deleteFromEntitlementList(){
		try {
			for(entitlement in entitlementMap){
				removeElementFromList(getOptionElementKey(entitlement));
			}
			entitlementMap = new Map<String, model.EntitlementT>();
		}catch(e : Dynamic){
			trace("Exception deleting elements from the list. Restore to previous view." 
				+ e);

		}
	}

	private function handleEntitlementSelected(ev : Event){
		var element : OptionElement = cast ev.target;
		var optionElementKey = element.id;
		var entitlement : model.EntitlementT = entitlementMap[optionElementKey];
		if(entitlement == null){
			throw ("Entitlement not found");
		}
		entitlement.crudType = "Read";
		if (entitlement.nickName == null){
			entitlement.nickName = MBooks_im.getSingleton().getNickName();
		}
		if (entitlement.commandType == null){
			entitlement.commandType = MANAGE_ENTITLEMENTS_COMMAND;
		}
		modelStream.resolve(entitlement);	
	}

}