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
	private var tabNameElement (default, null) : InputElement;
	private var tabName (default, null): String;
	private var sectionNameElement (default, null) : InputElement;
	private var sectionName (default, null) : String;
	private var addEntitlementButton : ButtonElement;
	private var updateEntitlementButton : ButtonElement;
	private var deleteEntitlementButton : ButtonElement;
	public var modelStream(default, null) : Deferred<model.EntitlementT>;
	public var view (default, null) : Deferred<model.EntitlementT>;
	private var modelObject : model.Entitlement;
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
		sectionName = sectionNameElement.value;
		//changes from external source.
		modelStream = new Deferred<model.EntitlementT>();
		//changes, external sources are interested in 
		//listening to.
		view = new Deferred<EntitlementT>();
		modelStream.then(updateSelf);
		modelObject = new model.Entitlement(modelStream);
		addEntitlementButton = cast (Browser.document.getElementById(ADD_ENTITLEMENT));
		updateEntitlementButton = cast (Browser.document.getElementById(UPDATE_ENTITLEMENT));
		deleteEntitlementButton = cast (Browser.document.getElementById(REMOVE_ENTITLEMENT));
		var addStream : Stream<Dynamic> = 
			MBooks_im.getSingleton().initializeElementStream(addEntitlementButton, "click");
		addStream.then(addEntitlementEvent);
		var updateStream : Stream<Dynamic> = 
			MBooks_im.getSingleton().initializeElementStream(updateEntitlementButton, "click");
		updateStream.then(updateEntitlementEvent);
		var deleteStream : Stream<Dynamic> =
			MBooks_im.getSingleton().initializeElementStream(deleteEntitlementButton, "click");
		deleteStream.then(deleteEntitlementEvent);

	}	
	private function addEntitlementEvent(ev : Event){
		trace("Add entitlement clicked");

	}
	private function updateEntitlementEvent(ev : Event){
		trace("Update entitlement clicked");
	}
	private function deleteEntitlementEvent(ev : Event){
		trace("Delete entitlement button clicked");
	}
	private function setTabName(aTabName : String) {
		tabName = aTabName;
	}
	public function setSectionName(aName : String){
		sectionName = aName;
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
	}
	private function updateIntoView(entitlement : model.EntitlementT) {
		//update into the view.
	}

}