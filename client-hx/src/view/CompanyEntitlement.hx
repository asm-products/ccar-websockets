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
import model.CompanyEntitlement; 
import model.Entitlement;


/**
* Manage entitlements for a company. 
* 
*/



class CompanyEntitlement {

	//Prefixes for the list manager.
	private static var MANAGE_ALL_USER_ENTS = "allUserEntitlements";
	private static var MANAGE_COMPANY_USER_ENTS = "companyUserEntitlements";

	private static var SEARCH_USER_ELEMENT : String = "searchUsers";
	private static var COMPANY_USERS : String = "companyUsers";
	private static var PENDING_APPROVAL_REQUESTS : 	String = "pendingApprovalRequests";
	private static var AVAILABLE_ENTITLEMENTS  : String = "availableEntitlements";
	private static var USER_ENTITLEMENTS : String = "userEntitlements";
	private static var ADD_USER_ENTITLEMENTS : String = "addUserEntitlements";
	private static var REMOVE_USER_ENTITLEMENTS : String = "removeUserEntitlements";

	private var addUserEntitlement : ButtonElement;
	private var removeUserEntitlement : ButtonElement;
	private var userEntitlementsList : SelectElement;
	private var entitlementsManager : ListManager<EntitlementT>;
	private var userListManager : ListManager<model.Person>;
	private var users : SelectElement;

	public var userListResponse(default, null) : Deferred<QueryCompanyUsers>;

	public function new (view : view.Entitlement
			, companyStream : Deferred<Dynamic> ){		
		userEntitlementsList = cast (Browser.document.getElementById(USER_ENTITLEMENTS));
		view.queryEntitlementResponse.then(handleQueryEntitlementResponse);
		entitlementsManager = 
				new ListManager<EntitlementT>(userEntitlementsList, 
					MANAGE_COMPANY_USER_ENTS, 
					Entitlement.optionId, Entitlement.listDisplay);
		users = cast (Browser.document.getElementById(COMPANY_USERS));
		userListManager = 
				new ListManager<model.Person>(
					users
					, COMPANY_USERS
					, model.Person.optionId 
					, model.Person.listDisplay
				);
		view.modelResponseStream.then(handleModelResponse);
		//active company stream 
		companyStream.then(getCompanyUsers);
		userListResponse = new Deferred<model.QueryCompanyUsers>();
		userListResponse.then(handleQueryCompanyUsers);
		addUserEntitlement = cast (Browser.document.getElementById(ADD_USER_ENTITLEMENTS));
		removeUserEntitlement = cast (Browser.document.getElementById(REMOVE_USER_ENTITLEMENTS));
	}	

	private function handleUserEntitlementSelect(userEv : Dynamic, entEv : Dynamic){
		var userList : SelectElement = cast userEv.target;
		var entList  : SelectElement  = cast entEv.target;
		
		for(user in userList.selectedOptions) {
			var u : OptionElement = cast user;
			trace("User " + u.id + " " + u.text);
			for(entE in entList.selectedOptions) {
				var ent : OptionElement = cast entE;
				trace("Ent " + ent.id + " " + ent.text);
			}
		}
	}
	private function handleUserListChange(ev : Dynamic){
		trace("Event received " + ev);
	}
	private function handleEntitlementsChange(ev : Dynamic){
		trace("Event received " + ev);

	}
	public function initializeStreams(){
		trace("Adding user entitlement stream");
		var addUserEntitlementStream : Stream<Dynamic> =
				MBooks_im.getSingleton().initializeElementStream(
					cast addUserEntitlement
					, "click"
			); 
		addUserEntitlementStream.then(addUserEntitlementF);	
		var removeUserEntitlementStream : Stream <Dynamic> = 
				MBooks_im.getSingleton().initializeElementStream(
				cast removeUserEntitlement 
				, "click"
				); 
		removeUserEntitlementStream.then(removeUserEntitlementF);
		var stream = MBooks_im.getSingleton().initializeElementStream(userListManager.listElement 
			, "change"
		);
		stream.then(handleUserListChange);
		var eStream = MBooks_im.getSingleton().initializeElementStream(entitlementsManager.listElement
			, "change");
		eStream.then(handleEntitlementsChange);
		Stream.whenever(stream, eStream).then(handleUserEntitlementSelect);

	}

	private function addUserEntitlementF(event : Dynamic) {
		trace("Add user entitlements " + event);
		for (userE in users) {
			var user : OptionElement = cast userE;
			for(entE in userEntitlementsList){
				var ent : OptionElement = cast entE;
				trace("Adding entitlements " + ent.id  + " to " + user.id);
				model.CompanyEntitlement.addUserEntitlement(user.id, ent.id);
			}
		}		
	}
	private function removeUserEntitlementF(event : Dynamic){
		trace("Remove user entitlement " + event);
	}
	private function getCompanyUsers(aCompanyId : Dynamic) {
		trace("Query all company users for " + aCompanyId);
		var queryCompanyUsers : QueryCompanyUsers = {
			nickName : MBooks_im.getSingleton().getNickName()
			, commandType : "QueryCompanyUsers"
			, companyID : aCompanyId
			, users : new Array<model.Person>()
		}
		MBooks_im.getSingleton().doSendJSON(queryCompanyUsers);

	}

	private function handleQueryCompanyUsers(incoming : Dynamic){
		trace("Handle query company users " + incoming);
		if(incoming == null){
			MBooks_im.getSingleton().incomingMessageNull("QueryEntitlement");
			return;
		}if(incoming.Left != null){
			MBooks_im.getSingleton().applicationErrorStream.resolve(incoming);
		}else if(incoming.Right != null){
			updateCompanyUsers(incoming.Right);
		}		
	}

	private function updateCompanyUsers(queryUserResult : model.QueryCompanyUsers){
		trace("Update company users list");
		for(user in queryUserResult.users){
			trace("Adding element to the list." + user);
			var stream = userListManager.add(user);
		}

	}


	private function handleQueryEntitlementResponse(incoming : Dynamic){
		trace("Query entitlements ");
		if(incoming == null){
			MBooks_im.getSingleton().incomingMessageNull("QueryEntitlement");
			return;
		}if(incoming.Left != null){
			MBooks_im.getSingleton().applicationErrorStream.resolve(incoming);
		}else if(incoming.Right != null){
			updateEntitlementList(incoming.Right);
		}		
	}


	private function updateEntitlementList(queryEntitlement : model.QueryEntitlement) {
		trace("Update entitlement list element");
		for(entitlement in queryEntitlement.resultSet){
			trace("Adding element to the list." + entitlement);			
			var stream = entitlementsManager.add(entitlement);
		}
	}

	private function handleModelResponse(incoming : Dynamic) {
		trace("handling model response");
		if(incoming == null){
			MBooks_im.getSingleton().incomingMessageNull("ModelResponse");
			return;
		}
		if(incoming.Left != null){
			MBooks_im.getSingleton().applicationErrorStream.resolve(incoming);
		}else if(incoming.Right != null){
			updateSelf(incoming.Right);
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
			entitlementsManager.delete(entitlement);
		}else {
			entitlementsManager.upsert(entitlement);
		}
	}


}
