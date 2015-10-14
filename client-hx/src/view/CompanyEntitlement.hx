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
	private static var ALL_COMPANY_USER_SEARCH_LIST : String = "allCompanyUsersSearchList";
	private static var PENDING_APPROVAL_REQUESTS : 	String = "pendingApprovalRequests";
	private static var AVAILABLE_ENTITLEMENTS  : String = "availableEntitlements";
	private static var USER_ENTITLEMENTS : String = "userEntitlements";
	private static var ADD_USER_ENTITLEMENTS : String = "addUserEntitlements";
	private static var REMOVE_USER_ENTITLEMENTS : String = "removeUserEntitlements";

	private var addUserEntitlement : ButtonElement;
	private var removeUserEntitlement : ButtonElement;
	private var userEntitlementsList : SelectElement;
	private var entitlementsManager : ListManager<EntitlementT>;
	public function new (view : view.Entitlement){
		userEntitlementsList = cast (Browser.document.getElementById(USER_ENTITLEMENTS));
		view.queryEntitlementResponse.then(handleQueryEntitlementResponse);
		entitlementsManager = new ListManager<EntitlementT>(userEntitlementsList, MANAGE_COMPANY_USER_ENTS, Entitlement.optionId, Entitlement.listDisplay);
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
			stream.then(entitlementAdded);
		}
	}
	private function entitlementAdded(ev : Event) {
		trace("Entitlement " + ev);
	}

}
