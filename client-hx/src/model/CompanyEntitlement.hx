package model;

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


//crudType is one of "Create", "C_Update", "Delete", "Read"

typedef QueryCompanyEntitlement = {
	var nickName : String;
	var queryParameters : String;
	var commandType : String;
	var resultSet : Array<CompanyEntitlementT>;
}


typedef CompanyEntitlementT = {
	var nickName : String;
	var crudType : String;
	var commandType : String;
	var userId : String;
	var tabName : String;
	var sectionName : String;
	}


typedef QueryCompanyUsers = {
	var nickName : String;
	var commandType : String;
	var companyID : String;
	var users : Array<Person>;
}
class CompanyEntitlement {
	private var payload : CompanyEntitlementT;

	public function new(stream : Deferred<CompanyEntitlementT>){
		Util.log("Creating new entitlement");
		stream.then(updateModel);
	}
	private function updateModel(anEntitlement : CompanyEntitlementT){
		trace("Updating model " + anEntitlement);
		MBooks_im.getSingleton().doSendJSON(anEntitlement);
	}
	
	public function queryAllEntitlements() {
		trace("Query all the entitlements");
		var queryEntitlements : QueryCompanyEntitlement = 
			{
				nickName : MBooks_im.getSingleton().getNickName()
				, queryParameters : "*"
				, commandType : "QueryCompanyEntitlements"
				, resultSet : new Array<CompanyEntitlementT>()
			}
		MBooks_im.getSingleton().doSendJSON(queryEntitlements);
	}
	public static function addUserEntitlement(userNickName : String, entitlementId : String){
		trace("Adding user entitlement for " + userNickName + " -> " + entitlementId);
	}	

}