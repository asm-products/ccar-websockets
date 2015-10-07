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

typedef QueryEntitlement = {
	var nickName : String;
	var queryParameters : String;
	var commandType : String;
	var resultSet : Array<EntitlementT>;
}


typedef EntitlementT = {
	var nickName : String;
	var crudType : String;
	var commandType : String;
	var tabName : String;
	var sectionName : String;
	}

class Entitlement {
	private var payload : EntitlementT;

	public function new(stream : Deferred<EntitlementT>){
		Util.log("Creating new entitlement");
		stream.then(updateModel);
	}
	private function updateModel(anEntitlement : EntitlementT){
		trace("Updating model " + anEntitlement);
		MBooks_im.getSingleton().doSendJSON(anEntitlement);
	}
	
	public function queryAllEntitlements() {
		trace("Query all the entitlements");
		var queryEntitlements : QueryEntitlement = 
			{
				nickName : MBooks_im.getSingleton().getNickName()
				, queryParameters : "*"
				, commandType : "QueryEntitlements"
				, resultSet : null
			}
		MBooks_im.getSingleton().doSendJSON(queryEntitlements);
	}
	

}