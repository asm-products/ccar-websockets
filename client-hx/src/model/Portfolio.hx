package model;
import util.Util;
import haxe.Json;
import haxe.Utf8;
import haxe.Timer;
import haxe.ds.ArraySort;
import haxe.ds.GenericStack;
import model.Contact;
import model.Login;
import model.Person;
import model.LoginStatus;
import model.Command;
import model.CommandType;
import model.UserOperation;
import util.Util;
import util.Config;
import js.Browser;
import promhx.Stream;
import promhx.Promise;
using promhx.haxe.EventTools;
import promhx.Deferred;
import promhx.base.EventLoop;


typedef PortfolioT = {
	var crudType : String;
	var commandType : String;
	var portfolioId : String;
	var companyId : String;
	var userId : String;
	var summary : String;
	var createdBy : String;
	var updatedBy : String;
	var nickName : String;
}

typedef PortfolioQuery = {
	var commandType : String;
	var nickName : String;
	var companyId : String;
	var userId : String;
	var resultSet : Array<Dynamic>;
}


class Portfolio {
	public var portfolioT (null, default) : PortfolioT;
	public var readerStream (null, default) : Deferred<PortfolioT>;
	public var writerStream (null, default) : Deferred<PortfolioT>;

	public function new(crudType : String, portfolioId: String
			, companyId : String
			, userId : String 
			, summary : String 
			, createdBy : String 
			, updatedBy : String) {
			portfolioT = {
				crudType : crudType
				, commandType : "ManagePortfolio"
				, portfolioId : portfolioId
				, companyId : companyId
				, userId : userId 
				, summary : summary
				, createdBy : createdBy 
				, updatedBy : updatedBy
				, nickName : MBooks_im.getSingleton().getNickName()
			}

	}
	//Manage portfolio.
	public function save(portfolio : PortfolioT) {
		trace("Saving portfolio");
	}


}
