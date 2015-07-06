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


typedef PortfolioStruct = {
	var symbol: String;
	var side : String;
	var quantity : Float;		
};

typedef PortfolioPayload = {
	var crudType : String;
	var struct : Array<PortfolioStruct>;
}

class Portfolio {
	public var symbol (null, default) : String;
	public var side (null, default) : String;
	public var quantity (null, default) : Float;
	public var readerStream (null, default) : Deferred<PortfolioStruct>;
	public var writerStream (null, default) : Deferred<PortfolioPayload>;


	public function new(sym : String, sid : String, q : Float) {
		symbol = sym;
		side = sid;
		quantity = q;
		readerStream = new Deferred<PortfolioStruct>();
		readerStream.then(readPayload);
		writerStream = new Deferred<PortfolioPayload>();
		writerStream.then(writePayload);

	}
	//Manage portfolio.
	public function save(portfolio : PortfolioStruct) {
		trace("Saving portfolio");
	}
	//Read values from the stream.
	public function readPayload(payload : PortfolioStruct) {
		trace("Overwriting values with payload");
		this.symbol = payload.symbol;
		this.side = payload.side;
		this.quantity = payload.quantity;

	}
	private function writePayload(payload : PortfolioPayload){
		trace("Writing " + payload);
		MBooks_im.getSingleton().doSendJSON(payload);
	}
	public function insert(p : Array<PortfolioStruct>) {
		var payload = {
			  crudType : "Insert"
			, struct : p
		};
		writePayload(payload);
	}

	public function update(p : Array<PortfolioStruct>){
		var payload  = {
			crudType : "Update"
			, struct : p 
		};
		writePayload(payload);
	}
	public function delete(p : Array<PortfolioStruct>){
		var payload = {
			crudType : "Delete"
			, struct : p
		}
		writePayload(payload);
	}

}
