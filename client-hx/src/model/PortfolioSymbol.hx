package model;
import promhx.Stream;
import promhx.Deferred;
import promhx.base.EventLoop;

typedef PortfolioSymbolT =  {
	  var crudType : String;
	  var commandType : String;
	  var portfolioID : String;
	  var symbol : String;
	  var quantity : String;
	  var side : String;
	  var symbolType : String;
	  var createdBy : String;
	  var updatedBy : String;
	  var nickName : String;
}

typedef PortfolioSymbolTypeQuery = {
		var nickName : String;
		var commandType : String;
		var symbolTypes : Array<Dynamic>;
}

typedef PortfolioSymbolSideQuery =  {
	var nickName : String;
	var commandType : String;
	var symbolSides : Array<Dynamic>;
}

typedef SymbolSide = {
	var symbolSide : String;
}
typedef SymbolType = {
	var symbolType : String;
}

class PortfolioSymbol  {
	public var portfolioSymbolT (null, default) : PortfolioSymbolT;
	//Plural names represent the collection, the 
	//singular represent the actual element.
	public var sidesStream(default, null) : Deferred<Dynamic>;
	public var typesStream(default, null) : Deferred<Dynamic>;
	public var sideStream (default, null) : Deferred<SymbolSide>;
	public var typeStream (default, null) : Deferred<SymbolType>;

	public function new() {
		trace("Creating portfolio symbol");
		sidesStream = new Deferred<Dynamic>();
		typesStream = new Deferred<Dynamic>();
		typeStream = new Deferred<SymbolType>();
		sideStream = new Deferred<SymbolSide>();
		sendPortfolioSymbolSideQuery();
		sendPortfolioSymbolTypeQuery();
		sidesStream.then(handleSymbolSideQuery);
		typesStream.then(handleSymbolTypeQuery);
	}

	private function sendPortfolioSymbolSideQuery() {
		var payload : PortfolioSymbolSideQuery =
			{
				nickName : MBooks_im.getSingleton().getNickName()
				, commandType : "PortfolioSymbolSidesQuery"
				, symbolSides : []
			};
		MBooks_im.getSingleton().doSendJSON(payload);
	}

	private function sendPortfolioSymbolTypeQuery () {
		var payload : PortfolioSymbolTypeQuery = 
			{
				nickName : MBooks_im.getSingleton().getNickName()
				, commandType : "PortfolioSymbolTypesQuery"
				, symbolTypes : []
			};
		MBooks_im.getSingleton().doSendJSON(payload);
	}
	public function handleSymbolSideQuery(incomingMessage : PortfolioSymbolSideQuery) {
		trace("Handle portfolio symbol side query" + incomingMessage);
		var resultSet : Array<Dynamic> = incomingMessage.symbolSides;
		for (optionSymbolSide in resultSet){
			var payload : SymbolSide = {
				symbolSide : optionSymbolSide
			};
			sideStream.resolve(payload);
		}
	}
	public function handleSymbolTypeQuery(incomingMessage : PortfolioSymbolTypeQuery) {
		trace ("Handle portfolio symbol type query " + incomingMessage);
		var resultSet : Array<Dynamic> = incomingMessage.symbolTypes;
		for (optionSymbolType in resultSet) {
			trace("Resolving " + optionSymbolType);
			var p : SymbolType  = {
				symbolType : optionSymbolType
			};
			typeStream.resolve(p);
		}
	}
	public function handlePortfolioSymbolTypeQuery(incomingMessage : PortfolioSymbolTypeQuery){
		trace("Handle portfolio symbol type query");
		var resultSet : Array<Dynamic> = incomingMessage.symbolTypes;
	}

	
}
