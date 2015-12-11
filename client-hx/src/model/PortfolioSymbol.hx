package model;
import promhx.Stream;
import promhx.Deferred;
import promhx.base.EventLoop;
import model.Portfolio;


typedef PortfolioSymbolT =  {
	  var crudType : String;
	  var commandType : String;
	  var portfolioId : String;
	  var symbol : String;
	  var quantity : String;
	  var side : String;
	  var symbolType : String;
	  var value : String;
	  var stressValue : String;
	  var creator : String;
	  var updator : String;
	  var nickName : String;
}

typedef PortfolioSymbolQueryT  = {
	var commandType : String;
	var portfolioId : String; 
	var nickName : String;
	var resultSet : Array<Dynamic>;
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


/**
	Plurals -> collection. 
	singular -> element.
*/
class PortfolioSymbol  {
	public var activePortfolio (default, null) : PortfolioT;
	/**
	* Stream processing sides for a trade.
	*/
	public var sidesStream(default, null) : Deferred<Dynamic>;

	/**
	* Product types: equity, options, bonds, futures, currencies
	* for example.
	*/
	public var typesStream(default, null) : Deferred<Dynamic>;
	public var sideStream (default, null) : Deferred<SymbolSide>;
	public var typeStream (default, null) : Deferred<SymbolType>;
	public var insertStream (default, null) : Deferred<PortfolioSymbolT>;
	public var updateStream (default, null) : Deferred<PortfolioSymbolT>;
	public var deleteStream (default, null) : Deferred<PortfolioSymbolT>;
	public var readStream (default, null) : Deferred<PortfolioSymbolT>;


	public function new() {
		trace("Creating portfolio symbol");
		sidesStream = new Deferred<Dynamic>();
		typesStream = new Deferred<Dynamic>();
		typeStream = new Deferred<SymbolType>();
		sideStream = new Deferred<SymbolSide>();
		insertStream = new Deferred<PortfolioSymbolT>();
		updateStream = new Deferred<PortfolioSymbolT>();
		deleteStream = new Deferred<PortfolioSymbolT>();
		readStream = new Deferred<PortfolioSymbolT>();
		sendPortfolioSymbolSideQuery();
		sendPortfolioSymbolTypeQuery();
		sidesStream.then(handleSymbolSideQuery);
		typesStream.then(handleSymbolTypeQuery);
		insertStream.then(sendPayload);
		updateStream.then(sendPayload);
		deleteStream.then(sendPayload);
		readStream.then(sendPayload);
		MBooks_im.getSingleton().portfolio.activePortfolioStream.then(processActivePortfolio);
	}

	private function processActivePortfolio(a : PortfolioT) {
		if(a == null){
			throw ("Active portfolio not defined " + a);
		}
		trace("Process active portfolio " + a);
		this.activePortfolio = a;
		sendPortfolioSymbolQuery();
	}

	private function sendPortfolioSymbolQuery() {
		if(this.activePortfolio == null){
			throw ("No active portfolio selected. Not fetching symbols");
		}
		var payload : PortfolioSymbolQueryT = {
			commandType : "QueryPortfolioSymbol"
			, portfolioId : activePortfolio.portfolioId
			, nickName : MBooks_im.getSingleton().getNickName()
			, resultSet : []
		}
		MBooks_im.getSingleton().doSendJSON(payload);
	}
	private function sendPayload(payload : PortfolioSymbolT) {
		trace("Processing sending payload "  + payload);
		MBooks_im.getSingleton().doSendJSON(payload);
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
}
