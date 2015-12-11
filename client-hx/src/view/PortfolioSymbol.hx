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
import model.MarketDataUpdate;
import js.Lib.*;
import util.*;
import format.csv.*;
import format.*;

typedef Field = String;
typedef Record = Array<Field>;
typedef Csv = Array<Record>;

class PortfolioSymbol {
	private static var SYMBOL_SIDE_LIST = "symbolSideID";
	private static var SYMBOL_TYPE_LIST = "symbolTypeID";
	private static var SYMBOL_ID_FIELD = "symbolID";
	private static var SYMBOL_QUANTITY_ID = "symbolQuantityID";
	private static var SAVE_SYMBOL_BUTTON = "saveSymbol";
	private static var DELETE_SYMBOL_BUTTON = "deleteSymbol";
	private static var UPDATE_SYMBOL_BUTTON = "updateSymbol";
	private static var PORTFOLIO_SYMBOL_TABLE = "portfolioSymbolTable";
	private static var UPLOAD_PORTFOLIO_FILE = "uploadPortfolioFile";
	private static var UPLOAD_PORTFOLIO_BUTTON =  "uploadPortfolioButton";



	private var insertStreamResponse(default, null) : Deferred<PortfolioSymbolT>;
	private var updateStreamResponse(default, null) : Deferred<PortfolioSymbolT>;
	private var deleteStreamResponse(default, null) : Deferred<PortfolioSymbolT>;
	private var readStreamResponse(default, null) : Deferred<PortfolioSymbolT>;
	public var symbolQueryResponse(default, null) : Deferred<PortfolioSymbolQueryT>;
	private var model : model.PortfolioSymbol;
	private var activePortfolio : PortfolioT;
	public function new(m : model.PortfolioSymbol){
		trace("Instantiating new portfolio symbol view");
		model = m;
		rowMap = new StringMap<TableRowElement>();
		setupStreams();
	}
	
	private function getUploadPortfolioFile() : InputElement {
		return (cast Browser.document.getElementById(UPLOAD_PORTFOLIO_FILE));
	}

	private function getUploadPortfolioButton() : ButtonElement {
		return (cast Browser.document.getElementById(UPLOAD_PORTFOLIO_BUTTON));
	}
	private function getPortfolioSymbolTable() : TableElement {
		return (cast Browser.document.getElementById(PORTFOLIO_SYMBOL_TABLE));
	}
	private function getDeletePortfolioSymbolButton() {
		return (cast Browser.document.getElementById(DELETE_SYMBOL_BUTTON));
	}
	private function getUpdatePortfolioSymbolButton() {
		return (cast Browser.document.getElementById(UPDATE_SYMBOL_BUTTON));
	}
	private function getInsertPortfolioSymbolButton() {
		return (cast Browser.document.getElementById(SAVE_SYMBOL_BUTTON));
	}

	private function getQuantityValueElement() : InputElement {
		return (cast Browser.document.getElementById(SYMBOL_QUANTITY_ID));
	}
	private function getQuantityValue() : String {
		return getQuantityValueElement().value;
	}
	private function setQuantityValue(aValue : String) {
		getQuantityValueElement().value = "";
	}
	private function getSymbolIdElement() : InputElement {
		return (cast Browser.document.getElementById(SYMBOL_ID_FIELD));
	}
	private function getSymbolIdValue()  {
		return getSymbolIdElement().value;
	}
	private function setSymbolIdValue(anId : String) {
		getSymbolIdElement().value = anId;
	}
	private function getSymbolTypeElement () {
		return (cast Browser.document.getElementById(SYMBOL_TYPE_LIST));
	}
	private function getSymbolSideElement() {
		return (cast Browser.document.getElementById(SYMBOL_SIDE_LIST));
	}

	private function getSelectedOptionElement(inputList, multiSelect) {
		var selectedOptions = inputList.selectedOptions;
		if(multiSelect) {
			trace("Multiple selection true. What can we do here?");
			throw "Multiple selection list not supported for this method";
		}else {
			var optionElement : OptionElement = 
					cast (selectedOptions.item(0));
			return optionElement.text;
		}
	}
	private function getSymbolTypeValue() {
		var multiSelect : Bool = false;
		return getSelectedOptionElement(getSymbolTypeElement(), multiSelect);
	}
	private function getSymbolSideValue() {
		var multiSelect : Bool = false;
		return getSelectedOptionElement(getSymbolSideElement(), multiSelect);
	}




	private function setupStreams() {
		model.sideStream.then(updateSidesStream);
		model.typeStream.then(updateTypesStream);
		var deleteP : Stream<Dynamic> =
			MBooks_im.getSingleton().initializeElementStream(
				cast getDeletePortfolioSymbolButton()
				, "click"
			);
		deleteP.then(deletePortfolioSymbol);
		var updateP : Stream<Dynamic> = 
			MBooks_im.getSingleton().initializeElementStream(
				cast getUpdatePortfolioSymbolButton()
				, "click"
			);
		updateP.then(updatePortfolioSymbol);
		var insertP : Stream<Dynamic> = 
			MBooks_im.getSingleton().initializeElementStream(
				cast getInsertPortfolioSymbolButton()
				, "click"
			);
		insertP.then(insertPortfolioSymbol);
		insertStreamResponse = new Deferred<PortfolioSymbolT>();
		updateStreamResponse = new Deferred<PortfolioSymbolT>();
		deleteStreamResponse = new Deferred<PortfolioSymbolT>();
		readStreamResponse = new Deferred<PortfolioSymbolT>();
		insertStreamResponse.then(insertResponse);
		updateStreamResponse.then(updateResponse);
		deleteStreamResponse.then(deleteResponse);
		readStreamResponse.then(readResponse);
		symbolQueryResponse = new Deferred<PortfolioSymbolQueryT>();
		symbolQueryResponse.then(handleQueryResponse);
		MBooks_im.getSingleton().marketDataStream.then(updateMarketData);
		MBooks_im.getSingleton().portfolio.activePortfolioStream.then(processActivePortfolio);
		var uploadPortfolioButtonStream : Stream<Dynamic> =
				MBooks_im.getSingleton().initializeElementStream(
					cast getUploadPortfolioButton()
					, "click"
				);
		uploadPortfolioButtonStream.then(uploadPortfolio);
	}

	private function uploadPortfolio(ev : Event) {
		trace("Save button pressed");
		var files  = getUploadPortfolioFile().files;
		for (file in files){
			var reader = new FileReader();
			var stream_1 : Stream<Dynamic> = 
				MBooks_im.getSingleton().initializeElementStream(
					cast reader
					, "load");
			stream_1.then(processFileUpload);
			reader.readAsText(file);

		}

	}
	//Assumes a header row.
	private function parsePortfolioDetails(fileContents : String){
		trace("Parsing portfolio details");
		var portfolioDetails : Array<Record> = 
				Reader.parseCsv(fileContents);
		var headerRead : Bool = false;

		for(aRecord in portfolioDetails) {
			var a : Record = cast aRecord;
			if(a.length == 4){
				if(headerRead == true){
					insertPortfolioSymbolI(StringTools.trim(a[0]), 
								StringTools.trim(a[1]), 
								StringTools.trim(a[2]), 
								StringTools.trim(a[3]));
				}else{
					trace("Skipping " + aRecord);
				}
			}else{
				trace("Invalid record length " + aRecord);
			}
			headerRead = true;
		}
	}
	private function processFileUpload(ev: Event) {
		trace("Processing file upload ");
		try {
			var reader : FileReader = cast ev.target;
			trace("Reading");
			parsePortfolioDetails(reader.result);
			trace("Read");
		}catch(e : Dynamic){
			trace("Exception " + e);
		}
	}
	private function computeInsertIndex() {
		return 1; //Need to compute the index based on the 
				//current state of the table.
	}

	private function getKey(payload : PortfolioSymbolT) {
		if(payload == null){
			throw ("Get failed. No payload");
		}
		return (payload.symbol + payload.side + payload.symbolType + payload.portfolioId);
	}

	private function processActivePortfolio(a : PortfolioT){
		trace("Deleting all the existing rows as active portfolio changed " + a);
		for(key in rowMap.keys()){
			trace("Deleting key " + key);
			var row : TableRowElement = 
					cast rowMap.get(key);
			var pSymbolTable : TableElement = getPortfolioSymbolTable();
			pSymbolTable.deleteRow(row.rowIndex);
		}	
		rowMap = new StringMap<TableRowElement>();
	}

	private function deleteTableRowMap(payload : PortfolioSymbolT) {
		trace("Deleting table row map " + payload);
		var key : String = getKey(payload);
		trace("Deleting key " + key);
		
		if(!rowMap.exists(key)) {
			trace("Nothing to delete " + payload);
			return;
		}
		var row : TableRowElement = cast rowMap.get(key);
		rowMap.remove(key);
		var pSymbolTable : TableElement = getPortfolioSymbolTable();
		pSymbolTable.deleteRow(row.rowIndex);
	}

	private function updateTableRowMap(payload : PortfolioSymbolT) {
		var key : String = getKey(payload);
		var row : TableRowElement = cast rowMap.get(key);
		if(payload.portfolioId != MBooks_im.getSingleton().portfolio.activePortfolio.portfolioId){
			trace("Throwing message " + payload);
			return;
		}
		if(row == null){
			var pSymbolTable = getPortfolioSymbolTable();
			row = cast(pSymbolTable.insertRow(computeInsertIndex()));
			rowMap.set(key, row);
			insertCells(row, payload);
		}else {
			var cells : HTMLCollection = row.children;
			for(cell in cells){
				var cellI : TableCellElement = cast cell;
				var cellIndex : Int = cellI.cellIndex;
				switch(cellIndex) {
					case 0 : cellI.innerHTML = payload.symbol;
					case 1 : cellI.innerHTML = payload.side;
					case 2 : cellI.innerHTML = payload.symbolType;
					case 3 : cellI.innerHTML = payload.quantity;
					case 4 : cellI.innerHTML = payload.value;
					case 5 : cellI.innerHTML = payload.stressValue;
					case 6 : cellI.innerHTML = Date.now().toString();
				}
			}
		}
	}

	private function insertCells(aRow : TableRowElement, payload : PortfolioSymbolT) {
		if(payload.portfolioId != MBooks_im.getSingleton().portfolio.activePortfolio.portfolioId){
			trace("Throwing away payload " + payload);
			return;
		}
		trace("Inserting cells from payload " + payload);
		var newCell : TableCellElement = cast (aRow.insertCell(0));
		newCell.innerHTML = payload.symbol;
		newCell = cast aRow.insertCell(1);
		newCell.innerHTML = payload.side;
		newCell = cast aRow.insertCell(2);
		newCell.innerHTML = payload.symbolType;
		newCell = cast aRow.insertCell(3);
		newCell.innerHTML = payload.quantity;
		newCell = cast aRow.insertCell(4);
		newCell.innerHTML = payload.value;
		newCell = cast aRow.insertCell(5);
		newCell.innerHTML = payload.stressValue;
		newCell = cast aRow.insertCell(6);
		newCell.innerHTML = Date.now().toString();
	}
	private function insertResponse(payload : PortfolioSymbolT) {
		trace("Inserting view " + payload);
		//clearFields();
		updateTableRowMap(payload);
	}

	private function updateResponse(payload : PortfolioSymbolT){
		trace("Updating view " + payload);
		//clearFields();
		updateTableRowMap(payload);
	}
	private function deleteResponse(payload : PortfolioSymbolT) {
		trace("Deleting view "  + payload);
		//clearFields();
		deleteTableRowMap(payload);
	}

	private function readResponse(payload : PortfolioSymbolT) {
		trace("Reading view " + payload);
		throw "Read response Not implemented";
	}

	private function getPortfolioId() {
		if(model == null){
			throw ("Model not defined");
		}
		return model.activePortfolio.portfolioId;
	}
	private function insertPortfolioSymbolI(aSymbol : String, aSymbolType : String, aSide: String, quantity : String){
		trace("Inserting portfolio symbol through upload ");
		var portfolioSymbolT : PortfolioSymbolT = {
			crudType : "Create"
			, commandType : "ManagePortfolioSymbol"
			, portfolioId : getPortfolioId()
			, symbol : aSymbol
			, quantity : quantity
			, side : aSide
			, symbolType : aSymbolType
			, value : "0.0"
			, stressValue : "0.0"
			, creator : MBooks_im.getSingleton().getNickName()
			, updator : MBooks_im.getSingleton().getNickName()
			, nickName : MBooks_im.getSingleton().getNickName()
		}
		model.insertStream.resolve(portfolioSymbolT);
	}
	private function insertPortfolioSymbol(ev : Event ){
		trace("Insert portfolio symbol " + ev);
		trace("Symbol side "  + getSymbolSideValue());
		trace("Symbol type "  + getSymbolTypeValue());
		var portfolioSymbolT  : PortfolioSymbolT = {
			crudType : "Create"
			, commandType : "ManagePortfolioSymbol"
			, portfolioId : getPortfolioId()
			, symbol : getSymbolIdValue()
			, quantity  : getQuantityValue()
			, side : getSymbolSideValue()
			, symbolType : getSymbolTypeValue()
			, value : "0.0"
			, stressValue : "0.0"
			, creator : MBooks_im.getSingleton().getNickName()
			, updator : MBooks_im.getSingleton().getNickName()
			, nickName : MBooks_im.getSingleton().getNickName()
		};
		model.insertStream.resolve(portfolioSymbolT);		

	}
	private function updatePortfolioSymbol(ev : Event) {
		trace("Update portfolio symbol " + ev);
		var portfolioSymbolT  : PortfolioSymbolT = {
			crudType : "P_Update"
			, commandType : "ManagePortfolioSymbol"
			, portfolioId : getPortfolioId()
			, symbol : getSymbolIdValue()
			, quantity  : getQuantityValue()
			, side : getSymbolSideValue()
			, symbolType : getSymbolTypeValue()
			, value : "0.0"
			, stressValue : "0.0"
			, creator : MBooks_im.getSingleton().getNickName()
			, updator : MBooks_im.getSingleton().getNickName()
			, nickName : MBooks_im.getSingleton().getNickName()
		};
		model.updateStream.resolve(portfolioSymbolT);
		
	}
	private function deletePortfolioSymbol(ev : Event){
		trace("Delete portfolio symbol " + ev);
		var portfolioSymbolT  : PortfolioSymbolT = {
			crudType : "Delete"
			, commandType : "ManagePortfolioSymbol"
			, portfolioId : getPortfolioId()
			, symbol : getSymbolIdValue()
			, quantity  : getQuantityValue()
			, side : getSymbolSideValue()
			, symbolType : getSymbolTypeValue()
			, value : "0.0"
			, "stressValue" : "0.0"
			, creator : MBooks_im.getSingleton().getNickName()
			, updator : MBooks_im.getSingleton().getNickName()
			, nickName : MBooks_im.getSingleton().getNickName()
		};
		model.deleteStream.resolve(portfolioSymbolT);
	}
	private function readPortfolio(someEvent : Dynamic){
		var portfolioSymbolT  : PortfolioSymbolT = {
			crudType : "Delete"
			, commandType : "ManagePortfolioSymbol"
			, portfolioId : "getPortfolioId()"
			, symbol : getSymbolIdValue()
			, quantity  : getQuantityValue()
			, side : getSymbolSideValue()
			, symbolType : getSymbolTypeValue()
			, value :""
			, stressValue : "0.0"
			, creator : MBooks_im.getSingleton().getNickName()
			, updator : MBooks_im.getSingleton().getNickName()
			, nickName : MBooks_im.getSingleton().getNickName()
		};
		model.readStream.resolve(portfolioSymbolT);	
	}
	private function updateSidesStream(symbolSide : SymbolSide) {
		trace("Resolving symbol side " + symbolSide);
		if(symbolSide == null) {
			trace("Invalid symbol side ");
			return;
		}
		var symbolSideList = getSymbolSideList();
		var optionId = SYMBOL_SIDE_LIST  + "_" + symbolSide.symbolSide;
		var optionElement : OptionElement  
			= cast (Browser.document.getElementById(optionId));
		if(optionElement == null){
			optionElement = cast (Browser.document.createOptionElement());
			optionElement.id = optionId;
			optionElement.text = symbolSide.symbolSide;
			var selectSymbolSideStream  = 
				MBooks_im.getSingleton().initializeElementStream(
					cast optionElement
					, "click"
				);
			selectSymbolSideStream.then(handleSymbolSideSelected);
			symbolSideList.appendChild(optionElement);
		}
	}

	private function updateTypesStream(symbolType : SymbolType) {
		trace("Resolving symbol type " + symbolType);
		if(symbolType == null) {
			trace ("Invalid symbol type " );
			return;
		}
		var symbolTypeList = getSymbolTypeList();
		var optionId = SYMBOL_TYPE_LIST + symbolType.symbolType;
		var optionElement : OptionElement 
			= cast (Browser.document.getElementById(optionId));
		if(optionElement == null){
			optionElement = cast (Browser.document.createOptionElement());
			optionElement.id = optionId;
			optionElement.text = symbolType.symbolType;
			var stream = 
				MBooks_im.getSingleton().initializeElementStream(
					cast optionElement
					, "click"
				);
			stream.then(handleSymbolTypeSelected);
			symbolTypeList.appendChild(optionElement);
		}
	}
	private function handleSymbolSideSelected(ev : Event){
		trace("handle symbol side selected " + ev);
	}
	private function handleSymbolTypeSelected(ev : Event){ 
		trace("handle symbol type selected " + ev);
	}
	private function getSymbolSideList() {
		return (cast Browser.document.getElementById(SYMBOL_SIDE_LIST));
	}
	private function getSymbolTypeList() {
		return (cast Browser.document.getElementById(SYMBOL_TYPE_LIST));
	}


	private function clearFields() {
		setQuantityValue("");
		setSymbolIdValue("");
	}
	public function manage(incomingMessage1 : Dynamic) {
		trace("Manage portfolio symbol " + incomingMessage1);
		if(incomingMessage1.Right != null) {
			var incomingMessage = incomingMessage1.Right;
			if(incomingMessage.crudType == "Create") {
				insertStreamResponse.resolve(incomingMessage);
			}else if(incomingMessage.crudType == "P_Update"){
				updateStreamResponse.resolve(incomingMessage);
			}else if (incomingMessage.crudType == "Read"){
				readStreamResponse.resolve(incomingMessage);
			}else if(incomingMessage.crudType == "Delete"){
				deleteStreamResponse.resolve(incomingMessage);
			}else {
				throw ("Undefined crud type " + incomingMessage);
			}		
		}else {
			MBooks_im.getSingleton().applicationErrorStream.resolve(incomingMessage1);
		}

	}


	private function handleQueryResponse(incomingMessage : Dynamic){
		trace("Processing symbol query response " + incomingMessage);
		if(incomingMessage.Left != null){
			MBooks_im.getSingleton().applicationErrorStream.resolve(incomingMessage);
		}else {
			if(incomingMessage.Right.resultSet == null){
				trace("Result set is not defined??");
				MBooks_im.getSingleton().applicationErrorStream.resolve(incomingMessage);
				return;
			}

			var pS : PortfolioSymbolQueryT = incomingMessage.Right;
			for (i in pS.resultSet) {
				if (i.Right != null){
					updateTableRowMap(i.Right);
				}else if(i.Left != null){
					MBooks_im.getSingleton().applicationErrorStream.resolve(i);
				}
			}

		}

	}
	private function updateMarketData(incomingMessage: Dynamic){
		trace("Inside update market data response " + incomingMessage);
		//this.updateTableRowMap(incomingMessage);

	}
	//When we allow users to move columns around, 
	//this dictionary needs to be updated.
	private var columnIndexMap : ObjectMap<String, Int>;
	private var rowMap : StringMap<TableRowElement>;
}