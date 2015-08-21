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
import haxe.ds.ObjectMap;
import promhx.Stream;
import promhx.Deferred;
import promhx.base.EventLoop;
import model.Portfolio;
import model.Company;
import js.Lib.*;
import util.*;


class Portfolio {
	private static var SAVE_PORTFOLIO : String = "savePortfolio";
	private static var UPDATE_PORTFOLIO : String = "updatePortfolio";
	private static var DELETE_PORTFOLIO : String = "deletePortfolio";
	private static var SYMBOL_INPUT_FIELD : String = "portfolioSymbol";
	private static var SIDE_INPUT_FIELD : String = "portfolioSide";
	private static var QUANTITY_INPUT_FIELD : String = "portfolioQuantity";
	private static var PORTFOLIO_LIST_FIELD  : String = "portfolioList";
	private static var PORTFOLIO_SUMMARY : String = "portfolioSummary";
	public var model (default, null): model.Portfolio;

	public var activePortfolioStream (default, null) : Deferred<PortfolioT>;
	//Symbol, side and quantity when entered (key press or select) should 
	//trigger a save event. 
	//Save event should trigger a save event.
	//Each symbol has a uuid that needs to be used to query the db.
	private function setupEvents() : Void {
		trace("Setting up ui events");
		var saveP : Stream<Dynamic> = 
			MBooks_im.getSingleton().initializeElementStream(
				cast getSavePortfolioButton()
				, "click"
				);
		saveP.then(savePortfolio);
		var updateP : Stream<Dynamic> =
			MBooks_im.getSingleton().initializeElementStream(
				cast getUpdatePortfolioButton()
				, "click"
				);
		updateP.then(updatePortfolio);
		var deleteP : Stream<Dynamic> =
			MBooks_im.getSingleton().initializeElementStream(
				cast getDeletePortfolioButton()
				, "click"
			);
		deleteP.then(deletePortfolio);
		MBooks_im.getSingleton().portfolioListStream.then(processPortfolioList);
		MBooks_im.getSingleton().activeCompanyStream.then(processActiveCompany);
		MBooks_im.getSingleton().portfolioStream.then(processManagePortfolio);
		this.getPortfoliosForUser();
	}

	public function new() {
		activePortfolioStream = new Deferred<PortfolioT>();
		setupEvents();
	}

	private function processActiveCompany(selected: model.Company){
		trace("Company selected for portfolio processing " + selected);
		this.activeCompany = selected;
		getPortfoliosForUser();
	}
	private function deletePortfolio(ev : Event){
		trace("Delete portfolio " + ev);
		deletePortfolioI();
	}

	private function savePortfolio(ev : Event) {
		if(activePortfolio == null) {
			insertPortfolioI();
		}else {
			updatePortfolioI();
		}
	}
	private function updatePortfolio(ev : Event) {
		trace("Update portfolio " + ev);
		if(activePortfolio == null){
			trace("Selected portfolio null. Not updating");
		}else {
			updatePortfolioI();
		}
	}



	private function readPortfolio(portfolioId) {
		var portfolioT : PortfolioT = {
			crudType : "Read"
			, commandType : "ManagePortfolio"
			, portfolioId : portfolioId
			, companyId : activeCompany.companyId
			, userId : MBooks_im.getSingleton().getNickName()
			, summary : getPortfolioSummary()
			, createdBy : MBooks_im.getSingleton().getNickName()
			, updatedBy : MBooks_im.getSingleton().getNickName()
			, nickName : MBooks_im.getSingleton().getNickName()
		};
		MBooks_im.getSingleton().doSendJSON(portfolioT);

	}
	private function insertPortfolioI() {
		var portfolioT : PortfolioT = {
			crudType : "Create"
			, commandType : "ManagePortfolio"
			, portfolioId : activePortfolio.portfolioId
			, companyId : activeCompany.companyId
			, userId : MBooks_im.getSingleton().getNickName()
			, summary : getPortfolioSummary()
			, createdBy : MBooks_im.getSingleton().getNickName()
			, updatedBy : MBooks_im.getSingleton().getNickName()
			, nickName : MBooks_im.getSingleton().getNickName()
		};
		MBooks_im.getSingleton().doSendJSON(portfolioT);
	}
	private function updatePortfolioI(){
		var portfolioT : PortfolioT = {
			crudType : "P_Update"
			, commandType : "ManagePortfolio"
			, portfolioId : activePortfolio.portfolioId
			, companyId : activeCompany.companyId
			, userId : MBooks_im.getSingleton().getNickName()
			, summary : getPortfolioSummary()
			, createdBy : MBooks_im.getSingleton().getNickName()
			, updatedBy : MBooks_im.getSingleton().getNickName()
			, nickName : MBooks_im.getSingleton().getNickName()
		};
		MBooks_im.getSingleton().doSendJSON(portfolioT);		
	}
	private function deletePortfolioI() {
		var portfolioT : PortfolioT = {
			crudType : "Delete"
			, commandType : "ManagePortfolio"
			, portfolioId : activePortfolio.portfolioId
			, companyId : activeCompany.companyId
			, userId : MBooks_im.getSingleton().getNickName()
			, summary : getPortfolioSummary()
			, createdBy : MBooks_im.getSingleton().getNickName()
			, updatedBy : MBooks_im.getSingleton().getNickName()
			, nickName : MBooks_im.getSingleton().getNickName()
		};
		MBooks_im.getSingleton().doSendJSON(portfolioT);		
	}

	private function setPortfolioSummary(aSummary) {
		getPortfolioSummaryElement().value = aSummary;
	}
	private function getPortfolioSummary() {
		return getPortfolioSummaryElement().value;
	}
	private function getPortfolioSummaryElement() : TextAreaElement {
		var sumButton = 
			cast Browser.document.getElementById(PORTFOLIO_SUMMARY);
		return sumButton;
	}
	private function getSavePortfolioButton() : ButtonElement {
		var saveButton : ButtonElement = 
			cast Browser.document.getElementById(SAVE_PORTFOLIO);
		return saveButton;
	}
	private function getUpdatePortfolioButton() : ButtonElement {
		var updateButton : ButtonElement =
			cast Browser.document.getElementById(UPDATE_PORTFOLIO);
		return updateButton;
	}
	
	private function getPortfolioList() : SelectElement {
		return (cast Browser.document.getElementById(PORTFOLIO_LIST_FIELD));
	}

	private function getDeletePortfolioButton() : ButtonElement {
		var deleteButton : ButtonElement = 
			cast Browser.document.getElementById(DELETE_PORTFOLIO);
		return deleteButton;
	}

	private function processPortfolioList(incomingPayload : PortfolioQuery) {
		trace("Processing portfolio list " + incomingPayload);
		var results = incomingPayload.resultSet;
		for(p in results) {
			if(p.Right != null){
				updatePortfolioList(p.Right);
			}else {
				MBooks_im.getSingleton().applicationErrorStream.resolve(incomingPayload);
			}
		} 
	}

	private function processManagePortfolio(incomingMessage : Dynamic){
		trace("Incoming message manage portfolio "  + incomingMessage);
		if(incomingMessage.Right != null){
			updatePortfolioList(incomingMessage.Right);
			copyIncomingValues(incomingMessage.Right);
			this.activePortfolioStream.resolve(incomingMessage.Right);
			if(incomingMessage.Right.crudType == "Delete"){
				deletePortfolioEntry(incomingMessage.Right);
			}else {
				updatePortfolioEntry(incomingMessage.Right);
			}

		}else if(incomingMessage.Left != null) {
			MBooks_im.getSingleton().applicationErrorStream.resolve(incomingMessage.Left);
		}
	}

	private function copyIncomingValues(input: PortfolioT) {
		setPortfolioSummary(input.summary);
	}

	//Return all the portfolios for the user registered for 
	//the currently actively company
	private function getPortfoliosForUser(){
		if(activeCompany == null){
			trace("No company selected");
			return;
		}
		var portfolioQuery : PortfolioQuery = {
			commandType : "QueryPortfolios"
			, nickName : MBooks_im.getSingleton().getNickName()
			, companyId : activeCompany.companyId
			, userId : MBooks_im.getSingleton().getNickName()
			, resultSet : []
		};
		trace("Sending " + portfolioQuery);
		MBooks_im.getSingleton().doSendJSON(portfolioQuery);
	}

	private function updatePortfolioEntry(update : PortfolioT){
		var optionElement : OptionElement 
			= cast (Browser.document.getElementById(update.portfolioId));
		if(optionElement != null){
			optionElement.selected = true;
		}else {
			throw ("Option element for portfolio Id not found " + update);
		}
	}
	private function deletePortfolioEntry(deleteMe : PortfolioT) {
		trace("Deleting portfolio " + deleteMe);
		var optionElement : OptionElement 
				= cast (Browser.document.getElementById(deleteMe.portfolioId));
		if(optionElement != null){
			getPortfolioList().removeChild(optionElement);
			clearValues();
		}else {
			trace("Nothing to delete");
		}
	}
	private function clearValues(){
		setPortfolioSummary("");
	}
	private function updatePortfolioList(portfolioObject : PortfolioT){
		var portfolioList = getPortfolioList();
		var portfolioId = portfolioObject.portfolioId;
		var optionElement : OptionElement 
				= (cast Browser.document.getElementById(portfolioId));
		if(optionElement == null){
				optionElement = 
				cast (Browser.document.createOptionElement());
				optionElement.id = portfolioId;
				optionElement.text = portfolioObject.summary;
				var portfolioSelectedStream = 
					MBooks_im.getSingleton().initializeElementStream(
						cast optionElement,
						"click"
						);
				portfolioSelectedStream.then(processPortfolioSelected);
				portfolioList.appendChild(optionElement);
		}else {
			optionElement.text = portfolioObject.summary;
		}
	}
	private function processPortfolioSelected(ev : Event){
		trace ("Portfolio selected " + ev.target);
		var selectionElement :OptionElement  = 
			cast ev.target;
		var selectionId = selectionElement.id;
		readPortfolio(selectionId);
		trace ("Returning symbols for portfolio " + selectionId);
	}

	private var activeCompany : model.Company;
	private var activePortfolio : PortfolioT;

}