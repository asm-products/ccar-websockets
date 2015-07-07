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
import js.Lib.*;
import util.*;


class Portfolio {
	private static var SAVE_PORTFOLIO : String = "savePortfolio";
	private static var UPDATE_PORTFOLIO : String = "updatePortfolio";
	private static var DELETE_PORTFOLIO : String = "deletePortfolio";
	private static var SYMBOL_INPUT_FIELD : String = "portfolioSymbol";
	private static var SIDE_INPUT_FIELD : String = "portfolioSide";
	private static var QUANTITY_INPUT_FIELD : String = "portfolioQuantity";
	private static var COMPANY_LIST_FIELD : String = "portfolioCompanyList";
	private static var PORTFOLIO_LIST_FIELD  : String = "portfolioList";
	public var model (default, null): model.Portfolio;

	//The stream of all portfolio payloads from the server.
	//Server events update the portfolio list presented to the user.
	public var portfolioListStream(default, null) : Deferred<PortfolioPayload>;
	public var activePortfolio (default, null) : Deferred<ActivePortfolio>;
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
		portfolioListStream.then(processPortfolioList);
		activePortfolio.then(processActivePortfolio);

	}

	private function deletePortfolio(ev : Event){
		trace("Delete portfolio " + ev);
	}
	private function savePortfolio(ev : Event) {
		trace("Save portfolio " + ev);
	}
	private function updatePortfolio(ev : Event) {
		trace("Update portfolio " + ev);
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

	private function getCompanyList() : SelectElement {
		return (cast Browser.document.getElementById(COMPANY_LIST_FIELD));
	}
	private function processPortfolioList(incomingPayload : PortfolioPayload) {
		trace("Processing portfolio list");
	}
	private function processActivePortfolio(activePortfolio : ActivePortfolio) {
		trace("Processing active portfolio");
	}


}