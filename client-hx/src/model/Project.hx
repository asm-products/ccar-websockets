package model;
import util.Util;
import haxe.Json;
import haxe.Utf8;
import haxe.Timer;
import haxe.ds.ArraySort;
import js.html.Element;
import haxe.ds.GenericStack;
import js.html.Event;
import js.html.CloseEvent;
import js.html.MessageEvent;
import js.html.WebSocket;
import js.html.DOMCoreException;
import js.html.DivElement;
import js.html.Document;
import js.html.File;
import js.html.KeyboardEvent;
import js.html.InputElement;
import js.html.SelectElement;
import js.html.OptionElement;
import js.html.FileReader;
import js.html.ImageElement;
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
import js.html.ButtonElement;
import js.html.TextAreaElement;
import promhx.Stream;
import promhx.Promise;
import massive.munit.TestRunner;
using promhx.haxe.EventTools;
import promhx.Deferred;
import promhx.base.EventLoop;


class Project {
	private static var SAVE_PROJECT = "saveProject";
	private static var DELETE_PROJECT = "deleteProject";
	private var newProject : Bool;

	private static var PROJECT_IDENTIFICATION = "projectIdentification";
	private static var COMPANY_LIST = "companyList";
	private static var PROJECT_START = "projectStart";
	private static var PROJECT_END = "projectEnd";
	private static var PREPARED_BY = "preparedBy";
	private static var PROJECT_SUMMARY = "projectSummary";
	private static var PROJECT_DETAILS = "projectDetails";

	var crudType : String;
	var identification : String;
	var companyUniqueId : String;
	var summary : String;
	var details : String;
	var preparedBy : String;
	var startDate : Date;
	var endDate : Date;
	var uploadedBy : String;
	var uploadTime : Date;


	public function new(company : Company) {
		try {
			trace("Instantiating Project");
			newProject = true;
			var stream : Stream<Dynamic> = 
				MBooks_im.getSingleton().initializeElementStream(
					cast getSaveProject()
					, "click"
					);
			stream.then(saveProject);
			company.getSelectListEventStream().then(processCompanyList);		
		}catch(err : Dynamic){
			trace("Error creating project " + err);
		}
	}

	private function getSaveProject() : ButtonElement {
		var buttonElement : ButtonElement 
			= cast Browser.document.getElementById(SAVE_PROJECT);
		return buttonElement;
	}

	private function saveProject(ev : Event) {

	}

	private function getCompanyListElement() : SelectElement {
		return (cast Browser.document.getElementById(COMPANY_LIST));
	}

	public function processCompanyList(incomingMessage : Dynamic) {
		trace("Processing company list " + incomingMessage);
		var companies = incomingMessage.company;
		var companiesSelectElement : SelectElement = getCompanyListElement();
		var cArray : Array<Dynamic> = incomingMessage.company;
		for(company in cArray){
			var companyID  = company.companyID;
			var companyName = company.companyName;
			trace("Company " + companyID + " -> " + companyName);
			var optionElement : OptionElement = 
				cast Browser.document.getElementById(companyID);
			if(optionElement == null){
				optionElement = 
						cast Browser.document.createOptionElement();
				optionElement.id = companyID;
				optionElement.text = companyName;
				companiesSelectElement.appendChild(optionElement);
			}

		}
		trace("Completed processing companies");
	}

	private function getProjectIdentification() : String {
		return getProjectIdentificationElement().value;
	}
	private function getProjectIdentificationElement() : InputElement {
		return (cast Browser.document.getElementById(PROJECT_IDENTIFICATION));
	}



}