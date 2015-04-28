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

	private static var MANAGE_PROJECT  : String= "ManageProject";
	private static var PROJECT_IDENTIFICATION = "projectIdentification";
	private static var COMPANY_LIST = "companyList";
	private static var PROJECT_START = "projectStart";
	private static var PROJECT_END = "projectEnd";
	private static var PREPARED_BY = "preparedBy";
	private static var PROJECT_SUMMARY = "projectSummary";
	private static var PROJECT_DETAILS = "projectDetails";
	private static var PROJECT_LIST = "projectList";

	private var crudType : String;
	private var projectID : String;
	private var uniqueCompanyID : String;
	private var summary : String;
	private var details : String;
	private var preparedBy : String;
	private var startDate : Date;
	private var endDate : Date;
	private var uploadedBy : String;
	private var uploadTime : Date;
	private var company : Company;

	public function new(companyI : Company) {
		try {
			trace("Instantiating Project");
			newProject = true;
			var stream : Stream<Dynamic> = 
				MBooks_im.getSingleton().initializeElementStream(
					cast getSaveProject()
					, "click"
					);
			stream.then(saveProject);
			this.company = companyI;
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

	private function getProjectsListElement() : SelectElement {
		return (cast Browser.document.getElementById(PROJECT_LIST));
	}

	private function processProjectList(incomingMessage : Dynamic) {
		trace("Project list " + incomingMessage);
		var projects = incomingMessage.project;
		var projectList = getProjectsListElement();
		var pArray : Array<Dynamic> = projects;
		for (project in pArray){
			var projectId = project.projectID;
			var projectSummary = project.summary;
			var optionElement : OptionElement = 
				cast Browser.document.getElementById(projectId);
			if(optionElement == null){
				optionElement = 
					cast (Browser.document.createOptionElement());
				optionElement.id = projectId;
				optionElement.text = projectSummary;
				var projectSelectedStream = 
					MBooks_im.getSingleton().initializeElementStream(
						cast optionElement,
						"click"
						);
				projectSelectedStream.then(processProjectSelected);
			}
		}
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
				var optionSelectedStream = 
					MBooks_im.getSingleton().initializeElementStream(
						cast optionElement
						, "click"
					);
				optionSelectedStream.then(processCompanySelected);
				companiesSelectElement.appendChild(optionElement);
			}else {
				trace("Element exists " + companyID);
			}

		}
		trace("Completed processing companies");
	}

	private function getProjectId() : String {
		return getProjectIdElement().value;
	}
	private function getProjectIdElement() : InputElement {
		return (cast Browser.document.getElementById(PROJECT_IDENTIFICATION));
	}
	private function setProjectId(pid : String) {
		getProjectIdElement().value = pid;
	}

	private function getProjectStart() : String {
		return (getProjectStartElement().value);
	}
	private function getProjectStartElement () : InputElement {
		return (cast Browser.document.getElementById(PROJECT_START));
	}
	private function setProjectStart(sDate : String) {
		getProjectStartElement().value = sDate;
	}
	private function getProjectEnd() : String {
		return (getProjectEndElement().value);
	}
	private function getProjectEndElement() : InputElement {
		return (cast (Browser.document.getElementById(PROJECT_END)));
	}
	private function setProjectEnd(eDate : String) {
		getProjectEndElement().value = eDate;
	}

	private function getPreparedBy() : String {
		return (getPreparedByElement().value);
	}
	private function getPreparedByElement (): InputElement {
		return (cast Browser.document.getElementById(PREPARED_BY));
	}
	private function setPreparedBy(aName : String){
		getPreparedByElement().value = aName;
	}

	private function getProjectSummary() : String {
		return (getProjectSummaryElement().value);
	}
	private function getProjectSummaryElement() : InputElement {
		return (cast Browser.document.getElementById(PROJECT_SUMMARY));
	}
	private function setProjectSummary(summary : String){
		getProjectSummaryElement().value = summary;
	}
	private function getProjectDetails() : String {
		return (getProjectDetailsElement().value);
	}
	private function getProjectDetailsElement() : InputElement {
		return (cast Browser.document.getElementById(PROJECT_DETAILS));
	}
	private function setProjectDetails(details : String){
		getProjectDetailsElement().value = details;
	}

	private function processCompanySelected(ev : Event){
		trace("Company selected" + " " + ev.target + " " + ev);

		var selectionElement : OptionElement 
				= cast ev.target;
	
		var selectedId = selectionElement.id;
		trace("Reading company information for " + selectedId);
		company.read(selectedId);	
	}

	private function processProjectSelected(ev : Event) {
		trace ("Project selected " + ev.target);
		var selectionElement :OptionElement  = 
			cast ev.target;
		var selectionId = selectionElement.id;
		sendReadRequest(selectionId);
	}

	private function processManageProject(incomingMessage){
		trace("Process manage company ");
		var crudType = incomingMessage.crudType;
		trace(incomingMessage);
		if(crudType == "Create") {
			trace("Create successful");
			copyIncomingValues(incomingMessage);
		}else if (crudType == "Read") {
			if(incomingMessage.projectID == "") {
				newProject = true;
				
			}else {
				copyIncomingValues(incomingMessage);
				newProject = false;			
			}
		}else if (crudType == "P_Update") {
			copyIncomingValues(incomingMessage);
		}else if (crudType == "Delete"){
			clearFields(incomingMessage);
		}else {
			throw ("Invalid crudtype " + crudType);
		}
		
	}
	private function copyIncomingValues(aMessage){
		try {
			this.setProjectId(aMessage.projectID);
			this.setProjectSummary(aMessage.summary);
			this.setProjectDetails(aMessage.details);
			this.setProjectStart(aMessage.startDate);
			this.setProjectEnd(aMessage.endDate);		
		}catch(err : Dynamic){
			trace("Error copying values "  + aMessage);
		}

	}
	private function clearFields(aMessage) {
		try {
			this.setProjectId("");
			this.setProjectSummary("");
			this.setProjectDetails("");
			this.setProjectStart("");
			this.setProjectEnd("");
		}catch(err : Dynamic){
			trace("Error clearing fields "  + err);
		}
		
	}

	private function sendReadRequest(projectID){
			try {
			var nickName = MBooks_im.getSingleton().getNickName();
			var payload = getPayload(nickName, "Read");
			MBooks_im.getSingleton().doSendJSON(payload);
			}catch(err : Dynamic) {
				trace("Error checking company " + err);
			}

	}


	private function getProjectStructure () {
		var result :Dynamic = 
			{
				projectId : projectID
				, uniqueCompanyID : uniqueCompanyID
				, summary : summary
				, details : details
				, startDate : startDate
				, endDate : endDate
				, uploadTime : Date.now()
				, preparedBy : preparedBy
			};
		return result;

	}

	private function getPayload(nickName, crudType) : Dynamic {
		var payload : Dynamic = {
			nickName : nickName
			, commandType : MANAGE_PROJECT
			, crudType : crudType
			, project : getProjectStructure()
		};
		return payload;

	}


}