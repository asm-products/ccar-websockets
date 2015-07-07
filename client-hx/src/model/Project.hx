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
	private static var CREATE = "Create";
	private static var UPDATE = "P_Update";
	private static var DELETE = "Delete";
	private static var READ = "Read";


	private var crudType : String;
	public var projectID(default, null) : String;
	private var uniqueCompanyID : String;
	private var summary : String;
	private var details : String;
	private var preparedBy : String;
	private var startDate : Date;
	private var endDate : Date;
	private var uploadedBy : String;
	private var uploadTime : Date;
	private var company : Company;
	private var projectStream : Deferred<Dynamic>;
	public var activeProjectWorkbench(default, null) : ProjectWorkbench;

	public function getSupportedScriptsStream() : Deferred<Dynamic> {
		if(activeProjectWorkbench == null){
			throw "No active project workbench found";
		}else {
			return activeProjectWorkbench.supportedScriptsStream;
		}
	}

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
			projectStream = new Deferred<Dynamic>();
			projectStream.then(processProjectList);
		}catch(err : Dynamic){
			trace("Error creating project " + err);
		}
	}


	public function getSelectActiveProjectsStream() : Deferred<Dynamic> {
		return projectStream;
	}

	private function getSaveProject() : ButtonElement {
		var buttonElement : ButtonElement 
			= cast Browser.document.getElementById(SAVE_PROJECT);
		return buttonElement;
	}

	private function saveProject(ev : Event) {
			try {
				trace("Saving project");
				var nickName = MBooks_im.getSingleton().getNickName();
				var payload = getPayloadD(nickName, getCrudType());
				MBooks_im.getSingleton().doSendJSON(payload);
			}catch(err : Dynamic) {
				trace("Error checking company " + err);
			}

	}

	private function getProjectsListElement() : SelectElement {
		return (cast Browser.document.getElementById(PROJECT_LIST));
	}

	public function processProjectList(incomingMessage : Dynamic) {
		trace("Project list " + incomingMessage);
		var projects = incomingMessage.projects;
		var projectList = getProjectsListElement();
		var pArray : Array<Dynamic> = projects;
		for (project in pArray){
			var projectId = project.identification;
			trace("Adding project id " + projectId);
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
				projectList.appendChild(optionElement);
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

	private function getProjectID() : String {
		if(getProjectIDElement().value != "") {
			return getProjectIDElement().value;	
		}else{
			return "";
		}
		
	}
	private function getProjectIDElement() : InputElement {
		return (cast Browser.document.getElementById(PROJECT_IDENTIFICATION));
	}
	private function setProjectID(pid : String) {
		getProjectIDElement().value = pid;
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
		getProjectList(selectedId);	
	}


	private function getProjectList(companyId: String) {
		trace("Processing select all projects " + companyId);
		var payload = {
			nickName : MBooks_im.getSingleton().getNickName()
			, commandType : "SelectActiveProjects"
			, companyId : companyId
		};
		MBooks_im.getSingleton().doSendJSON(payload);
	}	

	private function processProjectSelected(ev : Event) {
		trace ("Project selected " + ev.target);
		var selectionElement :OptionElement  = 
			cast ev.target;
		var selectionId = selectionElement.id;
		sendReadRequest(selectionId);
	}

	public function processManageProject(incomingMessage){
		trace("Process manage Projects  ");
		try {
			var crudType = incomingMessage.Right.crudType;
			//trace(incomingMessage);
			if(crudType == CREATE) {
				trace("Create successful");
				copyIncomingValues(incomingMessage);
			}else if (crudType == READ) {
				if(incomingMessage.Right.projectId == "") {
					newProject = true;					
				}else {
					copyIncomingValues(incomingMessage);
					newProject = false;
					var projectWorkbench : ProjectWorkbench = 
						new ProjectWorkbench(this);
					activeProjectWorkbench = projectWorkbench;
				}
			}else if (crudType == UPDATE) {
				copyIncomingValues(incomingMessage);
			}else if (crudType == DELETE){
				clearFields(incomingMessage);
			}else {
				throw ("Invalid crudtype " + crudType);
			}
		}catch(err : Dynamic){
			throw err;
		}
		
	}
	private function copyIncomingValues(wMessage){
		try {
			var aMessage = wMessage.Right;
			this.projectID = aMessage.projectId;
			//This is an issue.
			//UI elements need to listen on multiple events.
			//From the user as well as the server.
			//Need to handle this.
			this.setProjectID(aMessage.projectId);
			this.setProjectSummary(aMessage.summary);
			this.setProjectDetails(aMessage.details);
			this.setProjectStart(aMessage.startDate);
			this.setProjectEnd(aMessage.endDate);		
		}catch(err : Dynamic){
			trace("Error copying values "  + wMessage);
		}

	}
	private function clearFields(aMessage) {
		try {
			this.setProjectID("");
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
			var payload = getPayload(nickName, "Read", projectID);
			MBooks_im.getSingleton().doSendJSON(payload);
			}catch(err : Dynamic) {
				trace("Error checking company " + err);
			}

	}

	private function getCrudType() {
		if (getProjectID() == ""){
			return CREATE;
		}else {
			return UPDATE;
		}
	}
	private function getPayload(nickName, crudType, projectId) : Dynamic {
		var payload : Dynamic = {
			nickName : nickName
			, commandType : MANAGE_PROJECT
			, crudType : crudType
			, projectId : projectId
			, uniqueCompanyID : company.getCompanyID()
			, summary : getProjectSummary()
			, details : getProjectDetails()
			, startDate : [(Date.now())] // XXX: Fix this
			, endDate : [Date.now()]
			, uploadTime : [(Date.now())]
			, preparedBy : getPreparedBy()
			, uploadedBy : nickName
	
		};
		return payload;
	}

	private function getPayloadD(nickName, crudType) : Dynamic {
		return getPayload(nickName, crudType, getProjectID());
	}


}