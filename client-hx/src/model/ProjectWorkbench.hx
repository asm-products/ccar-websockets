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

enum WorkbenchCrudType {
	Create;
	WrkBench_Update;
	Delete;
	Read;
	}

typedef QuerySupportedScript = {
		var nickName : String;
		var commandType : String;
		var scriptTypes : Array<String>;
};

//I guess this namespace contamination 
//needs to be fixed.

typedef PrjWorkbench = {
	var crudType : String; //The default json writer add the code to the enum
	var workbenchId : String;
	var uniqueProjectId : String;
	var scriptType : String;
	var scriptSummary : String;
	var scriptData : String;
	var numberOfCores : Int;
	var scriptDataPath : String;
	var jobStartDate : String;
	var jobEndDate : String;
	var nickName : String;
	var commandType : String;

};
		

typedef QueryActiveWorkbenches = {
	var nickName : String;
	var commandType : String;
	var projectId : String;
	var workbenches : Array<PrjWorkbench>;
};



class ProjectWorkbench {
	//constants
	var PROJECT_WORKBENCH_LIST : String = "projectWorkbenches";
	var SAVE_WORKBENCH : String = "saveWorkbench";
	var SUPPORTED_SCRIPT_LIST_ELEMENT : String = "supportedScriptTypes";
	var WORKBENCH_ID_ELEMENT : String = "workbenchId";
	var SCRIPT_DATA_ELEMENT : String = "scriptData";
	var SCRIPT_UPLOAD_ELEMENT : String = "scriptUpload";
	var NUMBER_OF_CORES : String = "numberOfCores";	
	var SCRIPT_SUMMARY : String = "scriptSummary";
	var SCRIPT_DATA_PATH : String = "scriptDataPath";	
	var SCRIPT_META_TAGS : String = "scriptMetaTags";

	var SUPPORTED_SCRIPT_TYPES : String = "QuerySupportedScripts";
	var QUERY_ACTIVE_WORKBENCHES : String = "QueryActiveWorkbenches";
	var MANAGE_WORKBENCH : String = "ManageWorkbench";

	private function getSupportedScriptsListElement() : SelectElement {
		return (cast Browser.document.getElementById(SUPPORTED_SCRIPT_LIST_ELEMENT));
	}

	public var supportedScriptsStream(default, null) : Deferred<QuerySupportedScript>;
	public var queryActiveWorkbenchesStream(default, null) : Deferred<QueryActiveWorkbenches>;
	public function new(project : Project){
		trace("Instantiating project workbench " + haxe.Json.stringify(project));
		var stream : Stream<Dynamic> = 
				MBooks_im.getSingleton().initializeElementStream(
					cast getSaveWorkbench()
					, "click"
					);
		stream.then(saveWorkbench); 
		this.selectedProject = project;
		this.selectedScriptType = "UnsupportedScriptType";
		supportedScriptsStream = new Deferred<QuerySupportedScript>();
		supportedScriptsStream.then(processSupportedScripts);
		queryActiveWorkbenchesStream = new Deferred<QueryActiveWorkbenches>();
		queryActiveWorkbenchesStream.then(processQueryActiveWorkbenches);
		querySupportedScripts();
		
	}


	private function saveWorkbench(ev : Event){
		trace("Saving workbench ");
		//Load the upload script and save the workbench.
		var file = getScriptDataElement().files[0];
		var reader = new FileReader();
		var stream : Stream<Dynamic>  = 
				MBooks_im.getSingleton().initializeElementStream(
					cast reader
					, "load"
				);
		stream.then(uploadScript);
		reader.readAsDataURL(file);
	}
	private function read(anId){
		try {
			var payload = getPayloadFromUI(Read, "");
			payload.workbenchId = anId;
			trace("Reading workbench");
			MBooks_im.getSingleton().doSendJSON(payload);			
		}catch(err : Dynamic){
			trace("Error " + err);		
		}
	}

	private function saveWorkbenchModel(scriptData : String){
		try {
			if(scriptData == null || scriptData == "") {
				trace("Nothing to save");
				return;
			}
			var crudType = getCrudType();
			var payload = getPayloadFromUI(crudType, scriptData);
			trace("Saving workbench model " + haxe.Json.stringify(payload));
			MBooks_im.getSingleton().doSendJSON(payload);						
		}catch(err : Dynamic){
			trace ("Error saving workbench " + err);
		}
	}

	private function getCrudType() : WorkbenchCrudType {
		if(getWorkbenchIdFromUI() == null || getWorkbenchIdFromUI() == "") {
			return Create;
		}else {
			return WrkBench_Update;
		}
	}
	private function uploadScript(ev : Event) {
		trace ("Uploading script " + ev);
		try {
			var reader : FileReader = cast ev.target;
			saveWorkbenchModel(reader.result);
		}
	}

	private function getProjectWorkbenchListElement()  : InputElement {
		return (cast Browser.document.getElementById(PROJECT_WORKBENCH_LIST));
	}
	private function getWorkbenchIdElement() : InputElement {
		return (cast Browser.document.getElementById(WORKBENCH_ID_ELEMENT));

	}
	private function getWorkbenchIdFromUI() {
		return getWorkbenchIdElement().value;
	}
	private function setWorkbenchIdFromMessage(wid) {
		getWorkbenchIdElement().value = wid;
	}
	private function getScriptTypeElement() : InputElement {
		return (cast Browser.document.getElementById(SUPPORTED_SCRIPT_LIST_ELEMENT));
	}

	private function getScriptTypeFromUI() : String {
		return selectedScriptType;
	}
	private function getScriptSummaryElement() : InputElement {
		return (cast Browser.document.getElementById(SCRIPT_SUMMARY));
	}

	private function getScriptSummaryFromUI():String{
		return (getScriptSummaryElement().value);
	}

	private function setScriptSummaryFromMessage(aMessage: String) {
		getScriptSummaryElement().value = aMessage;
	}
	private function getScriptDataElement() : InputElement {
		return (cast Browser.document.getElementById(SCRIPT_UPLOAD_ELEMENT));
	}
	//Default values
	private function getNumberOfCoresFromUI() : Int {
		return 2;
	}
	private function getScriptDataPathFromUI() : String {
		return null;
	}
	private function getJobStartDateFromUI() : String {
		return null;
	}
	private function getJobEndDateFromUI() : String {
		return null;
	}
	private function toString(crudType : WorkbenchCrudType) {
		switch(crudType) {
			case Create: return "Create";
			case WrkBench_Update : return "WrkBench_Update";
			case Delete : return "Delete";
			case Read : return "Read";
		}
	}
	private function getPayloadFromUI(crudType : WorkbenchCrudType, scriptData) : PrjWorkbench {

		var result : PrjWorkbench =  {
			crudType : toString(crudType)
			, workbenchId : getWorkbenchIdFromUI()
			, uniqueProjectId : selectedProject.projectID
			, scriptType : getScriptTypeFromUI()
			, scriptSummary : getScriptSummaryFromUI()
			, scriptData : scriptData
			, numberOfCores : getNumberOfCoresFromUI()
			, scriptDataPath : getScriptDataPathFromUI()
			, jobStartDate : getJobStartDateFromUI()
			, jobEndDate : getJobEndDateFromUI()
			, nickName : MBooks_im.getSingleton().getNickName()
			, commandType : "ManageWorkbench"
		};
		return result;
	}


	private function getSaveWorkbench() : ButtonElement {
		return (cast Browser.document.getElementById(SAVE_WORKBENCH));
	}

	private function processSupportedScripts(supportedScripts : QuerySupportedScript)  : Void{
		trace("Process supported scripts  " + haxe.Json.stringify(supportedScripts));
		var supportedScriptListElement : SelectElement 
				= getSupportedScriptsListElement();
		if(supportedScriptListElement == null){
			throw "Script type list element is not defined";
		}
		for (sType in supportedScripts.scriptTypes) {
			var optionElement : OptionElement = 
				cast (Browser.document.getElementById(sType));
			if(optionElement == null){
				optionElement = 
					cast Browser.document.createOptionElement();
				optionElement.id = sType;
				optionElement.text = sType;
				supportedScriptListElement.appendChild(optionElement);
				var supportedScriptListStream = 
					MBooks_im.getSingleton().initializeElementStream(
						cast optionElement
						, "click"
						);
				supportedScriptListStream.then(processScriptTypeSelected);


			}else {
				trace("Option element exists " + sType);
			}	
		}
		queryWorkbenches();// Need a better place for this.
	}

	private function querySupportedScripts(){
		trace("Query supported scripts");
		var payload : QuerySupportedScript = 
			{
				nickName : MBooks_im.getSingleton().getNickName()
				, commandType : SUPPORTED_SCRIPT_TYPES
				, scriptTypes : []

			};
		MBooks_im.getSingleton().doSendJSON(payload);
	}


	private function queryWorkbenches() {
		try {
			trace ("Query all active workbenches for " + this.selectedProject.projectID);
			var payload : QueryActiveWorkbenches = 
				{
					nickName : MBooks_im.getSingleton().getNickName()
					, projectId : selectedProject.projectID
					, commandType : QUERY_ACTIVE_WORKBENCHES
					, workbenches : []
				};
			MBooks_im.getSingleton().doSendJSON(payload);		
		}catch(err : Dynamic){
			trace("Error query workbenches " + err);
		}
	}
	private function processQueryActiveWorkbenches(queryActiveWorkbenches : QueryActiveWorkbenches) {
		trace("Processing query active workbenches " + queryActiveWorkbenches);
		var workbenches : Array<PrjWorkbench> = queryActiveWorkbenches.workbenches;
		var workbenchesUI : InputElement = getProjectWorkbenchListElement();
		for (wrk in workbenches) {
			var wId : String = wrk.workbenchId;
			var optionElement : OptionElement = 
				cast Browser.document.getElementById(wId);
			if(optionElement == null){
				optionElement = cast (Browser.document.createOptionElement());
				optionElement.id = wId;
				optionElement.text = wId;
				var optionSelectedStream = 
					MBooks_im.getSingleton().initializeElementStream(
						cast optionElement
						, "click"
					);
				optionSelectedStream.then(processWorkbenchSelected);
				workbenchesUI.appendChild(optionElement);
			}else {
				trace("Element already exists " + wId);
			}
		}
	}

	private function processWorkbenchSelected(ev : Event) {
		var selectionElement : OptionElement =
			cast ev.target;
		var selectionId = selectionElement.id;
		read(selectionId);
	}

	private function processManageWorkbench(incomingMessage : PrjWorkbench) {
		trace("Processing manage workbench " + incomingMessage);
		var crudType : String = incomingMessage.crudType;
		copyIncomingValues(incomingMessage);
	}
	private function copyIncomingValues(incomingMessage) {
		this.setWorkbenchIdFromMessage(incomingMessage.workbenchId);
		this.setScriptSummaryFromMessage(incomingMessage.scriptSummary);
		processScriptData(incomingMessage.scriptData);
	}
	private function processScriptData(scriptData: String){
		trace("Processing script data " + scriptData);
	}
	//Initialization populates the workbench list
	//for the selected project. (User can select a single project at any time)
	//Populates the types of scripts supported.
	//Selecting a workbench, displays the details.
	//ideally syntax highlighted script.
	//Run the script
	//Console or a display area displays the result.

	//Save saves/inserts a new script.
	//Delete deletes.
	//Update: is automatic with a timer.
	
	//Actions - generate json actions to be handled.
	//Actions: -- User triggers them
	//Save: Saves or inserts a new script.
	//Delete: delete.
	//Update: Updates the script.
	//Run : action to run the script.
	//Select a workbench.
	//query all workbenches.
	//query all supported scripts

	//Actions -- system generated based on user preferences
	//Automatic workspace saver timer thread.
	//User should be able to pause the thread
	//anytime.

	//Events:
	//project selected. -> triggers query all workbenches.
	//script type selected -> triggers any syntax rules for the script.
	//Supported script types selected
	//workbench list published.
	//workbench selected.
	//save button clicked 
	//delete button clicked
	//update button clicked
	//run button pressed 


	//private variables.
	var selectedProject : Project;
	var workbenchId : String;
	//To help classify the script
	var scriptData : String;
	var numberOfCores : Int;
	var scriptDataPath : String;
	var newWorkBench : Bool;
	var scriptMetaTags : String;
	//A better pnemonic than the uuid.
	var scriptSummary : String;
	//How to access the currently selected option from an input element?
	var selectedScriptType : String;
	var autosave : Bool; //Enable when workbench has been inserted.

	private function processScriptTypeSelected(ev : Event) {
		trace("Script type selected " + ev);
		try {
			var selectionElement : OptionElement = 
				cast ev.target;
			selectedScriptType = selectionElement.id;
		}

	}
}