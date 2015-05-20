package model;
import util.Util;
import haxe.Json;
import haxe.Utf8;
import haxe.Timer;
import haxe.ds.ArraySort;
import js.html.Element;
import haxe.crypto.BaseCode;
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
import js.Browser;
import js.Lib;
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
	// A special option element that indicates that no 
	// option has been selected.	
	var CHOOSE_WORKBENCH 	   : String = "chooseWorkbench";
	var CHOOSE_SUPPORTED_SCRIPT : String = "chooseSupportedScriptType";
	var PROJECT_WORKBENCH_LIST : String = "projectWorkbenches";
	var UPDATE_WORKBENCH   : String = "updateWorkbench";
	var CREATE_WORKBENCH   : String = "insertWorkbench";
	var DELETE_WORKBENCH   : String = "deleteWorkbench";
	var EXECUTE_WORKBENCH  : String = "executeScript";
	var CLEAR_FIELDS 	   : String = "clearFields";

	var SUPPORTED_SCRIPT_LIST_ELEMENT : String = "supportedScriptTypes";
	var WORKBENCH_ID_ELEMENT : String = "workbenchId";
	var SCRIPT_DATA_ELEMENT : String = "scriptData";
	var SCRIPT_UPLOAD_ELEMENT : String = "uploadScript";
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
	public var manageWorkbenchStream(default, null) : Deferred<PrjWorkbench>;
	public function new(project : Project){
		trace("Instantiating project workbench " + haxe.Json.stringify(project));
		var stream : Stream<Dynamic> = 
				MBooks_im.getSingleton().initializeElementStream(
					cast getUpdateWorkbench()
					, "click"
					);
		stream.then(updateWorkbench);
		var createStream : Stream<Dynamic>  = 
			MBooks_im.getSingleton().initializeElementStream(
				cast getCreateWorkbench()
				, "click"
				);
		createStream.then(createWorkbench);
		var deleteStream : Stream<Dynamic> = 
			MBooks_im.getSingleton().initializeElementStream(
				cast getDeleteWorkbench()
				, "click"
				);
		deleteStream.then(deleteWorkbench);
		var clearFieldsStream : Stream<Dynamic> =
			MBooks_im.getSingleton().initializeElementStream (
				cast getClearFields()
				, "click"
				);
		clearFieldsStream.then(clearFields);
		this.selectedProject = project;
		this.selectedScriptType = "UnsupportedScriptType";
		supportedScriptsStream = new Deferred<QuerySupportedScript>();
		supportedScriptsStream.then(processSupportedScripts);
		queryActiveWorkbenchesStream = new Deferred<QueryActiveWorkbenches>();
		manageWorkbenchStream = new Deferred<PrjWorkbench>();
		queryActiveWorkbenchesStream.then(processQueryActiveWorkbenches);
		querySupportedScripts();
		manageWorkbenchStream.then(processManageWorkbench);
	}


	private function createWorkbench(ev : Event) {
		trace("Creating workbench");
		//Load the upload script and save the workbench.
		//Clear the workbench id, to indicate that 
		//this is a new entry

		var file = getScriptDataElement().files[0];
		var reader = new FileReader();
		var stream : Stream<Dynamic>  = 
				MBooks_im.getSingleton().initializeElementStream(
					cast reader
					, "load"
				);
		stream.then(uploadScript);
		reader.readAsText(file);
	}
	private function deleteWorkbench(ev : Event) {
		trace("Deleting workbench");

	}
	private function updateWorkbench(ev : Event){
		trace("Update workbench ");
		//Load the upload script and save the workbench.
		var file = getScriptDataElement().files[0];
		var reader = new FileReader();
		var stream : Stream<Dynamic>  = 
				MBooks_im.getSingleton().initializeElementStream(
					cast reader
					, "load"
				);
		stream.then(uploadScript);
		reader.readAsText(file);
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
	private function clearWorkbenchId() {
		getWorkbenchIdElement().value = "";
	}
	private function setWorkbenchIdFromMessage(wid) {
		getWorkbenchIdElement().value = wid;
	}
	private function getScriptTypeElement() : InputElement {
		return (cast Browser.document.getElementById(SUPPORTED_SCRIPT_LIST_ELEMENT));
	}

	private function clearSupportedScriptList() {
		var element : OptionElement = 
			cast Browser.document.getElementById(CHOOSE_SUPPORTED_SCRIPT);
		element.selected = true;
	}
	private function clearWorkbenchesList() {
		var element : OptionElement = 
			cast Browser.document.getElementById(CHOOSE_WORKBENCH);	
		element.selected = true;
	}
	private function getScriptTypeFromUI() : String {
		return selectedScriptType;
	}
	private function setScriptTypeFromMessage(aScriptType : String){
		var element : OptionElement
			 = cast Browser.document.getElementById(aScriptType);
		element.selected = true;
	}
	private function getScriptSummaryElement() : InputElement {
		return (cast Browser.document.getElementById(SCRIPT_SUMMARY));
	}

	private function getScriptSummaryFromUI():String{
		return (getScriptSummaryElement().value);
	}

	private function clearScriptSummary() {
		getScriptSummaryElement().value = "";
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
		var lCrudType : String = toString(crudType);
		if(lCrudType == "Create"){
			if(scriptData == null || scriptData == "") {
				trace("Nothing to save");
				throw "Inserting object with no script data ";
			}		
		}
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


	private function getUpdateWorkbench() : ButtonElement {
		return (cast Browser.document.getElementById(UPDATE_WORKBENCH));
	}
	private function getCreateWorkbench() : ButtonElement {
		return (cast Browser.document.getElementById(CREATE_WORKBENCH));
	}
	private function getDeleteWorkbench() : ButtonElement {
		return (cast Browser.document.getElementById(DELETE_WORKBENCH));
	}
	private function getExecuteWorkbench() : ButtonElement {
		return (cast Browser.document.getElementById(EXECUTE_WORKBENCH));
	}
	private function getClearFields() : ButtonElement {
		return (cast Browser.document.getElementById(CLEAR_FIELDS));
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
	private function insertToActiveWorkbenches(workbenchesUI : InputElement, wrk : PrjWorkbench) 
			: OptionElement{
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
		return optionElement;
	}

	private function processQueryActiveWorkbenches(queryActiveWorkbenches : QueryActiveWorkbenches) {
		trace("Processing query active workbenches " + queryActiveWorkbenches);
		var workbenches : Array<PrjWorkbench> = queryActiveWorkbenches.workbenches;
		var workbenchesUI : InputElement = getProjectWorkbenchListElement();
		var firstElement  = true;
		for (wrk in workbenches) {
			var optElement = insertToActiveWorkbenches(workbenchesUI, wrk);
			if(firstElement) {
				optElement.selected = true;
				read(wrk.workbenchId);
				firstElement = false;
			}
		}

	}

	private function deleteFromActiveWorkbenches(workbenchesUI, wrk: PrjWorkbench){
		var optionElement = cast Browser.document.getElementById(wrk.workbenchId);
		if(optionElement != null){
			workbenchesUI.removeChild(optionElement);
		}else {
			trace("Element not found " + wrk);
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
		setWorkbenchIdFromMessage(incomingMessage.workbenchId);
		if(crudType == "Create"){
			insertToActiveWorkbenches(getProjectWorkbenchListElement()
							, incomingMessage);
		}else if (crudType == "Delete"){
			deleteFromActiveWorkbenches(getProjectWorkbenchListElement()
				, incomingMessage);
		}
		copyIncomingValues(incomingMessage);
	}
	private function copyIncomingValues(incomingMessage) {
		this.setWorkbenchIdFromMessage(incomingMessage.workbenchId);
		this.setScriptSummaryFromMessage(incomingMessage.scriptSummary);
		this.setScriptTypeFromMessage(incomingMessage.scriptType);
		processScriptData(incomingMessage.scriptType, incomingMessage.scriptData);
	}
	private function clearFields(ev : Event) {
		clearWorkbenchId();
		clearScriptSummary();
		clearSupportedScriptList();
		clearWorkbenchesList();

	}
	private function processScriptData(scriptType : String, scriptData: String){
		trace("Processing script data " + scriptData);
		if(scriptType == "ThreeJS") {
			try {
				handleThreeJS(scriptData);
			}catch(err : Dynamic){
				trace("Error handling threejs " +  err);	
			}
			
		}else if (scriptType == "ThreeJS_JSON"){
			handleThreeJSJSON(scriptData);
		}else {
			trace("Invalid script type " + scriptType);
		}
	}
	private function handleThreeJS(scriptData : String){
		trace("processing three js");
	}

	private function handleThreeJSJSON(scriptData : String) {
		trace("Processing three js json loading");
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