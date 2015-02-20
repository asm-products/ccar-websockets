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

	import js.Lib.*;
	import util.*;
	/**
	* CCAR view has 3 components:
	* The creator id
	* A scenario name (required)
	* A scenario (the actual scenario)
	* A list of scenarios for a creator.
	*/
	class CCAR {
	private static var OPTION : String = "SelectElementOptionPrefix";
	private static var CCAR_DIV_TAG : String = "CCAR.Scenario";
	private static var NAME_CLASS  : String = "CCAR.Scenario.Name";
	private static var TEXT_CLASS : String = "CCAR.Scenario.Text";
	private static var PARSED_CLASS : String = "CCAR.Scenario.ParsedScenario";
	private static var LIST_CLASS : String = "CCAR.Scenario.List";
	private static var UPLOAD_BUTTON_CLASS : String =  "CCAR.Scenario.Button";
	//All buttons look the same, but this will have to change : TBD
	private static var DELETE_BUTTON_CLASS : String = "CCAR.Scenario.Button"; 
	private static var NAME : String = "Scenario Name";
	private static var TEXT : String = "Scenario Text";
	private static var PARSED_TEXT : String = "Parsed output";
	private static var LIST : String = "Scenario List";
	private static var UPLOAD_BUTTON : String = "Save Scenario";
	private static var DELETE_BUTTON : String = "Delete Scenario";

	private static var SEND_MESSAGE_TEXT_AREA : String = "Messages";
	private static var SEND_MESSAGE_TEXT_AREA_CLASS : String = "CCAR.Scenario.GroupCommunication.MessageArea";
	private static var SEND_MESSAGE_INPUT_AREA : String = "Input message";
	private static var SEND_MESSAGE_INPUT_AREA_CLASS : String = "CCAR.Scenario.GroupCommunication.InputArea";
	private static var SEND_MESSAGE_BUTTON_CLASS : String = "CCAR.Scenario.GroupCommunication.SendMessageButton";
	private static var SEND_MESSAGE_BUTTON : String = "Send";
	private static var MEMBER_LIST : String = "Members List";

	private var document : Document;
	private var model : model.CCAR;
	private var ccarDictionary : Map<String, model.CCAR>;
	public function new(a : model.CCAR) {
		this.model = a;
		document = Browser.document;
		ccarDictionary = new Map<String, model.CCAR>();
	}

	public function updateMessageAreaText(aValue) {
		var inputElement : InputElement = cast document.getElementById(SEND_MESSAGE_TEXT_AREA_CLASS);
		inputElement.value = inputElement.value + aValue;
	}
	private function getMessageTextAreaContents() : String {
		var inputElement : InputElement = cast document.getElementById(SEND_MESSAGE_TEXT_AREA_CLASS);
		return inputElement.value;
	}
	private function getMessageInputAreaContents() : String {
		var inputElement : InputElement = cast document.getElementById(SEND_MESSAGE_INPUT_AREA_CLASS);
		return inputElement.value;
	}
	private function getScenarioName() : String {
		var inputElement : InputElement = cast document.getElementById(NAME_CLASS);
		return inputElement.value;
	}
	//Copy values from the view into the model;
	private function copyValues(){
		var element : InputElement = cast document.getElementById(NAME_CLASS);
		if (element != null){
			model.setScenarioName(element.value);
		}
		var areaElement : TextAreaElement  = cast document.getElementById(TEXT_CLASS);
		if(areaElement != null){
			model.setScenarioText(areaElement.value);
		}	
	}
		//Copy the values from the model to the view.
	private function setValues(){
		var element : InputElement = cast document.getElementById(NAME);
		if(element != null){
			element.value = model.scenarioName;
		}
		var areaElement : TextAreaElement = 
		cast document.getElementById(TEXT);
		if(areaElement != null) {
			areaElement.value = model.scenarioText;
		}
	} 

	public function createCCARForm(parent : DivElement){
		//trace("Creating CCAR form ");
		var div : DivElement = Util.createDivTag(document, CCAR_DIV_TAG);
		Util.createElementWithLabel(document, div
			, NAME_CLASS
			, NAME);
		Util.createTextAreaElementWithLabel(document, div 
			, TEXT_CLASS
			, TEXT);
		var scenarioAreaElement : TextAreaElement = cast document.getElementById(TEXT_CLASS);
		var scenarioAreaElementStream = MBooks.getMBooks().initializeElementStream(scenarioAreaElement, "change");
		scenarioAreaElementStream.then(scenarioAreaElementUpdate);

		trace("Class name " + PARSED_CLASS);
		Util.createTextAreaElementWithLabel(document, div
			, PARSED_CLASS
			, PARSED_TEXT);
		Util.createSelectElement(document, div , LIST_CLASS, CCAR_DIV_TAG + LIST);
		Util.createButtonElement(document, div, UPLOAD_BUTTON_CLASS, UPLOAD_BUTTON);
		Util.createButtonElement(document, div, DELETE_BUTTON_CLASS, DELETE_BUTTON);
		Util.createTextAreaElementWithLabel(document, div, SEND_MESSAGE_TEXT_AREA_CLASS, SEND_MESSAGE_TEXT_AREA);
		Util.createTextAreaElementWithLabel(document, div, SEND_MESSAGE_INPUT_AREA_CLASS, SEND_MESSAGE_INPUT_AREA);
		var messageInputElement : TextAreaElement = cast document.getElementById(SEND_MESSAGE_INPUT_AREA_CLASS);
		messageInputElement.rows = 2;
		Util.createButtonElement(document, div, SEND_MESSAGE_BUTTON_CLASS, SEND_MESSAGE_BUTTON);
		var sendButtonElement : ButtonElement = cast document.getElementById(SEND_MESSAGE_BUTTON);
		var sendButtonStream = MBooks.getMBooks().initializeElementStream(sendButtonElement, "click");
		sendButtonStream.then(sendPrivateMessage);
		var selectElement : SelectElement = getCCARListElement();
		var buttonElement : ButtonElement = 
		cast document.getElementById(UPLOAD_BUTTON);
		var scenarioUploadButtonStream = MBooks.getMBooks().initializeElementStream(buttonElement, "click");
		scenarioUploadButtonStream.then(uploadCCARData);

		var deleteButton : ButtonElement = cast document.getElementById(DELETE_BUTTON);
		var deleteButtonStream = MBooks.getMBooks().initializeElementStream(deleteButton, "click");
		deleteButtonStream.then(deleteScenario);
		var listStream : Stream<Dynamic> = MBooks.getMBooks().initializeElementStream(selectElement, "change");
		listStream.then(selectScenario);	
		var keyboardListStream : Stream<Dynamic> = MBooks.getMBooks().initializeElementStream(selectElement, "keydown");
		keyboardListStream.then(selectScenarioKb);		
		parent.appendChild(div);
		
		var scenarioNameElement : InputElement = cast document.getElementById(NAME_CLASS);
		var scenarioNameStream : Stream<Dynamic> = MBooks.getMBooks().initializeElementStream(scenarioNameElement, "keyup");
		var keydownStream : Stream<Dynamic> = MBooks.getMBooks().initializeElementStream(scenarioNameElement, "keydown");
		scenarioNameStream.then(scenarioNameUpdate);
		scenarioNameElement.focus();
		keydownStream.then(scenarioNameUpdate);
	}

	private function getCCARListElement() : SelectElement {
		var selectElement : SelectElement = 
		cast document.getElementById(CCAR_DIV_TAG + LIST);
		return selectElement;			
	}

	public function scenarioAreaElementUpdate(ev: Event) : Void {
		trace("scenario area element update " + ev);
		uploadCCARData(ev);
	}
	//When the user modifies the scenario name directly, if the 
	//the name is not in the dictionary, then the list box should be disabled,
	//or we may lose the scenario
	public function scenarioNameUpdate(ev : KeyboardEvent) : Void {
		//trace("Inside scenario name update " + ev + "->" + ev.keyCode);
		var scenarioNameElement : InputElement = cast document.getElementById(NAME_CLASS);
		var selectElement : SelectElement = getCCARListElement();

		if(Util.isBackspace(ev.keyCode)){
			scenarioNameElement.value = "";
			selectElement.disabled = false;
		}
		if(Util.isSignificantWS(ev.keyCode)){
			if(!scenarioNameExists(scenarioNameElement.value)){
				//trace("New element " + scenarioNameElement.value);
				selectElement.disabled = true;
			}else {
					//trace("Element found so we are going to update an existing element " + scenarioNameElement.value);
			}
		}
	}

	private function safeGetScenario(sName : String) : model.CCAR {
		if(scenarioNameExists(sName)){
			return getScenario(sName);
			}else {
				throw ("Scenario not found " + sName);
			}
	}
	private function getScenario(sName : String) : model.CCAR {
		return ccarDictionary.get(sName);
	}
	private function scenarioNameExists(sName : String) : Bool {
		return ccarDictionary.exists(sName);
	}


	private function getDeleteOperation(sName : String) {
		if(scenarioNameExists(sName)){
			var ccarOperation = {
				tag : "Delete",
				contents : []
			};
			return ccarOperation;
		}else {
			throw ("Scenario not found to delete "  + sName);
		}
	}
	private function getOperation(sName : String)  {
			if(scenarioNameExists(sName)) {
			var ccarOperation = {
				tag : "Update",
				contents : []
			};
			return ccarOperation;
			}else {
				var ccarOperation = {
					tag : "Create",
					contents : []
				};
				return ccarOperation;
			}
			throw ("This should not happen. " + sName);
	}
	private function sendParseRequest(ccarModel : model.CCAR) {
		//trace("Sending parse request");
		var commandType : String = "ParsedCCARText";
		var payload  = {
			 nickName : MBooks.getMBooks().getNickName()
			, commandType : commandType
			, uploadedBy : ccarModel.creator
			, scenarioName : ccarModel.scenarioName
			, ccarText  : ccarModel.scenarioText
		};


		MBooks.getMBooks().doSendJSON(Json.stringify(payload));
	}

	public function deleteScenario(ev : Event){
		trace("Deleting ccar data");
		var nickName : String = MBooks.getMBooks().getNickName();
		var commandType : String = "CCARUpload";
		var scenarioName = getScenarioName();
		var ccarOperation = getDeleteOperation(scenarioName);
		copyValues();
		var payload = {
			nickName : nickName
			, commandType : commandType
			, ccarOperation : ccarOperation
			, uploadedBy : this.model.creator
			, ccarData : this.model
		};
		trace("Payload " + Json.stringify(payload));
		MBooks.getMBooks().doSendJSON(Json.stringify(payload));

	}

	public function sendPrivateMessage(ev : Event) {
		var commandType : String = "SendMessage";
		var nickName : String = MBooks.getMBooks().getNickName();
		var payload = {
			commandType : commandType
			, nickName : nickName
			, from : nickName
			, to : nickName 
			, privateMessage : getMessageInputAreaContents()
		}
		MBooks.getMBooks().doSendJSON(Json.stringify(payload));

	}
	public function uploadCCARData(ev : Event) {
		//trace("Uploading ccar data");
		var commandType : String = "CCARUpload";
		var scenarioName = getScenarioName();
		var ccarOperation = getOperation(scenarioName);
		copyValues();
		var payload = {
			nickName : MBooks.getMBooks().getNickName()
			, commandType : commandType
			, ccarOperation : ccarOperation
			, uploadedBy : this.model.creator
			, ccarData : this.model
		};
		MBooks.getMBooks().doSendJSON(Json.stringify(payload));
		var selectElement : SelectElement  = getCCARListElement();
		selectElement.disabled = false;
	}

	private function selectScenarioKb(ev : KeyboardEvent){
		trace("Keydown event " + ev.keyCode);
		if(Util.isUpOrDown(ev.keyCode)){
			var selectElement : SelectElement = cast ev.target;
			trace("Event target " + ev.target + " " + selectElement.value);
			var ccarText = ccarDictionary.get(selectElement.value);
			var textAreaElement : TextAreaElement = cast document.getElementById(TEXT_CLASS);
			//trace("Selected text " + ccarText.scenarioText);
			//trace("textAreaElement dimensions " + textAreaElement.rows + " -> " + textAreaElement.cols);
			textAreaElement.value = ccarText.scenarioText;
			var scenarioNameElement : InputElement = cast document.getElementById(NAME_CLASS);
			scenarioNameElement.value = selectElement.value;
			sendParseRequest(ccarText);
		}
	}

	private function selectScenario(ev : Event){
		//trace("Selecting ccar element " + ev);
		var selectElement : SelectElement = cast ev.target;
		//trace("Event target " + ev.target + " " + selectElement.value);
		var ccarText = ccarDictionary.get(selectElement.value);
		var textAreaElement : TextAreaElement = cast document.getElementById(TEXT_CLASS);
		//trace("Selected text " + ccarText.scenarioText);
		//trace("textAreaElement dimensions " + textAreaElement.rows + " -> " + textAreaElement.cols);
		textAreaElement.value = ccarText.scenarioText;
		var scenarioNameElement : InputElement = cast document.getElementById(NAME_CLASS);
		scenarioNameElement.value = selectElement.value;
		sendParseRequest(ccarText);
	}
	public function setParsedScenarioText(jsonString : String) : Void {
		trace("Setting json string " + jsonString);
		var parsed : TextAreaElement = cast document.getElementById(PARSED_CLASS);
		trace("Using " + parsed + "-> " + PARSED_CLASS);
		parsed.value = jsonString;
	}
	
	public function queryAllCCARs() {
		//trace("Querying all ccar objects");
		var commandType : String = "CCARUpload";
		var ccarOperation = {
		tag : "QueryAll" , //Tag is needed for the aeson objects.
			contents : this.model.creator
		};
		var payload = {
			nickName : MBooks.getMBooks().getNickName()
			, commandType : commandType
			, ccarOperation : ccarOperation
			, uploadedBy : this.model.creator
			, ccarData : this.model
		};
		MBooks.getMBooks().doSendJSON(Json.stringify(payload));
	}


 	public function populateList(document: Document
 			, elements : Array<model.CCAR>, newElement : model.CCAR){
 			//trace("Populate the elements in the list " + elements.length);
 			for (i in elements){
 				//trace("Dictionary " + i);
 				ccarDictionary[i.scenarioName] = i;
 				trace("Element added "+ ccarDictionary.get(i.scenarioName) + " " + i.scenarioName);
 			}
 			var list : SelectElement = cast document.getElementById(CCAR_DIV_TAG + LIST);
 			var options : List<OptionElement> = new List<OptionElement>();
 			var index : Int = 0;
 			var selectedModel : model.CCAR = null;
 			if(newElement != null && newElement.deleted == true) {
 				var optionElement = document.getElementById(OPTION + newElement.scenarioName);
 				list.removeChild(optionElement);
 			}
 			for ( key in ccarDictionary.keys()){
 				var i : model.CCAR = ccarDictionary[key];
 				trace("Populating " + i.scenarioName + "->" + i.deleted);
 				if(i.deleted == true){
 					continue;
 				}
 				if(i.scenarioName != "") {
 					var option : OptionElement = 
 					cast document.getElementById(i.scenarioName);
 					if (option == null){
 						option = document.createOptionElement();
 						option.id = OPTION + i.scenarioName;
 						option.text = i.scenarioName;
 						if(newElement == null) {
 							if(selectedModel == null) {
 								selectedModel = i; 
 							}

 						} else {
 							//this is not a new element.
 						}
 							list.appendChild(option);
 						}
 				}else {
 					trace("Ignoring empty scenario name " + i);
 				}
 				index = index + 1;
 			}
				
			if(newElement != null){
				sendParseRequest(newElement);
				}else {
					newElement = selectedModel; 
					updateUIModel(newElement);
					sendParseRequest(newElement);
				}
 		}


 	private function updateUIModel(aModel : model.CCAR) {
			var ccarText = ccarDictionary.get(aModel.scenarioName);
			var textAreaElement : TextAreaElement = cast document.getElementById(TEXT_CLASS);
			//trace("Selected text " + ccarText.scenarioText);
			//trace("textAreaElement dimensions " + textAreaElement.rows + " -> " + textAreaElement.cols);
			textAreaElement.value = ccarText.scenarioText;
			var scenarioNameElement : InputElement = cast document.getElementById(NAME_CLASS);
			scenarioNameElement.value = aModel.scenarioName;

 	}
}

