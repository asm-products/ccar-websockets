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
		private static var LIST_CLASS : String = "CCAR.Scenario.List";
		private static var UPLOAD_BUTTON_CLASS : String =  "CCAR.Scenario.Button";
		private static var NAME : String = "Scenario Name";
		private static var TEXT : String = "Scenario Text";
		private static var LIST : String = "Scenario List";
		private static var UPLOAD_BUTTON : String = "Save Scenario";
		private var document : Document;
		private var model : model.CCAR;
		private var ccarDictionary : Map<String, model.CCAR>;
		public function new(a : model.CCAR) {
			this.model = a;
			document = Browser.document;
			ccarDictionary = new Map<String, model.CCAR>();
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
			trace("Creating CCAR form ");
			var div : DivElement = Util.createDivTag(document, CCAR_DIV_TAG);
			Util.createElementWithLabel(document, div
				, NAME_CLASS
				, NAME);
			Util.createTextAreaElementWithLabel(document, div 
				, TEXT_CLASS
				, TEXT);
			Util.createSelectElement(document, div , LIST_CLASS, CCAR_DIV_TAG + LIST);
			Util.createButtonElement(document, div, UPLOAD_BUTTON_CLASS, UPLOAD_BUTTON);
			var selectElement : SelectElement = getCCARListElement();
			var buttonElement : ButtonElement = 
			cast document.getElementById(UPLOAD_BUTTON);
			var listStream : Stream<Dynamic> = MBooks.getMBooks().initializeElementStream(selectElement, "click");
			listStream.then(selectScenario);			
			parent.appendChild(div);
			buttonElement.onclick = uploadCCARData;
			var scenarioNameElement : InputElement = cast document.getElementById(NAME_CLASS);
			var scenarioNameStream : Stream<Dynamic> = MBooks.getMBooks().initializeElementStream(scenarioNameElement, "keyup");
			var keydownStream : Stream<Dynamic> = MBooks.getMBooks().initializeElementStream(scenarioNameElement, "keydownn");
			scenarioNameStream.then(scenarioNameUpdate);
			scenarioNameElement.focus();
			keydownStream.then(scenarioNameUpdate);
		}

		private function getCCARListElement() : SelectElement {
			var selectElement : SelectElement = 
			cast document.getElementById(CCAR_DIV_TAG + LIST);
			return selectElement;			
		}
		//When the user modifies the scenario name directly, if the 
		//the name is not in the dictionary, then the list box should be disabled,
		//or we may lose the scenario
		public function scenarioNameUpdate(ev : KeyboardEvent) : Void {
			trace("Inside scenario name update " + ev + "->" + ev.keyCode);
			if(ev.keyCode == 10 || ev.keyCode == 13 || ev.keyCode == 9){
				var scenarioNameElement : InputElement = cast document.getElementById(NAME_CLASS);
				if(!scenarioNameExists(scenarioNameElement.value)){
					trace("New element " + scenarioNameElement.value);
					var selectElement : SelectElement = getCCARListElement();
					selectElement.disabled = true;
				}else {
					trace("Element found so we are going to update an existing element " + scenarioNameElement.value);
				}
			}
		}

		
		private function scenarioNameExists(sName : String) : Bool {
			return ccarDictionary.exists(sName);
		}
		public function uploadCCARData(ev : Event) {
			trace("Uploading ccar data");
			var commandType : String = "CCARUpload";
			var ccarOperation = {
	 				tag : "Create" , //Tag is needed for the aeson objects.
	 				contents : []
	 			};
	 			copyValues();
	 			var payload = {
	 				commandType : commandType
	 				, ccarOperation : ccarOperation
	 				, uploadedBy : this.model.creator
	 				, ccarData : this.model
	 			};
	 			MBooks.getMBooks().doSendJSON(Json.stringify(payload));
	 			var selectElement : SelectElement  = getCCARListElement();
	 			selectElement.disabled = false;
	 		}

	 		private function selectScenario(ev : Event){
	 			trace("Selecting ccar element " + ev);
	 			var selectElement : SelectElement = cast ev.target;
	 			trace("Event target " + ev.target + " " + selectElement.value);
	 			var ccarText = ccarDictionary.get(selectElement.value);
	 			var textAreaElement : TextAreaElement = cast document.getElementById(TEXT_CLASS);
	 			trace("Selected text " + ccarText.scenarioText);
	 			textAreaElement.value = ccarText.scenarioText;
	 			var scenarioNameElement : InputElement = cast document.getElementById(NAME_CLASS);
	 			scenarioNameElement.value = selectElement.value;

	 		}
	 		public function queryAllCCARs() {
	 			trace("Querying all ccar objects");
	 			var commandType : String = "CCARUpload";
	 			var ccarOperation = {
	 				tag : "QueryAll" , //Tag is needed for the aeson objects.
	 				contents : this.model.creator
	 			};
	 			var payload = {
	 				commandType : commandType
	 				, ccarOperation : ccarOperation
	 				, uploadedBy : this.model.creator
	 				, ccarData : this.model
	 			};
	 			MBooks.getMBooks().doSendJSON(Json.stringify(payload));
	 		}


	 		public function populateList(document: Document
	 			, elements : Array<model.CCAR>){
	 			trace("Populate the elements in the list " + elements.length);
	 			for (i in elements){
	 				trace("Dictionary " + i);
	 				ccarDictionary[i.scenarioName] = i;
	 				trace("Element added "+ ccarDictionary.get(i.scenarioName) + " " + i.scenarioName);
	 			}
	 			var list : SelectElement = cast document.getElementById(CCAR_DIV_TAG + LIST);
	 			var options : List<OptionElement> = new List<OptionElement>();
	 			for ( i in elements){
	 				if(i.scenarioName != "") {
	 					var option : OptionElement = 
	 					cast document.getElementById(i.scenarioName);
	 					if (option == null){
	 						option = document.createOptionElement();
	 						option.id = OPTION + i.scenarioName;
	 						option.text = i.scenarioName;
	 						list.appendChild(option);
	 					}
	 					}else {
	 						trace("Ignoring empty scenario name " + i);
	 					}

	 				}


	 			}

	 		}