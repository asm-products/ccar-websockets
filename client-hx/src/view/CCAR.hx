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
	import js.html.OptionElement;
	import haxe.ds.ObjectMap;

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
		private static var CCAR_DIV_TAG : String = "CCAR.Scenario";
		private static var NAME_CLASS  : String = "CCAR.Scenario.Name";
		private static var TEXT_CLASS : String = "CCAR.Scenario.Text";
		private static var LIST_CLASS : String = "CCAR.Scenario.List";
		private static var UPLOAD_BUTTON_CLASS : String =  "CCAR.Scenario.Button";
		private static var NAME : String = "Scenario Name";
		private static var TEXT : String = "Scenario Text";
		private static var LIST : String = "Scenario List";
		private static var UPLOAD_BUTTON : String = "Upload Scenario";
		private var document : Document;
		private var model : model.CCAR;
		private var ccarDictionary : ObjectMap<String, model.CCAR>;
		public function new(a : model.CCAR) {
			this.model = a;
			document = Browser.document;
			ccarDictionary = new ObjectMap<String, model.CCAR>();
		}
		//Copy values from the view into the model;
		private function copyValues(){
			var element : InputElement = cast document.getElementById(NAME);
			if (element != null){
				model.setScenarioName(element.value);
			}
			var areaElement : TextAreaElement  = cast document.getElementById(TEXT);
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
			Util.createSelectElement(document, div , LIST_CLASS, LIST);
			Util.createButtonElement(document, div, UPLOAD_BUTTON_CLASS, UPLOAD_BUTTON);
			var buttonElement : ButtonElement = 
						cast document.getElementById(UPLOAD_BUTTON);
			var selectElement : SelectElement = 
						cast document.getElementById(LIST);
			selectElement.onclick = selectScenario;
			parent.appendChild(div);
			buttonElement.onclick = uploadCCARData;
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

		}

		private function selectScenario(ev : Event){
			trace("Selecting ccar element " + ev);

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
				ccarDictionary.set(i.scenarioName, i);
			}
			var list : SelectElement = cast document.getElementById(LIST);
			var options : List<OptionElement> = new List<OptionElement>();
			for ( i in elements){
			if(i.scenarioName != "") {
				var option : OptionElement = 
					cast document.getElementById(i.scenarioName);
				if (option == null){
						trace("Creating option element " + i);
						option = document.createOptionElement();
						option.id = i.scenarioName;
						option.text = i.scenarioName;
						list.appendChild(option);
				}
			}else {
				trace("Ignoring empty scenario name " + i);
			}

			}
		

		}

	}