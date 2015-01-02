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
	private var mbooks : MBooks ;
	public function new(a : model.CCAR, m : MBooks) {
		this.model = a;
		document = Browser.document;
		mbooks = m;	
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
		Util.createInputElement(document, div
		, NAME_CLASS
		, NAME);
		Util.createTextAreaElement(document, div 
		, TEXT_CLASS
		, TEXT);
		Util.createSelectElement(document, div , LIST, LIST_CLASS);
		Util.createButtonElement(document, div, UPLOAD_BUTTON_CLASS, UPLOAD_BUTTON);
		var buttonElement : ButtonElement = 
					cast document.getElementById(UPLOAD_BUTTON);
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
		mbooks.doSendJSON(Json.stringify(payload));

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
		mbooks.doSendJSON(Json.stringify(payload));
	}


	public static function populateList(document: Document
		, elements : List<model.CCAR>){
		trace("Populate the elements in the list " + elements);
		var list : SelectElement = cast document.getElementById(LIST);
		for ( i in elements){
			var option : OptionElement = 
				cast document.getElementById(i.scenarioName);
			if (option == null){
				trace("Creating option element");
				option = document.createOptionElement();
				option.id = i.scenarioName;
				list.appendChild(option);
			}

		}
	}


}