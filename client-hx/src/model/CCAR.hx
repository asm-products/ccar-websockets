package model;
import js.Browser;
import js.html.InputElement;
import js.html.TextAreaElement;
import js.html.ButtonElement;
import js.html.KeyboardEvent;
import js.html.Event;
import util.Util;
import promhx.Stream;
import promhx.Promise;
using promhx.haxe.EventTools;
import promhx.Deferred;


//The model is also doing most of the ui work.
typedef CCARStruct = {
		var scenarioName : String;
		var scenarioText : String;
		var creator : String;
		var deleted : Bool;
	}


class CCAR {
	
	private static var SCENARIO_NAME = "scenarioName";
	private static var SCENARIO_TEXT = "scenarioText";
	private static var SAVE_SCENARIO = "saveScenario";

	private var crudType : String ;
	private var ccarStruct : CCARStruct;
	// UI Accessors
	private function getScenarioName() : String {
		return getScenarioNameElement().value;
	}
	private function getScenarioNameElement() : InputElement {
		return (cast Browser.document.getElementById(SCENARIO_NAME));
	}

	private function getScenarioText() : String {
		return getScenarioTextElement().value;
	}
	private function getScenarioTextElement() : TextAreaElement {
		return (cast Browser.document.getElementById(SCENARIO_TEXT));
	}

	private function getSaveScenarioElement() : ButtonElement {
		return (cast Browser.document.getElementById(SAVE_SCENARIO));
	}

	public function new(name : String, text : String, creator : String) {
		ccarStruct.scenarioName = name;
		ccarStruct.scenarioText = text;
		ccarStruct.creator = creator;
		ccarStruct.deleted = false;	
		var stream : Stream<Dynamic> = 
			MBooks_im.getSingleton().initializeElementStream(
				cast getScenarioNameElement()
				, "keyup"
			);
		stream.then(checkScenarioExists);

	}


	private function copyIncomingValues(aMessage) {
		this.ccarStruct.scenarioName = aMessage.ccarData.scenarioName;
		this.ccarStruct.scenarioText = aMessage.ccarData.scenarioText;
		this.ccarStruct.creator = aMessage.ccarData.createdBy;
		this.ccarStruct.deleted = aMessage.ccarData.deleted;
	}
	private function copyValuesFromUI() {
		ccarStruct.scenarioName = getScenarioName();
		ccarStruct.scenarioText = getScenarioText();
		ccarStruct.creator = MBooks_im.getSingleton().getNickName();
		ccarStruct.deleted = false ; // Need to pick up from ui
	}

	private function checkScenarioExists(ev : KeyboardEvent) {
		if(Util.isSignificantWS(ev.keyCode)){
			var payload : Dynamic =  {
				nickName : MBooks_im.getSingleton().getNickName()
				, commandType : "CCARUpload"
				, ccarOperation : crudType
				, ccarData : ccarStruct
			};
			MBooks_im.getSingleton().doSendJSON(payload);
		}
	}


	public function processCCARUpload(incomingMessage : Dynamic){
		trace("Processing ccar upload");
		var ccarStruct : CCARStruct = incomingMessage.ccarData;
		var crudType = incomingMessage.crudType;
		copyIncomingValues(incomingMessage);
		if(crudType == "Create"){
			trace("Create successful");
			copyIncomingValues(incomingMessage);
		}else if(crudType == "Update"){
			trace("Update successful");
			copyIncomingValues(incomingMessage);
		}else if (crudType == "Query") {
			trace("Read returned " + incomingMessage);
			copyIncomingValues(incomingMessage);
			if(ccarStruct.scenarioText == "") {
				crudType = "Create";
			}else {
				crudType = "Update";
			}
		}
	}



}