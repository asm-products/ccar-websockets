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


class CCAR {
	
	private static var SCENARIO_NAME = "scenarioName";
	private static var SCENARIO_TEXT = "scenarioText";
	private static var SAVE_SCENARIO = "saveScenario";
	private static var PARSED_SCENARIO = "parsedScenario";
	private var scenarioName : String;
	private var scenarioText : String;
	private var creator : String;
	private var deleted : Bool;

	private var crudType : String ;
	// UI Accessors
	private function getScenarioName() : String {
		if(getScenarioNameElement() != null) {
			return getScenarioNameElement().value;
		}else {
			trace("Element not defined ");
			return "TBD";
		}
		
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

	public function new(name : String, text : String, cr : String) {
		try {
		trace("Creating ccar instance");
		scenarioName = name;
		scenarioText = text;
		creator = cr;
		deleted = false;	
		}catch(err : Dynamic) {
			trace("Exception creating ccar " + err);
		}
		trace("Created ccar instance successfully");

	}

	public function setupStreams() {
		var saveStream : Stream<Dynamic> = 
			MBooks_im.getSingleton().initializeElementStream(
				cast getSaveScenarioElement()
				, "click"
				);
		saveStream.then(sendPT);

	}
	private function copyIncomingValues(aMessage) {
		this.scenarioName = aMessage.ccarData.scenarioName;
		this.scenarioText = aMessage.ccarData.scenarioText;
		this.creator = aMessage.ccarData.createdBy;
		this.deleted = aMessage.ccarData.deleted;
	}
	private function copyValuesFromUI() {
		scenarioName = getScenarioName();
		scenarioText = getScenarioText();
		creator = MBooks_im.getSingleton().getNickName();
		deleted = false ; // Need to pick up from ui
	}

	private function saveScenario(ev : Event) {
		trace ("Saving scenario " );
		copyValuesFromUI();
			var payload : Dynamic =  {
				nickName : MBooks_im.getSingleton().getNickName()
				, uploadedBy : MBooks_im.getSingleton().getNickName()
				, commandType : "CCARUpload"
				, ccarOperation : { tag: crudType, contents : []}
				, ccarData : {
					scenarioName : scenarioName
					, scenarioText : scenarioText
					, creator : MBooks_im.getSingleton().getNickName()
					, deleted : false
				}
			};
			MBooks_im.getSingleton().doSendJSON(payload);

	}
	private function checkScenarioExists(ev : KeyboardEvent) {
		trace("Checking if scenario exists ");
		if(Util.isSignificantWS(ev.keyCode)){
			copyValuesFromUI();
			var payload : Dynamic =  {
				nickName : MBooks_im.getSingleton().getNickName()
				, uploadedBy : MBooks_im.getSingleton().getNickName()
				, commandType : "CCARUpload"
				, ccarOperation : { tag: "Query", contents : []}
				, ccarData : {
					scenarioName : scenarioName
					, scenarioText : scenarioText
					, creator : MBooks_im.getSingleton().getNickName()
					, deleted : false
				}
			};
			MBooks_im.getSingleton().doSendJSON(payload);
		}
	}

	private function sendPT(ev : Event){
		trace("Processing event " + ev);
		sendParsingRequest();
	}
	private function sendParsingRequest() {
		var payload : Dynamic = {
			nickName : MBooks_im.getSingleton().getNickName()
			, uploadedBy : MBooks_im.getSingleton().getNickName()
			, scenarioName : getScenarioName()
			, ccarText : getScenarioText()
			, commandType : "ParsedCCARText"
		};
		trace("Sending parsing request " + payload);
		MBooks_im.getSingleton().doSendJSON(payload);
	}

	public function processCCARUpload(incomingMessage : Dynamic){
		trace("Processing ccar upload");
		var ccarStruct : Dynamic = incomingMessage.ccarData;
		var crudType = incomingMessage.ccarOperation.tag;
		copyIncomingValues(incomingMessage);
		if(crudType == "Create"){
			trace("Create successful");
			copyIncomingValues(incomingMessage);
			sendParsingRequest();
		}else if(crudType == "Update"){
			trace("Update successful");
			copyIncomingValues(incomingMessage);
			sendParsingRequest();
		}else if (crudType == "Query") {
			trace("Read returned " + incomingMessage);
			copyIncomingValues(incomingMessage);
			if(ccarStruct.ccarResultSet == []) {
				crudType = "Create";
			}else {
				crudType = "Update";
			}
			sendParsingRequest();
		}
	}
	public function processParsedCCARText(incomingMessage : Dynamic){
		trace("Processing parsed text");
		setParsedScenario(haxe.Json.stringify(incomingMessage));
	}
	private function setParsedScenario(incomingM) {
		getParsedScenarioElement().value = incomingM;
	}
	private function getParsedScenarioElement(): InputElement {
		return (cast Browser.document.getElementById(PARSED_SCENARIO));
	}



}