import util.Util;
import haxe.Json;
import haxe.Utf8;
import haxe.Timer;
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
import promhx.PublicStream;
import massive.munit.TestRunner;
using promhx.haxe.EventTools;
import promhx.Deferred;


class Company {

	private static var SAVE_COMPANY = "saveCompany"; // disconnect?
	private static var DELETE_COMPANY = "deleteCompany";
	private static var COMPANY_IMAGE = "companyImage";
	private static var COMPANY_SPLASH_ELEMENT = "companySplash";
	private static var COMPANY_NAME = "companyName";
	private static var COMPANY_ID = "companyID";
	private static var COMPANY_MAILBOX = "generalMailbox";
	private static var COMPANY_FORM_ID = "companyForm";

	private var newCompany : Bool;
	private var selectListEventStream : Deferred<Dynamic>;
	

	private function getCompanySignup () : ButtonElement {
		var buttonElement : ButtonElement = cast Browser.document.getElementById(SAVE_COMPANY);
		return buttonElement;
	}
	private function getCompanyImageElement() : InputElement {
		var fileElement : InputElement = cast Browser.document.getElementById(COMPANY_IMAGE);
		return fileElement;
	}
	private function getCompanySplashElement() : ImageElement {
		return (cast Browser.document.getElementById(COMPANY_SPLASH_ELEMENT));
	}
	private function getCompanyNameElement() : InputElement {
		return (cast Browser.document.getElementById(COMPANY_NAME));
	}
	private function getCompanyName() {
		return (getCompanyNameElement().value);
	}
	private function getCompanyIDElement() : InputElement {
		return (cast Browser.document.getElementById(COMPANY_ID));
	}
	private function getCompanyID() {
		return (getCompanyIDElement().value);
	}

	private function getCompanyMailboxElement() : InputElement {
		return (cast Browser.document.getElementById(COMPANY_MAILBOX));
	}
	private function getCompanyMailbox() {
		return (getCompanyMailboxElement().value);
	}

	//Enable/disable
	private function hideCompanyForm(){
		Util.hideDivField(COMPANY_FORM_ID);
	}

	private function showCompanyForm() {
		Util.showDivField(COMPANY_FORM_ID);
	}

	//Save images in base64 encoded strings
	private function saveButtonPressed(ev : Event) {
		trace("Save button pressed");
		var file = getCompanyImageElement().files[0];
		var reader = new FileReader();
		var stream_1 : Stream<Dynamic> = 
			MBooks_im.getSingleton().initializeElementStream(
				cast reader
				, "load");
		stream_1.then(loadImage);
		reader.readAsDataURL(file);		
	}

	private function selectAllCompanies(loggedInMessage) {
		trace("Processing select all companies " + loggedInMessage);
		var payload = {
			nickName : MBooks_im.getSingleton().getNickName()
			, commandType : "SelectAllCompanies"
		};
		MBooks_im.getSingleton().doSendJSON(payload);
	}

	private function getPayload(nickName, crudType
					, companyName
					, companyID
					, companyMailbox 
					, companyImage1
					, updatedBy) : Dynamic {
		var payload : Dynamic = {
			nickName : nickName
			, commandType : "ManageCompany"
			, crudType : crudType
			, company : {
				companyName : companyName
				, companyID : companyID
				, generalMailbox : companyMailbox
				, companyImage : companyImage1
				, updatedBy : nickName
				} 
		};
		return payload;

	}

	private function loadImage(ev: Event){
		trace("Load image");
		try {
			var reader : FileReader = cast ev.target;
			var imageSplash : ImageElement = 
				getCompanySplashElement();
			saveCompanyInfo(reader.result);
		}catch(e : Dynamic) {
			trace("Exception " + e);
		}
	}

	private function saveCompanyInfo(encodedString) {
		trace("Saving company info");
		var companyName : String = getCompanyName();
		var companyID : String = getCompanyID();
		var companyMailbox : String = getCompanyMailbox();
		var imageSplash : ImageElement = getCompanySplashElement();
		var imageEncoded : String = encodedString;
		var nickName  = MBooks_im.getSingleton().getNickName();
		var crud = "";
		if(newCompany) {
			crud = "Create";
		}else {
			crud = "C_Update";
		}
		var payload = getPayload(nickName
				, crud
				, companyName
				, companyID
				, companyMailbox
				, imageEncoded
				, nickName);
		MBooks_im.getSingleton().doSendJSON(payload);

	}
	
	private function chkCompanyExists(ev : KeyboardEvent) {
		trace("Chk company exists " + ev.keyCode);
		if(Util.isSignificantWS(ev.keyCode)){
			try {
			var nickName = MBooks_im.getSingleton().getNickName();
			var payload = getPayload(nickName, "Read"
						, ""
						, getCompanyID()
						, ""
						, ""
						, "");

			MBooks_im.getSingleton().doSendJSON(payload);

			}catch(err : Dynamic) {
				trace("Error checking company " + err);
			}
		}
	}

	public function processManageCompany(incomingMessage) {
		trace("Process manage company ");
		var crudType = incomingMessage.crudType;
		trace(incomingMessage);
		if(crudType == "Create") {
			trace("Create successful");
			copyIncomingValues(incomingMessage);
		}else if (crudType == "Read") {
			if(incomingMessage.company.companyID == "") {
				newCompany = true;
				getCompanyIDElement().value = getCompanyID();
			}else {
				copyIncomingValues(incomingMessage);
				newCompany = false;			
			}
		}else if (crudType == "C_Update") {
			copyIncomingValues(incomingMessage);
		}else if (crudType == "Delete"){
			clearFields(incomingMessage);
		}else {
			throw ("Invalid crudtype " + crudType);
		}

	}
	private function copyIncomingValues(incomingMessage){
		try {
		getCompanyNameElement().value = incomingMessage.company.companyName;
		getCompanyIDElement().value = incomingMessage.company.companyID;
		getCompanyMailboxElement().value = incomingMessage.company.generalMailbox;
		var imageSplash : ImageElement = 
				getCompanySplashElement();
		imageSplash.src = incomingMessage.company.companyImage;		
		}catch(error: Dynamic) {
			throw error;
		}

	}

	private function clearFields(incomingMessage) {
		getCompanyNameElement().value = "";
		getCompanyIDElement().value = "";
		getCompanyMailboxElement().value = "";
		getCompanySplashElement().src = "";
	}

	//Streams
	//Called when the client send a request to load all 
	//companies.
	public function getSelectListEventStream() {
		return selectListEventStream;
	}

	//Initialization
	public function new() {
		trace("Instantiating company");
		newCompany = true;
		var stream : Stream<Dynamic> = 
			MBooks_im.getSingleton().initializeElementStream(
				cast getCompanySignup()
				,"click");
		var cidStream : Stream<Dynamic> = 
			MBooks_im.getSingleton().initializeElementStream(cast getCompanyIDElement(), "keyup");			
		cidStream.then(chkCompanyExists);
		stream.then(saveButtonPressed);
		selectListEventStream = new Deferred<Dynamic>();
		MBooks_im.getSingleton().getUserLoggedInStream().then(selectAllCompanies);

	}



}
