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
	public function new() {
		var stream : Stream<Dynamic> = 
			MBooks_im.getSingleton().initializeElementStream(
				cast getCompanySignup()
				,"click");
		stream.then(saveButtonPressed);
	}
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

	private function getCompanyMailboxElement() {
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


	private function loadImage(ev: Event){
		trace("Load image");
		try {
			var reader : FileReader = cast ev.target;
			var imageSplash : ImageElement = 
				getCompanySplashElement();
			imageSplash.src = reader.result;
			saveCompanyInfo();
		}catch(e : Dynamic) {
			trace("Exception " + e);
		}
	}

	private function saveCompanyInfo() {
		trace("Saving company info");
		var companyName : String = getCompanyName();
		var companyID : String = getCompanyID();
		var companyMailbox : String = getCompanyMailbox();
		var imageSplash : ImageElement = getCompanySplashElement();
		var imageEncoded : String = imageSplash.src;

		var payload : Dynamic = {
			nickName : MBooks_im.getSingleton().getNickName()
			, commandType : "ManageCompany"
			, crudType : "Create"
			, company : {
				companyName : companyName
				, companyID : companyID
				, generalMailbox : companyMailbox
				, companyImage : ""
				, updatedBy : MBooks_im.getSingleton().getNickName()
				} 
		};
		MBooks_im.getSingleton().doSendJSON(payload);

	}
	


}
