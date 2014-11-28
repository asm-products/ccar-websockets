package model;

import haxe.Json;
import js.html.Event;
import js.Browser;
import js.html.Text;
import js.html.DivElement;
import js.html.InputElement;
import js.html.DOMCoreException;
import js.html.Document;
import js.html.ButtonElement;


class Login {
	public var nickName (null, default) : String;
	public var password (null, default) : String;
	public var firstName (null, default): String;
	public var lastName (null, default): String;

	private var nickNameInput : InputElement;
	private var passwordInput : InputElement;
	private var firstNameInput : InputElement;
	private var lastNameInput : InputElement;
	private var register : ButtonElement;
	private var logout : ButtonElement;

	public function new(fName : String
		, lName : String
		, nName : String
		, pwd : String) {
		firstName = fName;
		lastName = lName;
		nickName = nName;
		password = pwd;
	}


	public function createRegistrationForm() : Void {
		try {
			trace("Creating registration formm");
			var document = Browser.document;
			var div : DivElement = createDivTag(document);
			createFormElements(document, div);
			document.body.appendChild(div);
		}catch(msg : DOMCoreException){
			trace("Exception e");
		}
	}
	private function createFormElements(document : Document
		, parent : DivElement) : Void{
		createFirstName(document, parent);
        createLastName (document, parent);
        createNickName (document, parent);
        createPassword (document, parent);
		
	}
	private function createFirstName(document : Document
			, parent : DivElement) : Void{

			var div = document.createDivElement();
			div.className = "Contact.Login.FirstName";
			var textElement = document.createTextNode("First Name");
			firstNameInput = document.createInputElement();
			div.appendChild(textElement);
            div.appendChild(firstNameInput);
			parent.appendChild(div);

	}

    private function createLastName(document : Document
        , parent : DivElement) : Void {
            var div = document.createDivElement();
            div.className  = "Contact.Login.LastName";
            var textElement = document.createTextNode("Last Name");
            lastNameInput = document.createInputElement();
            div.appendChild(textElement);
            div.appendChild(lastNameInput);
    }

    private function createNickName(document : Document
        , parent : DivElement): Void {

    }
    private function createPassword(document : Document
        , parent : DivElement) : Void {

    }
	private function createDivTag(document : Document) : DivElement {
		var div = document.createDivElement();
		div.className = "Contact.Login";
		document.body.appendChild(div);
		return div;
	}

}