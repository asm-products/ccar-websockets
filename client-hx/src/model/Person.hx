package model;
import haxe.Json;
import js.html.Event;
import js.Browser;
import js.html.Text;
import js.html.Element;
import js.html.DivElement;
import js.html.InputElement;
import js.html.DOMCoreException;
import js.html.Document;
import js.html.ButtonElement;


class Person {
	private static var FIRST_NAME : String = "FirstName";
	private static var FIRST_NAME_LABEL : String = "First Name";
	private static var LAST_NAME : String = "LastName";
	private static var LAST_NAME_LABEL : String = "Last Name";
	private static var PASSWORD : String = "Password";
	private static var PASSWORD_LABEL : String = "Password";
	private static var NICK_NAME : String = "Nickname";
	private static var NICK_NAME_LABEL : String = "Nick name (needs to be unique)";

	private var mbooks : MBooks;
	public var nickName (null, default) : String;
	public var password (null, default) : String;
	public var firstName (null, default): String;
	public var lastName (null, default): String;
	private var deleted : Bool;
	private var document : Document;

	private var register : ButtonElement;
	private var logout : ButtonElement;
	private var status : Element;
	public function new(fName : String
		, lName : String
		, nName : String
		, pwd : String) {
		firstName = fName;
		lastName = lName;
		nickName = nName;
		password = pwd;
		deleted = false; // Should be default..
		document = Browser.document;
	}

	private function copyValues(): Void {
		var element : InputElement = cast document.getElementById(PASSWORD);
		if(element != null) {
			password = element.value;
		}
		element = cast document.getElementById(FIRST_NAME);
		if(element != null){
			firstName = element.value;
		}
		element = cast document.getElementById(LAST_NAME);
		if(element != null) { 
			lastName = element.value;
		}
		element = cast document.getElementById(NICK_NAME);
		if(element != null){
			nickName = element.value;
		}

	}

	public function createNickNameForm(books : MBooks): Void {
		try {
			this.mbooks = books;
			trace("Creating login form");
			var document = Browser.document;
			var div : DivElement = createDivTag(document, "Person.Login");
			createNickName(document, div, NICK_NAME, NICK_NAME_LABEL);
			status = cast document.getElementById("status");
			status.innerHTML = "Welcome.";
			var nickNameInput : InputElement = getNickNameInput();
			nickNameInput.onchange = this.sendLogin;
		}catch(msg : DOMCoreException){
			trace ("Exception " + msg);
		}

	}
	public function createLoginForm() : Void {
		try {
			trace("Creating login form");
			var document = Browser.document;
			var div : DivElement = createDivTag(document, "Person.Login");
			div.appendChild(div);
			status = cast document.getElementById("status");
			status.innerHTML = "Welcome back. The last time you logged in";
			createNickName(document, div, NICK_NAME, NICK_NAME_LABEL);
			createPassword(document, div, PASSWORD, PASSWORD_LABEL);
		}catch(msg : DOMCoreException){
			trace ("Exception " + msg);
		}
	}
	private function createRegisterButton(document : Document
		, parent : DivElement) : Void {
			register = document.createButtonElement();
			register.value = "Register";
			register.innerHTML = "Register";
			parent.appendChild(register);
			register.onclick = registerUser;			
		}
	private function createLogoutButton(document : Document 
		, parent : DivElement) : Void {
			logout = document.createButtonElement();
			logout.value = "Logout";
			logout.innerHTML = "Logout";
			parent.appendChild(logout);
			logout.onclick = logoutUser;
		}

	private function logoutUser(ev : Event){
		trace("Logout user " + ev);
		this.mbooks.logout();
	}

	private function registerUser(ev: Event){
		trace("Register user " + ev);
		var commandType : String = "CreateUser";
		copyValues();
		var operation : UserOperation = new UserOperation("Create");

		var uo = {
			commandType : "CreateUser", 
			operation:  {
				tag : "Create" , //Tag is needed for the aeson objects.
				contents : []
			},
			person : {
				firstName : this.firstName,
				lastName : this.lastName,
				password : this.password,
				nickName : this.nickName,
				deleted : false
			}
		};
		this.mbooks.doSendJSON(haxe.Json.stringify(uo));
	}

	public function registerForm(books : MBooks) : Void{
		try {
			trace("Person :: Creating registration form" + books);
			var document = Browser.document;
			trace("Setting status ");
			status = cast document.getElementById("status");
			status.innerHTML = "Let me sign you up";
			var document = Browser.document;
			this.mbooks = books;
			trace("Creating the div tags");
			var div : DivElement = createDivTag(document, "Person.Registration");
			createFormElements(document, div);
			createRegisterButton(document, div);
			createLogoutButton(document, div);
			document.body.appendChild(div);
		}catch(msg : DOMCoreException){
			trace("Exception " + msg);
		}
	}
	private function createFormElements(document : Document
		, parent : DivElement) : Void{
		createFirstName(document, parent, FIRST_NAME, FIRST_NAME_LABEL);
        createLastName (document, parent, LAST_NAME, LAST_NAME_LABEL);
        createPassword (document, parent, PASSWORD, PASSWORD_LABEL);
		
	}

	private function createFirstName(document : Document
			, parent : DivElement, elementId : String, elementLabel : String) : Void{
			trace("Creating first name element ");
			var div = document.createDivElement();
			div.className = "Person.Login.FirstName";
			var textElement = document.createTextNode(elementLabel);
			var firstNameInput = document.createInputElement();
			firstNameInput.id = elementId;
			div.appendChild(textElement);
            div.appendChild(firstNameInput);
			parent.appendChild(div);

	}

    private function createLastName(document : Document
        , parent : DivElement, elementId : String, elementLabel : String) : Void {
            var div = document.createDivElement();
            div.className  = "Person.Login.LastName";
            var textElement = document.createTextNode(elementLabel);
            var lastNameInput : InputElement = document.createInputElement();
            lastNameInput.id = elementId;
            div.appendChild(textElement);
            div.appendChild(lastNameInput);
            parent.appendChild(div);
    }

    private function createNickName(document : Document
        , parent : DivElement, elementId:  String, elementLabel : String): Void {
        var div = document.createDivElement();
        div.className = "Person.Login.NickName";
        var textElement = document.createTextNode(elementLabel);
     	var nickNameInput = document.createInputElement();
     	nickNameInput.id = elementId;
     	div.appendChild(textElement);
     	div.appendChild(nickNameInput);
     	parent.appendChild(div);

    }
    private function createPassword(document : Document
        , parent : DivElement, elementId: String, elementLabel : String) : Void {
        var div = document.createDivElement();
        div.className = "Person.Login.Password";
        var textElement = document.createTextNode(elementLabel);
        var passwordInput : InputElement = document.createInputElement();
        passwordInput.id = elementId;
        div.appendChild(textElement);
        div.appendChild(passwordInput);
        parent.appendChild(div);
    }
	private function createDivTag(document : Document, className : String) : DivElement {
		var div = document.createDivElement();
		div.className = className;
		document.body.appendChild(div);
		return div;
	}
	public function sendLogin (ev: Event){
		trace("Copying values");
		copyValues();
		var lStatus : LoginStatus = Undefined;
		var cType : String = Std.string(CommandType.Login);
		var l : Login = new Login(cType, this, lStatus);
		trace("Sending login status " + l);
		mbooks.doSendJSON(Json.stringify(l));
	}
	private function getNickNameInput() : InputElement {
		return getInput(NICK_NAME);
	}
	private function getFirstName() : InputElement {
		return getInput(FIRST_NAME);
	}
	private function getLastName() : InputElement {
		return getInput(LAST_NAME);
	}
	private function getPassword() : InputElement {
		return getInput(PASSWORD);
	}
	private function getInput(id : String) : InputElement{
		var result : InputElement = cast document.getElementById(id);
		return result;
	}
}