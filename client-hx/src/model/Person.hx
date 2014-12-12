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
	private var mbooks : MBooks;
	public var nickName (null, default) : String;
	public var password (null, default) : String;
	public var firstName (null, default): String;
	public var lastName (null, default): String;
	private var deleted : Bool;

	private var nickNameInput : InputElement;
	private var passwordInput : InputElement;
	private var firstNameInput : InputElement;
	private var lastNameInput : InputElement;
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
	}

	public function createNickNameForm(books : MBooks): Void {
		try {
			this.mbooks = books;
			trace("Creating login form");
			var document = Browser.document;
			var div : DivElement = createDivTag(document, "Person.Login");
			createNickName(document, div);
			status = cast document.getElementById("status");
			status.innerHTML = "Welcome.";
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
			createNickName(document, div);
			createPassword(document, div);
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
		var operation : UserOperation = new UserOperation("Create");
		var uo : CommandUO = new CommandUO(operation, this);
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
		createFirstName(document, parent);
        createLastName (document, parent);
        createPassword (document, parent);
		
	}

	private function createFirstName(document : Document
			, parent : DivElement) : Void{

			var div = document.createDivElement();
			div.className = "Person.Login.FirstName";
			var textElement = document.createTextNode("First Name");
			firstNameInput = document.createInputElement();
			div.appendChild(textElement);
            div.appendChild(firstNameInput);
			parent.appendChild(div);

	}

    private function createLastName(document : Document
        , parent : DivElement) : Void {
            var div = document.createDivElement();
            div.className  = "Person.Login.LastName";
            var textElement = document.createTextNode("Last Name");
            lastNameInput = document.createInputElement();
            div.appendChild(textElement);
            div.appendChild(lastNameInput);
            parent.appendChild(div);
    }

    private function createNickName(document : Document
        , parent : DivElement): Void {
        var div = document.createDivElement();
        div.className = "Person.Login.NickName";
        var textElement = document.createTextNode("Nick name (needs to be unique)");
     	nickNameInput = document.createInputElement();
     	div.appendChild(textElement);
     	div.appendChild(nickNameInput);
     	parent.appendChild(div);

    }
    private function createPassword(document : Document
        , parent : DivElement) : Void {
        var div = document.createDivElement();
        div.className = "Person.Login.Password";
        var textElement = document.createTextNode ("Password (hidden)");
        passwordInput = document.createInputElement();
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
		var p : Person = new Person ("", "", nickNameInput.value, "");
		var lStatus : LoginStatus = Undefined;
		var cType : String = Std.string(CommandType.Login);
		var l : Login = new Login(cType, p, lStatus);
		mbooks.doSendJSON(Json.stringify(l));
	}

}