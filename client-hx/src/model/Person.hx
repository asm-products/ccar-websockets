package model;
import haxe.Json;
import js.html.Event;
import js.html.KeyboardEvent;
import js.Browser;
import js.html.Text;
import js.html.Element;
import js.html.DivElement;
import js.html.InputElement;
import js.html.DOMCoreException;
import js.html.Document;
import js.html.ButtonElement;
import js.Lib.*;
import haxe.ds.GenericStack;
import util.*;
import promhx.Stream;
import promhx.Promise;


class Person {
	private static var FIRST_NAME : String = "FirstName";
	private static var FIRST_NAME_LABEL : String = "First Name";
	private static var LAST_NAME : String = "LastName";
	private static var LAST_NAME_LABEL : String = "Last Name";
	private static var PASSWORD : String = "PasswordField";
	private static var PASSWORD_LABEL : String = "Password";
	private static var NICK_NAME : String = "Nickname";
	private static var NICK_NAME_LABEL : String = "Nick name (needs to be unique)";

	private var maxAttempts : Int;
	private var attempts : Int;
	public var nickName (default, null) : String;
	public var password (default, null) : String;
	public var firstName (default, null): String;
	public var lastName (default, null): String;
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
		attempts = 0;
		maxAttempts = 3;
	}


	public function hideDiv(element: DivElement) : Void {
		element.style.display = "Hide";
	}
	public function createLoginForm() : Person {
		try {
			trace("Creating login form " + this);
			popStack();
			var document = Browser.document;
			var divName : String = "Person.Login";
			var div : DivElement = Util.createDivTag(document, "Person.Login");
			status = cast document.getElementById("status");
			status.innerHTML = "Welcome back. The last time you logged in: Fix this";
			Util.createElementWithLabel(document, div, divName + NICK_NAME, NICK_NAME_LABEL);
			Util.createElementWithLabel(document, div, divName + PASSWORD, PASSWORD_LABEL);
			getInput(divName + PASSWORD).focus();
			getInput(divName + PASSWORD).onblur = validatePassword;
			setValues();
			pushStack(div);
			return this;
		}catch(msg : DOMCoreException){
			trace ("Exception " + msg);
		}
		return null;
	}
	
	private function setValues() : Void {
		var element : InputElement = cast document.getElementById(FIRST_NAME);
		if(element != null){
			element.value = this.firstName;
		}
		element = cast document.getElementById(LAST_NAME);
		if(element != null){
			element.value = this.lastName;
		}
		element = cast document.getElementById(NICK_NAME);
		if(element != null){
			element.value = this.nickName;
		}

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
		element = cast document.getElementById(PASSWORD);
		if(element != null){
			password = element.value;
		}

	}

	public function createNickNameForm(): Void {
		try {
			popStack();
			var document = Browser.document;
			trace("Creating nickname form");
			var divName : String = "Person.Nickname";
			var div : DivElement = Util.createDivTag(document, divName);
			Util.createElementWithLabel(document, div, divName + NICK_NAME, divName + NICK_NAME_LABEL);
			status = cast document.getElementById("status");
			status.innerHTML = "Welcome.";
			var nickNameInput : InputElement = getInput(divName + NICK_NAME);
			nickNameInput.focus();
			nickNameInput.select();
			var stream : Stream<Dynamic> = MBooks.getMBooks().initializeElementStream(nickNameInput, "keyup");
			stream.then(sendLogin);
			pushStack(div);
		}catch(msg : DOMCoreException){
			trace ("Exception " + msg);
		}
	}

	private function removeAllElements(document) : Void {
		deleteElement(document, NICK_NAME, NICK_NAME_LABEL);
		deleteElement(document, PASSWORD, PASSWORD_LABEL);
		deleteElement(document, FIRST_NAME, FIRST_NAME_LABEL);
		deleteElement(document, LAST_NAME, LAST_NAME_LABEL);
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
		MBooks.getMBooks().logout();
	}

	private function registerUser(ev: Event){
		trace("Register user " + ev);
		var commandType : String = "ManageUser";
		copyValues();
		var operation : UserOperation = new UserOperation("Create");

		var uo = {
			commandType : "ManageUser", 
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
		MBooks.getMBooks().doSendJSON(haxe.Json.stringify(uo));
	}

	public function registerForm() : Void{
		try {
			
			var document = Browser.document;
			popStack();
			removeAllElements(document);
			trace("Setting status ");
			status = cast document.getElementById("status");
			status.innerHTML = "Let me sign you up";
			var document = Browser.document;
			trace("Creating the div tags");
			var div : DivElement = Util.createDivTag(document, "Person.Registration");
			createFormElements(document, div);
			createRegisterButton(document, div);
			createLogoutButton(document, div);
			document.body.appendChild(div);
			setValues();
			pushStack(div);
		}catch(msg : DOMCoreException){
			trace("Exception " + msg);
		}
	}
	private function createFormElements(document : Document
		, parent : DivElement) : Void{
		Util.createElementWithLabel(document, parent, parent.id + NICK_NAME, NICK_NAME_LABEL);
		Util.createElementWithLabel(document, parent, parent.id + FIRST_NAME, FIRST_NAME_LABEL);
        Util.createElementWithLabel(document, parent, parent.id + LAST_NAME, LAST_NAME_LABEL);
        Util.createElementWithLabel(document, parent, parent.id + PASSWORD, PASSWORD_LABEL);
			
	}



	private function deleteElement(document : Document, elementId : String, 
		elementLabelId : String){
		trace("Deleting element if exists -> " + elementId);	
		var element : Element = document.getElementById(elementId);
		if (element != null){
			element.parentNode.removeChild(element);
		}
		var elementLabel : Element = document.getElementById(elementLabelId);
		if (elementLabel != null){
			elementLabel.parentNode.removeChild(elementLabel);
		}
	}

	private function validatePassword(ev : Event){
		var divName : String = "Person.Login";
		var passwordTest : String = getInput(divName + PASSWORD).value;
		trace("Password ?? " + passwordTest);
		if(passwordTest != this.password){
			js.Lib.alert("Invalid password. Try again");
			attempts++;
			if(attempts > maxAttempts){
				js.Lib.alert("Too many attempts. Logging you out.");
				MBooks.getMBooks().logout();
			}
		}else {
			var document : Document = Browser.document;
			var div : DivElement = Util.createDivTag(document, "Dashboard.Logout");
			createLogoutButton(document, div);
			MBooks.getMBooks().showDashboard(this);
			
		}
	}

	public function sendLogin (ev: KeyboardEvent){
		trace("Send login " + ev.keyCode);
		if(ev.keyCode == 10 || ev.keyCode == 13 || ev.keyCode == 9){
			var inputElement : InputElement = cast ev.target;
			this.nickName = inputElement.value;
			var lStatus : LoginStatus = Undefined;
			var cType : String = Std.string(CommandType.Login);

			var l : Login = new Login(cType, this, lStatus);
			trace("Sending login status " + l);
			MBooks.getMBooks().doSendJSON(Json.stringify(l));
		}
	}
	private function getInput(id : String) : InputElement{
		var result : InputElement = cast document.getElementById(id);
		return result;
	}
	//Pop before manipulating the current window, hide the window
	private function popStack() : Void {
		var prev : DivElement = MBooks.pop();
		if(prev != null) {
			trace("Popping " + prev + " -> " +  prev.id);
		}
		if(prev != null) {
			trace("Hiding div " + prev);
			prev.hidden = true;			
		}		
	}
	private function pushStack(div : DivElement) : Void {
		if(div == null){
			return;
		}
		trace("Pushing " + div + "->" + div.id);
		div.hidden = false;
		MBooks.push(div);
		trace("Div element pushed" + div);
	}


	public function copyInput(fName: String, lName : String, nName : String, pwd : String) : Void{
		this.firstName = fName;
		this.lastName = lName;
		this.nickName = nName;
		this.password = pwd;
	}

}