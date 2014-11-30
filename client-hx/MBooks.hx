
import haxe.Json;
import js.html.Event;
import js.html.MessageEvent;
import js.html.WebSocket;
import js.html.DOMCoreException;
import model.Contact;
import model.Login;
import model.Person;
import model.LoginStatus;

import js.Browser;
import js.html.ButtonElement;
class MBooks {
	
	var serverHost : String = "localhost";
	var protocol : String = "ws";
	var portNumber : Int = 3000;
	var websocket : WebSocket;
	var contact : Contact;	
	var connect : ButtonElement;
	function new (){
		serverHost = "localhost";
		protocol = "ws";
		portNumber = 3000;
		contact = new Contact("test", "test","test");
		createConnectionForm();
	}

	function initializeConnection(){
		websocket = new WebSocket(connectionString());
		websocket.onclose = onClose;
		websocket.onerror = onError;
		websocket.onmessage = onMessage;
		websocket.onopen = onOpen;

	}

	function connectionString() : String {
		return protocol + "://" + serverHost + ":" + portNumber;
	}
	static function main() {
		var test : MBooks= new MBooks();
	}
	public function onClose(ev: Event){
		trace("Connection closed");

	}
	public  function onOpen(ev: Event){
		trace("Connection opened");
	}

	public  function onMessage(ev: MessageEvent){
		trace("Received " + ev.data);
		var incomingMessage  : Login = haxe.Json.parse(ev.data);
		trace("Processing incoming message " + incomingMessage);
		processLoginResponse(incomingMessage);
	}
	private function processLoginResponse(lR : Login){
		trace("Processing login response " + lR.loginStatus);
		trace("Processing person object " + lR.person);
		var lStatus : LoginStatus = lR.loginStatus;
		trace("Processing lStatus " + lStatus);
		if(lStatus == UserNotFound){
			trace("User not found. Need to see why enum is not working");
			createRegistrationForm(lR);
		}
		if(lStatus == UserExists){
			createLoginForm(lR);
		}
		if(lStatus == InvalidPassword){
			createInvalidPassword(lR);
		}
		if(lStatus == Undefined){
			createUndefined();
		}
	}

	private function createUndefined() : Void {
		trace("Undefined as response..should not happen");
	}
	private function createRegistrationForm(lr : Login) : Void{
		trace("Creating registration form ");
		if(lr.person == null){
			var person = new Person("", "", "","");
			person.createRegistrationForm();
		}else {
			lr.person.createRegistrationForm();
		}
	}
	private function createLoginForm(lr : Login) : Void{
		trace("Creating login form");
		lr.person.createLoginForm();
	}
	private function createInvalidPassword(lr : Login) : Void{
		trace("Processing invalid login" + lr);
	}

	public  function onError(ev : Event){
		trace("Error " + haxe.Json.stringify(ev));
	}

	public  function doSend(aMessage : String){
		trace("Sending " + Json.stringify(aMessage));
		websocket.send(Json.stringify(aMessage));
	}
	private var loginInput : js.html.InputElement;
	private function createConnectionForm() : Void {
		try {
		trace("Creating connection form");
		var document = Browser.document;
		var div : js.html.DivElement = document.createDivElement();
		var login : js.html.Text = document.createTextNode("Login");
		loginInput = document.createInputElement();
		loginInput.onchange = sendLogin;
		div.appendChild(login);
		div.appendChild(loginInput);
		document.body.appendChild(div);
		initializeConnection();
		trace("Connection form created");
		} catch(msg:DOMCoreException){
			trace("Exception " + msg);
		} 
	}

	private function sendLogin (ev: Event){
		var p : Person = new Person ("", "", loginInput.value, "");

		var lStatus : LoginStatus = Undefined;
		var l : Login = new Login(p, lStatus);
		doSend(Json.stringify(l));
	}




}