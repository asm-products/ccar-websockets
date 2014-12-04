
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
		
		trace("Processing person object " + lR.person);
		trace("Processing lR status " + lR.loginStatus);
		if(lR.loginStatus == null){
			trace("Undefined state");
			return;
		}
		var lStatus : LoginStatus = Type.createEnum(LoginStatus, lR.loginStatus);
		trace("Processing lStatus " + lStatus);
		if(lStatus == UserNotFound){
			trace("User not found. Need to see why enum is not working");
			createRegistrationForm(this, lR);
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
	private function createRegistrationForm(books : MBooks, lr : Login) : Void{
		trace("Creating registration form ");
		if(lr.person == null){
			var person = new Person("", "", "","");
			person.createRegistrationForm(books);
		}else {
			lr.person.createRegistrationForm(books);
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

	/**
	* This will send the string as is. 
	* Clients could be sending json 
	*/
	public  function doSendJSON(aMessage : String){
		trace("Sending " + aMessage);
		websocket.send(aMessage);
	}
	private function createConnectionForm() : Void {
		try {
		trace("Creating connection form");
		var document = Browser.document;
		var person : Person = new Person("","","","");
		person.createNickNameForm(this);
		initializeConnection();
		trace("Connection form created");
		} catch(msg:DOMCoreException){
			trace("Exception " + msg);
		} 
	}


	public function logout() : Void{
		trace("Logging out ");
		if(websocket != null){
			websocket.close();
		}else {
			trace("No valid connection found");
		}
	}


}