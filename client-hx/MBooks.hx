

import jspi.html.*;
import js.html.Event;
import js.html.MessageEvent;
import js.html.WebSocket;
import js.html.DOMCoreException;
import model.Contact;
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

	}
	public  function onError(ev : Event){
		trace("Error " + ev);
	}
	public  function doSend(aMessage : String){
		trace("Sending " + aMessage);
		websocket.send(aMessage);
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

		trace("Connection form created");
		} catch(msg:DOMCoreException){
			trace("Exception " + msg);
		} 
	}

	private function sendLogin (ev: Event){
		initializeConnection();
		doSend(loginInput.value);
	}




}