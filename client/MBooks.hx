

import jspi.html.*;
import js.html.Event;
import js.html.MessageEvent;
import js.html.WebSocket;
import model.Contact;
import js.Browser;

class MBooks {

	
	var serverHost : String = "localhost";
	var protocol : String = "ws";
	var portNumber : Int = 3000;
	var websocket : WebSocket;
	var contact : Contact;	
	var connect : js.html.Button;
	function new (){
		serverHost = "localhost";
		protocol = "ws";
		portNumber = 3000;
		contact = new Contact("test", "test","test");


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
		websocket.send(aMessage);
	}

	private function createConnectionForm() : Void {
		var document = Browser.document;
		var protocol : js.html.Text;
		var server : js.html.Text;
		var port : js.html.Text;
		var pText : js.html.Text = document.createTextNode("Protocol");
		var sText : js.html.InputElement = document.createInputElement();
		sText.defaultValue = "ws";
		var serverText : js.html.Text = document.createTextNode("Server");
		connect = document.createButtonElement();
	}

}