import js.html.*;
import js.html.Event;
class Test {

	
	var serverHost : String = "localhost";
	var protocol : String = "ws";
	var portNumber : Int = 3000;
	var websocket : WebSocket;
	function new (){
		serverHost = "localhost";
		protocol = "ws";
		portNumber = 3000;
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
		var test : Test = new Test();
		trace(test.connectionString());
	}
	public function onClose(ev: Event){
		trace("Connection closed");

	}
	public  function onOpen(ev: Event){
		trace("Connection opened");
	}
	public  function onMessage(ev: Event){
		trace("Received " + ev);

	}
	public  function onError(ev : Event){
		trace("Error " + ev);
	}
	public  function doSend(aMessage : String){
		websocket.send(aMessage);
	}

}