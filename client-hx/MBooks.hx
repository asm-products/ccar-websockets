
import haxe.Json;
import haxe.Utf8;
import haxe.Timer;
import js.html.Event;
import js.html.MessageEvent;
import js.html.WebSocket;
import js.html.DOMCoreException;
import js.html.DivElement;
import model.Contact;
import model.Login;
import model.Person;
import model.LoginStatus;
import model.Command;
import model.CommandType;
import util.Util;
import js.Browser;
import js.html.ButtonElement;
class MBooks {

	//The client login
	var serverHost : String = "localhost";
	var protocol : String = "ws";
	var portNumber : Int = 3000;
	var keepAliveInterval : Int = 15000;
	var websocket : WebSocket;
	var contact : Contact;	
	var connect : ButtonElement;
	var timer : Timer;
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
		disableKeepAlive();

	}
	public  function onOpen(ev: Event){
		trace("Connection opened");
		initializeKeepAlive();
	}
	private function initializeKeepAlive() : Void {
		if(timer == null){
			timer = new haxe.Timer(keepAliveInterval);
			timer.run = keepAliveFunction;
		}else {
			trace("Timer already running. This should not happen");
		}
	}
	private function disableKeepAlive() : Void {
		if(timer == null){
			trace("Nothing to disable");
		}else {
			trace("Stopping the timer");
			timer.stop();
		}
	}
	private function parseCommandType(commandType : String) : CommandType {
	 trace("Parsing command type " + commandType);
		try {
		return Type.createEnum(CommandType, commandType);
		}catch(e : Dynamic){
			trace("Error " + e);
			return Undefined;
		}
	}
	public function parseIncomingMessage(incomingMessage : Dynamic) : Void {
		var commandType : CommandType = 
			parseCommandType(incomingMessage.commandType);
		switch(commandType){
			case Login : {
			    var person  : Person = incomingMessage.login;
				var login : Login = model.Login.createLoginResponse(incomingMessage, person);
				processLoginResponse(login);
			}

			case RegisterUser: {
				//processRegisterUser(incomingMessage);
			}
			case QueryUser : {
				//processQueryUser(incomingMessage);
			}
			case DeleteUser: {
				//processDeleteUser(incomingMessage);
			}
			case UpdateUser : {
				//processUpdateUser(incomingMessage);
			}
			case CreateUserTerms :{
				//processCreateUserTerms(incomingMessage);
			}
			case UpdateUserTerms : {
				//processUpdateUserTerms(incomingMessage);
			}
			case DeleteUserTerms : {
				//processDeleteUserTerms(incomingMessage);
			}
			case QueryUserTerms : {
				//processQueryUserTerms(incomingMessage);
			}
			case CreateUserPreferences : {
				//processCreateUserPreferences(incomingMessage);
			}
			case UpdateUserPreferences : {
				//processUpdateUserPreferences(incomingMessage);
			}
			case QueryUserPreferences : {
				//processQueryUserPreferences(incomingMessage);
			}
			case DeleteUserPreferences : {
				//processDeleteUserPreferences(incomingMessage);
			}
			case Undefined : {
				//processUndefinedCommandType();
			}

		}

	}
	public  function onMessage(ev: MessageEvent) : Void{
		trace("Received " + ev.data);
		var incomingMessage = haxe.Json.parse(ev.data);
		//XXX: Needed to parse the incoming message twice because
		// of \",s in the response. This is a bug.
		incomingMessage = haxe.Json.parse(incomingMessage);
		trace("Printing incoming message " + incomingMessage);
		parseIncomingMessage(incomingMessage);
	}

	private function processLoginResponse(lR : Login){		
		trace("Processing login object " + lR);
		trace("Processing lR status " + lR.loginStatus);
		if(lR.loginStatus == null){
			trace("Undefined state");
			return;
		}
		var lStatus : LoginStatus = Type.createEnum(LoginStatus, lR.loginStatus);
		trace("Processing lStatus " + lStatus);
		if(lStatus == UserNotFound){
			trace("User not found. Need to see why enum is not working " + lR);
			createRegistrationForm(this, lR);
		}
		if(lStatus == UserExists){
			trace("User exists " + lR);
			createLoginForm( this, lR);
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
		if(lr.login == null){
			var person : Person = new Person("", 
						"",
						"",
						"");
			
			person.registerForm(books);
		}else {
			trace("Login not null : Login  " + lr.login);
			var p : Person = lr.login;		
			//Copy the 
			var pCopy : Person = new Person(p.firstName, p.lastName, p.nickName, p.password);
			pCopy.registerForm(books);
		}
	}

	public function createLoginForm(books : MBooks, lr : Login) : Void{
		var p : Person = lr.login;		
		//Copy the 
		var pCopy : Person = new Person(p.firstName, p.lastName, p.nickName, p.password);
		pCopy.createLoginForm(this);
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
		var d : Dynamic = haxe.Json.parse(aMessage);
		trace("Parsed message should look like so " + d);
		websocket.send(aMessage);
	}
	private function createConnectionForm() : Void {
		try {
		trace("Creating connection form");
		var document = Browser.document;
		var person : Person = new Person("", 
						"",
						"",
						"");

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
	public function showDashboard(p : Person) : Void {
		trace("Showing dashboard");
		
		var ccarM : model.CCAR = new model.CCAR("", "", p.nickName);
		var ccar : view.CCAR = new view.CCAR(ccarM, this);
		var div : DivElement = Util.createDivTag(Browser.document
			, "CCAR_ROOT");
		ccar.createCCARForm(div);
		ccar.queryAllCCARs();
	}
	private function keepAliveFunction() : Void {
		var commandType : String = "KeepAlive";
		var payload = {
			commandType : commandType
			, keepAlive : "Ping"
		};
		doSendJSON(haxe.Json.stringify(payload));

	}

}