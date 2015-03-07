
import haxe.Json;
import haxe.Utf8;
import haxe.Timer;
import js.html.Element;
import haxe.ds.GenericStack;
import js.html.Event;
import js.html.CloseEvent;
import js.html.MessageEvent;
import js.html.WebSocket;
import js.html.DOMCoreException;
import js.html.DivElement;
import js.html.Document;
import js.html.KeyboardEvent;
import js.html.InputElement;
import js.html.SelectElement;
import js.html.OptionElement;
import model.Contact;
import model.Login;
import model.Person;
import model.LoginStatus;
import model.Command;
import model.CommandType;
import util.Util;
import js.Browser;
import js.html.ButtonElement;
import promhx.Stream;
import promhx.Promise;
import massive.munit.TestRunner;
using promhx.haxe.EventTools;
import promhx.Deferred;

class MBooks_im {

	private static var singleton : MBooks_im;

	private var maxAttempts : Int = 3;
	function new (){
		trace("Calling MBooks_im");
		person = new model.Person("", "", "", "");
		outputEventStream = new Deferred<String>();
		var stream : Stream<Dynamic> = initializeElementStream(cast getNickNameElement(), "keyup");
		stream.then(sendLogin);
		var pStream : Stream<Dynamic> = initializeElementStream(cast getPasswordElement(), "keyup");			
		pStream.then(validatePassword);


	}

	static function main() {
		singleton = new MBooks_im();
		singleton.connect();
	}

	// Connection details
	function connectionString() : String {
		return protocol + "://" + serverHost + ":" + portNumber;
	}
	function connect() {
		trace("Calling connect");
		websocket = new WebSocket(connectionString());
		websocket.onclose = onClose;
		websocket.onerror = onError;
		
		var openStream = initializeElementStream(cast websocket, "open");
		openStream.then(onOpen);
		var eventStream = initializeElementStream(cast websocket, "message");
		eventStream.then(onMessage);

		var closeStream = initializeElementStream(cast websocket, "close");
		closeStream.then(onClose);

		var errorStream = initializeElementStream(cast websocket, "error");
		errorStream.then(onError);

	}
	public function logout() : Void{
		//trace("Logging out ");
		if(websocket != null){
			websocket.close();
		}else {
			//trace("No valid connection found");
		}
	}

	//Server publish queue
	public function getOutputEventStream() {
		return outputEventStream.stream();
	}
	public function initializeElementStream(ws : Element, event : String
				, ?useCapture: Bool) : Stream<Dynamic>{
		var def = new Deferred<Dynamic> ();
		ws.addEventListener(event, def.resolve, useCapture);
		return def.stream();
	}

	public  function onOpen(ev: Event){
		trace("Connection opened");
		getOutputEventStream().then(sendEvents);
	}

	public function onClose(ev: CloseEvent){
		trace("Connection closed " + ev.code + "->" + ev.reason);
		disableKeepAlive();

	}

	public  function onError(ev : Event){
		//trace("Error " + haxe.Json.stringify(ev));
	}

	// Message processing 

	private function parseCommandType(commandType : String) : CommandType {
	 //trace("Parsing command type " + commandType);
		try {
		return Type.createEnum(CommandType, commandType);
		}catch(e : Dynamic){
			//trace("Error " + e);
			return Undefined;
		}
	}

	public function parseIncomingMessage(incomingMessage : Dynamic) : Void {
		var commandType : CommandType = 
			parseCommandType(incomingMessage.commandType);
		switch(commandType){
			case Login : {
			    var person  : model.Person = incomingMessage.login;
				var login : Login = model.Login.createLoginResponse(incomingMessage, person);
				processLoginResponse(login);
			}
			case CCARUpload : {
				trace("Parsing ccar upload " + incomingMessage);
			}
			case ParsedCCARText : {
				//processParsedCCARText(incomingMessage);
			}
			case ManageUser: {
				processManageUser(incomingMessage);
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
			case SendMessage : {
				processSendMessage(incomingMessage);
			}
			case UserJoined : {
				processUserJoined(incomingMessage);
			}
			case UserLeft : {
				processUserLeft(incomingMessage);
			}

		}
	}

	public function onMessage(ev: MessageEvent) : Void{
		trace("Received stream " + ev.data);
		var incomingMessage = haxe.Json.parse(ev.data);
		//XXX: Needed to parse the incoming message twice because
		// of \",s in the response. This is a bug.
		//incomingMessage = haxe.Json.parse(incomingMessage);
		//trace("Printing incoming message " + incomingMessage);
		parseIncomingMessage(incomingMessage);
	}
	
	private function processLoginResponse(lR : Login){		
		//trace("Processing login object " + lR);
		//trace("Processing lR status " + lR.loginStatus);
		if(lR.loginStatus == null){
			//trace("Undefined state");
			return;
		}
		var lStatus : LoginStatus = Type.createEnum(LoginStatus, lR.loginStatus);
		if(this.getNickName() == lR.login.nickName){
			this.person.setPassword(lR.login.password);
			this.person.setFirstName(lR.login.firstName);
			this.person.setLastName(lR.login.lastName);
		}else {
			throw ("Nick name and responses dont match!!!! -> " + this.getNickName() + " not the same as " + lR.login.nickName);

		}
		//trace("Processing lStatus " + lStatus);
		if(lStatus == UserNotFound){
			//User not found, so enable the registration fields
			showDivField(DIV_PASSWORD);
			showDivField(DIV_FIRST_NAME);
			showDivField(DIV_LAST_NAME);
			
		}
		if(lStatus == UserExists){
			showDivField(DIV_PASSWORD);

		}
		if(lStatus == InvalidPassword){
			//createInvalidPassword(lR);
		}
		if(lStatus == Undefined){
			//createUndefined();
		}
	}
	private function processManageUser(p : Dynamic) {
		var person : Person = p.person;
		trace("Manage person");
	}
	private function processSendMessage(incomingMessage) {
		trace("Processing incoming message " + incomingMessage);
	}

	private function processUserJoined(incomingMessage){
		var userNickName = incomingMessage.userNickName;
		addToUsersOnline(userNickName);
	}
	private function processUserLeft(incomingMessage) {
		var userNickName = incomingMessage.userNickName;
		removeFromUsersOnline(userNickName);
	}
	private function showDivField(fieldName : String) {
		var div : DivElement = cast (Browser.document.getElementById(fieldName));
		div.setAttribute("style", "display:normal");
	}

	private function hideDivField(fieldName : String) {
		var div : DivElement = cast Browser.document.getElementById(fieldName);
		div.setAttribute("style", "display:none");
	}




	//Keep alive 
	private function initializeKeepAlive() : Void {
		if(timer == null){
			timer = new haxe.Timer(keepAliveInterval);
			timer.run = keepAliveFunction;
		}else {
			//trace("Timer already running. This should not happen");
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
	private function keepAliveFunction() : Void {
		var commandType : String = "KeepAlive";
		var payload = {
			nickName : this.getNickName()
			, commandType : commandType
			, keepAlive : "Ping"
		};
		trace("Sending keep alive " + payload);
		doSendJSON(haxe.Json.stringify(payload));
	}


	//Send messages
	private function sendEvents(aMessage : String){
		//trace("Sending " + aMessage);
		if(aMessage == "") {
			trace("Empty string. Not sending the message");
		}
		var d : Dynamic = haxe.Json.parse(aMessage);
		websocket.send(aMessage);
		//trace("Sent " + aMessage);
	}


	/**
	* Clients could be sending json 
	*/
	public  function doSendJSON(aMessage : String){
		trace("Resolving message " + aMessage);
		this.outputEventStream.resolve(aMessage);
	}

	//UI
	private static var NICK_NAME = "nickName";
	private static var PASSWORD = "password";
	private static var FIRST_NAME = "firstName";
	private static var LAST_NAME = "lastName";
	private static var DIV_PASSWORD = "passwordDiv";
	private static var DIV_FIRST_NAME = "firstNameDiv";
	private static var DIV_LAST_NAME = "lastNameDiv";
	private static var USERS_ONLINE = "usersOnline";
	private function getNickName() : String{
		return getNickNameElement().value;
	}

	private function getNickNameElement() : InputElement {
		var inputElement : InputElement = cast Browser.document.getElementById(NICK_NAME);
		return inputElement;
	}
	private function getPasswordElement() : InputElement {
		var inputElement : InputElement = cast Browser.document.getElementById(PASSWORD);
		return inputElement;
	}
	private function getPassword() : String {
		return getPasswordElement().value;
	}
	
	private function addToUsersOnline(nickName : String) : Void {
		var usersOnline : SelectElement = cast Browser.document.getElementById(USERS_ONLINE);
		var nickNameId = "NICKNAME" + "_" + nickName;
		var optionElement : OptionElement = cast Browser.document.getElementById(nickNameId);
		if(optionElement == null){
			optionElement = cast Browser.document.createOptionElement();
			optionElement.id = nickNameId;
			optionElement.text = nickName;
			usersOnline.appendChild(optionElement);
		}else {
			throw "This user was alread online"  + nickName;
		}

	}
	private function removeFromUsersOnline(nickName : String) : Void {

	}

	//Login and other stuff
	private function sendLogin (ev: KeyboardEvent){
		var inputElement : InputElement = cast ev.target;
		if(Util.isBackspace(ev.keyCode)){
			inputElement.value = "";
		}
		if(Util.isSignificantWS(ev.keyCode)){
			this.person.setNickName(inputElement.value);
			var lStatus : LoginStatus = Undefined;			
			var cType : String = Std.string(CommandType.Login);
			var l : Login = new Login(cType, this.person, lStatus);
			trace("Sending login status " + l);
			doSendJSON(Json.stringify(l));
		}
	}

	private function validatePassword(ev : KeyboardEvent){
		if(Util.isSignificantWS(ev.keyCode)){
			if(getPassword() != this.person.password){
				js.Lib.alert("Invalid password. Try again");
				attempts++;
				if(attempts > maxAttempts){
					js.Lib.alert("Too many attempts. Logging you out.");
					logout();
				}
			}else {
				trace("Password works!");
				var userLoggedIn = {
					userName : getNickName()
					, commandType : "UserLoggedIn"
				};
				doSendJSON(Json.stringify(userLoggedIn));
			}
		}
	}


	var attempts : Int = 0;
	var serverHost : String = "localhost";
	var protocol : String = "ws";
	var portNumber : Int = 3000;
	var keepAliveInterval : Int = 15000;
	var websocket : WebSocket;
	var timer : Timer;
	var outputEventStream : Deferred<String>;
	var person : model.Person;

}