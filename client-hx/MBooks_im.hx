/**
* License file : license.txt
*/
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
import js.html.File;
import js.html.KeyboardEvent;
import js.html.InputElement;
import js.html.SelectElement;
import js.html.OptionElement;
import js.html.FileReader;
import js.html.ImageElement;
import model.Contact;
import model.Login;
import model.Person;
import model.LoginStatus;
import model.Command;
import model.CommandType;
import model.UserOperation;
import model.Project;
import model.CCAR;
import util.Util;
import util.Config;
import js.Browser;
import js.html.ButtonElement;
import js.html.TextAreaElement;
import promhx.Stream;
import promhx.Promise;
import js.d3.D3;
import js.d3.D3;
import js.d3.scale.Scale;

import js.d3.selection.Selection;
import js.d3.selection.Selection;
import js.d3.layout.Layout;


import massive.munit.TestRunner;
using promhx.haxe.EventTools;
import promhx.Deferred;


class MBooks_im {

	private static var singleton : MBooks_im;

	public static function getSingleton() {
		return singleton;
	}
	function reset() {
		clearValue(cast getNickNameElement());
		getNickNameElement().disabled = false;
		clearValue(cast getMessageHistoryElement());
		clearValue(cast getPasswordElement());
		clearValue(cast getRegisterElement());
		clearValue(cast getFirstNameElement());
		clearValue(cast getLastNameElement());

	}

	private var maxAttempts : Int = 3;
	function new (){
		trace("Calling MBooks_im");
		reset();
		person = new model.Person("", "", "", "");
		outputEventStream = new Deferred<Dynamic>();
		trace("Registering nickname");
		var stream : Stream<Dynamic> = initializeElementStream(cast getNickNameElement(), "keyup");
		stream.then(sendLogin);
		trace("Registering password");
		var pStream : Stream<Dynamic> = initializeElementStream(cast getPasswordElement(), "keyup");			
		pStream.then(validatePassword);

		var rStream : Stream<Dynamic> = initializeElementStream(cast getRegisterElement(), "keyup");
		rStream.then(registerUser);

		var kStream : Stream<Dynamic> = initializeElementStream(cast getKickUserElement(), "keyup");
		kStream.then(kickUser);

		var mStream : Stream<Dynamic> = 
			initializeElementStream(getMessageInput(), "keyup");
		mStream.then(sendMessage);
		userLoggedIn = new Deferred<Dynamic>();
	}


	// Connection details
	private function connectionString() : String {
		return protocol + "://" + Browser.location.hostname + ":" + portNumber + "/chat";
	}

	private function connect() {
		trace("Calling connect");
		try {
		websocket = new WebSocket(connectionString());
		websocket.onclose = onClose;
		websocket.onerror = onServerConnectionError;
		
		var openStream = initializeElementStream(cast websocket, "open");
		openStream.then(onOpen);
		var eventStream = initializeElementStream(cast websocket, "message");
		eventStream.then(onMessage);

		var closeStream = initializeElementStream(cast websocket, "close");
		closeStream.then(onClose);

		var errorStream = initializeElementStream(cast websocket, "error");
		errorStream.then(onServerConnectionError);
		}catch(err : Dynamic) {
			trace("Error establishing connection " + err);
		}
		trace("Connection successful");
	}


	private function logout() : Void{
		trace("Logging out ");
		if(websocket != null){
			websocket.close();
		}else {
			trace("No valid connection found");
		}
	}

	//Server publish queue
	private function getOutputEventStream() {
		return outputEventStream.stream();
	}

	public function initializeElementStream(ws : Element, event : String
				, ?useCapture: Bool) : Stream<Dynamic>{
		try {
			var def = new Deferred<Dynamic> ();
			ws.addEventListener(event, def.resolve, useCapture);
			return def.stream();
		}catch(err: Dynamic) {
			trace ("Error creating element stream for " + event);
			throw "Unable to setup stream";
		}
	}

	private  function onOpen(ev: Event){
		trace("Connection opened");
		getOutputEventStream().then(sendEvents);
	}

	private function onClose(ev: CloseEvent){
		trace("Connection closed " + ev.code + "->" + ev.reason);
		setError(ev.code + ":" + ev.reason);
		disableKeepAlive();


	}

	private  function onServerConnectionError(ev : Event){
		trace("Error " + haxe.Json.stringify(ev));
		getNickNameElement().disabled = true;
		setError("Server not available. Please try back later.");
	}

	// Message processing 
	private function parseCommandType(incomingMessage : Dynamic) : CommandType {
		var commandType = incomingMessage.commandType;
		if (commandType == null){
			if(incomingMessage.Right != null) {
				commandType = incomingMessage.Right.commandType;
				if(commandType == null) {
					//try the other field...this is 
					//directly because of the unqualified variable names in 
					//haskell: need to fix this
					commandType = incomingMessage.Right.executeWorkbenchCommandType;
				}
			}			
		}
		try {
			trace("Command type " + commandType);
			return Type.createEnum(CommandType, commandType);
		}catch(e : Dynamic){
			trace("Error " + e + " Command type " + commandType);
			return Undefined;
		}
	}

	private function parseIncomingMessage(incomingMessage : Dynamic) : Void {
		var commandType : CommandType = 
			parseCommandType(incomingMessage);
		switch(commandType){
			case Login : {
			    var person  : model.Person = incomingMessage.login;
				var login : Login = model.Login.createLoginResponse(incomingMessage, person);
				processLoginResponse(login);
			}
			case CCARUpload : {
				trace("Parsing ccar upload " + incomingMessage);
				ccar.processCCARUpload(incomingMessage);
				
			}
			case ManageCompany : {
				company.processManageCompany(incomingMessage);
			}
			case SelectAllCompanies: {
				trace("Updating company list event stream");
				company.getSelectListEventStream().resolve(incomingMessage);	
			}
			case QuerySupportedScripts : {
				trace("Processing get supported scripts");
				try {
					project.getSupportedScriptsStream().resolve(incomingMessage);
				}catch(err : Dynamic){
					trace("Error processing supported scripts "  + err);
				}				
			}
			case QueryActiveWorkbenches : {
				trace("Processing query active workbenches");
				try {
					project.activeProjectWorkbench.queryActiveWorkbenchesStream.resolve(incomingMessage);
				}catch(err : Dynamic){
					trace("Error processing query active workbenches " + err);
				}
			}
			case ManageWorkbench : {
				trace("Processing manage workbench ");
				try {
					project.activeProjectWorkbench.manageWorkbenchStream.resolve(incomingMessage);
				}catch(err : Dynamic) {
					trace("Error processing manage workbench " + err);
				}
			}
			case ExecuteWorkbench :{
				trace("Processing execute workbench");
				try {
					project.activeProjectWorkbench.executeWorkbenchStream.resolve(incomingMessage);
				}catch(err : Dynamic){
					trace("Error processing execute workbench " + err);
				}
			}
			case SelectActiveProjects : {
				trace("Processing all active projects ");
				project.getSelectActiveProjectsStream().resolve(incomingMessage);		
			}
			case ManageProject : {
				trace("Manage project");
				project.processManageProject(incomingMessage);
			}
			case ParsedCCARText : {
				trace("Parsing ccar text " + incomingMessage);
				ccar.processParsedCCARText(incomingMessage);
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
			case UserBanned : {
				processUserBanned(incomingMessage);
			}
			case UserLoggedIn:{
				processUserLoggedIn(incomingMessage);
			}
			case UserLeft : {
				processUserLeft(incomingMessage);
			}
			case KeepAlive : {
				trace("Processing keep alive");
			}

		}
	}

	private function onMessage(ev: MessageEvent) : Void{
		trace("Received stream " + ev.data);
		var incomingMessage = haxe.Json.parse(ev.data);
		trace("Printing incoming message " + haxe.Json.stringify(incomingMessage));
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
		if(this.getNickName() == lR.login.nickName){
			this.person.setPassword(lR.login.password);
			this.person.setFirstName(lR.login.firstName);
			this.person.setLastName(lR.login.lastName);
		}else {
			throw ("Nick name and responses dont match!!!! -> " + this.getNickName() + " not the same as " + lR.login.nickName);

		}
		trace("Processing lStatus " + lStatus);
		if(lStatus == UserNotFound){
			//User not found, so enable the registration fields
			showDivField(DIV_PASSWORD);
			showDivField(DIV_FIRST_NAME);
			showDivField(DIV_LAST_NAME);
			showDivField(DIV_REGISTER);
			this.initializeKeepAlive();
		}
		if(lStatus == UserExists){
			showDivField(DIV_PASSWORD);
		}

		if(lStatus == InvalidPassword){
			//createInvalidPassword(lR);
		}
		if(lStatus == Undefined){
			throw ("Undefined status");			
		}
	}
	private function processManageUser(p : Dynamic) {
		var person : Person = p.person;
		trace("Manage person");
	}
	private function processSendMessage(incomingMessage) {
		var textAreaElement : TextAreaElement = cast Browser.document.getElementById(MESSAGE_HISTORY);
		if(incomingMessage.privateMessage != "") {
			textAreaElement.value = textAreaElement.value + incomingMessage.sentTime + "@" + incomingMessage.from + ":" + incomingMessage.privateMessage + "\n";
		}
	}
	private function updateMessageHistory(currentTime : Date, localMessage : String) {
		var textAreaElement : TextAreaElement = cast Browser.document.getElementById(MESSAGE_HISTORY);
		if(localMessage != "") {
			textAreaElement.value = textAreaElement.value + currentTime + "@" + 
				getNickName() + ":" + localMessage + "\n";
		}
	}

	private function processUserJoined(incomingMessage){
		trace("User joined " + Date.now());
	}
	private function processUserLoggedIn(incomingMessage) {
		if(incomingMessage.userName != getNickName()){
			addToUsersOnline(incomingMessage.userName);
		}
		userLoggedIn.resolve(incomingMessage);

	}
	private function processUserLeft(incomingMessage) {
		var userNickName = incomingMessage.userName; // Haskell record types and not being modular...
		removeFromUsersOnline(userNickName);
	}
	private function processUserBanned(incomingMessage) {
		var userNickName = incomingMessage.userName;
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
	private function keepAliveFunction() : Void {
		var commandType : String = "KeepAlive";
		var payload  : Dynamic = {
			nickName : this.getNickName()
			, commandType : commandType
			, keepAlive : "Ping"
		};
		trace("Sending keep alive " + payload);
		doSendJSON(payload);
	}


	//Send messages
	private function sendEvents(aMessage : Dynamic){
		websocket.send(haxe.Json.stringify(aMessage));
		trace("Sent " + aMessage);
	}



	//UI
	private static var NICK_NAME = "nickName";
	private static var PASSWORD = "password";
	private static var FIRST_NAME = "firstName";
	private static var LAST_NAME = "lastName";
	private static var DIV_PASSWORD = "passwordDiv";
	private static var DIV_FIRST_NAME = "firstNameDiv";
	private static var DIV_LAST_NAME = "lastNameDiv";
	private static var DIV_REGISTER = "registerDiv";
	private static var USERS_ONLINE = "usersOnline";
	private static var REGISTER = "registerInput";
	private static var MESSAGE_HISTORY = "messageHistory";
	private static var MESSAGE_INPUT = "messageInput";
	private static var STATUS_MESSAGE = "statusMessage";
	private static var KICK_USER = "kickUser";
	private static var KICK_USER_DIV = "kickUserDiv";
	private function getKickUserElement() : InputElement {
		return (cast Browser.document.getElementById(KICK_USER));
	}



	/**
	* Clients could be sending json 
	*/
	public  function doSendJSON(aMessage : Dynamic){
		this.outputEventStream.resolve(aMessage);
	}

	//The ui disables nickName element once validated.
	public function getNickName() : String{
		return getNickNameElement().value;
	}

	private function getStatusMessageElement() : Element {
		return Browser.document.getElementById(STATUS_MESSAGE);
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
		return StringTools.trim(getPasswordElement().value);
	}

	private function getFirstName() : String {
		return getFirstNameElement().value;
	}
	private function getFirstNameElement() : InputElement {
		var inputElement : InputElement = cast Browser.document.getElementById(FIRST_NAME);
		return inputElement;
	}
	private function getLastName(): String {
		return getLastNameElement().value;
	}
	private function getLastNameElement() : InputElement {
		var inputElement : InputElement = cast Browser.document.getElementById(LAST_NAME);
		return inputElement;
	}
	private function getRegisterElement (): InputElement {
		var inputElement : InputElement = cast Browser.document.getElementById(REGISTER);
		return inputElement;

	}

	private function getMessageInput() : InputElement {
		var inputElement : InputElement = cast Browser.document.getElementById(MESSAGE_INPUT);
		return inputElement;
	}
	private function getMessage() : String {
		return getMessageInput().value;
	}
	private function getMessageHistoryElement() : InputElement {
		var inputElement : InputElement = cast Browser.document.getElementById(MESSAGE_HISTORY);
		return inputElement;		
	}
	private function getMessageHistory() : String {
		return getMessageHistoryElement().value;
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
			throw "This user was already online"  + nickName;
		}

	}
	private function removeFromUsersOnline(nickName : String) : Void {
		trace("Deleting user from the list " + nickName);
		var usersOnline : SelectElement = cast Browser.document.getElementById(USERS_ONLINE);
		var nickNameId = "NICKNAME" + "_" + nickName;
		var optionElement : OptionElement = cast Browser.document.getElementById(nickNameId);
		if(optionElement != null){
			usersOnline.removeChild(optionElement);
		}else {
			trace("This user was already removed : ?"  + nickName);
		}

	}

	private function loginAsGuest(){
	var payload : Dynamic = {
		nickName : getNickName()
		, userName : getKickUserElement().value
		, commandType : "GuestUser"
		};
		doSendJSON(payload);
		this.initializeKeepAlive();
		this.hideDivField(KICK_USER_DIV);
	}

	private function getLoginRequest(nickName : String, status : LoginStatus) : Dynamic { 
			var lStatus : LoginStatus = status;			
			var cType : String = Std.string(CommandType.Login);
			var l : Login = new Login(cType, this.person, lStatus);
			return l;

	}

	//Login and other stuff
	private function sendLogin (ev: KeyboardEvent){
		var inputElement : InputElement = cast ev.target;
		trace("Inside send login " + ev.keyCode);
		if(Util.isSignificantWS(ev.keyCode)){
			var inputValue : String = StringTools.trim(inputElement.value);
			trace("Sending login information: " +  inputValue + ":");
			if((inputValue != "")) {
				this.person.setNickName(inputElement.value);
				var lStatus : LoginStatus = Undefined;			
				var cType : String = Std.string(CommandType.Login);
				var l : Login = new Login(cType, this.person, lStatus);
				trace("Sending login status " + l);
				doSendJSON(l);			
			}else {
				trace("Not sending any login");
			}
		}
	}

	private function sendMessage(ev : KeyboardEvent) {
		var inputElement : InputElement = cast ev.target;
		if(Util.isBackspace(ev.keyCode)){
			//inputElement.value = "";
		}
		if(Util.isSignificantWS(ev.keyCode)){
			var sentTime = Date.now();
			var payload : Dynamic = {
				nickName : getNickName()
				, from : getNickName()
				, to : getNickName()
				, privateMessage : getMessage()
				, commandType : "SendMessage"
				, destination :  {
					tag : "Broadcast"
					, contents : []
				}
				, sentTime : sentTime
			};
			doSendJSON(payload);
			updateMessageHistory(sentTime, getMessage());

			inputElement.value= ""; //Should we handle an exception here.
		}

	}
	private function validatePassword(ev : KeyboardEvent){
		if(Util.isSignificantWS(ev.keyCode)){
			trace("Password: " + getPassword() + ":");
			if(getPassword() == ""){
				trace("Not sending password");
				return;
			}
			if(getPassword() != this.person.password){
				js.Lib.alert("Invalid password. Try again");
				attempts++;
				if(attempts > maxAttempts){
					loginAsGuest();
					trace("Logging in as guest");
				}
			}else {
				trace("Password works!");
				var userLoggedIn : Dynamic = {
					userName : getNickName()
					, commandType : "UserLoggedIn"
				};
				getNickNameElement().disabled = true;
				doSendJSON(userLoggedIn);
				addStatusMessage(getNickName());
				showDivField("statusMessageDiv");
				this.initializeKeepAlive();
			}
		}
	}
	private function addStatusMessage(userMessage : String) {
		getStatusMessageElement().innerHTML = 
			getStatusMessageElement().innerHTML +  " : "  + userMessage;
	}

	private function kickUser(ev : KeyboardEvent) {
		if(Util.isSignificantWS(ev.keyCode)){		
			var payload : Dynamic = {
				nickName : getNickName()
				, userName : getKickUserElement().value
				, commandType : "UserBanned"
			};

			doSendJSON(payload);
		}else {
			//trace("Not a terminator " + ev.keyCode);
		}

	}
	private function registerUser(ev: KeyboardEvent){
		trace("Register user " + ev.keyCode);
		if(Util.isSignificantWS(ev.keyCode)){		
			var commandType : String = "ManageUser";
			var operation : UserOperation = new UserOperation("Create");
			var modelPerson = new Person(getFirstName()
					, getLastName()
					, getNickName()
					, getPassword());
			var uo : Dynamic = {
				commandType : "ManageUser"
				, nickName : getNickName()
				, operation:  {
					tag : "Create", 
					contents : []
				}
				, person : modelPerson
			};
			doSendJSON(uo);
			this.initializeKeepAlive();
		}else {
			trace("Not a terminator " + ev.keyCode);
		}
	}

	private function clearValue(inputElement : InputElement) {
		if(inputElement != null){
			inputElement.value = "";
		}else {
			throw "Null value for input element";
		}
	}

	private function getServerErrorElement() {
		return (cast (Browser.document.getElementById(SERVER_ERROR)));
	}
	private static var SERVER_ERROR : String = "serverError";

	private function setError(aMessage) {
		getServerErrorElement().innerHTML = aMessage;
	}

	public function getCompany() : Company {
		return company;
	}

	//Clients can attach or detach from this stream.
	public function getUserLoggedInStream() : Deferred<Dynamic> {
		return userLoggedIn;
	}
	public function createD3Element(){
		trace('Test creating d3 element.');
		D3.selectAll("p").style("color", function() {
  			return "hsl(" + Math.random() * 360 + ",100%,50%)";
		});

	}
	private function histogram(){
		
		var values:Array<Float> = D3.range(1000).map(randomIrwinHall(10));
		
		// A formatter for counts.
		var formatCount = D3.format(",.0f");

		var margin = {top: 10, right: 30, bottom: 30, left: 30},
			width = 960 - margin.left - margin.right,
			height = 500 - margin.top - margin.bottom;

		var x:Linear = D3.scale.linear()
			.domain([0, 1])
			.range([0, width]);
		
		// Generate a histogram using twenty uniformly-spaced bins.
		var data:Histogram = D3.layout.histogram().bins(x.ticks(20))(values);
		
		var y = D3.scale.linear()
			.domain([0, D3.max(data, function(d) { return d.y; })])
			.range([height, 0]);
		
		var xAxis = D3.svg.axis().scale(x);
		
		var svg = D3.select("body").append("svg")
			.attr("width", width + margin.left + margin.right)
			.attr("height", height + margin.top + margin.bottom)
			.append("g")
			.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

		var bar = svg.selectAll(".bar")
			.data(data)
			.enter().append("g")
			.attr("class", "bar")
			.attr("transform", function(d) { return "translate(" + getDynamic("x")(d.x) + "," + getDynamic("y")(d.y) + ")"; });

		bar.append("rect")
			.attr("x", 1)
			.attr("width", getDynamic("x")(data[0].dx) - 1)
			.attr("height", function(d) { return height - getDynamic("y")(d.y); });

		bar.append("text")
			.attr("dy", ".75em")
			.attr("y", 6)
			.attr("x", getDynamic("x")(data[0].dx) / 2)
			.attr("text-anchor", "middle")
			.text(function(d) { return formatCount(d.y); });

		svg.append("g")
			.attr("class", "x axis")
			.attr("transform", "translate(0," + height + ")")
			.call(xAxis);
    }

	private function pie() {
		
		/* pie test... */		
		var data	= [200,200,200,200,200];
		var i		= 1;
		var w 		= 960;
		var	h 		= 500;
		var	r 		= Math.min(w, h) / 2;
		var	color 	= D3.scale.category20();
		var	donut 	= D3.layout.pie().sort(null);
		var	arc 	= D3.svg.arc().innerRadius(r - 100).outerRadius(r - 20);
		
		var svg = D3.select("body").append("svg:svg")
			.attr("width", w)
			.attr("height", h)
			.append("svg:g")
			.attr("transform", "translate(" + w / 2 + "," + h / 2 + ")");
		
		var arcs = svg.selectAll("path")
			.data(getDynamic("donut")(data)).enter()
			.append("svg:path")
			.attr("fill", function(d, i) { return getDynamic("color")(i); })
			.attr("d", arc);
		
		D3.select(js.Browser.document.body).on("click", function(datum, index) {
			data = D3.range(5).map(randomIrwinHall(2));
			arcs.data(getDynamic("donut")(data)); // update the data
			arcs.attr("d", arc); // redraw the arcs
		});
	}

	public function drawGraph(statProfile : Array<Dynamic>) {

	}
	var attempts : Int = 0;
	var serverHost : String = "localhost";
	var protocol : String = "ws";
	var portNumber : Int = 3000;
	var keepAliveInterval : Int = 15000;
	var websocket : WebSocket;
	var timer : Timer;
	//General conduit for events going out to the server
	var outputEventStream : Deferred<Dynamic>;
	//Pipeline for user logged in events
	var userLoggedIn : Deferred<Dynamic>;
	var person : model.Person;
	var company : Company;
	var project : model.Project;
	var ccar : model.CCAR;
	static function main() {
		singleton = new MBooks_im();
		singleton.pie();
		singleton.company = new Company();
		singleton.project = new Project(singleton.company);
		singleton.connect();
	}

	/**
	 * 
	 * @param	m
	 */
	private static function randomIrwinHall(m:Int):Void->Float {
	  return function() {
		var s = 0.0;
		for (j in 0...m) s += Math.random();
		return s / m;
	  };
	}
	
	
	/** 
	 * Get a dynamic reference to a typed object so you can call it as a function in js) 
	 * Not great, but you can't call a class like a function in JS...
	 * Saves having to create a separate var that_is:Dynamic;
	 * The var 'name' gets inlined in the compiled output when you use this...
	 */
	public static inline function getDynamic(name:String):Dynamic {
		return untyped __js__(name);
	}


}