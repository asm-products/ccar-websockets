(function () { "use strict";
var $estr = function() { return js.Boot.__string_rec(this,''); };
function $extend(from, fields) {
	function inherit() {}; inherit.prototype = from; var proto = new inherit();
	for (var name in fields) proto[name] = fields[name];
	if( fields.toString !== Object.prototype.toString ) proto.toString = fields.toString;
	return proto;
}
var HxOverrides = function() { }
HxOverrides.__name__ = ["HxOverrides"];
HxOverrides.dateStr = function(date) {
	var m = date.getMonth() + 1;
	var d = date.getDate();
	var h = date.getHours();
	var mi = date.getMinutes();
	var s = date.getSeconds();
	return date.getFullYear() + "-" + (m < 10?"0" + m:"" + m) + "-" + (d < 10?"0" + d:"" + d) + " " + (h < 10?"0" + h:"" + h) + ":" + (mi < 10?"0" + mi:"" + mi) + ":" + (s < 10?"0" + s:"" + s);
}
HxOverrides.cca = function(s,index) {
	var x = s.charCodeAt(index);
	if(x != x) return undefined;
	return x;
}
HxOverrides.substr = function(s,pos,len) {
	if(pos != null && pos != 0 && len != null && len < 0) return "";
	if(len == null) len = s.length;
	if(pos < 0) {
		pos = s.length + pos;
		if(pos < 0) pos = 0;
	} else if(len < 0) len = s.length + len - pos;
	return s.substr(pos,len);
}
HxOverrides.iter = function(a) {
	return { cur : 0, arr : a, hasNext : function() {
		return this.cur < this.arr.length;
	}, next : function() {
		return this.arr[this.cur++];
	}};
}
var Lambda = function() { }
Lambda.__name__ = ["Lambda"];
Lambda.has = function(it,elt) {
	var $it0 = $iterator(it)();
	while( $it0.hasNext() ) {
		var x = $it0.next();
		if(x == elt) return true;
	}
	return false;
}
var List = function() {
	this.length = 0;
};
List.__name__ = ["List"];
List.prototype = {
	iterator: function() {
		return { h : this.h, hasNext : function() {
			return this.h != null;
		}, next : function() {
			if(this.h == null) return null;
			var x = this.h[0];
			this.h = this.h[1];
			return x;
		}};
	}
	,isEmpty: function() {
		return this.h == null;
	}
	,pop: function() {
		if(this.h == null) return null;
		var x = this.h[0];
		this.h = this.h[1];
		if(this.h == null) this.q = null;
		this.length--;
		return x;
	}
	,add: function(item) {
		var x = [item];
		if(this.h == null) this.h = x; else this.q[1] = x;
		this.q = x;
		this.length++;
	}
	,__class__: List
}
var MBooks_im = function() {
	this.keepAliveInterval = 15000;
	this.portNumber = 3000;
	this.protocol = "wss";
	this.serverHost = "localhost";
	this.attempts = 0;
	this.maxAttempts = 3;
	console.log("Calling MBooks_im");
	this.reset();
	this.person = new model.Person("","","","");
	this.outputEventStream = new promhx.Deferred();
	console.log("Registering nickname");
	var blurStream = this.initializeElementStream(this.getNickNameElement(),"blur");
	blurStream.then($bind(this,this.sendLoginBlur));
	console.log("Registering password");
	var rStream = this.initializeElementStream(this.getRegisterElement(),"click");
	rStream.then($bind(this,this.registerUser));
	var kStream = this.initializeElementStream(this.getKickUserElement(),"keyup");
	kStream.then($bind(this,this.kickUser));
	var mStream = this.initializeElementStream(this.getMessageInput(),"keyup");
	mStream.then($bind(this,this.sendMessage));
	this.userLoggedIn = new promhx.Deferred();
	this.userLoggedIn.then($bind(this,this.authenticationChecks));
	this.selectedCompanyStream = new promhx.Deferred();
	this.assignCompanyStream = new promhx.Deferred();
	this.activeCompanyStream = new promhx.Deferred();
	this.portfolioListStream = new promhx.Deferred();
	this.portfolioStream = new promhx.Deferred();
	this.applicationErrorStream = new promhx.Deferred();
	this.applicationErrorStream.then($bind(this,this.updateErrorMessages));
	this.getUserLoggedInStream().then($bind(this,this.processSuccessfulLogin));
	var oauthStream = this.initializeElementStream(this.getGmailOauthButton(),"click");
	oauthStream.then($bind(this,this.performGmailOauth));
	this.entitlements = new view.Entitlement();
	this.marketDataStream = new promhx.Deferred();
	this.companyEntitlements = new view.CompanyEntitlement(this.entitlements,this.selectedCompanyStream);
};
MBooks_im.__name__ = ["MBooks_im"];
MBooks_im.getSingleton = function() {
	return MBooks_im.singleton;
}
MBooks_im.main = function() {
	MBooks_im.singleton = new MBooks_im();
	MBooks_im.singleton.setupStreams();
	MBooks_im.singleton.connect();
}
MBooks_im.getDynamic = function(name) {
	return __js__(name);
}
MBooks_im.prototype = {
	authenticationChecks: function(incoming) {
		console.log("Processing " + Std.string(incoming));
		this.entitlements.queryAllEntitlements();
	}
	,setupStreams: function() {
	}
	,getUserLoggedInStream: function() {
		return this.userLoggedIn;
	}
	,getCompany: function() {
		return this.company;
	}
	,setError: function(aMessage) {
		this.applicationErrorStream.resolve(aMessage);
	}
	,getServerErrorElement: function() {
		return js.Browser.document.getElementById(MBooks_im.SERVER_ERROR);
	}
	,updateErrorMessages: function(incomingError) {
		this.getApplicationErrorElement().value = this.getApplicationErrorElement().value + ("" + Std.string(incomingError));
	}
	,getApplicationErrorElement: function() {
		return js.Browser.document.getElementById(MBooks_im.APPLICATION_ERROR);
	}
	,clearValue: function(inputElement) {
		if(inputElement != null) inputElement.value = ""; else throw "Null value for input element";
	}
	,registerUser: function(ev) {
		var commandType = "ManageUser";
		var operation = new model.UserOperation("Create");
		var modelPerson = new model.Person(this.getFirstName(),this.getLastName(),this.getNickName(),this.getPassword());
		var uo = { commandType : "ManageUser", nickName : this.getNickName(), operation : "Create", person : modelPerson};
		this.doSendJSON(uo);
		this.initializeKeepAlive();
	}
	,kickUser: function(ev) {
		if(util.Util.isSignificantWS(ev.keyCode)) {
			var payload = { nickName : this.getNickName(), userName : this.getKickUserElement().value, commandType : "UserBanned"};
			this.doSendJSON(payload);
		} else {
		}
	}
	,addStatusMessage: function(userMessage) {
		this.getStatusMessageElement().innerHTML = this.getStatusMessageElement().innerHTML + " : " + userMessage;
	}
	,validatePassword: function(ev) {
		console.log("Password: " + this.getPassword() + ":");
		if(this.getPassword() == "") {
			console.log("Not sending password");
			return;
		}
		if(this.getPassword() != this.person.password) {
			js.Lib.alert("Invalid password. Try again");
			this.attempts++;
			if(this.attempts > this.maxAttempts) {
				this.loginAsGuest();
				console.log("Logging in as guest");
			}
		} else {
			console.log("Password works!");
			var userLoggedIn = { userName : this.getNickName(), commandType : "UserLoggedIn"};
			this.getNickNameElement().disabled = true;
			this.doSendJSON(userLoggedIn);
			this.addStatusMessage(this.getNickName());
			this.showDivField("statusMessageDiv");
			this.initializeKeepAlive();
		}
	}
	,sendMessage: function(ev) {
		var inputElement = ev.target;
		if(util.Util.isBackspace(ev.keyCode)) {
		}
		if(util.Util.isSignificantWS(ev.keyCode)) {
			var sentTime = new Date();
			var payload = { nickName : this.getNickName(), from : this.getNickName(), to : this.getNickName(), privateMessage : this.getMessage(), commandType : "SendMessage", destination : { tag : "Broadcast", contents : []}, sentTime : sentTime};
			this.doSendJSON(payload);
			this.updateMessageHistory(sentTime,this.getMessage());
			inputElement.value = "";
		}
	}
	,sendLogin: function(ev) {
		var inputElement = ev.target;
		console.log("Inside send login " + ev.keyCode);
		if(util.Util.isSignificantWS(ev.keyCode)) {
			var inputValue = StringTools.trim(inputElement.value);
			console.log("Sending login information: " + inputValue + ":");
			if(inputValue != "") {
				this.person.setNickName(inputElement.value);
				var lStatus = model.LoginStatus.Undefined;
				var cType = Std.string(model.CommandType.Login);
				var l = new model.Login(cType,this.person,lStatus);
				console.log("Sending login status " + Std.string(l));
				this.doSendJSON(l);
			} else console.log("Not sending any login");
		}
	}
	,sendLoginBlur: function(ev) {
		var inputElement = ev.target;
		var inputValue = StringTools.trim(inputElement.value);
		console.log("Sending login information: " + inputValue + ":");
		if(inputValue != "") {
			this.person.setNickName(inputElement.value);
			var lStatus = model.LoginStatus.Undefined;
			var cType = Std.string(model.CommandType.Login);
			var l = new model.Login(cType,this.person,lStatus);
			console.log("Sending login status " + Std.string(l));
			this.doSendJSON(l);
		} else console.log("Not sending any login");
	}
	,getLoginRequest: function(nickName,status) {
		var lStatus = status;
		var cType = Std.string(model.CommandType.Login);
		var l = new model.Login(cType,this.person,lStatus);
		return l;
	}
	,loginAsGuest: function() {
		var payload = { nickName : this.getNickName(), userName : this.getKickUserElement().value, commandType : "GuestUser"};
		this.doSendJSON(payload);
		this.initializeKeepAlive();
		this.hideDivField(MBooks_im.KICK_USER_DIV);
	}
	,removeFromUsersOnline: function(nickName) {
		console.log("Deleting user from the list " + nickName);
		var usersOnline = js.Browser.document.getElementById(MBooks_im.USERS_ONLINE);
		var nickNameId = "NICKNAME" + "_" + nickName;
		var optionElement = js.Browser.document.getElementById(nickNameId);
		if(optionElement != null) usersOnline.removeChild(optionElement); else console.log("This user was already removed : ?" + nickName);
	}
	,addToUsersOnline: function(nickName) {
		var usersOnline = js.Browser.document.getElementById(MBooks_im.USERS_ONLINE);
		var nickNameId = "NICKNAME" + "_" + nickName;
		var optionElement = js.Browser.document.getElementById(nickNameId);
		if(optionElement == null) {
			optionElement = js.Browser.document.createElement("option");
			optionElement.id = nickNameId;
			optionElement.text = nickName;
			usersOnline.appendChild(optionElement);
		} else throw "This user was already online" + nickName;
	}
	,getMessageHistory: function() {
		return this.getMessageHistoryElement().value;
	}
	,getMessageHistoryElement: function() {
		var inputElement = js.Browser.document.getElementById(MBooks_im.MESSAGE_HISTORY);
		return inputElement;
	}
	,getMessage: function() {
		return this.getMessageInput().value;
	}
	,getMessageInput: function() {
		var inputElement = js.Browser.document.getElementById(MBooks_im.MESSAGE_INPUT);
		return inputElement;
	}
	,disableTab: function(tabName,tabSectionName) {
		var element = js.Browser.document.getElementById(tabName);
		console.log("Disabling tab " + tabName);
		element.setAttribute("style","display:none");
		var containerElement = js.Browser.document.getElementById(tabSectionName);
		containerElement.setAttribute("style","display:none");
	}
	,getRegisterElement: function() {
		var buttonElement = js.Browser.document.getElementById(MBooks_im.REGISTER);
		return buttonElement;
	}
	,getLastNameElement: function() {
		var inputElement = js.Browser.document.getElementById(MBooks_im.LAST_NAME);
		return inputElement;
	}
	,getLastName: function() {
		return this.getLastNameElement().value;
	}
	,getFirstNameElement: function() {
		var inputElement = js.Browser.document.getElementById(MBooks_im.FIRST_NAME);
		return inputElement;
	}
	,getFirstName: function() {
		return this.getFirstNameElement().value;
	}
	,getPassword: function() {
		return StringTools.trim(this.getPasswordElement().value);
	}
	,getPasswordElement: function() {
		var inputElement = js.Browser.document.getElementById(MBooks_im.PASSWORD);
		return inputElement;
	}
	,getNickNameElement: function() {
		var inputElement = js.Browser.document.getElementById(MBooks_im.NICK_NAME);
		return inputElement;
	}
	,getStatusMessageElement: function() {
		return js.Browser.document.getElementById(MBooks_im.STATUS_MESSAGE);
	}
	,getNickName: function() {
		return this.getNickNameElement().value;
	}
	,doSendJSON: function(aMessage) {
		console.log("Sending " + Std.string(aMessage));
		this.outputEventStream.resolve(aMessage);
	}
	,getKickUserElement: function() {
		return js.Browser.document.getElementById(MBooks_im.KICK_USER);
	}
	,getInitWelcomeElement: function() {
		return js.Browser.document.getElementById(MBooks_im.INIT_WELCOME_MESSAGE);
	}
	,sendEvents: function(aMessage) {
		this.websocket.send(haxe.Json.stringify(aMessage));
		console.log("Sent " + Std.string(aMessage));
	}
	,keepAliveFunction: function() {
		var commandType = "KeepAlive";
		var payload = { nickName : this.getNickName(), commandType : commandType, keepAlive : "Ping"};
		console.log("Sending keep alive " + Std.string(payload));
		this.doSendJSON(payload);
	}
	,disableKeepAlive: function() {
		if(this.timer == null) console.log("Nothing to disable"); else {
			console.log("Stopping the timer");
			this.timer.stop();
		}
	}
	,initializeKeepAlive: function() {
		if(this.timer == null) {
			this.timer = new haxe.Timer(this.keepAliveInterval);
			this.timer.run = $bind(this,this.keepAliveFunction);
		} else console.log("Timer already running. This should not happen");
	}
	,hideDivField: function(fieldName) {
		var div = js.Browser.document.getElementById(fieldName);
		div.setAttribute("style","display:none");
	}
	,showDivField: function(fieldName) {
		var div = js.Browser.document.getElementById(fieldName);
		div.setAttribute("style","display:normal");
	}
	,processUserBanned: function(incomingMessage) {
		var userNickName = incomingMessage.userName;
		this.removeFromUsersOnline(userNickName);
	}
	,processUserLeft: function(incomingMessage) {
		var userNickName = incomingMessage.userName;
		this.removeFromUsersOnline(userNickName);
	}
	,processUserLoggedIn: function(incomingMessage) {
		if(incomingMessage.userName != this.getNickName()) this.addToUsersOnline(incomingMessage.userName);
		this.userLoggedIn.resolve(incomingMessage);
	}
	,processUserJoined: function(incomingMessage) {
		console.log("User joined " + Std.string(new Date()));
	}
	,updateMessageHistory: function(currentTime,localMessage) {
		var textAreaElement = js.Browser.document.getElementById(MBooks_im.MESSAGE_HISTORY);
		if(localMessage != "") textAreaElement.value = textAreaElement.value + Std.string(currentTime) + "@" + this.getNickName() + ":" + localMessage + "\n";
	}
	,processSendMessage: function(incomingMessage) {
		var textAreaElement = js.Browser.document.getElementById(MBooks_im.MESSAGE_HISTORY);
		if(incomingMessage.privateMessage != "") textAreaElement.value = textAreaElement.value + incomingMessage.sentTime + "@" + incomingMessage.from + ":" + incomingMessage.privateMessage + "\n";
	}
	,processManageUser: function(p) {
		if(p.Right != null) {
			var person = p.Right.person;
			this.setInitWelcome(person);
		} else {
			console.log("Error processing manage user " + Std.string(p));
			this.setError(p);
		}
	}
	,setInitWelcome: function(p) {
		try {
			var person = p;
			var inputElement = this.getInitWelcomeElement();
			inputElement.innerHTML = inputElement.innerHTML + "," + person.nickName;
			this.showDivField(MBooks_im.INIT_WELCOME_MESSAGE_DIV);
		} catch( err ) {
			console.log(err);
			this.setError(err);
		}
	}
	,processLoginResponse: function(lR) {
		console.log("Processing login object " + Std.string(lR));
		console.log("Processing lR status " + lR.loginStatus);
		if(lR.loginStatus == null) {
			console.log("Undefined state");
			return;
		}
		var lStatus = Type.createEnum(model.LoginStatus,lR.loginStatus);
		if(this.getNickName() == lR.login.nickName) {
			this.person.setPassword(lR.login.password);
			this.person.setFirstName(lR.login.firstName);
			this.person.setLastName(lR.login.lastName);
		} else throw "Nick name and responses dont match!!!! -> " + this.getNickName() + " not the same as " + lR.login.nickName;
		console.log("Processing lStatus " + Std.string(lStatus));
		if(lStatus == model.LoginStatus.UserNotFound) {
			this.showDivField(MBooks_im.DIV_PASSWORD);
			this.showDivField(MBooks_im.DIV_FIRST_NAME);
			this.showDivField(MBooks_im.DIV_LAST_NAME);
			this.showDivField(MBooks_im.DIV_REGISTER);
			this.initializeKeepAlive();
			this.getPasswordElement().focus();
		}
		if(lStatus == model.LoginStatus.UserExists) {
			this.showDivField(MBooks_im.DIV_PASSWORD);
			var pStream = this.initializeElementStream(this.getPasswordElement(),"blur");
			pStream.then($bind(this,this.validatePassword));
			this.getPasswordElement().focus();
		}
		if(lStatus == model.LoginStatus.InvalidPassword) {
		}
		if(lStatus == model.LoginStatus.Undefined) throw "Undefined status";
	}
	,onMessage: function(ev) {
		console.log("Received stream " + Std.string(ev.data));
		var incomingMessage = haxe.Json.parse(ev.data);
		console.log("Printing incoming message " + haxe.Json.stringify(incomingMessage));
		this.parseIncomingMessage(incomingMessage);
	}
	,processUndefinedCommandType: function(incomingMessage) {
		console.log("Unhandled command type " + Std.string(incomingMessage));
	}
	,incomingMessageNull: function(source) {
		var errorMessage = "Incoming message is null. Should never happen. @ " + source;
		MBooks_im.getSingleton().applicationErrorStream.resolve(errorMessage);
	}
	,parseIncomingMessage: function(incomingMessage) {
		var commandType = this.parseCommandType(incomingMessage);
		switch( (commandType)[1] ) {
		case 0:
			var person = incomingMessage.Right.login;
			var login = model.Login.createLoginResponse(incomingMessage,person);
			this.processLoginResponse(login);
			break;
		case 11:
			console.log("Parsing ccar upload " + Std.string(incomingMessage));
			this.ccar.processCCARUpload(incomingMessage);
			break;
		case 18:
			this.company.processManageCompany(incomingMessage);
			break;
		case 20:
			console.log("Updating company list event stream");
			this.company.getSelectListEventStream().resolve(incomingMessage);
			break;
		case 23:
			console.log("Processing get supported scripts");
			try {
				this.project.getSupportedScriptsStream().resolve(incomingMessage);
			} catch( err ) {
				console.log("Error processing supported scripts " + Std.string(err));
			}
			break;
		case 24:
			console.log("Processing query active workbenches");
			try {
				this.project.activeProjectWorkbench.queryActiveWorkbenchesStream.resolve(incomingMessage);
			} catch( err ) {
				console.log("Error processing query active workbenches " + Std.string(err));
			}
			break;
		case 25:
			console.log("Processing manage workbench ");
			try {
				this.project.activeProjectWorkbench.manageWorkbenchStream.resolve(incomingMessage);
			} catch( err ) {
				console.log("Error processing manage workbench " + Std.string(err));
			}
			break;
		case 26:
			console.log("Processing execute workbench");
			try {
				this.project.activeProjectWorkbench.executeWorkbenchStream.resolve(incomingMessage);
			} catch( err ) {
				console.log("Error processing execute workbench " + Std.string(err));
			}
			break;
		case 21:
			console.log("Processing all active projects ");
			this.project.getSelectActiveProjectsStream().resolve(incomingMessage);
			break;
		case 22:
			console.log("Manage project");
			this.project.processManageProject(incomingMessage);
			break;
		case 12:
			console.log("Parsing ccar text " + Std.string(incomingMessage));
			this.ccar.processParsedCCARText(incomingMessage);
			break;
		case 2:
			this.processManageUser(incomingMessage);
			break;
		case 3:
			break;
		case 4:
			break;
		case 6:
			break;
		case 5:
			break;
		case 7:
			break;
		case 8:
			break;
		case 9:
			break;
		case 10:
			break;
		case 1:
			this.processSendMessage(incomingMessage);
			break;
		case 14:
			this.processUserJoined(incomingMessage);
			break;
		case 17:
			this.processUserBanned(incomingMessage);
			break;
		case 16:
			this.processUserLoggedIn(incomingMessage);
			break;
		case 15:
			this.processUserLeft(incomingMessage);
			break;
		case 27:
			console.log("Processing assigning company");
			this.assignCompanyStream.resolve(incomingMessage);
			break;
		case 19:
			console.log("Processing keep alive");
			break;
		case 28:
			console.log("Processing " + Std.string(incomingMessage));
			this.portfolioSymbolModel.typesStream.resolve(incomingMessage);
			break;
		case 29:
			console.log("Processing " + Std.string(incomingMessage));
			this.portfolioSymbolModel.sidesStream.resolve(incomingMessage);
			break;
		case 30:
			console.log("Processing " + Std.string(incomingMessage));
			this.portfolioListStream.resolve(incomingMessage);
			break;
		case 31:
			console.log("Processing " + Std.string(incomingMessage));
			this.portfolioStream.resolve(incomingMessage);
			break;
		case 32:
			console.log("Processing " + Std.string(incomingMessage));
			this.portfolioSymbolView.manage(incomingMessage);
			break;
		case 34:
			console.log("Processing " + Std.string(incomingMessage));
			this.portfolioSymbolView.symbolQueryResponse.resolve(incomingMessage);
			break;
		case 35:
			console.log("Processing " + Std.string(incomingMessage));
			this.entitlements.modelResponseStream.resolve(incomingMessage);
			break;
		case 36:
			this.entitlements.queryEntitlementResponse.resolve(incomingMessage);
			break;
		case 37:
			this.companyEntitlements.userListResponse.resolve(incomingMessage);
			break;
		case 33:
			this.marketDataStream.resolve(incomingMessage);
			break;
		case 13:
			this.processUndefinedCommandType(incomingMessage);
			this.entitlements.modelResponseStream.resolve(incomingMessage);
			break;
		}
	}
	,parseCommandType: function(incomingMessage) {
		var commandType = incomingMessage.commandType;
		if(commandType == null) {
			if(incomingMessage.Right != null) {
				commandType = incomingMessage.Right.commandType;
				if(commandType == null) commandType = incomingMessage.Right.executeWorkbenchCommandType;
			}
		}
		try {
			console.log("Command type " + commandType);
			return Type.createEnum(model.CommandType,commandType);
		} catch( e ) {
			console.log("Error " + Std.string(e) + " Command type " + commandType);
			return model.CommandType.Undefined;
		}
	}
	,onServerConnectionError: function(ev) {
		console.log("Error " + Std.string(ev));
		this.getOutputEventStream().end();
		this.websocket.close();
	}
	,cleanup: function() {
		console.log("Do all of the cleanup");
	}
	,onClose: function(ev) {
		console.log("Connection closed " + ev.code + "->" + ev.reason);
		this.setError(ev.code + ":" + ev.reason);
		this.cleanup();
		this.disableKeepAlive();
	}
	,onOpen: function(ev) {
		console.log("Connection opened");
		this.getOutputEventStream().then($bind(this,this.sendEvents));
	}
	,initializeElementStream: function(ws,event,useCapture) {
		try {
			var def = new promhx.Deferred();
			ws.addEventListener(event,$bind(def,def.resolve),useCapture);
			return def.stream();
		} catch( err ) {
			console.log("Error creating element stream for " + event);
			throw "Unable to setup stream";
		}
	}
	,getOutputEventStream: function() {
		return this.outputEventStream.stream();
	}
	,logout: function() {
		console.log("Logging out ");
		if(this.websocket != null) this.websocket.close(); else console.log("No valid connection found");
	}
	,connect: function() {
		console.log("Calling connect");
		try {
			this.websocket = new WebSocket(this.connectionString());
			this.websocket.onclose = $bind(this,this.onClose);
			this.websocket.onerror = $bind(this,this.onServerConnectionError);
			var openStream = this.initializeElementStream(this.websocket,"open");
			openStream.then($bind(this,this.onOpen));
			var eventStream = this.initializeElementStream(this.websocket,"message");
			eventStream.then($bind(this,this.onMessage));
			var closeStream = this.initializeElementStream(this.websocket,"close");
			closeStream.then($bind(this,this.onClose));
			var errorStream = this.initializeElementStream(this.websocket,"error");
			errorStream.then($bind(this,this.onServerConnectionError));
		} catch( err ) {
			console.log("Error establishing connection " + Std.string(err));
		}
		console.log("Connection successful");
	}
	,connectionString: function() {
		return this.protocol + "://" + js.Browser.location.hostname + "/chat";
	}
	,processSuccessfulLogin: function(loginEvent) {
		console.log("Process successful login " + Std.string(loginEvent));
		if(loginEvent.userName == this.getNickName()) {
			MBooks_im.singleton.company = new view.Company();
			MBooks_im.singleton.project = new model.Project(MBooks_im.singleton.company);
			MBooks_im.singleton.ccar = new model.CCAR("","","");
			MBooks_im.singleton.ccar.setupStreams();
			MBooks_im.singleton.portfolio = new view.Portfolio();
			MBooks_im.singleton.portfolioSymbolModel = new model.PortfolioSymbol();
			MBooks_im.singleton.portfolioSymbolView = new view.PortfolioSymbol(MBooks_im.singleton.portfolioSymbolModel);
		} else console.log("A new user logged in " + Std.string(loginEvent));
	}
	,getGmailOauthButton: function() {
		return js.Browser.document.getElementById(MBooks_im.SETUP_GMAIL);
	}
	,oauthRequestData: function(data) {
		var message = data;
		console.log("Data " + Std.string(message.data));
	}
	,performGmailOauth: function(incoming) {
		console.log("Processing gmail outh" + Std.string(incoming));
		var oauthRequest = new XMLHttpRequest();
		var url = "http://" + js.Browser.location.hostname + "/" + MBooks_im.GOAUTH_URL;
		oauthRequest.open("GET",url);
		oauthRequest.onloadend = $bind(this,this.oauthRequestData);
		oauthRequest.send();
	}
	,reset: function() {
		this.clearValue(this.getNickNameElement());
		this.getNickNameElement().disabled = false;
		this.clearValue(this.getMessageHistoryElement());
		this.clearValue(this.getPasswordElement());
		this.clearValue(this.getFirstNameElement());
		this.clearValue(this.getLastNameElement());
	}
	,__class__: MBooks_im
}
var IMap = function() { }
IMap.__name__ = ["IMap"];
var Reflect = function() { }
Reflect.__name__ = ["Reflect"];
Reflect.hasField = function(o,field) {
	return Object.prototype.hasOwnProperty.call(o,field);
}
Reflect.field = function(o,field) {
	var v = null;
	try {
		v = o[field];
	} catch( e ) {
	}
	return v;
}
Reflect.fields = function(o) {
	var a = [];
	if(o != null) {
		var hasOwnProperty = Object.prototype.hasOwnProperty;
		for( var f in o ) {
		if(f != "__id__" && f != "hx__closures__" && hasOwnProperty.call(o,f)) a.push(f);
		}
	}
	return a;
}
Reflect.isFunction = function(f) {
	return typeof(f) == "function" && !(f.__name__ || f.__ename__);
}
Reflect.deleteField = function(o,field) {
	if(!Reflect.hasField(o,field)) return false;
	delete(o[field]);
	return true;
}
Reflect.makeVarArgs = function(f) {
	return function() {
		var a = Array.prototype.slice.call(arguments);
		return f(a);
	};
}
var Std = function() { }
Std.__name__ = ["Std"];
Std.string = function(s) {
	return js.Boot.__string_rec(s,"");
}
Std.parseInt = function(x) {
	var v = parseInt(x,10);
	if(v == 0 && (HxOverrides.cca(x,1) == 120 || HxOverrides.cca(x,1) == 88)) v = parseInt(x);
	if(isNaN(v)) return null;
	return v;
}
Std.parseFloat = function(x) {
	return parseFloat(x);
}
var StringBuf = function() {
	this.b = "";
};
StringBuf.__name__ = ["StringBuf"];
StringBuf.prototype = {
	addSub: function(s,pos,len) {
		this.b += len == null?HxOverrides.substr(s,pos,null):HxOverrides.substr(s,pos,len);
	}
	,__class__: StringBuf
}
var StringTools = function() { }
StringTools.__name__ = ["StringTools"];
StringTools.isSpace = function(s,pos) {
	var c = HxOverrides.cca(s,pos);
	return c > 8 && c < 14 || c == 32;
}
StringTools.ltrim = function(s) {
	var l = s.length;
	var r = 0;
	while(r < l && StringTools.isSpace(s,r)) r++;
	if(r > 0) return HxOverrides.substr(s,r,l - r); else return s;
}
StringTools.rtrim = function(s) {
	var l = s.length;
	var r = 0;
	while(r < l && StringTools.isSpace(s,l - r - 1)) r++;
	if(r > 0) return HxOverrides.substr(s,0,l - r); else return s;
}
StringTools.trim = function(s) {
	return StringTools.ltrim(StringTools.rtrim(s));
}
var ValueType = { __ename__ : true, __constructs__ : ["TNull","TInt","TFloat","TBool","TObject","TFunction","TClass","TEnum","TUnknown"] }
ValueType.TNull = ["TNull",0];
ValueType.TNull.toString = $estr;
ValueType.TNull.__enum__ = ValueType;
ValueType.TInt = ["TInt",1];
ValueType.TInt.toString = $estr;
ValueType.TInt.__enum__ = ValueType;
ValueType.TFloat = ["TFloat",2];
ValueType.TFloat.toString = $estr;
ValueType.TFloat.__enum__ = ValueType;
ValueType.TBool = ["TBool",3];
ValueType.TBool.toString = $estr;
ValueType.TBool.__enum__ = ValueType;
ValueType.TObject = ["TObject",4];
ValueType.TObject.toString = $estr;
ValueType.TObject.__enum__ = ValueType;
ValueType.TFunction = ["TFunction",5];
ValueType.TFunction.toString = $estr;
ValueType.TFunction.__enum__ = ValueType;
ValueType.TClass = function(c) { var $x = ["TClass",6,c]; $x.__enum__ = ValueType; $x.toString = $estr; return $x; }
ValueType.TEnum = function(e) { var $x = ["TEnum",7,e]; $x.__enum__ = ValueType; $x.toString = $estr; return $x; }
ValueType.TUnknown = ["TUnknown",8];
ValueType.TUnknown.toString = $estr;
ValueType.TUnknown.__enum__ = ValueType;
var Type = function() { }
Type.__name__ = ["Type"];
Type.getClass = function(o) {
	if(o == null) return null;
	return o.__class__;
}
Type.getSuperClass = function(c) {
	return c.__super__;
}
Type.getClassName = function(c) {
	var a = c.__name__;
	return a.join(".");
}
Type.createInstance = function(cl,args) {
	switch(args.length) {
	case 0:
		return new cl();
	case 1:
		return new cl(args[0]);
	case 2:
		return new cl(args[0],args[1]);
	case 3:
		return new cl(args[0],args[1],args[2]);
	case 4:
		return new cl(args[0],args[1],args[2],args[3]);
	case 5:
		return new cl(args[0],args[1],args[2],args[3],args[4]);
	case 6:
		return new cl(args[0],args[1],args[2],args[3],args[4],args[5]);
	case 7:
		return new cl(args[0],args[1],args[2],args[3],args[4],args[5],args[6]);
	case 8:
		return new cl(args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7]);
	default:
		throw "Too many arguments";
	}
	return null;
}
Type.createEmptyInstance = function(cl) {
	function empty() {}; empty.prototype = cl.prototype;
	return new empty();
}
Type.createEnum = function(e,constr,params) {
	var f = Reflect.field(e,constr);
	if(f == null) throw "No such constructor " + constr;
	if(Reflect.isFunction(f)) {
		if(params == null) throw "Constructor " + constr + " need parameters";
		return f.apply(e,params);
	}
	if(params != null && params.length != 0) throw "Constructor " + constr + " does not need parameters";
	return f;
}
Type["typeof"] = function(v) {
	var _g = typeof(v);
	switch(_g) {
	case "boolean":
		return ValueType.TBool;
	case "string":
		return ValueType.TClass(String);
	case "number":
		if(Math.ceil(v) == v % 2147483648.0) return ValueType.TInt;
		return ValueType.TFloat;
	case "object":
		if(v == null) return ValueType.TNull;
		var e = v.__enum__;
		if(e != null) return ValueType.TEnum(e);
		var c = v.__class__;
		if(c != null) return ValueType.TClass(c);
		return ValueType.TObject;
	case "function":
		if(v.__name__ || v.__ename__) return ValueType.TObject;
		return ValueType.TFunction;
	case "undefined":
		return ValueType.TNull;
	default:
		return ValueType.TUnknown;
	}
}
Type.enumEq = function(a,b) {
	if(a == b) return true;
	try {
		if(a[0] != b[0]) return false;
		var _g1 = 2, _g = a.length;
		while(_g1 < _g) {
			var i = _g1++;
			if(!Type.enumEq(a[i],b[i])) return false;
		}
		var e = a.__enum__;
		if(e != b.__enum__ || e == null) return false;
	} catch( e ) {
		return false;
	}
	return true;
}
Type.enumIndex = function(e) {
	return e[1];
}
var format = {}
format.csv = {}
format.csv.Reader = function(separator,escape,endOfLine) {
	var _g = this;
	this.sep = separator != null?separator:",";
	if(this.stringLength(this.sep) != 1) throw "Separator string \"" + this.sep + "\" not allowed, only single char";
	this.esc = escape != null?escape:"\"";
	if(this.stringLength(this.esc) != 1) throw "Escape string \"" + this.esc + "\" not allowed, only single char";
	this.eol = endOfLine != null?endOfLine:["\r\n","\n"];
	if(Lambda.has(this.eol,null) || Lambda.has(this.eol,"")) throw "EOL sequences can't be empty";
	this.eol.sort(function(a,b) {
		return _g.stringLength(b) - _g.stringLength(a);
	});
	this.eolsize = this.eol.map($bind(this,this.stringLength));
	this.open(null,null);
};
format.csv.Reader.__name__ = ["format","csv","Reader"];
format.csv.Reader.readCsv = function(stream,separator,escape,endOfLine) {
	var p = new format.csv.Reader(separator,escape,endOfLine);
	p.inp = stream;
	return p;
}
format.csv.Reader.parseCsv = function(text,separator,escape,endOfLine) {
	var p = new format.csv.Reader(separator,escape,endOfLine);
	p.buffer = text;
	return p.readAll();
}
format.csv.Reader.read = function(text,separator,escape,endOfLine) {
	return format.csv.Reader.parseCsv(text,separator,escape,endOfLine);
}
format.csv.Reader.prototype = {
	iterator: function() {
		return this;
	}
	,next: function() {
		var r = this.readRecord();
		var nl = this.nextToken();
		if(nl != null && !Lambda.has(this.eol,nl)) throw "Unexpected \"" + nl + "\" after record";
		return r;
	}
	,hasNext: function() {
		return this.peekToken() != null;
	}
	,readAll: function() {
		var r = [];
		var nl;
		while(this.peekToken() != null) {
			r.push(this.readRecord());
			nl = this.nextToken();
			if(nl != null && !Lambda.has(this.eol,nl)) throw "Unexpected \"" + nl + "\" after record";
		}
		return r;
	}
	,reset: function(string,stream) {
		return this.open(string,stream);
	}
	,open: function(string,stream) {
		this.buffer = string != null?string:"";
		this.inp = stream;
		this.pos = 0;
		this.bufferOffset = 0;
		this.cachedToken = null;
		this.cachedPos = 0;
		return this;
	}
	,readRecord: function() {
		var r = [];
		r.push(this.readField());
		while(this.peekToken() == this.sep) {
			this.nextToken();
			r.push(this.readField());
		}
		return r;
	}
	,readField: function() {
		var cur = this.peekToken();
		if(cur == this.esc) {
			this.nextToken();
			var s = this.readEscapedString();
			var fi = this.nextToken();
			if(fi != this.esc) throw "Missing " + this.esc + " at the end of escaped field " + (s.length > 15?HxOverrides.substr(s,0,10) + "[...]":s);
			return s;
		} else return this.readString();
	}
	,readString: function() {
		var buf = new StringBuf();
		var x = this.readSafeChar();
		while(x != null) {
			buf.b += Std.string(x);
			x = this.readSafeChar();
		}
		return buf.b;
	}
	,readEscapedString: function() {
		var buf = new StringBuf();
		var x = this.readEscapedChar();
		while(x != null) {
			buf.b += Std.string(x);
			x = this.readEscapedChar();
		}
		return buf.b;
	}
	,readEscapedChar: function() {
		var cur = this.peekToken();
		if(cur == this.esc) {
			if(this.peekToken(1) != this.esc) return null;
			this.nextToken();
		}
		return this.nextToken();
	}
	,readSafeChar: function() {
		var cur = this.peekToken();
		if(cur == this.sep || cur == this.esc || Lambda.has(this.eol,cur)) return null;
		return this.nextToken();
	}
	,nextToken: function() {
		var ret = this.peekToken();
		if(ret == null) return null;
		this.pos = this.cachedPos;
		this.cachedToken = null;
		return ret;
	}
	,peekToken: function(skip) {
		if(skip == null) skip = 0;
		var token = this.cachedToken, p = this.pos;
		if(token != null) {
			p = this.cachedPos;
			skip--;
		}
		while(skip-- >= 0) {
			token = this.get(p,1);
			if(token == null) break;
			var _g1 = 0, _g = this.eol.length;
			while(_g1 < _g) {
				var i = _g1++;
				var t = this.get(p,this.eolsize[i]);
				if(t == this.eol[i]) {
					token = t;
					break;
				}
			}
			p += this.stringLength(token);
			if(this.cachedToken == null) {
				this.cachedToken = token;
				this.cachedPos = p;
			}
		}
		return token;
	}
	,get: function(p,len) {
		var bpos = p - this.bufferOffset;
		if(bpos + len > this.stringLength(this.buffer)) {
			var more = this.fetchBytes(4096);
			if(more != null) {
				this.buffer = this.substring(this.buffer,this.pos - this.bufferOffset) + more;
				this.bufferOffset = this.pos;
				bpos = p - this.bufferOffset;
			}
		}
		var ret = this.substring(this.buffer,bpos,len);
		return ret != ""?ret:null;
	}
	,fetchBytes: function(n) {
		if(this.inp == null) return null;
		try {
			var bytes = haxe.io.Bytes.alloc(n);
			var got = this.inp.readBytes(bytes,0,n);
			return bytes.readString(0,got);
		} catch( e ) {
			if( js.Boot.__instanceof(e,haxe.io.Eof) ) {
				return null;
			} else throw(e);
		}
	}
	,stringLength: function(str) {
		return str.length;
	}
	,substring: function(str,pos,length) {
		return HxOverrides.substr(str,pos,length);
	}
	,__class__: format.csv.Reader
}
var haxe = {}
haxe.StackItem = { __ename__ : true, __constructs__ : ["CFunction","Module","FilePos","Method","Lambda"] }
haxe.StackItem.CFunction = ["CFunction",0];
haxe.StackItem.CFunction.toString = $estr;
haxe.StackItem.CFunction.__enum__ = haxe.StackItem;
haxe.StackItem.Module = function(m) { var $x = ["Module",1,m]; $x.__enum__ = haxe.StackItem; $x.toString = $estr; return $x; }
haxe.StackItem.FilePos = function(s,file,line) { var $x = ["FilePos",2,s,file,line]; $x.__enum__ = haxe.StackItem; $x.toString = $estr; return $x; }
haxe.StackItem.Method = function(classname,method) { var $x = ["Method",3,classname,method]; $x.__enum__ = haxe.StackItem; $x.toString = $estr; return $x; }
haxe.StackItem.Lambda = function(v) { var $x = ["Lambda",4,v]; $x.__enum__ = haxe.StackItem; $x.toString = $estr; return $x; }
haxe.CallStack = function() { }
haxe.CallStack.__name__ = ["haxe","CallStack"];
haxe.CallStack.exceptionStack = function() {
	return [];
}
haxe.Json = function() {
};
haxe.Json.__name__ = ["haxe","Json"];
haxe.Json.parse = function(text) {
	return new haxe.Json().doParse(text);
}
haxe.Json.stringify = function(value,replacer) {
	return new haxe.Json().toString(value,replacer);
}
haxe.Json.prototype = {
	parseNumber: function(c) {
		var start = this.pos - 1;
		var minus = c == 45, digit = !minus, zero = c == 48;
		var point = false, e = false, pm = false, end = false;
		while(true) {
			c = this.str.charCodeAt(this.pos++);
			switch(c) {
			case 48:
				if(zero && !point) this.invalidNumber(start);
				if(minus) {
					minus = false;
					zero = true;
				}
				digit = true;
				break;
			case 49:case 50:case 51:case 52:case 53:case 54:case 55:case 56:case 57:
				if(zero && !point) this.invalidNumber(start);
				if(minus) minus = false;
				digit = true;
				zero = false;
				break;
			case 46:
				if(minus || point) this.invalidNumber(start);
				digit = false;
				point = true;
				break;
			case 101:case 69:
				if(minus || zero || e) this.invalidNumber(start);
				digit = false;
				e = true;
				break;
			case 43:case 45:
				if(!e || pm) this.invalidNumber(start);
				digit = false;
				pm = true;
				break;
			default:
				if(!digit) this.invalidNumber(start);
				this.pos--;
				end = true;
			}
			if(end) break;
		}
		var f = Std.parseFloat(HxOverrides.substr(this.str,start,this.pos - start));
		var i = f | 0;
		return i == f?i:f;
	}
	,invalidNumber: function(start) {
		throw "Invalid number at position " + start + ": " + HxOverrides.substr(this.str,start,this.pos - start);
	}
	,parseString: function() {
		var start = this.pos;
		var buf = new StringBuf();
		while(true) {
			var c = this.str.charCodeAt(this.pos++);
			if(c == 34) break;
			if(c == 92) {
				buf.addSub(this.str,start,this.pos - start - 1);
				c = this.str.charCodeAt(this.pos++);
				switch(c) {
				case 114:
					buf.b += "\r";
					break;
				case 110:
					buf.b += "\n";
					break;
				case 116:
					buf.b += "\t";
					break;
				case 98:
					buf.b += "";
					break;
				case 102:
					buf.b += "";
					break;
				case 47:case 92:case 34:
					buf.b += String.fromCharCode(c);
					break;
				case 117:
					var uc = Std.parseInt("0x" + HxOverrides.substr(this.str,this.pos,4));
					this.pos += 4;
					buf.b += String.fromCharCode(uc);
					break;
				default:
					throw "Invalid escape sequence \\" + String.fromCharCode(c) + " at position " + (this.pos - 1);
				}
				start = this.pos;
			} else if(c != c) throw "Unclosed string";
		}
		buf.addSub(this.str,start,this.pos - start - 1);
		return buf.b;
	}
	,parseRec: function() {
		while(true) {
			var c = this.str.charCodeAt(this.pos++);
			switch(c) {
			case 32:case 13:case 10:case 9:
				break;
			case 123:
				var obj = { }, field = null, comma = null;
				while(true) {
					var c1 = this.str.charCodeAt(this.pos++);
					switch(c1) {
					case 32:case 13:case 10:case 9:
						break;
					case 125:
						if(field != null || comma == false) this.invalidChar();
						return obj;
					case 58:
						if(field == null) this.invalidChar();
						obj[field] = this.parseRec();
						field = null;
						comma = true;
						break;
					case 44:
						if(comma) comma = false; else this.invalidChar();
						break;
					case 34:
						if(comma) this.invalidChar();
						field = this.parseString();
						break;
					default:
						this.invalidChar();
					}
				}
				break;
			case 91:
				var arr = [], comma = null;
				while(true) {
					var c1 = this.str.charCodeAt(this.pos++);
					switch(c1) {
					case 32:case 13:case 10:case 9:
						break;
					case 93:
						if(comma == false) this.invalidChar();
						return arr;
					case 44:
						if(comma) comma = false; else this.invalidChar();
						break;
					default:
						if(comma) this.invalidChar();
						this.pos--;
						arr.push(this.parseRec());
						comma = true;
					}
				}
				break;
			case 116:
				var save = this.pos;
				if(this.str.charCodeAt(this.pos++) != 114 || this.str.charCodeAt(this.pos++) != 117 || this.str.charCodeAt(this.pos++) != 101) {
					this.pos = save;
					this.invalidChar();
				}
				return true;
			case 102:
				var save = this.pos;
				if(this.str.charCodeAt(this.pos++) != 97 || this.str.charCodeAt(this.pos++) != 108 || this.str.charCodeAt(this.pos++) != 115 || this.str.charCodeAt(this.pos++) != 101) {
					this.pos = save;
					this.invalidChar();
				}
				return false;
			case 110:
				var save = this.pos;
				if(this.str.charCodeAt(this.pos++) != 117 || this.str.charCodeAt(this.pos++) != 108 || this.str.charCodeAt(this.pos++) != 108) {
					this.pos = save;
					this.invalidChar();
				}
				return null;
			case 34:
				return this.parseString();
			case 48:case 49:case 50:case 51:case 52:case 53:case 54:case 55:case 56:case 57:case 45:
				return this.parseNumber(c);
			default:
				this.invalidChar();
			}
		}
	}
	,invalidChar: function() {
		this.pos--;
		throw "Invalid char " + this.str.charCodeAt(this.pos) + " at position " + this.pos;
	}
	,doParse: function(str) {
		this.str = str;
		this.pos = 0;
		return this.parseRec();
	}
	,quote: function(s) {
		this.buf.b += "\"";
		var i = 0;
		while(true) {
			var c = s.charCodeAt(i++);
			if(c != c) break;
			switch(c) {
			case 34:
				this.buf.b += "\\\"";
				break;
			case 92:
				this.buf.b += "\\\\";
				break;
			case 10:
				this.buf.b += "\\n";
				break;
			case 13:
				this.buf.b += "\\r";
				break;
			case 9:
				this.buf.b += "\\t";
				break;
			case 8:
				this.buf.b += "\\b";
				break;
			case 12:
				this.buf.b += "\\f";
				break;
			default:
				this.buf.b += String.fromCharCode(c);
			}
		}
		this.buf.b += "\"";
	}
	,toStringRec: function(k,v) {
		if(this.replacer != null) v = this.replacer(k,v);
		var _g = Type["typeof"](v);
		var $e = (_g);
		switch( $e[1] ) {
		case 8:
			this.buf.b += "\"???\"";
			break;
		case 4:
			this.objString(v);
			break;
		case 1:
			var v1 = v;
			this.buf.b += Std.string(v1);
			break;
		case 2:
			this.buf.b += Std.string(Math.isFinite(v)?v:"null");
			break;
		case 5:
			this.buf.b += "\"<fun>\"";
			break;
		case 6:
			var c = $e[2];
			if(c == String) this.quote(v); else if(c == Array) {
				var v1 = v;
				this.buf.b += "[";
				var len = v1.length;
				if(len > 0) {
					this.toStringRec(0,v1[0]);
					var i = 1;
					while(i < len) {
						this.buf.b += ",";
						this.toStringRec(i,v1[i++]);
					}
				}
				this.buf.b += "]";
			} else if(c == haxe.ds.StringMap) {
				var v1 = v;
				var o = { };
				var $it0 = v1.keys();
				while( $it0.hasNext() ) {
					var k1 = $it0.next();
					o[k1] = v1.get(k1);
				}
				this.objString(o);
			} else this.objString(v);
			break;
		case 7:
			var i = Type.enumIndex(v);
			var v1 = i;
			this.buf.b += Std.string(v1);
			break;
		case 3:
			var v1 = v;
			this.buf.b += Std.string(v1);
			break;
		case 0:
			this.buf.b += "null";
			break;
		}
	}
	,objString: function(v) {
		this.fieldsString(v,Reflect.fields(v));
	}
	,fieldsString: function(v,fields) {
		var first = true;
		this.buf.b += "{";
		var _g = 0;
		while(_g < fields.length) {
			var f = fields[_g];
			++_g;
			var value = Reflect.field(v,f);
			if(Reflect.isFunction(value)) continue;
			if(first) first = false; else this.buf.b += ",";
			this.quote(f);
			this.buf.b += ":";
			this.toStringRec(f,value);
		}
		this.buf.b += "}";
	}
	,toString: function(v,replacer) {
		this.buf = new StringBuf();
		this.replacer = replacer;
		this.toStringRec("",v);
		return this.buf.b;
	}
	,__class__: haxe.Json
}
haxe.Timer = function(time_ms) {
	var me = this;
	this.id = setInterval(function() {
		me.run();
	},time_ms);
};
haxe.Timer.__name__ = ["haxe","Timer"];
haxe.Timer.prototype = {
	run: function() {
		console.log("run");
	}
	,stop: function() {
		if(this.id == null) return;
		clearInterval(this.id);
		this.id = null;
	}
	,__class__: haxe.Timer
}
haxe.ds = {}
haxe.ds.ObjectMap = function() { }
haxe.ds.ObjectMap.__name__ = ["haxe","ds","ObjectMap"];
haxe.ds.ObjectMap.__interfaces__ = [IMap];
haxe.ds.Option = { __ename__ : true, __constructs__ : ["Some","None"] }
haxe.ds.Option.Some = function(v) { var $x = ["Some",0,v]; $x.__enum__ = haxe.ds.Option; $x.toString = $estr; return $x; }
haxe.ds.Option.None = ["None",1];
haxe.ds.Option.None.toString = $estr;
haxe.ds.Option.None.__enum__ = haxe.ds.Option;
haxe.ds.StringMap = function() {
	this.h = { };
};
haxe.ds.StringMap.__name__ = ["haxe","ds","StringMap"];
haxe.ds.StringMap.__interfaces__ = [IMap];
haxe.ds.StringMap.prototype = {
	iterator: function() {
		return { ref : this.h, it : this.keys(), hasNext : function() {
			return this.it.hasNext();
		}, next : function() {
			var i = this.it.next();
			return this.ref["$" + i];
		}};
	}
	,keys: function() {
		var a = [];
		for( var key in this.h ) {
		if(this.h.hasOwnProperty(key)) a.push(key.substr(1));
		}
		return HxOverrides.iter(a);
	}
	,remove: function(key) {
		key = "$" + key;
		if(!this.h.hasOwnProperty(key)) return false;
		delete(this.h[key]);
		return true;
	}
	,exists: function(key) {
		return this.h.hasOwnProperty("$" + key);
	}
	,get: function(key) {
		return this.h["$" + key];
	}
	,set: function(key,value) {
		this.h["$" + key] = value;
	}
	,__class__: haxe.ds.StringMap
}
haxe.io = {}
haxe.io.Bytes = function(length,b) {
	this.length = length;
	this.b = b;
};
haxe.io.Bytes.__name__ = ["haxe","io","Bytes"];
haxe.io.Bytes.alloc = function(length) {
	var a = new Array();
	var _g = 0;
	while(_g < length) {
		var i = _g++;
		a.push(0);
	}
	return new haxe.io.Bytes(length,a);
}
haxe.io.Bytes.prototype = {
	readString: function(pos,len) {
		if(pos < 0 || len < 0 || pos + len > this.length) throw haxe.io.Error.OutsideBounds;
		var s = "";
		var b = this.b;
		var fcc = String.fromCharCode;
		var i = pos;
		var max = pos + len;
		while(i < max) {
			var c = b[i++];
			if(c < 128) {
				if(c == 0) break;
				s += fcc(c);
			} else if(c < 224) s += fcc((c & 63) << 6 | b[i++] & 127); else if(c < 240) {
				var c2 = b[i++];
				s += fcc((c & 31) << 12 | (c2 & 127) << 6 | b[i++] & 127);
			} else {
				var c2 = b[i++];
				var c3 = b[i++];
				s += fcc((c & 15) << 18 | (c2 & 127) << 12 | c3 << 6 & 127 | b[i++] & 127);
			}
		}
		return s;
	}
	,__class__: haxe.io.Bytes
}
haxe.io.Eof = function() { }
haxe.io.Eof.__name__ = ["haxe","io","Eof"];
haxe.io.Eof.prototype = {
	toString: function() {
		return "Eof";
	}
	,__class__: haxe.io.Eof
}
haxe.io.Error = { __ename__ : true, __constructs__ : ["Blocked","Overflow","OutsideBounds","Custom"] }
haxe.io.Error.Blocked = ["Blocked",0];
haxe.io.Error.Blocked.toString = $estr;
haxe.io.Error.Blocked.__enum__ = haxe.io.Error;
haxe.io.Error.Overflow = ["Overflow",1];
haxe.io.Error.Overflow.toString = $estr;
haxe.io.Error.Overflow.__enum__ = haxe.io.Error;
haxe.io.Error.OutsideBounds = ["OutsideBounds",2];
haxe.io.Error.OutsideBounds.toString = $estr;
haxe.io.Error.OutsideBounds.__enum__ = haxe.io.Error;
haxe.io.Error.Custom = function(e) { var $x = ["Custom",3,e]; $x.__enum__ = haxe.io.Error; $x.toString = $estr; return $x; }
haxe.io.Input = function() { }
haxe.io.Input.__name__ = ["haxe","io","Input"];
haxe.io.Input.prototype = {
	readBytes: function(s,pos,len) {
		var k = len;
		var b = s.b;
		if(pos < 0 || len < 0 || pos + len > s.length) throw haxe.io.Error.OutsideBounds;
		while(k > 0) {
			b[pos] = this.readByte();
			pos++;
			k--;
		}
		return len;
	}
	,readByte: function() {
		return (function($this) {
			var $r;
			throw "Not implemented";
			return $r;
		}(this));
	}
	,__class__: haxe.io.Input
}
haxe.rtti = {}
haxe.rtti.Meta = function() { }
haxe.rtti.Meta.__name__ = ["haxe","rtti","Meta"];
haxe.rtti.Meta.getFields = function(t) {
	var meta = t.__meta__;
	return meta == null || meta.fields == null?{ }:meta.fields;
}
var js = {}
js.Boot = function() { }
js.Boot.__name__ = ["js","Boot"];
js.Boot.__string_rec = function(o,s) {
	if(o == null) return "null";
	if(s.length >= 5) return "<...>";
	var t = typeof(o);
	if(t == "function" && (o.__name__ || o.__ename__)) t = "object";
	switch(t) {
	case "object":
		if(o instanceof Array) {
			if(o.__enum__) {
				if(o.length == 2) return o[0];
				var str = o[0] + "(";
				s += "\t";
				var _g1 = 2, _g = o.length;
				while(_g1 < _g) {
					var i = _g1++;
					if(i != 2) str += "," + js.Boot.__string_rec(o[i],s); else str += js.Boot.__string_rec(o[i],s);
				}
				return str + ")";
			}
			var l = o.length;
			var i;
			var str = "[";
			s += "\t";
			var _g = 0;
			while(_g < l) {
				var i1 = _g++;
				str += (i1 > 0?",":"") + js.Boot.__string_rec(o[i1],s);
			}
			str += "]";
			return str;
		}
		var tostr;
		try {
			tostr = o.toString;
		} catch( e ) {
			return "???";
		}
		if(tostr != null && tostr != Object.toString) {
			var s2 = o.toString();
			if(s2 != "[object Object]") return s2;
		}
		var k = null;
		var str = "{\n";
		s += "\t";
		var hasp = o.hasOwnProperty != null;
		for( var k in o ) { ;
		if(hasp && !o.hasOwnProperty(k)) {
			continue;
		}
		if(k == "prototype" || k == "__class__" || k == "__super__" || k == "__interfaces__" || k == "__properties__") {
			continue;
		}
		if(str.length != 2) str += ", \n";
		str += s + k + " : " + js.Boot.__string_rec(o[k],s);
		}
		s = s.substring(1);
		str += "\n" + s + "}";
		return str;
	case "function":
		return "<function>";
	case "string":
		return o;
	default:
		return String(o);
	}
}
js.Boot.__interfLoop = function(cc,cl) {
	if(cc == null) return false;
	if(cc == cl) return true;
	var intf = cc.__interfaces__;
	if(intf != null) {
		var _g1 = 0, _g = intf.length;
		while(_g1 < _g) {
			var i = _g1++;
			var i1 = intf[i];
			if(i1 == cl || js.Boot.__interfLoop(i1,cl)) return true;
		}
	}
	return js.Boot.__interfLoop(cc.__super__,cl);
}
js.Boot.__instanceof = function(o,cl) {
	if(cl == null) return false;
	switch(cl) {
	case Int:
		return (o|0) === o;
	case Float:
		return typeof(o) == "number";
	case Bool:
		return typeof(o) == "boolean";
	case String:
		return typeof(o) == "string";
	case Dynamic:
		return true;
	default:
		if(o != null) {
			if(typeof(cl) == "function") {
				if(o instanceof cl) {
					if(cl == Array) return o.__enum__ == null;
					return true;
				}
				if(js.Boot.__interfLoop(o.__class__,cl)) return true;
			}
		} else return false;
		if(cl == Class && o.__name__ != null) return true;
		if(cl == Enum && o.__ename__ != null) return true;
		return o.__enum__ == cl;
	}
}
js.Browser = function() { }
js.Browser.__name__ = ["js","Browser"];
js.Lib = function() { }
js.Lib.__name__ = ["js","Lib"];
js.Lib.alert = function(v) {
	alert(js.Boot.__string_rec(v,""));
}
js.d3 = {}
js.d3._D3 = {}
js.d3._D3.InitPriority = function() { }
js.d3._D3.InitPriority.__name__ = ["js","d3","_D3","InitPriority"];
var massive = {}
massive.haxe = {}
massive.haxe.Exception = function(message,info) {
	this.message = message;
	this.info = info;
	this.type = massive.haxe.util.ReflectUtil.here({ fileName : "Exception.hx", lineNumber : 70, className : "massive.haxe.Exception", methodName : "new"}).className;
};
massive.haxe.Exception.__name__ = ["massive","haxe","Exception"];
massive.haxe.Exception.prototype = {
	toString: function() {
		var str = this.type + ": " + this.message;
		if(this.info != null) str += " at " + this.info.className + "#" + this.info.methodName + " (" + this.info.lineNumber + ")";
		return str;
	}
	,__class__: massive.haxe.Exception
}
massive.haxe.util = {}
massive.haxe.util.ReflectUtil = function() { }
massive.haxe.util.ReflectUtil.__name__ = ["massive","haxe","util","ReflectUtil"];
massive.haxe.util.ReflectUtil.here = function(info) {
	return info;
}
massive.munit = {}
massive.munit.Assert = function() { }
massive.munit.Assert.__name__ = ["massive","munit","Assert"];
massive.munit.Assert.isTrue = function(value,info) {
	massive.munit.Assert.assertionCount++;
	if(value != true) massive.munit.Assert.fail("Expected TRUE but was [" + Std.string(value) + "]",info);
}
massive.munit.Assert.isFalse = function(value,info) {
	massive.munit.Assert.assertionCount++;
	if(value != false) massive.munit.Assert.fail("Expected FALSE but was [" + Std.string(value) + "]",info);
}
massive.munit.Assert.isNull = function(value,info) {
	massive.munit.Assert.assertionCount++;
	if(value != null) massive.munit.Assert.fail("Value [" + Std.string(value) + "] was not NULL",info);
}
massive.munit.Assert.isNotNull = function(value,info) {
	massive.munit.Assert.assertionCount++;
	if(value == null) massive.munit.Assert.fail("Value [" + Std.string(value) + "] was NULL",info);
}
massive.munit.Assert.isNaN = function(value,info) {
	massive.munit.Assert.assertionCount++;
	if(!Math.isNaN(value)) massive.munit.Assert.fail("Value [" + value + "]  was not NaN",info);
}
massive.munit.Assert.isNotNaN = function(value,info) {
	massive.munit.Assert.assertionCount++;
	if(Math.isNaN(value)) massive.munit.Assert.fail("Value [" + value + "] was NaN",info);
}
massive.munit.Assert.isType = function(value,type,info) {
	massive.munit.Assert.assertionCount++;
	if(!js.Boot.__instanceof(value,type)) massive.munit.Assert.fail("Value [" + Std.string(value) + "] was not of type: " + Type.getClassName(type),info);
}
massive.munit.Assert.isNotType = function(value,type,info) {
	massive.munit.Assert.assertionCount++;
	if(js.Boot.__instanceof(value,type)) massive.munit.Assert.fail("Value [" + Std.string(value) + "] was of type: " + Type.getClassName(type),info);
}
massive.munit.Assert.areEqual = function(expected,actual,info) {
	massive.munit.Assert.assertionCount++;
	var equal = (function($this) {
		var $r;
		var _g = Type["typeof"](expected);
		$r = (function($this) {
			var $r;
			switch( (_g)[1] ) {
			case 7:
				$r = Type.enumEq(expected,actual);
				break;
			default:
				$r = expected == actual;
			}
			return $r;
		}($this));
		return $r;
	}(this));
	if(!equal) massive.munit.Assert.fail("Value [" + Std.string(actual) + "] was not equal to expected value [" + Std.string(expected) + "]",info);
}
massive.munit.Assert.areNotEqual = function(expected,actual,info) {
	massive.munit.Assert.assertionCount++;
	var equal = (function($this) {
		var $r;
		var _g = Type["typeof"](expected);
		$r = (function($this) {
			var $r;
			switch( (_g)[1] ) {
			case 7:
				$r = Type.enumEq(expected,actual);
				break;
			default:
				$r = expected == actual;
			}
			return $r;
		}($this));
		return $r;
	}(this));
	if(equal) massive.munit.Assert.fail("Value [" + Std.string(actual) + "] was equal to value [" + Std.string(expected) + "]",info);
}
massive.munit.Assert.areSame = function(expected,actual,info) {
	massive.munit.Assert.assertionCount++;
	if(expected != actual) massive.munit.Assert.fail("Value [" + Std.string(actual) + "] was not the same as expected value [" + Std.string(expected) + "]",info);
}
massive.munit.Assert.areNotSame = function(expected,actual,info) {
	massive.munit.Assert.assertionCount++;
	if(expected == actual) massive.munit.Assert.fail("Value [" + Std.string(actual) + "] was the same as expected value [" + Std.string(expected) + "]",info);
}
massive.munit.Assert.fail = function(msg,info) {
	throw new massive.munit.AssertionException(msg,info);
}
massive.munit.MUnitException = function(message,info) {
	massive.haxe.Exception.call(this,message,info);
	this.type = massive.haxe.util.ReflectUtil.here({ fileName : "MUnitException.hx", lineNumber : 50, className : "massive.munit.MUnitException", methodName : "new"}).className;
};
massive.munit.MUnitException.__name__ = ["massive","munit","MUnitException"];
massive.munit.MUnitException.__super__ = massive.haxe.Exception;
massive.munit.MUnitException.prototype = $extend(massive.haxe.Exception.prototype,{
	__class__: massive.munit.MUnitException
});
massive.munit.AssertionException = function(msg,info) {
	massive.munit.MUnitException.call(this,msg,info);
	this.type = massive.haxe.util.ReflectUtil.here({ fileName : "AssertionException.hx", lineNumber : 49, className : "massive.munit.AssertionException", methodName : "new"}).className;
};
massive.munit.AssertionException.__name__ = ["massive","munit","AssertionException"];
massive.munit.AssertionException.__super__ = massive.munit.MUnitException;
massive.munit.AssertionException.prototype = $extend(massive.munit.MUnitException.prototype,{
	__class__: massive.munit.AssertionException
});
massive.munit.ITestResultClient = function() { }
massive.munit.ITestResultClient.__name__ = ["massive","munit","ITestResultClient"];
massive.munit.ITestResultClient.prototype = {
	__class__: massive.munit.ITestResultClient
}
massive.munit.IAdvancedTestResultClient = function() { }
massive.munit.IAdvancedTestResultClient.__name__ = ["massive","munit","IAdvancedTestResultClient"];
massive.munit.IAdvancedTestResultClient.__interfaces__ = [massive.munit.ITestResultClient];
massive.munit.IAdvancedTestResultClient.prototype = {
	__class__: massive.munit.IAdvancedTestResultClient
}
massive.munit.ICoverageTestResultClient = function() { }
massive.munit.ICoverageTestResultClient.__name__ = ["massive","munit","ICoverageTestResultClient"];
massive.munit.ICoverageTestResultClient.__interfaces__ = [massive.munit.IAdvancedTestResultClient];
massive.munit.ICoverageTestResultClient.prototype = {
	__class__: massive.munit.ICoverageTestResultClient
}
massive.munit.TestClassHelper = function(type,isDebug) {
	if(isDebug == null) isDebug = false;
	this.type = type;
	this.isDebug = isDebug;
	this.tests = [];
	this.index = 0;
	this.className = Type.getClassName(type);
	this.beforeClass = $bind(this,this.nullFunc);
	this.afterClass = $bind(this,this.nullFunc);
	this.before = $bind(this,this.nullFunc);
	this.after = $bind(this,this.nullFunc);
	this.parse(type);
};
massive.munit.TestClassHelper.__name__ = ["massive","munit","TestClassHelper"];
massive.munit.TestClassHelper.prototype = {
	nullFunc: function() {
	}
	,sortTestsByName: function(x,y) {
		if(x.result.name == y.result.name) return 0;
		if(x.result.name > y.result.name) return 1; else return -1;
	}
	,addTest: function(field,testFunction,testInstance,isAsync,isIgnored,description) {
		var result = new massive.munit.TestResult();
		result.async = isAsync;
		result.ignore = isIgnored;
		result.className = this.className;
		result.description = description;
		result.name = field;
		var data = { test : testFunction, scope : testInstance, result : result};
		this.tests.push(data);
	}
	,searchForMatchingTags: function(fieldName,func,funcMeta) {
		var _g = 0, _g1 = massive.munit.TestClassHelper.META_TAGS;
		while(_g < _g1.length) {
			var tag = _g1[_g];
			++_g;
			if(Reflect.hasField(funcMeta,tag)) {
				var args = Reflect.field(funcMeta,tag);
				var description = args != null?args[0]:"";
				var isAsync = args != null && description == "Async";
				var isIgnored = Reflect.hasField(funcMeta,"Ignore");
				if(isAsync) description = ""; else if(isIgnored) {
					args = Reflect.field(funcMeta,"Ignore");
					description = args != null?args[0]:"";
				}
				switch(tag) {
				case "BeforeClass":
					this.beforeClass = func;
					break;
				case "AfterClass":
					this.afterClass = func;
					break;
				case "Before":
					this.before = func;
					break;
				case "After":
					this.after = func;
					break;
				case "AsyncTest":
					if(!this.isDebug) this.addTest(fieldName,func,this.test,true,isIgnored,description);
					break;
				case "Test":
					if(!this.isDebug) this.addTest(fieldName,func,this.test,isAsync,isIgnored,description);
					break;
				case "TestDebug":
					if(this.isDebug) this.addTest(fieldName,func,this.test,isAsync,isIgnored,description);
					break;
				}
			}
		}
	}
	,scanForTests: function(fieldMeta) {
		var fieldNames = Reflect.fields(fieldMeta);
		var _g = 0;
		while(_g < fieldNames.length) {
			var fieldName = fieldNames[_g];
			++_g;
			var f = Reflect.field(this.test,fieldName);
			if(Reflect.isFunction(f)) {
				var funcMeta = Reflect.field(fieldMeta,fieldName);
				this.searchForMatchingTags(fieldName,f,funcMeta);
			}
		}
	}
	,collateFieldMeta: function(inherintanceChain) {
		var meta = { };
		while(inherintanceChain.length > 0) {
			var clazz = inherintanceChain.pop();
			var newMeta = haxe.rtti.Meta.getFields(clazz);
			var markedFieldNames = Reflect.fields(newMeta);
			var _g = 0;
			while(_g < markedFieldNames.length) {
				var fieldName = markedFieldNames[_g];
				++_g;
				var recordedFieldTags = Reflect.field(meta,fieldName);
				var newFieldTags = Reflect.field(newMeta,fieldName);
				var newTagNames = Reflect.fields(newFieldTags);
				if(recordedFieldTags == null) {
					var tagsCopy = { };
					var _g1 = 0;
					while(_g1 < newTagNames.length) {
						var tagName = newTagNames[_g1];
						++_g1;
						tagsCopy[tagName] = Reflect.field(newFieldTags,tagName);
					}
					meta[fieldName] = tagsCopy;
				} else {
					var ignored = false;
					var _g1 = 0;
					while(_g1 < newTagNames.length) {
						var tagName = newTagNames[_g1];
						++_g1;
						if(tagName == "Ignore") ignored = true;
						if(!ignored && (tagName == "Test" || tagName == "AsyncTest") && Reflect.hasField(recordedFieldTags,"Ignore")) Reflect.deleteField(recordedFieldTags,"Ignore");
						var tagValue = Reflect.field(newFieldTags,tagName);
						recordedFieldTags[tagName] = tagValue;
					}
				}
			}
		}
		return meta;
	}
	,getInheritanceChain: function(clazz) {
		var inherintanceChain = [clazz];
		while((clazz = Type.getSuperClass(clazz)) != null) inherintanceChain.push(clazz);
		return inherintanceChain;
	}
	,parse: function(type) {
		this.test = Type.createEmptyInstance(type);
		var inherintanceChain = this.getInheritanceChain(type);
		var fieldMeta = this.collateFieldMeta(inherintanceChain);
		this.scanForTests(fieldMeta);
		this.tests.sort($bind(this,this.sortTestsByName));
	}
	,current: function() {
		return this.index <= 0?this.tests[0]:this.tests[this.index - 1];
	}
	,next: function() {
		return this.hasNext()?this.tests[this.index++]:null;
	}
	,hasNext: function() {
		return this.index < this.tests.length;
	}
	,__class__: massive.munit.TestClassHelper
}
massive.munit.TestResult = function() {
	this.passed = false;
	this.executionTime = 0.0;
	this.name = "";
	this.className = "";
	this.description = "";
	this.async = false;
	this.ignore = false;
	this.error = null;
	this.failure = null;
};
massive.munit.TestResult.__name__ = ["massive","munit","TestResult"];
massive.munit.TestResult.prototype = {
	get_type: function() {
		if(this.error != null) return massive.munit.TestResultType.ERROR;
		if(this.failure != null) return massive.munit.TestResultType.FAIL;
		if(this.ignore == true) return massive.munit.TestResultType.IGNORE;
		if(this.passed == true) return massive.munit.TestResultType.PASS;
		return massive.munit.TestResultType.UNKNOWN;
	}
	,get_location: function() {
		return this.name == "" && this.className == ""?"":this.className + "#" + this.name;
	}
	,__class__: massive.munit.TestResult
}
massive.munit.TestResultType = { __ename__ : true, __constructs__ : ["UNKNOWN","PASS","FAIL","ERROR","IGNORE"] }
massive.munit.TestResultType.UNKNOWN = ["UNKNOWN",0];
massive.munit.TestResultType.UNKNOWN.toString = $estr;
massive.munit.TestResultType.UNKNOWN.__enum__ = massive.munit.TestResultType;
massive.munit.TestResultType.PASS = ["PASS",1];
massive.munit.TestResultType.PASS.toString = $estr;
massive.munit.TestResultType.PASS.__enum__ = massive.munit.TestResultType;
massive.munit.TestResultType.FAIL = ["FAIL",2];
massive.munit.TestResultType.FAIL.toString = $estr;
massive.munit.TestResultType.FAIL.__enum__ = massive.munit.TestResultType;
massive.munit.TestResultType.ERROR = ["ERROR",3];
massive.munit.TestResultType.ERROR.toString = $estr;
massive.munit.TestResultType.ERROR.__enum__ = massive.munit.TestResultType;
massive.munit.TestResultType.IGNORE = ["IGNORE",4];
massive.munit.TestResultType.IGNORE.toString = $estr;
massive.munit.TestResultType.IGNORE.__enum__ = massive.munit.TestResultType;
massive.munit.async = {}
massive.munit.async.IAsyncDelegateObserver = function() { }
massive.munit.async.IAsyncDelegateObserver.__name__ = ["massive","munit","async","IAsyncDelegateObserver"];
massive.munit.async.IAsyncDelegateObserver.prototype = {
	__class__: massive.munit.async.IAsyncDelegateObserver
}
massive.munit.TestRunner = function(resultClient) {
	this.clients = new Array();
	this.addResultClient(resultClient);
	this.set_asyncFactory(this.createAsyncFactory());
	this.running = false;
	this.isDebug = false;
};
massive.munit.TestRunner.__name__ = ["massive","munit","TestRunner"];
massive.munit.TestRunner.__interfaces__ = [massive.munit.async.IAsyncDelegateObserver];
massive.munit.TestRunner.prototype = {
	createAsyncFactory: function() {
		return new massive.munit.async.AsyncFactory(this);
	}
	,asyncDelegateCreatedHandler: function(delegate) {
		this.asyncDelegate = delegate;
	}
	,asyncTimeoutHandler: function(delegate) {
		var testCaseData = this.activeHelper.current();
		var result = testCaseData.result;
		result.executionTime = massive.munit.util.Timer.stamp() - this.testStartTime;
		result.error = new massive.munit.async.AsyncTimeoutException("",delegate.info);
		this.asyncPending = false;
		this.asyncDelegate = null;
		this.errorCount++;
		var _g = 0, _g1 = this.clients;
		while(_g < _g1.length) {
			var c = _g1[_g];
			++_g;
			c.addError(result);
		}
		this.activeHelper.after.apply(this.activeHelper.test,this.emptyParams);
		this.execute();
	}
	,asyncResponseHandler: function(delegate) {
		var testCaseData = this.activeHelper.current();
		testCaseData.test = $bind(delegate,delegate.runTest);
		testCaseData.scope = delegate;
		this.asyncPending = false;
		this.asyncDelegate = null;
		this.executeTestCase(testCaseData,false);
		this.activeHelper.after.apply(this.activeHelper.test,this.emptyParams);
		this.execute();
	}
	,clientCompletionHandler: function(resultClient) {
		if(++this.clientCompleteCount == this.clients.length) {
			if(this.completionHandler != null) {
				var successful = this.passCount == this.testCount;
				var handler = this.completionHandler;
				massive.munit.util.Timer.delay(function() {
					handler(successful);
				},10);
			}
			this.running = false;
		}
	}
	,executeTestCase: function(testCaseData,async) {
		var result = testCaseData.result;
		try {
			var assertionCount = massive.munit.Assert.assertionCount;
			if(async) {
				testCaseData.test.apply(testCaseData.scope,[this.asyncFactory]);
				if(this.asyncDelegate == null) throw new massive.munit.async.MissingAsyncDelegateException("No AsyncDelegate was created in async test at " + result.get_location(),null);
				this.asyncPending = true;
			} else {
				testCaseData.test.apply(testCaseData.scope,this.emptyParams);
				result.passed = true;
				result.executionTime = massive.munit.util.Timer.stamp() - this.testStartTime;
				this.passCount++;
				var _g = 0, _g1 = this.clients;
				while(_g < _g1.length) {
					var c = _g1[_g];
					++_g;
					c.addPass(result);
				}
			}
		} catch( e ) {
			if(async && this.asyncDelegate != null) {
				this.asyncDelegate.cancelTest();
				this.asyncDelegate = null;
			}
			if(js.Boot.__instanceof(e,org.hamcrest.AssertionException)) e = new massive.munit.AssertionException(e.message,e.info);
			if(js.Boot.__instanceof(e,massive.munit.AssertionException)) {
				result.executionTime = massive.munit.util.Timer.stamp() - this.testStartTime;
				result.failure = e;
				this.failCount++;
				var _g = 0, _g1 = this.clients;
				while(_g < _g1.length) {
					var c = _g1[_g];
					++_g;
					c.addFail(result);
				}
			} else {
				result.executionTime = massive.munit.util.Timer.stamp() - this.testStartTime;
				if(!js.Boot.__instanceof(e,massive.munit.MUnitException)) e = new massive.munit.UnhandledException(e,result.get_location());
				result.error = e;
				this.errorCount++;
				var _g = 0, _g1 = this.clients;
				while(_g < _g1.length) {
					var c = _g1[_g];
					++_g;
					c.addError(result);
				}
			}
		}
	}
	,executeTestCases: function() {
		var _g = 0, _g1 = this.clients;
		while(_g < _g1.length) {
			var c = _g1[_g];
			++_g;
			if(js.Boot.__instanceof(c,massive.munit.IAdvancedTestResultClient)) {
				if(this.activeHelper.hasNext()) {
					var cl = c;
					cl.setCurrentTestClass(this.activeHelper.className);
				}
			}
		}
		var $it0 = this.activeHelper;
		while( $it0.hasNext() ) {
			var testCaseData = $it0.next();
			if(testCaseData.result.ignore) {
				this.ignoreCount++;
				var _g = 0, _g1 = this.clients;
				while(_g < _g1.length) {
					var c = _g1[_g];
					++_g;
					c.addIgnore(testCaseData.result);
				}
			} else {
				this.testCount++;
				this.activeHelper.before.apply(this.activeHelper.test,this.emptyParams);
				this.testStartTime = massive.munit.util.Timer.stamp();
				this.executeTestCase(testCaseData,testCaseData.result.async);
				if(!this.asyncPending) this.activeHelper.after.apply(this.activeHelper.test,this.emptyParams); else break;
			}
		}
	}
	,execute: function() {
		var _g1 = this.suiteIndex, _g = this.testSuites.length;
		while(_g1 < _g) {
			var i = _g1++;
			var suite = this.testSuites[i];
			while( suite.hasNext() ) {
				var testClass = suite.next();
				if(this.activeHelper == null || this.activeHelper.type != testClass) {
					this.activeHelper = new massive.munit.TestClassHelper(testClass,this.isDebug);
					this.activeHelper.beforeClass.apply(this.activeHelper.test,this.emptyParams);
				}
				this.executeTestCases();
				if(!this.asyncPending) this.activeHelper.afterClass.apply(this.activeHelper.test,this.emptyParams); else {
					suite.repeat();
					this.suiteIndex = i;
					return;
				}
			}
		}
		if(!this.asyncPending) {
			var time = massive.munit.util.Timer.stamp() - this.startTime;
			var _g = 0, _g1 = this.clients;
			while(_g < _g1.length) {
				var client = _g1[_g];
				++_g;
				if(js.Boot.__instanceof(client,massive.munit.IAdvancedTestResultClient)) {
					var cl = client;
					cl.setCurrentTestClass(null);
				}
				client.reportFinalStatistics(this.testCount,this.passCount,this.failCount,this.errorCount,this.ignoreCount,time);
			}
		}
	}
	,run: function(testSuiteClasses) {
		if(this.running) return;
		this.running = true;
		this.asyncPending = false;
		this.asyncDelegate = null;
		this.testCount = 0;
		this.failCount = 0;
		this.errorCount = 0;
		this.passCount = 0;
		this.ignoreCount = 0;
		this.suiteIndex = 0;
		this.clientCompleteCount = 0;
		massive.munit.Assert.assertionCount = 0;
		this.emptyParams = new Array();
		this.testSuites = new Array();
		this.startTime = massive.munit.util.Timer.stamp();
		var _g = 0;
		while(_g < testSuiteClasses.length) {
			var suiteType = testSuiteClasses[_g];
			++_g;
			this.testSuites.push(Type.createInstance(suiteType,new Array()));
		}
		this.execute();
	}
	,debug: function(testSuiteClasses) {
		this.isDebug = true;
		this.run(testSuiteClasses);
	}
	,addResultClient: function(resultClient) {
		var _g = 0, _g1 = this.clients;
		while(_g < _g1.length) {
			var client = _g1[_g];
			++_g;
			if(client == resultClient) return;
		}
		resultClient.set_completionHandler($bind(this,this.clientCompletionHandler));
		this.clients.push(resultClient);
	}
	,set_asyncFactory: function(value) {
		if(value == this.asyncFactory) return value;
		if(this.running) throw new massive.munit.MUnitException("Can't change AsyncFactory while tests are running",{ fileName : "TestRunner.hx", lineNumber : 127, className : "massive.munit.TestRunner", methodName : "set_asyncFactory"});
		value.observer = this;
		return this.asyncFactory = value;
	}
	,get_clientCount: function() {
		return this.clients.length;
	}
	,__class__: massive.munit.TestRunner
}
massive.munit.TestSuite = function() {
	this.tests = new Array();
	this.index = 0;
};
massive.munit.TestSuite.__name__ = ["massive","munit","TestSuite"];
massive.munit.TestSuite.prototype = {
	sortByName: function(x,y) {
		var xName = Type.getClassName(x);
		var yName = Type.getClassName(y);
		if(xName == yName) return 0;
		if(xName > yName) return 1; else return -1;
	}
	,sortTests: function() {
		this.tests.sort($bind(this,this.sortByName));
	}
	,repeat: function() {
		if(this.index > 0) this.index--;
	}
	,next: function() {
		return this.hasNext()?this.tests[this.index++]:null;
	}
	,hasNext: function() {
		return this.index < this.tests.length;
	}
	,add: function(test) {
		this.tests.push(test);
		this.sortTests();
	}
	,__class__: massive.munit.TestSuite
}
massive.munit.UnhandledException = function(source,testLocation) {
	massive.munit.MUnitException.call(this,Std.string(source.toString()) + this.formatLocation(source,testLocation),null);
	this.type = massive.haxe.util.ReflectUtil.here({ fileName : "UnhandledException.hx", lineNumber : 53, className : "massive.munit.UnhandledException", methodName : "new"}).className;
};
massive.munit.UnhandledException.__name__ = ["massive","munit","UnhandledException"];
massive.munit.UnhandledException.__super__ = massive.munit.MUnitException;
massive.munit.UnhandledException.prototype = $extend(massive.munit.MUnitException.prototype,{
	getStackTrace: function(source) {
		var s = "";
		if(s == "") {
			var stack = haxe.CallStack.exceptionStack();
			while(stack.length > 0) {
				var _g = stack.shift();
				var $e = (_g);
				switch( $e[1] ) {
				case 2:
					var line = $e[4], file = $e[3], _g_eFilePos_0 = $e[2];
					s += "\tat " + file + " (" + line + ")\n";
					break;
				case 3:
					var method = $e[3], classname = $e[2];
					s += "\tat " + classname + "#" + method + "\n";
					break;
				default:
				}
			}
		}
		return s;
	}
	,formatLocation: function(source,testLocation) {
		var stackTrace = " at " + testLocation;
		var stack = this.getStackTrace(source);
		if(stack != "") stackTrace += " " + HxOverrides.substr(stack,1,null);
		return stackTrace;
	}
	,__class__: massive.munit.UnhandledException
});
massive.munit.async.AsyncDelegate = function(testCase,handler,timeout,info) {
	var self = this;
	this.testCase = testCase;
	this.handler = handler;
	this.delegateHandler = Reflect.makeVarArgs($bind(this,this.responseHandler));
	this.info = info;
	this.params = [];
	this.timedOut = false;
	this.canceled = false;
	if(timeout == null || timeout <= 0) timeout = 400;
	this.timeoutDelay = timeout;
	this.timer = massive.munit.util.Timer.delay($bind(this,this.timeoutHandler),this.timeoutDelay);
};
massive.munit.async.AsyncDelegate.__name__ = ["massive","munit","async","AsyncDelegate"];
massive.munit.async.AsyncDelegate.prototype = {
	actualTimeoutHandler: function() {
		this.deferredTimer = null;
		this.handler = null;
		this.delegateHandler = null;
		this.timedOut = true;
		if(this.observer != null) {
			this.observer.asyncTimeoutHandler(this);
			this.observer = null;
		}
	}
	,timeoutHandler: function() {
		this.actualTimeoutHandler();
	}
	,delayActualResponseHandler: function() {
		this.observer.asyncResponseHandler(this);
		this.observer = null;
	}
	,responseHandler: function(params) {
		if(this.timedOut || this.canceled) return null;
		this.timer.stop();
		if(this.deferredTimer != null) this.deferredTimer.stop();
		if(params == null) params = [];
		this.params = params;
		if(this.observer != null) massive.munit.util.Timer.delay($bind(this,this.delayActualResponseHandler),1);
		return null;
	}
	,cancelTest: function() {
		this.canceled = true;
		this.timer.stop();
		if(this.deferredTimer != null) this.deferredTimer.stop();
	}
	,runTest: function() {
		this.handler.apply(this.testCase,this.params);
	}
	,__class__: massive.munit.async.AsyncDelegate
}
massive.munit.async.AsyncFactory = function(observer) {
	this.observer = observer;
	this.asyncDelegateCount = 0;
};
massive.munit.async.AsyncFactory.__name__ = ["massive","munit","async","AsyncFactory"];
massive.munit.async.AsyncFactory.prototype = {
	createHandler: function(testCase,handler,timeout,info) {
		var delegate = new massive.munit.async.AsyncDelegate(testCase,handler,timeout,info);
		delegate.observer = this.observer;
		this.asyncDelegateCount++;
		this.observer.asyncDelegateCreatedHandler(delegate);
		return delegate.delegateHandler;
	}
	,__class__: massive.munit.async.AsyncFactory
}
massive.munit.async.AsyncTimeoutException = function(message,info) {
	massive.munit.MUnitException.call(this,message,info);
	this.type = massive.haxe.util.ReflectUtil.here({ fileName : "AsyncTimeoutException.hx", lineNumber : 47, className : "massive.munit.async.AsyncTimeoutException", methodName : "new"}).className;
};
massive.munit.async.AsyncTimeoutException.__name__ = ["massive","munit","async","AsyncTimeoutException"];
massive.munit.async.AsyncTimeoutException.__super__ = massive.munit.MUnitException;
massive.munit.async.AsyncTimeoutException.prototype = $extend(massive.munit.MUnitException.prototype,{
	__class__: massive.munit.async.AsyncTimeoutException
});
massive.munit.async.MissingAsyncDelegateException = function(message,info) {
	massive.munit.MUnitException.call(this,message,info);
	this.type = massive.haxe.util.ReflectUtil.here({ fileName : "MissingAsyncDelegateException.hx", lineNumber : 47, className : "massive.munit.async.MissingAsyncDelegateException", methodName : "new"}).className;
};
massive.munit.async.MissingAsyncDelegateException.__name__ = ["massive","munit","async","MissingAsyncDelegateException"];
massive.munit.async.MissingAsyncDelegateException.__super__ = massive.munit.MUnitException;
massive.munit.async.MissingAsyncDelegateException.prototype = $extend(massive.munit.MUnitException.prototype,{
	__class__: massive.munit.async.MissingAsyncDelegateException
});
massive.munit.util = {}
massive.munit.util.Timer = function(time_ms) {
	this.id = massive.munit.util.Timer.arr.length;
	massive.munit.util.Timer.arr[this.id] = this;
	this.timerId = window.setInterval("massive.munit.util.Timer.arr[" + this.id + "].run();",time_ms);
};
$hxExpose(massive.munit.util.Timer, "massive.munit.util.Timer");
massive.munit.util.Timer.__name__ = ["massive","munit","util","Timer"];
massive.munit.util.Timer.delay = function(f,time_ms) {
	var t = new massive.munit.util.Timer(time_ms);
	t.run = function() {
		t.stop();
		f();
	};
	return t;
}
massive.munit.util.Timer.stamp = function() {
	return new Date().getTime() / 1000;
}
massive.munit.util.Timer.prototype = {
	run: function() {
	}
	,stop: function() {
		if(this.id == null) return;
		window.clearInterval(this.timerId);
		massive.munit.util.Timer.arr[this.id] = null;
		if(this.id > 100 && this.id == massive.munit.util.Timer.arr.length - 1) {
			var p = this.id - 1;
			while(p >= 0 && massive.munit.util.Timer.arr[p] == null) p--;
			massive.munit.util.Timer.arr = massive.munit.util.Timer.arr.slice(0,p + 1);
		}
		this.id = null;
	}
	,__class__: massive.munit.util.Timer
}
var model = {}
model.CCAR = function(name,text,cr) {
	try {
		console.log("Creating ccar instance");
		this.scenarioName = name;
		this.scenarioText = text;
		this.creator = cr;
		this.deleted = false;
	} catch( err ) {
		console.log("Exception creating ccar " + Std.string(err));
	}
	console.log("Created ccar instance successfully");
};
model.CCAR.__name__ = ["model","CCAR"];
model.CCAR.prototype = {
	getParsedScenarioElement: function() {
		return js.Browser.document.getElementById(model.CCAR.PARSED_SCENARIO);
	}
	,setParsedScenario: function(incomingM) {
		this.getParsedScenarioElement().value = incomingM;
	}
	,processParsedCCARText: function(incomingMessage) {
		console.log("Processing parsed text");
		this.setParsedScenario(haxe.Json.stringify(incomingMessage));
	}
	,processCCARUpload: function(incomingMessage) {
		console.log("Processing ccar upload");
		var ccarStruct = incomingMessage.ccarData;
		var crudType = incomingMessage.ccarOperation.tag;
		this.copyIncomingValues(incomingMessage);
		if(crudType == "Create") {
			console.log("Create successful");
			this.copyIncomingValues(incomingMessage);
			this.sendParsingRequest();
		} else if(crudType == "Update") {
			console.log("Update successful");
			this.copyIncomingValues(incomingMessage);
			this.sendParsingRequest();
		} else if(crudType == "Query") {
			console.log("Read returned " + Std.string(incomingMessage));
			this.copyIncomingValues(incomingMessage);
			if(ccarStruct.ccarResultSet == []) crudType = "Create"; else crudType = "Update";
			this.sendParsingRequest();
		}
	}
	,sendParsingRequest: function() {
		var payload = { nickName : MBooks_im.getSingleton().getNickName(), uploadedBy : MBooks_im.getSingleton().getNickName(), scenarioName : this.getScenarioName(), ccarText : this.getScenarioText(), commandType : "ParsedCCARText"};
		console.log("Sending parsing request " + Std.string(payload));
		MBooks_im.getSingleton().doSendJSON(payload);
	}
	,sendPT: function(ev) {
		console.log("Processing event " + Std.string(ev));
		this.sendParsingRequest();
	}
	,checkScenarioExists: function(ev) {
		console.log("Checking if scenario exists ");
		if(util.Util.isSignificantWS(ev.keyCode)) {
			this.copyValuesFromUI();
			var payload = { nickName : MBooks_im.getSingleton().getNickName(), uploadedBy : MBooks_im.getSingleton().getNickName(), commandType : "CCARUpload", ccarOperation : { tag : "Query", contents : []}, ccarData : { scenarioName : this.scenarioName, scenarioText : this.scenarioText, creator : MBooks_im.getSingleton().getNickName(), deleted : false}};
			MBooks_im.getSingleton().doSendJSON(payload);
		}
	}
	,saveScenario: function(ev) {
		console.log("Saving scenario ");
		this.copyValuesFromUI();
		var payload = { nickName : MBooks_im.getSingleton().getNickName(), uploadedBy : MBooks_im.getSingleton().getNickName(), commandType : "CCARUpload", ccarOperation : { tag : this.crudType, contents : []}, ccarData : { scenarioName : this.scenarioName, scenarioText : this.scenarioText, creator : MBooks_im.getSingleton().getNickName(), deleted : false}};
		MBooks_im.getSingleton().doSendJSON(payload);
	}
	,copyValuesFromUI: function() {
		this.scenarioName = this.getScenarioName();
		this.scenarioText = this.getScenarioText();
		this.creator = MBooks_im.getSingleton().getNickName();
		this.deleted = false;
	}
	,copyIncomingValues: function(aMessage) {
		this.scenarioName = aMessage.ccarData.scenarioName;
		this.scenarioText = aMessage.ccarData.scenarioText;
		this.creator = aMessage.ccarData.createdBy;
		this.deleted = aMessage.ccarData.deleted;
	}
	,setupStreams: function() {
		var saveStream = MBooks_im.getSingleton().initializeElementStream(this.getSaveScenarioElement(),"click");
		saveStream.then($bind(this,this.sendPT));
	}
	,getSaveScenarioElement: function() {
		return js.Browser.document.getElementById(model.CCAR.SAVE_SCENARIO);
	}
	,getScenarioTextElement: function() {
		return js.Browser.document.getElementById(model.CCAR.SCENARIO_TEXT);
	}
	,getScenarioText: function() {
		return this.getScenarioTextElement().value;
	}
	,getScenarioNameElement: function() {
		return js.Browser.document.getElementById(model.CCAR.SCENARIO_NAME);
	}
	,getScenarioName: function() {
		if(this.getScenarioNameElement() != null) return this.getScenarioNameElement().value; else {
			console.log("Element not defined ");
			return "TBD";
		}
	}
	,__class__: model.CCAR
}
model.Command = function(nickName,aCType,payload) {
	this.commandType = aCType;
	this.payload = payload;
};
model.Command.__name__ = ["model","Command"];
model.Command.prototype = {
	__class__: model.Command
}
model.CommandType = { __ename__ : true, __constructs__ : ["Login","SendMessage","ManageUser","CreateUserTerms","UpdateUserTerms","QueryUserTerms","DeleteUserTerms","CreateUserPreferences","UpdateUserPreferences","QueryUserPreferences","DeleteUserPreferences","CCARUpload","ParsedCCARText","Undefined","UserJoined","UserLeft","UserLoggedIn","UserBanned","ManageCompany","KeepAlive","SelectAllCompanies","SelectActiveProjects","ManageProject","QuerySupportedScripts","QueryActiveWorkbenches","ManageWorkbench","ExecuteWorkbench","AssignCompany","PortfolioSymbolTypesQuery","PortfolioSymbolSidesQuery","QueryPortfolios","ManagePortfolio","ManagePortfolioSymbol","MarketDataUpdate","QueryPortfolioSymbol","ManageEntitlements","QueryEntitlements","QueryCompanyUsers"] }
model.CommandType.Login = ["Login",0];
model.CommandType.Login.toString = $estr;
model.CommandType.Login.__enum__ = model.CommandType;
model.CommandType.SendMessage = ["SendMessage",1];
model.CommandType.SendMessage.toString = $estr;
model.CommandType.SendMessage.__enum__ = model.CommandType;
model.CommandType.ManageUser = ["ManageUser",2];
model.CommandType.ManageUser.toString = $estr;
model.CommandType.ManageUser.__enum__ = model.CommandType;
model.CommandType.CreateUserTerms = ["CreateUserTerms",3];
model.CommandType.CreateUserTerms.toString = $estr;
model.CommandType.CreateUserTerms.__enum__ = model.CommandType;
model.CommandType.UpdateUserTerms = ["UpdateUserTerms",4];
model.CommandType.UpdateUserTerms.toString = $estr;
model.CommandType.UpdateUserTerms.__enum__ = model.CommandType;
model.CommandType.QueryUserTerms = ["QueryUserTerms",5];
model.CommandType.QueryUserTerms.toString = $estr;
model.CommandType.QueryUserTerms.__enum__ = model.CommandType;
model.CommandType.DeleteUserTerms = ["DeleteUserTerms",6];
model.CommandType.DeleteUserTerms.toString = $estr;
model.CommandType.DeleteUserTerms.__enum__ = model.CommandType;
model.CommandType.CreateUserPreferences = ["CreateUserPreferences",7];
model.CommandType.CreateUserPreferences.toString = $estr;
model.CommandType.CreateUserPreferences.__enum__ = model.CommandType;
model.CommandType.UpdateUserPreferences = ["UpdateUserPreferences",8];
model.CommandType.UpdateUserPreferences.toString = $estr;
model.CommandType.UpdateUserPreferences.__enum__ = model.CommandType;
model.CommandType.QueryUserPreferences = ["QueryUserPreferences",9];
model.CommandType.QueryUserPreferences.toString = $estr;
model.CommandType.QueryUserPreferences.__enum__ = model.CommandType;
model.CommandType.DeleteUserPreferences = ["DeleteUserPreferences",10];
model.CommandType.DeleteUserPreferences.toString = $estr;
model.CommandType.DeleteUserPreferences.__enum__ = model.CommandType;
model.CommandType.CCARUpload = ["CCARUpload",11];
model.CommandType.CCARUpload.toString = $estr;
model.CommandType.CCARUpload.__enum__ = model.CommandType;
model.CommandType.ParsedCCARText = ["ParsedCCARText",12];
model.CommandType.ParsedCCARText.toString = $estr;
model.CommandType.ParsedCCARText.__enum__ = model.CommandType;
model.CommandType.Undefined = ["Undefined",13];
model.CommandType.Undefined.toString = $estr;
model.CommandType.Undefined.__enum__ = model.CommandType;
model.CommandType.UserJoined = ["UserJoined",14];
model.CommandType.UserJoined.toString = $estr;
model.CommandType.UserJoined.__enum__ = model.CommandType;
model.CommandType.UserLeft = ["UserLeft",15];
model.CommandType.UserLeft.toString = $estr;
model.CommandType.UserLeft.__enum__ = model.CommandType;
model.CommandType.UserLoggedIn = ["UserLoggedIn",16];
model.CommandType.UserLoggedIn.toString = $estr;
model.CommandType.UserLoggedIn.__enum__ = model.CommandType;
model.CommandType.UserBanned = ["UserBanned",17];
model.CommandType.UserBanned.toString = $estr;
model.CommandType.UserBanned.__enum__ = model.CommandType;
model.CommandType.ManageCompany = ["ManageCompany",18];
model.CommandType.ManageCompany.toString = $estr;
model.CommandType.ManageCompany.__enum__ = model.CommandType;
model.CommandType.KeepAlive = ["KeepAlive",19];
model.CommandType.KeepAlive.toString = $estr;
model.CommandType.KeepAlive.__enum__ = model.CommandType;
model.CommandType.SelectAllCompanies = ["SelectAllCompanies",20];
model.CommandType.SelectAllCompanies.toString = $estr;
model.CommandType.SelectAllCompanies.__enum__ = model.CommandType;
model.CommandType.SelectActiveProjects = ["SelectActiveProjects",21];
model.CommandType.SelectActiveProjects.toString = $estr;
model.CommandType.SelectActiveProjects.__enum__ = model.CommandType;
model.CommandType.ManageProject = ["ManageProject",22];
model.CommandType.ManageProject.toString = $estr;
model.CommandType.ManageProject.__enum__ = model.CommandType;
model.CommandType.QuerySupportedScripts = ["QuerySupportedScripts",23];
model.CommandType.QuerySupportedScripts.toString = $estr;
model.CommandType.QuerySupportedScripts.__enum__ = model.CommandType;
model.CommandType.QueryActiveWorkbenches = ["QueryActiveWorkbenches",24];
model.CommandType.QueryActiveWorkbenches.toString = $estr;
model.CommandType.QueryActiveWorkbenches.__enum__ = model.CommandType;
model.CommandType.ManageWorkbench = ["ManageWorkbench",25];
model.CommandType.ManageWorkbench.toString = $estr;
model.CommandType.ManageWorkbench.__enum__ = model.CommandType;
model.CommandType.ExecuteWorkbench = ["ExecuteWorkbench",26];
model.CommandType.ExecuteWorkbench.toString = $estr;
model.CommandType.ExecuteWorkbench.__enum__ = model.CommandType;
model.CommandType.AssignCompany = ["AssignCompany",27];
model.CommandType.AssignCompany.toString = $estr;
model.CommandType.AssignCompany.__enum__ = model.CommandType;
model.CommandType.PortfolioSymbolTypesQuery = ["PortfolioSymbolTypesQuery",28];
model.CommandType.PortfolioSymbolTypesQuery.toString = $estr;
model.CommandType.PortfolioSymbolTypesQuery.__enum__ = model.CommandType;
model.CommandType.PortfolioSymbolSidesQuery = ["PortfolioSymbolSidesQuery",29];
model.CommandType.PortfolioSymbolSidesQuery.toString = $estr;
model.CommandType.PortfolioSymbolSidesQuery.__enum__ = model.CommandType;
model.CommandType.QueryPortfolios = ["QueryPortfolios",30];
model.CommandType.QueryPortfolios.toString = $estr;
model.CommandType.QueryPortfolios.__enum__ = model.CommandType;
model.CommandType.ManagePortfolio = ["ManagePortfolio",31];
model.CommandType.ManagePortfolio.toString = $estr;
model.CommandType.ManagePortfolio.__enum__ = model.CommandType;
model.CommandType.ManagePortfolioSymbol = ["ManagePortfolioSymbol",32];
model.CommandType.ManagePortfolioSymbol.toString = $estr;
model.CommandType.ManagePortfolioSymbol.__enum__ = model.CommandType;
model.CommandType.MarketDataUpdate = ["MarketDataUpdate",33];
model.CommandType.MarketDataUpdate.toString = $estr;
model.CommandType.MarketDataUpdate.__enum__ = model.CommandType;
model.CommandType.QueryPortfolioSymbol = ["QueryPortfolioSymbol",34];
model.CommandType.QueryPortfolioSymbol.toString = $estr;
model.CommandType.QueryPortfolioSymbol.__enum__ = model.CommandType;
model.CommandType.ManageEntitlements = ["ManageEntitlements",35];
model.CommandType.ManageEntitlements.toString = $estr;
model.CommandType.ManageEntitlements.__enum__ = model.CommandType;
model.CommandType.QueryEntitlements = ["QueryEntitlements",36];
model.CommandType.QueryEntitlements.toString = $estr;
model.CommandType.QueryEntitlements.__enum__ = model.CommandType;
model.CommandType.QueryCompanyUsers = ["QueryCompanyUsers",37];
model.CommandType.QueryCompanyUsers.toString = $estr;
model.CommandType.QueryCompanyUsers.__enum__ = model.CommandType;
model.Company = function(n,cId,gM,ima) {
	this.name = n;
	this.companyId = cId;
	this.generalMailbox = gM;
	this.image = ima;
};
model.Company.__name__ = ["model","Company"];
model.Company.prototype = {
	__class__: model.Company
}
model.CompanyEntitlement = function(stream) {
	util.Util.log("Creating new entitlement");
	stream.then($bind(this,this.updateModel));
};
model.CompanyEntitlement.__name__ = ["model","CompanyEntitlement"];
model.CompanyEntitlement.addUserEntitlement = function(userNickName,entitlementId) {
	console.log("Adding user entitlement for " + userNickName + " -> " + entitlementId);
}
model.CompanyEntitlement.prototype = {
	queryAllEntitlements: function() {
		console.log("Query all the entitlements");
		var queryEntitlements = { nickName : MBooks_im.getSingleton().getNickName(), queryParameters : "*", commandType : "QueryCompanyEntitlements", resultSet : new Array()};
		MBooks_im.getSingleton().doSendJSON(queryEntitlements);
	}
	,updateModel: function(anEntitlement) {
		console.log("Updating model " + Std.string(anEntitlement));
		MBooks_im.getSingleton().doSendJSON(anEntitlement);
	}
	,__class__: model.CompanyEntitlement
}
model.Contact = function(aName,lName,aLogin) {
	this.firstName = aName;
	this.lastName = lName;
	this.login = aLogin;
};
model.Contact.__name__ = ["model","Contact"];
model.Contact.prototype = {
	__class__: model.Contact
}
model.Entitlement = function(stream) {
	util.Util.log("Creating new entitlement");
	stream.then($bind(this,this.updateModel));
};
model.Entitlement.__name__ = ["model","Entitlement"];
model.Entitlement.listDisplay = function(entT) {
	return entT.tabName + "->" + entT.sectionName;
}
model.Entitlement.optionId = function(entT) {
	return entT.tabName + entT.sectionName;
}
model.Entitlement.prototype = {
	queryAllEntitlements: function() {
		console.log("Query all the entitlements");
		var queryEntitlements = { nickName : MBooks_im.getSingleton().getNickName(), queryParameters : "*", commandType : "QueryEntitlements", resultSet : new Array()};
		MBooks_im.getSingleton().doSendJSON(queryEntitlements);
	}
	,updateModel: function(anEntitlement) {
		console.log("Updating model " + Std.string(anEntitlement));
		MBooks_im.getSingleton().doSendJSON(anEntitlement);
	}
	,__class__: model.Entitlement
}
model.Login = function(commandType,p,s) {
	this.commandType = commandType;
	this.login = p;
	this.nickName = p.nickName;
	this.loginStatus = Std.string(s);
};
model.Login.__name__ = ["model","Login"];
model.Login.createLoginResponse = function(incomingMessage,person) {
	if(incomingMessage.Right == null) throw "Invalid login " + Std.string(incomingMessage);
	var commandType = "" + Std.string(MBooks_im.getSingleton().parseCommandType(incomingMessage));
	var loginStatus = Type.createEnum(model.LoginStatus,incomingMessage.Right.loginStatus);
	var result = new model.Login(commandType,person,loginStatus);
	return result;
}
model.Login.prototype = {
	__class__: model.Login
}
model.LoginStatus = { __ename__ : true, __constructs__ : ["UserExists","UserNotFound","InvalidPassword","Undefined","Guest"] }
model.LoginStatus.UserExists = ["UserExists",0];
model.LoginStatus.UserExists.toString = $estr;
model.LoginStatus.UserExists.__enum__ = model.LoginStatus;
model.LoginStatus.UserNotFound = ["UserNotFound",1];
model.LoginStatus.UserNotFound.toString = $estr;
model.LoginStatus.UserNotFound.__enum__ = model.LoginStatus;
model.LoginStatus.InvalidPassword = ["InvalidPassword",2];
model.LoginStatus.InvalidPassword.toString = $estr;
model.LoginStatus.InvalidPassword.__enum__ = model.LoginStatus;
model.LoginStatus.Undefined = ["Undefined",3];
model.LoginStatus.Undefined.toString = $estr;
model.LoginStatus.Undefined.__enum__ = model.LoginStatus;
model.LoginStatus.Guest = ["Guest",4];
model.LoginStatus.Guest.toString = $estr;
model.LoginStatus.Guest.__enum__ = model.LoginStatus;
model.Person = function(fName,lName,nName,pwd) {
	this.firstName = fName;
	this.lastName = lName;
	this.nickName = nName;
	this.password = pwd;
	this.lastLoginTime = new Date();
	this.deleted = false;
};
model.Person.__name__ = ["model","Person"];
model.Person.listDisplay = function(person) {
	return person.firstName + " " + person.lastName;
}
model.Person.optionId = function(person) {
	return person.nickName;
}
model.Person.prototype = {
	setPassword: function(n) {
		this.password = n;
	}
	,setLastName: function(n) {
		this.lastName = n;
	}
	,setFirstName: function(n) {
		this.firstName = n;
	}
	,setNickName: function(n) {
		this.nickName = n;
	}
	,__class__: model.Person
}
model.Portfolio = function(crudType,portfolioId,companyId,userId,summary,createdBy,updatedBy) {
	this.portfolioT = { crudType : crudType, commandType : "ManagePortfolio", portfolioId : portfolioId, companyId : companyId, userId : userId, summary : summary, createdBy : createdBy, updatedBy : updatedBy, nickName : MBooks_im.getSingleton().getNickName()};
};
model.Portfolio.__name__ = ["model","Portfolio"];
model.Portfolio.prototype = {
	save: function(portfolio) {
		console.log("Saving portfolio");
	}
	,__class__: model.Portfolio
}
model.PortfolioSymbol = function() {
	console.log("Creating portfolio symbol");
	this.sidesStream = new promhx.Deferred();
	this.typesStream = new promhx.Deferred();
	this.typeStream = new promhx.Deferred();
	this.sideStream = new promhx.Deferred();
	this.insertStream = new promhx.Deferred();
	this.updateStream = new promhx.Deferred();
	this.deleteStream = new promhx.Deferred();
	this.readStream = new promhx.Deferred();
	this.sendPortfolioSymbolSideQuery();
	this.sendPortfolioSymbolTypeQuery();
	this.sidesStream.then($bind(this,this.handleSymbolSideQuery));
	this.typesStream.then($bind(this,this.handleSymbolTypeQuery));
	this.insertStream.then($bind(this,this.sendPayload));
	this.updateStream.then($bind(this,this.sendPayload));
	this.deleteStream.then($bind(this,this.sendPayload));
	this.readStream.then($bind(this,this.sendPayload));
	MBooks_im.getSingleton().portfolio.activePortfolioStream.then($bind(this,this.processActivePortfolio));
};
model.PortfolioSymbol.__name__ = ["model","PortfolioSymbol"];
model.PortfolioSymbol.prototype = {
	handleSymbolTypeQuery: function(incomingMessage) {
		console.log("Handle portfolio symbol type query " + Std.string(incomingMessage));
		var resultSet = incomingMessage.symbolTypes;
		var _g = 0;
		while(_g < resultSet.length) {
			var optionSymbolType = resultSet[_g];
			++_g;
			console.log("Resolving " + Std.string(optionSymbolType));
			var p = { symbolType : optionSymbolType};
			this.typeStream.resolve(p);
		}
	}
	,handleSymbolSideQuery: function(incomingMessage) {
		console.log("Handle portfolio symbol side query" + Std.string(incomingMessage));
		var resultSet = incomingMessage.symbolSides;
		var _g = 0;
		while(_g < resultSet.length) {
			var optionSymbolSide = resultSet[_g];
			++_g;
			var payload = { symbolSide : optionSymbolSide};
			this.sideStream.resolve(payload);
		}
	}
	,sendPortfolioSymbolTypeQuery: function() {
		var payload = { nickName : MBooks_im.getSingleton().getNickName(), commandType : "PortfolioSymbolTypesQuery", symbolTypes : []};
		MBooks_im.getSingleton().doSendJSON(payload);
	}
	,sendPortfolioSymbolSideQuery: function() {
		var payload = { nickName : MBooks_im.getSingleton().getNickName(), commandType : "PortfolioSymbolSidesQuery", symbolSides : []};
		MBooks_im.getSingleton().doSendJSON(payload);
	}
	,sendPayload: function(payload) {
		console.log("Processing sending payload " + Std.string(payload));
		MBooks_im.getSingleton().doSendJSON(payload);
	}
	,sendPortfolioSymbolQuery: function() {
		if(this.activePortfolio == null) throw "No active portfolio selected. Not fetching symbols";
		var payload = { commandType : "QueryPortfolioSymbol", portfolioId : this.activePortfolio.portfolioId, nickName : MBooks_im.getSingleton().getNickName(), resultSet : []};
		MBooks_im.getSingleton().doSendJSON(payload);
	}
	,processActivePortfolio: function(a) {
		if(a == null) throw "Active portfolio not defined " + Std.string(a);
		console.log("Process active portfolio " + Std.string(a));
		this.activePortfolio = a;
		this.sendPortfolioSymbolQuery();
	}
	,__class__: model.PortfolioSymbol
}
model.Project = function(companyI) {
	try {
		console.log("Instantiating Project");
		this.newProject = true;
		var stream = MBooks_im.getSingleton().initializeElementStream(this.getSaveProject(),"click");
		stream.then($bind(this,this.saveProject));
		this.company = companyI;
		this.company.getSelectListEventStream().then($bind(this,this.processCompanyList));
		this.projectStream = new promhx.Deferred();
		this.projectStream.then($bind(this,this.processProjectList));
	} catch( err ) {
		console.log("Error creating project " + Std.string(err));
	}
};
model.Project.__name__ = ["model","Project"];
model.Project.prototype = {
	getPayloadD: function(nickName,crudType) {
		return this.getPayload(nickName,crudType,this.getProjectID());
	}
	,getPayload: function(nickName,crudType,projectId) {
		var payload = { nickName : nickName, commandType : model.Project.MANAGE_PROJECT, crudType : crudType, projectId : projectId, uniqueCompanyID : this.company.getCompanyID(), summary : this.getProjectSummary(), details : this.getProjectDetails(), startDate : [new Date()], endDate : [new Date()], uploadTime : [new Date()], preparedBy : this.getPreparedBy(), uploadedBy : nickName};
		return payload;
	}
	,getCrudType: function() {
		if(this.getProjectID() == "") return model.Project.CREATE; else return model.Project.UPDATE;
	}
	,sendReadRequest: function(projectID) {
		try {
			var nickName = MBooks_im.getSingleton().getNickName();
			var payload = this.getPayload(nickName,"Read",projectID);
			MBooks_im.getSingleton().doSendJSON(payload);
		} catch( err ) {
			console.log("Error checking company " + Std.string(err));
		}
	}
	,clearFields: function(aMessage) {
		try {
			this.setProjectID("");
			this.setProjectSummary("");
			this.setProjectDetails("");
			this.setProjectStart("");
			this.setProjectEnd("");
		} catch( err ) {
			console.log("Error clearing fields " + Std.string(err));
		}
	}
	,copyIncomingValues: function(wMessage) {
		try {
			var aMessage = wMessage.Right;
			this.projectID = aMessage.projectId;
			this.setProjectID(aMessage.projectId);
			this.setProjectSummary(aMessage.summary);
			this.setProjectDetails(aMessage.details);
			this.setProjectStart(aMessage.startDate);
			this.setProjectEnd(aMessage.endDate);
		} catch( err ) {
			console.log("Error copying values " + Std.string(wMessage));
		}
	}
	,processManageProject: function(incomingMessage) {
		console.log("Process manage Projects  ");
		try {
			var crudType = incomingMessage.Right.crudType;
			if(crudType == model.Project.CREATE) {
				console.log("Create successful");
				this.copyIncomingValues(incomingMessage);
			} else if(crudType == model.Project.READ) {
				if(incomingMessage.Right.projectId == "") this.newProject = true; else {
					this.copyIncomingValues(incomingMessage);
					this.newProject = false;
					var projectWorkbench = new model.ProjectWorkbench(this);
					this.activeProjectWorkbench = projectWorkbench;
				}
			} else if(crudType == model.Project.UPDATE) this.copyIncomingValues(incomingMessage); else if(crudType == model.Project.DELETE) this.clearFields(incomingMessage); else throw "Invalid crudtype " + crudType;
		} catch( err ) {
			throw err;
		}
	}
	,processProjectSelected: function(ev) {
		console.log("Project selected " + Std.string(ev.target));
		var selectionElement = ev.target;
		var selectionId = selectionElement.id;
		this.sendReadRequest(selectionId);
	}
	,getProjectList: function(companyId) {
		console.log("Processing select all projects " + companyId);
		var payload = { nickName : MBooks_im.getSingleton().getNickName(), commandType : "SelectActiveProjects", companyId : companyId};
		MBooks_im.getSingleton().doSendJSON(payload);
	}
	,processCompanySelected: function(ev) {
		console.log("Company selected" + " " + Std.string(ev.target) + " " + Std.string(ev));
		var selectionElement = ev.target;
		var selectedId = selectionElement.id;
		console.log("Reading company information for " + selectedId);
		this.company.read(selectedId);
		this.getProjectList(selectedId);
		MBooks_im.getSingleton().selectedCompanyStream.resolve(selectedId);
	}
	,setProjectDetails: function(details) {
		this.getProjectDetailsElement().value = details;
	}
	,getProjectDetailsElement: function() {
		return js.Browser.document.getElementById(model.Project.PROJECT_DETAILS);
	}
	,getProjectDetails: function() {
		return this.getProjectDetailsElement().value;
	}
	,setProjectSummary: function(summary) {
		this.getProjectSummaryElement().value = summary;
	}
	,getProjectSummaryElement: function() {
		return js.Browser.document.getElementById(model.Project.PROJECT_SUMMARY);
	}
	,getProjectSummary: function() {
		return this.getProjectSummaryElement().value;
	}
	,setPreparedBy: function(aName) {
		this.getPreparedByElement().value = aName;
	}
	,getPreparedByElement: function() {
		return js.Browser.document.getElementById(model.Project.PREPARED_BY);
	}
	,getPreparedBy: function() {
		return this.getPreparedByElement().value;
	}
	,setProjectEnd: function(eDate) {
		this.getProjectEndElement().value = eDate;
	}
	,getProjectEndElement: function() {
		return js.Browser.document.getElementById(model.Project.PROJECT_END);
	}
	,getProjectEnd: function() {
		return this.getProjectEndElement().value;
	}
	,setProjectStart: function(sDate) {
		this.getProjectStartElement().value = sDate;
	}
	,getProjectStartElement: function() {
		return js.Browser.document.getElementById(model.Project.PROJECT_START);
	}
	,getProjectStart: function() {
		return this.getProjectStartElement().value;
	}
	,setProjectID: function(pid) {
		this.getProjectIDElement().value = pid;
	}
	,getProjectIDElement: function() {
		return js.Browser.document.getElementById(model.Project.PROJECT_IDENTIFICATION);
	}
	,getProjectID: function() {
		if(this.getProjectIDElement().value != "") return this.getProjectIDElement().value; else return "";
	}
	,processCompanyList: function(incomingMessage) {
		console.log("Processing company list " + Std.string(incomingMessage));
		var companies = incomingMessage.company;
		var companiesSelectElement = this.getCompanyListElement();
		var cArray = incomingMessage.company;
		var _g = 0;
		while(_g < cArray.length) {
			var company = cArray[_g];
			++_g;
			var companyID = company.companyID;
			var companyName = company.companyName;
			console.log("Company " + companyID + " -> " + companyName);
			var optionElement = js.Browser.document.getElementById(companyID);
			if(optionElement == null) {
				optionElement = js.Browser.document.createElement("option");
				optionElement.id = companyID;
				optionElement.text = companyName;
				var optionSelectedStream = MBooks_im.getSingleton().initializeElementStream(optionElement,"click");
				optionSelectedStream.then($bind(this,this.processCompanySelected));
				companiesSelectElement.appendChild(optionElement);
			} else console.log("Element exists " + companyID);
		}
		console.log("Completed processing companies");
	}
	,getCompanyListElement: function() {
		return js.Browser.document.getElementById(model.Project.COMPANY_LIST);
	}
	,processProjectList: function(incomingMessage) {
		console.log("Project list " + Std.string(incomingMessage));
		var projects = incomingMessage.projects;
		var projectList = this.getProjectsListElement();
		var pArray = projects;
		var _g = 0;
		while(_g < pArray.length) {
			var project = pArray[_g];
			++_g;
			var projectId = project.identification;
			console.log("Adding project id " + projectId);
			var projectSummary = project.summary;
			var optionElement = js.Browser.document.getElementById(projectId);
			if(optionElement == null) {
				optionElement = js.Browser.document.createElement("option");
				optionElement.id = projectId;
				optionElement.text = projectSummary;
				var projectSelectedStream = MBooks_im.getSingleton().initializeElementStream(optionElement,"click");
				projectSelectedStream.then($bind(this,this.processProjectSelected));
				projectList.appendChild(optionElement);
			}
		}
	}
	,getProjectsListElement: function() {
		return js.Browser.document.getElementById(model.Project.PROJECT_LIST);
	}
	,saveProject: function(ev) {
		try {
			console.log("Saving project");
			var nickName = MBooks_im.getSingleton().getNickName();
			var payload = this.getPayloadD(nickName,this.getCrudType());
			MBooks_im.getSingleton().doSendJSON(payload);
		} catch( err ) {
			console.log("Error checking company " + Std.string(err));
		}
	}
	,getSaveProject: function() {
		var buttonElement = js.Browser.document.getElementById(model.Project.SAVE_PROJECT);
		return buttonElement;
	}
	,getSelectActiveProjectsStream: function() {
		return this.projectStream;
	}
	,getSupportedScriptsStream: function() {
		if(this.activeProjectWorkbench == null) throw "No active project workbench found"; else return this.activeProjectWorkbench.supportedScriptsStream;
	}
	,__class__: model.Project
}
model.WorkbenchCrudType = { __ename__ : true, __constructs__ : ["Create","WrkBench_Update","Delete","Read"] }
model.WorkbenchCrudType.Create = ["Create",0];
model.WorkbenchCrudType.Create.toString = $estr;
model.WorkbenchCrudType.Create.__enum__ = model.WorkbenchCrudType;
model.WorkbenchCrudType.WrkBench_Update = ["WrkBench_Update",1];
model.WorkbenchCrudType.WrkBench_Update.toString = $estr;
model.WorkbenchCrudType.WrkBench_Update.__enum__ = model.WorkbenchCrudType;
model.WorkbenchCrudType.Delete = ["Delete",2];
model.WorkbenchCrudType.Delete.toString = $estr;
model.WorkbenchCrudType.Delete.__enum__ = model.WorkbenchCrudType;
model.WorkbenchCrudType.Read = ["Read",3];
model.WorkbenchCrudType.Read.toString = $estr;
model.WorkbenchCrudType.Read.__enum__ = model.WorkbenchCrudType;
model.ProjectWorkbench = function(project) {
	this.MANAGE_WORKBENCH = "ManageWorkbench";
	this.QUERY_ACTIVE_WORKBENCHES = "QueryActiveWorkbenches";
	this.SUPPORTED_SCRIPT_TYPES = "QuerySupportedScripts";
	this.SCRIPT_META_TAGS = "scriptMetaTags";
	this.SCRIPT_DATA_PATH = "scriptDataPath";
	this.SCRIPT_SUMMARY = "scriptSummary";
	this.NUMBER_OF_CORES = "numberOfCores";
	this.SCRIPT_UPLOAD_ELEMENT = "uploadScript";
	this.SCRIPT_DATA_ELEMENT = "scriptData";
	this.WORKBENCH_ID_ELEMENT = "workbenchId";
	this.SUPPORTED_SCRIPT_LIST_ELEMENT = "supportedScriptTypes";
	this.PROJECT_DETAILS = "project-details";
	this.DEFAULT_PROCESSORS = 4;
	this.SCRIPT_RESULT = "scriptResult";
	this.CLEAR_FIELDS = "clearFields";
	this.EXECUTE_WORKBENCH = "executeScript";
	this.DELETE_WORKBENCH = "deleteWorkbench";
	this.CREATE_WORKBENCH = "insertWorkbench";
	this.UPDATE_WORKBENCH = "updateWorkbench";
	this.PROJECT_WORKBENCH_LIST = "projectWorkbenches";
	this.CHOOSE_SUPPORTED_SCRIPT = "chooseSupportedScriptType";
	this.CHOOSE_WORKBENCH = "chooseWorkbench";
	console.log("Instantiating project workbench ");
	this.executeUponSave = true;
	var stream = MBooks_im.getSingleton().initializeElementStream(this.getUpdateWorkbench(),"click");
	stream.then($bind(this,this.updateWorkbench));
	var createStream = MBooks_im.getSingleton().initializeElementStream(this.getCreateWorkbench(),"click");
	createStream.then($bind(this,this.createWorkbench));
	var deleteStream = MBooks_im.getSingleton().initializeElementStream(this.getDeleteWorkbench(),"click");
	deleteStream.then($bind(this,this.deleteWorkbench));
	var clearFieldsStream = MBooks_im.getSingleton().initializeElementStream(this.getClearFields(),"click");
	clearFieldsStream.then($bind(this,this.clearFields));
	var executeWorkbenchButtonStream = MBooks_im.getSingleton().initializeElementStream(this.getExecuteWorkbench(),"click");
	executeWorkbenchButtonStream.then($bind(this,this.executeWorkbench));
	this.selectedProject = project;
	this.selectedScriptType = "UnsupportedScriptType";
	this.supportedScriptsStream = new promhx.Deferred();
	this.supportedScriptsStream.then($bind(this,this.processSupportedScripts));
	this.queryActiveWorkbenchesStream = new promhx.Deferred();
	this.manageWorkbenchStream = new promhx.Deferred();
	this.queryActiveWorkbenchesStream.then($bind(this,this.processQueryActiveWorkbenches));
	this.querySupportedScripts();
	this.manageWorkbenchStream.then($bind(this,this.processManageWorkbench));
	this.executeWorkbenchStream = new promhx.Deferred();
	this.executeWorkbenchStream.then($bind(this,this.processExecuteWorkbench));
};
model.ProjectWorkbench.__name__ = ["model","ProjectWorkbench"];
model.ProjectWorkbench.getDynamic = function(name) {
	return __js__(name);
}
model.ProjectWorkbench.prototype = {
	processScriptTypeSelected: function(ev) {
		console.log("Script type selected " + Std.string(ev));
		var selectionElement = ev.target;
		this.selectedScriptType = selectionElement.id;
	}
	,handleThreeJSJSON: function(scriptData) {
		console.log("Processing three js json loading");
	}
	,handleThreeJS: function(scriptData) {
		console.log("processing three js");
	}
	,processScriptData: function(scriptType,scriptData) {
		console.log("Processing script type " + scriptType);
	}
	,clearFields: function(ev) {
		this.clearWorkbenchId();
		this.clearScriptSummary();
		this.clearSupportedScriptList();
		this.clearWorkbenchesList();
	}
	,copyIncomingValues: function(incomingMessage) {
		this.setWorkbenchIdFromMessage(incomingMessage.workbenchId);
		this.setScriptSummaryFromMessage(incomingMessage.scriptSummary);
		this.setScriptDataFromMessage(incomingMessage.scriptData);
		this.setScriptTypeFromMessage(incomingMessage.scriptType);
		this.setNumberOfCoresFromMessage(incomingMessage.numberOfCores);
		this.processScriptData(incomingMessage.scriptType,incomingMessage.scriptData);
	}
	,processManageWorkbench: function(incomingMessage) {
		console.log("Processing manage workbench " + Std.string(incomingMessage));
		var crudType = incomingMessage.crudType;
		this.setWorkbenchIdFromMessage(incomingMessage.workbenchId);
		this.copyIncomingValues(incomingMessage);
		if(crudType == "Create") {
			this.insertToActiveWorkbenches(this.getProjectWorkbenchListElement(),incomingMessage);
			if(this.executeUponSave) this.callExecuteWorkbench();
		} else if(crudType == "Delete") this.deleteFromActiveWorkbenches(this.getProjectWorkbenchListElement(),incomingMessage);
	}
	,processWorkbenchSelected: function(ev) {
		var selectionElement = ev.target;
		var selectionId = selectionElement.id;
		this.read(selectionId);
	}
	,deleteFromActiveWorkbenches: function(workbenchesUI,wrk) {
		var optionElement = js.Browser.document.getElementById(wrk.workbenchId);
		if(optionElement != null) workbenchesUI.removeChild(optionElement); else console.log("Element not found " + Std.string(wrk));
	}
	,processQueryActiveWorkbenches: function(queryActiveWorkbenches) {
		console.log("Processing query active workbenches " + Std.string(queryActiveWorkbenches));
		var workbenches = queryActiveWorkbenches.workbenches;
		var workbenchesUI = this.getProjectWorkbenchListElement();
		var firstElement = true;
		var _g = 0;
		while(_g < workbenches.length) {
			var wrk = workbenches[_g];
			++_g;
			var optElement = this.insertToActiveWorkbenches(workbenchesUI,wrk);
			if(firstElement) {
				optElement.selected = true;
				this.read(wrk.workbenchId);
				firstElement = false;
			}
		}
	}
	,insertToActiveWorkbenches: function(workbenchesUI,wrk) {
		var wId = wrk.workbenchId;
		var optionElement = js.Browser.document.getElementById(wId);
		if(optionElement == null) {
			optionElement = js.Browser.document.createElement("option");
			optionElement.id = wId;
			optionElement.text = wId;
			var optionSelectedStream = MBooks_im.getSingleton().initializeElementStream(optionElement,"click");
			optionSelectedStream.then($bind(this,this.processWorkbenchSelected));
			workbenchesUI.appendChild(optionElement);
		} else console.log("Element already exists " + wId);
		optionElement.selected = true;
		return optionElement;
	}
	,queryWorkbenches: function() {
		try {
			console.log("Query all active workbenches for " + this.selectedProject.projectID);
			var payload = { nickName : MBooks_im.getSingleton().getNickName(), projectId : this.selectedProject.projectID, commandType : this.QUERY_ACTIVE_WORKBENCHES, workbenches : []};
			MBooks_im.getSingleton().doSendJSON(payload);
		} catch( err ) {
			console.log("Error query workbenches " + Std.string(err));
		}
	}
	,querySupportedScripts: function() {
		console.log("Query supported scripts");
		var payload = { nickName : MBooks_im.getSingleton().getNickName(), commandType : this.SUPPORTED_SCRIPT_TYPES, scriptTypes : []};
		MBooks_im.getSingleton().doSendJSON(payload);
	}
	,processSupportedScripts: function(supportedScripts) {
		console.log("Process supported scripts  " + haxe.Json.stringify(supportedScripts));
		var supportedScriptListElement = this.getSupportedScriptsListElement();
		if(supportedScriptListElement == null) throw "Script type list element is not defined";
		var _g = 0, _g1 = supportedScripts.scriptTypes;
		while(_g < _g1.length) {
			var sType = _g1[_g];
			++_g;
			var optionElement = js.Browser.document.getElementById(sType);
			if(optionElement == null) {
				optionElement = js.Browser.document.createElement("option");
				optionElement.id = sType;
				optionElement.text = sType;
				supportedScriptListElement.appendChild(optionElement);
				var supportedScriptListStream = MBooks_im.getSingleton().initializeElementStream(optionElement,"click");
				supportedScriptListStream.then($bind(this,this.processScriptTypeSelected));
			} else console.log("Option element exists " + sType);
		}
		this.queryWorkbenches();
	}
	,drawGraph: function(inputData) {
		console.log("Input data " + Std.string(inputData));
		var values = new Array();
		var index = 0;
		var _g = 0;
		while(_g < inputData.length) {
			var i = inputData[_g];
			++_g;
			console.log("Inside loop " + Std.string(i));
			try {
				var pValue = Reflect.field(i,"p.value");
				if(pValue != null) {
					var p2 = 10000 * pValue;
					console.log("Setting p.value " + p2);
					var t = { x : index, y : p2};
					values[index] = t;
					index = index + 1;
				}
			} catch( err ) {
				console.log("Ignoring " + Std.string(err));
			}
		}
		var formatCount = d3.format(",.0f");
		var formatCount1 = d3.format(",.0f");
		var margin = { top : 10, right : 30, bottom : 30, left : 30}, width = 960 - margin.left - margin.right, height = 500 - margin.top - margin.bottom;
		var x = d3.scale.linear().domain([0,values.length]).range([0,width]);
		var data = values;
		var y = d3.scale.linear().domain([0,d3.max(data,function(d) {
			return d.y;
		})]).range([height,0]);
		var xAxis = d3.svg.axis().scale(x);
		var svg = d3.select("body").append("svg").attr("width",width + margin.left + margin.right).attr("height",height + margin.top + margin.bottom).append("g").attr("transform","translate(" + margin.left + "," + margin.top + ")");
		var bar = svg.selectAll(".bar").data(data).enter().append("g").attr("class","bar").attr("transform",function(d) {
			return "translate(" + Std.string(x(d.x)) + "," + Std.string(y(d.y)) + ")";
		});
		bar.append("rect").attr("x",1).attr("width",10).attr("height",function(d) {
			return height - y(d.y);
		});
		bar.append("text").attr("dy",".75em").attr("y",6).attr("x",5).attr("text-anchor","middle").text(function(d) {
			return formatCount1(d.y);
		});
		svg.append("g").attr("class","x axis").attr("transform","translate(0," + height + ")").call(xAxis);
	}
	,setScriptResult: function(workbench) {
		var inputElement = js.Browser.document.getElementById(this.SCRIPT_RESULT);
		console.log("Workbench " + Std.string(workbench.Right));
		if(workbench.Right != null) {
			var tempResult = workbench.Right;
			var tempResultS = haxe.Json.parse(tempResult.scriptResult);
			this.drawGraph(tempResultS);
		} else inputElement.value = haxe.Json.stringify(workbench);
	}
	,processExecuteWorkbench: function(executeWorkbench) {
		console.log("Processing execute workbench " + haxe.Json.stringify(executeWorkbench));
		this.setScriptResult(executeWorkbench);
	}
	,getClearFields: function() {
		return js.Browser.document.getElementById(this.CLEAR_FIELDS);
	}
	,getExecuteWorkbench: function() {
		return js.Browser.document.getElementById(this.EXECUTE_WORKBENCH);
	}
	,getDeleteWorkbench: function() {
		return js.Browser.document.getElementById(this.DELETE_WORKBENCH);
	}
	,getCreateWorkbench: function() {
		return js.Browser.document.getElementById(this.CREATE_WORKBENCH);
	}
	,getUpdateWorkbench: function() {
		return js.Browser.document.getElementById(this.UPDATE_WORKBENCH);
	}
	,getPayloadFromUI: function(crudType,scriptData) {
		var lCrudType = this.toString(crudType);
		if(lCrudType == "Create") {
			if(scriptData == null || scriptData == "") {
				console.log("Nothing to save");
				throw "Inserting object with no script data ";
			}
		}
		var result = { crudType : this.toString(crudType), workbenchId : this.getWorkbenchIdFromUI(), uniqueProjectId : this.selectedProject.projectID, scriptType : this.getScriptTypeFromUI(), scriptSummary : this.getScriptSummaryFromUI(), scriptData : scriptData, numberOfCores : this.getNumberOfCoresFromUI(), scriptDataPath : this.getScriptDataPathFromUI(), jobStartDate : this.getJobStartDateFromUI(), jobEndDate : this.getJobEndDateFromUI(), nickName : MBooks_im.getSingleton().getNickName(), commandType : "ManageWorkbench"};
		return result;
	}
	,toString: function(crudType) {
		switch( (crudType)[1] ) {
		case 0:
			return "Create";
		case 1:
			return "WrkBench_Update";
		case 2:
			return "Delete";
		case 3:
			return "Read";
		}
	}
	,getJobEndDateFromUI: function() {
		return null;
	}
	,getJobStartDateFromUI: function() {
		return null;
	}
	,getScriptDataPathFromUI: function() {
		return null;
	}
	,getNumberOfCoresFromUI: function() {
		if(this.getNumberOfCoresElement().value == "") return this.DEFAULT_PROCESSORS;
		return Std.parseInt(this.getNumberOfCoresElement().value);
	}
	,getNumberOfCoresElement: function() {
		return js.Browser.document.getElementById(this.NUMBER_OF_CORES);
	}
	,setNumberOfCoresFromMessage: function(numberOfCores) {
		this.getNumberOfCoresElement().value = "" + numberOfCores;
	}
	,setScriptDataFromMessage: function(aMessage) {
		try {
			this.getScriptDataElement().value = aMessage;
		} catch( err ) {
			this.getScriptDataElement().value = haxe.Json.stringify(err);
		}
	}
	,getScriptDataElement: function() {
		return js.Browser.document.getElementById(this.SCRIPT_DATA_ELEMENT);
	}
	,getScriptUploadElement: function() {
		return js.Browser.document.getElementById(this.SCRIPT_UPLOAD_ELEMENT);
	}
	,setScriptSummaryFromMessage: function(aMessage) {
		this.getScriptSummaryElement().value = aMessage;
	}
	,clearScriptSummary: function() {
		this.getScriptSummaryElement().value = "";
	}
	,getScriptSummaryFromUI: function() {
		return this.getScriptSummaryElement().value;
	}
	,getScriptSummaryElement: function() {
		return js.Browser.document.getElementById(this.SCRIPT_SUMMARY);
	}
	,setScriptTypeFromMessage: function(aScriptType) {
		console.log("Setting script type");
		var element = js.Browser.document.getElementById(aScriptType);
		element.selected = true;
		this.selectedScriptType = aScriptType;
	}
	,getScriptTypeFromUI: function() {
		return this.selectedScriptType;
	}
	,clearWorkbenchesList: function() {
		var element = js.Browser.document.getElementById(this.CHOOSE_WORKBENCH);
		element.selected = true;
	}
	,clearSupportedScriptList: function() {
		var element = js.Browser.document.getElementById(this.CHOOSE_SUPPORTED_SCRIPT);
		element.selected = true;
	}
	,getScriptTypeElement: function() {
		return js.Browser.document.getElementById(this.SUPPORTED_SCRIPT_LIST_ELEMENT);
	}
	,setWorkbenchIdFromMessage: function(wid) {
		this.getWorkbenchIdElement().value = wid;
	}
	,clearWorkbenchId: function() {
		var workbenchId = this.getWorkbenchIdElement().value;
		if(workbenchId != "") {
			console.log("Clearing workbench id ");
			var optionElement = js.Browser.document.getElementById(workbenchId);
			optionElement.selected = false;
		} else console.log("Not clearing empty workbench id");
		var optionElement = js.Browser.document.getElementById(this.CHOOSE_WORKBENCH);
		optionElement.selected = true;
		this.getWorkbenchIdElement().value = "";
	}
	,getWorkbenchIdFromUI: function() {
		return this.getWorkbenchIdElement().value;
	}
	,getWorkbenchIdElement: function() {
		return js.Browser.document.getElementById(this.WORKBENCH_ID_ELEMENT);
	}
	,getProjectWorkbenchListElement: function() {
		return js.Browser.document.getElementById(this.PROJECT_WORKBENCH_LIST);
	}
	,uploadScript: function(ev) {
		console.log("Uploading script " + Std.string(ev));
		var reader = ev.target;
		this.saveWorkbenchModel(reader.result);
	}
	,getCrudType: function() {
		if(this.getWorkbenchIdFromUI() == null || this.getWorkbenchIdFromUI() == "") return model.WorkbenchCrudType.Create; else return model.WorkbenchCrudType.WrkBench_Update;
	}
	,executeWorkbench: function(ev) {
		this.callExecuteWorkbench();
	}
	,callExecuteWorkbench: function() {
		try {
			console.log("Execute the workbench");
			var payload = { executeWorkbenchCommandType : "ExecuteWorkbench", executeWorkbenchId : this.getWorkbenchIdFromUI(), scriptResult : "", nickName : MBooks_im.getSingleton().getNickName(), commandType : "ExecuteWorkbench"};
			MBooks_im.getSingleton().doSendJSON(payload);
		} catch( err ) {
			console.log("Error saving workbench " + Std.string(err));
		}
	}
	,saveWorkbenchModel: function(scriptData) {
		try {
			var crudType = this.getCrudType();
			var payload = this.getPayloadFromUI(crudType,scriptData);
			console.log("Saving workbench model " + haxe.Json.stringify(payload));
			MBooks_im.getSingleton().doSendJSON(payload);
		} catch( err ) {
			console.log("Error saving workbench " + Std.string(err));
		}
	}
	,read: function(anId) {
		try {
			var payload = this.getPayloadFromUI(model.WorkbenchCrudType.Read,"");
			payload.workbenchId = anId;
			console.log("Reading workbench");
			MBooks_im.getSingleton().doSendJSON(payload);
		} catch( err ) {
			console.log("Error " + Std.string(err));
		}
	}
	,updateWorkbench: function(ev) {
		console.log("Update workbench ");
		var file = this.getScriptUploadElement().files[0];
		var reader = new FileReader();
		var stream = MBooks_im.getSingleton().initializeElementStream(reader,"load");
		stream.then($bind(this,this.uploadScript));
		reader.readAsText(file);
	}
	,deleteWorkbench: function(ev) {
		console.log("Deleting workbench");
		try {
			var crudType = model.WorkbenchCrudType.Delete;
			this.scriptData = "";
			var payload = this.getPayloadFromUI(crudType,this.scriptData);
			console.log("Saving workbench model " + haxe.Json.stringify(payload));
			MBooks_im.getSingleton().doSendJSON(payload);
		} catch( err ) {
			console.log("Error saving workbench " + Std.string(err));
		}
	}
	,createWorkbench: function(ev) {
		console.log("Creating workbench");
		this.clearWorkbenchId();
		var file = this.getScriptUploadElement().files[0];
		var reader = new FileReader();
		var stream = MBooks_im.getSingleton().initializeElementStream(reader,"load");
		stream.then($bind(this,this.uploadScript));
		reader.readAsText(file);
	}
	,getSupportedScriptsListElement: function() {
		return js.Browser.document.getElementById(this.SUPPORTED_SCRIPT_LIST_ELEMENT);
	}
	,__class__: model.ProjectWorkbench
}
model.UserOperation = function(o) {
	this.operation = o;
};
model.UserOperation.__name__ = ["model","UserOperation"];
model.UserOperation.prototype = {
	__class__: model.UserOperation
}
var org = {}
org.hamcrest = {}
org.hamcrest.Exception = function(message,cause,info) {
	if(message == null) message = "";
	this.name = Type.getClassName(Type.getClass(this));
	this.message = message;
	this.cause = cause;
	this.info = info;
};
org.hamcrest.Exception.__name__ = ["org","hamcrest","Exception"];
org.hamcrest.Exception.prototype = {
	toString: function() {
		var str = this.get_name() + ": " + this.get_message();
		if(this.info != null) str += " at " + this.info.className + "#" + this.info.methodName + " (" + this.info.lineNumber + ")";
		if(this.get_cause() != null) str += "\n\t Caused by: " + Std.string(this.get_cause());
		return str;
	}
	,get_cause: function() {
		return this.cause;
	}
	,get_message: function() {
		return this.message;
	}
	,get_name: function() {
		return this.name;
	}
	,__class__: org.hamcrest.Exception
}
org.hamcrest.AssertionException = function(message,cause,info) {
	if(message == null) message = "";
	org.hamcrest.Exception.call(this,message,cause,info);
};
org.hamcrest.AssertionException.__name__ = ["org","hamcrest","AssertionException"];
org.hamcrest.AssertionException.__super__ = org.hamcrest.Exception;
org.hamcrest.AssertionException.prototype = $extend(org.hamcrest.Exception.prototype,{
	__class__: org.hamcrest.AssertionException
});
org.hamcrest.IllegalArgumentException = function(message,cause,info) {
	if(message == null) message = "Argument could not be processed.";
	org.hamcrest.Exception.call(this,message,cause,info);
};
org.hamcrest.IllegalArgumentException.__name__ = ["org","hamcrest","IllegalArgumentException"];
org.hamcrest.IllegalArgumentException.__super__ = org.hamcrest.Exception;
org.hamcrest.IllegalArgumentException.prototype = $extend(org.hamcrest.Exception.prototype,{
	__class__: org.hamcrest.IllegalArgumentException
});
org.hamcrest.MissingImplementationException = function(message,cause,info) {
	if(message == null) message = "Abstract method not overridden.";
	org.hamcrest.Exception.call(this,message,cause,info);
};
org.hamcrest.MissingImplementationException.__name__ = ["org","hamcrest","MissingImplementationException"];
org.hamcrest.MissingImplementationException.__super__ = org.hamcrest.Exception;
org.hamcrest.MissingImplementationException.prototype = $extend(org.hamcrest.Exception.prototype,{
	__class__: org.hamcrest.MissingImplementationException
});
org.hamcrest.UnsupportedOperationException = function(message,cause,info) {
	if(message == null) message = "";
	org.hamcrest.Exception.call(this,message,cause,info);
};
org.hamcrest.UnsupportedOperationException.__name__ = ["org","hamcrest","UnsupportedOperationException"];
org.hamcrest.UnsupportedOperationException.__super__ = org.hamcrest.Exception;
org.hamcrest.UnsupportedOperationException.prototype = $extend(org.hamcrest.Exception.prototype,{
	__class__: org.hamcrest.UnsupportedOperationException
});
var promhx = {}
promhx.base = {}
promhx.base.AsyncBase = function(d) {
	this._resolved = false;
	this._pending = false;
	this._errorPending = false;
	this._fulfilled = false;
	this._update = [];
	this._error = [];
	this._errored = false;
	if(d != null) promhx.base.AsyncBase.link(d,this,function(x) {
		return x;
	});
};
promhx.base.AsyncBase.__name__ = ["promhx","base","AsyncBase"];
promhx.base.AsyncBase.link = function(current,next,f) {
	current._update.push({ async : next, linkf : function(x) {
		next.handleResolve(f(x));
	}});
	promhx.base.AsyncBase.immediateLinkUpdate(current,next,f);
}
promhx.base.AsyncBase.immediateLinkUpdate = function(current,next,f) {
	if(current._errored) next.handleError(current._errorVal);
	if(current._resolved && !current._pending) try {
		next.handleResolve(f(current._val));
	} catch( e ) {
		next.handleError(e);
	}
}
promhx.base.AsyncBase.linkAll = function(all,next) {
	var cthen = function(arr,current,v) {
		if(arr.length == 0 || promhx.base.AsyncBase.allFulfilled(arr)) {
			var vals = (function($this) {
				var $r;
				var _g = [];
				var $it0 = $iterator(all)();
				while( $it0.hasNext() ) {
					var a = $it0.next();
					_g.push(a == current?v:a._val);
				}
				$r = _g;
				return $r;
			}(this));
			next.handleResolve(vals);
		}
		return null;
	};
	var $it1 = $iterator(all)();
	while( $it1.hasNext() ) {
		var a = $it1.next();
		a._update.push({ async : next, linkf : (function(f,a1,a2) {
			return function(v) {
				return f(a1,a2,v);
			};
		})(cthen,(function($this) {
			var $r;
			var _g = [];
			var $it2 = $iterator(all)();
			while( $it2.hasNext() ) {
				var a2 = $it2.next();
				if(a2 != a) _g.push(a2);
			}
			$r = _g;
			return $r;
		}(this)),a)});
	}
	if(promhx.base.AsyncBase.allFulfilled(all)) next.handleResolve((function($this) {
		var $r;
		var _g = [];
		var $it3 = $iterator(all)();
		while( $it3.hasNext() ) {
			var a = $it3.next();
			_g.push(a._val);
		}
		$r = _g;
		return $r;
	}(this)));
}
promhx.base.AsyncBase.pipeLink = function(current,ret,f) {
	var linked = false;
	var linkf = function(x) {
		if(!linked) {
			linked = true;
			var pipe_ret = f(x);
			pipe_ret._update.push({ async : ret, linkf : $bind(ret,ret.handleResolve)});
			promhx.base.AsyncBase.immediateLinkUpdate(pipe_ret,ret,function(x1) {
				return x1;
			});
		}
	};
	current._update.push({ async : ret, linkf : linkf});
	if(current._resolved && !current._pending) try {
		linkf(current._val);
	} catch( e ) {
		ret.handleError(e);
	}
}
promhx.base.AsyncBase.allResolved = function($as) {
	var $it0 = $iterator($as)();
	while( $it0.hasNext() ) {
		var a = $it0.next();
		if(!a._resolved) return false;
	}
	return true;
}
promhx.base.AsyncBase.allFulfilled = function($as) {
	var $it0 = $iterator($as)();
	while( $it0.hasNext() ) {
		var a = $it0.next();
		if(!a._fulfilled) return false;
	}
	return true;
}
promhx.base.AsyncBase.prototype = {
	isLinked: function(to) {
		var updated = false;
		var _g = 0, _g1 = this._update;
		while(_g < _g1.length) {
			var u = _g1[_g];
			++_g;
			if(u.async == to) return true;
		}
		return updated;
	}
	,unlink: function(to) {
		var _g = this;
		promhx.base.EventLoop.queue.add(function() {
			_g._update = _g._update.filter(function(x) {
				return x.async != to;
			});
		});
		promhx.base.EventLoop.continueOnNextLoop();
	}
	,then: function(f) {
		var ret = new promhx.base.AsyncBase();
		promhx.base.AsyncBase.link(this,ret,f);
		return ret;
	}
	,_handleError: function(error) {
		var _g = this;
		var update_errors = function(e) {
			if(_g._error.length > 0) {
				var _g1 = 0, _g2 = _g._error;
				while(_g1 < _g2.length) {
					var ef = _g2[_g1];
					++_g1;
					ef(e);
				}
			} else if(_g._update.length > 0) {
				var _g1 = 0, _g2 = _g._update;
				while(_g1 < _g2.length) {
					var up = _g2[_g1];
					++_g1;
					up.async.handleError(e);
				}
			} else throw e;
			_g._errorPending = false;
		};
		if(!this._errorPending) {
			this._errorPending = true;
			this._errored = true;
			this._errorVal = error;
			promhx.base.EventLoop.queue.add(function() {
				if(_g._errorMap != null) try {
					_g._resolve(_g._errorMap(error));
				} catch( e ) {
					update_errors(e);
				} else update_errors(error);
			});
			promhx.base.EventLoop.continueOnNextLoop();
		}
	}
	,handleError: function(error) {
		this._handleError(error);
	}
	,_resolve: function(val) {
		var _g = this;
		if(this._pending) {
			promhx.base.EventLoop.queue.add((function(f,a1) {
				return function() {
					return f(a1);
				};
			})($bind(this,this._resolve),val));
			promhx.base.EventLoop.continueOnNextLoop();
		} else {
			this._resolved = true;
			this._pending = true;
			promhx.base.EventLoop.queue.add(function() {
				_g._val = val;
				var _g1 = 0, _g2 = _g._update;
				while(_g1 < _g2.length) {
					var up = _g2[_g1];
					++_g1;
					try {
						up.linkf(val);
					} catch( e ) {
						up.async.handleError(e);
					}
				}
				_g._fulfilled = true;
				_g._pending = false;
			});
			promhx.base.EventLoop.continueOnNextLoop();
		}
	}
	,handleResolve: function(val) {
		this._resolve(val);
	}
	,isPending: function() {
		return this._pending;
	}
	,isFulfilled: function() {
		return this._fulfilled;
	}
	,isErrored: function() {
		return this._errored;
	}
	,isResolved: function() {
		return this._resolved;
	}
	,errorThen: function(f) {
		this._errorMap = f;
		return this;
	}
	,catchError: function(f) {
		this._error.push(f);
		return this;
	}
	,__class__: promhx.base.AsyncBase
}
promhx.Deferred = function() {
	promhx.base.AsyncBase.call(this);
};
$hxExpose(promhx.Deferred, "promhx.Deferred");
promhx.Deferred.__name__ = ["promhx","Deferred"];
promhx.Deferred.__super__ = promhx.base.AsyncBase;
promhx.Deferred.prototype = $extend(promhx.base.AsyncBase.prototype,{
	publicStream: function() {
		return new promhx.PublicStream(this);
	}
	,stream: function() {
		return new promhx.Stream(this);
	}
	,promise: function() {
		return new promhx.Promise(this);
	}
	,throwError: function(e) {
		this.handleError(e);
	}
	,resolve: function(val) {
		this.handleResolve(val);
	}
	,__class__: promhx.Deferred
});
promhx.Promise = function(d) {
	promhx.base.AsyncBase.call(this,d);
	this._rejected = false;
};
$hxExpose(promhx.Promise, "promhx.Promise");
promhx.Promise.__name__ = ["promhx","Promise"];
promhx.Promise.whenAll = function(itb) {
	var ret = new promhx.Promise();
	promhx.base.AsyncBase.linkAll(itb,ret);
	return ret;
}
promhx.Promise.promise = function(_val) {
	var ret = new promhx.Promise();
	ret.handleResolve(_val);
	return ret;
}
promhx.Promise.__super__ = promhx.base.AsyncBase;
promhx.Promise.prototype = $extend(promhx.base.AsyncBase.prototype,{
	errorPipe: function(f) {
		var ret = new promhx.Promise();
		this.catchError(function(e) {
			var piped = f(e);
			piped.then($bind(ret,ret._resolve));
		});
		this.then($bind(ret,ret._resolve));
		return ret;
	}
	,pipe: function(f) {
		var ret = new promhx.Promise();
		promhx.base.AsyncBase.pipeLink(this,ret,f);
		return ret;
	}
	,handleError: function(error) {
		this._rejected = true;
		this._handleError(error);
	}
	,unlink: function(to) {
		var _g = this;
		promhx.base.EventLoop.queue.add(function() {
			if(!_g._fulfilled) {
				var msg = "Downstream Promise is not fullfilled";
				_g.handleError(promhx.error.PromiseError.DownstreamNotFullfilled(msg));
			} else _g._update = _g._update.filter(function(x) {
				return x.async != to;
			});
		});
		promhx.base.EventLoop.continueOnNextLoop();
	}
	,then: function(f) {
		var ret = new promhx.Promise();
		promhx.base.AsyncBase.link(this,ret,f);
		return ret;
	}
	,handleResolve: function(val) {
		if(this._resolved) {
			var msg = "Promise has already been resolved";
			throw promhx.error.PromiseError.AlreadyResolved(msg);
		}
		this._resolve(val);
	}
	,reject: function(e) {
		this._rejected = true;
		this.handleError(e);
	}
	,isRejected: function() {
		return this._rejected;
	}
	,__class__: promhx.Promise
});
promhx.Stream = function(d) {
	promhx.base.AsyncBase.call(this,d);
	this._end_deferred = new promhx.Deferred();
	this._end_promise = this._end_deferred.promise();
};
$hxExpose(promhx.Stream, "promhx.Stream");
promhx.Stream.__name__ = ["promhx","Stream"];
promhx.Stream.foreach = function(itb) {
	var s = new promhx.Stream();
	var $it0 = $iterator(itb)();
	while( $it0.hasNext() ) {
		var i = $it0.next();
		s.handleResolve(i);
	}
	s.end();
	return s;
}
promhx.Stream.wheneverAll = function(itb) {
	var ret = new promhx.Stream();
	promhx.base.AsyncBase.linkAll(itb,ret);
	return ret;
}
promhx.Stream.concatAll = function(itb) {
	var ret = new promhx.Stream();
	var $it0 = $iterator(itb)();
	while( $it0.hasNext() ) {
		var i = $it0.next();
		ret.concat(i);
	}
	return ret;
}
promhx.Stream.mergeAll = function(itb) {
	var ret = new promhx.Stream();
	var $it0 = $iterator(itb)();
	while( $it0.hasNext() ) {
		var i = $it0.next();
		ret.merge(i);
	}
	return ret;
}
promhx.Stream.stream = function(_val) {
	var ret = new promhx.Stream();
	ret.handleResolve(_val);
	return ret;
}
promhx.Stream.__super__ = promhx.base.AsyncBase;
promhx.Stream.prototype = $extend(promhx.base.AsyncBase.prototype,{
	merge: function(s) {
		var ret = new promhx.Stream();
		this._update.push({ async : ret, linkf : $bind(ret,ret.handleResolve)});
		s._update.push({ async : ret, linkf : $bind(ret,ret.handleResolve)});
		promhx.base.AsyncBase.immediateLinkUpdate(this,ret,function(x) {
			return x;
		});
		promhx.base.AsyncBase.immediateLinkUpdate(s,ret,function(x) {
			return x;
		});
		return ret;
	}
	,concat: function(s) {
		var ret = new promhx.Stream();
		this._update.push({ async : ret, linkf : $bind(ret,ret.handleResolve)});
		promhx.base.AsyncBase.immediateLinkUpdate(this,ret,function(x) {
			return x;
		});
		this._end_promise.then(function(_) {
			s.pipe(function(x) {
				ret.handleResolve(x);
				return ret;
			});
			s._end_promise.then(function(_1) {
				ret.end();
			});
		});
		return ret;
	}
	,filter: function(f) {
		var ret = new promhx.Stream();
		this._update.push({ async : ret, linkf : function(x) {
			if(f(x)) ret.handleResolve(x);
		}});
		promhx.base.AsyncBase.immediateLinkUpdate(this,ret,function(x) {
			return x;
		});
		return ret;
	}
	,endThen: function(f) {
		return this._end_promise.then(f);
	}
	,end: function() {
		promhx.base.EventLoop.queue.add($bind(this,this.handleEnd));
		promhx.base.EventLoop.continueOnNextLoop();
		return this;
	}
	,handleEnd: function() {
		if(this._pending) {
			promhx.base.EventLoop.queue.add($bind(this,this.handleEnd));
			promhx.base.EventLoop.continueOnNextLoop();
		} else if(this._end_promise._resolved) return; else {
			this._end = true;
			var o = this._resolved?haxe.ds.Option.Some(this._val):haxe.ds.Option.None;
			this._end_promise.handleResolve(o);
			this._update = [];
			this._error = [];
		}
	}
	,errorPipe: function(f) {
		var ret = new promhx.Stream();
		this.catchError(function(e) {
			var piped = f(e);
			piped.then($bind(ret,ret._resolve));
			piped._end_promise.then(($_=ret._end_promise,$bind($_,$_._resolve)));
		});
		this.then($bind(ret,ret._resolve));
		this._end_promise.then(function(x) {
			ret.end();
		});
		return ret;
	}
	,pipe: function(f) {
		var ret = new promhx.Stream();
		promhx.base.AsyncBase.pipeLink(this,ret,f);
		this._end_promise.then(function(x) {
			ret.end();
		});
		return ret;
	}
	,pause: function(set) {
		if(set == null) set = !this._pause;
		this._pause = set;
	}
	,handleResolve: function(val) {
		if(!this._end && !this._pause) this._resolve(val);
	}
	,first: function() {
		var s = new promhx.Promise();
		this.then(function(x) {
			if(!s._resolved) s.handleResolve(x);
		});
		return s;
	}
	,detachStream: function(str) {
		var filtered = [];
		var removed = false;
		var _g = 0, _g1 = this._update;
		while(_g < _g1.length) {
			var u = _g1[_g];
			++_g;
			if(u.async == str) removed = true; else filtered.push(u);
		}
		this._update = filtered;
		return removed;
	}
	,then: function(f) {
		var ret = new promhx.Stream();
		promhx.base.AsyncBase.link(this,ret,f);
		this._end_promise.then(function(x) {
			ret.end();
		});
		return ret;
	}
	,__class__: promhx.Stream
});
promhx.PublicStream = function(def) {
	promhx.Stream.call(this,def);
};
$hxExpose(promhx.PublicStream, "promhx.PublicStream");
promhx.PublicStream.__name__ = ["promhx","PublicStream"];
promhx.PublicStream.publicstream = function(val) {
	var ps = new promhx.PublicStream();
	ps.handleResolve(val);
	return ps;
}
promhx.PublicStream.__super__ = promhx.Stream;
promhx.PublicStream.prototype = $extend(promhx.Stream.prototype,{
	update: function(val) {
		this.handleResolve(val);
	}
	,throwError: function(e) {
		this.handleError(e);
	}
	,resolve: function(val) {
		this.handleResolve(val);
	}
	,__class__: promhx.PublicStream
});
promhx.base.EventLoop = function() { }
promhx.base.EventLoop.__name__ = ["promhx","base","EventLoop"];
promhx.base.EventLoop.enqueue = function(eqf) {
	promhx.base.EventLoop.queue.add(eqf);
	promhx.base.EventLoop.continueOnNextLoop();
}
promhx.base.EventLoop.set_nextLoop = function(f) {
	if(promhx.base.EventLoop.nextLoop != null) throw "nextLoop has already been set"; else promhx.base.EventLoop.nextLoop = f;
	return promhx.base.EventLoop.nextLoop;
}
promhx.base.EventLoop.queueEmpty = function() {
	return promhx.base.EventLoop.queue.isEmpty();
}
promhx.base.EventLoop.finish = function(max_iterations) {
	if(max_iterations == null) max_iterations = 1000;
	var fn = null;
	while(max_iterations-- > 0 && (fn = promhx.base.EventLoop.queue.pop()) != null) fn();
	return promhx.base.EventLoop.queue.isEmpty();
}
promhx.base.EventLoop.clear = function() {
	promhx.base.EventLoop.queue = new List();
}
promhx.base.EventLoop.f = function() {
	var fn = promhx.base.EventLoop.queue.pop();
	if(fn != null) fn();
	if(!promhx.base.EventLoop.queue.isEmpty()) promhx.base.EventLoop.continueOnNextLoop();
}
promhx.base.EventLoop.continueOnNextLoop = function() {
	if(promhx.base.EventLoop.nextLoop != null) promhx.base.EventLoop.nextLoop(promhx.base.EventLoop.f); else setImmediate(promhx.base.EventLoop.f);
}
promhx.error = {}
promhx.error.PromiseError = { __ename__ : true, __constructs__ : ["AlreadyResolved","DownstreamNotFullfilled"] }
promhx.error.PromiseError.AlreadyResolved = function(message) { var $x = ["AlreadyResolved",0,message]; $x.__enum__ = promhx.error.PromiseError; $x.toString = $estr; return $x; }
promhx.error.PromiseError.DownstreamNotFullfilled = function(message) { var $x = ["DownstreamNotFullfilled",1,message]; $x.__enum__ = promhx.error.PromiseError; $x.toString = $estr; return $x; }
promhx.haxe = {}
promhx.haxe.EventTools = function() { }
promhx.haxe.EventTools.__name__ = ["promhx","haxe","EventTools"];
promhx.haxe.EventTools.eventStream = function(el,event,useCapture) {
	var def = new promhx.Deferred();
	el.addEventListener(event,$bind(def,def.resolve),useCapture);
	return def.promise();
}
var util = {}
util.Config = function() { }
util.Config.__name__ = ["util","Config"];
util.Util = function() { }
util.Util.__name__ = ["util","Util"];
util.Util.NEW_LINE = function() {
	return 10;
}
util.Util.TAB = function() {
	return 9;
}
util.Util.CR = function() {
	return 13;
}
util.Util.isUpOrDown = function(code) {
	return code == util.Util.UP_ARROW || code == util.Util.DOWN_ARROW;
}
util.Util.isUP = function(code) {
	return code == util.Util.UP_ARROW;
}
util.Util.issDown = function(code) {
	return code == util.Util.DOWN_ARROW;
}
util.Util.isTab = function(code) {
	return code == util.Util.TAB();
}
util.Util.isBackspace = function(code) {
	return code == util.Util.BACKSPACE;
}
util.Util.isSignificantWS = function(code) {
	return code == util.Util.TAB() || code == util.Util.NEW_LINE() || code == util.Util.CR();
}
util.Util.createDivTag = function(document,className) {
	console.log("Creating DIV tag " + className);
	var div = document.getElementById(className);
	if(div == null) {
		div = document.createElement("div");
		div.className = className;
		div.id = "DIV" + "_" + className;
		document.body.appendChild(div);
	} else console.log("Div tag exists -> " + className);
	return div;
}
util.Util.createInputElement = function(document,parent,elementClass,elementName) {
	var inputElement = document.createElement("input");
	inputElement.id = elementName;
	parent.appendChild(inputElement);
}
util.Util.createTextAreaElement = function(document,parent,elementName,elementClass) {
	var areaElement = document.createElement("textarea");
	areaElement.id = elementName;
	areaElement.rows = util.Util.DEFAULT_ROWS;
	areaElement.cols = util.Util.DEFAULT_COLS;
	parent.appendChild(areaElement);
}
util.Util.createListElement = function(document,parent,elementClass,elementName) {
	var listElement = document.createElement("ul");
	listElement.id = elementName;
	parent.appendChild(listElement);
}
util.Util.createButtonElement = function(document,parent,elementClass,elementName) {
	var element = document.createElement("button");
	element.value = elementName;
	element.id = elementName;
	element.innerHTML = elementName;
	parent.appendChild(element);
}
util.Util.createSelectElement = function(document,parent,elementClass,elementName) {
	var element = document.createElement("select");
	element.id = elementName;
	parent.appendChild(element);
}
util.Util.createElementWithLabel = function(document,parent,elementId,elementLabel) {
	var inputLabel = document.createElement("label");
	var input = document.createElement("input");
	input.id = elementId;
	inputLabel.id = util.Util.LABEL + elementId;
	inputLabel.innerHTML = elementLabel;
	parent.appendChild(inputLabel);
	parent.appendChild(input);
}
util.Util.createTextAreaElementWithLabel = function(document,parent,elementId,elementLabel) {
	var inputLabel = document.createElement("label");
	inputLabel.id = util.Util.LABEL + elementId;
	inputLabel.innerHTML = elementLabel;
	console.log("Element id before calling textareaelement " + elementId);
	util.Util.createTextAreaElement(document,parent,elementId,elementId);
	var textAreaElement = document.getElementById(elementId);
}
util.Util.showDivField = function(fieldName) {
	var div = js.Browser.document.getElementById(fieldName);
	div.setAttribute("style","display:normal");
}
util.Util.hideDivField = function(fieldName) {
	var div = js.Browser.document.getElementById(fieldName);
	div.setAttribute("style","display:none");
}
util.Util.logToServer = function(logMessage) {
}
util.Util.log = function(logMessage) {
	console.log(logMessage);
}
var view = {}
view.Company = function() {
	console.log("Instantiating company");
	this.newCompany = true;
	var stream = MBooks_im.getSingleton().initializeElementStream(this.getCompanySignup(),"click");
	var cidStream = MBooks_im.getSingleton().initializeElementStream(this.getCompanyIDElement(),"keyup");
	cidStream.then($bind(this,this.chkCompanyExists));
	stream.then($bind(this,this.saveButtonPressed));
	this.selectListEventStream = new promhx.Deferred();
	MBooks_im.getSingleton().getUserLoggedInStream().then($bind(this,this.selectAllCompanies));
	MBooks_im.getSingleton().selectedCompanyStream.then($bind(this,this.assignCompanyToUser));
	MBooks_im.getSingleton().assignCompanyStream.then($bind(this,this.assignCompanyResponse));
};
view.Company.__name__ = ["view","Company"];
view.Company.prototype = {
	assignCompanyResponse: function(res) {
		console.log("Processing assign company response " + Std.string(res));
	}
	,getSelectListEventStream: function() {
		return this.selectListEventStream;
	}
	,clearFields: function(incomingMessage) {
		this.getCompanyNameElement().value = "";
		this.getCompanyIDElement().value = "";
		this.getCompanyMailboxElement().value = "";
		this.getCompanySplashElement().src = "";
	}
	,getCompanySplashImageString: function() {
		try {
			var imageSplash = this.getCompanySplashElement();
			return imageSplash.src;
		} catch( error ) {
			throw error;
		}
	}
	,createCompany: function(incomingMessage) {
		var result = new model.Company(incomingMessage.company.companyName,incomingMessage.company.companyID,incomingMessage.company.generalMailbox,incomingMessage.company.image);
		return result;
	}
	,copyIncomingValues: function(incomingMessage) {
		try {
			this.getCompanyNameElement().value = incomingMessage.company.companyName;
			this.getCompanyIDElement().value = incomingMessage.company.companyID;
			this.getCompanyMailboxElement().value = incomingMessage.company.generalMailbox;
			var imageSplash = this.getCompanySplashElement();
			imageSplash.src = incomingMessage.company.companyImage;
		} catch( error ) {
			throw error;
		}
	}
	,processManageCompany: function(incomingMessage) {
		console.log("Process manage company ");
		var crudType = incomingMessage.crudType;
		console.log(incomingMessage);
		if(crudType == "Create") {
			console.log("Create successful");
			this.copyIncomingValues(incomingMessage);
		} else if(crudType == "Read") {
			if(incomingMessage.company.companyID == "") this.newCompany = true; else this.newCompany = false;
			this.copyIncomingValues(incomingMessage);
			MBooks_im.getSingleton().activeCompanyStream.resolve(this.createCompany(incomingMessage));
		} else if(crudType == "C_Update") this.copyIncomingValues(incomingMessage); else if(crudType == "Delete") this.clearFields(incomingMessage); else throw "Invalid crudtype " + crudType;
	}
	,chkCompanyExists: function(ev) {
		console.log("Chk company exists " + ev.keyCode);
		if(util.Util.isSignificantWS(ev.keyCode)) try {
			this.read(this.getCompanyID());
		} catch( err ) {
			console.log("Error checking company " + Std.string(err));
		}
	}
	,read: function(companyID) {
		try {
			var nickName = MBooks_im.getSingleton().getNickName();
			var payload = this.getPayload(nickName,"Read","",companyID,"","","");
			MBooks_im.getSingleton().doSendJSON(payload);
		} catch( err ) {
			console.log("Error checking company " + Std.string(err));
		}
	}
	,saveCompanyInfo: function(encodedString) {
		console.log("Saving company info");
		var companyName = this.getCompanyName();
		var companyID = this.getCompanyID();
		var companyMailbox = this.getCompanyMailbox();
		var imageSplash = this.getCompanySplashElement();
		var imageEncoded = encodedString;
		var nickName = MBooks_im.getSingleton().getNickName();
		var crud = "";
		if(this.newCompany) crud = "Create"; else crud = "C_Update";
		var payload = this.getPayload(nickName,crud,companyName,companyID,companyMailbox,imageEncoded,nickName);
		MBooks_im.getSingleton().doSendJSON(payload);
	}
	,loadImage: function(ev) {
		console.log("Load image");
		try {
			var reader = ev.target;
			this.saveCompanyInfo(reader.result);
		} catch( e ) {
			console.log("Exception " + Std.string(e));
		}
	}
	,getPayload: function(nickName,crudType,companyName,companyID,companyMailbox,companyImage1,updatedBy) {
		var payload = { nickName : nickName, commandType : "ManageCompany", crudType : crudType, company : { companyName : companyName, companyID : companyID, generalMailbox : companyMailbox, companyImage : companyImage1, updatedBy : nickName}};
		return payload;
	}
	,selectAllCompanies: function(loggedInMessage) {
		console.log("Processing select all companies " + loggedInMessage);
		var payload = { nickName : MBooks_im.getSingleton().getNickName(), commandType : "SelectAllCompanies"};
		MBooks_im.getSingleton().doSendJSON(payload);
	}
	,saveButtonPressed: function(ev) {
		console.log("Save button pressed");
		var file = this.getCompanyImageElement().files[0];
		if(file != null) {
			var reader = new FileReader();
			var stream_1 = MBooks_im.getSingleton().initializeElementStream(reader,"load");
			stream_1.then($bind(this,this.loadImage));
			reader.readAsDataURL(file);
		} else this.saveCompanyInfo(this.getCompanySplashImageString());
	}
	,assignCompanyToUser: function(ev) {
		console.log("Assigning company to a user:" + Std.string(ev));
		try {
			var payload = { commandType : "AssignCompany", companyID : ev, userName : MBooks_im.getSingleton().getNickName(), isChatMinder : false, isSupport : false, nickName : MBooks_im.getSingleton().getNickName()};
			MBooks_im.getSingleton().doSendJSON(payload);
		} catch( err ) {
			console.log("Error assigning company " + Std.string(ev));
		}
	}
	,showCompanyForm: function() {
		util.Util.showDivField(view.Company.COMPANY_FORM_ID);
	}
	,hideCompanyForm: function() {
		util.Util.hideDivField(view.Company.COMPANY_FORM_ID);
	}
	,getCompanyMailbox: function() {
		return this.getCompanyMailboxElement().value;
	}
	,getCompanyMailboxElement: function() {
		return js.Browser.document.getElementById(view.Company.COMPANY_MAILBOX);
	}
	,getCompanyID: function() {
		return this.getCompanyIDElement().value;
	}
	,getCompanyIDElement: function() {
		return js.Browser.document.getElementById(view.Company.COMPANY_ID);
	}
	,getCompanyName: function() {
		return this.getCompanyNameElement().value;
	}
	,getCompanyNameElement: function() {
		return js.Browser.document.getElementById(view.Company.COMPANY_NAME);
	}
	,getCompanySplashElement: function() {
		return js.Browser.document.getElementById(view.Company.COMPANY_SPLASH_ELEMENT);
	}
	,getCompanyImageElement: function() {
		var fileElement = js.Browser.document.getElementById(view.Company.COMPANY_IMAGE);
		return fileElement;
	}
	,getCompanySignup: function() {
		var buttonElement = js.Browser.document.getElementById(view.Company.SAVE_COMPANY);
		return buttonElement;
	}
	,getAssignCompany: function() {
		var buttonElement = js.Browser.document.getElementById(view.Company.ASSIGN_COMPANY);
		return buttonElement;
	}
	,__class__: view.Company
}
view.CompanyEntitlement = function(view1,companyStream) {
	this.userEntitlementsList = js.Browser.document.getElementById(view.CompanyEntitlement.USER_ENTITLEMENTS);
	view1.queryEntitlementResponse.then($bind(this,this.handleQueryEntitlementResponse));
	this.entitlementsManager = new view.ListManager(this.userEntitlementsList,view.CompanyEntitlement.MANAGE_COMPANY_USER_ENTS,model.Entitlement.optionId,model.Entitlement.listDisplay);
	this.users = js.Browser.document.getElementById(view.CompanyEntitlement.COMPANY_USERS);
	this.userListManager = new view.ListManager(this.users,view.CompanyEntitlement.COMPANY_USERS,model.Person.optionId,model.Person.listDisplay);
	view1.modelResponseStream.then($bind(this,this.handleModelResponse));
	companyStream.then($bind(this,this.getCompanyUsers));
	this.userListResponse = new promhx.Deferred();
	this.userListResponse.then($bind(this,this.handleQueryCompanyUsers));
	this.addUserEntitlement = js.Browser.document.getElementById(view.CompanyEntitlement.ADD_USER_ENTITLEMENTS);
	this.removeUserEntitlement = js.Browser.document.getElementById(view.CompanyEntitlement.REMOVE_USER_ENTITLEMENTS);
};
view.CompanyEntitlement.__name__ = ["view","CompanyEntitlement"];
view.CompanyEntitlement.prototype = {
	updateSelf: function(entitlement) {
		console.log("Updating view " + Std.string(entitlement));
		if(entitlement.crudType == "Delete") this.entitlementsManager["delete"](entitlement); else this.entitlementsManager.upsert(entitlement);
	}
	,handleModelResponse: function(incoming) {
		console.log("handling model response");
		if(incoming == null) {
			MBooks_im.getSingleton().incomingMessageNull("ModelResponse");
			return;
		}
		if(incoming.Left != null) MBooks_im.getSingleton().applicationErrorStream.resolve(incoming); else if(incoming.Right != null) this.updateSelf(incoming.Right);
	}
	,updateEntitlementList: function(queryEntitlement) {
		console.log("Update entitlement list element");
		var _g = 0, _g1 = queryEntitlement.resultSet;
		while(_g < _g1.length) {
			var entitlement = _g1[_g];
			++_g;
			console.log("Adding element to the list." + Std.string(entitlement));
			var stream = this.entitlementsManager.add(entitlement);
		}
	}
	,handleQueryEntitlementResponse: function(incoming) {
		console.log("Query entitlements ");
		if(incoming == null) {
			MBooks_im.getSingleton().incomingMessageNull("QueryEntitlement");
			return;
		}
		if(incoming.Left != null) MBooks_im.getSingleton().applicationErrorStream.resolve(incoming); else if(incoming.Right != null) this.updateEntitlementList(incoming.Right);
	}
	,updateCompanyUsers: function(queryUserResult) {
		console.log("Update company users list");
		var _g = 0, _g1 = queryUserResult.users;
		while(_g < _g1.length) {
			var user = _g1[_g];
			++_g;
			console.log("Adding element to the list." + Std.string(user));
			var stream = this.userListManager.add(user);
		}
	}
	,handleQueryCompanyUsers: function(incoming) {
		console.log("Handle query company users " + Std.string(incoming));
		if(incoming == null) {
			MBooks_im.getSingleton().incomingMessageNull("QueryEntitlement");
			return;
		}
		if(incoming.Left != null) MBooks_im.getSingleton().applicationErrorStream.resolve(incoming); else if(incoming.Right != null) this.updateCompanyUsers(incoming.Right);
	}
	,getCompanyUsers: function(aCompanyId) {
		console.log("Query all company users for " + Std.string(aCompanyId));
		var queryCompanyUsers = { nickName : MBooks_im.getSingleton().getNickName(), commandType : "QueryCompanyUsers", companyID : aCompanyId, users : new Array()};
		MBooks_im.getSingleton().doSendJSON(queryCompanyUsers);
	}
	,removeUserEntitlementF: function(event) {
		console.log("Remove user entitlement " + Std.string(event));
	}
	,addUserEntitlementF: function(event) {
		console.log("Add user entitlements " + Std.string(event));
		var _g = 0, _g1 = this.users;
		while(_g < _g1.length) {
			var userE = _g1[_g];
			++_g;
			var user = userE;
			var _g2 = 0, _g3 = this.userEntitlementsList;
			while(_g2 < _g3.length) {
				var entE = _g3[_g2];
				++_g2;
				var ent = entE;
				console.log("Adding entitlements " + ent.id + " to " + user.id);
				model.CompanyEntitlement.addUserEntitlement(user.id,ent.id);
			}
		}
	}
	,initializeStreams: function() {
		console.log("Adding user entitlement stream");
		var addUserEntitlementStream = MBooks_im.getSingleton().initializeElementStream(this.addUserEntitlement,"click");
		addUserEntitlementStream.then($bind(this,this.addUserEntitlementF));
		var removeUserEntitlementStream = MBooks_im.getSingleton().initializeElementStream(this.removeUserEntitlement,"click");
		removeUserEntitlementStream.then($bind(this,this.removeUserEntitlementF));
		var stream = MBooks_im.getSingleton().initializeElementStream(this.userListManager.listElement,"change");
		stream.then($bind(this,this.handleUserListChange));
		var eStream = MBooks_im.getSingleton().initializeElementStream(this.entitlementsManager.listElement,"change");
		eStream.then($bind(this,this.handleEntitlementsChange));
		((function($this) {
			var $r;
			var varargf = function(f) {
				var ret = new promhx.Stream();
				var arr = [stream,eStream];
				var p = promhx.Stream.wheneverAll(arr);
				p._update.push({ async : ret, linkf : function(x) {
					ret.handleResolve(f(stream._val,eStream._val));
				}});
				return ret;
			};
			$r = { then : varargf};
			return $r;
		}(this))).then($bind(this,this.handleUserEntitlementSelect));
	}
	,handleEntitlementsChange: function(ev) {
		console.log("Event received " + Std.string(ev));
	}
	,handleUserListChange: function(ev) {
		console.log("Event received " + Std.string(ev));
	}
	,handleUserEntitlementSelect: function(userEv,entEv) {
		var userList = userEv.target;
		var entList = entEv.target;
		var _g = 0, _g1 = userList.selectedOptions;
		while(_g < _g1.length) {
			var user = _g1[_g];
			++_g;
			var u = user;
			console.log("User " + u.id + " " + u.text);
			var _g2 = 0, _g3 = entList.selectedOptions;
			while(_g2 < _g3.length) {
				var entE = _g3[_g2];
				++_g2;
				var ent = entE;
				console.log("Ent " + ent.id + " " + ent.text);
			}
		}
	}
	,__class__: view.CompanyEntitlement
}
view.Entitlement = function() {
	console.log("Creating Entitlement view");
	this.tabNameElement = js.Browser.document.getElementById(view.Entitlement.TAB_NAME);
	if(this.tabNameElement == null) throw "Element not found " + view.Entitlement.TAB_NAME;
	this.tabName = this.tabNameElement.value;
	this.sectionNameElement = js.Browser.document.getElementById(view.Entitlement.SECTION_NAME);
	if(this.sectionNameElement == null) throw "Element not found " + view.Entitlement.SECTION_NAME;
	this.entitlementMap = new haxe.ds.StringMap();
	this.sectionName = this.sectionNameElement.value;
	this.textFields = new List();
	this.textFields.add(this.sectionNameElement);
	this.textFields.add(this.tabNameElement);
	this.entitlementsList = js.Browser.document.getElementById(view.Entitlement.ENTITLEMENT_LIST);
	if(this.entitlementsList == null) throw "Element not found  " + view.Entitlement.ENTITLEMENT_LIST;
	this.modelStream = new promhx.Deferred();
	this.view = new promhx.Deferred();
	this.modelResponseStream = new promhx.Deferred();
	this.modelResponseStream.then($bind(this,this.handleModelResponse));
	this.modelObject = new model.Entitlement(this.modelStream);
	this.queryEntitlementResponse = new promhx.Deferred();
	this.queryEntitlementResponse.then($bind(this,this.handleQueryEntitlementResponse));
	this.addEntitlementButton = js.Browser.document.getElementById(view.Entitlement.ADD_ENTITLEMENT);
	this.updateEntitlementButton = js.Browser.document.getElementById(view.Entitlement.UPDATE_ENTITLEMENT);
	this.deleteEntitlementButton = js.Browser.document.getElementById(view.Entitlement.REMOVE_ENTITLEMENT);
	this.setupStreams();
};
view.Entitlement.__name__ = ["view","Entitlement"];
view.Entitlement.prototype = {
	handleEntitlementSelected: function(ev) {
		var element = ev.target;
		var optionElementKey = element.id;
		var entitlement = this.entitlementMap.get(optionElementKey);
		if(entitlement == null) throw "Entitlement not found";
		entitlement.crudType = "Read";
		if(entitlement.nickName == null) entitlement.nickName = MBooks_im.getSingleton().getNickName();
		if(entitlement.commandType == null) entitlement.commandType = view.Entitlement.MANAGE_ENTITLEMENTS_COMMAND;
		this.modelStream.resolve(entitlement);
	}
	,deleteFromEntitlementList: function() {
		try {
			var $it0 = ((function(_e) {
				return function() {
					return _e.iterator();
				};
			})(this.entitlementMap))();
			while( $it0.hasNext() ) {
				var entitlement = $it0.next();
				this.removeElementFromList(this.getOptionElementKey(entitlement));
			}
			this.entitlementMap = new haxe.ds.StringMap();
		} catch( e ) {
			console.log("Exception deleting elements from the list. Restore to previous view." + Std.string(e));
		}
	}
	,removeElementFromList: function(id) {
		var optionElement = js.Browser.document.getElementById(id);
		if(optionElement == null) throw "Nothing to delete " + id;
		optionElement.parentNode.removeChild(optionElement);
		console.log("The above code should most likely work");
		var $it0 = ((function(_e) {
			return function() {
				return _e.iterator();
			};
		})(this.entitlementMap))();
		while( $it0.hasNext() ) {
			var entitlement = $it0.next();
			var optionElementKey = this.getOptionElementKey(entitlement);
			var optionElement1 = js.Browser.document.getElementById(optionElementKey);
			if(optionElement1 != null) {
				optionElement1.selected = true;
				this.updateTextFields(entitlement);
				break;
			}
		}
	}
	,removeFromList: function(entitlement) {
		console.log("Removing element from list");
		var optionElementKey = this.getOptionElementKey(entitlement);
		this.removeElementFromList(optionElementKey);
	}
	,updateList: function(entitlement) {
		console.log("Adding element to list");
		var optionElementKey = this.getOptionElementKey(entitlement);
		var optionElement = js.Browser.document.getElementById(optionElementKey);
		if(optionElement == null) {
			this.entitlementMap.set(optionElementKey,entitlement);
			entitlement;
			optionElement = js.Browser.document.createElement("option");
			optionElement.id = optionElementKey;
			optionElement.text = this.printListText(entitlement);
			var stream = MBooks_im.getSingleton().initializeElementStream(optionElement,"click");
			stream.then($bind(this,this.handleEntitlementSelected));
			this.entitlementsList.appendChild(optionElement);
		} else optionElement.text = this.printListText(entitlement);
		optionElement.selected = true;
	}
	,printListText: function(entitlement) {
		return entitlement.tabName + "->" + entitlement.sectionName;
	}
	,getOptionElementKey: function(entitlement) {
		console.log("Creating an option element key");
		var optionElementKey = view.Entitlement.MANAGE_ENTITLEMENTS_COMMAND + entitlement.tabName + entitlement.sectionName;
		return optionElementKey;
	}
	,updateTextFields: function(entitlement) {
		this.sectionNameElement.value = entitlement.sectionName;
		this.tabNameElement.value = entitlement.tabName;
	}
	,clearTextFields: function() {
		var $it0 = this.textFields.iterator();
		while( $it0.hasNext() ) {
			var i = $it0.next();
			i.value = "";
		}
	}
	,updateIntoView: function(entitlement) {
		this.clearTextFields();
		this.updateList(entitlement);
		this.updateTextFields(entitlement);
	}
	,deleteFromView: function(entitlement) {
		this.clearTextFields();
		this.removeFromList(entitlement);
	}
	,updateSelf: function(entitlement) {
		console.log("Updating view " + Std.string(entitlement));
		if(entitlement.crudType == "Delete") this.deleteFromView(entitlement); else this.updateIntoView(entitlement);
	}
	,updateEntitlementList: function(queryEntitlement) {
		var entitlementList = queryEntitlement.resultSet;
		this.deleteFromEntitlementList();
		var _g = 0;
		while(_g < entitlementList.length) {
			var entitlement = entitlementList[_g];
			++_g;
			this.updateIntoView(entitlement);
		}
	}
	,handleModelResponse: function(incoming) {
		console.log("handling model response");
		if(incoming == null) {
			this.incomingMessageNull("ModelResponse");
			return;
		}
		if(incoming.Left != null) MBooks_im.getSingleton().applicationErrorStream.resolve(incoming); else if(incoming.Right != null) this.updateSelf(incoming.Right);
	}
	,handleQueryEntitlementResponse: function(incoming) {
		if(incoming == null) {
			this.incomingMessageNull("QueryEntitlement");
			return;
		}
		if(incoming.Left != null) MBooks_im.getSingleton().applicationErrorStream.resolve(incoming); else if(incoming.Right != null) this.updateEntitlementList(incoming.Right);
	}
	,incomingMessageNull: function(source) {
		MBooks_im.getSingleton().incomingMessageNull(source);
	}
	,setSectionName: function(aName) {
		this.sectionName = aName;
	}
	,setTabName: function(aTabName) {
		this.tabName = aTabName;
	}
	,deleteEntitlementEvent: function(ev) {
		console.log("Deleting entitlement ");
		var change = this.getModelEntitlement("Delete");
		this.modelStream.resolve(change);
	}
	,updateEntitlementEvent: function(ev) {
		console.log("Update entitlement clicked");
		var change = this.getModelEntitlement("C_Update");
		this.modelStream.resolve(change);
	}
	,addEntitlementEvent: function(ev) {
		console.log("Add entitlement clicked");
		var change = this.getModelEntitlement("Create");
		this.modelStream.resolve(change);
	}
	,getModelEntitlement: function(aCrudType) {
		var change = { crudType : aCrudType, commandType : view.Entitlement.MANAGE_ENTITLEMENTS_COMMAND, tabName : this.tabNameElement.value, sectionName : this.sectionNameElement.value, nickName : MBooks_im.getSingleton().getNickName()};
		return change;
	}
	,setupStreams: function() {
		this.addEntitlementButton.addEventListener("click",$bind(this,this.addEntitlementEvent));
		this.updateEntitlementButton.addEventListener("click",$bind(this,this.updateEntitlementEvent));
		this.deleteEntitlementButton.addEventListener("click",$bind(this,this.deleteEntitlementEvent));
	}
	,queryAllEntitlements: function() {
		this.modelObject.queryAllEntitlements();
	}
	,__class__: view.Entitlement
}
view.ListManager = function(list,idPrefix,opt,listDisplay) {
	this.listElement = list;
	this.prefix = idPrefix;
	this.optionId = opt;
	this.listDisplay = listDisplay;
	this.streamMap = new haxe.ds.StringMap();
	this.modelMap = new haxe.ds.StringMap();
};
view.ListManager.__name__ = ["view","ListManager"];
view.ListManager.prototype = {
	closeStream: function(id) {
		console.log("Closing stream " + id);
		var stream = this.streamMap.get(id);
		if(stream != null) stream.end();
	}
	,removeFromList: function(id) {
		var optionElement = js.Browser.document.getElementById(id);
		if(optionElement == null) {
			this.closeStream(id);
			throw "Nothing to delete " + id;
		}
		optionElement.parentNode.removeChild(optionElement);
	}
	,clear: function() {
		console.log("Clearing list elements");
		var $it0 = ((function(_e) {
			return function() {
				return _e.iterator();
			};
		})(this.modelMap))();
		while( $it0.hasNext() ) {
			var entry = $it0.next();
			this.removeFromList(this.key(entry));
		}
		var $it1 = ((function(_e1) {
			return function() {
				return _e1.iterator();
			};
		})(this.streamMap))();
		while( $it1.hasNext() ) {
			var entry = $it1.next();
			entry.end();
		}
		this.modelMap = new haxe.ds.StringMap();
		this.streamMap = new haxe.ds.StringMap();
	}
	,'delete': function(element) {
		console.log("Deleting element " + Std.string(element));
		var key = this.key(element);
		var model = this.modelMap.get(key);
		this.modelMap.remove(key);
		this.removeFromList(key);
		var stream = this.streamMap.get(key);
		stream.end();
		this.streamMap.remove(key);
	}
	,assertEquals: function(a,b) {
		if(a != b) throw "Assertion failed " + Std.string(a) + " not equal " + Std.string(b);
	}
	,update: function(element) {
		var key = this.key(element);
		var optionElement = js.Browser.document.getElementById(key);
		if(optionElement == null) throw "Element not found " + Std.string(element); else {
			this.assertEquals(optionElement.id,key);
			optionElement.text = this.listDisplay(element);
			optionElement.selected = true;
		}
	}
	,add: function(element) {
		console.log("Adding element " + Std.string(element));
		var optionElement = js.Browser.document.getElementById(this.key(element));
		if(optionElement == null) {
			console.log("Element not found creating a new option");
			optionElement = js.Browser.document.createElement("option");
			optionElement.id = this.key(element);
			optionElement.text = this.listDisplay(element);
			var stream = MBooks_im.getSingleton().initializeElementStream(optionElement,"click");
			this.streamMap.set(this.key(element),stream);
			this.modelMap.set(this.key(element),element);
			this.listElement.appendChild(optionElement);
			optionElement.selected = true;
			return stream;
		}
		return null;
	}
	,upsert: function(element) {
		return this.add(element);
	}
	,key: function(element) {
		return this.prefix + this.optionId(element);
	}
	,__class__: view.ListManager
}
view.Portfolio = function() {
	console.log("Creating new portfolio view");
	this.activePortfolioStream = new promhx.Deferred();
	this.setupEvents();
	this.activePortfolioStream.then($bind(this,this.updateActivePortfolio));
};
view.Portfolio.__name__ = ["view","Portfolio"];
view.Portfolio.prototype = {
	updatePortfolioList: function(portfolioObject) {
		var portfolioList = this.getPortfolioList();
		var portfolioId = portfolioObject.portfolioId;
		var optionElement = js.Browser.document.getElementById(portfolioId);
		if(optionElement == null) {
			optionElement = js.Browser.document.createElement("option");
			optionElement.id = portfolioId;
			optionElement.text = portfolioObject.summary;
			portfolioList.appendChild(optionElement);
		} else optionElement.text = portfolioObject.summary;
	}
	,clearValues: function() {
		this.setPortfolioSummary("");
	}
	,deletePortfolioEntry: function(deleteMe) {
		console.log("Deleting portfolio " + Std.string(deleteMe));
		var optionElement = js.Browser.document.getElementById(deleteMe.portfolioId);
		if(optionElement != null) {
			this.getPortfolioList().removeChild(optionElement);
			this.clearValues();
		} else console.log("Nothing to delete");
	}
	,updatePortfolioEntry: function(update) {
		var optionElement = js.Browser.document.getElementById(update.portfolioId);
		if(optionElement != null) optionElement.selected = true; else throw "Option element for portfolio Id not found " + Std.string(update);
	}
	,portfolioListChanged: function(event) {
		console.log("Portfolio list changed " + Std.string(event));
		var portfolioList = event.target;
		var _g = 0, _g1 = portfolioList.selectedOptions;
		while(_g < _g1.length) {
			var portfolio = _g1[_g];
			++_g;
			var pOption = portfolio;
			if(pOption.text == "--Choose--") this.activePortfolio = null; else this.readPortfolio(pOption.id);
			console.log("Handling " + pOption.id + "->" + pOption.text);
		}
	}
	,getPortfoliosForUser: function() {
		if(this.activeCompany == null) {
			console.log("No company selected");
			return;
		}
		var portfolioQuery = { commandType : "QueryPortfolios", nickName : MBooks_im.getSingleton().getNickName(), companyId : this.activeCompany.companyId, userId : MBooks_im.getSingleton().getNickName(), resultSet : []};
		console.log("Sending " + Std.string(portfolioQuery));
		MBooks_im.getSingleton().doSendJSON(portfolioQuery);
	}
	,copyIncomingValues: function(input) {
		this.setPortfolioSummary(input.summary);
	}
	,processManagePortfolio: function(incomingMessage) {
		console.log("Incoming message manage portfolio " + Std.string(incomingMessage));
		if(incomingMessage.Right != null) {
			this.updatePortfolioList(incomingMessage.Right);
			this.copyIncomingValues(incomingMessage.Right);
			this.activePortfolioStream.resolve(incomingMessage.Right);
			if(incomingMessage.Right.crudType == "Delete") this.deletePortfolioEntry(incomingMessage.Right); else this.updatePortfolioEntry(incomingMessage.Right);
		} else if(incomingMessage.Left != null) MBooks_im.getSingleton().applicationErrorStream.resolve(incomingMessage.Left);
	}
	,processPortfolioList: function(incomingPayload) {
		console.log("Processing portfolio list " + Std.string(incomingPayload));
		var results = incomingPayload.resultSet;
		var _g = 0;
		while(_g < results.length) {
			var p = results[_g];
			++_g;
			if(p.Right != null) this.updatePortfolioList(p.Right); else MBooks_im.getSingleton().applicationErrorStream.resolve(incomingPayload);
		}
	}
	,getDeletePortfolioButton: function() {
		var deleteButton = js.Browser.document.getElementById(view.Portfolio.DELETE_PORTFOLIO);
		return deleteButton;
	}
	,getPortfolioList: function() {
		return js.Browser.document.getElementById(view.Portfolio.PORTFOLIO_LIST_FIELD);
	}
	,getUpdatePortfolioButton: function() {
		var updateButton = js.Browser.document.getElementById(view.Portfolio.UPDATE_PORTFOLIO);
		return updateButton;
	}
	,getSavePortfolioButton: function() {
		var saveButton = js.Browser.document.getElementById(view.Portfolio.SAVE_PORTFOLIO);
		return saveButton;
	}
	,getPortfolioSummaryElement: function() {
		var sumButton = js.Browser.document.getElementById(view.Portfolio.PORTFOLIO_SUMMARY);
		return sumButton;
	}
	,getPortfolioSummary: function() {
		return this.getPortfolioSummaryElement().value;
	}
	,setPortfolioSummary: function(aSummary) {
		this.getPortfolioSummaryElement().value = aSummary;
	}
	,deletePortfolioI: function() {
		var portfolioT = { crudType : "Delete", commandType : "ManagePortfolio", portfolioId : this.activePortfolio.portfolioId, companyId : this.activeCompany.companyId, userId : MBooks_im.getSingleton().getNickName(), summary : this.getPortfolioSummary(), createdBy : MBooks_im.getSingleton().getNickName(), updatedBy : MBooks_im.getSingleton().getNickName(), nickName : MBooks_im.getSingleton().getNickName()};
		MBooks_im.getSingleton().doSendJSON(portfolioT);
	}
	,updatePortfolioI: function() {
		var portfolioT = { crudType : "P_Update", commandType : "ManagePortfolio", portfolioId : this.activePortfolio.portfolioId, companyId : this.activeCompany.companyId, userId : MBooks_im.getSingleton().getNickName(), summary : this.getPortfolioSummary(), createdBy : MBooks_im.getSingleton().getNickName(), updatedBy : MBooks_im.getSingleton().getNickName(), nickName : MBooks_im.getSingleton().getNickName()};
		MBooks_im.getSingleton().doSendJSON(portfolioT);
	}
	,insertPortfolioI: function() {
		var portfolioT = { crudType : "Create", commandType : "ManagePortfolio", portfolioId : "-1", companyId : this.activeCompany.companyId, userId : MBooks_im.getSingleton().getNickName(), summary : this.getPortfolioSummary(), createdBy : MBooks_im.getSingleton().getNickName(), updatedBy : MBooks_im.getSingleton().getNickName(), nickName : MBooks_im.getSingleton().getNickName()};
		MBooks_im.getSingleton().doSendJSON(portfolioT);
	}
	,readPortfolio: function(portfolioId) {
		var portfolioT = { crudType : "Read", commandType : "ManagePortfolio", portfolioId : portfolioId, companyId : this.activeCompany.companyId, userId : MBooks_im.getSingleton().getNickName(), summary : this.getPortfolioSummary(), createdBy : MBooks_im.getSingleton().getNickName(), updatedBy : MBooks_im.getSingleton().getNickName(), nickName : MBooks_im.getSingleton().getNickName()};
		MBooks_im.getSingleton().doSendJSON(portfolioT);
	}
	,updatePortfolio: function(ev) {
		console.log("Update portfolio " + Std.string(ev));
		if(this.activePortfolio == null) console.log("Selected portfolio null. Not updating"); else this.updatePortfolioI();
	}
	,savePortfolio: function(ev) {
		console.log("Saving portfolio " + Std.string(ev));
		if(this.activePortfolio == null) {
			console.log("Inserting as no active portfolio selected");
			this.insertPortfolioI();
		} else this.updatePortfolioI();
	}
	,deletePortfolio: function(ev) {
		console.log("Delete portfolio " + Std.string(ev));
		this.deletePortfolioI();
	}
	,processActiveCompany: function(selected) {
		console.log("Company selected for portfolio processing " + Std.string(selected));
		this.activeCompany = selected;
		this.getPortfoliosForUser();
	}
	,updateActivePortfolio: function(p) {
		this.activePortfolio = p;
	}
	,setupEvents: function() {
		console.log("Setting up ui events");
		var saveP = MBooks_im.getSingleton().initializeElementStream(this.getSavePortfolioButton(),"click");
		saveP.then($bind(this,this.savePortfolio));
		var updateP = MBooks_im.getSingleton().initializeElementStream(this.getUpdatePortfolioButton(),"click");
		updateP.then($bind(this,this.updatePortfolio));
		var deleteP = MBooks_im.getSingleton().initializeElementStream(this.getDeletePortfolioButton(),"click");
		deleteP.then($bind(this,this.deletePortfolio));
		var portfolioListEvent = MBooks_im.getSingleton().initializeElementStream(this.getPortfolioList(),"change");
		portfolioListEvent.then($bind(this,this.portfolioListChanged));
		MBooks_im.getSingleton().portfolioListStream.then($bind(this,this.processPortfolioList));
		MBooks_im.getSingleton().activeCompanyStream.then($bind(this,this.processActiveCompany));
		MBooks_im.getSingleton().portfolioStream.then($bind(this,this.processManagePortfolio));
		this.getPortfoliosForUser();
	}
	,__class__: view.Portfolio
}
view.PortfolioSymbol = function(m) {
	console.log("Instantiating new portfolio symbol view");
	this.model = m;
	this.rowMap = new haxe.ds.StringMap();
	this.setupStreams();
};
view.PortfolioSymbol.__name__ = ["view","PortfolioSymbol"];
view.PortfolioSymbol.prototype = {
	updateMarketData: function(incomingMessage) {
		console.log("Inside update market data response " + Std.string(incomingMessage));
	}
	,handleQueryResponse: function(incomingMessage) {
		console.log("Processing symbol query response " + Std.string(incomingMessage));
		if(incomingMessage.Left != null) MBooks_im.getSingleton().applicationErrorStream.resolve(incomingMessage); else {
			if(incomingMessage.Right.resultSet == null) {
				console.log("Result set is not defined??");
				MBooks_im.getSingleton().applicationErrorStream.resolve(incomingMessage);
				return;
			}
			var pS = incomingMessage.Right;
			var _g = 0, _g1 = pS.resultSet;
			while(_g < _g1.length) {
				var i = _g1[_g];
				++_g;
				if(i.Right != null) this.updateTableRowMap(i.Right); else if(i.Left != null) MBooks_im.getSingleton().applicationErrorStream.resolve(i);
			}
		}
	}
	,manage: function(incomingMessage1) {
		console.log("Manage portfolio symbol " + Std.string(incomingMessage1));
		if(incomingMessage1.Right != null) {
			var incomingMessage = incomingMessage1.Right;
			if(incomingMessage.crudType == "Create") this.insertStreamResponse.resolve(incomingMessage); else if(incomingMessage.crudType == "P_Update") this.updateStreamResponse.resolve(incomingMessage); else if(incomingMessage.crudType == "Read") this.readStreamResponse.resolve(incomingMessage); else if(incomingMessage.crudType == "Delete") this.deleteStreamResponse.resolve(incomingMessage); else throw "Undefined crud type " + Std.string(incomingMessage);
		} else MBooks_im.getSingleton().applicationErrorStream.resolve(incomingMessage1);
	}
	,clearFields: function() {
		this.setQuantityValue("");
		this.setSymbolIdValue("");
	}
	,getSymbolTypeList: function() {
		return js.Browser.document.getElementById(view.PortfolioSymbol.SYMBOL_TYPE_LIST);
	}
	,getSymbolSideList: function() {
		return js.Browser.document.getElementById(view.PortfolioSymbol.SYMBOL_SIDE_LIST);
	}
	,handleSymbolTypeSelected: function(ev) {
		console.log("handle symbol type selected " + Std.string(ev));
	}
	,handleSymbolSideSelected: function(ev) {
		console.log("handle symbol side selected " + Std.string(ev));
	}
	,updateTypesStream: function(symbolType) {
		console.log("Resolving symbol type " + Std.string(symbolType));
		if(symbolType == null) {
			console.log("Invalid symbol type ");
			return;
		}
		var symbolTypeList = this.getSymbolTypeList();
		var optionId = view.PortfolioSymbol.SYMBOL_TYPE_LIST + symbolType.symbolType;
		var optionElement = js.Browser.document.getElementById(optionId);
		if(optionElement == null) {
			optionElement = js.Browser.document.createElement("option");
			optionElement.id = optionId;
			optionElement.text = symbolType.symbolType;
			var stream = MBooks_im.getSingleton().initializeElementStream(optionElement,"click");
			stream.then($bind(this,this.handleSymbolTypeSelected));
			symbolTypeList.appendChild(optionElement);
		}
	}
	,updateSidesStream: function(symbolSide) {
		console.log("Resolving symbol side " + Std.string(symbolSide));
		if(symbolSide == null) {
			console.log("Invalid symbol side ");
			return;
		}
		var symbolSideList = this.getSymbolSideList();
		var optionId = view.PortfolioSymbol.SYMBOL_SIDE_LIST + "_" + symbolSide.symbolSide;
		var optionElement = js.Browser.document.getElementById(optionId);
		if(optionElement == null) {
			optionElement = js.Browser.document.createElement("option");
			optionElement.id = optionId;
			optionElement.text = symbolSide.symbolSide;
			var selectSymbolSideStream = MBooks_im.getSingleton().initializeElementStream(optionElement,"click");
			selectSymbolSideStream.then($bind(this,this.handleSymbolSideSelected));
			symbolSideList.appendChild(optionElement);
		}
	}
	,readPortfolio: function(someEvent) {
		var portfolioSymbolT = { crudType : "Delete", commandType : "ManagePortfolioSymbol", portfolioId : "getPortfolioId()", symbol : this.getSymbolIdValue(), quantity : this.getQuantityValue(), side : this.getSymbolSideValue(), symbolType : this.getSymbolTypeValue(), value : "", stressValue : "0.0", creator : MBooks_im.getSingleton().getNickName(), updator : MBooks_im.getSingleton().getNickName(), nickName : MBooks_im.getSingleton().getNickName()};
		this.model.readStream.resolve(portfolioSymbolT);
	}
	,deletePortfolioSymbol: function(ev) {
		console.log("Delete portfolio symbol " + Std.string(ev));
		var portfolioSymbolT = { crudType : "Delete", commandType : "ManagePortfolioSymbol", portfolioId : this.getPortfolioId(), symbol : this.getSymbolIdValue(), quantity : this.getQuantityValue(), side : this.getSymbolSideValue(), symbolType : this.getSymbolTypeValue(), value : "0.0", stressValue : "0.0", creator : MBooks_im.getSingleton().getNickName(), updator : MBooks_im.getSingleton().getNickName(), nickName : MBooks_im.getSingleton().getNickName()};
		this.model.deleteStream.resolve(portfolioSymbolT);
	}
	,updatePortfolioSymbol: function(ev) {
		console.log("Update portfolio symbol " + Std.string(ev));
		var portfolioSymbolT = { crudType : "P_Update", commandType : "ManagePortfolioSymbol", portfolioId : this.getPortfolioId(), symbol : this.getSymbolIdValue(), quantity : this.getQuantityValue(), side : this.getSymbolSideValue(), symbolType : this.getSymbolTypeValue(), value : "0.0", stressValue : "0.0", creator : MBooks_im.getSingleton().getNickName(), updator : MBooks_im.getSingleton().getNickName(), nickName : MBooks_im.getSingleton().getNickName()};
		this.model.updateStream.resolve(portfolioSymbolT);
	}
	,insertPortfolioSymbol: function(ev) {
		console.log("Insert portfolio symbol " + Std.string(ev));
		console.log("Symbol side " + this.getSymbolSideValue());
		console.log("Symbol type " + this.getSymbolTypeValue());
		var portfolioSymbolT = { crudType : "Create", commandType : "ManagePortfolioSymbol", portfolioId : this.getPortfolioId(), symbol : this.getSymbolIdValue(), quantity : this.getQuantityValue(), side : this.getSymbolSideValue(), symbolType : this.getSymbolTypeValue(), value : "0.0", stressValue : "0.0", creator : MBooks_im.getSingleton().getNickName(), updator : MBooks_im.getSingleton().getNickName(), nickName : MBooks_im.getSingleton().getNickName()};
		this.model.insertStream.resolve(portfolioSymbolT);
	}
	,insertPortfolioSymbolI: function(aSymbol,aSymbolType,aSide,quantity) {
		console.log("Inserting portfolio symbol through upload ");
		var portfolioSymbolT = { crudType : "Create", commandType : "ManagePortfolioSymbol", portfolioId : this.getPortfolioId(), symbol : aSymbol, quantity : quantity, side : aSide, symbolType : aSymbolType, value : "0.0", stressValue : "0.0", creator : MBooks_im.getSingleton().getNickName(), updator : MBooks_im.getSingleton().getNickName(), nickName : MBooks_im.getSingleton().getNickName()};
		this.model.insertStream.resolve(portfolioSymbolT);
	}
	,getPortfolioId: function() {
		if(this.model == null) throw "Model not defined";
		return this.model.activePortfolio.portfolioId;
	}
	,readResponse: function(payload) {
		console.log("Reading view " + Std.string(payload));
		throw "Read response Not implemented";
	}
	,deleteResponse: function(payload) {
		console.log("Deleting view " + Std.string(payload));
		this.deleteTableRowMap(payload);
	}
	,updateResponse: function(payload) {
		console.log("Updating view " + Std.string(payload));
		this.updateTableRowMap(payload);
	}
	,insertResponse: function(payload) {
		console.log("Inserting view " + Std.string(payload));
		this.updateTableRowMap(payload);
	}
	,insertCells: function(aRow,payload) {
		if(payload.portfolioId != MBooks_im.getSingleton().portfolio.activePortfolio.portfolioId) {
			console.log("Throwing away payload " + Std.string(payload));
			return;
		}
		console.log("Inserting cells from payload " + Std.string(payload));
		var newCell = aRow.insertCell(0);
		newCell.innerHTML = payload.symbol;
		newCell = aRow.insertCell(1);
		newCell.innerHTML = payload.side;
		newCell = aRow.insertCell(2);
		newCell.innerHTML = payload.symbolType;
		newCell = aRow.insertCell(3);
		newCell.innerHTML = payload.quantity;
		newCell = aRow.insertCell(4);
		newCell.innerHTML = payload.value;
		newCell = aRow.insertCell(5);
		newCell.innerHTML = payload.stressValue;
		newCell = aRow.insertCell(6);
		newCell.innerHTML = HxOverrides.dateStr(new Date());
	}
	,updateTableRowMap: function(payload) {
		var key = this.getKey(payload);
		var row = this.rowMap.get(key);
		if(payload.portfolioId != MBooks_im.getSingleton().portfolio.activePortfolio.portfolioId) {
			console.log("Throwing message " + Std.string(payload));
			return;
		}
		if(row == null) {
			var pSymbolTable = this.getPortfolioSymbolTable();
			row = pSymbolTable.insertRow(this.computeInsertIndex());
			this.rowMap.set(key,row);
			this.insertCells(row,payload);
		} else {
			var cells = row.children;
			var _g = 0;
			while(_g < cells.length) {
				var cell = cells[_g];
				++_g;
				var cellI = cell;
				var cellIndex = cellI.cellIndex;
				switch(cellIndex) {
				case 0:
					cellI.innerHTML = payload.symbol;
					break;
				case 1:
					cellI.innerHTML = payload.side;
					break;
				case 2:
					cellI.innerHTML = payload.symbolType;
					break;
				case 3:
					cellI.innerHTML = payload.quantity;
					break;
				case 4:
					cellI.innerHTML = payload.value;
					break;
				case 5:
					cellI.innerHTML = payload.stressValue;
					break;
				case 6:
					cellI.innerHTML = HxOverrides.dateStr(new Date());
					break;
				}
			}
		}
	}
	,deleteTableRowMap: function(payload) {
		console.log("Deleting table row map " + Std.string(payload));
		var key = this.getKey(payload);
		console.log("Deleting key " + key);
		if(!this.rowMap.exists(key)) {
			console.log("Nothing to delete " + Std.string(payload));
			return;
		}
		var row = this.rowMap.get(key);
		this.rowMap.remove(key);
		var pSymbolTable = this.getPortfolioSymbolTable();
		pSymbolTable.deleteRow(row.rowIndex);
	}
	,processActivePortfolio: function(a) {
		console.log("Deleting all the existing rows as active portfolio changed " + Std.string(a));
		var $it0 = this.rowMap.keys();
		while( $it0.hasNext() ) {
			var key = $it0.next();
			console.log("Deleting key " + key);
			var row = this.rowMap.get(key);
			var pSymbolTable = this.getPortfolioSymbolTable();
			pSymbolTable.deleteRow(row.rowIndex);
		}
		this.rowMap = new haxe.ds.StringMap();
	}
	,getKey: function(payload) {
		if(payload == null) throw "Get failed. No payload";
		return payload.symbol + payload.side + payload.symbolType + payload.portfolioId;
	}
	,computeInsertIndex: function() {
		return 1;
	}
	,processFileUpload: function(ev) {
		console.log("Processing file upload ");
		try {
			var reader = ev.target;
			console.log("Reading");
			this.parsePortfolioDetails(reader.result);
			console.log("Read");
		} catch( e ) {
			console.log("Exception " + Std.string(e));
		}
	}
	,parsePortfolioDetails: function(fileContents) {
		console.log("Parsing portfolio details");
		var portfolioDetails = format.csv.Reader.parseCsv(fileContents);
		var headerRead = false;
		var _g = 0;
		while(_g < portfolioDetails.length) {
			var aRecord = portfolioDetails[_g];
			++_g;
			var a = aRecord;
			if(a.length == 4) {
				if(headerRead == true) this.insertPortfolioSymbolI(StringTools.trim(a[0]),StringTools.trim(a[1]),StringTools.trim(a[2]),StringTools.trim(a[3])); else console.log("Skipping " + Std.string(aRecord));
			} else console.log("Invalid record length " + Std.string(aRecord));
			headerRead = true;
		}
	}
	,uploadPortfolio: function(ev) {
		console.log("Save button pressed");
		var files = this.getUploadPortfolioFile().files;
		var _g = 0;
		while(_g < files.length) {
			var file = files[_g];
			++_g;
			var reader = new FileReader();
			var stream_1 = MBooks_im.getSingleton().initializeElementStream(reader,"load");
			stream_1.then($bind(this,this.processFileUpload));
			reader.readAsText(file);
		}
	}
	,setupStreams: function() {
		this.model.sideStream.then($bind(this,this.updateSidesStream));
		this.model.typeStream.then($bind(this,this.updateTypesStream));
		var deleteP = MBooks_im.getSingleton().initializeElementStream(this.getDeletePortfolioSymbolButton(),"click");
		deleteP.then($bind(this,this.deletePortfolioSymbol));
		var updateP = MBooks_im.getSingleton().initializeElementStream(this.getUpdatePortfolioSymbolButton(),"click");
		updateP.then($bind(this,this.updatePortfolioSymbol));
		var insertP = MBooks_im.getSingleton().initializeElementStream(this.getInsertPortfolioSymbolButton(),"click");
		insertP.then($bind(this,this.insertPortfolioSymbol));
		this.insertStreamResponse = new promhx.Deferred();
		this.updateStreamResponse = new promhx.Deferred();
		this.deleteStreamResponse = new promhx.Deferred();
		this.readStreamResponse = new promhx.Deferred();
		this.insertStreamResponse.then($bind(this,this.insertResponse));
		this.updateStreamResponse.then($bind(this,this.updateResponse));
		this.deleteStreamResponse.then($bind(this,this.deleteResponse));
		this.readStreamResponse.then($bind(this,this.readResponse));
		this.symbolQueryResponse = new promhx.Deferred();
		this.symbolQueryResponse.then($bind(this,this.handleQueryResponse));
		MBooks_im.getSingleton().marketDataStream.then($bind(this,this.updateMarketData));
		MBooks_im.getSingleton().portfolio.activePortfolioStream.then($bind(this,this.processActivePortfolio));
		var uploadPortfolioButtonStream = MBooks_im.getSingleton().initializeElementStream(this.getUploadPortfolioButton(),"click");
		uploadPortfolioButtonStream.then($bind(this,this.uploadPortfolio));
	}
	,getSymbolSideValue: function() {
		var multiSelect = false;
		return this.getSelectedOptionElement(this.getSymbolSideElement(),multiSelect);
	}
	,getSymbolTypeValue: function() {
		var multiSelect = false;
		return this.getSelectedOptionElement(this.getSymbolTypeElement(),multiSelect);
	}
	,getSelectedOptionElement: function(inputList,multiSelect) {
		var selectedOptions = inputList.selectedOptions;
		if(multiSelect) {
			console.log("Multiple selection true. What can we do here?");
			throw "Multiple selection list not supported for this method";
		} else {
			var optionElement = selectedOptions.item(0);
			return optionElement.text;
		}
	}
	,getSymbolSideElement: function() {
		return js.Browser.document.getElementById(view.PortfolioSymbol.SYMBOL_SIDE_LIST);
	}
	,getSymbolTypeElement: function() {
		return js.Browser.document.getElementById(view.PortfolioSymbol.SYMBOL_TYPE_LIST);
	}
	,setSymbolIdValue: function(anId) {
		this.getSymbolIdElement().value = anId;
	}
	,getSymbolIdValue: function() {
		return this.getSymbolIdElement().value;
	}
	,getSymbolIdElement: function() {
		return js.Browser.document.getElementById(view.PortfolioSymbol.SYMBOL_ID_FIELD);
	}
	,setQuantityValue: function(aValue) {
		this.getQuantityValueElement().value = "";
	}
	,getQuantityValue: function() {
		return this.getQuantityValueElement().value;
	}
	,getQuantityValueElement: function() {
		return js.Browser.document.getElementById(view.PortfolioSymbol.SYMBOL_QUANTITY_ID);
	}
	,getInsertPortfolioSymbolButton: function() {
		return js.Browser.document.getElementById(view.PortfolioSymbol.SAVE_SYMBOL_BUTTON);
	}
	,getUpdatePortfolioSymbolButton: function() {
		return js.Browser.document.getElementById(view.PortfolioSymbol.UPDATE_SYMBOL_BUTTON);
	}
	,getDeletePortfolioSymbolButton: function() {
		return js.Browser.document.getElementById(view.PortfolioSymbol.DELETE_SYMBOL_BUTTON);
	}
	,getPortfolioSymbolTable: function() {
		return js.Browser.document.getElementById(view.PortfolioSymbol.PORTFOLIO_SYMBOL_TABLE);
	}
	,getUploadPortfolioButton: function() {
		return js.Browser.document.getElementById(view.PortfolioSymbol.UPLOAD_PORTFOLIO_BUTTON);
	}
	,getUploadPortfolioFile: function() {
		return js.Browser.document.getElementById(view.PortfolioSymbol.UPLOAD_PORTFOLIO_FILE);
	}
	,__class__: view.PortfolioSymbol
}
function $iterator(o) { if( o instanceof Array ) return function() { return HxOverrides.iter(o); }; return typeof(o.iterator) == 'function' ? $bind(o,o.iterator) : o.iterator; };
var $_, $fid = 0;
function $bind(o,m) { if( m == null ) return null; if( m.__id__ == null ) m.__id__ = $fid++; var f; if( o.hx__closures__ == null ) o.hx__closures__ = {}; else f = o.hx__closures__[m.__id__]; if( f == null ) { f = function(){ return f.method.apply(f.scope, arguments); }; f.scope = o; f.method = m; o.hx__closures__[m.__id__] = f; } return f; };
Math.__name__ = ["Math"];
Math.NaN = Number.NaN;
Math.NEGATIVE_INFINITY = Number.NEGATIVE_INFINITY;
Math.POSITIVE_INFINITY = Number.POSITIVE_INFINITY;
Math.isFinite = function(i) {
	return isFinite(i);
};
Math.isNaN = function(i) {
	return isNaN(i);
};
String.prototype.__class__ = String;
String.__name__ = ["String"];
Array.prototype.__class__ = Array;
Array.__name__ = ["Array"];
Date.prototype.__class__ = Date;
Date.__name__ = ["Date"];
var Int = { __name__ : ["Int"]};
var Dynamic = { __name__ : ["Dynamic"]};
var Float = Number;
Float.__name__ = ["Float"];
var Bool = Boolean;
Bool.__ename__ = ["Bool"];
var Class = { __name__ : ["Class"]};
var Enum = { };
if(typeof(JSON) != "undefined") haxe.Json = JSON;
var global = window;
(function (global, undefined) {
    "use strict";

    var tasks = (function () {
        function Task(handler, args) {
            this.handler = handler;
            this.args = args;
        }
        Task.prototype.run = function () {
            // See steps in section 5 of the spec.
            if (typeof this.handler === "function") {
                // Choice of `thisArg` is not in the setImmediate spec; `undefined` is in the setTimeout spec though:
                // http://www.whatwg.org/specs/web-apps/current-work/multipage/timers.html
                this.handler.apply(undefined, this.args);
            } else {
                var scriptSource = "" + this.handler;
                /*jshint evil: true */
                eval(scriptSource);
            }
        };

        var nextHandle = 1; // Spec says greater than zero
        var tasksByHandle = {};
        var currentlyRunningATask = false;

        return {
            addFromSetImmediateArguments: function (args) {
                var handler = args[0];
                var argsToHandle = Array.prototype.slice.call(args, 1);
                var task = new Task(handler, argsToHandle);

                var thisHandle = nextHandle++;
                tasksByHandle[thisHandle] = task;
                return thisHandle;
            },
            runIfPresent: function (handle) {
                // From the spec: "Wait until any invocations of this algorithm started before this one have completed."
                // So if we're currently running a task, we'll need to delay this invocation.
                if (!currentlyRunningATask) {
                    var task = tasksByHandle[handle];
                    if (task) {
                        currentlyRunningATask = true;
                        try {
                            task.run();
                        } finally {
                            delete tasksByHandle[handle];
                            currentlyRunningATask = false;
                        }
                    }
                } else {
                    // Delay by doing a setTimeout. setImmediate was tried instead, but in Firefox 7 it generated a
                    // "too much recursion" error.
                    global.setTimeout(function () {
                        tasks.runIfPresent(handle);
                    }, 0);
                }
            },
            remove: function (handle) {
                delete tasksByHandle[handle];
            }
        };
    }());

    function canUseNextTick() {
        // Don't get fooled by e.g. browserify environments.
        return typeof process === "object" &&
               Object.prototype.toString.call(process) === "[object process]";
    }

    function canUseMessageChannel() {
        return !!global.MessageChannel;
    }

    function canUsePostMessage() {
        // The test against `importScripts` prevents this implementation from being installed inside a web worker,
        // where `global.postMessage` means something completely different and can't be used for this purpose.

        if (!global.postMessage || global.importScripts) {
            return false;
        }

        var postMessageIsAsynchronous = true;
        var oldOnMessage = global.onmessage;
        global.onmessage = function () {
            postMessageIsAsynchronous = false;
        };
        global.postMessage("", "*");
        global.onmessage = oldOnMessage;

        return postMessageIsAsynchronous;
    }

    function canUseReadyStateChange() {
        return "document" in global && "onreadystatechange" in global.document.createElement("script");
    }

    function installNextTickImplementation(attachTo) {
        attachTo.setImmediate = function () {
            var handle = tasks.addFromSetImmediateArguments(arguments);

            process.nextTick(function () {
                tasks.runIfPresent(handle);
            });

            return handle;
        };
    }

    function installMessageChannelImplementation(attachTo) {
        var channel = new global.MessageChannel();
        channel.port1.onmessage = function (event) {
            var handle = event.data;
            tasks.runIfPresent(handle);
        };
        attachTo.setImmediate = function () {
            var handle = tasks.addFromSetImmediateArguments(arguments);

            channel.port2.postMessage(handle);

            return handle;
        };
    }

    function installPostMessageImplementation(attachTo) {
        // Installs an event handler on `global` for the `message` event: see
        // * https://developer.mozilla.org/en/DOM/window.postMessage
        // * http://www.whatwg.org/specs/web-apps/current-work/multipage/comms.html#crossDocumentMessages

        var MESSAGE_PREFIX = "com.bn.NobleJS.setImmediate" + Math.random();

        function isStringAndStartsWith(string, putativeStart) {
            return typeof string === "string" && string.substring(0, putativeStart.length) === putativeStart;
        }

        function onGlobalMessage(event) {
            // This will catch all incoming messages (even from other windows!), so we need to try reasonably hard to
            // avoid letting anyone else trick us into firing off. We test the origin is still this window, and that a
            // (randomly generated) unpredictable identifying prefix is present.
            if (event.source === global && isStringAndStartsWith(event.data, MESSAGE_PREFIX)) {
                var handle = event.data.substring(MESSAGE_PREFIX.length);
                tasks.runIfPresent(handle);
            }
        }
        if (global.addEventListener) {
            global.addEventListener("message", onGlobalMessage, false);
        } else {
            global.attachEvent("onmessage", onGlobalMessage);
        }

        attachTo.setImmediate = function () {
            var handle = tasks.addFromSetImmediateArguments(arguments);

            // Make `global` post a message to itself with the handle and identifying prefix, thus asynchronously
            // invoking our onGlobalMessage listener above.
            global.postMessage(MESSAGE_PREFIX + handle, "*");

            return handle;
        };
    }

    function installReadyStateChangeImplementation(attachTo) {
        attachTo.setImmediate = function () {
            var handle = tasks.addFromSetImmediateArguments(arguments);

            // Create a <script> element; its readystatechange event will be fired asynchronously once it is inserted
            // into the document. Do so, thus queuing up the task. Remember to clean up once it's been called.
            var scriptEl = global.document.createElement("script");
            scriptEl.onreadystatechange = function () {
                tasks.runIfPresent(handle);

                scriptEl.onreadystatechange = null;
                scriptEl.parentNode.removeChild(scriptEl);
                scriptEl = null;
            };
            global.document.documentElement.appendChild(scriptEl);

            return handle;
        };
    }

    function installSetTimeoutImplementation(attachTo) {
        attachTo.setImmediate = function () {
            var handle = tasks.addFromSetImmediateArguments(arguments);

            global.setTimeout(function () {
                tasks.runIfPresent(handle);
            }, 0);

            return handle;
        };
    }

    if (!global.setImmediate) {
        // If supported, we should attach to the prototype of global, since that is where setTimeout et al. live.
        var attachTo = typeof Object.getPrototypeOf === "function" && "setTimeout" in Object.getPrototypeOf(global) ?
                          Object.getPrototypeOf(global)
                        : global;

        if (canUseNextTick()) {
            // For Node.js before 0.9
            installNextTickImplementation(attachTo);
        } else if (canUsePostMessage()) {
            // For non-IE10 modern browsers
            installPostMessageImplementation(attachTo);
        } else if (canUseMessageChannel()) {
            // For web workers, where supported
            installMessageChannelImplementation(attachTo);
        } else if (canUseReadyStateChange()) {
            // For IE 6–8
            installReadyStateChangeImplementation(attachTo);
        } else {
            // For older browsers
            installSetTimeoutImplementation(attachTo);
        }

        attachTo.clearImmediate = tasks.remove;
    }
}(typeof global === "object" && global ? global : this));
;
MBooks_im.MESSAGING_DIV = "workbench-messaging";
MBooks_im.GENERAL_DIV = "workbench-general";
MBooks_im.COMPANY_DIV = "workbench-company";
MBooks_im.PROJECT_DIV = "workbench-project";
MBooks_im.CCAR_DIV = "workbench-ccar";
MBooks_im.SECURITY_DIV = "workbench-security";
MBooks_im.PORTFOLIO_DIV = "workbench-portfolio";
MBooks_im.SETUP_GMAIL = "setupGmailOauth";
MBooks_im.NICK_NAME = "nickName";
MBooks_im.PASSWORD = "password";
MBooks_im.FIRST_NAME = "firstName";
MBooks_im.LAST_NAME = "lastName";
MBooks_im.DIV_PASSWORD = "passwordDiv";
MBooks_im.DIV_FIRST_NAME = "firstNameDiv";
MBooks_im.DIV_LAST_NAME = "lastNameDiv";
MBooks_im.DIV_REGISTER = "registerDiv";
MBooks_im.USERS_ONLINE = "usersOnline";
MBooks_im.REGISTER = "registerInput";
MBooks_im.MESSAGE_HISTORY = "messageHistory";
MBooks_im.MESSAGE_INPUT = "messageInput";
MBooks_im.STATUS_MESSAGE = "statusMessage";
MBooks_im.KICK_USER = "kickUser";
MBooks_im.KICK_USER_DIV = "kickUserDiv";
MBooks_im.INIT_WELCOME_MESSAGE_DIV = "initWelcomeMessageDiv";
MBooks_im.INIT_WELCOME_MESSAGE = "initWelcomeMessage";
MBooks_im.GOAUTH_URL = "gmail_oauthrequest";
MBooks_im.SERVER_ERROR = "serverError";
MBooks_im.APPLICATION_ERROR = "applicationError";
format.csv.Reader.FETCH_SIZE = 4096;
js.Browser.document = typeof window != "undefined" ? window.document : null;
js.Browser.location = typeof window != "undefined" ? window.location : null;
js.d3._D3.InitPriority.important = "important";
massive.munit.Assert.assertionCount = 0;
massive.munit.TestClassHelper.META_TAG_BEFORE_CLASS = "BeforeClass";
massive.munit.TestClassHelper.META_TAG_AFTER_CLASS = "AfterClass";
massive.munit.TestClassHelper.META_TAG_BEFORE = "Before";
massive.munit.TestClassHelper.META_TAG_AFTER = "After";
massive.munit.TestClassHelper.META_TAG_TEST = "Test";
massive.munit.TestClassHelper.META_TAG_ASYNC_TEST = "AsyncTest";
massive.munit.TestClassHelper.META_TAG_IGNORE = "Ignore";
massive.munit.TestClassHelper.META_PARAM_ASYNC_TEST = "Async";
massive.munit.TestClassHelper.META_TAG_TEST_DEBUG = "TestDebug";
massive.munit.TestClassHelper.META_TAGS = ["BeforeClass","AfterClass","Before","After","Test","AsyncTest","TestDebug"];
massive.munit.async.AsyncDelegate.DEFAULT_TIMEOUT = 400;
massive.munit.util.Timer.arr = new Array();
model.CCAR.SCENARIO_NAME = "scenarioName";
model.CCAR.SCENARIO_TEXT = "scenarioText";
model.CCAR.SAVE_SCENARIO = "saveScenario";
model.CCAR.PARSED_SCENARIO = "parsedScenario";
model.Project.SAVE_PROJECT = "saveProject";
model.Project.DELETE_PROJECT = "deleteProject";
model.Project.MANAGE_PROJECT = "ManageProject";
model.Project.PROJECT_IDENTIFICATION = "projectIdentification";
model.Project.COMPANY_LIST = "companyList";
model.Project.PROJECT_START = "projectStart";
model.Project.PROJECT_END = "projectEnd";
model.Project.PREPARED_BY = "preparedBy";
model.Project.PROJECT_SUMMARY = "projectSummary";
model.Project.PROJECT_DETAILS = "projectDetails";
model.Project.PROJECT_LIST = "projectList";
model.Project.CREATE = "Create";
model.Project.UPDATE = "P_Update";
model.Project.DELETE = "Delete";
model.Project.READ = "Read";
promhx.base.EventLoop.queue = new List();
util.Config.companyKey = "sbr";
util.Util.DEFAULT_ROWS = 10;
util.Util.DEFAULT_COLS = 50;
util.Util.BACKSPACE = 8;
util.Util.UP_ARROW = 38;
util.Util.DOWN_ARROW = 40;
util.Util.LABEL = "LABEL_";
util.Util.DIV = "DIV_";
view.Company.SAVE_COMPANY = "saveCompany";
view.Company.DELETE_COMPANY = "deleteCompany";
view.Company.COMPANY_IMAGE = "companyImage";
view.Company.COMPANY_SPLASH_ELEMENT = "companySplash";
view.Company.COMPANY_NAME = "companyName";
view.Company.COMPANY_ID = "companyID";
view.Company.COMPANY_MAILBOX = "generalMailbox";
view.Company.COMPANY_FORM_ID = "companyForm";
view.Company.ASSIGN_COMPANY = "assignCompany";
view.CompanyEntitlement.MANAGE_ALL_USER_ENTS = "allUserEntitlements";
view.CompanyEntitlement.MANAGE_COMPANY_USER_ENTS = "companyUserEntitlements";
view.CompanyEntitlement.SEARCH_USER_ELEMENT = "searchUsers";
view.CompanyEntitlement.COMPANY_USERS = "companyUsers";
view.CompanyEntitlement.PENDING_APPROVAL_REQUESTS = "pendingApprovalRequests";
view.CompanyEntitlement.AVAILABLE_ENTITLEMENTS = "availableEntitlements";
view.CompanyEntitlement.USER_ENTITLEMENTS = "userEntitlements";
view.CompanyEntitlement.ADD_USER_ENTITLEMENTS = "addUserEntitlements";
view.CompanyEntitlement.REMOVE_USER_ENTITLEMENTS = "removeUserEntitlements";
view.Entitlement.TAB_NAME = "entitlementTabName";
view.Entitlement.SECTION_NAME = "entitlementSectionName";
view.Entitlement.ENTITLEMENT_LIST = "entitlementsList";
view.Entitlement.ADD_ENTITLEMENT = "addEntitlement";
view.Entitlement.UPDATE_ENTITLEMENT = "updateEntitlement";
view.Entitlement.REMOVE_ENTITLEMENT = "removeEntitlement";
view.Entitlement.MANAGE_ENTITLEMENTS_COMMAND = "ManageEntitlements";
view.Portfolio.SAVE_PORTFOLIO = "savePortfolio";
view.Portfolio.UPDATE_PORTFOLIO = "updatePortfolio";
view.Portfolio.DELETE_PORTFOLIO = "deletePortfolio";
view.Portfolio.SYMBOL_INPUT_FIELD = "portfolioSymbol";
view.Portfolio.SIDE_INPUT_FIELD = "portfolioSide";
view.Portfolio.QUANTITY_INPUT_FIELD = "portfolioQuantity";
view.Portfolio.PORTFOLIO_LIST_FIELD = "portfolioList";
view.Portfolio.PORTFOLIO_SUMMARY = "portfolioSummary";
view.PortfolioSymbol.SYMBOL_SIDE_LIST = "symbolSideID";
view.PortfolioSymbol.SYMBOL_TYPE_LIST = "symbolTypeID";
view.PortfolioSymbol.SYMBOL_ID_FIELD = "symbolID";
view.PortfolioSymbol.SYMBOL_QUANTITY_ID = "symbolQuantityID";
view.PortfolioSymbol.SAVE_SYMBOL_BUTTON = "saveSymbol";
view.PortfolioSymbol.DELETE_SYMBOL_BUTTON = "deleteSymbol";
view.PortfolioSymbol.UPDATE_SYMBOL_BUTTON = "updateSymbol";
view.PortfolioSymbol.PORTFOLIO_SYMBOL_TABLE = "portfolioSymbolTable";
view.PortfolioSymbol.UPLOAD_PORTFOLIO_FILE = "uploadPortfolioFile";
view.PortfolioSymbol.UPLOAD_PORTFOLIO_BUTTON = "uploadPortfolioButton";
MBooks_im.main();
function $hxExpose(src, path) {
	var o = typeof window != "undefined" ? window : exports;
	var parts = path.split(".");
	for(var ii = 0; ii < parts.length-1; ++ii) {
		var p = parts[ii];
		if(typeof o[p] == "undefined") o[p] = {};
		o = o[p];
	}
	o[parts[parts.length-1]] = src;
}
})();
