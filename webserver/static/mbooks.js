(function () { "use strict";
var MBooks = function() {
	this.portNumber = 3000;
	this.protocol = "ws";
	this.serverHost = "localhost";
	this.serverHost = "localhost";
	this.protocol = "ws";
	this.portNumber = 3000;
	this.contact = new model.Contact("test","test","test");
	this.createConnectionForm();
};
MBooks.__name__ = true;
MBooks.main = function() {
	var test = new MBooks();
}
MBooks.prototype = {
	upgradeToServer: function(ev) {
		this.initializeConnection();
	}
	,createConnectionForm: function() {
		try {
			console.log("Creating connection form");
			var document = js.Browser.document;
			var protocol;
			var server;
			var port;
			var div = document.createElement("div");
			var pText = document.createTextNode("Protocol");
			console.log("Creating pText " + Std.string(document));
			div.appendChild(pText);
			console.log("Appending child");
			var sText = document.createElement("input");
			div.appendChild(sText);
			sText.defaultValue = "ws";
			var serverText = document.createTextNode("Server");
			div.appendChild(serverText);
			document.appendChild(div);
			this.connect = document.createElement("button");
			div.appendChild(this.connect);
			this.connect.onclick = $bind(this,this.upgradeToServer);
			document.appendChild(div);
			console.log("Connection form created");
		} catch( msg ) {
			if( js.Boot.__instanceof(msg,DOMException) ) {
				console.log("Exception " + Std.string(msg));
			} else throw(msg);
		}
	}
	,doSend: function(aMessage) {
		this.websocket.send(aMessage);
	}
	,onError: function(ev) {
		console.log("Error " + Std.string(ev));
	}
	,onMessage: function(ev) {
		console.log("Received " + Std.string(ev.data));
	}
	,onOpen: function(ev) {
		console.log("Connection opened");
	}
	,onClose: function(ev) {
		console.log("Connection closed");
	}
	,connectionString: function() {
		return this.protocol + "://" + this.serverHost + ":" + this.portNumber;
	}
	,initializeConnection: function() {
		this.websocket = new WebSocket(this.connectionString());
		this.websocket.onclose = $bind(this,this.onClose);
		this.websocket.onerror = $bind(this,this.onError);
		this.websocket.onmessage = $bind(this,this.onMessage);
		this.websocket.onopen = $bind(this,this.onOpen);
	}
	,__class__: MBooks
}
var Std = function() { }
Std.__name__ = true;
Std.string = function(s) {
	return js.Boot.__string_rec(s,"");
}
var js = {}
js.Boot = function() { }
js.Boot.__name__ = true;
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
js.Browser.__name__ = true;
var model = {}
model.Contact = function(aName,lName,aLogin) {
	this.firstName = aName;
	this.lastName = lName;
	this.login = aLogin;
	console.log("Creating contact with " + aName + "->" + lName + " -> " + aLogin);
};
model.Contact.__name__ = true;
model.Contact.prototype = {
	__class__: model.Contact
}
var $_, $fid = 0;
function $bind(o,m) { if( m == null ) return null; if( m.__id__ == null ) m.__id__ = $fid++; var f; if( o.hx__closures__ == null ) o.hx__closures__ = {}; else f = o.hx__closures__[m.__id__]; if( f == null ) { f = function(){ return f.method.apply(f.scope, arguments); }; f.scope = o; f.method = m; o.hx__closures__[m.__id__] = f; } return f; };
String.prototype.__class__ = String;
String.__name__ = true;
Array.prototype.__class__ = Array;
Array.__name__ = true;
var Int = { __name__ : ["Int"]};
var Dynamic = { __name__ : ["Dynamic"]};
var Float = Number;
Float.__name__ = ["Float"];
var Bool = Boolean;
Bool.__ename__ = ["Bool"];
var Class = { __name__ : ["Class"]};
var Enum = { };
js.Browser.document = typeof window != "undefined" ? window.document : null;
MBooks.main();
})();
