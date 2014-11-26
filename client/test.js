(function () { "use strict";
var Std = function() { }
Std.__name__ = true;
Std.string = function(s) {
	return js.Boot.__string_rec(s,"");
}
var Test = function() {
	this.portNumber = 3000;
	this.protocol = "ws";
	this.serverHost = "localhost";
	this.serverHost = "localhost";
	this.protocol = "ws";
	this.portNumber = 3000;
	this.websocket = new WebSocket(this.connectionString());
	this.websocket.onclose = $bind(this,this.onClose);
	this.websocket.onerror = $bind(this,this.onError);
	this.websocket.onmessage = $bind(this,this.onMessage);
	this.websocket.onopen = $bind(this,this.onOpen);
};
Test.__name__ = true;
Test.main = function() {
	var test = new Test();
	console.log(test.connectionString());
}
Test.prototype = {
	doSend: function(aMessage) {
		this.websocket.send(aMessage);
	}
	,onError: function(ev) {
		console.log("Error " + Std.string(ev));
	}
	,onMessage: function(ev) {
		console.log("Received " + Std.string(ev));
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
var $_, $fid = 0;
function $bind(o,m) { if( m == null ) return null; if( m.__id__ == null ) m.__id__ = $fid++; var f; if( o.hx__closures__ == null ) o.hx__closures__ = {}; else f = o.hx__closures__[m.__id__]; if( f == null ) { f = function(){ return f.method.apply(f.scope, arguments); }; f.scope = o; f.method = m; o.hx__closures__[m.__id__] = f; } return f; };
String.__name__ = true;
Array.__name__ = true;
Test.main();
})();
