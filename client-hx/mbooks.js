(function () { "use strict";
var $estr = function() { return js.Boot.__string_rec(this,''); };
var HxOverrides = function() { }
HxOverrides.__name__ = true;
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
var MBooks = function() {
	this.portNumber = 3000;
	this.protocol = "ws";
	this.serverHost = "localhost";
	this.serverHost = "localhost";
	this.protocol = "ws";
	this.portNumber = 3000;
	this.createConnectionForm();
};
MBooks.__name__ = true;
MBooks.main = function() {
	var test = new MBooks();
}
MBooks.prototype = {
	logout: function() {
		console.log("Logging out ");
		if(this.websocket != null) this.websocket.close(); else console.log("No valid connection found");
	}
	,createConnectionForm: function() {
		try {
			console.log("Creating connection form");
			var document = js.Browser.document;
			var person = new model.Person("","","","");
			person.createNickNameForm(this);
			this.initializeConnection();
			console.log("Connection form created");
		} catch( msg ) {
			if( js.Boot.__instanceof(msg,DOMException) ) {
				console.log("Exception " + Std.string(msg));
			} else throw(msg);
		}
	}
	,doSendJSON: function(aMessage) {
		console.log("Sending " + aMessage);
		var d = haxe.Json.parse(aMessage);
		console.log("Parsed message should look like so " + Std.string(d));
		this.websocket.send(aMessage);
	}
	,onError: function(ev) {
		console.log("Error " + haxe.Json.stringify(ev));
	}
	,createInvalidPassword: function(lr) {
		console.log("Processing invalid login" + Std.string(lr));
	}
	,createLoginForm: function(lr) {
		console.log("Creating login form");
		lr.login.createLoginForm();
	}
	,createRegistrationForm: function(books,lr) {
		if(lr.login == null) {
			var person = new model.Person("","","","");
			person.registerForm(books);
		} else {
			console.log("Login not null : Login  " + Std.string(lr.login));
			lr.login.registerForm(books);
		}
	}
	,createUndefined: function() {
		console.log("Undefined as response..should not happen");
	}
	,processLoginResponse: function(lR) {
		console.log("Processing login object " + Std.string(lR));
		console.log("Processing lR status " + lR.loginStatus);
		if(lR.loginStatus == null) {
			console.log("Undefined state");
			return;
		}
		var lStatus = Type.createEnum(model.LoginStatus,lR.loginStatus);
		console.log("Processing lStatus " + Std.string(lStatus));
		if(lStatus == model.LoginStatus.UserNotFound) {
			console.log("User not found. Need to see why enum is not working " + Std.string(lR));
			this.createRegistrationForm(this,lR);
		}
		if(lStatus == model.LoginStatus.UserExists) this.createLoginForm(lR);
		if(lStatus == model.LoginStatus.InvalidPassword) this.createInvalidPassword(lR);
		if(lStatus == model.LoginStatus.Undefined) this.createUndefined();
	}
	,onMessage: function(ev) {
		console.log("Received " + Std.string(ev.data));
		var incomingMessage = haxe.Json.parse(ev.data);
		incomingMessage = haxe.Json.parse(incomingMessage);
		console.log("Printing incoming message " + incomingMessage);
		this.parseIncomingMessage(incomingMessage);
	}
	,parseIncomingMessage: function(incomingMessage) {
		var commandType = this.parseCommandType(incomingMessage.commandType);
		switch( (commandType)[1] ) {
		case 1:
			var login = model.Login.createLoginResponse(incomingMessage);
			this.processLoginResponse(login);
			break;
		case 0:
			break;
		case 2:
			break;
		case 3:
			break;
		case 4:
			break;
		case 5:
			break;
		case 6:
			break;
		case 8:
			break;
		case 7:
			break;
		case 9:
			break;
		case 10:
			break;
		case 11:
			break;
		case 12:
			break;
		case 13:
			break;
		}
	}
	,parseCommandType: function(commandType) {
		console.log("Parsing command type " + commandType);
		try {
			return Type.createEnum(model.CommandType,commandType);
		} catch( e ) {
			console.log("Error " + Std.string(e));
			return model.CommandType.Undefined;
		}
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
var IMap = function() { }
IMap.__name__ = true;
var Reflect = function() { }
Reflect.__name__ = true;
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
var Std = function() { }
Std.__name__ = true;
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
StringBuf.__name__ = true;
StringBuf.prototype = {
	addSub: function(s,pos,len) {
		this.b += len == null?HxOverrides.substr(s,pos,null):HxOverrides.substr(s,pos,len);
	}
	,__class__: StringBuf
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
Type.__name__ = true;
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
Type.enumIndex = function(e) {
	return e[1];
}
var haxe = {}
haxe.Json = function() {
};
haxe.Json.__name__ = true;
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
haxe.ds = {}
haxe.ds.StringMap = function() { }
haxe.ds.StringMap.__name__ = true;
haxe.ds.StringMap.__interfaces__ = [IMap];
haxe.ds.StringMap.prototype = {
	keys: function() {
		var a = [];
		for( var key in this.h ) {
		if(this.h.hasOwnProperty(key)) a.push(key.substr(1));
		}
		return HxOverrides.iter(a);
	}
	,get: function(key) {
		return this.h["$" + key];
	}
	,__class__: haxe.ds.StringMap
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
model.Command = function(aCType,payload) {
	this.commandType = aCType;
	this.payload = payload;
};
model.Command.__name__ = true;
model.Command.prototype = {
	__class__: model.Command
}
model.CommandType = { __ename__ : true, __constructs__ : ["RegisterUser","Login","QueryUser","DeleteUser","UpdateUser","CreateUserTerms","UpdateUserTerms","QueryUserTerms","DeleteUserTerms","CreateUserPreferences","UpdateUserPreferences","QueryUserPreferences","DeleteUserPreferences","Undefined"] }
model.CommandType.RegisterUser = ["RegisterUser",0];
model.CommandType.RegisterUser.toString = $estr;
model.CommandType.RegisterUser.__enum__ = model.CommandType;
model.CommandType.Login = ["Login",1];
model.CommandType.Login.toString = $estr;
model.CommandType.Login.__enum__ = model.CommandType;
model.CommandType.QueryUser = ["QueryUser",2];
model.CommandType.QueryUser.toString = $estr;
model.CommandType.QueryUser.__enum__ = model.CommandType;
model.CommandType.DeleteUser = ["DeleteUser",3];
model.CommandType.DeleteUser.toString = $estr;
model.CommandType.DeleteUser.__enum__ = model.CommandType;
model.CommandType.UpdateUser = ["UpdateUser",4];
model.CommandType.UpdateUser.toString = $estr;
model.CommandType.UpdateUser.__enum__ = model.CommandType;
model.CommandType.CreateUserTerms = ["CreateUserTerms",5];
model.CommandType.CreateUserTerms.toString = $estr;
model.CommandType.CreateUserTerms.__enum__ = model.CommandType;
model.CommandType.UpdateUserTerms = ["UpdateUserTerms",6];
model.CommandType.UpdateUserTerms.toString = $estr;
model.CommandType.UpdateUserTerms.__enum__ = model.CommandType;
model.CommandType.QueryUserTerms = ["QueryUserTerms",7];
model.CommandType.QueryUserTerms.toString = $estr;
model.CommandType.QueryUserTerms.__enum__ = model.CommandType;
model.CommandType.DeleteUserTerms = ["DeleteUserTerms",8];
model.CommandType.DeleteUserTerms.toString = $estr;
model.CommandType.DeleteUserTerms.__enum__ = model.CommandType;
model.CommandType.CreateUserPreferences = ["CreateUserPreferences",9];
model.CommandType.CreateUserPreferences.toString = $estr;
model.CommandType.CreateUserPreferences.__enum__ = model.CommandType;
model.CommandType.UpdateUserPreferences = ["UpdateUserPreferences",10];
model.CommandType.UpdateUserPreferences.toString = $estr;
model.CommandType.UpdateUserPreferences.__enum__ = model.CommandType;
model.CommandType.QueryUserPreferences = ["QueryUserPreferences",11];
model.CommandType.QueryUserPreferences.toString = $estr;
model.CommandType.QueryUserPreferences.__enum__ = model.CommandType;
model.CommandType.DeleteUserPreferences = ["DeleteUserPreferences",12];
model.CommandType.DeleteUserPreferences.toString = $estr;
model.CommandType.DeleteUserPreferences.__enum__ = model.CommandType;
model.CommandType.Undefined = ["Undefined",13];
model.CommandType.Undefined.toString = $estr;
model.CommandType.Undefined.__enum__ = model.CommandType;
model.CommandUO = function(commandType,o,p) {
	this.commandType = commandType;
	this.operation = o;
	this.person = p;
};
model.CommandUO.__name__ = true;
model.CommandUO.prototype = {
	__class__: model.CommandUO
}
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
model.Login = function(commandType,p,s) {
	this.commandType = commandType;
	this.login = p;
	this.loginStatus = Std.string(s);
};
model.Login.__name__ = true;
model.Login.createLoginResponse = function(incomingMessage) {
	console.log("Creating login response " + Std.string(incomingMessage));
	var commandType = incomingMessage.commandType;
	var loginStatus = Type.createEnum(model.LoginStatus,incomingMessage.loginStatus);
	var p = incomingMessage.login;
	var person = new model.Person(p.firstName,p.lastName,p.nickName,p.password);
	var result = new model.Login(commandType,person,loginStatus);
	return result;
}
model.Login.prototype = {
	__class__: model.Login
}
model.LoginStatus = { __ename__ : true, __constructs__ : ["UserExists","UserNotFound","InvalidPassword","Undefined"] }
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
model.Person = function(fName,lName,nName,pwd) {
	this.firstName = fName;
	this.lastName = lName;
	this.nickName = nName;
	this.password = pwd;
	this.deleted = false;
};
model.Person.__name__ = true;
model.Person.prototype = {
	sendLogin: function(ev) {
		var p = new model.Person("","",this.nickNameInput.value,"");
		var lStatus = model.LoginStatus.Undefined;
		var cType = Std.string(model.CommandType.Login);
		var l = new model.Login(cType,p,lStatus);
		this.mbooks.doSendJSON(haxe.Json.stringify(l));
	}
	,createDivTag: function(document,className) {
		var div = document.createElement("div");
		div.className = className;
		document.body.appendChild(div);
		return div;
	}
	,createPassword: function(document,parent) {
		var div = document.createElement("div");
		div.className = "Person.Login.Password";
		var textElement = document.createTextNode("Password (hidden)");
		this.passwordInput = document.createElement("input");
		div.appendChild(textElement);
		div.appendChild(this.passwordInput);
		parent.appendChild(div);
	}
	,createNickName: function(document,parent) {
		var div = document.createElement("div");
		div.className = "Person.Login.NickName";
		var textElement = document.createTextNode("Nick name (needs to be unique)");
		this.nickNameInput = document.createElement("input");
		div.appendChild(textElement);
		div.appendChild(this.nickNameInput);
		parent.appendChild(div);
	}
	,createLastName: function(document,parent) {
		var div = document.createElement("div");
		div.className = "Person.Login.LastName";
		var textElement = document.createTextNode("Last Name");
		this.lastNameInput = document.createElement("input");
		div.appendChild(textElement);
		div.appendChild(this.lastNameInput);
		parent.appendChild(div);
	}
	,createFirstName: function(document,parent) {
		var div = document.createElement("div");
		div.className = "Person.Login.FirstName";
		var textElement = document.createTextNode("First Name");
		this.firstNameInput = document.createElement("input");
		div.appendChild(textElement);
		div.appendChild(this.firstNameInput);
		parent.appendChild(div);
	}
	,createFormElements: function(document,parent) {
		this.createFirstName(document,parent);
		this.createLastName(document,parent);
		this.createPassword(document,parent);
	}
	,registerForm: function(books) {
		try {
			console.log("Person :: Creating registration form" + Std.string(books));
			var document = js.Browser.document;
			console.log("Setting status ");
			this.status = document.getElementById("status");
			this.status.innerHTML = "Let me sign you up";
			var document1 = js.Browser.document;
			this.mbooks = books;
			console.log("Creating the div tags");
			var div = this.createDivTag(document1,"Person.Registration");
			this.createFormElements(document1,div);
			this.createRegisterButton(document1,div);
			this.createLogoutButton(document1,div);
			document1.body.appendChild(div);
		} catch( msg ) {
			if( js.Boot.__instanceof(msg,DOMException) ) {
				console.log("Exception " + Std.string(msg));
			} else throw(msg);
		}
	}
	,registerUser: function(ev) {
		console.log("Register user " + Std.string(ev));
		var commandType = "CreateUser";
		var operation = new model.UserOperation("Create");
		var uo = new model.CommandUO(commandType,operation,this);
		this.mbooks.doSendJSON(haxe.Json.stringify(uo));
	}
	,logoutUser: function(ev) {
		console.log("Logout user " + Std.string(ev));
		this.mbooks.logout();
	}
	,createLogoutButton: function(document,parent) {
		this.logout = document.createElement("button");
		this.logout.value = "Logout";
		this.logout.innerHTML = "Logout";
		parent.appendChild(this.logout);
		this.logout.onclick = $bind(this,this.logoutUser);
	}
	,createRegisterButton: function(document,parent) {
		this.register = document.createElement("button");
		this.register.value = "Register";
		this.register.innerHTML = "Register";
		parent.appendChild(this.register);
		this.register.onclick = $bind(this,this.registerUser);
	}
	,createLoginForm: function() {
		try {
			console.log("Creating login form");
			var document = js.Browser.document;
			var div = this.createDivTag(document,"Person.Login");
			div.appendChild(div);
			this.status = document.getElementById("status");
			this.status.innerHTML = "Welcome back. The last time you logged in";
			this.createNickName(document,div);
			this.createPassword(document,div);
		} catch( msg ) {
			if( js.Boot.__instanceof(msg,DOMException) ) {
				console.log("Exception " + Std.string(msg));
			} else throw(msg);
		}
	}
	,createNickNameForm: function(books) {
		try {
			this.mbooks = books;
			console.log("Creating login form");
			var document = js.Browser.document;
			var div = this.createDivTag(document,"Person.Login");
			this.createNickName(document,div);
			this.status = document.getElementById("status");
			this.status.innerHTML = "Welcome.";
			this.nickNameInput.onchange = $bind(this,this.sendLogin);
		} catch( msg ) {
			if( js.Boot.__instanceof(msg,DOMException) ) {
				console.log("Exception " + Std.string(msg));
			} else throw(msg);
		}
	}
	,__class__: model.Person
}
model.UserOperation = function(o) {
	this.operation = o;
};
model.UserOperation.__name__ = true;
model.UserOperation.prototype = {
	__class__: model.UserOperation
}
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
if(typeof(JSON) != "undefined") haxe.Json = JSON;
js.Browser.document = typeof window != "undefined" ? window.document : null;
MBooks.main();
})();
