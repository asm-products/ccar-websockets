package model;


class Login {
	
	public var commandType (default, null) : String;
	public var login(default, null) : Person;
	public var loginStatus(default, null) : String;
	public function new (commandType : String, p: Person, s : LoginStatus){
		this.commandType = commandType;
		login = p;
		loginStatus = Std.string(s);

	}
	public static function createLoginResponse(incomingMessage : Dynamic) : Login {
		trace("Creating login response " + incomingMessage);
		var commandType : String = incomingMessage.commandType;
		var loginStatus : LoginStatus = Type.createEnum (LoginStatus, incomingMessage.loginStatus);
		var p : Dynamic = incomingMessage.login;
		var person : Person = new Person(p.firstName, 
						p.lastName,
						p.nickName,
						p.password);
		var result : Login = new Login(commandType, person, loginStatus);
		return result;
	}
}