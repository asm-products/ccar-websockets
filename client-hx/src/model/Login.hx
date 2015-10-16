package model;


class Login {
	
	public var commandType (default, null) : String;
	public var login(default, null) : model.Person;
	public var loginStatus(default, null) : String;
	public var nickName(default, null) : String;
	public function new (commandType : String, p: Person, s : LoginStatus){
		this.commandType = commandType;
		login = p;
		this.nickName = p.nickName;
		loginStatus = Std.string(s);
	}
	public static function createLoginResponse(incomingMessage : Dynamic, person : Person) : Login {
		//trace("Creating login response " + incomingMessage);
		if(incomingMessage.Right == null){
			throw ("Invalid login " + incomingMessage);
		}
		var commandType : String = "" + MBooks_im.getSingleton().parseCommandType(incomingMessage);
		var loginStatus : LoginStatus = Type.createEnum (LoginStatus, incomingMessage.Right.loginStatus);
		var result : Login = new Login(commandType, person, loginStatus);
		return result;
	}
}