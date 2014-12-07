package model;


class Login {
	
	public var commandType (default, null) : String;
	public var person(default, null) : Person;
	public var loginStatus(default, null) : String;
	public function new (commandType : String, p: Person, s : LoginStatus){
		this.commandType = commandType;
		person = p;
		loginStatus = Std.string(s);

	}
}