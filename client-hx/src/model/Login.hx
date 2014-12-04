package model;


class Login {

	public var person(default, null) : Person;
	public var loginStatus(default, null) : String;
	public function new (p: Person, s : LoginStatus){
		person = p;
		loginStatus = Std.string(s);

	}
}