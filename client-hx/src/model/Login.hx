package model;


class Login {

	public var person(default, null) : Person;
	public var loginStatus(default, null) : LoginStatus;
	public function new (p: Person, s : LoginStatus){
		person = p;
		loginStatus = s;

	}
}