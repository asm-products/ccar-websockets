package model;

class Contact {
	public var firstName (null, default): String;
	public var lastName (null, default) : String;
	public var login (null, default): String;
	public function new (aName : String, lName : String, aLogin : String) {
		firstName = aName;
		lastName = lName;
		login = aLogin;
		//trace("Creating contact with " + aName + "->" + lName + " -> " + aLogin);
	}

}