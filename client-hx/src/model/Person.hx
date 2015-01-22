package model;

class Person {
	public var nickName (default, null) : String;
	public var password (default, null) : String;
	public var firstName (default, null): String;
	public var lastName (default, null): String;
	private var deleted : Bool;
	public function new(fName : String
		, lName : String
		, nName : String
		, pwd : String) {
		firstName = fName;
		lastName = lName;
		nickName = nName;
		password = pwd;
		deleted = false; // Should be default..
	}

	public function setNickName(n : String) : Void {
		this.nickName = n;
	}
	public  function setPassword(p : String) : Void {
		this.password = p;
	}
	public  function setFirstName(f : String): Void {
		this.firstName = f;
	}
	public  function setLastName(l : String): Void {
		this.lastName = l;
	}

	
}