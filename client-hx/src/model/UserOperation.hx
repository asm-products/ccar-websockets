package model;

class UserOperation {
	public var operation (default, null) : String;
	public function new(o : String){
		this.operation = o;
	}
}