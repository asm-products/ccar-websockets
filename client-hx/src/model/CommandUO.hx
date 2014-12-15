package model;

class CommandUO {
	public var commandType (default, null) : String;
	public var operation (default, null): UserOperation;
	public var person (default, null): Person;
	public function new(commandType : String, o : UserOperation, p : Person){
		this.commandType = commandType;
		this.operation = o;
		this.person = p;
	}
}