package model;


class Command {
	private var commandType : String;
	private var payload : String;
	public function new(aCType : String, payload : String){
		this.commandType = aCType;
		this.payload = payload;
	}
}