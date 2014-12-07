package model;


class Command {
	private var commandType : String;
	private var payload : Dynamic;
	public function new(aCType : String, payload : Dynamic){
		this.commandType = aCType;
		this.payload = payload;
	}
}