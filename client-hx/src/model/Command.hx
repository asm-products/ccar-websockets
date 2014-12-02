package model;


class Command {
	private var commandType : CommandType;
	private var payload : String;
	public function new(aCType : CommandType, payload : String){
		this.commandType = aCType;
		this.payload = payload;
	}
}