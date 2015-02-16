package model;


class Command {
	private var nickName : String;
	private var commandType : String;
	private var payload : Dynamic;
	public function new(nickName : String, aCType : String, payload : Dynamic){
		this.commandType = aCType;
		this.payload = payload;
	}
}