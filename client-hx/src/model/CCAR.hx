package model;

class CCAR {
	public var scenarioName (default, null) : String;
	public var scenarioText (default, null) : String;
	public var creator (default, null) : String;
	public var deleted (default, null): Bool;	
	public function new(name : String, text : String, creator : String) {
		scenarioName = name;
		scenarioText = text;
		this.creator = creator;
		this.deleted = false;
	}
	public function setScenarioName(aName : String): Void {
		this.scenarioName = aName;
	}
	public function setScenarioText(aText : String): Void {
		this.scenarioText = aText;
	}
}