package model;

class ParseCCARText {
	public var nickName (default, null) : String;
	public var textUploadedBy (default, null) : String;
	public var scenarioName (default, null) : String;
	public var ccarText (default, null) : String;
	public function new (t : String, s : String, c : String) {
		textUploadedBy = t;
		scenarioName = s;
		ccarText = c;
		nickName = MBooks.getMBooks().getNickName();
	}
}