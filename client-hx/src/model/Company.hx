package model;




class Company {
	public var name(default, null) : String;
	public var companyId(default, null) : String;
	public var generalMailbox(default, null) : String;
	public var image(default, null) : String;

	public function new(n, cId, gM, ima){
		this.name = n;
		this.companyId = cId;
		this.generalMailbox = gM;
		this.image = ima;
	}

}