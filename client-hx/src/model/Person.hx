package model;
import haxe.Json;
import js.html.Event;
import js.html.KeyboardEvent;
import js.Browser;
import js.html.Text;
import js.html.Element;
import js.html.DivElement;
import js.html.InputElement;
import js.html.DOMCoreException;
import js.html.Document;
import js.html.ButtonElement;
import js.Lib.*;
import haxe.ds.GenericStack;
import util.*;
import promhx.Stream;
import promhx.Promise;


class Person {
	public var nickName (default, null) : String;
	public var password (default, null) : String;
	public var firstName (default, null): String;
	public var lastName (default, null): String;
	public var lastLoginTime (default, default) : Date;
	private var deleted : Bool;
	public function new(fName : String
		, lName : String
		, nName : String
		, pwd : String) {
		firstName = fName;
		lastName = lName;
		nickName = nName;
		password = pwd;
		lastLoginTime  = Date.now();
		deleted = false; // Should be default..
	}
	public function setNickName(n : String): Void {
		this.nickName = n;
	}
	public function setFirstName(n : String) : Void {
		this.firstName = n;
	}
	public function setLastName(n : String): Void {
		this.lastName = n;
	}
	public function setPassword (n : String) : Void {
		this.password = n;
	}
	public static function listDisplay(person : model.Person ) : String {
		return (person.firstName + " " + person.lastName);
	}	
	public static function optionId(person : model.Person) : String {
		return person.nickName;
	}


}