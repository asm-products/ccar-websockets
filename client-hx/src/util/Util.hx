package util;
import haxe.Json;
import js.html.Event;
import js.Browser;
import js.html.Element;
import js.html.InputElement;
import js.html.Text;
import js.html.TextAreaElement;

import js.html.DivElement;
import js.html.Document;
import js.html.ButtonElement;
import js.html.SelectElement;
import js.html.OptionElement;

class Util {
	public static var DEFAULT_ROWS : Int = 10;
	public static var DEFAULT_COLS : Int = 50;
	public static var BACKSPACE : Int = 8;

	//XXX:This will bite!
	public static var UP_ARROW : Int = 38;
	public static var DOWN_ARROW : Int = 40;
	public static function NEW_LINE () : Int {
		return 10;
	}
	public static function TAB() : Int {
		return 9 ;
	}
	public static function CR() : Int {
		return 13;
	}
	public static function isUpOrDown(code : Int){
		return code == UP_ARROW || code == DOWN_ARROW;
	}
	public static function isUP(code : Int) {
		return code == UP_ARROW;
	}
	public static function issDown(code : Int) {
		return code == DOWN_ARROW;
	}
	public static function isTab(code : Int){
		return code == TAB();
	}
	public static function isBackspace(code : Int) {
		return code == BACKSPACE;
	}
	public static function isSignificantWS(code : Int) {
		//newline, tab or carriage return
		return (code == TAB() || code == NEW_LINE() || code == CR());
	}
	public static function createDivTag(document : Document, className : String) : DivElement {
		trace("Creating DIV tag "  + className);
		var div : DivElement = cast document.getElementById(className);
		if (div == null) {
			div = cast document.createDivElement();
			div.className = className;
			div.id = "DIV" +  "_" + className;
			document.body.appendChild(div);
		}else {
			trace("Div tag exists -> " + className);
		}
		return div;
	}


	public static function createInputElement(document : Document, 
		parent : DivElement, elementClass : String
		, elementName : String): Void {
		//trace("Creating input element " + elementName);
		var inputElement = document.createInputElement();
		inputElement.id = elementName;
		parent.appendChild(inputElement);
	}

	public static function createTextAreaElement(document : Document
		, parent : DivElement 
		, elementName : String
		, elementClass : String) : Void {
			//trace("Creating text area element");
			var areaElement = document.createTextAreaElement();
			areaElement.id = elementName;
			areaElement.rows = DEFAULT_ROWS;
			areaElement.cols = DEFAULT_COLS;
			parent.appendChild(areaElement);
	}

	public static function createListElement(document : Document
		, parent : DivElement
		, elementClass : String
		, elementName : String) : Void {
			//trace("Creating list element");
			var listElement = document.createUListElement();
			listElement.id = elementName;
			parent.appendChild(listElement);
		}
	public static function createButtonElement (document : Document
		, parent : DivElement
		, elementClass : String
		, elementName : String): Void {
			//trace("Creating button element");
			var element : ButtonElement = document.createButtonElement();
			element.value = elementName;
			element.id = elementName;
			element.innerHTML = elementName;
			parent.appendChild(element);
		}
	public static function createSelectElement(document : Document 
					, parent : DivElement
					, elementClass : String
					, elementName : String){
			//trace("Create selection element");
			var element : SelectElement = document.createSelectElement();
			element.id = elementName;
			parent.appendChild(element);
		}

	public static function createElementWithLabel(document : Document
			, parent : DivElement, elementId : String, elementLabel : String) : Void{
			//trace("Element id " + elementId + "->" + "Label " + elementLabel);

			var inputLabel = document.createLabelElement();
			var input = document.createInputElement();
			input.id = elementId;
			inputLabel.id = LABEL + elementId;
			inputLabel.innerHTML = elementLabel;
			parent.appendChild(inputLabel);
            parent.appendChild(input);

	}

	public static function createTextAreaElementWithLabel(document : Document
			, parent : DivElement
			, elementId : String 
			, elementLabel : String) : Void {
			
			var inputLabel = document.createLabelElement();
			inputLabel.id = LABEL + elementId;
			inputLabel.innerHTML = elementLabel;
			trace("Element id before calling textareaelement " + elementId);
			createTextAreaElement(document, parent, elementId, elementId);			
			var textAreaElement : Text = cast document.getElementById(elementId);

	}

	public static function showDivField(fieldName : String) {
		var div : DivElement = cast (Browser.document.getElementById(fieldName));
		div.setAttribute("style", "display:normal");
	}

	public static function hideDivField(fieldName : String) {
		var div : DivElement = cast Browser.document.getElementById(fieldName);
		div.setAttribute("style", "display:none");
	}

	public static function logToServer(logMessage : String){
		//Send the log message to the server.
	}

	public static function log(logMessage : String){
		trace(logMessage);
		
	}
	//Prefix: to maintain uniqueness
	private static var LABEL: String = "LABEL_";
	private static var DIV : String = "DIV_";
}