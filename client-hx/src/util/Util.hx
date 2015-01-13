package util;
import haxe.Json;
import js.html.Event;
import js.Browser;
import js.html.Element;
import js.html.InputElement;
import js.html.DivElement;
import js.html.Document;
import js.html.ButtonElement;
import js.html.SelectElement;
import js.html.OptionElement;

class Util {
	public static function createDivTag(document : Document, className : String) : DivElement {
		var div : DivElement = cast document.getElementById(className);
		if (div == null) {
			div = cast document.createDivElement();
			div.id = className;
			div.className = className;
			document.body.appendChild(div);
		}
		return div;
	}

	public static function createInputElement(document : Document, 
		parent : DivElement, elementClass : String
		, elementName : String): Void {
		trace("Creating input element " + elementName);
		var div = createDivTag(document, elementClass);
		var inputElement = document.createInputElement();
		inputElement.id = elementName;
		div.appendChild(inputElement);
		parent.appendChild(div);

	}

	public static function createTextAreaElement(document : Document
		, parent : DivElement 
		, elementClass : String
		, elementName : String) : Void {
			trace("Creating text area element");
			var div = createDivTag(document, elementClass);
			var areaElement = document.createTextAreaElement();
			areaElement.value = elementName;
			areaElement.id = elementName;
			div.appendChild(areaElement);
			parent.appendChild(div);

		}

	public static function createListElement(document : Document
		, parent : DivElement
		, elementClass : String
		, elementName : String) : Void {
			trace("Creating list element");
			var div = createDivTag(document, elementClass);
			var listElement = document.createUListElement();
			listElement.id = elementName;
			div.appendChild(listElement);
			parent.appendChild(div);
		}
	public static function createButtonElement (document : Document
		, parent : DivElement
		, elementClass : String
		, elementName : String): Void {
			trace("Creating button element");
			var div = createDivTag(document, elementClass);
			var element : ButtonElement = document.createButtonElement();
			element.value = elementName;
			element.id = elementName;
			element.innerHTML = elementName;
			div.appendChild(element);
			parent.appendChild(div);
		}
	public static function createSelectElement(document : Document 
					, parent : DivElement
					, elementClass : String
					, elementName : String){
			trace("Create selection element");
			var div = createDivTag(document, elementClass);
			var element : SelectElement = document.createSelectElement();
			element.id = elementName;
			div.appendChild(element);
			parent.appendChild(div);
		}
}