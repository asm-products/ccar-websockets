package view;
import js.Browser;
import js.html.Element;
import js.html.InputElement;
import js.html.TextAreaElement;
import js.html.DOMCoreException;
import js.html.Document;
import js.html.ButtonElement;
import js.html.DivElement;
import js.html.UListElement;
import js.html.LIElement;
import js.html.SelectElement;
import js.html.KeyboardEvent;
import js.html.OptionElement;
import js.html.TableElement;
import js.html.TableCellElement;
import js.html.TableRowElement;
import js.html.HTMLCollection;
import js.html.FileReader;
import promhx.Stream;
import promhx.Deferred;


/**
* Given a select element, manaage and update the contents to 
* a update, add, delete events.
*/

// Given a list and an array of model instances, 
// update the list element. 
// Set a selection index.
//display to display each element in the list.
//optionId : a unique id for the option element contained by the list.

class ListManager<T> {
	
	private var optionId : T -> String;
	private var listDisplay : T -> String;
	private var modelMap : Map<String, T>;
	private var streamMap : Map<String, Stream<Dynamic> > ;
	public var listElement (default, null) : SelectElement;
	/**
	* Prefix to manage the option element ids.
	*/
	public var prefix (default, null) : String;
	public function new (list : SelectElement, idPrefix : String, opt : T -> String, 
				listDisplay : T -> String){
		this.listElement = list;
		this.prefix = idPrefix;
		this.optionId = opt;
		this.listDisplay = listDisplay;
		streamMap = new Map<String, Stream<Dynamic> >();
		modelMap = new Map<String, T> ();
	}
	private function key(element : T) : String {
		return this.prefix + optionId(element);
	}

	//Upsert : add and return the stream.
	public function upsert(element : T) : Stream<Dynamic> {
		return add(element);
	}

	
	
	//Adding an element returns the selection stream
	//for the element. We need to add 
	//methods to merge streams for the selecction event.
	public function add(element : T ) : Stream<Dynamic>{
		trace("Adding element " + element);
		var optionElement : OptionElement 
			= cast (Browser.document.getElementById(key(element)));
		if(optionElement == null) {
			trace("Element not found creating a new option");
			optionElement = cast (Browser.document.createOptionElement());
			optionElement.id = key(element);
			optionElement.text = listDisplay(element);
			var stream = 
				MBooks_im.getSingleton().initializeElementStream(
					cast optionElement 
					, "click"
				);
			streamMap.set(key(element), stream);
			modelMap.set(key(element), element);
			listElement.appendChild(optionElement);
			optionElement.selected = true;
			return stream;
		}
		return null;
	}
	public function update(element : T){
		var key : String = key(element);
		var optionElement : OptionElement = 
			cast (Browser.document.getElementById(key));
		if(optionElement == null) {
			throw ("Element not found "  + element);
		}else {
			assertEquals(optionElement.id, key);
			optionElement.text = listDisplay(element);
			optionElement.selected = true;
		}
	}

	private function assertEquals(a : Dynamic, b : Dynamic){
		if(a != b){
			throw ("Assertion failed " + a  + " not equal " + b);
		}
	}
	public function delete(element : T ) {
		trace("Deleting element " + element);
		var key : String = key(element);
		var model : T = modelMap.get(key);
		modelMap.remove(key);
		removeFromList(key);
		var stream : Stream<Dynamic> = streamMap.get(key);
		stream.end();
		streamMap.remove(key);
	}
	public function clear(){
		trace("Clearing list elements");
		try {
			for (entry in modelMap) {
				removeFromList(key(entry));
			}
			for (entry in streamMap){
				entry.end();
			}
			modelMap = new Map<String, T>();
			streamMap = new Map<String, Stream<T> >();
		}
	}

	private function removeFromList(id : String){
		var optionElement : OptionElement =
			cast (Browser.document.getElementById(id));
		if (optionElement == null){
			closeStream(id);
			throw ("Nothing to delete " + id);
		}
		optionElement.parentNode.removeChild(optionElement);
	}
	private function closeStream(id : String){
		trace("Closing stream " + id);
		var stream : Stream<Dynamic	> = streamMap.get(id);
		if(stream != null){
			stream.end();
		}
	}

}