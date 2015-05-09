package model;
import util.Util;
import haxe.Json;
import haxe.Utf8;
import haxe.Timer;
import haxe.ds.ArraySort;
import js.html.Element;
import haxe.ds.GenericStack;
import js.html.Event;
import js.html.CloseEvent;
import js.html.MessageEvent;
import js.html.WebSocket;
import js.html.DOMCoreException;
import js.html.DivElement;
import js.html.Document;
import js.html.File;
import js.html.KeyboardEvent;
import js.html.InputElement;
import js.html.SelectElement;
import js.html.OptionElement;
import js.html.FileReader;
import js.html.ImageElement;
import model.Contact;
import model.Login;
import model.Person;
import model.LoginStatus;
import model.Command;
import model.CommandType;
import model.UserOperation;
import util.Util;
import util.Config;
import js.Browser;
import js.html.ButtonElement;
import js.html.TextAreaElement;
import promhx.Stream;
import promhx.Promise;
using promhx.haxe.EventTools;
import promhx.Deferred;

class ProjectWorkbench {
	//constants
	var PROJECT_WORKBENCH_LIST : String = "projectWorkbenches";

	//Initialization populates the workbench list
	//for the selected project. (single selection only)
	//Populates the types of scripts supported.
	//Selecting a workbench, displays the details.
	//ideally syntax highlighted script.
	//Run the script
	//Console or a display area displays the result.

	//Save saves/inserts a new script.
	//Delete deletes.
	//Update: is automatic with a timer.
	//Actions - generate json actions to be handled.
	//Actions: -- User triggers them
	//Save: Saves or inserts a new script.
	//Delete: delete.
	//Update: Updates the script.
	//Run : action to run the script.
	//Select a workbench.
	//query all workbenches.
	//query all supported scripts

	//Actions -- system generated based on user preferences
	//Automatic workspace saver timer thread.
	//User should be able to pause the thread
	//anytime.

	//Events:
	//project selected. -> triggers query all workbenches.
	//script type selected -> triggers any syntax rules for the script.
	//Supported script types selected
	//workbench list published.
	//workbench selected.
	//save button clicked 
	//delete button clicked
	//update button clicked
	//run button pressed 


	//private variables.
	var selectedProject : Project;
	var workbenchId : String;
	var supportedScripts : Array<String>;
	//To help classify the script
	var scriptData : String;
	var numberOfCores : int;
	var scriptDataPath : String;
	var newWorkBench : Boolean;
	var scriptMetaTags : String;
	//A better pnemonic than the uuid.
	var scriptSummary : String 

	var autosave : Boolean; //Enable when workbench has been inserted.

}