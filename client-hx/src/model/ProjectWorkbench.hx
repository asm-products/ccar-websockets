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

	//Initialization should populate the workbench list.
	//It should populate the types of scripts supported.
	//Selecting a workbench, should display
	//ideally syntax highlighted script.
	//Run should run the script.
	//Console should show the results.

	//private variables.
	var selectedProject : Project;
	var supportedScripts : Array<String>;
	var scriptData : String;
	var numberOfCores : int;
	var scriptDataPath : String;
	var newWorkBench : Boolean;
	var workbenchId : String;

	var autosave : Boolean; //Enable when workbench is not new.
}