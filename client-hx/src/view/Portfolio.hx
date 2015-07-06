	package view;

	import haxe.Json;
	import js.html.Event;
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
	import haxe.ds.ObjectMap;
	import promhx.Stream;

	import model.Portfolio;
	import js.Lib.*;
	import util.*;


class Portfolio {
	public static var SAVE_PORTFOLIO : String = "savePortfolio";
	public static var UPDATE_PORTFOLIO : String = "updatePortfolio";
	public static var DELETE_PORFOLIO : String = "deletePortfolio";

	public var model (default, null): model.Portfolio;


}