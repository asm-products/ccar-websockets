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
import promhx.base.EventLoop;

/*
MarketData 
    symbol Text 
    lastPrice Text 
    askSize Text 
    askPrice Text 
    bidSize Text 
    bidPrice Text 
    lastUpdateTime UTCTime default=CURRENT_TIMESTAMP
    marketDataProvider MarketDataProviderId 
*/

typedef MarketDataUpdate = {
	var symbol : String;
	var lastPrice: String;
	var askSize : String;
	var askPrice : String;
	var bidSize : String;
	var bidPrice : String;
	var lastUpdateTime : Date;
	var marketDataProvider : String;
};

