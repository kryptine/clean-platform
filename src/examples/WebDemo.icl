module WebDemo
/**
* Simple CGI application built solely on clean-platform libs
*/
import StdEnv
import Html	//Text.Html
import Http	//Internet.Http
import Cgi	//Internet.Http.Cgi

page = HtmlTag [] [head,body] 
head = HeadTag [] [TitleTag [] [Text "Hello World!"]]
body = BodyTag [] [H1Tag [] [Text "Hello World!"]]

helloPage :: !HTTPRequest !*World -> (!HTTPResponse,!*World)
helloPage req world
		= ({newHTTPResponse & rsp_data = toString (page (name req))},world)
where
	name req	= case get "name" req.arg_get of
		Nothing 	= "world"
		(Just n)	= n
	page name = HtmlTag [] [head name,body name] 
	head name = HeadTag [] [TitleTag [] [Text "Hello ", Text name]]
	body name = BodyTag [] [H1Tag [] [Text "Hello ", Text name]]

Start :: *World -> *World
Start world = startCGI [] [(const True, helloPage)] world
