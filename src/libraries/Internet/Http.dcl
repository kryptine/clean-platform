definition module Http

// This library defines HTTP related types and functions
import StdString
import StdMaybe
import HashTable

:: HTTPRequest	= {	req_method		:: 	String						// The HTTP request method (eg. GET, POST, HEAD)
				,	req_path		::	String						// The requested location (eg. /foo)
				,	req_query		::	String						// The query part of a location (eg. ?foo=bar&baz=42)
				,	req_version		::	String						// The http version (eg. HTTP/1.0 or HTTP/1.1)
				,	req_protocol	::	HTTPProtocol				// Protocol info, http or https
				,	req_headers		::	HashTable String String		// The headers sent with the request parsed into name/value pairs
				,	req_data		::	String						// The raw data of the request (without the headers)
				,	arg_get			::	HashTable String String		// The arguments passed in the url 
				,	arg_post		::	HashTable String String		// The arguments passed via the POST method
				,	arg_cookies		::	HashTable String String		// The cookies in the set-cookie header
				,	arg_uploads		::	HashTable String HTTPUpload	// Uploads that are sent via the POST method
				,	server_name		::	String						// Server host name or ip address
				,	server_port		::	Int							// Server port
				,	client_name		::	String						// Client host name or ip address
				}

:: HTTPProtocol	= HTTPProtoHTTP | HTTPProtoHTTPS					// The protocol used for a request

:: HTTPResponse	= {	rsp_headers		::	HashTable String String		// Extra return headers that should be sent (eg. ("Content-Type","text/plain"))
				,	rsp_data		::	String						// The body of the response. (eg. html code or file data)
				}
			
:: HTTPUpload	= { upl_name		::	String						// The name of the file input in the form
				,	upl_filename	::	String						// The filename of the uploaded file
				,	upl_mimetype	::	String						// The MIME content type of the file
				,	upl_content		::	String						// The actual content of the file.
				}

//Construction functions 
newHTTPRequest	:: HTTPRequest
newHTTPResponse	:: HTTPResponse
newHTTPUpload	:: HTTPUpload

//String instances
instance toString HTTPRequest
instance toString HTTPResponse

//Server utilities
parseRequestLine	:: !String																							-> Maybe (!String, !String, !String, !String)
parseHeader			:: !String																							-> Maybe (!String, !String)

//Request utilities
parseRequest 		::	!HTTPRequest																					-> HTTPRequest

//Generating responses
staticResponse		:: !HTTPRequest !*World																				-> (!HTTPResponse, !*World)
notfoundResponse	:: !HTTPRequest !*World 																			-> (!HTTPResponse, !*World)
forbiddenResponse	:: !HTTPRequest !*World 																			-> (!HTTPResponse, !*World)
customResponse		:: ![((String -> Bool),(HTTPRequest *World -> (HTTPResponse, *World)))] !Bool !HTTPRequest !*World	-> (!HTTPResponse, !*World)

//Response utilities
encodeResponse		:: !Bool !HTTPResponse !*World																		-> (!String,!*World)
