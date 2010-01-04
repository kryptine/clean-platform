module PM

import StdEnv, StdGeneric
import Text, Map, UrlEncoding, HTTP, CGI
import SQL, MySQL, RelationalMapping

import PMHtml, PMForms
import PMDataModel
import PMDatabase

//Database settings
dbHostname	:== "localhost"
dbUsername	:==	"root"
dbPassword	:==	"test"
dbDatabase	:== "pmdemo"

//Generic derives
derive relMap Employee, EmployeeID, Project, ProjectID, Task, TaskID
derive bimap Maybe, [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

//********************************************************************************
// Framework
//********************************************************************************

//Start the CGI Wrapper
Start :: *World -> *World
Start world = startCGI [CGIOptStaticFallback True] [(isPage, pageHandler)] world

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


//Predicate which checks if a request should be handled by this application
isPage :: !String -> Bool
isPage page
	| endsWith ".png" page			= False	//Images are served statically
	| endsWith ".jpg" page			= False	
	| endsWith ".css" page			= False	//Stylesheets as well
	| endsWith ".js" page 			= False	//And javascript too
									= True

//Main request handler. Initializes and then decides what page generation function should be applied
pageHandler :: !HTTPRequest !*World -> (HTTPResponse, *World)
pageHandler req world
	//Initialialize the database
	# (context,connection,cursor,world) = initDatabase dbHostname dbUsername dbPassword dbDatabase world
	//Parse the request 
	# (mbSection, mbId, mbAction)		= parseUrl req.req_path
	# message							= maybe "" id (get "msg" req.arg_get)
	//Create page content
	# (mbRedirect, title, content, cursor)	= makeContent mbSection mbId mbAction req cursor
	//Finalize database
	# world								= endDatabase context connection cursor world
	//Create http response
	| isJust mbRedirect					//Redirect
		# (url,msg)						= fromJust mbRedirect
		# url							= url +++ "?msg=" +++ urlEncode msg
		# rsp							= {newHTTPResponse & rsp_headers = fromList [("Status","301 Moved Permanently"),("Location",url)]}
		= (rsp, world)
	| otherwise							//Show a page
		# rsp							= {newHTTPResponse & rsp_data = toString (makePage title message content)}
		= (rsp, world)

parseUrl :: String -> (Maybe String, Maybe String, Maybe String)
parseUrl url 
	# parts 			= split "/+" url
	| length parts > 2	= (Nothing, Nothing, Nothing)
	# (url,action)		= if (length parts == 2) (hd parts, Just (last parts)) (hd parts, Nothing)
	# parts 			= split "/" url
	# parts				= if (last parts == "") (init parts) parts
	| length parts == 2	= (Just (last parts), Nothing, action)
	| length parts == 3 = (Just (join "/" (init (tl parts))), Just (last parts), action)
						= (Nothing, Nothing, action)
	
makeContent :: (Maybe String) (Maybe String) (Maybe String) HTTPRequest !*cur -> (Maybe (String,String), String, [HtmlTag], !*cur) | SQLCursor cur & bimap{|*|} cur
makeContent (Just "projects")	Nothing				(Just "add")	req	cursor	= createProjectPage req cursor
makeContent (Just "projects")	Nothing				_				req	cursor	= listProjectsPage cursor
makeContent (Just "projects")	(Just projectNr)	(Just "edit")	req	cursor	= editProjectPage (toInt projectNr) req cursor
makeContent (Just "projects")	(Just projectNr)	(Just "delete")	req	cursor	= deleteProjectPage (toInt projectNr) req cursor
makeContent (Just "projects")	(Just projectNr)	_				req	cursor	= showProjectPage (toInt projectNr) cursor
makeContent (Just "employees")	Nothing				(Just "add")	req	cursor	= createEmployeePage req cursor
makeContent (Just "employees")	Nothing				_				req	cursor	= listEmployeesPage cursor
makeContent (Just "employees")	(Just name)			(Just "edit")	req	cursor	= editEmployeePage name req cursor
makeContent (Just "employees")	(Just name)			(Just "delete")	req	cursor	= deleteEmployeePage name req cursor
makeContent (Just "employees")	(Just name)			_				req	cursor	= showEmployeePage name cursor
makeContent _					_					_				req	cursor	= startPage cursor

//********************************************************************************
// Pages
//********************************************************************************

startPage :: !*cur -> (Maybe (String,String), !String, ![HtmlTag], !*cur) | SQLCursor cur
startPage cursor = (Nothing,"PM Demo", content, cursor)
where
	content =
		[ PTag []	[ Text "This is a simple project management system "
					, Text "which makes use of the Clean GenSQL database library."
					]
		, PTag []	[ Text "All mapping between the underlying relational database and the "
					, Text "Clean data structures used in this web application is "
					, Text "done by just one generic function."
					]
		]

//****** Projects ******//

listProjectsPage :: !*cur -> (Maybe (String, String), !String, ![HtmlTag], !*cur) | SQLCursor cur
listProjectsPage cursor 
	# (mbErr,cursor)		= execute "SELECT projectNr,description FROM project" [] cursor
	| isJust mbErr			= (Nothing, "Error",[Text (toString (fromJust mbErr))],cursor)
	# (_,rows,cursor)		= fetchAll cursor
	# rows					= map makeRow rows
	= (Nothing, "Projects", [toolbar, makeTable ["Project Nr","Description"] rows], cursor)
	where
		makeRow [SQLVInteger id, SQLVVarchar description] = [ATag [HrefAttr ("/projects/" +++ (toString id))] [Text (toString id)],Text description]
		toolbar	= makeToolbar [makeLinkButton "Add project" "/projects/+add" (Just "add")]

showProjectPage :: !Int !*cur -> (Maybe (String,String), !String, ![HtmlTag], !*cur ) | SQLCursor cur & bimap{|*|} cur
showProjectPage pid cursor 
	# (mbErr, mbProject, cursor)	= mapRead pid cursor	//Relational mapping
	| isJust mbErr					= (Nothing, "Error",[Text (toString (fromJust mbErr))],cursor)
	| isNothing mbProject			= (Nothing, "Error",[Text ("There is no project with nr " +++ toString pid)], cursor)
	# project						= fromJust mbProject
	= (Nothing, project.project_description,[toolbar, showProjectForm project],cursor)
	where
		toolbar	= makeToolbar [makeLinkButton "Edit" ("/projects/" +++ (toString pid)+++ "/+edit") (Just "edit"),makeLinkButton "Delete" ("/projects/" +++ (toString pid) +++ "/+delete") (Just "delete")]

createProjectPage :: !HTTPRequest !*cur -> (Maybe (String,String), !String, [HtmlTag], !*cur) | SQLCursor cur & bimap{|*|} cur
createProjectPage req cursor
	| req.req_method == "POST"
		# project						= editProjectUpd req.arg_post
		# (mbErr,mbId, cursor)			= mapCreate project cursor //Relational mapping
		| isJust mbErr					= (Nothing, "Error",[Text (toString (fromJust mbErr))],cursor)
		= (Just ("/projects/" +++ toString (int (fromJust mbId)),"Succesfully created project nr " +++ toString (fromJust mbId)),"",[],cursor)
	| otherwise
		# project						= defaultProject
		# (projects, cursor)			= getProjectOptions cursor
		# (employees,cursor)			= getEmployeeOptions cursor
		= (Nothing, project.project_description,[editProjectForm True project projects employees],cursor)


editProjectPage :: !Int !HTTPRequest !*cur -> (Maybe (String,String), !String, [HtmlTag], !*cur) | SQLCursor cur & bimap{|*|} cur
editProjectPage pid req cursor
	| req.req_method == "POST"
		# project						= editProjectUpd req.arg_post
		# (mbErr,mbId, cursor)			= mapUpdate project cursor //Relational mapping
		| isJust mbErr					= (Nothing, "Error",[Text (toString (fromJust mbErr))],cursor)
		= (Just ("/projects/" +++ toString (int (fromJust mbId)), "Successfully updated project " +++ toString pid),"",[],cursor)
		//# (mbErr,cursor)				= updateProject project cursor
		//| isJust mbErr					= (Nothing, "Error",[Text (toString (fromJust mbErr))],cursor)
		//= (Just ("/projects/" +++ toString pid, "Successfully updated project " +++ toString pid),"",[], cursor)
	| otherwise
		# (mbErr, mbProject, cursor)	= mapRead pid cursor	//Relational mapping
		| isJust mbErr					= (Nothing, "Error",[Text (toString (fromJust mbErr))],cursor)
		| isNothing mbProject			= (Nothing, "Error",[Text ("There is no project with project nr " +++ toString pid)], cursor)
		# project						= fromJust mbProject
		# (projects, cursor)			= getProjectOptions cursor
		# (employees,cursor)			= getEmployeeOptions cursor
		= (Nothing, project.project_description,[editProjectForm False project projects employees],cursor)

deleteProjectPage :: !Int !HTTPRequest !*cur -> (Maybe (String,String), !String, ![HtmlTag], !*cur) | SQLCursor cur & bimap{|*|} cur
deleteProjectPage pid req cursor
	# (mbErr, mbProject, cursor)	= mapDelete pid cursor	//Relational mapping
	| isJust mbErr					= (Nothing, "Error",[Text (toString (fromJust mbErr))],cursor)
	| isNothing mbProject			= (Nothing, "Error",[Text ("There is no project with project nr " +++ toString pid)], cursor)
	# project						= fromJust mbProject
	= (Just ("/projects","Successfully deleted project nr " +++ toString project.Project.project_projectNr),"",[],cursor)

//****** Employees ******//
listEmployeesPage :: !*cur -> (Maybe (String,String), !String, [HtmlTag], !*cur) | SQLCursor cur
listEmployeesPage cursor
	# (mbErr,cursor)		= execute "SELECT name,description FROM employee" [] cursor
	| isJust mbErr			= (Nothing, "Error",[Text (toString (fromJust mbErr))],cursor)
	# (_,rows,cursor)		= fetchAll cursor
	# rows					= map makeRow rows
	= (Nothing, "Employees", [toolbar, makeTable ["Name","Description"] rows], cursor)
	where
		makeRow [SQLVVarchar name, SQLVVarchar description] = [ATag [HrefAttr ("/employees/" +++ name)] [Text name], Text description]
		toolbar	= makeToolbar [makeLinkButton "Add employee" "/employees/+add" (Just "add")]

showEmployeePage :: !String !*cur -> (Maybe (String,String), !String, ![HtmlTag], !*cur) | SQLCursor cur & bimap{|*|} cur
showEmployeePage name cursor
	# (mbErr, mbEmployee, cursor)	= mapRead name cursor	//Relational mapping
	| isJust mbErr					= (Nothing, "Error",[Text (toString (fromJust mbErr))],cursor)
	| isNothing mbEmployee			= (Nothing, "Error",[Text ("There is no employee with name " +++ name)], cursor)
	# employee						= fromJust mbEmployee
	= (Nothing, employee.Employee.employee_name,[toolbar,showEmployeeForm employee],cursor)
	where
		toolbar	= makeToolbar [makeLinkButton "Edit" ("/employees/" +++ name +++ "/+edit") (Just "edit"),makeLinkButton "Delete" ("/employees/" +++ name +++ "/+delete") (Just "delete")]

createEmployeePage :: !HTTPRequest !*cur -> (Maybe (String,String), !String, ![HtmlTag], !*cur) | SQLCursor cur & bimap{|*|} cur
createEmployeePage req cursor
	| req.req_method == "POST"
		# employee					= editEmployeeUpd req.arg_post
		# (mbErr,mbName, cursor)	= mapCreate employee cursor //Relational mapping
		| isJust mbErr				= (Nothing, "Error",[Text (toString (fromJust mbErr))],cursor)
		= (Just ("/employees/" +++ fromJust mbName,"Succesfully created employee " +++ fromJust mbName),"",[],cursor)
	| otherwise
		# employee						= defaultEmployee
		# (projects, cursor)			= getProjectOptions cursor
		= (Nothing, "New employee", [editEmployeeForm True employee projects], cursor)

editEmployeePage :: !String !HTTPRequest !*cur -> (Maybe (String, String), !String, ![HtmlTag], !*cur) | SQLCursor cur & bimap{|*|} cur
editEmployeePage name req cursor
	| req.req_method == "POST"
		# employee					= editEmployeeUpd req.arg_post
		# (mbErr,mbName, cursor)	= mapUpdate employee cursor //Relational mapping
		| isJust mbErr				= (Nothing, "Error",[Text (toString (fromJust mbErr))],cursor)
		= (Just ("/employees/" +++ fromJust mbName,"Succesfully updated employee " +++ fromJust mbName),"",[],cursor)
	| otherwise
		# (mbErr, mbEmployee, cursor)	= mapRead name cursor	//Relational mapping
		| isJust mbErr					= (Nothing, "Error",[Text (toString (fromJust mbErr))],cursor)
		| isNothing mbEmployee			= (Nothing, "Error",[Text ("There is no employee with name " +++ name)], cursor)
		# employee						= fromJust mbEmployee
		# (projects, cursor)			= getProjectOptions cursor
		= (Nothing, name,[editEmployeeForm False employee projects],cursor)

deleteEmployeePage :: !String !HTTPRequest !*cur -> (Maybe (String, String), !String, ![HtmlTag], !*cur) | SQLCursor cur & bimap{|*|} cur
deleteEmployeePage name req cursor
	# (mbErr, mbEmployee, cursor)	= mapDelete name cursor	//Relational mapping
	| isJust mbErr					= (Nothing, "Error",[Text (toString (fromJust mbErr))],cursor)
	| isNothing mbEmployee			= (Nothing, "Error",[Text ("There is no employee with name " +++ name)], cursor)
	# employee						= fromJust mbEmployee
	= (Just ("/employees","Successfully deleted employee " +++ employee.Employee.employee_name),"",[],cursor)

//********************************************************************************
// Utility functions
//********************************************************************************
getProjectOptions :: !*cur -> ([ProjectID], *cur) | SQLCursor cur
getProjectOptions cursor
	# (_,cursor)		= execute "SELECT projectNr FROM project" [] cursor
	# (_,rows,cursor)	= fetchAll cursor
	= ([{ProjectID | project_projectNr = x} \\ [SQLVInteger x] <- rows],cursor)

getEmployeeOptions :: !*cur -> ([EmployeeID], *cur) | SQLCursor cur
getEmployeeOptions cursor
	# (_,cursor)		= execute "SELECT name FROM employee" [] cursor
	# (_,rows,cursor)	= fetchAll cursor
	= ([{EmployeeID | employee_name = x} \\ [SQLVVarchar x] <- rows],cursor)

defaultEmployee :: Employee
defaultEmployee = 	{ Employee
					| employee_name								= ""
					, employee_description						= ""
					, projectworkers_project_ofwhich_employee	= []
					}

defaultProject :: Project
defaultProject =	{ Project
					| project_projectNr							= 0
					, project_description						= ""
					, project_parent							= Nothing
					, task_ofwhich_project						= []
					, project_ofwhich_parent					= []
					, projectworkers_employee_ofwhich_project	= []
					}

int :: Int -> Int
int x = x
