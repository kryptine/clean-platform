module SQLDbDemo
/*
* Simple program that illustrates connecting to a MySQL database and some examples of
* functions that insert or select data in a database.
*
* The database schema, with a simple addressbook, for this demo can be found in SQLDbDemo.sql
*/

import StdEnv,Text

import SQL		//Import SQL database API
import MySQL	//Import MySQL databaseimplementation

MYSQL_HOST :== "localhost"
MYSQL_USER :== "root"
MYSQL_PASS :== "test"
MYSQL_NAME :== "addressbook"

:: Contact =
	{ name		:: String
	, phoneNr	:: String
	}

Start :: !*World -> *World
Start world
	# (cursor, connection, context, world) 	= openMySQLDb world
	// ============================================================
	// Uncomment one of these demo actions
	// ============================================================
	
	// Fill the database with test data
	//# cursor								= fillDatabase cursor
	
	// Print all contacts
	//# (cursor,world)						= printAllContacts cursor world
	
	# world 								= closeMySQLDb cursor connection context world
	= world

//Database initialization for a MySQL database
openMySQLDb :: !*World -> (!*MySQLCursor, !*MySQLConnection, !*MySQLContext, !*World)
openMySQLDb world
	# (err,mbContext,world) 	= openContext world
	| isJust err				= abort (toString (fromJust err))
	# (err,mbConn,context)		= openConnection MYSQL_HOST MYSQL_USER MYSQL_PASS MYSQL_NAME (fromJust mbContext)
	| isJust err				= abort (toString (fromJust err))
	# (err,mbCursor,connection)	= openCursor (fromJust mbConn)
	| isJust err				= abort (toString (fromJust err))
	= (fromJust mbCursor,connection, context, world)
				
closeMySQLDb :: !*MySQLCursor !*MySQLConnection !*MySQLContext !*World -> *World
closeMySQLDb cursor connection context world
	# (err,connection)	= closeCursor cursor connection
	# (err,context) 	= closeConnection connection context
	# (err,world)		= closeContext context world
	= world

//Inserting data
fillDatabase :: !*cursor -> *cursor | SQLCursor cursor
fillDatabase cursor = insertContacts contacts cursor
where
	insertContacts [] cursor		= cursor
	insertContacts [c:cs] cursor	= insertContacts cs (insertContact c cursor)

	contacts = [ {Contact| name = "Bas", phoneNr = "1234567890"}
			   , {Contact| name = "Rinus", phoneNr = "0987654321"}
			   ]
	
insertContact :: Contact !*cursor -> *cursor | SQLCursor cursor
insertContact contact cursor
	# (error,cursor)	= execute "INSERT INTO contacts (name,phoneNr) VALUES(?,?)" values cursor
	| isJust error		= abort (toString (fromJust error))
	= cursor
where
	values = [SQLVVarchar contact.name, SQLVVarchar contact.phoneNr]
	
//Selecting data
printAllContacts :: !*cursor !*World -> (*cursor,*World) | SQLCursor cursor
printAllContacts cursor world
	# (error,cursor)		= execute "SELECT * FROM contacts" [] cursor
	| isJust error			= abort (toString (fromJust error))
	# (error,rows,cursor)	= fetchAll cursor
	| isJust error			= abort (toString (fromJust error))	
	# (console,world)		= stdio world	
	# console				= fwrites (join "\n" (map (\row -> join ", " (map toString row)) rows)) console
	# (_,world)				= fclose console world
	= (cursor,world)
	