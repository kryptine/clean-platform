module SQLDbDemo

import StdEnv

import SQL
import MySQL
import Text

Start :: !*World -> (!String,!*World)
Start world
	# (cursor, connection, context, world) 	= dbInit world
	//# (err,cursor)							= execute "INSERT INTO project VALUES(NULL,?,NULL)" [SQLVVarchar "testje"] cursor
	# (err,cursor)							= execute "SELECT * FROM project" [] cursor
	| isJust err	= (toString (fromJust err),world)
	//# (err,id,cursor)						= insertId cursor
	# (err,mbRow,cursor)					= fetchOne cursor
	| isJust err	= (toString (fromJust err),world)
	# world 								= dbEnd cursor connection context world
	= (foldr (+++) " " (map toString (fromJust mbRow)),world)
where
	dbInit :: !*World -> (!*MySQLCursor, !*MySQLConnection, !*MySQLContext, !*World)
	dbInit world
		# (err,mbContext,world) 	= openContext world
		| isJust err				= abort (toString (fromJust err))
		# (err,mbConn,context)		= openConnection "localhost" "root" "test" "pmdemo" (fromJust mbContext)
		| isJust err				= abort (toString (fromJust err))
		# (err,mbCursor,connection)	= openCursor (fromJust mbConn)
		| isJust err				= abort (toString (fromJust err))
		= (fromJust mbCursor,connection, context, world)
		
		
	dbEnd :: !*MySQLCursor !*MySQLConnection !*MySQLContext !*World -> *World
	dbEnd cursor connection context world
		# (err,connection)	= closeCursor cursor connection
		# (err,context) 	= closeConnection connection context
		# (err,world)		= closeContext context world
		= world