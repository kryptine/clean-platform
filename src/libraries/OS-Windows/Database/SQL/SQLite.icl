implementation module Database.SQL.SQLite
//SQLite implementation of the Clean SQL database API
//DUMMY
import Database.SQL
import StdEnv, Data.Maybe, System._Pointer

//SQLite Does not really need a context
:: SQLiteContext :== Int
	
//A wrapper for access to sqlite3 structs
:: SQLiteConnection =
	{ conn_ptr		:: !Pointer
	}
//A wrapper to sqlite3 result sets
:: SQLiteCursor =
	{ conn_ptr		:: !Pointer
	, stmt_ptr	    :: !Pointer
    , step_res      :: !Int
    , num_cols      :: !Int
	}

instance SQLEnvironment World SQLiteContext
where
	//Dummy environment
	openContext :: !*World -> (!(Maybe SQLError),!(Maybe *SQLiteContext),!*World)
	openContext world
        = (Just SQLNotSupportedError, Nothing, world)

	closeContext :: !*SQLiteContext !*World -> (!(Maybe SQLError), !*World)
	closeContext context world
        = (Just SQLNotSupportedError, world)

instance SQLContext SQLiteContext SQLiteConnection
where
	openConnection	:: !SQLDatabase !*SQLiteContext -> (!(Maybe SQLError),!(Maybe *SQLiteConnection),!*SQLiteContext)
	openConnection {SQLDatabase|host,username,password,database} context
        = (Just SQLNotSupportedError, Nothing, context)
	
	closeConnection	:: !*SQLiteConnection !*SQLiteContext -> (!(Maybe SQLError), !*SQLiteContext)
	closeConnection connection=:{SQLiteConnection|conn_ptr} context
        = (Just SQLNotSupportedError, context)

instance SQLConnection SQLiteConnection SQLiteCursor
where
	openCursor :: !*SQLiteConnection -> (!(Maybe SQLError), !(Maybe *SQLiteCursor), !*SQLiteConnection)
	openCursor connection=:{SQLiteConnection|conn_ptr}
        = (Just SQLNotSupportedError, Nothing, connection)

	closeCursor	:: !*SQLiteCursor !*SQLiteConnection -> (!(Maybe SQLError), !*SQLiteConnection)
	closeCursor cursor=:{SQLiteCursor|stmt_ptr} connection
        = (Just SQLNotSupportedError, connection)

instance SQLCursor SQLiteCursor
where
	execute	:: !SQLStatement ![SQLValue] !*SQLiteCursor -> (!(Maybe SQLError), !*SQLiteCursor)
	execute statement values cursor=:{SQLiteCursor|conn_ptr,stmt_ptr}
        = (Just SQLNotSupportedError, cursor)

	executeMany :: !SQLStatement ![[SQLValue]] !*SQLiteCursor -> (!(Maybe SQLError), !*SQLiteCursor)
    executeManye statement values cursor
        = (Just SQLNotSupportedError, cursor)

	insertId :: !*SQLiteCursor -> (!(Maybe SQLError), !Int, !*SQLiteCursor)
	insertId cursor=:{SQLiteCursor|conn_ptr}
        = (Just SQLNotSupportedError, 0, cursor)

    numRows	:: !*SQLiteCursor -> (!(Maybe SQLError), !Int, !*SQLiteCursor)
	numRows	cursor=:{SQLiteCursor|conn_ptr}
        = (Just SQLNotSupportedError, 0, cursor)
	
	numFields :: !*SQLiteCursor -> (!(Maybe SQLError), !Int, !*SQLiteCursor)
	numFields cursor=:{SQLiteCursor|stmt_ptr,num_cols}
        = (Just SQLNotSupportedError, 0, cursor)

	fetchOne :: !*SQLiteCursor -> (!(Maybe SQLError), !(Maybe SQLRow), !*SQLiteCursor)
	fetchOne cursor=:{SQLiteCursor|step_res=SQLITE_DONE}
        = (Just SQLNotSupportedError, Nothing, cursor)

	fetchMany :: !Int !*SQLiteCursor -> (!(Maybe SQLError), ![SQLRow], !*SQLiteCursor)
	fetchMany n cursor
        = (Just SQLNotSupportedError, [], cursor)

	fetchAll :: !*SQLiteCursor -> (!(Maybe SQLError), ![SQLRow], !*SQLiteCursor)
	fetchAll cursor
        = (Just SQLNotSupportedError, [], cursor)

	commit :: !*SQLiteCursor -> (!(Maybe SQLError), !*SQLiteCursor)
	commit cursor = (Just SQLNotSupportedError, cursor)

	rollback :: !*SQLiteCursor -> (!(Maybe SQLError), !*SQLiteCursor)
	rollback cursor = (Just SQLNotSupportedError, cursor)
