implementation module Database.SQL.SQLite
//SQLite implementation of the Clean SQL database API
//
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

SQLITE_OK       :== 0
SQLITE_ERROR    :== 1
SQLITE_ROW      :== 100
SQLITE_DONE     :== 101
SQLITE_INTEGER  :== 1
SQLITE_FLOAT    :== 2
SQLITE_TEXT     :== 3
SQLITE_BLOB     :== 4
SQLITE_NULL     :== 5

sqlite3_open :: !{#Char} -> (!Int,!Pointer)
sqlite3_open a0 = code {
	ccall sqlite3_open "s:Ip"
}
sqlite3_close :: !Pointer -> Int
sqlite3_close a0 = code {
    ccall sqlite3_close "p:I"
}
sqlite3_errcode :: !Pointer -> Int
sqlite3_errcode a0 = code {
    ccall sqlite3_errcode "p:I"
}
sqlite3_errmsg :: !Pointer -> Pointer
sqlite3_errmsg a0 = code {
    ccall sqlite3_errmsg "p:p"
}
sqlite3_prepare :: !Pointer !{#Char} !Int -> (!Int,!Pointer,!Pointer)
sqlite3_prepare a0 a1 a2 = code {
    ccall sqlite3_prepare "psI:Ipp"
}
sqlite3_step :: !Pointer -> Int
sqlite3_step a0 = code {
    ccall sqlite3_step "p:I"
}
sqlite3_finalize :: !Pointer -> Int
sqlite3_finalize a0 = code {
    ccall sqlite3_finalize "p:I"
}
sqlite3_column_count :: !Pointer -> Int
sqlite3_column_count a0 = code {
    ccall sqlite3_column_count "p:I"
}
sqlite3_column_type :: !Pointer !Int -> Int
sqlite3_column_type a0 a1 = code {
    ccall sqlite3_column_type "pI:I"
}
sqlite3_column_int :: !Pointer !Int -> Int
sqlite3_column_int a0 a1 = code {
    ccall sqlite3_column_int "pI:I"
}
sqlite3_column_text :: !Pointer !Int -> Pointer
sqlite3_column_text a0 a1 = code {
    ccall sqlite3_column_text "pI:p"
}
sqlite3_last_insert_rowid :: !Pointer -> Int
sqlite3_last_insert_rowid a0 = code {
    ccall sqlite3_last_insert_rowid "p:I"
}
sqlite3_changes :: !Pointer -> Int
sqlite3_changes a0 = code {
    ccall sqlite3_changes "p:I"
}

instance SQLEnvironment World SQLiteContext
where
	//Dummy environment
	openContext :: !*World -> (!(Maybe SQLError),!(Maybe *SQLiteContext),!*World)
	openContext world = (Nothing, Just 42, world)

	closeContext :: !*SQLiteContext !*World -> (!(Maybe SQLError), !*World)
	closeContext context world = (Nothing, world)

instance SQLContext SQLiteContext SQLiteConnection
where
	openConnection	:: !SQLDatabase !*SQLiteContext -> (!(Maybe SQLError),!(Maybe *SQLiteConnection),!*SQLiteContext)
	openConnection {SQLDatabase|host,username,password,database} context
		//Initialize a handle
		# (rc,conn_ptr) = sqlite3_open (packString database)
        | rc <> SQLITE_OK
			# errno 	= sqlite3_errcode conn_ptr
			# errmsg	= derefString (sqlite3_errmsg conn_ptr)
			| errno <> errno || errmsg <> errmsg	= undef //Force eval
            = (Just (SQLDatabaseError errno errmsg), Nothing, context)
		= (Nothing, Just {SQLiteConnection|conn_ptr = conn_ptr}, context)
		
	closeConnection	:: !*SQLiteConnection !*SQLiteContext -> (!(Maybe SQLError), !*SQLiteContext)
	closeConnection connection=:{SQLiteConnection|conn_ptr} context
        # rc = sqlite3_close conn_ptr
        | rc <> SQLITE_OK
			# errno 	= sqlite3_errcode conn_ptr
			# errmsg	= derefString (sqlite3_errmsg conn_ptr)
			| errno <> errno || errmsg <> errmsg	= undef //Force eval
            = (Just (SQLDatabaseError errno errmsg), context)
		= (Nothing, context)

instance SQLConnection SQLiteConnection SQLiteCursor
where
	openCursor :: !*SQLiteConnection -> (!(Maybe SQLError), !(Maybe *SQLiteCursor), !*SQLiteConnection)
	openCursor connection=:{SQLiteConnection|conn_ptr}
		# cursor = {SQLiteCursor
					| conn_ptr		= conn_ptr
					, stmt_ptr	    = 0
                    , step_res      = 0
                    , num_cols      = 0
					}
		= (Nothing, Just cursor, connection)
	
	closeCursor	:: !*SQLiteCursor !*SQLiteConnection -> (!(Maybe SQLError), !*SQLiteConnection)
	closeCursor cursor=:{SQLiteCursor|stmt_ptr} connection
		| stmt_ptr == 0
			= (Nothing, connection)
		# rc = sqlite3_finalize stmt_ptr
		| rc <> rc = undef	// Force eval
		= (Nothing,connection)

instance SQLCursor SQLiteCursor
where
	execute	:: !SQLStatement ![SQLValue] !*SQLiteCursor -> (!(Maybe SQLError), !*SQLiteCursor)
	execute statement values cursor=:{SQLiteCursor|conn_ptr,stmt_ptr}
        //Free finalize previous query
		# rc                        = if (stmt_ptr <> 0) (sqlite3_finalize stmt_ptr) 0
		| rc <> rc = undef	// Force eval
        //Create statement
        # (rc,stmt_ptr,_)           = sqlite3_prepare conn_ptr statement (size statement)
        | rc <> SQLITE_OK
			# errno 	= sqlite3_errcode conn_ptr
			# errmsg	= derefString (sqlite3_errmsg conn_ptr)
			| errno <> errno || errmsg <> errmsg	= undef //Force eval
            = (Just (SQLDatabaseError errno errmsg), cursor)
        //Get column count
        # num_cols = sqlite3_column_count stmt_ptr
		| num_cols <> num_cols = undef	// Force eval
        //Bind parameters
        //TODO
        //Step once to actually start executing the query
        # rc                        = sqlite3_step stmt_ptr
        | rc == SQLITE_ERROR
			# errno 	= sqlite3_errcode conn_ptr
			# errmsg	= derefString (sqlite3_errmsg conn_ptr)
			| errno <> errno || errmsg <> errmsg	= undef //Force eval
            = (Just (SQLDatabaseError errno errmsg), cursor)
        = (Nothing, {SQLiteCursor|cursor & stmt_ptr = stmt_ptr, step_res = rc, num_cols = num_cols})
        /*
    where
		//Convert an SQLValue to a string which is properly escaped for inclusion in an SQL statement
		formatSQLValue :: !SQLValue !*SQLiteCursor -> (!String, !*SQLiteCursor)
		formatSQLValue (SQLVChar s) cursor
			# (s, cursor) = escapeString s cursor
			= ("'" +++ s +++ "'", cursor)
		formatSQLValue (SQLVVarchar s) cursor
			# (s, cursor) = escapeString s cursor
			= ("'" +++ s +++ "'", cursor)
		formatSQLValue (SQLVText s) cursor
			# (s, cursor) = escapeString s cursor
			= ("'" +++ s +++ "'", cursor)
		formatSQLValue (SQLVInteger i) cursor = (toString i, cursor)
		formatSQLValue (SQLVReal r) cursor = (toString r, cursor)
		formatSQLValue (SQLVFloat f) cursor = (toString f, cursor)
		formatSQLValue (SQLVDouble d) cursor = (toString d, cursor)
		formatSQLValue (SQLVDate d) cursor = ("'" +++ toString d +++  "'", cursor)
		formatSQLValue (SQLVTime t) cursor = ("'" +++ toString t +++ "'", cursor)
		formatSQLValue (SQLVTimestamp t) cursor = (toString t, cursor)
		formatSQLValue (SQLVDatetime d t) cursor = ("'" +++ toString d +++ " " +++ toString t +++ "'", cursor) 
		formatSQLValue (SQLVEnum s) cursor
			# (s, cursor) = escapeString s cursor
			= ("'" +++ s +++ "'", cursor)
		formatSQLValue (SQLVNull) cursor = ("NULL", cursor)
		formatSQLValue (SQLVUnknown s) cursor
			# (s, cursor) = escapeString s cursor
			= ("'" +++ s +++ "'", cursor)
	    */	

	executeMany :: !SQLStatement ![[SQLValue]] !*SQLiteCursor -> (!(Maybe SQLError), !*SQLiteCursor)
	executeMany statement [] 	cursor = (Nothing, cursor)
	executeMany statement [x:xs] cursor
		# (error, cursor)	= execute statement x cursor
		| isJust error	= (error, cursor)
						= executeMany statement xs cursor

	insertId :: !*SQLiteCursor -> (!(Maybe SQLError), !Int, !*SQLiteCursor)
	insertId cursor=:{SQLiteCursor|conn_ptr}
        # insertId = sqlite3_last_insert_rowid conn_ptr
        | insertId <> insertId = undef //Force eval
        = (Nothing, insertId, cursor)
	
	numRows	:: !*SQLiteCursor -> (!(Maybe SQLError), !Int, !*SQLiteCursor)
	numRows	cursor=:{SQLiteCursor|conn_ptr} //TODO: This now does not count for select queries
        # num = sqlite3_changes conn_ptr
        | num <> num = undef //Force eval
        = (Nothing, num, cursor)
	
	numFields :: !*SQLiteCursor -> (!(Maybe SQLError), !Int, !*SQLiteCursor)
	numFields cursor=:{SQLiteCursor|stmt_ptr,num_cols}
        = (Nothing, num_cols, cursor)

	fetchOne :: !*SQLiteCursor -> (!(Maybe SQLError), !(Maybe SQLRow), !*SQLiteCursor)
	fetchOne cursor=:{SQLiteCursor|step_res=SQLITE_DONE}
        = (Nothing, Nothing, cursor)
    fetchOne cursor=:{SQLiteCursor|conn_ptr,stmt_ptr,step_res=SQLITE_ROW,num_cols}
        //Fetch rows
        # (row,cursor)  = foldr readField ([],cursor) [0..(num_cols - 1)]
        | length row < 0 = undef //Force eval
        //Step to next row
        # rc            = sqlite3_step stmt_ptr
        | rc == SQLITE_ERROR
			# errno 	= sqlite3_errcode conn_ptr
			# errmsg	= derefString (sqlite3_errmsg conn_ptr)
			| errno <> errno || errmsg <> errmsg	= undef //Force eval
            = (Just (SQLDatabaseError errno errmsg),Nothing, cursor)
        = (Nothing, Just row, {cursor & step_res = rc})
    where
		readField :: !Int (![SQLValue],!*SQLiteCursor) -> (![SQLValue], !*SQLiteCursor)
        readField i (row,cursor=:{SQLiteCursor|stmt_ptr})
            # type = sqlite3_column_type stmt_ptr i
            | type == SQLITE_INTEGER
                # val = sqlite3_column_int stmt_ptr i
                | val <> val = undef //Force eval
                = ([SQLVInteger val:row],cursor)
            | type == SQLITE_FLOAT
                # val = toReal (derefString (sqlite3_column_text stmt_ptr i))
                | val <> val = undef //Force eval
                = ([SQLVFloat val:row],cursor)
            | type == SQLITE_TEXT
                # val = derefString (sqlite3_column_text stmt_ptr i)
                | size val < 0 = undef //Force eval
                = ([SQLVText val:row],cursor)
            | type == SQLITE_BLOB
                # val = derefString (sqlite3_column_text stmt_ptr i)
                | size val < 0 = undef //Force eval
                = ([SQLVText val:row],cursor)
            | type == SQLITE_NULL
                = ([SQLVNull:row],cursor)
    fetchOne cursor
	    = (Just (SQLProgrammingError 1 "You cannot fetch a row when there is no result set") ,Nothing, cursor)

	fetchMany :: !Int !*SQLiteCursor -> (!(Maybe SQLError), ![SQLRow], !*SQLiteCursor)
	fetchMany 0 cursor = (Nothing, [], cursor)
	fetchMany n cursor
		# (error, row, cursor) = fetchOne cursor
		= case row of	Nothing		= (Nothing, [], cursor)
						(Just x)
							# (error, xs, cursor) = fetchMany (n - 1) cursor
							= (error, [x:xs], cursor)

	fetchAll :: !*SQLiteCursor -> (!(Maybe SQLError), ![SQLRow], !*SQLiteCursor)
	fetchAll cursor
		# (error, row, cursor) = fetchOne cursor
		= case row of	Nothing 	= (Nothing, [], cursor)
						(Just x)
							# (error, xs, cursor) = fetchAll cursor
							= (error, [x:xs], cursor)

	commit :: !*SQLiteCursor -> (!(Maybe SQLError), !*SQLiteCursor)
	commit cursor = (Just SQLNotSupportedError, cursor)

	rollback :: !*SQLiteCursor -> (!(Maybe SQLError), !*SQLiteCursor)
	rollback cursor = (Just SQLNotSupportedError, cursor)

