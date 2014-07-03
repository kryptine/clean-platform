definition module Database.SQL.SQLite
//This module defines implements the interface for relatational databases
//of SQL.dcl for the SQLite database engine

//CAREFUL: THIS IS A DUMMY IMPLEMENTATION, IT JUST RETURNS NOT IMPLEMENTED ERRORS
import Database.SQL
import Data.Maybe, StdString

:: SQLiteContext
:: SQLiteConnection
:: SQLiteCursor

instance SQLEnvironment		World			    SQLiteContext
instance SQLContext			SQLiteContext	    SQLiteConnection
instance SQLConnection		SQLiteConnection	SQLiteCursor
instance SQLCursor			SQLiteCursor
