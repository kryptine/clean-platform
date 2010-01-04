definition module PMDatabase

import StdEnv, Maybe
import SQL, MySQL
import PMDataModel

//Database initialization and finalization
initDatabase :: !String !String !String !String !*World -> (!*MySQLContext, !*MySQLConnection, !*MySQLCursor, *World)
endDatabase :: !*MySQLContext !*MySQLConnection !*MySQLCursor !*World -> *World

//Example of manually written database operation
updateProject :: Project !*cur -> (Maybe SQLError, *cur) | SQLCursor cur
