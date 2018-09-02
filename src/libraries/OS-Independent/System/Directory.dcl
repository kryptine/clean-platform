definition module System.Directory

from System.File import :: FileInfo
import System._Directory

/**
 * Recursively scan a directory and collect information about all files and
 * directories it contains.
 *
 * @param A function that is used to update a state (`st`) for every file or
 *   directory encountered. A directory is given before everything it contains.
 *   Otherwise, no guarantees w.r.t. order are made.
 * @param The initial state.
 * @param The directory to scan. The update function is also called with this
 *   directory as an argument.
 * @param The world.
 * @result All errors encountered.
 * @result The updated state.
 * @result The new world.
 */
scanDirectory :: !(FilePath FileInfo .st *World -> *(.st, *World)) !.st !FilePath !*World -> *(![OSError], !.st, !*World)
