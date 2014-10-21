definition module DataSources.SharedFile

import System.FilePath, Data.SharedDataSource

sharedFile :: !FilePath !(String -> a) !(a -> String) -> Shared a *World
