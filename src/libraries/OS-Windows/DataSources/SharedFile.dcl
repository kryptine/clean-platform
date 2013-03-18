definition module DataSources.SharedFile

import FilePath, SharedDataSource

sharedFile :: !FilePath !(String -> a) !(a -> String) -> Shared a *World
