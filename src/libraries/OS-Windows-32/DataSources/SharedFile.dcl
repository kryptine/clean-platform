definition module SharedFile

import FilePath, SharedDataSource

sharedFile :: !FilePath -> SymmetricSharedDataSource String *World