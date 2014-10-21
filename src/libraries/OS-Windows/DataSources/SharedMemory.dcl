definition module DataSources.SharedMemory

import Data.SharedDataSource

:: Memory :== Int

sharedMemory :: !a !*envC -> (!Shared a *envS, !*envC) | MemoryEnv envC & MemoryEnv envS

class MemoryEnv env
where
	accMemory :: !(*Memory -> (!a,!*Memory)) !*env -> (!a,!*env)
	
instance MemoryEnv World
