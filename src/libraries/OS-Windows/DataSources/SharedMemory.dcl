definition module SharedMemory

import SharedDataSource

sharedMemory :: !a !*World -> (!Shared a World, !*World)