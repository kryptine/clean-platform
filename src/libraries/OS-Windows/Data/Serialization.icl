implementation module Serialization

import StdEnv
import StdDynamicLowLevelInterface
import DynamicGraphConversion
from DynamicUtilities import WriteLong, NF
import Directory
import DynID
import md5
import DynamicLinkerInterface
import StdDynamicTypes
import StdDynamic

from Maybe import qualified ::Maybe(..)
import Error
import File

import _Unsafe

serialize :: !a -> String | TC a
serialize value = serializeDynamic (dynamic value)

deserialize :: !String -> 'Maybe'.Maybe a | TC a
deserialize str = 
	case deserializeDynamic str of
		'Maybe'.Just dyn = unpack dyn
		'Maybe'.Nothing  = 'Maybe'.Nothing
	where
		unpack :: Dynamic -> 'Maybe'.Maybe a | TC a
		unpack (value :: a^) = 'Maybe'.Just value
		unpack _ = 'Maybe'.Nothing

serializeDynamic :: !Dynamic -> String
serializeDynamic dynamic_value
	// this forces evaluation of the type part of the dynamic
	| typeCodeOfDynamic dynamic_value == TypeVar (-1)
		= abort "Data.Serialize, dynamicToString: malformed dynamic type"
	# (_,encoded_dynamic)
		= dynamic_to_string dynamic_value
	= write_encoded_dynamic encoded_dynamic
where
	write_encoded_dynamic {ed_encoded_graph,ed_dynamic_rts_info}
		// Offset/Size
		# (s_ed_encoded_graph,ed_encoded_graph)
			= usize ed_encoded_graph
		# ed_encoded_graph
			= WriteLong ed_encoded_graph (DYNAMIC_RTS_INFO_OFFSET - HEADER_SIZE_OFFSET) s_ed_encoded_graph
		
		# (s_ed_dynamic_rts_info,ed_dynamic_rts_info)
			= usize ed_dynamic_rts_info
		# ed_encoded_graph
			= WriteLong ed_encoded_graph (DYNAMIC_RTS_INFO_SIZE - HEADER_SIZE_OFFSET) s_ed_dynamic_rts_info
		= ed_encoded_graph +++ ed_dynamic_rts_info

deserializeDynamic :: !String -> 'Maybe'.Maybe Dynamic
deserializeDynamic dynamic_string = accUnsafe (stringToDynamic dynamic_string) 
where
	stringToDynamic :: !String !*World -> ('Maybe'.Maybe Dynamic, *World)
	stringToDynamic dynamic_string world
		# md5_dynamic_id = getMd5DigestFromString dynamic_string
		# system_dynamic_filename = CONVERTED_ENCODED_DYNAMIC_FILE_NAME_INTO_PATH GetDynamicLinkerPath md5_dynamic_id
		# (res, world) = writeFile system_dynamic_filename dynamic_string world
		| isError res = ('Maybe'.Nothing, world)
		# (ok, dyn, world) = readDynamic` system_dynamic_filename world
		# (_, world) = deleteFile system_dynamic_filename world
		| not ok = ('Maybe'.Nothing, world)
		= ('Maybe'.Just dyn, world)

readDynamic` :: String *World -> (Bool,Dynamic,*World)
readDynamic` file_name world
	#! (ok,file_name,dynamic_header=:{block_table_i,graph_i},file,world)
		= open_dynamic_for_read file_name world
	| not ok
		= (False,undef,world)

	// initializes the dynamic info-structure (gdid)
	#! (ok,gdid,file)
		= init_dynamic file_name dynamic_header file
	| not ok
		= abort "readDynamic: error; more detailled error information must be implemented";

	// construct top-level dynamic
	#! dyn
		= build_block (NF make_start_node_index) (NF { gdid & gdid.di_dummy = ""})
		
	#! (_,world)
		= close_dynamic_as_binary file world
	= (ok,dyn,world)

open_dynamic_for_read file_name1 world 
	#! (ok,file_name,world)
		= FILE_IDENTIFICATION (get_system_dynamic_identification file_name1 world) (True,file_name1,world)
	| not ok
		= (False,"",default_dynamic_header,stderr,world)

	#! (ok,dynamic_header,file1,world)
		= open_dynamic_as_binary file_name world
	| not ok
		#! (_,world)
			= close_dynamic_as_binary file1 world
		= (False,"",dynamic_header,stderr,world)
		
		= (True,file_name,dynamic_header,file1,world)

WriteLong :: !*{#Char} !Int !Int -> *{#Char}
WriteLong array i v
	= { array & [i] 	= (toChar v)		,	[i+1] = (toChar (v>>8)),
				[i+2]	= (toChar (v>>16))  ,	[i+3] = (toChar (v>>24))}
