implementation module Network.IP
/**
* Small module which provides basic IP functionality
*/
import StdString, StdInt, StdArray
import Data.Maybe, Text, System._Pointer
import code from library "wsock32.txt"

/**
* Type which represents an IP (v4) address
* We can fit an IP address into a 32 bit integer by putting the four quads in big endian order (network byte order)
*/
:: IPAddress = IPAddress Int

/**
* Convert an IP address to and from its 'dotted decimal' string representation
*/
instance toString IPAddress
where
	toString (IPAddress ip)	= toString b1 +++ "." +++ toString b2 +++ "." +++ toString b3 +++ "." +++ toString b4
	where
		b1	=  ip        bitand 255
		b2	= (ip >> 8)  bitand 255
		b3	= (ip >> 16) bitand 255
		b4	= (ip >> 24) bitand 255

instance fromString IPAddress
where
	fromString s = case (split "." s) of 
		[b1,b2,b3,b4]	= IPAddress ((toInt b1) + ((toInt b2) << 8) + ((toInt b3) << 16) + ((toInt b4) << 24))
		_				= IPAddress 0

instance toInt IPAddress
where
	toInt (IPAddress ip)	= ip

instance fromInt IPAddress
where
	fromInt i				= IPAddress i

/**
* Looks up a DNS name (e.g www.example.com) and returns an IP address on success
*/
lookupIPAddress :: !String !*World -> (!Maybe IPAddress, !*World)
lookupIPAddress name world
	# (_,world)		= WSAStartupC 1 (createArray 4 0) world //We supply a bogus array of 16 bytes to store the WSADATA struct in
	# (ptrhe,world) = gethostbynameC (packString name) world	
	| ptrhe == 0	= (Nothing, world)
	# ptrli			= readInt ptrhe 12
	# ptrad			= readInt ptrli 0
	# addr			= readInt ptrad 0
	| addr == addr
		# (_,world)		= WSACleanupC world
		= (Just (IPAddress addr), world)
	where
		WSAStartupC :: !Int !{#Int} !*World -> (!Int, !*World)
		WSAStartupC a0 a1 a2 = code {
			ccall WSAStartup@6 "PIA:I:A"
		}
		WSACleanupC :: !*World -> (!Int, !*World)
		WSACleanupC a0 = code {
			ccall WSACleanup@0 "P:I:A"
		}
		gethostbynameC :: !{#Char} !*World -> (!Pointer, !*World)
		gethostbynameC a0 a1 = code {
			ccall gethostbyname@4 "Ps:I:A"
		}
