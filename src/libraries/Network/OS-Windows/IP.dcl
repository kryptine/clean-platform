definition module IP
/**
* Small module which provides basic IP functionality
*/
import StdString
import Maybe

/**
* Type which represents an IP (v4) address
*/
:: IPAddress

/**
* Convert an IP address to and from its 'dotted decimal' string representation
*/
instance toString IPAddress
instance fromString IPAddress


/**
* Looks up a DNS name (e.g www.example.com) and returns an IP address on success
*/
lookupIPAddress :: !String !*World -> (!Maybe IPAddress, !*World)
