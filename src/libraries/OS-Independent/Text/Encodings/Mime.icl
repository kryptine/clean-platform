implementation module Mime

import StdOverloaded, StdString, StdList, StdArray
import Maybe, Text

encodeMimeMultipart :: !String [([(String,String)], String)] -> String
encodeMimeMultipart boundary parts
	= "This is a message with multiple parts in MIME format.\r\n" 
	+++ "--" +++ boundary +++ "\r\n"
	+++ join ("\r\n--" +++ boundary +++ "\r\n") [formatPart headers body  \\ (headers,body) <- parts]
	+++ "\r\n--" +++ boundary +++ "--"
where
	formatPart [] body		= body
	formatPart headers body = join "\r\n" [name +++ ": " +++ value \\ (name,value) <- headers ] +++ "\r\n\r\n" +++ body

decodeMimeMultipart :: !String !String -> [([(String,String)], String)]
decodeMimeMultipart boundary body
	# startindex		= indexOf ("--" +++ boundary +++ "\r\n") body //Locate the first boundary
	| startindex == -1	= [] //Fail
	# endindex			= indexOf ("\r\n" +++ "--" +++ boundary +++ "--") body //Locate the final boundary
	| endindex == -1	= [] //Fail
	# body				= body % (startindex + (size boundary) + 4, endindex - 1)
	# parts				= split ("\r\n" +++ "--" +++ boundary +++ "\r\n") body
	= map parsePart parts
where
	parsePart :: String -> ([(String,String)], String)
	parsePart part 
		# index 		= indexOf "\r\n\r\n" part
		| index < 1 	= ([], part)
						= ([fromJust header \\ header <- map parseHeader (split "\r\n" (part % (0, index - 1))) | isJust header]
							, part % (index + 4, size part))

	parseHeader :: !String -> Maybe (!String, !String)
	parseHeader header
		# index                 = indexOf ":" header
		| index < 1             = Nothing
		# name                  = trim (header % (0, index - 1))
		# value                 = trim (header % (index + 1, size header))
		= Just (name,value)
