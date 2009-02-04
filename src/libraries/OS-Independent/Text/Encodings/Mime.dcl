definition module Mime

encodeMimeMultipart :: !String [([(String,String)], String)] -> String
decodeMimeMultipart :: !String !String -> [([(String,String)], String)]
