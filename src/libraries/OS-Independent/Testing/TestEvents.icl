implementation module Testing.TestEvents

import Text.JSON, Control.Monad, StdFunc, StdTuple, StdList, Data.Maybe, Control.Applicative
import Data.Functor
import Data.List

JSONEncode{|TestEvent|} c (StartEvent se) = JSONEncode{|*|} c se
JSONEncode{|TestEvent|} c (EndEvent ee)   = JSONEncode{|*|} c ee

JSONDecode{|TestEvent|} b json = case JSONDecode{|*|} b json of
	(Just se, json) -> (Just (StartEvent se), json)
	_               -> case JSONDecode{|*|} b json of
		(Just ee, json) -> (Just (EndEvent ee), json)
		(Nothing, json) -> (Nothing, json)

JSONEncode{|StartEvent|} _ startEvent = [ JSONObject [ ("name",  JSONString startEvent.StartEvent.name)
                                                     , ("event", JSONString "start")
                                                     ]
                                        ]

JSONDecode{|StartEvent|} _ [JSONObject objFields : rest] = (mbEvent, rest)
where
    mbEvent :: Maybe StartEvent
    mbEvent = getField "name"  >>= \name  ->
              getField "event" >>= \event ->
              if (event == "start")
                 (pure {StartEvent | name = name})
                 mzero

	getField :: String -> Maybe a | JSONDecode{|*|} a
	getField field = lookup field objFields >>= fromJSON
JSONDecode{|StartEvent|} _ _ = (Nothing, [])

JSONEncode{|EndEvent|} _ endEvent = [JSONObject
	[ ("name", JSONString endEvent.EndEvent.name)
	, ("message", JSONString endEvent.message)
	, ("event", JSONString (typeToString endEvent.event))
	: case endEvent.event of
		Failed r -> [("failReason", case JSONEncode{|*|} False r of
			[JSONArray r] -> JSONArray r
			r             -> JSONArray r)]
		_        -> []
	]]
where
	typeToString :: EndEventType -> String
	typeToString Passed     = "passed"
	typeToString (Failed r) = "failed"
	typeToString Skipped    = "skipped"
	typeToString Lost       = "lost"

JSONDecode{|EndEvent|} _ [JSONObject fields:rest] = (mbEvent, rest)
where
	mbEvent :: Maybe EndEvent
	mbEvent =
		getField "name" >>= \name ->
		getField "event" >>= \event ->
		getField "message" >>= \message ->
		let e = {name=name, message=message, event=Passed} in case event of
			"passed"  -> pure e
			"failed"  -> (\r -> {e & event=Failed r}) <$> getField "failReason"
			"skipped" -> pure {e & event=Skipped}
			"lost"    -> pure {e & event=Lost}

	getField :: String -> Maybe a | JSONDecode{|*|} a
	getField field = lookup field fields >>= fromJSON
JSONDecode{|EndEvent|} _ _ = (Nothing, [])

JSONDecode{|EndEventType|} b [JSONString "failed" : rest] = case JSONDecode{|*|} b rest of
	(Just r,rest) -> (Just (Failed r), rest)
	_             -> (Nothing, rest)
JSONDecode{|EndEventType|} _ [JSONString eTypeStr : rest] = (mbEType, rest)
where
    mbEType = case eTypeStr of
        "passed"  -> Just Passed
        "skipped" -> Just Skipped
        "lost"    -> Just Lost
        _         -> Nothing
JSONDecode{|EndEventType|} _ nodes = (Nothing, nodes)

derive JSONEncode FailReason
derive JSONDecode FailReason
