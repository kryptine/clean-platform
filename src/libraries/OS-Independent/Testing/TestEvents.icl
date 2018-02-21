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

	getField :: String -> Maybe a | JSONDecode{|*|} a
	getField field = lookup field fields >>= fromJSON
JSONDecode{|EndEvent|} _ _ = (Nothing, [])

JSONEncode{|FailedAssertion|} _ fa = [JSONArray arr]
where
	arr = case fa of
		ExpectedRelation x r y ->
			[ JSONString "expected"
			, x
			, hd (JSONEncode{|*|} False r)
			, y
			]

JSONDecode{|FailedAssertion|} _ [JSONArray arr:rest] = (mbFA, rest)
where
	mbFA = case arr of
		[JSONString "expected":x:r:y:[]] -> case JSONDecode{|*|} False [r] of
			(Just r, []) -> Just (ExpectedRelation x r y)
			_ -> Nothing
		_ -> Nothing

JSONEncode{|Relation|} _ r = [JSONString s]
where
	s = case r of
		Eq -> "=="
		Ne -> "<>"
		Lt -> "<"
		Le -> "<="
		Gt -> ">"
		Ge -> ">="

JSONDecode{|Relation|} _ [JSONString s:rest] = (mbRel, rest)
where
	mbRel = case s of
		"==" -> Just Eq
		"<>" -> Just Ne
		"<"  -> Just Lt
		"<=" -> Just Le
		">"  -> Just Gt
		">=" -> Just Ge
		_    -> Nothing

derive JSONEncode FailReason, CounterExample
derive JSONDecode FailReason, CounterExample
