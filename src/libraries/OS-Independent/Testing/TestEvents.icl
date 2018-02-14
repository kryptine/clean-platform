implementation module Testing.TestEvents

import Text.JSON, Control.Monad, StdFunc, StdTuple, StdList, Data.Maybe, Control.Applicative

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
    getField fieldName = case filter ((==) fieldName o fst) objFields of
        [(_, jsonNode)] -> fromJSON jsonNode
        _               -> mzero
JSONDecode{|StartEvent|} _ _ = (Nothing, [])

JSONEncode{|EndEventType|} _ eType = [JSONString eTypeStr]
where
    eTypeStr = case eType of
        Passed  -> "passed"
        Failed  -> "failed"
        Skipped -> "skipped"
        Lost    -> "lost"

JSONDecode{|EndEventType|} _ [JSONString eTypeStr : rest] = (mbEType, rest)
where
    mbEType = case eTypeStr of
        "passed"  -> Just Passed
        "failed"  -> Just Failed
        "skipped" -> Just Skipped
        "lost"    -> Just Lost
        _         -> Nothing
JSONDecode{|EndEventType|} _ nodes = (Nothing, nodes)

derive JSONEncode EndEvent
derive JSONDecode EndEvent
