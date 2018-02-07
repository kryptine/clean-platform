implementation module Testing.TestEvents

import Text.JSON

JSONEncode{|StartEvent|} _ startEvent = [ JSONObject [ ("name",  JSONString startEvent.StartEvent.name)
                                                     , ("event", JSONString "start")
                                                     ]
                                        ]
JSONEncode{|EndEventType|} _ eType = [JSONString eTypeStr]
where
    eTypeStr = case eType of
        Passed  -> "passed"
        Failed  -> "failed"
        Skipped -> "skipped"

JSONDecode{|EndEventType|} _ [JSONString eTypeStr : rest] = (mbEType, rest)
where
    mbEType = case eTypeStr of
        "passed"  -> Just Passed
        "failed"  -> Just Failed
        "skipped" -> Just Skipped
        _         -> Nothing
JSONDecode{|EndEventType|} _ nodes = (Nothing, nodes)

derive JSONEncode EndEvent
derive JSONDecode StartEvent, EndEvent

