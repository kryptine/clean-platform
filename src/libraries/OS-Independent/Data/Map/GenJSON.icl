implementation module Data.Map.GenJSON

import StdEnv
import Data.Maybe
from Data.Map import :: Map, fromList, toList, toAscList, foldrWithKey
import Text.GenJSON

JSONEncode{|Map|} jek jev b map
	= [JSONObject [(toString (JSONArray (jek b k)), JSONArray (jev b v))\\(k, v)<-toList map]]
JSONDecode{|Map|} jdk jdv b a=:[JSONObject kvs:rest]
	# items = map parseNode kvs
	| length (filter isNothing items) > 0 = (Nothing, a)
	= (Just (fromList (map fromJust items)), rest)
where
	parseNode (k, JSONArray v)
		= case fromString k of
			JSONArray k = case jdk b k of
				(Just k, []) = case jdv b v of
					(Just v, []) = Just (k, v)
					(_, r) = Nothing
				(_, r) = Nothing
			_ = Nothing
JSONDecode{|Map|} _ _ _ nodes = (Nothing, nodes)
