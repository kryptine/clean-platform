definition module Text.GenXML.Gast

from Gast          import generic ggen, generic genShow, :: GenState
from Text.GenXML   import :: XMLDoc, :: XMLNode, :: XMLAttr, :: XMLQName

derive ggen    XMLDoc, XMLQName, XMLNode, XMLAttr
derive genShow XMLDoc, XMLQName, XMLNode, XMLAttr
