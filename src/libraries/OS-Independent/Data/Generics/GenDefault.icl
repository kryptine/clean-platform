implementation module Data.Generics.GenDefault

//import StdClass, StdArray, StdInt, StdFunc
import StdGeneric

generic gDefault a ::  a 

gDefault{|Int|}  				= 0
gDefault{|Bool|}  				= False
gDefault{|Real|}  				= 0.0
gDefault{|Char|}  				= '-'
gDefault{|String|}  			= ""
gDefault{|UNIT|} 			 	= UNIT
gDefault{|EITHER|} dl dr   	= LEFT   dl
gDefault{|PAIR|}   dl dr  	= PAIR   dl dr
gDefault{|CONS|}   dc     	= CONS   dc
gDefault{|FIELD|}  df     	= FIELD  df
gDefault{|OBJECT|} do     	= OBJECT do
gDefault{|RECORD|} do     	= RECORD do

derive gDefault (), [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
