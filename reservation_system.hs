--Fabian Ehrentraud, Bernhard Urban, 2011

{- TODO

	try out HXT example from http://www.haskell.org/haskellwiki/HXT
		which just copies an XML file
		
	build a text-ui wit IO monad for user interaction

-}

module Main
where
 
import Text.XML.HXT.Core
--import Text.XML.HXT.Curl -- use libcurl for HTTP access, only necessary when reading http://...
 
import System.Environment
 
main :: IO ()
main
	= do
		[src, dst] <- getArgs
		runX ( readDocument [withValidate no
                            ] src
		       >>>
		       writeDocument [withIndent yes
		                     ,withOutputEncoding utf8
		                     ] dst
		     )
		return ()

