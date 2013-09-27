definition module Data.Unicode.Encodings.UTF8

// Ported to Clean from GHC by László Domoszlai, 2013-09-27
// http://hackage.haskell.org/package/utf8-string-0.3.6/docs/src/Codec-Binary-UTF8-String.html
//
// Module      :  Codec.Binary.UTF8.String
// Copyright   :  (c) Eric Mertens 2007
// License     :  BSD3-style (see LICENSE)
// 
// Maintainer:    emertens@galois.com
// Stability   :  experimental
// Portability :  portable
//
// Support for encoding UTF8 Strings to and from @[Word8]@

import Data.Unicode.UString, Data.Word8

// | Encode a string using 'encode' and store the result in a 'String'.
encodeString :: UString -> String

// | Decode a string using 'decode' using a 'String' as input.
// | This is not safe but it is necessary if UTF-8 encoded text
// | has been loaded into a 'String' prior to being decoded.
decodeString :: String -> UString

// | Encode a Haskell String to a list of Word8 values, in UTF8 format.
encode :: UString -> [Word8]

// | Decode a UTF8 string packed into a list of Word8 values, directly to String
decode :: [Word8] -> UString

