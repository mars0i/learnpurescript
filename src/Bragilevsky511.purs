-- Bragilevsky sect 5.1.1
module Bragilevsky511 where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)

import Data.Maybe
import Data.Tuple
import Data.Foldable (lookup)


type Name = String
type Phone = String
type Location = String
type PhoneNumbers = Array (Tuple Name Phone)
type Locations = Array (Tuple Phone Location)

locateByName :: PhoneNumbers -> Locations -> Name -> Maybe Location
locateByName pnumbers locs name =
  lookup name pnumbers >>= flip lookup locs
-- Note rhs is ((flip lookup) locs)
-- (flip lookup) makes a function of type: foldable -> key -> val
-- and then by passing locs, a foldable, we get back a function of key -> val

ps = [Tuple "Elliott" "123", Tuple "Christopher" "456", Tuple "Bren" "789"]
ls = [Tuple "123" "bin", Tuple "456" "couch"] -- lookup of Bren's address will fail



main :: Effect Unit
main = do
       logShow $ locateByName ps ls "Elliott"
       logShow $ locateByName ps ls "Christopher"
       logShow $ locateByName ps ls "typo"
       logShow $ locateByName ps ls "Bren"

