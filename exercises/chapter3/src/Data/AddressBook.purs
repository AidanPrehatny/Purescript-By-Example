module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)

type Address = 
    {street :: String
    , city :: String
    , state :: String
    }

type Entry =
    { firstName :: String
    , lastName :: String
    , address :: Address
    }

type AddressBook = List Entry