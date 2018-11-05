module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
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

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                entry.firstName <> ", " <>
                showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                    addr.city <> ", " <>
                    addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

-- infixr 0 apply as $
-- $ is an alias for the regular function apply:
-- apply :: forall a b. (a -> b) -> a -> b
-- apply f x = f x

-- Instead of street (address (boss employee)),
--      we convert it to street $ address $ boss employee

-- findEntry firstName lastName book = head $ filter filterEntry book 

-- *** argument book is passed to the composition of the functions ```filter filterEntry``` and ```head``` ***
-- to simplify, we can pass a function composition operator <<< or >>> 

--      findEntry firstName lastName = head <<< filter filterEntry

-- filter :: (Entry -> Boolean) -> AddressBook -> AddressBook
-- head :: AddressBook -> Maybe Entry
findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = filter filterEntry >>> head
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

-- to call findEntry, create Address & Entry, add address to entry
johnAddr :: Address
johnAddr = {
    street: "502 Penn Rd",
    city: "Philadelphia",
    state: "Pennsylvania"
}

johnEntry :: Entry
johnEntry = {
    firstName: "John",
    lastName: "Smith",
    address: johnAddr
}

-- create Function to insert entry into AddressBook
johnAddy :: AddressBook
johnAddy = insertEntry johnEntry emptyBook -- Add entry to an empty address book
-- finally call as findEntry "John" "Smith" johnAddy
-- returns johnAddy Entry

-- lookup Entry in Address book by listing street and an address book (johnAddy)
findEntrySt :: String -> AddressBook -> Maybe Entry
findEntrySt street = filter filterForSt >>> head
    where
        filterForSt :: Entry -> Boolean
        filterForSt entry = entry.address.street == street

entryExists :: String -> String -> AddressBook -> Boolean
entryExists firstName lastName = filter filterForName >>> not null
    where
        filterForName :: Entry -> Boolean
        filterForName entry = entry.firstName == firstName && entry.lastName == lastName

johnEntryDuplicate :: Entry
johnEntryDuplicate = {
    firstName: "John",
    lastName: "Smith",
    address: johnAddr
}

isEqual :: Entry -> Entry -> Boolean
isEqual s1 s2 = s1 == s2 

removeDuplicates :: String -> String -> AddressBook -> AddressBook
removeDuplicates firstName lastName = nubBy hasSameName
 where
    hasSameName :: Entry -> Entry -> Boolean
    hasSameName entry1 entry2 =
      entry1.firstName == firstName &&
      entry1.lastName == lastName &&
      entry1.firstName == entry2.firstName &&
      entry1.lastName == entry2.lastName

duplicateAddy :: AddressBook
duplicateAddy = insertEntry johnEntry johnAddy