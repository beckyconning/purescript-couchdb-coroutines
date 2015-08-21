module Data.CouchDB where

import Prelude

-- TODO: Make imports more specific
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Undefined
import Data.Generic
import Data.Maybe
import Test.QuickCheck.Arbitrary

newtype Change = Change { rev :: String }

newtype Result = Result { seq :: Int, id :: String, deleted :: Maybe Boolean, changes :: Array Change }

newtype Notification = Notification { last_seq :: Int, results :: Array Result }

exampleResult = Result  { seq: 1
                        , id: "5c98f21de6f21c6bfb8bfc55cb00023e"
                        , deleted: Just true
                        , changes: [ Change { rev: "2-967a00dff5e02add41819138abb3284d" } ] }

exampleNotification = Notification { last_seq: 1, results: [ exampleResult ] }

derive instance genericChange :: Generic Change
derive instance genericResult :: Generic Result
derive instance genericNotification :: Generic Notification

instance arbChange :: Arbitrary Change where
  arbitrary = Change <$> ({ rev: _ } <$> arbitrary)

instance arbResult :: Arbitrary Result where
  arbitrary = Result <$> ({ seq: _, id: _, deleted: _, changes: _ } <$> arbitrary
                                                                    <*> arbitrary
                                                                    <*> arbitrary
                                                                    <*> arbitrary)

instance arbNotification :: Arbitrary Notification where
  arbitrary = Notification <$> ({ last_seq: _, results: _ } <$> arbitrary
                                                            <*> arbitrary)

changesUrl :: String -> String -> Int -> String
changesUrl couchDBURL dBName sinceSeq = couchDBURL ++ "/" ++ dBName ++ query
  where
  query = "/_changes?feed=longpoll&since=" ++ gShow sinceSeq
