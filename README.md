#EZCouch. A high level static library for working with CouchDB from Haskell

For people with Haskell background a schemaless database interfaced with dynamic JavaScript, which CouchDB is, may seem like a perfect opportunity to shoot yourself in the foot. This library approaches the issue by providing a completely static API over ADTs for it. 

EZCouch protects you on a typelevel from doing silly stuff such as forgetting to generate a view or mistyping its name. In fact, it generates and manages views for you. It also approaches other important issues, such as Random Fetching and Transactions. For more info please refer to the library's [API](http://hackage.haskell.org/package/ez-couch).

Not convinced yet? See the code.

##Demo

[source](src/RandomDemo.hs)

```haskell
-- | This module showcases how EZCouch can be used to elegantly and 
-- transparantly solve some quite untrivial CouchDB tasks. It shows how you can 
-- fetch, create and delete entities from the database, how to use EZCouch's 
-- powerful features, such as Random Fetching, and how EZCouch transparantly 
-- manages the views for you.

-- These are required flags.
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric #-}

-- Nothing fancy, we just block the standard Prelude here and import a way more 
-- convenient one. 
-- More details about it here: 
-- http://hackage.haskell.org/package/classy-prelude
import Prelude ()
import ClassyPrelude

-- Generics (or reflection). This allows us to easily generate instances of 
-- supporting classes without any implementation boilerplate.
-- This is a modern implementation of generics for Haskell. It is much more 
-- productive than SYB and may even go on par with TemplateHaskell-based 
-- solutions.
import GHC.Generics (Generic)

-- Everything that's needed to use EZCouch.
import EZCouch


-- Connection settings for connecting to a locally running CouchDB with an
-- existing database named "test" configured to provide free permissions to 
-- unauthenticated users.
connection = ConnectionSettings {
  connectionSettingsHost = "127.0.0.1",
  connectionSettingsPort = defaultPort,
  connectionSettingsAuth = Nothing,
  connectionSettingsDatabase = "test"
}

-- An entity of our model. This is the only data that we need to communicate
-- with db. No intermediate objects or quirky types.
-- Note: the entity must derive the `Generic` type imported from `GHC.Generics`.
data A = A {a :: Int, b :: Text, c :: Maybe Double}
  deriving (Show, Generic)
-- Generate instances of Aeson's typeclasses using generics.
instance ToJSON A
instance FromJSON A
-- Generate an instance of EZCouch's typeclass which is required to be able
-- to use the entity with db.
instance Doc A

-- Execute our program by running actions in the EZCouch's `MonadAction`.
-- First we regenerate all the entities in the db and then subsequently fetch
-- and print random ones from it.
main = run connection $ do
  regenerateAs
  printRandomAs
  printRandomAs
  printRandomAs


regenerateAs = do
  -- Fetch all existing entities of type `A`.
  as :: [Persisted A] <- readEntities ViewById KeysSelectionAll 0 Nothing False
  -- Delete them all.
  deleteEntities as
  -- Create new ones.
  createEntities $ map (\i -> A i "a" Nothing) [0..7]

printRandomAs = do
  -- Fetch at most 2 random entities.
  as :: [Persisted A] <- readRandomEntities $ Just 2
  -- Output info about them.
  liftIO $ putStrLn $ 
    "Got " 
      ++ (show $ length as) ++ " random results: " 
      ++ (show $ map persistedId as)
```

##Support
You're welcome to ask any questions under a tag "ez-couch" at [StackOverflow](http://stackoverflow.com/questions/tagged/ez-couch) and post any issues here, on the [issue tracker](https://github.com/nikita-volkov/ez-couch/issues).

##Contribution
As always, pull it!
