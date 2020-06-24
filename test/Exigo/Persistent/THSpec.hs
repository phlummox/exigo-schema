
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Exigo.Persistent.THSpec (main, spec) where

import Control.Monad
import Control.Monad.Catch                    (MonadMask)
import Control.Monad.IO.Class                 (MonadIO, liftIO)

import Data.List                              (intercalate)
import Data.Monoid                            ((<>))
import Data.String.Interpolate                (i)
import Data.Text.Arbitrary                    ()
import Data.Typeable                          (Typeable)

import qualified Language.Haskell.Interpreter as Hint
import           Language.Haskell.Interpreter ( InterpreterError(..)
                                              , GhcError(..), OptionVal((:=))
                                              , languageExtensions)
--import qualified UnliftIO as U
--import qualified UnliftIO.Directory as U
import System.Directory                       (withCurrentDirectory)
import System.IO.Temp                         (withSystemTempDirectory)

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck             (modifyMaxSuccess)

import Exigo.Types as ET

-- used for debugging:
-- import System.Process (readProcess)

deriving instance Typeable Question
deriving instance Typeable AssessmentMetadata

instance Arbitrary Question where
  arbitrary = Question <$> arbitrary
                       <*> arbitrary
                       <*> arbitrary
  shrink = genericShrink

instance Arbitrary AssessmentMetadata where
  arbitrary = AssessmentMetadata <$> arbitrary
                                 <*> arbitrary
  shrink = genericShrink

errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e


data EvalConfig = EvalConfig {
    moduleName :: String
  , imports    :: [String]
  , extensions :: [Hint.Extension]
  , isVerbose    :: Bool
  }
  deriving (Eq, Show)

putStrLn' :: MonadIO m => String -> m ()
putStrLn' = liftIO . putStrLn



interpretWithConfig ::
  (MonadIO m, MonadMask m) =>
    EvalConfig -> Hint.InterpreterT m a -> m a
interpretWithConfig (EvalConfig modName imports extensions isVerb) f = fmap handle' $ Hint.runInterpreter $ do
  when isVerb $
    putStrLn' $ "loading modules " ++ show [modName]
  Hint.loadModules [modName]
  when isVerb $
    putStrLn' $ "setting extensions " ++ show extensions
  Hint.set [languageExtensions := extensions]
  when isVerb $
    putStrLn' $ "setting imports " ++ show imports
  Hint.setImports imports
  when isVerb $
    putStrLn'   "running action"
  f
  where
    handle' = either (error . errorString) id

eval :: forall m t . (MonadIO m, MonadMask m, Typeable t) => EvalConfig -> String -> m t
eval cfg exprStr = interpretWithConfig cfg $ do
    when (isVerbose cfg) $
      putStrLn' $ "interpreting expr " ++ show exprStr
    Hint.interpret exprStr (Hint.as :: t)

-- | run action with a temporary directory,
-- cd'ing into it.
withTempDir :: IO a -> IO a
withTempDir a =
  withSystemTempDirectory "exigo-test" $ \dir ->
    withCurrentDirectory dir a


roundTripSaveAssessmentMetadata ::
  AssessmentMetadata -> IO AssessmentMetadata
roundTripSaveAssessmentMetadata s =
  withTempDir $ do
    liftIO $ writeFile (modName <> ".hs") (moduleSrc modName s)
    eval cfg "myVal"
  where
    modName = "MyModule"

    cfg = EvalConfig
            modName
            ["Prelude", "Exigo.Persistent.TH", "Exigo.Types", modName]
            [Hint.TemplateHaskell, Hint.OverloadedStrings ]
            False

    moduleSrc :: String -> AssessmentMetadata -> String
    moduleSrc modName ass = [i|
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module #{modName}
  where

import Exigo.Persistent.TH
import Exigo.Types

$(mkSaveAssessmentMetadata "myVal" #{ass} )
    |]

-- |
-- @
-- inspectQuestionFieldsAccessor fields
-- @
--
-- Compile and run 'mkQuestionFieldsAccessor' on
-- a schema for a "Marks" type whose fields are
-- specified by the @fields@ parameter.
--
-- See in main for example tests. e.g.:
--
-- >>> inspectQuestionFieldsAccessor [("myField", "Int")]
-- ("[Marks -> Double]",0)
--
-- ... we have no "question"-type fields, so the
-- length of the accessor produced is 0.
-- (And the type is as we expect.)
inspectQuestionFieldsAccessor ::
  [(String, String)] -> IO (String, Int)
inspectQuestionFieldsAccessor fields =
   withTempDir $ do
      writeFile (modName <> ".hs") (moduleSrc modName)
      -- liftIO $ do
      --    putStrLn $ "conts of " <> (modName <> ".hs") <> ":"
      --    readProcess "cat" ["-n", modName <> ".hs"] "" >>=
      --            putStrLn
      t <- interpretWithConfig cfg (Hint.typeOf "someAccessor")
      l <- eval cfg "length someAccessor"
      return (t, l)
  where
    modName = "MyModule"

    mkField :: String -> String -> String
    mkField fName fType = "    " <> fName <> "  " <> fType

    fieldLines = unlines $ map (uncurry mkField) fields

    cfg = EvalConfig
            modName
            ["Prelude", "Exigo.Types", modName]
            []
            False

    endQQ   = "|]"

    moduleSrc :: String -> String
    moduleSrc modName = [i|
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}

module #{modName}
  where

import Exigo.Persistent.TH    (mkQuestionFieldsAccessor)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH    (share
                               , mkSave, persistFileWith
                               , mkPersist, sqlSettings
                               , persistLowerCase
                               , mkMigrate)

share [
    mkPersist sqlSettings
  , mkQuestionFieldsAccessor sqlSettings "someAccessor"
  ] [persistLowerCase|

Marks
#{fieldLines}

#{endQQ}

    |]



-- |
-- @
-- inspectQuestionFieldsAccessor fields
-- @
--
-- Compile and run 'mkQuestionFieldsAccessor' on
-- a schema for a "Marks" type whose fields are
-- specified by the @fields@ parameter.
--
-- See in main for example tests.

inspectCommentFieldsAccessor ::
  [(String, String)] -> IO (String, Int)
inspectCommentFieldsAccessor fields =
   withTempDir $ do
      liftIO $ writeFile (modName <> ".hs") (moduleSrc modName)
      -- liftIO $ do
      --    putStrLn $ "conts of " <> (modName <> ".hs") <> ":"
      --    readProcess "cat" ["-n", modName <> ".hs"] "" >>=
      --            putStrLn
      t <- interpretWithConfig cfg (Hint.typeOf "someAccessor")
      l <- eval cfg "length someAccessor"
      return (t, l)
  where
    modName = "MyModule"

    mkField :: String -> String -> String
    mkField fName fType = "    " <> fName <> "  " <> fType

    fieldLines = unlines $ map (uncurry mkField) fields

    cfg = EvalConfig
            modName
            ["Prelude", "Exigo.Types", modName]
            []
            False

    endQQ   = "|]"

    moduleSrc :: String -> String
    moduleSrc modName = [i|
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}

module #{modName}
  where

import Exigo.Persistent.TH    (mkCommentFieldsAccessor)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH    (share
                               , mkSave, persistFileWith
                               , mkPersist, sqlSettings
                               , persistLowerCase
                               , mkMigrate)
import qualified Data.Text as T
import           Data.Text ( Text )


share [
    mkPersist sqlSettings
  , mkCommentFieldsAccessor sqlSettings "someAccessor"
  ] [persistLowerCase|

Marks
#{fieldLines}

#{endQQ}

    |]



-- make tests more readoable
pattern (:=>) :: a -> b -> (a, b)
pattern  a :=> b = (a, b)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mkSaveAssessmentMetadata" $
    modifyMaxSuccess (const 10) $
      it "should roundtrip its argument" $
        property $ \a ->
              ioProperty $ do
                res <- roundTripSaveAssessmentMetadata a
                return $ res == a
  describe "mkQuestionFieldsAccessor" $ do
    let tests = [
            [("a1", "Int")]     :=> ("[Marks -> Double]",0)
          , [("q1", "Int")]     :=> ("[Marks -> Double]",0)
          , [("a1", "Double")]  :=> ("[Marks -> Double]",0)          , [
              ("q1", "Double")
            ]                   :=> ("[Marks -> Double]",1)
          , [
              ("q1", "Double")
            , ("q2", "Int")
            ]                   :=> ("[Marks -> Double]",1)
          , [
              ("q1", "Double")
            , ("q2", "Double")
            ]                   :=> ("[Marks -> Double]",2)
          ]
    forM_ tests $ \(fields, expectedRes) ->
      it ("applied to fields " <> show fields) $
            inspectQuestionFieldsAccessor fields `shouldReturn` expectedRes
  describe "mkCommentFieldsAccessor" $ do
    let tests = [
            [("a1",         "Int")]         :=> ("[Marks -> Maybe Data.Text.Internal.Text]",0)
          , [("q1Comments", "Int")]         :=> ("[Marks -> Maybe Data.Text.Internal.Text]",0)
          , [("a1",         "Text Maybe")]  :=> ("[Marks -> Maybe Data.Text.Internal.Text]",0)
          , [
              ("q1Comments","Text Maybe")
            ]                               :=> ("[Marks -> Maybe Data.Text.Internal.Text]",1)
          , [
              ("q1Comments","Text Maybe")
            , ("q2Comments","Int")
            ]                               :=> ("[Marks -> Maybe Data.Text.Internal.Text]",1)
          , [
              ("q1Comments","Text Maybe")
            , ("comments","Text Maybe")
            ]                               :=> ("[Marks -> Maybe Data.Text.Internal.Text]",2)
          ]
    forM_ tests $ \(fields, expectedRes) ->
      it ("applied to fields " <> show fields) $
            inspectCommentFieldsAccessor fields `shouldReturn` expectedRes
