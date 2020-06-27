
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Exigo.Persistent.SchemaSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import           Control.Arrow
import           Control.Monad.Reader         (ReaderT)
import           Control.Monad.Logger         (NoLoggingT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Aeson as Ae
import qualified Data.Text as T
import           Data.Text.Arbitrary ()
import           Database.Esqueleto           ( select, from, where_
                                              , (^.), (==.), Entity(..) )
import qualified Database.Persist.Sqlite as S
import           Database.Persist.Sqlite      ( Migration
                                              , PersistEntity
                                              , PersistEntityBackend
                                              , SqlBackend )
import           Database.Persist.TH          ( mkMigrate )

import Exigo.Persistent.Schema

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Use ." #-}

default (T.Text)

mkMigrate "migrateExigo" savedMainModel

-- should compile ...
shouldCompile :: ()
shouldCompile = ()
  where
    unStudentKey' :: Key Student -> T.Text
    unStudentKey' = unStudentKey

    unSubmissionKey' :: Key Submission -> StudentId
    unSubmissionKey' = unSubmissionKey

    unLatePenaltyKey' :: Key LatePenalty -> StudentId
    unLatePenaltyKey' = unLatePenaltyKey

    migrateExigo' :: Migration
    migrateExigo' = migrateExigo

class (PersistEntity t, PersistEntityBackend t ~ SqlBackend) => IsExigoEntity t where

instance IsExigoEntity Student where
instance IsExigoEntity Submission where
instance IsExigoEntity LatePenalty where

type DBMonad a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a

insert' :: (IsExigoEntity record) => record -> DBMonad ()
insert' = S.insert_

selectList' ::
  (IsExigoEntity record) =>
      [S.Filter record]
      -> [S.SelectOpt record]
      -> DBMonad [record]
selectList' = fmap (fmap (map S.entityVal)) . S.selectList

withDB :: DBMonad a -> IO a
withDB f = S.runSqlite ":memory:" $
  S.runMigration migrateExigo >> f

dbTest ::
  DBMonad () -> DBMonad a -> (a -> Expectation) -> IO ()
dbTest insertion query assertion = do
  results <- withDB $ do
    insertion
    query
  assertion results


studentsRoundtripOK :: IO ()
studentsRoundtripOK =
      dbTest
        (mapM_ insert' students)
        (selectList' [] [])
        (`shouldBe` students)
  where
    students = [
         Student "0101" "alice"
        ,Student "0102" "bob"
        ]

submissionsRoundtripOK :: IO ()
submissionsRoundtripOK =
      dbTest
        (mapM_ insert' students >>
         mapM_ insert' submissions)
        (selectList' [] [])
        (`shouldBe` submissions)
  where
    students = [
         Student "0101" "alice"
        ,Student "0102" "bob"
        ]
    submissions = [
         Submission (StudentKey "0101") "alice01" "/work/sub1.pdf"
      ,  Submission (StudentKey "0102") "bob01"   "/work/sub2.pdf"
      ]

tableJoin :: DBMonad [(Student, Submission)]
tableJoin =
    map (entityVal *** entityVal) <$> query
  where
    query :: DBMonad [(Entity Student, Entity Submission)]
    query =
      select $
      from $ \(stud,sub) -> do
      where_
        (stud ^. StudentId ==. sub ^.  SubmissionStudent)
      return (stud,sub)

joinRoundTripsOK :: IO ()
joinRoundTripsOK =
      dbTest
        (mapM_ insert' students >>
             mapM_ insert' submissions)
        tableJoin
        (`shouldBe` zip students submissions)
  where
    students = [
         Student "0101" "alice"
        ,Student "0102" "bob"
        ]
    submissions = [
         Submission (StudentKey "0101") "alice01" "/work/sub1.pdf"
      ,  Submission (StudentKey "0102") "bob01"   "/work/sub2.pdf"
      ]

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

toValue :: ToJSON a => a -> Value
toValue = Ae.toJSON

shouldEncodeAs :: ToJSON a => a -> Value -> Expectation
shouldEncodeAs x o =
  toValue x `shouldBe` o


studentEncodesOK :: T.Text -> T.Text -> Expectation
studentEncodesOK sNum name =
    v `shouldEncodeAs` o
  where
    v = Student sNum name
    o = object [ "studNo" .= sNum
               , "name"   .= name
               ]

latePenaltyEncodesOK :: T.Text -> Int -> Expectation
latePenaltyEncodesOK sNum lateness =
    v `shouldEncodeAs`  o
  where
    v = LatePenalty (StudentKey sNum) lateness
    o = object [ "student"  .= sNum
               , "daysLate" .= lateness
               ]


submissionEncodesOK :: T.Text -> T.Text -> FilePath -> Expectation
submissionEncodesOK sNum logon subPath =
    v `shouldEncodeAs` o
  where
    v = Submission (StudentKey sNum) logon subPath
    o = object [ "student"      .= sNum
               , "studentLogin" .= logon
               , "path"         .= subPath
               ]

spec :: Spec
spec = do
  context "Entities should encode sensibly to JSON" $ do
    specify "Student" $
      property studentEncodesOK
    specify "LatePenalty" $
      property latePenaltyEncodesOK
    specify "Submission" $
      property submissionEncodesOK
  context "Should be able to round-trip entities thru Sqlite database" $ do
    specify "Student"
      studentsRoundtripOK
    specify "Submission"
      submissionsRoundtripOK
    specify "Student & Submission tables joined"
      joinRoundTripsOK
  describe "exported functions" $
    it "shouldCompile" $
      shouldCompile `shouldBe` ()



