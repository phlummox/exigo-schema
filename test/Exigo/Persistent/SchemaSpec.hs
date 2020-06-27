
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Exigo.Persistent.SchemaSpec (main, spec) where


import Test.Hspec
import Test.QuickCheck

import           Data.Aeson as Ae
import qualified Data.Text as T
import           Data.Text.Arbitrary ()

import Exigo.Persistent.Schema

default (T.Text)

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
  describe "exported functions" $
    it "shouldCompile" $
      shouldCompile `shouldBe` ()



