
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveGeneric #-}

{- |

Types used in exigo programs.

-}

module Exigo.Types where

import           Data.Aeson
import           Data.Text ( Text )
import           GHC.Generics
import           Language.Haskell.TH.Syntax (Lift)
import           Instances.TH.Lift ()

-- | a question (smallest assessable unit)
-- in an assessment
data Question =  Question {
    qid     :: Text -- ^ textual id, e.g. 1(a)
  , qDesc   :: Text -- ^ possible descriptive text
  , qMarks  :: Int  -- ^ maximum marks
  }
  deriving (Eq, Show, Lift, Generic)
  -- Lift instance is so we can convert questions
  -- from "compile time" to actual values.

-- | metadata about an assessment.
data AssessmentMetadata = AssessmentMetadata {
    assessmentTitle :: Text
  , assessmentQuestions :: [Question]
  }
  deriving (Eq, Show, Lift, Generic)

instance FromJSON Question where
instance ToJSON   Question where
instance FromJSON AssessmentMetadata where
instance ToJSON   AssessmentMetadata where


