
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
{-# LANGUAGE StandaloneDeriving         #-}

{- |

Basic database schema for exigo tools.

The assumption is that the schema will actually be composed
of this, base, schema, plus subsidiary ones for
individual assessments.

-}

module Exigo.Persistent.Schema
  (
    -- | Represents a student
    Student
    -- | A student ID
  , StudentId
    -- | Submission made by a 'Student'.
  , Submission
    -- | 'Submission' ID
  , SubmissionId
    -- | A late penalty applied
  , LatePenalty
    -- | 'LatePenalty' ID
  , LatePenaltyId
    -- | Saved entities from this schema
  , savedMainModel
  )
  where

import          Data.Aeson
import          Data.Binary
import          Data.Text               (Text)
import          Database.Persist.TH     (
                                          mkPersist
                                        , mkSave
                                        , persistFileWith
                                        , share
                                        , sqlSettings
                                        )
import          Database.Persist.Quasi
import          Database.Persist.Class
import          GHC.Generics

share [
    mkSave "savedMainModel"
  , mkPersist sqlSettings
  ]
  $(persistFileWith lowerCaseSettings "config/exigo.persistentmodels")

instance Binary Student     where

deriving instance Generic (Key Student)

instance Binary   (Key Student) where
instance Binary   Submission    where
instance Binary   LatePenalty   where

instance FromJSON Student       where
instance FromJSON Submission    where
instance FromJSON LatePenalty   where

instance ToJSON   Student       where
instance ToJSON   Submission    where
instance ToJSON   LatePenalty   where

