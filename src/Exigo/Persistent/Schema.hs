
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

{-# OPTIONS_GHC -Wno-duplicate-exports #-}

-- other extensions, needed to compile
-- for recent persistent-template versions

{-# LANGUAGE CPP                        #-}

#if MIN_VERSION_persistent_template(2,7,2)
{-# LANGUAGE UndecidableInstances #-}
#endif

#if MIN_VERSION_persistent_template(2,8,0)
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

{- |

Basic database schema for exigo tools.

The assumption is that the schema will actually be composed
of this, base, schema, plus subsidiary ones for
individual assessments.

-}

module Exigo.Persistent.Schema
  (
    -- * Database entities
    -- | Represents a student
    Student(..)
  , StudentId
    -- | Submission made by a 'Student'
  , Submission(..)
  , SubmissionId
    -- | A late penalty applied
  , LatePenalty(..)
  , LatePenaltyId
    -- * Runtime access to schema
    -- | Saved entities from this schema
  , savedMainModel
    -- * Keys for entities
  , Key(StudentKey,SubmissionKey
        ,LatePenaltyKey
        ,unStudentKey,unLatePenaltyKey
        ,unSubmissionKey)
  , StudentKey
  , SubmissionKey
  , LatePenaltyKey
    -- * Instances
  , module Exigo.Persistent.Schema
  )
  where

import          Data.Aeson ()
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

type StudentKey = Key Student
type LatePenaltyKey = Key LatePenalty
type SubmissionKey = Key Submission

