{- |

Template Haskell functions that allow all
questions and comments for an assessment to be
easily accessed.

It's assumed assessments will have their own add-on database
schemas, and will define an entity "Marks" itemizing
what marks can be awarded and what comments an assessor
might make.

-}
module Exigo.Persistent.TH
  (
    mkSaveAssessmentMetadata
  , mkQuestionFieldsAccessor
  , mkCommentFieldsAccessor
  )
  where

import Exigo.Persistent.TH.Internal

