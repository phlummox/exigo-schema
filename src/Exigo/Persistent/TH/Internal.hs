
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

{- |

Unstable, may change without warning.

Utility funcs to support "Exigo.Persistent.TH".

-}

module Exigo.Persistent.TH.Internal
  where

import           Database.Persist           ( FieldDef(..)
                                            , EntityDef(..)
                                            , FieldType(FTTypeCon)
                                            , HaskellName(..) )
import           Data.Char
import           Data.Monoid                ( (<>) )
import qualified Data.Text as T
import           Data.Text                  ( Text, cons, uncons )
import           Database.Persist.TH        ( MkPersistSettings(..) )
import           Language.Haskell.TH.Syntax

import           Exigo.Types ( AssessmentMetadata(..) )

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- | create a function which returns at runtime the
-- assessment metadata passed in at compile-time.
--
-- i.e., if @mkSaveAssessmentMetadata myFunc mData@ is called,
-- it creates a function like
--
-- @
-- myFunc :: AssessmentMetadata
-- myFunc = mData
-- @
mkSaveAssessmentMetadata :: String -> AssessmentMetadata -> Q [Dec]
mkSaveAssessmentMetadata name' mData' = do
  let name = mkName name'
  mData <- lift mData'
  return [ SigD name $ ConT ''AssessmentMetadata
         , FunD name [normalClause [] mData]
         ]

-- | create a function which returns all the question-type field
--  accessors for Marks.
--
-- i.e., @mkQuestionFieldsAccessor sqlSettings myName@
-- in a call to @share@
-- should produce a result something like
--
-- @
-- myNHame :: [Marks -> Double]
-- myName = [ marksQ1a, marksQ1b, marksQ1c .. ]
-- @
--
-- where the accessors are in the order they appear
-- in the EntityDef.
mkQuestionFieldsAccessor :: MkPersistSettings -> String -> [EntityDef] -> Q [Dec]
mkQuestionFieldsAccessor mpSettings nm es =
  let es' = filter isMarks es
  in concat <$> mapM (mkAcc nm) es'
  where

    mkAcc :: String -> EntityDef -> Q [Dec]
    mkAcc name' e = do
      let name = mkName name'
          -- the various field accessors: q1a, q1b, etc
          qFields = map (VarE . mkName . accessorName mpSettings . fName) $
                      filter isQField $ entityFields e
          funcType = x_to_listXT (marks_to_xT (ConT ''Double))
          funcSig = SigD name funcType
          func = FunD name [normalClause [] (ListE qFields)]
      return [ funcSig, func ]


    -- does this look like a "q1a" etc field?
    -- viz., it starts with a 'q', and doesn't contain
    -- the string "Comments" in its name, and
    -- is of type Double
    isQField :: FieldDef -> Bool
    isQField f = let n = unHaskellName $ fieldHaskell f
                 in "q" `T.isPrefixOf` n
                    && not ("comments" `T.isInfixOf` n)
                    && fieldType f == FTTypeCon Nothing "Double"


-- | create a function which returns all the comment-type field
--  accessors for Marks.
--
-- i.e., @mkCommentFieldsAccessor myName@
--
-- should produce a result something like
--
-- @
-- myNHame :: [Marks -> Maybe Text]
-- myName = [ marksQ1aComments, marksQ1bComments, marksQ1cComments .. ]
-- @
--
-- where the accessors are in the order they appear
-- in the EntityDef.
mkCommentFieldsAccessor ::
  MkPersistSettings -> String -> [EntityDef] -> Q [Dec]
mkCommentFieldsAccessor mpSettings nm es =
  let es' = filter isMarks es
  in concat <$> mapM (mkAcc nm) es'
  where

    mkAcc :: String -> EntityDef -> Q [Dec]
    mkAcc name' e = do
      let name = mkName name'
          -- the various field accessors: marksQ1aComments, etc
          qFields = map (VarE . mkName . accessorName mpSettings . fName) $
                      filter isCommentsField $ entityFields e
          funcType = x_to_listXT (marks_to_xT ( x_to_maybeXT $ ConT ''Text ))
          funcSig = SigD name funcType
          func = FunD name [normalClause [] (ListE qFields)]
      return [ funcSig, func ]

    -- does this look like a "q1a" etc field?
    -- viz., it starts with a 'q', and doesn't contain
    -- the string "Comments" in its name, and
    -- is of type Double
    isCommentsField :: FieldDef -> Bool
    isCommentsField f = let n = unHaskellName $ fieldHaskell f
                 in  "comments" `T.isInfixOf` T.map toLower n
                    && fieldType f == FTTypeCon Nothing "Text"
                    && "Maybe" `elem` fieldAttrs f


-- | given some field from an entty def -- e.g. "q1a" --
-- get the actual accessor name (i.e. "marksQ1a")
accessorName :: MkPersistSettings -> Text -> String
accessorName mpSettings f =
  T.unpack $ recName mpSettings (HaskellName "Marks") (HaskellName f)

-- | is this the "Marks" entity?
isMarks :: EntityDef -> Bool
isMarks e = let eNm = unHaskellName $ entityHaskell e
            in eNm == "Marks"

fName :: FieldDef -> Text
fName = unHaskellName . fieldHaskell

-- the "Marks" type
marksT :: Type
marksT = ConT $ mkName "Marks"

-- given type X, construct type [X]
x_to_listXT :: Type -> Type
x_to_listXT = AppT ListT

-- given type X, construct type [X]
x_to_maybeXT :: Type -> Type
x_to_maybeXT = AppT (ConT ''Maybe)

-- the type "Marks -> x"
marks_to_xT :: Type -> Type
marks_to_xT = AppT (AppT ArrowT marksT)

normalClause :: [Pat] -> Exp -> Clause
normalClause p e = Clause p (NormalB e) []

-- make first letter lowercase
lowerFirst :: Text -> Text
lowerFirst t =
    case uncons t of
        Just (a, b) -> cons (toLower a) b
        Nothing -> t

-- make first letter uppercase
upperFirst :: Text -> Text
upperFirst t =
    case uncons t of
        Just (a, b) -> cons (toUpper a) b
        Nothing -> t

-- make a ... record name for a field, with no underscore?
recNameNoUnderscore :: MkPersistSettings -> HaskellName -> HaskellName -> Text
recNameNoUnderscore mps dt f
  | mpsPrefixFields mps = lowerFirst (unHaskellName dt) <>  upperFirst ft
  | otherwise           = lowerFirst ft
  where
    ft = unHaskellName f

-- | name for a record field
--
-- If we call @recName datatypeName fieldName@
-- this returns a 'qualified field name' (i.e. data type name
-- is prepended, and the whole thing is camelCased.
--
-- i.e.
-- @
-- recName "MyType" "myfield" == "myTypeMyField"
-- @
-- (modulo name constructors).
recName :: MkPersistSettings -> HaskellName -> HaskellName -> Text
recName mps dt f =
    addUnderscore $ recNameNoUnderscore mps dt f
  where
    addUnderscore
        | mpsGenerateLenses mps = ("_" <>)
        | otherwise = id

