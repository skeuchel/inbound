

-- UUAGC 0.9.51 (src/Ag/Syntax.ag)
module Ag.Syntax( module InBound.Syntax.Core
  , module Ag.Syntax
  ) where

{-# LINE 10 "src/Ag/Syntax.ag" #-}

import InBound.Syntax.Core
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map
import qualified Data.Set
{-# LINE 16 "src/Ag/Syntax.hs" #-}
-- AttrDecl ----------------------------------------------------
data AttrDecl = SynDecl (AttrName) (Type)
              | InhDecl (AttrName) (Type)
              deriving ( Eq,Show)
-- AttrDecls ---------------------------------------------------
type AttrDecls = [AttrDecl]
-- AttrDef -----------------------------------------------------
data AttrDef = AttrDef (AttrRef) (Expr)
             deriving ( Eq,Show)
-- AttrDefs ----------------------------------------------------
type AttrDefs = [AttrDef]
-- AttrNameType ------------------------------------------------
type AttrNameType = ( (AttrName),Type)
-- AttrNameTypes -----------------------------------------------
type AttrNameTypes = [AttrNameType]
-- AttrRef -----------------------------------------------------
data AttrRef = AttrRef (NodeLabel) (AttrName)
             deriving ( Eq,Ord,Show)
-- CtorDecl ----------------------------------------------------
data CtorDecl = CtorDecl (CtorName) (CtorFieldDecls) (LocAttrDecls) (AttrDefs)
              deriving ( Eq,Show)
-- CtorDecls ---------------------------------------------------
type CtorDecls = [CtorDecl]
-- CtorFieldDecl -----------------------------------------------
data CtorFieldDecl = CFSubtree (FieldName) (SortName)
                   | CFTerminal (FieldName) (String)
                   deriving ( Eq,Show)
-- CtorFieldDecls ----------------------------------------------
type CtorFieldDecls = [CtorFieldDecl]
-- EnvAttrType -------------------------------------------------
type EnvAttrType = Data.Map.Map (((SortName,AttrName))) (Type)
-- Expr --------------------------------------------------------
data Expr = ExprAttrRef (AttrRef)
          | ExprField (FieldName)
          | ExprFresh (Expr)
          | ExprNil
          | ExprCons (Expr) (Expr)
          | ExprSetEmpty
          | ExprSetSingleton (Expr)
          | ExprSetInsert (Expr) (Expr)
          | ExprSetUnion (Expr) (Expr)
          | ExprSetUnions (Exprs)
          | ExprSetDifference (Expr) (Expr)
          | ExprMapEmpty
          | ExprMapSingleton (Expr) (Expr)
          | ExprMapInsert (Expr) (Expr) (Expr)
          | ExprMapLookup (Expr) (Expr) (Expr)
          | ExprCtor (CtorName) (Exprs)
          deriving ( Eq,Show)
-- Exprs -------------------------------------------------------
type Exprs = [Expr]
-- LocAttrDecl -------------------------------------------------
data LocAttrDecl = LocAttrDecl (AttrName) (Type)
                 deriving ( Eq,Show)
-- LocAttrDecls ------------------------------------------------
type LocAttrDecls = [LocAttrDecl]
-- LocEnvAttrType ----------------------------------------------
type LocEnvAttrType = Data.Map.Map ((AttrName)) (Type)
-- MbSortName --------------------------------------------------
type MbSortName = Maybe ((SortName))
-- NamespaceDecl -----------------------------------------------
data NamespaceDecl = NamespaceDecl (NamespaceName) (MbSortName)
                   deriving ( Eq,Show)
-- NamespaceDecls ----------------------------------------------
type NamespaceDecls = [NamespaceDecl]
-- NodeLabel ---------------------------------------------------
data NodeLabel = Lhs
               | Loc
               | Sub (FieldName)
               deriving ( Eq,Ord,Show)
-- SemFun ------------------------------------------------------
data SemFun = SemFun (String) (AttrNameTypes) (AttrNameTypes)
            deriving ( Eq,Show)
-- SemFuns -----------------------------------------------------
type SemFuns = [SemFun]
-- SortDecl ----------------------------------------------------
data SortDecl = SortDecl (SortName) (AttrDecls) (CtorDecls) (SemFuns)
              deriving ( Eq,Show)
-- SortDecls ---------------------------------------------------
type SortDecls = [SortDecl]
-- Specification -----------------------------------------------
data Specification = Specification (String) (Synonyms) (SortDecls)
                   deriving ( Eq,Show)
-- Synonym -----------------------------------------------------
data Synonym = Synonym (String) (Type)
             deriving ( Eq,Show)
-- Synonyms ----------------------------------------------------
type Synonyms = [Synonym]
-- Type --------------------------------------------------------
data Type = TString
          | TSet (Type)
          | TList (Type)
          | TMap (Type) (Type)
          | TTerminal (String)
          | TSort (SortName)
          deriving ( Eq,Show)