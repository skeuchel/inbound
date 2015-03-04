

-- UUAGC 0.9.51 (src/InBound/Syntax.ag)
module InBound.Syntax( module InBound.Syntax.Core
  , module InBound.Syntax
  ) where

{-# LINE 10 "src/InBound/Syntax.ag" #-}

import InBound.Syntax.Core
{-# LINE 12 "src/InBound/Syntax.hs" #-}
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
-- AttrRef -----------------------------------------------------
data AttrRef = AttrRef (NodeLabel) (AttrName)
             deriving ( Eq,Ord,Show)
-- CtorDecl ----------------------------------------------------
data CtorDecl = CtorDecl (CtorName) (CtorFieldDecls) (LocAttrDecls) (AttrDefs)
              deriving ( Eq,Show)
-- CtorDecls ---------------------------------------------------
type CtorDecls = [CtorDecl]
-- CtorFieldDecl -----------------------------------------------
data CtorFieldDecl = CFRef (FieldName) (AttrName)
                   | CFAtom (FieldName) (NamespaceName)
                   | CFSubtree (FieldName) (SortName)
                   | CFTerminal (FieldName) (String)
                   deriving ( Eq,Show)
-- CtorFieldDecls ----------------------------------------------
type CtorFieldDecls = [CtorFieldDecl]
-- Expr --------------------------------------------------------
data Expr = ExprAttrRef (AttrRef)
          | ExprNil
          | ExprCons (Expr) (FieldName)
          deriving ( Eq,Show)
-- LocAttrDecl -------------------------------------------------
data LocAttrDecl = LocAttrDecl (AttrName) (Type)
                 deriving ( Eq,Show)
-- LocAttrDecls ------------------------------------------------
type LocAttrDecls = [LocAttrDecl]
-- MbSortName --------------------------------------------------
type MbSortName = Maybe ((SortName))
-- NamespaceDecl -----------------------------------------------
data NamespaceDecl = NamespaceDecl (NamespaceName) (MbSortName)
                   deriving ( Eq,Show)
-- NamespaceDecls ----------------------------------------------
type NamespaceDecls = [NamespaceDecl]
-- NamespaceNames ----------------------------------------------
type NamespaceNames = [(NamespaceName)]
-- NodeLabel ---------------------------------------------------
data NodeLabel = Lhs
               | Loc
               | Sub (FieldName)
               deriving ( Eq,Ord,Show)
-- SortDecl ----------------------------------------------------
data SortDecl = SortDecl (SortName) (AttrDecls) (CtorDecls)
              deriving ( Eq,Show)
-- SortDecls ---------------------------------------------------
type SortDecls = [SortDecl]
-- Specification -----------------------------------------------
data Specification = Specification (String) (NamespaceDecls) (SortDecls)
                   deriving ( Eq,Show)
-- Type --------------------------------------------------------
data Type = Context (NamespaceName)
          deriving ( Eq,Show)