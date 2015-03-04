

-- UUAGC 0.9.51 (src/InBound/CopyRules.ag)
module InBound.CopyRules where
import InBound.Syntax
{-# LINE 4 "src/InBound/Environment.ag" #-}

import Control.Monad
import Data.Maybe
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import InBound.Syntax
{-# LINE 17 "src/InBound/CopyRules.hs" #-}

{-# LINE 10 "src/InBound/Syntax.ag" #-}

import InBound.Syntax.Core
{-# LINE 22 "src/InBound/CopyRules.hs" #-}
{-# LINE 192 "src/InBound/Environment.ag" #-}

localNodeLabel :: NodeLabel -> String
localNodeLabel Lhs      = "lhs_"
localNodeLabel Loc      = "loc_"
localNodeLabel (Sub fn) = fromFN fn ++ "_"

localAttrRef :: AttrRef -> Int -> AttrRef
localAttrRef (AttrRef nl (AN an)) depth =
  AttrRef Loc
    (AN $ localNodeLabel nl ++ an ++ "_" ++ show depth)
{-# LINE 34 "src/InBound/CopyRules.hs" #-}

{-# LINE 6 "src/InBound/CopyRules.ag" #-}

defaultValues :: Inh_Specification
defaultValues = (Inh_Specification {})

completion :: Specification -> Specification
completion spec =
  let sem = wrap_Specification (sem_Specification spec) defaultValues
  in completion_Syn_Specification sem
{-# LINE 45 "src/InBound/CopyRules.hs" #-}
-- AttrDecl ----------------------------------------------------
-- cata
sem_AttrDecl :: AttrDecl ->
                T_AttrDecl
sem_AttrDecl (SynDecl _attrName _attrType) =
    (sem_AttrDecl_SynDecl _attrName (sem_Type _attrType))
sem_AttrDecl (InhDecl _attrName _attrType) =
    (sem_AttrDecl_InhDecl _attrName (sem_Type _attrType))
-- semantic domain
type T_AttrDecl = SortName ->
                  NamespaceNames ->
                  (Map NamespaceName MbSortName) ->
                  (Map SortName [NamespaceName]) ->
                  ( AttrDecl,(Map AttrName Type),(Map AttrName Type),AttrDecl)
data Inh_AttrDecl = Inh_AttrDecl {envSort_Inh_AttrDecl :: SortName,envVars_Inh_AttrDecl :: NamespaceNames,namespaces_Inh_AttrDecl :: (Map NamespaceName MbSortName),varsorts_Inh_AttrDecl :: (Map SortName [NamespaceName])}
data Syn_AttrDecl = Syn_AttrDecl {completion_Syn_AttrDecl :: AttrDecl,sAttrInh_Syn_AttrDecl :: (Map AttrName Type),sAttrSyn_Syn_AttrDecl :: (Map AttrName Type),self_Syn_AttrDecl :: AttrDecl}
wrap_AttrDecl :: T_AttrDecl ->
                 Inh_AttrDecl ->
                 Syn_AttrDecl
wrap_AttrDecl sem (Inh_AttrDecl _lhsIenvSort _lhsIenvVars _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself) = sem _lhsIenvSort _lhsIenvVars _lhsInamespaces _lhsIvarsorts
     in  (Syn_AttrDecl _lhsOcompletion _lhsOsAttrInh _lhsOsAttrSyn _lhsOself))
sem_AttrDecl_SynDecl :: AttrName ->
                        T_Type ->
                        T_AttrDecl
sem_AttrDecl_SynDecl attrName_ attrType_ =
    (\ _lhsIenvSort
       _lhsIenvVars
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOsAttrSyn :: (Map AttrName Type)
              _lhsOsAttrInh :: (Map AttrName Type)
              _lhsOcompletion :: AttrDecl
              _lhsOself :: AttrDecl
              _attrTypeIcompletion :: Type
              _attrTypeIself :: Type
              _lhsOsAttrSyn =
                  ({-# LINE 99 "src/InBound/Environment.ag" #-}
                   M.singleton attrName_ _attrTypeIself
                   {-# LINE 85 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 77 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 90 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   SynDecl attrName_ _attrTypeIcompletion
                   {-# LINE 95 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  SynDecl attrName_ _attrTypeIself
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 102 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              ( _attrTypeIcompletion,_attrTypeIself) =
                  attrType_
          in  ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself)))
sem_AttrDecl_InhDecl :: AttrName ->
                        T_Type ->
                        T_AttrDecl
sem_AttrDecl_InhDecl attrName_ attrType_ =
    (\ _lhsIenvSort
       _lhsIenvVars
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOsAttrInh :: (Map AttrName Type)
              _lhsOsAttrSyn :: (Map AttrName Type)
              _lhsOcompletion :: AttrDecl
              _lhsOself :: AttrDecl
              _attrTypeIcompletion :: Type
              _attrTypeIself :: Type
              _lhsOsAttrInh =
                  ({-# LINE 100 "src/InBound/Environment.ag" #-}
                   M.singleton attrName_ _attrTypeIself
                   {-# LINE 126 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 76 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 131 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   InhDecl attrName_ _attrTypeIcompletion
                   {-# LINE 136 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  InhDecl attrName_ _attrTypeIself
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 143 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              ( _attrTypeIcompletion,_attrTypeIself) =
                  attrType_
          in  ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself)))
-- AttrDecls ---------------------------------------------------
-- cata
sem_AttrDecls :: AttrDecls ->
                 T_AttrDecls
sem_AttrDecls list =
    (Prelude.foldr sem_AttrDecls_Cons sem_AttrDecls_Nil (Prelude.map sem_AttrDecl list))
-- semantic domain
type T_AttrDecls = SortName ->
                   NamespaceNames ->
                   (Map NamespaceName MbSortName) ->
                   (Map SortName [NamespaceName]) ->
                   ( AttrDecls,(Map AttrName Type),(Map AttrName Type),AttrDecls)
data Inh_AttrDecls = Inh_AttrDecls {envSort_Inh_AttrDecls :: SortName,envVars_Inh_AttrDecls :: NamespaceNames,namespaces_Inh_AttrDecls :: (Map NamespaceName MbSortName),varsorts_Inh_AttrDecls :: (Map SortName [NamespaceName])}
data Syn_AttrDecls = Syn_AttrDecls {completion_Syn_AttrDecls :: AttrDecls,sAttrInh_Syn_AttrDecls :: (Map AttrName Type),sAttrSyn_Syn_AttrDecls :: (Map AttrName Type),self_Syn_AttrDecls :: AttrDecls}
wrap_AttrDecls :: T_AttrDecls ->
                  Inh_AttrDecls ->
                  Syn_AttrDecls
wrap_AttrDecls sem (Inh_AttrDecls _lhsIenvSort _lhsIenvVars _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself) = sem _lhsIenvSort _lhsIenvVars _lhsInamespaces _lhsIvarsorts
     in  (Syn_AttrDecls _lhsOcompletion _lhsOsAttrInh _lhsOsAttrSyn _lhsOself))
sem_AttrDecls_Cons :: T_AttrDecl ->
                      T_AttrDecls ->
                      T_AttrDecls
sem_AttrDecls_Cons hd_ tl_ =
    (\ _lhsIenvSort
       _lhsIenvVars
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOsAttrInh :: (Map AttrName Type)
              _lhsOsAttrSyn :: (Map AttrName Type)
              _lhsOcompletion :: AttrDecls
              _lhsOself :: AttrDecls
              _hdOenvSort :: SortName
              _hdOenvVars :: NamespaceNames
              _hdOnamespaces :: (Map NamespaceName MbSortName)
              _hdOvarsorts :: (Map SortName [NamespaceName])
              _tlOenvSort :: SortName
              _tlOenvVars :: NamespaceNames
              _tlOnamespaces :: (Map NamespaceName MbSortName)
              _tlOvarsorts :: (Map SortName [NamespaceName])
              _hdIcompletion :: AttrDecl
              _hdIsAttrInh :: (Map AttrName Type)
              _hdIsAttrSyn :: (Map AttrName Type)
              _hdIself :: AttrDecl
              _tlIcompletion :: AttrDecls
              _tlIsAttrInh :: (Map AttrName Type)
              _tlIsAttrSyn :: (Map AttrName Type)
              _tlIself :: AttrDecls
              _lhsOsAttrInh =
                  ({-# LINE 77 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsAttrInh _tlIsAttrInh)
                   {-# LINE 201 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 76 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsAttrSyn _tlIsAttrSyn)
                   {-# LINE 206 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   (:) _hdIcompletion _tlIcompletion
                   {-# LINE 211 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 218 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              _hdOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 225 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 230 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 235 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 240 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 245 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 250 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 255 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 260 "src/InBound/CopyRules.hs" #-}
                   )
              ( _hdIcompletion,_hdIsAttrInh,_hdIsAttrSyn,_hdIself) =
                  hd_ _hdOenvSort _hdOenvVars _hdOnamespaces _hdOvarsorts
              ( _tlIcompletion,_tlIsAttrInh,_tlIsAttrSyn,_tlIself) =
                  tl_ _tlOenvSort _tlOenvVars _tlOnamespaces _tlOvarsorts
          in  ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself)))
sem_AttrDecls_Nil :: T_AttrDecls
sem_AttrDecls_Nil =
    (\ _lhsIenvSort
       _lhsIenvVars
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOsAttrInh :: (Map AttrName Type)
              _lhsOsAttrSyn :: (Map AttrName Type)
              _lhsOcompletion :: AttrDecls
              _lhsOself :: AttrDecls
              _lhsOsAttrInh =
                  ({-# LINE 77 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 280 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 76 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 285 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   []
                   {-# LINE 290 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  []
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 297 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
          in  ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself)))
-- AttrDef -----------------------------------------------------
-- cata
sem_AttrDef :: AttrDef ->
               T_AttrDef
sem_AttrDef (AttrDef _attrDefRef _attrDefExpr) =
    (sem_AttrDef_AttrDef (sem_AttrRef _attrDefRef) (sem_Expr _attrDefExpr))
-- semantic domain
type T_AttrDef = (Map FieldName NamespaceName) ->
                 (Map (SortName,AttrName) Type) ->
                 (Map (SortName,AttrName) Type) ->
                 (Map FieldName SortName) ->
                 (Map AttrRef Type) ->
                 (Map AttrRef Type) ->
                 SortName ->
                 NamespaceNames ->
                 (Map AttrName Type) ->
                 (Map AttrName Type) ->
                 (Map NamespaceName MbSortName) ->
                 (Map SortName [NamespaceName]) ->
                 ( AttrDef,(Map AttrRef Expr),AttrDef)
data Inh_AttrDef = Inh_AttrDef {envAtom_Inh_AttrDef :: (Map FieldName NamespaceName),envAttrInh_Inh_AttrDef :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_AttrDef :: (Map (SortName,AttrName) Type),envField_Inh_AttrDef :: (Map FieldName SortName),envSetAttrDef_Inh_AttrDef :: (Map AttrRef Type),envSetAttrUse_Inh_AttrDef :: (Map AttrRef Type),envSort_Inh_AttrDef :: SortName,envVars_Inh_AttrDef :: NamespaceNames,locEnvAttrInh_Inh_AttrDef :: (Map AttrName Type),locEnvAttrSyn_Inh_AttrDef :: (Map AttrName Type),namespaces_Inh_AttrDef :: (Map NamespaceName MbSortName),varsorts_Inh_AttrDef :: (Map SortName [NamespaceName])}
data Syn_AttrDef = Syn_AttrDef {completion_Syn_AttrDef :: AttrDef,sSetAttrDef_Syn_AttrDef :: (Map AttrRef Expr),self_Syn_AttrDef :: AttrDef}
wrap_AttrDef :: T_AttrDef ->
                Inh_AttrDef ->
                Syn_AttrDef
wrap_AttrDef sem (Inh_AttrDef _lhsIenvAtom _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvField _lhsIenvSetAttrDef _lhsIenvSetAttrUse _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOcompletion,_lhsOsSetAttrDef,_lhsOself) = sem _lhsIenvAtom _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvField _lhsIenvSetAttrDef _lhsIenvSetAttrUse _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIvarsorts
     in  (Syn_AttrDef _lhsOcompletion _lhsOsSetAttrDef _lhsOself))
sem_AttrDef_AttrDef :: T_AttrRef ->
                       T_Expr ->
                       T_AttrDef
sem_AttrDef_AttrDef attrDefRef_ attrDefExpr_ =
    (\ _lhsIenvAtom
       _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvField
       _lhsIenvSetAttrDef
       _lhsIenvSetAttrUse
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOsSetAttrDef :: (Map AttrRef Expr)
              _attrDefExprOattrDefRef :: AttrRef
              _attrDefExprOdepth :: Int
              _lhsOcompletion :: AttrDef
              _lhsOself :: AttrDef
              _attrDefExprOenvAtom :: (Map FieldName NamespaceName)
              _attrDefExprOenvAttrInh :: (Map (SortName,AttrName) Type)
              _attrDefExprOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _attrDefExprOenvField :: (Map FieldName SortName)
              _attrDefExprOenvSort :: SortName
              _attrDefExprOenvVars :: NamespaceNames
              _attrDefRefIcompletion :: AttrRef
              _attrDefRefIself :: AttrRef
              _attrDefExprIattrName :: AttrRef
              _attrDefExprIcompletion :: Expr
              _attrDefExprIself :: Expr
              _lhsOsSetAttrDef =
                  ({-# LINE 148 "src/InBound/Environment.ag" #-}
                   M.singleton
                     _attrDefRefIself
                     _attrDefExprIself
                   {-# LINE 367 "src/InBound/CopyRules.hs" #-}
                   )
              _attrDefExprOattrDefRef =
                  ({-# LINE 206 "src/InBound/Environment.ag" #-}
                   _attrDefRefIself
                   {-# LINE 372 "src/InBound/CopyRules.hs" #-}
                   )
              _attrDefExprOdepth =
                  ({-# LINE 207 "src/InBound/Environment.ag" #-}
                   0
                   {-# LINE 377 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsnamespace =
                  ({-# LINE 208 "src/InBound/Environment.ag" #-}
                   case M.lookup _attrDefRefIself _lhsIenvSetAttrDef of
                     Just (Context ns) -> ns
                     Nothing           -> error "AttrDef namespace"
                   {-# LINE 384 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsmbsort =
                  ({-# LINE 212 "src/InBound/Environment.ag" #-}
                   join $ M.lookup _lhsnamespace     _lhsInamespaces
                   {-# LINE 389 "src/InBound/CopyRules.hs" #-}
                   )
              _lhssubstitutable =
                  ({-# LINE 213 "src/InBound/Environment.ag" #-}
                   isJust _lhsmbsort
                   {-# LINE 394 "src/InBound/CopyRules.hs" #-}
                   )
              _lhssort =
                  ({-# LINE 214 "src/InBound/Environment.ag" #-}
                   case _lhsmbsort     of
                     Just s  -> s
                     Nothing -> error "AttrDef lhssort"
                   {-# LINE 401 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   AttrDef _attrDefRefIcompletion _attrDefExprIcompletion
                   {-# LINE 406 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  AttrDef _attrDefRefIself _attrDefExprIself
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 413 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              _attrDefExprOenvAtom =
                  ({-# LINE 62 "src/InBound/Environment.ag" #-}
                   _lhsIenvAtom
                   {-# LINE 420 "src/InBound/CopyRules.hs" #-}
                   )
              _attrDefExprOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 425 "src/InBound/CopyRules.hs" #-}
                   )
              _attrDefExprOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 430 "src/InBound/CopyRules.hs" #-}
                   )
              _attrDefExprOenvField =
                  ({-# LINE 61 "src/InBound/Environment.ag" #-}
                   _lhsIenvField
                   {-# LINE 435 "src/InBound/CopyRules.hs" #-}
                   )
              _attrDefExprOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 440 "src/InBound/CopyRules.hs" #-}
                   )
              _attrDefExprOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 445 "src/InBound/CopyRules.hs" #-}
                   )
              ( _attrDefRefIcompletion,_attrDefRefIself) =
                  attrDefRef_
              ( _attrDefExprIattrName,_attrDefExprIcompletion,_attrDefExprIself) =
                  attrDefExpr_ _attrDefExprOattrDefRef _attrDefExprOdepth _attrDefExprOenvAtom _attrDefExprOenvAttrInh _attrDefExprOenvAttrSyn _attrDefExprOenvField _attrDefExprOenvSort _attrDefExprOenvVars
          in  ( _lhsOcompletion,_lhsOsSetAttrDef,_lhsOself)))
-- AttrDefs ----------------------------------------------------
-- cata
sem_AttrDefs :: AttrDefs ->
                T_AttrDefs
sem_AttrDefs list =
    (Prelude.foldr sem_AttrDefs_Cons sem_AttrDefs_Nil (Prelude.map sem_AttrDef list))
-- semantic domain
type T_AttrDefs = (Map FieldName NamespaceName) ->
                  (Map (SortName,AttrName) Type) ->
                  (Map (SortName,AttrName) Type) ->
                  (Map FieldName SortName) ->
                  (Map AttrRef Type) ->
                  (Map AttrRef Type) ->
                  SortName ->
                  NamespaceNames ->
                  (Map AttrName Type) ->
                  (Map AttrName Type) ->
                  (Map NamespaceName MbSortName) ->
                  (Map SortName [NamespaceName]) ->
                  ( AttrDefs,(Map AttrRef Expr),AttrDefs)
data Inh_AttrDefs = Inh_AttrDefs {envAtom_Inh_AttrDefs :: (Map FieldName NamespaceName),envAttrInh_Inh_AttrDefs :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_AttrDefs :: (Map (SortName,AttrName) Type),envField_Inh_AttrDefs :: (Map FieldName SortName),envSetAttrDef_Inh_AttrDefs :: (Map AttrRef Type),envSetAttrUse_Inh_AttrDefs :: (Map AttrRef Type),envSort_Inh_AttrDefs :: SortName,envVars_Inh_AttrDefs :: NamespaceNames,locEnvAttrInh_Inh_AttrDefs :: (Map AttrName Type),locEnvAttrSyn_Inh_AttrDefs :: (Map AttrName Type),namespaces_Inh_AttrDefs :: (Map NamespaceName MbSortName),varsorts_Inh_AttrDefs :: (Map SortName [NamespaceName])}
data Syn_AttrDefs = Syn_AttrDefs {completion_Syn_AttrDefs :: AttrDefs,sSetAttrDef_Syn_AttrDefs :: (Map AttrRef Expr),self_Syn_AttrDefs :: AttrDefs}
wrap_AttrDefs :: T_AttrDefs ->
                 Inh_AttrDefs ->
                 Syn_AttrDefs
wrap_AttrDefs sem (Inh_AttrDefs _lhsIenvAtom _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvField _lhsIenvSetAttrDef _lhsIenvSetAttrUse _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOcompletion,_lhsOsSetAttrDef,_lhsOself) = sem _lhsIenvAtom _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvField _lhsIenvSetAttrDef _lhsIenvSetAttrUse _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIvarsorts
     in  (Syn_AttrDefs _lhsOcompletion _lhsOsSetAttrDef _lhsOself))
sem_AttrDefs_Cons :: T_AttrDef ->
                     T_AttrDefs ->
                     T_AttrDefs
sem_AttrDefs_Cons hd_ tl_ =
    (\ _lhsIenvAtom
       _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvField
       _lhsIenvSetAttrDef
       _lhsIenvSetAttrUse
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOsSetAttrDef :: (Map AttrRef Expr)
              _lhsOcompletion :: AttrDefs
              _lhsOself :: AttrDefs
              _hdOenvAtom :: (Map FieldName NamespaceName)
              _hdOenvAttrInh :: (Map (SortName,AttrName) Type)
              _hdOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _hdOenvField :: (Map FieldName SortName)
              _hdOenvSetAttrDef :: (Map AttrRef Type)
              _hdOenvSetAttrUse :: (Map AttrRef Type)
              _hdOenvSort :: SortName
              _hdOenvVars :: NamespaceNames
              _hdOlocEnvAttrInh :: (Map AttrName Type)
              _hdOlocEnvAttrSyn :: (Map AttrName Type)
              _hdOnamespaces :: (Map NamespaceName MbSortName)
              _hdOvarsorts :: (Map SortName [NamespaceName])
              _tlOenvAtom :: (Map FieldName NamespaceName)
              _tlOenvAttrInh :: (Map (SortName,AttrName) Type)
              _tlOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _tlOenvField :: (Map FieldName SortName)
              _tlOenvSetAttrDef :: (Map AttrRef Type)
              _tlOenvSetAttrUse :: (Map AttrRef Type)
              _tlOenvSort :: SortName
              _tlOenvVars :: NamespaceNames
              _tlOlocEnvAttrInh :: (Map AttrName Type)
              _tlOlocEnvAttrSyn :: (Map AttrName Type)
              _tlOnamespaces :: (Map NamespaceName MbSortName)
              _tlOvarsorts :: (Map SortName [NamespaceName])
              _hdIcompletion :: AttrDef
              _hdIsSetAttrDef :: (Map AttrRef Expr)
              _hdIself :: AttrDef
              _tlIcompletion :: AttrDefs
              _tlIsSetAttrDef :: (Map AttrRef Expr)
              _tlIself :: AttrDefs
              _lhsOsSetAttrDef =
                  ({-# LINE 144 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsSetAttrDef _tlIsSetAttrDef)
                   {-# LINE 532 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   (:) _hdIcompletion _tlIcompletion
                   {-# LINE 537 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 544 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              _hdOenvAtom =
                  ({-# LINE 62 "src/InBound/Environment.ag" #-}
                   _lhsIenvAtom
                   {-# LINE 551 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 556 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 561 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvField =
                  ({-# LINE 61 "src/InBound/Environment.ag" #-}
                   _lhsIenvField
                   {-# LINE 566 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvSetAttrDef =
                  ({-# LINE 139 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrDef
                   {-# LINE 571 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvSetAttrUse =
                  ({-# LINE 141 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrUse
                   {-# LINE 576 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 581 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 586 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 591 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 596 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 601 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 606 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvAtom =
                  ({-# LINE 62 "src/InBound/Environment.ag" #-}
                   _lhsIenvAtom
                   {-# LINE 611 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 616 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 621 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvField =
                  ({-# LINE 61 "src/InBound/Environment.ag" #-}
                   _lhsIenvField
                   {-# LINE 626 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvSetAttrDef =
                  ({-# LINE 139 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrDef
                   {-# LINE 631 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvSetAttrUse =
                  ({-# LINE 141 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrUse
                   {-# LINE 636 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 641 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 646 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 651 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 656 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 661 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 666 "src/InBound/CopyRules.hs" #-}
                   )
              ( _hdIcompletion,_hdIsSetAttrDef,_hdIself) =
                  hd_ _hdOenvAtom _hdOenvAttrInh _hdOenvAttrSyn _hdOenvField _hdOenvSetAttrDef _hdOenvSetAttrUse _hdOenvSort _hdOenvVars _hdOlocEnvAttrInh _hdOlocEnvAttrSyn _hdOnamespaces _hdOvarsorts
              ( _tlIcompletion,_tlIsSetAttrDef,_tlIself) =
                  tl_ _tlOenvAtom _tlOenvAttrInh _tlOenvAttrSyn _tlOenvField _tlOenvSetAttrDef _tlOenvSetAttrUse _tlOenvSort _tlOenvVars _tlOlocEnvAttrInh _tlOlocEnvAttrSyn _tlOnamespaces _tlOvarsorts
          in  ( _lhsOcompletion,_lhsOsSetAttrDef,_lhsOself)))
sem_AttrDefs_Nil :: T_AttrDefs
sem_AttrDefs_Nil =
    (\ _lhsIenvAtom
       _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvField
       _lhsIenvSetAttrDef
       _lhsIenvSetAttrUse
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOsSetAttrDef :: (Map AttrRef Expr)
              _lhsOcompletion :: AttrDefs
              _lhsOself :: AttrDefs
              _lhsOsSetAttrDef =
                  ({-# LINE 144 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 693 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   []
                   {-# LINE 698 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  []
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 705 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
          in  ( _lhsOcompletion,_lhsOsSetAttrDef,_lhsOself)))
-- AttrRef -----------------------------------------------------
-- cata
sem_AttrRef :: AttrRef ->
               T_AttrRef
sem_AttrRef (AttrRef _nodeLabel _attrLabel) =
    (sem_AttrRef_AttrRef (sem_NodeLabel _nodeLabel) _attrLabel)
-- semantic domain
type T_AttrRef = ( AttrRef,AttrRef)
data Inh_AttrRef = Inh_AttrRef {}
data Syn_AttrRef = Syn_AttrRef {completion_Syn_AttrRef :: AttrRef,self_Syn_AttrRef :: AttrRef}
wrap_AttrRef :: T_AttrRef ->
                Inh_AttrRef ->
                Syn_AttrRef
wrap_AttrRef sem (Inh_AttrRef) =
    (let ( _lhsOcompletion,_lhsOself) = sem
     in  (Syn_AttrRef _lhsOcompletion _lhsOself))
sem_AttrRef_AttrRef :: T_NodeLabel ->
                       AttrName ->
                       T_AttrRef
sem_AttrRef_AttrRef nodeLabel_ attrLabel_ =
    (let _lhsOcompletion :: AttrRef
         _lhsOself :: AttrRef
         _nodeLabelIcompletion :: NodeLabel
         _nodeLabelIself :: NodeLabel
         _completion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              AttrRef _nodeLabelIcompletion attrLabel_
              {-# LINE 737 "src/InBound/CopyRules.hs" #-}
              )
         _self =
             AttrRef _nodeLabelIself attrLabel_
         _lhsOcompletion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              _completion
              {-# LINE 744 "src/InBound/CopyRules.hs" #-}
              )
         _lhsOself =
             _self
         ( _nodeLabelIcompletion,_nodeLabelIself) =
             nodeLabel_
     in  ( _lhsOcompletion,_lhsOself))
-- CtorDecl ----------------------------------------------------
-- cata
sem_CtorDecl :: CtorDecl ->
                T_CtorDecl
sem_CtorDecl (CtorDecl _ctorName _ctorFields _ctorLocAttrDecl _ctorAttrDefs) =
    (sem_CtorDecl_CtorDecl _ctorName (sem_CtorFieldDecls _ctorFields) (sem_LocAttrDecls _ctorLocAttrDecl) (sem_AttrDefs _ctorAttrDefs))
-- semantic domain
type T_CtorDecl = (Map (SortName,AttrName) Type) ->
                  (Map (SortName,AttrName) Type) ->
                  (Map AttrRef Type) ->
                  (Map AttrRef Type) ->
                  SortName ->
                  NamespaceNames ->
                  (Map AttrName Type) ->
                  (Map AttrName Type) ->
                  (Map NamespaceName MbSortName) ->
                  (Map SortName [NamespaceName]) ->
                  ( CtorDecl,(Map (SortName,AttrName) Type),(Map (SortName,AttrName) Type),(Map AttrRef Expr),CtorDecl)
data Inh_CtorDecl = Inh_CtorDecl {envAttrInh_Inh_CtorDecl :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_CtorDecl :: (Map (SortName,AttrName) Type),envSetAttrDef_Inh_CtorDecl :: (Map AttrRef Type),envSetAttrUse_Inh_CtorDecl :: (Map AttrRef Type),envSort_Inh_CtorDecl :: SortName,envVars_Inh_CtorDecl :: NamespaceNames,locEnvAttrInh_Inh_CtorDecl :: (Map AttrName Type),locEnvAttrSyn_Inh_CtorDecl :: (Map AttrName Type),namespaces_Inh_CtorDecl :: (Map NamespaceName MbSortName),varsorts_Inh_CtorDecl :: (Map SortName [NamespaceName])}
data Syn_CtorDecl = Syn_CtorDecl {completion_Syn_CtorDecl :: CtorDecl,sAttrInh_Syn_CtorDecl :: (Map (SortName,AttrName) Type),sAttrSyn_Syn_CtorDecl :: (Map (SortName,AttrName) Type),sSetAttrDef_Syn_CtorDecl :: (Map AttrRef Expr),self_Syn_CtorDecl :: CtorDecl}
wrap_CtorDecl :: T_CtorDecl ->
                 Inh_CtorDecl ->
                 Syn_CtorDecl
wrap_CtorDecl sem (Inh_CtorDecl _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSetAttrDef _lhsIenvSetAttrUse _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOsSetAttrDef,_lhsOself) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSetAttrDef _lhsIenvSetAttrUse _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIvarsorts
     in  (Syn_CtorDecl _lhsOcompletion _lhsOsAttrInh _lhsOsAttrSyn _lhsOsSetAttrDef _lhsOself))
sem_CtorDecl_CtorDecl :: CtorName ->
                         T_CtorFieldDecls ->
                         T_LocAttrDecls ->
                         T_AttrDefs ->
                         T_CtorDecl
sem_CtorDecl_CtorDecl ctorName_ ctorFields_ ctorLocAttrDecl_ ctorAttrDefs_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSetAttrDef
       _lhsIenvSetAttrUse
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _ctorFieldsOenvCopyRule :: (Map AttrName AttrRef)
              _lhsOcompletion :: CtorDecl
              _lhsOsAttrInh :: (Map (SortName,AttrName) Type)
              _lhsOsAttrSyn :: (Map (SortName,AttrName) Type)
              _lhsOsSetAttrDef :: (Map AttrRef Expr)
              _lhsOself :: CtorDecl
              _ctorFieldsOenvAttrInh :: (Map (SortName,AttrName) Type)
              _ctorFieldsOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _ctorFieldsOenvSort :: SortName
              _ctorFieldsOenvVars :: NamespaceNames
              _ctorFieldsOlocEnvAttrInh :: (Map AttrName Type)
              _ctorFieldsOlocEnvAttrSyn :: (Map AttrName Type)
              _ctorFieldsOmissingInhDef :: (Map FieldName [AttrName])
              _ctorFieldsOnamespaces :: (Map NamespaceName MbSortName)
              _ctorFieldsOvarsorts :: (Map SortName [NamespaceName])
              _ctorLocAttrDeclOnamespaces :: (Map NamespaceName MbSortName)
              _ctorLocAttrDeclOvarsorts :: (Map SortName [NamespaceName])
              _ctorAttrDefsOenvAtom :: (Map FieldName NamespaceName)
              _ctorAttrDefsOenvAttrInh :: (Map (SortName,AttrName) Type)
              _ctorAttrDefsOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _ctorAttrDefsOenvField :: (Map FieldName SortName)
              _ctorAttrDefsOenvSetAttrDef :: (Map AttrRef Type)
              _ctorAttrDefsOenvSetAttrUse :: (Map AttrRef Type)
              _ctorAttrDefsOenvSort :: SortName
              _ctorAttrDefsOenvVars :: NamespaceNames
              _ctorAttrDefsOlocEnvAttrInh :: (Map AttrName Type)
              _ctorAttrDefsOlocEnvAttrSyn :: (Map AttrName Type)
              _ctorAttrDefsOnamespaces :: (Map NamespaceName MbSortName)
              _ctorAttrDefsOvarsorts :: (Map SortName [NamespaceName])
              _ctorFieldsIcompletion :: CtorFieldDecls
              _ctorFieldsIcopyRuleDefs :: AttrDefs
              _ctorFieldsIenvCopyRule :: (Map AttrName AttrRef)
              _ctorFieldsIsAtomEnv :: (Map FieldName NamespaceName)
              _ctorFieldsIsFieldEnv :: (Map FieldName SortName)
              _ctorFieldsIself :: CtorFieldDecls
              _ctorLocAttrDeclIcompletion :: LocAttrDecls
              _ctorLocAttrDeclIself :: LocAttrDecls
              _ctorAttrDefsIcompletion :: AttrDefs
              _ctorAttrDefsIsSetAttrDef :: (Map AttrRef Expr)
              _ctorAttrDefsIself :: AttrDefs
              _envField =
                  ({-# LINE 66 "src/InBound/Environment.ag" #-}
                   _ctorFieldsIsFieldEnv
                   {-# LINE 836 "src/InBound/CopyRules.hs" #-}
                   )
              _envAtom =
                  ({-# LINE 67 "src/InBound/Environment.ag" #-}
                   _ctorFieldsIsAtomEnv
                   {-# LINE 841 "src/InBound/CopyRules.hs" #-}
                   )
              _envSetAttrDef =
                  ({-# LINE 165 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrDef `M.union`
                   M.fromList
                     [ (AttrRef (Sub f) a, t)
                     | (f,s1)     <- M.toList _envField
                     , ((s2,a),t) <- M.toList _lhsIenvAttrInh
                     , s1 == s2
                     ]
                   {-# LINE 852 "src/InBound/CopyRules.hs" #-}
                   )
              _envSetAttrUse =
                  ({-# LINE 173 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrUse `M.union`
                   M.fromList
                     [ (AttrRef (Sub f) a, t)
                     | (f,s1)     <- M.toList _envField
                     , ((s2,a),t) <- M.toList _lhsIenvAttrSyn
                     , s1 == s2
                     ]
                   {-# LINE 863 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorFieldsOenvCopyRule =
                  ({-# LINE 40 "src/InBound/CopyRules.ag" #-}
                   M.mapWithKey
                     (\an ty -> AttrRef Lhs an)
                     _lhsIlocEnvAttrInh
                   {-# LINE 870 "src/InBound/CopyRules.hs" #-}
                   )
              _missingDefSet =
                  ({-# LINE 45 "src/InBound/CopyRules.ag" #-}
                   S.difference
                     (M.keysSet _envSetAttrDef    )
                     (M.keysSet _ctorAttrDefsIsSetAttrDef)
                   {-# LINE 877 "src/InBound/CopyRules.hs" #-}
                   )
              _missingInhDef =
                  ({-# LINE 49 "src/InBound/CopyRules.ag" #-}
                   M.fromListWith (++)
                     [ (fn,[an])
                     | AttrRef (Sub fn) an <- S.toList _missingDefSet
                     ]
                   {-# LINE 885 "src/InBound/CopyRules.hs" #-}
                   )
              _missingSynDef =
                  ({-# LINE 54 "src/InBound/CopyRules.ag" #-}
                   [ an | AttrRef Lhs an <- S.toList _missingDefSet     ]
                   {-# LINE 890 "src/InBound/CopyRules.hs" #-}
                   )
              _copyRuleDefs =
                  ({-# LINE 56 "src/InBound/CopyRules.ag" #-}
                   [ AttrDef (AttrRef Lhs an) (ExprAttrRef rhs)
                   | an <- _missingSynDef    ,
                     let Just rhs = M.lookup an _ctorFieldsIenvCopyRule
                   ]
                   {-# LINE 898 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOcompletion =
                  ({-# LINE 63 "src/InBound/CopyRules.ag" #-}
                   CtorDecl
                     ctorName_
                     _ctorFieldsIself
                     _ctorLocAttrDeclIself
                     (_copyRuleDefs     ++ _ctorFieldsIcopyRuleDefs ++ _ctorAttrDefsIself)
                   {-# LINE 907 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 83 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 912 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 82 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 917 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsSetAttrDef =
                  ({-# LINE 144 "src/InBound/Environment.ag" #-}
                   _ctorAttrDefsIsSetAttrDef
                   {-# LINE 922 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   CtorDecl ctorName_ _ctorFieldsIcompletion _ctorLocAttrDeclIcompletion _ctorAttrDefsIcompletion
                   {-# LINE 927 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  CtorDecl ctorName_ _ctorFieldsIself _ctorLocAttrDeclIself _ctorAttrDefsIself
              _lhsOself =
                  _self
              _ctorFieldsOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 936 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorFieldsOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 941 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorFieldsOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 946 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorFieldsOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 951 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorFieldsOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 956 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorFieldsOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 961 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorFieldsOmissingInhDef =
                  ({-# LINE 34 "src/InBound/CopyRules.ag" #-}
                   _missingInhDef
                   {-# LINE 966 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorFieldsOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 971 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorFieldsOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 976 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorLocAttrDeclOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 981 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorLocAttrDeclOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 986 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorAttrDefsOenvAtom =
                  ({-# LINE 62 "src/InBound/Environment.ag" #-}
                   _envAtom
                   {-# LINE 991 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorAttrDefsOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 996 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorAttrDefsOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1001 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorAttrDefsOenvField =
                  ({-# LINE 61 "src/InBound/Environment.ag" #-}
                   _envField
                   {-# LINE 1006 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorAttrDefsOenvSetAttrDef =
                  ({-# LINE 139 "src/InBound/Environment.ag" #-}
                   _envSetAttrDef
                   {-# LINE 1011 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorAttrDefsOenvSetAttrUse =
                  ({-# LINE 141 "src/InBound/Environment.ag" #-}
                   _envSetAttrUse
                   {-# LINE 1016 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorAttrDefsOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1021 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorAttrDefsOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 1026 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorAttrDefsOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 1031 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorAttrDefsOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 1036 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorAttrDefsOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 1041 "src/InBound/CopyRules.hs" #-}
                   )
              _ctorAttrDefsOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 1046 "src/InBound/CopyRules.hs" #-}
                   )
              ( _ctorFieldsIcompletion,_ctorFieldsIcopyRuleDefs,_ctorFieldsIenvCopyRule,_ctorFieldsIsAtomEnv,_ctorFieldsIsFieldEnv,_ctorFieldsIself) =
                  ctorFields_ _ctorFieldsOenvAttrInh _ctorFieldsOenvAttrSyn _ctorFieldsOenvCopyRule _ctorFieldsOenvSort _ctorFieldsOenvVars _ctorFieldsOlocEnvAttrInh _ctorFieldsOlocEnvAttrSyn _ctorFieldsOmissingInhDef _ctorFieldsOnamespaces _ctorFieldsOvarsorts
              ( _ctorLocAttrDeclIcompletion,_ctorLocAttrDeclIself) =
                  ctorLocAttrDecl_ _ctorLocAttrDeclOnamespaces _ctorLocAttrDeclOvarsorts
              ( _ctorAttrDefsIcompletion,_ctorAttrDefsIsSetAttrDef,_ctorAttrDefsIself) =
                  ctorAttrDefs_ _ctorAttrDefsOenvAtom _ctorAttrDefsOenvAttrInh _ctorAttrDefsOenvAttrSyn _ctorAttrDefsOenvField _ctorAttrDefsOenvSetAttrDef _ctorAttrDefsOenvSetAttrUse _ctorAttrDefsOenvSort _ctorAttrDefsOenvVars _ctorAttrDefsOlocEnvAttrInh _ctorAttrDefsOlocEnvAttrSyn _ctorAttrDefsOnamespaces _ctorAttrDefsOvarsorts
          in  ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOsSetAttrDef,_lhsOself)))
-- CtorDecls ---------------------------------------------------
-- cata
sem_CtorDecls :: CtorDecls ->
                 T_CtorDecls
sem_CtorDecls list =
    (Prelude.foldr sem_CtorDecls_Cons sem_CtorDecls_Nil (Prelude.map sem_CtorDecl list))
-- semantic domain
type T_CtorDecls = (Map (SortName,AttrName) Type) ->
                   (Map (SortName,AttrName) Type) ->
                   (Map AttrRef Type) ->
                   (Map AttrRef Type) ->
                   SortName ->
                   NamespaceNames ->
                   (Map AttrName Type) ->
                   (Map AttrName Type) ->
                   (Map NamespaceName MbSortName) ->
                   (Map SortName [NamespaceName]) ->
                   ( CtorDecls,(Map (SortName,AttrName) Type),(Map (SortName,AttrName) Type),(Map AttrRef Expr),CtorDecls)
data Inh_CtorDecls = Inh_CtorDecls {envAttrInh_Inh_CtorDecls :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_CtorDecls :: (Map (SortName,AttrName) Type),envSetAttrDef_Inh_CtorDecls :: (Map AttrRef Type),envSetAttrUse_Inh_CtorDecls :: (Map AttrRef Type),envSort_Inh_CtorDecls :: SortName,envVars_Inh_CtorDecls :: NamespaceNames,locEnvAttrInh_Inh_CtorDecls :: (Map AttrName Type),locEnvAttrSyn_Inh_CtorDecls :: (Map AttrName Type),namespaces_Inh_CtorDecls :: (Map NamespaceName MbSortName),varsorts_Inh_CtorDecls :: (Map SortName [NamespaceName])}
data Syn_CtorDecls = Syn_CtorDecls {completion_Syn_CtorDecls :: CtorDecls,sAttrInh_Syn_CtorDecls :: (Map (SortName,AttrName) Type),sAttrSyn_Syn_CtorDecls :: (Map (SortName,AttrName) Type),sSetAttrDef_Syn_CtorDecls :: (Map AttrRef Expr),self_Syn_CtorDecls :: CtorDecls}
wrap_CtorDecls :: T_CtorDecls ->
                  Inh_CtorDecls ->
                  Syn_CtorDecls
wrap_CtorDecls sem (Inh_CtorDecls _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSetAttrDef _lhsIenvSetAttrUse _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOsSetAttrDef,_lhsOself) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSetAttrDef _lhsIenvSetAttrUse _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIvarsorts
     in  (Syn_CtorDecls _lhsOcompletion _lhsOsAttrInh _lhsOsAttrSyn _lhsOsSetAttrDef _lhsOself))
sem_CtorDecls_Cons :: T_CtorDecl ->
                      T_CtorDecls ->
                      T_CtorDecls
sem_CtorDecls_Cons hd_ tl_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSetAttrDef
       _lhsIenvSetAttrUse
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOsAttrInh :: (Map (SortName,AttrName) Type)
              _lhsOsAttrSyn :: (Map (SortName,AttrName) Type)
              _lhsOsSetAttrDef :: (Map AttrRef Expr)
              _lhsOcompletion :: CtorDecls
              _lhsOself :: CtorDecls
              _hdOenvAttrInh :: (Map (SortName,AttrName) Type)
              _hdOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _hdOenvSetAttrDef :: (Map AttrRef Type)
              _hdOenvSetAttrUse :: (Map AttrRef Type)
              _hdOenvSort :: SortName
              _hdOenvVars :: NamespaceNames
              _hdOlocEnvAttrInh :: (Map AttrName Type)
              _hdOlocEnvAttrSyn :: (Map AttrName Type)
              _hdOnamespaces :: (Map NamespaceName MbSortName)
              _hdOvarsorts :: (Map SortName [NamespaceName])
              _tlOenvAttrInh :: (Map (SortName,AttrName) Type)
              _tlOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _tlOenvSetAttrDef :: (Map AttrRef Type)
              _tlOenvSetAttrUse :: (Map AttrRef Type)
              _tlOenvSort :: SortName
              _tlOenvVars :: NamespaceNames
              _tlOlocEnvAttrInh :: (Map AttrName Type)
              _tlOlocEnvAttrSyn :: (Map AttrName Type)
              _tlOnamespaces :: (Map NamespaceName MbSortName)
              _tlOvarsorts :: (Map SortName [NamespaceName])
              _hdIcompletion :: CtorDecl
              _hdIsAttrInh :: (Map (SortName,AttrName) Type)
              _hdIsAttrSyn :: (Map (SortName,AttrName) Type)
              _hdIsSetAttrDef :: (Map AttrRef Expr)
              _hdIself :: CtorDecl
              _tlIcompletion :: CtorDecls
              _tlIsAttrInh :: (Map (SortName,AttrName) Type)
              _tlIsAttrSyn :: (Map (SortName,AttrName) Type)
              _tlIsSetAttrDef :: (Map AttrRef Expr)
              _tlIself :: CtorDecls
              _lhsOsAttrInh =
                  ({-# LINE 83 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsAttrInh _tlIsAttrInh)
                   {-# LINE 1133 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 82 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsAttrSyn _tlIsAttrSyn)
                   {-# LINE 1138 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsSetAttrDef =
                  ({-# LINE 144 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsSetAttrDef _tlIsSetAttrDef)
                   {-# LINE 1143 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   (:) _hdIcompletion _tlIcompletion
                   {-# LINE 1148 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 1155 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              _hdOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1162 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1167 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvSetAttrDef =
                  ({-# LINE 139 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrDef
                   {-# LINE 1172 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvSetAttrUse =
                  ({-# LINE 141 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrUse
                   {-# LINE 1177 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1182 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 1187 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 1192 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 1197 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 1202 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 1207 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1212 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1217 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvSetAttrDef =
                  ({-# LINE 139 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrDef
                   {-# LINE 1222 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvSetAttrUse =
                  ({-# LINE 141 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrUse
                   {-# LINE 1227 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1232 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 1237 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 1242 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 1247 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 1252 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 1257 "src/InBound/CopyRules.hs" #-}
                   )
              ( _hdIcompletion,_hdIsAttrInh,_hdIsAttrSyn,_hdIsSetAttrDef,_hdIself) =
                  hd_ _hdOenvAttrInh _hdOenvAttrSyn _hdOenvSetAttrDef _hdOenvSetAttrUse _hdOenvSort _hdOenvVars _hdOlocEnvAttrInh _hdOlocEnvAttrSyn _hdOnamespaces _hdOvarsorts
              ( _tlIcompletion,_tlIsAttrInh,_tlIsAttrSyn,_tlIsSetAttrDef,_tlIself) =
                  tl_ _tlOenvAttrInh _tlOenvAttrSyn _tlOenvSetAttrDef _tlOenvSetAttrUse _tlOenvSort _tlOenvVars _tlOlocEnvAttrInh _tlOlocEnvAttrSyn _tlOnamespaces _tlOvarsorts
          in  ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOsSetAttrDef,_lhsOself)))
sem_CtorDecls_Nil :: T_CtorDecls
sem_CtorDecls_Nil =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSetAttrDef
       _lhsIenvSetAttrUse
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOsAttrInh :: (Map (SortName,AttrName) Type)
              _lhsOsAttrSyn :: (Map (SortName,AttrName) Type)
              _lhsOsSetAttrDef :: (Map AttrRef Expr)
              _lhsOcompletion :: CtorDecls
              _lhsOself :: CtorDecls
              _lhsOsAttrInh =
                  ({-# LINE 83 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1284 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 82 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1289 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsSetAttrDef =
                  ({-# LINE 144 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1294 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   []
                   {-# LINE 1299 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  []
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 1306 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
          in  ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOsSetAttrDef,_lhsOself)))
-- CtorFieldDecl -----------------------------------------------
-- cata
sem_CtorFieldDecl :: CtorFieldDecl ->
                     T_CtorFieldDecl
sem_CtorFieldDecl (CFRef _ctorFieldName _ctorFieldType) =
    (sem_CtorFieldDecl_CFRef _ctorFieldName _ctorFieldType)
sem_CtorFieldDecl (CFAtom _ctorFieldName _ctorFieldType) =
    (sem_CtorFieldDecl_CFAtom _ctorFieldName _ctorFieldType)
sem_CtorFieldDecl (CFSubtree _ctorFieldName _ctorFieldType) =
    (sem_CtorFieldDecl_CFSubtree _ctorFieldName _ctorFieldType)
sem_CtorFieldDecl (CFTerminal _ctorFieldName _ctorFieldType) =
    (sem_CtorFieldDecl_CFTerminal _ctorFieldName _ctorFieldType)
-- semantic domain
type T_CtorFieldDecl = (Map (SortName,AttrName) Type) ->
                       (Map (SortName,AttrName) Type) ->
                       (Map AttrName AttrRef) ->
                       SortName ->
                       NamespaceNames ->
                       (Map AttrName Type) ->
                       (Map AttrName Type) ->
                       (Map FieldName [AttrName]) ->
                       (Map NamespaceName MbSortName) ->
                       (Map SortName [NamespaceName]) ->
                       ( CtorFieldDecl,AttrDefs,(Map AttrName AttrRef),(Map FieldName NamespaceName),(Map FieldName SortName),CtorFieldDecl)
data Inh_CtorFieldDecl = Inh_CtorFieldDecl {envAttrInh_Inh_CtorFieldDecl :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_CtorFieldDecl :: (Map (SortName,AttrName) Type),envCopyRule_Inh_CtorFieldDecl :: (Map AttrName AttrRef),envSort_Inh_CtorFieldDecl :: SortName,envVars_Inh_CtorFieldDecl :: NamespaceNames,locEnvAttrInh_Inh_CtorFieldDecl :: (Map AttrName Type),locEnvAttrSyn_Inh_CtorFieldDecl :: (Map AttrName Type),missingInhDef_Inh_CtorFieldDecl :: (Map FieldName [AttrName]),namespaces_Inh_CtorFieldDecl :: (Map NamespaceName MbSortName),varsorts_Inh_CtorFieldDecl :: (Map SortName [NamespaceName])}
data Syn_CtorFieldDecl = Syn_CtorFieldDecl {completion_Syn_CtorFieldDecl :: CtorFieldDecl,copyRuleDefs_Syn_CtorFieldDecl :: AttrDefs,envCopyRule_Syn_CtorFieldDecl :: (Map AttrName AttrRef),sAtomEnv_Syn_CtorFieldDecl :: (Map FieldName NamespaceName),sFieldEnv_Syn_CtorFieldDecl :: (Map FieldName SortName),self_Syn_CtorFieldDecl :: CtorFieldDecl}
wrap_CtorFieldDecl :: T_CtorFieldDecl ->
                      Inh_CtorFieldDecl ->
                      Syn_CtorFieldDecl
wrap_CtorFieldDecl sem (Inh_CtorFieldDecl _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvCopyRule _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsImissingInhDef _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOcompletion,_lhsOcopyRuleDefs,_lhsOenvCopyRule,_lhsOsAtomEnv,_lhsOsFieldEnv,_lhsOself) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvCopyRule _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsImissingInhDef _lhsInamespaces _lhsIvarsorts
     in  (Syn_CtorFieldDecl _lhsOcompletion _lhsOcopyRuleDefs _lhsOenvCopyRule _lhsOsAtomEnv _lhsOsFieldEnv _lhsOself))
sem_CtorFieldDecl_CFRef :: FieldName ->
                           AttrName ->
                           T_CtorFieldDecl
sem_CtorFieldDecl_CFRef ctorFieldName_ ctorFieldType_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvCopyRule
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsImissingInhDef
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOcopyRuleDefs :: AttrDefs
              _lhsOsAtomEnv :: (Map FieldName NamespaceName)
              _lhsOsFieldEnv :: (Map FieldName SortName)
              _lhsOcompletion :: CtorFieldDecl
              _lhsOself :: CtorFieldDecl
              _lhsOenvCopyRule :: (Map AttrName AttrRef)
              _namespace =
                  ({-# LINE 123 "src/InBound/Environment.ag" #-}
                   case M.findWithDefault
                           (error "unknown context attribute")
                           ctorFieldType_
                           _lhsIlocEnvAttrInh of
                     Context ns -> ns
                   {-# LINE 1370 "src/InBound/CopyRules.hs" #-}
                   )
              _mbsort =
                  ({-# LINE 128 "src/InBound/Environment.ag" #-}
                   join $ M.lookup _namespace     _lhsInamespaces
                   {-# LINE 1375 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOcopyRuleDefs =
                  ({-# LINE 36 "src/InBound/CopyRules.ag" #-}
                   []
                   {-# LINE 1380 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAtomEnv =
                  ({-# LINE 54 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1385 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsFieldEnv =
                  ({-# LINE 53 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1390 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   CFRef ctorFieldName_ ctorFieldType_
                   {-# LINE 1395 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  CFRef ctorFieldName_ ctorFieldType_
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 1402 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOenvCopyRule =
                  ({-# LINE 30 "src/InBound/CopyRules.ag" #-}
                   _lhsIenvCopyRule
                   {-# LINE 1409 "src/InBound/CopyRules.hs" #-}
                   )
          in  ( _lhsOcompletion,_lhsOcopyRuleDefs,_lhsOenvCopyRule,_lhsOsAtomEnv,_lhsOsFieldEnv,_lhsOself)))
sem_CtorFieldDecl_CFAtom :: FieldName ->
                            NamespaceName ->
                            T_CtorFieldDecl
sem_CtorFieldDecl_CFAtom ctorFieldName_ ctorFieldType_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvCopyRule
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsImissingInhDef
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOsAtomEnv :: (Map FieldName NamespaceName)
              _lhsOcopyRuleDefs :: AttrDefs
              _lhsOsFieldEnv :: (Map FieldName SortName)
              _lhsOcompletion :: CtorFieldDecl
              _lhsOself :: CtorFieldDecl
              _lhsOenvCopyRule :: (Map AttrName AttrRef)
              _lhsOsAtomEnv =
                  ({-# LINE 58 "src/InBound/Environment.ag" #-}
                   M.singleton ctorFieldName_ ctorFieldType_
                   {-# LINE 1435 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOcopyRuleDefs =
                  ({-# LINE 36 "src/InBound/CopyRules.ag" #-}
                   []
                   {-# LINE 1440 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsFieldEnv =
                  ({-# LINE 53 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1445 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   CFAtom ctorFieldName_ ctorFieldType_
                   {-# LINE 1450 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  CFAtom ctorFieldName_ ctorFieldType_
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 1457 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOenvCopyRule =
                  ({-# LINE 30 "src/InBound/CopyRules.ag" #-}
                   _lhsIenvCopyRule
                   {-# LINE 1464 "src/InBound/CopyRules.hs" #-}
                   )
          in  ( _lhsOcompletion,_lhsOcopyRuleDefs,_lhsOenvCopyRule,_lhsOsAtomEnv,_lhsOsFieldEnv,_lhsOself)))
sem_CtorFieldDecl_CFSubtree :: FieldName ->
                               SortName ->
                               T_CtorFieldDecl
sem_CtorFieldDecl_CFSubtree ctorFieldName_ ctorFieldType_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvCopyRule
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsImissingInhDef
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOsFieldEnv :: (Map FieldName SortName)
              _lhsOcopyRuleDefs :: AttrDefs
              _lhsOenvCopyRule :: (Map AttrName AttrRef)
              _lhsOsAtomEnv :: (Map FieldName NamespaceName)
              _lhsOcompletion :: CtorFieldDecl
              _lhsOself :: CtorFieldDecl
              _lhsOsFieldEnv =
                  ({-# LINE 57 "src/InBound/Environment.ag" #-}
                   M.singleton ctorFieldName_ ctorFieldType_
                   {-# LINE 1490 "src/InBound/CopyRules.hs" #-}
                   )
              _subtreeSynAttrs =
                  ({-# LINE 131 "src/InBound/Environment.ag" #-}
                   [ (an,ty)
                   | ((sn,an),ty) <- M.toList _lhsIenvAttrSyn
                   , sn == ctorFieldType_
                   ]
                   {-# LINE 1498 "src/InBound/CopyRules.hs" #-}
                   )
              _missingInhDef =
                  ({-# LINE 74 "src/InBound/CopyRules.ag" #-}
                   M.findWithDefault []
                     ctorFieldName_
                     _lhsImissingInhDef
                   {-# LINE 1505 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOcopyRuleDefs =
                  ({-# LINE 80 "src/InBound/CopyRules.ag" #-}
                   [ AttrDef (AttrRef (Sub ctorFieldName_) an) (ExprAttrRef rhs)
                   | an <- _missingInhDef
                   , let Just rhs = M.lookup an _lhsIenvCopyRule
                   ]
                   {-# LINE 1513 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOenvCopyRule =
                  ({-# LINE 88 "src/InBound/CopyRules.ag" #-}
                   M.union
                     (M.fromList [ (an,AttrRef (Sub ctorFieldName_) an)
                                 | (an,_) <- _subtreeSynAttrs
                                 ])
                     _lhsIenvCopyRule
                   {-# LINE 1522 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAtomEnv =
                  ({-# LINE 54 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1527 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   CFSubtree ctorFieldName_ ctorFieldType_
                   {-# LINE 1532 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  CFSubtree ctorFieldName_ ctorFieldType_
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 1539 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
          in  ( _lhsOcompletion,_lhsOcopyRuleDefs,_lhsOenvCopyRule,_lhsOsAtomEnv,_lhsOsFieldEnv,_lhsOself)))
sem_CtorFieldDecl_CFTerminal :: FieldName ->
                                String ->
                                T_CtorFieldDecl
sem_CtorFieldDecl_CFTerminal ctorFieldName_ ctorFieldType_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvCopyRule
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsImissingInhDef
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOcopyRuleDefs :: AttrDefs
              _lhsOsAtomEnv :: (Map FieldName NamespaceName)
              _lhsOsFieldEnv :: (Map FieldName SortName)
              _lhsOcompletion :: CtorFieldDecl
              _lhsOself :: CtorFieldDecl
              _lhsOenvCopyRule :: (Map AttrName AttrRef)
              _lhsOcopyRuleDefs =
                  ({-# LINE 36 "src/InBound/CopyRules.ag" #-}
                   []
                   {-# LINE 1567 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAtomEnv =
                  ({-# LINE 54 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1572 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsFieldEnv =
                  ({-# LINE 53 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1577 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   CFTerminal ctorFieldName_ ctorFieldType_
                   {-# LINE 1582 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  CFTerminal ctorFieldName_ ctorFieldType_
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 1589 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOenvCopyRule =
                  ({-# LINE 30 "src/InBound/CopyRules.ag" #-}
                   _lhsIenvCopyRule
                   {-# LINE 1596 "src/InBound/CopyRules.hs" #-}
                   )
          in  ( _lhsOcompletion,_lhsOcopyRuleDefs,_lhsOenvCopyRule,_lhsOsAtomEnv,_lhsOsFieldEnv,_lhsOself)))
-- CtorFieldDecls ----------------------------------------------
-- cata
sem_CtorFieldDecls :: CtorFieldDecls ->
                      T_CtorFieldDecls
sem_CtorFieldDecls list =
    (Prelude.foldr sem_CtorFieldDecls_Cons sem_CtorFieldDecls_Nil (Prelude.map sem_CtorFieldDecl list))
-- semantic domain
type T_CtorFieldDecls = (Map (SortName,AttrName) Type) ->
                        (Map (SortName,AttrName) Type) ->
                        (Map AttrName AttrRef) ->
                        SortName ->
                        NamespaceNames ->
                        (Map AttrName Type) ->
                        (Map AttrName Type) ->
                        (Map FieldName [AttrName]) ->
                        (Map NamespaceName MbSortName) ->
                        (Map SortName [NamespaceName]) ->
                        ( CtorFieldDecls,AttrDefs,(Map AttrName AttrRef),(Map FieldName NamespaceName),(Map FieldName SortName),CtorFieldDecls)
data Inh_CtorFieldDecls = Inh_CtorFieldDecls {envAttrInh_Inh_CtorFieldDecls :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_CtorFieldDecls :: (Map (SortName,AttrName) Type),envCopyRule_Inh_CtorFieldDecls :: (Map AttrName AttrRef),envSort_Inh_CtorFieldDecls :: SortName,envVars_Inh_CtorFieldDecls :: NamespaceNames,locEnvAttrInh_Inh_CtorFieldDecls :: (Map AttrName Type),locEnvAttrSyn_Inh_CtorFieldDecls :: (Map AttrName Type),missingInhDef_Inh_CtorFieldDecls :: (Map FieldName [AttrName]),namespaces_Inh_CtorFieldDecls :: (Map NamespaceName MbSortName),varsorts_Inh_CtorFieldDecls :: (Map SortName [NamespaceName])}
data Syn_CtorFieldDecls = Syn_CtorFieldDecls {completion_Syn_CtorFieldDecls :: CtorFieldDecls,copyRuleDefs_Syn_CtorFieldDecls :: AttrDefs,envCopyRule_Syn_CtorFieldDecls :: (Map AttrName AttrRef),sAtomEnv_Syn_CtorFieldDecls :: (Map FieldName NamespaceName),sFieldEnv_Syn_CtorFieldDecls :: (Map FieldName SortName),self_Syn_CtorFieldDecls :: CtorFieldDecls}
wrap_CtorFieldDecls :: T_CtorFieldDecls ->
                       Inh_CtorFieldDecls ->
                       Syn_CtorFieldDecls
wrap_CtorFieldDecls sem (Inh_CtorFieldDecls _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvCopyRule _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsImissingInhDef _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOcompletion,_lhsOcopyRuleDefs,_lhsOenvCopyRule,_lhsOsAtomEnv,_lhsOsFieldEnv,_lhsOself) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvCopyRule _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsImissingInhDef _lhsInamespaces _lhsIvarsorts
     in  (Syn_CtorFieldDecls _lhsOcompletion _lhsOcopyRuleDefs _lhsOenvCopyRule _lhsOsAtomEnv _lhsOsFieldEnv _lhsOself))
sem_CtorFieldDecls_Cons :: T_CtorFieldDecl ->
                           T_CtorFieldDecls ->
                           T_CtorFieldDecls
sem_CtorFieldDecls_Cons hd_ tl_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvCopyRule
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsImissingInhDef
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOcopyRuleDefs :: AttrDefs
              _lhsOsAtomEnv :: (Map FieldName NamespaceName)
              _lhsOsFieldEnv :: (Map FieldName SortName)
              _lhsOcompletion :: CtorFieldDecls
              _lhsOself :: CtorFieldDecls
              _lhsOenvCopyRule :: (Map AttrName AttrRef)
              _hdOenvAttrInh :: (Map (SortName,AttrName) Type)
              _hdOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _hdOenvCopyRule :: (Map AttrName AttrRef)
              _hdOenvSort :: SortName
              _hdOenvVars :: NamespaceNames
              _hdOlocEnvAttrInh :: (Map AttrName Type)
              _hdOlocEnvAttrSyn :: (Map AttrName Type)
              _hdOmissingInhDef :: (Map FieldName [AttrName])
              _hdOnamespaces :: (Map NamespaceName MbSortName)
              _hdOvarsorts :: (Map SortName [NamespaceName])
              _tlOenvAttrInh :: (Map (SortName,AttrName) Type)
              _tlOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _tlOenvCopyRule :: (Map AttrName AttrRef)
              _tlOenvSort :: SortName
              _tlOenvVars :: NamespaceNames
              _tlOlocEnvAttrInh :: (Map AttrName Type)
              _tlOlocEnvAttrSyn :: (Map AttrName Type)
              _tlOmissingInhDef :: (Map FieldName [AttrName])
              _tlOnamespaces :: (Map NamespaceName MbSortName)
              _tlOvarsorts :: (Map SortName [NamespaceName])
              _hdIcompletion :: CtorFieldDecl
              _hdIcopyRuleDefs :: AttrDefs
              _hdIenvCopyRule :: (Map AttrName AttrRef)
              _hdIsAtomEnv :: (Map FieldName NamespaceName)
              _hdIsFieldEnv :: (Map FieldName SortName)
              _hdIself :: CtorFieldDecl
              _tlIcompletion :: CtorFieldDecls
              _tlIcopyRuleDefs :: AttrDefs
              _tlIenvCopyRule :: (Map AttrName AttrRef)
              _tlIsAtomEnv :: (Map FieldName NamespaceName)
              _tlIsFieldEnv :: (Map FieldName SortName)
              _tlIself :: CtorFieldDecls
              _lhsOcopyRuleDefs =
                  ({-# LINE 36 "src/InBound/CopyRules.ag" #-}
                   _hdIcopyRuleDefs ++ _tlIcopyRuleDefs
                   {-# LINE 1680 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAtomEnv =
                  ({-# LINE 54 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsAtomEnv _tlIsAtomEnv)
                   {-# LINE 1685 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsFieldEnv =
                  ({-# LINE 53 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsFieldEnv _tlIsFieldEnv)
                   {-# LINE 1690 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   (:) _hdIcompletion _tlIcompletion
                   {-# LINE 1695 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 1702 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOenvCopyRule =
                  ({-# LINE 30 "src/InBound/CopyRules.ag" #-}
                   _tlIenvCopyRule
                   {-# LINE 1709 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1714 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1719 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvCopyRule =
                  ({-# LINE 30 "src/InBound/CopyRules.ag" #-}
                   _lhsIenvCopyRule
                   {-# LINE 1724 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1729 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 1734 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 1739 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 1744 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOmissingInhDef =
                  ({-# LINE 34 "src/InBound/CopyRules.ag" #-}
                   _lhsImissingInhDef
                   {-# LINE 1749 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 1754 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 1759 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1764 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1769 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvCopyRule =
                  ({-# LINE 30 "src/InBound/CopyRules.ag" #-}
                   _hdIenvCopyRule
                   {-# LINE 1774 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1779 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 1784 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 1789 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 1794 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOmissingInhDef =
                  ({-# LINE 34 "src/InBound/CopyRules.ag" #-}
                   _lhsImissingInhDef
                   {-# LINE 1799 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 1804 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 1809 "src/InBound/CopyRules.hs" #-}
                   )
              ( _hdIcompletion,_hdIcopyRuleDefs,_hdIenvCopyRule,_hdIsAtomEnv,_hdIsFieldEnv,_hdIself) =
                  hd_ _hdOenvAttrInh _hdOenvAttrSyn _hdOenvCopyRule _hdOenvSort _hdOenvVars _hdOlocEnvAttrInh _hdOlocEnvAttrSyn _hdOmissingInhDef _hdOnamespaces _hdOvarsorts
              ( _tlIcompletion,_tlIcopyRuleDefs,_tlIenvCopyRule,_tlIsAtomEnv,_tlIsFieldEnv,_tlIself) =
                  tl_ _tlOenvAttrInh _tlOenvAttrSyn _tlOenvCopyRule _tlOenvSort _tlOenvVars _tlOlocEnvAttrInh _tlOlocEnvAttrSyn _tlOmissingInhDef _tlOnamespaces _tlOvarsorts
          in  ( _lhsOcompletion,_lhsOcopyRuleDefs,_lhsOenvCopyRule,_lhsOsAtomEnv,_lhsOsFieldEnv,_lhsOself)))
sem_CtorFieldDecls_Nil :: T_CtorFieldDecls
sem_CtorFieldDecls_Nil =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvCopyRule
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsImissingInhDef
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOcopyRuleDefs :: AttrDefs
              _lhsOsAtomEnv :: (Map FieldName NamespaceName)
              _lhsOsFieldEnv :: (Map FieldName SortName)
              _lhsOcompletion :: CtorFieldDecls
              _lhsOself :: CtorFieldDecls
              _lhsOenvCopyRule :: (Map AttrName AttrRef)
              _lhsOcopyRuleDefs =
                  ({-# LINE 36 "src/InBound/CopyRules.ag" #-}
                   []
                   {-# LINE 1837 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAtomEnv =
                  ({-# LINE 54 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1842 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsFieldEnv =
                  ({-# LINE 53 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1847 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   []
                   {-# LINE 1852 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  []
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 1859 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOenvCopyRule =
                  ({-# LINE 30 "src/InBound/CopyRules.ag" #-}
                   _lhsIenvCopyRule
                   {-# LINE 1866 "src/InBound/CopyRules.hs" #-}
                   )
          in  ( _lhsOcompletion,_lhsOcopyRuleDefs,_lhsOenvCopyRule,_lhsOsAtomEnv,_lhsOsFieldEnv,_lhsOself)))
-- Expr --------------------------------------------------------
-- cata
sem_Expr :: Expr ->
            T_Expr
sem_Expr (ExprAttrRef _attrRefName) =
    (sem_Expr_ExprAttrRef (sem_AttrRef _attrRefName))
sem_Expr (ExprNil) =
    (sem_Expr_ExprNil)
sem_Expr (ExprCons _tail _head) =
    (sem_Expr_ExprCons (sem_Expr _tail) _head)
-- semantic domain
type T_Expr = AttrRef ->
              Int ->
              (Map FieldName NamespaceName) ->
              (Map (SortName,AttrName) Type) ->
              (Map (SortName,AttrName) Type) ->
              (Map FieldName SortName) ->
              SortName ->
              NamespaceNames ->
              ( AttrRef,Expr,Expr)
data Inh_Expr = Inh_Expr {attrDefRef_Inh_Expr :: AttrRef,depth_Inh_Expr :: Int,envAtom_Inh_Expr :: (Map FieldName NamespaceName),envAttrInh_Inh_Expr :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_Expr :: (Map (SortName,AttrName) Type),envField_Inh_Expr :: (Map FieldName SortName),envSort_Inh_Expr :: SortName,envVars_Inh_Expr :: NamespaceNames}
data Syn_Expr = Syn_Expr {attrName_Syn_Expr :: AttrRef,completion_Syn_Expr :: Expr,self_Syn_Expr :: Expr}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr _lhsIattrDefRef _lhsIdepth _lhsIenvAtom _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvField _lhsIenvSort _lhsIenvVars) =
    (let ( _lhsOattrName,_lhsOcompletion,_lhsOself) = sem _lhsIattrDefRef _lhsIdepth _lhsIenvAtom _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvField _lhsIenvSort _lhsIenvVars
     in  (Syn_Expr _lhsOattrName _lhsOcompletion _lhsOself))
sem_Expr_ExprAttrRef :: T_AttrRef ->
                        T_Expr
sem_Expr_ExprAttrRef attrRefName_ =
    (\ _lhsIattrDefRef
       _lhsIdepth
       _lhsIenvAtom
       _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvField
       _lhsIenvSort
       _lhsIenvVars ->
         (let _lhsOcompletion :: Expr
              _lhsOself :: Expr
              _lhsOattrName :: AttrRef
              _attrRefNameIcompletion :: AttrRef
              _attrRefNameIself :: AttrRef
              _attrName =
                  ({-# LINE 221 "src/InBound/Environment.ag" #-}
                   _attrRefNameIself
                   {-# LINE 1916 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   ExprAttrRef _attrRefNameIcompletion
                   {-# LINE 1921 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  ExprAttrRef _attrRefNameIself
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 1928 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOattrName =
                  ({-# LINE 188 "src/InBound/Environment.ag" #-}
                   _attrName
                   {-# LINE 1935 "src/InBound/CopyRules.hs" #-}
                   )
              ( _attrRefNameIcompletion,_attrRefNameIself) =
                  attrRefName_
          in  ( _lhsOattrName,_lhsOcompletion,_lhsOself)))
sem_Expr_ExprNil :: T_Expr
sem_Expr_ExprNil =
    (\ _lhsIattrDefRef
       _lhsIdepth
       _lhsIenvAtom
       _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvField
       _lhsIenvSort
       _lhsIenvVars ->
         (let _lhsOcompletion :: Expr
              _lhsOself :: Expr
              _lhsOattrName :: AttrRef
              _attrName =
                  ({-# LINE 223 "src/InBound/Environment.ag" #-}
                   localAttrRef _lhsIattrDefRef _lhsIdepth
                   {-# LINE 1956 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   ExprNil
                   {-# LINE 1961 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  ExprNil
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 1968 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOattrName =
                  ({-# LINE 188 "src/InBound/Environment.ag" #-}
                   _attrName
                   {-# LINE 1975 "src/InBound/CopyRules.hs" #-}
                   )
          in  ( _lhsOattrName,_lhsOcompletion,_lhsOself)))
sem_Expr_ExprCons :: T_Expr ->
                     FieldName ->
                     T_Expr
sem_Expr_ExprCons tail_ head_ =
    (\ _lhsIattrDefRef
       _lhsIdepth
       _lhsIenvAtom
       _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvField
       _lhsIenvSort
       _lhsIenvVars ->
         (let _tailOdepth :: Int
              _lhsOcompletion :: Expr
              _lhsOself :: Expr
              _lhsOattrName :: AttrRef
              _tailOattrDefRef :: AttrRef
              _tailOenvAtom :: (Map FieldName NamespaceName)
              _tailOenvAttrInh :: (Map (SortName,AttrName) Type)
              _tailOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _tailOenvField :: (Map FieldName SortName)
              _tailOenvSort :: SortName
              _tailOenvVars :: NamespaceNames
              _tailIattrName :: AttrRef
              _tailIcompletion :: Expr
              _tailIself :: Expr
              _attrName =
                  ({-# LINE 225 "src/InBound/Environment.ag" #-}
                   localAttrRef _lhsIattrDefRef _lhsIdepth
                   {-# LINE 2007 "src/InBound/CopyRules.hs" #-}
                   )
              _tailOdepth =
                  ({-# LINE 226 "src/InBound/Environment.ag" #-}
                   1 + _lhsIdepth
                   {-# LINE 2012 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   ExprCons _tailIcompletion head_
                   {-# LINE 2017 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  ExprCons _tailIself head_
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 2024 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOattrName =
                  ({-# LINE 188 "src/InBound/Environment.ag" #-}
                   _attrName
                   {-# LINE 2031 "src/InBound/CopyRules.hs" #-}
                   )
              _tailOattrDefRef =
                  ({-# LINE 185 "src/InBound/Environment.ag" #-}
                   _lhsIattrDefRef
                   {-# LINE 2036 "src/InBound/CopyRules.hs" #-}
                   )
              _tailOenvAtom =
                  ({-# LINE 62 "src/InBound/Environment.ag" #-}
                   _lhsIenvAtom
                   {-# LINE 2041 "src/InBound/CopyRules.hs" #-}
                   )
              _tailOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2046 "src/InBound/CopyRules.hs" #-}
                   )
              _tailOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2051 "src/InBound/CopyRules.hs" #-}
                   )
              _tailOenvField =
                  ({-# LINE 61 "src/InBound/Environment.ag" #-}
                   _lhsIenvField
                   {-# LINE 2056 "src/InBound/CopyRules.hs" #-}
                   )
              _tailOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2061 "src/InBound/CopyRules.hs" #-}
                   )
              _tailOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 2066 "src/InBound/CopyRules.hs" #-}
                   )
              ( _tailIattrName,_tailIcompletion,_tailIself) =
                  tail_ _tailOattrDefRef _tailOdepth _tailOenvAtom _tailOenvAttrInh _tailOenvAttrSyn _tailOenvField _tailOenvSort _tailOenvVars
          in  ( _lhsOattrName,_lhsOcompletion,_lhsOself)))
-- LocAttrDecl -------------------------------------------------
-- cata
sem_LocAttrDecl :: LocAttrDecl ->
                   T_LocAttrDecl
sem_LocAttrDecl (LocAttrDecl _attrName _attrType) =
    (sem_LocAttrDecl_LocAttrDecl _attrName (sem_Type _attrType))
-- semantic domain
type T_LocAttrDecl = (Map NamespaceName MbSortName) ->
                     (Map SortName [NamespaceName]) ->
                     ( LocAttrDecl,LocAttrDecl)
data Inh_LocAttrDecl = Inh_LocAttrDecl {namespaces_Inh_LocAttrDecl :: (Map NamespaceName MbSortName),varsorts_Inh_LocAttrDecl :: (Map SortName [NamespaceName])}
data Syn_LocAttrDecl = Syn_LocAttrDecl {completion_Syn_LocAttrDecl :: LocAttrDecl,self_Syn_LocAttrDecl :: LocAttrDecl}
wrap_LocAttrDecl :: T_LocAttrDecl ->
                    Inh_LocAttrDecl ->
                    Syn_LocAttrDecl
wrap_LocAttrDecl sem (Inh_LocAttrDecl _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOcompletion,_lhsOself) = sem _lhsInamespaces _lhsIvarsorts
     in  (Syn_LocAttrDecl _lhsOcompletion _lhsOself))
sem_LocAttrDecl_LocAttrDecl :: AttrName ->
                               T_Type ->
                               T_LocAttrDecl
sem_LocAttrDecl_LocAttrDecl attrName_ attrType_ =
    (\ _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOcompletion :: LocAttrDecl
              _lhsOself :: LocAttrDecl
              _attrTypeIcompletion :: Type
              _attrTypeIself :: Type
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   LocAttrDecl attrName_ _attrTypeIcompletion
                   {-# LINE 2102 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  LocAttrDecl attrName_ _attrTypeIself
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 2109 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              ( _attrTypeIcompletion,_attrTypeIself) =
                  attrType_
          in  ( _lhsOcompletion,_lhsOself)))
-- LocAttrDecls ------------------------------------------------
-- cata
sem_LocAttrDecls :: LocAttrDecls ->
                    T_LocAttrDecls
sem_LocAttrDecls list =
    (Prelude.foldr sem_LocAttrDecls_Cons sem_LocAttrDecls_Nil (Prelude.map sem_LocAttrDecl list))
-- semantic domain
type T_LocAttrDecls = (Map NamespaceName MbSortName) ->
                      (Map SortName [NamespaceName]) ->
                      ( LocAttrDecls,LocAttrDecls)
data Inh_LocAttrDecls = Inh_LocAttrDecls {namespaces_Inh_LocAttrDecls :: (Map NamespaceName MbSortName),varsorts_Inh_LocAttrDecls :: (Map SortName [NamespaceName])}
data Syn_LocAttrDecls = Syn_LocAttrDecls {completion_Syn_LocAttrDecls :: LocAttrDecls,self_Syn_LocAttrDecls :: LocAttrDecls}
wrap_LocAttrDecls :: T_LocAttrDecls ->
                     Inh_LocAttrDecls ->
                     Syn_LocAttrDecls
wrap_LocAttrDecls sem (Inh_LocAttrDecls _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOcompletion,_lhsOself) = sem _lhsInamespaces _lhsIvarsorts
     in  (Syn_LocAttrDecls _lhsOcompletion _lhsOself))
sem_LocAttrDecls_Cons :: T_LocAttrDecl ->
                         T_LocAttrDecls ->
                         T_LocAttrDecls
sem_LocAttrDecls_Cons hd_ tl_ =
    (\ _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOcompletion :: LocAttrDecls
              _lhsOself :: LocAttrDecls
              _hdOnamespaces :: (Map NamespaceName MbSortName)
              _hdOvarsorts :: (Map SortName [NamespaceName])
              _tlOnamespaces :: (Map NamespaceName MbSortName)
              _tlOvarsorts :: (Map SortName [NamespaceName])
              _hdIcompletion :: LocAttrDecl
              _hdIself :: LocAttrDecl
              _tlIcompletion :: LocAttrDecls
              _tlIself :: LocAttrDecls
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   (:) _hdIcompletion _tlIcompletion
                   {-# LINE 2153 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 2160 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              _hdOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 2167 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 2172 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 2177 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 2182 "src/InBound/CopyRules.hs" #-}
                   )
              ( _hdIcompletion,_hdIself) =
                  hd_ _hdOnamespaces _hdOvarsorts
              ( _tlIcompletion,_tlIself) =
                  tl_ _tlOnamespaces _tlOvarsorts
          in  ( _lhsOcompletion,_lhsOself)))
sem_LocAttrDecls_Nil :: T_LocAttrDecls
sem_LocAttrDecls_Nil =
    (\ _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOcompletion :: LocAttrDecls
              _lhsOself :: LocAttrDecls
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   []
                   {-# LINE 2198 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  []
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 2205 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
          in  ( _lhsOcompletion,_lhsOself)))
-- MbSortName --------------------------------------------------
-- cata
sem_MbSortName :: MbSortName ->
                  T_MbSortName
sem_MbSortName (Prelude.Just x) =
    (sem_MbSortName_Just x)
sem_MbSortName Prelude.Nothing =
    sem_MbSortName_Nothing
-- semantic domain
type T_MbSortName = ( MbSortName,MbSortName)
data Inh_MbSortName = Inh_MbSortName {}
data Syn_MbSortName = Syn_MbSortName {completion_Syn_MbSortName :: MbSortName,self_Syn_MbSortName :: MbSortName}
wrap_MbSortName :: T_MbSortName ->
                   Inh_MbSortName ->
                   Syn_MbSortName
wrap_MbSortName sem (Inh_MbSortName) =
    (let ( _lhsOcompletion,_lhsOself) = sem
     in  (Syn_MbSortName _lhsOcompletion _lhsOself))
sem_MbSortName_Just :: SortName ->
                       T_MbSortName
sem_MbSortName_Just just_ =
    (let _lhsOcompletion :: MbSortName
         _lhsOself :: MbSortName
         _completion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              Just just_
              {-# LINE 2236 "src/InBound/CopyRules.hs" #-}
              )
         _self =
             Just just_
         _lhsOcompletion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              _completion
              {-# LINE 2243 "src/InBound/CopyRules.hs" #-}
              )
         _lhsOself =
             _self
     in  ( _lhsOcompletion,_lhsOself))
sem_MbSortName_Nothing :: T_MbSortName
sem_MbSortName_Nothing =
    (let _lhsOcompletion :: MbSortName
         _lhsOself :: MbSortName
         _completion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              Nothing
              {-# LINE 2255 "src/InBound/CopyRules.hs" #-}
              )
         _self =
             Nothing
         _lhsOcompletion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              _completion
              {-# LINE 2262 "src/InBound/CopyRules.hs" #-}
              )
         _lhsOself =
             _self
     in  ( _lhsOcompletion,_lhsOself))
-- NamespaceDecl -----------------------------------------------
-- cata
sem_NamespaceDecl :: NamespaceDecl ->
                     T_NamespaceDecl
sem_NamespaceDecl (NamespaceDecl _namespaceName _namespaceTarget) =
    (sem_NamespaceDecl_NamespaceDecl _namespaceName (sem_MbSortName _namespaceTarget))
-- semantic domain
type T_NamespaceDecl = ( NamespaceDecl,(Map NamespaceName MbSortName),NamespaceDecl)
data Inh_NamespaceDecl = Inh_NamespaceDecl {}
data Syn_NamespaceDecl = Syn_NamespaceDecl {completion_Syn_NamespaceDecl :: NamespaceDecl,namespaces_Syn_NamespaceDecl :: (Map NamespaceName MbSortName),self_Syn_NamespaceDecl :: NamespaceDecl}
wrap_NamespaceDecl :: T_NamespaceDecl ->
                      Inh_NamespaceDecl ->
                      Syn_NamespaceDecl
wrap_NamespaceDecl sem (Inh_NamespaceDecl) =
    (let ( _lhsOcompletion,_lhsOnamespaces,_lhsOself) = sem
     in  (Syn_NamespaceDecl _lhsOcompletion _lhsOnamespaces _lhsOself))
sem_NamespaceDecl_NamespaceDecl :: NamespaceName ->
                                   T_MbSortName ->
                                   T_NamespaceDecl
sem_NamespaceDecl_NamespaceDecl namespaceName_ namespaceTarget_ =
    (let _lhsOnamespaces :: (Map NamespaceName MbSortName)
         _lhsOcompletion :: NamespaceDecl
         _lhsOself :: NamespaceDecl
         _namespaceTargetIcompletion :: MbSortName
         _namespaceTargetIself :: MbSortName
         _lhsOnamespaces =
             ({-# LINE 21 "src/InBound/Environment.ag" #-}
              M.singleton namespaceName_ _namespaceTargetIself
              {-# LINE 2295 "src/InBound/CopyRules.hs" #-}
              )
         _completion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              NamespaceDecl namespaceName_ _namespaceTargetIcompletion
              {-# LINE 2300 "src/InBound/CopyRules.hs" #-}
              )
         _self =
             NamespaceDecl namespaceName_ _namespaceTargetIself
         _lhsOcompletion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              _completion
              {-# LINE 2307 "src/InBound/CopyRules.hs" #-}
              )
         _lhsOself =
             _self
         ( _namespaceTargetIcompletion,_namespaceTargetIself) =
             namespaceTarget_
     in  ( _lhsOcompletion,_lhsOnamespaces,_lhsOself))
-- NamespaceDecls ----------------------------------------------
-- cata
sem_NamespaceDecls :: NamespaceDecls ->
                      T_NamespaceDecls
sem_NamespaceDecls list =
    (Prelude.foldr sem_NamespaceDecls_Cons sem_NamespaceDecls_Nil (Prelude.map sem_NamespaceDecl list))
-- semantic domain
type T_NamespaceDecls = ( NamespaceDecls,(Map NamespaceName MbSortName),NamespaceDecls)
data Inh_NamespaceDecls = Inh_NamespaceDecls {}
data Syn_NamespaceDecls = Syn_NamespaceDecls {completion_Syn_NamespaceDecls :: NamespaceDecls,namespaces_Syn_NamespaceDecls :: (Map NamespaceName MbSortName),self_Syn_NamespaceDecls :: NamespaceDecls}
wrap_NamespaceDecls :: T_NamespaceDecls ->
                       Inh_NamespaceDecls ->
                       Syn_NamespaceDecls
wrap_NamespaceDecls sem (Inh_NamespaceDecls) =
    (let ( _lhsOcompletion,_lhsOnamespaces,_lhsOself) = sem
     in  (Syn_NamespaceDecls _lhsOcompletion _lhsOnamespaces _lhsOself))
sem_NamespaceDecls_Cons :: T_NamespaceDecl ->
                           T_NamespaceDecls ->
                           T_NamespaceDecls
sem_NamespaceDecls_Cons hd_ tl_ =
    (let _lhsOnamespaces :: (Map NamespaceName MbSortName)
         _lhsOcompletion :: NamespaceDecls
         _lhsOself :: NamespaceDecls
         _hdIcompletion :: NamespaceDecl
         _hdInamespaces :: (Map NamespaceName MbSortName)
         _hdIself :: NamespaceDecl
         _tlIcompletion :: NamespaceDecls
         _tlInamespaces :: (Map NamespaceName MbSortName)
         _tlIself :: NamespaceDecls
         _lhsOnamespaces =
             ({-# LINE 17 "src/InBound/Environment.ag" #-}
              (M.union _hdInamespaces _tlInamespaces)
              {-# LINE 2346 "src/InBound/CopyRules.hs" #-}
              )
         _completion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              (:) _hdIcompletion _tlIcompletion
              {-# LINE 2351 "src/InBound/CopyRules.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOcompletion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              _completion
              {-# LINE 2358 "src/InBound/CopyRules.hs" #-}
              )
         _lhsOself =
             _self
         ( _hdIcompletion,_hdInamespaces,_hdIself) =
             hd_
         ( _tlIcompletion,_tlInamespaces,_tlIself) =
             tl_
     in  ( _lhsOcompletion,_lhsOnamespaces,_lhsOself))
sem_NamespaceDecls_Nil :: T_NamespaceDecls
sem_NamespaceDecls_Nil =
    (let _lhsOnamespaces :: (Map NamespaceName MbSortName)
         _lhsOcompletion :: NamespaceDecls
         _lhsOself :: NamespaceDecls
         _lhsOnamespaces =
             ({-# LINE 17 "src/InBound/Environment.ag" #-}
              M.empty
              {-# LINE 2375 "src/InBound/CopyRules.hs" #-}
              )
         _completion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              []
              {-# LINE 2380 "src/InBound/CopyRules.hs" #-}
              )
         _self =
             []
         _lhsOcompletion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              _completion
              {-# LINE 2387 "src/InBound/CopyRules.hs" #-}
              )
         _lhsOself =
             _self
     in  ( _lhsOcompletion,_lhsOnamespaces,_lhsOself))
-- NamespaceNames ----------------------------------------------
-- cata
sem_NamespaceNames :: NamespaceNames ->
                      T_NamespaceNames
sem_NamespaceNames list =
    (Prelude.foldr sem_NamespaceNames_Cons sem_NamespaceNames_Nil list)
-- semantic domain
type T_NamespaceNames = ( NamespaceNames)
data Inh_NamespaceNames = Inh_NamespaceNames {}
data Syn_NamespaceNames = Syn_NamespaceNames {self_Syn_NamespaceNames :: NamespaceNames}
wrap_NamespaceNames :: T_NamespaceNames ->
                       Inh_NamespaceNames ->
                       Syn_NamespaceNames
wrap_NamespaceNames sem (Inh_NamespaceNames) =
    (let ( _lhsOself) = sem
     in  (Syn_NamespaceNames _lhsOself))
sem_NamespaceNames_Cons :: NamespaceName ->
                           T_NamespaceNames ->
                           T_NamespaceNames
sem_NamespaceNames_Cons hd_ tl_ =
    (let _lhsOself :: NamespaceNames
         _tlIself :: NamespaceNames
         _self =
             (:) hd_ _tlIself
         _lhsOself =
             _self
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_NamespaceNames_Nil :: T_NamespaceNames
sem_NamespaceNames_Nil =
    (let _lhsOself :: NamespaceNames
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- NodeLabel ---------------------------------------------------
-- cata
sem_NodeLabel :: NodeLabel ->
                 T_NodeLabel
sem_NodeLabel (Lhs) =
    (sem_NodeLabel_Lhs)
sem_NodeLabel (Loc) =
    (sem_NodeLabel_Loc)
sem_NodeLabel (Sub _nodeFieldLabel) =
    (sem_NodeLabel_Sub _nodeFieldLabel)
-- semantic domain
type T_NodeLabel = ( NodeLabel,NodeLabel)
data Inh_NodeLabel = Inh_NodeLabel {}
data Syn_NodeLabel = Syn_NodeLabel {completion_Syn_NodeLabel :: NodeLabel,self_Syn_NodeLabel :: NodeLabel}
wrap_NodeLabel :: T_NodeLabel ->
                  Inh_NodeLabel ->
                  Syn_NodeLabel
wrap_NodeLabel sem (Inh_NodeLabel) =
    (let ( _lhsOcompletion,_lhsOself) = sem
     in  (Syn_NodeLabel _lhsOcompletion _lhsOself))
sem_NodeLabel_Lhs :: T_NodeLabel
sem_NodeLabel_Lhs =
    (let _lhsOcompletion :: NodeLabel
         _lhsOself :: NodeLabel
         _completion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              Lhs
              {-# LINE 2456 "src/InBound/CopyRules.hs" #-}
              )
         _self =
             Lhs
         _lhsOcompletion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              _completion
              {-# LINE 2463 "src/InBound/CopyRules.hs" #-}
              )
         _lhsOself =
             _self
     in  ( _lhsOcompletion,_lhsOself))
sem_NodeLabel_Loc :: T_NodeLabel
sem_NodeLabel_Loc =
    (let _lhsOcompletion :: NodeLabel
         _lhsOself :: NodeLabel
         _completion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              Loc
              {-# LINE 2475 "src/InBound/CopyRules.hs" #-}
              )
         _self =
             Loc
         _lhsOcompletion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              _completion
              {-# LINE 2482 "src/InBound/CopyRules.hs" #-}
              )
         _lhsOself =
             _self
     in  ( _lhsOcompletion,_lhsOself))
sem_NodeLabel_Sub :: FieldName ->
                     T_NodeLabel
sem_NodeLabel_Sub nodeFieldLabel_ =
    (let _lhsOcompletion :: NodeLabel
         _lhsOself :: NodeLabel
         _completion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              Sub nodeFieldLabel_
              {-# LINE 2495 "src/InBound/CopyRules.hs" #-}
              )
         _self =
             Sub nodeFieldLabel_
         _lhsOcompletion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              _completion
              {-# LINE 2502 "src/InBound/CopyRules.hs" #-}
              )
         _lhsOself =
             _self
     in  ( _lhsOcompletion,_lhsOself))
-- SortDecl ----------------------------------------------------
-- cata
sem_SortDecl :: SortDecl ->
                T_SortDecl
sem_SortDecl (SortDecl _sortName _sortAttributes _sortCtors) =
    (sem_SortDecl_SortDecl _sortName (sem_AttrDecls _sortAttributes) (sem_CtorDecls _sortCtors))
-- semantic domain
type T_SortDecl = (Map (SortName,AttrName) Type) ->
                  (Map (SortName,AttrName) Type) ->
                  (Map NamespaceName MbSortName) ->
                  (Map SortName [NamespaceName]) ->
                  ( SortDecl,(Map (SortName,AttrName) Type),(Map (SortName,AttrName) Type),SortDecl)
data Inh_SortDecl = Inh_SortDecl {envAttrInh_Inh_SortDecl :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_SortDecl :: (Map (SortName,AttrName) Type),namespaces_Inh_SortDecl :: (Map NamespaceName MbSortName),varsorts_Inh_SortDecl :: (Map SortName [NamespaceName])}
data Syn_SortDecl = Syn_SortDecl {completion_Syn_SortDecl :: SortDecl,sAttrInh_Syn_SortDecl :: (Map (SortName,AttrName) Type),sAttrSyn_Syn_SortDecl :: (Map (SortName,AttrName) Type),self_Syn_SortDecl :: SortDecl}
wrap_SortDecl :: T_SortDecl ->
                 Inh_SortDecl ->
                 Syn_SortDecl
wrap_SortDecl sem (Inh_SortDecl _lhsIenvAttrInh _lhsIenvAttrSyn _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsInamespaces _lhsIvarsorts
     in  (Syn_SortDecl _lhsOcompletion _lhsOsAttrInh _lhsOsAttrSyn _lhsOself))
sem_SortDecl_SortDecl :: SortName ->
                         T_AttrDecls ->
                         T_CtorDecls ->
                         T_SortDecl
sem_SortDecl_SortDecl sortName_ sortAttributes_ sortCtors_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOsAttrSyn :: (Map (SortName,AttrName) Type)
              _lhsOsAttrInh :: (Map (SortName,AttrName) Type)
              _sortCtorsOenvSetAttrDef :: (Map AttrRef Type)
              _sortCtorsOenvSetAttrUse :: (Map AttrRef Type)
              _lhsOcompletion :: SortDecl
              _lhsOself :: SortDecl
              _sortAttributesOenvSort :: SortName
              _sortAttributesOenvVars :: NamespaceNames
              _sortAttributesOnamespaces :: (Map NamespaceName MbSortName)
              _sortAttributesOvarsorts :: (Map SortName [NamespaceName])
              _sortCtorsOenvAttrInh :: (Map (SortName,AttrName) Type)
              _sortCtorsOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _sortCtorsOenvSort :: SortName
              _sortCtorsOenvVars :: NamespaceNames
              _sortCtorsOlocEnvAttrInh :: (Map AttrName Type)
              _sortCtorsOlocEnvAttrSyn :: (Map AttrName Type)
              _sortCtorsOnamespaces :: (Map NamespaceName MbSortName)
              _sortCtorsOvarsorts :: (Map SortName [NamespaceName])
              _sortAttributesIcompletion :: AttrDecls
              _sortAttributesIsAttrInh :: (Map AttrName Type)
              _sortAttributesIsAttrSyn :: (Map AttrName Type)
              _sortAttributesIself :: AttrDecls
              _sortCtorsIcompletion :: CtorDecls
              _sortCtorsIsAttrInh :: (Map (SortName,AttrName) Type)
              _sortCtorsIsAttrSyn :: (Map (SortName,AttrName) Type)
              _sortCtorsIsSetAttrDef :: (Map AttrRef Expr)
              _sortCtorsIself :: CtorDecls
              _envSort =
                  ({-# LINE 48 "src/InBound/Environment.ag" #-}
                   sortName_
                   {-# LINE 2566 "src/InBound/CopyRules.hs" #-}
                   )
              _envVars =
                  ({-# LINE 49 "src/InBound/Environment.ag" #-}
                   M.findWithDefault [] sortName_ _lhsIvarsorts
                   {-# LINE 2571 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 104 "src/InBound/Environment.ag" #-}
                   M.mapKeysMonotonic
                     (\an -> (sortName_,an))
                     _sortAttributesIsAttrSyn
                   {-# LINE 2578 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 107 "src/InBound/Environment.ag" #-}
                   M.mapKeysMonotonic
                     (\an -> (sortName_,an))
                     _sortAttributesIsAttrInh
                   {-# LINE 2585 "src/InBound/CopyRules.hs" #-}
                   )
              _locEnvAttrSyn =
                  ({-# LINE 113 "src/InBound/Environment.ag" #-}
                   _sortAttributesIsAttrSyn
                   {-# LINE 2590 "src/InBound/CopyRules.hs" #-}
                   )
              _locEnvAttrInh =
                  ({-# LINE 114 "src/InBound/Environment.ag" #-}
                   _sortAttributesIsAttrInh
                   {-# LINE 2595 "src/InBound/CopyRules.hs" #-}
                   )
              _sortCtorsOenvSetAttrDef =
                  ({-# LINE 157 "src/InBound/Environment.ag" #-}
                   M.mapKeysMonotonic (\an -> AttrRef Lhs an) _locEnvAttrSyn
                   {-# LINE 2600 "src/InBound/CopyRules.hs" #-}
                   )
              _sortCtorsOenvSetAttrUse =
                  ({-# LINE 160 "src/InBound/Environment.ag" #-}
                   M.mapKeysMonotonic (\an -> AttrRef Lhs an) _locEnvAttrInh
                   {-# LINE 2605 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   SortDecl sortName_ _sortAttributesIcompletion _sortCtorsIcompletion
                   {-# LINE 2610 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  SortDecl sortName_ _sortAttributesIself _sortCtorsIself
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 2617 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              _sortAttributesOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _envSort
                   {-# LINE 2624 "src/InBound/CopyRules.hs" #-}
                   )
              _sortAttributesOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _envVars
                   {-# LINE 2629 "src/InBound/CopyRules.hs" #-}
                   )
              _sortAttributesOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 2634 "src/InBound/CopyRules.hs" #-}
                   )
              _sortAttributesOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 2639 "src/InBound/CopyRules.hs" #-}
                   )
              _sortCtorsOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2644 "src/InBound/CopyRules.hs" #-}
                   )
              _sortCtorsOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2649 "src/InBound/CopyRules.hs" #-}
                   )
              _sortCtorsOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _envSort
                   {-# LINE 2654 "src/InBound/CopyRules.hs" #-}
                   )
              _sortCtorsOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _envVars
                   {-# LINE 2659 "src/InBound/CopyRules.hs" #-}
                   )
              _sortCtorsOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _locEnvAttrInh
                   {-# LINE 2664 "src/InBound/CopyRules.hs" #-}
                   )
              _sortCtorsOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _locEnvAttrSyn
                   {-# LINE 2669 "src/InBound/CopyRules.hs" #-}
                   )
              _sortCtorsOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 2674 "src/InBound/CopyRules.hs" #-}
                   )
              _sortCtorsOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 2679 "src/InBound/CopyRules.hs" #-}
                   )
              ( _sortAttributesIcompletion,_sortAttributesIsAttrInh,_sortAttributesIsAttrSyn,_sortAttributesIself) =
                  sortAttributes_ _sortAttributesOenvSort _sortAttributesOenvVars _sortAttributesOnamespaces _sortAttributesOvarsorts
              ( _sortCtorsIcompletion,_sortCtorsIsAttrInh,_sortCtorsIsAttrSyn,_sortCtorsIsSetAttrDef,_sortCtorsIself) =
                  sortCtors_ _sortCtorsOenvAttrInh _sortCtorsOenvAttrSyn _sortCtorsOenvSetAttrDef _sortCtorsOenvSetAttrUse _sortCtorsOenvSort _sortCtorsOenvVars _sortCtorsOlocEnvAttrInh _sortCtorsOlocEnvAttrSyn _sortCtorsOnamespaces _sortCtorsOvarsorts
          in  ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself)))
-- SortDecls ---------------------------------------------------
-- cata
sem_SortDecls :: SortDecls ->
                 T_SortDecls
sem_SortDecls list =
    (Prelude.foldr sem_SortDecls_Cons sem_SortDecls_Nil (Prelude.map sem_SortDecl list))
-- semantic domain
type T_SortDecls = (Map (SortName,AttrName) Type) ->
                   (Map (SortName,AttrName) Type) ->
                   (Map NamespaceName MbSortName) ->
                   (Map SortName [NamespaceName]) ->
                   ( SortDecls,(Map (SortName,AttrName) Type),(Map (SortName,AttrName) Type),SortDecls)
data Inh_SortDecls = Inh_SortDecls {envAttrInh_Inh_SortDecls :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_SortDecls :: (Map (SortName,AttrName) Type),namespaces_Inh_SortDecls :: (Map NamespaceName MbSortName),varsorts_Inh_SortDecls :: (Map SortName [NamespaceName])}
data Syn_SortDecls = Syn_SortDecls {completion_Syn_SortDecls :: SortDecls,sAttrInh_Syn_SortDecls :: (Map (SortName,AttrName) Type),sAttrSyn_Syn_SortDecls :: (Map (SortName,AttrName) Type),self_Syn_SortDecls :: SortDecls}
wrap_SortDecls :: T_SortDecls ->
                  Inh_SortDecls ->
                  Syn_SortDecls
wrap_SortDecls sem (Inh_SortDecls _lhsIenvAttrInh _lhsIenvAttrSyn _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsInamespaces _lhsIvarsorts
     in  (Syn_SortDecls _lhsOcompletion _lhsOsAttrInh _lhsOsAttrSyn _lhsOself))
sem_SortDecls_Cons :: T_SortDecl ->
                      T_SortDecls ->
                      T_SortDecls
sem_SortDecls_Cons hd_ tl_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOsAttrInh :: (Map (SortName,AttrName) Type)
              _lhsOsAttrSyn :: (Map (SortName,AttrName) Type)
              _lhsOcompletion :: SortDecls
              _lhsOself :: SortDecls
              _hdOenvAttrInh :: (Map (SortName,AttrName) Type)
              _hdOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _hdOnamespaces :: (Map NamespaceName MbSortName)
              _hdOvarsorts :: (Map SortName [NamespaceName])
              _tlOenvAttrInh :: (Map (SortName,AttrName) Type)
              _tlOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _tlOnamespaces :: (Map NamespaceName MbSortName)
              _tlOvarsorts :: (Map SortName [NamespaceName])
              _hdIcompletion :: SortDecl
              _hdIsAttrInh :: (Map (SortName,AttrName) Type)
              _hdIsAttrSyn :: (Map (SortName,AttrName) Type)
              _hdIself :: SortDecl
              _tlIcompletion :: SortDecls
              _tlIsAttrInh :: (Map (SortName,AttrName) Type)
              _tlIsAttrSyn :: (Map (SortName,AttrName) Type)
              _tlIself :: SortDecls
              _lhsOsAttrInh =
                  ({-# LINE 83 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsAttrInh _tlIsAttrInh)
                   {-# LINE 2737 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 82 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsAttrSyn _tlIsAttrSyn)
                   {-# LINE 2742 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   (:) _hdIcompletion _tlIcompletion
                   {-# LINE 2747 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 2754 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
              _hdOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2761 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2766 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 2771 "src/InBound/CopyRules.hs" #-}
                   )
              _hdOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 2776 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2781 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2786 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 2791 "src/InBound/CopyRules.hs" #-}
                   )
              _tlOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 2796 "src/InBound/CopyRules.hs" #-}
                   )
              ( _hdIcompletion,_hdIsAttrInh,_hdIsAttrSyn,_hdIself) =
                  hd_ _hdOenvAttrInh _hdOenvAttrSyn _hdOnamespaces _hdOvarsorts
              ( _tlIcompletion,_tlIsAttrInh,_tlIsAttrSyn,_tlIself) =
                  tl_ _tlOenvAttrInh _tlOenvAttrSyn _tlOnamespaces _tlOvarsorts
          in  ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself)))
sem_SortDecls_Nil :: T_SortDecls
sem_SortDecls_Nil =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOsAttrInh :: (Map (SortName,AttrName) Type)
              _lhsOsAttrSyn :: (Map (SortName,AttrName) Type)
              _lhsOcompletion :: SortDecls
              _lhsOself :: SortDecls
              _lhsOsAttrInh =
                  ({-# LINE 83 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 2816 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 82 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 2821 "src/InBound/CopyRules.hs" #-}
                   )
              _completion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   []
                   {-# LINE 2826 "src/InBound/CopyRules.hs" #-}
                   )
              _self =
                  []
              _lhsOcompletion =
                  ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
                   _completion
                   {-# LINE 2833 "src/InBound/CopyRules.hs" #-}
                   )
              _lhsOself =
                  _self
          in  ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself)))
-- Specification -----------------------------------------------
-- cata
sem_Specification :: Specification ->
                     T_Specification
sem_Specification (Specification _specModuleName _specNamespaceDecls _specSortDecls) =
    (sem_Specification_Specification _specModuleName (sem_NamespaceDecls _specNamespaceDecls) (sem_SortDecls _specSortDecls))
-- semantic domain
type T_Specification = ( Specification,(Map (SortName,AttrName) Type),(Map (SortName,AttrName) Type),Specification)
data Inh_Specification = Inh_Specification {}
data Syn_Specification = Syn_Specification {completion_Syn_Specification :: Specification,sAttrInh_Syn_Specification :: (Map (SortName,AttrName) Type),sAttrSyn_Syn_Specification :: (Map (SortName,AttrName) Type),self_Syn_Specification :: Specification}
wrap_Specification :: T_Specification ->
                      Inh_Specification ->
                      Syn_Specification
wrap_Specification sem (Inh_Specification) =
    (let ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself) = sem
     in  (Syn_Specification _lhsOcompletion _lhsOsAttrInh _lhsOsAttrSyn _lhsOself))
sem_Specification_Specification :: String ->
                                   T_NamespaceDecls ->
                                   T_SortDecls ->
                                   T_Specification
sem_Specification_Specification specModuleName_ specNamespaceDecls_ specSortDecls_ =
    (let _lhsOsAttrInh :: (Map (SortName,AttrName) Type)
         _lhsOsAttrSyn :: (Map (SortName,AttrName) Type)
         _lhsOcompletion :: Specification
         _lhsOself :: Specification
         _specSortDeclsOenvAttrInh :: (Map (SortName,AttrName) Type)
         _specSortDeclsOenvAttrSyn :: (Map (SortName,AttrName) Type)
         _specSortDeclsOnamespaces :: (Map NamespaceName MbSortName)
         _specSortDeclsOvarsorts :: (Map SortName [NamespaceName])
         _specNamespaceDeclsIcompletion :: NamespaceDecls
         _specNamespaceDeclsInamespaces :: (Map NamespaceName MbSortName)
         _specNamespaceDeclsIself :: NamespaceDecls
         _specSortDeclsIcompletion :: SortDecls
         _specSortDeclsIsAttrInh :: (Map (SortName,AttrName) Type)
         _specSortDeclsIsAttrSyn :: (Map (SortName,AttrName) Type)
         _specSortDeclsIself :: SortDecls
         _namespaces =
             ({-# LINE 32 "src/InBound/Environment.ag" #-}
              _specNamespaceDeclsInamespaces
              {-# LINE 2877 "src/InBound/CopyRules.hs" #-}
              )
         _varsorts =
             ({-# LINE 33 "src/InBound/Environment.ag" #-}
              M.fromListWith (++)
                [ (v,[k])
                | (k,Just v) <- M.toList _namespaces
                ]
              {-# LINE 2885 "src/InBound/CopyRules.hs" #-}
              )
         _envAttrSyn =
             ({-# LINE 118 "src/InBound/Environment.ag" #-}
              _specSortDeclsIsAttrSyn
              {-# LINE 2890 "src/InBound/CopyRules.hs" #-}
              )
         _envAttrInh =
             ({-# LINE 119 "src/InBound/Environment.ag" #-}
              _specSortDeclsIsAttrInh
              {-# LINE 2895 "src/InBound/CopyRules.hs" #-}
              )
         _lhsOsAttrInh =
             ({-# LINE 83 "src/InBound/Environment.ag" #-}
              _specSortDeclsIsAttrInh
              {-# LINE 2900 "src/InBound/CopyRules.hs" #-}
              )
         _lhsOsAttrSyn =
             ({-# LINE 82 "src/InBound/Environment.ag" #-}
              _specSortDeclsIsAttrSyn
              {-# LINE 2905 "src/InBound/CopyRules.hs" #-}
              )
         _completion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              Specification specModuleName_ _specNamespaceDeclsIcompletion _specSortDeclsIcompletion
              {-# LINE 2910 "src/InBound/CopyRules.hs" #-}
              )
         _self =
             Specification specModuleName_ _specNamespaceDeclsIself _specSortDeclsIself
         _lhsOcompletion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              _completion
              {-# LINE 2917 "src/InBound/CopyRules.hs" #-}
              )
         _lhsOself =
             _self
         _specSortDeclsOenvAttrInh =
             ({-# LINE 96 "src/InBound/Environment.ag" #-}
              _envAttrInh
              {-# LINE 2924 "src/InBound/CopyRules.hs" #-}
              )
         _specSortDeclsOenvAttrSyn =
             ({-# LINE 95 "src/InBound/Environment.ag" #-}
              _envAttrSyn
              {-# LINE 2929 "src/InBound/CopyRules.hs" #-}
              )
         _specSortDeclsOnamespaces =
             ({-# LINE 26 "src/InBound/Environment.ag" #-}
              _namespaces
              {-# LINE 2934 "src/InBound/CopyRules.hs" #-}
              )
         _specSortDeclsOvarsorts =
             ({-# LINE 28 "src/InBound/Environment.ag" #-}
              _varsorts
              {-# LINE 2939 "src/InBound/CopyRules.hs" #-}
              )
         ( _specNamespaceDeclsIcompletion,_specNamespaceDeclsInamespaces,_specNamespaceDeclsIself) =
             specNamespaceDecls_
         ( _specSortDeclsIcompletion,_specSortDeclsIsAttrInh,_specSortDeclsIsAttrSyn,_specSortDeclsIself) =
             specSortDecls_ _specSortDeclsOenvAttrInh _specSortDeclsOenvAttrSyn _specSortDeclsOnamespaces _specSortDeclsOvarsorts
     in  ( _lhsOcompletion,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself))
-- Type --------------------------------------------------------
-- cata
sem_Type :: Type ->
            T_Type
sem_Type (Context _namespace) =
    (sem_Type_Context _namespace)
-- semantic domain
type T_Type = ( Type,Type)
data Inh_Type = Inh_Type {}
data Syn_Type = Syn_Type {completion_Syn_Type :: Type,self_Syn_Type :: Type}
wrap_Type :: T_Type ->
             Inh_Type ->
             Syn_Type
wrap_Type sem (Inh_Type) =
    (let ( _lhsOcompletion,_lhsOself) = sem
     in  (Syn_Type _lhsOcompletion _lhsOself))
sem_Type_Context :: NamespaceName ->
                    T_Type
sem_Type_Context namespace_ =
    (let _lhsOcompletion :: Type
         _lhsOself :: Type
         _completion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              Context namespace_
              {-# LINE 2970 "src/InBound/CopyRules.hs" #-}
              )
         _self =
             Context namespace_
         _lhsOcompletion =
             ({-# LINE 21 "src/InBound/CopyRules.ag" #-}
              _completion
              {-# LINE 2977 "src/InBound/CopyRules.hs" #-}
              )
         _lhsOself =
             _self
     in  ( _lhsOcompletion,_lhsOself))