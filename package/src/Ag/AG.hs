

-- UUAGC 0.9.51 (src/Ag/AG.ag)
module Ag.AG where
import Ag.Syntax
{-# LINE 7 "src/Ag/Pretty.ag" #-}

import Control.Monad
import Data.List (intersperse)
import Data.Maybe

import Ag.Pretty.Common
import Ag.Syntax
{-# LINE 15 "src/Ag/AG.hs" #-}

{-# LINE 4 "src/Ag/Environment.ag" #-}

import Data.Maybe
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import Ag.Syntax
{-# LINE 26 "src/Ag/AG.hs" #-}

{-# LINE 10 "src/Ag/Syntax.ag" #-}

import InBound.Syntax.Core
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map
import qualified Data.Set
{-# LINE 35 "src/Ag/AG.hs" #-}
{-# LINE 16 "src/Ag/Pretty.ag" #-}

defaultValues :: Inh_Specification
defaultValues = (Inh_Specification {})

ppSpecification :: Specification -> Doc
ppSpecification spec =
  let sem = wrap_Specification (sem_Specification spec) defaultValues
  in pretty_Syn_Specification sem
{-# LINE 45 "src/Ag/AG.hs" #-}

{-# LINE 298 "src/Ag/Pretty.ag" #-}

nextUnique :: Int -> (Int,Int)
nextUnique u = (u+1,u)
{-# LINE 51 "src/Ag/AG.hs" #-}
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
                  ( Doc,LocEnvAttrType,LocEnvAttrType,AttrDecl)
data Inh_AttrDecl = Inh_AttrDecl {envSort_Inh_AttrDecl :: SortName}
data Syn_AttrDecl = Syn_AttrDecl {pretty_Syn_AttrDecl :: Doc,sAttrInh_Syn_AttrDecl :: LocEnvAttrType,sAttrSyn_Syn_AttrDecl :: LocEnvAttrType,self_Syn_AttrDecl :: AttrDecl}
wrap_AttrDecl :: T_AttrDecl ->
                 Inh_AttrDecl ->
                 Syn_AttrDecl
wrap_AttrDecl sem (Inh_AttrDecl _lhsIenvSort) =
    (let ( _lhsOpretty,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself) = sem _lhsIenvSort
     in  (Syn_AttrDecl _lhsOpretty _lhsOsAttrInh _lhsOsAttrSyn _lhsOself))
sem_AttrDecl_SynDecl :: AttrName ->
                        T_Type ->
                        T_AttrDecl
sem_AttrDecl_SynDecl attrName_ attrType_ =
    (\ _lhsIenvSort ->
         (let _lhsOsAttrSyn :: LocEnvAttrType
              _lhsOsAttrInh :: LocEnvAttrType
              _lhsOself :: AttrDecl
              _lhsOpretty :: Doc
              _attrTypeIpretty :: Doc
              _attrTypeIself :: Type
              _pretty =
                  ({-# LINE 142 "src/Ag/Pretty.ag" #-}
                   myFsep [ text "syn"
                          , text (fromAN attrName_)
                          , text "::"
                          , braces _attrTypeIpretty
                          ]
                   {-# LINE 89 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 42 "src/Ag/Environment.ag" #-}
                   M.singleton attrName_ _attrTypeIself
                   {-# LINE 94 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 20 "src/Ag/Environment.ag" #-}
                   M.empty
                   {-# LINE 99 "src/Ag/AG.hs" #-}
                   )
              _self =
                  SynDecl attrName_ _attrTypeIself
              _lhsOself =
                  _self
              _lhsOpretty =
                  ({-# LINE 128 "src/Ag/Pretty.ag" #-}
                   _pretty
                   {-# LINE 108 "src/Ag/AG.hs" #-}
                   )
              ( _attrTypeIpretty,_attrTypeIself) =
                  attrType_
          in  ( _lhsOpretty,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself)))
sem_AttrDecl_InhDecl :: AttrName ->
                        T_Type ->
                        T_AttrDecl
sem_AttrDecl_InhDecl attrName_ attrType_ =
    (\ _lhsIenvSort ->
         (let _lhsOsAttrInh :: LocEnvAttrType
              _lhsOsAttrSyn :: LocEnvAttrType
              _lhsOself :: AttrDecl
              _lhsOpretty :: Doc
              _attrTypeIpretty :: Doc
              _attrTypeIself :: Type
              _pretty =
                  ({-# LINE 149 "src/Ag/Pretty.ag" #-}
                   myFsep [ text "inh"
                          , text (fromAN attrName_)
                          , text "::"
                          , braces _attrTypeIpretty
                          ]
                   {-# LINE 131 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 43 "src/Ag/Environment.ag" #-}
                   M.singleton attrName_ _attrTypeIself
                   {-# LINE 136 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 19 "src/Ag/Environment.ag" #-}
                   M.empty
                   {-# LINE 141 "src/Ag/AG.hs" #-}
                   )
              _self =
                  InhDecl attrName_ _attrTypeIself
              _lhsOself =
                  _self
              _lhsOpretty =
                  ({-# LINE 128 "src/Ag/Pretty.ag" #-}
                   _pretty
                   {-# LINE 150 "src/Ag/AG.hs" #-}
                   )
              ( _attrTypeIpretty,_attrTypeIself) =
                  attrType_
          in  ( _lhsOpretty,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself)))
-- AttrDecls ---------------------------------------------------
-- cata
sem_AttrDecls :: AttrDecls ->
                 T_AttrDecls
sem_AttrDecls list =
    (Prelude.foldr sem_AttrDecls_Cons sem_AttrDecls_Nil (Prelude.map sem_AttrDecl list))
-- semantic domain
type T_AttrDecls = SortName ->
                   ( ([Doc]),LocEnvAttrType,LocEnvAttrType,AttrDecls)
data Inh_AttrDecls = Inh_AttrDecls {envSort_Inh_AttrDecls :: SortName}
data Syn_AttrDecls = Syn_AttrDecls {pretty_Syn_AttrDecls :: ([Doc]),sAttrInh_Syn_AttrDecls :: LocEnvAttrType,sAttrSyn_Syn_AttrDecls :: LocEnvAttrType,self_Syn_AttrDecls :: AttrDecls}
wrap_AttrDecls :: T_AttrDecls ->
                  Inh_AttrDecls ->
                  Syn_AttrDecls
wrap_AttrDecls sem (Inh_AttrDecls _lhsIenvSort) =
    (let ( _lhsOpretty,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself) = sem _lhsIenvSort
     in  (Syn_AttrDecls _lhsOpretty _lhsOsAttrInh _lhsOsAttrSyn _lhsOself))
sem_AttrDecls_Cons :: T_AttrDecl ->
                      T_AttrDecls ->
                      T_AttrDecls
sem_AttrDecls_Cons hd_ tl_ =
    (\ _lhsIenvSort ->
         (let _lhsOpretty :: ([Doc])
              _lhsOsAttrInh :: LocEnvAttrType
              _lhsOsAttrSyn :: LocEnvAttrType
              _lhsOself :: AttrDecls
              _hdOenvSort :: SortName
              _tlOenvSort :: SortName
              _hdIpretty :: Doc
              _hdIsAttrInh :: LocEnvAttrType
              _hdIsAttrSyn :: LocEnvAttrType
              _hdIself :: AttrDecl
              _tlIpretty :: ([Doc])
              _tlIsAttrInh :: LocEnvAttrType
              _tlIsAttrSyn :: LocEnvAttrType
              _tlIself :: AttrDecls
              _lhsOpretty =
                  ({-# LINE 129 "src/Ag/Pretty.ag" #-}
                   _hdIpretty : _tlIpretty
                   {-# LINE 194 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 20 "src/Ag/Environment.ag" #-}
                   (M.union _hdIsAttrInh _tlIsAttrInh)
                   {-# LINE 199 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 19 "src/Ag/Environment.ag" #-}
                   (M.union _hdIsAttrSyn _tlIsAttrSyn)
                   {-# LINE 204 "src/Ag/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 213 "src/Ag/AG.hs" #-}
                   )
              _tlOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 218 "src/Ag/AG.hs" #-}
                   )
              ( _hdIpretty,_hdIsAttrInh,_hdIsAttrSyn,_hdIself) =
                  hd_ _hdOenvSort
              ( _tlIpretty,_tlIsAttrInh,_tlIsAttrSyn,_tlIself) =
                  tl_ _tlOenvSort
          in  ( _lhsOpretty,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself)))
sem_AttrDecls_Nil :: T_AttrDecls
sem_AttrDecls_Nil =
    (\ _lhsIenvSort ->
         (let _lhsOpretty :: ([Doc])
              _lhsOsAttrInh :: LocEnvAttrType
              _lhsOsAttrSyn :: LocEnvAttrType
              _lhsOself :: AttrDecls
              _lhsOpretty =
                  ({-# LINE 129 "src/Ag/Pretty.ag" #-}
                   []
                   {-# LINE 235 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 20 "src/Ag/Environment.ag" #-}
                   M.empty
                   {-# LINE 240 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 19 "src/Ag/Environment.ag" #-}
                   M.empty
                   {-# LINE 245 "src/Ag/AG.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOpretty,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself)))
-- AttrDef -----------------------------------------------------
-- cata
sem_AttrDef :: AttrDef ->
               T_AttrDef
sem_AttrDef (AttrDef _attrDefRef _attrDefExpr) =
    (sem_AttrDef_AttrDef (sem_AttrRef _attrDefRef) (sem_Expr _attrDefExpr))
-- semantic domain
type T_AttrDef = EnvAttrType ->
                 EnvAttrType ->
                 SortName ->
                 LocEnvAttrType ->
                 LocEnvAttrType ->
                 ( Doc,AttrDef)
data Inh_AttrDef = Inh_AttrDef {envAttrInh_Inh_AttrDef :: EnvAttrType,envAttrSyn_Inh_AttrDef :: EnvAttrType,envSort_Inh_AttrDef :: SortName,locEnvAttrInh_Inh_AttrDef :: LocEnvAttrType,locEnvAttrSyn_Inh_AttrDef :: LocEnvAttrType}
data Syn_AttrDef = Syn_AttrDef {pretty_Syn_AttrDef :: Doc,self_Syn_AttrDef :: AttrDef}
wrap_AttrDef :: T_AttrDef ->
                Inh_AttrDef ->
                Syn_AttrDef
wrap_AttrDef sem (Inh_AttrDef _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn) =
    (let ( _lhsOpretty,_lhsOself) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn
     in  (Syn_AttrDef _lhsOpretty _lhsOself))
sem_AttrDef_AttrDef :: T_AttrRef ->
                       T_Expr ->
                       T_AttrDef
sem_AttrDef_AttrDef attrDefRef_ attrDefExpr_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn ->
         (let _lhsOpretty :: Doc
              _lhsOself :: AttrDef
              _attrDefExprOenvAttrInh :: EnvAttrType
              _attrDefExprOenvAttrSyn :: EnvAttrType
              _attrDefExprOenvSort :: SortName
              _attrDefRefIpretty :: Doc
              _attrDefRefIself :: AttrRef
              _attrDefExprIpretty :: Doc
              _attrDefExprIself :: Expr
              _lhsOpretty =
                  ({-# LINE 200 "src/Ag/Pretty.ag" #-}
                   _attrDefRefIpretty <+>
                   equals <+>
                   _attrDefExprIpretty
                   {-# LINE 296 "src/Ag/AG.hs" #-}
                   )
              _self =
                  AttrDef _attrDefRefIself _attrDefExprIself
              _lhsOself =
                  _self
              _attrDefExprOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 305 "src/Ag/AG.hs" #-}
                   )
              _attrDefExprOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 310 "src/Ag/AG.hs" #-}
                   )
              _attrDefExprOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 315 "src/Ag/AG.hs" #-}
                   )
              ( _attrDefRefIpretty,_attrDefRefIself) =
                  attrDefRef_
              ( _attrDefExprIpretty,_attrDefExprIself) =
                  attrDefExpr_ _attrDefExprOenvAttrInh _attrDefExprOenvAttrSyn _attrDefExprOenvSort
          in  ( _lhsOpretty,_lhsOself)))
-- AttrDefs ----------------------------------------------------
-- cata
sem_AttrDefs :: AttrDefs ->
                T_AttrDefs
sem_AttrDefs list =
    (Prelude.foldr sem_AttrDefs_Cons sem_AttrDefs_Nil (Prelude.map sem_AttrDef list))
-- semantic domain
type T_AttrDefs = EnvAttrType ->
                  EnvAttrType ->
                  SortName ->
                  LocEnvAttrType ->
                  LocEnvAttrType ->
                  ( ([Doc]),AttrDefs)
data Inh_AttrDefs = Inh_AttrDefs {envAttrInh_Inh_AttrDefs :: EnvAttrType,envAttrSyn_Inh_AttrDefs :: EnvAttrType,envSort_Inh_AttrDefs :: SortName,locEnvAttrInh_Inh_AttrDefs :: LocEnvAttrType,locEnvAttrSyn_Inh_AttrDefs :: LocEnvAttrType}
data Syn_AttrDefs = Syn_AttrDefs {pretty_Syn_AttrDefs :: ([Doc]),self_Syn_AttrDefs :: AttrDefs}
wrap_AttrDefs :: T_AttrDefs ->
                 Inh_AttrDefs ->
                 Syn_AttrDefs
wrap_AttrDefs sem (Inh_AttrDefs _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn) =
    (let ( _lhsOpretty,_lhsOself) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn
     in  (Syn_AttrDefs _lhsOpretty _lhsOself))
sem_AttrDefs_Cons :: T_AttrDef ->
                     T_AttrDefs ->
                     T_AttrDefs
sem_AttrDefs_Cons hd_ tl_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn ->
         (let _lhsOpretty :: ([Doc])
              _lhsOself :: AttrDefs
              _hdOenvAttrInh :: EnvAttrType
              _hdOenvAttrSyn :: EnvAttrType
              _hdOenvSort :: SortName
              _hdOlocEnvAttrInh :: LocEnvAttrType
              _hdOlocEnvAttrSyn :: LocEnvAttrType
              _tlOenvAttrInh :: EnvAttrType
              _tlOenvAttrSyn :: EnvAttrType
              _tlOenvSort :: SortName
              _tlOlocEnvAttrInh :: LocEnvAttrType
              _tlOlocEnvAttrSyn :: LocEnvAttrType
              _hdIpretty :: Doc
              _hdIself :: AttrDef
              _tlIpretty :: ([Doc])
              _tlIself :: AttrDefs
              _lhsOpretty =
                  ({-# LINE 175 "src/Ag/Pretty.ag" #-}
                   _hdIpretty : _tlIpretty
                   {-# LINE 371 "src/Ag/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 380 "src/Ag/AG.hs" #-}
                   )
              _hdOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 385 "src/Ag/AG.hs" #-}
                   )
              _hdOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 390 "src/Ag/AG.hs" #-}
                   )
              _hdOlocEnvAttrInh =
                  ({-# LINE 33 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 395 "src/Ag/AG.hs" #-}
                   )
              _hdOlocEnvAttrSyn =
                  ({-# LINE 32 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 400 "src/Ag/AG.hs" #-}
                   )
              _tlOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 405 "src/Ag/AG.hs" #-}
                   )
              _tlOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 410 "src/Ag/AG.hs" #-}
                   )
              _tlOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 415 "src/Ag/AG.hs" #-}
                   )
              _tlOlocEnvAttrInh =
                  ({-# LINE 33 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 420 "src/Ag/AG.hs" #-}
                   )
              _tlOlocEnvAttrSyn =
                  ({-# LINE 32 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 425 "src/Ag/AG.hs" #-}
                   )
              ( _hdIpretty,_hdIself) =
                  hd_ _hdOenvAttrInh _hdOenvAttrSyn _hdOenvSort _hdOlocEnvAttrInh _hdOlocEnvAttrSyn
              ( _tlIpretty,_tlIself) =
                  tl_ _tlOenvAttrInh _tlOenvAttrSyn _tlOenvSort _tlOlocEnvAttrInh _tlOlocEnvAttrSyn
          in  ( _lhsOpretty,_lhsOself)))
sem_AttrDefs_Nil :: T_AttrDefs
sem_AttrDefs_Nil =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn ->
         (let _lhsOpretty :: ([Doc])
              _lhsOself :: AttrDefs
              _lhsOpretty =
                  ({-# LINE 175 "src/Ag/Pretty.ag" #-}
                   []
                   {-# LINE 444 "src/Ag/AG.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOpretty,_lhsOself)))
-- AttrNameType ------------------------------------------------
-- cata
sem_AttrNameType :: AttrNameType ->
                    T_AttrNameType
sem_AttrNameType ( x1,x2) =
    (sem_AttrNameType_Tuple x1 (sem_Type x2))
-- semantic domain
type T_AttrNameType = SortName ->
                      Int ->
                      LocEnvAttrType ->
                      LocEnvAttrType ->
                      ( Int,Doc,Doc,Doc,Doc,AttrNameType)
data Inh_AttrNameType = Inh_AttrNameType {envSort_Inh_AttrNameType :: SortName,fresh_Inh_AttrNameType :: Int,locEnvAttrInh_Inh_AttrNameType :: LocEnvAttrType,locEnvAttrSyn_Inh_AttrNameType :: LocEnvAttrType}
data Syn_AttrNameType = Syn_AttrNameType {fresh_Syn_AttrNameType :: Int,prettyInhInit_Syn_AttrNameType :: Doc,prettyInhParam_Syn_AttrNameType :: Doc,prettySynAccess_Syn_AttrNameType :: Doc,prettyType_Syn_AttrNameType :: Doc,self_Syn_AttrNameType :: AttrNameType}
wrap_AttrNameType :: T_AttrNameType ->
                     Inh_AttrNameType ->
                     Syn_AttrNameType
wrap_AttrNameType sem (Inh_AttrNameType _lhsIenvSort _lhsIfresh _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn) =
    (let ( _lhsOfresh,_lhsOprettyInhInit,_lhsOprettyInhParam,_lhsOprettySynAccess,_lhsOprettyType,_lhsOself) = sem _lhsIenvSort _lhsIfresh _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn
     in  (Syn_AttrNameType _lhsOfresh _lhsOprettyInhInit _lhsOprettyInhParam _lhsOprettySynAccess _lhsOprettyType _lhsOself))
sem_AttrNameType_Tuple :: AttrName ->
                          T_Type ->
                          T_AttrNameType
sem_AttrNameType_Tuple x1_ x2_ =
    (\ _lhsIenvSort
       _lhsIfresh
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn ->
         (let _lhsOprettyType :: Doc
              _lhsOprettyInhParam :: Doc
              _lhsOprettyInhInit :: Doc
              _lhsOprettySynAccess :: Doc
              _lhsOfresh :: Int
              _inhVar' :: Int
              _lhsOself :: AttrNameType
              _x2Ipretty :: Doc
              _x2Iself :: Type
              _lhsOprettyType =
                  ({-# LINE 359 "src/Ag/Pretty.ag" #-}
                   _x2Ipretty
                   {-# LINE 491 "src/Ag/AG.hs" #-}
                   )
              _inhVar =
                  ({-# LINE 361 "src/Ag/Pretty.ag" #-}
                   'x':show _inhVar'
                   {-# LINE 496 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettyInhParam =
                  ({-# LINE 362 "src/Ag/Pretty.ag" #-}
                   text _inhVar
                   {-# LINE 501 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettyInhInit =
                  ({-# LINE 363 "src/Ag/Pretty.ag" #-}
                   text (fromAN x1_ ++ "_Inh_" ++ fromSN _lhsIenvSort) <+>
                   text "=" <+>
                   text _inhVar
                   {-# LINE 508 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettySynAccess =
                  ({-# LINE 366 "src/Ag/Pretty.ag" #-}
                   text (fromAN x1_ ++ "_Syn_" ++ fromSN _lhsIenvSort) <+>
                   text "syn"
                   {-# LINE 514 "src/Ag/AG.hs" #-}
                   )
              (_lhsOfresh,_inhVar') =
                  ({-# LINE 360 "src/Ag/Pretty.ag" #-}
                   let __cont = _lhsIfresh in seq __cont ( case nextUnique __cont of { (__cont, inhVar') -> (__cont,inhVar')} )
                   {-# LINE 519 "src/Ag/AG.hs" #-}
                   )
              _self =
                  (x1_,_x2Iself)
              _lhsOself =
                  _self
              ( _x2Ipretty,_x2Iself) =
                  x2_
          in  ( _lhsOfresh,_lhsOprettyInhInit,_lhsOprettyInhParam,_lhsOprettySynAccess,_lhsOprettyType,_lhsOself)))
-- AttrNameTypes -----------------------------------------------
-- cata
sem_AttrNameTypes :: AttrNameTypes ->
                     T_AttrNameTypes
sem_AttrNameTypes list =
    (Prelude.foldr sem_AttrNameTypes_Cons sem_AttrNameTypes_Nil (Prelude.map sem_AttrNameType list))
-- semantic domain
type T_AttrNameTypes = SortName ->
                       Int ->
                       LocEnvAttrType ->
                       LocEnvAttrType ->
                       ( ([Doc]),([Doc]),([Doc]),([Doc]),AttrNameTypes)
data Inh_AttrNameTypes = Inh_AttrNameTypes {envSort_Inh_AttrNameTypes :: SortName,fresh_Inh_AttrNameTypes :: Int,locEnvAttrInh_Inh_AttrNameTypes :: LocEnvAttrType,locEnvAttrSyn_Inh_AttrNameTypes :: LocEnvAttrType}
data Syn_AttrNameTypes = Syn_AttrNameTypes {prettyInhInit_Syn_AttrNameTypes :: ([Doc]),prettyInhParam_Syn_AttrNameTypes :: ([Doc]),prettySynAccess_Syn_AttrNameTypes :: ([Doc]),prettyType_Syn_AttrNameTypes :: ([Doc]),self_Syn_AttrNameTypes :: AttrNameTypes}
wrap_AttrNameTypes :: T_AttrNameTypes ->
                      Inh_AttrNameTypes ->
                      Syn_AttrNameTypes
wrap_AttrNameTypes sem (Inh_AttrNameTypes _lhsIenvSort _lhsIfresh _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn) =
    (let ( _lhsOprettyInhInit,_lhsOprettyInhParam,_lhsOprettySynAccess,_lhsOprettyType,_lhsOself) = sem _lhsIenvSort _lhsIfresh _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn
     in  (Syn_AttrNameTypes _lhsOprettyInhInit _lhsOprettyInhParam _lhsOprettySynAccess _lhsOprettyType _lhsOself))
sem_AttrNameTypes_Cons :: T_AttrNameType ->
                          T_AttrNameTypes ->
                          T_AttrNameTypes
sem_AttrNameTypes_Cons hd_ tl_ =
    (\ _lhsIenvSort
       _lhsIfresh
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn ->
         (let _lhsOprettyInhInit :: ([Doc])
              _lhsOprettyInhParam :: ([Doc])
              _lhsOprettySynAccess :: ([Doc])
              _lhsOprettyType :: ([Doc])
              _lhsOself :: AttrNameTypes
              _hdOenvSort :: SortName
              _hdOfresh :: Int
              _hdOlocEnvAttrInh :: LocEnvAttrType
              _hdOlocEnvAttrSyn :: LocEnvAttrType
              _tlOenvSort :: SortName
              _tlOfresh :: Int
              _tlOlocEnvAttrInh :: LocEnvAttrType
              _tlOlocEnvAttrSyn :: LocEnvAttrType
              _hdIfresh :: Int
              _hdIprettyInhInit :: Doc
              _hdIprettyInhParam :: Doc
              _hdIprettySynAccess :: Doc
              _hdIprettyType :: Doc
              _hdIself :: AttrNameType
              _tlIprettyInhInit :: ([Doc])
              _tlIprettyInhParam :: ([Doc])
              _tlIprettySynAccess :: ([Doc])
              _tlIprettyType :: ([Doc])
              _tlIself :: AttrNameTypes
              _lhsOprettyInhInit =
                  ({-# LINE 352 "src/Ag/Pretty.ag" #-}
                   _hdIprettyInhInit : _tlIprettyInhInit
                   {-# LINE 583 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettyInhParam =
                  ({-# LINE 350 "src/Ag/Pretty.ag" #-}
                   _hdIprettyInhParam : _tlIprettyInhParam
                   {-# LINE 588 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettySynAccess =
                  ({-# LINE 355 "src/Ag/Pretty.ag" #-}
                   _hdIprettySynAccess : _tlIprettySynAccess
                   {-# LINE 593 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettyType =
                  ({-# LINE 347 "src/Ag/Pretty.ag" #-}
                   _hdIprettyType : _tlIprettyType
                   {-# LINE 598 "src/Ag/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 607 "src/Ag/AG.hs" #-}
                   )
              _hdOfresh =
                  ({-# LINE 290 "src/Ag/Pretty.ag" #-}
                   _lhsIfresh
                   {-# LINE 612 "src/Ag/AG.hs" #-}
                   )
              _hdOlocEnvAttrInh =
                  ({-# LINE 33 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 617 "src/Ag/AG.hs" #-}
                   )
              _hdOlocEnvAttrSyn =
                  ({-# LINE 32 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 622 "src/Ag/AG.hs" #-}
                   )
              _tlOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 627 "src/Ag/AG.hs" #-}
                   )
              _tlOfresh =
                  ({-# LINE 288 "src/Ag/Pretty.ag" #-}
                   _hdIfresh
                   {-# LINE 632 "src/Ag/AG.hs" #-}
                   )
              _tlOlocEnvAttrInh =
                  ({-# LINE 33 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 637 "src/Ag/AG.hs" #-}
                   )
              _tlOlocEnvAttrSyn =
                  ({-# LINE 32 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 642 "src/Ag/AG.hs" #-}
                   )
              ( _hdIfresh,_hdIprettyInhInit,_hdIprettyInhParam,_hdIprettySynAccess,_hdIprettyType,_hdIself) =
                  hd_ _hdOenvSort _hdOfresh _hdOlocEnvAttrInh _hdOlocEnvAttrSyn
              ( _tlIprettyInhInit,_tlIprettyInhParam,_tlIprettySynAccess,_tlIprettyType,_tlIself) =
                  tl_ _tlOenvSort _tlOfresh _tlOlocEnvAttrInh _tlOlocEnvAttrSyn
          in  ( _lhsOprettyInhInit,_lhsOprettyInhParam,_lhsOprettySynAccess,_lhsOprettyType,_lhsOself)))
sem_AttrNameTypes_Nil :: T_AttrNameTypes
sem_AttrNameTypes_Nil =
    (\ _lhsIenvSort
       _lhsIfresh
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn ->
         (let _lhsOprettyInhInit :: ([Doc])
              _lhsOprettyInhParam :: ([Doc])
              _lhsOprettySynAccess :: ([Doc])
              _lhsOprettyType :: ([Doc])
              _lhsOself :: AttrNameTypes
              _lhsOprettyInhInit =
                  ({-# LINE 352 "src/Ag/Pretty.ag" #-}
                   []
                   {-# LINE 663 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettyInhParam =
                  ({-# LINE 350 "src/Ag/Pretty.ag" #-}
                   []
                   {-# LINE 668 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettySynAccess =
                  ({-# LINE 355 "src/Ag/Pretty.ag" #-}
                   []
                   {-# LINE 673 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettyType =
                  ({-# LINE 347 "src/Ag/Pretty.ag" #-}
                   []
                   {-# LINE 678 "src/Ag/AG.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOprettyInhInit,_lhsOprettyInhParam,_lhsOprettySynAccess,_lhsOprettyType,_lhsOself)))
-- AttrRef -----------------------------------------------------
-- cata
sem_AttrRef :: AttrRef ->
               T_AttrRef
sem_AttrRef (AttrRef _nodeLabel _attrLabel) =
    (sem_AttrRef_AttrRef (sem_NodeLabel _nodeLabel) _attrLabel)
-- semantic domain
type T_AttrRef = ( Doc,AttrRef)
data Inh_AttrRef = Inh_AttrRef {}
data Syn_AttrRef = Syn_AttrRef {pretty_Syn_AttrRef :: Doc,self_Syn_AttrRef :: AttrRef}
wrap_AttrRef :: T_AttrRef ->
                Inh_AttrRef ->
                Syn_AttrRef
wrap_AttrRef sem (Inh_AttrRef) =
    (let ( _lhsOpretty,_lhsOself) = sem
     in  (Syn_AttrRef _lhsOpretty _lhsOself))
sem_AttrRef_AttrRef :: T_NodeLabel ->
                       AttrName ->
                       T_AttrRef
sem_AttrRef_AttrRef nodeLabel_ attrLabel_ =
    (let _lhsOpretty :: Doc
         _lhsOself :: AttrRef
         _nodeLabelIpretty :: Doc
         _nodeLabelIself :: NodeLabel
         _lhsOpretty =
             ({-# LINE 206 "src/Ag/Pretty.ag" #-}
              _nodeLabelIpretty <> dot <> text (fromAN attrLabel_)
              {-# LINE 712 "src/Ag/AG.hs" #-}
              )
         _self =
             AttrRef _nodeLabelIself attrLabel_
         _lhsOself =
             _self
         ( _nodeLabelIpretty,_nodeLabelIself) =
             nodeLabel_
     in  ( _lhsOpretty,_lhsOself))
-- CtorDecl ----------------------------------------------------
-- cata
sem_CtorDecl :: CtorDecl ->
                T_CtorDecl
sem_CtorDecl (CtorDecl _ctorName _ctorFields _ctorLocAttrDecl _ctorAttrDefs) =
    (sem_CtorDecl_CtorDecl _ctorName (sem_CtorFieldDecls _ctorFields) (sem_LocAttrDecls _ctorLocAttrDecl) (sem_AttrDefs _ctorAttrDefs))
-- semantic domain
type T_CtorDecl = EnvAttrType ->
                  EnvAttrType ->
                  SortName ->
                  LocEnvAttrType ->
                  LocEnvAttrType ->
                  ( Doc,Doc,EnvAttrType,EnvAttrType,CtorDecl)
data Inh_CtorDecl = Inh_CtorDecl {envAttrInh_Inh_CtorDecl :: EnvAttrType,envAttrSyn_Inh_CtorDecl :: EnvAttrType,envSort_Inh_CtorDecl :: SortName,locEnvAttrInh_Inh_CtorDecl :: LocEnvAttrType,locEnvAttrSyn_Inh_CtorDecl :: LocEnvAttrType}
data Syn_CtorDecl = Syn_CtorDecl {pretty_Syn_CtorDecl :: Doc,prettySem_Syn_CtorDecl :: Doc,sAttrInh_Syn_CtorDecl :: EnvAttrType,sAttrSyn_Syn_CtorDecl :: EnvAttrType,self_Syn_CtorDecl :: CtorDecl}
wrap_CtorDecl :: T_CtorDecl ->
                 Inh_CtorDecl ->
                 Syn_CtorDecl
wrap_CtorDecl sem (Inh_CtorDecl _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn) =
    (let ( _lhsOpretty,_lhsOprettySem,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn
     in  (Syn_CtorDecl _lhsOpretty _lhsOprettySem _lhsOsAttrInh _lhsOsAttrSyn _lhsOself))
sem_CtorDecl_CtorDecl :: CtorName ->
                         T_CtorFieldDecls ->
                         T_LocAttrDecls ->
                         T_AttrDefs ->
                         T_CtorDecl
sem_CtorDecl_CtorDecl ctorName_ ctorFields_ ctorLocAttrDecl_ ctorAttrDefs_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn ->
         (let _lhsOpretty :: Doc
              _lhsOsAttrInh :: EnvAttrType
              _lhsOsAttrSyn :: EnvAttrType
              _lhsOself :: CtorDecl
              _lhsOprettySem :: Doc
              _ctorFieldsOenvSort :: SortName
              _ctorFieldsOlocEnvAttrInh :: LocEnvAttrType
              _ctorFieldsOlocEnvAttrSyn :: LocEnvAttrType
              _ctorAttrDefsOenvAttrInh :: EnvAttrType
              _ctorAttrDefsOenvAttrSyn :: EnvAttrType
              _ctorAttrDefsOenvSort :: SortName
              _ctorAttrDefsOlocEnvAttrInh :: LocEnvAttrType
              _ctorAttrDefsOlocEnvAttrSyn :: LocEnvAttrType
              _ctorFieldsIhPretty :: Doc
              _ctorFieldsIlPretty :: ([Doc])
              _ctorFieldsIself :: CtorFieldDecls
              _ctorFieldsIvPretty :: Doc
              _ctorLocAttrDeclIself :: LocAttrDecls
              _ctorAttrDefsIpretty :: ([Doc])
              _ctorAttrDefsIself :: AttrDefs
              _pretty =
                  ({-# LINE 107 "src/Ag/Pretty.ag" #-}
                   text (fromCN ctorName_) <+>
                     indent 4 (myVcat _ctorFieldsIlPretty)
                   {-# LINE 777 "src/Ag/AG.hs" #-}
                   )
              _hasAttrDefs =
                  ({-# LINE 191 "src/Ag/Pretty.ag" #-}
                   True
                   {-# LINE 782 "src/Ag/AG.hs" #-}
                   )
              _prettySem =
                  ({-# LINE 192 "src/Ag/Pretty.ag" #-}
                   if _hasAttrDefs
                     then text "|" <+> text (fromCN ctorName_) <$>
                          indent 4 (myVcat _ctorAttrDefsIpretty)
                     else empty
                   {-# LINE 790 "src/Ag/AG.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 74 "src/Ag/Pretty.ag" #-}
                   _pretty
                   {-# LINE 795 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 26 "src/Ag/Environment.ag" #-}
                   M.empty
                   {-# LINE 800 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 25 "src/Ag/Environment.ag" #-}
                   M.empty
                   {-# LINE 805 "src/Ag/AG.hs" #-}
                   )
              _self =
                  CtorDecl ctorName_ _ctorFieldsIself _ctorLocAttrDeclIself _ctorAttrDefsIself
              _lhsOself =
                  _self
              _lhsOprettySem =
                  ({-# LINE 172 "src/Ag/Pretty.ag" #-}
                   _prettySem
                   {-# LINE 814 "src/Ag/AG.hs" #-}
                   )
              _ctorFieldsOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 819 "src/Ag/AG.hs" #-}
                   )
              _ctorFieldsOlocEnvAttrInh =
                  ({-# LINE 33 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 824 "src/Ag/AG.hs" #-}
                   )
              _ctorFieldsOlocEnvAttrSyn =
                  ({-# LINE 32 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 829 "src/Ag/AG.hs" #-}
                   )
              _ctorAttrDefsOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 834 "src/Ag/AG.hs" #-}
                   )
              _ctorAttrDefsOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 839 "src/Ag/AG.hs" #-}
                   )
              _ctorAttrDefsOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 844 "src/Ag/AG.hs" #-}
                   )
              _ctorAttrDefsOlocEnvAttrInh =
                  ({-# LINE 33 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 849 "src/Ag/AG.hs" #-}
                   )
              _ctorAttrDefsOlocEnvAttrSyn =
                  ({-# LINE 32 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 854 "src/Ag/AG.hs" #-}
                   )
              ( _ctorFieldsIhPretty,_ctorFieldsIlPretty,_ctorFieldsIself,_ctorFieldsIvPretty) =
                  ctorFields_ _ctorFieldsOenvSort _ctorFieldsOlocEnvAttrInh _ctorFieldsOlocEnvAttrSyn
              ( _ctorLocAttrDeclIself) =
                  ctorLocAttrDecl_
              ( _ctorAttrDefsIpretty,_ctorAttrDefsIself) =
                  ctorAttrDefs_ _ctorAttrDefsOenvAttrInh _ctorAttrDefsOenvAttrSyn _ctorAttrDefsOenvSort _ctorAttrDefsOlocEnvAttrInh _ctorAttrDefsOlocEnvAttrSyn
          in  ( _lhsOpretty,_lhsOprettySem,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself)))
-- CtorDecls ---------------------------------------------------
-- cata
sem_CtorDecls :: CtorDecls ->
                 T_CtorDecls
sem_CtorDecls list =
    (Prelude.foldr sem_CtorDecls_Cons sem_CtorDecls_Nil (Prelude.map sem_CtorDecl list))
-- semantic domain
type T_CtorDecls = EnvAttrType ->
                   EnvAttrType ->
                   SortName ->
                   LocEnvAttrType ->
                   LocEnvAttrType ->
                   ( Doc,([Doc]),([Doc]),EnvAttrType,EnvAttrType,CtorDecls,Doc)
data Inh_CtorDecls = Inh_CtorDecls {envAttrInh_Inh_CtorDecls :: EnvAttrType,envAttrSyn_Inh_CtorDecls :: EnvAttrType,envSort_Inh_CtorDecls :: SortName,locEnvAttrInh_Inh_CtorDecls :: LocEnvAttrType,locEnvAttrSyn_Inh_CtorDecls :: LocEnvAttrType}
data Syn_CtorDecls = Syn_CtorDecls {hPretty_Syn_CtorDecls :: Doc,lPretty_Syn_CtorDecls :: ([Doc]),prettySem_Syn_CtorDecls :: ([Doc]),sAttrInh_Syn_CtorDecls :: EnvAttrType,sAttrSyn_Syn_CtorDecls :: EnvAttrType,self_Syn_CtorDecls :: CtorDecls,vPretty_Syn_CtorDecls :: Doc}
wrap_CtorDecls :: T_CtorDecls ->
                  Inh_CtorDecls ->
                  Syn_CtorDecls
wrap_CtorDecls sem (Inh_CtorDecls _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn) =
    (let ( _lhsOhPretty,_lhsOlPretty,_lhsOprettySem,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself,_lhsOvPretty) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn
     in  (Syn_CtorDecls _lhsOhPretty _lhsOlPretty _lhsOprettySem _lhsOsAttrInh _lhsOsAttrSyn _lhsOself _lhsOvPretty))
sem_CtorDecls_Cons :: T_CtorDecl ->
                      T_CtorDecls ->
                      T_CtorDecls
sem_CtorDecls_Cons hd_ tl_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn ->
         (let _lhsOlPretty :: ([Doc])
              _lhsOprettySem :: ([Doc])
              _lhsOsAttrInh :: EnvAttrType
              _lhsOsAttrSyn :: EnvAttrType
              _lhsOself :: CtorDecls
              _lhsOhPretty :: Doc
              _lhsOvPretty :: Doc
              _hdOenvAttrInh :: EnvAttrType
              _hdOenvAttrSyn :: EnvAttrType
              _hdOenvSort :: SortName
              _hdOlocEnvAttrInh :: LocEnvAttrType
              _hdOlocEnvAttrSyn :: LocEnvAttrType
              _tlOenvAttrInh :: EnvAttrType
              _tlOenvAttrSyn :: EnvAttrType
              _tlOenvSort :: SortName
              _tlOlocEnvAttrInh :: LocEnvAttrType
              _tlOlocEnvAttrSyn :: LocEnvAttrType
              _hdIpretty :: Doc
              _hdIprettySem :: Doc
              _hdIsAttrInh :: EnvAttrType
              _hdIsAttrSyn :: EnvAttrType
              _hdIself :: CtorDecl
              _tlIhPretty :: Doc
              _tlIlPretty :: ([Doc])
              _tlIprettySem :: ([Doc])
              _tlIsAttrInh :: EnvAttrType
              _tlIsAttrSyn :: EnvAttrType
              _tlIself :: CtorDecls
              _tlIvPretty :: Doc
              _lPretty =
                  ({-# LINE 87 "src/Ag/Pretty.ag" #-}
                   _hdIpretty : _tlIlPretty
                   {-# LINE 925 "src/Ag/AG.hs" #-}
                   )
              _vPretty =
                  ({-# LINE 88 "src/Ag/Pretty.ag" #-}
                   vsep _lPretty
                   {-# LINE 930 "src/Ag/AG.hs" #-}
                   )
              _hPretty =
                  ({-# LINE 89 "src/Ag/Pretty.ag" #-}
                   hsep _lPretty
                   {-# LINE 935 "src/Ag/AG.hs" #-}
                   )
              _lhsOlPretty =
                  ({-# LINE 77 "src/Ag/Pretty.ag" #-}
                   _lPretty
                   {-# LINE 940 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettySem =
                  ({-# LINE 173 "src/Ag/Pretty.ag" #-}
                   _hdIprettySem : _tlIprettySem
                   {-# LINE 945 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 26 "src/Ag/Environment.ag" #-}
                   (M.union _hdIsAttrInh _tlIsAttrInh)
                   {-# LINE 950 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 25 "src/Ag/Environment.ag" #-}
                   (M.union _hdIsAttrSyn _tlIsAttrSyn)
                   {-# LINE 955 "src/Ag/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOhPretty =
                  ({-# LINE 79 "src/Ag/Pretty.ag" #-}
                   _hPretty
                   {-# LINE 964 "src/Ag/AG.hs" #-}
                   )
              _lhsOvPretty =
                  ({-# LINE 78 "src/Ag/Pretty.ag" #-}
                   _vPretty
                   {-# LINE 969 "src/Ag/AG.hs" #-}
                   )
              _hdOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 974 "src/Ag/AG.hs" #-}
                   )
              _hdOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 979 "src/Ag/AG.hs" #-}
                   )
              _hdOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 984 "src/Ag/AG.hs" #-}
                   )
              _hdOlocEnvAttrInh =
                  ({-# LINE 33 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 989 "src/Ag/AG.hs" #-}
                   )
              _hdOlocEnvAttrSyn =
                  ({-# LINE 32 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 994 "src/Ag/AG.hs" #-}
                   )
              _tlOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 999 "src/Ag/AG.hs" #-}
                   )
              _tlOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1004 "src/Ag/AG.hs" #-}
                   )
              _tlOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1009 "src/Ag/AG.hs" #-}
                   )
              _tlOlocEnvAttrInh =
                  ({-# LINE 33 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 1014 "src/Ag/AG.hs" #-}
                   )
              _tlOlocEnvAttrSyn =
                  ({-# LINE 32 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 1019 "src/Ag/AG.hs" #-}
                   )
              ( _hdIpretty,_hdIprettySem,_hdIsAttrInh,_hdIsAttrSyn,_hdIself) =
                  hd_ _hdOenvAttrInh _hdOenvAttrSyn _hdOenvSort _hdOlocEnvAttrInh _hdOlocEnvAttrSyn
              ( _tlIhPretty,_tlIlPretty,_tlIprettySem,_tlIsAttrInh,_tlIsAttrSyn,_tlIself,_tlIvPretty) =
                  tl_ _tlOenvAttrInh _tlOenvAttrSyn _tlOenvSort _tlOlocEnvAttrInh _tlOlocEnvAttrSyn
          in  ( _lhsOhPretty,_lhsOlPretty,_lhsOprettySem,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself,_lhsOvPretty)))
sem_CtorDecls_Nil :: T_CtorDecls
sem_CtorDecls_Nil =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn ->
         (let _lhsOlPretty :: ([Doc])
              _lhsOprettySem :: ([Doc])
              _lhsOsAttrInh :: EnvAttrType
              _lhsOsAttrSyn :: EnvAttrType
              _lhsOself :: CtorDecls
              _lhsOhPretty :: Doc
              _lhsOvPretty :: Doc
              _lPretty =
                  ({-# LINE 83 "src/Ag/Pretty.ag" #-}
                   []
                   {-# LINE 1043 "src/Ag/AG.hs" #-}
                   )
              _vPretty =
                  ({-# LINE 84 "src/Ag/Pretty.ag" #-}
                   empty
                   {-# LINE 1048 "src/Ag/AG.hs" #-}
                   )
              _hPretty =
                  ({-# LINE 85 "src/Ag/Pretty.ag" #-}
                   empty
                   {-# LINE 1053 "src/Ag/AG.hs" #-}
                   )
              _lhsOlPretty =
                  ({-# LINE 77 "src/Ag/Pretty.ag" #-}
                   _lPretty
                   {-# LINE 1058 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettySem =
                  ({-# LINE 173 "src/Ag/Pretty.ag" #-}
                   []
                   {-# LINE 1063 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 26 "src/Ag/Environment.ag" #-}
                   M.empty
                   {-# LINE 1068 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 25 "src/Ag/Environment.ag" #-}
                   M.empty
                   {-# LINE 1073 "src/Ag/AG.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOhPretty =
                  ({-# LINE 79 "src/Ag/Pretty.ag" #-}
                   _hPretty
                   {-# LINE 1082 "src/Ag/AG.hs" #-}
                   )
              _lhsOvPretty =
                  ({-# LINE 78 "src/Ag/Pretty.ag" #-}
                   _vPretty
                   {-# LINE 1087 "src/Ag/AG.hs" #-}
                   )
          in  ( _lhsOhPretty,_lhsOlPretty,_lhsOprettySem,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself,_lhsOvPretty)))
-- CtorFieldDecl -----------------------------------------------
-- cata
sem_CtorFieldDecl :: CtorFieldDecl ->
                     T_CtorFieldDecl
sem_CtorFieldDecl (CFSubtree _ctorFieldName _ctorFieldType) =
    (sem_CtorFieldDecl_CFSubtree _ctorFieldName _ctorFieldType)
sem_CtorFieldDecl (CFTerminal _ctorFieldName _ctorFieldType) =
    (sem_CtorFieldDecl_CFTerminal _ctorFieldName _ctorFieldType)
-- semantic domain
type T_CtorFieldDecl = SortName ->
                       LocEnvAttrType ->
                       LocEnvAttrType ->
                       ( Doc,CtorFieldDecl)
data Inh_CtorFieldDecl = Inh_CtorFieldDecl {envSort_Inh_CtorFieldDecl :: SortName,locEnvAttrInh_Inh_CtorFieldDecl :: LocEnvAttrType,locEnvAttrSyn_Inh_CtorFieldDecl :: LocEnvAttrType}
data Syn_CtorFieldDecl = Syn_CtorFieldDecl {pretty_Syn_CtorFieldDecl :: Doc,self_Syn_CtorFieldDecl :: CtorFieldDecl}
wrap_CtorFieldDecl :: T_CtorFieldDecl ->
                      Inh_CtorFieldDecl ->
                      Syn_CtorFieldDecl
wrap_CtorFieldDecl sem (Inh_CtorFieldDecl _lhsIenvSort _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn) =
    (let ( _lhsOpretty,_lhsOself) = sem _lhsIenvSort _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn
     in  (Syn_CtorFieldDecl _lhsOpretty _lhsOself))
sem_CtorFieldDecl_CFSubtree :: FieldName ->
                               SortName ->
                               T_CtorFieldDecl
sem_CtorFieldDecl_CFSubtree ctorFieldName_ ctorFieldType_ =
    (\ _lhsIenvSort
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn ->
         (let _lhsOpretty :: Doc
              _lhsOself :: CtorFieldDecl
              _pretty =
                  ({-# LINE 113 "src/Ag/Pretty.ag" #-}
                   mySep [ text (fromFN ctorFieldName_)
                         , text "::"
                         , text (fromSN ctorFieldType_)
                         ]
                   {-# LINE 1126 "src/Ag/AG.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 74 "src/Ag/Pretty.ag" #-}
                   _pretty
                   {-# LINE 1131 "src/Ag/AG.hs" #-}
                   )
              _self =
                  CFSubtree ctorFieldName_ ctorFieldType_
              _lhsOself =
                  _self
          in  ( _lhsOpretty,_lhsOself)))
sem_CtorFieldDecl_CFTerminal :: FieldName ->
                                String ->
                                T_CtorFieldDecl
sem_CtorFieldDecl_CFTerminal ctorFieldName_ ctorFieldType_ =
    (\ _lhsIenvSort
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn ->
         (let _lhsOpretty :: Doc
              _lhsOself :: CtorFieldDecl
              _pretty =
                  ({-# LINE 119 "src/Ag/Pretty.ag" #-}
                   mySep [ text (fromFN ctorFieldName_)
                         , text "::"
                         , text ctorFieldType_
                         ]
                   {-# LINE 1153 "src/Ag/AG.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 74 "src/Ag/Pretty.ag" #-}
                   _pretty
                   {-# LINE 1158 "src/Ag/AG.hs" #-}
                   )
              _self =
                  CFTerminal ctorFieldName_ ctorFieldType_
              _lhsOself =
                  _self
          in  ( _lhsOpretty,_lhsOself)))
-- CtorFieldDecls ----------------------------------------------
-- cata
sem_CtorFieldDecls :: CtorFieldDecls ->
                      T_CtorFieldDecls
sem_CtorFieldDecls list =
    (Prelude.foldr sem_CtorFieldDecls_Cons sem_CtorFieldDecls_Nil (Prelude.map sem_CtorFieldDecl list))
-- semantic domain
type T_CtorFieldDecls = SortName ->
                        LocEnvAttrType ->
                        LocEnvAttrType ->
                        ( Doc,([Doc]),CtorFieldDecls,Doc)
data Inh_CtorFieldDecls = Inh_CtorFieldDecls {envSort_Inh_CtorFieldDecls :: SortName,locEnvAttrInh_Inh_CtorFieldDecls :: LocEnvAttrType,locEnvAttrSyn_Inh_CtorFieldDecls :: LocEnvAttrType}
data Syn_CtorFieldDecls = Syn_CtorFieldDecls {hPretty_Syn_CtorFieldDecls :: Doc,lPretty_Syn_CtorFieldDecls :: ([Doc]),self_Syn_CtorFieldDecls :: CtorFieldDecls,vPretty_Syn_CtorFieldDecls :: Doc}
wrap_CtorFieldDecls :: T_CtorFieldDecls ->
                       Inh_CtorFieldDecls ->
                       Syn_CtorFieldDecls
wrap_CtorFieldDecls sem (Inh_CtorFieldDecls _lhsIenvSort _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn) =
    (let ( _lhsOhPretty,_lhsOlPretty,_lhsOself,_lhsOvPretty) = sem _lhsIenvSort _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn
     in  (Syn_CtorFieldDecls _lhsOhPretty _lhsOlPretty _lhsOself _lhsOvPretty))
sem_CtorFieldDecls_Cons :: T_CtorFieldDecl ->
                           T_CtorFieldDecls ->
                           T_CtorFieldDecls
sem_CtorFieldDecls_Cons hd_ tl_ =
    (\ _lhsIenvSort
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn ->
         (let _lhsOlPretty :: ([Doc])
              _lhsOself :: CtorFieldDecls
              _lhsOhPretty :: Doc
              _lhsOvPretty :: Doc
              _hdOenvSort :: SortName
              _hdOlocEnvAttrInh :: LocEnvAttrType
              _hdOlocEnvAttrSyn :: LocEnvAttrType
              _tlOenvSort :: SortName
              _tlOlocEnvAttrInh :: LocEnvAttrType
              _tlOlocEnvAttrSyn :: LocEnvAttrType
              _hdIpretty :: Doc
              _hdIself :: CtorFieldDecl
              _tlIhPretty :: Doc
              _tlIlPretty :: ([Doc])
              _tlIself :: CtorFieldDecls
              _tlIvPretty :: Doc
              _lPretty =
                  ({-# LINE 87 "src/Ag/Pretty.ag" #-}
                   _hdIpretty : _tlIlPretty
                   {-# LINE 1210 "src/Ag/AG.hs" #-}
                   )
              _vPretty =
                  ({-# LINE 88 "src/Ag/Pretty.ag" #-}
                   vsep _lPretty
                   {-# LINE 1215 "src/Ag/AG.hs" #-}
                   )
              _hPretty =
                  ({-# LINE 89 "src/Ag/Pretty.ag" #-}
                   hsep _lPretty
                   {-# LINE 1220 "src/Ag/AG.hs" #-}
                   )
              _lhsOlPretty =
                  ({-# LINE 77 "src/Ag/Pretty.ag" #-}
                   _lPretty
                   {-# LINE 1225 "src/Ag/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOhPretty =
                  ({-# LINE 79 "src/Ag/Pretty.ag" #-}
                   _hPretty
                   {-# LINE 1234 "src/Ag/AG.hs" #-}
                   )
              _lhsOvPretty =
                  ({-# LINE 78 "src/Ag/Pretty.ag" #-}
                   _vPretty
                   {-# LINE 1239 "src/Ag/AG.hs" #-}
                   )
              _hdOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1244 "src/Ag/AG.hs" #-}
                   )
              _hdOlocEnvAttrInh =
                  ({-# LINE 33 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 1249 "src/Ag/AG.hs" #-}
                   )
              _hdOlocEnvAttrSyn =
                  ({-# LINE 32 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 1254 "src/Ag/AG.hs" #-}
                   )
              _tlOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1259 "src/Ag/AG.hs" #-}
                   )
              _tlOlocEnvAttrInh =
                  ({-# LINE 33 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 1264 "src/Ag/AG.hs" #-}
                   )
              _tlOlocEnvAttrSyn =
                  ({-# LINE 32 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 1269 "src/Ag/AG.hs" #-}
                   )
              ( _hdIpretty,_hdIself) =
                  hd_ _hdOenvSort _hdOlocEnvAttrInh _hdOlocEnvAttrSyn
              ( _tlIhPretty,_tlIlPretty,_tlIself,_tlIvPretty) =
                  tl_ _tlOenvSort _tlOlocEnvAttrInh _tlOlocEnvAttrSyn
          in  ( _lhsOhPretty,_lhsOlPretty,_lhsOself,_lhsOvPretty)))
sem_CtorFieldDecls_Nil :: T_CtorFieldDecls
sem_CtorFieldDecls_Nil =
    (\ _lhsIenvSort
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn ->
         (let _lhsOlPretty :: ([Doc])
              _lhsOself :: CtorFieldDecls
              _lhsOhPretty :: Doc
              _lhsOvPretty :: Doc
              _lPretty =
                  ({-# LINE 83 "src/Ag/Pretty.ag" #-}
                   []
                   {-# LINE 1288 "src/Ag/AG.hs" #-}
                   )
              _vPretty =
                  ({-# LINE 84 "src/Ag/Pretty.ag" #-}
                   empty
                   {-# LINE 1293 "src/Ag/AG.hs" #-}
                   )
              _hPretty =
                  ({-# LINE 85 "src/Ag/Pretty.ag" #-}
                   empty
                   {-# LINE 1298 "src/Ag/AG.hs" #-}
                   )
              _lhsOlPretty =
                  ({-# LINE 77 "src/Ag/Pretty.ag" #-}
                   _lPretty
                   {-# LINE 1303 "src/Ag/AG.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOhPretty =
                  ({-# LINE 79 "src/Ag/Pretty.ag" #-}
                   _hPretty
                   {-# LINE 1312 "src/Ag/AG.hs" #-}
                   )
              _lhsOvPretty =
                  ({-# LINE 78 "src/Ag/Pretty.ag" #-}
                   _vPretty
                   {-# LINE 1317 "src/Ag/AG.hs" #-}
                   )
          in  ( _lhsOhPretty,_lhsOlPretty,_lhsOself,_lhsOvPretty)))
-- EnvAttrType -------------------------------------------------
-- cata
sem_EnvAttrType :: EnvAttrType ->
                   T_EnvAttrType
sem_EnvAttrType m =
    (Data.Map.foldrWithKey sem_EnvAttrType_Entry sem_EnvAttrType_Nil (Data.Map.map sem_Type m))
-- semantic domain
type T_EnvAttrType = ( EnvAttrType)
data Inh_EnvAttrType = Inh_EnvAttrType {}
data Syn_EnvAttrType = Syn_EnvAttrType {self_Syn_EnvAttrType :: EnvAttrType}
wrap_EnvAttrType :: T_EnvAttrType ->
                    Inh_EnvAttrType ->
                    Syn_EnvAttrType
wrap_EnvAttrType sem (Inh_EnvAttrType) =
    (let ( _lhsOself) = sem
     in  (Syn_EnvAttrType _lhsOself))
sem_EnvAttrType_Entry :: ((SortName,AttrName)) ->
                         T_Type ->
                         T_EnvAttrType ->
                         T_EnvAttrType
sem_EnvAttrType_Entry key_ val_ tl_ =
    (let _lhsOself :: EnvAttrType
         _valIpretty :: Doc
         _valIself :: Type
         _tlIself :: EnvAttrType
         _self =
             Data.Map.insert key_ _valIself _tlIself
         _lhsOself =
             _self
         ( _valIpretty,_valIself) =
             val_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_EnvAttrType_Nil :: T_EnvAttrType
sem_EnvAttrType_Nil =
    (let _lhsOself :: EnvAttrType
         _self =
             Data.Map.empty
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Expr --------------------------------------------------------
-- cata
sem_Expr :: Expr ->
            T_Expr
sem_Expr (ExprAttrRef _attrRefName) =
    (sem_Expr_ExprAttrRef (sem_AttrRef _attrRefName))
sem_Expr (ExprField _fn) =
    (sem_Expr_ExprField _fn)
sem_Expr (ExprFresh _ctx) =
    (sem_Expr_ExprFresh (sem_Expr _ctx))
sem_Expr (ExprNil) =
    (sem_Expr_ExprNil)
sem_Expr (ExprCons _tail _head) =
    (sem_Expr_ExprCons (sem_Expr _tail) (sem_Expr _head))
sem_Expr (ExprSetEmpty) =
    (sem_Expr_ExprSetEmpty)
sem_Expr (ExprSetSingleton _elem) =
    (sem_Expr_ExprSetSingleton (sem_Expr _elem))
sem_Expr (ExprSetInsert _elem _elems) =
    (sem_Expr_ExprSetInsert (sem_Expr _elem) (sem_Expr _elems))
sem_Expr (ExprSetUnion _left _right) =
    (sem_Expr_ExprSetUnion (sem_Expr _left) (sem_Expr _right))
sem_Expr (ExprSetUnions _elems) =
    (sem_Expr_ExprSetUnions (sem_Exprs _elems))
sem_Expr (ExprSetDifference _left _right) =
    (sem_Expr_ExprSetDifference (sem_Expr _left) (sem_Expr _right))
sem_Expr (ExprMapEmpty) =
    (sem_Expr_ExprMapEmpty)
sem_Expr (ExprMapSingleton _key _value) =
    (sem_Expr_ExprMapSingleton (sem_Expr _key) (sem_Expr _value))
sem_Expr (ExprMapInsert _key _value _mapping) =
    (sem_Expr_ExprMapInsert (sem_Expr _key) (sem_Expr _value) (sem_Expr _mapping))
sem_Expr (ExprMapLookup _def _key _mapping) =
    (sem_Expr_ExprMapLookup (sem_Expr _def) (sem_Expr _key) (sem_Expr _mapping))
sem_Expr (ExprCtor _ctorName _fields) =
    (sem_Expr_ExprCtor _ctorName (sem_Exprs _fields))
-- semantic domain
type T_Expr = EnvAttrType ->
              EnvAttrType ->
              SortName ->
              ( Doc,Expr)
data Inh_Expr = Inh_Expr {envAttrInh_Inh_Expr :: EnvAttrType,envAttrSyn_Inh_Expr :: EnvAttrType,envSort_Inh_Expr :: SortName}
data Syn_Expr = Syn_Expr {pretty_Syn_Expr :: Doc,self_Syn_Expr :: Expr}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort) =
    (let ( _lhsOpretty,_lhsOself) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort
     in  (Syn_Expr _lhsOpretty _lhsOself))
sem_Expr_ExprAttrRef :: T_AttrRef ->
                        T_Expr
sem_Expr_ExprAttrRef attrRefName_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOpretty :: Doc
              _lhsOself :: Expr
              _attrRefNameIpretty :: Doc
              _attrRefNameIself :: AttrRef
              _lhsOpretty =
                  ({-# LINE 218 "src/Ag/Pretty.ag" #-}
                   text "@" <> _attrRefNameIpretty
                   {-# LINE 1424 "src/Ag/AG.hs" #-}
                   )
              _self =
                  ExprAttrRef _attrRefNameIself
              _lhsOself =
                  _self
              ( _attrRefNameIpretty,_attrRefNameIself) =
                  attrRefName_
          in  ( _lhsOpretty,_lhsOself)))
sem_Expr_ExprField :: FieldName ->
                      T_Expr
sem_Expr_ExprField fn_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOpretty :: Doc
              _lhsOself :: Expr
              _lhsOpretty =
                  ({-# LINE 220 "src/Ag/Pretty.ag" #-}
                   text "@" <> text (fromFN fn_)
                   {-# LINE 1444 "src/Ag/AG.hs" #-}
                   )
              _self =
                  ExprField fn_
              _lhsOself =
                  _self
          in  ( _lhsOpretty,_lhsOself)))
sem_Expr_ExprFresh :: T_Expr ->
                      T_Expr
sem_Expr_ExprFresh ctx_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOpretty :: Doc
              _lhsOself :: Expr
              _ctxOenvAttrInh :: EnvAttrType
              _ctxOenvAttrSyn :: EnvAttrType
              _ctxOenvSort :: SortName
              _ctxIpretty :: Doc
              _ctxIself :: Expr
              _lhsOpretty =
                  ({-# LINE 223 "src/Ag/Pretty.ag" #-}
                   text "fresh" <+> parens _ctxIpretty
                   {-# LINE 1467 "src/Ag/AG.hs" #-}
                   )
              _self =
                  ExprFresh _ctxIself
              _lhsOself =
                  _self
              _ctxOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1476 "src/Ag/AG.hs" #-}
                   )
              _ctxOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1481 "src/Ag/AG.hs" #-}
                   )
              _ctxOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1486 "src/Ag/AG.hs" #-}
                   )
              ( _ctxIpretty,_ctxIself) =
                  ctx_ _ctxOenvAttrInh _ctxOenvAttrSyn _ctxOenvSort
          in  ( _lhsOpretty,_lhsOself)))
sem_Expr_ExprNil :: T_Expr
sem_Expr_ExprNil =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOpretty :: Doc
              _lhsOself :: Expr
              _lhsOpretty =
                  ({-# LINE 226 "src/Ag/Pretty.ag" #-}
                   text "[]"
                   {-# LINE 1501 "src/Ag/AG.hs" #-}
                   )
              _self =
                  ExprNil
              _lhsOself =
                  _self
          in  ( _lhsOpretty,_lhsOself)))
sem_Expr_ExprCons :: T_Expr ->
                     T_Expr ->
                     T_Expr
sem_Expr_ExprCons tail_ head_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOpretty :: Doc
              _lhsOself :: Expr
              _tailOenvAttrInh :: EnvAttrType
              _tailOenvAttrSyn :: EnvAttrType
              _tailOenvSort :: SortName
              _headOenvAttrInh :: EnvAttrType
              _headOenvAttrSyn :: EnvAttrType
              _headOenvSort :: SortName
              _tailIpretty :: Doc
              _tailIself :: Expr
              _headIpretty :: Doc
              _headIself :: Expr
              _lhsOpretty =
                  ({-# LINE 228 "src/Ag/Pretty.ag" #-}
                   parens _headIpretty <+>
                   colon <+>
                   parens _tailIpretty
                   {-# LINE 1532 "src/Ag/AG.hs" #-}
                   )
              _self =
                  ExprCons _tailIself _headIself
              _lhsOself =
                  _self
              _tailOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1541 "src/Ag/AG.hs" #-}
                   )
              _tailOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1546 "src/Ag/AG.hs" #-}
                   )
              _tailOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1551 "src/Ag/AG.hs" #-}
                   )
              _headOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1556 "src/Ag/AG.hs" #-}
                   )
              _headOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1561 "src/Ag/AG.hs" #-}
                   )
              _headOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1566 "src/Ag/AG.hs" #-}
                   )
              ( _tailIpretty,_tailIself) =
                  tail_ _tailOenvAttrInh _tailOenvAttrSyn _tailOenvSort
              ( _headIpretty,_headIself) =
                  head_ _headOenvAttrInh _headOenvAttrSyn _headOenvSort
          in  ( _lhsOpretty,_lhsOself)))
sem_Expr_ExprSetEmpty :: T_Expr
sem_Expr_ExprSetEmpty =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOpretty :: Doc
              _lhsOself :: Expr
              _lhsOpretty =
                  ({-# LINE 233 "src/Ag/Pretty.ag" #-}
                   text "Data.Set.empty"
                   {-# LINE 1583 "src/Ag/AG.hs" #-}
                   )
              _self =
                  ExprSetEmpty
              _lhsOself =
                  _self
          in  ( _lhsOpretty,_lhsOself)))
sem_Expr_ExprSetSingleton :: T_Expr ->
                             T_Expr
sem_Expr_ExprSetSingleton elem_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOpretty :: Doc
              _lhsOself :: Expr
              _elemOenvAttrInh :: EnvAttrType
              _elemOenvAttrSyn :: EnvAttrType
              _elemOenvSort :: SortName
              _elemIpretty :: Doc
              _elemIself :: Expr
              _lhsOpretty =
                  ({-# LINE 235 "src/Ag/Pretty.ag" #-}
                   text "Data.Set.singleton" <+>
                   parens _elemIpretty
                   {-# LINE 1607 "src/Ag/AG.hs" #-}
                   )
              _self =
                  ExprSetSingleton _elemIself
              _lhsOself =
                  _self
              _elemOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1616 "src/Ag/AG.hs" #-}
                   )
              _elemOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1621 "src/Ag/AG.hs" #-}
                   )
              _elemOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1626 "src/Ag/AG.hs" #-}
                   )
              ( _elemIpretty,_elemIself) =
                  elem_ _elemOenvAttrInh _elemOenvAttrSyn _elemOenvSort
          in  ( _lhsOpretty,_lhsOself)))
sem_Expr_ExprSetInsert :: T_Expr ->
                          T_Expr ->
                          T_Expr
sem_Expr_ExprSetInsert elem_ elems_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOpretty :: Doc
              _lhsOself :: Expr
              _elemOenvAttrInh :: EnvAttrType
              _elemOenvAttrSyn :: EnvAttrType
              _elemOenvSort :: SortName
              _elemsOenvAttrInh :: EnvAttrType
              _elemsOenvAttrSyn :: EnvAttrType
              _elemsOenvSort :: SortName
              _elemIpretty :: Doc
              _elemIself :: Expr
              _elemsIpretty :: Doc
              _elemsIself :: Expr
              _lhsOpretty =
                  ({-# LINE 238 "src/Ag/Pretty.ag" #-}
                   text "Data.Set.insert" <+>
                   parens _elemIpretty <+>
                   parens _elemsIpretty
                   {-# LINE 1655 "src/Ag/AG.hs" #-}
                   )
              _self =
                  ExprSetInsert _elemIself _elemsIself
              _lhsOself =
                  _self
              _elemOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1664 "src/Ag/AG.hs" #-}
                   )
              _elemOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1669 "src/Ag/AG.hs" #-}
                   )
              _elemOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1674 "src/Ag/AG.hs" #-}
                   )
              _elemsOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1679 "src/Ag/AG.hs" #-}
                   )
              _elemsOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1684 "src/Ag/AG.hs" #-}
                   )
              _elemsOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1689 "src/Ag/AG.hs" #-}
                   )
              ( _elemIpretty,_elemIself) =
                  elem_ _elemOenvAttrInh _elemOenvAttrSyn _elemOenvSort
              ( _elemsIpretty,_elemsIself) =
                  elems_ _elemsOenvAttrInh _elemsOenvAttrSyn _elemsOenvSort
          in  ( _lhsOpretty,_lhsOself)))
sem_Expr_ExprSetUnion :: T_Expr ->
                         T_Expr ->
                         T_Expr
sem_Expr_ExprSetUnion left_ right_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOpretty :: Doc
              _lhsOself :: Expr
              _leftOenvAttrInh :: EnvAttrType
              _leftOenvAttrSyn :: EnvAttrType
              _leftOenvSort :: SortName
              _rightOenvAttrInh :: EnvAttrType
              _rightOenvAttrSyn :: EnvAttrType
              _rightOenvSort :: SortName
              _leftIpretty :: Doc
              _leftIself :: Expr
              _rightIpretty :: Doc
              _rightIself :: Expr
              _lhsOpretty =
                  ({-# LINE 242 "src/Ag/Pretty.ag" #-}
                   text "Data.Set.union" <+>
                   parens _leftIpretty <+>
                   parens _rightIpretty
                   {-# LINE 1720 "src/Ag/AG.hs" #-}
                   )
              _self =
                  ExprSetUnion _leftIself _rightIself
              _lhsOself =
                  _self
              _leftOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1729 "src/Ag/AG.hs" #-}
                   )
              _leftOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1734 "src/Ag/AG.hs" #-}
                   )
              _leftOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1739 "src/Ag/AG.hs" #-}
                   )
              _rightOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1744 "src/Ag/AG.hs" #-}
                   )
              _rightOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1749 "src/Ag/AG.hs" #-}
                   )
              _rightOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1754 "src/Ag/AG.hs" #-}
                   )
              ( _leftIpretty,_leftIself) =
                  left_ _leftOenvAttrInh _leftOenvAttrSyn _leftOenvSort
              ( _rightIpretty,_rightIself) =
                  right_ _rightOenvAttrInh _rightOenvAttrSyn _rightOenvSort
          in  ( _lhsOpretty,_lhsOself)))
sem_Expr_ExprSetUnions :: T_Exprs ->
                          T_Expr
sem_Expr_ExprSetUnions elems_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOpretty :: Doc
              _lhsOself :: Expr
              _elemsOenvAttrInh :: EnvAttrType
              _elemsOenvAttrSyn :: EnvAttrType
              _elemsOenvSort :: SortName
              _elemsIhPretty :: Doc
              _elemsIlPretty :: ([Doc])
              _elemsIself :: Exprs
              _elemsIvPretty :: Doc
              _lhsOpretty =
                  ({-# LINE 250 "src/Ag/Pretty.ag" #-}
                   text "Data.Set.unions" <+>
                   list _elemsIlPretty
                   {-# LINE 1780 "src/Ag/AG.hs" #-}
                   )
              _self =
                  ExprSetUnions _elemsIself
              _lhsOself =
                  _self
              _elemsOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1789 "src/Ag/AG.hs" #-}
                   )
              _elemsOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1794 "src/Ag/AG.hs" #-}
                   )
              _elemsOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1799 "src/Ag/AG.hs" #-}
                   )
              ( _elemsIhPretty,_elemsIlPretty,_elemsIself,_elemsIvPretty) =
                  elems_ _elemsOenvAttrInh _elemsOenvAttrSyn _elemsOenvSort
          in  ( _lhsOpretty,_lhsOself)))
sem_Expr_ExprSetDifference :: T_Expr ->
                              T_Expr ->
                              T_Expr
sem_Expr_ExprSetDifference left_ right_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOpretty :: Doc
              _lhsOself :: Expr
              _leftOenvAttrInh :: EnvAttrType
              _leftOenvAttrSyn :: EnvAttrType
              _leftOenvSort :: SortName
              _rightOenvAttrInh :: EnvAttrType
              _rightOenvAttrSyn :: EnvAttrType
              _rightOenvSort :: SortName
              _leftIpretty :: Doc
              _leftIself :: Expr
              _rightIpretty :: Doc
              _rightIself :: Expr
              _lhsOpretty =
                  ({-# LINE 246 "src/Ag/Pretty.ag" #-}
                   text "Data.Set.difference" <+>
                   parens _leftIpretty <+>
                   parens _rightIpretty
                   {-# LINE 1828 "src/Ag/AG.hs" #-}
                   )
              _self =
                  ExprSetDifference _leftIself _rightIself
              _lhsOself =
                  _self
              _leftOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1837 "src/Ag/AG.hs" #-}
                   )
              _leftOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1842 "src/Ag/AG.hs" #-}
                   )
              _leftOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1847 "src/Ag/AG.hs" #-}
                   )
              _rightOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1852 "src/Ag/AG.hs" #-}
                   )
              _rightOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1857 "src/Ag/AG.hs" #-}
                   )
              _rightOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1862 "src/Ag/AG.hs" #-}
                   )
              ( _leftIpretty,_leftIself) =
                  left_ _leftOenvAttrInh _leftOenvAttrSyn _leftOenvSort
              ( _rightIpretty,_rightIself) =
                  right_ _rightOenvAttrInh _rightOenvAttrSyn _rightOenvSort
          in  ( _lhsOpretty,_lhsOself)))
sem_Expr_ExprMapEmpty :: T_Expr
sem_Expr_ExprMapEmpty =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOpretty :: Doc
              _lhsOself :: Expr
              _lhsOpretty =
                  ({-# LINE 254 "src/Ag/Pretty.ag" #-}
                   text "Data.Map.empty"
                   {-# LINE 1879 "src/Ag/AG.hs" #-}
                   )
              _self =
                  ExprMapEmpty
              _lhsOself =
                  _self
          in  ( _lhsOpretty,_lhsOself)))
sem_Expr_ExprMapSingleton :: T_Expr ->
                             T_Expr ->
                             T_Expr
sem_Expr_ExprMapSingleton key_ value_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOpretty :: Doc
              _lhsOself :: Expr
              _keyOenvAttrInh :: EnvAttrType
              _keyOenvAttrSyn :: EnvAttrType
              _keyOenvSort :: SortName
              _valueOenvAttrInh :: EnvAttrType
              _valueOenvAttrSyn :: EnvAttrType
              _valueOenvSort :: SortName
              _keyIpretty :: Doc
              _keyIself :: Expr
              _valueIpretty :: Doc
              _valueIself :: Expr
              _lhsOpretty =
                  ({-# LINE 256 "src/Ag/Pretty.ag" #-}
                   text "Data.Map.singleton" <+>
                   parens _keyIpretty <+>
                   parens _valueIpretty
                   {-# LINE 1910 "src/Ag/AG.hs" #-}
                   )
              _self =
                  ExprMapSingleton _keyIself _valueIself
              _lhsOself =
                  _self
              _keyOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1919 "src/Ag/AG.hs" #-}
                   )
              _keyOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1924 "src/Ag/AG.hs" #-}
                   )
              _keyOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1929 "src/Ag/AG.hs" #-}
                   )
              _valueOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1934 "src/Ag/AG.hs" #-}
                   )
              _valueOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1939 "src/Ag/AG.hs" #-}
                   )
              _valueOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1944 "src/Ag/AG.hs" #-}
                   )
              ( _keyIpretty,_keyIself) =
                  key_ _keyOenvAttrInh _keyOenvAttrSyn _keyOenvSort
              ( _valueIpretty,_valueIself) =
                  value_ _valueOenvAttrInh _valueOenvAttrSyn _valueOenvSort
          in  ( _lhsOpretty,_lhsOself)))
sem_Expr_ExprMapInsert :: T_Expr ->
                          T_Expr ->
                          T_Expr ->
                          T_Expr
sem_Expr_ExprMapInsert key_ value_ mapping_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOpretty :: Doc
              _lhsOself :: Expr
              _keyOenvAttrInh :: EnvAttrType
              _keyOenvAttrSyn :: EnvAttrType
              _keyOenvSort :: SortName
              _valueOenvAttrInh :: EnvAttrType
              _valueOenvAttrSyn :: EnvAttrType
              _valueOenvSort :: SortName
              _mappingOenvAttrInh :: EnvAttrType
              _mappingOenvAttrSyn :: EnvAttrType
              _mappingOenvSort :: SortName
              _keyIpretty :: Doc
              _keyIself :: Expr
              _valueIpretty :: Doc
              _valueIself :: Expr
              _mappingIpretty :: Doc
              _mappingIself :: Expr
              _lhsOpretty =
                  ({-# LINE 260 "src/Ag/Pretty.ag" #-}
                   text "Data.Map.insert" <+>
                   parens _keyIpretty <+>
                   parens _valueIpretty <+>
                   parens _mappingIpretty
                   {-# LINE 1982 "src/Ag/AG.hs" #-}
                   )
              _self =
                  ExprMapInsert _keyIself _valueIself _mappingIself
              _lhsOself =
                  _self
              _keyOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1991 "src/Ag/AG.hs" #-}
                   )
              _keyOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1996 "src/Ag/AG.hs" #-}
                   )
              _keyOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2001 "src/Ag/AG.hs" #-}
                   )
              _valueOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2006 "src/Ag/AG.hs" #-}
                   )
              _valueOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2011 "src/Ag/AG.hs" #-}
                   )
              _valueOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2016 "src/Ag/AG.hs" #-}
                   )
              _mappingOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2021 "src/Ag/AG.hs" #-}
                   )
              _mappingOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2026 "src/Ag/AG.hs" #-}
                   )
              _mappingOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2031 "src/Ag/AG.hs" #-}
                   )
              ( _keyIpretty,_keyIself) =
                  key_ _keyOenvAttrInh _keyOenvAttrSyn _keyOenvSort
              ( _valueIpretty,_valueIself) =
                  value_ _valueOenvAttrInh _valueOenvAttrSyn _valueOenvSort
              ( _mappingIpretty,_mappingIself) =
                  mapping_ _mappingOenvAttrInh _mappingOenvAttrSyn _mappingOenvSort
          in  ( _lhsOpretty,_lhsOself)))
sem_Expr_ExprMapLookup :: T_Expr ->
                          T_Expr ->
                          T_Expr ->
                          T_Expr
sem_Expr_ExprMapLookup def_ key_ mapping_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOpretty :: Doc
              _lhsOself :: Expr
              _defOenvAttrInh :: EnvAttrType
              _defOenvAttrSyn :: EnvAttrType
              _defOenvSort :: SortName
              _keyOenvAttrInh :: EnvAttrType
              _keyOenvAttrSyn :: EnvAttrType
              _keyOenvSort :: SortName
              _mappingOenvAttrInh :: EnvAttrType
              _mappingOenvAttrSyn :: EnvAttrType
              _mappingOenvSort :: SortName
              _defIpretty :: Doc
              _defIself :: Expr
              _keyIpretty :: Doc
              _keyIself :: Expr
              _mappingIpretty :: Doc
              _mappingIself :: Expr
              _lhsOpretty =
                  ({-# LINE 265 "src/Ag/Pretty.ag" #-}
                   text "Data.Map.findWithDefault" <+>
                   parens _defIpretty <+>
                   parens _keyIpretty <+>
                   parens _mappingIpretty
                   {-# LINE 2071 "src/Ag/AG.hs" #-}
                   )
              _self =
                  ExprMapLookup _defIself _keyIself _mappingIself
              _lhsOself =
                  _self
              _defOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2080 "src/Ag/AG.hs" #-}
                   )
              _defOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2085 "src/Ag/AG.hs" #-}
                   )
              _defOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2090 "src/Ag/AG.hs" #-}
                   )
              _keyOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2095 "src/Ag/AG.hs" #-}
                   )
              _keyOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2100 "src/Ag/AG.hs" #-}
                   )
              _keyOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2105 "src/Ag/AG.hs" #-}
                   )
              _mappingOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2110 "src/Ag/AG.hs" #-}
                   )
              _mappingOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2115 "src/Ag/AG.hs" #-}
                   )
              _mappingOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2120 "src/Ag/AG.hs" #-}
                   )
              ( _defIpretty,_defIself) =
                  def_ _defOenvAttrInh _defOenvAttrSyn _defOenvSort
              ( _keyIpretty,_keyIself) =
                  key_ _keyOenvAttrInh _keyOenvAttrSyn _keyOenvSort
              ( _mappingIpretty,_mappingIself) =
                  mapping_ _mappingOenvAttrInh _mappingOenvAttrSyn _mappingOenvSort
          in  ( _lhsOpretty,_lhsOself)))
sem_Expr_ExprCtor :: CtorName ->
                     T_Exprs ->
                     T_Expr
sem_Expr_ExprCtor ctorName_ fields_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOpretty :: Doc
              _lhsOself :: Expr
              _fieldsOenvAttrInh :: EnvAttrType
              _fieldsOenvAttrSyn :: EnvAttrType
              _fieldsOenvSort :: SortName
              _fieldsIhPretty :: Doc
              _fieldsIlPretty :: ([Doc])
              _fieldsIself :: Exprs
              _fieldsIvPretty :: Doc
              _lhsOpretty =
                  ({-# LINE 271 "src/Ag/Pretty.ag" #-}
                   text (fromCN ctorName_) <+>
                   hsep (map parens _fieldsIlPretty)
                   {-# LINE 2149 "src/Ag/AG.hs" #-}
                   )
              _self =
                  ExprCtor ctorName_ _fieldsIself
              _lhsOself =
                  _self
              _fieldsOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2158 "src/Ag/AG.hs" #-}
                   )
              _fieldsOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2163 "src/Ag/AG.hs" #-}
                   )
              _fieldsOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2168 "src/Ag/AG.hs" #-}
                   )
              ( _fieldsIhPretty,_fieldsIlPretty,_fieldsIself,_fieldsIvPretty) =
                  fields_ _fieldsOenvAttrInh _fieldsOenvAttrSyn _fieldsOenvSort
          in  ( _lhsOpretty,_lhsOself)))
-- Exprs -------------------------------------------------------
-- cata
sem_Exprs :: Exprs ->
             T_Exprs
sem_Exprs list =
    (Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list))
-- semantic domain
type T_Exprs = EnvAttrType ->
               EnvAttrType ->
               SortName ->
               ( Doc,([Doc]),Exprs,Doc)
data Inh_Exprs = Inh_Exprs {envAttrInh_Inh_Exprs :: EnvAttrType,envAttrSyn_Inh_Exprs :: EnvAttrType,envSort_Inh_Exprs :: SortName}
data Syn_Exprs = Syn_Exprs {hPretty_Syn_Exprs :: Doc,lPretty_Syn_Exprs :: ([Doc]),self_Syn_Exprs :: Exprs,vPretty_Syn_Exprs :: Doc}
wrap_Exprs :: T_Exprs ->
              Inh_Exprs ->
              Syn_Exprs
wrap_Exprs sem (Inh_Exprs _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort) =
    (let ( _lhsOhPretty,_lhsOlPretty,_lhsOself,_lhsOvPretty) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort
     in  (Syn_Exprs _lhsOhPretty _lhsOlPretty _lhsOself _lhsOvPretty))
sem_Exprs_Cons :: T_Expr ->
                  T_Exprs ->
                  T_Exprs
sem_Exprs_Cons hd_ tl_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOlPretty :: ([Doc])
              _lhsOself :: Exprs
              _lhsOhPretty :: Doc
              _lhsOvPretty :: Doc
              _hdOenvAttrInh :: EnvAttrType
              _hdOenvAttrSyn :: EnvAttrType
              _hdOenvSort :: SortName
              _tlOenvAttrInh :: EnvAttrType
              _tlOenvAttrSyn :: EnvAttrType
              _tlOenvSort :: SortName
              _hdIpretty :: Doc
              _hdIself :: Expr
              _tlIhPretty :: Doc
              _tlIlPretty :: ([Doc])
              _tlIself :: Exprs
              _tlIvPretty :: Doc
              _lPretty =
                  ({-# LINE 87 "src/Ag/Pretty.ag" #-}
                   _hdIpretty : _tlIlPretty
                   {-# LINE 2218 "src/Ag/AG.hs" #-}
                   )
              _vPretty =
                  ({-# LINE 88 "src/Ag/Pretty.ag" #-}
                   vsep _lPretty
                   {-# LINE 2223 "src/Ag/AG.hs" #-}
                   )
              _hPretty =
                  ({-# LINE 89 "src/Ag/Pretty.ag" #-}
                   hsep _lPretty
                   {-# LINE 2228 "src/Ag/AG.hs" #-}
                   )
              _lhsOlPretty =
                  ({-# LINE 77 "src/Ag/Pretty.ag" #-}
                   _lPretty
                   {-# LINE 2233 "src/Ag/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOhPretty =
                  ({-# LINE 79 "src/Ag/Pretty.ag" #-}
                   _hPretty
                   {-# LINE 2242 "src/Ag/AG.hs" #-}
                   )
              _lhsOvPretty =
                  ({-# LINE 78 "src/Ag/Pretty.ag" #-}
                   _vPretty
                   {-# LINE 2247 "src/Ag/AG.hs" #-}
                   )
              _hdOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2252 "src/Ag/AG.hs" #-}
                   )
              _hdOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2257 "src/Ag/AG.hs" #-}
                   )
              _hdOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2262 "src/Ag/AG.hs" #-}
                   )
              _tlOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2267 "src/Ag/AG.hs" #-}
                   )
              _tlOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2272 "src/Ag/AG.hs" #-}
                   )
              _tlOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2277 "src/Ag/AG.hs" #-}
                   )
              ( _hdIpretty,_hdIself) =
                  hd_ _hdOenvAttrInh _hdOenvAttrSyn _hdOenvSort
              ( _tlIhPretty,_tlIlPretty,_tlIself,_tlIvPretty) =
                  tl_ _tlOenvAttrInh _tlOenvAttrSyn _tlOenvSort
          in  ( _lhsOhPretty,_lhsOlPretty,_lhsOself,_lhsOvPretty)))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort ->
         (let _lhsOlPretty :: ([Doc])
              _lhsOself :: Exprs
              _lhsOhPretty :: Doc
              _lhsOvPretty :: Doc
              _lPretty =
                  ({-# LINE 83 "src/Ag/Pretty.ag" #-}
                   []
                   {-# LINE 2296 "src/Ag/AG.hs" #-}
                   )
              _vPretty =
                  ({-# LINE 84 "src/Ag/Pretty.ag" #-}
                   empty
                   {-# LINE 2301 "src/Ag/AG.hs" #-}
                   )
              _hPretty =
                  ({-# LINE 85 "src/Ag/Pretty.ag" #-}
                   empty
                   {-# LINE 2306 "src/Ag/AG.hs" #-}
                   )
              _lhsOlPretty =
                  ({-# LINE 77 "src/Ag/Pretty.ag" #-}
                   _lPretty
                   {-# LINE 2311 "src/Ag/AG.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOhPretty =
                  ({-# LINE 79 "src/Ag/Pretty.ag" #-}
                   _hPretty
                   {-# LINE 2320 "src/Ag/AG.hs" #-}
                   )
              _lhsOvPretty =
                  ({-# LINE 78 "src/Ag/Pretty.ag" #-}
                   _vPretty
                   {-# LINE 2325 "src/Ag/AG.hs" #-}
                   )
          in  ( _lhsOhPretty,_lhsOlPretty,_lhsOself,_lhsOvPretty)))
-- LocAttrDecl -------------------------------------------------
-- cata
sem_LocAttrDecl :: LocAttrDecl ->
                   T_LocAttrDecl
sem_LocAttrDecl (LocAttrDecl _attrName _attrType) =
    (sem_LocAttrDecl_LocAttrDecl _attrName (sem_Type _attrType))
-- semantic domain
type T_LocAttrDecl = ( LocAttrDecl)
data Inh_LocAttrDecl = Inh_LocAttrDecl {}
data Syn_LocAttrDecl = Syn_LocAttrDecl {self_Syn_LocAttrDecl :: LocAttrDecl}
wrap_LocAttrDecl :: T_LocAttrDecl ->
                    Inh_LocAttrDecl ->
                    Syn_LocAttrDecl
wrap_LocAttrDecl sem (Inh_LocAttrDecl) =
    (let ( _lhsOself) = sem
     in  (Syn_LocAttrDecl _lhsOself))
sem_LocAttrDecl_LocAttrDecl :: AttrName ->
                               T_Type ->
                               T_LocAttrDecl
sem_LocAttrDecl_LocAttrDecl attrName_ attrType_ =
    (let _lhsOself :: LocAttrDecl
         _attrTypeIpretty :: Doc
         _attrTypeIself :: Type
         _self =
             LocAttrDecl attrName_ _attrTypeIself
         _lhsOself =
             _self
         ( _attrTypeIpretty,_attrTypeIself) =
             attrType_
     in  ( _lhsOself))
-- LocAttrDecls ------------------------------------------------
-- cata
sem_LocAttrDecls :: LocAttrDecls ->
                    T_LocAttrDecls
sem_LocAttrDecls list =
    (Prelude.foldr sem_LocAttrDecls_Cons sem_LocAttrDecls_Nil (Prelude.map sem_LocAttrDecl list))
-- semantic domain
type T_LocAttrDecls = ( LocAttrDecls)
data Inh_LocAttrDecls = Inh_LocAttrDecls {}
data Syn_LocAttrDecls = Syn_LocAttrDecls {self_Syn_LocAttrDecls :: LocAttrDecls}
wrap_LocAttrDecls :: T_LocAttrDecls ->
                     Inh_LocAttrDecls ->
                     Syn_LocAttrDecls
wrap_LocAttrDecls sem (Inh_LocAttrDecls) =
    (let ( _lhsOself) = sem
     in  (Syn_LocAttrDecls _lhsOself))
sem_LocAttrDecls_Cons :: T_LocAttrDecl ->
                         T_LocAttrDecls ->
                         T_LocAttrDecls
sem_LocAttrDecls_Cons hd_ tl_ =
    (let _lhsOself :: LocAttrDecls
         _hdIself :: LocAttrDecl
         _tlIself :: LocAttrDecls
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_LocAttrDecls_Nil :: T_LocAttrDecls
sem_LocAttrDecls_Nil =
    (let _lhsOself :: LocAttrDecls
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- LocEnvAttrType ----------------------------------------------
-- cata
sem_LocEnvAttrType :: LocEnvAttrType ->
                      T_LocEnvAttrType
sem_LocEnvAttrType m =
    (Data.Map.foldrWithKey sem_LocEnvAttrType_Entry sem_LocEnvAttrType_Nil (Data.Map.map sem_Type m))
-- semantic domain
type T_LocEnvAttrType = ( LocEnvAttrType)
data Inh_LocEnvAttrType = Inh_LocEnvAttrType {}
data Syn_LocEnvAttrType = Syn_LocEnvAttrType {self_Syn_LocEnvAttrType :: LocEnvAttrType}
wrap_LocEnvAttrType :: T_LocEnvAttrType ->
                       Inh_LocEnvAttrType ->
                       Syn_LocEnvAttrType
wrap_LocEnvAttrType sem (Inh_LocEnvAttrType) =
    (let ( _lhsOself) = sem
     in  (Syn_LocEnvAttrType _lhsOself))
sem_LocEnvAttrType_Entry :: AttrName ->
                            T_Type ->
                            T_LocEnvAttrType ->
                            T_LocEnvAttrType
sem_LocEnvAttrType_Entry key_ val_ tl_ =
    (let _lhsOself :: LocEnvAttrType
         _valIpretty :: Doc
         _valIself :: Type
         _tlIself :: LocEnvAttrType
         _self =
             Data.Map.insert key_ _valIself _tlIself
         _lhsOself =
             _self
         ( _valIpretty,_valIself) =
             val_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_LocEnvAttrType_Nil :: T_LocEnvAttrType
sem_LocEnvAttrType_Nil =
    (let _lhsOself :: LocEnvAttrType
         _self =
             Data.Map.empty
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MbSortName --------------------------------------------------
-- cata
sem_MbSortName :: MbSortName ->
                  T_MbSortName
sem_MbSortName (Prelude.Just x) =
    (sem_MbSortName_Just x)
sem_MbSortName Prelude.Nothing =
    sem_MbSortName_Nothing
-- semantic domain
type T_MbSortName = ( MbSortName)
data Inh_MbSortName = Inh_MbSortName {}
data Syn_MbSortName = Syn_MbSortName {self_Syn_MbSortName :: MbSortName}
wrap_MbSortName :: T_MbSortName ->
                   Inh_MbSortName ->
                   Syn_MbSortName
wrap_MbSortName sem (Inh_MbSortName) =
    (let ( _lhsOself) = sem
     in  (Syn_MbSortName _lhsOself))
sem_MbSortName_Just :: SortName ->
                       T_MbSortName
sem_MbSortName_Just just_ =
    (let _lhsOself :: MbSortName
         _self =
             Just just_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_MbSortName_Nothing :: T_MbSortName
sem_MbSortName_Nothing =
    (let _lhsOself :: MbSortName
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- NamespaceDecl -----------------------------------------------
-- cata
sem_NamespaceDecl :: NamespaceDecl ->
                     T_NamespaceDecl
sem_NamespaceDecl (NamespaceDecl _namespaceName _namespaceTarget) =
    (sem_NamespaceDecl_NamespaceDecl _namespaceName (sem_MbSortName _namespaceTarget))
-- semantic domain
type T_NamespaceDecl = ( NamespaceDecl)
data Inh_NamespaceDecl = Inh_NamespaceDecl {}
data Syn_NamespaceDecl = Syn_NamespaceDecl {self_Syn_NamespaceDecl :: NamespaceDecl}
wrap_NamespaceDecl :: T_NamespaceDecl ->
                      Inh_NamespaceDecl ->
                      Syn_NamespaceDecl
wrap_NamespaceDecl sem (Inh_NamespaceDecl) =
    (let ( _lhsOself) = sem
     in  (Syn_NamespaceDecl _lhsOself))
sem_NamespaceDecl_NamespaceDecl :: NamespaceName ->
                                   T_MbSortName ->
                                   T_NamespaceDecl
sem_NamespaceDecl_NamespaceDecl namespaceName_ namespaceTarget_ =
    (let _lhsOself :: NamespaceDecl
         _namespaceTargetIself :: MbSortName
         _self =
             NamespaceDecl namespaceName_ _namespaceTargetIself
         _lhsOself =
             _self
         ( _namespaceTargetIself) =
             namespaceTarget_
     in  ( _lhsOself))
-- NamespaceDecls ----------------------------------------------
-- cata
sem_NamespaceDecls :: NamespaceDecls ->
                      T_NamespaceDecls
sem_NamespaceDecls list =
    (Prelude.foldr sem_NamespaceDecls_Cons sem_NamespaceDecls_Nil (Prelude.map sem_NamespaceDecl list))
-- semantic domain
type T_NamespaceDecls = ( NamespaceDecls)
data Inh_NamespaceDecls = Inh_NamespaceDecls {}
data Syn_NamespaceDecls = Syn_NamespaceDecls {self_Syn_NamespaceDecls :: NamespaceDecls}
wrap_NamespaceDecls :: T_NamespaceDecls ->
                       Inh_NamespaceDecls ->
                       Syn_NamespaceDecls
wrap_NamespaceDecls sem (Inh_NamespaceDecls) =
    (let ( _lhsOself) = sem
     in  (Syn_NamespaceDecls _lhsOself))
sem_NamespaceDecls_Cons :: T_NamespaceDecl ->
                           T_NamespaceDecls ->
                           T_NamespaceDecls
sem_NamespaceDecls_Cons hd_ tl_ =
    (let _lhsOself :: NamespaceDecls
         _hdIself :: NamespaceDecl
         _tlIself :: NamespaceDecls
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_NamespaceDecls_Nil :: T_NamespaceDecls
sem_NamespaceDecls_Nil =
    (let _lhsOself :: NamespaceDecls
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
type T_NodeLabel = ( Doc,NodeLabel)
data Inh_NodeLabel = Inh_NodeLabel {}
data Syn_NodeLabel = Syn_NodeLabel {pretty_Syn_NodeLabel :: Doc,self_Syn_NodeLabel :: NodeLabel}
wrap_NodeLabel :: T_NodeLabel ->
                  Inh_NodeLabel ->
                  Syn_NodeLabel
wrap_NodeLabel sem (Inh_NodeLabel) =
    (let ( _lhsOpretty,_lhsOself) = sem
     in  (Syn_NodeLabel _lhsOpretty _lhsOself))
sem_NodeLabel_Lhs :: T_NodeLabel
sem_NodeLabel_Lhs =
    (let _lhsOpretty :: Doc
         _lhsOself :: NodeLabel
         _lhsOpretty =
             ({-# LINE 210 "src/Ag/Pretty.ag" #-}
              text "lhs"
              {-# LINE 2571 "src/Ag/AG.hs" #-}
              )
         _self =
             Lhs
         _lhsOself =
             _self
     in  ( _lhsOpretty,_lhsOself))
sem_NodeLabel_Loc :: T_NodeLabel
sem_NodeLabel_Loc =
    (let _lhsOpretty :: Doc
         _lhsOself :: NodeLabel
         _lhsOpretty =
             ({-# LINE 212 "src/Ag/Pretty.ag" #-}
              text "loc"
              {-# LINE 2585 "src/Ag/AG.hs" #-}
              )
         _self =
             Loc
         _lhsOself =
             _self
     in  ( _lhsOpretty,_lhsOself))
sem_NodeLabel_Sub :: FieldName ->
                     T_NodeLabel
sem_NodeLabel_Sub nodeFieldLabel_ =
    (let _lhsOpretty :: Doc
         _lhsOself :: NodeLabel
         _lhsOpretty =
             ({-# LINE 214 "src/Ag/Pretty.ag" #-}
              text (fromFN nodeFieldLabel_)
              {-# LINE 2600 "src/Ag/AG.hs" #-}
              )
         _self =
             Sub nodeFieldLabel_
         _lhsOself =
             _self
     in  ( _lhsOpretty,_lhsOself))
-- SemFun ------------------------------------------------------
-- cata
sem_SemFun :: SemFun ->
              T_SemFun
sem_SemFun (SemFun _semName _semInh _semSyn) =
    (sem_SemFun_SemFun _semName (sem_AttrNameTypes _semInh) (sem_AttrNameTypes _semSyn))
-- semantic domain
type T_SemFun = EnvAttrType ->
                EnvAttrType ->
                SortName ->
                LocEnvAttrType ->
                LocEnvAttrType ->
                ( Doc,SemFun)
data Inh_SemFun = Inh_SemFun {envAttrInh_Inh_SemFun :: EnvAttrType,envAttrSyn_Inh_SemFun :: EnvAttrType,envSort_Inh_SemFun :: SortName,locEnvAttrInh_Inh_SemFun :: LocEnvAttrType,locEnvAttrSyn_Inh_SemFun :: LocEnvAttrType}
data Syn_SemFun = Syn_SemFun {pretty_Syn_SemFun :: Doc,self_Syn_SemFun :: SemFun}
wrap_SemFun :: T_SemFun ->
               Inh_SemFun ->
               Syn_SemFun
wrap_SemFun sem (Inh_SemFun _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn) =
    (let ( _lhsOpretty,_lhsOself) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn
     in  (Syn_SemFun _lhsOpretty _lhsOself))
sem_SemFun_SemFun :: String ->
                     T_AttrNameTypes ->
                     T_AttrNameTypes ->
                     T_SemFun
sem_SemFun_SemFun semName_ semInh_ semSyn_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn ->
         (let _semInhOfresh :: Int
              _semSynOfresh :: Int
              _lhsOpretty :: Doc
              _lhsOself :: SemFun
              _semInhOenvSort :: SortName
              _semInhOlocEnvAttrInh :: LocEnvAttrType
              _semInhOlocEnvAttrSyn :: LocEnvAttrType
              _semSynOenvSort :: SortName
              _semSynOlocEnvAttrInh :: LocEnvAttrType
              _semSynOlocEnvAttrSyn :: LocEnvAttrType
              _semInhIprettyInhInit :: ([Doc])
              _semInhIprettyInhParam :: ([Doc])
              _semInhIprettySynAccess :: ([Doc])
              _semInhIprettyType :: ([Doc])
              _semInhIself :: AttrNameTypes
              _semSynIprettyInhInit :: ([Doc])
              _semSynIprettyInhParam :: ([Doc])
              _semSynIprettySynAccess :: ([Doc])
              _semSynIprettyType :: ([Doc])
              _semSynIself :: AttrNameTypes
              _semInhOfresh =
                  ({-# LINE 295 "src/Ag/Pretty.ag" #-}
                   1
                   {-# LINE 2661 "src/Ag/AG.hs" #-}
                   )
              _semSynOfresh =
                  ({-# LINE 296 "src/Ag/Pretty.ag" #-}
                   1
                   {-# LINE 2666 "src/Ag/AG.hs" #-}
                   )
              _signature =
                  ({-# LINE 305 "src/Ag/Pretty.ag" #-}
                   hsep [ text semName_
                        , text "::"
                        , hsep (punctuate' (text "->") $
                                  [text $ fromSN _lhsIenvSort] ++
                                  _semInhIprettyType ++
                                  [parenList _semSynIprettyType])
                        ]
                   {-# LINE 2677 "src/Ag/AG.hs" #-}
                   )
              _definition =
                  ({-# LINE 313 "src/Ag/Pretty.ag" #-}
                   vcat
                   [ hsep
                     [ text semName_
                     , hsep (text "x0" : _semInhIprettyInhParam)
                     , text "="
                     , text "res"
                     ]
                   , indent 2 $ text "where"
                   , indent 4 . hsep $
                     [ text "inh"
                     , text "="
                     , text ("Inh_" ++ fromSN _lhsIenvSort)
                     , braceList _semInhIprettyInhInit
                     ]
                   , indent 4 . hsep $
                     [ text "syn"
                     , text "="
                     , text ("wrap_" ++ fromSN _lhsIenvSort)
                     , parens (text ("sem_" ++ fromSN _lhsIenvSort) <+>
                               text "x0")
                     , text "inh"
                     ]
                   , indent 4 . hsep $
                     [ text "res"
                     , text "="
                     , parenList _semSynIprettySynAccess
                     ]
                   ]
                   {-# LINE 2709 "src/Ag/AG.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 342 "src/Ag/Pretty.ag" #-}
                   _signature     <$>
                   _definition
                   {-# LINE 2715 "src/Ag/AG.hs" #-}
                   )
              _self =
                  SemFun semName_ _semInhIself _semSynIself
              _lhsOself =
                  _self
              _semInhOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2724 "src/Ag/AG.hs" #-}
                   )
              _semInhOlocEnvAttrInh =
                  ({-# LINE 33 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 2729 "src/Ag/AG.hs" #-}
                   )
              _semInhOlocEnvAttrSyn =
                  ({-# LINE 32 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 2734 "src/Ag/AG.hs" #-}
                   )
              _semSynOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2739 "src/Ag/AG.hs" #-}
                   )
              _semSynOlocEnvAttrInh =
                  ({-# LINE 33 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 2744 "src/Ag/AG.hs" #-}
                   )
              _semSynOlocEnvAttrSyn =
                  ({-# LINE 32 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 2749 "src/Ag/AG.hs" #-}
                   )
              ( _semInhIprettyInhInit,_semInhIprettyInhParam,_semInhIprettySynAccess,_semInhIprettyType,_semInhIself) =
                  semInh_ _semInhOenvSort _semInhOfresh _semInhOlocEnvAttrInh _semInhOlocEnvAttrSyn
              ( _semSynIprettyInhInit,_semSynIprettyInhParam,_semSynIprettySynAccess,_semSynIprettyType,_semSynIself) =
                  semSyn_ _semSynOenvSort _semSynOfresh _semSynOlocEnvAttrInh _semSynOlocEnvAttrSyn
          in  ( _lhsOpretty,_lhsOself)))
-- SemFuns -----------------------------------------------------
-- cata
sem_SemFuns :: SemFuns ->
               T_SemFuns
sem_SemFuns list =
    (Prelude.foldr sem_SemFuns_Cons sem_SemFuns_Nil (Prelude.map sem_SemFun list))
-- semantic domain
type T_SemFuns = EnvAttrType ->
                 EnvAttrType ->
                 SortName ->
                 LocEnvAttrType ->
                 LocEnvAttrType ->
                 ( Doc,([Doc]),SemFuns,Doc)
data Inh_SemFuns = Inh_SemFuns {envAttrInh_Inh_SemFuns :: EnvAttrType,envAttrSyn_Inh_SemFuns :: EnvAttrType,envSort_Inh_SemFuns :: SortName,locEnvAttrInh_Inh_SemFuns :: LocEnvAttrType,locEnvAttrSyn_Inh_SemFuns :: LocEnvAttrType}
data Syn_SemFuns = Syn_SemFuns {hPretty_Syn_SemFuns :: Doc,lPretty_Syn_SemFuns :: ([Doc]),self_Syn_SemFuns :: SemFuns,vPretty_Syn_SemFuns :: Doc}
wrap_SemFuns :: T_SemFuns ->
                Inh_SemFuns ->
                Syn_SemFuns
wrap_SemFuns sem (Inh_SemFuns _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn) =
    (let ( _lhsOhPretty,_lhsOlPretty,_lhsOself,_lhsOvPretty) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn
     in  (Syn_SemFuns _lhsOhPretty _lhsOlPretty _lhsOself _lhsOvPretty))
sem_SemFuns_Cons :: T_SemFun ->
                    T_SemFuns ->
                    T_SemFuns
sem_SemFuns_Cons hd_ tl_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn ->
         (let _lhsOlPretty :: ([Doc])
              _lhsOself :: SemFuns
              _lhsOhPretty :: Doc
              _lhsOvPretty :: Doc
              _hdOenvAttrInh :: EnvAttrType
              _hdOenvAttrSyn :: EnvAttrType
              _hdOenvSort :: SortName
              _hdOlocEnvAttrInh :: LocEnvAttrType
              _hdOlocEnvAttrSyn :: LocEnvAttrType
              _tlOenvAttrInh :: EnvAttrType
              _tlOenvAttrSyn :: EnvAttrType
              _tlOenvSort :: SortName
              _tlOlocEnvAttrInh :: LocEnvAttrType
              _tlOlocEnvAttrSyn :: LocEnvAttrType
              _hdIpretty :: Doc
              _hdIself :: SemFun
              _tlIhPretty :: Doc
              _tlIlPretty :: ([Doc])
              _tlIself :: SemFuns
              _tlIvPretty :: Doc
              _lPretty =
                  ({-# LINE 87 "src/Ag/Pretty.ag" #-}
                   _hdIpretty : _tlIlPretty
                   {-# LINE 2809 "src/Ag/AG.hs" #-}
                   )
              _vPretty =
                  ({-# LINE 88 "src/Ag/Pretty.ag" #-}
                   vsep _lPretty
                   {-# LINE 2814 "src/Ag/AG.hs" #-}
                   )
              _hPretty =
                  ({-# LINE 89 "src/Ag/Pretty.ag" #-}
                   hsep _lPretty
                   {-# LINE 2819 "src/Ag/AG.hs" #-}
                   )
              _lhsOlPretty =
                  ({-# LINE 77 "src/Ag/Pretty.ag" #-}
                   _lPretty
                   {-# LINE 2824 "src/Ag/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOhPretty =
                  ({-# LINE 79 "src/Ag/Pretty.ag" #-}
                   _hPretty
                   {-# LINE 2833 "src/Ag/AG.hs" #-}
                   )
              _lhsOvPretty =
                  ({-# LINE 78 "src/Ag/Pretty.ag" #-}
                   _vPretty
                   {-# LINE 2838 "src/Ag/AG.hs" #-}
                   )
              _hdOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2843 "src/Ag/AG.hs" #-}
                   )
              _hdOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2848 "src/Ag/AG.hs" #-}
                   )
              _hdOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2853 "src/Ag/AG.hs" #-}
                   )
              _hdOlocEnvAttrInh =
                  ({-# LINE 33 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 2858 "src/Ag/AG.hs" #-}
                   )
              _hdOlocEnvAttrSyn =
                  ({-# LINE 32 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 2863 "src/Ag/AG.hs" #-}
                   )
              _tlOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2868 "src/Ag/AG.hs" #-}
                   )
              _tlOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2873 "src/Ag/AG.hs" #-}
                   )
              _tlOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2878 "src/Ag/AG.hs" #-}
                   )
              _tlOlocEnvAttrInh =
                  ({-# LINE 33 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 2883 "src/Ag/AG.hs" #-}
                   )
              _tlOlocEnvAttrSyn =
                  ({-# LINE 32 "src/Ag/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 2888 "src/Ag/AG.hs" #-}
                   )
              ( _hdIpretty,_hdIself) =
                  hd_ _hdOenvAttrInh _hdOenvAttrSyn _hdOenvSort _hdOlocEnvAttrInh _hdOlocEnvAttrSyn
              ( _tlIhPretty,_tlIlPretty,_tlIself,_tlIvPretty) =
                  tl_ _tlOenvAttrInh _tlOenvAttrSyn _tlOenvSort _tlOlocEnvAttrInh _tlOlocEnvAttrSyn
          in  ( _lhsOhPretty,_lhsOlPretty,_lhsOself,_lhsOvPretty)))
sem_SemFuns_Nil :: T_SemFuns
sem_SemFuns_Nil =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn ->
         (let _lhsOlPretty :: ([Doc])
              _lhsOself :: SemFuns
              _lhsOhPretty :: Doc
              _lhsOvPretty :: Doc
              _lPretty =
                  ({-# LINE 83 "src/Ag/Pretty.ag" #-}
                   []
                   {-# LINE 2909 "src/Ag/AG.hs" #-}
                   )
              _vPretty =
                  ({-# LINE 84 "src/Ag/Pretty.ag" #-}
                   empty
                   {-# LINE 2914 "src/Ag/AG.hs" #-}
                   )
              _hPretty =
                  ({-# LINE 85 "src/Ag/Pretty.ag" #-}
                   empty
                   {-# LINE 2919 "src/Ag/AG.hs" #-}
                   )
              _lhsOlPretty =
                  ({-# LINE 77 "src/Ag/Pretty.ag" #-}
                   _lPretty
                   {-# LINE 2924 "src/Ag/AG.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOhPretty =
                  ({-# LINE 79 "src/Ag/Pretty.ag" #-}
                   _hPretty
                   {-# LINE 2933 "src/Ag/AG.hs" #-}
                   )
              _lhsOvPretty =
                  ({-# LINE 78 "src/Ag/Pretty.ag" #-}
                   _vPretty
                   {-# LINE 2938 "src/Ag/AG.hs" #-}
                   )
          in  ( _lhsOhPretty,_lhsOlPretty,_lhsOself,_lhsOvPretty)))
-- SortDecl ----------------------------------------------------
-- cata
sem_SortDecl :: SortDecl ->
                T_SortDecl
sem_SortDecl (SortDecl _sortName _sortAttributes _sortCtors _sortSemFuns) =
    (sem_SortDecl_SortDecl _sortName (sem_AttrDecls _sortAttributes) (sem_CtorDecls _sortCtors) (sem_SemFuns _sortSemFuns))
-- semantic domain
type T_SortDecl = EnvAttrType ->
                  EnvAttrType ->
                  ( Doc,Doc,Doc,Doc,EnvAttrType,EnvAttrType,SortDecl)
data Inh_SortDecl = Inh_SortDecl {envAttrInh_Inh_SortDecl :: EnvAttrType,envAttrSyn_Inh_SortDecl :: EnvAttrType}
data Syn_SortDecl = Syn_SortDecl {pretty_Syn_SortDecl :: Doc,prettyAttr_Syn_SortDecl :: Doc,prettySem_Syn_SortDecl :: Doc,prettySemFun_Syn_SortDecl :: Doc,sAttrInh_Syn_SortDecl :: EnvAttrType,sAttrSyn_Syn_SortDecl :: EnvAttrType,self_Syn_SortDecl :: SortDecl}
wrap_SortDecl :: T_SortDecl ->
                 Inh_SortDecl ->
                 Syn_SortDecl
wrap_SortDecl sem (Inh_SortDecl _lhsIenvAttrInh _lhsIenvAttrSyn) =
    (let ( _lhsOpretty,_lhsOprettyAttr,_lhsOprettySem,_lhsOprettySemFun,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself) = sem _lhsIenvAttrInh _lhsIenvAttrSyn
     in  (Syn_SortDecl _lhsOpretty _lhsOprettyAttr _lhsOprettySem _lhsOprettySemFun _lhsOsAttrInh _lhsOsAttrSyn _lhsOself))
sem_SortDecl_SortDecl :: SortName ->
                         T_AttrDecls ->
                         T_CtorDecls ->
                         T_SemFuns ->
                         T_SortDecl
sem_SortDecl_SortDecl sortName_ sortAttributes_ sortCtors_ sortSemFuns_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn ->
         (let _lhsOsAttrSyn :: EnvAttrType
              _lhsOsAttrInh :: EnvAttrType
              _lhsOpretty :: Doc
              _lhsOself :: SortDecl
              _lhsOprettyAttr :: Doc
              _lhsOprettySem :: Doc
              _lhsOprettySemFun :: Doc
              _sortAttributesOenvSort :: SortName
              _sortCtorsOenvAttrInh :: EnvAttrType
              _sortCtorsOenvAttrSyn :: EnvAttrType
              _sortCtorsOenvSort :: SortName
              _sortCtorsOlocEnvAttrInh :: LocEnvAttrType
              _sortCtorsOlocEnvAttrSyn :: LocEnvAttrType
              _sortSemFunsOenvAttrInh :: EnvAttrType
              _sortSemFunsOenvAttrSyn :: EnvAttrType
              _sortSemFunsOenvSort :: SortName
              _sortSemFunsOlocEnvAttrInh :: LocEnvAttrType
              _sortSemFunsOlocEnvAttrSyn :: LocEnvAttrType
              _sortAttributesIpretty :: ([Doc])
              _sortAttributesIsAttrInh :: LocEnvAttrType
              _sortAttributesIsAttrSyn :: LocEnvAttrType
              _sortAttributesIself :: AttrDecls
              _sortCtorsIhPretty :: Doc
              _sortCtorsIlPretty :: ([Doc])
              _sortCtorsIprettySem :: ([Doc])
              _sortCtorsIsAttrInh :: EnvAttrType
              _sortCtorsIsAttrSyn :: EnvAttrType
              _sortCtorsIself :: CtorDecls
              _sortCtorsIvPretty :: Doc
              _sortSemFunsIhPretty :: Doc
              _sortSemFunsIlPretty :: ([Doc])
              _sortSemFunsIself :: SemFuns
              _sortSemFunsIvPretty :: Doc
              _pretty =
                  ({-# LINE 93 "src/Ag/Pretty.ag" #-}
                   blankline $
                     mySep [ text "data"
                           , text (fromSN sortName_)
                           ] <$>
                     indent 2 (vcat (map ((char '|')<+>) _sortCtorsIlPretty)) <$>
                     mySep [ text "deriving"
                           , text (fromSN sortName_)
                           , colon
                           , text "Show"
                           ]
                   {-# LINE 3012 "src/Ag/AG.hs" #-}
                   )
              _hasAttrDecls =
                  ({-# LINE 133 "src/Ag/Pretty.ag" #-}
                   not $ null _sortAttributesIself
                   {-# LINE 3017 "src/Ag/AG.hs" #-}
                   )
              _prettyAttr =
                  ({-# LINE 134 "src/Ag/Pretty.ag" #-}
                   if _hasAttrDecls
                     then text "attr" <+> text (fromSN sortName_) <$>
                          indent 2 (myVcat _sortAttributesIpretty)
                     else empty
                   {-# LINE 3025 "src/Ag/AG.hs" #-}
                   )
              _hasAttrDefs =
                  ({-# LINE 182 "src/Ag/Pretty.ag" #-}
                   True
                   {-# LINE 3030 "src/Ag/AG.hs" #-}
                   )
              _prettySem =
                  ({-# LINE 183 "src/Ag/Pretty.ag" #-}
                   if _hasAttrDefs
                     then text "sem" <+> text (fromSN sortName_) <$>
                          indent 2 (myVcat _sortCtorsIprettySem)
                     else empty
                   {-# LINE 3038 "src/Ag/AG.hs" #-}
                   )
              _prettySemFun =
                  ({-# LINE 280 "src/Ag/Pretty.ag" #-}
                   vcat (_sortSemFunsIlPretty)
                   {-# LINE 3043 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 47 "src/Ag/Environment.ag" #-}
                   M.mapKeysMonotonic
                     (\an -> (sortName_,an))
                     _sortAttributesIsAttrSyn
                   {-# LINE 3050 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 50 "src/Ag/Environment.ag" #-}
                   M.mapKeysMonotonic
                     (\an -> (sortName_,an))
                     _sortAttributesIsAttrInh
                   {-# LINE 3057 "src/Ag/AG.hs" #-}
                   )
              _locEnvAttrSyn =
                  ({-# LINE 56 "src/Ag/Environment.ag" #-}
                   _sortAttributesIsAttrSyn
                   {-# LINE 3062 "src/Ag/AG.hs" #-}
                   )
              _locEnvAttrInh =
                  ({-# LINE 57 "src/Ag/Environment.ag" #-}
                   _sortAttributesIsAttrInh
                   {-# LINE 3067 "src/Ag/AG.hs" #-}
                   )
              _envSort =
                  ({-# LINE 73 "src/Ag/Environment.ag" #-}
                   sortName_
                   {-# LINE 3072 "src/Ag/AG.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 74 "src/Ag/Pretty.ag" #-}
                   _pretty
                   {-# LINE 3077 "src/Ag/AG.hs" #-}
                   )
              _self =
                  SortDecl sortName_ _sortAttributesIself _sortCtorsIself _sortSemFunsIself
              _lhsOself =
                  _self
              _lhsOprettyAttr =
                  ({-# LINE 126 "src/Ag/Pretty.ag" #-}
                   _prettyAttr
                   {-# LINE 3086 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettySem =
                  ({-# LINE 170 "src/Ag/Pretty.ag" #-}
                   _prettySem
                   {-# LINE 3091 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettySemFun =
                  ({-# LINE 275 "src/Ag/Pretty.ag" #-}
                   _prettySemFun
                   {-# LINE 3096 "src/Ag/AG.hs" #-}
                   )
              _sortAttributesOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _envSort
                   {-# LINE 3101 "src/Ag/AG.hs" #-}
                   )
              _sortCtorsOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 3106 "src/Ag/AG.hs" #-}
                   )
              _sortCtorsOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 3111 "src/Ag/AG.hs" #-}
                   )
              _sortCtorsOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _envSort
                   {-# LINE 3116 "src/Ag/AG.hs" #-}
                   )
              _sortCtorsOlocEnvAttrInh =
                  ({-# LINE 33 "src/Ag/Environment.ag" #-}
                   _locEnvAttrInh
                   {-# LINE 3121 "src/Ag/AG.hs" #-}
                   )
              _sortCtorsOlocEnvAttrSyn =
                  ({-# LINE 32 "src/Ag/Environment.ag" #-}
                   _locEnvAttrSyn
                   {-# LINE 3126 "src/Ag/AG.hs" #-}
                   )
              _sortSemFunsOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 3131 "src/Ag/AG.hs" #-}
                   )
              _sortSemFunsOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 3136 "src/Ag/AG.hs" #-}
                   )
              _sortSemFunsOenvSort =
                  ({-# LINE 69 "src/Ag/Environment.ag" #-}
                   _envSort
                   {-# LINE 3141 "src/Ag/AG.hs" #-}
                   )
              _sortSemFunsOlocEnvAttrInh =
                  ({-# LINE 33 "src/Ag/Environment.ag" #-}
                   _locEnvAttrInh
                   {-# LINE 3146 "src/Ag/AG.hs" #-}
                   )
              _sortSemFunsOlocEnvAttrSyn =
                  ({-# LINE 32 "src/Ag/Environment.ag" #-}
                   _locEnvAttrSyn
                   {-# LINE 3151 "src/Ag/AG.hs" #-}
                   )
              ( _sortAttributesIpretty,_sortAttributesIsAttrInh,_sortAttributesIsAttrSyn,_sortAttributesIself) =
                  sortAttributes_ _sortAttributesOenvSort
              ( _sortCtorsIhPretty,_sortCtorsIlPretty,_sortCtorsIprettySem,_sortCtorsIsAttrInh,_sortCtorsIsAttrSyn,_sortCtorsIself,_sortCtorsIvPretty) =
                  sortCtors_ _sortCtorsOenvAttrInh _sortCtorsOenvAttrSyn _sortCtorsOenvSort _sortCtorsOlocEnvAttrInh _sortCtorsOlocEnvAttrSyn
              ( _sortSemFunsIhPretty,_sortSemFunsIlPretty,_sortSemFunsIself,_sortSemFunsIvPretty) =
                  sortSemFuns_ _sortSemFunsOenvAttrInh _sortSemFunsOenvAttrSyn _sortSemFunsOenvSort _sortSemFunsOlocEnvAttrInh _sortSemFunsOlocEnvAttrSyn
          in  ( _lhsOpretty,_lhsOprettyAttr,_lhsOprettySem,_lhsOprettySemFun,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself)))
-- SortDecls ---------------------------------------------------
-- cata
sem_SortDecls :: SortDecls ->
                 T_SortDecls
sem_SortDecls list =
    (Prelude.foldr sem_SortDecls_Cons sem_SortDecls_Nil (Prelude.map sem_SortDecl list))
-- semantic domain
type T_SortDecls = EnvAttrType ->
                   EnvAttrType ->
                   ( Doc,([Doc]),([Doc]),([Doc]),([Doc]),EnvAttrType,EnvAttrType,SortDecls,Doc)
data Inh_SortDecls = Inh_SortDecls {envAttrInh_Inh_SortDecls :: EnvAttrType,envAttrSyn_Inh_SortDecls :: EnvAttrType}
data Syn_SortDecls = Syn_SortDecls {hPretty_Syn_SortDecls :: Doc,lPretty_Syn_SortDecls :: ([Doc]),prettyAttr_Syn_SortDecls :: ([Doc]),prettySem_Syn_SortDecls :: ([Doc]),prettySemFun_Syn_SortDecls :: ([Doc]),sAttrInh_Syn_SortDecls :: EnvAttrType,sAttrSyn_Syn_SortDecls :: EnvAttrType,self_Syn_SortDecls :: SortDecls,vPretty_Syn_SortDecls :: Doc}
wrap_SortDecls :: T_SortDecls ->
                  Inh_SortDecls ->
                  Syn_SortDecls
wrap_SortDecls sem (Inh_SortDecls _lhsIenvAttrInh _lhsIenvAttrSyn) =
    (let ( _lhsOhPretty,_lhsOlPretty,_lhsOprettyAttr,_lhsOprettySem,_lhsOprettySemFun,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself,_lhsOvPretty) = sem _lhsIenvAttrInh _lhsIenvAttrSyn
     in  (Syn_SortDecls _lhsOhPretty _lhsOlPretty _lhsOprettyAttr _lhsOprettySem _lhsOprettySemFun _lhsOsAttrInh _lhsOsAttrSyn _lhsOself _lhsOvPretty))
sem_SortDecls_Cons :: T_SortDecl ->
                      T_SortDecls ->
                      T_SortDecls
sem_SortDecls_Cons hd_ tl_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn ->
         (let _lhsOlPretty :: ([Doc])
              _lhsOprettyAttr :: ([Doc])
              _lhsOprettySem :: ([Doc])
              _lhsOprettySemFun :: ([Doc])
              _lhsOsAttrInh :: EnvAttrType
              _lhsOsAttrSyn :: EnvAttrType
              _lhsOself :: SortDecls
              _lhsOhPretty :: Doc
              _lhsOvPretty :: Doc
              _hdOenvAttrInh :: EnvAttrType
              _hdOenvAttrSyn :: EnvAttrType
              _tlOenvAttrInh :: EnvAttrType
              _tlOenvAttrSyn :: EnvAttrType
              _hdIpretty :: Doc
              _hdIprettyAttr :: Doc
              _hdIprettySem :: Doc
              _hdIprettySemFun :: Doc
              _hdIsAttrInh :: EnvAttrType
              _hdIsAttrSyn :: EnvAttrType
              _hdIself :: SortDecl
              _tlIhPretty :: Doc
              _tlIlPretty :: ([Doc])
              _tlIprettyAttr :: ([Doc])
              _tlIprettySem :: ([Doc])
              _tlIprettySemFun :: ([Doc])
              _tlIsAttrInh :: EnvAttrType
              _tlIsAttrSyn :: EnvAttrType
              _tlIself :: SortDecls
              _tlIvPretty :: Doc
              _lPretty =
                  ({-# LINE 87 "src/Ag/Pretty.ag" #-}
                   _hdIpretty : _tlIlPretty
                   {-# LINE 3216 "src/Ag/AG.hs" #-}
                   )
              _vPretty =
                  ({-# LINE 88 "src/Ag/Pretty.ag" #-}
                   vsep _lPretty
                   {-# LINE 3221 "src/Ag/AG.hs" #-}
                   )
              _hPretty =
                  ({-# LINE 89 "src/Ag/Pretty.ag" #-}
                   hsep _lPretty
                   {-# LINE 3226 "src/Ag/AG.hs" #-}
                   )
              _lhsOlPretty =
                  ({-# LINE 77 "src/Ag/Pretty.ag" #-}
                   _lPretty
                   {-# LINE 3231 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettyAttr =
                  ({-# LINE 127 "src/Ag/Pretty.ag" #-}
                   _hdIprettyAttr : _tlIprettyAttr
                   {-# LINE 3236 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettySem =
                  ({-# LINE 171 "src/Ag/Pretty.ag" #-}
                   _hdIprettySem : _tlIprettySem
                   {-# LINE 3241 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettySemFun =
                  ({-# LINE 276 "src/Ag/Pretty.ag" #-}
                   _hdIprettySemFun : _tlIprettySemFun
                   {-# LINE 3246 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 26 "src/Ag/Environment.ag" #-}
                   (M.union _hdIsAttrInh _tlIsAttrInh)
                   {-# LINE 3251 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 25 "src/Ag/Environment.ag" #-}
                   (M.union _hdIsAttrSyn _tlIsAttrSyn)
                   {-# LINE 3256 "src/Ag/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOhPretty =
                  ({-# LINE 79 "src/Ag/Pretty.ag" #-}
                   _hPretty
                   {-# LINE 3265 "src/Ag/AG.hs" #-}
                   )
              _lhsOvPretty =
                  ({-# LINE 78 "src/Ag/Pretty.ag" #-}
                   _vPretty
                   {-# LINE 3270 "src/Ag/AG.hs" #-}
                   )
              _hdOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 3275 "src/Ag/AG.hs" #-}
                   )
              _hdOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 3280 "src/Ag/AG.hs" #-}
                   )
              _tlOenvAttrInh =
                  ({-# LINE 39 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 3285 "src/Ag/AG.hs" #-}
                   )
              _tlOenvAttrSyn =
                  ({-# LINE 38 "src/Ag/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 3290 "src/Ag/AG.hs" #-}
                   )
              ( _hdIpretty,_hdIprettyAttr,_hdIprettySem,_hdIprettySemFun,_hdIsAttrInh,_hdIsAttrSyn,_hdIself) =
                  hd_ _hdOenvAttrInh _hdOenvAttrSyn
              ( _tlIhPretty,_tlIlPretty,_tlIprettyAttr,_tlIprettySem,_tlIprettySemFun,_tlIsAttrInh,_tlIsAttrSyn,_tlIself,_tlIvPretty) =
                  tl_ _tlOenvAttrInh _tlOenvAttrSyn
          in  ( _lhsOhPretty,_lhsOlPretty,_lhsOprettyAttr,_lhsOprettySem,_lhsOprettySemFun,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself,_lhsOvPretty)))
sem_SortDecls_Nil :: T_SortDecls
sem_SortDecls_Nil =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn ->
         (let _lhsOlPretty :: ([Doc])
              _lhsOprettyAttr :: ([Doc])
              _lhsOprettySem :: ([Doc])
              _lhsOprettySemFun :: ([Doc])
              _lhsOsAttrInh :: EnvAttrType
              _lhsOsAttrSyn :: EnvAttrType
              _lhsOself :: SortDecls
              _lhsOhPretty :: Doc
              _lhsOvPretty :: Doc
              _lPretty =
                  ({-# LINE 83 "src/Ag/Pretty.ag" #-}
                   []
                   {-# LINE 3313 "src/Ag/AG.hs" #-}
                   )
              _vPretty =
                  ({-# LINE 84 "src/Ag/Pretty.ag" #-}
                   empty
                   {-# LINE 3318 "src/Ag/AG.hs" #-}
                   )
              _hPretty =
                  ({-# LINE 85 "src/Ag/Pretty.ag" #-}
                   empty
                   {-# LINE 3323 "src/Ag/AG.hs" #-}
                   )
              _lhsOlPretty =
                  ({-# LINE 77 "src/Ag/Pretty.ag" #-}
                   _lPretty
                   {-# LINE 3328 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettyAttr =
                  ({-# LINE 127 "src/Ag/Pretty.ag" #-}
                   []
                   {-# LINE 3333 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettySem =
                  ({-# LINE 171 "src/Ag/Pretty.ag" #-}
                   []
                   {-# LINE 3338 "src/Ag/AG.hs" #-}
                   )
              _lhsOprettySemFun =
                  ({-# LINE 276 "src/Ag/Pretty.ag" #-}
                   []
                   {-# LINE 3343 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 26 "src/Ag/Environment.ag" #-}
                   M.empty
                   {-# LINE 3348 "src/Ag/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 25 "src/Ag/Environment.ag" #-}
                   M.empty
                   {-# LINE 3353 "src/Ag/AG.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOhPretty =
                  ({-# LINE 79 "src/Ag/Pretty.ag" #-}
                   _hPretty
                   {-# LINE 3362 "src/Ag/AG.hs" #-}
                   )
              _lhsOvPretty =
                  ({-# LINE 78 "src/Ag/Pretty.ag" #-}
                   _vPretty
                   {-# LINE 3367 "src/Ag/AG.hs" #-}
                   )
          in  ( _lhsOhPretty,_lhsOlPretty,_lhsOprettyAttr,_lhsOprettySem,_lhsOprettySemFun,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself,_lhsOvPretty)))
-- Specification -----------------------------------------------
-- cata
sem_Specification :: Specification ->
                     T_Specification
sem_Specification (Specification _specModuleName _specSynonyms _specSortDecls) =
    (sem_Specification_Specification _specModuleName (sem_Synonyms _specSynonyms) (sem_SortDecls _specSortDecls))
-- semantic domain
type T_Specification = ( Doc,EnvAttrType,EnvAttrType,Specification)
data Inh_Specification = Inh_Specification {}
data Syn_Specification = Syn_Specification {pretty_Syn_Specification :: Doc,sAttrInh_Syn_Specification :: EnvAttrType,sAttrSyn_Syn_Specification :: EnvAttrType,self_Syn_Specification :: Specification}
wrap_Specification :: T_Specification ->
                      Inh_Specification ->
                      Syn_Specification
wrap_Specification sem (Inh_Specification) =
    (let ( _lhsOpretty,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself) = sem
     in  (Syn_Specification _lhsOpretty _lhsOsAttrInh _lhsOsAttrSyn _lhsOself))
sem_Specification_Specification :: String ->
                                   T_Synonyms ->
                                   T_SortDecls ->
                                   T_Specification
sem_Specification_Specification specModuleName_ specSynonyms_ specSortDecls_ =
    (let _lhsOpretty :: Doc
         _lhsOsAttrInh :: EnvAttrType
         _lhsOsAttrSyn :: EnvAttrType
         _lhsOself :: Specification
         _specSortDeclsOenvAttrInh :: EnvAttrType
         _specSortDeclsOenvAttrSyn :: EnvAttrType
         _specSynonymsIself :: Synonyms
         _specSortDeclsIhPretty :: Doc
         _specSortDeclsIlPretty :: ([Doc])
         _specSortDeclsIprettyAttr :: ([Doc])
         _specSortDeclsIprettySem :: ([Doc])
         _specSortDeclsIprettySemFun :: ([Doc])
         _specSortDeclsIsAttrInh :: EnvAttrType
         _specSortDeclsIsAttrSyn :: EnvAttrType
         _specSortDeclsIself :: SortDecls
         _specSortDeclsIvPretty :: Doc
         _pretty =
             ({-# LINE 32 "src/Ag/Pretty.ag" #-}
              mySep [ text "module"
                    , braces (text specModuleName_)
                    , braces empty
                    , braces empty
                    ]
              <$> empty
              <$> text "imports"
              <$> char '{'
              <$> myVcat
                  [ text "import Data.Map (Map)"
                  , text "import Data.Set (Set)"
                  , text "import qualified Data.List"
                  , text "import qualified Data.Map"
                  , text "import qualified Data.Set"
                  ]
              <$> char '}'
              <$> empty
              <$> char '{'
              <$> myVcat
                  [ mySep [ text "type"
                          , text sn
                          , text "="
                          , text "String"
                          ]
                  | Synonym sn ty <- _specSynonymsIself
                  ]
              <$> empty
              <$> text "fresh :: Set String -> String"
              <$> text "fresh ctx = head [v | v <- vs, not (Data.Set.member v ctx)]"
              <$> text "  where vs = map (\\n -> 'v' : show n) [0..]"
              <$> char '}'
              <$> _specSortDeclsIvPretty
              <$> myVcat _specSortDeclsIprettyAttr
              <$> myVcat _specSortDeclsIprettySem
              <$> empty
              <$> char '{'
              <$> myVcat _specSortDeclsIprettySemFun
              <$> char '}'
              {-# LINE 3447 "src/Ag/AG.hs" #-}
              )
         _envAttrSyn =
             ({-# LINE 61 "src/Ag/Environment.ag" #-}
              _specSortDeclsIsAttrSyn
              {-# LINE 3452 "src/Ag/AG.hs" #-}
              )
         _envAttrInh =
             ({-# LINE 62 "src/Ag/Environment.ag" #-}
              _specSortDeclsIsAttrInh
              {-# LINE 3457 "src/Ag/AG.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 27 "src/Ag/Pretty.ag" #-}
              _pretty
              {-# LINE 3462 "src/Ag/AG.hs" #-}
              )
         _lhsOsAttrInh =
             ({-# LINE 26 "src/Ag/Environment.ag" #-}
              _specSortDeclsIsAttrInh
              {-# LINE 3467 "src/Ag/AG.hs" #-}
              )
         _lhsOsAttrSyn =
             ({-# LINE 25 "src/Ag/Environment.ag" #-}
              _specSortDeclsIsAttrSyn
              {-# LINE 3472 "src/Ag/AG.hs" #-}
              )
         _self =
             Specification specModuleName_ _specSynonymsIself _specSortDeclsIself
         _lhsOself =
             _self
         _specSortDeclsOenvAttrInh =
             ({-# LINE 39 "src/Ag/Environment.ag" #-}
              _envAttrInh
              {-# LINE 3481 "src/Ag/AG.hs" #-}
              )
         _specSortDeclsOenvAttrSyn =
             ({-# LINE 38 "src/Ag/Environment.ag" #-}
              _envAttrSyn
              {-# LINE 3486 "src/Ag/AG.hs" #-}
              )
         ( _specSynonymsIself) =
             specSynonyms_
         ( _specSortDeclsIhPretty,_specSortDeclsIlPretty,_specSortDeclsIprettyAttr,_specSortDeclsIprettySem,_specSortDeclsIprettySemFun,_specSortDeclsIsAttrInh,_specSortDeclsIsAttrSyn,_specSortDeclsIself,_specSortDeclsIvPretty) =
             specSortDecls_ _specSortDeclsOenvAttrInh _specSortDeclsOenvAttrSyn
     in  ( _lhsOpretty,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself))
-- Synonym -----------------------------------------------------
-- cata
sem_Synonym :: Synonym ->
               T_Synonym
sem_Synonym (Synonym _synonym _ty) =
    (sem_Synonym_Synonym _synonym (sem_Type _ty))
-- semantic domain
type T_Synonym = ( Synonym)
data Inh_Synonym = Inh_Synonym {}
data Syn_Synonym = Syn_Synonym {self_Syn_Synonym :: Synonym}
wrap_Synonym :: T_Synonym ->
                Inh_Synonym ->
                Syn_Synonym
wrap_Synonym sem (Inh_Synonym) =
    (let ( _lhsOself) = sem
     in  (Syn_Synonym _lhsOself))
sem_Synonym_Synonym :: String ->
                       T_Type ->
                       T_Synonym
sem_Synonym_Synonym synonym_ ty_ =
    (let _lhsOself :: Synonym
         _tyIpretty :: Doc
         _tyIself :: Type
         _self =
             Synonym synonym_ _tyIself
         _lhsOself =
             _self
         ( _tyIpretty,_tyIself) =
             ty_
     in  ( _lhsOself))
-- Synonyms ----------------------------------------------------
-- cata
sem_Synonyms :: Synonyms ->
                T_Synonyms
sem_Synonyms list =
    (Prelude.foldr sem_Synonyms_Cons sem_Synonyms_Nil (Prelude.map sem_Synonym list))
-- semantic domain
type T_Synonyms = ( Synonyms)
data Inh_Synonyms = Inh_Synonyms {}
data Syn_Synonyms = Syn_Synonyms {self_Syn_Synonyms :: Synonyms}
wrap_Synonyms :: T_Synonyms ->
                 Inh_Synonyms ->
                 Syn_Synonyms
wrap_Synonyms sem (Inh_Synonyms) =
    (let ( _lhsOself) = sem
     in  (Syn_Synonyms _lhsOself))
sem_Synonyms_Cons :: T_Synonym ->
                     T_Synonyms ->
                     T_Synonyms
sem_Synonyms_Cons hd_ tl_ =
    (let _lhsOself :: Synonyms
         _hdIself :: Synonym
         _tlIself :: Synonyms
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Synonyms_Nil :: T_Synonyms
sem_Synonyms_Nil =
    (let _lhsOself :: Synonyms
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Type --------------------------------------------------------
-- cata
sem_Type :: Type ->
            T_Type
sem_Type (TString) =
    (sem_Type_TString)
sem_Type (TSet _valueType) =
    (sem_Type_TSet (sem_Type _valueType))
sem_Type (TList _valueType) =
    (sem_Type_TList (sem_Type _valueType))
sem_Type (TMap _keyType _valueType) =
    (sem_Type_TMap (sem_Type _keyType) (sem_Type _valueType))
sem_Type (TTerminal _typeName) =
    (sem_Type_TTerminal _typeName)
sem_Type (TSort _sortName) =
    (sem_Type_TSort _sortName)
-- semantic domain
type T_Type = ( Doc,Type)
data Inh_Type = Inh_Type {}
data Syn_Type = Syn_Type {pretty_Syn_Type :: Doc,self_Syn_Type :: Type}
wrap_Type :: T_Type ->
             Inh_Type ->
             Syn_Type
wrap_Type sem (Inh_Type) =
    (let ( _lhsOpretty,_lhsOself) = sem
     in  (Syn_Type _lhsOpretty _lhsOself))
sem_Type_TString :: T_Type
sem_Type_TString =
    (let _lhsOpretty :: Doc
         _lhsOself :: Type
         _lhsOpretty =
             ({-# LINE 160 "src/Ag/Pretty.ag" #-}
              text "String"
              {-# LINE 3596 "src/Ag/AG.hs" #-}
              )
         _self =
             TString
         _lhsOself =
             _self
     in  ( _lhsOpretty,_lhsOself))
sem_Type_TSet :: T_Type ->
                 T_Type
sem_Type_TSet valueType_ =
    (let _lhsOpretty :: Doc
         _lhsOself :: Type
         _valueTypeIpretty :: Doc
         _valueTypeIself :: Type
         _lhsOpretty =
             ({-# LINE 161 "src/Ag/Pretty.ag" #-}
              text "Set" <+> parens _valueTypeIpretty
              {-# LINE 3613 "src/Ag/AG.hs" #-}
              )
         _self =
             TSet _valueTypeIself
         _lhsOself =
             _self
         ( _valueTypeIpretty,_valueTypeIself) =
             valueType_
     in  ( _lhsOpretty,_lhsOself))
sem_Type_TList :: T_Type ->
                  T_Type
sem_Type_TList valueType_ =
    (let _lhsOpretty :: Doc
         _lhsOself :: Type
         _valueTypeIpretty :: Doc
         _valueTypeIself :: Type
         _lhsOpretty =
             ({-# LINE 162 "src/Ag/Pretty.ag" #-}
              brackets _valueTypeIpretty
              {-# LINE 3632 "src/Ag/AG.hs" #-}
              )
         _self =
             TList _valueTypeIself
         _lhsOself =
             _self
         ( _valueTypeIpretty,_valueTypeIself) =
             valueType_
     in  ( _lhsOpretty,_lhsOself))
sem_Type_TMap :: T_Type ->
                 T_Type ->
                 T_Type
sem_Type_TMap keyType_ valueType_ =
    (let _lhsOpretty :: Doc
         _lhsOself :: Type
         _keyTypeIpretty :: Doc
         _keyTypeIself :: Type
         _valueTypeIpretty :: Doc
         _valueTypeIself :: Type
         _lhsOpretty =
             ({-# LINE 163 "src/Ag/Pretty.ag" #-}
              text "Map" <+>
              parens _keyTypeIpretty <+>
              parens _valueTypeIpretty
              {-# LINE 3656 "src/Ag/AG.hs" #-}
              )
         _self =
             TMap _keyTypeIself _valueTypeIself
         _lhsOself =
             _self
         ( _keyTypeIpretty,_keyTypeIself) =
             keyType_
         ( _valueTypeIpretty,_valueTypeIself) =
             valueType_
     in  ( _lhsOpretty,_lhsOself))
sem_Type_TTerminal :: String ->
                      T_Type
sem_Type_TTerminal typeName_ =
    (let _lhsOpretty :: Doc
         _lhsOself :: Type
         _lhsOpretty =
             ({-# LINE 166 "src/Ag/Pretty.ag" #-}
              text typeName_
              {-# LINE 3675 "src/Ag/AG.hs" #-}
              )
         _self =
             TTerminal typeName_
         _lhsOself =
             _self
     in  ( _lhsOpretty,_lhsOself))
sem_Type_TSort :: SortName ->
                  T_Type
sem_Type_TSort sortName_ =
    (let _lhsOpretty :: Doc
         _lhsOself :: Type
         _lhsOpretty =
             ({-# LINE 167 "src/Ag/Pretty.ag" #-}
              text (fromSN sortName_)
              {-# LINE 3690 "src/Ag/AG.hs" #-}
              )
         _self =
             TSort sortName_
         _lhsOself =
             _self
     in  ( _lhsOpretty,_lhsOself))