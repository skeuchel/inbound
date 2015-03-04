

-- UUAGC 0.9.51 (src/InBound/AG.ag)
module InBound.AG where
import InBound.Syntax
{-# LINE 4 "src/InBound/Elaboration/SubstSynthesis.ag" #-}

import Control.Monad
import Data.Maybe
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import InBound.Syntax
import qualified Ag.Syntax as Ag
{-# LINE 18 "src/InBound/AG.hs" #-}

{-# LINE 4 "src/InBound/Elaboration/SubstMap.ag" #-}

import Data.Maybe
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import InBound.Syntax
import qualified Ag.Syntax as Ag
{-# LINE 30 "src/InBound/AG.hs" #-}

{-# LINE 4 "src/InBound/Elaboration/RenameSynthesis.ag" #-}

import Data.Maybe
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import InBound.Syntax
import qualified Ag.Syntax as Ag
{-# LINE 42 "src/InBound/AG.hs" #-}

{-# LINE 4 "src/InBound/Elaboration/RenameMap.ag" #-}

import Data.Maybe
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import InBound.Syntax
import qualified Ag.Syntax as Ag
{-# LINE 54 "src/InBound/AG.hs" #-}

{-# LINE 4 "src/InBound/Elaboration/RenameContext.ag" #-}

import Data.Maybe
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import InBound.Syntax
import qualified Ag.Syntax as Ag
{-# LINE 66 "src/InBound/AG.hs" #-}

{-# LINE 4 "src/InBound/Elaboration/FreeVar.ag" #-}

import Data.Maybe
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import InBound.Syntax
import qualified Ag.Syntax as Ag
{-# LINE 78 "src/InBound/AG.hs" #-}

{-# LINE 4 "src/InBound/Elaboration/Context.ag" #-}

import Data.Maybe
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import InBound.Syntax
import qualified Ag.Syntax as Ag
{-# LINE 90 "src/InBound/AG.hs" #-}

{-# LINE 4 "src/InBound/Elaboration/Term.ag" #-}

import Data.Maybe
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import InBound.Syntax
import qualified Ag.Syntax as Ag
{-# LINE 102 "src/InBound/AG.hs" #-}

{-# LINE 4 "src/InBound/Environment.ag" #-}

import Control.Monad
import Data.Maybe
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import InBound.Syntax
{-# LINE 114 "src/InBound/AG.hs" #-}

{-# LINE 10 "src/InBound/Syntax.ag" #-}

import InBound.Syntax.Core
{-# LINE 119 "src/InBound/AG.hs" #-}
{-# LINE 16 "src/InBound/Elaboration/SubstSynthesis.ag" #-}

substAN :: AttrName
substAN = AN "subst"

substRef :: Ag.AttrRef
substRef = Ag.AttrRef Ag.Lhs substAN
{-# LINE 127 "src/InBound/AG.hs" #-}

{-# LINE 15 "src/InBound/Elaboration/SubstMap.ag" #-}

attrNameToSubstMapName :: AttrName -> AttrName
attrNameToSubstMapName (AN an) = AN $ an ++ "_sub"

attrRefToSubstMapRef :: AttrRef -> Ag.AttrRef
attrRefToSubstMapRef (AttrRef nl an) =
  Ag.AttrRef (elabNodeLabel nl) (attrNameToSubstMapName an)
{-# LINE 137 "src/InBound/AG.hs" #-}

{-# LINE 15 "src/InBound/Elaboration/RenameSynthesis.ag" #-}

renameAN :: AttrName
renameAN = AN "rename"

renameRef :: Ag.AttrRef
renameRef = Ag.AttrRef Ag.Loc renameAN
{-# LINE 146 "src/InBound/AG.hs" #-}

{-# LINE 15 "src/InBound/Elaboration/RenameMap.ag" #-}

attrNameToRenameMapName :: AttrName -> AttrName
attrNameToRenameMapName (AN an) = AN $ an ++ "_ren"

attrRefToRenameMapRef :: AttrRef -> Ag.AttrRef
attrRefToRenameMapRef (AttrRef nl an) =
  Ag.AttrRef (elabNodeLabel nl) (attrNameToRenameMapName an)
{-# LINE 156 "src/InBound/AG.hs" #-}

{-# LINE 15 "src/InBound/Elaboration/RenameContext.ag" #-}

attrNameToRenameContextName :: AttrName -> AttrName
attrNameToRenameContextName (AN an) = AN $ an ++ "_rctx"

attrRefToRenameContextRef :: AttrRef -> Ag.AttrRef
attrRefToRenameContextRef (AttrRef nl an) =
  Ag.AttrRef (elabNodeLabel nl) (attrNameToRenameContextName an)

renamedFieldRef :: FieldName -> Ag.AttrRef
renamedFieldRef (FN fn) =
  Ag.AttrRef (elabNodeLabel Loc) (AN $ fn ++ "_ren")
{-# LINE 170 "src/InBound/AG.hs" #-}

{-# LINE 15 "src/InBound/Elaboration/FreeVar.ag" #-}

attrNameToFreeVarName :: AttrName -> AttrName
attrNameToFreeVarName (AN an) = AN $ an ++ "_fv"

attrRefToFreeVarRef :: AttrRef -> Ag.AttrRef
attrRefToFreeVarRef (AttrRef nl an) =
  Ag.AttrRef (elabNodeLabel nl) (attrNameToFreeVarName an)
{-# LINE 180 "src/InBound/AG.hs" #-}

{-# LINE 15 "src/InBound/Elaboration/Context.ag" #-}

elabNodeLabel :: NodeLabel -> Ag.NodeLabel
elabNodeLabel Lhs      = Ag.Lhs
elabNodeLabel Loc      = Ag.Loc
elabNodeLabel (Sub fn) = Ag.Sub fn

attrRefToContextRef :: AttrRef -> Ag.AttrRef
attrRefToContextRef (AttrRef nl an) =
  Ag.AttrRef (elabNodeLabel nl) an
{-# LINE 192 "src/InBound/AG.hs" #-}

{-# LINE 15 "src/InBound/Elaboration/Term.ag" #-}

defaultValues :: Inh_Specification
defaultValues = (Inh_Specification {})

elaborateSpec :: Specification -> Ag.Specification
elaborateSpec spec =
  let sem = wrap_Specification (sem_Specification spec) defaultValues
  in elaboration_Syn_Specification sem
{-# LINE 203 "src/InBound/AG.hs" #-}

{-# LINE 192 "src/InBound/Environment.ag" #-}

localNodeLabel :: NodeLabel -> String
localNodeLabel Lhs      = "lhs_"
localNodeLabel Loc      = "loc_"
localNodeLabel (Sub fn) = fromFN fn ++ "_"

localAttrRef :: AttrRef -> Int -> AttrRef
localAttrRef (AttrRef nl (AN an)) depth =
  AttrRef Loc
    (AN $ localNodeLabel nl ++ an ++ "_" ++ show depth)
{-# LINE 216 "src/InBound/AG.hs" #-}
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
                  ( (Ag.AttrDecls),(Ag.AttrNameTypes),(Ag.AttrNameTypes),(Ag.AttrNameTypes),(Ag.AttrNameTypes),(Map AttrName Type),(Map AttrName Type),AttrDecl,(Ag.AttrNameTypes),(Ag.AttrNameTypes))
data Inh_AttrDecl = Inh_AttrDecl {envSort_Inh_AttrDecl :: SortName,envVars_Inh_AttrDecl :: NamespaceNames,namespaces_Inh_AttrDecl :: (Map NamespaceName MbSortName),varsorts_Inh_AttrDecl :: (Map SortName [NamespaceName])}
data Syn_AttrDecl = Syn_AttrDecl {elaboration_Syn_AttrDecl :: (Ag.AttrDecls),freeVarFunInh_Syn_AttrDecl :: (Ag.AttrNameTypes),freeVarFunSyn_Syn_AttrDecl :: (Ag.AttrNameTypes),renameFunInh_Syn_AttrDecl :: (Ag.AttrNameTypes),renameFunSyn_Syn_AttrDecl :: (Ag.AttrNameTypes),sAttrInh_Syn_AttrDecl :: (Map AttrName Type),sAttrSyn_Syn_AttrDecl :: (Map AttrName Type),self_Syn_AttrDecl :: AttrDecl,substFunInh_Syn_AttrDecl :: (Ag.AttrNameTypes),substFunSyn_Syn_AttrDecl :: (Ag.AttrNameTypes)}
wrap_AttrDecl :: T_AttrDecl ->
                 Inh_AttrDecl ->
                 Syn_AttrDecl
wrap_AttrDecl sem (Inh_AttrDecl _lhsIenvSort _lhsIenvVars _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOelaboration,_lhsOfreeVarFunInh,_lhsOfreeVarFunSyn,_lhsOrenameFunInh,_lhsOrenameFunSyn,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself,_lhsOsubstFunInh,_lhsOsubstFunSyn) = sem _lhsIenvSort _lhsIenvVars _lhsInamespaces _lhsIvarsorts
     in  (Syn_AttrDecl _lhsOelaboration _lhsOfreeVarFunInh _lhsOfreeVarFunSyn _lhsOrenameFunInh _lhsOrenameFunSyn _lhsOsAttrInh _lhsOsAttrSyn _lhsOself _lhsOsubstFunInh _lhsOsubstFunSyn))
sem_AttrDecl_SynDecl :: AttrName ->
                        T_Type ->
                        T_AttrDecl
sem_AttrDecl_SynDecl attrName_ attrType_ =
    (\ _lhsIenvSort
       _lhsIenvVars
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOelaboration :: (Ag.AttrDecls)
              _lhsOfreeVarFunInh :: (Ag.AttrNameTypes)
              _lhsOrenameFunSyn :: (Ag.AttrNameTypes)
              _lhsOsubstFunSyn :: (Ag.AttrNameTypes)
              _lhsOsAttrSyn :: (Map AttrName Type)
              _lhsOfreeVarFunSyn :: (Ag.AttrNameTypes)
              _lhsOrenameFunInh :: (Ag.AttrNameTypes)
              _lhsOsAttrInh :: (Map AttrName Type)
              _lhsOsubstFunInh :: (Ag.AttrNameTypes)
              _lhsOself :: AttrDecl
              _attrTypeIself :: Type
              _lhsOelaboration =
                  ({-# LINE 32 "src/InBound/Elaboration/Context.ag" #-}
                   foldr ($) _elaboration_augmented_syn [_elaboration_augmented_f1, _elaboration_augmented_f2, _elaboration_augmented_f3, _elaboration_augmented_f4, _elaboration_augmented_f5]
                   {-# LINE 261 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_f1 =
                  ({-# LINE 32 "src/InBound/Elaboration/Context.ag" #-}
                   [Ag.SynDecl attrName_ _contextType    ] ++
                   {-# LINE 266 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_f2 =
                  ({-# LINE 32 "src/InBound/Elaboration/Context.ag" #-}
                   [ Ag.InhDecl
                     _freeVarAName
                     _freeVarType
                   ] ++
                   {-# LINE 274 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_f3 =
                  ({-# LINE 32 "src/InBound/Elaboration/Context.ag" #-}
                   [ Ag.SynDecl
                       (attrNameToRenameContextName attrName_)
                       _renameContextType
                   ] ++
                   {-# LINE 282 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_f4 =
                  ({-# LINE 32 "src/InBound/Elaboration/Context.ag" #-}
                   [ Ag.SynDecl
                       (attrNameToRenameMapName attrName_)
                       _renameMapType
                   ] ++
                   {-# LINE 290 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_f5 =
                  ({-# LINE 32 "src/InBound/Elaboration/Context.ag" #-}
                   if _substitutable
                   then ([ _agdecl
                             (attrNameToSubstMapName attrName_)
                             _substMapType
                         ] ++)
                   else id
                   {-# LINE 300 "src/InBound/AG.hs" #-}
                   )
              _lhsOfreeVarFunInh =
                  ({-# LINE 108 "src/InBound/Elaboration/FreeVar.ag" #-}
                   foldr ($) _freeVarFunInh_augmented_syn [_freeVarFunInh_augmented_f1]
                   {-# LINE 305 "src/InBound/AG.hs" #-}
                   )
              _freeVarFunInh_augmented_f1 =
                  ({-# LINE 108 "src/InBound/Elaboration/FreeVar.ag" #-}
                   (_freeVarAName    ,_freeVarType    ) :
                   {-# LINE 310 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameFunSyn =
                  ({-# LINE 85 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   foldr ($) _renameFunSyn_augmented_syn [_renameFunSyn_augmented_f1]
                   {-# LINE 315 "src/InBound/AG.hs" #-}
                   )
              _renameFunSyn_augmented_f1 =
                  ({-# LINE 85 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   _renameFun     ++
                   {-# LINE 320 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstFunSyn =
                  ({-# LINE 97 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   foldr ($) _substFunSyn_augmented_syn [_substFunSyn_augmented_f1]
                   {-# LINE 325 "src/InBound/AG.hs" #-}
                   )
              _substFunSyn_augmented_f1 =
                  ({-# LINE 97 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   (_renameFun     ++ _substFun    ) ++
                   {-# LINE 330 "src/InBound/AG.hs" #-}
                   )
              _substFun =
                  ({-# LINE 93 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   if _substitutable
                   then [(attrNameToSubstMapName attrName_, _substMapType    )]
                   else []
                   {-# LINE 337 "src/InBound/AG.hs" #-}
                   )
              _agdecl =
                  ({-# LINE 27 "src/InBound/Elaboration/SubstMap.ag" #-}
                   Ag.SynDecl
                   {-# LINE 342 "src/InBound/AG.hs" #-}
                   )
              _namespace =
                  ({-# LINE 31 "src/InBound/Elaboration/SubstMap.ag" #-}
                   case _attrTypeIself of
                     Context ns -> ns
                   {-# LINE 348 "src/InBound/AG.hs" #-}
                   )
              _mbsort =
                  ({-# LINE 33 "src/InBound/Elaboration/SubstMap.ag" #-}
                   join $ M.lookup _namespace     _lhsInamespaces
                   {-# LINE 353 "src/InBound/AG.hs" #-}
                   )
              _substitutable =
                  ({-# LINE 34 "src/InBound/Elaboration/SubstMap.ag" #-}
                   isJust _mbsort
                   {-# LINE 358 "src/InBound/AG.hs" #-}
                   )
              _sort =
                  ({-# LINE 35 "src/InBound/Elaboration/SubstMap.ag" #-}
                   fromJust _mbsort
                   {-# LINE 363 "src/InBound/AG.hs" #-}
                   )
              _substMapType =
                  ({-# LINE 36 "src/InBound/Elaboration/SubstMap.ag" #-}
                   Ag.TMap
                     (Ag.TTerminal $ fromNN _namespace    )
                     (Ag.TSort _sort    )
                   {-# LINE 370 "src/InBound/AG.hs" #-}
                   )
              _renameFun =
                  ({-# LINE 80 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   [ (attrNameToRenameContextName attrName_,
                      _renameContextType    )
                   , (attrNameToRenameMapName attrName_,
                      _renameMapType    )
                   ]
                   {-# LINE 379 "src/InBound/AG.hs" #-}
                   )
              _renameMapType =
                  ({-# LINE 27 "src/InBound/Elaboration/RenameMap.ag" #-}
                   case _attrTypeIself of
                     Context ns -> Ag.TMap
                                     (Ag.TTerminal (fromNN ns))
                                     (Ag.TTerminal (fromNN ns))
                   {-# LINE 387 "src/InBound/AG.hs" #-}
                   )
              _renameContextType =
                  ({-# LINE 31 "src/InBound/Elaboration/RenameContext.ag" #-}
                   case _attrTypeIself of
                     Context ns -> Ag.TSet (Ag.TTerminal (fromNN ns))
                   {-# LINE 393 "src/InBound/AG.hs" #-}
                   )
              _freeVarType =
                  ({-# LINE 26 "src/InBound/Elaboration/FreeVar.ag" #-}
                   case _attrTypeIself of
                     Context ns -> Ag.TSet (Ag.TTerminal (fromNN ns))
                   {-# LINE 399 "src/InBound/AG.hs" #-}
                   )
              _freeVarAName =
                  ({-# LINE 29 "src/InBound/Elaboration/FreeVar.ag" #-}
                   attrNameToFreeVarName attrName_
                   {-# LINE 404 "src/InBound/AG.hs" #-}
                   )
              _contextType =
                  ({-# LINE 29 "src/InBound/Elaboration/Context.ag" #-}
                   case _attrTypeIself of
                     Context ns -> Ag.TSet (Ag.TTerminal (fromNN ns))
                   {-# LINE 410 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 99 "src/InBound/Environment.ag" #-}
                   M.singleton attrName_ _attrTypeIself
                   {-# LINE 415 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_syn =
                  ({-# LINE 32 "src/InBound/Elaboration/Context.ag" #-}
                   []
                   {-# LINE 420 "src/InBound/AG.hs" #-}
                   )
              _freeVarFunInh_augmented_syn =
                  ({-# LINE 108 "src/InBound/Elaboration/FreeVar.ag" #-}
                   []
                   {-# LINE 425 "src/InBound/AG.hs" #-}
                   )
              _lhsOfreeVarFunSyn =
                  ({-# LINE 104 "src/InBound/Elaboration/FreeVar.ag" #-}
                   []
                   {-# LINE 430 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameFunInh =
                  ({-# LINE 75 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   []
                   {-# LINE 435 "src/InBound/AG.hs" #-}
                   )
              _renameFunSyn_augmented_syn =
                  ({-# LINE 85 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   []
                   {-# LINE 440 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 77 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 445 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstFunInh =
                  ({-# LINE 88 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   []
                   {-# LINE 450 "src/InBound/AG.hs" #-}
                   )
              _substFunSyn_augmented_syn =
                  ({-# LINE 97 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   []
                   {-# LINE 455 "src/InBound/AG.hs" #-}
                   )
              _self =
                  SynDecl attrName_ _attrTypeIself
              _lhsOself =
                  _self
              ( _attrTypeIself) =
                  attrType_
          in  ( _lhsOelaboration,_lhsOfreeVarFunInh,_lhsOfreeVarFunSyn,_lhsOrenameFunInh,_lhsOrenameFunSyn,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself,_lhsOsubstFunInh,_lhsOsubstFunSyn)))
sem_AttrDecl_InhDecl :: AttrName ->
                        T_Type ->
                        T_AttrDecl
sem_AttrDecl_InhDecl attrName_ attrType_ =
    (\ _lhsIenvSort
       _lhsIenvVars
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOelaboration :: (Ag.AttrDecls)
              _lhsOfreeVarFunSyn :: (Ag.AttrNameTypes)
              _lhsOrenameFunInh :: (Ag.AttrNameTypes)
              _lhsOsubstFunInh :: (Ag.AttrNameTypes)
              _lhsOsAttrInh :: (Map AttrName Type)
              _lhsOfreeVarFunInh :: (Ag.AttrNameTypes)
              _lhsOrenameFunSyn :: (Ag.AttrNameTypes)
              _lhsOsAttrSyn :: (Map AttrName Type)
              _lhsOsubstFunSyn :: (Ag.AttrNameTypes)
              _lhsOself :: AttrDecl
              _attrTypeIself :: Type
              _lhsOelaboration =
                  ({-# LINE 37 "src/InBound/Elaboration/Context.ag" #-}
                   foldr ($) _elaboration_augmented_syn [_elaboration_augmented_f1, _elaboration_augmented_f2, _elaboration_augmented_f3, _elaboration_augmented_f4, _elaboration_augmented_f5]
                   {-# LINE 486 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_f1 =
                  ({-# LINE 37 "src/InBound/Elaboration/Context.ag" #-}
                   [Ag.InhDecl attrName_ _contextType    ] ++
                   {-# LINE 491 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_f2 =
                  ({-# LINE 37 "src/InBound/Elaboration/Context.ag" #-}
                   [ Ag.SynDecl
                     _freeVarAName
                     _freeVarType
                   ] ++
                   {-# LINE 499 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_f3 =
                  ({-# LINE 37 "src/InBound/Elaboration/Context.ag" #-}
                   [ Ag.InhDecl
                       (attrNameToRenameContextName attrName_)
                       _renameContextType
                   ] ++
                   {-# LINE 507 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_f4 =
                  ({-# LINE 37 "src/InBound/Elaboration/Context.ag" #-}
                   [ Ag.InhDecl
                       (attrNameToRenameMapName attrName_)
                       _renameMapType
                   ] ++
                   {-# LINE 515 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_f5 =
                  ({-# LINE 37 "src/InBound/Elaboration/Context.ag" #-}
                   if _substitutable
                   then ([ _agdecl
                             (attrNameToSubstMapName attrName_)
                             _substMapType
                         ] ++)
                   else id
                   {-# LINE 525 "src/InBound/AG.hs" #-}
                   )
              _lhsOfreeVarFunSyn =
                  ({-# LINE 110 "src/InBound/Elaboration/FreeVar.ag" #-}
                   foldr ($) _freeVarFunSyn_augmented_syn [_freeVarFunSyn_augmented_f1]
                   {-# LINE 530 "src/InBound/AG.hs" #-}
                   )
              _freeVarFunSyn_augmented_f1 =
                  ({-# LINE 110 "src/InBound/Elaboration/FreeVar.ag" #-}
                   (_freeVarAName    ,_freeVarType    ) :
                   {-# LINE 535 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameFunInh =
                  ({-# LINE 92 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   foldr ($) _renameFunInh_augmented_syn [_renameFunInh_augmented_f1]
                   {-# LINE 540 "src/InBound/AG.hs" #-}
                   )
              _renameFunInh_augmented_f1 =
                  ({-# LINE 92 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   _renameFun     ++
                   {-# LINE 545 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstFunInh =
                  ({-# LINE 103 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   foldr ($) _substFunInh_augmented_syn [_substFunInh_augmented_f1]
                   {-# LINE 550 "src/InBound/AG.hs" #-}
                   )
              _substFunInh_augmented_f1 =
                  ({-# LINE 103 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   (_renameFun     ++ _substFun    ) ++
                   {-# LINE 555 "src/InBound/AG.hs" #-}
                   )
              _substFun =
                  ({-# LINE 99 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   if _substitutable
                   then [(attrNameToSubstMapName attrName_, _substMapType    )]
                   else []
                   {-# LINE 562 "src/InBound/AG.hs" #-}
                   )
              _agdecl =
                  ({-# LINE 29 "src/InBound/Elaboration/SubstMap.ag" #-}
                   Ag.InhDecl
                   {-# LINE 567 "src/InBound/AG.hs" #-}
                   )
              _namespace =
                  ({-# LINE 31 "src/InBound/Elaboration/SubstMap.ag" #-}
                   case _attrTypeIself of
                     Context ns -> ns
                   {-# LINE 573 "src/InBound/AG.hs" #-}
                   )
              _mbsort =
                  ({-# LINE 33 "src/InBound/Elaboration/SubstMap.ag" #-}
                   join $ M.lookup _namespace     _lhsInamespaces
                   {-# LINE 578 "src/InBound/AG.hs" #-}
                   )
              _substitutable =
                  ({-# LINE 34 "src/InBound/Elaboration/SubstMap.ag" #-}
                   isJust _mbsort
                   {-# LINE 583 "src/InBound/AG.hs" #-}
                   )
              _sort =
                  ({-# LINE 35 "src/InBound/Elaboration/SubstMap.ag" #-}
                   fromJust _mbsort
                   {-# LINE 588 "src/InBound/AG.hs" #-}
                   )
              _substMapType =
                  ({-# LINE 36 "src/InBound/Elaboration/SubstMap.ag" #-}
                   Ag.TMap
                     (Ag.TTerminal $ fromNN _namespace    )
                     (Ag.TSort _sort    )
                   {-# LINE 595 "src/InBound/AG.hs" #-}
                   )
              _renameFun =
                  ({-# LINE 87 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   [ (attrNameToRenameContextName attrName_,
                     _renameContextType    )
                   , (attrNameToRenameMapName attrName_,
                      _renameMapType    )
                   ]
                   {-# LINE 604 "src/InBound/AG.hs" #-}
                   )
              _renameMapType =
                  ({-# LINE 27 "src/InBound/Elaboration/RenameMap.ag" #-}
                   case _attrTypeIself of
                     Context ns -> Ag.TMap
                                     (Ag.TTerminal (fromNN ns))
                                     (Ag.TTerminal (fromNN ns))
                   {-# LINE 612 "src/InBound/AG.hs" #-}
                   )
              _renameContextType =
                  ({-# LINE 31 "src/InBound/Elaboration/RenameContext.ag" #-}
                   case _attrTypeIself of
                     Context ns -> Ag.TSet (Ag.TTerminal (fromNN ns))
                   {-# LINE 618 "src/InBound/AG.hs" #-}
                   )
              _freeVarType =
                  ({-# LINE 36 "src/InBound/Elaboration/FreeVar.ag" #-}
                   case _attrTypeIself of
                     Context ns -> Ag.TSet (Ag.TTerminal (fromNN ns))
                   {-# LINE 624 "src/InBound/AG.hs" #-}
                   )
              _freeVarAName =
                  ({-# LINE 39 "src/InBound/Elaboration/FreeVar.ag" #-}
                   attrNameToFreeVarName attrName_
                   {-# LINE 629 "src/InBound/AG.hs" #-}
                   )
              _contextType =
                  ({-# LINE 34 "src/InBound/Elaboration/Context.ag" #-}
                   case _attrTypeIself of
                     Context ns -> Ag.TSet (Ag.TTerminal (fromNN ns))
                   {-# LINE 635 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 100 "src/InBound/Environment.ag" #-}
                   M.singleton attrName_ _attrTypeIself
                   {-# LINE 640 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_syn =
                  ({-# LINE 37 "src/InBound/Elaboration/Context.ag" #-}
                   []
                   {-# LINE 645 "src/InBound/AG.hs" #-}
                   )
              _lhsOfreeVarFunInh =
                  ({-# LINE 103 "src/InBound/Elaboration/FreeVar.ag" #-}
                   []
                   {-# LINE 650 "src/InBound/AG.hs" #-}
                   )
              _freeVarFunSyn_augmented_syn =
                  ({-# LINE 110 "src/InBound/Elaboration/FreeVar.ag" #-}
                   []
                   {-# LINE 655 "src/InBound/AG.hs" #-}
                   )
              _renameFunInh_augmented_syn =
                  ({-# LINE 92 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   []
                   {-# LINE 660 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameFunSyn =
                  ({-# LINE 76 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   []
                   {-# LINE 665 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 76 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 670 "src/InBound/AG.hs" #-}
                   )
              _substFunInh_augmented_syn =
                  ({-# LINE 103 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   []
                   {-# LINE 675 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstFunSyn =
                  ({-# LINE 89 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   []
                   {-# LINE 680 "src/InBound/AG.hs" #-}
                   )
              _self =
                  InhDecl attrName_ _attrTypeIself
              _lhsOself =
                  _self
              ( _attrTypeIself) =
                  attrType_
          in  ( _lhsOelaboration,_lhsOfreeVarFunInh,_lhsOfreeVarFunSyn,_lhsOrenameFunInh,_lhsOrenameFunSyn,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself,_lhsOsubstFunInh,_lhsOsubstFunSyn)))
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
                   ( (Ag.AttrDecls),(Ag.AttrNameTypes),(Ag.AttrNameTypes),(Ag.AttrNameTypes),(Ag.AttrNameTypes),(Map AttrName Type),(Map AttrName Type),AttrDecls,(Ag.AttrNameTypes),(Ag.AttrNameTypes))
data Inh_AttrDecls = Inh_AttrDecls {envSort_Inh_AttrDecls :: SortName,envVars_Inh_AttrDecls :: NamespaceNames,namespaces_Inh_AttrDecls :: (Map NamespaceName MbSortName),varsorts_Inh_AttrDecls :: (Map SortName [NamespaceName])}
data Syn_AttrDecls = Syn_AttrDecls {elaboration_Syn_AttrDecls :: (Ag.AttrDecls),freeVarFunInh_Syn_AttrDecls :: (Ag.AttrNameTypes),freeVarFunSyn_Syn_AttrDecls :: (Ag.AttrNameTypes),renameFunInh_Syn_AttrDecls :: (Ag.AttrNameTypes),renameFunSyn_Syn_AttrDecls :: (Ag.AttrNameTypes),sAttrInh_Syn_AttrDecls :: (Map AttrName Type),sAttrSyn_Syn_AttrDecls :: (Map AttrName Type),self_Syn_AttrDecls :: AttrDecls,substFunInh_Syn_AttrDecls :: (Ag.AttrNameTypes),substFunSyn_Syn_AttrDecls :: (Ag.AttrNameTypes)}
wrap_AttrDecls :: T_AttrDecls ->
                  Inh_AttrDecls ->
                  Syn_AttrDecls
wrap_AttrDecls sem (Inh_AttrDecls _lhsIenvSort _lhsIenvVars _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOelaboration,_lhsOfreeVarFunInh,_lhsOfreeVarFunSyn,_lhsOrenameFunInh,_lhsOrenameFunSyn,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself,_lhsOsubstFunInh,_lhsOsubstFunSyn) = sem _lhsIenvSort _lhsIenvVars _lhsInamespaces _lhsIvarsorts
     in  (Syn_AttrDecls _lhsOelaboration _lhsOfreeVarFunInh _lhsOfreeVarFunSyn _lhsOrenameFunInh _lhsOrenameFunSyn _lhsOsAttrInh _lhsOsAttrSyn _lhsOself _lhsOsubstFunInh _lhsOsubstFunSyn))
sem_AttrDecls_Cons :: T_AttrDecl ->
                      T_AttrDecls ->
                      T_AttrDecls
sem_AttrDecls_Cons hd_ tl_ =
    (\ _lhsIenvSort
       _lhsIenvVars
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOelaboration :: (Ag.AttrDecls)
              _lhsOfreeVarFunInh :: (Ag.AttrNameTypes)
              _lhsOfreeVarFunSyn :: (Ag.AttrNameTypes)
              _lhsOrenameFunInh :: (Ag.AttrNameTypes)
              _lhsOrenameFunSyn :: (Ag.AttrNameTypes)
              _lhsOsAttrInh :: (Map AttrName Type)
              _lhsOsAttrSyn :: (Map AttrName Type)
              _lhsOsubstFunInh :: (Ag.AttrNameTypes)
              _lhsOsubstFunSyn :: (Ag.AttrNameTypes)
              _lhsOself :: AttrDecls
              _hdOenvSort :: SortName
              _hdOenvVars :: NamespaceNames
              _hdOnamespaces :: (Map NamespaceName MbSortName)
              _hdOvarsorts :: (Map SortName [NamespaceName])
              _tlOenvSort :: SortName
              _tlOenvVars :: NamespaceNames
              _tlOnamespaces :: (Map NamespaceName MbSortName)
              _tlOvarsorts :: (Map SortName [NamespaceName])
              _hdIelaboration :: (Ag.AttrDecls)
              _hdIfreeVarFunInh :: (Ag.AttrNameTypes)
              _hdIfreeVarFunSyn :: (Ag.AttrNameTypes)
              _hdIrenameFunInh :: (Ag.AttrNameTypes)
              _hdIrenameFunSyn :: (Ag.AttrNameTypes)
              _hdIsAttrInh :: (Map AttrName Type)
              _hdIsAttrSyn :: (Map AttrName Type)
              _hdIself :: AttrDecl
              _hdIsubstFunInh :: (Ag.AttrNameTypes)
              _hdIsubstFunSyn :: (Ag.AttrNameTypes)
              _tlIelaboration :: (Ag.AttrDecls)
              _tlIfreeVarFunInh :: (Ag.AttrNameTypes)
              _tlIfreeVarFunSyn :: (Ag.AttrNameTypes)
              _tlIrenameFunInh :: (Ag.AttrNameTypes)
              _tlIrenameFunSyn :: (Ag.AttrNameTypes)
              _tlIsAttrInh :: (Map AttrName Type)
              _tlIsAttrSyn :: (Map AttrName Type)
              _tlIself :: AttrDecls
              _tlIsubstFunInh :: (Ag.AttrNameTypes)
              _tlIsubstFunSyn :: (Ag.AttrNameTypes)
              _lhsOelaboration =
                  ({-# LINE 69 "src/InBound/Elaboration/Term.ag" #-}
                   _hdIelaboration ++ _tlIelaboration
                   {-# LINE 758 "src/InBound/AG.hs" #-}
                   )
              _lhsOfreeVarFunInh =
                  ({-# LINE 103 "src/InBound/Elaboration/FreeVar.ag" #-}
                   _hdIfreeVarFunInh ++ _tlIfreeVarFunInh
                   {-# LINE 763 "src/InBound/AG.hs" #-}
                   )
              _lhsOfreeVarFunSyn =
                  ({-# LINE 104 "src/InBound/Elaboration/FreeVar.ag" #-}
                   _hdIfreeVarFunSyn ++ _tlIfreeVarFunSyn
                   {-# LINE 768 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameFunInh =
                  ({-# LINE 75 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   _hdIrenameFunInh ++ _tlIrenameFunInh
                   {-# LINE 773 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameFunSyn =
                  ({-# LINE 76 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   _hdIrenameFunSyn ++ _tlIrenameFunSyn
                   {-# LINE 778 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 77 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsAttrInh _tlIsAttrInh)
                   {-# LINE 783 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 76 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsAttrSyn _tlIsAttrSyn)
                   {-# LINE 788 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstFunInh =
                  ({-# LINE 88 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   _hdIsubstFunInh ++ _tlIsubstFunInh
                   {-# LINE 793 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstFunSyn =
                  ({-# LINE 89 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   _hdIsubstFunSyn ++ _tlIsubstFunSyn
                   {-# LINE 798 "src/InBound/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 807 "src/InBound/AG.hs" #-}
                   )
              _hdOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 812 "src/InBound/AG.hs" #-}
                   )
              _hdOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 817 "src/InBound/AG.hs" #-}
                   )
              _hdOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 822 "src/InBound/AG.hs" #-}
                   )
              _tlOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 827 "src/InBound/AG.hs" #-}
                   )
              _tlOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 832 "src/InBound/AG.hs" #-}
                   )
              _tlOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 837 "src/InBound/AG.hs" #-}
                   )
              _tlOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 842 "src/InBound/AG.hs" #-}
                   )
              ( _hdIelaboration,_hdIfreeVarFunInh,_hdIfreeVarFunSyn,_hdIrenameFunInh,_hdIrenameFunSyn,_hdIsAttrInh,_hdIsAttrSyn,_hdIself,_hdIsubstFunInh,_hdIsubstFunSyn) =
                  hd_ _hdOenvSort _hdOenvVars _hdOnamespaces _hdOvarsorts
              ( _tlIelaboration,_tlIfreeVarFunInh,_tlIfreeVarFunSyn,_tlIrenameFunInh,_tlIrenameFunSyn,_tlIsAttrInh,_tlIsAttrSyn,_tlIself,_tlIsubstFunInh,_tlIsubstFunSyn) =
                  tl_ _tlOenvSort _tlOenvVars _tlOnamespaces _tlOvarsorts
          in  ( _lhsOelaboration,_lhsOfreeVarFunInh,_lhsOfreeVarFunSyn,_lhsOrenameFunInh,_lhsOrenameFunSyn,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself,_lhsOsubstFunInh,_lhsOsubstFunSyn)))
sem_AttrDecls_Nil :: T_AttrDecls
sem_AttrDecls_Nil =
    (\ _lhsIenvSort
       _lhsIenvVars
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOelaboration :: (Ag.AttrDecls)
              _lhsOfreeVarFunInh :: (Ag.AttrNameTypes)
              _lhsOfreeVarFunSyn :: (Ag.AttrNameTypes)
              _lhsOrenameFunInh :: (Ag.AttrNameTypes)
              _lhsOrenameFunSyn :: (Ag.AttrNameTypes)
              _lhsOsAttrInh :: (Map AttrName Type)
              _lhsOsAttrSyn :: (Map AttrName Type)
              _lhsOsubstFunInh :: (Ag.AttrNameTypes)
              _lhsOsubstFunSyn :: (Ag.AttrNameTypes)
              _lhsOself :: AttrDecls
              _lhsOelaboration =
                  ({-# LINE 69 "src/InBound/Elaboration/Term.ag" #-}
                   []
                   {-# LINE 868 "src/InBound/AG.hs" #-}
                   )
              _lhsOfreeVarFunInh =
                  ({-# LINE 103 "src/InBound/Elaboration/FreeVar.ag" #-}
                   []
                   {-# LINE 873 "src/InBound/AG.hs" #-}
                   )
              _lhsOfreeVarFunSyn =
                  ({-# LINE 104 "src/InBound/Elaboration/FreeVar.ag" #-}
                   []
                   {-# LINE 878 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameFunInh =
                  ({-# LINE 75 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   []
                   {-# LINE 883 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameFunSyn =
                  ({-# LINE 76 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   []
                   {-# LINE 888 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 77 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 893 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 76 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 898 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstFunInh =
                  ({-# LINE 88 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   []
                   {-# LINE 903 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstFunSyn =
                  ({-# LINE 89 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   []
                   {-# LINE 908 "src/InBound/AG.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOelaboration,_lhsOfreeVarFunInh,_lhsOfreeVarFunSyn,_lhsOrenameFunInh,_lhsOrenameFunSyn,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself,_lhsOsubstFunInh,_lhsOsubstFunSyn)))
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
                 ( (Map AttrRef [Ag.Expr]),(Ag.AttrDefs),(Map AttrRef Expr),AttrDef)
data Inh_AttrDef = Inh_AttrDef {envAtom_Inh_AttrDef :: (Map FieldName NamespaceName),envAttrInh_Inh_AttrDef :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_AttrDef :: (Map (SortName,AttrName) Type),envField_Inh_AttrDef :: (Map FieldName SortName),envSetAttrDef_Inh_AttrDef :: (Map AttrRef Type),envSetAttrUse_Inh_AttrDef :: (Map AttrRef Type),envSort_Inh_AttrDef :: SortName,envVars_Inh_AttrDef :: NamespaceNames,locEnvAttrInh_Inh_AttrDef :: (Map AttrName Type),locEnvAttrSyn_Inh_AttrDef :: (Map AttrName Type),namespaces_Inh_AttrDef :: (Map NamespaceName MbSortName),varsorts_Inh_AttrDef :: (Map SortName [NamespaceName])}
data Syn_AttrDef = Syn_AttrDef {elabFreeVar_Syn_AttrDef :: (Map AttrRef [Ag.Expr]),elaboration_Syn_AttrDef :: (Ag.AttrDefs),sSetAttrDef_Syn_AttrDef :: (Map AttrRef Expr),self_Syn_AttrDef :: AttrDef}
wrap_AttrDef :: T_AttrDef ->
                Inh_AttrDef ->
                Syn_AttrDef
wrap_AttrDef sem (Inh_AttrDef _lhsIenvAtom _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvField _lhsIenvSetAttrDef _lhsIenvSetAttrUse _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOelabFreeVar,_lhsOelaboration,_lhsOsSetAttrDef,_lhsOself) = sem _lhsIenvAtom _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvField _lhsIenvSetAttrDef _lhsIenvSetAttrUse _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIvarsorts
     in  (Syn_AttrDef _lhsOelabFreeVar _lhsOelaboration _lhsOsSetAttrDef _lhsOself))
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
         (let _lhsOelaboration :: (Ag.AttrDefs)
              _lhsOelabFreeVar :: (Map AttrRef [Ag.Expr])
              _lhsOsSetAttrDef :: (Map AttrRef Expr)
              _attrDefExprOattrDefRef :: AttrRef
              _attrDefExprOdepth :: Int
              _lhsOself :: AttrDef
              _attrDefExprOenvAtom :: (Map FieldName NamespaceName)
              _attrDefExprOenvAttrInh :: (Map (SortName,AttrName) Type)
              _attrDefExprOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _attrDefExprOenvField :: (Map FieldName SortName)
              _attrDefExprOenvSort :: SortName
              _attrDefExprOenvVars :: NamespaceNames
              _attrDefRefIself :: AttrRef
              _attrDefExprIattrName :: AttrRef
              _attrDefExprIelabContext :: ([Ag.AttrDef])
              _attrDefExprIelabFreeVar :: (Map AttrRef [Ag.Expr])
              _attrDefExprIelabRenameContext :: ([Ag.AttrDef])
              _attrDefExprIelabRenameMap :: ([Ag.AttrDef])
              _attrDefExprIelabSubstMap :: ([Ag.AttrDef])
              _attrDefExprIself :: Expr
              _lhsOelaboration =
                  ({-# LINE 42 "src/InBound/Elaboration/Context.ag" #-}
                   foldr ($) _elaboration_augmented_syn [_elaboration_augmented_f1, _elaboration_augmented_f2, _elaboration_augmented_f3, _elaboration_augmented_f4]
                   {-# LINE 982 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_f1 =
                  ({-# LINE 42 "src/InBound/Elaboration/Context.ag" #-}
                   (Ag.AttrDef
                    (attrRefToContextRef _attrDefRefIself)
                    (Ag.ExprAttrRef $
                       attrRefToContextRef _attrDefExprIattrName) :
                    _attrDefExprIelabContext) ++
                   {-# LINE 991 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_f2 =
                  ({-# LINE 42 "src/InBound/Elaboration/Context.ag" #-}
                   (Ag.AttrDef
                    (attrRefToRenameContextRef _attrDefRefIself)
                    (Ag.ExprAttrRef $
                       attrRefToRenameContextRef _attrDefExprIattrName) :
                    _attrDefExprIelabRenameContext) ++
                   {-# LINE 1000 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_f3 =
                  ({-# LINE 42 "src/InBound/Elaboration/Context.ag" #-}
                   (Ag.AttrDef
                    (attrRefToRenameMapRef _attrDefRefIself)
                    (Ag.ExprAttrRef $
                       attrRefToRenameMapRef _attrDefExprIattrName) :
                    _attrDefExprIelabRenameMap) ++
                   {-# LINE 1009 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_f4 =
                  ({-# LINE 42 "src/InBound/Elaboration/Context.ag" #-}
                   if _lhssubstitutable
                   then ((Ag.AttrDef
                            (attrRefToSubstMapRef _attrDefRefIself)
                            (Ag.ExprAttrRef $
                               attrRefToSubstMapRef _attrDefExprIattrName) :
                            _attrDefExprIelabSubstMap) ++)
                   else id
                   {-# LINE 1020 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabFreeVar =
                  ({-# LINE 69 "src/InBound/Elaboration/FreeVar.ag" #-}
                   M.insertWith (++)
                     _attrDefExprIattrName
                     [ (Ag.ExprAttrRef $
                          attrRefToFreeVarRef _attrDefRefIself)
                     ] _attrDefExprIelabFreeVar
                   {-# LINE 1029 "src/InBound/AG.hs" #-}
                   )
              _lhsOsSetAttrDef =
                  ({-# LINE 148 "src/InBound/Environment.ag" #-}
                   M.singleton
                     _attrDefRefIself
                     _attrDefExprIself
                   {-# LINE 1036 "src/InBound/AG.hs" #-}
                   )
              _attrDefExprOattrDefRef =
                  ({-# LINE 206 "src/InBound/Environment.ag" #-}
                   _attrDefRefIself
                   {-# LINE 1041 "src/InBound/AG.hs" #-}
                   )
              _attrDefExprOdepth =
                  ({-# LINE 207 "src/InBound/Environment.ag" #-}
                   0
                   {-# LINE 1046 "src/InBound/AG.hs" #-}
                   )
              _lhsnamespace =
                  ({-# LINE 208 "src/InBound/Environment.ag" #-}
                   case M.lookup _attrDefRefIself _lhsIenvSetAttrDef of
                     Just (Context ns) -> ns
                     Nothing           -> error "AttrDef namespace"
                   {-# LINE 1053 "src/InBound/AG.hs" #-}
                   )
              _lhsmbsort =
                  ({-# LINE 212 "src/InBound/Environment.ag" #-}
                   join $ M.lookup _lhsnamespace     _lhsInamespaces
                   {-# LINE 1058 "src/InBound/AG.hs" #-}
                   )
              _lhssubstitutable =
                  ({-# LINE 213 "src/InBound/Environment.ag" #-}
                   isJust _lhsmbsort
                   {-# LINE 1063 "src/InBound/AG.hs" #-}
                   )
              _lhssort =
                  ({-# LINE 214 "src/InBound/Environment.ag" #-}
                   case _lhsmbsort     of
                     Just s  -> s
                     Nothing -> error "AttrDef lhssort"
                   {-# LINE 1070 "src/InBound/AG.hs" #-}
                   )
              _elaboration_augmented_syn =
                  ({-# LINE 42 "src/InBound/Elaboration/Context.ag" #-}
                   []
                   {-# LINE 1075 "src/InBound/AG.hs" #-}
                   )
              _self =
                  AttrDef _attrDefRefIself _attrDefExprIself
              _lhsOself =
                  _self
              _attrDefExprOenvAtom =
                  ({-# LINE 62 "src/InBound/Environment.ag" #-}
                   _lhsIenvAtom
                   {-# LINE 1084 "src/InBound/AG.hs" #-}
                   )
              _attrDefExprOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1089 "src/InBound/AG.hs" #-}
                   )
              _attrDefExprOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1094 "src/InBound/AG.hs" #-}
                   )
              _attrDefExprOenvField =
                  ({-# LINE 61 "src/InBound/Environment.ag" #-}
                   _lhsIenvField
                   {-# LINE 1099 "src/InBound/AG.hs" #-}
                   )
              _attrDefExprOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1104 "src/InBound/AG.hs" #-}
                   )
              _attrDefExprOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 1109 "src/InBound/AG.hs" #-}
                   )
              ( _attrDefRefIself) =
                  attrDefRef_
              ( _attrDefExprIattrName,_attrDefExprIelabContext,_attrDefExprIelabFreeVar,_attrDefExprIelabRenameContext,_attrDefExprIelabRenameMap,_attrDefExprIelabSubstMap,_attrDefExprIself) =
                  attrDefExpr_ _attrDefExprOattrDefRef _attrDefExprOdepth _attrDefExprOenvAtom _attrDefExprOenvAttrInh _attrDefExprOenvAttrSyn _attrDefExprOenvField _attrDefExprOenvSort _attrDefExprOenvVars
          in  ( _lhsOelabFreeVar,_lhsOelaboration,_lhsOsSetAttrDef,_lhsOself)))
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
                  ( (Map AttrRef [Ag.Expr]),(Ag.AttrDefs),(Map AttrRef Expr),AttrDefs)
data Inh_AttrDefs = Inh_AttrDefs {envAtom_Inh_AttrDefs :: (Map FieldName NamespaceName),envAttrInh_Inh_AttrDefs :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_AttrDefs :: (Map (SortName,AttrName) Type),envField_Inh_AttrDefs :: (Map FieldName SortName),envSetAttrDef_Inh_AttrDefs :: (Map AttrRef Type),envSetAttrUse_Inh_AttrDefs :: (Map AttrRef Type),envSort_Inh_AttrDefs :: SortName,envVars_Inh_AttrDefs :: NamespaceNames,locEnvAttrInh_Inh_AttrDefs :: (Map AttrName Type),locEnvAttrSyn_Inh_AttrDefs :: (Map AttrName Type),namespaces_Inh_AttrDefs :: (Map NamespaceName MbSortName),varsorts_Inh_AttrDefs :: (Map SortName [NamespaceName])}
data Syn_AttrDefs = Syn_AttrDefs {elabFreeVar_Syn_AttrDefs :: (Map AttrRef [Ag.Expr]),elaboration_Syn_AttrDefs :: (Ag.AttrDefs),sSetAttrDef_Syn_AttrDefs :: (Map AttrRef Expr),self_Syn_AttrDefs :: AttrDefs}
wrap_AttrDefs :: T_AttrDefs ->
                 Inh_AttrDefs ->
                 Syn_AttrDefs
wrap_AttrDefs sem (Inh_AttrDefs _lhsIenvAtom _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvField _lhsIenvSetAttrDef _lhsIenvSetAttrUse _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOelabFreeVar,_lhsOelaboration,_lhsOsSetAttrDef,_lhsOself) = sem _lhsIenvAtom _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvField _lhsIenvSetAttrDef _lhsIenvSetAttrUse _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIvarsorts
     in  (Syn_AttrDefs _lhsOelabFreeVar _lhsOelaboration _lhsOsSetAttrDef _lhsOself))
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
         (let _lhsOelabFreeVar :: (Map AttrRef [Ag.Expr])
              _lhsOelaboration :: (Ag.AttrDefs)
              _lhsOsSetAttrDef :: (Map AttrRef Expr)
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
              _hdIelabFreeVar :: (Map AttrRef [Ag.Expr])
              _hdIelaboration :: (Ag.AttrDefs)
              _hdIsSetAttrDef :: (Map AttrRef Expr)
              _hdIself :: AttrDef
              _tlIelabFreeVar :: (Map AttrRef [Ag.Expr])
              _tlIelaboration :: (Ag.AttrDefs)
              _tlIsSetAttrDef :: (Map AttrRef Expr)
              _tlIself :: AttrDefs
              _lhsOelabFreeVar =
                  ({-# LINE 50 "src/InBound/Elaboration/FreeVar.ag" #-}
                   (M.unionWith (++) _hdIelabFreeVar _tlIelabFreeVar)
                   {-# LINE 1199 "src/InBound/AG.hs" #-}
                   )
              _lhsOelaboration =
                  ({-# LINE 120 "src/InBound/Elaboration/Term.ag" #-}
                   _hdIelaboration ++ _tlIelaboration
                   {-# LINE 1204 "src/InBound/AG.hs" #-}
                   )
              _lhsOsSetAttrDef =
                  ({-# LINE 144 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsSetAttrDef _tlIsSetAttrDef)
                   {-# LINE 1209 "src/InBound/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOenvAtom =
                  ({-# LINE 62 "src/InBound/Environment.ag" #-}
                   _lhsIenvAtom
                   {-# LINE 1218 "src/InBound/AG.hs" #-}
                   )
              _hdOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1223 "src/InBound/AG.hs" #-}
                   )
              _hdOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1228 "src/InBound/AG.hs" #-}
                   )
              _hdOenvField =
                  ({-# LINE 61 "src/InBound/Environment.ag" #-}
                   _lhsIenvField
                   {-# LINE 1233 "src/InBound/AG.hs" #-}
                   )
              _hdOenvSetAttrDef =
                  ({-# LINE 139 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrDef
                   {-# LINE 1238 "src/InBound/AG.hs" #-}
                   )
              _hdOenvSetAttrUse =
                  ({-# LINE 141 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrUse
                   {-# LINE 1243 "src/InBound/AG.hs" #-}
                   )
              _hdOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1248 "src/InBound/AG.hs" #-}
                   )
              _hdOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 1253 "src/InBound/AG.hs" #-}
                   )
              _hdOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 1258 "src/InBound/AG.hs" #-}
                   )
              _hdOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 1263 "src/InBound/AG.hs" #-}
                   )
              _hdOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 1268 "src/InBound/AG.hs" #-}
                   )
              _hdOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 1273 "src/InBound/AG.hs" #-}
                   )
              _tlOenvAtom =
                  ({-# LINE 62 "src/InBound/Environment.ag" #-}
                   _lhsIenvAtom
                   {-# LINE 1278 "src/InBound/AG.hs" #-}
                   )
              _tlOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1283 "src/InBound/AG.hs" #-}
                   )
              _tlOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1288 "src/InBound/AG.hs" #-}
                   )
              _tlOenvField =
                  ({-# LINE 61 "src/InBound/Environment.ag" #-}
                   _lhsIenvField
                   {-# LINE 1293 "src/InBound/AG.hs" #-}
                   )
              _tlOenvSetAttrDef =
                  ({-# LINE 139 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrDef
                   {-# LINE 1298 "src/InBound/AG.hs" #-}
                   )
              _tlOenvSetAttrUse =
                  ({-# LINE 141 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrUse
                   {-# LINE 1303 "src/InBound/AG.hs" #-}
                   )
              _tlOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1308 "src/InBound/AG.hs" #-}
                   )
              _tlOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 1313 "src/InBound/AG.hs" #-}
                   )
              _tlOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 1318 "src/InBound/AG.hs" #-}
                   )
              _tlOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 1323 "src/InBound/AG.hs" #-}
                   )
              _tlOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 1328 "src/InBound/AG.hs" #-}
                   )
              _tlOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 1333 "src/InBound/AG.hs" #-}
                   )
              ( _hdIelabFreeVar,_hdIelaboration,_hdIsSetAttrDef,_hdIself) =
                  hd_ _hdOenvAtom _hdOenvAttrInh _hdOenvAttrSyn _hdOenvField _hdOenvSetAttrDef _hdOenvSetAttrUse _hdOenvSort _hdOenvVars _hdOlocEnvAttrInh _hdOlocEnvAttrSyn _hdOnamespaces _hdOvarsorts
              ( _tlIelabFreeVar,_tlIelaboration,_tlIsSetAttrDef,_tlIself) =
                  tl_ _tlOenvAtom _tlOenvAttrInh _tlOenvAttrSyn _tlOenvField _tlOenvSetAttrDef _tlOenvSetAttrUse _tlOenvSort _tlOenvVars _tlOlocEnvAttrInh _tlOlocEnvAttrSyn _tlOnamespaces _tlOvarsorts
          in  ( _lhsOelabFreeVar,_lhsOelaboration,_lhsOsSetAttrDef,_lhsOself)))
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
         (let _lhsOelabFreeVar :: (Map AttrRef [Ag.Expr])
              _lhsOelaboration :: (Ag.AttrDefs)
              _lhsOsSetAttrDef :: (Map AttrRef Expr)
              _lhsOself :: AttrDefs
              _lhsOelabFreeVar =
                  ({-# LINE 50 "src/InBound/Elaboration/FreeVar.ag" #-}
                   M.empty
                   {-# LINE 1361 "src/InBound/AG.hs" #-}
                   )
              _lhsOelaboration =
                  ({-# LINE 120 "src/InBound/Elaboration/Term.ag" #-}
                   []
                   {-# LINE 1366 "src/InBound/AG.hs" #-}
                   )
              _lhsOsSetAttrDef =
                  ({-# LINE 144 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1371 "src/InBound/AG.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOelabFreeVar,_lhsOelaboration,_lhsOsSetAttrDef,_lhsOself)))
-- AttrRef -----------------------------------------------------
-- cata
sem_AttrRef :: AttrRef ->
               T_AttrRef
sem_AttrRef (AttrRef _nodeLabel _attrLabel) =
    (sem_AttrRef_AttrRef (sem_NodeLabel _nodeLabel) _attrLabel)
-- semantic domain
type T_AttrRef = ( AttrRef)
data Inh_AttrRef = Inh_AttrRef {}
data Syn_AttrRef = Syn_AttrRef {self_Syn_AttrRef :: AttrRef}
wrap_AttrRef :: T_AttrRef ->
                Inh_AttrRef ->
                Syn_AttrRef
wrap_AttrRef sem (Inh_AttrRef) =
    (let ( _lhsOself) = sem
     in  (Syn_AttrRef _lhsOself))
sem_AttrRef_AttrRef :: T_NodeLabel ->
                       AttrName ->
                       T_AttrRef
sem_AttrRef_AttrRef nodeLabel_ attrLabel_ =
    (let _lhsOself :: AttrRef
         _nodeLabelIself :: NodeLabel
         _self =
             AttrRef _nodeLabelIself attrLabel_
         _lhsOself =
             _self
         ( _nodeLabelIself) =
             nodeLabel_
     in  ( _lhsOself))
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
                  ( (Ag.CtorDecl),(Map (SortName,AttrName) Type),(Map (SortName,AttrName) Type),(Map AttrRef Expr),CtorDecl)
data Inh_CtorDecl = Inh_CtorDecl {envAttrInh_Inh_CtorDecl :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_CtorDecl :: (Map (SortName,AttrName) Type),envSetAttrDef_Inh_CtorDecl :: (Map AttrRef Type),envSetAttrUse_Inh_CtorDecl :: (Map AttrRef Type),envSort_Inh_CtorDecl :: SortName,envVars_Inh_CtorDecl :: NamespaceNames,locEnvAttrInh_Inh_CtorDecl :: (Map AttrName Type),locEnvAttrSyn_Inh_CtorDecl :: (Map AttrName Type),namespaces_Inh_CtorDecl :: (Map NamespaceName MbSortName),varsorts_Inh_CtorDecl :: (Map SortName [NamespaceName])}
data Syn_CtorDecl = Syn_CtorDecl {elaboration_Syn_CtorDecl :: (Ag.CtorDecl),sAttrInh_Syn_CtorDecl :: (Map (SortName,AttrName) Type),sAttrSyn_Syn_CtorDecl :: (Map (SortName,AttrName) Type),sSetAttrDef_Syn_CtorDecl :: (Map AttrRef Expr),self_Syn_CtorDecl :: CtorDecl}
wrap_CtorDecl :: T_CtorDecl ->
                 Inh_CtorDecl ->
                 Syn_CtorDecl
wrap_CtorDecl sem (Inh_CtorDecl _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSetAttrDef _lhsIenvSetAttrUse _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOelaboration,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOsSetAttrDef,_lhsOself) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSetAttrDef _lhsIenvSetAttrUse _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIvarsorts
     in  (Syn_CtorDecl _lhsOelaboration _lhsOsAttrInh _lhsOsAttrSyn _lhsOsSetAttrDef _lhsOself))
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
         (let _lhsOelaboration :: (Ag.CtorDecl)
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
              _ctorFieldsOnamespaces :: (Map NamespaceName MbSortName)
              _ctorFieldsOsubstVarFailAttrDefExpr :: (Ag.Expr)
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
              _ctorFieldsIelabFreeVar :: (Map AttrRef [Ag.Expr])
              _ctorFieldsIelaboration :: (Ag.CtorFieldDecls)
              _ctorFieldsIrenameAttrDef :: (Ag.AttrDefs)
              _ctorFieldsIrenameAttrDefExpr :: (Ag.Exprs)
              _ctorFieldsIsAtomEnv :: (Map FieldName NamespaceName)
              _ctorFieldsIsFieldEnv :: (Map FieldName SortName)
              _ctorFieldsIself :: CtorFieldDecls
              _ctorFieldsIsubstAttrDef :: (Ag.Exprs)
              _ctorFieldsIsubstVarAttrDef :: (Maybe Ag.Expr)
              _ctorFieldsIsubstitutable :: Bool
              _ctorLocAttrDeclIself :: LocAttrDecls
              _ctorAttrDefsIelabFreeVar :: (Map AttrRef [Ag.Expr])
              _ctorAttrDefsIelaboration :: (Ag.AttrDefs)
              _ctorAttrDefsIsSetAttrDef :: (Map AttrRef Expr)
              _ctorAttrDefsIself :: AttrDefs
              _substitutable =
                  ({-# LINE 32 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   _ctorFieldsIsubstitutable
                   {-# LINE 1495 "src/InBound/AG.hs" #-}
                   )
              _substVarFailAttrDefExpr =
                  ({-# LINE 44 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   Ag.ExprCtor ctorName_ _ctorFieldsIsubstAttrDef
                   {-# LINE 1500 "src/InBound/AG.hs" #-}
                   )
              _substAttrDef =
                  ({-# LINE 46 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   case _ctorFieldsIsubstVarAttrDef of
                     Just e | _substitutable     -> [ Ag.AttrDef substRef e ]
                     _ -> [ Ag.AttrDef substRef _substVarFailAttrDefExpr     ]
                   {-# LINE 1507 "src/InBound/AG.hs" #-}
                   )
              _renameAttrDef =
                  ({-# LINE 33 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   _ctorFieldsIrenameAttrDef ++
                   [ Ag.AttrDef renameRef $
                       Ag.ExprCtor ctorName_
                         _ctorFieldsIrenameAttrDefExpr
                   ]
                   {-# LINE 1516 "src/InBound/AG.hs" #-}
                   )
              _attrDefsElabFree =
                  ({-# LINE 90 "src/InBound/Elaboration/FreeVar.ag" #-}
                   [ Ag.AttrDef
                       (attrRefToFreeVarRef k)
                       (Ag.ExprSetUnions v)
                   | (k,v) <- M.toList (M.unionsWith (++)
                                          [ M.map (const []) _envSetAttrUse
                                          , _ctorFieldsIelabFreeVar
                                          , _ctorAttrDefsIelabFreeVar
                                          ])
                   ]
                   {-# LINE 1529 "src/InBound/AG.hs" #-}
                   )
              _attrDefsElab =
                  ({-# LINE 81 "src/InBound/Elaboration/Term.ag" #-}
                   _ctorAttrDefsIelaboration ++
                   _attrDefsElabFree     ++
                   _renameAttrDef     ++
                   _substAttrDef
                   {-# LINE 1537 "src/InBound/AG.hs" #-}
                   )
              _lhsOelaboration =
                  ({-# LINE 88 "src/InBound/Elaboration/Term.ag" #-}
                   Ag.CtorDecl
                     ctorName_
                     _ctorFieldsIelaboration
                     []
                     _attrDefsElab
                   {-# LINE 1546 "src/InBound/AG.hs" #-}
                   )
              _envField =
                  ({-# LINE 66 "src/InBound/Environment.ag" #-}
                   _ctorFieldsIsFieldEnv
                   {-# LINE 1551 "src/InBound/AG.hs" #-}
                   )
              _envAtom =
                  ({-# LINE 67 "src/InBound/Environment.ag" #-}
                   _ctorFieldsIsAtomEnv
                   {-# LINE 1556 "src/InBound/AG.hs" #-}
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
                   {-# LINE 1567 "src/InBound/AG.hs" #-}
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
                   {-# LINE 1578 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 83 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1583 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 82 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1588 "src/InBound/AG.hs" #-}
                   )
              _lhsOsSetAttrDef =
                  ({-# LINE 144 "src/InBound/Environment.ag" #-}
                   _ctorAttrDefsIsSetAttrDef
                   {-# LINE 1593 "src/InBound/AG.hs" #-}
                   )
              _self =
                  CtorDecl ctorName_ _ctorFieldsIself _ctorLocAttrDeclIself _ctorAttrDefsIself
              _lhsOself =
                  _self
              _ctorFieldsOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1602 "src/InBound/AG.hs" #-}
                   )
              _ctorFieldsOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1607 "src/InBound/AG.hs" #-}
                   )
              _ctorFieldsOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1612 "src/InBound/AG.hs" #-}
                   )
              _ctorFieldsOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 1617 "src/InBound/AG.hs" #-}
                   )
              _ctorFieldsOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 1622 "src/InBound/AG.hs" #-}
                   )
              _ctorFieldsOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 1627 "src/InBound/AG.hs" #-}
                   )
              _ctorFieldsOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 1632 "src/InBound/AG.hs" #-}
                   )
              _ctorFieldsOsubstVarFailAttrDefExpr =
                  ({-# LINE 53 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   _substVarFailAttrDefExpr
                   {-# LINE 1637 "src/InBound/AG.hs" #-}
                   )
              _ctorFieldsOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 1642 "src/InBound/AG.hs" #-}
                   )
              _ctorLocAttrDeclOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 1647 "src/InBound/AG.hs" #-}
                   )
              _ctorLocAttrDeclOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 1652 "src/InBound/AG.hs" #-}
                   )
              _ctorAttrDefsOenvAtom =
                  ({-# LINE 62 "src/InBound/Environment.ag" #-}
                   _envAtom
                   {-# LINE 1657 "src/InBound/AG.hs" #-}
                   )
              _ctorAttrDefsOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1662 "src/InBound/AG.hs" #-}
                   )
              _ctorAttrDefsOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1667 "src/InBound/AG.hs" #-}
                   )
              _ctorAttrDefsOenvField =
                  ({-# LINE 61 "src/InBound/Environment.ag" #-}
                   _envField
                   {-# LINE 1672 "src/InBound/AG.hs" #-}
                   )
              _ctorAttrDefsOenvSetAttrDef =
                  ({-# LINE 139 "src/InBound/Environment.ag" #-}
                   _envSetAttrDef
                   {-# LINE 1677 "src/InBound/AG.hs" #-}
                   )
              _ctorAttrDefsOenvSetAttrUse =
                  ({-# LINE 141 "src/InBound/Environment.ag" #-}
                   _envSetAttrUse
                   {-# LINE 1682 "src/InBound/AG.hs" #-}
                   )
              _ctorAttrDefsOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1687 "src/InBound/AG.hs" #-}
                   )
              _ctorAttrDefsOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 1692 "src/InBound/AG.hs" #-}
                   )
              _ctorAttrDefsOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 1697 "src/InBound/AG.hs" #-}
                   )
              _ctorAttrDefsOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 1702 "src/InBound/AG.hs" #-}
                   )
              _ctorAttrDefsOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 1707 "src/InBound/AG.hs" #-}
                   )
              _ctorAttrDefsOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 1712 "src/InBound/AG.hs" #-}
                   )
              ( _ctorFieldsIelabFreeVar,_ctorFieldsIelaboration,_ctorFieldsIrenameAttrDef,_ctorFieldsIrenameAttrDefExpr,_ctorFieldsIsAtomEnv,_ctorFieldsIsFieldEnv,_ctorFieldsIself,_ctorFieldsIsubstAttrDef,_ctorFieldsIsubstVarAttrDef,_ctorFieldsIsubstitutable) =
                  ctorFields_ _ctorFieldsOenvAttrInh _ctorFieldsOenvAttrSyn _ctorFieldsOenvSort _ctorFieldsOenvVars _ctorFieldsOlocEnvAttrInh _ctorFieldsOlocEnvAttrSyn _ctorFieldsOnamespaces _ctorFieldsOsubstVarFailAttrDefExpr _ctorFieldsOvarsorts
              ( _ctorLocAttrDeclIself) =
                  ctorLocAttrDecl_ _ctorLocAttrDeclOnamespaces _ctorLocAttrDeclOvarsorts
              ( _ctorAttrDefsIelabFreeVar,_ctorAttrDefsIelaboration,_ctorAttrDefsIsSetAttrDef,_ctorAttrDefsIself) =
                  ctorAttrDefs_ _ctorAttrDefsOenvAtom _ctorAttrDefsOenvAttrInh _ctorAttrDefsOenvAttrSyn _ctorAttrDefsOenvField _ctorAttrDefsOenvSetAttrDef _ctorAttrDefsOenvSetAttrUse _ctorAttrDefsOenvSort _ctorAttrDefsOenvVars _ctorAttrDefsOlocEnvAttrInh _ctorAttrDefsOlocEnvAttrSyn _ctorAttrDefsOnamespaces _ctorAttrDefsOvarsorts
          in  ( _lhsOelaboration,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOsSetAttrDef,_lhsOself)))
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
                   ( (Ag.CtorDecls),(Map (SortName,AttrName) Type),(Map (SortName,AttrName) Type),(Map AttrRef Expr),CtorDecls)
data Inh_CtorDecls = Inh_CtorDecls {envAttrInh_Inh_CtorDecls :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_CtorDecls :: (Map (SortName,AttrName) Type),envSetAttrDef_Inh_CtorDecls :: (Map AttrRef Type),envSetAttrUse_Inh_CtorDecls :: (Map AttrRef Type),envSort_Inh_CtorDecls :: SortName,envVars_Inh_CtorDecls :: NamespaceNames,locEnvAttrInh_Inh_CtorDecls :: (Map AttrName Type),locEnvAttrSyn_Inh_CtorDecls :: (Map AttrName Type),namespaces_Inh_CtorDecls :: (Map NamespaceName MbSortName),varsorts_Inh_CtorDecls :: (Map SortName [NamespaceName])}
data Syn_CtorDecls = Syn_CtorDecls {elaboration_Syn_CtorDecls :: (Ag.CtorDecls),sAttrInh_Syn_CtorDecls :: (Map (SortName,AttrName) Type),sAttrSyn_Syn_CtorDecls :: (Map (SortName,AttrName) Type),sSetAttrDef_Syn_CtorDecls :: (Map AttrRef Expr),self_Syn_CtorDecls :: CtorDecls}
wrap_CtorDecls :: T_CtorDecls ->
                  Inh_CtorDecls ->
                  Syn_CtorDecls
wrap_CtorDecls sem (Inh_CtorDecls _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSetAttrDef _lhsIenvSetAttrUse _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOelaboration,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOsSetAttrDef,_lhsOself) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSetAttrDef _lhsIenvSetAttrUse _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIvarsorts
     in  (Syn_CtorDecls _lhsOelaboration _lhsOsAttrInh _lhsOsAttrSyn _lhsOsSetAttrDef _lhsOself))
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
         (let _lhsOelaboration :: (Ag.CtorDecls)
              _lhsOsAttrInh :: (Map (SortName,AttrName) Type)
              _lhsOsAttrSyn :: (Map (SortName,AttrName) Type)
              _lhsOsSetAttrDef :: (Map AttrRef Expr)
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
              _hdIelaboration :: (Ag.CtorDecl)
              _hdIsAttrInh :: (Map (SortName,AttrName) Type)
              _hdIsAttrSyn :: (Map (SortName,AttrName) Type)
              _hdIsSetAttrDef :: (Map AttrRef Expr)
              _hdIself :: CtorDecl
              _tlIelaboration :: (Ag.CtorDecls)
              _tlIsAttrInh :: (Map (SortName,AttrName) Type)
              _tlIsAttrSyn :: (Map (SortName,AttrName) Type)
              _tlIsSetAttrDef :: (Map AttrRef Expr)
              _tlIself :: CtorDecls
              _lhsOelaboration =
                  ({-# LINE 75 "src/InBound/Elaboration/Term.ag" #-}
                   _hdIelaboration : _tlIelaboration
                   {-# LINE 1799 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 83 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsAttrInh _tlIsAttrInh)
                   {-# LINE 1804 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 82 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsAttrSyn _tlIsAttrSyn)
                   {-# LINE 1809 "src/InBound/AG.hs" #-}
                   )
              _lhsOsSetAttrDef =
                  ({-# LINE 144 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsSetAttrDef _tlIsSetAttrDef)
                   {-# LINE 1814 "src/InBound/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1823 "src/InBound/AG.hs" #-}
                   )
              _hdOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1828 "src/InBound/AG.hs" #-}
                   )
              _hdOenvSetAttrDef =
                  ({-# LINE 139 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrDef
                   {-# LINE 1833 "src/InBound/AG.hs" #-}
                   )
              _hdOenvSetAttrUse =
                  ({-# LINE 141 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrUse
                   {-# LINE 1838 "src/InBound/AG.hs" #-}
                   )
              _hdOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1843 "src/InBound/AG.hs" #-}
                   )
              _hdOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 1848 "src/InBound/AG.hs" #-}
                   )
              _hdOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 1853 "src/InBound/AG.hs" #-}
                   )
              _hdOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 1858 "src/InBound/AG.hs" #-}
                   )
              _hdOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 1863 "src/InBound/AG.hs" #-}
                   )
              _hdOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 1868 "src/InBound/AG.hs" #-}
                   )
              _tlOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 1873 "src/InBound/AG.hs" #-}
                   )
              _tlOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 1878 "src/InBound/AG.hs" #-}
                   )
              _tlOenvSetAttrDef =
                  ({-# LINE 139 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrDef
                   {-# LINE 1883 "src/InBound/AG.hs" #-}
                   )
              _tlOenvSetAttrUse =
                  ({-# LINE 141 "src/InBound/Environment.ag" #-}
                   _lhsIenvSetAttrUse
                   {-# LINE 1888 "src/InBound/AG.hs" #-}
                   )
              _tlOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 1893 "src/InBound/AG.hs" #-}
                   )
              _tlOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 1898 "src/InBound/AG.hs" #-}
                   )
              _tlOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 1903 "src/InBound/AG.hs" #-}
                   )
              _tlOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 1908 "src/InBound/AG.hs" #-}
                   )
              _tlOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 1913 "src/InBound/AG.hs" #-}
                   )
              _tlOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 1918 "src/InBound/AG.hs" #-}
                   )
              ( _hdIelaboration,_hdIsAttrInh,_hdIsAttrSyn,_hdIsSetAttrDef,_hdIself) =
                  hd_ _hdOenvAttrInh _hdOenvAttrSyn _hdOenvSetAttrDef _hdOenvSetAttrUse _hdOenvSort _hdOenvVars _hdOlocEnvAttrInh _hdOlocEnvAttrSyn _hdOnamespaces _hdOvarsorts
              ( _tlIelaboration,_tlIsAttrInh,_tlIsAttrSyn,_tlIsSetAttrDef,_tlIself) =
                  tl_ _tlOenvAttrInh _tlOenvAttrSyn _tlOenvSetAttrDef _tlOenvSetAttrUse _tlOenvSort _tlOenvVars _tlOlocEnvAttrInh _tlOlocEnvAttrSyn _tlOnamespaces _tlOvarsorts
          in  ( _lhsOelaboration,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOsSetAttrDef,_lhsOself)))
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
         (let _lhsOelaboration :: (Ag.CtorDecls)
              _lhsOsAttrInh :: (Map (SortName,AttrName) Type)
              _lhsOsAttrSyn :: (Map (SortName,AttrName) Type)
              _lhsOsSetAttrDef :: (Map AttrRef Expr)
              _lhsOself :: CtorDecls
              _lhsOelaboration =
                  ({-# LINE 75 "src/InBound/Elaboration/Term.ag" #-}
                   []
                   {-# LINE 1945 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 83 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1950 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 82 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1955 "src/InBound/AG.hs" #-}
                   )
              _lhsOsSetAttrDef =
                  ({-# LINE 144 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 1960 "src/InBound/AG.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOelaboration,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOsSetAttrDef,_lhsOself)))
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
                       SortName ->
                       NamespaceNames ->
                       (Map AttrName Type) ->
                       (Map AttrName Type) ->
                       (Map NamespaceName MbSortName) ->
                       (Ag.Expr) ->
                       (Map SortName [NamespaceName]) ->
                       ( (Map AttrRef [Ag.Expr]),(Ag.CtorFieldDecl),(Ag.AttrDefs),(Ag.Expr),(Map FieldName NamespaceName),(Map FieldName SortName),CtorFieldDecl,(Ag.Expr),(Maybe Ag.Expr),Bool)
data Inh_CtorFieldDecl = Inh_CtorFieldDecl {envAttrInh_Inh_CtorFieldDecl :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_CtorFieldDecl :: (Map (SortName,AttrName) Type),envSort_Inh_CtorFieldDecl :: SortName,envVars_Inh_CtorFieldDecl :: NamespaceNames,locEnvAttrInh_Inh_CtorFieldDecl :: (Map AttrName Type),locEnvAttrSyn_Inh_CtorFieldDecl :: (Map AttrName Type),namespaces_Inh_CtorFieldDecl :: (Map NamespaceName MbSortName),substVarFailAttrDefExpr_Inh_CtorFieldDecl :: (Ag.Expr),varsorts_Inh_CtorFieldDecl :: (Map SortName [NamespaceName])}
data Syn_CtorFieldDecl = Syn_CtorFieldDecl {elabFreeVar_Syn_CtorFieldDecl :: (Map AttrRef [Ag.Expr]),elaboration_Syn_CtorFieldDecl :: (Ag.CtorFieldDecl),renameAttrDef_Syn_CtorFieldDecl :: (Ag.AttrDefs),renameAttrDefExpr_Syn_CtorFieldDecl :: (Ag.Expr),sAtomEnv_Syn_CtorFieldDecl :: (Map FieldName NamespaceName),sFieldEnv_Syn_CtorFieldDecl :: (Map FieldName SortName),self_Syn_CtorFieldDecl :: CtorFieldDecl,substAttrDef_Syn_CtorFieldDecl :: (Ag.Expr),substVarAttrDef_Syn_CtorFieldDecl :: (Maybe Ag.Expr),substitutable_Syn_CtorFieldDecl :: Bool}
wrap_CtorFieldDecl :: T_CtorFieldDecl ->
                      Inh_CtorFieldDecl ->
                      Syn_CtorFieldDecl
wrap_CtorFieldDecl sem (Inh_CtorFieldDecl _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIsubstVarFailAttrDefExpr _lhsIvarsorts) =
    (let ( _lhsOelabFreeVar,_lhsOelaboration,_lhsOrenameAttrDef,_lhsOrenameAttrDefExpr,_lhsOsAtomEnv,_lhsOsFieldEnv,_lhsOself,_lhsOsubstAttrDef,_lhsOsubstVarAttrDef,_lhsOsubstitutable) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIsubstVarFailAttrDefExpr _lhsIvarsorts
     in  (Syn_CtorFieldDecl _lhsOelabFreeVar _lhsOelaboration _lhsOrenameAttrDef _lhsOrenameAttrDefExpr _lhsOsAtomEnv _lhsOsFieldEnv _lhsOself _lhsOsubstAttrDef _lhsOsubstVarAttrDef _lhsOsubstitutable))
sem_CtorFieldDecl_CFRef :: FieldName ->
                           AttrName ->
                           T_CtorFieldDecl
sem_CtorFieldDecl_CFRef ctorFieldName_ ctorFieldType_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsInamespaces
       _lhsIsubstVarFailAttrDefExpr
       _lhsIvarsorts ->
         (let _lhsOsubstAttrDef :: (Ag.Expr)
              _lhsOsubstVarAttrDef :: (Maybe Ag.Expr)
              _lhsOrenameAttrDefExpr :: (Ag.Expr)
              _lhsOrenameAttrDef :: (Ag.AttrDefs)
              _lhsOelabFreeVar :: (Map AttrRef [Ag.Expr])
              _lhsOelaboration :: (Ag.CtorFieldDecl)
              _lhsOsAtomEnv :: (Map FieldName NamespaceName)
              _lhsOsFieldEnv :: (Map FieldName SortName)
              _lhsOsubstitutable :: Bool
              _lhsOself :: CtorFieldDecl
              _substitutable =
                  ({-# LINE 29 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   isJust _mbsort
                   {-# LINE 2024 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstAttrDef =
                  ({-# LINE 62 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   Ag.ExprAttrRef $ renamedFieldRef ctorFieldName_
                   {-# LINE 2029 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstVarAttrDef =
                  ({-# LINE 64 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   if _substitutable
                   then Just $
                          Ag.ExprMapLookup
                            _lhsIsubstVarFailAttrDefExpr
                            (Ag.ExprAttrRef $ renamedFieldRef ctorFieldName_)
                            (Ag.ExprAttrRef
                             (attrRefToSubstMapRef
                              (AttrRef Lhs ctorFieldType_)))
                   else Nothing
                   {-# LINE 2042 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameAttrDefExpr =
                  ({-# LINE 49 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   Ag.ExprAttrRef (renamedFieldRef ctorFieldName_)
                   {-# LINE 2047 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameAttrDef =
                  ({-# LINE 51 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   [ Ag.AttrDef
                       (renamedFieldRef ctorFieldName_) $
                       Ag.ExprMapLookup
                         (Ag.ExprField ctorFieldName_)
                         (Ag.ExprField ctorFieldName_)
                         (Ag.ExprAttrRef
                           (attrRefToRenameMapRef
                              (AttrRef Lhs ctorFieldType_)))
                   ]
                   {-# LINE 2060 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabFreeVar =
                  ({-# LINE 82 "src/InBound/Elaboration/FreeVar.ag" #-}
                   M.singleton
                     (AttrRef Lhs ctorFieldType_)
                     [Ag.ExprSetSingleton $
                        Ag.ExprField ctorFieldName_]
                   {-# LINE 2068 "src/InBound/AG.hs" #-}
                   )
              _lhsOelaboration =
                  ({-# LINE 103 "src/InBound/Elaboration/Term.ag" #-}
                   case M.findWithDefault
                          (error $ "CFRef elab: " ++ show ctorFieldType_)
                          ctorFieldType_
                          _lhsIlocEnvAttrInh of
                    Context (NN ns) -> Ag.CFTerminal ctorFieldName_ ns
                   {-# LINE 2077 "src/InBound/AG.hs" #-}
                   )
              _namespace =
                  ({-# LINE 123 "src/InBound/Environment.ag" #-}
                   case M.findWithDefault
                           (error "unknown context attribute")
                           ctorFieldType_
                           _lhsIlocEnvAttrInh of
                     Context ns -> ns
                   {-# LINE 2086 "src/InBound/AG.hs" #-}
                   )
              _mbsort =
                  ({-# LINE 128 "src/InBound/Environment.ag" #-}
                   join $ M.lookup _namespace     _lhsInamespaces
                   {-# LINE 2091 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAtomEnv =
                  ({-# LINE 54 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 2096 "src/InBound/AG.hs" #-}
                   )
              _lhsOsFieldEnv =
                  ({-# LINE 53 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 2101 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstitutable =
                  ({-# LINE 26 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   _substitutable
                   {-# LINE 2106 "src/InBound/AG.hs" #-}
                   )
              _self =
                  CFRef ctorFieldName_ ctorFieldType_
              _lhsOself =
                  _self
          in  ( _lhsOelabFreeVar,_lhsOelaboration,_lhsOrenameAttrDef,_lhsOrenameAttrDefExpr,_lhsOsAtomEnv,_lhsOsFieldEnv,_lhsOself,_lhsOsubstAttrDef,_lhsOsubstVarAttrDef,_lhsOsubstitutable)))
sem_CtorFieldDecl_CFAtom :: FieldName ->
                            NamespaceName ->
                            T_CtorFieldDecl
sem_CtorFieldDecl_CFAtom ctorFieldName_ ctorFieldType_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsInamespaces
       _lhsIsubstVarFailAttrDefExpr
       _lhsIvarsorts ->
         (let _lhsOsubstAttrDef :: (Ag.Expr)
              _lhsOrenameAttrDefExpr :: (Ag.Expr)
              _lhsOelaboration :: (Ag.CtorFieldDecl)
              _lhsOsAtomEnv :: (Map FieldName NamespaceName)
              _lhsOelabFreeVar :: (Map AttrRef [Ag.Expr])
              _lhsOrenameAttrDef :: (Ag.AttrDefs)
              _lhsOsFieldEnv :: (Map FieldName SortName)
              _lhsOsubstVarAttrDef :: (Maybe Ag.Expr)
              _lhsOsubstitutable :: Bool
              _lhsOself :: CtorFieldDecl
              _lhsOsubstAttrDef =
                  ({-# LINE 75 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   Ag.ExprAttrRef
                     (renamedFieldRef ctorFieldName_)
                   {-# LINE 2140 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameAttrDefExpr =
                  ({-# LINE 62 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   Ag.ExprAttrRef
                     (renamedFieldRef ctorFieldName_)
                   {-# LINE 2146 "src/InBound/AG.hs" #-}
                   )
              _lhsOelaboration =
                  ({-# LINE 110 "src/InBound/Elaboration/Term.ag" #-}
                   case ctorFieldType_ of
                     NN ns -> Ag.CFTerminal ctorFieldName_ ns
                   {-# LINE 2152 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAtomEnv =
                  ({-# LINE 58 "src/InBound/Environment.ag" #-}
                   M.singleton ctorFieldName_ ctorFieldType_
                   {-# LINE 2157 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabFreeVar =
                  ({-# LINE 77 "src/InBound/Elaboration/FreeVar.ag" #-}
                   M.empty
                   {-# LINE 2162 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameAttrDef =
                  ({-# LINE 45 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   []
                   {-# LINE 2167 "src/InBound/AG.hs" #-}
                   )
              _lhsOsFieldEnv =
                  ({-# LINE 53 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 2172 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstVarAttrDef =
                  ({-# LINE 58 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   mzero
                   {-# LINE 2177 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstitutable =
                  ({-# LINE 26 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   False
                   {-# LINE 2182 "src/InBound/AG.hs" #-}
                   )
              _self =
                  CFAtom ctorFieldName_ ctorFieldType_
              _lhsOself =
                  _self
          in  ( _lhsOelabFreeVar,_lhsOelaboration,_lhsOrenameAttrDef,_lhsOrenameAttrDefExpr,_lhsOsAtomEnv,_lhsOsFieldEnv,_lhsOself,_lhsOsubstAttrDef,_lhsOsubstVarAttrDef,_lhsOsubstitutable)))
sem_CtorFieldDecl_CFSubtree :: FieldName ->
                               SortName ->
                               T_CtorFieldDecl
sem_CtorFieldDecl_CFSubtree ctorFieldName_ ctorFieldType_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsInamespaces
       _lhsIsubstVarFailAttrDefExpr
       _lhsIvarsorts ->
         (let _lhsOsubstAttrDef :: (Ag.Expr)
              _lhsOrenameAttrDefExpr :: (Ag.Expr)
              _lhsOelaboration :: (Ag.CtorFieldDecl)
              _lhsOsFieldEnv :: (Map FieldName SortName)
              _lhsOelabFreeVar :: (Map AttrRef [Ag.Expr])
              _lhsOrenameAttrDef :: (Ag.AttrDefs)
              _lhsOsAtomEnv :: (Map FieldName NamespaceName)
              _lhsOsubstVarAttrDef :: (Maybe Ag.Expr)
              _lhsOsubstitutable :: Bool
              _lhsOself :: CtorFieldDecl
              _lhsOsubstAttrDef =
                  ({-# LINE 79 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   Ag.ExprAttrRef
                     (Ag.AttrRef (Ag.Sub ctorFieldName_) substAN)
                   {-# LINE 2216 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameAttrDefExpr =
                  ({-# LINE 66 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   Ag.ExprAttrRef
                     (Ag.AttrRef (Ag.Sub ctorFieldName_) renameAN)
                   {-# LINE 2222 "src/InBound/AG.hs" #-}
                   )
              _lhsOelaboration =
                  ({-# LINE 114 "src/InBound/Elaboration/Term.ag" #-}
                   Ag.CFSubtree ctorFieldName_ ctorFieldType_
                   {-# LINE 2227 "src/InBound/AG.hs" #-}
                   )
              _lhsOsFieldEnv =
                  ({-# LINE 57 "src/InBound/Environment.ag" #-}
                   M.singleton ctorFieldName_ ctorFieldType_
                   {-# LINE 2232 "src/InBound/AG.hs" #-}
                   )
              _subtreeSynAttrs =
                  ({-# LINE 131 "src/InBound/Environment.ag" #-}
                   [ (an,ty)
                   | ((sn,an),ty) <- M.toList _lhsIenvAttrSyn
                   , sn == ctorFieldType_
                   ]
                   {-# LINE 2240 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabFreeVar =
                  ({-# LINE 77 "src/InBound/Elaboration/FreeVar.ag" #-}
                   M.empty
                   {-# LINE 2245 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameAttrDef =
                  ({-# LINE 45 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   []
                   {-# LINE 2250 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAtomEnv =
                  ({-# LINE 54 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 2255 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstVarAttrDef =
                  ({-# LINE 58 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   mzero
                   {-# LINE 2260 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstitutable =
                  ({-# LINE 26 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   False
                   {-# LINE 2265 "src/InBound/AG.hs" #-}
                   )
              _self =
                  CFSubtree ctorFieldName_ ctorFieldType_
              _lhsOself =
                  _self
          in  ( _lhsOelabFreeVar,_lhsOelaboration,_lhsOrenameAttrDef,_lhsOrenameAttrDefExpr,_lhsOsAtomEnv,_lhsOsFieldEnv,_lhsOself,_lhsOsubstAttrDef,_lhsOsubstVarAttrDef,_lhsOsubstitutable)))
sem_CtorFieldDecl_CFTerminal :: FieldName ->
                                String ->
                                T_CtorFieldDecl
sem_CtorFieldDecl_CFTerminal ctorFieldName_ ctorFieldType_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsInamespaces
       _lhsIsubstVarFailAttrDefExpr
       _lhsIvarsorts ->
         (let _lhsOsubstAttrDef :: (Ag.Expr)
              _lhsOrenameAttrDefExpr :: (Ag.Expr)
              _lhsOelaboration :: (Ag.CtorFieldDecl)
              _lhsOelabFreeVar :: (Map AttrRef [Ag.Expr])
              _lhsOrenameAttrDef :: (Ag.AttrDefs)
              _lhsOsAtomEnv :: (Map FieldName NamespaceName)
              _lhsOsFieldEnv :: (Map FieldName SortName)
              _lhsOsubstVarAttrDef :: (Maybe Ag.Expr)
              _lhsOsubstitutable :: Bool
              _lhsOself :: CtorFieldDecl
              _lhsOsubstAttrDef =
                  ({-# LINE 83 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   Ag.ExprField ctorFieldName_
                   {-# LINE 2298 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameAttrDefExpr =
                  ({-# LINE 70 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   Ag.ExprField ctorFieldName_
                   {-# LINE 2303 "src/InBound/AG.hs" #-}
                   )
              _lhsOelaboration =
                  ({-# LINE 116 "src/InBound/Elaboration/Term.ag" #-}
                   Ag.CFTerminal ctorFieldName_ ctorFieldType_
                   {-# LINE 2308 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabFreeVar =
                  ({-# LINE 77 "src/InBound/Elaboration/FreeVar.ag" #-}
                   M.empty
                   {-# LINE 2313 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameAttrDef =
                  ({-# LINE 45 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   []
                   {-# LINE 2318 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAtomEnv =
                  ({-# LINE 54 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 2323 "src/InBound/AG.hs" #-}
                   )
              _lhsOsFieldEnv =
                  ({-# LINE 53 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 2328 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstVarAttrDef =
                  ({-# LINE 58 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   mzero
                   {-# LINE 2333 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstitutable =
                  ({-# LINE 26 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   False
                   {-# LINE 2338 "src/InBound/AG.hs" #-}
                   )
              _self =
                  CFTerminal ctorFieldName_ ctorFieldType_
              _lhsOself =
                  _self
          in  ( _lhsOelabFreeVar,_lhsOelaboration,_lhsOrenameAttrDef,_lhsOrenameAttrDefExpr,_lhsOsAtomEnv,_lhsOsFieldEnv,_lhsOself,_lhsOsubstAttrDef,_lhsOsubstVarAttrDef,_lhsOsubstitutable)))
-- CtorFieldDecls ----------------------------------------------
-- cata
sem_CtorFieldDecls :: CtorFieldDecls ->
                      T_CtorFieldDecls
sem_CtorFieldDecls list =
    (Prelude.foldr sem_CtorFieldDecls_Cons sem_CtorFieldDecls_Nil (Prelude.map sem_CtorFieldDecl list))
-- semantic domain
type T_CtorFieldDecls = (Map (SortName,AttrName) Type) ->
                        (Map (SortName,AttrName) Type) ->
                        SortName ->
                        NamespaceNames ->
                        (Map AttrName Type) ->
                        (Map AttrName Type) ->
                        (Map NamespaceName MbSortName) ->
                        (Ag.Expr) ->
                        (Map SortName [NamespaceName]) ->
                        ( (Map AttrRef [Ag.Expr]),(Ag.CtorFieldDecls),(Ag.AttrDefs),(Ag.Exprs),(Map FieldName NamespaceName),(Map FieldName SortName),CtorFieldDecls,(Ag.Exprs),(Maybe Ag.Expr),Bool)
data Inh_CtorFieldDecls = Inh_CtorFieldDecls {envAttrInh_Inh_CtorFieldDecls :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_CtorFieldDecls :: (Map (SortName,AttrName) Type),envSort_Inh_CtorFieldDecls :: SortName,envVars_Inh_CtorFieldDecls :: NamespaceNames,locEnvAttrInh_Inh_CtorFieldDecls :: (Map AttrName Type),locEnvAttrSyn_Inh_CtorFieldDecls :: (Map AttrName Type),namespaces_Inh_CtorFieldDecls :: (Map NamespaceName MbSortName),substVarFailAttrDefExpr_Inh_CtorFieldDecls :: (Ag.Expr),varsorts_Inh_CtorFieldDecls :: (Map SortName [NamespaceName])}
data Syn_CtorFieldDecls = Syn_CtorFieldDecls {elabFreeVar_Syn_CtorFieldDecls :: (Map AttrRef [Ag.Expr]),elaboration_Syn_CtorFieldDecls :: (Ag.CtorFieldDecls),renameAttrDef_Syn_CtorFieldDecls :: (Ag.AttrDefs),renameAttrDefExpr_Syn_CtorFieldDecls :: (Ag.Exprs),sAtomEnv_Syn_CtorFieldDecls :: (Map FieldName NamespaceName),sFieldEnv_Syn_CtorFieldDecls :: (Map FieldName SortName),self_Syn_CtorFieldDecls :: CtorFieldDecls,substAttrDef_Syn_CtorFieldDecls :: (Ag.Exprs),substVarAttrDef_Syn_CtorFieldDecls :: (Maybe Ag.Expr),substitutable_Syn_CtorFieldDecls :: Bool}
wrap_CtorFieldDecls :: T_CtorFieldDecls ->
                       Inh_CtorFieldDecls ->
                       Syn_CtorFieldDecls
wrap_CtorFieldDecls sem (Inh_CtorFieldDecls _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIsubstVarFailAttrDefExpr _lhsIvarsorts) =
    (let ( _lhsOelabFreeVar,_lhsOelaboration,_lhsOrenameAttrDef,_lhsOrenameAttrDefExpr,_lhsOsAtomEnv,_lhsOsFieldEnv,_lhsOself,_lhsOsubstAttrDef,_lhsOsubstVarAttrDef,_lhsOsubstitutable) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvSort _lhsIenvVars _lhsIlocEnvAttrInh _lhsIlocEnvAttrSyn _lhsInamespaces _lhsIsubstVarFailAttrDefExpr _lhsIvarsorts
     in  (Syn_CtorFieldDecls _lhsOelabFreeVar _lhsOelaboration _lhsOrenameAttrDef _lhsOrenameAttrDefExpr _lhsOsAtomEnv _lhsOsFieldEnv _lhsOself _lhsOsubstAttrDef _lhsOsubstVarAttrDef _lhsOsubstitutable))
sem_CtorFieldDecls_Cons :: T_CtorFieldDecl ->
                           T_CtorFieldDecls ->
                           T_CtorFieldDecls
sem_CtorFieldDecls_Cons hd_ tl_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsInamespaces
       _lhsIsubstVarFailAttrDefExpr
       _lhsIvarsorts ->
         (let _lhsOelabFreeVar :: (Map AttrRef [Ag.Expr])
              _lhsOelaboration :: (Ag.CtorFieldDecls)
              _lhsOrenameAttrDef :: (Ag.AttrDefs)
              _lhsOrenameAttrDefExpr :: (Ag.Exprs)
              _lhsOsAtomEnv :: (Map FieldName NamespaceName)
              _lhsOsFieldEnv :: (Map FieldName SortName)
              _lhsOsubstAttrDef :: (Ag.Exprs)
              _lhsOsubstVarAttrDef :: (Maybe Ag.Expr)
              _lhsOsubstitutable :: Bool
              _lhsOself :: CtorFieldDecls
              _hdOenvAttrInh :: (Map (SortName,AttrName) Type)
              _hdOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _hdOenvSort :: SortName
              _hdOenvVars :: NamespaceNames
              _hdOlocEnvAttrInh :: (Map AttrName Type)
              _hdOlocEnvAttrSyn :: (Map AttrName Type)
              _hdOnamespaces :: (Map NamespaceName MbSortName)
              _hdOsubstVarFailAttrDefExpr :: (Ag.Expr)
              _hdOvarsorts :: (Map SortName [NamespaceName])
              _tlOenvAttrInh :: (Map (SortName,AttrName) Type)
              _tlOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _tlOenvSort :: SortName
              _tlOenvVars :: NamespaceNames
              _tlOlocEnvAttrInh :: (Map AttrName Type)
              _tlOlocEnvAttrSyn :: (Map AttrName Type)
              _tlOnamespaces :: (Map NamespaceName MbSortName)
              _tlOsubstVarFailAttrDefExpr :: (Ag.Expr)
              _tlOvarsorts :: (Map SortName [NamespaceName])
              _hdIelabFreeVar :: (Map AttrRef [Ag.Expr])
              _hdIelaboration :: (Ag.CtorFieldDecl)
              _hdIrenameAttrDef :: (Ag.AttrDefs)
              _hdIrenameAttrDefExpr :: (Ag.Expr)
              _hdIsAtomEnv :: (Map FieldName NamespaceName)
              _hdIsFieldEnv :: (Map FieldName SortName)
              _hdIself :: CtorFieldDecl
              _hdIsubstAttrDef :: (Ag.Expr)
              _hdIsubstVarAttrDef :: (Maybe Ag.Expr)
              _hdIsubstitutable :: Bool
              _tlIelabFreeVar :: (Map AttrRef [Ag.Expr])
              _tlIelaboration :: (Ag.CtorFieldDecls)
              _tlIrenameAttrDef :: (Ag.AttrDefs)
              _tlIrenameAttrDefExpr :: (Ag.Exprs)
              _tlIsAtomEnv :: (Map FieldName NamespaceName)
              _tlIsFieldEnv :: (Map FieldName SortName)
              _tlIself :: CtorFieldDecls
              _tlIsubstAttrDef :: (Ag.Exprs)
              _tlIsubstVarAttrDef :: (Maybe Ag.Expr)
              _tlIsubstitutable :: Bool
              _lhsOelabFreeVar =
                  ({-# LINE 77 "src/InBound/Elaboration/FreeVar.ag" #-}
                   (M.unionWith (++) _hdIelabFreeVar _tlIelabFreeVar)
                   {-# LINE 2434 "src/InBound/AG.hs" #-}
                   )
              _lhsOelaboration =
                  ({-# LINE 97 "src/InBound/Elaboration/Term.ag" #-}
                   _hdIelaboration : _tlIelaboration
                   {-# LINE 2439 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameAttrDef =
                  ({-# LINE 42 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   _hdIrenameAttrDef ++ _tlIrenameAttrDef
                   {-# LINE 2444 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameAttrDefExpr =
                  ({-# LINE 41 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   _hdIrenameAttrDefExpr : _tlIrenameAttrDefExpr
                   {-# LINE 2449 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAtomEnv =
                  ({-# LINE 54 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsAtomEnv _tlIsAtomEnv)
                   {-# LINE 2454 "src/InBound/AG.hs" #-}
                   )
              _lhsOsFieldEnv =
                  ({-# LINE 53 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsFieldEnv _tlIsFieldEnv)
                   {-# LINE 2459 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstAttrDef =
                  ({-# LINE 52 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   _hdIsubstAttrDef : _tlIsubstAttrDef
                   {-# LINE 2464 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstVarAttrDef =
                  ({-# LINE 58 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   (mplus _hdIsubstVarAttrDef _tlIsubstVarAttrDef)
                   {-# LINE 2469 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstitutable =
                  ({-# LINE 26 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   _hdIsubstitutable || _tlIsubstitutable
                   {-# LINE 2474 "src/InBound/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2483 "src/InBound/AG.hs" #-}
                   )
              _hdOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2488 "src/InBound/AG.hs" #-}
                   )
              _hdOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2493 "src/InBound/AG.hs" #-}
                   )
              _hdOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 2498 "src/InBound/AG.hs" #-}
                   )
              _hdOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 2503 "src/InBound/AG.hs" #-}
                   )
              _hdOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 2508 "src/InBound/AG.hs" #-}
                   )
              _hdOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 2513 "src/InBound/AG.hs" #-}
                   )
              _hdOsubstVarFailAttrDefExpr =
                  ({-# LINE 56 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   _lhsIsubstVarFailAttrDefExpr
                   {-# LINE 2518 "src/InBound/AG.hs" #-}
                   )
              _hdOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 2523 "src/InBound/AG.hs" #-}
                   )
              _tlOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2528 "src/InBound/AG.hs" #-}
                   )
              _tlOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2533 "src/InBound/AG.hs" #-}
                   )
              _tlOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2538 "src/InBound/AG.hs" #-}
                   )
              _tlOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 2543 "src/InBound/AG.hs" #-}
                   )
              _tlOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrInh
                   {-# LINE 2548 "src/InBound/AG.hs" #-}
                   )
              _tlOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _lhsIlocEnvAttrSyn
                   {-# LINE 2553 "src/InBound/AG.hs" #-}
                   )
              _tlOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 2558 "src/InBound/AG.hs" #-}
                   )
              _tlOsubstVarFailAttrDefExpr =
                  ({-# LINE 53 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   _lhsIsubstVarFailAttrDefExpr
                   {-# LINE 2563 "src/InBound/AG.hs" #-}
                   )
              _tlOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 2568 "src/InBound/AG.hs" #-}
                   )
              ( _hdIelabFreeVar,_hdIelaboration,_hdIrenameAttrDef,_hdIrenameAttrDefExpr,_hdIsAtomEnv,_hdIsFieldEnv,_hdIself,_hdIsubstAttrDef,_hdIsubstVarAttrDef,_hdIsubstitutable) =
                  hd_ _hdOenvAttrInh _hdOenvAttrSyn _hdOenvSort _hdOenvVars _hdOlocEnvAttrInh _hdOlocEnvAttrSyn _hdOnamespaces _hdOsubstVarFailAttrDefExpr _hdOvarsorts
              ( _tlIelabFreeVar,_tlIelaboration,_tlIrenameAttrDef,_tlIrenameAttrDefExpr,_tlIsAtomEnv,_tlIsFieldEnv,_tlIself,_tlIsubstAttrDef,_tlIsubstVarAttrDef,_tlIsubstitutable) =
                  tl_ _tlOenvAttrInh _tlOenvAttrSyn _tlOenvSort _tlOenvVars _tlOlocEnvAttrInh _tlOlocEnvAttrSyn _tlOnamespaces _tlOsubstVarFailAttrDefExpr _tlOvarsorts
          in  ( _lhsOelabFreeVar,_lhsOelaboration,_lhsOrenameAttrDef,_lhsOrenameAttrDefExpr,_lhsOsAtomEnv,_lhsOsFieldEnv,_lhsOself,_lhsOsubstAttrDef,_lhsOsubstVarAttrDef,_lhsOsubstitutable)))
sem_CtorFieldDecls_Nil :: T_CtorFieldDecls
sem_CtorFieldDecls_Nil =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsIenvSort
       _lhsIenvVars
       _lhsIlocEnvAttrInh
       _lhsIlocEnvAttrSyn
       _lhsInamespaces
       _lhsIsubstVarFailAttrDefExpr
       _lhsIvarsorts ->
         (let _lhsOelabFreeVar :: (Map AttrRef [Ag.Expr])
              _lhsOelaboration :: (Ag.CtorFieldDecls)
              _lhsOrenameAttrDef :: (Ag.AttrDefs)
              _lhsOrenameAttrDefExpr :: (Ag.Exprs)
              _lhsOsAtomEnv :: (Map FieldName NamespaceName)
              _lhsOsFieldEnv :: (Map FieldName SortName)
              _lhsOsubstAttrDef :: (Ag.Exprs)
              _lhsOsubstVarAttrDef :: (Maybe Ag.Expr)
              _lhsOsubstitutable :: Bool
              _lhsOself :: CtorFieldDecls
              _lhsOelabFreeVar =
                  ({-# LINE 77 "src/InBound/Elaboration/FreeVar.ag" #-}
                   M.empty
                   {-# LINE 2599 "src/InBound/AG.hs" #-}
                   )
              _lhsOelaboration =
                  ({-# LINE 97 "src/InBound/Elaboration/Term.ag" #-}
                   []
                   {-# LINE 2604 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameAttrDef =
                  ({-# LINE 42 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   []
                   {-# LINE 2609 "src/InBound/AG.hs" #-}
                   )
              _lhsOrenameAttrDefExpr =
                  ({-# LINE 41 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   []
                   {-# LINE 2614 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAtomEnv =
                  ({-# LINE 54 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 2619 "src/InBound/AG.hs" #-}
                   )
              _lhsOsFieldEnv =
                  ({-# LINE 53 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 2624 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstAttrDef =
                  ({-# LINE 52 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   []
                   {-# LINE 2629 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstVarAttrDef =
                  ({-# LINE 58 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   mzero
                   {-# LINE 2634 "src/InBound/AG.hs" #-}
                   )
              _lhsOsubstitutable =
                  ({-# LINE 26 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   False
                   {-# LINE 2639 "src/InBound/AG.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOelabFreeVar,_lhsOelaboration,_lhsOrenameAttrDef,_lhsOrenameAttrDefExpr,_lhsOsAtomEnv,_lhsOsFieldEnv,_lhsOself,_lhsOsubstAttrDef,_lhsOsubstVarAttrDef,_lhsOsubstitutable)))
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
              ( AttrRef,([Ag.AttrDef]),(Map AttrRef [Ag.Expr]),([Ag.AttrDef]),([Ag.AttrDef]),([Ag.AttrDef]),Expr)
data Inh_Expr = Inh_Expr {attrDefRef_Inh_Expr :: AttrRef,depth_Inh_Expr :: Int,envAtom_Inh_Expr :: (Map FieldName NamespaceName),envAttrInh_Inh_Expr :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_Expr :: (Map (SortName,AttrName) Type),envField_Inh_Expr :: (Map FieldName SortName),envSort_Inh_Expr :: SortName,envVars_Inh_Expr :: NamespaceNames}
data Syn_Expr = Syn_Expr {attrName_Syn_Expr :: AttrRef,elabContext_Syn_Expr :: ([Ag.AttrDef]),elabFreeVar_Syn_Expr :: (Map AttrRef [Ag.Expr]),elabRenameContext_Syn_Expr :: ([Ag.AttrDef]),elabRenameMap_Syn_Expr :: ([Ag.AttrDef]),elabSubstMap_Syn_Expr :: ([Ag.AttrDef]),self_Syn_Expr :: Expr}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr _lhsIattrDefRef _lhsIdepth _lhsIenvAtom _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvField _lhsIenvSort _lhsIenvVars) =
    (let ( _lhsOattrName,_lhsOelabContext,_lhsOelabFreeVar,_lhsOelabRenameContext,_lhsOelabRenameMap,_lhsOelabSubstMap,_lhsOself) = sem _lhsIattrDefRef _lhsIdepth _lhsIenvAtom _lhsIenvAttrInh _lhsIenvAttrSyn _lhsIenvField _lhsIenvSort _lhsIenvVars
     in  (Syn_Expr _lhsOattrName _lhsOelabContext _lhsOelabFreeVar _lhsOelabRenameContext _lhsOelabRenameMap _lhsOelabSubstMap _lhsOself))
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
         (let _lhsOelabSubstMap :: ([Ag.AttrDef])
              _lhsOelabRenameMap :: ([Ag.AttrDef])
              _lhsOelabRenameContext :: ([Ag.AttrDef])
              _lhsOelabFreeVar :: (Map AttrRef [Ag.Expr])
              _lhsOelabContext :: ([Ag.AttrDef])
              _lhsOself :: Expr
              _lhsOattrName :: AttrRef
              _attrRefNameIself :: AttrRef
              _lhsOelabSubstMap =
                  ({-# LINE 62 "src/InBound/Elaboration/SubstMap.ag" #-}
                   []
                   {-# LINE 2696 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabRenameMap =
                  ({-# LINE 59 "src/InBound/Elaboration/RenameMap.ag" #-}
                   []
                   {-# LINE 2701 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabRenameContext =
                  ({-# LINE 61 "src/InBound/Elaboration/RenameContext.ag" #-}
                   []
                   {-# LINE 2706 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabFreeVar =
                  ({-# LINE 55 "src/InBound/Elaboration/FreeVar.ag" #-}
                   M.empty
                   {-# LINE 2711 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabContext =
                  ({-# LINE 53 "src/InBound/Elaboration/Context.ag" #-}
                   []
                   {-# LINE 2716 "src/InBound/AG.hs" #-}
                   )
              _attrName =
                  ({-# LINE 221 "src/InBound/Environment.ag" #-}
                   _attrRefNameIself
                   {-# LINE 2721 "src/InBound/AG.hs" #-}
                   )
              _self =
                  ExprAttrRef _attrRefNameIself
              _lhsOself =
                  _self
              _lhsOattrName =
                  ({-# LINE 188 "src/InBound/Environment.ag" #-}
                   _attrName
                   {-# LINE 2730 "src/InBound/AG.hs" #-}
                   )
              ( _attrRefNameIself) =
                  attrRefName_
          in  ( _lhsOattrName,_lhsOelabContext,_lhsOelabFreeVar,_lhsOelabRenameContext,_lhsOelabRenameMap,_lhsOelabSubstMap,_lhsOself)))
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
         (let _lhsOelabSubstMap :: ([Ag.AttrDef])
              _lhsOelabRenameMap :: ([Ag.AttrDef])
              _lhsOelabRenameContext :: ([Ag.AttrDef])
              _lhsOelabFreeVar :: (Map AttrRef [Ag.Expr])
              _lhsOelabContext :: ([Ag.AttrDef])
              _lhsOself :: Expr
              _lhsOattrName :: AttrRef
              _lhsOelabSubstMap =
                  ({-# LINE 64 "src/InBound/Elaboration/SubstMap.ag" #-}
                   [ Ag.AttrDef
                       (attrRefToSubstMapRef _attrName    )
                       Ag.ExprSetEmpty
                   ]
                   {-# LINE 2758 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabRenameMap =
                  ({-# LINE 61 "src/InBound/Elaboration/RenameMap.ag" #-}
                   [ Ag.AttrDef
                       (attrRefToRenameMapRef _attrName    )
                       Ag.ExprSetEmpty
                   ]
                   {-# LINE 2766 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabRenameContext =
                  ({-# LINE 63 "src/InBound/Elaboration/RenameContext.ag" #-}
                   [ Ag.AttrDef
                       (attrRefToRenameContextRef _attrName    )
                       Ag.ExprSetEmpty
                   ]
                   {-# LINE 2774 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabFreeVar =
                  ({-# LINE 55 "src/InBound/Elaboration/FreeVar.ag" #-}
                   M.empty
                   {-# LINE 2779 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabContext =
                  ({-# LINE 55 "src/InBound/Elaboration/Context.ag" #-}
                   [ Ag.AttrDef
                     (attrRefToContextRef _attrName    )
                     Ag.ExprSetEmpty
                   ]
                   {-# LINE 2787 "src/InBound/AG.hs" #-}
                   )
              _attrName =
                  ({-# LINE 223 "src/InBound/Environment.ag" #-}
                   localAttrRef _lhsIattrDefRef _lhsIdepth
                   {-# LINE 2792 "src/InBound/AG.hs" #-}
                   )
              _self =
                  ExprNil
              _lhsOself =
                  _self
              _lhsOattrName =
                  ({-# LINE 188 "src/InBound/Environment.ag" #-}
                   _attrName
                   {-# LINE 2801 "src/InBound/AG.hs" #-}
                   )
          in  ( _lhsOattrName,_lhsOelabContext,_lhsOelabFreeVar,_lhsOelabRenameContext,_lhsOelabRenameMap,_lhsOelabSubstMap,_lhsOself)))
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
         (let _lhsOelabFreeVar :: (Map AttrRef [Ag.Expr])
              _lhsOelabSubstMap :: ([Ag.AttrDef])
              _lhsOelabRenameMap :: ([Ag.AttrDef])
              _lhsOelabRenameContext :: ([Ag.AttrDef])
              _lhsOelabContext :: ([Ag.AttrDef])
              _tailOdepth :: Int
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
              _tailIelabContext :: ([Ag.AttrDef])
              _tailIelabFreeVar :: (Map AttrRef [Ag.Expr])
              _tailIelabRenameContext :: ([Ag.AttrDef])
              _tailIelabRenameMap :: ([Ag.AttrDef])
              _tailIelabSubstMap :: ([Ag.AttrDef])
              _tailIself :: Expr
              _lhsOelabFreeVar =
                  ({-# LINE 57 "src/InBound/Elaboration/FreeVar.ag" #-}
                   foldr ($) _elabFreeVar_augmented_syn [_elabFreeVar_augmented_f1]
                   {-# LINE 2841 "src/InBound/AG.hs" #-}
                   )
              _elabFreeVar_augmented_f1 =
                  ({-# LINE 57 "src/InBound/Elaboration/FreeVar.ag" #-}
                   M.insertWith (++)
                     _tailIattrName
                     [ Ag.ExprSetDifference
                       (Ag.ExprAttrRef $
                        attrRefToFreeVarRef _attrName    )
                       (Ag.ExprSetSingleton $
                        Ag.ExprField head_)
                     ]
                   {-# LINE 2853 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabSubstMap =
                  ({-# LINE 70 "src/InBound/Elaboration/SubstMap.ag" #-}
                   [ Ag.AttrDef
                       (attrRefToSubstMapRef _attrName    )
                       (Ag.ExprAttrRef $
                          attrRefToSubstMapRef _tailIattrName)
                   ] ++ _tailIelabSubstMap
                   {-# LINE 2862 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabRenameMap =
                  ({-# LINE 67 "src/InBound/Elaboration/RenameMap.ag" #-}
                   [ Ag.AttrDef
                       (attrRefToRenameMapRef _attrName    )
                       (Ag.ExprMapInsert
                          (Ag.ExprField head_)
                          (Ag.ExprAttrRef $ renamedFieldRef head_)
                          (Ag.ExprAttrRef $
                             attrRefToRenameMapRef _tailIattrName))
                   ] ++ _tailIelabRenameMap
                   {-# LINE 2874 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabRenameContext =
                  ({-# LINE 69 "src/InBound/Elaboration/RenameContext.ag" #-}
                   [ Ag.AttrDef
                       (renamedFieldRef head_)
                       (Ag.ExprFresh . Ag.ExprAttrRef $
                          attrRefToRenameContextRef _tailIattrName)
                   , Ag.AttrDef
                       (attrRefToRenameContextRef _attrName    )
                       (Ag.ExprSetInsert
                          (Ag.ExprAttrRef $ renamedFieldRef head_)
                          (Ag.ExprAttrRef $
                             attrRefToRenameContextRef _tailIattrName))
                   ] ++ _tailIelabRenameContext
                   {-# LINE 2889 "src/InBound/AG.hs" #-}
                   )
              _lhsOelabContext =
                  ({-# LINE 61 "src/InBound/Elaboration/Context.ag" #-}
                   [ Ag.AttrDef
                       (attrRefToContextRef _attrName    )
                       (Ag.ExprSetInsert
                          (Ag.ExprField head_)
                          (Ag.ExprAttrRef $
                             attrRefToContextRef _tailIattrName))
                   ] ++ _tailIelabContext
                   {-# LINE 2900 "src/InBound/AG.hs" #-}
                   )
              _attrName =
                  ({-# LINE 225 "src/InBound/Environment.ag" #-}
                   localAttrRef _lhsIattrDefRef _lhsIdepth
                   {-# LINE 2905 "src/InBound/AG.hs" #-}
                   )
              _tailOdepth =
                  ({-# LINE 226 "src/InBound/Environment.ag" #-}
                   1 + _lhsIdepth
                   {-# LINE 2910 "src/InBound/AG.hs" #-}
                   )
              _self =
                  ExprCons _tailIself head_
              _lhsOself =
                  _self
              _lhsOattrName =
                  ({-# LINE 188 "src/InBound/Environment.ag" #-}
                   _attrName
                   {-# LINE 2919 "src/InBound/AG.hs" #-}
                   )
              _elabFreeVar_augmented_syn =
                  ({-# LINE 57 "src/InBound/Elaboration/FreeVar.ag" #-}
                   _tailIelabFreeVar
                   {-# LINE 2924 "src/InBound/AG.hs" #-}
                   )
              _tailOattrDefRef =
                  ({-# LINE 185 "src/InBound/Environment.ag" #-}
                   _lhsIattrDefRef
                   {-# LINE 2929 "src/InBound/AG.hs" #-}
                   )
              _tailOenvAtom =
                  ({-# LINE 62 "src/InBound/Environment.ag" #-}
                   _lhsIenvAtom
                   {-# LINE 2934 "src/InBound/AG.hs" #-}
                   )
              _tailOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 2939 "src/InBound/AG.hs" #-}
                   )
              _tailOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 2944 "src/InBound/AG.hs" #-}
                   )
              _tailOenvField =
                  ({-# LINE 61 "src/InBound/Environment.ag" #-}
                   _lhsIenvField
                   {-# LINE 2949 "src/InBound/AG.hs" #-}
                   )
              _tailOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _lhsIenvSort
                   {-# LINE 2954 "src/InBound/AG.hs" #-}
                   )
              _tailOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _lhsIenvVars
                   {-# LINE 2959 "src/InBound/AG.hs" #-}
                   )
              ( _tailIattrName,_tailIelabContext,_tailIelabFreeVar,_tailIelabRenameContext,_tailIelabRenameMap,_tailIelabSubstMap,_tailIself) =
                  tail_ _tailOattrDefRef _tailOdepth _tailOenvAtom _tailOenvAttrInh _tailOenvAttrSyn _tailOenvField _tailOenvSort _tailOenvVars
          in  ( _lhsOattrName,_lhsOelabContext,_lhsOelabFreeVar,_lhsOelabRenameContext,_lhsOelabRenameMap,_lhsOelabSubstMap,_lhsOself)))
-- LocAttrDecl -------------------------------------------------
-- cata
sem_LocAttrDecl :: LocAttrDecl ->
                   T_LocAttrDecl
sem_LocAttrDecl (LocAttrDecl _attrName _attrType) =
    (sem_LocAttrDecl_LocAttrDecl _attrName (sem_Type _attrType))
-- semantic domain
type T_LocAttrDecl = (Map NamespaceName MbSortName) ->
                     (Map SortName [NamespaceName]) ->
                     ( LocAttrDecl)
data Inh_LocAttrDecl = Inh_LocAttrDecl {namespaces_Inh_LocAttrDecl :: (Map NamespaceName MbSortName),varsorts_Inh_LocAttrDecl :: (Map SortName [NamespaceName])}
data Syn_LocAttrDecl = Syn_LocAttrDecl {self_Syn_LocAttrDecl :: LocAttrDecl}
wrap_LocAttrDecl :: T_LocAttrDecl ->
                    Inh_LocAttrDecl ->
                    Syn_LocAttrDecl
wrap_LocAttrDecl sem (Inh_LocAttrDecl _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOself) = sem _lhsInamespaces _lhsIvarsorts
     in  (Syn_LocAttrDecl _lhsOself))
sem_LocAttrDecl_LocAttrDecl :: AttrName ->
                               T_Type ->
                               T_LocAttrDecl
sem_LocAttrDecl_LocAttrDecl attrName_ attrType_ =
    (\ _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOself :: LocAttrDecl
              _attrTypeIself :: Type
              _self =
                  LocAttrDecl attrName_ _attrTypeIself
              _lhsOself =
                  _self
              ( _attrTypeIself) =
                  attrType_
          in  ( _lhsOself)))
-- LocAttrDecls ------------------------------------------------
-- cata
sem_LocAttrDecls :: LocAttrDecls ->
                    T_LocAttrDecls
sem_LocAttrDecls list =
    (Prelude.foldr sem_LocAttrDecls_Cons sem_LocAttrDecls_Nil (Prelude.map sem_LocAttrDecl list))
-- semantic domain
type T_LocAttrDecls = (Map NamespaceName MbSortName) ->
                      (Map SortName [NamespaceName]) ->
                      ( LocAttrDecls)
data Inh_LocAttrDecls = Inh_LocAttrDecls {namespaces_Inh_LocAttrDecls :: (Map NamespaceName MbSortName),varsorts_Inh_LocAttrDecls :: (Map SortName [NamespaceName])}
data Syn_LocAttrDecls = Syn_LocAttrDecls {self_Syn_LocAttrDecls :: LocAttrDecls}
wrap_LocAttrDecls :: T_LocAttrDecls ->
                     Inh_LocAttrDecls ->
                     Syn_LocAttrDecls
wrap_LocAttrDecls sem (Inh_LocAttrDecls _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOself) = sem _lhsInamespaces _lhsIvarsorts
     in  (Syn_LocAttrDecls _lhsOself))
sem_LocAttrDecls_Cons :: T_LocAttrDecl ->
                         T_LocAttrDecls ->
                         T_LocAttrDecls
sem_LocAttrDecls_Cons hd_ tl_ =
    (\ _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOself :: LocAttrDecls
              _hdOnamespaces :: (Map NamespaceName MbSortName)
              _hdOvarsorts :: (Map SortName [NamespaceName])
              _tlOnamespaces :: (Map NamespaceName MbSortName)
              _tlOvarsorts :: (Map SortName [NamespaceName])
              _hdIself :: LocAttrDecl
              _tlIself :: LocAttrDecls
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 3035 "src/InBound/AG.hs" #-}
                   )
              _hdOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 3040 "src/InBound/AG.hs" #-}
                   )
              _tlOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 3045 "src/InBound/AG.hs" #-}
                   )
              _tlOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 3050 "src/InBound/AG.hs" #-}
                   )
              ( _hdIself) =
                  hd_ _hdOnamespaces _hdOvarsorts
              ( _tlIself) =
                  tl_ _tlOnamespaces _tlOvarsorts
          in  ( _lhsOself)))
sem_LocAttrDecls_Nil :: T_LocAttrDecls
sem_LocAttrDecls_Nil =
    (\ _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOself :: LocAttrDecls
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOself)))
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
type T_NamespaceDecl = ( (Ag.Synonym),(Map NamespaceName MbSortName),NamespaceDecl)
data Inh_NamespaceDecl = Inh_NamespaceDecl {}
data Syn_NamespaceDecl = Syn_NamespaceDecl {elaboration_Syn_NamespaceDecl :: (Ag.Synonym),namespaces_Syn_NamespaceDecl :: (Map NamespaceName MbSortName),self_Syn_NamespaceDecl :: NamespaceDecl}
wrap_NamespaceDecl :: T_NamespaceDecl ->
                      Inh_NamespaceDecl ->
                      Syn_NamespaceDecl
wrap_NamespaceDecl sem (Inh_NamespaceDecl) =
    (let ( _lhsOelaboration,_lhsOnamespaces,_lhsOself) = sem
     in  (Syn_NamespaceDecl _lhsOelaboration _lhsOnamespaces _lhsOself))
sem_NamespaceDecl_NamespaceDecl :: NamespaceName ->
                                   T_MbSortName ->
                                   T_NamespaceDecl
sem_NamespaceDecl_NamespaceDecl namespaceName_ namespaceTarget_ =
    (let _lhsOelaboration :: (Ag.Synonym)
         _lhsOnamespaces :: (Map NamespaceName MbSortName)
         _lhsOself :: NamespaceDecl
         _namespaceTargetIself :: MbSortName
         _lhsOelaboration =
             ({-# LINE 45 "src/InBound/Elaboration/Term.ag" #-}
              Ag.Synonym (fromNN namespaceName_) Ag.TString
              {-# LINE 3129 "src/InBound/AG.hs" #-}
              )
         _lhsOnamespaces =
             ({-# LINE 21 "src/InBound/Environment.ag" #-}
              M.singleton namespaceName_ _namespaceTargetIself
              {-# LINE 3134 "src/InBound/AG.hs" #-}
              )
         _self =
             NamespaceDecl namespaceName_ _namespaceTargetIself
         _lhsOself =
             _self
         ( _namespaceTargetIself) =
             namespaceTarget_
     in  ( _lhsOelaboration,_lhsOnamespaces,_lhsOself))
-- NamespaceDecls ----------------------------------------------
-- cata
sem_NamespaceDecls :: NamespaceDecls ->
                      T_NamespaceDecls
sem_NamespaceDecls list =
    (Prelude.foldr sem_NamespaceDecls_Cons sem_NamespaceDecls_Nil (Prelude.map sem_NamespaceDecl list))
-- semantic domain
type T_NamespaceDecls = ( (Ag.Synonyms),(Map NamespaceName MbSortName),NamespaceDecls)
data Inh_NamespaceDecls = Inh_NamespaceDecls {}
data Syn_NamespaceDecls = Syn_NamespaceDecls {elaboration_Syn_NamespaceDecls :: (Ag.Synonyms),namespaces_Syn_NamespaceDecls :: (Map NamespaceName MbSortName),self_Syn_NamespaceDecls :: NamespaceDecls}
wrap_NamespaceDecls :: T_NamespaceDecls ->
                       Inh_NamespaceDecls ->
                       Syn_NamespaceDecls
wrap_NamespaceDecls sem (Inh_NamespaceDecls) =
    (let ( _lhsOelaboration,_lhsOnamespaces,_lhsOself) = sem
     in  (Syn_NamespaceDecls _lhsOelaboration _lhsOnamespaces _lhsOself))
sem_NamespaceDecls_Cons :: T_NamespaceDecl ->
                           T_NamespaceDecls ->
                           T_NamespaceDecls
sem_NamespaceDecls_Cons hd_ tl_ =
    (let _lhsOelaboration :: (Ag.Synonyms)
         _lhsOnamespaces :: (Map NamespaceName MbSortName)
         _lhsOself :: NamespaceDecls
         _hdIelaboration :: (Ag.Synonym)
         _hdInamespaces :: (Map NamespaceName MbSortName)
         _hdIself :: NamespaceDecl
         _tlIelaboration :: (Ag.Synonyms)
         _tlInamespaces :: (Map NamespaceName MbSortName)
         _tlIself :: NamespaceDecls
         _lhsOelaboration =
             ({-# LINE 39 "src/InBound/Elaboration/Term.ag" #-}
              _hdIelaboration : _tlIelaboration
              {-# LINE 3175 "src/InBound/AG.hs" #-}
              )
         _lhsOnamespaces =
             ({-# LINE 17 "src/InBound/Environment.ag" #-}
              (M.union _hdInamespaces _tlInamespaces)
              {-# LINE 3180 "src/InBound/AG.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIelaboration,_hdInamespaces,_hdIself) =
             hd_
         ( _tlIelaboration,_tlInamespaces,_tlIself) =
             tl_
     in  ( _lhsOelaboration,_lhsOnamespaces,_lhsOself))
sem_NamespaceDecls_Nil :: T_NamespaceDecls
sem_NamespaceDecls_Nil =
    (let _lhsOelaboration :: (Ag.Synonyms)
         _lhsOnamespaces :: (Map NamespaceName MbSortName)
         _lhsOself :: NamespaceDecls
         _lhsOelaboration =
             ({-# LINE 39 "src/InBound/Elaboration/Term.ag" #-}
              []
              {-# LINE 3199 "src/InBound/AG.hs" #-}
              )
         _lhsOnamespaces =
             ({-# LINE 17 "src/InBound/Environment.ag" #-}
              M.empty
              {-# LINE 3204 "src/InBound/AG.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOelaboration,_lhsOnamespaces,_lhsOself))
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
type T_NodeLabel = ( NodeLabel)
data Inh_NodeLabel = Inh_NodeLabel {}
data Syn_NodeLabel = Syn_NodeLabel {self_Syn_NodeLabel :: NodeLabel}
wrap_NodeLabel :: T_NodeLabel ->
                  Inh_NodeLabel ->
                  Syn_NodeLabel
wrap_NodeLabel sem (Inh_NodeLabel) =
    (let ( _lhsOself) = sem
     in  (Syn_NodeLabel _lhsOself))
sem_NodeLabel_Lhs :: T_NodeLabel
sem_NodeLabel_Lhs =
    (let _lhsOself :: NodeLabel
         _self =
             Lhs
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_NodeLabel_Loc :: T_NodeLabel
sem_NodeLabel_Loc =
    (let _lhsOself :: NodeLabel
         _self =
             Loc
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_NodeLabel_Sub :: FieldName ->
                     T_NodeLabel
sem_NodeLabel_Sub nodeFieldLabel_ =
    (let _lhsOself :: NodeLabel
         _self =
             Sub nodeFieldLabel_
         _lhsOself =
             _self
     in  ( _lhsOself))
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
                  ( (Ag.SortDecl),(Map (SortName,AttrName) Type),(Map (SortName,AttrName) Type),SortDecl)
data Inh_SortDecl = Inh_SortDecl {envAttrInh_Inh_SortDecl :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_SortDecl :: (Map (SortName,AttrName) Type),namespaces_Inh_SortDecl :: (Map NamespaceName MbSortName),varsorts_Inh_SortDecl :: (Map SortName [NamespaceName])}
data Syn_SortDecl = Syn_SortDecl {elaboration_Syn_SortDecl :: (Ag.SortDecl),sAttrInh_Syn_SortDecl :: (Map (SortName,AttrName) Type),sAttrSyn_Syn_SortDecl :: (Map (SortName,AttrName) Type),self_Syn_SortDecl :: SortDecl}
wrap_SortDecl :: T_SortDecl ->
                 Inh_SortDecl ->
                 Syn_SortDecl
wrap_SortDecl sem (Inh_SortDecl _lhsIenvAttrInh _lhsIenvAttrSyn _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOelaboration,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsInamespaces _lhsIvarsorts
     in  (Syn_SortDecl _lhsOelaboration _lhsOsAttrInh _lhsOsAttrSyn _lhsOself))
sem_SortDecl_SortDecl :: SortName ->
                         T_AttrDecls ->
                         T_CtorDecls ->
                         T_SortDecl
sem_SortDecl_SortDecl sortName_ sortAttributes_ sortCtors_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOelaboration :: (Ag.SortDecl)
              _lhsOsAttrSyn :: (Map (SortName,AttrName) Type)
              _lhsOsAttrInh :: (Map (SortName,AttrName) Type)
              _sortCtorsOenvSetAttrDef :: (Map AttrRef Type)
              _sortCtorsOenvSetAttrUse :: (Map AttrRef Type)
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
              _sortAttributesIelaboration :: (Ag.AttrDecls)
              _sortAttributesIfreeVarFunInh :: (Ag.AttrNameTypes)
              _sortAttributesIfreeVarFunSyn :: (Ag.AttrNameTypes)
              _sortAttributesIrenameFunInh :: (Ag.AttrNameTypes)
              _sortAttributesIrenameFunSyn :: (Ag.AttrNameTypes)
              _sortAttributesIsAttrInh :: (Map AttrName Type)
              _sortAttributesIsAttrSyn :: (Map AttrName Type)
              _sortAttributesIself :: AttrDecls
              _sortAttributesIsubstFunInh :: (Ag.AttrNameTypes)
              _sortAttributesIsubstFunSyn :: (Ag.AttrNameTypes)
              _sortCtorsIelaboration :: (Ag.CtorDecls)
              _sortCtorsIsAttrInh :: (Map (SortName,AttrName) Type)
              _sortCtorsIsAttrSyn :: (Map (SortName,AttrName) Type)
              _sortCtorsIsSetAttrDef :: (Map AttrRef Expr)
              _sortCtorsIself :: CtorDecls
              _substAttrDecl =
                  ({-# LINE 36 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   [ Ag.SynDecl
                       substAN
                       (Ag.TSort sortName_)
                   ]
                   {-# LINE 3361 "src/InBound/AG.hs" #-}
                   )
              _substFun =
                  ({-# LINE 107 "src/InBound/Elaboration/SubstSynthesis.ag" #-}
                   Ag.SemFun
                     ("subst_" ++ fromSN sortName_)
                     _sortAttributesIsubstFunInh
                     ((substAN, Ag.TSort sortName_):
                        _sortAttributesIsubstFunSyn)
                   {-# LINE 3370 "src/InBound/AG.hs" #-}
                   )
              _renameAttrDecl =
                  ({-# LINE 25 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   [ Ag.SynDecl
                       renameAN
                       (Ag.TSort sortName_)
                   ]
                   {-# LINE 3378 "src/InBound/AG.hs" #-}
                   )
              _renameFun =
                  ({-# LINE 96 "src/InBound/Elaboration/RenameSynthesis.ag" #-}
                   Ag.SemFun
                     ("rename_" ++ fromSN sortName_)
                     _sortAttributesIrenameFunInh
                     ((renameAN, Ag.TSort sortName_):
                        _sortAttributesIrenameFunSyn)
                   {-# LINE 3387 "src/InBound/AG.hs" #-}
                   )
              _freeVarFun =
                  ({-# LINE 114 "src/InBound/Elaboration/FreeVar.ag" #-}
                   Ag.SemFun
                     ("fvs_" ++ fromSN sortName_)
                     _sortAttributesIfreeVarFunInh
                     _sortAttributesIfreeVarFunSyn
                   {-# LINE 3395 "src/InBound/AG.hs" #-}
                   )
              _lhsOelaboration =
                  ({-# LINE 56 "src/InBound/Elaboration/Term.ag" #-}
                   Ag.SortDecl
                     sortName_
                     (_sortAttributesIelaboration ++
                      _renameAttrDecl     ++
                      _substAttrDecl    )
                     _sortCtorsIelaboration
                     [_freeVarFun    , _renameFun    , _substFun    ]
                   {-# LINE 3406 "src/InBound/AG.hs" #-}
                   )
              _envSort =
                  ({-# LINE 48 "src/InBound/Environment.ag" #-}
                   sortName_
                   {-# LINE 3411 "src/InBound/AG.hs" #-}
                   )
              _envVars =
                  ({-# LINE 49 "src/InBound/Environment.ag" #-}
                   M.findWithDefault [] sortName_ _lhsIvarsorts
                   {-# LINE 3416 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 104 "src/InBound/Environment.ag" #-}
                   M.mapKeysMonotonic
                     (\an -> (sortName_,an))
                     _sortAttributesIsAttrSyn
                   {-# LINE 3423 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 107 "src/InBound/Environment.ag" #-}
                   M.mapKeysMonotonic
                     (\an -> (sortName_,an))
                     _sortAttributesIsAttrInh
                   {-# LINE 3430 "src/InBound/AG.hs" #-}
                   )
              _locEnvAttrSyn =
                  ({-# LINE 113 "src/InBound/Environment.ag" #-}
                   _sortAttributesIsAttrSyn
                   {-# LINE 3435 "src/InBound/AG.hs" #-}
                   )
              _locEnvAttrInh =
                  ({-# LINE 114 "src/InBound/Environment.ag" #-}
                   _sortAttributesIsAttrInh
                   {-# LINE 3440 "src/InBound/AG.hs" #-}
                   )
              _sortCtorsOenvSetAttrDef =
                  ({-# LINE 157 "src/InBound/Environment.ag" #-}
                   M.mapKeysMonotonic (\an -> AttrRef Lhs an) _locEnvAttrSyn
                   {-# LINE 3445 "src/InBound/AG.hs" #-}
                   )
              _sortCtorsOenvSetAttrUse =
                  ({-# LINE 160 "src/InBound/Environment.ag" #-}
                   M.mapKeysMonotonic (\an -> AttrRef Lhs an) _locEnvAttrInh
                   {-# LINE 3450 "src/InBound/AG.hs" #-}
                   )
              _self =
                  SortDecl sortName_ _sortAttributesIself _sortCtorsIself
              _lhsOself =
                  _self
              _sortAttributesOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _envSort
                   {-# LINE 3459 "src/InBound/AG.hs" #-}
                   )
              _sortAttributesOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _envVars
                   {-# LINE 3464 "src/InBound/AG.hs" #-}
                   )
              _sortAttributesOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 3469 "src/InBound/AG.hs" #-}
                   )
              _sortAttributesOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 3474 "src/InBound/AG.hs" #-}
                   )
              _sortCtorsOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 3479 "src/InBound/AG.hs" #-}
                   )
              _sortCtorsOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 3484 "src/InBound/AG.hs" #-}
                   )
              _sortCtorsOenvSort =
                  ({-# LINE 42 "src/InBound/Environment.ag" #-}
                   _envSort
                   {-# LINE 3489 "src/InBound/AG.hs" #-}
                   )
              _sortCtorsOenvVars =
                  ({-# LINE 44 "src/InBound/Environment.ag" #-}
                   _envVars
                   {-# LINE 3494 "src/InBound/AG.hs" #-}
                   )
              _sortCtorsOlocEnvAttrInh =
                  ({-# LINE 89 "src/InBound/Environment.ag" #-}
                   _locEnvAttrInh
                   {-# LINE 3499 "src/InBound/AG.hs" #-}
                   )
              _sortCtorsOlocEnvAttrSyn =
                  ({-# LINE 88 "src/InBound/Environment.ag" #-}
                   _locEnvAttrSyn
                   {-# LINE 3504 "src/InBound/AG.hs" #-}
                   )
              _sortCtorsOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 3509 "src/InBound/AG.hs" #-}
                   )
              _sortCtorsOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 3514 "src/InBound/AG.hs" #-}
                   )
              ( _sortAttributesIelaboration,_sortAttributesIfreeVarFunInh,_sortAttributesIfreeVarFunSyn,_sortAttributesIrenameFunInh,_sortAttributesIrenameFunSyn,_sortAttributesIsAttrInh,_sortAttributesIsAttrSyn,_sortAttributesIself,_sortAttributesIsubstFunInh,_sortAttributesIsubstFunSyn) =
                  sortAttributes_ _sortAttributesOenvSort _sortAttributesOenvVars _sortAttributesOnamespaces _sortAttributesOvarsorts
              ( _sortCtorsIelaboration,_sortCtorsIsAttrInh,_sortCtorsIsAttrSyn,_sortCtorsIsSetAttrDef,_sortCtorsIself) =
                  sortCtors_ _sortCtorsOenvAttrInh _sortCtorsOenvAttrSyn _sortCtorsOenvSetAttrDef _sortCtorsOenvSetAttrUse _sortCtorsOenvSort _sortCtorsOenvVars _sortCtorsOlocEnvAttrInh _sortCtorsOlocEnvAttrSyn _sortCtorsOnamespaces _sortCtorsOvarsorts
          in  ( _lhsOelaboration,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself)))
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
                   ( (Ag.SortDecls),(Map (SortName,AttrName) Type),(Map (SortName,AttrName) Type),SortDecls)
data Inh_SortDecls = Inh_SortDecls {envAttrInh_Inh_SortDecls :: (Map (SortName,AttrName) Type),envAttrSyn_Inh_SortDecls :: (Map (SortName,AttrName) Type),namespaces_Inh_SortDecls :: (Map NamespaceName MbSortName),varsorts_Inh_SortDecls :: (Map SortName [NamespaceName])}
data Syn_SortDecls = Syn_SortDecls {elaboration_Syn_SortDecls :: (Ag.SortDecls),sAttrInh_Syn_SortDecls :: (Map (SortName,AttrName) Type),sAttrSyn_Syn_SortDecls :: (Map (SortName,AttrName) Type),self_Syn_SortDecls :: SortDecls}
wrap_SortDecls :: T_SortDecls ->
                  Inh_SortDecls ->
                  Syn_SortDecls
wrap_SortDecls sem (Inh_SortDecls _lhsIenvAttrInh _lhsIenvAttrSyn _lhsInamespaces _lhsIvarsorts) =
    (let ( _lhsOelaboration,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself) = sem _lhsIenvAttrInh _lhsIenvAttrSyn _lhsInamespaces _lhsIvarsorts
     in  (Syn_SortDecls _lhsOelaboration _lhsOsAttrInh _lhsOsAttrSyn _lhsOself))
sem_SortDecls_Cons :: T_SortDecl ->
                      T_SortDecls ->
                      T_SortDecls
sem_SortDecls_Cons hd_ tl_ =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOelaboration :: (Ag.SortDecls)
              _lhsOsAttrInh :: (Map (SortName,AttrName) Type)
              _lhsOsAttrSyn :: (Map (SortName,AttrName) Type)
              _lhsOself :: SortDecls
              _hdOenvAttrInh :: (Map (SortName,AttrName) Type)
              _hdOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _hdOnamespaces :: (Map NamespaceName MbSortName)
              _hdOvarsorts :: (Map SortName [NamespaceName])
              _tlOenvAttrInh :: (Map (SortName,AttrName) Type)
              _tlOenvAttrSyn :: (Map (SortName,AttrName) Type)
              _tlOnamespaces :: (Map NamespaceName MbSortName)
              _tlOvarsorts :: (Map SortName [NamespaceName])
              _hdIelaboration :: (Ag.SortDecl)
              _hdIsAttrInh :: (Map (SortName,AttrName) Type)
              _hdIsAttrSyn :: (Map (SortName,AttrName) Type)
              _hdIself :: SortDecl
              _tlIelaboration :: (Ag.SortDecls)
              _tlIsAttrInh :: (Map (SortName,AttrName) Type)
              _tlIsAttrSyn :: (Map (SortName,AttrName) Type)
              _tlIself :: SortDecls
              _lhsOelaboration =
                  ({-# LINE 50 "src/InBound/Elaboration/Term.ag" #-}
                   _hdIelaboration : _tlIelaboration
                   {-# LINE 3572 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 83 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsAttrInh _tlIsAttrInh)
                   {-# LINE 3577 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 82 "src/InBound/Environment.ag" #-}
                   (M.union _hdIsAttrSyn _tlIsAttrSyn)
                   {-# LINE 3582 "src/InBound/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 3591 "src/InBound/AG.hs" #-}
                   )
              _hdOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 3596 "src/InBound/AG.hs" #-}
                   )
              _hdOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 3601 "src/InBound/AG.hs" #-}
                   )
              _hdOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 3606 "src/InBound/AG.hs" #-}
                   )
              _tlOenvAttrInh =
                  ({-# LINE 96 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrInh
                   {-# LINE 3611 "src/InBound/AG.hs" #-}
                   )
              _tlOenvAttrSyn =
                  ({-# LINE 95 "src/InBound/Environment.ag" #-}
                   _lhsIenvAttrSyn
                   {-# LINE 3616 "src/InBound/AG.hs" #-}
                   )
              _tlOnamespaces =
                  ({-# LINE 26 "src/InBound/Environment.ag" #-}
                   _lhsInamespaces
                   {-# LINE 3621 "src/InBound/AG.hs" #-}
                   )
              _tlOvarsorts =
                  ({-# LINE 28 "src/InBound/Environment.ag" #-}
                   _lhsIvarsorts
                   {-# LINE 3626 "src/InBound/AG.hs" #-}
                   )
              ( _hdIelaboration,_hdIsAttrInh,_hdIsAttrSyn,_hdIself) =
                  hd_ _hdOenvAttrInh _hdOenvAttrSyn _hdOnamespaces _hdOvarsorts
              ( _tlIelaboration,_tlIsAttrInh,_tlIsAttrSyn,_tlIself) =
                  tl_ _tlOenvAttrInh _tlOenvAttrSyn _tlOnamespaces _tlOvarsorts
          in  ( _lhsOelaboration,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself)))
sem_SortDecls_Nil :: T_SortDecls
sem_SortDecls_Nil =
    (\ _lhsIenvAttrInh
       _lhsIenvAttrSyn
       _lhsInamespaces
       _lhsIvarsorts ->
         (let _lhsOelaboration :: (Ag.SortDecls)
              _lhsOsAttrInh :: (Map (SortName,AttrName) Type)
              _lhsOsAttrSyn :: (Map (SortName,AttrName) Type)
              _lhsOself :: SortDecls
              _lhsOelaboration =
                  ({-# LINE 50 "src/InBound/Elaboration/Term.ag" #-}
                   []
                   {-# LINE 3646 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrInh =
                  ({-# LINE 83 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 3651 "src/InBound/AG.hs" #-}
                   )
              _lhsOsAttrSyn =
                  ({-# LINE 82 "src/InBound/Environment.ag" #-}
                   M.empty
                   {-# LINE 3656 "src/InBound/AG.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOelaboration,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself)))
-- Specification -----------------------------------------------
-- cata
sem_Specification :: Specification ->
                     T_Specification
sem_Specification (Specification _specModuleName _specNamespaceDecls _specSortDecls) =
    (sem_Specification_Specification _specModuleName (sem_NamespaceDecls _specNamespaceDecls) (sem_SortDecls _specSortDecls))
-- semantic domain
type T_Specification = ( (Ag.Specification),(Map (SortName,AttrName) Type),(Map (SortName,AttrName) Type),Specification)
data Inh_Specification = Inh_Specification {}
data Syn_Specification = Syn_Specification {elaboration_Syn_Specification :: (Ag.Specification),sAttrInh_Syn_Specification :: (Map (SortName,AttrName) Type),sAttrSyn_Syn_Specification :: (Map (SortName,AttrName) Type),self_Syn_Specification :: Specification}
wrap_Specification :: T_Specification ->
                      Inh_Specification ->
                      Syn_Specification
wrap_Specification sem (Inh_Specification) =
    (let ( _lhsOelaboration,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself) = sem
     in  (Syn_Specification _lhsOelaboration _lhsOsAttrInh _lhsOsAttrSyn _lhsOself))
sem_Specification_Specification :: String ->
                                   T_NamespaceDecls ->
                                   T_SortDecls ->
                                   T_Specification
sem_Specification_Specification specModuleName_ specNamespaceDecls_ specSortDecls_ =
    (let _lhsOelaboration :: (Ag.Specification)
         _lhsOsAttrInh :: (Map (SortName,AttrName) Type)
         _lhsOsAttrSyn :: (Map (SortName,AttrName) Type)
         _lhsOself :: Specification
         _specSortDeclsOenvAttrInh :: (Map (SortName,AttrName) Type)
         _specSortDeclsOenvAttrSyn :: (Map (SortName,AttrName) Type)
         _specSortDeclsOnamespaces :: (Map NamespaceName MbSortName)
         _specSortDeclsOvarsorts :: (Map SortName [NamespaceName])
         _specNamespaceDeclsIelaboration :: (Ag.Synonyms)
         _specNamespaceDeclsInamespaces :: (Map NamespaceName MbSortName)
         _specNamespaceDeclsIself :: NamespaceDecls
         _specSortDeclsIelaboration :: (Ag.SortDecls)
         _specSortDeclsIsAttrInh :: (Map (SortName,AttrName) Type)
         _specSortDeclsIsAttrSyn :: (Map (SortName,AttrName) Type)
         _specSortDeclsIself :: SortDecls
         _lhsOelaboration =
             ({-# LINE 31 "src/InBound/Elaboration/Term.ag" #-}
              Ag.Specification
                specModuleName_
                _specNamespaceDeclsIelaboration
                _specSortDeclsIelaboration
              {-# LINE 3705 "src/InBound/AG.hs" #-}
              )
         _namespaces =
             ({-# LINE 32 "src/InBound/Environment.ag" #-}
              _specNamespaceDeclsInamespaces
              {-# LINE 3710 "src/InBound/AG.hs" #-}
              )
         _varsorts =
             ({-# LINE 33 "src/InBound/Environment.ag" #-}
              M.fromListWith (++)
                [ (v,[k])
                | (k,Just v) <- M.toList _namespaces
                ]
              {-# LINE 3718 "src/InBound/AG.hs" #-}
              )
         _envAttrSyn =
             ({-# LINE 118 "src/InBound/Environment.ag" #-}
              _specSortDeclsIsAttrSyn
              {-# LINE 3723 "src/InBound/AG.hs" #-}
              )
         _envAttrInh =
             ({-# LINE 119 "src/InBound/Environment.ag" #-}
              _specSortDeclsIsAttrInh
              {-# LINE 3728 "src/InBound/AG.hs" #-}
              )
         _lhsOsAttrInh =
             ({-# LINE 83 "src/InBound/Environment.ag" #-}
              _specSortDeclsIsAttrInh
              {-# LINE 3733 "src/InBound/AG.hs" #-}
              )
         _lhsOsAttrSyn =
             ({-# LINE 82 "src/InBound/Environment.ag" #-}
              _specSortDeclsIsAttrSyn
              {-# LINE 3738 "src/InBound/AG.hs" #-}
              )
         _self =
             Specification specModuleName_ _specNamespaceDeclsIself _specSortDeclsIself
         _lhsOself =
             _self
         _specSortDeclsOenvAttrInh =
             ({-# LINE 96 "src/InBound/Environment.ag" #-}
              _envAttrInh
              {-# LINE 3747 "src/InBound/AG.hs" #-}
              )
         _specSortDeclsOenvAttrSyn =
             ({-# LINE 95 "src/InBound/Environment.ag" #-}
              _envAttrSyn
              {-# LINE 3752 "src/InBound/AG.hs" #-}
              )
         _specSortDeclsOnamespaces =
             ({-# LINE 26 "src/InBound/Environment.ag" #-}
              _namespaces
              {-# LINE 3757 "src/InBound/AG.hs" #-}
              )
         _specSortDeclsOvarsorts =
             ({-# LINE 28 "src/InBound/Environment.ag" #-}
              _varsorts
              {-# LINE 3762 "src/InBound/AG.hs" #-}
              )
         ( _specNamespaceDeclsIelaboration,_specNamespaceDeclsInamespaces,_specNamespaceDeclsIself) =
             specNamespaceDecls_
         ( _specSortDeclsIelaboration,_specSortDeclsIsAttrInh,_specSortDeclsIsAttrSyn,_specSortDeclsIself) =
             specSortDecls_ _specSortDeclsOenvAttrInh _specSortDeclsOenvAttrSyn _specSortDeclsOnamespaces _specSortDeclsOvarsorts
     in  ( _lhsOelaboration,_lhsOsAttrInh,_lhsOsAttrSyn,_lhsOself))
-- Type --------------------------------------------------------
-- cata
sem_Type :: Type ->
            T_Type
sem_Type (Context _namespace) =
    (sem_Type_Context _namespace)
-- semantic domain
type T_Type = ( Type)
data Inh_Type = Inh_Type {}
data Syn_Type = Syn_Type {self_Syn_Type :: Type}
wrap_Type :: T_Type ->
             Inh_Type ->
             Syn_Type
wrap_Type sem (Inh_Type) =
    (let ( _lhsOself) = sem
     in  (Syn_Type _lhsOself))
sem_Type_Context :: NamespaceName ->
                    T_Type
sem_Type_Context namespace_ =
    (let _lhsOself :: Type
         _self =
             Context namespace_
         _lhsOself =
             _self
     in  ( _lhsOself))