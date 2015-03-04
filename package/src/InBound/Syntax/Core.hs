
module InBound.Syntax.Core where

import GHC.Read

newtype NamespaceName = NN { fromNN :: String }
  deriving (Eq,Ord)
instance Read NamespaceName where
  readPrec = fmap NN readPrec
instance Show NamespaceName where
  showsPrec i (NN s) = showsPrec i s

newtype SortName = SN { fromSN :: String }
  deriving (Eq,Ord)
instance Read SortName where
  readPrec = fmap SN readPrec
instance Show SortName where
  showsPrec i (SN s) = showsPrec i s

newtype AttrName = AN { fromAN :: String }
  deriving (Eq,Ord)
instance Read AttrName where
  readPrec = fmap AN readPrec
instance Show AttrName where
  showsPrec i (AN s) = showsPrec i s

newtype CtorName = CN { fromCN :: String }
  deriving (Eq,Ord)
instance Read CtorName where
  readPrec = fmap CN readPrec
instance Show CtorName where
  showsPrec i (CN s) = showsPrec i s

newtype FieldName = FN { fromFN :: String }
  deriving (Eq,Ord)
instance Read FieldName where
  readPrec = fmap FN readPrec
instance Show FieldName where
  showsPrec i (FN s) = showsPrec i s
