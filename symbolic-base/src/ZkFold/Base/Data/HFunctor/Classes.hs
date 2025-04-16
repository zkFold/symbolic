module ZkFold.Base.Data.HFunctor.Classes where

class (forall f. Eq1 f => Eq (c f)) => HEq c where

class (forall f. Show1 f => Show (c f)) => HShow c where

class (forall f. NFData1 f => NFData (c f)) => HNFData c where
