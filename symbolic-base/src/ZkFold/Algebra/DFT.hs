module ZkFold.Algebra.DFT (genericDft) where

import           Control.Monad        (forM_)
import qualified Data.STRef           as ST
import qualified Data.Vector          as V
import qualified Data.Vector.Mutable  as VM
import           Prelude              hiding (mod, sum, (*), (+), (-), (/), (^))
import qualified Prelude              as P

import           ZkFold.Algebra.Class

-- | Generif FFT algorithm. Can be both direct and inverse depending on @wn@ (root of unity or its inverse) supplied.
-- Does not apply scaling when it's inverse.
-- Requires the vector to be of length 2^@n@.
--
genericDft
    :: forall a
     . Ring a
    => Integer
    -> a
    -> V.Vector a
    -> V.Vector a
genericDft 0 _ v  = v
genericDft n wn v = V.create $ do
    result <- VM.new (2 P.^ n)
    wRef <- ST.newSTRef one
    forM_ [0 .. halfLen P.- 1] $ \k -> do
        w <- ST.readSTRef wRef
        VM.write result k               $ a0Hat `V.unsafeIndex` k + w * a1Hat `V.unsafeIndex` k
        VM.write result (k P.+ halfLen) $ a0Hat `V.unsafeIndex` k - w * a1Hat `V.unsafeIndex` k
        ST.modifySTRef wRef (*wn)
    pure result
  where
    a0 = V.ifilter (\i _ -> i `P.mod` 2 == 0) v
    a1 = V.ifilter (\i _ -> i `P.mod` 2 == 1) v

    wn2 = wn * wn

    a0Hat = genericDft (n P.- 1) wn2 a0
    a1Hat = genericDft (n P.- 1) wn2 a1

    halfLen = 2 P.^ (n P.- 1)

