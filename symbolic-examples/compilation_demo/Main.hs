{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import           Prelude                                    hiding (Fractional (..), Num (..), length)

import           ZkFold.Algebra.EllipticCurve.BLS12_381     (Fr)
import qualified ZkFold.Symbolic.Compiler                   as C
import qualified ZkFold.Symbolic.Compiler.ArithmeticCircuit as AC
import           ZkFold.Symbolic.Examples.SmartWallet       (expModContract)

main :: IO ()
main = do
    -- expModContract is a Symbolic function defined in ZkFold.Symbolic.Examples.SmartWallet
    -- We use `compile` to transform it into an ArithmeticCircuit
    -- It can be done with any Symbolic function
    let expModCircuit = C.compile @Fr expModContract

    -- We print the number of polynomial constraints in this circuit
    print $ AC.acSizeN expModCircuit

    -- And the number of lookup constraints
    print $ AC.acSizeL expModCircuit

