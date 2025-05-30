module Tests.Algebra.Univariate (specUnivariate) where

import           Test.Hspec                       (Spec)
import           Tests.Algebra.Univariate.Poly    (specUnivariatePoly)
import           Tests.Algebra.Univariate.PolyVec (specUnivariatePolyVec)

specUnivariate :: Spec
specUnivariate = do
    specUnivariatePoly
    specUnivariatePolyVec
