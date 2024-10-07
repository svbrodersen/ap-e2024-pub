module APL.Tests where

import APL.AST (Exp (..), VName)
import Test.QuickCheck (Gen, elements, listOf, oneof, sample, sized)

genVar :: Gen VName
genVar = do
  alpha <- elements ['a' .. 'z']
  alphaNums <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
  pure (alpha : alphaNums)

genExp :: Int -> Gen Exp
genExp n =
  if n < 1
    then
      Var <$> genVar
    else
      let half = (n - 1) `div` 2
       in oneof
            [ Lambda <$> genVar <*> genExp (n - 1)
            , Var <$> genVar
            , Apply <$> genExp half <*> genExp half
            ]
