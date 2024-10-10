module APL.Tests (
  properties,
)
where

import APL.AST (Exp (..), VName, subExp)
import APL.Check (checkExp)
import APL.Error (isDomainError, isTypeError, isVariableError)
import Control.Monad (when)
import Data.Bool (bool)
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Gen,
  Property,
  checkCoverage,
  cover,
  elements,
  frequency,
  oneof,
  property,
  quickCheck,
  sized,
  vectorOf,
  withMaxSuccess,
 )

instance Arbitrary Exp where
  arbitrary = sized (`genExp` [])

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

genVar :: Int -> Gen VName
genVar size = do
  alpha <- vectorOf size $ elements ['a' .. 'z']
  pure alpha

genExp :: Int -> [VName] -> Gen Exp
genExp 0 _ = oneof [CstInt <$> arbitrary, CstBool <$> arbitrary]
genExp size vs =
  frequency
    [ (t `div` 20, CstInt <$> arbitrary)
    , (t `div` 20, CstBool <$> arbitrary)
    ,
      ( 1
      , Add
          <$> genExp halfSize vs
          <*> frequency
            [ (4, genExp halfSize vs)
            ,
              ( 1
              , CstBool <$> arbitrary
              )
            ]
      )
    ,
      ( 1
      , Sub
          <$> genExp halfSize vs
          <*> frequency
            [ (4, genExp halfSize vs)
            ,
              ( 1
              , CstBool <$> arbitrary
              )
            ]
      )
    ,
      ( 1
      , Mul
          <$> genExp halfSize vs
          <*> frequency
            [ (4, genExp halfSize vs)
            ,
              ( 1
              , CstBool <$> arbitrary
              )
            ]
      )
    , (t `div` 30, Div <$> genExp halfSize vs <*> frequency [(1, genExp halfSize vs), (2, pure $ CstInt 0)])
    , (t `div` 30, Pow <$> genExp halfSize vs <*> frequency [(1, genExp halfSize vs), (2, pure $ Sub (CstInt 0) (CstInt 1))])
    , (1, Eql <$> genExp halfSize vs <*> frequency [(4, genExp halfSize vs), (1, CstInt <$> arbitrary)])
    , (1, If <$> frequency [(3, genExp thirdSize vs), (1, CstInt <$> arbitrary)] <*> genExp thirdSize vs <*> genExp thirdSize vs)
    ,
      ( t `div` 25
      , Var <$> chooseVar
      )
    ,
      ( t * 4
      , do
          v <- genVar 3
          (Let v <$> genExp halfSize (v : vs)) <*> genExp halfSize (v : vs)
      )
    ,
      ( t `div` 10
      , Lambda <$> genVar 3 <*> genExp (size - 1) vs
      )
    , (1, Apply <$> genExp halfSize vs <*> genExp halfSize vs)
    , (1, TryCatch <$> genExp halfSize vs <*> genExp halfSize vs)
    ]
 where
  halfSize = size `div` 2
  thirdSize = size `div` 3
  t = 100 :: Int
  chooseVar :: Gen VName
  chooseVar =
    case vs of
      [] ->
        {- if no let or lambda has been called, we create one-}
        pure "No variable"
      (x : _) ->
        {- If there exists a variable, we use the first one -}
        pure x

expCoverage :: Exp -> Property
expCoverage e =
  checkCoverage
    . cover 20 (any isDomainError (checkExp e)) "domain error"
    . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
    . cover 20 (any isTypeError (checkExp e)) "type error"
    . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
    . cover 5 (any isVariableError (checkExp e)) "variable error"
    . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
    . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
    $ ()

parsePrinted :: Exp -> Bool
parsePrinted _ = undefined

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors _ = undefined

properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage)
  , ("onlyCheckedErrors", property onlyCheckedErrors)
  , ("parsePrinted", property parsePrinted)
  ]
