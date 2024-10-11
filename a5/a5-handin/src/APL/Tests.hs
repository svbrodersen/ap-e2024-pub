module APL.Tests (
  properties,
)
where

import APL.AST (Exp (..), VName, printExp, subExp)

-- For Task 4
import APL.Check (checkExp)
import APL.Error (isDomainError, isTypeError, isVariableError)
import APL.Eval (eval, runEval)
import APL.Parser (parseAPL)
import Control.Monad (when)
import Data.Bool (bool)
import Debug.Trace (trace)
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
  suchThat,
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
  alpha <- vectorOf half $ elements ['a' .. 'z']
  nums <- vectorOf half $ elements ['1' .. '9']
  pure $ alpha ++ nums
 where
  half = size `div` 2

genPos :: Gen Integer
genPos = (arbitrary :: Gen Integer) `suchThat` (> 0)

genExp :: Int -> [VName] -> Gen Exp
genExp 0 vs =
  frequency
    [ (1, CstInt <$> genPos)
    , (1, CstBool <$> arbitrary)
    , (1000, Var <$> chooseVar)
    ]
 where
  chooseVar :: Gen VName
  chooseVar =
    case vs of
      [] ->
        {- if no let or lambda has been called, we create one-}
        genVar 3
      (x : _) ->
        {- If there exists a variable, we use the first one -}
        pure x
genExp size vs =
  frequency
    [
      ( 1
      , Add
          <$> genExp halfSize vs
          <*> genExp halfSize vs
      )
    ,
      ( 1
      , Sub
          <$> genExp halfSize vs
          <*> genExp halfSize vs
      )
    ,
      ( 1
      , Mul
          <$> genExp halfSize vs
          <*> genExp halfSize vs
      )
    ,
      ( t `div` 30
      , Div
          <$> genExp halfSize vs
          <*> genExp
            halfSize
            vs
      )
    ,
      ( 1
      , Eql
          <$> genExp halfSize vs
          <*> genExp halfSize vs
      )
    ,
      ( 1
      , If
          <$> genExp thirdSize vs
          <*> genExp thirdSize vs
          <*> genExp thirdSize vs
      )
    ,
      ( t
      , do
          v <- genVar 3
          ( Let v
              <$> genExp halfSize vs
            )
            <*> genExp halfSize (v : vs)
      )
    ,
      ( t
      , do
          v <- genVar 3
          Lambda v <$> genExp (size - 1) (v : vs)
      )
    , (1, Apply <$> genExp halfSize vs <*> genExp halfSize vs)
    , (1, TryCatch <$> genExp halfSize vs <*> genExp halfSize vs)
    ]
 where
  halfSize = size `div` 2
  thirdSize = size `div` 3
  t = 100 :: Int

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
parsePrinted e1 =
  case parseAPL "" (printExp e1) of
    Left err ->
      trace ("String error returned\n" ++ err) False
    Right e2 ->
      if e1 /= e2
        then
          trace ("Found: " ++ printExp e2 ++ "\nExpected: " ++ printExp e1) e1
            == e2
        else
          e1 == e2

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors expr =
  let
    checkedErrors = checkExp expr
    evalResult = runEval (eval expr)
   in
    case evalResult of
      Left evalError -> evalError `elem` checkedErrors
      Right _ -> null checkedErrors

properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage)
  , ("onlyCheckedErrors", property onlyCheckedErrors)
  , ("parsePrinted", property parsePrinted)
  ]
