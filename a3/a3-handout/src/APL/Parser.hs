module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Exception.Base (patError)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Debug.Trace (traceM)
import Text.Megaparsec (
  Parsec,
  choice,
  chunk,
  eof,
  errorBundlePretty,
  many,
  notFollowedBy,
  parse,
  satisfy,
  some,
  try,
 )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "if"
  , "then"
  , "else"
  , "true"
  , "false"
  , "let"
  , "in"
  , "try"
  , "catch"
  , "print"
  , "put"
  , "get"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

lBool :: Parser Bool
lBool =
  lexeme . try . choice $
    [ const True <$> lKeyword "true"
    , const False <$> lKeyword "false"
    ]

lWord :: Parser String
lWord = lexeme $ lString "\"" *> some (satisfy isAlphaNum) <* lString "\""

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger
    , CstBool <$> lBool
    , Var <$> lVName
    , lString "(" *> pExp <* lString ")"
    ]

{-
 - pFEexp' = pFExp'
 - | empty
 -
 - pFExp = pAtom
 - | pFExp'
 - -}

pFExp :: Parser Exp
pFExp = do
  x <- pAtom
  chain x
 where
  chain x =
    choice
      [ do
          y <- pAtom
          chain $ Apply x y
      , pure x
      ]

pLExp :: Parser Exp
pLExp =
  choice
    [ If
        <$> (lKeyword "if" *> pExp)
        <*> (lKeyword "then" *> pExp)
        <*> (lKeyword "else" *> pExp)
    , do
        _ <- lKeyword "let"
        v <- lVName
        _ <- lString "="
        e1 <- pExp
        _ <- lString "in"
        Let v e1 <$> pExp
    , do
        _ <- lKeyword "try"
        e1 <- pExp
        _ <- lString "catch"
        TryCatch e1 <$> pExp
    , do
        _ <- lString "\\"
        v <- lVName
        _ <- lKeyword "->"
        Lambda v <$> pExp
    , pFExp
    ]

pPGPExp :: Parser Exp
pPGPExp =
  choice
    [ do
        _ <- lKeyword "print"
        s <- lWord
        Print s <$> pAtom
    , do
        _ <- lKeyword "get"
        KvGet <$> pAtom
    , do
        _ <- lKeyword "put"
        e1 <- pAtom
        KvPut e1 <$> pAtom
    , pLExp
    ]

{- Exp := LExp
 - | Exp "**" Exp
 -
 - Exp' := LExp
 -
 - Exp := Exp' "**" Exp
 -
 -
 - -}

pExp3' :: Parser [Exp]
pExp3' =
  do
    x <- pPGPExp
    chain [x]
 where
  chain x =
    choice
      [ do
          lString "**"
          y <- pPGPExp
          chain $ x ++ [y]
      , pure x
      ]

pExp3 :: Parser Exp
pExp3 =
  do
    (x : xs) <- pExp3'
    pure $ foldl Pow x xs

pExp2 :: Parser Exp
pExp2 = pExp3 >>= chain
 where
  chain x =
    choice
      [ do
          lString "*"
          y <- pExp3
          chain $ Mul x y
      , do
          lString "/"
          y <- pExp3
          chain $ Div x y
      , pure x
      ]

pExp1 :: Parser Exp
pExp1 = pExp2 >>= chain
 where
  chain x =
    choice
      [ do
          lString "+"
          y <- pExp2
          chain $ Add x y
      , do
          lString "-"
          y <- pExp2
          chain $ Sub x y
      , pure x
      ]

pExp0 :: Parser Exp
pExp0 =
  do
    x <- pExp1
    chain x
 where
  chain x =
    choice
      [ do
          lString "=="
          y <- pExp1
          chain $ Eql x y
      , pure x
      ]

pExp :: Parser Exp
pExp = pExp0

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
