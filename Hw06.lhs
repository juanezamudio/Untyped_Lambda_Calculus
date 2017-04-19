> {-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

> import Control.Applicative
> import Control.Monad.Except
> import Data.Char
> import Data.String
>
> import qualified Data.Map as Map
> import Data.Map (Map)
>

> type VarName = String
>
> newtype Parser a = Parser { parse :: String -> Maybe (a, String) }
>
> keywords :: [String]
> keywords = ["let", "in", "=", "lambda", "."]
>
> kw :: String -> Parser ()
> kw s = pure () <* spaces <* (sequenceA $ map (ensure (satisfy . (==))) s)
>
> var :: Parser VarName
> var = ensure (not . (`elem` keywords)) (spaces *> id)
>  where id = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
>
> data LC = 
>     Var VarName 
>   | App LC LC 
>   | Lam VarName LC 
>   deriving Show
>
> apply :: LC -> [LC] -> LC
> apply e1 [] = e1
> apply e1 (e2:es) = apply (App e1 e2) es
>
> apply' :: LC -> [LC] -> LC
> apply' e1 es = foldl App e1 es
>
> mkLam :: [VarName] -> LC -> LC
> mkLam xs e = foldr Lam e xs
>
> expr, atom, lam :: Parser LC
> expr =     foldl App <$> atom <*> many atom
> atom =     lam 
>        <|> Var <$> var
>        <|> char '(' *> expr <* char ')'
> lam = (\xs e -> foldr Lam e xs) <$> (kw "lambda" *> vars) <*> (char '.' *> expr)
>
> vars :: Parser [VarName]
> vars = some var
>
> instance Show LExp where
>   show (Var x) = x
>   show ()
>
>> interp :: LExp -> LExp
