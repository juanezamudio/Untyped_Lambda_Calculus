> {-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}
>
> import Control.Applicative
> import Control.Monad.Except
> import Data.Char
> import Data.String
>
> import qualified Data.Map as Map
> import Data.Map (Map)
>
> import qualified Prelude as Pre
> import Prelude hiding (pred, id)
>
> newtype Parser a = Parser { parse :: String -> Maybe (a, String) }
>
> instance Functor Parser where
>  fmap f p = Parser $ \s ->
>    case parse p s of
>      Nothing -> Nothing
>      Just (v,s') -> Just (f v,s')
>
> instance Applicative Parser where
>  pure a = Parser $ \s -> Just (a,s)
>  f <*> a = Parser $ \s ->
>    case parse f s of
>      Nothing -> Nothing
>      Just (g,s') -> parse (fmap g a) s'
>
> eof :: Parser ()
> eof = Parser $ \s -> if null s then Just ((),"") else Nothing
>
> ensure :: (a -> Bool) -> Parser a -> Parser a
> ensure pred p = Parser $ \s ->
>  case parse p s of
>    Nothing -> Nothing
>    Just (a,s') -> if pred a then Just (a,s') else Nothing
>
> satisfy :: (Char -> Bool) -> Parser Char
> satisfy p = Parser f
>  where f [] = Nothing
>        f (x:xs) = if p x then Just (x,xs) else Nothing
>
> lookahead :: Parser (Maybe Char)
> lookahead = Parser f
>  where f [] = Just (Nothing,[])
>        f (c:s) = Just (Just c,c:s)
>
> instance Alternative Parser where
>  empty = Parser $ \_ -> Nothing
>  p1 <|> p2 = Parser $ \s ->
>    case parse p1 s of
>      Just (a,s') -> Just (a,s')
>      Nothing -> parse p2 s
>
>
>
> type VarName = String
>
> keywords :: [String]
> keywords = ["let", "in", "lambda"]
>
> kw :: String -> Parser String
> kw s = spaces *> Parser f  
>  where f x = if (s == x && s `elem` keywords) then sequenceA $ map (satisfy . (==)) s else Nothing
>
> kw' :: String -> Parser ()
> kw' s = pure () <* spaces <* ensure (`elem` keywords) word
>  where word = sequenceA $ map (satisfy . (==)) s
>
> var :: Parser VarName
> var = ensure (not . (`elem` keywords)) (spaces *> id)
>  where id = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
>
> spaces :: Parser ()
> spaces = many (satisfy isSpace) *> pure ()
>
> char :: Char -> Parser Char
> char c = spaces *> satisfy (==c)
>
> vars :: Parser [VarName]
> vars = some var
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
> letIn :: VarName -> LC -> LC -> LC
> letIn x e1 e2 = App (Lam x e2) e1
>
> expr, atom, lam, lin :: Parser LC
> expr =     foldl App <$> atom <*> many atom
> atom =     lam
>        <|> lin 
>        <|> Var <$> var
>        <|> char '(' *> expr <* char ')'
> lam = (\xs e -> foldr Lam e xs) <$> (kw "lambda" *> vars) <*> (char '.' *> expr)
> lin = letIn <$> (kw "let" *> var) <*> (char '=' *> expr <* kw "in") <*> expr
>
>
>
> --instance Show LC where
> --  show (Var x) = x
> --  show ()
>
> -- interp :: LExp -> LExp