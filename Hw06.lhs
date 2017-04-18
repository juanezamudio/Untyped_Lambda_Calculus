> {-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

> module Hw06 where
>
> import Control.Applicative
> import Data.Char
> import Data.String
>
> import qualified Data.Map as Map
> import Data.Map (Map)
>

> type VarName = String
>
> data LExp = 
>     Var VarName
>   | LExp LExp
>   | Token LExp Token LExp
>   deriving Show
>
> data Token =
>     TCompose 
>   | TLambda 
>   | TLParen
>   | TRParen
>   | TLet
>   | TEquals
>   | TIn
>   deriving (Show, Eq)
>
> lexer :: String -> [Token]
> lexer [] = []
> lexer (w:s) | isSpace w = lexer (dropWhile isSpace s)
> lexer ('.',s) = TCompose : lexer s
> lexer ("lambda",s) = TLambda : lexer s
> lexer ('(',s) = TLParen : lexer s
> lexer (')',s) = TRParen : lexer s
> lexer ("let",s) = TLet : lexer s
> lexer ('=',s) = TEquals : lexer s
> lexer ("in",s) = TIn : lexer s
>
>
>
> newtype Parser a = Parser { parse :: String -> Maybe  }
>
> instance Show LExp where
>   show (Var x) = x
>   show ()
>
>
> interp :: LExp -> LExp
