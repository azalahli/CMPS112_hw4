{
{-# LANGUAGE FlexibleContexts #-}

module Language.Nano.Lexer (
  Token(..),
  scanTokens
) where

import Control.Monad.Except

}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  "#".*                         ;

  ------------------------------------------------------------------------------
  -- Syntax  [ THIS IS THE ONLY SEGMENT YOU NEED TO CHANGE ]

  in                            { \p _ -> IN     p }
  "&&"                          { \p _ -> AND    p }
  \(                            { \p _ -> LPAREN p }
  \)                            { \p _ -> RPAREN p }
  \:                            { \p _ -> COLON  p }
  \,                            { \p _ -> COMMA  p }
  "True"                        { \p _ -> TRUE   p }
  "False"                       { \p _ -> FALSE  p }


  "let"                         { \p s -> LET    p }
  "="                           { \p s -> EQB    p }
  "in"                          { \p s -> IN    p }
  "\"                           { \p s -> LAM    p }
  "->"                          { \p s -> ARROW    p }
  "if"                          { \p s -> IF    p }
  "then"                        { \p s -> THEN    p }
  "else"                        { \p s -> ELSE    p }

  "+"                           { \p s -> PLUS    p }
  "-"                           { \p s -> MINUS    p }
  "*"                           { \p s -> MUL    p }
  "<"                           { \p s -> LESS    p }
  "<="                          { \p s -> LEQ    p }
  "=="                          { \p s -> EQL    p }
  "/="                          { \p s -> NEQ    p }
  "&&"                          { \p s -> AND    p }
  "||"                          { \p s -> OR    p }
  
  "("                           { \p s -> LPAREN    p }
  ")"                           { \p s -> RPAREN    p }

  "["                           { \p s -> LBRAC    p }
  "]"                           { \p s -> RBRAC    p }
  ","                           { \p s -> COMMA    p }
  ":"                           { \p s -> COLON    p }

  $alpha [$alpha $digit]*       { \p s -> ID     p s }
  $digit+                       { \p s -> NUM    p (read s) }
  -- DO NOT CHANGE ANYTHING AFTER THIS LINE ------------------------------------
  ------------------------------------------------------------------------------
{

data Token
  = LET    AlexPosn
  | TRUE   AlexPosn
  | FALSE  AlexPosn
  | IN     AlexPosn
  | IF     AlexPosn
  | THEN   AlexPosn
  | ELSE   AlexPosn
  | AND    AlexPosn
  | OR     AlexPosn
  | LESS   AlexPosn
  | LEQ    AlexPosn
  | NEQ    AlexPosn
  | LAM    AlexPosn
  | NUM    AlexPosn Int
  | ID     AlexPosn String
  | ARROW  AlexPosn
  | EQB    AlexPosn
  | EQL    AlexPosn
  | PLUS   AlexPosn
  | MINUS  AlexPosn
  | MUL    AlexPosn
  | LPAREN AlexPosn
  | RPAREN AlexPosn
  | LBRAC  AlexPosn
  | RBRAC  AlexPosn
  | COLON  AlexPosn
  | COMMA  AlexPosn
  | EOF    AlexPosn
  deriving (Eq,Show)


getLineNum :: AlexPosn -> Int
getLineNum (AlexPn _ lineNum _) = lineNum

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn _ _ colNum) = colNum

scanTokens :: String -> Except String [Token]
scanTokens str = go (alexStartPos,'\n',[],str)
  where
    go inp@(pos,_,_,str) =
      case alexScan inp 0 of
        AlexEOF -> return []
        AlexError ((AlexPn _ line column),_,_,_) -> throwError $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
        AlexSkip  inp' _       -> go inp'
        AlexToken inp' len act -> do
          res <- go inp'
          let rest = act pos (take len str)
          return (rest : res)

}
