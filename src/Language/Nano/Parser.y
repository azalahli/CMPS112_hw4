{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Nano.Parser (
    parseExpr
  , parseTokens
  ) where

import Language.Nano.Lexer
import Language.Nano.Types hiding (Nano (..))
import Control.Monad.Except
import Control.Exception

}

-- Entry point
%name top

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { LET _    }
    true  { TRUE _   }
    false { FALSE _  }
    in    { IN _     }
    if    { IF _     }
    then  { THEN _   }
    else  { ELSE _   }
    TNUM  { NUM _ $$ }
    ID    { ID _ $$  }
    '\\'  { LAM _    }
    '->'  { ARROW _  }
    '='   { EQB _    }
    '+'   { PLUS _   }
    '-'   { MINUS _  }
    '*'   { MUL _    }
    '&&'  { AND _    }
    '||'  { OR  _    }
    '=='  { EQL _    }
    '/='  { NEQ _    }
    '<'   { LESS _   }
    '<='  { LEQ _    }
    ':'   { COLON _  }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }
    '['   { LBRAC _  }
    ']'   { RBRAC _  }
    ','   { COMMA _  }


-- Operators
{-}
%right in
%nonassoc '=' '==' '/=' '<' '<=' if then else
%right ':' '->'
%left '||' '&&'
%left '+' '-'
%left '*'
%%
-}
%right LOWR
%left LOWL
%nonassoc if then else
%right in '->'
%left '||'
%left '&&'
%left '==' '/=' '<' '<='
%right ':' ','
%right ']'
%left '+' '-'
%left '*'
%left APP
%left FSTL
%right TESTR

%%
Top  : ID '=' Expr                 { $3 }
     | Expr                        { $1 }

Expr : 
      if Expr then Expr else Expr       { EIf $2 $4 $6 }
    | Expr '||' Expr                    { EBin Or $1 $3}
    | Expr '&&' Expr                    { EBin And $1 $3}
    | Expr '<=' Expr                    { EBin Le $1 $3}
    | Expr '<' Expr                     { EBin Lt $1 $3}
    | Expr '/=' Expr                    { EBin Ne $1 $3}
    | Expr '==' Expr                    { EBin Eq $1 $3}
    | Expr ':' Expr                     { EBin Cons $1 $3}
    | Expr '-' Expr                     { EBin Minus $1 $3}
    | Expr '+' Expr                     { EBin Plus $1 $3}
    | Expr '*' Expr                     { EBin Mul $1 $3}
    | '\\'ID '->'  Expr                 { ELam $2 $4}
    | let ID '=' Expr in Expr           { ELet $2 $4 $6 }
    | let ID ids '=' Expr in Expr       { ELet $2 (mkLam $3 $5) $7 }

    | func Expr2 %prec APP               { EApp $1 $2 }
    | Expr2 Expr2 %prec APP               { EApp $1 $2}

    | TNUM                              { EInt $1 }
    | ID                                { EVar $1 }
    | true                              { EBool True}
    | false                             { EBool False}
    | '(' Expr ')'                      { $2 }
    | '[' lst ']'                       { $2 }
    | '[' ']'                           { ENil }

func: func Expr2 {EApp $1 $2}
 | Expr {$1}

Expr2: TNUM                             { EInt $1 }
    | ID                                { EVar $1 }
    | true                              { EBool True}
    | false                             { EBool False}
    | '(' Expr ')'                      { $2 }
    | '[' lst ']'                       { $2 }
    | '[' ']'                           { ENil }

body: Expr body {EApp $1 $2}


lst: Expr ',' lst    { EBin Cons $1 $3}
    | Expr            {EBin Cons $1 ENil}

    --| Expr Expr %prec APP          { EApp $1 $2}
    --|  Expr ',' Expr               { EBin Cons $1 $3}
    --| ']'                          { ENil }
    --| '[' Expr ']'                 { $2 }
    --| Expr Expr %prec FPR               { EApp $1 $2 }
    --| Expr remainder %prec FPR          { EApp $1 $2 }
    --| fbody                             {$1}
    --| remainder Expr %prec FPR          { EApp $1 $2}
    --| Expr Expr %prec FPR               { EApp $1 $2 }
    --| Expr ']' %prec LOWL              { EBin Cons $1 ENil}
--remainder : 
--        Expr                            {$1}
--      | remainder Expr %prec FPR        {EApp $1 $2}
--fbody: Expr Expr {EApp $1 $2}
--| fbody Expr %prec FPR {EApp $1 $2}
--fbody:  fbody Expr {EApp $2 $1}
--|Expr Expr{EApp $1 $2}



--list : list ',' list { EBin Cons $1 $3} 
--    |  list ']'      { EBin Cons $1 ENil}
--list1 : list1 ',' list1 { $1:$3}

ids : ID                            {[$1]}
    | ID ids                        { $1:$2 }

{
mkLam :: [Id] -> Expr -> Expr
mkLam []     e = e
mkLam (x:xs) e = ELam x (mkLam xs e)

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpr :: String -> Expr
parseExpr s = case parseExpr' s of
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e

parseExpr' input = runExcept $ do
   tokenStream <- scanTokens input
   top tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens


}
