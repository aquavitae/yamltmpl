module ParseWhile where

import           Control.Monad
import           System.IO
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

-- expression: [ scopeWord ] textExpr [ "|" filterExpr ]
-- scopeWord:  "lookup"
-- textExpr: textLiteral | varExpression ( textOp textExpr )


data BExpr = BoolConst Bool
          | Not BExpr
          | BBinary BBinOp BExpr BExpr
          | RBinary RBinOp AExpr AExpr
           deriving (Show)

data BBinOp = And | Or deriving (Show)

data RBinOp = Greater | Less deriving (Show)

data AExpr = Var String
          | IntConst Integer
          | Neg AExpr
          | ABinary ABinOp AExpr AExpr
            deriving (Show)

data ABinOp = Add
           | Subtract
           | Multiply
           | Divide
             deriving (Show)

data Expression =
blockStart      = "(("
blockEnd        = "))"

languageDef =
 emptyDef { Token.identStart      = letter
          , Token.identLetter     = alphaNum
          , Token.reservedNames   = [ "if"
                                    , "then"
                                    , "else"
                                    , "true"
                                    , "false"
                                    , "not"
                                    , "and"
                                    , "or"
                                    ]
          , Token.reservedOpNames = ["+", "-", "*", "/", "="
                                    , "<", ">", "and", "or", "not"
                                    ]
          }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

parser :: Parser Expression
parser = whiteSpace >> statement

statement :: Parser Stmt
statement =   parens statement
         <|> sequenceOfStmt

sequenceOfStmt = do
  list <- sepBy1 statement' semi
  -- If there's only one statement return it without using Seq.
  return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' =   ifStmt
          <|> assignStmt

ifStmt :: Parser Stmt
ifStmt =
 do reserved "if"
    cond  <- bExpression
    reserved "then"
    stmt1 <- statement
    reserved "else"
    stmt2 <- statement
    return $ If cond stmt1 stmt2

assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     reservedOp "="
     expr <- aExpression
     return $ Assign var expr

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
             , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft,
                Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
              ]

bOperators = [ [Prefix (reservedOp "not" >> return (Not             ))          ]
            , [Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft,
               Infix  (reservedOp "or"  >> return (BBinary Or      )) AssocLeft]
            ]

aTerm =  parens aExpression
    <|> liftM Var identifier
    <|> liftM IntConst integer

bTerm =  parens bExpression
    <|> (reserved "true"  >> return (BoolConst True ))
    <|> (reserved "false" >> return (BoolConst False))
    <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RBinary op a1 a2

relation =   (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)
