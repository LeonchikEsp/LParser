module ParseWhile where
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data BooleanExpressions = BoolConst Bool
           | Not BooleanExpressions
           | BBinary BoolBinOps BooleanExpressions BooleanExpressions
           | RBinary RelBinOp ArithExpr ArithExpr
           deriving (Show)

data BoolBinOps = And | Or deriving (Show)
data RelBinOp = Greater | Less deriving (Show)
data ArithExpr = Var String
           | IntConst Integer
           | Neg ArithExpr
           | ABinary ArithBinOps ArithExpr ArithExpr
           deriving (Show)

data ArithBinOps = Add
            | Subtract
            | Multiply
            | Divide
            deriving (Show)

data Statement = Seq [Statement]
          | Assign String ArithExpr
          | If BooleanExpressions Statement Statement
          | While BooleanExpressions Statement
          | Skip
          deriving (Show)

languageDef =
  emptyDef { Token.commentStart    = "(*"
           , Token.commentEnd      = "*)"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "fi"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "od"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     ]
           , Token.reservedOpNames = ["==", "!=", ">=", "<=", "&&", "||",
                                      "+", "-", "*", "/", ":=",
                                     "<", ">", "and", "or", "not", "%"
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

whileParser :: Parser Statement
whileParser = whiteSpace >> statement

statement :: Parser Statement
statement =   parens statement
          <|> sequenceOfStmt
 
sequenceOfStmt =
  do list <- (sepBy1 statement' semi)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else Seq list

statement' :: Parser Statement
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt

ifStmt :: Parser Statement
ifStmt =
  do reserved "if"
     cond  <- boolExpr
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     reserved "fi"
     return $ If cond stmt1 stmt2
 
whileStmt :: Parser Statement
whileStmt =
  do reserved "while"
     cond <- boolExpr
     reserved "do"
     stmt <- statement
     reserved "od"
     return $ While cond stmt 
 
assignStmt :: Parser Statement
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr


skipStmt :: Parser Statement
skipStmt = reserved "skip" >> return Skip         

aExpression :: Parser ArithExpr
aExpression = buildExpressionParser aOperators aTerm
 
boolExpr :: Parser BooleanExpressions
boolExpr = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
             , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft,
                Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
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

bTerm =  parens boolExpr
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

parseString :: String -> Statement
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Statement
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r