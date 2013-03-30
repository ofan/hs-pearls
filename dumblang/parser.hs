module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Token (braces, whiteSpace)
import AST
import Lexer

spaces :: Parser ()
spaces = skipMany1 space

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

block :: Parser Stmt
block = braces statement

statement :: Parser Stmt
statement = statement' <|> sequenceOfStmt

sequenceOfStmt =
  do list <- (sepBy1 statement' semi)
     return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' = ifStmt
          <|> whileStmt
          <|> skipStmt
          <|> assignStmt

ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond <- parens bExpression
     reserved "then"
     stmt1 <- block <|> statement'
     reserved "else"
     stmt2 <- block <|> statement'
     return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- parens bExpression
     stmt <- block <|> statement'
     return $ While cond stmt

declStmt :: Parser Stmt
declStmt =
  do varType <- T.identifier
     
     varName <- T.identifier
assignStmt :: Parser Stmt
assignStmt =
  do varType <- letter >> many1 (alphaNum <|> char '_')

