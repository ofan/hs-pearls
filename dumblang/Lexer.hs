module Lexer where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language
import Control.Monad (void)

nondigit = letter <|> char '_'

langDef :: LanguageDef String
langDef =
  emptyDef { T.commentStart = "/*"
           , T.commentEnd   = "*/"
           , T.identStart   = nondigit
           , T.identLetter  = (nondigit <|> digit)
           , T.reservedNames = [ "if"
                               , "then"
                               , "else"
                               , "while"
                               , "do"
                               , "true"
                               , "false"
                               , "not"
                               , "and"
                               , "or"
                               ]
           , T.reservedOpNames = [ "+", "-", "*", "/", "=", "<", ">"
                                 , "<=", ">=", "and", "or", "not"
                                 ]
           }

lexer = T.makeTokenParser langDef

identifier = T.identifier  lexer
reserved   = T.reserved    lexer
reservedOp = T.reservedOp  lexer
parens     = T.parens      lexer
integer    = T.integer     lexer
semi       = T.semi        lexer
whitespace = T.whiteSpace  lexer
