{-# LANGUAGE NoMonomorphismRestriction #-}

module NaturalParser
    where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Char
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

parseStr = parse expr ""

expr = buildExpressionParser table term

-- Define basic rules of our language
mathExprDef = emptyDef {
        P.commentStart = "",
        P.commentEnd = "",
        P.identStart = letter <|> digit,
        P.identLetter = letter <|> digit <|> char '_',
        P.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
        P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
        P.reservedOpNames = ["+", "-", "*", "/", "sin", "cos", "pow"]
    }

-- Generate the lexer
lexer = P.makeTokenParser mathExprDef

-- Bind the lexer to the parsers from Parsec.Token.
parens = P.parens lexer
float = P.float lexer
natural = P.natural lexer
symbol = P.symbol lexer
reservedOp = P.reservedOp lexer
term = parens expr
    <|> try float
    <|> try (natural >>= \h -> return (fromInteger h))
    <?> "term (simple expr)"

table = [
          [prefix "-" negate, prefix "+" id]
        , [prefix "sin" sin, prefix "cos" cos]
        , [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft]
        , [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft]
        ]

binary s f = Infix ( reservedOp s >> return f <?> "binary operator")
prefix s f = Prefix ( reservedOp s >> return f <?> "unary prefix operator")
