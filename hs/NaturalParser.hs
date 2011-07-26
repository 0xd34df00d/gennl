{-# LANGUAGE NoMonomorphismRestriction #-}

module NaturalParser
    (
        parseStr
    )
    where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import ExprTree
import SupportUtils

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
        P.reservedOpNames = ["+", "-", "*", "/", "sin", "cos", "^"]
    }

-- Generate the lexer
lexer = P.makeTokenParser mathExprDef

-- Bind the lexer to the parsers from Parsec.Token.
parens = P.parens lexer
float = P.float lexer
natural = P.natural lexer
reservedOp = P.reservedOp lexer
identifier = P.identifier lexer

term = parens expr
    <|> try (float >>= \f -> return (realLeaf f))
    <|> try (natural >>= \h -> return (intLeaf h))
    <|> try (identifier >>= \s -> return (varLeaf s))
    <?> "term (simple expr)"

table = [
            [prefix' "-" negate', prefix' "+" id],
            [prefix "sin", prefix "cos"],
            [binary "^" AssocRight],
            [binary "*" AssocLeft, binary "/" AssocLeft],
            [binary "+" AssocLeft, binary "-" AssocLeft]
        ]
    where negate' = stdF (\_ y -> y) binaryNode "*" (intLeaf (-1))

binary' s f = Infix ( reservedOp s >> return f <?> "binary operator")
prefix' s f = Prefix ( reservedOp s >> return f <?> "unary prefix operator")

stdF ar g s = case g s of
        Just x -> ar s x
        Nothing -> failStr $ "Got nothing for " ++ show s

prefix = stdF prefix' unaryNode
binary = stdF binary' binaryNode
