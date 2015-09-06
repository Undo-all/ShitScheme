module Parse where

import Scheme
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

parseExprs :: Parser [Expr]
parseExprs = spaces *> many (parseExpr <* spaces)

parseExpr :: Parser Expr
parseExpr = parseList
        <|> parseAtom
        <?> "expression"

parseList :: Parser Expr
parseList = List <$> between (char '(' <* spaces) (char ')') parseExprs

parseAtom :: Parser Expr
parseAtom = Atom <$> parseValue

parseValue :: Parser Value
parseValue = parseQuoted
         <|> parseNumber
         <|> parseSymbol
         <?> "value"

parseQuoted :: Parser Value
parseQuoted = SExp <$> (char '\'' *> parseExpr)

parseNumber :: Parser Value
parseNumber = Number <$> floating2 True

parseSymbol :: Parser Value
parseSymbol = Symbol <$> many1 (noneOf "()' \t\r\n")

