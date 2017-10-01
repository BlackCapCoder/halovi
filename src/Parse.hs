{-# LANGUAGE TypeFamilies, LambdaCase #-}
module Parse where

import Halovi
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe

type Parser = Parsec Void String

enter :: Parser String
enter = choice
  [ eol
  , string "⏎"
  , eof >> return ""
  , lookAhead (many (char ' ') >> string "~#")
  ]

enter' = enter <|> lookAhead (string ">")

terminatedString
  = manyTill (shortReg <|> fmap Chr anyChar) enter

termString p = choice
  [ pure <$> shortReg >>= (<$> p) . flip id
  , p <*> terminatedString
  ]

exCmd str = exCmds [str]
exCmds :: [String] -> Parser String
exCmds strs = do
  char ':'
  choice $ map string strs
  enter'

normReg = do
  char '"'
  name <- anyChar
  return $ Reg name
shortReg = do
  name <- oneOf "⁰¹²³⁴⁵⁶⁷⁸⁹"
  let Just name' = lookup name $ zip "⁰¹²³⁴⁵⁶⁷⁸⁹" "0123456789"
  return $ Reg name'
reg = normReg <|> shortReg

optional' o f = fromMaybe f <$> optional o

withReg d f p = do
  r <- optional' reg $ Reg d
  p
  return $ f r

open     = termString $ char 'o'  >> return Open
input    = termString $ char 'i'  >> return Input
search   = termString $ char '/'  >> return Search
query    = termString $ char '\\' >> return Query
find     = termString $ char 'f' >> return (\x -> Group [Search x, Click])
find'    = termString $ char 'F' >> return (\x -> Group [Query x, Click])
quit     = exCmds (words "quit! quit q! q") >> return Quit
quitA    = exCmds (words "quitall! quitall qall! qall qa! qa") >> return QuitAll
next     = char 'n' >> return Next
prev     = char 'N' >> return Prev
star     = char '*' >> return NextOfType
nextPage = string "]]" >> return NextPage
prevPage = string "[[" >> return PrevPage
yankText = withReg '"' YankText $ string "Y"
yankURL  = withReg '"' YankURL  $ string "yy"
appText  = withReg '"' AppendText $ string "A"
goUp     = string "gu" >> return GoUp
goRoot   = string "gU" >> return GoRoot
goTop    = string "gg" >> return GoTop
goBottom = string "G" >> return GoBottom

yankAttribute = do
  r <- optional' reg $ Reg '"'
  string "yA"
  termString . return $ YankAttribute r

edit = do
  r <- optional' reg $ Reg '"'
  string "e"
  Edit r <$> terminatedString

yankEdit = do
  r <- optional' reg $ Reg '"'
  string "ye"
  YankEdit r <$> terminatedString

yankEditURL = do
  string "ge"
  YankEditURL <$> terminatedString

paste = do
  r <- optional' (pure<$>reg) [Reg '0']
  char 'p'
  return $ Open r

loop = do
  char '<'
  fmap (Loop . rmNops) . manyTill stmt $ (eof >> return '>') <|> char '>'

rep = do
  num <- some digitChar
  op  <- stmt
  return $ Repeat (read num) op

nop = choice
  [ anyChar >> return NOP
  ]

comment = do
  string "~#"
  manyTill anyChar $ eol <|> (eof >> return " ")
  return NOP


stmt = choice $ map try
  [ comment

  , open, input, quitA, quit, search, query, loop, paste
  , next, prev, find, find', nextPage, prevPage
  , yankText, yankURL, yankAttribute, appText
  , star, rep, goUp, goRoot, goTop, goBottom
  ,  yankEdit, yankEditURL, edit

  , nop
  ]

rmNops = filter (\case NOP -> False; _ -> True)

parse c
  | r <- rmNops <$> Text.Megaparsec.parse (some stmt) "" c
  = case r of
      Left e -> error $ parseErrorPretty' c e
      Right x -> x
