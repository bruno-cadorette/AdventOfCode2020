{-# LANGUAGE TypeFamilies #-}

module Day4 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Control.Monad
import Data.Void
import Data.Maybe
import Data.List.Split
import Text.Read
import qualified Data.Set as Set
type Parser = Parsec Void String

data FieldName = 
    Byr
  | Iyr
  | Eyr
  | Hgt
  | Hcl
  | Ecl
  | Pid
  | Cid
  deriving (Show, Eq, Ord)

fieldName = 
    Byr <$ string "byr" <|> 
    Iyr <$ string "iyr" <|> 
    Eyr <$ string "eyr" <|> 
    Hgt <$ string "hgt" <|>   
    Hcl <$ string "hcl" <|> 
    Ecl <$ string "ecl" <|> 
    Pid <$ string "pid" <|> 
    Cid <$ string "cid" 

verifyWithParser :: Parser Bool -> String -> Bool
verifyWithParser parser = fromMaybe False . parseMaybe parser

yearVerify lowest highest value = 
  let val' = readMaybe value
  in Just lowest <= val' && val' <= Just highest

heightVerify = 
  verifyWithParser $ do
      n <- decimal
      unit <- string "cm" <|> string "in"
      case unit of
        "cm" -> return $ 150 <= n && n <= 193
        "in" -> return $ 59 <= n && n <= 76

hairColorVerify = 
  verifyWithParser $ do
    char '#'
    xs <- many alphaNumChar
    return (length xs == 6)

eyeColorVerify value = elem value ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
passportIdVerify value = 
  let pid = readMaybe value :: Maybe Int
  in length value == 9 && isJust pid 

verifyInformationValidity :: FieldName -> String -> Bool
verifyInformationValidity fieldName =
    case fieldName of
      Byr -> yearVerify 1920 2002
      Iyr -> yearVerify 2010 2020
      Eyr -> yearVerify 2020 2030
      Hgt -> heightVerify
      Hcl -> hairColorVerify
      Ecl -> eyeColorVerify
      Pid -> passportIdVerify
      Cid -> const True


field :: Parser (Maybe (FieldName, String))
field = do
  key <- fieldName
  char ':'
  value <- many (alphaNumChar <|> char '#')
  return $ Just (key, value)

fieldSeparator = Nothing <$ (char ' ' <|> newline)

removeSeparators = fmap catMaybes . (splitOn [Nothing, Nothing])

passportParsed =  removeSeparators . fromJust . parseMaybe (many (field <|> fieldSeparator))

requiredFields = Set.fromList [Byr, Iyr, Eyr, Hgt, Hcl, Ecl, Pid]

hasAllRequiredFields pp = requiredFields `Set.isSubsetOf` Set.fromList (fmap fst pp)

day4p1 = length . filter hasAllRequiredFields . passportParsed

day4p2 = length . filter (all (\(key, value) -> verifyInformationValidity key value)). filter hasAllRequiredFields . passportParsed

