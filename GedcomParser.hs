-- A GEDCOM to XML converter written using Parsec as a
-- solution for rubyquiz 6 (http://rubyquiz.com/quiz6.html).
-- Example GEDCOM document at
-- http://cpansearch.perl.org/src/PJCJ/Gedcom-1.16/royal.ged

{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards #-}

module GedcomParser where

import Text.Parsec hiding (spaces, Line)
import System.IO

-- a line in a GEDCOM document
data Line = Line {
				lineLevel :: Int,
				lineTag :: String,
				lineValue :: Maybe String,
				lineId :: Maybe String
			} deriving (Show)

-- an element in a GEDCOM document
data Elem = Elem {
				elemTag :: String,
				elemValue :: Maybe String,
				elemId :: Maybe String,
				elemChildren :: [Elem]
			} deriving (Show)

indent n = concat . (replicate n) $ "  "

trimValue value = case value of
	Nothing -> Nothing
	Just v
		| v == "" -> Nothing
		| otherwise -> Just v

normalizeValue = maybe "" id

spaces = many (char ' ' <|> tab)
whitespaces = many (char ' ' <|> tab <|> newline)

-- parses a line
line level = do
	string (show level)
	spaces
	id <- optionMaybe $ between (char '@') (char '@') (many1 alphaNum)
	spaces
	tag <- many1 upper
	spaces
	value <- fmap trimValue $ optionMaybe $ manyTill (anyChar) newline
	return $ Line level tag value id

-- parses an element
element level = do
	ml <- optionMaybe $ line level
	case ml of
		Nothing -> fail ("invalid level " ++ show level)
		Just Line{..} -> do
			children <- many (element $ level + 1)
			return $ Elem lineTag lineValue lineId children

-- parses a document
document = (element 0) `endBy` whitespaces

-- normalizes an element by merging values of CONC and CONT
-- elements with parent element value
normalizeElem element =
	let
		conChildren = filter concOrCont $ elemChildren element
		text = foldl (\t el -> t
						++ (if elemTag el == "CONC" then "\n" else " ")
						++ normalizeValue (elemValue el))
		 			"" conChildren
		nonConChildren = filter (not . concOrCont) $ elemChildren element
	in
		element { elemValue = trimValue $
						Just (normalizeValue (elemValue element) ++ text),
			   	  elemChildren = map normalizeElem nonConChildren }
	where
		concOrCont el = elemTag el `elem` ["CONC", "CONT"]

-- normalizes a document
normalizeDoc = map normalizeElem

-- converts an element to XML
elemToXml indentation Elem{..} =
	indent indentation
	++ "<" ++ elemTag
	++ maybe "" (\i -> " id=\"@" ++ i ++ "@\"") elemId
	++ case elemChildren of
		[] -> ">" ++ normalizeValue elemValue ++ "</" ++ elemTag ++ ">"
		_ -> maybe "" (\v -> " value=\"" ++ v ++ "\"") elemValue ++ ">\n"
			++ unlines (map (elemToXml (indentation + 1)) elemChildren)
			++ indent indentation ++ "</" ++ elemTag ++ ">"

-- converts a document to XML
documentToXml doc = "<DOCUMENT>\n"
	++ (unlines . map (elemToXml 1) $ doc)
	++ "</DOCUMENT>"

-- converts a GEDCOM document supplied through STDIN into XML
-- and prints to STDOUT
main = do
	text <- getContents
	case parse document "GEDCOM Parser" text of
		Right []  -> return ()
		Right doc -> putStrLn $ documentToXml (normalizeDoc doc)
		Left e    -> print e