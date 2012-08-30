module Main where

import qualified Options.Applicative as Op
import Data.Char (toUpper, ord, chr)
import Data.List (unfoldr, splitAt, sort, elemIndex)
import Data.Maybe (fromJust)
import Control.Applicative ((<*>))
import Control.Monad ((>=>))

data Card = RankCard Int | JokerA | JokerB deriving (Eq)

instance Show Card where
    show (RankCard r) = show r
    show JokerA = "A"
    show JokerB = "B"

type Deck = [Card]

aJokerIdx = cardIndex JokerA
bJokerIdx = cardIndex JokerB

cardValue :: Card -> Int
cardValue (RankCard val) =
    if val > 0 && val <= 52
    then val
    else error "Invalid card value"
cardValue JokerA = 53
cardValue JokerB = 53

cardIndex :: Card -> [Card] -> Int
cardIndex card = fromJust . elemIndex card

serialDeck :: Deck
serialDeck = map RankCard [1..52] ++ [JokerA, JokerB]

keyStream :: Deck -> Int -> [Char]
keyStream deck keyCount =
    take keyCount $ unfoldr (Just . keyChar) deck

keyChar :: Deck -> (Char, Deck)
keyChar deck =
    let
        -- move down JokerA by 1
        deck' = moveCard deck (aJokerIdx deck) 1
        -- move donw JokerB by 2
        deck'' = moveCard deck' (bJokerIdx deck') 2

        -- triple cut around the jokers
        [i, j] = sort [aJokerIdx deck'', bJokerIdx deck'']
        (top, rest) = splitAt i deck''
        (mid, bottom) = splitAt (j + 1 - i) rest
        cards' = bottom ++ mid ++ top

        -- count cut using the value of the bottom card
        c = cardValue (last cards')
        (top', bottom') = splitAt c cards'
        cards'' = init bottom' ++ top' ++ [last cards']

        -- output value
        cV = cardValue (cards'' !! (cardValue . head $ cards''))
    in
        if cV == 53
            then keyChar cards''
            else (numToChar (if cV > 26 then cV - 26 else cV), cards'')

moveCard :: [a] -> Int -> Int -> [a]
moveCard lst idx move =
    before ++ [lst !! idx] ++ after
    where
        (left, right) = let (l, r) = splitAt idx lst in (l, tail r)
        (before, after) = splitAt (wrappedIdx (idx + move) (length lst)) (left ++ right)

wrappedIdx :: Int -> Int -> Int
wrappedIdx i len
    | i < 0 = wrappedIdx (i + len) len
    | i < len = i
    | otherwise = (i + i `div` len) `mod` len

cleanupText :: String -> String
cleanupText text =
    let
        t = map toUpper text
        l = length t
        (q, r) = l `divMod` 5
    in
        if r == 0
        then t
        else take ((q + 1) * 5) $ t ++ repeat 'X'

charsToNums :: [Char] -> [Int]
charsToNums = map (\c -> ord c - 64)

numToChar :: Int -> Char
numToChar = chr . (+ 64)

numsToChars :: [Int] -> [Char]
numsToChars = map numToChar

addCharNums :: (Int, Int) -> Int
addCharNums (a, b) =
    let s = a + b in
    if s <= 26 then s else s - 26

subCharNums :: (Int, Int) -> Int
subCharNums (a, b) =
    let m = a - b in
    if m > 0 then m else m + 26

encrypt :: Deck -> String -> String
encrypt deck clearText =
    numsToChars . map addCharNums $ zip textNums ksNums
    where
        text = cleanupText clearText
        ks = keyStream deck (length text)
        textNums = charsToNums text
        ksNums = charsToNums ks

decrypt :: Deck -> String -> String
decrypt deck encText =
    numsToChars . map subCharNums $ zip encTextNums ksNums
    where
        ks = keyStream deck (length encText)
        encTextNums = charsToNums encText
        ksNums = charsToNums ks

data Command = Encrypt String | Decrypt String

optParser = Op.subparser $
    Op.command "encrypt"
        (Op.info
            (Op.helper <*>
                (Op.argument (Op.str >=> Just . Encrypt) (Op.metavar "CLEARTEXT")))
            (Op.progDesc "Encrypts a cleartext"))
    Op.& Op.command "decrypt"
        (Op.info
            (Op.helper <*>
                (Op.argument (Op.str >=> Just . Decrypt) (Op.metavar "CRYPTTEXT")))
            (Op.progDesc "Decrypts a crypttext"))

opts = Op.info (Op.helper <*> optParser) $
    Op.progDesc "Encrypt or decrypts a string using the solataire cipher algorithm"

main = do
    command <- Op.execParser opts
    case command of
        (Encrypt clearText) -> putStrLn $ encrypt serialDeck clearText
        (Decrypt cryptText) -> putStrLn $ decrypt serialDeck cryptText