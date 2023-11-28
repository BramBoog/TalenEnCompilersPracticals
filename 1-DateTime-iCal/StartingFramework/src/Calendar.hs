{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Calendar where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime

import Data.List (sort, find, splitAt)
import Data.Maybe (maybe)


-- Exercise 6
type Text = String

data Calendar = Calendar { prodid :: Text
                         , events :: [Event] }
    deriving (Eq, Ord, Show)

data Event = Event { dtstamp     :: DateTime
                   , uid         :: Text
                   , dtstart     :: DateTime
                   , dtend       :: DateTime
                   , description :: Maybe Text
                   , summary     :: Maybe Text
                   , location    :: Maybe Text }
    deriving (Eq, Ord, Show)

-- Exercise 7
data Token = BeginCalendar
           | EndCalendar
           | ProdID Text
           | Version
           | BeginEvent
           | DTStamp DateTime
           | UID Text
           | DTStart DateTime
           | DTEnd DateTime
           | Description Text
           | Summary Text
           | Location Text
           | EndEvent
    deriving (Eq, Ord, Show)

parseToken :: Parser Char Token
parseToken = parseLiteral      BeginCalendar "BEGIN:VCALENDAR"
         <|> parseLiteral      EndCalendar "END:VCALENDAR"
         <|> parseWithTextBody ProdID "PRODID:"
         <|> parseLiteral      Version "VERSION:2.0"
         <|> parseLiteral      BeginEvent "BEGIN:VEVENT"
         <|> parseLiteral      EndEvent "END:VEVENT"
         <|> parseWithDTBody   DTStamp "DTSTAMP:"
         <|> parseWithTextBody UID "UID:"
         <|> parseWithDTBody   DTStart "DTSTART:"
         <|> parseWithDTBody   DTEnd "DTEND:"
         <|> parseWithTextBody Description "DESCRIPTION:"
         <|> parseWithTextBody Summary "SUMMARY:"
         <|> parseWithTextBody Location "LOCATION:"
    where
        parseCRLF :: Parser Char String
        parseCRLF = token "\r\n"

        -- Parse a string literal and return a Token constructor without arguments
        parseLiteral :: Token -> String -> Parser Char Token
        parseLiteral t s = const t <$ token s <*> parseCRLF -- ff kieken voor andere manier

        -- Given a Token constructor which takes a Text body and a string literal which identifies (i.e. always precedes)
        -- the Token body in the text to parse, return a parser for this Token.
        parseWithTextBody :: (Text -> Token) -> String -> Parser Char Token
        parseWithTextBody t s = (\_ b -> t b) <$> token s <*> parseText <* parseCRLF
            where
                parseText :: Parser Char Text
                -- Parse a Text body as a series of series of characters with an optional CRLF + space at the end,
                -- ignoring the optional CRLF + space, then concatenate all the series of characters together to obtain the full Text
                parseText = concat <$> some (some (satisfy (/= '\r')) <* optional (token "\r\n "))
                            
        -- Given a Token constructor which takes a DateTime body and a string literal which identifies (i.e. always precedes)
        -- the Token body in the text to parse, return a parser for this Token.
        parseWithDTBody :: (DateTime -> Token) -> String -> Parser Char Token
        parseWithDTBody t s = (\_ b -> t b) <$> token s <*> parseDateTime <* parseCRLF

scanCalendar :: Parser Char [Token]
scanCalendar = some parseToken

parseEvent :: Parser Token Event
parseEvent = (\_ ts _ -> constructEvent ts) <$> symbol BeginEvent <*> some (satisfy (/= EndEvent)) <*> symbol EndEvent
    where
        -- Constructs an Event based on a list of tokens containing the required and optional eventprops
        constructEvent :: [Token] -> Event
        constructEvent ts = let sorted = sort ts
                                dtstamp'     = case sorted !! 0 of DTStamp dt -> dt
                                uid'         = case sorted !! 1 of UID u -> u
                                dtstart'     = case sorted !! 2 of DTStart dt -> dt
                                dtend'       = case sorted !! 3 of DTEnd dt -> dt
                                description' = (\(Description d) -> d) <$> find (\case (Description d) -> True; _ -> False) sorted
                                summary'     = (\(Summary s) -> s) <$> find (\case (Summary s) -> True; _ -> False) sorted
                                location'    = (\(Location l) -> l) <$> find (\case (Location l) -> True; _ -> False) sorted
                             in Event dtstamp' uid' dtstart' dtend' description' summary' location'

parseCalendar :: Parser Token Calendar
parseCalendar = (\_ pid es _ -> Calendar pid es) <$> symbol BeginCalendar <*> parseProdID <*> many parseEvent <*> symbol EndCalendar
    where
        -- Extracts the ProdID Text body from the two calprops, a Version and a ProdID, at the begin of the calendar,
        -- which can be in any order
        parseProdID :: Parser Token Text
        parseProdID = (\_ (ProdID t) -> t) <$> symbol Version <*> satisfy isProdID
                  <|> (\(ProdID t) _ -> t) <$> satisfy isProdID <*> symbol Version
            where
                isProdID :: Token -> Bool
                isProdID (ProdID _) = True
                isProdID _ = False

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar Calendar{prodid, events} =
    "BEGIN:VCALENDAR\r\n" ++
    "VERSION:2.0\r\n" ++
    printWithTextBody "PRODID:" prodid ++
    printEvents events ++
    "END:VCALENDAR\r\n"
    where
        printEvents :: [Event] -> String
        printEvents = foldl (\s e -> s ++ createEventString e) ""

        createEventString :: Event -> String
        createEventString Event{dtstamp, uid, dtstart, dtend, description, summary, location} =
            "BEGIN:VEVENT\r\n" ++
            printWithDTBody "DTSTAMP:" dtstamp ++
            printWithTextBody "UID:" uid ++
            printWithDTBody "DTSTART:" dtstart ++
            printWithDTBody "DTEND:" dtend ++
            printOptionalText "DESCRIPTION:" description ++
            printOptionalText "SUMMARY:" summary ++
            printOptionalText "LOCATION:" location ++
            "END:VEVENT\r\n"
        
        printWithTextBody :: String -> String -> String
        printWithTextBody pf b = insertNewLine (pf ++ b) ++ "\r\n"

        printWithDTBody :: String -> DateTime -> String 
        printWithDTBody pf b = pf ++ printDateTime b ++ "\r\n"

        printOptionalText :: String -> Maybe String -> String
        printOptionalText pf = maybe "" (printWithTextBody pf)
        
        insertNewLine :: String -> String
        insertNewLine cs | length cs > 42 = let (first, second) = splitAt 42 cs 
                                            in first ++ "\r\n " ++ second
                         | otherwise      = cs

                
