{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Calendar where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime

import Data.List (sort, find)
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
         <|> parseLiteral      BeginCalendar "END:VCALENDAR"
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
        -- Check if this is a CRLF separating two tokens, or one in a Text, which is determined by whether there's a ' ' character afther the \r\n.
        -- Looking ahead allows only consuming the CRLF and not the character after preemptively.
        parseCRLF = look >>= (\case
                                '\r':'\n':' ':_ -> failp
                                '\r':'\n':_ -> token "\r\n"
                                _ -> failp
                             )
        
        -- Parse a string literal and return a Token constructor without arguments
        parseLiteral :: Token -> String -> Parser Char Token
        parseLiteral t s = const t <$ token s <*> parseCRLF -- ff kieken voor andere manier

        -- Given a Token constructor which takes a Text body and a string literal which identifies (i.e. always precedes)
        -- the Token body in the text to parse, return a parser for this Token.
        parseWithTextBody :: (Text -> Token) -> String -> Parser Char Token
        parseWithTextBody t s = (\_ b _ -> t b) <$> token s <*> parseText <*> parseCRLF
            where
                parseText :: Parser Char Text
                parseText = some anySymbol

        -- Given a Token constructor which takes a DateTime body and a string literal which identifies (i.e. always precedes)
        -- the Token body in the text to parse, return a parser for this Token.
        parseWithDTBody :: (DateTime -> Token) -> String -> Parser Char Token
        parseWithDTBody t s = (\_ b _ -> t b) <$> token s <*> parseDateTime <*> parseCRLF

scanCalendar :: Parser Char [Token]
scanCalendar = some parseToken

parseEvent :: Parser Token Event
parseEvent = (\_ ts _ -> constructEvent ts) <$> symbol BeginEvent <*> some anySymbol <*> symbol EndEvent
    where
        -- Constructs an Event based on a list of tokens containing the required and optional eventprops
        constructEvent :: [Token] -> Event
        constructEvent ts = let sorted = sort ts
                                dtstamp'     = case sorted !! 0 of DTStamp dt -> dt
                                uid'         = case sorted !! 1 of UID u -> u
                                dtstart'     = case sorted !! 2 of DTStart dt -> dt
                                dtend'       = case sorted !! 3 of DTEnd dt -> dt
                                description' = (\(Description d) -> d) <$> find (\(Description d) -> True) sorted
                                summary'     = (\(Summary s) -> s) <$> find (\ (Summary s) -> True) sorted
                                location'    = (\(Location l) -> l) <$> find (\(Location l) -> True) sorted
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
                isProdID (ProdID _) = True
                isProdID _ = False

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar (Calendar id es) = 
    "BEGIN:VCALENDAR\r\n" ++ 
    "VERSION:2.0\r\n" ++
    "PRODID:" ++ id ++ "\r\n" ++
    printEvents es ++
    "END:VCALENDAR\r\n"
    where
        printEvents = foldl (\s e -> s ++ createEventString e) ""
        createEventString Event{dtstamp, uid, dtstart, dtend, description, summary, location} = 
            "BEGIN:VEVENT\r\n" ++
            "DTSTAMP:" ++ printDateTime dtstamp ++ "\r\n" ++
            "UID:" ++ uid ++ "\r\n" ++
            "DTSTART:" ++ printDateTime dtstart ++ "\r\n" ++
            "DTEND:" ++ printDateTime dtend ++ "\r\n" ++
            printOptionalText "DESCRIPTION:" description ++
            printOptionalText "SUMMARY:" summary ++
            printOptionalText "LOCATION:" location ++
            "END:VEVENT\r\n"
        printOptionalText pf = maybe "" (\t -> pf ++ t ++ "\r\n")
