module Calendar where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime


-- Exercise 6
type Text = String

data Calendar = Calendar { prodid :: Text
                         , events :: [Event] }
    deriving (Eq, Ord, Show)

data Event = Event { dtstamp :: DateTime
                   , uid :: Text
                   , dtstart :: DateTime
                   , dtend :: DateTime
                   , description :: Maybe Text
                   , summary :: Maybe Text
                   , location :: Maybe Text }
    deriving (Eq, Ord, Show)

-- Exercise 7
data Token = BeginCalendar
           | EndCalendar
           | ProdID Text
           | Version
           | BeginEvent
           | EndEvent
           | DTStamp Text
           | UID Text
           | DTStart Text
           | DTEnd Text
           | Description Text
           | Summary Text
           | Location Text
    deriving (Eq, Ord, Show)

parseToken :: Parser Char Token
parseToken = parseLiteral BeginCalendar "BEGIN:VCALENDAR"
         <|> parseLiteral BeginCalendar "END:VCALENDAR"
         <|> parseWithBody ProdID "PRODID:"
         <|> parseLiteral Version "VERSION:2.0"
         <|> parseLiteral BeginEvent "BEGIN:VEVENT"
         <|> parseLiteral EndEvent "END:VEVENT"
         <|> parseWithBody DTStamp "DTSTAMP:"
         <|> parseWithBody UID "UID:"
         <|> parseWithBody DTStart "DTSTART:"
         <|> parseWithBody DTStamp "DTSTAMP:"
         <|> parseWithBody DTEnd "DTEND:" 
         <|> parseWithBody Description "DESCRIPTION:" 
         <|> parseWithBody Summary "SUMMARY:" 
         <|> parseWithBody Location "LOCATION:"
    where
        -- Parse a string literal and return a Token constructor without arguments
        parseLiteral :: Token -> String -> Parser Char Token
        parseLiteral t s = const t <$ token s <*> parseCRLF -- ff kieken voor andere manier

        -- Given a Token constructor which takes a Text body and a string literal which identifies (i.e. always precedes)
        -- the Token body in the text to parse, return a parser for this Token.
        parseWithBody :: (Text -> Token) -> String -> Parser Char Token
        parseWithBody t s = (\_ p _ -> t p) <$> token s <*> parseText <*> parseCRLF
            where
                parseText :: Parser Char Text
                parseText = some anySymbol
                        <|> token "\r\n "

        parseCRLF :: Parser Char String
        parseCRLF = token "\r\n"

scanCalendar :: Parser Char [Token]
scanCalendar = some parseToken

parseEvent :: Parser Token Event
parseEvent = Event <$> symbol BeginEvent
-- hier kunnen we niet alle mogelijke volgordes uitschrijven;
-- we moeten een list maken van alle tokens en dan gaan filteren en zoeken (voor de optionele elementen handig want Maybe)

parseCalendar :: Parser Token Calendar
parseCalendar = (\_ pid es -> Calendar pid es) <$> symbol BeginCalendar <*> parseProdID <*> many parseEvent
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
printCalendar = undefined
