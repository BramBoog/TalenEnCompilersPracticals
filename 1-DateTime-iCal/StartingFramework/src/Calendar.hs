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
        parseLiteral :: Token -> Text -> Parser Char Token
        parseLiteral c t = const c <$ token t <*> parseCRLF -- ff kieken voor andere manier

        parseWithBody :: (Text -> Token) -> Text -> Parser Char Token
        parseWithBody c t = (\_ p _ -> c p) <$> token t <*> parseText <*> parseCRLF
            where
                parseText :: Parser Char Text
                parseText = some anySymbol
                        <|> token "\r\n "

        parseCRLF :: Parser Char Text
        parseCRLF = token "\r\n"

scanCalendar :: Parser Char [Token]
scanCalendar = some parseToken

data CalProp = Version' | ProdID' Text

parseCalendar :: Parser Token Calendar
parseCalendar = (\_ p1 p2 es -> Calendar (returnID p1 p2) es) <$> symbol BeginCalendar <*> parseCalProp <*> parseCalProp <*> parseEvent
    where
        parseCalProp :: Parser Token CalProp
        parseCalProp = Version' <$ symbol Version
                   <|> (\(ProdID t) -> ProdID' t) <$> symbol (\t -> ProdID t)

        parseEvent :: Parser Token Event
        parseEvent = Event <$> symbol BeginEvent

        returnID p1(ProdID t) _ = t
        returnID _ p2(ProdID t) = t

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
