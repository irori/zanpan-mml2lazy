import Char
import Numeric
import Data.List
import Data.Word
import qualified Data.ByteString.Lazy as B

durUnit :: Int
durUnit = 64
samplingRate :: Int
samplingRate = 8000

data Context = Context {
      tempo :: Int
    , octave :: Int
    , defaultLength :: Int
    , volume :: Vol
    } deriving (Show)

initialContext = Context { tempo = 183,
                           octave = 0,
                           defaultLength = quot durUnit 4,
                           volume = 16
                         }

data Note = Rest Dur
          | Note Freq Dur Vol
     deriving Show

type Dur = Int
type Freq = Int
type Vol = Word8

parsePart :: Context -> String -> [Note]
parsePart ctx [] = []
parsePart ctx ('>':s) = parsePart ctx{ octave = octave ctx + 1 } s
parsePart ctx ('<':s) = parsePart ctx{ octave = octave ctx - 1 } s
parsePart ctx ('l':s) = parsePart ctx{ defaultLength = len } s'
    where (len, s') = readLen ctx s
parsePart ctx ('v':s) = parsePart ctx{ volume = vol } s'
    where [(vol, s')] = readDec s
parsePart ctx ('o':s) = parsePart ctx{ octave = oct - 4 } s'
    where [(oct, s')] = readDec s
parsePart ctx ('r':s) = Rest (duration ctx len) : parsePart ctx s'
    where (len, s') = readLen ctx s
parsePart ctx s = Note freq (duration ctx len) (volume ctx) : parsePart ctx s''
    where (freq, s') = readPitch ctx s
          (len, s'') = readLen ctx s'

readPitch :: Context -> String -> (Freq, String)
readPitch ctx (ch : '+' : s) = (frequency (tone ch + 1) (octave ctx), s)
readPitch ctx (ch : '-' : s) = (frequency (tone ch - 1) (octave ctx), s)
readPitch ctx (ch : s) = (frequency (tone ch) (octave ctx), s)

tone :: Char -> Int
tone 'c' = -9
tone 'd' = -7
tone 'e' = -5
tone 'f' = -4
tone 'g' = -2
tone 'a' = 0
tone 'b' = 2
tone c = error $ "invalid character " ++ [c]

frequency :: Int -> Int -> Int
frequency n oct = round $ 440 * 2 ** fromRational (fromIntegral n / 12 + fromIntegral oct)

readLen :: Context -> String -> (Int, String)
readLen ctx ('.':s) = (quot (defaultLength ctx * 3) 2, s)
readLen ctx s =
    case readDec s of
      [(n, '.':s')] -> (quot (durUnit * 3) (2 * n), s')
      [(n, s')] -> (quot durUnit n, s')
      [] -> (defaultLength ctx, s)

duration :: Context -> Int -> Dur
duration ctx r = r * (quot (fromIntegral samplingRate * 240) (tempo ctx * durUnit))

play :: [Note] -> [Word8]
play [] = []
play (Rest dur : ns) = replicate dur 0 ++ play ns
play (Note freq dur vol : ns) = take dur (square freq vol) ++ play ns

square :: Int -> Vol -> [Vol]
square freq vol = cycle $ replicate th vol ++ replicate tl 0
    where t = round $ fromIntegral samplingRate / fromIntegral freq
          th = quot t 2
          tl = t - th

parseMML :: String -> [[Note]]
parseMML mml = [parsePart initialContext $ map toLower line | line <- lines mml]

playMML :: String -> [Word8]
playMML mml = map (foldl1 (+)) (transpose parts)
    where parts = map play (parseMML mml)

main = do input <- getContents
          B.putStr $ B.pack $ playMML input
--          mapM_ putStrLn $ map show $ parseMML input
