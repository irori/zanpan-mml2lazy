import Data.List
import Data.Word
import qualified Data.ByteString.Lazy as B

durUnit :: Int
durUnit = 64
samplingRate :: Int
samplingRate = 8000
volume :: Word8
volume = 40

data Context = Context {
      tempo :: Int
    , octave :: Int
    , defaultLength :: Int
    } deriving (Show)

initialContext = Context { tempo = 183, octave = 0, defaultLength = quot durUnit 4 }

data Note = Rest Dur
          | Note Freq Dur
     deriving Show

type Dur = Int
type Freq = Int

parsePart :: Context -> String -> [Note]
parsePart ctx [] = []
parsePart ctx ('>':s) = parsePart ctx{ octave = octave ctx + 1 } s
parsePart ctx ('<':s) = parsePart ctx{ octave = octave ctx - 1 } s
parsePart ctx ('l':s) = parsePart ctx{ defaultLength = len } s'
    where (len, s') = readLen ctx s
parsePart ctx ('r':s) = Rest (duration ctx len) : parsePart ctx s'
    where (len, s') = readLen ctx s
parsePart ctx s = Note freq (duration ctx len) : parsePart ctx s''
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
    case reads s of
      [(n, '.':s')] -> (quot (durUnit * 3) (2 * n), s')
      [(n, s')] -> (quot durUnit n, s')
      [] -> (defaultLength ctx, s)

duration :: Context -> Int -> Dur
duration ctx r = r * (quot (fromIntegral samplingRate * 240) (tempo ctx * durUnit))

play :: [Note] -> [Word8]
play [] = []
play (Rest dur : ns) = replicate dur 0 ++ play ns
play (Note freq dur : ns) = take dur (square freq) ++ play ns

square :: Int -> [Word8]
square freq = cycle $ replicate t volume ++ replicate t 0
    where t = round $ fromIntegral samplingRate / 2 / fromIntegral freq

part1 = "l32aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar4<<l8cccccccc32c32c32c32dddddddd16d16<bbbbbbbb32b32b32b32>eeee32e32e32e32dddd32d32d32d32cccccccc16c16dddddddd32d32d32d32eereeereeereeere16e16cccccccl32ccccl8dddddddd16d16<bbbbbbbb16b16>eeee16e16dddl32ddddl8<aaaaaaaabbbbbbbb>ee8r2eeee8r4.r4e32e32e32e32rererererereeef4rererererereeef4eer2eer2eel32eeeeeeeel8eer2eer2eef4rererererereeef4rererererereeef4ccccccc>c<ddddddd>d<<bbbbbbb>beee>e<ddd>d<<aaaaaaaa32a32a32a32a+a+a+a+a+a+a+a+32a+32a+32a+32bbbbbbbbbbbbbbbb>>a32a+32b16ae<bcc16c16>c<cc>c<c>c<dd16d16>d<dd>d<d>d<<bb16b16>b<bb>b<b>l32bbbbl8ee16e16>e<edd16d16>d<dcc16c16>c<cc>c<c>l32ccccl8<dd16d16>d<dd>d<d>l32ddddl8<eereeereeereeerecc16c16>c<cc>c<c>c32c32c32c32<dd16d16>d<dd>d<d>d<<bb16b16>b<bb>b<b>bee16e16>e<edd16d16>d<d<aa1"
part2 = "l4r1.bagagab2a32b8.r32agageg2bagagb.l8dd32e16.dede4edd32e16.dedg4g2l4bagagab2bagageg2g+32a32b8.agagb.l8dedede4edededf32f+32g8.gr1r1r1r1dede4.dedededg4gdede4.dedededf32f+32g.l4gr1r1r1.reee8dcab2r8egf+8deae2r8ege8g2e8gef+g8g8f+ef+ab.a8l32gagagagagagagagar2l4rbagf+32g32a8.gab2bagageg2bagagb.l8dedede4edededg4g2l4a32a+32b"
part3 = "l4r1.gf+ef+ef+g2gf+ef+ede2gf+ef+eg.r1r1r4.gf+ef+ef+g2gf+ef+ede2gf+ef+eg2l1rrrrl8r<ereeereeereerf4l1rrrrrrl8rereeereeereerf4l1rrrrrrrrr2.>l4gf+ef+ef+g2gf+ef+ede2gf+ef+eg.r1r1r4.gf+ef+ef+g2gf+ef+ede2"

music = map (foldl1 (+)) (transpose parts)
    where parts = map (play . parsePart initialContext) [part1, part2, part3]
main = B.putStr (B.pack music)
--main = putStrLn $ show $ parsePart initialContext part1