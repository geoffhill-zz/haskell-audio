import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Word

-- Time

sampleRate :: (Num a) => a
sampleRate = 192000

bytesPerSample :: (Num a) => a
bytesPerSample = 3

data Time = Instant | Samples Rational | Seconds Rational | Minutes Rational | Forever deriving Eq
(inst, spl, sec, mins, inf) = (Instant, Samples, Seconds, Minutes, Forever)

bpm :: Rational -> Rational -> Time
bpm x n = Minutes $ n / x

spls :: Time -> Maybe Rational
spls Instant = Just $ 0
spls (Samples i) = Just $ i
spls (Seconds i) = Just $ i * sampleRate
spls (Minutes i) = Just $ i * 60 * sampleRate
spls Forever = Nothing

tapp op x = op <$> spls x
tapp2 op x y = op <$> spls x <*> spls y

tconv op x = propInf $ tapp op x
tconv2 op x y = propInf $ tapp2 op x y
propInf Nothing = Forever
propInf (Just x) = Samples x

instance Num Time where
    (+) = tconv2 (+)
    (*) = tconv2 (*)
    (-) = tconv2 (-)
    negate = tconv negate
    abs = tconv abs
    signum = tconv signum
    fromInteger = Samples . fromInteger

instance Ord Time where
    Forever `compare` Forever = EQ
    Forever `compare` y = GT
    x `compare` Forever = LT
    x `compare` y = case tapp2 compare x y of Nothing -> EQ
                                              (Just x) -> x

instance Show Time where
    show Instant = show 0
    show (Samples i) = show i
    show (Seconds i) = show i ++ "s"
    show (Minutes i) = show i ++ "m"
    show Forever = "Inf"

-- Synth

type Freq = Double
type Volume = Double
type SampleNum = Integer
type WavePoint = Double
type WavePoint2 = (WavePoint, WavePoint)

note :: String -> Freq
note = (/ sampleRate) . noteS
noteS "A" = 440.0
noteS "A#" = 466.16
noteS "Bb" = 466.16
noteS "B" = 493.88
noteS "C" = 523.25
noteS "C#" = 554.37
noteS "Db" = 554.37
noteS "D" = 587.33
noteS "D#" = 622.25
noteS "Eb" = 622.25
noteS "E" = 659.26
noteS "F" = 698.46
noteS "F#" = 739.99
noteS "Gb" = 739.99
noteS "G" = 783.99
noteS "G#" = 830.61
noteS "Ab" = 830.61
noteS _ = 440.0

newtype GeneratorFn = GeneratorFn { getGenFn :: SampleNum -> WavePoint2 }
newtype AdjustmentFn = AdjustmentFn { getAdjFn :: SampleNum -> WavePoint2 -> WavePoint2 }

mkLeftGen f = GeneratorFn $ \i -> (f i, 0)
mkRightGen f = GeneratorFn $ \i -> (0, f i)
mkMonoGen f = GeneratorFn $ \i -> (f i, f i)
mkStereoGen = GeneratorFn

mkLeftAdj f = AdjustmentFn $ \i -> \(l, r) -> (f i l, r)
mkRightAdj f = AdjustmentFn $ \i -> \(l, r) -> (l, f i r)
mkMonoAdj f = AdjustmentFn $ \i -> \(l, r) -> (f i l, f i r)
mkStereoAdj = AdjustmentFn

-- Player

data Stream = Generator { streamStart :: Time
                        , streamEnd :: Time
                        , generator :: GeneratorFn }
            | Adjustment { streamStart :: Time
                         , streamEnd :: Time
                         , adjustment :: AdjustmentFn
                         , original :: Stream }

instance Show Stream where
    show (Generator s e _) = concat ["GEN(", show s, "-", show e, ")"]
    show (Adjustment s e _ o) = concat ["ADJ(", show s, "-", show e, ") {", show o, "}"]

evalStreams :: SampleNum -> [Stream] -> WavePoint2
evalStreams i = sum2 . map (evalStream i)
    where sum2 = foldl accum (0, 0)
          accum (ls, lr) (l, r) = (ls+l, lr+r)

evalStream :: SampleNum -> Stream -> WavePoint2
evalStream i (Generator s e g) | fromInteger i < s = (0,0)
                               | fromInteger i > e = (0,0)
                               | otherwise = evalGen (fromInteger i - s) g
evalStream i (Adjustment s e a o) | fromInteger i < s = (0,0)
                                  | fromInteger i > e = (0,0)
                                  | otherwise = evalAdj (fromInteger i - s) a (evalStream i o)

evalGen :: Time -> GeneratorFn -> WavePoint2
evalGen i 

type StreamKey = Integer
data Player = Player { allStreams :: Map.Map StreamKey Stream
                     , currentTime :: Time } deriving Show

getMaxKey :: (Num k) => Map.Map k a -> k
getMaxKey as = case Map.maxViewWithKey as of Nothing -> 0
                                             Just ((k, _), _) -> k

execAndGetStreams :: State Player a -> [Stream]
execAndGetStreams c = Map.elems $ allStreams finalState
    where finalState = execState c initialState
          initialState = Player Map.empty 0

play :: Time -> GeneratorFn -> State Player StreamKey
play d g = state $ \(Player as t) ->
    (nextKey as, Player (Map.insert (nextKey as) (gen t) as) t)
        where nextKey = (+1) . getMaxKey
              gen t = Generator t (t + d) g

patch :: Time -> StreamKey -> AdjustmentFn -> State Player StreamKey
patch d k a = state $ \(Player as t) ->
    (k, Player (Map.adjust (\s -> adj t s) k as) t)
        where adj t s = Adjustment t (t + d) a s

wait :: Time -> State Player ()
wait d = state $ \(Player as t) -> ((), Player as (t + d))

stopAll :: State Player ()
stopAll = state $ \(Player as t) -> ((), Player (Map.map (\s -> s {streamEnd = t}) as) t)

stop :: StreamKey -> State Player StreamKey
stop k = state $ \(Player as t) -> (k, Player (Map.adjust (\s -> s {streamEnd = t}) k as) t)

playF = play Forever
patchF = patch Forever
playAndWait d g = play d g >>= (\k -> wait d >> return k)

-- Encoder

writeWAVFile :: FilePath -> State Player a -> IO ()
writeWAVFile fp c = B.putStr $ genWAVFile c

genWAVFile :: State Player a -> B.ByteString
genWAVFile = (P.runPut . putWAV . mix . execAndGetStreams)

putWAV :: (Word32, B.ByteString) -> P.Put
putWAV (numSamples, sampleData) = do
    P.putLazyByteString $ B.pack "RIFF"
    P.putWord32le 0
    P.putLazyByteString $ B.pack "WAVE"
    P.putLazyByteString $ B.pack "fmt "
    P.putWord32le 16   -- Subchunk Size
    P.putWord16le 1    -- Audio Format (PCM)
    P.putWord16le 2    -- Num Channels
    P.putWord32le sampleRate
    P.putWord32le $ sampleRate * 2 * bytesPerSample
    P.putWord16le $ 2 * bytesPerSample
    P.putWord16le $ 8 * bytesPerSample
    P.putLazyByteString $ B.pack "data"
    P.putWord32le $ numSamples * 2 * bytesPerSample
    P.putLazyByteString sampleData
    
mix :: [Stream] -> (Word32, B.ByteString)
mix as = (0, B.pack "")

-- Example

sine :: Freq -> Volume -> GeneratorFn
sine f v = mkMonoGen w
    where w i = v * (sin $ fromIntegral i * 2 * pi * f)

amplify :: Volume -> AdjustmentFn
amplify v = mkMonoAdj w
    where w _ x = v * x

beat = bpm 120.0

melody = do
    c <- play (beat 8) (sine (note "C") 0.5)
    wait (beat 1)
    d <- play (beat 7) (sine (note "D") 0.5)
    wait (beat 1)
    mapM_ (\n -> playF (sine (note n) 0.5)) ["E","F","G"]
    wait (beat 1)
    patchF c (amplify 1.1)
    patchF d (amplify 1.1)
    wait (beat 11)
    stopAll

main = writeWAVFile "sample.wav" melody
