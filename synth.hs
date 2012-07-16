import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Binary.Put as P
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Int
import Data.Word

-- Time

type Sample = Integer
data Time = Infinite | Finite Sample deriving Eq

instance Num Time where
    (+) (Finite x) (Finite y) = Finite $ x + y
    (+) _ _ = Infinite
    (*) (Finite x) (Finite y) = Finite $ x * y
    (*) _ _ = Infinite
    abs Infinite = Infinite
    abs (Finite x) = Finite $ abs x
    signum Infinite = Infinite
    signum (Finite x) = Finite $ signum x
    fromInteger = Finite

maxTime :: Time -> Time -> Time
maxTime (Finite x) (Finite y) = Finite $ max x y
maxTime _ _ = Infinite

instance Ord Time where
    compare Infinite Infinite = EQ
    compare Infinite _ = GT
    compare _ Infinite = LT
    compare (Finite x) (Finite y) = compare x y

instance Show Time where
    show Infinite = show "Infinity"
    show (Finite x) = show x

sampleRate :: (Num a) => a
sampleRate = 44100

bytesPerSample :: (Num a) => a
bytesPerSample = 2

bpm x n = mins (n / x)
mins n = Finite $ round $ n * 60 * sampleRate
secs n = Finite $ round $ n * sampleRate
inf = Infinite :: Time

-- Synth

type Freq = Double
type Volume = Double
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

newtype GeneratorFn = GeneratorFn { getGenFn :: Sample -> WavePoint2 }
newtype AdjustmentFn = AdjustmentFn { getAdjFn :: Sample -> WavePoint2 -> WavePoint2 }

mkLeftGen f = GeneratorFn $ \i -> (f $ fromIntegral i, 0)
mkRightGen f = GeneratorFn $ \i -> (0, f $ fromIntegral i)
mkMonoGen f = GeneratorFn $ \i -> (f $ fromIntegral i, f $ fromIntegral i)
mkStereoGen f = GeneratorFn $ \i -> f $ fromIntegral i

mkLeftAdj f = AdjustmentFn $ \i -> \(l, r) -> (f (fromIntegral i) l, r)
mkRightAdj f = AdjustmentFn $ \i -> \(l, r) -> (l, f (fromIntegral i) r)
mkMonoAdj f = AdjustmentFn $ \i -> \(l, r) -> (f (fromIntegral i) l, f (fromIntegral i) r)
mkStereoAdj f = AdjustmentFn $ \i -> f $ fromIntegral i

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

evalStreams :: Sample -> [Stream] -> WavePoint2
evalStreams i = clip2 . sum2 . map (evalStream i)
    where sum2 = foldl accum (0, 0)
          accum (ls, lr) (l, r) = (ls+l, lr+r)
          clip2 (l, r) = (clip l, clip r)
          clip x | x <= lbound = lbound
                 | x >= ubound = ubound
                 | otherwise = x
          (lbound, ubound) = (-1 + 1e-9, 1 - 1e-9)

evalStream :: Sample -> Stream -> WavePoint2
evalStream _ (Generator Infinite _ _) = (0,0)
evalStream i (Generator (Finite s) Infinite g) | i <= s = (0,0)
                                               | otherwise = (getGenFn g) (i - s)
evalStream i (Generator (Finite s) (Finite e) g) | i <= s || i > e = (0,0)
                                                 | otherwise = (getGenFn g) (i - s)
evalStream i (Adjustment Infinite _ _ o) = evalStream i o
evalStream i (Adjustment (Finite s) Infinite a o) | i <= s = evalStream i o
                                                  | otherwise = (getAdjFn a) (i - s) (evalStream i o)
evalStream i (Adjustment (Finite s) (Finite e) a o) | i <= s || i > e = evalStream i o
                                                    | otherwise = (getAdjFn a) (i - s) (evalStream i o)

type StreamKey = Integer
type PlayerState = State Player
data Player = Player { allStreams :: M.Map StreamKey Stream
                     , currentTime :: Time } deriving Show


getMaxKey :: (Num k) => M.Map k a -> k
getMaxKey as = case M.maxViewWithKey as of Nothing -> 0
                                           Just ((k, _), _) -> k

execAndGetStreams :: PlayerState a -> ([Stream], Time)
execAndGetStreams c = (M.elems as, ct)
    where (Player as ct) = execState c initialState
          initialState = Player M.empty $ Finite 0

play :: Time -> GeneratorFn -> PlayerState StreamKey
play d g = state $ \(Player as t) ->
    (nextKey as, Player (M.insert (nextKey as) (gen t) as) t)
        where nextKey = (+1) . getMaxKey
              gen t = Generator t (t + d) g

patch :: Time -> StreamKey -> AdjustmentFn -> PlayerState StreamKey
patch d k a = state $ \(Player as t) ->
    (k, Player (M.adjust (\s -> adj t s) k as) t)
        where adj t s = Adjustment t (t + d) a s

wait :: Time -> PlayerState ()
wait d = state $ \(Player as t) -> ((), Player as (t + d))

stopIfPlaying :: Time -> Stream -> Stream
stopIfPlaying t s = s {streamEnd = min (streamEnd s) t}

stopAll :: PlayerState ()
stopAll = state $ \(Player as t) -> ((), Player (M.map (stopIfPlaying t) as) t)

stop :: StreamKey -> PlayerState StreamKey
stop k = state $ \(Player as t) -> (k, Player (M.adjust (stopIfPlaying t) k as) t)

-- playF = play inf
-- patchF = patch inf
playAndWait d g = play d g >>= (\k -> wait d >> return k)

-- Encoder

writeWAVFile :: FilePath -> PlayerState a -> IO ()
writeWAVFile fp = (B.writeFile fp) . genWAVFile

genWAVFile :: PlayerState a -> B.ByteString
genWAVFile = P.runPut . putWAV . mix . execAndGetStreams

putWAV :: (Sample, P.Put) -> P.Put
putWAV (numSamples, audioDataPut) = do
    P.putLazyByteString $ B.pack riff
    P.putWord32le $ fromInteger $ 36 + sub2Size
    P.putLazyByteString $ B.pack wave
    P.putLazyByteString $ B.pack fmt
    P.putWord32le 16          -- Subchunk Size
    P.putWord16le 1           -- Audio Format (PCM)
    P.putWord16le 2           -- Num Channels
    P.putWord32le sampleRate
    P.putWord32le $ sampleRate * 2 * bytesPerSample
    P.putWord16le $ 2 * bytesPerSample
    P.putWord16le $ 8 * bytesPerSample
    P.putLazyByteString $ B.pack dataS
    P.putWord32le $ fromInteger $ sub2Size
    audioDataPut
        where riff = [0x52, 0x49, 0x46, 0x46]
              wave = [0x57, 0x41, 0x56, 0x45]
              fmt = [0x66, 0x6d, 0x74, 0x20]
              dataS = [0x64, 0x61, 0x74, 0x61]
              sub2Size = numSamples * 2 * bytesPerSample

mix :: ([Stream], Time) -> (Sample, P.Put)
mix (as, t) = (numSamples, mapM_ (encodeStreams as) [1..numSamples])
    where numSamples = timeToSamples timeSamples
          timeSamples = foldl maxTime t (map streamEnd as)
          timeToSamples Infinite = 0
          timeToSamples (Finite x) = x

encodeStreams :: [Stream] -> Sample -> P.Put
encodeStreams as i = do 
    P.putWord16le $ f l
    P.putWord16le $ f r
        where (l, r) = evalStreams i as
              f x = round $ x * 2^(8 * bytesPerSample - 1)
          -- mapM_ (encodeBytes (f l, f r)) [1..bytesPerSample]

encodeBytes :: (Int64, Int64) -> Int -> P.Put
encodeBytes (l, r) n = do
    P.putWord8 $ f l
    P.putWord8 $ f r
        where f x = fromIntegral x `shiftR` (8 * (n - 1)) .&. 0xff

-- Example

sine :: Freq -> Volume -> GeneratorFn
sine f v = mkMonoGen $ \i -> v * (sin $ 2 * pi * i * f)

sawtooth :: Freq -> Volume -> GeneratorFn
sawtooth f v = mkMonoGen $ \i -> let x = i * f in 2 * v * (x - fromIntegral (floor x) - 0.5)

amplify :: Volume -> AdjustmentFn
amplify v = mkMonoAdj $ \_ -> \x -> v * x

beat = bpm 60.0

melody = do
    chord sine
    chord sawtooth

chord s = do
    play (beat 4) (s (note "C") 0.3)
    wait (beat 1)
    play (beat 3) (s (note "E") 0.2)
    wait (beat 1)
    play (beat 2) (s (note "G") 0.2)
    wait (beat 2)
    
    -- mapM_ (\n -> play inf (sine (note n) 0.2)) ["G","F","G"]
    -- wait (beat 1)
    -- patch inf c (amplify 2)
    -- patch inf d (amplify 2)

main = writeWAVFile "sample.wav" melody
