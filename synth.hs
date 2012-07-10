import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import Control.Monad.State

-- Time

sampleRate :: (Num a) => a
sampleRate = 192000

data Time a = Instant | Samples a | Seconds a | Minutes a | Forever
(inst, spl, sec, mins, inf) = (Instant, Samples, Seconds, Minutes, Forever)

bpm :: (Fractional a) => a -> a -> Time a
bpm x n = Minutes $ n / x

spls :: (Num a) => Time a -> Maybe a
spls Instant = Just $ 0
spls (Samples i) = Just $ i
spls (Seconds i) = Just $ i * sampleRate
spls (Minutes i) = Just $ i * 60 * sampleRate
spls Forever = Nothing

propInf Nothing = Forever
propInf (Just x) = Samples x
tapp op x = propInf $ op <$> spls x
tapp2 op x y = propInf $ op <$> spls x <*> spls y

instance (Num a) => Num (Time a) where
    (+) = tapp2 (+)
    (*) = tapp2 (*)
    (-) = tapp2 (-)
    negate = tapp negate
    abs = tapp abs
    signum = tapp signum
    fromInteger = Samples . fromInteger

instance (Show a) => Show (Time a) where
    show Instant = show 0
    show (Samples i) = show i
    show (Seconds i) = show i ++ "s"
    show (Minutes i) = show i ++ "m"
    show Forever = "Inf"

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

newtype GeneratorFn = GeneratorFn { getGenFn :: Integer -> WavePoint2 }
newtype AdjustmentFn = AdjustmentFn { getAdjFn :: Integer -> WavePoint2 -> WavePoint2 }

mkLeftGen f = GeneratorFn $ \i -> (f i, 0)
mkRightGen f = GeneratorFn $ \i -> (0, f i)
mkMonoGen f = GeneratorFn $ \i -> (f i, f i)
mkStereoGen = GeneratorFn

mkLeftAdj f = AdjustmentFn $ \i -> \(l, r) -> (f i l, r)
mkRightAdj f = AdjustmentFn $ \i -> \(l, r) -> (l, f i r)
mkMonoAdj f = AdjustmentFn $ \i -> \(l, r) -> (f i l, f i r)
mkStereoAdj = AdjustmentFn


-- Player

data Stream a = Generator { streamStart :: Time a
                          , streamEnd :: Time a
                          , generator :: GeneratorFn }
              | Adjustment { streamStart :: Time a
                           , streamEnd :: Time a
                           , adjustment :: AdjustmentFn
                           , source :: Stream a }

instance (Show a) => Show (Stream a) where
    show (Generator s e _) = concat ["GEN(", show s, "-", show e, ")"]
    show (Adjustment s e _ o) = concat ["ADJ(", show s, "-", show e, ") {", show o, "}"]

type StreamKey = Integer
data Player a = Player { allStreams :: Map.Map StreamKey (Stream a)
                       , currentTime :: Time a} deriving Show

getMaxKey :: (Num k) => Map.Map k a -> k
getMaxKey as = case Map.maxViewWithKey as of Nothing -> 0
                                             Just ((k, _), _) -> k

execAndGetStreams :: (Num a) => State (Player a) b -> [Stream a]
execAndGetStreams c = Map.elems $ allStreams finalState
    where finalState = execState c initialState
          initialState = Player Map.empty 0

play :: (Num a) => Time a -> GeneratorFn -> State (Player a) StreamKey
play d g = state $ \(Player as t) ->
    (nextKey as, Player (Map.insert (nextKey as) (gen t) as) t)
        where nextKey = (+1) . getMaxKey
              gen t = Generator t (t + d) g

patch :: (Num a) => Time a -> StreamKey -> AdjustmentFn -> State (Player a) StreamKey
patch d k a = state $ \(Player as t) ->
    (k, Player (Map.adjust (\s -> adj t s) k as) t)
        where adj t s = Adjustment t (t + d) a s

wait :: (Num a) => Time a -> State (Player a) ()
wait d = state $ \(Player as t) -> ((), Player as (t + d))

stopAll :: State (Player a) ()
stopAll = state $ \(Player as t) -> ((), Player (Map.map (\s -> s {streamEnd = t}) as) t)

stop :: StreamKey -> State (Player a) StreamKey
stop k = state $ \(Player as t) -> (k, Player (Map.adjust (\s -> s {streamEnd = t}) k as) t)

playF = play Forever
patchF = patch Forever
playAndWait d g = play d g >>= (\k -> wait d >> return k)

-- Encoder

writeWaveFile :: (Num a) => FilePath -> State (Player a) b -> IO ()
writeWaveFile fp c = print $ execAndGetStreams melody 


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

main = print $ execAndGetStreams melody
