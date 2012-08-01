{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, TupleSections #-}

module Main where

import Control.Monad.State (State, state, execState)
import qualified Data.Map as M
import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy as B
import Data.Bits ((.&.), shiftR)
import Data.Ratio ((%))
import Data.Int
import Data.Word

-- Time

type Picosecond = Int64
data Time = Infinite | Finite Picosecond deriving (Eq, Read, Show)

timeMaybe :: a -> (Picosecond -> a) -> Time -> a
timeMaybe x _ Infinite = x
timeMaybe _ f (Finite y) = f y

timeTryOrInf :: (Picosecond -> Picosecond) -> Time -> Time
timeTryOrInf f = timeMaybe Infinite (Finite . f)

timeTryOrInf3 :: (Picosecond -> Picosecond -> Picosecond) -> Time -> Time -> Time
timeTryOrInf3 f x y = timeMaybe Infinite g x
    where g s = timeMaybe Infinite (Finite . (f s)) y

instance Num Time where
    (+) = timeTryOrInf3 (+)
    (*) = timeTryOrInf3 (*)
    abs = timeTryOrInf abs
    signum = timeTryOrInf signum
    fromInteger = Finite . fromInteger

maxTime :: Time -> Time -> Time
maxTime (Finite x) (Finite y) = Finite $ max x y
maxTime _ _ = Infinite

instance Ord Time where
    compare Infinite Infinite = EQ
    compare Infinite _ = GT
    compare _ Infinite = LT
    compare (Finite x) (Finite y) = compare x y

secs :: (RealFrac a) => a -> Time
secs = fromIntegral . round . (* (1000000000000))

mins :: (RealFrac a) => a -> Time
mins = secs . (* 60)

inf :: Time
inf = Infinite

bpm :: (RealFrac a) => a -> a -> Time
bpm x n = mins (n / x)

-- Synth

type Freq = Double
type Volume = Double
type Point = Double
newtype Point2 = Point2 { chans :: (Point, Point) } deriving (Eq, Read, Show)

leftApp, rightApp, monoApp :: (Point -> Point) -> Point2 -> Point2
leftApp f (Point2 (u, v)) = Point2 (f u, v)
rightApp f (Point2 (u, v)) = Point2 (u, f v)
monoApp f (Point2 (u, v)) = Point2 (f u, f v)

stereoApp :: ((Point, Point) -> (Point, Point)) -> Point2 -> Point2
stereoApp f (Point2 s) = Point2 (f s)

leftApp2, rightApp2, monoApp2 :: (Point -> Point -> Point) -> Point2 -> Point2 -> Point2
leftApp2 f (Point2 (u, v)) (Point2 (x, y)) = Point2 (f u x, v)
rightApp2 f (Point2 (u, v)) (Point2 (x, y)) = Point2 (u, f v y)
monoApp2 f (Point2 (u, v)) (Point2 (x, y)) = Point2 (f u x, f v y)

mtPoint2 :: Point2
mtPoint2 = Point2 (0, 0)

instance Num Point2 where
    (+) = monoApp2 (+)
    (*) = monoApp2 (*)
    abs = monoApp abs
    signum = monoApp signum
    fromInteger x = Point2 (i, i)
        where i = fromInteger x

note :: String -> Freq
note = (/ 1000000000000) . (notes M.!)

notes :: M.Map String Freq
notes = M.fromList [ ("A", 440.00), ("A#", 466.16), ("Bb", 466.16), ("B", 493.88)
                   , ("C", 523.25), ("C#", 554.37), ("Db", 554.37), ("D", 587.33)
                   , ("D#", 622.25), ("Eb", 622.25), ("E", 659.26), ("F", 698.46)
                   , ("F#", 739.99), ("Gb",739.99), ("G", 783.99), ("G#", 830.61)
                   , ("Ab", 830.61) ]

newtype GeneratorFn = GeneratorFn { getGenFn :: Picosecond -> Point2 }
newtype AdjustmentFn = AdjustmentFn { getAdjFn :: Picosecond -> Point2 -> Point2 }

-- Simple generalized constructors for generators and adjustors

mkLeftGen, mkRightGen, mkMonoGen :: (Num a) => (a -> Point) -> GeneratorFn
mkLeftGen f = GeneratorFn $ Point2 . (, 0) . f . fromIntegral
mkRightGen f = GeneratorFn $ Point2 . (0 ,) . f . fromIntegral
mkMonoGen f = GeneratorFn $ Point2 . (\x -> (x, x)) . f . fromIntegral

mkStereoGen :: (Num a) => (a -> (Point, Point)) -> GeneratorFn
mkStereoGen f = GeneratorFn $ Point2 . f . fromIntegral

mkLeftAdj, mkRightAdj, mkMonoAdj :: (Num a) => (a -> Point -> Point) -> AdjustmentFn
mkLeftAdj f = AdjustmentFn $ leftApp . f . fromIntegral
mkRightAdj f = AdjustmentFn $ rightApp . f . fromIntegral
mkMonoAdj f = AdjustmentFn $ monoApp . f . fromIntegral

mkStereoAdj :: (Num a) => (a -> (Point, Point) -> (Point, Point)) -> AdjustmentFn
mkStereoAdj f = AdjustmentFn $ stereoApp . f . fromIntegral

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

generationStart :: Stream -> Time
generationStart (Generator s _ _) = s
generationStart (Adjustment _ _ _ o) = generationStart o

generationEnd :: Stream -> Time
generationEnd (Generator _ e _) = e
generationEnd (Adjustment _ _ _ o) = generationEnd o

evalStreams :: Picosecond -> [Stream] -> Point2
evalStreams i = clip2 . sum2 . map (evalStream i)
    where sum2 = foldr (+) mtPoint2
          clip2 = monoApp clip
          clip x | x <= lbound = lbound
                 | x >= ubound = ubound
                 | otherwise = x
          (lbound, ubound) = (-1 + 1e-9, 1 - 1e-9)

evalStream :: Picosecond -> Stream -> Point2
evalStream _ (Generator Infinite _ _) = mtPoint2
evalStream i (Generator (Finite s) Infinite g) | i <= s = mtPoint2
                                               | otherwise = (getGenFn g) (i - s)
evalStream i (Generator (Finite s) (Finite e) g) | i <= s || i > e = mtPoint2
                                                 | otherwise = (getGenFn g) (i - s)
evalStream i (Adjustment Infinite _ _ o) = evalStream i o
evalStream i (Adjustment (Finite s) Infinite a o) | i <= s = evalStream i o
                                                  | otherwise = (getAdjFn a) (i - s) (evalStream i o)
evalStream i (Adjustment (Finite s) (Finite e) a o) | i <= s || i > e = evalStream i o
                                                    | otherwise = (getAdjFn a) (i - s) (evalStream i o)

type StreamKey = Int32
type PlayerState = State Player
data Player = Player { allStreams :: M.Map StreamKey Stream
                     , currentTime :: Time } deriving (Show)

getMaxKey :: (Num k) => M.Map k a -> k
getMaxKey as = case M.maxViewWithKey as of Nothing -> 0
                                           Just ((k, _), _) -> k

execAndGetStreams :: PlayerState a -> ([Stream], Time)
execAndGetStreams c = (M.elems as, ct)
    where (Player as ct) = execState c initialState
          initialState = Player M.empty (Finite 0)

play :: Time -> GeneratorFn -> PlayerState StreamKey
play d g = state $ \(Player as t) ->
    (nextKey as, Player (M.insert (nextKey as) (gen t) as) t)
        where nextKey = (+1) . getMaxKey
              gen t = Generator t (t + d) g

patch :: Time -> StreamKey -> AdjustmentFn -> PlayerState StreamKey
patch d k a = state $ \(Player as t) ->
    (k, Player (M.adjust (adj t) k as) t)
        where adj t s = Adjustment t (t + d) a s

wait :: Time -> PlayerState ()
wait d = state $ \(Player as t) -> ((), Player as (t + d))

stopIfPlaying :: Time -> Stream -> Stream
stopIfPlaying t s = s {streamEnd = min (streamEnd s) t}

stopAll :: PlayerState ()
stopAll = state $ \(Player as t) -> ((), Player (M.map (stopIfPlaying t) as) t)

stop :: StreamKey -> PlayerState StreamKey
stop k = state $ \(Player as t) -> (k, Player (M.adjust (stopIfPlaying t) k as) t)

playF :: GeneratorFn -> PlayerState StreamKey
playF = play inf

patchF :: StreamKey -> AdjustmentFn -> PlayerState StreamKey
patchF = patch inf

playAndWait :: Time -> GeneratorFn -> PlayerState StreamKey
playAndWait d g = play d g >>= (\k -> wait d >> return k)

patchAndWait :: Time -> StreamKey -> AdjustmentFn -> PlayerState StreamKey
patchAndWait d k a = patch d k a >>= (\k -> wait d >> return k)

repeatM :: (Integral a) => a -> PlayerState b -> PlayerState b
repeatM x m = repeatLoop x m
    where repeatLoop x acc | x <= 1 = acc
                           | otherwise = repeatLoop (x - 1) (acc >> m)

-- Encoder

type Sample = Int64
type SampleRate = Int64
type BitsPerSample = Int64

writeWAVFile :: (Integral a) => FilePath -> SampleRate -> a -> PlayerState b -> IO ()
writeWAVFile fp sr bps ps = B.writeFile fp $ genWAVFile sr (fromIntegral $ bps `div` 8) ps

genWAVFile :: SampleRate -> BitsPerSample -> PlayerState a -> B.ByteString
genWAVFile sr bps = P.runPut . (putWAV sr bps) . (mix sr bps) . execAndGetStreams

putWAV :: SampleRate -> BitsPerSample -> (Sample, P.Put) -> P.Put
putWAV sr bps (numSamples, audioDataPut) = do
    P.putLazyByteString $ B.pack riff
    P.putWord32le . fromIntegral $ 36 + sub2Size
    P.putLazyByteString $ B.pack wave
    P.putLazyByteString $ B.pack fmt
    P.putWord32le 16          -- Subchunk Size
    P.putWord16le 1           -- Audio Format (PCM)
    P.putWord16le 2           -- Num Channels
    P.putWord32le . fromIntegral $ sr
    P.putWord32le . fromIntegral $ sr * bps * 2
    P.putWord16le . fromIntegral $ bps * 2
    P.putWord16le . fromIntegral $ bps * 8
    P.putLazyByteString $ B.pack dataS
    P.putWord32le . fromIntegral $ sub2Size
    audioDataPut
        where riff = [0x52, 0x49, 0x46, 0x46]
              wave = [0x57, 0x41, 0x56, 0x45]
              fmt = [0x66, 0x6d, 0x74, 0x20]
              dataS = [0x64, 0x61, 0x74, 0x61]
              sub2Size = numSamples * bps * 2

mix :: SampleRate -> BitsPerSample -> ([Stream], Time) -> (Sample, P.Put)
mix sr bps (as, t) = (numSamples, encodeStreams bps as 0 oneSamplePs endPs)
    where numSamples = psToSample endPs
          endPs = timeMaybe 0 id endTime
          endTime = foldr maxTime t (map generationEnd as)
          psToSample x = sr * round (x % 1000000000000)
          oneSamplePs = round (1000000000000 % sr)

encodeStreams :: BitsPerSample -> [Stream] -> Picosecond -> Picosecond -> Picosecond -> P.Put
encodeStreams bps as i j k = encodeStreamLoop i k
    where encodeStreamLoop i k | i >= k = return ()
                               | otherwise = do
                                    encodeStreams i
                                    encodeStreamLoop (i+j) k
          encodeStreams i = do
                let Point2 (l, r) = evalStreams i as
                P.putWord16le $ norm l
                P.putWord16le $ norm r
          norm x = round $ x * 2^(bps * 8 - 1)


encodeBytes :: (Int64, Int64) -> Int -> P.Put
encodeBytes (l, r) n = do
    P.putWord8 $ f l
    P.putWord8 $ f r
        where f x = fromIntegral x `shiftR` (n - 1) .&. 0xff

-- Example

sine :: Freq -> Volume -> GeneratorFn
sine f v = mkMonoGen g
    where g i = v * (sin $ 2 * pi * i * f)

sawtooth :: Freq -> Volume -> GeneratorFn
sawtooth f v = mkMonoGen g
    where g i = 2 * v * (x - fromIntegral (floor x) - 0.5)
                where x = i * f

amplify :: Volume -> AdjustmentFn
amplify v = mkMonoAdj g
    where g _ x = v * x

fadeIn :: Time -> AdjustmentFn
fadeIn Infinite = mkMonoAdj $ const . const 0
fadeIn (Finite s) = mkMonoAdj g
    where g i x = if i < t then (t - i) * x else x
          t = fromIntegral s

-- Examples

example1 = do
    play (beat 4) (sine (note "C") 0.3)
    wait (beat 1)
    play (beat 3) (sine (note "E") 0.2)
    wait (beat 1)
    play (beat 2) (sine (note "G") 0.2)
    wait (beat 3)
    where beat = bpm 80.0

example2 = mapM f ns
    where beat = bpm 140.0
          f n = playAndWait (beat 1) (sine (note n) 0.3)
          ns = ["F","G","A","Bb","C","D","E","F"
               ,"E","D","C","Bb","A","G","F","F"]

example3 = chord sine >> chord sawtooth
    where beat = bpm 80.0
          chord s = do
              play (beat 4) (s (note "C") 0.3)
              wait (beat 1)
              play (beat 3) (s (note "E") 0.2)
              wait (beat 1)
              play (beat 2) (s (note "G") 0.2)
              wait (beat 3)

example4 = do
    c <- play (beat 5) (sine (note "C") 0.3)
    patch inf c (fadeIn (beat 1))
    where beat = bpm 120.0

example5 = do
    c <- play (beat 5) (sine (note "C") 0.3)
    wait (beat 1)
    patch (beat 2) c (amplify 1.8)
    where beat = bpm 120.0

example6 = do
    c <- play inf (sine (note "F#") 0.3)
    rampVolume c
    rampVolume c
    rampVolume c
    stopAll
    where rampVolume x = do
              patch inf x (amplify 1.25)
              wait (secs 1)

main = writeWAVFile "sample.wav" 44100 16 example4
