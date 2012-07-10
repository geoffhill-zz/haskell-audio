import Data.Binary
import Data.Array

type FArray e = Array Int e

amapMi :: (Monad m, Ix i) => ((i,a) -> m (i,b)) -> (Array i a) -> m (Array i b)
amapMi f a = do
    newAssocs <- mapM f (assocs a)
    return $ array (bounds a) newAssocs

amapMi_ :: (Monad m, Ix i) => ((i,a) -> m (i,b)) -> (Array i a) -> m ()
amapMi_ f a = mapM_ f (assocs a)

amapM :: (Monad m, Ix i) => (a -> m b) -> (Array i a) -> m (Array i b)
amapM f a = amapMi g a
    where g (i, x) = do
            newVal <- f x
            return (i, newVal)

amapM_ :: (Monad m, Ix i) => (a -> m b) -> (Array i a) -> m ()
amapM_ f a = amapMi_ g a
    where g (i, x) = return (i, f x)

data Flac = Flac { firstMeta :: Meta
                 , restMeta :: FArray Meta
                 , frames :: FArray Frame
                 } deriving Show
data Meta = StreamInfo { minBlockSize :: Word16
                       , maxBlockSize :: Word16
                       , minFrameSize :: Word32
                       , maxFrameSize :: Word32
                       , defaultSampleRate :: Word32
                       , numChannels :: Word8
                       , bitsPerSample :: Word8
                       , numSamples :: Word64
                       , checksum :: FArray Word8
                       }
          | Padding {numBytes :: Int}
          | Application { appID :: Word32
                        , appData :: FArray Word8
                        }
          | SeekTable { sampleNum :: Word64
                      , offset :: Word64
                      , numFrameSamples :: Word16
                      }
          | Comment { fields :: FArray (String,String) }
          | CueSheet { catalogNumber :: String
                     , numLeadIn :: Word64
                     , compactDisk :: Bool
                     , tracks :: FArray CueSheetTrack
                     }
          | Picture { pictureType :: Word32
                    , mime :: String
                    , description :: String
                    , width :: Word32
                    , height :: Word32
                    , colorDepth :: Word32
                    , indexedColors :: Word32
                    , pictureData :: FArray Word8
                    }
          deriving Show
data CueSheetTrack = CueSheetTrack { trackOffset :: Word64
                                   , trackNum :: Word8
                                   , isrc :: String
                                   , audio :: Bool
                                   , preEmphasis :: Bool
                                   , indices :: [CueSheetTrackIndex]
                                   } deriving Show
data CueSheetTrackIndex = CueSheetTrackIndex { indexOffset :: Word64
                                             , indexNum :: Word8
                                             } deriving Show
data Frame = Frame { header :: FrameHeader
                   , subframes :: FArray Subframe
                   } deriving Show
data FrameHeader = FrameHeader { fixedBlocksize :: Bool
                               , blockSize :: Word8
                               , sampleRate :: Word8
                               , channelAssignment :: Word8
                               , sampleSize :: Word8
                               } deriving Show
data Subframe = Subframe { wastedBitsPerSample :: Int
                         , subframeData :: SubframeData
                         } deriving Show
data SubframeData = SubframeConst { constData :: Word32 }
                  | SubframeVerbatim { verbatimData :: FArray Word8 }
                  | SubframeFixed { orderPoly :: Word8
                                  , residuals :: FArray Residual
                                  }
                  | SubframeDataLPC { orderLPC :: Word32
                                    , residuals :: FArray Residual
                                    }
                  deriving Show
data Residual = Res4 { partitionOrder :: Word8
                     , partitions4 :: FArray Part4
                     }
              | Res5 { partitionOrder :: Word8
                     , partitions5 :: FArray Part5
                     } deriving Show
data Part4 = Rice4 { riceParam4 :: Word8
                   , residual4 :: FArray Word8
                   }
           | Unenc4 { residual4 :: FArray Word8 }
           deriving Show
data Part5 = Rice5 { riceParam5 :: Word8
                   , residual5 :: FArray Word8
                   }
           | Unenc5 { residual5 :: FArray Word8 }
           deriving Show

instance Binary Flac where
    put (Flac firstMeta restMeta frames) = do
        mapM_ put "fLaC"
        put firstMeta
        amapM_ put restMeta
        amapM_ put frames
    get = do
        get :: Get Word32
        return Flac

instance Binary Meta where
    put _ = return ()
    get = return $ Padding 0

instance Binary Frame where
    put _ = return ()
    get = return ()

main = do
    encodeFile "samp.flac" Flac

