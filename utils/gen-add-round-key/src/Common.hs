module Common
    ( AesState(..)
    , xorAesStates

    , Address(..)
    , Key
    , toKey
    , getKeyRoundState
    , Iv
    , getIv
    , toIv
    , Round(..)
    ) where

import           Data.Bits       (shiftL, xor, (.|.))
import           Data.ByteString (ByteString)
import           Data.Word       (Word32, Word64)

import qualified Data.ByteString as BS

data AesState = AesState !Word32 !Word32 !Word32 !Word32

xorAesStates :: AesState -> AesState -> AesState
xorAesStates (AesState w11 w12 w13 w14) (AesState w21 w22 w23 w24) =
    AesState (w11 `xor` w21) (w12 `xor` w22) (w13 `xor` w23) (w14 `xor` w24)

newtype Address = Address (Word64) deriving newtype Read

newtype Key = Key (ByteString)

toKey :: ByteString -> Maybe Key
toKey key
  | BS.length key == 240 = Just (Key key)
  | otherwise            = Nothing

getKeyRoundState :: Key -> Round -> AesState
getKeyRoundState (Key key) (Round rnd) = AesState (getWord32 0)
                                                  (getWord32 1)
                                                  (getWord32 2)
                                                  (getWord32 3)
  where getWord32 :: Int -> Word32
        getWord32 j = (b4 `shiftL` 24) .|. (b3 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b1
          where b1 = fromIntegral $ BS.index key w32Idx
                b2 = fromIntegral $ BS.index key (w32Idx + 1)
                b3 = fromIntegral $ BS.index key (w32Idx + 2)
                b4 = fromIntegral $ BS.index key (w32Idx + 3)
                w32Idx = rndIdx + j * 4

        rndIdx :: Int
        rndIdx = fromIntegral rnd * 16

newtype Iv = Iv (AesState)

getIv :: Iv -> AesState
getIv (Iv iv) = iv

toIv :: ByteString -> Maybe Iv
toIv iv
  | BS.length iv == 16 = Just (Iv aesState)
  | otherwise          = Nothing
  where aesState = AesState (getWord32 0) (getWord32 1) (getWord32 2) (getWord32 3)
        getWord32 :: Int -> Word32
        getWord32 j = (b4 `shiftL` 24) .|. (b3 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b1
          where b1 = fromIntegral $ BS.index iv w32Idx
                b2 = fromIntegral $ BS.index iv (w32Idx + 1)
                b3 = fromIntegral $ BS.index iv (w32Idx + 2)
                b4 = fromIntegral $ BS.index iv (w32Idx + 3)
                w32Idx = j * 4

newtype Round = Round Word32 deriving newtype (Eq, Ord, Enum)
