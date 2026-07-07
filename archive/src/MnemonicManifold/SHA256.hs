{-# LANGUAGE BangPatterns #-}

module MnemonicManifold.SHA256
  ( sha256
  , sha256U64BE
  ) where

import Data.Bits
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL

sha256 :: BS.ByteString -> BS.ByteString
sha256 msg = BL.toStrict (BB.toLazyByteString (BB.byteString (digestBytes (go (initH) (pad msg)))))
  where
    go :: (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32) -> BS.ByteString -> (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32)
    go !h bs
      | BS.null bs = h
      | otherwise =
          let (chunk, rest) = BS.splitAt 64 bs
              w = schedule chunk
              h' = compress h w
          in go h' rest

digestBytes :: (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32) -> BS.ByteString
digestBytes (a,b,c,d,e,f,g,h) =
  BL.toStrict $ BB.toLazyByteString $
    BB.word32BE a <> BB.word32BE b <> BB.word32BE c <> BB.word32BE d <>
    BB.word32BE e <> BB.word32BE f <> BB.word32BE g <> BB.word32BE h

sha256U64BE :: BS.ByteString -> Word64
sha256U64BE bs =
  let d = sha256 bs
      b0 = fromIntegral (BS.index d 0) :: Word64
      b1 = fromIntegral (BS.index d 1) :: Word64
      b2 = fromIntegral (BS.index d 2) :: Word64
      b3 = fromIntegral (BS.index d 3) :: Word64
      b4 = fromIntegral (BS.index d 4) :: Word64
      b5 = fromIntegral (BS.index d 5) :: Word64
      b6 = fromIntegral (BS.index d 6) :: Word64
      b7 = fromIntegral (BS.index d 7) :: Word64
  in  shiftL b0 56
   .|. shiftL b1 48
   .|. shiftL b2 40
   .|. shiftL b3 32
   .|. shiftL b4 24
   .|. shiftL b5 16
   .|. shiftL b6 8
   .|. b7

pad :: BS.ByteString -> BS.ByteString
pad m =
  let mlBits :: Word64
      mlBits = fromIntegral (BS.length m) * 8
      -- append 0x80, then 0x00 until length ≡ 56 (mod 64), then 64-bit length
      withOne = m <> BS.pack [0x80]
      k = (56 - (BS.length withOne `mod` 64)) `mod` 64
      zeros = BS.replicate k 0x00
      lenBytes = BL.toStrict (BB.toLazyByteString (BB.word64BE mlBits))
  in withOne <> zeros <> lenBytes

schedule :: BS.ByteString -> [Word32]
schedule chunk =
  let w0_15 = [word32At (i*4) | i <- [0..15]]
      word32At off =
        let b0 = fromIntegral (BS.index chunk off) :: Word32
            b1 = fromIntegral (BS.index chunk (off+1)) :: Word32
            b2 = fromIntegral (BS.index chunk (off+2)) :: Word32
            b3 = fromIntegral (BS.index chunk (off+3)) :: Word32
        in  shiftL b0 24 .|. shiftL b1 16 .|. shiftL b2 8 .|. b3
      extend ws i =
        let s0 = smallSigma0 (ws !! (i-15))
            s1 = smallSigma1 (ws !! (i-2))
            w' = ws !! (i-16) + s0 + ws !! (i-7) + s1
        in ws ++ [w']
  in foldl extend w0_15 [16..63]

compress
  :: (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32)
  -> [Word32]
  -> (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32)
compress (h0,h1,h2,h3,h4,h5,h6,h7) w =
  let (a0,b0,c0,d0,e0,f0,g0,h0') = (h0,h1,h2,h3,h4,h5,h6,h7)
      step (!a,!b,!c,!d,!e,!f,!g,!h) (wi,ki) =
        let t1 = h + bigSigma1 e + ch e f g + ki + wi
            t2 = bigSigma0 a + maj a b c
        in (t1 + t2, a, b, c, d + t1, e, f, g)
      (a1,b1,c1,d1,e1,f1,g1,h1') = foldl step (a0,b0,c0,d0,e0,f0,g0,h0') (zip w k)
  in (h0 + a1, h1 + b1, h2 + c1, h3 + d1, h4 + e1, h5 + f1, h6 + g1, h7 + h1')

rotr :: Word32 -> Int -> Word32
rotr x n = rotateR x n

shr :: Word32 -> Int -> Word32
shr x n = shiftR x n

ch :: Word32 -> Word32 -> Word32 -> Word32
ch x y z = (x .&. y) `xor` (complement x .&. z)

maj :: Word32 -> Word32 -> Word32 -> Word32
maj x y z = (x .&. y) `xor` (x .&. z) `xor` (y .&. z)

bigSigma0, bigSigma1, smallSigma0, smallSigma1 :: Word32 -> Word32
bigSigma0 x = rotr x 2 `xor` rotr x 13 `xor` rotr x 22
bigSigma1 x = rotr x 6 `xor` rotr x 11 `xor` rotr x 25
smallSigma0 x = rotr x 7 `xor` rotr x 18 `xor` shr x 3
smallSigma1 x = rotr x 17 `xor` rotr x 19 `xor` shr x 10

initH :: (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32)
initH =
  ( 0x6a09e667
  , 0xbb67ae85
  , 0x3c6ef372
  , 0xa54ff53a
  , 0x510e527f
  , 0x9b05688c
  , 0x1f83d9ab
  , 0x5be0cd19
  )

k :: [Word32]
k =
  [ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5
  , 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
  , 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3
  , 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
  , 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc
  , 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
  , 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7
  , 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
  , 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13
  , 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
  , 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3
  , 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
  , 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5
  , 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
  , 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208
  , 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
  ]

