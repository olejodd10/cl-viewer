module Rgb where
import Graphics.Image
import Graphics.Image.Interface (toListPx)

rgb :: Pixel RGB Word8 -> (Word8, Word8, Word8)
rgb p = let (r:g:b:_) = toListPx p in (r,g,b)
