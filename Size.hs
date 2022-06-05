module Size where
import Graphics.Image
import System.Console.Terminal.Size


scaleToWidth :: (Int, Int) -> Int -> (Int, Int)
scaleToWidth (h,w) maxW = (h*maxW `div` w, w*maxW `div` w)

scaleToHeight :: (Int, Int) -> Int -> (Int, Int)
scaleToHeight (h,w) maxH = (h*maxH `div` h, w*maxH `div` h)

-- Scale so that (h,w) fits within (maxH, maxW)
fitWithin :: (Int, Int) -> (Int, Int) -> (Int, Int)
fitWithin (maxH, maxW) (h,w) = if w*maxH > h*maxW then scaleToWidth (h,w) maxW else scaleToHeight (h,w) maxH

-- The relative width and height of a monospace character in my current terminal. Very scientific.
fontWidth :: Int
fontWidth = 11
fontHeight :: Int
fontHeight = 24

-- Scale to compensate for font aspect ratio
scaleToFont :: (Int, Int) -> (Int, Int)
scaleToFont (h,w) = (h*fontWidth, w*fontHeight)

-- Makes the longest side size long
resizeImage :: (Int, Int) -> Image VS RGB Double -> Image VS RGB Double
resizeImage = resize Bilinear Edge 

terminalDims :: IO (Int, Int)
terminalDims = do
    maybeWindow <- size
    let dims = maybe (50,50) (\(Window h w) -> (h,w)) maybeWindow
    return dims
