module Main where
import Ansi
import Size
import System.Environment
import Data.Time.Clock
import Control.Concurrent
import Graphics.Image hiding (map)
import Graphics.Image.ColorSpace (toWord8I)

printImage :: FilePath -> (Int, Int) -> IO ()
printImage path maxDims = do
    img <- readImage' path :: IO (Image VS RGB Double) -- Only double will work: https://stackoverflow.com/questions/56570816/how-to-generate-an-image-using-haskell-image-processing
    let newDims = fitWithin maxDims $ scaleToFont $ dims img -- FIX: If image is very tall, it will not be terminalW wide!
    putStr $ ansify $ toWord8I $ resizeImage newDims img

-- https://hackage.haskell.org/package/hip-1.5.6.0/docs/Graphics-Image-IO.html#g:6
displayGif :: FilePath -> (Int, Int) -> IO ()
displayGif path (maxH, maxW) = do
    rgbaFrames <- readImageExact' (Seq GIF) path :: IO [(GifDelay, Image VS RGBA Word8)] -- JuicyPixel panics on RGB8, so RGBA8 is needed
    let imgDims = dims $ (snd . head) rgbaFrames
    let newDims = fitWithin (maxH-1, maxW) $ scaleToFont imgDims -- Reason for maxH-1: ansiMoveUp can only move up a maximum of fst termDims
    let ansiFrames = map (\(delay, img) -> (delay, ansify . toWord8I . (resizeImage newDims) . toImageRGB $ img)) rgbaFrames -- toImageRGB changes precision to Double, so we need to change it back again after resizing
    mapM_ (\(ansiFrame) -> (displayFrame ansiFrame >> (putStr $ ansiMoveUp $ fst newDims))) $ cycle ansiFrames

-- Display frame for delay*10 milliseconds
displayFrame :: (GifDelay, String) -> IO ()
displayFrame (delay, frame) = do
    start <- getCurrentTime
    putStr frame
    end <- getCurrentTime
    let elapsedMicros = round $ (realToFrac $ diffUTCTime end start) * 1000000
    let sleepTime = max 0 $ delay*10000 - elapsedMicros
    threadDelay sleepTime

main :: IO ()
main = do
    path <- fmap head getArgs
    termDims <- terminalDims
    let extension = '.' : (reverse $ takeWhile (/='.') $ reverse path)
    let displayMethod = if isFormat extension GIF then displayGif else printImage
    displayMethod path termDims