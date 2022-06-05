module Ansi where
import Rgb
import Graphics.Image hiding (map)

-- https://en.wikipedia.org/wiki/ANSI_escape_code#24-bit
-- https://stackoverflow.com/questions/63662243/rgb-terminal-colors-with-haskell-and-brick
ansiBackColor :: (Word8, Word8, Word8) -> String
ansiBackColor (r,g,b) = "\ESC[48;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

ansiForeColor :: (Word8, Word8, Word8) -> String
ansiForeColor (r,g,b) = "\ESC[38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

ansiResetGraphics :: String
ansiResetGraphics = "\ESC[0m"

ansiMoveUp :: Int -> String
ansiMoveUp n = "\ESC[" ++ show n ++ "F"

-- Returns a string with ANSI escape codes matching the image pixels
ansify :: Image VS RGB Word8 -> String
ansify img = concat $ map (\row -> foldr (++) (ansiResetGraphics ++ "\n") (map ansiPixel row)) $ toLists img
 where ansiPixel p = ansiBackColor (rgb p) ++ " "
