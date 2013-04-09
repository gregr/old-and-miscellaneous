module Util where
import System.IO

--import Data.List -- unfoldr
--import Control.Monad -- guard

-- trans _ [] = []
-- trans f xs = part : trans f xs'
--     where (part, xs') = f xs
-- an obscure one-line alternative... I think I prefer the original's clarity
--trans f xs = unfoldr (\ys -> guard (not $ null ys) >> return $ f ys) xs

openApp = "("
closeApp = ")"
openList = "["
closeList = "]"

join sep ss = drop 1 $ foldr (++) "" $ map (\s -> sep++s) ss
surround left right s = left ++ s ++ right

streamHandle f h = hGetContents h >>= f

streamFile f path = withFile path ReadMode $ streamHandle f

--testing
testFile proc = streamFile proc "test.ax"
