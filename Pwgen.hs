import System.Directory
import System.Random (randomRIO)
import Control.Applicative
import Control.Monad
import Data.Char
 
upper  = 20
lower  = 5 
len    = 5 

pick :: Int -> [a] -> [IO a]
pick 0 xs = []
pick i xs = (pick' xs) : (pick (i-1) xs)
  where
    pick' :: [a] -> IO a
    pick' xs = randomRIO (0, length xs - 1) >>= return . (xs !!)
 
files = do 
  let dict = "/usr/share/dict/"
  fs <- getDirectoryContents dict
  return . (fmap (dict++)) . filter (liftA2 (&&) (/=".") (/="..")) $ fs


main = do
  x <- files >>= mapM readFile 
  let dict = filter (liftA2 (&&) ((< upper).length) ((> lower).length)) (x >>= words)
  let dictEntropy = log((fromIntegral $ length $ dict)^len) / log(2)
  pws <- foldr1 (liftM2 (flip (++) . (' ':))) $ pick len dict
  let letterEntropy = log(26^(fromIntegral $ length $ pws)) / log(2)
  putStrLn . map toLower $ pws
  putStrLn $ "[/usr/share/dict] entropy : " ++ show dictEntropy
  putStrLn $ "[a-z] alphabet-entropy    : " ++ show letterEntropy 
