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
 
files :: IO [String]
files = do 
  let dict = "/usr/share/dict/"
  fs <- getDirectoryContents dict
  return . (fmap (dict++)) . filter (liftA2 (&&) (/=".") (/="..")) $ fs

genPw :: Int -> Int -> Int -> IO (String, Double, Double)
genPw lowerWordLength upperWordLength len = do
  x <- files >>= mapM readFile 
  let dict = filter (liftA2 (&&) ((<= upperWordLength).length) ((>= lowerWordLength).length)) (x >>= words)
  let dictEntropy = log (((^) . fromIntegral . length) dict len) / log 2
  pws <- foldr1 (liftM2 (flip (++) . (' ':))) $ pick len dict
  let letterEntropy = (/(log 2)) . log . (^) 26 . fromIntegral . length $ pws
  return (map toLower $ pws, dictEntropy, letterEntropy)

main = do
  (pw, dictEntropy, letterEntropy) <- genPw 1 10 5
  putStrLn pw
  putStrLn $ "dictEntropy  : " ++ (show dictEntropy)
  putStrLn $ "letterEntropy: " ++ (show letterEntropy)
  return ()
