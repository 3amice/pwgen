import System.Directory
import System.Random (randomRIO)
import Control.Applicative
 
upper  = 10
lower  = 5 
len    = 3 

pick :: Int -> [a] -> [IO a]
pick 0 xs = []
pick i xs = (pick' xs) : (pick (i-1) xs)
  where
    pick' :: [a] -> IO a
    pick' xs = randomRIO (0, length xs - 1) >>= return . (xs !!)
 
conts = do 
  fs <- getDirectoryContents "/usr/share/dict"
  return . (fmap ("/usr/share/dict/"++)) . filter (liftA2 (&&) (/=".") (/="..")) $ fs

main = do
  x <- conts >>= mapM readFile 
  let dict = filter (liftA2 (&&) ((< upper).length) ((> lower).length)) (x >>= words)
  let dictLen = fromIntegral $ length $ dict
  let entropy = (log(dictLen) / log(2)) ^ (len)
  -- ruby "join" 
  mapM_ (>>= putStr . (++" ")) ((pick len dict))
  putStrLn ""
  putStrLn $  "entropy is " ++ show entropy
