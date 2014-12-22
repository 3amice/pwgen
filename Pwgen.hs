import System.Directory
import Control.Applicative
 
import System.Random (randomRIO)

pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

pick' :: Int -> [a] -> [IO a]
pick' 0 xs = []
pick' i xs = (pick xs) : (pick' (i-1) xs)
 
upper  = 20 
lower  = 5 
len    = 5

fileNames = do 
  fs <- getDirectoryContents "/usr/share/dict"
  return . filter (liftA2 (&&) (/=".") (/="..")) $ fs

-- Conts is fileName:s absolute paths.
conts :: IO [[Char]]
conts = fileNames >>= (return . fmap (("/usr/share/dict/"++))) 

main = do
  x <- conts >>= mapM readFile 
  let dict = filter (liftA2 (&&) ((< upper).length) ((> lower).length)) (x >>= words)
  putStrLn (show . length $ dict)
  -- entropy is (log(length dict)/log(2))^len
  y <- pick dict
  putStrLn y
