import System.Console.GetOpt
import System.Environment
import Data.Maybe ( fromMaybe )
import Text.Read
import Utils 

data Flag 
  = MinLength (Maybe Int) | MaxLength (Maybe Int) | NrWords (Maybe Int)
    deriving Show

options :: [OptDescr Flag]
options =
  [ Option []     ["minlength"]  (ReqArg (MinLength . readMaybe)  "integer") "minimum length of words from dictionary (default 5)"
  , Option []     ["maxlength"]  (ReqArg (MaxLength . readMaybe)  "integer") "maximum length of words from dictionary (default 20)"
  , Option ['l']     ["words"]      (ReqArg (NrWords . readMaybe)  "integer") "number of words to use (default 4)"
  ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
  case getOpt Permute options argv of
     (o,n,[])   -> return (o,n)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
 where header = "Usage: Pwgen [OPTION...] files..."

injectOpts :: [Flag] -> IO (String, Double, Double)
injectOpts flags = do
  let maxLength = findMaxLength flags
  let minLength = findMinLength flags
  let nrWords   = findWords flags
  genPw minLength maxLength nrWords
    where
      findMinLength :: [Flag] -> Int 
      findMinLength []           = 5 
      findMinLength ((MinLength (Just a)): _) = a
      findMinLength (_:xs) = findMinLength xs

      findMaxLength :: [Flag] -> Int 
      findMaxLength []           = 20
      findMaxLength ((MaxLength (Just a)): _) = a
      findMaxLength (_:xs) = findMaxLength xs

      findWords     :: [Flag] -> Int 
      findWords     []           = 4 
      findWords     ((NrWords (Just a)): _) = a
      findWords     (_:xs) = findWords xs


main = do
  args <- getArgs
  (flags, _) <- compilerOpts args
  result <- injectOpts flags
  putStrLn $ show $ result 
