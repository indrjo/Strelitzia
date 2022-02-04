
import Prelude hiding (error)
import Data.List.Extra
import Control.Conditional
import System.IO
import System.Environment
import System.FilePath.Posix
import System.Directory
import System.Process
import System.Exit

-- A unique command-line option is expected, that is the file to parse. If
-- none is provide, nothing is done.
main :: IO ()
main = getArgs >>=
  \args -> case args of
    this:_ ->
      ifM (doesFileExist this)
        (ulysses this)
        (fatal "no such file.")
    _ -> fatal "you must provide a file."

ulysses :: FilePath -> IO ()
ulysses fp =
    withFile fp ReadMode $ hLoop isGarbage (`cp` (takeDirectory fp))
  where
    -- Take a predicate and file handle as argument. From the handle get a
    -- line if some of them satisifes the given property, then apply a
    -- function to it, otherwise go ahead with the followng line.
    hLoop :: (String -> Bool) -> (String -> IO ()) -> Handle -> IO ()
    hLoop p f hdl =
      unlessM (hIsEOF hdl) $
        hGetLine hdl >>= \line -> do
          unless (p line) $ (f . trim) line
          hLoop p f hdl
    -- Decide whether a line is to be taken into account. There is a
    -- trivial criterion: if a string is made of ' ' or has as first
    -- non ' ' char is '#' then is considered *garbage*, otherwise the
    -- program has to parse it.
    isGarbage :: String -> Bool
    isGarbage str = case dropWhile (== ' ') str of
        { '#':_ -> True; "" -> True; _ -> False }

-- A `readCreateProcessWithExitCode` that says what command is going to
-- issue to the host system before.
rcpwec :: String -> IO (ExitCode, String, String)
rcpwec cmd = do
  putStrLn $ "[running] " ++ cmd ++ "..."
  readCreateProcessWithExitCode (shell cmd) ""

exec :: String -> IO ()
exec cmd = rcpwec cmd >>=
  \(exitCode, outMsg, errMsg) ->
    unless (exitCode == ExitSuccess) $ do
      unless (null outMsg) (warn outMsg)
      unless (null errMsg) (error errMsg)

cp :: FilePath -> FilePath -> IO ()
cp src dst = exec $ "cp -ru " ++ src ++ " " ++ dst

-- Warnings and errors from the host system. These kind of messages does
-- not make the program terminate its execution.
warn, error :: String -> IO ()
warn = hPutStr stderr . ("[warning] " ++)
error = hPutStr stderr . ("[error] " ++)

-- Fatal errors. These make the program immediately stop.
fatal :: String -> IO a
fatal = die . ("[fatal] " ++)

