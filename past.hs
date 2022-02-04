
import Data.List.Extra
import Control.Conditional
import System.IO
import System.Process hiding (runCommand)
import System.Exit
import System.Directory
import Options

-- *** THE MAIN ***

-- Bring the file to parse from the `strelitzia`. But, before applying
-- that function, the program does some controls: if some file is actually
-- given and, consequently, its existence.
main :: IO ()
main = option fileToUse >>=
  maybe (warn "no file provided!")
    (\fp -> ifM (doesFileExist fp)
      (strelitzia fp) (warn "non existent file!"))

-- *** THE FUNCTIONS THAT MOVE THE PROGRAM ***

-- This is the core function of this program: it takes a file and parses
-- its lines, one by one.
strelitzia :: FilePath -> IO ()
strelitzia fp = mapM_ strelitzia' . lines =<< readFile fp
  where
    strelitzia' :: String -> IO ()
    strelitzia' ln =
      exec (read ln) >>=
        \exit -> case exit of
          Error errMsg -> warn errMsg
          _ -> return ()

-- This is the link provided between StrelitziaLine and Exit.
exec :: StrelitziaLine -> IO Exit
exec strel = case strel of
  -- There's nothing sophisticated when the argument is a `Garbage` or an
  -- `Idk`; in the latter case, the user is told what the program doesn't
  -- know how to handle.
  Garbage -> return Ok
  Idk cmd -> return (Error $ cmd ++ ": what?")
  -- In the other cases, the system that hosts the program is involved.
  _ -> let sys = show strel in do
    -- The user is told what is happening, unless otherwise wanted. (See
    -- the definition of the function `out`.)
    out ("[running] " ++ sys)
    -- If the option "--do-nothing" is given, no system command is issued;
    -- in that case, `Ok`-s are barely returned (how could something go
    -- wrong in a "preview"?); otherwise, here is the exact point where
    -- system commands are invoked.
    ifM (option doNothing) (return Ok) (fmap toExit (run sys))

-- Take a system command (a string) and make the host system run it; once
-- the system has done its job, get exit-code, any stdout and stderr.
run :: String -> IO (ExitCode, String, String)
run = flip readCreateProcessWithExitCode "" . shell

-- The function below takes the result of the previous function, and turn
-- it into an `Exit`. Notice that stdout is actually neglected.
toExit :: (ExitCode, String, String) -> Exit
toExit (exitCode, _, errMsg) =
  if exitCode == ExitSuccess then Ok else Error (trim errMsg)

-- *** TYPES ***

-- The lines (that is, `String`-s) of a file brought to the program are
-- turned into `StrelitziaLine`-s. This data indicates to the program the
-- way the corresponding line in the file shall be parsed. There exist
-- very few kinds of acceptable lines and, consequently, a small amount
-- of actions the program can perform.
data StrelitziaLine =
  -- Comments (lines whose non-' ' character is '#') and blank lines (the
  -- ones that solely consists of ' '-s) are neglected.
    Garbage
  -- The following data constructors indicates actions the program can do:
  -- so far, we have the "cp -ru" and "mv" ones.
  | Cp FilePath FilePath
  | Mv FilePath FilePath
  -- `Idk` is for things the program does not know how to parse and handle
  -- (this is because the program is not enough instructed to understand
  -- more things than it actually does).
  | Idk String

-- Take some text and turn it into a `StrelitziaLine`.
instance Read StrelitziaLine where
  readsPrec _ str = [(strelLn, "")]
    where
      strelLn :: StrelitziaLine
      strelLn = case dropWhile (== ' ') str of
        "" -> Garbage
        '#':_ -> Garbage
        _ -> case words str of
          a:">":b:_ -> Cp a b
          a:"@":b:_ -> Mv a b
          _ -> Idk (trim str)

-- Convert `StrelitziaLine`-s into system commands (`String`-s).
instance Show StrelitziaLine where
  show (Cp fp1 fp2) = "cp -ru " ++ fp1 ++ " " ++ fp2
  show (Mv fp1 fp2) = "mv " ++ fp1 ++ " " ++ fp2
  show _ = error "you shouldn\'t show a Idk or a CommentOrBlankLine!"

-- The data below embodies the *success* or the *failure* of execution of
-- system commands issued by the program. It doesn't really matter which
-- exit-code is thrown out, so this feature is hidden; instead, having
-- error messages is far more important.
data Exit = Ok | Error String

-- *** COMMAND-LINE OPTIONS ***

option :: Options o => (o -> a) -> IO a
option f = runCommand (\opts _ -> return (f opts))

-- The options that affect the work of the program.
data StrelOpts = StrelOpts
  {
    -- Tell the program to do nothing.
      doNothing     :: Bool
    -- Silence the standard output.
    , stdOutBeQuiet :: Bool
    -- Silence the standard error output.
    , stdErrBeQuiet :: Bool
    -- Make warnings fatal.
    , fatalWarnings :: Bool
    -- Indicate to the program what file to parse.
    , fileToUse     :: Maybe FilePath
  }

instance Options StrelOpts where
  defineOptions = pure StrelOpts
    <*> simpleOption "do-nothing"     False   doNothingDoc
    <*> simpleOption "quiet"          False   stdOutBeQuietDoc
    <*> simpleOption "stderr-quiet"   False   stdErrBeQuietDoc
    <*> simpleOption "fatal-warnings" False   fatalWarningsDoc
    <*> simpleOption "use"            Nothing fileToUseDoc

doNothingDoc :: String
doNothingDoc = "The program will only show what is going to do."

stdOutBeQuietDoc :: String
stdOutBeQuietDoc = "stdout messages are silenced."

stdErrBeQuietDoc :: String
stdErrBeQuietDoc = "stderr messages are silenced."

fatalWarningsDoc :: String
fatalWarningsDoc = "Make warnings fatal."

fileToUseDoc :: String
fileToUseDoc = "Indicate to strelitzia what file to read."

-- *** COMMUNICATING WITH THE WORLD OUTSIDE ***

out :: String -> IO ()
out = unlessM (option stdOutBeQuiet) . putStrLn

warn :: String -> IO ()
warn msg = do
    unlessM (option stdErrBeQuiet) $
      hPutStrLn stderr ("[warning] " ++ msg)
    whenM (option fatalWarnings) exitFailure
