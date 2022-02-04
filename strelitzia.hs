{-# LANGUAGE FlexibleInstances #-}

import Data.List.Extra
import Control.Conditional
import System.IO
import System.Process (readCreateProcessWithExitCode, shell)
import System.Exit
import System.Directory
import Options


-- *** THE MAIN ***

-- Roughly speaking, `main` is `streltizia` wrapped by some controls.
main :: IO ()
main = maybe (warn "no file provided!") strelitzia =<< optFileToUse
  where
    -- This function is the actual heart of the program: it takes a file,
    -- reads its lines and makes the hosting system consequentely act.
    strelitzia :: FilePath -> IO ()
    strelitzia fp = ifM (doesFileExist fp)
      (mapM_ toWorld . lines =<< readFile fp)
      (warn (fp ++ " does not exist!"))


-- *** I PART: TRANSLATING THINGS ***

-- The program performs two consecutive traslations.
--
--  > The first goes from a (simple) textual language to one *internal* at
--    the program; that language amounts of `Strelitzia`-s, and uniquely
--    serves the needs of the program. Albeit such language has a quite
--    narrow extension, it is enough.
--
--  > `Strelitzia`-s are then transformed into system commands. Beware
--    that so far this part intimately relies on your OS: the ideal "host
--    system" is GNU/Linux, where the program is designed; elsewhere the
--    program may fail or not, I do not know :)
--
-- The `Read` and `Show` instances do that.

data Strelitzia = Garbage
                | Cp FilePath FilePath
                | Mv FilePath FilePath
                | Idk String

instance Read Strelitzia where
  readsPrec _ str = [(strel, "")]
    where
      strel :: Strelitzia
      strel = case dropWhile (== ' ') str of
        "" -> Garbage
        '#':_ -> Garbage
        _ -> case words str of
          a:">":b:_ -> Cp a b
          a:"@":b:_ -> Mv a b
          _ -> Idk (trim str)

instance Show Strelitzia where
  show (Cp src dst) = "cp -ru " ++ src ++ " " ++ dst
  show (Mv src dst) = "mv "     ++ src ++ " " ++ dst
  show _ = error "you shouldn\'t show a Garbage or an Idk!"


-- *** II PART: MOVING THE HOST SYSTEM ***

-- The class below isn't so sophisticated (it has a unique function), but
-- it is general enough to encompass the main actions of the program.
class ToWorld a where
  toWorld :: a -> IO ()

-- Take a `Strelitzia` and make the host system do something.
instance ToWorld Strelitzia where
  toWorld strel = toWorld =<< case strel of
    -- There's nothing sophisticated when the argument is a `Garbage` or
    -- an `Idk`; in the latter case, the user is told what the program
    -- doesn't know how to handle.
    Garbage -> return Silence
    Idk cmd -> return (Warn (cmd ++ ": what?"))
    -- In the other cases, the system that hosts the program is involved.
    _ -> let sys = show strel in do
      -- The user is told what is happening, unless otherwise wanted.
      say ("[running] " ++ sys)
      -- If the option "--do-nothing" is given, no system command is
      -- issued; in that case, `Ok`-s are barely returned (how could
      -- something go wrong in a "preview"?); otherwise, here is the
      -- exact point where system commands are invoked.
      ifM optDoNothing (return Silence) $
          run sys >>= \(exitCode, _, errMsg) ->
            return $! if exitCode == ExitSuccess
              then Silence else Warn (trim errMsg)

-- Take a system command (a `String`) and make the host system run it.
-- Once the system has done its job, get exit-code, any stdout and stderr.
run :: String -> IO (ExitCode, String, String)
run = flip readCreateProcessWithExitCode "" . shell

-- Take a `String`, turn it into a `Strelitzia` and apply `toWorld` to it.
instance ToWorld String where
  toWorld = toWorld . (read :: String -> Strelitzia)

-- The data below, which represents how the program communicates.
data Report = Silence     -- saying nothing
            | Say String  -- saying something, via stdout
            | Warn String -- warnings, via stderr

-- Communicate with the users.
instance ToWorld Report where
  toWorld Silence = return ()
  toWorld (Say msg) =
    unlessM optStdOutBeQuiet (putStrLn msg)
  toWorld (Warn msg) = do
    unlessM optStdErrBeQuiet $
      hPutStrLn stderr ("[warning] " ++ msg)
    whenM optFatalWarnings exitFailure

-- Tell users things.
say :: String -> IO ()
say = toWorld . Say

warn :: String -> IO ()
warn = toWorld . Warn


-- *** AFFECTING THE BEHAVIOUR OF THE PROGRAM ***

-- The unique way to interact with the program is the command-line: via
-- that tool you change settings and indicate the file to read. The data
-- `StrelOpts` represent all and only the acceptable options.
data StrelOpts = StrelOpts
  {
    -- By default, the program sends to your OS the commands to run.
    -- However, you can prevent that passage of instructions by passing
    -- the option "--do-nothing".
      doNothing     :: Bool
    -- By default, the program is instructed to tell everything. If you
    -- can't be bothered by standard outputs, pass the option "--quiet".
    , stdOutBeQuiet :: Bool
    -- Every standard error output is important: for that reason, it is
    -- not recommended to disable them. Anyway, if you desire to do so,
    -- there is the option "--no-errs".
    , stdErrBeQuiet :: Bool
    -- The program has no thing such as *fatal* errors nor warnings. The
    -- program tries to do something and in ase something goes awry, you
    -- are informed and goes ahead. If you want the program to abort its
    -- execution at the first stderr message, use the option "--fatal".
    , fatalWarnings :: Bool
    -- Indicate to the program what file to parse:
    --  $ strelitzia --use THIS_FILE
    -- You can use virtually any file on your OS, and this may obviously
    -- lead to troubles.
    , fileToUse     :: Maybe FilePath
  }

instance Options StrelOpts where
  defineOptions = pure StrelOpts
    <*> simpleOption "do-nothing" False   doNothingDoc
    <*> simpleOption "quiet"      False   stdOutBeQuietDoc
    <*> simpleOption "no-errs"    False   stdErrBeQuietDoc
    <*> simpleOption "fatal"      False   fatalWarningsDoc
    <*> simpleOption "use"        Nothing fileToUseDoc

-- Documentation for the options.
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

-- Introducing some CAF-s...
option :: Options o => (o -> a) -> IO a
option f = runCommand (\opts _ -> return (f opts))

optDoNothing :: IO Bool
optDoNothing = option doNothing

optStdOutBeQuiet :: IO Bool
optStdOutBeQuiet = option stdOutBeQuiet

optStdErrBeQuiet :: IO Bool
optStdErrBeQuiet = option stdErrBeQuiet

optFatalWarnings :: IO Bool
optFatalWarnings = option fatalWarnings

optFileToUse :: IO (Maybe FilePath)
optFileToUse = option fileToUse

