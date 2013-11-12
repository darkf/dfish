import System.Environment (getArgs)
import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import System.IO (hFlush, hGetContents, Handle, stdout)
import qualified System.Process as P

data Direction = RLeft | RRight deriving (Show, Eq)
data Cmd = Cmd String [String]
		 | Pipe Cmd Cmd
		 | Redirection Direction Cmd String
	 deriving (Show, Eq)

(|.) = Pipe
(<.) = Redirection RLeft
(>.) = Redirection RRight

data Proc = Proc { p_stdin :: Handle, p_stdout :: Handle, p_proc :: P.ProcessHandle }

runCmd' :: Cmd -> IO Proc
runCmd' (Cmd cmd args) = do
	(pin', pout', _, p) <- P.createProcess (P.proc cmd args) { P.std_in = P.CreatePipe, P.std_out = P.CreatePipe }
	let pin = fromMaybe (error "") pin'
	let pout = fromMaybe (error "") pout'
	let proc = Proc { p_stdin=pin, p_stdout=pout, p_proc=p }
	return proc
	

runCmd :: Cmd -> IO ()
runCmd cmd = do
	putStrLn $ "cmd: " ++ show cmd
	proc <- runCmd' cmd
	x <- hGetContents $ p_stdout proc
	i <- P.waitForProcess $ p_proc proc
	putStrLn $ "proc exited with status" ++ show i
	putStrLn $ "output: " ++ x

main = runCmd $ Cmd "echo" ["foo"] -- Cmd "echo" ["foo"] |.  Cmd "cat" ["-", "bar.txt"] 
{-
prompt = putStr "$ " >> hFlush stdout >> getLine
evalCmd cmd = undefined
main = do
	args <- getArgs
	case args of
		["-c", cmd] -> evalCmd cmd
		otherwise -> repl
	where
	repl = prompt >>= \p -> unless (p == "quit") (evalCmd p >> repl)
-}
