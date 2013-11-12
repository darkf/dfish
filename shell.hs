import System.Environment (getArgs)
import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import System.IO (hFlush, hGetContents, Handle, stdout, openFile, IOMode(..))
import qualified System.Process as P

data Direction = RLeft | RRight deriving (Show, Eq)
data Cmd = Cmd String [String]
		 | Pipe Cmd Cmd
		 | Redirection Direction Cmd String
		 | Empty
	 deriving (Show, Eq)

(|.) = Pipe
(<.) = Redirection RLeft
(>.) = Redirection RRight

data Proc = Proc { p_stdin :: Handle, p_stdout :: Handle, p_proc :: P.ProcessHandle }

runCmd' :: Cmd -> (P.StdStream, P.StdStream) -> IO Proc
runCmd' (Cmd cmd args) std = do
	(pin', pout', _, p) <- P.createProcess (P.proc cmd args) { P.std_in = fst std, P.std_out = snd std }
	let pin = fromMaybe (error "") pin'
	let pout = fromMaybe (error "") pout'
	let proc = Proc { p_stdin=pin, p_stdout=pout, p_proc=p }
	return proc

runCmd' (Redirection dir cmd file) std = do
	let mode = case dir of
		RLeft -> ReadMode
		RRight -> WriteMode
	file <- openFile file mode

	let std' = case dir of
		RLeft -> (P.UseHandle file, snd std)
		RRight -> (fst std, P.UseHandle file)
	proc <- runCmd' cmd std'
	return proc

runCmd' (Pipe cmd1 cmd2) std = do
	proc1 <- runCmd' cmd1 std
	proc2 <- runCmd' cmd2 (P.UseHandle (p_stdout proc1), P.CreatePipe)
	return proc2

runCmd :: Cmd -> IO ()
runCmd cmd = do
	putStrLn $ "cmd: " ++ show cmd
	proc <- runCmd' cmd (P.CreatePipe, P.CreatePipe)
	x <- hGetContents $ p_stdout proc
	i <- P.waitForProcess $ p_proc proc
	putStrLn $ "proc exited with status" ++ show i
	putStrLn $ "output: " ++ x

parseCmd' :: String -> Cmd -> Cmd
parseCmd' "|" cmd@(Cmd _ _) = Pipe cmd Empty
parseCmd' arg Empty = Cmd arg []
parseCmd' arg (Cmd prg args) = Cmd prg (args ++ [arg])
parseCmd' arg (Pipe cmd1 cmd2) = Pipe cmd1 (parseCmd' arg cmd2)

parseCmd :: String -> Cmd
parseCmd cmd =
	let tokens = words cmd
	in foldl (flip parseCmd') Empty tokens

evalCmd :: String -> IO ()
evalCmd = runCmd . parseCmd

prompt = putStr "$ " >> hFlush stdout >> getLine
main = do
	args <- getArgs
	case args of
		["-c", cmd] -> evalCmd cmd
		otherwise -> repl
	where
	repl = prompt >>= \p -> unless (p == "quit") (evalCmd p >> repl)
