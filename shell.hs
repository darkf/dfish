import System.Environment (getArgs)
import Control.Applicative ((<$>))
import Control.Monad (unless)
import System.IO (hFlush, stdout)
import qualified System.Process as P

data Direction = RLeft | RRight
data Cmd = Cmd String [String]
		 | Pipe Cmd Cmd
		 | Redirection Direction Cmd String

(|.) = Pipe
(<.) = Redirection RLeft
(>.) = Redirection RRight

runCmd cmd = undefined

prompt = putStr "$ " >> hFlush stdout >> getLine
main = do
	args <- getArgs
	case args of
		["-c", cmd] -> runCmd cmd
		otherwise -> repl
	where
	repl = prompt >>= \p -> unless (p == "quit") (runCmd p >> repl)
