module Stage where

import Foreign
import Foreign.C
import Numeric (showHex)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (openTempFile, hClose, hPutStr)
import System.Process (createProcess, proc, waitForProcess, ProcessHandle)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Error (isDoesNotExistError)
import Debug.Trace (putTraceMsg)
import Control.Monad (liftM)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket, bracket_, evaluate, tryJust, IOException)
import System.Posix.DynamicLinker (withDL, RTLDFlags(RTLD_NOW), dlsym)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S

-- Render floating-point constants in hexadecimal

showADouble :: Double -> String
showADouble d | isNaN d = "NAN"
showADouble d = case decodeFloat d of
                (m,n) -> case compare m 0 of
                         LT | isInfinite d     -> "(-INFINITY)"
                         LT -> "-0x" ++ showHex (-m) ("p" ++ show n)
                         EQ | isNegativeZero d -> "-0."
                         EQ                    -> "0."
                         GT | isInfinite d     -> "INFINITY"
                         GT -> "0x" ++ showHex m ("p" ++ show n)

-- Offshore to C the computation of the function whose root we want

type Fun = {- const double olds   [ln] -} Ptr Double ->
           {-       double current[lm] -} Ptr Double ->
           {-       double diff   [ln] -} Ptr Double -> IO ()
foreign import ccall "dynamic" fun__ :: FunPtr Fun -> Fun

type Doubles = S.Vector Double

data Stmt = Compute String | Invoke String

paginateCode :: String ->   -- parameters of a function "fun" returning void
                String ->   -- arguments to use when calling the function
                [String] -> -- statements that the function should execute
                [String]    -- reasonably short function definitions
paginateCode params args = go 1 [] [] . map Compute where
  go :: Int ->      -- a number with which to name the next function defined
        [String] -> -- thunks to invoke at the start of "fun" in reverse order
        [String] -> -- function definitions accumulated so far in reverse order
        [Stmt] -> [String]
  go n uses defns stmts =
    case splitAt 1000 stmts of
    (_, []) | null uses -> rev defns [define "fun" stmts]
            | otherwise -> go n [] defns (rev (map Invoke uses) stmts)
    (f, un) -> let name = "f" ++ show n
               in go (succ n) (name : uses) (define name f : defns) un
  declare name = "void " ++ name ++ "(" ++ params ++ ");"
  define name stmts = unlines $
        ["#define _ISOC99_SOURCE", "#include <math.h>"] -- for NAN and INFINITY
     ++ [ declare name' | Invoke name' <- stmts ]
     ++ ["void " ++ name ++ "(" ++ params ++ ") {"]
     ++ [ case stmt of Compute s -> s
                       Invoke name' -> name' ++ "(" ++ args ++ ");"
        | stmt <- stmts ]
     ++ ["}"]
  rev []     a = a
  rev (x:xs) a = rev xs (x:a)

withFortranFiles :: (a -> FilePath -> (a -> IO w) -> IO w) ->
                    a -> [String] -> (a -> IO w) -> IO w
withFortranFiles compile = go where
  go state [] k = k state
  go state (code:codes) k = bracket
    (getTemporaryDirectory >>= (`openTempFile` "fortran.c"))
    (\(path, handle) -> do cleanup (hClose handle)
                           cleanup (removeFile path))
    (\(path, handle) -> do hPutStr handle code
                           cleanup (hClose handle)
                           compile state path (\state -> go state codes k))

type ProcessHandle' = (FilePath, ProcessHandle)

gcc :: [String] -> FilePath -> (ProcessHandle' -> IO a) -> IO a
gcc argv' output k = do
  let argc = "gcc44"
      argv = argv' ++ ["-o", output]
  putTraceMsg (unwords (argc:argv))
  bracket (createProcess (proc argc argv))
          (\_ -> cleanup (removeFile output))
          (\(_,_,_,p) -> k (output, p))

waitForProcessToSucceed :: ProcessHandle' -> IO ()
waitForProcessToSucceed (path, processHandle) = do
  ExitSuccess <- waitForProcess processHandle
  putTraceMsg ("done " ++ path)
  return ()

withFortran :: Int -> Int -> [String] ->
               ((Doubles -> (Doubles, Doubles)) -> w) -> w
withFortran lm ln codes k | lm >= ln =
  unsafePerformIO $
  let compile (paths_o, ps) path_c k = do
        ps <- if length ps < 4 then return ps
              else waitForProcessToSucceed (last ps) >> return (init ps)
        gcc ["-fpic", "-c", path_c] (init path_c ++ "o")
            (\p@(path_o,_) -> k (path_o:paths_o, p:ps))
      link paths_o =
        gcc ("-shared" : paths_o) (init (head paths_o) ++ "so")
  in
  withFortranFiles compile ([], []) codes (\(paths_o, processHandles) -> do
  mapM_ waitForProcessToSucceed processHandles
  link paths_o (\p@(path_so,_) -> do
  waitForProcessToSucceed p
  withMVar dl_lock (\() -> do
  withDL path_so [RTLD_NOW] (\dl -> do
    fun_ptr <- dlsym dl "fun"
    let fun = fun__ fun_ptr
    evaluate (k (\olds -> unsafePerformIO $
      case S.unsafeToForeignPtr olds of
      (oldsP, oldsO, oldsS) | oldsS >= ln -> do
        currentP <- mallocForeignPtrArray lm
        diffP    <- mallocForeignPtrArray ln
        withForeignPtr oldsP      (\oldsP    ->
          withForeignPtr currentP (\currentP ->
            withForeignPtr diffP  (\diffP    -> do
              fillNaN currentP lm -- to help debugging
              fillNaN diffP    ln -- to help debugging
              fun (advancePtr oldsP oldsO) currentP diffP)))
        return (S.unsafeFromForeignPtr currentP 0 lm,
                S.unsafeFromForeignPtr diffP    0 ln)))))))

dl_lock :: MVar () -- This doesn't seem to help fix multithreading problems
dl_lock = unsafePerformIO (newMVar ())

fillNaN :: Ptr Double -> Int -> IO ()
fillNaN _ n | n <= 0 = return ()
fillNaN p n = poke p (0/0) >> fillNaN (advancePtr p 1) (n - 1)

cleanup :: IO () -> IO ()
cleanup m = tryJust (Just :: IOException -> Maybe IOException) m >> return ()

removeFileNot :: FilePath -> IO ()
removeFileNot path = return () -- for debugging
