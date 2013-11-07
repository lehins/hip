module Data.Image.Interactive where
       
import Data.Image
import Data.List(intercalate)
import Data.IORef
import System.IO.Unsafe
import System.IO

--vector>=0.10.0.2
import qualified Data.Vector.Unboxed as V

--process>=1.1.0.2
import System.Process

{-| Sets the program to use when making a call to display and specifies if
    the program can accept an image via stdin. If it cannot, then a temporary
    file will be created and passed as an argument instead. By default,
    ImageMagick ("display") is the default program to use and it is read
    using stdin.

    >>>setDisplayProgram "gimp" False
    
    >>>setDisplayProgram "xv" False

    >>>setDisplayProgram "display" True
 -}
setDisplayProgram :: String -> Bool -> IO ()
setDisplayProgram program stdin = writeIORef displayProgram program >> writeIORef useStdin stdin


{-| Makes a call to the current display program to be displayed. If the
    program cannot read from standard in, a file named ".tmp-img" is created
    and used as an argument to the program.

    >>>frog <- readImage "images/frog.pgm"
    >>>display frog

 -}
display :: (DisplayFormat df) => df -> IO (Handle, Handle, Handle, ProcessHandle)
display img = do
  usestdin <- readIORef useStdin
  display <- readIORef displayProgram
  if usestdin then runCommandWithStdIn display . format $ img
              else do
    writeImage ".tmp-img" img
    runInteractiveCommand (display ++ " .tmp-img")

displayProgram :: IORef String
displayProgram = unsafePerformIO $ do
  dp <- newIORef "display"
  return dp

useStdin :: IORef Bool
useStdin = unsafePerformIO $ do
  usestdin <- newIORef True
  return usestdin
  
-- Run a command via the shell with the input given as stdin
runCommandWithStdIn :: String -> String -> IO (Handle, Handle, Handle, ProcessHandle)
runCommandWithStdIn cmd stdin =
  do
    ioval <- runInteractiveCommand cmd
    let stdInput = (\ (x, _, _, _) -> x) ioval
    hPutStr stdInput stdin
    hFlush stdInput
    hClose stdInput
    return ioval
