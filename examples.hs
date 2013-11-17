module Main where
import System.Environment
import Data.Image
import Data.Image.Gray
import Data.Image.Complex
import Data.Image.Complex.Algorithms
import Data.Image.Internal hiding (map)
import qualified Data.Image.Internal as I
import Data.Image.IO
import Data.Image.Interactive
import Data.Image.Processing


getAvg imgs = sum imgs / l
  where l = fromIntegral $ length imgs

getPowerSpec img = realImage (fftImg * conjImage fftImg)
  where fftImg = fft $ toComplex img

getAvgPower imgs = getAvg $ map getPowerSpec imgs


getLog = I.map log

applyFilter img filt = realImage (ifft ((fft . toComplex $ img) * (toComplex filt)))

main = do
  let signalNames = ["signal/signal"++show x++".pgm" | x <- [1..24]]
  let noiseNames = ["noise/noise"++show x++".pgm" | x <- [1..22]]
  let testNames = ["test/test"++show x++".pgm" | x <- [1..6]]
  signals <- mapM readGrayImage signalNames
  noises <- mapM readGrayImage noiseNames
  tests <- mapM readGrayImage testNames
  let signalsAvgPow = getAvgPower signals
  let noisesAvgPow = getAvgPower noises
  let wiener = (signalsAvgPow / (signalsAvgPow + noisesAvgPow))
  writeImage "signalsLogAvgPow.png" (getLog signalsAvgPow) []
  writeImage "noisesLogAvgPow.png" (getLog noisesAvgPow) []
  writeImage "wiener.png" wiener []
  let testWNames = ["test/testW"++show x++".png" | x <- [1..6]]
  let testWriter (fname, testImg) =
        writeImage fname (applyFilter testImg wiener) []
  mapM_ testWriter $ zip testWNames tests
  --im <- readColorImage "msc.jpg"
  --let iim = fft $ toComplex im
  --display im
  --display $ realImage iim
  --display $ realImage $ ifft iim
  {-display im
  display (im * 2)
  display $ normalize (im * 2)
  -}
  --let iim =  fft $ toComplex im 
  --display $ realImage $ ifft iim 
  --display $ realImage iim
  return ()
