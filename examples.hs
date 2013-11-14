module Main where
import System.Environment
import Data.Image
import Data.Image.Gray
import Data.Image.Complex
import Data.Image.Internal
import Data.Image.IO
--import Data.Image.Convolution


getAvg imgs = foldr (+) (head imgs) (tail imgs) / l
  where l = fromIntegral $ length imgs

getPowerSpec img = realImage (fftImg * conjugateImage fftImg)
  where fftImg = fft $ toComplex img img

getAvgPower imgs = getAvg $ map getPowerSpec imgs


getLog = imageMap log

applyFilter img filt = realImage . ifft $ (fft img * toComplex filt filt)

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
  writeImage "signalsLogAvgPow.pgm" (getLog signalsAvgPow) PNG inRGB8
  writeImage "noisesLogAvgPow.pgm" (getLog noisesAvgPow) PNG inRGB8
  writeImage "wiener.pgm" wiener PNG inRGB8
  let testWNames = ["test/testW"++show x++".pgm" | x <- [1..6]]
  let testWriter (fname, testImg) =
        writeImage fname (applyFilter (toComplex testImg testImg) wiener) PNG inRGB8
  mapM_ testWriter $ zip testWNames tests
