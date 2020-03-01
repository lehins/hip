{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- |
-- Module      : Graphics.Image.IO.Formats.JuicyPixels
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.IO.Formats.JuicyPixels
  ( -- * JuicyPixels formats
    SaveOption(..)
    -- ** BMP
  , BMP(..)
    -- ** GIF
  , GIF(..)
  , GIFA(..)
  , JP.GifDelay
  , JP.GifLooping(..)
  , JP.PaletteOptions(..)
  , JP.PaletteCreationMethod(..)
  -- ** HDR
  , HDR(..)
  -- ** JPG
  , JPG(..)
  -- ** PNG
  , PNG(..)
  -- ** TGA
  , TGA(..)
  -- ** TIF
  , TIF(..)
  -- * JuciyPixels conversion
  -- ** To JuicyPixels
  -- O(1) Conversion to JuicyPixels images
  , toJPImageY8
  , toJPImageYA8
  , toJPImageY16
  , toJPImageYA16
  , toJPImageYF
  , toJPImageRGB8
  , toJPImageRGBA8
  , toJPImageRGB16
  , toJPImageRGBA16
  , toJPImageRGBF
  , toJPImageYCbCr8
  , toJPImageCMYK8
  , toJPImageCMYK16
  -- ** From JuicyPixels
  -- O(1) Conversion from JuicyPixels images
  , fromJPImageY8
  , fromJPImageYA8
  , fromJPImageY16
  , fromJPImageYA16
  , fromJPImageYF
  , fromJPImageRGB8
  , fromJPImageRGBA8
  , fromJPImageRGB16
  , fromJPImageRGBA16
  , fromJPImageRGBF
  , fromJPImageYCbCr8
  , fromJPImageCMYK8
  , fromJPImageCMYK16
  ) where

import           Prelude                         as P

import qualified Codec.Picture                   as JP
import qualified Codec.Picture.ColorQuant        as JP
import qualified Codec.Picture.Gif               as JP
import qualified Codec.Picture.Jpg               as JP
import           Control.Monad                   ((<=<))
import qualified Data.ByteString                 as B (ByteString)
import qualified Data.ByteString.Lazy            as BL (ByteString)
import qualified Data.Monoid                     as M (mempty)
import qualified Data.Vector.Storable            as V
import           Graphics.Image.ColorSpace
import           Graphics.Image.Interface        as I hiding (map)
import           Graphics.Image.Interface.Vector (VS)
import           Graphics.Image.IO.Base




-- Encoding

imageToJPImageUnsafe
  :: (JP.Pixel a, Array VS cs (JP.PixelBaseComponent a))
  => Image VS cs (JP.PixelBaseComponent a)
  -> JP.Image a
imageToJPImageUnsafe img = JP.Image n m $ V.unsafeCast $ toVector img where
  (m, n) = dims img
{-# INLINE imageToJPImageUnsafe #-}

toJPImageY8 :: Image VS Y Word8 -> JP.Image JP.Pixel8
toJPImageY8 = imageToJPImageUnsafe
{-# INLINE toJPImageY8 #-}

toJPImageY16 :: Image VS Y Word16 -> JP.Image JP.Pixel16
toJPImageY16 = imageToJPImageUnsafe
{-# INLINE toJPImageY16 #-}

toJPImageYA8 :: Image VS YA Word8 -> JP.Image JP.PixelYA8
toJPImageYA8 = imageToJPImageUnsafe
{-# INLINE toJPImageYA8 #-}

toJPImageYA16 :: Image VS YA Word16 -> JP.Image JP.PixelYA16
toJPImageYA16 = imageToJPImageUnsafe
{-# INLINE toJPImageYA16 #-}

toJPImageYF :: Image VS Y Float -> JP.Image JP.PixelF
toJPImageYF = imageToJPImageUnsafe
{-# INLINE toJPImageYF #-}

toJPImageRGB8 :: Image VS RGB Word8 -> JP.Image JP.PixelRGB8
toJPImageRGB8 = imageToJPImageUnsafe
{-# INLINE toJPImageRGB8 #-}

toJPImageRGBA8 :: Image VS RGBA Word8 -> JP.Image JP.PixelRGBA8
toJPImageRGBA8 = imageToJPImageUnsafe
{-# INLINE toJPImageRGBA8 #-}

toJPImageRGB16 :: Image VS RGB Word16 -> JP.Image JP.PixelRGB16
toJPImageRGB16 = imageToJPImageUnsafe
{-# INLINE toJPImageRGB16 #-}

toJPImageRGBA16 :: Image VS RGBA Word16 -> JP.Image JP.PixelRGBA16
toJPImageRGBA16 = imageToJPImageUnsafe
{-# INLINE toJPImageRGBA16 #-}

toJPImageRGBF :: Image VS RGB Float -> JP.Image JP.PixelRGBF
toJPImageRGBF = imageToJPImageUnsafe
{-# INLINE toJPImageRGBF #-}

toJPImageYCbCr8 :: Image VS YCbCr Word8 -> JP.Image JP.PixelYCbCr8
toJPImageYCbCr8 = imageToJPImageUnsafe
{-# INLINE toJPImageYCbCr8 #-}

toJPImageCMYK8 :: Image VS CMYK Word8 -> JP.Image JP.PixelCMYK8
toJPImageCMYK8 = imageToJPImageUnsafe
{-# INLINE toJPImageCMYK8 #-}

toJPImageCMYK16 :: Image VS CMYK Word16 -> JP.Image JP.PixelCMYK16
toJPImageCMYK16 = imageToJPImageUnsafe
{-# INLINE toJPImageCMYK16 #-}




-- General decoding and helper functions


jpImageToImageUnsafe :: (Array VS cs e, JP.Pixel jpx) =>
                        JP.Image jpx -> Image VS cs e
jpImageToImageUnsafe (JP.Image n m !v) = fromVector (m, n) $ V.unsafeCast v
{-# INLINE jpImageToImageUnsafe #-}


fromJPImageY8 :: JP.Image JP.Pixel8 -> Image VS Y Word8
fromJPImageY8 = jpImageToImageUnsafe
{-# INLINE fromJPImageY8 #-}

fromJPImageY16 :: JP.Image JP.Pixel16 -> Image VS Y Word16
fromJPImageY16 = jpImageToImageUnsafe
{-# INLINE fromJPImageY16 #-}

fromJPImageY32 :: JP.Image JP.Pixel32 -> Image VS Y Word32
fromJPImageY32 = jpImageToImageUnsafe
{-# INLINE fromJPImageY32 #-}

fromJPImageYA8 :: JP.Image JP.PixelYA8 -> Image VS YA Word8
fromJPImageYA8 = jpImageToImageUnsafe
{-# INLINE fromJPImageYA8 #-}

fromJPImageYA16 :: JP.Image JP.PixelYA16 -> Image VS YA Word16
fromJPImageYA16 = jpImageToImageUnsafe
{-# INLINE fromJPImageYA16 #-}

fromJPImageRGB8 :: JP.Image JP.PixelRGB8 -> Image VS RGB Word8
fromJPImageRGB8 = jpImageToImageUnsafe
{-# INLINE fromJPImageRGB8 #-}

fromJPImageYF :: JP.Image JP.PixelF -> Image VS Y Float
fromJPImageYF = jpImageToImageUnsafe
{-# INLINE fromJPImageYF #-}

fromJPImageRGBA8 :: JP.Image JP.PixelRGBA8 -> Image VS RGBA Word8
fromJPImageRGBA8 = jpImageToImageUnsafe
{-# INLINE fromJPImageRGBA8 #-}

fromJPImageRGB16 :: JP.Image JP.PixelRGB16 -> Image VS RGB Word16
fromJPImageRGB16 = jpImageToImageUnsafe
{-# INLINE fromJPImageRGB16 #-}

fromJPImageRGBA16 :: JP.Image JP.PixelRGBA16 -> Image VS RGBA Word16
fromJPImageRGBA16 = jpImageToImageUnsafe
{-# INLINE fromJPImageRGBA16 #-}

fromJPImageRGBF :: JP.Image JP.PixelRGBF -> Image VS RGB Float
fromJPImageRGBF = jpImageToImageUnsafe
{-# INLINE fromJPImageRGBF #-}

fromJPImageYCbCr8 :: JP.Image JP.PixelYCbCr8 -> Image VS YCbCr Word8
fromJPImageYCbCr8 = jpImageToImageUnsafe
{-# INLINE fromJPImageYCbCr8 #-}

fromJPImageCMYK8 :: JP.Image JP.PixelCMYK8 -> Image VS CMYK Word8
fromJPImageCMYK8 = jpImageToImageUnsafe
{-# INLINE fromJPImageCMYK8 #-}

fromJPImageCMYK16 :: JP.Image JP.PixelCMYK16 -> Image VS CMYK Word16
fromJPImageCMYK16 = jpImageToImageUnsafe
{-# INLINE fromJPImageCMYK16 #-}


jpImageY8ToImage :: JP.DynamicImage -> Either String (Image VS Y Word8)
jpImageY8ToImage (JP.ImageY8 jimg) = Right (fromJPImageY8 jimg)
jpImageY8ToImage jimg              = jpCSError "Y8 (Pixel Y Word8)" jimg
{-# INLINE jpImageY8ToImage #-}

jpImageY16ToImage :: JP.DynamicImage -> Either String (Image VS Y Word16)
jpImageY16ToImage (JP.ImageY16 jimg) = Right (fromJPImageY16 jimg)
jpImageY16ToImage jimg               = jpCSError "Y16 (Pixel Y Word16)" jimg
{-# INLINE jpImageY16ToImage #-}

jpImageYA8ToImage :: JP.DynamicImage -> Either String (Image VS YA Word8)
jpImageYA8ToImage (JP.ImageYA8 jimg) = Right (fromJPImageYA8 jimg)
jpImageYA8ToImage jimg               = jpCSError "YA8 (Pixel YA Word8)" jimg
{-# INLINE jpImageYA8ToImage #-}

jpImageYA16ToImage :: JP.DynamicImage -> Either String (Image VS YA Word16)
jpImageYA16ToImage (JP.ImageYA16 jimg) = Right (fromJPImageYA16 jimg)
jpImageYA16ToImage jimg = jpCSError "YA16 (Pixel YA Word16)" jimg
{-# INLINE jpImageYA16ToImage #-}

jpImageRGB8ToImage :: JP.DynamicImage -> Either String (Image VS RGB Word8)
jpImageRGB8ToImage (JP.ImageRGB8 jimg) = Right (fromJPImageRGB8 jimg)
jpImageRGB8ToImage jimg = jpCSError "RGB8 (Pixel RGB Word8)" jimg
{-# INLINE jpImageRGB8ToImage #-}

jpImageRGB16ToImage :: JP.DynamicImage -> Either String (Image VS RGB Word16)
jpImageRGB16ToImage (JP.ImageRGB16 jimg) = Right (fromJPImageRGB16 jimg)
jpImageRGB16ToImage jimg = jpCSError "RGB16 (Pixel RGB Word16)" jimg
{-# INLINE jpImageRGB16ToImage #-}

jpImageRGBFToImage :: JP.DynamicImage -> Either String (Image VS RGB Float)
jpImageRGBFToImage (JP.ImageRGBF jimg) = Right (fromJPImageRGBF jimg)
jpImageRGBFToImage jimg = jpCSError "RGBF (Pixel RGB Float)" jimg
{-# INLINE jpImageRGBFToImage #-}

jpImageRGBA8ToImage :: JP.DynamicImage -> Either String (Image VS RGBA Word8)
jpImageRGBA8ToImage (JP.ImageRGBA8 jimg) = Right (fromJPImageRGBA8 jimg)
jpImageRGBA8ToImage jimg = jpCSError "RGBA8 (Pixel RGBA Word8)" jimg
{-# INLINE jpImageRGBA8ToImage #-}

jpImageRGBA16ToImage :: JP.DynamicImage -> Either String (Image VS RGBA Word16)
jpImageRGBA16ToImage (JP.ImageRGBA16 jimg) = Right (fromJPImageRGBA16 jimg)
jpImageRGBA16ToImage jimg = jpCSError "RGBA16 (Pixel RGBA Word16)" jimg
{-# INLINE jpImageRGBA16ToImage #-}

jpImageYCbCr8ToImage :: JP.DynamicImage -> Either String (Image VS YCbCr Word8)
jpImageYCbCr8ToImage (JP.ImageYCbCr8 jimg) = Right (fromJPImageYCbCr8 jimg)
jpImageYCbCr8ToImage jimg = jpCSError "YCbCr8 (Pixel YCbCr Word8)" jimg
{-# INLINE jpImageYCbCr8ToImage #-}

jpImageCMYK8ToImage :: JP.DynamicImage -> Either String (Image VS CMYK Word8)
jpImageCMYK8ToImage (JP.ImageCMYK8 jimg) = Right (fromJPImageCMYK8 jimg)
jpImageCMYK8ToImage jimg = jpCSError "CMYK8 (Pixel CMYK Word8)" jimg
{-# INLINE jpImageCMYK8ToImage #-}

jpImageCMYK16ToImage :: JP.DynamicImage -> Either String (Image VS CMYK Word16)
jpImageCMYK16ToImage (JP.ImageCMYK16 jimg) = Right (fromJPImageCMYK16 jimg)
jpImageCMYK16ToImage jimg = jpCSError "CMYK16 (Pixel CMYK Word16)" jimg
{-# INLINE jpImageCMYK16ToImage #-}


jpDynamicImageToImage
  :: (Convertible cs e, ColorSpace cs e, V.Storable (Pixel cs e)) =>
     JP.DynamicImage -> Image VS cs e
jpDynamicImageToImage (JP.ImageY8 jimg)     = convert $ fromJPImageY8 jimg
jpDynamicImageToImage (JP.ImageYA8 jimg)    = convert $ fromJPImageYA8 jimg
jpDynamicImageToImage (JP.ImageY16 jimg)    = convert $ fromJPImageY16 jimg
jpDynamicImageToImage (JP.ImageY32 jimg)    = convert $ fromJPImageY32 jimg
jpDynamicImageToImage (JP.ImageYA16 jimg)   = convert $ fromJPImageYA16 jimg
jpDynamicImageToImage (JP.ImageYF jimg)     = convert $ fromJPImageYF jimg
jpDynamicImageToImage (JP.ImageRGB8 jimg)   = convert $ fromJPImageRGB8 jimg
jpDynamicImageToImage (JP.ImageRGBA8 jimg)  = convert $ fromJPImageRGBA8 jimg
jpDynamicImageToImage (JP.ImageRGB16 jimg)  = convert $ fromJPImageRGB16 jimg
jpDynamicImageToImage (JP.ImageRGBA16 jimg) = convert $ fromJPImageRGBA16 jimg
jpDynamicImageToImage (JP.ImageRGBF jimg)   = convert $ fromJPImageRGBF jimg
jpDynamicImageToImage (JP.ImageYCbCr8 jimg) = convert $ fromJPImageYCbCr8 jimg
jpDynamicImageToImage (JP.ImageCMYK8 jimg)  = convert $ fromJPImageCMYK8 jimg
jpDynamicImageToImage (JP.ImageCMYK16 jimg) = convert $ fromJPImageCMYK16 jimg
{-# INLINE jpDynamicImageToImage #-}


jpImageShowCS :: JP.DynamicImage -> String
jpImageShowCS (JP.ImageY8 _)     = "Y8 (Pixel Y Word8)"
jpImageShowCS (JP.ImageY16 _)    = "Y16 (Pixel Y Word16)"
jpImageShowCS (JP.ImageY32 _)    = "Y32 (Pixel Y Word32)"
jpImageShowCS (JP.ImageYF _)     = "YF (Pixel Y Float)"
jpImageShowCS (JP.ImageYA8 _)    = "YA8 (Pixel YA Word8)"
jpImageShowCS (JP.ImageYA16 _)   = "YA16 (Pixel YA Word16)"
jpImageShowCS (JP.ImageRGB8 _)   = "RGB8 (Pixel RGB Word8)"
jpImageShowCS (JP.ImageRGB16 _)  = "RGB16 (Pixel RGB Word16)"
jpImageShowCS (JP.ImageRGBF _)   = "RGBF (Pixel RGB Float)"
jpImageShowCS (JP.ImageRGBA8 _)  = "RGBA8 (Pixel RGBA Word8)"
jpImageShowCS (JP.ImageRGBA16 _) = "RGBA16 (Pixel RGBA Word16)"
jpImageShowCS (JP.ImageYCbCr8 _) = "YCbCr8 (Pixel YCbCr Word8)"
jpImageShowCS (JP.ImageCMYK8 _)  = "CMYK8 (Pixel CMYK Word8)"
jpImageShowCS (JP.ImageCMYK16 _) = "CMYK16 (Pixel CMYK Word16)"


jpError :: String -> Either String a
jpError err = Left $ "JuicyPixel decoding error: " ++ err


jpCSError :: String -> JP.DynamicImage -> Either String a
jpCSError cs jimg =
  jpError $
  "Input image is in " ++
  jpImageShowCS jimg ++ ", cannot convert it to " ++ cs ++ " colorspace."



-- | Bitmap image with @.bmp@ extension.
data BMP = BMP deriving Show

instance ImageFormat BMP where
  data SaveOption BMP

  ext _ = ".bmp"


--------------------------------------------------------------------------------
-- Decoding BMP Format ---------------------------------------------------------
--------------------------------------------------------------------------------

instance Readable (Image VS X Bit) BMP where
  decode _ = fmap toImageBinary . jpImageY8ToImage <=< JP.decodeBitmap

instance Readable (Image VS Y Word8) BMP where
  decode _ = jpImageY8ToImage <=< JP.decodeBitmap

instance Readable (Image VS RGB Word8) BMP where
  decode _ = jpImageRGB8ToImage <=< JP.decodeBitmap

instance Readable (Image VS RGBA Word8) BMP where
  decode _ = jpImageRGBA8ToImage <=< JP.decodeBitmap


instance Readable (Image VS Y Double) BMP where
  decode _ = fmap jpDynamicImageToImage . JP.decodeBitmap

instance Readable (Image VS YA Double) BMP where
  decode _ = fmap jpDynamicImageToImage . JP.decodeBitmap

instance Readable (Image VS RGB Double) BMP where
  decode _ = fmap jpDynamicImageToImage . JP.decodeBitmap

instance Readable (Image VS RGBA Double) BMP where
  decode _ = fmap jpDynamicImageToImage . JP.decodeBitmap


--------------------------------------------------------------------------------
-- Encoding BMP Format ---------------------------------------------------------
--------------------------------------------------------------------------------

instance Writable (Image VS Y Word8) BMP where
  encode _ _ = JP.encodeBitmap . toJPImageY8

instance Writable (Image VS RGB Word8) BMP where
  encode _ _ = JP.encodeBitmap . toJPImageRGB8

instance Writable (Image VS RGBA Word8) BMP where
  encode _ _ = JP.encodeBitmap . toJPImageRGBA8

instance Writable (Image VS X Bit) BMP where
  encode _ _ = JP.encodeBitmap . toJPImageY8 . fromImageBinary


instance Writable (Image VS Y Double) BMP where
  encode _ _ = JP.encodeBitmap . toJPImageY8 . toWord8I

instance Writable (Image VS YA Double) BMP where
  encode _ _ = JP.encodeBitmap . toJPImageY8 . toWord8I . toImageY

instance Writable (Image VS RGB Double) BMP where
  encode _ _ = JP.encodeBitmap . toJPImageRGB8 . toWord8I

instance Writable (Image VS RGBA Double) BMP where
  encode _ _ = JP.encodeBitmap . toJPImageRGBA8 . toWord8I



-- | Graphics Interchange Format image with @.gif@ extension.
data GIF = GIF deriving Show

instance ImageFormat GIF where
  data SaveOption GIF = GIFPalette JP.PaletteOptions

  ext _ = ".gif"


-- | Graphics Interchange Format animated image with @.gif@ extension.
data GIFA = GIFA deriving Show
{-# DEPRECATED GIFA "use (`Seq` `GIF`) instead" #-}


instance ImageFormat GIFA where
  data SaveOption GIFA = GIFAPalette JP.PaletteOptions
                       | GIFALooping JP.GifLooping

  ext _ = ext GIF



instance ImageFormat (Seq GIF) where
  data SaveOption (Seq GIF) = GIFSeqPalette JP.PaletteOptions
                            | GIFSeqLooping JP.GifLooping
                            | GIFSeqDisposal JP.GifDisposalMethod
  ext _ = ext GIF



--------------------------------------------------------------------------------
-- Decoding GIF Format ---------------------------------------------------------
--------------------------------------------------------------------------------

instance Readable (Image VS RGB Word8) GIF where
  decode _ = jpImageRGB8ToImage <=< JP.decodeGif

instance Readable (Image VS RGBA Word8) GIF where
  decode _ = jpImageRGBA8ToImage <=< JP.decodeGif


instance Readable (Image VS Y Double) GIF where
  decode _ = fmap jpDynamicImageToImage . JP.decodeGif

instance Readable (Image VS YA Double) GIF where
  decode _ = fmap jpDynamicImageToImage . JP.decodeGif

instance Readable (Image VS RGB Double) GIF where
  decode _ = fmap jpDynamicImageToImage . JP.decodeGif

instance Readable (Image VS RGBA Double) GIF where
  decode _ = fmap jpDynamicImageToImage . JP.decodeGif


-- Animated GIF Format frames reading into a list

decodeGifs :: (JP.DynamicImage -> Either String img)
           -> B.ByteString -> Either String [img]
decodeGifs decoder bs = do
  imgs <- JP.decodeGifImages bs
  sequence $ fmap decoder imgs


decodeGifsDelays :: (JP.DynamicImage -> Either String img)
                 -> B.ByteString -> Either String [(JP.GifDelay, img)]
decodeGifsDelays decoder bs = do
  imgs <- JP.decodeGifImages bs
  delays <- JP.getDelaysGifImages bs
  gifs <- sequence $ fmap decoder imgs
  return $ zip delays gifs



instance Readable [Image VS RGB Word8] GIFA where
  decode _ = decodeGifs jpImageRGB8ToImage

instance Readable [Image VS RGBA Word8] GIFA where
  decode _ = decodeGifs jpImageRGBA8ToImage

instance Readable [(JP.GifDelay, Image VS RGB Word8)] GIFA where
  decode _ = decodeGifsDelays jpImageRGB8ToImage

instance Readable [(JP.GifDelay, Image VS RGBA Word8)] GIFA where
  decode _ = decodeGifsDelays jpImageRGBA8ToImage



instance Readable [Image VS Y Double] GIFA where
  decode _ = decodeGifs (Right . jpDynamicImageToImage)

instance Readable [Image VS YA Double] GIFA where
  decode _ = decodeGifs (Right . jpDynamicImageToImage)

instance Readable [Image VS RGB Double] GIFA where
  decode _ = decodeGifs (Right . jpDynamicImageToImage)

instance Readable [Image VS RGBA Double] GIFA where
  decode _ = decodeGifs (Right . jpDynamicImageToImage)


instance Readable [Image VS RGB Word8] (Seq GIF) where
  decode _ = decodeGifs jpImageRGB8ToImage

instance Readable [Image VS RGBA Word8] (Seq GIF) where
  decode _ = decodeGifs jpImageRGBA8ToImage

instance Readable [(JP.GifDelay, Image VS RGB Word8)] (Seq GIF) where
  decode _ = decodeGifsDelays jpImageRGB8ToImage

instance Readable [(JP.GifDelay, Image VS RGBA Word8)] (Seq GIF) where
  decode _ = decodeGifsDelays jpImageRGBA8ToImage



instance Readable [Image VS Y Double] (Seq GIF) where
  decode _ = decodeGifs (Right . jpDynamicImageToImage)

instance Readable [Image VS YA Double] (Seq GIF) where
  decode _ = decodeGifs (Right . jpDynamicImageToImage)

instance Readable [Image VS RGB Double] (Seq GIF) where
  decode _ = decodeGifs (Right . jpDynamicImageToImage)

instance Readable [Image VS RGBA Double] (Seq GIF) where
  decode _ = decodeGifs (Right . jpDynamicImageToImage)



--------------------------------------------------------------------------------
-- Encoding GIF Format ---------------------------------------------------------
--------------------------------------------------------------------------------

encodeGIF :: [SaveOption GIF]
          -> Image VS RGB Word8
          -> BL.ByteString
encodeGIF []                     =
  either error id . uncurry JP.encodeGifImageWithPalette .
  JP.palettize JP.defaultPaletteOptions . toJPImageRGB8
encodeGIF (GIFPalette palOpts:_) =
  either error id . uncurry JP.encodeGifImageWithPalette .
  JP.palettize palOpts . toJPImageRGB8
{-# INLINE encodeGIF #-}

instance Writable (Image VS RGB Word8) GIF where
  encode _ = encodeGIF

instance Writable (Image VS Y Double) GIF where
  encode _ _ = JP.encodeGifImage . toJPImageY8 . toWord8I

instance Writable (Image VS YA Double) GIF where
  encode f opts = encode f opts . toImageY

instance Writable (Image VS RGB Double) GIF where
  encode _ opts = encodeGIF opts . toWord8I

instance Writable (Image VS RGBA Double) GIF where
  encode f opts = encode f opts . toImageRGB


encodeGIFA :: [SaveOption GIFA]
           -> [(JP.GifDelay, Image VS RGB Word8)] -> BL.ByteString
encodeGIFA !opts =
  either error id . JP.encodeGifImages (getGIFALoop opts) . P.map palletizeGif where
    getGIFALoop []                = JP.LoopingNever
    getGIFALoop (GIFALooping l:_) = l
    getGIFALoop (_:xs)            = getGIFALoop xs
    getGIFAPal []                      = JP.defaultPaletteOptions
    getGIFAPal (GIFAPalette palOpts:_) = palOpts
    getGIFAPal (_:xs)                  = getGIFAPal xs
    palletizeGif !(d, img) = (p, d, jimg) where
      !(jimg, p) = JP.palettize (getGIFAPal opts) $ toJPImageRGB8 img


instance Writable [(JP.GifDelay, Image VS RGB Word8)] GIFA where
  encode _ opts = encodeGIFA opts

instance Writable [(JP.GifDelay, Image VS RGB Double)] GIFA where
  encode _ opts = encodeGIFA opts . fmap (\ !(d, i) -> (d, toWord8I i))


encodeGIFSeq :: [SaveOption (Seq GIF)]
           -> [(JP.GifDelay, Image VS RGB Word8)] -> BL.ByteString
encodeGIFSeq !opts =
  either error id . JP.encodeGifImages (getGIFSeqLoop opts) . P.map palletizeGif where
    getGIFSeqLoop []                  = JP.LoopingNever
    getGIFSeqLoop (GIFSeqLooping l:_) = l
    getGIFSeqLoop (_:xs)              = getGIFSeqLoop xs
    getGIFSeqPal []                        = JP.defaultPaletteOptions
    getGIFSeqPal (GIFSeqPalette palOpts:_) = palOpts
    getGIFSeqPal (_:xs)                    = getGIFSeqPal xs
    palletizeGif !(d, img) = (p, d, jimg) where
      !(jimg, p) = JP.palettize (getGIFSeqPal opts) $ toJPImageRGB8 img
    {-# INLINE palletizeGif #-}
{-# INLINE encodeGIFSeq #-}

{-# INLINE encodeGIFSeqA #-}
encodeGIFSeqA :: [SaveOption (Seq GIF)]
           -> [(JP.GifDelay, Image VS RGBA Word8)] -> BL.ByteString
encodeGIFSeqA !opts frms =
  case output of
    Left err -> error err
    Right res -> res
  where width = JP.imageWidth $ snd $ head jPimgs
        height = JP.imageHeight $ snd $ head jPimgs
        jPimgs = map (\(d,i) -> (d,toJPImageRGBA8 i)) frms
        frames = JP.palettizeWithAlpha jPimgs $ getGIFSeqDisposal opts
        getGIFSeqDisposal []                          = JP.DisposalRestoreBackground
        getGIFSeqDisposal (GIFSeqDisposal disposal:_) = disposal
        getGIFSeqDisposal (_:xs)                      = getGIFSeqDisposal xs
        getGIFSeqLoop []                  = JP.LoopingNever
        getGIFSeqLoop (GIFSeqLooping l:_) = l
        getGIFSeqLoop (_:xs)              = getGIFSeqLoop xs
        input = JP.GifEncode width height Nothing Nothing (getGIFSeqLoop opts) frames
        output = JP.encodeComplexGifImage input

instance Writable [(JP.GifDelay, Image VS RGB Word8)] (Seq GIF) where
  encode _ opts = encodeGIFSeq opts

instance Writable [(JP.GifDelay, Image VS RGBA Word8)] (Seq GIF) where
  encode _ opts = encodeGIFSeqA opts

instance Writable [(JP.GifDelay, Image VS RGB Double)] (Seq GIF) where
  encode _ opts = encodeGIFSeq opts . fmap (fmap toWord8I)






-- | High-dynamic-range image with @.hdr@ or @.pic@ extension.
data HDR = HDR deriving Show

instance ImageFormat HDR where
  data SaveOption HDR

  ext _ = ".hdr"

  exts _ = [".hdr", ".pic"]


--------------------------------------------------------------------------------
-- Decoding HDR Format ---------------------------------------------------------
--------------------------------------------------------------------------------

instance Readable (Image VS RGB Float) HDR where
  decode _ = jpImageRGBFToImage <=< JP.decodeHDR


instance Readable (Image VS Y Double) HDR where
  decode _ = fmap jpDynamicImageToImage . JP.decodeHDR

instance Readable (Image VS YA Double) HDR where
  decode _ = fmap jpDynamicImageToImage . JP.decodeHDR

instance Readable (Image VS RGB Double) HDR where
  decode _ = fmap jpDynamicImageToImage . JP.decodeHDR

instance Readable (Image VS RGBA Double) HDR where
  decode _ = fmap jpDynamicImageToImage . JP.decodeHDR


--------------------------------------------------------------------------------
-- Encoding HDR Format ---------------------------------------------------------
--------------------------------------------------------------------------------

instance Writable (Image VS RGB Float) HDR where
  encode _ _ = JP.encodeHDR . toJPImageRGBF

instance Writable (Image VS Y Double) HDR where
  encode _ _ = JP.encodeHDR . toJPImageRGBF . toFloatI . toImageRGB

instance Writable (Image VS YA Double) HDR where
  encode _ _ = JP.encodeHDR . toJPImageRGBF . toFloatI . toImageRGB

instance Writable (Image VS RGB Double) HDR where
  encode _ _ = JP.encodeHDR . toJPImageRGBF . toFloatI

instance Writable (Image VS RGBA Double) HDR where
  encode _ _ = JP.encodeHDR . toJPImageRGBF . toFloatI . toImageRGB


-- | Joint Photographic Experts Group image with @.jpg@ or @.jpeg@ extension.
data JPG = JPG deriving Show

instance ImageFormat JPG where
  data SaveOption JPG = JPGQuality Word8

  ext _ = ".jpg"

  exts _ = [".jpg", ".jpeg"]


--------------------------------------------------------------------------------
-- Decoding JPG Format ---------------------------------------------------------
--------------------------------------------------------------------------------

instance Readable (Image VS Y Word8) JPG where
  decode _ = jpImageY8ToImage <=< JP.decodeJpeg

instance Readable (Image VS YA Word8) JPG where
  decode _ = jpImageYA8ToImage <=< JP.decodeJpeg

instance Readable (Image VS RGB Word8) JPG where
  decode _ = jpImageRGB8ToImage <=< JP.decodeJpeg

instance Readable (Image VS CMYK Word8) JPG where
  decode _ = jpImageCMYK8ToImage <=< JP.decodeJpeg

instance Readable (Image VS YCbCr Word8) JPG where
  decode _ = jpImageYCbCr8ToImage <=< JP.decodeJpeg


instance Readable (Image VS Y Double) JPG where
  decode _ = fmap jpDynamicImageToImage . JP.decodeJpeg

instance Readable (Image VS YA Double) JPG where
  decode _ = fmap jpDynamicImageToImage . JP.decodeJpeg

instance Readable (Image VS RGB Double) JPG where
  decode _ = fmap jpDynamicImageToImage . JP.decodeJpeg

instance Readable (Image VS RGBA Double) JPG where
  decode _ = fmap jpDynamicImageToImage . JP.decodeJpeg


--------------------------------------------------------------------------------
-- Encoding JPG Format ---------------------------------------------------------
--------------------------------------------------------------------------------

encodeJPG
  :: JP.JpgEncodable px
  => [SaveOption JPG] -> JP.Image px -> BL.ByteString
encodeJPG []               =
  JP.encodeDirectJpegAtQualityWithMetadata 100 M.mempty
encodeJPG (JPGQuality q:_) =
  JP.encodeDirectJpegAtQualityWithMetadata q M.mempty
{-# INLINE encodeJPG #-}

instance Writable (Image VS Y Word8) JPG where
  encode _ opts = encodeJPG opts . toJPImageY8

instance Writable (Image VS RGB Word8) JPG where
  encode _ opts = encodeJPG opts . toJPImageRGB8

instance Writable (Image VS CMYK Word8) JPG where
  encode _ opts = encodeJPG opts . toJPImageCMYK8

instance Writable (Image VS YCbCr Word8) JPG where
  encode _ opts = encodeJPG opts . toJPImageYCbCr8

-- | Image is converted `YCbCr` color space prior to encoding.
instance Writable (Image VS Y Double) JPG where
  encode _ opts = encodeJPG opts . toJPImageYCbCr8 . toWord8I . toImageYCbCr

-- | Image is converted `YCbCr` color space prior to encoding.
instance Writable (Image VS YA Double) JPG where
  encode _ opts = encodeJPG opts . toJPImageYCbCr8 . toWord8I . toImageYCbCr

-- | Image is converted `YCbCr` color space prior to encoding.
instance Writable (Image VS RGB Double) JPG where
  encode _ opts = encodeJPG opts . toJPImageYCbCr8 . toWord8I . toImageYCbCr

-- | Image is converted `YCbCr` color space prior to encoding.
instance Writable (Image VS RGBA Double) JPG where
  encode _ opts = encodeJPG opts . toJPImageYCbCr8 . toWord8I . toImageYCbCr




-- | Portable Network Graphics image with @.png@ extension.
data PNG = PNG deriving Show

instance ImageFormat PNG where
  data SaveOption PNG

  ext _ = ".png"



--------------------------------------------------------------------------------
-- Decoding PNG Format ---------------------------------------------------------
--------------------------------------------------------------------------------

instance Readable (Image VS X Bit) PNG where
  decode _ = fmap toImageBinary . jpImageY8ToImage <=< JP.decodePng

instance Readable (Image VS Y Word8) PNG where
  decode _ = jpImageY8ToImage <=< JP.decodePng

instance Readable (Image VS Y Word16) PNG where
  decode _ = jpImageY16ToImage <=< JP.decodePng

instance Readable (Image VS YA Word8) PNG where
  decode _ = jpImageYA8ToImage <=< JP.decodePng

instance Readable (Image VS YA Word16) PNG where
  decode _ = jpImageYA16ToImage <=< JP.decodePng

instance Readable (Image VS RGB Word8) PNG where
  decode _ = jpImageRGB8ToImage <=< JP.decodePng

instance Readable (Image VS RGB Word16) PNG where
  decode _ = jpImageRGB16ToImage <=< JP.decodePng

instance Readable (Image VS RGBA Word8) PNG where
  decode _ = jpImageRGBA8ToImage <=< JP.decodePng

instance Readable (Image VS RGBA Word16) PNG where
  decode _ = jpImageRGBA16ToImage <=< JP.decodePng


instance Readable (Image VS Y Double) PNG where
  decode _ = fmap jpDynamicImageToImage . JP.decodePng

instance Readable (Image VS YA Double) PNG where
  decode _ = fmap jpDynamicImageToImage . JP.decodePng

instance Readable (Image VS RGB Double) PNG where
  decode _ = fmap jpDynamicImageToImage . JP.decodePng

instance Readable (Image VS RGBA Double) PNG where
  decode _ = fmap jpDynamicImageToImage . JP.decodePng


--------------------------------------------------------------------------------
-- Encoding PNG Format ---------------------------------------------------------
--------------------------------------------------------------------------------

instance Writable (Image VS X Bit) PNG where
  encode _ _ = JP.encodePng . toJPImageY8 . fromImageBinary

instance Writable (Image VS Y Word8) PNG where
  encode _ _ = JP.encodePng . toJPImageY8

instance Writable (Image VS Y Word16) PNG where
  encode _ _ = JP.encodePng . toJPImageY16

instance Writable (Image VS YA Word8) PNG where
  encode _ _ = JP.encodePng . toJPImageYA8

instance Writable (Image VS YA Word16) PNG where
  encode _ _ = JP.encodePng . toJPImageYA16

instance Writable (Image VS RGB Word8) PNG where
  encode _ _ = JP.encodePng . toJPImageRGB8

instance Writable (Image VS RGB Word16) PNG where
  encode _ _ = JP.encodePng . toJPImageRGB16

instance Writable (Image VS RGBA Word8) PNG where
  encode _ _ = JP.encodePng . toJPImageRGBA8

instance Writable (Image VS RGBA Word16) PNG where
  encode _ _ = JP.encodePng . toJPImageRGBA16


instance Writable (Image VS Y Double) PNG where
  encode _ _ = JP.encodePng . toJPImageY16 . toWord16I

instance Writable (Image VS YA Double) PNG where
  encode _ _ = JP.encodePng . toJPImageYA16 . toWord16I

instance Writable (Image VS RGB Double) PNG where
  encode _ _ = JP.encodePng . toJPImageRGB16 . toWord16I

instance Writable (Image VS RGBA Double) PNG where
  encode _ _ = JP.encodePng . toJPImageRGBA16 . toWord16I






-- | Truevision Graphics Adapter image with .tga extension.
data TGA = TGA

instance ImageFormat TGA where
  data SaveOption TGA

  ext _ = ".tga"


--------------------------------------------------------------------------------
-- Decoding TGA Format ---------------------------------------------------------
--------------------------------------------------------------------------------

instance Readable (Image VS X Bit) TGA where
  decode _ = fmap toImageBinary . jpImageY8ToImage <=< JP.decodeTga

instance Readable (Image VS Y Word8) TGA where
  decode _ = jpImageY8ToImage <=< JP.decodeTga

instance Readable (Image VS RGB Word8) TGA where
  decode _ = jpImageRGB8ToImage <=< JP.decodeTga

instance Readable (Image VS RGBA Word8) TGA where
  decode _ = jpImageRGBA8ToImage <=< JP.decodeTga


instance Readable (Image VS Y Double) TGA where
  decode _ = fmap jpDynamicImageToImage . JP.decodeTga

instance Readable (Image VS YA Double) TGA where
  decode _ = fmap jpDynamicImageToImage . JP.decodeTga

instance Readable (Image VS RGB Double) TGA where
  decode _ = fmap jpDynamicImageToImage . JP.decodeTga

instance Readable (Image VS RGBA Double) TGA where
  decode _ = fmap jpDynamicImageToImage . JP.decodeTga


--------------------------------------------------------------------------------
-- Encoding TGA Format ---------------------------------------------------------
--------------------------------------------------------------------------------

instance Writable (Image VS X Bit) TGA where
  encode _ _ = JP.encodeTga . toJPImageY8 . fromImageBinary

instance Writable (Image VS Y Word8) TGA where
  encode _ _ = JP.encodeTga . toJPImageY8

instance Writable (Image VS RGB Word8) TGA where
  encode _ _ = JP.encodeTga . toJPImageRGB8

instance Writable (Image VS RGBA Word8) TGA where
  encode _ _ = JP.encodeTga . toJPImageRGBA8


instance Writable (Image VS Y Double) TGA where
  encode _ _ = JP.encodeTga . toJPImageY8 . toWord8I

instance Writable (Image VS YA Double) TGA where
  encode _ _ = JP.encodeTga . toJPImageY8 . toWord8I . toImageY

instance Writable (Image VS RGB Double) TGA where
  encode _ _ = JP.encodeTga . toJPImageRGB8 . toWord8I

instance Writable (Image VS RGBA Double) TGA where
  encode _ _ = JP.encodeTga . toJPImageRGBA8 . toWord8I



-- | Tagged Image File Format image with @.tif@ or @.tiff@ extension.
data TIF = TIF deriving Show

instance ImageFormat TIF where
  data SaveOption TIF

  ext _ = ".tif"

  exts _ = [".tif", ".tiff"]


--------------------------------------------------------------------------------
-- Decoding TIF Format ---------------------------------------------------------
--------------------------------------------------------------------------------

instance Readable (Image VS X Bit) TIF where
  decode _ = fmap toImageBinary . jpImageY8ToImage <=< JP.decodeTiff

instance Readable (Image VS Y Word8) TIF where
  decode _ = jpImageY8ToImage <=< JP.decodeTiff

instance Readable (Image VS Y Word16) TIF where
  decode _ = jpImageY16ToImage <=< JP.decodeTiff

instance Readable (Image VS YA Word8) TIF where
  decode _ = jpImageYA8ToImage <=< JP.decodeTiff

instance Readable (Image VS YA Word16) TIF where
  decode _ = jpImageYA16ToImage <=< JP.decodeTiff

instance Readable (Image VS RGB Word8) TIF where
  decode _ = jpImageRGB8ToImage <=< JP.decodeTiff

instance Readable (Image VS RGB Word16) TIF where
  decode _ = jpImageRGB16ToImage <=< JP.decodeTiff

instance Readable (Image VS RGBA Word8) TIF where
  decode _ = jpImageRGBA8ToImage <=< JP.decodeTiff

instance Readable (Image VS RGBA Word16) TIF where
  decode _ = jpImageRGBA16ToImage <=< JP.decodeTiff

instance Readable (Image VS CMYK Word8) TIF where
  decode _ = jpImageCMYK8ToImage <=< JP.decodeTiff

instance Readable (Image VS CMYK Word16) TIF where
  decode _ = jpImageCMYK16ToImage <=< JP.decodeTiff


instance Readable (Image VS Y Double) TIF where
  decode _ = fmap jpDynamicImageToImage . JP.decodeTiff

instance Readable (Image VS YA Double) TIF where
  decode _ = fmap jpDynamicImageToImage . JP.decodeTiff

instance Readable (Image VS RGB Double) TIF where
  decode _ = fmap jpDynamicImageToImage . JP.decodeTiff

instance Readable (Image VS RGBA Double) TIF where
  decode _ = fmap jpDynamicImageToImage . JP.decodeTiff


--------------------------------------------------------------------------------
-- Encoding TIF Format ---------------------------------------------------------
--------------------------------------------------------------------------------

instance Writable (Image VS Y Word8) TIF where
  encode _ _ = JP.encodeTiff . toJPImageY8

instance Writable (Image VS Y Word16) TIF where
  encode _ _ = JP.encodeTiff . toJPImageY16

instance Writable (Image VS YA Word8) TIF where
  encode _ _ = JP.encodeTiff . toJPImageYA8

instance Writable (Image VS YA Word16) TIF where
  encode _ _ = JP.encodeTiff . toJPImageYA16

instance Writable (Image VS RGB Word8) TIF where
  encode _ _ = JP.encodeTiff . toJPImageRGB8

instance Writable (Image VS RGB Word16) TIF where
  encode _ _ = JP.encodeTiff . toJPImageRGB16

instance Writable (Image VS RGBA Word8) TIF where
  encode _ _ = JP.encodeTiff . toJPImageRGBA8

instance Writable (Image VS RGBA Word16) TIF where
  encode _ _ = JP.encodeTiff . toJPImageRGBA16

instance Writable (Image VS YCbCr Word8) TIF where
  encode _ _ = JP.encodeTiff . toJPImageYCbCr8

instance Writable (Image VS CMYK Word8) TIF where
  encode _ _ = JP.encodeTiff . toJPImageCMYK8

instance Writable (Image VS CMYK Word16) TIF where
  encode _ _ = JP.encodeTiff . toJPImageCMYK16


instance Writable (Image VS X Bit) TIF where
  encode _ _ = JP.encodeTiff . toJPImageY8 . fromImageBinary

instance Writable (Image VS Y Double) TIF where
  encode _ _ = JP.encodeTiff . toJPImageY16 . toWord16I

instance Writable (Image VS YA Double) TIF where
  encode _ _ = JP.encodeTiff . toJPImageYA16 . toWord16I

instance Writable (Image VS RGB Double) TIF where
  encode _ _ = JP.encodeTiff . toJPImageRGB16 . toWord16I

instance Writable (Image VS RGBA Double) TIF where
  encode _ _ = JP.encodeTiff . toJPImageRGBA16 . toWord16I

instance Writable (Image VS YCbCr Double) TIF where
  encode _ _ = JP.encodeTiff . toJPImageYCbCr8 . toWord8I

instance Writable (Image VS CMYK Double) TIF where
  encode _ _ = JP.encodeTiff . toJPImageCMYK16 . toWord16I


