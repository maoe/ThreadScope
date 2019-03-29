-------------------------------------------------------------------------------
--- $Id: ViewerColours.hs#2 2009/07/18 22:48:30 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/ViewerColours.hs $
-------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
module GUI.ViewerColours
  ( Color
  , setSourceRGBAhex
    -- * Colours
  , runningColour
  , gcColour
  , gcWaitColour
  , gcStartColour
  , gcWorkColour
  , gcIdleColour
  , gcEndColour
  , createThreadColour
  , seqGCReqColour
  , parGCReqColour
  , migrateThreadColour
  , threadWakeupColour
  , shutdownColour
  , labelTextColour
  , bookmarkColour
  , fizzledDudsColour
  , createdConvertedColour
  , overflowedColour
  , userMessageColour
  , outerPercentilesColour

  , module Data.Colour.Names
  ) where
import Data.Word (Word16)

import Data.Colour.SRGB
import Data.Colour.Names
import GI.Cairo.Render

type Color = Colour Double

setSourceRGBAhex
  :: Color
  -> Double -- ^ Alpha
  -> Render ()
setSourceRGBAhex colour = setSourceRGBA channelRed channelGreen channelBlue
  where
    RGB {..} = toSRGB colour

sRGB48 :: (Ord a, Floating a) => Word16 -> Word16 -> Word16 -> Colour a
sRGB48 = sRGBBounded

-------------------------------------------------------------------------------

-- Colours

runningColour :: (Ord a, Floating a) => Colour a
runningColour = darkGreen

gcColour :: (Ord a, Floating a) => Colour a
gcColour = orange

gcWaitColour :: (Ord a, Floating a) => Colour a
gcWaitColour = lightOrange

gcStartColour, gcWorkColour, gcIdleColour, gcEndColour
  :: (Ord a, Floating a)
  => Colour a
gcStartColour = lightOrange
gcWorkColour  = orange
gcIdleColour  = lightOrange
gcEndColour   = lightOrange

createThreadColour :: (Ord a, Floating a) => Colour a
createThreadColour = lightBlue

seqGCReqColour :: (Ord a, Floating a) => Colour a
seqGCReqColour = cyan

parGCReqColour :: (Ord a, Floating a) => Colour a
parGCReqColour = darkBlue

migrateThreadColour :: (Ord a, Floating a) => Colour a
migrateThreadColour = darkRed

threadWakeupColour :: (Ord a, Floating a) => Colour a
threadWakeupColour = green

shutdownColour :: (Ord a, Floating a) => Colour a
shutdownColour = darkBrown

labelTextColour :: (Ord a, Floating a) => Colour a
labelTextColour = white

bookmarkColour :: (Ord a, Floating a) => Colour a
bookmarkColour = sRGB48 0xff00 0x0000 0xff00 -- pinkish

fizzledDudsColour, createdConvertedColour, overflowedColour
  :: (Ord a, Floating a)
  => Colour a
fizzledDudsColour      = grey
createdConvertedColour = darkGreen
overflowedColour       = red

userMessageColour :: (Ord a, Floating a) => Colour a
userMessageColour = darkRed

outerPercentilesColour :: (Ord a, Floating a) => Colour a
outerPercentilesColour = lightGrey

-------------------------------------------------------------------------------

lightGrey :: (Ord a, Floating a) => Colour a
lightGrey = sRGB48 0xD000 0xD000 0xD000

darkGreen :: (Ord a, Floating a) => Colour a
darkGreen = sRGB48 0x0000 0x6600 0x0000

lightBlue :: (Ord a, Floating a) => Colour a
lightBlue = sRGB48 0x6600 0x9900 0xFF00

darkBlue :: (Ord a, Floating a) => Colour a
darkBlue = sRGB48 0 0 0xBB00

darkRed :: (Ord a, Floating a) => Colour a
darkRed = sRGB48 0xcc00 0x0000 0x0000

lightOrange :: (Ord a, Floating a) => Colour a
lightOrange = sRGB48 0xE000 0xD000 0xB000 -- orange

darkBrown :: (Ord a, Floating a) => Colour a
darkBrown = sRGB48 0x6600 0 0
