
{-# LANGUAGE OverloadedStrings #-}

module Style.Base
( defaultStyle
) where

import Prelude hiding (all, div, (**))
import Data.Monoid
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as TL (unpack)

import Clay
import qualified Clay.Media as M

import Style.Font


defaultStyle :: String
defaultStyle = TL.unpack $ render $ do

  star ? do
    boxSizing   borderBox

  body ? do
    sym margin     nil
    sym padding    nil
    display        inlineBlock
    letterSpacing  normal
    wordSpacing    normal
    verticalAlign  (alignSide sideTop)
    textRendering  auto

-----

  importFonts

-----

  header ? do
    width       (pct 100)
    top         auto
    textAlign   (alignSide sideCenter)
    sym2 margin (em 3) auto
    background   bgHeaderC
    color        font1HeaderC

    wide $ do
      width (pct 25)
      textAlign   (alignSide sideRight)
      sym3 margin (pct 80) (em 2) nil
      position   fixed
      top        nil
      bottom     nil

  ".logo" ? do
    sym margin     nil
    textDecoration none

  ".tagline" ? do
    sym margin nil
    fontWeight (weight 300)
    color      font2HeaderC

  nav ? do
    sym margin     nil
    sym padding    nil
    display        inlineBlock
    background     transparent
    border         solid (px 2) font2HeaderC
    color          font1HeaderC
    marginTop      (em 1)
    letterSpacing  (em 0.05)
    textTransform  uppercase
    textDecoration none
    fontSize       (pct 85)

-----

  ".content" ? do
    width (pct 100)

    wide $ do
      width (pct 75)



-------------------------------------------------------

bgHeaderC, font1HeaderC, font2HeaderC :: Color
bgHeaderC    = rgb  22 147 165
font1HeaderC = rgb 255 255 255
font2HeaderC = rgb   0 205 172

fontContentC :: Color
fontContentC = rgb 170 170 170

-------------------------------------------------------

pageWidth :: Size Abs
pageWidth = em 48

wide :: Css -> Css
wide = query screen [M.minWidth pageWidth]

-------------------------------------------------------

contentFont :: Css
contentFont = do
  kokuMin
  fontSize    (px 16)
  lineHeight  u1
  color       txtC

uiFont :: Css
uiFont = do
  kokuGo
  fontSize       (px 18)
  lineHeight     u1
  textDecoration none
  color          txtC
  fontWeight     bold

smallFont :: Css
smallFont =
  do kokuGo
     fontSize (pct 85)
     color    (setA 120 txtC)

-- codeBlocks :: Css
-- codeBlocks =
--   do bgBorder 20
--      rictyDiminished
--      syntax

-- syntax :: Css
-- syntax =
--   do color (rgb 0 60 100)
--      ".kw" ? fontWeight bold
--      ".kw" ? color (rgb   0   0   0)
--      ".dt" ? color (rgb  20  60 180)
--      ".dv" ? color (rgb 100   0 200)
--      ".st" ? color (rgb 200  80 100)
--      ".ot" ? color (rgb 160  80  80)
--      ".fu" ? color (rgb   0 160 120)
--      ".co" ? color (rgb 160 160 160)

-------------------------------------------------------

