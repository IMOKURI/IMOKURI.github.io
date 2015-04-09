
{-# LANGUAGE OverloadedStrings #-}

module Style.Base
( defaultStyle
) where

import Prelude hiding (all, div, (**))
import Data.Monoid
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as TL (unpack)

import Clay

import Style.Font


defaultStyle :: String
defaultStyle = TL.unpack $ render $ do

  importFonts

  body ? do
    sym2 margin nil auto
    width       (px 600)
    bg

  importArticles

  header ? do
    borderBottom solid (px 2) black
    marginBottom (px 30)
    sym2 padding (px 12) nil

  header ** nav ? do
    textAlign (alignSide sideRight)

  header ** nav ** a ? do
    uiFont
    marginLeft     (px 12)

  div # "#logo" ** a ? do
    uiFont
    float          floatLeft

  footer ? do
    borderTop      solid (px 2) black
    smallFont
    marginTop      (px 30)
    sym2 padding   (px 12) nil
    textAlign      (alignSide sideRight)

-------------------------------------------------------

bgC, txtC :: Color
bgC   = rgb 246 246 246
txtC  = rgb   0  20  40

-------------------------------------------------------

unit :: Integer -> Size Abs
unit = px . (* 24)

u1 :: Size Abs
u1 = unit 1

-------------------------------------------------------

gif :: T.Text -> BackgroundImage
gif im = url ("../images/" <> im <> ".gif")

bg :: Css
bg = background (gif "bg", bgC)

-- bgBorder :: Integer -> Css
-- bgBorder o = outline solid (px 1) (setA o black)

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

importArticles :: Css
importArticles = article ? do
  contentFont
  marginBottom u1

  ".info" ? do
    smallFont
    fontStyle italic

  h1 ? do
    fontSize (px 22)

  h2 ? do
    fontSize (px 20)

