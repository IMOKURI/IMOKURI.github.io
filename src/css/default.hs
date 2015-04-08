
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (all, div, (**))
import Data.Monoid
import Data.Text (Text)
import Clay

import qualified Clay.Media as M


main :: IO ()
main = putCss defaultStyle


defaultStyle :: Css
defaultStyle = do

  importFonts

  body ? do
    sym2 margin nil auto
    width       (px 600)
    bg

  importArticles

  div # "#header" ? do
    borderBottom solid (px 2) black
    marginBottom (px 30)
    sym2 padding (px 12) nil

  div # "#header" ** "#navigation" ? do
    textAlign (alignSide sideRight)

  div # "#header" ** "#navigation" ** a ? do
    uiFont
    marginLeft     (px 12)

  div # "#logo" ** a ? do
    uiFont
    float          floatLeft

  div # "#footer" ? do
    borderTop      solid (px 2) black
    smallFont
    marginTop      (px 30)
    sym2 padding   (px 12) nil
    textAlign      (alignSide sideRight)

-------------------------------------------------------

bgC, txtC, emC, linkC :: Color
bgC   = rgb 246 246 246
txtC  = rgb   0  20  40
emC   = rgb  40  20   0
linkC = rgb   0 100 180

-------------------------------------------------------

unit, half :: Integer -> Size Abs
unit = px . (* 24)
half = px . (* 12)

u1, u2, u3, u4 :: Size Abs
u1 = unit 1
u2 = unit 2
u3 = unit 3
u4 = unit 4

pageWidth :: Size Abs
pageWidth = unit 25

narrow :: Css -> Css
narrow = query all [M.maxWidth pageWidth]

wide :: Css -> Css
wide = query all [M.minWidth pageWidth]

-------------------------------------------------------

gif :: Text -> BackgroundImage
gif im = url ("../images/" <> im <> ".gif")

bg :: Css
bg = background (gif "bg", bgC)

bgBorder :: Integer -> Css
bgBorder o = outline solid (px 1) (setA o black)

-------------------------------------------------------

importFonts :: Css
importFonts = do

  fontFace $ do
    fontFamily ["Koku Mincho"] []
    fontFaceSrc [FontFaceSrcUrl "fonts/font_1_kokumr_1.00_rls.ttf" (Just TrueType)]

  fontFace $ do
    fontFamily ["Koku Gothic"] []
    fontFaceSrc [FontFaceSrcUrl "fonts/font_1_kokugl_1.15_rls.ttf" (Just TrueType)]

  fontFace $ do
    fontFamily ["Ricty Diminished"] []
    fontFaceSrc [FontFaceSrcUrl "fonts/RictyDiminished-Regular.ttf" (Just TrueType)]


kokuMin :: Css
kokuMin = fontFamily ["Koku Mincho"] [serif]

kokuGo :: Css
kokuGo = fontFamily ["Koku Gothic"] [sansSerif]

rictyDiminished :: Css
rictyDiminished = fontFamily ["Ricty Diminished"] [monospace]

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

codeBlocks :: Css
codeBlocks =
  do bgBorder 20
     rictyDiminished
     syntax

syntax :: Css
syntax =
  do color (rgb 0 60 100)
     ".kw" ? fontWeight bold
     ".kw" ? color (rgb   0   0   0)
     ".dt" ? color (rgb  20  60 180)
     ".dv" ? color (rgb 100   0 200)
     ".st" ? color (rgb 200  80 100)
     ".ot" ? color (rgb 160  80  80)
     ".fu" ? color (rgb   0 160 120)
     ".co" ? color (rgb 160 160 160)

-------------------------------------------------------

importArticles :: Css
importArticles = article ? do
  contentFont
  marginBottom u1

  ".info" ? do
    smallFont
    fontStyle italic

  h1 ? do
    fontSize (px 24)

  h2 ? do
    fontSize (px 20)

