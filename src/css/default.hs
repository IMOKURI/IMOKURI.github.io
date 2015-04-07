
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (all, div, (**))
import Clay

import qualified Clay.Media as M


main :: IO ()
main = putCss defaultStyle


defaultStyle :: Css
defaultStyle = do

  body ? do
    color       black
    fontSize    (px 16)
    sym2 margin nil auto
    width       (px 600)

  div # "#header" ? do
    borderBottom solid (px 2) black
    marginBottom (px 30)
    sym2 padding (px 12) nil

  div # "#header" ** "#navigation" ? do
    textAlign (alignSide sideRight)

  div # "#header" ** "#navigation" ** a ? do
    color          black
    fontSize       (px 18)
    fontWeight     bold
    marginLeft     (px 12)
    textDecoration none
    textTransform  uppercase

  div # "#logo" ** a ? do
    color          black
    float          floatLeft
    fontSize       (px 18)
    fontWeight     bold
    textDecoration none

  h1 ? do
    fontSize (px 24)

  h2 ? do
    fontSize (px 20)

  div # ".info" ? do
    color    "#555"
    fontSize  (px 14)
    fontStyle italic

  div # "#footer" ? do
    borderTop      solid (px 2) black
    color          "#555"
    fontSize       (px 12)
    marginTop      (px 30)
    sym2 padding   (px 12) nil
    textAlign      (alignSide sideRight)
    textTransform  uppercase

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

