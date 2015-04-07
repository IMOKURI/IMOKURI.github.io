
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (div, (**))
import Clay

defaultStyle :: Css
defaultStyle = do

  body ? do
    color       black
    fontSize    (px 16)
    sym2 margin (px 0) auto
    width       (px 600)

  div # "#header" ? do
    borderBottom solid (px 2) black
    marginBottom (px 30)
    sym2 padding (px 12) (px 0)

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
    borderTop    solid (px 2) black
    color        "#555"
    fontSize     (px 12)
    marginTop    (px 30)
    sym2 padding (px 12) (px 0)
    textAlign    (alignSide sideRight)


main :: IO ()
main = putCss defaultStyle

