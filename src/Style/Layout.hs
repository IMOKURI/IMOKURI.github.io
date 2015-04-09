
{-# LANGUAGE OverloadedStrings #-}

module Style.Layout
( importLayout
) where

import Data.Monoid

import Clay
import qualified Clay.Media as M

-------------------------------------------------

importLayout :: Css
importLayout = do

  html <> body ? do
    display    inlineBlock
    -- *display   inline
    -- zoom       1
    letterSpacing  normal
    wordSpacing    normal
    verticalAlign  (alignSide sideTop)
    textRendering  auto

  header ? do
    width (pct 100)
    wide $ do
      width (pct 25)
      -- *width (pct 24.969)

  "#content" <> footer ? do
    width (pct 100)
    wide $ do
      width (pct 75)
      -- *width (pct 74.969)

-------------------------------------------------

pageWidth :: Size Abs
pageWidth = em 48

wide :: Css -> Css
wide = query screen [M.minWidth pageWidth]

