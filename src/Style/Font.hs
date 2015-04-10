
{-# LANGUAGE OverloadedStrings #-}

module Style.Font
( importFonts
, kokuMin
, kokuGo
, rictyDiminished
) where

import Clay


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

  h1 $ do
    fontSize    (em 2)
    sym2 margin (em 0.67) nil


kokuMin :: Css
kokuMin = fontFamily ["Koku Mincho"] [serif]

kokuGo :: Css
kokuGo = fontFamily ["Koku Gothic"] [sansSerif]

rictyDiminished :: Css
rictyDiminished = fontFamily ["Ricty Diminished"] [monospace]

