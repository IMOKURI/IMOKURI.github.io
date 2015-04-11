
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
    fontFaceSrc [ FontFaceSrcUrl "fonts/font_1_kokumr_1.00_rls.eot" Nothing ]
    fontFaceSrc [ FontFaceSrcUrl "fonts/font_1_kokumr_1.00_rls.eot?#iefix" (Just EmbeddedOpenType)
                , FontFaceSrcUrl "fonts/font_1_kokumr_1.00_rls.woff" (Just WOFF)
                , FontFaceSrcUrl "fonts/font_1_kokumr_1.00_rls.ttf" (Just TrueType) ]

  fontFace $ do
    fontFamily ["Koku Gothic"] []
    fontFaceSrc [ FontFaceSrcUrl "fonts/font_1_kokugl_1.15_rls.eot" Nothing ]
    fontFaceSrc [ FontFaceSrcUrl "fonts/font_1_kokugl_1.15_rls.eot?#iefix" (Just EmbeddedOpenType)
                , FontFaceSrcUrl "fonts/font_1_kokugl_1.15_rls.woff" (Just WOFF)
                , FontFaceSrcUrl "fonts/font_1_kokugl_1.15_rls.ttf" (Just TrueType) ]

  fontFace $ do
    fontFamily ["Ricty Diminished"] []
    fontFaceSrc [ FontFaceSrcUrl "fonts/RictyDiminished-Regular.eot" Nothing ]
    fontFaceSrc [ FontFaceSrcUrl "fonts/RictyDiminished-Regular.eot?#iefix" (Just EmbeddedOpenType)
                , FontFaceSrcUrl "fonts/RictyDiminished-Regular.woff" (Just WOFF)
                , FontFaceSrcUrl "fonts/RictyDiminished-Regular.ttf" (Just TrueType) ]


kokuMin :: Css
kokuMin = fontFamily ["Koku Mincho"] [serif]

kokuGo :: Css
kokuGo = fontFamily ["Koku Gothic"] [sansSerif]

rictyDiminished :: Css
rictyDiminished = fontFamily ["Ricty Diminished"] [monospace]

