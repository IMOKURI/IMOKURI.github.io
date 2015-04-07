--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid ((<>))
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- match "contents/images/*" $ do
    --     route   $ gsubRoute "contents/" (const "")
    --     compile copyFileCompiler

    match "src/css/*.hs" $ do
        route   $ setExtension "css" `composeRoutes` gsubRoute "src/" (const "")
        compile $ getResourceString
            -- If you can use cabal-install 1.20 or higher, you should use "cabal exec".
            -- Travis CI uses cabal-install 1.18 as of April, 2015... So this source uses "runghc".
            -- >>= withItemBody (unixFilter "cabal" ["exec", "runghc"])
            >>= withItemBody (unixFilter "runghc" ["-package-db=.cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d"])
            >>= return . fmap compressCss

    match (fromList ["contents/about.markdown", "contents/contact.markdown"]) $ do
        route   $ setExtension "html" `composeRoutes` gsubRoute "contents/" (const "")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "contents/posts/*" $ do
        route   $ setExtension "html" `composeRoutes` gsubRoute "contents/" (const "")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let blogCtx = listField "posts" postCtx (return posts)
                       <> constField "title" "Blog"
                       <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html"    blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls


    match "contents/index.html" $ do
        route   $ gsubRoute "contents/" (const "")
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx = listField "posts" postCtx (return posts)
                        <> constField "title" "Home"
                        <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
       <> defaultContext

