--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid ((<>))
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "src/css/main.hs" $ do
        route   $ setExtension "css" `composeRoutes` gsubRoute "src/" (const "")
        compile $ getResourceString
            >>= withItemBody (unixFilter "cabal" ["exec", "runghc"])
            >>= return . fmap compressCss

    match "pages/about.markdown" $ do
        route   $ setExtension "html" `composeRoutes` gsubRoute "pages/" (const "")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route   $ setExtension "html"
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

    match "pages/index.html" $ do
        route   $ gsubRoute "pages/" (const "")
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx = listField "posts" postCtx (return posts)
                        <> constField "title" "Home"
                        <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "pages/robots.txt" $ do
        route   $ gsubRoute "pages/" (const "")
        compile copyFileCompiler

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
       <> defaultContext

