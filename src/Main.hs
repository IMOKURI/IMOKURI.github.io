--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid ((<>))
import           Hakyll
import           System.FilePath

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match ( "images/*"
       .||. "fonts/*"
       .||. "README.md" ) $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*/*/*" $ do
        route   $ setExtension "html" `composeRoutes` gsubRoute "posts/" (const "blog/")
        compile $ pandocCompiler
            >>= saveSnapshot "posts"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["blog.html"] $ do
        route $ customRoute directoryIndex
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*/*/*"
            let blogCtx = listField "posts" postCtx (return posts)
                       <> constField "title" "Blog"
                       <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html"    blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls

    -- blog <- buildPaginateWith
    --   (sortRecentFirst >=> return . paginateEvery 10)
    --   "posts/*/*/*"
    --   (\n -> if n == 1
    --          then fromFilePath "blog/"
    --          else fromFilePath $ "blog/" ++ show n ++ "/")

    match "pages/index.html" $ do
        route   $ gsubRoute "pages/" (const "")
        compile $ do
            posts <- fmap (take 5) . recentFirst =<< loadAll "posts/*/*/*"
            let indexCtx = listField "posts" postCtx (return posts)
                        <> constField "title" "Home"
                        <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "pages/about.markdown" $ do
        route   $ customRoute directoryIndex
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "pages/404.markdown" $ do
        route   $ setExtension "html" `composeRoutes` gsubRoute "pages/" (const "")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "etc/*" $ do
        route   $ gsubRoute "etc/" (const "")
        compile copyFileCompiler

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
       <> teaserField "teaser" "posts"
       <> defaultContext

directoryIndex :: Identifier -> FilePath
directoryIndex = (</> "index.html") . dropExtension . takeFileName . toFilePath

