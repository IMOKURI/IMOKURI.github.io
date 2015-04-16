--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
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
            >>= saveSnapshot "contents"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    blog <- buildPaginateWith
        (sortRecentFirst >=> return . paginateEvery 10)
        "posts/*/*/*"
        (\n -> if n == 1
               then fromFilePath "blog/index.html"
               else fromFilePath $ "blog/" ++ show n ++ "/index.html")

    paginateRules blog $ \pageNum pattern -> do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let blogCtx = constField "title" "Blog"
                       <> listField "posts" postCtx (return posts)
                       <> paginateContext blog pageNum
                       <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls

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
        route   $ customRoute rootDirIndex
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

    create ["sitemap.xml"] $ do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*/*/*"
            let postCtx' = postCtx <> constField "host" (feedRoot feedConfig)
                sitemapCtx = listField "posts" postCtx' (return posts)
                          <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    create ["feed/rss.xml"] $ do
        route   idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*/*/*" "contents"
            renderRss feedConfig feedCtx posts

    create ["feed/atom.xml"] $ do
        route   idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*/*/*" "contents"
            renderAtom feedConfig feedCtx posts

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
       <> teaserField "teaser" "contents"
       <> defaultContext

rootDirIndex :: Identifier -> FilePath
rootDirIndex = (</> "index.html") . dropExtension . takeFileName . toFilePath

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Wake up! Good night* - 最近の投稿"
    , feedDescription = "インフラ系SEのまとめノート"
    , feedAuthorName  = "IMOKURI"
    , feedAuthorEmail = ""
    , feedRoot        = "http://imokuri123.com"
    }

