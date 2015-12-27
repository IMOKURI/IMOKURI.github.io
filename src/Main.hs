--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Monoid ((<>))
import           Data.List (isInfixOf)
import           Hakyll
import           System.FilePath
import qualified Text.Highlighting.Kate as K

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith hakyllConfig $ do
    match ( "images/*"
       .||. "fonts/*"
       .||. "robots.txt"
       .||. "CNAME"
       .||. "README.md"
       .||. "google393d69a2d334900c.html" ) $ do
        route   idRoute
        compile copyFileCompiler

    create ["css/highlight.css"] $ do
        route   idRoute
        compile $ makeItem (compressCss $ K.styleToCss K.pygments)

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "blog/*/*/*" (fromCapture "tags/*.html")

    match "blog/*/*/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "contents"
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    blog <- buildPaginateWith
        (sortRecentFirst >=> return . paginateEvery 10)
        "blog/*/*/*"
        (\n -> if n == 1
               then fromFilePath "blog"
               else fromFilePath $ "blog/" ++ show n)

    paginateRules blog $ \pageNum pattern -> do
        route   $ customRoute rootDirIndex
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let blogCtx = constField "title" "Blog"
                       <> listField "posts" (postCtx tags) (return posts)
                       <> paginateContext blog pageNum
                       <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= removeIndexHtml
                >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let tagCtx = constField "title" ("Posts tagged " ++ tag)
                      <> listField "posts" (postCtx tags) (return posts)
                      <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" tagCtx
                >>= loadAndApplyTemplate "templates/default.html" tagCtx
                >>= relativizeUrls

    match "index.html" $ do
        route   idRoute
        compile $ do
            posts <- fmap (take 5) . recentFirst =<< loadAll "blog/*/*/*"
            tagList <- renderTagList (sortTagsBy caseInsensitiveTags tags)
            let indexCtx = listField "posts" (postCtx tags) (return posts)
                        <> constField "title" "Home"
                        <> constField "taglist" tagList
                        <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "about.markdown" $ do
        route   $ customRoute rootDirIndex
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "404.markdown" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

    create ["sitemap.xml"] $ do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "blog/*/*/*"
            let sitemapCtx = listField "posts" (postCtx tags) (return posts)
                          <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    create ["feed/rss.xml"] $ do
        route   idRoute
        compile $ do
            let feedCtx = postCtx tags
                       <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "blog/*/*/*" "contents"
            renderRss feedConfig feedCtx posts

    create ["feed/atom.xml"] $ do
        route   idRoute
        compile $ do
            let feedCtx = postCtx tags
                       <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "blog/*/*/*" "contents"
            renderAtom feedConfig feedCtx posts

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = dateField "date" "%B %e, %Y"
            <> dateField "simple-date" "%Y-%m-%d"
            <> tagsField "tags" tags
            <> teaserField "teaser" "contents"
            <> constField "host" (feedRoot feedConfig)
            <> defaultContext

rootDirIndex :: Identifier -> FilePath
rootDirIndex = (</> "index.html") . dropExtension . toFilePath

removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item
  where
    removeIndexStr url | takeFileName url == "index.html" && isNotUrl url = dropFileName url
                       | otherwise                                        = url
    isNotUrl = not . isInfixOf "://"

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Wake up! Good night* - 最近の投稿"
    , feedDescription = "インフラ系SEのまとめノート"
    , feedAuthorName  = "IMOKURI"
    , feedAuthorEmail = ""
    , feedRoot        = "https://imokuri123.com"
    }

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
    { previewHost = "0.0.0.0"
    }

