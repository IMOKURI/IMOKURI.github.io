--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Monoid ((<>))
import           Data.List (isInfixOf)
import qualified Data.Set as S
import           Data.String.Utils (replace)
import           Hakyll
import           System.FilePath
import qualified Text.Highlighting.Kate as K
import           Text.Pandoc.Options (Extension(Ext_east_asian_line_breaks), readerExtensions)

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
        compile $ customPandocCompiler
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

    paginateRules blog $ \pageNum patt -> do
        route   $ customRoute rootDirIndex
        compile $ do
            posts <- recentFirst =<< loadAll patt
            let blogCtx = constField "title" "Blog"
                       <> listField "posts" (postCtx tags) (return posts)
                       <> paginateContext blog pageNum
                       <> defCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= removeIndexHtml
                >>= relativizeUrls

    tagsRules tags $ \tag patt -> do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll patt
            let tagCtx = constField "title" ("Posts tagged " ++ tag)
                      <> listField "posts" (postCtx tags) (return posts)
                      <> defCtx

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
                        <> defCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "about.markdown" $ do
        route   $ customRoute rootDirIndex
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defCtx
            >>= relativizeUrls

    match "404.markdown" $ do
        route   $ setExtension "html"
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defCtx

    create ["sitemap.xml"] $ do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "blog/*/*/*"
            let sitemapCtx = listField "posts" (postCtx tags) (return posts)
                          <> defCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    create ["feed/rss.xml"] $ do
        route   idRoute
        compile $ do
            let feedCtx = postCtx tags
                       <> teaserField "description" "contents"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "blog/*/*/*" "contents"
            renderRss feedConfig feedCtx posts

    match "templates/*" $ compile templateCompiler


--- Context -----------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = dateField "date" "%B %e, %Y"
            <> dateField "simple-date" "%Y-%m-%d"
            <> tagsField "tags" tags
            <> teaserField "teaser" "contents"
            <> constField "host" (feedRoot feedConfig)
            <> defCtx

defCtx :: Context String
defCtx = boolField "sourcecode" hasSourceCode
      <> defaultContext

hasSourceCode :: Item String -> Bool
hasSourceCode item = "sourceCode" `isInfixOf` itemBody item

--- Routes -----------------------------------------------------------------------------
rootDirIndex :: Identifier -> FilePath
rootDirIndex = replace "\\" "/" . (</> "index.html") . dropExtension . toFilePath

--- Compiler -----------------------------------------------------------------------------
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item
  where
    removeIndexStr url | takeFileName url == "index.html" && isNotUrl url = dropFileName url
                       | otherwise                                        = url
    isNotUrl = not . isInfixOf "://"

customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  let
    defaultExtensions = readerExtensions defaultHakyllReaderOptions
    newExtensions = S.insert Ext_east_asian_line_breaks defaultExtensions
    readerOptions = defaultHakyllReaderOptions { readerExtensions = newExtensions }
  in
    pandocCompilerWith readerOptions defaultHakyllWriterOptions

--- Configuration -----------------------------------------------------------------------------
feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "最近の投稿 - Wake up! Good night*"
    , feedDescription = "インフラ系SEのまとめノート"
    , feedAuthorName  = "IMOKURI"
    , feedAuthorEmail = "nenegi.01mo@gmail.com"
    , feedRoot        = "https://imokuri123.com"
    }

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
    { previewHost = "0.0.0.0"
    }
