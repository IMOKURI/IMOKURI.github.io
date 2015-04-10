--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid ((<>))
import           Hakyll

import           Style.Base

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    create ["css/default.css"] $ do
        route   idRoute
        compile $ makeItem $ compressCss defaultStyle

    match "posts/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
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

    match "pages/about.markdown" $ do
        route   $ setExtension "html" `composeRoutes` gsubRoute "pages/" (const "")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
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

    match "etc/*" $ do
        route   $ gsubRoute "etc/" (const "")
        compile copyFileCompiler

    match "README.md" $ do
        route   idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
       <> teaserField "teaser" "content"
       <> defaultContext

