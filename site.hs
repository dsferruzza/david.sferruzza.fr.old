--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           GHC.IO.Encoding
import           Network (withSocketsDo)
import           Hakyll
import           Data.Monoid ((<>))


--------------------------------------------------------------------------------
main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  withSocketsDo $ hakyll $ do

    match "assets/fonts/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "pages/index.md" $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*.md"
            let pageCtx = listField "posts" postCtx (return posts) <> defaultContext
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" pageCtx
                >>= relativizeUrls

    match "pages/*.md" $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "posts/*.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext
