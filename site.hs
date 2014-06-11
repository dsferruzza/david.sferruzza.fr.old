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

    -- Assets & resources
    match "assets/fonts/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Pages that needs to display the post list
    match "pages/index.md" $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            posts <- fmap (take 5) . recentFirst =<< loadAll postsPattern
            let pageCtx = listField "posts" postCtx (return posts) <> defaultContext
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" pageCtx
                >>= relativizeUrls

    -- Other pages
    match "pages/*.md" $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Posts
    match postsPattern $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Archive
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll postsPattern
            let archiveCtx = listField "posts" postCtx (return posts) <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postsPattern :: Pattern
postsPattern = "posts/*.md"

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext
