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
            itemTpl <- loadBody "templates/postListItem.html"
            list <- applyTemplateList itemTpl postCtx posts
            let pageCtx = constField "title" "Recent posts" <> constField "posts" list <> defaultContext
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

    -- Build tags
    tags <- buildTags postsPattern (fromCapture "tags/*.html")

    -- Posts
    match postsPattern $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (tagsCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Archive
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll postsPattern
            itemTpl <- loadBody "templates/postListItem.html"
            list <- applyTemplateList itemTpl postCtx posts
            let archiveCtx = tagCloudField "tags" 100 200 tags <> constField "title" "Blog" <> constField "posts" list <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/postList.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            let archiveCtx = constField "title" title <> constField "posts" list <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/postList.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postsPattern :: Pattern
postsPattern = "posts/*.md"

postCtx :: Context String
postCtx =
    metadataField <>
    dateField "date" "%B %e, %Y" <>
    defaultContext

tagsCtx :: Tags -> Context String
tagsCtx tags = 
    tagsField "prettytags" tags <>
    postCtx

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postListItem.html"
    posts <- preprocess' =<< loadAll pattern
    applyTemplateList postItemTpl (tagsCtx tags) posts
