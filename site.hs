--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           GHC.IO.Encoding
import           Network (withSocketsDo)
import           Hakyll
import           Data.Monoid ((<>))
import           Data.List (isPrefixOf)
import           Data.Text (pack, unpack, replace, empty)


--------------------------------------------------------------------------------
main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  withSocketsDo $ hakyll $ do

    -- Assets & resources
    match ("assets/fonts/**" .||. "assets/img/**") $ do
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
            let pageCtx = constField "sectionTitle" "Recent posts" <> constField "posts" list <> defaultContext
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
            >>= (externalizeUrls $ feedRoot myFeedConfiguration)
            >>= saveSnapshot "content"
            >>= (unExternalizeUrls $ feedRoot myFeedConfiguration)
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
            let titleFields = constField "title" "Blog" <> constField "sectionTitle" "Blog"
            let archiveCtx = tagCloudField "tags" 100 200 tags <> titleFields <> constField "posts" list <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/postList.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" (titleFields <> defaultContext)
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            let title = "Posts tagged \"" ++ tag ++ "\""
            let titleFields = constField "title" title <> constField "sectionTitle" title
            let archiveCtx = titleFields <> constField "posts" list <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/postList.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" (titleFields <> defaultContext)
                >>= relativizeUrls

    -- Atom feed
    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = bodyField "description" <> postCtx
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots postsPattern "content"
            renderAtom myFeedConfiguration feedCtx posts

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postsPattern :: Pattern
postsPattern = "posts/*.md"

postCtx :: Context String
postCtx =
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

--------------------------------------------------------------------------------
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "dsferruzza"
    , feedDescription = "David Sferruzza's blog"
    , feedAuthorName  = "David Sferruzza"
    , feedAuthorEmail = "david.sferruzza@gmail.com"
    , feedRoot        = "http://david.sferruzza.fr"
    }

externalizeUrls :: String -> Item String -> Compiler (Item String)
externalizeUrls root item = return $ fmap (externalizeUrlsWith root) item

externalizeUrlsWith :: String -- ^ Path to the site root
                    -> String -- ^ HTML to externalize
                    -> String -- ^ Resulting HTML
externalizeUrlsWith root = withUrls ext
  where
    ext x = if isExternal x then x else root ++ x

unExternalizeUrls :: String -> Item String -> Compiler (Item String)
unExternalizeUrls root item = return $ fmap (unExternalizeUrlsWith root) item

unExternalizeUrlsWith :: String -- ^ Path to the site root
                      -> String -- ^ HTML to unExternalize
                      -> String -- ^ Resulting HTML
unExternalizeUrlsWith root = withUrls unExt
  where
    unExt x = if root `isPrefixOf` x then unpack $ replace (pack root) empty (pack x) else x
