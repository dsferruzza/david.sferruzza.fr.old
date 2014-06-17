---
title: New blog with Hakyll!
author: David Sferruzza
lang: en
tags: blog,hakyll,haskell
description: Why I'm opening this blog and how I made it
published: 2014-06-18 00:10:00+02:00
---

## Why?

I had this *« make yourself a decent website »* task on my todo list for more than a year.
As I am more and more interested in what I do, I decided it was time to begin to give back and share some stuff I like.

Also, the *« I should totally write a blog post about that! »* situation happened too many times repeatedly these last weeks.

I think I will mostly write about Computer Science/Programming, and projects I like and/or make.
But who knows...

## How?

The first thing I want to write about is how I made this website/blog.

I used a [Haskell](http://haskell.org/) library called [Hakyll](http://jaspervdj.be/hakyll/).
Hakyll makes it easy to build a static website generator.

There are lots of static website generators[^1], but Hakyll is the best I tried.
It is **fast**, while being **highly customizable**.
And it can handle many cool stuff almost out-of-the-box, including:

- templating
- markdown (and many other formats, because it uses [Pandoc](http://johnmacfarlane.net/pandoc/))
- incremental build (you don't have to rebuild everything on every modification)
- CSS compression
- tags and metadata
- RSS/Atom feeds
- ...

Making a static website by hand can be painful.
To avoid repeating code, people often use dynamic languages (such as PHP).
But this sounds like a waste to me: on each request, the server has to compute the same operations over and over to serve the same page, just because the developer wants DRY[^2] code.
This makes sense when you need to handle user inputs, but that is not always the case.
For that kind of projects, Hakyll is a really nice tool.

You can find the source code here: <https://github.com/dsferruzza/david.sferruzza.fr>

## Some little details

Two static website generators are never exactly the same (that's the point).
So building one involves picking up some stuff from the example site, or from [nice people](http://jaspervdj.be/hakyll/examples.html), and developing the rest.

I will quickly show some of these features I picked up or developed.

I'm still a beginner, so **if you have better solutions, please let me know!**

### Links in feed

As I said, Hakyll is able to generate RSS and Atom feeds[^3].
While generating a post's page, we will tell Hakyll to take a snapshot of the result, just after converting the post's markdown to HTML, and just before applying it to a template.
This snapshot will then be put in the feed.

*But what if a post contains a relative link?*

Well, it will stay relative, and that's what we want for the post's page.
But we don't want that for the feed, because feed readers won't be able to resolve it.

A solution would be to use the `xml:base`[^4] attribute in a root element of the feed.
The feed [rendering function](https://github.com/jaspervdj/hakyll/blob/master/src/Hakyll/Web/Feed.hs#L122) let me think it would be possible to override the [default feed templates](https://github.com/jaspervdj/hakyll/tree/master/data/templates).
But I don't know if it is possible yet, nor how to do it...

So I picked up [another solution](https://github.com/divarvel/blog/blob/master/site.hs#L31-L33) from [Clément Delafargue's blog](http://blog.clement.delafargue.name).
When generating posts' pages, Hakyll will resolve relative links before taking a snapshot for the feed, and then transform them back so that they will be relative in the rendered page (but not in the feed).

I believe that the first solution would be cleaner, but the second works nicely.

### Update date

When I update a blog post, I want the date of the update to appear in the blog's page, but also in the feed (using `<updated>` tags).

Hakyll handles that with a `modificationTimeField`[^5] you can add to your post's context.
But I don't really like the idea of the modification date only be kept on my file system.
I want to be able to clone my blog's Git repository, build it and get exactly the same result every time.
This would not be the case if I use a `modificationTimeField`, because Git don't store files' modification date.

So I created a `updatedField`, which is implemented exactly like `dateField` except that it only tries to get the date from a `updated` field in my content's metadata.

```haskell
updatedField :: String -> String -> Context a
updatedField key format = field key $ \i -> do
    time <- getUpdatedTime locale $ itemIdentifier i
    return $ formatTime locale format time
  where
    locale = defaultTimeLocale

getUpdatedTime :: MonadMetadata m => TimeLocale -> Identifier -> m UTCTime
getUpdatedTime locale id' = do
    metadata <- getMetadata id'
    let tryField k fmt = M.lookup k metadata >>= parseTime' fmt
    maybe empty' return $ msum $ [tryField "updated" fmt | fmt <- formats]
  where
    empty'     = fail $ "getUpdatedTime: " ++
        "could not parse time for " ++ show id'
    parseTime' = parseTime locale
    formats    =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d"
        , "%B %e, %Y %l:%M %p"
        , "%B %e, %Y"
        , "%b %d, %Y"
        ]
```

Now, if I want to indicate that I updated a post, I just need to add something like `updated: 2014-12-01 12:30:00+02:00` in the post's metadata.
It will be synced by Git :) 

### ISO 8601 dates

I wanted to use `<time>`[^6] tags in my posts' pages.
`<time>` tags binds a machine-readable date to a human-readable date.
This means we need a machine-readable date.

I picked up a [quite simple solution](https://github.com/jtanguy/julien.jhome.fr/blob/master/bin/blog.hs#L160) from [Julien Tanguy's blog](http://julien.jhome.fr/).
You just need to add a new `dateField` to the post's context.
This `dateField` would have a *format* argument like that: `(iso8601DateFormat Nothing)`.

### Cabal sandbox

When you install Hakyll following the [official tutorial](http://jaspervdj.be/hakyll/tutorials/01-installation.html), you type `cabal install hakyll`, which installs globally the latest version of Hakyll.
In a previous project, I had some troubles when I started to update this globally-installed Hakyll.

So I decided I will use a sandbox this time.

> Sandboxes ( in cabal > 1.18 ) are self contained environments of Haskell packages.[^7]

My haskell's dependencies (including Hakyll) are written in a `.cabal` file.
Then it's easy:

- `cabal sandbox init` to initialize the sandbox
- `cabal install --dependencies-only` to install dependencies

It seems like, in a near future, Hakyll will generate the `.cabal` file when doing a `hakyll-init my-site`[^8].

### Comments

Of course, it is not possible to make a comment system using Hakyll.
But it is possible to integrate one.

I was lazy so I choose to use Disqus.
As it includes a lot of stuff (Google Analytics, ~ 700 ko of data which is around 200% of this page's size without comments), I don't let it load itself automatically.

I made something *quick and dirty* to do so:

```html
<div id="disqus_thread"><a href="#disqus_thread" onclick="disqus()">Load comments</a></div>

<script type="text/javascript">
	function disqus() {
		// Disqus code
	}
</script>
```

**You should give [Hakyll](http://jaspervdj.be/hakyll) a try!**

[^1]: Someone even made [a website to list them](http://staticsitegenerators.net/)
[^2]: Don't Repeat Yourself
[^3]: See [the official tutorial](http://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html)
[^4]: See <https://developer.mozilla.org/en-US/docs/XML/xml:base>
[^5]: See <http://jaspervdj.be/hakyll/reference/Hakyll-Web-Template-Context.html#v:modificationTimeField>
[^6]: Specification: <http://www.w3.org/TR/html5/text-level-semantics.html#the-time-element>
[^7]: I took this quote from <http://dev.stephendiehl.com/hask/>
[^8]: According to <https://github.com/jaspervdj/hakyll/issues/267>
