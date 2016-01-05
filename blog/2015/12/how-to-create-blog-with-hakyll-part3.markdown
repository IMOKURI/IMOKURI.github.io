---
date: 2015-12-19 00:02:00
slug: how-to-create-blog-with-hakyll-part3
title: Hakyllでブログを作る(実践編3)
tags: haskell,hakyll,github,github-pages
---

さて、[前回](/blog/2015/12/how-to-create-blog-with-hakyll-part2.html)の続きになります。

今回は、カスタマイズ後半戦です。

カスタマイズしたソースは[こちら](https://github.com/IMOKURI/hakyll-blog-example)に公開していますので、適宜ご参照ください。


### 目次

* [テンプレ展開](/blog/2015/12/how-to-create-blog-with-hakyll-part1.html#テンプレ展開)
* [ページ作成の流れ](/blog/2015/12/how-to-create-blog-with-hakyll-part1.html#ページ作成の流れ)
* [記事一覧を5記事ごとに分割(ページネーション)](/blog/2015/12/how-to-create-blog-with-hakyll-part2.html#ページネーション)
* [RSS/Atom Feed作成](/blog/2015/12/how-to-create-blog-with-hakyll-part2.html#feed作成)
* [「続きを読む」のリンク(ティーザー)](#ティーザー)
* [タグ付けされた記事一覧](#タグ付け記事一覧)
* [シンタックスハイライト](/blog/2015/12/how-to-create-blog-with-hakyll-part4.html#シンタックスハイライト)
* [【リンク】github pagesで作ったブログを公開](/blog/2015/12/how-to-create-blog-with-hakyll-part4.html#ブログ公開)

それでは、さっそくいってみましょう。

<!--more-->

## ティーザー

ブログでは、ときどき、記事の冒頭部分が見えていて、「続きを読む」的なリンクが付いていることがあります。それもやってみましょう。

まずは、ブログの記事に、「どこまで見せておくか」のしるしとして「\<\!\-\-more\-\-\>」を入れておきます。


ここまでのチュートリアルで、記事の本文は、「\$body\$」の情報で、埋め込まれていました。ティーザーでは、同じように「\<\!\-\-more\-\-\> までの記事」を「\$teaser\$」として埋め込むことになります。

このとき、記事の内容は、前回つかったスナップショットで保存した記事を使います。

``` {.haskell}
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    teaserField "teaser" "content" `mappend`
    defaultContext
```

埋め込まれた情報を使うには、「テンプレートに反映」ということで、「\$teaser\$」をテンプレートに埋め込めば、ちょいだし記事の完成です。

``` {.html}
<ul>
    $for(posts)$
        <li>
            $title$ - $date$
            $teaser$...<a href="$url$">(続きを読む)</a>
        </li>
    $endfor$
</ul>
```


## タグ付け記事一覧

記事にどんな情報が載っているか、をタグとしてつけておくのもよくあります。記事にタグをつけつつ、その一覧ページも作ってみましょう。

付与するタグは、記事の冒頭に書いておきます。

``` {.markdown}
---
title: example post
tags: example, hakyll
---
```

続いて、Haskellのソースの方で、タグのルールを設定します。

``` {.haskell}
tags <- buildTags "posts/*" (fromCapture "tags/*.html")
```

> 「\"posts/\*\"」にマッチする記事からタグ情報を収集して、  
> タグごとのURLは、「tag/*.html」の形にする。  
> 収集したタグの情報は「tags」に入れておく。


ルールが決まって、タグの情報を収集したので、まずは、記事の方にタグの情報を表示してみましょう。

例によって、タグの情報を埋め込むことになります。

``` {.haskell}
postCtx :: Tags -> Context String
postCtx tags =
    dateField "date" "%B %e, %Y"   `mappend`
    teaserField "teaser" "content" `mappend`
    tagsField "tags" tags          `mappend`
    defaultContext
```

「tagsField」を使って埋め込みましょう。この時、タグの情報を「postCtx」の引数に指定する必要があるので、注意しましょう。

埋め込んだ情報を使うためにテンプレートを更新します。

``` {.html}
<div class="info">
    $if(tags)$
        Tags: $tags$
    $endif$
</div>
```

続いて、タグ一覧ページを作りましょう。

``` {.haskell}
tagsRules tags $ \tag pattern -> do
    route   idRoute
    compile $ do
        posts <- recentFirst =<< loadAll pattern
        let tagCtx =
                constField "title" ("Posts tagged " ++ tag)     `mappend`
                listField "posts" (postCtx tags) (return posts) `mappend`
                defaultContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/tag.html" tagCtx
            >>= loadAndApplyTemplate "templates/default.html" tagCtx
            >>= relativizeUrls
```

・・・ページネーションのにそっくりですね♪

ということで、残りは、「tag.html」のテンプレートの準備です。
対象となる記事をリストすればよいので、こんな感じで十分ですね。

``` {.html}
$partial("templates/post-list.html")$
```

おまけで、タグといえば、タグのリストや、タグクラウドも欲しくなります。作り方はほぼ同じなので、ここでは、タグクラウドを作って、トップページに貼り付けておきたいと思います。

``` {.haskell}
match "index.html" $ do
    route   idRoute
    compile $ do
        posts <- fmap (take 3) . recentFirst =<< loadAll "posts/*"
        tagCloud <- renderTagCloud 80.0 120.0 tags
        let indexCtx =
                listField "posts" (postCtx tags) (return posts) `mappend`
                constField "title" "Home"                       `mappend`
                constField "tagcloud" tagCloud                  `mappend`
                defaultContext

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls
```

> タグの情報を読み込み、最小のフォントが80%、最大が120%で、タグクラウドの情報を生成します。 (tagCloud <- renderTagCloud 80.0 120.0 tags)  
> 生成したタグクラウドの情報を「tagcloud」の名前で埋め込んでおきます。 (constField \"tagcloud\" tagCloud)

あとはタグクラウドを表示したい場所に、「\$tagcloud\$」を入れておきましょう。



さて、[次回](/blog/2015/12/how-to-create-blog-with-hakyll-part4.html)、最終回に続きますー。


