---
date: 2015-12-19 00:01:00
slug: how-to-create-blog-with-hakyll-part2
title: Hakyllでブログを作る(実践編2)
tags: haskell,hakyll,github,github-pages
---

さて、[前回](/blog/2015/12/how-to-create-blog-with-hakyll-part1.html)の続きになります。

今回は、テンプレートを離れて、いろいろカスタマイズしていきます。

カスタマイズしたソースは[こちら](https://github.com/IMOKURI/hakyll-blog-example)に公開していますので、適宜ご参照ください。


### 目次

* [テンプレ展開](/blog/2015/12/how-to-create-blog-with-hakyll-part1.html#テンプレ展開)
* [ページ作成の流れ](/blog/2015/12/how-to-create-blog-with-hakyll-part1.html#ページ作成の流れ)
* [記事一覧を5記事ごとに分割(ページネーション)](#ページネーション)
* [RSS/Atom Feed作成](#feed作成)
* [「続きを読む」のリンク(ティーザー)](/blog/2015/12/how-to-create-blog-with-hakyll-part3.html#ティーザー)
* [タグ付けされた記事一覧](/blog/2015/12/how-to-create-blog-with-hakyll-part3.html#タグ付け記事一覧)
* [シンタックスハイライト](/blog/2015/12/how-to-create-blog-with-hakyll-part4.html#シンタックスハイライト)
* [【リンク】github pagesで作ったブログを公開](/blog/2015/12/how-to-create-blog-with-hakyll-part4.html#ブログ公開)

それでは、さっそくいってみましょう。

<!--more-->

## ページネーション

前回確認したテンプレートの中の、「Archives」のページは、過去記事一覧ということで、記事を追加していくと、(たぶん)際限なくリストされていきます。それはそれで気分がいいかもしれませんが、記事の一覧は、ある程度の単位(5個とか10個とか)でページが分割されてほしいものです。それをやってみます。

まずは、「どんなルールでページネーションをするか」を設定します。

``` {.haskell}
archive <- buildPaginateWith
    (sortRecentFirst >=> return . paginateEvery 5)
    "posts/*"
    (\n -> if n == 1
           then fromFilePath "archive.html"
           else fromFilePath $ "archive/" ++ show n ++ ".html")
```

こちらのソースは以下の様な意味になります。

> 記事が新しい順に、5記事で1ページにする。 ((sortRecentFirst >=> return . paginateEvery 5))  
> ページネーションの対象になるのは \"posts/*\" にマッチする記事。  
> ページネーションの1ページ目のURLは http://user.github.io/archive.html  
> 2ページ目以降のURLは http://user.github.io/archive/ページ番号.html


ルールが決まったので、何ページ目にどの記事が入るか、が決まりました。
次に、ページネーションのページごとに、実際のページを組み立てていきます。

``` {.haskell}
paginateRules archive $ \pageNum pattern -> do
    route   idRoute
    compile $ do
        posts <- recentFirst =<< loadAll pattern
        let archiveCtx =
                constField "title" "Archives"            `mappend`
                listField "posts" postCtx (return posts) `mappend`
                paginateContext archive pageNum          `mappend`
                defaultContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
            >>= relativizeUrls
```

> ページネーションのページ番号は 「pageNum」、対象となる記事は 「pattern」に入ります。  
> 「paginateContext」では、ページネーションに必要な「\$nextPageNum\$」や、「\$nextPageUrl\$」などの情報が埋め込まれます。その他の埋め込まれる情報は[こちら](https://github.com/jaspervdj/hakyll/blob/master/src/Hakyll/Web/Paginate.hs#L99-L109)を参照ください。  


組み立てたページを反映させるテンプレート 「archive.html」で、埋め込んだ情報を書いていきましょう。具体的には、各ページネーションのページに、「次のページ」や「最後のページ」のリンクを作っていきます。このとき、「最後のページ」には、その次のページはありませんので、「次のページ」が存在するときのみ、リンクを作成する、となるようif文を使っていきます。


``` {.html}
<div class="pagination">
  $if(firstPageUrl)$
    <a href="$firstPageUrl$">&lt;&lt;&nbsp;First</a>&nbsp;
  $endif$
  $if(previousPageUrl)$
    <a href="$previousPageUrl$">&lt;&nbsp;Previous</a>&nbsp;
  $endif$
  &mdash;
  $currentPageNum$ of $numPages$
  &mdash;
  $if(nextPageUrl)$
    &nbsp;<a href="$nextPageUrl$">Next&nbsp;&gt;</a>
  $endif$
  $if(lastPageUrl)$
    &nbsp;<a href="$lastPageUrl$">Last&nbsp;&gt;&gt;</a>
  $endif$
</div>
```


## Feed作成

HakyllはRSSとAtomのFeedが作成できるようになっています。HakyllでFeedを作ろうとした時、Feedに載る記事は当然HTMLに変換されていて欲しいのですが、記事のページ全体は必要ありません（ナビゲーションバーとか）。そこで、記事の内容をHTMLに変換したあと、記事のページ全体にする（default.htmlのテンプレートを反映させる）前の状態をsnapshotとして保存しておくことができます。


``` {.haskell}
match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
```

> 「\"posts/\*\"」にマッチするドキュメントを、pandocでHTMLに変換し、「post.html」のテンプレートを反映した状態の記事を、  
> 「content」という名前で保存して、そっとしておく。 (saveSnapshot \"content\")  
> その後の処理は継続して、続けていく。

という感じで、保存したスナップショットをFeedを作るときに利用します。

``` {.haskell}
create ["atom.xml"] $ do
    route   idRoute
    compile $ do
        let feedCtx =
                postCtx                 `mappend`
                bodyField "description"
        posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
        renderAtom myFeedConfiguration feedCtx posts
```

> 「loadAllSnapshots」で、「\"content\"」に保存したsnapshotを読み込みます。 (loadAllSnapshots \"posts/\*\" \"content\")  
> 読み込んだ記事を、新しい順に前から10件取得します。 (fmap (take 10) . recentFirst)  
> 取得した10件でAtomフィードのページを作ります。 (renderAtom myFeedConfiguration feedCtx posts)

最後に、「myFeedConfiguration」を準備しておきます。

``` {.haskell}
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "My Hakyll Blog"
    , feedDescription = "Hakyllでブログを作る"
    , feedAuthorName  = "username"
    , feedAuthorEmail = "test@example.com"
    , feedRoot        = "http://user.github.io"
    }
```


ということで、だいぶブログっぽくなってきました。

[次回](/blog/2015/12/how-to-create-blog-with-hakyll-part3.html)は、カスタマイズ後半戦です。


