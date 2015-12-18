---
date: 2015-12-19 00:03:00
slug: how-to-create-blog-with-hakyll-part4
title: Hakyllでブログを作る(実践編4)
tags: haskell,hakyll,github,github-pages
---

さて、[前回](/blog/2015/12/how-to-create-blog-with-hakyll-part3.html)の続きになります。

最終回は、シンタックスハイライト＋αで締めくくりたいと思います。

カスタマイズしたソースは[こちら](https://github.com/IMOKURI/hakyll-blog-example)に公開していますので、適宜ご参照ください。


#### 目次

* [テンプレ展開](/blog/2015/12/how-to-create-blog-with-hakyll-part1.html#テンプレ展開)
* [ページ作成の流れ](/blog/2015/12/how-to-create-blog-with-hakyll-part1.html#ページ作成の流れ)
* [記事一覧を5記事ごとに分割(ページネーション)](/blog/2015/12/how-to-create-blog-with-hakyll-part2.html#ページネーション)
* [RSS/Atom Feed作成](/blog/2015/12/how-to-create-blog-with-hakyll-part2.html#feed作成)
* [「続きを読む」のリンク(ティーザー)](/blog/2015/12/how-to-create-blog-with-hakyll-part3.html#ティーザー)
* [タグ付けされた記事一覧](/blog/2015/12/how-to-create-blog-with-hakyll-part3.html#タグ付け記事一覧)
* [シンタックスハイライト](#シンタックスハイライト)
* [【リンク】github pagesで作ったブログを公開](#ブログ公開)

それでは、さっそくいってみましょう。

<!--more-->

### シンタックスハイライト

pandocでコンパイルしたhtmlのコードブロックには、色付けのためのclassが付与されていますので、そのclassに合わせた色付けのcssが準備できれば良いことになります。

今回は外部パッケージの「[highlighting-kate](https://hackage.haskell.org/package/highlighting-kate)」を使って簡単に実装します。

まずは、パッケージをビルドするため、cabalファイルの「build-depends」に「highlighting-kate」を追加しておきましょう。

highlighting-kateには、いくつかカラーバリエーションが有りますので、お好みを選んで、cssファイルを作成します。

``` {.haskell}
create ["css/highlight.css"] $ do
    route   idRoute
    compile $ makeItem (compressCss $ K.styleToCss K.pygments)
```

> 「K.styleToCss」でお好みのテーマ「K.pygments」をCSSに変換します。  
> 「compressCss」でCSSをぬかりなく圧縮しておきましょう。
> カラーテーマは[こちら](https://hackage.haskell.org/package/highlighting-kate-0.6/docs/Text-Highlighting-Kate-Styles.html)に一覧があります。

CSSファイルが出来上がったので、あとはテンプレートで読み込んでおけばOKです。


### ブログ公開

せっかく作ったブログはぜひ公開したいものです。

そんな時は、Github Pages。(回し者とかではないですが、オススメです)

Github PagesでHakyllで作ったブログを公開するなら、Travis CIと連携して自動化するのが便利です。  
[参考リンク1](http://335g.github.io/posts/2015-08-09-hakyll_travis.html), [参考リンク2](/blog/2015/04/create-github-pages-with-hakyll.html)


### デザインについて

やっぱりブログやるならかっこいいのがいいですよね（笑

Hakyllでは、ブログデザインは、頑張らないといけない感じです（汗  
([ここ](http://katychuang.com/hakyll-cssgarden/gallery/)にちょっとテンプレを公開してくれている人もいます)

とはいえ、流石にゼロから作るのはきついので、フリーのボイラープレートなどを利用するのがいいのかな、と思います。

Hakyllは苦痛じゃないけど、デザインは苦痛だという方、一緒に頑張りましょう（笑


### コメントについて

記事の中でよくわからないとか、もっと説明してほしいとか、はもちろんのこと、

Hakyllでこれどうやるの？的なのもコメントに書いても良いのかなと思ったりします。

質問サイトに書くのももちろん良いと思いますが、直接問いかけることができるのがコメント欄に書くメリットですね。

答えられるかどうかは別問題として。。


#### 参照

[Hakyll setup](http://yannesposito.com/Scratch/en/blog/Hakyll-setup/)  
[Pagination with Hakyll](http://dannysu.com/2015/10/29/hakyll-pagination/)  
[TUTORIAL: SNAPSHOTS, AND HOW TO PRODUCE AN RSS/ATOM FEED](http://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html)  
[TUTORIAL: USING TEASERS IN HAKYLL](http://jaspervdj.be/hakyll/tutorials/using-teasers-in-hakyll.html)  
[Add tags to your Hakyll blog](http://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html)  
[Hakyll tips](http://philopon.github.io/posts/2014/hakyll_tips.html)  

