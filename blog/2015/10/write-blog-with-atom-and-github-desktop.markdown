---
date: 2015-10-27
slug: write-blog-with-atom-and-github-desktop
title: Github DesktopとAtomでブログの記事を書いてみる
tags: github,github-desktop,github-pages,atom
---

このブログはgithub pagesで運営しています。

いつもは、Linuxから記事を書いているのですが、ちょっと趣向をかえて、Github DesktopとAtomを使って、Windows上から記事を書いてみたいと思います。

使ってみたのは、こちらです。

* GitHub Desktop
* Atom
* Atomのパッケージのmarkdown-writer

特に、markdown-writerがそこそこ使えそうなので、さわってみました。

<!--more-->

Github Desktopはインストールして、起動して、github pages用のレポジトリをクローンしておきます。（雑）

Atomもインストールして、起動して、お好みのパッケージとmarkdown-writerを入れておきます。（ry


### markdown-writerの設定

ここからが本題です。

markdown-writerでブログを書きやすくするため、各設定を入れていきます。


まず、 `Site Local Directory` にクローンしたレポジトリのディレクトリを入れます。

Windowsの場合、`C:\Users\<ユーザ名>\Documents\GitHub\<レポジトリ名>\` みたいな感じになるかと思います。

デフォルトのファイルパスは、スラッシュ(/)区切りで書いてありますが、ここはWindowsらしくバックスラッシュ(\\)でOKです。


続いて、 `Site Drafts Directory` や `Site Posts Directory` に上記のディレクトリからの相対パスを入れます。

`drafts\` や `{year}\{month}\` のような感じです。


`New Draft File Name` や `New Post File Name` も好みに合わせて変えておきます。


### 記事を書く

基本的な設定は、ひとまずこのくらいで実際に記事を書いてみます。

Atomで、コマンドパレット(Ctrl+Shift+P)を開き、 `new draft`！で、記事のタイトルなどを入力すると、先ほどの下書き用ディレクトリに、ファイルが出来上がります。

最初に挿入されるテンプレが気に入らなければ、[File]→[Open Your Config]で、以下の様なブロックを追加すれば、変更できます。

```
  "markdown-writer":
    frontMatter: """
    ---
    date: <date>
    slug: <title>
    title: title in japanese
    categories: category
    tags: aaa,bbb,ccc
    ---


    <!--more-->
    """
```

記事を書くにあたっても、markdown-writerには、markdownを書くのに便利なコマンドが用意されているので、コマンドパレットを開いて、ゴニョゴニョ書いていくと、結構スイスイかけます。


### 公開ディレクトリに移す

わざわざ、エクスプローラーを開いて移動する必要はありません。

大活躍のコマンドパレットで `publish draft`！で、先ほどの下書きディレクトリから、公開ディレクトリに移動してくれます。

あとは、Github Desktopで、commitして、syncすればOKです。


注意点として、文字コードをUTF-8にしておかないと、GitHub Desktopで文字化けてしまいます。

Windows環境だと、普段はSJISで書いている人も多いと思いますので、記事を書き始める前に、AtomのワークスペースをUTF-8にしてくださいませ。(ステータスバーの右下の方をクリック。もしくは[Ctrl+Shift+U]からのUTF-8)


快適なブログ書きを～
