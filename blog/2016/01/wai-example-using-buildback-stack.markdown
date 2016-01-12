---
date: 2016-01-12 21:00:00
slug: wai-example-using-buildback-stack
title: stackのbuildpackを使って、herokuにHaskellのプログラムをリリースする。
tags: haskell,stack,heroku
---

Haskellのプログラムをherokuにリリースしようとして、調べていたところ、[dockerを使ってリリースする方法](http://qiita.com/eielh/items/e52aeee1419ba611a84d)が見つかったのですが、なんだかんだでうまく行かず、一旦断念。それはリベンジすることにして、[buildpackのstack版](https://github.com/mfine/heroku-buildpack-stack)が見つかったので、それを使って、リリースをしてみました。

<!--more-->

準備するものは、通常のstackのプログラム一式と、herokuでの起動プログラムを指定するProcfileです。

buildpackでデプロイすると、起動プログラムはパスの通っている /app/.local/bin にコピーされるので、Procfileでファイルパスの指定は不要です。

`web: <起動プログラム名>`


ソースが準備出来たら、以下の要領で、herokuアプリを作成します。

`heroku create <アプリ名> --buildpack https://github.com/mfine/heroku-buildpack-stack.git`


herokuにpushすると、1回目は依存パッケージを含めたbuildが始まります。2回目以降は、インストール済みのパッケージが使えるので、buildの時間はかなり短縮されます。

`git push heroku master`


今回作ったレポジトリは[こちら](https://github.com/IMOKURI/wai-example-using-buildback-stack)です。

