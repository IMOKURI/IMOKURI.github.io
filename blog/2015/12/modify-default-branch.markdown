---
date: 2015-12-25 18:00:00
slug: modify-default-branch
title: Github PagesのDefault branchを変えてみた。
tags: github,github-pages,hakyll,travis-ci
---

[GithubのProfileのページ](https://github.com/IMOKURI)で、草が生えないなぁと思っていたのです。  
Github Pagesのレポジトリを結構更新しているのになぁです。

調べてみると、[草を生やすには、Default branchへの更新でないとダメ](https://help.github.com/articles/why-are-my-contributions-not-showing-up-on-my-profile/#commits)と判明。。

であれば、Github PagesのDefault branchを変えてみたらどうだろうか、をやってみました。

<!--more-->

---

このブログはHakyllを使って、作っています。作り方は、「source」ブランチでHakyllのソースを作って、commit & pushすると、Travis CIがbuildして、「master」ブランチにpushしてくれます。

そのため、「master」ブランチへの貢献度は、私 0%、Travis CI君 100%、みたいな感じで、Default branchが「master」だとそりゃ草 生えんわ、って感じです。

もともと、Default branchは、「プルリクなどを受けるベースとなるブランチ」ということで、Hakyllのブログとしては、当然「source」ブランチがデフォルトになって然るべきだろう、ということで、Default branch変更です！

（個人のブログのソースでプルリクなんて考える必要なかと。）

（そりゃいっちゃあおしまいだ）



変更の仕方は簡単で、「Settings」→「Branches」で、「Default branch」を変更すればOKです。

一応、Travis CIでの自動化に影響がないかは確認しました。



これで、また、ちょいちょい草が生えるはず。

