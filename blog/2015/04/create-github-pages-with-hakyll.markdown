---
comments: true
date: 2015-04-05 20:07:53
layout: post
slug: create-github-pages-with-hakyll
title: Hakyllを使ってGitHub Pagesを作成して、そのソースも管理して、Travis CIで自動デプロイする。
wordpressid: 421
categories: Programming
tags: github,github-pages,hakyll,haskell,travis-ci
---

[GitHub Pages](https://pages.github.com/)は、レポジトリに配置されたファイルにそって、Webページが生成されます。そのため、`index.html` などは、レポジトリのルートに配置されている必要があります。  

一方、[Hakyll](http://jaspervdj.be/hakyll/)は、公開用の `index.html` などのファイルが、ルートディレクトリではなく、 `_site` というディレクトリ配下に生成されます。





そのため、HakyllのソースのルートディレクトリからGithub Pagesのレポジトリにソースをアップロードしてしまうと、正しくWebページが表示されません。  

そこで、レポジトリのmasterブランチでは、公開用にHakyllで生成したファイル( `_site` 配下の `index.html` など)が配置され、sourceブランチに、Webページ生成元となるソースを含めたファイルを配置するようにレポジトリを作成したいと思います。  

その際、sourceブランチを修正、プッシュしたら、自動でmasterブランチが更新されるよう、[Travis CI](https://travis-ci.org/)を使用して自動化したいと思います。



<!--more-->



* * *





はじめに、GitHub Pagesを作成するレポジトリを作成します。





GitHub Pagesのレポジトリは、 `<account name>.github.io` で[作成します](https://github.com/IMOKURI/IMOKURI.github.io)。





レポジトリ作成の時点では、 `Initialize this repository with a README` はチェック **しない** ことを想定しています。





* * *





まずは、Hakyllをsandboxにインストールして、Webページのソースを生成します。







  * Webページを生成するためのディレクトリを作成。  

`mkdir $HOME/hakyll; cd $HOME/hakyll`


  * sandboxを初期化。  

`cabal sandbox init`


  * Hakyllをインストール（ちょっと時間かかる）。  

`cabal install -j --disable-documentation hakyll`


  * Webページのソースを生成。  

`cabal exec hakyll-init <account name>.github.io`


  * あとで、buildの確認にsandoboxを使いたいので、移動。  

`mv .cabal-sandbox <account name>.github.io/`





* * *





続いて、GitHubのレポジトリに空のmasterブランチを作成します。







  * gitを初期化。  

`git init`


  * remoteブランチを登録。  

`git remote add origin git@github.com:<account name>/<account name>.github.io.git`


  * masterブランチを空コミット。  

`git commit --allow-empty -m "Initial commit"`


  * GitHubのレポジトリにアップロード。  

`git push origin master`





* * *





sourceブランチを作成し、 `_site` ディレクトリをサブモジュールに登録します。







  * sourceブランチを作成。  

`git checkout -b source`


  * `_site` ディレクトリをサブモジュールに登録。  

`git submodule add git@github.com:<account name>/<account name>.github.io.git _site`


  * .gitmodulesファイルをTravisからアクセスできるように修正。  

`vi .gitmodules`  

`url` を `https://github.com/<account name>/<account name>.github.io.git` に修正。


  * .gitignoreファイルを作成。  

`vi .gitignore`




```    
.cabal-sandbox
cabal.sandbox.config
dist/
_cache
_site
```






  * Hakyllのソースをbuild確認。  

`cabal sandbox init`  

`cabal run -j build`





* * *





Travis CIがGitHubのレポジトリを更新できるようTokenを取得します。







  * GitHubの[Tokenを生成するページ](https://github.com/settings/applications)にアクセス。


  * `Generate new token` をクリック。


  * `Select scopes` は、デフォルトのに追加して、 `read:org` 、 `write:repo_hook` をチェック。


  * 生成されたTokenは控えておく。





* * *





Travis CIでGitHubと連携する設定を追加します。







  * [Travis CIのアカウント管理画面](https://travis-ci.org/profile/)で、GitHubのレポジトリの情報を取得。  

`sync` ボタンをクリック。  

`<account name>/<account name>.github.io` のスイッチをON。  

歯車アイコンを押して、設定画面へ。  

`Build only if .travis.yml is present` のスイッチをON。





* * *





TokenをTravisの設定に追加するため、暗号化します。







  * Rubyのgemが必要なのでインストール。  

`sudo yum install rubygems ruby-devel`


  * Travisをインストール。  

`sudo gem install travis`


  * Tokenの情報と、メールアドレスの情報を暗号化。情報を控える。  

`travis encrypt -r <account name>/<account name>.github.io GH_EMAIL=<your email>@<your domain>.<tld>`  

`travis encrypt -r <account name>/<account name>.github.io GH_TOKEN=<the token that is used to access github>`





* * *





Travis CIとの連携を `.travis.yml` に記載します。







  * `.travis.yml` ファイルを作成。  

`vi .travis.yml`



```yaml
# NB: don't set `language: haskell` here

# See also https://github.com/hvr/multi-ghc-travis for more information

branches:
  only:
    - source

env:
  global:
    - GH_NAME="Travis on behalf of IMOKURI"
    - secure: "<text that encrypts your email address>"
    - secure: "<text that encrypts github token>"

  matrix:
    - CABALVER=1.20 GHCVER=7.8.3
#    - CABALVER=1.22 GHCVER=7.10.1
#    - CABALVER=head GHCVER=head

before_install:
  - travis_retry sudo add-apt-repository -y ppa:hvr/ghc
  - travis_retry sudo apt-get update
  - travis_retry sudo apt-get install cabal-install-$CABALVER ghc-$GHCVER
  - export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH

  - git submodule foreach --recursive 'git checkout master; git ls-files | grep -v README | grep -v CNAME | xargs -r git rm'

install:
  - travis_retry cabal update
  - cabal sandbox init
  - cabal install --only-dependencies --disable-documentation
  - cabal configure --disable-library-profiling --disable-tests --disable-library-coverage --disable-benchmarks --disable-split-objs

before_script:
  - git config --global user.name "$GH_NAME"
  - git config --global user.email "$GH_EMAIL"

script:
  - cabal run -j build

after_success:
  - if [[ "$TRAVIS_BRANCH" == "source" ]]; then
    cd _site;
    export REMOTE=$(git config remote.origin.url | sed 's/.*:\/\///');
    git remote add github https://${GH_TOKEN}@${REMOTE};
    git add --all;
    git status;
    git commit -m "Built by Travis ( build $TRAVIS_BUILD_NUMBER )";
    git push github master:master | grep -v http;
    fi
```
    



* * *





時は来た。。







  * すべてのファイルをコミットしてsourceブランチにプッシュ。  

`git add --all`  

`git commit -m "Add source"`  

`git push origin source`


  * [Travis CIのbuildの様子](https://travis-ci.org/repositories)を見守る。


  * GitHubのmasterブランチが更新されたか確認。


  * GitHub Pages(.github.io)に反映されたか確認。





* * *






参考:  

[Create a static site with Hakyll, Github and Travis CI](http://begriffs.com/posts/2014-08-12-create-static-site-with-hakyll-github.html)  

[Hakyll, Github and building a static site with Travis CI](http://timbaumann.info/posts/2013-08-04-hakyll-github-and-travis.html)



