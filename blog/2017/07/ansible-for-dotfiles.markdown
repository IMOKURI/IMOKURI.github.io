---
date: 2017-07-12 19:00:00
slug: ansible-for-dotfiles
title: AnsibleでDotfilesをインストール
tags: ansible,dotfiles
---

DotfilesのインストールをAnsibleのPlaybookにしました。

[dotfiles](https://github.com/IMOKURI/dotfiles)

デメリットはもちろんAnsibleがインストールされていないといけないことなので、ここはスクリプトでやることにしました。

Ansibleにするメリットは、任意のパートを実行しやすいことと、冪等性が保たれていることです。また、dotfilesをデプロイする前に、いろいろパッケージをダウンロードしたり、インストールしたりするのが、意外と手間だったので、それを自動化したい思いもありました。


<!--more-->

はじめに[インストーラー](https://github.com/IMOKURI/dotfiles/blob/master/install)をキックします `bash -c "$(curl -fsSL https://git.io/imokuri)"` 。 この時、Proxyが必要な環境では、環境変数を設定しておきます。環境変数でProxyが設定されている場合は、AnsibleでもProxyを使用するようになっています。

キックされたインストーラーでAnsibleをインストールして、[Playbook](https://github.com/IMOKURI/dotfiles/blob/master/setup.yaml)をキックします。

デプロイ対象のdotfilesはレポジトリにあるファイルのうち、「デプロイしないもの」を設定して、それ以外をデプロイ（シンボリックリンクを作る）するようにしています。これによって、デプロイ対象が増えても、Playbookを修正する手間がなくなります。

``` yaml
- name: Find Dotfiles Path
  find:
      paths: ~/.dotfiles
      patterns: "^(?!(\\.|README.md|LICENSE|install|setup.yaml)).*$"
      use_regex: true
  register: find
  tags: deploy
```

シンボリックリンクを作成する際は、正規表現を使って、走査したパスからファイル名を取得しています。

``` yaml
- name: Create Symbolic Links To Dotfiles
  file:
      src: "~/.dotfiles/{{ item.path | regex_replace('^.*/([^/]+)$', '\\1') }}"
      dest: "~/.{{ item.path | regex_replace('^.*/([^/]+)$', '\\1') }}"
      state: link
      force: true
  with_items:
      - "{{ find.files }}"
  tags: deploy
```

これで、インストール直後に、コマンド一発でいつもの環境になりました！


