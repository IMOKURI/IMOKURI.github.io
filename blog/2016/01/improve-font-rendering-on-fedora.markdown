---
date: 2016-01-27 19:00:00
slug: improve-font-rendering-on-fedora
title: FedoraでChromeのフォントをきれいにする
tags: fedora,linux,font
---

普段遣いのパソコンをWindowsからLinux(Fedora)に入れ替えました。日頃仕事で、LinuxはRedhatを使用することが多く、使いなれているので、Fedoraを選びました。ゆうゆう使い始めたのも束の間、Chromeのフォントが汚い。。。そこで以下の対処をしました。

<!--more-->

1. RPM Fusionをインストールする。
    * デフォルトのレポジトリには含まれていないアプリケーションが含まれているレポジトリです。
    * [こちら](http://rpmfusion.org/Configuration)からレポジトリのRPMをダウンロードして、 `dnf install <rpm名>` のような感じでインストールします。
2. 「freetype-freeworld」というフォントレンダリングエンジンをインストールします。 `dnf install freetype-freeworld`
3. `/etc/fonts/local.conf` に以下の設定を追加します。

``` xml
<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
  <match target="font">
    <edit name="antialias" mode="assign">
      <bool>true</bool>
    </edit>
    <edit name="autohint" mode="assign">
      <bool>false</bool>
    </edit>
    <edit name="embeddedbitmap" mode="assign">
      <bool>false</bool>
    </edit>
    <edit name="hinting" mode="assign">
      <bool>true</bool>
    </edit>
    <edit name="hintstyle" mode="assign">
      <const>hintslight</const>
    </edit>
    <edit name="lcdfilter" mode="assign">
      <const>lcdlight</const>
    </edit>
    <edit name="rgba" mode="assign">
      <const>rgb</const>
    </edit>
  </match>

<!-- Set preferred serif, sans serif, and monospace fonts. -->
  <alias>
    <family>serif</family>
    <prefer><family>フォント名</family></prefer>
  </alias>
  <alias>
    <family>sans-serif</family>
    <prefer><family>フォント名</family></prefer>
  </alias>
  <alias>
    <family>sans</family>
    <prefer><family>フォント名</family></prefer>
  </alias>
  <alias>
    <family>monospace</family>
    <prefer><family>フォント名</family></prefer>
  </alias>
</fontconfig>
```

---

2016/1/29 追記：

「フォント名」のところは、好みのフォントを指定します。フォントファイルは、 `/usr/share/fonts/` ディレクトリに、フォント名のフォルダを作成して格納しておきます。

---

[Trying Fedora 23 for the first time. Why do fonts suck so bad?](https://www.reddit.com/r/Fedora/comments/3o6ijr/trying_fedora_23_for_the_first_time_why_do_fonts/)
