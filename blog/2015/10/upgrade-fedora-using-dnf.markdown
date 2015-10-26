---
date: 2015-10-26
slug: upgrade-fedora-using-dnf
title: DNFを使ってFedoraをアップグレードする（Fedora22->23）
categories: Linux
tags: linux,fedora,dnf
---

Fedora 23から DNF を使って、Fedoraをアップグレードするのが[推奨][1]となるようですので、早速試してみたいと思います。

Fedora 23はこの記事を書いている時点ではベータ版ですが、DNFでのアップグレードができました。

<!--more-->


### まずは、アップグレードに必要なパッケージをインストールします。

```
sudo dnf install dnf-plugin-system-upgrade --enablerepo=updates-testing
```


### 続いて、アップグレードしたいバージョン(今回はFedora 23)のパッケージをダウンロードします。

```
sudo dnf system-upgrade download --releasever=23
```

私の環境では、1600パッケージ（1.2GB！）ほどがダウンロードの対象となりました。


### アップグレードします！

```
sudo dnf system-upgrade reboot
```

OSが再起動して、起動中に先ほどダウンロードしたパッケージがインストールされていきます。所要時間20分ほどでしょうか。


### 無事、アップグレード完了

```
$ cat /etc/fedora-release
Fedora release 23 (Twenty Three)
```


[1]: https://fedoraproject.org/wiki/DNF_system_upgrade

