---
date: 2015-04-29
slug: netconf-basics
title: SNMPに取って代わるYANG/NETCONF(part2 NETCONF基礎編)
categories: Network
tags: yang,netconf,snmp,smi,mib,network
---

[前回](/blog/2015/04/snmp-will-be-replaced-by-yang-netconf.html)、NETCONF／YANGの概要と、SNMP／SMIv2との主な違いについて整理してみました。

今回はNETCONFについて、もう少し踏み込んだ内容を確認していきたいと思います。

用語を整理しておきたいと思います。本記事では、  
ネットワークを管理する側を、NETCONFのクライアント、  
管理されるデバイス側を、NETCONFのサーバー、  
と呼びます。

<!--more-->

**NETCONFでのセッションについて**

NETCONFはセッションベースのプロトコルです。

* セキュリティを考慮したデータ転送方法を必要とし、SSH/TCPは必須、TLS/TCPはオプションで動作します
* データ転送の際は、認証の目的のため、サーバにアクセスするユーザを特定できなければいけません

なぜ、セッションを使用するのでしょうか。

* NETCONFのいくつかのプロシージャー（函数）は複数のプロトコルのオペレーションを必要とします
* セッションが破棄されたり、削除されたりした場合に、自動でトランザクションのロールバックをする必要があります

セッションの流れについて。

* NETCONFでの機能は、「ケーパビリティ(capability) URI」によって特定されます。
* セッションが開始されると、クライアントと、サーバーの間でけーパビリティのリストが交換されます。
* サーバは、自身の持つ全てのケーパビリティのリストを提示するとともに、セッションIDを払い出します。
* クライアントは、自身がサポートするプロトコルのバージョンを提示します。


---


長くなってきましたので、つづきは、また今度。


---

参考：  
[A Guide to NETCONF for SNMP Developers](http://www.ieee802.org/802_tutorials/2014-07/Tutorial_Berman_1407.pdf)

---


