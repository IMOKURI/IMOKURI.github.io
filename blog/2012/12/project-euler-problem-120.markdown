---
comments: true
date: 2012-12-15 18:11:02
layout: post
slug: project-euler-problem-120
title: Project Euler Problem 120
wordpressid: 102
categories: Programming
tags: project-euler,mathematics
---

このブログで、数式とソースコードを書くテストとして、Project Eulerから1問を問いてみようと思います。

[問題はこちら](http://projecteuler.net/problem=120)。

<!--more-->

まずは  $(a-1)^{n}+(a+1)^{n}$ を展開していきます。

展開すると以下のとおり偶数番目の項は相殺されます。

$$(a-1)^{n}+(a+1)^{n}=2a^{n}+2_{n}C_{2}a^{n-2}+\cdots$$

さらに、最後の項以外は、$a^{2}$を項に含むため、

$a^{2}$を法とした剰余は最後の項だけ考えれば良いことになります。

最後の項は、nが偶数の時は、常に2。nが奇数の時は、最後の項は2naとなるため、

$a^{2}$ を法とした2naの最大値が $r_{max}$ となります。

と言った感じで考えて行きまして。

Haskellで書いてみたソースは以下のようになりました。

```haskell
listA :: [Int]
listA = [3..1000]

make2N :: Int -> Int
make2N a = if mod a 2 == 0 then a-2 else a-1

makeResult :: [Int] -> Int
makeResult = foldr (\a acc -> (make2N a)*a + acc) 0

main = print $ makeResult listA
```

プログラマではないので、イケてないかもですが、、そこはご容赦を。。

とりあえず、数式もソースコードも書けました。

ただ、書きながらちょっとイマイチなところもあったので、今後要改善ですね。
