<meta charset="utf-8">

将作用域写入类型 Encoding scope in types
=======================================

本文的主要受 [魔理沙的一篇文章](https://zhuanlan.zhihu.com/p/22231273?refer=marisa) 启发，精简了内容（去掉了与作用域无关的强类型之类的东西），增加了一些解释。

我们需要一些 GHC 扩展

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> module Something where

本文要求读者对以上类型系统扩展有所了解。事实上，`FlexibleInstances`、`FlexibleContexts` 的作用只是放宽一些限制，`ScopedTypeVariables` 的作用将在下面用到的时候讲解。关于这些扩展，可以阅读 [GHC 文档](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html) 来了解更多。

一个类似摘要的东西
-----------------

我们希望达到的效果，是用类型系统来帮助我们处理匿名函数中变量的作用域问题。我们将构建一个类似无类型 Lambda Calculus 的 EDSL，用多层类型嵌套来表示一层一层的作用域，最终实现将其输出为 SKI 组合子的形式。这样有两个好处：

1. 类型系统阻止我们写出变量作用域对不上的表达式
2. 类型系统帮我们正确地处理变量

效果大概是

> c :: SKI -- 后面定义
> c = lam (\f -> lam (\x -> lam (\y -> f @- y @- x)))

得到的结果如下

```
ghci> c
((S ((S (K S)) ((S ((S (K S)) ((S (K K)) (K S)))) ((S ((S (K S)) ((S ((S (K S)) ((S (K K)) (K S)))) ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) I))))) ((S (K K)) (K I)))))) ((S ((S (K S)) ((S (K K)) (K K)))) (K I)))
```

SKI 组合子演算
-------------

我们考虑这么一件事：将无类型的 Lambda Calculus 翻译成几个有限的组合子的函数应用。设翻译过程为 `T(e)`，则需要考虑这些情况

```
T(a b) = ?
T(\x -> e) = ?
```

第一个很好填

```
T(a b) = T(a) T(b)
```

第二个的话好像有点难办。既然需要函数，我们换个角度，从函数上下文中考虑：

定义 `[x] e` 为函数 `\x -> e` 的组合子表示形式，我们考虑这些情况：

```
[x] x = ?
[x] v = ? -- v 不是 x
[x] (\y -> e) = ?
[x] (a b) = ?
```

然后发现居然是 lambda 情况好写

```
[x] x = ?
[x] v = ? -- v 不是 x
[x] (a b) = ?
[x] (\y -> e) = [x] ([y] e) -- 我们删去了一个 lambda，所以递归一定终止
```

其它的话，就要引入组合子了啊。过程显然是要递归的，先人经过一些尝试后，发现这么递归是好的：

```
[x] x = ?
[x] v = ? v
[x] (a b) = ? ([x] a) ([x] b)
```

只要给三个组合子起名字就好了

```
[x] x = i
[x] v = k v
[x] (a b) = s ([x] a) ([x] b)
```

定义为

```
i x = x
k x y = x
s f g x = f x (g x)
```

验证可知这样的翻译是对的。

我们完成的事情：将无类型的 Lambda Calculus 翻译成 SKI 组合子的函数应用。翻译后没有 lambda 了。so，

**我们成功地将整个无类型 Lambda Calculus 里的 lambda 翻译到不用到 lambda 的 SKI 组合子。**

不是 lambda 的非常好办，所以：

**我们成功地将整个无类型 Lambda Calculus 翻译到不用到 lambda 的 SKI 组合子。**

好办到什么程度呢？下文中我们不管它。

想了解更多的话可以参阅[维基百科 SKI combinator calculus
](https://en.wikipedia.org/wiki/SKI_combinator_calculus) 和 [Combinatory logic](https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis)

说真的，其实你只要理解这个翻译是在做什么就可以了，没必要扣细节，因为细节我们这里不会直接用到。后面再说。

举个例子吧

```
[x] ((x y) x)
= s ([x] (x y)) ([x] x)
= s (s ([x] x) ([x] y)) ([x] x)
= s (s i (k y)) i
```

看起来挺对的。

在 Haskell 中我们如下定义 SKI 组合子

> data SKI
>   = App SKI SKI
>   | S
>   | K
>   | I

> instance Show SKI where
>   show S = "S"
>   show K = "K"
>   show I = "I"
>   show (App u v) = "(" ++ show u ++ " " ++ show v ++ ")"

细说 `[x] e` 形式
----------------

再仔细看看上面的例子。y 是什么？是从外面作用域来的伟大的使者。每通过一次 lambda 的关卡，它就会在外面包一层 k。

```
[x] y = k y
```

`s` 代替了原来 `[x] e` 中 `e` 部分的函数应用，而 `i` 代替了其中的 `x`。很好。

考虑将 `[x] e` 作为一个新的 Lambda Calculus。这里面多定义了一个特殊的东西叫做“那个变量”，记作 `x`。作用域者是也。

有“新”的，说明有旧的。旧的叫做 `repr` 的话，

> newtype Scope repr = Scope { abstract :: repr }

我们直接调用 `repr` 的 SKI 来实现新的 `Scope repr` 中的各种操作。`abstract` 提取出里面放的底层的表示，相当于从 `[x] e` 到 SKI 表示的 `(\x -> e)`。

实现新加进来的“那个变量”。上面说过是 `i`。我们用传入一个 `i` 实现这个操作:

> getVar :: repr -> Scope repr
> getVar iR = Scope iR

把其它不含“那个变量”的表达式从 `repr` 提升到 `Scope repr`，需要 `k` 和底层的函数应用。照样传进来。

> liftConst
>   :: repr
>   -> (repr -> repr -> repr)
>   -> repr
>   -> Scope repr
> liftConst kR appR x = Scope $ kR `appR` x

函数应用，用 `s` 和底层的函数应用

> liftApp
>   :: repr
>   -> (repr -> repr -> repr)
>   -> Scope repr
>   -> Scope repr
>   -> Scope repr
> liftApp sR appR (Scope f) (Scope x) = Scope $ (sR `appR` f) `appR` x

这么写累死人啊！于是自然想到，诶，类型类可不是徒有其名。

用类型类来自动完成操作的提升
--------------------------

> class HasSKI repr where
>   s, k, i :: repr
>   (@-) :: repr -> repr -> repr -- 函数应用
>
> infixl 9 @-

`lift` 啥啥的，不要怕，类型是个好东西

> instance HasSKI SKI where
>   s = S
>   k = K
>   i = I
>   (@-) = App
>
> instance (HasSKI repr) => HasSKI (Scope repr) where
>   s = liftConst k (@-) s
>   k = liftConst k (@-) k
>   i = liftConst k (@-) i
>   (@-) = liftApp s (@-)

嗯就酱。这样我们只要用 `s` 或者 `@-` 这类的就好了，不需要手动 `lift` 来 `lift` 去的。

为了方便我们把 `getVar` 和 `liftConst` 包装一下。

> var :: (HasSKI repr) => Scope repr
> var = getVar i

> suc :: (HasSKI repr) => repr -> Scope repr
> suc = liftConst k (@-)

现在我们已经可以写 lambda 表达式了！

```
ghci> (abstract $ abstract $ abstract $ (suc $ suc var) @- var @- (suc var)) :: SKI
((S ((S (K S)) ((S ((S (K S)) ((S (K K)) (K S)))) ((S ((S (K S)) ((S ((S (K S)) ((S (K K)) (K S)))) ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) I))))) ((S (K K)) (K I)))))) ((S ((S (K S)) ((S (K K)) (K K)))) (K I)))
```

等等。。。啥？

![](ski-scope.png)

比如 `f` 跳过了两个作用域（`x` 的和 `y` 的），所以需要 `k` 两次，也就是 suc 两次。

我们好像不小心发明了 [de Bruijn 标号](https://en.wikipedia.org/wiki/De_Bruijn_index) 的 Lambda Calculus。

自动管理变量跳过的作用域
-----------------------

似乎还离我们的目标差一些呢。

```haskell
c = lam (\f -> lam (\x -> lam (\y -> f @- y @- x)))
```

可以看出的是：

```haskell
lam :: (HasSKI repr) => (? -> Scope repr) -> repr
```

根据上面那个表达式，这里的 `f` `x` `y` 都是 `(suc (suc ... (suc var) ... ))` 的形式。但是我怎么知道它该被 suc 几层呢？

有类型啊！

在 `lam (\x -> e) :: repr` 中，`e :: Scope repr`，如果被用到的时候的类型是 `Scope (Scope repr)`，说明需要多跳过一个作用域，所以应该 `suc` 一次。同理如果是 `Scope (Scope (Scope repr))`，则需要 `suc` 两次。

我们接着来用类型类表示类型间的转换关系。

> class LiftVar u v where
>   liftVar :: u -> v

这三个 `instance` 看起来有点奇怪……请读者尽量忽略奇怪的部分，看重点 orz，那些 `{-# ... #-}` 都不是重点 orz

> instance {-# OVERLAPPABLE #-}
>     (HasSKI v, LiftVar u v) => LiftVar u (Scope v) where
>   liftVar = suc . liftVar

> instance {-# OVERLAPS #-} LiftVar a a where liftVar = id

> -- 这个 instance 纯粹是为了让 GHC 高兴
> instance LiftVar (Scope repr) (Scope repr) where liftVar = id

然后我们只要在传入变量的时候，提前 liftVar 过就可以了。

`v :: Scope repr`，则 `liftVar v :: forall k. LiftVar (Scope repr) k => k`。就这么愉快地决定了

> lam :: forall repr. HasSKI repr
>     => ((forall k. LiftVar (Scope repr) k => k) -> Scope repr) -> repr
> lam f = abstract (f $ liftVar (var :: Scope repr))

我们需要在函数体内（`var :: Scope repr`）用到类型里绑定的类型变量 `repr`。`ScopedTypeVariables` 正好就能达到这个目的。也就是说，这个 `forall`，配上 `ScopedTypeVariables`，使函数体内的 `repr` 和类型签名里的是同一个。这样，`f` 接受到的参数会自动过五关斩六将，不断 `suc` 自己来适应环境。

于是我们写完了？

<blockquote>
It's over. It's finally over.

【好像是刀剑里谁说的，不记得了】
</blockquote>

```
ghci> c
((S ((S (K S)) ((S ((S (K S)) ((S (K K)) (K S)))) ((S ((S (K S)) ((S ((S (K S)) ((S (K K)) (K S)))) ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) I))))) ((S (K K)) (K I)))))) ((S ((S (K S)) ((S (K K)) (K K)))) (K I)))
```

没完呢，还有件事。`c` 的最一般的类型不是 `SKI`，是：

```
ghci> :t lam (\f -> lam (\x -> lam (\y -> f @- y @- x)))
lam (\f -> lam (\x -> lam (\y -> f @- y @- x)))
  :: HasSKI repr => repr
```

也就是说，我们在文章一半的位置不小心发明了 [Finally Tagless](http://okmij.org/ftp/tagless-final/index.html)。

那就这样了。
