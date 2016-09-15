    class Language repr where
        app :: repr (a -> b) -> repr a -> repr b
        -- More coming soon

考虑什么叫“变量抽象”。一个 `Next repr a b` 表示一个基于 repr 的新的表达式语言，总体的类型是 `b`，而其中有一个变量 `v` 在外部环境中被绑定，绑定的类型是 `a`。我们可以把它看作是一个有“洞”的表达式，可以填一个 a 进去。如在 `\(x :: u) -> f x 2` 中，子表达式 `(f x 2)` 可以用一个 `Next repr u v` 来表示，其中我不知道 `repr` 和 `v` 是什么，但是它们并不重要。

为了方便，以下用 `[x :: a] E` 来表示表达式 `E` 里面出现的，没有被其它地方绑定的 `x` 代表的是一个类型为 `a` 的“洞”，如上文中的子表达式 `[x :: u] (f x 2)`。出现在代码里时，`{E}` 表示 `E` 不是一段真正的 Haskell 代码，而是思路的简记。

我们需要有以下组合子

    lam :: ... => Next repr a b -> repr (a -> b)

既然挖洞，我们就可以对这个变量进行抽象得到一个函数 `h`。`lam { [x :: a] E } = { \(x :: a) -> E }`

    instance ... => Language (Next repr a)

`app` 的效果就是在 `[x :: a] E` 中的 `E` 部分工作，如 `app { [x :: a] M } { [x :: a] N } = { [x :: a] M N }`，把 `M` 和 `N` 给 `app` 到了一起。

    var :: ... => Next repr a a

既然绑定了这个变量，我们需要能把这个变量拿到。`var = { [x :: a] x }`

    conv :: ... => repr b -> Next repr a b

如果我们在 `[x :: a] E` 中的 `E` 部分只能用 `x` 组合东西可就不好玩了。但是如果 `E` 中并没有出现 `x` 的话，似乎在环境中多出一个 `x` 不会对它有什么影响？于是我们就有了 `conv {E} = { [x :: a] E }`

好好好，该怎么实现呢？

首先确定类型。我们很容(qi)易(guai)地想到：

    newtype Next repr a b = Next { lam :: repr (a -> b) }

于是

    lam :: Next repr a b -> repr (a -> b)

我们 cheat 出了 lam，其它的能实现么?

回忆一下前面，我们是怎么构建出这三种表达式的？

- `{ [x] y } = conv { y }` （当 `y` 中并没有出现未被绑定的 `x`）
- `{ [x] x } = var`
- `{ [x] (M N) } := app { [x] M } { [x] N }`

但是由上面的 `newtype` 知，其实 `Next repr a b` 中的 `[x :: a] E` 就是 `repr` 中的 `\(x :: a) -> E`。如果我们能在 `repr` 里面实现 `s` `k` `i`，使得：

- `k y = \_ -> y`
- `i = \x -> x`
- `s (\x -> m) (\x -> n) = (\x -> m n)` （`m` `n` 中可以出现 在 "`\x ->`" 部分绑定的 `x`）

从左往右看，分别对应三个情况，不就好了么？

注意到 `s` `k` `i` 都可以直接 `conv` 到 `Next`。

    class SKI repr where
        app :: repr (a -> b) -> repr a -> repr b
        s :: repr ( (a -> b -> c) -> (a -> b) -> a -> c )
        k :: repr ( a -> b -> a )
        i :: repr ( a -> a )
    
    instance (SKI repr) => SKI (Next repr a) where
        app f x = (s `app` f) `app` x
        
        s = conv s -- conv defined below
        k = conv k
        i = conv i
    
    var :: (SKI repr) => Next repr a a
    var = Next i
    
    conv :: (SKI repr) => repr b -> Next repr a b
    conv x = Next (k `app` x)

测试一下

    c :: (SKI repr) => repr ((a -> b -> c) -> b -> a -> c)
    c = lam (lam (lam (app (app (conv $ conv var) var) $ conv var)))

这里的 `conv` `var` 什么的我好晕啊。。什么奇怪的东西

`lam (lam (lam ???))` 中，如果我在 `???` 里写一个 `var`，是哪个 `lam` 的？最里面那个。`conv var` 是从里往外第二个，`(conv $ conv var)` 从里往外第三个，以此类推。

Why?

> RTFT (Read The Types)

    ghci> :t var
    var :: SKI repr => Next repr a a
    
    ghci> :t lam var
    lam var :: SKI repr => repr (b -> b)
    
    ghci> :t lam (conv var)  -- *
    lam (conv var) :: SKI repr => Next repr b (a -> b)
    
    ghci> :t lam (lam (conv var))
    lam (lam (conv var)) :: SKI repr => repr (b -> a -> b)

注意标 `*` 的第三个。这里的 var 在哪里？它在 `lam` 外面。还记得 `conv` 么？

    conv {E} = {[x :: a] E}

本质上来说，`conv` 一层，跳过一个 `lam`。标 `*` 的那个里面跳过了一个 `lam`，所以是往外第二个 `lam` 的 `var`。如果有 27 个 `conv`，就会跳过 27 个 `lam`，就会是从里往外第 28 个 `lam` 的 `var`。

先辈出场。我们刚刚定义 `[x :: a] E` 的那个东西叫做 [Bracket abstraction](https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis)（不是正好的位置，从 6 条规则和 3 条规则之间夹的那段开始）。

没有什么问题是 Bracket abstraction 不能解决的。如果有，嵌套几层 Bracket abstraction。

Bracket abstraction 表示的 SKI 组合子的三个变换规则

- `{ [x] y } = K { y }` （当 `y` 中并没有出现未被绑定的 `x`）
- `{ [x] x } = I`
- `{ [x] (M N) } := S { [x] M } { [x] N }`

所以我们如果能在 `repr` 上实现 `S` `K` `I`，我们就能在 `Next repr a` 上实现 Bracket abstraction，而 lambda 也好办。
