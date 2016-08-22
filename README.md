Xn
======
A chat bot for #developerslv@freenode channel.

Linked with lambdabot (Haskell) and clojurebot (Clojure).

Commands
-
<b><i>url</i></b> - shows title tag content of webpage

<b>s/<i>str</i>/<i>subst</i></b> - finds last message by sender and replaces <i>str</i> with <i>subst</i>

<b>!id &lt;msg&gt;</b> - shows <i>msg</i> on channel

<b>!uptime</b> - shows bot uptime

<b>!history<i>[ num]</i></b> - PMs last <i>num</i> messages from channel (default 50)

<b>!ping</b> - shows "pong" on channel

<b>!rand &lt;num&gt;</b> - generates random Integer in range 0..<i>num</i>

<b>!tell &lt;nick&gt; &lt;msg&gt;</b> - saves <i>msg</i> and posts it to <i>nick</i> once he posts anything

<b>!tell &lt;nick&gt;</b> - deletes saved <i>msg</i> to <i>nick</i> from author

<b>!fm &lt;code&gt;</b> - translates from Morse <i>code</i>

<b>!tm &lt;str&gt;</b> - translates <i>str</i> to Morse code

<b>!lb &lt;cmd&gt;</b> - sends <i>cmd</i> to lambdabot and shows response

Some of lambdabot commands are simplified:

  <ul>
    <li><b>&gt; <i>expr</i></b> - execute the expression<br />
        &nbsp;&nbsp;> map (^3) [1..5]<br />
        &nbsp;&nbsp;λ [1,8,27,64,125]
    </li>
    <li><b>:t <i>expr</i></b> - type of expression<br />
        &nbsp;&nbsp;:t foldl (+)<br />
        &nbsp;&nbsp;λ (Num b, Foldable t) => b -> t b -> b
    </li>
    <li><b>@free <i>ident</i></b> - free theorem generator<br />
        &nbsp;&nbsp;@free filter<br />
        &nbsp;&nbsp;λ $map f . filter (g . f) = filter g . $map f
    </li>
    <li><b>@hoogle <i>expr</i></b> - haskell API Search for either names, or types<br />
        &nbsp;&nbsp;@hoogle (a -> Bool) -> [a] -> [a]<br />
        &nbsp;&nbsp;λ Prelude dropWhile :: (a -> Bool) -> [a] -> [a]<br />
        &nbsp;&nbsp;λ Data.List dropWhile :: (a -> Bool) -> [a] -> [a]<br />
        &nbsp;&nbsp;λ Prelude filter :: (a -> Bool) -> [a] -> [a]
    </li>
    <li><b>@pl <i>expr</i></b> - pointless notation of expression<br />
        &nbsp;&nbsp;@pl \a -> a + 1<br />
        &nbsp;&nbsp;λ (1 +)</li>
    <li><b>@pointful <i>expr</i></b> - opposite to <b>@pl</b><br />
        &nbsp;&nbsp;@pointful (foo . bar .) . baz<br />
        &nbsp;&nbsp;λ (\ d j -> foo (bar (baz d j)))
    </li>
    <li><b>@quote <i>nick</i></b> - quote from nick or random<br />
        &nbsp;&nbsp;@quote deficiencies<br />
        &nbsp;&nbsp;λ hoare says: There are two ways of constructing a software design: One way is to make it so simple that there are obviously no deficiencies, and the other way is to make it so complicated that there are no obvious deficiencies. The first method is far more difficult</li>
    <li><b>@src <i>func</i></b> - implementation of function<br />
        &nbsp;&nbsp;@src filter<br />
        &nbsp;&nbsp;λ filter _ []     = []<br />
        &nbsp;&nbsp;λ filter p (x:xs)<br />
        &nbsp;&nbsp;λ &nbsp;&nbsp;| p x       = x : filter p xs<br />
        &nbsp;&nbsp;λ &nbsp;&nbsp;| otherwise = filter p xs
    </li>
    <li><b>@undo <i>expr</i></b> - translate do notation to Monad operators<br />
        &nbsp;&nbsp;@undo do {a <- foo; bar a}<br />
        &nbsp;&nbsp;λ foo >>= \ a -> bar a
    </li>
  </ul>

<b>!cl &lt;cmd&gt;</b> - sends <i>cmd</i> to clojurebot and shows response

<b>!ab &lt;abbr&gt;</b> - replaces abbreviations like "til" or "afaik" with respective phrases
