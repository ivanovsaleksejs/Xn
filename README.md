Xn_pls
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

<b>!lb &lt;cmd&gt;</b> - sends <i>cmd</i> to lambdabot and shows response

Some of lambdabot commands are simplified:

  <ul>
    <li><b>&gt; <i>expr</i></b> - execute the expression</li>
    <li><b>:t <i>expr</i></b> - type of expression</li>
    <li><b>@free <i>ident</i></b> - free theorem generator</li>
    <li><b>@hoogle <i>expr</i></b> - haskell API Search for either names, or types</li>
    <li><b>@pl <i>expr</i></b> - pointless notation of expression<br />
        > @pl \a -> a + 1<br />
        < (1 +)</li>
    <li><b>@pointful <i>expr</i></b> - opposite to <b>@pl</b><br />
        > @pointful (foo . bar .) . baz<br />
        < (\ d j -> foo (bar (baz d j)))
    </li>
    <li><b>@quote <i>nick</i></b> - quote from nick or random</li>
    <li><b>@undo <i>expr</i></b> - translate do notation to Monad operators<br />
        > @undo do {a <- foo; bar a}<br />
        < foo >>= \ a -> bar a
    </li>
    <li><b>@src <i>func</i></b> - implementation of function</li>
  </ul>

<b>!cl &lt;cmd&gt;</b> - sends <i>cmd</i> to clojurebot and shows response

<b>!ab &lt;abbr&gt;</b> - replaces abbreviations like "til" or "afaik" with respective phrases
