# lambda


A lambda calculus parser and evaluator written in Elm.

Try it: http://grge.github.io/lambda

## Building

Needs Elm 0.19.1 (`npm install -g elm@latest-0.19.1 elm-test`). GitHub Pages serves `index.html`
and the built `static/lambda.js` from `master`.

```
elm make src/App.elm --optimize --output=static/lambda.js
elm-test
```

## TODO:

* Customizeable reduction methods (eta-reduction, applicative, lazy)
* Step-by-step evaluation mode
* Improve debugging by passing lexer offset references through to evaluator
* Parsing shorthand notation \xyz.x y z
* Render Graphical formats
  * http://homepages.cwi.nl/~tromp/cl/diagrams.html
  * http://dkeenan.com/Lambda/
  * http://bntr.planet.ee/lambda/work/visual_lambda.pdf
