# cl-lambdacalc

This is a simple library that allows you to define lisp functions using lambda calculus syntax.  Function strings should use the uppercase letter 'A' as the lambda character.  Nested functions may appear in the body of a function (example: `Aa.Ab.(Axyz.xyz)ab(Axy.y)`), and you may shorten arguments into one group (example: `Afx.x` is equivalent to `Af.Ax.x`).

The project includes a number of tests in the cl-lambdacalc-test package.  These may be run with the included script.

## Installation

You can install the library with Quicklisp.

```
(ql:quickload "cl-lambdacalc")
```

## Example

```
> (define-lc-func foo "Ax.Ay.xy")
#<FUNCTION (LAMBDA (X)) {52DF54DB}>

> (funcall (foo (lc-func "Ax.x")) 42)
42

> (funcall (lc-and #'lc-true) #'lc-true)
#<FUNCTION (LAMBDA (X)) {52D34A8B}>

> (lc-true-p (funcall (lc-and #'lc-true) #'lc-true))
T

> (lc-true-p (funcall (lc-and #'lc-true) (lc-not #'lc-true)))
NIL

> (define-lc-func four "Afx.f(f(f(fx)))")
#<FUNCTION (LAMBDA (F)) {52DF583B}>

> (church-numeral-to-number (lc-succ #'four))
5

> (church-numeral-to-number (funcall (lc-expt #'four) #'four))
256
```

## Exported Symbols

- **lc-func func-string** Returns func-string as a lisp function.
- **define-lc-func name func-string** (Macro) Binds the func-string function to the function slot of the symbol name.
- **lc-true x** `Axy.x`
- **lc-false x** `Axy.y`
- **lc-if x** `Axyz.xyz`
- **lc-and x** `Axy.(Axyz.xyz)xy(Axy.y)`
- **lc-or x** `Axy.(Axyz.xyz)x(Axy.x)y`
- **lc-not x** `Ax.(Axyz.xyz)x(Axy.y)(Axy.x)`
- **lc-true-p func** Returns true if func is lc-true, otherwise nil.
- **lc-false-p func** Returns true if func is lc-false, otherwise nil.
- **lc-succ n** `Anfx.f(nfx)`
- **lc-add m** `Amnfx.mf(nfx)`
- **lc-mult x** `Axyz.x(yz)`
- **lc-expt x** `Axy.yx`
- **church-numeral-to-number func** Returns the number equivalent of the church numeral func.
- **number-to-church-numeral num** Returns the church numeral function equivalent of the number num.
