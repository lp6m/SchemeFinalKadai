SchemeFinalKadai
================
<http://www.stdio.h.kyoto-u.ac.jp/~sakura/scheme/8p.html>の解答。汚いけど.  

`> (conv '(^ (+ (* 2 x) y) 3)) `  
`    => '((8 x x x) (12 x x y) (6 x y y) (1 y y y))`  
`> (ppp  (conv '(^ (+ (* 2 x) y) 3)))`  
`    => 8*x^3  +12*x^2*y +6*x*y^2   +y^3 `  
  
のように式を展開できる.
