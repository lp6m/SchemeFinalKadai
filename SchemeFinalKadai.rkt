#lang racket
;Scheme最終課題
;http://www.stdio.h.kyoto-u.ac.jp/~sakura/scheme/8p.html
; convは式の展開を行う関数.ppp は展開された多項式を標準の形で表示する関数.
;> (conv '(^ (+ (* 2 x) y) 3)) 
;    => '((8 x x x) (12 x x y) (6 x y y) (1 y y y))
;> (ppp  (conv '(^ (+ (* 2 x) y) 3)))
;    => 8*x^3  +12*x^2*y +6*x*y^2   +y^3  


;kadai28-1 esort1
;   esort1は，ソートされた2つの単項式をマージして変数をソートする関数である．
;   要するに，2つの単項式の積を求める関数である．
(define (esort1 x y)
  (if (null? x)
      y
       (esort1 (cdr x) (einsert1 (car x) y))))
;   esort2は，ソートされた2つの単項式の列をマージしてソートする関数である．
;   要するに，2つの多項式の和を求めて整理する関数である．
;kadaip28-2 esort2
(define (esort2 x y)
  (if (null? x)
      y
       (esort2 (cdr x) (einsert2 (car x) y))))
;kadaip29
;   展開された多項式のリストを与えられて，それらの積を求める関数
(define (mult x)
   (if (null? x)
       1
       (if (= 1 (length x))
           (car x)
           (mult2 (car x) (mult (cdr x))))))
;kadaip30
;   2つの展開された多項式の積を求める関数
(define (mult2 x y)
   (if (= 1 (length x))
       (map (lambda (q) (econst1 (esort1 (car x) q))) y)
       (esort2 (map (lambda (q) (econst1 (esort1 (car x) q))) y) (mult2 (cdr x) y))))
;kadaip31
;展開された多項式のリストを与えられて，それらの和を求める関数
(define (sum x)
  (sum_sub (allappend x)))
(define (sum_sub x)
  (if (= 1 (length x))
      x
      (esort2 (cons (car x) '()) (sum_sub (cdr x)))))

;kadaip32
;  以下に出てくる展開された多項式を，通常に近い形で表示する関数ppp
(define (ppp x)
   (if (null? x)
       0
       (if (= 1 (length x))
           (ppp_sub1 x)
           (begin (ppp_sub1 (car x)) (ppp_sub2 (cdr x))))))
;展開された多項式を表示（1つめの項用）
;1つめの項は+x^3をx^3と表示しないといけないので例外として作成(kaku_mainの改造版)
(define (ppp_sub1 x)
   (if (equal? 1 (car x))
      (kaku (cdr x))
      (if (equal? -1 (car x))
          (begin (display "-") (kaku (cdr x)))
          (begin (display (car x)) (display "*") (kaku (cdr x))))))
; 展開された多項式を表示（2つめ以降の項用)
(define (ppp_sub2 x)
  (if (null? x)
      0
      (if (= (length x) 1)
          (kaku_main (car x))
          (if (= (length x) 2)
              (begin (kaku_main (car x)) (write-char #\ ) (kaku_main (car(nozoku 1 x))))
              (begin (kaku_main (car x))  (write-char #\ ) (ppp_sub2 (cdr x)))))))
;pppの下請けのmain関数.単項式の表示を行う
;+や-を数字の前につける.+1や-1のときは+や-だけを表示する
(define (kaku_main x)
  (if (equal? 1 (car x))
      (begin (display "+") (kaku (cdr x)))
      (if (equal? -1 (car x))
          (begin (display "-") (kaku (cdr x)))
          (begin (pone (car x)) (display "*") (kaku (cdr x))))))
;整数のリストを渡すと"+"をつけて表示(1,-1でも表示する)
;(pone '(5)) -> +5
;(pone '(-4)) -> -4
(define (pone x);;1,-1は駄目
      (if (> x 0)
          (values (display "+") (display x))
          (display x)))
;渡されたリストの頭から等しい部分を返す
;(onaji '(x x z)) -> '(x x)
;(onaji '(a b)) -> '(a)
(define (onaji x)
  (if (null? x)
      '()
      (if (= 1 (length x))
          x
          (if (equal? (car x) (car (cdr x)))
              (cons (car x) (onaji (cdr x)))
              (cons (car x) '())))))
;指定された個数,リストの要素を頭から削除する
;(nozoku 3 '(a s d f)) -> '(f)
(define (nozoku n lst)
  (if (= n 0)
      lst
      (nozoku (- n 1) (cdr lst))))
;同じ変数だけのリストを渡してべき上の形で表示する
;(kakuone '(a a)) -> a^2
;(kakuone '(b)) -> b
(define (kakuone x)
  (if (null? x)
      '()
      (if (= 1 (length (onaji x)))
          (display (car x))
          (values (display (car x)) (display "^") (display (length (onaji x))) ))))
;*を挿入する
(define (kaku_sub x) 
  (if (null? x)
       (write-char #\ )
       (display "*")))
;同じ変数の部分リストを渡されたとき、それを表示する関数
(define (kaku x)
  (if (null? x)
        (write-char #\ ) 
       (if (= 1 (length x))
            (display (car x))
           (begin (kakuone (onaji x)) (kaku_sub (nozoku (length (onaji x)) x)) (kaku (nozoku (length (onaji x)) x))))))
      
;kadaip33
;LISPのS式で表された多項式を展開する関数,-に対応済み
;   (このプログラムのメインの関数)
;   ただし，S式中の関数の位置には+,*,^のみが現れてもよいものとする．
;
 (define (conv x)
        (cond ((number? x) (list (list x)))
        ((symbol? x) (list (list 1 x)))
        ((eq? (car x) '+) (sum (map conv (cdr x))))
        ((eq? (car x) '*) (mult (map conv (cdr x))))
              ((eq? (car x) '-) (cons (car (sum (map conv (cdr x)))) (minus_exception (minus (cdr (sum (map conv (cdr x))))) )))
        ((eq? (car x) '^) (power (conv (cadr x)) (caddr x)))))
;二番目以降の単項式に-1をかける
(define (minus x)
  (if (= 1 (length x))
      (econst1 (esort1 '(-1) (car x) ))
      (cons (econst1 (esort1 '(-1) (car x))) (cons (minus (cdr x)) '()))))
;例外用
(define (minus_exception x)
  (if (= 1 (length (cons x '())))
      (cons x '())
      x))
;;;;;;;;;;;;;;;;;;;以上自作した関数;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;以下課題で既にあたえられた関数;;;;;;;;;;;;
(define (econst1 x)
  (cond ((null? x) '(1))
        ((number? (car x)) (econst2 (car x) (cdr x)))
        (#t (cons 1 x))))

 (define (econst2 x l)
        (cond ((null? l) (list x))
        ((= x 0) '(0))
        ((number? (car l)) (econst2 (* x (car l)) (cdr l)))
        (#t (cons x l))))

(define (allappend x)
  (if (null? x) '()
  (append (car x) (allappend (cdr x)))))

 (define (einsert1 x l)
        (if (null? l) (list x)
      (let ((r (ecomp? x (car l))))
         (cond ((<= r 0) (cons x l))
         (#t (cons (car l) (einsert1 x (cdr l))))))))

;インサーションを行う関数 einsert2 
(define (einsert2 x l)
        (if (null? l) (list x)
      (let ((r (ecomp? x (car l))))
         (cond ((< r 0) (cons x l))
                     ((= r 0)
          (let ((y (eadd2 x (car l))))
      (if (= (car y) 0) (cdr l)
          (cons y (cdr l)))))
         (#t (cons (car l) (einsert2 x (cdr l))))))))

;2つの単項式の和を求める関数eadd2.両方ともソートされて標準形になっていることを仮定している．
(define (eadd2 x y)
        (cond ((and (number? x) (number? y)) (list (+ x y)))
        ((and (symbol? x) (symbol? y)) (list 2 x))
        ((and (null? x) (null? y)) '(2))
        ((and (null? x) (number? y)) (list (+ 1 y)))
        ((null? x) (list (+ 1 (car y))))
        ((and (null? y) (number? x)) (list (+ 1 x)))
        ((null? y) (list (+ 1 (car x))))
        ((and (number? (car x)) (number? (car y)))
         (cons (+ (car x) (car y)) (cdr x)))
        ((number? (car x)) (cons (+ (car x) 1) (cdr x)))
        ((number? (car y)) (cons (+ (car y) 1) (cdr y)))
        (#t (cons 2 x))))
;xとyが両方とも係数または変数の時,あるいは,両方とも単項式の時,
;比較結果を返す．<のとき-1, =のとき0, >のとき1を返す．ecomp
(define (ecomp? x y)
        (cond ((and (null? x) (null? y)) 0)
        ((null? x) -1)
        ((null? y) 1)
        ((and (number? x) (number? y)) 0)
        ((number? x) -1)
        ((number? y) 1)
        ((and (symbol? x) (symbol? y))
         (cond ((eq? x y) 0)
               ((string<? (symbol->string x) (symbol->string y)) -1)
               (#t 1)))
        ((symbol? x) -1)
        ((symbol? y) 1)
        ((number? (car x)) (ecomp? (cdr x) y))
        ((number? (car y)) (ecomp? x (cdr y)))
        ((eq? (car x) (car y)) (ecomp? (cdr x) (cdr y)))
        ((string=? (symbol->string (car x)) (symbol->string (car y))) 0)
        ((string<? (symbol->string (car x)) (symbol->string (car y))) -1)
        (#t 1)))
;展開された多項式と,自然数nを与えられて,多項式のn乗を求める関数power
 (define (power x n)
        (cond ((= n 0) '((1)))
        (#t (mult2 x (power x (- n 1))))))
