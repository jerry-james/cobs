#lang racket

(require cobs)
(require math/statistics)



(define (f0 i n*)
  (if (= i 0)
      n*
      (f1 (- i 1) (random #x100) n*)))

(define (f1 i n n*)
  (f0 i (cons n n*)))

(define (random-list-generator n)
  (f0 n '()))

(random-list-generator 10)

(define (s00 v*)
  (s0 v* (hash)))

(define (s0 v* h)
  (if (empty? v*)
      h
      (s1 (first v*) (rest v*) h)))

(define (s1 v v* h)
  (s2 v v* (hash-ref h v 0) h))

(define (s2 v v* c h)
  (s0 v* (hash-set h v (+ c 1))))

(define (x0 s)
  (x1 (apply + (hash-values s)) s))

(define (x1 t s)
  (* 100.0 (/ (hash-ref s 0) t)))

(define r (s00 (random-list-generator 1000000)))

(x0 r)