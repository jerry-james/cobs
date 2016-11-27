#lang racket/base

(require racket/list
         racket/contract)

(provide
 (contract-out [cobs/encode (-> (listof byte?) (listof byte?))]
               [cobs/decode (-> (listof byte?) (listof byte?))]))

(define (cobs/encode v*)
  (if (empty? v*)
      (error "Can't encode an empty list")
      (f0 (append v* '(0)) '())))

(define (zero-index v*)
  (let k ([v* v*] [i 0])
    (if (empty? v*) #f
        (if (= (first v*) 0) i
            (k (rest v*) (+ i 1)) )))) 

(define (f0 v* c*)
  (if (empty? v*)
      (f4 c* '())
      (f1′′ (+ 1 (zero-index v*)) v* c*)))

(define (f1′′ i v* c*)
  (if (< i #xFF)
      (f0 (drop v* i)    (cons (take v* i)    c*))
      (f0 (drop v* #xFE) (cons (take v* #xFE) c*))))

(define (f4 c* b*)
  (if (empty? c*)
      (apply append b*)
      (f6 (first c*) (rest c*) b*)))

(define (f6 c c* b*)
  (let ([l (length c)])
    (cond
      [(< l #xFE) (f9 (drop-zero c) l c* b*)]
      [(= l #xFE) (f9 c #xFF c* b*)]
      [else (error "Invalid chunk length" l)])))

(define (f9 c l c* b*)
  (f4 c* (cons (cons l c) b*)))




; cobs/decode : listof byte -> listof byte
(define (cobs/decode v*)
  (if (empty? v*)
      (error "Nothing to decode")
      (decode-next-chunk v* '())))

(define (decode-next-chunk v* c*)
  (if (empty? v*)
      (chunks-transform (reverse c*))
      (decode-code-byte (first v*) (rest v*) c*)) )

(define (decode-code-byte head tail c*)
  (case head
    [(#x00) (error "Not allowed")]
    [(#x01) (decode-one       head tail c*)]
    [(#xFF) (decode-big-chunk head tail c*)]
    [else (decode-else      head tail c*)]))

(define (decode-one head tail c*)
  (decode-next-chunk tail (cons (list 0) c*)))

(define (decode-big-chunk head tail c*)
  (decode-next-chunk (drop tail #xFE) (cons (take tail #xFE) c*)))

(define (decode-else head tail c*)
  (let ([v* (drop tail (- head 1))]
        [chunk (take tail (- head 1))])
    (append-chunk-to-chunks v* chunk c*)))

(define (append-chunk-to-chunks v* c c*)
  (decode-next-chunk v* (cons (append c '(0)) c*)))

(define (chunks-transform c*)
  (drop-zero? (apply append c*)))

(define (drop-zero? lst)
  (if (= 0 (list-ref lst (- (length lst) 1)))
      (drop-zero lst)
      list))

(define (drop-zero lst)
  (take lst (- (length lst) 1)))



