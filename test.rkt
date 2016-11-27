#lang racket
(module+ test
  (require cobs)
  (require rackunit)
  
  (let* ([input         '(#x45 #x00 #x00 #x2c #x4c #x79 #x00 #x00 #x40 #x06 #x4f #x37)]
         [expected '(#x02 #x45 #x01 #x04 #x2c #x4c #x79 #x01 #x05 #x40 #x06 #x4f #x37)]
         [actual (cobs/encode input)])
    (check-equal? actual expected) )

  (let* ([input    '(#x02 #x45 #x01 #x04 #x2c #x4c #x79 #x01 #x05 #x40 #x06 #x4f #x37)]
         [expected      '(#x45 #x00 #x00 #x2c #x4c #x79 #x00 #x00 #x40 #x06 #x4f #x37)]
         [actual (cobs/decode input)])
    (check-equal? actual expected) )
  
  (let* ([input (range 1 #xFF)]
         [expected (append '(#xFF) (range 1 #xFF) '(1))]
         [actual (cobs/encode input)])
    (check-equal? actual expected) )
  
  (let* ([input (range 1 #x100)]
         [expected (append (cons #xFF (range 1 #xFF)) (cobs/encode (range #xFF #x100)))]
         [actual (cobs/encode input)])
    (check-equal? actual expected) ) 
  
  (let ([v '(8 0 3 3 8 9 0 9 8 0)])
    (check-equal? (cobs/decode (cobs/encode v)) v))
  
  
  
  )