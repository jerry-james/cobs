#lang scribble/manual
@require[@for-label[cobs
                    racket/base]]

@title{cobs}
@author{bab}

@defmodule[cobs]

Package Description Here



@deftogether[(@defthing[cobs/encode (-> (listof byte?) (listof byte?))]
               @defthing[cobs/decode (-> (listof byte?) (listof byte?))])]{
  Returns a sandwich given the right ingredients.
 @codeblock|{
  (let* ([input         '(#x45 #x00 #x00 #x2c #x4c #x79 #x00 #x00 #x40 #x06 #x4f #x37)]
         [expected '(#x02 #x45 #x01 #x04 #x2c #x4c #x79 #x01 #x05 #x40 #x06 #x4f #x37)]
         [actual (cobs/encode input)])
           (check-equal? actual expected) )
  }|

 @codeblock|{
   (let* ([input    '(#x02 #x45 #x01 #x04 #x2c #x4c #x79 #x01 #x05 #x40 #x06 #x4f #x37)]
          [expected      '(#x45 #x00 #x00 #x2c #x4c #x79 #x00 #x00 #x40 #x06 #x4f #x37)]
          [actual (cobs/decode input)])
            (check-equal? actual expected) )
   }|
}