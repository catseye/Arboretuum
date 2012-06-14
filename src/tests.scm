;
; Test suite for forest-rewriting project
; Chris Pressey, sometime late January 2006
;

; Copyright (c)2008 Cat's Eye Technologies.  All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
;
; 1. Redistributions of source code must retain the above copyright
;    notices, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notices, this list of conditions, and the following disclaimer in
;    the documentation and/or other materials provided with the
;    distribution.
; 3. Neither the names of the copyright holders nor the names of their
;    contributors may be used to endorse or promote products derived
;    from this software without specific prior written permission. 
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.


(load "utils.scm");--------------------------------------------------------

(test 'split-1
  (lambda ()
    (split '(1 2 3 4 5 6 7 8) '() '()))
  '((1 3 5 7) . (2 4 6 8))
)

(test 'split-2
  (lambda ()
    (split '(1 2 3 4 5 6 7) '() '()))
  '((1 3 5 7) . (2 4 6))
)

(test 'merge-1
  (lambda ()
    (merge '(2 4 6 8) '(1 3 5 7) > '()))
  '(1 2 3 4 5 6 7 8)
)

(test 'split-and-merge-1
  (lambda ()
    (let* ((pair (split '(1 2 3 4 5 6 7) '() '()))
           (left (car pair))
           (right (cdr pair)))
      (merge left right > '())))
  '(1 2 3 4 5 6 7)  
)

(test 'mergesort-1
  (lambda ()
    (mergesort '(8 26 4 78 13 65 12 91 64 2) >))
  '(2 4 8 12 13 26 64 65 78 91)
)

(load "unify.scm");--------------------------------------------------------

(test 'unify-1
  (lambda ()
    (unify
      '(+ 1 2)
      '(+ #(a) #(b))
      '()
    ))
  '((b . 2) (a . 1))
)

(test 'unify-2
  (lambda ()
    (unify
      '(+ 1 1)
      '(+ #(a) #(a))
      '()
    ))
  '((a . 1))
)

(test 'unify-3
  (lambda ()
    (unify
      '(+ 1 2)
      '(+ #(a) #(a))
      '()
    ))
  #f
)

(test 'unify-4
  (lambda ()
    (unify
      '(+ 1 1)
      '(+ #(a) #(a))
      '((a . 2))
    ))
  #f
)

(load "tree-rewrite.scm");--------------------------------------------------

(test 'reduce-term-1
  (lambda ()
    (reduce-term
      '(+ 6 (+ 3 3))
      '(
          ((+ #(A) #(A)) . (* #(A) 2))
       )
    ))
  '(+ 6 (* 3 2))
)

(test 'reduce-term-2
  (lambda ()
    (reduce-term
      '(+ (const 9) (* (const 2) (const 3)))
      '(
           ((const #(a))                  . (push #(a) _)                    )
           ((+ #(l) #(r))                 . (then (then #(l) #(r)) (add _))  )
           ((* #(l) #(r))                 . (then (then #(l) #(r)) (mul _))  )
           ((then _ #(c))                 . #(c)                             )
           ((then (#(op) #(a)) #(b))      . (#(op) (then #(a) #(b)))         )
           ((then (#(op) #(k) #(a)) #(b)) . (#(op) #(k) (then #(a) #(b)))    )
       )
    ))
  '(push 9 (push 2 (push 3 (mul (add _)))))
)

(test 'reduce-term-3
  (lambda ()
    (reduce-term
      '(+ (const 9)
         (if (> (const 3) (const 2))
                (* (const 2) (const 3))
                (const 1)))
      '(
           ((const #(a))                  . (push #(a) _)                   )
           ((+ #(l) #(r))                 . (then (then #(l) #(r)) (add _)) )
           ((* #(l) #(r))                 . (then (then #(l) #(r)) (mul _)) )
           ((> #(l) #(r))                 . (then (then #(l) #(r)) (gt _))  )

           ((if #(q) #(t) #(f))           .
                  (then #(q) (jfalse label:
                  (then #(t) (jmp end:
                             (label label:
                  (then #(f) (label end: _)))))))                           )

           ((gt (jfalse #(l) #(rest)))    . (jle #(l) #(rest))              )

           ((then _ #(c))                 . #(c)                            )
           ((then (#(op) #(a)) #(b))      . (#(op) (then #(a) #(b)))        )
           ((then (#(op) #(k) #(a)) #(b)) . (#(op) #(k) (then #(a) #(b)))   )
       )
    ))
  '(push 9 (push 3 (push 2 (jle label: (push 2 (push 3 (mul (jmp end:
     (label label: (push 1 (label end: (add _))))))))))))
)

(load "preprocess.scm");---------------------------------------------------

(test 'preprocess-1
  (lambda ()
    (preprocess
      '(
         (                                            ; list of named terms
           (ast:   (const a 4 (+ 3 (* a 3))))         ; a named term
           (stab:  eot)
           (out:   halt)
         )
         (                                            ; list of compound-rules
           ((ast: foo => bar) (stab: bee => hive))    ; a compound-rule
         )
       )
    ))
   '#(
       #((const a 4 (+ 3 (* a 3))) eot halt)
        ((#(0 foo bar) #(1 bee hive)))
     )
)

(load "forest-rewrite.scm");--------------------------------------------------

(test 'rewrite-tree-1
  (lambda ()
    (rewrite-forest (preprocess
      '(
         (
           (ast:   (+ (const 9) (* (const 2) (const 3))))
         )
         (
           ((ast: (const #(a))                  => (push #(a) _)                     ))
           ((ast: (+ #(l) #(r))                 => (then (then #(l) #(r)) (add _))   ))
           ((ast: (* #(l) #(r))                 => (then (then #(l) #(r)) (mul _))   ))
           ((ast: (then _ #(c))                 => #(c)                              ))
           ((ast: (then (#(op) #(a)) #(b))      => (#(op) (then #(a) #(b)))          ))
           ((ast: (then (#(op) #(k) #(a)) #(b)) => (#(op) #(k) (then #(a) #(b)))     ))
         )
       )
    )))
  '#((push 9 (push 2 (push 3 (mul (add _))))))
)

(test 'rewrite-tree-2
  (lambda ()
    (rewrite-forest (preprocess
      '(
         (
           (ast:   (+ (const 9)
                      (if (> (const 3) (const 2))
                          (* (const 2) (const 3))
                          (const 1)))
           )
         )
         (
           ((ast: (const #(a))                  => (push #(a) _)                     ))
           ((ast: (+ #(l) #(r))                 => (then (then #(l) #(r)) (add _))   ))
           ((ast: (* #(l) #(r))                 => (then (then #(l) #(r)) (mul _))   ))
           ((ast: (> #(l) #(r))                 => (then (then #(l) #(r)) (gt _))    ))

           ((ast: (if #(q) #(t) #(f))           =>
                  (then #(q) (jfalse label: (then #(t) (jmp end: (label label: (then #(f) (label end: _)))))))  ))

           ((ast: (gt (jfalse #(l) #(rest)))    => (jle #(l) #(rest))                ))

           ((ast: (then _ #(c))                 => #(c)                              ))
           ((ast: (then (#(op) #(a)) #(b))      => (#(op) (then #(a) #(b)))          ))
           ((ast: (then (#(op) #(k) #(a)) #(b)) => (#(op) #(k) (then #(a) #(b)))     ))
         )
       )
    )))
  '#((push 9 (push 3 (push 2 (jle label: (push 2 (push 3 (mul (jmp end:
     (label label: (push 1 (label end: (add _)))))))))))))
)

;-------------------------------------------------------------------

(test 'rewrite-forest-1
  (lambda ()
    (rewrite-forest (preprocess
      '(
         (
           (ast:   (+ 3 (* 2 3)))
           (out:   halt)
         )
         (
           ((ast: #(a num) => _) (out: halt => (push #(a) halt)))
           ((ast: (+ _ _) => _)  (out: halt => (add halt)))
           ((ast: (* _ _) => _)  (out: halt => (mul halt)))
         )
       )
    )))
  '#(_ (push 3 (push 2 (push 3 (mul (add halt))))))
)

(test 'rewrite-forest-2
  (lambda ()
    (rewrite-forest (preprocess
      '(
         (
           (stab:  (a 4 eot))
           (ast:   (+ 1 2 3 a 5 6 a 7 8 9))
         )
         (
           ( (stab: (#(n) #(v) #(tab))            => (#(n) #(v) #(tab)) )
             (ast:  #(n sym)                      => #(v)               ) )
         )
       )
    )))
  '#((a 4 eot) (+ 1 2 3 4 5 6 4 7 8 9))
)

(test 'rewrite-forest-3
  (lambda ()
    (rewrite-forest (preprocess
      '(
         (
           (ast:   (let a 4 (+ 3 (* a 3))) )
           (stab:  eot)
           (out:   halt)
         )
         (
           ((ast:  (let #(n sym) #(v) #(expr)) => #(expr)            )
            (stab: eot                         => (#(n) #(v) eot)    ))
           ((ast:  #(n sym)                    => #(v)               )
            (stab: (#(n) #(v) #(tab))          => (#(n) #(v) #(tab)) ))
           ((ast: #(a num)                     => _                  )
            (out: halt                         => (push #(a) halt)   ))
           ((ast: (+ _ _)                      => _                  )
            (out: halt                         => (add halt)         ))
           ((ast: (* _ _)                      => _                  )
            (out: halt                         => (mul halt)         ))
         )
       )
    )))
  '#(_ (a 4 eot) (push 3 (push 4 (push 3 (mul (add halt))))))
)

;(test 'rewrite-forest-4
;  (lambda ()
;    (rewrite-forest (preprocess
;      '(
;         (
;           (ast:   (if (> 6 4) (print 1) (print 2)) )
;          (bpt:   eot)
;           (out:   halt)
;         )
;         (
;           ((ast: (> _ _)                      => _                  )
;           (out: halt                         => (gt halt)          ))
;          ((ast: (print _)                    => _                  )
;           (out: halt                         => (print halt)       ))
;           ((ast: #(a num)                     => _                  )
;           (out: halt                         => (push #(a) halt)   ))
;           ((ast:  (if _ #(t) #(p))            => (hmm #(t) #(p))    )
;           (out: halt                         => (test halt)        ))
;         )
;       )
;    )))
;  '#(_ (a 4 eot) (push 3 (push 4 (push 3 (mul (add halt))))))
;)

