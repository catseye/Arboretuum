;
; Utility functions used by forest-rewriting project
; Chris Pressey, late January 2006
;
; This work is in the public domain.  See the file UNLICENSE for more
; information.
;

;
; Sort a list using mergesort.
;
(define mergesort
  (lambda (lst gt-pred?)
    (cond ((null? lst)
            lst)
          ((null? (cdr lst))
            lst)
          (else
            (let* ((pair  (split lst '() '()))
                   (left  (mergesort (car pair) gt-pred?))
                   (right (mergesort (cdr pair) gt-pred?)))
                (merge left right gt-pred? '()))))))

;
; Yield a pair of lists, each containing roughly half the
; elements of the given list.
;
(define split
  (lambda (lst acc1 acc2)
    (cond
      ((null? lst)
        (cons (reverse acc1) (reverse acc2)))
      ((null? (cdr lst))
        (cons (reverse (append lst acc1)) (reverse acc2)))
      (else
        (let* ((left  (car lst))
               (right (cadr lst))
               (rest  (cddr lst)))
          (split rest (cons left acc1) (cons right acc2)))))))

;
; Given two sorted lists, return a sorted list that contains
; all of the elements from both lists.
;
(define merge
  (lambda (list1 list2 gt-pred? acc)
    (cond
      ((and (null? list1) (null? list2))
        (reverse acc))
      ((null? list1)
        (merge list1 (cdr list2) gt-pred? (cons (car list2) acc)))
      ((null? list2)
        (merge (cdr list1) list2 gt-pred? (cons (car list1) acc)))
      ((gt-pred? (car list1) (car list2))
        (merge list1 (cdr list2) gt-pred? (cons (car list2) acc)))
      (else
        (merge (cdr list1) list2 gt-pred? (cons (car list1) acc))))))

;
; Side-effect-free alternative to vector-set!
;
(define vector-store
  (lambda (vec index item)
    (let loop ((items (vector->list vec))
               (index index)
               (item  item)
               (acc   '()))
      (cond ((null? items)
              (list->vector (reverse acc)))
            ((zero? index)
              (loop (cdr items) (- index 1) item (cons item acc)))
            (else
              (loop (cdr items) (- index 1) item (cons (car items) acc)))))))

;
; Debugging output.
;
(define print
  (lambda list
    (for-each display list)))

(define println
  (lambda list
    (for-each display list)
    (newline)))

;
; Testing framework.
;
(define test
  (lambda (test-name proc expected)
    (print "Running test: " test-name "... ")
    (let ((result (proc)))
      (cond
        ((equal? result expected)
          (println "passed."))
        (else
          (println "FAILED!")
          (println "Expected: " expected)
          (println "Actual:   " result))))))
