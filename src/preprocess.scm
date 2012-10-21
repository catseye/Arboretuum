;
; Preprocessor for Forest Rewriter
; Chris Pressey, sometime late January 2006
;
; This work is in the public domain.  See the file UNLICENSE for more
; information.
;

;
; The goal here is to allow the compound-rules to be specified in a nicer,
; more flexible syntax, and preprocess them so that they are in a form that
; this engine can handle (eventually they should be compiled to super-efficient
; sequential code that "knows" which rewrites are likely immediately after
; other rewrites occur; but, first things first.)
;
; The most important part of this is *sorting* the rules by *specificity*
; so that the most specific rules are applied first.
;
; Ideally this would solve all our problems.  But it might not, so we probably
; want a stable sorting algorithm that preserves the relative order specified
; by the programmer.
;
; Another reason to do this is to do type checking and other static analysis.
; e.g. a variable which appears on some RHS of a compound-rule, must also
; appear on some LHS of that compound-rule.
;

;
; The terms and compound-rules, before preprocessing, look like this:
;
; (
;   (                                            ; list of named terms
;     (ast: ())                                  ; a named terms
;     (stab: ())
;     (out: ())
;   )
;   (                                            ; list of compound-rules
;     ((ast: foo => bar) (stab: bee => hive))    ; a compound-rule
;   )
; )


;
; Let's borrow Aardappel's specificity ordering here: var < num < sym < list
;
(define more-general-pattern-than?
  (lambda (pattern-a pattern-b)
    (cond ((null? pattern-a)
            #t)
          ((null? pattern-b)
            #f)
          ((and (list? pattern-a) (list? pattern-b))
            (or (more-general-pattern-than? (car pattern-a) (car pattern-b))
                (more-general-pattern-than? (cdr pattern-a) (cdr pattern-b))))
          (else
            (< (term-specificity pattern-a) (term-specificity pattern-b))))))

(define term-specificity
  (lambda (term)
    (cond ((pattern-var? term)
            1)
          ((number? term)
            2)
          ((symbol? term)
            3)
          (else                ; list, most likely
            4))))

(define more-general-rule-than?
  (lambda (rule-a rule-b)
    (let* ((pattern-a (vector-ref rule-a 1))
           (pattern-b (vector-ref rule-b 1)))
      (more-general-pattern-than? pattern-a pattern-b))))

(define sort-compound-rule
  (lambda (compound-rule)
    (mergesort compound-rule more-general-rule-than?)))

;
; Returns a list like: ((ast: . 1) (stab: . 2) (out: . 3))
; so that we can access a term's position in the vector given its name
;
(define form-term-map
  (lambda (named-terms-depic n acc)
    (cond ((null? named-terms-depic)
            (reverse acc))
          (else
            (let* ((named-term-depic (car named-terms-depic))
                   (name             (car named-term-depic))
                   (pair             (cons name n))
                   (new-acc          (cons pair acc)))
              (form-term-map (cdr named-terms-depic) (+ n 1) new-acc))))))

(define preprocess-named-terms
  (lambda (named-terms-depic acc)
    (cond ((null? named-terms-depic)
            (list->vector (reverse acc)))
          (else
            (let* ((named-term-depic (car named-terms-depic))
                   (term             (cadr named-term-depic))
                   (new-acc          (cons term acc)))
              (preprocess-named-terms (cdr named-terms-depic) new-acc))))))

;
; ((ast: foo => bar) (stab: bee => hive))
;
(define preprocess-compound-rule
  (lambda (compound-rule-depic term-map acc)
    (cond ((null? compound-rule-depic)
            (reverse acc))
          (else
            (let* ((rule-depic       (car compound-rule-depic))
                   (rule-term-name   (car rule-depic))
                   (rule-term-index  (cdr (assq rule-term-name term-map)))
                   (rule-pattern     (cadr rule-depic))
                   (rule-replacement (cadddr rule-depic))
                   (rule             (vector rule-term-index rule-pattern rule-replacement))
                   (new-acc          (cons rule acc)))
              (preprocess-compound-rule (cdr compound-rule-depic) term-map new-acc))))))

(define preprocess-compound-rules
  (lambda (compound-rules-depic term-map acc)
    (cond ((null? compound-rules-depic)
            (reverse acc))
          (else
            (let* ((compound-rule-depic  (car compound-rules-depic))
                   (compound-rule        (preprocess-compound-rule compound-rule-depic term-map '()))
                   (sorted-compound-rule (sort-compound-rule compound-rule))
                   (new-acc              (cons sorted-compound-rule acc)))
              (preprocess-compound-rules (cdr compound-rules-depic) term-map new-acc))))))

(define preprocess
  (lambda (depic)
    (let* ((named-terms-depic    (car depic))
           (compound-rules-depic (cadr depic))
           (term-map             (form-term-map named-terms-depic 0 '()))
           (term-vector          (preprocess-named-terms named-terms-depic '()))
           (compound-rules       (preprocess-compound-rules compound-rules-depic term-map '())))
      (vector term-vector compound-rules))))
