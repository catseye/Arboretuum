;
; Bottom-Up Tree-Rewriting (Term-Rewriting)
; Chris Pressey, sometime late January 2006
;
; This work is in the public domain.  See the file UNLICENSE for more
; information.
;

;
; Try unifying the pattern part of the given rule with the given term;
; if it matches, return a rewritten term based on the unifier and the
; replacement part of the rule; otherwise return #f.
;
(define rewrite-single-term
  (lambda (term rules)
    (cond
      ((null? rules)
        #f)
      (else
        (let* ((rule        (car rules))
               (pattern     (car rule))
               (replacement (cdr rule))
               (unifier     (unify term pattern '())))
          (cond
            (unifier
              (expand-vars replacement unifier))
            (else
              (rewrite-single-term term (cdr rules)))))))))

;
; Rewrite the given term recursively, with the given set of rules,
; from the bottom up (preorder traversal.)  Returns the rewritten
; term if successful, #f if not.  rules is a list of pat-repl pairs.
;
(define rewrite-bottom-up
  (lambda (term rules)
    (cond
      ((list? term)
        (let loop ((subterms term)    ; first try to unify with each child
                   (acc      '()))    ; keep track of subterms we've seen
          (cond
            ((null? subterms)         ; no more children, try rewrite.
              (rewrite-single-term term rules))
            (else
              (let* ((subterm     (car subterms))
                     (rest        (cdr subterms))
                     (new-subterm (rewrite-bottom-up subterm rules)))
                (cond
                  (new-subterm        ; this child succeeded. incorporate it
                    (let* ((front        (reverse acc))
                           (back         (cons new-subterm rest))
                           (spliced-term (append front back)))
                      spliced-term))
                  (else               ; this child failed, try next one
                    (loop (cdr subterms) (cons subterm acc)))))))))
    (else
      (rewrite-single-term term rules)))))

;
; Repeatedly rewrite the given term with the given rules until it
; is reduced into a normal form (if one exists for these rules.)
; Return the reduced term.
;
(define reduce-term
  (lambda (term rules)
    (let* ((new-term (rewrite-bottom-up term rules)))
      (cond
        (new-term
          (reduce-term new-term rules))
        (else
          term)))))
