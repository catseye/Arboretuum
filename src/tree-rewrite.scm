;
; Bottom-Up Tree-Rewriting (Term-Rewriting)
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
