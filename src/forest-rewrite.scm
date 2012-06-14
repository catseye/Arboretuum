;
; Forest Rewriting
; Chris Pressey, late January 2006
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
; Given a term, a pattern to look for in that term, a replacement pattern,
; and a unifier (set of substitutions,) determine if the term could be
; rewritten, and if so, return a vector consisting of:
;
; - a partially rewritten term.  The replacement pattern is substituted into
;   the term but WITHOUT the variables expanded into ground terms.
; - a (possibly new) unifier
;
; The rewrite process first recurses into the term's children (bottom-up
; rewriting.) If the given pattern fails to unify anywhere in the term,
; #f is returned.
;
(define partial-rewrite
  (lambda (term pattern replacement unifier)
    (cond ((list? term)
             (let loop ((subterms term)              ; first try to unify with each child
                        (acc      '()))              ; keep track of subterms we've seen
                (cond ((null? subterms)              ; no more children. lament failure.
                        (direct-partial-rewrite term pattern replacement unifier))
                      (else
                        (let* ((subterm (car subterms))
                               (rest    (cdr subterms))
                               (result  (partial-rewrite subterm pattern replacement unifier)))
                          (cond (result                   ; this child succeeded. pass along its result
                                  (let* ((result-term    (vector-ref result 0))
                                         (result-unifier (vector-ref result 1))
                                         (front          (reverse acc))
                                         (back           (cons result-term rest))
                                         (spliced-term   (append front back)))
                                    (vector spliced-term result-unifier)))
                                (else                     ; this child failed,
                                  (loop rest (cons subterm acc))))))))) ; try the next one.
          (else
            (direct-partial-rewrite term pattern replacement unifier)))))

;
; Essentially a helper function for partial-rewrite; if the given
; pattern unifies with the given term, just return a vector containing
; the replacement and the (updated) unifier, else return #f.
;
(define direct-partial-rewrite
  (lambda (term pattern replacement unifier)
    (let* ((new-unifier (unify term pattern unifier)))    ; try to unify
      (cond (new-unifier                                  ; successfully unified, so rewrite
              (vector replacement new-unifier))
            (else
              #f)))))

;
; Given a vector of terms with variable placeholders in them, and
; a unifier, modify the vector so that the variables are replaced
; by their respective replacements (substitutions) in the unifier,
; and return the modified vector.
;
(define expand-forest
  (lambda (terms unifier)
    (let loop ((terms    terms)
               (term-num (- (vector-length terms) 1)))
      (cond ((< term-num 0)
              terms)
            (else
              (let* ((term          (vector-ref terms term-num))
                     (new-term      (expand-vars term unifier))
                     (new-terms     (vector-store terms term-num new-term))
                     (next-term-num (- term-num 1)))
                (loop new-terms next-term-num)))))))

;
; Rewrite a vector of terms in tandem using a list of rules, with a
; shared unifier (so that variable matches are common to all terms.)
; Return a vector of rewritten terms, if the rule list matched, otherwise #f.
;
(define rewrite-terms-with-compound-rule
  (lambda (original-terms original-compound-rule)
      (let loop ((terms         original-terms)
                 (compound-rule original-compound-rule)
                 (unifier       '()))
        (cond ((null? compound-rule)           ; when we reach the end of the list,
                (expand-forest terms unifier)) ; expand variables in all the new terms
              (else
                (let* ((rule         (car compound-rule))
                       (rest-rules   (cdr compound-rule))
                       (targ-term-no (vector-ref rule 0))
                       (pattern      (vector-ref rule 1))
                       (replacement  (vector-ref rule 2))
                       (term         (vector-ref terms targ-term-no))
                       (result       (partial-rewrite term pattern replacement unifier)))
                  (cond (result         ; we matched. update term, and try the next rule
                          (let* ((new-term      (vector-ref result 0))
                                 (new-unifier   (vector-ref result 1))
                                 (new-terms     (vector-store terms targ-term-no new-term)))
                            (loop new-terms rest-rules new-unifier)))
                        (else           ; no match.  abort the entire thing.
                          #f))))))))

;
; Given a vector(#2) of:
;   a vector of terms, and
;   a list of compound rules,
; rewrite all terms simultaneously with each of the compound rules.
; Rewriting a set of terms simultaneously means that the variables in the
; compound rule are shared across the terms, and will only unify with subterms
; that are common to all of the terms.
;
; Keep applying compound rules until there are none that apply any longer.
;
; Return a vector of terms so rewritten.
;
(define rewrite-forest
  (lambda (everything)
    (let* ((original-terms     (vector-ref everything 0))
           (all-compound-rules (vector-ref everything 1)))
      (let loop ((terms           original-terms)
                 (compound-rules  all-compound-rules))
        (cond ((null? compound-rules)
                terms)                                  ; terminate and return new termlist
              (else
                (let* ((compound-rule (car compound-rules))
                       (new-terms     (rewrite-terms-with-compound-rule
                                         terms compound-rule)))
                  (cond (new-terms                       ; successfully rewrote.
                          (loop new-terms all-compound-rules)) ; try again, using all compound-rules
                        (else
                          (loop terms (cdr compound-rules))))))))))) ; try again, using rest of compound-rules
