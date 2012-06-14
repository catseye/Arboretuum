;
; Simple support for unification & pattern matching
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
; Return #t if the given pattern is a variable, #f otherwise.
;
(define pattern-var?
  (lambda (pattern)
    (vector? pattern)))                                   ; just check that it is a vector

;
; Return the name of the given pattern variable.
;
(define get-pattern-var-name
  (lambda (pattern-var)
    (vector-ref pattern-var 0)))                          ; just return the 1st element of the vector

;
; Return the optional predicate associated with the given pattern
; variable, which determines what kind of (Scheme) terms it can
; unify with.  If no predicate is associated with the variable,
; a dummy always-true predicate is returned.
;
(define get-pattern-var-pred
  (lambda (pattern-var)
    (cond ((< (vector-length pattern-var) 2)
            (lambda (x) #t))
          (else
            (let ((term-type (vector-ref pattern-var 1)))
              (cond ((eqv? term-type 'num)
                      (lambda (x) (number? x)))
                    ((eqv? term-type 'sym)
                      (lambda (x) (symbol? x)))
                    (else
                      (lambda (x) #f))))))))

;
; Register that the named pattern variable should be associated with the given
; term in the given unifier.  A new unifier containing the new variable-term
; association will be returned if possible; if it is not possible (i.e. the
; variable is already bound to a different term,) #f is returned.
;
(define bind-pattern-var
  (lambda (term pattern unifier)
    (let* ((var-name  (get-pattern-var-name pattern))
           (var-pred? (get-pattern-var-pred pattern))
           (pair      (assq var-name unifier)))
       (cond
         ((not (var-pred? term))
           #f)
         ((not (pair? pair))                    ; if it's not in unifier,
           (cons (cons var-name term) unifier)) ; add it up front
         ((eqv? (cdr pair) term) ; already bound to the given term: alright
           unifier)
         (else                   ; already bound to something else: not good
           #f)))))

;
; Helper function.
; Given a term and a pattern, where we know both are lists,
; fold over both of them, unifying all the corresponding elements.
;
(define unify-lists
  (lambda (term pattern unifier)
    (cond ((and (null? term) (null? pattern))  ; end of both
            unifier)
          ((or (null? term) (null? pattern))   ; end of one but not the other
            #f)
          (else
            (let ((new-unifier (unify (car term) (car pattern) unifier)))
              (if new-unifier
                (unify-lists (cdr term) (cdr pattern) new-unifier)
                #f))))))

;
; Return #f if the term does not unify with the pattern,
; or a list of substitutions if it does unify.
;
(define unify
  (lambda (term pattern unifier)
    (cond ((pattern-var? pattern)
            (bind-pattern-var term pattern unifier))
          ((and (list? term) (list? pattern))
            (unify-lists term pattern unifier))
          ((eqv? term pattern)
            unifier)
          (else
            #f))))

;
; Given a pattern and a unifier (set of substitutions,) return a term
; where all the variables in the pattern have been replaced by their
; associated term in the unifier.
;
(define expand-vars
  (lambda (pattern unifier)
    (cond ((pattern-var? pattern)      ; variable - replace if in unifier
            (let* ((pair (assq (get-pattern-var-name pattern) unifier)))
              (cond ((pair? pair)
                      (cdr pair))
                    (else
                      pattern))))
          ((list? pattern)             ; list - recurse
            (map (lambda (subpattern)
                   (expand-vars subpattern unifier))
                 pattern))
          (else                        ; ground term - leave it alone.
            pattern))))
