#lang play

;;;;;;;;;;;;;;;;;;;;;;;
;; Machine definition
;;;;;;;;;;;;;;;;;;;;;;;


#|
Instructions
|#
(deftype Instruction
  (INT-CONST n) 
  (ADD)
  (SUB)
  (CLOSURE ins))

;; values
(deftype Val
  (closureV body env))


#|
<Stack> :: = <EmptyStack>
        | {Stacked <val><Stack>}
|#

(deftype Stack
  (Stacked value next)
  (EmptyStack))

;; stack-init :: -> Stack
;; Retorna un Stack vacio
(define (stack-init)
  (EmptyStack))


;; stack-pop :: Stack -> Stack
;; Retorna el stack luego de haber retirado el ultimo elemento. Error si esta vacio
(define (stack-pop s)
  (match s
    [(Stacked v n) n]
    [(EmptyStack) (error "stack-pop to an EmptyStack")]))



;; stack-peek :: Stack -> V
;;Retorna el valor que esta en el tope del stack, Error si esta vacio
(define (stack-peek s)
  (match s
    [(Stacked v n) v]
    [(EmptyStack) (error "stack-peek to an EmptyStack")]))


;; stack-push :: Stack, V -> Stack
;Retorna el stack despues de agregar el nuevo valor
(define (stack-push s v)
  (Stacked v s))

;; stack-empty :: Stack -> Boolean
;;Retorna true si el stack esta vacio, falso en otro caso
(define (stack-empty? s)
  (match s
    [(EmptyStack) #t]
    [(Stacked v n) #f]))


;; stack-size :: Stack -> Int
;;Retorna cuantos elementos hay en el stack
(define (stack-size s)
  (letrec ([sstr (λ(s c)
                   (match s
                     [(EmptyStack) c]
                     [(Stacked v n) (sstr n (+ 1 c))]))])
    (sstr s 0)))


;; run :: List[Instruction], Stack[Instructions], List -> CONST
;; ejecuta la lista de instrucciones con el stack y el ambiente dados
(define (run ins-list stack env)
  ;(debug-run ins-list stack)
  (if (> (stack-size stack) 100)
      (error "STACK_OVERFLOW")
      (match ins-list
        ['() (if (= 1 (stack-size stack))
                 (match (stack-peek stack)
                   [(INT-CONST n) n]
                   [(closureV ins env) (closureV ins env)]
                   [e "CORRUPT_ENDING_STATE"])
                 (error "CORRUPT_ENDING_STATE")
                 ;stack
                 )]
        [_ (let ([non-local-exn? (λ(ex) (and (not (string=? (exn-message ex)
                                                            "CORRUPT_ENDING_STATE"))
                                             (not (string=? (exn-message ex)
                                                            "STACK_OVERFLOW"))))]
                 [fault (λ(ex)
                          (error "SEGFAULT")
                          )])
             (with-handlers ([non-local-exn? fault])
               
               (match ins-list
                 [(list (INT-CONST n) tail ...)
                  (run tail (stack-push stack (INT-CONST n)) env )]

                 [(list (ADD) tail ...) (def (INT-CONST n1) (stack-peek stack))
                                        (def (INT-CONST n2) (stack-peek (stack-pop stack)))
                                        (def new-stack (stack-pop (stack-pop stack)))
                                        (run tail (stack-push new-stack (INT-CONST (+ n2 n1))) env )]
                 [(list (SUB) tail ...) (def (INT-CONST n1) (stack-peek stack))
                                        (def (INT-CONST n2) (stack-peek (stack-pop stack)))
                                        (def new-stack (stack-pop (stack-pop stack)))
                                        (run tail (stack-push new-stack (INT-CONST (- n1 n2))) env )]
       
                 )))])))


;machine
;; machine :: List[Instruction] -> Expr
;; ejecuta la lista de instrucciones en una maquina con stack y ambiente vacios
(define (machine ins-list)
  (run ins-list (stack-init) '()))

(define (new-line s) (string-append s "\n") )

(define (display exp)
  ( display-to-file (new-line exp)
       "file.txt"
       #:mode 'text
       #:exists 'append))
  
       