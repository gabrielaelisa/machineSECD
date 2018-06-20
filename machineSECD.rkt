#lang play

;;;;;;;;;;;;;;;;;;;;;;;
;; Machine definition
;;;;;;;;;;;;;;;;;;;;;;;


#|
Instructions
|#
(deftype Instruction
  (INT_CONST n) 
  (ADD)
  (SUB))


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


;; usamos el registro r0 para almacenar temporalmente las constantes
;; en los registros r1 y r2  guardamos los valores del tope del sp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run :: List[Instruction], Stack[Instructions], List -> CONST
;; ejecuta la lista de instrucciones con el stack y el ambiente dados
(define (run ins-list stack armStack)
  ;(debug-run ins-list stack)
  (if (> (stack-size stack) 100)
      (error "STACK_OVERFLOW")
      (match ins-list
        ['() (if (= 1 (stack-size stack))
                 (match (stack-peek stack)
                   [(INT_CONST n) (end n (display (string-append "     ldmfd r13!, {r1}\n" ;carga el resultado final en r1
                                                                 "     bl printf\n"
                                                                 "     ldmfd sp!, {pc}\n"
                                                                 ".data\n"
                                                                 "string: .asciz \"%d\\n\"")))]
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
                 [(list (INT_CONST n) tail ...)
                  ;;;;;;;;;;;;; se guarda en el registro r1 el numero, luego se hace un push a el stack
                  (run tail (stack-push stack (INT_CONST n)) (display (string-append "     mov r1, #"(number->string n)
                                                                                     "\n     stmfd r13!, {r1}")))]

                 [(list (ADD) tail ...) (def (INT_CONST n1) (stack-peek stack))
                                        (def (INT_CONST n2) (stack-peek (stack-pop stack)))
                                        (def new-stack (stack-pop (stack-pop stack)))
                                        ;;;;load multiple del stack a los registros r2 y r3 primero se llena r3 (decendiente)
                                        (run tail (stack-push new-stack (INT_CONST (+ n2 n1)))
                                             (display (string-append "     ldmfd r13!,{r2,r3}\n" 
                                                                     "     add r1, r2, r3 \n"
                                                                     "     stmfd r13!,{r1}"))
                                         )]
                 [(list (SUB) tail ...) (def (INT_CONST n1) (stack-peek stack))
                                        (def (INT_CONST n2) (stack-peek (stack-pop stack)))
                                        (def new-stack (stack-pop (stack-pop stack)))
                                        (run tail (stack-push new-stack (INT_CONST (- n1 n2)))
                                        ;;;;;; se hace un load multiple igual que en la suma
                                             (display(string-append "     ldmfd r13!,{r2,r3}\n"
                                                                    "     sub r1, r2, r3 \n"
                                                                    "     stmfd r13!,{r1}"))
                                          )]
       
                 )))])))


;machine
;; machine :: List[Instruction] -> Expr
;; ejecuta la lista de instrucciones en una maquina con stack y ambiente vacios
;; crea un archivo en ARM con las instrucciones a ejecutar
(define (machine ins-list)
  (run ins-list (stack-init)(display
".text
.global main
.extern printf
main:
     stmfd sp!, {lr}
     ldr r0, =string")))

;;end :: number, function -> number
;;retorna el número y se ejecuta pasivamente la función
(define (end n file) n)
(define (new-line s) (string-append s "\n") )

(define (display exp)
  ( display-to-file (new-line exp)
       "file.s"
       #:mode 'text
       #:exists 'append))

(define (displaySymbol exp)
  ( display-to-file exp
       "file.s"
       #:mode 'binary
       #:exists 'append))


  
       