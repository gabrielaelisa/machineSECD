.text
.global main
.extern printf
main:
     stmfd sp!, {lr}
     ldr r0, =string
     mov r1, #3
     stmfd r13!, {r1}
     mov r1, #8
     stmfd r13!, {r1}
     mov r1, #6
     stmfd r13!, {r1}
     ldmfd R13!,{r2,r3}
     add r1, r3, r2 
     stmfd r13!,{r1}
     ldmfd R13!,{r2,r3}
     sub r1, r3, r2 
     stmfd r13!,{r1}
     bl printf
     ldmfd sp!, {pc}
.data
string: .asciz "%d
"
