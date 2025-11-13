PUSHALL MACRO
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
ENDM

POPALL MACRO
    POP DX
    POP CX
    POP BX
    POP AX
ENDM

PRINT MACRO FONTE
        ; imprime uma msg
        MOV AH,9
        LEA DX,FONTE
        INT 21h
ENDM

LEIA MACRO
    ; le teclado
    MOV AH,1
    INT 21h
ENDM

PULA_LINHA MACRO
    MOV AH,2
    MOV DL,10
    INT 21h
ENDM

TO_ASM MACRO
    ; transforma numeros de 1-3 em coordenadas de matriz
    ; assume que a coluna esta guardada em bx e que a linha esta guardada em si
    PUSH DX
    PUSH AX
    ; coluna 0 .. 2 = coluna input - 1
    DEC BX
    ; linha 0 .. 6 = (linha input - 1) * DIM
    DEC SI
    MOV AX,SI
    MOV SI,DIM
    MUL SI
    MOV SI,AX
    POP AX
    POP DX
ENDM

SALVA_COORDS MACRO VETOR
    PUSH CX
    MOV VETOR[0],BX
    MOV VETOR[2],SI
    POP CX
ENDM

GET_AND_COMPARE MACRO
    MOV BX,COORD_STORAGE[0]
    MOV SI,COORD_STORAGE[2]
    AND BX,00FFh
    AND SI,00FFh
    CMP VELHA[BX][SI],'+'
ENDM