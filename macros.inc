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

SALVA_COORDS MACRO
    PUSH CX
    MOV COORD_STORAGE[0],BX
    MOV COORD_STORAGE[2],SI
    POP CX
ENDM

GET_AND_COMPARE MACRO
    MOV BX,COORD_STORAGE[0]
    MOV SI,COORD_STORAGE[2]
    AND BX,00FFh
    AND SI,00FFh
    CMP VELHA[BX][SI],'+'
ENDM

APAGA_LINHA MACRO
    LOCAL APAGA

    PUSH CX
    PUSH DX

    MOV AH,2
    MOV DL,13 ; mexe o cursor para o começo da linha
    INT 21h

    ; 'apaga' a mensagem colocando um espaço por cima de cada caracter
    MOV DL,' '
    MOV CX,11
    APAGA:
        INT 21h
    LOOP APAGA

    MOV DL,13
    INT 21h

    POP DX
    POP CX
ENDM

CENTRO MACRO
    CWD
    MOV AX,DIM
    MOV CX,2
    DIV CX
    ADD AX,DX
    MOV BX,AX
    MOV SI,AX
    TO_ASM
ENDM

TOGGLE MACRO VAR, INDEX
    LOCAL ON, OFF

    ; liga/desliga opções (VAR) no menu Extras
    ; coloca um check (no index especificado) indicando que foi ligado

    CMP OUTROS[INDEX],251
    JE OFF
    MOV OUTROS[INDEX],251
    MOV VAR,1
    JMP ON
    OFF:
    MOV OUTROS[INDEX],' '
    MOV VAR,0
    ON:
ENDM