TITLE AAAAAAA
.MODEL SMALL
.STACK 100h
.DATA
    VELHA DB '+', '+', '+'
          DB '+', '+', '+'
          DB '+', '+', '+'

    CLEAR DB 10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,'$'

    LINHA DB 10,13,'DIGITE A LINHA: $'
    COLUNA DB 10,13,'DIGITE A COLUNA: $'
.CODE
    MAIN PROC
        MOV AX,@DATA
        MOV DS,AX

        CALL RNG

        MOV AH,4Ch
        INT 21h
    MAIN ENDP

    RNG PROC
        XOR BX,BX

        MOV AH,00h
        INT 1Ah

        RA:
        TEST CX,0003h
        JNZ NEXT_NUM

        

        SHR CX,1
        JMP RA

        NEXT_NUM:

        MOV BX,CX

        SEGUNDO:
        TEST DX,0003h
        JNZ NEXT_C
        
        SHR DX,1
        JMP SEGUNDO

        NEXT_C:
        MOV SI,DX
        

        RET
    RNG ENDP

    PRINT PROC
        MOV AH,9
        INT 21h
        RET
    PRINT ENDP

    LEIA PROC
        MOV AH,1
        INT 21h
        RET
    LEIA ENDP

    MULT3 PROC
        MOV BL,BH
        ;  * 2
        SHL BH,1
        ; (BH * 2) + BH = BH * 3
        ADD BH,BL
        RET
    MULT3 ENDP

    SALVAR_ESTADO PROC
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        PUSH SI

        MOV AH,9
        LEA DX,CLEAR
        INT 21h

        PUSH SI
        PUSH DX
        PUSH CX
        PUSH BX
        PUSH AX

    SALVAR_ESTADO ENDP

    MATRIZ PROC
        XOR SI,SI
        MOV CH,3

        MOV AH,2
        MOV DL,10
        INT 21h

        LINHA_LOOP:
        MOV CL,3
            COLUNA_LOOP:
                MOV AL,VELHA[SI]
                MOV DL,AL
                MOV AH,2
                INT 21h

                INC SI
                DEC CL
            JNZ COLUNA_LOOP
        MOV AH,2
        MOV DL,10
        INT 21h

        DEC CH
        JNZ LINHA_LOOP

        RET
    MATRIZ ENDP
END MAIN

; FAZER PVP
; FAZER PVCPU
; DETERMINAR SE GANHO (EG: SUBTITUIR CADA CARACTERE POR "|")
; LIMITAR INPUT
; OTIMIZAR
; fazer "+ | + | + ..."