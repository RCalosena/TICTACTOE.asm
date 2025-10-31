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

        XOR CX,CX
        JMP COMECO

        ORIGIN:

        XOR AL,AL
        XOR BX,BX

        CALL SALVAR_ESTADO

        ;MOV AH,9
        ;LEA DX,CLEAR
        ;INT 21h

        CALL MATRIZ

        COMECO:

            LEA DX,COLUNA
            
            CALL PRINT
            CALL LEIA

            CMP AL,'1'
            JL ORIGIN
            CMP AL,'3'
            JA ORIGIN

            MOV BL,AL


            LEA DX,LINHA

            CALL PRINT
            CALL LEIA

            CMP AL,'1'
            JL ORIGIN
            CMP AL,'3'
            JA ORIGIN

            MOV BH,AL

            TEST CH,01h
            JNZ ITS_O

            MOV CL,'X'
            JMP PRINT_MATRIX

            ITS_O:
            MOV CL,'O'

            PRINT_MATRIX:

            XOR DX,DX
            XOR AX,AX

            AND BL,0Fh
            DEC BL

            MOV AL,BL
            AND BH,0Fh
            DEC BH
            CALL MULT3

            MOV DL,BH
            
            ADD AX,DX
            MOV SI,AX

            MOV VELHA[SI],CL

            MOV AH,2
            MOV DL,10
            INT 21h

            PUSH CX

            CALL MATRIZ

            POP CX

            INC CH

        JMP COMECO

        MOV AH,4Ch
        INT 21h
    MAIN ENDP

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