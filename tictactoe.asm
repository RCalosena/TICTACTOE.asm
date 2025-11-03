TITLE AAAAAAA
.MODEL SMALL
.STACK 100h
.DATA
    VELHA DB '+', '+', '+'
          DB '+', '+', '+'
          DB '+', '+', '+'

    LINHA DB 10,13,'DIGITE A LINHA: $'
    COLUNA DB 'DIGITE A COLUNA: $'
.CODE
    MAIN PROC
        MOV AX,@DATA
        MOV DS,AX

        XOR CX,CX

        COMECO:

            MOV AH,2
            MOV DL,10
            INT 21h

            LEA DX,COLUNA
            CALL PRINT
            CALL LEIA
            CALL VALIDA

            MOV BX,AX

            LEA DX,LINHA
            CALL PRINT
            CALL LEIA
            CALL VALIDA

            MOV SI,AX

            AND CH,01h
            JNZ ITS_O

            MOV CL,'X'
            JMP PRINT_MATRIX

            ITS_O:
            MOV CL,'O'

            PRINT_MATRIX:
            AND BX,000Fh
            DEC BX

            AND SI,000Fh
            DEC SI
            CALL MULT3
            
            MOV VELHA[BX][SI],CL

            MOV AH,2
            MOV DL,10
            INT 21h

            PUSH CX

            CALL MATRIZP

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
        MOV AX,SI
        ;  * 2
        SHL SI,1
        ; (SI * 2) + AX = SI * 3
        ADD SI,AX
        RET
    MULT3 ENDP

    VALIDA PROC
        VALIDA_DE_NOVO:

            CMP AL,'1'
            JL INVALIDO

            CMP AL,'3'
            JA INVALIDO

            JMP VALIDO

            INVALIDO:
                MOV AH,2
                MOV DL,8
                INT 21h
                MOV DL,' '
                INT 21h
                MOV DL,8
                INT 21h

                CALL LEIA
        JMP VALIDA_DE_NOVO

        VALIDO:

        RET
    VALIDA ENDP

    MATRIZP PROC
        XOR SI,SI

        MOV CH,3
        LINHA_LOOP:
        MOV CL,3
        XOR BX,BX
            COLUNA_LOOP:
                MOV AL,VELHA[BX][SI]
                MOV DL,AL
                MOV AH,2
                INT 21h

                INC BX
                DEC CL
            JNZ COLUNA_LOOP
        MOV AH,2
        MOV DL,10
        INT 21h

        ADD SI,3
        DEC CH
        JNZ LINHA_LOOP

        RET
    MATRIZP ENDP

    HORIZONTAL PROC
        XOR SI,SI

        MOV CL,3
            NLINHA:
                MOV CH,3
                NCOLUNA:
                    CMP VELHA[SI],'X'
                    JE UM
                    CMP VELHA[SI],'O'
                    JE ZERO

                    JMP AUMENTA_SI

                    UM:
                        MOV VELHA[SI],1
                        JMP AUMENTA_SI
                    ZERO:
                        MOV VELHA[SI],0

                    AUMENTA_SI:

    

                    INC SI
                    DEC CH
                JNZ NCOLUNA

        RET
    HORIZONTAL ENDP
END MAIN

; FAZER PVP
; FAZER PVCPU
; DETERMINAR SE GANHO (EG: SUBTITUIR CADA CARACTERE POR "|")
; LIMITAR INPUT
; OTIMIZAR