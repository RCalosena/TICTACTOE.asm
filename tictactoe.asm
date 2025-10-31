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

            MOV BL,AL


            LEA DX,LINHA

            CALL PRINT
            CALL LEIA

            CALL VALIDA

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
            CALL VERIFICA_COLUNA

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

    VALIDA PROC
        CMP AL,'1'
        JL COMECO

        CMP AL,'3'
        JA COMECO

        RET
    VALIDA ENDP

    MATRIZ PROC
        XOR SI,SI
        MOV CH,3

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