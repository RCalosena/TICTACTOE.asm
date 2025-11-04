TITLE JOGO DA VELHA
.MODEL SMALL
.386
.STACK 100h
.DATA
    DIM EQU 3

    VELHA DB '+', '+', '+'
          DB '+', '+', '+'
          DB '+', '+', '+'

    LINHA    DB 10,13,'Digite a linha (1-3): $'
    COLUNA   DB 10,13,'Digite a coluna (1-3): $'
    OCUPADA  DB 10,13,'Coordenada ocupada! Tente de novo.$'
    VENCEDOR  DB 10,13,'Jogador ','$'
    VENCEU   DB ' venceu!$'
    EMPATE   DB 10,13,'empate!$'
    BORDA   DB ' | $'

    JOGADAS DB 0

.CODE
    MAIN PROC
        MOV AX,@DATA
        MOV DS,AX
        XOR CX,CX

        COMECO:
            ; input para a coluna
            ; caracteres fora de 1-3 sao omitidos
            LEA DX,COLUNA
            CALL PRINT
            CALL LEIA
            CALL VALIDA
            MOV BX,AX

            ; input para a linha
            ; caracteres fora de 1-3 sao omitidos
            LEA DX,LINHA
            CALL PRINT
            CALL LEIA
            CALL VALIDA
            MOV SI,AX

            ; determina o jogador atual
            AND CH,01h
            JNZ ITS_O
            MOV CL,'X'
            JMP PRINT_MATRIX
            ITS_O:
            MOV CL,'O'

            PRINT_MATRIX:
            ; transforma os caracteres em coordenadas de matriz
            AND BX,000Fh
            ; coluna 0 .. 2 = coluna input (1-3) - 1
            DEC BX

            ; linha 0 .. 6 = linha input (1-3) - 1 * 3
            AND SI,000Fh
            DEC SI
            MOV AX,SI
            ; * 2
            SHL SI,1
            ; + valor original
            ADD SI,AX
            
            ; verifica se está ocupada
            CALL IS_OCCUPIED

            ; coloca o X ou O na posição
            MOV VELHA[BX][SI],CL

            ; abre dois espaços e imprime a matriz
            MOV AH,2
            MOV DL,10
            INT 21h
            INT 21h

            ; salva o simbolo (X-O) para futuro uso
            MOV DI,CX
            AND DI,00FFh
            ; salva CX para depois determinar o jogador
            PUSH CX

            ; imprime matriz e verifica se ha vitoria
            CALL MATRIZP
            CALL VERIFICA_VITORIA
            
            ; volta o valor de cx
            POP CX
            ; CH == 1 { VEZ DO 'O' }
            INC CH
            ; incrementa o numero de jogadas
            MOV AL,JOGADAS
            INC AL
            MOV JOGADAS,AL
            ; se o maximo de jogadas for atendido, eh empate
            CMP JOGADAS,9
            JE DRAW

        JMP COMECO

        ; tipo de finalização

        DRAW:
        LEA DX,EMPATE
        CALL PRINT
        JMP FINAL

        VITORIA:
        LEA DX,VENCEDOR
        CALL PRINT
        MOV AH,2
        MOV DX,DI
        INT 21h
        LEA DX,VENCEU
        CALL PRINT

        FINAL:
        MOV AH,4Ch
        INT 21h
    MAIN ENDP

    PRINT PROC
        ; imprime uma msg
        MOV AH,9
        INT 21h
        RET
    PRINT ENDP

    LEIA PROC
        ; le teclado
        MOV AH,1
        INT 21h
        RET
    LEIA ENDP

    VALIDA PROC

        ; verifica se o input eh entre 1-3
        ; caso contrário, apaga o caracter anterior e pede input de novo

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

    IS_OCCUPIED PROC

        ; verifica se a posição escolhida já foi ocupada por um X ou O

        MOV AL,VELHA[BX][SI]
        CMP AL,'+'
        JE FALSE

        LEA DX,OCUPADA
        CALL PRINT
        JMP COMECO

        FALSE:
        RET
    IS_OCCUPIED ENDP

    MATRIZP PROC

        ; imprime a matriz salva
        ; adiciona bordas nos lados para estilizar

        XOR SI,SI

        MOV CH,DIM
        MOV AH,2
        LINHA_LOOP:
        MOV CL,DIM
        XOR BX,BX
            COLUNA_LOOP:

                PUSH AX
                LEA DX,BORDA
                CALL PRINT
                POP AX

                MOV AL,VELHA[BX][SI]
                MOV DL,AL
                INT 21h

                INC BX
                DEC CL
            JNZ COLUNA_LOOP

        PUSH AX
        LEA DX,BORDA
        CALL PRINT
        POP AX

        MOV DL,10
        INT 21h

        ADD SI,DIM
        DEC CH
        JNZ LINHA_LOOP

        RET
    MATRIZP ENDP

    VERIFICA_VITORIA PROC

        ; passa por colunas linhas e diagonais para verificar se alguem ganhou

        CALL VER_COLUNAS
        CALL VER_LINHAS
        CALL VER_DIAGONAIS

        RET
    VERIFICA_VITORIA ENDP

    VER_COLUNAS PROC

        ; escaneia as colunas por um valor igual a DI (Simbolo do último jogador)
        ; se achar, incrementa um contador. Se o contador chega a 3, tem vitoria

        XOR SI,SI
        XOR BX,BX
        XOR CX,CX
        MOV AX,DI
        MOV CL,DIM
        COUNT_F:
            NL:
            CMP AL,VELHA[BX][SI]
            JNE SEGUINTE_F

            INC CH
            CMP CH,3
            JE VITORIA

            ADD SI,3
            JMP NL

            SEGUINTE_F:
            XOR CH,CH
            INC BX
            DEC CL
        JNZ COUNT_F

        RET
    VER_COLUNAS ENDP

    VER_LINHAS PROC

        ; escaneia as linhas por um valor igual a DI (Simbolo do último jogador)
        ; se achar, incrementa um contador. Se o contador chega a 3, tem vitoria

        XOR SI,SI
        XOR BX,BX
        XOR CX,CX
        MOV AX,DI
        MOV CL,DIM
        COUNT_L:
            NC:
            CMP AL,VELHA[BX][SI]
            JNE SEGUINTE_L

            INC CH
            CMP CH,3
            JE VITORIA

            INC BX
            JMP NC

            SEGUINTE_L:
            XOR CH,CH
            ADD SI,3
            DEC CL
        JNZ COUNT_L

        RET
    VER_LINHAS ENDP

    VER_DIAGONAIS PROC

        ; escaneia a diagonal e diagonal espelhada por um valor igual a DI (Simbolo do último jogador)
        ; se achar, incrementa um contador. Se o contador chega a 3, tem vitoria

        XOR SI,SI
        XOR BX,BX
        XOR CX,CX
        MOV AX,DI

        ND:
        CMP AL,VELHA[BX][SI]
        JNE SEGUINTE_D

        INC CH
        CMP CH,3
        JE VITORIA

        ADD SI,3
        INC BX
        JMP ND

        SEGUINTE_D:
        
        MOV BX,2
        XOR SI,SI

        NDI:
        CMP AL,VELHA[BX][SI]
        JNE NADA

        INC CH
        CMP CH,3
        JE VITORIA

        ADD SI,3
        DEC BX
        JMP NDI

        NADA:

        RET
    VER_DIAGONAIS ENDP
END MAIN

; FAZER PVP
; FAZER PVCPU
; DETERMINAR SE GANHO (EG: SUBTITUIR CADA CARACTERE POR "|")
; LIMITAR INPUT
; OTIMIZAR