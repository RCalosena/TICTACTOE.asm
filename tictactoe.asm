TITLE JOGO DA VELHA
.MODEL SMALL
.386
.STACK 100h

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
    PUSH AX
    ; coluna 0 .. 2 = coluna input - 1
    DEC BX
    ; linha 0 .. 6 = (linha input - 1) * 3
    DEC SI
    MOV AX,SI
    SHL SI,1 ; * 2
    ADD SI,AX ; + valor original
    POP AX
ENDM

SALVA_COORDS MACRO
    PUSH CX
    MOV WORD PTR COORD_STORAGE[0],BX
    MOV WORD PTR COORD_STORAGE[1],SI
    POP CX
ENDM

GET_AND_COMPARE MACRO
    MOV BX,WORD PTR COORD_STORAGE[0]
    MOV SI,WORD PTR COORD_STORAGE[1]
    AND BX,00FFh
    AND SI,00FFh
    CMP VELHA[BX][SI],'+'
ENDM

.DATA
    DIM EQU 3

    VELHA DB '+', '+', '+'
          DB '+', '+', '+'
          DB '+', '+', '+'

    BEM_VINDO DB 'Modos de Jogo:',10,13,10,13,'1) Jogador Contra Jogador',10,13,'2) Jogador Contra CPU',10,13,'$'
    ESCOLHA DB 10,13,'Escolha: $'
    DIFFICULDADES DB 10,13,10,13,'Difficuldades:',10,13,10,13,'1) Facil',10,13,'2) Medio',10,13,'3) Dificil',10,13,'4) Impossivel',10,13,'$'
    LINHA    DB 10,13,'Digite a linha (1-3): $'
    COLUNA   DB 10,13,'Digite a coluna (1-3): $'
    OCUPADA  DB 10,13,'Coordenada ocupada! Tente de novo.$'
    VENCEDOR  DB 10,13,'Jogador ',34,?,34,' venceu!$'
    EMPATE   DB 10,13,'empate!$'
    BORDA   DB ' | $'

    MODO DB 0
    DIFFICULDADE DB 0
    JOGADAS DB 0
    COORD_STORAGE DB 0,0
.CODE
    MAIN PROC
        MOV AX,@DATA
        MOV DS,AX

        PRINT BEM_VINDO
        PRINT ESCOLHA

        MOV CL,'1'
        MOV CH,'2'
        CALL LEIA_E_VALIDA
        AND AL,0Fh
        MOV MODO,AL

        CMP MODO,2
        JNE PVP

        PRINT DIFFICULDADES
        PRINT ESCOLHA

        MOV CL,'1'
        MOV CH,'4'
        CALL LEIA_E_VALIDA
        AND AL,0Fh
        MOV DIFFICULDADE,AL

        PVP:
        XOR CX,CX
        XOR DI,DI
        PULA_LINHA

        COMECO:
            ; determina o jogador atual
            AND CH,01h
            JNZ JOGADOR_O
            MOV CL,'X'
            JMP SETUP
            JOGADOR_O:

            MOV CL,'O'
            CMP MODO,2
            JNE SETUP
                        
            CALL PLAN_MOVE
            JMP CPU_SKIP

            SETUP:

            ; input para a coluna e linha
            ; caracteres fora de 1-3 sao omitidos
            PUSH CX
            PRINT COLUNA
            MOV CL,'1'
            MOV CH,'3'
            CALL LEIA_E_VALIDA
            AND AX,000Fh
            MOV BX,AX
            PRINT LINHA
            CALL LEIA_E_VALIDA
            AND AX,000Fh
            MOV SI,AX
            POP CX
            ; transforma BX e SI em coordenadas legíveis pelo programa
            TO_ASM
            ; verifica se tá ocupada
            CALL IS_OCCUPIED
            CMP AL,1
            JE COMECO

            CPU_SKIP:
            ; coloca o X ou O na posição
            MOV VELHA[BX][SI],CL

            ; dois espaços
            PULA_LINHA
            PULA_LINHA

            ; imprime matriz e verifica se ha vitoria
            CALL MATRIZP
            
            CMP JOGADAS,3
            JNA NAOVER
                ; passa por colunas linhas e diagonais para verificar se alguem ganhou
                MOV DL,DIM
                
                CALL VER_COLUNAS
                CMP AL,1
                JE VITORIA

                CALL VER_LINHAS
                CMP AL,1
                JE VITORIA

                CALL VER_DIAGONAIS
                CMP AL,1
                JE VITORIA
            NAOVER:

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
        PRINT EMPATE
        JMP FINAL

        VITORIA:
        MOV DL,CL
        MOV VENCEDOR[11],DL
        PRINT VENCEDOR

        FINAL:
        MOV AH,4Ch
        INT 21h
    MAIN ENDP

    LEIA_E_VALIDA PROC

        ; inputs:
        ;   CL = valor minimo
        ;   CH = valor maximo

        ; verifica se o caracter lido eh de CL a CH
        ; caso contrário, apaga o caracter anterior e pede input de novo

        PUSH DX

        XOR AX,AX
        JMP READ

        VALIDA_DE_NOVO:

            CMP AL,CL
            JL INVALIDO

            CMP AL,CH
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

                READ:
                LEIA
        JMP VALIDA_DE_NOVO

        VALIDO:

        POP DX

        RET
    LEIA_E_VALIDA ENDP

    IS_OCCUPIED PROC

        ; inputs: 
        ;   BX = coluna
        ;   SI = linha
        ;   CL = jogador (X ou O)

        ; output: AL = 0 ou 1

        ; verifica se a posição escolhida não esta vazia
        ; imprime uma mensagem se quem escolheu a posição eh um jogador

        CHECK:

        MOV AL,VELHA[BX][SI]
        CMP AL,'+'
        JE FALSE

        ; verifica se eh jogador ou cpu
        ; se eh cpu, nao imprime a mensagem
        CMP CL,'O'
        JNE PLAYER
        CMP MODO,2
        JNE PLAYER
        JMP V

        PLAYER:
        PRINT OCUPADA

        V:
        MOV AL,1
        JMP TRUE


        FALSE:
        XOR AL,AL
        TRUE:
        RET
    IS_OCCUPIED ENDP

    MATRIZP PROC

        ; imprime a matriz salva
        ; adiciona bordas nos lados para estilizar

        PUSHA

        XOR SI,SI

        MOV CH,DIM
        MOV AH,2
        LINHA_LOOP:
        MOV CL,DIM
        XOR BX,BX
            COLUNA_LOOP:

                PUSH AX
                PRINT BORDA
                POP AX

                MOV AL,VELHA[BX][SI]
                MOV DL,AL
                INT 21h

                INC BX
                DEC CL
            JNZ COLUNA_LOOP

        PUSH AX
        PRINT BORDA
        PULA_LINHA
        POP AX

        ADD SI,DIM
        DEC CH
        JNZ LINHA_LOOP

        POPA

        RET
    MATRIZP ENDP

    VER_COLUNAS PROC

        ; inputs:
        ;   CL = ASCII a comparar
        ;   DL = Meta
        ;   DI = Começa a partir da (DI)a coluna

        ; output:
        ;   AL = 0 ou 1

        ; escaneia as colunas por um valor igual a CL
        ; cada sucesso incrementa um contador CH
        ; se CH é maior ou igual à meta DL o procedimento retorna verdadeiro (AL = 1)

        PUSH BX
        PUSH CX
        PUSH DX
        PUSH SI

        ; coordenadas
        XOR SI,SI
        XOR CH,CH
        XOR DH,DH

        ; começa a partir da coluna indicada por DI
        MOV BX,DI

        ; salva o simbolo do jogador em al
        MOV AL,CL
        ; contador de scans
        MOV CL,DIM
        COUNT_F:
            NL:
            CMP AL,VELHA[BX][SI]
            JNE SALVA_C

            ; incrementa e compara com a meta
            INC CH
            
            JMP SEGUINTE_F
                SALVA_C:
                SALVA_COORDS
            SEGUINTE_F:
            ADD SI,DIM
            INC DH
            CMP DH,DIM
            JB NL ; Next Line

            CMP CH,DL
            JAE CTRUE

            ; reseteia o contador de sucesso
            XOR DH,DH
            XOR CH,CH

            ; nova coluna
            XOR SI,SI
            INC BX

            DEC CL
        JNZ COUNT_F ; Count Fileiras
        JMP CFALSE

        CTRUE:
        MOV AL,1
        JMP CRETURN

        CFALSE:
        XOR AL,AL

        CRETURN:

        POP SI
        POP DX
        POP CX
        POP BX
        
        RET
    VER_COLUNAS ENDP

    VER_LINHAS PROC

        ; inputs:
        ;   CL = ASCII a comparar
        ;   DL = Meta
        ;   DI = Começa a partir da (DI)a linha

        ; output:
        ;   AL = 0 ou 1

        ; escaneia as linhas por um valor igual a CL
        ; cada sucesso incrementa um contador CH
        ; se CH é maior ou igual à meta DL o procedimento retorna verdadeiro (AL = 1)

        PUSH BX
        PUSH CX
        PUSH DX
        PUSH SI

        ; coordenadas
        XOR SI,SI
        XOR BX,BX
        XOR CH,CH
        XOR DH,DH

        ; SI começa a partir do especificado em DI, então:
        ; SI = DI * DIM
        PUSH AX
        PUSH CX
        PUSH DX
        MOV AX,DI        
        MOV CX,DIM
        MUL CL            
        MOV SI,AX
        POP DX
        POP CX
        POP AX         

        ; guarda o simbolo em al
        MOV AL,CL
        ; n de scans
        MOV CL,DIM
        COUNT_L:
            NC:
            CMP AL,VELHA[BX][SI]
            JNE SALVA_L

            ; incrementa e compara com a meta
            INC CH

            JMP SEGUINTE_L
                SALVA_L:
                SALVA_COORDS
            SEGUINTE_L:
            INC BX
            INC DH
            CMP DH,DIM
            JB NC ; Next Column

            CMP CH,DL
            JE LTRUE

            ; reset sucesso
            XOR DH,DH
            XOR CH,CH
            ; nova linha
            XOR BX,BX
            ADD SI,DIM

            DEC CL
        JNZ COUNT_L ; Count Linhas
        JMP LFALSE

        LTRUE:
        MOV AL,1
        JMP LRETURN

        LFALSE:
        XOR AL,AL

        LRETURN:

        POP SI
        POP DX
        POP CX
        POP BX

        RET
    VER_LINHAS ENDP

    VER_DIAGONAIS PROC

        ; inputs:
        ;   CL = ASCII a comparar
        ;   DL = Meta
        ;   DI = Começa a partir da (DI)a diagonal (0-1)

        ; output:
        ;   AL = 0 ou 1

        ; escaneia as diagonais por um valor igual a CL
        ; cada sucesso incrementa um contador CH
        ; se CH é maior ou igual à meta DL o procedimento retorna verdadeiro (AL = 1)

        PUSH BX
        PUSH CX
        PUSH DX
        PUSH SI

        ; coordenadas
        XOR SI,SI
        XOR BX,BX
        XOR CH,CH
        XOR DH,DH
        ; simbolo do jogador em al
        MOV AL,CL

        CMP DI,1
        JE DI_SKIP

        ND:
        CMP AL,VELHA[BX][SI]
        JNE SALVA_D

        ; incrementa sucesso e compara com a meta
        INC CH

        JMP SEGUINTE_D
            SALVA_D:
            SALVA_COORDS
        SEGUINTE_D:
        ADD SI,DIM
        INC BX
        INC DH
        CMP DH,DIM
        JB ND ; Next Diagonal

        CMP CH,DL
        JE DTRUE
        ; reseteia sucesso
        XOR DH,DH
        XOR CH,CH

        DI_SKIP:

        ; comeca no extremo direito
        XOR SI,SI
        MOV BX,2

        NDI:
        CMP AL,VELHA[BX][SI]
        JNE SALVA_DIN

        ; incrementa sucesso e compara
        INC CH

        JMP SEGUINTE_DIN
            SALVA_DIN:
            SALVA_COORDS
        SEGUINTE_DIN:
        ADD SI,DIM
        DEC BX
        INC DH
        CMP DH,DIM
        JB NDI ; Next Diagonal Inverted
        CMP CH,DL
        JE DTRUE
        JMP DFALSE

        DTRUE:
        MOV AL,1
        JMP DRETURN

        DFALSE:
        XOR AL,AL

        DRETURN:

        POP SI
        POP DX
        POP CX
        POP BX

        RET
    VER_DIAGONAIS ENDP

    RNG PROC

        ; retorna duas coordenadas aleatorias 1-3
        ; resultado: BX = rndm(0-2), SI = rndm(0-6)

        PUSH AX
        PUSH CX
        PUSH DX

        XOR BX,BX
        MOV AH,00h
        INT 1Ah
        ADD DX,12756

        T:
        TEST DH,03h
        JNZ NEXT_NUM
        ADD DH,138
        JMP T

        NEXT_NUM:
        AND DH,03h
        MOV BL,DH

        T2:
        TEST DL,03h
        JNZ NEXT_C
        ADD DL,117
        JMP T2

        NEXT_C:
        AND DX,0003h
        MOV SI,DX
        
        TO_ASM

        POP DX
        POP CX
        POP AX

        RET
    RNG ENDP

    PLAN_MOVE PROC

        ; Executa a jogada do CPU em base a sua difficuldade
        ; Difficuldade 1: pega uma coordenada aleatoria
        ; Difficuldade 2: mesmo que a anterior mas também tenta ganhar ou bloquear ao jogador
        ; Difficuldade 3: mesmo que a anterior mas sempre começa no meio ou nas esquinas

        CMP DIFFICULDADE,1
        JE RNDM

        CMP DIFFICULDADE,2
        JE WIN_OR_AVOID_LOSS

        WIN_OR_AVOID_LOSS:

        JMP TRY_WIN
        TRY_BLOCK:
        MOV CL,'X'
        TRY_WIN:

        MOV DL,2

        COL:
        CALL VER_COLUNAS
        CMP AL,1
        JE WINC
        LIN:
        CALL VER_LINHAS
        CMP AL,1
        JE WINL
        DIG:
        CALL VER_DIAGONAIS
        CMP AL,1
        JE WIND
        CMP CL,'O'
        JE TRY_BLOCK
        JMP RNDM

        WINC:
        GET_AND_COMPARE
        JE GO_BACK
        INC DI
        CMP DI,3
        JNE COL
        XOR DI,DI
        JMP LIN

        WINL:
        GET_AND_COMPARE
        JE GO_BACK
        INC DI
        CMP DI,3
        JNE LIN
        XOR DI,DI
        JMP DIG

        WIND:
        GET_AND_COMPARE
        JE GO_BACK
        INC DI
        CMP DI,2
        JNE DIG
        XOR DI,DI
        JMP RNDM

        RNDM:

        RPT:
        MOV CL,'O'
        CALL RNG
        CALL IS_OCCUPIED
        CMP AL,1
        JE RPT
        
        GO_BACK:
        XOR DI,DI
        MOV CL,'O'

        RET
    PLAN_MOVE ENDP

END MAIN

; Mudar RNG para uma chance que muda em base a "avaliable tiles"
; Otimizar ou organizar os VER_* PROCS e a lógica do WIN_OR_AVOID_LOSS

; FAZER CPU DIFICIL
; FAZER CPU IMPOSSIVEL
; FAZER EXTRAS COM DEFINIÇÃO DE DIMENSÕES DA MATRIZ
; OTIMIZAR