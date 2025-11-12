TITLE JOGO DA VELHA
.MODEL SMALL
include macros.inc
.STACK 100h
.DATA
    DIM EQU 3

    VELHA DB DIM DUP(DIM DUP('+'))

    LIMPA_TELA DB 20 DUP(10) ,'$'
    BEM_VINDO DB 'Jogo da Velha',10,13,10,13,'Modos de Jogo:',10,13,10,13,'1) Jogador Contra Jogador',10,13,'2) Jogador Contra CPU',10,13,'3) Extras...',10,13,'$'
    CREDITOS DB 'Criadores: Joao Vitor Schneider Avanzo | Giovanni Nicolas Tapia Rodriguez',10,13,'$'
    OUTROS DB 10,13,'1) Definir Tamanho da Matriz',10,13,'2) BARRICADE',10,13,'3) Voltar...',10,13,'$'
    MENU_MATRIZ DB 'Defina um tamanho para a sua matriz (3-9): $'
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
    COORD_STORAGE DW 0,0
    RNG_VELHO DW 0
.CODE
    MAIN PROC
        MOV AX,@DATA
        MOV DS,AX

        MENU_PRINCIPAL:

        PRINT LIMPA_TELA
        PRINT BEM_VINDO
        PRINT ESCOLHA

        MOV CL,'1'
        MOV CH,'3'
        CALL LEIA_E_VALIDA

        AND AL,0Fh
        CMP AL,3
        JE EXTRAS

        MOV MODO,AL

        CMP MODO,2
        JNE JOGO

        PRINT LIMPA_TELA
        PRINT DIFFICULDADES
        PRINT ESCOLHA

        MOV CL,'1'
        MOV CH,'4'
        CALL LEIA_E_VALIDA
        AND AL,0Fh
        MOV DIFFICULDADE,AL
        JMP JOGO

        EXTRAS:

        PRINT LIMPA_TELA
        PRINT CREDITOS
        PRINT OUTROS
        PRINT ESCOLHA

        MOV CL,'3'
        MOV CH,'3'
        CALL LEIA_E_VALIDA
        AND AL,0Fh

        CMP AL,1
        JE TAMANHO

        CMP AL,3
        JE MENU_PRINCIPAL

        TAMANHO:


        JOGO:
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

    PLAN_MOVE PROC

        ; Executa a jogada do CPU em base a sua difficuldade
        ; Difficuldade 1: pega uma coordenada aleatoria
        ; Difficuldade 2: mesmo que a anterior mas também tenta ganhar ou bloquear ao jogador
        ; Difficuldade 3: mesmo que a anterior mas sempre começa no meio ou nas esquinas
        ; Difficuldade 4: mesmo que a anterior mas se o jogador não esta por ganhar na terceira jogada, o CPU pega outro canto

        CMP DIFFICULDADE,1
        JE RNDM

        CMP DIFFICULDADE,2
        JE WIN_OR_AVOID_LOSS
        JMP STTGZ

        STRAT:
        CALL CHECK_FORKS
        CMP AL,1
        JE FOUND_MOVE

        STTGZ:

        CMP JOGADAS,1
        JA WIN_OR_AVOID_LOSS

        CALL STRATEGIZE
        JMP FOUND_MOVE

        WIN_OR_AVOID_LOSS:
        
        CALL CHECK_WINNING
        CMP AL,1
        JE FOUND_MOVE

        CMP DIFFICULDADE,4
        JNE RNDM

        CMP JOGADAS,3
        JNA STRAT

        RNDM:
        MOV CL,'O'
        MOV DI,3

        XOR SI,SI
        XOR BX,BX

        CALL RNG
        MOV BX,AX
        CALL RNG
        MOV SI,AX
        TO_ASM

        CALL IS_OCCUPIED
        CMP AL,1
        JE RNDM
        
        FOUND_MOVE:
        XOR DI,DI
        MOV CL,'O'

        RET
    PLAN_MOVE ENDP

    MATRIZP PROC

        ; imprime a matriz salva
        ; adiciona bordas nos lados para estilizar

        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        PUSH SI

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

        POP SI
        POP DX
        POP CX
        POP BX
        POP AX

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
        MUL CX           
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
            JAE LTRUE

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
        JAE DTRUE
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
        JAE DTRUE
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

        ; retorna um valor aleatório de 1 a DI
        ; se DI eh zero, retorna zero

        PUSH BX
        PUSH CX
        PUSH DX

        ; pega o tempo do sistema em milesimos de segundos
        MOV AH,00h
        ; guarda em CX:DX
        INT 1Ah

        ; pega o seed anterior e mistura com o timer de agora
        MOV AX,RNG_VELHO
        XOR AX,DX
        XOR AX,CX
        ; n aleatório constante
        ADD AX,037A1h

        ; seed (em AX) = seed * 25173 (constante aleatoria) + 13849 (const aleatoria)
        MOV BX,25173

        ; AX * BX guardado em DX:AX
        MUL BX
        ; outra const aleatoria
        ADD AX,13849
        ; salva o seed para o seguinte call da RNG
        MOV RNG_VELHO,AX

        ; reduz o seed (AX) para o intervalo de 1-DI
        XOR DX,DX
        TEST DI,DI
        JZ ZERO_RNG
        ; DX = resto (0-DI-1)
        DIV DI
        MOV AX,DX
        ; 1-DI
        INC AX

        JMP FINAL_RNG

    ZERO_RNG:
        XOR AX,AX

    FINAL_RNG:
        POP DX
        POP CX
        POP BX

        RET
    RNG ENDP

    CHECK_WINNING PROC
        ; Começa checando posivel vitoria ('O') first
        MOV CL,'O'
        ; DI determina a partir de qual coluna/linha/diagonal o escaneio acontece
        XOR DI,DI

        JMP TRY_WIN
        TRY_BLOCK:
        ; Agora checa posivel bloqueio do jogador
        MOV CL,'X'
        XOR DI,DI
        TRY_WIN:

        ; parametro para os VAR_ procs
        MOV DL,2

        COL:
        CALL VER_COLUNAS
        CMP AL,1
        JE WINC
        INC DI
        CMP DI,3
        JB COL
        XOR DI,DI

        LIN:
        CALL VER_LINHAS
        CMP AL,1
        JE WINL
        INC DI
        CMP DI,3
        JB LIN
        XOR DI,DI

        DIG:
        CALL VER_DIAGONAIS
        CMP AL,1
        JE WIND
        INC DI
        CMP DI,2
        JB DIG
        DIG_FALSE:
        CMP CL,'O'
        JE TRY_BLOCK
        JMP BACK_FALSE

        WINC:
        GET_AND_COMPARE
        JE GO_BACK
        INC DI
        CMP DI,3
        JB COL
        XOR DI,DI
        JMP LIN

        WINL:
        GET_AND_COMPARE
        JE GO_BACK
        INC DI
        CMP DI,3
        JB LIN
        XOR DI,DI
        JMP DIG

        WIND:
        GET_AND_COMPARE
        JE GO_BACK
        INC DI
        CMP DI,2
        JB DIG
        XOR DI,DI
        JMP DIG_FALSE

        BACK_FALSE:
        XOR AL,AL
        JMP RETURN_FALSE

        GO_BACK:
        MOV AL,1

        RETURN_FALSE:

        RET

    CHECK_WINNING ENDP

    STRATEGIZE PROC

        PUSH AX
        PUSH CX
        PUSH DX
        PUSH DI

        CWD
        MOV AX,DIM
        MOV CX,2
        DIV CX
        ADD AX,DX
        MOV BX,AX
        MOV SI,AX
        TO_ASM
        MOV CL,'O'
        CALL IS_OCCUPIED
        CMP AL,1
        JNE FOUND

        CMP VELHA[BX][SI],'X'
        JE NOT_THIS_STRAT
        CMP DIFFICULDADE,3
        JE NOT_THIS_STRAT
            PUSH DX
            MOV DL,2
            MOV CL,'X'
            CALL VER_DIAGONAIS
            MOV CL,'O'
            POP DX
            CMP AL,1
            JE PLACE_ADJACENT
        NOT_THIS_STRAT:

        MOV DI,2
        XOR SI,SI
        XOR BX,BX

        OUTRO_CANTO:
        CALL RNG
        CMP AX,1
        JE ZEROBX

        MOV BX,DIM
        JMP TOSI

        ZEROBX:
        OR BX,1

        MOV AX, RNG_VELHO
        XOR AX, BX
        ROR AX, 1
        ADD AX, 137
        MOV RNG_VELHO, AX

        TOSI:

        CALL RNG
        CMP AX,1
        JE ZEROSI

        MOV SI,DIM
        JMP TOASM

        ZEROSI:
        OR SI,1

        TOASM:
        TO_ASM

        CALL IS_OCCUPIED
        CMP AL,1
        JE OUTRO_CANTO
        JMP FOUND

        PLACE_ADJACENT:

        CALL FIND_ADJACENT

        FOUND:

        POP DI
        POP DX
        POP CX
        POP AX

        RET
    STRATEGIZE ENDP

    FIND_ADJACENT PROC

        CWD
        MOV AX,DIM
        MOV CX,2
        DIV CX
        ADD AX,DX
        MOV BX,AX
        MOV SI,AX
        TO_ASM

        CMP VELHA[BX][SI-3],'+'
        JNE NOT_HERE
        SUB SI,3
        JMP FOUND_ADJ

        NOT_HERE:
        CMP VELHA[BX][SI+3],'+'
        JNE NOT_HERE2
        ADD SI,3
        JMP FOUND_ADJ

        NOT_HERE2:
        CMP VELHA[BX-1][SI],'+'
        JNE NOT_HERE3
        SUB BX,1
        JMP FOUND_ADJ

        NOT_HERE3:
        ADD BX,1

        FOUND_ADJ:

        RET
    FIND_ADJACENT ENDP

    CHECK_FORKS PROC
        
        ; imprime a matriz salva
        ; adiciona bordas nos lados para estilizar

        PUSH CX
        PUSH DX
        PUSH DI

        XOR DX,DX
        XOR SI,SI

        MOV CH,DIM
        SCANEIA_LINHA:
        MOV CL,DIM
        XOR BX,BX
            SCANEIA_COLUNA:
                XOR DH,DH

                CMP VELHA[BX][SI],'+'
                JNE PROX_COORD

                MOV VELHA[BX][SI],'X'
                XOR DI,DI
                MOV DL,2

                PUSH CX
                MOV CL,'X'
                CALL TESTA_FORK
                POP CX
                CMP AL,1
                JE FORK

                PROX_COORD:

                INC BX
                DEC CL
            JNZ SCANEIA_COLUNA

        ADD SI,DIM
        DEC CH
        JNZ SCANEIA_LINHA
        XOR AL,AL
        JMP NO_FORK

        FORK:

        MOV VELHA[BX][SI],'O'
        MOV AL,1

        NO_FORK:

        POP DI
        POP DX
        POP CX

        RET
    CHECK_FORKS ENDP

    TESTA_FORK PROC

        CALL VER_COLUNAS
        CMP AL,1
        JNE CFL

        PUSH BX
        PUSH SI
        GET_AND_COMPARE
        POP SI
        POP BX
        JNE CFL

        INC DH

        CFL:
        CALL VER_LINHAS
        CMP AL,1
        JNE CFD

        PUSH BX
        PUSH SI
        GET_AND_COMPARE
        POP SI
        POP BX
        JNE CFD

        INC DH

        CFD:
        CALL VER_DIAGONAIS
        CMP AL,1
        JNE RESULT
        PUSH BX
        PUSH SI
        GET_AND_COMPARE
        POP SI
        POP BX
        JNE RESULT
        
        INC DH

        RESULT:
        CMP DH,2
        JAE FORK_TRUE

        XOR AL,AL
        JMP FORK_FALSE

        FORK_TRUE:
        MOV AL,1

        FORK_FALSE:

        MOV VELHA[BX][SI],'+'

        RET
    TESTA_FORK ENDP

END MAIN

; Otimizar ou organizar os VER_* PROCS e CHECK_WINNING

; FAZER EXTRAS 
; EXTRAS CPU IMPOSSIVEL
; EXTRAS DEFINIÇÃO DE DIMENSÕES DA MATRIZ
; EXTRAS BARRICADE

; OTIMIZAR
; 1000!!!!!!!!!!!!!