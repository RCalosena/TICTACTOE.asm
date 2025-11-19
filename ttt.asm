TITLE JOGO DA VELHA
.MODEL SMALL
include macros.inc
.STACK 100h
.DATA
    ; Dimensões e criação do tabuleiro
    DIM EQU 3
    VELHA DB DIM DUP(DIM DUP('+'))

    ; Mensagens e elementos
    LIMPA_TELA DB 25 DUP(10) ,'$'
    BEM_VINDO DB 'Jogo da Velha',10,13,10,13,'Modos de Jogo:',10,13,10,13,'1) Jogador Contra Jogador',10,13,'2) Jogador Contra CPU',10,13,'3) Extras...',10,13,'$'
    CREDITOS DB 'Criadores: Joao Vitor Schneider Avanzo | Giovanni Nicolas Tapia Rodriguez',10,13,'$'
    OUTROS DB 10,13,'1) Definir tamanho do tabuleiro',10,13,'2) BARRICADE ',?,10,13,'3) Tirar delay do CPU ',?,10,13,'4) Voltar...',10,13,'$'
    MENU_MATRIZ DB 'Para definir o tamanho do tabuleiro, procure a constante ',10,13,34,'DIM EQU 3',34,'e muda para qualquer numero entre 1-9',10,13,10,13,'1) Voltar... $'
    ESCOLHA DB 10,13,'Escolha: $'
    DIFFICULDADES DB 10,13,10,13,'Difficuldades:',10,13,10,13,'1) Facil',10,13,'2) Medio',10,13,'3) Dificil',10,13,'4) Impossivel',10,13,'$'
    LINHA    DB 10,13,'Digite a linha (1-',?,'): $'
    COLUNA   DB 10,13,'Digite a coluna (1-',?,'): $'
    OCUPADA  DB 10,13,'Coordenada ocupada! Tente de novo.$'
    VENCEDOR  DB 10,13,'Jogador ',34,?,34,' venceu!$'
    EMPATE   DB 10,13,'empate!$'
    BORDA   DB ' | $'
    PENSANDO DB 'pensando',?,?,?,'$'

    ; Variaveis
    MODO DB 0
    DIFFICULDADE DB 0
    JOGADAS DB 0
    COORD_STORAGE DW 0,0
    RNG_VELHO DW 0
    BARR DB 0
    NO_THINK DB 0
    LAST_BARR DW 14 DUP(0)
.CODE
    MAIN PROC
        MOV AX,@DATA
        MOV DS,AX

        MENU_PRINCIPAL:
        ; Imprime o menu principal
        PRINT LIMPA_TELA
        PRINT BEM_VINDO
        PRINT ESCOLHA

        ; Le só as teclas 1,2,3
        MOV CL,'1'
        MOV CH,'3'
        CALL LEIA_E_VALIDA
        AND AL,0Fh
        ; Vai para o menu de Extras
        CMP AL,3
        JE EXTRAS
        ; 1 e 2 indicam o modo de jogo (jogador vs jogador, jogador vs cpu)
        MOV MODO,AL

        ; Começa o jogo se o modo de jogo eh 1
        CMP MODO,1
        JE JOGO

        ; Imprime o menu de difficuldades
        PRINT LIMPA_TELA
        PRINT DIFFICULDADES
        PRINT ESCOLHA

        ; só as teclas 1,2,3,4
        MOV CL,'1'
        MOV CH,'4'
        CALL LEIA_E_VALIDA
        ; resultado == nivel de difficuldade
        AND AL,0Fh
        MOV DIFFICULDADE,AL
        ; começa
        JMP JOGO

        EXTRAS:

        CALL MENU_EXTRAS
        JMP MENU_PRINCIPAL

        JOGO:

        ; Numero maximo aceitado pelo teclado = DIM
        MOV CL,DIM
        OR CL,30h
        MOV COLUNA[21],CL
        MOV LINHA[20],CL

        ; CH define de quem eh o turno
        ; CL guarda o Simbolo do jogador atual
        XOR CX,CX

        ; Parametro para alguns procedimentos
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
            ; se o modo de jogo eh 2, 'O' eh o CPU
            CMP MODO,2
            JNE SETUP
            ; Intelligência do CPU
            CALL PLAN_MOVE
            ; Pula leitura dos inputs do jogador 'O'
            JMP CPU_SKIP

            SETUP:

            ; input para a coluna e linha
            ; caracteres fora de 1-DIM sao omitidos
            PUSH CX
            PRINT COLUNA
            MOV CL,'1'
            MOV CH,DIM
            OR CH,30h
            CALL LEIA_E_VALIDA
            ; transforma em numero e guarda em BX (apontador de coluna)
            AND AX,000Fh
            MOV BX,AX
            PRINT LINHA
            CALL LEIA_E_VALIDA
            ; transforma em numeri e guarda em SI (apontador de linha)
            AND AX,000Fh
            MOV SI,AX
            POP CX
            ; transforma BX e SI em coordenadas legíveis pelo programa (ex: 1,2 --> 0,3)
            TO_ASM
            ; verifica se a posição ja foi ocupada
            CALL IS_OCCUPIED
            ; se verdadeiro, volte ao começo
            CMP AL,1
            JE COMECO

            CPU_SKIP:
            ; coloca o X ou O na posição escolhida
            MOV VELHA[BX][SI],CL

            ; dois espaços
            PULA_LINHA
            PULA_LINHA

            ; input dos procedimentos VER_
            MOV DL,DIM
            ; passa por colunas linhas e diagonais para verificar se alguem ganhou
            CALL VER_COLUNAS
            CMP AL,1
            JE VITORIA
            CALL VER_LINHAS
            CMP AL,1
            JE VITORIA
            CALL VER_DIAGONAIS
            CMP AL,1
            JE VITORIA

            ; Muda de quem eh o turno
            INC CH
            ; incrementa o numero de jogadas
            MOV AL,JOGADAS
            INC AL
            MOV JOGADAS,AL
            CALL VERIFICA_EMPATE
            CMP AL,1
            JE DRAW

            ; coloca barricadas se o modo foi ativado
            CMP BARR,1
            JNE NO_BARR
                CALL PLACE_BARR
            NO_BARR:

            ; imprime matriz
            CALL MATRIZP

        JMP COMECO

        ; tipo de finalização

        ; imprime empate e finaliza
        DRAW:
        CALL MATRIZP
        PRINT EMPATE
        JMP FINAL

        ; imprime mensagem do ganhador e finaliza
        VITORIA:
        CALL MATRIZP
        ; coloca o simbolo do jogador que ganhou na mensagem
        MOV VENCEDOR[11],CL
        PRINT VENCEDOR

        FINAL:
        MOV AH,4Ch
        INT 21h
    MAIN ENDP

    MENU_EXTRAS PROC
        
        ; menu de extras

        XTRAS:

        ; Imprime o menu de extras
        PRINT LIMPA_TELA
        PRINT CREDITOS
        PRINT OUTROS
        PRINT ESCOLHA

        ; opções 1-4
        MOV CL,'1'
        MOV CH,'4'
        CALL LEIA_E_VALIDA
        AND AL,0Fh

        ; 1 = menu tamanho da tela
        CMP AL,1
        JE TAMANHO
        ; 2 = liga/desliga modo BARRICADE
        CMP AL,2
        JE TOGGLE_BARR
        ; 3 = liga/desliga pensamento do computador
        CMP AL,3
        JE TOGGLE_THINK
        JMP VOLTA

        TAMANHO:
        PRINT LIMPA_TELA
        PRINT MENU_MATRIZ
        MOV CL,'1'
        MOV CH,'1'
        CALL LEIA_E_VALIDA
        JMP XTRAS

        TOGGLE_BARR:
        TOGGLE BARR 48
        JMP XTRAS

        TOGGLE_THINK:
        TOGGLE NO_THINK 73
        JMP XTRAS

        VOLTA:

        RET
    MENU_EXTRAS ENDP

    LEIA_E_VALIDA PROC

        ; inputs:
        ;   CL = valor minimo
        ;   CH = valor maximo

        ; le do teclado
        ; verifica se o caracter lido eh de CL a CH
        ; caso contrário, apaga o caracter anterior e lê de novo

        PUSH DX
        JMP READ

        VALIDA_DE_NOVO:

            CMP AL,CL
            JL INVALIDO
            CMP AL,CH
            JA INVALIDO
            JMP VALIDO

            INVALIDO:
                ; imprime backspace, espaço e backspace de novo para apagar o caracter
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

        ; '+' eh uma casa livre
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

    PLACE_BARR PROC

        ; coloca barricadas em pontos aleatorios do tabuleiro
        ; n de barricadas muda com o tamanho da matriz (BARR = DIM - 2)
        ; a posição das barricadas eh salva em LAST_BARR 
        ; para depois remove-las e colocar novas a proxima vez que PLACE_BARR for chamado

        PUSHALL
        PUSH DI

        ; q de barrs a serem removidas
        MOV CX,DIM
        SUB CX,2
        XOR DI,DI

        ; pega pos das ultimas barrs no LAST_BARR
        REMOVE:
            MOV BX,LAST_BARR[DI]
            MOV SI,LAST_BARR[DI+2]
            AND BX,00FFh
            AND SI,00FFh
            ; se não tiver uma barreira na pos, para o loop
            CMP VELHA[BX][SI],178
            JNE STOP

            ; apaga (coloca um + onde a barreira tava)
            MOV VELHA[BX][SI],'+'
            ; seguinte item no LAST_BARR
            ADD DI,4
        LOOP REMOVE

        ; se o tabuleiro e menor que 3x3, barricadas não são colocadas
        MOV AL,DIM
        CMP AL,3
        JB BARRICADED

        STOP:
        ; q de barrs para colocar
        MOV CX,DIM
        SUB CX,2
        XOR DI,DI

        PLACE:
        XOR SI,SI
        XOR BX,BX

        ; pega coordenadas aleatorias
        PUSH DI
        MOV DI,DIM
        CALL RNG
        MOV BX,AX
        CALL RNG
        MOV SI,AX
        TO_ASM
        POP DI

        ; evita colocar em cima do jogador na sua primeira jogada
        CMP JOGADAS,2
        JA PLACE_ANYWHERE
        CMP VELHA[BX][SI],'+'
        JNE PLACE
        PLACE_ANYWHERE:
        ; evita colocar em cima de outra barreira
        CMP VELHA[BX][SI],178
        JE PLACE
        
        ; coloca a barr e salva as coordenadas
        MOV VELHA[BX][SI],178
        MOV LAST_BARR[DI],BX
        MOV LAST_BARR[DI+2],SI

        ; repete até CX (q de barrs) ser 0
        ADD DI,4
        LOOP PLACE

        BARRICADED:
        POP DI
        POPALL

        RET
    PLACE_BARR ENDP

    PLAN_MOVE PROC

        ; Executa a jogada do CPU em base a sua difficuldade
        ; As difficuldades são camadas, ou seja, se uma lógica não funcionar, utilizará a lógica da difficuldade anterior e assim por diante

        ; Difficuldade 1: pega uma coordenada aleatoria
        ; Difficuldade 2: tenta ganhar ou bloquear ao jogador
        ; Difficuldade 3: sempre começa no meio ou nas esquinas
        ; Difficuldade 4: procura e bloqueia situações de dupla vitória do jogador

        ; pensa por 2 segundos (pode ser desligado no EXTRAS)
        CMP NO_THINK,1
        JE NAOPENSA
            CALL DELAY
        NAOPENSA:

        ; pula para a lógica da difficuldade 1
        CMP DIFFICULDADE,1
        JE RNDM

        ; pula para a logica da difficuldade 2
        CMP DIFFICULDADE,2
        JE WIN_OR_AVOID_LOSS

        ; se difficuldade > 2 e eh a primeira jogada, escolha um lugar estratégico para começar
        CMP JOGADAS,1
        JA WIN_OR_AVOID_LOSS
        STTGZ:
        ; meio, cantos, laterais ou verifica dupla vitoria
        CALL STRATEGIZE
        JMP FOUND_MOVE

        ; ganha ou evita perder
        WIN_OR_AVOID_LOSS:
        CALL CHECK_WINNING
        CMP AL,1
        JE FOUND_MOVE

        ; se não achou uma jogada e não eh o CPU impossível, pega uma coordenada aleatoria
        CMP DIFFICULDADE,4
        JNE RNDM

        ; se difficuldade == 4 e jogadas < 3, pega um lugar estratégico de novo
        CMP JOGADAS,3
        JNA STTGZ

        ; se > 3 jogadas, procura evitar vitorias duplas do jogador
        CALL CHECK_FORKS
        CMP AL,1
        JE FOUND_MOVE

        ; gera um numero aleatório entre 1-DIM
        RNDM:
        MOV CL,'O'
        XOR SI,SI
        XOR BX,BX

        ; gera as coordenadas
        MOV DI,DIM
        CALL RNG
        MOV BX,AX
        CALL RNG
        MOV SI,AX
        TO_ASM

        ; tenta de novo se as coordenadas estão ocupadas
        CALL IS_OCCUPIED
        CMP AL,1
        JE RNDM
        
        FOUND_MOVE:
        MOV CL,'O'
        XOR DI,DI

        RET
    PLAN_MOVE ENDP

    MATRIZP PROC

        ; imprime a matriz salva
        ; adiciona bordas nos lados para estilizar

        PUSHALL
        PUSH SI

        ; linha
        XOR SI,SI

        ; ch = linhas restantes
        MOV CH,DIM
        MOV AH,2
        LINHA_LOOP:
            ; cl = colunas restantes
            MOV CL,DIM
            ; coluna
            XOR BX,BX
                COLUNA_LOOP:
                    ; imprime borda
                    PUSH AX
                    PRINT BORDA
                    POP AX

                    ; imprime valor na matriz
                    MOV AL,VELHA[BX][SI]
                    MOV DL,AL
                    INT 21h

                    ; seguinte coluna
                    INC BX
                    DEC CL
                JNZ COLUNA_LOOP

            PUSH AX
            PRINT BORDA
            PULA_LINHA
            POP AX

            ; seguinte linha
            ADD SI,DIM
            DEC CH
        JNZ LINHA_LOOP

        POP SI
        POPALL

        RET
    MATRIZP ENDP

    VER_COLUNAS PROC

        ; inputs:
        ;   CL = ASCII a comparar
        ;   DL = Meta
        ;   DI = Começa a partir da (DI)a coluna

        ; outputs:
        ;   AL = 0 ou 1
        ;   COORD_STORAGE = BX,SI

        ; escaneia as colunas por um valor igual a CL
        ; cada sucesso incrementa o contador CH
        ; se CH é maior ou igual à meta DL o procedimento retorna verdadeiro (AL = 1)
        ; coordenadas com valores diferentes de CL são guardadas no COORD_STORAGE

        PUSH BX
        PUSH CX
        PUSH DX
        PUSH SI

        ; linha
        XOR SI,SI
        ; contador de sucesso
        XOR CH,CH
        ; contador do loop
        XOR DH,DH

        ; começa a partir da coluna indicada por DI
        MOV BX,DI

        ; salva o simbolo do jogador em al
        MOV AL,CL
        ; contador de scans
        MOV CL,DIM
        COUNT_F:
            CMP AL,VELHA[BX][SI]
            JNE SALVA_C

            ; incrementa
            INC CH
            
            ; seguinte fileira
            JMP SEGUINTE_F
                SALVA_C:
                ; guarda a posição
                SALVA_COORDS
            SEGUINTE_F:
            ADD SI,DIM
            INC DH
            CMP DH,DIM
            JB COUNT_F

            ; se a meta for atingida, termina o loop
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

        ; outputs:
        ;   AL = 0 ou 1
        ;   COORD_STORAGE = BX,SI

        ; escaneia as linhas por um valor igual a CL
        ; cada sucesso incrementa o contador CH
        ; se CH é maior ou igual à meta DL o procedimento retorna verdadeiro (AL = 1)
        ; coordenadas com valores diferentes de CL são guardadas no COORD_STORAGE

        PUSH BX
        PUSH CX
        PUSH DX
        PUSH SI

        ; linha
        XOR SI,SI
        ; coluna
        XOR BX,BX
        ; sucessos
        XOR CH,CH
        ; n de scans
        XOR DH,DH

        ; SI começa a partir do especificado em DI, então:
        ; SI = DI * DIM
        PUSHALL
        MOV AX,DI        
        MOV CX,DIM
        MUL CX           
        MOV SI,AX
        POPALL    

        ; guarda o jogador em al
        MOV AL,CL
        ; contador de linhas
        MOV CL,DIM
        COUNT_L:
            CMP AL,VELHA[BX][SI]
            JNE SALVA_L

            ; incrementa
            INC CH

            JMP SEGUINTE_L
                SALVA_L:
                SALVA_COORDS
            SEGUINTE_L:
            INC BX
            INC DH
            CMP DH,DIM
            JB COUNT_L

            ; se a meta eh atingida, termina o proc
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

        ; outputs:
        ;   AL = 0 ou 1
        ;   COORD_STORAGE = BX,SI

        ; escaneia as diagonais por um valor igual a CL
        ; cada sucesso incrementa um contador CH
        ; se CH é maior ou igual à meta DL o procedimento retorna verdadeiro (AL = 1)
        ; coordenadas com valores diferentes de CL são guardadas no COORD_STORAGE

        PUSH BX
        PUSH CX
        PUSH DX
        PUSH SI

        ; coordenadas
        XOR SI,SI
        XOR BX,BX

        ; sucessos
        XOR CH,CH
        ; loop
        XOR DH,DH
        ; simbolo do jogador em al
        MOV AL,CL
        ; se DI == 1, escaneia só a diagonal espelhada
        CMP DI,1
        JE DI_SKIP

        ND:
        CMP AL,VELHA[BX][SI]
        JNE SALVA_D

        ; incrementa sucesso
        INC CH

        ; seguinte diagonal
        JMP SEGUINTE_D
            SALVA_D:
            ; guarda a coordenada
            SALVA_COORDS
        SEGUINTE_D:
        ADD SI,DIM
        INC BX
        INC DH
        CMP DH,DIM
        JB ND ; Next Diagonal

        ; veja se atingiu a meta
        CMP CH,DL
        JAE DTRUE
        ; reseteia sucesso
        XOR DH,DH
        XOR CH,CH

        DI_SKIP:
        ; mesmo escaneio mas comeca no extremo direito
        XOR SI,SI
        MOV BX,DIM
        DEC BX

        NDI:
        CMP AL,VELHA[BX][SI]
        JNE SALVA_DIN

        ; inc sucesso
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

    VERIFICA_EMPATE PROC
        
        ; Verifica se não tem mais casas livres

        PUSH BX
        PUSH CX
        PUSH SI

        XOR AX,AX

        ; linha
        XOR SI,SI
        ; ch = linhas restantes
        MOV CH,DIM
        LINHA_SCAN:
        ; cl = colunas restantes
        MOV CL,DIM
        ; coluna
        XOR BX,BX
            COLUNA_SCAN:
                ; se tem uma casa livre, não tem empate
                CMP VELHA[BX][SI],'+'
                JE NODRAW
                ; seguinte coluna
                INC BX
                DEC CL
            JNZ COLUNA_SCAN
        ; seguinte linha
        ADD SI,DIM
        DEC CH
        JNZ LINHA_SCAN

        ; teve empate
        MOV AL,1
        JMP YESDRAW

        NODRAW:
        XOR AL,AL

        YESDRAW:
        POP SI
        POP CX
        POP BX

        RET
    VERIFICA_EMPATE ENDP

    RNG PROC

        ; retorna com um valor aleatório de 1 a DI em AX
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
        ; Começa checando posivel vitoria ('O')
        MOV CL,'O'
        ; DI determina a partir de qual coluna/linha/diagonal o escaneio acontece
        XOR DI,DI

        JMP TRY_WIN
        TRY_BLOCK:
        ; Agora checa possivel bloqueio do jogador
        MOV CL,'X'
        XOR DI,DI
        TRY_WIN:
        ; parametro para os VAR_ procs
        MOV DL,DIM
        DEC DL

        ; verifica se tem dois simbolos consecutivos nas colunas
        COL:
        CALL VER_COLUNAS
        CMP AL,1
        JE WINC
        ; escaneia de novo a partir de outra coluna 
        ; (evita que não leia algumas possíveis vitórias/bloqueios)
        INC DI
        ; até di chegar ao limite
        CMP DI,DIM
        JB COL
        XOR DI,DI

        ; mesma logica para as linhas
        LIN:
        CALL VER_LINHAS
        CMP AL,1
        JE WINL
        ; escaneia todas as linhas
        INC DI
        CMP DI,DIM
        JB LIN
        XOR DI,DI

        ; mesma logica para as diagonais
        DIG:
        CALL VER_DIAGONAIS
        CMP AL,1
        JE WIND
        ; escaneia todas as diagonais
        INC DI
        CMP DI,2
        JB DIG

        DIG_FALSE:
        ; se não achou vitoria, tenta bloquear, se não achou, o procedimento retorna falso
        CMP CL,'O'
        JE TRY_BLOCK
        JMP BACK_FALSE

        ; se VER_COLUNAS retorna verdadeiro
        WINC:
        ; pega as ultimas coordenadas salvas (candidato para colocar o simbolo do CPU)
        GET_AND_COMPARE ; compara com '+' no macro, se for igual, achou uma coordenada valida
        JE GO_BACK
        INC DI ; se a coordenada não for livre, verifica de novo a partir de outra coluna
        CMP DI,DIM
        JB COL
        XOR DI,DI ; vai para o escaneio de linhas
        JMP LIN

        ; se VER_LINHAS retorna verdadeiro
        ; mesma lógica que o anterior
        WINL:
        GET_AND_COMPARE
        JE GO_BACK
        INC DI
        CMP DI,DIM
        JB LIN
        XOR DI,DI
        JMP DIG

        ; se VER_DIAGONAIS retorna verdadeiro
        ; mesma lógica que o anterior
        WIND:
        GET_AND_COMPARE
        JE GO_BACK
        INC DI
        CMP DI,2
        JB DIG
        XOR DI,DI
        JMP DIG_FALSE

        ; retorna falso
        BACK_FALSE:
        XOR AL,AL
        JMP RETURN_FALSE

        ; retorna verdadeiro
        GO_BACK:
        MOV AL,1

        RETURN_FALSE:
        RET

    CHECK_WINNING ENDP

    STRATEGIZE PROC

        ; Procura posições estratégicas no tabuleiro

        PUSH AX
        PUSH CX
        PUSH DX
        PUSH DI

        ; calcule o meio da matriz (necessário para matrizes de diferentes tamanhos)
        ; cociente de (DIM / 2) + resto
        CENTRO
        ; verifica se o meio ja foi pegado
        MOV CL,'O'
        CALL IS_OCCUPIED
        CMP AL,1
        JNE FOUND

        ; se o meio contem um X ou a difficuldade eh 3 (difficil), pula essa lógica
        CMP VELHA[BX][SI],'X'
        JE NOT_THIS_STRAT
        CMP DIFFICULDADE,3
        JE NOT_THIS_STRAT

            ; analisa se o jogador pegou dois cantos opostos
            CALL ANALISE
            ; se o simbolo esta no meio, coloca numa lateral
            CMP AL,1
            JE PLACE_ADJACENT
            ; se o simbolo do CPU nao esta no meio, procura situações de "dois vitorias"
            CMP VELHA[BX][SI],'O'
            JE NOT_THIS_STRAT

            ; veja se o jogador pode ter uma dupla vitoria
            CALL CHECK_FORKS
            CMP AL,1
            JE FOUND

        NOT_THIS_STRAT:

        ; coordenadas
        XOR SI,SI
        XOR BX,BX

        ; pega qualquer canto
        ; se RNG = 1, coordenada == 0, se RNG == 2, coordenada == 3
        OUTRO_CANTO:
        ; parametro para RNG
        MOV DI,2
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

        ; coloca numa lateral
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

        ; aponta a uma coordenada das laterais da matriz

        ; calcule o centro da matriz
        CENTRO

        ; veja se as laterais estão ocupadas
        CMP VELHA[BX][SI-DIM],'+'
        JNE NOT_HERE
        SUB SI,DIM
        JMP FOUND_ADJ

        NOT_HERE:
        CMP VELHA[BX][SI+DIM],'+'
        JNE NOT_HERE2
        ADD SI,DIM
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
        
        ; Verifica se alguma jogada futura do jogador vai terminar com duas possíveis vitorias

        PUSH CX
        PUSH DX
        PUSH DI

        XOR DX,DX
        ; linha
        XOR SI,SI
        MOV CH,DIM
        SCANEIA_LINHA:
            ; coluna
            XOR BX,BX
            MOV CL,DIM
                SCANEIA_COLUNA:
                    ; se ja foi ocupada a cordenada, pula para a seguinte
                    CMP VELHA[BX][SI],'+'
                    JNE PROX_COORD

                    ; simula que o jogador colocou X nessa coordenada
                    MOV VELHA[BX][SI],'X'

                    ; verifica se tem duas possiveis vitorias
                    PUSH CX
                    MOV CL,'X'
                    CALL TESTA_FORK
                    ; tira o X simulado após a verificação
                    MOV VELHA[BX][SI],'+'
                    POP CX
                    ; achou fork
                    CMP AL,1
                    JE FORK

                    ; seguinte coordenada
                    PROX_COORD:
                    INC BX
                    DEC CL
                JNZ SCANEIA_COLUNA

            ; seguinte linha
            ADD SI,DIM
            DEC CH
        JNZ SCANEIA_LINHA

        ; retorna falso se não achou forks
        XOR AL,AL
        JMP NO_FORK

        ; retorna verdadeiro se achou
        FORK:
        ; bloqueia o fork
        MOV VELHA[BX][SI],'O'
        MOV AL,1

        NO_FORK:
        POP DI
        POP DX
        POP CX

        RET
    CHECK_FORKS ENDP

    TESTA_FORK PROC

        ; input:
        ;   CL = jogador
        ; output:
        ;   AL = 0-1

        ; Verifica se existe mais de uma possível vitória do jogador

        ; contador de possiveis vitorias
        XOR DH,DH
        ; parametros dos procedimentos VER_ (DI = começo, DL = meta)
        XOR DI,DI
        MOV DL,DIM
        DEC DL
        
        ; checa colunas
        CALL VER_COLUNAS
        CMP AL,1
        JNE CFL ; ve linhas se falso

        ; checa se a ultima coordenada eh livre
        PUSH BX
        PUSH SI
        GET_AND_COMPARE
        POP SI
        POP BX
        JNE CFL ; ve linhas se a coordenada estiver ocupada

        ; têm possivel vitoria. Incrementa DH
        INC DH

        ; checa linhas
        CFL:
        CALL VER_LINHAS
        CMP AL,1
        JNE CFD

        ; checa se a ultima coordenada eh livre
        PUSH BX
        PUSH SI
        GET_AND_COMPARE
        POP SI
        POP BX
        JNE CFD

        ; têm possivel vitoria. Incrementa DH
        INC DH

        ; checa diagonais
        CFD:
        CALL VER_DIAGONAIS
        CMP AL,1
        JNE RESULT

        ; checa se a ultima coordenada eh livre
        PUSH BX
        PUSH SI
        GET_AND_COMPARE
        POP SI
        POP BX
        JNE RESULT
        
        ; têm possivel vitoria. Incrementa DH
        INC DH

        ; calcule o resultado
        RESULT:
        CMP DH,2
        JAE FORK_TRUE

        ; retorna falso
        XOR AL,AL
        JMP FORK_FALSE

        ; retorna verdadeiro
        FORK_TRUE:
        MOV AL,1

        FORK_FALSE:
        RET
    TESTA_FORK ENDP

    ANALISE PROC
        
        ; verifica se as diagonais têm mais de um X
        ; retorna também as últimas coordenadas salvas

        PUSH DX
        MOV DL,DIM
        DEC DL
        MOV CL,'X'
        CALL VER_DIAGONAIS
        MOV CL,'O'
        POP DX
        GET_AND_COMPARE

        RET
    ANALISE ENDP

    DELAY PROC

        ; força o CPU a 'pensar' por 2 segundos 
        ; (delay de 2 segundos que evita jogadas instantâneas do CPU)

        PUSHALL
        PUSH SI
        PUSH DI

        ; system time
        MOV AH,00h
        INT 1Ah
        MOV BX,DX ; salva o tempo registrado em BX
        MOV SI,8 ; apontador do vetor (PENSANDO)

        ; feedback para o usuario ("PENSANDO")
        PRINT PENSANDO

        ; Timer para a animação do PENSANDO
        MOV CX,9
        PUSH CX

    WAIT_LOOP:
        ; pega o sys time de novo
        MOV AH,00h
        INT 1Ah
        SUB DX,BX ; DX = quantidade de tempo
    JB WAIT_LOOP ; se o resultado eh negativo, tenta de novo

        ; animação da mensagem
        POP CX
        CMP CX,36 ; se timer >= 2s, pula
        JAE NODOT
        CMP DX,CX ; quando o delay chegar a CX segundos, coloca um ponto novo na msg e imprime de novo
        JNE NODOT
        MOV PENSANDO[SI],'.'
        INC SI
        ADD CX,9
        APAGA_LINHA
        PUSH DX
        PRINT PENSANDO
        POP DX
        NODOT:
        PUSH CX


    CMP DX,36 ; espera aprox 2 segundos (2 * 18 ticks)
    JB WAIT_LOOP ; espera até o delay terminar

        ; apaga os pontos colocados no vetor PENSANDO
        MOV SI,8
        REM_DOT:
            MOV PENSANDO[SI],?
            INC SI
            CMP SI,10
        JBE REM_DOT
        APAGA_LINHA

        POP CX
        POP DI
        POP SI
        POPALL
        RET
    DELAY ENDP
END MAIN