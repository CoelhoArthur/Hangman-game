:- module(game_logic, [selecaoJogar/0, jogar/0, escolhe_palavra_aleatoria/4]).
:- use_module(points).
:- use_module(db).
:- use_module(confrontos). 
:- use_module(sidequests).
:- use_module(library(http/json)).

% Início do jogo
selecaoJogar :- 
    clear_screen, 
    write('\nSelecione a opção desejada:\n'),
    write('1 - SINGLEPLAYER\n'),
    write('2 - MULTIPLAYER\n'),
    write('3 - Retomar jogo\n'),
    write('4 - Sair do jogo\n'),
    read_line_to_string(user_input, Option),
    mode_selection(Option).

mode_selection("1") :- jogar.
mode_selection("2") :- jogarMultiplayer.  % Placeholder para o futuro
mode_selection("3") :- retomar_jogo.
mode_selection("4") :- halt.
mode_selection(_) :- 
    write('Opção inválida! Tente novamente.\n'),
    selecaoJogar.

% Função para selecionar a dificuldade do jogo
seleciona_dificuldade(Dificuldade) :-
    write('Escolha o nível de dificuldade:\n'),
    write('1 - Fácil\n'),
    write('2 - Médio\n'),
    write('3 - Difícil\n'),
    read_line_to_string(user_input, Option),
    (   Option == "1" -> Dificuldade = facil;
        Option == "2" -> Dificuldade = medio;
        Option == "3" -> Dificuldade = dificil;
        write('Opção inválida! Tente novamente.\n'),
        seleciona_dificuldade(Dificuldade)
    ).

% Função para selecionar o tema
seleciona_tema(Tema) :-
    write('Escolha o tema:\n'),
    write('1 - Haskell\n'),
    write('2 - Python\n'),
    write('3 - Prolog\n'),
    read_line_to_string(user_input, Option),
    (   Option == "1" -> Tema = haskell;  % Usar átomos
        Option == "2" -> Tema = python;
        Option == "3" -> Tema = prolog;
        write('Opção inválida! Tente novamente.\n'),
        seleciona_tema(Tema)
    ).

% Função para iniciar o jogo
jogar :-
    write('Digite seu nome de usuário: '),
    read_line_to_string(user_input, PlayerName),
    (   find_user_by_name(PlayerName, UserData)
    ->  % Carregar a pontuação atual do jogador do JSON
        CurrentPoints = UserData.pontos,
        seleciona_tema(Tema),
        seleciona_dificuldade(Dificuldade),
        escolhe_palavra_aleatoria(Tema, Dificuldade, Palavra, Dica),
        atom_chars(Palavra, Letras),
        inicializa_forca(Letras, Espacos),
        jogar_forca(Letras, Espacos, 7, [], CurrentPoints, Palavra, PlayerName, Dica)
    ;   % Se o usuário não for encontrado, realizar cadastro
        write('Usuário não encontrado. Realizando cadastro...\n'),
        add_user(PlayerName),
        seleciona_tema(Tema),
        seleciona_dificuldade(Dificuldade),
        escolhe_palavra_aleatoria(Tema, Dificuldade, Palavra, Dica),
        atom_chars(Palavra, Letras),
        inicializa_forca(Letras, Espacos),
        jogar_forca(Letras, Espacos, 7, [], 0, Palavra, PlayerName, Dica)
    ).

% Função para iniciar o jogo multiplayer com bonecos e tentativas individuais
jogarMultiplayer :-
    write('Digite o nome do Jogador 1: '),
    read_line_to_string(user_input, Player1Name),
    (   find_user_by_name(Player1Name, UserData1)
    ->  Player1Points = UserData1.pontos,
        Player1Victories = UserData1.vitorias,
        Player1Defeats = UserData1.derrotas
    ;   write('Usuário não encontrado. Realizando cadastro para Jogador 1...\n'),
        add_user(Player1Name),
        Player1Points = 0,
        Player1Victories = 0,
        Player1Defeats = 0
    ),

    write('Digite o nome do Jogador 2: '),
    read_line_to_string(user_input, Player2Name),
    (   find_user_by_name(Player2Name, UserData2)
    ->  Player2Points = UserData2.pontos,
        Player2Victories = UserData2.vitorias,
        Player2Defeats = UserData2.derrotas
    ;   write('Usuário não encontrado. Realizando cadastro para Jogador 2...\n'),
        add_user(Player2Name),
        Player2Points = 0,
        Player2Victories = 0,
        Player2Defeats = 0
    ),

    seleciona_tema(Tema),
    seleciona_dificuldade(Dificuldade),

    escolhe_palavra_aleatoria(Tema, Dificuldade, Palavra, Dica),
    atom_chars(Palavra, Letras),
    inicializa_forca(Letras, Espacos1),
    inicializa_forca(Letras, Espacos2),

    TentativasIniciais = 7,
    jogar_forca_multiplayer(Letras, Espacos1, Espacos2, TentativasIniciais, TentativasIniciais, [], [], Player1Name, Player2Name, Dica).

% Loop principal do jogo multiplayer com atualização de pontos
jogar_forca_multiplayer(Letras, Espacos1, Espacos2, Tentativas1, Tentativas2, Tentadas1, Tentadas2, Player1Name, Player2Name, Dica) :-
    % Verifica condições de término antes do turno do Jogador 1
    (   Tentativas1 =< 0
    ->  % Jogador 1 perdeu
        clear_screen,
        format('Jogador ~w perdeu! Tentativas esgotadas.\n', [Player1Name]),
        format('Jogador ~w venceu!\n', [Player2Name]),
        update_defeats(Player1Name),
        update_victories(Player2Name),
        pause_and_continue,
        show_menu
    ;   \+ member('_', Espacos1)
    ->  % Jogador 1 ganhou
        clear_screen,
        format('Parabéns, ~w! Você ganhou! A palavra é:\n', [Player1Name]),
        escreve_palavra(Espacos1),
        update_victories(Player1Name),
        update_defeats(Player2Name),
        pause_and_continue,
        show_menu
    ;   % Turno do Jogador 1
        turno_jogador_multiplayer(Letras, Espacos1, Tentativas1, Tentadas1, Player1Name, Dica, NovoEspacos1, NovoTentativas1, NovoTentadas1),
        % Verifica condições de término após o turno do Jogador 1
        (   NovoTentativas1 =< 0
        ->  clear_screen,
            format('Jogador ~w perdeu! Tentativas esgotadas.\n', [Player1Name]),
            format('Jogador ~w venceu!\n', [Player2Name]),
            update_defeats(Player1Name),
            update_victories(Player2Name),
            pause_and_continue,
            show_menu
        ;   \+ member('_', NovoEspacos1)
        ->  clear_screen,
            format('Parabéns, ~w! Você ganhou! A palavra é:\n', [Player1Name]),
            escreve_palavra(NovoEspacos1),
            update_victories(Player1Name),
            update_defeats(Player2Name),
            pause_and_continue,
            show_menu
        ;   % Verifica condições de término antes do turno do Jogador 2
            (   Tentativas2 =< 0
            ->  clear_screen,
                format('Jogador ~w perdeu! Tentativas esgotadas.\n', [Player2Name]),
                format('Jogador ~w venceu!\n', [Player1Name]),
                update_defeats(Player2Name),
                update_victories(Player1Name),
                pause_and_continue,
                show_menu
            ;   \+ member('_', Espacos2)
            ->  clear_screen,
                format('Parabéns, ~w! Você ganhou! A palavra é:\n', [Player2Name]),
                escreve_palavra(Espacos2),
                update_victories(Player2Name),
                update_defeats(Player1Name),
                pause_and_continue,
                show_menu
            ;   % Turno do Jogador 2
                turno_jogador_multiplayer(Letras, Espacos2, Tentativas2, Tentadas2, Player2Name, Dica, NovoEspacos2, NovoTentativas2, NovoTentadas2),
                % Verifica condições de término após o turno do Jogador 2
                (   NovoTentativas2 =< 0
                ->  clear_screen,
                    format('Jogador ~w perdeu! Tentativas esgotadas.\n', [Player2Name]),
                    format('Jogador ~w venceu!\n', [Player1Name]),
                    update_defeats(Player2Name),
                    update_victories(Player1Name),
                    pause_and_continue,
                    show_menu
                ;   \+ member('_', NovoEspacos2)
                ->  clear_screen,
                    format('Parabéns, ~w! Você ganhou! A palavra é:\n', [Player2Name]),
                    escreve_palavra(NovoEspacos2),
                    update_victories(Player2Name),
                    update_defeats(Player1Name),
                    pause_and_continue,
                    show_menu
                ;   % Próxima rodada
                    jogar_forca_multiplayer(Letras, NovoEspacos1, NovoEspacos2, NovoTentativas1, NovoTentativas2, NovoTentadas1, NovoTentadas2, Player1Name, Player2Name, Dica)
                )
            )
        )
    ).

% Função que representa o turno de um jogador no multiplayer com atualização de pontos
turno_jogador_multiplayer(Letras, Espacos, Tentativas, Tentadas, PlayerName, Dica, NovoEspacos, NovoTentativas, NovoTentadas) :-
    % Obtém a pontuação atual do jogador
    find_user_by_name(PlayerName, UserData),
    CurrentPoints = UserData.pontos,
    clear_screen,
    format('Turno do Jogador: ~w\n', [PlayerName]),
    format('Dica: ~w\n', [Dica]),
    draw_hangman(Tentativas),
    writeln('Palavra atual:'),
    escreve_palavra(Espacos),
    format('Tentativas restantes: ~w\n', [Tentativas]),
    writeln('Letras já tentadas:'),
    escreve_palavra(Tentadas),
    writeln('Você deseja tentar "chutar" a palavra completa? (s/n)'),
    read_line_to_string(user_input, Chutar),
    (   Chutar == "s"
    ->  % Jogador opta por chutar a palavra completa
        writeln('Digite a palavra completa:'),
        read_line_to_string(user_input, ChutePalavra),
        atom_chars(ChutePalavra, ChuteLista),
        (   ChuteLista == Letras
        ->  % Jogador acertou a palavra
            clear_screen,
            format('Parabéns, ~w! Você acertou a palavra completa!\n', [PlayerName]),
            NovoEspacos = Letras,
            NovoTentativas = Tentativas,
            NovoTentadas = Tentadas,
            % Atualiza pontos e vitórias
            add_points(CurrentPoints, 50, _, PlayerName),
            pause_and_continue
        ;   % Jogador errou a palavra
            writeln('Palavra incorreta! Você perde uma tentativa e 10 pontos.'),
            NovoTentativas is Tentativas - 1,
            NovoEspacos = Espacos,
            NovoTentadas = Tentadas,
            subtract_points(CurrentPoints, 10, _, PlayerName),
            pause_and_continue
        )
    ;   % Jogador opta por chutar uma letra
        writeln('Digite uma letra:'),
        read_line_to_string(user_input, ChuteLetra),
        string_lower(ChuteLetra, ChuteLower),
        string_chars(ChuteLower, [Letra]),
        (   member(Letra, Tentadas)
        ->  writeln('Você já tentou essa letra. Tente novamente.'),
            pause_and_continue,
            turno_jogador_multiplayer(Letras, Espacos, Tentativas, Tentadas, PlayerName, Dica, NovoEspacos, NovoTentativas, NovoTentadas)
        ;   (   member(Letra, Letras)
            ->  % Jogador acertou a letra
                writeln('Acertou! Você ganhou 30 pontos.'),
                atualiza_palavra(Letras, Espacos, Letra, AtualizadoEspacos),
                NovoTentadas = [Letra|Tentadas],
                NovoTentativas = Tentativas,
                NovoEspacos = AtualizadoEspacos,
                add_points(CurrentPoints, 30, _, PlayerName),
                pause_and_continue
            ;   % Jogador errou a letra
                writeln('Letra incorreta! Você perde uma tentativa e 10 pontos.'),
                NovoTentativas is Tentativas - 1,
                NovoTentadas = [Letra|Tentadas],
                NovoEspacos = Espacos,
                subtract_points(CurrentPoints, 10, _, PlayerName),
                pause_and_continue
            )
        )
    ).

% Função para um jogador jogar sua vez
jogar_turno(PlayerName, Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, Dica, OpponentPoints, OpponentName, NovaTentativas, NovaTentativasFeitas, NovaEspacos, NovaPoints) :-
    clear_screen,
    format('~w, é sua vez!\n', [PlayerName]),
    writeln('Dica: '), writeln(Dica),
    draw_hangman(Tentativas),
    escreve_palavra(Espacos),
    format('Tentativas restantes: ~w\n', [Tentativas]),
    writeln('Letras já tentadas:'),
    escreve_palavra(TentativasFeitas),
    writeln('Você deseja tentar "chutar" a palavra completa? (s/n)'),
    read_line_to_string(user_input, Chutar),
    (   Chutar == "s"
    ->  writeln('Digite a palavra completa:'),
        read_line_to_string(user_input, ChutePalavra),
        atom_chars(ChutePalavra, ChuteLista),
        (   ChuteLista == Letras
        ->  clear_screen, writeln('Parabéns, você acertou a palavra completa!'),
            NovaTentativas is 0,
            NovaEspacos = Letras  % Todas as letras estão reveladas
        ;   writeln('Palavra incorreta! Você perde uma tentativa.'),
            NovaTentativas is Tentativas - 1,
            NovaTentativasFeitas = TentativasFeitas,
            NovaPoints = Points,
            NovaEspacos = Espacos  % Mantém os espaços inalterados
        )
    ;   writeln('Digite uma letra:'),
        read_line_to_string(user_input, Chute),
        string_chars(Chute, [Letra]),
        (   member(Letra, TentativasFeitas)
        ->  writeln('Você já tentou essa letra. Tente novamente.'),
            NovaTentativas = Tentativas,
            NovaTentativasFeitas = TentativasFeitas,
            NovaPoints = Points,
            NovaEspacos = Espacos  % Mantém os espaços inalterados
        ;   (   member(Letra, Letras)
            ->  writeln('Acertou!'),
                atualiza_palavra(Letras, Espacos, Letra, NovaEspacos),
                NovaTentativas = Tentativas,
                NovaTentativasFeitas = [Letra|TentativasFeitas],
                NovaPoints = Points
            ;   writeln('Letra incorreta!'),
                NovaTentativas is Tentativas - 1,
                NovaTentativasFeitas = [Letra|TentativasFeitas],
                NovaPoints = Points,
                NovaEspacos = Espacos  % Mantém os espaços inalterados
            )
        )
    ).

% Função que atualiza a palavra com a letra correta
atualiza_palavra(Letras, Espacos, Letra, NovaEspacos) :-
    maplist(replace_letter(Letra), Letras, Espacos, NovaEspacos).

replace_letter(Letra, Letra, '_', Letra) :- !.  % Substitui o espaço vazio pela letra
replace_letter(_, _, Espaco, Espaco).          % Mantém as letras já reveladas



% Escolhe uma palavra aleatória com base no tema e dificuldade e retorna a dica associada
escolhe_palavra_aleatoria(Tema, Dificuldade, Palavra, Dica) :-
    load_palavras(Tema, Dificuldade, ListaPalavras),
    random_member(WordDict, ListaPalavras),
    Palavra = WordDict.palavra,
    Dica = WordDict.dica.

% Função para carregar palavras do JSON
load_palavras(Tema, Dificuldade, Palavras) :-
    open('palavras.json', read, Stream),
    json_read_dict(Stream, JsonDict),
    close(Stream),
    get_dict(Tema, JsonDict.temas, TemaDict),
    get_dict(Dificuldade, TemaDict, Palavras).

% Inicializa os espaços da palavra
inicializa_forca([], []).
inicializa_forca([_|Resto], ['_'|EspacosResto]) :-
    inicializa_forca(Resto, EspacosResto).

% Salva o estado do jogo em um arquivo JSON.
pausar_jogo(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName, Dica) :-
    Estado = _{
        letras: Letras,
        espacos: Espacos,
        tentativas: Tentativas,
        tentativas_feitas: TentativasFeitas,
        pontos: Points,
        palavra: Palavra,
        jogador: PlayerName,
        dica: Dica
    },
    open('estado_jogo.json', write, Stream),
    json_write(Stream, Estado),
    close(Stream),
    writeln('Jogo pausado.'),
    pause_and_continue,
    show_menu.

% Função para retomar o jogo a partir do estado salvo
retomar_jogo :-
    (   exists_file('estado_jogo.json') ->
        open('estado_jogo.json', read, Stream),
        json_read_dict(Stream, EstadoJogo),
        close(Stream),
        % Verifique se todos os campos do estado do jogo foram carregados corretamente
        (   EstadoJogo.get(letras) \== _,
            EstadoJogo.get(espacos) \== _,
            EstadoJogo.get(tentativas) \== _,
            EstadoJogo.get(tentativas_feitas) \== _,
            EstadoJogo.get(pontos) \== _,
            EstadoJogo.get(palavra) \== _,
            EstadoJogo.get(jogador) \== _,
            EstadoJogo.get(dica) \== _ ->
            atom_chars(EstadoJogo.palavra, Letras),
            % Converter Espacos para átomos, se forem strings
            maplist(string_to_atom, EstadoJogo.espacos, Espacos),   
            TentativasFeitas = EstadoJogo.tentativas_feitas,
            jogar_forca(Letras, Espacos, EstadoJogo.tentativas, TentativasFeitas, 
                        EstadoJogo.pontos, EstadoJogo.palavra, EstadoJogo.jogador, EstadoJogo.dica)
        ;   writeln('Erro: O arquivo de estado está incompleto ou corrompido.'),
            pause_and_continue,
            show_menu
        )
    ;   writeln('Nenhum jogo salvo encontrado. Por favor, inicie um novo jogo.'),
        pause_and_continue,
        show_menu
    ).

% Loop do jogo
jogar_forca(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName, Dica) :-
    clear_screen,
    writeln('Dica: '), writeln(Dica),
    draw_hangman(Tentativas),
    writeln('Palavra atual:'),
    escreve_palavra(Espacos),
    format('Tentativas restantes: ~w~n', [Tentativas]),
    writeln('Letras já tentadas:'),
    escreve_palavra(TentativasFeitas),
    (   \+ member('_', Espacos) ->
        writeln('Parabéns, você ganhou! A palavra é:'),
        escreve_palavra(Espacos),
        format('Sua pontuação final é: ~w~n', [Points]),
        update_victories(PlayerName),
        pause_and_continue,
        show_menu
    ;   Tentativas =:= 0 ->
        writeln('Você perdeu! Tentativas esgotadas.'),
        writeln('A palavra era:'),
        writeln(Palavra),
        format('Sua pontuação final é: ~w~n', [Points]),
        update_defeats(PlayerName),
        pause_and_continue,
        show_menu
    ;   continuar_jogo(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName, Dica)
    ).

% Função auxiliar para continuar o jogo
continuar_jogo(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName, Dica) :-
    writeln('Você deseja tentar "chutar" a palavra completa? (s/n/q/pause)'),
    read_line_to_string(user_input, Chutar),
    (   Chutar == "s" -> 
        processar_chute_palavra(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName, Dica)
    ;   Chutar == "n" -> 
        processar_chute_letra(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName, Dica)
    ;   Chutar == "q" -> 
        processar_sidequest(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName, Dica)
    ;   Chutar == "pause" ->
        pausar_jogo(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName, Dica)
    ;   % Caso padrão para opção inválida
        writeln('Opção inválida'),
        sleep(1),
        jogar_forca(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName, Dica)
    ).

processar_chute_palavra(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName, Dica) :-
    writeln('Digite a palavra completa:'),
    read_line_to_string(user_input, ChutePalavra),
    atom_chars(ChutePalavra, ChuteLista),
    atom_chars(Palavra, PalavraLista),
    (   ChuteLista == PalavraLista ->  
        clear_screen,
        writeln('Dica: '), writeln(Dica),
        writeln('Parabéns, você acertou a palavra completa!'),
        length(Espacos, LetrasFaltando),
        PontosGanhos is 30 * LetrasFaltando,
        add_points(Points, PontosGanhos, NewPoints, PlayerName),
        escreve_palavra(Letras),
        format('Sua pontuação final é: ~w~n', [NewPoints]),
        update_victories(PlayerName),
        pause_and_continue,
        show_menu
    ;   writeln('Palavra incorreta! Você perde uma tentativa e 10 pontos.'),
        subtract_points(Points, 10, NewPoints, PlayerName),
        NovasTentativas is Tentativas - 1,
        pause_and_continue,
        jogar_forca(Letras, Espacos, NovasTentativas, TentativasFeitas, NewPoints, Palavra, PlayerName, Dica)
    ).

processar_chute_letra(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName, Dica) :-
    writeln('Digite uma letra:'),
    read_line_to_string(user_input, Chute),
    string_chars(Chute, [Letra]),
    (   member(Letra, TentativasFeitas) ->  
        writeln('Você já tentou essa letra. Tente novamente.'),
        pause_and_continue,
        jogar_forca(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName, Dica)
    ;   (   member(Letra, Letras) ->  
            writeln('Acertou!'),
            atualiza_palavra(Letras, Espacos, Letra, NovaPalavra),
            add_points(Points, 30, NewPoints, PlayerName),
            pause_and_continue,
            jogar_forca(Letras, NovaPalavra, Tentativas, [Letra|TentativasFeitas], NewPoints, Palavra, PlayerName, Dica)
        ;   writeln('Letra incorreta!'),
            NovasTentativas is Tentativas - 1,
            subtract_points(Points, 10, NewPoints, PlayerName),
            pause_and_continue,
            jogar_forca(Letras, Espacos, NovasTentativas, [Letra|TentativasFeitas], NewPoints, Palavra, PlayerName, Dica)
        )
    ).

processar_sidequest(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName, Dica) :-
    (   Tentativas < 7 -> 
        (   pergunta_sidequest -> 
            NovasTentativas is Tentativas + 1,
            jogar_forca(Letras, Espacos, NovasTentativas, TentativasFeitas, Points, Palavra, PlayerName, Dica)
        ;   jogar_forca(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName, Dica)
        )
    ;   writeln('Você ainda não possui erros.'),
        sleep(2),
        jogar_forca(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName, Dica)
    ).
     
% Escreve a palavra na tela
escreve_palavra([]) :- nl.
escreve_palavra([H|T]) :-
    write(H),
    write(' '),
    escreve_palavra(T).

% Desenha a forca com base no número de vidas restantes
draw_hangman(7) :-
    writeln("                                 ###############"),
    writeln("                                 #### FORCA ####"),
    writeln("                                 ###############"),
    writeln("                                 #      |      #"),
    writeln("                                 #             #"),
    writeln("                                 #             #"),
    writeln("                                 #             #"),
    writeln("                                 ###############").
draw_hangman(6) :-
    writeln("                                 ###############"),
    writeln("                                 #    ('-')    #"),
    writeln("                                 #             #"),
    writeln("                                 #             #"),
    writeln("                                 ###############").
draw_hangman(5) :-
    writeln("                                 ###############"),
    writeln("                                 #    ('-')    #"),
    writeln("                                 #      |      #"),
    writeln("                                 #             #"),
    writeln("                                 ###############").
draw_hangman(4) :-
    writeln("                                 ###############"),
    writeln("                                 #    ('-')    #"),
    writeln("                                 #     /|      #"),
    writeln("                                 #             #"),
    writeln("                                 ###############").
draw_hangman(3) :-
    writeln("                                 ###############"),
    writeln("                                 #    ('-')    #"),
    writeln("                                 #     /|\\     #"),
    writeln("                                 #             #"),
    writeln("                                 ###############").
draw_hangman(2) :-
    writeln("                                 ###############"),
    writeln("                                 #    ('-')    #"),
    writeln("                                 #     /|\\     #"),
    writeln("                                 #     /       #"),
    writeln("                                 ###############").
draw_hangman(1) :-
    writeln("                                 ###############"),
    writeln("                                 #    ('-')    #"),
    writeln("                                 #     /|\\     #"),
    writeln("                                 #     / \\     #"),
    writeln("                                 ###############").
draw_hangman(0) :-
    writeln("                                 ###############"),
    writeln("                                 #      |      #"),
    writeln("                                 #    (-.-)    #"),
    writeln("                                 #     /|\\     #"),
    writeln("                                 #     / \\     #"),
    writeln("                                 ###############").

% Limpa a tela
clear_screen :- tty_clear.

% Função de pausa para leitura
pause_and_continue :-
    write('Pressione ENTER para continuar...'),
    get_char(_).

%jogarMultiplayer :-
    % Placeholder para implementação futura do modo multiplayer
    write('Modo multiplayer em desenvolvimento.\n'),
    pause_and_continue,
    show_menu.