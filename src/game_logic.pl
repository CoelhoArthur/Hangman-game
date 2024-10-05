:- module(game_logic, [selecaoJogar/0, jogar/0, escolhe_palavra_aleatoria/2]).
:- use_module(points).
:- use_module(db).  % Certifique-se de importar o módulo db para manipular JSON

% Início do jogo
selecaoJogar :- 
    clear_screen, 
    write('\nSelecione a opção desejada:\n'),
    write('1 - SINGLEPLAYER\n'),
    write('2 - MULTIPLAYER\n'),
    write('3 - Sair do jogo\n'),
    read_line_to_string(user_input, Option),
    mode_selection(Option).

mode_selection("1") :- jogar.
mode_selection("2") :- jogarMultiplayer.  % Placeholder para o futuro
mode_selection("3") :- halt.
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

% Função para iniciar o jogo
jogar :-
    write('Digite seu nome de usuário: '),
    read_line_to_string(user_input, PlayerName),
    (   find_user_by_name(PlayerName, UserData)
    ->  % Carregar a pontuação atual do jogador do JSON
        CurrentPoints = UserData.pontos,
        seleciona_dificuldade(Dificuldade),
        escolhe_palavra_aleatoria(Dificuldade, Palavra),
        atom_chars(Palavra, Letras),
        inicializa_forca(Letras, Espacos),
        jogar_forca(Letras, Espacos, 7, [], CurrentPoints, Palavra, PlayerName)
    ;   % Se o usuário não for encontrado, realizar cadastro
        write('Usuário não encontrado. Realizando cadastro...\n'),
        add_user(PlayerName),
        seleciona_dificuldade(Dificuldade),
        escolhe_palavra_aleatoria(Dificuldade, Palavra),
        atom_chars(Palavra, Letras),
        inicializa_forca(Letras, Espacos),
        jogar_forca(Letras, Espacos, 7, [], 0, Palavra, PlayerName)
    ).

% Escolhe uma palavra aleatória com base na dificuldade
escolhe_palavra_aleatoria(facil, Palavra) :-
    palavras_faceis(ListaPalavras),
    random_member(Palavra, ListaPalavras).
escolhe_palavra_aleatoria(medio, Palavra) :-
    palavras_medias(ListaPalavras),
    random_member(Palavra, ListaPalavras).
escolhe_palavra_aleatoria(dificil, Palavra) :-
    palavras_dificeis(ListaPalavras),
    random_member(Palavra, ListaPalavras).

palavras_faceis(['gato', 'casa', 'sol', 'paz']).
palavras_medias(['prolog', 'tabela', 'computador', 'teoria']).
palavras_dificeis(['paralelepipedo', 'conhecimento', 'supercalifragilistico']).

% Inicializa os espaços da palavra
inicializa_forca([], []).
inicializa_forca([_|Resto], ['_'|EspacosResto]) :-
    inicializa_forca(Resto, EspacosResto).

% Loop do jogo
jogar_forca(_, Espacos, 0, TentativasFeitas, Points, Palavra, PlayerName) :-
    clear_screen,
    draw_hangman(0),
    writeln('Você perdeu! Tentativas esgotadas.'),
    writeln('A palavra era:'),
    writeln(Palavra),
    writeln('Letras tentadas:'),
    escreve_palavra(TentativasFeitas),
    format('Sua pontuação final é: ~w~n', [Points]),
    update_defeats(PlayerName),
    pause_and_continue,
    show_menu.

jogar_forca(Letras, Espacos, _, _, Points, Palavra, PlayerName) :-
    \+ member('_', Espacos),
    clear_screen,
    writeln('Parabéns, você ganhou! A palavra é:'),
    escreve_palavra(Espacos),
    format('Sua pontuação final é: ~w~n', [Points]),
    update_victories(PlayerName),
    pause_and_continue,
    show_menu.

jogar_forca(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName) :-
    clear_screen,
    draw_hangman(Tentativas),
    writeln('Palavra atual:'),
    escreve_palavra(Espacos),
    format('Tentativas restantes: ~w~n', [Tentativas]),
    writeln('Letras já tentadas:'),
    escreve_palavra(TentativasFeitas),
    writeln('Você deseja tentar "chutar" a palavra completa? (s/n)'),
    read_line_to_string(user_input, Chutar),
    (   Chutar == "s"
    ->  writeln('Digite a palavra completa:'),
        read_line_to_string(user_input, ChutePalavra),
        atom_chars(ChutePalavra, ChuteLista),
        atom_chars(Palavra, PalavraLista),
        (   ChuteLista == PalavraLista
        ->  clear_screen,
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
            jogar_forca(Letras, Espacos, NovasTentativas, TentativasFeitas, NewPoints, Palavra, PlayerName)
        )
    ;   % Se não quiser chutar a palavra, continua o jogo normalmente
        writeln('Digite uma letra:'),
        read_line_to_string(user_input, Chute),
        string_chars(Chute, [Letra]),
        (   member(Letra, TentativasFeitas)
        ->  writeln('Você já tentou essa letra. Tente novamente.'),
            pause_and_continue,
            jogar_forca(Letras, Espacos, Tentativas, TentativasFeitas, Points, Palavra, PlayerName)
        ;   (   member(Letra, Letras)
            ->  writeln('Acertou!'),
                atualiza_palavra(Letras, Espacos, Letra, NovaPalavra),
                add_points(Points, 30, NewPoints, PlayerName),
                pause_and_continue,
                jogar_forca(Letras, NovaPalavra, Tentativas, [Letra|TentativasFeitas], NewPoints, Palavra, PlayerName)
            ;   writeln('Letra incorreta!'),
                NovasTentativas is Tentativas - 1,
                subtract_points(Points, 10, NewPoints, PlayerName),  % Subtrai sempre 10 pontos
                pause_and_continue,
                jogar_forca(Letras, Espacos, NovasTentativas, [Letra|TentativasFeitas], NewPoints, Palavra, PlayerName)
            )
        )
    ).

% Atualiza os espaços da palavra com a letra chutada
atualiza_palavra([], [], _, []).
atualiza_palavra([Letra|Resto], ['_'|EspacosResto], Letra, [Letra|NovaPalavra]) :-
    atualiza_palavra(Resto, EspacosResto, Letra, NovaPalavra).
atualiza_palavra([Outro|Resto], [Espaco|EspacosResto], Letra, [Espaco|NovaPalavra]) :-
    Outro \= Letra,
    atualiza_palavra(Resto, EspacosResto, Letra, NovaPalavra).

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
    writeln("                                 #    ('-')__  #"),
    writeln("                                 #             #"),
    writeln("                                 ###############").
draw_hangman(4) :-
    writeln("                                 ###############"),
    writeln("                                 #  __('-')__  #"),
    writeln("                                 #             #"),
    writeln("                                 ###############").
draw_hangman(3) :-
    writeln("                                 ###############"),
    writeln("                                 #  __('-')__  #"),
    writeln("                                 #      |      #"),
    writeln("                                 ###############").
draw_hangman(2) :-
    writeln("                                 ###############"),
    writeln("                                 #  __('-')__  #"),
    writeln("                                 #      |      #"),
    writeln("                                 #     /       #"),
    writeln("                                 ###############").
draw_hangman(1) :-
    writeln("                                 ###############"),
    writeln("                                 #  __('-')__  #"),
    writeln("                                 #      |      #"),
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

jogarMultiplayer :- 
    % Placeholder para implementação futura do modo multiplayer
    write('Modo multiplayer em desenvolvimento.\n'),
    pause_and_continue,
    show_menu.
