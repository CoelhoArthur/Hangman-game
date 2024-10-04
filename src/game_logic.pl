:- module(game_logic, selecaoJogar/0, [jogar/0],selecaoJogar/0).

% Palavra a ser adivinhada
palavra('prolog').

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
mode_selection("2") :- jogarMultiplayer. %TODO
mode_selection("3") :- halt.
mode_selection(_) :- 
    write('Opção inválida! Tente novamente.\n'),
    selecaoJogar.

%TODO: PARTE DE PEGAR O USER DO JSON
% IMAGINO QUE O MÉTODO find_user_by_name PODERÁ AJUDAR
jogar :-
    palavra(Palavra),
    atom_chars(Palavra, Letras),
    inicializa_forca(Letras, Espacos),
    jogar_forca(Letras, Espacos, 7, []).

% Inicializa os espaços da palavra
inicializa_forca([], []).
inicializa_forca([_|Resto], ['_'|EspacosResto]) :-
    inicializa_forca(Resto, EspacosResto).

% Loop do jogo
jogar_forca(_, Espacos, 0, TentativasFeitas) :-
    clear_screen,
    draw_hangman(0),
    writeln('Você perdeu! Tentativas esgotadas.'),
    writeln('A palavra era:'),
    palavra(Palavra),
    writeln(Palavra),
    writeln('Letras tentadas:'),
    escreve_palavra(TentativasFeitas),
    pause_and_continue,
    show_menu. % Retorna ao menu após a pausa
jogar_forca(Letras, Espacos, _, _) :-
    \+ member('_', Espacos),
    clear_screen,
    writeln('Parabéns, você ganhou! A palavra é:'),
    escreve_palavra(Espacos),
    pause_and_continue,
    show_menu. % Retorna ao menu após a pausa
jogar_forca(Letras, Espacos, Tentativas, TentativasFeitas) :-
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
        palavra(PalavraCompleta),
        atom_chars(PalavraCompleta, PalavraLista),
        (   ChuteLista == PalavraLista
        ->  clear_screen,
            writeln('Parabéns, você acertou a palavra completa!'),
            escreve_palavra(Letras),
            pause_and_continue,
            show_menu % Retorna ao menu após a vitória
        ;   writeln('Palavra incorreta! Você perde a vez.'),
            pause_and_continue,
            jogar_forca(Letras, Espacos, Tentativas, TentativasFeitas)
        )
    ;   % Se não quiser chutar a palavra, continua o jogo normalmente
        writeln('Digite uma letra:'),
        read_line_to_string(user_input, Chute),
        string_chars(Chute, [Letra]),
        (   member(Letra, TentativasFeitas)
        ->  writeln('Você já tentou essa letra. Tente novamente.'),
            pause_and_continue,
            jogar_forca(Letras, Espacos, Tentativas, TentativasFeitas)
        ;   (   member(Letra, Letras)
            ->  writeln('Acertou!'),
                atualiza_palavra(Letras, Espacos, Letra, NovaPalavra),
                pause_and_continue,
                jogar_forca(Letras, NovaPalavra, Tentativas, [Letra|TentativasFeitas])
            ;   writeln('Letra incorreta!'),
                NovasTentativas is Tentativas - 1,
                pause_and_continue,
                jogar_forca(Letras, Espacos, NovasTentativas, [Letra|TentativasFeitas])
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
    % Jogador 1
    write('JOGADOR 1: '),
    read_line_to_string(user_input, User1),
    process_player(User1),
    sleep(3),
    % Jogador 2
    write('JOGADOR 2: '),
    read_line_to_string(user_input, User2),
    process_player(User2),
    sleep(3),

    % Continuar com o jogo
    clear_screen,
    show_menu.

% Processa e exibe os dados do jogador ou solicita cadastro
process_player(Name) :-
    ( find_user_by_name(Name, UserData) ->
        display_user_info(UserData)
    ; 
        % Jogador não encontrado, solicita cadastro
        write('Jogador não encontrado. Iremos Realizar seu cadastro\n'),
        request_user_data(Name)
    ).

% Exibe as informações do jogador formatadas
display_user_info(UserData) :-
    UserName = UserData.nome,
    Pontos = UserData.pontos,
    Vitorias = UserData.vitorias,
    Derrotas = UserData.derrotas,
    
    format('Jogador: ~w~n', [UserName]),
    format('Pontos: ~w~n', [Pontos]),
    format('Vitórias: ~w~n', [Vitorias]),
    format('Derrotas: ~w~n \n', [Derrotas]).

% Solicita os dados e cadastra um novo jogador
request_user_data(Name) :-
    add_user(Name).

%TODO: PARTE LÓGICA DO JOGO MULTIPLAYER