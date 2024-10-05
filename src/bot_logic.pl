:- module(bot_logic, [jogar_vs_bot/0]).
:- use_module(game_logic, [escolhe_palavra_aleatoria/2, inicializa_forca/2, atualiza_palavra/4, draw_hangman/1, escreve_palavra/1, clear_screen/0, pause_and_continue/0, show_menu/0]).

% Função principal para iniciar o jogo contra o bot
jogar_vs_bot :-
    write('Escolha o nível de dificuldade:\n'),
    write('1 - Fácil\n'),
    write('2 - Médio\n'),
    write('3 - Difícil\n'),
    read_line_to_string(user_input, Option),
    (   Option == "1" -> Dificuldade = facil;
        Option == "2" -> Dificuldade = medio;
        Option == "3" -> Dificuldade = dificil;
        % Caso contrário, entrada inválida
        write('Opção inválida! Tente novamente.\n'),
        pause_and_continue,
        jogar_vs_bot  % Reinicia a função se a opção for inválida
    ),
    % Apenas prossegue se uma opção válida foi selecionada
    escolhe_palavra_aleatoria(Dificuldade, Palavra),  % Escolhe uma palavra aleatória
    atom_chars(Palavra, Letras),
    inicializa_forca(Letras, EspacosJogador),
    inicializa_forca(Letras, EspacosBot),
    jogar_forca_vs_bot(Letras, EspacosJogador, EspacosBot, 7, 7, [], []).

% Loop do jogo - Jogador vs Bot
jogar_forca_vs_bot(_, EspacosJogador, _, 0, _, TentativasJogador, _) :- % Jogador perde
    clear_screen,
    draw_hangman(0),
    writeln('Você perdeu! Tentativas esgotadas.'),
    writeln('Bot venceu!'),
    writeln('A palavra era:'),
    escreve_palavra(EspacosJogador),
    writeln('Letras tentadas pelo Jogador:'),
    escreve_palavra(TentativasJogador),
    pause_and_continue,
    show_menu.

jogar_forca_vs_bot(_, _, EspacosBot, _, 0, _, TentativasBot) :- % Bot perde
    clear_screen,
    draw_hangman(0),
    writeln('Bot perdeu! Tentativas esgotadas.'),
    writeln('Jogador venceu!'),
    writeln('A palavra era:'),
    escreve_palavra(EspacosBot),
    writeln('Letras tentadas pelo Bot:'),
    escreve_palavra(TentativasBot),
    pause_and_continue,
    show_menu.

jogar_forca_vs_bot(_, EspacosJogador, _, _, _, _, _) :- % Jogador vence
    \+ member('_', EspacosJogador),
    clear_screen,
    writeln('Parabéns, você ganhou! A palavra é:'),
    escreve_palavra(EspacosJogador),
    writeln('Jogador venceu!'),
    pause_and_continue,
    show_menu.

jogar_forca_vs_bot(_, _, EspacosBot, _, _, _, _) :- % Bot vence
    \+ member('_', EspacosBot),
    clear_screen,
    writeln('Bot ganhou! A palavra é:'),
    escreve_palavra(EspacosBot),
    writeln('Bot venceu!'),
    pause_and_continue,
    show_menu.

jogar_forca_vs_bot(Letras, EspacosJogador, EspacosBot, TentativasJogador, TentativasBot, TentativasJogadorLista, TentativasBotLista) :-
    clear_screen,
    % Exibe a situação do jogo para o jogador e o bot
    writeln('Jogo contra o Bot:'),
    writeln('-------------------------------------------'),
    % Exibe a situação do Jogador
    writeln('Jogador:'),
    draw_hangman(TentativasJogador),
    writeln('Palavra atual:'),
    escreve_palavra(EspacosJogador),
    writeln('Letras já tentadas pelo Jogador:'),
    escreve_palavra(TentativasJogadorLista),
    writeln('-------------------------------------------'),
    % Exibe a situação do Bot
    writeln('Bot:'),
    draw_hangman(TentativasBot),
    writeln('Palavra atual:'),
    escreve_palavra(EspacosBot),
    writeln('Letras já tentadas pelo Bot:'),
    escreve_palavra(TentativasBotLista),
    writeln('-------------------------------------------'),

    % Turno do Jogador
    writeln('Sua vez! Você deseja tentar "chutar" a palavra completa? (s/n)'),
    read_line_to_string(user_input, Chutar),
    (   Chutar == "s"
    ->  writeln('Digite a palavra completa:'),
        read_line_to_string(user_input, ChutePalavra),
        atom_chars(ChutePalavra, ChuteLista),
        atom_chars(PalavraCompleta, Letras),
        (   ChuteLista == Letras
        ->  clear_screen,
            writeln('Parabéns, você acertou a palavra completa!'),
            escreve_palavra(Letras),
            writeln('Jogador venceu!'),
            pause_and_continue,
            show_menu
        ;   writeln('Palavra incorreta! Você perde a vez.'),
            pause_and_continue,
            % Continua para o turno do bot
            bot_turn(Letras, EspacosJogador, EspacosBot, TentativasJogador, TentativasBot, TentativasJogadorLista, TentativasBotLista)
        )
    ;   writeln('Digite uma letra:'),
        read_line_to_string(user_input, Chute),
        string_chars(Chute, [Letra]),
        (   member(Letra, TentativasJogadorLista)
        ->  writeln('Você já tentou essa letra. Tente novamente.'),
            pause_and_continue,
            jogar_forca_vs_bot(Letras, EspacosJogador, EspacosBot, TentativasJogador, TentativasBot, TentativasJogadorLista, TentativasBotLista)
        ;   (   member(Letra, Letras)
            ->  writeln('Acertou!'),
                atualiza_palavra(Letras, EspacosJogador, Letra, NovaPalavraJogador),
                pause_and_continue,
                % Verifica vitória após atualizar
                (   \+ member('_', NovaPalavraJogador)
                ->  clear_screen,
                    writeln('Parabéns, você ganhou! A palavra é:'),
                    escreve_palavra(NovaPalavraJogador),
                    writeln('Jogador venceu!'),
                    pause_and_continue,
                    show_menu
                ;   bot_turn(Letras, NovaPalavraJogador, EspacosBot, TentativasJogador, TentativasBot, [Letra|TentativasJogadorLista], TentativasBotLista)
                )
            ;   writeln('Letra incorreta!'),
                NovasTentativasJogador is TentativasJogador - 1,
                pause_and_continue,
                bot_turn(Letras, EspacosJogador, EspacosBot, NovasTentativasJogador, TentativasBot, [Letra|TentativasJogadorLista], TentativasBotLista)
            )
        )
    ).

% Turno do Bot
bot_turn(Letras, EspacosJogador, EspacosBot, TentativasJogador, TentativasBot, TentativasJogadorLista, TentativasBotLista) :-
    % Bot escolhe uma letra aleatória
    bot_guess_letter(Letras, TentativasBotLista, LetraBot),
    (   member(LetraBot, Letras)
    ->  writeln('Bot acertou!'),
        atualiza_palavra(Letras, EspacosBot, LetraBot, NovaPalavraBot),
        pause_and_continue,
        % Verifica vitória após atualizar
        (   \+ member('_', NovaPalavraBot)
        ->  clear_screen,
            writeln('Bot ganhou! A palavra é:'),
            escreve_palavra(NovaPalavraBot),
            writeln('Bot venceu!'),
            pause_and_continue,
            show_menu
        ;   jogar_forca_vs_bot(Letras, EspacosJogador, NovaPalavraBot, TentativasJogador, TentativasBot, TentativasJogadorLista, [LetraBot|TentativasBotLista])
        )
    ;   writeln('Bot errou!'),
        NovasTentativasBot is TentativasBot - 1,
        pause_and_continue,
        jogar_forca_vs_bot(Letras, EspacosJogador, EspacosBot, TentativasJogador, NovasTentativasBot, TentativasJogadorLista, [LetraBot|TentativasBotLista])
    ).

% Bot escolhe uma letra aleatória que ainda não foi tentada
bot_guess_letter(Letras, TentativasBotLista, LetraBot) :-
    random_member(LetraBot, ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']),
    \+ member(LetraBot, TentativasBotLista).
