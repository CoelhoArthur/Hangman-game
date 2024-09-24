% Função para jogar o jogo da forca
play_game(User, [Word1, Word2, Word3], Wins, Losses) :-
    write('Vamos começar o jogo!\n'),
    write('Você tem 3 palavras para adivinhar.\n'),
    play_round(User, Word1, Result1),
    play_round(User, Word2, Result2),
    play_round(User, Word3, Result3),
    update_results(User, [Result1, Result2, Result3]).

% Jogar uma rodada
play_round(User, Word, Result) :-
    initialize_hidden_word(Word, HiddenWord),
    game_loop(User, Word, HiddenWord, 0, 0, Result).

% Função para jogar uma rodada do jogo
game_loop(User, Word, HiddenWord, Errors, Points, Result) :-
    display_hidden_word(HiddenWord),
    display_hangman(Errors),
    (   Errors >= 6
    ->  write('Você perdeu!\n'),
        Result = lost
    ;   HiddenWord == Word
    ->  write('Parabéns! Você ganhou!\n'),
        Result = won
    ;   write('Digite uma letra: '),
        read_line_to_string(user_input, Guess),
        process_guess(User, Word, HiddenWord, Guess, Errors, Points, NewHiddenWord, NewErrors, NewPoints),
        game_loop(User, Word, NewHiddenWord, NewErrors, NewPoints, Result)
    ).

% Processar o palpite do usuário
process_guess(_, Word, HiddenWord, Guess, Errors, Points, NewHiddenWord, NewErrors, NewPoints) :-
    (   sub_atom(Word, _, 1, _, Guess)
    ->  update_hidden_word(Word, HiddenWord, Guess, NewHiddenWord),
        NewErrors = Errors,
        NewPoints is Points + 30
    ;   NewHiddenWord = HiddenWord,
        NewErrors is Errors + 1,
        NewPoints is Points - 10
    ).

% Função para recuperar as palavras
retrieve_words(Difficulty, [Word1, Word2, Word3]) :-
    open('data/palavras.txt', read, Stream),
    read_words(Stream, Difficulty, Words),
    close(Stream),
    random_permutation(Words, [Word1, Word2, Word3]).

read_words(Stream, Difficulty, Words) :-
    read(Stream, Word),
    (   Word == end_of_file
    ->  Words = []
    ;   Word =.. [_, Difficulty, Palavras],
        Words = Palavras
    ).
