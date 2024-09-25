save_game(User, Word, HiddenWord, Errors, Points) :-
    open('data/save_game.txt', write, Stream),
    format(Stream, 'save(~q, ~q, ~q, ~d, ~d).\n', [User, Word, HiddenWord, Errors, Points]),
    close(Stream).

load_game(User, Word, HiddenWord, Errors, Points) :-
    open('data/save_game.txt', read, Stream),
    read(Stream, Save),
    close(Stream),
    Save =.. [save, User, Word, HiddenWord, Errors, Points].