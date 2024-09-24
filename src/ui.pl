% Exibir a palavra com as letras adivinhadas
display_hidden_word(HiddenWord) :-
    write('Palavra: '),
    maplist(write_letter_or_underscore, HiddenWord),
    nl.

write_letter_or_underscore(Letter) :-
    (   var(Letter) 
    ->  write('_ ')
    ;   write(Letter), write(' ')
    ).

% Exibir o boneco da forca
display_hangman(Errors) :-
    boneco(Errors, Boneco),
    write(Boneco), nl.

boneco(0, '  _____\n |     |\n |\n |\n |\n |').
boneco(1, '  _____\n |     |\n |     O\n |\n |\n |').
boneco(2, '  _____\n |     |\n |     O\n |     |\n |\n |').
boneco(3, '  _____\n |     |\n |     O\n |    /|\n |\n |').
boneco(4, '  _____\n |     |\n |     O\n |    /|\\\n |\n |').
boneco(5, '  _____\n |     |\n |     O\n |    /|\\\n |    /\n |').
boneco(6, '  _____\n |     |\n |     O\n |    /|\\\n |    / \\\n |').
