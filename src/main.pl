:- [db, game_logic, ui, sidequests].

% Função principal para iniciar o jogo
main :-
    welcome_message,
    authenticate_user(User),
    select_difficulty(Difficulty),
    start_game(User, Difficulty).

% Mensagem inicial
welcome_message :-
    write('Bem-vindo ao Hangman Game CCC!\n'),
    write('Vamos começar o jogo!\n').

% Função para autenticar o usuário
authenticate_user(User) :-
    write('Digite seu nome de usuário: '),
    read_line_to_string(user_input, User),
    (   user_exists(User) 
    ->  write('Usuário autenticado com sucesso!\n')
    ;   register_user(User)
    ).

% Função para selecionar dificuldade
select_difficulty(Difficulty) :-
    write('Escolha a dificuldade (facil, medio, dificil): '),
    read_line_to_string(user_input, Difficulty).

% Função para iniciar o jogo
start_game(User, Difficulty) :-
    retrieve_words(Difficulty, Words),
    play_game(User, Words, 0, 0).

