:- dynamic user/3.

% Verifica se o usuário já está cadastrado
user_exists(User) :-
    open('data/users.txt', read, Stream),
    read_users(Stream, Users),
    close(Stream),
    memberchk(user(User, _, _), Users).

% Leitura dos usuários a partir do arquivo
read_users(Stream, Users) :-
    read(Stream, Users),
    (   Users == end_of_file
    ->  Users = []
    ;   true
    ).

% Cadastro de novo usuário
register_user(User) :-
    open('data/users.txt', append, Stream),
    format(Stream, 'user(~q, 0, 0).\n', [User]),
    close(Stream),
    write('Usuário registrado com sucesso!\n').

% Persistência dos dados dos usuários
save_user(User, Wins, Losses) :-
    retractall(user(User, _, _)),
    assert(user(User, Wins, Losses)),
    open('data/users.txt', append, Stream),
    format(Stream, 'user(~q, ~d, ~d).\n', [User, Wins, Losses]),
    close(Stream).
