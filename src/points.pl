:- module(points, [initialize_points/1, add_points/4, subtract_points/4, update_victories/1, update_defeats/1, display_classification/0]).
:- use_module(db).  % Importa o módulo para manipular o JSON

% Inicializa a pontuação do jogador
initialize_points(0).

% Adiciona pontos ao jogador e atualiza o JSON
add_points(CurrentPoints, PointsToAdd, NewPoints, PlayerName) :-
    NewPoints is CurrentPoints + PointsToAdd,
    update_user(PlayerName, pontos, NewPoints).  % Atualiza a pontuação no JSON

% Subtrai pontos do jogador e atualiza o JSON
subtract_points(CurrentPoints, PointsToSubtract, NewPoints, PlayerName) :-
    NewPoints is CurrentPoints - PointsToSubtract,
    update_user(PlayerName, pontos, NewPoints).  % Atualiza a pontuação no JSON

% Atualiza vitórias do jogador no JSON
update_victories(PlayerName) :-
    find_user_by_name(PlayerName, UserData),
    CurrentVictories = UserData.vitorias,
    NewVictories is CurrentVictories + 1,
    update_user(PlayerName, vitorias, NewVictories).

% Atualiza derrotas do jogador no JSON
update_defeats(PlayerName) :-
    find_user_by_name(PlayerName, UserData),
    CurrentDefeats = UserData.derrotas,
    NewDefeats is CurrentDefeats + 1,
    update_user(PlayerName, derrotas, NewDefeats).

% Exibe a classificação dos jogadores
display_classification :-
    read_json('users.json', Users),  % Lê os dados do JSON
    % Ordena os jogadores pela pontuação em ordem decrescente
    sort_users_by_points(Users, SortedUsers),
    writeln('Classificação dos Jogadores:'),
    writeln('------------------------------------------------------------'),
    writeln('| Posição |  Nome  | Pontos | Vitórias | Derrotas |'),
    writeln('------------------------------------------------------------'),
    write_classification(SortedUsers, 1),
    writeln('------------------------------------------------------------').

% Ordena os jogadores pela pontuação em ordem decrescente
sort_users_by_points(Users, SortedUsers) :-
    predsort(compare_points, Users, SortedUsers).

% Compara os pontos dos jogadores para ordenação (decrescente)
compare_points('<', User1, User2) :-
    P1 = User1.pontos,
    P2 = User2.pontos,
    P1 > P2.
compare_points('>', User1, User2) :-
    P1 = User1.pontos,
    P2 = User2.pontos,
    P1 =< P2.

% Exibe a lista de jogadores com a posição, pontos, vitórias e derrotas
write_classification([], _).
write_classification([User | Rest], Position) :-
    format('| ~w~t~8+ | ~w~t~12+ | ~w~t~7+ | ~w~t~9+ | ~w~t~8+ |~n', 
           [Position, User.nome, User.pontos, User.vitorias, User.derrotas]),
    NextPosition is Position + 1,
    write_classification(Rest, NextPosition).
