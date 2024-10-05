:- module(points, [initialize_points/1, add_points/4, subtract_points/4, update_victories/1, update_defeats/1]).
:- use_module(db).  % Certifique-se de que o módulo db está carregado para manipular o JSON

% Inicializa a pontuação do jogador
initialize_points(0).

% Adiciona pontos ao jogador e atualiza o JSON
add_points(CurrentPoints, PointsToAdd, NewPoints, PlayerName) :-
    NewPoints is CurrentPoints + PointsToAdd,
    update_user(PlayerName, pontos, NewPoints).  % Atualiza a pontuação no JSON

% Subtrai 10 pontos do jogador e atualiza o JSON
subtract_points(CurrentPoints, PointsToSubtract, NewPoints, PlayerName) :-
    NewPoints is CurrentPoints - PointsToSubtract,  % Subtrai os pontos, permitindo valores negativos
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
