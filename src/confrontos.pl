:- module(game_logic, [
    mostrar_confrontos_jogador/0
]).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).


% Função para exibir confrontos de um jogador específico
exibir_confrontos_de_jogador(Nome) :-
    ler_confrontos('confrontos.json', Confrontos),
    buscar_confrontos(Nome, Confrontos).

% Função para buscar e exibir confrontos de um jogador
buscar_confrontos(_, []) :-
    format("Nenhum confronto encontrado para o jogador.~n").
buscar_confrontos(Nome, [Confronto | Resto]) :-
    (   Confronto.jogador1 = Nome
    ->  format("Confronto: ~w vs ~w. Vitórias de ~w: ~w~n", 
               [Confronto.jogador1, Confronto.jogador2, Confronto.jogador1, Confronto.vitorias_jogador1])
    ;   Confronto.jogador2 = Nome
    ->  format("Confronto: ~w vs ~w. Vitórias de ~w: ~w~n", 
               [Confronto.jogador1, Confronto.jogador2, Confronto.jogador2, Confronto.vitorias_jogador2])
    ;   true
    ),
    buscar_confrontos(Nome, Resto).

% Função para pegar o nome do jogador como input e exibir seus confrontos
mostrar_confrontos_jogador :-
    format("Nome do jogador: "),
    read(Nome),
    exibir_confrontos_de_jogador(Nome).
