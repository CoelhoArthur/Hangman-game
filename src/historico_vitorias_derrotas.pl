:- use_module(library(http/json)).

% Função para ler o arquivo JSON e retornar o conteúdo
ler_json(Arquivo, Dados) :-
    open(Arquivo, read, Stream),
    json_read(Stream, Dados),
    close(Stream).

% Função para encontrar o jogador pelo nome
buscar_jogador([], _, _) :- fail.
buscar_jogador([Jogador|_], Nome, Jogador) :-
    Jogador.nome = Nome.
buscar_jogador([_|Resto], Nome, Jogador) :-
    buscar_jogador(Resto, Nome, Jogador).

% Função para exibir informações do jogador
exibir_informacoes(Jogador) :-
    format("Nome: ~w~n", [Jogador.nome]),
    format("Pontos: ~w~n", [Jogador.pontos]),
    format("Vitórias: ~w~n", [Jogador.vitorias]),
    format("Derrotas: ~w~n", [Jogador.derrotas]).

% Função principal
mostrar_informacoes_do_jogador(Nome) :-
    ler_json('users.json', Dados),
    buscar_jogador(Dados, Nome, Jogador),
    exibir_informacoes(Jogador).

