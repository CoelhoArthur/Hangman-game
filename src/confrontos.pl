:- module(game_logic, [
    adicionar_confronto/4,
    registrar_confronto/0,
    mostrar_confrontos_jogador/0
]).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

% Função para ler o arquivo JSON dos confrontos
ler_confrontos(Arquivo, Dados) :-
    open(Arquivo, read, Stream),
    json_read(Stream, Dados),
    close(Stream).

% Função para escrever o JSON atualizado no arquivo
salvar_confrontos(Arquivo, Dados) :-
    open(Arquivo, write, Stream),
    json_write(Stream, Dados),
    close(Stream).

% Exemplo de função para adicionar ou atualizar confrontos no JSON
adicionar_confronto(Jogador1, Jogador2, VitoriasJogador1, VitoriasJogador2) :-
    % Abrindo o arquivo confrontos.json para leitura
    ler_confrontos('confrontos.json', Confrontos),
    
    % Atualiza ou adiciona novo confronto na lista
    (   member(confronto(Jogador1, Jogador2, Vit1, Vit2), Confrontos)
    ->  % Se o confronto já existe, atualiza as vitórias
        NewVit1 is Vit1 + VitoriasJogador1,
        NewVit2 is Vit2 + VitoriasJogador2,
        select(confronto(Jogador1, Jogador2, Vit1, Vit2), Confrontos, confronto(Jogador1, Jogador2, NewVit1, NewVit2), UpdatedConfrontos)
    ;   % Se não existe, adiciona um novo
        UpdatedConfrontos = [confronto(Jogador1, Jogador2, VitoriasJogador1, VitoriasJogador2) | Confrontos]
    ),

    % Chama a função salvar_confrontos para gravar as atualizações no arquivo confrontos.json
    salvar_confrontos('confrontos.json', UpdatedConfrontos).


% Função para capturar input do usuário
pegar_input(Prompt, Input) :-
    format("~w", [Prompt]),
    read(Input).

% Função principal para registrar o confronto
registrar_confronto :-
    pegar_input("Nome do Jogador 1: ", Jogador1),
    pegar_input("Nome do Jogador 2: ", Jogador2),
    pegar_input("Vitórias de ~w contra ~w: ", [Jogador1, Jogador2], Vitorias1),
    pegar_input("Vitórias de ~w contra ~w: ", [Jogador2, Jogador1], Vitorias2),
    adicionar_confronto(Jogador1, Jogador2, Vitorias1, Vitorias2).

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
