% Define o módulo e os métodos exportados
:- module(vitorias, [
    somar_vitorias/4,
    obter_e_somar_vitorias/0
]).

% Inclui a biblioteca JSON necessária para manipulação de arquivos JSON
:- use_module(library(http/json)).
:- use_module(db).

% Função para somar as vitórias de dois jogadores 
somar_vitorias(JogadorA, JogadorB, VitoriasA, VitoriasB) :-
    read_json('confrontos.json', Confrontos),
    somar_vitorias_aux(Confrontos, JogadorA, JogadorB, 0, 0, VitoriasA, VitoriasB).

% Função auxiliar recursiva para somar as vitórias
somar_vitorias_aux([], _, _, VitoriasA, VitoriasB, VitoriasA, VitoriasB). % Base da recursão

somar_vitorias_aux([Confronto | Resto], JogadorA, JogadorB, VitoriasAAcc, VitoriasBAcc, VitoriasA, VitoriasB) :-
    (   Confronto.Jogador1 = JogadorA,
        Confronto.Jogador2 = JogadorB
    ->  NovoVA = VitoriasAAcc + Confronto.VitoriasJogador1,
        NovoVB = VitoriasBAcc + Confronto.VitoriasJogador2
    ;   Confronto.Jogador1 = JogadorB,
        Confronto.Jogador2 = JogadorA
    ->  NovoVA = VitoriasAAcc + Confronto.VitoriasJogador2,
        NovoVB = VitoriasBAcc + Confronto.VitoriasJogador1
    ;   NovoVA = VitoriasAAcc,
        NovoVB = VitoriasBAcc
    ),
    somar_vitorias_aux(Resto, JogadorA, JogadorB, NovoVA, NovoVB, VitoriasA, VitoriasB).


% Função para obter os nomes dos jogadores e somar as vitórias
obter_e_somar_vitorias :-
    write("Digite o nome do Jogador A: "),
    read_line_to_string(user_input, JogadorAString),
    write("Digite o nome do Jogador B: "),
    read_line_to_string(user_input, JogadorBString),

    string_to_atom(JogadorAString, JogadorA),
    string_to_atom(JogadorBString, JogadorB),

    somar_vitorias(JogadorA, JogadorB, VitoriasA, VitoriasB),

    format("Vitórias de ~w contra ~w: ~d~n", [JogadorA, JogadorB, VitoriasA]),
    format("Vitórias de ~w contra ~w: ~d~n", [JogadorB, JogadorA, VitoriasB]).
