
:- module(sidequests,[
    pergunta_sidequest/0
]).
:- [db].
:- use_module(library(random)).

carregar_perguntas_respostas(Arquivo, PerguntasRespostas) :-
    read_json(Arquivo, PerguntasRespostas).

% Função para buscar uma pergunta aleatória
selecionar_pergunta(PerguntasRespostas, Pergunta, Resposta) :-
    length(PerguntasRespostas, Tamanho),
    random(0, Tamanho, Index),
    nth0(Index, PerguntasRespostas, PerguntaResposta),
    Pergunta = PerguntaResposta.pergunta,
    Resposta = PerguntaResposta.resposta.

% Função que exibe a pergunta e verifica a resposta do jogador
pergunta_sidequest :-
    carregar_perguntas_respostas('sidequests.json', PerguntasRespostas),
    selecionar_pergunta(PerguntasRespostas, Pergunta, RespostaCorreta),
    format('Pergunta: ~w~n', [Pergunta]),
    write('Sua resposta: '),
    read_line_to_string(user_input, RespostaJogador),
    string_upper(RespostaJogador, RespostaJogadorMaiuscula),
    (RespostaJogadorMaiuscula == RespostaCorreta ->
        writeln('Acertou!'),
        sleep(2),
        true
    ;
        writeln('Errou!'),
        sleep(2),
        false
    ).