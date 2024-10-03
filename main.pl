% Predicado principal que inicia o menu
menu :- 
    writeln('Selecione a opção desejada:'),
    writeln('1 - Começar o jogo'),
    writeln('2 - Histórico de derrotas/vitórias'),
    writeln('3 - Sair do jogo'),
    read(Opcao),
    tratar_opcao(Opcao).

% Predicado que trata a opção escolhida
tratar_opcao(1) :- 
    writeln('Iniciando o jogo...'),
    % Chame aqui a função que inicia o jogo
    jogo,
    menu.

tratar_opcao(2) :- 
    writeln('Mostrando histórico de derrotas/vitórias...'),
    % Chame aqui a função que mostra o histórico
    historico,
    menu.
    
tratar_opcao(3) :- 
    writeln('Saindo do jogo...'),
    halt.
tratar_opcao(_) :- 
    writeln('Opção inválida. Tente novamente.'),
    menu.

% Exemplo de função que representa o jogo
jogo :- 
    writeln('Aqui é onde o jogo acontece.').

% Exemplo de função que mostra o histórico
historico :- 
    writeln('Aqui é onde o histórico é exibido.').

% Inicia o programa
main :- menu.