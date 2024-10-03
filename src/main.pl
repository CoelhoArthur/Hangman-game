:- [game_logic].

% Função principal para iniciar o menu
main :- 
    show_opening, % Mostra a tela de abertura
    show_menu.

% Tela de abertura
show_opening :-
    clear_screen,
    writeln("      ____________..___________                                                 "),
    writeln("     | .___________))__________|                                                "),
    writeln("     | | / /       ||                                                           "),
    writeln("     | |/ /        ||                          _                                "),
    writeln("     | | /         ||.-''.                    | |                               "),
    writeln("     | |/          |/  _  \\                   | | ___   __ _  ___              "),
    writeln("     | |           ||  `/,|               _   | |/ _ \\ / _` |/ _ \\            "),
    writeln("     | |           (\\\\`_.'               | |__| | (_) | (_| | (_) |           "),
    writeln("     | |          .-`--'.                 \\____/ \\___/ \\___ |\\___/          "),
    writeln("     | |         /Y . . Y\\                              __/ |                  "),
    writeln("     | |        // |   | \\\\                            |___/                  "),
    writeln("     | |       //  | . |  \\\\                                                  "),
    writeln("     | |      ')   | _ |   (`         _           ______                        "),
    writeln("     | |           || ||             | |         |  ____|                       "),
    writeln("     | |           || ||           __| | __ _    | |__ ___  _ __ ___ __ _       "),
    writeln("     | |           || ||          / _` |/ _` |   |  __/ _ \\| '__/ __/ _` |     "),
    writeln("     | |           || ||         | (_| | (_| |   | | | (_) | | | (_| (_| |      "),
    writeln("     | |          / | | \\         \\____|\\____|   |_|  \\___/|_|  \\___\\____|"),
    writeln("     | |          `-' `-'                                                       "),
    writeln("     |_|                                                                        "),
    writeln("                                   Aguarde...                                   "),
    sleep(3).

% Limpa a tela
clear_screen :- tty_clear.

% Mostra o menu interativo
show_menu :-
    clear_screen, % Limpa a tela antes de mostrar o menu
    write('\nSelecione a opção desejada:\n'),
    write('1 - Começar jogo\n'),
    write('2 - Histórico de vitórias/derrotas\n'),
    write('3 - Classificação\n'),
    write('4 - Partida contra Bot\n'),
    write('5 - Sair do jogo\n'),
    read_line_to_string(user_input, Option),
    handle_menu_option(Option).

% Lida com as opções do menu
handle_menu_option("1") :- 
    jogar.  % Chama a função para começar o jogo
handle_menu_option("2") :- 
    write('Você escolheu: Histórico de vitórias/derrotas.\n'),
    pause_and_continue,
    show_menu.
handle_menu_option("3") :- 
    write('Você escolheu: Classificação.\n'),
    pause_and_continue,
    show_menu.
handle_menu_option("4") :- 
    write('Você escolheu: Partida contra Bot.\n'),
    pause_and_continue,
    show_menu.
handle_menu_option("5") :- 
    write('Saindo do jogo...\n'),
    halt.
handle_menu_option(_) :- 
    write('Opção inválida! Tente novamente.\n'),
    pause_and_continue,
    show_menu.

% Função de pausa para leitura
pause_and_continue :-
    write('Pressione ENTER para continuar...'),
    get_char(_).
