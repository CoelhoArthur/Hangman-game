% Side quest para o usuário recuperar parte dos erros
side_quest(User, Errors, NewErrors) :-
    write('Side Quest: Responda a pergunta corretamente para remover uma parte da forca.\n'),
    ask_question(Question, Options, CorrectAnswer),
    write(Question), nl,
    write('Escolha: '), write(Options), nl,
    read_line_to_string(user_input, Answer),
    (   Answer == CorrectAnswer
    ->  write('Resposta correta! Você recuperou uma parte do boneco.\n'),
        NewErrors is max(0, Errors - 1)
    ;   write('Resposta errada! Nenhuma mudança no boneco.\n'),
        NewErrors = Errors
    ).

ask_question('Qual é a capital da França?', ['1) Paris', '2) Londres', '3) Berlim'], '1').
