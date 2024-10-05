:- module(db,[
    read_json/2,
    write_json/2,
    add_user/1,
    find_user_by_name/2,
    find_user/1,
    update_user/3
]).


:- use_module(library(http/json)).

% Lê os dados do arquivo JSON
read_json(FilePath, JSONData) :-
    open(FilePath, read, Stream),
    json_read_dict(Stream, JSONData),
    close(Stream).

% Escreve os dados no arquivo JSON
write_json(FilePath, JSONData) :-
    open(FilePath, write, Stream),
    json_write_dict(Stream, JSONData),
    close(Stream).

% Função para adicionar um usuário
% add_user('Orlando', 30, 'orlando@example.com').
add_user(Nome) :-
    read_json('users.json', Users),
    % Garante que todos os usuários sejam representados como listas de pares chave-valor
    maplist(convert_to_json_object, Users, UsersAsJsonObjects),
    
    % Cria um novo usuário como uma lista de pares chave-valor
    NewUser = json([
        nome-Nome,
        pontos-0,
        vitorias-0,
        derrotas-0
    ]),
    
    append(UsersAsJsonObjects, [NewUser], UpdatedUsers),
    write_json('users.json', UpdatedUsers),
    write("Usuário adicionado com sucesso!\n").

% Converte um dicionário para um objeto JSON mantendo a ordem desejada
convert_to_json_object(Dict, json([
    nome-Nome,
    pontos-Pontos,
    vitorias-Vitorias,
    derrotas-Derrotas
])) :-
    Nome = Dict.nome,
    Pontos = Dict.pontos,
    Vitorias = Dict.vitorias,
    Derrotas = Dict.derrotas.

% Busca um usuário pelo nome
% Retorna User, que vai conter os dados do usuario
find_user_by_name(Name, User) :-
    read_json('users.json', Users),
    % Procura o usuário com o nome fornecido
    member(User, Users),
    User.nome == Name, !.

% Função para atualizar um único atributo do usuário
%update_user('Orlando', pontos, 30).
update_user(Name, Attribute, NewValue) :-
    read_json('users.json', Users),
    % Procura e atualiza o usuário
    (   select(User, Users, Rest),
        string_lower(User.nome, LowerUserName),
        string_lower(Name, LowerName),
        LowerUserName == LowerName
    ->  UpdatedUser = User.put(Attribute, NewValue),
        append(Rest, [UpdatedUser], UpdatedUsers),
        write_json('users.json', UpdatedUsers),
        write("Usuário atualizado com sucesso!\n")
    ;   write("Usuário não encontrado.\n")
    ).