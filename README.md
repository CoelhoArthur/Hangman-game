# Hangman-game
Grupo:
- Arthur de Sousa Coelho
- Axel Vaz Lima
- Gabriel Cabral de Medeiros
- Jhonathan Pereira de Andrade
- Orlando Vírginio Penha Júnior

## COMO EXECUTAR
1° Ao fazer fazer o push do repositório, entre na pasta *Hangman-game* e execute os comandos: 

```
$ sudo apt-get update
$ sudo apt-get install swi-prolog
```

3° Após isso, entre na pasta *Hangman-game/src* e execute o comando:   `swipl main.pl`

2° Em seguida, digite:   ` ? - main.`

E assim, dará ínicio ao jogo.

## COMO FUNCIONA O JOGO:
- Será possível jogar nos modos:
> Singleplayer: Onde o jogador terá 7 tentativas de chutar uma letra da palavra ou poderá chutar a palavra toda.

> Multiplayer: Onde 2 jogadores se enfrentarão, terão a mesma palavra, mas cada um terá suas tentativas e letras já tentadas.

> Jogador vs Bot: Onde o jogador disputará a partida contra um bot apenas.

- _SIDEQUESTS_: No modo singleplayer, é possível ter acesso aos sidequests, que são perguntas sobre programação, onde, caso o jogador acerte a palavra ele consegue recuperar uma tentativa, caso erre, continuará no estado anterior.

- O jogo singlePlayer poderá ser pausado a qualquer instante e retomado posteriormente.

- No jogo multiplayer, os jogadores compartilharão a mesma palavra. Ao final da partida, quando um dos jogadores errar a 7° vez, o confronto ficará salvo com o ganhador e o perdedor.

- Será possível ver a classificação dos jogadores e exibir a quantidade de vitórias e derrotas de um jogador específico contra o outro.
