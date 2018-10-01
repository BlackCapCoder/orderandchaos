# Order And Chaos

This is a bot programming server for the game Order and chaos. Order and chaos is a tic tac toe variant where the goal is to get 5 in a row on a 6x6 board. You can choose to play either X or O for every round.

Once started the server listens for connections on port `4242`, and will pair every two clients that connect in a game of order and chaos. There is a beginner bot written in `C` in the bot folder.


## Usage

#### Start the server
```
stack run
```

#### Run the beginner bot
```
cd bot
gcc main.c -o main
ncat IP PORT -e main
```

