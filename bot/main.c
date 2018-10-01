#include <stdio.h>

typedef int bool;
typedef enum   { Empty, X, O                } piece;
typedef struct { piece p; char x, y;        } move;
typedef struct {
  piece ps[6][6];
  short numMoves;
  bool  isPlayer1;
} board;



move readMove () {
  piece p = getchar() == 'X'? X : O;
  char  x = getchar() - 48 - 1;
  char  y = getchar() - 48 - 1;

  return (move) { .p=p, .x=x, .y=y };
}
void writeMove (const move * m) {
  putchar((*m).p == X? 'X': 'O');
  putchar((*m).x + 48 + 1);
  putchar((*m).y + 48 + 1);
  fflush(stdout);
}

void mkMove (board * b, const move * m) {
  (*b).ps[(*m).x][(*m).y] = (*m).p;
  (*b).numMoves++;
}

bool getResult (int * res) {
  return 1;
  int x = getchar();
  if (x == 'W' || x == 'L' || x == 'T') {
    *res = x;
    return 0;
  }

  ungetc(x, stdin);
  return 1;
}


move ai (board b) {
  return (move) { .p=X, .x=b.numMoves, .y=b.isPlayer1? 0 : 1 };
}

int main(int argc, const char *argv[]) {
  bool  isP1 = getchar() == '1';
  board b    = {0, .isPlayer1 = isP1};
  int   res  = -1;

  if (isP1) {
    fprintf(stderr, "I am player 1\n");
  } else {
    fprintf(stderr, "I am player 2\n");
    move m = readMove();
    mkMove(&b, &m);
  }

  while (getResult(&res)) {
    move m = ai(b);
    fprintf(stderr, "Putting %c on %d, %d\n", m.p=='X'? 'X': 'O', m.x, m.y);
    writeMove(&m);
    mkMove(&b, &m);
    m = readMove();
    mkMove(&b, &m);
  }

  fprintf(stderr, "Done, result was: %c\n", res);
  fflush(stderr);

  return 0;
}
