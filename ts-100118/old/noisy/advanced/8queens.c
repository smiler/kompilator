
/* Test.
** 8-queen.

Expected output:

04752613

This corresponds to the following board:

--x-----
-----x--
---x----
-x------
-------x
----x---
------x-
x-------

*/

int n;
int board[8];

void printInt(int x);

void printboard(int board[]) {
  int i;
  i = 0;
  while(i < n) {
      printInt( board[i]);
      i = i+1;
    }
}

int check(int col, int row) {
  int i;
  int j;

  i = col-1;
  while (i >= 0) {
    j = board[i];
    if (j == row ||                  // Same row
	j > row && col-i == j-row || // Diagonal...
	col-i == row-j)              // ...diagonal
      return 0;
    i = i-1;
  }
  return 1;
}

int queen(int col, int row) {
  if (col >= n) 
    return 1; // Returning false will generate all solutions...
  while (row < n) {
    board[col] = row;
    if (check(col,row) && queen(col+1,0))
      return 1;
    row = row + 1;
  }
  return 0;
}

int main(void) {
  n = 8;
  queen(0,0);
  printboard(board);
}



