

int a;
int b;
int c;
int d;

int main (void) {
  a = 0;

  b = 1;

  c = 3;

  d = 4;

  // 'and' and 'or' are both left-associative, but 'and' has
  // higher priority. The expression should be parsed as
  // ( (A or B) or ((C and D) and E) ).

  return a == b || c != d || a < b && b < c && c < d;

}
