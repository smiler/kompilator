/* Test file for semantic errors. Contains exactly one error. */

char first(int a[]) {
  char b[10];

  b = a;		// b and a are of different types
  return 1;
}

