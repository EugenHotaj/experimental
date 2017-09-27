#include <stdio.h>

typedef void (*alu_fn) (int*, int*, int*);

typedef struct {
  alu_fn add;
} Alu;

void alu_add(int *rs, int *rt, int *rd) {
  *rd = *rs + *rt;
}

/** Factory to generate a new ALU -- only create new ALUs using this factory. */
Alu createAlu() {
  Alu alu;
  alu.add = alu_add;
  return alu;
}

int main() {
  Alu alu = createAlu();
  int rs = 10;
  int rt = 5;
  int rd = 0;
  printf("old %d\n", rd);
  alu.add(&rs, &rt, &rd);
  printf("new %d\n", rd);
  return 0;
}
