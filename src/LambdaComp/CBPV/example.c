#define STACK_MAX 10000

#include <stdlib.h>

typedef struct thunk thunk;
typedef union item item;
struct stack;

struct thunk {
  item *env;
  void (*body)(const item *env, item *ret);
};

union item {
  int int_item;
  float float_item;
  thunk thunk_item;
};

typedef struct stack {
  int top;
  item items[STACK_MAX];
} stack;

stack global_stack = {0};

void thunk_body1(const item *env, item *ret)
{
  free((void *)env);
}

item int_item(const int value)
{
  item a = {
    .int_item = value
  };
  return a;
}

item float_item(const float value)
{
  item a = {
    .float_item = value
  };
  return a;
}

int main()
{
  item a = {.thunk_item = {.env = malloc(5 * sizeof(item)), .body = thunk_body1}};
  item ret;
  a.thunk_item.body(a.thunk_item.env, &ret);
}
