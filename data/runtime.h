#ifndef _RUNTIME_H_
#define _RUNTIME_H_

#include <stdlib.h>
#include <stdio.h>
#define STACK_MAX 10000

typedef union item item;
typedef struct stack stack;
typedef struct thunk thunk;

inline item int_item(const int value);
inline item double_item(const double value);

struct thunk {
void (*code)(item *const env, item *const ret);
item *env;
};

union item
{
int int_item;
double double_item;
thunk thunk_item;
};

struct stack
{
int top;
item items[STACK_MAX];
};

stack global_stack = {0};

item int_item(const int value)
{
const item a = {.int_item = value};
return a;
}

item double_item(const double value)
{
const item a = {.double_item = value};
return a;
}
#endif
