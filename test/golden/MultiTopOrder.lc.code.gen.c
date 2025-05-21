#include <runtime.h>

void sys_thunk_1(item *const env, item *const ret);
void sys_thunk_3(item *const env, item *const ret);
item top_u_g;
void sys_thunk_6(item *const env, item *const ret);
void sys_thunk_8(item *const env, item *const ret);
item top_u_f;
void sys_thunk_11(item *const env, item *const ret);
item top_u_main;

void sys_thunk_1(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmInt 3)) */
const item var_u_x = (global_stack.items[--global_stack.top]);
const item sys_arg0_0 = var_u_x;
const item sys_arg1_0 = {.int_item = 3};
(*ret).int_item = sys_arg0_0.int_item + sys_arg1_0.int_item;
}

void sys_thunk_3(item *const _, item *const ret)
{
/* TmPrintInt (TmInt 5) (TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmInt 3))))) */
const item sys_msg_2 = {.int_item = 5};
printf("%d\n", sys_msg_2.int_item);
(*ret).thunk_item.code = sys_thunk_1;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_6(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmInt 1)) */
const item var_u_x = (global_stack.items[--global_stack.top]);
const item sys_arg0_5 = var_u_x;
const item sys_arg1_5 = {.int_item = 1};
(*ret).int_item = sys_arg0_5.int_item + sys_arg1_5.int_item;
}

void sys_thunk_8(item *const _, item *const ret)
{
/* TmPrintInt (TmInt 4) (TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmInt 1))))) */
const item sys_msg_7 = {.int_item = 4};
printf("%d\n", sys_msg_7.int_item);
(*ret).thunk_item.code = sys_thunk_6;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_11(item *const _, item *const ret)
{
/* TmPrintInt (TmInt 3) (TmReturn (TmInt 0)) */
const item sys_msg_10 = {.int_item = 3};
printf("%d\n", sys_msg_10.int_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmPrintInt (TmInt 5) (TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmInt 3))))) */
const item sys_t_4 = {.thunk_item = {.code = sys_thunk_3, .env = NULL}};
sys_t_4.thunk_item.code(sys_t_4.thunk_item.env, ret);
top_u_g = (*ret);
/* TmPrintInt (TmInt 4) (TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmInt 1))))) */
const item sys_t_9 = {.thunk_item = {.code = sys_thunk_8, .env = NULL}};
sys_t_9.thunk_item.code(sys_t_9.thunk_item.env, ret);
top_u_f = (*ret);
/* TmPrintInt (TmInt 3) (TmReturn (TmInt 0)) */
const item sys_t_12 = {.thunk_item = {.code = sys_thunk_11, .env = NULL}};
sys_t_12.thunk_item.code(sys_t_12.thunk_item.env, ret);
top_u_main = (*ret);
}
return top_u_main.int_item;
}

