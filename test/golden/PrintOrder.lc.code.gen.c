#include <runtime.h>

void sys_thunk_1(item *const env, item *const ret);
void sys_thunk_3(item *const env, item *const ret);
item top_e_f;
void sys_thunk_7(item *const env, item *const ret);
item top_e_main;

void sys_thunk_1(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmInt 3)) */
const item var_e_x = (global_stack.items[--global_stack.top]);
const item sys_arg0_0 = var_e_x;
const item sys_arg1_0 = {.int_item = 3};
(*ret).int_item = sys_arg0_0.int_item + sys_arg1_0.int_item;
}

void sys_thunk_3(item *const _, item *const ret)
{
/* TmPrintInt (TmInt 5) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmInt 3))))) */
const item sys_msg_2 = {.int_item = 5};
printf("%d\n", sys_msg_2.int_item);
(*ret).thunk_item.code = sys_thunk_1;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_7(item *const _, item *const ret)
{
/* TmTo (TmApp (TmForce (TmGlobal "e_f")) (TmInt 3)) "c_v_3" (TmPrintInt (TmVar "c_v_3") (TmReturn (TmInt 0))) */
(global_stack.items[global_stack.top++]).int_item = 3;
const item sys_t_5 = top_e_f;
sys_t_5.thunk_item.code(sys_t_5.thunk_item.env, ret);
const item var_c_v_3 = (*ret);
const item sys_msg_6 = var_c_v_3;
printf("%d\n", sys_msg_6.int_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmPrintInt (TmInt 5) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmInt 3))))) */
const item sys_t_4 = {.thunk_item = {.code = sys_thunk_3, .env = NULL}};
sys_t_4.thunk_item.code(sys_t_4.thunk_item.env, ret);
top_e_f = (*ret);
/* TmTo (TmApp (TmForce (TmGlobal "e_f")) (TmInt 3)) "c_v_3" (TmPrintInt (TmVar "c_v_3") (TmReturn (TmInt 0))) */
const item sys_t_8 = {.thunk_item = {.code = sys_thunk_7, .env = NULL}};
sys_t_8.thunk_item.code(sys_t_8.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

