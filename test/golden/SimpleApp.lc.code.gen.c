#include <runtime.h>

void sys_thunk_0(item *const env, item *const ret);
void sys_thunk_2(item *const env, item *const ret);
void sys_thunk_5(item *const env, item *const ret);
item top_e_main;

void sys_thunk_0(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmVar "e_x")) */
const item var_e_x = (global_stack.items[--global_stack.top]);
(*ret) = var_e_x;
}

void sys_thunk_2(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpUp (TpInt :->: TpDown TpInt)}) (TmApp (TmForce (TmVar "e_x")) (TmInt 5)) */
const item var_e_x = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]).int_item = 5;
const item sys_t_1 = var_e_x;
sys_t_1.thunk_item.code(sys_t_1.thunk_item.env, ret);
}

void sys_thunk_5(item *const _, item *const ret)
{
/* TmLet "c_a0_1" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmVar "e_x")))) (TmLet "c_f0_1" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpUp (TpInt :->: TpDown TpInt)}) (TmApp (TmForce (TmVar "e_x")) (TmInt 5)))) (TmTo (TmApp (TmForce (TmVar "c_f0_1")) (TmVar "c_a0_1")) "c_v_2" (TmPrintInt (TmVar "c_v_2") (TmReturn (TmInt 0))))) */
const item var_c_a0_1 = {.thunk_item = {.code = sys_thunk_0, .env = NULL}};
const item var_c_f0_1 = {.thunk_item = {.code = sys_thunk_2, .env = NULL}};
(global_stack.items[global_stack.top++]) = var_c_a0_1;
const item sys_t_3 = var_c_f0_1;
sys_t_3.thunk_item.code(sys_t_3.thunk_item.env, ret);
const item var_c_v_2 = (*ret);
const item sys_msg_4 = var_c_v_2;
printf("%d\n", sys_msg_4.int_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmLet "c_a0_1" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmVar "e_x")))) (TmLet "c_f0_1" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpUp (TpInt :->: TpDown TpInt)}) (TmApp (TmForce (TmVar "e_x")) (TmInt 5)))) (TmTo (TmApp (TmForce (TmVar "c_f0_1")) (TmVar "c_a0_1")) "c_v_2" (TmPrintInt (TmVar "c_v_2") (TmReturn (TmInt 0))))) */
const item sys_t_6 = {.thunk_item = {.code = sys_thunk_5, .env = NULL}};
sys_t_6.thunk_item.code(sys_t_6.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

