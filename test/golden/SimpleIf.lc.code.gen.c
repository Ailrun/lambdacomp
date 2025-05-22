#include <runtime.h>

void sys_thunk_0(item *const env, item *const ret);
void sys_thunk_5(item *const env, item *const ret);
item top_e_main;

void sys_thunk_0(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpBool}) (TmReturn (TmVar "e_x")) */
const item var_e_x = (global_stack.items[--global_stack.top]);
(*ret) = var_e_x;
}

void sys_thunk_5(item *const _, item *const ret)
{
/* TmLet "c_f0_0" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpBool}) (TmReturn (TmVar "e_x")))) (TmTo (TmApp (TmForce (TmVar "c_f0_0")) TmTrue) "c_c_1" (TmIf (TmVar "c_c_1") (TmPrintInt (TmInt 1) (TmReturn (TmInt 1))) (TmPrintInt (TmInt 0) (TmReturn (TmInt 0))))) */
const item var_c_f0_0 = {.thunk_item = {.code = sys_thunk_0, .env = NULL}};
(global_stack.items[global_stack.top++]).int_item = 1;
const item sys_t_1 = var_c_f0_0;
sys_t_1.thunk_item.code(sys_t_1.thunk_item.env, ret);
const item var_c_c_1 = (*ret);
const item sys_c_4 = var_c_c_1;
if (sys_c_4.int_item)
{
const item sys_msg_2 = {.int_item = 1};
printf("%d\n", sys_msg_2.int_item);
(*ret).int_item = 1;
}
else
{
const item sys_msg_3 = {.int_item = 0};
printf("%d\n", sys_msg_3.int_item);
(*ret).int_item = 0;
}

}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmLet "c_f0_0" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpBool}) (TmReturn (TmVar "e_x")))) (TmTo (TmApp (TmForce (TmVar "c_f0_0")) TmTrue) "c_c_1" (TmIf (TmVar "c_c_1") (TmPrintInt (TmInt 1) (TmReturn (TmInt 1))) (TmPrintInt (TmInt 0) (TmReturn (TmInt 0))))) */
const item sys_t_6 = {.thunk_item = {.code = sys_thunk_5, .env = NULL}};
sys_t_6.thunk_item.code(sys_t_6.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

