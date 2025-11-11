#include <runtime.h>

void sys_thunk_1(item *const env, item *const ret);
void sys_thunk_4(item *const env, item *const ret);
void sys_thunk_5(item *const env, item *const ret);
item top_e_f;
void sys_thunk_9(item *const env, item *const ret);
item top_e_main;

void sys_thunk_1(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmConst (TmCInt 3))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
const item sys_arg0_0 = var_e_x;
const item sys_arg1_0 = {.int_item = 3};
(*ret).int_item = sys_arg0_0.int_item + sys_arg1_0.int_item;
}

void sys_thunk_4(item *const _, item *const ret)
{
/* TmTo (TmPrintInt (TmConst (TmCInt 5)) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmConst (TmCInt 3))))))) "aa_0_0_temp" (TmForce (TmVar "aa_0_0_temp")) */
const item sys_msg_2 = {.int_item = 5};
printf("%d\n", sys_msg_2.int_item);
(*ret).thunk_item.code = sys_thunk_1;
(*ret).thunk_item.env = NULL;
const item var_aa_0_0_temp = (*ret);
const item sys_t_3 = var_aa_0_0_temp;
sys_t_3.thunk_item.code(sys_t_3.thunk_item.env, ret);
}

void sys_thunk_5(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmTo (TmPrintInt (TmConst (TmCInt 5)) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmConst (TmCInt 3))))))) "aa_0_0_temp" (TmForce (TmVar "aa_0_0_temp")))) */
(*ret).thunk_item.code = sys_thunk_4;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_9(item *const _, item *const ret)
{
/* TmTo (TmApp (TmForce (TmGlobal "e_f")) (TmConst (TmCInt 3))) "c_v_3" (TmPrintInt (TmVar "c_v_3") (TmReturn (TmConst (TmCInt 0)))) */
(global_stack.items[global_stack.top++]).int_item = 3;
const item sys_t_7 = top_e_f;
sys_t_7.thunk_item.code(sys_t_7.thunk_item.env, ret);
const item var_c_v_3 = (*ret);
const item sys_msg_8 = var_c_v_3;
printf("%d\n", sys_msg_8.int_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmTo (TmPrintInt (TmConst (TmCInt 5)) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmConst (TmCInt 3))))))) "aa_0_0_temp" (TmForce (TmVar "aa_0_0_temp")))) */
const item sys_t_6 = {.thunk_item = {.code = sys_thunk_5, .env = NULL}};
sys_t_6.thunk_item.code(sys_t_6.thunk_item.env, ret);
top_e_f = (*ret);
/* TmTo (TmApp (TmForce (TmGlobal "e_f")) (TmConst (TmCInt 3))) "c_v_3" (TmPrintInt (TmVar "c_v_3") (TmReturn (TmConst (TmCInt 0)))) */
const item sys_t_10 = {.thunk_item = {.code = sys_thunk_9, .env = NULL}};
sys_t_10.thunk_item.code(sys_t_10.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

