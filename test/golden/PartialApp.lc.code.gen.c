#include <runtime.h>

void sys_thunk_2(item *const env, item *const ret);
void sys_thunk_3(item *const env, item *const ret);
item top_e_f;
void sys_thunk_6(item *const env, item *const ret);
void sys_thunk_7(item *const env, item *const ret);
item top_e_g;
void sys_thunk_11(item *const env, item *const ret);
item top_e_main;

void sys_thunk_2(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmTo (TmPrimBinOp PrimIAdd (TmVar "aa_0_1") (TmVar "aa_1_1")) "c_inp0_1" (TmLam (Param {paramName = "aa_2_1", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "aa_2_1"))))) */
const item var_aa_0_1 = (global_stack.items[--global_stack.top]);
const item var_aa_1_1 = (global_stack.items[--global_stack.top]);
const item sys_arg0_0 = var_aa_0_1;
const item sys_arg1_0 = var_aa_1_1;
(*ret).int_item = sys_arg0_0.int_item + sys_arg1_0.int_item;
const item var_c_inp0_1 = (*ret);
const item var_aa_2_1 = (global_stack.items[--global_stack.top]);
const item sys_arg0_1 = var_c_inp0_1;
const item sys_arg1_1 = var_aa_2_1;
(*ret).int_item = sys_arg0_1.int_item + sys_arg1_1.int_item;
}

void sys_thunk_3(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmTo (TmPrimBinOp PrimIAdd (TmVar "aa_0_1") (TmVar "aa_1_1")) "c_inp0_1" (TmLam (Param {paramName = "aa_2_1", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "aa_2_1"))))))) */
(*ret).thunk_item.code = sys_thunk_2;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_6(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_f")) (TmInt 3)) (TmVar "aa_0_0")) (TmVar "aa_1_0"))) */
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
const item var_aa_1_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_0;
(global_stack.items[global_stack.top++]) = var_aa_0_0;
(global_stack.items[global_stack.top++]).int_item = 3;
const item sys_t_5 = top_e_f;
sys_t_5.thunk_item.code(sys_t_5.thunk_item.env, ret);
}

void sys_thunk_7(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_f")) (TmInt 3)) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))) */
(*ret).thunk_item.code = sys_thunk_6;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_11(item *const _, item *const ret)
{
/* TmTo (TmApp (TmApp (TmForce (TmGlobal "e_g")) (TmInt 2)) (TmInt 5)) "c_v_4" (TmPrintInt (TmVar "c_v_4") (TmReturn (TmInt 0))) */
(global_stack.items[global_stack.top++]).int_item = 5;
(global_stack.items[global_stack.top++]).int_item = 2;
const item sys_t_9 = top_e_g;
sys_t_9.thunk_item.code(sys_t_9.thunk_item.env, ret);
const item var_c_v_4 = (*ret);
const item sys_msg_10 = var_c_v_4;
printf("%d\n", sys_msg_10.int_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmTo (TmPrimBinOp PrimIAdd (TmVar "aa_0_1") (TmVar "aa_1_1")) "c_inp0_1" (TmLam (Param {paramName = "aa_2_1", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "aa_2_1"))))))) */
const item sys_t_4 = {.thunk_item = {.code = sys_thunk_3, .env = NULL}};
sys_t_4.thunk_item.code(sys_t_4.thunk_item.env, ret);
top_e_f = (*ret);
/* TmReturn (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_f")) (TmInt 3)) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))) */
const item sys_t_8 = {.thunk_item = {.code = sys_thunk_7, .env = NULL}};
sys_t_8.thunk_item.code(sys_t_8.thunk_item.env, ret);
top_e_g = (*ret);
/* TmTo (TmApp (TmApp (TmForce (TmGlobal "e_g")) (TmInt 2)) (TmInt 5)) "c_v_4" (TmPrintInt (TmVar "c_v_4") (TmReturn (TmInt 0))) */
const item sys_t_12 = {.thunk_item = {.code = sys_thunk_11, .env = NULL}};
sys_t_12.thunk_item.code(sys_t_12.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

