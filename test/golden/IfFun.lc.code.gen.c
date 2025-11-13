#include <runtime.h>

void sys_thunk_1(item *const env, item *const ret);
void sys_thunk_3(item *const env, item *const ret);
void sys_thunk_6(item *const env, item *const ret);
void sys_thunk_7(item *const env, item *const ret);
item top_e_f;
void sys_thunk_13(item *const env, item *const ret);
item top_e_main;

void sys_thunk_1(item *const _, item *const ret)
{
/* TmLam (BTyped (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmConst (TmCInt 3)))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
const item sys_arg0_0 = var_e_x;
const item sys_arg1_0 = {.int_item = 3};
(*ret).int_item = sys_arg0_0.int_item + sys_arg1_0.int_item;
}

void sys_thunk_3(item *const _, item *const ret)
{
/* TmLam (BTyped (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "e_x"))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
const item sys_arg0_2 = var_e_x;
const item sys_arg1_2 = var_e_x;
(*ret).int_item = sys_arg0_2.int_item * sys_arg1_2.int_item;
}

void sys_thunk_6(item *const _, item *const ret)
{
/* TmLam (BTyped (Param {paramName = "e_c", paramType = TpConst TpCBool}) (TmTo (TmIf (TmVar "e_c") (TmReturn (TmThunk (TmLam (BTyped (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmConst (TmCInt 3))))))) (TmReturn (TmThunk (TmLam (BTyped (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "e_x"))))))) (BUntyped "c_f_3" (TmApp (TmForce (TmVar "c_f_3")) (TmConst (TmCInt 3)))))) */
const item var_e_c = (global_stack.items[--global_stack.top]);
const item sys_c_4 = var_e_c;
if (sys_c_4.int_item)
{
(*ret).thunk_item.code = sys_thunk_1;
(*ret).thunk_item.env = NULL;
}
else
{
(*ret).thunk_item.code = sys_thunk_3;
(*ret).thunk_item.env = NULL;
}

const item var_c_f_3 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 3;
const item sys_t_5 = var_c_f_3;
sys_t_5.thunk_item.code(sys_t_5.thunk_item.env, ret);
}

void sys_thunk_7(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLam (BTyped (Param {paramName = "e_c", paramType = TpConst TpCBool}) (TmTo (TmIf (TmVar "e_c") (TmReturn (TmThunk (TmLam (BTyped (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmConst (TmCInt 3))))))) (TmReturn (TmThunk (TmLam (BTyped (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "e_x"))))))) (BUntyped "c_f_3" (TmApp (TmForce (TmVar "c_f_3")) (TmConst (TmCInt 3)))))))) */
(*ret).thunk_item.code = sys_thunk_6;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_13(item *const _, item *const ret)
{
/* TmTo (TmApp (TmForce (TmGlobal "e_f")) (TmConst TmCTrue)) (BUntyped "c_v_5" (TmTo (TmApp (TmForce (TmGlobal "e_f")) (TmConst TmCFalse)) (BUntyped "c_v_7" (TmPrintInt (TmVar "c_v_5") (TmPrintInt (TmVar "c_v_7") (TmReturn (TmConst (TmCInt 0)))))))) */
(global_stack.items[global_stack.top++]).int_item = 1;
const item sys_t_9 = top_e_f;
sys_t_9.thunk_item.code(sys_t_9.thunk_item.env, ret);
const item var_c_v_5 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 0;
const item sys_t_10 = top_e_f;
sys_t_10.thunk_item.code(sys_t_10.thunk_item.env, ret);
const item var_c_v_7 = (*ret);
const item sys_msg_12 = var_c_v_5;
printf("%d\n", sys_msg_12.int_item);
const item sys_msg_11 = var_c_v_7;
printf("%d\n", sys_msg_11.int_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmLam (BTyped (Param {paramName = "e_c", paramType = TpConst TpCBool}) (TmTo (TmIf (TmVar "e_c") (TmReturn (TmThunk (TmLam (BTyped (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmConst (TmCInt 3))))))) (TmReturn (TmThunk (TmLam (BTyped (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "e_x"))))))) (BUntyped "c_f_3" (TmApp (TmForce (TmVar "c_f_3")) (TmConst (TmCInt 3)))))))) */
const item sys_t_8 = {.thunk_item = {.code = sys_thunk_7, .env = NULL}};
sys_t_8.thunk_item.code(sys_t_8.thunk_item.env, ret);
top_e_f = (*ret);
/* TmTo (TmApp (TmForce (TmGlobal "e_f")) (TmConst TmCTrue)) (BUntyped "c_v_5" (TmTo (TmApp (TmForce (TmGlobal "e_f")) (TmConst TmCFalse)) (BUntyped "c_v_7" (TmPrintInt (TmVar "c_v_5") (TmPrintInt (TmVar "c_v_7") (TmReturn (TmConst (TmCInt 0)))))))) */
const item sys_t_14 = {.thunk_item = {.code = sys_thunk_13, .env = NULL}};
sys_t_14.thunk_item.code(sys_t_14.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

