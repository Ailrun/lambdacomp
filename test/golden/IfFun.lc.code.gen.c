#include <runtime.h>

void sys_thunk_3(item *const env, item *const ret);
void sys_thunk_4(item *const env, item *const ret);
item top_e_f;
void sys_thunk_10(item *const env, item *const ret);
item top_e_main;

void sys_thunk_3(item *const _, item *const ret)
{
/* TmLam (BTyped (Param {paramName = "e_c", paramType = TpConst TpCBool}) (TmIf (TmVar "e_c") (TmPrimBinOp PrimIAdd (TmConst (TmCInt 3)) (TmConst (TmCInt 3))) (TmPrimBinOp PrimIMul (TmConst (TmCInt 3)) (TmConst (TmCInt 3))))) */
const item var_e_c = (global_stack.items[--global_stack.top]);
const item sys_c_2 = var_e_c;
if (sys_c_2.int_item)
{
const item sys_arg0_0 = {.int_item = 3};
const item sys_arg1_0 = {.int_item = 3};
(*ret).int_item = sys_arg0_0.int_item + sys_arg1_0.int_item;
}
else
{
const item sys_arg0_1 = {.int_item = 3};
const item sys_arg1_1 = {.int_item = 3};
(*ret).int_item = sys_arg0_1.int_item * sys_arg1_1.int_item;
}

}

void sys_thunk_4(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLam (BTyped (Param {paramName = "e_c", paramType = TpConst TpCBool}) (TmIf (TmVar "e_c") (TmPrimBinOp PrimIAdd (TmConst (TmCInt 3)) (TmConst (TmCInt 3))) (TmPrimBinOp PrimIMul (TmConst (TmCInt 3)) (TmConst (TmCInt 3))))))) */
(*ret).thunk_item.code = sys_thunk_3;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_10(item *const _, item *const ret)
{
/* TmTo (TmApp (TmForce (TmGlobal "e_f")) (TmConst TmCTrue)) (BUntyped "c_v_5" (TmPrintInt (TmVar "c_v_5") (TmTo (TmApp (TmForce (TmGlobal "e_f")) (TmConst TmCFalse)) (BUntyped "c_v_7" (TmPrintInt (TmVar "c_v_7") (TmReturn (TmConst (TmCInt 0)))))))) */
(global_stack.items[global_stack.top++]).int_item = 1;
const item sys_t_6 = top_e_f;
sys_t_6.thunk_item.code(sys_t_6.thunk_item.env, ret);
const item var_c_v_5 = (*ret);
const item sys_msg_9 = var_c_v_5;
printf("%d\n", sys_msg_9.int_item);
(global_stack.items[global_stack.top++]).int_item = 0;
const item sys_t_7 = top_e_f;
sys_t_7.thunk_item.code(sys_t_7.thunk_item.env, ret);
const item var_c_v_7 = (*ret);
const item sys_msg_8 = var_c_v_7;
printf("%d\n", sys_msg_8.int_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmLam (BTyped (Param {paramName = "e_c", paramType = TpConst TpCBool}) (TmIf (TmVar "e_c") (TmPrimBinOp PrimIAdd (TmConst (TmCInt 3)) (TmConst (TmCInt 3))) (TmPrimBinOp PrimIMul (TmConst (TmCInt 3)) (TmConst (TmCInt 3))))))) */
const item sys_t_5 = {.thunk_item = {.code = sys_thunk_4, .env = NULL}};
sys_t_5.thunk_item.code(sys_t_5.thunk_item.env, ret);
top_e_f = (*ret);
/* TmTo (TmApp (TmForce (TmGlobal "e_f")) (TmConst TmCTrue)) (BUntyped "c_v_5" (TmPrintInt (TmVar "c_v_5") (TmTo (TmApp (TmForce (TmGlobal "e_f")) (TmConst TmCFalse)) (BUntyped "c_v_7" (TmPrintInt (TmVar "c_v_7") (TmReturn (TmConst (TmCInt 0)))))))) */
const item sys_t_11 = {.thunk_item = {.code = sys_thunk_10, .env = NULL}};
sys_t_11.thunk_item.code(sys_t_11.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

