#include <runtime.h>

void sys_thunk_6(item *const env, item *const ret);
void sys_thunk_7(item *const env, item *const ret);
void sys_thunk_8(item *const env, item *const ret);
item top_e_recF;
void sys_thunk_11(item *const env, item *const ret);
item top_e_main;

void sys_thunk_6(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_0", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimIGt (TmVar "aa_0_0") (TmConst (TmCInt 0))) "c_c_3" (TmLam (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmPrintInt (TmVar "e_x") (TmPrintInt (TmConst (TmCInt 2)) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "aa_0_0") (TmConst (TmCInt 1))) "c_a0_5" (TmApp (TmApp (TmForce (TmVar "e_recF")) (TmVar "c_a0_5")) (TmVar "e_x"))) (TmReturn (TmConst (TmCInt 0)))))))) */
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
const item sys_arg0_0 = var_aa_0_0;
const item sys_arg1_0 = {.int_item = 0};
(*ret).int_item = sys_arg0_0.int_item > sys_arg1_0.int_item;
const item var_c_c_3 = (*ret);
const item var_e_x = (global_stack.items[--global_stack.top]);
const item sys_msg_5 = var_e_x;
printf("%d\n", sys_msg_5.int_item);
const item sys_msg_4 = {.int_item = 2};
printf("%d\n", sys_msg_4.int_item);
const item sys_c_3 = var_c_c_3;
if (sys_c_3.int_item)
{
const item sys_arg0_1 = var_aa_0_0;
const item sys_arg1_1 = {.int_item = 1};
(*ret).int_item = sys_arg0_1.int_item - sys_arg1_1.int_item;
const item var_c_a0_5 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_x;
(global_stack.items[global_stack.top++]) = var_c_a0_5;
const item sys_t_2 = (env[0]);
sys_t_2.thunk_item.code(sys_t_2.thunk_item.env, ret);
}
else
{
(*ret).int_item = 0;
}

}

void sys_thunk_7(item *const _, item *const ret)
{
/* TmRec (Param {paramName = "e_recF", paramType = TpUp (TpConst TpCInt :->: (TpConst TpCInt :->: TpDown (TpConst TpCInt)))}) (TmLam (Param {paramName = "aa_0_0", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimIGt (TmVar "aa_0_0") (TmConst (TmCInt 0))) "c_c_3" (TmLam (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmPrintInt (TmVar "e_x") (TmPrintInt (TmConst (TmCInt 2)) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "aa_0_0") (TmConst (TmCInt 1))) "c_a0_5" (TmApp (TmApp (TmForce (TmVar "e_recF")) (TmVar "c_a0_5")) (TmVar "e_x"))) (TmReturn (TmConst (TmCInt 0))))))))) */
const item var_e_recF = {.thunk_item = {.code = sys_thunk_6, .env = (item *) malloc(1 * sizeof(item))}};
(var_e_recF.thunk_item.env[0]) = var_e_recF;
var_e_recF.thunk_item.code(var_e_recF.thunk_item.env, ret);
}

void sys_thunk_8(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmRec (Param {paramName = "e_recF", paramType = TpUp (TpConst TpCInt :->: (TpConst TpCInt :->: TpDown (TpConst TpCInt)))}) (TmLam (Param {paramName = "aa_0_0", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimIGt (TmVar "aa_0_0") (TmConst (TmCInt 0))) "c_c_3" (TmLam (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmPrintInt (TmVar "e_x") (TmPrintInt (TmConst (TmCInt 2)) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "aa_0_0") (TmConst (TmCInt 1))) "c_a0_5" (TmApp (TmApp (TmForce (TmVar "e_recF")) (TmVar "c_a0_5")) (TmVar "e_x"))) (TmReturn (TmConst (TmCInt 0))))))))))) */
(*ret).thunk_item.code = sys_thunk_7;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_11(item *const _, item *const ret)
{
/* TmApp (TmApp (TmForce (TmGlobal "e_recF")) (TmConst (TmCInt 1000000))) (TmConst (TmCInt 3)) */
(global_stack.items[global_stack.top++]).int_item = 3;
(global_stack.items[global_stack.top++]).int_item = 1000000;
const item sys_t_10 = top_e_recF;
sys_t_10.thunk_item.code(sys_t_10.thunk_item.env, ret);
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmRec (Param {paramName = "e_recF", paramType = TpUp (TpConst TpCInt :->: (TpConst TpCInt :->: TpDown (TpConst TpCInt)))}) (TmLam (Param {paramName = "aa_0_0", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimIGt (TmVar "aa_0_0") (TmConst (TmCInt 0))) "c_c_3" (TmLam (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmPrintInt (TmVar "e_x") (TmPrintInt (TmConst (TmCInt 2)) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "aa_0_0") (TmConst (TmCInt 1))) "c_a0_5" (TmApp (TmApp (TmForce (TmVar "e_recF")) (TmVar "c_a0_5")) (TmVar "e_x"))) (TmReturn (TmConst (TmCInt 0))))))))))) */
const item sys_t_9 = {.thunk_item = {.code = sys_thunk_8, .env = NULL}};
sys_t_9.thunk_item.code(sys_t_9.thunk_item.env, ret);
top_e_recF = (*ret);
/* TmApp (TmApp (TmForce (TmGlobal "e_recF")) (TmConst (TmCInt 1000000))) (TmConst (TmCInt 3)) */
const item sys_t_12 = {.thunk_item = {.code = sys_thunk_11, .env = NULL}};
sys_t_12.thunk_item.code(sys_t_12.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

