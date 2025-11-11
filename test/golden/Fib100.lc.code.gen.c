#include <runtime.h>

void sys_thunk_7(item *const env, item *const ret);
void sys_thunk_8(item *const env, item *const ret);
void sys_thunk_9(item *const env, item *const ret);
item top_e_recFib;
void sys_thunk_12(item *const env, item *const ret);
void sys_thunk_13(item *const env, item *const ret);
item top_e_fib;
void sys_thunk_16(item *const env, item *const ret);
item top_e_main;

void sys_thunk_7(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmTo (TmPrimBinOp PrimILt (TmVar "aa_0_1") (TmVar "aa_1_1")) "c_c_1" (TmLam (Param {paramName = "aa_2_1", paramType = TpInt}) (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmConst (TmCInt 0))) (TmTo (TmPrimBinOp PrimIAdd (TmVar "aa_1_1") (TmConst (TmCInt 1))) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "aa_2_1") (TmVar "e_y")) "c_a3_6" (TmPrintInt (TmVar "aa_1_1") (TmPrintInt (TmVar "aa_2_1") (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_1")) (TmVar "c_a1_6")) (TmVar "e_y")) (TmVar "c_a3_6"))))))))))) */
const item var_aa_0_1 = (global_stack.items[--global_stack.top]);
const item var_aa_1_1 = (global_stack.items[--global_stack.top]);
const item sys_arg0_0 = var_aa_0_1;
const item sys_arg1_0 = var_aa_1_1;
(*ret).int_item = sys_arg0_0.int_item < sys_arg1_0.int_item;
const item var_c_c_1 = (*ret);
const item var_aa_2_1 = (global_stack.items[--global_stack.top]);
const item var_e_y = (global_stack.items[--global_stack.top]);
const item sys_c_6 = var_c_c_1;
if (sys_c_6.int_item)
{
(*ret).int_item = 0;
}
else
{
const item sys_arg0_1 = var_aa_1_1;
const item sys_arg1_1 = {.int_item = 1};
(*ret).int_item = sys_arg0_1.int_item + sys_arg1_1.int_item;
const item var_c_a1_6 = (*ret);
const item sys_arg0_2 = var_aa_2_1;
const item sys_arg1_2 = var_e_y;
(*ret).int_item = sys_arg0_2.int_item + sys_arg1_2.int_item;
const item var_c_a3_6 = (*ret);
const item sys_msg_5 = var_aa_1_1;
printf("%d\n", sys_msg_5.int_item);
const item sys_msg_4 = var_aa_2_1;
printf("%d\n", sys_msg_4.int_item);
(global_stack.items[global_stack.top++]) = var_c_a3_6;
(global_stack.items[global_stack.top++]) = var_e_y;
(global_stack.items[global_stack.top++]) = var_c_a1_6;
(global_stack.items[global_stack.top++]) = var_aa_0_1;
const item sys_t_3 = (env[0]);
sys_t_3.thunk_item.code(sys_t_3.thunk_item.env, ret);
}

}

void sys_thunk_8(item *const _, item *const ret)
{
/* TmRec (Param {paramName = "e_recFib", paramType = TpUp (TpInt :->: (TpInt :->: (TpInt :->: (TpInt :->: TpDown TpInt))))}) (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmTo (TmPrimBinOp PrimILt (TmVar "aa_0_1") (TmVar "aa_1_1")) "c_c_1" (TmLam (Param {paramName = "aa_2_1", paramType = TpInt}) (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmConst (TmCInt 0))) (TmTo (TmPrimBinOp PrimIAdd (TmVar "aa_1_1") (TmConst (TmCInt 1))) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "aa_2_1") (TmVar "e_y")) "c_a3_6" (TmPrintInt (TmVar "aa_1_1") (TmPrintInt (TmVar "aa_2_1") (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_1")) (TmVar "c_a1_6")) (TmVar "e_y")) (TmVar "c_a3_6")))))))))))) */
const item var_e_recFib = {.thunk_item = {.code = sys_thunk_7, .env = (item *) malloc(1 * sizeof(item))}};
(var_e_recFib.thunk_item.env[0]) = var_e_recFib;
var_e_recFib.thunk_item.code(var_e_recFib.thunk_item.env, ret);
}

void sys_thunk_9(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmRec (Param {paramName = "e_recFib", paramType = TpUp (TpInt :->: (TpInt :->: (TpInt :->: (TpInt :->: TpDown TpInt))))}) (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmTo (TmPrimBinOp PrimILt (TmVar "aa_0_1") (TmVar "aa_1_1")) "c_c_1" (TmLam (Param {paramName = "aa_2_1", paramType = TpInt}) (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmConst (TmCInt 0))) (TmTo (TmPrimBinOp PrimIAdd (TmVar "aa_1_1") (TmConst (TmCInt 1))) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "aa_2_1") (TmVar "e_y")) "c_a3_6" (TmPrintInt (TmVar "aa_1_1") (TmPrintInt (TmVar "aa_2_1") (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_1")) (TmVar "c_a1_6")) (TmVar "e_y")) (TmVar "c_a3_6")))))))))))))) */
(*ret).thunk_item.code = sys_thunk_8;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_12(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_recFib")) (TmVar "e_n")) (TmConst (TmCInt 0))) (TmConst (TmCInt 0))) (TmConst (TmCInt 1))) */
const item var_e_n = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]).int_item = 1;
(global_stack.items[global_stack.top++]).int_item = 0;
(global_stack.items[global_stack.top++]).int_item = 0;
(global_stack.items[global_stack.top++]) = var_e_n;
const item sys_t_11 = top_e_recFib;
sys_t_11.thunk_item.code(sys_t_11.thunk_item.env, ret);
}

void sys_thunk_13(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_recFib")) (TmVar "e_n")) (TmConst (TmCInt 0))) (TmConst (TmCInt 0))) (TmConst (TmCInt 1))))) */
(*ret).thunk_item.code = sys_thunk_12;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_16(item *const _, item *const ret)
{
/* TmApp (TmForce (TmGlobal "e_fib")) (TmConst (TmCInt 100)) */
(global_stack.items[global_stack.top++]).int_item = 100;
const item sys_t_15 = top_e_fib;
sys_t_15.thunk_item.code(sys_t_15.thunk_item.env, ret);
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmRec (Param {paramName = "e_recFib", paramType = TpUp (TpInt :->: (TpInt :->: (TpInt :->: (TpInt :->: TpDown TpInt))))}) (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmTo (TmPrimBinOp PrimILt (TmVar "aa_0_1") (TmVar "aa_1_1")) "c_c_1" (TmLam (Param {paramName = "aa_2_1", paramType = TpInt}) (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmConst (TmCInt 0))) (TmTo (TmPrimBinOp PrimIAdd (TmVar "aa_1_1") (TmConst (TmCInt 1))) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "aa_2_1") (TmVar "e_y")) "c_a3_6" (TmPrintInt (TmVar "aa_1_1") (TmPrintInt (TmVar "aa_2_1") (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_1")) (TmVar "c_a1_6")) (TmVar "e_y")) (TmVar "c_a3_6")))))))))))))) */
const item sys_t_10 = {.thunk_item = {.code = sys_thunk_9, .env = NULL}};
sys_t_10.thunk_item.code(sys_t_10.thunk_item.env, ret);
top_e_recFib = (*ret);
/* TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_recFib")) (TmVar "e_n")) (TmConst (TmCInt 0))) (TmConst (TmCInt 0))) (TmConst (TmCInt 1))))) */
const item sys_t_14 = {.thunk_item = {.code = sys_thunk_13, .env = NULL}};
sys_t_14.thunk_item.code(sys_t_14.thunk_item.env, ret);
top_e_fib = (*ret);
/* TmApp (TmForce (TmGlobal "e_fib")) (TmConst (TmCInt 100)) */
const item sys_t_17 = {.thunk_item = {.code = sys_thunk_16, .env = NULL}};
sys_t_17.thunk_item.code(sys_t_17.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

