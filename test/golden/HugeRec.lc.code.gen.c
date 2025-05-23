#include <runtime.h>

void sys_thunk_1(item *const env, item *const ret);
void sys_thunk_2(item *const env, item *const ret);
void sys_thunk_10(item *const env, item *const ret);
void sys_thunk_11(item *const env, item *const ret);
void sys_thunk_13(item *const env, item *const ret);
void sys_thunk_16(item *const env, item *const ret);
void sys_thunk_17(item *const env, item *const ret);
void sys_thunk_18(item *const env, item *const ret);
void sys_thunk_21(item *const env, item *const ret);
void sys_thunk_22(item *const env, item *const ret);
item top_e_recF;
void sys_thunk_25(item *const env, item *const ret);
void sys_thunk_26(item *const env, item *const ret);
void sys_thunk_29(item *const env, item *const ret);
item top_e_main;

void sys_thunk_1(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_recF")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) */
const item var_aa_1_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_0;
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_0 = (env[1]);
sys_t_0.thunk_item.code(sys_t_0.thunk_item.env, ret);
}

void sys_thunk_2(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_recF")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))) */
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_1;
(*ret).thunk_item.env = (item *) malloc(2 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_0;
((*ret).thunk_item.env[1]) = (env[0]);
}

void sys_thunk_10(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmLet "c_f0_5" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_recF")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimIGt (TmVar "e_n") (TmInt 0)) "c_c_3" (TmPrintInt (TmVar "e_x") (TmPrintInt (TmInt 2) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a0_5" (TmTo (TmApp (TmForce (TmVar "c_f0_5")) (TmVar "c_a0_5")) "c_f1_5" (TmApp (TmForce (TmVar "c_f1_5")) (TmVar "e_x")))) (TmReturn (TmInt 0))))))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
const item var_c_f0_5 = {.thunk_item = {.code = sys_thunk_2, .env = (item *) malloc(1 * sizeof(item))}};
(var_c_f0_5.thunk_item.env[0]) = (env[1]);
const item sys_arg0_3 = (env[0]);
const item sys_arg1_3 = {.int_item = 0};
(*ret).int_item = sys_arg0_3.int_item > sys_arg1_3.int_item;
const item var_c_c_3 = (*ret);
const item sys_msg_9 = var_e_x;
printf("%d\n", sys_msg_9.int_item);
const item sys_msg_8 = {.int_item = 2};
printf("%d\n", sys_msg_8.int_item);
const item sys_c_7 = var_c_c_3;
if (sys_c_7.int_item)
{
const item sys_arg0_4 = (env[0]);
const item sys_arg1_4 = {.int_item = 1};
(*ret).int_item = sys_arg0_4.int_item - sys_arg1_4.int_item;
const item var_c_a0_5 = (*ret);
(global_stack.items[global_stack.top++]) = var_c_a0_5;
const item sys_t_5 = var_c_f0_5;
sys_t_5.thunk_item.code(sys_t_5.thunk_item.env, ret);
const item var_c_f1_5 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_6 = var_c_f1_5;
sys_t_6.thunk_item.code(sys_t_6.thunk_item.env, ret);
}
else
{
(*ret).int_item = 0;
}

}

void sys_thunk_11(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmLet "c_f0_5" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_recF")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimIGt (TmVar "e_n") (TmInt 0)) "c_c_3" (TmPrintInt (TmVar "e_x") (TmPrintInt (TmInt 2) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a0_5" (TmTo (TmApp (TmForce (TmVar "c_f0_5")) (TmVar "c_a0_5")) "c_f1_5" (TmApp (TmForce (TmVar "c_f1_5")) (TmVar "e_x")))) (TmReturn (TmInt 0)))))))))) */
const item var_e_n = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_10;
(*ret).thunk_item.env = (item *) malloc(2 * sizeof(item));
((*ret).thunk_item.env[0]) = var_e_n;
((*ret).thunk_item.env[1]) = (env[0]);
}

void sys_thunk_13(item *const env, item *const ret)
{
/* TmLet "c_r_6" (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmLet "c_f0_5" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_recF")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimIGt (TmVar "e_n") (TmInt 0)) "c_c_3" (TmPrintInt (TmVar "e_x") (TmPrintInt (TmInt 2) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a0_5" (TmTo (TmApp (TmForce (TmVar "c_f0_5")) (TmVar "c_a0_5")) "c_f1_5" (TmApp (TmForce (TmVar "c_f1_5")) (TmVar "e_x")))) (TmReturn (TmInt 0)))))))))))) (TmForce (TmVar "c_r_6")) */
const item var_c_r_6 = {.thunk_item = {.code = sys_thunk_11, .env = (item *) malloc(1 * sizeof(item))}};
(var_c_r_6.thunk_item.env[0]) = (env[0]);
const item sys_t_12 = var_c_r_6;
sys_t_12.thunk_item.code(sys_t_12.thunk_item.env, ret);
}

void sys_thunk_16(item *const env, item *const ret)
{
/* TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_6" (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmLet "c_f0_5" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_recF")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimIGt (TmVar "e_n") (TmInt 0)) "c_c_3" (TmPrintInt (TmVar "e_x") (TmPrintInt (TmInt 2) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a0_5" (TmTo (TmApp (TmForce (TmVar "c_f0_5")) (TmVar "c_a0_5")) "c_f1_5" (TmApp (TmForce (TmVar "c_f1_5")) (TmVar "e_x")))) (TmReturn (TmInt 0)))))))))))) (TmForce (TmVar "c_r_6")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0"))))) */
const item var_aa_0_0_temp = {.thunk_item = {.code = sys_thunk_13, .env = (item *) malloc(1 * sizeof(item))}};
(var_aa_0_0_temp.thunk_item.env[0]) = (env[0]);
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_0;
const item sys_t_14 = var_aa_0_0_temp;
sys_t_14.thunk_item.code(sys_t_14.thunk_item.env, ret);
const item var_aa_1_0_temp = (*ret);
const item var_aa_1_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_0;
const item sys_t_15 = var_aa_1_0_temp;
sys_t_15.thunk_item.code(sys_t_15.thunk_item.env, ret);
}

void sys_thunk_17(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmApp (TmRec (Param {paramName = "e_recF", paramType = TpUp (TpInt :->: (TpInt :->: TpDown TpInt))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_6" (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmLet "c_f0_5" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_recF")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimIGt (TmVar "e_n") (TmInt 0)) "c_c_3" (TmPrintInt (TmVar "e_x") (TmPrintInt (TmInt 2) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a0_5" (TmTo (TmApp (TmForce (TmVar "c_f0_5")) (TmVar "c_a0_5")) "c_f1_5" (TmApp (TmForce (TmVar "c_f1_5")) (TmVar "e_x")))) (TmReturn (TmInt 0)))))))))))) (TmForce (TmVar "c_r_6")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0"))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1")) */
const item var_aa_1_1 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_1;
(global_stack.items[global_stack.top++]) = (env[0]);
const item var_e_recF = {.thunk_item = {.code = sys_thunk_16, .env = (item *) malloc(1 * sizeof(item))}};
(var_e_recF.thunk_item.env[0]) = var_e_recF;
var_e_recF.thunk_item.code(var_e_recF.thunk_item.env, ret);
}

void sys_thunk_18(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmApp (TmRec (Param {paramName = "e_recF", paramType = TpUp (TpInt :->: (TpInt :->: TpDown TpInt))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_6" (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmLet "c_f0_5" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_recF")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimIGt (TmVar "e_n") (TmInt 0)) "c_c_3" (TmPrintInt (TmVar "e_x") (TmPrintInt (TmInt 2) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a0_5" (TmTo (TmApp (TmForce (TmVar "c_f0_5")) (TmVar "c_a0_5")) "c_f1_5" (TmApp (TmForce (TmVar "c_f1_5")) (TmVar "e_x")))) (TmReturn (TmInt 0)))))))))))) (TmForce (TmVar "c_r_6")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0"))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1"))))) */
const item var_aa_0_1 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_17;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_1;
}

void sys_thunk_21(item *const _, item *const ret)
{
/* TmLet "aa_0_1_temp" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmApp (TmRec (Param {paramName = "e_recF", paramType = TpUp (TpInt :->: (TpInt :->: TpDown TpInt))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_6" (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmLet "c_f0_5" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_recF")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimIGt (TmVar "e_n") (TmInt 0)) "c_c_3" (TmPrintInt (TmVar "e_x") (TmPrintInt (TmInt 2) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a0_5" (TmTo (TmApp (TmForce (TmVar "c_f0_5")) (TmVar "c_a0_5")) "c_f1_5" (TmApp (TmForce (TmVar "c_f1_5")) (TmVar "e_x")))) (TmReturn (TmInt 0)))))))))))) (TmForce (TmVar "c_r_6")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0"))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1"))))))) (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_1_temp")) (TmVar "aa_0_1")) "aa_1_1_temp" (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_1_temp")) (TmVar "aa_1_1"))))) */
const item var_aa_0_1_temp = {.thunk_item = {.code = sys_thunk_18, .env = NULL}};
const item var_aa_0_1 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_1;
const item sys_t_19 = var_aa_0_1_temp;
sys_t_19.thunk_item.code(sys_t_19.thunk_item.env, ret);
const item var_aa_1_1_temp = (*ret);
const item var_aa_1_1 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_1;
const item sys_t_20 = var_aa_1_1_temp;
sys_t_20.thunk_item.code(sys_t_20.thunk_item.env, ret);
}

void sys_thunk_22(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLet "aa_0_1_temp" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmApp (TmRec (Param {paramName = "e_recF", paramType = TpUp (TpInt :->: (TpInt :->: TpDown TpInt))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_6" (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmLet "c_f0_5" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_recF")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimIGt (TmVar "e_n") (TmInt 0)) "c_c_3" (TmPrintInt (TmVar "e_x") (TmPrintInt (TmInt 2) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a0_5" (TmTo (TmApp (TmForce (TmVar "c_f0_5")) (TmVar "c_a0_5")) "c_f1_5" (TmApp (TmForce (TmVar "c_f1_5")) (TmVar "e_x")))) (TmReturn (TmInt 0)))))))))))) (TmForce (TmVar "c_r_6")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0"))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1"))))))) (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_1_temp")) (TmVar "aa_0_1")) "aa_1_1_temp" (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_1_temp")) (TmVar "aa_1_1"))))))) */
(*ret).thunk_item.code = sys_thunk_21;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_25(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_recF")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) */
const item var_aa_1_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_0;
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_24 = top_e_recF;
sys_t_24.thunk_item.code(sys_t_24.thunk_item.env, ret);
}

void sys_thunk_26(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_recF")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))) */
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_25;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_0;
}

void sys_thunk_29(item *const _, item *const ret)
{
/* TmLet "c_f0_7" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_recF")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmApp (TmForce (TmVar "c_f0_7")) (TmInt 1000000)) "c_f1_7" (TmApp (TmForce (TmVar "c_f1_7")) (TmInt 3))) */
const item var_c_f0_7 = {.thunk_item = {.code = sys_thunk_26, .env = NULL}};
(global_stack.items[global_stack.top++]).int_item = 1000000;
const item sys_t_27 = var_c_f0_7;
sys_t_27.thunk_item.code(sys_t_27.thunk_item.env, ret);
const item var_c_f1_7 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 3;
const item sys_t_28 = var_c_f1_7;
sys_t_28.thunk_item.code(sys_t_28.thunk_item.env, ret);
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmLet "aa_0_1_temp" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmApp (TmRec (Param {paramName = "e_recF", paramType = TpUp (TpInt :->: (TpInt :->: TpDown TpInt))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_6" (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmLet "c_f0_5" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_recF")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimIGt (TmVar "e_n") (TmInt 0)) "c_c_3" (TmPrintInt (TmVar "e_x") (TmPrintInt (TmInt 2) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a0_5" (TmTo (TmApp (TmForce (TmVar "c_f0_5")) (TmVar "c_a0_5")) "c_f1_5" (TmApp (TmForce (TmVar "c_f1_5")) (TmVar "e_x")))) (TmReturn (TmInt 0)))))))))))) (TmForce (TmVar "c_r_6")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0"))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1"))))))) (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_1_temp")) (TmVar "aa_0_1")) "aa_1_1_temp" (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_1_temp")) (TmVar "aa_1_1"))))))) */
const item sys_t_23 = {.thunk_item = {.code = sys_thunk_22, .env = NULL}};
sys_t_23.thunk_item.code(sys_t_23.thunk_item.env, ret);
top_e_recF = (*ret);
/* TmLet "c_f0_7" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_recF")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmApp (TmForce (TmVar "c_f0_7")) (TmInt 1000000)) "c_f1_7" (TmApp (TmForce (TmVar "c_f1_7")) (TmInt 3))) */
const item sys_t_30 = {.thunk_item = {.code = sys_thunk_29, .env = NULL}};
sys_t_30.thunk_item.code(sys_t_30.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

