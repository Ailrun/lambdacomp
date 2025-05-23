#include <runtime.h>

void sys_thunk_1(item *const env, item *const ret);
void sys_thunk_2(item *const env, item *const ret);
void sys_thunk_3(item *const env, item *const ret);
void sys_thunk_4(item *const env, item *const ret);
void sys_thunk_15(item *const env, item *const ret);
void sys_thunk_16(item *const env, item *const ret);
void sys_thunk_17(item *const env, item *const ret);
void sys_thunk_18(item *const env, item *const ret);
void sys_thunk_20(item *const env, item *const ret);
void sys_thunk_25(item *const env, item *const ret);
void sys_thunk_26(item *const env, item *const ret);
void sys_thunk_27(item *const env, item *const ret);
void sys_thunk_28(item *const env, item *const ret);
void sys_thunk_29(item *const env, item *const ret);
void sys_thunk_34(item *const env, item *const ret);
void sys_thunk_35(item *const env, item *const ret);
item top_e_recFib;
void sys_thunk_38(item *const env, item *const ret);
void sys_thunk_39(item *const env, item *const ret);
void sys_thunk_40(item *const env, item *const ret);
void sys_thunk_41(item *const env, item *const ret);
void sys_thunk_46(item *const env, item *const ret);
void sys_thunk_48(item *const env, item *const ret);
void sys_thunk_49(item *const env, item *const ret);
item top_e_fib;
void sys_thunk_52(item *const env, item *const ret);
void sys_thunk_54(item *const env, item *const ret);
item top_e_main;

void sys_thunk_1(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0")) */
const item var_aa_3_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_3_0;
(global_stack.items[global_stack.top++]) = (env[2]);
(global_stack.items[global_stack.top++]) = (env[1]);
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_0 = (env[3]);
sys_t_0.thunk_item.code(sys_t_0.thunk_item.env, ret);
}

void sys_thunk_2(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))) */
const item var_aa_2_0 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_1;
(*ret).thunk_item.env = (item *) malloc(4 * sizeof(item));
((*ret).thunk_item.env[0]) = (env[0]);
((*ret).thunk_item.env[1]) = (env[1]);
((*ret).thunk_item.env[2]) = var_aa_2_0;
((*ret).thunk_item.env[3]) = (env[2]);
}

void sys_thunk_3(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0")))))))) */
const item var_aa_1_0 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_2;
(*ret).thunk_item.env = (item *) malloc(3 * sizeof(item));
((*ret).thunk_item.env[0]) = (env[0]);
((*ret).thunk_item.env[1]) = var_aa_1_0;
((*ret).thunk_item.env[2]) = (env[1]);
}

void sys_thunk_4(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))) */
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_3;
(*ret).thunk_item.env = (item *) malloc(2 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_0;
((*ret).thunk_item.env[1]) = (env[0]);
}

void sys_thunk_15(item *const env, item *const ret)
{
/* TmLet "c_f0_6" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmTo (TmPrimBinOp PrimILt (TmVar "e_n") (TmVar "e_l")) "c_c_1" (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "c_f0_6")) (TmVar "e_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "e_y")) "c_f3_6" (TmPrintInt (TmVar "e_l") (TmPrintInt (TmVar "e_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6")))))))))))) */
const item var_c_f0_6 = {.thunk_item = {.code = sys_thunk_4, .env = (item *) malloc(1 * sizeof(item))}};
(var_c_f0_6.thunk_item.env[0]) = (env[2]);
const item sys_arg0_5 = (env[1]);
const item sys_arg1_5 = (env[0]);
(*ret).int_item = sys_arg0_5.int_item < sys_arg1_5.int_item;
const item var_c_c_1 = (*ret);
const item var_e_y = (global_stack.items[--global_stack.top]);
const item sys_c_14 = var_c_c_1;
if (sys_c_14.int_item)
{
(*ret).int_item = 0;
}
else
{
const item sys_arg0_6 = (env[0]);
const item sys_arg1_6 = {.int_item = 1};
(*ret).int_item = sys_arg0_6.int_item + sys_arg1_6.int_item;
const item var_c_a1_6 = (*ret);
const item sys_arg0_7 = (env[3]);
const item sys_arg1_7 = var_e_y;
(*ret).int_item = sys_arg0_7.int_item + sys_arg1_7.int_item;
const item var_c_a3_6 = (*ret);
(global_stack.items[global_stack.top++]) = (env[1]);
const item sys_t_8 = var_c_f0_6;
sys_t_8.thunk_item.code(sys_t_8.thunk_item.env, ret);
const item var_c_f1_6 = (*ret);
(global_stack.items[global_stack.top++]) = var_c_a1_6;
const item sys_t_9 = var_c_f1_6;
sys_t_9.thunk_item.code(sys_t_9.thunk_item.env, ret);
const item var_c_f2_6 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_y;
const item sys_t_10 = var_c_f2_6;
sys_t_10.thunk_item.code(sys_t_10.thunk_item.env, ret);
const item var_c_f3_6 = (*ret);
const item sys_msg_13 = (env[0]);
printf("%d\n", sys_msg_13.int_item);
const item sys_msg_12 = (env[3]);
printf("%d\n", sys_msg_12.int_item);
(global_stack.items[global_stack.top++]) = var_c_a3_6;
const item sys_t_11 = var_c_f3_6;
sys_t_11.thunk_item.code(sys_t_11.thunk_item.env, ret);
}

}

void sys_thunk_16(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmThunk (TmLet "c_f0_6" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmTo (TmPrimBinOp PrimILt (TmVar "e_n") (TmVar "e_l")) "c_c_1" (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "c_f0_6")) (TmVar "e_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "e_y")) "c_f3_6" (TmPrintInt (TmVar "e_l") (TmPrintInt (TmVar "e_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6"))))))))))))))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_15;
(*ret).thunk_item.env = (item *) malloc(4 * sizeof(item));
((*ret).thunk_item.env[0]) = (env[0]);
((*ret).thunk_item.env[1]) = (env[1]);
((*ret).thunk_item.env[2]) = (env[2]);
((*ret).thunk_item.env[3]) = var_e_x;
}

void sys_thunk_17(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "e_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmThunk (TmLet "c_f0_6" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmTo (TmPrimBinOp PrimILt (TmVar "e_n") (TmVar "e_l")) "c_c_1" (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "c_f0_6")) (TmVar "e_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "e_y")) "c_f3_6" (TmPrintInt (TmVar "e_l") (TmPrintInt (TmVar "e_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6")))))))))))))))))) */
const item var_e_l = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_16;
(*ret).thunk_item.env = (item *) malloc(3 * sizeof(item));
((*ret).thunk_item.env[0]) = var_e_l;
((*ret).thunk_item.env[1]) = (env[0]);
((*ret).thunk_item.env[2]) = (env[1]);
}

void sys_thunk_18(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmThunk (TmLet "c_f0_6" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmTo (TmPrimBinOp PrimILt (TmVar "e_n") (TmVar "e_l")) "c_c_1" (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "c_f0_6")) (TmVar "e_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "e_y")) "c_f3_6" (TmPrintInt (TmVar "e_l") (TmPrintInt (TmVar "e_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6"))))))))))))))))))))) */
const item var_e_n = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_17;
(*ret).thunk_item.env = (item *) malloc(2 * sizeof(item));
((*ret).thunk_item.env[0]) = var_e_n;
((*ret).thunk_item.env[1]) = (env[0]);
}

void sys_thunk_20(item *const env, item *const ret)
{
/* TmLet "c_r_7" (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmThunk (TmLet "c_f0_6" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmTo (TmPrimBinOp PrimILt (TmVar "e_n") (TmVar "e_l")) "c_c_1" (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "c_f0_6")) (TmVar "e_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "e_y")) "c_f3_6" (TmPrintInt (TmVar "e_l") (TmPrintInt (TmVar "e_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6"))))))))))))))))))))))) (TmForce (TmVar "c_r_7")) */
const item var_c_r_7 = {.thunk_item = {.code = sys_thunk_18, .env = (item *) malloc(1 * sizeof(item))}};
(var_c_r_7.thunk_item.env[0]) = (env[0]);
const item sys_t_19 = var_c_r_7;
sys_t_19.thunk_item.code(sys_t_19.thunk_item.env, ret);
}

void sys_thunk_25(item *const env, item *const ret)
{
/* TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_7" (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmThunk (TmLet "c_f0_6" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmTo (TmPrimBinOp PrimILt (TmVar "e_n") (TmVar "e_l")) "c_c_1" (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "c_f0_6")) (TmVar "e_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "e_y")) "c_f3_6" (TmPrintInt (TmVar "e_l") (TmPrintInt (TmVar "e_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6"))))))))))))))))))))))) (TmForce (TmVar "c_r_7")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0")) "aa_2_0_temp" (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_2_0_temp")) (TmVar "aa_2_0")) "aa_3_0_temp" (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_3_0_temp")) (TmVar "aa_3_0"))))))))) */
const item var_aa_0_0_temp = {.thunk_item = {.code = sys_thunk_20, .env = (item *) malloc(1 * sizeof(item))}};
(var_aa_0_0_temp.thunk_item.env[0]) = (env[0]);
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_0;
const item sys_t_21 = var_aa_0_0_temp;
sys_t_21.thunk_item.code(sys_t_21.thunk_item.env, ret);
const item var_aa_1_0_temp = (*ret);
const item var_aa_1_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_0;
const item sys_t_22 = var_aa_1_0_temp;
sys_t_22.thunk_item.code(sys_t_22.thunk_item.env, ret);
const item var_aa_2_0_temp = (*ret);
const item var_aa_2_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_2_0;
const item sys_t_23 = var_aa_2_0_temp;
sys_t_23.thunk_item.code(sys_t_23.thunk_item.env, ret);
const item var_aa_3_0_temp = (*ret);
const item var_aa_3_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_3_0;
const item sys_t_24 = var_aa_3_0_temp;
sys_t_24.thunk_item.code(sys_t_24.thunk_item.env, ret);
}

void sys_thunk_26(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_3_1", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmRec (Param {paramName = "e_recFib", paramType = TpUp (TpInt :->: (TpInt :->: (TpInt :->: (TpInt :->: TpDown TpInt))))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_7" (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmThunk (TmLet "c_f0_6" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmTo (TmPrimBinOp PrimILt (TmVar "e_n") (TmVar "e_l")) "c_c_1" (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "c_f0_6")) (TmVar "e_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "e_y")) "c_f3_6" (TmPrintInt (TmVar "e_l") (TmPrintInt (TmVar "e_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6"))))))))))))))))))))))) (TmForce (TmVar "c_r_7")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0")) "aa_2_0_temp" (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_2_0_temp")) (TmVar "aa_2_0")) "aa_3_0_temp" (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_3_0_temp")) (TmVar "aa_3_0"))))))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1")) (TmVar "aa_2_1")) (TmVar "aa_3_1")) */
const item var_aa_3_1 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_3_1;
(global_stack.items[global_stack.top++]) = (env[2]);
(global_stack.items[global_stack.top++]) = (env[1]);
(global_stack.items[global_stack.top++]) = (env[0]);
const item var_e_recFib = {.thunk_item = {.code = sys_thunk_25, .env = (item *) malloc(1 * sizeof(item))}};
(var_e_recFib.thunk_item.env[0]) = var_e_recFib;
var_e_recFib.thunk_item.code(var_e_recFib.thunk_item.env, ret);
}

void sys_thunk_27(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_2_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_1", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmRec (Param {paramName = "e_recFib", paramType = TpUp (TpInt :->: (TpInt :->: (TpInt :->: (TpInt :->: TpDown TpInt))))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_7" (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmThunk (TmLet "c_f0_6" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmTo (TmPrimBinOp PrimILt (TmVar "e_n") (TmVar "e_l")) "c_c_1" (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "c_f0_6")) (TmVar "e_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "e_y")) "c_f3_6" (TmPrintInt (TmVar "e_l") (TmPrintInt (TmVar "e_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6"))))))))))))))))))))))) (TmForce (TmVar "c_r_7")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0")) "aa_2_0_temp" (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_2_0_temp")) (TmVar "aa_2_0")) "aa_3_0_temp" (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_3_0_temp")) (TmVar "aa_3_0"))))))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1")) (TmVar "aa_2_1")) (TmVar "aa_3_1"))))) */
const item var_aa_2_1 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_26;
(*ret).thunk_item.env = (item *) malloc(3 * sizeof(item));
((*ret).thunk_item.env[0]) = (env[0]);
((*ret).thunk_item.env[1]) = (env[1]);
((*ret).thunk_item.env[2]) = var_aa_2_1;
}

void sys_thunk_28(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_1", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmRec (Param {paramName = "e_recFib", paramType = TpUp (TpInt :->: (TpInt :->: (TpInt :->: (TpInt :->: TpDown TpInt))))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_7" (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmThunk (TmLet "c_f0_6" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmTo (TmPrimBinOp PrimILt (TmVar "e_n") (TmVar "e_l")) "c_c_1" (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "c_f0_6")) (TmVar "e_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "e_y")) "c_f3_6" (TmPrintInt (TmVar "e_l") (TmPrintInt (TmVar "e_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6"))))))))))))))))))))))) (TmForce (TmVar "c_r_7")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0")) "aa_2_0_temp" (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_2_0_temp")) (TmVar "aa_2_0")) "aa_3_0_temp" (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_3_0_temp")) (TmVar "aa_3_0"))))))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1")) (TmVar "aa_2_1")) (TmVar "aa_3_1")))))))) */
const item var_aa_1_1 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_27;
(*ret).thunk_item.env = (item *) malloc(2 * sizeof(item));
((*ret).thunk_item.env[0]) = (env[0]);
((*ret).thunk_item.env[1]) = var_aa_1_1;
}

void sys_thunk_29(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_1", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmRec (Param {paramName = "e_recFib", paramType = TpUp (TpInt :->: (TpInt :->: (TpInt :->: (TpInt :->: TpDown TpInt))))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_7" (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmThunk (TmLet "c_f0_6" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmTo (TmPrimBinOp PrimILt (TmVar "e_n") (TmVar "e_l")) "c_c_1" (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "c_f0_6")) (TmVar "e_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "e_y")) "c_f3_6" (TmPrintInt (TmVar "e_l") (TmPrintInt (TmVar "e_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6"))))))))))))))))))))))) (TmForce (TmVar "c_r_7")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0")) "aa_2_0_temp" (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_2_0_temp")) (TmVar "aa_2_0")) "aa_3_0_temp" (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_3_0_temp")) (TmVar "aa_3_0"))))))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1")) (TmVar "aa_2_1")) (TmVar "aa_3_1"))))))))))) */
const item var_aa_0_1 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_28;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_1;
}

void sys_thunk_34(item *const _, item *const ret)
{
/* TmLet "aa_0_1_temp" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_1", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmRec (Param {paramName = "e_recFib", paramType = TpUp (TpInt :->: (TpInt :->: (TpInt :->: (TpInt :->: TpDown TpInt))))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_7" (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmThunk (TmLet "c_f0_6" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmTo (TmPrimBinOp PrimILt (TmVar "e_n") (TmVar "e_l")) "c_c_1" (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "c_f0_6")) (TmVar "e_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "e_y")) "c_f3_6" (TmPrintInt (TmVar "e_l") (TmPrintInt (TmVar "e_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6"))))))))))))))))))))))) (TmForce (TmVar "c_r_7")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0")) "aa_2_0_temp" (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_2_0_temp")) (TmVar "aa_2_0")) "aa_3_0_temp" (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_3_0_temp")) (TmVar "aa_3_0"))))))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1")) (TmVar "aa_2_1")) (TmVar "aa_3_1"))))))))))))) (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_1_temp")) (TmVar "aa_0_1")) "aa_1_1_temp" (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_1_1_temp")) (TmVar "aa_1_1")) "aa_2_1_temp" (TmLam (Param {paramName = "aa_2_1", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_2_1_temp")) (TmVar "aa_2_1")) "aa_3_1_temp" (TmLam (Param {paramName = "aa_3_1", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_3_1_temp")) (TmVar "aa_3_1"))))))))) */
const item var_aa_0_1_temp = {.thunk_item = {.code = sys_thunk_29, .env = NULL}};
const item var_aa_0_1 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_1;
const item sys_t_30 = var_aa_0_1_temp;
sys_t_30.thunk_item.code(sys_t_30.thunk_item.env, ret);
const item var_aa_1_1_temp = (*ret);
const item var_aa_1_1 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_1;
const item sys_t_31 = var_aa_1_1_temp;
sys_t_31.thunk_item.code(sys_t_31.thunk_item.env, ret);
const item var_aa_2_1_temp = (*ret);
const item var_aa_2_1 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_2_1;
const item sys_t_32 = var_aa_2_1_temp;
sys_t_32.thunk_item.code(sys_t_32.thunk_item.env, ret);
const item var_aa_3_1_temp = (*ret);
const item var_aa_3_1 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_3_1;
const item sys_t_33 = var_aa_3_1_temp;
sys_t_33.thunk_item.code(sys_t_33.thunk_item.env, ret);
}

void sys_thunk_35(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLet "aa_0_1_temp" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_1", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmRec (Param {paramName = "e_recFib", paramType = TpUp (TpInt :->: (TpInt :->: (TpInt :->: (TpInt :->: TpDown TpInt))))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_7" (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmThunk (TmLet "c_f0_6" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmTo (TmPrimBinOp PrimILt (TmVar "e_n") (TmVar "e_l")) "c_c_1" (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "c_f0_6")) (TmVar "e_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "e_y")) "c_f3_6" (TmPrintInt (TmVar "e_l") (TmPrintInt (TmVar "e_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6"))))))))))))))))))))))) (TmForce (TmVar "c_r_7")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0")) "aa_2_0_temp" (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_2_0_temp")) (TmVar "aa_2_0")) "aa_3_0_temp" (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_3_0_temp")) (TmVar "aa_3_0"))))))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1")) (TmVar "aa_2_1")) (TmVar "aa_3_1"))))))))))))) (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_1_temp")) (TmVar "aa_0_1")) "aa_1_1_temp" (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_1_1_temp")) (TmVar "aa_1_1")) "aa_2_1_temp" (TmLam (Param {paramName = "aa_2_1", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_2_1_temp")) (TmVar "aa_2_1")) "aa_3_1_temp" (TmLam (Param {paramName = "aa_3_1", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_3_1_temp")) (TmVar "aa_3_1"))))))))))) */
(*ret).thunk_item.code = sys_thunk_34;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_38(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0")) */
const item var_aa_3_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_3_0;
(global_stack.items[global_stack.top++]) = (env[2]);
(global_stack.items[global_stack.top++]) = (env[1]);
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_37 = top_e_recFib;
sys_t_37.thunk_item.code(sys_t_37.thunk_item.env, ret);
}

void sys_thunk_39(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))) */
const item var_aa_2_0 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_38;
(*ret).thunk_item.env = (item *) malloc(3 * sizeof(item));
((*ret).thunk_item.env[0]) = (env[0]);
((*ret).thunk_item.env[1]) = (env[1]);
((*ret).thunk_item.env[2]) = var_aa_2_0;
}

void sys_thunk_40(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0")))))))) */
const item var_aa_1_0 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_39;
(*ret).thunk_item.env = (item *) malloc(2 * sizeof(item));
((*ret).thunk_item.env[0]) = (env[0]);
((*ret).thunk_item.env[1]) = var_aa_1_0;
}

void sys_thunk_41(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))) */
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_40;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_0;
}

void sys_thunk_46(item *const _, item *const ret)
{
/* TmLet "c_f0_8" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "c_f0_8")) (TmVar "e_n")) "c_f1_8" (TmTo (TmApp (TmForce (TmVar "c_f1_8")) (TmInt 0)) "c_f2_8" (TmTo (TmApp (TmForce (TmVar "c_f2_8")) (TmInt 0)) "c_f3_8" (TmApp (TmForce (TmVar "c_f3_8")) (TmInt 1)))))) */
const item var_c_f0_8 = {.thunk_item = {.code = sys_thunk_41, .env = NULL}};
const item var_e_n = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_e_n;
const item sys_t_42 = var_c_f0_8;
sys_t_42.thunk_item.code(sys_t_42.thunk_item.env, ret);
const item var_c_f1_8 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 0;
const item sys_t_43 = var_c_f1_8;
sys_t_43.thunk_item.code(sys_t_43.thunk_item.env, ret);
const item var_c_f2_8 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 0;
const item sys_t_44 = var_c_f2_8;
sys_t_44.thunk_item.code(sys_t_44.thunk_item.env, ret);
const item var_c_f3_8 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 1;
const item sys_t_45 = var_c_f3_8;
sys_t_45.thunk_item.code(sys_t_45.thunk_item.env, ret);
}

void sys_thunk_48(item *const _, item *const ret)
{
/* TmLet "aa_0_0_temp" (TmThunk (TmLet "c_f0_8" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "c_f0_8")) (TmVar "e_n")) "c_f1_8" (TmTo (TmApp (TmForce (TmVar "c_f1_8")) (TmInt 0)) "c_f2_8" (TmTo (TmApp (TmForce (TmVar "c_f2_8")) (TmInt 0)) "c_f3_8" (TmApp (TmForce (TmVar "c_f3_8")) (TmInt 1)))))))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0"))) */
const item var_aa_0_0_temp = {.thunk_item = {.code = sys_thunk_46, .env = NULL}};
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_0;
const item sys_t_47 = var_aa_0_0_temp;
sys_t_47.thunk_item.code(sys_t_47.thunk_item.env, ret);
}

void sys_thunk_49(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_f0_8" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "c_f0_8")) (TmVar "e_n")) "c_f1_8" (TmTo (TmApp (TmForce (TmVar "c_f1_8")) (TmInt 0)) "c_f2_8" (TmTo (TmApp (TmForce (TmVar "c_f2_8")) (TmInt 0)) "c_f3_8" (TmApp (TmForce (TmVar "c_f3_8")) (TmInt 1)))))))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0"))))) */
(*ret).thunk_item.code = sys_thunk_48;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_52(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmGlobal "e_fib")) (TmVar "aa_0_0")) */
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_0;
const item sys_t_51 = top_e_fib;
sys_t_51.thunk_item.code(sys_t_51.thunk_item.env, ret);
}

void sys_thunk_54(item *const _, item *const ret)
{
/* TmLet "c_f0_9" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmGlobal "e_fib")) (TmVar "aa_0_0")))) (TmApp (TmForce (TmVar "c_f0_9")) (TmInt 10)) */
const item var_c_f0_9 = {.thunk_item = {.code = sys_thunk_52, .env = NULL}};
(global_stack.items[global_stack.top++]).int_item = 10;
const item sys_t_53 = var_c_f0_9;
sys_t_53.thunk_item.code(sys_t_53.thunk_item.env, ret);
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmLet "aa_0_1_temp" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_1", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_1", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmRec (Param {paramName = "e_recFib", paramType = TpUp (TpInt :->: (TpInt :->: (TpInt :->: (TpInt :->: TpDown TpInt))))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_7" (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmThunk (TmLet "c_f0_6" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmVar "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmTo (TmPrimBinOp PrimILt (TmVar "e_n") (TmVar "e_l")) "c_c_1" (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "c_f0_6")) (TmVar "e_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "e_y")) "c_f3_6" (TmPrintInt (TmVar "e_l") (TmPrintInt (TmVar "e_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6"))))))))))))))))))))))) (TmForce (TmVar "c_r_7")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0")) "aa_2_0_temp" (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_2_0_temp")) (TmVar "aa_2_0")) "aa_3_0_temp" (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_3_0_temp")) (TmVar "aa_3_0"))))))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1")) (TmVar "aa_2_1")) (TmVar "aa_3_1"))))))))))))) (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_1_temp")) (TmVar "aa_0_1")) "aa_1_1_temp" (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_1_1_temp")) (TmVar "aa_1_1")) "aa_2_1_temp" (TmLam (Param {paramName = "aa_2_1", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_2_1_temp")) (TmVar "aa_2_1")) "aa_3_1_temp" (TmLam (Param {paramName = "aa_3_1", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_3_1_temp")) (TmVar "aa_3_1"))))))))))) */
const item sys_t_36 = {.thunk_item = {.code = sys_thunk_35, .env = NULL}};
sys_t_36.thunk_item.code(sys_t_36.thunk_item.env, ret);
top_e_recFib = (*ret);
/* TmReturn (TmThunk (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_f0_8" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_3_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_recFib")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) (TmVar "aa_3_0"))))))))))))) (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "c_f0_8")) (TmVar "e_n")) "c_f1_8" (TmTo (TmApp (TmForce (TmVar "c_f1_8")) (TmInt 0)) "c_f2_8" (TmTo (TmApp (TmForce (TmVar "c_f2_8")) (TmInt 0)) "c_f3_8" (TmApp (TmForce (TmVar "c_f3_8")) (TmInt 1)))))))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0"))))) */
const item sys_t_50 = {.thunk_item = {.code = sys_thunk_49, .env = NULL}};
sys_t_50.thunk_item.code(sys_t_50.thunk_item.env, ret);
top_e_fib = (*ret);
/* TmLet "c_f0_9" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmGlobal "e_fib")) (TmVar "aa_0_0")))) (TmApp (TmForce (TmVar "c_f0_9")) (TmInt 10)) */
const item sys_t_55 = {.thunk_item = {.code = sys_thunk_54, .env = NULL}};
sys_t_55.thunk_item.code(sys_t_55.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

