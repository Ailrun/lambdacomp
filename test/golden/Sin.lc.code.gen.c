#include <runtime.h>

void sys_thunk_1(item *const env, item *const ret);
void sys_thunk_2(item *const env, item *const ret);
void sys_thunk_9(item *const env, item *const ret);
void sys_thunk_10(item *const env, item *const ret);
void sys_thunk_12(item *const env, item *const ret);
void sys_thunk_15(item *const env, item *const ret);
void sys_thunk_16(item *const env, item *const ret);
void sys_thunk_17(item *const env, item *const ret);
void sys_thunk_20(item *const env, item *const ret);
void sys_thunk_21(item *const env, item *const ret);
item top_e_pow;
void sys_thunk_24(item *const env, item *const ret);
void sys_thunk_30(item *const env, item *const ret);
void sys_thunk_32(item *const env, item *const ret);
void sys_thunk_34(item *const env, item *const ret);
void sys_thunk_35(item *const env, item *const ret);
void sys_thunk_37(item *const env, item *const ret);
void sys_thunk_38(item *const env, item *const ret);
item top_e_fact;
void sys_thunk_41(item *const env, item *const ret);
void sys_thunk_42(item *const env, item *const ret);
void sys_thunk_44(item *const env, item *const ret);
void sys_thunk_50(item *const env, item *const ret);
void sys_thunk_51(item *const env, item *const ret);
void sys_thunk_54(item *const env, item *const ret);
void sys_thunk_55(item *const env, item *const ret);
item top_e_sinEntry;
void sys_thunk_58(item *const env, item *const ret);
void sys_thunk_59(item *const env, item *const ret);
void sys_thunk_61(item *const env, item *const ret);
void sys_thunk_62(item *const env, item *const ret);
void sys_thunk_64(item *const env, item *const ret);
void sys_thunk_65(item *const env, item *const ret);
void sys_thunk_67(item *const env, item *const ret);
void sys_thunk_68(item *const env, item *const ret);
void sys_thunk_70(item *const env, item *const ret);
void sys_thunk_71(item *const env, item *const ret);
void sys_thunk_73(item *const env, item *const ret);
void sys_thunk_74(item *const env, item *const ret);
void sys_thunk_76(item *const env, item *const ret);
void sys_thunk_77(item *const env, item *const ret);
void sys_thunk_79(item *const env, item *const ret);
void sys_thunk_80(item *const env, item *const ret);
void sys_thunk_104(item *const env, item *const ret);
void sys_thunk_106(item *const env, item *const ret);
void sys_thunk_107(item *const env, item *const ret);
item top_e_sin;
void sys_thunk_110(item *const env, item *const ret);
void sys_thunk_113(item *const env, item *const ret);
item top_e_main;

void sys_thunk_1(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) */
const item var_aa_1_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_0;
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_0 = (env[1]);
sys_t_0.thunk_item.code(sys_t_0.thunk_item.env, ret);
}

void sys_thunk_2(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))) */
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_1;
(*ret).thunk_item.env = (item *) malloc(2 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_0;
((*ret).thunk_item.env[1]) = (env[0]);
}

void sys_thunk_9(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmLet "c_f0_3" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmInt 0)) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmDouble 1.0)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a1_3" (TmTo (TmApp (TmForce (TmVar "c_f0_3")) (TmVar "e_x")) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "e_x") (TmVar "c_inp1_4")))))))) */
const item var_e_n = (global_stack.items[--global_stack.top]);
const item var_c_f0_3 = {.thunk_item = {.code = sys_thunk_2, .env = (item *) malloc(1 * sizeof(item))}};
(var_c_f0_3.thunk_item.env[0]) = (env[0]);
const item sys_arg0_3 = var_e_n;
const item sys_arg1_3 = {.int_item = 0};
(*ret).int_item = sys_arg0_3.int_item <= sys_arg1_3.int_item;
const item var_c_c_1 = (*ret);
const item sys_c_8 = var_c_c_1;
if (sys_c_8.int_item)
{
(*ret).double_item = 1.0;
}
else
{
const item sys_arg0_4 = var_e_n;
const item sys_arg1_4 = {.int_item = 1};
(*ret).int_item = sys_arg0_4.int_item - sys_arg1_4.int_item;
const item var_c_a1_3 = (*ret);
(global_stack.items[global_stack.top++]) = (env[1]);
const item sys_t_5 = var_c_f0_3;
sys_t_5.thunk_item.code(sys_t_5.thunk_item.env, ret);
const item var_c_f1_3 = (*ret);
(global_stack.items[global_stack.top++]) = var_c_a1_3;
const item sys_t_6 = var_c_f1_3;
sys_t_6.thunk_item.code(sys_t_6.thunk_item.env, ret);
const item var_c_inp1_4 = (*ret);
const item sys_arg0_7 = (env[1]);
const item sys_arg1_7 = var_c_inp1_4;
(*ret).double_item = sys_arg0_7.double_item * sys_arg1_7.double_item;
}

}

void sys_thunk_10(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmLet "c_f0_3" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmInt 0)) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmDouble 1.0)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a1_3" (TmTo (TmApp (TmForce (TmVar "c_f0_3")) (TmVar "e_x")) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "e_x") (TmVar "c_inp1_4"))))))))))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_9;
(*ret).thunk_item.env = (item *) malloc(2 * sizeof(item));
((*ret).thunk_item.env[0]) = (env[0]);
((*ret).thunk_item.env[1]) = var_e_x;
}

void sys_thunk_12(item *const env, item *const ret)
{
/* TmLet "c_r_5" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmLet "c_f0_3" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmInt 0)) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmDouble 1.0)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a1_3" (TmTo (TmApp (TmForce (TmVar "c_f0_3")) (TmVar "e_x")) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "e_x") (TmVar "c_inp1_4"))))))))))))) (TmForce (TmVar "c_r_5")) */
const item var_c_r_5 = {.thunk_item = {.code = sys_thunk_10, .env = (item *) malloc(1 * sizeof(item))}};
(var_c_r_5.thunk_item.env[0]) = (env[0]);
const item sys_t_11 = var_c_r_5;
sys_t_11.thunk_item.code(sys_t_11.thunk_item.env, ret);
}

void sys_thunk_15(item *const env, item *const ret)
{
/* TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_5" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmLet "c_f0_3" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmInt 0)) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmDouble 1.0)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a1_3" (TmTo (TmApp (TmForce (TmVar "c_f0_3")) (TmVar "e_x")) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "e_x") (TmVar "c_inp1_4"))))))))))))) (TmForce (TmVar "c_r_5")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0"))))) */
const item var_aa_0_0_temp = {.thunk_item = {.code = sys_thunk_12, .env = (item *) malloc(1 * sizeof(item))}};
(var_aa_0_0_temp.thunk_item.env[0]) = (env[0]);
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_0;
const item sys_t_13 = var_aa_0_0_temp;
sys_t_13.thunk_item.code(sys_t_13.thunk_item.env, ret);
const item var_aa_1_0_temp = (*ret);
const item var_aa_1_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_0;
const item sys_t_14 = var_aa_1_0_temp;
sys_t_14.thunk_item.code(sys_t_14.thunk_item.env, ret);
}

void sys_thunk_16(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmApp (TmRec (Param {paramName = "e_pow", paramType = TpUp (TpDouble :->: (TpInt :->: TpDown TpDouble))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_5" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmLet "c_f0_3" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmInt 0)) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmDouble 1.0)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a1_3" (TmTo (TmApp (TmForce (TmVar "c_f0_3")) (TmVar "e_x")) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "e_x") (TmVar "c_inp1_4"))))))))))))) (TmForce (TmVar "c_r_5")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0"))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1")) */
const item var_aa_1_1 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_1;
(global_stack.items[global_stack.top++]) = (env[0]);
const item var_e_pow = {.thunk_item = {.code = sys_thunk_15, .env = (item *) malloc(1 * sizeof(item))}};
(var_e_pow.thunk_item.env[0]) = var_e_pow;
var_e_pow.thunk_item.code(var_e_pow.thunk_item.env, ret);
}

void sys_thunk_17(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_1", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmApp (TmRec (Param {paramName = "e_pow", paramType = TpUp (TpDouble :->: (TpInt :->: TpDown TpDouble))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_5" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmLet "c_f0_3" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmInt 0)) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmDouble 1.0)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a1_3" (TmTo (TmApp (TmForce (TmVar "c_f0_3")) (TmVar "e_x")) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "e_x") (TmVar "c_inp1_4"))))))))))))) (TmForce (TmVar "c_r_5")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0"))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1"))))) */
const item var_aa_0_1 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_16;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_1;
}

void sys_thunk_20(item *const _, item *const ret)
{
/* TmLet "aa_0_1_temp" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmApp (TmRec (Param {paramName = "e_pow", paramType = TpUp (TpDouble :->: (TpInt :->: TpDown TpDouble))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_5" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmLet "c_f0_3" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmInt 0)) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmDouble 1.0)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a1_3" (TmTo (TmApp (TmForce (TmVar "c_f0_3")) (TmVar "e_x")) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "e_x") (TmVar "c_inp1_4"))))))))))))) (TmForce (TmVar "c_r_5")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0"))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1"))))))) (TmLam (Param {paramName = "aa_0_1", paramType = TpDouble}) (TmTo (TmApp (TmForce (TmVar "aa_0_1_temp")) (TmVar "aa_0_1")) "aa_1_1_temp" (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_1_temp")) (TmVar "aa_1_1"))))) */
const item var_aa_0_1_temp = {.thunk_item = {.code = sys_thunk_17, .env = NULL}};
const item var_aa_0_1 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_1;
const item sys_t_18 = var_aa_0_1_temp;
sys_t_18.thunk_item.code(sys_t_18.thunk_item.env, ret);
const item var_aa_1_1_temp = (*ret);
const item var_aa_1_1 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_1;
const item sys_t_19 = var_aa_1_1_temp;
sys_t_19.thunk_item.code(sys_t_19.thunk_item.env, ret);
}

void sys_thunk_21(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLet "aa_0_1_temp" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmApp (TmRec (Param {paramName = "e_pow", paramType = TpUp (TpDouble :->: (TpInt :->: TpDown TpDouble))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_5" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmLet "c_f0_3" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmInt 0)) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmDouble 1.0)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a1_3" (TmTo (TmApp (TmForce (TmVar "c_f0_3")) (TmVar "e_x")) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "e_x") (TmVar "c_inp1_4"))))))))))))) (TmForce (TmVar "c_r_5")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0"))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1"))))))) (TmLam (Param {paramName = "aa_0_1", paramType = TpDouble}) (TmTo (TmApp (TmForce (TmVar "aa_0_1_temp")) (TmVar "aa_0_1")) "aa_1_1_temp" (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_1_temp")) (TmVar "aa_1_1"))))))) */
(*ret).thunk_item.code = sys_thunk_20;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_24(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmVar "e_fact")) (TmVar "aa_0_0")) */
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_0;
const item sys_t_23 = (env[0]);
sys_t_23.thunk_item.code(sys_t_23.thunk_item.env, ret);
}

void sys_thunk_30(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmLet "c_f0_9" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmVar "e_fact")) (TmVar "aa_0_0")))) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmInt 0)) "c_c_7" (TmIf (TmVar "c_c_7") (TmReturn (TmInt 1)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmInt 1)) "c_a0_9" (TmTo (TmApp (TmForce (TmVar "c_f0_9")) (TmVar "c_a0_9")) "c_inp1_10" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_10"))))))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
const item var_c_f0_9 = {.thunk_item = {.code = sys_thunk_24, .env = (item *) malloc(1 * sizeof(item))}};
(var_c_f0_9.thunk_item.env[0]) = (env[0]);
const item sys_arg0_25 = var_e_x;
const item sys_arg1_25 = {.int_item = 0};
(*ret).int_item = sys_arg0_25.int_item <= sys_arg1_25.int_item;
const item var_c_c_7 = (*ret);
const item sys_c_29 = var_c_c_7;
if (sys_c_29.int_item)
{
(*ret).int_item = 1;
}
else
{
const item sys_arg0_26 = var_e_x;
const item sys_arg1_26 = {.int_item = 1};
(*ret).int_item = sys_arg0_26.int_item - sys_arg1_26.int_item;
const item var_c_a0_9 = (*ret);
(global_stack.items[global_stack.top++]) = var_c_a0_9;
const item sys_t_27 = var_c_f0_9;
sys_t_27.thunk_item.code(sys_t_27.thunk_item.env, ret);
const item var_c_inp1_10 = (*ret);
const item sys_arg0_28 = var_e_x;
const item sys_arg1_28 = var_c_inp1_10;
(*ret).int_item = sys_arg0_28.int_item * sys_arg1_28.int_item;
}

}

void sys_thunk_32(item *const env, item *const ret)
{
/* TmLet "c_r_11" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmLet "c_f0_9" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmVar "e_fact")) (TmVar "aa_0_0")))) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmInt 0)) "c_c_7" (TmIf (TmVar "c_c_7") (TmReturn (TmInt 1)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmInt 1)) "c_a0_9" (TmTo (TmApp (TmForce (TmVar "c_f0_9")) (TmVar "c_a0_9")) "c_inp1_10" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_10"))))))))) (TmForce (TmVar "c_r_11")) */
const item var_c_r_11 = {.thunk_item = {.code = sys_thunk_30, .env = (item *) malloc(1 * sizeof(item))}};
(var_c_r_11.thunk_item.env[0]) = (env[0]);
const item sys_t_31 = var_c_r_11;
sys_t_31.thunk_item.code(sys_t_31.thunk_item.env, ret);
}

void sys_thunk_34(item *const env, item *const ret)
{
/* TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_11" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmLet "c_f0_9" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmVar "e_fact")) (TmVar "aa_0_0")))) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmInt 0)) "c_c_7" (TmIf (TmVar "c_c_7") (TmReturn (TmInt 1)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmInt 1)) "c_a0_9" (TmTo (TmApp (TmForce (TmVar "c_f0_9")) (TmVar "c_a0_9")) "c_inp1_10" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_10"))))))))) (TmForce (TmVar "c_r_11")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0"))) */
const item var_aa_0_0_temp = {.thunk_item = {.code = sys_thunk_32, .env = (item *) malloc(1 * sizeof(item))}};
(var_aa_0_0_temp.thunk_item.env[0]) = (env[0]);
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_0;
const item sys_t_33 = var_aa_0_0_temp;
sys_t_33.thunk_item.code(sys_t_33.thunk_item.env, ret);
}

void sys_thunk_35(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmApp (TmRec (Param {paramName = "e_fact", paramType = TpUp (TpInt :->: TpDown TpInt)}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_11" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmLet "c_f0_9" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmVar "e_fact")) (TmVar "aa_0_0")))) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmInt 0)) "c_c_7" (TmIf (TmVar "c_c_7") (TmReturn (TmInt 1)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmInt 1)) "c_a0_9" (TmTo (TmApp (TmForce (TmVar "c_f0_9")) (TmVar "c_a0_9")) "c_inp1_10" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_10"))))))))) (TmForce (TmVar "c_r_11")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0"))))) (TmVar "aa_0_1")) */
const item var_aa_0_1 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_1;
const item var_e_fact = {.thunk_item = {.code = sys_thunk_34, .env = (item *) malloc(1 * sizeof(item))}};
(var_e_fact.thunk_item.env[0]) = var_e_fact;
var_e_fact.thunk_item.code(var_e_fact.thunk_item.env, ret);
}

void sys_thunk_37(item *const _, item *const ret)
{
/* TmLet "aa_0_1_temp" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmApp (TmRec (Param {paramName = "e_fact", paramType = TpUp (TpInt :->: TpDown TpInt)}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_11" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmLet "c_f0_9" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmVar "e_fact")) (TmVar "aa_0_0")))) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmInt 0)) "c_c_7" (TmIf (TmVar "c_c_7") (TmReturn (TmInt 1)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmInt 1)) "c_a0_9" (TmTo (TmApp (TmForce (TmVar "c_f0_9")) (TmVar "c_a0_9")) "c_inp1_10" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_10"))))))))) (TmForce (TmVar "c_r_11")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0"))))) (TmVar "aa_0_1")))) (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_0_1_temp")) (TmVar "aa_0_1"))) */
const item var_aa_0_1_temp = {.thunk_item = {.code = sys_thunk_35, .env = NULL}};
const item var_aa_0_1 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_1;
const item sys_t_36 = var_aa_0_1_temp;
sys_t_36.thunk_item.code(sys_t_36.thunk_item.env, ret);
}

void sys_thunk_38(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLet "aa_0_1_temp" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmApp (TmRec (Param {paramName = "e_fact", paramType = TpUp (TpInt :->: TpDown TpInt)}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_11" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmLet "c_f0_9" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmVar "e_fact")) (TmVar "aa_0_0")))) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmInt 0)) "c_c_7" (TmIf (TmVar "c_c_7") (TmReturn (TmInt 1)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmInt 1)) "c_a0_9" (TmTo (TmApp (TmForce (TmVar "c_f0_9")) (TmVar "c_a0_9")) "c_inp1_10" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_10"))))))))) (TmForce (TmVar "c_r_11")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0"))))) (TmVar "aa_0_1")))) (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_0_1_temp")) (TmVar "aa_0_1"))))) */
(*ret).thunk_item.code = sys_thunk_37;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_41(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) */
const item var_aa_1_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_0;
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_40 = top_e_pow;
sys_t_40.thunk_item.code(sys_t_40.thunk_item.env, ret);
}

void sys_thunk_42(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))) */
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_41;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_0;
}

void sys_thunk_44(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmApp (TmForce (TmGlobal "e_fact")) (TmVar "aa_0_1")) */
const item var_aa_0_1 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_1;
const item sys_t_43 = top_e_fact;
sys_t_43.thunk_item.code(sys_t_43.thunk_item.env, ret);
}

void sys_thunk_50(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmLet "c_f0_12" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmLet "c_f0_13" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmApp (TmForce (TmGlobal "e_fact")) (TmVar "aa_0_1")))) (TmTo (TmApp (TmForce (TmVar "c_f0_12")) (TmVar "e_x")) "c_f1_12" (TmTo (TmApp (TmForce (TmVar "c_f1_12")) (TmVar "e_n")) "c_inp0_15" (TmTo (TmApp (TmForce (TmVar "c_f0_13")) (TmVar "e_n")) "c_inp_14" (TmTo (TmPrimUnOp PrimIToD (TmVar "c_inp_14")) "c_inp1_15" (TmPrimBinOp PrimDDiv (TmVar "c_inp0_15") (TmVar "c_inp1_15")))))))) */
const item var_e_n = (global_stack.items[--global_stack.top]);
const item var_c_f0_12 = {.thunk_item = {.code = sys_thunk_42, .env = NULL}};
const item var_c_f0_13 = {.thunk_item = {.code = sys_thunk_44, .env = NULL}};
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_45 = var_c_f0_12;
sys_t_45.thunk_item.code(sys_t_45.thunk_item.env, ret);
const item var_c_f1_12 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_n;
const item sys_t_46 = var_c_f1_12;
sys_t_46.thunk_item.code(sys_t_46.thunk_item.env, ret);
const item var_c_inp0_15 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_n;
const item sys_t_47 = var_c_f0_13;
sys_t_47.thunk_item.code(sys_t_47.thunk_item.env, ret);
const item var_c_inp_14 = (*ret);
const item sys_arg_48 = var_c_inp_14;
(*ret).double_item = (double)sys_arg_48.int_item;
const item var_c_inp1_15 = (*ret);
const item sys_arg0_49 = var_c_inp0_15;
const item sys_arg1_49 = var_c_inp1_15;
(*ret).double_item = sys_arg0_49.double_item / sys_arg1_49.double_item;
}

void sys_thunk_51(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmLet "c_f0_12" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmLet "c_f0_13" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmApp (TmForce (TmGlobal "e_fact")) (TmVar "aa_0_1")))) (TmTo (TmApp (TmForce (TmVar "c_f0_12")) (TmVar "e_x")) "c_f1_12" (TmTo (TmApp (TmForce (TmVar "c_f1_12")) (TmVar "e_n")) "c_inp0_15" (TmTo (TmApp (TmForce (TmVar "c_f0_13")) (TmVar "e_n")) "c_inp_14" (TmTo (TmPrimUnOp PrimIToD (TmVar "c_inp_14")) "c_inp1_15" (TmPrimBinOp PrimDDiv (TmVar "c_inp0_15") (TmVar "c_inp1_15"))))))))))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_50;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_e_x;
}

void sys_thunk_54(item *const _, item *const ret)
{
/* TmLet "aa_0_0_temp" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmLet "c_f0_12" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmLet "c_f0_13" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmApp (TmForce (TmGlobal "e_fact")) (TmVar "aa_0_1")))) (TmTo (TmApp (TmForce (TmVar "c_f0_12")) (TmVar "e_x")) "c_f1_12" (TmTo (TmApp (TmForce (TmVar "c_f1_12")) (TmVar "e_n")) "c_inp0_15" (TmTo (TmApp (TmForce (TmVar "c_f0_13")) (TmVar "e_n")) "c_inp_14" (TmTo (TmPrimUnOp PrimIToD (TmVar "c_inp_14")) "c_inp1_15" (TmPrimBinOp PrimDDiv (TmVar "c_inp0_15") (TmVar "c_inp1_15"))))))))))))) (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0"))))) */
const item var_aa_0_0_temp = {.thunk_item = {.code = sys_thunk_51, .env = NULL}};
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_0;
const item sys_t_52 = var_aa_0_0_temp;
sys_t_52.thunk_item.code(sys_t_52.thunk_item.env, ret);
const item var_aa_1_0_temp = (*ret);
const item var_aa_1_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_0;
const item sys_t_53 = var_aa_1_0_temp;
sys_t_53.thunk_item.code(sys_t_53.thunk_item.env, ret);
}

void sys_thunk_55(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLet "aa_0_0_temp" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmLet "c_f0_12" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmLet "c_f0_13" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmApp (TmForce (TmGlobal "e_fact")) (TmVar "aa_0_1")))) (TmTo (TmApp (TmForce (TmVar "c_f0_12")) (TmVar "e_x")) "c_f1_12" (TmTo (TmApp (TmForce (TmVar "c_f1_12")) (TmVar "e_n")) "c_inp0_15" (TmTo (TmApp (TmForce (TmVar "c_f0_13")) (TmVar "e_n")) "c_inp_14" (TmTo (TmPrimUnOp PrimIToD (TmVar "c_inp_14")) "c_inp1_15" (TmPrimBinOp PrimDDiv (TmVar "c_inp0_15") (TmVar "c_inp1_15"))))))))))))) (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0"))))))) */
(*ret).thunk_item.code = sys_thunk_54;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_58(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) */
const item var_aa_1_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_0;
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_57 = top_e_sinEntry;
sys_t_57.thunk_item.code(sys_t_57.thunk_item.env, ret);
}

void sys_thunk_59(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))) */
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_58;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_0;
}

void sys_thunk_61(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_1")) (TmVar "aa_1_1")) */
const item var_aa_1_1 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_1;
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_60 = top_e_sinEntry;
sys_t_60.thunk_item.code(sys_t_60.thunk_item.env, ret);
}

void sys_thunk_62(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_1", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_1")) (TmVar "aa_1_1"))))) */
const item var_aa_0_1 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_61;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_1;
}

void sys_thunk_64(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_2", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_2")) (TmVar "aa_1_2")) */
const item var_aa_1_2 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_2;
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_63 = top_e_sinEntry;
sys_t_63.thunk_item.code(sys_t_63.thunk_item.env, ret);
}

void sys_thunk_65(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_2", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_2", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_2")) (TmVar "aa_1_2"))))) */
const item var_aa_0_2 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_64;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_2;
}

void sys_thunk_67(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_3", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_3")) (TmVar "aa_1_3")) */
const item var_aa_1_3 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_3;
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_66 = top_e_sinEntry;
sys_t_66.thunk_item.code(sys_t_66.thunk_item.env, ret);
}

void sys_thunk_68(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_3", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_3", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_3")) (TmVar "aa_1_3"))))) */
const item var_aa_0_3 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_67;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_3;
}

void sys_thunk_70(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_4", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_4")) (TmVar "aa_1_4")) */
const item var_aa_1_4 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_4;
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_69 = top_e_sinEntry;
sys_t_69.thunk_item.code(sys_t_69.thunk_item.env, ret);
}

void sys_thunk_71(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_4", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_4", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_4")) (TmVar "aa_1_4"))))) */
const item var_aa_0_4 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_70;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_4;
}

void sys_thunk_73(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_5", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_5")) (TmVar "aa_1_5")) */
const item var_aa_1_5 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_5;
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_72 = top_e_sinEntry;
sys_t_72.thunk_item.code(sys_t_72.thunk_item.env, ret);
}

void sys_thunk_74(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_5", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_5", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_5")) (TmVar "aa_1_5"))))) */
const item var_aa_0_5 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_73;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_5;
}

void sys_thunk_76(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_6", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_6")) (TmVar "aa_1_6")) */
const item var_aa_1_6 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_6;
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_75 = top_e_sinEntry;
sys_t_75.thunk_item.code(sys_t_75.thunk_item.env, ret);
}

void sys_thunk_77(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_6", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_6", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_6")) (TmVar "aa_1_6"))))) */
const item var_aa_0_6 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_76;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_6;
}

void sys_thunk_79(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_7", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_7")) (TmVar "aa_1_7")) */
const item var_aa_1_7 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_7;
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_78 = top_e_sinEntry;
sys_t_78.thunk_item.code(sys_t_78.thunk_item.env, ret);
}

void sys_thunk_80(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_7", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_7", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_7")) (TmVar "aa_1_7"))))) */
const item var_aa_0_7 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_79;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_7;
}

void sys_thunk_104(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmLet "c_f0_16" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmLet "c_f0_17" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_1")) (TmVar "aa_1_1"))))))) (TmLet "c_f0_19" (TmThunk (TmLam (Param {paramName = "aa_0_2", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_2", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_2")) (TmVar "aa_1_2"))))))) (TmLet "c_f0_21" (TmThunk (TmLam (Param {paramName = "aa_0_3", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_3", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_3")) (TmVar "aa_1_3"))))))) (TmLet "c_f0_23" (TmThunk (TmLam (Param {paramName = "aa_0_4", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_4", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_4")) (TmVar "aa_1_4"))))))) (TmLet "c_f0_25" (TmThunk (TmLam (Param {paramName = "aa_0_5", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_5", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_5")) (TmVar "aa_1_5"))))))) (TmLet "c_f0_27" (TmThunk (TmLam (Param {paramName = "aa_0_6", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_6", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_6")) (TmVar "aa_1_6"))))))) (TmLet "c_f0_29" (TmThunk (TmLam (Param {paramName = "aa_0_7", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_7", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_7")) (TmVar "aa_1_7"))))))) (TmTo (TmApp (TmForce (TmVar "c_f0_16")) (TmVar "e_x")) "c_f1_16" (TmTo (TmApp (TmForce (TmVar "c_f1_16")) (TmInt 1)) "c_inp0_18" (TmTo (TmApp (TmForce (TmVar "c_f0_17")) (TmVar "e_x")) "c_f1_17" (TmTo (TmApp (TmForce (TmVar "c_f1_17")) (TmInt 3)) "c_inp1_18" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_18") (TmVar "c_inp1_18")) "c_inp0_20" (TmTo (TmApp (TmForce (TmVar "c_f0_19")) (TmVar "e_x")) "c_f1_19" (TmTo (TmApp (TmForce (TmVar "c_f1_19")) (TmInt 5)) "c_inp1_20" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_20") (TmVar "c_inp1_20")) "c_inp0_22" (TmTo (TmApp (TmForce (TmVar "c_f0_21")) (TmVar "e_x")) "c_f1_21" (TmTo (TmApp (TmForce (TmVar "c_f1_21")) (TmInt 7)) "c_inp1_22" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_22") (TmVar "c_inp1_22")) "c_inp0_24" (TmTo (TmApp (TmForce (TmVar "c_f0_23")) (TmVar "e_x")) "c_f1_23" (TmTo (TmApp (TmForce (TmVar "c_f1_23")) (TmInt 9)) "c_inp1_24" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_24") (TmVar "c_inp1_24")) "c_inp0_26" (TmTo (TmApp (TmForce (TmVar "c_f0_25")) (TmVar "e_x")) "c_f1_25" (TmTo (TmApp (TmForce (TmVar "c_f1_25")) (TmInt 11)) "c_inp1_26" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_26") (TmVar "c_inp1_26")) "c_inp0_28" (TmTo (TmApp (TmForce (TmVar "c_f0_27")) (TmVar "e_x")) "c_f1_27" (TmTo (TmApp (TmForce (TmVar "c_f1_27")) (TmInt 13)) "c_inp1_28" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_28") (TmVar "c_inp1_28")) "c_inp0_30" (TmTo (TmApp (TmForce (TmVar "c_f0_29")) (TmVar "e_x")) "c_f1_29" (TmTo (TmApp (TmForce (TmVar "c_f1_29")) (TmInt 15)) "c_inp1_30" (TmPrimBinOp PrimDSub (TmVar "c_inp0_30") (TmVar "c_inp1_30")))))))))))))))))))))))))))))))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
const item var_c_f0_16 = {.thunk_item = {.code = sys_thunk_59, .env = NULL}};
const item var_c_f0_17 = {.thunk_item = {.code = sys_thunk_62, .env = NULL}};
const item var_c_f0_19 = {.thunk_item = {.code = sys_thunk_65, .env = NULL}};
const item var_c_f0_21 = {.thunk_item = {.code = sys_thunk_68, .env = NULL}};
const item var_c_f0_23 = {.thunk_item = {.code = sys_thunk_71, .env = NULL}};
const item var_c_f0_25 = {.thunk_item = {.code = sys_thunk_74, .env = NULL}};
const item var_c_f0_27 = {.thunk_item = {.code = sys_thunk_77, .env = NULL}};
const item var_c_f0_29 = {.thunk_item = {.code = sys_thunk_80, .env = NULL}};
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_81 = var_c_f0_16;
sys_t_81.thunk_item.code(sys_t_81.thunk_item.env, ret);
const item var_c_f1_16 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 1;
const item sys_t_82 = var_c_f1_16;
sys_t_82.thunk_item.code(sys_t_82.thunk_item.env, ret);
const item var_c_inp0_18 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_83 = var_c_f0_17;
sys_t_83.thunk_item.code(sys_t_83.thunk_item.env, ret);
const item var_c_f1_17 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 3;
const item sys_t_84 = var_c_f1_17;
sys_t_84.thunk_item.code(sys_t_84.thunk_item.env, ret);
const item var_c_inp1_18 = (*ret);
const item sys_arg0_85 = var_c_inp0_18;
const item sys_arg1_85 = var_c_inp1_18;
(*ret).double_item = sys_arg0_85.double_item - sys_arg1_85.double_item;
const item var_c_inp0_20 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_86 = var_c_f0_19;
sys_t_86.thunk_item.code(sys_t_86.thunk_item.env, ret);
const item var_c_f1_19 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 5;
const item sys_t_87 = var_c_f1_19;
sys_t_87.thunk_item.code(sys_t_87.thunk_item.env, ret);
const item var_c_inp1_20 = (*ret);
const item sys_arg0_88 = var_c_inp0_20;
const item sys_arg1_88 = var_c_inp1_20;
(*ret).double_item = sys_arg0_88.double_item + sys_arg1_88.double_item;
const item var_c_inp0_22 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_89 = var_c_f0_21;
sys_t_89.thunk_item.code(sys_t_89.thunk_item.env, ret);
const item var_c_f1_21 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 7;
const item sys_t_90 = var_c_f1_21;
sys_t_90.thunk_item.code(sys_t_90.thunk_item.env, ret);
const item var_c_inp1_22 = (*ret);
const item sys_arg0_91 = var_c_inp0_22;
const item sys_arg1_91 = var_c_inp1_22;
(*ret).double_item = sys_arg0_91.double_item - sys_arg1_91.double_item;
const item var_c_inp0_24 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_92 = var_c_f0_23;
sys_t_92.thunk_item.code(sys_t_92.thunk_item.env, ret);
const item var_c_f1_23 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 9;
const item sys_t_93 = var_c_f1_23;
sys_t_93.thunk_item.code(sys_t_93.thunk_item.env, ret);
const item var_c_inp1_24 = (*ret);
const item sys_arg0_94 = var_c_inp0_24;
const item sys_arg1_94 = var_c_inp1_24;
(*ret).double_item = sys_arg0_94.double_item + sys_arg1_94.double_item;
const item var_c_inp0_26 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_95 = var_c_f0_25;
sys_t_95.thunk_item.code(sys_t_95.thunk_item.env, ret);
const item var_c_f1_25 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 11;
const item sys_t_96 = var_c_f1_25;
sys_t_96.thunk_item.code(sys_t_96.thunk_item.env, ret);
const item var_c_inp1_26 = (*ret);
const item sys_arg0_97 = var_c_inp0_26;
const item sys_arg1_97 = var_c_inp1_26;
(*ret).double_item = sys_arg0_97.double_item - sys_arg1_97.double_item;
const item var_c_inp0_28 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_98 = var_c_f0_27;
sys_t_98.thunk_item.code(sys_t_98.thunk_item.env, ret);
const item var_c_f1_27 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 13;
const item sys_t_99 = var_c_f1_27;
sys_t_99.thunk_item.code(sys_t_99.thunk_item.env, ret);
const item var_c_inp1_28 = (*ret);
const item sys_arg0_100 = var_c_inp0_28;
const item sys_arg1_100 = var_c_inp1_28;
(*ret).double_item = sys_arg0_100.double_item + sys_arg1_100.double_item;
const item var_c_inp0_30 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_101 = var_c_f0_29;
sys_t_101.thunk_item.code(sys_t_101.thunk_item.env, ret);
const item var_c_f1_29 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 15;
const item sys_t_102 = var_c_f1_29;
sys_t_102.thunk_item.code(sys_t_102.thunk_item.env, ret);
const item var_c_inp1_30 = (*ret);
const item sys_arg0_103 = var_c_inp0_30;
const item sys_arg1_103 = var_c_inp1_30;
(*ret).double_item = sys_arg0_103.double_item - sys_arg1_103.double_item;
}

void sys_thunk_106(item *const _, item *const ret)
{
/* TmLet "aa_0_0_temp" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmLet "c_f0_16" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmLet "c_f0_17" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_1")) (TmVar "aa_1_1"))))))) (TmLet "c_f0_19" (TmThunk (TmLam (Param {paramName = "aa_0_2", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_2", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_2")) (TmVar "aa_1_2"))))))) (TmLet "c_f0_21" (TmThunk (TmLam (Param {paramName = "aa_0_3", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_3", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_3")) (TmVar "aa_1_3"))))))) (TmLet "c_f0_23" (TmThunk (TmLam (Param {paramName = "aa_0_4", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_4", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_4")) (TmVar "aa_1_4"))))))) (TmLet "c_f0_25" (TmThunk (TmLam (Param {paramName = "aa_0_5", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_5", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_5")) (TmVar "aa_1_5"))))))) (TmLet "c_f0_27" (TmThunk (TmLam (Param {paramName = "aa_0_6", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_6", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_6")) (TmVar "aa_1_6"))))))) (TmLet "c_f0_29" (TmThunk (TmLam (Param {paramName = "aa_0_7", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_7", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_7")) (TmVar "aa_1_7"))))))) (TmTo (TmApp (TmForce (TmVar "c_f0_16")) (TmVar "e_x")) "c_f1_16" (TmTo (TmApp (TmForce (TmVar "c_f1_16")) (TmInt 1)) "c_inp0_18" (TmTo (TmApp (TmForce (TmVar "c_f0_17")) (TmVar "e_x")) "c_f1_17" (TmTo (TmApp (TmForce (TmVar "c_f1_17")) (TmInt 3)) "c_inp1_18" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_18") (TmVar "c_inp1_18")) "c_inp0_20" (TmTo (TmApp (TmForce (TmVar "c_f0_19")) (TmVar "e_x")) "c_f1_19" (TmTo (TmApp (TmForce (TmVar "c_f1_19")) (TmInt 5)) "c_inp1_20" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_20") (TmVar "c_inp1_20")) "c_inp0_22" (TmTo (TmApp (TmForce (TmVar "c_f0_21")) (TmVar "e_x")) "c_f1_21" (TmTo (TmApp (TmForce (TmVar "c_f1_21")) (TmInt 7)) "c_inp1_22" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_22") (TmVar "c_inp1_22")) "c_inp0_24" (TmTo (TmApp (TmForce (TmVar "c_f0_23")) (TmVar "e_x")) "c_f1_23" (TmTo (TmApp (TmForce (TmVar "c_f1_23")) (TmInt 9)) "c_inp1_24" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_24") (TmVar "c_inp1_24")) "c_inp0_26" (TmTo (TmApp (TmForce (TmVar "c_f0_25")) (TmVar "e_x")) "c_f1_25" (TmTo (TmApp (TmForce (TmVar "c_f1_25")) (TmInt 11)) "c_inp1_26" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_26") (TmVar "c_inp1_26")) "c_inp0_28" (TmTo (TmApp (TmForce (TmVar "c_f0_27")) (TmVar "e_x")) "c_f1_27" (TmTo (TmApp (TmForce (TmVar "c_f1_27")) (TmInt 13)) "c_inp1_28" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_28") (TmVar "c_inp1_28")) "c_inp0_30" (TmTo (TmApp (TmForce (TmVar "c_f0_29")) (TmVar "e_x")) "c_f1_29" (TmTo (TmApp (TmForce (TmVar "c_f1_29")) (TmInt 15)) "c_inp1_30" (TmPrimBinOp PrimDSub (TmVar "c_inp0_30") (TmVar "c_inp1_30")))))))))))))))))))))))))))))))))) (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0"))) */
const item var_aa_0_0_temp = {.thunk_item = {.code = sys_thunk_104, .env = NULL}};
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_0;
const item sys_t_105 = var_aa_0_0_temp;
sys_t_105.thunk_item.code(sys_t_105.thunk_item.env, ret);
}

void sys_thunk_107(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLet "aa_0_0_temp" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmLet "c_f0_16" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmLet "c_f0_17" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_1")) (TmVar "aa_1_1"))))))) (TmLet "c_f0_19" (TmThunk (TmLam (Param {paramName = "aa_0_2", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_2", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_2")) (TmVar "aa_1_2"))))))) (TmLet "c_f0_21" (TmThunk (TmLam (Param {paramName = "aa_0_3", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_3", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_3")) (TmVar "aa_1_3"))))))) (TmLet "c_f0_23" (TmThunk (TmLam (Param {paramName = "aa_0_4", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_4", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_4")) (TmVar "aa_1_4"))))))) (TmLet "c_f0_25" (TmThunk (TmLam (Param {paramName = "aa_0_5", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_5", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_5")) (TmVar "aa_1_5"))))))) (TmLet "c_f0_27" (TmThunk (TmLam (Param {paramName = "aa_0_6", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_6", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_6")) (TmVar "aa_1_6"))))))) (TmLet "c_f0_29" (TmThunk (TmLam (Param {paramName = "aa_0_7", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_7", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_7")) (TmVar "aa_1_7"))))))) (TmTo (TmApp (TmForce (TmVar "c_f0_16")) (TmVar "e_x")) "c_f1_16" (TmTo (TmApp (TmForce (TmVar "c_f1_16")) (TmInt 1)) "c_inp0_18" (TmTo (TmApp (TmForce (TmVar "c_f0_17")) (TmVar "e_x")) "c_f1_17" (TmTo (TmApp (TmForce (TmVar "c_f1_17")) (TmInt 3)) "c_inp1_18" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_18") (TmVar "c_inp1_18")) "c_inp0_20" (TmTo (TmApp (TmForce (TmVar "c_f0_19")) (TmVar "e_x")) "c_f1_19" (TmTo (TmApp (TmForce (TmVar "c_f1_19")) (TmInt 5)) "c_inp1_20" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_20") (TmVar "c_inp1_20")) "c_inp0_22" (TmTo (TmApp (TmForce (TmVar "c_f0_21")) (TmVar "e_x")) "c_f1_21" (TmTo (TmApp (TmForce (TmVar "c_f1_21")) (TmInt 7)) "c_inp1_22" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_22") (TmVar "c_inp1_22")) "c_inp0_24" (TmTo (TmApp (TmForce (TmVar "c_f0_23")) (TmVar "e_x")) "c_f1_23" (TmTo (TmApp (TmForce (TmVar "c_f1_23")) (TmInt 9)) "c_inp1_24" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_24") (TmVar "c_inp1_24")) "c_inp0_26" (TmTo (TmApp (TmForce (TmVar "c_f0_25")) (TmVar "e_x")) "c_f1_25" (TmTo (TmApp (TmForce (TmVar "c_f1_25")) (TmInt 11)) "c_inp1_26" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_26") (TmVar "c_inp1_26")) "c_inp0_28" (TmTo (TmApp (TmForce (TmVar "c_f0_27")) (TmVar "e_x")) "c_f1_27" (TmTo (TmApp (TmForce (TmVar "c_f1_27")) (TmInt 13)) "c_inp1_28" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_28") (TmVar "c_inp1_28")) "c_inp0_30" (TmTo (TmApp (TmForce (TmVar "c_f0_29")) (TmVar "e_x")) "c_f1_29" (TmTo (TmApp (TmForce (TmVar "c_f1_29")) (TmInt 15)) "c_inp1_30" (TmPrimBinOp PrimDSub (TmVar "c_inp0_30") (TmVar "c_inp1_30")))))))))))))))))))))))))))))))))) (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0"))))) */
(*ret).thunk_item.code = sys_thunk_106;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_110(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmApp (TmForce (TmGlobal "e_sin")) (TmVar "aa_0_0")) */
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_0;
const item sys_t_109 = top_e_sin;
sys_t_109.thunk_item.code(sys_t_109.thunk_item.env, ret);
}

void sys_thunk_113(item *const _, item *const ret)
{
/* TmLet "c_f0_31" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmApp (TmForce (TmGlobal "e_sin")) (TmVar "aa_0_0")))) (TmTo (TmApp (TmForce (TmVar "c_f0_31")) (TmDouble 0.5)) "c_v_32" (TmPrintDouble (TmVar "c_v_32") (TmReturn (TmInt 0)))) */
const item var_c_f0_31 = {.thunk_item = {.code = sys_thunk_110, .env = NULL}};
(global_stack.items[global_stack.top++]).double_item = 0.5;
const item sys_t_111 = var_c_f0_31;
sys_t_111.thunk_item.code(sys_t_111.thunk_item.env, ret);
const item var_c_v_32 = (*ret);
const item sys_msg_112 = var_c_v_32;
printf("%.15g\n", sys_msg_112.double_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmLet "aa_0_1_temp" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmApp (TmRec (Param {paramName = "e_pow", paramType = TpUp (TpDouble :->: (TpInt :->: TpDown TpDouble))}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_5" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmLet "c_f0_3" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmInt 0)) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmDouble 1.0)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a1_3" (TmTo (TmApp (TmForce (TmVar "c_f0_3")) (TmVar "e_x")) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "e_x") (TmVar "c_inp1_4"))))))))))))) (TmForce (TmVar "c_r_5")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0"))))))) (TmVar "aa_0_1")) (TmVar "aa_1_1"))))))) (TmLam (Param {paramName = "aa_0_1", paramType = TpDouble}) (TmTo (TmApp (TmForce (TmVar "aa_0_1_temp")) (TmVar "aa_0_1")) "aa_1_1_temp" (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_1_temp")) (TmVar "aa_1_1"))))))) */
const item sys_t_22 = {.thunk_item = {.code = sys_thunk_21, .env = NULL}};
sys_t_22.thunk_item.code(sys_t_22.thunk_item.env, ret);
top_e_pow = (*ret);
/* TmReturn (TmThunk (TmLet "aa_0_1_temp" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmApp (TmRec (Param {paramName = "e_fact", paramType = TpUp (TpInt :->: TpDown TpInt)}) (TmLet "aa_0_0_temp" (TmThunk (TmLet "c_r_11" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmLet "c_f0_9" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmVar "e_fact")) (TmVar "aa_0_0")))) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmInt 0)) "c_c_7" (TmIf (TmVar "c_c_7") (TmReturn (TmInt 1)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmInt 1)) "c_a0_9" (TmTo (TmApp (TmForce (TmVar "c_f0_9")) (TmVar "c_a0_9")) "c_inp1_10" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_10"))))))))) (TmForce (TmVar "c_r_11")))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0"))))) (TmVar "aa_0_1")))) (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_0_1_temp")) (TmVar "aa_0_1"))))) */
const item sys_t_39 = {.thunk_item = {.code = sys_thunk_38, .env = NULL}};
sys_t_39.thunk_item.code(sys_t_39.thunk_item.env, ret);
top_e_fact = (*ret);
/* TmReturn (TmThunk (TmLet "aa_0_0_temp" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmLet "c_f0_12" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmLet "c_f0_13" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpInt}) (TmApp (TmForce (TmGlobal "e_fact")) (TmVar "aa_0_1")))) (TmTo (TmApp (TmForce (TmVar "c_f0_12")) (TmVar "e_x")) "c_f1_12" (TmTo (TmApp (TmForce (TmVar "c_f1_12")) (TmVar "e_n")) "c_inp0_15" (TmTo (TmApp (TmForce (TmVar "c_f0_13")) (TmVar "e_n")) "c_inp_14" (TmTo (TmPrimUnOp PrimIToD (TmVar "c_inp_14")) "c_inp1_15" (TmPrimBinOp PrimDDiv (TmVar "c_inp0_15") (TmVar "c_inp1_15"))))))))))))) (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0"))))))) */
const item sys_t_56 = {.thunk_item = {.code = sys_thunk_55, .env = NULL}};
sys_t_56.thunk_item.code(sys_t_56.thunk_item.env, ret);
top_e_sinEntry = (*ret);
/* TmReturn (TmThunk (TmLet "aa_0_0_temp" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmLet "c_f0_16" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmLet "c_f0_17" (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_1", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_1")) (TmVar "aa_1_1"))))))) (TmLet "c_f0_19" (TmThunk (TmLam (Param {paramName = "aa_0_2", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_2", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_2")) (TmVar "aa_1_2"))))))) (TmLet "c_f0_21" (TmThunk (TmLam (Param {paramName = "aa_0_3", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_3", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_3")) (TmVar "aa_1_3"))))))) (TmLet "c_f0_23" (TmThunk (TmLam (Param {paramName = "aa_0_4", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_4", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_4")) (TmVar "aa_1_4"))))))) (TmLet "c_f0_25" (TmThunk (TmLam (Param {paramName = "aa_0_5", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_5", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_5")) (TmVar "aa_1_5"))))))) (TmLet "c_f0_27" (TmThunk (TmLam (Param {paramName = "aa_0_6", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_6", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_6")) (TmVar "aa_1_6"))))))) (TmLet "c_f0_29" (TmThunk (TmLam (Param {paramName = "aa_0_7", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_7", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "aa_0_7")) (TmVar "aa_1_7"))))))) (TmTo (TmApp (TmForce (TmVar "c_f0_16")) (TmVar "e_x")) "c_f1_16" (TmTo (TmApp (TmForce (TmVar "c_f1_16")) (TmInt 1)) "c_inp0_18" (TmTo (TmApp (TmForce (TmVar "c_f0_17")) (TmVar "e_x")) "c_f1_17" (TmTo (TmApp (TmForce (TmVar "c_f1_17")) (TmInt 3)) "c_inp1_18" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_18") (TmVar "c_inp1_18")) "c_inp0_20" (TmTo (TmApp (TmForce (TmVar "c_f0_19")) (TmVar "e_x")) "c_f1_19" (TmTo (TmApp (TmForce (TmVar "c_f1_19")) (TmInt 5)) "c_inp1_20" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_20") (TmVar "c_inp1_20")) "c_inp0_22" (TmTo (TmApp (TmForce (TmVar "c_f0_21")) (TmVar "e_x")) "c_f1_21" (TmTo (TmApp (TmForce (TmVar "c_f1_21")) (TmInt 7)) "c_inp1_22" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_22") (TmVar "c_inp1_22")) "c_inp0_24" (TmTo (TmApp (TmForce (TmVar "c_f0_23")) (TmVar "e_x")) "c_f1_23" (TmTo (TmApp (TmForce (TmVar "c_f1_23")) (TmInt 9)) "c_inp1_24" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_24") (TmVar "c_inp1_24")) "c_inp0_26" (TmTo (TmApp (TmForce (TmVar "c_f0_25")) (TmVar "e_x")) "c_f1_25" (TmTo (TmApp (TmForce (TmVar "c_f1_25")) (TmInt 11)) "c_inp1_26" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_26") (TmVar "c_inp1_26")) "c_inp0_28" (TmTo (TmApp (TmForce (TmVar "c_f0_27")) (TmVar "e_x")) "c_f1_27" (TmTo (TmApp (TmForce (TmVar "c_f1_27")) (TmInt 13)) "c_inp1_28" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_28") (TmVar "c_inp1_28")) "c_inp0_30" (TmTo (TmApp (TmForce (TmVar "c_f0_29")) (TmVar "e_x")) "c_f1_29" (TmTo (TmApp (TmForce (TmVar "c_f1_29")) (TmInt 15)) "c_inp1_30" (TmPrimBinOp PrimDSub (TmVar "c_inp0_30") (TmVar "c_inp1_30")))))))))))))))))))))))))))))))))) (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0"))))) */
const item sys_t_108 = {.thunk_item = {.code = sys_thunk_107, .env = NULL}};
sys_t_108.thunk_item.code(sys_t_108.thunk_item.env, ret);
top_e_sin = (*ret);
/* TmLet "c_f0_31" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpDouble}) (TmApp (TmForce (TmGlobal "e_sin")) (TmVar "aa_0_0")))) (TmTo (TmApp (TmForce (TmVar "c_f0_31")) (TmDouble 0.5)) "c_v_32" (TmPrintDouble (TmVar "c_v_32") (TmReturn (TmInt 0)))) */
const item sys_t_114 = {.thunk_item = {.code = sys_thunk_113, .env = NULL}};
sys_t_114.thunk_item.code(sys_t_114.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

