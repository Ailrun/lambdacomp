#include <runtime.h>

void sys_thunk_6(item *const env, item *const ret);
void sys_thunk_7(item *const env, item *const ret);
void sys_thunk_9(item *const env, item *const ret);
void sys_thunk_10(item *const env, item *const ret);
void sys_thunk_11(item *const env, item *const ret);
item top_e_pow;
void sys_thunk_18(item *const env, item *const ret);
void sys_thunk_20(item *const env, item *const ret);
void sys_thunk_21(item *const env, item *const ret);
void sys_thunk_22(item *const env, item *const ret);
item top_e_fact;
void sys_thunk_29(item *const env, item *const ret);
void sys_thunk_30(item *const env, item *const ret);
void sys_thunk_31(item *const env, item *const ret);
item top_e_sinEntry;
void sys_thunk_56(item *const env, item *const ret);
void sys_thunk_57(item *const env, item *const ret);
item top_e_sin;
void sys_thunk_61(item *const env, item *const ret);
item top_e_main;

void sys_thunk_6(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmInt 0)) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmDouble 1.0)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a1_3" (TmTo (TmApp (TmForce (TmVar "e_pow")) (TmVar "e_x")) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "e_x") (TmVar "c_inp1_4"))))))) */
const item var_e_n = (global_stack.items[--global_stack.top]);
const item sys_arg0_0 = var_e_n;
const item sys_arg1_0 = {.int_item = 0};
(*ret).int_item = sys_arg0_0.int_item <= sys_arg1_0.int_item;
const item var_c_c_1 = (*ret);
const item sys_c_5 = var_c_c_1;
if (sys_c_5.int_item)
{
(*ret).double_item = 1.0;
}
else
{
const item sys_arg0_1 = var_e_n;
const item sys_arg1_1 = {.int_item = 1};
(*ret).int_item = sys_arg0_1.int_item - sys_arg1_1.int_item;
const item var_c_a1_3 = (*ret);
(global_stack.items[global_stack.top++]) = (env[1]);
const item sys_t_2 = (env[0]);
sys_t_2.thunk_item.code(sys_t_2.thunk_item.env, ret);
const item var_c_f1_3 = (*ret);
(global_stack.items[global_stack.top++]) = var_c_a1_3;
const item sys_t_3 = var_c_f1_3;
sys_t_3.thunk_item.code(sys_t_3.thunk_item.env, ret);
const item var_c_inp1_4 = (*ret);
const item sys_arg0_4 = (env[1]);
const item sys_arg1_4 = var_c_inp1_4;
(*ret).double_item = sys_arg0_4.double_item * sys_arg1_4.double_item;
}

}

void sys_thunk_7(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmInt 0)) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmDouble 1.0)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a1_3" (TmTo (TmApp (TmForce (TmVar "e_pow")) (TmVar "e_x")) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "e_x") (TmVar "c_inp1_4")))))))))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_6;
(*ret).thunk_item.env = (item *) malloc(2 * sizeof(item));
((*ret).thunk_item.env[0]) = (env[0]);
((*ret).thunk_item.env[1]) = var_e_x;
}

void sys_thunk_9(item *const env, item *const ret)
{
/* TmLet "c_r_5" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmInt 0)) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmDouble 1.0)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a1_3" (TmTo (TmApp (TmForce (TmVar "e_pow")) (TmVar "e_x")) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "e_x") (TmVar "c_inp1_4")))))))))))) (TmForce (TmVar "c_r_5")) */
const item var_c_r_5 = {.thunk_item = {.code = sys_thunk_7, .env = (item *) malloc(1 * sizeof(item))}};
(var_c_r_5.thunk_item.env[0]) = (env[0]);
const item sys_t_8 = var_c_r_5;
sys_t_8.thunk_item.code(sys_t_8.thunk_item.env, ret);
}

void sys_thunk_10(item *const _, item *const ret)
{
/* TmRec (Param {paramName = "e_pow", paramType = TpUp (TpDouble :->: TpDown (TpUp (TpInt :->: TpDown TpDouble)))}) (TmLet "c_r_5" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmInt 0)) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmDouble 1.0)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a1_3" (TmTo (TmApp (TmForce (TmVar "e_pow")) (TmVar "e_x")) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "e_x") (TmVar "c_inp1_4")))))))))))) (TmForce (TmVar "c_r_5"))) */
const item var_e_pow = {.thunk_item = {.code = sys_thunk_9, .env = (item *) malloc(1 * sizeof(item))}};
(var_e_pow.thunk_item.env[0]) = var_e_pow;
var_e_pow.thunk_item.code(var_e_pow.thunk_item.env, ret);
}

void sys_thunk_11(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmRec (Param {paramName = "e_pow", paramType = TpUp (TpDouble :->: TpDown (TpUp (TpInt :->: TpDown TpDouble)))}) (TmLet "c_r_5" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmInt 0)) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmDouble 1.0)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a1_3" (TmTo (TmApp (TmForce (TmVar "e_pow")) (TmVar "e_x")) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "e_x") (TmVar "c_inp1_4")))))))))))) (TmForce (TmVar "c_r_5"))))) */
(*ret).thunk_item.code = sys_thunk_10;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_18(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmInt 0)) "c_c_7" (TmIf (TmVar "c_c_7") (TmReturn (TmInt 1)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmInt 1)) "c_a0_9" (TmTo (TmApp (TmForce (TmVar "e_fact")) (TmVar "c_a0_9")) "c_inp1_10" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_10")))))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
const item sys_arg0_13 = var_e_x;
const item sys_arg1_13 = {.int_item = 0};
(*ret).int_item = sys_arg0_13.int_item <= sys_arg1_13.int_item;
const item var_c_c_7 = (*ret);
const item sys_c_17 = var_c_c_7;
if (sys_c_17.int_item)
{
(*ret).int_item = 1;
}
else
{
const item sys_arg0_14 = var_e_x;
const item sys_arg1_14 = {.int_item = 1};
(*ret).int_item = sys_arg0_14.int_item - sys_arg1_14.int_item;
const item var_c_a0_9 = (*ret);
(global_stack.items[global_stack.top++]) = var_c_a0_9;
const item sys_t_15 = (env[0]);
sys_t_15.thunk_item.code(sys_t_15.thunk_item.env, ret);
const item var_c_inp1_10 = (*ret);
const item sys_arg0_16 = var_e_x;
const item sys_arg1_16 = var_c_inp1_10;
(*ret).int_item = sys_arg0_16.int_item * sys_arg1_16.int_item;
}

}

void sys_thunk_20(item *const env, item *const ret)
{
/* TmLet "c_r_11" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmInt 0)) "c_c_7" (TmIf (TmVar "c_c_7") (TmReturn (TmInt 1)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmInt 1)) "c_a0_9" (TmTo (TmApp (TmForce (TmVar "e_fact")) (TmVar "c_a0_9")) "c_inp1_10" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_10")))))))) (TmForce (TmVar "c_r_11")) */
const item var_c_r_11 = {.thunk_item = {.code = sys_thunk_18, .env = (item *) malloc(1 * sizeof(item))}};
(var_c_r_11.thunk_item.env[0]) = (env[0]);
const item sys_t_19 = var_c_r_11;
sys_t_19.thunk_item.code(sys_t_19.thunk_item.env, ret);
}

void sys_thunk_21(item *const _, item *const ret)
{
/* TmRec (Param {paramName = "e_fact", paramType = TpUp (TpInt :->: TpDown TpInt)}) (TmLet "c_r_11" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmInt 0)) "c_c_7" (TmIf (TmVar "c_c_7") (TmReturn (TmInt 1)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmInt 1)) "c_a0_9" (TmTo (TmApp (TmForce (TmVar "e_fact")) (TmVar "c_a0_9")) "c_inp1_10" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_10")))))))) (TmForce (TmVar "c_r_11"))) */
const item var_e_fact = {.thunk_item = {.code = sys_thunk_20, .env = (item *) malloc(1 * sizeof(item))}};
(var_e_fact.thunk_item.env[0]) = var_e_fact;
var_e_fact.thunk_item.code(var_e_fact.thunk_item.env, ret);
}

void sys_thunk_22(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmRec (Param {paramName = "e_fact", paramType = TpUp (TpInt :->: TpDown TpInt)}) (TmLet "c_r_11" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmInt 0)) "c_c_7" (TmIf (TmVar "c_c_7") (TmReturn (TmInt 1)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmInt 1)) "c_a0_9" (TmTo (TmApp (TmForce (TmVar "e_fact")) (TmVar "c_a0_9")) "c_inp1_10" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_10")))))))) (TmForce (TmVar "c_r_11"))))) */
(*ret).thunk_item.code = sys_thunk_21;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_29(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmTo (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "e_x")) "c_f1_12" (TmTo (TmApp (TmForce (TmVar "c_f1_12")) (TmVar "e_n")) "c_inp0_15" (TmTo (TmApp (TmForce (TmGlobal "e_fact")) (TmVar "e_n")) "c_inp_14" (TmTo (TmPrimUnOp PrimIToD (TmVar "c_inp_14")) "c_inp1_15" (TmPrimBinOp PrimDDiv (TmVar "c_inp0_15") (TmVar "c_inp1_15")))))) */
const item var_e_n = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_24 = top_e_pow;
sys_t_24.thunk_item.code(sys_t_24.thunk_item.env, ret);
const item var_c_f1_12 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_n;
const item sys_t_25 = var_c_f1_12;
sys_t_25.thunk_item.code(sys_t_25.thunk_item.env, ret);
const item var_c_inp0_15 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_n;
const item sys_t_26 = top_e_fact;
sys_t_26.thunk_item.code(sys_t_26.thunk_item.env, ret);
const item var_c_inp_14 = (*ret);
const item sys_arg_27 = var_c_inp_14;
(*ret).double_item = (double)sys_arg_27.int_item;
const item var_c_inp1_15 = (*ret);
const item sys_arg0_28 = var_c_inp0_15;
const item sys_arg1_28 = var_c_inp1_15;
(*ret).double_item = sys_arg0_28.double_item / sys_arg1_28.double_item;
}

void sys_thunk_30(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmTo (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "e_x")) "c_f1_12" (TmTo (TmApp (TmForce (TmVar "c_f1_12")) (TmVar "e_n")) "c_inp0_15" (TmTo (TmApp (TmForce (TmGlobal "e_fact")) (TmVar "e_n")) "c_inp_14" (TmTo (TmPrimUnOp PrimIToD (TmVar "c_inp_14")) "c_inp1_15" (TmPrimBinOp PrimDDiv (TmVar "c_inp0_15") (TmVar "c_inp1_15"))))))))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_29;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_e_x;
}

void sys_thunk_31(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmTo (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "e_x")) "c_f1_12" (TmTo (TmApp (TmForce (TmVar "c_f1_12")) (TmVar "e_n")) "c_inp0_15" (TmTo (TmApp (TmForce (TmGlobal "e_fact")) (TmVar "e_n")) "c_inp_14" (TmTo (TmPrimUnOp PrimIToD (TmVar "c_inp_14")) "c_inp1_15" (TmPrimBinOp PrimDDiv (TmVar "c_inp0_15") (TmVar "c_inp1_15"))))))))))) */
(*ret).thunk_item.code = sys_thunk_30;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_56(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_16" (TmTo (TmApp (TmForce (TmVar "c_f1_16")) (TmInt 1)) "c_inp0_18" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_17" (TmTo (TmApp (TmForce (TmVar "c_f1_17")) (TmInt 3)) "c_inp1_18" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_18") (TmVar "c_inp1_18")) "c_inp0_20" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_19" (TmTo (TmApp (TmForce (TmVar "c_f1_19")) (TmInt 5)) "c_inp1_20" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_20") (TmVar "c_inp1_20")) "c_inp0_22" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_21" (TmTo (TmApp (TmForce (TmVar "c_f1_21")) (TmInt 7)) "c_inp1_22" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_22") (TmVar "c_inp1_22")) "c_inp0_24" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_23" (TmTo (TmApp (TmForce (TmVar "c_f1_23")) (TmInt 9)) "c_inp1_24" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_24") (TmVar "c_inp1_24")) "c_inp0_26" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_25" (TmTo (TmApp (TmForce (TmVar "c_f1_25")) (TmInt 11)) "c_inp1_26" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_26") (TmVar "c_inp1_26")) "c_inp0_28" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_27" (TmTo (TmApp (TmForce (TmVar "c_f1_27")) (TmInt 13)) "c_inp1_28" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_28") (TmVar "c_inp1_28")) "c_inp0_30" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_29" (TmTo (TmApp (TmForce (TmVar "c_f1_29")) (TmInt 15)) "c_inp1_30" (TmPrimBinOp PrimDSub (TmVar "c_inp0_30") (TmVar "c_inp1_30")))))))))))))))))))))))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_33 = top_e_sinEntry;
sys_t_33.thunk_item.code(sys_t_33.thunk_item.env, ret);
const item var_c_f1_16 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 1;
const item sys_t_34 = var_c_f1_16;
sys_t_34.thunk_item.code(sys_t_34.thunk_item.env, ret);
const item var_c_inp0_18 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_35 = top_e_sinEntry;
sys_t_35.thunk_item.code(sys_t_35.thunk_item.env, ret);
const item var_c_f1_17 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 3;
const item sys_t_36 = var_c_f1_17;
sys_t_36.thunk_item.code(sys_t_36.thunk_item.env, ret);
const item var_c_inp1_18 = (*ret);
const item sys_arg0_37 = var_c_inp0_18;
const item sys_arg1_37 = var_c_inp1_18;
(*ret).double_item = sys_arg0_37.double_item - sys_arg1_37.double_item;
const item var_c_inp0_20 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_38 = top_e_sinEntry;
sys_t_38.thunk_item.code(sys_t_38.thunk_item.env, ret);
const item var_c_f1_19 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 5;
const item sys_t_39 = var_c_f1_19;
sys_t_39.thunk_item.code(sys_t_39.thunk_item.env, ret);
const item var_c_inp1_20 = (*ret);
const item sys_arg0_40 = var_c_inp0_20;
const item sys_arg1_40 = var_c_inp1_20;
(*ret).double_item = sys_arg0_40.double_item + sys_arg1_40.double_item;
const item var_c_inp0_22 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_41 = top_e_sinEntry;
sys_t_41.thunk_item.code(sys_t_41.thunk_item.env, ret);
const item var_c_f1_21 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 7;
const item sys_t_42 = var_c_f1_21;
sys_t_42.thunk_item.code(sys_t_42.thunk_item.env, ret);
const item var_c_inp1_22 = (*ret);
const item sys_arg0_43 = var_c_inp0_22;
const item sys_arg1_43 = var_c_inp1_22;
(*ret).double_item = sys_arg0_43.double_item - sys_arg1_43.double_item;
const item var_c_inp0_24 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_44 = top_e_sinEntry;
sys_t_44.thunk_item.code(sys_t_44.thunk_item.env, ret);
const item var_c_f1_23 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 9;
const item sys_t_45 = var_c_f1_23;
sys_t_45.thunk_item.code(sys_t_45.thunk_item.env, ret);
const item var_c_inp1_24 = (*ret);
const item sys_arg0_46 = var_c_inp0_24;
const item sys_arg1_46 = var_c_inp1_24;
(*ret).double_item = sys_arg0_46.double_item + sys_arg1_46.double_item;
const item var_c_inp0_26 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_47 = top_e_sinEntry;
sys_t_47.thunk_item.code(sys_t_47.thunk_item.env, ret);
const item var_c_f1_25 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 11;
const item sys_t_48 = var_c_f1_25;
sys_t_48.thunk_item.code(sys_t_48.thunk_item.env, ret);
const item var_c_inp1_26 = (*ret);
const item sys_arg0_49 = var_c_inp0_26;
const item sys_arg1_49 = var_c_inp1_26;
(*ret).double_item = sys_arg0_49.double_item - sys_arg1_49.double_item;
const item var_c_inp0_28 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_50 = top_e_sinEntry;
sys_t_50.thunk_item.code(sys_t_50.thunk_item.env, ret);
const item var_c_f1_27 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 13;
const item sys_t_51 = var_c_f1_27;
sys_t_51.thunk_item.code(sys_t_51.thunk_item.env, ret);
const item var_c_inp1_28 = (*ret);
const item sys_arg0_52 = var_c_inp0_28;
const item sys_arg1_52 = var_c_inp1_28;
(*ret).double_item = sys_arg0_52.double_item + sys_arg1_52.double_item;
const item var_c_inp0_30 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_53 = top_e_sinEntry;
sys_t_53.thunk_item.code(sys_t_53.thunk_item.env, ret);
const item var_c_f1_29 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 15;
const item sys_t_54 = var_c_f1_29;
sys_t_54.thunk_item.code(sys_t_54.thunk_item.env, ret);
const item var_c_inp1_30 = (*ret);
const item sys_arg0_55 = var_c_inp0_30;
const item sys_arg1_55 = var_c_inp1_30;
(*ret).double_item = sys_arg0_55.double_item - sys_arg1_55.double_item;
}

void sys_thunk_57(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_16" (TmTo (TmApp (TmForce (TmVar "c_f1_16")) (TmInt 1)) "c_inp0_18" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_17" (TmTo (TmApp (TmForce (TmVar "c_f1_17")) (TmInt 3)) "c_inp1_18" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_18") (TmVar "c_inp1_18")) "c_inp0_20" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_19" (TmTo (TmApp (TmForce (TmVar "c_f1_19")) (TmInt 5)) "c_inp1_20" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_20") (TmVar "c_inp1_20")) "c_inp0_22" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_21" (TmTo (TmApp (TmForce (TmVar "c_f1_21")) (TmInt 7)) "c_inp1_22" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_22") (TmVar "c_inp1_22")) "c_inp0_24" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_23" (TmTo (TmApp (TmForce (TmVar "c_f1_23")) (TmInt 9)) "c_inp1_24" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_24") (TmVar "c_inp1_24")) "c_inp0_26" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_25" (TmTo (TmApp (TmForce (TmVar "c_f1_25")) (TmInt 11)) "c_inp1_26" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_26") (TmVar "c_inp1_26")) "c_inp0_28" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_27" (TmTo (TmApp (TmForce (TmVar "c_f1_27")) (TmInt 13)) "c_inp1_28" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_28") (TmVar "c_inp1_28")) "c_inp0_30" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_29" (TmTo (TmApp (TmForce (TmVar "c_f1_29")) (TmInt 15)) "c_inp1_30" (TmPrimBinOp PrimDSub (TmVar "c_inp0_30") (TmVar "c_inp1_30")))))))))))))))))))))))))) */
(*ret).thunk_item.code = sys_thunk_56;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_61(item *const _, item *const ret)
{
/* TmTo (TmApp (TmForce (TmGlobal "e_sin")) (TmDouble 0.5)) "c_v_32" (TmPrintDouble (TmVar "c_v_32") (TmReturn (TmInt 0))) */
(global_stack.items[global_stack.top++]).double_item = 0.5;
const item sys_t_59 = top_e_sin;
sys_t_59.thunk_item.code(sys_t_59.thunk_item.env, ret);
const item var_c_v_32 = (*ret);
const item sys_msg_60 = var_c_v_32;
printf("%.15g\n", sys_msg_60.double_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmRec (Param {paramName = "e_pow", paramType = TpUp (TpDouble :->: TpDown (TpUp (TpInt :->: TpDown TpDouble)))}) (TmLet "c_r_5" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmInt 0)) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmDouble 1.0)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmInt 1)) "c_a1_3" (TmTo (TmApp (TmForce (TmVar "e_pow")) (TmVar "e_x")) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "e_x") (TmVar "c_inp1_4")))))))))))) (TmForce (TmVar "c_r_5"))))) */
const item sys_t_12 = {.thunk_item = {.code = sys_thunk_11, .env = NULL}};
sys_t_12.thunk_item.code(sys_t_12.thunk_item.env, ret);
top_e_pow = (*ret);
/* TmReturn (TmThunk (TmRec (Param {paramName = "e_fact", paramType = TpUp (TpInt :->: TpDown TpInt)}) (TmLet "c_r_11" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmInt 0)) "c_c_7" (TmIf (TmVar "c_c_7") (TmReturn (TmInt 1)) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmInt 1)) "c_a0_9" (TmTo (TmApp (TmForce (TmVar "e_fact")) (TmVar "c_a0_9")) "c_inp1_10" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_10")))))))) (TmForce (TmVar "c_r_11"))))) */
const item sys_t_23 = {.thunk_item = {.code = sys_thunk_22, .env = NULL}};
sys_t_23.thunk_item.code(sys_t_23.thunk_item.env, ret);
top_e_fact = (*ret);
/* TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_n", paramType = TpInt}) (TmTo (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "e_x")) "c_f1_12" (TmTo (TmApp (TmForce (TmVar "c_f1_12")) (TmVar "e_n")) "c_inp0_15" (TmTo (TmApp (TmForce (TmGlobal "e_fact")) (TmVar "e_n")) "c_inp_14" (TmTo (TmPrimUnOp PrimIToD (TmVar "c_inp_14")) "c_inp1_15" (TmPrimBinOp PrimDDiv (TmVar "c_inp0_15") (TmVar "c_inp1_15"))))))))))) */
const item sys_t_32 = {.thunk_item = {.code = sys_thunk_31, .env = NULL}};
sys_t_32.thunk_item.code(sys_t_32.thunk_item.env, ret);
top_e_sinEntry = (*ret);
/* TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpDouble}) (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_16" (TmTo (TmApp (TmForce (TmVar "c_f1_16")) (TmInt 1)) "c_inp0_18" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_17" (TmTo (TmApp (TmForce (TmVar "c_f1_17")) (TmInt 3)) "c_inp1_18" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_18") (TmVar "c_inp1_18")) "c_inp0_20" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_19" (TmTo (TmApp (TmForce (TmVar "c_f1_19")) (TmInt 5)) "c_inp1_20" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_20") (TmVar "c_inp1_20")) "c_inp0_22" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_21" (TmTo (TmApp (TmForce (TmVar "c_f1_21")) (TmInt 7)) "c_inp1_22" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_22") (TmVar "c_inp1_22")) "c_inp0_24" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_23" (TmTo (TmApp (TmForce (TmVar "c_f1_23")) (TmInt 9)) "c_inp1_24" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_24") (TmVar "c_inp1_24")) "c_inp0_26" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_25" (TmTo (TmApp (TmForce (TmVar "c_f1_25")) (TmInt 11)) "c_inp1_26" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_26") (TmVar "c_inp1_26")) "c_inp0_28" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_27" (TmTo (TmApp (TmForce (TmVar "c_f1_27")) (TmInt 13)) "c_inp1_28" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_28") (TmVar "c_inp1_28")) "c_inp0_30" (TmTo (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) "c_f1_29" (TmTo (TmApp (TmForce (TmVar "c_f1_29")) (TmInt 15)) "c_inp1_30" (TmPrimBinOp PrimDSub (TmVar "c_inp0_30") (TmVar "c_inp1_30")))))))))))))))))))))))))) */
const item sys_t_58 = {.thunk_item = {.code = sys_thunk_57, .env = NULL}};
sys_t_58.thunk_item.code(sys_t_58.thunk_item.env, ret);
top_e_sin = (*ret);
/* TmTo (TmApp (TmForce (TmGlobal "e_sin")) (TmDouble 0.5)) "c_v_32" (TmPrintDouble (TmVar "c_v_32") (TmReturn (TmInt 0))) */
const item sys_t_62 = {.thunk_item = {.code = sys_thunk_61, .env = NULL}};
sys_t_62.thunk_item.code(sys_t_62.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

