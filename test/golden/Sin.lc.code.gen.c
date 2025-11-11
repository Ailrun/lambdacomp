#include <runtime.h>

void sys_thunk_5(item *const env, item *const ret);
void sys_thunk_6(item *const env, item *const ret);
void sys_thunk_7(item *const env, item *const ret);
item top_e_pow;
void sys_thunk_14(item *const env, item *const ret);
void sys_thunk_15(item *const env, item *const ret);
void sys_thunk_16(item *const env, item *const ret);
item top_e_fact;
void sys_thunk_22(item *const env, item *const ret);
void sys_thunk_23(item *const env, item *const ret);
item top_e_sinEntry;
void sys_thunk_40(item *const env, item *const ret);
void sys_thunk_41(item *const env, item *const ret);
item top_e_sin;
void sys_thunk_45(item *const env, item *const ret);
item top_e_main;

void sys_thunk_5(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_4", paramType = TpConst TpCDouble}) (TmLam (Param {paramName = "e_n", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmConst (TmCInt 0))) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmConst (TmCDouble 1.0))) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmConst (TmCInt 1))) "c_a1_3" (TmTo (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_4")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "aa_0_4") (TmVar "c_inp1_4"))))))) */
const item var_aa_0_4 = (global_stack.items[--global_stack.top]);
const item var_e_n = (global_stack.items[--global_stack.top]);
const item sys_arg0_0 = var_e_n;
const item sys_arg1_0 = {.int_item = 0};
(*ret).int_item = sys_arg0_0.int_item <= sys_arg1_0.int_item;
const item var_c_c_1 = (*ret);
const item sys_c_4 = var_c_c_1;
if (sys_c_4.int_item)
{
(*ret).double_item = 1.0;
}
else
{
const item sys_arg0_1 = var_e_n;
const item sys_arg1_1 = {.int_item = 1};
(*ret).int_item = sys_arg0_1.int_item - sys_arg1_1.int_item;
const item var_c_a1_3 = (*ret);
(global_stack.items[global_stack.top++]) = var_c_a1_3;
(global_stack.items[global_stack.top++]) = var_aa_0_4;
const item sys_t_2 = (env[0]);
sys_t_2.thunk_item.code(sys_t_2.thunk_item.env, ret);
const item var_c_inp1_4 = (*ret);
const item sys_arg0_3 = var_aa_0_4;
const item sys_arg1_3 = var_c_inp1_4;
(*ret).double_item = sys_arg0_3.double_item * sys_arg1_3.double_item;
}

}

void sys_thunk_6(item *const _, item *const ret)
{
/* TmRec (Param {paramName = "e_pow", paramType = TpUp (TpConst TpCDouble :->: (TpConst TpCInt :->: TpDown (TpConst TpCDouble)))}) (TmLam (Param {paramName = "aa_0_4", paramType = TpConst TpCDouble}) (TmLam (Param {paramName = "e_n", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmConst (TmCInt 0))) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmConst (TmCDouble 1.0))) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmConst (TmCInt 1))) "c_a1_3" (TmTo (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_4")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "aa_0_4") (TmVar "c_inp1_4")))))))) */
const item var_e_pow = {.thunk_item = {.code = sys_thunk_5, .env = (item *) malloc(1 * sizeof(item))}};
(var_e_pow.thunk_item.env[0]) = var_e_pow;
var_e_pow.thunk_item.code(var_e_pow.thunk_item.env, ret);
}

void sys_thunk_7(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmRec (Param {paramName = "e_pow", paramType = TpUp (TpConst TpCDouble :->: (TpConst TpCInt :->: TpDown (TpConst TpCDouble)))}) (TmLam (Param {paramName = "aa_0_4", paramType = TpConst TpCDouble}) (TmLam (Param {paramName = "e_n", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmConst (TmCInt 0))) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmConst (TmCDouble 1.0))) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmConst (TmCInt 1))) "c_a1_3" (TmTo (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_4")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "aa_0_4") (TmVar "c_inp1_4")))))))))) */
(*ret).thunk_item.code = sys_thunk_6;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_14(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmConst (TmCInt 0))) "c_c_7" (TmIf (TmVar "c_c_7") (TmReturn (TmConst (TmCInt 1))) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmConst (TmCInt 1))) "c_a0_9" (TmTo (TmApp (TmForce (TmVar "e_fact")) (TmVar "c_a0_9")) "c_inp1_10" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_10")))))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
const item sys_arg0_9 = var_e_x;
const item sys_arg1_9 = {.int_item = 0};
(*ret).int_item = sys_arg0_9.int_item <= sys_arg1_9.int_item;
const item var_c_c_7 = (*ret);
const item sys_c_13 = var_c_c_7;
if (sys_c_13.int_item)
{
(*ret).int_item = 1;
}
else
{
const item sys_arg0_10 = var_e_x;
const item sys_arg1_10 = {.int_item = 1};
(*ret).int_item = sys_arg0_10.int_item - sys_arg1_10.int_item;
const item var_c_a0_9 = (*ret);
(global_stack.items[global_stack.top++]) = var_c_a0_9;
const item sys_t_11 = (env[0]);
sys_t_11.thunk_item.code(sys_t_11.thunk_item.env, ret);
const item var_c_inp1_10 = (*ret);
const item sys_arg0_12 = var_e_x;
const item sys_arg1_12 = var_c_inp1_10;
(*ret).int_item = sys_arg0_12.int_item * sys_arg1_12.int_item;
}

}

void sys_thunk_15(item *const _, item *const ret)
{
/* TmRec (Param {paramName = "e_fact", paramType = TpUp (TpConst TpCInt :->: TpDown (TpConst TpCInt))}) (TmLam (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmConst (TmCInt 0))) "c_c_7" (TmIf (TmVar "c_c_7") (TmReturn (TmConst (TmCInt 1))) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmConst (TmCInt 1))) "c_a0_9" (TmTo (TmApp (TmForce (TmVar "e_fact")) (TmVar "c_a0_9")) "c_inp1_10" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_10"))))))) */
const item var_e_fact = {.thunk_item = {.code = sys_thunk_14, .env = (item *) malloc(1 * sizeof(item))}};
(var_e_fact.thunk_item.env[0]) = var_e_fact;
var_e_fact.thunk_item.code(var_e_fact.thunk_item.env, ret);
}

void sys_thunk_16(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmRec (Param {paramName = "e_fact", paramType = TpUp (TpConst TpCInt :->: TpDown (TpConst TpCInt))}) (TmLam (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmConst (TmCInt 0))) "c_c_7" (TmIf (TmVar "c_c_7") (TmReturn (TmConst (TmCInt 1))) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmConst (TmCInt 1))) "c_a0_9" (TmTo (TmApp (TmForce (TmVar "e_fact")) (TmVar "c_a0_9")) "c_inp1_10" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_10"))))))))) */
(*ret).thunk_item.code = sys_thunk_15;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_22(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_1", paramType = TpConst TpCDouble}) (TmLam (Param {paramName = "e_n", paramType = TpConst TpCInt}) (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "aa_0_1")) (TmVar "e_n")) "c_inp0_15" (TmTo (TmApp (TmForce (TmGlobal "e_fact")) (TmVar "e_n")) "c_inp_14" (TmTo (TmPrimUnOp PrimIToD (TmVar "c_inp_14")) "c_inp1_15" (TmPrimBinOp PrimDDiv (TmVar "c_inp0_15") (TmVar "c_inp1_15")))))) */
const item var_aa_0_1 = (global_stack.items[--global_stack.top]);
const item var_e_n = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_e_n;
(global_stack.items[global_stack.top++]) = var_aa_0_1;
const item sys_t_18 = top_e_pow;
sys_t_18.thunk_item.code(sys_t_18.thunk_item.env, ret);
const item var_c_inp0_15 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_n;
const item sys_t_19 = top_e_fact;
sys_t_19.thunk_item.code(sys_t_19.thunk_item.env, ret);
const item var_c_inp_14 = (*ret);
const item sys_arg_20 = var_c_inp_14;
(*ret).double_item = (double)sys_arg_20.int_item;
const item var_c_inp1_15 = (*ret);
const item sys_arg0_21 = var_c_inp0_15;
const item sys_arg1_21 = var_c_inp1_15;
(*ret).double_item = sys_arg0_21.double_item / sys_arg1_21.double_item;
}

void sys_thunk_23(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpConst TpCDouble}) (TmLam (Param {paramName = "e_n", paramType = TpConst TpCInt}) (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "aa_0_1")) (TmVar "e_n")) "c_inp0_15" (TmTo (TmApp (TmForce (TmGlobal "e_fact")) (TmVar "e_n")) "c_inp_14" (TmTo (TmPrimUnOp PrimIToD (TmVar "c_inp_14")) "c_inp1_15" (TmPrimBinOp PrimDDiv (TmVar "c_inp0_15") (TmVar "c_inp1_15")))))))) */
(*ret).thunk_item.code = sys_thunk_22;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_40(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpConst TpCDouble}) (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 1))) "c_inp0_18" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 3))) "c_inp1_18" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_18") (TmVar "c_inp1_18")) "c_inp0_20" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 5))) "c_inp1_20" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_20") (TmVar "c_inp1_20")) "c_inp0_22" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 7))) "c_inp1_22" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_22") (TmVar "c_inp1_22")) "c_inp0_24" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 9))) "c_inp1_24" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_24") (TmVar "c_inp1_24")) "c_inp0_26" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 11))) "c_inp1_26" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_26") (TmVar "c_inp1_26")) "c_inp0_28" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 13))) "c_inp1_28" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_28") (TmVar "c_inp1_28")) "c_inp0_30" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 15))) "c_inp1_30" (TmPrimBinOp PrimDSub (TmVar "c_inp0_30") (TmVar "c_inp1_30")))))))))))))))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]).int_item = 1;
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_25 = top_e_sinEntry;
sys_t_25.thunk_item.code(sys_t_25.thunk_item.env, ret);
const item var_c_inp0_18 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 3;
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_26 = top_e_sinEntry;
sys_t_26.thunk_item.code(sys_t_26.thunk_item.env, ret);
const item var_c_inp1_18 = (*ret);
const item sys_arg0_27 = var_c_inp0_18;
const item sys_arg1_27 = var_c_inp1_18;
(*ret).double_item = sys_arg0_27.double_item - sys_arg1_27.double_item;
const item var_c_inp0_20 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 5;
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_28 = top_e_sinEntry;
sys_t_28.thunk_item.code(sys_t_28.thunk_item.env, ret);
const item var_c_inp1_20 = (*ret);
const item sys_arg0_29 = var_c_inp0_20;
const item sys_arg1_29 = var_c_inp1_20;
(*ret).double_item = sys_arg0_29.double_item + sys_arg1_29.double_item;
const item var_c_inp0_22 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 7;
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_30 = top_e_sinEntry;
sys_t_30.thunk_item.code(sys_t_30.thunk_item.env, ret);
const item var_c_inp1_22 = (*ret);
const item sys_arg0_31 = var_c_inp0_22;
const item sys_arg1_31 = var_c_inp1_22;
(*ret).double_item = sys_arg0_31.double_item - sys_arg1_31.double_item;
const item var_c_inp0_24 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 9;
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_32 = top_e_sinEntry;
sys_t_32.thunk_item.code(sys_t_32.thunk_item.env, ret);
const item var_c_inp1_24 = (*ret);
const item sys_arg0_33 = var_c_inp0_24;
const item sys_arg1_33 = var_c_inp1_24;
(*ret).double_item = sys_arg0_33.double_item + sys_arg1_33.double_item;
const item var_c_inp0_26 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 11;
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_34 = top_e_sinEntry;
sys_t_34.thunk_item.code(sys_t_34.thunk_item.env, ret);
const item var_c_inp1_26 = (*ret);
const item sys_arg0_35 = var_c_inp0_26;
const item sys_arg1_35 = var_c_inp1_26;
(*ret).double_item = sys_arg0_35.double_item - sys_arg1_35.double_item;
const item var_c_inp0_28 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 13;
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_36 = top_e_sinEntry;
sys_t_36.thunk_item.code(sys_t_36.thunk_item.env, ret);
const item var_c_inp1_28 = (*ret);
const item sys_arg0_37 = var_c_inp0_28;
const item sys_arg1_37 = var_c_inp1_28;
(*ret).double_item = sys_arg0_37.double_item + sys_arg1_37.double_item;
const item var_c_inp0_30 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 15;
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_38 = top_e_sinEntry;
sys_t_38.thunk_item.code(sys_t_38.thunk_item.env, ret);
const item var_c_inp1_30 = (*ret);
const item sys_arg0_39 = var_c_inp0_30;
const item sys_arg1_39 = var_c_inp1_30;
(*ret).double_item = sys_arg0_39.double_item - sys_arg1_39.double_item;
}

void sys_thunk_41(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpConst TpCDouble}) (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 1))) "c_inp0_18" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 3))) "c_inp1_18" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_18") (TmVar "c_inp1_18")) "c_inp0_20" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 5))) "c_inp1_20" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_20") (TmVar "c_inp1_20")) "c_inp0_22" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 7))) "c_inp1_22" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_22") (TmVar "c_inp1_22")) "c_inp0_24" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 9))) "c_inp1_24" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_24") (TmVar "c_inp1_24")) "c_inp0_26" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 11))) "c_inp1_26" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_26") (TmVar "c_inp1_26")) "c_inp0_28" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 13))) "c_inp1_28" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_28") (TmVar "c_inp1_28")) "c_inp0_30" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 15))) "c_inp1_30" (TmPrimBinOp PrimDSub (TmVar "c_inp0_30") (TmVar "c_inp1_30")))))))))))))))))) */
(*ret).thunk_item.code = sys_thunk_40;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_45(item *const _, item *const ret)
{
/* TmTo (TmApp (TmForce (TmGlobal "e_sin")) (TmConst (TmCDouble 0.5))) "c_v_32" (TmPrintDouble (TmVar "c_v_32") (TmReturn (TmConst (TmCInt 0)))) */
(global_stack.items[global_stack.top++]).double_item = 0.5;
const item sys_t_43 = top_e_sin;
sys_t_43.thunk_item.code(sys_t_43.thunk_item.env, ret);
const item var_c_v_32 = (*ret);
const item sys_msg_44 = var_c_v_32;
printf("%.15g\n", sys_msg_44.double_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmRec (Param {paramName = "e_pow", paramType = TpUp (TpConst TpCDouble :->: (TpConst TpCInt :->: TpDown (TpConst TpCDouble)))}) (TmLam (Param {paramName = "aa_0_4", paramType = TpConst TpCDouble}) (TmLam (Param {paramName = "e_n", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmConst (TmCInt 0))) "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmConst (TmCDouble 1.0))) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmConst (TmCInt 1))) "c_a1_3" (TmTo (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_4")) (TmVar "c_a1_3")) "c_inp1_4" (TmPrimBinOp PrimDMul (TmVar "aa_0_4") (TmVar "c_inp1_4")))))))))) */
const item sys_t_8 = {.thunk_item = {.code = sys_thunk_7, .env = NULL}};
sys_t_8.thunk_item.code(sys_t_8.thunk_item.env, ret);
top_e_pow = (*ret);
/* TmReturn (TmThunk (TmRec (Param {paramName = "e_fact", paramType = TpUp (TpConst TpCInt :->: TpDown (TpConst TpCInt))}) (TmLam (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmConst (TmCInt 0))) "c_c_7" (TmIf (TmVar "c_c_7") (TmReturn (TmConst (TmCInt 1))) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmConst (TmCInt 1))) "c_a0_9" (TmTo (TmApp (TmForce (TmVar "e_fact")) (TmVar "c_a0_9")) "c_inp1_10" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_10"))))))))) */
const item sys_t_17 = {.thunk_item = {.code = sys_thunk_16, .env = NULL}};
sys_t_17.thunk_item.code(sys_t_17.thunk_item.env, ret);
top_e_fact = (*ret);
/* TmReturn (TmThunk (TmLam (Param {paramName = "aa_0_1", paramType = TpConst TpCDouble}) (TmLam (Param {paramName = "e_n", paramType = TpConst TpCInt}) (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "aa_0_1")) (TmVar "e_n")) "c_inp0_15" (TmTo (TmApp (TmForce (TmGlobal "e_fact")) (TmVar "e_n")) "c_inp_14" (TmTo (TmPrimUnOp PrimIToD (TmVar "c_inp_14")) "c_inp1_15" (TmPrimBinOp PrimDDiv (TmVar "c_inp0_15") (TmVar "c_inp1_15")))))))) */
const item sys_t_24 = {.thunk_item = {.code = sys_thunk_23, .env = NULL}};
sys_t_24.thunk_item.code(sys_t_24.thunk_item.env, ret);
top_e_sinEntry = (*ret);
/* TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpConst TpCDouble}) (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 1))) "c_inp0_18" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 3))) "c_inp1_18" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_18") (TmVar "c_inp1_18")) "c_inp0_20" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 5))) "c_inp1_20" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_20") (TmVar "c_inp1_20")) "c_inp0_22" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 7))) "c_inp1_22" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_22") (TmVar "c_inp1_22")) "c_inp0_24" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 9))) "c_inp1_24" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_24") (TmVar "c_inp1_24")) "c_inp0_26" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 11))) "c_inp1_26" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_26") (TmVar "c_inp1_26")) "c_inp0_28" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 13))) "c_inp1_28" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_28") (TmVar "c_inp1_28")) "c_inp0_30" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 15))) "c_inp1_30" (TmPrimBinOp PrimDSub (TmVar "c_inp0_30") (TmVar "c_inp1_30")))))))))))))))))) */
const item sys_t_42 = {.thunk_item = {.code = sys_thunk_41, .env = NULL}};
sys_t_42.thunk_item.code(sys_t_42.thunk_item.env, ret);
top_e_sin = (*ret);
/* TmTo (TmApp (TmForce (TmGlobal "e_sin")) (TmConst (TmCDouble 0.5))) "c_v_32" (TmPrintDouble (TmVar "c_v_32") (TmReturn (TmConst (TmCInt 0)))) */
const item sys_t_46 = {.thunk_item = {.code = sys_thunk_45, .env = NULL}};
sys_t_46.thunk_item.code(sys_t_46.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

