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
/* TmLam (BTyped (Param {paramName = "aa_0_4", paramType = TpConst TpCDouble}) (TmLam (BTyped (Param {paramName = "e_n", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmConst (TmCInt 0))) (BUntyped "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmConst (TmCDouble 1.0))) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmConst (TmCInt 1))) (BUntyped "c_a_4" (TmTo (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_4")) (TmVar "c_a_4")) (BUntyped "c_inp1_5" (TmPrimBinOp PrimDMul (TmVar "aa_0_4") (TmVar "c_inp1_5")))))))))))) */
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
const item var_c_a_4 = (*ret);
(global_stack.items[global_stack.top++]) = var_c_a_4;
(global_stack.items[global_stack.top++]) = var_aa_0_4;
const item sys_t_2 = (env[0]);
sys_t_2.thunk_item.code(sys_t_2.thunk_item.env, ret);
const item var_c_inp1_5 = (*ret);
const item sys_arg0_3 = var_aa_0_4;
const item sys_arg1_3 = var_c_inp1_5;
(*ret).double_item = sys_arg0_3.double_item * sys_arg1_3.double_item;
}

}

void sys_thunk_6(item *const _, item *const ret)
{
/* TmRec (BTyped (Param {paramName = "e_pow", paramType = TpUp (TpConst TpCDouble :->: (TpConst TpCInt :->: TpDown (TpConst TpCDouble)))}) (TmLam (BTyped (Param {paramName = "aa_0_4", paramType = TpConst TpCDouble}) (TmLam (BTyped (Param {paramName = "e_n", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmConst (TmCInt 0))) (BUntyped "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmConst (TmCDouble 1.0))) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmConst (TmCInt 1))) (BUntyped "c_a_4" (TmTo (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_4")) (TmVar "c_a_4")) (BUntyped "c_inp1_5" (TmPrimBinOp PrimDMul (TmVar "aa_0_4") (TmVar "c_inp1_5")))))))))))))) */
const item var_e_pow = {.thunk_item = {.code = sys_thunk_5, .env = (item *) malloc(1 * sizeof(item))}};
(var_e_pow.thunk_item.env[0]) = var_e_pow;
var_e_pow.thunk_item.code(var_e_pow.thunk_item.env, ret);
}

void sys_thunk_7(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmRec (BTyped (Param {paramName = "e_pow", paramType = TpUp (TpConst TpCDouble :->: (TpConst TpCInt :->: TpDown (TpConst TpCDouble)))}) (TmLam (BTyped (Param {paramName = "aa_0_4", paramType = TpConst TpCDouble}) (TmLam (BTyped (Param {paramName = "e_n", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmConst (TmCInt 0))) (BUntyped "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmConst (TmCDouble 1.0))) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmConst (TmCInt 1))) (BUntyped "c_a_4" (TmTo (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_4")) (TmVar "c_a_4")) (BUntyped "c_inp1_5" (TmPrimBinOp PrimDMul (TmVar "aa_0_4") (TmVar "c_inp1_5")))))))))))))))) */
(*ret).thunk_item.code = sys_thunk_6;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_14(item *const env, item *const ret)
{
/* TmLam (BTyped (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmConst (TmCInt 0))) (BUntyped "c_c_8" (TmIf (TmVar "c_c_8") (TmReturn (TmConst (TmCInt 1))) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmConst (TmCInt 1))) (BUntyped "c_a_10" (TmTo (TmApp (TmForce (TmVar "e_fact")) (TmVar "c_a_10")) (BUntyped "c_inp1_11" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_11")))))))))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
const item sys_arg0_9 = var_e_x;
const item sys_arg1_9 = {.int_item = 0};
(*ret).int_item = sys_arg0_9.int_item <= sys_arg1_9.int_item;
const item var_c_c_8 = (*ret);
const item sys_c_13 = var_c_c_8;
if (sys_c_13.int_item)
{
(*ret).int_item = 1;
}
else
{
const item sys_arg0_10 = var_e_x;
const item sys_arg1_10 = {.int_item = 1};
(*ret).int_item = sys_arg0_10.int_item - sys_arg1_10.int_item;
const item var_c_a_10 = (*ret);
(global_stack.items[global_stack.top++]) = var_c_a_10;
const item sys_t_11 = (env[0]);
sys_t_11.thunk_item.code(sys_t_11.thunk_item.env, ret);
const item var_c_inp1_11 = (*ret);
const item sys_arg0_12 = var_e_x;
const item sys_arg1_12 = var_c_inp1_11;
(*ret).int_item = sys_arg0_12.int_item * sys_arg1_12.int_item;
}

}

void sys_thunk_15(item *const _, item *const ret)
{
/* TmRec (BTyped (Param {paramName = "e_fact", paramType = TpUp (TpConst TpCInt :->: TpDown (TpConst TpCInt))}) (TmLam (BTyped (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmConst (TmCInt 0))) (BUntyped "c_c_8" (TmIf (TmVar "c_c_8") (TmReturn (TmConst (TmCInt 1))) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmConst (TmCInt 1))) (BUntyped "c_a_10" (TmTo (TmApp (TmForce (TmVar "e_fact")) (TmVar "c_a_10")) (BUntyped "c_inp1_11" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_11")))))))))))) */
const item var_e_fact = {.thunk_item = {.code = sys_thunk_14, .env = (item *) malloc(1 * sizeof(item))}};
(var_e_fact.thunk_item.env[0]) = var_e_fact;
var_e_fact.thunk_item.code(var_e_fact.thunk_item.env, ret);
}

void sys_thunk_16(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmRec (BTyped (Param {paramName = "e_fact", paramType = TpUp (TpConst TpCInt :->: TpDown (TpConst TpCInt))}) (TmLam (BTyped (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmConst (TmCInt 0))) (BUntyped "c_c_8" (TmIf (TmVar "c_c_8") (TmReturn (TmConst (TmCInt 1))) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmConst (TmCInt 1))) (BUntyped "c_a_10" (TmTo (TmApp (TmForce (TmVar "e_fact")) (TmVar "c_a_10")) (BUntyped "c_inp1_11" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_11")))))))))))))) */
(*ret).thunk_item.code = sys_thunk_15;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_22(item *const _, item *const ret)
{
/* TmLam (BTyped (Param {paramName = "aa_0_1", paramType = TpConst TpCDouble}) (TmLam (BTyped (Param {paramName = "e_n", paramType = TpConst TpCInt}) (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "aa_0_1")) (TmVar "e_n")) (BUntyped "c_inp0_17" (TmTo (TmApp (TmForce (TmGlobal "e_fact")) (TmVar "e_n")) (BUntyped "c_inp_16" (TmTo (TmPrimUnOp PrimIToD (TmVar "c_inp_16")) (BUntyped "c_inp1_17" (TmPrimBinOp PrimDDiv (TmVar "c_inp0_17") (TmVar "c_inp1_17"))))))))))) */
const item var_aa_0_1 = (global_stack.items[--global_stack.top]);
const item var_e_n = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_e_n;
(global_stack.items[global_stack.top++]) = var_aa_0_1;
const item sys_t_18 = top_e_pow;
sys_t_18.thunk_item.code(sys_t_18.thunk_item.env, ret);
const item var_c_inp0_17 = (*ret);
(global_stack.items[global_stack.top++]) = var_e_n;
const item sys_t_19 = top_e_fact;
sys_t_19.thunk_item.code(sys_t_19.thunk_item.env, ret);
const item var_c_inp_16 = (*ret);
const item sys_arg_20 = var_c_inp_16;
(*ret).double_item = (double)sys_arg_20.int_item;
const item var_c_inp1_17 = (*ret);
const item sys_arg0_21 = var_c_inp0_17;
const item sys_arg1_21 = var_c_inp1_17;
(*ret).double_item = sys_arg0_21.double_item / sys_arg1_21.double_item;
}

void sys_thunk_23(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLam (BTyped (Param {paramName = "aa_0_1", paramType = TpConst TpCDouble}) (TmLam (BTyped (Param {paramName = "e_n", paramType = TpConst TpCInt}) (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "aa_0_1")) (TmVar "e_n")) (BUntyped "c_inp0_17" (TmTo (TmApp (TmForce (TmGlobal "e_fact")) (TmVar "e_n")) (BUntyped "c_inp_16" (TmTo (TmPrimUnOp PrimIToD (TmVar "c_inp_16")) (BUntyped "c_inp1_17" (TmPrimBinOp PrimDDiv (TmVar "c_inp0_17") (TmVar "c_inp1_17"))))))))))))) */
(*ret).thunk_item.code = sys_thunk_22;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_40(item *const _, item *const ret)
{
/* TmLam (BTyped (Param {paramName = "e_x", paramType = TpConst TpCDouble}) (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 1))) (BUntyped "c_inp0_22" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 3))) (BUntyped "c_inp1_22" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_22") (TmVar "c_inp1_22")) (BUntyped "c_inp0_25" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 5))) (BUntyped "c_inp1_25" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_25") (TmVar "c_inp1_25")) (BUntyped "c_inp0_28" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 7))) (BUntyped "c_inp1_28" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_28") (TmVar "c_inp1_28")) (BUntyped "c_inp0_31" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 9))) (BUntyped "c_inp1_31" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_31") (TmVar "c_inp1_31")) (BUntyped "c_inp0_34" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 11))) (BUntyped "c_inp1_34" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_34") (TmVar "c_inp1_34")) (BUntyped "c_inp0_37" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 13))) (BUntyped "c_inp1_37" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_37") (TmVar "c_inp1_37")) (BUntyped "c_inp0_40" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 15))) (BUntyped "c_inp1_40" (TmPrimBinOp PrimDSub (TmVar "c_inp0_40") (TmVar "c_inp1_40"))))))))))))))))))))))))))))))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]).int_item = 1;
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_25 = top_e_sinEntry;
sys_t_25.thunk_item.code(sys_t_25.thunk_item.env, ret);
const item var_c_inp0_22 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 3;
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_26 = top_e_sinEntry;
sys_t_26.thunk_item.code(sys_t_26.thunk_item.env, ret);
const item var_c_inp1_22 = (*ret);
const item sys_arg0_27 = var_c_inp0_22;
const item sys_arg1_27 = var_c_inp1_22;
(*ret).double_item = sys_arg0_27.double_item - sys_arg1_27.double_item;
const item var_c_inp0_25 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 5;
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_28 = top_e_sinEntry;
sys_t_28.thunk_item.code(sys_t_28.thunk_item.env, ret);
const item var_c_inp1_25 = (*ret);
const item sys_arg0_29 = var_c_inp0_25;
const item sys_arg1_29 = var_c_inp1_25;
(*ret).double_item = sys_arg0_29.double_item + sys_arg1_29.double_item;
const item var_c_inp0_28 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 7;
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_30 = top_e_sinEntry;
sys_t_30.thunk_item.code(sys_t_30.thunk_item.env, ret);
const item var_c_inp1_28 = (*ret);
const item sys_arg0_31 = var_c_inp0_28;
const item sys_arg1_31 = var_c_inp1_28;
(*ret).double_item = sys_arg0_31.double_item - sys_arg1_31.double_item;
const item var_c_inp0_31 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 9;
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_32 = top_e_sinEntry;
sys_t_32.thunk_item.code(sys_t_32.thunk_item.env, ret);
const item var_c_inp1_31 = (*ret);
const item sys_arg0_33 = var_c_inp0_31;
const item sys_arg1_33 = var_c_inp1_31;
(*ret).double_item = sys_arg0_33.double_item + sys_arg1_33.double_item;
const item var_c_inp0_34 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 11;
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_34 = top_e_sinEntry;
sys_t_34.thunk_item.code(sys_t_34.thunk_item.env, ret);
const item var_c_inp1_34 = (*ret);
const item sys_arg0_35 = var_c_inp0_34;
const item sys_arg1_35 = var_c_inp1_34;
(*ret).double_item = sys_arg0_35.double_item - sys_arg1_35.double_item;
const item var_c_inp0_37 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 13;
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_36 = top_e_sinEntry;
sys_t_36.thunk_item.code(sys_t_36.thunk_item.env, ret);
const item var_c_inp1_37 = (*ret);
const item sys_arg0_37 = var_c_inp0_37;
const item sys_arg1_37 = var_c_inp1_37;
(*ret).double_item = sys_arg0_37.double_item + sys_arg1_37.double_item;
const item var_c_inp0_40 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 15;
(global_stack.items[global_stack.top++]) = var_e_x;
const item sys_t_38 = top_e_sinEntry;
sys_t_38.thunk_item.code(sys_t_38.thunk_item.env, ret);
const item var_c_inp1_40 = (*ret);
const item sys_arg0_39 = var_c_inp0_40;
const item sys_arg1_39 = var_c_inp1_40;
(*ret).double_item = sys_arg0_39.double_item - sys_arg1_39.double_item;
}

void sys_thunk_41(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLam (BTyped (Param {paramName = "e_x", paramType = TpConst TpCDouble}) (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 1))) (BUntyped "c_inp0_22" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 3))) (BUntyped "c_inp1_22" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_22") (TmVar "c_inp1_22")) (BUntyped "c_inp0_25" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 5))) (BUntyped "c_inp1_25" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_25") (TmVar "c_inp1_25")) (BUntyped "c_inp0_28" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 7))) (BUntyped "c_inp1_28" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_28") (TmVar "c_inp1_28")) (BUntyped "c_inp0_31" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 9))) (BUntyped "c_inp1_31" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_31") (TmVar "c_inp1_31")) (BUntyped "c_inp0_34" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 11))) (BUntyped "c_inp1_34" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_34") (TmVar "c_inp1_34")) (BUntyped "c_inp0_37" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 13))) (BUntyped "c_inp1_37" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_37") (TmVar "c_inp1_37")) (BUntyped "c_inp0_40" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 15))) (BUntyped "c_inp1_40" (TmPrimBinOp PrimDSub (TmVar "c_inp0_40") (TmVar "c_inp1_40"))))))))))))))))))))))))))))))))) */
(*ret).thunk_item.code = sys_thunk_40;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_45(item *const _, item *const ret)
{
/* TmTo (TmApp (TmForce (TmGlobal "e_sin")) (TmConst (TmCDouble 0.5))) (BUntyped "c_v_42" (TmPrintDouble (TmVar "c_v_42") (TmReturn (TmConst (TmCInt 0))))) */
(global_stack.items[global_stack.top++]).double_item = 0.5;
const item sys_t_43 = top_e_sin;
sys_t_43.thunk_item.code(sys_t_43.thunk_item.env, ret);
const item var_c_v_42 = (*ret);
const item sys_msg_44 = var_c_v_42;
printf("%.15g\n", sys_msg_44.double_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmRec (BTyped (Param {paramName = "e_pow", paramType = TpUp (TpConst TpCDouble :->: (TpConst TpCInt :->: TpDown (TpConst TpCDouble)))}) (TmLam (BTyped (Param {paramName = "aa_0_4", paramType = TpConst TpCDouble}) (TmLam (BTyped (Param {paramName = "e_n", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_n") (TmConst (TmCInt 0))) (BUntyped "c_c_1" (TmIf (TmVar "c_c_1") (TmReturn (TmConst (TmCDouble 1.0))) (TmTo (TmPrimBinOp PrimISub (TmVar "e_n") (TmConst (TmCInt 1))) (BUntyped "c_a_4" (TmTo (TmApp (TmApp (TmForce (TmVar "e_pow")) (TmVar "aa_0_4")) (TmVar "c_a_4")) (BUntyped "c_inp1_5" (TmPrimBinOp PrimDMul (TmVar "aa_0_4") (TmVar "c_inp1_5")))))))))))))))) */
const item sys_t_8 = {.thunk_item = {.code = sys_thunk_7, .env = NULL}};
sys_t_8.thunk_item.code(sys_t_8.thunk_item.env, ret);
top_e_pow = (*ret);
/* TmReturn (TmThunk (TmRec (BTyped (Param {paramName = "e_fact", paramType = TpUp (TpConst TpCInt :->: TpDown (TpConst TpCInt))}) (TmLam (BTyped (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmTo (TmPrimBinOp PrimILe (TmVar "e_x") (TmConst (TmCInt 0))) (BUntyped "c_c_8" (TmIf (TmVar "c_c_8") (TmReturn (TmConst (TmCInt 1))) (TmTo (TmPrimBinOp PrimISub (TmVar "e_x") (TmConst (TmCInt 1))) (BUntyped "c_a_10" (TmTo (TmApp (TmForce (TmVar "e_fact")) (TmVar "c_a_10")) (BUntyped "c_inp1_11" (TmPrimBinOp PrimIMul (TmVar "e_x") (TmVar "c_inp1_11")))))))))))))) */
const item sys_t_17 = {.thunk_item = {.code = sys_thunk_16, .env = NULL}};
sys_t_17.thunk_item.code(sys_t_17.thunk_item.env, ret);
top_e_fact = (*ret);
/* TmReturn (TmThunk (TmLam (BTyped (Param {paramName = "aa_0_1", paramType = TpConst TpCDouble}) (TmLam (BTyped (Param {paramName = "e_n", paramType = TpConst TpCInt}) (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_pow")) (TmVar "aa_0_1")) (TmVar "e_n")) (BUntyped "c_inp0_17" (TmTo (TmApp (TmForce (TmGlobal "e_fact")) (TmVar "e_n")) (BUntyped "c_inp_16" (TmTo (TmPrimUnOp PrimIToD (TmVar "c_inp_16")) (BUntyped "c_inp1_17" (TmPrimBinOp PrimDDiv (TmVar "c_inp0_17") (TmVar "c_inp1_17"))))))))))))) */
const item sys_t_24 = {.thunk_item = {.code = sys_thunk_23, .env = NULL}};
sys_t_24.thunk_item.code(sys_t_24.thunk_item.env, ret);
top_e_sinEntry = (*ret);
/* TmReturn (TmThunk (TmLam (BTyped (Param {paramName = "e_x", paramType = TpConst TpCDouble}) (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 1))) (BUntyped "c_inp0_22" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 3))) (BUntyped "c_inp1_22" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_22") (TmVar "c_inp1_22")) (BUntyped "c_inp0_25" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 5))) (BUntyped "c_inp1_25" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_25") (TmVar "c_inp1_25")) (BUntyped "c_inp0_28" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 7))) (BUntyped "c_inp1_28" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_28") (TmVar "c_inp1_28")) (BUntyped "c_inp0_31" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 9))) (BUntyped "c_inp1_31" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_31") (TmVar "c_inp1_31")) (BUntyped "c_inp0_34" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 11))) (BUntyped "c_inp1_34" (TmTo (TmPrimBinOp PrimDSub (TmVar "c_inp0_34") (TmVar "c_inp1_34")) (BUntyped "c_inp0_37" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 13))) (BUntyped "c_inp1_37" (TmTo (TmPrimBinOp PrimDAdd (TmVar "c_inp0_37") (TmVar "c_inp1_37")) (BUntyped "c_inp0_40" (TmTo (TmApp (TmApp (TmForce (TmGlobal "e_sinEntry")) (TmVar "e_x")) (TmConst (TmCInt 15))) (BUntyped "c_inp1_40" (TmPrimBinOp PrimDSub (TmVar "c_inp0_40") (TmVar "c_inp1_40"))))))))))))))))))))))))))))))))) */
const item sys_t_42 = {.thunk_item = {.code = sys_thunk_41, .env = NULL}};
sys_t_42.thunk_item.code(sys_t_42.thunk_item.env, ret);
top_e_sin = (*ret);
/* TmTo (TmApp (TmForce (TmGlobal "e_sin")) (TmConst (TmCDouble 0.5))) (BUntyped "c_v_42" (TmPrintDouble (TmVar "c_v_42") (TmReturn (TmConst (TmCInt 0))))) */
const item sys_t_46 = {.thunk_item = {.code = sys_thunk_45, .env = NULL}};
sys_t_46.thunk_item.code(sys_t_46.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

