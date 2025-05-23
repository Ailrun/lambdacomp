#include <runtime.h>

void sys_thunk_2(item *const env, item *const ret);
void sys_thunk_3(item *const env, item *const ret);
void sys_thunk_4(item *const env, item *const ret);
void sys_thunk_8(item *const env, item *const ret);
void sys_thunk_9(item *const env, item *const ret);
item top_e_f;
void sys_thunk_12(item *const env, item *const ret);
void sys_thunk_13(item *const env, item *const ret);
void sys_thunk_14(item *const env, item *const ret);
void sys_thunk_18(item *const env, item *const ret);
void sys_thunk_19(item *const env, item *const ret);
item top_e_g;
void sys_thunk_22(item *const env, item *const ret);
void sys_thunk_23(item *const env, item *const ret);
void sys_thunk_27(item *const env, item *const ret);
item top_e_main;

void sys_thunk_2(item *const env, item *const ret)
{
/* TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_inp0_1" (TmLam (Param {paramName = "e_z", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "e_z"))) */
const item sys_arg0_0 = (env[0]);
const item sys_arg1_0 = (env[1]);
(*ret).int_item = sys_arg0_0.int_item + sys_arg1_0.int_item;
const item var_c_inp0_1 = (*ret);
const item var_e_z = (global_stack.items[--global_stack.top]);
const item sys_arg0_1 = var_c_inp0_1;
const item sys_arg1_1 = var_e_z;
(*ret).int_item = sys_arg0_1.int_item + sys_arg1_1.int_item;
}

void sys_thunk_3(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_inp0_1" (TmLam (Param {paramName = "e_z", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "e_z")))))) */
const item var_e_y = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_2;
(*ret).thunk_item.env = (item *) malloc(2 * sizeof(item));
((*ret).thunk_item.env[0]) = (env[0]);
((*ret).thunk_item.env[1]) = var_e_y;
}

void sys_thunk_4(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_inp0_1" (TmLam (Param {paramName = "e_z", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "e_z"))))))))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_3;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_e_x;
}

void sys_thunk_8(item *const _, item *const ret)
{
/* TmLet "aa_0_0_temp" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_inp0_1" (TmLam (Param {paramName = "e_z", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "e_z"))))))))))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0")) "aa_2_0_temp" (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_2_0_temp")) (TmVar "aa_2_0"))))))) */
const item var_aa_0_0_temp = {.thunk_item = {.code = sys_thunk_4, .env = NULL}};
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_0;
const item sys_t_5 = var_aa_0_0_temp;
sys_t_5.thunk_item.code(sys_t_5.thunk_item.env, ret);
const item var_aa_1_0_temp = (*ret);
const item var_aa_1_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_0;
const item sys_t_6 = var_aa_1_0_temp;
sys_t_6.thunk_item.code(sys_t_6.thunk_item.env, ret);
const item var_aa_2_0_temp = (*ret);
const item var_aa_2_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_2_0;
const item sys_t_7 = var_aa_2_0_temp;
sys_t_7.thunk_item.code(sys_t_7.thunk_item.env, ret);
}

void sys_thunk_9(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLet "aa_0_0_temp" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_inp0_1" (TmLam (Param {paramName = "e_z", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "e_z"))))))))))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0")) "aa_2_0_temp" (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_2_0_temp")) (TmVar "aa_2_0"))))))))) */
(*ret).thunk_item.code = sys_thunk_8;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_12(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_f")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")) */
const item var_aa_2_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_2_0;
(global_stack.items[global_stack.top++]) = (env[1]);
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_11 = top_e_f;
sys_t_11.thunk_item.code(sys_t_11.thunk_item.env, ret);
}

void sys_thunk_13(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_f")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0"))))) */
const item var_aa_1_0 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_12;
(*ret).thunk_item.env = (item *) malloc(2 * sizeof(item));
((*ret).thunk_item.env[0]) = (env[0]);
((*ret).thunk_item.env[1]) = var_aa_1_0;
}

void sys_thunk_14(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_f")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")))))))) */
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_13;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_0;
}

void sys_thunk_18(item *const _, item *const ret)
{
/* TmLet "c_f0_2" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_f")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")))))))))) (TmTo (TmApp (TmForce (TmVar "c_f0_2")) (TmInt 3)) "aa_0_0_temp" (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0")))))) */
const item var_c_f0_2 = {.thunk_item = {.code = sys_thunk_14, .env = NULL}};
(global_stack.items[global_stack.top++]).int_item = 3;
const item sys_t_15 = var_c_f0_2;
sys_t_15.thunk_item.code(sys_t_15.thunk_item.env, ret);
const item var_aa_0_0_temp = (*ret);
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_0_0;
const item sys_t_16 = var_aa_0_0_temp;
sys_t_16.thunk_item.code(sys_t_16.thunk_item.env, ret);
const item var_aa_1_0_temp = (*ret);
const item var_aa_1_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_0;
const item sys_t_17 = var_aa_1_0_temp;
sys_t_17.thunk_item.code(sys_t_17.thunk_item.env, ret);
}

void sys_thunk_19(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLet "c_f0_2" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_f")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")))))))))) (TmTo (TmApp (TmForce (TmVar "c_f0_2")) (TmInt 3)) "aa_0_0_temp" (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0")))))))) */
(*ret).thunk_item.code = sys_thunk_18;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_22(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_g")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) */
const item var_aa_1_0 = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_aa_1_0;
(global_stack.items[global_stack.top++]) = (env[0]);
const item sys_t_21 = top_e_g;
sys_t_21.thunk_item.code(sys_t_21.thunk_item.env, ret);
}

void sys_thunk_23(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_g")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))) */
const item var_aa_0_0 = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_22;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_aa_0_0;
}

void sys_thunk_27(item *const _, item *const ret)
{
/* TmLet "c_f0_3" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_g")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmApp (TmForce (TmVar "c_f0_3")) (TmInt 2)) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmInt 5)) "c_v_4" (TmPrintInt (TmVar "c_v_4") (TmReturn (TmInt 0))))) */
const item var_c_f0_3 = {.thunk_item = {.code = sys_thunk_23, .env = NULL}};
(global_stack.items[global_stack.top++]).int_item = 2;
const item sys_t_24 = var_c_f0_3;
sys_t_24.thunk_item.code(sys_t_24.thunk_item.env, ret);
const item var_c_f1_3 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 5;
const item sys_t_25 = var_c_f1_3;
sys_t_25.thunk_item.code(sys_t_25.thunk_item.env, ret);
const item var_c_v_4 = (*ret);
const item sys_msg_26 = var_c_v_4;
printf("%d\n", sys_msg_26.int_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmLet "aa_0_0_temp" (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_y", paramType = TpInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_inp0_1" (TmLam (Param {paramName = "e_z", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "e_z"))))))))))) (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0")) "aa_2_0_temp" (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_2_0_temp")) (TmVar "aa_2_0"))))))))) */
const item sys_t_10 = {.thunk_item = {.code = sys_thunk_9, .env = NULL}};
sys_t_10.thunk_item.code(sys_t_10.thunk_item.env, ret);
top_e_f = (*ret);
/* TmReturn (TmThunk (TmLet "c_f0_2" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_2_0", paramType = TpInt}) (TmApp (TmApp (TmApp (TmForce (TmGlobal "e_f")) (TmVar "aa_0_0")) (TmVar "aa_1_0")) (TmVar "aa_2_0")))))))))) (TmTo (TmApp (TmForce (TmVar "c_f0_2")) (TmInt 3)) "aa_0_0_temp" (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmTo (TmApp (TmForce (TmVar "aa_0_0_temp")) (TmVar "aa_0_0")) "aa_1_0_temp" (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmForce (TmVar "aa_1_0_temp")) (TmVar "aa_1_0")))))))) */
const item sys_t_20 = {.thunk_item = {.code = sys_thunk_19, .env = NULL}};
sys_t_20.thunk_item.code(sys_t_20.thunk_item.env, ret);
top_e_g = (*ret);
/* TmLet "c_f0_3" (TmThunk (TmLam (Param {paramName = "aa_0_0", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "aa_1_0", paramType = TpInt}) (TmApp (TmApp (TmForce (TmGlobal "e_g")) (TmVar "aa_0_0")) (TmVar "aa_1_0"))))))) (TmTo (TmApp (TmForce (TmVar "c_f0_3")) (TmInt 2)) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmInt 5)) "c_v_4" (TmPrintInt (TmVar "c_v_4") (TmReturn (TmInt 0))))) */
const item sys_t_28 = {.thunk_item = {.code = sys_thunk_27, .env = NULL}};
sys_t_28.thunk_item.code(sys_t_28.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

