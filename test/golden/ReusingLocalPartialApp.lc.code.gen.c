#include <runtime.h>

void sys_thunk_2(item *const env, item *const ret);
void sys_thunk_3(item *const env, item *const ret);
void sys_thunk_4(item *const env, item *const ret);
void sys_thunk_5(item *const env, item *const ret);
item top_e_f;
void sys_thunk_14(item *const env, item *const ret);
item top_e_main;

void sys_thunk_2(item *const env, item *const ret)
{
/* TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_inp0_1" (TmLam (Param {paramName = "e_z", paramType = TpConst TpCInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "e_z"))) */
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
/* TmLam (Param {paramName = "e_y", paramType = TpConst TpCInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_inp0_1" (TmLam (Param {paramName = "e_z", paramType = TpConst TpCInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "e_z")))))) */
const item var_e_y = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_2;
(*ret).thunk_item.env = (item *) malloc(2 * sizeof(item));
((*ret).thunk_item.env[0]) = (env[0]);
((*ret).thunk_item.env[1]) = var_e_y;
}

void sys_thunk_4(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_y", paramType = TpConst TpCInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_inp0_1" (TmLam (Param {paramName = "e_z", paramType = TpConst TpCInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "e_z"))))))))) */
const item var_e_x = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_3;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_e_x;
}

void sys_thunk_5(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_y", paramType = TpConst TpCInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_inp0_1" (TmLam (Param {paramName = "e_z", paramType = TpConst TpCInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "e_z"))))))))))) */
(*ret).thunk_item.code = sys_thunk_4;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_14(item *const _, item *const ret)
{
/* TmTo (TmApp (TmForce (TmGlobal "e_f")) (TmConst (TmCInt 3))) "c_a0_7" (TmTo (TmApp (TmForce (TmVar "c_a0_7")) (TmConst (TmCInt 2))) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmConst (TmCInt 5))) "c_v_4" (TmTo (TmApp (TmForce (TmVar "c_a0_7")) (TmConst (TmCInt 4))) "c_f1_5" (TmTo (TmApp (TmForce (TmVar "c_f1_5")) (TmConst (TmCInt 1))) "c_v_6" (TmPrintInt (TmVar "c_v_4") (TmPrintInt (TmVar "c_v_6") (TmReturn (TmConst (TmCInt 0))))))))) */
(global_stack.items[global_stack.top++]).int_item = 3;
const item sys_t_7 = top_e_f;
sys_t_7.thunk_item.code(sys_t_7.thunk_item.env, ret);
const item var_c_a0_7 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 2;
const item sys_t_8 = var_c_a0_7;
sys_t_8.thunk_item.code(sys_t_8.thunk_item.env, ret);
const item var_c_f1_3 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 5;
const item sys_t_9 = var_c_f1_3;
sys_t_9.thunk_item.code(sys_t_9.thunk_item.env, ret);
const item var_c_v_4 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 4;
const item sys_t_10 = var_c_a0_7;
sys_t_10.thunk_item.code(sys_t_10.thunk_item.env, ret);
const item var_c_f1_5 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 1;
const item sys_t_11 = var_c_f1_5;
sys_t_11.thunk_item.code(sys_t_11.thunk_item.env, ret);
const item var_c_v_6 = (*ret);
const item sys_msg_13 = var_c_v_4;
printf("%d\n", sys_msg_13.int_item);
const item sys_msg_12 = var_c_v_6;
printf("%d\n", sys_msg_12.int_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmLam (Param {paramName = "e_x", paramType = TpConst TpCInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "e_y", paramType = TpConst TpCInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimIAdd (TmVar "e_x") (TmVar "e_y")) "c_inp0_1" (TmLam (Param {paramName = "e_z", paramType = TpConst TpCInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "e_z"))))))))))) */
const item sys_t_6 = {.thunk_item = {.code = sys_thunk_5, .env = NULL}};
sys_t_6.thunk_item.code(sys_t_6.thunk_item.env, ret);
top_e_f = (*ret);
/* TmTo (TmApp (TmForce (TmGlobal "e_f")) (TmConst (TmCInt 3))) "c_a0_7" (TmTo (TmApp (TmForce (TmVar "c_a0_7")) (TmConst (TmCInt 2))) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmConst (TmCInt 5))) "c_v_4" (TmTo (TmApp (TmForce (TmVar "c_a0_7")) (TmConst (TmCInt 4))) "c_f1_5" (TmTo (TmApp (TmForce (TmVar "c_f1_5")) (TmConst (TmCInt 1))) "c_v_6" (TmPrintInt (TmVar "c_v_4") (TmPrintInt (TmVar "c_v_6") (TmReturn (TmConst (TmCInt 0))))))))) */
const item sys_t_15 = {.thunk_item = {.code = sys_thunk_14, .env = NULL}};
sys_t_15.thunk_item.code(sys_t_15.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

