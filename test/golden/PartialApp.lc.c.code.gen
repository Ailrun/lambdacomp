#include <runtime.h>

void sys_thunk_2(item *const env, item *const ret);
void sys_thunk_3(item *const env, item *const ret);
void sys_thunk_4(item *const env, item *const ret);
void sys_thunk_5(item *const env, item *const ret);
item top_u_f;
void sys_thunk_8(item *const env, item *const ret);
item top_u_g;
void sys_thunk_13(item *const env, item *const ret);
item top_u_main;

void sys_thunk_2(item *const env, item *const ret)
{
/* TmTo (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmVar "u_y")) "c_inp0_1" (TmLam (Param {paramName = "u_z", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "u_z"))) */
const item sys_arg0_0 = (env[0]);
const item sys_arg1_0 = (env[1]);
(*ret).int_item = sys_arg0_0.int_item + sys_arg1_0.int_item;
const item var_c_inp0_1 = (*ret);
const item var_u_z = (global_stack.items[--global_stack.top]);
const item sys_arg0_1 = var_c_inp0_1;
const item sys_arg1_1 = var_u_z;
(*ret).int_item = sys_arg0_1.int_item + sys_arg1_1.int_item;
}

void sys_thunk_3(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "u_y", paramType = TpInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmVar "u_y")) "c_inp0_1" (TmLam (Param {paramName = "u_z", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "u_z")))))) */
const item var_u_y = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_2;
(*ret).thunk_item.env = (item *) malloc(2 * sizeof(item));
((*ret).thunk_item.env[0]) = (env[0]);
((*ret).thunk_item.env[1]) = var_u_y;
}

void sys_thunk_4(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_y", paramType = TpInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmVar "u_y")) "c_inp0_1" (TmLam (Param {paramName = "u_z", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "u_z"))))))))) */
const item var_u_x = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_3;
(*ret).thunk_item.env = (item *) malloc(1 * sizeof(item));
((*ret).thunk_item.env[0]) = var_u_x;
}

void sys_thunk_5(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_y", paramType = TpInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmVar "u_y")) "c_inp0_1" (TmLam (Param {paramName = "u_z", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "u_z"))))))))))) */
(*ret).thunk_item.code = sys_thunk_4;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_8(item *const _, item *const ret)
{
/* TmApp (TmForce (TmGlobal "u_f")) (TmInt 3) */
(global_stack.items[global_stack.top++]).int_item = 3;
const item sys_t_7 = top_u_f;
sys_t_7.thunk_item.code(sys_t_7.thunk_item.env, ret);
}


void sys_thunk_13(item *const _, item *const ret)
{
/* TmTo (TmApp (TmForce (TmGlobal "u_g")) (TmInt 2)) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmInt 5)) "c_v_4" (TmPrintInt (TmVar "c_v_4") (TmReturn (TmInt 0)))) */
(global_stack.items[global_stack.top++]).int_item = 2;
const item sys_t_10 = top_u_g;
sys_t_10.thunk_item.code(sys_t_10.thunk_item.env, ret);
const item var_c_f1_3 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 5;
const item sys_t_11 = var_c_f1_3;
sys_t_11.thunk_item.code(sys_t_11.thunk_item.env, ret);
const item var_c_v_4 = (*ret);
const item sys_msg_12 = var_c_v_4;
printf("%d\n", sys_msg_12.int_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_y", paramType = TpInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmVar "u_y")) "c_inp0_1" (TmLam (Param {paramName = "u_z", paramType = TpInt}) (TmPrimBinOp PrimIAdd (TmVar "c_inp0_1") (TmVar "u_z"))))))))))) */
const item sys_t_6 = {.thunk_item = {.code = sys_thunk_5, .env = NULL}};
sys_t_6.thunk_item.code(sys_t_6.thunk_item.env, ret);
top_u_f = (*ret);
/* TmApp (TmForce (TmGlobal "u_f")) (TmInt 3) */
const item sys_t_9 = {.thunk_item = {.code = sys_thunk_8, .env = NULL}};
sys_t_9.thunk_item.code(sys_t_9.thunk_item.env, ret);
top_u_g = (*ret);
/* TmTo (TmApp (TmForce (TmGlobal "u_g")) (TmInt 2)) "c_f1_3" (TmTo (TmApp (TmForce (TmVar "c_f1_3")) (TmInt 5)) "c_v_4" (TmPrintInt (TmVar "c_v_4") (TmReturn (TmInt 0)))) */
const item sys_t_14 = {.thunk_item = {.code = sys_thunk_13, .env = NULL}};
sys_t_14.thunk_item.code(sys_t_14.thunk_item.env, ret);
top_u_main = (*ret);
}
return top_u_main.int_item;
}

