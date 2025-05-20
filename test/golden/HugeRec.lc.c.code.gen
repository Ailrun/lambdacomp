#include <runtime.h>

void sys_thunk_7(item *const env, item *const ret);
void sys_thunk_8(item *const env, item *const ret);
void sys_thunk_10(item *const env, item *const ret);
void sys_thunk_11(item *const env, item *const ret);
void sys_thunk_12(item *const env, item *const ret);
item top_u_recF;
void sys_thunk_16(item *const env, item *const ret);
item top_u_main;

void sys_thunk_7(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmTo (TmPrimBinOp PrimIGt (TmVar "u_n") (TmInt 0)) "c_c_3" (TmPrintInt (TmVar "u_x") (TmPrintInt (TmInt 2) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "u_n") (TmInt 1)) "c_a0_5" (TmTo (TmApp (TmForce (TmVar "u_recF")) (TmVar "c_a0_5")) "c_f1_5" (TmApp (TmForce (TmVar "c_f1_5")) (TmVar "u_x")))) (TmReturn (TmInt 0)))))) */
const item var_u_x = (global_stack.items[--global_stack.top]);
const item sys_arg0_0 = (env[0]);
const item sys_arg1_0 = {.int_item = 0};
(*ret).int_item = sys_arg0_0.int_item > sys_arg1_0.int_item;
const item var_c_c_3 = (*ret);
const item sys_msg_6 = var_u_x;
printf("%d\n", sys_msg_6.int_item);
const item sys_msg_5 = {.int_item = 2};
printf("%d\n", sys_msg_5.int_item);
const item sys_c_4 = var_c_c_3;
if (sys_c_4.int_item)
{
const item sys_arg0_1 = (env[0]);
const item sys_arg1_1 = {.int_item = 1};
(*ret).int_item = sys_arg0_1.int_item - sys_arg1_1.int_item;
const item var_c_a0_5 = (*ret);
(global_stack.items[global_stack.top++]) = var_c_a0_5;
const item sys_t_2 = (env[1]);
sys_t_2.thunk_item.code(sys_t_2.thunk_item.env, ret);
const item var_c_f1_5 = (*ret);
(global_stack.items[global_stack.top++]) = var_u_x;
const item sys_t_3 = var_c_f1_5;
sys_t_3.thunk_item.code(sys_t_3.thunk_item.env, ret);
}
else
{
(*ret).int_item = 0;
}

}

void sys_thunk_8(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "u_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmTo (TmPrimBinOp PrimIGt (TmVar "u_n") (TmInt 0)) "c_c_3" (TmPrintInt (TmVar "u_x") (TmPrintInt (TmInt 2) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "u_n") (TmInt 1)) "c_a0_5" (TmTo (TmApp (TmForce (TmVar "u_recF")) (TmVar "c_a0_5")) "c_f1_5" (TmApp (TmForce (TmVar "c_f1_5")) (TmVar "u_x")))) (TmReturn (TmInt 0))))))))) */
const item var_u_n = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_7;
(*ret).thunk_item.env = (item *) malloc(2 * sizeof(item));
((*ret).thunk_item.env[0]) = var_u_n;
((*ret).thunk_item.env[1]) = (env[0]);
}

void sys_thunk_10(item *const env, item *const ret)
{
/* TmLet "c_r_6" (TmThunk (TmLam (Param {paramName = "u_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmTo (TmPrimBinOp PrimIGt (TmVar "u_n") (TmInt 0)) "c_c_3" (TmPrintInt (TmVar "u_x") (TmPrintInt (TmInt 2) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "u_n") (TmInt 1)) "c_a0_5" (TmTo (TmApp (TmForce (TmVar "u_recF")) (TmVar "c_a0_5")) "c_f1_5" (TmApp (TmForce (TmVar "c_f1_5")) (TmVar "u_x")))) (TmReturn (TmInt 0))))))))))) (TmForce (TmVar "c_r_6")) */
const item var_c_r_6 = {.thunk_item = {.code = sys_thunk_8, .env = (item *) malloc(1 * sizeof(item))}};
(var_c_r_6.thunk_item.env[0]) = (env[0]);
const item sys_t_9 = var_c_r_6;
sys_t_9.thunk_item.code(sys_t_9.thunk_item.env, ret);
}

void sys_thunk_11(item *const _, item *const ret)
{
/* TmRec (Param {paramName = "u_recF", paramType = TpUp (TpInt :->: TpDown (TpUp (TpInt :->: TpDown TpInt)))}) (TmLet "c_r_6" (TmThunk (TmLam (Param {paramName = "u_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmTo (TmPrimBinOp PrimIGt (TmVar "u_n") (TmInt 0)) "c_c_3" (TmPrintInt (TmVar "u_x") (TmPrintInt (TmInt 2) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "u_n") (TmInt 1)) "c_a0_5" (TmTo (TmApp (TmForce (TmVar "u_recF")) (TmVar "c_a0_5")) "c_f1_5" (TmApp (TmForce (TmVar "c_f1_5")) (TmVar "u_x")))) (TmReturn (TmInt 0))))))))))) (TmForce (TmVar "c_r_6"))) */
const item var_u_recF = {.thunk_item = {.code = sys_thunk_10, .env = (item *) malloc(1 * sizeof(item))}};
(var_u_recF.thunk_item.env[0]) = var_u_recF;
var_u_recF.thunk_item.code(var_u_recF.thunk_item.env, ret);
}

void sys_thunk_12(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmRec (Param {paramName = "u_recF", paramType = TpUp (TpInt :->: TpDown (TpUp (TpInt :->: TpDown TpInt)))}) (TmLet "c_r_6" (TmThunk (TmLam (Param {paramName = "u_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmTo (TmPrimBinOp PrimIGt (TmVar "u_n") (TmInt 0)) "c_c_3" (TmPrintInt (TmVar "u_x") (TmPrintInt (TmInt 2) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "u_n") (TmInt 1)) "c_a0_5" (TmTo (TmApp (TmForce (TmVar "u_recF")) (TmVar "c_a0_5")) "c_f1_5" (TmApp (TmForce (TmVar "c_f1_5")) (TmVar "u_x")))) (TmReturn (TmInt 0))))))))))) (TmForce (TmVar "c_r_6"))))) */
(*ret).thunk_item.code = sys_thunk_11;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_16(item *const _, item *const ret)
{
/* TmTo (TmApp (TmForce (TmGlobal "u_recF")) (TmInt 1000000)) "c_f1_7" (TmApp (TmForce (TmVar "c_f1_7")) (TmInt 3)) */
(global_stack.items[global_stack.top++]).int_item = 1000000;
const item sys_t_14 = top_u_recF;
sys_t_14.thunk_item.code(sys_t_14.thunk_item.env, ret);
const item var_c_f1_7 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 3;
const item sys_t_15 = var_c_f1_7;
sys_t_15.thunk_item.code(sys_t_15.thunk_item.env, ret);
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmRec (Param {paramName = "u_recF", paramType = TpUp (TpInt :->: TpDown (TpUp (TpInt :->: TpDown TpInt)))}) (TmLet "c_r_6" (TmThunk (TmLam (Param {paramName = "u_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmTo (TmPrimBinOp PrimIGt (TmVar "u_n") (TmInt 0)) "c_c_3" (TmPrintInt (TmVar "u_x") (TmPrintInt (TmInt 2) (TmIf (TmVar "c_c_3") (TmTo (TmPrimBinOp PrimISub (TmVar "u_n") (TmInt 1)) "c_a0_5" (TmTo (TmApp (TmForce (TmVar "u_recF")) (TmVar "c_a0_5")) "c_f1_5" (TmApp (TmForce (TmVar "c_f1_5")) (TmVar "u_x")))) (TmReturn (TmInt 0))))))))))) (TmForce (TmVar "c_r_6"))))) */
const item sys_t_13 = {.thunk_item = {.code = sys_thunk_12, .env = NULL}};
sys_t_13.thunk_item.code(sys_t_13.thunk_item.env, ret);
top_u_recF = (*ret);
/* TmTo (TmApp (TmForce (TmGlobal "u_recF")) (TmInt 1000000)) "c_f1_7" (TmApp (TmForce (TmVar "c_f1_7")) (TmInt 3)) */
const item sys_t_17 = {.thunk_item = {.code = sys_thunk_16, .env = NULL}};
sys_t_17.thunk_item.code(sys_t_17.thunk_item.env, ret);
top_u_main = (*ret);
}
return top_u_main.int_item;
}

