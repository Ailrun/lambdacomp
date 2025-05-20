#include <runtime.h>

void sys_thunk_10(item *const env, item *const ret);
void sys_thunk_11(item *const env, item *const ret);
void sys_thunk_12(item *const env, item *const ret);
void sys_thunk_13(item *const env, item *const ret);
void sys_thunk_15(item *const env, item *const ret);
void sys_thunk_16(item *const env, item *const ret);
void sys_thunk_17(item *const env, item *const ret);
item top_u_recFib;
void sys_thunk_23(item *const env, item *const ret);
void sys_thunk_24(item *const env, item *const ret);
item top_u_fib;
void sys_thunk_27(item *const env, item *const ret);
item top_u_main;

void sys_thunk_10(item *const env, item *const ret)
{
/* TmTo (TmPrimBinOp PrimILt (TmVar "u_n") (TmVar "u_l")) "c_c_1" (TmLam (Param {paramName = "u_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmVar "u_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "u_recFib")) (TmVar "u_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "u_y")) "c_f3_6" (TmPrintInt (TmVar "u_l") (TmPrintInt (TmVar "u_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6"))))))))))) */
const item sys_arg0_0 = (env[1]);
const item sys_arg1_0 = (env[0]);
(*ret).int_item = sys_arg0_0.int_item < sys_arg1_0.int_item;
const item var_c_c_1 = (*ret);
const item var_u_y = (global_stack.items[--global_stack.top]);
const item sys_c_9 = var_c_c_1;
if (sys_c_9.int_item)
{
(*ret).int_item = 0;
}
else
{
const item sys_arg0_1 = (env[0]);
const item sys_arg1_1 = {.int_item = 1};
(*ret).int_item = sys_arg0_1.int_item + sys_arg1_1.int_item;
const item var_c_a1_6 = (*ret);
const item sys_arg0_2 = (env[3]);
const item sys_arg1_2 = var_u_y;
(*ret).int_item = sys_arg0_2.int_item + sys_arg1_2.int_item;
const item var_c_a3_6 = (*ret);
(global_stack.items[global_stack.top++]) = (env[1]);
const item sys_t_3 = (env[2]);
sys_t_3.thunk_item.code(sys_t_3.thunk_item.env, ret);
const item var_c_f1_6 = (*ret);
(global_stack.items[global_stack.top++]) = var_c_a1_6;
const item sys_t_4 = var_c_f1_6;
sys_t_4.thunk_item.code(sys_t_4.thunk_item.env, ret);
const item var_c_f2_6 = (*ret);
(global_stack.items[global_stack.top++]) = var_u_y;
const item sys_t_5 = var_c_f2_6;
sys_t_5.thunk_item.code(sys_t_5.thunk_item.env, ret);
const item var_c_f3_6 = (*ret);
const item sys_msg_8 = (env[0]);
printf("%d\n", sys_msg_8.int_item);
const item sys_msg_7 = (env[3]);
printf("%d\n", sys_msg_7.int_item);
(global_stack.items[global_stack.top++]) = var_c_a3_6;
const item sys_t_6 = var_c_f3_6;
sys_t_6.thunk_item.code(sys_t_6.thunk_item.env, ret);
}

}

void sys_thunk_11(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimILt (TmVar "u_n") (TmVar "u_l")) "c_c_1" (TmLam (Param {paramName = "u_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmVar "u_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "u_recFib")) (TmVar "u_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "u_y")) "c_f3_6" (TmPrintInt (TmVar "u_l") (TmPrintInt (TmVar "u_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6")))))))))))))) */
const item var_u_x = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_10;
(*ret).thunk_item.env = (item *) malloc(4 * sizeof(item));
((*ret).thunk_item.env[0]) = (env[0]);
((*ret).thunk_item.env[1]) = (env[1]);
((*ret).thunk_item.env[2]) = (env[2]);
((*ret).thunk_item.env[3]) = var_u_x;
}

void sys_thunk_12(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "u_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimILt (TmVar "u_n") (TmVar "u_l")) "c_c_1" (TmLam (Param {paramName = "u_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmVar "u_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "u_recFib")) (TmVar "u_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "u_y")) "c_f3_6" (TmPrintInt (TmVar "u_l") (TmPrintInt (TmVar "u_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6"))))))))))))))))) */
const item var_u_l = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_11;
(*ret).thunk_item.env = (item *) malloc(3 * sizeof(item));
((*ret).thunk_item.env[0]) = var_u_l;
((*ret).thunk_item.env[1]) = (env[0]);
((*ret).thunk_item.env[2]) = (env[1]);
}

void sys_thunk_13(item *const env, item *const ret)
{
/* TmLam (Param {paramName = "u_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimILt (TmVar "u_n") (TmVar "u_l")) "c_c_1" (TmLam (Param {paramName = "u_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmVar "u_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "u_recFib")) (TmVar "u_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "u_y")) "c_f3_6" (TmPrintInt (TmVar "u_l") (TmPrintInt (TmVar "u_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6")))))))))))))))))))) */
const item var_u_n = (global_stack.items[--global_stack.top]);
(*ret).thunk_item.code = sys_thunk_12;
(*ret).thunk_item.env = (item *) malloc(2 * sizeof(item));
((*ret).thunk_item.env[0]) = var_u_n;
((*ret).thunk_item.env[1]) = (env[0]);
}

void sys_thunk_15(item *const env, item *const ret)
{
/* TmLet "c_r_7" (TmThunk (TmLam (Param {paramName = "u_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimILt (TmVar "u_n") (TmVar "u_l")) "c_c_1" (TmLam (Param {paramName = "u_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmVar "u_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "u_recFib")) (TmVar "u_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "u_y")) "c_f3_6" (TmPrintInt (TmVar "u_l") (TmPrintInt (TmVar "u_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6")))))))))))))))))))))) (TmForce (TmVar "c_r_7")) */
const item var_c_r_7 = {.thunk_item = {.code = sys_thunk_13, .env = (item *) malloc(1 * sizeof(item))}};
(var_c_r_7.thunk_item.env[0]) = (env[0]);
const item sys_t_14 = var_c_r_7;
sys_t_14.thunk_item.code(sys_t_14.thunk_item.env, ret);
}

void sys_thunk_16(item *const _, item *const ret)
{
/* TmRec (Param {paramName = "u_recFib", paramType = TpUp (TpInt :->: TpDown (TpUp (TpInt :->: TpDown (TpUp (TpInt :->: TpDown (TpUp (TpInt :->: TpDown TpInt)))))))}) (TmLet "c_r_7" (TmThunk (TmLam (Param {paramName = "u_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimILt (TmVar "u_n") (TmVar "u_l")) "c_c_1" (TmLam (Param {paramName = "u_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmVar "u_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "u_recFib")) (TmVar "u_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "u_y")) "c_f3_6" (TmPrintInt (TmVar "u_l") (TmPrintInt (TmVar "u_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6")))))))))))))))))))))) (TmForce (TmVar "c_r_7"))) */
const item var_u_recFib = {.thunk_item = {.code = sys_thunk_15, .env = (item *) malloc(1 * sizeof(item))}};
(var_u_recFib.thunk_item.env[0]) = var_u_recFib;
var_u_recFib.thunk_item.code(var_u_recFib.thunk_item.env, ret);
}

void sys_thunk_17(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmRec (Param {paramName = "u_recFib", paramType = TpUp (TpInt :->: TpDown (TpUp (TpInt :->: TpDown (TpUp (TpInt :->: TpDown (TpUp (TpInt :->: TpDown TpInt)))))))}) (TmLet "c_r_7" (TmThunk (TmLam (Param {paramName = "u_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimILt (TmVar "u_n") (TmVar "u_l")) "c_c_1" (TmLam (Param {paramName = "u_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmVar "u_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "u_recFib")) (TmVar "u_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "u_y")) "c_f3_6" (TmPrintInt (TmVar "u_l") (TmPrintInt (TmVar "u_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6")))))))))))))))))))))) (TmForce (TmVar "c_r_7"))))) */
(*ret).thunk_item.code = sys_thunk_16;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_23(item *const _, item *const ret)
{
/* TmLam (Param {paramName = "u_n", paramType = TpInt}) (TmTo (TmApp (TmForce (TmGlobal "u_recFib")) (TmVar "u_n")) "c_f1_8" (TmTo (TmApp (TmForce (TmVar "c_f1_8")) (TmInt 0)) "c_f2_8" (TmTo (TmApp (TmForce (TmVar "c_f2_8")) (TmInt 0)) "c_f3_8" (TmApp (TmForce (TmVar "c_f3_8")) (TmInt 1))))) */
const item var_u_n = (global_stack.items[--global_stack.top]);
(global_stack.items[global_stack.top++]) = var_u_n;
const item sys_t_19 = top_u_recFib;
sys_t_19.thunk_item.code(sys_t_19.thunk_item.env, ret);
const item var_c_f1_8 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 0;
const item sys_t_20 = var_c_f1_8;
sys_t_20.thunk_item.code(sys_t_20.thunk_item.env, ret);
const item var_c_f2_8 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 0;
const item sys_t_21 = var_c_f2_8;
sys_t_21.thunk_item.code(sys_t_21.thunk_item.env, ret);
const item var_c_f3_8 = (*ret);
(global_stack.items[global_stack.top++]).int_item = 1;
const item sys_t_22 = var_c_f3_8;
sys_t_22.thunk_item.code(sys_t_22.thunk_item.env, ret);
}

void sys_thunk_24(item *const _, item *const ret)
{
/* TmReturn (TmThunk (TmLam (Param {paramName = "u_n", paramType = TpInt}) (TmTo (TmApp (TmForce (TmGlobal "u_recFib")) (TmVar "u_n")) "c_f1_8" (TmTo (TmApp (TmForce (TmVar "c_f1_8")) (TmInt 0)) "c_f2_8" (TmTo (TmApp (TmForce (TmVar "c_f2_8")) (TmInt 0)) "c_f3_8" (TmApp (TmForce (TmVar "c_f3_8")) (TmInt 1))))))) */
(*ret).thunk_item.code = sys_thunk_23;
(*ret).thunk_item.env = NULL;
}


void sys_thunk_27(item *const _, item *const ret)
{
/* TmApp (TmForce (TmGlobal "u_fib")) (TmInt 10) */
(global_stack.items[global_stack.top++]).int_item = 10;
const item sys_t_26 = top_u_fib;
sys_t_26.thunk_item.code(sys_t_26.thunk_item.env, ret);
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmReturn (TmThunk (TmRec (Param {paramName = "u_recFib", paramType = TpUp (TpInt :->: TpDown (TpUp (TpInt :->: TpDown (TpUp (TpInt :->: TpDown (TpUp (TpInt :->: TpDown TpInt)))))))}) (TmLet "c_r_7" (TmThunk (TmLam (Param {paramName = "u_n", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_l", paramType = TpInt}) (TmReturn (TmThunk (TmLam (Param {paramName = "u_x", paramType = TpInt}) (TmReturn (TmThunk (TmTo (TmPrimBinOp PrimILt (TmVar "u_n") (TmVar "u_l")) "c_c_1" (TmLam (Param {paramName = "u_y", paramType = TpInt}) (TmIf (TmVar "c_c_1") (TmReturn (TmInt 0)) (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_l") (TmInt 1)) "c_a1_6" (TmTo (TmPrimBinOp PrimIAdd (TmVar "u_x") (TmVar "u_y")) "c_a3_6" (TmTo (TmApp (TmForce (TmVar "u_recFib")) (TmVar "u_n")) "c_f1_6" (TmTo (TmApp (TmForce (TmVar "c_f1_6")) (TmVar "c_a1_6")) "c_f2_6" (TmTo (TmApp (TmForce (TmVar "c_f2_6")) (TmVar "u_y")) "c_f3_6" (TmPrintInt (TmVar "u_l") (TmPrintInt (TmVar "u_x") (TmApp (TmForce (TmVar "c_f3_6")) (TmVar "c_a3_6")))))))))))))))))))))) (TmForce (TmVar "c_r_7"))))) */
const item sys_t_18 = {.thunk_item = {.code = sys_thunk_17, .env = NULL}};
sys_t_18.thunk_item.code(sys_t_18.thunk_item.env, ret);
top_u_recFib = (*ret);
/* TmReturn (TmThunk (TmLam (Param {paramName = "u_n", paramType = TpInt}) (TmTo (TmApp (TmForce (TmGlobal "u_recFib")) (TmVar "u_n")) "c_f1_8" (TmTo (TmApp (TmForce (TmVar "c_f1_8")) (TmInt 0)) "c_f2_8" (TmTo (TmApp (TmForce (TmVar "c_f2_8")) (TmInt 0)) "c_f3_8" (TmApp (TmForce (TmVar "c_f3_8")) (TmInt 1))))))) */
const item sys_t_25 = {.thunk_item = {.code = sys_thunk_24, .env = NULL}};
sys_t_25.thunk_item.code(sys_t_25.thunk_item.env, ret);
top_u_fib = (*ret);
/* TmApp (TmForce (TmGlobal "u_fib")) (TmInt 10) */
const item sys_t_28 = {.thunk_item = {.code = sys_thunk_27, .env = NULL}};
sys_t_28.thunk_item.code(sys_t_28.thunk_item.env, ret);
top_u_main = (*ret);
}
return top_u_main.int_item;
}

