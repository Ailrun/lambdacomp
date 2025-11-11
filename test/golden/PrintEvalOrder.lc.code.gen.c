#include <runtime.h>

void sys_thunk_3(item *const env, item *const ret);
item top_e_main;

void sys_thunk_3(item *const _, item *const ret)
{
/* TmTo (TmPrintInt (TmConst (TmCInt 7)) (TmReturn (TmConst (TmCInt 3)))) "c_a_3" (TmTo (TmPrintInt (TmConst (TmCInt 2)) (TmReturn (TmVar "c_a_3"))) "c_v_4" (TmPrintInt (TmVar "c_v_4") (TmReturn (TmConst (TmCInt 0))))) */
const item sys_msg_0 = {.int_item = 7};
printf("%d\n", sys_msg_0.int_item);
(*ret).int_item = 3;
const item var_c_a_3 = (*ret);
const item sys_msg_1 = {.int_item = 2};
printf("%d\n", sys_msg_1.int_item);
(*ret) = var_c_a_3;
const item var_c_v_4 = (*ret);
const item sys_msg_2 = var_c_v_4;
printf("%d\n", sys_msg_2.int_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmTo (TmPrintInt (TmConst (TmCInt 7)) (TmReturn (TmConst (TmCInt 3)))) "c_a_3" (TmTo (TmPrintInt (TmConst (TmCInt 2)) (TmReturn (TmVar "c_a_3"))) "c_v_4" (TmPrintInt (TmVar "c_v_4") (TmReturn (TmConst (TmCInt 0))))) */
const item sys_t_4 = {.thunk_item = {.code = sys_thunk_3, .env = NULL}};
sys_t_4.thunk_item.code(sys_t_4.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

