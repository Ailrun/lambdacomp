#include <runtime.h>

void sys_thunk_3(item *const env, item *const ret);
item top_e_main;

void sys_thunk_3(item *const _, item *const ret)
{
/* TmPrintInt (TmConst (TmCInt 7)) (TmPrintInt (TmConst (TmCInt 2)) (TmPrintInt (TmConst (TmCInt 3)) (TmReturn (TmConst (TmCInt 0))))) */
const item sys_msg_2 = {.int_item = 7};
printf("%d\n", sys_msg_2.int_item);
const item sys_msg_1 = {.int_item = 2};
printf("%d\n", sys_msg_1.int_item);
const item sys_msg_0 = {.int_item = 3};
printf("%d\n", sys_msg_0.int_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmPrintInt (TmConst (TmCInt 7)) (TmPrintInt (TmConst (TmCInt 2)) (TmPrintInt (TmConst (TmCInt 3)) (TmReturn (TmConst (TmCInt 0))))) */
const item sys_t_4 = {.thunk_item = {.code = sys_thunk_3, .env = NULL}};
sys_t_4.thunk_item.code(sys_t_4.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

