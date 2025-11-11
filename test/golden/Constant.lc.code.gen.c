#include <runtime.h>

void sys_thunk_1(item *const env, item *const ret);
item top_e_main;

void sys_thunk_1(item *const _, item *const ret)
{
/* TmPrintInt (TmConst (TmCInt 5)) (TmReturn (TmConst (TmCInt 0))) */
const item sys_msg_0 = {.int_item = 5};
printf("%d\n", sys_msg_0.int_item);
(*ret).int_item = 0;
}


int main(void)
{
item retv;
{
item *const ret = &retv;
/* TmPrintInt (TmConst (TmCInt 5)) (TmReturn (TmConst (TmCInt 0))) */
const item sys_t_2 = {.thunk_item = {.code = sys_thunk_1, .env = NULL}};
sys_t_2.thunk_item.code(sys_t_2.thunk_item.env, ret);
top_e_main = (*ret);
}
return top_e_main.int_item;
}

