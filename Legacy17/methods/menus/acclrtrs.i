/* acclrtrs.i */

FIND acclrtrs WHERE acclrtrs.acclrtr = "{1}" NO-LOCK NO-ERROR.
IF AVAILABLE acclrtrs THEN
MENU-ITEM m_{1}:ACCELERATOR = acclrtrs.func_key.
