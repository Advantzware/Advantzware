/* security.i */

DEFINE OUTPUT PARAMETER op-flag AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

op-flag = NO.
DO i = 1 TO NUM-ENTRIES(g_groups):         /*entry(num-groups,g_groups) : gets error */
    IF NOT CAN-DO(TRIM(b-prgrms.can_{&ACCESSTYPE}),ENTRY(i,g_groups)) THEN
        NEXT.
    op-flag = YES.
    LEAVE.
END.
IF NOT CAN-DO(TRIM(b-prgrms.can_{&ACCESSTYPE}),USERID("ASI")) AND NOT op-flag THEN
DO:
    op-flag = NO.
END.
ELSE
    op-flag = YES.
