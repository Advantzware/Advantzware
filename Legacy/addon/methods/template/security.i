/* security.i */

DEFINE OUTPUT PARAMETER op-flag AS LOGICAL NO-UNDO.

op-flag = NO.
DO num-groups = 1 TO NUM-ENTRIES(g_groups):
    IF NOT CAN-DO(TRIM(b-prgrms.can_{&ACCESSTYPE}),ENTRY(num-groups,g_groups)) THEN
        NEXT.
    op-flag = YES.
    LEAVE.
END.
IF NOT CAN-DO(TRIM(b-prgrms.can_{&ACCESSTYPE}),USERID("ASI")) AND NOT op-flag THEN
DO:
    MESSAGE "Security Access for this Function Denied!"
        VIEW-AS ALERT-BOX INFORMATION.
    op-flag = NO.
END.
ELSE
    op-flag = YES.
