/* security.i */

DEFINE OUTPUT PARAMETER op-flag AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

op-flag = no.
DO i = 1 TO NUM-ENTRIES(g_groups):         /*entry(num-groups,g_groups) : gets error */
  IF NOT CAN-DO(b-prgrms.can_{&ACCESSTYPE},ENTRY(i,g_groups)) THEN
  NEXT.
  op-flag = yes.
  LEAVE.
END.
IF NOT CAN-DO(b-prgrms.can_{&ACCESSTYPE},USERID("NOSWEAT")) AND NOT op-flag THEN
DO:
  MESSAGE "Security Access for this Function Denied!"
      VIEW-AS ALERT-BOX INFORMATION.
  op-flag = no.
END.
ELSE
op-flag = yes.
