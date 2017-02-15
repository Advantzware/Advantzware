/* security.i */

DEFINE OUTPUT PARAMETER op-flag AS LOGICAL NO-UNDO.

op-flag = no.
DO num-groups = 1 TO NUM-ENTRIES(g_groups):
  IF NOT CAN-DO(b-prgrms.can_{&ACCESSTYPE},ENTRY(num-groups,g_groups)) THEN
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
