/* prgsecur.i {1} - WIN or blank */

{methods/defines/globdefs.i}

DEFINE BUFFER b-prgrms FOR prgrms.

DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE Audit_File AS CHARACTER NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.
DEFINE VARIABLE num-groups AS INTEGER NO-UNDO.
DEFINE VARIABLE group-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE access-close AS LOGICAL NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("NOSWEAT") + "..".
ELSE
ASSIGN
/*   period_pos = INDEX(PROGRAM-NAME(1),".")                                             */
/*   v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 10) + 1) */
  v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1)
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

FIND b-prgrms WHERE b-prgrms.prgmname = v-prgmname NO-LOCK NO-ERROR.
IF AVAILABLE b-prgrms THEN
DO:
  DO num-groups = 1 TO NUM-ENTRIES(g_groups):
    IF NOT CAN-DO(b-prgrms.can_run,ENTRY(num-groups,g_groups)) AND
       NOT CAN-DO(b-prgrms.can_update,ENTRY(num-groups,g_groups)) AND
       NOT CAN-DO(b-prgrms.can_create,ENTRY(num-groups,g_groups)) AND
       NOT CAN-DO(b-prgrms.can_delete,ENTRY(num-groups,g_groups)) THEN
    NEXT.
    group-ok = yes.
    LEAVE.
  END.
  IF NOT CAN-DO(b-prgrms.can_run,USERID("NOSWEAT")) AND
     NOT CAN-DO(b-prgrms.can_update,USERID("NOSWEAT")) AND
     NOT CAN-DO(b-prgrms.can_create,USERID("NOSWEAT")) AND
     NOT CAN-DO(b-prgrms.can_delete,USERID("NOSWEAT")) AND NOT group-ok THEN
  DO:
    MESSAGE "Program :" PROGRAM-NAME(1) SKIP "Title :" b-prgrms.prgtitle SKIP(1)
        "Access to this Program Denied - Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
    access-close = YES.    /* used later in methods/template/windows.i - local-initialize procedure */
    
    IF "{1}" NE "WIN" THEN
    DO:
       APPLY 'CLOSE' TO THIS-PROCEDURE. /*task 10020703*/
       RETURN.
    END.
  END.
END. 
ELSE
DO: 
  MESSAGE "Program :" PROGRAM-NAME(1) SKIP(1)
      "Program Master Record Does Not Exist - Contact Systems Manager" 
          VIEW-AS ALERT-BOX ERROR.
  RETURN.
END.
