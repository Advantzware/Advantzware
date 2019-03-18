/* prgsecur.i */

{methods/defines/globdefs.i}

DEFINE BUFFER b-prgrms FOR prgrms.

DEFINE VARIABLE v-prgmname   LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE period_pos   AS INTEGER NO-UNDO.
DEFINE VARIABLE num-groups   AS INTEGER NO-UNDO.
DEFINE VARIABLE group-ok     AS LOGICAL NO-UNDO.
DEFINE VARIABLE access-close AS LOGICAL NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
    v-prgmname = USERID("ASI") + "..".
ELSE
    ASSIGN
        period_pos = INDEX(PROGRAM-NAME(1),".")
        v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
        v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

FIND b-prgrms WHERE b-prgrms.prgmname = v-prgmname NO-LOCK NO-ERROR.
IF AVAILABLE b-prgrms THEN
DO:
    /* Added for dynamic groups security Otherwise had to reenter into the software */
    g_groups = "".
    FOR EACH usergrps NO-LOCK:
        IF CAN-DO(TRIM(usergrps.users),USERID("ASI")) THEN
            g_groups = g_groups + usergrps.usergrps + ",".
    END.
    DO num-groups = 1 TO NUM-ENTRIES(g_groups):
        IF NOT CAN-DO(TRIM(b-prgrms.can_run),ENTRY(num-groups,g_groups)) THEN
            NEXT.
        group-ok = YES.
        LEAVE.
    END.
    IF NOT CAN-DO(TRIM(b-prgrms.can_run),USERID("ASI")) AND NOT group-ok THEN
    DO:
        MESSAGE "Program :" PROGRAM-NAME(1) SKIP 
            "Title :" b-prgrms.prgtitle SKIP(1)
            "Access to this Program Denied - Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.
        access-close = YES. /* used later in methods/template/windows.i - local-initialize procedure */
    END.
END. 
ELSE
DO: 
    MESSAGE "Program :" PROGRAM-NAME(1) SKIP(1)
        "Program Master Record Does Not Exist - Contact Systems Manager" 
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
