/* prgsecu2.i copied from prgsecur.i same but no message displayed for smart object 
            in the conatiner */

DEFINE VARIABLE v_groups AS cha NO-UNDO.
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

v_groups = "". /* YSK need to reset */
FOR EACH usergrps NO-LOCK:
    IF CAN-DO(TRIM(usergrps.users),USERID("ASI")) THEN
        v_groups = v_groups + usergrps.usergrps + ",".  /* YSK "," added  */
END.
 
FIND b-prgrms WHERE b-prgrms.prgmname = v-prgmname NO-LOCK NO-ERROR.
IF AVAILABLE b-prgrms THEN
DO:
    IF NUM-ENTRIES(v_groups) >= 1 THEN
    DO num-groups = 1 TO NUM-ENTRIES(v_groups):
        IF NOT CAN-DO(TRIM(REPLACE(b-prgrms.can_run," ","")),ENTRY(num-groups,v_groups)) AND
            NOT CAN-DO(TRIM(REPLACE(b-prgrms.can_update," ","")),ENTRY(num-groups,v_groups)) AND
            NOT CAN-DO(TRIM(REPLACE(b-prgrms.can_create," ","")),ENTRY(num-groups,v_groups)) AND
            NOT CAN-DO(TRIM(REPLACE(b-prgrms.can_delete," ","")),ENTRY(num-groups,v_groups)) THEN
            NEXT.
        group-ok = YES.
        LEAVE.
    END.
    IF NOT CAN-DO(TRIM(REPLACE(b-prgrms.can_run," ","")),USERID("ASI")) AND
        NOT CAN-DO(TRIM(REPLACE(b-prgrms.can_update," ","")),USERID("ASI")) AND
        NOT CAN-DO(TRIM(REPLACE(b-prgrms.can_create," ","")),USERID("ASI")) AND
        NOT CAN-DO(TRIM(REPLACE(b-prgrms.can_delete," ","")),USERID("ASI")) AND NOT group-ok THEN
    DO:
        access-close = YES.    /* used later in methods/template/windows.i - local-initialize procedure */
    END.
END. 
ELSE
DO: 
    RETURN.
END.
