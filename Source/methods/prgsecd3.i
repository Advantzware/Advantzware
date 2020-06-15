/* prgsecdt.i  security for can-run,can-update,can_create and can_delete */
/*
{methods/defines/globdefs.i}
*/
DEFINE BUFFER b-prgrms FOR prgrms.

DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname    NO-UNDO.
DEFINE VARIABLE v-dirname  LIKE b-prgrms.DIR_group   NO-UNDO.
DEFINE VARIABLE period_pos              AS INTEGER   NO-UNDO.
DEFINE VARIABLE num-groups              AS INTEGER   NO-UNDO.
DEFINE VARIABLE group-ok                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE access-close            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-can-run               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-can-update            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-can-create            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-can-delete            AS LOGICAL   NO-UNDO.

{methods/fShowRestrictionMessage.i}

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("ASI") + "..".
ELSE DO:
    v-prgmname = PROGRAM-NAME(1).
    IF INDEX(v-prgmname,"\") GT 0 THEN
    v-prgmname = REPLACE(v-prgmname,"\","/").
    ASSIGN
        period_pos = INDEX(PROGRAM-NAME(1),".")
        v-dirname  = IF INDEX(v-prgmname,"/") GT 0 THEN SUBSTRING(v-prgmname,1,INDEX(v-prgmname,"/") - 1) ELSE ""
        v-prgmname = IF INDEX(v-prgmname,"/") GT 0 THEN SUBSTRING(v-prgmname,INDEX(v-prgmname,"/") + 1) ELSE v-prgmname
        period_pos = INDEX(v-prgmname,".")
        v-prgmname = SUBSTRING(v-prgmname,1,period_pos)
        .
END.
v-prgmname = "{1}".
FIND FIRST b-prgrms NO-LOCK
     WHERE b-prgrms.prgmname  EQ v-prgmname
       AND b-prgrms.DIR_group EQ v-dirname
     NO-ERROR.
IF NOT AVAILABLE b-prgrms THEN
FIND FIRST b-prgrms NO-LOCK
     WHERE b-prgrms.prgmname EQ v-prgmname
     NO-ERROR.
IF AVAILABLE b-prgrms THEN DO:
    DO num-groups = 1 TO NUM-ENTRIES(g_groups):
        IF NOT CAN-DO(TRIM(REPLACE(b-prgrms.can_run," ","")),   ENTRY(num-groups,g_groups)) AND
           NOT CAN-DO(TRIM(REPLACE(b-prgrms.can_update," ","")),ENTRY(num-groups,g_groups)) AND
           NOT CAN-DO(TRIM(REPLACE(b-prgrms.can_create," ","")),ENTRY(num-groups,g_groups)) AND
           NOT CAN-DO(TRIM(REPLACE(b-prgrms.can_delete," ","")),ENTRY(num-groups,g_groups)) THEN
            NEXT.
        ASSIGN
            v-can-run    = NOT v-can-run    AND CAN-DO(TRIM(REPLACE(b-prgrms.can_run," ","")),   ENTRY(num-groups,g_groups))
            v-can-update = NOT v-can-update AND CAN-DO(TRIM(REPLACE(b-prgrms.can_update," ","")),ENTRY(num-groups,g_groups))
            v-can-create = NOT v-can-create AND CAN-DO(TRIM(REPLACE(b-prgrms.can_create," ","")),ENTRY(num-groups,g_groups))
            v-can-delete = NOT v-can-delete AND CAN-DO(TRIM(REPLACE(b-prgrms.can_delete," ","")),ENTRY(num-groups,g_groups))
            group-ok     = YES
            .
    END. /* do num-groups */
    IF NOT CAN-DO(TRIM(REPLACE(b-prgrms.can_run," ","")),   USERID("ASI")) AND
       NOT CAN-DO(TRIM(REPLACE(b-prgrms.can_update," ","")),USERID("ASI")) AND
       NOT CAN-DO(TRIM(REPLACE(b-prgrms.can_create," ","")),USERID("ASI")) AND
       NOT CAN-DO(TRIM(REPLACE(b-prgrms.can_delete," ","")),USERID("ASI")) AND
       NOT group-ok THEN DO:
        IF fShowRestrictionMessage(g_company) THEN
        MESSAGE
            "Program :" PROGRAM-NAME(1) SKIP 
            "Title :" b-prgrms.prgtitle SKIP(1)
            "Access to this Program Denied - Contact Systems Manager"
        VIEW-AS ALERT-BOX ERROR.
            access-close = YES.  /* used later in methods/template/windows.i - local-initialize procedure */
    END.
    ELSE 
    ASSIGN
        v-can-run    = NOT v-can-run    AND CAN-DO(TRIM(REPLACE(b-prgrms.can_run," ","")),   USERID("ASI"))
        v-can-update = NOT v-can-update AND CAN-DO(TRIM(REPLACE(b-prgrms.can_update," ","")),USERID("ASI"))
        v-can-create = NOT v-can-create AND CAN-DO(TRIM(REPLACE(b-prgrms.can_create," ","")),USERID("ASI"))
        v-can-delete = NOT v-can-delete AND CAN-DO(TRIM(REPLACE(b-prgrms.can_delete," ","")),USERID("ASI"))
        .
END. /* if avail */ 
ELSE DO: 
    MESSAGE "Program :" PROGRAM-NAME(1) SKIP(1)
        "Program Master Record Does Not Exist - Contact Systems Manager" 
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END. /* else */
