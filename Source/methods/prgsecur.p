/* prgsecur.p  security in place of prgsecur.i */
/*                                                                                         */
/* ip-program= program name or security function                                           */
/* ip-basis  = return op-run-access based on create, update, delete, etc                   */
/* ip-directory-check = Find program record based on directory in addition to program name */
/* ip-show-message = Show the user a message when not authorized                           */
/* ip-group-override = Determines group list differently - see code                        */
/* op-run-access     = Primary value returned                                              */
/* op-access-close   = For compatablility with other security include files                */
/* op-access-list    = list of 1's and 0's indicating can-run, etc in case this is needed  */
/*                     (position 1 = can-run, 2 = update, 3 = create, 4 = delete)          */

DEF INPUT PARAMETER ip-program AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-basis   AS CHAR NO-UNDO.
/* ip-basis can be access, view, add, update, delete or all */
DEF INPUT PARAMETER ip-directory-check AS LOG NO-UNDO.
DEF INPUT PARAMETER ip-show-messages AS LOG NO-UNDO.
DEF INPUT PARAMETER ip-group-override AS LOG NO-UNDO.
DEF OUTPUT PARAMETER op-run-access   AS LOG NO-UNDO.
DEF OUTPUT PARAMETER op-access-close AS LOG NO-UNDO.
DEF OUTPUT PARAMETER op-access-list AS CHAR NO-UNDO. /* list of 0 = no, 1 = yes */

{methods/defines/globdefs.i}

DEFINE BUFFER b-prgrms FOR prgrms.

DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE v-dirname LIKE b-prgrms.DIR_group NO-UNDO.

DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.
DEFINE VARIABLE num-groups AS INTEGER NO-UNDO.
DEFINE VARIABLE group-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE access-close AS LOGICAL NO-UNDO .
DEFINE VARIABLE run-access AS LOGICAL NO-UNDO.
DEF VAR v_groups AS cha NO-UNDO.
DEF VAR v-can-run AS LOG NO-UNDO.
DEF VAR v-can-view AS LOG NO-UNDO.
DEF VAR v-can-update AS LOG NO-UNDO.
DEF VAR v-can-create AS LOG NO-UNDO.
DEF VAR v-can-delete AS LOG NO-UNDO.

FUNCTION get-char-log RETURNS CHAR
  ( ip-log-val AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN (IF ip-log-val THEN "1" ELSE "0").

END FUNCTION.

IF ip-group-override THEN DO:
    v_groups = "". /* YSK need to reset */
    FOR EACH usergrps NO-LOCK:
        IF CAN-DO(usergrps.users,USERID("NOSWEAT")) THEN
        v_groups = v_groups + usergrps.usergrps + ",".  /* YSK "," added  */
    END.
END.
ELSE
    v_groups = g_groups.

IF INDEX(ip-program,".uib") NE 0 OR
   INDEX(ip-program,".ab")  NE 0 OR
   INDEX(ip-program,".ped") NE 0 THEN v-prgmname = USERID("NOSWEAT") + "..".
ELSE DO:
   v-prgmname = ip-program.

   IF INDEX(v-prgmname,"\") > 0 THEN v-prgmname = REPLACE(v-prgmname,"\","/").

  ASSIGN
  period_pos = INDEX(ip-program,".")
  v-dirname = IF INDEX(v-prgmname,"/") > 0 THEN SUBSTRING(v-prgmname,1,INDEX(v-prgmname,"/") - 1) ELSE ""
  v-prgmname = IF INDEX(v-prgmname,"/") > 0 THEN SUBSTR(v-prgmname,INDEX(v-prgmname,"/") + 1) ELSE v-prgmname
  
  /*v-prgmname = substring(ip-program,1,period_pos)*/
  period_pos = INDEX(v-prgmname,".").
  IF period_pos GT 0 THEN
    v-prgmname = substring(v-prgmname,1,period_pos).

END.



run-access = NO.
FIND b-prgrms WHERE b-prgrms.prgmname = v-prgmname AND
                    (b-prgrms.DIR_group = v-dirname 
                     OR 
                     NOT ip-directory-check)

                     NO-LOCK NO-ERROR.

IF NOT AVAIL b-prgrms THEN FIND b-prgrms WHERE b-prgrms.prgmname = v-prgmname NO-LOCK NO-ERROR.


IF AVAILABLE b-prgrms THEN
DO:
  DO num-groups = 1 TO NUM-ENTRIES(v_groups):
    
    IF NOT CAN-DO(b-prgrms.can_run,ENTRY(num-groups,v_groups)) AND       
       NOT CAN-DO(b-prgrms.can_update,ENTRY(num-groups,v_groups)) AND
       NOT CAN-DO(b-prgrms.can_create,ENTRY(num-groups,v_groups)) AND
       NOT CAN-DO(b-prgrms.can_delete,ENTRY(num-groups,v_groups)) THEN
    NEXT.
    
    IF NOT v-can-run AND CAN-DO(b-prgrms.can_run,ENTRY(num-groups,v_groups))
          THEN v-can-run = YES.
    IF NOT v-can-update AND CAN-DO(b-prgrms.can_update,ENTRY(num-groups,v_groups))
          THEN v-can-update = YES.
    IF NOT v-can-create AND CAN-DO(b-prgrms.can_create,ENTRY(num-groups,v_groups))
          THEN v-can-create = YES.
    IF NOT v-can-delete AND CAN-DO(b-prgrms.can_delete,ENTRY(num-groups,v_groups))
          THEN v-can-delete = YES.
        
    group-ok = yes.
    CASE ip-basis:
        WHEN "ACCESS" THEN run-access = v-can-run.        
        WHEN "VIEW"   THEN run-access = v-can-run.
        WHEN "CREATE" THEN run-access = v-can-create.
        WHEN "UPDATE" THEN run-access = v-can-update.
        WHEN "DELETE" THEN run-access = v-can-delete.
        WHEN "ALL" THEN run-access = v-can-run AND v-can-create AND v-can-update
                                     AND v-can-delete.
    END CASE.
    
    /* LEAVE.  */
  END.

  IF NOT CAN-DO(b-prgrms.can_run,USERID("NOSWEAT")) AND
     NOT CAN-DO(b-prgrms.can_update,USERID("NOSWEAT")) AND
     NOT CAN-DO(b-prgrms.can_create,USERID("NOSWEAT")) AND
     NOT CAN-DO(b-prgrms.can_delete,USERID("NOSWEAT")) AND NOT group-ok THEN
  DO:
    IF ip-show-messages THEN
    MESSAGE "Program :" ip-program SKIP "Title :" b-prgrms.prgtitle SKIP(1)
        "Access to this Program Denied - Contact Systems Manager" VIEW-AS ALERT-BOX ERROR.

    access-close = YES.  /* used later in methods/template/windows.i - local-initialize procedure */
    run-access = NO.
    CASE ip-basis:
        WHEN "ACCESS" THEN run-access = v-can-run.        
        WHEN "VIEW"   THEN run-access = v-can-run.
        WHEN "CREATE" THEN run-access = v-can-create.
        WHEN "UPDATE" THEN run-access = v-can-update.
        WHEN "DELETE" THEN run-access = v-can-delete.
        WHEN "ALL" THEN run-access = v-can-run AND v-can-create AND v-can-update
                                     AND v-can-delete.
    END CASE.
  
  END.
  ELSE DO:
      IF NOT v-can-run AND CAN-DO(b-prgrms.can_run,USERID("NOSWEAT"))
            THEN v-can-run = YES.
      IF NOT v-can-update AND CAN-DO(b-prgrms.can_update,USERID("NOSWEAT"))
            THEN v-can-update = YES.
      IF NOT v-can-create AND CAN-DO(b-prgrms.can_create,USERID("NOSWEAT"))
            THEN v-can-create = YES.
      IF NOT v-can-delete AND CAN-DO(b-prgrms.can_delete,USERID("NOSWEAT"))
            THEN v-can-delete = YES.
      IF v-can-run THEN
          run-access = NO.

      CASE ip-basis:
          WHEN "ACCESS" THEN run-access = v-can-run.        
          WHEN "VIEW"   THEN run-access = v-can-run.
          WHEN "CREATE" THEN run-access = v-can-create.
          WHEN "UPDATE" THEN run-access = v-can-update.
          WHEN "DELETE" THEN run-access = v-can-delete.
          WHEN "ALL" THEN run-access = v-can-run AND v-can-create AND v-can-update
                                       AND v-can-delete.
      END CASE.

  END.
END. 
ELSE
DO: 
  IF ip-show-messages THEN
  MESSAGE "Program :" ip-program SKIP(1)
      "Program Master Record Does Not Exist - Contact Systems Manager" 
          VIEW-AS ALERT-BOX ERROR.
  run-access = NO.
END.

op-access-close = access-close.
op-run-access   = run-access.

SUBSTRING(op-access-list, 1, 1) = get-char-log(v-can-run).
SUBSTRING(op-access-list, 2, 1) = get-char-log(v-can-update).
SUBSTRING(op-access-list, 3, 1) = get-char-log(v-can-create).
SUBSTRING(op-access-list, 4, 1) = get-char-log(v-can-delete).
