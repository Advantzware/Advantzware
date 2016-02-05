/*batch/batchrun.p  Run Batch procedures*/

DEF INPUT PARAM ip-seq-from AS INT NO-UNDO.
DEF INPUT PARAM ip-seq-to AS INT NO-UNDO.

{custom/globdefs.i}


DEF VAR li AS INT NO-UNDO.
DEF VAR v-run-param AS cha NO-UNDO.
DEF VAR v-run-prog AS cha NO-UNDO.

FOR EACH user-print WHERE user-print.company = g_company
                      AND user-print.BATCH <> "" 
                      AND user-print.batch-seq >= ip-seq-from
                      AND user-print.batch-seq <= ip-seq-to:
  /*
  v-run-param = "".
  v-run-prog = "".

  FIND FIRST prgrms WHERE prgrms.prgmname = user-print.program-id NO-LOCK NO-ERROR.
  IF AVAIL prgrms THEN v-run-prog = prgrms.dir_group + "/" + prgrms.prgmname + "r".
  */
  v-run-prog = user-print.program-id.

  IF search(v-run-prog) <> ?  THEN do:
     RUN VALUE(v-run-prog) (user-print.batch-seq). 
     ASSIGN user-print.last-date = TODAY
            user-print.last-time = TIME.

  END.
END.

