
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-act-rate AS LOG INIT NO NO-UNDO.
DEF OUTPUT PARAM op-tot-rate AS DEC INIT 0 NO-UNDO.

DEF VAR li AS INT NO-UNDO.
DEF VAR g_company LIKE mch-act.company NO-UNDO.


FIND mch-act WHERE ROWID(mch-act) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL mch-act THEN DO:
  g_company = mch-act.company.

  {sys/inc/tspost.i}

  IF v-tspost-val EQ "Actual" THEN
  DO li = 1 TO EXTENT(mch-act.emp-id):
    ASSIGN
     op-act-rate = YES
     op-tot-rate = op-tot-rate + mch-act.rate[li].
  END.
END.
