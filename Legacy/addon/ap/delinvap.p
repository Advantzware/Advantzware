
DEF INPUT PARAM ip-company LIKE ap-inv.company NO-UNDO.                                          

DEF BUFFER b-ap-inv FOR ap-inv.


FOR EACH ap-inv NO-LOCK
    WHERE ap-inv.company  EQ ip-company
      AND ap-inv.posted   EQ NO
      AND ap-inv.user-id  EQ USERID("nosweat")
      AND (ap-inv.inv-no  BEGINS FILL(" ",100) OR
           ap-inv.i-no    EQ 0):

  FIND b-ap-inv WHERE ROWID(b-ap-inv) EQ ROWID(ap-inv)
      EXCLUSIVE NO-ERROR NO-WAIT.
  IF AVAIL b-ap-inv THEN DELETE b-ap-inv.
END.
