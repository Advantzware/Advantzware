
DEF INPUT  PARAM ip-rowid AS   ROWID NO-UNDO.
DEF INPUT  PARAM ip-start AS   DATE NO-UNDO.
DEF INPUT  PARAM ip-end   AS   DATE NO-UNDO.
DEF OUTPUT PARAM op-bal   AS   DEC NO-UNDO.


FIND account WHERE ROWID(account) EQ ip-rowid NO-LOCK NO-ERROR.

RUN gl/gl-opend.p (ROWID(account), ip-start, OUTPUT op-bal).

FOR EACH glhist NO-LOCK
    WHERE glhist.company EQ account.company
      AND glhist.actnum  EQ account.actnum
      AND glhist.tr-date GE ip-start
      AND glhist.tr-date LE ip-end:

  op-bal = op-bal + glhist.tr-amt.
END.

FOR EACH gltrans NO-LOCK
    WHERE gltrans.company EQ account.company
      AND gltrans.actnum  EQ account.actnum
      AND gltrans.tr-date GE ip-start
      AND gltrans.tr-date LE ip-end:  
  op-bal = op-bal + gltrans.tr-amt.
END.
