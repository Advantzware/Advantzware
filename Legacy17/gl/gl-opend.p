
DEF INPUT  PARAM ip-rowid AS   ROWID NO-UNDO.
DEF INPUT  PARAM ip-date  AS   DATE NO-UNDO.
DEF OUTPUT PARAM op-bal   AS   DEC NO-UNDO.


FIND account WHERE ROWID(account) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL account THEN
FIND LAST period
    WHERE period.company EQ account.company
      AND period.pst     LE ip-date
      AND period.pend    GE ip-date
    NO-LOCK.

IF AVAIL period THEN DO:
  RUN gl/gl-open1.p (RECID(account), period.yr, ip-date, period.pnum,
                     OUTPUT op-bal).

  FOR EACH glhist NO-LOCK
      WHERE glhist.company EQ account.company
        AND glhist.actnum  EQ account.actnum
        AND glhist.tr-date GE period.pst 
        AND glhist.tr-date LT ip-date:

    op-bal = op-bal + glhist.tr-amt.
  END.

  FOR EACH gltrans NO-LOCK
      WHERE gltrans.company EQ account.company
        AND gltrans.actnum  EQ account.actnum
        AND gltrans.tr-date GE period.pst 
        AND gltrans.tr-date LT ip-date:

    op-bal = op-bal + gltrans.tr-amt.
  END.
END.
