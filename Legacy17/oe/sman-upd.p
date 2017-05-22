
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF VAR cocode AS CHAR NO-UNDO.

DEF TEMP-TABLE wf NO-UNDO
    FIELD man LIKE sman.sman
    FIELD pct AS   DEC
    FIELD com AS   DEC
    FIELD r-i AS   ROWID
    FIELD tbl AS   INT.

DEF BUFFER bf FOR wf.

DEF VAR li AS INT NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR lv-s-man LIKE oe-ordl.s-man NO-UNDO.
DEF VAR lv-s-pct LIKE oe-ordl.s-pct NO-UNDO.
DEF VAR lv-s-com LIKE oe-ordl.s-comm NO-UNDO.


FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-ordl THEN
  FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

ELSE DO:
  FIND oe-ordm WHERE ROWID(oe-ordm) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL oe-ordm THEN
    FIND FIRST oe-ord OF oe-ordm NO-LOCK NO-ERROR.
  ELSE
    FIND oe-ord WHERE ROWID(oe-ord) EQ ip-rowid NO-LOCK NO-ERROR.
END.

RELEASE oe-ordl.
RELEASE oe-ordm.

IF AVAIL oe-ord THEN DO:
  cocode = oe-ord.company.

  DO TRANSACTION:
    {sys/inc/oecomm.i}
  END.

  IF oecomm-int EQ 0 THEN ll = NO.

  ELSE DO:
    DO li = 1 TO EXTENT(oe-ord.sman):
      CREATE wf.
      ASSIGN
       wf.man = oe-ord.sman[li]
       wf.pct = oe-ord.s-pct[li]
       wf.com = oe-ord.s-comm[li]
       wf.r-i = ROWID(oe-ord)
       wf.tbl = 1.
    END.

    FOR EACH oe-ordl OF oe-ord NO-LOCK:
      DO li = 1 TO EXTENT(oe-ordl.s-man):
        CREATE wf.
        ASSIGN
         wf.man = oe-ordl.s-man[li]
         wf.pct = oe-ordl.s-pct[li]
         wf.com = oe-ordl.s-comm[li]
         wf.r-i = ROWID(oe-ordl)
         wf.tbl = 2.
      END.
    END.

    FOR EACH oe-ordm OF oe-ord NO-LOCK:
      DO li = 1 TO EXTENT(oe-ordm.s-man):
        CREATE wf.
        ASSIGN
         wf.man = oe-ordm.s-man[li]
         wf.pct = oe-ordm.s-pct[li]
         wf.com = oe-ordm.s-comm[li]
         wf.r-i = ROWID(oe-ordm)
         wf.tbl = 3.
      END.
    END.

    FOR EACH wf WHERE wf.man EQ "":
      DELETE wf.
    END.

    FOR EACH bf WHERE bf.r-i NE ip-rowid:
      ll = NO.

      FIND FIRST wf
          WHERE wf.man EQ bf.man
            AND wf.r-i EQ ip-rowid
          NO-ERROR.

      ll = AVAIL wf.

      IF ll THEN BUFFER-COMPARE wf EXCEPT r-i tbl TO bf SAVE RESULT IN ll.

      IF NOT ll THEN DO:
        MESSAGE "Sales Rep info does not match on Header & All Line Items,"
                "update all order records with this data?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll.

        LEAVE.
      END.
    END.
  END.

  IF ll THEN DO:
    li = 0.
    FOR EACH wf WHERE wf.r-i EQ ip-rowid:
      li = li + 1.
      IF li LE EXTENT(lv-s-man) THEN
        ASSIGN
         lv-s-man[li] = wf.man
         lv-s-pct[li] = wf.pct
         lv-s-com[li] = wf.com.
    END.

    FOR EACH wf WHERE wf.r-i NE ip-rowid BREAK BY wf.r-i:
      IF FIRST-OF(wf.r-i) THEN DO:
        IF wf.tbl EQ 1 THEN DO:
          FIND oe-ord WHERE ROWID(oe-ord) EQ wf.r-i NO-ERROR.
          IF AVAIL oe-ord THEN
          DO li = 1 TO EXTENT(lv-s-man):
            ASSIGN
             oe-ord.sman[li]   = lv-s-man[li]
             oe-ord.s-pct[li]  = lv-s-pct[li]
             oe-ord.s-comm[li] = lv-s-com[li].
          END.
        END.

        ELSE
        IF wf.tbl EQ 2 THEN DO:
          FIND oe-ordl WHERE ROWID(oe-ordl) EQ wf.r-i NO-ERROR.
          IF AVAIL oe-ordl THEN
          DO li = 1 TO EXTENT(lv-s-man):
            ASSIGN
             oe-ordl.s-man[li]  = lv-s-man[li]
             oe-ordl.s-pct[li]  = lv-s-pct[li]
             oe-ordl.s-comm[li] = lv-s-com[li].
          END.
        END.

        ELSE
        IF wf.tbl EQ 3 THEN DO:
          FIND oe-ordm WHERE ROWID(oe-ordm) EQ wf.r-i NO-ERROR.
          IF AVAIL oe-ordm THEN
          DO li = 1 TO EXTENT(lv-s-man):
            ASSIGN
             oe-ordm.s-man[li]  = lv-s-man[li]
             oe-ordm.s-pct[li]  = lv-s-pct[li]
             oe-ordm.s-comm[li] = lv-s-com[li].
          END.
        END.
      END.
    END.
  END.
END.

