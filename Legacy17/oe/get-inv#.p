
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF VAR lv-inv-no LIKE inv-head.inv-no NO-UNDO.


FIND inv-head WHERE ROWID(inv-head) EQ ip-rowid NO-ERROR.

IF AVAIL inv-head THEN DO:
  
  loop:
  REPEAT:
  
     FIND FIRST ar-ctrl WHERE
          ar-ctrl.company EQ inv-head.company
          EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    
     IF AVAIL ar-ctrl THEN
     DO:
        ASSIGN
           lv-inv-no        = ar-ctrl.last-inv
           ar-ctrl.last-inv = ar-ctrl.last-inv + 1.
    
        FIND CURRENT ar-ctrl NO-LOCK.
        LEAVE loop.
     END.
  END.

  ASSIGN
   inv-head.inv-no  = lv-inv-no + 1
   inv-head.printed = YES
   inv-head.stat    = "X".

  RUN oe/checkinv#.p(BUFFER inv-head).
END.
