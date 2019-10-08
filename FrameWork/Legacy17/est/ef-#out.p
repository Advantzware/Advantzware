
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-#out   AS INT NO-UNDO.

DEF VAR li AS INT NO-UNDO.
DEF VAR ll-foam AS LOG NO-UNDO.

DEF BUFFER bf-nsh FOR ef-nsh.


op-#out = 1.

FIND ef WHERE ROWID(ef) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL ef THEN DO:
  IF ef.spare-int-1 EQ 0 THEN DO:
      IF ef.n-out   NE 0 THEN op-#out = op-#out * ef.n-out.
      IF ef.n-out-l NE 0 THEN op-#out = op-#out * ef.n-out-l.
      IF ef.n-out-d NE 0 THEN op-#out = op-#out * ef.n-out-d.
    
      RUN cec/isitfoam.p (ROWID(ef), OUTPUT ll-foam). 
               
      IF ll-foam THEN
      FIND FIRST ef-nsh OF ef NO-LOCK NO-ERROR.
    
      IF AVAIL ef-nsh THEN DO:
        op-#out = 0.
    
        FOR EACH bf-nsh OF ef NO-LOCK BREAK BY bf-nsh.sheet-no:
          IF FIRST-OF(bf-nsh.sheet-no) THEN li = 1.
          li = li * bf-nsh.n-out-l * bf-nsh.n-out-w * bf-nsh.n-out-d.
          IF LAST-OF(bf-nsh.sheet-no) THEN op-#out = op-#out + li.
        END.
      END.
  END.
  ELSE DO:
      op-#out = ef.spare-int-1.
  END.
END.
