/* Update line number of all related tables when the oe-ordl.line changes */
DEFINE INPUT  PARAMETER iprOeOrdl AS ROWID       NO-UNDO.
DEFINE INPUT  PARAMETER ipcOldINo AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiOldLine AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER iplNoDuplINo AS LOGICAL  NO-UNDO.

DEF BUFFER oe-ordl-q-no FOR reftable.
DEF BUFFER oe-ordl-whs-item FOR reftable.

FIND FIRST oe-ordl WHERE ROWID(oe-ordl) EQ iprOeOrdl
  NO-LOCK NO-ERROR.

IF AVAIL oe-ordl THEN DO:

  IF oe-ordl.line NE 0                       AND
     ipiOldLine NE 0                         AND
     (oe-ordl.line NE ipiOldLine OR
      oe-ordl.i-no NE ipcOldINo OR iplNoDuplINo) THEN DO:

    RELEASE oe-ordm.
    DO WHILE TRUE:
      FIND NEXT oe-ordm EXCLUSIVE
          WHERE oe-ordm.company    EQ oe-ordl.company
            AND oe-ordm.ord-no     EQ oe-ordl.ord-no
            AND oe-ordm.ord-i-no   EQ ipcOldINo
            AND (oe-ordm.ord-line  EQ ipiOldLine OR
                 (oe-ordm.ord-line NE oe-ordl.line AND iplNoDuplINo))
          NO-ERROR NO-WAIT.

      IF AVAIL oe-ordm THEN
        ASSIGN
         oe-ordm.ord-i-no = oe-ordl.i-no
         oe-ordm.ord-line = oe-ordl.line.

      ELSE LEAVE.
    END.

    RELEASE oe-rel.
    DO WHILE TRUE:
      FIND NEXT oe-rel EXCLUSIVE
          WHERE oe-rel.company EQ oe-ordl.company
            AND oe-rel.ord-no  EQ oe-ordl.ord-no
            AND oe-rel.i-no    EQ ipcOldINo
            AND (oe-rel.line   EQ ipiOldLine OR
                 (oe-rel.line  NE oe-ordl.line AND iplNoDuplINo))
          NO-ERROR NO-WAIT.

      IF AVAIL oe-rel THEN
        ASSIGN
         oe-rel.i-no = oe-ordl.i-no
         oe-rel.line = oe-ordl.line.

      ELSE LEAVE.
    END.

    RELEASE oe-rell.
    DO WHILE TRUE:
      FIND NEXT oe-rell EXCLUSIVE
          WHERE oe-rell.company EQ oe-ordl.company
            AND oe-rell.ord-no  EQ oe-ordl.ord-no
            AND oe-rell.i-no    EQ ipcOldINo
            AND (oe-rell.line   EQ ipiOldLine OR
                 (oe-rell.line  NE oe-ordl.line AND iplNoDuplINo))
          NO-ERROR NO-WAIT.

      IF AVAIL oe-rell THEN
        ASSIGN
         oe-rell.i-no = oe-ordl.i-no
         oe-rell.line = oe-ordl.line.

      ELSE LEAVE.
    END.

    RELEASE oe-boll.
    DO WHILE TRUE:
      FIND NEXT oe-boll EXCLUSIVE
          WHERE oe-boll.company EQ oe-ordl.company
            AND oe-boll.ord-no  EQ oe-ordl.ord-no
            AND oe-boll.i-no    EQ ipcOldINo
            AND (oe-boll.line   EQ ipiOldLine OR
                 (oe-boll.line  NE oe-ordl.line AND iplNoDuplINo))
          NO-ERROR NO-WAIT.

      IF AVAIL oe-boll THEN
        ASSIGN
         oe-boll.i-no = oe-ordl.i-no
         oe-boll.line = oe-ordl.line.

      ELSE LEAVE.
    END.

    RELEASE inv-line.
    DO WHILE TRUE:
      FIND NEXT inv-line EXCLUSIVE
          WHERE inv-line.company EQ oe-ordl.company
            AND inv-line.ord-no  EQ oe-ordl.ord-no
            AND inv-line.i-no    EQ ipcOldINo
            AND (inv-line.line   EQ ipiOldLine OR
                 (inv-line.line  NE oe-ordl.line AND iplNoDuplINo))
          NO-ERROR NO-WAIT.

      IF AVAIL inv-line THEN DO:
     
        ASSIGN
         inv-line.i-no = oe-ordl.i-no
         inv-line.line = oe-ordl.line.

        RELEASE inv-misc.
        DO WHILE TRUE:
          FIND NEXT inv-misc EXCLUSIVE
              WHERE inv-misc.r-no    EQ inv-misc.r-no                                
                AND inv-misc.line  EQ ipiOldLine             
              NO-ERROR NO-WAIT.
    
          IF AVAIL inv-misc THEN
            ASSIGN             
             inv-misc.line = oe-ordl.line.
    
          ELSE LEAVE.
        END.

      END.
      ELSE LEAVE.
    END.


    RELEASE oe-ordl-q-no.
    DO WHILE TRUE:
      FIND NEXT oe-ordl-q-no EXCLUSIVE
          WHERE oe-ordl-q-no.reftable EQ "oe-ordl.q-no"
            AND oe-ordl-q-no.company  EQ oe-ordl.company
            AND oe-ordl-q-no.loc      EQ STRING(oe-ordl.ord-no,"9999999999")
            AND oe-ordl-q-no.code     EQ ipcOldINo
            AND (oe-ordl-q-no.code2   EQ STRING(ipiOldLine,"9999999999") OR
                 (oe-ordl-q-no.code2  NE STRING(oe-ordl.line,"9999999999") AND iplNoDuplINo))
          NO-ERROR NO-WAIT.

      IF AVAIL oe-ordl-q-no THEN
        ASSIGN
         oe-ordl-q-no.code  = oe-ordl.i-no
         oe-ordl-q-no.code2 = STRING(oe-ordl.line,"9999999999").

      ELSE LEAVE.
    END.

    RELEASE oe-ordl-whs-item.
    DO WHILE TRUE:
      FIND NEXT oe-ordl-whs-item EXCLUSIVE
          WHERE oe-ordl-whs-item.reftable EQ "oe-ordl.whs-item"
            AND oe-ordl-whs-item.company  EQ oe-ordl.company
            AND oe-ordl-whs-item.loc      EQ STRING(oe-ordl.ord-no,"9999999999")
            AND oe-ordl-whs-item.code     EQ ipcOldINo
            AND (oe-ordl-whs-item.code2   EQ STRING(ipiOldLine,"9999999999") OR
                 (oe-ordl-whs-item.code2  NE STRING(oe-ordl.line,"9999999999") AND iplNoDuplINo))
          NO-ERROR NO-WAIT.

      IF AVAIL oe-ordl-whs-item THEN
        ASSIGN
         oe-ordl-whs-item.code  = oe-ordl.i-no
         oe-ordl-whs-item.code2 = STRING(oe-ordl.line,"9999999999").

      ELSE LEAVE.
    END.
  END.
END.
