
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF VAR ld-po-cost AS DEC NO-UNDO.


FIND vend WHERE ROWID(vend) EQ ip-rowid NO-ERROR.

IF AVAIL vend THEN DO:
  vend.ord-bal = 0.

  FOR EACH po-ord NO-LOCK
      WHERE po-ord.company EQ vend.company
        AND po-ord.opened  EQ YES
        AND po-ord.vend-no EQ vend.vend-no
      USE-INDEX opened:

    ld-po-cost = po-ord.t-cost.

    FOR EACH ap-invl NO-LOCK
        WHERE ap-invl.company EQ po-ord.company
          AND ap-invl.po-no   EQ po-ord.po-no
          AND CAN-FIND(FIRST ap-inv
                       WHERE ap-inv.i-no   EQ ap-invl.i-no
                         AND ap-inv.posted EQ YES)
        USE-INDEX po-no:
      ld-po-cost = ld-po-cost - ap-invl.amt.  
    END.
        
    IF ld-po-cost GT 0 THEN vend.ord-bal = vend.ord-bal + ld-po-cost.
  END.
END.
