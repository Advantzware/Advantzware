/* fEstOpRecKey.i */

FUNCTION fEstOpRecKey RETURNS CHARACTER (ipcRecKey AS CHARACTER):
    DEFINE BUFFER bEstOp FOR est-op.
    
    IF est-op.line GE 500 THEN
        FIND FIRST bEstOp NO-LOCK
            WHERE bEstOp.company EQ est-op.company
              AND bEstOp.est-no  EQ est-op.est-no
              AND bEstOp.eqty    EQ est-op.eqty
              AND bEstOp.qty     EQ est-op.qty
              AND bEstOp.s-num   EQ est-op.s-num
              AND bEstOp.d-seq   EQ est-op.d-seq
              AND bEstOp.b-num   EQ est-op.b-num
              AND bEstOp.op-pass EQ est-op.op-pass
              AND bEstOp.m-code  EQ est-op.m-code
              AND bEstOp.dept    EQ est-op.dept
              AND bEstOp.line    EQ est-op.line - 500
            NO-ERROR.
    RETURN IF AVAILABLE bEstOp THEN bEstOp.rec_key ELSE ipcRecKey.
END FUNCTION.
