/* --------------------------------------------------- oe/closchk.p 10/01 JLF */
/*                                                                            */
/* Order Close - Check to see if order for invoice line should be closed      */
/*                                                                            */
/* -------------------------------------------------------------------------- */
DEFINE INPUT  PARAMETER ipiOrdNo LIKE oe-ord.ord-no     NO-UNDO.

{sys/inc/var.i SHARED}

{oe/closchk.i}

DEFINE VARIABLE lOrdClosed AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cStatus AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cReason AS CHARACTER   NO-UNDO.

FOR EACH w-ord BREAK BY w-ord.ord-no:
  IF NOT FIRST-OF(w-ord.ord-no) THEN DELETE w-ord.
END.

IF ipiOrdNo NE 0 THEN DO:
    FIND FIRST oe-ord
        WHERE oe-ord.company EQ cocode
          AND oe-ord.ord-no  EQ ipiOrdNo
        NO-LOCK NO-ERROR.
    CREATE w-ord.
    ASSIGN
        w-ord.ord-no = oe-ord.ord-no
        w-ord.rec-id = RECID(oe-ord)
        .
END.

FOR EACH w-ord:
    FIND FIRST oe-ord
        WHERE oe-ord.company EQ cocode
          AND oe-ord.ord-no  EQ w-ord.ord-no
        NO-LOCK NO-ERROR.
    lOrdClosed = AVAIL oe-ord.                
    IF lOrdClosed THEN
        FOR EACH oe-ordl
            WHERE oe-ordl.company EQ oe-ord.company
              AND oe-ordl.ord-no  EQ oe-ord.ord-no
              AND oe-ordl.stat    NE "C"
            NO-LOCK:
        
            RUN oe/CloseOrder.p(INPUT ROWID(oe-ordl),
                                INPUT NO,
                                OUTPUT cStatus,
                                OUTPUT cReason).
            IF cStatus NE 'C' THEN DO:
                lOrdClosed = NO.
                LEAVE.
            END.
        END.
    
    {oe/closeaud.i oe-ord}
    
    IF lOrdClosed THEN 
        reftable.val[1] = 2.
    ELSE DO:
        reftable.val[1] = 1.
        DELETE w-ord.
    END.
END.   /* for each w-ord */
