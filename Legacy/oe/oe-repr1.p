/* -------------------------------------------------- oe/oe-repr1.p 3/96 fwk  */
/*                                                                            */
/* reprice option - ITEM REPRICING FROM PRICE MATRIX                          */
/*                   FOR STOCK BOXES ONLY                                     */
/* -------------------------------------------------------------------------- */
DEFINE PARAMETER BUFFER ipbf-oe-ord FOR oe-ord.
DEFINE INPUT PARAMETER ipiPriceLevel AS INTEGER NO-UNDO.

DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
DEFINE BUFFER bf-cust FOR cust.

DEFINE VARIABLE dOrderOrigAmount        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dOrderTotalAmountChange AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lMatrixMatchFound       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMatrixMatchMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lQtyMatchFound          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lQtyWithinMatrixRange   AS LOGICAL   NO-UNDO.

DEFINE VARIABLE hdPriceProcs  AS HANDLE.
{oe/ttPriceHold.i "NEW SHARED"}
RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.

FIND FIRST bf-cust NO-LOCK 
    WHERE bf-cust.company EQ ipbf-oe-ord.company
    AND bf-cust.cust-no EQ ipbf-oe-ord.cust-no
    USE-INDEX cust 
    NO-ERROR.

IF NOT AVAILABLE bf-cust OR NOT bf-cust.auto-reprice THEN RETURN.

ASSIGN 
    dOrderTotalAmountChange = 0
    dOrderOrigAmount        = 0.

FOR EACH bf-oe-ordl EXCLUSIVE-LOCK  
    WHERE bf-oe-ordl.company EQ ipbf-oe-ord.company
    AND bf-oe-ordl.ord-no EQ ipbf-oe-ord.ord-no
    , FIRST itemfg NO-LOCK 
    WHERE itemfg.company  EQ bf-oe-ordl.company 
    AND itemfg.i-no EQ bf-oe-ordl.i-no
    :
    dOrderOrigAmount = bf-oe-ordl.t-price.

    RUN GetPriceMatrixPrice IN hdPriceProcs (bf-oe-ordl.company, bf-oe-ordl.i-no, ipbf-oe-ord.cust-no, ipbf-oe-ord.ship-id,
        0, ipiPriceLevel,
        OUTPUT lMatrixMatchFound, OUTPUT cMatrixMatchMessage,
        INPUT-OUTPUT bf-oe-ordl.price, INPUT-OUTPUT bf-oe-ordl.pr-uom, 
        OUTPUT lQtyMatchFound, OUTPUT lQtyWithinMatrixRange).
    RUN GetPriceTotal IN hdPriceProcs (bf-oe-ordl.qty,
                                       bf-oe-ordl.price,
                                       bf-oe-ordl.pr-uom,
                                       itemfg.case-count,
                                       bf-oe-ordl.disc,
                                       OUTPUT bf-oe-ordl.t-price).
    
/*    RUN  oe/GetPriceTotal.p(     */
/*        INPUT bf-oe-ordl.qty,    */
/*        INPUT bf-oe-ordl.price,  */
/*        INPUT bf-oe-ordl.pr-uom, */
/*        INPUT itemfg.case-count, */
/*        INPUT bf-oe-ordl.disc,   */
/*        OUTPUT bf-oe-ordl.t-price*/
/*        ).                       */
    ASSIGN 
        dOrderTotalAmountChange = dOrderTotalAmountChange + bf-oe-ordl.t-price - dOrderOrigAmount. 
END. 

DO:
    FIND FIRST bf-cust EXCLUSIVE-LOCK 
        WHERE bf-cust.company EQ ipbf-oe-ord.company 
        AND bf-cust.cust-no EQ ipbf-oe-ord.cust-no
        USE-INDEX cust NO-ERROR.
    IF AVAILABLE bf-cust THEN
        ASSIGN bf-cust.ord-bal = bf-cust.ord-bal + dOrderTotalAmountChange.
END.
RELEASE bf-oe-ordl.
RELEASE bf-cust.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

