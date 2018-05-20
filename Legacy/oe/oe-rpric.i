/* -------------------------------------------------- oe/oe-rpric.i 11/96 JLF */
/*                                                                            */
/* order entry - ITEM PRICING FROM PRICE MATRIX                               */
/*                   FOR STOCK BOXES ONLY                                     */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

DEFINE SHARED BUFFER x{2} FOR {2}. /* BUFFER WITH ORDER HEADER */
DEFINE VARIABLE lMatrixExists AS LOGICAL NO-UNDO.
DEFINE VARIABLE hdPriceProcs  AS HANDLE  NO-UNDO.
{oe/ttPriceHold.i "NEW SHARED"}
RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.

DISABLE TRIGGERS FOR LOAD OF {1}.

FIND FIRST cust NO-LOCK
    WHERE cust.company EQ x{2}.company
    AND cust.cust-no EQ  x{2}.cust-no
    USE-INDEX cust NO-ERROR.
IF NOT AVAILABLE cust THEN RETURN.

IF cust.auto-reprice THEN 
    FIND FIRST {1} OF x{2} {3} NO-ERROR.  
IF NOT AVAILABLE {1} THEN RETURN.

RUN CalculateLinePrice IN hdPriceProcs (ROWID({1}), {1}.i-no, {1}.cust-no, {1}.ship-id, {1}.qty, 
    OUTPUT lMatrixExists, INPUT-OUTPUT {1}.price, INPUT-OUTPUT {1}.pr-uom).
