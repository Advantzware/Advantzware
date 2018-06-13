/* --------------------------------------------------- oe/oe-price.i 5/93 rd  */
/*                                                                            */
/* order entry - ITEM PRICING FROM PRICE MATRIX                               */
/*                   FOR STOCK BOXES ONLY                                     */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

DEFINE SHARED BUFFER x{2} FOR {2}. /* BUFFER WITH ORDER HEADER */

DEFINE SHARED VARIABLE save_id      AS RECID  NO-UNDO.  /* RECORD ID FOR ORDER LINE */
DEFINE SHARED VARIABLE v-i-item     LIKE itemfg.i-no NO-UNDO. /* INPUT ITEM */
DEFINE SHARED VARIABLE v-i-qty      LIKE {1}.qty NO-UNDO. /* INPUT QUANTITY */
DEFINE SHARED VARIABLE price-ent    AS LOG    NO-UNDO.
DEFINE SHARED VARIABLE matrixExists AS LOG    NO-UNDO.

DEFINE        VARIABLE hdPriceProcs AS HANDLE NO-UNDO.
{oe/ttPriceHold.i "NEW SHARED"}
RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.
DEFINE VARIABLE cShipID   AS CHARACTER NO-UNDO.
DEFINE BUFFER bf-inv-line FOR inv-line . 
DEFINE BUFFER bf-oe-ordl FOR oe-ordl.

DISABLE TRIGGERS FOR LOAD OF {1}.

IF save_id NE ? THEN
    FIND {1} WHERE RECID({1}) EQ save_id NO-ERROR.
  
IF NOT AVAILABLE {1} THEN RETURN.
FIND FIRST cust NO-LOCK 
    WHERE cust.company EQ x{2}.company
    AND cust.cust-no EQ x{2}.cust-no
    USE-INDEX cust NO-ERROR.
IF NOT AVAILABLE cust THEN RETURN.
   
IF "{1}" EQ "inv-line" THEN 
DO:  /*called from oe-ipric.p*/
/*get ship id from Bol for invoice if possible*/
    FIND FIRST bf-inv-line NO-LOCK 
        WHERE ROWID(bf-inv-line) EQ ROWID({1})
        NO-ERROR.
    IF AVAILABLE bf-inv-line THEN 
        FIND FIRST oe-bolh NO-LOCK 
            WHERE oe-bolh.b-no EQ bf-inv-line.b-no
            NO-ERROR.
    IF AVAILABLE oe-bolh THEN cShipID = oe-bolh.ship-id.
END.
ELSE DO:
    FIND FIRST bf-oe-ordl NO-LOCK 
        WHERE ROWID(bf-oe-ordl) EQ ROWID({1})
        NO-ERROR.
    IF AVAILABLE bf-oe-ordl THEN cShipID = bf-oe-ordl.ship-id.
END.    
RUN CalculateLinePrice IN hdPriceProcs (ROWID({1}), v-i-item, cust.cust-no, cShipID, v-i-qty, YES,
    OUTPUT matrixExists, INPUT-OUTPUT {1}.price, INPUT-OUTPUT {1}.pr-uom).
    
 