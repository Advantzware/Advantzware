/*------------------------------------------------------------------------
  File:         SchedShipvsQtyHand.p
  Description:  Business Logic
  Author:       Sewa Singh
  Date Created: 5.22.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttSchedShipvsQtyHand
DEFINE TEMP-TABLE ttSchedShipvsQtyHand NO-UNDO
    FIELD fgItem   AS CHARACTER FORMAT "x(15)"        LABEL "FG Item"
    FIELD relDate  AS DATE      FORMAT "99/99/9999"   LABEL "Release Date"
    FIELD req      AS INTEGER   FORMAT "->>>>>>>9"    LABEL "Required"
    FIELD variance AS INTEGER   FORMAT "->>>,>>>,>>9" LABEL "Variance"
    FIELD ordQty   AS INTEGER   FORMAT "->>>,>>>,>>9" LABEL "Ordered Qty"
    FIELD qtyHand  AS INTEGER   FORMAT "->>>,>>>,>>9" LABEL "On Hand Qty"
    FIELD netQty   AS INTEGER   FORMAT "->>>,>>>,>>9" LABEL "Net Qty"
    FIELD balShip  AS INTEGER   FORMAT "->>>,>>>,>>9" LABEL "Bal to Ship"
    FIELD openPo   AS INTEGER   FORMAT "->>>,>>>,>>9" LABEL "Open PO's"
    FIELD relNo    AS CHARACTER FORMAT "x(10)"        LABEL "Release"
    .
DEFINE TEMP-TABLE tt-report LIKE report
    FIELD required AS INTEGER
    FIELD variance AS INTEGER
    .
DEFINE NEW SHARED VARIABLE cocode AS CHARACTER FORMAT "x(3)" NO-UNDO.    

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 116
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE iBalShip  AS INTEGER FORMAT "->>>,>>>,>>9" INITIAL 0 NO-UNDO.
    DEFINE VARIABLE iNetQty   AS INTEGER FORMAT "->>>,>>>,>>9" INITIAL 0 NO-UNDO.
    DEFINE VARIABLE iQtyHand  AS INTEGER FORMAT "->>>,>>>,>>9" INITIAL 0 NO-UNDO. 
    DEFINE VARIABLE iQtyOrder AS INTEGER FORMAT "->>>,>>>,>>9" INITIAL 0 NO-UNDO.
    DEFINE VARIABLE iQtyPo    AS INTEGER FORMAT "->>>,>>>,>>9" INITIAL 0 NO-UNDO.
    DEFINE VARIABLE iRelQty LIKE iQtyOrder NO-UNDO.     
    DEFINE VARIABLE dHldQty   AS DECIMAL   NO-UNDO.    

    FOR EACH itemfg NO-LOCK
        WHERE itemfg.company EQ cCompany
          AND itemfg.i-no    GE cStartFGItem
          AND itemfg.i-no    LE cEndFGItem          
        :
        iQtyHand = 0.        
        FOR EACH fg-bin NO-LOCK
            WHERE fg-bin.company EQ cCompany
              AND fg-bin.i-no    EQ itemfg.i-no
            :
            iQtyHand = iQtyHand + fg-bin.qty.
        END.        
        iNetQty = iQtyHand.        
        ASSIGN
            iRelQty   = 0
            iQtyOrder = 0
            iBalShip  = 0
            .
        EMPTY TEMP-TABLE tt-report.        
        FOR EACH oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ cCompany
              AND oe-ordl.i-no    EQ itemfg.i-no
            USE-INDEX item,
            FIRST oe-ord OF oe-ordl NO-LOCK
            WHERE oe-ord.stat     NE "D"
              AND oe-ord.stat     NE "C"
              AND oe-ord.stat     NE "Z",
            EACH oe-rel NO-LOCK
            WHERE oe-rel.company  EQ cCompany
              AND oe-rel.ord-no   EQ oe-ord.ord-no
              AND oe-rel.i-no     EQ oe-ordl.i-no
              AND oe-rel.line     EQ oe-ordl.line
            BREAK BY oe-rel.rel-date
            :
            ASSIGN
                iBalShip  = iBalShip + oe-rel.qty
                iRelQty   = iRelQty + oe-rel.qty
                iQtyOrder = iQtyOrder + oe-rel.qty
                iNetQty   = iNetQty - oe-rel.qty
                .
            RELEASE oe-bolh.
            IF oe-rel.link-no NE 0 THEN
            FOR EACH oe-rell NO-LOCK
                WHERE oe-rell.company EQ oe-rel.company
                  AND oe-rell.ord-no  EQ oe-rel.ord-no
                  AND oe-rell.r-no    EQ oe-rel.link-no
                  AND oe-rell.i-no    EQ oe-rel.i-no
                  AND oe-rell.line    EQ oe-rel.line,
                FIRST oe-relh NO-LOCK
                WHERE oe-relh.r-no   EQ oe-rell.r-no
                  AND oe-relh.posted EQ YES,
                EACH oe-boll NO-LOCK
                WHERE oe-boll.company  EQ oe-rell.company
                  AND oe-boll.ord-no   EQ oe-rell.ord-no
                  AND oe-boll.line     EQ oe-rell.line
                  AND oe-boll.i-no     EQ oe-rell.i-no
                  AND oe-boll.r-no     EQ oe-rell.r-no
                  AND oe-boll.rel-no   EQ oe-rell.rel-no
                  AND oe-boll.b-ord-no EQ oe-rell.b-ord-no,
                FIRST oe-bolh NO-LOCK
                WHERE oe-bolh.b-no   EQ oe-boll.b-no
                  AND oe-bolh.posted EQ YES
                :
                LEAVE.
            END.
            IF AVAILABLE oe-bolh THEN
            ASSIGN
                iBalShip = iBalShip - oe-rell.qty
                iRelQty  = iRelQty - oe-rell.qty
                iNetQty  = iNetQty + oe-rell.qty
                .  
            IF LAST-OF(oe-rel.rel-date) AND
               oe-rel.rel-date GE dtStartShipDate AND
               oe-rel.rel-date LE dtEndShipDate AND
               iRelQty GT 0 THEN DO:
                CREATE tt-report.
                ASSIGN
                    tt-report.key-01   = STRING(YEAR(oe-rel.rel-date),"9999")
                                       + STRING(MONTH(oe-rel.rel-date),"99")
                                       + STRING(DAY(oe-rel.rel-date),"99")
                    tt-report.key-02   = STRING(MONTH(oe-rel.rel-date),"99") + "/"
                                       + STRING(DAY(oe-rel.rel-date),"99")   + "/"
                                       + STRING(YEAR(oe-rel.rel-date),"9999")
                    tt-report.key-03   = STRING(iRelQty,"-9999999999")
                    tt-report.key-04   = STRING(iNetQty,"-9999999999")
                    tt-report.required = iRelQty
                    tt-report.variance = iNetQty
                    tt-report.key-05   = STRING(oe-rel.i-no)
                    tt-report.key-06   = IF AVAILABLE oe-relh THEN STRING(oe-relh.release#) ELSE "0"
                    iRelQty = 0
                    .      
            END.
        END.
        iQtyPo = 0.
        FOR EACH po-ordl NO-LOCK
            WHERE po-ordl.company   EQ itemfg.company
              AND po-ordl.i-no      EQ itemfg.i-no
              AND po-ordl.item-type EQ NO
              AND lookup(po-ordl.stat,"O,P,U") GT 0
              AND po-ordl.t-rec-qty LT po-ordl.cons-qty,
            FIRST po-ord NO-LOCK
            WHERE po-ord.company EQ po-ordl.company
              AND po-ord.po-no   EQ po-ordl.po-no
              AND LOOKUP(po-ord.stat,"N,O,R,U") GT 0
              AND po-ord.po-date                GE dtStartShipDate
              AND po-ord.po-date                LE dtEndShipDate
            :
            IF po-ordl.cons-uom EQ "EA" THEN
            dHldQty = po-ordl.cons-qty.
            ELSE
            RUN sys/ref/convquom.p (
                po-ordl.cons-uom,
                "EA",
                0,
                0,
                0,
                0,
                po-ordl.cons-qty,
                OUTPUT dHldQty
                ).
            IF dHldQty - po-ordl.t-rec-qty GT 0 THEN
            iQtyPo = iQtyPo + (dHldQty - po-ordl.t-rec-qty).
        END.
        iNetQty = iNetQty + iQtyPo.     
        FOR EACH tt-report NO-LOCK:      
            CREATE ttSchedShipvsQtyHand.
            ASSIGN
                ttSchedShipvsQtyHand.fgItem   = itemfg.i-no
                ttSchedShipvsQtyHand.relDate  = DATE(tt-report.key-02)
                ttSchedShipvsQtyHand.req      = tt-report.required
                ttSchedShipvsQtyHand.variance = tt-report.variance
                ttSchedShipvsQtyHand.ordQty   = iQtyOrder
                ttSchedShipvsQtyHand.qtyHand  = (iQtyHand)
                ttSchedShipvsQtyHand.netQty   = (iNetQty)
                ttSchedShipvsQtyHand.balShip  = iBalShip
                ttSchedShipvsQtyHand.openPo   = iQtyPo
                ttSchedShipvsQtyHand.relNo    = tt-report.key-06            
                .
        END.
    END. /* each ttReport */
END PROCEDURE.
