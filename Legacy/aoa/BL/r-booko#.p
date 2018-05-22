/* r-booko#.p */

/* temp-table definitions */
{aoa/tempTable/ttOrdersBookedByOrderNo.i}

{sys/ref/CustList.i NEW}


/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttOrdersBookedByOrderNo.
{aoa/includes/pOrdersBookedByOrderNo.i}

DEFINE NEW SHARED VARIABLE cocode AS CHARACTER NO-UNDO.

/* local variables */
DEFINE VARIABLE lOrdQty         AS LOGICAL          NO-UNDO INITIAL YES.
DEFINE VARIABLE dTotOrd         AS DECIMAL          NO-UNDO EXTENT 2.
DEFINE VARIABLE dTaxRate        AS DECIMAL          NO-UNDO.
DEFINE VARIABLE lShip           AS LOGICAL          NO-UNDO INITIAL NO.
DEFINE VARIABLE dTotTax       LIKE oe-ord.tax       NO-UNDO.
DEFINE VARIABLE dTotFreight   LIKE oe-ord.t-freight NO-UNDO.
DEFINE VARIABLE iQtyLft       LIKE oe-ordl.qty      NO-UNDO.
DEFINE VARIABLE dExtPrice     LIKE oe-ordl.t-price  NO-UNDO.
DEFINE VARIABLE lPrtCont        AS LOGICAL          NO-UNDO INITIAL NO.
DEFINE VARIABLE dMargin         AS DECIMAL          NO-UNDO.
DEFINE VARIABLE dMarginTot      AS DECIMAL          NO-UNDO.
DEFINE VARIABLE dExtCost        AS DECIMAL          NO-UNDO.
DEFINE VARIABLE dOrderedMSF     AS DECIMAL          NO-UNDO.
DEFINE VARIABLE iJobShipQty     AS INTEGER          NO-UNDO.
DEFINE VARIABLE dBoardProfit    AS DECIMAL          NO-UNDO.
DEFINE VARIABLE iBoardPO        AS INTEGER          NO-UNDO.
DEFINE VARIABLE iBoardPOQty     AS INTEGER          NO-UNDO.
DEFINE VARIABLE dBoardCost      AS DECIMAL          NO-UNDO.
DEFINE VARIABLE dBoardTotalCost AS DECIMAL          NO-UNDO.
DEFINE VARIABLE iBoardTotalQty  AS INTEGER          NO-UNDO.
DEFINE VARIABLE dOrder%Profit   AS DECIMAL          NO-UNDO.
DEFINE VARIABLE dMSFRec         AS DECIMAL          NO-UNDO.
DEFINE VARIABLE dtFGShipDate    AS DATE             NO-UNDO.
DEFINE VARIABLE dtPORecDate     AS DATE             NO-UNDO.
DEFINE VARIABLE dFGExtPrice     AS DECIMAL          NO-UNDO.
DEFINE VARIABLE dPORecCost      AS DECIMAL          NO-UNDO.
DEFINE VARIABLE dProfitSold$    AS DECIMAL          NO-UNDO.
DEFINE VARIABLE dProfitSold%    AS DECIMAL          NO-UNDO.
DEFINE VARIABLE iUnitsBoard     AS INTEGER          NO-UNDO.
DEFINE VARIABLE dUnitLoss$      AS DECIMAL          NO-UNDO.
DEFINE VARIABLE dLoss%          AS DECIMAL          NO-UNDO.
DEFINE VARIABLE iBOL#           AS INTEGER          NO-UNDO.
DEFINE VARIABLE iInv#           AS INTEGER          NO-UNDO.
DEFINE VARIABLE hdPriceProcs  AS HANDLE.
{oe/ttPriceHold.i "NEW SHARED"}
RUN oe/PriceProcs.p PERSISTENT SET hdPriceProcs.

DEFINE BUFFER bOEOrdl FOR oe-ordl.

/* subject business logic */
cocode = ipcCompany.
FOR EACH oe-ord NO-LOCK
    WHERE oe-ord.company  EQ ipcCompany
      AND oe-ord.ord-no   GE iStartOrderNo
      AND oe-ord.ord-no   LE iEndOrderNo
      AND oe-ord.cust-no  GE cStartCustNo
      AND oe-ord.cust-no  LE cEndCustNo
      AND oe-ord.ord-date GE dtStartOrderDate
      AND oe-ord.ord-date LE dtEndOrderDate
      AND oe-ord.stat     NE "D"
      AND oe-ord.type     NE "T"
    USE-INDEX ord-no,
    FIRST bOEOrdl NO-LOCK
    WHERE bOEOrdl.company EQ ipcCompany
      AND bOEOrdl.ord-no  EQ oe-ord.ord-no
      AND bOEOrdl.i-no    GE cStartItemNo
      AND bOEOrdl.i-no    LE cEndItemNo,
    FIRST cust NO-LOCK
    WHERE (cust.company EQ ipcCompany) 
      AND cust.cust-no  EQ oe-ord.cust-no
    BY oe-ord.ord-no
    :
    IF lCustList AND
       NOT CAN-FIND(FIRST ttCustList
                    WHERE ttCustList.cust-no EQ oe-ord.cust-no
                      AND ttCustList.log-fld EQ TRUE) THEN
    NEXT.
    dTotOrd[1] = 0.

    FOR EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ oe-ord.company
          AND oe-ordl.ord-no  EQ oe-ord.ord-no
          AND oe-ordl.i-no    GE cStartItemNo
          AND oe-ordl.i-no    LE cEndItemNo
        BREAK BY oe-ordl.ord-no
        :
        ASSIGN
            lShip      = oe-ordl.stat NE "I" AND oe-ordl.stat NE "B"
            iQtyLft   = oe-ordl.qty - (IF lOrdQty THEN 0 ELSE oe-ordl.inv-qty)
            dExtPrice = 0
            .
        IF iQtyLft LT 0 THEN iQtyLft = 0.
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ ipcCompany
               AND itemfg.i-no EQ oe-ordl.i-no
             NO-ERROR.
        RUN GetPriceTotal IN hdPriceProcs (oe-ordl.qty,
                                       oe-ordl.price,
                                       oe-ordl.pr-uom,
                                       itemfg.case-count,
                                       ( IF AVAIL itemfg THEN itemfg.case-count ELSE 0),
                                       OUTPUT dExtPrice).
                                        
        
        dTotFreight = dTotFreight
                       + (ROUND(oe-ordl.t-freight / oe-ordl.qty, 2)
                       * iQtyLft).
        /** CALCULATE TAX CHARGES **/
        IF oe-ordl.tax AND dTaxRate GT 0 THEN
        dTotTax = dTotTax + ROUND((dExtPrice * dTaxRate) / 100,2).
        /*if lPrtCont then */
        ASSIGN
            dExtCost        = (oe-ordl.cost * oe-ordl.qty) / 1000
            dMargin         = dExtPrice - dExtCost
            iBoardTotalQty  = 0
            iBoardPO        = 0
            iBoardPOQty     = 0
            dBoardCost      = 0
            dBoardTotalCost = 0
            dBoardProfit    = 0
            dOrderedMSF     = 0
            .
        IF oe-ordl.po-no-po NE 0 THEN DO:
            FIND FIRST po-ordl NO-LOCK
                 WHERE po-ordl.company      EQ oe-ordl.company 
                   AND po-ordl.po-no        EQ oe-ordl.po-no-po
                   AND ((po-ordl.item-type  EQ YES
                   AND TRIM(oe-ordl.job-no) NE ""
                   AND po-ordl.job-no       EQ oe-ordl.job-no
                   AND po-ordl.job-no2      EQ oe-ordl.job-no2)   
                    OR (po-ordl.item-type   EQ NO
                   AND po-ordl.i-no         EQ oe-ordl.i-no))
                 NO-ERROR.
            ASSIGN
                iBoardTotalQty  = IF AVAILABLE po-ordl THEN po-ordl.t-rec-qty ELSE 0
                iBoardPO        = IF AVAILABLE oe-ordl THEN oe-ordl.po-no-po  ELSE 0
                iBoardPOQty     = IF AVAILABLE po-ordl THEN po-ordl.ord-qty   ELSE 0
                dBoardCost      = IF AVAILABLE po-ordl THEN po-ordl.cost      ELSE 0
                dBoardTotalCost = IF AVAILABLE po-ordl THEN po-ordl.t-cost    ELSE 0
                .
           RUN pCalcPOMSF (ipcCompany,OUTPUT dOrderedMSF).
           IF dOrderedMSF EQ ? THEN dOrderedMSF = 0.
           dtPORecDate = ?.

           IF oe-ordl.po-no-po NE 0 THEN
           FOR EACH fg-rcpth NO-LOCK
               WHERE fg-rcpth.company      EQ oe-ordl.company 
                 AND fg-rcpth.rita-code    EQ "R"
                 AND ((fg-rcpth.trans-date GE dtStartReceiptDate 
                 AND fg-rcpth.trans-date   LE dtStartReceiptDate AND lUseReceiptDate) OR NOT lUseReceiptDate)
                 AND fg-rcpth.po-no        EQ STRING(oe-ordl.po-no-po),
               EACH fg-rdtlh NO-LOCK
               WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                 AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
               BY fg-rcpth.trans-date
               :
               ASSIGN dtPORecDate = fg-rcpth.trans-date.
               LEAVE.
           END.  /* end of for each fg-rcpth */
        END.  /*IF oe-ordl.po-no-po*/
        dBoardProfit = dExtPrice - dBoardTotalCost.

        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ ipcCompany 
               AND itemfg.i-no    EQ oe-ordl.i-no
             NO-ERROR.
        ASSIGN
            iJobShipQty  = 0
            dtFGShipDate = ?
            .
        IF oe-ordl.job-no NE "" THEN
        FOR EACH fg-rcpth OF itemfg NO-LOCK
            WHERE fg-rcpth.rita-code    EQ "S"
              AND fg-rcpth.job-no       EQ oe-ordl.job-no 
              AND fg-rcpth.job-no2      EQ oe-ordl.job-no2
              AND ((fg-rcpth.trans-date GE dtStartShipDate
              AND fg-rcpth.trans-date   LE dtEndShipDate
              AND lUseShipDate)
               OR NOT lUseShipDate),
            EACH fg-rdtlh NO-LOCK
            WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
              AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code 
            BY fg-rcpth.trans-date
            : 
            ASSIGN
                iJobShipQty  = iJobShipQty + fg-rdtlh.qty
                dtFGShipDate = IF dtFGShipDate EQ ? THEN fg-rcpth.trans-date
                                                        ELSE dtFGShipDate
                .
        END.  /* end of for each fg-rcpth */ 

        IF (oe-ordl.po-no-po NE 0 AND
            NOT CAN-FIND(FIRST fg-rcpth
                         WHERE fg-rcpth.company    EQ oe-ordl.company 
                           AND fg-rcpth.rita-code  EQ "R"
                           AND fg-rcpth.trans-date GE dtStartReceiptDate
                           AND fg-rcpth.trans-date LE dtEndReceiptDate
                           AND fg-rcpth.po-no      EQ STRING(oe-ordl.po-no-po))
                           AND lUseReceiptDate)
                            OR (oe-ordl.job-no     NE "" AND 
            NOT CAN-FIND(FIRST fg-rcpth OF itemfg
                         WHERE fg-rcpth.rita-code  EQ "S"
                           AND fg-rcpth.job-no     EQ oe-ordl.job-no 
                           AND fg-rcpth.job-no2    EQ oe-ordl.job-no2
                           AND fg-rcpth.trans-date GE dtStartShipDate
                           AND fg-rcpth.trans-date LE dtEndShipDate)
                           AND lUseShipDate) THEN NEXT.
        /* ==== new for Selectable columns =====*/
        FIND FIRST oe-boll NO-LOCK
             WHERE oe-boll.company EQ oe-ordl.company 
               AND oe-boll.ord-no  EQ oe-ordl.ord-no
               AND oe-boll.i-no    EQ oe-ordl.i-no
             NO-ERROR.
        iBOL# = IF AVAILABLE oe-boll THEN oe-boll.bol-no ELSE 0.

        FIND FIRST ar-invl NO-LOCK
             WHERE ar-invl.company EQ oe-ordl.company
               AND ar-invl.ord-no  EQ oe-ordl.ord-no
              AND ar-invl.i-no     EQ oe-ordl.i-no
             NO-ERROR.
        iInv# = IF AVAILABLE ar-invl THEN ar-invl.inv-no ELSE 0.

        IF iInv# EQ 0  THEN DO:
            FIND FIRST inv-line NO-LOCK
                 WHERE inv-line.company EQ oe-ordl.company
                   AND inv-line.ord-no  EQ oe-ordl.ord-no
                   AND inv-line.i-no    EQ oe-ordl.i-no
                 NO-ERROR.
            iInv# = IF AVAILABLE inv-line THEN inv-line.inv-no ELSE 0.
        END.  /*IF iInv# = 0 */
        ASSIGN
            dOrder%Profit = dBoardProfit / dExtPrice
            dMSFRec       = dOrderedMSF / iBoardPOQty * iBoardTotalQty              
            dFGExtPrice   = oe-ordl.price / 1000 * iJobShipQty
            dPORecCost    = dBoardTotalCost / iBoardPOQty * iBoardTotalQty
            dProfitSold$  = dFGExtPrice - dPORecCost 
            dProfitSold%  = dProfitSold$ / dFGExtPrice
            iUnitsBoard   = iQtyLft / iBoardPOQty
            dUnitLoss$    = iBoardTotalQty * iUnitsBoard - iJobShipQty
            dLoss%        = dUnitLoss$ / (iBoardTotalQty * iUnitsBoard )
            .
        IF dOrder%Profit EQ ? THEN dOrder%Profit = 0.
        IF dMSFRec       EQ ? THEN dMSFRec       = 0.
        IF dPORecCost    EQ ? THEN dPORecCost    = 0.
        IF dProfitSold$  EQ ? THEN dProfitSold$  = 0.
        IF dProfitSold%  EQ ? THEN dProfitSold%  = 0.
        IF iUnitsBoard   EQ ? THEN iUnitsBoard   = 0.
        IF dUnitLoss$    EQ ? THEN dUnitLoss$    = 0.
        IF dLoss%        EQ ? THEN dLoss%        = 0.

        CREATE ttOrdersBookedByOrderNo.
        ASSIGN
            ttOrdersBookedByOrderNo.orderNo      = oe-ord.ord-no         
            ttOrdersBookedByOrderNo.estNo        = oe-ordl.est-no        
            ttOrdersBookedByOrderNo.jobNo        = oe-ordl.job-no        
            ttOrdersBookedByOrderNo.orddate      = oe-ord.ord-date       
            ttOrdersBookedByOrderNo.custNo       = oe-ord.cust-no        
            ttOrdersBookedByOrderNo.custName     = oe-ord.cust-name      
            ttOrdersBookedByOrderNo.fgItem       = oe-ordl.i-no          
            ttOrdersBookedByOrderNo.fgItemName   = oe-ordl.i-name        
            ttOrdersBookedByOrderNo.fgOrderQty   = iQtyLft             
            ttOrdersBookedByOrderNo.fgCost       = oe-ordl.cost          
            ttOrdersBookedByOrderNo.price        = oe-ordl.price         
            ttOrdersBookedByOrderNo.uom          = oe-ordl.pr-uom        
            ttOrdersBookedByOrderNo.extPrice     = dExtPrice           
            ttOrdersBookedByOrderNo.fgItemProfit = dMargin              
            ttOrdersBookedByOrderNo.poMsf        = dOrderedMSF          
            ttOrdersBookedByOrderNo.fgShipped    = iJobShipQty          
            ttOrdersBookedByOrderNo.poProfit     = dBoardProfit         
            ttOrdersBookedByOrderNo.poNo         = iBoardPO             
            ttOrdersBookedByOrderNo.poQty        = iBoardPOQty          
            ttOrdersBookedByOrderNo.poCost       = dBoardCost           
            ttOrdersBookedByOrderNo.poTotalCost  = dBoardTotalCost      
            ttOrdersBookedByOrderNo.poReceived   = iBoardTotalQty       
            ttOrdersBookedByOrderNo.orderProfit  = dOrder%Profit        
            ttOrdersBookedByOrderNo.msfReceived  = dMSFRec              
            ttOrdersBookedByOrderNo.fgShipDate   = dtFGShipDate          
            ttOrdersBookedByOrderNo.poRecDate    = dtPORecDate           
            ttOrdersBookedByOrderNo.fgExtPrice   = dFGExtPrice          
            ttOrdersBookedByOrderNo.poRecCost    = dPORecCost           
            ttOrdersBookedByOrderNo.profSold     = dProfitSold$         
            ttOrdersBookedByOrderNo.profSoldp    = dProfitSold%         
            ttOrdersBookedByOrderNo.unitBoard    = iUnitsBoard          
            ttOrdersBookedByOrderNo.unitWaste    = dUnitLoss$           
            ttOrdersBookedByOrderNo.lossp        = dLoss%               
            ttOrdersBookedByOrderNo.bolNo        = iBOL#                
            ttOrdersBookedByOrderNo.invoiceNo    = iInv#
            .
        /*===== end of new ===== */
        IF lPrtCont THEN
            IF dMargin NE ? THEN
            dMarginTot = dMarginTot + dMargin.
        dTotOrd[1] = dTotOrd[1] + dExtPrice.
    END.  /* each oe-ordl */

    IF lPrintMiscCharges THEN
    FOR EACH oe-ordm NO-LOCK
        WHERE oe-ordm.company EQ oe-ord.company
          AND oe-ordm.ord-no  EQ oe-ord.ord-no
        BREAK BY oe-ordm.ord-no
        :
        IF oe-ordm.bill EQ "Y" THEN DO:
            dTotOrd[1] = dTotOrd[1] + oe-ordm.amt.
            IF oe-ordm.tax AND dTaxRate EQ 0 THEN
            dTotTax = dTotTax + ROUND((oe-ordm.amt * dTaxRate) / 100,2).
        END.  /*IF oe-ordm.tax*/
    END.  /* each oe-ordm */   

    dTotOrd[2] = dTotOrd[2] + dTotOrd[1].    
END.  /* each oe-ord */ 

PROCEDURE pCalcPOMSF:
/*------------------------------------------------------------------------------
  Purpose:     Orders Booked by Order No.rpa
  Parameters:  Company, output Total MSF
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opTotalMsf AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE dBasisW    AS DECIMAL            NO-UNDO. /* for po/po-adder2.p */
    DEFINE VARIABLE dLength  LIKE po-ordl.s-len      NO-UNDO.
    DEFINE VARIABLE dWidth   LIKE po-ordl.s-wid      NO-UNDO.
    DEFINE VARIABLE dv-dep   LIKE po-ordl.s-len      NO-UNDO.
    DEFINE VARIABLE iOrdQty  LIKE po-ordl.ord-qty    NO-UNDO.
    DEFINE VARIABLE cOrigUOM   AS CHARACTER          NO-UNDO.
    DEFINE VARIABLE factor#    AS DECIMAL            NO-UNDO.
    DEFINE VARIABLE lEach      AS LOGICAL            NO-UNDO INITIAL NO.
    DEFINE VARIABLE iUOM     LIKE po-ordl.pr-qty-uom NO-UNDO INITIAL NO.
    DEFINE VARIABLE cFGUOMList AS CHARACTER          NO-UNDO.
    DEFINE VARIABLE iOutQty    AS INTEGER            NO-UNDO.
    DEFINE VARIABLE cocode     AS CHARACTER          NO-UNDO.
    
    IF NOT AVAILABLE po-ordl THEN RETURN.

    FIND FIRST sys-ctrl NO-LOCK
         WHERE sys-ctrl.company EQ ipcCompany
           AND sys-ctrl.name    EQ "poprint" 
         NO-ERROR.
    factor# = IF AVAILABLE sys-ctrl AND CAN-DO("Premier,Middlesx,16th's",sys-ctrl.char-fld) THEN .16 ELSE 1.
    cocode = ipcCompany.
    {ce/msfcalc.i}
    
    FIND FIRST item NO-LOCK
         WHERE item.company EQ oe-ordl.company
           AND item.i-no    EQ po-ordl.i-no
         NO-ERROR.
    ASSIGN
        dBasisW  = IF AVAILABLE item THEN item.basis-w ELSE dBasisW
        dv-dep   = IF AVAILABLE item THEN item.s-dep   ELSE dv-dep
        dLength  = po-ordl.s-len
        dWidth   = po-ordl.s-wid
        iOrdQty  = po-ordl.ord-qty
        cOrigUOM = po-ordl.pr-qty-uom 
        {po/calc10.i dLength} 
        {po/calc10.i dWidth}
        .
    IF NOT AVAILABLE item THEN
    FIND FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ ipcCompany
           AND itemfg.i-no    EQ po-ordl.i-no
         NO-ERROR.
    IF AVAILABLE itemfg THEN
    RUN sys/ref/ea-um-fg.p (po-ordl.pr-qty-uom, OUTPUT lEach).
    IF lEach THEN ASSIGN iUOM = po-ordl.pr-qty-uom. 

    IF dLength EQ 0 AND AVAILABLE item AND
       ITEM.i-code EQ "R" AND item.r-wid GT 0 THEN DO:
        dLength = 12.
        IF cOrigUOM EQ "ROLL" THEN DO:
            FIND FIRST uom NO-LOCK
                 WHERE uom.uom EQ "ROLL"
                 NO-ERROR.
            IF AVAILABLE uom THEN
            iOrdQty = iOrdQty * uom.mult.
        END.  /*IF cOrigUOM*/ 
    END.  /*IF dLength EQ 0*/

    RUN sys/ref/uom-fg.p (?, OUTPUT cFGUOMList).

    IF po-ordl.pr-qty-uom{2} EQ "EA" OR
      (NOT po-ordl.item-type AND
       LOOKUP(po-ordl.pr-qty-uom,cFGUOMList) GT 0) THEN
    opTotalMsf = IF v-corr THEN ((dLength * dWidth * .007 * DEC(po-ordl.ord-qty{2})) / 1000)
                 ELSE ((((dLength * dWidth) / 144) * DEC(po-ordl.ord-qty{2})) / 1000).
    ELSE DO:
        /*convert whatever the UOM is into "EACH" first*/
        opTotalMsf = 0.
        IF po-ordl.pr-qty-uom NE "EA" THEN DO:
            opTotalMsf = 0.
            RUN sys/ref/convquom.p
                (po-ordl.pr-qty-uom,
                 "EA",
                 dBasisW,
                 dLength,
                 dWidth,
                 dv-dep,
                 iOrdQty,
                 OUTPUT iOutQty
                 ).
            /*now convert from "EACH" into MSF*/   
            opTotalMsf = IF v-corr THEN ((dLength * dWidth * .007 * iOutQty) / 1000)
                         ELSE ((((dLength * dWidth) / 144) * iOutQty) / 1000).
            IF po-ordl.pr-qty-uom EQ "ROLL" THEN
            opTotalMsf = OpTotalMsf * (12 / dLength).
        END. /* if po-ordl.pr-qty-uom ne */
    END. /* else */ 
END PROCEDURE.

{aoa/BL/pBuildCustList.i}
