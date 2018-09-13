/*------------------------------------------------------------------------
  File: r-fgobb.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Customer Inventory.rpa */
{aoa/tempTable/ttInventoryValue.i}

{sys/ref/CustList.i NEW}

&SCOPED-DEFINE itemfg-index i-no job-no job-no2 loc loc-bin tag bin-cust-no
DEFINE NEW SHARED TEMP-TABLE tt-itemfg NO-UNDO
    FIELD row-id      AS ROWID
    FIELD i-no        LIKE itemfg.i-no
    FIELD i-name      LIKE itemfg.i-name
    FIELD cust-no     LIKE itemfg.cust-no
    FIELD part-no     LIKE itemfg.part-no
    FIELD part-cust   AS CHARACTER
    FIELD procat      LIKE itemfg.procat
    FIELD job-no      LIKE fg-rcpth.job-no
    FIELD job-no2     LIKE fg-rcpth.job-no2
    FIELD loc         LIKE fg-rdtlh.loc
    FIELD loc-bin     LIKE fg-rdtlh.loc-bin
    FIELD tag         LIKE fg-rdtlh.tag
    FIELD bin-cust-no LIKE fg-rdtlh.cust-no
    FIELD loc-bin-tag AS CHARACTER
        INDEX i-no                    {&itemfg-index}
        INDEX cust-no     cust-no     {&itemfg-index}
        INDEX partNo      part-cust   {&itemfg-index}
        INDEX procat      procat      {&itemfg-index}
        INDEX loc-bin-tag loc-bin-tag {&itemfg-index}
        .
{fg/rep/tt-fgbin.i NEW SHARED}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttInventoryValue.
{aoa/includes/pInventoryValue.i}

/* local variables */
DEFINE VARIABLE lProcessRel      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lProcessLastSale AS LOGICAL   NO-UNDO.

/* subject business logic */
ASSIGN
    lProcessLastSale = CAN-DO(cSelectedColumns,"lastSale")
    lProcessRel      = CAN-DO(cSelectedColumns,"relQty") OR
                       CAN-DO(cSelectedColumns,"relPO")
    .

EMPTY TEMP-TABLE tt-fg-bin.
EMPTY TEMP-TABLE tt-itemfg.

FOR EACH itemfg NO-LOCK
    WHERE itemfg.company EQ ipcCompany
      AND itemfg.cust-no GE cStartCustNo
      AND itemfg.cust-no LE cEndCustNo
      AND itemfg.i-no    GE cStartItemNo
      AND itemfg.i-no    LE cEndItemNo
      AND itemfg.procat  GE cStartProdCategory
      AND itemfg.procat  LE cEndProdCategory
      AND (itemfg.i-code EQ cItemCode OR cItemCode EQ "A")
      AND (itemfg.stat EQ "A" OR lIncludeInactiveItems)
      AND (NOT lPrintSetandComponentsOnly OR itemfg.isaset
       OR CAN-FIND(FIRST fg-set
                   WHERE fg-set.company EQ itemfg.company
                     AND fg-set.part-no EQ itemfg.i-no))
    USE-INDEX customer
    :
    IF lCustList AND
       NOT CAN-FIND(FIRST ttCustList
                    WHERE ttCustList.cust-no EQ itemfg.cust-no
                      AND ttCustList.log-fld EQ TRUE) THEN
    NEXT.
    RUN fg/rep/tt-fgbin.p
        (BUFFER itemfg,
         dtAsOfDate, "", "zzzzzzzzzz",
         cStartLoc, cEndLoc, cStartLocBin, cEndLocBin,
         lIncludeZeroBalance, iShowQOHOlderThanDays, YES,
         lIncludeCustomerOwnerdWarehouse).
    
    FOR EACH tt-fg-bin
        WHERE tt-fg-bin.company EQ itemfg.company
          AND tt-fg-bin.i-no    EQ itemfg.i-no
          AND (lIncludeCustomerOwnerdWarehouse OR lOnlyCustomerOwnedWarehouse
           OR (tt-fg-bin.cust-no EQ "" AND tt-fg-bin.loc NE "CUST"))
          AND (NOT lOnlyCustomerOwnedWarehouse
           OR (tt-fg-bin.cust-no NE "" OR tt-fg-bin.loc EQ "CUST"))
        USE-INDEX co-ino
        :
        IF tt-fg-bin.qty NE 0 OR lIncludeZeroBalance THEN DO:
            CREATE tt-itemfg.
            BUFFER-COPY itemfg TO tt-itemfg
            ASSIGN
                tt-itemfg.row-id      = ROWID(itemfg) 
                tt-itemfg.job-no      = tt-fg-bin.job-no
                tt-itemfg.job-no2     = tt-fg-bin.job-no2
                tt-itemfg.loc         = tt-fg-bin.loc
                tt-itemfg.loc-bin     = tt-fg-bin.loc-bin
                tt-itemfg.tag         = tt-fg-bin.tag
                tt-itemfg.bin-cust-no = tt-fg-bin.cust-no
                tt-itemfg.part-cust   = STRING(tt-itemfg.part-no,"x(20)")
                                      + STRING(tt-itemfg.cust-no,"x(20)") 
                tt-itemfg.loc-bin-tag = STRING(tt-itemfg.loc,"x(10)")
                                      + STRING(tt-itemfg.loc-bin,"x(10)")
                                      + STRING(tt-itemfg.tag,"x(20)")
                . 
        END. /* if qty ne 0 */
        ELSE DELETE tt-fg-bin.
    END. /* each tt-fg-bin */
END. /* each itemfg */

RUN pInventoryValue
    (ipcCompany,
     cStartSalesRep,
     cEndSalesRep,
     cSort,
     lPrintSummaryByBinQty,
     lIncludeZeroBalance,
     dtAsOfDate,
     lProcessRel,
     lProcessLastSale
     ).

PROCEDURE pInventoryValue:
    DEFINE INPUT PARAMETER ipcCompany              AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcStartSalesRep        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEndSalesRep          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcSort                 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPrintSummaryByBinQty AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeZeroBalance   AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOfDate            AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER iplProcessRel           AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplProcessLastSale      AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE cCustNo          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustName        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSalesRep        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFGLotVal        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFirst           AS LOGICAL   NO-UNDO EXTENT 2.
    DEFINE VARIABLE dTotSum          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dExtSum          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iQOH             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cProCat          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lBin             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dTotBinSum       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dExtBinSum       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iBinQOH          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBinMSF          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iBinArq          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOrdPrice        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dtRctDate        AS DATE      NO-UNDO.
    DEFINE VARIABLE dCostL           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostM           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCost1           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCost            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSellPriceFG     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSellPrice       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cSellUOM         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSellUOMFG       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCaseCount       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSellPriceOrd    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSellValueOrd    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSellValueFG     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cSellUOMOrd      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFoundJob        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dExt             AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSellValueFGs    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSellValueOrds   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dMSFOH           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotQty          AS DECIMAL   NO-UNDO EXTENT 3.
    DEFINE VARIABLE dTotMSF          AS DECIMAL   NO-UNDO EXTENT 3.
    DEFINE VARIABLE dTotCst          AS DECIMAL   NO-UNDO EXTENT 3.
    DEFINE VARIABLE dTotExt          AS DECIMAL   NO-UNDO EXTENT 3.
    DEFINE VARIABLE dTotMat          AS DECIMAL   NO-UNDO EXTENT 3.
    DEFINE VARIABLE dTotLab          AS DECIMAL   NO-UNDO EXTENT 3.
    DEFINE VARIABLE dTotFGSell       AS DECIMAL   NO-UNDO EXTENT 3.
    DEFINE VARIABLE dTotOrdSell      AS DECIMAL   NO-UNDO EXTENT 3.
    DEFINE VARIABLE cJobNo           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPONo            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPOOrd           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQOHf            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQOHs            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iArq             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dtLastInv        AS DATE      NO-UNDO.
    DEFINE VARIABLE cPORel           AS CHARACTER NO-UNDO.

    FOR EACH tt-itemfg USE-INDEX cust-No NO-LOCK,    
        FIRST itemfg NO-LOCK
        WHERE ROWID(itemfg) EQ tt-itemfg.row-id,
        EACH tt-fg-bin
        WHERE tt-fg-bin.company  EQ itemfg.company
          AND tt-fg-bin.i-no     EQ itemfg.i-no
          AND tt-fg-bin.job-no   EQ tt-itemfg.job-no
          AND tt-fg-bin.job-no2  EQ tt-itemfg.job-no2
          AND tt-fg-bin.loc      EQ tt-itemfg.loc
          AND tt-fg-bin.loc-bin  EQ tt-itemfg.loc-bin
          AND tt-fg-bin.tag      EQ tt-itemfg.tag
          AND tt-fg-bin.cust-no  EQ tt-itemfg.bin-cust-no
          AND ((ipcStartSalesRep EQ ""
          AND ipcEndSalesRep     BEGINS "zzz")
           OR  CAN-FIND(FIRST cust
                        WHERE cust.company EQ itemfg.company
                          AND cust.sman    GE ipcStartsalesRep
                          AND cust.sman    LE ipcEndSalesRep
                          AND cust.cust-no EQ itemfg.cust-no))
        USE-INDEX co-ino
        BREAK BY tt-itemfg.cust-no
              BY tt-itemfg.i-no
              BY tt-itemfg.loc
              BY tt-itemfg.loc-bin
              BY tt-itemfg.job-no
              BY tt-itemfg.job-no2
        :

        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ itemfg.company
               AND cust.cust-no EQ itemfg.cust-no
             NO-ERROR.
   
        ASSIGN
            cSalesRep = ""
            cCustNo   = itemfg.cust-no
            cCustName = IF AVAILABLE cust THEN cust.name ELSE ""
            .
                                                              
        IF AVAILABLE cust AND cust.active NE "X" THEN DO:
            FOR EACH cust-part NO-LOCK
                WHERE cust-part.company EQ itemfg.company
                  AND cust-part.i-no    EQ itemfg.i-no
                  AND cust-part.cust-no EQ cust.cust-no:
                IF cust-part.spare-char-1 NE "" THEN DO:
                    FIND FIRST sman NO-LOCK
                         WHERE sman.company EQ itemfg.company
                           AND sman.sman    EQ cust-part.spare-char-1
                         NO-ERROR.
                    IF AVAILABLE sman THEN cSalesRep = sman.sman.
                    LEAVE .
                END.
            END. /* each cust-part */
         
            IF AVAILABLE cust AND cSalesRep EQ "" THEN DO:
                FIND FIRST sman NO-LOCK
                     WHERE sman.company EQ cust.company
                       AND sman.sman    EQ cust.sman
                     NO-ERROR.
                IF AVAILABLE sman THEN cSalesRep = sman.sman.
            END.
        END.
        ELSE DO:
            FIND FIRST sman NO-LOCK
                 WHERE sman.company EQ cust.company
                   AND sman.sman    EQ cust.sman
                 NO-ERROR.
            IF AVAILABLE sman THEN cSalesRep = sman.sman.
        END. /* else */

        FIND FIRST fg-rdtlh NO-LOCK
             WHERE fg-rdtlh.company    EQ ipcCompany
               AND fg-rdtlh.i-no       EQ tt-fg-bin.i-no
               AND fg-rdtlh.job-no     EQ tt-fg-bin.job-no
               AND fg-rdtlh.job-no2    EQ INTEGER(tt-fg-bin.job-no2)
               AND fg-rdtlh.loc        EQ tt-fg-bin.loc
               AND fg-rdtlh.loc-bin    EQ tt-fg-bin.loc-bin
               AND fg-rdtlh.tag        EQ tt-fg-bin.tag 
               AND fg-rdtlh.stack-code NE ""
             NO-ERROR.
        IF NOT AVAILABLE fg-rdtlh THEN
        FIND FIRST fg-rdtlh
             WHERE fg-rdtlh.company    EQ ipcCompany
               AND fg-rdtlh.i-no       EQ tt-fg-bin.i-no
               AND fg-rdtlh.job-no     EQ tt-fg-bin.job-no
               AND fg-rdtlh.job-no2    EQ INT(tt-fg-bin.job-no2)
               AND fg-rdtlh.loc        EQ tt-fg-bin.loc
               AND fg-rdtlh.loc-bin    EQ tt-fg-bin.loc-bin
               AND fg-rdtlh.stack-code NE ""
             NO-ERROR.
        
        cFGLotVal = IF AVAILABLE fg-rdtlh THEN fg-rdtlh.stack-code ELSE "".
        
        IF FIRST-OF(tt-itemfg.cust-no) THEN lFirst[2] = YES.
     
        IF FIRST-OF(tt-itemfg.i-no) THEN DO:
            ASSIGN
                lFirst[1] = YES
                dTotSum   = 0
                dExtSum   = 0
                iQOH      = 0
                .

            IF ipcSort EQ "Wh" THEN lFirst[2] = YES.
        END. /* first-of */

        ASSIGN
            cProCat = itemfg.procat
            lBin    = NO
            .

        IF iplPrintSummaryByBinQty AND FIRST-OF(tt-itemfg.job-no2) THEN
        ASSIGN 
            dTotBinSum = 0
            dExtBinSum = 0
            iBinQOH    = 0
            iBinMSF    = 0
            iBinArq    = 0
            dOrdPrice  = 0
            .

        ASSIGN
            dtRctDate = tt-fg-bin.first-date
            dCostL    = tt-fg-bin.std-lab-Cost * tt-fg-bin.qty
            dCostM    = tt-fg-bin.std-mat-Cost * tt-fg-bin.qty 
            dCost1    = tt-fg-bin.std-tot-cost
            dCost     = dCost1 * tt-fg-bin.qty
            .

        /* Calculate Cost */
        IF tt-fg-bin.pur-uom EQ "CS" AND tt-fg-bin.case-count NE 0 THEN
        ASSIGN
            dCostL = dCostL / tt-fg-bin.case-count
            dCostM = dCostM / tt-fg-bin.case-count
            dCost  = dCost  / tt-fg-bin.case-count
            .
        ELSE
        IF tt-fg-bin.pur-uom EQ "L" THEN
        ASSIGN
            dCostL = dCostL / tt-fg-bin.qty
            dCostM = dCostM / tt-fg-bin.qty
            dCost  = dCostM / tt-fg-bin.qty
            .
        ELSE DO:
           FIND FIRST uom NO-LOCK
                WHERE uom.uom  EQ itemfg.prod-uom
                  AND uom.mult NE 0
                NO-ERROR.
           IF AVAILABLE uom THEN
           ASSIGN
               dCostL = dCostL / uom.mult
               dCostM = dCostM / uom.mult
               dCost  = dCost  / uom.mult
               .
           ELSE
           ASSIGN
               dCostL = dCostL / 1000
               dCostM = dCostM / 1000
               dCost  = dCost  / 1000
               .
        END. /* else */

        ASSIGN
            dSellPriceFG  = itemfg.sell-Price
            dSellPrice    = itemfg.sell-Price
            cSellUOM      = itemfg.sell-uom
            cSellUOMFG    = itemfg.sell-uom
            dCaseCount    = itemfg.case-count
            dSellPriceOrd = 0
            dSellValueOrd = 0
            dSellValueFG  = 0
            .

        IF tt-fg-bin.po-no NE "" THEN DO:
            FIND FIRST po-ordl NO-LOCK
                 WHERE po-ordl.company EQ tt-fg-bin.company
                   AND po-ordl.po-no   EQ INTEGER(tt-fg-bin.po-no)
                   AND po-ordl.i-no    EQ tt-fg-bin.i-no
                 NO-ERROR.
            IF AVAILABLE po-ordl THEN DO:
                FIND LAST oe-ordl NO-LOCK
                     WHERE oe-ordl.company EQ tt-fg-bin.company
                       AND oe-ordl.ord-no  EQ po-ordl.ord-no
                       AND oe-ordl.i-no    EQ tt-fg-bin.i-no
                       AND (oe-ordl.pr-uom NE "CS"
                        OR oe-ordl.cas-cnt NE 0)
                     NO-ERROR.
                IF AVAILABLE oe-ordl THEN
                ASSIGN
                    dSellPriceOrd = oe-ordl.price * (1 - (oe-ordl.disc / 100))
                    cSellUOMOrd   = oe-ordl.pr-uom
                    dCaseCount    = oe-ordl.cas-cnt
                    .
            END. /* avail po-ordl */
        END. /* po-no ne "" */
        ELSE
        IF TRIM(tt-fg-bin.job-no) NE "" THEN DO:
            lFoundJob = NO.
            FOR EACH job-hdr FIELDS(ord-no company i-No) NO-LOCK
                WHERE job-hdr.company EQ tt-fg-bin.company
                  AND job-hdr.job-no  EQ tt-fg-bin.job-no
                  AND job-hdr.job-no2 EQ tt-fg-bin.job-no2
                  AND job-hdr.i-no    EQ tt-fg-bin.i-no
                  AND job-hdr.ord-no  NE 0
                USE-INDEX job-no,
                FIRST oe-ordl FIELDS(price pr-uom cas-cnt disc) NO-LOCK
                WHERE oe-ordl.company EQ job-hdr.company
                  AND oe-ordl.ord-no  EQ job-hdr.ord-no
                  AND oe-ordl.i-no    EQ job-hdr.i-no
                  AND (oe-ordl.pr-uom NE "CS"
                   OR oe-ordl.cas-cnt NE 0)
                USE-INDEX item-ord
                BY job-hdr.ord-no DESCENDING
                :
                ASSIGN
                    dSellPriceOrd = oe-ordl.price * (1 - (oe-ordl.disc / 100))
                    cSellUOMOrd   = oe-ordl.pr-uom
                    dCaseCount    = oe-ordl.cas-cnt
                    lFoundJob     = YES
                    .
                    LEAVE.
            END. /* each job-hdr */
            IF lFoundJob EQ NO THEN DO:
                FIND LAST oe-ordl NO-LOCK
                     WHERE oe-ordl.company EQ tt-fg-bin.company
                       AND oe-ordl.job-no  EQ tt-fg-bin.job-no
                       AND oe-ordl.job-no2 EQ tt-fg-bin.job-no2
                       AND oe-ordl.i-no EQ tt-fg-bin.i-no
                       AND (oe-ordl.pr-uom NE "CS"
                        OR oe-ordl.cas-cnt NE 0)
                     NO-ERROR.
                IF AVAILABLE oe-ordl THEN
                ASSIGN
                    dSellPriceOrd = oe-ordl.price * (1 - (oe-ordl.disc / 100))
                    cSellUOMOrd   = oe-ordl.pr-uom
                    dCaseCount    = oe-ordl.cas-cnt
                    .
            END. /* if found */
        END. /* if trim */
      
        /* Calculate Selling Price - FG */
        IF cSellUOMFG EQ "CS" AND dCaseCount NE 0 THEN
        ASSIGN 
            dExt         = (tt-fg-bin.qty * dSellPrice) / dCaseCount
            dSellValueFG = (tt-fg-bin.qty * dSellPriceFG) / dCaseCount
            .
        ELSE DO:
            FIND FIRST uom NO-LOCK
                 WHERE uom.uom  EQ cSellUOMFG
                   AND uom.mult NE 0
                 NO-ERROR.
            ASSIGN
                dExt = tt-fg-bin.qty * dSellPrice /
                      (IF AVAILABLE uom THEN uom.mult ELSE 1000)
                dSellValueFG = tt-fg-bin.qty * dSellPriceFG /
                      (IF AVAILABLE uom THEN uom.mult ELSE 1000)
                .
        END. /* else */

        IF cSellUOMFG EQ "L" THEN
        ASSIGN 
            dExt         = IF tt-fg-bin.qty LE 0 THEN 0 ELSE dSellPrice
            dSellValueFG = IF tt-fg-bin.qty LE 0 THEN 0 ELSE dSellPriceFG
            .
            
        /* Calculate Selling Price - Order */
        IF cSellUOMOrd EQ "CS" AND dCaseCount NE 0 THEN
        dSellValueOrd = (tt-fg-bin.qty * dSellPriceOrd) / dCaseCount.
        ELSE DO:
            FIND FIRST uom NO-LOCK
                 WHERE uom.uom  EQ cSellUOMOrd
                   AND uom.mult NE 0
                 NO-ERROR.
            dSellValueOrd = tt-fg-bin.qty * dSellPriceOrd /
                           (IF AVAILABLE uom THEN uom.mult ELSE 1000).
        END. /* else */
        
        IF cSellUOMOrd EQ "L" THEN
        dSellValueOrd = IF tt-fg-bin.qty LE 0 THEN 0 ELSE dSellPriceOrd.

        ASSIGN
            dExt           = ROUND(dExt,2)  
            dSellValueOrd  = ROUND(dSellValueOrd,2)
            dSellValueFG   = ROUND(dSellValueFG,2)
            dSellValueFGs  = dSellValueFGs  + dSellValueFG
            dSellValueOrds = dSellValueOrds + dSellValueOrd
            .
            
        IF dCostL EQ ? THEN dCostL = 0.
        IF dCostM EQ ? THEN dCostM = 0.
        IF dCost  EQ ? THEN dCost  = 0.
        IF dExt   EQ ? THEN dExt   = 0.

        ASSIGN
            iQOH           = tt-fg-bin.qty
            dTotSum        = dCost
            dExtSum        = dExt
            dMSFOH         = iQOH * itemfg.t-sqft / 1000
            iBinQOH        = iBinQOH + iQOH
            iBinMSF        = iBinMSF + dMSFOH 
            dTotBinSum     = dTotBinSum + dCost1
            dExtBinSum     = dExtBinSum + dCost    
            dTotQty[1]     = dTotQty[1] + iQOH
            dTotMSF[1]     = dTotMSF[1] + dMSFOH
            dTotCst[1]     = dTotCst[1] + dTotSum
            dTotExt[1]     = dTotExt[1] + dExtSum  
            dTotMat[1]     = dTotMat[1] + dCostM
            dTotLab[1]     = dTotLab[1] + dCostL 
            dTotFGSell[1]  = dTotFGSell[1] + dSellValueFG
            dTotOrdSell[1] = dTotOrdSell[1] + dSellValueOrd
            .

        IF iplIncludeZeroBalance OR iQOH NE 0 THEN DO:
            cJobNo = IF tt-fg-bin.job-no EQ "" THEN ""
                     ELSE TRIM(tt-fg-bin.job-no) + "-" + string(tt-fg-bin.job-no2,"99").
        
            FIND FIRST job-hdr NO-LOCK
                 WHERE job-hdr.company EQ ipcCompany
                   AND job-hdr.job-no  EQ tt-fg-bin.job-no
                   AND job-hdr.job-no2 EQ tt-fg-bin.job-no2
                   AND job-hdr.i-no    EQ tt-fg-bin.i-no
                 USE-INDEX job-No NO-ERROR.
            IF AVAILABLE job-hdr THEN DO:
                FIND FIRST oe-ordl NO-LOCK
                     WHERE oe-ordl.company EQ ipcCompany
                       AND oe-ordl.ord-no  EQ job-hdr.ord-no
                       AND oe-ordl.i-no    EQ job-hdr.i-no
                     NO-ERROR.
                IF AVAILABLE oe-ordl THEN 
                ASSIGN  
                    cPONo    = oe-ordl.po-no
                    dOrdPrice = oe-ordl.price
                    .
                FIND FIRST oe-ord NO-LOCK
                     WHERE oe-ord.company EQ ipcCompany
                       AND oe-ord.ord-no  EQ job-hdr.ord-no
                     NO-ERROR.
                IF AVAILABLE oe-ord THEN cPOOrd = oe-ord.po-no.
            END. /* avail job-hdr */
            ELSE
            ASSIGN
                cPONo  = ""
                cPOOrd = ""
                .

            ASSIGN
                cQOHf = "->>>,>>>,>>9.99"
                cQOHs = STRING(iQOH,cQOHf)
                iArq  = 0
                .

            IF iplPrintSummaryByBinQty THEN
            cQOHs = STRING(iBinQOH,cQOHf).

            dtLastInv = ?.
            IF iplProcessLastSale THEN
            FOR EACH ar-invl NO-LOCK
                WHERE ar-invl.i-no EQ itemfg.i-no,
                EACH ar-inv NO-LOCK
                WHERE ar-inv.x-no EQ ar-invl.x-no
                BY ar-inv.inv-date DESCENDING
                :
                dtLastInv = ar-inv.inv-date.
                LEAVE.
            END. /* each ar-invl */

            cPORel = "" . 
            IF iplProcessRel THEN
            FOR EACH oe-relh FIELDS(company r-no) NO-LOCK
                WHERE oe-relh.company EQ tt-fg-bin.company
                  AND oe-relh.posted  EQ NO
                USE-INDEX post,
                EACH oe-rell FIELDS(qty po-no) NO-LOCK
                WHERE oe-rell.company EQ oe-relh.company
                  AND oe-rell.r-no    EQ oe-relh.r-no
                  AND oe-rell.i-no    EQ tt-fg-bin.i-no
                  AND oe-rell.loc     EQ tt-fg-bin.loc
                  AND oe-rell.loc-bin EQ tt-fg-bin.loc-bin
                  AND oe-rell.tag     EQ tt-fg-bin.tag
                  AND oe-rell.cust-no EQ tt-fg-bin.cust-no
                USE-INDEX r-no
                :
                ASSIGN
                    iArq   = iArq + oe-rell.qty
                    cPORel = oe-rell.po-no
                    .
            END. /* oe-relh */
            iBinArq = iBinArq + iArq.

            FIND FIRST fg-set NO-LOCK
                 WHERE fg-set.company EQ itemfg.company
                   AND fg-set.part-no EQ itemfg.i-no
                 NO-ERROR.
        END. /* if lIncludeZeroBalance */

        CREATE ttInventoryValue.
        ASSIGN
            ttInventoryValue.custNo       = cCustNo
            ttInventoryValue.custName     = cCustName
            ttInventoryValue.salesRep     = cSalesRep
            ttInventoryValue.iNo          = tt-fg-bin.i-no
            ttInventoryValue.iName        = tt-itemfg.i-name
            ttInventoryValue.tagNo        = IF SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no THEN (SUBSTR(tt-fg-bin.tag,16,5)) ELSE  ""
            ttInventoryValue.tag          = tt-fg-bin.tag
            ttInventoryValue.fgLotVal     = cFGLotVal
            ttInventoryValue.partNo       = tt-itemfg.part-no
            ttInventoryValue.procat       = tt-itemfg.procat
            ttInventoryValue.loc          = tt-fg-bin.loc
            ttInventoryValue.bin          = tt-fg-bin.loc-bin
            ttInventoryValue.jobNo        = cJobNo
            ttInventoryValue.msfOnHand    = dMSFOH
            ttInventoryValue.qtyOnHand    = tt-fg-bin.qty
            ttInventoryValue.relQty       = iBinArq
            ttInventoryValue.sellPrice    = dSellPrice
            ttInventoryValue.ordPr        = dSellPriceOrd
            ttInventoryValue.uomCost      = IF lSecure THEN dCost1 ELSE 0
            ttInventoryValue.totCost      = IF lSecure THEN dCost  ELSE 0
            ttInventoryValue.matCost      = IF lSecure THEN dCostM ELSE 0
            ttInventoryValue.labCost      = IF lSecure THEN dCostL ELSE 0
            ttInventoryValue.costUom      = tt-fg-bin.pur-uom
            ttInventoryValue.sellValueFg  = dSellValueFG
            ttInventoryValue.sellValueOrd = dSellValueOrd
            ttInventoryValue.lastSale     = dtLastInv
            ttInventoryValue.viewPo       = cPOOrd
            ttInventoryValue.linePo       = cPONo
            ttInventoryValue.relPo        = cPORel
            ttInventoryValue.daysOld      = ipdtAsOfDate - dtRctDate
            ttInventoryValue.custNoOwned  = ""
            ttInventoryValue.setHeader    = IF AVAILABLE fg-set AND cJobNo NE "" THEN fg-set.set-no   ELSE ""
            ttInventoryValue.qtyPerSet    = IF AVAILABLE fg-set AND cJobNo NE "" THEN fg-set.qtyPerSet ELSE 0
            ttInventoryValue.recDate      = dtRctDate
            ttInventoryValue.xxSort       = IF ipcSort EQ "Customer"         THEN ttInventoryValue.custNo
                                       ELSE IF ipcSort EQ "FG Item"          THEN ttInventoryValue.iNo
                                       ELSE IF ipcSort EQ "Part"             THEN ttInventoryValue.partNo
                                       ELSE IF ipcSort EQ "Product Category" THEN ttInventoryValue.procat
                                       ELSE IF ipcSort EQ "Whs/Bin"          THEN ttInventoryValue.loc + ttInventoryValue.bin
                                       ELSE ""
            ttInventoryValue.xxSort       = ttInventoryValue.xxSort + ttInventoryValue.iNo
            .
        RELEASE fg-set.
    END. /* each tt-itemfg */
END PROCEDURE.

{aoa/BL/pBuildCustList.i}
