/*------------------------------------------------------------------------
  File: shiprpt.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Shipment Report.rpa */
{aoa/tempTable/ttShipmentReport.i}
{aoa/tempTable/ttShipmentReportDetail.i}
{aoa/tempTable/ttShipmentReportSummary.i}

DEFINE TEMP-TABLE ttReport NO-UNDO
    FIELD company    AS CHARACTER 
    FIELD custNo     AS CHARACTER 
    FIELD tableName  AS CHARACTER 
    FIELD tableRecID AS RECID 
    INDEX idx custNo tableName.

{sys/ref/CustList.i NEW}

/* Parameters Definitions */
DEFINE OUTPUT PARAMETER TABLE FOR ttShipmentReport.
{aoa/includes/pShipmentReport.i}

/* Local Variables */
DEFINE VARIABLE lMisc            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cTerm            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobNo           AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPallets         AS INTEGER   NO-UNDO.
DEFINE VARIABLE dQtyPallets      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dFrtRate         AS DECIMAL   NO-UNDO EXTENT 2.
DEFINE VARIABLE dFrtCharge       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dTotOtherFreight AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dOtherFreight    AS DECIMAL   NO-UNDO.

DEFINE BUFFER b-oe-boll FOR oe-boll.

{custom/globdefs.i &NEW="NEW"}
ASSIGN
    g_company   = ipcCompany
    g_loc       = cLocation
    cStartJobNo = FILL(" ",6 - LENGTH(TRIM(cStartJobNo)))
                + TRIM(cStartJobNo)
    cEndJobNo   = FILL(" ",6 - LENGTH(TRIM(cEndJobNo)))
                + TRIM(cEndJobNo)
    .
/* Business Logic */       
FOR EACH ar-inv NO-LOCK 
    WHERE ar-inv.company  EQ ipcCompany
      AND ar-inv.posted   EQ YES
      AND ar-inv.inv-date GE dtStartInvoiceDate
      AND ar-inv.inv-date LE dtEndInvoiceDate
      AND ar-inv.cust-no  GE cStartCustNo
      AND ar-inv.cust-no  LE cEndCustNo
      AND ar-inv.type     NE "FC"
    USE-INDEX inv-date,
    FIRST cust NO-LOCK 
    WHERE cust.company EQ ar-inv.company
      AND cust.cust-no EQ ar-inv.cust-no,
    EACH  ar-invl NO-LOCK 
    WHERE ar-invl.x-no EQ ar-inv.x-no
      AND ((FILL(" ",6 - LENGTH(TRIM(ar-invl.job-no))) +
            TRIM(ar-invl.job-no) +
            STRING(ar-invl.job-no2,"99") GE cStartJobNo + STRING(iStartJobNo2,"99")
      AND   FILL(" ",6 - LENGTH(TRIM(ar-invl.job-no))) +
            TRIM(ar-invl.job-no) +
            STRING(ar-invl.job-no2,"99") LE cEndJobNo + STRING(iEndJobNo2,"99"))
       OR   lAllJobNo EQ YES)
      AND ar-invl.misc EQ NO,
    FIRST itemfg NO-LOCK 
    WHERE itemfg.company EQ ar-inv.company
      AND itemfg.i-no    EQ ar-invl.i-no
    BREAK BY ar-inv.inv-no BY ar-inv.inv-date
    :
    IF lCustList AND
       NOT CAN-FIND(FIRST ttCustList
                    WHERE ttCustList.cust-no EQ ar-inv.cust-no
                      AND ttCustList.log-fld EQ TRUE) THEN
    NEXT.
    ASSIGN
        iPallets  = 0
        dFrtRate[1] = 0
        .
    FOR EACH oe-boll NO-LOCK
        WHERE oe-boll.company EQ ar-invl.company
          AND oe-boll.b-no    EQ ar-invl.b-no
          AND oe-boll.ord-no  EQ ar-invl.ord-no
          AND oe-boll.i-no    EQ ar-invl.i-no
          AND oe-boll.po-no   EQ ar-invl.po-no
        USE-INDEX b-no,
        FIRST oe-bolh OF oe-boll NO-LOCK,
        FIRST shipto NO-LOCK
        WHERE shipto.company EQ oe-bolh.company
          AND shipto.cust-no EQ oe-bolh.cust-no
          AND shipto.ship-id EQ oe-bolh.ship-id,
        FIRST carrier OF oe-bolh NO-LOCK
        :
        FIND FIRST fg-bin NO-LOCK
            WHERE fg-bin.company EQ oe-boll.company
              AND fg-bin.i-no    EQ oe-boll.i-no
              AND fg-bin.tag     EQ oe-boll.tag
              AND fg-bin.loc     EQ oe-boll.loc
              AND fg-bin.loc-bin EQ oe-boll.loc-bin
              AND fg-bin.job-no  EQ oe-boll.job-no
              AND fg-bin.job-no2 EQ oe-boll.job-no2
            NO-ERROR.
        dQtyPallets = oe-boll.qty-case.
        dTotOtherFreight = 0.
        IF AVAILABLE oe-boll THEN DO:
            FOR EACH b-oe-boll NO-LOCK
                WHERE b-oe-boll.company = oe-boll.company
                  AND b-oe-boll.bol-no  = oe-boll.bol-no
                  AND b-oe-boll.i-no    = oe-boll.i-no
                :
                RUN pOtherLines (INPUT ROWID(b-oe-boll), OUTPUT dOtherFreight).
                /* total weight of all lines on BOL for this item */
                dTotOtherFreight = dTotOtherFreight + dOtherFreight.
            END. /* each b-oe-boll */
        END. /* avail oe-boll */
        IF AVAILABLE fg-bin THEN
        dQtyPallets = (IF dQtyPallets NE 0 THEN dQtyPallets ELSE
                       IF fg-bin.case-count   EQ 0 THEN 1 ELSE fg-bin.case-count)  *
                      (IF fg-bin.cases-unit   EQ 0 THEN 1 ELSE fg-bin.cases-unit)  *
                      (IF fg-bin.units-pallet EQ 0 THEN 1 ELSE fg-bin.units-pallet).
        dQtyPallets = oe-boll.qty / dQtyPallets.
        {sys/inc/roundup.i dQtyPallets}
        IF dQtyPallets LT 0 THEN dQtyPallets = dQtyPallets * -1.
        iPallets = iPallets + dQtyPallets.
        CASE carrier.chg-method:
            WHEN "W" THEN /* Weight in Lbs */
            dFrtCharge = itemfg.weight-100 * oe-boll.qty / 100.
            WHEN "P" THEN /* # of Pallets */
            dFrtCharge = dQtyPallets.                        
            OTHERWISE /* MSF */
            dFrtCharge = itemfg.t-sqft * oe-boll.qty / 1000.
        END CASE.
        RUN sys/inc/getfrate.p (
            shipto.loc,
            oe-bolh.carrier,
            shipto.dest-code,
            shipto.ship-zip,
            dFrtCharge,
            dTotOtherFreight,
            1,
            OUTPUT dFrtCharge
            ).
        dFrtRate[1] = dFrtRate[1] + dFrtCharge.
    END. /* each oe-boll */
    cJobNo = TRIM(ar-invl.job-no) + "-" + STRING(ar-invl.job-no2,"99").
    IF cJobNo EQ "-00" THEN cJobNo = "".
    CREATE ttShipmentReportDetail.
    ASSIGN 
        ttShipmentReportDetail.itemNo    = ar-invl.i-no
        ttShipmentReportDetail.itemDscr  = itemfg.i-name
        ttShipmentReportDetail.custPart  = itemfg.part-no
        ttShipmentReportDetail.invNo     = ar-inv.inv-no
        ttShipmentReportDetail.invDate   = ar-inv.inv-date
        ttShipmentReportDetail.jobNo     = cJobNo
        ttShipmentReportDetail.qtyShip   = ar-invl.ship-qty
        ttShipmentReportDetail.skid      = iPallets
        ttShipmentReportDetail.weight100 = itemfg.weight-100
        ttShipmentReportDetail.frtCharge = dFrtRate[1]
        .
    dFrtRate[2] = dFrtRate[2] + dFrtRate[1].
END. /* each ar-inv */

RUN pShipmentReportSummary.
RUN pShipmentReport.

PROCEDURE pOtherLines :
    DEFINE INPUT  PARAMETER iprOEBollRowID AS ROWID   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdeOutFrt     AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE iPallets AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-oe-boll FOR oe-boll.
    DEFINE BUFFER bf-shipto  FOR shipto.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    DEFINE BUFFER bf-carrier FOR carrier.
    DEFINE BUFFER bf-fg-bin  FOR fg-bin.
    
    FIND FIRST bf-oe-boll NO-LOCK
         WHERE ROWID(bf-oe-boll) = iprOEBollRowID
         NO-ERROR.
    FIND FIRST oe-bolh NO-LOCK
         WHERE oe-bolh.b-no EQ bf-oe-boll.b-no
         NO-ERROR.
    IF AVAILABLE oe-bolh THEN
    FIND FIRST bf-shipto NO-LOCK
        WHERE bf-shipto.company EQ oe-bolh.company
          AND bf-shipto.cust-no EQ oe-bolh.cust-no
          AND bf-shipto.ship-id EQ oe-bolh.ship-id
        NO-ERROR.
    IF AVAILABLE bf-shipto THEN
    FIND FIRST bf-carrier NO-LOCK
        WHERE bf-carrier.company EQ bf-shipto.company
          AND bf-carrier.loc     EQ bf-shipto.loc
          AND bf-carrier.carrier EQ oe-bolh.carrier
        NO-ERROR.
    IF AVAILABLE bf-carrier THEN
    FOR EACH bf-itemfg NO-LOCK
        WHERE bf-itemfg.company EQ bf-oe-boll.company
          AND bf-itemfg.i-no    EQ bf-oe-boll.i-no
        :
        FIND FIRST bf-fg-bin NO-LOCK
             WHERE bf-fg-bin.company EQ bf-oe-boll.company
               AND bf-fg-bin.i-no    EQ bf-oe-boll.i-no
               AND bf-fg-bin.tag     EQ bf-oe-boll.tag
               AND bf-fg-bin.loc     EQ bf-oe-boll.loc
               AND bf-fg-bin.loc-bin EQ bf-oe-boll.loc-bin
               AND bf-fg-bin.job-no  EQ bf-oe-boll.job-no
               AND bf-fg-bin.job-no2 EQ bf-oe-boll.job-no2
             NO-ERROR.
        CASE bf-carrier.chg-method:
            WHEN "W" THEN /* Weight in Lbs */
            opdeOutFrt = IF bf-oe-boll.weight NE 0 THEN bf-oe-boll.weight
                        ELSE (bf-itemfg.weight-100 * bf-oe-boll.qty / 100).
            WHEN "P" THEN /* # of Pallets */
            DO:                                     
                RUN oe/pallcalc.p (ROWID(bf-oe-boll), OUTPUT iPallets).
                opdeOutFrt = iPallets.                        
            END. /* p */
            OTHERWISE /* MSF */
            opdeOutFrt = bf-itemfg.t-sqft * bf-oe-boll.qty / 1000.
        END CASE.
    END. /* each bf-itemfg */
END PROCEDURE.

PROCEDURE pCreatettShipmentReportSummary:
    DEFINE INPUT PARAMETER ipcItemNo     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemDscr   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustPart   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiYear       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiMonth      AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiQtyShipped AS INTEGER   NO-UNDO.
    
    FIND FIRST ttShipmentReportSummary
         WHERE ttShipmentReportSummary.itemNo   EQ ipcItemNo
           AND ttShipmentReportSummary.qtyYear  EQ ipiYear
           AND ttShipmentReportSummary.qtyMonth EQ ipiMonth
         NO-ERROR.
    IF NOT AVAILABLE ttShipmentReportSummary THEN DO:
        CREATE ttShipmentReportSummary.
        ASSIGN 
            ttShipmentReportSummary.itemNo   = ipcItemNo
            ttShipmentReportSummary.itemDscr = ipcItemDscr
            ttShipmentReportSummary.custPart = ipcCustPart
            ttShipmentReportSummary.qtyYear  = ipiYear
            ttShipmentReportSummary.qtyMonth = ipiMonth
            .
    END. /* not avail */
    ttShipmentReportSummary.qtyShipped = ttShipmentReportSummary.qtyShipped + ipiQtyShipped.
END PROCEDURE.

PROCEDURE pQtyShipped:
    DEFINE INPUT PARAMETER ipiExtent AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcValue  AS CHARACTER NO-UNDO.
    
    CASE ipiExtent:
        {AOA/BL/shiprpt.i 1}
        {AOA/BL/shiprpt.i 2}
        {AOA/BL/shiprpt.i 3}
        {AOA/BL/shiprpt.i 4}
        {AOA/BL/shiprpt.i 5}
        {AOA/BL/shiprpt.i 6}
        {AOA/BL/shiprpt.i 7}
        {AOA/BL/shiprpt.i 8}
        {AOA/BL/shiprpt.i 9}
        {AOA/BL/shiprpt.i 10}
        {AOA/BL/shiprpt.i 11}
        {AOA/BL/shiprpt.i 12}
        {AOA/BL/shiprpt.i 13}
        {AOA/BL/shiprpt.i 14}
        {AOA/BL/shiprpt.i 15}
        {AOA/BL/shiprpt.i 16}
        {AOA/BL/shiprpt.i 17}
        {AOA/BL/shiprpt.i 18}
        {AOA/BL/shiprpt.i 19}
        {AOA/BL/shiprpt.i 20}
        {AOA/BL/shiprpt.i 21}
        {AOA/BL/shiprpt.i 22}
        {AOA/BL/shiprpt.i 23}
        {AOA/BL/shiprpt.i 24}
        {AOA/BL/shiprpt.i 25}
        {AOA/BL/shiprpt.i 26}
        {AOA/BL/shiprpt.i 27}
    END CASE.
END PROCEDURE. 

PROCEDURE pShipmentReport:
    DEFINE VARIABLE cMonth  AS CHARACTER NO-UNDO INITIAL "January,February,March,April,May,June,July,August,September,October,November,December".
    DEFINE VARIABLE iOrder  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iExtent AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cItemNo AS CHARACTER NO-UNDO.

    FIND FIRST ttShipmentReportSummary
         WHERE ttShipmentReportSummary.rowType EQ "Data"
         NO-ERROR.
    IF NOT AVAILABLE ttShipmentReportSummary THEN RETURN.
    cItemNo = ttShipmentReportSummary.itemNo.
    
    /* header line 1 */
    CREATE ttShipmentReport.
    ASSIGN 
        iOrder                   = iOrder + 1
        ttShipmentReport.xxOrder = iOrder
        iExtent                  = 0
        .
    FOR EACH ttShipmentReportSummary
        WHERE ttShipmentReportSummary.rowType  EQ "Data"
          AND ttShipmentReportSummary.itemNo   EQ cItemNo
           BY ttShipmentReportSummary.qtyYear  DESCENDING
           BY ttShipmentReportSummary.qtyMonth DESCENDING
        :
        iExtent = iExtent + 1.
        IF ttShipmentReportSummary.qtyMonth EQ 99 THEN
        RUN pQtyShipped (iExtent, STRING(ttShipmentReportSummary.qtyYear,"9999")).
        IF ttShipmentReportSummary.qtyYear  EQ 0 THEN
        RUN pQtyShipped (iExtent, "Grand").
    END. /* each ttShipmentReport */ 

    /* header line 2 */
    CREATE ttShipmentReport.
    ASSIGN 
        iOrder                    = iOrder + 1
        ttShipmentReport.xxOrder  = iOrder
        ttShipmentReport.itemNo   = "FG Item"
        ttShipmentReport.itemDscr = "FG Description"
        ttShipmentReport.custPart = "Customer Part"
        iExtent                   = 0
        .
    FOR EACH ttShipmentReportSummary
        WHERE ttShipmentReportSummary.rowType  EQ "Data"
          AND ttShipmentReportSummary.itemNo   EQ cItemNo
           BY ttShipmentReportSummary.qtyYear  DESCENDING
           BY ttShipmentReportSummary.qtyMonth DESCENDING
        :
        iExtent = iExtent + 1.
        IF ttShipmentReportSummary.qtyMonth EQ 99 THEN
        RUN pQtyShipped (iExtent, "Total").
        IF ttShipmentReportSummary.qtyMonth NE 0 AND ttShipmentReportSummary.qtyMonth NE 99 THEN
        RUN pQtyShipped (iExtent, ENTRY(ttShipmentReportSummary.qtyMonth,cMonth)).
        IF ttShipmentReportSummary.qtyYear  EQ 0 THEN
        RUN pQtyShipped (iExtent, "Total").
    END. /* each ttShipmentReport */ 

    /* detail lines */
    FOR EACH ttShipmentReportSummary
        WHERE ttShipmentReportSummary.rowType EQ "Data"
        BREAK BY ttShipmentReportSummary.itemNo
              BY ttShipmentReportSummary.qtyYear  DESCENDING
              BY ttShipmentReportSummary.qtyMonth DESCENDING
        :
        IF FIRST-OF(ttShipmentReportSummary.itemNo) THEN DO:
            CREATE ttShipmentReport.
            ASSIGN 
                iOrder                    = iOrder + 1
                ttShipmentReport.xxOrder  = iOrder
                ttShipmentReport.itemNo   = ttShipmentReportSummary.itemNo 
                ttShipmentReport.itemDscr = ttShipmentReportSummary.itemDscr
                ttShipmentReport.custPart = ttShipmentReportSummary.custPart
                iExtent                   = 0
                .
            IF ttShipmentReportSummary.itemNo EQ "Grand Totals" THEN
            ttShipmentReport.xxOrder = 999999.
        END. /* first-of */
        iExtent = iExtent + 1.
        RUN pQtyShipped (iExtent,
            IF ttShipmentReportSummary.qtyShipped NE 0 THEN STRING(ttShipmentReportSummary.qtyShipped,"->,>>>,>>>,>>9")
            ELSE "").
    END. /* each ttShipmentReport */
END PROCEDURE.

PROCEDURE pShipmentReportSummary:
    DEFINE VARIABLE dDate AS DATE NO-UNDO.
    
    FOR EACH ttShipmentReportDetail
        BREAK BY ttShipmentReportDetail.itemNo
        :
        IF FIRST-OF(ttShipmentReportDetail.itemNo) THEN DO:
            /* prebuild all the records needed from 1st month to last month */
            DO dDate = dtEndInvoiceDate TO dtStartInvoiceDate BY -1:
                /* individual month/year cell for an item */
                RUN pCreatettShipmentReportSummary (
                    ttShipmentReportDetail.itemNo,
                    ttShipmentReportDetail.itemDscr,
                    ttShipmentReportDetail.custPart,
                    YEAR(dDate),MONTH(dDate),0
                    ).
                /* year total for an item */
                RUN pCreatettShipmentReportSummary (
                    ttShipmentReportDetail.itemNo,
                    ttShipmentReportDetail.itemDscr,
                    ttShipmentReportDetail.custPart,
                    YEAR(dDate),99,0
                    ).
                /* grand total for an item */
                RUN pCreatettShipmentReportSummary (
                    ttShipmentReportDetail.itemNo,
                    ttShipmentReportDetail.itemDscr,
                    ttShipmentReportDetail.custPart,
                    0,0,0
                    ).
                /* grand totals for all items */
                RUN pCreatettShipmentReportSummary (
                    "Grand Totals","","",
                    YEAR(dDate),MONTH(dDate),0
                    ).
                RUN pCreatettShipmentReportSummary (
                    "Grand Totals","","",
                    YEAR(dDate),99,0
                    ).
                RUN pCreatettShipmentReportSummary (
                    "Grand Totals","","",
                    0,0,0
                    ).
            END. /* do ddate */
        END. /* if first-of */
        /* individual month/year cell for an item */
        RUN pCreatettShipmentReportSummary (
            ttShipmentReportDetail.itemNo,"","",
            YEAR(ttShipmentReportDetail.invDate),
            MONTH(ttShipmentReportDetail.invDate),
            ttShipmentReportDetail.qtyShipped
            ).
        /* year total for an item */
        RUN pCreatettShipmentReportSummary (
            ttShipmentReportDetail.itemNo,"","",
            YEAR(ttShipmentReportDetail.invDate),99,
            ttShipmentReportDetail.qtyShipped
            ).
        /* grand total for an item */
        RUN pCreatettShipmentReportSummary (
            ttShipmentReportDetail.itemNo,"","",0,0,
            ttShipmentReportDetail.qtyShipped
            ).
        /* grand totals for all items */
        RUN pCreatettShipmentReportSummary (
            "Grand Totals","","",
            YEAR(ttShipmentReportDetail.invDate),
            MONTH(ttShipmentReportDetail.invDate),
            ttShipmentReportDetail.qtyShipped
            ).
        RUN pCreatettShipmentReportSummary (
            "Grand Totals","","",
            YEAR(ttShipmentReportDetail.invDate),99,
            ttShipmentReportDetail.qtyShipped
            ).
        RUN pCreatettShipmentReportSummary (
            "Grand Totals","","",0,0,
            ttShipmentReportDetail.qtyShipped
            ).
    END. /* each ttShipmentReportDetail */ 
END PROCEDURE.
