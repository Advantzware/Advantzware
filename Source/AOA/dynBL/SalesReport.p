/*------------------------------------------------------------------------
  File:         SalesReport.p
  Description:  Business Logic
  Author:       Sewa Singh
  Date Created: 8.4.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttSalesReport
DEFINE TEMP-TABLE ttSalesReport NO-UNDO
    FIELD ArClass           AS CHARACTER FORMAT "x(10)" LABEL "AR Class"
    FIELD custNo            AS CHARACTER FORMAT "x(10)" LABEL "Customer"
    FIELD custName          AS CHARACTER FORMAT "x(30)" LABEL "Customer Name"
    FIELD custAdd1          AS CHARACTER FORMAT "x(30)" LABEL "Customer Add1"
    FIELD custAdd2          AS CHARACTER FORMAT "x(30)" LABEL "Customer Add2"
    FIELD custAdd3          AS CHARACTER FORMAT "x(30)" LABEL "Customer Add3"
    FIELD custCity          AS CHARACTER FORMAT "x(20)" LABEL "Customer City"
    FIELD custState         AS CHARACTER FORMAT "x(2)" LABEL "Customer State"
    FIELD custZip           AS CHARACTER FORMAT "x(20)" LABEL "Customer Zip"    
    FIELD custPhone         AS CHARACTER FORMAT "x(20)" LABEL "Customer Phone"
    FIELD custContact       AS CHARACTER FORMAT "x(30)" LABEL "Customer Contact"
    FIELD custNcaisCode     AS CHARACTER FORMAT "x(20)" LABEL "NCAIS Code"    
    FIELD custMarketSeg     AS CHARACTER FORMAT "x(20)" LABEL "Market Segment"
    FIELD custParentCust    AS CHARACTER FORMAT "x(30)" LABEL "Parent Customer"
    FIELD custAccType       AS CHARACTER FORMAT "x(20)" LABEL "Account Type"
    FIELD custSplitType     AS CHARACTER FORMAT "x(20)" LABEL "Split Type"    
    FIELD shipToCode        AS CHARACTER FORMAT "x(10)" LABEL "Ship to Code"
    FIELD shipToName        AS CHARACTER FORMAT "x(30)" LABEL "Ship to Name"
    FIELD shipToAdd1        AS CHARACTER FORMAT "x(30)" LABEL "Ship to Add1"
    FIELD shipToAdd2        AS CHARACTER FORMAT "x(30)" LABEL "Ship to Add2"
    FIELD shipToAdd3        AS CHARACTER FORMAT "x(30)" LABEL "Ship to Add3"
    FIELD shipToCity        AS CHARACTER FORMAT "x(20)" LABEL "Ship to City"
    FIELD shipToState       AS CHARACTER FORMAT "x(2)" LABEL "Ship to State"
    FIELD shipToZip         AS CHARACTER FORMAT "x(20)" LABEL "Ship to Zip"     
    FIELD custPartNo        AS CHARACTER FORMAT "x(15)" LABEL "Cust Part No"
    FIELD proCat            AS CHARACTER FORMAT "x(8)" LABEL "Product Category"
    FIELD itemUnitPrice     AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" LABEL "Item Unit Price"
    FIELD itemSqFt          AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" LABEL "Item SqFt"
    FIELD itemCost          AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" LABEL "Item Cost"
    FIELD itemStyle         AS CHARACTER FORMAT "x(10)" LABEL "Item Style"
    FIELD itemFlute         AS CHARACTER FORMAT "x(10)" LABEL "Item Flute"
    FIELD itemTest          AS CHARACTER FORMAT "x(10)" LABEL "Item Test"
    FIELD itemLen           AS DECIMAL   FORMAT "->,>>>,>>9.99" LABEL "Item Length"
    FIELD itemWid           AS DECIMAL   FORMAT "->,>>>,>>9.99" LABEL "Item Width"
    FIELD itemDep           AS DECIMAL   FORMAT "->,>>>,>>9.99" LABEL "Item Depth"
    FIELD itemWeight        AS DECIMAL   FORMAT "->,>>>,>>9.99" LABEL "Item Weight"     
    FIELD ordQty            AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Quantity Ordered"
    FIELD ordOrderNo        AS INTEGER   FORMAT ">>>>>>9" LABEL "Order Number"
    FIELD ordType           AS CHARACTER FORMAT "x(10)" LABEL "Order Type"
    FIELD ordLine           AS INTEGER   FORMAT ">>>" LABEL "Order Line"
    FIELD ordEstNo          AS CHARACTER FORMAT "x(8)" LABEL "Estimate Number"
    FIELD ordDate           AS CHARACTER FORMAT "99/99/9999" LABEL "Order Date"    
    FIELD invNo             AS INTEGER   FORMAT ">>>>>>>9" LABEL "Invoice#"
    FIELD invDate           AS CHARACTER FORMAT "99/99/9999" LABEL "Invoice Date"
    FIELD invItemNo         AS CHARACTER FORMAT "x(15)" LABEL "Item Number"
    FIELD invQtyUom         AS CHARACTER FORMAT "x(3)" LABEL "Quantity UOM"
    FIELD invQtyShip        AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Quantity Shipped"
    FIELD invQtyInv         AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Quantity Invoiced"
    FIELD invUnitPrice      AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Unit Price"
    FIELD invPriceUom       AS CHARACTER FORMAT "x(3)" LABEL "Price UOM"
    FIELD invExtPrice       AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Extended Price"
    FIELD invTaxable        AS LOGICAL   LABEL "Taxable"
    FIELD invCarrier        AS CHARACTER FORMAT "x(10)" LABEL "Carrier"
    FIELD invFob            AS CHARACTER FORMAT "x(15)" LABEL "FOB"
    FIELD invFrtPay         AS CHARACTER FORMAT "x(15)" LABEL "Freight Pay"
    FIELD invTaxGroup       AS CHARACTER FORMAT "x(10)" LABEL "Tax Group"
    FIELD invTotCost        AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Total Cost"
    FIELD invTotWeight      AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Total Weight"
    FIELD invDiscount       AS DECIMAL   FORMAT "->>>>.99" LABEL "Discount %"
    FIELD invBolNo          AS INTEGER   FORMAT ">>>>>>>>9" LABEL "BOL Number"
    FIELD invCustPoNO       AS CHARACTER FORMAT "x(15)" LABEL "Customer PO Number"
    FIELD invAmtDue         AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Current Amount Due"
    FIELD invPaidDate       AS CHARACTER FORMAT "99/99/9999" LABEL "Paid Date"
    FIELD invLastPayDate    AS CHARACTER FORMAT "99/99/9999" LABEL "Last Payment Date"
    FIELD invPayCheckNo     AS CHARACTER FORMAT "x(30)" LABEL "Last Payment Check Number"
    FIELD invGLAccount      AS CHARACTER FORMAT "x(20)" LABEL "GL Account Number"
    FIELD invGLPeriod       AS INTEGER   FORMAT ">>>" LABEL "GL Period"
    FIELD invGLYear         AS INTEGER   FORMAT ">>>>>" LABEL "GL Year"
    FIELD invGLRun          AS INTEGER   FORMAT ">>>>>>>>" LABEL "GL Run Number"    
    FIELD invFreightAmt     AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Freight Amount"
    FIELD invTaxAmt         AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Tax Amount"
    FIELD invSman           AS CHARACTER FORMAT "x(10)" LABEL "Salesgroup Id"
    FIELD invCurrCode       AS CHARACTER FORMAT "x(10)" LABEL "Currency Code"
    FIELD invCurrRate       AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Currency Rate"    
    FIELD invExtPricePerM   AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Extended Price Per M"
    FIELD invExtPricePerTon AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Extended Price Per Ton"
    FIELD invExtPricePerMsf AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Extended Price Per MSF"    
    FIELD jobNo             AS CHARACTER FORMAT "x(8)" LABEL "Job Number"
    FIELD jobNo2            AS INTEGER   FORMAT ">>9" LABEL "Job Number2"
    FIELD jobForm           AS INTEGER   FORMAT ">>9" LABEL "Form"
    FIELD jobBlank          AS INTEGER   FORMAT ">>9" LABEL "Blank"
    FIELD jobColor          AS INTEGER   FORMAT ">>>9" LABEL "Colors"
    FIELD jobShipDate       AS CHARACTER FORMAT "99/99/9999" LABEL "Ship Date"
    FIELD jobPostDate       AS CHARACTER FORMAT "99/99/9999" LABEL "Post Date"    
    FIELD SalesManager      AS CHARACTER FORMAT "x(15)" LABEL "Sales Manager"
    FIELD SalesGroupName    AS CHARACTER FORMAT "x(15)" LABEL "Salesgroup Name"    
    FIELD profit            AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Profit $ "
    FIELD profitPer         AS DECIMAL   FORMAT "->>>9.99" LABEL "Profit % "        
    FIELD CostStdFreight    AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Std Freight Cost"        
    FIELD CostStdWarehouse  AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Std Warehouse Cost"              
    FIELD costStdDeviation  AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Std Deviation Cost" 
    FIELD costStdManufacture AS DECIMAL  FORMAT "->>,>>>,>>>,>>9.99" LABEL "Std Manufacture Cost"
    .
    
DEFINE VARIABLE cShipId   LIKE ar-inv.ship-id NO-UNDO.
DEFINE VARIABLE lExc      AS LOG       NO-UNDO.
DEFINE VARIABLE v-sman-no AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE i         AS INTEGER   NO-UNDO.
DEFINE VARIABLE j         AS INTEGER   NO-UNDO.
/* Parameters Definitions ---                                           */

&Scoped-define subjectID 140
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cocode AS CHARACTER NO-UNDO .
DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD v-po AS CHARACTER .

DEFINE BUFFER xreport FOR tt-report.

DEFINE TEMP-TABLE w-data NO-UNDO
    FIELD i-no   LIKE ar-invl.i-no COLUMN-LABEL "FG Item"
    FIELD inv-no LIKE ar-invl.inv-no COLUMN-LABEL "Invoice!Number" FORMAT ">>>>>>>9"
    FIELD rec-id AS RECID.  

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE v-inc-fc    AS LOG       INIT NO NO-UNDO.
    DEFINE VARIABLE fdate       AS DATE      FORMAT "99/99/9999" NO-UNDO.
    DEFINE VARIABLE tdate       LIKE fdate NO-UNDO.    
    DEFINE VARIABLE lv-r-no     LIKE oe-retl.r-no NO-UNDO.
    DEFINE VARIABLE lv-type     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtCheckDate LIKE ar-inv.inv-date NO-UNDO.     
    DEFINE VARIABLE dUnitPrice  LIKE ar-invl.unit-pr COLUMN-LABEL "Unit Price" NO-UNDO.
    DEFINE VARIABLE cPriceUom   LIKE ar-invl.pr-uom COLUMN-LABEL "UOM" NO-UNDO.    
    DEFINE VARIABLE dPct        AS DECIMAL   FORMAT "99.99" NO-UNDO.          
    DEFINE VARIABLE dDiscount   LIKE ar-invl.disc NO-UNDO. 
    DEFINE VARIABLE iQtyShipped AS INTEGER   EXTENT 5 COLUMN-LABEL "Qty Shipped" NO-UNDO.
    DEFINE VARIABLE dInvoiceAmt AS DECIMAL   EXTENT 5 COLUMN-LABEL "Invoice Amt" NO-UNDO. 
    DEFINE VARIABLE li-seq      AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE lv-quotes   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dInvAmount  AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dFGItemSqft AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotalSqft  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iOrderNo    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dProfit     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOutQty     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dPer        AS DECIMAL   NO-UNDO.      

    
    fdate      = dtStartInvoiceDate .
    tdate      = dtEndInvoiceDate .
    cocode  = cCompany .
    v-inc-fc   = lIncludeFinanceCharges .

    FOR EACH cust
        WHERE cust.company EQ cocode
        AND cust.cust-no GE cStartCustNo
        AND cust.cust-no LE cEndCustNo        
        AND cust.type    GE cStartCustType
        AND cust.type    LE cEndCustType
        USE-INDEX cust NO-LOCK:
        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
        {sa/sa-sls03.i "fdate" "tdate"}    
    END.
  
  
    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
        AND tt-report.key-01  EQ ""
        AND tt-report.key-02  EQ ""
        AND tt-report.key-03  EQ ""
        AND tt-report.key-04  EQ ""
        AND tt-report.key-05  EQ ""
        AND tt-report.key-06  EQ ""
        AND tt-report.key-07  EQ ""
        AND tt-report.key-08  EQ "",

        FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ tt-report.key-09
        NO-LOCK

        TRANSACTION:

        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}

        IF tt-report.key-10 EQ "ar-inv" THEN 
        DO:       
            FIND ar-inv WHERE RECID(ar-inv) EQ tt-report.rec-id NO-LOCK.       

            RUN ship-data.             

            FOR EACH ar-invl
                WHERE ar-invl.x-no    EQ ar-inv.x-no                  
                AND (ar-invl.billable OR NOT ar-invl.misc)
                USE-INDEX x-no NO-LOCK:

                RUN create-report1 (RECID(ar-invl),
                    IF ar-invl.misc THEN ar-invl.i-name ELSE
                    IF ar-invl.i-no NE "" THEN ar-invl.i-no ELSE
                    "AR SALE",
                    STRING(ar-inv.inv-no,"9999999"), "").
            END.
                                       
            DELETE tt-report.
        END.

        ELSE
            IF tt-report.key-10 EQ "ar-cashl" THEN 
            DO:      
                FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK.
                FIND ar-cash  WHERE ar-cash.c-no    EQ ar-cashl.c-no NO-LOCK.

                ASSIGN
                    lExc             = YES
                    tt-report.key-01 = IF cSummary-Details EQ "3" THEN cust.sman ELSE  cust.cust-no
                    tt-report.key-02 = STRING(ar-cashl.inv-no,">>>>>>>9")
                    tt-report.key-03 = "MEMO"
                    tt-report.key-04 = STRING(ar-cashl.inv-no,">>>>>>>9")
                    tt-report.key-05 = tt-report.key-09
                    tt-report.key-06 = cust.sman
                    tt-report.key-07 = tt-report.key-03.

                RELEASE ar-inv.

                RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

                ASSIGN
                    lv-r-no = 0
                    lv-type = "".

                IF AVAILABLE reftable THEN
                    ASSIGN
                        lv-r-no = reftable.val[1]
                        lv-type = reftable.dscr.
                ELSE
                    IF ar-cashl.dscr MATCHES "*OE RETURN*" THEN
                        ASSIGN
                            lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
                            lv-type = TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)).

                IF lv-r-no NE 0 THEN 
                DO:
                    FIND FIRST oe-reth
                        WHERE oe-reth.company EQ cocode
                        AND oe-reth.r-no    EQ lv-r-no
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE oe-reth THEN
                        FIND FIRST ar-inv
                            WHERE ar-inv.company EQ cocode
                            AND ar-inv.cust-no EQ oe-reth.cust-no
                            AND ar-inv.inv-no  EQ oe-reth.inv-no
                            NO-LOCK NO-ERROR.
                END.       

                IF AVAILABLE ar-inv THEN 
                DO:
                    RUN ship-data.
                    
                    IF lv-type EQ "items" THEN 
                    DO:
                        RELEASE ar-invl.
                        FIND FIRST oe-retl
                            WHERE oe-retl.company EQ cocode
                            AND oe-retl.r-no    EQ oe-reth.r-no
                            AND oe-retl.line    EQ ar-cashl.LINE                            
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE oe-retl THEN
                            FIND FIRST ar-invl
                                WHERE ar-invl.company EQ cocode
                                AND ar-invl.cust-no EQ ar-cash.cust-no
                                AND ar-invl.inv-no  EQ ar-cashl.inv-no
                                AND ar-invl.i-no    EQ oe-retl.i-no                                  
                                AND (ar-invl.billable OR NOT ar-invl.misc)
                                NO-LOCK NO-ERROR.
                        IF AVAILABLE ar-invl THEN 
                        DO:
                            RUN create-report1 (RECID(ar-cashl), oe-retl.i-no,
                                tt-report.key-04, "").
                            DELETE tt-report.
                        END.
                    END.                      

                    ELSE
                        IF lv-type   EQ "tax"                  AND                              
                            cust.sman GE cStartSalesGroup      AND
                            cust.sman LE cEndSalesGroup        THEN
                            ASSIGN
                                lExc             = NO
                                tt-report.key-02 = ""
                                tt-report.key-03 = "TAX"
                                tt-report.key-05 = cShipId.
                        ELSE
                            IF 
                                cust.sman GE cStartSalesGroup AND
                                cust.sman LE cEndSalesGroup THEN lExc = NO.
                END.  
                ELSE
                    IF  cust.sman        GE cStartSalesGroup AND
                        cust.sman        LE cEndSalesGroup  THEN lExc = NO.

                IF AVAILABLE tt-report THEN 
                DO:
                    tt-report.key-07 = tt-report.key-03. 
                    IF lExc THEN DELETE tt-report.                    
                END.     
            END.
    END.
   
   
    RELEASE xreport .

    FOR EACH tt-report WHERE tt-report.term-id EQ "",
        FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ tt-report.key-09
        NO-LOCK

        BREAK BY tt-report.key-01
        BY tt-report.key-02
        BY tt-report.key-03
        BY tt-report.key-04
        BY tt-report.key-05
        BY tt-report.key-06

        WITH FRAME itemx DOWN

        TRANSACTION:
        
        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}
        ASSIGN
        iQtyShipped = 0
        dInvoiceAmt = 0
        dUnitPrice  = 0
        iOrderNo    = 0 .

        CREATE w-data.
        ASSIGN
            w-data.i-no   = tt-report.key-07
            w-data.inv-no = int(tt-report.key-04)
            w-data.rec-id = tt-report.rec-id.

        FIND FIRST ar-invl
            WHERE RECID(ar-invl) EQ w-data.rec-id
            NO-LOCK NO-ERROR.

        IF AVAILABLE ar-invl THEN 
        DO:
            FIND ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK.
            ASSIGN
                dtCheckDate = ar-inv.inv-date
                iOrderNo    = ar-invl.ord-no
                dUnitPrice  = ar-invl.unit-pr
                cPriceUom   = ar-invl.pr-uom
                iQtyShipped[1] = ar-invl.ship-qty
                dInvoiceAmt[1] = ar-invl.amt
                dDiscount      = ar-invl.disc
                dPct           = 1.

            IF tt-report.key-10 EQ "FREIGHT" THEN
                ASSIGN
                    dUnitPrice = ar-inv.freight
                    cPriceUom      = ""
                    iQtyShipped[1] = 0
                    dInvoiceAmt[1] = ar-inv.freight
                    dDiscount      = 0.

            ELSE 
            DO:
                DO i = 1 TO 3:
                    IF ar-invl.sman[i] EQ tt-report.key-06 THEN
                        ASSIGN
                            dPct  = ar-invl.s-pct[i] / 100
                            i     = 3.
                END.

                IF dPct EQ 0 THEN
                DO i = 1 TO 3:
                    IF i EQ 1 THEN j = 0.
                    IF ar-invl.sman[i] NE "" THEN j = j + 1.
                    IF i EQ 3 THEN dPct = 1 / j.
                END.

                IF dPct LE 0 OR dPct EQ ? THEN dPct = 1.
            END.

            dInvoiceAmt[1] = dInvoiceAmt[1] * dPct.
        END.

        ELSE 
        DO:
            FIND FIRST ar-cashl
                WHERE RECID(ar-cashl) EQ w-data.rec-id
                NO-LOCK NO-ERROR.

            IF AVAILABLE ar-cashl THEN 
            DO:
                FIND FIRST ar-cash WHERE ar-cash.c-no EQ ar-cashl.c-no NO-LOCK.

                ASSIGN
                    dtCheckDate = ar-cash.check-date
                    iOrderNo    = 0
                    dUnitPrice  = ar-cashl.amt-paid - ar-cashl.amt-disc
                    cPriceUom   = ""
                    iQtyShipped[1] = 0
                    dInvoiceAmt[1] = ar-cashl.amt-paid - ar-cashl.amt-disc
                    dDiscount      = 0.

                RELEASE ar-invl.

                RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

                IF AVAILABLE oe-retl THEN 
                DO:
                    ASSIGN
                        iOrderNo       = oe-retl.ord-no
                        dUnitPrice     = oe-retl.unit-pr
                        cPriceUom      = oe-retl.uom
                        iQtyShipped[1] = - oe-retl.tot-qty-return.

                    FIND FIRST ar-invl
                        WHERE ar-invl.company EQ cocode
                        AND ar-invl.cust-no EQ ar-cash.cust-no
                        AND ar-invl.inv-no  EQ ar-cashl.inv-no
                        AND ar-invl.i-no    EQ oe-retl.i-no
                        NO-LOCK NO-ERROR.

                    IF AVAILABLE ar-invl THEN 
                    DO:
                        /* Added for decimal problem */
                        ASSIGN 
                            dUnitPrice = ar-invl.unit-pr.
                            
                        FIND ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK.

                        DO i = 1 TO 3:
                            IF ar-invl.sman[i] EQ tt-report.key-06 THEN
                                ASSIGN
                                    dPct = ar-invl.s-pct[i] / 100
                                    i     = 3.
                        END.

                        IF dPct EQ 0 THEN
                        DO i = 1 TO 3:
                            IF i EQ 1 THEN j = 0.
                            IF ar-invl.sman[i] NE "" THEN j = j + 1.
                            IF i EQ 3 THEN dPct = 1 / j.
                        END.

                        IF dPct LE 0 OR dPct EQ ? THEN dPct = 1.
                        ASSIGN
                            dInvoiceAmt[1] = dInvoiceAmt[1] * dPct
                            dDiscount      = ar-invl.disc.
                    END.
                END.
            END.
        END.
                                  
        FIND FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ w-data.i-no
            NO-LOCK NO-ERROR.
                
        dFGItemSqft = 0.
        IF AVAILABLE itemfg THEN
            RUN fg/GetFGArea.p (ROWID(itemfg), "SF", OUTPUT dFGItemSqft ).
        
               
        dInvAmount = dInvAmount + dInvoiceAmt[1] .
        
        dProfit = dProfit + ( dInvoiceAmt[1] - (IF AVAILABLE ar-invl THEN ar-invl.cost ELSE 0) ) .
                
        IF  (cSummary-Details EQ "3" AND LAST-OF(tt-report.key-01) )
            OR ( cSummary-Details EQ "1" AND LAST-OF(tt-report.key-02))
            OR ( cSummary-Details EQ "2") THEN 
        DO: 
        
            CREATE ttSalesReport .
            ASSIGN
                ttSalesReport.custNo         = cust.cust-no 
                ttSalesReport.custName       = cust.NAME 
                ttSalesReport.ArClass        = STRING(cust.classID )
                ttSalesReport.custAdd1       = cust.addr[1] 
                ttSalesReport.custAdd2       = cust.addr[2]
                ttSalesReport.custAdd3       = cust.spare-char-3 
                ttSalesReport.custCity       = cust.city
                ttSalesReport.custState      = cust.state 
                ttSalesReport.custZip        = cust.zip
                ttSalesReport.custPhone      = cust.phone 
                ttSalesReport.custContact    = cust.contact         
                ttSalesReport.custNcaisCode  = cust.naicsCode
                ttSalesReport.custMarketSeg  = cust.marketSegment 
                ttSalesReport.custParentCust = cust.parentCust
                ttSalesReport.custAccType    = cust.accountType 
                ttSalesReport.custSplitType  = STRING(cust.splitType) .
        
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ cust.cust-no
                AND shipto.ship-id EQ tt-report.key-05
                NO-ERROR.
          
            IF AVAILABLE shipto THEN
            DO:
                ASSIGN
                    ttSalesReport.shipToName  = shipto.ship-NAME             
                    ttSalesReport.shipToAdd1  = shipto.ship-addr[1] 
                    ttSalesReport.shipToAdd2  = shipto.ship-addr[2]
                    ttSalesReport.shipToAdd3  = shipto.spare-char-3 
                    ttSalesReport.shipToCity  = shipto.ship-city
                    ttSalesReport.shipToState = shipto.ship-state 
                    ttSalesReport.shipToZip   = shipto.ship-zip .               
            END.
        
            ASSIGN
                ttSalesReport.shipToCode    = tt-report.key-05
                ttSalesReport.invNo         = w-data.inv-no                
                ttSalesReport.invSman       = tt-report.key-06             
                ttSalesReport.custPartNo    = IF AVAILABLE itemfg THEN itemfg.part-no ELSE ""
                ttSalesReport.proCat        = IF AVAILABLE itemfg THEN itemfg.procat ELSE ""
                ttSalesReport.itemUnitPrice = IF AVAILABLE itemfg THEN itemfg.sell-price ELSE 0
                ttSalesReport.itemSqFt      = dFGItemSqft 
                ttSalesReport.itemCost      = IF AVAILABLE itemfg THEN itemfg.total-std-cost ELSE 0
                ttSalesReport.itemStyle     = IF AVAILABLE itemfg THEN itemfg.style ELSE ""         
                ttSalesReport.itemWeight    = IF AVAILABLE ar-invl THEN ar-invl.t-weight ELSE 0 .
        
            FIND FIRST oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ cocode
                AND oe-ordl.ord-no EQ  iOrderNo
                AND oe-ordl.i-no EQ w-data.i-no NO-ERROR . 
             
            FIND FIRST oe-ord NO-LOCK
                WHERE oe-ord.company EQ cocode
                AND oe-ord.ord-no EQ  iOrderNo  NO-ERROR .     
                
            ASSIGN   
                ttSalesReport.ordQty             = IF AVAILABLE oe-ordl THEN oe-ordl.qty ELSE 0 
                ttSalesReport.ordOrderNo         = iOrderNo
                ttSalesReport.ordType            = IF AVAILABLE oe-ord THEN oe-ord.type ELSE "" 
                ttSalesReport.ordLine            = IF AVAILABLE oe-ordl THEN oe-ordl.LINE ELSE 0
                ttSalesReport.ordEstNo           = IF AVAILABLE oe-ordl THEN oe-ordl.est-no ELSE "" 
                ttSalesReport.ordDate            = IF AVAILABLE oe-ord AND oe-ord.ord-date NE ? THEN string(oe-ord.ord-date) ELSE ""             
                ttSalesReport.invNo              = IF AVAILABLE ar-invl THEN ar-invl.inv-no ELSE 0 
                ttSalesReport.invDate            = IF AVAILABLE ar-inv AND ar-inv.inv-date NE ? THEN string(ar-inv.inv-date) ELSE "" 
                ttSalesReport.invItemNo          = IF AVAILABLE ar-invl THEN ar-invl.i-no ELSE ""
                ttSalesReport.invQtyUom          = IF AVAILABLE ar-invl THEN ar-invl.cons-uom ELSE "" 
                ttSalesReport.invQtyShip         = iQtyShipped[1] 
                ttSalesReport.invQtyInv          = IF AVAILABLE ar-invl THEN ar-invl.inv-qty ELSE 0         
                ttSalesReport.invUnitPrice       = dUnitPrice  
                ttSalesReport.invPriceUom        = cPriceUom 
                ttSalesReport.invExtPrice        = dInvAmount
                ttSalesReport.invTaxable         = IF AVAILABLE ar-invl THEN ar-invl.tax ELSE FALSE
                ttSalesReport.invCarrier         = IF AVAILABLE ar-inv THEN ar-inv.carrier ELSE "" 
                ttSalesReport.invFob             = IF AVAILABLE ar-inv THEN ar-inv.fob-code ELSE ""         
                ttSalesReport.invFrtPay          = IF AVAILABLE ar-inv THEN ar-inv.frt-pay ELSE "" 
                ttSalesReport.invTaxGroup        = IF AVAILABLE ar-inv THEN ar-inv.tax-code ELSE ""
                ttSalesReport.invTotCost         = IF AVAILABLE ar-invl THEN ar-invl.cost ELSE 0
                ttSalesReport.invTotWeight       = IF AVAILABLE ar-invl THEN ar-invl.t-weight ELSE 0  
                ttSalesReport.invDiscount        = IF AVAILABLE ar-inv THEN ar-inv.disc-taken ELSE 0 
                ttSalesReport.invBolNo           = IF AVAILABLE ar-invl THEN ar-invl.bol-no ELSE 0        
                ttSalesReport.invCustPoNO        = IF AVAILABLE ar-invl THEN ar-invl.po-no ELSE "" 
                ttSalesReport.invAmtDue          = IF AVAILABLE ar-inv THEN ar-inv.due ELSE 0
                ttSalesReport.invPaidDate        = IF AVAILABLE ar-inv AND ar-inv.pay-date NE ? THEN string(ar-inv.pay-date) ELSE ""
                ttSalesReport.invLastPayDate     = IF dtCheckDate NE ? THEN string(dtCheckDate) ELSE ""
                ttSalesReport.invPayCheckNo      = IF AVAILABLE ar-inv THEN STRING(ar-inv.check-no) ELSE ""  
                ttSalesReport.invGLAccount       = IF AVAILABLE ar-invl THEN ar-invl.actnum ELSE ""                 
                ttSalesReport.invFreightAmt      = IF AVAILABLE ar-inv THEN ar-inv.freight ELSE 0 
                ttSalesReport.invTaxAmt          = IF AVAILABLE ar-inv THEN ar-inv.tax-amt ELSE 0         
                ttSalesReport.invCurrCode        = IF AVAILABLE ar-inv THEN ar-inv.curr-code[1] ELSE ""
                ttSalesReport.invCurrRate        = IF AVAILABLE ar-inv THEN ar-inv.ex-rate ELSE 0  
                ttSalesReport.costStdFreight     = IF AVAILABLE ar-invl THEN ar-invl.costStdFreight ELSE 0 
                ttSalesReport.costStdWarehouse   = IF AVAILABLE ar-invl THEN ar-invl.costStdWarehouse ELSE 0         
                ttSalesReport.costStdDeviation   = IF AVAILABLE ar-invl THEN ar-invl.costStdDeviation ELSE 0
                ttSalesReport.costStdManufacture = IF AVAILABLE ar-invl THEN ar-invl.costStdManufacture ELSE 0                 
               .
        
            RELEASE ar-ledger.
            RELEASE gltrans.
            IF AVAILABLE ar-inv THEN
                FIND FIRST ar-ledger
                    WHERE ar-ledger.company  EQ cocode
                    AND ar-ledger.cust-no  EQ ar-inv.cust-no
                    AND ar-ledger.ref-date EQ ar-inv.inv-date
                    AND ar-ledger.ref-num  EQ "INV# " + string(ar-inv.inv-no)
                    NO-LOCK NO-ERROR.
          
            IF AVAILABLE ar-inv AND AVAILABLE ar-ledger  THEN
                FIND FIRST gltrans NO-LOCK
                    WHERE  gltrans.company EQ cocode 
                    AND gltrans.trnum EQ  ar-ledger.tr-num NO-ERROR .   
             
            ASSIGN
                ttSalesReport.invGLPeriod = IF AVAILABLE gltrans THEN gltrans.period ELSE 0
                ttSalesReport.invGLYear   = IF AVAILABLE gltrans THEN YEAR(gltrans.tr-date) ELSE 0
                ttSalesReport.invGLRun    = IF AVAILABLE gltrans THEN gltrans.trnum ELSE 0 .               
        
            RELEASE job-hdr .
            IF AVAILABLE ar-invl THEN
                FIND FIRST job-hdr NO-LOCK
                    WHERE job-hdr.company EQ cocode
                    AND job-hdr.job-no EQ ar-invl.job-no
                    AND job-hdr.job-no2 EQ ar-invl.job-no2 NO-ERROR .
             
            RELEASE eb.  
            IF AVAILABLE ar-invl AND AVAILABLE job-hdr THEN
                FIND FIRST eb NO-LOCK
                    WHERE eb.company EQ cocode
                    AND eb.est-no EQ  job-hdr.est-no
                    AND eb.stock-no EQ job-hdr.i-no NO-ERROR.
        
            ASSIGN
                ttSalesReport.itemFlute = IF AVAILABLE eb THEN eb.flute ELSE "" 
                ttSalesReport.itemTest  = IF AVAILABLE eb THEN eb.test ELSE ""
                ttSalesReport.itemLen   = IF AVAILABLE eb THEN eb.len ELSE 0
                ttSalesReport.itemWid   = IF AVAILABLE eb THEN eb.wid ELSE 0
                ttSalesReport.itemDep   = IF AVAILABLE eb THEN eb.dep ELSE 0 
                ttSalesReport.jobNo     = IF AVAILABLE ar-invl THEN ar-invl.job-no ELSE "" 
                ttSalesReport.jobNo2    = IF AVAILABLE ar-invl THEN ar-invl.job-no2 ELSE 0 
                ttSalesReport.jobForm   = IF AVAILABLE job-hdr THEN job-hdr.frm ELSE 0
                ttSalesReport.jobBlank  = IF AVAILABLE job-hdr THEN job-hdr.blank-no ELSE 0
                ttSalesReport.jobColor  = IF AVAILABLE eb THEN eb.i-col ELSE 0  
                /* ttSalesReport.jobShipDate = IF AVAIL ar-inv THEN ar-inv.curr-code[1] ELSE ""
                 ttSalesReport.jobPostDate = IF AVAIL job-hdr THEN job-hdr.due-date ELSE 01/01/0001*/  .
        
            RELEASE sman.
            RELEASE salesgrpMember.
       
            FIND FIRST sman NO-LOCK
                WHERE sman.company EQ cocode 
                AND sman.sman EQ report.key-06 NO-ERROR .
            IF AVAILABLE sman THEN       
                FIND FIRST salesgrpMember NO-LOCK
                    WHERE salesgrpMember.company EQ cocode
                    AND salesgrpMember.sman    = sman.sman NO-ERROR . 
            
            dPer = (dProfit * 100 / dInvAmount ).
        
            ASSIGN
                ttSalesReport.SalesManager   = IF AVAILABLE sman THEN sman.salesManager ELSE "" 
                ttSalesReport.SalesGroupName = IF AVAILABLE salesgrpMember AND AVAILABLE sman AND sman.hasMembers THEN STRING(salesgrpMember.salesmanID + " - " + salesgrpMember.salesmanName) ELSE "" 
                ttSalesReport.profit         = dProfit
                ttSalesReport.profitPer      = IF dPer NE ? THEN dPer ELSE 0  . 
                
            IF AVAILABLE ar-invl THEN
            DO:               
                
                RUN sys/ref/convsuom.p (ar-invl.cons-uom,
                    "M",
                    ar-invl.sf-sht,
                    ar-invl.qty,
                    OUTPUT dOutQty).                                   
                 
                ASSIGN
                    ttSalesReport.invExtPricePerM = IF (dOutQty * ar-invl.unit-pr) EQ 0
                                                    then (ar-invl.qty * ar-invl.unit-pr)
                                                    else (dOutQty * ar-invl.unit-pr) .
                                                 
                RUN sys/ref/convsuom.p (ar-invl.cons-uom,
                    "MSF",
                    ar-invl.sf-sht,
                    ar-invl.qty,
                    OUTPUT dOutQty).                             
                 
                ttSalesReport.invExtPricePerTon = IF (dOutQty * ar-invl.unit-pr) EQ 0
                THEN (ar-invl.qty * ar-invl.unit-pr)
                ELSE (dOutQty * ar-invl.unit-pr) .
                                                 
                RUN sys/ref/convsuom.p (ar-invl.cons-uom,
                    "TON",
                    ar-invl.sf-sht,
                    ar-invl.qty,
                    OUTPUT dOutQty).    
                    
                ttSalesReport.invExtPricePerMsf = IF (dOutQty * ar-invl.unit-pr) EQ 0
                THEN (ar-invl.qty * ar-invl.unit-pr)
                ELSE (dOutQty * ar-invl.unit-pr) .                                               
            END.
        
            dInvAmount = 0 .
            dTotalSqft = 0.
            dProfit = 0.          
        END.         

        DELETE w-data. 
    END.
              

END PROCEDURE.


PROCEDURE ship-data :
    RELEASE shipto.

    IF AVAILABLE ar-inv AND ar-inv.ship-id NE "" THEN
        FIND FIRST shipto
            WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ ar-inv.cust-no
            AND shipto.ship-id EQ ar-inv.ship-id
            NO-LOCK NO-ERROR.

    IF AVAILABLE shipto THEN
        ASSIGN
            cShipId = ar-inv.ship-id.              
    ELSE
        IF AVAILABLE ar-inv AND ar-inv.sold-id NE "" THEN
            ASSIGN
                cShipId = ar-inv.sold-id.               
        ELSE
            ASSIGN
                cShipId = ar-inv.cust-no .
END PROCEDURE.


PROCEDURE create-report :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid  AS   RECID            NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-03 LIKE tt-report.key-03 NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-04 LIKE tt-report.key-04 NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-10 LIKE tt-report.key-10 NO-UNDO.


    CREATE xreport.

    ASSIGN
        lExc            = NO
        xreport.term-id = ""
        xreport.rec-id  = ip-recid
        xreport.key-01  = IF cSummary-Details EQ "3" THEN v-sman-no ELSE
                   tt-report.key-09
        xreport.key-02  = STRING(ar-invl.inv-no,"99999999") 
        xreport.key-03  = ip-key-03
        xreport.key-04  = ip-key-04
        xreport.key-05  = cShipId
        xreport.key-06  = v-sman-no
        xreport.key-07  = xreport.key-03
        xreport.key-09  = tt-report.key-09
        xreport.key-10  = ip-key-10 
        xreport.v-po    = ar-invl.po-no .  
        
END PROCEDURE.


PROCEDURE create-report1 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid  AS   RECID         NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-03 LIKE report.key-03 NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-04 LIKE report.key-04 NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-10 LIKE report.key-10 NO-UNDO.

    DO i = 1 TO 3:
        v-sman-no = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman
        ELSE ar-invl.sman[i].

        IF v-sman-no   LT cStartSalesGroup                         OR
            v-sman-no   GT cEndSalesGroup                         OR
            (i NE 1 AND
            (v-sman-no EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.
        RUN create-report (ip-recid, ip-key-03, ip-key-04, ip-key-10).
        LEAVE.
    END.

END PROCEDURE.
