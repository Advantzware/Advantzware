/*------------------------------------------------------------------------
  File: dbinvmh.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Invoice Highlights.edp */
{aoa/tempTable/ttInvoiceHighlights.i}

DEFINE TEMP-TABLE ttReport NO-UNDO
    FIELD company    AS CHARACTER 
    FIELD custNo     AS CHARACTER 
    FIELD tableName  AS CHARACTER 
    FIELD tableRecID AS RECID 
        INDEX idx custNo tableName
   .
/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttInvoiceHighlights.
{aoa/includes/pInvoiceHighlights.i}

/* local variables */
DEFINE VARIABLE dtFromDate AS DATE    NO-UNDO.
DEFINE VARIABLE dtToDate   AS DATE    NO-UNDO.
DEFINE VARIABLE dCost      AS DECIMAL NO-UNDO.
DEFINE VARIABLE dtDateIdx  AS DATE    NO-UNDO.
DEFINE VARIABLE dtAsOfDate AS DATE    NO-UNDO.

/* load parameter values from above record into variables */
ASSIGN
    dtFromDate = DATE(1,1,iYear - 1)
    dtToDate   = DATE(12,31,iYear)
   .

FOR EACH company NO-LOCK
    WHERE company.company GT ""
    :
    DO dtDateIdx = dtFromDate TO dtToDate :
        CREATE ttInvoiceHighlights.
        ASSIGN
            ttInvoiceHighlights.dateIdx = dtDateIdx 
            ttInvoiceHighlights.company = company.company
           .
        RELEASE ttInvoiceHighlights.
    END. /* do dtDateIdx */
END. /* each company */

FOR EACH cust NO-LOCK 
    WHERE cust.company GT ""
    :
 
    FOR EACH ar-inv FIELDS () NO-LOCK 
        WHERE ar-inv.company  EQ cust.company
          AND ar-inv.posted   EQ YES 
          AND ar-inv.cust-no  EQ cust.cust-no
          AND ar-inv.inv-date GE dtFromDate 
          AND ar-inv.inv-date LE dtToDate 
          AND ar-inv.type     NE "FC"
        : 
    
        CREATE ttReport.
        ASSIGN 
          ttReport.company    = cust.company
          ttReport.custNo     = cust.cust-no
          ttReport.tableName  = "ar-inv"
          ttReport.tableRecID = RECID (ar-inv).
    END. /* each ar-inv */
 
    FOR EACH ar-cash FIELDS (c-no) NO-LOCK 
        WHERE ar-cash.company    EQ cust.company 
          AND ar-cash.cust-no    EQ cust.cust-no 
          AND ar-cash.check-date GE dtFromDate 
          AND ar-cash.check-date LE dtToDate 
          AND ar-cash.posted     EQ YES ,
        EACH ar-cashl FIELDS (company actnum) NO-LOCK
        WHERE ar-cashl.c-no      EQ ar-cash.c-no 
          AND ar-cashl.posted    EQ YES 
          AND ar-cashl.memo      EQ YES 
          AND CAN-FIND(FIRST account
                       WHERE account.company EQ ar-cashl.company 
                         AND account.actnum  EQ ar-cashl.actnum 
                         AND account.type    EQ "R")
        :
 
        CREATE ttReport.
        ASSIGN 
            ttReport.company    = cust.company
            ttReport.custNo     = cust.cust-no
            ttReport.tableName  = "ar-cashl"
            ttReport.tableRecID = RECID (ar-cashl)
           .
    END. /* each ar-cash */
END. /* each cust */

FOR EACH ttReport : 
    IF ttReport.tableName EQ "ar-inv" THEN DO :
        FIND ar-inv NO-LOCK WHERE RECID (ar-inv) EQ ttReport.tableRecID.
 
        FIND FIRST ttInvoiceHighlights
             WHERE ttInvoiceHighlights.company EQ ar-inv.company
               AND ttInvoiceHighlights.dateIdx EQ ar-inv.inv-date.
    
        FOR EACH ar-invl NO-LOCK 
            WHERE ar-invl.x-no EQ ar-inv.x-no
            :
    
            FIND FIRST itemfg NO-LOCK 
                 WHERE itemfg.company EQ ar-inv.company
                   AND itemfg.i-no    EQ ar-invl.i-no
                 NO-ERROR.
                 
            RUN salrep/salecost.p (3 , /*Invoice Cost*/
                                   ROWID (ar-invl) ,
                                   ar-invl.job-no ,
                                   ar-invl.job-no2 ,
                                   ar-invl.ship-qty ,
                                   OUTPUT dCost ).
            IF YEAR (ttInvoiceHighlights.dateIdx) EQ iYear THEN
            ASSIGN 
                ttInvoiceHighlights.company = ar-inv.company
                ttInvoiceHighlights.cyrQty  = ttInvoiceHighlights.cyrQty + ar-invl.ship-qty
                ttInvoiceHighlights.cyrMSF  = ttInvoiceHighlights.cyrMSF +
                                      ( IF ar-invl.amt-msf NE 0 THEN ar-invl.amt-msf
                                        ELSE IF AVAILABLE itemfg THEN
                                      ( ar-invl.ship-qty * itemfg.t-sqft / 1000 )
                                        ELSE 0 )
                ttInvoiceHighlights.cyrCost = ttInvoiceHighlights.cyrCost + dCost
                ttInvoiceHighlights.cyrTons = ttInvoiceHighlights.cyrTons +
                                     (( IF ar-invl.t-weight NE 0 THEN ar-invl.t-weight
                                        ELSE IF AVAILABLE itemfg THEN 
                                      ( ar-invl.ship-qty * itemfg.weight-100 / 100 )
                                        ELSE 0) / 2000 )
               ttInvoiceHighlights.cyrSales = ttInvoiceHighlights.cyrSales + ar-invl.amt
              .
            ELSE 
            ASSIGN 
                ttInvoiceHighlights.company = ar-inv.company
                ttInvoiceHighlights.lyrQty  = ttInvoiceHighlights.lyrQty + ar-invl.ship-qty
                ttInvoiceHighlights.lyrMSF  = ttInvoiceHighlights.lyrMSF +
                                      ( IF ar-invl.amt-msf NE 0 THEN ar-invl.amt-msf
                                        ELSE IF AVAILABLE itemfg THEN
                                      ( ar-invl.ship-qty * itemfg.t-sqft / 1000 )
                                        ELSE 0 )
                ttInvoiceHighlights.lyrCost = ttInvoiceHighlights.lyrCost + dCost
                ttInvoiceHighlights.lyrTons = ttInvoiceHighlights.lyrTons +
                                     (( IF ar-invl.t-weight NE 0 THEN ar-invl.t-weight
                                        ELSE IF AVAILABLE itemfg THEN 
                                      ( ar-invl.ship-qty * itemfg.weight-100 / 100 )
                                        ELSE 0) / 2000 )
               ttInvoiceHighlights.lyrSales = ttInvoiceHighlights.lyrSales + ar-invl.amt
              .
        END. /* each ar-invl */
    END. /* if ar-inv */
    ELSE IF ttReport.tableName EQ "ar-cashl" THEN DO :
        FIND ar-cashl NO-LOCK WHERE RECID (ar-cashl) EQ ttReport.tableRecID.
        FIND ar-cash  NO-LOCK WHERE ar-cash.c-no     EQ ar-cashl.c-no. 
        FIND FIRST ttInvoiceHighlights
             WHERE ttInvoiceHighlights.company EQ ar-cashl.company
               AND ttInvoiceHighlights.dateIdx EQ ar-cash.check-date.
 
        RUN salrep/getoeret.p ( ROWID(ar-cashl), BUFFER oe-retl ).
 
        IF AVAILABLE oe-retl THEN DO:
            FIND FIRST ar-invl NO-LOCK 
                 WHERE ar-invl.company   EQ ar-cashl.company
                   AND ar-invl.cust-no   EQ ar-cash.cust-no
                   AND ar-invl.inv-no    EQ ar-cashl.inv-no
                   AND ar-invl.i-no      EQ oe-retl.i-no
                   AND (ar-invl.billable OR NOT ar-invl.misc)
                 NO-ERROR.
 
            IF AVAILABLE ar-invl THEN DO :          
                FIND FIRST itemfg NO-LOCK 
                     WHERE itemfg.company EQ ar-cashl.company
                       AND itemfg.i-no    EQ ar-invl.i-no
                     NO-ERROR.
               
                RUN salrep/salecost.p (3 , /*Invoice Cost*/
                                       ROWID(ar-invl) ,
                                       oe-retl.job-no ,
                                       oe-retl.job-no2 ,
                                       oe-retl.tot-qty-return ,
                                       OUTPUT dCost).
            
                IF YEAR (ttInvoiceHighlights.dateIdx) EQ iYear THEN
                ASSIGN 
                    ttInvoiceHighlights.company = ar-cashl.company
                    ttInvoiceHighlights.cyrQty  = ttInvoiceHighlights.cyrQty - oe-retl.tot-qty-return
                    ttInvoiceHighlights.cyrMSF  = ttInvoiceHighlights.cyrMSF - 
                                          ( IF AVAILABLE itemfg THEN 
                                          ( oe-retl.tot-qty-return * itemfg.t-sqft / 1000 )
                                            ELSE 0 )
                    ttInvoiceHighlights.cyrTons = ttInvoiceHighlights.cyrTons +
                                         (( IF AVAILABLE itemfg THEN 
                                          ( oe-retl.tot-qty-return * itemfg.weight-100 / 100 )
                                            ELSE 0) / 2000 )
                    ttInvoiceHighlights.cyrCost = ttInvoiceHighlights.cyrCost + dCost
                   .
                ELSE 
                ASSIGN 
                    ttInvoiceHighlights.company = ar-cashl.company
                    ttInvoiceHighlights.lyrQty  = ttInvoiceHighlights.lyrQty - oe-retl.tot-qty-return
                    ttInvoiceHighlights.lyrMSF  = ttInvoiceHighlights.lyrMSF - 
                                          ( IF AVAILABLE itemfg THEN 
                                          ( oe-retl.tot-qty-return * itemfg.t-sqft / 1000 )
                                            ELSE 0 )
                    ttInvoiceHighlights.lyrTons = ttInvoiceHighlights.lyrTons +
                                         (( IF AVAILABLE itemfg THEN 
                                          ( oe-retl.tot-qty-return * itemfg.weight-100 / 100 )
                                            ELSE 0) / 2000 )
                    ttInvoiceHighlights.lyrCost = ttInvoiceHighlights.lyrCost + dCost
                   .
            END. /* avail ar-invl */
        END. /* avail oe-retl */
        IF YEAR (ttInvoiceHighlights.dateIdx) EQ iYear THEN
        ASSIGN ttInvoiceHighlights.cyrSales = ttInvoiceHighlights.cyrSales + ar-cashl.amt-paid - ar-cashl.amt-disc.
        ELSE 
        ASSIGN ttInvoiceHighlights.lyrSales = ttInvoiceHighlights.lyrSales + ar-cashl.amt-paid - ar-cashl.amt-disc.
    END. /* if ar-cashl */ 
END. /* each ttreport */

FOR EACH ttInvoiceHighlights :
    ASSIGN 
        ttInvoiceHighlights.cyrNetProfit = IF ttInvoiceHighlights.cyrSales NE 0 THEN
                                 ( ttInvoiceHighlights.cyrSales - ttInvoiceHighlights.cyrCost ) / ttInvoiceHighlights.cyrSales
                                   ELSE 0 
        ttInvoiceHighlights.lyrNetProfit = IF ttInvoiceHighlights.lyrSales NE 0 THEN
                                 ( ttInvoiceHighlights.lyrSales - ttInvoiceHighlights.lyrCost ) / ttInvoiceHighlights.lyrSales
                                   ELSE 0
       .
END. /* each ttInvoiceHighlights */
