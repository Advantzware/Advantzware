&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoaAppSrv/aoaHS.p
    Purpose     : AppServer Functions and Procedures

    Syntax      : 

    Description : AppServer Functions and Procedures

    Author(s)   : Ron Stark
    Created     : 3.23.2016
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Invoice Hightlights.edp */
DEFINE TEMP-TABLE ttRawSales NO-UNDO
    FIELD company      AS CHARACTER LABEL "Company"
    FIELD salesYear    AS INTEGER   LABEL "Year"
    FIELD dateIdx      AS DATE      LABEL "Date"
    FIELD lyrSales     AS DECIMAL   LABEL "LYR Sales"
    FIELD lyrCost      AS DECIMAL   LABEL "LYR Cost"
    FIELD lyrQty       AS DECIMAL   LABEL "LYR Qty"
    FIELD lyrMSF       AS DECIMAL   LABEL "LYR MSF"
    FIELD lyrTons      AS DECIMAL   LABEL "LYR Tons"
    FIELD lyrNetProfit AS DECIMAL   LABEL "LYR Net Profit"
    FIELD cyrSales     AS DECIMAL   LABEL "CYR Sales"
    FIELD cyrCost      AS DECIMAL   LABEL "CYR Cost"
    FIELD cyrQty       AS DECIMAL   LABEL "CYR Qty"
    FIELD cyrMSF       AS DECIMAL   LABEL "CYR MSF"
    FIELD cyrTons      AS DECIMAL   LABEL "CYR Tons"
    FIELD cyrNetProfit AS DECIMAL   LABEL "CYR Net Profit"
        INDEX idx company dateIdx ASCENDING .

DEFINE TEMP-TABLE ttReport NO-UNDO
    FIELD company    AS CHARACTER 
    FIELD custNo     AS CHARACTER 
    FIELD tableName  AS CHARACTER 
    FIELD tableRecID AS RECID 
        INDEX idx custNo tableName .
/* Invoice Hightlights.edp */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fInvoiceHighlights) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fInvoiceHighlights Procedure 
FUNCTION fInvoiceHighlights RETURNS HANDLE
  ( ipiYear AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pRawSalesProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRawSalesProc Procedure 
PROCEDURE pRawSalesProc :
/*------------------------------------------------------------------------------
  Purpose:     Invoice Hightlights.edp
  Parameters:  base current year
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiYear AS INTEGER NO-UNDO .

    DEFINE VARIABLE cCompany   AS CHARACTER NO-UNDO .
    DEFINE VARIABLE dtFromDate AS DATE      NO-UNDO .
    DEFINE VARIABLE dtToDate   AS DATE      NO-UNDO .
    DEFINE VARIABLE dCost      AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dtDateIdx  AS DATE      NO-UNDO .
    DEFINE VARIABLE dtAsOfDate AS DATE      NO-UNDO .
    
    ASSIGN
        dtFromDate = DATE(1,1,ipiYear - 1)
        dtToDate   = DATE(12,31,ipiYear)
        .
 
    FOR EACH company NO-LOCK
        WHERE company.company GT ""
        :
        DO dtDateIdx = dtFromDate TO dtToDate :
            CREATE ttRawSales .
            ASSIGN
                ttRawSales.dateIdx    = dtDateIdx 
                ttRawSales.company    = company.company
                .
            RELEASE ttRawSales .
        END . /* do dtDateIdx */
    END . /* each company */
    
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
              ttReport.company     = cust.company
              ttReport.custNo      = cust.cust-no
              ttReport.tableName   = "ar-inv"
              ttReport.tableRecID  = RECID (ar-inv).
        END . /* each ar-inv */
     
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
     
            CREATE ttReport .
            ASSIGN 
                ttReport.company     = cust.company
                ttReport.custNo      = cust.cust-no
                ttReport.tableName   = "ar-cashl"
                ttReport.tableRecID  = RECID (ar-cashl)
                .
        END . /* each ar-cash */
    END . /* each cust */
    
    FOR EACH ttReport : 
        IF ttReport.tableName EQ "ar-inv" THEN DO :
            FIND ar-inv NO-LOCK WHERE RECID (ar-inv) EQ ttReport.tableRecID .
     
            FIND FIRST ttRawSales
                 WHERE ttRawSales.company EQ ar-inv.company
                   AND ttRawSales.dateIdx EQ ar-inv.inv-date .
        
            FOR EACH ar-invl NO-LOCK 
                WHERE ar-invl.x-no EQ ar-inv.x-no
                :
        
                FIND FIRST itemfg NO-LOCK 
                     WHERE itemfg.company EQ ar-inv.company
                       AND itemfg.i-no    EQ ar-invl.i-no
                     NO-ERROR .
                     
                RUN salrep/salecost.p (3 , /*Invoice Cost*/
                                       ROWID (ar-invl) ,
                                       ar-invl.job-no ,
                                       ar-invl.job-no2 ,
                                       ar-invl.ship-qty ,
                                       OUTPUT dCost ).
                IF YEAR (ttRawSales.dateIdx) EQ ipiYear THEN
                ASSIGN 
                    ttRawSales.company   = ar-inv.company
                    ttRawSales.cyrQty    = ttRawSales.cyrQty + ar-invl.ship-qty
                    ttRawSales.cyrMSF    = ttRawSales.cyrMSF +
                                          ( IF ar-invl.amt-msf NE 0 THEN ar-invl.amt-msf
                                            ELSE IF AVAILABLE itemfg THEN
                                          ( ar-invl.ship-qty * itemfg.t-sqft / 1000 )
                                            ELSE 0 )
                    ttRawSales.cyrCost   = ttRawSales.cyrCost + dCost
                    ttRawSales.cyrTons   = ttRawSales.cyrTons +
                                         (( IF ar-invl.t-weight NE 0 THEN ar-invl.t-weight
                                            ELSE IF AVAILABLE itemfg THEN 
                                          ( ar-invl.ship-qty * itemfg.weight-100 / 100 )
                                            ELSE 0) / 2000 )
                   ttRawSales.cyrSales   = ttRawSales.cyrSales + ar-invl.amt
                   .
                ELSE 
                ASSIGN 
                    ttRawSales.company   = ar-inv.company
                    ttRawSales.lyrQty    = ttRawSales.lyrQty + ar-invl.ship-qty
                    ttRawSales.lyrMSF    = ttRawSales.lyrMSF +
                                          ( IF ar-invl.amt-msf NE 0 THEN ar-invl.amt-msf
                                            ELSE IF AVAILABLE itemfg THEN
                                          ( ar-invl.ship-qty * itemfg.t-sqft / 1000 )
                                            ELSE 0 )
                    ttRawSales.lyrCost   = ttRawSales.lyrCost + dCost
                    ttRawSales.lyrTons   = ttRawSales.lyrTons +
                                         (( IF ar-invl.t-weight NE 0 THEN ar-invl.t-weight
                                            ELSE IF AVAILABLE itemfg THEN 
                                          ( ar-invl.ship-qty * itemfg.weight-100 / 100 )
                                            ELSE 0) / 2000 )
                   ttRawSales.lyrSales   = ttRawSales.lyrSales + ar-invl.amt
                   .
            END . /* each ar-invl */
        END . /* if ar-inv */
        ELSE IF ttReport.tableName EQ "ar-cashl" THEN DO :
            FIND ar-cashl NO-LOCK WHERE RECID (ar-cashl) EQ ttReport.tableRecID .
            FIND ar-cash  NO-LOCK WHERE ar-cash.c-no     EQ ar-cashl.c-no . 
            FIND FIRST ttRawSales
                 WHERE ttRawSales.company EQ ar-cashl.company
                   AND ttRawSales.dateIdx EQ ar-cash.check-date .
     
            RUN salrep/getoeret.p ( ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl ).
     
            IF AVAILABLE oe-retl THEN DO:
                FIND FIRST ar-invl NO-LOCK 
                     WHERE ar-invl.company   EQ ar-cashl.company
                       AND ar-invl.cust-no   EQ ar-cash.cust-no
                       AND ar-invl.inv-no    EQ ar-cashl.inv-no
                       AND ar-invl.i-no      EQ oe-retl.i-no
                       AND (ar-invl.billable OR NOT ar-invl.misc)
                     NO-ERROR .
     
                IF AVAILABLE ar-invl THEN DO :          
                    FIND FIRST itemfg NO-LOCK 
                         WHERE itemfg.company EQ ar-cashl.company
                           AND itemfg.i-no    EQ ar-invl.i-no
                         NO-ERROR .
                   
                    RUN salrep/salecost.p (3 , /*Invoice Cost*/
                                           ROWID(ar-invl) ,
                                           oe-retl.job-no ,
                                           oe-retl.job-no2 ,
                                           oe-retl.tot-qty-return ,
                                           OUTPUT dCost) .
                
                    IF YEAR (ttRawSales.dateIdx) EQ ipiYear THEN
                    ASSIGN 
                        ttRawSales.company   = ar-cashl.company
                        ttRawSales.cyrQty    = ttRawSales.cyrQty - oe-retl.tot-qty-return
                        ttRawSales.cyrMSF    = ttRawSales.cyrMSF - 
                                              ( IF AVAILABLE itemfg THEN 
                                              ( oe-retl.tot-qty-return * itemfg.t-sqft / 1000 )
                                                ELSE 0 )
                        ttRawSales.cyrTons   = ttRawSales.cyrTons +
                                             (( IF AVAILABLE itemfg THEN 
                                              ( oe-retl.tot-qty-return * itemfg.weight-100 / 100 )
                                                ELSE 0) / 2000 )
                        ttRawSales.cyrCost   = ttRawSales.cyrCost + dCost
                        .
                    ELSE 
                    ASSIGN 
                        ttRawSales.company   = ar-cashl.company
                        ttRawSales.lyrQty    = ttRawSales.lyrQty - oe-retl.tot-qty-return
                        ttRawSales.lyrMSF    = ttRawSales.lyrMSF - 
                                              ( IF AVAILABLE itemfg THEN 
                                              ( oe-retl.tot-qty-return * itemfg.t-sqft / 1000 )
                                                ELSE 0 )
                        ttRawSales.lyrTons   = ttRawSales.lyrTons +
                                             (( IF AVAILABLE itemfg THEN 
                                              ( oe-retl.tot-qty-return * itemfg.weight-100 / 100 )
                                                ELSE 0) / 2000 )
                        ttRawSales.lyrCost   = ttRawSales.lyrCost + dCost
                        .
                END . /* avail ar-invl */
            END . /* avail oe-retl */
            IF YEAR (ttRawSales.dateIdx) EQ ipiYear THEN
            ASSIGN ttRawSales.cyrSales = ttRawSales.cyrSales + ar-cashl.amt-paid - ar-cashl.amt-disc .
            ELSE 
            ASSIGN ttRawSales.lyrSales = ttRawSales.lyrSales + ar-cashl.amt-paid - ar-cashl.amt-disc .
        END . /* if ar-cashl */ 
    END . /* each ttreport */
 
    FOR EACH ttRawSales :
        ASSIGN 
            ttRawSales.cyrNetProfit = IF ttRawSales.cyrSales NE 0 THEN
                                     ( ttRawSales.cyrSales - ttRawSales.cyrCost ) / ttRawSales.cyrSales
                                       ELSE 0 
            ttRawSales.lyrNetProfit = IF ttRawSales.lyrSales NE 0 THEN
                                     ( ttRawSales.lyrSales - ttRawSales.lyrCost ) / ttRawSales.lyrSales
                                       ELSE 0
            .
    END. /* each ttrawsales */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fInvoiceHighlights) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fInvoiceHighlights Procedure 
FUNCTION fInvoiceHighlights RETURNS HANDLE
  ( ipiYear AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  Invoice Hightlights.edp
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttRawSales .
    EMPTY TEMP-TABLE ttReport .

    RUN pRawSalesProc (ipiYear).
    
    RETURN TEMP-TABLE ttRawSales:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

