&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoaAppSrv/aoaAR.p
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

/* Customer.rpa */
DEFINE TEMP-TABLE ttCust NO-UNDO RCODE-INFORMATION
    FIELD company  LIKE cust.company  
    FIELD custNo   LIKE cust.cust-no
    FIELD custName LIKE cust.name
        INDEX ttCust company custNo
        .
/* Customer.rpa */

/* Commision Cash Receipt.rpa */
DEFINE TEMP-TABLE ttCommissionCashReceipt NO-UNDO RCODE-INFORMATION
    FIELD salesRep    AS CHARACTER LABEL "Sales Rep"
    FIELD custNo      AS CHARACTER LABEL "Customer"
    FIELD invoiceNo   AS INTEGER   LABEL "Invoice"
    FIELD invDate     AS DATE      LABEL "Inv Date"
    FIELD custPart    AS CHARACTER LABEL "Cust Part"
    FIELD orderNo     AS CHARACTER LABEL "Order"
    FIELD invQty      AS DECIMAL   LABEL "Quantity"
    FIELD invAmt      AS DECIMAL   LABEL "Inv Amt"
    FIELD datePaid    AS DATE      LABEL "Date Paid"
    FIELD amtPaid     AS DECIMAL   LABEL "Amt Paid"
    FIELD amtD        AS DECIMAL   LABEL "Amt D"
    FIELD delta       AS DECIMAL   LABEL "Delta"
    FIELD grossProfit AS DECIMAL   LABEL "Gross Profit"
    FIELD cost        AS DECIMAL   LABEL "Cost"
    FIELD commission  AS DECIMAL   LABEL "Commission"
    FIELD commAmt     AS DECIMAL   LABEL "Comm Amt"    
    FIELD commPct     AS DECIMAL   LABEL "Comm Pct"
    FIELD cashDate    AS DATE      LABEL "Cash Date"
    FIELD rec-id      AS RECID
    FIELD row-id      AS ROWID
    .
{sys/ref/CustList.i NEW}
/* Commision Cash Receipt.rpa */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fCommissionCashReceipt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCommissionCashReceipt Procedure 
FUNCTION fCommissionCashReceipt RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fCustomers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCustomers Procedure 
FUNCTION fCustomers RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER )  FORWARD.

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

&IF DEFINED(EXCLUDE-pBuildCustList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildCustList Procedure 
PROCEDURE pBuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Commission Cash Receipt.rpa
  Parameters:  Company, Use List?, Start Cust, End Cust, ID
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplList      AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcStartCust AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEndCust   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcID        AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bCust FOR cust.
    
    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttCustList.

    IF iplList THEN
    RUN sys/ref/CustList.p (ipcCompany, ipcID, YES, OUTPUT lActive).
    ELSE DO:
        FOR EACH bCust NO-LOCK
            WHERE bCust.company EQ ipcCompany
              AND bCust.cust-no GE ipcStartCust
              AND bCust.cust-no LE ipcEndCust
            :
            CREATE ttCustList.
            ASSIGN 
                ttCustList.cust-no = bCust.cust-no
                ttCustList.log-fld = YES
                .
        END. /* each bcust */
    END. /* else */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCommCashReceipt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCommCashReceipt Procedure 
PROCEDURE pCommCashReceipt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplCalc     AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplDetailed AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcDelta    AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cCommBasis AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dProfit    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE p-sman     AS CHARACTER NO-UNDO.

    FOR EACH ttCommissionCashReceipt,
        FIRST ar-invl NO-LOCK WHERE RECID(ar-invl) EQ ttCommissionCashReceipt.rec-id,
        FIRST ar-inv NO-LOCK WHERE ar-inv.x-no EQ ar-invl.x-no,
        FIRST cust NO-LOCK
        WHERE cust.company EQ ar-inv.company
          AND cust.cust-no EQ ar-inv.cust-no
        BREAK BY ttCommissionCashReceipt.salesRep
              BY ttCommissionCashReceipt.custNo
              BY ttCommissionCashReceipt.invoiceNo
              BY ttCommissionCashReceipt.row-id
              BY ttCommissionCashReceipt.rec-id
              BY ROWID(ttCommissionCashReceipt)
        :

      FIND FIRST sman NO-LOCK
          WHERE sman.company EQ ipcCompany
            AND sman.sman    EQ ttCommissionCashReceipt.salesRep
          NO-ERROR.

      RUN custom/combasis.p (
          ipcCompany,
          ttCommissionCashReceipt.salesRep,
          cust.type,
          "",
          0,
          cust.cust-no,
          OUTPUT cCommBasis
          ).

      RELEASE prep.
      RELEASE itemfg.

      IF ar-invl.misc THEN
      FIND FIRST prep NO-LOCK
           WHERE prep.company EQ ipcCompany
             AND prep.code    EQ ar-invl.i-name
           NO-ERROR.
      ELSE
      FIND FIRST itemfg NO-LOCK
           WHERE itemfg.company EQ ipcCompany
             AND itemfg.i-no    EQ ar-invl.i-no
           NO-ERROR.

      RUN custom/combasis.p (
          ipcCompany,
          ttCommissionCashReceipt.salesRep,
          cust.type,
         (IF AVAIL itemfg THEN itemfg.procat ELSE ""),
          0,
          cust.cust-no,
          OUTPUT cCommBasis
          ).

      IF ttCommissionCashReceipt.cost EQ ? THEN ttCommissionCashReceipt.cost = 0.
      IF ttCommissionCashReceipt.commission EQ ? THEN ttCommissionCashReceipt.commission = 0.
      
      ttCommissionCashReceipt.commission = 0.
      
      ASSIGN
          dProfit = ttCommissionCashReceipt.invAmt - ttCommissionCashReceipt.cost
          ttCommissionCashReceipt.commAmt = IF cCommBasis EQ "G" THEN (dProfit * ttCommissionCashReceipt.commission / 100)
                     ELSE (ttCommissionCashReceipt.invAmt * (ttCommissionCashReceipt.commission / 100))
          ttCommissionCashReceipt.commission = ttCommissionCashReceipt.commission
          ttCommissionCashReceipt.grossProfit   = ROUND(dProfit / ttCommissionCashReceipt.invAmt * 100,2)
          ttCommissionCashReceipt.commAmt = ROUND(ttCommissionCashReceipt.commAmt,2)
          .

      IF ttCommissionCashReceipt.delta LE .99 THEN ttCommissionCashReceipt.delta = 0.

      IF iplCalc AND ttCommissionCashReceipt.delta GT 0 THEN ttCommissionCashReceipt.commAmt = 0.

      IF ttCommissionCashReceipt.commission  EQ ? THEN ttCommissionCashReceipt.commission  = 0.
      IF ttCommissionCashReceipt.grossProfit EQ ? THEN ttCommissionCashReceipt.grossProfit = 0.

      {sys/inc/roundup.i ttCommissionCashReceipt.invQty}

      DELETE ttCommissionCashReceipt.
    END. /* each ttCommissionCashReceipt */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCommissionCashReceipt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCommissionCashReceipt Procedure 
PROCEDURE pCommissionCashReceipt :
/*------------------------------------------------------------------------------
  Purpose:     Commission Cash Receipt.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBatch   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dtDate         AS DATE      NO-UNDO.
    DEFINE VARIABLE dAmt           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dAmtPaid       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dAmtD          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvPct        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lComplete      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cSalesRep      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dSalesPct      AS DECIMAL   NO-UNDO.

    DEFINE BUFFER bARInvl FOR ar-invl.

    RUN getParamValues (ipcCompany, "r-commcr.", ipcUserID, ipiBatch).

    DEFINE VARIABLE dtStartDate      AS DATE      NO-UNDO.
    DEFINE VARIABLE cStartDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndDate        AS DATE      NO-UNDO.
    DEFINE VARIABLE cEndDateOption   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllSalesReps     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cStartSalesRep   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndSalesRep     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCustList        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lAllCustomers    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cStartCustNo     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShowInvoice     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDelta           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lDetailed        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lPrep            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lCalc            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    ASSIGN
        dtStartDate      = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartDate"))
        cStartDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartDateOption")
        dtEndDate        = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndDate"))
        cEndDateOption   = DYNAMIC-FUNCTION("fGetParamValue","svEndDateOption")
        lAllSalesReps    = DYNAMIC-FUNCTION("fGetParamValue","svAllSalesReps") EQ "yes"
        cStartSalesRep   = DYNAMIC-FUNCTION("fGetParamValue","svStartSalesRep")
        cEndSalesRep     = DYNAMIC-FUNCTION("fGetParamValue","svEndSalesRep")
        lCustList        = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllCustomers    = DYNAMIC-FUNCTION("fGetParamValue","svAllCustomers") EQ "yes"
        cStartCustNo     = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo       = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        cShowInvoice     = DYNAMIC-FUNCTION("fGetParamValue","svShowInvoice")
        cDelta           = DYNAMIC-FUNCTION("fGetParamValue","svDelta")
        lDetailed        = DYNAMIC-FUNCTION("fGetParamValue","svDetailed") EQ "yes"
        lPrep            = DYNAMIC-FUNCTION("fGetParamValue","svPrep") EQ "yes"
        lCalc            = DYNAMIC-FUNCTION("fGetParamValue","svCalc") EQ "yes"
        dtStartDate      = DYNAMIC-FUNCTION("fDateOptionDate",cStartDateOption,dtStartDate)
        dtEndDate        = DYNAMIC-FUNCTION("fDateOptionDate",cEndDateOption,dtEndDate)
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    IF lAllSalesReps THEN
    ASSIGN
        cStartSalesRep = CHR(32)
        cEndSalesRep   = CHR(127)
        .

    IF lAllCustomers THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(127)
        .

    OUTPUT TO "aoaAR.log" APPEND.
    PUT UNFORMATTED
        "[" NOW "] pCommissionCashReceipt" SKIP
        "ipiBatch: " ipiBatch SKIP
        "ipcCompany: " ipcCompany SKIP
        "ipcUserID: " ipcUserID SKIP
        "dtStartDate: " dtStartDate SKIP
        "cStartDateOption: " cStartDateOption SKIP
        "dtEndDate: " dtEndDate SKIP
        "cEndDateOption: " cEndDateOption SKIP
        "lAllSalesReps: " lAllSalesReps SKIP
        "cStartSalesRep: " cStartSalesRep SKIP
        "cEndSalesRep: " cEndSalesRep SKIP
        "lCustList: " lCustList SKIP
        "lAllCustomers: " lAllCustomers SKIP
        "cStartCustNo: " cStartCustNo SKIP
        "cEndCustNo: " cEndCustNo SKIP
        "cShowInvoice: " cShowInvoice SKIP
        "cDelta: " cDelta SKIP
        "lDetailed: " lDetailed SKIP
        "lPrep: " lPrep SKIP
        "lCalc: " lCalc SKIP
        "cSelectedColumns: " cSelectedColumns SKIP(1)
        .
    OUTPUT CLOSE.

    RUN pBuildCustList (ipcCompany, lCustList, cStartCustNo, cEndCustNo, "AR15").

    FIND FIRST oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ ipcCompany NO-ERROR.
    IF NOT AVAILABLE oe-ctrl THEN RETURN.

    FOR EACH ttCustList NO-LOCK
        WHERE ttCustList.log-fld,
        EACH ar-inv NO-LOCK
        WHERE ar-inv.company  EQ ipcCompany
          AND ar-inv.posted   EQ YES
          AND ar-inv.cust-no  EQ ttCustList.cust-no
          AND ar-inv.inv-date GE dtStartDate
          AND ar-inv.inv-date LE dtEndDate,
        FIRST cust NO-LOCK
        WHERE cust.company EQ ar-inv.company
          AND cust.cust-no EQ ar-inv.cust-no
        :

      dtDate = ?.
      FOR EACH ar-cashl NO-LOCK
          WHERE ar-cashl.company  EQ ar-inv.company
            AND ar-cashl.posted   EQ YES
            AND ar-cashl.cust-no  EQ ar-inv.cust-no
            AND ar-cashl.inv-no   EQ ar-inv.inv-no
          USE-INDEX inv-no,
          EACH ar-cash NO-LOCK
          WHERE ar-cash.c-no EQ ar-cashl.c-no
            AND ar-cash.memo EQ NO
          USE-INDEX c-no            
          BREAK BY ar-cash.check-date
          :
        /* Duplicate loop below just to get correct dtDate */
        IF NOT ar-cashl.memo THEN
        ASSIGN dtDate = ar-cash.check-date.
      END.

      dAmt = IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt THEN ar-inv.net
             ELSE ar-inv.gross.

      IF dtDate NE ? AND dtDate LE ar-inv.inv-date - ar-inv.disc-days THEN
      ASSIGN dAmt = dAmt - ROUND(dAmt * (ar-inv.disc-% / 100),2).

      IF dAmt EQ ? THEN dAmt = 0.

      ASSIGN 
          dAmtD    = dAmt
          dAmtPaid = dAmt
          dtDate   = ?
          .

      FOR EACH ar-cashl NO-LOCK
          WHERE ar-cashl.company  EQ ar-inv.company
            AND ar-cashl.posted   EQ YES
            AND ar-cashl.cust-no  EQ ar-inv.cust-no
            AND ar-cashl.inv-no   EQ ar-inv.inv-no
          USE-INDEX inv-no,
          EACH ar-cash NO-LOCK
          WHERE ar-cash.c-no EQ ar-cashl.c-no
            AND ar-cash.memo EQ NO
          USE-INDEX c-no
          BREAK BY ar-cash.check-date
          :

        IF ar-cashl.memo THEN    /* Just in case we change FOR EACH */
          IF ar-cashl.amt-disc NE 0 AND ar-cashl.amt-paid EQ 0 THEN
          ASSIGN
              dAmtD    = dAmtD    - ar-cashl.amt-disc
              dAmtPaid = dAmtPaid - ar-cashl.amt-disc
              .
          ELSE 
          IF ar-cashl.amt-paid + ar-cashl.amt-disc GT 0 THEN
          ASSIGN
              dAmtD    = dAmtD    + (ar-cashl.amt-paid + ar-cashl.amt-disc)
              dAmtPaid = dAmtPaid + (ar-cashl.amt-paid + ar-cashl.amt-disc)
              .
          ELSE
          ASSIGN
              dAmtD    = dAmtD    + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc)))
              dAmtPaid = dAmtPaid + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc)))
              .
        ELSE
        ASSIGN
            dAmtD     = dAmtD    + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1))
            dAmtPaid  = dAmtPaid + (ar-cashl.amt-paid * -1)
            dtDate    = ar-cash.check-date
            .
      END. /* each ar-cashl */

      ASSIGN
          dAmtD    = dAmt - dAmtD
          dAmtPaid = dAmt - dAmtPaid
          .

      IF cShowInvoice EQ "All"                        OR
         (dAmtD LT dAmt AND cShowInvoice EQ "Unpaid") OR
         (dAmtD GE dAmt AND cShowInvoice EQ "Paid")   THEN DO:

        dInvPct = 0.
        FOR EACH bARInvl WHERE bARInvl.x-no EQ ar-inv.x-no NO-LOCK:
          dInvPct = dInvPct + bARInvl.amt.
        END. /* each barinvl */
        dInvPct = dInvPct / dAmt.
        IF dInvPct EQ ? THEN dInvPct = 0.

        ASSIGN
            dAmt     = dAmt     * dInvPct
            dAmtD    = dAmtD    * dInvPct
            dAmtPaid = dAmtPaid * dInvPct
            .

        FOR EACH ar-invl NO-LOCK
            WHERE ar-invl.x-no EQ ar-inv.x-no
              AND ((lPrep AND ar-invl.billable) OR NOT ar-invl.misc)
            :

          RUN oe/invlcomp.p (ROWID(ar-invl), OUTPUT lComplete).
          IF lComplete THEN NEXT.

          dInvPct = 0.
          FOR EACH bARInvl WHERE bARInvl.x-no EQ ar-invl.x-no NO-LOCK:
            dInvPct = dInvPct + bARInvl.amt.
            ACCUMULATE 1 (TOTAL).
          END. /* each barinvl */

          ASSIGN dInvPct = IF dInvPct EQ 0 THEN
              (1 / IF (ACCUM TOTAL 1) EQ 0 THEN 1
                   ELSE (ACCUM TOTAL 1))
                   ELSE (ar-invl.amt / dInvPct).

          IF dInvPct EQ ? THEN dInvPct = 0.

          DO idx = 1 TO 3:
            cSalesRep = IF ar-invl.sman[idx] EQ "" AND idx EQ 1 AND NOT ar-invl.misc THEN cust.sman
                        ELSE ar-invl.sman[idx].

            IF cSalesRep   LT cStartSalesRep OR
               cSalesRep   GT cEndSalesRep   OR
             ((idx NE 1 OR ar-invl.misc)     AND
              (cSalesRep EQ "" OR ar-invl.s-pct[idx] EQ 0)) THEN NEXT.

            dSalesPct = IF ar-invl.sman[idx] EQ "" OR
                          (ar-invl.s-pct[idx] EQ 0 AND idx EQ 1) THEN 100
                        ELSE ar-invl.s-pct[idx].

            FIND FIRST sman NO-LOCK
                WHERE sman.company EQ ipcCompany
                  AND sman.sman    EQ cSalesRep
                NO-ERROR.

            CREATE ttCommissionCashReceipt.
            ASSIGN
                ttCommissionCashReceipt.salesRep   = cSalesRep
                ttCommissionCashReceipt.custNo     = ar-inv.cust-no
                ttCommissionCashReceipt.invoiceNo  = ar-inv.inv-no
                ttCommissionCashReceipt.rec-id     = RECID(ar-invl)
                ttCommissionCashReceipt.row-id     = ROWID(ar-invl)
                ttCommissionCashReceipt.invQty     = ar-invl.inv-qty * dSalesPct / 100
                ttCommissionCashReceipt.invAmt     = dAmt * dInvPct * dSalesPct / 100
                ttCommissionCashReceipt.amtD       = dAmtD * dInvPct * dSalesPct / 100
                ttCommissionCashReceipt.amtPaid    = dAmtPaid * dInvPct * dSalesPct / 100
                ttCommissionCashReceipt.delta      = ttCommissionCashReceipt.invAmt - ttCommissionCashReceipt.amtD
                ttCommissionCashReceipt.cost       = ar-invl.t-cost * dSalesPct / 100
                ttCommissionCashReceipt.commission = IF ar-invl.sman[idx] EQ "" AND AVAIL sman
                                      THEN sman.scomm ELSE ar-invl.s-comm[idx]
                ttCommissionCashReceipt.cashDate   = dtDate
                .
          END. /* do idx */
        END. /* each ar-invl */
      END. /* show all invoice */
    END. /* each ttCustList */

    /* section editor size limitations */
    RUN pCommCashReceipt (ipcCompany, lCalc, lDetailed, cDelta).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fCommissionCashReceipt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCommissionCashReceipt Procedure 
FUNCTION fCommissionCashReceipt RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Commission Cash Receipt.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttCommissionCashReceipt.

    RUN pCommissionCashReceipt (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttCommissionCashReceipt:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fCustomers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCustomers Procedure 
FUNCTION fCustomers RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Customer.rpa
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo   AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttCust .
    
    RUN getParamValues (ipcCompany, "custaoa.", ipcUserID, ipiBatch).

    ASSIGN
        cStartCustNo   = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo     = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        .

    FOR EACH cust NO-LOCK 
        WHERE cust.company EQ ipcCompany
          AND cust.cust-no GT cStartCustNo
          AND cust.cust-no LE cEndCustNo
        :
        CREATE ttCust .
        ASSIGN 
            ttCust.company  = cust.company
            ttCust.custNo   = cust.cust-no
            ttCust.custName = cust.name
            .
    END .

    RETURN TEMP-TABLE ttCust:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    CASE ipcProgramID:
        /* Commision Cash Receipt.rpa */
        WHEN "r-commcr." THEN
        RETURN TEMP-TABLE ttCommissionCashReceipt:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

