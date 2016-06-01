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

/* Cash Receipt By SalesRep Name.rpa */
DEFINE TEMP-TABLE ttCashReceiptBySalesRepName NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD salesRep  AS CHARACTER LABEL "Sales Rep"     FORMAT "x(3)"
    FIELD salesName AS CHARACTER LABEL "Sales Name"    FORMAT "x(30)"
    FIELD custNo    AS CHARACTER LABEL "Customer"      FORMAT "x(8)"
    FIELD custName  AS CHARACTER LABEL "Customer Name" FORMAT "x(30)"
    FIELD terms     AS CHARACTER LABEL "Terms"         FORMAT "x(8)"
    FIELD invoiceNo AS INTEGER   LABEL "Invoice"       FORMAT ">>>>>>>>"
    FIELD invDate   AS DATE      LABEL "Inv Date"      FORMAT 99/99/9999
    FIELD chkDate   AS DATE      LABEL "Check Date"    FORMAT 99/99/9999
    FIELD Aging     AS DECIMAL   LABEL "Aging"         FORMAT "$->>,>>9.99" 
    FIELD invAmt    AS DECIMAL   LABEL "Inv Amount"    FORMAT "$->>>,>>>,>>9.99"
    FIELD amtPaid   AS DECIMAL   LABEL "Amt Paid"      FORMAT "$->>>,>>>,>>9.99"
    FIELD discount  AS DECIMAL   LABEL "Discount"      FORMAT "$->>>>,>>9.99"
    FIELD balAftPay AS DECIMAL   LABEL "Bal Aft Pymt"  FORMAT "$->>>,>>>,>>9.99"
    FIELD commAmt   AS DECIMAL   LABEL "Comm Amt"      FORMAT "$->>,>>>,>>9.99"
    FIELD commPct   AS DECIMAL   LABEL "Comm Pct"      FORMAT "->>9.99" 
    FIELD xxSort    AS CHARACTER LABEL "Sort"          FORMAT "x(50)"
    FIELD rec-id    AS RECID
    FIELD row-id    AS ROWID
    .

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD inv-no   AS INTEGER
    FIELD chk-inv  AS LOGICAL INITIAL YES
    FIELD q-onh    LIKE itemfg.q-onh
    FIELD q-shp    LIKE itemfg.q-onh
    FIELD q-rel    LIKE itemfg.q-onh
    FIELD q-wip    LIKE itemfg.q-onh
    FIELD q-avl    LIKE itemfg.q-onh
    FIELD po-no    LIKE oe-ord.po-no
    FIELD inv      AS   LOGICAL
    FIELD cad-no   LIKE itemfg.cad-no
    FIELD row-id   AS ROWID 
    FIELD due-date     LIKE oe-ordl.req-date
    FIELD unit-count   LIKE eb.cas-cnt
    FIELD units-pallet LIKE eb.cas-pal
    .

DEFINE TEMP-TABLE tt-report-inv NO-UNDO LIKE report 
    FIELD inv-no AS INT.
/* Cash Receipt By SalesRep Name.rpa */

/* Commision Cash Receipt.rpa */
DEFINE TEMP-TABLE ttCommissionCashReceipt NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD salesRep    AS CHARACTER LABEL "Sales Rep"    FORMAT "x(3)"
    FIELD custNo      AS CHARACTER LABEL "Customer"     
    FIELD invoiceNo   AS INTEGER   LABEL "Invoice"      FORMAT ">>>>>9"
    FIELD invDate     AS DATE      LABEL "Inv Date"     FORMAT "99/99/9999"
    FIELD custPart    AS CHARACTER LABEL "Cust Part"    FORMAT "x(15)"
    FIELD orderNo     AS INTEGER   LABEL "Order"        FORMAT ">>>>>9"
    FIELD invQty      AS DECIMAL   LABEL "Quantity"
    FIELD invAmt      AS DECIMAL   LABEL "Inv Amt"      FORMAT "$->>>,>>9.99"
    FIELD cashDate    AS DATE      LABEL "Cash Date"    FORMAT "99/99/9999"
    FIELD delta       AS DECIMAL   LABEL "Delta"        FORMAT "$->>>,>>9.99"
    FIELD grossProfit AS DECIMAL   LABEL "Gross Profit" FORMAT "$->>>,>>9.99"
    FIELD commission  AS DECIMAL   LABEL "Commission"   FORMAT "$->>>,>>9.99"
    FIELD commAmt     AS DECIMAL   LABEL "Comm Amt"     FORMAT "$->>>,>>9.99"
    FIELD basis       AS CHARACTER LABEL "Basis"        FORMAT "x"
    FIELD totalCost   AS DECIMAL   LABEL "Total Cost"
    FIELD xxAmtPaid   AS DECIMAL
    FIELD xxAmtD      AS DECIMAL
    FIELD xxCost      AS DECIMAL
    FIELD xxCommPct   AS DECIMAL
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

&IF DEFINED(EXCLUDE-fCashReceiptBySalesRepName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCashReceiptBySalesRepName Procedure 
FUNCTION fCashReceiptBySalesRepName RETURNS HANDLE
    ( ipcCompany AS CHARACTER,
      ipiBatch   AS INTEGER,
      ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fCommissionCashReceipt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCommissionCashReceipt Procedure 
FUNCTION fCommissionCashReceipt RETURNS HANDLE
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

&IF DEFINED(EXCLUDE-pCashReceiptBySalesRepName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCashReceiptBySalesRepName Procedure 
PROCEDURE pCashReceiptBySalesRepName :
/*------------------------------------------------------------------------------
  Purpose:     Cash Receipt By SalesRep Name.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pCashReceiptBySalesRepName.i}

    /* local variables */
    DEFINE VARIABLE cSortKey    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSMan       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dAmt      LIKE ar-cashl.amt-paid NO-UNDO EXTENT 2.
    DEFINE VARIABLE dPaid     LIKE dAmt      NO-UNDO.
    DEFINE VARIABLE dDsc      LIKE dAmt      NO-UNDO.
    DEFINE VARIABLE dCom      LIKE ar-cashl.amt-paid NO-UNDO.
    DEFINE VARIABLE dCom-2    LIKE dCom      NO-UNDO.
    DEFINE VARIABLE dComm%      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dPct        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dMisc       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cBasis    LIKE sman.commbasis NO-UNDO INITIAL "".
    DEFINE VARIABLE cTerm     LIKE tt-report.term-id NO-UNDO.
    DEFINE VARIABLE dTotAmt     AS DECIMAL   NO-UNDO EXTENT 4.
    DEFINE VARIABLE dTotCom   LIKE dTotAmt   NO-UNDO.
    DEFINE VARIABLE iAging      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dtCheckDate AS DATE      NO-UNDO.
    DEFINE VARIABLE dtInvDate   AS DATE      NO-UNDO.
    DEFINE VARIABLE dAmt-full   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvFull    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dRemBal     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTmpAmt1    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lInvFound   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE j           AS INTEGER   NO-UNDO.

    /* subject business logic */
    FOR EACH cust NO-LOCK
        WHERE cust.company EQ ipcCompany:
        IF iDayOld EQ 0 THEN
        FOR EACH ar-inv NO-LOCK
            WHERE ar-inv.company  EQ ipcCompany
              AND ar-inv.posted   EQ YES
              AND ar-inv.cust-no  EQ cust.cust-no
              AND ar-inv.inv-date GE dtStartReceiptDate
              AND ar-inv.inv-date LE dtEndReceiptDate
              AND ar-inv.terms    EQ "CASH",
            EACH ar-invl NO-LOCK
            WHERE ar-invl.x-no EQ ar-inv.x-no:
            DO i = 1 TO 3:
                cSMan = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman
                         ELSE ar-invl.sman[i].
                IF cSMan LT cStartSalesRep OR cSMan GT cEndSalesRep OR
                  (i NE 1 AND (cSMan EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.
                IF NOT CAN-FIND(FIRST tt-report
                                WHERE tt-report.term-id EQ cTerm
                                  AND tt-report.key-01  EQ cSMan
                                  AND tt-report.inv-no  EQ ar-invl.inv-no
                                  AND tt-report.rec-id  EQ RECID(ar-invl)) THEN DO:
                    CREATE tt-report.
                    ASSIGN
                        tt-report.term-id = cTerm
                        tt-report.key-01  = cSMan
                        tt-report.key-09  = cust.cust-no
                        tt-report.key-08  = cust.terms
                        tt-report.key-10  = cust.name
                        tt-report.rec-id  = RECID(ar-invl)
                        tt-report.inv-no  = ar-invl.inv-no.
                    CASE cSort:
                        WHEN "Customer" THEN
                        ASSIGN
                            tt-report.key-02 = cust.cust-no
                            tt-report.key-03 = STRING(ar-invl.inv-no,"9999999999").
                        WHEN "Invoice" THEN
                        ASSIGN
                            tt-report.key-02 = STRING(ar-invl.inv-no,"9999999999")
                            tt-report.key-03 = cust.cust-no.
                    END CASE.
                    RELEASE tt-report.
                END. /* if not can-find */
            END. /* do i */
        END. /* each ar-inv */
        FOR EACH ar-cash NO-LOCK
            WHERE ar-cash.company    EQ ipcCompany
              AND ar-cash.cust-no    EQ cust.cust-no
              AND ar-cash.check-date GE dtStartReceiptDate
              AND ar-cash.check-date LE dtEndReceiptDate
              AND ar-cash.posted     EQ YES
              AND ar-cash.check-no   NE 0,
            EACH ar-cashl NO-LOCK
            WHERE ar-cashl.c-no   EQ ar-cash.c-no
              AND ar-cashl.posted EQ YES
              AND ar-cashl.memo   EQ NO:
            IF NOT (iDayOld EQ 0 OR
                   (ar-cash.check-date - ar-cashl.inv-date GT iDayOld AND
                    ar-cashl.inv-no NE 0)) THEN NEXT.
            IF ar-cashl.inv-no NE 0 THEN
            FOR EACH ar-invl NO-LOCK
                WHERE ar-invl.company EQ ipcCompany
                  AND ar-invl.cust-no EQ ar-cash.cust-no
                  AND ar-invl.inv-no  EQ ar-cashl.inv-no:
                DO i = 1 TO 3:
                    cSMan = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman
                    ELSE ar-invl.sman[i].
                    IF cSMan  LT cStartSalesRep OR cSMan GT cEndSalesRep OR
                      (i NE 1 AND (cSMan EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.
                    IF NOT CAN-FIND(FIRST tt-report
                                    WHERE tt-report.term-id EQ cTerm
                                      AND tt-report.key-01  EQ cSMan
                                      AND tt-report.inv-no  EQ ar-invl.inv-no
                                      AND tt-report.rec-id  EQ RECID(ar-cashl)) THEN DO:
                        CREATE tt-report.
                        ASSIGN
                            tt-report.term-id = cTerm
                            tt-report.key-01  = cSMan
                            tt-report.key-09  = cust.cust-no
                            tt-report.key-08  = cust.terms
                            tt-report.key-10  = cust.name
                            tt-report.rec-id  = RECID(ar-cashl)
                            tt-report.inv-no  = ar-invl.inv-no.
                        CASE cSort:
                            WHEN "Customer" THEN
                            ASSIGN
                                tt-report.key-02 = cust.cust-no
                                tt-report.key-03 = STRING(ar-invl.inv-no,"9999999999").
                            WHEN "Invoice" THEN
                                ASSIGN
                                tt-report.key-02 = STRING(ar-invl.inv-no,"9999999999")
                                tt-report.key-03 = cust.cust-no.
                        END CASE.
                        RELEASE tt-report.
                    END. /* if not can-find */
                END. /* do i */
            END. /* each ar-invl */
            ELSE
            IF cust.sman GE cStartSalesRep AND
               cust.sman LE cEndSalesRep   THEN DO:
                cSMan = cust.sman.
                IF NOT CAN-FIND(FIRST tt-report
                                WHERE tt-report.term-id EQ cTerm
                                  AND tt-report.key-01  EQ cSMan
                                  AND tt-report.inv-no  EQ ar-cashl.inv-no
                                  AND tt-report.rec-id  EQ RECID(ar-cashl)) THEN DO:
                    CREATE tt-report.
                    ASSIGN
                        tt-report.term-id = cTerm
                        tt-report.key-01  = cSMan
                        tt-report.key-09  = cust.cust-no
                        tt-report.key-08  = cust.terms
                        tt-report.key-10  = cust.name
                        tt-report.rec-id  = RECID(ar-cashl)
                        tt-report.inv-no  = ar-cashl.inv-no.
                    CASE cSort:
                        WHEN "Customer" THEN
                        ASSIGN
                            tt-report.key-02 = cust.cust-no
                            tt-report.key-03 = STRING(ar-cashl.inv-no,"9999999999").
                        WHEN "Invoice" THEN
                        ASSIGN
                            tt-report.key-02 = STRING(ar-cashl.inv-no,"9999999999")
                            tt-report.key-03 = cust.cust-no.
                    END CASE.
                    RELEASE tt-report.
                END. /* if not can-find */
            END. /* else if */
        END. /* each ar-cashl */
    END. /* each cust */
    FOR EACH tt-report NO-LOCK:
        FIND FIRST tt-report-inv NO-LOCK
             WHERE tt-report-inv.inv-no = tt-report.inv-no 
               AND  tt-report-inv.key-01 NE tt-report.key-01
             NO-ERROR.
        IF NOT AVAIL tt-report-inv THEN DO:
            CREATE  tt-report-inv.
            ASSIGN
                tt-report-inv.key-01 = tt-report.key-01
                tt-report-inv.key-02 = tt-report.key-02
                tt-report-inv.key-03 = tt-report.key-03
                tt-report-inv.inv-no = tt-report.inv-no.
        END. /* not avail tt-report-inv */
        ELSE tt-report.chk-inv = FALSE.
    END. /* each tt-report */
    FOR EACH tt-report-inv NO-LOCK:
        DELETE tt-report-inv .
    END. /* each tt-report-inv */
    FOR EACH tt-report
        WHERE tt-report.term-id EQ cTerm,
        FIRST cust NO-LOCK
        WHERE cust.company EQ ipcCompany
          AND cust.cust-no EQ tt-report.key-09
        BREAK BY tt-report.key-01
              BY tt-report.key-02
              BY tt-report.key-03
              BY tt-report.inv-no:
        FIND FIRST sman NO-LOCK
             WHERE sman.company EQ ipcCompany
               AND sman.sman    EQ tt-report.key-01
             NO-ERROR.
        RELEASE ar-inv.
        RELEASE ar-cash.
        ASSIGN
            dtCheckDate = ?
            dtInvDate = ?
            dAmt-full = 0
            dInvFull = 0
            dMisc = 0.
        FIND ar-cashl NO-LOCK
             WHERE RECID(ar-cashl) EQ tt-report.rec-id
             NO-ERROR.
        IF AVAILABLE ar-cashl THEN DO:
            FIND FIRST ar-cash NO-LOCK WHERE ar-cash.c-no EQ ar-cashl.c-no.
            ASSIGN
                dDsc[1] = IF lIncludeTermsDiscount THEN ar-cashl.amt-disc ELSE 0
                dtCheckDate = ar-cash.check-date
                dAmt-full = ar-cashl.amt-paid
                lInvFound = NO.
            IF ar-cashl.inv-no NE 0 THEN
            FOR EACH ar-invl NO-LOCK
                WHERE ar-invl.company EQ ipcCompany
                  AND ar-invl.cust-no EQ ar-cash.cust-no
                  AND ar-invl.inv-no  EQ ar-cashl.inv-no,
                FIRST ar-inv NO-LOCK
                WHERE ar-inv.x-no EQ ar-invl.x-no
                BREAK BY ar-invl.inv-no:
                lInvFound = YES.
                FIND FIRST itemfg NO-LOCK
                     WHERE itemfg.company EQ ipcCompany
                      AND itemfg.i-no     EQ ar-invl.i-no
                     NO-ERROR.
                RUN custom/combasis.p (ipcCompany,tt-report.key-01,cust.type,
                   (IF AVAILABLE itemfg THEN itemfg.procat ELSE ""),0,cust.cust-no,OUTPUT cBasis).
                IF FIRST-OF(tt-report.inv-no) THEN DO:
                    IF FIRST(ar-invl.inv-no) THEN
                    ASSIGN
                        dAmt    = 0
                        dAmt[1] = ar-inv.tax-amt +
                        (if ar-inv.f-bill THEN ar-inv.freight ELSE 0)
                        dInvFull = dAmt[1]
                        dCom    = 0
                        dCom-2  = 0
                        dtInvDate = ar-inv.inv-date.
                    dInvFull = dInvFull + ar-invl.amt.
                    IF NOT lIncludePrepCharges AND ar-invl.misc THEN
                    dMisc = dMisc + ar-invl.amt.
                    ELSE DO:
                        ASSIGN
                            dAmt[1] = dAmt[1] + ar-invl.amt
                            dTmpAmt1 = dAmt[1].
                        IF ar-invl.sman[1] NE "" THEN
                        DO i = 1 TO 3:
                            IF tt-report.key-01 EQ ar-invl.sman[i] THEN DO:
                                ASSIGN
                                    dAmt[2] = dAmt[2] + (ar-invl.amt * ar-invl.s-pct[i] / 100)
                                    dCom    = dCom
                                            + (((ar-invl.amt - IF cBasis EQ "G" THEN ar-invl.t-cost ELSE 0) *
                                    ar-invl.s-pct[i] / 100) * ar-invl.s-comm[i] / 100).
                                LEAVE.
                            END. /* if key-01 */
                        END. /* do i */
                        ELSE
                        ASSIGN
                            dAmt[2] = dAmt[2] + ar-invl.amt
                            dCom    = dCom +
                            ((ar-invl.amt - IF cBasis EQ "G" THEN ar-invl.t-cost ELSE 0) *
                            (IF AVAILABLE sman THEN (sman.scomm / 100) ELSE 0)).
                    END. /* else */
                END. /*end FIRST-OF(tt-report.inv-no)*/
            END. /*end each ar-invl*/
            IF lInvFound = NO THEN
            ASSIGN
                dAmt[1] = ar-cashl.amt-paid + dDsc[1] - dMisc
                dTmpAmt1 = dAmt[1]
                dAmt[2] = dAmt[1]
                dCom    = dAmt[1] * (if AVAILABLE sman THEN (sman.scomm / 100) ELSE 0).
            ASSIGN
                dPct    = dAmt[2] / dTmpAmt1
                dAmt[1] = (ar-cashl.amt-paid + dDsc[1] - dMisc) * dPct    /* task 02261403 */
                dPct    = dAmt[1] / dAmt[2]
                dCom-2  = dCom * dPct.
            RELEASE ar-inv.
        END. /* avail ar-cashl */
        ELSE DO:
            FIND ar-invl NO-LOCK WHERE RECID(ar-invl) EQ tt-report.rec-id.
            FIND FIRST ar-inv NO-LOCK WHERE ar-inv.x-no EQ ar-invl.x-no.
            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ ipcCompany
                   AND itemfg.i-no    EQ ar-invl.i-no
                 NO-ERROR.
            RUN custom/combasis.p
                (ipcCompany, tt-report.key-01, cust.type,
                (IF AVAIL itemfg THEN itemfg.procat ELSE ""), 0,cust.cust-no,OUTPUT cBasis).
            dInvFull = ar-invl.amt.
            IF NOT lIncludePrepCharges AND ar-invl.misc THEN NEXT.
            ASSIGN
                dAmt[1] = ar-invl.amt 
                dTmpAmt1 = dAmt[1]
                dCom    = (ar-invl.amt - IF cBasis EQ "G" THEN ar-invl.t-cost ELSE 0) *
                (if AVAILABLE sman THEN (sman.scomm / 100) ELSE 0)
                dCom-2  = dCom.
        END. /* else do */
        IF dCom-2  EQ ? THEN dCom-2 = 0.
        IF dAmt[1] EQ ? THEN
        ASSIGN
            dAmt[1] = 0
            dTmpAmt1 = 0.
        ASSIGN
            dComm% = dCom-2 / dAmt[1] * 100
            dAmt[1] = dTmpAmt1. /* multiple payments against an invoice, reset dAmt[1] value */
        IF dComm% EQ ? THEN dComm% = 0.
        ASSIGN
            dPaid[1] = dAmt-full
            iAging = (IF dtCheckDate NE ? AND dtInvDate NE ? THEN dtCheckDate - dtInvDate ELSE 0).
        dRemBal = IF FIRST-OF(tt-report.inv-no) THEN dInvFull - dAmt-full - dDsc[1]
                  ELSE dRemBal - dAmt-full - dDsc[1].
        CREATE ttCashReceiptBySalesRepName.
        ASSIGN
            ttCashReceiptBySalesRepName.xxSort    = tt-report.key-01
                                                  + tt-report.key-02
                                                  + tt-report.key-03
            ttCashReceiptBySalesRepName.salesRep  = STRING(tt-report.key-01) 
            ttCashReceiptBySalesRepName.salesName = IF AVAILABLE sman THEN STRING(sman.sname) ELSE ""
            ttCashReceiptBySalesRepName.custNo    = STRING(tt-report.key-09)
            ttCashReceiptBySalesRepName.custName  = tt-report.key-10
            ttCashReceiptBySalesRepName.terms     = tt-report.key-08
            ttCashReceiptBySalesRepName.invoiceNo = IF AVAILABLE ar-cashl THEN ar-cashl.inv-no ELSE IF AVAIL ar-inv THEN  ar-inv.inv-no ELSE 0
            ttCashReceiptBySalesRepName.invDate   = dtInvDate   
            ttCashReceiptBySalesRepName.chkDate   = dtCheckDate 
            ttCashReceiptBySalesRepName.Aging     = iAging
            ttCashReceiptBySalesRepName.invAmt    = dInvFull
            ttCashReceiptBySalesRepName.amtPaid   = dAmt-full
            ttCashReceiptBySalesRepName.discount  = dDsc[1]
            ttCashReceiptBySalesRepName.balAftPay = dRemBal
            ttCashReceiptBySalesRepName.commAmt   = dCom-2
            ttCashReceiptBySalesRepName.commPct   = dComm%.
        IF LAST-OF(tt-report.inv-no) THEN DELETE tt-report.
    END. /* each tt-report */

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
    {aoaAppSrv/pCommissionCashReceipt.i}
    
    /* local variables */
    DEFINE VARIABLE dtDate     AS DATE      NO-UNDO.
    DEFINE VARIABLE dAmt       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dAmtPaid   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dAmtD      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvPct    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lComplete  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cSalesRep  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dSalesPct  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCommBasis AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dProfit    AS DECIMAL   NO-UNDO.

    /* subject business logic */
    DEFINE BUFFER bARInvl FOR ar-invl.
    
    FIND FIRST oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ ipcCompany NO-ERROR.
    IF NOT AVAILABLE oe-ctrl THEN RETURN.

    FOR EACH ttCustList NO-LOCK
        WHERE ttCustList.log-fld,
        EACH ar-inv NO-LOCK
        WHERE ar-inv.company  EQ ipcCompany
          AND ar-inv.posted   EQ YES
          AND ar-inv.cust-no  EQ ttCustList.cust-no
          AND ar-inv.inv-date GE dtStartInvoiceDate
          AND ar-inv.inv-date LE dtEndInvoiceDate,
        FIRST cust NO-LOCK
        WHERE cust.company EQ ar-inv.company
          AND cust.cust-no EQ ar-inv.cust-no
        :

      dtDate = ?.
      FOR EACH ar-cashl NO-LOCK
          WHERE ar-cashl.company EQ ar-inv.company
            AND ar-cashl.posted  EQ YES
            AND ar-cashl.cust-no EQ ar-inv.cust-no
            AND ar-cashl.inv-no  EQ ar-inv.inv-no
          USE-INDEX inv-no,
          EACH ar-cash NO-LOCK
          WHERE ar-cash.c-no EQ ar-cashl.c-no
            AND ar-cash.memo EQ NO
            AND ar-cash.check-date GE dtStartReceiptDate
            AND ar-cash.check-date LE dtEndReceiptDate
          USE-INDEX c-no            
          BREAK BY ar-cash.check-date
          :
        /* Duplicate loop below just to get correct dtDate */
        IF NOT ar-cashl.memo THEN
        ASSIGN dtDate = ar-cash.check-date.
      END. /* each ar-cashl */

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

      FOR EACH ar-cashl NO-LOCK USE-INDEX inv-no
          WHERE ar-cashl.company  EQ ar-inv.company
            AND ar-cashl.posted   EQ YES
            AND ar-cashl.cust-no  EQ ar-inv.cust-no
            AND ar-cashl.inv-no   EQ ar-inv.inv-no,
          EACH ar-cash  NO-LOCK USE-INDEX c-no
          WHERE ar-cash.c-no EQ ar-cashl.c-no
            AND ar-cash.memo EQ NO
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

            PUT UNFORMATTED "ttCommissionCashReceipt" SKIP.

            CREATE ttCommissionCashReceipt.
            ASSIGN
                ttCommissionCashReceipt.salesRep   = cSalesRep
                ttCommissionCashReceipt.custNo     = ar-inv.cust-no
                ttCommissionCashReceipt.invoiceNo  = ar-inv.inv-no
                ttCommissionCashReceipt.invQty     = ar-invl.inv-qty * dSalesPct / 100
                ttCommissionCashReceipt.invAmt     = dAmt * dInvPct * dSalesPct / 100
                ttCommissionCashReceipt.xxAmtD     = dAmtD * dInvPct * dSalesPct / 100
                ttCommissionCashReceipt.xxAmtPaid  = dAmtPaid * dInvPct * dSalesPct / 100
                ttCommissionCashReceipt.delta      = ttCommissionCashReceipt.invAmt - ttCommissionCashReceipt.xxAmtD
                ttCommissionCashReceipt.xxCost     = ar-invl.t-cost * dSalesPct / 100
                ttCommissionCashReceipt.commission = IF ar-invl.sman[idx] EQ "" AND AVAIL sman
                                                     THEN sman.scomm ELSE ar-invl.s-comm[idx]
                ttCommissionCashReceipt.cashDate   = dtDate
                ttCommissionCashReceipt.rec-id     = RECID(ar-invl)
                ttCommissionCashReceipt.row-id     = ROWID(ar-invl)
                .
          END. /* do idx */
        END. /* each ar-invl */
      END. /* show all invoice */
    END. /* each ttCustList */

    FOR EACH ttCommissionCashReceipt,
        FIRST ar-invl NO-LOCK
        WHERE RECID(ar-invl) EQ ttCommissionCashReceipt.rec-id,
        FIRST ar-inv NO-LOCK
        WHERE ar-inv.x-no EQ ar-invl.x-no,
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
         (IF AVAILABLE itemfg THEN itemfg.procat ELSE ""),
          0,
          cust.cust-no,
          OUTPUT cCommBasis
          ).

      IF ttCommissionCashReceipt.xxCost EQ ? THEN ttCommissionCashReceipt.xxCost = 0.
      IF ttCommissionCashReceipt.commission EQ ? THEN ttCommissionCashReceipt.commission = 0.
      
      ttCommissionCashReceipt.xxCommPct = 0.
      
      ASSIGN
          dProfit = ttCommissionCashReceipt.invAmt - ttCommissionCashReceipt.xxCost
          ttCommissionCashReceipt.commAmt     = IF cCommBasis EQ "G" THEN (dProfit * ttCommissionCashReceipt.commission / 100)
                                                ELSE (ttCommissionCashReceipt.invAmt * (ttCommissionCashReceipt.commission / 100))
          ttCommissionCashReceipt.xxCommPct   = ttCommissionCashReceipt.commission
          ttCommissionCashReceipt.grossProfit = ROUND(dProfit / ttCommissionCashReceipt.invAmt * 100,2)
          ttCommissionCashReceipt.commAmt     = ROUND(ttCommissionCashReceipt.commAmt,2)
          ttCommissionCashReceipt.invDate     = ar-inv.inv-date
          ttCommissionCashReceipt.custPart    = ar-invl.part-no
          ttCommissionCashReceipt.orderNo     = ar-invl.ord-no
          ttCommissionCashReceipt.basis       = IF AVAILABLE sman THEN sman.commbasis ELSE ""
          ttCommissionCashReceipt.totalCost   = IF AVAILABLE ar-invl THEN ar-invl.cost ELSE 0
          .

      IF ttCommissionCashReceipt.delta LE .99 THEN ttCommissionCashReceipt.delta = 0.

      IF lCalc AND ttCommissionCashReceipt.delta GT 0 THEN ttCommissionCashReceipt.commAmt = 0.

      IF ttCommissionCashReceipt.xxCommPct EQ ? THEN ttCommissionCashReceipt.xxCommPct = 0.
      IF ttCommissionCashReceipt.grossProfit EQ ? THEN ttCommissionCashReceipt.grossProfit = 0.

      {sys/inc/roundup.i ttCommissionCashReceipt.invQty}

    END. /* each ttCommissionCashReceipt */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fCashReceiptBySalesRepName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCashReceiptBySalesRepName Procedure 
FUNCTION fCashReceiptBySalesRepName RETURNS HANDLE
    ( ipcCompany AS CHARACTER,
      ipiBatch   AS INTEGER,
      ipcUserID  AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  Cash Receipt By SalesRep Name.rpa
      Notes:  
  ------------------------------------------------------------------------------*/
      EMPTY TEMP-TABLE ttCashReceiptBySalesRepName.

      RUN pCashReceiptBySalesRepName (ipcCompany, ipiBatch, ipcUserID).

      RETURN TEMP-TABLE ttCashReceiptBySalesRepName:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    CASE ipcProgramID:
        /* Cash Receipt By SalesRep Name.rpa */
        WHEN "r-cashs2." THEN
        RETURN TEMP-TABLE ttCashReceiptBySalesRepName:HANDLE.
        /* Commision Cash Receipt.rpa */
        WHEN "r-commcr." THEN
        RETURN TEMP-TABLE ttCommissionCashReceipt:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

