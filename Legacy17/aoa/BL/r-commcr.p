/*------------------------------------------------------------------------
  File: r-commcr.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Commision Cash Receipt.rpa */
{aoa/tempTable/ttCommissionCashReceipt.i}

{sys/ref/CustList.i NEW}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttCommissionCashReceipt.
{aoa/includes/pCommissionCashReceipt.i}

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

FOR EACH ar-inv NO-LOCK
    WHERE ar-inv.company  EQ ipcCompany
      AND ar-inv.posted   EQ YES
      AND ar-inv.cust-no  GE cStartCustNo
      AND ar-inv.cust-no  LE cEndCustNo
      AND ar-inv.inv-date GE dtStartInvoiceDate
      AND ar-inv.inv-date LE dtEndInvoiceDate,
    FIRST cust NO-LOCK
    WHERE cust.company EQ ar-inv.company
      AND cust.cust-no EQ ar-inv.cust-no
    :
  IF lCustList AND
     NOT CAN-FIND(FIRST ttCustList
                  WHERE ttCustList.cust-no EQ ar-inv.cust-no
                    AND ttCustList.log-fld EQ TRUE) THEN
  NEXT.
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
END. /* each ar-inv */

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

{aoa/BL/pBuildCustList.i}
