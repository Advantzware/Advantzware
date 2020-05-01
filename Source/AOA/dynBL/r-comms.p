/*------------------------------------------------------------------------
  File:         r-comms.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 4.28.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttCommissions
DEFINE TEMP-TABLE ttCommissions NO-UNDO
    FIELD salesRep      AS CHARACTER FORMAT "x(3)"             LABEL "Rep"
    FIELD custNo        AS CHARACTER FORMAT "x(12)"            LABEL "Customer"
    FIELD custName      AS CHARACTER FORMAT "x(30)"            LABEL "Customer Name"
    FIELD custType      AS CHARACTER FORMAT "x(8)"             LABEL "Type"
    FIELD fgItem        AS CHARACTER FORMAT "x(15)"            LABEL "FG Item"
    FIELD custPart      AS CHARACTER FORMAT "x(15)"            LABEL "Cust Part"
    FIELD orderNo       AS INTEGER   FORMAT ">>>>>>9"          LABEL "Order"
    FIELD invoiceNo     AS INTEGER   FORMAT ">>>>>>9"          LABEL "Invoice"
    FIELD category      AS CHARACTER FORMAT "x(3)"             LABEL "Cat"
    FIELD quantity      AS INTEGER   FORMAT "->>>>>>>9"        LABEL "Quantity"
    FIELD sellPrice     AS DECIMAL   FORMAT "->>>>>>>9.99"     LABEL "Sell Price"
    FIELD totalCost     AS DECIMAL   FORMAT "->>>>>>>9.99"     LABEL "Total Cost"
    FIELD fullCost      AS DECIMAL   FORMAT "->>>>>>>9.99"     LABEL "Full Cost"
    FIELD totalGP       AS DECIMAL   FORMAT "->>>>9.99"        LABEL "Total GP%"
    FIELD fullGP        AS DECIMAL   FORMAT "->>>>9.99"        LABEL "Full GP%"
    FIELD commission    AS DECIMAL   FORMAT "->>>9.99"         LABEL "Commission"
    FIELD salesGroup    AS CHARACTER FORMAT "x(3)"             LABEL "Group"
    FIELD currency      AS CHARACTER FORMAT "x(3)"             LABEL "$$$"
    FIELD invoiceDate   AS DATE      FORMAT "99/99/9999"       LABEL "Inv Date"
    FIELD warehouse     AS CHARACTER FORMAT "x(5)"             LABEL "Whse"
    FIELD shipID        AS CHARACTER FORMAT "x(8)"             LABEL "ShipID"
    FIELD msf           AS DECIMAL   FORMAT "->>>>>9.99<<<"    LABEL "MSF"
    FIELD freightCost   AS DECIMAL   FORMAT "->>>>,>>9.99"     LABEL "Freight"
    FIELD whseCost      AS DECIMAL   FORMAT "->>,>>>,>>9.99"   LABEL "Whse Cost"
    FIELD manufCost     AS DECIMAL   FORMAT "->>>>,>>>,>>9.99" LABEL "Manuf Cost"
    FIELD deviaCost     AS DECIMAL   FORMAT "->>,>>>,>>9.99"   LABEL "Deviation"
    FIELD salesManager1 AS CHARACTER FORMAT "x(3)"             LABEL "SM1"
    FIELD salesManager2 AS CHARACTER FORMAT "x(3)"             LABEL "SM2"
    FIELD salesManager3 AS CHARACTER FORMAT "x(3)"             LABEL "SM3"
    FIELD parentCust    AS CHARACTER FORMAT "x(12)"            LABEL "Parent Cust"
    FIELD parentName    AS CHARACTER FORMAT "x(30)"            LABEL "Parent Cust Name"
    FIELD accountType   AS CHARACTER FORMAT "x(12)"            LABEL "Account Type"
    FIELD marketSegment AS CHARACTER FORMAT "x(16)"            LABEL "Market Segment"
    FIELD splitType     AS INTEGER   FORMAT ">>9"              LABEL "Split"
    FIELD naicsCode     AS CHARACTER FORMAT "x(6)"             LABEL "NAICS"
    FIELD naicsDescrip  AS CHARACTER FORMAT "x(64)"            LABEL "NAICS Description"
    .
DEFINE TEMP-TABLE ttReport NO-UNDO LIKE report
    FIELD row-id AS ROWID
    .
DEFINE TEMP-TABLE ttSalesRep NO-UNDO
    FIELD sman    AS   CHARACTER
    FIELD samt    LIKE ar-invl.amt
    FIELD camt    LIKE ar-invl.amt
    FIELD cost    LIKE ar-invl.amt
    FIELD scat    AS   CHARACTER
        INDEX ttSalesRep IS PRIMARY
            sman
            scat
            .
/* Parameters Definitions ---                                           */

&Scoped-define subjectID 104
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cBasis           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCategory        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCost            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustPart        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFGItem          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProdCat         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSalesRep        AS CHARACTER NO-UNDO EXTENT 3.
    DEFINE VARIABLE cShipID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWhse            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dAmount          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCommission      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostAmt         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPct         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dDeviationCost   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dFreightCost     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dFullCost        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dFullGP          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvPct          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dManufactureCost AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dMSFCalc         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dProfit          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSalesComm       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSalesPct        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotalCost       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotalGP         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dUseCost         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dWarehouseCost   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dtInvoiceDate    AS DATE      NO-UNDO.
    DEFINE VARIABLE iBOLNo           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iInvoiceNo       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iJobNo2          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrdNo           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iQty             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lComponent       AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bARCashl FOR ar-cashl.
    DEFINE BUFFER bARInvl  FOR ar-invl.
    DEFINE BUFFER bCust    FOR cust.
    DEFINE BUFFER bItemFG  FOR itemfg.
    DEFINE BUFFER bfItemFG FOR itemfg.

    FOR EACH cust NO-LOCK
        WHERE cust.company      EQ cCompany
          AND cust.cust-no      GE cStartCustNo
          AND cust.cust-no      LE cEndCustNo
          AND cust.type         GE cStartCustType
          AND cust.type         LE cEndCustType
          AND cust.spare-char-2 GE cStartSalesGroup
          AND cust.spare-char-2 LE cEndSalesGroup
        :
        FOR EACH ar-inv NO-LOCK
            WHERE ar-inv.company  EQ cust.company
              AND ar-inv.posted   EQ YES
              AND ar-inv.cust-no  EQ cust.cust-no
              AND ar-inv.inv-date GE dtStartInvoiceDate
              AND ar-inv.inv-date LE dtEndInvoiceDate,
            EACH ar-invl NO-LOCK
            WHERE ar-invl.x-no EQ ar-inv.x-no
              AND ar-invl.misc EQ NO
            :
            RUN oe/invlcomp.p (ROWID(ar-invl), OUTPUT lComponent).
            IF lComponent THEN NEXT.
            cCategory = "".
            IF ar-invl.misc THEN DO:
                FIND FIRST prep NO-LOCK
                     WHERE prep.company EQ ar-invl.company
                       AND prep.code    EQ ar-invl.i-name
                     NO-ERROR.
                IF AVAILABLE prep THEN
                cCategory = prep.fgcat.
            END. /* if misc */
            ELSE DO:
                FIND FIRST itemfg NO-LOCK
                    WHERE itemfg.company EQ ar-invl.company
                      AND itemfg.i-no    EQ ar-invl.i-no
                    NO-ERROR.
                IF AVAILABLE itemfg THEN
                cCategory = itemfg.procat.
            END. /* else */
            IF cCategory LT cStartProCat OR
               cCategory GT cEndProCat THEN NEXT.
            DO idx = 1 TO 3:
                cSalesRep[1] = IF ar-invl.sman[idx] EQ "" AND idx EQ 1 THEN cust.sman
                             ELSE ar-invl.sman[idx].              
              IF cSalesRep[1] LT cStartSalesRep OR
                 cSalesRep[1] GT cEndSalesRep   OR
                 (idx NE 1 AND
                 (cSalesRep[1] EQ "" OR ar-invl.s-pct[idx] EQ 0)) THEN NEXT.
              CREATE ttReport.
              ASSIGN
                  ttReport.key-01 = cSalesRep[1]
                  ttReport.key-02 = cust.cust-no
                  ttReport.key-03 = STRING(ar-inv.inv-no,"9999999")
                  ttReport.key-10 = "ar-invl"
                  ttReport.rec-id = RECID(ar-invl)
                  ttReport.row-id = ROWID(ar-invl)
                  .
            END. /* do idx */
        END. /* each ar-inv */

        FOR EACH ar-cashl NO-LOCK
            WHERE ar-cashl.company EQ cust.company
              AND ar-cashl.cust-no EQ cust.cust-no
              AND ar-cashl.posted  EQ YES
              AND ar-cashl.memo    EQ YES
              AND CAN-FIND(FIRST account
                           WHERE account.company EQ ar-cashl.company
                             AND account.actnum  EQ ar-cashl.actnum
                             AND account.type    EQ "R"),
            EACH ar-cash NO-LOCK
            WHERE ar-cash.c-no       EQ ar-cashl.c-no
              AND ar-cash.company    EQ ar-cashl.company
              AND ar-cash.cust-no    EQ ar-cashl.cust-no
              AND ar-cash.check-date GE dtStartInvoiceDate
              AND ar-cash.check-date LE dtEndInvoiceDate
              AND ar-cash.posted     EQ YES
            USE-INDEX c-no
            :
            RELEASE ttReport.
            RELEASE ar-invl.
            RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).
            IF AVAILABLE oe-retl THEN
            FIND FIRST ar-invl NO-LOCK
                 WHERE ar-invl.company EQ ar-cashl.company
                   AND ar-invl.cust-no EQ cust.cust-no
                   AND ar-invl.inv-no  EQ ar-cashl.inv-no
                   AND ar-invl.i-no    EQ oe-retl.i-no
                 NO-ERROR.
            IF ar-cashl.inv-no NE 0 AND
              (AVAILABLE ar-invl    OR
              (NOT AVAILABLE reftable AND
               NOT ar-cashl.dscr MATCHES "*oe return*") OR
               SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,5) EQ "items") THEN
            FOR EACH bARInvl
                WHERE bARInvl.company EQ ar-cashl.company
                  AND bARInvl.cust-no EQ cust.cust-no
                  AND bARInvl.inv-no  EQ ar-cashl.inv-no
                  AND (NOT AVAILABLE ar-invl OR ROWID(bARInvl) EQ ROWID(ar-invl))
                NO-LOCK
                :
                IF AVAILABLE ar-invl THEN DO:
                    RUN oe/invlcomp.p (ROWID(bARInvl), OUTPUT lComponent).
                    IF lComponent THEN NEXT.
                END. /* if avail */
                RELEASE itemfg.
                FIND FIRST itemfg NO-LOCK
                    WHERE itemfg.company EQ bARInvl.company
                      AND itemfg.i-no    EQ bARInvl.i-no
                    NO-ERROR.        
                IF NOT AVAILABLE itemfg OR
                   cCategory LT itemfg.procat OR
                   cCategory GT itemfg.procat THEN NEXT.
                DO idx = 1 TO 3:
                    cSalesRep[1] = IF bARInvl.sman[idx] EQ "" AND idx EQ 1 THEN cust.sman
                                 ELSE bARInvl.sman[idx].      
                    IF cSalesRep[1] LT cStartSalesRep OR
                       cSalesRep[1] GT cEndSalesRep   OR
                       (idx NE 1 AND
                       (cSalesRep[1] EQ "" OR bARInvl.s-pct[idx] EQ 0)) THEN NEXT.
                    CREATE ttReport.
                    ASSIGN
                        ttReport.key-01 = cSalesRep[1]
                        ttReport.key-03 = STRING(bARInvl.inv-no,"9999999")
                        ttReport.row-id = ROWID(bARInvl)
                        ttReport.key-02 = cust.cust-no
                        ttReport.key-10 = "ar-cashl"
                        ttReport.rec-id = RECID(ar-cashl)
                        .
                END. /* do idx */
            END. /* each bARInvl */
            ELSE DO:
                IF cCategory LT cStartProCat OR
                   cCategory GT cEndProCat THEN NEXT.
                ELSE
                IF cust.sman GE cStartSalesRep AND
                   cust.sman LE cEndSalesRep THEN DO:
                    CREATE ttReport.
                    ASSIGN
                        ttReport.key-01 = cust.sman
                        ttReport.key-03 = STRING(ar-cashl.inv-no,"9999999")
                        .
                END. /* else */
                IF AVAILABLE ttReport THEN
                ASSIGN
                    ttReport.key-02 = cust.cust-no
                    ttReport.key-10 = "ar-cashl"
                    ttReport.rec-id = RECID(ar-cashl)
                    .
            END. /* else */
        END. /* each ar-cashl */
    END. /* each cust */

    input-work:
    FOR EACH ttReport,
        FIRST cust NO-LOCK
        WHERE cust.company EQ cCompany
          AND cust.cust-no EQ ttReport.key-02
        BREAK BY ttReport.key-01
              BY ttReport.key-02
              BY ttReport.key-03
              BY ttReport.row-id
              BY ttReport.key-10
              BY ttReport.rec-id
              BY ROWID(ttReport)
        :
        FIND FIRST sman
             WHERE sman.company EQ cust.company
               AND sman.sman    EQ ttReport.key-01
             NO-ERROR.
        RELEASE ar-invl.
        RELEASE ar-cashl.
        ASSIGN
            cCustPart        = ""
            cWhse            = ""
            cJobNo           = ""
            iJobNo2          = 0
            iOrdNo           = 0
            cFGItem          = ""
            iBOLNo           = 0
            dAmount          = 0
            dTotalCost       = 0
            iQty             = 0
            cShipID          = ""
            dFreightCost     = 0
            dWarehouseCost   = 0
            dManufactureCost = 0
            dDeviationCost   = 0
            .
        RUN custom/combasis.p (
            cust.company,
            ttReport.key-01,
            cust.type,
            "",
            0,
            cust.cust-no,
            OUTPUT cBasis
            ).
        IF ttReport.key-10 EQ "ar-invl" THEN
        FIND FIRST ar-invl NO-LOCK
             WHERE RECID(ar-invl) EQ ttReport.rec-id
             NO-ERROR.
        IF AVAILABLE ar-invl THEN DO:
            RELEASE prep.
            RELEASE itemfg.
            FIND FIRST ar-inv NO-LOCK
                 WHERE ar-inv.x-no EQ ar-invl.x-no
                 NO-ERROR.
            dtInvoiceDate = IF AVAILABLE ar-inv THEN ar-inv.inv-date
                                                ELSE ar-invl.inv-date.
            IF ar-invl.misc THEN
            FIND FIRST prep NO-LOCK
                WHERE prep.company EQ ar-invl.company
                  AND prep.code    EQ ar-invl.i-name
                NO-ERROR.
            ELSE
            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ ar-invl.company
                  AND itemfg.i-no    EQ ar-invl.i-no
                NO-ERROR.    
            DO idx = 1 TO 3:
              IF ar-invl.sman[idx] EQ ttReport.key-01 OR
                 ar-invl.sman[1] EQ "" THEN LEAVE.
              IF idx EQ 3 THEN NEXT input-work.
            END. /* do idx */
            ASSIGN
                iInvoiceNo       = ar-invl.inv-no               
                cProdCat         = IF ar-invl.misc THEN
                                       IF AVAILABLE prep THEN prep.fgcat ELSE "MISC"
                                   ELSE
                                       IF AVAILABLE itemfg THEN itemfg.procat ELSE "ARINV"
                dSalesPct        = IF ar-invl.sman[idx] EQ "" OR
                                     (ar-invl.s-pct[idx] EQ 0 AND idx EQ 1) THEN 100
                                   ELSE ar-invl.s-pct[idx]
                iQty             = (IF ar-invl.inv-qty NE 0 THEN ar-invl.inv-qty * dSalesPct / 100
                                    ELSE 0)
                dAmount          = ar-invl.amt * dSalesPct / 100
                dSalesComm       = ar-invl.s-comm[idx]
                cCustPart        = ar-invl.part-no
                cJobNo           = ar-invl.job-no
                iJobNo2          = ar-invl.job-no2
                iOrdNo           = ar-invl.ord-no
                cFGItem          = ar-invl.i-no
                iBOLNo           = ar-invl.bol-no
                dTotalCost       = 0
                cShipID          = ar-inv.ship-id
                dFreightCost     = ar-invl.costStdFreight
                dWarehouseCost   = ar-invl.costStdWarehouse
                dManufactureCost = ar-invl.costStdManufacture
                dDeviationCost   = ar-invl.costStdDeviation
                .
            IF ar-invl.loc NE "" THEN
            cWhse = ar-invl.loc.
            ELSE DO:
                FIND FIRST oe-boll NO-LOCK
                    WHERE oe-boll.company EQ ar-invl.company
                      AND oe-boll.bol-no  EQ ar-invl.bol-no
                      AND oe-boll.i-no    EQ ar-invl.i-no
                    NO-ERROR.
                IF AVAILABLE oe-boll THEN
                cWhse = oe-boll.loc.
            END. /* else */
            IF ar-invl.t-cost NE 0 THEN
            dTotalCost = ar-invl.t-cost * dSalesPct / 100.
            /*when updating cost via A-U-1, bug where t-cost
              was not being updated after posting*/
            ELSE IF ar-invl.cost NE 0 THEN DO:
                IF ar-invl.dscr[1] EQ "M" OR
                   ar-invl.dscr[1] EQ "" THEN
                dTotalCost = ar-invl.cost * (ar-invl.inv-qty / 1000) * dSalesPct / 100.
                ELSE dTotalCost = ar-invl.cost * ar-invl.inv-qty * dSalesPct / 100.              
            END. /* else if */
            IF ar-invl.misc AND AVAILABLE prep AND NOT prep.commissionable THEN
            dSalesComm = 0.
            RUN custom/combasis.p (
                cust.company,
                ttReport.key-01,
                cust.type,
                (IF AVAILABLE itemfg THEN itemfg.procat ELSE ""),
                0,
                cust.cust-no,
                OUTPUT cBasis
                ).
        END. /* if avail */
        ELSE IF ttReport.key-10 EQ "ar-cashl" THEN
        FIND FIRST ar-cashl NO-LOCK
             WHERE RECID(ar-cashl) EQ ttReport.rec-id
             NO-ERROR.
        IF AVAILABLE ar-cashl THEN DO:
            RELEASE oe-retl.
            RELEASE ar-invl.    
            FIND FIRST ar-invl NO-LOCK
                 WHERE ROWID(ar-invl) EQ ttReport.row-id
                 NO-ERROR.
            RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).
            IF AVAILABLE oe-retl AND NOT AVAILABLE ar-invl THEN 
            FIND FIRST ar-invl NO-LOCK
                 WHERE ar-invl.company EQ cust.company
                   AND ar-invl.cust-no EQ cust.cust-no
                   AND ar-invl.inv-no  EQ ar-cashl.inv-no
                   AND ar-invl.i-no    EQ oe-retl.i-no
                 NO-ERROR.
            IF AVAILABLE ar-invl THEN DO:
                FIND FIRST ar-inv NO-LOCK
                     WHERE ar-inv.x-no EQ ar-invl.x-no
                     NO-ERROR.
                dtInvoiceDate = IF AVAILABLE ar-inv THEN ar-inv.inv-date
                                                    ELSE ar-invl.inv-date.
                DO idx = 1 TO 3:
                    IF ar-invl.sman[idx] EQ ttReport.key-01 OR
                       ar-invl.sman[1] EQ "" THEN LEAVE.
                    IF idx EQ 3 THEN NEXT input-work.
                END. /* do idx */
                RELEASE prep.
                RELEASE itemfg.
                IF ar-invl.misc THEN
                FIND FIRST prep NO-LOCK
                     WHERE prep.company EQ ar-invl.company
                       AND prep.code    EQ ar-invl.i-name
                     NO-ERROR.
                ELSE
                FIND FIRST itemfg NO-LOCK
                     WHERE itemfg.company EQ ar-invl.company
                       AND itemfg.i-no    EQ ar-invl.i-no
                     NO-ERROR.
                ASSIGN
                    iInvoiceNo       = ar-invl.inv-no
                    dSalesPct        = IF ar-invl.sman[idx]  EQ "" OR
                                         (ar-invl.s-pct[idx] EQ 0 AND idx EQ 1) THEN 100
                                       ELSE ar-invl.s-pct[idx]
                    dSalesComm       = ar-invl.s-comm[idx]
                    iQty             = 0
                    dAmount          = (ar-cashl.amt-paid - ar-cashl.amt-disc) * (dSalesPct / 100)
                    dTotalCost       = 0 
                    cProdCat         = IF ar-invl.misc THEN
                                          IF AVAILABLE prep   THEN prep.fgcat    ELSE "MISC"
                                       ELSE
                                          IF AVAILABLE itemfg THEN itemfg.procat ELSE "CRMEMO"
                    cCustPart        = ar-invl.part-no
                    cJobNo           = ar-invl.job-no
                    iJobNo2          = ar-invl.job-no2
                    iOrdNo           = ar-invl.ord-no
                    cFGItem          = ar-invl.i-no
                    cShipID          = ar-inv.ship-id
                    dFreightCost     = ar-invl.costStdFreight
                    dWarehouseCost   = ar-invl.costStdWarehouse
                    dManufactureCost = ar-invl.costStdManufacture
                    dDeviationCost   = ar-invl.costStdDeviation
                    .
                IF AVAILABLE oe-retl THEN
                ASSIGN
                    iQty       = oe-retl.tot-qty-return * -1
                    dTotalCost = oe-retl.cost * (oe-retl.tot-qty-return / 1000) * (dSalesPct / 100) * -1
                    .
                ELSE DO:
                    dInvPct = 0.
                    FOR EACH bARInvl FIELDS(amt billable misc i-no) NO-LOCK
                        WHERE bARInvl.x-no EQ ar-invl.x-no
                        :
                        RELEASE bItemFG.
                        FIND FIRST bItemFG NO-LOCK
                            WHERE bItemFG.company EQ bARInvl.company
                              AND bItemFG.i-no    EQ bARInvl.i-no
                            NO-ERROR.        
                        IF NOT AVAILABLE bItemFG OR
                           cCategory LT bItemFG.procat OR
                           cCategory GT bItemFG.procat THEN NEXT.
                        dInvPct = dInvPct + bARInvl.amt.
                        ACCUMULATE 1 (TOTAL).
                    END. /* each barinvl */             
                    dInvPct = IF dInvPct EQ 0 THEN (1 / IF (ACCUM TOTAL 1) EQ 0 THEN 1
                                                      ELSE (ACCUM TOTAL 1))
                              ELSE (ar-invl.amt / dInvPct).
                    IF dInvPct EQ ? THEN
                    dInvPct = 0.          
                    dCostPct = 0.
                    FOR EACH bARCashl FIELDS(amt-paid amt-disc) NO-LOCK
                        WHERE bARCashl.c-no   EQ ar-cashl.c-no
                          AND bARCashl.inv-no EQ ar-cashl.inv-no
                        :
                        dCostPct = dCostPct + (bARCashl.amt-paid - bARCashl.amt-disc).
                    END.
                    dCostPct = (ar-cashl.amt-paid - ar-cashl.amt-disc) / dCostPct.          
                    IF dCostPct EQ ? THEN
                    dCostPct = 0.
                    dAmount = dAmount * dInvPct.
                END. /* else */
                IF ar-invl.misc AND AVAILABLE prep AND NOT prep.commissionable THEN
                dSalesComm = 0.
                RUN custom/combasis.p (
                    cust.company,
                    ttReport.key-01,
                    cust.type,
                    (IF AVAILABLE itemfg THEN itemfg.procat ELSE ""),
                    0,
                    cust.cust-no,
                    OUTPUT cBasis
                    ).
            END. /* if avail */
            ELSE
            ASSIGN
                iInvoiceNo    = ar-cashl.inv-no
                dtInvoiceDate =  ar-cashl.inv-date
                cProdCat      = "CRMEM"
                dSalesPct     = 100
                iQty          = 0
                dAmount       = ar-cashl.amt-paid - ar-cashl.amt-disc
                dTotalCost    = 0
                dSalesComm    = IF AVAILABLE sman THEN sman.scomm ELSE 0
                cCustPart     = ""
                .
        END. /* if avail */
        IF cFGItem NE "" THEN DO:
            IF cCost EQ "E" THEN
            RUN sys/inc/bordcost.p (
                cJobNo,
                iJobNo2,
                cFGItem,
                iBOLNo,
                iQty,
                NO,
                OUTPUT dTotalCost
                ).
            ELSE
            IF cCost EQ "O" AND iOrdNo NE 0 THEN DO:
                FIND FIRST oe-ordl NO-LOCK
                     WHERE oe-ordl.company EQ ar-cashl.company
                       AND oe-ordl.ord-no  EQ iOrdNo
                       AND oe-ordl.i-no    EQ cFGItem
                     NO-ERROR.
                IF AVAILABLE oe-ordl THEN
                dTotalCost = oe-ordl.cost * iQty / 1000.             
            END. /* if cCost */
        END. /* if cfgitem */
        IF dTotalCost EQ ? THEN dTotalCost = 0.
        IF dSalesComm EQ ? THEN dSalesComm = 0.
        IF iQty EQ 0 AND AVAILABLE ar-cashl THEN
        dTotalCost = dAmount.
        ASSIGN
            cSalesRep[1] = ttReport.key-01
            dProfit      = dAmount - dTotalCost
            dCostAmt     = IF cBasis EQ "G" THEN (dProfit * dSalesComm / 100)
                                            ELSE (dAmount * dSalesComm / 100)
                         .
        IF iQty EQ 0 AND AVAILABLE ar-cashl THEN
        ASSIGN
            dCostAmt   = 0
            dTotalCost = 0
            dProfit    = 0
            .
        ASSIGN
            dCommission = ROUND(dCostAmt / dAmount * 100,2)
            dTotalGP    = ROUND(dProfit  / dAmount * 100,2)
            dCostAmt    = ROUND(dCostAmt,2)
            .
        FIND FIRST ttSalesRep
             WHERE ttSalesRep.sman EQ cSalesRep[1]
               AND ttSalesRep.scat EQ cProdCat
             NO-ERROR.
        IF NOT AVAILABLE ttSalesRep THEN DO:
            CREATE ttSalesRep.
            ASSIGN
                ttSalesRep.sman = cSalesRep[1]
                ttSalesRep.scat = cProdCat
                .
        END. /* if not avail */
        ASSIGN 
            ttSalesRep.samt = ttSalesRep.samt + dAmount
            ttSalesRep.camt = ttSalesRep.camt + dCostAmt
            ttSalesRep.cost = ttSalesRep.cost + dTotalCost
            .
        IF dCommission EQ ? THEN dCommission = 0.
        IF dTotalGP    EQ ? THEN dTotalGP    = 0.
        {sys/inc/roundup.i iQty}
        dUseCost = 0.
        IF ar-invl.spare-dec-1 GT 0 THEN
        dUseCost = ar-invl.spare-dec-1.
        ELSE DO:
            FIND FIRST bfItemFG NO-LOCK
                 WHERE bfItemFG.company EQ ar-invl.company
                   AND bfItemFG.i-no    EQ ar-invl.i-no 
                 NO-ERROR.
            IF AVAILABLE bfItemFG AND bfItemFG.spare-dec-1 NE 0 THEN
            dUseCost = bfItemFG.spare-dec-1.
        END.
        IF ar-invl.dscr[1] EQ "M" OR
           ar-invl.dscr[1] EQ ""  OR
           (AVAILABLE bfItemFG AND bfItemFG.prod-uom EQ "M") THEN
        dUseCost = dUseCost * (ar-invl.inv-qty / 1000) * dSalesPct / 100.
        ELSE
        dUseCost = dUseCost * ar-invl.inv-qty * dSalesPct / 100.
        ASSIGN 
            dFullCost = IF dUseCost NE 0 THEN dUseCost ELSE dTotalCost
            dProfit   = dAmount - dFullCost
            dProfit   = IF dProfit EQ ? THEN 0 ELSE dProfit
            dFullGP   = ROUND(dProfit / dAmount * 100 , 2)
            dFullGP   = IF dFullGP EQ ? THEN 0 ELSE dFullGP
            .
        IF AVAILABLE itemfg THEN DO:
            RUN fg/GetFGArea.p (ROWID(itemfg), "MSF", OUTPUT dMsfCalc). 
            dMsfCalc = dMsfCalc * iQty.
        END. /* if avail */
        ELSE dMsfCalc = 0.
        FIND FIRST sman NO-LOCK
             WHERE sman.company EQ cust.company
               AND sman.sman    EQ ttReport.key-01
             NO-ERROR.
        FIND FIRST bCust NO-LOCK
             WHERE bCust.company EQ cust.company
               AND bCust.cust-no EQ cust.parentCust
             NO-ERROR.
        FIND FIRST naics NO-LOCK
             WHERE naics.naicsID EQ cust.naicsCode
             NO-ERROR.
        CREATE ttCommissions.
        ASSIGN
            ttCommissions.salesRep      = ttReport.key-01
            ttCommissions.custNo        = cust.cust-no
            ttCommissions.custName      = cust.name
            ttCommissions.custType      = cust.type
            ttCommissions.fgItem        = cFGItem
            ttCommissions.custPart      = cCustPart
            ttCommissions.orderNo       = iOrdNo
            ttCommissions.invoiceNo     = iInvoiceNo
            ttCommissions.category      = cProdCat
            ttCommissions.quantity      = iQty
            ttCommissions.sellPrice     = dAmount
            ttCommissions.totalCost     = dTotalCost
            ttCommissions.fullCost      = dFullCost
            ttCommissions.totalGP       = dTotalGP
            ttCommissions.fullGP        = dFullGP
            ttCommissions.commission    = dCommission
            ttCommissions.salesGroup    = cust.spare-char-2
            ttCommissions.currency      = cust.curr-code
            ttCommissions.invoiceDate   = dtInvoiceDate
            ttCommissions.warehouse     = cWhse
            ttCommissions.shipID        = cShipID
            ttCommissions.msf           = dMSFCalc
            ttCommissions.freightCost   = dFreightCost
            ttCommissions.whseCost      = dWarehouseCost
            ttCommissions.manufCost     = dManufactureCost
            ttCommissions.deviaCost     = dDeviationCost
            ttCommissions.salesManager1 = IF AVAILABLE sman THEN sman.salesManager  ELSE ""
            ttCommissions.salesManager2 = IF AVAILABLE sman THEN sman.salesManager2 ELSE ""
            ttCommissions.salesManager3 = IF AVAILABLE sman THEN sman.salesManager3 ELSE ""
            ttCommissions.parentCust    = cust.parentCust
            ttCommissions.parentName    = IF AVAILABLE bCust THEN bCust.name ELSE ""
            ttCommissions.accountType   = ENTRY(LOOKUP(cust.accountType,",H,O,S"),",Handed,Originated,Split")
            ttCommissions.marketSegment = cust.marketSegment
            ttCommissions.splitType     = cust.splitType
            ttCommissions.naicsCode     = cust.naicsCode
            ttCommissions.naicsDescrip  = IF AVAILABLE naics THEN naics.description ELSE "" 
            .
    END. /* each ttReport */
END PROCEDURE.
