/*------------------------------------------------------------------------
  File:         expARInv.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 8.13.2019
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttTempTable
DEFINE TEMP-TABLE ttTempTable NO-UNDO
    FIELD exportText AS CHARACTER FORMAT "X(22)" INITIAL "Export of AR Invoices"
    FIELD exportDate AS DATE      FORMAT "99/99/9999"
    FIELD exportTime AS CHARACTER FORMAT "x(11)" 
    .
DEFINE TEMP-TABLE tt-report LIKE report.

DEFINE BUFFER btt-report FOR tt-report.

DEFINE TEMP-TABLE w-data NO-UNDO
    FIELD sorter   AS CHARACTER
    FIELD i-no   LIKE ar-invl.i-no
    FIELD inv-no LIKE ar-invl.inv-no
    FIELD rec-id   AS RECID
    .

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 35
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* ************************  Function Prototypes ********************** */

FUNCTION fExtCost RETURNS DECIMAL 
    (dCost AS DECIMAL, cUom AS CHARACTER, dQty AS DECIMAL) FORWARD.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBoardCost:
/* -------------------------------------------- sys/inc/boardCost.p 01/02 JLF */
/* Calculate the total cost of the board received for a job/item              */
/* -------------------------------------------------------------------------- */
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER      NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobNo  LIKE job.job-no     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobNo2 LIKE job.job-no2    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemNo LIKE itemfg.i-no    NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBOLNo  LIKE oe-boll.bol-no NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQty      AS INTEGER        NO-UNDO.
    DEFINE INPUT  PARAMETER iplActCost  AS LOGICAL        NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTotCost  AS DECIMAL        NO-UNDO.

    DEFINE VARIABLE iJobQty  LIKE job-hdr.qty  NO-UNDO.
    DEFINE VARIABLE dCost      AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE iEstType LIKE est.est-type NO-UNDO.

    IF ipcJobNo EQ "" THEN DO:
        /* Find BOL for item. */
        FIND FIRST oe-boll NO-LOCK
             WHERE oe-boll.company EQ ipcCompany
               AND oe-boll.bol-no  EQ ipiBOLNo
               AND oe-boll.i-no    EQ ipcItemNo
             NO-ERROR.
        IF AVAILABLE oe-boll THEN
        ASSIGN
            ipcJobNo  = oe-boll.job-no
            ipiJobNo2 = oe-boll.job-no2
            .
    END. /* if ipcJobNo eq "" */

    FIND FIRST job NO-LOCK
         WHERE job.company EQ ipcCompany
           AND job.job-no  EQ ipcJobNo
           AND job.job-no2 EQ ipiJobNo2
         NO-ERROR.            
    IF AVAILABLE job THEN DO:
        FIND FIRST job-hdr NO-LOCK
             WHERE job-hdr.company EQ job.company
               AND job-hdr.job     EQ job.job
               AND job-hdr.job-no  EQ job.job-no
               AND job-hdr.job-no2 EQ job.job-no2
               AND job-hdr.i-no    EQ ipcItemNo
             NO-ERROR.
        IF AVAILABLE job-hdr THEN DO:
            IF job.est-no NE "" THEN
                FIND FIRST est NO-LOCK
                     WHERE est.company EQ job.company
                       AND est.est-no  EQ job.est-no
                     NO-ERROR.
            iEstType = IF AVAILABLE est THEN est.est-type ELSE 1.
            IF iEstType GT 4 THEN
            iEstType = iEstType - 4.
    
            FOR EACH fg-rcpth NO-LOCK
                WHERE fg-rcpth.company   EQ job-hdr.company
                  AND fg-rcpth.job-no    EQ job-hdr.job-no
                  AND fg-rcpth.job-no2   EQ job-hdr.job-no2
                  AND fg-rcpth.i-no      EQ job-hdr.i-no
                  AND fg-rcpth.rita-code EQ "R",
                EACH fg-rdtlh NO-LOCK
                WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                :
                iJobQty = iJobQty + fg-rdtlh.qty.
            END.
    
            IF iplActCost THEN DO:
                FOR EACH mat-act NO-LOCK
                    WHERE mat-act.company EQ job-hdr.company
                      AND mat-act.job     EQ job-hdr.job
                      AND mat-act.job-no  EQ job-hdr.job-no
                      AND mat-act.job-no2 EQ job-hdr.job-no2
                      AND (mat-act.s-num  EQ job-hdr.frm
                       OR iEstType EQ 2),
                    FIRST job-mat NO-LOCK
                    WHERE job-mat.company  EQ mat-act.company
                      AND job-mat.job      EQ mat-act.job
                      AND job-mat.frm      EQ mat-act.s-num
                      AND job-mat.blank-no EQ mat-act.b-num
                      AND job-mat.i-no     EQ mat-act.i-no,
                    FIRST item NO-LOCK
                    WHERE item.company EQ mat-act.company
                      AND item.i-no    EQ mat-act.i-no
                      AND INDEX("BA",item.mat-type) GT 0
                    :      
                    IF item.r-wid EQ 0 THEN
                        RUN sys/ref/convcuom.p (
                            job-mat.sc-uom,
                            mat-act.qty-uom,
                           (IF job-mat.basis-w  NE 0 THEN
                            job-mat.basis-w ELSE item.basis-w),
                           (IF job-mat.len      NE 0 THEN
                            job-mat.len ELSE item.s-len),
                           (IF job-mat.wid      NE 0 THEN
                            job-mat.wid ELSE item.s-wid),
                            item.s-dep,   
                            mat-act.cost,
                            OUTPUT dCost
                            ).
                    ELSE
                        RUN sys/ref/convcuom.p (
                            job-mat.sc-uom,
                            mat-act.qty-uom,
                           (IF job-mat.basis-w  NE 0 THEN
                            job-mat.basis-w ELSE item.basis-w),
                            job-mat.len,
                           (IF job-mat.wid      NE 0 THEN
                            job-mat.wid ELSE item.r-wid),
                            item.s-dep,   
                            mat-act.cost,
                            OUTPUT dCost
                            ).
                    IF mat-act.ext-cost EQ 0 OR mat-act.ext-cost EQ ? THEN
                        opdTotCost = opdTotCost + (dCost * mat-act.qty).
                    ELSE
                        opdTotCost = opdTotCost + mat-act.ext-cost.
                END.
        
                ASSIGN
                    opdTotCost = opdTotCost * (job-hdr.qty / iJobQty)
                    opdTotCost = opdTotCost / job-hdr.qty * ipiQty
                    .
            END. /* if iplActCost */
      
            ELSE
                FOR EACH job-mat NO-LOCK
                    WHERE job-mat.company EQ job-hdr.company
                      AND job-mat.job     EQ job-hdr.job
                      AND job-mat.job-no  EQ job-hdr.job-no
                      AND job-mat.job-no2 EQ job-hdr.job-no2
                      AND (job-mat.frm    EQ job-hdr.frm
                       OR iEstType EQ 2),
                    FIRST item NO-LOCK
                    WHERE item.company EQ job-mat.company
                      AND item.i-no    EQ job-mat.i-no
                      AND index("BA",item.mat-type) GT 0
                    BREAK BY job-mat.frm 
                          BY item.mat-type
                    :      
                    IF item.mat-type NE "B" OR FIRST-OF(item.mat-type) THEN
                    opdTotCost = opdTotCost + (job-mat.cost-m * ipiQty / 1000).      
                END.
        END.
    END.
END PROCEDURE.

PROCEDURE pBoardCostM:
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER      NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobNo  LIKE job.job-no     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobNo2 LIKE job.job-no2    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemNo LIKE itemfg.i-no    NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBOLNo  LIKE oe-boll.bol-no NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQty      AS INTEGER        NO-UNDO.
    DEFINE INPUT  PARAMETER iplActCost  AS LOGICAL        NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTotCost  AS DECIMAL        NO-UNDO.

    DEFINE VARIABLE iFGRecQty LIKE job-hdr.qty  NO-UNDO.
    DEFINE VARIABLE iEstType  LIKE est.est-type NO-UNDO.
    DEFINE VARIABLE dPOCost     AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE iIssuedQty  AS INTEGER      NO-UNDO.
    DEFINE VARIABLE dSubTot     AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE dSubQty     AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE iReqQty     AS INTEGER      NO-UNDO.

    /* Find BOL for item. */
    FOR EACH oe-boll NO-LOCK
        WHERE oe-boll.company EQ ipcCompany
          AND oe-boll.bol-no  EQ ipiBOLNo
          AND oe-boll.i-no    EQ ipcItemNo
        :
        ASSIGN 
            ipcJobNo  = oe-boll.job-no
            ipiJobNo2 = oe-boll.job-no2
            iFGRecQty = 0
            dSubTot   = 0 
            iReqQty   = 0
            dSubQty   = 0
            .
        FIND FIRST job NO-LOCK
             WHERE job.company EQ oe-boll.company
               AND job.job-no  EQ oe-boll.job-no
               AND job.job-no2 EQ oe-boll.job-no2
             NO-ERROR.    
        IF AVAILABLE job THEN DO:
            FIND FIRST job-hdr NO-LOCK
                WHERE job-hdr.company EQ job.company
                  AND job-hdr.job     EQ job.job
                  AND job-hdr.job-no  EQ job.job-no
                  AND job-hdr.job-no2 EQ job.job-no2
                  AND job-hdr.i-no    EQ ipcItemNo
                NO-ERROR.
            IF AVAILABLE job-hdr THEN DO:
                IF job.est-no NE "" THEN
                    FIND FIRST est NO-LOCK
                         WHERE est.company EQ job.company
                           AND est.est-no  EQ job.est-no
                         NO-ERROR.
                iEstType = IF AVAILABLE est THEN est.est-type ELSE 1.
                IF iEstType GT 4 THEN iEstType = iEstType - 4.
    
                FOR EACH fg-rcpth NO-LOCK
                    WHERE fg-rcpth.company   EQ job-hdr.company
                      AND fg-rcpth.job-no    EQ job-hdr.job-no
                      AND fg-rcpth.job-no2   EQ job-hdr.job-no2
                      AND fg-rcpth.i-no      EQ job-hdr.i-no
                      AND fg-rcpth.rita-code EQ "R",
                    EACH fg-rdtlh NO-LOCK
                    WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                    :
                    iFGRecQty = iFGRecQty + fg-rdtlh.qty.
                END.
         
                FOR EACH mat-act NO-LOCK
                    WHERE mat-act.company EQ job-hdr.company
                      AND mat-act.job     EQ job-hdr.job
                      AND mat-act.job-no  EQ job-hdr.job-no
                      AND mat-act.job-no2 EQ job-hdr.job-no2
                      AND (mat-act.s-num  EQ job-hdr.frm
                       OR iEstType EQ 2),
                    FIRST job-mat NO-LOCK
                    WHERE job-mat.company  EQ mat-act.company
                      AND job-mat.job      EQ mat-act.job
                      AND job-mat.frm      EQ mat-act.s-num
                      AND job-mat.blank-no EQ mat-act.b-num
                      AND job-mat.i-no     EQ mat-act.i-no,
                    FIRST item NO-LOCK
                    WHERE item.company EQ mat-act.company
                      AND item.i-no    EQ mat-act.i-no
                      AND index("BA",item.mat-type) GT 0
                    BREAK BY job-mat.i-no
                    :
                    FIND FIRST job-mch NO-LOCK
                         WHERE job-mch.company EQ job.company 
                           AND job-mch.job     EQ job.job 
                           AND job-mch.job-no  EQ job.job-no 
                           AND job-mch.job-no2 EQ job.job-no2 
                         USE-INDEX line-idx NO-ERROR.
                    IF AVAILABLE job-mch THEN
                    iReqQty = job-mch.run-qty.
              
                    FOR EACH po-ordl NO-LOCK
                        WHERE po-ordl.company   EQ job-mat.company
                          AND po-ordl.i-no      EQ job-mat.i-no
                          AND po-ordl.job-no    EQ ipcJobNo
                          AND po-ordl.job-no2   EQ ipiJobNo2
                        USE-INDEX item 
                        BY po-ordl.po-no DESCENDING
                        :
                        LEAVE.
                    END.        
                    IF AVAILABLE po-ordl THEN DO:
                    ASSIGN
                        dPOCost    = po-ordl.cons-cost * 1000  /* po-ordl.t-cost */
                        iIssuedQty = po-ordl.t-rec-qty
                        .
                    ASSIGN
                        dSubTot    = dSubTot + (iIssuedQty / 1000 * dPOCost)
                        dSubQty    = (dSubTot / (MIN( iIssuedQty / iReqQty, 1) * job-hdr.qty ) * iFGRecQty)
                        opdTotCost = opdTotCost + (dSubQty / 1000 * oe-boll.qty)
                        .
                    END.
                    ELSE DO:
                        IF FIRST-OF(job-mat.i-no) THEN
                        opdTotCost = opdTotCost + oe-boll.qty / 1000 * job-mat.cost-m.
                    END.
                END.  /* job-mat */
            END. /* avail job-hdr */
        END. /* avail job */
    END.  /* for each oe-boll */
END PROCEDURE.

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cDate         AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE dUnitPrice  LIKE ar-invl.unit-pr  NO-UNDO.
    DEFINE VARIABLE cUOM        LIKE ar-invl.pr-uom   NO-UNDO.
    DEFINE VARIABLE dBrdC         AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dOrdC         AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dInvC         AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dMarg         AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dBrdP         AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE d$MSF         AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE iQty          AS INTEGER EXTENT 4 NO-UNDO.
    DEFINE VARIABLE dMSF          AS DECIMAL EXTENT 4 NO-UNDO.
    DEFINE VARIABLE dCost         AS DECIMAL EXTENT 4 NO-UNDO.
    DEFINE VARIABLE dCost1        AS DECIMAL EXTENT 4 NO-UNDO.
    DEFINE VARIABLE dCost2        AS DECIMAL EXTENT 4 NO-UNDO.
    DEFINE VARIABLE dAmt          AS DECIMAL EXTENT 4 NO-UNDO.
    DEFINE VARIABLE cCustNo     LIKE cust.cust-no     NO-UNDO.
    DEFINE VARIABLE cOrderDate    AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cJobNo      LIKE job.job-no       NO-UNDO.
    DEFINE VARIABLE iJobNo2     LIKE job.job-no2      NO-UNDO.
    DEFINE VARIABLE iPONoPO     LIKE ar-invl.po-no-po NO-UNDO.
    DEFINE VARIABLE cShipToName   AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cShipToStreet AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cShipToCity   AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cShipToState  AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cShipToZip    AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE iColor        AS INTEGER          NO-UNDO.
    DEFINE VARIABLE cVColor       AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cStyle        AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cFlute        AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cEstNo        AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cTest         AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE dLen          AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dWid          AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dDep          AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE cCustPartNo   AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cCustPartNo2  AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE iPeriod       AS INTEGER          NO-UNDO.

    IF lPurge THEN
    FOR EACH invoiceLines.
        DELETE invoiceLines.
    END. /* each invoicelines */ 

    FOR EACH company NO-LOCK
        WHERE company.company GE cStartCompany
          AND company.company LE cEndCompany,
        EACH cust NO-LOCK
        WHERE cust.company EQ company.company
          AND cust.cust-no GE cStartCustNo
          AND cust.cust-no LE cEndCustNo
        :
        FOR EACH ar-inv NO-LOCK
            WHERE ar-inv.company  EQ company.company
              AND ar-inv.posted   EQ YES
              AND ar-inv.cust-no  EQ cust.cust-no
              AND ar-inv.inv-date GE dtStartInvoiceDate
              AND ar-inv.inv-date LE dtEndInvoiceDate       
              AND ar-inv.type     NE "FC"
            :
            FIND FIRST invoiceLines NO-LOCK
                 WHERE invoiceLines.company       EQ ar-inv.company 
                   AND invoiceLines.invoiceNumber EQ ar-inv.inv-no
                 NO-ERROR.
            IF AVAILABLE invoiceLines THEN NEXT.
            CREATE tt-report.
            ASSIGN 
                tt-report.term-id = ""
                tt-report.key-08  = ar-inv.company
                tt-report.key-09  = ar-inv.cust-no
                tt-report.key-10  = "ar-inv"
                tt-report.rec-id  = RECID(ar-inv)
                .
        END. /* each ar-inv */
    END. /* each company */
    
    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
          AND tt-report.key-01  EQ ""
          AND tt-report.key-02  EQ ""
          AND tt-report.key-03  EQ ""
          AND tt-report.key-04  EQ ""
          AND tt-report.key-05  EQ ""
          AND tt-report.key-06  EQ ""
          AND tt-report.key-07  EQ ""
        :
        FIND ar-inv NO-LOCK 
             WHERE RECID(ar-inv) EQ tt-report.rec-id.
        FOR EACH ar-invl NO-LOCK
            WHERE ar-invl.x-no EQ ar-inv.x-no
              AND ar-invl.i-no GE cStartItemNo
              AND ar-invl.i-no LE cEndItemNo
              AND ar-invl.misc EQ NO
            :
            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ ar-invl.company
                   AND itemfg.i-no    EQ ar-invl.i-no
                   AND itemfg.procat  GE cStartProCat
                   AND itemfg.procat  LE cEndProCat
                 NO-ERROR.
            IF NOT AVAILABLE itemfg THEN NEXT.
            CREATE btt-report.
            ASSIGN
                btt-report.term-id = ""
                btt-report.rec-id  = RECID(ar-invl)
                btt-report.key-01  = tt-report.key-09
                btt-report.key-02  = ar-invl.i-no
                btt-report.key-03  = STRING(ar-invl.inv-no,"9999999")
                btt-report.key-08  = tt-report.key-08
                btt-report.key-09  = tt-report.key-09
                btt-report.key-10  = "ar-invl"
                .
        END. /* each ar-invl */
    END. /* each tt-report */

    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
        BREAK BY tt-report.key-01
              BY tt-report.key-02
              BY tt-report.key-03
        TRANSACTION:
    
        CREATE w-data.
        ASSIGN
            w-data.i-no   = tt-report.key-02
            w-data.inv-no = INTEGER(tt-report.key-03)
            w-data.rec-id = tt-report.rec-id
            .       
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ tt-report.key-08
               AND itemfg.i-no    EQ w-data.i-no
            NO-ERROR.          
        /* # of colors */
        IF AVAILABLE itemfg THEN DO:
            FOR EACH itemfg-ink OF itemfg NO-LOCK
                WHERE itemfg-ink.i-no EQ itemfg.i-no, 
                EACH item NO-LOCK
                WHERE item.company EQ itemfg-ink.company
                  AND item.i-no EQ itemfg-ink.rm-i-no 
                :
                IF AVAILABLE itemfg-ink THEN
                iColor = iColor + itemfg-ink.occurs.
            END.
            ASSIGN
                cCustPartNo  = itemfg.part-no
                cCustPartNo2 = w-data.i-no
                cStyle       = itemfg.style
                dLen         = itemfg.l-score[50]
                dWid         = itemfg.w-score[50]
                dDep         = itemfg.d-score[50]
                .
            FOR EACH eb FIELDS(test flute est-no) NO-LOCK
                WHERE eb.company  EQ itemfg.company
                  AND eb.est-no   EQ itemfg.est-no
                  AND eb.stock-no EQ itemfg.i-no
                :
                ASSIGN 
                    cTest  = eb.test
                    cFlute = eb.flute
                    .
                LEAVE.
            END. /* each eb */
        END. /* if avail */
        
        IF tt-report.key-10 EQ "ar-invl" THEN DO:
            FIND FIRST ar-invl NO-LOCK WHERE RECID(ar-invl) EQ w-data.rec-id.
            FIND ar-inv NO-LOCK WHERE ar-inv.x-no EQ ar-invl.x-no.
            ASSIGN
                cCustNo    = ar-inv.cust-name
                cDate      = STRING(ar-inv.inv-date)
                dUnitPrice = ar-invl.unit-pr
                cUOM       = ar-invl.pr-uom
                cJobNo     = ar-invl.job-no
                iJobNo2    = ar-invl.job-no2
                iPONoPO    = ar-invl.po-no-po
                iQty[1]    = IF ar-invl.ship-qty GT 0 THEN ar-invl.ship-qty
                             ELSE ar-invl.inv-qty
                dAmt[1]    = ar-invl.amt
                dMSF[1]    = ar-invl.amt-msf
                .        
            FIND FIRST period NO-LOCK
                WHERE period.company EQ ar-inv.company
                  AND period.pst     LE ar-inv.inv-date
                  AND period.pend    GE ar-inv.inv-date
                  AND period.pstat
                NO-ERROR.  
            IF AVAILABLE period THEN
            iPeriod = period.pnum.
         
            FIND FIRST oe-ord NO-LOCK
                 WHERE oe-ord.company EQ ar-inv.company 
                   AND oe-ord.ord-no  EQ ar-inv.ord-no  
                 NO-ERROR.
            IF AVAILABLE oe-ord THEN
            cOrderDate = STRING(oe-ord.ord-date).
            
            FIND FIRST job-hdr NO-LOCK
                 WHERE job-hdr.company EQ ar-invl.company
                   AND job-hdr.job-no  EQ ar-invl.job-no
                   AND job-hdr.job-no2 EQ ar-invl.job-no2
                   AND job-hdr.i-no    EQ w-data.i-no
                 NO-ERROR.            
            IF AVAILABLE(job-hdr) AND job-hdr.est-no NE "" THEN
                cEstNo = TRIM(job-hdr.est-no).
            ELSE
                cEstNo = TRIM(ar-invl.est-no).
            IF cEstNo EQ "" AND AVAILABLE itemfg THEN
            cEstNo = TRIM(itemfg.est-no).
            
            FIND FIRST oe-boll NO-LOCK 
                 WHERE oe-boll.company EQ ar-invl.company
                   AND oe-boll.b-no    EQ ar-invl.b-no
                 NO-ERROR. 
            IF AVAILABLE oe-boll THEN DO:
                FIND FIRST oe-bolh NO-LOCK 
                     WHERE oe-bolh.b-no EQ oe-boll.b-no
                     NO-ERROR.
                IF AVAILABLE oe-bolh THEN 
                FIND FIRST shipto NO-LOCK 
                     WHERE shipto.company EQ oe-bolh.company 
                       AND shipto.cust-no EQ oe-bolh.cust-no 
                       AND shipto.ship-id EQ oe-bolh.ship-id
                    NO-ERROR. 
                IF AVAILABLE shipto THEN 
                ASSIGN 
                    cShipToName   = shipto.ship-name
                    cShipToStreet = shipto.ship-addr[1]
                    cShipToCity   = shipto.ship-city
                    cShipToState  = shipto.ship-state
                    cShipToZip    = shipto.ship-zip
                    . 
            END. /* if avail */
               
            RUN pSaleCost (
                "4",
                ROWID(ar-invl),
                cJobNo,
                iJobNo2,
                iQty[1],
                OUTPUT dCost[1]
                ).
            RUN pSaleCost (
                "2",
                ROWID(ar-invl),
                cJobNo,
                iJobNo2,
                iQty[1],
                OUTPUT dCost1[1]
                ).
            RUN pSaleCost (
                "3",
                ROWID(ar-invl),
                cJobNo,
                iJobNo2,
                iQty[1],
                OUTPUT dCost2[1]
                ).
                
            IF dMSF[1] EQ 0 AND AVAILABLE itemfg THEN
                dMSF[1] = (iQty[1] * itemfg.t-sqft / 1000).
    
            IF dMSF[1] EQ 0 AND AVAILABLE itemfg THEN
                dMSF[1] = (iQty[1] * itemfg.t-sqft / 1000).
    
            ASSIGN
                dBrdC   = dCost[1] / (iQty[1] / 1000)
                dOrdC   = dCost1[1] / (iQty[1] / 1000)
                dInvC   = dCost2[1] / (iQty[1] / 1000)
                dMarg   = dCost[1] / dMSF[1]
                dBrdP   = (dAmt[1] - dCost[1]) / dAmt[1] * 100
                d$MSF   = dAmt[1] / dMSF[1]
                cVColor = STRING(iColor)
                .
            IF dBrdC EQ ? THEN dBrdC = 0.
            IF dOrdC EQ ? THEN dOrdC = 0.
            IF dInvC EQ ? THEN dInvC = 0.
            IF dMarg EQ ? THEN dMarg = 0.
            IF dBrdP EQ ? THEN dBrdP = 0.
            IF d$MSF EQ ? THEN d$MSF = 0.
    
            CREATE InvoiceLines.    
            ASSIGN
                InvoiceLines.AdhesiveCost           = 0
                InvoiceLines.BoardCost              = dBrdC
                InvoiceLines.BoardGrade             = ""
                InvoiceLines.CoatingsCost           = 0
                InvoiceLines.company                = ar-invl.company
                InvoiceLines.ConsolCustName         = ""
                InvoiceLines.CustomerName           = cCustNo
                InvoiceLines.CustomerPONumber       = ar-inv.po-no
                InvoiceLines.CuttingDieNumbers      = ""
                InvoiceLines.DeliveryCost           = ar-invl.t-freight
                InvoiceLines.DeliveryType           = ""
                InvoiceLines.DesignNumber           = ""
                InvoiceLines.DirectCosts            = 0
                InvoiceLines.DirectOverheadCost     = 0
                InvoiceLines.EquipmentRecovery      = 0
                InvoiceLines.FixedCosts             = (IF AVAILABLE itemfg THEN fExtCost(itemfg.std-fix-cost, itemfg.prod-uom, iQty[1]) ELSE 0)
                InvoiceLines.FixedOverHeadCosts     = (IF AVAILABLE itemfg THEN fExtCost(itemfg.std-fix-cost, itemfg.prod-uom, iQty[1]) ELSE 0)
                InvoiceLines.VariableOverheadcost   = (IF AVAILABLE itemfg THEN fExtCost(itemfg.std-var-cost, itemfg.prod-uom, iQty[1]) ELSE 0)
                InvoiceLines.GlueCost               = 0
                InvoiceLines.InkCost                = 0
                InvoiceLines.InvoiceDate            = ar-invl.inv-date
                InvoiceLines.InvoiceLineNumber      = ar-invl.line
                InvoiceLines.InvoiceMonthChar       = ""
                InvoiceLines.InvoiceNumber          = w-data.inv-no
                InvoiceLines.InvoiceType            = ar-inv.type
                InvoiceLines.LabelsCost             = 0
                InvoiceLines.LabourCost             = (IF AVAILABLE itemfg THEN fExtCost(itemfg.std-lab-cost, itemfg.prod-uom, iQty[1]) ELSE 0)
                InvoiceLines.LabourDirectCost       = 0 
                InvoiceLines.LabourIndirectCost     = 0
                InvoiceLines.MachineRouting         = ""
                InvoiceLines.MaintenancePartscost   = 0
                InvoiceLines.MaterialsCost          = (IF AVAILABLE itemfg THEN fExtCost(itemfg.std-mat-cost, itemfg.prod-uom, iQty[1]) ELSE 0)
                InvoiceLines.MSFPerInvoice          = dMSF[1]
                InvoiceLines.OrderNumber            = ar-invl.ord-no
                InvoiceLines.PalletsCost            = 0
                InvoiceLines.PaperCost              = 0
                InvoiceLines.PostingPeriodChar      = "" 
                InvoiceLines.PrintPlateNumbers      = ""
                InvoiceLines.ProductCode            = w-data.i-no
                InvoiceLines.ProductCodeDescription = ""
                InvoiceLines.ProductID              = cCustPartNo
                InvoiceLines.ProductIdDescription   = ""
                InvoiceLines.QuantitySold           = iQty[1]
                InvoiceLines.Rebates                = 0
                InvoiceLines.SalesDollsPerInvoice   = ar-inv.t-sales
                InvoiceLines.SGACosts               = (IF AVAILABLE itemfg THEN fExtCost(itemfg.std-var-cost, itemfg.prod-uom, iQty[1]) ELSE 0)
                InvoiceLines.ShipToCity             = cShipToCity
                InvoiceLines.ShipToCountry          = ""
                InvoiceLines.ShipToName             = cShipToName
                InvoiceLines.ShipToState            = cShipToState
                InvoiceLines.ShipToStreet           = cShipToStreet
                InvoiceLines.ShipToZip              = cShipToZip
                InvoiceLines.StrappingBundlingCost  = 0
                InvoiceLines.StretchWrapCost        = 0
                InvoiceLines.TermsDiscount          = 0
                InvoiceLines.ToolingCost            = 0
                InvoiceLines.TotalCost              = (IF AVAILABLE itemfg THEN fExtCost(itemfg.total-std-cost, itemfg.prod-uom, iQty[1]) ELSE 0)
                InvoiceLines.UnitPrice              = dUnitPrice
                InvoiceLines.UOM                    = cUOM
                InvoiceLines.WarehouseBin           = ""
                InvoiceLines.WarehouseCode          = ""
                InvoiceLines.WarehouseCost          = 0
                InvoiceLines.WeightLbs              = 0
                .                                
        END. /* tt-report.key-10 ar-invl */
    END. /* each tt-report */
    CREATE ttTempTable.
    ASSIGN
        ttTempTable.exportDate = TODAY
        ttTempTable.exportTime = STRING(TIME,"hh:mm:ss am")
        .
END PROCEDURE.

PROCEDURE pSaleCost:
    DEFINE INPUT  PARAMETER ip-type    AS   INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ip-rowid   AS   ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER ip-job-no  LIKE job.job-no  NO-UNDO.
    DEFINE INPUT  PARAMETER ip-job-no2 LIKE job.job-no2 NO-UNDO.
    DEFINE INPUT  PARAMETER ip-qty     AS   INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER op-cost    AS   DECIMAL     NO-UNDO.

    DEFINE VARIABLE cUOM   LIKE itemfg.prod-uom NO-UNDO.
    DEFINE VARIABLE iBOLNo LIKE oe-boll.bol-no  NO-UNDO.

    iBOLNo = 0.
    FIND ar-invl NO-LOCK
         WHERE ROWID(ar-invl) EQ ip-rowid
         NO-ERROR.
    IF AVAILABLE ar-invl THEN DO:
        iBOLNo = ar-invl.bol-no.
        FIND fg-ctrl NO-LOCK
             WHERE fg-ctrl.company EQ ar-invl.company
             NO-ERROR.
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ ar-invl.company
               AND itemfg.i-no    EQ ar-invl.i-no
             NO-ERROR.
        IF ip-type EQ 3 AND ar-invl.t-cost NE ? THEN DO:
            IF ar-invl.dscr[1] EQ "M" OR ar-invl.dscr[1] EQ "" THEN
                op-cost = ip-qty / 1000 * ar-invl.cost.
            ELSE
                op-cost = ip-qty * ar-invl.cost.
        END.

        IF ip-type EQ 2 THEN DO:
            FIND FIRST oe-ordl NO-LOCK
                 WHERE oe-ordl.company EQ ar-invl.company
                   AND oe-ordl.ord-no  EQ ar-invl.ord-no
                   AND oe-ordl.i-no    EQ ar-invl.i-no
                 NO-ERROR.
            IF AVAILABLE oe-ordl THEN
            op-cost = oe-ordl.cost * ip-qty / 1000.
        END.

        IF ip-type EQ 1 THEN
            RUN pBoardCost (
                ar-invl.company,
                ip-job-no,
                ip-job-no2,                            
                ar-invl.i-no,
                iBOLNo,
                ip-qty,
                YES,
                OUTPUT op-cost
                ).
        IF ip-type EQ 4 THEN
            RUN pBoardCostM (
                ar-invl.company,
                ip-job-no,
                ip-job-no2,                            
                ar-invl.i-no,
                iBOLNo,
                ip-qty,
                YES,
                OUTPUT op-cost
                ).

        IF op-cost EQ ? THEN
        op-cost = 0.

        IF op-cost EQ 0 THEN DO:
            FIND FIRST po-ordl NO-LOCK
                 WHERE po-ordl.company   EQ ar-invl.company
                   AND po-ordl.po-no     EQ ar-invl.po-no-po
                   AND po-ordl.i-no      EQ ar-invl.i-no
                   AND po-ordl.deleted   EQ NO
                   AND po-ordl.item-type EQ NO
                   AND po-ordl.job-no    EQ ip-job-no
                   AND po-ordl.job-no2   EQ ip-job-no2
                 USE-INDEX po-no NO-ERROR.
            IF NOT AVAILABLE po-ordl THEN
                FOR EACH po-ordl NO-LOCK
                    WHERE po-ordl.company   EQ ar-invl.company
                      AND po-ordl.i-no      EQ ar-invl.i-no
                      AND po-ordl.deleted   EQ NO
                      AND po-ordl.item-type EQ NO
                      AND po-ordl.job-no    EQ ip-job-no
                      AND po-ordl.job-no2   EQ ip-job-no2
                    USE-INDEX item
                    BY po-ordl.po-no DESCENDING:
                    LEAVE.
                END.
            IF NOT AVAILABLE po-ordl THEN
                FOR EACH po-ordl NO-LOCK
                    WHERE po-ordl.company   EQ ar-invl.company
                      AND po-ordl.i-no      EQ ar-invl.i-no
                      AND po-ordl.deleted   EQ NO
                      AND po-ordl.item-type EQ NO
                    USE-INDEX item
                    BY po-ordl.po-no DESCENDING:
                    LEAVE.
                END.
            IF AVAILABLE po-ordl THEN
            ASSIGN
                op-cost = po-ordl.cons-cost
                cUOM  = po-ordl.cons-uom
                .
            IF op-cost EQ 0 AND AVAILABLE itemfg THEN
            ASSIGN
                op-cost = IF itemfg.i-code EQ "C" AND ip-type EQ 2 THEN
                          itemfg.std-mat-cost ELSE
                          IF fg-ctrl.inv-meth EQ "A" THEN itemfg.avg-cost
                          ELSE itemfg.last-cost
                cUOM    = itemfg.prod-uom
                .
            op-cost = op-cost * ip-qty /
                     (IF cUOM EQ "C"  THEN 100  ELSE
                      IF cUOM EQ "M"  THEN 1000 ELSE
                      IF cUOM EQ "CS" AND AVAILABLE itemfg AND
                      itemfg.case-count NE 0 THEN itemfg.case-count ELSE 1).
        END.

        IF op-cost EQ ? THEN
        op-cost = 0.
    END.
END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fExtCost RETURNS DECIMAL 
    (INPUT dCost AS DECIMAL, INPUT cUom AS CHARACTER , INPUT dQty AS DECIMAL):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE dResult AS DECIMAL NO-UNDO.
    
    dResult = dCost * dQty / 
             (IF cUom EQ "C"  THEN 100  ELSE
              IF cUom EQ "M"  THEN 1000 ELSE
              IF cUom EQ "CS" AND AVAILABLE itemfg AND
              itemfg.case-count NE 0 THEN itemfg.case-count ELSE 1).
    IF dResult = ? THEN
    dResult = 0.

    RETURN dResult.
END FUNCTION.
