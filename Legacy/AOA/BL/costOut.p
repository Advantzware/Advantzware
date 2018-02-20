/*------------------------------------------------------------------------
  File: costOut.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Cost Out Report.rpa */
{AOA/tempTable/ttCostOutReport.i}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttCostOutReport.
{AOA/includes/pCostOutReport.i}

/* local variables */
DEFINE NEW SHARED VARIABLE cocode AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE locode AS CHARACTER NO-UNDO.

DEFINE VARIABLE cUOM             AS CHARACTER NO-UNDO.
DEFINE VARIABLE dEstMatCost      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dActMatCost      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dEstLabCost      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dActLabCost      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dEstCost         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dActCost         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dDifference      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lSuppressEstCost AS LOGICAL   NO-UNDO.

/* subject business logic */
{jc/rep/job-sum.i NEW}

DEFINE BUFFER bWorkMat FOR work-mat.
DEFINE BUFFER bItem    FOR item.

ASSIGN
    cocode      = ipcCompany
    locode      = cLocation
    cStartJobNo = FILL(" ",6 - LENGTH(TRIM(cStartJobNo)))
                + TRIM(cStartJobNo)
    cEndJobNo   = FILL(" ",6 - LENGTH(TRIM(cEndJobNo)))
                + TRIM(cEndJobNo)
    .
FOR EACH job NO-LOCK
    WHERE   job.company EQ ipcCompany
      AND  (job.opened EQ (cOpened EQ "Yes") OR cOpened EQ "Both")
      AND ((FILL(" ",6 - LENGTH(TRIM(job.job-no))) +
            TRIM(job.job-no) +
            STRING(job.job-no2,"99") GE cStartJobNo + STRING(iStartJobNo2,"99")
      AND   FILL(" ",6 - LENGTH(TRIM(job.job-no))) +
            TRIM(job.job-no) +
            STRING(job.job-no2,"99") LE cEndJobNo + STRING(iEndJobNo2,"99"))
       OR   lAllJobNo EQ YES)
      AND ((job.close-date GE dtStartDate
      AND   job.close-date LE dtEndDate
      AND   job.opened     EQ NO)
       OR  (job.start-date GE dtStartDate
      AND   job.start-date LE dtEndDate
      AND   job.opened     EQ YES)),
    FIRST job-hdr NO-LOCK
    WHERE job-hdr.company  EQ job.company
      AND job-hdr.job      EQ job.job
      AND job-hdr.job-no   EQ job.job-no
      AND job-hdr.job-no2  EQ job.job-no2
    BREAK BY job.job-no
          BY job.job-no2
    :
    /* build work-mat for material costs */
    EMPTY TEMP-TABLE work-mat.    
    RUN pEstMatCost.
    RUN pActMatCost.
    /* build work-mch for labor costs */
    EMPTY TEMP-TABLE work-mch.
    RUN jc/rep/job-sumi.p (RECID(job)).
    RUN pMatPrdQty.
    FIND FIRST work-item NO-ERROR.
    IF NOT AVAILABLE work-item THEN NEXT.
    RUN jc/rep/job-sumr.p (ROWID(job),
        ipcCompany,
        cLocation,
        NO,  /* exclude run if no production */
        "Standard",
        YES, /* use current mach rates */
        NO   /* use crew */
        ).
    ASSIGN
        dEstLabCost   = 0
        dActLabCost   = 0
        .
    FOR EACH work-mch:
        IF work-mch.est-mr-cost  EQ ? THEN work-mch.est-mr-cost  = 0.
        IF work-mch.est-run-cost EQ ? THEN work-mch.est-run-cost = 0.
        IF work-mch.mr-cost1     EQ ? THEN work-mch.mr-cost1     = 0.
        IF work-mch.mr-cost2     EQ ? THEN work-mch.mr-cost2     = 0.
        IF work-mch.mr-cost3     EQ ? THEN work-mch.mr-cost3     = 0.
        IF work-mch.run-cost1    EQ ? THEN work-mch.run-cost1    = 0.
        IF work-mch.run-cost2    EQ ? THEN work-mch.run-cost2    = 0.
        IF work-mch.run-cost3    EQ ? THEN work-mch.run-cost3    = 0.
        ASSIGN
            dEstLabCost = dEstLabCost
                        + work-mch.est-mr-cost
                        + work-mch.est-run-cost
            dActLabCost = dActLabCost
                        + work-mch.mr-cost1
                        + work-mch.mr-cost2
                        + work-mch.mr-cost3
                        + work-mch.run-cost1
                        + work-mch.run-cost2
                        + work-mch.run-cost3
                        .
        IF dEstLabCost EQ ? THEN dEstLabCost = 0.
        IF dActLabCost EQ ? THEN dActLabCost = 0.
    END. /* each work-mch */
    ASSIGN
        dEstMatCost = 0
        dActMatCost = 0
        .
    FOR EACH work-mat:
        lSuppressEstCost = NO.
        FIND FIRST item NO-LOCK 
             WHERE item.company EQ job.company
               AND item.i-no    EQ work-mat.i-no
             NO-ERROR.
        IF AVAILABLE item THEN DO: 
            IF work-mat.prd-qty NE 0 AND 
               work-mat.act-qty NE 0 AND 
               work-mat.est-qty NE 0 THEN DO:
                IF work-mat.act-qty-uom NE "EA" THEN
                RUN sys/ref/convquom.p (
                    "EA",
                    work-mat.act-qty-uom,
                    work-mat.basis-w,
                    work-mat.len,
                    work-mat.wid,
                    item.s-dep,
                    work-mat.prd-qty,
                    OUTPUT work-mat.prd-qty
                    ).
                work-mat.est-cost = work-mat.est-cost / work-mat.est-qty * work-mat.prd-qty.
            END. /* qty's ne ea */
            IF item.mat-type EQ "D" THEN DO:
                IF work-mat.act-qty GT 0 THEN DO:
                    FOR EACH bWorkMat
                        WHERE ROWID(bWorkMat)   NE ROWID(work-mat) 
                          AND bWorkMat.est-cost GT 0,
                        FIRST bItem NO-LOCK 
                        WHERE bItem.company  EQ job.company
                          AND bItem.i-no     EQ bWorkMat.i-no
                          AND bItem.mat-type EQ "D"
                        USE-INDEX i-no
                        :
                        LEAVE.
                    END. /* each bworkmat */
                    lSuppressEstCost =  AVAILABLE bWorkMat.                                                
                END. /* act-qty gt 0 */
            END. /* avail item */
        END. /* avail item */
        IF lSuppressEstCost EQ NO THEN
        dEstMatCost = dEstMatCost + work-mat.est-cost.
        dActMatCost = dActMatCost + work-mat.act-cost.
    END. /* each work-mat */

    IF dEstMatCost EQ ? THEN dEstMatCost = 0.
    IF dActMatCost EQ ? THEN dActMatCost = 0.
    
    ASSIGN
        dEstCost    = dEstLabCost + dEstMatCost
        dActCost    = dActLabCost + dActMatCost
        dDifference = dEstCost    - dActCost
        .
    CREATE ttCostOutReport.
    ASSIGN
        ttCostOutReport.jobNo        = job-hdr.job-no
        ttCostOutReport.estLaborCost = dEstLabCost
        ttCostOutReport.actLaborCost = dActLabCost
        ttCostOutReport.estMatCost   = dEstMatCost
        ttCostOutReport.actMatCost   = dActMatCost
        ttCostOutReport.totalEstCost = dEstCost
        ttCostOutReport.totalActCost = dActCost
        ttCostOutReport.difference   = dDifference
        .
    RUN pJobDetail (ROWID(job)).
END. /* each job, each jo-hdr */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pActMatCost:
    DEFINE VARIABLE dCost AS DECIMAL NO-UNDO.
    
    FOR EACH mat-act NO-LOCK
        WHERE mat-act.company EQ job.company
          AND mat-act.job     EQ job.job,
        FIRST item FIELDS(r-wid basis-w s-wid s-len s-dep mat-type) NO-LOCK
        WHERE item.company EQ mat-act.company
          AND item.i-no    EQ mat-act.i-no
        USE-INDEX i-no
        :    
        FIND FIRST work-mat
             WHERE work-mat.i-no    EQ mat-act.i-no
               AND work-mat.form-no EQ mat-act.s-num
             NO-ERROR.    
        IF NOT AVAILABLE work-mat THEN DO:    
            CREATE work-mat.
            ASSIGN
                work-mat.i-no     = mat-act.i-no
                work-mat.form-no  = mat-act.s-num
                work-mat.board    = item.mat-type EQ "B"
                work-mat.mat-type = item.mat-type
                .
        END. /* not avail work-mat */
        IF item.r-wid EQ 0 THEN
            RUN sys/ref/convcuom.p (
               (IF work-mat.sc-uom GT "" THEN work-mat.sc-uom
                ELSE mat-act.qty-uom),
                mat-act.qty-uom,
               (IF work-mat.basis-w NE 0 THEN work-mat.basis-w
                ELSE item.basis-w),
               (IF work-mat.len     NE 0 THEN work-mat.len
                ELSE item.s-len),
               (IF work-mat.wid     NE 0 THEN work-mat.wid
                ELSE item.s-wid),
                item.s-dep, 
                mat-act.cost,
                OUTPUT dCost
                ).    
        ELSE
            RUN sys/ref/convcuom.p (
               (IF work-mat.sc-uom GT "" THEN work-mat.sc-uom
                ELSE mat-act.qty-uom),
                mat-act.qty-uom,
               (IF work-mat.basis-w NE 0 THEN work-mat.basis-w
                ELSE item.basis-w),
                work-mat.len,
               (IF work-mat.wid     NE 0 THEN work-mat.wid
                ELSE item.r-wid),
                item.s-dep, 
                mat-act.cost,
                OUTPUT dCost
                ).
        IF mat-act.ext-cost EQ 0 OR mat-act.ext-cost EQ ? THEN
            work-mat.act-cost = work-mat.act-cost + mat-act.qty * dCost.
        ELSE
            work-mat.act-cost = work-mat.act-cost + mat-act.ext-cost.

        ASSIGN
            work-mat.act-qty     = work-mat.act-qty + mat-act.qty
            work-mat.act-qty-uom = mat-act.qty-uom
            .    
        IF work-mat.act-cost EQ ? THEN work-mat.act-cost = 0.
    END. /* each mat-act */
END PROCEDURE.

PROCEDURE pEstMatCost:
    DEFINE VARIABLE dCost AS DECIMAL NO-UNDO.
    
    FOR EACH job-mat NO-LOCK
        WHERE job-mat.company EQ job.company
          AND job-mat.job     EQ job.job
        USE-INDEX seq-idx,        
        FIRST item FIELDS(mat-type s-dep) NO-LOCK
        WHERE item.company EQ job-mat.company
          AND item.i-no    EQ job-mat.i-no
        USE-INDEX i-no        
        BREAK BY job-mat.frm
              BY item.mat-type
              BY job-mat.j-no
              BY job-mat.rec_key
        :    
        FIND FIRST work-mat
             WHERE work-mat.i-no    EQ job-mat.i-no
               AND work-mat.form-no EQ job-mat.frm
               AND work-mat.len     EQ job-mat.len
               AND work-mat.wid     EQ job-mat.wid
               AND work-mat.n-up    EQ job-mat.n-up
             NO-ERROR.    
        IF NOT AVAILABLE work-mat THEN DO:
            CREATE work-mat.
            ASSIGN
                work-mat.i-no     = job-mat.i-no
                work-mat.form-no  = job-mat.frm
                work-mat.sc-uom   = job-mat.sc-uom
                work-mat.basis-w  = job-mat.basis-w
                work-mat.len      = job-mat.len
                work-mat.wid      = job-mat.wid
                work-mat.n-up     = job-mat.n-up
                work-mat.board    = item.mat-type EQ "B"
                work-mat.mat-type = item.mat-type
                .
        END. /* not avail work-mat */
        RUN sys/ref/convcuom.p (
            job-mat.sc-uom,
            job-mat.qty-uom,
            job-mat.basis-w,
            job-mat.len,
            job-mat.wid,
            item.s-dep,
            job-mat.std-cost,
            OUTPUT dCost
            ).    
        ASSIGN
            work-mat.est-qty     = work-mat.est-qty  + job-mat.qty
            work-mat.est-qty-uom = job-mat.qty-uom
            work-mat.est-cost    = work-mat.est-cost + job-mat.qty * dCost
            .
    END. /* each job-mat */
END PROCEDURE.

PROCEDURE pJobDetail:
    DEFINE INPUT PARAMETER iprJobRowID AS ROWID NO-UNDO.
    
    DEFINE VARIABLE cSalesRep      AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE dSellingPrice  AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE iQtyOrdered    AS INTEGER          NO-UNDO.
    DEFINE VARIABLE iTotalOrdered  AS INTEGER          NO-UNDO.
    DEFINE VARIABLE iQtyProduced   AS INTEGER          NO-UNDO.
    DEFINE VARIABLE dAvgPrice      AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dSales         AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER          NO-UNDO.
    DEFINE VARIABLE dPct           AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dPriceAmount LIKE oe-ord.t-revenue NO-UNDO.
    DEFINE VARIABLE dRevenue     LIKE oe-ordl.t-price  NO-UNDO.
    DEFINE VARIABLE dProfitPer     AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dOrdQty      LIKE oe-ordl.qty      NO-UNDO.
    DEFINE VARIABLE dQM            AS DECIMAL          NO-UNDO.

    DEFINE BUFFER job-hdr FOR job-hdr.
    DEFINE BUFFER job     FOR job.
    
    FIND FIRST job NO-LOCK WHERE ROWID(job) EQ iprJobRowID.
    
    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ job.company
          AND job-hdr.job     EQ job.job
          AND job-hdr.job-no  EQ job.job-no
          AND job-hdr.job-no2 EQ job.job-no2
        BREAK BY job-hdr.i-no
              BY job-hdr.frm
        :
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ job-hdr.company
               AND cust.cust-no EQ job-hdr.cust-no
             NO-ERROR.
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ job-hdr.company
               AND itemfg.i-no    EQ job-hdr.i-no
             NO-ERROR.    
        FIND FIRST oe-ord NO-LOCK
             WHERE oe-ord.company EQ job-hdr.company
               AND oe-ord.ord-no  EQ job-hdr.ord-no
             NO-ERROR.
        FIND FIRST oe-ordl NO-LOCK
             WHERE oe-ordl.company EQ job-hdr.company
               AND oe-ordl.ord-no  EQ job-hdr.ord-no
               AND oe-ordl.i-no    EQ job-hdr.i-no
             NO-ERROR.
        IF LAST-OF(job-hdr.frm) THEN DO:
            ASSIGN
                iQtyOrdered   = job-hdr.qty
                dSellingPrice = IF AVAILABLE itemfg THEN itemfg.sell-price ELSE 0
                cUOM          = IF AVAILABLE itemfg THEN itemfg.sell-uom ELSE ""
                cSalesRep     = ""
                dProfitPer    = 0
                .
            IF AVAILABLE oe-ordl THEN DO:
                ASSIGN
                    iQtyOrdered   = oe-ordl.qty
                    dSellingPrice = oe-ordl.price
                    cUOM          = oe-ordl.pr-uom
                    .
                DO idx = 1 TO EXTENT(oe-ordl.s-man):
                    IF oe-ordl.s-man[idx] EQ "" THEN NEXT.
                    ASSIGN
                        dPct         = oe-ordl.s-pct[idx] / 100
                        dOrdQty      = oe-ordl.qty * dPct
                        dPriceAmount = oe-ordl.t-price * dPct
                        dQM          = oe-ordl.qty / 1000
                        dRevenue     = dPriceAmount
                        dProfitPer   = (dRevenue
                                     - (oe-ordl.cost * dQM))
                                     / dRevenue * 100
                        .
                    FIND FIRST sman NO-LOCK
                         WHERE sman.company EQ oe-ordl.company
                           AND sman.sman    EQ oe-ordl.s-man[idx]
                         NO-ERROR.
                    IF AVAILABLE sman THEN
                        cSalesRep = sman.sname.
                    LEAVE.
                END. /* do idx */
            END. /* avail oe-ordl */
            iTotalOrdered = iTotalOrdered + iQtyOrdered.
            IF cUOM NE "M" THEN DO:
                IF cUOM BEGINS "L" THEN
                    dSellingPrice = dSellingPrice / iQtyOrdered *
                                   (IF iQtyOrdered LT 0 THEN -1 ELSE 1000).
                ELSE
                IF cUOM EQ "CS" THEN
                    dSellingPrice = dSellingPrice
                                  / (IF AVAILABLE oe-ordl AND oe-ordl.cas-cnt NE 0 THEN oe-ordl.cas-cnt
                                     ELSE IF AVAILABLE itemfg AND itemfg.case-count NE 0 THEN itemfg.case-count ELSE 1)
                                  * 1000.
                ELSE
                IF cUOM EQ "C" THEN
                    dSellingPrice = dSellingPrice / 100.
                ELSE
                    dSellingPrice = dSellingPrice * 1000.
            END. /* if uom ne m */
            dSales = dSales + iQtyOrdered / 1000 * dSellingPrice.
            IF AVAILABLE itemfg AND itemfg.isaset AND itemfg.alloc THEN
            FOR EACH fg-act FIELDS(qty) NO-LOCK
                WHERE fg-act.company EQ job-hdr.company
                  AND fg-act.job-no  EQ job-hdr.job-no
                  AND fg-act.job-no2 EQ job-hdr.job-no2
                  AND fg-act.i-no    EQ job-hdr.i-no
                :
                iQtyProduced = iQtyProduced + fg-act.qty.
            END. /* each fg-act */
        
            FOR EACH fg-rcpth NO-LOCK
                WHERE fg-rcpth.company   EQ job-hdr.company
                  AND fg-rcpth.i-no      EQ job-hdr.i-no
                  AND fg-rcpth.job-no    EQ job-hdr.job-no
                  AND fg-rcpth.job-no2   EQ job-hdr.job-no2
                  AND fg-rcpth.rita-code EQ "R",    
                EACH fg-rdtlh NO-LOCK
                WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                  AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code            
                BREAK BY fg-rcpth.company
                :    
                iQtyProduced = iQtyProduced + fg-rdtlh.qty.
            END. /* each fg-rcpth */
        END. /* last frm */
        IF LAST(job-hdr.frm) THEN DO:
            IF AVAILABLE cust THEN
            ttCostOutReport.custName = cust.name.
            ASSIGN
                dAvgPrice                    = dSales    / (iTotalOrdered / 1000)
                dSales                       = dAvgPrice * (iQtyProduced  / 1000)
                ttCostOutReport.dieNo        = IF AVAILABLE itemfg THEN itemfg.die-no ELSE ""
                ttCostOutReport.salesRep     = cSalesRep
                ttCostOutReport.itemNo       = job-hdr.i-no
                ttCostOutReport.sellingPrice = dSellingPrice
                ttCostOutReport.qtyOrdered   = iTotalOrdered
                ttCostOutReport.category     = IF AVAILABLE itemfg THEN itemfg.procat ELSE ""
                ttCostOutReport.qtyProduced  = iQtyProduced
                ttCostOutReport.sales        = dSales
                ttCostOutReport.bookedCF     = dProfitPer
                ttCostOutReport.estimatedDF  = dSales
                                             - (dSales
                                             * (ttCostOutReport.bookedCF
                                             / 100))
                ttCostOutReport.actualCF     = ((dSales
                                             - ttCostOutReport.totalActCost)
                                             / dSales)
                                             * 100
                ttCostOutReport.xxSort       = IF cSort EQ "Die No"    THEN ttCostOutReport.dieNo
                                          ELSE IF cSort EQ "Sales Rep" THEN ttCostOutReport.salesRep
                                          ELSE IF cSort EQ "Customer"  THEN ttCostOutReport.custName
                                          ELSE IF cSort EQ "Item No"   THEN ttCostOutReport.itemNo
                                          ELSE ""
                                             + job-hdr.job-no + STRING(job-hdr.job-no2,"99")
                .
        END. /* if last */
    END. /* each job-hdr */
END PROCEDURE.

PROCEDURE pMatPrdQty:
    DEFINE VARIABLE lMultiForms AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iNumForms   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMostSheets AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNumUp      AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNumOut     AS INTEGER NO-UNDO.
    DEFINE VARIABLE dAvgQty   AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bWorkMat  FOR work-mat.
    DEFINE BUFFER bWorkItem FOR work-item.
    
    FOR EACH work-item:
        IF CAN-FIND(FIRST bWorkItem
           WHERE bWorkItem.i-no EQ bWorkItem.i-no
             AND bWorkItem.form-no NE bWorkItem.form-no) THEN DO:
              lMultiForms = YES.
              LEAVE.
        END. /* if can-find */
    END. /* each work-item */

    IF lMultiForms EQ YES AND
       CAN-FIND(FIRST work-item
                WHERE work-item.qty-prod GT 0) THEN
    FOR EACH work-item
          BY work-item.form-no
        :      
        iNumForms = 1.
        FOR EACH bWorkItem
            WHERE bWorkItem.i-no    EQ work-item.i-no
              AND bWorkItem.form-no NE work-item.form-no
            :
            iNumForms = iNumForms + 1.
        END. /* each bworkitem */
        ASSIGN
            dAvgQty = (work-item.qty-prod + work-item.est-spo) / iNumForms
            iNumUp  = 1
            .       
        FIND FIRST eb NO-LOCK
             WHERE eb.company EQ job.company
               AND eb.est-no EQ job.est-no
               AND eb.form-no EQ work-item.form-no
               AND eb.stock-no EQ work-item.i-no
             NO-ERROR.
        IF AVAILABLE eb THEN
        iNumUp = eb.num-up.
        dAvgQty = dAvgQty / iNumUp.
        {sys/inc/roundup.i dAvgQty}
        work-item.avg-qty = dAvgQty.
    END. /* each work-item */

    FOR EACH work-mat
        BREAK BY work-mat.i-no
        :
        FIND FIRST item NO-LOCK
             WHERE item.company EQ job.company
               AND item.i-no    EQ work-mat.i-no
             USE-INDEX i-no
             NO-ERROR.
        IF FIRST-OF(work-mat.i-no) THEN
        FOR EACH bWorkMat
            WHERE bWorkMat.i-no    EQ work-mat.i-no
              AND bWorkMat.est-qty NE 0
            :
            IF AVAILABLE item AND item.mat-type EQ "B" THEN DO:
                IF lMultiForms EQ NO THEN DO:
                    IF work-mat.n-up EQ 0 THEN DO:
                        iNumUp = 1.
                        FIND FIRST ef NO-LOCK
                             WHERE ef.company EQ job.company
                               AND ef.est-no  EQ job.est-no
                               AND ef.form-no EQ bWorkMat.form-no
                             NO-ERROR.
                        IF AVAILABLE ef THEN DO:
                            RUN sys/inc/numup.p (job.company, job.est-no, bWorkMat.form-no, OUTPUT iNumUp).
                            RUN est/ef-#out.p (ROWID(ef), OUTPUT iNumOut).
                            iNumUp = iNumUp * iNumOut.
                        END. /* avail ef */
                    END. /* n-up eq 0 */
                    ELSE iNumUp = work-mat.n-up.         
                    IF CAN-FIND(FIRST work-item
                                WHERE work-item.form-no  EQ bWorkMat.form-no
                                  AND work-item.qty-prod GT 0) THEN DO:          
                        FOR EACH work-item
                            WHERE work-item.form-no EQ bWorkMat.form-no
                            :                 
                            work-mat.prd-qty = work-mat.prd-qty
                                             + ((work-item.qty-prod
                                             + work-item.est-spo)
                                             / iNumUp).
                        END. /* each work-item */
                    END. /* if can-find */
                END.
                ELSE /*item on multiple forms*/ DO:
                    IF CAN-FIND(FIRST work-item 
                                WHERE work-item.qty-prod GT 0) THEN DO:
                        FOR EACH work-item
                            BREAK BY work-item.form-no
                            :
                            IF FIRST-OF(work-item.form-no) THEN DO:
                                iMostSheets = -1.                   
                                FOR EACH bWorkItem
                                    WHERE bWorkItem.form-no EQ work-item.form-no
                                    :                   
                                    IF bWorkItem.avg-qty GT iMostSheets THEN
                                    iMostSheets = bWorkItem.avg-qty.
                                END. /* each bWorkItem */
                                work-mat.prd-qty = work-mat.prd-qty + iMostSheets.
                            END. /* if first-of */
                        END. /* each work-item */
                    END. /* if can-find */
                END. /* else */
            END. /* avail item */
            IF RECID(bWorkMat) NE recid(work-mat) THEN DO:     
                ASSIGN
                    work-mat.est-qty  = work-mat.est-qty  + bWorkMat.est-qty
                    work-mat.est-cost = work-mat.est-cost + bWorkMat.est-cost
                    work-mat.act-qty  = work-mat.act-qty  + bWorkMat.act-qty
                    work-mat.act-cost = work-mat.act-cost + bWorkMat.act-cost
                    .
                DELETE bWorkMat.
            END. /* if recid */
        END. /* each bWorkMat */
    END. /* each work-mat */
END PROCEDURE.
