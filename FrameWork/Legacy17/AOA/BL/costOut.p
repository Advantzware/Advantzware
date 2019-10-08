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

DEFINE VARIABLE cUOM        AS CHARACTER NO-UNDO.
DEFINE VARIABLE dEstMatCost AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dActMatCost AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dEstLabCost AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dActLabCost AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dEstCost    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dActCost    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dDifference AS DECIMAL   NO-UNDO.

/* subject business logic */
{jc/rep/job-sum.i NEW}

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
    /* build work-mch for labor costs */
    EMPTY TEMP-TABLE work-mch.
    RUN jc/rep/job-sumr.p (
        ROWID(job),
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
    RUN pEstMatCost (OUTPUT dEstMatCost).
    RUN pActMatCost (OUTPUT dActMatCost).
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

PROCEDURE pActMatCost:
    DEFINE OUTPUT PARAMETER opdCost AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dCost             AS DECIMAL NO-UNDO.
    
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
            opdCost = opdCost + mat-act.qty * dCost.
        ELSE
            opdCost = opdCost + mat-act.ext-cost.    
        IF opdCost EQ ? THEN opdCost = 0.
    END. /* each mat-act */
END PROCEDURE.

PROCEDURE pEstMatCost:
    DEFINE OUTPUT PARAMETER opdCost AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE lNonZeroCostFound AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dCost             AS DECIMAL NO-UNDO.
    
    EMPTY TEMP-TABLE work-mat.
    
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
        opdCost = opdCost + job-mat.qty * dCost.
        IF FIRST-OF(item.mat-type) THEN
            lNonZeroCostFound = FALSE.    
        IF INDEX("1234BR",item.mat-type) GT 0 AND lNonZeroCostFound EQ TRUE THEN 
            opdCost = 0.
        IF lNonZeroCostFound EQ FALSE AND opdCost GT 0 THEN
            lNonZeroCostFound = TRUE.    
        IF opdCost EQ ? THEN opdCost = 0.    
    END. /* each job-mat */
END PROCEDURE.
