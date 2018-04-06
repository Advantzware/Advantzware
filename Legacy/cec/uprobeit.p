/* ------------------------------------------------- cec/uprobeit.p 10/96 JLF */

{sys/inc/var.i shared}
{sys/form/s-top.f}

DEFINE NEW SHARED VARIABLE qty          AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-shared-rel AS INTEGER NO-UNDO.

DEFINE INPUT PARAMETER v-recid AS RECID.

DEFINE SHARED BUFFER xest FOR est.
DEFINE SHARED BUFFER xef  FOR ef.
DEFINE SHARED BUFFER xeb  FOR eb.

{cec/print4.i shared shared}
{cec/print42.i shared}

DEFINE BUFFER b-blk     FOR blk.
DEFINE BUFFER b-probemk FOR reftable.
DEFINE BUFFER probe-ref FOR reftable.

DEFINE VARIABLE qm                 AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-comm             LIKE tt-tot NO-UNDO.
DEFINE VARIABLE v-royl             LIKE tt-tot NO-UNDO.
DEFINE VARIABLE v-ware             LIKE tt-tot NO-UNDO.
DEFINE VARIABLE v-cust             LIKE tt-tot NO-UNDO.
DEFINE VARIABLE v-nman             LIKE tt-tot NO-UNDO.
DEFINE VARIABLE v-price            LIKE tt-tot NO-UNDO.
DEFINE VARIABLE blk-cost           LIKE blk.cost.
DEFINE VARIABLE blk-fact           LIKE blk.fact.
DEFINE VARIABLE v-yld              AS DECIMAL.
DEFINE VARIABLE v-qty              AS DECIMAL.
DEFINE VARIABLE v-rel              AS DECIMAL.
DEFINE VARIABLE lv-sell-by         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-sell-by-ce-ctrl AS CHARACTER NO-UNDO.
DEFINE VARIABLE ll-use-margin      AS LOG       NO-UNDO.
DEFINE VARIABLE board-cst          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-tmp-set-markup   LIKE probe.set-chg NO-UNDO.
DEFINE VARIABLE v-freight          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-found            AS LOG       NO-UNDO.
DEFINE VARIABLE dBoardCst          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dBoardPct          AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE dMarginCostG       AS DECIMAL  NO-UNDO.
DEFINE        VARIABLE dMarginCostN       AS DECIMAL  NO-UNDO.
DEFINE SHARED TEMP-TABLE tt-rel NO-UNDO LIKE reftable.
                              

IF xest.est-type LT 6 THEN LEAVE.

FIND probe WHERE RECID(probe) EQ v-recid.

{sys/inc/cerun.i C}

{cec/combasis.i}

{cec/msfcalc.i}

{cec/rollfac.i}

ASSIGN
    qty                = probe.est-qty
    qm                 = qty / 1000 * v-sqft-fac
 
    blk-cost           = 0
    blk-fact           = 0
    i                  = 0
    
    lv-sell-by         = ce-ctrl.sell-by
    lv-sell-by-ce-ctrl = ce-ctrl.sell-by.

IF cerunc EQ "Fibre" THEN
    RUN est/usemargin.p (ROWID(xest), OUTPUT ll-use-margin).

FIND FIRST probeit
    WHERE probeit.company EQ probe.company
    AND probeit.est-no  EQ probe.est-no
    AND probeit.line    EQ probe.line
    NO-LOCK NO-ERROR.

IF AVAILABLE probeit THEN 
DO:
    FIND FIRST blk NO-ERROR.
    IF NOT AVAILABLE blk THEN
        FOR EACH eb
            WHERE eb.company EQ xest.company 
            AND eb.est-no  EQ xest.est-no
            AND eb.form-no NE 0
            NO-LOCK:
            CREATE blk.
            ASSIGN
                blk.kli  = eb.cust-no 
                blk.id   = eb.part-no
                blk.snum = eb.form-no
                blk.bnum = eb.blank-no
                blk.qreq = IF eb.est-type GE 7 THEN probeit.bl-qty ELSE probeit.yld-qty
                blk.qyld = probeit.yld-qty
                blk.yr$  = eb.yrprice.
        END.

    ASSIGN
        v-tt-tot   = 0
        v-ord-cost = 0.
    
    FOR EACH probeit
        WHERE probeit.company EQ probe.company
        AND probeit.est-no  EQ probe.est-no
        AND probeit.line    EQ probe.line:

        ASSIGN
            v-comm            = (probeit.sell-price - (IF v-basis EQ "G" THEN probeit.fact-cost ELSE 0)) *
              (probe.comm / 100)
            probeit.full-cost = probeit.full-cost - v-comm.

        FIND FIRST blk WHERE blk.id EQ probeit.part-no NO-ERROR.

        ASSIGN
            v-yld                = IF xest.est-type LT 7 OR probeit.yrprice THEN probeit.yld-qty
                                                      ELSE probeit.bl-qty

            v-tt-tot[blk.snum]   = v-tt-tot[blk.snum] +
                            (probeit.full-cost * (v-yld / 1000))
            v-ord-cost[blk.snum] = v-ord-cost[blk.snum] +
                            (probeit.fact-cost * (v-yld / 1000))

            blk.cost             = probeit.full-cost * (v-yld / 1000)
            blk.fact             = probeit.fact-cost * (v-yld / 1000).
    END.
END.

FOR EACH blk:
    ASSIGN
        blk-cost = blk-cost + blk.cost
        blk-fact = blk-fact + blk.fact.
END.

IF lv-sell-by EQ "S" THEN 
DO:
{cec/sqftmrkp.i probe.tot-lbs v-pct}
END.

FOR EACH blk BREAK BY blk.snum:
    IF vmclean2 AND FIRST-OF(blk.snum) THEN 
    DO:
        ASSIGN
            blk-cost = 0
            blk-fact = 0.
  
        FOR EACH b-blk WHERE b-blk.snum EQ blk.snum:
            ASSIGN
                blk-cost = blk-cost + b-blk.cost
                blk-fact = blk-fact + b-blk.fact.
        END.
    END.
  
    FIND FIRST ef
        WHERE ef.company  EQ xest.company
        AND ef.est-no   EQ xest.est-no
        AND ef.form-no  EQ blk.snum
        NO-LOCK NO-ERROR.
  
    FIND FIRST eb
        WHERE eb.company  EQ xest.company
        AND eb.est-no   EQ xest.est-no
        AND eb.form-no  EQ blk.snum
        AND eb.blank-no EQ blk.bnum
        NO-LOCK NO-ERROR.
  
    v-yld = IF eb.est-type GE 7 THEN 1
    ELSE
        IF eb.quantityPerSet LT 0  THEN (-1 / eb.quantityPerSet)
        ELSE eb.quantityPerSet.
  
    FIND FIRST probeit
        WHERE probeit.company EQ probe.company
        AND probeit.est-no  EQ probe.est-no
        AND probeit.line    EQ probe.line
        AND probeit.part-no EQ blk.id
        NO-ERROR.
    IF NOT AVAILABLE probeit THEN 
    DO:
        CREATE probeit.
        ASSIGN
            probeit.company   = xest.company
            probeit.est-no    = xest.est-no
            probeit.e-num     = xest.e-num
            probeit.line      = probe.line
            probeit.part-no   = blk.id
            probeit.cust-no   = eb.cust-no
            probeit.part-no   = eb.part-no
            probeit.yrprice   = eb.yrprice
            probeit.mku_gsa-l = IF eb.pur-man THEN rm-rate-f ELSE ce-ctrl.rm-rate
            probeit.mku_gsa-m = IF eb.pur-man THEN fg-rate-f ELSE ce-ctrl.fg-rate
            probeit.mku_com   = probe.comm.
    END. /* new */
  
    IF eb.est-type GE 7 THEN
        ASSIGN
            probeit.bl-qty    = probeit.bl-qty + blk.qreq
            probeit.yld-qty   = probeit.yld-qty + blk.qyld
            probeit.fact-cost = probeit.fact-cost + blk.fact
            probeit.full-cost = probeit.full-cost + blk.cost.
    ELSE 
    DO:
        ASSIGN
            probeit.bl-qty         = probeit.bl-qty + (qty * v-yld)
            probeit.yld-qty        = probeit.yld-qty + (qty * v-yld)
            probeit.contrib-pct[1] = probeit.contrib-pct[1] + (qty * v-yld).
  
        IF vmclean2 THEN
            ASSIGN
                probeit.full-cost = v-tt-tot[blk.snum]   * (blk.cost / blk-cost)
                probeit.fact-cost = v-ord-cost[blk.snum] * (blk.fact / blk-fact).
        ELSE
            ASSIGN
                probeit.full-cost = probe.full-cost * (blk.cost / blk-cost) * qm
                probeit.fact-cost = probe.fact-cost * (blk.fact / blk-fact) * qm.
    END.
END.

FIND FIRST probe-ref
    WHERE probe-ref.reftable EQ "probe-ref"
    AND probe-ref.company  EQ probe.company
    AND probe-ref.loc      EQ ""
    AND probe-ref.code     EQ probe.est-no
    AND probe-ref.code2    EQ STRING(probe.line,"9999999999")
    NO-ERROR.
IF NOT AVAILABLE probe-ref THEN 
DO:
    CREATE probe-ref.
    ASSIGN
        probe-ref.reftable = "probe-ref"
        probe-ref.company  = probe.company
        probe-ref.loc      = ""
        probe-ref.code     = probe.est-no
        probe-ref.code2    = STRING(probe.line,"9999999999").
END.
ASSIGN
    probe-ref.val[6] = 0
    probe-ref.val[7] = 0
    probe-ref.val[8] = 0
    probe-ref.val[9] = 0.

FOR EACH probeit
    WHERE probeit.company EQ probe.company
    AND probeit.est-no  EQ probe.est-no
    AND probeit.line    EQ probe.line:

    IF xest.est-type LT 7 OR probeit.yrprice THEN
        v-qty = IF probeit.contrib-pct[1] NE 0 THEN probeit.contrib-pct[1] ELSE probeit.yld-qty.
    ELSE
        v-qty = probeit.bl-qty.

    ASSIGN
        probeit.full-cost = probeit.full-cost / (v-qty / 1000)
        probeit.fact-cost = probeit.fact-cost / (v-qty / 1000)
        v-rel             = 0.

    FOR EACH eb NO-LOCK
        WHERE eb.company EQ probeit.company
        AND eb.est-no  EQ probeit.est-no
        AND eb.part-no EQ probeit.part-no,
        FIRST tt-rel NO-LOCK
        WHERE tt-rel.reftable EQ "ce/com/selwhif1.w"
        AND tt-rel.company  EQ eb.company
        AND tt-rel.loc      EQ eb.est-no
        AND tt-rel.code     EQ STRING(eb.form-no,"9999999999")
        AND tt-rel.code2    EQ STRING(eb.blank-no,"9999999999"):
        v-rel = v-rel + tt-rel.val[1].
    END.

    FIND FIRST b-probemk
        WHERE b-probemk.reftable EQ "ce/com/probemk.p"
        AND b-probemk.company  EQ probeit.company
        AND b-probemk.loc      EQ probeit.est-no
        AND b-probemk.code     EQ STRING(probeit.line,"9999999999")
        AND b-probemk.code2    EQ probeit.part-no
        NO-ERROR.
    IF NOT AVAILABLE b-probemk THEN 
    DO:
        CREATE b-probemk.
        ASSIGN
            b-probemk.reftable = "ce/com/probemk.p"
            b-probemk.company  = probeit.company
            b-probemk.loc      = probeit.est-no
            b-probemk.code     = STRING(probeit.line,"9999999999")
            b-probemk.code2    = probeit.part-no.
    END.
    b-probemk.val[1] = b-probemk.val[1] + (IF v-rel EQ 0 THEN 1 ELSE v-rel).

    IF b-probemk.val[1] GT probe.freight THEN probe.freight = b-probemk.val[1].

    board-cst = 0. 
    FOR EACH blk WHERE blk.id EQ probeit.part-no,
        FIRST ef NO-LOCK
        WHERE ef.company EQ xest.company
        AND ef.est-no  EQ xest.est-no
        AND ef.form-no EQ blk.snum,
        EACH brd WHERE brd.form-no EQ ef.form-no:

        board-cst = board-cst + (brd.cost-m * blk.pct * (t-blkqty[ef.form-no] / 1000)).
    END.

    board-cst = board-cst / (v-qty / 1000).
    
   
    /*For ticket 19263 - storage of total man hours op-tot[8] = tot MR man hours, op-tot[9] = tot Run man hours*/    
    FIND FIRST reftable
        WHERE reftable.reftable EQ "probe.board"
        AND reftable.company  EQ probe.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ probe.est-no
        AND reftable.code2    EQ STRING(probe.line,"9999999999")
        NO-ERROR.
            
    dBoardCst = 0.
    IF xest.est-type EQ  6  THEN /*If estimate is a set, calculate per item board cost and total fact cost*/
    DO:
        FOR EACH blk WHERE blk.id EQ probeit.part-no,
            FIRST ef NO-LOCK
            WHERE ef.company EQ xest.company
            AND ef.est-no  EQ xest.est-no
            AND ef.form-no EQ blk.snum,
            EACH brd WHERE brd.form-no EQ ef.form-no, 
            FIRST ITEM NO-LOCK
            WHERE item.company EQ ef.company
            AND item.i-no EQ brd.i-no
            AND item.mat-type EQ "B":

            dBoardCst = dBoardCst + (brd.cost-m * blk.pct * (t-blkqty[ef.form-no] / 1000)).
        END.
/*        dBoardCst = dBoardCst / (v-qty / 1000).*/

   
        IF probeit.fact-cost GT 0 THEN 
            dBoardPct = (dBoardCst / (v-qty / 1000)) / probeit.fact-cost * 100.
        IF AVAILABLE reftable THEN 
        DO:
            reftable.val[6] = dTotalManHrs.
            FIND CURRENT reftable NO-LOCK. 
        END.
    END. /*Set calc*/
    ELSE /*Tandems, combos*/
    DO:
         
    
        /*Pull the Total Board Cost from the probe.board (Reftable)*/
        IF AVAILABLE reftable THEN 
        DO:
            reftable.val[6] = op-tot[8] + op-tot[9].
            dBoardCst = reftable.val[1].
            FIND CURRENT reftable NO-LOCK.
        END.    
            
        /*Use the ord-cost which is the Total Factory Cost to determine the reduction lookup pct*/
        IF ord-cost GT 0 THEN 
            dBoardPct = dBoardCst / ord-cost * 100.
       
    END.
    
    /*eb is not available at this point and is needed for markup matrix lookup - find the non-set header blank*/       
    IF NOT AVAILABLE eb THEN 
        FIND FIRST eb NO-LOCK
            WHERE eb.company EQ probeit.company
            AND eb.est-no EQ probeit.est-no
            AND eb.part-no EQ probeit.part-no
            AND eb.form-no GT 0
            NO-ERROR.
    IF AVAILABLE eb THEN DO:
        ASSIGN 
            iFormNo = eb.form-no
            iBlankNo = eb.blank-no
            dMCostToExcludeMisc = 0
            dMPriceToAddMisc = 0
            .
        FOR EACH ttPrepMiscM NO-LOCK 
            WHERE ttPrepMiscM.iForm EQ eb.form-no
            AND (ttPrepMiscM.iBlank EQ eb.blank-no
                OR ttPrepMiscM.iBlank EQ 0):
                ASSIGN  
                    dMCostToExcludeMisc = dMCostToExcludeMisc + ttPrepMiscM.dCostTotal
                    dMPriceToAddMisc = dMPriceToAddMisc + ttPrepMiscM.dPriceTotal
                    .
            DELETE ttPrepMiscM.
        END.
        
    END.
    RUN custom/markup.p (ROWID(eb),
        dBoardCst,
        probeit.fact-cost * (v-qty / 1000),
        probeit.full-cost * (v-qty / 1000),
        dBoardPct,
        INPUT-OUTPUT lv-sell-by,
        INPUT-OUTPUT v-pct).
 
    /*  IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN DO:                        */
    /*    board-cst = 0.                                                                   */
    /*                                                                                     */
    /*    FOR EACH blk WHERE blk.id EQ probeit.part-no,                                    */
    /*        FIRST ef NO-LOCK                                                             */
    /*        WHERE ef.company EQ xest.company                                             */
    /*          AND ef.est-no  EQ xest.est-no                                              */
    /*          AND ef.form-no EQ blk.snum,                                                */
    /*        EACH brd WHERE brd.form-no EQ ef.form-no:                                    */
    /*                                                                                     */
    /*      board-cst = board-cst + (brd.cost-m * blk.pct * (t-blkqty[ef.form-no] / 1000)).*/
    /*    END.                                                                             */
    /*                                                                                     */
    /*    board-cst = board-cst / (v-qty / 1000).                                          */
    /*  END.                                                                               */
    IF AVAILABLE probe THEN
        FIND FIRST est-summ
            WHERE est-summ.company EQ probe.company
            AND est-summ.est-no  EQ probe.est-no
            NO-LOCK NO-ERROR.

    v-freight = 0. 
    v-found = FALSE.
    IF AVAILABLE est-summ THEN 
    DO:
        FOR EACH est-summ
            WHERE est-summ.company EQ probe.company
            AND est-summ.est-no  EQ probe.est-no
            /* AND est-summ.e-num   EQ probe.line  */
            /*AND est-summ.part-no EQ probeit.part-no */
            USE-INDEX est-qty NO-LOCK.
            FIND FIRST eb NO-LOCK
                WHERE eb.company EQ probeit.company
                AND eb.est-no  EQ probeit.est-no
                AND eb.part-no EQ probeit.part-no
                NO-ERROR.
    
            IF AVAILABLE eb THEN 
            DO:
                IF SUBSTR(est-summ.summ-tot,31) BEGINS "Freight" AND v-found  THEN 
                DO:
                    v-freight = est-summ.per-m.
                    LEAVE.
                END.
    
                IF SUBSTR(est-summ.summ-tot,31) BEGINS "Total Fact" 
                    AND ABS(est-summ.per-m - (eb.yld-qty * probeit.fact-cost)) < .02 THEN
                    v-found = TRUE. 
            END.
    
        END.
    END.

    IF ll-use-margin THEN v-pct = probe.market-price.

    IF xest.est-type EQ 6 AND probe.set-chg NE 0 AND vmclean2 THEN
        v-tmp-set-markup = probe.set-chg.

/*    ASSIGN                                                        */
/*        dMCostToExcludeMisc = dMCostToExcludeMisc / (v-qty / 1000)*/
/*        dMCostToExcludePrep = dMCostToExcludePrep / (v-qty / 1000)*/
/*        dMPriceToAddMisc = dMPriceToAddMisc / (v-qty / 1000)      */
/*        dMPriceToAddPrep = dMPriceToAddPrep / (v-qty / 1000)      */
/*        .                                                         */
    dMarginCostG = IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN board-cst ELSE probeit.fact-cost.
    dMarginCostN = IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN 0
        ELSE (IF lv-sell-by = "F" THEN v-freight ELSE (probeit.full-cost - probeit.fact-cost)).
    
    ASSIGN /*calculate totals before price/commission calc*/
    dMarginCostG = dMarginCostG * (v-qty / 1000)
    dMarginCostN = dMarginCostN * (v-qty / 1000)
    .

    /*Exclude SIMON = M Costs from Price Margin Calculation*/
    dMarginCostG = dMarginCostG - dMCostToExcludeMisc - dMCostToExcludePrep.
     /*this commission logic also in oe/ordfrest.i and jc/jc-calc.p*/
    RUN est/CalcSellPrice.p (lv-sell-by-ce-ctrl,
        lv-sell-by,
        v-basis,
        dMarginCostG,
        dMarginCostN,
        (IF ll-use-margin OR
        (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN 0
        ELSE probe.comm),
        v-pct + v-tmp-set-markup,
        dMPriceToAddMisc + dMPriceToAddPrep,
        OUTPUT probeit.sell-price,
        OUTPUT v-comm).
    FIND FIRST ttCostHeader EXCLUSIVE-LOCK 
        WHERE ttCostHeader.estimateNo EQ probeit.est-no
        AND ttCostHeader.formNo EQ iFormNo
        AND ttCostHeader.blankNo EQ iBlankNo
        AND ttCostHeader.quantityMaster EQ iMasterQuantity
        AND ttCostHeader.jobNo EQ cJobNo
        AND ttCostHeader.jobNo2 EQ iJobNo2
    NO-ERROR.
    IF AVAILABLE ttCostHeader THEN DO: 
        ASSIGN 
            ttCostHeader.stdCostBoard = dBoardCst * (v-qty / 1000)
            ttCostHeader.stdCostCommission = v-comm 
            ttCostHeader.stdSellPrice = probeit.sell-price
            ttCostHeader.stdCostFull            = probeit.full-cost * (v-qty / 1000) + ttCostHeader.stdCostCommission
            ttCostHeader.stdProfitGross         = ttCostHeader.stdSellPrice - ttCostHeader.stdCostTotalFactory
            ttCostHeader.stdProfitNet           = ttCostHeader.stdSellPrice - ttCostHeader.stdCostFull
            .        
            
    END.
    ASSIGN 
        v-comm = v-comm / (v-qty / 1000)
        probeit.sell-price = probeit.sell-price / (v-qty / 1000)
        dMCostToExcludeMisc = 0
        dMCostToExcludePrep = 0
        dMPriceToAddMisc = 0
        dMPriceToAddPrep = 0
        .

    IF ll-use-margin OR
        (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN
        v-comm = probeit.sell-price * probe.comm / 100.

    ASSIGN
        v-price           = v-price + (probeit.sell-price * (v-qty / 1000))
        v-nman            = v-comm
        probeit.full-cost = probeit.full-cost + v-comm
        b-probemk.val[2]  = probe.comm
        b-probemk.val[6]  = v-comm * (v-qty / 1000)
        b-probemk.val[7]  = v-royl * (v-qty / 1000)
        b-probemk.val[8]  = v-ware * (v-qty / 1000)
        b-probemk.val[9]  = v-cust * (v-qty / 1000)
        probe-ref.val[6]  = probe-ref.val[6] + b-probemk.val[6]
        probe-ref.val[7]  = probe-ref.val[7] + b-probemk.val[7]
        probe-ref.val[8]  = probe-ref.val[8] + b-probemk.val[8]
        probe-ref.val[9]  = probe-ref.val[9] + b-probemk.val[9].
END.

ASSIGN
    probe-ref.val[2]  = probe-ref.val[6] / v-price * 10000000
    probe-ref.val[3]  = probe-ref.val[7] / v-price * 10000000
    probe-ref.val[4]  = probe-ref.val[8] / v-price * 10000000
    probe-ref.val[5]  = probe-ref.val[9] / v-price * 10000000
    probe-ref.val[10] = fac-tot2 - fac-tot.
