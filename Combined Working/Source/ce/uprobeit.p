
DEFINE INPUT PARAMETER v-recid AS RECID NO-UNDO.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}

DEFINE NEW SHARED VARIABLE qty AS INTEGER NO-UNDO.

DEFINE SHARED BUFFER xest FOR est.
DEFINE SHARED BUFFER xef  FOR ef.
DEFINE SHARED BUFFER xeb  FOR eb.

{ce/print4.i SHARED SHARED}
{ce/print42.i SHARED}

DEFINE BUFFER b-blk     FOR blk.
DEFINE BUFFER b-probemk FOR reftable.
DEFINE BUFFER probe-ref FOR reftable.

DEFINE VARIABLE qm                 AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-comm             LIKE tt-tot NO-UNDO.
DEFINE VARIABLE v-royl             LIKE tt-tot NO-UNDO.
DEFINE VARIABLE v-ware             LIKE tt-tot NO-UNDO.
DEFINE VARIABLE v-cust             LIKE tt-tot NO-UNDO.
DEFINE VARIABLE v-nman             LIKE tt-tot NO-UNDO.
DEFINE VARIABLE v-price            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE blk-cost           LIKE blk.cost NO-UNDO.
DEFINE VARIABLE blk-fact           LIKE blk.fact NO-UNDO.
DEFINE VARIABLE v-yld              AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-qty              AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-rel              AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lv-sell-by         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-sell-by-ce-ctrl AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-basis            LIKE sman.commbasis INIT "" NO-UNDO.
DEFINE VARIABLE v-pct              LIKE eb.comm INIT 0 EXTENT 5 NO-UNDO.
DEFINE VARIABLE ll-tandem          AS LOG       NO-UNDO.
DEFINE VARIABLE ll-use-margin      AS LOG       NO-UNDO.
DEFINE VARIABLE board-cst          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dBoardCst          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dBoardPct          AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE dMarginCostG       AS DECIMAL  NO-UNDO.
DEFINE        VARIABLE dMarginCostN       AS DECIMAL  NO-UNDO.

DEFINE SHARED TEMP-TABLE tt-rel NO-UNDO LIKE reftable.
                              

IF xest.est-type LT 2 THEN LEAVE.

RUN ce/com/istandem.p (ROWID(xest), OUTPUT ll-tandem).

{sys/inc/cerun.i F}
{sys/inc/msfcalc.i}
{sys/inc/ceround.i}
{sys/inc/ceprice.i}
{sys/inc/cecomm.i}

FIND FIRST ce-ctrl NO-LOCK
    WHERE ce-ctrl.company EQ cocode
    AND ce-ctrl.loc     EQ locode
    NO-ERROR.

IF NOT AVAILABLE ce-ctrl THEN
    FIND FIRST ce-ctrl NO-LOCK
        WHERE ce-ctrl.company    EQ cocode
        AND ce-ctrl.prof-mrkup NE 0
        NO-ERROR.

IF NOT AVAILABLE ce-ctrl THEN
    FIND FIRST ce-ctrl NO-LOCK
        WHERE ce-ctrl.company EQ cocode
        NO-ERROR.

IF AVAILABLE ce-ctrl THEN v-pct[1] = ce-ctrl.prof-mrkup.

FIND probe WHERE RECID(probe) EQ v-recid.

FIND FIRST xeb NO-LOCK
    WHERE xeb.company EQ probe.company
    AND xeb.est-no  EQ probe.est-no
    AND xeb.cust-no NE ""
    NO-ERROR.

IF AVAILABLE xeb THEN
    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ xeb.company
        AND cust.cust-no EQ xeb.cust-no
        NO-ERROR.

IF AVAILABLE cust THEN 
DO:
    RUN custom/combasis.p (cocode, xeb.sman, cust.type, xeb.procat, 0,
        cust.cust-no,
        OUTPUT v-basis).

    IF cust.markup NE 0 THEN v-pct[2] = cust.markup.
END.

ASSIGN
    qty                = probe.est-qty
    qm                 = qty / 1000
 
    blk-cost           = 0
    blk-fact           = 0
    i                  = 0
    
    lv-sell-by         = ce-ctrl.sell-by
    lv-sell-by-ce-ctrl = ce-ctrl.sell-by.

IF cerunf EQ "Fibre" THEN
    RUN est/usemargin.p (ROWID(xest), OUTPUT ll-use-margin).

FIND FIRST probeit NO-LOCK
    WHERE probeit.company EQ probe.company
    AND probeit.est-no  EQ probe.est-no
    AND probeit.line    EQ probe.line
    NO-ERROR.

IF AVAILABLE probeit THEN 
DO:
    FIND FIRST blk NO-ERROR.
    IF NOT AVAILABLE blk THEN
        FOR EACH eb NO-LOCK
            WHERE eb.company EQ xest.company 
            AND eb.est-no  EQ xest.est-no
            AND eb.form-no NE 0:
            CREATE blk.
            ASSIGN
                blk.kli  = eb.cust-no 
                blk.id   = eb.part-no
                blk.snum = eb.form-no
                blk.bnum = eb.blank-no
                blk.qreq = IF eb.est-type GE 3 THEN probeit.bl-qty ELSE probeit.yld-qty
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
            v-yld                = IF xest.est-type LT 3 OR probeit.yrprice THEN probeit.yld-qty
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

    FIND FIRST ef NO-LOCK
        WHERE ef.company  EQ xest.company
        AND ef.est-no   EQ xest.est-no
        AND ef.form-no  EQ blk.snum
        NO-ERROR.

    FIND FIRST eb NO-LOCK
        WHERE eb.company  EQ xest.company
        AND eb.est-no   EQ xest.est-no
        AND eb.form-no  EQ blk.snum
        AND eb.blank-no EQ blk.bnum
        NO-ERROR.

    v-yld = IF eb.est-type GE 3 THEN 1
    ELSE
        IF eb.cust-% LT 0   THEN (-1 / eb.cust-%)
        ELSE eb.cust-%.

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
            probeit.mku_gsa-m = IF eb.pur-man THEN fg-rate-f ELSE ce-ctrl.fg-rate.
    END. /* new */

    IF eb.est-type GE 3 THEN
        ASSIGN
            probeit.bl-qty    = probeit.bl-qty + blk.qreq
            probeit.yld-qty   = probeit.yld-qty + blk.qyld
            probeit.fact-cost = probeit.fact-cost + (blk.fact * ((probe.fact-cost * qm) / blk-fact))
            probeit.full-cost = probeit.full-cost + (blk.cost * ((probe.full-cost * qm) / blk-cost)).

    ELSE 
    DO:
        ASSIGN
            probeit.bl-qty  = probeit.bl-qty + (qty * v-yld)
            probeit.yld-qty = probeit.yld-qty + (qty * v-yld).

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

    ASSIGN
        v-qty             = IF xest.est-type LT 3 OR probeit.yrprice THEN probeit.yld-qty
                                                    ELSE probeit.bl-qty
        probeit.full-cost = probeit.full-cost / (v-qty / 1000)
        probeit.fact-cost = probeit.fact-cost / (v-qty / 1000)
        v-rel             = 0
        ctrl2[18]         = 0.

    FOR EACH eb NO-LOCK
        WHERE eb.company EQ probeit.company
        AND eb.est-no  EQ probeit.est-no
        AND eb.part-no EQ probeit.part-no:

        FIND FIRST style NO-LOCK
            WHERE style.company EQ eb.company
            AND style.style   EQ eb.style
            NO-ERROR.
        IF AVAILABLE style THEN ctrl2[18] = style.royalty.

        FIND FIRST tt-rel NO-LOCK
            WHERE tt-rel.reftable EQ "ce/com/selwhif1.w"
            AND tt-rel.company  EQ eb.company
            AND tt-rel.loc      EQ eb.est-no
            AND tt-rel.code     EQ STRING(eb.form-no,"9999999999")
            AND tt-rel.code2    EQ STRING(eb.blank-no,"9999999999")
            NO-ERROR.
        IF AVAILABLE tt-rel THEN v-rel = v-rel + tt-rel.val[1].
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

    v-pct[5] = v-pct[2].

    board-cst = 0.
    FOR EACH blk WHERE blk.id EQ probeit.part-no,
        FIRST ef FIELDS(form-no) NO-LOCK
        WHERE ef.company EQ xest.company
        AND ef.est-no  EQ xest.est-no
        AND ef.form-no EQ blk.snum,
        EACH brd WHERE brd.form-no EQ ef.form-no:

        board-cst = board-cst + (brd.cost-m * blk.pct * (t-blkqty[ef.form-no] / 1000)).
    END.
    
    /*Note - calculation of board specific cost differs from Corrugated since there is not total Board Cost reftable*/
    /*This is specific to the particular item*/
    dBoardCst = 0.
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

    dBoardCst = dBoardCst / (v-qty / 1000).
    
    IF probeit.fact-cost GT 0 THEN 
        dBoardPct = dBoardCst / probeit.fact-cost * 100.
 
    
    /*eb is not available at this point and is needed for markup matrix lookup - find the non-set header blank*/       
    IF NOT AVAILABLE eb THEN 
        FIND FIRST eb NO-LOCK
            WHERE eb.company EQ probeit.company
            AND eb.est-no EQ probeit.est-no
            AND eb.part-no EQ probeit.part-no
            AND eb.form-no GT 0
            NO-ERROR.
    
    RUN custom/markup.p (ROWID(eb),
        dBoardCst,
        probeit.fact-cost * (v-qty / 1000),
        probeit.full-cost * (v-qty / 1000),
        dBoardPct,
        INPUT-OUTPUT lv-sell-by,
        INPUT-OUTPUT v-pct[3]).
   
    /*  IF lv-sell-by-ce-ctrl NE "B" AND                                                      */
    /*     lv-sell-by EQ "B" THEN DO:                                                         */
    /*     board-cst = 0.                                                                     */
    /*                                                                                        */
    /*     FOR EACH blk WHERE blk.id EQ probeit.part-no,                                      */
    /*         FIRST ef FIELDS(form-no) NO-LOCK                                               */
    /*         WHERE ef.company EQ xest.company                                               */
    /*           AND ef.est-no  EQ xest.est-no                                                */
    /*           AND ef.form-no EQ blk.snum,                                                  */
    /*         EACH brd WHERE brd.form-no EQ ef.form-no:                                      */
    /*                                                                                        */
    /*         board-cst = board-cst + (brd.cost-m * blk.pct * (t-blkqty[ef.form-no] / 1000)).*/
    /*     END.                                                                               */
    /*  END.                                                                                  */

    v-pct[4] = v-pct[1] + v-markup.  /* from sys/inc/ceprice.i */

    IF v-pct[3] NE 0 THEN v-pct[4] = v-pct[3].

    IF cerunf EQ "Dee" THEN
        v-pct[4] = v-pct[1] + (ctrl[1] * 100) + ctrl2[18] + v-pct[5].
    ELSE 
    DO:
        IF v-pct[3] EQ 0 AND v-pct[5] NE 0 THEN v-pct[4] = v-pct[5] + v-markup.
        v-pct[5] = 0.
    END.

    IF ll-use-margin THEN v-pct[4] = probe.market-price.

    ASSIGN 
        dMCostToExcludeMisc = dMCostToExcludeMisc / (v-qty / 1000)
        dMCostToExcludePrep = dMCostToExcludePrep / (v-qty / 1000)
        dMPriceToAddMisc = dMPriceToAddMisc / (v-qty / 1000)
        dMPriceToAddPrep = dMPriceToAddPrep / (v-qty / 1000)
        .
    dMarginCostG = IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN board-cst ELSE probeit.fact-cost.
    dMarginCostN = IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN 0 ELSE (probeit.full-cost - probeit.fact-cost).
    
    /*Exclude SIMON = M Costs from Price Margin Calculation*/
    dMarginCostG = dMarginCostG - dMCostToExcludeMisc - dMCostToExcludePrep.

    RUN est/CalcSellPrice.p (lv-sell-by-ce-ctrl,
        lv-sell-by,
        v-basis,
        dMarginCostG,
        dMarginCostN,
        (IF ll-use-margin OR
        (lv-sell-by-ce-ctrl NE "B" AND
        lv-sell-by EQ "B") THEN 0
        ELSE probe.comm),
        v-pct[4],
        dMPriceToAddMisc + dMPriceToAddPrep,
        OUTPUT probeit.sell-price,
        OUTPUT v-comm).

    ASSIGN 
        dMCostToExcludeMisc = 0
        dMCostToExcludePrep = 0
        dMPriceToAddMisc = 0
        dMPriceToAddPrep = 0
        .

    IF ll-use-margin OR
        (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN
        v-comm = probeit.sell-price * probe.comm / 100.
      
    ASSIGN
        v-price = v-price + (probeit.sell-price * (v-qty / 1000))
        v-nman  = v-comm.

    IF cerunf EQ "Dee" THEN
        ASSIGN
            v-royl = probeit.sell-price * ctrl2[18] / 100
            v-ware = probeit.sell-price * ctrl[1]
            v-cust = probeit.sell-price * v-pct[5] / 100
            v-nman = v-nman + v-royl + v-ware + v-cust.
    ELSE
        ASSIGN
            v-royl = 0
            v-ware = 0
            v-cust = 0.

    ASSIGN
        probeit.full-cost  = probeit.full-cost + v-nman
        probeit.sell-price = probeit.sell-price / (1 - (v-match-up / 100))
        b-probemk.val[2]   = probe.comm
        b-probemk.val[3]   = ctrl2[18]
        b-probemk.val[4]   = ctrl[1] * 100
        b-probemk.val[5]   = v-pct[5]
        b-probemk.val[6]   = v-comm * (v-qty / 1000)
        b-probemk.val[7]   = v-royl * (v-qty / 1000)
        b-probemk.val[8]   = v-ware * (v-qty / 1000)
        b-probemk.val[9]   = v-cust * (v-qty / 1000)
        probe-ref.val[6]   = probe-ref.val[6] + b-probemk.val[6]
        probe-ref.val[7]   = probe-ref.val[7] + b-probemk.val[7]
        probe-ref.val[8]   = probe-ref.val[8] + b-probemk.val[8]
        probe-ref.val[9]   = probe-ref.val[9] + b-probemk.val[9].
END.

ASSIGN
    probe-ref.val[2]  = probe-ref.val[6] / v-price * 10000000
    probe-ref.val[3]  = probe-ref.val[7] / v-price * 10000000
    probe-ref.val[4]  = probe-ref.val[8] / v-price * 10000000
    probe-ref.val[5]  = probe-ref.val[9] / v-price * 10000000
    probe-ref.val[10] = fac-tot2 - fac-tot.
