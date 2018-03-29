/* ---------------------------------------------------- ce/probemk.p 4/93 cd  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER v-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DEFINE SHARED VARIABLE qty AS INTEGER NO-UNDO .

DEFINE SHARED BUFFER xest FOR est.
DEFINE SHARED BUFFER xef  FOR ef.
DEFINE SHARED BUFFER xeb  FOR eb.

{ce/print4.i SHARED SHARED}

DEFINE BUFFER probe-ref FOR reftable.

DEFINE SHARED VARIABLE v-prep-mat         LIKE tprep-mat NO-UNDO.  /* for probemk cost */
DEFINE SHARED VARIABLE v-prep-lab         LIKE tprep-lab NO-UNDO.

DEFINE        VARIABLE v-comm             LIKE tt-tot NO-UNDO.
DEFINE        VARIABLE v-royl             LIKE tt-tot NO-UNDO.
DEFINE        VARIABLE v-ware             LIKE tt-tot NO-UNDO.
DEFINE        VARIABLE v-cust             LIKE tt-tot NO-UNDO.
DEFINE        VARIABLE v-nman             LIKE tt-tot NO-UNDO.
DEFINE        VARIABLE v-price            LIKE tt-tot NO-UNDO.
DEFINE        VARIABLE v-gsh-qty          AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-msf              AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE qm                 AS de        NO-UNDO.
DEFINE        VARIABLE v-prf-s            AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-pct-s            AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE lv-sell-by         AS CHARACTER NO-UNDO.
DEFINE        VARIABLE lv-sell-by-ce-ctrl AS CHARACTER NO-UNDO.
DEFINE        VARIABLE v-basis            LIKE sman.commbasis INIT "" NO-UNDO.
DEFINE        VARIABLE v-com              LIKE eb.comm INIT 0 NO-UNDO.
DEFINE        VARIABLE v-pct              LIKE eb.comm INIT 0 EXTENT 3 NO-UNDO.
DEFINE        VARIABLE ll-use-margin      AS LOG       NO-UNDO.
DEFINE        VARIABLE board-cst          AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE dBoardPct          AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE dMarginCostG       AS DECIMAL  NO-UNDO.
DEFINE        VARIABLE dMarginCostN       AS DECIMAL  NO-UNDO.



DO TRANSACTION:
    {sys/inc/cerun.i F}
    {sys/inc/msfcalc.i}
    {sys/inc/ceround.i}
    {sys/inc/ceprice.i}
    {sys/inc/cecomm.i}
END.

FIND probe WHERE ROWID(probe) EQ v-rowid NO-LOCK NO-ERROR.

IF cecomm-log THEN v-com = xeb.comm.

FIND FIRST ce-ctrl NO-LOCK
    WHERE ce-ctrl.company EQ xeb.company
    AND ce-ctrl.loc     EQ xeb.loc
    NO-ERROR.

IF NOT AVAILABLE ce-ctrl THEN
    FIND FIRST ce-ctrl NO-LOCK
        WHERE ce-ctrl.company    EQ cocode
        AND ce-ctrl.prof-mrkup NE 0
        NO-ERROR.
IF AVAILABLE ce-ctrl THEN v-pct[1] = ce-ctrl.prof-mrkup.

FIND FIRST cust NO-LOCK
    WHERE cust.company EQ xeb.company
    AND cust.cust-no EQ xeb.cust-no
    NO-ERROR.
IF AVAILABLE cust THEN 
DO:
    RUN custom/combasis.p (cocode, xeb.sman, cust.type, xeb.procat, 0, cust.cust-no,
        OUTPUT v-basis).

    IF cust.markup NE 0 THEN v-pct[2] = cust.markup.
END.

ASSIGN
    qm     = qty / 1000
    v-hopf = IF cerunf EQ "HOP" THEN xef.n-out ELSE 1.

FIND FIRST est-op
    WHERE est-op.company EQ xest.company
    AND est-op.est-no  EQ xest.est-no
    AND est-op.s-num   EQ xef.form-no
    AND est-op.line    GE 500
    NO-LOCK NO-ERROR.

ASSIGN
    lv-sell-by         = ce-ctrl.sell-by
    lv-sell-by-ce-ctrl = ce-ctrl.sell-by
    v-gsh-qty          = xef.gsh-qty.

{sys/inc/roundup.i v-gsh-qty}

board-cst = 0.
     
FOR EACH brd WHERE brd.form-no EQ xef.form-no:
    board-cst = board-cst + (brd.cost-m * qty / 1000).
END.

board-cst = board-cst / (qty / 1000).
IF ord-cost GT 0 AND qm GT 0 THEN 
    dBoardPct = board-cst / ord-cost * 100.
    
RUN custom/markup.p (ROWID(xeb),
    board-cst,
    fac-tot,
    tt-tot,
    dBoardPct,
    INPUT-OUTPUT lv-sell-by,
    INPUT-OUTPUT v-pct[3]).

v-pct[1] = v-pct[1] + v-markup.  /* from sys/inc/ceprice.i */

/*IF lv-sell-by-ce-ctrl NE "B" AND                         */
/*   lv-sell-by EQ "B" THEN DO:                            */
/*   board-cst = 0.                                        */
/*                                                         */
/*   FOR EACH brd WHERE brd.form-no EQ xef.form-no:        */
/*       board-cst = board-cst + (brd.cost-m * qty / 1000).*/
/*   END.                                                  */
/*                                                         */
/*   board-cst = board-cst / (qty / 1000).                 */
/*END.                                                     */

IF v-pct[3] NE 0 THEN v-pct[1] = v-pct[3].

IF cerunf EQ "Dee" THEN
    v-pct[1] = v-pct[1] + (ctrl[1] * 100) + ctrl2[18] + v-pct[2].
ELSE 
DO:
    IF v-pct[3] EQ 0 AND v-pct[2] NE 0 THEN v-pct[1] = v-pct[2] + v-markup.
    v-pct[2] = 0.
END.

IF cerunf EQ "Fibre" THEN
    RUN est/usemargin.p (ROWID(xest), OUTPUT ll-use-margin).

IF ll-use-margin THEN
    RUN est/getsmanmtrx.p (ROWID(xest), "M",
        INPUT-OUTPUT v-com,
        INPUT-OUTPUT v-pct[1]).
dMarginCostG = IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN board-cst ELSE fac-tot.
dMarginCostN = IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN 0 ELSE tt-tot - fac-tot.

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
    ELSE v-com),
    v-pct[1],
    dMPriceToAddMisc + dMPriceToAddPrep,
    OUTPUT v-price,
    OUTPUT v-comm).

 v-price = v-price + dMPriceToAddMisc + dMPriceToAddPrep.

ASSIGN 
    dMCostToExcludeMisc = 0
    dMCostToExcludePrep = 0
    dMPriceToAddMisc = 0
    dMPriceToAddPrep = 0
    .
    
IF ll-use-margin OR
    (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B")
    THEN v-comm = v-price * v-com / 100.

v-nman = v-comm.

IF cerunf EQ "Dee" THEN
    ASSIGN
        v-royl = v-price * ctrl2[18] / 100
        v-ware = v-price * ctrl[1]
        v-cust = v-price * v-pct[2] / 100
        v-nman = v-nman + v-royl + v-ware + v-cust.

tt-tot = tt-tot + v-nman.
  
os-copy VALUE(outfile1) VALUE(outfile3).

OUTPUT TO VALUE(outfile3) APPEND.

CREATE blk.
ASSIGN
    blk.kli     = xeb.cust-no
    blk.id      = xeb.part-no
    blk.bnum    = 1
    blk.qreq    = qty / xeb.yld-qty
    blk.fact    = fac-tot / (qty / 1000)
    blk.cost    = tt-tot / (qty / 1000)
    blk.fg-wt   = fg-wt
    blk.fg-wt$  = tt-tot
    blk.dscr    = xeb.part-dscr1
    blk.pur-man = xeb.pur-man.

blk.sell = (IF ce-ctrl.sell-by EQ "G" THEN blk.fact ELSE blk.cost)
    / (1 - (v-pct[1] / 100)).

IF vprint THEN 
DO TRANSACTION:
    FIND CURRENT probe.

    ASSIGN
        probe.freight       = rels[k]             /* Holds Number of Releases */
        mku_gsa-l           = ce-ctrl.rm-rate
        mku_gsa-m           = ce-ctrl.fg-rate
        mku_com             = ce-ctrl.comm-mrkup
        mku_whs             = ce-ctrl.whse-mrkup
        probe.est-qty       = qty
        probe.fact-cost     = fac-tot / qm
        probe.full-cost     = tt-tot / qm
        probe.sell-price    = v-price / qm
        probe.sell-price-wo = (v-price - v-nman) / qm
        probe.comm          = v-com   
        probe.prof-on       = "Net"
        probe.gsh-qty       = v-gsh-qty * v-hopf
        probe.tot-lbs       = fg-wt                    
        probe.dscr          = xeb.part-dscr1
        probe.bsf           = IF v-corr THEN (xeb.t-sqin * .007) ELSE (xeb.t-sqin / 144)
        probe.mat-cost      = (dm-tot[5] + mis-tot[1] + v-prep-mat) / (qtty[k] / 1000)
        probe.spare-dec-1   = dm-tot[5] / (qtty[k] / 1000)  /*24941 - Mat % Ex Mis and Prep*/
        probe.lab-cost      = (opsplit$[1] + mis-tot[3] + v-prep-lab + ctrl2[2] + ctrl2[3]) /
                         (qtty[k] / 1000)
        probe.vo-cost       = opsplit$[2] / (qtty[k] / 1000)
        probe.fo-cost       = opsplit$[3] / (qtty[k] / 1000).

    IF INDEX("BSG",ce-ctrl.sell-by) GT 0 THEN
        probe.prof-on = ENTRY(INDEX("BSG",ce-ctrl.sell-by),"Cust,MSF,Gross").

    IF v-round NE "PENNY" THEN 
    DO:
        IF v-round EQ "DOLLAR" THEN 
        DO:
        {sys/inc/roundup.i probe.sell-price}
        END.
    END.

    ASSIGN
        probe.net-profit   = ROUND((1 - (probe.full-cost / probe.sell-price)) * 100,2)
        probe.gross-profit = ROUND((1 - (probe.fact-cost / probe.sell-price)) * 100,2)
        probe.market-price = v-pct[1]        /* Actually Margin % */
        v-prf-s            = probe.sell-price - probe.fact-cost
        v-pct-s            = ROUND(v-prf-s / probe.fact-cost * 100,2).
      
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
        probe-ref.val[2]  = v-comm / v-price * 10000000
        probe-ref.val[3]  = v-royl / v-price * 10000000
        probe-ref.val[4]  = v-ware / v-price * 10000000
        probe-ref.val[5]  = v-cust / v-price * 10000000
        probe-ref.val[6]  = v-comm
        probe-ref.val[7]  = v-royl
        probe-ref.val[8]  = v-ware
        probe-ref.val[9]  = v-cust
        probe-ref.val[10] = fac-tot2 - fac-tot.

    IF NOT vmclean THEN 
    DO:
        IF cerunf EQ "Dee" THEN
            PUT "Royalty"                         FORMAT "x(19)"
                STRING(ctrl2[18],">>9.99%")       TO 30
                v-royl / qm                       TO 48
                v-royl                            TO 80 SKIP

                "Warehouse Markup"                FORMAT "x(19)"
                STRING(ctrl[1] * 100,">>9.99%")   TO 30
                v-ware / qm                       TO 48
                v-ware                            TO 80 SKIP

                "Customer Markup"                 FORMAT "x(19)"
                STRING(v-pct[2],">>9.99%")        TO 30
                v-cust / qm                       TO 48
                v-cust                            TO 80 SKIP

                "Commission"                      FORMAT "x(19)"
                STRING(v-com,">>9.99%")           TO 30
                v-comm / qm                       TO 48
                v-comm                            TO 80 SKIP

                "NON MANF. COST"                  FORMAT "x(19)"
                v-nman / qm                       TO 48
                v-nman                            TO 80 SKIP.
        ELSE
            PUT "Commission on " +
                (IF v-basis EQ "G" THEN "GM"
                ELSE "SP")     FORMAT "x(19)"
                STRING(v-com,">>9.99%") TO 30
                v-comm / qm TO 48
                v-comm      TO 80 SKIP.

        PUT "FULL COST" tt-tot / qm      TO 48
            tt-tot FORMAT ">>>>,>>9.99"  TO 80 SKIP.

        /*if v-rollfac then
          put "FULL COST PER ROLL" tt-tot / (qty / xeb.yld-qty) to 48.*/

        IF ce-ctrl.sell-by EQ "S" THEN
            PUT "Markup on Fact Cost"         FORMAT "x(19)"
                STRING(v-pct-s,"->>9.99%")    TO 29
                v-prf-s                       TO 48 FORMAT "->>,>>9.99"
                v-prf-s * qm                  TO 80 FORMAT "->>>,>>9.99"
                SKIP.

        ELSE
            PUT "Net Profit "                         FORMAT "x(19)"
                STRING(v-pct[1],"->>9.99%")              TO 29
                probe.sell-price * (v-pct[1] / 100)      TO 48 FORMAT "->>,>>9.99"
                probe.sell-price * (v-pct[1] / 100) * qm TO 80 FORMAT "->>>,>>9.99"
                SKIP.

        IF cerunf EQ "Fibre" THEN
            PUT "Available Margin"                   FORMAT "x(19)"
                STRING(probe.market-price,"->>9.99%") TO 29
                probe.sell-price * (probe.market-price / 100)      TO 48 FORMAT "->>,>>9.99"
                probe.sell-price * (probe.market-price / 100) * qm TO 80 FORMAT "->>>,>>9.99"
                SKIP.

        PUT "SELLING PRICE"
            probe.sell-price      TO 48 FORMAT "->>,>>9.99"
            probe.sell-price * qm TO 80 FORMAT "->>>,>>9.99"
            SKIP(1).

    /*if v-rollfac then
      put "SELLING PRICE PER ROLL"
          probe.sell-price * qm / (qty / xeb.yld-qty) to 48.*/
    END.

    ELSE 
    DO:
        ASSIGN
            vmcl-desc = "Commission on " + (IF v-basis EQ "G" THEN "GM" ELSE "SP")
            vmcl-cost = v-comm / qm.
        {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
        mclean.rec-type = "commission".

        ASSIGN
            vmcl-desc = "    Commission %"
            vmcl-cost = v-com.
    {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
        mclean.rec-type = "commission".

        ASSIGN
            vmcl-desc = "FULL COST"
            vmcl-cost = tt-tot / qm.
    {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

        /*if v-rollfac then do:
          assign
           vmcl-desc = "FULL COST PER ROLL"
           vmcl-cost = tt-tot / (qty / xeb.yld-qty).
          {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
        end.*/

        IF ce-ctrl.sell-by EQ "S" THEN 
        DO:
            ASSIGN
                vmcl-desc = "Margin on Fact Cost"
                vmcl-cost = v-prf-s.                
       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
            mclean.rec-type = "profit-s".

            ASSIGN
                vmcl-desc = "    Fact Margin %"
                vmcl-cost = v-pct-s.
      {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
            mclean.rec-type = "profit-s".
        END.

        ELSE 
        DO:
            ASSIGN
                vmcl-desc = "Net Margin"
                vmcl-cost = probe.sell-price * (v-pct[1] / 100).                
      {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
            mclean.rec-type = "profit".

            ASSIGN
                vmcl-desc = "    Net Margin %"
                vmcl-cost = v-pct[1].
      {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
            mclean.rec-type = "profit".
        END.

        ASSIGN
            vmcl-desc = "SELLING PRICE"
            vmcl-cost = probe.sell-price.
    {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

    /*if v-rollfac then do:
      assign
       vmcl-desc = "SELLING PRICE PER ROLL"
       vmcl-cost = probe.sell-price * qm / (qty / xeb.yld-qty).
      {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
    end.*/
    END.
END.

OUTPUT CLOSE.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
