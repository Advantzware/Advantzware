/* --------------------------------------------------- cec/probemk.p 4/93 cd  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER v-rowid AS ROWID NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

DEFINE SHARED VARIABLE qty AS INTEGER NO-UNDO .

DEFINE SHARED BUFFER xest FOR est.
DEFINE SHARED BUFFER xef  FOR ef.
DEFINE SHARED BUFFER xeb  FOR eb.

{cec/print4.i shared shared}
{cec/print42.i shared}

DEFINE BUFFER probe-ref   FOR reftable.
DEFINE BUFFER probe-board FOR reftable.

DEFINE SHARED VARIABLE v-prep-mat         LIKE tprep-mat NO-UNDO.  /* for probemk cost */
DEFINE SHARED VARIABLE v-prep-lab         LIKE tprep-lab NO-UNDO.

DEFINE        VARIABLE v-comm             LIKE tt-tot NO-UNDO.
DEFINE        VARIABLE v-price            LIKE tt-tot NO-UNDO.
DEFINE        VARIABLE v-gsh-qty          AS DECIMAL  NO-UNDO.
DEFINE        VARIABLE v-msf              AS DECIMAL  NO-UNDO.
DEFINE        VARIABLE qm                 AS de   NO-UNDO.
DEFINE        VARIABLE v-prf-s            AS DECIMAL  NO-UNDO.
DEFINE        VARIABLE v-pct-s            AS DECIMAL  NO-UNDO.
DEFINE        VARIABLE lv-sell-by         AS CHARACTER NO-UNDO.
DEFINE        VARIABLE lv-sell-by-ce-ctrl AS CHARACTER NO-UNDO.
DEFINE        VARIABLE ll-use-margin      AS LOG  NO-UNDO.
DEFINE        VARIABLE board-cst          AS DECIMAL  NO-UNDO.
DEFINE        VARIABLE cerunc-dec         AS DECIMAL  NO-UNDO.
DEFINE        VARIABLE vn-out             AS INTEGER  NO-UNDO.
DEFINE        VARIABLE v-max-sheets       AS INTEGER  NO-UNDO.
DEFINE        VARIABLE v-sheets           AS INTEGER  NO-UNDO.
DEFINE        VARIABLE dBoardPct          AS DECIMAL  NO-UNDO.
DEFINE        VARIABLE dMarginCostG       AS DECIMAL  NO-UNDO.
DEFINE        VARIABLE dMarginCostN       AS DECIMAL  NO-UNDO.

DO TRANSACTION:
    {sys/inc/cerun.i C}

    cerunc-dec = sys-ctrl.dec-fld.

    {cec/msfcalc.i}
    {sys/inc/ceround.i}
    {cec/rollfac.i}
    {cec/combasis.i}
END.

FIND probe WHERE ROWID(probe) EQ v-rowid NO-LOCK.

qm = qty / 1000 * v-sqft-fac.

FIND FIRST est-op
    WHERE est-op.company = xest.company
    AND est-op.est-no EQ xest.est-no
    AND est-op.s-num EQ xef.form-no
    AND est-op.line  GE 500
    NO-LOCK NO-ERROR.

RUN est/ef-#out.p (ROWID(xef), OUTPUT vn-out).

v-sheets = 0.
IF xef.est-type NE 6 THEN
    v-gsh-qty = xef.gsh-qty.
ELSE 
DO:
    v-max-sheets = 0.
    FOR EACH eb WHERE eb.company = xeb.company
        AND eb.est-no  = xeb.est-no
        AND eb.form-no EQ xef.form-no
        NO-LOCK.

        v-sheets =  probe.est-qty * (IF eb.yld-qty GT 0 THEN eb.yld-qty ELSE 
            IF eb.yld-qty EQ 0 THEN 1 ELSE (-1 / eb.yld-qty)) / eb.num-up / vn-out.
        IF v-sheets GT v-max-sheets THEN
            v-max-sheets = v-sheets.

    END.
    IF v-max-sheets GT 0 THEN
        v-gsh-qty = v-max-sheets.
END.
IF v-gsh-qty = 0 THEN
    v-gsh-qty = xef.gsh-qty.
   
    {sys/inc/roundup.i v-gsh-qty}

ASSIGN
    lv-sell-by         = ce-ctrl.sell-by
    lv-sell-by-ce-ctrl = ce-ctrl.sell-by
    v-msf              = IF v-corr THEN
                ROUND(((xef.gsh-len * xef.gsh-wid) * .007) * v-gsh-qty,0)
              ELSE
                ROUND(((xef.gsh-len * xef.gsh-wid) / 144) * v-gsh-qty,0).        

IF lv-sell-by EQ "S" THEN 
DO:
{cec/sqftmrkp.i "v-msf / 1000" v-pct}
END.

FIND FIRST probe-board 
    WHERE probe-board.reftable EQ "probe.board"
    AND probe-board.company  EQ probe.company
    AND probe-board.loc      EQ ""
    AND probe-board.code     EQ probe.est-no
    AND probe-board.code2    EQ STRING(probe.line,"9999999999")
    NO-ERROR.
IF AVAILABLE probe-board THEN DO:
     board-cst = probe-board.val[1].
     /*For ticket 19263 - storage of total man hours op-tot[8] = tot MR man hours, op-tot[9] = tot Run man hours*/ 
     probe-board.val[6] = op-tot[8] + op-tot[9].
     FIND CURRENT probe-board NO-LOCK.
END.
IF ord-cost GT 0 AND qm GT 0 THEN 
    dBoardPct = board-cst / ord-cost * 100.
    
RUN custom/markup.p (ROWID(xeb),
    board-cst,
    ord-cost,
    tt-tot,
    dBoardPct,
    INPUT-OUTPUT lv-sell-by,
    INPUT-OUTPUT v-pct).


IF cerunc EQ "Fibre" THEN
    RUN est/usemargin.p (ROWID(xest), OUTPUT ll-use-margin).

IF ll-use-margin THEN
    RUN est/getsmanmtrx.p (ROWID(xest), "M",
        INPUT-OUTPUT v-com,
        INPUT-OUTPUT v-pct).

dMarginCostG = IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN board-cst ELSE ord-cost.
dMarginCostN = IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN 0 ELSE tt-tot - ord-cost.

/*Exclude SIMON = M Costs from Price Margin Calculation*/
dMarginCostG = dMarginCostG - dMCostToExcludeMisc - dMCostToExcludePrep.

RUN custom/CalcSellPrice.p (lv-sell-by-ce-ctrl,
    lv-sell-by,
    v-basis,
    dMarginCostG,
    dMarginCostN,
    (IF ll-use-margin OR
    (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN 0 ELSE v-com),
    v-pct,
    dMPriceToAddMisc + dMPriceToAddPrep,
    OUTPUT v-price,
    OUTPUT v-comm).

 
ASSIGN 
    dMCostToExcludeMisc = 0
    dMCostToExcludePrep = 0
    dMPriceToAddMisc = 0
    dMPriceToAddPrep = 0
    .
    
IF ll-use-margin OR
    (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN v-comm = v-price * v-com / 100.

tt-tot = tt-tot + v-comm.

IF OPSYS EQ "unix" THEN
    UNIX SILENT COPY VALUE(outfile1) VALUE(outfile3).
ELSE
    DOS SILENT COPY VALUE(outfile1) VALUE(outfile3).

OUTPUT to value(outfile3) append.

CREATE blk.
ASSIGN
    blk.kli     = xeb.cust-no
    blk.id      = xeb.part-no
    blk.bnum    = 1
    blk.qreq    = qty / xeb.yld-qty
    blk.fact    = ord-cost / (qty / 1000)
    blk.cost    = tt-tot / (qty / 1000)
    blk.fg-wt   = fg-wt
    blk.fg-wt$  = tt-tot
    blk.dscr    = xeb.part-dscr1
    blk.pur-man = xeb.pur-man.

blk.sell = (IF ce-ctrl.sell-by = "G" THEN blk.fact ELSE blk.cost)
    / (1 - (v-pct / 100)).

IF vprint THEN 
DO:
    FIND CURRENT probe.

    ASSIGN
        probe.freight       = rels[k]             /* Holds Number of Releases */
        mku_gsa-l           = ce-ctrl.rm-rate
        mku_gsa-m           = ce-ctrl.fg-rate
        mku_com             = ce-ctrl.comm-mrkup
        mku_whs             = ce-ctrl.whse-mrkup
        probe.est-qty       = qty / xeb.yld-qty
        probe.fact-cost     = ord-cost / qm
        probe.full-cost     = tt-tot / qm
        probe.sell-price    = v-price / qm
        probe.sell-price-wo = (v-price - v-comm) / qm
        probe.comm          = v-com 
        probe.prof-on       = "Net"
        probe.gsh-qty       = v-gsh-qty
        probe.tot-lbs       = v-msf                    
        probe.dscr          = xeb.part-dscr1
        probe.bsf           = (IF v-corr THEN (xeb.t-sqin * .007) ELSE (xeb.t-sqin / 144))
                          / v-sqft-fac
        probe.mat-cost      = (dm-tot[5] + mis-tot[1] + v-prep-mat) / (qtty[k] / 1000)
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
        probe.market-price = v-pct
        v-pct              = probe.net-profit
        v-prf-s            = probe.sell-price - probe.fact-cost
        v-pct-s            = ROUND(v-prf-s / probe.fact-cost * 100,2).

    IF NOT vmclean THEN 
    DO:

        IF cerunc-dec EQ 0 THEN
        DO:
            PUT "Commission on " +
                (IF v-basis EQ "G" THEN "GM"
                ELSE "SP")                      FORMAT "x(19)"
                STRING(v-com,">>9.99%") TO 30
                v-comm / qm TO 48
                v-comm      TO 80 SKIP.
      
            PUT "FULL COST" tt-tot / qm      TO 48
                tt-tot FORMAT ">>>>,>>9.99"  TO 80 SKIP.
      
            IF v-rollfac THEN
                PUT "FULL COST PER ROLL" tt-tot / (qty / xeb.yld-qty) TO 48.
      
            IF ce-ctrl.sell-by EQ "S" THEN
                PUT "Markup on Fact Cost"         FORMAT "x(19)"
                    STRING(v-pct-s,"->>9.99%")    TO 30
                    v-prf-s                       TO 48
                    v-prf-s * qm                  TO 80
                    SKIP.
      
            ELSE
                PUT "Net Profit "                         FORMAT "x(19)"
                    STRING(v-pct,"->>9.99%")              TO 30
                    probe.sell-price * (v-pct / 100)      TO 48
                    probe.sell-price * (v-pct / 100) * qm TO 80
                    SKIP.
      
            PUT "SELLING PRICE"
                probe.sell-price      TO 48
                probe.sell-price * qm TO 80
                SKIP(1).
      
            IF v-rollfac THEN
                PUT "SELLING PRICE PER ROLL"
                    probe.sell-price * qm / (qty / xeb.yld-qty) TO 48.
        END.
        ELSE
        DO:
            PUT "Commission on " +
                (IF v-basis EQ "G" THEN "GM"
                ELSE "SP")                      FORMAT "x(19)"
                STRING(v-com,">>9.99%") TO 30
                v-comm / qm FORMAT ">,>>>,>>9.99" TO 48
                v-comm       FORMAT ">,>>>,>>9.99" TO 80 SKIP.
      
            PUT "FULL COST" tt-tot / qm FORMAT ">,>>>,>>9.99"     TO 48
                tt-tot FORMAT ">,>>>,>>9.99"  TO 80 SKIP.
      
            IF v-rollfac THEN
                PUT "FULL COST PER ROLL" tt-tot / (qty / xeb.yld-qty) FORMAT ">,>>>,>>9.99" TO 48.
      
            IF ce-ctrl.sell-by EQ "S" THEN
                PUT "Markup on Fact Cost"         FORMAT "x(19)"
                    STRING(v-pct-s,"->>9.99%")    TO 30
                    v-prf-s FORMAT ">,>>>,>>9.99" TO 48
                    v-prf-s * qm FORMAT ">,>>>,>>9.99" TO 80
                    SKIP.
      
            ELSE
                PUT "Net Profit "                         FORMAT "x(19)"
                    STRING(v-pct,"->>9.99%")              TO 30
                    probe.sell-price * (v-pct / 100) FORMAT ">,>>>,>>9.99" TO 48
                    probe.sell-price * (v-pct / 100) * qm FORMAT ">,>>>,>>9.99" TO 80
                    SKIP.
      
            PUT "SELLING PRICE"
                probe.sell-price FORMAT ">,>>>,>>>"  TO 48
                probe.sell-price * qm FORMAT ">,>>>,>>>" TO 80
                SKIP(1).
      
            IF v-rollfac THEN
                PUT "SELLING PRICE PER ROLL"
                    probe.sell-price * qm / (qty / xeb.yld-qty) FORMAT ">,>>>,>>>" TO 48.
        END.
    END.

    ELSE 
    DO:
        ASSIGN
            vmcl-desc = "Commission on " + (IF v-basis EQ "G" THEN "GM" ELSE "SP")
            vmcl-cost = v-comm / qm.
        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100000}
        mclean.rec-type = "commission".

        ASSIGN
            vmcl-desc = "    Commission %"
            vmcl-cost = v-com.
        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100001}
        mclean.rec-type = "commission".

        ASSIGN
            vmcl-desc = "FULL COST"
            vmcl-cost = tt-tot / qm.
        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100002}

        IF v-rollfac THEN 
        DO:
            ASSIGN
                vmcl-desc = "FULL COST PER ROLL"
                vmcl-cost = tt-tot / (qty / xeb.yld-qty).
            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100003}
        END.

        IF ce-ctrl.sell-by EQ "S" THEN 
        DO:
            ASSIGN
                vmcl-desc = "Margin on Fact Cost"
                vmcl-cost = v-prf-s.                
            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100004}
            mclean.rec-type = "profit-s".

            ASSIGN
                vmcl-desc = "    Fact Margin %"
                vmcl-cost = v-pct-s.
            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100005}
            mclean.rec-type = "profit-s".
        END.

        ELSE 
        DO:
            ASSIGN
                vmcl-desc = "Net Margin"
                vmcl-cost = probe.sell-price * (v-pct / 100).                
            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100006}
            mclean.rec-type = "profit".

            ASSIGN
                vmcl-desc = "    Net Margin %"
                vmcl-cost = v-pct.
            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100007}
            mclean.rec-type = "profit".
        END.

        IF cerunc EQ "Fibre" THEN 
        DO:
            ASSIGN
                vmcl-desc = "Available Margin"
                vmcl-cost = probe.sell-price * probe.market-price / 100.
            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100008}
            mclean.rec-type = "avail margin".

            ASSIGN
                vmcl-desc = "    Available Margin %"
                vmcl-cost = probe.market-price.
            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100009}
            mclean.rec-type = "avail margin".
        END.

        ASSIGN
            vmcl-desc = "SELLING PRICE"
            vmcl-cost = probe.sell-price.
        {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100010}

        IF v-rollfac THEN 
        DO:
            ASSIGN
                vmcl-desc = "SELLING PRICE PER ROLL"
                vmcl-cost = probe.sell-price * qm / (qty / xeb.yld-qty).
            {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100011}
        END.
    END.
END.

OUTPUT close.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
