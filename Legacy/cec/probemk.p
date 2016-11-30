/* --------------------------------------------------- cec/probemk.p 4/93 cd  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM v-rowid AS ROWID NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared var qty as INT NO-UNDO .

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{cec/print4.i shared shared}
{cec/print42.i shared}

DEF BUFFER probe-ref FOR reftable.
DEF BUFFER probe-board FOR reftable.

def shared var v-prep-mat like tprep-mat no-undo.  /* for probemk cost */
def shared var v-prep-lab like tprep-lab no-undo.

def var v-comm like tt-tot NO-UNDO.
def var v-price like tt-tot NO-UNDO.
def var v-gsh-qty as dec NO-UNDO.
def var v-msf as dec NO-UNDO.
def var qm as de NO-UNDO.
def var v-prf-s as dec NO-UNDO.
def var v-pct-s as dec NO-UNDO.
DEF VAR lv-sell-by AS CHAR NO-UNDO.
DEF VAR lv-sell-by-ce-ctrl AS CHAR NO-UNDO.
DEF VAR ll-use-margin AS LOG NO-UNDO.
DEF VAR board-cst AS DEC NO-UNDO.
DEF VAR cerunc-dec AS DEC NO-UNDO.
DEF VAR vn-out AS INT NO-UNDO.
DEF VAR v-max-sheets AS INT NO-UNDO.
DEF VAR v-sheets AS INT NO-UNDO.

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

find first est-op
    where est-op.company = xest.company
      and est-op.est-no eq xest.est-no
      and est-op.s-num eq xef.form-no
      and est-op.line  ge 500
    no-lock no-error.

RUN est/ef-#out.p (ROWID(xef), OUTPUT vn-out).

v-sheets = 0.
IF xef.est-type NE 6 THEN
  v-gsh-qty = xef.gsh-qty.
ELSE DO:
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

assign
 lv-sell-by = ce-ctrl.sell-by
 lv-sell-by-ce-ctrl = ce-ctrl.sell-by
 v-msf      = if v-corr then
                round(((xef.gsh-len * xef.gsh-wid) * .007) * v-gsh-qty,0)
              else
                round(((xef.gsh-len * xef.gsh-wid) / 144) * v-gsh-qty,0).        

IF lv-sell-by EQ "S" THEN DO:
  {cec/sqftmrkp.i "v-msf / 1000" v-pct}
END.

FIND FIRST probe-board
      WHERE probe-board.reftable EQ "probe.board"
        AND probe-board.company  EQ probe.company
        AND probe-board.loc      EQ ""
        AND probe-board.code     EQ probe.est-no
        AND probe-board.code2    EQ STRING(probe.line,"9999999999")
      NO-ERROR.
  IF AVAIL probe-board THEN board-cst = probe-board.val[1].

RUN custom/markup.p (ROWID(xeb),
                     board-cst,
                     INPUT-OUTPUT lv-sell-by,
                     INPUT-OUTPUT v-pct).

/*IF lv-sell-by-ce-ctrl NE "B" AND                                   */
/*   lv-sell-by EQ "B" THEN DO:                                      */
/*  FIND FIRST probe-board                                           */
/*      WHERE probe-board.reftable EQ "probe.board"                  */
/*        AND probe-board.company  EQ probe.company                  */
/*        AND probe-board.loc      EQ ""                             */
/*        AND probe-board.code     EQ probe.est-no                   */
/*        AND probe-board.code2    EQ STRING(probe.line,"9999999999")*/
/*      NO-ERROR.                                                    */
/*  IF AVAIL probe-board THEN board-cst = probe-board.val[1].        */
/*END.                                                               */

IF cerunc EQ "Fibre" THEN
  RUN est/usemargin.p (ROWID(xest), OUTPUT ll-use-margin).

IF ll-use-margin THEN
  RUN est/getsmanmtrx.p (ROWID(xest), "M",
                         INPUT-OUTPUT v-com,
                         INPUT-OUTPUT v-pct).

RUN custom/sellpric.p (lv-sell-by-ce-ctrl,
                       lv-sell-by,
                       v-basis,
                       (IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN board-cst ELSE ord-cost),
                       (IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN 0 ELSE (tt-tot - ord-cost)),
                       (IF ll-use-margin OR
                           (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN 0 ELSE v-com),
                       v-pct,
                       OUTPUT v-price,
                       OUTPUT v-comm).

IF ll-use-margin OR
   (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN v-comm = v-price * v-com / 100.

tt-tot = tt-tot + v-comm.

if opsys eq "unix" then
  unix silent copy value(outfile1) value(outfile3).
else
  dos silent copy value(outfile1) value(outfile3).

output to value(outfile3) append.

create blk.
assign
 blk.kli         = xeb.cust-no
 blk.id          = xeb.part-no
 blk.bnum        = 1
 blk.qreq        = qty / xeb.yld-qty
 blk.fact        = ord-cost / (qty / 1000)
 blk.cost        = tt-tot / (qty / 1000)
 blk.fg-wt       = fg-wt
 blk.fg-wt$      = tt-tot
 blk.dscr        = xeb.part-dscr1
 blk.pur-man     = xeb.pur-man.

blk.sell = (if ce-ctrl.sell-by = "G" then blk.fact else blk.cost)
           / (1 - (v-pct / 100)).

if vprint then do:
  FIND CURRENT probe.

  assign
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
   probe.bsf           = (if v-corr then (xeb.t-sqin * .007) else (xeb.t-sqin / 144))
                          / v-sqft-fac
   probe.mat-cost      = (dm-tot[5] + mis-tot[1] + v-prep-mat) / (qtty[k] / 1000)
   probe.lab-cost      = (opsplit$[1] + mis-tot[3] + v-prep-lab + ctrl2[2] + ctrl2[3]) /
                         (qtty[k] / 1000)
   probe.vo-cost       = opsplit$[2] / (qtty[k] / 1000)
   probe.fo-cost       = opsplit$[3] / (qtty[k] / 1000).

  IF INDEX("BSG",ce-ctrl.sell-by) GT 0 THEN
    probe.prof-on = ENTRY(INDEX("BSG",ce-ctrl.sell-by),"Cust,MSF,Gross").

  if v-round ne "PENNY" then do:
    if v-round eq "DOLLAR" then do:
      {sys/inc/roundup.i probe.sell-price}
    end.
  end.

  assign
   probe.net-profit   = round((1 - (probe.full-cost / probe.sell-price)) * 100,2)
   probe.gross-profit = round((1 - (probe.fact-cost / probe.sell-price)) * 100,2)
   probe.market-price = v-pct
   v-pct              = probe.net-profit
   v-prf-s            = probe.sell-price - probe.fact-cost
   v-pct-s            = ROUND(v-prf-s / probe.fact-cost * 100,2).

  if not vmclean then do:

    IF cerunc-dec EQ 0 THEN
    DO:
       put "Commission on " +
          (if v-basis eq "G" then "GM"
                              else "SP")                      format "x(19)"
           string(v-com,">>9.99%") to 30
           v-comm / qm to 48
           v-comm      to 80 skip.
      
       put "FULL COST" tt-tot / qm      to 48
           tt-tot format ">>>>,>>9.99"  to 80 skip.
      
       if v-rollfac then
         put "FULL COST PER ROLL" tt-tot / (qty / xeb.yld-qty) to 48.
      
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
      
       put "SELLING PRICE"
           probe.sell-price      to 48
           probe.sell-price * qm to 80
           skip(1).
      
       if v-rollfac then
          put "SELLING PRICE PER ROLL"
              probe.sell-price * qm / (qty / xeb.yld-qty) to 48.
    END.
    ELSE
    DO:
       put "Commission on " +
           (if v-basis eq "G" then "GM"
                              else "SP")                      format "x(19)"
           string(v-com,">>9.99%") to 30
           v-comm / qm format ">,>>>,>>9.99" to 48
           v-comm       format ">,>>>,>>9.99" to 80 skip.
      
       put "FULL COST" tt-tot / qm format ">,>>>,>>9.99"     to 48
           tt-tot format ">,>>>,>>9.99"  to 80 skip.
      
       if v-rollfac then
         put "FULL COST PER ROLL" tt-tot / (qty / xeb.yld-qty) format ">,>>>,>>9.99" to 48.
      
       IF ce-ctrl.sell-by EQ "S" THEN
         PUT "Markup on Fact Cost"         FORMAT "x(19)"
             STRING(v-pct-s,"->>9.99%")    TO 30
             v-prf-s format ">,>>>,>>9.99" TO 48
             v-prf-s * qm format ">,>>>,>>9.99" TO 80
             SKIP.
      
       ELSE
         PUT "Net Profit "                         FORMAT "x(19)"
             STRING(v-pct,"->>9.99%")              TO 30
             probe.sell-price * (v-pct / 100) format ">,>>>,>>9.99" TO 48
             probe.sell-price * (v-pct / 100) * qm format ">,>>>,>>9.99" TO 80
             SKIP.
      
       put "SELLING PRICE"
           probe.sell-price FORMAT ">,>>>,>>>"  to 48
           probe.sell-price * qm FORMAT ">,>>>,>>>" to 80
           skip(1).
      
       if v-rollfac then
         put "SELLING PRICE PER ROLL"
             probe.sell-price * qm / (qty / xeb.yld-qty) FORMAT ">,>>>,>>>" to 48.
    END.
  end.

  else do:
    assign
     vmcl-desc = "Commission on " + (if v-basis eq "G" then "GM" else "SP")
     vmcl-cost = v-comm / qm.
    {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100000}
    mclean.rec-type = "commission".

    assign
     vmcl-desc =  "    Commission %"
     vmcl-cost = v-com.
    {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100001}
    mclean.rec-type = "commission".

    assign
     vmcl-desc = "FULL COST"
     vmcl-cost = tt-tot / qm.
    {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100002}

    if v-rollfac then do:
      assign
       vmcl-desc = "FULL COST PER ROLL"
       vmcl-cost = tt-tot / (qty / xeb.yld-qty).
      {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100003}
    end.

    IF ce-ctrl.sell-by EQ "S" THEN DO:
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

    ELSE DO:
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

    IF cerunc EQ "Fibre" THEN DO:
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

    assign
     vmcl-desc = "SELLING PRICE"
     vmcl-cost = probe.sell-price.
    {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100010}

    if v-rollfac then do:
      assign
       vmcl-desc = "SELLING PRICE PER ROLL"
       vmcl-cost = probe.sell-price * qm / (qty / xeb.yld-qty).
      {cec/pr4-mcln.i vmcl-desc vmcl vmcl-cost 100011}
    end.
  end.
end.

output close.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
