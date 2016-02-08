/* ----------------------------------------------- ce/tan/probemk.p 06/96 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM v-rowid AS ROWID NO-UNDO.

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def BUFFER xeb2 for eb.

{ce/print4.i shared shared}

def shared var v-prep-mat like tprep-mat NO-UNDO.
def shared var v-prep-lab like tprep-lab NO-UNDO.

def var v-dec as dec NO-UNDO.
def var qm    as de NO-UNDO.
def var v-net as dec NO-UNDO.
DEF SHARED VAR qty AS INT NO-UNDO.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

{sys/inc/cerun.i F}

{sys/inc/msfcalc.i}

assign
     qm     = qty / 1000
     v-hopf = if cerunf eq "HOP" then xef.n-out else 1.


  DO i = 1 TO xef.blank-qty:
    FIND FIRST xeb2 WHERE xeb2.company = xest.company
                      AND xeb2.est-no = xest.est-no AND                          
                          xeb2.form-no = xef.form-no AND 
      xeb2.blank-no = i NO-LOCK NO-ERROR.

    

      {ce/markup.i}

      create blk.
      assign
       blk.kli      = xeb2.cust-no
       blk.id       = xeb2.part-no
       blk.bnum     = xeb2.blank-no
       blk.qreq     = xeb2.bl-qty
       blk.stock-no = xeb2.stock-no
       blk.fact     = (fac-tot / qm)
       blk.cost     = (tt-tot / qm)
       blk.fg-wt    = (qm * brd-wu[1]) * (xeb2.bl-qty / qty)
       blk.fg-wt$   = (fr-tot * (xeb2.bl-qty / qty)) / (blk.fg-wt / 100)
       blk.sell     = (if ce-ctrl.sell-by = "G" then blk.fact else blk.cost)
                      / (1 - (v-pct / 100)).

      blk.cost = blk.cost - blk.fg-wt$.

      find first xjob where xjob.i-no eq xeb2.part-no no-error.
      if not available xjob then do:
        create xjob.
        assign
         xjob.i-no     = xeb2.part-no
         xjob.cust-no  = XEB2.cust-no
         xjob.form-no  = xeb2.form-no
         xjob.blank-no = xeb2.blank-no
         xjob.pct      = 1.00
         xjob.stock-no = xeb2.stock-no.
      end.

      assign
       xjob.mat = (dm-tot[5]   + v-prep-mat + mis-tot[1]) / qm
       xjob.lab = (opsplit$[1] + v-prep-lab + mis-tot[3] + ctrl2[2] + ctrl2[3])
                              / qm
       xjob.voh = opsplit$[2] / qm
       xjob.foh = opsplit$[3] / qm.

      if vprint then do:
        find probe where rowid(probe) eq v-rowid.
        
        ASSIGN
         mku_gsa-l = ce-ctrl.rm-rate
         mku_gsa-m = ce-ctrl.fg-rate
         mku_com = ce-ctrl.comm-mrkup
         mku_whs = ce-ctrl.whse-mrkup
         /*probe.freight = if xeb.chg-method ne "P" then 0 else (fr-tot / qm)*/
         /*
         probe.fact-cost = (fac-tot / qm) * (xeb2.bl-qty / qty)
         probe.full-cost = ( tt-tot / qm) * (xeb2.bl-qty / qty)
         */
         probe.fact-cost = (fac-tot / qm)
         probe.full-cost = ( tt-tot / qm)
         probe.gsh-qty   = xef.gsh-qty * (xeb2.bl-qty / qty)
         /*probe.tot-lbs   = (if v-corr then (xef.gsh-len * xef.gsh-wid * .007)
                                      else (xef.gsh-len * xef.gsh-wid / 144)) *
                           probe.gsh-qty / 1000 * xef.weight*/
         probe.gsh-qty   = probe.gsh-qty * v-hopf.

        if ce-ctrl.sell-by = "G" then
           assign
            probe.prof-on      = "Gross"
            probe.sell-price   = probe.fact-cost / (1 - (v-pct / 100)).

        else
           assign
            probe.prof-on      = "Net"
            probe.sell-price   = probe.full-cost / (1 - (v-pct / 100)).

        assign
         v-pct              = v-pct - v-com
         probe.net-profit   =
                       round((1 - (probe.full-cost / probe.sell-price)) * 100,2)
         probe.gross-profit =
                       round((1 - (probe.fact-cost / probe.sell-price)) * 100,2)
         probe.gross-profit = probe.gross-profit - v-com
         probe.net-profit   = probe.net-profit   - v-com.

      end.
  END.

/* JLF added 04/24/96 */
  v-dec = 0.

  for each probe where probe.company = xest.company
                   AND probe.est-no eq xest.est-no break by probe.est-no:
    v-dec = v-dec + probe.gsh-qty.
    if last(probe.est-no) and v-dec ne (xef.gsh-qty * v-hopf) then
       probe.gsh-qty = probe.gsh-qty + ((xef.gsh-qty * v-hopf) - v-dec).
  end.
/* JLF added 04/24/96 */

  IF opsys = "unix" THEN
    UNIX silent copy value(outfile1) value(outfile3).
  ELSE /* IF opsys = "msdos" THEN */
    DOS silent copy value(outfile1) value(outfile3).
    
  OUTPUT TO VALUE(outfile3) APPEND.
  if vprint then do:
    put "Commission"  string(v-com,">>9.99") + "%" to 30
        probe.sell-price * (v-com / 100) to 48
        probe.sell-price * (v-com / 100) * qm to 80 skip

        "Profit - " + string(v-pct,"->>9.99")
            (if ce-ctrl.sell-by eq "N" then
               "% on Full Cost" else "% on Fact Cost") format "x(30)"
        probe.sell-price * (v-pct / 100) to 48 FORMAT "->>,>>9.99"
        probe.sell-price * (v-pct / 100) * qm to 80 FORMAT "->>>,>>9.99" skip

        "SELLING PRICE"
        probe.sell-price to 48 FORMAT "->>,>>9.99"
        probe.sell-price * qm to 80 FORMAT "->>>,>>9.99" skip(1).
  end.

  OUTPUT CLOSE.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
