/* ----------------------------------------------- ce/box/pr42-brd.p 2/93 cd  */
/* box - 2 sheet                                                              */
/* -------------------------------------------------------------------------- */

def input  parameter v-vend-no like e-item-vend.vend-no NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{ce/print4.i shared shared}

DEF SHARED VAR qty AS INT NO-UNDO.

def var call_id as recid no-undo.
def var v-dec as dec.
def var tmp-waste as de extent 99.
def var max-w as de.
def var fr-mshts as de.
def var rm-wt$ as de.
def var rm-wt% as de.
def var rm-wt  as de.
def var v-n-out as int init 1 no-undo.
def var v-num-up like xeb.num-up.
def var v-num-sh like est-op.num-sh.
def var v-cost like item.avg-cost.
def var v-cqty as   dec.
def var v-setup like e-item-vend.setup no-undo.
DEF VAR lv-bestvend-id AS ROWID NO-UNDO.
DEF VAR lv-save-vend LIKE v-vend-no NO-UNDO.
DEF VAR ld-rm AS DEC NO-UNDO.
DEF VAR ld-hp AS DEC NO-UNDO.
DEF VAR li-blkw AS INT NO-UNDO.
DEF VAR li-blks AS INT NO-UNDO.
DEF VAR li-blkt AS INT NO-UNDO.

{sys\inc\ceboard.i}

do transaction:
  {sys/inc/cerun.i F}

  {ce/msfcalc.i}
end.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

lv-save-vend = v-vend-no.

for each xef
    where xef.company eq xest.company
      and xef.est-no  eq xest.est-no
    with frame brd2 no-labels no-box STREAM-IO width 80 down:

  RUN sys/inc/numup.p (xef.company, xef.est-no, xef.form-no, OUTPUT v-num-up).   

  tmp-waste = 0. /* zero array */
  find first item {sys/look/itemW.i} and
    item.i-no = xef.board no-lock no-error.
  find first xeb
      where xef.company eq xest.company
        and xef.est-no  eq xest.est-no
        and xeb.form-no eq xef.form-no
      no-lock no-error.
      
  IF xeb.pur-man THEN
    ASSIGN
     ld-rm = rm-rate-f
     ld-hp = hand-pct-f.
  ELSE
    ASSIGN
     ld-rm = ctrl[3]
     ld-hp = ctrl[2].

  run ce/box/prokalk2.p .
/*
  assign
   v-num-up  = xeb.num-up / (if xest.form-qty eq 1 then 2 else 1)
   tmp-waste = 0
   b-waste   = 0
   v-num-sh  = 0.

  for each est-op
      where est-op.company eq xef.company
        and est-op.est-no  eq xef.est-no
        and est-op.s-num   eq xef.form-no 
        and est-op.line    gt 500:

    if est-op.num-sh gt v-num-sh then v-num-sh = est-op.num-sh.

    run sys/inc/numout.p (recid(est-op), output v-n-out).

    if est-op.b-num eq 0 then
      b-waste = b-waste + est-op.op-waste.
    else
      tmp-waste[est-op.b-num] =
      tmp-waste[est-op.b-num] + (est-op.op-waste / v-num-up /
                                 (if xef.n-out   eq 0 then 1 else xef.n-out) /
                                 (if xef.n-out-l eq 0 then 1 else xef.n-out-l)).
  end.
  max-w = 0.
  do i = 1 TO 99:
    if tmp-waste[i] > max-w then max-w = tmp-waste[i].
  end.
  /* max-w = most waste for blank fed ops this sheet */
  b-waste = b-waste + max-w.
  {sys/inc/roundup.i b-waste}

  r-spo[xef.form-no] =
    truncate(v-num-sh - b-waste -
             (tt-blk / v-num-up /
             (if xef.n-out   eq 0 then 1 else xef.n-out) /
             (if xef.n-out-l eq 0 then 1 else xef.n-out-l)),0).
*/

  b-waste = spo.

  if avail e-item then
  find first e-item-vend of e-item
      where e-item-vend.item-type eq yes
        and e-item-vend.vend-no   eq (IF lv-save-vend EQ "bestvendor" THEN ""
                                      ELSE lv-save-vend)
      no-lock no-error.

  /* board */
  if xef.board ne "" then do with frame aa no-labels STREAM-IO no-box:
    find first est-op
        where est-op.company eq xef.company
          and est-op.est-no  eq xef.est-no
          and est-op.s-num   eq xef.form-no
          and est-op.line    gt 500
        no-lock.

    assign
     v-setup  = 0
                
     /* msf for  job */
     b-qty = (if v-corr then (xef.gsh-len * xef.gsh-wid * .007)
                        else (xef.gsh-len * xef.gsh-wid / 144)) *
             est-op.num-sh / 1000.

    {ce/pr4-brd.i}

    if b-uom eq "REM" then
       b-msh = b-msh * IF NOT((ceboard-log EQ NO OR v-vend-no EQ "") AND xef.cost-msh gt 0) then (xef.gsh-wid / 26) else 1.

    ASSIGN
     li-blkw             = b-waste * v-num-up * xef.n-out * xef.n-out-l
     li-blks             = r-spo[xef.form-no] * v-num-up * xef.n-out * xef.n-out-l
     li-blkt             = t-blkqty[xef.form-no] + li-blkw + li-blks
     b-tot               = b-cost * (t-blkqty[xef.form-no] / li-blkt)
     b-totw              = b-cost * (li-blkw / li-blkt)
     r-spo$[xef.form-no] = b-cost * (li-blks / li-blkt).

    /* cost per sht * #wasted shts */
    if NOT xeb.pur-man AND xef.nc = NO then
       assign b-msh = 0 b-cost = 0 b-tot = 0 b-totw = 0 r-spo$[xef.form-no] = 0 v-setup = 0.

    ASSIGN
       dm-tot[3] = dm-tot[3] + b-totw + v-setup
       dm-tot[5] = dm-tot[5] + b-tot + b-totw + r-spo$[xef.form-no] + v-setup
       v-hopf = if cerunf eq "HOP" then xef.n-out else 1.

    display
      string(xef.form-no,"9") + "-00" format "x(4)"
      item.i-name format "x(20)" item.basis-w
      item.cal format ">9.999<"
      "$" + string(b-msh,">>>9.99") TO 50 space b-uom space(0)
      v-setup when v-setup ne 0 format ">>>9.99" to 61
      b-tot + (v-setup / (t-blkqty[xef.form-no] / 1000)) TO 69
      b-tot + v-setup format ">>>>,>>9.99" TO 80 skip
      space(5)
      "Board MR  Waste"
      b-waste * v-hopf format ">>>>9" TO 50 space(0) " Sht" space(0)
      (b-tot / (t-blkqty[xef.form-no] / v-num-up)) * b-waste format ">>>9.99" TO 61
      /* cost of waste amortized per 1000 of all blanks on this form */
      b-totw / (t-blkqty[xef.form-no] / 1000) format ">>>9.99" TO 69
      b-totw format ">>>>,>>9.99" TO 80 skip
      space(5)
      "Board RUN Waste"
      r-spo[xef.form-no] * v-hopf format ">>>>9" TO 50 space(0) " Sht" space(0)
      /* cost of spoil amortized per 1000 of all blanks on this form */
      r-spo$[xef.form-no] /
      (t-blkqty[xef.form-no] / 1000) format ">>>9.99" TO 69
      r-spo$[xef.form-no]
      format ">>>>,>>9.99" TO 80 skip.
    lin-count = lin-count + 1.

    find first brd where brd.form-no = xef.form-no and
                         brd.i-no    = xef.board
                         no-error.
    if not available brd then do:
       create brd.
       assign brd.form-no = xef.form-no
              brd.blank-no = 00
              brd.i-no    = xef.board
              brd.dscr    = xef.brd-dscr
              brd.basis-w = item.basis-w.
    end.

    ASSIGN
    brd.qty = t-shtfrm[xef.form-no] + b-waste + r-spo[xef.form-no]
    brd.qty-uom = "EA"
    brd.sc-uom = b-uom
    brd.cost = b-msh + (v-setup / b-qty)
    brd.cost-m = b-cost +
                 ((v-setup +
                  (b-cost * ((b-waste + r-spo[xef.form-no]) *
                                               v-num-up * v-out / 1000))) /
                  (t-blkqty[xef.form-no] / 1000))
    brd.len = xef.gsh-len
    brd.wid = xef.gsh-wid
    zzz = 0.

    if xef.fr-msh ne 0 then do:
      if xef.fr-uom = "MSH" then
      zzz =  (t-shtfrm[xef.form-no] / 1000) * xef.fr-msh.
      else
      if xef.fr-uom = "MSF" then
      zzz =  b-qty * xef.fr-msh.
      else
      if xef.fr-uom = "TON" then
      zzz = ((b-qty * xef.weight) / 2000) * xef.fr-msh.
      else
      if xef.fr-uom = "CWT" then
      zzz = ((b-qty * xef.weight) / 100 ) * xef.fr-msh.
      put space(5) "Freight In Costs" "$" +
        string(xef.fr-msh,">>>9.99") TO 50 space(1) xef.fr-uom
        zzz TO 80 skip.
    end.
    ASSIGN
    fr-mshts = zzz
    dm-tot[5] = dm-tot[5] +  fr-mshts.
  end.

   /* rm handling chg per cwt*/
  if ld-rm ne 0 then
    assign
     rm-wt    = b-qty * item.basis-w
     rm-wt$   = (rm-wt / 100) * ld-rm
     ctrl2[3] = ctrl2[3] + rm-wt$.

  /* rm handling pct. */
  if ld-hp ne 0 then
    assign
     rm-wt%   = (b-tot + b-totw + r-spo$[xef.form-no]) * ld-hp
     ctrl2[2] = ctrl2[2] + rm-wt%.

  /* distribute cost to each blank */
  for each blk where blk.snum = xeb.form-no:
    if ld-rm ne 0 then
    blk.lab = blk.lab + (rm-wt$ * blk.pct).
    if ld-hp ne 0 then
    blk.lab = blk.lab + (rm-wt% * blk.pct).
    blk.cost = blk.cost + ((b-tot + v-setup + b-totw + fr-mshts + rm-wt$ + rm-wt% +
    + r-spo$[xef.form-no]) * blk.pct).
  end.

  v-dec = t-blkqty[xef.form-no] / (t-blksht[xef.form-no] * xef.n-out * xef.n-out-l).
  {sys/inc/roundup.i v-dec}
  xef.gsh-qty = v-dec + b-waste + r-spo[xef.form-no].
end.

call_id = recid(item).

/* end ---------------------------------- copr. 1992  advanced software, inc. */
