/* ----------------------------------------------- ce/tan/pr4-brd.p 12/92 cd  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def input  parameter v-vend-no like e-item-vend.vend-no NO-UNDO.

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
DEF BUFFER bf-eb FOR eb.
{ce/print4.i shared shared}

def var v-dec as DEC NO-UNDO.
def var v-num-up as int NO-UNDO.
def var v-cost like item.avg-cost NO-UNDO.
def var v-cqty as   dec NO-UNDO.
def var v-dept like est-op.dept NO-UNDO.
def var v-sqty as   dec NO-UNDO.
def var esty   like est-op.op-waste NO-UNDO.
def var v-num-out as int NO-UNDO.
DEF VAR lv-bestvend-id AS ROWID NO-UNDO.
DEF VAR lv-save-vend LIKE v-vend-no NO-UNDO.
def var v-setup like e-item-vend.setup no-undo.
DEF VAR ld-rm AS DEC NO-UNDO.
DEF VAR ld-hp AS DEC NO-UNDO.

DEF SHARED VAR qty AS INT NO-UNDO.


{sys/inc/cerun.i F}
{sys/inc/ceboard.i}

{ce/msfcalc.i}

IF xeb.pur-man THEN
  ASSIGN
   ld-rm = rm-rate-f
   ld-hp = hand-pct-f.
ELSE
  ASSIGN
   ld-rm = ctrl[3]
   ld-hp = ctrl[2].

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

lv-save-vend = v-vend-no.

/* board */
if xef.board ne "" then
do with frame aa no-labels no-box:
  FIND CURRENT xef EXCLUSIVE.

  find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.
  find first item
      {sys/look/itemW.i}
        and item.i-no eq xef.board
      no-lock.

  b-waste = 0.
  for each est-op where est-op.company = xest.company
                    AND est-op.est-no = xest.est-no
                    AND est-op.line gt 500 no-lock:
    run sys/inc/numout.p (recid(est-op), output v-num-out).
    b-waste = b-waste +
              (est-op.op-waste /
              (v-num-out * if not est-op.op-sb then xeb.num-up else 1)).
  end.
  {sys/inc/roundup.i b-waste}

  find first est-op where  est-op.company = xest.company
                    AND est-op.est-no = xest.est-no
                    AND est-op.line ge 500 no-lock.

  assign
   v-sqty = 0
   v-dept = est-op.dept.

  if est-op.b-num eq 0 then v-sqty = est-op.num-sh.

  else
  for each bf-eb
      where bf-eb.company = xest.company
        AND bf-eb.est-no   eq xest.est-no
        and bf-eb.form-no  ne 0
        and bf-eb.blank-no ne 0
      no-lock:

    find first est-op
        where est-op.company = xest.company
          AND est-op.est-no eq bf-eb.est-no
          and est-op.s-num eq bf-eb.form-no
          and est-op.b-num eq bf-eb.blank-no
          and est-op.line  ge 500
          and est-op.dept  eq v-dept
        no-lock no-error.

    if avail est-op then v-sqty = v-sqty + est-op.num-sh.

    else do:
      find first est-op
          where est-op.company = bf-eb.company
            AND est-op.est-no eq bf-eb.est-no
            and est-op.s-num eq bf-eb.form-no
            and est-op.b-num eq 0
            and est-op.line  ge 500
            and est-op.dept  ne v-dept
          no-lock no-error.

      if avail est-op then
        v-sqty = v-sqty +
                 round(est-op.num-sh * (bf-eb.bl-qty / xest.est-qty[1]),0).
    end.

  end.

  assign
   xxx   = qty / (xeb.num-up * xef.n-out)
   b-qty = (if v-corr then (xef.gsh-len * xef.gsh-wid * .007)
                      else (xef.gsh-len * xef.gsh-wid / 144)) *
           v-sqty / 1000.  /* total msf including waste */

  {sys/inc/roundup.i xxx}

  {ce/pr4-brd.i}

  assign
   r-spo[1] = v-sqty - round(qty / (xeb.num-up * xef.n-out * xef.n-out-l),0) - b-waste
   qty      = qty + ((b-waste + r-spo[1]) * xeb.num-up * xef.n-out * xef.n-out-l)     
   b-cost   = b-cost / (qty / 1000)                                    
   qty      = qty - ((b-waste + r-spo[1]) * xeb.num-up * xef.n-out * xef.n-out-l).


  if b-uom eq "REM" then
    b-msh = b-msh * if xef.cost-msh eq 0 then (xef.gsh-wid / 26) else 1.

  b-tot  = b-cost * (qty / 1000) .
  b-totw = (b-tot / xxx) * b-waste.
  b-tot  = round(b-tot,2).
  b-totw = round(b-totw,2).
  b-wt   = item.basis-w.
  if NOT xeb.pur-man AND xef.nc = no then do:
    b-msh   = 0.
    b-cost  = 0.
    b-tot   = 0.
    b-totw  = 0.
    b-tot   = 0.
    b-totw  = 0.
  end.
  dm-tot[3] = b-totw.
  dm-tot[4] = b-cost.
  dm-tot[5] = b-tot + b-totw.
  if ld-rm > 0 then
  ctrl2[3] = ((b-qty * item.basis-w) / 100) * ld-rm. /* handling chg */

  if ld-hp > 0 then
  ctrl2[2] = (b-tot + b-totw) * ld-hp. /* handling pct. */

  r-spo$[1] = r-spo[1] * (b-tot / xxx).

/* CTS - zero out spoilage if no spoil/waste found (fix rounding problem) */
def var op-spoil-found as log.
def var op-waste-found as log.

assign
  op-spoil-found = false
  op-waste-found = false.

findspoloop:
for each est-op where est-op.company = xest.company
                  AND est-op.est-no = xest.est-no and est-op.line > 500
by est-op.line descending:
  if est-op.op-spoil ne 0 then
  do:
    op-spoil-found = true.
    leave findspoloop.
  end.
  if est-op.op-waste ne 0 then
  do:
    op-waste-found = true.
    leave findspoloop.
  end.
end.

if not(op-spoil-found) and not(op-waste-found) then
assign
  r-spo[1] = 0
  r-spo$[1] = 0.
/* CTS end */

  v-hopf = if cerunf eq "HOP" then xef.n-out else 1.

  display
    item.i-name format "x(20)" item.basis-w item.cal format ">9.999<"
    "$" + string(b-msh,">>>>9.99") format "x(9)" TO 46 b-uom SPACE(0)
    b-cost TO 68 b-tot format ">,>>>,>>9.99" TO 80 SKIP
    "Board MR  Waste"
    b-waste * v-hopf format ">>>>9" TO 46 "Sht"
    (b-cost * (xeb.num-up * xef.n-out * xef.n-out-l)) * b-waste / 1000
    format ">>>>9.99" TO 59
    b-totw / (qty / 1000) format ">>>>9.99" TO 68
    b-totw format ">,>>>,>>9.99" TO 80 SKIP
    "Board RUN Waste"
    r-spo[1] * v-hopf format ">>>>>9" TO 46 "Sht"
    r-spo$[1] / (qty / 1000) format ">>>>9.99" TO 68
    r-spo$[1] format ">,>>>,>>9.99" TO 80 SKIP WITH STREAM-IO.

  find first brd where brd.form-no = xef.form-no and
                       brd.i-no    = xef.board
                       no-error.
  if not avail brd then do:
     create brd.
     assign brd.form-no = xef.form-no
            brd.blank-no = 00
            brd.i-no    = xef.board
            brd.dscr    = xef.brd-dscr
            brd.basis-w = item.basis-w.
  end.
  brd.qty = (qty / xeb.num-up) + b-waste + r-spo[1].
  brd.qty-uom = "EA".
  brd.sc-uom = b-uom.
  brd.cost = b-msh.
  brd.len  = brd-l[3].
  brd.wid  = brd-w[3].
  brd.cost-m = b-cost.
  brd.len = xef.gsh-len.
  brd.wid = xef.gsh-wid.

  zzz = 0.
  if xef.fr-msh ne 0 then do:
    if xef.fr-uom = "MSH" then
    zzz =  ((qty / xeb.num-up) / 1000) * xef.fr-msh.
    else
    if xef.fr-uom = "MSF" then
    zzz =  b-qty * xef.fr-msh.
    else
    if xef.fr-uom = "TON" then
    zzz = ((b-qty * xef.weight) / 2000) * xef.fr-msh.
    else
    if xef.fr-uom = "CWT" then
    zzz = ((b-qty * xef.weight) / 100 ) * xef.fr-msh.
    put "Freight In Costs" "$" +
      string(xef.fr-msh,">>>9.99") TO 46 xef.fr-uom
      zzz / (qty / 1000) format ">>>>9.99" TO 68
      zzz format ">>>>,>>9.99" TO 80 SKIP.
  end.
  dm-tot[4] = dm-tot[4] + (b-totw / (qty / 1000)) +
  (r-spo$[1] / (qty / 1000)) +
  (zzz    / (qty / 1000)).
  dm-tot[5] = dm-tot[5] + r-spo$[1] + zzz.

  v-dec = xest.est-qty[1] / (xeb.num-up * xef.n-out * xef.n-out-l).
  {sys/inc/roundup.i v-dec}
  xef.gsh-qty = v-dec + b-waste + r-spo[1].
  FIND CURRENT xef NO-LOCK.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
