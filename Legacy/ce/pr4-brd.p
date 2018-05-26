/* --------------------------------------------------- ce/pr4-brd.p 12/92 cd  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def input  parameter v-vend-no like e-item-vend.vend-no NO-UNDO.

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def shared buffer xop  for est-op.
DEF SHARED VAR qty AS INT NO-UNDO.

{ce/print4.i shared shared}

def var v-cost like item.avg-cost.
def var v-cqty as   dec.
DEF VAR lv-bestvend-id AS ROWID NO-UNDO.
DEF VAR lv-save-vend LIKE v-vend-no NO-UNDO.
DEF VAR brd-spo LIKE spo.
DEF VAR brd-r-spo LIKE r-spo.
def var v-num-up as int NO-UNDO.
def var v-setup like e-item-vend.setup no-undo.
DEF VAR ld-rate AS DEC NO-UNDO.
DEF VAR v-t-win AS DEC DECIMALS 4 NO-UNDO.
DEF BUFFER b-item FOR ITEM.
DEF VAR v-li AS INT NO-UNDO.

{sys/inc/cerun.i F}
{sys/inc/ceboard.i}

v-hopf = if cerunf eq "HOP" then xef.n-out else 1.

{ce/msfcalc.i}

lv-save-vend = v-vend-no.

/* board */
if xef.board ne "" then do with frame aaxx no-labels no-box:
  FIND CURRENT xef.

  find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.
  find first item
      {sys/look/itemW.i}
        and item.i-no eq xef.board
      no-lock.
      
  find first est-op WHERE est-op.company = xest.company 
                      AND est-op.est-no = xest.est-no
                      and est-op.qty  eq v-op-qty
                      and est-op.line ge 500
                      no-lock no-error.
     
  assign
   zzz   = qty / (xeb.num-up * xef.n-out * xef.n-out-l)
   b-qty = (if v-corr then (xef.gsh-len * xef.gsh-wid * .007)
                      else (xef.gsh-len * xef.gsh-wid / 144)) *
           est-op.num-sh / 1000.  /* total msf including waste */

  {sys/inc/roundup.i zzz}

  ASSIGN
     v-t-win = 0
     v-setup = 0.

  {ce/pr4-brd.i}

  assign
   qty    = qty + ((spo + r-spo[1]) * xeb.num-up * xef.n-out * xef.n-out-l)
   b-cost = b-cost / (qty / 1000)        /* 1000 blks */
   qty    = qty - ((spo + r-spo[1]) * xeb.num-up * xef.n-out * xef.n-out-l)
   b-qty  = (if v-corr then (xef.gsh-len * xef.gsh-wid * .007)
                       else (xef.gsh-len * xef.gsh-wid / 144)) *
            est-op.num-sh / 1000.  /* total msf including waste */

  if b-uom eq "REM" then
     b-msh = b-msh * IF NOT((ceboard-log EQ NO OR v-vend-no EQ "") AND xef.cost-msh gt 0) then (xef.gsh-wid / 26) else 1.

  IF xeb.est-type EQ 1 THEN
  do v-li = 1 TO 4:
    find first b-item WHERE
         b-item.company EQ xef.company and
         b-item.i-no eq xef.leaf[v-li]
         no-lock no-error.

    if avail b-item and b-item.mat-type eq "W" and
       (xef.leaf-l[v-li] ne 0 and xef.leaf-w[v-li] ne 0) then
       DO:
          IF xef.leaf-bnum[v-li] NE 0 THEN
             v-t-win = v-t-win + (xef.leaf-l[v-li] * xef.leaf-w[v-li]).
          ELSE
             v-t-win = v-t-win + (xef.leaf-l[v-li] * xef.leaf-w[v-li] / xeb.num-up).
       END.
  end.
  ELSE
     v-t-win = xeb.t-win.

  ASSIGN
  fg-qty = (if v-corr then ((xeb.t-sqin - v-t-win) * .007)
                      else ((xeb.t-sqin - v-t-win) / 144)) *
           qty / 1000 /* msf */
  fg-wt = fg-qty * item.basis-w

  b-tot  = b-cost * (qty / 1000)
  b-tot  = round(b-tot,2)
  b-totw = (b-tot / zzz) * spo
  b-totw = round(b-totw,2)
  b-wt   = item.basis-w.

  if NOT xeb.pur-man AND xef.nc = no THEN
    ASSIGN
       b-msh   = 0
       b-cost  = 0
       b-tot   = 0
       b-totw  = 0
       v-setup = 0.
  
  dm-tot[3] = b-totw.

  ld-rate = IF xeb.pur-man THEN rm-rate-f ELSE ctrl[3].
  IF ld-rate GT 0 THEN
    ctrl2[3] = ((b-qty * IF AVAIL item THEN item.basis-w ELSE 1) / 100) *
               ld-rate. /* handling chg */

  ld-rate = IF xeb.pur-man THEN hand-pct-f ELSE ctrl[2].
  IF ld-rate GT 0 THEN ctrl2[2] = (b-tot + b-totw) * ld-rate. /* handling pct. */

  DISPLAY
    item.i-name format "x(20)" item.basis-w item.cal format ">9.999<"
    "$" + string(b-msh,">>>>9.99") format "x(9)" TO 48 b-uom space(0)
    v-setup when v-setup ne 0 format ">>>9.99" to 59
    b-cost + (v-setup / (hld-qty / 1000)) format ">>>>9.99" to 68
    b-tot + v-setup format ">>>>,>>9.99" to 80 skip
    "Board MR  Waste"
    spo * v-hopf format ">>>>>9" TO 48 space(0) " Sht" space(0)
    round(spo * (b-tot / zzz),2) format ">>>9.99" TO 59
    (b-totw / (qty / 1000)) format ">>>>9.99" TO 68
    b-totw format ">,>>>,>>9.99" TO 80 skip
    "Board RUN Waste"
    r-spo[1] * v-hopf format ">>>>>>9" TO 48 space(0) " Sht" space(0)
    /* cost of spoil amortized per 1000 of all blanks on this form */
    ((b-tot / zzz) * r-spo[1]) /
    (qty / 1000) format ">>>>9.99"   TO 68
    ((b-tot / zzz) * r-spo[1])
    format ">,>>>,>>9.99" TO 80 SKIP WITH STREAM-IO.

  ASSIGN
     dm-tot[5] = b-tot + v-setup + b-totw + ((b-tot / zzz) * r-spo[1])
     dm-tot[4] = dm-tot[5] / (qty / 1000)
     lctr = lctr + 3.

  if NOT xeb.pur-man AND not avail item-bom then do:
    find first brd where brd.form-no eq xef.form-no and
                         brd.i-no    eq xef.board
                         no-error.
    if not avail brd then do:
       create brd.
       assign brd.form-no = xef.form-no
              brd.blank-no = 01
              brd.i-no    = xef.board
              brd.dscr    = xef.brd-dscr
              brd.basis-w = item.basis-w.
    end.

    ASSIGN
    brd.qty = zzz + spo + r-spo[1]
    brd.qty-mr = spo
    brd.qty-wst = r-spo[1]
    brd.qty-uom = "EA"
    brd.sc-uom = b-uom
    brd.cost = b-msh + (v-setup / b-qty)
    brd.cost-m = b-cost +
                 ((v-setup +
                  (b-cost * ((spo + r-spo[1]) * v-num-up * v-out / 1000))) /
                  (hld-qty / 1000))
    brd.len = xef.gsh-len
    brd.wid = xef.gsh-wid
    brd.dep = xef.gsh-dep.
  END.

  zzz = 0. /* now used for freight charges */
  if xef.fr-msh ne 0 then do:
    if xef.fr-uom eq "MSH" then
    zzz =  (xop.num-sh / 1000) * xef.fr-msh.
    else
    if xef.fr-uom eq "MSF" then
    zzz =  b-qty * xef.fr-msh.
    else
    if xef.fr-uom eq "TON" then
    zzz = ((b-qty * xef.weight) / 2000) * xef.fr-msh.
    else
    if xef.fr-uom eq "CWT" then
    zzz = ((b-qty * xef.weight) / 100 ) * xef.fr-msh.

    put "Freight In Costs" "$" +
      string(xef.fr-msh,">>>9.99") TO 48 space(1) xef.fr-uom
      zzz / (qty / 1000)
      format ">>>9.99" TO 68
      zzz TO 80 skip.
    lctr = lctr + 1.
  end.

  brd-spo = spo.
  DO i = 1 TO EXTENT(spo):
    brd-r-spo[i] = r-spo[i].
  END.
   
  if xef.medium ne "" or xef.flute ne "" or xef.lam-code ne "" then
    run ce/pr4-mfl.p . /* medium-flute-liner laminate */

  spo = brd-spo.
  DO i = 1 TO EXTENT(spo):
    r-spo[i] = brd-r-spo[i].
  END.

  ASSIGN
  dm-tot[3] = dm-tot[3] + v-setup
  dm-tot[4] = dm-tot[4] + (zzz / (qty / 1000))
  dm-tot[5] = dm-tot[5] + zzz.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
