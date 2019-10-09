/* -------------------------------------------------- cec/pr4-brd.p 12/92 cd  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def input  parameter v-vend-no like e-item-vend.vend-no NO-UNDO.
def output parameter v-vend-list AS CHAR NO-UNDO.

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def shared var qty as int NO-UNDO.
def shared var v-drop-rc as log no-undo.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def shared buffer xop  for est-op.

{cec/print4.i shared shared}

def var j as int no-undo.
def var zzz as DEC no-undo.
def var v-setup like e-item-vend.setup NO-UNDO.
def var v-qty   as   dec decimals 10 NO-UNDO.
def var v-out   as   int NO-UNDO.
DEF VAR v-num-up AS INT NO-UNDO.
def var v-msf   as   dec NO-UNDO.
DEF VAR lv-bestvend-id AS ROWID NO-UNDO.
DEF VAR lv-save-vend LIKE v-vend-no NO-UNDO.
DEF VAR ll-foam AS LOG NO-UNDO.
DEF VAR hld-qty LIKE save-qty NO-UNDO.
DEF VAR ld-rm-rate AS DEC NO-UNDO.
DEF VAR old-save-qty LIKE save-qty NO-UNDO.
DEF VAR old-b-qty LIKE b-qty NO-UNDO.

def shared workfile w-form
    field form-no like xef.form-no
    field min-msf as   log.


{cec/msfcalc.i}

{cec/rollfac.i}

{sys/inc/foamcost.i}
{sys/inc/ceboard.i}
{sys/inc/cecpurwas.i}

FIND CURRENT xef EXCLUSIVE.

RUN sys/inc/numup.p (xef.company, xef.est-no, xef.form-no, OUTPUT v-num-up).

ASSIGN
 hld-qty      = qty
 save-qty     = hld-qty
 lv-save-vend = v-vend-no.

FIND FIRST est-op
    WHERE est-op.company EQ xest.company
      AND est-op.est-no  EQ xest.est-no
      AND est-op.qty     EQ v-op-qty
      AND est-op.line    GE 500
    NO-LOCK NO-ERROR.
    
IF AVAIL est-op THEN
   qty = est-op.num-sh * v-num-up.

find first item 
    {sys/look/itemW.i} 
      and item.i-no eq xef.board 
    no-lock no-error.

if avail item then
find first e-item of item no-lock no-error.

if avail e-item then
find first e-item-vend of e-item
    where e-item-vend.item-type eq yes
      and e-item-vend.vend-no   eq (IF lv-save-vend EQ "bestvendor" THEN ""
                                    ELSE lv-save-vend)
    no-lock no-error.

/* board */
if xef.board ne "" then
printboard:
do with frame aaxx no-labels no-box:
  v-setup = 0.
  if avail item then 
  find first item-bom 
      where item-bom.company  eq cocode
        and item-bom.parent-i eq item.i-no
        AND item-bom.line# LT 9
      no-lock no-error.
  if avail item-bom and not avail e-item-vend then leave printboard.

  /* If bom (medium/liner) exist, then only print item
     code with the bom items calculated as costs */
  find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

  RUN est/ef-#out.p (ROWID(xef), OUTPUT v-out).

  RUN cec/isitfoam.p (ROWID(xef), OUTPUT ll-foam).

  IF ll-foam THEN DO:
    v-out = 1.
    RUN cec/foamblks.p (ROWID(xef), OUTPUT v-num-up).

    IF AVAIL est-op THEN
       save-qty = (est-op.num-sh - (spo + r-spo[1])) * v-num-up * v-out.

  END.

  /* msf */
  IF cecpurwaste-log AND xeb.pur-man THEN
     ASSIGN
     old-save-qty = save-qty + ((spo + r-spo[1]) * (v-num-up * v-out))
     old-b-qty    = if v-corr then
                    (((xef.gsh-wid * xef.gsh-len) * (old-save-qty / v-num-up))
                       / v-out) * .000007
                    else
                    (((xef.gsh-wid * xef.gsh-len) * (old-save-qty / v-num-up))
                       / v-out) / 144000. 
  
  ELSE
     save-qty  = save-qty + ((spo + r-spo[1]) * (v-num-up * v-out)).

  ASSIGN
   b-qty     = if v-corr then
                 (((xef.gsh-wid * xef.gsh-len) * (save-qty / v-num-up))
                   / v-out) * .000007
               else
                 (((xef.gsh-wid * xef.gsh-len) * (save-qty / v-num-up))
                   / v-out) / 144000
   v-msf     = b-qty.

  {cec/pr4-brd.i}

  IF  v-board-cost-from-blank AND ll-foam THEN 
      ASSIGN
        b-tot = b-msh * (xeb.t-len * xeb.t-wid * (IF xeb.t-dep > 0 THEN xeb.t-dep ELSE 1) / 144) * hld-qty
        b-cost  = b-tot / hld-qty * 1000.
  ELSE   
      ASSIGN
        b-cost = b-cost / save-qty * hld-qty
        b-tot  = b-cost * (hld-qty / 1000).
  
  ASSIGN
  fg-qty = if v-corr then (xeb.t-sqin - xeb.t-win) * hld-qty * .000007
                     else (xeb.t-sqin - xeb.t-win) * hld-qty / 144000
  fg-wt = fg-qty * if avail item then item.basis-w else 1
                         
  b-totw = b-cost * (v-num-up * v-out * spo / 1000)
  b-tot  = round(b-tot,2)
  b-totw = round(b-totw,2)
  b-wt   = if avail item then item.basis-w else 0.
  IF NOT v-board-cost-from-blank THEN 
    b-cost = b-cost / v-sqft-fac.
  
  IF cecpurwaste-log AND xeb.pur-man THEN
     ASSIGN
        save-qty = old-save-qty
        b-qty = old-b-qty
        v-msf = b-qty.

  if NOT xeb.pur-man AND xef.nc = no THEN
     ASSIGN
        b-msh = 0
        b-cost = 0
        b-tot = 0
        b-totw = 0
        v-setup = 0.

  ASSIGN
     dm-tot[3] = (b-cost * v-num-up * v-out) *
                 spo / 1000
     ld-rm-rate = IF xeb.pur-man THEN rm-rate-f ELSE ctrl[3].

  if ld-rm-rate gt 0 then
    ctrl2[3] = ((v-msf * if avail item then item.basis-w else 1) / 100) *
               ld-rm-rate. /* handling chg */

  display item.i-name format "x(20)" item.basis-w when avail item
          v-vend-no AT 30
          "$" + string(b-msh,">>>>9.99") format "x(9)" to 48 b-uom space(0)
          v-setup when v-setup ne 0 format ">>>9.99" to 59
          b-cost + (v-setup / (hld-qty / 1000)) format ">>>>9.99" to 68
          b-tot + v-setup format ">>>>,>>9.99" to 80 skip
          "Board SU  Waste"
          spo format ">>>>>9" to 48 space(0) " Sht" space(0)
          (b-cost * (spo * v-num-up * v-out / 1000)) format ">>>9.99" to 59
          (b-totw / (hld-qty / 1000)) / v-sqft-fac format ">>>>9.99" to 68
          b-totw to 80 format ">>>>,>>9.99" skip
          "Board RUN Waste"
          r-spo[1] format ">>>>>>9" to 48 space(0) " Sht" space(0)
          /* cost of spoil amortized per 1000 of all blanks on this form */
          (b-cost * (r-spo[1] * v-num-up * v-out / 1000)) /
                        (hld-qty / 1000) / v-sqft-fac format ">>>>9.99" to 68
          (b-cost * (r-spo[1] * v-num-up * v-out / 1000))
                                                format ">>>,>>9.99" to 80 skip with stream-io no-box.

  ASSIGN
  dm-tot[5] = b-tot + v-setup + b-totw +
              ((b-tot / (hld-qty / (v-num-up * v-out))) *
               r-spo[1])
  dm-tot[4] = dm-tot[5] / (hld-qty / 1000).

  if NOT xeb.pur-man AND not avail item-bom then do:
     find first brd 
         where brd.form-no eq xef.form-no 
           and brd.i-no    eq xef.board
         no-error.
     if not avail brd then do:
       create brd.
       assign 
        brd.form-no = xef.form-no
        brd.blank-no = 01
        brd.i-no    = xef.board
        brd.dscr    = xef.brd-dscr
        brd.basis-w = item.basis-w.
     end.
    
     find first est-op
         where est-op.company = xest.company
           and est-op.est-no =  xest.est-no
           and est-op.qty  eq v-op-qty
           and est-op.s-num eq xef.form-no
           and est-op.line  ge 500
         no-lock no-error.
       
     ASSIGN
        brd.qty = save-qty / (v-num-up * v-out)
        brd.qty-mr = spo
        brd.qty-wst = r-spo[1]
        brd.qty-uom = "EA"
        brd.sc-uom = b-uom
        brd.cost = b-msh + (v-setup / b-qty)
        brd.len  = brd-l[3]
        brd.wid  = brd-w[3]
        brd.cost-m = b-cost +
                     ((v-setup +
                      (b-cost * ((spo + r-spo[1]) * v-num-up * v-out / 1000))) /
                      (hld-qty / 1000)).
  end.

  zzz = 0.
  if xef.fr-msh ne 0 then do:
    if xef.fr-uom eq "MSH" then
      zzz = (xop.num-sh / 1000) * xef.fr-msh.

    else 
    if xef.fr-uom eq "MSF" then
      zzz = v-msf * xef.fr-msh.

    else 
    if xef.fr-uom eq "TON" then
      zzz = ((v-msf * xef.weight) / 2000) * xef.fr-msh.

    else 
    if xef.fr-uom eq "CWT" then
      zzz = ((v-msf * xef.weight) / 100 ) * xef.fr-msh.

    put "Freight In Costs" "$" +
        string(xef.fr-msh,">>>9.99") to 48 space(1) xef.fr-uom
        zzz / (hld-qty / 1000) format ">>>9.99" to 68
        zzz to 80 skip.
  end.

  ASSIGN
     dm-tot[3] = dm-tot[3] + v-setup
     dm-tot[4] = dm-tot[4] + (zzz / (hld-qty / 1000))
     dm-tot[5] = dm-tot[5] + zzz.
end.

if xef.board ne "" then
printmed-lin:
do with frame aaxx2 no-labels no-box:
  /* If bom (medium/liner) exist, then only print item
     code with the bom items calculated as costs */
  if avail item then 
  find first item-bom 
      where item-bom.company  eq cocode
        and item-bom.parent-i eq item.i-no
        AND item-bom.line# LT 9
      no-lock no-error.
  if avail item-bom then do:
     if not avail e-item-vend then
        display xef.brd-dscr " -----Bill of materials:" with stream-io.

     run cec/pr4-mfl.p (v-vend-no, (not avail e-item-vend), hld-qty). /* medium-flute-liner-laminate */
  end.
end.

FIND CURRENT xef NO-LOCK.

qty = hld-qty.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
