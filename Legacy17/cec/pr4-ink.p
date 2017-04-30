/* ---------------------------------------------------- cec/pr4-ink.p 4/92 cd */

def input parameter v-vend-no like e-item-vend.vend-no.

{sys/inc/var.i shared}

def shared var qty as int NO-UNDO.

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

{cec/print4.i shared shared}

def var vuom like item.cons-uom NO-UNDO.
def var vqty as   char NO-UNDO.
def var g-qty  as de  NO-UNDO.
def var g-cost as de format ">>,>>9.99" NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.
DEF VAR v-first-pass AS LOG NO-UNDO.

DEF TEMP-TABLE tt-ink NO-UNDO FIELD i-code LIKE ink.i-code
                              FIELD i-dscr LIKE ink.i-dscr
                              FIELD pass AS INT.

DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

{cec/msfcalc.i}

{cec/rollfac.i}

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

{cec/pr4-ink-single.i}

j = 0.
FOR EACH est-op NO-LOCK
    WHERE est-op.company EQ xeb.company
      AND est-op.est-no  EQ xeb.est-no
      AND (est-op.qty    EQ v-op-qty or xeb.est-type ge 7)
      AND est-op.s-num   EQ xeb.form-no
      AND est-op.line    GE 500
      AND est-op.dept    EQ "PR",
    FIRST mach NO-LOCK
    {sys/ref/machW.i}
      AND mach.m-code EQ est-op.m-code
    BY est-op.line.
  j = j + 1.
  FOR EACH tt-ink WHERE tt-ink.pass EQ j,
      FIRST ink
      WHERE ink.i-code EQ tt-ink.i-code
        AND ink.i-dscr EQ tt-ink.i-dscr
        AND ink.snum   EQ est-op.s-num:
    ink.i-qty = ink.i-qty + mach.ink-waste.
  END.
END.

for each ink with frame ab down no-labels no-box:
   find first item
       where item.company eq cocode
/*          and item.loc     eq locode 01291503 */
         and item.i-no    eq ink.i-code
       no-lock no-error.
   if avail item then
   find first e-item
       where e-item.company eq cocode
         and e-item.i-no    eq item.i-no
       no-lock no-error.
   else next.
   if ink.i-qty < item.min-lbs then ink.i-qty = item.min-lbs.

   {est/matcost.i ink.i-qty ink.i-cost ink}

   ASSIGN
    ink.i-cost = (ink.i-cost * ink.i-qty) + lv-setup-ink
    dm-tot[4]  = dm-tot[4] + (ink.i-cost / (qty / 1000))
    dm-tot[5]  = dm-tot[5] + ink.i-cost.

   find first brd where brd.form-no = xeb.form-no and
                        brd.blank-no = xeb.blank-no and
                        brd.i-no    = ink.i-code
                        no-error.
   if not avail brd then
   do:
      create brd.
      assign brd.form-no = xeb.form-no
             brd.blank-no = xeb.blank-no
             brd.i-no    = ink.i-code
             brd.dscr    = ink.i-dscr
             brd.basis-w = item.basis-w.
   end.
   ASSIGN
   brd.qty = brd.qty + ink.i-qty
   brd.qty-uom = "LB"
   brd.sc-uom  = "LB"
   brd.cost = ink.i-cost / ink.i-qty
   brd.cost-m = ink.i-cost / (qty / 1000).

   display ink.i-dscr
           ink.i-qty  format ">>>>>9.99" to 48 space(0) " LB"
           lv-setup-ink when lv-setup-ink ne 0 format ">>>9.99" to 59
           ink.i-cost / (qty / 1000) / v-sqft-fac format ">>>>9.99" to 68
           ink.i-cost format ">>>>,>>9.99" to 80 skip with stream-io.
    
end.

if xeb.adhesive ne "" and xeb.lin-in ne 0 then
do with frame ab2 down no-labels no-box:
   find first item
       where item.company eq cocode
         and item.i-no    eq xeb.adhesive
       no-lock no-error.
   if avail item then find first e-item of item no-lock no-error.
   else leave.
   for each est-op
       where est-op.company = xest.company 
         and est-op.est-no eq xest.est-no
         and est-op.qty   eq v-op-qty 
         and est-op.s-num eq xef.form-no
         and est-op.line  ge 500
       no-lock,
       
       first mach
       {sys/look/machW.i}
         and mach.m-code   eq est-op.m-code
         and (mach.dept[1] eq "GL" or mach.dept[2] eq "GL" or
              mach.dept[3] eq "GL" or mach.dept[4] eq "GL")
       no-lock
       by est-op.line
       by est-op.op-pass:
     leave.  
   end.
   
   g-qty = 0.
   if avail mach and avail est-op THEN
     g-qty = round((xeb.lin-in * (est-op.num-sh * xeb.num-up * v-n-out)) /
             item.linin-lb,2).

   {est/matcost.i g-qty g-cost adhesive}

   ASSIGN
    g-cost    = (g-cost * g-qty) + lv-setup-adhesive
    dm-tot[4] = dm-tot[4] + (g-cost / (qty / 1000))
    dm-tot[5] = dm-tot[5] + g-cost.

   find first brd where brd.form-no = xeb.form-no and
                        brd.blank-no = xeb.blank-no and
                        brd.i-no    = xeb.adhesive
                        no-error.
   if not avail brd then do:
      create brd.
      assign brd.form-no = xeb.form-no
             brd.blank-no = xeb.blank-no
             brd.i-no    = xeb.adhesive
             brd.dscr    = item.est-dscr
             brd.basis-w = item.basis-w.
   end.
   ASSIGN
   brd.qty = brd.qty + g-qty
   brd.qty-uom = "LB"
   brd.sc-uom = "LB"
   brd.cost = g-cost / g-qty
   brd.cost-m = g-cost / (qty / 1000)
   vqty = string(g-qty,">>>>>9.<<")
   vqty = fill(" ",6 - length(trim(vqty))) + trim(vqty).

   display item.i-name
           vqty format "x(6)" to 48 " LB"
           lv-setup-adhesive when lv-setup-adhesive ne 0 format ">>>9.99" to 59
           g-cost / (qty / 1000) / v-sqft-fac format ">>>>9.99" to 68
           g-cost format ">>>>,>>9.99" to 80 skip with stream-io.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
