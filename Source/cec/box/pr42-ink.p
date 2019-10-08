/* ----------------------------------------------- cec/box/pr42-ink.p 2/93 cd */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared var qty as int NO-UNDO.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
DEF BUFFER b-eb2 FOR eb.

def var vuom like item.cons-uom no-undo.
DEF VAR v-n-out AS INT NO-UNDO.

{cec/print4.i shared shared}
{cec/print42.i shared}

def input parameter v-vend-no like e-item-vend.vend-no.
DEFINE INPUT PARAMETER TABLE FOR tt-all-forms-ink.

define TEMP-TABLE glu NO-UNDO
   field id as ch
   field snum as int
   field bnum as int
   field i-code as ch  format "x(10)"
   field i-dscr as ch  format "x(19)"
   field i-%    as int format ">>9"
   field i-qty  as de  format ">>>9.99"
   field i-cost as de  format ">>,>>9.99".

define buffer b-ink for ink.
define buffer b-glu for glu.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

def var iqty as de no-undo.
def var icost as de no-undo.
def var ipct as de no-undo.
def var gqty as de no-undo.
def var gcost as de no-undo.
def var gpct as de no-undo.
def var rm-wt$ as de no-undo.
def var rm-wt% as de no-undo.
DEF VAR ld-rm-rate AS DEC NO-UNDO.
DEF VAR v-tmp-qty AS DEC NO-UNDO.
def var v-yld as DEC NO-UNDO.
DEF VAR v-first-pass AS LOG NO-UNDO.

DEF TEMP-TABLE tt-ink NO-UNDO FIELD i-code LIKE ink.i-code
                              FIELD i-dscr LIKE ink.i-dscr
                              FIELD pass AS INT.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

/* i n k s */

for each xef where xef.company = xest.company
               and xef.est-no    eq xest.est-no
               and (xef.form-no eq v-form-no or (not vmclean2)):

   RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

   for each xeb where xeb.company = xest.company 
                  and xeb.est-no = xest.est-no and xeb.form-no = xef.form-no
       BREAK BY xeb.form-no:

      {cec/pr4-ink.i}

      find first item where item.company = cocode and
                            item.i-no    = xeb.adhesive no-lock no-error.
      if avail item then find first e-item of item no-lock no-error.
      else next.
      if xeb.adhesive eq "" or xeb.lin-in eq 0 then next.

      find first glu where glu.i-code = item.i-no    and
                              glu.snum   = xeb.form-no  and
                              glu.bnum   = xeb.blank-no no-error.
      if not avail glu then do:
         create glu.
         assign
            glu.id     = xeb.part-no
            glu.snum   = xeb.form-no
            glu.bnum   = xeb.blank-no
            glu.i-code = xeb.adhesive
            glu.i-dscr = item.est-dscr.
      end.

      RELEASE est-op.

      for each est-op
          where est-op.company eq xest.company 
            and est-op.est-no eq xest.est-no
            and est-op.qty   eq v-op-qty 
            and est-op.s-num eq xef.form-no
            and est-op.line  ge 500
          no-lock,
       
          first mach
          {sys/look/machW.i}
            AND mach.m-code   eq est-op.m-code
            and (mach.dept[1] eq "GL" or mach.dept[2] eq "GL" or
                 mach.dept[3] eq "GL" or mach.dept[4] eq "GL")
          no-lock
          by est-op.line
          by est-op.op-pass:
        leave.  
      end.

      glu.i-qty = glu.i-qty +
                  ((IF AVAIL est-op THEN (est-op.num-sh * xeb.num-up * v-n-out)
                                    ELSE qty) *
                   xeb.lin-in / item.linin-lb).
   end.

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
       BY est-op.LINE:

     j = j + 1.
     FOR EACH tt-ink WHERE tt-ink.pass EQ j,
         FIRST ink
         WHERE ink.i-code EQ tt-ink.i-code
           AND ink.i-dscr EQ tt-ink.i-dscr
           AND ink.snum   EQ est-op.s-num:

         ink.i-qty = ink.i-qty + mach.ink-waste.
     END.
   END.
end.

for each ink WHERE
   break by ink.i-code
   with frame ab down no-labels no-box  stream-io:

   IF NOT(NOT vmclean2 AND PROGRAM-NAME(4) EQ "jc/jc-calc.p") AND
      not first-of(ink.i-code) then
      next.

   find first item where item.company = cocode and
                         item.i-no    = ink.i-code no-lock no-error.
   if avail item then find first e-item of item no-lock no-error.
   else next.

   ASSIGN
      iqty  = 0
      v-tmp-qty = 0.

   for each b-ink where b-ink.i-code = ink.i-code:
       iqty = iqty + b-ink.i-qty.
   end.

   IF v-do-all-forms-ink EQ NO THEN
      v-tmp-qty = iqty.
   ELSE
   DO:
      FIND FIRST tt-all-forms-ink WHERE
           tt-all-forms-ink.i-code = ink.i-code
           NO-ERROR.

      IF AVAIL tt-all-forms-ink THEN
         v-tmp-qty = tt-all-forms-ink.i-qty.
   END.

   if iqty < item.min-lbs then iqty = item.min-lbs.
   IF v-tmp-qty < ITEM.min-lbs THEN v-tmp-qty = ITEM.min-lbs.

   {est/matcost.i v-tmp-qty icost ink}

   IF NOT vmclean2 AND PROGRAM-NAME(4) EQ "jc/jc-calc.p" THEN
      iqty = ink.i-qty.

   IF v-do-all-forms-ink AND AVAIL tt-all-forms-ink AND
      tt-all-forms-ink.i-qty NE 0 THEN
      lv-setup-ink = lv-setup-ink * ( iqty / tt-all-forms-ink.i-qty).

   ASSIGN
    icost     = (icost * iqty) + lv-setup-ink
    dm-tot[5] = dm-tot[5] + icost.

   find first brd where brd.form-no = ink.snum and
                        brd.blank-no = ink.bnum and
                        brd.i-no    = ink.i-code
                        no-error.
   if not avail brd then
   do:
      create brd.
      assign brd.form-no = ink.snum
             brd.blank-no = ink.bnum
             brd.i-no    = ink.i-code
             brd.dscr    = ink.i-dscr
             brd.basis-w = item.basis-w.
   end.

   ASSIGN
      brd.qty = brd.qty + iqty
      brd.qty-uom = "LB"
      brd.sc-uom  = "LB"
      brd.cost = icost / iqty
      brd.cost-m = icost / (tt-blk / 1000)
      v-yld = 1.

   FIND FIRST b-eb2 WHERE
        b-eb2.company EQ cocode AND
        b-eb2.est-no EQ xest.est-no AND
        b-eb2.form-no EQ ink.snum AND
        b-eb2.blank-no EQ ink.bnum
        NO-LOCK NO-ERROR.

   IF AVAIL b-eb2 THEN
      v-yld = if b-eb2.quantityPerSet lt 0 then -1 / b-eb2.quantityPerSet else b-eb2.quantityPerSet.

   display "*" space(4)
           ink.i-dscr
           iqty  to 50 space(0) " LB"
           lv-setup-ink when lv-setup-ink ne 0 format ">>>9.99" to 63
           icost / (tt-blk * (if xest.form-qty eq 1 or vmclean2 then v-yld else 1) / 1000) to 71 format ">>>>9.99"
           icost to 80 format ">>>>>9.99" /*skip*/
       with stream-io.

   ASSIGN                         
      iqty  = 0
      lin-count = lin-count + 1.

   for each b-ink where
       b-ink.i-code = ink.i-code:
       iqty = iqty + b-ink.i-qty.
   end.

   for each b-ink where
       b-ink.i-code = ink.i-code:
       find first blk where blk.id = b-ink.id and
                         blk.snum = b-ink.snum and
                         blk.bnum = b-ink.bnum no-error.
      /* assign proper cost share of this ink to blk record */
      IF iqty NE 0 AND iqty NE ? THEN
        ASSIGN
         blk.cost = blk.cost + ((b-ink.i-qty / iqty) * icost)
         b-ink.i-cost = (b-ink.i-qty / iqty) * icost.

      /* rm handling chg per cwt */
      ASSIGN
       ld-rm-rate  = IF blk.pur-man THEN rm-rate-f ELSE ctrl[3]
       ctrl2[3]    = ctrl2[3] + ((b-ink.i-qty / 100) * ld-rm-rate)
       blk.cost    = blk.cost + ((b-ink.i-qty / 100) * ld-rm-rate)
       blk.lab     = blk.lab + ((b-ink.i-qty / 100) * ld-rm-rate).
   end.
end.

for each glu break by glu.i-code /*BY glu.snum*/ with frame abc  stream-io down no-labels no-box:
   
   IF NOT(NOT vmclean2 AND PROGRAM-NAME(4) EQ "jc/jc-calc.p") AND 
      not first-of(glu.i-code) /*AND NOT FIRST-OF(glu.snum)*/ then
      next.  /* 05191405 */

   find first item where item.company = cocode and
                         item.i-no    = glu.i-code no-lock no-error.
   if avail item then find first e-item of item no-lock no-error.
   else next.

   gqty  = 0.
   for each b-glu where b-glu.i-code = glu.i-code /*AND xglu.snum EQ glu.snum*/:
      gqty = gqty + b-glu.i-qty.   /* total pounds */
   end.

   {est/matcost.i gqty gcost glue}

   IF NOT vmclean2 AND PROGRAM-NAME(4) EQ "jc/jc-calc.p" THEN
      gqty = glu.i-qty.

   ASSIGN
    gcost     = (gcost * gqty) + lv-setup-glue
    dm-tot[5] = dm-tot[5] + gcost.

   find first brd where brd.form-no = glu.snum and
                        brd.blank-no = glu.bnum and
                        brd.i-no    = glu.i-code
                        no-error.
   if not avail brd then
   do:
      create brd.
      assign brd.form-no = glu.snum
             brd.blank-no = glu.bnum
             brd.i-no    = glu.i-code
             brd.dscr    = glu.i-dscr
             brd.basis-w = item.basis-w.
   end.
   ASSIGN
      brd.qty = brd.qty + gqty
      brd.qty-uom = "LB"
      brd.sc-uom =  "LB"
      brd.cost = gcost / gqty
      brd.cost-m = gcost / (tt-blk / 1000)
      v-yld = 1.

   FIND FIRST b-eb2 WHERE
        b-eb2.company EQ cocode AND
        b-eb2.est-no EQ xest.est-no AND
        b-eb2.form-no EQ glu.snum AND
        b-eb2.blank-no EQ glu.bnum
        NO-LOCK NO-ERROR.

   IF AVAIL b-eb2 THEN
      v-yld = if b-eb2.quantityPerSet lt 0 then -1 / b-eb2.quantityPerSet else b-eb2.quantityPerSet.

   display "*" space(4)
           item.i-name
           gqty to 50 "LB"
           lv-setup-glue when lv-setup-glue ne 0 format ">>>9.99" to 63
           gcost / (tt-blk * (if xest.form-qty eq 1 or vmclean2 then v-yld else 1) / 1000) to 71 format ">>>>9.99"
           gcost to 80 format ">>>>>9.99" SKIP
       WITH STREAM-IO.

   for each b-glu where b-glu.i-code = glu.i-code:
      find first blk where blk.id = b-glu.id and
                         blk.snum = b-glu.snum and
                         blk.bnum = b-glu.bnum no-error.
      /* assign proper cost share of this glu to blk record */
      IF gqty NE 0 AND gqty NE ? THEN
        ASSIGN
         blk.cost = blk.cost + ((b-glu.i-qty / gqty) * gcost)
         b-glu.i-cost = (b-glu.i-qty / gqty) * gcost.

      /* rm handling chg per cwt and pct */
      ASSIGN
       ld-rm-rate  = IF blk.pur-man THEN rm-rate-f ELSE ctrl[3]
       ctrl2[3]    = ctrl2[3] + ((b-glu.i-qty / 100) * ld-rm-rate)
       blk.cost    = blk.cost + ((b-glu.i-qty / 100) * ld-rm-rate)
       blk.lab     = blk.lab + ((b-glu.i-qty / 100) * ld-rm-rate).
   end.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
