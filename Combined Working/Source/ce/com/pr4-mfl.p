/* ----------------------------------------------- ce/com/pr4-mfl.p  7/92 cd  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
DEF SHARED VAR qty AS INT NO-UNDO.

{ce/print4.i shared shared}

def var lm-spo   as de NO-UNDO.
def var lm-waste as de NO-UNDO.
def var lm-qty as de NO-UNDO.

def var m-spo   as de NO-UNDO.
def var m-waste as de NO-UNDO.
def var mqty as de NO-UNDO.
DEF VAR med-len LIKE xef.nsh-len NO-UNDO.
DEF VAR med-wid LIKE xef.nsh-wid NO-UNDO.
def var cumul as de NO-UNDO.
def var prev-mach as ch NO-UNDO.
def var med-qty as de NO-UNDO.
def var mfl$ as de format ">>>>9.99" NO-UNDO.
def var rm-wt$ as de NO-UNDO.
def var rm-wt% as de NO-UNDO.
def var rm-wt  as de NO-UNDO.
DEF VAR call_id AS RECID NO-UNDO.
DEF VAR ld-rm AS DEC NO-UNDO.
DEF VAR ld-hp AS DEC NO-UNDO.
def var v-nsh-sqft as dec no-undo.
DEFINE VARIABLE dShrink AS DECIMAL     NO-UNDO.

DEF SHARED VAR gEstSummaryOnly AS LOG NO-UNDO.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

assign
    call_id = recid(xeb)
    m-spo   = lm-spo
    m-waste = lm-waste
    xxx     = lm-qty
    mqty    = t-shtfrm[xef.form-no]
    v-nsh-sqft = (xef.nsh-len * xef.nsh-wid) / 12.

if xef.lam-dscr = "R"
   then assign sh-wid = xef.gsh-wid
               sh-len = xef.gsh-len.
   else assign sh-wid = xef.gsh-len
               sh-len = xef.gsh-wid.

find first eb
    where eb.company  eq xef.company
      and eb.est-no   eq xef.est-no 
      and eb.form-no  eq xef.form-no
    no-lock no-error.

IF eb.pur-man THEN
  ASSIGN
   ld-rm = rm-rate-f
   ld-hp = hand-pct-f.
ELSE
  ASSIGN
   ld-rm = ctrl[3]
   ld-hp = ctrl[2].

ASSIGN
   cumul = mqty
   spo   = 0.

for each est-op 
    where est-op.company eq xest.company
      and est-op.est-no  eq xest.est-no
      and est-op.s-num = xef.form-no
      and est-op.line  > 500 by est-op.line
    descending:
   find first mach {sys/look/machW.i} and mach.m-code = est-op.m-code
   no-lock no-error.
   find first eb
       where eb.company  eq est-op.company
         and eb.est-no   eq est-op.est-no 
         and eb.form-no  eq est-op.s-num
         and eb.blank-no eq est-op.b-num
       no-lock no-error.
   if prev-mach = "LM" and mach.dept[1] ne "LM" then leave.
   if est-op.op-sb = no THEN
      ASSIGN
         spo     = (cumul / (1 - (est-op.op-spoil / 100))) - cumul
         m-spo   = m-spo   + spo
         m-waste = m-waste +       (est-op.op-waste / eb.num-up)
         cumul   = cumul   + spo + (est-op.op-waste / eb.num-up).
   ELSE
      ASSIGN
         spo     = cumul / (1 - (est-op.op-spoil / 100)) - cumul
         m-spo   = m-spo + spo
         m-waste = m-waste + est-op.op-waste
         cumul   = cumul + spo + est-op.op-waste.
   
   prev-mach = mach.dept[1].
end.
/*mqty = cumul.*/

{sys/inc/roundup.i mqty}
{sys/inc/roundup.i m-waste}
{sys/inc/roundup.i m-spo}

if xef.medium ne "" then
DO WITH STREAM-IO no-box no-labels frame med1:
   find first item {sys/look/itemW.i} and
                     item.i-no = xef.medium no-lock no-error.
   if available item THEN DO:
       find first e-item of item no-lock no-error.
       /*override item shrink % with shrink entered in BOM button on Layout screen*/
          IF xef.spare-dec-1 NE 0 
              THEN dShrink = xef.spare-dec-1.
              ELSE dShrink = ITEM.shrink.
   END.
   

   adh-qty[1] = 1.

   if xef.n-out-l = 0 THEN
      ASSIGN
      med-len = xef.nsh-len
      med-wid = (IF item.i-code EQ "R" THEN ITEM.r-wid ELSE xef.nsh-wid) / (1 - (dShrink / 100)). /* proper length */
   ELSE
      ASSIGN
      med-len = xef.nsh-len / (1 - (dShrink / 100)) /* proper length */
      med-wid = (IF item.i-code EQ "R" THEN ITEM.r-wid ELSE xef.nsh-wid).
   
   ASSIGN
      med-qty = (( med-wid * med-len) * mqty) / 144000 /*now msf*/
      fg-wt = fg-wt + ((fg-qty / (1 - (dShrink / 100))) * item.basis-w).

   FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

   b-uom = IF AVAIL e-item AND e-item.std-uom NE "" THEN e-item.std-uom
                                                    ELSE item.cons-uom.

   IF b-uom EQ "TON" THEN med-qty = med-qty * item.basis-w / 2000.

   {est/matcost.i med-qty mfl$ medium}

   ASSIGN
    mfl$      = (mfl$ * med-qty) + lv-setup-medium
    b-msh     = mfl$ / med-qty
    dm-tot[3] = dm-tot[3] + ((mfl$ / mqty) * m-waste)
    dm-tot[4] = dm-tot[4] + (mfl$ / (qty / 1000))
    dm-tot[4] = dm-tot[4] + ((mfl$ / mqty) * m-waste) / (qty / 1000)
    dm-tot[5] = dm-tot[5] + mfl$
    dm-tot[5] = dm-tot[5] + ((mfl$ / mqty ) * m-waste)

   /* add run spoil & waste */
   dm-tot[4] = dm-tot[4] + ((m-spo * (mfl$ / mqty)) / (qty / 1000)).
   dm-tot[5] = dm-tot[5] + (m-spo * (mfl$ / mqty)).

   find first BRD where BRD.form-no = xef.form-no and
                        BRD.i-no    = xef.medium
                        no-error.
   if not available BRD then
   do:
      create BRD.
      assign BRD.form-no = xef.form-no
             BRD.blank-no = 00
             BRD.i-no    = xef.medium
             BRD.dscr    = xef.medium
             BRD.basis-w = item.basis-w.
   end.
   ASSIGN
      BRD.qty = (qty / (xeb.num-up * xef.n-out)) /*+ m-waste + m-spo*/
      BRD.qty-uom = "Sht"
      BRD.sc-uom = b-uom
      BRD.cost = b-msh
      BRD.cost-m = mfl$ / (qty / 1000)
      brd.len  = med-len
      brd.wid  = med-wid
      brd.qty-mr = m-waste
      brd.qty-wst = m-spo
      brd.amount = m-spo * (mfl$ / mqty)
      .

   IF NOT gEstSummaryOnly THEN
   display
         space(5)
         xef.medium
         item.basis-w to 32
         item.cal format ">9.999<" to 39
         "$" + string(b-msh,">>>9.99") to 50
                space(1) b-uom space(0)
         mfl$ / (qty / 1000) format ">>>9.99" to 69
         mfl$ format ">>>>,>>9.99" to 80 skip
         space(5)
         "Medium MR  Waste"
         m-waste format ">>>>9" to 50 space(0) " Sht" space(0)
         (mfl$ / mqty) * m-waste format ">>>9.99" to 61
         /* cost of waste amortized per 1000 of all blanks on this form */
         ((mfl$ / mqty) * m-waste) /
            ((mqty * t-blksht[xef.form-no]) / 1000) format ">>>9.99" to 69
         (mfl$ / mqty) * m-waste format ">>>>,>>9.99" to 80 skip
         space(5)
         "Medium RUN Waste"
         m-spo format ">>>>9" to 50 space(0) " Sht" space(0)
         /* cost of spoil amortized per 1000 of all blanks on this form */
         (m-spo * (mfl$ / mqty)) /
            ((mqty * t-blksht[xef.form-no]) / 1000) format ">>>9.99" to 69
         m-spo * (mfl$ / mqty) format ">>>>,>>9.99" to 80 skip.


   /* rm handling chg per cwt*/
   if ld-rm ne 0 then
      ASSIGN
         rm-wt = (med-qty + ((m-waste * v-nsh-sqft) / 1000) +
                 ((m-spo * v-nsh-sqft) / 1000)) * item.basis-w
         rm-wt$ = (rm-wt / 100) * ld-rm
         ctrl2[3] = ctrl2[3] + rm-wt$.
   
   /* rm handling pct. */
   if ld-hp ne 0 THEN
      ASSIGN
         rm-wt% = ( mfl$ +
                  ((mfl$ / mqty ) * m-waste) +
                  ((mfl$ / mqty ) * m-spo) )
         ctrl2[2] = ctrl2[2] + (rm-wt% * ld-hp).
   
   for each xeb of xef:
      find first blk where blk.snum = xeb.form-no and
                           blk.bnum = xeb.blank-no and
                           blk.id   = xeb.part-no no-error.
      if available blk then do:
         if ld-rm ne 0 then blk.lab = blk.lab + (rm-wt$ * blk.pct).
         if ld-hp ne 0 then blk.lab = blk.lab + (rm-wt% * blk.pct).
         blk.cost = blk.cost + ((mfl$ + ((mfl$ / mqty) * m-waste) +
                                        (m-spo * (mfl$ / mqty))   +
                                         rm-wt$ + rm-wt%) * blk.pct).
      end.
   end.
end.
find xeb where recid(xeb) = call_id no-lock no-error.

if xef.flute ne "" then
DO WITH STREAM-IO no-box no-labels frame flute:
   find first item {sys/look/itemW.i} and
                     item.i-no = xef.flute no-lock no-error.
   if available item then
   find first e-item of item no-lock no-error.
   ASSIGN
      adh-qty[2] = 1
      med-qty = ((xef.nsh-len * ITEM.r-wid) * mqty) / 144000 /*now msf*/
      fg-wt = fg-wt + (fg-qty * item.basis-w).
   
   FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

   b-uom = IF item.i-code EQ "E" OR AVAIL e-item THEN e-item.std-uom
                                                 ELSE item.cons-uom.

   IF b-uom EQ "TON" THEN med-qty = med-qty * item.basis-w / 2000.

   {est/matcost.i med-qty mfl$ flute}

   ASSIGN
    mfl$      = (mfl$ * med-qty) + lv-setup-flute
    b-msh     = mfl$ / med-qty
    dm-tot[3] = dm-tot[3] + ((mfl$ / mqty) * m-waste)
    dm-tot[4] = dm-tot[4] + (mfl$ / (qty / 1000))
    dm-tot[4] = dm-tot[4] + ((mfl$ / mqty) * m-waste) / (qty / 1000)
    dm-tot[5] = dm-tot[5] + mfl$
    dm-tot[5] = dm-tot[5] + ((mfl$ / mqty) * m-waste)

   /* add run spoil */
   dm-tot[4] = dm-tot[4] + ((m-spo * (mfl$ / mqty)) / (qty / 1000))
   dm-tot[5] = dm-tot[5] + (m-spo * (mfl$ / mqty)).

   if xef.medium = "" then tmpstore = "Flute ".
   else tmpstore = "Liner ".

   find first BRD where BRD.form-no = xef.form-no and
                        BRD.i-no    = xef.flute
                        no-error.
   if not available BRD then
   do:
      create BRD.
      assign BRD.form-no = xef.form-no
             BRD.blank-no = 00
             BRD.i-no    = xef.flute
             BRD.dscr    = xef.flute
             BRD.basis-w = item.basis-w.
   end.
   ASSIGN
      BRD.qty = (qty / (xeb.num-up * xef.n-out)) /*+ m-waste + m-spo*/
      BRD.qty-uom = "Sht"
      BRD.sc-uom = b-uom
      BRD.cost = b-msh
      BRD.cost-m = mfl$ / (qty / 1000)
      BRD.len  = xef.nsh-len
      BRD.wid  = xef.nsh-wid.

   display
         space(5)
         xef.flute
         item.basis-w to 32
         item.cal format ">9.999<" to 39
         "$" + string(b-msh,">>>9.99") to 50
                space(1) b-uom space(0)
         mfl$ / (qty / 1000) format ">>>9.99" to 69
         mfl$ format ">>>>,>>9.99" to 80 skip
         space(5)
         tmpstore + "MR  Waste" format "x(15)"
         m-waste format ">>>>9" to 50 space(0) " Sht" space(0)
         (mfl$ / mqty) * m-waste format ">>>9.99" to 61
         /* cost of waste amortized per 1000 of all blanks on this form */
         ((mfl$ / mqty) * m-waste) /
           ((mqty * t-blksht[xef.form-no]) / 1000) format ">>>9.99" to 69
         (mfl$ / mqty) * m-waste format ">>>>,>>9.99" to 80 skip
         space(5)
         tmpstore + "RUN Waste" format "x(15)"
         m-spo format ">>>>9" to 50 space(0) " Sht" space(0)
         /* cost of spoil amortized per 1000 of all blanks on this form */
         m-spo * (mfl$ / mqty) /
            ((mqty * t-blksht[xef.form-no]) / 1000) format ">>>9.99" to 69
         m-spo * (mfl$ / mqty) format ">>>>,>>9.99" to 80 skip.

   /* rm handling chg per cwt*/
   if ld-rm ne 0 then
      ASSIGN
         rm-wt = (med-qty + ((m-waste * v-nsh-sqft) / 1000) +
                  ((m-spo   * v-nsh-sqft) / 1000)) * item.basis-w
         rm-wt$ = (rm-wt / 100) * ld-rm
         ctrl2[3] = ctrl2[3] + rm-wt$.
   
   /* rm handling pct. */
   if ld-hp ne 0 then
      ASSIGN
         rm-wt% = ( mfl$ +
                  ((mfl$ / mqty ) * m-waste) +
                  ((mfl$ / mqty ) * m-spo) )
         ctrl2[2] = ctrl2[2] + (rm-wt% * ld-hp).
   
   for each xeb of xef:
      find first blk where blk.snum = xeb.form-no and
                           blk.bnum = xeb.blank-no and
                           blk.id   = xeb.part-no no-error.
      if available blk then do:
         if ld-rm ne 0 then blk.lab = blk.lab + (rm-wt$ * blk.pct).
         if ld-hp ne 0 then blk.lab = blk.lab + (rm-wt% * blk.pct).
         blk.cost = blk.cost + ((mfl$ + ((mfl$ / mqty) * m-waste) +
                                        ((mfl$ / mqty) * m-spo)   +
                                         rm-wt$ + rm-wt%) * blk.pct).
      end.
   end.
end.

find xeb where recid(xeb) = call_id no-lock no-error.

call_id = recid(item).

/* end ---------------------------------- copr. 1992  advanced software, inc. */
