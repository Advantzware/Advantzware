/* ----------------------------------------------- ce/com/pr4-mfl.p  7/92 cd  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
DEF SHARED VAR qty AS INT NO-UNDO.

{cec/print4.i shared shared}

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
DEF VAR ld-rm-rate AS DEC NO-UNDO.
DEF VAR ld-hand-pct AS DEC NO-UNDO.

DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

find first ce-ctrl {sys/look/ce-ctrl.w} no-lock no-error.

assign
    call_id = recid(xeb)
    m-spo   = lm-spo
    m-waste = lm-waste
    xxx     = lm-qty
    mqty    = t-shtfrm[xef.form-no].

if xef.lam-dscr = "R"
   then assign sh-wid = xef.gsh-wid
               sh-len = xef.gsh-len.
   else assign sh-wid = xef.gsh-len
               sh-len = xef.gsh-wid.

ASSIGN
   cumul = mqty
   spo   = 0.

for each est-op 
    where est-op.company eq xest.company
      and est-op.est-no  eq xest.est-no
      and est-op.s-num = xef.form-no
      and est-op.line  > 500 by est-op.line
    descending:
   find first mach {sys/look/mach.w} and mach.m-code = est-op.m-code
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
   else
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
   find first item {sys/look/item.w} and
                     item.i-no = xef.medium no-lock no-error.
   if available item then
   find first e-item of item no-lock no-error.

   if xef.n-out-l = 0 THEN
      ASSIGN
      med-len = sh-len
      med-wid = sh-wid / (1 - (item.shrink / 100)). /* proper length */
   ELSE
      ASSIGN
      med-wid = sh-wid
      med-len = sh-len / (1 - (item.shrink / 100)). /* proper length */
   
   med-qty = ((med-wid * med-len) * mqty) / 144000. /*now msf*/

   FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

   b-uom = IF AVAIL e-item AND e-item.std-uom NE "" THEN e-item.std-uom
                                                    ELSE item.cons-uom.

   IF b-uom EQ "TON" THEN med-qty = med-qty * item.basis-w / 2000.

   {est/matcost.i med-qty mfl$ medium}

   ASSIGN
    mfl$      = (mfl$ * med-qty) + lv-setup-medium
    b-msh     = mfl$ / med-qty
    dm-tot[3] = dm-tot[3] + ((mfl$ / mqty) * m-waste)
    dm-tot[5] = dm-tot[5] + mfl$.

   /* add run spoil & waste */
   dm-tot[5] = dm-tot[5] + (m-spo * (mfl$ / mqty)) + (m-waste * (mfl$ / mqty)).

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
   BRD.qty = (qty / xeb.num-up) + m-waste + m-spo
   BRD.qty-uom = "Sht"
   BRD.sc-uom = "MSH"
   BRD.cost = mfl$ / (qty / 1000)
   BRD.cost-m = mfl$ / (qty / 1000)
   brd.len  = med-len
   brd.wid  = med-wid.

   display
     /*  string(xef.form-no,"9") + "-00" format "x(4)" */
         space(5)
         xef.medium
         item.basis-w to 32
         item.cal format ">9.999<" to 39
         "$" + string(mfl$ / (mqty / 1000),">>>9.99") to 50
                space(0) " MShts" space(0)
         mfl$ / ((mqty * t-blksht[xef.form-no]) / 1000)
                                                  format ">>>9.99" to 69
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
   ld-rm-rate = IF xeb.pur-man THEN rm-rate-f ELSE ctrl[3].
   if ld-rm-rate ne 0 then
      ASSIGN
      rm-wt = (med-qty + ((m-waste * brd-sf[2]) / 1000) +
                       ((m-spo * brd-sf[2]) / 1000)) * item.basis-w
      rm-wt$ = (rm-wt / 100) * ld-rm-rate
      ctrl2[3] = ctrl2[3] + rm-wt$.
   
   /* rm handling pct. */
   ld-hand-pct = IF xeb.pur-man THEN hand-pct-f ELSE ctrl[2].
   if ld-hand-pct ne 0 THEN
      ASSIGN
      rm-wt% = ( mfl$ +
               ((mfl$ / mqty ) * m-waste) +
               ((mfl$ / mqty ) * m-spo) )
      ctrl2[2] = ctrl2[2] + (rm-wt% * ld-hand-pct).
   
   for each xeb of xef:
      find first blk where blk.snum = xeb.form-no and
                           blk.bnum = xeb.blank-no and
                           blk.id   = xeb.part-no no-error.
      if available blk then do:
         if rm-wt$ ne 0 then blk.lab = blk.lab + (rm-wt$ * blk.pct).
         if rm-wt% ne 0 then blk.lab = blk.lab + (rm-wt% * blk.pct).
         blk.cost = blk.cost + ((mfl$ + ((mfl$ / mqty) * m-waste) +
                                        (m-spo * (mfl$ / mqty))   +
                                         rm-wt$ + rm-wt%) * blk.pct).
      end.
   end.
end.
find xeb where recid(xeb) = call_id no-lock no-error.

if xef.flute ne "" then
DO WITH STREAM-IO no-box no-labels frame flute:
   find first item {sys/look/item.w} and
                     item.i-no = xef.flute no-lock no-error.
   if available item then
   find first e-item of item no-lock no-error.
   med-qty = ((sh-wid * sh-len) * mqty) / 144000.   /* msf */
   
   FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

   b-uom = IF AVAIL e-item AND e-item.std-uom NE "" THEN e-item.std-uom
                                                    ELSE item.cons-uom.

   IF b-uom EQ "TON" THEN med-qty = med-qty * item.basis-w / 2000.

   {est/matcost.i med-qty mfl$ flute}

   ASSIGN
    mfl$      = (mfl$ * med-qty) + lv-setup-flute
    b-msh     = mfl$ / med-qty
    dm-tot[3] = dm-tot[3] + ( (mfl$ / mqty) * m-waste)
    dm-tot[5] = dm-tot[5] + mfl$
    /* add run spoil */
    dm-tot[5] = dm-tot[5] + (m-spo   * (mfl$ / mqty)) +
                            (m-waste * (mfl$ / mqty)).

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
   BRD.qty = (qty / xeb.num-up) + m-waste + m-spo
   BRD.qty-uom = "Sht"
   BRD.sc-uom = "MSH"
   BRD.cost = mfl$ / (qty / 1000)
   BRD.cost-m = mfl$ / (qty / 1000)
   BRD.len  = brd-l[3]
   BRD.wid  = brd-w[3].

   display
      /*   string(xef.form-no,"9") + "-00" format "x(4)" */
         space(5)
         xef.flute
         item.basis-w to 32
         item.cal format ">9.999<" to 39
         "$" + string(mfl$ / (mqty / 1000),">>>9.99") to 50
                space(0) " MShts" space(0)
         mfl$ /
            ((mqty * t-blksht[xef.form-no]) / 1000) format ">>>9.99" to 69
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
   ld-rm-rate = IF xeb.pur-man THEN rm-rate-f ELSE ctrl[3].
   if ld-rm-rate ne 0 then
      ASSIGN
      rm-wt = (med-qty + ((m-waste * brd-sf[2]) / 1000) +
                       ((m-spo * brd-sf[2]) / 1000)) * item.basis-w
      rm-wt$ = (rm-wt / 100) * ld-rm-rate
      ctrl2[3] = ctrl2[3] + rm-wt$.
   
   /* rm handling pct. */
   ld-hand-pct = IF xeb.pur-man THEN hand-pct-f ELSE ctrl[2].
   if ld-hand-pct ne 0 THEN
      ASSIGN
      rm-wt% = ( mfl$ +
               ((mfl$ / mqty ) * m-waste) +
               ((mfl$ / mqty ) * m-spo) )
      ctrl2[2] = ctrl2[2] + (rm-wt% * ld-hand-pct).
   
   for each xeb of xef:
      find first blk where blk.snum = xeb.form-no and
                           blk.bnum = xeb.blank-no and
                           blk.id   = xeb.part-no no-error.
      if available blk then do:
         if rm-wt$ ne 0 then blk.lab = blk.lab + (rm-wt$ * blk.pct).
         if rm-wt% ne 0 then blk.lab = blk.lab + (rm-wt% * blk.pct).
         blk.cost = blk.cost + ((mfl$ + ((mfl$ / mqty) * m-waste) +
                                        ((mfl$ / mqty) * m-spo)   +
                                         rm-wt$ + rm-wt%) * blk.pct).
      end.
   end.
end.
find xeb where recid(xeb) = call_id no-lock no-error.
/*
if xef.lam-code ne "" then
DO WITH STREAM-IO no-box no-labels frame lamin:
   find first item {sys/look/item.w} and
                     item.i-no = xef.lam-code no-lock no-error.
   if available item then
   find first e-item of item no-lock no-error.
   med-qty = ((sh-wid * sh-len) * mqty) / 144000.
  
   FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

   b-uom = IF AVAIL e-item AND e-item.std-uom NE "" THEN e-item.std-uom
                                                    ELSE item.cons-uom.

   IF b-uom EQ "TON" THEN med-qty = med-qty * item.basis-w / 2000.

   {est/matcost.i med-qty mfl$ lam-code}

   ASSIGN
    mfl$      = (mfl$ * med-qty) + lv-setup-lam-code
    b-msh     = mfl$ / med-qty
    dm-tot[3] = dm-tot[3] + ( (mfl$ / mqty) * m-waste)
    dm-tot[5] = dm-tot[5] + mfl$.

   /* add run spoil */
   dm-tot[5] = dm-tot[5] + (m-spo   * (mfl$ / mqty)) +
                           (m-waste * (mfl$ / mqty)).

   find first BRD where BRD.form-no = xef.form-no and
                        BRD.i-no    = xef.lam-code
                        no-error.
   if not available BRD then
   do:
      create BRD.
      assign BRD.form-no = xef.form-no
             BRD.blank-no = 00
             BRD.i-no    = xef.lam-code
             BRD.dscr    = xef.lam-code
             BRD.basis-w = item.basis-w.
   end.
   BRD.qty = (qty / xeb.num-up) + m-waste + m-spo.
   BRD.qty-uom = "Sht".
   BRD.sc-uom = "MSH".
   BRD.cost = mfl$ / (qty / 1000).
   BRD.cost-m = mfl$ / (qty / 1000).
   BRD.len  = brd-l[3].
   BRD.wid  = brd-w[3].

   display
     /*    string(xef.form-no,"9") + "-00" format "x(4)" */
         space(5)
         xef.lam-code
         /* item.basis-w to 32
         item.cal format ">9.999<" to 39 */
         "$" + string(mfl$ / (mqty / 1000),">>>9.99") to 50
                space(0) " MShts" space(0)
         mfl$ /
           ((mqty * t-blksht[xef.form-no]) / 1000) format ">>>9.99" to 69
         mfl$ format ">>>>,>>9.99" to 80 skip
         space(5)
         "Laminate MR  Waste"
         m-waste format ">>>>9" to 50 space(0) " Sht" space(0)
         (mfl$ / mqty) * m-waste format ">>>9.99" to 61
         /* cost amortized per 1000 of all blanks on this form */
         ((mfl$ / mqty) * m-waste) /
            ((mqty * t-blksht[xef.form-no]) / 1000) format ">>>9.99" to 69
         (mfl$ / mqty) * m-waste format ">>>>,>>9.99" to 80 skip
         space(5)
         "Laminate RUN Waste"
         m-spo format ">>>>9" to 50 space(0) " Sht" space(0)
         /* cost amortized per 1000 of all blanks on this form */
         (m-spo * (mfl$ / mqty)) /
            ((mqty * t-blksht[xef.form-no]) / 1000) format ">>>9.99" to 69
         m-spo * (mfl$ / mqty) format ">>>>,>>9.99" to 80 skip.

   /* rm handling chg per cwt*/
   ld-rm-rate = IF xeb.pur-man THEN rm-rate-f ELSE ctrl[3].
   if ld-rm-rate ne 0 then
   do:
      rm-wt = (med-qty + ((m-waste * brd-sf[2]) / 1000) +
                       ((m-spo * brd-sf[2]) / 1000)) * item.basis-w.
      rm-wt$ = (rm-wt / 100) * ld-rm-rate.
      ctrl2[3] = ctrl2[3] + rm-wt$.
   end.
   /* rm handling pct. */
   ld-hand-pct = IF xeb.pur-man THEN hand-pct-f ELSE ctrl[2].
   if ld-hand-pct ne 0 then do:
      rm-wt% = ( mfl$ +
               ((mfl$ / mqty ) * m-waste) +
               ((mfl$ / mqty ) * m-spo) ).
      ctrl2[2] = ctrl2[2] + (rm-wt% * ld-hand-pct).
   end.
   for each xeb of xef:
      find first blk where blk.snum = xeb.form-no and
                           blk.bnum = xeb.blank-no and
                           blk.id   = xeb.part-no no-error.
      if available blk then do:
         if rm-wt$ ne 0 then blk.lab = blk.lab + (rm-wt$ * blk.pct).
         if rm-wt% ne 0 then blk.lab = blk.lab + (rm-wt% * blk.pct).
         blk.cost = blk.cost + ((mfl$ + ((mfl$ / mqty) * m-waste) +
                                        ((mfl$ / mqty) * m-spo)   +
                                         rm-wt$ + rm-wt%) * blk.pct).
      end.
   end.
end.
*/
find xeb where recid(xeb) = call_id no-lock no-error.
call_id = recid(item).

/* end ---------------------------------- copr. 1992  advanced software, inc. */
