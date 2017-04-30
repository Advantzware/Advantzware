/* --------------------------------------------------- ce/pr4-mfl.p  7/92 cd  */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
DEF SHARED VAR qty AS INT NO-UNDO.

{ce/print4.i shared shared}

def var rm-wt$ as de NO-UNDO.
def var rm-wt% as de NO-UNDO.
def var rm-wt  as de NO-UNDO.
def var med-qty as de NO-UNDO.
def var mfl$ as de format ">>>>9.99" NO-UNDO.
def var mqty as de NO-UNDO.
DEF VAR med-len LIKE xef.nsh-len NO-UNDO.
DEF VAR med-wid LIKE xef.nsh-wid NO-UNDO.
def var m-waste as de NO-UNDO.
def var m-spo   as de NO-UNDO.
def var prev-nsh as de NO-UNDO.
def var prev-mach as ch NO-UNDO.
def var v-nsh-sqft as dec no-undo.
DEF VAR call_id AS RECID NO-UNDO.
DEF VAR ll-has-an-LM AS LOG NO-UNDO.
DEF VAR ld-rm AS DEC NO-UNDO.
DEF VAR ld-hp AS DEC NO-UNDO.
DEF VAR v-qty AS DEC NO-UNDO.
DEF VAR v-end-qty AS DEC NO-UNDO.
DEF VAR v-lm-sheets AS DEC NO-UNDO.
DEFINE VARIABLE dShrink AS DECIMAL     NO-UNDO.

DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

IF xeb.pur-man THEN
  ASSIGN
   ld-rm = rm-rate-f
   ld-hp = hand-pct-f.
ELSE
  ASSIGN
   ld-rm = ctrl[3]
   ld-hp = ctrl[2].

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

assign prev-nsh = qty / (xeb.num-up * xef.n-out)
       v-nsh-sqft = (xef.nsh-len * xef.nsh-wid) / 12.
{sys/inc/roundup.i prev-nsh}
ll-has-an-LM = FALSE.
for each est-op FIELDS(LINE dept op-sb num-sh op-waste)
    where est-op.company eq xest.company
      and est-op.est-no  eq xest.est-no
      and est-op.qty     eq v-op-qty
      and est-op.line    ge 500
    by est-op.line descending:

   IF prev-mach EQ "LM" OR est-op.dept EQ "LM" THEN
       ll-has-an-LM = TRUE.
   IF prev-mach EQ "LM" AND est-op.dept NE "LM" THEN LEAVE.

   if est-op.op-sb
   then spo = est-op.num-sh - (prev-nsh + est-op.op-waste).
   else spo = est-op.num-sh - (prev-nsh + (est-op.op-waste / (xeb.num-up * xef.n-out))).
   m-spo   = m-spo   + spo.
   {sys/inc/roundup.i m-spo}
   if est-op.op-sb then m-waste = m-waste + est-op.op-waste.
   else m-waste = m-waste + (est-op.op-waste / (xeb.num-up * xef.n-out)).
   IF est-op.dept EQ "LM" THEN DO:       
       v-lm-sheets = est-op.num-sh.
   END.
   ASSIGN
      prev-nsh = est-op.num-sh
      prev-mach = est-op.dept.
end.
mqty = qty / (xeb.num-up * xef.n-out). /* number of sheets */
{sys/inc/roundup.i mqty}
{sys/inc/roundup.i m-waste}

if xef.medium ne "" then
do  WITH STREAM-IO no-box no-labels frame med1:
   find first item {sys/look/itemW.i} and
                     item.i-no = xef.medium no-lock no-error.
   if available item then DO:
        find first e-item of item no-lock no-error.
        dShrink = ITEM.shrink.
   END.
   /*override item shrink % with shrink entered in BOM button on Layout screen*/
   IF xef.spare-dec-1 NE 0 THEN dShrink = xef.spare-dec-1. 

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
    dm-tot[3] = dm-tot[3] + ((mfl$ / mqty ) * m-waste)
    dm-tot[4] = dm-tot[4] + (mfl$ / (qty / 1000))
    dm-tot[4] = dm-tot[4] + ((mfl$ / mqty) * m-waste) / (qty / 1000)
    dm-tot[5] = dm-tot[5] + mfl$
    dm-tot[5] = dm-tot[5] + ((mfl$ / mqty ) * m-waste)

    /* add run spoil */
    dm-tot[4] = dm-tot[4] + ((m-spo * (mfl$ / mqty)) / (qty / 1000)).
    dm-tot[5] = dm-tot[5] + ( m-spo * (mfl$ / mqty) ).

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
      BRD.qty-uom = ITEM.cons-uom
      BRD.sc-uom = b-uom
      BRD.cost = b-msh
      BRD.cost-m = mfl$ / (qty / 1000)
      BRD.len  = med-len
      BRD.wid  = med-wid.
      
      IF ITEM.mat-type = "P" AND ll-has-an-LM AND brd.qty-uom = "LF" THEN DO:
        RUN jc/calc-jobq.p (INPUT ROWID(xest), INPUT brd.i-no, INPUT "LM", OUTPUT v-qty, OUTPUT v-end-qty).
/*            v-qty / v-end-qty adjusts to the run-qty based on the estimate */
/*            original qty. Eg. may originally have been 100,000 but is now  */
/*            being set to 90,000.                                           */
        DEF VAR abc AS DEC.
        abc = v-op-qty * v-qty / v-end-qty.
        
        brd.qty = v-lm-sheets * brd.len / 12.
        
      END.
      IF ITEM.mat-type = "P" OR ITEM.mat-type = "B" THEN
          {sys/inc/roundup.i brd.qty}.

      
   display
         xef.medium
         item.basis-w to 27
         item.cal format ">9.999<" to 35
         "$" + string(b-msh,">>>9.99") to 48 b-uom space(0)
         mfl$ / (qty / 1000) format ">>>>9.99" to 68
         mfl$ format ">,>>>,>>9.99" to 80 skip
         "Medium MR  Waste"
         m-waste format ">>>>>>9" to 48 "Sht" space(0)
         (mfl$ / mqty ) * m-waste format ">>>9.99" to 59
         /* cost of waste amortized per 1000 of all blanks on this form */
         ((mfl$ / mqty) * m-waste) / (qty / 1000)
            format ">>>>9.99" to 68
         (mfl$ / mqty) * m-waste format ">,>>>,>>9.99" to 80 skip
         "Medium RUN Waste"
         m-spo format ">>>>>>9" to 48 "Sht" space(0)
         /* cost of spoil amortized per 1000 of all blanks on this form */
         (m-spo * (mfl$ / mqty)) / (qty / 1000)
                                              format ">>>>9.99" to 68
         m-spo * (mfl$ / mqty ) format ">,>>>,>>9.99" to 80 skip.
         lctr = lctr + 3.

   /* rm handling chg per cwt*/
   if ld-rm ne 0 then
      ASSIGN
         rm-wt = (med-qty + ((m-waste * v-nsh-sqft) / 1000) +
                 ((m-spo * v-nsh-sqft) / 1000)) * item.basis-w
         rm-wt$ = (rm-wt / 100) * ld-rm
         ctrl2[3] = ctrl2[3] + rm-wt$.
   
   /* rm handling pct. */
   if ld-hp ne 0 then
      ASSIGN
         rm-wt% = ( mfl$ +
                  ((mfl$ / mqty) * m-waste) +
                  ((mfl$ / mqty) * m-spo  ) )
         ctrl2[2] = ctrl2[2] + (rm-wt% * ld-hp).
   
end.

if xef.flute ne "" then
do WITH STREAM-IO no-box no-labels frame flute:
   find first item {sys/look/itemW.i} and
                     item.i-no = xef.flute no-lock no-error.
   if available item then
   find first e-item of item no-lock no-error.
   ASSIGN
      adh-qty[2] = 1
      med-qty = ((xef.nsh-len * (IF item.i-code EQ "R" THEN ITEM.r-wid ELSE xef.nsh-wid) ) * mqty) / 144000 /*now msf*/
      fg-wt = fg-wt + (fg-qty * item.basis-w).

   FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

   b-uom = IF item.i-code EQ "E" OR (AVAIL e-item /*AND vprint*/) THEN e-item.std-uom
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
      BRD.qty-uom = /* "Sht" */ ITEM.cons-uom
      BRD.sc-uom = b-uom
      BRD.cost = b-msh
      BRD.cost-m = mfl$ / (qty / 1000)
      BRD.len  = xef.nsh-len
      BRD.wid  = /*xef.nsh-wid */ ITEM.r-wid.

   IF ITEM.mat-type = "P" AND ll-has-an-LM AND brd.qty-uom = "LF" THEN DO:

     RUN jc/calc-jobq.p (INPUT ROWID(xest), INPUT brd.i-no, INPUT "LM", OUTPUT v-qty, OUTPUT v-end-qty).
/*            v-qty / v-end-qty adjusts to the run-qty based on the estimate */
/*            original qty. Eg. may originally have been 100,000 but is now  */
/*            being set to 90,000.                                           */
            
      /* brd.qty = v-op-qty * v-qty / v-end-qty * brd.len / 12. */
                     brd.qty = v-lm-sheets * brd.len / 12.
       
   END.
   IF ITEM.mat-type = "P" OR ITEM.mat-type = "B" THEN
       {sys/inc/roundup.i brd.qty}.
   display
         xef.flute
         item.basis-w to 27
         item.cal format ">9.999<" to 35
         "$" + string(b-msh,">>>>9.99") to 48 b-uom space(0)
         mfl$ / (qty / 1000) format ">>>>9.99" to 68
         mfl$ format ">,>>>,>>9.99" to 80 skip
         tmpstore + "MR  Waste" format "x(15)"
         m-waste format ">>>>>>9" to 48 space(0) " Sht" space(0)
         (mfl$ / mqty) * m-waste format ">>>9.99" to 59
         /* cost of waste amortized per 1000 of all blanks on this form */
         ((mfl$ / mqty) * m-waste) / (qty / 1000)
            format ">>>>9.99" to 68
         (mfl$ / mqty) * m-waste format ">,>>>,>>9.99" to 80 skip
         tmpstore + "RUN Waste" format "x(15)"
         m-spo format ">>>>>>9" to 48 space(0) " Sht" space(0)
         /* cost of spoil amortized per 1000 of all blanks on this form */
         (m-spo * (mfl$ / mqty)) / (qty / 1000) format ">>>>9.99" to 68
         m-spo * (mfl$ / mqty) format ">,>>>,>>9.99" to 80 skip.
         lctr = lctr + 3.

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
                  ((mfl$ / mqty) * m-waste) +
                  ((mfl$ / mqty) * m-spo  ) )
         ctrl2[2] = ctrl2[2] + (rm-wt% * ld-hp).
end.

/*
if xef.lam-code ne "" then
do WITH STREAM-IO no-box no-labels frame lamin:
   find first item {sys/look/itemW.i} and
                     item.i-no = xef.lam-code no-lock no-error.
   if available item then
   find first e-item of item no-lock no-error.
   adh-qty[3] = 1.
   fg-wt = fg-wt + (fg-qty * item.basis-w).
   med-qty = ((xef.nsh-len * xef.nsh-wid) * mqty) / 144000. /*now msf*/

   FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

   b-uom = IF item.i-code EQ "E" OR (AVAIL e-item AND vprint) THEN e-item.std-uom
                                                              ELSE item.cons-uom.

   IF b-uom EQ "TON" THEN med-qty = med-qty * item.basis-w / 2000.

   {est/matcost.i med-qty mfl$ lam-code}

   ASSIGN
    mfl$      = (mfl$ * med-qty) + lv-setup-lam-code
    b-msh     = mfl$ / med-qty
    dm-tot[3] = dm-tot[3] + ((mfl$ / mqty) * m-waste)
    dm-tot[4] = dm-tot[4] + (mfl$ / (save-qty / 1000))
    dm-tot[5] = dm-tot[5] + mfl$.

   /* add run spoil */
   dm-tot[4] = dm-tot[4] + m-spo * (((mfl$ / mqty)) / (save-qty / 1000)).
   dm-tot[5] = dm-tot[5] + (m-spo * (mfl$ / mqty)).

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
   BRD.qty = (qty / (xeb.num-up * xef.n-out)) + m-waste + m-spo.
   BRD.qty-uom = "Sht".
   BRD.sc-uom = "MSH".
   BRD.cost = mfl$ / (qty / 1000).
   BRD.cost-m = mfl$ / (qty / 1000).
   BRD.len  = xef.nsh-len.
   BRD.wid  = xef.nsh-wid.

   display
         xef.lam-code
         /* item.basis-w to 27
         item.cal format ">9.999<" to 35 */
         "$" + string(b-msh,">>>>9.99") to 48 b-uom space(0)
         mfl$ / (qty / 1000) format ">>>>9.99" to 68
         mfl$ format ">,>>>,>>9.99" to 80 skip
         "Laminate MR  Waste"
         m-waste format ">>>>>>9" to 48 "Sht" space(0)
         (mfl$ / mqty) * m-waste format ">>>9.99" to 59
         /* cost amortized per 1000 of all blanks on this form */
         ((mfl$ / mqty) * m-waste) / (qty / 1000)
            format ">>>>9.99" to 68
         (mfl$ / mqty) * m-waste format ">,>>>,>>9.99" to 80 skip
         "Laminate RUN Waste"
         m-spo format ">>>>>>9" to 48 "Sht" space(0)
         /* cost amortized per 1000 of all blanks on this form */
         (m-spo * (mfl$ / mqty)) / (qty / 1000)
                                              format ">>>>9.99" to 68
         m-spo * (mfl$ / mqty) format ">,>>>,>>9.99" to 80 skip.
         lctr = lctr + 3.

   /* rm handling chg per cwt*/
   if ld-rm ne 0 then
   do:
      rm-wt = (med-qty + ((m-waste * v-nsh-sqft) / 1000) +
                       ((m-spo   * v-nsh-sqft) / 1000)) * item.basis-w.
      rm-wt$ = (rm-wt / 100) * ld-rm.
      ctrl2[3] = ctrl2[3] + rm-wt$.
   end.
   /* rm handling pct. */
   if ld-hp ne 0 then
   do:
      rm-wt% = ( mfl$ +
               ((mfl$ / mqty) * m-waste) +
               ((mfl$ / mqty) * m-spo  ) ).
      ctrl2[2] = ctrl2[2] + (rm-wt% * ld-hp).
   end.
end.
*/
call_id = recid(item).

/* end ---------------------------------- copr. 1992  advanced software, inc. */
