/* -------------------------------------------------- cec/pr4-mfl.p  7/92 cd  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def input parameter v-vend-no like e-item-vend.vend-no.
def input parameter v-add-to-est as log.
DEF INPUT PARAMETER hld-qty AS DEC NO-UNDO.

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def shared var qty as int NO-UNDO.
def var j as int no-undo.
def var zzz as int no-undo.
DEF VAR tmpstore AS CHAR NO-UNDO.
DEF VAR CALL_id AS RECID NO-UNDO.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{cec/print4.i shared shared}

DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

def var rm-wt$ as de NO-UNDO.
def var rm-wt% as de NO-UNDO.
def var rm-wt  as de NO-UNDO.
def var mfl$ as de format ">>>>9.99" NO-UNDO.
def var med-qty as de NO-UNDO. 
def var cumul as de NO-UNDO.
def var prev-nsh as de NO-UNDO.
def var prev-mach as ch NO-UNDO.
def var mqty as de NO-UNDO.
def var m-waste as de NO-UNDO.
def var m-spo   as de NO-UNDO.
def var v-on-f  as INT NO-UNDO.
def var v-qty   like med-qty NO-UNDO.
DEF VAR vuom AS CHAR NO-UNDO.
DEF VAR gqty AS DEC NO-UNDO.
DEF VAR gcost AS DEC NO-UNDO.

DEF TEMP-TABLE w-brd NO-UNDO LIKE brd FIELD rm-qty LIKE v-qty.

DEF VAR tot-qty LIKE w-brd.qty NO-UNDO.
DEF VAR tot-cst LIKE w-brd.cost NO-UNDO.
DEF VAR tot-c-m LIKE w-brd.cost-m NO-UNDO.
DEF VAR ld-rm-rate AS DEC NO-UNDO.
DEF VAR vqty AS CHAR NO-UNDO.
DEF VAR v-liner-qty LIKE w-brd.qty NO-UNDO.
DEF VAR v-medium-qty LIKE w-brd.qty NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

{cec/msfcalc.i}

{cec/rollfac.i}

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

find first est-op
    where est-op.company eq xest.company
      and est-op.est-no  eq xest.est-no
      and est-op.qty     eq v-op-qty
      and est-op.line    gt 500
      and est-op.dept    eq "CR"
    no-lock.

run sys/inc/numout.p (recid(est-op), output v-on-f).

assign
 tot-qty = qty /* / xeb.num-up / v-on-f */
 mqty    = tot-qty - r-spo[xef.form-no] - spo
 m-spo   = r-spo[xef.form-no]
 m-waste = spo.
 
{sys/inc/roundup.i mqty}

item-bom-loop:
repeat:

find next item-bom
    where item-bom.company  eq xef.company
      and item-bom.parent-i eq xef.board
      AND item-bom.line# LT 9
    no-lock no-error.

if not(avail(item-bom)) then leave item-bom-loop.

/*** if xef.medium ne "" then  ***/ /* CTS */
/** even lines are medium types **/
if (item-bom.line# mod 2) eq 0 then
do with no-box no-labels frame med1 STREAM-IO:
   
   find first item {sys/look/itemW.i} and
                     item.i-no = item-bom.i-no no-lock no-error.
   if avail item then
   find first e-item of item no-lock no-error.
   if not(avail(item)) then leave.

   assign
    mfl$  = 0
    b-msh = 0
    v-medium-qty = 0
    adh-qty[1] = 1.

   RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

   IF xeb.num-up * v-n-out NE 0 THEN
      v-medium-qty = hld-qty / (xeb.num-up * v-n-out).

   ASSIGN
      med-qty = if v-corr then ((brd-l[3] * brd-w[3]) * v-medium-qty / (1 - (item-bom.shrink / 100))) * .000007
                else ((brd-l[3] * brd-w[3]) * v-medium-qty / (1 - (item-bom.shrink / 100))) / 144000
      fg-wt = fg-wt + ((fg-qty / (1 - (item-bom.shrink / 100))) * item.basis-w).

   FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

   b-uom = IF AVAIL e-item AND e-item.std-uom NE "" THEN e-item.std-uom
                                                    ELSE item.cons-uom.

   IF b-uom EQ "LF"  THEN
     v-qty = (IF v-corr THEN (med-qty / .000007)
              ELSE (med-qty * 144000)) / brd-w[3] / 12.
   ELSE
   IF b-uom EQ "TON" THEN
     v-qty = med-qty * item.basis-w / 2000.
   ELSE
   IF b-uom EQ "MSF" THEN
     v-qty = med-qty.
   ELSE
     v-qty = med-qty * item.basis-w.

   {est/matcost.i v-qty mfl$ medium}

   create w-brd.
   assign b-msh = mfl$
          mfl$  = (b-msh * v-qty) + lv-setup-medium
          w-brd.form-no  = xef.form-no
          w-brd.blank-no = 1
          w-brd.i-no     = item-bom.i-no
          w-brd.dscr     = xef.medium
          w-brd.basis-w  = item.basis-w
          w-brd.len      = brd-l[3] / (1 - (item-bom.shrink / 100))
          w-brd.wid      = brd-w[3]
          w-brd.cost = mfl$
          w-brd.qty = qty
          w-brd.cost-m = v-qty
          w-brd.qty-uom = "EA"
          w-brd.sc-uom = b-uom.

   if v-add-to-est then do:
     assign
      dm-tot[3] = dm-tot[3] + ( (mfl$ / mqty ) * m-waste)
      dm-tot[4] = dm-tot[4] + (mfl$ / (save-qty / 1000))
      dm-tot[5] = dm-tot[5] + mfl$
      /* add run spoil */
      dm-tot[4] = dm-tot[4] + ( (m-spo * (mfl$ / mqty)) / (save-qty / 1000))
      dm-tot[5] = dm-tot[5] + ( m-spo * (mfl$ / mqty) ).

     display
         item.i-name format "x(20)"
         item.basis-w when avail(item) to 27
         item.cal when avail(item) format ">9.999<" to 35
         "$" + string(b-msh,">>>9.99") to 48 b-uom space(0)
         lv-setup-medium when lv-setup-medium ne 0 format ">>>9.99" to 59
         mfl$ / (save-qty / 1000) / v-sqft-fac format ">>>>9.99" to 68
         mfl$ format ">>>>,>>9.99" to 80 skip
         "Medium MR  Waste"
         m-waste format ">>>>>>9" to 48 "Sht" space(0)
         (mfl$ / mqty ) * m-waste format ">>>9.99" to 59
         /* cost of waste amortized per 1000 of all blanks on this form */
         ((mfl$ / mqty) * m-waste) / (save-qty / 1000) / v-sqft-fac
            format ">>>>9.99" to 68
         (mfl$ / mqty) * m-waste format ">>>>,>>9.99" to 80 skip
         "Medium RUN Waste"
         m-spo format ">>>>>>9" to 48 "Sht" space(0)
         /* cost of spoil amortized per 1000 of all blanks on this form */
         (m-spo * (mfl$ / mqty)) / (save-qty / 1000) / v-sqft-fac
                                              format ">>>>9.99" to 68
         m-spo * (mfl$ / mqty ) format ">>>>,>>9.99" to 80 skip.

     /* rm handling chg per cwt*/
     ld-rm-rate = IF xeb.pur-man THEN rm-rate-f ELSE ctrl[3].
     if ld-rm-rate gt 0 THEN
       ASSIGN
       rm-wt = (med-qty + ((m-waste * brd-sf[3]) / 1000) +
                          ((m-spo * brd-sf[3]) / 1000)) *
                          (if avail(item) then item.basis-w else 1)
       rm-wt$ = (rm-wt / 100) * ld-rm-rate
       ctrl2[3] = ctrl2[3] + rm-wt$.
     
   end.
end.

/*** if xef.flute ne "" then ***/
/*** Odd number lines are liners **/
if (item-bom.line# mod 2) ne 0 then
do with no-box no-labels frame flute STREAM-IO:
   assign
    mfl$  = 0
    b-msh = 0
    v-liner-qty = 0.

   find first item {sys/look/itemW.i} and
                     item.i-no = item-bom.i-no no-lock no-error.
   if avail item then
   find first e-item of item no-lock no-error.
   if not(avail(item)) then leave.

   RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

   IF xeb.num-up * v-n-out NE 0 THEN
      v-liner-qty = hld-qty / (xeb.num-up * v-n-out).

   ASSIGN
      adh-qty[2] = 1
      med-qty = if v-corr then ((brd-l[3] * brd-w[3]) * v-liner-qty) * .000007
                          else ((brd-l[3] * brd-w[3]) * v-liner-qty) / 144000
      fg-wt = fg-wt + (fg-qty * (if avail(item) then item.basis-w else 1)).

   FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

   b-uom = IF AVAIL e-item AND e-item.std-uom NE "" THEN e-item.std-uom
                                                    ELSE item.cons-uom.

   IF b-uom EQ "LF"  THEN
      v-qty = (IF v-corr THEN (med-qty / .000007)
               ELSE (med-qty * 144000)) / brd-w[3] / 12.
   ELSE
   IF b-uom EQ "TON" THEN
      v-qty = med-qty * item.basis-w / 2000.
   ELSE
   IF b-uom EQ "MSF" THEN
      v-qty = med-qty.
   ELSE
      v-qty = med-qty * item.basis-w.

   {est/matcost.i v-qty mfl$ flute}

   ASSIGN
    b-msh = mfl$
    mfl$  = (b-msh * v-qty) + lv-setup-flute.

   create w-brd.
   assign w-brd.form-no = xef.form-no
          w-brd.blank-no = 1
          w-brd.i-no    = item-bom.i-no
          w-brd.dscr    = xef.medium
          w-brd.basis-w = item.basis-w
          w-brd.len     = brd-l[3] / (1 - (item-bom.shrink / 100))
          w-brd.wid     = brd-w[3]
          w-brd.cost = mfl$
          w-brd.qty = qty
          w-brd.cost-m = v-qty
          w-brd.qty-uom = "EA"
          w-brd.sc-uom = b-uom
          /*mfl$ = mfl$ / qty * mqty*/
          tmpstore = "Liner ".

   if v-add-to-est then do:
     assign
      dm-tot[3] = dm-tot[3] + ( (mfl$ / mqty) * m-waste )
      dm-tot[4] = dm-tot[4] + (mfl$ / (save-qty / 1000))
      dm-tot[5] = dm-tot[5] + mfl$
      /* add run spoil */
      dm-tot[4] = dm-tot[4] + ( (m-spo * (mfl$ / mqty)) / (save-qty / 1000))
      dm-tot[5] = dm-tot[5] + (m-spo * (mfl$ / mqty)).
      
     display
         item.i-name format "x(20)"
         item.basis-w when avail(item) to 27
         item.cal when avail(item) format ">9.999<" to 35
         "$" + string(b-msh,">>>9.99") to 48 b-uom space(0)
         lv-setup-flute when lv-setup-flute ne 0 format ">>>9.99" to 59
         mfl$ / (save-qty / 1000) / v-sqft-fac format ">>>>9.99" to 68
         mfl$ format ">>>>,>>9.99" to 80 skip
         tmpstore + "MR  Waste" format "x(15)"
         m-waste format ">>>>>>9" to 48 space(0) " Sht" space(0)
         (mfl$ / mqty) * m-waste format ">>>9.99" to 59
         /* cost of waste amortized per 1000 of all blanks on this form */
         ((mfl$ / mqty) * m-waste) / (save-qty / 1000) / v-sqft-fac
            format ">>>>9.99" to 68
         (mfl$ / mqty) * m-waste format ">>>>,>>9.99" to 80 skip
         tmpstore + "RUN Waste" format "x(15)"
         m-spo format ">>>>>>9" to 48 space(0) " Sht" space(0)
         /* cost of spoil amortized per 1000 of all blanks on this form */
         (m-spo * (mfl$ / mqty)) / (save-qty / 1000) / v-sqft-fac
            format ">>>>9.99" to 68
         m-spo * (mfl$ / mqty) format ">>>>,>>9.99" to 80 skip.

     /* rm handling chg per cwt*/
     ld-rm-rate = IF xeb.pur-man THEN rm-rate-f ELSE ctrl[3].
     if ld-rm-rate gt 0 then
        ASSIGN
           rm-wt = (med-qty + ((m-waste * brd-sf[3]) / 1000) +
                              ((m-spo * brd-sf[3]) / 1000)) *
                              (if avail(item) then item.basis-w else 1)
           rm-wt$ = (rm-wt / 100) * ld-rm-rate
           ctrl2[3] = ctrl2[3] + rm-wt$.
   end. /*end v-add-to-est*/
end. /*end frame flute*/

end. /*end repeat*/

if v-add-to-est AND
   xef.adh-code ne "" then DO with no-box no-labels frame adh-frame STREAM-IO:
   find first item where
        item.company = cocode and
        item.i-no    = xef.adh-code
        no-lock no-error.

   if available item then
   DO:
      vuom = item.cons-uom.

      IF ITEM.sqin-lb NE 0 THEN
         gqty = xef.adh-sqin * est-op.num-sh / item.sqin-lb.

      find first e-item of item no-lock no-error.

      IF AVAIL e-item THEN
      DO:
         EMPTY TEMP-TABLE tt-ei.
         CREATE tt-ei.
         DO j = 1 TO 10:
            ASSIGN
               tt-ei.run-qty[j] = e-item.run-qty[j]
               tt-ei.run-cost[j] = e-item.run-cost[j].
         END.
         
                  
            DO j = 1 TO 10:
               ASSIGN
                  tt-ei.run-qty[j + 10] = e-item.runQty[j]
                  tt-ei.run-cost[j + 10] = e-item.runCost[j].
            END.
      
         DO j = 1 TO 20:
            IF tt-ei.run-qty[j] LT gqty THEN NEXT.
               gcost = gqty * tt-ei.run-cost[j].
               IF e-item.std-uom NE "" THEN vuom = e-item.std-uom.
            LEAVE.
         END.
      END.

      ELSE gcost = gqty * IF ce-ctrl.r-cost THEN item.avg-cost
                                            ELSE item.last-cost.

      ASSIGN
         dm-tot[4] = dm-tot[4] + (gcost / (save-qty / 1000) / v-sqft-fac)
         dm-tot[5] = dm-tot[5] + gcost.

      /* rm handling chg per cwt */
      IF xeb.pur-man THEN
        IF rm-rate-f NE 0 THEN ctrl2[3] = ctrl2[3] + ((gqty / 100) * rm-rate-f).
        ELSE.
      ELSE
        IF ctrl[3] NE 0 THEN ctrl2[3] = ctrl2[3] + ((gqty / 100) * ctrl[3]).
     
      /* rm handling pct. */
      IF xeb.pur-man THEN
        IF hand-pct-f NE 0 THEN ctrl2[2] = ctrl2[2] + (gcost * hand-pct-f).
        ELSE.
      ELSE
        IF ctrl[2] NE 0 THEN ctrl2[2] = ctrl2[2] + (gcost * ctrl[2]).

      ASSIGN
         vqty = string(gqty,">>>>>9.99")
         vqty = fill(" ",9 - length(trim(vqty))) + trim(vqty).

      display item.i-name
           vqty                     format "x(9)"           to 48
           vuom
           gcost / (save-qty / 1000 / v-sqft-fac) format ">>>>9.99"  to 68
           gcost                     format ">,>>>,>>9.99"   to 80 SKIP.
   END.
end.

if v-add-to-est AND
   xef.lam-code ne "" then DO with no-box no-labels frame lam-frame STREAM-IO:
   find first item where
        item.company = cocode and
        item.i-no    = xef.lam-code
        no-lock no-error.

   if available item then
   DO:
      ASSIGN
         vuom = item.cons-uom
         gcost = 0.

      IF ITEM.sqin-lb NE 0 THEN
         gqty = xef.gsh-wid * xef.gsh-len * est-op.num-sh / item.sqin-lb.
      ELSE
         gqty = 0.

      find first e-item of item no-lock no-error.

      IF AVAIL e-item THEN
      DO:
      
         EMPTY TEMP-TABLE tt-ei.
         CREATE tt-ei.
         DO j = 1 TO 10:
            ASSIGN
               tt-ei.run-qty[j] = e-item.run-qty[j]
               tt-ei.run-cost[j] = e-item.run-cost[j].
         END.
         
                  
            DO j = 1 TO 10:
               ASSIGN
                  tt-ei.run-qty[j + 10] = e-item.runQty[j]
                  tt-ei.run-cost[j + 10] = e-item.runCost[j].
            END.

         DO j = 1 TO 20:
            IF tt-ei.run-qty[j] LT gqty THEN NEXT.
               gcost = gqty * tt-ei.run-cost[j].
               IF e-item.std-uom NE "" THEN vuom = e-item.std-uom.
               LEAVE.
         END.
      END.
      ELSE gcost = gqty * IF ce-ctrl.r-cost THEN item.avg-cost
                                            ELSE item.last-cost.

      ASSIGN
         dm-tot[4] = dm-tot[4] + (gcost / (save-qty / 1000) / v-sqft-fac)
         dm-tot[5] = dm-tot[5] + gcost.

      /* rm handling chg per cwt */
      IF xeb.pur-man THEN
        IF rm-rate-f NE 0 THEN ctrl2[3] = ctrl2[3] + ((gqty / 100) * rm-rate-f).
        ELSE.
      ELSE
        IF ctrl[3] NE 0 THEN ctrl2[3] = ctrl2[3] + ((gqty / 100) * ctrl[3]).
     
      /* rm handling pct. */
      IF xeb.pur-man THEN
        IF hand-pct-f NE 0 THEN ctrl2[2] = ctrl2[2] + (gcost * hand-pct-f).
        ELSE.
      ELSE
        IF ctrl[2] NE 0 THEN ctrl2[2] = ctrl2[2] + (gcost * ctrl[2]).

      ASSIGN
         vqty = string(gqty,">>>>>9.99")
         vqty = fill(" ",9 - length(trim(vqty))) + trim(vqty).

      display item.i-name
           vqty  format "x(9)" to 48
           vuom
           gcost / (save-qty / 1000 / v-sqft-fac) format ">>>>9.99" to 68
           gcost format ">,>>>,>>9.99"   to 80 SKIP.
   END.
end.

tot-qty = 0.

FOR EACH w-brd BREAK BY w-brd.form-no BY w-brd.i-no BY w-brd.len BY w-brd.wid:
  ASSIGN
   tot-qty = tot-qty + w-brd.qty
   tot-cst = tot-cst + w-brd.cost
   tot-c-m = tot-c-m + w-brd.cost-m.

  IF LAST-OF(w-brd.wid) THEN DO:
    CREATE brd.
    BUFFER-COPY w-brd TO brd
    ASSIGN
     brd.cost   = tot-cst / tot-c-m
     brd.cost-m = tot-cst / (save-qty / 1000)
     brd.qty    = tot-qty
     tot-qty    = 0
     tot-cst    = 0
     tot-c-m    = 0.
  END.
END.

call_id = if avail(item) then recid(item) else ?.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
