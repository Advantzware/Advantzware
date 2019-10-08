/* --------------------------------------------- cec/box/pr42-mfl.p 08/00 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def input parameter v-vend-no like e-item-vend.vend-no.
def input parameter v-add-to-est as log.

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def shared var qty as INT NO-UNDO.

def var j as int no-undo.
def var tmpstore as cha no-undo.
def var call_id as recid no-undo.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

{cec/print4.i shared shared}

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
DEF VAR vqty AS CHAR NO-UNDO. 

DEF TEMP-TABLE w-brd NO-UNDO LIKE brd FIELD rm-qty LIKE v-qty.

DEF VAR tot-qty LIKE w-brd.qty NO-UNDO.
DEF VAR tot-cst LIKE w-brd.cost NO-UNDO.
DEF VAR tot-c-m LIKE w-brd.cost-m NO-UNDO.
DEF VAR ld-rm-rate AS DEC NO-UNDO.

{cec/msfcalc.i}

{cec/rollfac.i}

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

assign
 mqty    = t-shtfrm[xef.form-no]
 m-spo   = r-spo[xef.form-no]
 m-waste = b-waste
 tot-qty = mqty + m-spo + m-waste.
 
{sys/inc/roundup.i mqty}

item-bom-loop:
repeat:

find next item-bom
    where item-bom.company  eq xef.company
      and item-bom.parent-i eq xef.board
      AND item-bom.line# LT 9
    no-lock no-error.

if not avail item-bom then leave item-bom-loop.

/*** if xef.medium ne "" then  ***/ /* CTS */
/** even lines are medium types **/
if (item-bom.line# mod 2) eq 0 then
do with no-box no-labels frame med1  stream-io :
   assign
    mfl$    = 0
    b-msh   = 0.

   find first item {sys/look/itemW.i} and
                   item.i-no = item-bom.i-no no-lock no-error.

   if not avail item then leave.
   adh-qty[1] = 1.

   if xef.n-out-l eq 0 then
     assign
      med-qty = brd-w[2] / (1 - (item-bom.shrink / 100))
      med-qty = if v-corr then ((med-qty * brd-l[2]) * tot-qty) * .000007
                          else ((med-qty * brd-l[2]) * qty) / 144000.
   else
     assign
      med-qty = brd-l[2] / (1 - (item-bom.shrink / 100))
      med-qty = if v-corr then ((med-qty * brd-w[2]) * tot-qty) * .000007
                          else ((med-qty * brd-w[2]) * tot-qty) / 144000.

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

   ASSIGN
    b-msh = mfl$
    mfl$  = (b-msh * v-qty) + lv-setup-medium.

   /*find first brd where brd.form-no = xef.form-no and
                        brd.i-no    = item-bom.i-no
                        brd.len     = brd-l[3] / (1 - (item-bom.shrink / 100))
                        brd.wid     = brd-w[3]  
                        no-error.
   if not avail brd then do:*/
      create w-brd.
      assign w-brd.form-no  = xef.form-no
             w-brd.blank-no = 1
             w-brd.i-no     = item-bom.i-no
             w-brd.dscr     = xef.medium
             w-brd.basis-w  = item.basis-w
             w-brd.len      = brd-l[3] / (1 - (item-bom.shrink / 100))
             w-brd.wid      = brd-w[3].
   /*end.*/
   ASSIGN
      w-brd.cost = mfl$
      w-brd.qty = tot-qty
      w-brd.cost-m = v-qty
      w-brd.qty-uom = "EA"
      w-brd.sc-uom = b-uom
      mfl$ = mfl$ / qty * mqty.

   if v-add-to-est then do:
     assign
      dm-tot[3] = dm-tot[3] + ( (mfl$ / mqty ) * m-waste)
      dm-tot[4] = dm-tot[4] + (mfl$ / (save-qty / 1000) / v-sqft-fac)
      dm-tot[5] = dm-tot[5] + mfl$
      /* add run spoil */
      dm-tot[4] = dm-tot[4] + ( (m-spo * (mfl$ / mqty)) / (save-qty / 1000) / v-sqft-fac)
      dm-tot[5] = dm-tot[5] + ( m-spo * (mfl$ / mqty) )
      dm-tot[5] = dm-tot[5] + ((mfl$ / mqty) * m-waste).

     display
         item.i-name format "x(25)" item.basis-w item.cal format ">9.999<"
         "$" + string(b-msh,">>>9.99") to 50 b-uom space(0)
         lv-setup-medium when lv-setup-medium ne 0 format ">>>9.99" to 61
         mfl$ / (save-qty / 1000) / v-sqft-fac format ">>>>9.99" to 69
         mfl$ format ">>>>,>>9.99" to 80 skip
         "Medium MR  Waste"
         m-waste format ">>>>>>9" to 50 "Sht" space(0)
         (mfl$ / mqty ) * m-waste format ">>>9.99" to 61
         /* cost of waste amortized per 1000 of all blanks on this form */
         ((mfl$ / mqty) * m-waste) / (save-qty / 1000) / v-sqft-fac
            format ">>>>9.99" to 69
         (mfl$ / mqty) * m-waste format ">>>>,>>9.99" to 80 skip
         "Medium RUN Waste"
         m-spo format ">>>>>>9" to 50 "Sht" space(0)
         /* cost of spoil amortized per 1000 of all blanks on this form */
         (m-spo * (mfl$ / mqty)) / (save-qty / 1000) / v-sqft-fac
                                              format ">>>>9.99" to 69
         m-spo * (mfl$ / mqty ) format ">>>>,>>9.99" to 80 SKIP WITH STREAM-IO.

     /* rm handling chg per cwt*/
     ld-rm-rate = IF xeb.pur-man THEN rm-rate-f ELSE ctrl[3].
     if ld-rm-rate ne 0 then
        ASSIGN
           rm-wt = (med-qty + ((m-waste * brd-sf[2]) / 1000) +
                              ((m-spo * brd-sf[2]) / 1000)) *
                              (if avail(item) then item.basis-w else 1)
           rm-wt$ = (rm-wt / 100) * ld-rm-rate
           ctrl2[3] = ctrl2[3] + rm-wt$.
     
   end.
end.

/*** if xef.flute ne "" then ***/
/*** Odd number lines are liners **/
if (item-bom.line# mod 2) ne 0 then
do with no-box no-labels frame flute  stream-io :
   assign
    mfl$    = 0
    b-msh   = 0.

   find first item {sys/look/itemW.i} and
                     item.i-no = item-bom.i-no no-lock no-error.

   if not avail item then leave.

   ASSIGN
      adh-qty[2] = 1
      med-qty = if v-corr then ((brd-l[2] * brd-w[2]) * tot-qty) * .000007
                       else ((brd-l[2] * brd-w[2]) * tot-qty) / 144000
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
   assign w-brd.form-no  = xef.form-no
          w-brd.blank-no = 1
          w-brd.i-no     = item-bom.i-no
          w-brd.dscr     = xef.medium
          w-brd.basis-w  = item.basis-w
          w-brd.len      = brd-l[3] / (1 - (item-bom.shrink / 100))
          w-brd.wid      = brd-w[3]
          w-brd.cost = mfl$
          w-brd.qty = tot-qty
          w-brd.cost-m = v-qty
          w-brd.qty-uom = "EA"
          w-brd.sc-uom = b-uom
          mfl$ = mfl$ / qty * mqty
          tmpstore = "Liner ".

   if v-add-to-est then do:
      assign
       dm-tot[3] = dm-tot[3] + ( (mfl$ / mqty) * m-waste )
       dm-tot[4] = dm-tot[4] + (mfl$ / (save-qty / 1000) / v-sqft-fac)
       dm-tot[5] = dm-tot[5] + mfl$
       /* add run spoil */
       dm-tot[4] = dm-tot[4] + ( (m-spo * (mfl$ / mqty)) / (save-qty / 1000) / v-sqft-fac)
       dm-tot[5] = dm-tot[5] + (m-spo * (mfl$ / mqty))
       dm-tot[5] = dm-tot[5] + ((mfl$ / mqty) * m-waste).
     
      display
         item.i-name format "x(25)" item.basis-w item.cal format ">9.999<"
         "$" + string(b-msh,">>>9.99") to 50 b-uom space(0)
         lv-setup-flute when lv-setup-flute ne 0 format ">>>9.99" to 61
         mfl$ / (save-qty / 1000) / v-sqft-fac format ">>>>9.99" to 69
         mfl$ format ">>>>,>>9.99" to 80 skip
         tmpstore + "MR  Waste" format "x(15)"
         m-waste format ">>>>>>9" to 50 space(0) " Sht" space(0)
         (mfl$ / mqty) * m-waste format ">>>9.99" to 61
         /* cost of waste amortized per 1000 of all blanks on this form */
         ((mfl$ / mqty) * m-waste) / (save-qty / 1000) / v-sqft-fac
            format ">>>>9.99" to 69
         (mfl$ / mqty) * m-waste format ">>>>,>>9.99" to 80 skip
         tmpstore + "RUN Waste" format "x(15)"
         m-spo format ">>>>>>9" to 50 space(0) " Sht" space(0)
         /* cost of spoil amortized per 1000 of all blanks on this form */
         (m-spo * (mfl$ / mqty)) / (save-qty / 1000) / v-sqft-fac
            format ">>>>9.99" to 69
         m-spo * (mfl$ / mqty) format ">>>>,>>9.99" to 80 SKIP WITH STREAM-IO.
     
      /* rm handling chg per cwt*/
      ld-rm-rate = IF xeb.pur-man THEN rm-rate-f ELSE ctrl[3].
      if ld-rm-rate ne 0 THEN
         ASSIGN
           rm-wt = (med-qty + ((m-waste * brd-sf[2]) / 1000) +
                              ((m-spo   * brd-sf[2]) / 1000)) *
                              (if avail(item) then item.basis-w else 1)
           rm-wt$ = (rm-wt / 100) * ld-rm-rate
           ctrl2[3] = ctrl2[3] + rm-wt$.
     
   end. /*v-add-to-est*/
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
      DO:
         find first est-op WHERE
              est-op.company eq xest.company AND
              est-op.est-no  eq xest.est-no AND
              est-op.qty     eq v-op-qty AND
              est-op.line    gt 500 AND
              est-op.dept    eq "CR"
              NO-LOCK NO-ERROR.

         IF AVAIL est-op THEN
         DO:
            gqty = xef.adh-sqin * est-op.num-sh / item.sqin-lb.
            RELEASE est-op.
         END.
         ELSE
            gqty = 0.
      END.
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
         
         FIND FIRST b-qty WHERE
              b-qty.reftable = "blank-vend-qty" AND
              b-qty.company = e-item.company AND
              b-qty.CODE    = e-item.i-no
              NO-LOCK NO-ERROR.
         
         IF AVAIL b-qty THEN
         DO:
            FIND FIRST b-cost WHERE
                 b-cost.reftable = "blank-vend-cost" AND
                 b-cost.company = e-item.company AND
		         b-cost.CODE    = e-item.i-no
                 NO-LOCK NO-ERROR.
         
            DO j = 1 TO 10:
               ASSIGN
                  tt-ei.run-qty[j + 10] = b-qty.val[j]
                  tt-ei.run-cost[j + 10] = b-cost.val[j].
            END.
         END.
        
         DO j = 1 TO 20:
            IF tt-ei.run-qty[j] LT gqty THEN NEXT.
               gcost = gqty * tt-ei.run-cost[j].
               IF e-item.std-uom NE "" THEN vuom = e-item.std-uom.
            LEAVE.
         END.
      END.
      ELSE
         gcost = gqty * IF ce-ctrl.r-cost THEN item.avg-cost
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
           vqty                                   format "x(9)"         to 50
           vuom
           gcost / (save-qty / 1000) / v-sqft-fac format ">>,>>9.99"    to 69
           gcost                                  format ">>>>,>>9.99"  to 80 SKIP.
   END. 
end. /*xef.adh-code*/

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
      DO:
         find first est-op WHERE
              est-op.company eq xest.company AND
              est-op.est-no  eq xest.est-no AND
              est-op.qty     eq v-op-qty AND
              est-op.line    gt 500 AND
              est-op.dept    eq "CR"
              NO-LOCK NO-ERROR.

         IF AVAIL est-op THEN
         DO:
            gqty = xef.gsh-wid * xef.gsh-len * est-op.num-sh / item.sqin-lb.
            RELEASE est-op.
         END.
         ELSE
            gqty = 0.
      END.
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
         
         FIND FIRST b-qty WHERE
              b-qty.reftable = "blank-vend-qty" AND
              b-qty.company = e-item.company AND
	          b-qty.CODE    = e-item.i-no
              NO-LOCK NO-ERROR.
         
         IF AVAIL b-qty THEN
         DO:
            FIND FIRST b-cost WHERE
                 b-cost.reftable = "blank-vend-cost" AND
                 b-cost.company = e-item.company AND
		         b-cost.CODE    = e-item.i-no
                 NO-LOCK NO-ERROR.
         
            DO j = 1 TO 10:
               ASSIGN
                  tt-ei.run-qty[j + 10] = b-qty.val[j]
                  tt-ei.run-cost[j + 10] = b-cost.val[j].
            END.
         END.

         DO j = 1 TO 20:
            IF tt-ei.run-qty[j] LT gqty THEN NEXT.
            gcost = gqty * tt-ei.run-cost[j].
            IF e-item.std-uom NE "" THEN vuom = e-item.std-uom.
            LEAVE.
         END.
      END.
      ELSE
         gcost = gqty * IF ce-ctrl.r-cost THEN item.avg-cost
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
              vqty  format "x(9)" to 50
              vuom
              gcost / (save-qty / 1000) / v-sqft-fac format ">>,>>9.99" to 69
              gcost format ">>>>,>>9.99"   to 80 SKIP.
   END.
end. /*lam-code*/

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
