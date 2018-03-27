/* ----------------------------------------------------- ce/box/pr42-cas.p */

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def var i as int no-undo.
def var j as int no-undo.
def var zzz as int no-undo.
def var yyy as int no-undo.
def shared var qty as int NO-UNDO.
def var tmpstore as cha no-undo.
def var v-cas-pal-w like item.basis-w NO-UNDO.

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

DEF BUFFER b-ef FOR ef.
DEF BUFFER b-eb FOR eb.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

{cec/print4.i shared shared}
{cec/print42.i shared}

def buffer xcas for cas.

def var v-pallets as log init yes no-undo.
def var v-pck-code as char format "x(12)" initial "Pack Code/Unit" no-undo.
def var v-yld as dec no-undo.
DEF VAR v-setup LIKE e-item-vend.setup NO-UNDO.
DEF VAR b-wt-tot LIKE b-wt NO-UNDO.
DEF VAR li-qty LIKE c-qty NO-UNDO.
DEF VAR ld-rm AS DEC NO-UNDO.
DEF VAR ld-hp AS DEC NO-UNDO.
DEF VAR ll-unitize AS LOG NO-UNDO.
DEFINE VARIABLE iCaseMult AS INTEGER     NO-UNDO.

save-qty = qty.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

ll-unitize = CAN-FIND(FIRST b-eb                /* Set is Unitized */
                      WHERE b-eb.company  EQ xest.company
                        AND b-eb.est-no   EQ xest.est-no
                        AND b-eb.form-no  EQ 0
                        AND b-eb.blank-no EQ 0
                        AND b-eb.tr-no    NE ""
                        AND b-eb.pur-man  EQ YES).

/******************************** C A S E S ****************************/
FOR EACH xef
    WHERE xef.company  EQ xest.company
      AND xef.est-no   EQ xest.est-no
    NO-LOCK:

  FIND FIRST item
      {sys/look/itemW.i}
        AND item.i-no EQ xef.board
      NO-LOCK NO-ERROR.
  b-wt = IF AVAIL item THEN item.basis-w ELSE 0.

  IF NOT ll-unitize THEN
  FOR EACH xeb
      WHERE xeb.company EQ xef.company
        AND xeb.est-no  EQ xef.est-no
        AND xeb.form-no EQ xef.form-no
      NO-LOCK:

    ASSIGN
     v-yld    = IF xeb.cust-% LT 0 THEN -1 / xeb.cust-% ELSE xeb.cust-%
     qty      = tt-blk * v-yld
     b-wt-tot = (xeb.t-sqin - xeb.t-win) * qty / 144000 * b-wt.

    RUN do-cas-no.
  END.

  IF xef.form-no EQ xest.form-qty THEN
  FOR EACH xeb
      WHERE xeb.company EQ xest.company
        AND xeb.est-no  EQ xest.est-no
        AND xeb.cas-no  NE ""
        AND xeb.form-no EQ 0
      NO-LOCK:

    RUN get-wt.

    ASSIGN
     qty   = tt-blk
     v-yld = 1.

    RUN do-cas-no.

    LEAVE.
  END.
END.

for each cas where cas.typ = 1 by cas.snum by cas.bnum with no-labels no-box:
   find first xeb where xeb.company  = cocode     and
                        xeb.est-no    = xest.est-no and
                        xeb.form-no  = cas.snum   and
                        xeb.blank-no = cas.bnum   no-lock no-error.
   find first item {sys/look/itemW.i} and item.i-no = cas.ino
   no-lock no-error.

   cas.t-qty = 0.
   FOR EACH xcas WHERE xcas.typ EQ 1 AND xcas.ino EQ cas.ino:
     cas.t-qty = cas.t-qty + xcas.qty.
   END.

   v-setup = 0.

   IF xeb.cas-cost GT 0 THEN cas.cost = xeb.cas-cost * cas.qty.
      
   ELSE DO:
     {est/matcost.i cas.t-qty cas.cost 1}

     ASSIGN
      cas.cost = (cas.cost * cas.qty) + lv-setup-1
      v-setup  = lv-setup-1.
   END.

   /* cosm was set to tot # blanks this item; set to cost now */
   ASSIGN
    zzz      = cas.cosm
    cas.cosm = cas.cost / (cas.cosm / 1000).

   find first brd where brd.form-no = cas.snum and
                        brd.blank-no = cas.bnum and
                        brd.i-no    = cas.ino
                        no-error.
   if not available brd then
   do:
      create brd.
      assign brd.form-no = cas.snum
             brd.blank-no = cas.bnum
             brd.i-no    = cas.ino
             brd.dscr    = cas.dscr
             brd.basis-w = item.basis-w.
   end.
   ASSIGN
   brd.qty = brd.qty + cas.qty
   brd.qty-uom = "Ea"
   brd.sc-uom  = "Ea"
   brd.cost = cas.cost / cas.qty
   brd.cost-m = cas.cosm.

   display string(cas.snum,"99") + "-" + string(cas.bnum,"9") format "x(4)"
           item.i-name format "x(20)"
           (zzz / cas.qty) format ">>>>9" "Pieces/Case"
           cas.qty format ">>>>>9" to 50 "Cas"
           v-setup when v-setup ne 0 format ">>>9.99" to 61
           cas.cosm to 69
           cas.cost to 80 format ">>>,>>9.99" skip
       with stream-io.

   ASSIGN
   zzz = 0
   yyy = cas.qty.   /* Used in pallets */
end.

for each cas where cas.typ = 5 by cas.snum by cas.bnum with no-labels no-box:
   find first xeb where xeb.company  = cocode     and
                        xeb.est-no    = xest.est-no and
                        xeb.form-no  = cas.snum   and
                        xeb.blank-no = cas.bnum   no-lock no-error.
   find first item {sys/look/itemW.i} and item.i-no = cas.ino
   no-lock no-error.

   cas.t-qty = 0.
   FOR EACH xcas WHERE xcas.typ EQ 5 AND xcas.ino EQ cas.ino:
       cas.t-qty = cas.t-qty + xcas.qty.
   END.

   v-setup = 0.

   {est/matcost.i cas.t-qty cas.cost 5}

   ASSIGN
    cas.cost = (cas.cost * cas.qty) + lv-setup-5
    v-setup  = lv-setup-5
   /* cosm was set to tot # blanks this item; set to cost now */
    zzz      = cas.cosm
    cas.cosm = cas.cost / (cas.cosm / 1000).

   find first brd where brd.form-no = cas.snum and
                        brd.blank-no = cas.bnum and
                        brd.i-no    = cas.ino
                        no-error.
   if not available brd then
   do:
      create brd.
      assign brd.form-no = cas.snum
             brd.blank-no = cas.bnum
             brd.i-no    = cas.ino
             brd.dscr    = cas.dscr
             brd.basis-w = item.basis-w.
   end.
   ASSIGN
   brd.qty = brd.qty + cas.qty
   brd.qty-uom = "Ea"
   brd.sc-uom  = "Ea"
   brd.cost = cas.cost / cas.qty
   brd.cost-m = cas.cosm.

   display string(cas.snum,"99") + "-" + string(cas.bnum,"9") format "x(4)"
           item.i-name format "x(20)"
           /*(zzz / cas.qty) format ">>>>9" "Pieces/Case"*/
           cas.qty format ">>>>>9" to 50 "Pad"
           v-setup when v-setup ne 0 format ">>>9.99" to 61
           cas.cosm to 69
           cas.cost to 80 format ">>>,>>9.99" skip
       with stream-io.
   zzz = 0.
end.

for each cas where cas.typ = 6 by cas.snum by cas.bnum with no-labels no-box:
   find first xeb where xeb.company  = cocode     and
                        xeb.est-no    = xest.est-no and
                        xeb.form-no  = cas.snum   and
                        xeb.blank-no = cas.bnum   no-lock no-error.
   find first item {sys/look/itemW.i} and item.i-no = cas.ino
   no-lock no-error.

   cas.t-qty = 0.
   FOR EACH xcas WHERE xcas.typ EQ 6 AND xcas.ino EQ cas.ino:
       cas.t-qty = cas.t-qty + xcas.qty.
   END.

   v-setup = 0.

   {est/matcost.i cas.t-qty cas.cost 6}

   ASSIGN
    cas.cost = (cas.cost * cas.qty) + lv-setup-6
    v-setup  = lv-setup-6
   /* cosm was set to tot # blanks this item; set to cost now */
    zzz      = cas.cosm
    cas.cosm = cas.cost / (cas.cosm / 1000).

   find first brd where brd.form-no = cas.snum and
                        brd.blank-no = cas.bnum and
                        brd.i-no    = cas.ino
                        no-error.
   if not available brd then
   do:
      create brd.
      assign brd.form-no = cas.snum
             brd.blank-no = cas.bnum
             brd.i-no    = cas.ino
             brd.dscr    = cas.dscr
             brd.basis-w = item.basis-w.
   end.
   ASSIGN
   brd.qty = brd.qty + cas.qty
   brd.qty-uom = "Ea"
   brd.sc-uom  = "Ea"
   brd.cost = cas.cost / cas.qty
   brd.cost-m = cas.cosm.

   display string(cas.snum,"99") + "-" + string(cas.bnum,"9") format "x(4)"
           item.i-name format "x(20)"
           /*(zzz / cas.qty) format ">>>>9" "Pieces/Case"*/
           cas.qty format ">>>>>9" to 50 "Div"
           v-setup when v-setup ne 0 format ">>>9.99" to 61
           cas.cosm to 69
           cas.cost to 80 format ">>>,>>9.99" skip
       with stream-io.
   zzz = 0.
end.

/*************************** T R A Y S **********************************/
for each xeb where xeb.company = xest.company
               and xeb.est-no    eq xest.est-no
          break by xeb.part-no:
   v-yld = if xeb.cust-% lt 0 then -1 / xeb.cust-% else xeb.cust-%.

   if not first-of(xeb.part-no) then next.
   if xeb.tr-no ne "" then do with frame ac3 no-box no-labels stream-io:
      find first cas where cas.typ = 2 and cas.id = xeb.part-no no-error.
      if not available cas then do:
         find first item {sys/look/itemW.i} and item.i-no = xeb.tr-no
         no-lock no-error.

         if item.mat-type eq "Z" then v-pallets = no.
         
         if true then leave.
         
         find first e-item of item no-lock no-error.
         create cas.
         assign
            cas.typ  = 2
            cas.id   = xeb.part-no
            cas.snum = xeb.form-no
            cas.bnum = xeb.blank-no
            cas.ino  = item.i-no
            cas.dscr = item.est-dscr.
      end.
      cas.cosm = cas.cosm + (tt-blk * v-yld).
      find first xcas where xcas.typ = 1 and xcas.id = xeb.part-no no-error.
      if available xcas then do:
         t-qty = xeb.tr-cas * xcas.qty.
         {sys/inc/roundup.i t-qty}
         cas.qty = cas.qty + t-qty.
      end.
   end.
end.

for each cas where cas.typ = 2 by cas.snum by cas.bnum with no-labels no-box:
   find first xeb where xeb.company  = cocode     and
                        xeb.est-no    = xest.est-no and
                        xeb.form-no  = cas.snum   and
                        xeb.blank-no = cas.bnum   no-lock no-error.
   find first item {sys/look/itemW.i}
                     and item.i-no = cas.ino no-lock no-error.

   cas.t-qty = 0.
   FOR EACH xcas WHERE xcas.typ EQ 2 AND xcas.ino EQ cas.ino:
     cas.t-qty = cas.t-qty + xcas.qty.
   END.

   FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

   b-uom = IF AVAIL e-item AND e-item.std-uom NE "" THEN e-item.std-uom
                                                    ELSE item.cons-uom.

   IF b-uom EQ "M" THEN
     ASSIGN
      cas.qty   = cas.qty / 1000
      cas.t-qty = cas.t-qty / 1000.

   v-setup = 0.

   IF xeb.cas-cost GT 0 THEN cas.cost = xeb.cas-cost * cas.qty.
      
   ELSE DO:
     {est/matcost.i cas.t-qty cas.cost 2}

     ASSIGN
      cas.cost = (cas.cost * cas.qty) + lv-setup-2
      v-setup  = lv-setup-2.
   END.

   /* cosm was set to tot # blanks this item; set to cost now */
   cas.cosm = cas.cost / (cas.cosm / 1000).

   find first brd where brd.form-no = cas.snum and
                        brd.blank-no = cas.bnum and
                        brd.i-no    = cas.ino
                        no-error.
   if not available brd then
   do:
      create brd.
      assign brd.form-no = cas.snum
             brd.blank-no = cas.bnum
             brd.i-no    = cas.ino
             brd.dscr    = cas.dscr
             brd.basis-w = item.basis-w.
   end.
   ASSIGN
   brd.qty = brd.qty + cas.qty
   brd.qty-uom = "Ea"
   brd.sc-uom  = "Ea"
   brd.cost = cas.cost / cas.qty
   brd.cost-m = cas.cosm.
   display string(cas.snum,"99") + "-" + string(cas.bnum,"9") format "x(4)"
           item.i-name cas.qty format ">>>>>9" to 50 "Tr"
           v-setup when v-setup ne 0 format ">>>9.99" to 61
           cas.cosm to 69
           cas.cost to 80 format ">>>,>>9.99" skip
       with stream-io.
end.

/**************** P A L L E T S *************************/
IF v-pallets THEN
FOR EACH xef
    WHERE xef.company  EQ xest.company
      AND xef.est-no   EQ xest.est-no
    NO-LOCK:

  FIND FIRST item
      {sys/look/itemW.i}
        AND item.i-no EQ xef.board
      NO-LOCK NO-ERROR.
  b-wt = IF AVAIL item THEN item.basis-w ELSE 0.

  IF NOT ll-unitize THEN
  FOR EACH xeb
      WHERE xeb.company EQ xef.company
        AND xeb.est-no  EQ xef.est-no
        AND xeb.form-no EQ xef.form-no
        AND xeb.tr-no   NE ""
      NO-LOCK:

    ASSIGN
     v-yld    = IF xeb.cust-% LT 0 THEN -1 / xeb.cust-% ELSE xeb.cust-%
     qty      = tt-blk * v-yld
     b-wt-tot = (xeb.t-sqin - xeb.t-win) * qty / 144000 * b-wt.

    RUN do-tr-no.
  END.

  IF xef.form-no EQ xest.form-qty THEN
  FOR EACH xeb
      WHERE xeb.company EQ xest.company
        AND xeb.est-no  EQ xest.est-no
        AND xeb.tr-no   NE ""
        AND xeb.form-no EQ 0
      NO-LOCK:

    RUN get-wt.
.
    ASSIGN
     qty   = tt-blk
     v-yld = 1.

    RUN do-tr-no.

    LEAVE.
  END.
END.

for each cas where cas.typ = 3 by cas.snum by cas.bnum with no-labels no-box:
   find first xeb where xeb.company  = cocode     and
                        xeb.est-no    = xest.est-no and
                        xeb.form-no  = cas.snum   and
                        xeb.blank-no = cas.bnum   no-lock no-error.
   find first item {sys/look/itemW.i}
                     and item.i-no = cas.ino no-lock no-error.

   cas.t-qty = 0.
   FOR EACH xcas WHERE xcas.typ EQ 3 AND xcas.ino EQ cas.ino:
      cas.t-qty = cas.t-qty + xcas.qty.
   END.

   v-setup = 0.

   IF xeb.tr-cost GT 0 THEN cas.cost = xeb.tr-cost * cas.qty.
      
   ELSE DO:
     {est/matcost.i cas.t-qty cas.cost 3}

     ASSIGN
      cas.cost = (cas.cost * cas.qty) + lv-setup-3
      v-setup  = lv-setup-3.
   END.

   /* cosm was set to tot # blanks this item; set to cost now */
   cas.cosm = cas.cost / (cas.cosm / 1000).

   find first brd where brd.form-no = cas.snum and
                        brd.blank-no = cas.bnum and
                        brd.i-no    = cas.ino
                        no-error.
   if not available brd then
   do:
      create brd.
      assign brd.form-no = cas.snum
             brd.blank-no = cas.bnum
             brd.i-no    = cas.ino
             brd.dscr    = cas.dscr
             brd.basis-w = item.basis-w.
   end.
   ASSIGN
   brd.qty = brd.qty + cas.qty
   brd.qty-uom = "Ea"
   brd.sc-uom  = "Ea"
   brd.cost = cas.cost / cas.qty
   brd.cost-m = cas.cosm.

   if xeb.cas-no ne "" then
     v-pck-code = caps(substr(xeb.cas-no,1,1)) +
                  lc(substr(xeb.cas-no,2,9)) +
                  "s/Unit".

   find first xcas
        where xcas.typ  = 1 
          and xcas.id   = xeb.part-no
          and xcas.snum = xeb.form-no
        no-error.

   if cas.qty ne 0 then do:
      display string(cas.snum,"99") + "-" + string(cas.bnum,"9") format "x(4)"
              item.i-name format "x(20)"
              xcas.qty / cas.qty when avail xcas format ">>>>9"
              "" @ v-pck-code
              v-pck-code when avail xcas
              space(0)
              cas.qty format ">>>>>9" to 50 "Pal"
              v-setup when v-setup ne 0 format ">>>9.99" to 61
              cas.cosm to 69
              cas.cost to 80 format ">>>,>>9.99" skip
          with stream-io.

      {cec/pr4-str.i}

      if strap-qty ne 0 then do:
         strap-qty = strap-qty * cas.qty.

         find first item
             where item.company eq cocode
               and item.i-no    eq stackPattern.strapCode
             no-lock no-error.

         find first xcas where xcas.typ = 4 and xcas.id = cas.id no-error.
         if not available xcas then do:
            create xcas.
            assign
             xcas.typ  = 4
             xcas.id   = cas.id
             xcas.snum = cas.snum
             xcas.bnum = cas.bnum
             xcas.ino  = item.i-no
             xcas.dscr = item.est-dscr.
         end.
         xcas.qty = xcas.qty + strap-qty.
      end.
   end.
end.

for each cas where cas.typ = 4 by cas.snum by cas.bnum with no-labels no-box:
   find first xeb where xeb.company  = cocode     and
                        xeb.est-no    = xest.est-no and
                        xeb.form-no  = cas.snum   and
                        xeb.blank-no = cas.bnum   no-lock no-error.

   find first item
       where item.company eq cocode
         and item.i-no    eq cas.ino
       no-lock no-error.

   if not avail item then next.

   cas.t-qty = 0.
   FOR EACH xcas WHERE xcas.typ EQ 4 AND xcas.ino EQ cas.ino:
      cas.t-qty = cas.t-qty + xcas.qty.
   END.

   strap-qty = cas.qty / 1000.

   {est/matcost.i strap-qty cas.cost strap}

   cas.cost = (cas.cost * strap-qty) + lv-setup-strap.

   if strap-qty ne 0 then do with frame ac4 no-box no-labels stream-io:
      display string(cas.snum,"99") + "-" + string(cas.bnum,"9") format "x(4)"
              item.i-name
              strap-qty         format ">>>.999"         to 50 "MLI"
              lv-setup-strap when lv-setup-strap ne 0 format ">>>9.99" to 61
              cas.cost / (tt-blk / 1000)                 to 69
              cas.cost          format ">>>,>>9.99"      to 80 skip
          with stream-io.

      find first brd
          where brd.form-no  eq xeb.form-no
            and brd.blank-no eq xeb.blank-no
            and brd.i-no     eq item.i-no
          no-error.
      if not avail brd then do:
         create brd.
         assign
          brd.form-no  = cas.bnum
          brd.blank-no = cas.snum
          brd.i-no     = cas.ino
          brd.dscr     = cas.dscr
          brd.basis-w  = item.basis-w
          brd.qty-uom  = "MLI"
          brd.sc-uom   = "MLI".
       end.
       assign
        brd.cost   = (brd.cost * brd.qty) + cas.cost
        brd.qty    = brd.qty + strap-qty
        brd.cost   = brd.cost / brd.qty
        brd.cost-m = (brd.cost * brd.qty) / (tt-blk / 1000).
   end.
end.

for each cas where cas.typ = 1:
   find first blk
       where (blk.id = cas.id and blk.snum = cas.snum and blk.bnum = cas.bnum)
          or (blk.snum = xest.form-qty and cas.snum = 0)
       no-error.
      
  IF blk.pur-man THEN
    ASSIGN
     ld-rm = rm-rate-f
     ld-hp = hand-pct-f.
  ELSE
    ASSIGN
     ld-rm = ctrl[3]
     ld-hp = ctrl[2].

   /* assign proper cost to blk record */
   ASSIGN
   blk.cost  = blk.cost  + cas.cost
   dm-tot[5] = dm-tot[5] + cas.cost.
   if ld-rm ne 0 then do:
       find first item
          where item.company eq cocode
            and item.i-no    eq cas.id
          no-lock no-error.

      ASSIGN
      v-cas-pal-w = if avail item then item.basis-w else ce-ctrl.def-cas-w
      ctrl2[3] = ctrl2[3] + ((cas.qty * v-cas-pal-w) / 100) * ld-rm
      blk.cost = blk.cost + ((cas.qty * v-cas-pal-w) / 100) * ld-rm
      blk.lab  = blk.lab  + ((cas.qty * v-cas-pal-w) / 100) * ld-rm.
   end.
   /*if ld-hp ne 0 then do:
      ctrl2[2] = ctrl2[2] + (cas.cost * ld-hp).
      blk.cost = blk.cost + (cas.cost * ld-hp).
      blk.lab  = blk.lab  + (cas.cost * ld-hp).
   end.*/
end.

for each cas where cas.typ = 5 or cas.typ = 6:
   find first blk
       where (blk.id = cas.id and blk.snum = cas.snum and blk.bnum = cas.bnum)
          or (blk.snum = xest.form-qty and cas.snum = 0)
       no-error.
      
  IF blk.pur-man THEN
    ASSIGN
     ld-rm = rm-rate-f
     ld-hp = hand-pct-f.
  ELSE
    ASSIGN
     ld-rm = ctrl[3]
     ld-hp = ctrl[2].

   /* assign proper cost to blk record */
   ASSIGN
   blk.cost  = blk.cost  + cas.cost
   dm-tot[5] = dm-tot[5] + cas.cost.
   if ld-rm ne 0 then do:
       find first item
          where item.company eq cocode
            and item.i-no    eq cas.id
          no-lock no-error.
      ASSIGN
      v-cas-pal-w = if avail item then item.weight-100 else 0
      ctrl2[3] = ctrl2[3] + ((cas.qty * v-cas-pal-w) / 100) * ld-rm
      blk.cost = blk.cost + ((cas.qty * v-cas-pal-w) / 100) * ld-rm
      blk.lab  = blk.lab  + ((cas.qty * v-cas-pal-w) / 100) * ld-rm.
   end.
   /*if ld-hp ne 0 then do:
      ctrl2[2] = ctrl2[2] + (cas.cost * ld-hp).
      blk.cost = blk.cost + (cas.cost * ld-hp).
      blk.lab  = blk.lab  + (cas.cost * ld-hp).
   end.*/
end.

for each cas where cas.typ = 2 or cas.typ = 4:
   find first blk
       where (blk.id = cas.id and blk.snum = cas.snum and blk.bnum = cas.bnum)
          or (blk.snum = xest.form-qty and cas.snum = 0)
       no-error.
   /* assign proper cost to blk record */
   ASSIGN
   blk.cost  = blk.cost  + cas.cost
   dm-tot[5] = dm-tot[5] + cas.cost.
end.

for each cas where cas.typ = 3:
   find first blk
       where (blk.id = cas.id and blk.snum = cas.snum and blk.bnum = cas.bnum)
          or (blk.snum = xest.form-qty and cas.snum = 0)
       no-error.
      
  IF blk.pur-man THEN
    ASSIGN
     ld-rm = rm-rate-f
     ld-hp = hand-pct-f.
  ELSE
    ASSIGN
     ld-rm = ctrl[3]
     ld-hp = ctrl[2].

   /* assign proper cost to blk record */
   ASSIGN
   blk.cost  = blk.cost  + cas.cost
   dm-tot[5] = dm-tot[5] + cas.cost.
   if ld-rm ne 0 then do:
      find first item
          where item.company eq cocode
            and item.i-no    eq cas.id
          no-lock no-error.
      ASSIGN
      v-cas-pal-w = if avail item then item.basis-w else ce-ctrl.def-pal-w
      ctrl2[3] = ctrl2[3] + ((cas.qty * v-cas-pal-w) / 100) * ld-rm
      blk.cost = blk.cost + ((cas.qty * v-cas-pal-w) / 100) * ld-rm
      blk.lab  = blk.lab  + ((cas.qty * v-cas-pal-w) / 100) * ld-rm.
   end.
   /*if ld-hp ne 0 then do:
      ctrl2[2] = ctrl2[2] + (cas.cost * ld-hp).
      blk.cost = blk.cost + (cas.cost * ld-hp).
      blk.lab  = blk.lab  + (cas.cost * ld-hp).
   end.*/
end.

IF CAN-FIND(FIRST cas WHERE cas.typ EQ 1 AND cas.snum EQ 0) THEN
FOR EACH cas WHERE cas.snum NE 0:
  DELETE cas.
END.

qty = save-qty.

RETURN.

PROCEDURE do-cas-no:

  /* case */
  if xeb.cas-no ne "" then do with frame ac2 no-box no-labels stream-io:
    find first item {sys/look/itemW.i} and item.i-no = xeb.cas-no
    no-lock no-error.
    find first e-item of item no-lock no-error.
    find first cas
        where cas.typ  eq 1
          and cas.snum eq xeb.form-no
          and cas.bnum eq xeb.blank-no
        no-error.
    if not available cas then do:
      create cas.
      assign
       cas.typ  = 1
       cas.id   = xeb.part-no
       cas.snum = xeb.form-no
       cas.bnum = xeb.blank-no
       cas.ino  = item.i-no
       cas.dscr = item.est-dscr.
    end.
    if xeb.cas-cnt ne 0 then c-qty = qty / xeb.cas-cnt.
      /* following if .. do .. end added 9508 by CAH */
    else
    if xeb.cas-cnt = 0 and xeb.cas-wt > 0 and xeb.weight-m > 0 then do:
      def var ws_cas-cnt as int no-undo.
      ASSIGN
      ws_cas-cnt = xeb.cas-wt / xeb.weight-m * 1000
      c-qty = qty / ws_cas-cnt. /* number of cases required, by weight */
    end.
    else c-qty = b-wt-tot / item.avg-w.
    {sys/inc/roundup.i c-qty} /* CTS end */
     /*02031503-set case qty based on multipliers for cost and material calculations*/
      IF xeb.spare-int-3 GT 0 THEN 
          iCaseMult = xeb.spare-int-3.
      ELSE
          iCaseMult = 1.
      c-qty = c-qty * iCaseMult.

    ASSIGN
    cas.qty = cas.qty + c-qty
    cas.cosm = cas.cosm + qty.

     /*02031503-reset case qty for calculation of layers and dividers*/
     IF iCaseMult GT 1 THEN c-qty = c-qty / iCaseMult.
  end.

  if xeb.cas-no ne "" and xeb.layer-pad ne "" and xeb.lp-up ne 0 then do:
    find first item {sys/look/itemW.i} and item.i-no = xeb.layer-pad
    no-lock no-error.
    find first e-item of item no-lock no-error.
    find first cas
        where cas.typ  eq 5
          and cas.snum eq xeb.form-no
          and cas.bnum eq xeb.blank-no
        no-error.
    if not available cas then do:
      create cas.
      assign
       cas.typ  = 5
       cas.id   = xeb.part-no
       cas.snum = xeb.form-no
       cas.bnum = xeb.blank-no
       cas.ino  = item.i-no
       cas.dscr = item.est-dscr.
    end.
    IF xeb.spare-char-3 EQ "P" THEN DO:
          li-qty = c-qty / xeb.cas-pal.
          {sys/inc/roundup.i li-qty}
          li-qty = li-qty * xeb.lp-up.  /*per pallet*/
      END.
      ELSE
          li-qty = c-qty * xeb.lp-up. /*per case - DEFAULT*/
    {sys/inc/roundup.i li-qty} /* CTS end */
    ASSIGN
    cas.qty = cas.qty + li-qty
    cas.cosm = cas.cosm + qty.
  end.

  if xeb.cas-no ne "" and xeb.divider ne "" and xeb.div-up ne 0 then do:
    find first item {sys/look/itemW.i} and item.i-no = xeb.divider
    no-lock no-error.
    find first e-item of item no-lock no-error.
    find first cas
        where cas.typ  eq 6
          and cas.snum eq xeb.form-no
          and cas.bnum eq xeb.blank-no
        no-error.
    if not available cas then do:
      create cas.
      assign
       cas.typ  = 6
       cas.id   = xeb.part-no
       cas.snum = xeb.form-no
       cas.bnum = xeb.blank-no
       cas.ino  = item.i-no
       cas.dscr = item.est-dscr.
    end.
     IF xeb.spare-char-4 EQ "P" THEN DO:
          li-qty = c-qty / xeb.cas-pal.
          {sys/inc/roundup.i li-qty}
          li-qty = li-qty * xeb.div-up.  /*per pallet*/
      END.
      ELSE
          li-qty = c-qty * xeb.div-up. /*per case - DEFAULT*/
    {sys/inc/roundup.i li-qty} /* CTS end */
    ASSIGN
    cas.qty = cas.qty + li-qty
    cas.cosm = cas.cosm + qty.
  end.

END PROCEDURE.

PROCEDURE do-tr-no:

  /* Pallets */
  find first item {sys/look/itemW.i} and item.i-no = xeb.tr-no
  no-lock no-error.
  find first e-item of item no-lock no-error.

  if xeb.cas-no ne "" then do with frame ac33 no-box no-labels stream-io:
      find first xcas
          where xcas.typ  eq 1
            and xcas.snum eq xeb.form-no
            and xcas.bnum eq xeb.blank-no
          no-error.
      find first cas
        where cas.typ  eq 3
          and cas.snum eq xeb.form-no
          and cas.bnum eq xeb.blank-no
        no-error.
    if not available cas then do:
      create cas.
      assign
       cas.typ  = 3
       cas.id   = xeb.part-no
       cas.snum = xeb.form-no
       cas.bnum = xeb.blank-no
       cas.ino  = item.i-no
       cas.dscr = item.est-dscr.
    end.
    cas.cosm = cas.cosm + qty.
    if xeb.cas-pal ne 0 then p-qty = xcas.qty / xeb.cas-pal.
    else p-qty = (b-wt-tot + xcas.qty) / item.avg-w.
    {sys/inc/roundup.i p-qty}
    cas.qty = cas.qty + p-qty.
  end.

END PROCEDURE.

PROCEDURE get-wt.

  b-wt-tot = 0.

  FOR EACH b-ef
      WHERE b-ef.company EQ xef.company
        AND b-ef.est-no  EQ xef.est-no
      NO-LOCK:

    FIND FIRST item
        WHERE item.company EQ b-ef.company
          AND item.i-no    EQ b-ef.board
        NO-LOCK NO-ERROR.
    b-wt = IF AVAIL item THEN item.basis-w ELSE 0.

    FOR EACH b-eb
        WHERE b-eb.company EQ b-ef.company
          AND b-eb.est-no  EQ b-ef.est-no
          AND b-eb.form-no EQ b-ef.form-no
        NO-LOCK:
      ASSIGN
       v-yld    = IF b-eb.cust-% LT 0 THEN -1 / b-eb.cust-% ELSE b-eb.cust-%
       qty      = tt-blk * v-yld
       b-wt-tot = b-wt-tot +
                  ((b-eb.t-sqin - b-eb.t-win) * qty / 144000 * b-wt).
    END.
  END.

END PROCEDURE.
/* end ---------------------------------- copr. 1992  advanced software, inc. */
