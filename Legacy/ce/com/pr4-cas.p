/* ----------------------------------------------------- ce/pr4-cas.p 4/92 cd */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

{ce/print4.i shared shared}

def buffer xcas for cas.

def var v-cas-cnt as DEC NO-UNDO.
def var v-blk-frm as char format "x(5)" NO-UNDO.
DEF VAR li-qty LIKE c-qty NO-UNDO.
DEFINE VARIABLE iCaseMult AS INTEGER     NO-UNDO.
DEFINE VARIABLE dCaseQty LIKE c-qty NO-UNDO.

def shared var v-summ as log NO-UNDO.

DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

def TEMP-TABLE w-cas NO-UNDO
    field i-no   like cas.ino
    field qty    like cas.qty
    field units  like xeb.yld-qty
    field cost   like cas.cost
    field rec-id as   recid.
DEF SHARED VAR gEstSummaryOnly AS LOG NO-UNDO.
{ce/msfcalc.i}

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

/******************************** C A S E S ****************************/
for each xef where xef.company = xest.company
               AND xef.est-no = xest.est-no :
 find first item where item.i-no = xef.board no-lock no-error.
 b-wt = item.basis-w.  /************Should also check CAL & Laminates...******/
 for each xeb where xeb.company = xest.company
                AND xeb.est-no = xest.est-no and xeb.form-no = xef.form-no:
   /* case */
   if xeb.cas-no ne "" then do with frame ac2 no-box no-labels:
      find first item {sys/look/itemW.i} and item.i-no = xeb.cas-no
      no-lock no-error.
      find first e-item of item no-lock no-error.
      find first cas
          where cas.typ  eq 1
            and cas.snum eq xeb.form-no
            and cas.bnum eq xeb.blank-no
          no-error.
      if not avail cas then do:
         create cas.
         assign
            cas.typ  = 1
            cas.id   = xeb.part-no
            cas.snum = xeb.form-no
            cas.bnum = xeb.blank-no
            cas.ino  = item.i-no
            cas.dscr = item.est-dscr.
      end.

      if xeb.cas-cnt ne 0 then
        c-qty = xeb.yld-qty / xeb.cas-cnt.

      else
        c-qty = (xeb.yld-qty * b-wt *
                (if v-corr then ((xeb.t-sqin - xeb.t-win) * .000007)
                           else ((xeb.t-sqin - xeb.t-win) / 144000))) /
                (if xeb.cas-wt ne 0 then xeb.cas-wt else item.avg-w).

      {sys/inc/roundup.i c-qty} /* CTS end */
      
      /*02031503-set case qty based on multipliers for cost and material calculations*/
      IF xeb.spare-int-3 GT 0 THEN 
          iCaseMult = xeb.spare-int-3.
      ELSE
          iCaseMult = 1.
      c-qty = c-qty * iCaseMult.
     
      ASSIGN
         cas.qty = cas.qty + c-qty
         cas.cosm = cas.cosm + if xeb.yrprice then xeb.yld-qty else xeb.bl-qty.

      release w-cas.
      if v-summ then
      find first w-cas where w-cas.i-no eq cas.ino no-error.

      if not avail w-cas then do:
        create w-cas.
        assign
         w-cas.i-no   = cas.ino
         w-cas.rec-id = recid(cas).
      end.

      w-cas.units = w-cas.units + xeb.yld-qty.

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
      if not avail cas then do:
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
          li-qty = c-qty * xeb.lp-up. /*per case - DEFAULT*/.

      {sys/inc/roundup.i li-qty} /* CTS end */

      ASSIGN
         cas.qty = cas.qty + li-qty
         cas.cosm = cas.cosm + if xeb.yrprice then xeb.yld-qty else xeb.bl-qty.

      release w-cas.
      if v-summ then
      find first w-cas where w-cas.i-no eq cas.ino no-error.

      if not avail w-cas then do:
        create w-cas.
        assign
         w-cas.i-no   = cas.ino
         w-cas.rec-id = recid(cas).
      end.

      w-cas.units = w-cas.units + xeb.yld-qty.
   end.

   if xeb.cas-no ne "" and xeb.divider ne "" and xeb.lp-up ne 0 then do:
      find first item {sys/look/itemW.i} and item.i-no = xeb.divider
      no-lock no-error.
      find first e-item of item no-lock no-error.
      find first cas
          where cas.typ  eq 6
            and cas.snum eq xeb.form-no
            and cas.bnum eq xeb.blank-no
          no-error.
      if not avail cas then do:
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
         cas.cosm = cas.cosm + if xeb.yrprice then xeb.yld-qty else xeb.bl-qty.

      release w-cas.
      if v-summ then
      find first w-cas where w-cas.i-no eq cas.ino no-error.

      if not avail w-cas then do:
        create w-cas.
        assign
         w-cas.i-no   = cas.ino
         w-cas.rec-id = recid(cas).
      end.

      w-cas.units = w-cas.units + xeb.yld-qty.
   end.
 end.
end.

for each cas where cas.typ eq 1
    by cas.snum
    by cas.bnum:

   find first xeb
       where xeb.company  eq cocode
         and xeb.est-no   eq xest.est-no
         and xeb.form-no  eq cas.snum
         and xeb.blank-no eq cas.bnum
       no-lock no-error.
   find first item
       {sys/look/itemW.i}
         and item.i-no eq xeb.cas-no
       no-lock no-error.

   IF xeb.cas-cost GT 0 THEN cas.cost = xeb.cas-cost * cas.qty.
      
   ELSE DO:
     cas.t-qty = 0.

     FOR EACH xcas WHERE xcas.typ EQ 1 AND xcas.ino EQ cas.ino:
       cas.t-qty = cas.t-qty + xcas.qty.
     END.

     {est/matcost.i cas.t-qty cas.cost 1}

     cas.cost = (cas.cost * cas.qty) + lv-setup-1.
   END.

   ASSIGN
    zzz      = cas.cosm
    /* cosm was set to tot # blanks this item; set to cost now */
    cas.cosm = cas.cost / (cas.cosm / 1000).

   v-cas-cnt = if xeb.cas-cnt eq 0 then (zzz / cas.qty) else xeb.cas-cnt.
   {sys/inc/roundup.i v-cas-cnt}

   find first brd where brd.form-no = cas.snum and
                        brd.blank-no = cas.bnum and
                        brd.i-no    = xeb.cas-no
                        no-error.
   if not avail brd then do:
      create brd.
      assign brd.form-no = cas.snum
             brd.blank-no = cas.bnum
             brd.i-no    = xeb.cas-no
             brd.dscr    = cas.dscr
             brd.basis-w = item.basis-w.
   end.

   ASSIGN
      brd.qty = brd.qty + cas.qty
      brd.qty-uom = "Ea"
      brd.sc-uom  = "Ea"
      brd.cost = cas.cost / cas.qty
      brd.cost-m = cas.cosm
      brd.amount = brd.amount + cas.cost.

   find first w-cas where w-cas.rec-id eq recid(cas) no-error.
   if not avail w-cas then
   find first w-cas where w-cas.i-no eq cas.ino no-error.
   if avail w-cas then
     assign
      w-cas.qty  = w-cas.qty  + cas.qty
      w-cas.cost = w-cas.cost + cas.cost.
      
   ASSIGN
      zzz = 0
      yyy = cas.qty.   /* Used in pallets */
end.

for each cas where cas.typ eq 1,

    first w-cas where w-cas.rec-id eq recid(cas),

    first item
    {sys/look/itemW.i}
      and item.i-no eq cas.ino
    no-lock,

    first xeb
    where xeb.company  eq cocode
      and xeb.est-no   eq xest.est-no
      and xeb.form-no  eq cas.snum
      and xeb.blank-no eq cas.bnum
    no-lock

    with no-labels no-box:

   assign
    v-blk-frm = if v-summ then "*"
                else string(cas.snum,">9") + "-" + string(cas.bnum,"99")
    v-cas-cnt = if xeb.cas-cnt ne 0 then xeb.cas-cnt
                else w-cas.units / w-cas.qty.

   {sys/inc/roundup.i v-cas-cnt}
   IF NOT gEstSummaryOnly THEN
   display v-blk-frm
           item.i-name                          format "x(20)"
           space(0)
           v-cas-cnt                            format ">>>>9"
             when not v-summ
           "Pieces/Case"
             when not v-summ
           w-cas.qty                    to 50   format ">>>>9"
           "Cas"
           cas.cosm                     to 69
             when not v-summ
           w-cas.cost / (w-cas.units / 1000)
             when v-summ                @ cas.cosm
           w-cas.cost                   to 80   format ">>>>,>>9.99"
           SKIP WITH STREAM-IO.
end.

for each cas where cas.typ eq 5
    by cas.snum
    by cas.bnum:

   find first xeb
       where xeb.company  eq cocode
         and xeb.est-no   eq xest.est-no
         and xeb.form-no  eq cas.snum
         and xeb.blank-no eq cas.bnum
       no-lock no-error.
   find first item
       {sys/look/itemW.i}
         and item.i-no eq xeb.layer-pad
       no-lock no-error.

   cas.t-qty = 0.

   FOR EACH xcas WHERE xcas.typ EQ 5 AND xcas.ino EQ cas.ino:
       cas.t-qty = cas.t-qty + xcas.qty.
   END.

   {est/matcost.i cas.t-qty cas.cost 5}

   cas.cost = (cas.cost * cas.qty) + lv-setup-5.

   ASSIGN
    zzz      = cas.cosm
    /* cosm was set to tot # blanks this item; set to cost now */
    cas.cosm = cas.cost / (cas.cosm / 1000)
   v-cas-cnt = if xeb.cas-cnt eq 0 then (zzz / cas.qty) else xeb.cas-cnt.

   {sys/inc/roundup.i v-cas-cnt}

   find first brd where brd.form-no = cas.snum and
                        brd.blank-no = cas.bnum and
                        brd.i-no    = cas.ino
                        no-error.

   if not avail brd then do:
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
      brd.cost-m = cas.cosm
      brd.amount = brd.amount + cas.cost.

   find first w-cas where w-cas.rec-id eq recid(cas) no-error.
   if not avail w-cas then
   find first w-cas where w-cas.i-no eq cas.ino no-error.
   if avail w-cas then
     assign
      w-cas.qty  = w-cas.qty  + cas.qty
      w-cas.cost = w-cas.cost + cas.cost.
      
   zzz = 0.
end.

for each cas where cas.typ eq 5,

    first w-cas where w-cas.rec-id eq recid(cas),

    first item
    {sys/look/itemW.i}
      and item.i-no eq cas.ino
    no-lock,

    first xeb
    where xeb.company  eq cocode
      and xeb.est-no   eq xest.est-no
      and xeb.form-no  eq cas.snum
      and xeb.blank-no eq cas.bnum
    no-lock

    with no-labels no-box:

   assign
    v-blk-frm = if v-summ then "*"
                else string(cas.snum,">9") + "-" + string(cas.bnum,"99")
    v-cas-cnt = if xeb.cas-cnt ne 0 then xeb.cas-cnt
                else w-cas.units / w-cas.qty.

   {sys/inc/roundup.i v-cas-cnt}

   IF NOT gEstSummaryOnly THEN
   display v-blk-frm
           item.i-name                          format "x(20)"
           space(0)
           w-cas.qty                    to 50   format ">>>>9"
           "Cas"
           cas.cosm                     to 69
             when not v-summ
           w-cas.cost / (w-cas.units / 1000)
             when v-summ                @ cas.cosm
           w-cas.cost                   to 80   format ">>>>,>>9.99"
           SKIP WITH STREAM-IO.
end.

for each cas where cas.typ eq 6
    by cas.snum
    by cas.bnum:

   find first xeb
       where xeb.company  eq cocode
         and xeb.est-no   eq xest.est-no
         and xeb.form-no  eq cas.snum
         and xeb.blank-no eq cas.bnum
       no-lock no-error.
   find first item
       {sys/look/itemW.i}
         and item.i-no eq xeb.divider
       no-lock no-error.

   cas.t-qty = 0.

   FOR EACH xcas WHERE xcas.typ EQ 6 AND xcas.ino EQ cas.ino:
     cas.t-qty = cas.t-qty + xcas.qty.
   END.

   {est/matcost.i cas.t-qty cas.cost 6}

   cas.cost = (cas.cost * cas.qty) + lv-setup-5.

   ASSIGN
    zzz      = cas.cosm
    /* cosm was set to tot # blanks this item; set to cost now */
    cas.cosm = cas.cost / (cas.cosm / 1000)
    v-cas-cnt = if xeb.cas-cnt eq 0 then (zzz / cas.qty) else xeb.cas-cnt.

   {sys/inc/roundup.i v-cas-cnt}

   find first brd where brd.form-no = cas.snum and
                        brd.blank-no = cas.bnum and
                        brd.i-no    = cas.ino
                        no-error.
   if not avail brd then do:
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
      brd.cost-m = cas.cosm
      brd.amount = brd.amount + cas.cost.

   find first w-cas where w-cas.rec-id eq recid(cas) no-error.
   if not avail w-cas then
   find first w-cas where w-cas.i-no eq cas.ino no-error.
   if avail w-cas then
     assign
      w-cas.qty  = w-cas.qty  + cas.qty
      w-cas.cost = w-cas.cost + cas.cost.
      
   zzz = 0.
end.

for each cas where cas.typ eq 6,

    first w-cas where w-cas.rec-id eq recid(cas),

    first item
    {sys/look/itemW.i}
      and item.i-no eq cas.ino
    no-lock,

    first xeb
    where xeb.company  eq cocode
      and xeb.est-no   eq xest.est-no
      and xeb.form-no  eq cas.snum
      and xeb.blank-no eq cas.bnum
    no-lock

    with no-labels no-box:

   assign
    v-blk-frm = if v-summ then "*"
                else string(cas.snum,">9") + "-" + string(cas.bnum,"99")
    v-cas-cnt = if xeb.cas-cnt ne 0 then xeb.cas-cnt
                else w-cas.units / w-cas.qty.

   {sys/inc/roundup.i v-cas-cnt}

   IF NOT gEstSummaryOnly THEN
   display v-blk-frm
           item.i-name                          format "x(20)"
           space(0)
           w-cas.qty                    to 50   format ">>>>9"
           "Cas"
           cas.cosm                     to 69
             when not v-summ
           w-cas.cost / (w-cas.units / 1000)
             when v-summ                @ cas.cosm
           w-cas.cost                   to 80   format ">>>>,>>9.99"
           SKIP WITH STREAM-IO.
end.

/**************** P A L L E T S *************************/
for each xeb where xeb.company = xest.company
               AND xeb.est-no = xest.est-no :
   if xeb.tr-no eq "" then do:
     find first cust
         where cust.company EQ xest.company
           and cust.cust-no eq xeb.cust-no
         no-lock no-error.
     if avail cust then xeb.tr-no = cust.pallet.
     if xeb.tr-no eq "" then xeb.tr-no = ce-ctrl.def-pal.
   end.
   find first item
       {sys/look/itemW.i}
         and item.i-no eq xeb.tr-no
       no-lock no-error.
   find first e-item of item no-lock no-error.
   if xeb.cas-no ne "" then do with frame ac33 no-box no-labels:
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
      if not avail cas then do:
         create cas.
         assign
            cas.typ  = 3
            cas.id   = xeb.part-no
            cas.snum = xeb.form-no
            cas.bnum = xeb.blank-no
            cas.ino  = item.i-no
            cas.dscr = item.est-dscr.
      end.
      IF xeb.spare-int-3 GT 0 THEN 
          iCaseMult = xeb.spare-int-3.
      ELSE
          iCaseMult = 1.
      dCaseQty = xcas.qty / iCaseMult.
      cas.cosm = cas.cosm + if xeb.yrprice then xeb.yld-qty else xeb.bl-qty.
      if xeb.cas-pal ne 0 then p-qty = dCaseQty /*xcas.qty*/ / xeb.cas-pal.
         else p-qty = ((((xeb.t-sqin - xeb.t-win) * xeb.yld-qty / 144000)
                       * b-wt) + dCaseQty /*xcas.qty*/) / item.avg-w.
      {sys/inc/roundup.i p-qty}
      cas.qty = cas.qty + p-qty.

      release w-cas.
      if v-summ then
      find first w-cas where w-cas.i-no eq cas.ino no-error.

      if not avail w-cas then do:
        create w-cas.
        assign
         w-cas.i-no   = cas.ino
         w-cas.rec-id = recid(cas).
      end.

      w-cas.units = w-cas.units + xeb.yld-qty.
   end.
end.

for each cas where cas.typ eq 3
    by cas.snum
    by cas.bnum:

   find first xeb
       where xeb.company  eq cocode
         and xeb.est-no    eq xest.est-no
         and xeb.form-no  eq cas.snum
         and xeb.blank-no eq cas.bnum
       no-lock no-error.
   find first item
       {sys/look/itemW.i}
         and item.i-no eq xeb.tr-no
       no-lock no-error.

   find first e-item of item no-lock no-error.

   cas.t-qty = 0.

   FOR EACH xcas WHERE xcas.typ EQ 3 AND xcas.ino EQ cas.ino:
     cas.t-qty = cas.t-qty + xcas.qty.
   END.

   IF xeb.tr-cost GT 0 THEN cas.cost = xeb.tr-cost * cas.qty.

   ELSE DO:
     {est/matcost.i cas.t-qty cas.cost 3}

     cas.cost = (cas.cost * cas.qty) + lv-setup-3.
   END.

   /* cosm was set to tot # blanks this item; set to cost now */
   cas.cosm = cas.cost / (cas.cosm / 1000).

   find first brd where brd.form-no = cas.snum and
                        brd.blank-no = cas.bnum and
                        brd.i-no    = cas.ino
                        no-error.
   if not avail brd then do:
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
      brd.cost-m = cas.cosm
      brd.amount = brd.amount + cas.cost.

   find first w-cas where w-cas.rec-id eq recid(cas) no-error.
   if not avail w-cas then
   find first w-cas where w-cas.i-no eq cas.ino no-error.
   if avail w-cas then
     assign
      w-cas.qty  = w-cas.qty  + cas.qty
      w-cas.cost = w-cas.cost + cas.cost.
end.

for each cas where cas.typ eq 3,
    first w-cas where w-cas.rec-id eq recid(cas),

    first item
    {sys/look/itemW.i}
      and item.i-no eq cas.ino
    no-lock

    with no-labels no-box:

   v-blk-frm = if v-summ then "*"
               else string(cas.snum,">9") + "-" + string(cas.bnum,"99").
   IF NOT gEstSummaryOnly THEN
   display v-blk-frm
           item.i-name                          format "x(20)"
           w-cas.qty                    to 50   format ">>>9"
           "Pal"
           cas.cosm                     to 69
             when not v-summ
           w-cas.cost / (w-cas.units / 1000)
             when v-summ                @ cas.cosm
           w-cas.cost                   to 80   format ">>>>,>>9.99"
           skip WITH STREAM-IO.

  delete w-cas.
end.

for each cas where cas.typ = 1:
   find first blk where blk.id = cas.id and
                      blk.snum = cas.snum and
                      blk.bnum = cas.bnum no-error.
   find first item {sys/look/itemW.i} and item.i-no = cas.ino
   no-lock no-error.
   if avail blk then do:
      /* assign proper cost to blk record */
      ASSIGN
         blk.cost  = blk.cost  + cas.cost
         dm-tot[5] = dm-tot[5] + cas.cost.

      if ctrl[3] ne 0 THEN
         ASSIGN
            ctrl2[3] = ctrl2[3] + (((cas.qty * item.basis-w) / 100) * ctrl[3])
            blk.cost = blk.cost + (((cas.qty * item.basis-w) / 100) * ctrl[3])
            blk.lab  = blk.lab  + (((cas.qty * item.basis-w) / 100) * ctrl[3]).
   end.
end.

for each cas where cas.typ = 5 or cas.typ = 6:
   find first blk where blk.id = cas.id and
                      blk.snum = cas.snum and
                      blk.bnum = cas.bnum no-error.
   find first item {sys/look/itemW.i} and item.i-no = cas.ino
   no-lock no-error.
   if avail blk then do:
      /* assign proper cost to blk record */
      ASSIGN
         blk.cost  = blk.cost  + cas.cost
         dm-tot[5] = dm-tot[5] + cas.cost.

      if ctrl[3] ne 0 THEN
         ASSIGN
         ctrl2[3] = ctrl2[3] + (((cas.qty * item.weight-100) / 100) * ctrl[3])
         blk.cost = blk.cost + (((cas.qty * item.weight-100) / 100) * ctrl[3])
         blk.lab  = blk.lab  + (((cas.qty * item.weight-100) / 100) * ctrl[3]).
      
   end.
end.

for each cas where cas.typ = 3:
   find first blk where blk.id = cas.id and
                      blk.snum = cas.snum and
                      blk.bnum = cas.bnum no-error.
   find first item {sys/look/itemW.i} and item.i-no = cas.ino
   no-lock no-error.
   if avail blk then do:
      /* assign proper cost to blk record */
      ASSIGN
         blk.cost  = blk.cost  + cas.cost
         dm-tot[5] = dm-tot[5] + cas.cost.
      if ctrl[3] ne 0 THEN
         ASSIGN
            ctrl2[3] = ctrl2[3] + (((cas.qty * item.basis-w) / 100) * ctrl[3])
            blk.cost = blk.cost + (((cas.qty * item.basis-w) / 100) * ctrl[3])
            blk.lab  = blk.lab  + (((cas.qty * item.basis-w) / 100) * ctrl[3]).
   end.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
