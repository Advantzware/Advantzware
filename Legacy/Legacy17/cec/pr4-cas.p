/* ---------------------------------------------------- cec/pr4-cas.p 4/92 cd */

def input parameter v-vend-no like e-item-vend.vend-no.

def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def var i as int no-undo.
def var j as int no-undo.
def shared var qty as INT no-undo.
def var tmpstore as cha no-undo.
def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

{cec/print4.i shared shared}

{cec/msfcalc.i}

{cec/rollfac.i}

assign
 save-qty = qty
 qty = qty / xeb.yld-qty.
 
find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

   /* case */
   if xeb.cas-no ne "" then do with frame ac2 no-box no-labels:
      find first item {sys/look/itemW.i} and item.i-no = xeb.cas-no
      no-lock no-error.
      if avail item then find first e-item of item no-lock no-error.
      if xeb.cas-cnt ne 0 then c-qty = qty / xeb.cas-cnt.

      else c-qty = if v-corr then
                     (((xeb.t-sqin - xeb.t-win) * qty * .000007) * b-wt)
                     / (if avail(item) then item.avg-w else 1)
                   else
                     (((xeb.t-sqin - xeb.t-win) * qty / 144000)  * b-wt)
                     / (if avail(item) then item.avg-w else 1).

      if c-qty > int(c-qty) then c-qty = integer(c-qty) + 1.
      else c-qty = integer(c-qty).

      IF xeb.cas-cost GT 0 THEN c-cost = xeb.cas-cost * c-qty.
      
      ELSE DO:
        {est/matcost.i c-qty c-cost case}

        c-cost = (c-cost * c-qty) + lv-setup-case.
      END.

      ASSIGN
       dm-tot[4] = dm-tot[4] + (c-cost / (qty / 1000))
       dm-tot[5] = dm-tot[5] + c-cost.

      find first brd where brd.form-no = xeb.form-no and
                           brd.blank-no = xeb.blank-no and
                           brd.i-no    = xeb.cas-no
                           no-error.
      if not avail brd then
      do:
         create brd.
         assign brd.form-no = xeb.form-no
                brd.blank-no = xeb.blank-no
                brd.i-no    = xeb.cas-no
                brd.dscr    = item.est-dscr
                brd.basis-w = item.basis-w.
      end.
      ASSIGN
      brd.qty = c-qty
      brd.qty-uom = "Ea"
      brd.sc-uom  = "Ea"
      brd.cost = c-cost / c-qty
      brd.cost-m = c-cost / (qty / 1000).

      display item.i-name format "x(20)" when avail(item)
              xeb.cas-cnt format ">>>>9" "Pieces/Pack"
              (qty / c-qty) when xeb.cas-cnt eq 0 @ xeb.cas-cnt
              c-qty format ">>>>>9" to 48 "Bdl"
              lv-setup-case when lv-setup-case ne 0 format ">>>9.99" to 59
              c-cost / (save-qty / 1000) / v-sqft-fac format ">>>>9.99" to 68
              c-cost format ">>>>,>>9.99" to 80 skip with stream-io.

   end.

   /* pallet */
   if xeb.tr-no ne "" then do with frame ac3 no-box no-labels:
      find first item {sys/look/itemW.i} and item.i-no = xeb.tr-no
      no-lock no-error.
      if avail item then  find first e-item of item no-lock no-error.
      if xeb.cas-pal ne 0 then p-qty = c-qty / xeb.cas-pal.

      else p-qty = if v-corr then
                   ((((xeb.t-sqin - xeb.t-win) * qty * .000007) * b-wt) + c-qty)
                   / (if avail(item) then item.avg-w else 1)
                   else
                   ((((xeb.t-sqin - xeb.t-win) * qty / 144000)  * b-wt) + c-qty)
                   / (if avail(item) then item.avg-w else 1).

      if p-qty > integer(p-qty) then p-qty = integer(p-qty) + 1.
      else p-qty = integer(p-qty).

      IF xeb.tr-cost GT 0 THEN p-cost = xeb.tr-cost * p-qty.
      
      ELSE DO:
        {est/matcost.i p-qty p-cost pallet}

        p-cost = (p-cost * p-qty) + lv-setup-pallet.
      END.

      ASSIGN
       dm-tot[4] = dm-tot[4] + (p-cost / (qty / 1000))
       dm-tot[5] = dm-tot[5] + p-cost.

      find first brd where brd.form-no = xeb.form-no and
                           brd.blank-no = xeb.blank-no and
                           brd.i-no    = xeb.tr-no
                           no-error.
      if not avail brd then
      do:
         create brd.
         assign brd.form-no = xeb.form-no
                brd.blank-no = xeb.blank-no
                brd.i-no    = xeb.tr-no
                brd.dscr    = item.est-dscr
                brd.basis-w = item.basis-w.
      end.
      ASSIGN
      brd.qty = p-qty
      brd.qty-uom = "Ea"
      brd.sc-uom  = "Ea"
      brd.cost = p-cost / p-qty
      brd.cost-m = p-cost / (qty / 1000).

      if p-qty ne 0 then pallet-blok: do.
         display item.i-name when avail(item)
                 p-qty format ">>>>>9" to 48 "Ea."
                 lv-setup-pallet when lv-setup-pallet ne 0 format ">>>9.99" to 59
                 p-cost / (save-qty / 1000) / v-sqft-fac format ">>>>9.99" to 68
                 p-cost format ">>>>,>>9.99" to 80 skip with stream-io.

         {cec/pr4-str.i}

         if strap-qty ne 0 then do:
            strap-qty = strap-qty * p-qty / 1000.

            find first item
                where item.company eq cocode
                  and item.i-no    eq strap.code2
                no-lock no-error.

            if not avail item then leave pallet-blok.

            {est/matcost.i strap-qty strap-cst strap}

            strap-cst = (strap-cst * strap-qty) + lv-setup-strap.

            if strap-qty ne 0 then do with frame ac4 no-box no-labels:
               display item.i-name
                       strap-qty format ">>>,>>>"                 to 48 "MLI"
                       lv-setup-strap when lv-setup-strap ne 0 format ">>>9.99" to 59
                       strap-cst / (save-qty / 1000) / v-sqft-fac format ">>>>9.99" to 68
                       strap-cst format ">>>>>>>9.99" to 80 skip
                   with stream-io.

               find first brd
                   where brd.form-no  eq xeb.form-no
                     and brd.blank-no eq xeb.blank-no
                     and brd.i-no     eq item.i-no
                   no-error.
               if not avail brd then do:
                  create brd.
                  assign
                   brd.form-no  = xeb.form-no
                   brd.blank-no = xeb.blank-no
                   brd.i-no     = item.i-no
                   brd.dscr     = item.est-dscr
                   brd.basis-w  = item.basis-w.
               end.
               assign
                brd.qty     = strap-qty
                brd.qty-uom = "MLI"
                brd.sc-uom  = "MLI"
                brd.cost    = strap-cst / strap-qty
                brd.cost-m  = strap-cst / (qty / 1000)
                dm-tot[4]   = dm-tot[4] + (strap-cst / (qty / 1000))
                dm-tot[5]   = dm-tot[5] + strap-cst.
            end.
         end.
      end. /* pallet-blok */
   end.

qty = save-qty.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
