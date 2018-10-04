/* ----------------------------------------------------- ce/pr4-cas.p 4/92 cd */
{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.
DEF SHARED VAR qty AS INT NO-UNDO.
DEF BUFFER b-item FOR ITEM.

{ce/print4.i shared shared}

def var v-cas-cnt as DEC NO-UNDO.
DEF VAR li-qty LIKE c-qty NO-UNDO.
DEF VAR v-t-win AS DEC DECIMALS 4 NO-UNDO.
DEF VAR v-li AS INT NO-UNDO.
DEFINE VARIABLE iCaseMult AS INTEGER NO-UNDO.

DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

{ce/msfcalc.i}

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

v-t-win = 0.

IF xeb.est-type EQ 1 THEN
do v-li = 1 TO 4:
   find first b-item WHERE
        b-item.company EQ xef.company and
        b-item.i-no eq xef.leaf[v-li]
        no-lock no-error.

   if avail b-item and b-item.mat-type eq "W" and
      (xef.leaf-l[v-li] ne 0 and xef.leaf-w[v-li] ne 0) then
      DO:
         IF xef.leaf-bnum[v-li] EQ 0 THEN
            v-t-win = v-t-win + (xef.leaf-l[v-li] * xef.leaf-w[v-li] / xeb.num-up).
         ELSE
            v-t-win = v-t-win + (xef.leaf-l[v-li] * xef.leaf-w[v-li]).
      END.
end.
ELSE
   v-t-win = xeb.t-win.

   /* case */
   if xeb.cas-no ne "" then do with frame ac2 no-box no-labels:
      find first item {sys/look/itemW.i} and item.i-no = xeb.cas-no
      no-lock no-error.
      if available item then find first e-item of item no-lock no-error.

      if xeb.cas-cnt ne 0 then c-qty = qty / xeb.cas-cnt.
      else
      c-qty = (qty * b-wt *
               (if v-corr then ((xeb.t-sqin - v-t-win) * .000007)
                          else ((xeb.t-sqin - v-t-win) / 144000))) /
              (if xeb.cas-wt ne 0 then xeb.cas-wt else item.avg-w).

      {sys/inc/roundup.i c-qty}
      
      /*02031503-set case qty based on multipliers for cost and material calculations*/
      IF xeb.spare-int-3 GT 0 THEN 
          iCaseMult = xeb.spare-int-3.
      ELSE
          iCaseMult = 1.
      c-qty = c-qty * iCaseMult.

      IF xeb.cas-cost GT 0 THEN c-cost = xeb.cas-cost * c-qty.
      
      ELSE DO:
        {est/matcost.i c-qty c-cost case}

        c-cost = (c-cost * c-qty) + lv-setup-case.
      END.

      ASSIGN 
       dm-tot[4] = dm-tot[4] + (c-cost / (qty / 1000)).
       dm-tot[5] = dm-tot[5] + c-cost.

      find first BRD where BRD.form-no = xeb.form-no and
                           BRD.blank-no = xeb.blank-no and
                           BRD.i-no    = xeb.cas-no
                           no-error.
      if not available BRD then
      do:
         create BRD.
         assign BRD.form-no = xeb.form-no
                BRD.blank-no = xeb.blank-no
                BRD.i-no    = xeb.cas-no
                BRD.dscr    = item.est-dscr
                BRD.basis-w = item.basis-w.
      end.

      ASSIGN
         BRD.qty = c-qty
         BRD.qty-uom = "Ea"
         BRD.sc-uom  = "Ea"
         BRD.cost = c-cost / c-qty
         BRD.cost-m = c-cost / (qty / 1000)
         v-cas-cnt = if xeb.cas-cnt eq 0 then (qty / c-qty) else xeb.cas-cnt.

      {sys/inc/roundup.i v-cas-cnt}

      display item.i-name format "x(20)"
              v-cas-cnt format ">>>>9" "Pieces/Case"
              c-qty format ">>>>>9" to 48 "Cas"
              c-cost / (qty / 1000) format ">>>>9.99" to 68
              c-cost format ">,>>>,>>9.99" to 80 SKIP WITH STREAM-IO.
      /*02031503-reset case qty for calculation of layers and dividers*/
      IF iCaseMult GT 1 THEN c-qty = c-qty / iCaseMult.
   end.

   if xeb.cas-no ne "" and xeb.layer-pad ne "" and xeb.lp-up ne 0 then
   do with frame ac3 no-box no-labels:
      p-cost = 0.

      /* layer pad */
      find first item
          {sys/look/itemW.i}
            and item.i-no eq xeb.layer-pad
          no-lock no-error.
      if available item then find first e-item of item no-lock no-error.
      
      IF xeb.spare-char-3 EQ "P" THEN DO:
          li-qty = c-qty / xeb.cas-pal.
          {sys/inc/roundup.i li-qty}
          li-qty = li-qty * xeb.lp-up.  /*per pallet*/
      END.
      ELSE
          li-qty = c-qty * xeb.lp-up. /*per case - DEFAULT*/

      {sys/inc/roundup.i li-qty}

      {est/matcost.i li-qty p-cost layer-pad}

      p-cost = (p-cost * li-qty) + lv-setup-layer-pad.

      ASSIGN
       dm-tot[4] = dm-tot[4] + (p-cost / (qty / 1000))
       dm-tot[5] = dm-tot[5] + p-cost.

      find first BRD where BRD.form-no = xeb.form-no and
                           BRD.blank-no = xeb.blank-no and
                           BRD.i-no    = xeb.layer-pad
                           no-error.
      if not available BRD then
      do:
         create BRD.
         assign BRD.form-no = xeb.form-no
                BRD.blank-no = xeb.blank-no
                BRD.i-no    = xeb.layer-pad
                BRD.dscr    = item.est-dscr
                BRD.basis-w = item.basis-w.
      end.
      ASSIGN
      BRD.qty = li-qty
      BRD.qty-uom = "Ea"
      BRD.sc-uom  = "Ea"
      BRD.cost = p-cost / li-qty
      BRD.cost-m = p-cost / (qty / 1000).

      if p-cost ne 0 then
      display item.i-name li-qty format ">>>>>9" to 48 "Ea."
              p-cost / (qty / 1000) format ">>>>9.99" to 68
              p-cost format ">,>>>,>>9.99" to 80 SKIP WITH STREAM-IO.
   end.

   if xeb.cas-no ne "" and xeb.divider ne "" and xeb.div-up ne 0 then
   do with frame ac4 no-box no-labels:
      p-cost = 0.

      /* layer pad */
      find first item
          {sys/look/itemW.i}
            and item.i-no eq xeb.divider
          no-lock no-error.
      if available item then find first e-item of item no-lock no-error.
      
      IF xeb.spare-char-4 EQ "P" THEN DO:
          li-qty = c-qty / xeb.cas-pal.
          {sys/inc/roundup.i li-qty}
          li-qty = li-qty * xeb.div-up.  /*per pallet*/
      END.
      ELSE
          li-qty = c-qty * xeb.div-up. /*per case - DEFAULT*/

      {sys/inc/roundup.i li-qty}

      {est/matcost.i li-qty p-cost divider}

      ASSIGN
       p-cost = (p-cost * li-qty) + lv-setup-divider
       dm-tot[4] = dm-tot[4] + (p-cost / (qty / 1000))
       dm-tot[5] = dm-tot[5] + p-cost.

      find first BRD where BRD.form-no = xeb.form-no and
                           BRD.blank-no = xeb.blank-no and
                           BRD.i-no    = xeb.divider
                           no-error.
      if not available BRD then
      do:
         create BRD.
         assign BRD.form-no = xeb.form-no
                BRD.blank-no = xeb.blank-no
                BRD.i-no    = xeb.divider
                BRD.dscr    = item.est-dscr
                BRD.basis-w = item.basis-w.
      end.

      ASSIGN
         BRD.qty = li-qty
         BRD.qty-uom = "Ea"
         BRD.sc-uom  = "Ea"
         BRD.cost = p-cost / li-qty
         BRD.cost-m = p-cost / (qty / 1000).

      if p-cost ne 0 then
      display item.i-name li-qty format ">>>>>9" to 48 "Ea."
              p-cost / (qty / 1000) format ">>>>9.99" to 68
              p-cost format ">,>>>,>>9.99" to 80 SKIP WITH STREAM-IO.
   end.

   if xeb.cas-no ne "" then do with frame ac5 no-box no-labels:
      p-cost = 0.

      /* pallet */
      if xeb.tr-no eq "" then do:
        find eb where recid(eb) eq recid(xeb).
        find first cust
            where cust.company eq cocode
              and cust.cust-no eq xeb.cust-no
            no-lock no-error.
        if avail cust then eb.tr-no = cust.pallet.
        if eb.tr-no eq "" then eb.tr-no = ce-ctrl.def-pal.
        find xeb where recid(xeb) eq recid(eb) no-lock.
        release eb.
      end.
      find first item
          {sys/look/itemW.i}
            and item.i-no eq xeb.tr-no
          no-lock no-error.
      if available item then  find first e-item of item no-lock no-error.
      if xeb.cas-pal ne 0 then p-qty = c-qty / xeb.cas-pal.
      else p-qty = ((((xeb.t-sqin - v-t-win) * qty / 144000) * b-wt) + c-qty )
                    / item.avg-w.
                    /* ce-ctrl.avg-palwt was previosly used. */
      {sys/inc/roundup.i p-qty}

      IF xeb.tr-cost GT 0 THEN p-cost = xeb.tr-cost * p-qty.
      
      ELSE DO:
        {est/matcost.i p-qty p-cost pallet}

        p-cost = (p-cost * p-qty) + lv-setup-pallet.
      END.

      ASSIGN
       dm-tot[4] = dm-tot[4] + (p-cost / (qty / 1000))
       dm-tot[5] = dm-tot[5] + p-cost.

      find first BRD where BRD.form-no = xeb.form-no and
                           BRD.blank-no = xeb.blank-no and
                           BRD.i-no    = xeb.tr-no
                           no-error.
      if not available BRD then
      do:
         create BRD.
         assign BRD.form-no = xeb.form-no
                BRD.blank-no = xeb.blank-no
                BRD.i-no    = xeb.tr-no
                BRD.dscr    = item.est-dscr
                BRD.basis-w = item.basis-w.
      end.

      ASSIGN
         BRD.qty = p-qty
         BRD.qty-uom = "Ea"
         BRD.sc-uom  = "Ea"
         BRD.cost = p-cost / p-qty
         BRD.cost-m = p-cost / (qty / 1000).

      if p-cost ne 0 then
      display item.i-name p-qty format ">>>>>9" to 48 "Ea."
              p-cost / (qty / 1000) format ">>>>9.99" to 68
              p-cost format ">,>>>,>>9.99" to 80 SKIP WITH STREAM-IO.
   end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
