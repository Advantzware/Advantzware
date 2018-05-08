/* ----------------------------------------------------- ce/pr4-cas.p 4/92 cd */
{sys/inc/var.i shared}
def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

{ce/print4.i shared shared}

def var v-cas-cnt as DEC NO-UNDO.
DEF SHARED VAR qty AS INT NO-UNDO.
DEFINE VARIABLE iCaseMult AS INTEGER     NO-UNDO.
{ce/msfcalc.i}


DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

   /* case */
   if xeb.cas-no ne "" then do with frame ac2 no-box no-labels:
      find first item {sys/look/itemW.i} and item.i-no = xeb.cas-no
      no-lock no-error.
      find first e-item of item no-lock no-error.

      if xeb.cas-cnt ne 0 then c-qty = qty / xeb.cas-cnt.
      else
      c-qty = (qty * b-wt *
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

      if xeb.cas-cost ne 0 then c-cost = c-qty * xeb.cas-cost.
      else
	  if available e-item then
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
                  tt-ei.run-qty[j + 10] = e-item.run-qty[j]
                  tt-ei.run-cost[j + 10] = e-item.run-cost[j].
            END. 


         do j = 1 to 20:
	        if tt-ei.run-qty[j] < c-qty then next.
	        c-cost = (c-qty * tt-ei.run-cost[j]).
            leave.
	     end.
      END.
	  else do:
	    if ce-ctrl.r-cost = true then c-cost = c-qty * item.avg-cost.
	    else c-cost = c-qty * item.last-cost.
      END.

      assign
      dm-tot[4] = dm-tot[4] + (c-cost / (qty / 1000))
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
		BRD.dscr    = item.i-dscr
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
	      c-qty format ">>>>>9" to 46 "Cas"
	      c-cost / (qty / 1000) format ">>>>9.99" to 68
	      c-cost to 80 format ">,>>>,>>9.99" SKIP WITH STREAM-IO.
      
      /*02031503-reset case qty for calculation of layers and dividers*/
      IF iCaseMult GT 1 THEN c-qty = c-qty / iCaseMult.
   end.

   /* pallet */
   if xeb.cas-no ne "" then do with frame ac3 no-box no-labels:
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
      find first e-item of item no-lock no-error.
      if xeb.cas-pal ne 0 then p-qty = c-qty / xeb.cas-pal.
      else p-qty = ((((xeb.t-sqin - xeb.t-win) * qty / 144000) * b-wt) +
		    (c-qty * 2)) / item.avg-w.
      if p-qty > integer(p-qty) then p-qty = integer(p-qty) + 1.
      else p-qty = integer(p-qty).

      IF xeb.tr-cost GT 0 THEN p-cost = xeb.tr-cost * p-qty.
      else
	  if available e-item then
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
                  tt-ei.run-qty[j + 10] = e-item.run-qty[j]
                  tt-ei.run-cost[j + 10] = e-item.run-cost[j].
            END.

         do j = 1 to 20:
	        if tt-ei.run-qty[j] < p-qty then next.
	        p-cost = (p-qty * tt-ei.run-cost[j]).
            leave.
	     end.
      END.
      else do:
	    if ce-ctrl.r-cost = true then p-cost = p-qty * item.avg-cost.
	    else p-cost = p-qty * item.last-cost.
	  end.

      assign
      dm-tot[4] = dm-tot[4] + (p-cost / (qty / 1000))
      dm-tot[5] = dm-tot[5] + p-cost.

      find first BRD where BRD.form-no = xeb.form-no and
			   BRD.blank-no = xeb.blank-no and
			   BRD.i-no    = ce-ctrl.def-pal
			   no-error.
      if not available BRD then
      do:
	 create BRD.
	 assign BRD.form-no = xeb.form-no
		BRD.blank-no = xeb.blank-no
		BRD.i-no    = ce-ctrl.def-pal
		BRD.dscr    = item.i-dscr
		BRD.basis-w = item.basis-w.
      end.
     ASSIGN
      BRD.qty = p-qty
      BRD.qty-uom = "Ea"
      BRD.sc-uom  = "Ea"
      BRD.cost = p-cost / p-qty
      BRD.cost-m = p-cost / (qty / 1000).

      display item.i-name p-qty format ">>>9" to 46 "Pal"
	      p-cost / (qty / 1000) format ">>>>>9.99" to 68
	      p-cost to 80 format ">,>>>,>>9.99" skip WITH STREAM-IO.
   end.


/* end ---------------------------------- copr. 1992  advanced software, inc. */
