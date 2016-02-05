/* ---------------------------------------------------- cec/pr4-flm.p 4/92 cd */

def input parameter v-vend-no like e-item-vend.vend-no.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

{cec/print4.i shared shared}

def var v-setup like e-item-vend.setup NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.
DEF VAR lv-sqin-lb LIKE item.sqin-lb NO-UNDO.


save-qty = qty.
find first ce-ctrl {sys/look/ce-ctrl.w} no-lock no-error.

RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

/* films */
do i = 1 to 2 with frame ac0  down no-labels no-box:
   if xef.leaf[i] ne "" then do:
      find first item where item.company = cocode and item.loc     = locode and
			    item.i-no    = xef.leaf[i] no-lock no-error.
      if avail item then find first e-item of item no-lock no-error.
      else next.

      if item.mat-type = "W" then do:
	    find first est-op
	        where est-op.company eq xest.company
	          and est-op.est-no  eq xest.est-no
              and est-op.qty     eq v-op-qty
	          and est-op.line    ge 500
	          and est-op.dept    eq "WN"
	        no-lock no-error.
	    if not avail est-op then leave.
	    assign
	     f-qty[i]   = (est-op.num-sh * xeb.num-up * v-n-out *
		               (if v-corr then
			             (xef.leaf-l[i] * xef.leaf-w[i] * .007)
                        else
			             (xef.leaf-l[i] * xef.leaf-w[i] / 144))) /
		              1000 * b-i.basis-w
	     b-uom      = "LB"
         lv-sqin-lb = 1.

	    do j = 1 to 6:
	      if xef.adder[j] ne "" then do:
	        find first b-i
	            where b-i.company eq cocode
		          and b-i.i-no    eq xef.adder[j]
		        no-lock no-error.
	        if avail b-i then 
	          f-qty[i] = f-qty[i] +
		                 ((est-op.num-sh * xeb.num-up * v-n-out *
			              (if v-corr then
			                (xef.leaf-l[i] * xef.leaf-w[i] * .007)
                           else
			                (xef.leaf-l[i] * xef.leaf-w[i] / 144))) /
			              1000 * b-i.basis-w).
	      end.
	    end.

	    f-qty[i] = f-qty[i] * item.shrink.
      end.

      else 
      if item.mat-type = "F" then do:
	    find first est-op
	        where est-op.company eq xest.company
	          and est-op.est-no  eq xest.est-no
              and est-op.qty     eq v-op-qty
		      and est-op.line    ge 500
              and est-op.dept    eq "FS"
            no-lock no-error.
	    if not avail est-op then leave.
	    assign
         f-qty[i] = (xef.leaf-w[i] * xef.leaf-l[i]) *
		            (est-op.num-sh * xeb.num-up * v-n-out)
         lv-sqin-lb = item.sqin-lb.
      end.
      if not avail item then next.

      FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

      b-uom = IF AVAIL e-item AND e-item.std-uom NE "" THEN e-item.std-uom
                                                       ELSE item.cons-uom.
       
      IF b-uom EQ "MSI" THEN
        f-qty[i] = f-qty[i] / 1000.
      ELSE
      IF b-uom EQ "LB" THEN
        f-qty[i] = f-qty[i] / lv-sqin-lb.

      {est/matcost.i f-qty[i] f-cost[i] film}

      ASSIGN
       f-cost[i] = (f-cost[i] * f-qty[i]) + lv-setup-film
       dm-tot[4] = dm-tot[4] + (f-cost[i] / (save-qty / 1000))
       dm-tot[5] = dm-tot[5] + f-cost[i].

      display xef.leaf-dscr[i] f-qty[i]  to 48 space(0) b-uom
	          f-cost[i] / (save-qty / 1000) format ">>>>9.99" to 68
	          f-cost[i] format ">>>>,>>9.99" to 80 skip with stream-io.
   end.
end.
qty = save-qty.

/* end ---------------------------------- copr. 1992  advanced software, inc. */

