/* shprpt1_.i  RM Board cost list 
   05/22/2001  Created by Laura Bryant
*/

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'shrpt1_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="item" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="item" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="item" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="item" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/
/*
{methods/lstlogic/shownote.i &db_table="item" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="item" &col="5" &frame-name="f-miscflds"}
*/

def var cocode as cha no-undo.
def var locode as cha no-undo.
def var save_id as recid no-undo.
def var i as int no-undo.
def var j as int no-undo.
def var time_stamp as ch no-undo.
time_stamp = string(time, "HH:MMam").

def var head1 as ch format "x(20)".
def var head2 as ch format "x(19)".
def var head3 as ch format "x(20)".
def var head4 as ch format "x(15)".
def var head5 as ch format "x(39)".
def var head6 as ch format "x(29)".
def var head21 as ch format "x(76)".
def var rm-cst-type like rm-ctrl.avg-lst-cst.
def var rm-cst-amt like item.last-cost.

head1 = " === Board/Paper ===".
head2 = "======= Ink =======".
head3 = "== Film/Leaf/Glue ==".
head4 = "== Case/Pall.==".
head5 = "===== Board/Paper Speed Reduction =====".
head6 = "   Valid Estimated Roll Width".
head21 =
"            On Hand      On Order     Committed   Backordered     Available".

def var fco as ch.
def var tco like fco.
def var floc as ch.
def var tloc like floc.
def var fcat as ch initial "000000".
def var tcat like fcat initial "ZZZZZZ".
def var doe    as logical initial true.
def var dor    as logical initial true.
def var detail as logical initial false.

form
   skip
  "  Item No:" to 10 item.i-no     " Item Code:" to 60 item.i-code    skip
  "     Name:" to 10 item.i-name   "Mat'l Type:" to 60 item.mat-type  skip
  "     Desc:" to 10 item.i-dscr   " Cost Type:" to 60 item.cost-type skip
  " Est.Desc:" to 10 item.est-dscr "  Category:" to 60 item.procat    skip
  "  Caliper:" item.cal     to 20 " Sheet Len:" to 45 item.s-len skip
  " Basis Wt:" item.basis-w to 20 " Sheet Wid:" to 45 item.s-wid skip
  "    Reg.#:" item.reg-no  to 20 "  Roll Wid:" to 45 item.r-wid skip
  " Shrink %:" item.shrink  to 20  skip
  space(21) head5 skip
  "   Department Name: "
  space(2) item.dept-name[1] space(2) item.dept-name[2]
  space(2) item.dept-name[3] space(2) item.dept-name[4]
  space(2) item.dept-name[5] space(2) item.dept-name[6]
  space(2) item.dept-name[7] space(2) item.dept-name[8]
  space(2) item.dept-name[9] space(2) item.dept-name[10] skip
  "       Reduction %: " item.speed%[1 for 10]
  skip
  with frame item row 2 width 80 overlay no-labels no-underline
       /*color value(col-bg) prompt value(col-input)*/.

form
  " Vendor 1:" item.vend-no  "Item #:" item.vend-item
    "Auto Alloc.?" item.alloc   skip
  " Vendor 2:" item.vend2-no "Item #:" item.vend2-item
    "    Stocked?" item.stocked skip
  " Reorder Policy:" item.ord-policy
	"Purchased or Manf?" to 65 item.pur-man skip
  " Reorder Level :" item.ord-level
	"Purchased UOM:" to 65 item.pur-uom skip
  " Minimum Order :" item.ord-min
	"Lead Time (Days):" to 65 item.lead-days skip
  " Maximum Order :" item.ord-max
	"Beg Balance :" to 60 item.beg-bal skip
	"Beg Bal Date:" to 60 item.beg-date
  "  Warehouse:" item.loc loc.dscr format "x(25)"
      "Last Count:" to 60 item.last-count skip
  "        Bin:" item.loc-bin "Count Date:" to 60 item.last-date  skip
  " Cycle Code:" item.cc-code space(10) "  Cons. UOM:" item.cons-uom
      "Last Cost :" to 60 item.last-cost  skip
  " Purch. Rpt Code:" item.pur-rcode "Prod.Code:" item.pic-code
      "Avg. Cost :" to 60 item.avg-cost skip
  head21 skip
  " QTY:" item.q-onh item.q-ono item.q-comm item.q-back item.q-avail skip(1)
with frame item2 row 2 width 80 overlay no-labels no-underline
       /*color value(col-bg) prompt value(col-input)*/.

form skip(1)
     /*
     "   For Company :" fco   space(5) "To Company :" tco   space(5) skip(1)
     "   For Location:" floc  space(5) "To Location:" tloc  skip(1)
     */
     "   For Category:" fcat  space(5) "To Category:" tcat  skip(1)
     "        List Real Materials?" dor skip
     "   List Estimated Materials?" doe skip
     "          Detail Real Items?" detail skip(1)
     with title "  B O A R D  - P A P E R   L I S T  "
     frame selec row 8 centered overlay no-labels
	  /*color value(col-title) prompt value(col-input)*/.
form
    item.procat
    item.i-no
    item.i-name    format "x(27)"
    item.cal
    item.basis-w
    item.last-cost
    item.cons-uom
    item.q-onh     format "->>>>,>>9.999"
    item.q-ono     format "->>>>,>>9.999"
    item.q-comm    format  ">>>>,>>9.999"
    item.q-avail   format "->>>>,>>9.999"
    skip
header
"CAT   ITEM       DESCRIPTION                 CALIPER WEIGHT     COST     UOM       On Hand      On Order     Allocated     Available"
    with frame itemx no-box no-labels down width 132.

form
     item.procat item.i-no item.i-name item.cal item.basis-w
     space(3) e-item.roll-w[1] space(3) e-item.roll-w[2]
     space(3) e-item.roll-w[3] space(3) e-item.roll-w[4]
     space(3) e-item.roll-w[5]
header
"CAT   ITEM       DESCRIPTION                    CALIPER WEIGHT   <               Valid   Roll   Widths               >"
    with frame iteme no-box no-labels down width 119 column 8.


{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}
assign fcat = begin_procat
       tcat = end_procat
       cocode = gcompany
       locode = gloc
       dor = is-real-mat
       doe = is-est-mat 
       .
       

find first ce-ctrl where ce-ctrl.company = cocode and ce-ctrl.loc = locode
no-lock no-error.

 
      if dor then
      do:
message "dor" view-as alert-box.
	      /*{sys/inc/outprint.i 58}*/
	      view frame r-top.
	      if not detail then
	      for each item
	      where /*
		    (item.company >= fco and item.company <= tco)     and
		    (item.loc >= floc and item.loc <= tloc)           and
		    */
		    item.company = cocode and item.loc = locode and
		    (item.i-code = "R")                               and
		    (item.mat-type = "B" or item.mat-type = "P")      and
		    (item.procat >= fcat and item.procat <= tcat)
	      break by item.company by item.loc by item.i-code
	      by item.mat-type by item.procat by item.i-no with frame itemx:
		 /* CTS */
		 clear frame itemx all no-pause.
		 /* CTS */

       find first rm-ctrl where rm-ctrl.company = cocode.
	 if rm-ctrl.avg-lst-cst = true then rm-cst-amt = item.avg-cost.
	 if rm-ctrl.avg-lst-cst = false then rm-cst-amt = item.last-cost.

		 if line-counter > 56 then page.
		 display
		    /*
		    item.company
		    item.loc
		    item.i-code
		    */
		    item.procat  when first-of(item.procat)
		    item.i-no
		    item.i-name
		    item.cal
		    item.basis-w
		    /*ce-ctrl.r-cost = no*/
		    rm-cst-amt @ item.last-cost
		    /*ce-ctrl.r-cost = yes @ item.last-cost*/
		    item.cons-uom
		    item.q-onh
		    item.q-ono
		    item.q-comm
		    item.q-avail.
		 down.
	      end.  /* for each . not detail */
	      else
	      for each item where item.company = cocode and item.loc = locode  and
			     (item.i-code = "R") and
			     (item.mat-type = "B" or item.mat-type = "P") and
			     (item.procat >= fcat and item.procat <= tcat)
	      break by item.company by item.loc by item.i-code
	      by item.mat-type by item.procat by item.i-no with frame item:
	      /* CTS */
	      clear frame item all no-pause.
	      /* CTS */
		 if line-counter > 35 then page.
		 display

			head5
			item.i-no
			item.i-name
			item.i-dscr
			item.est-dscr
			item.i-code
			item.mat-type
			item.cost-type
			item.procat
			head5
			item.cal     when item.cal     ne 0
			item.basis-w when item.basis-w ne 0
			item.reg-no  when item.reg-no  ne ""
			item.shrink  when item.shrink  ne 0
			item.s-len   when item.s-len   ne 0
			item.s-wid   when item.s-wid   ne 0
			item.r-wid   when item.r-wid   ne 0
			item.dept-name[1 ] when item.dept-name[1 ] ne ""
			  item.speed%[1 ] when     item.speed[1 ] ne 0
			item.dept-name[2 ] when item.dept-name[2 ] ne ""
			  item.speed%[2 ] when     item.speed[2 ] ne 0
			item.dept-name[3 ] when item.dept-name[3 ] ne ""
			  item.speed%[3 ] when     item.speed[3 ] ne 0
			item.dept-name[4 ] when item.dept-name[4 ] ne ""
			  item.speed%[4 ] when     item.speed[4 ] ne 0
			item.dept-name[5 ] when item.dept-name[5 ] ne ""
			  item.speed%[5 ] when     item.speed[5 ] ne 0
			item.dept-name[6 ] when item.dept-name[6 ] ne ""
			  item.speed%[6 ] when     item.speed[6 ] ne 0
			item.dept-name[7 ] when item.dept-name[7 ] ne ""
			  item.speed%[7 ] when     item.speed[7 ] ne 0
			item.dept-name[8 ] when item.dept-name[8 ] ne ""
			  item.speed%[8 ] when     item.speed[8 ] ne 0
			item.dept-name[9 ] when item.dept-name[9 ] ne ""
			  item.speed%[9 ] when     item.speed[9 ] ne 0
			item.dept-name[10] when item.dept-name[10] ne ""
		     item.speed%[10] when     item.speed[10] ne 0.

	    do with frame item2:
		    find first loc where loc.company = cocode and
					     loc.loc = item.loc no-lock no-error.

	      /* CTS */
	      clear frame item2 all no-pause.
	      /* CTS */
	       display
		  item.vend-no
		  item.vend-item
		  item.alloc
		  item.vend2-no
		  item.vend2-item
		  item.stocked
		  item.ord-policy
		  item.ord-level   when item.ord-level ne 0
		  item.ord-min     when item.ord-min   ne 0
		  item.ord-max     when item.ord-max   ne 0
		  item.pur-man
		  item.pur-uom
		  item.lead-days   when item.lead-days ne 0
		  item.loc
		  loc.dscr         when available loc
		  item.loc-bin
		  item.cc-code
		  item.cons-uom
		  item.pur-rcode
		  item.pic-code
		  item.beg-bal      when item.beg-bal ne 0
		  item.beg-date
		  item.last-count   when item.last-count ne 0
		  item.last-date
		  item.last-cost    when item.last-cost  ne 0
		  item.avg-cost     when item.avg-cost   ne 0
		  head21
		  item.q-onh        when item.q-onh   ne 0
		  item.q-ono        when item.q-ono   ne 0
		  item.q-comm       when item.q-comm  ne 0
		  item.q-back       when item.q-back  ne 0
		  item.q-avail      when item.q-avail ne 0.

		 end.  /* do with frame item2 */
	      end.  /* for each . detail */
      end.  /* dor */

      if doe then
      do:
/*	      if dor then
    do: {sys/inc/close.i} end.
	      {sys/inc/outprint.i 58}
						 /* to reset page-number*/
*/
	      view frame r-top.
	      for each item
	      where /*
		    (item.company >= fco and item.company <= tco)     and
		    (item.loc >= floc and item.loc <= tloc)           and
		    */
		    item.company = cocode and item.loc = locode and
		    (item.i-code = "E")                               and
		    (item.mat-type = "B" or item.mat-type = "P")      and
		    (item.procat >= fcat and item.procat <= tcat)
	      break by item.company by item.loc by item.i-code
	      by item.mat-type by item.procat by item.i-no with frame iteme:
		 if line-counter > 56 then page.
		 find first e-item where e-item.company = item.company and
				    e-item.loc     = item.loc     and
				    e-item.i-no    = item.i-no no-lock no-error.

		/* CTS */
		clear frame iteme all no-pause.
		/* CTS */
		display
		    /*
		    item.company
		    item.loc
		    item.i-code
		    */
		    "" @ item.procat
		    item.procat when first-of(item.procat)
		    item.i-no
		    item.i-name
		    item.cal
		    item.basis-w
		    e-item.roll-w[1] when e-item.roll-w[1] ne 0
		    e-item.roll-w[2] when e-item.roll-w[2] ne 0
		    e-item.roll-w[3] when e-item.roll-w[3] ne 0
		    e-item.roll-w[4] when e-item.roll-w[4] ne 0
		    e-item.roll-w[5] when e-item.roll-w[5] ne 0.
		 put skip.
		 do i = 1 to 10:
		    if i = 1 then
		    do:
		  put
		  space(16) "Cost--> Up To " e-item.run-qty[1] to 41
		  space(1) e-item.std-uom to 45
		  space(1) "$" e-item.run-cost[1] " per " e-item.std-uom
		  space(6).
		  do j = 1 to 5:
		     if e-item.roll-w[(i * 5) + j] ne 0
		     then put e-item.roll-w[(i * 5) + j] space(3).
		  end.
		  put skip.
		    end.
		    else if e-item.run-qty[i] ne 0 or (i < 6 and
		       e-item.roll-w[(i * 5)] ne 0) then
		    do:
		  if e-item.run-qty[i] ne 0
		  then put e-item.run-qty[i] to 41
			   space(5) "$" e-item.run-cost[i] " " at 72.
		  else put space(72).
		  if i < 6 and e-item.roll-w[i * 5] ne 0 then
		  do j = 1 to 5:
		     if e-item.roll-w[(i * 5) + j] ne 0
		     then put e-item.roll-w[(i * 5) + j] space(3).
		  end.
		  put skip.
		    end.
		 end.
		 put skip(1).
		 down.
	      end.
      end.
   
/*   leave. /* fake loop */
end.      /* outers */
hide frame selec no-pause.
hide frame print no-pause.
*/
/* end ---------------------------------- copr. 1992  advanced software, inc. */

