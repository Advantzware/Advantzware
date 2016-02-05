/* --------------------------------------------------- oe/fg-qtys.f 11/96 JLF */
/*                                                                            */
/* Order entry Lines - o/e module - FG quantities                             */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def var fg-qtys-h as char format "x(78)" extent 3.

assign
 fg-qtys-h[2] =
      "             Total     Jobs/POs  Allocated  Total      Total        Reorder"
 fg-qtys-h[3] =
      "             On Hand   On Order  To Orders  Backorder  Available    Level".

form /* fg-qtys-h[1] at 2 */
     "Item:"      at 2
     itemfg.i-no
     fg-qtys-h[2] at 2
     fg-qtys-h[3] at 2
/*     "Qty:"       at 3*/
     itemfg.beg-bal  format "->,>>>,>>9"
     itemfg.q-onh    format "->,>>>,>>9"
     itemfg.q-ono    format "->,>>>,>>9"
     itemfg.q-alloc  format "->,>>>,>>9"
     itemfg.q-back   format "->,>>>,>>9"
     itemfg.q-avail  format "->,>>>,>>9"
     itemfg.ord-level format "->,>>>,>>9"

    with frame fg-qtys row 4 width 80 overlay no-labels no-underline no-box
	 color value(col-bg) prompt value(col-input).

/* end ---------------------------------- copr. 1993  advanced software, inc. */

