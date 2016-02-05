/* rm-reord_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'rm-reord_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="item" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="item" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="item" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="item" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

/* ----------------------------------------------- rm/rep/rm-reord.p 10/93 cd */
/* raw materials - reordering advice report                                   */
/* -------------------------------------------------------------------------- */

/*{sys/inc/var.i shared}
def var save_id as recid.*/

def var time_stamp as ch.
time_stamp = string(time, "HH:MMam").

def var floc  like loc.loc.
def var tloc  like floc.
def var fcat  like procat.procat.
def var tcat  like fcat.
def var fitem like item.i-no.
def var titem like fitem.
def var inc   as logical format "Y/N".
def var cocode as char no-undo.

/*
{sys/form/s-top.f}
{sys/form/r-top3w.f}

str-tit  = coname + " - " + loname.
str-tit2 = "RAW MATERIALS - REORDER REPORT".
str-tit3 = "".
x = (112 - length(str-tit)) / 2.
str-tit  = fill(" ",x) + str-tit .
x = (112 - length(str-tit2)) / 2.
str-tit2 = fill(" ",x) + str-tit2 .
*/

assign floc = Begin_whse
       tloc = end_whse
       fcat = Begin_procat
       tcat = End_procat
       fitem = begin_i-no
       titem = End_i-no
       inc = t-inc-bal
       .
/*      
form skip(2)
     "        For Whse: " floc  space(5) "     To Whse: " tloc  skip(1)
     "        For Item: " fitem space(3)    "  To Item: " titem skip(1)
     "    For Category: " fcat  space(5) " To Category: " tcat  skip(1)
     "    Include Quantity on Order with Quantity on Hand? " inc   skip(1)
     skip(1)
     with title "           RAW MATERIAL REORDERING ADVICE REPORT           "
     frame selec row 8 centered overlay no-labels
	  color value(col-title) prompt value(col-input).

form
    item.i-no
    item.procat
    item.cons-uom
    item.loc
    item.ord-level format "->>>>9.99"
    item.q-onh     format "->>>>9.999"
    item.q-comm    format "->>>>9.999"
    item.q-ono     format "->>>>9.999"
    item.ord-min
    item.vend-no
    item.vend-item
    skip
header
"           PROD              REORDER   QUANTITY   QUANTITY   QUANTITY                                         VENDOR"
"ITEM #     CAT   UOM LOC       LEVEL    ON HAND  ALLOCATED   ON ORDER       MIN ORDER           VENDOR      ITEM NUMBER"
    with frame itemx no-box no-labels down width 132.
*/
/*===================================
outers:
repeat on error undo:

   pause 0.
   find first loc where loc.company = cocode no-lock no-error.
   floc = loc.loc.
   find last loc where loc.company = cocode no-lock no-error.
   tloc = loc.loc.
   find first item use-index i-no where item.company = cocode no-lock no-error.
   if avail item then fitem = item.i-no.
   find last  item use-index i-no where item.company = cocode no-lock no-error.
   if avail item then titem = item.i-no.
   for each procat by procat.procat :
      fcat = procat.procat. leave.
   end.
   for each procat by procat.procat descending:
      tcat = procat.procat. leave.
   end.

   update floc tloc fitem titem fcat tcat inc with frame selec.

   assign str-tit3 = " FROM WHSE: " + floc + " TO WHSE: " + tloc
	  x = (128 - length(str-tit3)) / 2
	  str-tit3 = fill(" ",x) + str-tit3.
   hide frame selec no-pause.
   {sys/inc/print2.i}
   if keyfunction(lastkey) = "end-error" then undo outers, leave outers.
   if choice then do:
      {sys/msg/print.i print}
      {sys/inc/outprint.i 58}
==================================*/
      
      view frame r-top.
      for each item use-index i-no
	  where item.company eq cocode
	    and item.i-no    ge fitem
	    and item.i-no    le titem
	    and item.loc     ge floc
	    and item.loc     le tloc
	    and item.procat  ge fcat
	    and item.procat  le tcat
	    and (item.q-onh + (if inc then item.q-ono else 0) - item.q-comm)
			     lt item.ord-level

	  break by item.company
		by item.i-no
		by item.loc with frame itemx:

	    if line-counter > 56 then page.

	    display
	       item.i-no    when first-of(item.i-no)
	       item.procat
	       item.cons-uom
	       item.loc
	       item.ord-level
	       item.q-onh
	       item.q-comm
	       item.q-ono
	       item.ord-min
	       item.vend-no
	       item.vend-item.
	    down.
      end.
      /*end.*/
/* ============not in gui      
{sys/inc/close.i}
   end.
   leave. /* fake loop */
end.      /* outers */
*/

hide frame selec no-pause.
hide frame print no-pause.

/* end ---------------------------------- copr. 1993  advanced software, inc. */

