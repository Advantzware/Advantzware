/* rm-ibtag_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'rm-ibtag_'.

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

/* ------------------------------------------------ rm/rep/rm-ibtag.p 9/93 cd */
/* raw materials - inventory by bin/tag report                                */
/* -------------------------------------------------------------------------- */

def var time_stamp as ch.
time_stamp = string(time, "HH:MMam").

def var save_id   as recid.
def var v-price     as dec  format "->>>>9.99".
def var v-tot-price as dec  format "$->>,>>>,>>9.99".
def var v-cum-qty   as dec  format "->>>>>9.999".
def var v-cum-price as dec  format "->>>,>>9.99".
def var v-cum-qty2   as dec  format "->>>>>9.999".  /* item totals */
def var v-cum-price2 as dec  format "->>>,>>9.99".  /* item totals */
def var fitm like rm-bin.i-no format "X(10)" init "".
def var titm like fitm init "zzzzzzzzzz".
def var floc like rm-bin.loc init "".
def var tloc like floc init "zzzzz".
def var fcat as ch init "".
def var tcat like fcat init "zzzzzz".
def var type as log format "R/E" init yes.
def var ftyp like item.mat-type init "".
def var ttyp like ftyp init "z".
def var zbal as log format "Y/N" init no.
def var v-fst-loc as logical.
def var v-fst-ino as logical.
def var v-lst-ino as logical.
def var v-prnt-line as int.
def var v-cost like rm-bin.cost.
def var psubtot as log init yes.
def var pgtot as log init no.

/*
assign
 str-tit  = coname + " - " + loname
 str-tit2 = "RAW MATERIALS - INVENTORY ON HAND BY BIN/TAG"
 str-tit3 = ""
 x = (112 - length(str-tit)) / 2
 str-tit  = fill(" ",x) + str-tit
 x = (112 - length(str-tit2)) / 2
 str-tit2 = fill(" ",x) + str-tit2
 x = (112 - length(str-tit3)) / 2
 str-tit3 = fill(" ",x) + str-tit3 .

form skip(1)
     "     From Item #:" fitm       "To Item:" to 43 titm  skip(1)
     "  From Warehouse:" floc  "To Warehouse:" to 43 tloc  skip(1)
     "   From Category:" fcat   "To Category:" to 43 tcat  skip(1)
     "  From Mat. Type:" ftyp  "To Mat. Type:" to 43 ttyp  skip(1)
     "  Include Zero Balances:" zbal                       skip(1)
     "      Print Sub Totals?:" psubtot                    skip(1)
     "    Print Grand Totals?:" pgtot                      skip(1)
     
     with title "       RAW MATERIAL INVENTORY ON HAND BY BIN / TAG       "
     frame selec row 4 centered overlay no-labels
          color value(col-title) prompt value(col-input).
*/
assign fitm = begin_i-no
       titm = end_i-no
       floc = begin_whse
       tloc = end_whse
       fcat = begin_procat
       tcat = end_procat
       ftyp = begin_mtype
       ttyp = end_mtype
       zbal = t-inc-bal
       psubtot = t-subtot
       pgtot = t-gtot
       .

message fitm titm floc tloc fcat tcat skip
        ftyp ttyp zbal psubtot pgtot
        view-as alert-box .

if true then return.
        
find first ce-ctrl where ce-ctrl.company = gcompany no-lock.

/* ========= not in GUI
outers:
repeat on error undo:
  pause 0.

  update fitm titm floc tloc fcat tcat zbal ftyp ttyp psubtot pgtot
      with frame selec.

  if keyfunction(lastkey) eq "end-error" then undo outers, leave outers.

  {sys/inc/print2.i}

  if keyfunction(lastkey) eq "end-error" then next.

  {sys/msg/print.i print}
  {sys/inc/outprint.i 56}
=========== */

  view frame r-top.

  for each  rm-bin
      where rm-bin.company         eq gcompany
        and rm-bin.i-no            ne ""
        and rm-bin.i-no            ge fitm
        and rm-bin.i-no            le titm
        and rm-bin.loc             ge floc
        and rm-bin.loc             le tloc
        and (zbal or
             rm-bin.qty            ne 0)
      no-lock,

      each  item of rm-bin
      where item.procat            ge fcat
        and item.procat            le tcat
        and item.mat-type          ge ftyp
        and item.mat-type          le ttyp
        and item.i-code            eq "R"
      no-lock

      break by rm-bin.loc
            by rm-bin.i-no
            by rm-bin.loc-bin
            by rm-bin.tag

      with frame itemx:

    if first-of(rm-bin.loc) or
       line-counter gt page-size - 10 then do:
      page.
      v-prnt-line = 0.
    end.

    else v-prnt-line = 1.

    v-cost = if ce-ctrl.r-cost then item.avg-cost else rm-bin.cost.

    display rm-bin.loc             when first-of(rm-bin.loc)
                                     or v-prnt-line eq 0
                                   label "Whse"
            rm-bin.i-no            when first-of(rm-bin.i-no)
                                     or v-prnt-line eq 0
                                   label "Item"
            item.i-name            when first-of(rm-bin.i-no)
                                     or v-prnt-line eq 0
                                   format "x(30)"
                                   label "Description"
            rm-bin.loc-bin         label "Bin"
            rm-bin.tag             label "Tag"
            rm-bin.qty             format "->>>>>9.999"
                                   label "Quantity"
            v-cost                 format ">>>,>>9.99<<<<"
                                   label "Unit Cost"
            rm-bin.qty * v-cost    format "->>>,>>9.99"
                                   column-label "Total!Cost Value"     skip

         with frame itemx no-box no-attr-space down width 132.

    assign
     v-cum-qty   = v-cum-qty   + rm-bin.qty
     v-cum-price = v-cum-price + (rm-bin.qty * v-cost).

    if last-of(rm-bin.loc-bin) then do:
      if not first-of(rm-bin.loc-bin) and psubtot then
        put "-----------"          to 77
            "-----------"          to 89 skip
            "Bin Sub-total"        at 52
            v-cum-qty              to 77
            v-cum-price            to 89.
      
      if not last-of(rm-bin.i-no) then put skip(1).

      assign
       v-cum-qty2   = v-cum-qty2   + v-cum-qty
       v-cum-price2 = v-cum-price2 + v-cum-price
       v-cum-qty    = 0
       v-cum-price  = 0.
    end.

    if last-of(rm-bin.i-no) then do:
      if not first-of(rm-bin.i-no) and psubtot then
        put "-----------"          to 77
            "-----------"          to 89 skip
            "Item Total"           at 52
            v-cum-qty2             to 77
            v-cum-price2           to 89.

      put skip(1).
      
      assign
       v-tot-price  = v-tot-price + v-cum-price2
       v-cum-qty2   = 0
       v-cum-price2 = 0.
    end.

    if last-of(rm-bin.loc) then do:
      if pgtot then
        put skip(1)
            "--------------"            to 89
            "Total Inventory Value "    to 73
            v-tot-price                 to 89
            "--------------"            to 89.

      v-tot-price = 0.
    end.
  end.

/*  not in GUI
*  {sys/inc/close.i}
*
*  leave. /* fake loop */
*end.      /* outers */
*
*hide all no-pause.
*/

/* end ---------------------------------- copr. 1993  advanced software, inc. */

