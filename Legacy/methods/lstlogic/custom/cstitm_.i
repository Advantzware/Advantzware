/* cstitm_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'cstitm_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

/* ---------------------------------------------- fg/rep/fgreord1.p 01/01 JLF */
/* Reorder Advice Report by Ship-To                                           */
/* -------------------------------------------------------------------------- */

/*{sys/inc/var.i shared}
{sys/form/s-top.f}
*/
def var save_id as recid.
def var cocode as cha no-undo.
def var time_stamp as ch.
time_stamp = string(time, "hh:mmam").

def var v-cust      like itemfg.cust-no extent 2 init ["","zzzzzzzz"].
def var v-ship      like oe-rel.ship-id extent 2 init ["","zzzzzzzz"].
def var v-cat       like itemfg.procat extent 2 init ["","zzzzzz"].
def var v-item      like itemfg.i-no extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-inconh    as   log format "Y/N" init "Y".
def var v-totrel    as   log format "Tot All/Release" init "Y".
def var v-date      as   date format "99/99/9999" init today.
def var v-pur-man   as   char format "!" init "A".
def var v-lot-reo   as   char format "!" init "R".
def var v-prt-cpn   as   log format "Y/N" init no.

def var v-reord-qty as   int.
def var v-qty-avail as   int.
def var v-alloc-qty as   int.
def var v-rec-date  as   date.
def var v-coverage as int no-undo.
def var v-tot-cust-qty as int no-undo.
def var v-tot-annual-cons as int no-undo.
{sys/inc/sa-sls01.i}

/*
help-id = program-name(1).
{sa/sa-sls01.i}

{sys/form/r-top3w.f}

assign
 str-tit  = coname + " - " + loname
 str-tit2 = "FINISHED GOODS - REORDER REPORT"
 str-tit3 = ""
 x = (112 - length(str-tit)) / 2
 str-tit  = fill(" ",x) + str-tit
 x = (116 - length(str-tit2)) / 2
 str-tit2 = fill(" ",x) + str-tit2
 x = (132 - length(str-tit3)) / 2
 str-tit3 = fill(" ",x) + str-tit3.

form skip(1)
     "   From Customer: " v-cust[1]  " To Customer:" at 35 v-cust[2] space(2)
     skip(1)
     "    From Ship-To: " v-ship[1]  "  To Ship-To:" at 35 v-ship[2] space(2)
     skip(1)
     "       From Item: " v-item[1]  "     To Item:" at 35 v-item[2] space(2)
     skip(1)
     "   From Category: " v-cat[1]   " To Category:" at 35 v-cat[2]
     skip(1)
     "   Include Quantity on Order with Quantity on Hand:" v-inconh  skip
     "           Use Total Allocated or Release Quantity:" v-totrel  skip
     "                                             As of:" v-date    skip
     "                  Print Purchased/Manufactured/All:" v-pur-man skip
     "            Print Lot Controller Level/Reorder/All:" v-lot-reo skip(1)

    with title " FG REORDER ADVICE REPORT BY SHIP-TO "
         frame selec row 4 centered overlay no-labels
         color value(col-title) prompt value(col-input).


outers:
repeat on error undo:
  update v-cust v-ship v-item v-cat v-inconh v-totrel v-date
         v-pur-man validate(index("PMA",input v-pur-man) gt 0,
                          "ERROR: Must enter 'P', 'M', or 'A'")
         v-lot-reo validate(index("LRA",input v-lot-reo) gt 0,
                          "ERROR: Must enter 'L', 'R', or 'A'")
                          
      with frame selec editing:

    if frame-field eq "v-date"          and
       keyfunction(lastkey) eq "return" and
       input v-totrel                   then apply keycode("return").

    readkey.
    apply lastkey.
  end.

  {sys/inc/print2.i}
  if keyfunction(lastkey) eq "end-error" then next.

  if choice then do:
    {sys/msg/print.i print}
    {sys/inc/outprint.i 55}

    view frame r-top.
    put skip.
*/
   
   assign cocode = gcompany
          v-cust[1] = begin_cust-no
          v-cust[2] = end_cust-no
          v-ship[1] = begin_shipto
          v-ship[2] = end_shipto
          v-item[1] = begin_i-no
          v-item[2] = end_i-no
          v-cat[1] = begin_cat
          v-cat[2] = end_cat
         /* v-lot-reo = rd-print-lot
          v-pur-man = rd-print
          v-inconh = t-include-order
          v-totrel = rd-tot-rel */
          v-date = ldt-as-of
          .
           
    for each itemfg
        where itemfg.company    eq cocode
          and itemfg.i-no       ge v-item[1]
          and itemfg.i-no       le v-item[2]
          and itemfg.procat     ge v-cat[1]
          and itemfg.procat     le v-cat[2]
          and ((itemfg.ord-policy     and v-lot-reo eq "R") or
               (not itemfg.ord-policy and v-lot-reo eq "L") or v-lot-reo eq "A")
          and ((itemfg.pur-man        and v-pur-man eq "P") or
               (not itemfg.pur-man    and v-pur-man eq "M") or v-pur-man eq "A")
        use-index i-no no-lock:
   
      assign
       v-qty-avail = itemfg.q-onh + (if v-inconh then itemfg.q-ono else 0)
       v-alloc-qty = 0.

      if v-totrel then v-alloc-qty = itemfg.q-alloc.

      else
      for each oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.i-no    eq itemfg.i-no
          use-index item no-lock,

          each oe-rel
          where oe-rel.company  eq cocode
            and oe-rel.ord-no   eq oe-ordl.ord-no
            and oe-rel.i-no     eq oe-ordl.i-no
            and oe-rel.link-no  eq 0
            and oe-rel.rel-date le v-date
          use-index ord-item no-lock:

        v-alloc-qty = v-alloc-qty + oe-rel.qty.
      end.
      
      v-qty-avail = v-qty-avail - v-alloc-qty.

      if itemfg.ord-level gt v-qty-avail then
      for each oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.i-no    eq itemfg.i-no
          no-lock,
            
          each oe-rel
          where oe-rel.company eq cocode
            and oe-rel.ord-no  eq oe-ordl.ord-no
            and oe-rel.i-no    eq oe-ordl.i-no
            and oe-rel.line    eq oe-ordl.line
          no-lock
          
          by oe-rel.rel-date desc
          by oe-rel.r-no     desc:
        
        if oe-rel.cust-no ge v-cust[1] and
           oe-rel.cust-no le v-cust[2] and
           oe-rel.ship-id ge v-ship[1] and
           oe-rel.ship-id le v-ship[2] then do:
        
           /* added logic for customer's inventory */
          assign v-tot-cust-qty = 0
                 v-tot-annual-cons = 0
                 .
          for each cust-itm where cust-itm.company = cocode
                            and cust-itm.cust-no = oe-rel.cust-no
                         and cust-itm.i-no = oe-rel.i-no
                         no-lock .
             v-tot-cust-qty = v-tot-cust-qty + cust-itm.qty.
             v-tot-annual-cons = v-tot-annual-cons + cust-itm.consum.
          end.
          /* ========================== */          
          create report.
          assign report.term-id = v-term
                 report.key-01  = oe-rel.cust-no
                 report.key-02  = oe-rel.ship-id
                 report.key-03  = itemfg.i-no
                 report.key-04  = string(v-alloc-qty,"-999999999999")
                 report.key-05  = string(v-qty-avail,"-999999999999")
                 report.key-06  = if t-use-cust-inv then string(v-tot-cust-qty,"-999999999999") else "0"
                 report.key-07  = string(v-tot-annual-cons,"-999999999999")
                 report.rec-id  = recid(oe-rel).
           
          leave. 
        end.   
      end.
    end. /* each itemfg */
    
    for each report where report.term-id eq v-term,
        first oe-rel where recid(oe-rel) eq report.rec-id no-lock,
        first oe-ordl where oe-ordl.company eq cocode
                        and oe-ordl.ord-no  eq oe-rel.ord-no
                        and oe-ordl.i-no    eq oe-rel.i-no
                        and oe-ordl.line    eq oe-rel.line
                       no-lock,
        first itemfg        where itemfg.company eq cocode
                   and itemfg.i-no    eq oe-rel.i-no      no-lock  
        break by report.key-01
              by report.key-02
              by report.key-03
        transaction:
        
        assign  v-reord-qty = itemfg.ord-level - int(report.key-05) 
                              - int(report.key-06)
                v-rec-date  = ?.
      
      for each fg-bin
          where fg-bin.company eq cocode
            and fg-bin.i-no    eq itemfg.i-no
            and fg-bin.qty     gt 0
          no-lock,
          
          each fg-rcpth
          where fg-rcpth.company   eq cocode
            and fg-rcpth.i-no      eq fg-bin.i-no
            and fg-rcpth.job-no    eq fg-bin.job-no
            and fg-rcpth.job-no2   eq fg-bin.job-no2
            and fg-rcpth.rita-code eq "R"
          no-lock,
          
          each fg-rdtlh
          where fg-rdtlh.r-no      eq fg-rcpth.r-no
            and fg-rdtlh.rita-code eq fg-rcpth.rita-code
            and fg-rdtlh.loc       eq fg-bin.loc
            and fg-rdtlh.loc-bin   eq fg-bin.loc-bin
            and fg-rdtlh.tag       eq fg-bin.tag
          no-lock
          
          by fg-rcpth.trans-date
          BY fg-rdtlh.trans-time
          by fg-rcpth.r-no:
          
        v-rec-date = fg-rcpth.trans-date.  
          
        leave.  
      end.  /* for each fg-bin */
      v-coverage = ( int(report.key-05) + int(report.key-06) ) / 
                   ( int(report.key-07) / 52 ).
                   
      display oe-rel.cust-no            column-label "Customer"
              oe-rel.ship-id            column-label "Ship!To"
              oe-rel.i-no               column-label "Item"
              oe-rel.po-no              column-label "Customer PO"
              oe-rel.ord-no             column-label "Order"
              oe-ordl.t-price / oe-ordl.qty
                                        column-label "Unit Pr"
                                        format ">>>9.99<<"
              int(report.key-06)        column-label "Customer!Inventory"
                                        format "->>>>>>9"
              itemfg.q-onh              column-label "On!Hand"
                                        format "->>>>>>9"
              itemfg.q-ono when v-inconh
                                        column-label "On!Order"
                                        format "->>>>>9"
              int(report.key-04)        column-label "Allo!cated"
                                        format "->>>>>>9"
              int(report.key-05) + int(report.key-06) column-label "Avail!able"
                                        format "->>>>>>9"
              v-coverage                label "Coverage"
                                        form "->>>>>>>9"
              v-reord-qty               column-label "Reord!Qty"
                                        format ">>>>>9"
              itemfg.ord-level          column-label "Reord!Lev"
                                        format ">>>>>9"
              itemfg.ord-min            column-label "Min!Order"
                                        format ">>>>>9"
              v-rec-date                column-label "1st!Recpt"
                                        format "99/99/99"
              "N/A" when v-rec-date eq ? @ v-rec-date
              itemfg.ord-policy         column-label "R!L"
              
          with down no-box width 180 stream-io.
        
      delete report.  
    end.
/*
    {sys/inc/close.i}
  end. /* choice */

  leave. /* fake loop */
end.      /* outers */

hide all no-pause.
*/
/* end ---------------------------------- copr. 2001  advanced software, inc. */
