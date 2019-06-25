/* ---------------------------------------------- oe/rep/bolchill.p 10/01 JLF */
/* Print Chillicothe BOL                                                      */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/FORM/r-top.i}

{oe/rep/oe-lad.i}

def var v-carrier    like carrier.dscr.
def var v-delzone    like carr-mtx.del-dscr.
def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".

def var v-po         as   char.
def var v-ord        as   char.
def var v-partial    like oe-boll.cases.

{sa/sa-sls01.i}

find first oe-bolh no-lock no-error.

format header
       trim(string(oe-bolh.bol-no,">>>>>>>>>>9"))   at 70 format "x(10)"
       skip(14)
       v-ship-name                                  at 10
       v-ship-addr[1]                               at 42
       v-ship-addr[2]                               at 42
       v-ship-city                                  at 6
       v-ship-addr3                                 at 58 format "x(14)"
       oe-bolh.trailer                              at 12
       skip(6)

    with frame bol-top page-top no-box no-underline stream-io width 85.

form oe-boll.cases                  to 5  format ">>>>>"
     oe-boll.qty-case               to 12 format ">>>>>>"
     oe-boll.qty                    to 21 format ">>>>>>>>"
     oe-boll.i-no                   at 24
     v-po                           to 63 format "x(20)"
     oe-boll.weight                 to 72 format ">,>>>,>>>"
     skip
     v-partial                      to 5  format ">>>>>"
     oe-boll.partial                to 12 format ">>>>>>"
     itemfg.i-name                  at 24 format "x(28)"
     v-ord                          to 63 format "x(11)"

    with frame bol-mid down no-box no-labels stream-io width 85.

form header
     " "
     skip
     skip(8)
     
    with frame bol-bot page-bottom no-box no-underline stream-io width 85.


find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

for each report   where report.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh) eq report.rec-id,
    each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    
  create xreport.
  assign
   xreport.term-id = v-term-id
   xreport.rec-id  = recid(oe-boll)
   xreport.key-01  = string(oe-bolh.bol-no,"9999999999")
   xreport.key-02  = oe-boll.po-no
   xreport.key-03  = oe-boll.i-no
   xreport.key-04  = string(oe-boll.ord-no,"9999999999")
   oe-bolh.printed = yes
   oe-boll.printed = yes.
end.
    
for each report   where report.term-id eq v-term-id,
    first oe-boll where recid(oe-boll) eq report.rec-id no-lock,

    first oe-bolh
    where oe-bolh.b-no eq oe-boll.b-no
    no-lock

    break by report.key-01
          by report.key-02
          by report.key-03
          by report.key-04:

  if first-of(report.key-01) then do:
    find first carrier
        where carrier.company eq cocode
          and carrier.carrier eq oe-bolh.carrier
        no-lock no-error.
    v-carrier = if avail carrier and carrier.dscr ne "" then carrier.dscr
                                                        else oe-bolh.carrier.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    assign
     v-po           = report.key-02
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-city    = shipto.ship-city
     v-ship-addr3   = shipto.ship-state + "  " +
                      shipto.ship-zip
     v-delzone      = shipto.dest-code.
                      
    release carr-mtx.
    
    if avail carrier then
    find first carr-mtx
        where carr-mtx.company  eq cocode
          and carr-mtx.loc      eq locode
          and carr-mtx.carrier  eq carrier.carrier
          and carr-mtx.del-zone eq shipto.dest-code
        no-lock no-error.
    if avail carr-mtx then v-delzone = carr-mtx.del-dscr.

    view frame bol-top.

    page.
    
    if first(report.key-01) then view frame bol-bot.
  end.
  
  find first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq oe-boll.i-no
      no-lock no-error.
      
  assign
   v-po  = "PO# "  + trim(oe-boll.po-no)
   v-ord = "ORD# " + trim(string(oe-boll.ord-no,">>>>>>")).
   
  if v-po  eq "PO# "  then v-po  = "".
  if v-ord eq "ORD# " then v-ord = "".
  
  assign 
   v-po  = fill(" ",20 - length(trim(v-po)))  + trim(v-po)
   v-ord = fill(" ",11 - length(trim(v-ord))) + trim(v-ord).

  display oe-boll.cases
          1 when oe-boll.cases eq 0 and oe-boll.partial ne 0 @ oe-boll.cases
          
          oe-boll.qty-case
          oe-boll.partial when oe-boll.cases eq 0 and oe-boll.partial ne 0
                                                             @ oe-boll.qty-case
                                                             
          oe-boll.qty
          oe-boll.i-no
          v-po
          oe-boll.weight
          
          1 when oe-boll.partial ne 0 and oe-boll.cases ne 0 @ v-partial
          0 when oe-boll.cases eq 0 and oe-boll.partial ne 0 @ v-partial
          
          oe-boll.partial when oe-boll.partial ne 0 and oe-boll.cases ne 0
          0 when oe-boll.cases eq 0 and oe-boll.partial ne 0 @ oe-boll.partial
          
          itemfg.i-name when avail itemfg
          v-ord
          
      with frame bol-mid.
  down with frame bol-mid.    
end. /* for each oe-bolh */

hide frame bol-top no-pause.
page.
