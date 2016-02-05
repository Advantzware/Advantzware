/* ---------------------------------------------- oe/rep/bolmaxpk.p 10/00 JLF */
/* Print Max Pak BOL                                                          */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

{oe/rep/oe-lad.i}

def var v-po         as   char.
def var v-carrier    like carrier.dscr.
def var v-ord-qty    like oe-ordl.qty.
def var v-shp-qty    like oe-boll.qty.
def var v-part-dscr  as   char.
def var v-lines      as   int.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".

{sa/sa-sls01.i}

find first oe-bolh no-lock no-error.

format header
       skip(7)
       trim(string(oe-bolh.bol-no,">>>>>>>>>>9"))   at 61 format "x(10)"
       skip(1)
       v-po                                         at 61 format "x(15)"
       skip(2)
       v-ship-name                                  at 10
       v-carrier                                    at 47
       v-ship-addr[1]                               at 10
       v-ship-addr[2]                               at 10
       v-ship-addr3                                 at 10               skip
       oe-bolh.bol-date                             at 47 format "99/99/9999"
       skip(3)

    with frame bol-top page-top no-box no-underline stream-io width 85.

form v-ord-qty                      to 15 format ">>>>>>>9"
     v-shp-qty                      to 24 format ">>>>>>>9"
     oe-boll.i-no                   at 26
     itemfg.i-name                  at 42 format "x(28)"

    with frame bol-mid down no-box no-labels stream-io width 85.

form header
     " "
     skip(7)
     
    with frame bol-bot page-bottom no-box no-underline stream-io width 85.


view frame bol-top.

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

  if first-of(report.key-02) then do:
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
     v-ship-addr3   = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip.

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".

    page.
    
    if first(report.key-01) then view frame bol-bot.
  end.
  
  v-shp-qty = v-shp-qty + oe-boll.qty.
  
  if last-of(report.key-04) then
  for each oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.ord-no  eq oe-boll.ord-no
        and oe-ordl.i-no    eq oe-boll.i-no
      no-lock:
      
    v-ord-qty = v-ord-qty + oe-ordl.qty.  
  end.    

  if last-of(report.key-03) then do:
    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq oe-boll.i-no
        no-lock no-error.
        
    v-lines = 2.
    if itemfg.part-dscr1 ne "" then v-lines = v-lines + 1.
    if itemfg.part-dscr2 ne "" then v-lines = v-lines + 1.
  
    if line-counter - 1 + v-lines gt page-size + 1 then page.

    display v-ord-qty
            v-shp-qty
            oe-boll.i-no
            itemfg.i-name
        with frame bol-mid.
    down with frame bol-mid.    
  
    do i = 1 to 2:
      v-part-dscr = if i eq 1 then itemfg.part-dscr1
                              else itemfg.part-dscr2.
        
      if v-part-dscr ne "" then put v-part-dscr at 42 skip.
    end.
  
    put skip(1).
    
    assign
     v-ord-qty = 0
     v-shp-qty = 0.
  end.
end. /* for each oe-bolh */

hide frame bol-top no-pause.
page.

