/* ---------------------------------------------- oe/rep/bolinlnd.p 06/01 JLF */
/* PRINT Inland BOL                                                           */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.
def buffer xxreport     for report.

{oe/rep/oe-lad.i}

def var v-salesman          as   char format "x(26)".
def var v-fob               as   char format "x(12)".
def var v-tot-cases         as   int format ">>>>9".
def var v-tot-pallets       as   int format ">>>>9".
def var v-tot-wt            as   dec format ">>,>>>,>>9".

def var v-tot-pkgs          as   int format ">>9".
def var v-ord-qty           like oe-ordl.qty.
def var v-bol-qty           like oe-boll.qty.
def var v-ship-qty          like oe-ordl.ship-qty.
def var v-bol-wt            as   dec.
def var v-part-dscr         as   char format "x(30)".
def var v-part-qty          as   dec.
def var v-part-comp         as   char format "x".
def var v-ord-no            like oe-boll.ord-no.
def var v-po-no             like oe-bolh.po-no.
def var v-job-no            as   char format "x(9)" no-undo.
def var v-phone-num         as   char format "x(13)" no-undo.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
def var v-comp-name  like company.name.
def var v-comp-addr  like company.addr.
def var v-comp-city  like company.city.
def var v-comp-state like company.state.
def var v-comp-zip   like company.zip.
def var v-comp-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".
def var v-1          as   char format "x(2)" initial "1@" no-undo.

def var v-terms like oe-ord.terms-d no-undo.
def var v-frt-terms as char format "x(10)" no-undo.
def var v-zone like carr-mtx.del-zone no-undo.

def workfile w2 no-undo
    field cases            as   int format ">9"
    field cas-cnt          as   int format ">>>>9".

def workfile w3 no-undo
    field ship-i           as   char format "x(60)".

assign tmpstore = fill("-",80).

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

form header
     skip(2)
     v-comp-name                  at 1
     "Bill of Lading #:"          to 70
        trim(string(oe-bolh.bol-no,">>>>>>>9"))
                                  to 80
     v-comp-addr[1]               at 1
     "Page #:"                    to 70
        string(trim(string(page-number - v-last-page,">9")) + " of " +
               trim(string(v-page-tot,">9")))
                                  to 80 format "x(8)"
     v-comp-addr[2]               at 1
     "Date:"                      to 70
     today                        to 80
     v-comp-addr3                 at 1
     skip(2)
     "Ship To:"                   at 10 skip
     v-ship-name                  at 10
     "Carrier:"                   to 60
     carrier.dscr                 at 65 format "x(15)" when avail carrier
     v-ship-addr[1]               at 10
     "Trailer #:"                 to 60
     oe-bolh.trailer              at 65 format "x(15)"
     v-ship-addr[2]               at 10
     "Freight Terms:"             to 60 
     v-frt-terms                  at 65 skip
     v-ship-addr3                 at 10
     "Zone:"                      to 60
     v-zone                       at 65 skip
     v-phone-num                  at 10 format "(999)999-9999"
     "FOB:"                       to 60
     v-fob                        at 65
     skip(1)
     "Shipping Instructions:"     to 23
     "Payment Terms:"             to 60
     v-terms                      at 65 format "x(15)"
     oe-bolh.ship-i               at 1
     "Part #          PO#"        at 1
     "File #          JOB #           Description                    Unit-Qty  ~P/C Wgt"                                  at 1 skip
     tmpstore                     at 1 format "x(80)"
   with frame bol-top page-top no-box no-attr-space no-underline stream-io width 80.

form oe-ordl.part-no                at 1
     oe-boll.po-no                  at 17
     itemfg.i-name                  at 33
     oe-boll.cases                  at 64 format ">>9"
     "@"                            at 67
     oe-boll.qty-case               at 68 format ">>>>9"
     oe-boll.i-no                   at 1
     v-job-no                       at 17
     itemfg.part-dscr1              at 33
     v-1                            at 66
     oe-boll.partial                at 68 format ">>>>9" skip
     "---------"                    at 64 skip
     v-tot-pkgs                     at 64 format ">>9"
     "="                            at 67
     oe-boll.qty                    at 68 format ">>>>9"
     v-part-comp                    at 75
     oe-boll.weight                 to 80 format ">>>>9"
    with frame bol-mid1 down no-box no-labels stream-io width 80.

form header
     " "
     skip(4)
    with frame bol-bot1 page-bottom no-box no-underline stream-io width 80.

form header
     skip(2)
     "Total Pallets:"               to 60
     v-tot-pallets                  to 80
     "Total Weight:"                to 60
     v-tot-wt                       to 80
     skip(1)
     tmpstore                       at 1 format "x(80)" skip(1)
     "Customer_____________________                      Carrier_______________~______"
     skip(1)
     "    Date____________                                  Date____________"
    with frame bol-bot2 page-bottom no-box no-underline stream-io width 80.

def stream last-page.

{sa/sa-sls01.i}

output stream last-page to value(tmp-dir + "bolinlnd.txt") page-size VALUE(v-lines-per-page).

find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

view frame bol-top.
view stream last-page frame bol-top.

if avail company then
  assign v-comp-name    = company.name
         v-comp-addr[1] = company.addr[1]
         v-comp-addr[2] = company.addr[2]
         v-comp-addr3   = company.city + ", " +
                          company.state + "  " +
                          company.zip.

if v-comp-addr[2] eq "" then
  assign v-comp-addr[2] = v-comp-addr3
         v-comp-addr3   = "".

for each xxreport where xxreport.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh)   eq xxreport.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq oe-bolh.cust-no
    no-lock

    break by oe-bolh.bol-no:

  if first-of(oe-bolh.bol-no) then do:
    find first carrier
        where carrier.company eq oe-bolh.company
          and carrier.carrier eq oe-bolh.carrier
        no-lock no-error.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).
    assign
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-addr3   = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip
     v-cust-addr3   = cust.city + ", " +
                      cust.state + "  " +
                      cust.zip
     v-phone-num    = cust.area-code + cust.phone.

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

    assign
     v-salesman  = ""
     v-fob       = ""
     v-terms     = ""
     v-frt-terms = "".

    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
        first oe-ord
	    where oe-ord.company eq oe-boll.company
	      and oe-ord.ord-no  eq oe-boll.ord-no
	    NO-LOCK:

      if not available carrier then
      find first carrier where carrier.company = oe-ord.company
        and carrier.carrier = oe-ord.carrier no-lock no-error.

      do i = 1 to 3:
        if oe-ord.sman[i] ne "" then
          v-salesman = trim(v-salesman) + " " + oe-ord.sman[i] + ",".
      end.

/*    if can-do("COD,CIA", oe-ord.terms) then v-terms = oe-ord.terms-d. */
      assign v-terms = oe-ord.terms-d
             v-frt-terms = if cust.frt-pay eq "P" then "Prepaid"
                           else if cust.frt-pay eq "B" then "Bill"
                           else if cust.frt-pay eq "C" then "Collect"
                           else if cust.frt-pay eq "T" then "Third Party"
                           else ""
             v-zone = cust.del-zone.
             
      if v-terms eq "" then
      do:
        find first terms where terms.t-code eq oe-ord.terms no-lock no-error.
        if avail terms then
          assign v-terms = terms.dscr.
      end.
      
      v-salesman = trim(v-salesman).

      if v-salesman gt '' then
        if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
          substr(v-salesman,length(trim(v-salesman)),1) = "".

      v-fob = if oe-ord.fob-code begins "ORIG" then "Origin" else "Destination".

      LEAVE.
    end.

    for each w3:
      delete w3.
    end.
    
    page.
    page stream last-page.
    
    v-tot-pallets = 0.
  end. /* first-of(oe-bolh.bol-no) */

  do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then do:
      find first w3 where w3.ship-i eq oe-bolh.ship-i[i] no-error.
      if not avail w3 then create w3.
      w3.ship-i = oe-bolh.ship-i[i].
    end.
  end.

  for each oe-boll
      where oe-boll.company eq oe-bolh.company
        and oe-boll.b-no    eq oe-bolh.b-no:
    create report.
    assign
     report.term-id  = v-term-id
     report.key-01   = oe-boll.i-no
     report.key-02   = string(oe-boll.ord-no,"999999")
     report.rec-id   = recid(oe-boll)
     oe-boll.printed = yes.
  end.
  
  if last-of(oe-bolh.bol-no) then do:
    {oe/rep/bolinlnd.i "stream last-page"}

    v-page-tot = page-number (last-page) - v-last-page.

    {oe/rep/bolinlnd.i}

    v-last-page = page-number.

    hide frame bol-bot1.
    view frame bol-bot2.

    for each report where report.term-id eq v-term-id,
        first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
      delete report.
    end.
  end.
  
  v-tot-pallets = v-tot-pallets + oe-bolh.tot-pallets.

  oe-bolh.printed = yes.
end. /* for each oe-bolh */

output stream last-page close.

hide frame bol-top no-pause.
page.

/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */
