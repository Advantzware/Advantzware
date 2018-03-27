/* ---------------------------------------------- oe/rep/bolbrck1.p 11/01 JLF */
/* Print Brick BOL when sys-ctrl.log-fld eq yes                               */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.

{oe/rep/oe-lad.i}

def var v-fob-code          as   char.
def var v-terms             like oe-ord.terms.
def var v-carrier           like oe-bolh.carrier.
def var v-ord-date          like oe-ord.ord-date.
def var v-part-qty          as   dec.
def var v-po-no             like oe-boll.po-no extent 2.
def var v-bol-qty           like oe-boll.qty.
def var v-bin-tag           as   char format "x(19)".
def var v-bin               as   char format "x(22)" extent 4.
def var v-inst              as   char extent 8.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".

def workfile w1
  field part-no like fg-set.part-no
  field rec-id  as   recid.

find first company where company.company eq cocode no-lock no-error.
find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.
find first report no-lock no-error.

form header
     skip(4)
     company.name                 at 16
     "BOL Number:"                to 109
     oe-bolh.bol-no               to 122 format ">>>>>>>>"
     company.addr[1]              at 16
     "Order Number:"              to 109
     int(report.key-03)           to 122 format ">>>>>>>>"
     trim(company.city) + ", " + trim(company.state) + "  " + trim(company.zip)
                                  at 16  format "x(30)"
     "Order Date:"                to 109
     v-ord-date                   to 122 format "99/99/99"
     "Phone: (714) 994-4140  Fax: (714) 994-1377"
                                  at 16
     "Ship Date:"                 to 109
     oe-bolh.bol-date             to 122 format "99/99/99"
     "FOB:"                       to 109
     v-fob-code                        to 122 format "x(12)"
     "Terms:"                     to 109
     v-terms                      to 122 format "x(12)"
     "Ship Via:"                  to 109
     v-carrier                    to 122 format "x(12)"
     skip(1)
     "Bill To:"                   at 16
     report.key-01                at 56  format "x(8)"
     "Ship To:"                   at 70
     report.key-02                at 110 format "x(8)"
     cust.name                    at 16
     v-ship-name                  at 70
     cust.addr[1]                 at 16
     v-ship-addr[1]               at 70
     cust.addr[2]                 at 16
     v-ship-addr[2]               at 70
     v-cust-addr3                 at 16
     v-ship-addr3                 at 70
     "P.O. Number:"               at 16
     v-po-no[1]
     "P.O. Number:"               at 70
     v-po-no[2]
     fill(chr(45),108)           at 15  format "x(108)"
     "Ln"                         at 16
     "Set"                        at 19
     "Part No"                    at 23
     "Part Description"           at 39
     "COC*"                       at 67
     "Location/Tag#"              at 77
     "Rel Qty"                    to 102
     "Pick"                       at 106
     "Ship"                       at 116
     fill(chr(45),108)           at 15  format "x(108)"

    with frame bol-top page-top no-box no-underline stream-io width 136.

form oe-boll.line                 at 16  format ">>"
     itemfg.isaset                at 20  format "Y/N"
     oe-boll.i-no                 at 23
     itemfg.i-name                at 39  format "x(25)"
     "________"                   at 65
     v-bin-tag                    at 74
     v-bol-qty                    to 102 format ">>,>>>,>>>"
     "________"                   at 104
     "________"                   at 114
     skip(1)
         
    with frame bol-mid no-box no-underline no-labels down stream-io width 136.

form header
     fill(chr(45),108)           at 15  format "x(108)"
     "Special Instructions/Delivery Notes:"
                                  at 16
     skip(1)
     v-inst[1 for 4]              at 29  format "x(80)"
     skip(1)
     v-inst[5 for 4]              at 29  format "x(80)"
     skip(1)
     "*COC - Certificate of Compliance"
                                  at 16
     fill(chr(45),108)           at 15  format "x(108)"
     "Picked By:"                 at 16
     "Shipped By:"                at 44
     "Shipped Date:"              at 71
     fill(chr(45),108)           at 15  format "x(108)"
     "Received By:"               at 16
     "Received Date:"             at 44
     fill(chr(45),108)           at 15  format "x(108)"
                                    
    with frame bol-bot page-bottom no-box no-underline no-labels down stream-io width 136.

view frame bol-top.
view frame bol-bot.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

{sa/sa-sls01.i}

for each report   where report.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh) eq report.rec-id no-lock:

  for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no no-lock:

    release oe-rel.

    find first oe-rell
        where oe-rell.company eq cocode
          and oe-rell.r-no    eq oe-boll.r-no
          AND oe-rell.ord-no  EQ oe-boll.ord-no
          and oe-rell.i-no    eq oe-boll.i-no
          and oe-rell.line    eq oe-boll.line
         no-lock no-error.
    if avail oe-rell then do:
      find first oe-relh of oe-rell no-lock.
      find first oe-rel
          where oe-rel.company eq cocode
            and oe-rel.ord-no  eq oe-rell.ord-no
            and oe-rel.line    eq oe-rell.line
            and oe-rel.link-no eq oe-rell.r-no
            and oe-rel.ship-no eq oe-relh.ship-no
            and oe-rel.i-no    eq oe-rell.i-no
          no-lock no-error.
      if not avail oe-rel then
      find first oe-rel
          where oe-rel.company  eq cocode
            and oe-rel.ord-no   eq oe-rell.ord-no
            and oe-rel.line     eq oe-rell.line
            and oe-rel.rel-date eq oe-relh.rel-date
            and oe-rel.ship-no  eq oe-relh.ship-no
            and oe-rel.i-no     eq oe-rell.i-no
          no-lock no-error.
    end.

    create xreport.
    assign
     xreport.term-id = v-term-id
     xreport.key-01  = report.key-01
     xreport.key-02  = report.key-02
     xreport.key-03  = STRING(oe-boll.ord-no,"99999999")
     xreport.key-04  = oe-boll.po-no
     xreport.key-05  = if avail oe-rel then oe-rel.po-no else ""
     xreport.key-06  = string(oe-boll.line,"9999999999")
     xreport.rec-id  = recid(oe-boll).
  end.

  delete report.
end.

for each report   where report.term-id eq v-term-id no-lock,
    first oe-boll where recid(oe-boll) eq report.rec-id no-lock,
    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq oe-boll.i-no
    no-lock,
    first oe-bolh where oe-bolh.b-no   eq oe-boll.b-no no-lock,
    first cust    where cust.cust-no   eq oe-bolh.cust-no no-lock

    break by report.key-01
          by report.key-02
          by report.key-03
          by report.key-04
          by report.key-05
          by report.key-06:

  assign
   v-po-no[1] = report.key-04
   v-po-no[2] = report.key-05.

  if first-of(report.key-05) then do:
    find first carrier
        where carrier.company eq cocode
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
                      cust.zip.

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".
    
    assign
     v-fob-code      = ""
     v-terms    = ""
     v-carrier  = oe-bolh.carrier
     v-ord-date = oe-bolh.bol-date.

    find first oe-ord
        where oe-ord.company eq cocode
          and oe-ord.ord-no  eq oe-boll.ord-no
        no-lock no-error.

    if avail oe-ord then do:
      if not avail carrier then
      find first carrier
          where carrier.company eq cocode
            and carrier.carrier eq oe-ord.carrier
          no-lock no-error.

      assign
       v-fob-code = if oe-ord.fob-code begins "ORIG" then "Origin"
                                                     else "Destination"
       v-terms    = oe-ord.terms              
       v-ord-date = oe-ord.ord-date.
    end.
    
    assign
     v-fob-code = fill(" ",12 - length(trim(v-fob-code))) + trim(v-fob-code)
     v-terms    = fill(" ",12 - length(trim(v-terms)))    + trim(v-terms)
     v-carrier  = fill(" ",12 - length(trim(v-carrier)))  + trim(v-carrier).
     
    page.
    
    v-inst = "".
    
    do i = 1 to 4:
      if shipto.notes[i] ne "" then v-inst[i] = shipto.notes[i].
    end.
    
    do i = 1 to 4:
      if oe-bolh.ship-i[i] ne "" then v-inst[i + 4] = oe-bolh.ship-i[i].
    end.
  end.
  
  v-bol-qty = v-bol-qty + oe-boll.qty.
  
  if last-of(report.key-06) then do:
    find first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-boll.ord-no
          and oe-ordl.i-no    eq oe-boll.i-no
          and oe-ordl.line    eq oe-boll.line
        no-lock no-error.
        
    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq oe-boll.i-no
        no-lock no-error.
    assign   
     v-bin-tag = if itemfg.isaset then "Set"
                 else
                 if avail fg-bin  then (fg-bin.loc-bin + "/" + fg-bin.tag)
                 else ""
     v-bin-tag = fill(" ",int((19 - length(trim(v-bin-tag))) / 2)) +
                 v-bin-tag.
    
    display oe-boll.line
            itemfg.isaset
            oe-boll.i-no
            itemfg.i-name
              oe-ordl.i-name when avail oe-ordl and oe-ordl.i-name ne ""
              @ itemfg.i-name
            v-bin-tag
            v-bol-qty
            
        with frame bol-mid.
    down with frame bol-mid.
    
    assign
     i     = 0
     j     = 0
     v-bin = "".

    if itemfg.isaset then
    for each fg-set
        where fg-set.company eq cocode
          and fg-set.set-no  eq itemfg.i-no
        no-lock,
        
        first xitemfg
        where xitemfg.company eq cocode
          and xitemfg.i-no    eq fg-set.part-no
        no-lock:

      {sys/inc/part-qty.i v-part-qty fg-set}

      find first fg-bin
          where fg-bin.company eq cocode
            and fg-bin.i-no    eq fg-set.part-no
          no-lock no-error.
      assign
       v-bin-tag = if avail fg-bin then (fg-bin.loc-bin + "/" + fg-bin.tag)
                   else ""
       v-bin-tag = fill(" ",int((19 - length(trim(v-bin-tag))) / 2)) +
                   v-bin-tag.

      display no                     @ itemfg.isaset
              fg-set.part-no         @ oe-boll.i-no
              itemfg.i-name
              v-bin-tag
              v-bol-qty * v-part-qty @ v-bol-qty
            
          with frame bol-mid.
      down with frame bol-mid.
    end.
    
    v-bol-qty = 0.
  end.
end. /* for each report */

hide frame bol-top no-pause.
page.
 
/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */
