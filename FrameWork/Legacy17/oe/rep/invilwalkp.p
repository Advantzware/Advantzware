/* ------------------------------------------------ oe/rep/invilwalkp.p       */
/* Ilwalker reprint posted O/E invoices                                       */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

{ar/rep/invoice.i}

def var v-case-line as CHAR NO-UNDO.
def var v-part-line as CHAR NO-UNDO.

def TEMP-TABLE w-sman NO-UNDO field sman as char format "x(4)".
DEF TEMP-TABLE wf NO-UNDO like ar-invl
    FIELD cases like oe-boll.cases 
    FIELD qty-case like oe-boll.qty-case
    FIELD partial like oe-boll.partial 
    FIELD v-case-line like v-case-line
    FIELD v-part-line like v-part-line.

def var save_id         AS recid no-undo.
def var v-print-align   AS log format "Y/N" no-undo.
def var v-align-ok      AS log format "Y/N" no-undo.
def var v-line-number   AS int no-undo.
def var v-subtot        like ar-inv.t-sales NO-UNDO.
def var v-invhead AS char format "x(13)" INIT "I N V O I C E".
def var big_ul AS char no-undo format 'x(80)'.
def var big_ul2 like big_ul no-undo.
def var v-disc-avail like ar-inv.disc-taken no-undo
    label "Disc Avail"
    column-label "Disc Avail".
def var v-disc-date like ar-inv.due-date no-undo
    label "Disc Date"
    column-label "Disc Date".
def var note-lines   AS int initial 0 no-undo.
def var note-cnt   AS int initial 0 no-undo.
def var adv AS int no-undo.
def var v-remit-to     as char format "x(90)".
def var end-page as INT INIT 54 NO-UNDO.
def var v-recs as int init 0.
def var v-addr-info as char.
def var v-cust like cust.addr extent 4.
def var v-ship like v-cust.
def var v-price like ar-invl.unit-pr no-undo.

def var v-c-name         like company.name.
def var v-c-addr         like company.addr.
def var v-c-city         like company.city.
def var v-c-state        like company.state.
def var v-c-zip          like company.zip.
def var v-c-phone        as   char format "x(30)". 
def var v-c-fax          as   char format "x(30)".
def var v-c-email        like cust.email.

DEF VAR v-date-ship AS DATE NO-UNDO.
def var v-salesman as char format "x(14)" NO-UNDO.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO.
def var v-del-no as int format ">>>>>>" NO-UNDO.
def var v-tot-pallets as INT NO-UNDO.
DEF VAR v-bol-no LIKE oe-bolh.bol-no NO-UNDO.
DEF VAR v-net AS DEC NO-UNDO.
def var tmp1 as DEC NO-UNDO.
def var tmp2 as DATE NO-UNDO.
def var v-ord-del-hdr  as char format "x(4)" init "Del#" NO-UNDO.
def var v-shipto AS CHAR NO-UNDO.
def var v-case-cnt as char format "x(80)" extent 5.
def var v-ord#         as int format ">>>>>>" no-undo.
def var v-part-info    as char format "x(30)".
def var v-printline as INT NO-UNDO.
def var v-beeler-lines as INT NO-UNDO.
def var v              as INT NO-UNDO.
def var v-tx-rate like stax.tax-rate NO-UNDO.
def var net1 as DEC NO-UNDO.
def var net2 as DEC NO-UNDO.
def var net3 as DEC NO-UNDO.
def var v-tax-code like stax.tax-code NO-UNDO.
def var disp-frt as char init "Freight:" NO-UNDO.

ASSIGN
  big_ul = FILL('=',80)
  big_ul2 = big_ul.

format skip (2)
      v-invhead AT 24
      ar-inv.inv-date        AT 72 FORMAT "99/99/99" skip
      v-c-name AT 11 skip
      v-c-addr[1] AT 11 skip
      v-c-addr[2] AT 11
      ar-inv.inv-no          AT 69 format "zzzzz9" skip
      v-c-city AT 11
      v-c-state v-c-zip skip
      v-c-phone at 11
      v-salesman AT 69
      v-c-fax at 11
      v-c-email at 11
      skip(1)
      v-fob AT 4
      ar-inv.terms-d         FORM "x(30)" AT 32
      skip (3)
      ar-inv.cust-no         AT 11
      v-shipto AT 51 SKIP
      v-cust[1]              AT 11
      v-ship[1]              AT 51
      v-cust[2]              AT 11
      v-ship[2]              AT 51
      v-cust[3]              AT 11
      v-ship[3]              AT 51
      v-cust[4]              AT 11
      v-ship[4]              AT 51
      skip (4)
      v-date-ship
      v-shipvia at 11 format "x(25)"
      ar-inv.t-weight to 44 format ">>>>9.99"
      v-del-no to 53
      v-bol-no to 65 format ">>>>>9"
      v-tot-pallets at 69 skip(2)
      WITH FRAME invhead no-BOX no-labels stream-io width 90.

format skip (2)
      "Date:" TO 56 ar-inv.inv-date FORMAT "99/99/99" skip
      v-c-name at 11 skip
      v-c-addr[1] at 11 skip
      v-c-addr[2] at 11
      "Inv #:" TO 56 ar-inv.inv-no skip
      v-c-city at 11
      v-c-state v-c-zip skip
      "Salesman:" TO 56 v-salesman skip(3)
      "FOB:" v-fob
      "TERMS:" ar-inv.terms-d FORM "x(30)"
      skip(2)
      "SOLD TO:" AT 11
      ar-inv.cust-no
      "SHIP TO:" AT 51
      ar-inv.ship-id
      v-cust[1]              AT 11
      v-ship[1]              AT 51 format "x(27)"
      v-cust[2]              AT 11
      v-ship[2]              AT 51 format "x(27)"
      v-cust[3]              AT 11
      v-ship[3]              AT 51 format "x(27)"
      v-cust[4]              AT 11
      v-ship[4]              AT 51 format "x(27)"
      skip(2)
      "--------------------------------------------------------------------------------" skip
      "Ship Date" "Shipped Via" AT 11 "Weight" TO 44 "Ord#" to 51
      "BOL #" TO 65 "Pallets" AT 72 skip
      v-date-ship
      v-shipvia at 11 format "x(25)"
      ar-inv.t-weight to 44 format ">>>>9.99"
      v-del-no to 53
      v-bol-no to 65 format ">>>>>9"
      v-tot-pallets at 69 skip
      "--------------------------------------------------------------------------------" skip(1)
      "Customer PO#" "Item" AT 24 "Qty Invoiced" TO 53 skip
      "  Buyer PO #" "Description" AT 21 "Qty Shipped" TO 53 "Price" TO 64
      "Amount" TO 80 skip
      "------------" "----------------" AT 19 "------------" TO 53 "-----" TO 64
      "------" TO 80 skip
      WITH FRAME ilwalker-comp no-BOX no-labels stream-io width 90.

form
  oe-ordl.po-no
  wf.i-no at 19
  wf.inv-qty to 52 format "->,>>>,>>9"
  wf.unit-pr to 64 format "->>,>>9.99" space(0)
  wf.pr-uom to 68
  wf.amt to 80 format "->>>,>>9.99" skip
  wf.po-no
  wf.i-name at 19 format "x(23)"
  wf.ship-qty to 52 format "->,>>>,>>9" skip
with frame ilw-detail no-labels no-box no-underline down stream-io width 90.

form v-ord# at 1 v-part-info at 19
    with frame beeler no-labels no-box no-underline down stream-io width 90.

form "Taxes- " at 1
   v-tax-code[1] at 9 format "x(4)"
   space(0) ":"
   net1 at 15 format "->>,>>9.99"
   v-tax-code[2] at 28 format "x(4)"
   space(0) ":"
   net2 at 34 format "->>,>>9.99"
    v-tax-code[3] at 46 format "x(4)"
    space(0) ":"
   net3 at 52 format "->>,>>9.99"
   "Tot Tax:" to 70
   space(0)
   ar-inv.tax-amt  to 80 format "->>,>>9.99"
   with frame tax no-labels no-box no-underline stream-io width 90.

form disp-frt at 1
    ar-inv.freight format "->,>>9.99" at 11
    ar-inv.tax-amt format "->,>>9.99" at 21
    v-subtot to 43 format "->,>>>,>>9.99"
    tmp1 to 56 format "->,>>9.99"
    tmp2 at 60
    space(0)
    ar-inv.due to 80 format "->,>>>,>>9.99"
    with frame totals no-labels no-box no-underline stream-io width 90.

form "Freight" to 20 "Tax" to 27
    "Subtotal" TO 43 "Cash Disc" TO 56 "If Paid By" AT 59
    "Invoice Amt" TO 80
    ar-inv.freight format "->,>>9.99" at 11
    ar-inv.tax-amt format "->,>>9.99" at 21
    v-subtot to 43 format "->,>>>,>>9.99"
    tmp1 to 56 format "->,>>9.99"
    tmp2 at 60
    space(0)
    ar-inv.due to 80 format "->,>>>,>>9.99"
    with frame totals-comp no-labels no-box no-underline stream-io width 90.

form "Freight" to 20
    "Subtotal" TO 43 "Cash Disc" TO 56 "If Paid By" AT 59
    "Invoice Amt" TO 80
    ar-inv.freight format "->,>>9.99" at 11
    ar-inv.tax-amt format "->,>>9.99" at 21
    v-subtot to 43 format "->,>>>,>>9.99"
    tmp1 to 56 format "->,>>9.99"
    tmp2 at 60
    space(0)
    ar-inv.due to 80 format "->,>>>,>>9.99"
    with frame totals-comp2 no-labels no-box no-underline stream-io width 90.

find first company where company.company = cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

find first cust
    where cust.company eq cocode
      and cust.active  eq "X"
    no-lock no-error.
if avail cust then do:
  if cust.area-code ne "" and cust.phone ne "" then
    v-c-phone = cust.area-code + '-' + 
                substr(cust.phone,1,3) + '-' +
                substr(cust.phone,4,4). 
  else
  if cust.area-code eq "" and cust.phone ne "" then
    v-c-phone = substr(cust.phone,1,3) +
                substr(cust.phone,4,4). 

  if cust.fax ne "" then
    v-c-fax = substr(cust.fax,1,3) + '-' +
              substr(cust.fax,4,3) + '-' +
              substr(cust.fax,7,4).
                 
  v-c-email = cust.email.
     
  if v-c-phone ne "" then v-c-phone = "Phone: " + v-c-phone.
  if v-c-fax   ne "" then v-c-fax   = "  Fax: " + v-c-fax.
  if v-c-email ne "" then v-c-email = "EMail: " + v-c-email.
     
  find first shipto of cust no-lock no-error.
   
  if avail shipto then
    v-remit-to = shipto.ship-addr[1] + " " + shipto.ship-addr[2] + " " +
                 shipto.ship-city + ", " + shipto.ship-state + " " +
                 shipto.ship-zip.
end.

if oe-ctrl.prcom then
   assign
     v-c-name    = company.name
     v-c-addr[1] = company.addr[1]
     v-c-addr[2] = company.addr[2]
     v-c-city    = company.city
     v-c-state   = company.state
     v-c-zip     = company.zip
     v-c-phone   = ""
     v-c-fax     = ""
     v-c-email   = "".

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id:

   v-recs = v-recs + 1.
   if ar-inv.fob-code begins "ORIG" THEN assign v-fob = "Origin".
   ELSE assign v-fob = "Destination".

   find FIRST carrier where carrier.company eq ar-inv.company and
          carrier.carrier eq ar-inv.carrier no-lock no-error.
         if avail carrier then
           assign v-shipvia = carrier.dscr.
         else
           assign v-shipvia = "".

  CLEAR FRAME invhead ALL no-PAUSE.
  assign
    v-disc-avail = 0
    v-disc-date = ?
    v-subtot = 0
    v-shipto = STRING(ar-inv.sold-no).

  find first cust where cust.company = cocode and
    cust.cust-no = ar-inv.cust-no no-lock no-error.

  find first shipto where shipto.company = cocode and
    shipto.cust-no = ar-inv.cust-no and
    shipto.ship-id = ar-inv.ship-id no-lock no-error.

  find first carrier where carrier.company = cocode and
    carrier.carrier = ar-inv.carrier no-lock no-error.

  find first sman where sman.company = cocode and
    sman.sman = cust.sman no-lock no-error.
    
  assign
   j      = 0
   v-cust = "".
   
  if avail cust then
  do i = 1 to 4:
    v-addr-info = if i eq 1 then cust.name    else
                  if i eq 2 then cust.addr[1] else
                  if i eq 3 then cust.addr[2] else
                  (cust.city + ", " +
                   cust.state + "  " +
                   cust.zip).
          
    if trim(v-addr-info) eq "," then v-addr-info = "".
    
    if v-addr-info ne "" then
      assign
       j         = j + 1
       v-cust[j] = v-addr-info.
  end.
        
  assign
   j      = 0
   v-ship = "".
   
  if avail shipto then
  do i = 1 to 4:
    v-addr-info = if i eq 1 then shipto.ship-name      else
                  if i eq 2 then shipto.ship-addr[1]   else
                  if i eq 3 then shipto.ship-addr[2]   else
                  (shipto.ship-city + ", " +
                   shipto.ship-state + "  " +
                   shipto.ship-zip).
                         
    if trim(v-addr-info) eq "," then v-addr-info = "".               
    
    if v-addr-info ne "" then
      assign
       j         = j + 1
       v-ship[j] = v-addr-info.
  end.

  FIND FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no USE-INDEX X-no NO-LOCK NO-ERROR.
  
  ASSIGN
    v-bol-no = IF AVAIL ar-invl THEN ar-invl.bol-no ELSE 0
    v-del-no = 0
    v-tot-pallets = 0
    v-printline = 32
    v-salesman = ""
    v-date-ship = TODAY.

  find first stax
      {sys/ref/stax1W.i}
        and {sys/ref/taxgroup.i stax} eq ar-inv.tax-code
      no-lock no-error.

  if avail stax then
    assign v-tax-code[1] = stax.tax-code[1]
           v-tax-code[2] = stax.tax-code[2]
           v-tax-code[3] = stax.tax-code[3]
           v-tx-rate[1]  = stax.tax-rate[1]
           v-tx-rate[2]  = stax.tax-rate[2]
           v-tx-rate[3]  = stax.tax-rate[3].

  IF v-bol-no NE 0 THEN
  DO:
     find first oe-bolh where oe-bolh.company eq ar-inv.company and
          oe-bolh.bol-no eq v-bol-no use-index bol-no no-lock no-error.
      if avail oe-bolh then do:
        find first oe-relh where oe-relh.company eq oe-bolh.company and
                   oe-relh.r-no eq oe-bolh.r-no no-lock no-error.
        if avail oe-relh then
        find first shipto where shipto.company  eq oe-bolh.company and
                   shipto.cust-no eq oe-relh.cust-no and
                   shipto.ship-id eq oe-bolh.ship-id no-lock no-error.
        IF AVAIL shipto THEN
        DO:
           v-shipto = shipto.ship-id.
           RELEASE shipto.
        END.

        RELEASE oe-relh.
        RELEASE oe-bolh.
      END.
  END.

  FOR EACH ar-invl WHERE
      ar-invl.x-no = ar-inv.x-no /*AND
      ar-invl.ord-no ne 0*/
      USE-INDEX X-no NO-LOCK:

      IF v-del-no EQ 0 THEN
         v-del-no = ar-invl.ord-no.

      do i = 1 to 3:

         if ar-invl.sman[i] ne "" then do:
           create w-sman.
           assign w-sman.sman = ar-invl.sman[i].
         end.
      END.

      for each oe-bolh no-lock where oe-bolh.b-no eq ar-invl.b-no:

         ASSIGN
           v-date-ship = oe-bolh.bol-date
           v-tot-pallets = v-tot-pallets + oe-bolh.tot-pallets.
      end. /* each oe-bolh */
  END.

  v-salesman = "".
  for each w-sman break by w-sman.sman:
    if first-of(w-sman.sman) then
      assign v-salesman = v-salesman + w-sman.sman.
    delete w-sman.
  end.

  IF v-print-head then
     DISPLAY
     ar-inv.inv-date
     v-c-name
     v-c-addr[1]
     v-c-addr[2]
     ar-inv.inv-no
     v-c-city
     v-c-state
     v-c-zip
     v-salesman
     v-fob
     ar-inv.terms-d
     ar-inv.cust-no
     ar-inv.ship-id
     v-cust[1]
     v-ship[1]
     v-cust[2]
     v-ship[2]
     v-cust[3]
     v-ship[3]
     v-cust[4]
     v-ship[4]
     v-date-ship
     v-shipvia
     ar-inv.t-weight
     v-del-no
     v-bol-no
     v-tot-pallets
     WITH FRAME ilwalker-comp.
  
  ELSE 
     DISPLAY
       v-invhead when oe-ctrl.prcom
       ar-inv.inv-date
       v-c-name
       v-c-addr[1]
       v-c-addr[2]
       ar-inv.inv-no
       v-c-city
       v-c-state
       v-c-zip
       v-c-phone
       v-salesman
       v-c-fax
       v-c-email
       v-fob
       ar-inv.terms-d
       ar-inv.cust-no
       v-shipto
       v-cust[1]
       v-ship[1]
       v-cust[2]
       v-ship[2]
       v-cust[3]
       v-ship[3]
       v-cust[4]
       v-ship[4]
       v-date-ship
       v-shipvia 
       ar-inv.t-weight
       v-del-no
       v-bol-no
       v-tot-pallets
       WITH FRAME invhead.
        
  ASSIGN v-line-number = 22.

  for each ar-invl where ar-invl.x-no = ar-inv.x-no BREAK BY ar-invl.line:
    
    v-subtot = v-subtot + ar-invl.amt.
    create wf.
    buffer-copy ar-invl to wf.
  END.

  for each wf no-lock where wf.x-no eq ar-inv.x-no:  
      assign
       v-case-line = ""
       v-part-line = ""
       v-case-cnt = "".
      
      for each oe-boll
          where oe-boll.company eq wf.company
            and oe-boll.bol-no eq v-bol-no
            and oe-boll.i-no eq wf.i-no
            and oe-boll.ord-no eq wf.ord-no
          no-lock use-index bol-no:
          
                                 /** Build Case Count Display Lines **/
        if oe-boll.cases ne 0 and oe-boll.qty-case ne 0 then
          v-case-line = string(oe-boll.cases) + " @ " +
                        string(oe-boll.qty-case).
        else v-case-line = "".
        
        if oe-boll.partial ne 0 then
          v-part-line = "1" + " @ " + string(oe-boll.partial).
        else v-part-line = "".

        do i = 1 to 5:
          if (80 - length(v-case-cnt[i])) gt length(v-case-line) and
             v-case-line ne "" then
            assign
             v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
             v-case-line   = "".
          if (80 - length(v-case-cnt[i])) gt length(v-part-line) and
             v-part-line ne "" then
            assign
             v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
             v-part-line   = "".
        end. /* 1 to 5 */
      end. /* each oe-boll */

      assign
        v-printline = v-printline + 2
        v-beeler-lines = 0.
        
        do v = 1 to 3:
          
          ASSIGN
          v-ord# = 0
          v-part-info = if      v eq 1 then wf.part-no
                        else if v eq 2 then wf.part-dscr1
                        else                wf.part-dscr2.

          if v-part-info ne "" then
             v-beeler-lines = v-beeler-lines + 1.
        end.

        do i = 1 to 5:
          if v-case-cnt[i] ne "" then
            assign v-beeler-lines = v-beeler-lines + 1.
        end.
        
        v-printline = v-printline + v-beeler-lines.
        
        if v-printline + 2 ge end-page then do:
          put "* CONTINUED *" at 68 SKIP.
          v-printline = 32 /*+ v-beeler-lines*/ .
          page.

          IF v-print-head then
             DISPLAY
             ar-inv.inv-date
             v-c-name
             v-c-addr[1]
             v-c-addr[2]
             ar-inv.inv-no
             v-c-city
             v-c-state
             v-c-zip
             v-salesman
             v-fob
             ar-inv.terms-d
             ar-inv.cust-no
             ar-inv.ship-id
             v-cust[1]
             v-ship[1]
             v-cust[2]
             v-ship[2]
             v-cust[3]
             v-ship[3]
             v-cust[4]
             v-ship[4]
             v-date-ship
             v-shipvia
             ar-inv.t-weight
             v-del-no
             v-bol-no
             v-tot-pallets
             WITH FRAME ilwalker-comp.
          ELSE 
             DISPLAY
               v-invhead when oe-ctrl.prcom
               ar-inv.inv-date
               v-c-name
               v-c-addr[1]
               v-c-addr[2]
               ar-inv.inv-no
               v-c-city
               v-c-state
               v-c-zip
               v-c-phone
               v-salesman
               v-c-fax
               v-c-email
               v-fob
               ar-inv.terms-d
               ar-inv.cust-no
               v-shipto
               v-cust[1]
               v-ship[1]
               v-cust[2]
               v-ship[2]
               v-cust[3]
               v-ship[3]
               v-cust[4]
               v-ship[4]
               v-date-ship
               v-shipvia 
               ar-inv.t-weight
               v-del-no
               v-bol-no
               v-tot-pallets
               WITH FRAME invhead.
        end.
          
        find first oe-ordl WHERE
             oe-ordl.company eq wf.company
             and oe-ordl.ord-no  eq wf.ord-no
             /*and oe-ordl.line    eq wf.line*/
             and oe-ordl.i-no    eq wf.i-no
             no-lock no-error.

        display oe-ordl.po-no WHEN AVAIL oe-ordl
                wf.i-no wf.inv-qty
                wf.unit-pr wf.pr-uom wf.amt
                wf.po-no wf.i-name wf.ship-qty
                with frame ilw-detail.

        down with frame ilw-detail.
  
        do v = 1 to 3:
          
          ASSIGN
          v-ord# = 0
          v-part-info = if      v eq 1 then wf.part-no
                        else if v eq 2 then wf.part-dscr1
                        else                wf.part-dscr2.

          if v-part-info ne "" then do:
            display v-ord# v-part-info with frame beeler.
            down with frame beeler.
          end.
        end.

        /** Display Case Count Lines **/
        do i = 1 to 5:
          if v-case-cnt[i] ne "" then do:
            put v-case-cnt[i] at 5 skip.
          end.
        end. /* 1 to 5 */

        put skip(1).
        v-printline = v-printline + 1.

  END. /*end each wf*/

  PUT SKIP(1).
  
  do note-cnt = 1 to 4:
    if ar-inv.bill-i[note-cnt] ne "" then
    DO:
      put ar-inv.bill-i[note-cnt] at 6 skip.
      v-printline = v-printline + 1.
    END.
  end.
  put skip(1).

  /* T O T A L S */
  put skip(end-page + 1 - v-printline - 10).

  assign
   tmp1  = 0
   tmp2  = ?
   v-net = ar-inv.net.

  release terms.
  find first terms where terms.t-code eq ar-inv.terms no-lock no-error.


  if avail terms then
    assign
     tmp1 = v-net * (round(terms.disc-rate / 100, 2))
     tmp2 = today + terms.disc-days.

  find first cust where cust.company eq cocode and
             cust.cust-no eq ar-inv.cust-no no-lock no-error.
  if avail cust and cust.sort eq "Y" then do:

    assign
     net1 = v-net * (v-tx-rate[1] / 100)
     net2 = v-net * (v-tx-rate[2] / 100)
     net3 = v-net * (v-tx-rate[3] / 100)

     net1 = net1 + (ar-inv.freight * (v-tx-rate[1] / 100))
     net2 = net2 + (ar-inv.freight * (v-tx-rate[2] / 100))
     net3 = net3 + (ar-inv.freight * (v-tx-rate[3] / 100))

     net1 = round(net1,2)
     net2 = round(net2,2)
     net3 = round(net3,2).

    if ar-inv.tax-amt ne (net1 + net2 + net3) then
      if net3 gt 0 then
        net3 = net3 + (ar-inv.tax-amt - (net1 + net2 + net3)).
      else
      if net2 gt 0 then
        net2 = net2 + (ar-inv.tax-amt - (net1 + net2 + net3)).
      else
        net1 = net1 + (ar-inv.tax-amt - (net1 + net2 + net3)).

    display
        v-tax-code[1]
        net1
        v-tax-code[2]
        net2
        v-tax-code[3]
        net3
        ar-inv.tax-amt skip(1)
        with frame tax.
  end.
  else display " " skip(1) with frame blankl. 

  clear frame totals-comp  no-pause.
  clear frame totals-comp2 no-pause.
  clear frame totals      no-pause.

  if v-print-head /* Print invoice headers */ then
    if cust.sort eq "N" then do:
       display ar-inv.freight
               ar-inv.tax-amt
               v-subtot
               tmp1 when avail terms
               tmp2 when avail terms
               v-subtot @ ar-inv.due
               ar-inv.due when avail terms or
                               ar-inv.terms eq "CASH"
                 with frame totals-comp.
    end.
    else do:
       display ar-inv.freight
               v-subtot
               tmp1 when avail terms
               tmp2 when avail terms
               v-subtot @ ar-inv.due
               ar-inv.due when avail terms or
                           ar-inv.terms eq "CASH"
                 with frame totals-comp2.
    end.

  else
    if cust.sort eq "N" then do:
       display disp-frt
               ar-inv.freight
               ar-inv.tax-amt
               v-subtot
               tmp1 when avail terms
               tmp2 when avail terms
               v-subtot @ ar-inv.due
               ar-inv.due when avail terms or
                         ar-inv.terms eq "CASH"
                  with frame totals.
    end.
    else 
       display disp-frt  /*  DAR */
               ar-inv.freight
               v-subtot
               tmp1 when avail terms
               tmp2 when avail terms
               v-subtot @ ar-inv.due
               ar-inv.due when avail terms or
                         ar-inv.terms eq "CASH"
                  with frame totals.
  
  ar-inv.printed = yes.
end. /* for each ar-inv record */

/* End ---------------------------- Copr. 1992 - 1994  Advanced Software Inc. */
