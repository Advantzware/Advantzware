/* ------------------------------------------------ ar/rep/invimper.p 7/95 CAH*/
/* A/R Invoice Print Program - A/R Module                                     */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

{ar/rep/invoice.i}

def var save_id         AS recid no-undo.
def var v-print-align   AS log format "Y/N" no-undo.
def var v-align-ok      AS log format "Y/N" no-undo.
def var v-line-number   AS int no-undo.
def var v-subtot        like ar-inv.t-sales no-undo column-label "Subtotal".
def var v-invhead AS char format "x(13)" init
  "I N V O I C E".
def var letterhead AS char no-undo format 'x(50)' EXTENT 8.
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
def var end-page as int.
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
DEF VAR v-date-ship AS DATE FORM "99/99/99" NO-UNDO.
DEF VAR v-shipvia AS cha FORM "x(30)" NO-UNDO.
def var v-del-no as int format ">>>>>>" NO-UNDO.
DEF VAR v-bol-no LIKE oe-bolh.bol-no NO-UNDO.
def var v-tot-pallets as INT NO-UNDO.
def var v-tax-rate as dec format "->>>.99" NO-UNDO.
def var v-tax-code like stax.tax-code NO-UNDO.
def var v-tx-rate like stax.tax-rate NO-UNDO.
def var net1 as dec NO-UNDO.
def var net2 as dec NO-UNDO.
def var net3 as dec NO-UNDO.
def var tmp1 as dec NO-UNDO.
def var tmp2 as date NO-UNDO.
def var v-net like inv-head.t-inv-rev NO-UNDO.
def var disp-frt as char init "Freight:" NO-UNDO.
big_ul = FILL('=',80).
big_ul2 = big_ul.

      
format 
      v-invhead AT 24
      ar-inv.inv-date        AT 72 FORMAT "99/99/99" skip
      v-c-name AT 11 skip
      v-c-addr[1] AT 11 skip
      v-c-addr[2] AT 11
      ar-inv.inv-no          AT 69 format "zzzzz9" skip
      v-c-city AT 11
      v-c-state v-c-zip skip
      v-c-phone at 11
      sman.sman         AT 69
      v-c-fax at 11
      v-c-email at 11
      ar-inv.fob-code        AT 4
      /*ar-inv.terms           AT 32 */
      ar-inv.terms-d      AT 32    format "x(20)"
      skip (5)
      ar-inv.cust-no         AT 11 ar-inv.ship-id         AT 55
      v-cust[1]              AT 11 v-ship[1]              AT 55 format "x(27)"
      v-cust[2]              AT 11 v-ship[2]              AT 55 format "x(27)"
      v-cust[3]              AT 11 v-ship[3]              AT 55 format "x(27)"
      v-cust[4]              AT 11 v-ship[4]              AT 55 format "x(27)"
      skip (2)
      v-date-ship
      v-shipvia AT 11 FORM "x(25)"
      ar-inv.t-weight TO 44 FORM ">>>>9.99" v-del-no TO 53 v-bol-no TO 65 FORM ">>>>>9"
      v-tot-pallets AT 69 SKIP(2)
      WITH FRAME invoice-header no-BOX no-labels stream-io width 85.
      
format 
      SKIP(1)
      v-invhead AT 24
      ar-inv.inv-date        AT 72 FORMAT "99/99/99" skip
      v-c-name AT 11 skip
      v-c-addr[1] AT 11 skip
      v-c-addr[2] AT 11
      ar-inv.inv-no          AT 69 format "zzzzz9" skip
      v-c-city AT 11
      v-c-state v-c-zip skip
      v-c-phone at 11
      sman.sman         AT 69
      v-c-fax at 11
      v-c-email at 11
      ar-inv.fob-code        AT 4
      /*ar-inv.terms           AT 32 */
      ar-inv.terms-d      AT 32    format "x(20)"
      skip (3)
      ar-inv.cust-no         AT 11 ar-inv.ship-id         AT 55
      v-cust[1]              AT 11 v-ship[1]              AT 55 format "x(27)"
      v-cust[2]              AT 11 v-ship[2]              AT 55 format "x(27)"
      v-cust[3]              AT 11 v-ship[3]              AT 55 format "x(27)"
      v-cust[4]              AT 11 v-ship[4]              AT 55 format "x(27)"
      skip (2)
      v-date-ship
      v-shipvia AT 11 FORM "x(25)"
      ar-inv.t-weight TO 44 FORM ">>>>9.99" v-del-no TO 53 v-bol-no TO 65 FORM ">>>>>9"
      v-tot-pallets AT 69 SKIP(1)
      WITH FRAME invoice-header-ag no-BOX no-labels stream-io width 85.

format ar-inv.po-no           AT 1
       ar-invl.i-name AT 19 FORM "x(23)" 
       ar-invl.inv-qty        TO 52 format "->,>>>,>>9"
       v-price        to 64 format "->>,>>9.99" SPACE(0)
       ar-invl.pr-uom TO 68
       ar-invl.amt            TO 80 format "->>>,>>9.99"  /* DAR 9507 CAH + >, */
       ar-invl.i-dscr         AT 19 FORM "x(23)"
       ar-invl.ship-qty TO 52 FORM "->,>>>,>>9"
       skip 
       WITH FRAME invoice-line no-BOX no-labels stream-io width 85 DOWN.

format skip (1)
  ar-inv.net             AT 30 format "->,>>>,>>9.99" /* 9507 CAH was 6 dig */
  ar-inv.disc-taken      AT 45 format "zzz,zz9.99"
  ar-inv.due-date        AT 59 FORMAT "99/99/99" 
  ar-inv.due             AT 68 format "-z,zzz,zz9.99" /* 9507 CAH was 6 dig */
  WITH FRAME invoice-total no-BOX no-labels stream-io width 80.
      
format skip (2)
      letterhead[1] AT 5
      "     Date:" TO 70 ar-inv.inv-date FORMAT "99/99/99" skip
      letterhead[2] AT 5
      letterhead[3] AT 5
      "Invoice #:" TO 70 ar-inv.inv-no skip
      letterhead[4] AT 5
      letterhead[5] AT 5
      letterhead[6] AT 5
      letterhead[7] AT 5
      letterhead[8] AT 5
      skip(1)
      "SOLD TO:" AT 11
      ar-inv.cust-no
      "SHIP TO:" AT 55
      ar-inv.ship-id
      v-cust[1]              AT 11
      v-ship[1]              AT 55 format "x(27)"
      v-cust[2]              AT 11
      v-ship[2]              AT 55 format "x(27)"
      v-cust[3]              AT 11
      v-ship[3]              AT 55 format "x(27)"
      v-cust[4]              AT 11
      v-ship[4]              AT 55 format "x(27)"
      skip(5)
      big_ul AT 1
      WITH FRAME invoice-header-labels no-BOX no-labels stream-io width 85.
      
format skip (2)
      letterhead[1] AT 5
      "     Date:" TO 70 ar-inv.inv-date FORMAT "99/99/99" skip
      letterhead[2] AT 5
      letterhead[3] AT 5
      "Invoice #:" TO 70 ar-inv.inv-no skip
      letterhead[4] AT 5
      letterhead[5] AT 5
      letterhead[6] AT 5
      letterhead[7] AT 5
      letterhead[8] AT 5
      skip(1)
      "SOLD TO:" AT 11
      ar-inv.cust-no
      "SHIP TO:" AT 55
      ar-inv.ship-id
      v-cust[1]              AT 11
      v-ship[1]              AT 55 format "x(27)"
      v-cust[2]              AT 11
      v-ship[2]              AT 55 format "x(27)"
      v-cust[3]              AT 11
      v-ship[3]              AT 55 format "x(27)"
      v-cust[4]              AT 11
      v-ship[4]              AT 55 format "x(27)"
      skip(5)
      big_ul AT 1
      WITH FRAME invoice-header-labels-ag no-BOX no-labels stream-io width 85.

format
  ar-inv.po-no   AT 6
  /* ar-inv.terms 9507 CAH */
  ar-inv.terms-d    column-label "Terms"
  ar-inv.fob        column-label "FOB"
  ar-inv.carrier    column-label "Via"
  /* carrier.dscr      column-label "Carrier" */
  ar-inv.t-weight   column-label "Weight" format "->>,>>9"
  sman.sman        column-label "Rep" skip
  WITH FRAME INVOICE-RIBBON-labels no-BOX no-UNDERLINE stream-io width 80 1 DOWN.

format
  ar-invl.i-name  column-label "Item / Description" AT 6
  ar-invl.inv-qty format "zzz,zz9.99" column-label "Quantity"
  v-price format "->,>>>,>>9.99<<" column-label "Price"
  ar-invl.amt     column-label "Amount"
  WITH FRAME INVOICE-LINE-labels no-BOX stream-io width 80 DOWN.

format
    space(21)   /* to align with line item amount field */
  v-subtot format "$->>,>>>,>>9.99"
  ar-inv.freight  format "->>>,>>9.99"
  column-label "Freight"
  ar-inv.tax-amt  format "->>>,>>9.99"
  column-label "Sales Tax"
  /*
  column-label "Net Amount"
  ar-inv.disc-taken       format "->>,>>9.99"
  column-label "Cash Disc"
  ar-inv.due-date
  column-label "If Paid By"
  ar-inv.due              format "->,>>>,>>>.99"
  */
  ar-inv.net              format "$->>,>>>,>>9.99"
  column-label "Invoice Amt"
  WITH FRAME invoice-total-labels no-BOX no-UNDERLINE stream-io width 85.

form "Taxes- " at 1
   v-tax-code[1] at 9 format "x(4)"
   space(0) ":"
   net1 at 15 format "->>,>>9.99"
/*    v-tax-code[2] at 28 format "x(4)" */
/*    space(0) ":"                      */
   net2 at 34 format "->>,>>9.99"
/*     v-tax-code[3] at 46 format "x(4)" */
/*     space(0) ":"                      */
   net3 at 47 format "->>,>>9.99"
   "Tot Tax:" to 70
   space(0)
   ar-inv.tax-amt  to 80 format "->>,>>9.99"
   with frame tax no-labels no-box no-underline stream-io width 90.

 form disp-frt at 1
    ar-inv.freight format "->,>>9.99" at 11
    ar-inv.tax-amt format "->,>>9.99" at 21
    v-net to 43 format "->,>>>,>>9.99"
   /* tmp1 to 56 format "->,>>9.99"
    tmp2 at 60
    space(0)
    ar-inv.net to 80 format "->,>>>,>>9.99"
    */
     ar-inv.disc-taken  SPACE(3)
     ar-inv.due-date  FORM "99/99/99" 
     ar-inv.due  FORM "->,>>>,>>9.99"    
    with frame totals no-labels no-box no-underline stream-io width 90.


end-page = (IF v-print-fmt EQ "AgMach" THEN 52 ELSE 56) - 2.

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

ASSIGN v-c-fax = ""
       v-c-phone = ""
       v-c-email = "".

if v-print-head then do:
  assign
   letterhead[1] = if oe-ctrl.prcom then "=== " + v-invhead + " ===" else ""
   letterhead[2] = v-c-name
   letterhead[3] = v-c-addr[1]
   letterhead[4] = v-c-addr[2]
   letterhead[5] = v-c-city + ", " + v-c-state + "  " + v-c-zip
   letterhead[6] = v-c-phone
   letterhead[7] = v-c-fax
   letterhead[8] = v-c-email.

  do i = 5 to 2 by -1:
    if letterhead[i - 1] le "" and letterhead[i] ge "" then
      assign
       letterhead[i - 1] = letterhead[i]
       letterhead[i]     = "".
  end.
  
  do i = 1 to 8:
    {sys/inc/ctrtext.i letterhead[i] 50}.
  end.
end.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id:

  v-recs = v-recs + 1.

  CLEAR FRAME invoice-header ALL no-PAUSE.
  CLEAR FRAME invoice-header-ag ALL no-PAUSE.
  assign
    v-disc-avail = 0
    v-disc-date = ?
    v-subtot = 0.

  find first cust where cust.company = cocode and
    cust.cust-no = ar-inv.cust-no no-lock no-error.

  find first shipto where shipto.company = cocode and
    shipto.cust-no = ar-inv.cust-no and
    shipto.ship-id = ar-inv.ship-id no-lock no-error.

  find first carrier where carrier.company = cocode and
    carrier.carrier = ar-inv.carrier no-lock no-error.
  if avail carrier THEN ASSIGN v-shipvia = carrier.dscr.
  else assign v-shipvia = "".
  v-date-ship = ar-inv.inv-date.

  find first sman where sman.company = cocode and
    sman.sman = cust.sman no-lock no-error.

  find first ar-invl where ar-invl.x-no = ar-inv.x-no no-lock no-error.
  v-bol-no = if avail ar-invl THEN ar-invl.bol-no ELSE 0 .  

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

  IF v-print-head then
  DO:                     /* Print Headers */
     IF v-print-fmt EQ "AgMach" THEN
       DISPLAY
          ar-inv.inv-date
          letterhead[1]
          letterhead[2]
          letterhead[3]
          letterhead[4]
          letterhead[5]
          letterhead[6]
          letterhead[7]
          letterhead[8]
          ar-inv.inv-no
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
          big_ul
          WITH FRAME invoice-header-labels-ag.
     ELSE
       DISPLAY
          ar-inv.inv-date
          letterhead[1]
          letterhead[2]
          letterhead[3]
          letterhead[4]
          letterhead[5]
          letterhead[6]
          letterhead[7]
          letterhead[8]
          ar-inv.inv-no
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
          big_ul
          WITH FRAME invoice-header-labels.

        DISPLAY
          ar-inv.po-no
          ar-inv.terms-d
          ar-inv.carrier
          ar-inv.fob-code
          ar-inv.t-weight WHEN ar-inv.t-weight <> 0
          sman.sman when avail sman
          WITH FRAME invoice-ribbon-labels.
        PUT skip BIG_UL2 skip(1).
  end.
  ELSE do:
      IF v-print-fmt EQ "AgMach" THEN
          DISPLAY
            ar-inv.inv-date
            v-c-name
            v-c-addr[1]
            ar-inv.inv-no
            v-c-addr[2]
            v-c-city
            v-c-state
            v-c-zip
            v-c-phone
            sman.sman when avail sman
            v-c-fax
            v-c-email
            ar-inv.fob-code
            "" @ ar-inv.terms
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
          /*  "" @ ar-inv.carrier
            carrier.dscr         WHEN AVAILABLE carrier */
            v-date-ship
            v-shipvia 
            ar-inv.t-weight  v-del-no v-bol-no 
            v-tot-pallets              
            WITH FRAME invoice-header-ag.

      ELSE
          DISPLAY
            ar-inv.inv-date
            v-c-name
            v-c-addr[1]
            ar-inv.inv-no
            v-c-addr[2]
            v-c-city
            v-c-state
            v-c-zip
            v-c-phone
            sman.sman when avail sman
            v-c-fax
            v-c-email
            ar-inv.fob-code
            "" @ ar-inv.terms
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
          /*  "" @ ar-inv.carrier
            carrier.dscr         WHEN AVAILABLE carrier */
            v-date-ship
            v-shipvia 
            ar-inv.t-weight  v-del-no v-bol-no 
            v-tot-pallets              
            WITH FRAME invoice-header.
  END.
    
  ASSIGN v-line-number = 22.

  for each ar-invl where ar-invl.x-no = ar-inv.x-no BREAK BY ar-invl.line:
    v-price = ar-invl.unit-pr.

    IF v-print-head then DO WITH FRAME invoice-line-labels:
      if ar-invl.ord-no ne 0 then
        DISPLAY
          ar-invl.i-name
          ar-invl.inv-qty
          v-price
          ar-invl.amt.
      else
        DISPLAY
          ar-invl.i-name
          ar-invl.qty @ ar-invl.inv-qty
          v-price
          ar-invl.amt.
      DOWN 1.
      IF ar-invl.i-dscr > ' ' then DO:
        DISPLAY ar-invl.i-dscr @ ar-invl.i-name.
        DOWN 1.
      end.
      DOWN 1.
    end.
    ELSE DO:
      if ar-invl.ord-no ne 0 then
        DISPLAY ar-inv.po-no
                ar-invl.i-name
                ar-invl.inv-qty
                v-price
                ar-invl.pr-uom
                ar-invl.amt
                ar-invl.i-dscr
                WITH FRAME invoice-line DOWN.
      else
        DISPLAY ar-inv.po-no
                ar-invl.i-name
                ar-invl.qty @ ar-invl.inv-qty
                v-price
                ar-invl.pr-uom
                ar-invl.amt
                ar-invl.i-dscr
            WITH FRAME invoice-line DOWN.
      DOWN 1 WITH FRAME invoice-line.
    end.

    v-subtot = v-subtot + ar-invl.amt.

    IF v-print-head then
    DO:    /* page break logic for drawn form */
      IF LINE-COUNTER >= (PAGE-SIZE - 7 - 2) then
      DO:
        PUT skip(1)
          space(29) "- - - CONTINUED - - -".
          PAGE.

        VIEW FRAME invoice-header-labels.
        VIEW FRAME invoice-ribbon-labels.
        PUT skip BIG_UL2 skip(1).
        VIEW FRAME invoice-line-labels.
      end.
    end.
    ELSE DO:        /* page break logic for manual form */
      ASSIGN v-line-number = v-line-number + 3.
      IF v-line-number >= 52 - 2 then
      DO:
        DISPLAY skip (1)
          "- - - CONTINUED - - -" AT 30
/*        skip (7) */
          WITH FRAME page-break no-BOX no-labels no-ATTR-SPACE.
        PAGE.

        DISPLAY WITH FRAME invoice-header.
        v-line-number = 22.
      end.
    end.    /* preprinted form */


  

  end. /* for each ar-invl record */

  assign note-lines = 1.
  IF v-prntinst THEN
  do note-cnt = 1 to 4:
    if ar-inv.bill-i[note-cnt] ne "" then note-lines = note-lines + 1.
  end.

  if line-counter + note-lines >= (page-size - 7) then
  do:
    IF v-print-head then
    DO:
      put skip(1) space(29) "- - - CONTINUED - - -".
      page.
      view frame invoice-header-labels.
      view frame invoice-ribbon-labels.
      put skip BIG_UL2 skip(1).
      view frame invoice-line-labels.
    end.
    else
    do:
        DISPLAY skip (1)
          "- - - CONTINUED - - -" AT 30
          WITH FRAME page-break1 no-BOX no-labels no-ATTR-SPACE.
        PAGE.
        DISPLAY WITH FRAME invoice-header.
        v-line-number = 22.
    end.
  end.

  put skip(end-page - note-lines - line-counter).

  IF v-prntinst THEN
  do note-cnt = 1 to 4:
    if ar-inv.bill-i[note-cnt] ne "" then
      put ar-inv.bill-i[note-cnt] at 6 skip.
  end.
  put skip(1).

  if ar-inv.disc-% > 0 and ar-inv.net > 0 then do:
    assign
        v-disc-avail = round(ar-inv.net * (ar-inv.disc-% / 100), 2)
        v-disc-date  = ar-inv.inv-date + ar-inv.disc-days.
    if v-disc-avail = 0 then v-disc-date = ?.
  end.

  IF v-print-head then
  DO:
    adv = end-page - LINE-COUNTER.
    PUT skip(adv).

    DISPLAY
      v-subtot
      ar-inv.freight
      ar-inv.tax-amt
      ar-inv.net
      WITH FRAME invoice-total-labels.
    if v-disc-avail > 0 then
        put skip(1)
            space(30)
            "If paid by " v-disc-date " you may deduct:"
             v-disc-avail format "$>>>,>>>.99".
  end.
  ELSE
  DO:
      find first stax
            {sys/ref/stax1W.i}
              and {sys/ref/taxgroup.i stax} eq ar-inv.tax-code
            no-lock no-error.

        if avail stax then
          assign v-tax-rate = stax.tax-rate[1] +
                              stax.tax-rate[2] + stax.tax-rate[3]
                 v-tax-code[1] = stax.tax-code[1]
                 v-tax-code[2] = stax.tax-code[2]
                 v-tax-code[3] = stax.tax-code[3]
                 v-tx-rate[1]  = stax.tax-rate[1]
                 v-tx-rate[2]  = stax.tax-rate[2]
                 v-tx-rate[3]  = stax.tax-rate[3].

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
         if avail cust /*and cust.sort eq "Y" */ then do:

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
             else net1 = net1 + (ar-inv.tax-amt - (net1 + net2 + net3)).

           display
               v-tax-code[1]
               net1
/*                v-tax-code[2] */
               net2
/*                v-tax-code[3] */
               net3
               ar-inv.tax-amt skip(1)
               with frame tax.
           disp disp-frt 
                ar-inv.freight 
                ar-inv.tax-amt 
                v-net 
                v-disc-avail @ ar-inv.disc-taken
                v-disc-date  @ ar-inv.due-date                 
                ar-inv.due
                WITH FRAME totals.
         END.

    adv = end-page - LINE-COUNTER + 2.
    PUT skip(adv).
   /*
    DISPLAY ar-inv.net
      v-disc-avail @ ar-inv.disc-taken
      v-disc-date  @ ar-inv.due-date
      ar-inv.due
      WITH FRAME invoice-total.
    */  
  end.

  ar-inv.printed = yes.
end. /* for each ar-inv record */

/* End ---------------------------- Copr. 1992 - 1994  Advanced Software Inc. */
