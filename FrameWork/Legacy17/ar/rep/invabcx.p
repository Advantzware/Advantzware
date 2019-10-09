/* ---------------------------------------------- ar/rep/invabcx.p */
/* PRINT INVOICE   Xprint form for Action                          */
/* -------------------------------------------------------------------------- */
DEF INPUT PARAM ip-copy-title AS cha NO-UNDO.

{sys/inc/var.i shared}

{ar/rep/invoice.i}
{custom/notesdef.i}
def var v-salesman as char format "x(14)" NO-UNDO.
def var v-salesname as char format "x(30)" NO-UNDO.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO.
def var v-addr3 as char format "x(30)" NO-UNDO.
def var v-sold-addr3 as char format "x(30)" NO-UNDO.
def var v-shipto-id as char format "x(10)" NO-UNDO.
def var v-shipto-name as char format "x(30)" NO-UNDO.
def var v-shipto-addr as char format "x(30)" extent 2 NO-UNDO.
def var v-shipto-city as char format "x(15)" NO-UNDO.
def var v-shipto-state as char format "x(2)" NO-UNDO.
def var v-shipto-zip as char format "x(10)" NO-UNDO.
def var v-line as int NO-UNDO.
def var v-printline as INT NO-UNDO.
def var v-invhead as char format "x(13)" initial
  "I N V O I C E".
def var v-pitch like asi.printer.pitch NO-UNDO.
def var v-len as int NO-UNDO.
def var v-hldpitch like asi.printer.pitch NO-UNDO.
def var v-t-weight like ar-invl.t-weight NO-UNDO.
def var v-inv-no as int NO-UNDO.
def var v-tot-cas as dec format "->>>9.9999" NO-UNDO.
def var v-tot-pallets as int NO-UNDO.
def var v-tot-qty as INT NO-UNDO.
def var v-inv-date as date initial TODAY FORM "99/99/9999" NO-UNDO.
def shared var v-fr-tax as logical initial no NO-UNDO.
def var v-tax-rate as dec format "->>>.99" NO-UNDO.
def var v-tax-code like stax.tax-code NO-UNDO.
def var v-tx-rate like stax.tax-rate NO-UNDO.
def var v-ans as logical initial no NO-UNDO.
def var v-date-ship as date initial today NO-UNDO.
def var v-del-no as int format ">>>>>>" NO-UNDO.
def var v-bol-cases LIKE oe-boll.cases NO-UNDO.
def var v-set-qty AS INT NO-UNDO.
def var v-part-qty AS DEC FORMAT "999.9999" NO-UNDO.
def var v-net like ar-inv.gross NO-UNDO.
def var v-case-cnt as char format "x(80)" extent 5 NO-UNDO.
def var v-case-line as char NO-UNDO.
def var v-part-line as char NO-UNDO.
def var net1 as dec NO-UNDO.
def var net2 as dec NO-UNDO.
def var net3 as dec NO-UNDO.
def var cnt as int NO-UNDO.
def var disp-frt as char init "Freight:" format "x(8)" NO-UNDO.
def var minus-ship as int NO-UNDO.
DEF VAR v-pc AS cha NO-UNDO. /* partial or complete */
DEF VAR v-i-dscr2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-bol-no LIKE oe-boll.bol-no NO-UNDO.

def buffer xar-inv for ar-inv .


def workfile w-sman
  field sman as char format "x(4)".

def var v-ord-del-hdr as char format "x(3)" init "Del".
def var v-beeler-lines as int.
def var v-part-info as char format "x(30)".
def var v as int.
def var v-bo-qty as int format "99999" no-undo.
def var v-inv-qty as int format "99999" no-undo.
def var v-ship-qty as int format "99999" no-undo.
def var v-i-no as char format "x(15)" no-undo.
def var v-i-dscr as char format "x(18)" no-undo.
def var v-price as dec format ">>>>9.9999" no-undo.
def var v-t-price as dec format ">>>>>>9.99" no-undo.
def var v-po-no like ar-invl.po-no no-undo.
def var v-bill-i as char format "x(25)" no-undo.
def var v-ord-no like oe-ord.ord-no no-undo.
def var v-ord-date like oe-ord.ord-date no-undo.
def var v-ship-i as char format "x(25)" no-undo.
def var v-rel-po-no like oe-rel.po-no no-undo.
def var v-price-head as char format "x(5)" no-undo.
DEF VAR v-subtot-lines AS DEC NO-UNDO.
def workfile w-tax
    field w-dsc as   char
    field w-tax as   dec.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-bot-name   AS CHAR FORMAT "X(5)" EXTENT 3 NO-UNDO.
DEF VAR v-lines AS INT NO-UNDO.
DEF VAR v-inv-freight LIKE ar-inv.freight NO-UNDO.
DEF VAR v-frt-tax AS DEC NO-UNDO.
DEF VAR v-notes AS cha EXTENT 4 FORM "x(80)" NO-UNDO.
DEF VAR v-notes-line AS INT NO-UNDO.
DEF VAR v-inv-total AS DEC NO-UNDO.

/* === with xprint ====*/
DEF VAR ls-full-img1 AS cha FORM "x(50)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(50)" NO-UNDO.

ASSIGN
  FILE-INFO:FILE-NAME = "images\ABCAdd.jpg"
  ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">"
  FILE-INFO:FILE-NAME = "images\ABCInvPar.jpg"
  ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
    
    find first company where company.company = cocode no-lock no-error.
    find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

    for each report where report.term-id eq v-term-id no-lock,
        first ar-inv where recid(ar-inv) eq report.rec-id no-lock
        break by report.key-01
              by report.key-02:

      FIND FIRST cust WHERE cust.company = ar-inv.company
                        AND cust.cust-no = ar-inv.cust-no NO-LOCK NO-ERROR.
      IF ar-inv.sold-name <> "" THEN
         assign  v-shipto-name = ar-inv.sold-name
              v-shipto-addr[1] = ar-inv.sold-addr[1]
              v-shipto-addr[2] = ar-inv.sold-addr[2]
              v-shipto-city = ar-inv.sold-city
              v-shipto-state = ar-inv.sold-state
              v-shipto-zip = ar-inv.sold-zip
              v-shipto-id = ar-inv.sold-id.
     ELSE DO:
         FIND FIRST shipto WHERE shipto.company = ar-inv.company
                             AND shipto.cust-no = ar-inv.cust-no
                             AND shipto.ship-id = ar-inv.ship-id NO-LOCK NO-ERROR.
         IF AVAIL shipto THEN
             assign  v-shipto-name = shipto.ship-name
                     v-shipto-addr[1] = shipto.ship-addr[1]
                     v-shipto-addr[2] = shipto.ship-addr[2]
                     v-shipto-city = shipto.ship-city
                     v-shipto-state = shipto.ship-state
                     v-shipto-zip = shipto.ship-zip
                     v-shipto-id = shipto.ship-id.                            
     END.

   v-del-no = 0.

   
   if ar-inv.inv-date ne ? THEN assign v-inv-date = ar-inv.inv-date
                                       v-date-ship = ar-inv.inv-date.

   if ar-inv.fob-code begins "ORIG" THEN assign v-fob = "Origin".
   ELSE assign v-fob = "Destination".

   find FIRST carrier where carrier.company = ar-inv.company and
                            carrier.carrier = ar-inv.carrier no-lock no-error.
   if avail carrier THEN assign v-shipvia = carrier.dscr.
   ELSE assign v-shipvia = "".

   assign
     v-addr3 = ar-inv.city + ", " + ar-inv.state + "  " + ar-inv.zip
     v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
         "  " + v-shipto-zip
     v-line = 1
     v-printline = 0.
   
   find first stax
       {sys/ref/stax1W.i}
         and {sys/ref/taxgroup.i stax} eq ar-inv.tax-code
       no-lock no-error.
   if not avail stax then
   find first stax where stax.tax-group eq ar-inv.tax-code
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

   assign v-tot-pallets = 0.

   for each ar-invl NO-LOCK where ar-invl.x-no eq ar-inv.x-no  
                break by ar-invl.i-no:
     do i = 1 to 3:
        if ar-invl.sman[i] ne "" then do:
          create w-sman.
          assign w-sman.sman = ar-invl.sman[i].
        end.
     end.
     assign v-tot-qty = v-tot-qty + ar-invl.ship-qty
            v-t-weight = v-t-weight + (round(ar-invl.t-weight /
                         ar-invl.qty, 2) * ar-invl.inv-qty).
    v-tot-pallets = 0.
    v-pc = "C". /* complete*/
    
    if last-of(ar-invl.i-no) then do:
      if ar-invl.est-no ne "" then
      do:
        find first eb where eb.company = ar-invl.company and
          eb.est-no = ar-invl.est-no and
          eb.e-num = ar-invl.e-num and
          eb.form-no = ar-invl.form-no and
          eb.blank-no = ar-invl.blank-no no-lock no-error.

        IF ar-invl.form-no = 0 AND ar-invl.est-type = 2 THEN
        DO:
          FOR EACH fg-set NO-LOCK WHERE fg-set.company = ar-invl.company
             AND fg-set.set-no = ar-invl.i-no:
            ASSIGN v-set-qty = v-set-qty + fg-set.part-qty.
          END.
          IF v-set-qty = 0 THEN
             ASSIGN v-set-qty = 1.
          FOR EACH eb NO-LOCK WHERE eb.company = ar-invl.company AND
             eb.est-no = ar-invl.est-no AND
             eb.e-num = ar-invl.e-num AND
             eb.form-no NE 0:
            FIND fg-set WHERE fg-set.company = ar-invl.company AND
               fg-set.set-no = ar-invl.i-no  AND
               fg-set.part-no = eb.stock-no NO-LOCK NO-ERROR.

            IF AVAIL fg-set AND fg-set.part-qty NE 0 THEN
              ASSIGN v-part-qty = fg-set.part-qty / v-set-qty.
            ELSE
              ASSIGN v-part-qty = 1 / v-set-qty.


           IF eb.cas-cnt = 0 THEN
              ASSIGN v-tot-cas = ROUND((v-t-weight * v-part-qty) /
                                 eb.cas-wt, 2).
            ELSE
              ASSIGN v-tot-cas = ROUND((v-tot-qty * v-part-qty) /
                                 eb.cas-cnt, 2).
            if v-bol-cases ne 0 then
              assign v-tot-cas = v-bol-cases.
          END. /* each eb */
        END. /* do */
        ELSE
        IF AVAIL eb THEN
        DO:
          IF eb.cas-cnt = 0 THEN
            ASSIGN v-tot-cas = ROUND(v-t-weight / eb.cas-wt, 2).
          ELSE
            ASSIGN v-tot-cas = ROUND(v-tot-qty / eb.cas-cnt, 2).
          if v-bol-cases ne 0 then
            assign v-tot-cas = v-bol-cases.
        END. /* do */
      end. /* est-no ne "" */
     assign
        v-t-weight = 0
        v-tot-cas = 0
        v-tot-qty = 0.
    end. /* last-of i-no */
   end. /* each ar-invl */
   
                   /** Build Salesman Id String **/
   v-salesman = "".
   for each w-sman break by w-sman.sman:
     if first-of(w-sman.sman) then
       assign v-salesman = v-salesman + w-sman.sman.
     delete w-sman.
   end.

   assign v-po-no = ar-inv.po-no
          v-bill-i = ar-inv.bill-i[1]
          v-ord-no = ar-inv.ord-no
          v-ord-date = ar-inv.ord-date.
   IF v-salesman = "" THEN DO:
      find first oe-ord where oe-ord.company eq cocode
                          and oe-ord.ord-no   eq ar-inv.ord-no
                          no-lock no-error.
      IF AVAIL oe-ord THEN do:
         FIND FIRST sman WHERE sman.company = cocode AND
                               sman.sman = oe-ord.sman[1] NO-LOCK NO-ERROR.
         v-salesman = oe-ord.sman[1].
         IF AVAIL sman THEN v-salesname = sman.sname.
      END.
      IF v-salesman = "" THEN DO:
         v-salesman = cust.sman.
         FIND FIRST sman WHERE sman.company = cocode AND
                               sman.sman = cust.sman NO-LOCK NO-ERROR.              
         IF AVAIL sman THEN v-salesname = sman.sname.
      END.
   END.
   
   find first ar-invl where ar-invl.x-no = ar-inv.x-no no-lock no-error.
   if avail ar-invl then
      assign v-price-head = ar-invl.pr-uom
             v-po-no = ar-invl.po-no                  
             v-ord-no = ar-invl.ord-no
             lv-bol-no = ar-invl.bol-no.

 {ar/rep/invabcx.i}  /* xprint form */

   ASSIGN
     v-subtot-lines = 0
     v-t-tax = 0.
   for each ar-invl no-lock where ar-invl.x-no = ar-inv.x-no:
     assign v-case-line = ""
            v-part-line = ""
            v-case-cnt = ""
            i = 0.

     for each oe-boll no-lock where oe-boll.company = ar-invl.company
                   and oe-boll.bol-no = ar-invl.bol-no
                   and oe-boll.i-no = ar-invl.i-no /*use-index bol-no*/:

                                  /** Build Case Count Display Lines **/
       if oe-boll.cases ne 0 and oe-boll.qty-case ne 0 then
       assign v-case-line = string(oe-boll.cases) + " @ " +
                                string(oe-boll.qty-case).
       else assign v-case-line = "".
       if oe-boll.partial ne 0 then
       assign v-part-line = "1" + " @ " + string(oe-boll.partial).
       else assign v-part-line = "".

       IF oe-boll.p-c THEN v-pc = "C". /*complete*/
       i = i + 1.
       IF i < 6 THEN DO:
         if (80 - length(v-case-cnt[i])) > length(v-case-line) and
           v-case-line ne "" then
         assign v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                v-case-line = "".
         if (80 - length(v-case-cnt[i])) > length(v-part-line) and
           v-part-line ne "" then
         assign v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
                v-part-line = "".
       END.
     end. /* each oe-boll */

     IF v-printline > 45 THEN do:           
        PAGE.
        {ar/rep/invabcx.i}  /* xprint form */
        v-printline = 21.
     END.
      
     find first oe-ordl where oe-ordl.company = cocode and
                                oe-ordl.ord-no = ar-invl.ord-no and
                                oe-ordl.i-no = ar-invl.i-no AND
                                oe-ordl.LINE = ar-invl.LINE
                                no-lock no-error.
      if avail oe-ordl then
         assign v-bo-qty = if (ar-invl.qty - ar-invl.ship-qty -
                               oe-ordl.t-ship-qty) < 0 then 0 else
                              (ar-invl.qty - ar-invl.ship-qty -
                               oe-ordl.t-ship-qty).
       else
         assign v-bo-qty = if ( ar-invl.qty - ar-invl.ship-qty ) < 0
                             then 0 else ar-invl.qty - ar-invl.ship-qty.

       assign v-inv-qty = ar-invl.qty
              v-ship-qty = ar-invl.ship-qty
              v-i-no = ar-invl.i-no
              v-i-dscr = ar-invl.i-name
              v-price = ar-invl.unit-pr * (1 - (ar-invl.disc / 100))
              v-t-price = ar-invl.amt
              v-subtot-lines = v-subtot-lines + ar-invl.amt.
      
           if ar-invl.tax and avail stax then
           do i = 1 to 3:
             if stax.tax-code[i] ne "" then do:
               create w-tax.
               assign
                w-dsc      = stax.tax-dscr[i]
                w-tax      = round((if stax.accum-tax then v-t-price
                                                      else ar-invl.amt) *
                                   stax.tax-rate[i] / 100,2)
                v-t-price  = v-t-price + w-tax
                v-t-tax[i] = v-t-tax[i] + w-tax
                v-lines    = v-lines + 1.

               IF stax.accum-tax AND i GT 1 THEN /*tax on tax*/
                  v-t-tax[i] = v-t-tax[i] + ROUND(w-tax * stax.tax-rate[i] / 100,2).
             end.
           end.
      
           if v-t-price ne ar-invl.amt then do:
             create w-tax.
             assign
              w-dsc     = "******ITEM TOTAL:"
              w-tax     = v-t-price
              v-lines   = v-lines + 1.
           end.

       ASSIGN v-po-no  = ar-invl.po-no
              v-ord-no = ar-invl.ord-no
              v-price-head = ar-invl.pr-uom.
       v-i-dscr2 = ar-invl.part-dscr1.
       IF v-i-dscr2 = "" THEN v-i-dscr2 = ar-invl.i-dscr.
       IF v-ord-no = 0 AND v-ship-qty = 0 THEN v-ship-qty = v-inv-qty.
       PUT space(1)
           v-po-no 
           ar-invl.part-no  SPACE(1)
           v-i-dscr FORM "x(30)" 
           v-ship-qty  format "->>>>>>9" SPACE(2)
           v-price  format "->>,>>9.9999"                
           ar-invl.amt  format "->>>,>>9.99"                
           SKIP
           v-ord-no SPACE(10)
           ar-invl.i-no SPACE(1)
           v-i-dscr2  SPACE(11)
           v-pc  FORM "x" SPACE(7)
           v-price-head SKIP.
           
       v-printline = v-printline + 2.
       do i = 1 to 5:
          if v-case-cnt[i] ne "" THEN DO:
              PUT v-case-cnt[i] SKIP.
              v-printline = v-printline + 1.
          END.
       end.
       put skip(1).
       v-printline = v-printline + 1.
   end. /* each ar-invl */
   
   IF v-prntinst THEN
   do i = 1 to 4:
     if ar-inv.bill-i[i] ne "" then do:
        put ar-inv.bill-i[i] at 10 skip.
        assign v-printline = v-printline + 1.
     end.
   end. /* 1 to 4 */

   IF v-printline > 45 THEN do:           
      PAGE.
      {ar/rep/invabcx.i}  /* xprint form */
      v-printline = 21.
   END.

   ASSIGN v-notes = ""
          v-notes-line = 0
          lv-line-chars = 80.

   {custom/notesprtA.i ar-inv v-notes 4}
   
   v-frt-tax = ar-inv.freight.
   v-inv-freight = if ar-inv.f-bill THEN ar-inv.freight
                   ELSE 0.
   
   IF ar-inv.tax-code <> "" and
      ar-inv.f-bill AND ar-inv.freight <> 0 AND AVAIL stax THEN
   do i = 1 to 3:
      if stax.tax-code[i] ne "" then do:
           create w-tax.
           assign
            w-dsc      = stax.tax-dscr[i]
            w-tax      = round((if stax.company eq "yes" then v-frt-tax
                                                    ELSE ar-inv.freight) *
                                   stax.tax-rate[i] / 100,2)                 
            v-frt-tax  = v-frt-tax + w-tax
            v-t-tax[i] = v-t-tax[i] + w-tax
            v-lines    = v-lines + 1.

            IF stax.accum-tax AND i GT 1 THEN /*tax on tax*/
               v-t-tax[i] = v-t-tax[i] + ROUND(w-tax * stax.tax-rate[i] / 100,2).
      END.
   end.      

    do i = 1 to 3:
       ASSIGN
         v-bot-lab[i] = STRING(v-t-tax[i],"->>>>>9.99")
         v-bot-name[i] = IF AVAIL stax THEN FILL(" ",5 - LENGTH(stax.tax-code[i])) + stax.tax-code[i]  
                         ELSE FILL(" ",5).
    end.
                        
    ASSIGN
       v-inv-total = v-subtot-lines + v-t-tax[1] + v-t-tax[2] + v-t-tax[3] + v-inv-freight
       v-net = ar-inv.net.

    PUT "<R57><C1><#7><FROM><C+81.5><LINE>"
        "<=7><C28><FROM><R+2.4><LINE>"
        "<=7><C40><FROM><R+2.4><LINE>"
        "<=7><C50><FROM><R+2.4><LINE>"
        "<=7><C60><FROM><R+2.4><LINE>"
        "<=7><C71><FROM><R+2.4><LINE>"
        "<=7><C82.5><FROM><R+2.4><LINE>"
        "<=7><C28>" space(1) v-net FORM '->,>>>,>>9.99' SPACE(3) v-inv-freight FORM '>>,>>9.99' SPACE(2) v-bot-lab[1] SPACE(2) v-bot-lab[2] space(1) v-inv-total FORM "->>>>,>>9.99"
        "<=7><R+1.2><C28><FROM><C+54.5><LINE>"
        "<=7><R+1.2><C35.2><P6>SUB TOTAL<C46>FREIGHT               " v-bot-name[1] SPACE(17) v-bot-name[2] "         INVOICE AMOUNT"
        "<=7><R+2.4><C28><FROM><C+54.5><LINE>"
        "<R58.4><C1><#8>" 
        "<R+5><C+26.8><IMAGE#8=" ls-full-img2
        "<FArial><R61><C63><#9><P12><B> THANK YOU. </B> <P9> " SKIP
        "<=9><R-6>" v-notes[1]
        "<=9><R-5>" v-notes[2]
        "<=9><R-4>" v-notes[3]
        "<=9><R-3>" v-notes[4].
    
    v-printline = v-printline + 6.

    IF v-printline <= 66 THEN page.
     
    DO TRANSACTION:
       FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
       ASSIGN xar-inv.printed = yes.
              xar-inv.stat = "X".
    END. /* DO TRANSACTION avail ar-inv */ 
 
    end. /* each ar-inv */
