/* ---------------------------------------------- ar/rep/invaccrd.p */
/* PRINT INVOICE   Xprint form for Midwest            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{ar/rep/invoice.i}

def var v-salesman as char format "x(14)" NO-UNDO.
def var v-salesname as char format "x(30)" NO-UNDO.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO.
def var v-addr3 as char format "x(30)" NO-UNDO.
def var v-sold-addr3 as char format "x(30)" NO-UNDO.
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
def var tmp1 as dec NO-UNDO.
def var tmp2 as date NO-UNDO.
def var net1 as dec NO-UNDO.
def var net2 as dec NO-UNDO.
def var net3 as dec NO-UNDO.
def var cnt as int NO-UNDO.
def var disp-frt as char init "Freight:" format "x(8)" NO-UNDO.
def var minus-ship as int NO-UNDO.
DEF VAR v-pc AS cha NO-UNDO. /* partial or complete */
DEF VAR v-i-dscr2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR lv-email AS cha FORM "x(48)" NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-page-num AS INT NO-UNDO.
DEF VAR v-bolno AS INT NO-UNDO.
def buffer xar-inv for ar-inv .
DEF BUFFER xar-invl FOR ar-invl.


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
DEF VAR v-lines AS INT NO-UNDO.
DEF VAR v-inv-freight LIKE ar-inv.freight NO-UNDO.
DEF VAR v-frt-tax AS DEC NO-UNDO.
DEF VAR lv-inv-list AS CHAR NO-UNDO.
DEF VAR li-inv-list AS CHAR  NO-UNDO. 
DEF VAR v-tmp-lines AS dec NO-UNDO.
DEF VAR v-notes AS cha EXTENT 4 FORM "x(80)" NO-UNDO.
DEF VAR v-notes-line AS INT NO-UNDO.
DEF VAR v-inv-total AS DEC NO-UNDO.

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(150)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(150)" NO-UNDO.
ASSIGN ls-image1 = "images\accord.jpg"
       ls-image2 = "images\accord.jpg".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
FILE-INFO:FILE-NAME = ls-image2.
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "INVPRINT" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".


 IF lv-display-comp THEN DO:
    FIND FIRST cust WHERE cust.company = cocode AND
                       cust.active = "X" NO-LOCK NO-ERROR.
    IF AVAIL cust THEN
       ASSIGN v-comp-add1 = cust.addr[1]
           v-comp-add2 = cust.addr[2]
           v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
           v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
           v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
           lv-email    = "Email:  " + cust.email 
           lv-comp-name = cust.NAME   
           .
    
 END.
    
    find first company where company.company = cocode no-lock no-error.
/*    ASSIGN v-comp-add1 = company.addr[1]
           v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
           v-comp-add3 = "Phone: 604.533.2545" 
           v-comp-add4 = "Fax  : 604.533.2633".
*/  
    /*ASSIGN v-comp-add1 = "MIDWEST FIBRE PRODUCTS, INC."
           v-comp-add2 = "2819 95TH AVENUE - PO BOX 397 - VIOLA ILLINOIS 61486"
           v-comp-add3 = "          (309)596-2955   FAX (309)596-2901"
           v-comp-add4 = "CORRUGATED CARTONS - FOLDING CARTONS - SET-UP BOXES"
           .*/
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
              v-shipto-zip = ar-inv.sold-zip.
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
                     v-shipto-zip = shipto.ship-zip.                            
     END.

   v-del-no = 0.
/*
   find first oe-bolh where oe-bolh.company = ar-inv.company and
                    oe-bolh.bol-no = ar-inv.bol-no use-index bol-no no-lock no-error.
      if avail oe-bolh then do:
        find first oe-relh where oe-relh.company = oe-bolh.company and
                   oe-relh.r-no = oe-bolh.r-no no-lock no-error.
        if avail oe-relh then
        find first shipto where shipto.company  = oe-bolh.company and
                   shipto.cust-no = oe-relh.cust-no and
                   shipto.ship-no = oe-bolh.ship-no no-lock no-error.
        if avail shipto then
        assign  v-shipto-name = shipto.ship-name
                v-shipto-addr[1] = shipto.ship-addr[1]
                v-shipto-addr[2] = shipto.ship-addr[2]
                v-shipto-city = shipto.ship-city
                v-shipto-state = shipto.ship-state
                v-shipto-zip = shipto.ship-zip.

      end. /* avail oe-bolh */
   */
  
      /**
        if v-reprint and ar-inv.inv-no = 0 then next.
        else if not v-reprint and ar-inv.inv-no ne 0 then next.
      **/
     
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
         /*
         FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = ar-invl.b-no 
             /*oe-bolh.ord-no = ar-invl.ord-no*/ :
           v-pc = "P". /* partial*/ 
           FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = oe-bolh.company AND
              oe-boll.b-no = oe-bolh.b-no AND
              oe-boll.i-no = ar-invl.i-no AND
              oe-boll.ord-no = ar-invl.ord-no:

                                      /** Bill Of Lading TOTAL CASES **/
              ASSIGN v-bol-cases = v-bol-cases + oe-boll.cases
                     v-tot-pallets = v-tot-pallets + oe-boll.cases +
                                     (if oe-boll.partial gt 0 then 1 else 0).
              IF oe-boll.p-c THEN v-pc = "C". /*complete*/
              
           END. /* each oe-boll */
           assign v-date-ship = oe-bolh.bol-date
                 /* v-tot-pallets = v-tot-pallets + v-bol-cases +
                                  (if oe-boll.partial gt 0 then 1 else 0) */.
         END. /* each oe-bolh */
         */
         

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
                 /***
                 ASSIGN v-tot-pallets = v-tot-pallets +
                      ROUND((v-tot-cas  / eb.cas-pal) + .49, 0).
                 ***/
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
               /***
               ASSIGN v-tot-pallets = v-tot-pallets +
                   ROUND((v-tot-cas  / eb.cas-pal) + .49, 0).
               ***/
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
 /*
        find first oe-bolh where oe-bolh.company = ar-inv.company and
                                 oe-bolh.bol-no = ar-inv.bol-no
                                 USE-INDEX bol-no no-lock no-error.
        if avail oe-bolh then
          assign v-rel-po-no = oe-bolh.po-no.
 */
        /*
        find first ar-invl where ar-invl.r-no = ar-inv.r-no
                                  no-lock no-error.
        if avail ar-invl then
        do:
          assign v-price-head = ar-invl.pr-uom.
          find first oe-ord where oe-ord.company = cocode and
                                  oe-ord.ord-no = ar-invl.ord-no
                                  no-lock no-error.
          if avail oe-ord then
          do:
            assign v-po-no = oe-ord.po-no
                   v-bill-i = oe-ord.bill-i[1]
                   v-ord-no = oe-ord.ord-no
                   v-ord-date = oe-ord.ord-date.
          end.
          else
            assign v-price-head = ar-invl.pr-uom.
        end.
        */
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
        do:
           assign v-price-head = ar-invl.pr-uom
                  v-po-no = ar-invl.po-no                  
                  v-ord-no = ar-invl.ord-no
                  v-bolno  = ar-invl.bol-no
                  .
        END.
        /* display heder info 
         view frame invhead-comp.  /* Print headers */
                */
 {ar/rep/invaccrd.i}  /* xprint form */
  v-printline = 28.

        v-subtot-lines = 0.
        v-t-tax = 0.
        for each ar-invl no-lock where ar-invl.x-no = ar-inv.x-no:
          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = "".
          /*
          v-pc = "P". /* partial*/ 
          for each oe-boll no-lock where oe-boll.company = ar-invl.company
                        and oe-boll.bol-no = ar-inv.bol-no
                        /*and oe-boll.b-no = ar-invl.b-no*/
                        and oe-boll.i-no = ar-invl.i-no use-index bol-no:

                                       /** Build Case Count Display Lines **/
            if oe-boll.cases ne 0 and oe-boll.qty-case ne 0 then
            assign v-case-line = string(oe-boll.cases) + " @ " +
                                     string(oe-boll.qty-case).
            else assign v-case-line = "".
            if oe-boll.partial ne 0 then
            assign v-part-line = "1" + " @ " + string(oe-boll.partial).
            else assign v-part-line = "".

            IF oe-boll.p-c THEN v-pc = "C". /*complete*/

            do i = 1 to 5:
              if (80 - length(v-case-cnt[i])) > length(v-case-line) and
                v-case-line ne "" then
              assign v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                     v-case-line = "".
              if (80 - length(v-case-cnt[i])) > length(v-part-line) and
                v-part-line ne "" then
              assign v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
                     v-part-line = "".
            end. /* 1 to 5 */

            
          end. /* each oe-boll */
*/
          IF v-printline GE 56 THEN do:           
                PAGE.
                {ar/rep/invaccrd.i}
                v-printline = 28.
          END.
           ASSIGN lv-inv-list = "" .
          /*  assign v-line = v-line + 1
                   v-printline = v-printline + 2.  */
           
           find first oe-ordl where oe-ordl.company = cocode and
                                     oe-ordl.ord-no = ar-invl.ord-no and
                                     oe-ordl.i-no = ar-invl.i-no AND
                                     oe-ordl.LINE = ar-invl.LINE
                                     no-lock no-error.
           if avail oe-ordl THEN DO:
            IF NOT CAN-FIND(FIRST oe-boll
                              WHERE oe-boll.company EQ ar-invl.company
                                AND oe-boll.b-no    EQ ar-invl.b-no
                                AND oe-boll.po-no   EQ ar-invl.po-no
                                AND oe-boll.ord-no  EQ oe-ordl.ord-no
                                AND oe-boll.i-no    EQ oe-ordl.i-no
                                AND oe-boll.line    EQ oe-ordl.line
                                AND oe-boll.s-code  EQ "I"
                              USE-INDEX b-no) THEN
              FOR EACH xar-invl
                  WHERE xar-invl.company EQ oe-ordl.company
                    AND xar-invl.ord-no  EQ oe-ordl.ord-no
                    AND xar-invl.i-no    EQ oe-ordl.i-no
                    AND ROWID(xar-invl)  NE ROWID(ar-invl)
                    AND CAN-FIND(FIRST oe-boll
                                 WHERE oe-boll.company EQ xar-invl.company
                                   AND oe-boll.b-no    EQ xar-invl.b-no
                                   AND oe-boll.po-no   EQ xar-invl.po-no
                                   AND oe-boll.ord-no  EQ oe-ordl.ord-no
                                   AND oe-boll.i-no    EQ oe-ordl.i-no
                                   AND oe-boll.line    EQ oe-ordl.line
                                   AND oe-boll.s-code  EQ "I"
                                 USE-INDEX b-no)
                    
                  NO-LOCK
                ,
                FIRST xar-inv
                    WHERE xar-inv.x-no EQ xar-invl.x-no
                      AND xar-inv.inv-date LT ar-inv.inv-date
                NO-LOCK:
                lv-inv-list = lv-inv-list + TRIM(STRING(xar-invl.inv-no,">>>>>>>>>>")) + " ".
              END.
            END.

           if avail oe-ordl then
              assign v-bo-qty = if (ar-invl.qty - ar-invl.ship-qty -
                                    oe-ordl.t-ship-qty) < 0 then 0 else
                                   (ar-invl.qty - ar-invl.ship-qty -
                                    oe-ordl.t-ship-qty).
            else
              assign v-bo-qty = if ( ar-invl.qty - ar-invl.ship-qty ) < 0
                                  then 0 else ar-invl.qty - ar-invl.ship-qty.

             IF lv-inv-list <> "" THEN
                  li-inv-list = "Previous Invoice(s): " .

            assign v-inv-qty = ar-invl.qty
                   v-ship-qty = ar-invl.ship-qty
                   v-i-no = ar-invl.i-no
                   v-i-dscr = ar-invl.i-name
                   v-price = ar-invl.unit-pr * (1 - (ar-invl.disc / 100))
                   v-t-price = ar-invl.amt
                   v-subtot-lines = v-subtot-lines + ar-invl.amt.

           /*
                if ar-invl.tax and avail stax then
                do i = 1 to 3:
                  if stax.tax-code[i] ne "" then do:
                    create w-tax.
                    assign
                     w-dsc      = stax.tax-dscr[i]
                     w-tax      = round((if stax.company eq "yes" then v-t-price
                                                                  else ar-invl.amt) *
                                        stax.tax-rate[i] / 100,2)
                     v-t-price  = v-t-price + w-tax
                     v-t-tax[i] = v-t-tax[i] + w-tax
                     v-lines    = v-lines + 1.
                  end.
                end.
           */
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

            FIND FIRST itemfg WHERE itemfg.company = cocode
                                        AND itemfg.i-no = ar-invl.i-no NO-LOCK NO-ERROR.
                    IF AVAIL itemfg AND v-i-dscr2 = "" THEN
                        ASSIGN v-i-dscr2 = itemfg.part-dscr1 .

            IF v-i-dscr2 = "" THEN v-i-dscr2 = ar-invl.i-dscr.
            IF v-ord-no = 0 AND v-ship-qty = 0 THEN v-ship-qty = v-inv-qty.

            
            PUT space(1)
                v-po-no 
                v-i-no  format "x(15)" SPACE(1)
                v-i-dscr  format "x(23)" SPACE(1)
                v-inv-qty format "->>>>>>9" SPACE(1) 
                v-ship-qty  format "->>>>>>>" SPACE(1)
              /*  v-bo-qty  format "->>>>>9" SPACE(1)
                v-i-no  format "x(15)" SPACE(1)  space(13)  */                             
                v-price  format ">>>>9.9999" SPACE(1)               
                ar-invl.amt  format "->>>,>>9.99"                
                SKIP
                SPACE(1) v-ord-no FORM ">>>>>>" SPACE(9)
                ar-invl.part-no format "x(15)" SPACE(1)
                v-i-dscr2 space(19)
                v-price-head SPACE(1) SKIP
                /*space(16) ar-invl.part-dscr2 */
                .

               IF lv-inv-list <> "" AND ar-invl.part-dscr2 <> ""  THEN
                   PUT  li-inv-list FORMAT "x(20)" lv-inv-list FORMAT "x(12)" SPACE(1)
                        ar-invl.part-dscr2 .
               ELSE IF lv-inv-list <> "" AND ar-invl.part-dscr2 = "" THEN
                  PUT SPACE(16) li-inv-list FORMAT "x(20)" lv-inv-list FORMAT "x(15)" .
               ELSE
                   PUT SPACE(32) ar-invl.part-dscr2 . 

             v-printline = v-printline + 3.
             
            put skip(1).
            v-printline = v-printline + 1.
            
            
        end. /* each ar-invl */
        
    /*
        for each inv-misc no-lock where inv-misc.company = ar-inv.company and
          inv-misc.r-no = ar-inv.r-no and
          inv-misc.bill = "Y" break by ord-no with frame detailm:
          IF v-printline > 62 THEN do:
              
                PAGE.                
                {ar/rep/invaccrd.i}
                v-printline = 21.
          END.
          if first(inv-misc.ord-no) then
          do:
            put "** Miscellaneous Items **" at 23 skip(1).
            assign v-printline = v-printline + 2.
          end.
/*
            if v-printline gt 29 then
            do:
              put skip(30 - v-printline) "* CONTINUED *" at 72.
              assign v-printline = 0.
              page.

            end.
*/
            put inv-misc.charge AT 17 inv-misc.dscr inv-misc.amt AT 85 SKIP.
            v-subtot-lines = v-subtot-lines + inv-misc.amt.
            v-printline = v-printline + 1.
            if inv-misc.tax and avail stax then
            do i = 1 to 3:
              if stax.tax-code[i] ne "" then do:
                create w-tax.
                assign
                 w-dsc      = stax.tax-dscr[i]
                 w-tax      = if stax.company eq "yes" then v-t-price
                              else inv-misc.amt
                 w-tax      = round(w-tax * (1 + (stax.tax-rate[i] / 100)),2) - w-tax
                 v-t-price  = v-t-price + w-tax
                 v-t-tax[i] = v-t-tax[i] + w-tax
                 v-lines    = v-lines + 1.
              end.
            end.

            if v-t-price ne inv-misc.amt then do:
              create w-tax.
              assign
               w-dsc     = "******ITEM TOTAL:"
               w-tax     = v-t-price
               v-lines   = v-lines + 1.
            end.

        end. /* each inv-misc */
    */

        if v-prntinst then do:
         do i = 1 to 4:
          if ar-inv.bill-i[i] ne "" then do:
       /*     if v-printline gt 29 then
            do:
              put skip(30 - v-printline) "* CONTINUED *" at 72.
              assign v-printline = 0.
              page.

            end.
       */
            put ar-inv.bill-i[i] at 10 skip.
            assign v-printline = v-printline + 1.
          end.
         end. /* 1 to 4 */
        end.

        /* T O T A L S */
     /*   put skip(30 - v-printline). */
   /*
        find first cust where cust.company = cocode and
                   cust.cust-no = ar-inv.cust-no no-lock no-error.
        if avail cust and cust.sort = "Y" AND ar-inv.t-inv-tax <> 0 then
          put "Total Tax: " ar-inv.t-inv-tax SKIP.

        else PUT " " skip(1) .

        if ar-inv.f-bill then
           put disp-frt
               ar-inv.t-inv-freight
               ar-inv.t-inv-rev SKIP .
        else
          put "" SKIP.
              
  */
        
        ASSIGN v-notes = ""
               v-notes-line = 0.

        NOTES:
        FOR EACH notes WHERE notes.rec_key = ar-inv.rec_key NO-LOCK:
             v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
             {SYS/INC/ROUNDUP.I v-tmp-lines}

            IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
                ASSIGN v-notes-line = v-notes-line + 1
                       v-notes[v-notes-line] = substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80)
                       .              

                
                IF v-notes-line >= 4  THEN LEAVE notes.
            /*
                v-printline = v-printline + 1.
                IF v-printline > 62 THEN do:           
                   PAGE.
                   {ar/rep/invaccrd.i}
                   v-printline = 21.
                END.  */
        
             END.
        end.
        IF v-notes[1] <> "" OR v-notes[2] <> "" OR v-notes[3] <> "" OR v-notes[4] <> "" THEN
          IF v-printline GE 56 THEN do:           
                PAGE.
                {ar/rep/invaccrd.i}
                v-printline = 28.
          END.
        ELSE
            IF v-printline GE 56 THEN do:           
                PAGE.
                {ar/rep/invaccrd.i}
                v-printline = 28.
            END.

        v-frt-tax = ar-inv.freight.
        v-inv-freight = if ar-inv.f-bill THEN ar-inv.freight
/*                         ELSE IF ar-inv.freight <> 0 THEN ar-inv.freight */
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
           END.
        end.      


    do i = 1 to 3:
       v-bot-lab[i] = if v-t-tax[i] ne 0 then
                    /*  ((if avail stax then string(stax.tax-dscr[i],"x(5)")
                        else fill(" ",5))*/ 
                        ((IF AVAIL stax THEN string(CAPS(stax.tax-code[i]),"x(5)") 
                           ELSE FILL(" ",5) ) +
                       fill(" ",6) + ":" +
                       string(v-t-tax[i],"->>>>>9.99")) else "".
    end.
                        /*ar-inv.t-inv-freight*/.
    v-inv-total = v-subtot-lines + ar-inv.tax-amt + v-inv-freight
                /*  - (v-t-tax[1] + v-t-tax[2] + v-t-tax[3]) */ .

    PUT "<R58><C58><#8><FROM><R+5><C+22><RECT> " 
        "<=8> Sub Total    :" v-subtot-lines FORM "->>,>>9.99"
        "<=8><R+1> Freight      :" v-inv-freight
        /*"<=8><R+2> ""  " v-bot-lab[1]  */
        "<=8><R+2> Sales Tax    :" ar-inv.tax-amt FORM "->>,>>9.99"
                  /*  v-bot-lab[2] */
        "<=8><R+3>" "" 
        "<=8><R+4> Total Invoice:" v-inv-total FORM "->>,>>9.99" . /* ar-inv.gross*/


    PUT "<FArial><R58><C1><#9><P12><B> THANK YOU. </B> <P9> " SKIP
   /*      "  Your business is greatly appreciated! Thank You!"SKIP
         "  Please pay by invoice - no statements are issued." SKIP
         "  24% per annum interest charge on overdue accounts. " SKIP
         "  Any credit card purchases are subject to a charge of 2%." SKIP
         "  GST # R129463212.  Check out our website at www.pacificpackaging.ca" SKIP.
    */                                                        
        "<=9><R-6>" v-notes[1]
        "<=9><R-5>" v-notes[2]
        "<=9><R-4>" v-notes[3]
        "<=9><R-3>" v-notes[4]
        .
    v-printline = v-printline + 6.

   
    IF v-printline <= 66 THEN page. /*PUT SKIP(74 - v-printline). */
     
    DO TRANSACTION:
       FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
       ASSIGN xar-inv.printed = yes.
              xar-inv.stat = "X".
    END. /* DO TRANSACTION avail ar-inv */ 

    
 
    end. /* each ar-inv */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
