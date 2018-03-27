/* ---------------------------------------------- oe/rep/invaccrd.p */
/* PRINT INVOICE   Xprint form for Accord            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/invoice.i}

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
def var v-t-weight like inv-line.t-weight NO-UNDO.
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
def var v-net like inv-head.t-inv-rev NO-UNDO.
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

def buffer xinv-head for inv-head .
def buffer xinv-line for inv-line .

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
def var v-i-dscr-2 as char format "x(18)" no-undo.
def var v-price as dec format ">>>>9.9999" no-undo.
def var v-t-price as dec format ">>>>>>9.99" no-undo.
def var v-po-no like inv-line.po-no no-undo.
def var v-po-ord like inv-line.po-no no-undo.
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
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-frt-tax AS DEC NO-UNDO.
DEF VAR lv-inv-list AS CHAR FORMAT "x(15)" NO-UNDO.  
DEF VAR li-inv-list AS CHAR  NO-UNDO. 

FIND FIRST inv-head NO-LOCK NO-ERROR.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(200)" NO-UNDO.
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
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-email AS cha FORM "x(48)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR v-page-num AS INT NO-UNDO.
DEF VAR cBillNotes LIKE inv-head.bill-i NO-UNDO.
DEFINE BUFFER bf-inv-head FOR inv-head .

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
   /* ASSIGN v-comp-add1 = "MIDWEST FIBRE PRODUCTS, INC."
           v-comp-add2 = "2819 95TH AVENUE - PO BOX 397 - VIOLA ILLINOIS 61486"
           v-comp-add3 = "          (309)596-2955   FAX (309)596-2901"
           v-comp-add4 = "CORRUGATED CARTONS - FOLDING CARTONS - SET-UP BOXES"
           .*/
    find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

    for each report where report.term-id eq v-term-id no-lock,
        first xinv-head where recid(xinv-head) eq report.rec-id no-lock
        break by report.key-01
              by report.key-02:

      FIND FIRST cust WHERE cust.company = xinv-head.company
                        AND cust.cust-no = xinv-head.cust-no NO-LOCK NO-ERROR.

      assign  v-shipto-name = xinv-head.sold-name
              v-shipto-addr[1] = xinv-head.sold-addr[1]
              v-shipto-addr[2] = xinv-head.sold-addr[2]
              v-shipto-city = xinv-head.sold-city
              v-shipto-state = xinv-head.sold-state
              v-shipto-zip = xinv-head.sold-zip.

      v-del-no  = 0.
      v-po-ord  = "" .

      find first oe-bolh where oe-bolh.company = xinv-head.company and
          oe-bolh.bol-no = xinv-head.bol-no use-index bol-no no-lock no-error.
      if avail oe-bolh then do:
          /*
        find first oe-relh where oe-relh.company = oe-bolh.company and
                   oe-relh.r-no = oe-bolh.r-no no-lock no-error.
        if avail oe-relh then
        find first shipto where shipto.company  = oe-bolh.company and
                   shipto.cust-no = oe-relh.cust-no and
                   shipto.ship-no = oe-bolh.ship-no no-lock no-error.
         */
        find first shipto where shipto.company eq cocode
                     and shipto.cust-no eq oe-bolh.cust-no
                     and shipto.ship-id eq oe-bolh.ship-id
                   use-index ship-id no-lock no-error.

        if avail shipto then
        assign  v-shipto-name = shipto.ship-name
                v-shipto-addr[1] = shipto.ship-addr[1]
                v-shipto-addr[2] = shipto.ship-addr[2]
                v-shipto-city = shipto.ship-city
                v-shipto-state = shipto.ship-state
                v-shipto-zip = shipto.ship-zip.

      end. /* avail oe-bolh */

      IF NOT v-reprint OR xinv-head.inv-no EQ 0 THEN
        RUN oe/get-inv#.p (ROWID(xinv-head)).

      DO TRANSACTION:
        FIND inv-head WHERE ROWID(inv-head) EQ ROWID(xinv-head).

        if inv-head.inv-date ne ? then v-inv-date = inv-head.inv-date.

        if inv-head.fob-code begins "ORIG" then
         assign v-fob = "Origin".
        else
         assign v-fob = "Destination".

        find FIRST carrier where carrier.company = inv-head.company and
          carrier.carrier = inv-head.carrier no-lock no-error.
         if avail carrier then
           assign v-shipvia = carrier.dscr.
         else
           assign v-shipvia = "".
        assign
          v-addr3 = inv-head.city + ", " + inv-head.state + "  " + inv-head.zip
          v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip
          v-line = 1
          v-printline = 0.
    
        find first stax
            {sys/ref/stax1W.i}
              and {sys/ref/taxgroup.i stax} eq inv-head.tax-gr
            no-lock no-error.
        if not avail stax then
        find first stax where stax.tax-group eq inv-head.tax-gr
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
        for each xinv-line no-lock where xinv-line.r-no = inv-head.r-no
          break by xinv-line.i-no:
         do i = 1 to 3:
          if xinv-line.sman[i] ne "" then do:
            create w-sman.
            assign w-sman.sman = xinv-line.sman[i].
          end.
         end.
         assign v-tot-qty = v-tot-qty + xinv-line.ship-qty
                v-t-weight = v-t-weight + (round(xinv-line.t-weight /
                            xinv-line.qty, 2) * xinv-line.inv-qty).
         v-tot-pallets = 0.
         v-pc = "C". /* complete*/
         FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = xinv-line.b-no 
             /*oe-bolh.ord-no = xinv-line.ord-no*/ :
           v-pc = "P". /* partial*/ 
           FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = oe-bolh.company AND
              oe-boll.b-no = oe-bolh.b-no AND
              oe-boll.i-no = xinv-line.i-no AND
              oe-boll.ord-no = xinv-line.ord-no:

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
         
         if last-of(xinv-line.i-no) then do:
           if xinv-line.est-no ne "" then
           do:
             find first eb where eb.company = xinv-line.company and
               eb.est-no = xinv-line.est-no and
               eb.e-num = xinv-line.e-num and
               eb.form-no = xinv-line.form-no and
               eb.blank-no = xinv-line.blank-no no-lock no-error.

             IF xinv-line.form-no = 0 AND xinv-line.est-type = 2 THEN
             DO:
               FOR EACH fg-set NO-LOCK WHERE fg-set.company = xinv-line.company
                  AND fg-set.set-no = xinv-line.i-no:
                 ASSIGN v-set-qty = v-set-qty + fg-set.part-qty.
               END.
               IF v-set-qty = 0 THEN
                  ASSIGN v-set-qty = 1.
               FOR EACH eb NO-LOCK WHERE eb.company = xinv-line.company AND
                  eb.est-no = xinv-line.est-no AND
                  eb.e-num = xinv-line.e-num AND
                  eb.form-no NE 0:
                 FIND fg-set WHERE fg-set.company = xinv-line.company AND
                    fg-set.set-no = xinv-line.i-no  AND
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
        end. /* each xinv-line */
    
                        /** Build Salesman Id String **/
                v-salesman = "" .
               
        for each w-sman break by w-sman.sman:
          if first-of(w-sman.sman) then
            assign v-salesman = v-salesman + w-sman.sman.
          delete w-sman.
        end.

        find first oe-bolh where oe-bolh.company = inv-head.company and
                                 oe-bolh.bol-no = inv-head.bol-no
                                 USE-INDEX bol-no no-lock no-error.
        if avail oe-bolh then
          assign v-rel-po-no = oe-bolh.po-no.

        find first inv-line where inv-line.r-no = inv-head.r-no
                                  no-lock no-error.
        if avail inv-line then
        do:
          assign v-price-head = inv-line.pr-uom.
          find first oe-ord where oe-ord.company = cocode and
                                  oe-ord.ord-no = inv-line.ord-no
                                  no-lock no-error.
          if avail oe-ord then
          do:
            assign v-po-no = oe-ord.po-no
                   v-bill-i = oe-ord.bill-i[1]
                   v-ord-no = oe-ord.ord-no
                   v-ord-date = oe-ord.ord-date.
            IF oe-ord.po-no <> "" THEN
                 v-po-ord = oe-ord.po-no .
          end.
          else
            assign v-price-head = inv-line.pr-uom.
        end.
        /* display heder info 
         view frame invhead-comp.  /* Print headers */
                */
 {oe/rep/invaccrd.i}  /* xprint form */
 v-printline = 28.

        v-subtot-lines = 0.
        v-t-tax = 0.
        for each inv-line no-lock where inv-line.r-no = inv-head.r-no:
          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = "".

          v-pc = "P". /* partial*/ 
          for each oe-boll no-lock where oe-boll.company = inv-line.company
                        and oe-boll.bol-no = inv-head.bol-no
                        /*and oe-boll.b-no = inv-line.b-no*/
                        and oe-boll.i-no = inv-line.i-no use-index bol-no:

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

          IF v-printline GE 56 THEN do:           
                PAGE.
                {oe/rep/invaccrd.i}
                v-printline = 28.
          END.
            
           ASSIGN
             lv-inv-list = "" .
            /*assign v-line = v-line + 1
                   v-printline = v-printline + 2.*/
            find first oe-ordl where oe-ordl.company = cocode and
                                     oe-ordl.ord-no = inv-line.ord-no and
                                     oe-ordl.i-no = inv-line.i-no
                                     no-lock no-error.
            if avail oe-ordl then DO:
                IF NOT CAN-FIND(FIRST oe-boll
                              WHERE oe-boll.company EQ inv-line.company
                                AND oe-boll.b-no    EQ inv-line.b-no
                                AND oe-boll.po-no   EQ inv-line.po-no
                                AND oe-boll.ord-no  EQ oe-ordl.ord-no
                                AND oe-boll.i-no    EQ oe-ordl.i-no
                                AND oe-boll.line    EQ oe-ordl.line
                                AND oe-boll.s-code  EQ "I"
                              USE-INDEX b-no) THEN
              FOR EACH ar-invl
                  WHERE ar-invl.company  EQ oe-ordl.company
                    AND ar-invl.ord-no   EQ oe-ordl.ord-no
                    AND ar-invl.i-no     EQ oe-ordl.i-no
                    AND CAN-FIND(FIRST oe-boll
                                 WHERE oe-boll.company EQ ar-invl.company
                                   AND oe-boll.b-no    EQ ar-invl.b-no
                                   AND oe-boll.po-no   EQ ar-invl.po-no
                                   AND oe-boll.ord-no  EQ oe-ordl.ord-no
                                   AND oe-boll.i-no    EQ oe-ordl.i-no
                                   AND oe-boll.line    EQ oe-ordl.line
                                   AND oe-boll.s-code  EQ "I"
                                 USE-INDEX b-no)
                    NO-LOCK
                    ,
                    FIRST ar-inv 
                        WHERE ar-inv.x-no eq ar-invl.x-no 
                          AND ar-inv.inv-date LT inv-head.inv-date
                    NO-LOCK:
                lv-inv-list = lv-inv-list + TRIM(STRING(ar-invl.inv-no,">>>>>>>>>>")) + " ".
              END.
            END.

            if avail oe-ordl then
              assign v-bo-qty = if (inv-line.qty - inv-line.ship-qty -
                                    oe-ordl.t-ship-qty) < 0 then 0 else
                                   (inv-line.qty - inv-line.ship-qty -
                                    oe-ordl.t-ship-qty).
            else
              assign v-bo-qty = if ( inv-line.qty - inv-line.ship-qty ) < 0
                                  then 0 else inv-line.qty - inv-line.ship-qty.

              IF lv-inv-list <> "" THEN
                  li-inv-list = "Previous Invoice(s): " .


            assign v-inv-qty = inv-line.qty
                   v-ship-qty = inv-line.ship-qty
                   v-i-no = inv-line.i-no
                   v-i-dscr = inv-line.i-name
                   v-price = inv-line.price * (1 - (inv-line.disc / 100))
                   v-t-price = inv-line.t-price
                   v-subtot-lines = v-subtot-lines + inv-line.t-price 
                   v-i-dscr-2 = inv-line.part-dscr1 .

                   FIND FIRST itemfg WHERE itemfg.company = cocode
                                        AND itemfg.i-no = inv-line.i-no NO-LOCK NO-ERROR.
                    IF AVAIL itemfg AND v-i-dscr-2 = "" THEN
                        ASSIGN v-i-dscr-2 = itemfg.part-dscr1 .

                if inv-line.tax and avail stax then
                do i = 1 to 3:
                  if stax.tax-code[i] ne "" then do:
                    create w-tax.
                    assign
                     w-dsc      = stax.tax-dscr[i]
                     w-tax      = round((if stax.company eq "yes" then v-t-price
                                                                  else inv-line.t-price) *
                                        stax.tax-rate[i] / 100,2)
                     v-t-price  = v-t-price + w-tax
                     v-t-tax[i] = v-t-tax[i] + w-tax
                     v-lines    = v-lines + 1.
                  end.
                end.

                if v-t-price ne inv-line.t-price then do:
                  create w-tax.
                  assign
                   w-dsc     = "******ITEM TOTAL:"
                   w-tax     = v-t-price
                   v-lines   = v-lines + 1.
                end.
            
            ASSIGN v-po-no  = inv-line.po-no
                   v-ord-no = inv-line.ord-no
                   v-price-head = inv-line.pr-uom.

            PUT
                v-po-no FORMAT "x(15)" SPACE(1)
                v-i-no  format "x(15)" SPACE(1)
                v-i-dscr  format "x(23)" SPACE(1)
                v-inv-qty FORMAT "->>>>>>9" SPACE(1)
                v-ship-qty  format "->>>>>>9" SPACE(1)
              /*  v-bo-qty  format "->>>>>9" SPACE(1)
                v-i-no  format "x(15)" SPACE(1)  space(13)   */                            
                v-price  format ">>>>9.9999" SPACE(1)               
                inv-line.t-price  format "->>>,>>9.99"                
                SKIP
                v-ord-no SPACE(10)
                inv-line.part-no SPACE(1)
                v-i-dscr-2 space(20)
                v-price-head SPACE(1) SKIP.
            
               IF lv-inv-list <> "" AND inv-line.part-dscr2 <> ""  THEN
                   PUT  li-inv-list FORMAT "x(20)" lv-inv-list FORMAT "x(12)" SPACE(1)
                        inv-line.part-dscr2 .
               ELSE IF lv-inv-list <> "" AND inv-line.part-dscr2 = "" THEN
                  PUT SPACE(16) li-inv-list FORMAT "x(20)" lv-inv-list FORMAT "x(15)" .
               ELSE
                   PUT SPACE(32) inv-line.part-dscr2 . 
                
                .
             v-printline = v-printline + 3.
             
            put skip(1).
            v-printline = v-printline + 1.
            
            
        end. /* each inv-line */
        

        for each inv-misc no-lock where inv-misc.company = inv-head.company and
          inv-misc.r-no = inv-head.r-no and
          inv-misc.bill = "Y" break by ord-no with frame detailm:

          IF v-printline GE 56 THEN do:
                PAGE.                
                {oe/rep/invaccrd.i}
                v-printline = 28.
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
            PUT inv-misc.po-no FORMAT "x(15)" SPACE(1)
            inv-misc.charge AT 17 inv-misc.dscr inv-misc.amt AT 85 SKIP.
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



        if v-prntinst then do:
            ASSIGN 
            cBillNotes[1] = ""
            cBillNotes[2] = ""
            cBillNotes[3] = ""
            cBillNotes[4] = "".
           
            IF inv-head.multi-invoice THEN
                FOR EACH bf-inv-head NO-LOCK
                   WHERE bf-inv-head.company EQ inv-head.company
                     AND bf-inv-head.cust-no EQ inv-head.cust-no
                     AND bf-inv-head.bol-no EQ xinv-head.bol-no
                     AND NOT bf-inv-head.multi-invoice
                  BREAK BY bf-inv-head.inv-date DESC:
                  ASSIGN 
                    cBillNotes[1] = bf-inv-head.bill-i[1]
                    cBillNotes[2] = bf-inv-head.bill-i[2]
                    cBillNotes[3] = bf-inv-head.bill-i[3]
                    cBillNotes[4] = bf-inv-head.bill-i[4]
                      .
                  LEAVE.
                END.
                ELSE 
                    ASSIGN
                        cBillNotes[1] = inv-head.bill-i[1]
                        cBillNotes[2] = inv-head.bill-i[2]
                        cBillNotes[3] = inv-head.bill-i[3]
                        cBillNotes[4] = inv-head.bill-i[4]  .

         do i = 1 to 4:
          if cBillNotes[i] ne "" then do:
            put cBillNotes[i] at 10 skip.
            assign v-printline = v-printline + 1.
          end.
         end. /* 1 to 4 */
        end.

        /* T O T A L S */
     /*   put skip(30 - v-printline). */
   /*
        find first cust where cust.company = cocode and
                   cust.cust-no = inv-head.cust-no no-lock no-error.
        if avail cust and cust.sort = "Y" AND inv-head.t-inv-tax <> 0 then
          put "Total Tax: " inv-head.t-inv-tax SKIP.

        else PUT " " skip(1) .

        if inv-head.f-bill then
           put disp-frt
               inv-head.t-inv-freight
               inv-head.t-inv-rev SKIP .
        else
          put "" SKIP.
              
  */
        v-frt-tax = inv-head.t-inv-freight.
        IF inv-head.tax-gr <> "" and
           inv-head.f-bill AND inv-head.t-inv-freight <> 0 AND AVAIL stax THEN
        do i = 1 to 3:

           if stax.tax-code[i] ne "" then do:
                create w-tax.
                assign
                 w-dsc      = stax.tax-dscr[i]
                 w-tax      = round((if stax.company eq "yes" then v-frt-tax
                                                         ELSE inv-head.t-inv-freight) *
                                        stax.tax-rate[i] / 100,2)                 
                 v-frt-tax  = v-frt-tax + w-tax
                 v-t-tax[i] = v-t-tax[i] + w-tax
                 v-lines    = v-lines + 1.
           END.
        end.      

        assign
         inv-head.printed = yes
         inv-head.stat = "X".
      end. /* DO TRANSACTION avail inv-head */

    do i = 1 to 3:
       v-bot-lab[i] = if v-t-tax[i] ne 0 then
                    /*  ((if avail stax then string(stax.tax-dscr[i],"x(5)")
                        else fill(" ",5))*/ 
                        ((IF AVAIL stax THEN string(CAPS(stax.tax-code[i]),"x(5)") 
                           ELSE FILL(" ",5) ) +
                       fill(" ",6) + ":" +
                       string(v-t-tax[i],"->>>>>9.99")) else "".
    end.
    v-inv-freight = if inv-head.f-bill THEN inv-head.t-inv-freight ELSE 0.
                    /*inv-head.t-inv-freight*/.

    PUT "<R58><C58><#8><FROM><R+5><C+22><RECT> " 
        "<=8> Sub Total    :" v-subtot-lines FORM "->>,>>9.99"
        "<=8><R+1> Freight      :" v-inv-freight
        /*"<=8><R+2> ""  " v-bot-lab[1]  */
        "<=8><R+2> Sales Tax    :" inv-head.t-inv-tax FORM "->>,>>9.99"
                  /*  v-bot-lab[2] */
        "<=8><R+3>" "" 
        "<=8><R+4> Total Invoice:" inv-head.t-inv-rev FORM "->>,>>9.99" .

    PUT "<FArial><R58><C1><P12><B> THANK YOU. </B> <P9> " SKIP
   /*      "  Your business is greatly appreciated! Thank You!"SKIP
         "  Please pay by invoice - no statements are issued." SKIP
         "  24% per annum interest charge on overdue accounts. " SKIP
         "  Any credit card purchases are subject to a charge of 2%." SKIP
         "  GST # R129463212.  Check out our website at www.pacificpackaging.ca" SKIP.
    */                                                        
        .
    v-printline = v-printline + 6.

   
    IF v-printline <= 66 THEN page. /*PUT SKIP(74 - v-printline). */
    
 
    end. /* each xinv-head */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */

