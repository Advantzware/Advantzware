/* ---------------------------------------------- oe/rep/invconsb.p  */
/* PRINT INVOICE   Xprint Form  for Consoldated Box                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/invoice.i}
{custom/notesdef.i}
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.

def var v-salesman as char format "x(14)" NO-UNDO.
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
def var v-due-date as date initial TODAY FORM "99/99/9999" NO-UNDO.
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

def buffer xinv-head for inv-head .
def buffer xinv-line for inv-line .

def workfile w-sman
  field sman as char format "x(4)".

def var v-ord-del-hdr as char format "x(3)" init "Del".
def var v-beeler-lines as int.
def var v-part-info as char format "x(30)".
def var v as int.
def var v-bo-qty as int format "99999" no-undo.
def var v-ship-qty as int format "99999" no-undo.
def var v-i-no as char format "x(15)" no-undo.
def var v-i-dscr as char format "x(18)" no-undo.
def var v-price as dec format ">>>>9.9999" no-undo.
def var v-t-price as dec format ">>>>>>9.99" no-undo.
def var v-po-no like inv-line.po-no no-undo.
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
DEF VAR lv-inv-list AS CHAR NO-UNDO.

FIND FIRST inv-head NO-LOCK NO-ERROR.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(50)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(50)" NO-UNDO.
ASSIGN ls-image1 = "images\consol.jpg"
       ls-image2 = "".

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
DEF VAR lv-email AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR v-page-num AS INT NO-UNDO.
DEF VAR v-tot-tax AS DEC NO-UNDO.
find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "INVPRINT" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company eq cocode NO-LOCK. 
/*if avail company then
assign
 v-sname     = company.name
 v-saddr [1] = company.addr [1]
 v-saddr [2] = company.addr [2]
 v-scity     = company.city
 v-sstate    = company.state
 v-szip      = company.zip
 v-comp-add1 = company.addr[1]
 v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
 v-comp-add3 = "Phone: 604.533.2545" 
 v-comp-add4 = "Fax  : 604.533.2633".
 .
 */
 ASSIGN v-comp-add1 = ""
        v-comp-add2 = "" 
        v-comp-add3 = ""
        v-comp-add4 = ""
        v-comp-add5 = ""
        .
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

/*    ASSIGN v-comp-add1 = company.addr[1]
           v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
           v-comp-add3 = "Phone: 604.533.2545" 
           v-comp-add4 = "Fax  : 604.533.2633".
*/
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

      v-del-no = 0.

      find first oe-bolh where oe-bolh.company = xinv-head.company and
          oe-bolh.bol-no = xinv-head.bol-no use-index bol-no no-lock no-error.
      if avail oe-bolh then do:
        /*find first oe-relh where oe-relh.company = oe-bolh.company and
                   oe-relh.r-no = oe-bolh.r-no no-lock no-error.
        if avail oe-relh then */
        find first shipto where shipto.company  = oe-bolh.company and
                   shipto.cust-no = oe-bolh.cust-no and
                   shipto.ship-id = oe-bolh.ship-id no-lock no-error.
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
    
        FIND FIRST stax WHERE stax.company = cocode AND
                              stax.tax-group = inv-head.tax-gr NO-LOCK NO-ERROR.
        /*find first stax
            {sys/ref/stax1W.i}
              and {sys/ref/taxgroup.i stax} eq inv-head.tax-gr
            no-lock no-error. */
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
           FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = xinv-line.b-no /*AND
               oe-bolh.ord-no = xinv-line.ord-no*/:
               FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = oe-bolh.company AND
                                          oe-boll.b-no = oe-bolh.b-no AND
                                          oe-boll.i-no = xinv-line.i-no AND
                                          oe-boll.ord-no = xinv-line.ord-no:

                                      /** Bill Of Lading TOTAL CASES **/
                    ASSIGN v-bol-cases = v-bol-cases + oe-boll.cases
                       v-tot-pallets = v-tot-pallets + oe-boll.cases +
                                       (if oe-boll.partial gt 0 then 1 else 0).
               END. /* each oe-boll */
               assign v-date-ship = oe-bolh.bol-date
                 /* v-tot-pallets = v-tot-pallets + v-bol-cases +
                                  (if oe-boll.partial gt 0 then 1 else 0) */.

           END. /* each oe-bolh */
           if last-of(xinv-line.i-no) then do:
              if xinv-line.est-no ne "" THEN do:
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
                 ELSE IF AVAIL eb THEN DO:
                    IF eb.cas-cnt = 0 THEN ASSIGN v-tot-cas = ROUND(v-t-weight / eb.cas-wt, 2).
                    ELSE ASSIGN v-tot-cas = ROUND(v-tot-qty / eb.cas-cnt, 2).
                    if v-bol-cases ne 0 THEN assign v-tot-cas = v-bol-cases.
                 END. /* do */
              end. /* est-no ne "" */
              ASSIGN v-t-weight = 0 
                     v-tot-cas = 0
                     v-tot-qty = 0.
           end. /* last-of i-no */
        end. /* each xinv-line */
        /** Build Salesman Id String **/
        v-salesman = "".
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
            assign v-bill-i = oe-ord.bill-i[1]
                   v-ord-no = oe-ord.ord-no
                   v-ord-date = oe-ord.ord-date.
          end.
          else
            assign v-price-head = inv-line.pr-uom.
        end.

        find first inv-line
            where inv-line.r-no  eq inv-head.r-no
              and inv-line.po-no ne ""
            no-lock no-error.
        v-po-no = if avail inv-line then inv-line.po-no else "".

        /* display heder info 
         view frame invhead-comp.  /* Print headers */
                */
        find first terms where terms.company = inv-head.company AND terms.t-code  = inv-head.terms
		      	   no-lock no-error.
        if available terms THEN assign v-due-date  = inv-head.inv-date + terms.net-days.
        ELSE v-due-date = v-inv-date.

        assign v-tot-pallets = 0
               v-t-weight = 0.
        for each xinv-line no-lock where xinv-line.r-no = inv-head.r-no :    
            v-t-weight = v-t-weight + (round(xinv-line.t-weight /
                         xinv-line.qty, 2) * xinv-line.inv-qty).
            FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = xinv-line.b-no /*AND
            oe-bolh.ord-no = xinv-line.ord-no*/:
               FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = oe-bolh.company AND
                                       oe-boll.b-no = oe-bolh.b-no AND
                                       oe-boll.i-no = xinv-line.i-no AND
                                       oe-boll.ord-no = xinv-line.ord-no:
                                   /** Bill Of Lading TOTAL CASES **/
                  ASSIGN v-tot-pallets = v-tot-pallets + oe-boll.cases +
                                    (if oe-boll.partial gt 0 then 1 else 0).
               END. /* each oe-boll */
            END.
        END.
        {oe/rep/invconsb.i}

        v-subtot-lines = 0.
        v-t-tax = 0.
        for each inv-line no-lock where inv-line.r-no = inv-head.r-no:
          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = "".

          IF v-printline > 45 THEN DO:
               PAGE.
               v-printline = 0.
               {oe/rep/invconsb.i}
          END.

          for each oe-boll
              where oe-boll.company eq inv-line.company
                and oe-boll.ord-no  eq inv-line.ord-no
                and oe-boll.b-no    eq inv-line.b-no
                and oe-boll.i-no    eq inv-line.i-no
                and oe-boll.line    eq inv-line.line
                and oe-boll.po-no   eq inv-line.po-no
              use-index bol-no no-lock:

                                       /** Build Case Count Display Lines **/
            if oe-boll.cases ne 0 and oe-boll.qty-case ne 0 then
            assign v-case-line = string(oe-boll.cases) + " @ " +
                                     string(oe-boll.qty-case).
            else assign v-case-line = "".
            if oe-boll.partial ne 0 then
            assign v-part-line = "1" + " @ " + string(oe-boll.partial).
            else assign v-part-line = "".

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

            assign v-line = v-line + 1
                   .

            lv-inv-list = "".

            find first oe-ordl where oe-ordl.company = cocode and
                                     oe-ordl.ord-no = inv-line.ord-no and
                                     oe-ordl.i-no = inv-line.i-no
                                     no-lock no-error.
            if avail oe-ordl then DO:
              v-bo-qty = if (inv-line.qty - inv-line.ship-qty -
                             oe-ordl.t-ship-qty) < 0 then 0 else
                             (inv-line.qty - inv-line.ship-qty -
                              oe-ordl.t-ship-qty).

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
                    
                  NO-LOCK:
                lv-inv-list = lv-inv-list + TRIM(STRING(ar-invl.inv-no,">>>>>>>>>>")) + " ".
              END.
            END.
            else
              assign v-bo-qty = if ( inv-line.qty - inv-line.ship-qty ) < 0
                                  then 0 else inv-line.qty - inv-line.ship-qty.

            v-beeler-lines = 0.
            do v = 1 to 3:
              v-part-info = if v eq 1 then inv-line.part-dscr1
                            else
                            if v eq 2 then inv-line.part-dscr2
                            else  ""/*         trim(lv-inv-list)*/ .

              if v-part-info ne "" OR (v = 1 AND inv-line.part-no <> "") then
                v-beeler-lines = v-beeler-lines + 1.
            end.
            v-printline = v-printline + v-beeler-lines.
/*
            if v-printline gt 29 then
            do:
              put skip(30 - v-printline) "* CONTINUED *" at 72.

              v-printline = 2 + v-beeler-lines.

              page.

            end.
*/
            assign v-ship-qty = inv-line.ship-qty
                   v-i-no = inv-line.i-no
                   v-i-dscr = inv-line.i-name
                   v-price = inv-line.price * (1 - (inv-line.disc / 100))
                   v-t-price = inv-line.t-price
                   v-subtot-lines = v-subtot-lines + inv-line.t-price.


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
            
            v-price-head = inv-line.pr-uom.

            PUT space(1) v-ship-qty format "->>>>>9" SPACE(2)
                /*v-ship-qty  format "->>>>>9" SPACE(1)
                v-bo-qty  format "->>>>>9" SPACE(1) 
                inv-line.ord-no FORMAT ">>>>>>9" SPACE(1) */
                v-i-no  format "x(15)" SPACE(3)
                v-i-dscr  format "x(30)" SPACE(7)
                v-price  format "->>>,>>9.99<<" SPACE(6)
                /*v-price-head SPACE(1)*/
                inv-line.t-price  format "->>>,>>9.99"                     
                SKIP.
             v-printline = v-printline + 1.

      /* not display tax for lines      
         for each w-tax :
                put fill(" ",15)  w-dsc 
                      w-tax  SKIP .             
                   
                delete w-tax.
            end.
            */
            do v = 1 to 2:
              v-part-info = if v eq 1 then inv-line.part-dscr1
                            else
                            if v eq 2 then inv-line.part-dscr2
                            else  ""/*         trim(lv-inv-list)*/.

              if v-part-info ne "" OR (v = 1 AND inv-line.part-no <> "") then do:
                /*put space(40) v-part-info skip.*/
                 IF v = 1 THEN PUT SPACE(28) /*inv-line.part-no SPACE*/ v-part-info SKIP.
                 ELSE
                 IF v = 2 THEN PUT SPACE(28) v-part-info SKIP.
                 /*ELSE          PUT SPACE(23) "Previous Invoice(s): " v-part-info SKIP.*/
                 v-printline = v-printline + 1.
              end.
            end.
            put skip(1).
            v-printline = v-printline + 1.
       /*
            IF v-printline > 50 THEN DO:
                MESSAGE 30 "," v-printline VIEW-AS ALERT-BOX.
               PAGE.
               v-printline = 0.
               {oe/rep/invconsb.i}
            END.
       */     
        end. /* each inv-line */

        for each inv-misc no-lock where inv-misc.company = inv-head.company and
          inv-misc.r-no = inv-head.r-no and
          inv-misc.bill = "Y" break by ord-no with frame detailm:
          if first(inv-misc.ord-no) then
          do:
            IF v-printline > 45 THEN DO:
               PAGE.
               v-printline = 0.
               {oe/rep/invconsb.i}
            END.
            put "** Miscellaneous Items **" at 23 skip(1).
            assign v-printline = v-printline + 2.
          end.
            
          put inv-misc.charge AT 10 inv-misc.dscr inv-misc.amt  SKIP.
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
/* no need to display tax for misc
            for each w-tax :
                put fill(" ",15)  w-dsc
                      w-tax                
                      SKIP.
                delete w-tax.
            end.


            assign v-line = v-line + 1
                   v-printline = v-printline + 2.
*/         IF v-printline > 53 THEN DO:
               PAGE.
               v-printline = 0.
               {oe/rep/invconsb.i}
           END.

        end. /* each inv-misc */

        if v-prntinst then do:
         {custom/notesprt.i inv-head v-inst 4}
         DO i = 1 TO 4:
              IF v-inst[i] <> "" THEN DO:                
                 IF v-printline > 45 THEN DO:
                    PAGE.
                    v-printline = 0.
                    {oe/rep/invconsb.i}
                 END.
                 PUT v-inst[i] SKIP.
                 v-printline = v-printline + 1.
              END.
           END.
        end.
        /*
        IF v-printline > 45 THEN DO:
                    PAGE.
                    v-printline = 0.
                    {oe/rep/invconsb.i}
        END.
        */
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

           if stax.tax-code[i] ne "" AND stax.tax-frt[i] EQ YES then do:
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
    v-tot-tax = 0.
    do i = 1 to 3:
       v-bot-lab[i] = if v-t-tax[i] ne 0 then
                    /*  ((if avail stax then string(stax.tax-dscr[i],"x(5)")
                        else fill(" ",5))*/ 
                        ((IF AVAIL stax THEN string(CAPS(stax.tax-code[i]),"x(5)") 
                           ELSE FILL(" ",5) ) +
                       fill(" ",6) + ":" +
                       string(v-t-tax[i],"->>>>>9.99")) else "".
       v-tot-tax = v-tot-tax + v-t-tax[i].
    end.
    v-inv-freight = if inv-head.f-bill THEN inv-head.t-inv-freight ELSE 0.
                    /*inv-head.t-inv-freight*/.

    PUT "<R49><C53><#8>" /*<FROM><R+5><C+20><RECT> " */
             "<=8>           Sub Total  " v-subtot-lines FORM "->>,>>9.99"
        "<=8><R+1>           Sales Tax  " v-tot-tax FORM "->>,>>9.99" /*freight:v-inv-freight*/
        /*"<=8><R+2> " v-bot-lab[1] 
        "<=8><R+3> " /*PST        :" inv-head.t-inv-tax FORM "->>,>>9.99"*/
                    v-bot-lab[2] */
        "<=8><R+2>Tatal Invoice Amount  " inv-head.t-inv-rev FORM "->>,>>9.99"
        "<=8><R+3>    Payment Received  "
        "<=8><R+4>          Grand Total " inv-head.t-inv-rev FORM "->>,>>9.99" 
        "<=8><C-43><FGCOLOR=RED>Please Remit To" 
        "<=8><R+1><C-43>P.O. Box 26323" 
        "<=8><R+2><C-43>Jacksonville, FL 32226<FGCOLOR=BLACK>"
        "<=8><R+4><C-43>Thank You" 
        "<=8><C-52><R+5><FROM><C+79><LINE><||3>".

    PUT "<LPI=8><R72.5><C1><P7><FArial>Buyer agrees with seller, MAS Enterprise of Fort Lauderdale, Inc. D/B/A Consolidated Box Manufacturing, 1314 Eastport Rd. Jacksonville, Florida 32218 to pay for"
        "<R73.5><C1>all purchase within 30 days. All charges billed but not paid at the end of 30 days will be considered delinquent and subject to finance charges. Finance charges are computed by a"
        "<R74.5><C1>periodic rate of 1.5% per month, witch is annual percentage rate of 18% applied to the “previous” unpaid balance less current credits. The minimum payment due will be payment"
        "<R75.5><C1>to buyer’s Indebtedness over 30 days. If not paid, Consolidated may declare the unpaid balance due and payable immediately. Buyer agrees to pay all collection expenses including"
        "<R76.5><C1>reasonable Attorneys fees and court cost, if it is necessary to collect through action."
        "<R77.5><C7>Buyer assumes full responsibility for all material purchased from Consolidated, and agrees to be personally liable of all charges promptly.  Buyer agrees to notify"
        "<R78.5><C1>Consolidated in writing within 10 days of receipt of invoice of any discrepancy in billing. Failure to notify Consolidated Box Manufacturing signifies total acceptance and"
        "<R79.5><C1>responsibility for payments according to terms. Consolidated reserves the right to send out “Notice to Owners” and files liens on past due accounts, and use any and all legal"
        "<R80.5><C1>means available to force collection, if such action becomes necessary."
        "<R82><C1>All damaged and/or defective items must be reported by Buyer to Consolidated Box Manufacturing in writing within (3) days of delivery of the items to Buyer. Buyer’s failure to"
        "<R83><C1>do so shall constitute Buyer’s absolute waiver of any claims for damaged or defective items included in such delivery." 
        "<R84.5><C7>Consolidated Box Manufacturing may cancel courtesy agreement at any time it deems it necessary.<P10>"
        "<LPI=6>".

    v-printline = v-printline + 6.
    v-page-num = PAGE-NUM.
    page. /*IF v-printline < 50 THEN PUT SKIP(60 - v-printline). */
 
    end. /* each xinv-head */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
