/* ---------------------------------------------- oe/rep/invhughs.p */
/* PRINT INVOICE   Xprint form for Hughes             */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/invoice.i}

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
def shared var v-fr-tax as logical initial no NO-UNDO.
def var v-tax-rate as dec format "->>>.99" NO-UNDO.
def var v-tax-code like stax.tax-code NO-UNDO.
def var v-tx-rate like stax.tax-rate NO-UNDO.
def var v-ans as logical initial no NO-UNDO.
def var v-date-ship as date initial today NO-UNDO.
def var v-del-no as int format ">>>>>>" NO-UNDO.
def var v-bol-cases LIKE oe-boll.cases NO-UNDO.
def var v-set-qty AS DECIMAL NO-UNDO.
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
DEF VAR v-date-shipa AS CHAR NO-UNDO INIT "".
DEFINE VARIABLE v-misc-ln AS CHAR NO-UNDO INIT "".
DEFINE VARIABLE v-item-ln LIKE oe-ordl.e-num.

def buffer xinv-head for inv-head .
def buffer xinv-line for inv-line .
DEF BUFFER xitemfg FOR itemfg.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF VAR v-part-dscr AS cha NO-UNDO.

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
DEF VAR v-ordered-qty AS INT FORM ">>>>9" NO-UNDO.

FIND FIRST inv-head NO-LOCK NO-ERROR.

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-show-parts AS LOG NO-UNDO.

    find first company where company.company = cocode no-lock no-error.
    find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

    for each report where report.term-id eq v-term-id no-lock,
        first xinv-head where recid(xinv-head) eq report.rec-id no-lock
        break by report.key-01
              by report.key-02:

      FIND FIRST cust WHERE cust.company = xinv-head.company
                        AND cust.cust-no = xinv-head.cust-no NO-LOCK NO-ERROR.

      if avail cust and cust.show-set then
         v-show-parts = YES.
      ELSE
         v-show-parts = NO.

      assign  v-shipto-name = xinv-head.sold-name
              v-shipto-addr[1] = xinv-head.sold-addr[1]
              v-shipto-addr[2] = xinv-head.sold-addr[2]
              v-shipto-city = xinv-head.sold-city
              v-shipto-state = xinv-head.sold-state
              v-shipto-zip = xinv-head.sold-zip
              v-del-no = 0.

      IF xinv-head.bol-no LE 0 THEN 
         ASSIGN v-date-ship = ?
                v-date-shipa = "N/A".

      find first oe-bolh where oe-bolh.company = xinv-head.company and
          oe-bolh.bol-no = xinv-head.bol-no use-index bol-no no-lock no-error.
      if avail oe-bolh then do:
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
        FOR EACH xinv-line where xinv-line.r-no = inv-head.r-no NO-LOCK,
           EACH oe-bolh WHERE oe-bolh.b-no = xinv-line.b-no
                        NO-LOCK
                        BREAK BY oe-bolh.bol-no:
          IF LAST-OF(oe-bolh.bol-no) THEN DO:
              v-tot-pallets = v-tot-pallets + oe-bolh.tot-pallets.
          END.
        END.

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
        /* v-tot-pallets = 0. */
         FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = xinv-line.b-no:
           FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = oe-bolh.company AND
              oe-boll.b-no = oe-bolh.b-no AND
              oe-boll.i-no = xinv-line.i-no AND
              oe-boll.ord-no = xinv-line.ord-no:

                                      /** Bill Of Lading TOTAL CASES **/
              ASSIGN v-bol-cases = v-bol-cases + oe-boll.cases.
/*                     v-tot-pallets = v-tot-pallets + oe-boll.cases +
                                     (if oe-boll.partial gt 0 then 1 else 0). */
           END. /* each oe-boll */
           IF oe-bolh.bol-no LE 0 THEN 
              ASSIGN v-date-ship = ?
                     v-date-shipa = "N/A".
           ELSE
              ASSIGN v-date-ship = oe-bolh.bol-date
                     v-date-shipa = STRING(v-date-ship,"99/99/9999").

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
                 ASSIGN v-set-qty = v-set-qty + fg-set.QtyPerSet.
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

                 IF AVAIL fg-set AND fg-set.QtyPerSet NE 0 THEN
                   ASSIGN v-part-qty = fg-set.QtyPerSet / v-set-qty.
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
          assign v-price-head = inv-line.pr-uom
                 v-rel-po-no = inv-line.po-no.
          find first oe-ord where oe-ord.company = cocode and
                                  oe-ord.ord-no = inv-line.ord-no
                                  no-lock no-error.
          if avail oe-ord then
          do:
            assign v-po-no = oe-ord.po-no
                   v-bill-i = oe-ord.bill-i[1]
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
        if avail inv-line then v-rel-po-no = inv-line.po-no.

        {oe/rep/invhugh1.i}
     
        ASSIGN
           v-subtot-lines = 0
           v-t-tax = 0.
        for each inv-line no-lock where inv-line.r-no = inv-head.r-no:

          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = ""
                 v-price-head = inv-line.pr-uom.

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
            
          v-beeler-lines = 0.
            do v = 1 to 2:
              v-part-info = if v eq 1 then inv-line.part-dscr1
                            else           inv-line.part-dscr2.

              if v-part-info ne "" then v-beeler-lines = v-beeler-lines + 1.
            end.
            v-printline = v-printline + v-beeler-lines.
            IF v-printline > 54 THEN DO:
               PAGE.
               v-printline = 1.
               {oe/rep/invhugh1.i}
            END.

            find first oe-ordl where oe-ordl.company = cocode and
                                     oe-ordl.ord-no = inv-line.ord-no and
                                     oe-ordl.i-no = inv-line.i-no
                                     no-lock no-error.
            if avail oe-ordl then
              assign v-bo-qty = if (inv-line.qty - inv-line.ship-qty -
                                    oe-ordl.t-ship-qty) < 0 then 0 else
                                   (inv-line.qty - inv-line.ship-qty -
                                    oe-ordl.t-ship-qty)
                     v-ordered-qty = oe-ordl.qty
                     v-item-ln = oe-ordl.e-num .
            else
              assign v-bo-qty = if ( inv-line.qty - inv-line.ship-qty ) < 0
                                  then 0 else inv-line.qty - inv-line.ship-qty
                     v-ordered-qty = inv-line.qty 
                     v-item-ln = inv-line.e-num.

            assign v-ordered-qty = inv-line.qty
                   v-inv-qty = inv-line.inv-qty
                   v-ship-qty = inv-line.ship-qty
                   v-i-no = IF inv-line.part-no NE "" THEN inv-line.part-no
                                                      ELSE inv-line.i-no
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
            
            IF NOT (NOT v-show-parts AND AVAIL oe-ordl AND
                    oe-ordl.is-a-component) THEN
            DO:
               PUT space(1) v-ordered-qty format "->>>>>9" SPACE(1)
                   v-ship-qty  format "->>>>>9" SPACE(1)
                   v-inv-qty FORM "->>>>>9" SPACE(1)
                   v-i-no  format "x(15)" SPACE(1)
                   v-i-dscr  format "x(21)" SPACE(1)
                   v-item-ln FORM ">>9" SPACE(1)
                   v-price  format ">>>>9.9999" SPACE(1)
                   v-price-head SPACE(1)
                   inv-line.t-price  format "->>>,>>9.99"  
                    SKIP.
                v-printline = v-printline + 1.
              
               do v = 1 to 2:
                 v-part-info = if v eq 1 then inv-line.part-dscr1
                               else           inv-line.part-dscr2.
              
                 if v-part-info ne "" then do:
                   put space(40) v-part-info skip.
                   v-printline = v-printline + 1.
                 end.
               end.
               put skip(1).
               v-printline = v-printline + 2.

            END.

            IF v-printline > 55 THEN DO:
               PAGE.
               v-printline = 1.
               {oe/rep/invhugh1.i}
            END.
            /* display components of set */

            IF v-show-parts THEN
            DO:
               FIND first itemfg where itemfg.company eq inv-line.company
                                   and itemfg.i-no    eq inv-line.i-no NO-LOCK NO-ERROR.

               if AVAIL itemfg AND itemfg.isaset AND
                  (NOT AVAIL oe-ordl OR
                   NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})) THEN
               for each fg-set where fg-set.company eq cocode
	                          and fg-set.set-no  eq itemfg.i-no   no-lock:
              
                   find first xitemfg where xitemfg.company eq cocode
	                                    and xitemfg.i-no    eq fg-set.part-no no-lock no-error.
                     v-part-dscr = string(fg-set.part-no,"x(16)") +
		                           (if avail xitemfg then xitemfg.i-name else "").
                     {sys/inc/part-qty.i v-part-qty fg-set}
              
                    PUT v-part-dscr            at 26 format "x(46)"
	                    v-inv-qty * v-part-qty to 79 format "->>>>>>"                     
	                    skip.
                    v-printline = v-printline + 1.
              
                    IF v-printline > 44 THEN DO:
                      PAGE.
                      v-printline = 1.
                      {oe/rep/invhugh1.i}
                      PUT SKIP.
                    END.
               end.
            END.
        end. /* each inv-line */

        for each inv-misc no-lock where inv-misc.company = inv-head.company and
          inv-misc.r-no = inv-head.r-no and
          inv-misc.bill = "Y" break by ord-no BY LINE with frame detailm:
          if first(inv-misc.ord-no) then
          do:
          /*  put "** Miscellaneous Items **" at 23 skip(1).*/
            assign v-printline = v-printline + 2.
          end.
           FIND FIRST oe-ordm 
             WHERE oe-ordm.company = inv-misc.company
             AND oe-ordm.ord-no = inv-misc.ord-no
             AND oe-ordm.LINE = inv-misc.LINE
             AND oe-ordm.charge = inv-misc.charge
            NO-LOCK NO-ERROR.
          IF AVAIL oe-ordm THEN
              v-misc-ln = string(oe-ordm.spare-int-1).
          ELSE
              v-misc-ln = " ".
           /* task 07221402 */
          PUT "<C1>" "      0       0"  "<C34>" inv-misc.charge "<C56>" inv-misc.amt "<C71>" inv-misc.amt SKIP
              "<C34>" inv-misc.dscr  v-misc-ln  SKIP
            /* gdm - 08050903 */
             "<C34>" inv-misc.po-no SKIP  .

          v-subtot-lines = v-subtot-lines + inv-misc.amt.
          v-printline = v-printline + 2.
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
         do i = 1 to 4:
          if inv-head.bill-i[i] ne "" then do:
       
            put inv-head.bill-i[i] at 10 skip.
            assign v-printline = v-printline + 1.
          end.
         end. /* 1 to 4 */
        end.
        
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
                    
                       ((IF AVAIL stax THEN string(CAPS(stax.tax-code[i]),"x(5)") 
                          ELSE FILL(" ",5) ) +
                      fill(" ",6) + ":" +
                      string(v-t-tax[i],"->>>>>9.99")) else "".
    end.
    v-inv-freight = if inv-head.f-bill THEN inv-head.t-inv-freight ELSE 0.

    PUT "<R53><C60><#8><FROM><R+5><C+20><RECT> " 
        "<=8> Sub Total  :" v-subtot-lines FORM "->>,>>9.99"
        "<=8><R+1> Freight    :" v-inv-freight
        "<=8><R+2> " v-bot-lab[1] 
        "<=8><R+3> " v-bot-lab[2]
        "<=8><R+4> Grand Total:" inv-head.t-inv-rev FORM "->>,>>9.99" .

    PUT "<FArial><R56><C1><P12><B> Comments </B> <P9> " SKIP         
        "  Our GST # R102421401." SKIP
        "  2% Interest charged monthly" SKIP
        "  on overdue items (24% p/a)" SKIP. 
    v-printline = v-printline + 6.
    IF v-printline < 50 THEN PUT SKIP(60 - v-printline).
 
    end. /* each xinv-head */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
