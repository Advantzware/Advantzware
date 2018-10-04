/* ------------------------------------------ ar/rep/invcapcin.p 04270902 GDM */
/* INVOICE PRINT  Program for N-K-1-INVPRINT = CapCityIN                        */
/* ------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{ar/rep/invoice.i}
{custom/notesdef.i}

{custom/formtext.i NEW}

DEF TEMP-TABLE tt-ar-invl NO-UNDO LIKE ar-invl 
    FIELD enum  LIKE oe-ordl.e-num .

DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.
def var v-salesman as char format "x(14)" NO-UNDO EXTENT 2.
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
def var v-net like inv-head.t-inv-rev NO-UNDO.
def var v-case-cnt as char format "x(80)" extent 5 NO-UNDO.
def var v-case-line as char NO-UNDO.
def var v-part-line as char NO-UNDO.
DEF VAR v-max-line AS INT INIT 69.
DEF VAR lv-bol-no LIKE oe-bolh.bol-no NO-UNDO.

DEF BUFFER xar-inv FOR ar-inv.
DEF BUFFER xar-invl FOR ar-invl.

def TEMP-TABLE w-sman NO-UNDO
  field sman as char format "x(4)".

def var v-ord-del-hdr as char format "x(3)" init "Del".
def var v-beeler-lines as int.
def var v-part-info as char format "x(30)".
def var v as int.
def var v-bo-qty as int format "99999" no-undo.
def var v-inv-qty as dec no-undo.
DEF VAR v-item-count AS INT NO-UNDO.
def var v-ship-qty as int format "99999" no-undo.
def var v-i-no as char format "x(15)" no-undo.
def var v-i-dscr as char format "x(18)" no-undo.
def var v-price as dec format ">>>>9.9999" no-undo.
def var v-t-price as dec format ">>>>>>9.99" no-undo.
def var v-po-no like ar-invl.po-no no-undo.
DEF VAR v-ordqty LIKE oe-ordl.qty NO-UNDO.
DEF VAR v-ord-ln LIKE oe-ordl.e-num format ">>9" NO-UNDO.
def var v-ship-i as char format "x(25)" no-undo.
def var v-rel-po-no like oe-rel.po-no no-undo.
DEF VAR v-subtot-lines AS DEC NO-UNDO.
def TEMP-TABLE w-tax NO-UNDO
    field w-dsc as   char
    field w-tax as   dec.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
def var v-bot-prc    AS DEC extent 3 NO-UNDO.
DEF VAR v-lines AS INT NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-frt-tax AS DEC NO-UNDO.
DEF VAR lv-inv-list AS CHAR NO-UNDO.
DEF VAR v-int AS DEC NO-UNDO.
DEF VAR v-tot-tax AS DEC NO-UNDO.

FIND FIRST ar-inv NO-LOCK NO-ERROR.
DEF VAR ls-image1    AS CHAR NO-UNDO.
DEF VAR ls-full-img1 AS CHAR FORMAT "x(100)" NO-UNDO.

ASSIGN 
    ls-image1 = "images\capcity.jpg"
    FILE-INFO:FILE-NAME = ls-image1
    ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">" .

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.

DEF VAR v-blt-addr1 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-blt-addr2 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-blt-addr3 AS CHAR FORMAT "x(30)" NO-UNDO.

DEF VAR v-sh-addr1 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-sh-addr2 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-sh-addr3 AS CHAR FORMAT "x(30)" NO-UNDO.

DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-email AS cha FORM "x(48)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR v-page-num AS INT NO-UNDO.
DEF VAR v-p-c AS CHAR NO-UNDO.
DEF VAR v-addr3-printed AS LOG INIT NO.
DEF VAR v-sold-addr3-printed AS LOG INIT NO.

DEF VAR v-icnt AS INT NO-UNDO.

DEF VAR v-text          AS CHAR                         NO-UNDO.
DEF VAR v-licnt         AS INT                          NO-UNDO.
DEF VAR v-notes         AS CHAR FORMAT "x(80)" EXTENT 5 NO-UNDO.
DEF VAR note-count      AS INT                          NO-UNDO. 
DEF VAR v-text1         AS CHAR FORMAT "x(170)" EXTENT 10 NO-UNDO.


FOR EACH tt-ar-invl NO-LOCK:
   DELETE tt-ar-invl.
END.

DEF BUFFER bf-cust FOR cust.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "INVPRINT" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".
   
    find first company where company.company = cocode no-lock no-error.

     ASSIGN v-comp-add1 = ""
        v-comp-add2 = "" 
        v-comp-add3 = ""
        v-comp-add4 = ""
        v-comp-add5 = "".

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
           lv-comp-name = cust.NAME.
 END.
    
    find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

    for each report where report.term-id eq v-term-id no-lock,
        first ar-inv where recid(ar-inv) eq report.rec-id no-lock,
        FIRST cust WHERE cust.company = ar-inv.company
                     AND cust.cust-no = ar-inv.cust-no NO-LOCK 

        break by (IF v-print-fmt EQ "ASIXprnt" THEN "" ELSE ar-inv.cust-no)
              by ar-inv.inv-no:
     
        find first carrier where carrier.company eq cocode
             and carrier.carrier eq ar-inv.carrier no-lock no-error.
        if avail carrier THEN ASSIGN v-shipvia = carrier.dscr.
        else assign v-shipvia = "".

        find first shipto where shipto.company eq cocode
             and shipto.cust-no eq ar-inv.cust-no
             and shipto.ship-id eq ar-inv.ship-id no-lock no-error.

        IF AVAIL shipto THEN 
           assign  v-shipto-name = shipto.ship-name
                   v-shipto-addr[1] = shipto.ship-addr[1]
                   v-shipto-addr[2] = shipto.ship-addr[2]
                   v-shipto-city = shipto.ship-city
                   v-shipto-state = shipto.ship-state
                   v-shipto-zip = shipto.ship-zip
                   v-addr3 = ar-inv.city + ", " + ar-inv.state + "  " + ar-inv.zip.

        ASSIGN
         v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip
         v-addr3      = IF TRIM(v-addr3) EQ ","   
                         THEN ""
                         ELSE TRIM(v-addr3)
         v-sold-addr3 = IF TRIM(v-sold-addr3) EQ ","  
                         THEN ""
                         ELSE TRIM(v-sold-addr3).

        if ar-inv.fob-code begins "ORIG" then
         assign v-fob = "Origin".
        else
         assign v-fob = "Destination".

        assign
          v-line = 1
          v-printline = 0.
    
        find first stax
            {sys/ref/stax1W.i}
              and {sys/ref/taxgroup.i stax} eq ar-inv.tax-code
            no-lock no-error.
        if not avail stax then
           find first stax where stax.company = ar-inv.company AND stax.tax-group eq ar-inv.tax-code
               no-lock no-error.
        if not avail stax then
           find first stax where stax.tax-group eq ar-inv.tax-code no-lock no-error.

        if avail stax then
          assign v-tax-rate = stax.tax-rate[1] +
                              stax.tax-rate[2] + stax.tax-rate[3]
                 v-tax-code[1] = stax.tax-code[1]
                 v-tax-code[2] = stax.tax-code[2]
                 v-tax-code[3] = stax.tax-code[3]
                 v-tx-rate[1]  = stax.tax-rate[1]
                 v-tx-rate[2]  = stax.tax-rate[2]
                 v-tx-rate[3]  = stax.tax-rate[3].

        assign v-tot-pallets = 0
               v-date-ship   = ar-inv.inv-date.

       /* gdm - 09230909 */
       EMPTY TEMP-TABLE w-sman.

       ASSIGN v-salesman = "".
       /* gdm - 09230909 */

       for each ar-invl NO-LOCK
           where ar-invl.x-no  eq ar-inv.x-no  
             and (ar-invl.misc eq no or ar-invl.billable)
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

         
        
         FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = ar-invl.b-no AND
             oe-bolh.ord-no = ar-invl.ord-no:
           FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = oe-bolh.company AND
              oe-boll.b-no = oe-bolh.b-no AND
              oe-boll.i-no = ar-invl.i-no:

                                      /** Bill Of Lading TOTAL CASES **/
              ASSIGN v-bol-cases = v-bol-cases + oe-boll.cases.
              RUN oe/pallcalc.p (ROWID(oe-boll), OUTPUT v-int).
              v-tot-pallets = v-tot-pallets + v-int.
           END. /* each oe-boll */
           assign v-date-ship = oe-bolh.bol-date.

         END. /* each oe-bolh */
         
         FIND FIRST oe-bolh WHERE oe-bolh.b-no = ar-invl.b-no NO-LOCK NO-ERROR.
         IF AVAIL oe-bolh THEN v-date-ship = oe-bolh.bol-date.

         if last-of(ar-invl.i-no) then do:
           if ar-invl.est-no ne "" then
           do:
             find first eb where eb.company = ar-invl.company and
               eb.est-no = ar-invl.est-no and
               eb.form-no = ar-invl.form-no and
               eb.blank-no = ar-invl.blank-no no-lock no-error.

             IF ar-invl.form-no = 0 AND ar-invl.est-type = 2 THEN
             DO:
               FOR EACH fg-set NO-LOCK WHERE fg-set.company = ar-invl.company
                                       AND fg-set.set-no = ar-invl.i-no:
                    ASSIGN v-set-qty = v-set-qty + fg-set.part-qty.
               END.
               IF v-set-qty = 0 THEN ASSIGN v-set-qty = 1.
               FOR EACH eb NO-LOCK WHERE eb.company = ar-invl.company AND
                              eb.est-no = ar-invl.est-no AND
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
        ASSIGN i = 0 v-salesman = "".
        for each w-sman break by w-sman.sman:
         ASSIGN i = i + 1.
          IF STRING(w-sman.sman) NE "" 
             THEN ASSIGN v-salesman[i] = w-sman.sman.
          delete w-sman.
          IF i = 2 THEN LEAVE.
        end.
        ASSIGN i = 0
               lv-bol-no = 0
               v-inv-date = ar-inv.inv-date.
        
         find first ar-invl
             where ar-invl.x-no  eq ar-inv.x-no  
               and (ar-invl.misc eq no or ar-invl.billable)
            no-lock no-error.

        IF AVAIL ar-invl THEN
           lv-bol-no = ar-invl.bol-no.

        {ar/rep/invcapcin.i}

        ASSIGN
        v-subtot-lines = 0
        v-t-tax = 0.

       for each ar-invl no-lock
             where ar-invl.x-no  eq ar-inv.x-no  
               and (ar-invl.misc eq no or ar-invl.billable) 
              BY ar-invl.po-no BY ar-invl.misc  BY ar-invl.i-no:
            
          CREATE tt-ar-invl .
          buffer-copy ar-invl to tt-ar-invl .

          find first oe-ordl where oe-ordl.company = cocode and
                                     oe-ordl.ord-no = ar-invl.ord-no and
                                     oe-ordl.i-no = ar-invl.i-no
                                     no-lock no-error.

          IF AVAIL oe-ordl AND oe-ordl.e-num NE 0 THEN
             tt-ar-invl.enum = oe-ordl.e-num.
          ELSE
             tt-ar-invl.enum = 9999.
       END.

        for each tt-ar-invl no-lock
             where tt-ar-invl.x-no  eq ar-inv.x-no  
               and (tt-ar-invl.misc eq no or tt-ar-invl.billable) 
              BY tt-ar-invl.misc BY tt-ar-invl.po-no BY tt-ar-invl.enum BY tt-ar-invl.i-no:

          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = ""
                 v-line = v-line + 1
                 v-beeler-lines = 0
                 lv-inv-list = ""
                 v-ship-qty  = IF tt-ar-invl.ord-no EQ 0 THEN tt-ar-invl.qty
                               ELSE tt-ar-invl.ship-qty
                 v-p-c = "P".

           FOR EACH oe-boll FIELDS(p-c) WHERE
              oe-boll.company EQ tt-ar-invl.company AND
              oe-boll.bol-no eq tt-ar-invl.bol-no AND
              oe-boll.i-no eq tt-ar-invl.i-no AND
              oe-boll.ord-no EQ tt-ar-invl.ord-no
              no-lock:
        
              IF oe-boll.p-c THEN v-p-c = "C".
           END.

            find first oe-ordl where oe-ordl.company = cocode and
                                     oe-ordl.ord-no = tt-ar-invl.ord-no and
                                     oe-ordl.i-no = tt-ar-invl.i-no
                                     no-lock no-error.
            if avail oe-ordl THEN DO:
               assign 
                  v-ordqty = oe-ordl.qty
                  v-ord-ln = oe-ordl.e-num
                  v-bo-qty = if (tt-ar-invl.qty - v-ship-qty -
                                    oe-ordl.t-ship-qty) < 0 then 0 else
                                   (tt-ar-invl.qty - v-ship-qty -
                                    oe-ordl.t-ship-qty).

              IF NOT CAN-FIND(FIRST oe-boll
                              WHERE oe-boll.company EQ tt-ar-invl.company
                                AND oe-boll.b-no    EQ tt-ar-invl.b-no
                                AND oe-boll.po-no   EQ tt-ar-invl.po-no
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
                    
                  NO-LOCK:
                lv-inv-list = lv-inv-list + TRIM(STRING(xar-invl.inv-no,">>>>>>>>>>")) + " ".
              END.
            END.

            else
              assign v-bo-qty = if ( tt-ar-invl.qty - v-ship-qty ) < 0 THEN 0
                                else tt-ar-invl.qty - v-ship-qty.

            assign v-inv-qty = tt-ar-invl.inv-qty
                   v-i-no = tt-ar-invl.i-no
                   v-i-dscr = tt-ar-invl.i-name
                   v-price = tt-ar-invl.unit-pr * (1 - (tt-ar-invl.disc / 100))
                   v-t-price = tt-ar-invl.amt
                   v-subtot-lines = v-subtot-lines + tt-ar-invl.amt
                   v-po-no = IF tt-ar-invl.po-no <> "" THEN tt-ar-invl.po-no ELSE ar-inv.po-no.

                if tt-ar-invl.tax and avail stax then
                do i = 1 to 3:
                  if stax.tax-code[i] ne "" then do:
                    create w-tax.
                    assign
                     w-dsc      = stax.tax-dscr[i]
                     w-tax      = round((if stax.company eq "yes" then v-t-price
                                                                  else tt-ar-invl.amt) *
                                        stax.tax-rate[i] / 100,2)
                     v-t-price  = v-t-price + w-tax
                     v-t-tax[i] = v-t-tax[i] + w-tax
                     v-lines    = v-lines + 1.
                  end.
                end.

                if v-t-price ne tt-ar-invl.amt then do:
                  create w-tax.
                  assign
                   w-dsc     = "******ITEM TOTAL:"
                   w-tax     = v-t-price
                   v-lines   = v-lines + 1.
                end.

           /* Duplication of code below to determine # of lines in item
              print section of form.  If # lines will go beyond end of page
              then do next page */
           v-item-count = 0.

           /* handle first item detail line */
           v-item-count = v-item-count + 1.

           do v = 1 to 4:
              v-part-info = IF v EQ 1 THEN v-i-dscr
                            ELSE
                            if v eq 2 then (IF tt-ar-invl.part-dscr1 <> "" THEN tt-ar-invl.part-dscr1 ELSE tt-ar-invl.i-dscr)
                            else
                            if v eq 3 then tt-ar-invl.part-dscr2
                            else           trim(lv-inv-list).

              if v-part-info ne "" OR 
                 (v = 1 AND tt-ar-invl.part-no <> "") OR
                  /* gdm - 08260913 */
                  (v = 2 and v-po-no <> "")
                 THEN DO:
                  IF v = 1 THEN 
                    v-item-count = v-item-count + 1.
                  ELSE
                  IF v = 2 THEN
                    v-item-count = v-item-count + 1.
                  ELSE
                  IF v = 3 THEN
                    v-item-count = v-item-count + 1.
                  ELSE          
                    v-item-count = v-item-count + 1.
                 end.
            end.
            IF NOT v-print-dept THEN
               v-item-count = v-item-count + 1.


           IF v-print-dept AND AVAIL oe-ordl THEN
           DO:
              FOR EACH notes WHERE
                  notes.rec_key EQ oe-ordl.rec_key AND 
                  CAN-DO(v-depts,notes.note_code)
                  NO-LOCK
                  BY notes.note_code:


                  DO i = 1 TO v-tmp-lines:
                      v-item-count = v-item-count + 1.
                  END.
              END.
           END.  /* IF v-print-dept */
           
           IF v-item-count + v-printline > v-max-line THEN DO:
              PAGE.
              v-printline = 0.
              {ar/rep/invcapcin.i}
           END.

           /*********** End of advance calculation of # lf lines per item ****/


            IF v-ship-qty GT 999999 OR v-inv-qty GT 999999 THEN
            PUT 
                v-ordqty            FORMAT "->>>>,>>9"    SPACE(1)
                v-inv-qty           FORMAT "->>>>,>>9"    SPACE(1)  
                tt-ar-invl.bol-no   FORMAT ">>>>>>9"      SPACE(1)
                tt-ar-invl.part-no  FORMAT "x(15)"        SPACE(1) 
                tt-ar-invl.po-no    FORMAT "x(15)"        SPACE(8)
                v-ord-ln            FORMAT ">>9"          SPACE(1)

                "<C60.2>"v-price FORMAT "->>,>>9.99<<" SPACE(1)
                tt-ar-invl.pr-uom                         SPACE(1)
               "<C71>" 
                tt-ar-invl.amt      FORMAT "->>>,>>9.99"      
                SKIP.
            ELSE
               PUT 
                v-ordqty            FORMAT "->>>>,>>9"    SPACE(1)
                v-inv-qty           FORMAT "->>>>,>>9"    SPACE(1)  
                tt-ar-invl.bol-no   FORMAT ">>>>>>9"      SPACE(1)
                tt-ar-invl.part-no  FORMAT "x(15)"        SPACE(1) 
                tt-ar-invl.po-no    FORMAT "x(15)"        SPACE(8)
                v-ord-ln            FORMAT ">>9"          SPACE(1)

                "<C60.2>"v-price FORMAT "->>,>>9.99<<"  SPACE(1)
                tt-ar-invl.pr-uom                         SPACE(1)
               "<C71>" 
                tt-ar-invl.amt      FORMAT "->>>,>>9.99"      
                SKIP.

            v-printline = v-printline + 1.
            
            do v = 1 to 4:
              v-part-info = IF v EQ 1 THEN v-i-dscr
                            ELSE
                            if v eq 2 then (IF tt-ar-invl.part-dscr1 <> "" THEN tt-ar-invl.part-dscr1 ELSE tt-ar-invl.i-dscr)
                            else
                            if v eq 3 then tt-ar-invl.part-dscr2
                            else           trim(lv-inv-list).

              if v-part-info ne "" OR 
                 (v = 1 AND tt-ar-invl.part-no <> "") OR
                  /* gdm - 08260913 */
                  (v = 2 and v-po-no <> "")
                 THEN DO:
                  IF v = 1 THEN 
                    PUT  tt-ar-invl.ord-no  FORMAT "->>>>>>>9" SPACE(2)
                         v-ship-qty       FORMAT "->>>,>>9"     SPACE(9)
                         v-i-no           FORMAT "x(15)"        SPACE(1)
                         v-part-info      SKIP.
                 ELSE
                 IF v = 2 THEN
                    PUT SPACE(44)
                        v-part-info SKIP.
                 ELSE
                 IF v = 3 THEN
                    PUT SPACE(44)
                         "" SKIP.
                 ELSE          
                   PUT SPACE(20) "Previous Invoice(s): " v-part-info SKIP.
                 v-printline = v-printline + 1.
              end.
            end.
            
            IF NOT v-print-dept THEN PUT skip(1).
            
            v-printline = v-printline + 1.
           
            IF v-print-dept AND AVAIL oe-ordl THEN
            DO:
               FOR EACH notes WHERE
                   notes.rec_key EQ oe-ordl.rec_key AND 
                   CAN-DO(v-depts,notes.note_code)
                   NO-LOCK
                   BY notes.note_code:
                   
                   v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                   {SYS/INC/ROUNDUP.I v-tmp-lines}
                   
                   DO i = 1 TO v-tmp-lines:
                      IF v-printline > v-max-line THEN DO:
                         PAGE.
                         v-printline = 0.
                         {ar/rep/invcapcin.i}
                      END.
                   
                      PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.
                          v-printline = v-printline + 1.
                      ASSIGN v-printline = v-printline + 1.
                      
                   END.
               END.
            END.  /* IF v-print-dept */
            
            IF v-printline > v-max-line THEN DO:
               PAGE.
               v-printline = 0.
               {ar/rep/invcapcin.i}
            END.

        end. /* each ar-invl */

        IF v-print-dept THEN DO:
           
           IF CAN-FIND(FIRST notes
             WHERE notes.rec_key EQ ar-inv.rec_key) THEN DO:
           
            FOR EACH notes NO-LOCK 
              WHERE notes.rec_key = ar-inv.rec_key 
                 BY notes.note_code:

              EMPTY TEMP-TABLE tt-formtext.

              ASSIGN v-text = ""
                     v-text = v-text + " " + notes.note_text.

              DO v-licnt = 1 TO 5:

                 CREATE tt-formtext.
                 ASSIGN tt-line-no = v-licnt
                        tt-length  = 100. 
              END.

              RUN custom/formtext.p (v-text).

              ASSIGN  i = 0 v-notes = "" note-count = 0.

              FOR EACH tt-formtext:

                ASSIGN i = i + 1.

                IF i <= 5 THEN
                   ASSIGN v-notes[i] = tt-formtext.tt-text.

                 IF v-notes[i] <> "" THEN note-count = i.
              END.

              DO i = 1 TO note-count:

                IF v-notes[i] NE "" THEN DO:                  

                   IF v-printline > v-max-line THEN DO:
                      PAGE.
                       v-printline = 0.
                      {ar/rep/invcapcin.i}
                   END.  
                      
                   PUT v-notes[i] FORM "x(80)" SKIP.
                   ASSIGN v-printline = v-printline + 1.
                END.
              END.
            END. /* FOR EACH NOTES */
           END. /* FIND FIRST NOTES */
        END. /* v-print-dept */
        
        IF v-prntinst THEN DO:            
          IF v-print-dept = NO THEN
          DO:
              PUT SKIP(3).
              ASSIGN v-printline = v-printline + 3.
          END.
          ELSE DO:
             PUT SKIP(2).
             ASSIGN v-printline = v-printline + 2.
          END.
          
          DO i = 1 TO 4:
             IF ar-inv.bill-i[i] <> "" THEN DO:
                IF v-printline > v-max-line THEN DO:
                   PAGE.
                   v-printline = 0.
                   {ar/rep/invcapcin.i}
                END.
                PUT ar-inv.bill-i[i] SKIP.
                v-printline = v-printline + 1.
             END.
          END.
        END.
        
        IF v-printline > v-max-line THEN DO:
           PAGE.
           v-printline = 0.
           {ar/rep/invcapcin.i}
        END.

        v-frt-tax = ar-inv.freight.        
        IF ar-inv.tax-code <> "" and
           (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
           AND ar-inv.freight <> 0
           AND AVAIL stax THEN
        do i = 1 to 3:

           if stax.tax-code[i] ne "" AND stax.tax-frt[i] EQ YES then do:
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

    IF ar-inv.tax-amt EQ 0 THEN v-t-tax = 0.

    ELSE DO:
      v-tot-tax = 0.
      DO i = 1 TO 3:
        v-tot-tax = v-tot-tax + v-t-tax[i].
      END.
      IF v-tot-tax EQ 0 THEN
        ASSIGN
         v-t-tax    = 0
         v-t-tax[1] = ar-inv.tax-amt.

      ELSE DO:
        IF v-tot-tax NE ar-inv.tax-amt THEN
        DO i = 1 TO 3:
          v-t-tax[i] = ROUND(v-t-tax[i] * (ar-inv.tax-amt / v-tot-tax),2).
        END.
        v-tot-tax = 0.
        DO i = 1 TO 3:
          v-tot-tax = v-tot-tax + v-t-tax[i].
        END.
        IF v-tot-tax NE ar-inv.tax-amt THEN
          v-t-tax[1] = v-t-tax[1] + (ar-inv.tax-amt - v-tot-tax).
      END.
    END.
    
    PUT "<FArial><R57.7><C1><#5><FROM><R+5.5><C+35><RECT>"
         "<=5><R57.7><C3>Please Remit To:"
         "<=5><R59.7><C6>     Capitol City Container Corp.".
    PUT "<=5><R60.7><C6>     PO Box 68531" .
    PUT "<=5><R61.7><C6>     Indianapolis, Indiana  46268".
    
    DO i = 1 TO 3:
     ASSIGN
      v-bot-lab[i] = IF v-t-tax[i] ne 0 
                        THEN
                          IF AVAIL stax 
                            THEN STRING(CAPS(stax.tax-code[i]),"x(5)") 
                            ELSE ""
                        ELSE ""
      v-bot-prc[i] = v-t-tax[i].
    END.

    v-inv-freight = if (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
                    THEN ar-inv.freight ELSE 0.

    PUT "<FLucida Console><R57.5><C58><#8><FROM><R+5.5><C+22><RECT> " 
        "<=8><R+0.2> Sub Total" "<C69>:" v-subtot-lines     FORM "->>>,>>9.99"
        "<=8><R+1.2> Freight"   "<C69>:" v-inv-freight      FORM "->>>,>>9.99"
        "<=8><R+2.2> IN Sales Tax"   "<C69>:" ar-inv.tax-amt  FORM "->>>,>>9.99".

     IF TRIM(v-bot-lab[3]) NE ""
      THEN
       PUT "<=8><R+5> " TRIM(v-bot-lab[3]) "<C69>:" 
           v-bot-prc[3] FORM "->>>,>>9.99".
     PUT
        "<=8><R+4.5> Grand Total" "<C69>:"
         v-subtot-lines + v-t-tax[1] + v-t-tax[2] + v-t-tax[3] + v-inv-freight FORM "->>>,>>9.99" .
     PUT "<FArial>".
    
    ASSIGN
       v-printline = v-printline + 6
       v-page-num = PAGE-NUM.
    
    PAGE. 

    DO TRANSACTION:
       FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
       ASSIGN xar-inv.printed = yes.

    END. /* DO TRANSACTION avail ar-inv */ 
 
end. /* each report, ar-inv */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
