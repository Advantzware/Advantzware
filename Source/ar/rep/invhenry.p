/* ---------------------------------------------- ar/rep/invhenry.p   */
/* PRINT INVOICE   Henry form                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{ar/rep/invoice.i}
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
DEF VAR lv-bol-no LIKE oe-bolh.bol-no NO-UNDO.

DEF BUFFER xar-inv FOR ar-inv.
DEF BUFFER xar-invl FOR ar-invl.

def TEMP-TABLE w-sman
  field sman as char format "x(4)".

def var v-ord-del-hdr as char format "x(3)" init "Del".
def var v-beeler-lines as int.
def var v-part-info as char format "x(30)".
def var v as int.
def var v-bo-qty as int format "99999" no-undo.
def var v-inv-qty as dec no-undo.
def var v-ship-qty as int format "99999" no-undo.
DEFINE VARIABLE v-ord-qty as int format "99999" no-undo.
def var v-i-no as char format "x(15)" no-undo.
def var v-i-dscr as char format "x(18)" no-undo.
def var v-price as dec format ">>>>9.9999" no-undo.
def var v-t-price as dec format ">>>>>>9.99" no-undo.
def var v-po-no like ar-invl.po-no no-undo.
def var v-bill-i as char format "x(25)" no-undo.
def var v-ord-no like oe-ord.ord-no no-undo.
def var v-ord-date like oe-ord.ord-date no-undo.
def var v-ship-i as char format "x(25)" no-undo.
def var v-ord-po-no like oe-ord.po-no no-undo.
def var v-price-head as char format "x(5)" no-undo.
DEF VAR v-subtot-lines AS DEC NO-UNDO.
def TEMP-TABLE w-tax
    field w-dsc as   char
    field w-tax as   dec.
def var v-t-tax      as   dec extent 5 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 5 NO-UNDO.
DEF VAR v-lines AS INT NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-frt-tax AS DEC NO-UNDO.
DEF VAR lv-inv-list AS CHAR NO-UNDO.
DEF VAR v-int AS DEC NO-UNDO.
DEF VAR v-tot-tax AS DEC NO-UNDO.

FIND FIRST ar-inv NO-LOCK NO-ERROR.

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
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE ls-full-img1 AS CHAR FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE vRelPo like oe-rel.po-no no-undo.
DEFINE VARIABLE iPoCheck AS LOGICAL NO-UNDO.
DEFINE VARIABLE cPo-No AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
DEF VAR cStockNotes AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
DEFINE SHARED VARIABLE lPrintQtyAll  as LOGICAL no-undo .
DEF BUFFER bf-cust FOR cust .
DEFINE VARIABLE lValid         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound AND cRtnChar NE "" THEN DO:
    cRtnChar = DYNAMIC-FUNCTION (
                   "fFormatFilePath",
                   cRtnChar
                   ).
                   
    /* Validate the N-K-1 BusinessFormLogo image file */
    RUN FileSys_ValidateFile(
        INPUT  cRtnChar,
        OUTPUT lValid,
        OUTPUT cMessage
        ) NO-ERROR.

    IF NOT lValid THEN DO:
        MESSAGE "Unable to find image file '" + cRtnChar + "' in N-K-1 setting for BusinessFormLogo"
            VIEW-AS ALERT-BOX ERROR.
    END.
END.

ASSIGN ls-full-img1 = cRtnChar + ">" .

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
           lv-comp-name = cust.NAME   
           .
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
                   v-addr3 = ar-inv.city + ", " + ar-inv.state + "  " + ar-inv.zip
                   .
         v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip .


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
        cPo-No = "".
        
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

         IF ar-invl.po-no NE "" THEN DO:
                 cPo-No = cPo-No + ar-invl.po-no + ",". 
         END.
        
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
                    ASSIGN v-set-qty = v-set-qty + fg-set.qtyPerSet.
               END.
               IF v-set-qty = 0 THEN ASSIGN v-set-qty = 1.
               FOR EACH eb NO-LOCK WHERE eb.company = ar-invl.company AND
                              eb.est-no = ar-invl.est-no AND
                              eb.form-no NE 0:
                 FIND fg-set WHERE fg-set.company = ar-invl.company AND
                    fg-set.set-no = ar-invl.i-no  AND
                    fg-set.part-no = eb.stock-no NO-LOCK NO-ERROR.

                 IF AVAIL fg-set AND fg-set.qtyPerSet NE 0 THEN
                   ASSIGN v-part-qty = fg-set.qtyPerSet / v-set-qty.
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
        DO iCount = 1 TO NUM-ENTRIES(cPo-No):
           IF ENTRY(1,cPo-No) NE ENTRY(iCount,cPo-No) and ENTRY(iCount,cPo-No) ne ""  THEN iPoCheck = YES. 
       END.
    
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
               v-ord-date = ar-inv.ord-date
               v-ord-po-no = ""
               .
        
        find first ar-invl
             where ar-invl.x-no  eq ar-inv.x-no  
               and (ar-invl.misc eq no or ar-invl.billable)
            no-lock no-error.
        if avail ar-invl then
        do:
           assign v-price-head = ar-invl.pr-uom
                  v-po-no = IF ar-invl.po-no <> "" THEN ar-invl.po-no ELSE ar-inv.po-no
                  v-ord-no = ar-invl.ord-no
                  lv-bol-no = ar-invl.bol-no
                  v-ord-po-no = IF iPoCheck EQ YES THEN "See below" ELSE ar-invl.po-no.
         /*find first oe-ord where oe-ord.company = cocode and
                                  oe-ord.ord-no = ar-invl.ord-no
                                  no-lock no-error.   
          if avail oe-ord then
          do:
            ASSIGN v-ord-po-no = IF iPoCheck EQ YES THEN "See below" ELSE oe-ord.po-no.
          end.*/
        end.      

        /* display heder info 
         view frame invhead-comp.  /* Print headers */  */
        IF v-salesman = "" THEN v-salesman = cust.sman.
        v-inv-date = ar-inv.inv-date.
        
        {ar/rep/invhenry.i}

        ASSIGN
           v-subtot-lines = 0
           v-t-tax = 0.

        for each ar-invl no-lock
             where ar-invl.x-no  eq ar-inv.x-no  
               and (ar-invl.misc eq no or ar-invl.billable) 
              BY ar-invl.misc  BY ar-invl.i-no:
          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = ""
                 v-line = v-line + 1
                 v-beeler-lines = 0
                 lv-inv-list = ""
                 v-ship-qty  = IF ar-invl.ord-no EQ 0 THEN ar-invl.qty
                               ELSE ar-invl.ship-qty.

            find first oe-ordl where oe-ordl.company = cocode and
                                     oe-ordl.ord-no = ar-invl.ord-no and
                                     oe-ordl.i-no = ar-invl.i-no
                                     no-lock no-error.
            
            if avail oe-ordl THEN DO:
              assign v-bo-qty = if (ar-invl.qty - v-ship-qty -
                                    oe-ordl.t-ship-qty) < 0 then 0 else
                                   (ar-invl.qty - v-ship-qty -
                                    oe-ordl.t-ship-qty).
            /* ASSIGN vRelPo = "".
             FOR EACH oe-rel NO-LOCK
                WHERE oe-rel.company = cocode
                AND oe-rel.ord-no = oe-ordl.ord-no
                AND oe-rel.i-no = oe-ordl.i-no
                AND oe-rel.LINE = oe-ordl.LINE :

                IF oe-rel.po-no NE "" THEN DO:
                  vRelPo = oe-rel.po-no.  
                  LEAVE.
                END.
             END.*/

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
                    
                  NO-LOCK:
                lv-inv-list = lv-inv-list + TRIM(STRING(xar-invl.inv-no,">>>>>>>>>>")) + " ".
              END.
            END.

            else
              assign v-bo-qty = if ( ar-invl.qty - v-ship-qty ) < 0
                                  then 0 else ar-invl.qty - v-ship-qty.

            assign v-inv-qty = ar-invl.qty
                   v-i-no = ar-invl.i-no
                   v-i-dscr = ar-invl.i-name
                   v-price = ar-invl.unit-pr * (1 - (ar-invl.disc / 100))
                   v-t-price = ar-invl.amt
                   v-subtot-lines = v-subtot-lines + ar-invl.amt
                   v-ord-qty = ar-invl.qty .
                
              IF lPrintQtyAll THEN
                   v-inv-qty = ar-invl.inv-qty .

                if ar-invl.tax and avail stax then
                do i = 1 to 5:
                  if stax.tax-code1[i] ne "" then do:
                    create w-tax.
                    assign
                     w-dsc      = stax.tax-dscr1[i]
                     w-tax      = round((if stax.company eq "yes" then v-t-price
                                                                  else ar-invl.amt) *
                                        stax.tax-rate1[i] / 100,2)
                     v-t-price  = v-t-price + w-tax
                     v-t-tax[i] = v-t-tax[i] + w-tax
                     v-lines    = v-lines + 1.
                  end.
                end.

                if v-t-price ne ar-invl.amt then do:
                  create w-tax.
                  assign
                   w-dsc     = "******ITEM TOTAL:"
                   w-tax     = v-t-price
                   v-lines   = v-lines + 1.
                end.
            
          IF NOT lPrintQtyAll THEN do:  
            PUT space(1)        /*"->>>>9.9<"*/
                v-inv-qty format  "->>>>>>9" SPACE(1)
                v-ship-qty  format "->>>>>>9" SPACE(1)
                /*v-bo-qty  format "->>>>>9" SPACE(1) */
                ar-invl.ord-no FORM ">>>>>>9" SPACE(1)
                v-i-no  format "x(15)" SPACE(3)
                v-i-dscr  format "x(25)" SPACE(3)
                v-price  format "$->>>,>>9.99" /*"$->,>>9.99<<"*/ SPACE(1)
                v-price-head SPACE(1)
                ar-invl.amt  format "$->>>>,>>9.99" /*"$->>>,>>9.99" */               
                SKIP.
          END.
          ELSE DO:
              PUT space(1)        /*"->>>>9.9<"*/
                v-ord-qty format  "->>>>>>9" SPACE(1)
                v-inv-qty  format "->>>>>>9" SPACE(1)
                /*v-bo-qty  format "->>>>>9" SPACE(1) */
                ar-invl.ord-no FORM ">>>>>>9" SPACE(1)
                v-i-no  format "x(15)" SPACE(3)
                v-i-dscr  format "x(25)" SPACE(3)
                v-price  format "$->>>,>>9.99" /*"$->,>>9.99<<"*/ SPACE(1)
                v-price-head SPACE(1)
                ar-invl.amt  format "$->>>>,>>9.99" /*"$->,>>9.99<<"*/                
                SKIP.

          END.    /* else do */
             v-printline = v-printline + 1.
      
            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company = ar-invl.company 
                AND itemfg.i-no = ar-invl.i-no NO-ERROR.

            do v = 1 to 4:
              CASE v:
                WHEN 1 THEN ASSIGN v-part-info = IF ar-invl.part-dscr1 <> "" THEN ar-invl.part-dscr1 ELSE ar-invl.i-dscr.
                WHEN 2 THEN ASSIGN v-part-info = IF ar-invl.part-dscr2 NE "" THEN ar-invl.part-dscr2 ELSE
                                                 IF AVAIL itemfg THEN itemfg.part-dscr2 ELSE "".
                WHEN 3 THEN ASSIGN v-part-info = IF AVAIL itemfg THEN itemfg.part-dscr3 ELSE "".
                WHEN 4 THEN ASSIGN v-part-info = "".
              END CASE.

              if v-part-info ne "" OR (v = 1 AND ar-invl.part-no <> "") then do:
                 IF v = 1 THEN DO:

                     IF lPrintQtyAll THEN do:
                      PUT SPACE(1) v-ship-qty FORMAT "->>>>>>9" .

                         IF LENGTH(ar-invl.po-no) LE 8 THEN DO:
                             PUT SPACE(9) ar-invl.po-no FORMAT "x(8)" SPACE(1)   ar-invl.part-no SPACE(3) v-part-info SKIP.
                         END.
                         ELSE DO: 
                             PUT  SPACE(1) ar-invl.po-no FORMAT "x(15)" SPACE(2)   ar-invl.part-no SPACE(3) v-part-info SKIP.
                         END.
                     END. /* lPrintQtyAll*/
                     ELSE DO:
                         IF LENGTH(ar-invl.po-no) LE 8 THEN DO:
                             PUT  SPACE(18) ar-invl.po-no FORMAT "x(8)" SPACE(1)   ar-invl.part-no SPACE(3) v-part-info SKIP.
                         END.
                         ELSE DO: 
                             PUT SPACE(11) ar-invl.po-no FORMAT "x(15)" SPACE(1)   ar-invl.part-no SPACE(3) v-part-info SKIP.
                         END.

                     END.    /* else do */
                 END.
                 ELSE 
                 IF v = 2 THEN PUT SPACE(45) v-part-info SKIP.
                 ELSE          PUT SPACE(45) v-part-info SKIP.
                 v-printline = v-printline + 1.
              end.
            end.
            put skip(1).
            v-printline = v-printline + 1.
            
            IF v-print-dept AND AVAIL oe-ordl THEN
            DO:
               FIND FIRST job-hdr WHERE
                    job-hdr.company eq cocode AND
                    job-hdr.job-no  EQ oe-ordl.job-no AND
                    job-hdr.job-no2 EQ oe-ordl.job-no2
                    NO-LOCK NO-ERROR.

               IF AVAIL job-hdr THEN
               DO:
                  FIND FIRST job WHERE
                       job.company eq cocode AND
                       job.job     eq job-hdr.job AND
                       job.job-no  eq job-hdr.job-no AND
                       job.job-no2 eq job-hdr.job-no2
                       NO-LOCK NO-ERROR.
                 
                  IF AVAIL job THEN
                  DO:
                     FOR EACH notes WHERE
                         notes.rec_key EQ job.rec_key AND /*for capitol task 06180806*/
                         CAN-DO(v-depts,notes.note_code)
                         NO-LOCK
                         BY notes.note_code:
                        
                         v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                         {SYS/INC/ROUNDUP.I v-tmp-lines}
                    
                         IF notes.note_text <> "" THEN
                            DO i = 1 TO v-tmp-lines:
                           
                               IF v-printline > 50 THEN DO:
                                  PAGE.
                                  v-printline = 0.
                                  {ar/rep/invhenry.i}
                               END.
                           
                               PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                               v-printline = v-printline + 1.
                            END.
                     
                     END.
                 
                     RELEASE job.
                  END.
                 
                  RELEASE job-hdr.
               END.
            END.
            
            IF v-printline > 50 THEN DO:
               PAGE.
               v-printline = 0.
               {ar/rep/invhenry.i}
            END.

        end. /* each ar-invl */

        lv-line-chars = 80.

        if v-prntinst then do:
           {custom/notesprtA.i ar-inv v-inst 4}
           DO i = 1 TO 4:
                IF v-inst[i] <> "" THEN DO:                
                   IF v-printline > 50 THEN DO:
                      PAGE.
                      v-printline = 0.
                      {ar/rep/invhenry.i}
                   END.
                   PUT v-inst[i] SKIP.
                   v-printline = v-printline + 1.
                END.            
           end.
          
           DO i = 1 TO 4:
              IF ar-inv.bill-i[i] <> "" THEN DO:
                 IF v-printline > 50 THEN DO:
                    PAGE.
                    v-printline = 0.
                    {ar/rep/invhenry.i}
                 END.
                 PUT ar-inv.bill-i[i] SKIP.
                 v-printline = v-printline + 1.
              END.
           END.
        END.

        IF v-printline > 50 THEN DO:
           PAGE.
           v-printline = 0.
           {ar/rep/invhenry.i}
        END.

        v-frt-tax = ar-inv.freight.        
        IF ar-inv.tax-code <> "" and
           (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
           AND ar-inv.freight <> 0
           AND AVAIL stax THEN
        do i = 1 to 5:

           if stax.tax-code1[i] ne "" AND stax.tax-frt1[i] EQ YES then do:
                create w-tax.
                assign
                 w-dsc      = stax.tax-dscr1[i]
                 w-tax      = round((if stax.company eq "yes" then v-frt-tax
                                                         ELSE ar-inv.freight) *
                                        stax.tax-rate1[i] / 100,2)                 
                 v-frt-tax  = v-frt-tax + w-tax
                 v-t-tax[i] = v-t-tax[i] + w-tax
                 v-lines    = v-lines + 1.
           END.
        end. 

    IF ar-inv.tax-amt EQ 0 THEN v-t-tax = 0.

    ELSE DO:
      v-tot-tax = 0.
      DO i = 1 TO 5:
        v-tot-tax = v-tot-tax + v-t-tax[i].
      END.
      IF v-tot-tax EQ 0 THEN
        ASSIGN
         v-t-tax    = 0
         v-t-tax[1] = ar-inv.tax-amt.

      ELSE DO:
        IF v-tot-tax NE ar-inv.tax-amt THEN
        DO i = 1 TO 5:
          v-t-tax[i] = ROUND(v-t-tax[i] * (ar-inv.tax-amt / v-tot-tax),2).
        END.
        v-tot-tax = 0.
        DO i = 1 TO 5:
          v-tot-tax = v-tot-tax + v-t-tax[i].
        END.
        IF v-tot-tax NE ar-inv.tax-amt THEN
          v-t-tax[1] = v-t-tax[1] + (ar-inv.tax-amt - v-tot-tax).
      END.
    END.

    do i = 1 to 5:
       v-bot-lab[i] = if v-t-tax[i] ne 0 then
                    /*  ((if avail stax then string(stax.tax-dscr[i],"x(5)")
                        else fill(" ",5))*/ 
                        ((IF AVAIL stax THEN string(CAPS(stax.tax-code1[i] + " TAX"),"x(7)") 
                           ELSE FILL(" ",7) ) +
                       fill(" ",4) + ":" +
                       string(v-t-tax[i],"->>>>,>>9.99")) else "".
    end.
    v-inv-freight = if (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
                    THEN ar-inv.freight ELSE 0.    
                    /*ar-inv.t-inv-freight*/.
 /*
  IF ltb_print-message THEN DO:
      IF v-printline > 55 THEN DO:
          PAGE.
          v-printline = 0.
          {ar/rep/invhenry.i}
      END.
      PUT "<FArial><R56><C1><P12><B> Remit to: </B>" cInvMessage[1] FORMAT "x(30)" SKIP
      "<c9>" cInvMessage[2] FORMAT "x(30)" SKIP
      "<c9>" cInvMessage[3] FORMAT "x(30)" SKIP
      "<c9>" cInvMessage[4] FORMAT "x(30)" SKIP
      "<c9>" cInvMessage[5] FORMAT "x(30)" SKIP.
      
      v-printline = v-printline + 5.
  END.
 */
 IF ltb_print-message THEN DO:
 FOR EACH bf-cust NO-LOCK
        WHERE bf-cust.company EQ cocode
        AND bf-cust.ACTIVE EQ "X":

        RUN pNotes(INPUT bf-cust.rec_key, OUTPUT cStockNotes).
        
        PUT "<p8><R62><C3>" cStockNotes[1] SKIP
                "<R63><C3>" cStockNotes[2] SKIP
                "<R64><C3>" cStockNotes[3] SKIP
                "<R65><C3>" cStockNotes[4] SKIP
                "<p10>".
        
 END.
 END.
IF v-bot-lab[4] <> "" THEN
    PUT "<R58><C59><#8><FROM><R+8><C+21><RECT> " 
        "<=8> Sub Total  :" v-subtot-lines FORM "$->>>,>>9.99"
        "<=8><R+1> Freight    :" v-inv-freight FORM "->>>>,>>9.99"
        "<=8><R+2> " v-bot-lab[1] 
        "<=8><R+3> " v-bot-lab[2]
        "<=8><R+4> " v-bot-lab[3]
        "<=8><R+5> " v-bot-lab[4]
        "<=8><R+6> " v-bot-lab[5]
        "<=8><R+7> Grand Total:" v-subtot-lines + v-t-tax[1] + v-t-tax[2] + v-t-tax[3] + v-t-tax[4] + v-t-tax[5] + v-inv-freight FORM "$->>,>>9.99" .
ELSE
    PUT "<R58><C59><#8><FROM><R+6><C+21><RECT> " 
        "<=8> Sub Total  :" v-subtot-lines FORM "$->>>,>>9.99"
        "<=8><R+1> Freight    :" v-inv-freight FORM "->>>>,>>9.99"
        "<=8><R+2> " v-bot-lab[1] 
        "<=8><R+3> " v-bot-lab[2]
        "<=8><R+4> " v-bot-lab[3]
        "<=8><R+5> Grand Total:" v-subtot-lines + v-t-tax[1] + v-t-tax[2] + v-t-tax[3] + v-inv-freight FORM "$->>>,>>9.99" .

    ASSIGN
       v-printline = v-printline + 6
       v-page-num = PAGE-NUM.

    /*IF v-printline < 50 THEN PUT SKIP(60 - v-printline). */
    PAGE. 

    DO TRANSACTION:
       FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
       ASSIGN xar-inv.printed = yes.

    END. /* DO TRANSACTION avail ar-inv */ 
 
end. /* each report, ar-inv */

PROCEDURE pNotes:

DEFINE INPUT PARAMETER reckey LIKE cust.rec_key NO-UNDO.
DEFINE OUTPUT PARAMETER cNotes AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
    
ASSIGN 
       v-tmp-lines = 0
       j = 0
       K = 0
       lv-got-return = 0.

FOR EACH notes WHERE notes.rec_key = reckey 
     AND notes.note_type = "G"
     AND notes.note_group = "BN" NO-LOCK:

    IF v-prev-note-rec <> ? AND
       v-prev-note-rec <> RECID(notes) THEN v-prev-extent = /*v-prev-extent +*/ k.
    DO i = 1 TO LENGTH(notes.note_text) :        
           IF i - j >= lv-line-chars THEN ASSIGN j = i
                                                 lv-got-return = lv-got-return + 1.
                  
           v-tmp-lines = ( i - j ) / lv-line-chars.
           {SYS/INC/ROUNDUP.I v-tmp-lines}
           k = v-tmp-lines + lv-got-return +
               IF (v-prev-note-rec <> RECID(notes) AND v-prev-note-rec <> ?) THEN v-prev-extent ELSE 0.

           IF k > 0 AND k <= 6 THEN cNotes[k] = cNotes[k] + 
                                  IF SUBSTRING(notes.note_text,i,1) <> CHR(10) AND SUBSTRING(notes.note_text,i,1) <> CHR(13)
                                  THEN SUBSTRING(notes.note_text,i,1)
                                  ELSE "" .              
           
           IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
           THEN do:
                  lv-got-return = lv-got-return + 1.
                  j = i.
           END.
    END.
    ASSIGN v-prev-note-rec = RECID(notes)
           j = 0
           lv-got-return = 0.
    
    IF k > 6 THEN LEAVE.
END.

END PROCEDURE.


/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
