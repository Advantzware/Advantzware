/* ---------------------------------------------- oe/rep/ackpkrit.p           */
/* ORDER ACKNOLEDGEMENT   for Packrite                                        */
/* for ORDER STATUS = (R), (U)                                                */
/* -------------------------------------------------------------------------- */
DEF INPUT PARAM ip-format AS cha NO-UNDO.

{sys/inc/var.i shared}

{oe/rep/acknowl.i}

def var v-salesman as char format "x(3)" NO-UNDO.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO.
def var v-addr3 as char format "x(30)" NO-UNDO.
def var v-addr4 as char format "x(30)" NO-UNDO.
def var v-sold-addr3 as char format "x(30)" NO-UNDO.
def var v-line as INT NO-UNDO.
def var v-printline as int NO-UNDO.
def var v-totord as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-totlin as dec format "->>,>>>,>>9.99" NO-UNDO.
def var lcnt as int init 1 NO-UNDO.
def var v-cust-phone as char format "(999)999-9999" no-undo.
def var v-part like oe-ordl.part-no no-undo.
def var v-tax-rate     as dec format ">,>>9.99<<<".
def var v-frt-tax-rate like v-tax-rate.
DEF VAR ll-calc-disc-first AS LOG NO-UNDO.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.

ASSIGN ls-image1 = "images\packrite.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-billinst AS cha FORM "x(70)" EXTENT 4 NO-UNDO.
DEF VAR v-inst-lines AS INT NO-UNDO.
DEF VAR lv-first-note AS LOG NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-email AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.
DEF VAR lv-line-print AS INT INIT 47 NO-UNDO.
DEF VAR v-ext-price AS DEC NO-UNDO.
DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR lv-tot-pg AS INT NO-UNDO.
DEF VAR ln-cnt AS INT NO-UNDO.
DEF VAR v-pdscr1 AS cha NO-UNDO.
DEF VAR v-ship-adr AS cha EXTENT 4 FORMAT "X(30)" NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "ACKHEAD" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

find first company where company.company eq cocode no-lock no-error.
{custom/notesdef.i}

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

  ll-calc-disc-first = NO.
  FOR EACH sys-ctrl
      WHERE sys-ctrl.company  EQ cocode
        AND sys-ctrl.name     EQ "INVPRINT"
        AND sys-ctrl.char-fld EQ "Dayton"
      NO-LOCK:
    ll-calc-disc-first = YES.
    LEAVE.
  END.

  find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
  
  FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
      FIRST oe-ord WHERE RECID(oe-ord) EQ report.rec-id BREAK BY oe-ord.ord-no:

      IF FIRST-OF(oe-ord.ord-no) THEN lv-tot-pg = 1.

      IF oe-ord.sman[1] <> "" THEN DO:
         FIND FIRST sman WHERE sman.company EQ oe-ord.company
                           AND sman.sman    EQ oe-ord.sman[1] NO-LOCK NO-ERROR.
         v-salesman = IF AVAIL sman THEN sman.sname ELSE oe-ord.sman[1].
      END.

      if oe-ord.fob-code eq "ORIG" then
        v-fob = "Origin".
      else
        v-fob = "Destination".

      find FIRST carrier
          where carrier.company eq oe-ord.company
            and carrier.carrier eq oe-ord.carrier
          no-lock no-error.
      if avail carrier then
        v-shipvia = carrier.dscr.
      else
        v-shipvia = "".
      
      assign
       v-printline = 0
       v-addr3 = oe-ord.city + ", " + oe-ord.state + "  " + oe-ord.zip
       v-sold-addr3 = oe-ord.sold-city + ", " + oe-ord.sold-state +
                      "  " + oe-ord.sold-zip
       v-line = 1
       v-q-no = 0.

      IF oe-ord.sold-city = "" AND oe-ord.sold-state = ""
         AND oe-ord.sold-zip = "" THEN v-sold-addr3 = v-addr3.

      find first cust
          where cust.company eq cocode
            and cust.cust-no eq oe-ord.cust-no
          no-lock no-error.
      if avail cust then v-cust-phone = cust.area-code + cust.phone.
      /* get q-no for first item */
      
      FIND first oe-ordl where oe-ordl.company eq oe-ord.company
                           and oe-ordl.ord-no  eq oe-ord.ord-no
                           NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl AND oe-ordl.est-no <> "" THEN DO:
         FIND last quotehd WHERE quotehd.company = oe-ordl.company
                              AND quotehd.est-no = oe-ordl.est-no
                              NO-LOCK NO-ERROR.
         IF AVAIL quotehd THEN DO:
            FIND FIRST quoteitm OF quotehd WHERE quoteitm.part-no = oe-ordl.part-no /*i-no*/
                     NO-LOCK NO-ERROR.
            IF AVAIL quoteitm THEN DO:
                v-q-no = quoteitm.q-no.
               FIND FIRST quoteqty WHERE quoteqty.company = oe-ordl.company
                                     AND quoteqty.loc = quoteitm.loc
                                     AND quoteqty.q-no = quoteitm.q-no
                                     AND quoteqty.LINE = quoteitm.LINE
                                     AND quoteqty.qty = oe-ordl.qty
                                     NO-LOCK NO-ERROR.
               IF AVAIL quoteqty THEN v-q-no = quoteqty.q-no.
            END.
         END.
      END.

      /* get total page-num */
      ln-cnt = 0.
      for each oe-ordl FIELDS(company ord-no i-no LINE)
          where oe-ordl.company eq oe-ord.company
            and oe-ordl.ord-no  eq oe-ord.ord-no
          no-lock:

          ln-cnt = ln-cnt + 2.

          FOR EACH oe-rel FIELDS(link-no)
              WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.i-no    EQ oe-ordl.i-no
                AND oe-rel.line    EQ oe-ordl.line
                AND ((oe-rel.link-no EQ 0 AND v-schrel)
                 OR  (oe-rel.link-no NE 0 AND v-actrel))
              NO-LOCK BREAK BY oe-rel.link-no DESC:

              if first-of(oe-rel.link-no) then DO:
                 if oe-rel.link-no eq 0 then
                    ln-cnt = ln-cnt + 1.
              END.
              ELSE if first(oe-rel.link-no) THEN
                   ln-cnt = ln-cnt + 1.
          END.
          find first itemfg {sys/look/itemfgrlW.i}
               and itemfg.i-no eq oe-ordl.i-no no-lock no-error.
          {custom/notesprt.i itemfg v-inst 4}
          
          IF v-prntinst THEN
             DO i = 1 TO 4:
                IF v-inst[i] <> "" THEN ASSIGN ln-cnt = ln-cnt + 1.
             END.
      END.

      for each oe-ordm fields(ord-no) where
          oe-ordm.company eq oe-ord.company and
          oe-ordm.ord-no eq oe-ord.ord-no
          NO-LOCK
          break BY ord-no:
          if first(oe-ordm.ord-no) THEN ln-cnt = ln-cnt + 2.
          ln-cnt = ln-cnt + 1.
      END.
      
      lv-tot-pg = lv-tot-pg + TRUNC(ln-cnt / 25,0).

      if v-shipto then do:

         v-ship-adr = "".

         FIND FIRST oe-rel WHERE
              oe-rel.company EQ oe-ord.company AND
              oe-rel.ord-no EQ oe-ord.ord-no
              NO-LOCK NO-ERROR.

         IF AVAIL oe-rel THEN
         DO:
            find first shipto
                 where shipto.company eq cocode
                   and shipto.cust-no eq oe-rel.cust-no
                   and shipto.ship-id eq oe-rel.ship-id
                 no-lock no-error.
            if avail shipto then
               ASSIGN v-ship-adr[1] = shipto.ship-name 
                      v-ship-adr[2] = shipto.ship-addr[1]  
                      v-ship-adr[3] = shipto.ship-addr[2]  
                      v-ship-adr[4] = shipto.ship-city + ", " + shipto.ship-state + "  " + shipto.ship-zip.
         END.
      end.

      {oe/rep/ackpkrit.i}

      for each oe-ordl
          where oe-ordl.company eq oe-ord.company
            and oe-ordl.ord-no  eq oe-ord.ord-no
          no-lock:
        if v-printline ge lv-line-print then
        do:
            PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
            PAGE .
            {oe/rep/ackpkrit.i}
            assign v-printline = 20.          
        end.

        ASSIGN
           v-ext-price = oe-ordl.t-price
           v-part = if oe-ordl.i-no ne oe-ordl.part-no then oe-ordl.part-no
                    else oe-ordl.part-no
           v-printline = v-printline + 1.

        put v-part AT 2  oe-ordl.po-no SPACE(1)
            oe-ordl.i-name FORM "x(19)" SPACE(1)
            oe-ordl.qty FORM "->,>>>,>>>,>>9" 
            oe-ordl.price  FORM "$>>>,>>9.99<<<<" SPACE(1)
            oe-ordl.pr-uom SPACE(1)
            v-ext-price FORM "$->>>,>>9.99"   SKIP.
        
       if v-printline ge lv-line-print then
        do:
            PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
            PAGE .
            {oe/rep/ackpkrit.i}
            assign v-printline = 20.          
        end.
        
        ASSIGN v-pdscr1 = oe-ordl.i-no
               v-printline = v-printline + 1.
        
        PUT v-pdscr1 FORM "x(30)" AT 33  SKIP.
        
        if v-printline ge lv-line-print then
        do:
            PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
            PAGE .
            {oe/rep/ackpkrit.i}
            assign v-printline = 20.          
        end.
        
        FOR EACH oe-rel
            WHERE oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
              AND ((oe-rel.link-no EQ 0 AND v-schrel)
               OR  (oe-rel.link-no NE 0 AND v-actrel))
            NO-LOCK BREAK BY oe-rel.link-no DESC WITH FRAME sched-rel DOWN:

          if first-of(oe-rel.link-no) then
            if oe-rel.link-no eq 0 then lcnt = 1.
            else
            if first(oe-rel.link-no) then lcnt = 1.
  
          if v-printline ge lv-line-print then
          do:
            PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
            PAGE .
            {oe/rep/ackppi.i}
            assign v-printline = 20.          
          end.
          if first-of(oe-rel.link-no) then do:
            if oe-rel.link-no eq 0 then do:
              put "Scheduled Releases:" at 21 SKIP .
              v-printline = v-printline + 1.
            end.
            else
            if first(oe-rel.link-no) then do:
              put "Actual Releases:" at 21 SKIP. 
              v-printline = v-printline + 1.  
            end.
          end.
          
          if v-printline ge lv-line-print then
          do:
            PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
            PAGE .
            {oe/rep/ackppi.i}
            assign v-printline = 20.          
          end.
          

          {oe/rel-stat.i lv-stat}
          IF AVAIL oe-rell THEN
          FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
          ld-date = IF AVAIL oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date.

          /*put lcnt AT 46 SPACE(3) ld-date  oe-rel.qty  SKIP. */
          /*down with frame sched-rel. */

          assign
       /*    v-printline = v-printline + 1 */
           lcnt        = lcnt + 1.

          if v-shipto then do:
            find first shipto
                where shipto.company eq cocode
                  and shipto.cust-no eq oe-rel.cust-no
                  and shipto.ship-id eq oe-rel.ship-id
                no-lock no-error.
            if avail shipto then
              v-addr4 = shipto.ship-city + ", " +
                        shipto.ship-state + "  " + shipto.ship-zip.
      
            if v-printline ge lv-line-print  then
            do:
               PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
               PAGE .
               {oe/rep/ackppi.i}
               assign v-printline = 20.          
            end.
            IF AVAIL shipto THEN DO:
                put oe-rel.po-no AT 2 shipto.ship-name AT 21 FORM "x(28)" lcnt AT 55 FORM ">>9" SPACE(1) ld-date  (IF oe-rel.link-no EQ 0 THEN oe-rel.tot-qty ELSE oe-rel.qty)  SKIP .
                v-printline = v-printline + 1.
                if v-printline ge lv-line-print then
                do:
                    PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                    PAGE .
                    {oe/rep/ackppi.i}
                    assign v-printline = 20.          
                end.
                IF shipto.ship-addr[1] <> "" THEN DO:
                   PUT shipto.ship-addr[1] AT 21  SKIP.
                   v-printline = v-printline + 1.
                   if v-printline ge lv-line-print then
                   do:
                      PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                      PAGE .
                      {oe/rep/ackppi.i}
                      assign v-printline = 20.          
                   end.
                END.
                IF shipto.ship-addr[2] <> "" THEN DO:
                    PUT shipto.ship-addr[2] AT 21  SKIP.
                    v-printline = v-printline + 1.
                    /*if v-printline ge lv-line-print then
                    do:
                        PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                        PAGE .
                        {oe/rep/ackcentx.i}
                        assign v-printline = 20.          
                    end. */
                END.
                IF v-addr4 <> "" THEN DO:
                   PUT v-addr4 AT 21 SKIP.
                   v-printline = v-printline + 1.
                END.                
            END.
          end.
        end.   /* for each oe-rel  */
        v-line = v-line + 1.
    
        if oe-ordl.pr-uom begins "L" AND oe-ordl.pr-uom NE "LB" then
           assign v-totlin = oe-ordl.price * IF oe-ordl.qty LT 0 THEN -1 ELSE 1.                             
        else
        if oe-ordl.pr-uom eq "CS" then
        do:
          find first itemfg {sys/look/itemfgrlW.i}
            and itemfg.i-no eq oe-ordl.i-no no-lock no-error.

          v-totlin = oe-ordl.qty /
                     (if oe-ordl.cas-cnt ne 0 then oe-ordl.cas-cnt else
                      if avail itemfg and itemfg.case-count ne 0
                      then itemfg.case-count else 1) *
                     oe-ordl.price.
        end.
        else
        if oe-ordl.pr-uom eq "C" then
          v-totlin = oe-ordl.qty / 100 * oe-ordl.price.

        else
        if oe-ordl.pr-uom eq "M" then
          v-totlin = oe-ordl.qty / 1000 * oe-ordl.price.

        else /** DEFAULT TO EACH **/
          v-totlin = oe-ordl.qty * oe-ordl.price.

        v-totlin = ROUND(v-totlin,2).

        IF oe-ordl.disc NE 0 THEN
           v-totlin = IF ll-calc-disc-first THEN 
                        (v-totlin - ROUND(v-totlin * oe-ordl.disc / 100,2))
                      ELSE
                        ROUND(v-totlin * (1 - (oe-ordl.disc / 100)),2).

        v-totord = v-totord + v-totlin.

     IF v-prntinst THEN DO:
                   /* print spec notes */
        find first itemfg {sys/look/itemfgrlW.i}
             and itemfg.i-no eq oe-ordl.i-no no-lock no-error.

        IF AVAIL itemfg THEN DO:
           lv-first-note = yes.         
           {custom/notesprt.i itemfg v-inst 4}
           DO i = 1 TO 4:
              IF v-inst[i] <> "" THEN DO:
                 IF lv-first-note THEN do:
                    PUT SKIP(1).
                    v-printline = v-printline + 1.
                    lv-first-note = NO.
                 END.
                 PUT v-inst[i] SKIP.
                 v-printline = v-printline + 1.
              END.
              if v-printline ge lv-line-print then
              do:
                  PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                  PAGE .
                  {oe/rep/ackpkrit.i}
                  assign v-printline = 20.          
              end.
           END.
           IF NOT lv-first-note THEN do:
              PUT SKIP(1).
              v-printline = v-printline + 1.           
           END.
           if v-printline ge lv-line-print then
           do:
              PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
              PAGE .
              {oe/rep/ackpkrit.i}
              assign v-printline = 20.          
           end.
        END. /* v-prntinst */
         END.
      end. /* each oe-ordl */

      for each oe-ordm no-lock where oe-ordm.company eq oe-ord.company and
          oe-ordm.ord-no eq oe-ord.ord-no break BY ord-no:

          if v-printline ge lv-line-print then
          do:
              PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
              PAGE .
              {oe/rep/ackpkrit.i}
              assign v-printline = 20.          
          end.

          if first(oe-ordm.ord-no) then
          do:
            put "**** Miscellaneous Items ****" at 21 .
            if v-print-fmt eq "HOP" then put "Taxable" at 62.
            put skip(1).
            assign v-printline = v-printline + 2.
          end.
          if v-printline ge lv-line-print then
          do:
              PUT "<R59><c65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
              PAGE .
              {oe/rep/ackpkrit.i}
              assign v-printline = 20.          
          end.
          if oe-ordm.bill eq "N" then
             PUT oe-ordm.charge AT 21 oe-ordm.dscr FORM "x(30)" "      N/C" .          
          else
           PUT oe-ordm.charge AT 21 oe-ordm.dscr FORM "x(30)"
               oe-ordm.amt FORM "$->>>>,>>9.99" AT 83.
          PUT SKIP.
          assign v-line = v-line + 1
                 v-printline = v-printline + 1.
          if v-printline ge lv-line-print then
          do:
              PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
              PAGE .
              {oe/rep/ackpkrit.i}
              assign v-printline = 20.          
          end.
          if oe-ordm.bill ne "N" THEN assign v-totord = v-totord + oe-ordm.amt.
      end. /* each oe-ordm */
           
      /* print billing notes */
      ASSIGN v-billinst = "".

      IF v-prntinst THEN 
      DO i = 1 TO 4:
          v-billinst[i] = oe-ord.bill-i[i].
      END.
      
      PUT "<R55><C1><#10><P12><B> Comments </B> <P10> " 
        "<R56><C1>" v-billinst[1] 
        "<R57><C1>" v-billinst[2] 
        "<R58><C1>" v-billinst[3] 
        "<R59><C1>" v-billinst[4]   "<P10>Order Total:" AT 71 "<U>"  v-totord FORM "$->>>>,>>9.99"  "</U><P9>"
        "<R62><C1>" SPACE(32) "<P9><B>THIS IS A CONFIRMATION OF YOUR ORDER,NOT AN INVOICE.       <P16>Thank You! <P10></B>" 
        "<R64><C65>Page "  string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)".

      assign
       v-totord        = 0
       oe-ord.ack-prnt = YES
       v-printline = v-printline + 6
       lv-pg-num = PAGE-NUM.

      IF v-printline <= 66 THEN page.

    end. /* each oe-ord */

/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */
