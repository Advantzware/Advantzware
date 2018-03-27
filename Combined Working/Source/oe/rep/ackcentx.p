/* ----------------------------------------------- oe/rep/ackcentx.p 10/10 YSK */
/* ORDER ACKNOLEDGEMENT                                                       */
/* for ORDER STATUS = (R), (U)                                                */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-prt-revised AS LOG NO-UNDO.

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
def var v-totlin as dec format "->>,>>>,>>9.99".
def var lcnt as int init 1 NO-UNDO.
def var v-cust-phone as char format "(999)999-9999" no-undo.
def var v-part like oe-ordl.part-no no-undo.
def var v-tax-rate     as dec format ">,>>9.99<<<".
def var v-frt-tax-rate like v-tax-rate.
DEF VAR ll-calc-disc-first AS LOG NO-UNDO.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(200)" NO-UNDO.
DEF VAR v-first AS LOG NO-UNDO.

IF cocode = "002" THEN ls-image1 = "images\pyxisale.jpg".
ELSE ASSIGN ls-image1 = "images\cbxsale.jpg".

ASSIGN
FILE-INFO:FILE-NAME = ls-image1
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">"
FILE-INFO:FILE-NAME = ls-image2
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-billinst AS cha FORM "x(70)" EXTENT 4 NO-UNDO.
DEF VAR v-inst-lines AS INT NO-UNDO.
DEF VAR lv-first-note AS LOG NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-email AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-line-print AS INT INIT 47 NO-UNDO.
DEF VAR v-ext-price AS DEC NO-UNDO.
DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR lv-tot-pg AS INT INIT 1 NO-UNDO.
DEF VAR ln-cnt AS INT NO-UNDO.
DEF VAR lv-is-fgnote AS LOG NO-UNDO.
DEF VAR v-pdscr1 AS cha NO-UNDO.
DEF VAR v-pdscr2 AS cha NO-UNDO.
DEF VAR v-pdscr3 AS cha NO-UNDO.
DEF VAR lv-prt-sts AS cha NO-UNDO.
DEF VAR lv-prt-date AS DATE FORM "99/99/9999" NO-UNDO.
DEF VAR lv-prt-time AS cha NO-UNDO.
DEF VAR v-prod-code AS cha NO-UNDO.
DEF VAR v-whse-txt AS cha FORM "x(75)" NO-UNDO.
DEF VAR v-stat AS CHAR NO-UNDO.
DEF VAR v-schrel-found AS LOG NO-UNDO.
DEF VAR v-actrel-found AS LOG NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "ACKHEAD" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company eq cocode no-lock no-error.
{custom/notesdef.i}

 ASSIGN v-comp-add1 = ""
        v-comp-add2 = ""
        v-comp-add3 = ""
        v-comp-add4 = ""
        v-comp-add5 = ""
        lv-email = ""
        lv-comp-name = "".

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

      ASSIGN
       v-printline = 0
       v-addr3 = oe-ord.city + ", " + oe-ord.state + "  " + oe-ord.zip
       v-sold-addr3 = oe-ord.sold-city + ", " + oe-ord.sold-state +
                      "  " + oe-ord.sold-zip
       v-line = 1.
      IF oe-ord.sold-city = "" AND oe-ord.sold-state = ""
         AND oe-ord.sold-zip = "" THEN v-sold-addr3 = v-addr3.

      find first cust
          where cust.company eq cocode
            and cust.cust-no eq oe-ord.cust-no
          no-lock no-error.
      if avail cust then v-cust-phone = cust.area-code + cust.phone.
      /* get q-no for first item */
      v-q-no = 0.
      
      FIND first oe-ordl where oe-ordl.company eq oe-ord.company
                           and oe-ordl.ord-no  eq oe-ord.ord-no
                           NO-LOCK NO-ERROR.
      
      IF AVAIL oe-ordl THEN DO:
         
         ASSIGN v-q-no = oe-ordl.q-no.
      END.
      /* get total page-num */
      ln-cnt = 0.
      for each oe-ordl
          where oe-ordl.company eq oe-ord.company
            and oe-ordl.ord-no  eq oe-ord.ord-no AND
                oe-ordl.LINE > 0
          no-lock:
          ln-cnt = ln-cnt + 3.
          if /*oe-ordl.i-no ne oe-ordl.part-no or*/
             oe-ordl.part-dscr1 ne ""        then ln-cnt = ln-cnt + 1.
          if oe-ordl.part-dscr2 ne "" then ln-cnt = ln-cnt + 1.

          ASSIGN
             v-schrel-found = NO
             v-actrel-found = NO.

          IF v-schrel OR v-actrel THEN
          FOR EACH oe-rel
              WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.i-no    EQ oe-ordl.i-no
                AND oe-rel.line    EQ oe-ordl.line
              NO-LOCK:

              RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-stat).

              IF LOOKUP(v-stat,"I,L,S") > 0 THEN
                 v-schrel-found = YES.
              ELSE IF LOOKUP(v-stat,"A,B") > 0 THEN
                 v-actrel-found = YES.
              
              v-addr4 = "".
              if v-shipto then do:
                 find first shipto where shipto.company eq cocode
                                   and shipto.cust-no eq oe-rel.cust-no
                                   and shipto.ship-id eq oe-rel.ship-id
                                   no-lock no-error.
                 if avail shipto THEN
                    ASSIGN
                       ln-cnt = ln-cnt + 1
                       v-addr4 = shipto.ship-city + ", " +
                                 shipto.ship-state + "  " + shipto.ship-zip.
                 
                 IF shipto.ship-addr[1] <> "" THEN ln-cnt = ln-cnt + 1.
                 IF shipto.ship-addr[2] <> "" THEN ln-cnt = ln-cnt + 1.
                 IF v-addr4 <> "" THEN ln-cnt = ln-cnt + 1.
              END.
          END.
        
          IF v-schrel-found THEN
             ln-cnt = ln-cnt + 2.
          IF v-actrel-found THEN
             ln-cnt = ln-cnt + 2.

          IF v-prntinst THEN DO:
             find first itemfg {sys/look/itemfgrlW.i}
               and itemfg.i-no eq oe-ordl.i-no no-lock no-error.
             {custom/notesprt.i itemfg v-inst 4}
             lv-is-fgnote = NO.
             DO i = 1 TO 4:
                IF v-inst[i] <> "" THEN ASSIGN ln-cnt = ln-cnt + 1
                                             lv-is-fgnote = YES.
             END.
             IF lv-is-fgnote THEN ASSIGN ln-cnt = ln-cnt + 2
                                         lv-is-fgnote = NO.
          END.
      END.
      for each oe-ordm no-lock where oe-ordm.company eq oe-ord.company and
                           oe-ordm.ord-no eq oe-ord.ord-no break BY ord-no:
           if first(oe-ordm.ord-no) THEN ln-cnt = ln-cnt + 2.
           ln-cnt = ln-cnt + 1.
      END.
      IF oe-ord.f-bill THEN ln-cnt = ln-cnt + 1.
      ASSIGN
      lv-tot-pg = IF (ln-cnt MOD 27) = 0 THEN TRUNC( ln-cnt / 27,0)
                  ELSE lv-tot-pg + TRUNC( ln-cnt / 27,0)

      lv-prt-sts = IF ip-prt-revised THEN "<FGCOLOR=RED>REVISED<FGCOLOR=BLACK>" 
                                     ELSE "<FGCOLOR=BLACK>ORIGINAL<FGCOLOR=BLACK>" 
      lv-prt-date = TODAY
      lv-prt-time = STRING(TIME,"hh:mm am").

{oe/rep/ackcentx.i}

      for each oe-ordl
          where oe-ordl.company eq oe-ord.company
            and oe-ordl.ord-no  eq oe-ord.ord-no
            AND oe-ordl.LINE > 0 
          no-lock:
        if v-printline ge lv-line-print then
        do:
            PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
            PAGE .
            {oe/rep/ackcentx.i}
            assign v-printline = 20.          
        end.

        ASSIGN
        v-ext-price = oe-ordl.t-price
        v-part = if oe-ordl.i-no ne oe-ordl.part-no then oe-ordl.part-no
                 else oe-ordl.part-no

        /* task 06050602
        v-prod-code = ENTRY(INDEX("NR",oe-ordl.type-code),"NEW,REPEAT") NO-ERROR.
        IF ERROR-STATUS:ERROR THEN v-prod-code = "".
        */
        v-prod-code = IF INDEX("ON",oe-ordl.TYPE-code) > 0 THEN "NEW"
                      ELSE IF oe-ordl.type-code = "R" THEN "REPEAT"
                      ELSE "".

        put v-part AT 2  SPACE(1)             
            v-prod-code FORM "x(6)" SPACE(1)
            oe-ordl.i-name SPACE(8)
            oe-ordl.qty FORM "->>,>>>,>>>,>>9" 
            oe-ordl.price  FORM "$->>>>,>>9.99<<<<" SPACE(4)
            oe-ordl.pr-uom SPACE(2) 
            v-ext-price FORM "$->>>>,>>9.99"    
            SKIP.

        ASSIGN
           v-printline = v-printline + 1
           v-pdscr1 = oe-ordl.part-dscr1
           v-pdscr2 = oe-ordl.part-dscr2
           v-pdscr3 = oe-ordl.i-no.

        IF v-pdscr1 = "" THEN  ASSIGN v-pdscr1 = v-pdscr2
                                      v-pdscr2 = v-pdscr3
                                      v-pdscr3 = "".
        IF v-pdscr1 = "" THEN ASSIGN v-pdscr1 = v-pdscr2
                                     v-pdscr2 = v-pdscr3
                                     v-pdscr3 = "".
        if v-printline ge lv-line-print then
        do:
            PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
            PAGE .
            {oe/rep/ackcentx.i}
            assign v-printline = 20.          
        end.
        PUT oe-ordl.po-no AT 2
            v-pdscr1 FORM "x(30)" AT 25 SKIP.
        v-printline = v-printline + 1.
        
        IF v-pdscr2 <> "" THEN do:
           if v-printline ge lv-line-print then
           do:
             PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
             PAGE .
             {oe/rep/ackcentx.i}
             assign v-printline = 20.          
           end.
           PUT v-pdscr2 FORM "x(30)" AT 25  SKIP.
           v-printline = v-printline + 1.
        END.

        v-first = YES.
        IF v-schrel THEN
        for each oe-rel
            where oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
            no-lock break by oe-rel.rel-date:

            RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-stat).

            IF LOOKUP(v-stat,"I,L,S") > 0 THEN
            DO:
               RUN print-rels (v-first, YES).
               v-first = NO.
            END.
        end.   /* for each oe-rel  */

        v-first = YES.
        IF v-actrel THEN
        for each oe-rel
            where oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
            no-lock break by oe-rel.rel-date:

            RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-stat).

            IF LOOKUP(v-stat,"A,B") > 0 THEN
            DO:
               RUN print-rels (v-first, NO).
               v-first = NO.
            END.
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
                 if v-printline ge lv-line-print then
                 do:
                   PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                   PAGE .
                   {oe/rep/ackcentx.i}
                   assign v-printline = 20.          
                 end.
                 IF lv-first-note THEN do:
                    PUT SKIP(1).
                    ASSIGN
                    v-printline = v-printline + 1
                    lv-first-note = NO.
                 END.
                 PUT v-inst[i] SKIP.
                 v-printline = v-printline + 1.
              END.           
           END.
           IF NOT lv-first-note THEN do:
              PUT SKIP(1).
              v-printline = v-printline + 1.           
           END.
        END. 
     END. /* v-prntinst */
              PUT SKIP(1).
              v-printline = v-printline + 1.           
   end. /* each oe-ordl */

      for each oe-ordm no-lock where oe-ordm.company eq oe-ord.company and
          oe-ordm.ord-no eq oe-ord.ord-no break BY ord-no:

          if v-printline ge lv-line-print then
          do:
              PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
              PAGE .
              {oe/rep/ackcentx.i}
              assign v-printline = 20.          
          end.

        if first(oe-ordm.ord-no) then
        do:
          put "** Miscellaneous Items **" at 25 .
          if v-print-fmt eq "HOP" then put "Taxable" at 62.
          put skip(1).
          assign v-printline = v-printline + 2.
        end.
        if v-printline ge lv-line-print then
        do:
            PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
            PAGE .
            {oe/rep/ackcentx.i}
            assign v-printline = 20.          
        end.
        if oe-ordm.bill eq "N" then
           PUT oe-ordm.charge AT 25 oe-ordm.dscr FORM "x(30)" "      N/C" .          
        else
         PUT oe-ordm.charge AT 25 oe-ordm.dscr FORM "x(30)" /*SPACE(11)*/             
             oe-ordm.amt FORM "$->>>>,>>9.99" AT 101.

        PUT SKIP.
        assign v-line = v-line + 1
          v-printline = v-printline + 1.
       
        if oe-ordm.bill ne "N" THEN assign v-totord = v-totord + oe-ordm.amt.
      end. /* each oe-ordm */
           
      /* print freight*/
      IF oe-ord.f-bill THEN DO:
         v-totord = v-totord + oe-ord.t-freight.
         PUT "Freight:" AT 25 oe-ord.t-freight FORM "$->>>>,>>9.99" AT 101 .
         v-printline = v-printline + 1.
      END.
      /* print billing notes */
      ASSIGN v-billinst = "".

      IF v-prntinst THEN 
      DO i = 1 TO 4:
          v-billinst[i] = oe-ord.bill-i[i].
      END.      
 
      IF cust.ship-day <> 0 THEN 
         v-whse-txt = "Maximum warehouse hold term are " + string(cust.ship-day) + " days.".
      ELSE v-whse-txt = "This order will be manufactured and shipped immediately as make and ship.".

      PUT "<R54><C1>" v-whse-txt 
        "<R55><C1><#10><P12><B> Comments </B> <P10> " 
        "<R56><C1>" v-billinst[1] 
        "<R57><C1>" v-billinst[2] 
        "<R58><C1>" v-billinst[3] 
        "<R59><C1>" v-billinst[4]   "<P10>Order Total:" AT 90 "<U>"  v-totord FORM "$>>>>,>>9.99"  "</U><P9>"               
        "<R61><C1>" " ______________________________________(Please sign and fax back) " 
        "<R62><C1>" SPACE(32) "<P9><B>THIS IS A CONFIRMATION OF YOUR ORDER,NOT AN INVOICE.                   <P16> Thank You! <P10></B>" 
        "<R64><C1>" "<C65>Page "  string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" 
        .

      assign
       v-totord        = 0
       oe-ord.ack-prnt = yes
       v-printline = v-printline + 6
       lv-pg-num = PAGE-NUM .

      IF v-printline <= 66 THEN page.

    end. /* each oe-ord */

RETURN.

PROCEDURE print-rels:
   DEF INPUT PARAM ip-first AS LOG NO-UNDO.
   DEF INPUT PARAM ip-sch AS LOG NO-UNDO.

   DO WITH with frame sched-rel DOWN:
      if ip-first then do:
         lcnt = 1.
         if ip-sch THEN do:
           if v-printline ge lv-line-print then
           do:
              PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
              PAGE .
              {oe/rep/ackcentx.i}
              assign v-printline = 20.          
           end.
           put "Scheduled Releases:" at 25 SKIP .
           v-printline = v-printline + 1.
         end.
         else do:
           if v-printline ge lv-line-print then
           do:
              PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
              PAGE .
              {oe/rep/ackcentx.i}
              assign v-printline = 20.          
           end.
           put "Actual Releases:" at 25 SKIP. 
           v-printline = v-printline + 1.  
         end.
      end.
      
      IF AVAIL oe-rell THEN
      FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
      ASSIGN
         ld-date = IF AVAIL oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date
         lcnt        = lcnt + 1.
      
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
         {oe/rep/ackcentx.i}
         assign v-printline = 20.          
      end.
      IF AVAIL shipto THEN DO:
          IF v-shipto THEN
             put oe-rel.po-no AT 2 shipto.ship-name AT 25 FORM "x(28)" /*lcnt AT 55 FORM ">>9" SPACE(1)*/  ld-date AT 59 (if ip-sch then oe-rel.tot-qty else oe-rel.qty)  SKIP .
          ELSE
             PUT oe-rel.po-no AT 2 ld-date AT 59 (if ip-sch then oe-rel.tot-qty else oe-rel.qty)  SKIP .
             
          v-printline = v-printline + 1.
           
          IF v-shipto AND shipto.ship-addr[1] <> "" THEN DO:
             if v-printline ge lv-line-print then
             do:
               PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
               PAGE .
               {oe/rep/ackcentx.i}
               assign v-printline = 20.          
             end.
             PUT shipto.ship-addr[1] AT 25  SKIP.
             v-printline = v-printline + 1.
        
          END.
          IF v-shipto AND shipto.ship-addr[2] <> "" THEN DO:
              if v-printline ge lv-line-print then
              do:
                PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                PAGE .
                {oe/rep/ackcentx.i}
                assign v-printline = 20.          
              end. 
              PUT shipto.ship-addr[2] AT 25 SKIP.
              v-printline = v-printline + 1.
          END.
          IF v-shipto AND v-addr4 <> "" THEN DO:
             if v-printline ge lv-line-print then
             do:
                PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                PAGE .
                {oe/rep/ackcentx.i}
                assign v-printline = 20.          
             end. 
             PUT v-addr4 AT 25  SKIP.
             v-printline = v-printline + 1.
          END.
      END.      
      PUT SKIP(1).
      v-printline = v-printline + 1.
         
   END.

END PROCEDURE.

/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */
