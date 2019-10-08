/* ----------------------------------------------- oe/rep/acksoule.p 05/05 YSK */
/* ORDER ACKNOLEDGEMENT     Copied from ackunipk.p                            */
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
def var v-ship-name as char format "x(30)" NO-UNDO.
def var v-ship-add as char format "x(30)" NO-UNDO.
DEF VAR v-ship-add2 AS CHAR FORMAT "x(30)" NO-UNDO.
def var v-sold-addr3 as char format "x(30)" NO-UNDO.
def var v-line as INT NO-UNDO.
def var v-printline as int NO-UNDO.
def var v-ackhead as char format "x(32)" init
  "A C K N O W L E D G E M E N T" NO-UNDO.
def var v-len as int NO-UNDO.
def var v-totord as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-totlin as dec format "->>,>>>,>>9.99".
def var v-ans as log init no NO-UNDO.
def var lcnt as int init 1 NO-UNDO.
def var pagebreak as int init 28 NO-UNDO.
def var v-cust-phone as char format "(999)999-9999" no-undo.
def var v-part like oe-ordl.part-no no-undo.
def var v-tax-rate     as dec format ">,>>9.99<<<".
def var v-frt-tax-rate like v-tax-rate.
DEF VAR ll-calc-disc-first AS LOG NO-UNDO.
def var v-part-qty          as   dec.
DEF SHARED VAR v-print-components AS LOG NO-UNDO.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(200)" NO-UNDO.

/*IF cocode = "002" THEN ls-image1 = "images\unipak.jpg".
ELSE*/ ASSIGN ls-image1 = "images\Soule.jpg".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
FILE-INFO:FILE-NAME = ls-image2.
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(40)" NO-UNDO .

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
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR lv-line-start AS INT INIT 20 NO-UNDO. /*line to start body*/
DEF VAR lv-line-print AS INT INIT 46 NO-UNDO. /*# of lines in body*/
DEF VAR v-ext-price AS DEC NO-UNDO.
DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR lv-tot-pg AS INT INIT 1 NO-UNDO.
DEF VAR ln-cnt AS INT NO-UNDO.
DEF VAR lv-is-fgnote AS LOG NO-UNDO.
DEF VAR v-pdscr1 AS cha NO-UNDO.
DEF VAR v-pdscr2 AS cha NO-UNDO.
DEF VAR v-pdscr3 AS cha NO-UNDO.
DEF VAR v-color AS cha NO-UNDO.
DEF VAR lv-prt-sts AS cha NO-UNDO.
DEF VAR lv-prt-date AS DATE FORM "99/99/9999" NO-UNDO.
DEF VAR lv-prt-time AS cha NO-UNDO.
DEF VAR v-board AS cha NO-UNDO.
DEF VAR vitem-name AS cha NO-UNDO.
def buffer xitemfg      for itemfg.
 def buffer vitemfg      for itemfg.
DEF BUFFER xeb FOR eb.
DEF BUFFER xef FOR ef.

/* gdm - 05270902 */
DEF VAR v-size     AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR style-dscr AS CHAR FORMAT "x(30)" NO-UNDO.

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
      /*  ASSIGN v-comp-add1 = company.addr[1]
               v-comp-add2 = company.addr[2]
               v-comp-add3 = company.city + ", " + company.st + "  " + company.zip
               v-comp-add4 = "Phone:  " + IF AVAIL cust THEN string(cust.area-code,"(999)") + string(cust.phone,"999-9999") ELSE "" 
               v-comp-add5 = "Fax     :  " + IF AVAIL cust THEN string(cust.fax,"(999)999-9999") ELSE ""
               lv-email    = "Email:  " + IF AVAIL cust THEN cust.email ELSE ""
               lv-comp-name = company.NAME   
               .
      */ 
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

      /* gdm - 05270902 */
      FIND FIRST est NO-LOCK 
        WHERE est.company EQ oe-ord.company
          AND est.est-no  EQ oe-ord.est-no NO-ERROR.

      IF FIRST-OF(oe-ord.ord-no) THEN lv-tot-pg = 1.
      /*
      if oe-ord.sman[2] eq "" and oe-ord.sman[3] eq "" then
        v-salesman = oe-ord.sman[1].
      else
        v-salesman = oe-ord.sman[1] + oe-ord.sman[2] + oe-ord.sman[3].
      */
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
      v-printline = 0.
      assign
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
      /* old way
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
      =========*/
      IF AVAIL oe-ordl THEN DO:
         
         ASSIGN v-q-no = oe-ordl.q-no.
      END.
/*       /* get total page-num */                                                          */
/*       ln-cnt = 0.                                                                       */
/*       for each oe-ordl                                                                  */
/*           where oe-ordl.company eq oe-ord.company                                       */
/*             and oe-ordl.ord-no  eq oe-ord.ord-no AND                                    */
/*                 oe-ordl.LINE > 0                                                        */
/*           no-lock:                                                                      */
/*           ln-cnt = ln-cnt + 3.                                                          */
/*           if /*oe-ordl.i-no ne oe-ordl.part-no or*/                                     */
/*              oe-ordl.part-dscr1 ne ""        then ln-cnt = ln-cnt + 1.                  */
/*           if oe-ordl.part-dscr2 ne "" then ln-cnt = ln-cnt + 1.                         */
/*                                                                                         */
/*           FOR EACH oe-rel                                                               */
/*               WHERE oe-rel.company EQ oe-ordl.company                                   */
/*                 AND oe-rel.ord-no  EQ oe-ordl.ord-no                                    */
/*                 AND oe-rel.i-no    EQ oe-ordl.i-no                                      */
/*                 AND oe-rel.line    EQ oe-ordl.line                                      */
/*                 AND ((oe-rel.link-no EQ 0 AND v-schrel) OR                              */
/*                      (oe-rel.link-no NE 0 AND v-actrel))                                */
/*               NO-LOCK BREAK BY oe-rel.link-no DESC:                                     */
/*                                                                                         */
/*               if first-of(oe-rel.link-no) then DO:                                      */
/*                  if oe-rel.link-no eq 0 then ln-cnt = ln-cnt + 1.                       */
/*                  ELSE if first(oe-rel.link-no) then ln-cnt = ln-cnt + 1.                */
/*               end.                                                                      */
/* /*               v-addr4 = "".                                            */            */
/* /*               if v-shipto then do:                                     */            */
/* /*                 find first shipto where shipto.company eq cocode       */            */
/* /*                                   and shipto.cust-no eq oe-rel.cust-no */            */
/* /*                                   and shipto.ship-id eq oe-rel.ship-id */            */
/* /*                                   no-lock no-error.                    */            */
/* /*                 if avail shipto THEN do:                               */            */
/* /*                     ln-cnt = ln-cnt + 1.                               */            */
/* /*                    ASSIGN                                              */            */
/* /*                        v-ship-name = shipto.ship-name                  */            */
/* /*                        v-ship-add  = shipto.ship-addr[1]               */            */
/* /*                        v-addr4 = shipto.ship-city + ", " +             */            */
/* /*                        shipto.ship-state + "  " + shipto.ship-zip.     */            */
/* /*                 END.                                                   */            */
/* /*                 IF shipto.ship-addr[1] <> "" THEN ln-cnt = ln-cnt + 1. */            */
/* /*                 IF shipto.ship-addr[2] <> "" THEN ln-cnt = ln-cnt + 1. */            */
/* /*                 IF v-addr4 <> "" THEN ln-cnt = ln-cnt + 1.             */            */
/* /*               END.                                                     */            */
/*               ln-cnt = ln-cnt + 1.                                                      */
/*                                                                                         */
/*           END.                                                                          */
/*           find first itemfg {sys/look/itemfgrlW.i}                                       */
/*                and itemfg.i-no eq oe-ordl.i-no no-lock no-error.                        */
/*           {custom/notesprt.i itemfg v-inst 4}                                           */
/*           lv-is-fgnote = NO.                                                            */
/*           DO i = 1 TO 4:                                                                */
/*               IF v-inst[i] <> "" THEN ASSIGN ln-cnt = ln-cnt + 1                        */
/*                                              lv-is-fgnote = YES.                        */
/*           END.                                                                          */
/*           IF lv-is-fgnote THEN ASSIGN ln-cnt = ln-cnt + 2                               */
/*                                       lv-is-fgnote = NO.                                */
/*                                                                                         */
/*       END.                                                                              */
/*       for each oe-ordm no-lock where oe-ordm.company eq oe-ord.company and              */
/*                            oe-ordm.ord-no eq oe-ord.ord-no break BY ord-no:             */
/*            if first(oe-ordm.ord-no) THEN ln-cnt = ln-cnt + 2.                           */
/*            ln-cnt = ln-cnt + 1.                                                         */
/*       END.                                                                              */
/*       IF oe-ord.f-bill THEN ln-cnt = ln-cnt + 1.                                        */
/* MESSAGE ln-cnt TRUNC( ln-cnt / (lv-line-print - lv-line-start),0)                       */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                  */
/*       lv-tot-pg = IF (ln-cnt MOD (lv-line-print - lv-line-start)) = 0 THEN              */
/*           TRUNC( ln-cnt / (lv-line-print - lv-line-start),0)                            */
/*                   ELSE lv-tot-pg + TRUNC( ln-cnt / (lv-line-print - lv-line-start),0) . */

ASSIGN lv-prt-sts = /*IF NOT oe-ord.ack-prnt THEN "ORIGINAL" ELSE "REVISED"*/
                    IF ip-prt-revised THEN "<FGCOLOR=RED>REVISED<FGCOLOR=BLACK>" 
                                      ELSE "<FGCOLOR=BLACK>ORIGINAL<FGCOLOR=BLACK>" 
       lv-prt-date = TODAY
       lv-prt-time = STRING(TIME,"hh:mm am").
       FIND FIRST oe-rel
              WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.i-no    EQ oe-ordl.i-no
           NO-LOCK NO-ERROR.
       IF NOT AVAIL oe-rel THEN
           FIND FIRST oe-rel
              WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
           NO-LOCK NO-ERROR.
      v-addr4 = "".
      IF /*v-shipto AND*/ AVAIL oe-rel then do:
        find first shipto where shipto.company eq cocode
                          and shipto.cust-no eq oe-rel.cust-no
                          and shipto.ship-id eq oe-rel.ship-id
                          no-lock no-error.
        if avail shipto THEN do:
/*             ln-cnt = ln-cnt + 1. */
           ASSIGN
               v-ship-name = shipto.ship-name
               v-ship-add  = shipto.ship-addr[1]
               v-ship-add2 = shipto.ship-addr[2]
               v-contact   = shipto.contact + " " + 
                STRING(shipto.area-code,"(999)") + " " + 
                STRING(shipto.phone,"999-9999")
               v-addr4 = shipto.ship-city + ", " +
                shipto.ship-state + "  " + shipto.ship-zip.
        END.
/*         IF shipto.ship-addr[1] <> "" THEN ln-cnt = ln-cnt + 1. */
/*         IF shipto.ship-addr[2] <> "" THEN ln-cnt = ln-cnt + 1. */
/*         IF v-addr4 <> "" THEN ln-cnt = ln-cnt + 1.             */
      END.
PUT "[@startPage" oe-ord.ord-no "]".
{oe/rep/acksoule.i}

      for each oe-ordl
          where oe-ordl.company eq oe-ord.company
            and oe-ordl.ord-no  eq oe-ord.ord-no
            AND oe-ordl.LINE > 0 
          no-lock:

    /* gdm - 05270902 */
       IF AVAIL est THEN 
        FIND FIRST eb
          WHERE eb.company EQ est.company
            AND eb.est-no  EQ oe-ordl.est-no
            AND eb.part-no EQ oe-ordl.part-no
            AND eb.form-no NE 0 NO-LOCK NO-ERROR.
        IF AVAIL eb THEN 
          FIND FIRST ef
            WHERE ef.company EQ est.company
              AND ef.est-no  EQ est.est-no
              AND ef.form-no EQ eb.form-no NO-LOCK NO-ERROR.        
       /* gdm - 05270902 end */

/*         if v-printline ge lv-line-print then                                                                     */
/*         do:                                                                                                      */
/*             PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" . */
/*             PAGE .                                                                                               */
/*             {oe/rep/acksoule.i}                                                                                  */
/*             assign v-printline = 20.                                                                             */
/*         end.                                                                                                     */
        v-color = "".
        v-ext-price = oe-ordl.t-price.
        v-part = if oe-ordl.i-no ne oe-ordl.part-no then oe-ordl.part-no
                 else oe-ordl.part-no.

        ASSIGN vitem-name = oe-ordl.i-name
               v-pdscr1 = oe-ordl.part-dscr1
               v-pdscr2 = oe-ordl.part-dscr2
               v-pdscr3 = oe-ordl.i-no.
        IF v-pdscr1 = "" THEN  ASSIGN v-pdscr1 = v-pdscr2
                                      v-pdscr2 = v-pdscr3
                                      v-pdscr3 = "".
        IF v-pdscr1 = "" THEN ASSIGN v-pdscr1 = v-pdscr2
                                     v-pdscr2 = v-pdscr3
                                     v-pdscr3 = "".

        IF AVAIL ef THEN
            ASSIGN v-board = ef.board .
        ELSE
            v-board = "".

        IF AVAIL eb THEN
            v-color = eb.i-coldscr.

        IF AVAIL ef AND v-pdscr1 EQ "" THEN do:
            FIND FIRST item WHERE  item.company = cocode AND
                 item.i-no = ef.board NO-LOCK NO-ERROR.
                IF AVAIL ITEM THEN do:
/*                     ASSIGN                             */
/* /*                         vitem-name = ITEM.i-name */ */
/*                         v-pdscr1 = ITEM.i-dscr .       */
/*                     IF item.mat-type EQ "B" THEN       */
/*                         ASSIGN                         */
                        v-pdscr1 = ITEM.est-dscr.
                END.
        END.

        IF v-board = "" AND v-pdscr1 EQ "" THEN do:
         FIND FIRST  vitemfg
             where vitemfg.company eq cocode
             and vitemfg.i-no    eq oe-ordl.i-no
             NO-LOCK NO-ERROR.

         FIND FIRST xeb
                WHERE xeb.company EQ cocode
                AND xeb.est-no  EQ vitemfg.est-no
                AND xeb.stock-no EQ vitemfg.i-no
                AND xeb.form-no NE 0 NO-LOCK NO-ERROR.
           IF AVAIL xeb THEN 
               FIND FIRST xef
               WHERE xef.company EQ cocode
               AND xef.est-no  EQ xeb.est-no
               AND xef.form-no EQ xeb.form-no NO-LOCK NO-ERROR. 
         
           IF AVAIL xef THEN
               ASSIGN
                    v-board = xef.board
                     .
           ELSE
            v-board = "".

            FIND FIRST item WHERE  item.company = cocode AND
                 item.i-no = v-board NO-LOCK NO-ERROR.
                IF AVAIL ITEM THEN do:
                    ASSIGN
/*                         vitem-name = ITEM.i-name */
                        v-pdscr1 = ITEM.i-dscr .
                    IF item.mat-type EQ "B" THEN
                        ASSIGN v-pdscr1 = ITEM.est-dscr .
                END.

        END.
        RUN addLines(1).
        put     oe-ordl.LINE AT 2  FORM ">99" SPACE(3)
                v-part FORMAT "X(15)"              
                vitem-name FORM "x(30)" SPACE(11)
                oe-ordl.qty FORM "->>>>>>>>>>>>>9" 
                oe-ordl.price  FORM "$->>>,>>9.99<<<<" SPACE(4)
                oe-ordl.pr-uom SPACE(2) 
                v-ext-price FORM "$->>>,>>9.99"              
                SKIP
                .
/*         v-printline = v-printline + 1. */
        
      
        /*
       if oe-ordl.i-no ne oe-ordl.part-no or
           oe-ordl.part-dscr1 ne ""        then do:
          v-part = if oe-ordl.i-no ne oe-ordl.part-no then oe-ordl.part-no
                   else "".
          put ov-part             at 2
              oe-ordl.part-dscr1  at 25  skip.
          v-printline = v-printline + 1.
        end. 
        if v-printline ge lv-line-print then
        do:
            PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
            PAGE .
            {oe/rep/acksoule.i}
            assign v-printline = 20.          
        end.
        if oe-ordl.po-no <> "" OR oe-ordl.part-dscr2 ne "" then do:
          put oe-ordl.po-no AT 2 oe-ordl.part-dscr2 at 25  skip.
          v-printline = v-printline + 1.
        end.
        */

        FIND FIRST quoteitm NO-LOCK
            WHERE quoteitm.company EQ oe-ordl.company
              /*AND quoteitm.i-no    EQ oe-ordl.i-no*/
              AND quoteitm.part-no = oe-ordl.part-no
              AND quoteitm.q-no    EQ v-q-no NO-ERROR.
        

        IF v-color = "" THEN DO:
            IF AVAIL quoteitm THEN
                v-color = string(quoteitm.i-coldscr) .
            ELSE IF AVAIL xeb THEN
                v-color = STRING(xeb.i-coldscr).
        END.

        IF v-color = "" THEN ASSIGN      /* task 12031306 */
            v-color = v-pdscr1
            v-pdscr1 = "" .

/*       if v-printline ge lv-line-print then                                                                       */
/*         do:                                                                                                      */
/*             PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" . */
/*             PAGE .                                                                                               */
/*             {oe/rep/acksoule.i}                                                                                  */
/*             assign v-printline = 20.                                                                             */
/*         end.                                                                                                     */
        
        RUN addLines(1).
        PUT oe-ordl.po-no AT 8
            v-color  /*v-pdscr1*/ FORM "x(30)" AT 23 SKIP.

/*         v-printline = v-printline + 1. */
        
        
        IF v-pdscr1 <> "" THEN do: 
/*            if v-printline ge lv-line-print then                                                                   */
/*            do:                                                                                                    */
/*              PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" . */
/*              PAGE .                                                                                               */
/*              {oe/rep/acksoule.i}                                                                                  */
/*              assign v-printline = 20.                                                                             */
/*            end.                                                                                                   */
           RUN addLines(1).
           PUT v-pdscr1 FORM "x(30)" AT 23  SKIP.
/*            v-printline = v-printline + 1. */
           
        END.

        IF oe-ordl.i-no  <> "" THEN do:  
/*            if v-printline ge lv-line-print then                                                                   */
/*            do:                                                                                                    */
/*              PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" . */
/*              PAGE .                                                                                               */
/*              {oe/rep/acksoule.i}                                                                                  */
/*              assign v-printline = 20.                                                                             */
/*            end.                                                                                                   */
            RUN addLines(1).
            PUT "FG#: " +  oe-ordl.i-no  FORM "x(30)" AT 23  SKIP.
/*            v-printline = v-printline + 1. */
           
        END.
        
        if v-print-components then
            for each fg-set
            where fg-set.company eq cocode
            and fg-set.set-no  eq oe-ordl.i-no
            no-lock,

        first xitemfg
            where xitemfg.company eq cocode
            and xitemfg.i-no    eq fg-set.part-no
            no-lock

            break by fg-set.set-no:

            FIND FIRST xeb
                WHERE xeb.company EQ est.company
                AND xeb.est-no  EQ est.est-no
                AND xeb.stock-no EQ fg-set.part-no
                AND xeb.form-no NE 0 NO-LOCK NO-ERROR.
           IF AVAIL xeb THEN 
               FIND FIRST xef
               WHERE xef.company EQ est.company
               AND xef.est-no  EQ est.est-no
               AND xef.form-no EQ xeb.form-no NO-LOCK NO-ERROR. 
           
            {sys/inc/part-qty.i v-part-qty fg-set}

/*                 if v-printline ge lv-line-print then                                                                     */
/*                     do:                                                                                                  */
/*                     PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" . */
/*                     PAGE .                                                                                               */
/*                     {oe/rep/acksoule.i}                                                                                  */
/*                         assign v-printline = 20.                                                                         */
/*                  end.                                                                                                    */
                     IF FIRST-OF(fg-set.set-no) THEN DO:
                        RUN addLines(1).
                        PUT /*SKIP(1)*/ "Kit Includes:" AT 23 SKIP.
                     END.

                RUN addLines(1).
                put 
                xitemfg.part-no FORMAT "x(15)" AT 8 
                fg-set.part-no                  AT 23
                xitemfg.i-name                        FORMAT "x(30)"
                oe-ordl.qty * v-part-qty        TO 78 FORMAT "->>>>>9"
                  .
                
                IF AVAIL xef AND xef.brd-dscr <> "" THEN do:
                    RUN addLines(1).
                    PUT xef.brd-dscr AT 23 .
/*                 v-printline = v-printline + 1. */
                END.

                /*IF LAST-OF(fg-set.set-no) THEN
                    PUT SKIP(1).*/
                RUN addLines(2).
/*             v-printline = v-printline + 2. */
        end.
        
        /* gdm - 05270902 */
        IF AVAIL eb AND AVAIL est THEN DO:        

          FIND FIRST quoteitm NO-LOCK
            WHERE quoteitm.company EQ oe-ordl.company
              AND quoteitm.i-no    EQ oe-ordl.i-no
              AND quoteitm.q-no    EQ v-q-no NO-ERROR.
          IF AVAIL quoteitm 
            THEN
             FIND FIRST style NO-LOCK  
               WHERE style.company EQ quoteitm.company
                 AND style.style   EQ quoteitm.style NO-ERROR.

             IF AVAIL style THEN ASSIGN style-dscr = style.dscr.
                            ELSE 
                             IF AVAIL quoteitm 
                              THEN ASSIGN style-dscr = quoteitm.style.
                              ELSE ASSIGN style-dscr = "".

          ASSIGN v-size = ""
                 v-size = TRIM(STRING(eb.len)) + " x " + 
                             TRIM(STRING(eb.wid)) + " x " + 
                             TRIM(STRING(eb.dep)).

         /* PUT v-size   AT 10
              style-dscr  AT 30 SKIP.

          v-printline = v-printline + 1.*/          

        /*  PUT   
           "DIE#: " + IF AVAIL eb THEN eb.die-no ELSE "" AT 10 FORM "x(23)"
           IF AVAIL quoteitm THEN quoteitm.i-coldscr  
                             ELSE ""  AT 30 FORM "x(30)" SKIP.

          ASSIGN v-printline = v-printline + 1.

          PUT 
           "CAD#: " + (IF AVAIL eb THEN eb.cad-no ELSE "") AT 10  FORM "x(23)" 
           IF AVAIL ef THEN ef.brd-dscr  ELSE ""  AT 30 FORMAT "x(25)" SKIP.

          ASSIGN v-printline = v-printline + 1.*/

         /* PUT 
           "FG#: " + (IF AVAIL eb THEN eb.stock-no ELSE "") AT 30 FORM "x(23)"
              SKIP.
          ASSIGN v-printline = v-printline + 1.*/
        END. 

/*         IF v-printline GE lv-line-print THEN DO:                                                                 */
/*             PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" . */
/*             PAGE .                                                                                               */
/*             {oe/rep/acksoule.i}                                                                                  */
/*             ASSIGN v-printline = 20.                                                                             */
/*         END.                                                                                                     */
        /* gdm - 05270902 end */

        FOR EACH oe-rel
            where oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
              AND ((oe-rel.link-no EQ 0 AND v-schrel) OR
                   (oe-rel.link-no NE 0 AND v-actrel))
            NO-LOCK BREAK BY oe-rel.link-no DESC WITH FRAME sched-rel DOWN:

          if first-of(oe-rel.link-no) then
            if oe-rel.link-no eq 0 then lcnt = 1.
            else
            if first(oe-rel.link-no) then lcnt = 1.
         /*
          if v-printline ge lv-line-print then
          do:
            PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
            PAGE .
            {oe/rep/acksoule.i}
            assign v-printline = 20.          
          end.
          */
          if first-of(oe-rel.link-no) then do:
            if oe-rel.link-no eq 0 then do:
/*               if v-printline ge lv-line-print then                                                                    */
/*               do:                                                                                                     */
/*                  PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" . */
/*                  PAGE .                                                                                               */
/*                  {oe/rep/acksoule.i}                                                                                  */
/*                  assign v-printline = 20.                                                                             */
/*               end.                                                                                                    */
              RUN addLines(1).
              put "Scheduled Releases:" at 23 SKIP .
/*               v-printline = v-printline + 1. */
              
            end.
            else
            if first(oe-rel.link-no) then do:
/*               if v-printline ge lv-line-print then                                                                    */
/*               do:                                                                                                     */
/*                  PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" . */
/*                  PAGE .                                                                                               */
/*                  {oe/rep/acksoule.i}                                                                                  */
/*                  assign v-printline = 20.                                                                             */
/*               end.                                                                                                    */
              RUN addLines(1).
              put "Actual Releases:" at 23 SKIP. 
              
/*               v-printline = v-printline + 1. */
            end.
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
                ASSIGN
                v-addr4 = shipto.ship-city + ", " + shipto.ship-state + "  " + shipto.ship-zip
                v-ship-name = shipto.ship-name
                v-ship-add  = shipto.ship-addr[1]
                v-ship-add2 = shipto.ship-addr[2]
                v-contact   = shipto.contact + " " + 
                    STRING(shipto.area-code,"(999)") + " " + 
                    STRING(shipto.phone,"999-9999")
                .
      
/*             if v-printline ge lv-line-print  then */
/*             do:                                                                                                     */
/*                PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" . */
/*                PAGE .                                                                                               */
/*                {oe/rep/acksoule.i}                                                                                  */
/*                assign v-printline = 20.                                                                             */
/*             end.                                                                                                    */
            IF AVAIL shipto THEN DO:
                RUN addLines(1).
                put oe-rel.po-no AT 8 shipto.ship-name AT 23 FORM "x(28)" /*lcnt AT 55 FORM ">>9" SPACE(1)*/  ld-date AT 59 space(2) (IF oe-rel.link-no EQ 0 THEN oe-rel.tot-qty ELSE oe-rel.qty) FORM "->>>>>>>>9"
                    SKIP .
                    IF  v-ship-name <> "" THEN DO:
                         RUN addLines(1).
                        PUT v-ship-name AT 8 SKIP.
                    END.
                    IF  v-ship-add <> "" THEN DO:
                         RUN addLines(1).
                        PUT v-ship-add AT 8 SKIP.
                    END.
                    IF  v-ship-add2 <> "" THEN DO:
                         RUN addLines(1).
                        PUT v-ship-add2 AT 8 SKIP.
                    END.
                    IF  v-addr4 <> "" THEN DO:
                         RUN addLines(1).
                        PUT v-addr4 AT 8 SKIP.
                    END.
                    /* IF shipto.ship-addr[1] <> "" THEN DO:
                   if v-printline ge lv-line-print then
                   do:
                     PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                     PAGE .
                     {oe/rep/acksoule.i}
                     assign v-printline = 20.          
                   end.
                   PUT shipto.ship-addr[1] AT 30  SKIP.
                   v-printline = v-printline + 1.
          
                END.*/
               /* IF shipto.ship-addr[2] <> "" THEN DO:
                    if v-printline ge lv-line-print then
                    do:
                      PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                      PAGE .
                      {oe/rep/acksoule.i}
                      assign v-printline = 20.          
                    end. 
                    PUT shipto.ship-addr[2] AT 30 SKIP.
                    v-printline = v-printline + 1.
                    /*if v-printline ge lv-line-print then
                    do:
                        PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                        PAGE .
                        {oe/rep/acksoule.i}
                        assign v-printline = 20.          
                    end. */
                END.*/
               /* IF v-addr4 <> "" THEN DO:
                   if v-printline ge lv-line-print then
                   do:
                      PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                      PAGE .
                      {oe/rep/acksoule.i}
                      assign v-printline = 20.          
                   end. 
                   PUT v-addr4 AT 30  SKIP.
                   v-printline = v-printline + 1.
                END.*/
            END.
          end.
          ELSE DO:
/*               if v-printline ge lv-line-print  then                                                                  */
/*               do:                                                                                                    */
/*                 PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" . */
/*                 PAGE .                                                                                               */
/*                 {oe/rep/acksoule.i}                                                                                  */
/*                 assign v-printline = 20.                                                                             */
/*               end.                                                                                                   */
              RUN addLines(1).
              put oe-rel.po-no AT 8  ld-date AT 59 SPACE(2) (IF oe-rel.link-no EQ 0 THEN oe-rel.tot-qty ELSE oe-rel.qty) FORM "->>>>>>>>9"     SKIP .
          END.
          PUT SKIP(1).
/*           v-printline = v-printline + 1. */
          RUN addLines(1).
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
/*  
        if v-printline ge lv-line-print then
        do:
            PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
            PAGE .
            {oe/rep/acksoule.i}
            assign v-printline = 20.          
        end.
  */
     IF v-prntinst THEN DO:
                   /* print spec notes */
        find first itemfg {sys/look/itemfgrlW.i}
             and itemfg.i-no eq oe-ordl.i-no no-lock no-error.

        IF AVAIL itemfg THEN DO:
           lv-first-note = yes.         
           {custom/notesprt.i itemfg v-inst 4}
           
           DO i = 1 TO 4:
              IF v-inst[i] <> "" THEN DO:
/*                  if v-printline ge lv-line-print then                                                                   */
/*                  do:                                                                                                    */
/*                    PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" . */
/*                    PAGE .                                                                                               */
/*                    {oe/rep/acksoule.i}                                                                                  */
/*                    assign v-printline = 20.                                                                             */
/*                  end.                                                                                                   */
                 IF lv-first-note THEN do:
                    PUT SKIP(1).
/*                     v-printline = v-printline + 1. */
                    RUN addLines(1).
                    lv-first-note = NO.
                 END.
                 RUN addLines(1).
                 PUT v-inst[i] SKIP.
/*                  v-printline = v-printline + 1. */

              END.           
           END.
           IF NOT lv-first-note THEN do:
              PUT SKIP(1).
/*               v-printline = v-printline + 1. */
              RUN addLines(1).
           END.
        END. 
     END. /* v-prntinst */
              PUT SKIP(1).
/*               v-printline = v-printline + 1. */
              RUN addLines(1).
   end. /* each oe-ordl */

      for each oe-ordm no-lock where oe-ordm.company eq oe-ord.company and
          oe-ordm.ord-no eq oe-ord.ord-no break BY ord-no:

/*           if v-printline ge lv-line-print then                                                                     */
/*           do:                                                                                                      */
/*               PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" . */
/*               PAGE .                                                                                               */
/*               {oe/rep/acksoule.i}                                                                                  */
/*               assign v-printline = 20.                                                                             */
/*           end.                                                                                                     */

        if first(oe-ordm.ord-no) then
        do:
          
          put "** Miscellaneous Items **" at 25 .
          if v-print-fmt eq "HOP" then put "Taxable" at 62.
          put skip(1).
          RUN addLines(2).
          /*assign v-printline = v-printline + 2.*/
          
        end.
/*         if v-printline ge lv-line-print then                                                                     */
/*         do:                                                                                                      */
/*             PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" . */
/*             PAGE .                                                                                               */
/*             {oe/rep/acksoule.i}                                                                                  */
/*             assign v-printline = 20.                                                                             */
/*         end.                                                                                                     */
        if oe-ordm.bill eq "N" then
           PUT oe-ordm.po-no FORM "x(15)" AT 8 
               oe-ordm.charge AT 23 oe-ordm.dscr FORM "x(30)" "      N/C" .          
        else
         PUT oe-ordm.po-no FORM "x(15)" AT 8 
             oe-ordm.charge AT 23 oe-ordm.dscr FORM "x(30)" /*SPACE(11)*/             
             oe-ordm.amt FORM "$->>>>,>>9.99" AT 100 
             .
        PUT SKIP.
        assign v-line = v-line + 1.
        RUN addLines(1).
/*           v-printline = v-printline + 1. */
       
        if oe-ordm.bill ne "N" THEN assign v-totord = v-totord + oe-ordm.amt.
      end. /* each oe-ordm */
           
      /* print freight*/
      IF oe-ord.f-bill THEN DO:
         v-totord = v-totord + oe-ord.t-freight.
         PUT "Freight:" AT 23 oe-ord.t-freight FORM "$->>>,>>9.99" AT 101.
         RUN addLines(1).
/*          v-printline = v-printline + 1. */
      END.
      /* print billing notes */
      ASSIGN v-billinst = "".

      IF v-prntinst THEN 
      DO i = 1 TO 4:
          v-billinst[i] = oe-ord.bill-i[i].
      END.      
 /*     
      if v-printline ge lv-line-print then
      do:
            PUT "<R59><C65>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
            PAGE .
            {oe/rep/acksoule.i}
            assign v-printline = 20.          
      end.
*/
      PUT "<R53><C1> A 10% underrun or overrun will constitute fulfillment of order.  " 
          "<R54><C1> If any items are returned, a 15% restocking fee will be charged on all returned items. " 
          "<R55><C1> No return will be accepted for custom orders. " .


      PUT "<R56><C1><#10><P12><B> Comments </B> <P10> " 
        "<R57><C1>" v-billinst[1] 
        "<R58><C1>" v-billinst[2] 
        "<R59><C1>" v-billinst[3]   "<P10>Order Total:" AT 90 "<U>"  v-totord FORM "$>>>>,>>9.99"  "</U><P9>"               
        "<R60><C1>" v-billinst[4]   
        /*"<R61><C1>" " ______________________________________(Please sign and fax back) " */
        "<R62><C1>" SPACE(32) "<P9><B>THIS IS A CONFIRMATION OF YOUR ORDER,NOT AN INVOICE.</B>                   <P16><B> Thank You! <P10></B>" 
/*         "<R64><C65>Page "  string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" */
        "[@endPage" oe-ord.ord-no "]"
        .
        RUN printPageNumLine.
      assign
       v-totord        = 0
       oe-ord.ack-prnt = yes.

      v-printline = v-printline + 6.
      lv-pg-num = PAGE-NUM .

      IF v-printline <= 66 THEN page. /*PUT SKIP(60 - v-printline). */

    end. /* each oe-ord */

PROCEDURE testNewPage:
    IF v-printline GE lv-line-print  THEN
    DO:
        RUN printPageNumLine.
/*         PUT "<R64><C65>Page " STRING(PAGE-NUM - lv-pg-num,">>9") + " of " + STRING(lv-tot-pg) FORM "x(20)". */
        PAGE .
        {oe/rep/acksoule.i}
        ASSIGN v-printline = lv-line-start.          
    END.

END PROCEDURE.

PROCEDURE addLines:
    DEFINE INPUT PARAMETER ipiNumLines AS INTEGER NO-UNDO.
    
    v-printline = v-printline + ipiNumLines.
    RUN testNewPage.

END PROCEDURE.
PROCEDURE printPageNumLine:
    PUT "<R64><C65>Page " + STRING(PAGE-NUM - lv-pg-num,">>9") + " of [=@endPage" + string(oe-ord.ord-no) + "-@startPage" + string(oe-ord.ord-no) + "+1] " FORM "x(120)" .
END PROCEDURE.

/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */
