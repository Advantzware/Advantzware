/* ----------------------------------------------- oe/rep/ackpacif.p 10/10 YSK */
/* ORDER ACKNOLEDGEMENT                                                       */
/* for ORDER STATUS = (R), (U)                                                */
/* -------------------------------------------------------------------------- */

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
def var v-ackhead as char format "x(32)" init
  "A C K N O W L E D G E M E N T" NO-UNDO.
def var v-len as int NO-UNDO.
def var v-totord as dec format "->>,>>>,>>9.99" NO-UNDO.
def var dSubtotord as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-totlin as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-ans as log init no NO-UNDO.
def var lcnt as int init 1 NO-UNDO.
def var pagebreak as int init 28 NO-UNDO.
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
/*ASSIGN ls-image1 = "images\pacific1.bmp"
       ls-image2 = "images\pacific2.bmp".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
FILE-INFO:FILE-NAME = ls-image2.
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".
*/
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
DEF VAR lv-email AS cha FORM "x(40)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR lv-line-print AS INT INIT 44 NO-UNDO.
DEF VAR lv-due-date AS DATE NO-UNDO.
DEFINE VARIABLE v-ship-name AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE var v-ship-add as char format "x(30)" NO-UNDO.
DEF VAR v-ship-add2 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-ship-add3 AS CHAR FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEF VAR v-ext-price AS DEC NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).

ASSIGN ls-full-img1 = cRtnChar + ">" .

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
      FIRST oe-ord WHERE RECID(oe-ord) EQ report.rec-id:

      if oe-ord.sman[2] eq "" and oe-ord.sman[3] eq "" then
        v-salesman = oe-ord.sman[1].
      else
        v-salesman = oe-ord.sman[1] + oe-ord.sman[2] + oe-ord.sman[3].
        IF oe-ord.sman[1] NE "" THEN
            v-salesman = oe-ord.sman[1].
        ELSE IF oe-ord.sman[2] NE "" THEN
            v-salesman = oe-ord.sman[2].
        ELSE IF oe-ord.sman[3] NE "" THEN
            v-salesman = oe-ord.sman[3].

        FIND FIRST sman NO-LOCK 
            WHERE sman.company = cocode
            AND sman.sman EQ v-salesman NO-ERROR.
        IF AVAIL sman THEN
           v-salesman = sman.sname.

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

      IF AVAIL oe-ordl AND oe-ordl.est-no <> "" THEN DO:
         FIND last quotehd WHERE quotehd.company = oe-ordl.company
                              AND quotehd.est-no = oe-ordl.est-no
                              NO-LOCK NO-ERROR.
         IF AVAIL quotehd THEN DO:
            FIND FIRST quoteitm OF quotehd WHERE quoteitm.part-no = oe-ordl.part-no
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
      lv-due-date = IF AVAIL oe-ordl THEN oe-ordl.req-date ELSE oe-ord.due-date.
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
               v-ship-add3 = shipto.ship-city + ", " +
                shipto.ship-state + "  " + shipto.ship-zip.
        END.
/*         IF shipto.ship-addr[1] <> "" THEN ln-cnt = ln-cnt + 1. */
/*         IF shipto.ship-addr[2] <> "" THEN ln-cnt = ln-cnt + 1. */
/*         IF v-addr4 <> "" THEN ln-cnt = ln-cnt + 1.             */
      END.
      /*
      format header
          "ACKNOWLEDGEMENT" at 62 skip
          "Order No." at 60 "Order Date" at 70 skip
          "---------" at 60 "----------" at 70 skip
          oe-ord.ord-no at 61
          oe-ord.ord-date at 72 FORMAT "99/99/99" skip
          company.name at 10 skip
           company.addr[1] at 10 "Salesman:" at 61 v-salesman at 71 skip
          company.addr[2] at 10 skip
          company.city at 10 company.state company.zip "Customer PO#" at 61 skip
          "------------" at 61 skip
          oe-ord.po-no at 61 skip(2)
          "F.O.B." at 4 "Terms" at 32 "Ship VIA" at 58 skip
          "------" at 4 "-----" at 32 "--------" at 58 skip
          v-fob at 4 oe-ord.terms-d at 32 v-shipvia at 58 format "x(22)" skip(2)
          "Bill To:" at 5 "Sold To:" at 49 skip
          "--------" at 5 "--------" at 49 skip
          oe-ord.cust-name at 10 oe-ord.sold-name    at 54 skip
          oe-ord.addr[1]   at 10 oe-ord.sold-addr[1] at 54 skip
          oe-ord.addr[2]   at 10 oe-ord.sold-addr[2] at 54 skip
          v-addr3          at 10 v-sold-addr3        at 54 skip
          v-cust-phone   at 10 skip(1)
          "No." at 1 "Item Number" at 5 "Description" at 22
          "Ordered" at 60 "Price Per M" to 80 skip
          FILL("-",80) FORMAT "x(80)"
          with frame ackhead-head page-top no-labels no-box
                no-underline stream-io width 90.
       */
{oe/rep/ackshamrock.i}

      for each oe-ordl
          where oe-ordl.company eq oe-ord.company
            and oe-ordl.ord-no  eq oe-ord.ord-no
          no-lock:
        if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackshamrock.i}
            assign v-printline = 20.          
        end.
    ASSIGN v-ext-price = oe-ordl.t-price.

        put "<P9>" v-line FORM ">>>9" SPACE(3)
                oe-ordl.i-no  SPACE(4)             
                oe-ordl.i-name SPACE(1)
                oe-ordl.qty SPACE(1)
                oe-ordl.price  FORM "->,>>>,>>9.99<<<<" SPACE(2)
                oe-ordl.pr-uom SPACE(2)
                v-ext-price FORM "->,>>>,>>9.99" SKIP
            .
        v-printline = v-printline + 1.
       if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackshamrock.i}
            assign v-printline = 20.          
        end.
       if oe-ordl.i-no ne oe-ordl.part-no or
           oe-ordl.part-dscr1 ne ""        then do:
          v-part = if oe-ordl.i-no ne oe-ordl.part-no then oe-ordl.part-no
                   else "".
          put v-part             at 8
              oe-ordl.part-dscr1  at 27  skip.
          v-printline = v-printline + 1.
        end. 
        if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackshamrock.i}
            assign v-printline = 20.          
        end.
        if oe-ordl.part-dscr2 ne "" then do:
          put oe-ordl.part-dscr2 at 27  skip.
          v-printline = v-printline + 1.
        end.
        if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackshamrock.i}
            assign v-printline = 20.          
        end.

       /* IF v-schrel THEN
        for each oe-rel
            where oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
              AND oe-rel.link-no EQ 0
            no-lock break by oe-rel.rel-date:

          RUN print-rels (FIRST(oe-rel.rel-date)).
        end.   /* for each oe-rel  */

        IF v-actrel THEN
        for each oe-rel
            where oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
              AND oe-rel.link-no NE 0
            no-lock break by oe-rel.rel-date:

          RUN print-rels (FIRST(oe-rel.rel-date)).
        end.   /* for each oe-rel  */ */

        v-line = v-line + 1.

      /*  put "" skip.
        assign
         v-line = v-line + 1
         v-printline = v-printline + 1.
      */
        if v-printline GT lv-line-print then
        do:
            PAGE .
            {oe/rep/ackshamrock.i}
            assign v-printline = 20.          
        end.

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
  
        if v-printline GT lv-line-print then
        do:
            PAGE .
            {oe/rep/ackshamrock.i}
            assign v-printline = 20.          
        end.
           /* print spec notes */
        IF v-prntinst THEN DO:
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
                 if v-printline GT lv-line-print then
                 do:
                    PAGE .
                    {oe/rep/ackshamrock.i}
                    assign v-printline = 20.          
                 end.
              END.
              IF NOT lv-first-note THEN do:
                 PUT SKIP(1).
                 v-printline = v-printline + 1.           
              END.     
           END.
        END.  /* if v-prntinst*/

      end. /* each oe-ordl */

      for each oe-ordm no-lock where oe-ordm.company eq oe-ord.company and
          oe-ordm.ord-no eq oe-ord.ord-no break BY ord-no:

          if v-printline ge lv-line-print then
          do:
              PAGE .
              {oe/rep/ackshamrock.i}
              assign v-printline = 20.          
          end.

        if first(oe-ordm.ord-no) then
        do:
          put "** Miscellaneous Items **" at 23.
          if v-print-fmt eq "HOP" then put "Taxable" at 62.
          put skip(1).
          assign v-printline = v-printline + 2.
        end.
        if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackshamrock.i}
            assign v-printline = 20.          
        end.
        if oe-ordm.bill eq "N" then
           PUT v-line FORM ">>>9" SPACE(3)
               oe-ordm.charge oe-ordm.dscr "      N/C" .          
        else
         PUT v-line FORM ">>>9" space(3)
             oe-ordm.charge oe-ordm.dscr SPACE(11)
             oe-ordm.amt
             .
        PUT SKIP.
        assign v-line = v-line + 1
          v-printline = v-printline + 2.   
        
        if oe-ordm.bill ne "N" THEN assign v-totord = v-totord + oe-ordm.amt.
      end. /* each oe-ordm */
           
      /* print billing notes */
      ASSIGN v-billinst = "".

      IF v-prntinst THEN 
      DO i = 1 TO 4:
      
          v-billinst[i] = oe-ord.bill-i[i].
      END.      
       dSubtotord = v-totord.
      v-totord = v-totord + oe-ord.tax.

/*
      if oe-ctrl.p-ack then do:
        put "Total Order Value:" to 65 v-totord skip.
        v-printline = v-printline + 1.
      end.
*/      
/*      
      put skip(pagebreak - v-printline - 1)
          space(32)
          "THIS IS A CONFIRMATION OF YOUR ORDER,NOT AN INVOICE."
          skip.
*/
      /*if v-printline ge lv-line-print then
      do:
            PAGE .
            {oe/rep/ackshamrock.i}
            assign v-printline = 20.          
      end.*/
      PUT "<|10><R50><C52><#15><FROM><R55><C80><RECT>" SKIP  .
      PUT "<=15><R+1><P10><C55><B>Order sub-total: </B>" dSubtotord FORM ">>>>,>>9.99" .
      PUT  "<=15><R+2><P10><C60><B>Sales Tax:   </B>" oe-ord.tax FORM "->,>>9.99"
           "<=15><R+3><C58.2><B>Order Total: </B>" v-totord FORM ">>>>,>>9.99" SKIP(2).
      PUT "<R53><C70><FROM><R53><C78><LINE>" .
    assign
        dSubtotord = 0
       v-totord        = 0
       oe-ord.ack-prnt = yes.
/*       
      PUT "<FArial><R56><C1><#10><P12><B> Comments </B> <P8> " SKIP        
        v-billinst[1] SKIP
        v-billinst[2] SKIP
        v-billinst[3] SKIP
        v-billinst[4] SKIP(1)
        " ______________________________________(Please sign and fax back) " SKIP
        "<=10><R-3>" SPACE(32) "<P9><B>THIS IS A CONFIRMATION OF YOUR ORDER,NOT AN INVOICE.</B>" .
*/      
      PUT "<FArial><R55><C1><#10><P12> " 
        "<=10><R+1>" SPACE(32) "<P10><B>* This is a confirmation of receipt of order. If the requested delivery date cannot be met, a</B>" 
        "<=10><R+2>" SPACE(32) "<P10><B> notification will be sent. The seller does not agree to furnish the exact quantity ordered and shall</B>"
        "<=10><R+3>" SPACE(32) "<P10><B>                                            be allowed a variation of 10% over/underrun.</B>" 
        "<=10><R+5>" SPACE(32) "<P12><B>                                                   Thank You</B>"      .
      v-printline = v-printline + 6. 
      IF v-printline <= 66 THEN page. /*PUT SKIP(60 - v-printline). */

    end. /* each oe-ord */

RETURN.

PROCEDURE print-rels:
    DEF INPUT PARAM ip-first AS LOG NO-UNDO.


    DO WITH FRAME sched-rel DOWN:
          if v-printline GT lv-line-print then
          do:
            PAGE .
            {oe/rep/ackshamrock.i}
            assign v-printline = 20.          
          end.

          if ip-first then do:
            lcnt = 1.
            if oe-rel.link-no eq 0 then do:
              put "Scheduled Releases:" at 10  skip.
              v-printline = v-printline + 1.
            end.
            ELSE do:
              put "Actual Releases:" at 10 skip.
              v-printline = v-printline + 1.
            end.
          end.
          if v-printline GT lv-line-print then
          do:
            PAGE .
            {oe/rep/ackshamrock.i}
            assign v-printline = 20.          
          end.
          {oe/rel-stat.i lv-stat}
          IF AVAIL oe-rell THEN
          FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
          ld-date = IF AVAIL oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date.
          if oe-rel.link-no eq 0 then 
              put lcnt AT 10 oe-rel.tot-qty space(5) ld-date  SKIP.
          ELSE
              put lcnt AT 10 oe-rel.qty space(5) ld-date  SKIP.


          ASSIGN 
              v-printline = v-printline + 1
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
      
            if v-printline GT lv-line-print  then
            do:
               PAGE .
               {oe/rep/ackshamrock.i}
               assign v-printline = 20.          
            end.
            IF AVAIL shipto THEN DO:
                put shipto.ship-name AT 10 SKIP .
                v-printline = v-printline + 1.
                if v-printline GT lv-line-print then
                do:
                    PAGE .
                    {oe/rep/ackshamrock.i}
                    assign v-printline = 20.          
                end.
                IF shipto.ship-addr[1] <> "" THEN DO:
                   PUT shipto.ship-addr[1] AT 10  SKIP.
                   v-printline = v-printline + 1.
                   if v-printline GT lv-line-print then
                   do:
                      PAGE .
                      {oe/rep/ackshamrock.i}
                      assign v-printline = 20.          
                   end.
                END.
                IF shipto.ship-addr[2] <> "" THEN DO:
                    PUT shipto.ship-addr[2] AT 10  SKIP.
                    v-printline = v-printline + 1.
                    if v-printline GT lv-line-print then
                    do:
                        PAGE .
                        {oe/rep/ackshamrock.i}
                        assign v-printline = 20.          
                    end.
                END.
                IF v-addr4 <> "" THEN DO:
                   PUT v-addr4 AT 10 SKIP.
                   v-printline = v-printline + 1.
                END.
                
            END.
          end.
    END.
END PROCEDURE

/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */
