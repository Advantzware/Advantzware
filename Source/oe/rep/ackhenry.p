/* ----------------------------------- oe/rep/ackhenry.p 01/22 Sachin Cahahal */
/* ORDER ACKNOLEDGEMENT                                                       */
/* for ORDER STATUS = (R), (U)                                                */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/acknowl.i}
{custom/formtext.i NEW}

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
DEFINE VARIABLE dQty LIKE oe-ordl.qty NO-UNDO.
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
DEF VAR v-ship-name AS CHAR format "x(30)" NO-UNDO.
DEF VAR v-ship-add AS CHAR format "x(30)" EXTENT 3 NO-UNDO.
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR cShipNotes AS CHAR FORM "x(70)" EXTENT 4 NO-UNDO.
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
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE dSetItemQty AS DECIMAL NO-UNDO .
DEFINE BUFFER bf-shipto FOR shipto .
DEFINE BUFFER bf-oe-rel FOR oe-rel .
DEFINE VARIABLE lValid         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTotalQty      AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-cas-cnt      AS INTEGER NO-UNDO.
DEFINE VARIABLE v-blank        AS INTEGER NO-UNDO.
DEF VAR lv-text        AS CHARACTER                     NO-UNDO.
DEF VAR li             AS INTEGER                       NO-UNDO.
DEF VAR cShiptoAddress AS CHAR FORMAT "x(80)"  EXTENT 4 NO-UNDO.

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

      RUN oe/custxship.p (oe-ord.company,
                        oe-ord.cust-no,
                        oe-ord.ship-id,
                        BUFFER bf-shipto).
      IF AVAIL bf-shipto AND bf-shipto.broker then DO:
       ASSIGN
          lv-comp-name = oe-ord.sold-name
          v-comp-add1 = oe-ord.sold-addr[1]
          v-comp-add2 = oe-ord.sold-addr[2]
          v-comp-add3 =  oe-ord.sold-city + ", " + oe-ord.sold-state + "  " + oe-ord.sold-zip
          v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999")
          v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
          lv-email    = "Email:  " + cust.email  .

      END.
      
      IF AVAIL bf-shipto THEN
      DO:
        ASSIGN
            v-ship-name   = bf-shipto.ship-name
            v-ship-add[1] = bf-shipto.ship-addr[1]
            v-ship-add[2] = bf-shipto.ship-addr[2]
            v-ship-add[3] = bf-shipto.ship-city + ", " +
                            bf-shipto.ship-state + "  " + bf-shipto.ship-zip
                            .
          
      END.

{oe/rep/ackhenry.i}
      dSetItemQty = 0 .
      for each oe-ordl
          where oe-ordl.company eq oe-ord.company
            and oe-ordl.ord-no  eq oe-ord.ord-no
          no-lock:
        if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackhenry.i}
            assign v-printline = 20.          
        end.

        ASSIGN dQty = oe-ordl.qty .

        find first eb
            where eb.company  eq oe-ordl.company
            and eb.est-no   eq oe-ordl.est-no
            and eb.form-no  eq 0
            no-lock no-error.

        if AVAILABLE eb AND (eb.est-type eq 2 or eb.est-type eq 6) then do:
            IF eb.stock-no EQ oe-ordl.i-no THEN
                dSetItemQty = oe-ordl.qty .
            for each fg-set
                where fg-set.company eq oe-ordl.company
                and fg-set.part-no  eq oe-ordl.i-no
                AND fg-set.set-no EQ eb.stock-no
                no-lock:
                dQty = dSetItemQty * fg-set.QtyPerSet.
            END.
        END.

        IF v-print-fmt eq "Henry" THEN
        DO:
        
            IF AVAILABLE oe-ordl THEN 
            DO:
                IF oe-ordl.pr-uom EQ "L" THEN
                    xxx = oe-ordl.price.
                ELSE
                    IF oe-ordl.pr-uom EQ "CS" THEN
                    DO:
                        find first itemfg WHERE
                            itemfg.company EQ quoteqty.company AND
                            itemfg.i-no eq eb.stock-no
                            NO-LOCK NO-ERROR.
                            
                        IF (AVAILABLE eb AND eb.cas-no NE "") OR AVAILABLE eb THEN
                            RUN est/getcscnt.p ((IF AVAILABLE eb AND
                                eb.cas-no NE "" THEN ROWID(eb) ELSE ROWID(eb)),
                                OUTPUT v-cas-cnt,OUTPUT v-blank).
                        ELSE
                            v-cas-cnt = 0.

                        xxx = oe-ordl.qty / (IF v-cas-cnt ne 0 then v-cas-cnt else
                            if available itemfg and itemfg.case-count ne 0 THEN
                            itemfg.case-count ELSE 1) * oe-ordl.price.
                    END.
                    ELSE
                        IF oe-ordl.pr-uom EQ "C" THEN
                            xxx = ((oe-ordl.qty / 100) * oe-ordl.price).
                        ELSE
                            IF oe-ordl.pr-uom EQ "M" THEN
                                xxx = ((oe-ordl.qty / 1000) * oe-ordl.price).
                            ELSE
                                xxx = (oe-ordl.qty * oe-ordl.price).
                                
               END.  /* IF AVAILABLE oe-ordl THEN  */
               
               put v-line FORM ">>>9" SPACE(3)
                    oe-ordl.i-no  SPACE(1)             
                    oe-ordl.i-name  
                    "<C45>"oe-ordl.qty /*dQty*/
                    "<C54>" oe-ordl.price  FORM "->,>>>,>>9.99<<<<" 
                    "<C66.5>"oe-ordl.pr-uom  
                    "<C71.5>"XXX SKIP
                .
           
        END.  /*IF v-print-fmt eq "Henry" THEN*/
        ELSE DO:
        
        put v-line FORM ">>>9" SPACE(3)
                oe-ordl.i-no  SPACE(2)             
                oe-ordl.i-name SPACE(2)
                oe-ordl.qty /*dQty*/ SPACE(2)
                oe-ordl.price  FORM "->,>>>,>>9.99<<<<" SPACE(5)
                oe-ordl.pr-uom  SKIP
            .
         END.
         
        v-printline = v-printline + 1.
       if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackhenry.i}
            assign v-printline = 20.          
        end.
       if oe-ordl.i-no ne oe-ordl.part-no or
           oe-ordl.part-dscr1 ne ""        then do:
          v-part = if oe-ordl.i-no ne oe-ordl.part-no then oe-ordl.part-no
                   else "".
          put v-part             at 8
              oe-ordl.part-dscr1  at 25  skip.
          v-printline = v-printline + 1.
        end. 
        if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackhenry.i}
            assign v-printline = 20.          
        end.
        if oe-ordl.part-dscr2 ne "" then do:
          put oe-ordl.part-dscr2 at 25  skip.
          v-printline = v-printline + 1.
        end.
        if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackhenry.i}
            assign v-printline = 20.          
        end.
        if oe-ordl.po-no ne "" AND oe-ordl.po-no NE oe-ord.po-no  then do:
          PUT "Item Po#: " + oe-ordl.po-no FORMAT "x(25)" at 25  skip.
          v-printline = v-printline + 1.
        end.
        if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackhenry.i}
            assign v-printline = 20.          
        end.

        IF v-schrel THEN
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
        end.   /* for each oe-rel  */

        v-line = v-line + 1.

      /*  put "" skip.
        assign
         v-line = v-line + 1
         v-printline = v-printline + 1.
      */
        if v-printline GT lv-line-print then
        do:
            PAGE .
            {oe/rep/ackhenry.i}
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
            {oe/rep/ackhenry.i}
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
                    {oe/rep/ackhenry.i}
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
              {oe/rep/ackhenry.i}
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
            {oe/rep/ackhenry.i}
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
           
      /* print ship notes */
      
      FIND first oe-ordl where oe-ordl.company eq oe-ord.company
                           and oe-ordl.ord-no  eq oe-ord.ord-no
                           NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN
      FIND FIRST bf-oe-rel NO-LOCK
          WHERE bf-oe-rel.company = oe-ordl.company 
          AND bf-oe-rel.ord-no = oe-ordl.ord-no
          AND bf-oe-rel.i-no = oe-ordl.i-no 
          AND bf-oe-rel.line = oe-ordl.LINE
          NO-ERROR.
      
      ASSIGN cShipNotes = "".

      IF v-prntinst THEN 
      DO i = 1 TO 4:
      
          cShipNotes[i] = IF AVAIL bf-oe-rel THEN bf-oe-rel.ship-i[i] ELSE "".
      END.      
     
      

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
            {oe/rep/ackhenry.i}
            assign v-printline = 20.          
      end.*/

      assign
       v-totord        = 0
       oe-ord.ack-prnt = yes.
      
      PUT "<FArial><R55><C1><#10><P12><B> Comments </B> <P8> " 
        "<R56><C2>" cShipNotes[1] 
        "<R57><C2>" cShipNotes[2] 
        "<R58><C2>" cShipNotes[3] 
        "<R59><C2>" cShipNotes[4] 
        //"<R61><C1>" " ______________________________________(Please sign and fax back) " 
        "<=10><R-2>" SPACE(32) "<P9><B>THIS IS A CONFIRMATION OF YOUR ORDER,NOT AN INVOICE.</B>" .
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
            {oe/rep/ackhenry.i}
            assign v-printline = 20.          
          end.

          if ip-first then do:
            lcnt = 1.
            if oe-rel.link-no eq 0 then do:
              PUT SKIP(1) "Scheduled Releases:" at 10  skip.
              v-printline = v-printline + 1.
            end.
            ELSE do:
              PUT SKIP(1) "Actual Releases:" at 10 skip.
              v-printline = v-printline + 1.
            end.
          end.
          if v-printline GT lv-line-print then
          do:
            PAGE .
            {oe/rep/ackhenry.i}
            assign v-printline = 20.          
          end.
          {oe/rel-stat.i lv-stat}
          IF AVAIL oe-rell THEN
          FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
          ld-date = IF AVAIL oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date.
          iTotalQty = if oe-rel.link-no eq 0 THEN oe-rel.tot-qty ELSE oe-rel.qty.
          
              put lcnt AT 7 iTotalQty space(3) ld-date SPACE(1).
              IF oe-rel.po-no NE "" AND oe-rel.po-no NE oe-ordl.po-no THEN
              PUT "Release PO#:" oe-rel.po-no FORMAT "x(15)" .
              IF oe-rel.lot-no NE "" THEN
              PUT space(1) "Customer Lot#: " oe-rel.lot-no FORMAT "x(15)".
              PUT SKIP.
           

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
               {oe/rep/ackhenry.i}
               assign v-printline = 20.          
            end.
            IF AVAIL shipto THEN DO:
                FOR EACH tt-formtext:
                    DELETE tt-formtext.
                END.
                
                ASSIGN lv-text = trim(shipto.ship-name) + " " + trim(shipto.ship-addr[1]) + " " + trim(shipto.ship-addr[2]) + " " + v-addr4 .
                
                DO li = 1 TO 4:
                  CREATE tt-formtext.
                  ASSIGN tt-line-no = li
                         tt-length  = 65. 
                END.

                RUN custom/formtext.p (lv-text).
                
                ASSIGN i = 0.
                PUT "Deliver to:" AT 10.
                FOR EACH tt-formtext:
                  i = i + 1.
                  IF  i <= 4 THEN cShiptoAddress[i] = tt-formtext.tt-text.
                  
                  IF cShiptoAddress[i] NE "" THEN DO:
                    PUT cShiptoAddress[i] AT 22 SKIP.
                    v-printline = v-printline + 1.
                    IF v-printline GT lv-line-print THEN
                    DO:
                        PAGE .
                        {oe/rep/ackhenry.i}
                        ASSIGN v-printline = 20.          
                    END. /* IF v-printline GT lv-line-print */
                  END. /* IF cShiptoAddress[i] NE "" */
                END. /* FOR EACH tt-formtext */
                                 
            END. /* IF AVAIL shipto */
          END. /* if v-shipto then */
    END. /* DO WITH FRAME sched-rel DOWN */
END PROCEDURE

/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */