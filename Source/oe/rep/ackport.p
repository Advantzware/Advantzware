/* ----------------------------------------------- oe/rep/ackPort.p 10/10 YSK */
/* ORDER ACKNOLEDGEMENT                                                       */
/* for ORDER STATUS = (R), (U)                                                */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/acknowl.i}
/* bpv 05291402 */
&SCOPED-DEFINE sysCtrlcXML cXMLAck
{XMLOutput/XMLOutput.i &cXMLOutput={&sysCtrlcXML} &Company=cocode &c=c}
/* bpv 05291402 */
def var v-salesman as char format "x(3)" NO-UNDO.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO.
def var v-addr3 as char format "x(30)" NO-UNDO.
def var v-addr4 as char format "x(30)" NO-UNDO.
def var cShipAdd3 as char format "x(30)" NO-UNDO.
def var v-line as INT NO-UNDO.
def var v-printline as int NO-UNDO.
def var v-ackhead as char format "x(32)" init
  "R E C O N H E C I M E N T O" NO-UNDO.
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
DEF VAR v-disp-price AS DEC NO-UNDO.
DEF VAR ll-calc-disc-first AS LOG NO-UNDO.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
/*ASSIGN ls-image1 = "images\premier.jpg"
       ls-image2 = "".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
FILE-INFO:FILE-NAME = ls-image2.
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".*/


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
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR lv-line-print AS INT INIT 45 NO-UNDO.
DEF SHARED VAR v-print-po AS LOG NO-UNDO.
DEF VAR po-no AS CHAR FORM "x(15)" NO-UNDO.
DEF VAR lGeneratecXML AS LOG NO-UNDO.
DEF VAR cShipmentDate AS CHAR NO-UNDO.
DEF VAR cDeliveryDate AS CHAR NO-UNDO.

DEF SHARED VAR v-UntCnt AS LOG NO-UNDO.
DEF SHARED VAR v-Shpnot AS LOG NO-UNDO.
DEF SHARED VAR v-print-tot AS LOG NO-UNDO.
DEFINE VARIABLE lValid         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
DEFINE VARIABLE opcDateStringRelDate AS CHARACTER NO-UNDO.
DEFINE VARIABLE opcDateStringOrdDate AS CHARACTER NO-UNDO.
DEFINE VARIABLE opcDateStringDueDate AS CHARACTER NO-UNDO.
DEFINE BUFFER bf-shipto FOR shipto.

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
    /* bpv 05291402 */
     FIND FIRST sys-ctrl NO-LOCK
       WHERE sys-ctrl.company EQ  cocode
         AND sys-ctrl.name    EQ 'cXMLAck' NO-ERROR.
        IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN DO:
            FIND FIRST sys-ctrl-shipto OF sys-ctrl NO-LOCK
                WHERE sys-ctrl-shipto.cust-vend EQ YES
                  AND sys-ctrl-shipto.cust-vend-no EQ oe-ord.cust-no
                  AND sys-ctrl-shipto.log-fld EQ YES
                NO-ERROR.
            IF AVAIL sys-ctrl-shipto THEN 
                ASSIGN 
                    cXMLIdentity = sys-ctrl-shipto.char-fld
                    cXMLDTD = 'http://xml.cXML.org/schemas/cXML/1.2.014/Fulfill.dtd'
                    lGeneratecXML = YES.
          END. /* avail sys-ctrl */
          
          find first bf-shipto no-lock
               where bf-shipto.company eq cocode
               and bf-shipto.cust-no eq oe-ord.cust-no
               and bf-shipto.ship-id eq oe-ord.ship-id
               no-error.
          IF AVAIL bf-shipto THEN     
          cShipAdd3 = bf-shipto.ship-city + ", " + bf-shipto.ship-state +
                      "  " + bf-shipto.ship-zip .     

        IF lGeneratecXML AND NOT oe-ord.ack-prnt THEN DO:
            clXMLOutput = YES.
          {XMLOutput/cXMLCust.i
            &cXMLSysCtrl={&sysCtrlcXML}
            &Company=oe-ord.company
            &Customer=oe-ord.cust-no}
        END. /* if not printed */
        ELSE lGeneratecXML = NO.
    /* bpv 05291402 */

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
       v-line = 1.
      IF cShipAdd3 = "" THEN cShipAdd3 = v-addr3.

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
      /* bpv 05291402 */
      ASSIGN
        cXMLPayloadID = oe-ord.spare-char-3
        cXMLProcessID = STRING(oe-ord.ord-no)
/*         cOrderDate =  STRING(YEAR(oe-ord.ord-date),'9999')         */
/*                              + '-'                                 */
/*                              + STRING(MONTH(oe-ord.ord-date),'99') */
/*                              + '-'                                 */
/*                              + STRING(DAY(oe-ord.ord-date),'99')   */
/*                              + 'T'                                 */
/*                              + STRING(0,'hh:mm:ss')                */
/*                              + '-05:00'                            */
        .
      IF lGeneratecXML THEN DO:
       RUN cXMLOutput (clXMLOutput,'Request deploymentMode="' + cXMLProduction + '"','','Row').
       RUN cXMLOutput (clXMLOutput,'ConfirmationRequest','','Row').
       RUN cXMLOutput (clXMLOutput,'ConfirmationHeader type="accept" noticeDate="' +
                                    cXMLTimeStamp + '" operation="new" confirmID="' +
                                    STRING(oe-ord.ord-no) + '"','','Row').
       RUN cXMLOutput (clXMLOutput,'Total','','Row').
       RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
       RUN cXMLOutput (clXMLOutput,'',STRING(oe-ord.t-revenue),'Col').
       RUN cXMLOutput (clXMLOutput,'/Money','','Row').
       RUN cXMLOutput (clXMLOutput,'/Total','','Row').
       RUN cXMLOutput (clXMLOutput,'Shipping','','Row').
       RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
       RUN cXMLOutput (clXMLOutput,'',STRING(oe-ord.t-freight),'Col').
       RUN cXMLOutput (clXMLOutput,'/Money','','Row').
       RUN cXMLOutput (clXMLOutput,'Description xml:lang="en-US"','','Row').
       RUN cXMLOutput (clXMLOutput,'',oe-ord.carrier,'Col').
       RUN cXMLOutput (clXMLOutput,'/Description','','Row').
       RUN cXMLOutput (clXMLOutput,'/Shipping','','Row').
       RUN cXMLOutput (clXMLOutput,'Tax','','Row').
       RUN cXMLOutput (clXMLOutput,'Money currency="USD"','','Row').
       RUN cXMLOutput (clXMLOutput,'',STRING(oe-ord.tax),'Col').
       RUN cXMLOutput (clXMLOutput,'/Money','','Row').
       RUN cXMLOutput (clXMLOutput,'Description xml:lang="en-US"','','Row').
       RUN cXMLOutput (clXMLOutput,'',oe-ord.tax-gr,'Col').
       RUN cXMLOutput (clXMLOutput,'/Description','','Row').
       RUN cXMLOutput (clXMLOutput,'/Tax','','Row').
       RUN cXMLOutput (clXMLOutput,'/ConfirmationHeader','','Row').
       RUN cXMLOutput (clXMLOutput,'OrderReference orderID="' + oe-ord.po-no + '"','','Row').
       RUN cXMLOutput (clXMLOutput,'DocumentReference payloadID="' + cXMLPayloadID + '" /','','Row').
       RUN cXMLOutput (clXMLOutput,'/OrderReference','','Row').
       ciXMLOutput = 0.
       /* rstark 05291402 */
     END.
      /* bpv 05291402 */
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

{oe/rep/ackPort.i}
for each oe-ordl
          where oe-ordl.company eq oe-ord.company
            and oe-ordl.ord-no  eq oe-ord.ord-no
          no-lock:

      /* bpv 05291402 */
        IF lGeneratecXML THEN DO:
            cShipmentDate =  STRING(YEAR(oe-ordl.req-date),'9999')
                             + '-'
                             + STRING(MONTH(oe-ordl.req-date),'99')
                             + '-'
                             + STRING(DAY(oe-ordl.req-date),'99')
                             + 'T'
                             + STRING(0,'hh:mm:ss')
                             + '-05:00'.
            cDeliveryDate =  STRING(YEAR(oe-ordl.req-date + 1),'9999')
                             + '-'
                             + STRING(MONTH(oe-ordl.req-date + 1),'99')
                             + '-'
                             + STRING(DAY(oe-ordl.req-date + 1),'99')
                             + 'T'
                             + STRING(0,'hh:mm:ss')
                             + '-05:00'.

            RUN cXMLOutput (clXMLOutput,'ConfirmationItem lineNumber="' + STRING(oe-ordl.line) 
                            + '" quantity="' + IF oe-ordl.spare-dec-1 NE 0 THEN STRING(oe-ordl.spare-dec-1) + '"' ELSE STRING(oe-ordl.qty) + '"','','Row').
            RUN cXMLOutput (clXMLOutput,'UnitOfMeasure',IF oe-ordl.spare-char-2 NE '' THEN oe-ordl.spare-char-2 ELSE 'EA','Col').
            RUN cXMLOutput (clXMLOutput,'ConfirmationStatus type="accept" ' + ' deliveryDate="' + cDeliveryDate + '"' + ' quantity="' + IF oe-ordl.spare-dec-1 NE 0 THEN STRING(oe-ordl.spare-dec-1) + '"' ELSE STRING(oe-ordl.qty) + '"' 
                            + ' shipmentDate="' + cShipmentDate + '"','','Row').
            RUN cXMLOutput (clXMLOutput,'UnitOfMeasure',IF oe-ordl.spare-char-2 NE '' THEN oe-ordl.spare-char-2 ELSE 'EA','Col').
            RUN cXMLOutput (clXMLOutput,'/ConfirmationStatus','','Row').
            RUN cXMLOutput (clXMLOutput,'/ConfirmationItem','','Row').
        END.
      /* bpv 05291402 */
        
        if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackPort.i}
            assign v-printline = 20.          
        end.

        /* Duplicating put so that form could be different */
        IF v-hide-price THEN
            put v-line FORM ">>>9" SPACE(3)
                    oe-ordl.i-no  SPACE(2)             
                    oe-ordl.i-name SPACE(1)
                    oe-ordl.qty FORMAT "->,>>>,>>>,>>9" SPACE(2)
                    0  FORM "->>>>>>>>>>>>>>>>" SPACE(5)
                    oe-ordl.pr-uom  SKIP
                .
        ELSE 
            put v-line FORM ">>>9" SPACE(3)
                    oe-ordl.i-no  SPACE(2)             
                    oe-ordl.i-name SPACE(1)
                    oe-ordl.qty FORMAT "->,>>>,>>>,>>9" SPACE(2)
                    oe-ordl.price  FORM "->,>>>,>>9.99<<<<" SPACE(5)
                    oe-ordl.pr-uom  SKIP
                .
        v-printline = v-printline + 1.
       if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackPort.i}
            assign v-printline = 20.          
        end.
       IF v-print-tot AND NOT v-hide-price THEN do:
       if oe-ordl.i-no ne oe-ordl.part-no or
           oe-ordl.part-dscr1 ne "" OR   oe-ordl.t-price NE 0    then do:
          v-part = if oe-ordl.i-no ne oe-ordl.part-no then oe-ordl.part-no
                   else "".
          put v-part             at 8
              oe-ordl.part-dscr1  at 25 
              oe-ordl.t-price FORM "->,>>>,>>9.99<<<<"  AT 72
              SPACE(3) "Total"  skip.
          v-printline = v-printline + 1.
        end. 
       END.
        ELSE DO:
            if oe-ordl.i-no ne oe-ordl.part-no or
           oe-ordl.part-dscr1 ne ""        then do:
          v-part = if oe-ordl.i-no ne oe-ordl.part-no then oe-ordl.part-no
                   else "".
          put v-part             at 8
              oe-ordl.part-dscr1  at 25  skip.
            v-printline = v-printline + 1.
        end. 

        END.

        ASSIGN po-no = "" .
        IF v-print-po = YES THEN do:
           ASSIGN po-no = oe-ordl.po-no .
        END.

        if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackPort.i}
            assign v-printline = 20.          
        end.
        if oe-ordl.part-dscr2 ne "" OR po-no NE "" then do:
          put po-no AT 8
              oe-ordl.part-dscr2 at 25 skip.
          v-printline = v-printline + 1.
        end.
        if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackPort.i}
            assign v-printline = 20.          
        end.
        IF v-UntCnt THEN DO:
            find first itemfg WHERE itemfg.company EQ oe-ordl.company
            and itemfg.i-no eq oe-ordl.i-no no-lock no-error.
            IF AVAIL itemfg THEN do:
                PUT "Contagem de unidades: " AT 25 itemfg.case-count  SKIP.
                v-printline = v-printline + 1.
            END.
        END.
        if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackPort.i}
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
  /*
          if (v-printline ge pagebreak) or
             (v-printline ge pagebreak - 1 and lcnt eq 1) then do:
            v-printline = 0.
            page.
          end.
  */
          if v-printline ge lv-line-print then
          do:
            PAGE .
            {oe/rep/ackPort.i}
            assign v-printline = 20.          
          end.
          if first-of(oe-rel.link-no) then do:
            if oe-rel.link-no eq 0 then do:
              put "Libera��es programadas:" at 10  skip.
              v-printline = v-printline + 1.
            end.
            else
            if first(oe-rel.link-no) then do:
              put "Libera��es atual:" at 10 skip.
              v-printline = v-printline + 1.
            end.
          end.
          if v-printline ge lv-line-print then
          do:
            PAGE .
            {oe/rep/ackPort.i}
            assign v-printline = 20.          
          end.

          {oe/rel-stat.i lv-stat}
          IF AVAIL oe-rell THEN
          FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
          ld-date = IF AVAIL oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date.
          
          RUN Format_Date(ld-date,"DD/MM/YYYY", OUTPUT opcDateStringRelDate).
          
          if oe-rel.link-no eq 0 then
              put lcnt AT 10 space(1) oe-rel.tot-qty space(5) opcDateStringRelDate FORMAT "x(10)"  SKIP.
          ELSE
              put lcnt AT 10 space(1) oe-rel.qty space(5) opcDateStringRelDate FORMAT "x(10)"  SKIP.
          /*down with frame sched-rel. */
          assign
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
      
            if v-printline ge lv-line-print  then
            do:
               PAGE .
               {oe/rep/ackPort.i}
               assign v-printline = 20.          
            end.
            IF AVAIL shipto THEN DO:
                put shipto.ship-name AT 10 SKIP .
                v-printline = v-printline + 1.
                if v-printline ge lv-line-print then
                do:
                    PAGE .
                    {oe/rep/ackPort.i}
                    assign v-printline = 20.          
                end.
                IF shipto.ship-addr[1] <> "" THEN DO:
                   PUT shipto.ship-addr[1] AT 10  SKIP.
                   v-printline = v-printline + 1.
                   if v-printline ge lv-line-print then
                   do:
                      PAGE .
                      {oe/rep/ackPort.i}
                      assign v-printline = 20.          
                   end.
                END.
                IF shipto.ship-addr[2] <> "" THEN DO:
                    PUT shipto.ship-addr[2] AT 10  SKIP.
                    v-printline = v-printline + 1.
                    if v-printline ge lv-line-print then
                    do:
                        PAGE .
                        {oe/rep/ackPort.i}
                        assign v-printline = 20.          
                    end.
                END.
                IF v-addr4 <> "" THEN DO:
                   PUT v-addr4 AT 10 SKIP.
                   v-printline = v-printline + 1.
                END.
            END.
          END.
          IF v-shpnot THEN DO:
              IF oe-rel.ship-i[1] NE "" THEN do:
                  PUT oe-rel.ship-i[1] SKIP.
                  v-printline = v-printline + 1.
              END.
              IF oe-rel.ship-i[2] NE "" THEN do:
                  PUT oe-rel.ship-i[2] SKIP.
                  v-printline = v-printline + 1.
              END.
              IF oe-rel.ship-i[3] NE "" THEN do:
                  PUT oe-rel.ship-i[3] SKIP.
                  v-printline = v-printline + 1.
              END.
              IF oe-rel.ship-i[4] NE "" THEN do:
                  PUT oe-rel.ship-i[4] SKIP.
                  v-printline = v-printline + 1.
              END.
          END.
        end.   /* for each oe-rel  */
        v-line = v-line + 1.

      /*  put "" skip.
        assign
         v-line = v-line + 1
         v-printline = v-printline + 1.
      */
        if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackPort.i}
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
  
        if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackPort.i}
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
                  if v-printline ge lv-line-print then
                  do:
                      PAGE .
                      {oe/rep/ackPort.i}
                      assign v-printline = 20.          
                  end.
    
               END.
               IF NOT lv-first-note THEN do:
                  PUT SKIP(1).
                  v-printline = v-printline + 1.           
               END.
               if v-printline ge lv-line-print then
               do:
                  PAGE .
                  {oe/rep/ackPort.i}
                  assign v-printline = 20.          
               end.
            END.
        END. /* v-prntinst*/          
      end. /* each oe-ordl */

      for each oe-ordm no-lock where oe-ordm.company eq oe-ord.company and
          oe-ordm.ord-no eq oe-ord.ord-no break BY ord-no:

          if v-printline ge lv-line-print then
          do:
              PAGE .
              {oe/rep/ackPort.i}
              assign v-printline = 20.          
          end.

        if first(oe-ordm.ord-no) then
        do:
          put "** Itens variados **" at 23.
          if v-print-fmt eq "HOP" then put "Taxable" at 62.
          put skip(1).
          assign v-printline = v-printline + 2.
        end.
        if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackPort.i}
            assign v-printline = 20.          
        end.

        IF v-hide-price THEN do:
            if oe-ordm.bill eq "N" then
                PUT v-line FORM ">>>9" SPACE(3)
                oe-ordm.charge oe-ordm.dscr SPACE(1) "PO#: " oe-ordm.po-no .          
            else
                PUT v-line FORM ">>>9" space(3)
                    oe-ordm.charge oe-ordm.dscr SPACE(1) "PO#: " oe-ordm.po-no .
        END.
        ELSE DO:
            if oe-ordm.bill eq "N" then
                PUT v-line FORM ">>>9" SPACE(3)
                oe-ordm.charge oe-ordm.dscr SPACE(1) "PO#: " oe-ordm.po-no "             N/C" .                    
            else
                PUT v-line FORM ">>>9" space(3)
                    oe-ordm.charge oe-ordm.dscr SPACE(1) "PO#: " oe-ordm.po-no SPACE(5)
                    oe-ordm.amt .
        END.

        PUT SKIP.
        assign v-line = v-line + 1
          v-printline = v-printline + 1.
        if v-printline ge lv-line-print then
        do:
            PAGE .
            {oe/rep/ackPort.i}
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
      if v-printline ge lv-line-print then
      do:
            PAGE .
            {oe/rep/ackPort.i}
            assign v-printline = 20.          
      end.

      ASSIGN oe-ord.ack-prnt = yes.
/*       
      PUT "<FArial><R56><C1><#10><P12><B> Comments </B> <P8> " SKIP        
        v-billinst[1] SKIP
        v-billinst[2] SKIP
        v-billinst[3] SKIP
        v-billinst[4] SKIP(1)
        " ______________________________________(Please sign and fax back) " SKIP
        "<=10><R-3>" SPACE(32) "<P9><B>THIS IS A CONFIRMATION OF YOUR ORDER,NOT AN INVOICE.</B>" .
*/      
      IF v-hide-price THEN
          v-totord = 0.
      PUT "<FArial><R55><C1><#10><P12><B> Coment�rios </B>   <P10><C60> Valor total do pedido:" v-totord "<P8>" 
        "<R56><C1>" v-billinst[1] 
        "<R57><C1>" v-billinst[2] 
        "<R58><C1>" v-billinst[3] 
        "<R59><C1>" v-billinst[4] 
        "<R61><C1>" " ______________________________________(Assine e envie por fax) " 
        "<=10><R-2><C23><P9><B>ESTA � UMA CONFIRMA��O DO SEU PEDIDO, N�O UMA FATURA.</B>" .
      v-printline = v-printline + 6.
      
      IF v-printline <= 66 THEN page. /*PUT SKIP(60 - v-printline). */
      v-totord = 0.
        IF lGeneratecXML THEN DO: 
          /* rstark 05291402 */
          RUN cXMLOutput (clXMLOutput,'/ConfirmationRequest','','Row').
          RUN cXMLOutput (clXMLOutput,'/Request','','Row').
          {XMLOutput/XMLOutput.i &c=c &XMLClose} /* rstark 05291402 */
          /* rstark 05291402 */
        END.
    end. /* each oe-ord */

/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */
