/* ---------------------------------------------- oe/rep/bolpacifp 10/02 YSK *
*    Program Name  : oe/rep/bolxprt.p                                        *
      Author       :                                                         *
      Purpose      :   PRINT Empire BOL                                      *
      Date         :                                                         *
      Modify By    : Aj 06/24/2008 Frieght Terms was not printing correctly  *
                     Prior it was from customer now its from oe-bolh table   *       
* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.
def buffer xxreport     for report.

{oe/rep/oe-lad.i}

def var v-salesman          as   char format "x(26)".
def var v-fob               as   char format "x(12)".
def var v-tot-cases         as   int format "->,>>>,>>9".
def var v-tot-palls         as   int format "->,>>>,>>9".
def var v-tot-wt            as   dec format "->>,>>>,>>9".

def var v-tot-pkgs          as   int format ">>9".
def var v-pal-cnt           as   dec.
def var v-ord-qty           like oe-ordl.qty.
def var v-bol-qty           like oe-boll.qty.
def var v-ship-qty          like oe-ordl.ship-qty.
def var v-bol-wt            as   dec.
def var v-part-dscr         as   char format "x(30)".
def var v-part-comp         as   char format "x".
def var v-part-qty          as   dec.
def var v-ord-no            like oe-boll.ord-no.
def var v-po-no             like oe-bolh.po-no.
def var v-job-no            as   char format "x(9)" no-undo.
def var v-phone-num         as   char format "x(13)" no-undo.
DEF VAR v-ship-phone        AS   CHAR FORMAT "X(13)" NO-UNDO.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
DEFINE VARIABLE cShipContact as   CHARACTER format "x(30)".
DEFINE VARIABLE cCustContact as   CHARACTER format "x(30)".
def var v-comp-name  like company.name.
def var v-comp-addr  like company.addr.
def var v-comp-city  like company.city.
def var v-comp-state like company.state.
def var v-comp-zip   like company.zip.
def var v-comp-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".
def var v-1          LIKE oe-boll.cases INIT 1 no-undo.

def var v-terms like oe-ord.terms-d no-undo.
def var v-frt-terms as char format "x(12)" no-undo.
def var v-zone like carr-mtx.del-zone no-undo.
DEFINE VARIABLE cRefnum AS CHARACTER NO-UNDO.

def workfile w2 no-undo
    field cases            as   int format ">9"
    field cas-cnt          as   int format ">>>>9".

def workfile w3 no-undo
    field ship-i           as   char format "x(60)".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.

DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.  /* display company address */
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-email AS cha FORM "x(56)" NO-UNDO.
DEF VAR v-cusx-add1 AS cha NO-UNDO.
DEF VAR v-cusx-add2 AS cha NO-UNDO.
DEF VAR v-cusx-add3 AS cha NO-UNDO.
DEF VAR v-cusx-add4 AS cha NO-UNDO.
DEF VAR v-cusx-add5 AS cha NO-UNDO.
DEF VAR v-cusx-email AS cha NO-UNDO.
DEF VAR v-cusx-name AS cha NO-UNDO.

DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR lv-bolfmt-int AS INT NO-UNDO.
def var v-weight LIKE oe-boll.weight no-undo.
def var v-job-po            as   CHAR NO-UNDO.
DEF VAR v-phone AS cha NO-UNDO.
DEF VAR v-shipto-contact LIKE shipto.contact NO-UNDO.
DEF VAR v-ship-i AS cha EXTENT 4 FORM "x(60)" NO-UNDO.
DEF VAR v-tmp-lines AS DEC NO-UNDO.
DEF VAR v-print-barTag AS LOG NO-UNDO.
DEFINE VARIABLE ls-image1 AS CHARACTER NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE SHARED VAR v-print-unassembled AS LOG NO-UNDO.
DEFINE VARIABLE lValid         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdFileSysProcs AS HANDLE    NO-UNDO.

RUN system/FileSysProcs.p PERSISTENT SET hdFileSysProcs.
/*ASSIGN
   ls-image1 = "images\Lovepac_logo.jpg"
   FILE-INFO:FILE-NAME = ls-image1
   ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".*/

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound AND cRtnChar NE "" THEN DO:
    cRtnChar = DYNAMIC-FUNCTION (
                   "fFormatFilePath" IN hdFileSysProcs,
                   cRtnChar
                   ).
                   
    /* Validate the N-K-1 BusinessFormLogo image file */
    RUN FileSys_ValidateFile IN hdFileSysProcs (
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

RUN GetPrintBarTag IN SOURCE-PROCEDURE (OUTPUT v-Print-BarTag) NO-ERROR.

ASSIGN tmpstore = fill("-",130).

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "BOLFMT" no-lock no-error.
IF AVAIL sys-ctrl THEN 
   ASSIGN lv-display-comp = sys-ctrl.log-fld 
          lv-bolfmt-int = sys-ctrl.int-fld.
ELSE ASSIGN lv-display-comp = NO
            lv-bolfmt-int = 0.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company = cocode no-lock no-error.

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
            v-cusx-add1 = v-comp-add1
            v-cusx-add2 = v-comp-add2
            v-cusx-add3 = v-comp-add3
            v-cusx-add4 = v-comp-add4
            v-cusx-add5 = v-comp-add5
            v-cusx-email = lv-email
            v-cusx-name = lv-comp-name
            .

END.

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.
{sa/sa-sls01.i}

find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
v-printline = 0.

for each xxreport where xxreport.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh)   eq xxreport.rec-id,
    first cust
    where cust.company eq cocode
      and cust.cust-no eq oe-bolh.cust-no
    NO-LOCK
    break by oe-bolh.bol-no:
  
    if first-of(oe-bolh.bol-no) then do:
    find first carrier
        where carrier.company eq oe-bolh.company
          and carrier.carrier eq oe-bolh.carrier
        no-lock no-error.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    assign
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-addr3   = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip
     v-phone-num    = cust.area-code + cust.phone
     v-ship-phone   = IF shipto.area-code + shipto.phone <> "" THEN
                      "(" + shipto.area-code + ")" + string(shipto.phone,"xxx-xxxx")
                      ELSE ""
    v-phone = IF oe-bolh.area-code + oe-bolh.phone <> "" THEN 
              "(" + oe-bolh.area-code + ")" + string(oe-bolh.phone,"xxx-xxxx")
              ELSE ""
    v-shipto-contact = oe-bolh.contact
    cShipContact = shipto.contact .

    IF v-phone = "" THEN v-phone = "(" + shipto.area-code + ")" + string(shipto.phone,"xxx-xxxx").
    IF v-shipto-contact = "" THEN v-shipto-contact = shipto.contact.

    if shipto.broker then DO:
       ASSIGN v-comp-add1 = cust.addr[1]
              v-comp-add2 = cust.addr[2]
              v-comp-add3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip
              v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
              v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
              lv-email    = "Email:  " + cust.email   
              lv-comp-name = cust.NAME .
       /* sold to address from order */
       FIND FIRST oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK NO-ERROR.
       IF AVAIL oe-boll THEN DO:
          FIND FIRST oe-ord WHERE oe-ord.company = oe-bolh.company
                              AND oe-ord.ord-no = oe-boll.ord-no NO-LOCK NO-ERROR.
          IF AVAIL oe-ord THEN
             ASSIGN lv-comp-name = oe-ord.sold-name
                    v-comp-add1 = oe-ord.sold-addr[1]
                    v-comp-add2 = oe-ord.sold-addr[2]
                    v-comp-add3 = oe-ord.sold-city + ", " +
                                  oe-ord.sold-state + "  " +
                                  oe-ord.sold-zip.        
       END.
    END.
    ELSE ASSIGN v-comp-add1 = v-cusx-add1
                v-comp-add2 = v-cusx-add2    
                v-comp-add3 = v-cusx-add3    
                v-comp-add4 = v-cusx-add4                
                v-comp-add5 = v-cusx-add5
                lv-email    = v-cusx-email
                lv-comp-name = v-cusx-name.
   /* assign
       v-comp-name    = cust.name
       v-comp-addr[1] = cust.addr[1]
       v-comp-addr[2] = cust.addr[2]
       v-comp-addr3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip. */
    FIND FIRST oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK NO-ERROR.
    IF AVAIL oe-boll THEN DO:
        FIND FIRST oe-ord WHERE oe-ord.company = oe-bolh.company
            AND oe-ord.ord-no = oe-boll.ord-no NO-LOCK NO-ERROR.
        IF AVAIL oe-ord THEN do:
            FIND FIRST soldto NO-LOCK
                WHERE soldto.company EQ oe-bolh.company
                AND soldto.cust-no EQ oe-bolh.cust-no
                AND soldto.sold-id EQ oe-ord.sold-id NO-ERROR .
            IF AVAIL soldto THEN
                assign
                v-comp-name    = soldto.sold-name
                v-comp-addr[1] = soldto.sold-addr[1]
                v-comp-addr[2] = soldto.sold-addr[2]
                v-comp-addr3   = soldto.sold-city + ", " +
                                 soldto.sold-state + "  " +
                                 soldto.sold-zip
                cCustContact  = IF AVAIL cust THEN cust.contact ELSE "" .
            ELSE
                ASSIGN
                    v-comp-name    = oe-ord.sold-name
                    v-comp-addr[1] = oe-ord.sold-addr[1]
                    v-comp-addr[2] = oe-ord.sold-addr[2]
                    v-comp-addr3   = oe-ord.sold-city + ", " +
                                     oe-ord.sold-state + "  " +
                                     oe-ord.sold-zip
                   cCustContact  = IF AVAIL cust THEN cust.contact ELSE "" .
        END.
    END.

    if trim(v-comp-addr3) eq "," then v-comp-addr3 = "".
              
    if v-comp-addr[2] eq "" then
      assign
       v-comp-addr[2] = v-comp-addr3
       v-comp-addr3   = "".
    if v-ship-addr[2] eq "" then
      assign
       v-ship-addr[2] = v-ship-addr3
       v-ship-addr3   = "".

    IF v-comp-addr3 EQ "" THEN
        ASSIGN v-comp-addr3 = cCustContact
               cCustContact = "" .
    IF v-ship-addr3 EQ "" THEN
        ASSIGN v-ship-addr3 = cShipContact
               cShipContact = "" .

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

    assign
     v-salesman = ""
     v-fob      = ""
     v-terms    = "".
    
    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
        first oe-ord
	    where oe-ord.company eq oe-boll.company
	      and oe-ord.ord-no  eq oe-boll.ord-no
	    NO-LOCK:

      if not available carrier then
      find first carrier where carrier.company = oe-ord.company
        and carrier.carrier = oe-ord.carrier no-lock no-error.

      do i = 1 to 3:
        if oe-ord.sman[i] ne "" then
          v-salesman = trim(v-salesman) + " " + oe-ord.sman[i] + ",".
      end.
      
      assign v-terms = oe-ord.terms-d
             v-frt-terms = if oe-bolh.frt-pay eq "P" then "Prepaid"
                           else if oe-bolh.frt-pay eq "B" then "Bill"
                           else if oe-bolh.frt-pay eq "C" then "Collect"
                           else if oe-bolh.frt-pay eq "T" then "Third Party"
                           else ""
             v-zone = cust.del-zone.
             
      if v-terms eq "" then
      do:
        find first terms where terms.t-code eq oe-ord.terms no-lock no-error.
        if avail terms then
          assign v-terms = terms.dscr.
      end.
      
      v-salesman = trim(v-salesman).
      v-po-no = oe-boll.po-no.
      v-job-no = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>")).
      if v-salesman gt '' then
        if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
          substr(v-salesman,length(trim(v-salesman)),1) = "".

      v-fob = if oe-ord.fob-code begins "ORIG" then "Origin" else "Destination".
      
      LEAVE.
    end.

    for each w3:
      delete w3.
    end.
  end. /* first-of(oe-bolh.bol-no) */

  do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then do:
      find first w3 where w3.ship-i eq oe-bolh.ship-i[i] no-error.
      if not avail w3 then create w3.
      w3.ship-i = oe-bolh.ship-i[i].
    end.
  end.

  for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    create report.
    assign
     report.term-id  = v-term-id
     report.key-01   = oe-boll.i-no
     report.key-02   = string(oe-boll.ord-no,"9999999999")
     report.rec-id   = recid(oe-boll)
     oe-boll.printed = yes.
  end.

  if last-of(oe-bolh.bol-no) then do:
     IF v-comp-addr[2] = "" THEN
           ASSIGN v-comp-addr[2] = v-comp-addr3
                  v-comp-addr3 = "".
     IF v-ship-addr[2] = "" THEN
           ASSIGN v-ship-addr[2] = v-ship-addr3
                  v-ship-addr3 = "".
     IF v-comp-addr3 EQ "" THEN
        ASSIGN v-comp-addr3 = cCustContact
               cCustContact = "" .
    IF v-ship-addr3 EQ "" THEN
        ASSIGN v-ship-addr3 = cShipContact
               cShipContact = "" .
     
     {oe/rep/bolxprn10can.i}
     {oe/rep/bolxprnt10can.i}

    v-last-page = page-number.

  IF oe-bolh.tot-pallets NE 0 THEN v-tot-palls = oe-bolh.tot-pallets.

  PUT "<R54><C50><#8><FROM><R+4><C+30><RECT> " 
    "<=8><R+1> Nombre De Palettes  :" v-tot-palls 
    "<=8><R+3> Total Poids/Weight  :" v-tot-wt /*fORM ">>,>>9.99"*/ .
    
  ASSIGN v-ship-i = "".
  IF v-print-shipnotes THEN
     ASSIGN v-ship-i[1] = oe-bolh.ship-i[1]
            v-ship-i[2] = oe-bolh.ship-i[2]
            v-ship-i[3] = oe-bolh.ship-i[3]
            v-ship-i[4] = oe-bolh.ship-i[4].

  PUT "<FArial><R51><C1><P12><B>     Instructions de Livraison: </B> <P9> " 
    "<R53><C1>" v-ship-i[1] AT 7 
    "<R54><C1>" v-ship-i[2] AT 7 
    "<R55><C1>" v-ship-i[3] AT 7 
    "<R56><C1>" v-ship-i[4] AT 7 
    "<R58><C1>"
    "__________________________________________________________________________________________________________________" 
    "<R59><C1>" "<B>  Signature De R�ception </B>" 
    "<R60><C7>" "Client ________________________________________                       Transporteur/Carrier_____________________________" 
    "<R62><C7>" "Date ____________________________________________                       Date _________________________________________"     
    .

  v-printline = v-printline + 14.
 
/*  IF v-printline < 45 THEN PUT SKIP(60 - v-printline). */
  PAGE.
  v-printline = 0.

  IF v-Print-BarTag THEN RUN PrintBarTag.

  for each report where report.term-id eq v-term-id,
      first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
    delete report.
  end.

  END.  /* last-of*/

  oe-bolh.printed = yes.
end. /* for each oe-bolh */

IF VALID-HANDLE(hdFileSysProcs) THEN
    DELETE PROCEDURE hdFileSysProcs.

PROCEDURE PrintBarTag:
   DEF VAR iBarLine AS INT NO-UNDO.
   DEF VAR iBarCount AS INT NO-UNDO.
   DEF VAR cBarTag AS cha NO-UNDO.
   DEF VAR iLineOfTags AS INT NO-UNDO.
   
   FOR EACH oe-boll NO-LOCK where oe-boll.company eq oe-bolh.company 
                      and oe-boll.b-no eq oe-bolh.b-no
                      AND oe-boll.tag <> ""
                      BREAK BY oe-boll.i-no BY oe-boll.tag:

       IF FIRST-OF(oe-boll.i-no) OR iLineOfTags >= 5 THEN DO:
          FIND itemfg OF oe-boll NO-LOCK NO-ERROR.
          PAGE.
          PUT skip
              "<FArial><R3><C7><P10><B>BOL#: " oe-boll.bol-no
              "<C19>PART NUMBER: " itemfg.part-no /*oe-boll.i-no */
              "<C46>CUSTOMER: " cust.NAME /*oe-bolh.cust-no */
              "</B>"
              SKIP
              .
          ASSIGN iBarLine = 1
                 iBarcount = 0
                 iLineOfTags = 0
                 .
       END.
       ASSIGN cBarTag =  cBartag +
                 (IF iBarCount = 0 THEN "<UNITS=INCHES><AT=" + string(iBarline) + ",.8><FROM><AT=+1,+3>" 
                 ELSE "<UNITS=INCHES><AT=" + string(iBarLine) + ",4.7><FROM><AT=+1,+3>")     
                     + 
                 "<BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" + oe-boll.tag + ">" +
                 (IF iBarCount = 0 THEN "<AT=" + string(iBarLine + 1) + ",1.7>" ELSE "<AT=" + string(iBarline + 1) + ",5.5>" ) + oe-boll.tag 
              
              iBarCount = iBarCount + 1.

       IF iBarCount > 1 OR last-of(oe-boll.i-no) THEN DO: /* print*/
          PUT unformatted
              cBarTag
              SKIP
              .

          ASSIGN cBarTag = ""
                 iBarCount = 0
                 iBarLine = iBarLine + 2
                 iLineOfTags = iLineOfTags + 1.
       
       END.
   END.
   IF iBarCount > 0 THEN DO: /* print*/
      PUT UNFORMATTED cBarTag.
      iBarCount = 0.
   END.
END PROCEDURE.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

