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

{XMLOutput/XMLOutput.i &XMLOutput=XMLBOL &Company=cocode} /* rstark 05181205 */
RUN XMLOutput (lXMLOutput,'','','Header'). /* rstark 05181205 */

def var v-salesman          as   char format "x(26)".
def var v-fob               as   char format "x(12)".
def var v-tot-cases         as   int format "->,>>>,>>9".
def var v-tot-palls         as   int format "->,>>>,>>9".
def var v-tot-wt            as   dec format "->>,>>>,>>9".
DEF VAR v-shipqty           AS   DEC FORMAT "->>,>>>,>>9.9".
DEF VAR v-ordqty            AS   DEC FORMAT "->>,>>>,>>9.9".
DEF VAR v-backord           AS   DEC FORMAT "->>,>>>,>>9.9".
DEF VAR v-untcount          AS   INT FORMAT "->>>>>9".

DEF VAR v-tot-part          AS   CHAR FORMAT "x(15)".
DEF VAR v-tot-desc          AS   CHAR FORMAT "x(30)".
DEF VAR v-tot-unittype      AS   CHAR FORMAT "x(3)".


def var v-tot-pkgs          as   int format ">>9".
def var v-pal-cnt           as   dec.
def var v-ord-qty           like oe-ordl.qty.
def var v-bol-qty           like oe-boll.qty.
def var v-ship-qty          like oe-ordl.ship-qty.
def var v-orddate         like oe-ord.ord-date.
DEF VAR v-sale-num LIKE oe-ord.ord-no.
DEF VAR v-po-num LIKE oe-ord.po-no.
DEF VAR v-ship-date LIKE oe-ord.last-date.

def var v-bol-wt            as   dec.
def var v-part-dscr         as   char format "x(30)".
def var v-part-comp         as   char format "x".
def var v-part-qty          as   dec.
def var v-ord-no            like oe-boll.ord-no.
def var v-po-no             like oe-bolh.po-no.
def var v-bol-no            like oe-bolh.bol-no.
def var v-job-no            as   char format "x(9)" no-undo.
def var v-phone-num         as   char format "x(13)" no-undo.
DEF VAR v-ship-phone        AS   CHAR FORMAT "X(13)" NO-UNDO.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
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
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR v-cust-contact LIKE cust.contact NO-UNDO.
DEF VAR v-cust-account LIKE cust.cust-no NO-UNDO.
ASSIGN
 /*  ls-image1 = "images\loylang.JPG"*/
 /*     ls-image1 = "images\Larmar Logo.jpg" */
 ls-image1 = "images\Lamar.jpg".
   FILE-INFO:FILE-NAME = ls-image1.
   ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR lv-bolfmt-int AS INT NO-UNDO.
def var v-weight LIKE oe-boll.weight no-undo.
def var v-job-po            as   CHAR NO-UNDO.
DEF VAR v-phone AS cha NO-UNDO.
DEF VAR v-shipto-contact LIKE shipto.contact NO-UNDO.
DEF VAR v-ship-i AS cha EXTENT 4 FORM "x(60)" NO-UNDO.
DEF VAR v-tmp-lines AS DEC NO-UNDO.

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
        v-bol-no = oe-bolh.bol-no
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
    v-shipto-contact = oe-bolh.contact.

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
             ASSIGN 
                    v-orddate  = oe-ord.ord-date
                    v-sale-num = oe-ord.ord-no
                    v-po-num   = oe-boll.po-no
                    v-ship-date = oe-ord.last-date
                    v-comp-name = oe-ord.sold-name
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
     FIND FIRST oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK NO-ERROR.
       IF AVAIL oe-boll THEN DO:
          FIND FIRST oe-ord WHERE oe-ord.company = oe-bolh.company
                              AND oe-ord.ord-no = oe-boll.ord-no NO-LOCK NO-ERROR.
          IF AVAIL oe-ord THEN
             ASSIGN 
                    v-orddate = oe-ord.ord-date
                    v-sale-num = oe-ord.ord-no
                    v-po-num = oe-boll.po-no
                    v-ship-date = oe-ord.last-date
                   .        
       END.
    assign
       v-cust-contact = cust.contact
       v-cust-account = cust.cust-no
       v-comp-name    = cust.name
       v-comp-addr[1] = cust.addr[1]
       v-comp-addr[2] = cust.addr[2]
       v-comp-addr3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip.

    if trim(v-comp-addr3) eq "," then v-comp-addr3 = "".
              
    if v-comp-addr[2] eq "" then
      assign
       v-comp-addr[2] = v-comp-addr3
       v-comp-addr3   = "".
    if v-ship-addr[2] eq "" then
      assign
       v-ship-addr[2] = v-ship-addr3
       v-ship-addr3   = "".

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

  end. /* first-of(oe-bolh.bol-no) */

  
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
     
     /* rstark 05181205 */
     RUN XMLOutput (lXMLOutput,'BOLHeader','','Row').
     RUN XMLOutput (lXMLOutput,'BOLNumber',v-bol-no,'Col').
     RUN XMLOutput (lXMLOutput,'Order_Date',STRING(v-orddate),'Col').
     RUN XMLOutput (lXMLOutput,'Ship_Date',STRING(v-ship-date),'Col').
     RUN XMLOutput (lXMLOutput,'Sales_Order',v-sale-num,'Col').
     RUN XMLOutput (lXMLOutput,'Customer_Contact',v-cust-contact,'Col').
     RUN XMLOutput (lXMLOutput,'Purchase_Order',v-po-num,'Col').
     RUN XMLOutput (lXMLOutput,'Customer_Account',v-cust-account,'Col').
     RUN XMLOutput (lXMLOutput,'Customer_Name',v-comp-name,'Col').
     RUN XMLOutput (lXMLOutput,'Bill_To_1',v-comp-addr[1],'Col').
     RUN XMLOutput (lXMLOutput,'Bill_To_2',v-comp-addr[2],'Col').
     RUN XMLOutput (lXMLOutput,'Bill_To_3',v-comp-addr3,'Col').
     RUN XMLOutput (lXMLOutput,'Ship_1',v-ship-name,'Col').
     RUN XMLOutput (lXMLOutput,'Ship_2',v-ship-addr[1],'Col').
     RUN XMLOutput (lXMLOutput,'Ship_3',v-ship-addr[2],'Col').
     RUN XMLOutput (lXMLOutput,'Ship_4',v-ship-addr3,'Col').
     RUN XMLOutput (lXMLOutput,'/BOLHeader','','Row').
     /* rstark 05181205 */
     
     {oe/rep/bollamarpkg2.i}
     
     {oe/rep/bollamarpkg.i}

    v-last-page = page-number.
  
  PUT
    "<FArial> "
    "<R53><C1><P12>"
    "_________________________________________________________________________________________" 
    "<R55><C2>" "</B> Person Receiving __________________             Person Receiving Signature ____________________" 
    "<R56><C2>" "                                               Printed                                                                                    Signed"     
    "<R58><C2>" "<P9><B> WE VERIFY THAT THIS SHIPMENT HAS BEEN INSPECTED FOR QUALITY AND THAT PRODUCT IS 100% FREE OF "
    "<R59><C20> " "DEFECTS.____________________ PERSONS CHECKING ORDER INITIALS </B> "
    "<R61><C2> " " <P12> Please contact the Lamar and Associates, LLC at(419)842-0309 with any questions or concerns. "
    "<R62><C27>" "<B> Thank you for your order!</B> "
    .

  v-printline = v-printline + 14.
 
/*  IF v-printline < 45 THEN PUT SKIP(60 - v-printline). */
  PAGE.
  v-printline = 0.

  for each report where report.term-id eq v-term-id,
      first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
    delete report.
  end.

  END.  /* last-of*/

  oe-bolh.printed = yes.
end. /* for each oe-bolh */

RUN XMLOutput (lXMLOutput,'Last',STRING(PAGE-NUM),'Page'). /* rstark 05181205 */
{XMLOutput/XMLOutput.i &XMLClose} /* rstark 05181205 */

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

