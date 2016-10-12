/* ---------------------------------------------- oe/rep/bolpacifp 10/02 YSK *
*    Program Name  : fg/rep/boltxprt.p                                        *
      Author       :                                                         *
      Purpose      :   PRINT Empire BOL                                      *
      Date         :                                                         *
      Modify By    : Aj 10/12/2016 Frieght Terms was not printing correctly  *
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
DEF VAR lv-cust-no AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-email AS cha FORM "x(40)" NO-UNDO.
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

DEFINE SHARED VARIABLE cShipTO AS CHAR NO-UNDO .
DEF SHARED TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd
    FIELD row-id   AS ROWID
    FIELD has-rec  AS LOG INIT NO
    FIELD invoiced AS LOG INIT NO
    FIELD old-tag AS CHAR
    FIELD ret-loc AS CHAR
    FIELD ret-loc-bin AS CHAR
    FIELD blank-cust AS CHAR.

DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(150)" NO-UNDO.
DEF VAR lv-net-wt as dec NO-UNDO.
DEF VAR v-receipt-date AS DATE FORM "99/99/99" NO-UNDO.
DEF VAR v-ship-from AS CHAR NO-UNDO.
DEF VAR iBolno     AS INTEGER NO-UNDO .
DEF VAR v-tot-pallets AS DECIMAL NO-UNDO .
RUN GetPrintBarTag IN SOURCE-PROCEDURE (OUTPUT v-Print-BarTag) NO-ERROR.

ASSIGN tmpstore = fill("-",130).

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "BOLFMTTran" no-lock no-error.
IF AVAIL sys-ctrl THEN 
   ASSIGN lv-display-comp = sys-ctrl.log-fld 
          lv-bolfmt-int = sys-ctrl.int-fld.
ELSE ASSIGN lv-display-comp = NO
            lv-bolfmt-int = 0.

ASSIGN
   ls-image1 = "images\metrobol.jpg"
   FILE-INFO:FILE-NAME = ls-image1
   ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company = cocode no-lock no-error.


FIND FIRST cust WHERE cust.company = cocode AND
                     cust.active = "X" NO-LOCK NO-ERROR.
IF AVAIL cust THEN
 ASSIGN v-comp-add1 = cust.addr[1]
        v-comp-add2 = cust.addr[2]
        v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
        v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
        v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
        lv-email    = "Email:  " + cust.email 
        lv-cust-no  = cust.cust-no 
        lv-comp-name = cust.NAME   
        v-cusx-add1 = v-comp-add1
        v-cusx-add2 = v-comp-add2
        v-cusx-add3 = v-comp-add3
        v-cusx-add4 = v-comp-add4
        v-cusx-add5 = v-comp-add5
        v-cusx-email = lv-email
        v-cusx-name = lv-comp-name
        v-ship-from = cust.loc
        .


FOR EACH w-fg-rctd NO-LOCK :

     RUN oe/pallcalc2.p (INPUT cocode,
                         INPUT w-fg-rctd.i-no,
                         INPUT w-fg-rctd.job-no,
                         INPUT INT(w-fg-rctd.job-no2),
                         INPUT w-fg-rctd.loc,
                         INPUT w-fg-rctd.loc-bin,
                         INPUT w-fg-rctd.tag,
                         INPUT w-fg-rctd.cust-no,
                         INPUT INT(w-fg-rctd.partial),
                         INPUT INT(w-fg-rctd.qty),
                         INPUT INT(w-fg-rctd.cases),
                         OUTPUT v-tot-pallets).
    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ cocode 
        AND itemfg.i-no EQ w-fg-rctd.i-no NO-ERROR.
    

    ASSIGN v-tot-wt = v-tot-wt +  (((w-fg-rctd.qty-case * w-fg-rctd.cases ) + w-fg-rctd.partial) * itemfg.weight-100 / 100)
           w-fg-rctd.tot-wt = (((w-fg-rctd.qty-case * w-fg-rctd.cases ) + w-fg-rctd.partial) * itemfg.weight-100 / 100)
           v-tot-palls = v-tot-palls + v-tot-pallets   .
    IF w-fg-rctd.bol-no <> 0 THEN
        iBolno =  w-fg-rctd.bol-no .
END.

FIND FIRST w-fg-rctd NO-LOCK NO-ERROR .
IF NOT AVAIL w-fg-rctd THEN DO:   
      RETURN.
  END.
IF iBolno EQ 0 THEN DO:
    RUN oe/oe-bolno.p (cocode, OUTPUT iBolno).
END.

{sa/sa-sls01.i}

find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
v-printline = 0.

for each w-fg-rctd
    break by w-fg-rctd.blank-cust:
     FIND FIRST cust WHERE cust.company = cocode AND
                         cust.active = "X" NO-LOCK NO-ERROR.
     IF w-fg-rctd.bol-no EQ 0 THEN do:
         ASSIGN w-fg-rctd.bol-no =  iBolno .
         FIND FIRST fg-rctd EXCLUSIVE-LOCK 
             WHERE ROWID(fg-rctd)    EQ w-fg-rctd.row-id 
             NO-ERROR .
         ASSIGN fg-rctd.bol-no = iBolno .
     END.
  
    if first-of(w-fg-rctd.blank-cust) then do:
  
    RUN oe/custxship.p (cust.company,
                        lv-cust-no,
                        cShipTO,
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
    /*v-phone = IF oe-bolh.area-code + oe-bolh.phone <> "" THEN 
              "(" + oe-bolh.area-code + ")" + string(oe-bolh.phone,"xxx-xxxx")
              ELSE ""*/
    v-shipto-contact = shipto.contact  .

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
       
    END.
    ELSE ASSIGN v-comp-add1 = v-cusx-add1
                v-comp-add2 = v-cusx-add2    
                v-comp-add3 = v-cusx-add3    
                v-comp-add4 = v-cusx-add4                
                v-comp-add5 = v-cusx-add5
                lv-email    = v-cusx-email
                lv-comp-name = v-cusx-name.
    assign
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

    assign
     v-salesman = ""
     v-fob      = ""
     v-terms    = "" .
      .
   
      if not available carrier then
      find first carrier where carrier.company = cust.company
        and carrier.carrier = shipto.carrier no-lock no-error.

          v-salesman = cust.sman .
      
      assign v-terms = cust.terms
             v-frt-terms = if cust.frt-pay eq "P" then "Prepaid"
                           else if cust.frt-pay eq "B" then "Bill"
                           else if cust.frt-pay eq "C" then "Collect"
                           else if cust.frt-pay eq "T" then "Third Party"
                           else ""
             v-zone = cust.del-zone.
             
      if v-terms NE "" then
      do:
        find first terms where terms.t-code eq cust.terms no-lock no-error.
        if avail terms then
          assign v-terms = terms.dscr.
      end.
      
      v-salesman = trim(v-salesman).
      v-job-no = IF w-fg-rctd.job-no = "" THEN "" ELSE (w-fg-rctd.job-no + "-" + STRING(w-fg-rctd.job-no2,">>")).
      if v-salesman gt '' then
        if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
          substr(v-salesman,length(trim(v-salesman)),1) = "".

      v-fob = if cust.fob-code begins "ORIG" then "Origin" else "Destination".
  
    {fg/rep/boltxprt.i}

  end. /* first-of(cust.cust-no) */

  PUT SPACE(2) w-fg-rctd.i-no FORMAT "x(15)" SPACE(7)
      w-fg-rctd.tag FORMAT "x(30)"
      (w-fg-rctd.qty-case * w-fg-rctd.cases ) + w-fg-rctd.partial SPACE(10)
      w-fg-rctd.tot-wt SKIP .
    
   v-printline = v-printline + 1.

   IF v-printline >= 40 THEN DO:
       v-printline = 0.
       PAGE {1}.
       {fg/rep/boltxprt.i}
   END.
  
  
  if last-of(w-fg-rctd.blank-cust) then do:
     IF v-comp-addr[2] = "" THEN
           ASSIGN v-comp-addr[2] = v-comp-addr3
                  v-comp-addr3 = "".
     IF v-ship-addr[2] = "" THEN
           ASSIGN v-ship-addr[2] = v-ship-addr3
                  v-ship-addr3 = "".
     
     
     /*{oe/rep/bolxprnt.i}*/

    v-last-page = page-number.

  /*IF oe-bolh.tot-pallets NE 0 THEN v-tot-palls = oe-bolh.tot-pallets.*/

  PUT "<R54><C50><#8><FROM><R+4><C+30><RECT> " 
    "<=8><R+1> Total Pallets       :" v-tot-palls 
    "<=8><R+3> Total Weight        :" v-tot-wt /*fORM ">>,>>9.99"*/ .
    
  ASSIGN v-ship-i = "".
  /*IF v-print-shipnotes THEN
     ASSIGN v-ship-i[1] = oe-bolh.ship-i[1]
            v-ship-i[2] = oe-bolh.ship-i[2]
            v-ship-i[3] = oe-bolh.ship-i[3]
            v-ship-i[4] = oe-bolh.ship-i[4].*/

  PUT "<FArial><R51><C1><P12><B>     Shipping Instructions: </B> <P9> " 
    "<R53><C1>" v-ship-i[1] AT 7 
    "<R54><C1>" v-ship-i[2] AT 7 
    "<R55><C1>" v-ship-i[3] AT 7 
    "<R56><C1>" v-ship-i[4] AT 7 
    "<R58><C1>"
    "__________________________________________________________________________________________________________________" 
    "<R59><C1>" "<B>  Signature of Receipt </B>" 
    "<R60><C7>" "Customer ________________________________________                       Carrier _______________________________________" 
    "<R62><C7>" "Date ____________________________________________                       Date _________________________________________"     
    .

  v-printline = v-printline + 14.
 
/*  IF v-printline < 45 THEN PUT SKIP(60 - v-printline). */
  PAGE.
  v-printline = 0.
  
  END.  /* last-of*/

  /*oe-bolh.printed = yes.*/
end. /* for each w-fg-rctd  */



