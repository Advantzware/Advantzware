/* ---------------------------------------------- oe/rep/bolprint.p 10/09 GDM */
/* N-K BOLFMT = Printers - FORM for Loylang                                    */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.
def buffer xxreport     for report.
DEF BUFFER ref-lot-no FOR reftable.

{oe/rep/oe-lad.i}

def var v-salesman          as   char format "x(26)" NO-UNDO.
def var v-fob               as   char format "x(12)" NO-UNDO.
def var v-tot-cases         as   int format "->,>>>,>>9" NO-UNDO.
def var v-tot-palls         as   int format "->>,>>>,>>9" NO-UNDO.
def var v-tot-wt            as   dec format "->>,>>>,>>9" NO-UNDO.

def var v-tot-pkgs          as   int format ">>9" NO-UNDO.
def var v-pal-cnt           as   DEC NO-UNDO.
def var v-ord-qty           like oe-ordl.qty NO-UNDO.
def var v-bol-qty           like oe-boll.qty NO-UNDO.
def var v-ship-qty          like oe-ordl.ship-qty NO-UNDO.
def var v-bol-wt            as   DEC NO-UNDO.
def var v-part-dscr         as   char format "x(30)" NO-UNDO.
def var v-part-comp         as   char format "x" NO-UNDO.
def var v-part-qty          as   DEC NO-UNDO.
def var v-ord-no            like oe-boll.ord-no NO-UNDO.
def var v-po-no             like oe-bolh.po-no NO-UNDO.
def var v-job-no            as   char format "x(9)" no-undo.
def var v-phone-num         as   char format "x(13)" no-undo.
DEF VAR v-ship-phone        AS   CHAR FORMAT "X(13)" NO-UNDO.

def var v-ship-name  like shipto.ship-name NO-UNDO.
def var v-ship-addr  like shipto.ship-addr NO-UNDO.
def var v-ship-city  like shipto.ship-city NO-UNDO.
def var v-ship-state like shipto.ship-state NO-UNDO.
def var v-ship-zip   like shipto.ship-zip NO-UNDO.
def var v-ship-addr3 as   char format "x(30)" NO-UNDO.
def var v-comp-name  like company.NAME NO-UNDO.
def var v-comp-addr  like company.addr NO-UNDO.
def var v-comp-city  like company.city NO-UNDO.
def var v-comp-state like company.state NO-UNDO.
def var v-comp-zip   like company.zip NO-UNDO.
def var v-comp-addr3 as   char format "x(30)" NO-UNDO.
def var v-cust-addr3 as   char format "x(30)" NO-UNDO.
def var v-1          LIKE oe-boll.cases INIT 1 no-undo.

def var v-terms like oe-ord.terms-d no-undo.
def var v-frt-terms as char format "x(10)" no-undo.
def var v-zone like carr-mtx.del-zone no-undo.
DEF VAR v-job-var AS CHAR NO-UNDO.

def TEMP-TABLE w2 no-undo
    field cases            as   int format ">9"
    field cas-cnt          as   int format ">>>>9".

def TEMP-TABLE w3 no-undo
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
DEF VAR lv-display-comp AS LOG NO-UNDO.
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

DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
ASSIGN
   ls-image1 = "images/loyprinters.jpg"
   FILE-INFO:FILE-NAME = ls-image1 
   ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-broker LIKE shipto.broker     NO-UNDO.
DEF VAR v-fgitem AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-recnt  AS INT                 NO-UNDO.
DEF VAR v-dock   LIKE shipto.dock-loc   NO-UNDO.
DEF VAR v-dockHr LIKE shipto.dock-hour  NO-UNDO.

DEF BUFFER bf-reftable FOR reftable.

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
             v-cusx-name = lv-comp-name.
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

    ASSIGN
     v-broker = shipto.broker
     v-dock   = shipto.dock-loc
     v-dockHr = shipto.dock-hour
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
             ASSIGN lv-comp-name = oe-ord.sold-name
                    v-comp-add1 = oe-ord.sold-addr[1]
                    v-comp-add2 = oe-ord.sold-addr[2]
                    v-comp-add3 = oe-ord.sold-city + ", " +
                                  oe-ord.sold-state + "  " +
                                  oe-ord.sold-zip.        
       END.
    END.
    ELSE
       ASSIGN v-comp-add1 = v-cusx-add1
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
      
      ASSIGN
         v-salesman = trim(v-salesman)
         v-po-no = oe-boll.po-no
         v-job-no = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>")).

      if v-salesman gt '' then
        if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
          substr(v-salesman,length(trim(v-salesman)),1) = "".
   
        FOR EACH oe-rel WHERE 
            oe-rel.company EQ oe-ord.company AND 
            oe-rel.ord-no  EQ oe-ord.ord-no NO-LOCK BY oe-rel.rel-date 
                                                    BY oe-rel.po-no
                                                    BY oe-rel.ship-no 
                                                    BY oe-rel.qty:

               ASSIGN 
                     v-fob = if oe-rel.fob-code begins "O" then "Origin" else "Destination" .
        END.

        IF v-fob = "" THEN
            v-fob = if oe-ord.fob-code begins "ORIG" then "Origin" else "Destination".
      
      LEAVE.
    end.

    EMPTY TEMP-TABLE w3.
    
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
     
     {oe/rep/bollprint1.i}            
     {oe/rep/bollprint2.i}             

    v-last-page = page-number.

  /*IF oe-bolh.tot-pallets NE 0 THEN v-tot-palls = oe-bolh.tot-pallets.*/

  PUT "<R54><C50><#8><FROM><R+4><C+30><RECT> " 
    "<=8><R+1> Total Units         :" /*v-tot-palls*/  v-tot-cases
    "<=8><R+3> Total Weight        :" v-tot-wt /*fORM ">>,>>9.99"*/ .
    
  ASSIGN v-ship-i = "".
  IF v-print-shipnotes THEN
     ASSIGN v-ship-i[1] = oe-bolh.ship-i[1]
            v-ship-i[2] = oe-bolh.ship-i[2]
            v-ship-i[3] = oe-bolh.ship-i[3]
            v-ship-i[4] = oe-bolh.ship-i[4].
  
  PUT "<FArial><R50><C1><P10><B>     Dock#: " "<U>" v-dock FORMAT "x(10)" "</U>"
      "<C25>Dock Hrs: <U>"  v-dockHr FORMAT "x(10)" "</U>"
      .

  PUT "<FArial><R51><C1><P12>     Shipping Instructions: </B> <P9> " 
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
 
  PAGE.
  v-printline = 0.

  for each report where report.term-id eq v-term-id,
      first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
    delete report.
  end.

  END.  /* last-of*/

  oe-bolh.printed = yes.
end. /* for each oe-bolh */

/* END ---------------------------------- copr. 2009  Advanced Software, Inc. */
