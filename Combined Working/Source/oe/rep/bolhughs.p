/* ---------------------------------------------- oe/rep/bolhughs.p */
/* PRINT Hughes BOL                                                           */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.
def buffer xxreport     for report.

{oe/rep/oe-lad.i}

def var v-salesman          as   char format "x(26)" NO-UNDO.
def var v-fob               as   char format "x(12)" NO-UNDO.
def var v-tot-cases         as   int format ">>>>9" NO-UNDO.
def var v-tot-wt            as   dec format ">>,>>>,>>9" NO-UNDO.
DEF VAR v-pallets           AS   INT NO-UNDO.
DEF VAR v-calc-pallets      AS   INT NO-UNDO.
def var v-tot-pkgs          as   int format ">>9" NO-UNDO.
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
/*def var v-1          LIKE oe-boll.cases INIT 1 no-undo.*/
def var v-1          LIKE oe-boll.tot-pallets INIT 1 no-undo.

def var v-terms like oe-ord.terms-d no-undo.
def var v-frt-terms as char format "x(10)" no-undo.
def var v-zone like carr-mtx.del-zone no-undo.

def TEMP-TABLE w2 no-undo
    field cases            as   int format ">9"
    field cas-cnt          as   int format ">>>>9".

def TEMP-TABLE w3 no-undo
    field ship-i           as   char format "x(60)".

{fg/fullset.i NEW}

DEF VAR v-part-d AS DEC EXTENT 3 NO-UNDO.
DEF VAR v-part-dims AS CHAR NO-UNDO.
DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.

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
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR v-rel-qty LIKE oe-rell.qty NO-UNDO.
DEF VAR ll-consol-bolls AS LOG NO-UNDO.
DEF VAR v-gt-package AS INT NO-UNDO.
DEF VAR v-gt-qty     AS INT NO-UNDO.
DEF VAR v-boll-qty   AS INT NO-UNDO.
DEF VAR v-gt-weight AS DEC NO-UNDO.
DEF VAR v-gt-cases  AS INT NO-UNDO.
DEF VAR v-2nd-line-printed AS LOG NO-UNDO.
assign tmpstore = fill("-",80).

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

{sa/sa-sls01.i}
{sys/inc/f16to32.i}
DEF BUFFER bf-tt-boll FOR tt-boll.
find first company where company.company eq cocode no-lock.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "BOLFMT" no-lock no-error.
ASSIGN
 ll-consol-bolls = AVAIL sys-ctrl AND sys-ctrl.int-fld NE 0.

ASSIGN v-comp-add1 = company.addr[1]
       v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
       v-comp-add3 = "Phone: 604.533.2545" 
       v-comp-add4 = "Fax  : 604.533.2633"
      v-printline = 0.

for each xxreport where xxreport.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh)   eq xxreport.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq oe-bolh.cust-no
    no-lock

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
     v-comp-name    = cust.name
     v-comp-addr[1] = cust.addr[1]
     v-comp-addr[2] = cust.addr[2]
     v-comp-addr3   = cust.city + ", " +
                      cust.state + "  " +
                      cust.zip.
     FIND FIRST oe-boll WHERE oe-boll.company = oe-bolh.company
                          AND oe-boll.bol-no  = oe-bolh.bol-no
                        NO-LOCK NO-ERROR.
     IF AVAIL oe-boll AND oe-boll.s-code = "T" THEN DO:
         FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.
         IF AVAIL company THEN
             ASSIGN v-comp-name = company.NAME
                    v-comp-addr[1] = company.addr[1]
                    v-comp-addr[2] = company.addr[2]
                    v-comp-addr3 = company.city + ", " + 
                                   company.state + "  " + 
                                   company.zip.
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
             v-frt-terms = if cust.frt-pay eq "P" then "Prepaid"
                           else if cust.frt-pay eq "B" then "Bill"
                           else if cust.frt-pay eq "C" then "Collect"
                           else if cust.frt-pay eq "T" then "Third Party"
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
      v-job-no = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>"))
      v-fob = if oe-ord.fob-code begins "ORIG" then "Origin" else "Destination".

      if v-salesman gt '' then
        if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
          substr(v-salesman,length(trim(v-salesman)),1) = "".

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
     report.key-03   = STRING(oe-boll.qty-case)
     report.rec-id   = recid(oe-boll)
     oe-boll.printed = yes.

    IF ll-consol-bolls THEN DO:
      /*IF oe-boll.qty - (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
          v-calc-pallets = oe-boll.tot-pallets - 1.
      ELSE*/
          v-calc-pallets = oe-boll.tot-pallets.
      IF (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
        RUN create-tt-boll (oe-boll.qty, oe-boll.cases, v-calc-pallets).
      /*IF oe-boll.qty - (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
        RUN create-tt-boll (oe-boll.qty - (oe-boll.qty-case * oe-boll.cases), 1, 1).*/
    END.
    ELSE DO:
      CREATE tt-boll.
      BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll.
    END.

  end.

  if last-of(oe-bolh.bol-no) then do:
     IF v-comp-addr[2] = "" THEN
           ASSIGN v-comp-addr[2] = v-comp-addr3
                  v-comp-addr3 = "".
     IF v-ship-addr[2] = "" THEN
           ASSIGN v-ship-addr[2] = v-ship-addr3
                  v-ship-addr3 = "".
     {oe/rep/bolhugh1.i}
    {oe/rep/bolhughs.i}

    v-last-page = page-number.


    for each report where report.term-id eq v-term-id,
        first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
      delete report.
    end.
    FOR EACH tt-boll.
        DELETE tt-boll.
    END.
  

  PUT "<R48><C50><#8><FROM><R+4><C+30><RECT> " 
    "<=8><R+1> Total Pallets       :" oe-bolh.tot-pallets /*v-tot-cases */
    "<=8><R+3> Total Weight        :" v-tot-wt FORM ">>,>>9.99".
    

PUT "<FArial><R46><C1><P12><B>     Shipping Instructions: </B> <P9> " SKIP(1)
    oe-bolh.ship-i[1] AT 7 SKIP
    oe-bolh.ship-i[2] AT 7 SKIP
    oe-bolh.ship-i[3] AT 7 SKIP
    oe-bolh.ship-i[4] AT 7 SKIP(1)
    "__________________________________________________________________________________________________________________" SKIP
    "<B>  Signature of Receipt </B>" SKIP
    "Customer ________________________________________                       Carrier _______________________________________" AT 20 SKIP(1)
    "Date ____________________________________________                       Date _________________________________________" AT 20 SKIP   
    .

  v-printline = v-printline + 14.
  
  PAGE.
  v-printline = 0.
 END.

  oe-bolh.printed = yes.
end. /* for each oe-bolh */


PROCEDURE create-tt-boll.
  DEF INPUT PARAM ip-qty-case LIKE oe-boll.qty-case NO-UNDO.
  DEF INPUT PARAM ip-cases    LIKE oe-boll.cases NO-UNDO.
  DEF INPUT PARAM ip-pallets  LIKE oe-boll.tot-pallets NO-UNDO.

  IF ip-qty-case LT 0 THEN
    ASSIGN
     ip-qty-case = ip-qty-case * -1
     ip-cases    = ip-cases * -1.

  FIND FIRST tt-boll
      WHERE tt-boll.i-no     EQ oe-boll.i-no
/*        AND tt-boll.po-no    EQ oe-boll.po-no
        AND tt-boll.ord-no   EQ oe-boll.ord-no
        AND tt-boll.line     EQ oe-boll.line */
        AND tt-boll.qty-case EQ ip-qty-case
      NO-LOCK NO-ERROR.

  IF NOT AVAIL tt-boll THEN DO:
    CREATE tt-boll.
    BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll
    ASSIGN
     tt-boll.qty-case = ip-qty-case
     tt-boll.cases    = 0
     tt-boll.qty      = 0
     tt-boll.weight   = 0
     tt-boll.partial  = 0
     tt-boll.tot-pallets = 0
     tt-boll.tot-qty     = 0  .
  END.

  ASSIGN
   tt-boll.cases  = tt-boll.cases + ip-cases
   tt-boll.qty    = /*tt-boll.qty +*/ /*(ip-qty-case * ip-cases)*/  ip-qty-case
   tt-boll.weight = tt-boll.weight + oe-boll.weight 
                   /* ((oe-boll.qty-case * ip-cases) / oe-boll.qty * oe-boll.weight)*/
   tt-boll.tot-pallets = tt-boll.tot-pallets + ip-pallets
   tt-boll.tot-qty = tt-boll.tot-qty + ip-qty-case .

  IF oe-boll.p-c THEN tt-boll.p-c = YES.

END PROCEDURE.
/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

