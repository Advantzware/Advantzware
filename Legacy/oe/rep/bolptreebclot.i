/* ---------------------------------------------- oe/rep/bolptreebc.p 01/2011 btr */
/* PRINT PREACHTREE BOL    - cloned from bolcscin.p                                                       */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.
def buffer xxreport     for report.

{oe/rep/oe-lad.i}

def var v-salesman          as   char format "x(26)" no-undo.
def var v-fob               as   char format "x(12)" no-undo.
def var v-tot-palls         as   int format "->,>>>,>>9" no-undo.
def var v-tot-wt            as   dec format "->>,>>>,>>9" no-undo.

def var v-tot-pkgs          as   int format ">>9" no-undo.
def var v-pal-cnt           as   DEC no-undo.
def var v-ord-qty           like oe-ordl.qty no-undo.
def var v-bol-qty           like oe-boll.qty no-undo.
def var v-ship-qty          like oe-ordl.ship-qty no-undo.
def var v-bol-wt            as   DEC no-undo.
def var v-part-dscr         as   char format "x(30)" no-undo.
def var v-part-comp         as   char format "x" no-undo.
def var v-part-qty          as   DEC no-undo.
def var v-ord-no            like oe-boll.ord-no no-undo.
def var v-po-no             like oe-bolh.po-no no-undo.
def var v-job-no            as   char format "x(9)" no-undo.
def var v-phone-num         as   char format "x(13)" no-undo.

def var v-ship-name  like shipto.ship-name no-undo.
def var v-ship-addr  like shipto.ship-addr no-undo.
def var v-ship-city  like shipto.ship-city no-undo.
def var v-ship-state like shipto.ship-state no-undo.
def var v-ship-zip   like shipto.ship-zip no-undo.
def var v-ship-addr3 as   char format "x(30)" no-undo.
def var v-comp-name  like company.NAME no-undo.
def var v-comp-addr  like company.addr no-undo.
def var v-comp-city  like company.city no-undo.
def var v-comp-state like company.state no-undo.
def var v-comp-zip   like company.zip no-undo.
def var v-comp-addr3 as   char format "x(30)" no-undo.
def var v-cust-addr3 as   char format "x(30)" no-undo.
def var v-1          LIKE oe-boll.cases INIT 1 no-undo.
DEF VAR vll-is-transfer AS LOGICAL INITIAL FALSE NO-UNDO.
DEF VAR vlc-transfer AS CHAR FORMAT "x(10)" INITIAL "" NO-UNDO.
DEF VAR lv-bolfmt-int AS INT NO-UNDO.
DEF VAR v-i-part-no  AS CHAR NO-UNDO.
DEF VAR v-ord-po-no  AS CHAR NO-UNDO.
DEF VAR v-descr      AS CHAR NO-UNDO.
def var v-job-po            as   CHAR NO-UNDO.
DEF VAR v-shipto-contact LIKE shipto.contact NO-UNDO.
DEF VAR v-phone AS cha NO-UNDO FORMAT "x(13)".
DEF VAR v-ship-phone        AS   CHAR FORMAT "X(13)" NO-UNDO.
def var v-weight LIKE oe-boll.weight no-undo.
DEF VAR v-tmp-lines AS DEC NO-UNDO.
DEF VAR v-summ-case-tot AS INTE NO-UNDO.

def var v-terms like oe-ord.terms-d no-undo.
def var v-frt-terms as char format "x(10)" no-undo.
def var v-zone like carr-mtx.del-zone no-undo.
DEF VAR v-carrier AS cha FORM "x(20)" NO-UNDO.
DEF VAR v-total-weight LIKE tt-boll.weight no-undo.
DEF VAR v-sqft AS DEC FORM ">>>>>.<<<<" NO-UNDO.
DEF VAR v-str AS CHAR NO-UNDO.
DEF VAR v-pg-num AS INT NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEF VAR v-count2 AS INT NO-UNDO.
DEF VAR v-par-comp LIKE oe-boll.p-c NO-UNDO.
DEF VARIABLE cPc AS CHARACTER NO-UNDO .
{fg/fullset.i NEW}

def TEMP-TABLE w2 no-undo
    field cases            as   int format ">9"
    field cas-cnt          as   int format ">>>>9"
    FIELD ord-po-no        AS CHAR
    FIELD rec-id AS RECID
    FIELD i-no LIKE oe-ordl.i-no
    FIELD job-po AS cha
    FIELD qty AS INT 
    FIELD dscr LIKE oe-ordl.part-dscr1 
    FIELD partl LIKE oe-boll.p-c.
    
def TEMP-TABLE w3 no-undo
    field ship-i           as   char format "x(60)".
     
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.


/*ASSIGN ls-image1 = "images\Peachtree.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".*/

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).

ASSIGN ls-full-img1 = cRtnChar + ">" .

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .
DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.

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

DEF VAR comp-ctr AS INTE NO-UNDO.

def buffer b-itemfg     for itemfg.
DEF BUFFER bf-ttboll FOR tt-boll.
DEF VAR v-tot-case-qty AS INT NO-UNDO.
DEF VAR ll-consol-bolls AS LOG INIT FALSE NO-UNDO.
DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR v-lines AS INT NO-UNDO.
def var v-job            as   CHAR NO-UNDO.
DEF VAR v-description       AS CHAR NO-UNDO.
DEF VAR v-i-no              AS CHAR NO-UNDO.
DEF VAR v-item-part-no      AS CHAR NO-UNDO.
DEF VAR v-case-tot          AS INTE NO-UNDO.

form 
     w2.i-no                         format "x(15)"
     w2.ord-po-no                    format "x(16)"
     w2.dscr                         format "x(30)"
     w2.cases                        format "->>>>>9"
     w2.cas-cnt                      format "->>>>,>>Z"
     v-case-tot                      FORMAT "->>,>>>,>>z"
     cPc                             FORMAT "x(1)"
    with frame bol-mid down no-box no-labels stream-io width 120.
form 
     v-item-part-no                       format "x(15)"
     v-ord-po-no                          format "x(16)"
     v-part-dscr                          format "x(30)"
     w2.cases                             format "->>>>>9"
     w2.cas-cnt                           format "->>>>,>>Z"
     v-case-tot                           FORMAT "->>,>>>,>>z"
     cPc                             FORMAT "x(1)"
    with frame bol-mid2 down no-box no-labels stream-io width 120.

ASSIGN tmpstore = fill("-",130).

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "BOLFMT" no-lock no-error.
IF AVAIL sys-ctrl THEN 
   ASSIGN lv-display-comp = sys-ctrl.log-fld 
          lv-bolfmt-int = sys-ctrl.int-fld.

ASSIGN
 ll-consol-bolls = AVAIL sys-ctrl AND sys-ctrl.int-fld NE 0.

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

for each xxreport where xxreport.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh)   eq xxreport.rec-id,
    first cust
    where cust.company eq cocode
      and cust.cust-no eq oe-bolh.cust-no
    NO-LOCK
    break by oe-bolh.bol-no:
  
    ASSIGN
       v-pg-num = 1
       v-count = v-count + 1.

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
     v-phone        = string(cust.area-code,"(999)") + string(cust.phone,"999-9999")
     v-ship-phone   = string(shipto.area-code,"(999)") + string(shipto.phone,"999-9999").
     
    if shipto.broker then DO:
      
       assign
       v-comp-add1 = cust.addr[1]
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
                           else if oe-bolh.frt-pay eq "T" then "3rd Party"
                           else ""
             v-zone = cust.del-zone.
             
      if v-terms eq "" then
      do:
        find first terms where terms.t-code eq oe-ord.terms no-lock no-error.
        if avail terms THEN
          assign v-terms = terms.dscr.
      end.
      
      ASSIGN
      v-salesman = trim(v-salesman)
      v-po-no = oe-boll.po-no
      v-job-no = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>")).
      if v-salesman gt '' then
        if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
          substr(v-salesman,length(trim(v-salesman)),1) = "".

      v-fob = if oe-ord.fob-code begins "ORIG" then "Origin" else "Destination".

      IF oe-boll.s-code EQ "T" OR oe-ord.type EQ "T" THEN
         vll-is-transfer = TRUE.
      ELSE
         vll-is-transfer = FALSE.

      LEAVE.
    end.

    EMPTY TEMP-TABLE w3.
    
    FOR EACH tt-boll:
       DELETE tt-boll.
    END.
  end. /* first-of(oe-bolh.bol-no) */

  do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then do:
      find first w3 where w3.ship-i eq oe-bolh.ship-i[i] no-error.
      if not avail w3 then create w3.
      w3.ship-i = oe-bolh.ship-i[i].
    end.
  end.

  for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    /*create report.
    assign
     report.term-id  = v-term-id
     report.key-01   = oe-boll.i-no
     report.key-02   = string(oe-boll.ord-no,"9999999999")
     report.rec-id   = recid(oe-boll)*/
     oe-boll.printed = yes.
  end.

FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    IF ll-consol-bolls THEN DO:
      IF (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
        RUN create-tt-boll (oe-boll.qty-case, oe-boll.cases).

      IF oe-boll.qty - (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
        RUN create-tt-boll (oe-boll.qty - (oe-boll.qty-case * oe-boll.cases), 1).
    END.

    ELSE DO:
      CREATE tt-boll.
      BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll.
    END.

    oe-boll.printed = YES.
  END.

  if last-of(oe-bolh.bol-no) then do:
     IF v-comp-addr[2] = "" THEN
        ASSIGN v-comp-addr[2] = v-comp-addr3
               v-comp-addr3 = "".

     IF v-ship-addr[2] = "" THEN
           ASSIGN v-ship-addr[2] = v-ship-addr3
                  v-ship-addr3 = "".
     ASSIGN
        lv-email = ""
        v-carrier = carrier.dscr.

     PUT UNFORMATTED "[@s" + STRING(v-count) + "]" .

     v-str = "[=@e" + STRING(v-count) + "-@s" + STRING(v-count) + "+1]".

     /* header information */
     IF NOT ll-consol-bolls THEN DO:
        {oe/rep/bolptreebc2.i}
     END.
     ELSE DO:
         {oe/rep/bolptreebc2c.i}
     END.
    
  {oe/rep/bolptreebc3.i}

  v-last-page = page-number.

  IF oe-bolh.tot-pallets NE 0 THEN v-tot-palls = oe-bolh.tot-pallets.

  PUT UNFORMATTED "[@e" + STRING(v-count) + "]".

  PUT "<R60><C50><#8><FROM><R+4><C+30><RECT>" 
      "<=8><R+1> Total Pallets       :" v-tot-palls.
  PUT "<FArial>".
  IF v-print-shipnotes THEN 
  PUT "<R57><C1><P12><B>     Shipping Instructions: </B> <P9> " 
    "<R59><C1>" oe-bolh.ship-i[1] AT 7 /* 53 */
    "<R60><C1>" oe-bolh.ship-i[2] AT 7 
    "<R61><C1>" oe-bolh.ship-i[3] AT 7 
    "<R62><C1>" oe-bolh.ship-i[4] AT 7 .
  PUT
    "<R64><C1>"
    "__________________________________________________________________________________________________________________" 
    "<R65><C1>" "<B>  Signature of Receipt </B>" 
    "" "Customer ________________________________________                       Date _______________________________________" AT 20 
    /*"<R62><C1>" "Date ____________________________________________                       Date _________________________________________" AT 20    */
    .
  PAGE.

  v-printline = 0.

  for each report where report.term-id eq v-term-id,
      first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
      delete report.
  end.

  END.  /* last-of*/

  oe-bolh.printed = yes.
end. /* for each oe-bolh */
PROCEDURE create-tt-boll.  /* btr */
  DEF INPUT PARAM ip-qty-case LIKE oe-boll.qty-case NO-UNDO.
  DEF INPUT PARAM ip-cases    LIKE oe-boll.cases NO-UNDO.


  IF ip-qty-case LT 0 THEN
    ASSIGN
     ip-qty-case = ip-qty-case * -1
     ip-cases    = ip-cases * -1.

  FIND FIRST tt-boll
      WHERE tt-boll.i-no     EQ oe-boll.i-no
        AND tt-boll.po-no    EQ oe-boll.po-no
        AND tt-boll.ord-no   EQ oe-boll.ord-no
        AND tt-boll.line     EQ oe-boll.line
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
     tt-boll.partial  = 0.
  END.

  ASSIGN
   tt-boll.cases  = tt-boll.cases + ip-cases
   tt-boll.qty    = tt-boll.qty + (ip-qty-case * ip-cases)
   tt-boll.weight = tt-boll.weight + 
                    ((ip-qty-case * ip-cases) / oe-boll.qty * oe-boll.weight).

  IF oe-boll.p-c THEN tt-boll.p-c = YES.

END PROCEDURE.

PROCEDURE pGetP-C:
  DEFINE OUTPUT parameter opcP-c AS CHARACTER .
  DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
  DEF VAR bolPartial-char AS CHAR NO-UNDO.
  DEF VAR v-sum-qty LIKE oe-boll.qty NO-UNDO.
  DEF BUFFER bf-oe-ordl FOR oe-ordl.
  DEF BUFFER tmp-oe-boll FOR oe-boll.
  DEF VAR v-p-c LIKE oe-boll.p-c NO-UNDO.
  DEF VAR iSumRelQty LIKE oe-rell.qty NO-UNDO.
  DEFINE BUFFER bf-oe-rell FOR oe-rell.

  RUN sys/ref/nk1look.p (INPUT cocode, "BOLPartial", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
 OUTPUT cRtnChar, OUTPUT lRecFound).

IF lRecFound THEN
    bolPartial-char = cRtnChar NO-ERROR. 


    FIND FIRST bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ tt-boll.company
          AND bf-oe-ordl.ord-no  EQ tt-boll.ord-no
          AND bf-oe-ordl.i-no    EQ tt-boll.i-no
        NO-ERROR.

  find first oe-rell no-lock
      where oe-rell.company eq tt-boll.company
        and oe-rell.ord-no  eq tt-boll.ord-no
        and oe-rell.i-no    eq tt-boll.i-no
        and oe-rell.line    eq tt-boll.line no-error.

  IF ll-consol-bolls = YES THEN DO:   
  iSumRelQty = 0.
  FOR EACH bf-oe-rell no-lock
      where bf-oe-rell.company eq tt-boll.company
        and bf-oe-rell.ord-no  eq tt-boll.ord-no
        and bf-oe-rell.i-no    eq tt-boll.i-no :
       iSumRelQty = iSumRelQty + bf-oe-rell.qty.
  END.
  v-sum-qty = 0.
  FOR EACH tmp-oe-boll FIELDS(qty) NO-LOCK
      WHERE tmp-oe-boll.company EQ bf-oe-ordl.company
      AND tmp-oe-boll.ord-no  EQ bf-oe-ordl.ord-no
      AND tmp-oe-boll.i-no    EQ bf-oe-ordl.i-no 
      USE-INDEX ord-no:
      v-sum-qty = v-sum-qty + tmp-oe-boll.qty.

  END.
  IF bolPartial-char eq "Release Quantity" and avail oe-rell THEN DO:
        v-p-c = v-sum-qty ge iSumRelQty.  
  END.
  ELSE DO:
      v-p-c = tt-boll.p-c.
  END.
END.
ELSE DO:
  IF bolPartial-char eq "Release Quantity" and avail oe-rell THEN DO:
        v-p-c = tt-boll.qty ge oe-rell.qty.  
  END.
  ELSE DO:
      v-p-c = tt-boll.p-c.
  END.
END.
  
  opcP-c = IF v-p-c EQ YES THEN "C" ELSE "P".

END PROCEDURE.




/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

