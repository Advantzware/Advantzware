/* bolharwell.i - used by bolHarwell.p */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.
def buffer xxreport     for report.
{oe/bolSign.i}
{oe/rep/oe-lad.i}

def var v-salesman          as   char format "x(26)".
def var v-fob               as   char format "x(12)".
def var v-tot-cases         as   int format ">>>>9".
def var v-tot-wt            as   dec format ">>,>>>,>>9".

def var v-tot-pkgs          as   int format ">>9".
def var v-ord-qty           like oe-ordl.qty.
def var v-bol-qty           like oe-boll.qty.
def var v-ship-qty          like oe-ordl.ship-qty.
def var v-bol-wt            as   dec.
def var v-part-dscr         as   char format "x(30)".
def var v-part-comp         as   char format "x".
def var v-part-qty          as   dec.
def var v-ord-no            like oe-boll.ord-no.
def var v-po-no             like oe-bolh.po-no.
def var v-phone-num         as   char format "x(13)" no-undo.

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
DEF VAR v-i-part-no  AS CHAR NO-UNDO.
DEF VAR v-ord-po-no  AS CHAR NO-UNDO.
DEF VAR v-descr      AS CHAR NO-UNDO.
def var v-job-po            as   CHAR NO-UNDO.
DEF VAR v-grand-total-cases as   int format ">>>>9".
DEF VAR v-summ-case-tot AS INTE NO-UNDO.

def var v-terms like oe-ord.terms-d no-undo.
def var v-frt-terms as char format "x(10)" no-undo.
def var v-zone like carr-mtx.del-zone no-undo.
DEF VAR v-total-weight LIKE tt-boll.weight.
DEFINE VARIABLE iReprint AS INTEGER     NO-UNDO.

def TEMP-TABLE w2 no-undo
    field cases            as   int format ">9"
    field cas-cnt          as   int format ">>>>9"
    FIELD ord-po-no        AS CHAR
    FIELD rec-id AS RECID
    FIELD i-no LIKE oe-ordl.i-no
    FIELD job-po AS cha
    FIELD qty AS INT 
    FIELD dscr LIKE oe-ordl.part-dscr1.

def TEMP-TABLE w3 no-undo
    field ship-i           as   char format "x(60)".
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR cSignatureFile AS CHAR FORM "X(200)" NO-UNDO.

/*ASSIGN ls-image1 = "images\premier{&fmt}.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".*/

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

DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR v-trailer AS cha FORMAT "x(20)" NO-UNDO.
DEF VAR comp-ctr AS INTE NO-UNDO.

def buffer b-itemfg     for itemfg.
DEF BUFFER bf-ttboll FOR tt-boll.
DEF VAR v-tot-case-qty AS INT NO-UNDO.
DEF VAR ll-consol-bolls AS LOG NO-UNDO.
DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR v-lines AS INT NO-UNDO.
def var v-job            as   CHAR NO-UNDO.
DEF VAR v-description       AS CHAR NO-UNDO.
DEF VAR v-i-no              AS CHAR NO-UNDO.
DEF VAR v-item-part-no      AS CHAR NO-UNDO.
DEF VAR v-case-tot          AS INTE NO-UNDO.
DEF VAR cOrderDate          AS CHAR NO-UNDO.
DEF VAR dOrigQty            AS DEC NO-UNDO.
DEF VAR cOrigUom            AS CHAR NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cImageFooter AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE lValid         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BOLImageFooter", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    cImageFooter = cRtnChar NO-ERROR. 

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


form 
     w2.i-no                         format "x(15)" AT 2
     w2.ord-po-no                    format "x(16)"
     w2.dscr                         format "x(32)"
     w2.cases                        format "->>>>>9"
     w2.cas-cnt                      format "->>>>,>>Z"
     v-case-tot                      FORMAT "->>,>>>,>>z"
    with frame bol-mid down no-box no-labels stream-io width 110.
form 
     v-item-part-no                       format "x(15)" AT 2
     v-ord-po-no                          format "x(16)"
     v-part-dscr                          format "x(32)"
     w2.cases                             format "->>>>>9"
     w2.cas-cnt                           format "->>>>,>>Z"
     v-case-tot                           FORMAT "->>,>>>,>>z"
    with frame bol-mid2 down no-box no-labels stream-io width 110.

form v-tot-pkgs                     to 04
     v-ord-qty                      to 10 format ">>>>>>"
     v-part-dscr                    at 13 format "x(27)"
     oe-boll.po-no                  at 42 format "x(14)"
     w2.cases                       to 67.5 format ">>9"
     w2.cas-cnt                     to 76.5 format ">>>>9"
     v-bol-qty                      to 84 format ">>>>>9"
     oe-boll.p-c                    at 85
     v-bol-wt                       to 94.5 format ">>>>>9"
    with frame bol-mid1 down no-box no-labels stream-io width 110.

ASSIGN tmpstore = fill("-",130).

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "BOLFMT" no-lock no-error.
ASSIGN
 ll-consol-bolls = AVAIL sys-ctrl AND sys-ctrl.int-fld NE 0.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company = cocode no-lock no-error.

ASSIGN v-comp-add1 = "3870  NE  33rd  Street"
       v-comp-add2 = "Ocala,   FL   34479" 
       v-comp-add3 = "Phone:  (352)401-9000"
       v-comp-add4 = "FAX   :  (352)401-9226"
       v-comp-add5 = "".

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
            .
END.

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

{sa/sa-sls01.i}


find first oe-ctrl where oe-ctrl.company eq cocode no-lock.

v-printline = 0.

DISABLE TRIGGERS FOR LOAD OF oe-bolh.
DISABLE TRIGGERS FOR LOAD OF oe-boll.

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
                       cust.zip
      /* 05291402 */
      v-ship-city    = shipto.ship-city
      v-ship-state   = shipto.ship-state
      v-ship-zip     = shipto.ship-zip
      v-comp-city    = cust.city
      v-comp-state   = cust.state
      v-comp-zip     = cust.zip
      /* 05291402 */
     .
    /*kludgy workaround to import bug*/
    IF v-ship-addr[2] = '345 Court Street' THEN v-ship-addr[2] = ''.

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

    FOR EACH oe-boll NO-LOCK
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.b-no    EQ oe-bolh.b-no,
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
         v-job-po = IF oe-boll.job-no = "" THEN "" ELSE 
                    TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', oe-boll.job-no, oe-boll.job-no2))).

      if v-salesman gt '' then
        if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
          substr(v-salesman,length(trim(v-salesman)),1) = "".

      v-fob = if oe-ord.fob-code begins "ORIG" then "Origin" else "Destination".
      
      LEAVE.
    end.

    for each w3:
      delete w3.
    end.
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
  j = 0.
  
  if last-of(oe-bolh.bol-no) then do:
     IF v-comp-addr[2] = "" THEN
        ASSIGN v-comp-addr[2] = v-comp-addr3
               v-comp-addr3 = "".

     IF v-ship-addr[2] = "" THEN
        ASSIGN v-ship-addr[2] = v-ship-addr3
               v-ship-addr3 = "".
    
     v-trailer = oe-bolh.trailer.
 
    {oe/rep/bolHarwell2.i} /* header */
    {oe/rep/bolHarwell3.i}

    v-last-page = page-number.
  end.
  
  cSignatureFile = "".
  RUN oe\GetBolSign.p (cocode,ROWID(oe-bolh),NO,NO,OUTPUT cSignatureFile).

  IF cSignatureFile NE "" THEN
      cSignatureFile = cSignatureFile + ">".

 IF v-footer THEN do:
     PUT "<FArial><C2><R46><#3><R+5><C+160><IMAGE#3=" cImageFooter + ">" FORMAT "x(200)"  .
 END.

  PUT "<R56><C53><#8><FROM><R+4><C+27><RECT> " 
      "<=8><R+1> Unités/Units Total:" v-grand-total-cases format ">>,>>>,>>9"
      "<=8><R+3> Poids/Weight Total:" v-tot-wt format ">>,>>>,>>9".
  
  PUT "<FArial><R55><C2><P12><B> Instructions de Livraison/Delivery Instructions: <P10> "
      "<R56.5><C3.5>" oe-bolh.ship-i[1] 
      "<R57.5><C3.5>" oe-bolh.ship-i[2] 
      "<R58.5><C3.5>" oe-bolh.ship-i[3] 
      "<R59.5><C3.5>" oe-bolh.ship-i[4] "</B><P9>".
  
  PUT "<R64><C2>RECU EN BON ORDRE PAR"
      "<R65><C2>RECEIVED IN GOOD ORDER BY _____________________________________________ DATE ___________________________"
      "<R66><C2>____________________________________________________________________________________________________________________________"
      "<R67><C2>TOUTE MATRICE EN ACIER ET MATRICE D'IMPRESSION NON-                 ANY STEEL DIE OR PRINT PLATE NOT USED FOR MORE THAN"  
      "<R68><C2>UTILISEE DEPUIS PLUS DE DEUX(2) ANS DETRUITE SANS PRE-AVIS     TWO(2) YEARS WILL BE DESTROYED WITHOUT PRIOR NOTICE"
      
      "<RESTORE=LPI>".
 

  v-printline = v-printline + 14. 
  
  PAGE.   

  ASSIGN
  v-printline = 0
  oe-bolh.printed = yes
  v-grand-total-cases = 0.

  /* BOLCERT block*/
  if last-of(oe-bolh.bol-no) THEN DO:
    FIND FIRST sys-ctrl-shipto WHERE
     sys-ctrl-shipto.company = cocode AND
     sys-ctrl-shipto.NAME = "BOLCERT" AND
     sys-ctrl-shipto.cust-vend = YES AND
     sys-ctrl-shipto.cust-vend-no = oe-bolh.cust-no AND
     sys-ctrl-shipto.ship-id      = oe-bolh.ship-id
     NO-LOCK NO-ERROR. /*AND
     sys-ctrl-shipto.char-fld = "PremierPkg")*/
    IF NOT AVAIL sys-ctrl-shipto THEN
     FIND FIRST sys-ctrl-shipto WHERE
                sys-ctrl-shipto.company = cocode AND
                sys-ctrl-shipto.NAME = "BOLCERT" AND
                sys-ctrl-shipto.cust-vend = YES AND
                sys-ctrl-shipto.cust-vend-no = oe-bolh.cust-no
                NO-LOCK NO-ERROR. /*AND
                sys-ctrl-shipto.char-fld = "PremierPkg")) THEN */
    IF AVAIL sys-ctrl-shipto THEN DO:
        CASE sys-ctrl-shipto.char-fld:
            WHEN "PremierPkgU" THEN
                RUN oe/rep/cocprempkgu.p(INPUT recid(oe-bolh)).
            WHEN "PremierPkgM" THEN
                RUN oe/rep/cocprempkgm.p(INPUT RECID(oe-bolh)).
            OTHERWISE
                RUN oe/rep/cocprempkg.p(INPUT recid(oe-bolh)).
        END CASE.
        for each report where report.term-id eq v-term-id,
            first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
            delete report.
        end.
     END.
  END. /*last of bolh.bol-no and end of BOL CERT block*/
end. /* for each oe-bolh */

for each report where report.term-id eq v-term-id,
    first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
    delete report.
end.

PROCEDURE create-tt-boll.
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
