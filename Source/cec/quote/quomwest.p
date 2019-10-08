/* ------------------------------------------- cec/quote/quomwest.p 10/02 YSK */
/* print quotes in Pacific - Xprint format   for Midwest                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xquo for quotehd.
def buffer xqitm for quoteitm.
def buffer xqqty for quoteqty.
def buffer xqchg for quotechg.
def buffer b-qi for quoteitm.
def buffer x-qi for quoteitm.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
{est/printquo.i}
def var numfit as int no-undo.
def var sold as ch extent 5 FORM "x(30)" no-undo.
def var bill as ch extent 5 FORM "x(30)" no-undo.
def var ship as ch extent 5 FORM "x(30)" no-undo.
def var tot as de no-undo.
def var v-over-under as char no-undo.
def var v-comp-name like company.name extent 4.
def var trim-size like quoteitm.size no-undo.
def var temp-trim-size like quoteitm.size no-undo.
def var cc as int no-undo.
def var v-printline as int initial 0 no-undo.
def var v-first-q-no like quotehd.q-no no-undo.
DEF VAR v-lines AS INT NO-UNDO.
def var v-rels as int.
def new shared var v-out1-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */
def new shared var v-out2-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */
def var v-part              like quoteitm.part-no                        no-undo.
def var v-board             as   char                                   no-undo.
def var v-last as log initial no no-undo.
DEF VAR v-quo-date AS DATE FORM "99/99/9999" NO-UNDO.

DEFINE VARIABLE logSetPrinting AS LOGICAL    NO-UNDO.
DEFINE VARIABLE chrX           AS CHARACTER  NO-UNDO.
DEF VAR adder-print AS LOG NO-UNDO.
DEFINE VARIABLE logPrint       AS LOGICAL    NO-UNDO.

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR style-dscr AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-est-no AS cha NO-UNDO.
DEF VAR lv-chg-amt LIKE quotechg.amt NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.  /* display company address */
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-email AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-note-lines AS INT NO-UNDO.
DEF VAR v-contact LIKE quotehd.contact NO-UNDO.
DEF VAR lv-first-qreckey LIKE quotehd.rec_key NO-UNDO.
DEF VAR lv-first-qrecid AS recid NO-UNDO.
DEF BUFFER bf-quo FOR quotehd.
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
{custom/notesdef.i}
DEF VAR v-inst2 AS cha EXTENT 6 NO-UNDO.    
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
DEF VAR v-note-length AS INT INIT 80 NO-UNDO.
DEF VAR li-cline AS INT NO-UNDO.
DEFINE VARIABLE cAdder  AS   CHARACTER NO-UNDO.

DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR lv-tot-pg AS INT INIT 1 NO-UNDO.
DEF VAR ln-cnt AS INT NO-UNDO.
DEF BUFFER bf-report FOR report.
DEF BUFFER bf-eb FOR eb.

DEF VAR lv-part-dscr1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-fg# AS cha FORM "x(15)" NO-UNDO.
DEF VAR ld-metric AS DEC INIT 1 NO-UNDO.
DEF VAR lv-format AS CHAR INIT ">>>>>9.9<<<<" NO-UNDO.
DEF VAR ld-wid AS DEC NO-UNDO.
DEF VAR ld-len AS DEC NO-UNDO.
DEF VAR ld-dep AS DEC NO-UNDO.
DEF VAR ll-prt-dscr2 AS LOG NO-UNDO.
DEF VAR lv-part-dscr2 AS cha NO-UNDO.
DEF VAR lv-i-coldscr AS cha NO-UNDO.
DEF VAR idummy AS INT NO-UNDO.
DEF VAR idx AS INT NO-UNDO.
{cecrep/jobtick2.i "new shared"}

ASSIGN tmpstore = fill("-",130).

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "QUOPRINT" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

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
            .
END.

FIND first report where report.term-id eq v-term-id NO-LOCK NO-ERROR.
FIND first xquo  where recid(xquo) eq report.rec-id NO-LOCK NO-ERROR.
IF NOT AVAIL xquo THEN RETURN.
v-quo-date = xquo.quo-date.
v-contact = xquo.contact.
find first est where est.company = xquo.company
                   AND est.est-no eq xquo.est-no no-lock no-error.
  find first sman
      where sman.company eq cocode
        and sman.sman    eq xquo.sman
      no-lock no-error.
  find first carrier
      where carrier.company eq cocode
        and carrier.carrier eq xquo.carrier
      no-lock no-error.
  find first terms
      where terms.company eq cocode
        and terms.t-code  eq xquo.terms
      no-lock no-error.
  find first cust
      where cust.company eq xquo.company
        and cust.cust-no eq xquo.cust-no
      no-lock no-error.

  if avail cust then
     v-over-under = trim(string(cust.over-pct,">>9.9<%")) + "-" +
                    trim(string(cust.under-pct,">>9.9<%")).

  assign
   sold[5] = trim(string(xquo.sold-no))
   ship[5] = trim(string(xquo.ship-id))
   bill[5] = trim(string(xquo.cust-no)).

  do i = 1 to 4:
    ASSIGN sold[i] = xquo.soldto[i]
           ship[i] = xquo.shipto[i]
           bill[i] = xquo.billto[i].
  end.

  if (xquo.shipto[1] eq xquo.soldto[1] and
      xquo.shipto[2] eq xquo.soldto[2] and
      xquo.shipto[3] eq xquo.soldto[3] and
      xquo.shipto[4] eq xquo.soldto[4]) then
    assign
     ship[1] = ""
     ship[2] = ""
     ship[3] = ""
     ship[4] = ""
     ship[5] = "SAME".

  ASSIGN
  v-first-q-no = xquo.q-no
  v-line-total = 0
  v-printline = 0.
PUT "<Farial>". 

ln-cnt = 0.
if (not ch-multi) then do:
    /* get total page number */
  FOR EACH xqqty OF xqitm NO-LOCK:
    ln-cnt = ln-cnt + 1.
  END.
  ln-cnt = ln-cnt * 5.
  FOR EACH xqqty OF xqitm NO-LOCK,
      EACH xqchg WHERE xqchg.company = xqqty.company
                   AND xqchg.loc = xqqty.loc
                   AND xqchg.q-no = xqqty.q-no
                   AND xqchg.LINE = xqqty.LINE
                   AND xqchg.qty = xqqty.qty NO-LOCK
      BREAK BY xqchg.qty
            BY xqchg.charge:
      IF FIRST-OF(xqchg.qty) THEN DO:
         IF FIRST(xqchg.qty) THEN ln-cnt = ln-cnt + 1.
         ln-cnt = ln-cnt + 1.
      END.
  END.

  FOR EACH xqchg OF xquo
      WHERE xqchg.qty  EQ 0
        AND xqchg.line EQ 0
      NO-LOCK
      BREAK BY xqchg.charge:
      IF FIRST(xqchg.charge) THEN ln-cnt = ln-cnt + 1.
  END.
  IF s-print-comp THEN DO :      /* Print components of a set */

    FIND FIRST est WHERE est.company EQ xquo.company
                     AND est.est-no  EQ xquo.est-no NO-LOCK NO-ERROR.
    IF AVAIL est AND est.est-type EQ 6 AND est.form-qty GT 1 THEN
    FOR EACH ef WHERE ef.company EQ est.company
                 AND ef.est-no  EQ est.est-no NO-LOCK,      
        EACH eb OF ef NO-LOCK BREAK BY ef.form-no :
         ln-cnt = ln-cnt + 4.
    END.
  END.
  lv-tot-pg = lv-tot-pg + TRUNC( ln-cnt / 25,0) .
   /* get total page number */

   {cec/quote/quoxpnt2.i}

   {cec/quote/quoxprnt.i 1}
   v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].
        
   PUT "<FArial><R57><C1><P12><B> Comments </B> <P9> " .
   
    v-tmp-lines = 0.
   
    li-cline = 0.
    do i = 1 to 5:
       if xquo.comment[i] ne "" then do:
          put "<C1><R" string(58 + li-cline,">9") + "><C6>" xquo.comment[i]. 
          li-cline = li-cline + 1.
       END.
    end.
    
    PUT "<C1><R" string(58 + li-cline,">9") + "><C22>" "Necessary printing plates and cutting dies to be furnished by customer " 
        "<C1><R" string(58 + li-cline + 1,">9") + ">" "<C30><B>WE HOPE THAT WE CAN BE OF SERVICE"  .

    v-printline = v-printline + 8.

    IF v-printline < 50 THEN PAGE .

    release est.
    if v-prt-box then DO:
      find first est
          where est.company eq xquo.company
            and est.loc     eq xquo.loc
            and est.est-no  eq xquo.est-no
          no-lock no-error.
      if avail est then do:
         PAGE.
         PUT "<FCouriar New>" SKIP.
         FIND FIRST xest WHERE RECID(xest) = RECID(est) NO-LOCK NO-ERROR.
         run cec/desprnt2.p (?,
                           input-output v-lines,
                           recid(xest)).
      end.
    END.
end.

else do:

    for each report where report.term-id eq v-term-id,
       first xquo  where recid(xquo) eq report.rec-id
       no-lock
       break by report.key-01
             by report.key-02
             by report.key-03
       transaction:

    find first est
        where est.company eq xquo.company
          AND est.est-no  EQ xquo.est-no
        no-lock no-error.
    find first sman
        where sman.company eq cocode
          and sman.sman    eq xquo.sman
        no-lock no-error.
    find first carrier
        where carrier.company eq cocode
          and carrier.carrier eq xquo.carrier
        no-lock no-error.
    find first terms
        where terms.company eq cocode
          and terms.t-code  eq xquo.terms
        no-lock no-error.
    find first cust
        where cust.company eq xquo.company
          and cust.cust-no eq xquo.cust-no
        no-lock no-error.

    if avail cust then
      v-over-under = trim(string(cust.over-pct,">>9.9<%")) + "-" +
                     trim(string(cust.under-pct,">>9.9<%")).


    assign
     sold[5] = trim(string(xquo.sold-no))
     ship[5] = trim(string(xquo.ship-id))
     bill[5] = trim(string(xquo.cust-no)).

    do i = 1 to 4:
      assign
       sold[i] = xquo.soldto[i]
       ship[i] = xquo.shipto[i]
       bill[i] = xquo.billto[i].
    end.
    
    IF s-sep-page OR first(report.key-01) THEN
       ASSIGN lv-first-qreckey = xquo.rec_key
              lv-first-qrecid = recid(xquo).

    if s-sep-page OR first-of(report.key-01) then do:
    /*  page.  */
      v-first-q-no = xquo.q-no.
      v-printline = 0.

      lv-tot-pg = 1.
     /* get total page number */
    FOR EACH bf-report where bf-report.term-id eq v-term-id
                         AND bf-report.key-01 = report.key-01,
        first bf-quo  where recid(bf-quo) eq bf-report.rec-id,
        EACH xqitm OF bf-quo NO-LOCK:
     
        ln-cnt = ln-cnt + 1.

    END.
    ln-cnt = ln-cnt * 6.

    FOR EACH bf-report where bf-report.term-id eq v-term-id
                         AND bf-report.key-01 = report.key-01,
        first bf-quo  where recid(bf-quo) eq bf-report.rec-id,
        EACH xqitm OF bf-quo NO-LOCK,
        EACH xqqty OF xqitm NO-LOCK,
        EACH xqchg WHERE xqchg.company = xqqty.company
                 AND xqchg.loc = xqqty.loc
                 AND xqchg.q-no = xqqty.q-no
                 AND xqchg.LINE = xqqty.LINE
                 AND xqchg.qty = xqqty.qty NO-LOCK
        BREAK BY xqchg.qty
              BY xqchg.charge:
        IF FIRST-OF(xqchg.qty) THEN DO:
          IF FIRST(xqchg.qty) THEN ln-cnt = ln-cnt + 1.
          ln-cnt = ln-cnt + 1.
        END.
    END.

    FOR EACH bf-report where bf-report.term-id eq v-term-id
                         AND bf-report.key-01 = report.key-01,
        first bf-quo  where recid(bf-quo) eq bf-report.rec-id,
        EACH xqchg OF bf-quo WHERE xqchg.qty  EQ 0 AND xqchg.line EQ 0 NO-LOCK
                BREAK BY xqchg.charge:
        IF FIRST(xqchg.charge) THEN ln-cnt = ln-cnt + 1.
    END.

    IF s-print-comp THEN DO :      /* Print components of a set */
        FOR EACH bf-report where bf-report.term-id eq v-term-id
                           AND bf-report.key-01 = report.key-01 NO-LOCK,
            first bf-quo  where recid(bf-quo) eq bf-report.rec-id NO-LOCK,
            FIRST est WHERE est.company EQ bf-quo.company
                     AND est.est-no  EQ bf-quo.est-no 
                     AND est.est-type EQ 6 AND est.form-qty GT 1 NO-LOCK,
            EACH ef WHERE ef.company EQ est.company
                 AND ef.est-no  EQ est.est-no NO-LOCK,      
            EACH eb OF ef NO-LOCK BREAK BY ef.form-no :
            ln-cnt = ln-cnt + 4.
        END.
    END.
    lv-tot-pg = lv-tot-pg + TRUNC( ln-cnt / 25,0) .
    /* get total page number */

    end.
   
    IF s-sep-page or FIRST-OF(report.key-01) THEN DO:
       {cec/quote/quoxpnt2.i}           
    END. /* key-03 */
    v-last = last-of(report.key-01).
    v-line-total = 0.
  
    {cec/quote/quoxprnt.i 2}  
    v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].

   IF LINE-COUNTER > PAGE-SIZE - 1 THEN DO:      
      v-printline = 0.
      page.  
      {cec/quote/quoxpnt2.i}
   END.

   IF (ch-multi and (v-last OR s-sep-page)) then do:
      PUT "<FArial><R57><C1><P12><B> Comments </B> <P9> " .

     FIND bf-quo WHERE RECID(bf-quo) = lv-first-qrecid NO-LOCK NO-ERROR. 
     li-cline = 0.
     do i = 1 to 5:      
        if bf-quo.comment[i] ne "" then DO:
            li-cline = li-cline + 1.
            put "<C1><R" string(57 + li-cline,">9") + "><C6>" bf-quo.comment[i]  .  
        END.            
     end.
    
     put "<C1><R" string(57 + li-cline + 2,">9") + "><C22>"  "Necessary printing plates and cutting dies to be furnished by customer " 
         "<C1><R" string(57 + li-cline + 3,">9") + "><C30>"  "<B>WE HOPE THAT WE CAN BE OF SERVICE".  

     v-printline = v-printline + 8.
     IF v-printline < 50 THEN DO:
        page.
     END.

     if v-prt-box then DO:
        find first est where est.company eq xquo.company
                   and est.loc     eq xquo.loc
                   and est.est-no  eq xquo.est-no
                   no-lock no-error.
        if avail est then do:
          PAGE.
          PUT "<FCouriar New>" SKIP.
          FIND FIRST xest WHERE RECID(xest) = RECID(est) NO-LOCK NO-ERROR.
          run cec/desprnt2.p (?,
                          input-output v-lines,
                          recid(xest)).
        end.
     END.

   END.
  end. /* for each report */
  
end.  /* multi */


PROCEDURE printHeader:
  DEFINE INPUT PARAMETER ipPageOffSet AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opInitVar AS INTEGER NO-UNDO.

  IF LINE-COUNTER > PAGE-SIZE - ipPageOffSet THEN DO:
    PAGE.
    {cec/quote/quoxpnt2.i}
    opInitVar = 0.
  END.
END PROCEDURE.

/* end ---------------------------------- copr. 2000  advanced software, inc. */
