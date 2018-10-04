/* -------------------------------------------------- ar/ar-aging.i 01/97 JLF */
/* A/R Aged Receivables Report Program - A/R Module                           */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/ref/CustList.i}
{ar/ar-agng2.i}
{sys/inc/ttRptSel.i}
{sys/form/r-top3w.f}

DEF  SHARED VAR ldummy AS LOG NO-UNDO.
DEF  SHARED VAR cTextListToSelect AS cha NO-UNDO.
DEF  SHARED VAR cFieldListToSelect AS cha NO-UNDO.
DEF  SHARED VAR cFieldLength AS cha NO-UNDO.
DEF  SHARED VAR cFieldType AS cha NO-UNDO.
DEF  SHARED VAR iColumnLength AS INT NO-UNDO.
DEF  SHARED VAR cTextListToDefault AS cha NO-UNDO.
DEF  SHARED VAR cColumnInit AS LOG INIT YES NO-UNDO.
DEF  SHARED VAR cSelectedList AS cha NO-UNDO.
DEF  SHARED VAR str-line AS cha FORM "x(300)" NO-UNDO.

DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.

def var v-cr-db-amt as dec format "->>>,>>>,>>9.99" NO-UNDO.
def var v-disc-amt  as dec format "->>>,>>>,>>9.99" NO-UNDO.
def var v-type     as char format "x(2)" NO-UNDO.
def var v-first-cust as LOGICAL NO-UNDO.
def var d          as int label "Days" NO-UNDO.
def var ni         as INT NO-UNDO.
def var cust-t     as dec extent 6 format "->,>>>,>>>,>>9.99" NO-UNDO.
DEF VAR cust-t-pri as dec extent 6 format "->,>>>,>>>,>>9.99" NO-UNDO.
DEF VAR cust-t-fc  as dec extent 6 format "->,>>>,>>>,>>9.99" NO-UNDO.
def var sman-t     as dec extent 6 format "->,>>>,>>>,>>9.99" NO-UNDO.
def var sman-t-pri as dec extent 6 format "->,>>>,>>>,>>9.99" NO-UNDO.
def var sman-t-fc  as dec extent 6 format "->,>>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-current-trend-days AS INT NO-UNDO FORMAT "->>9".
def var curr-t     as dec extent 6 format "->,>>>,>>>,>>9.99" NO-UNDO.
def var curr-t-pri as dec extent 6 format "->,>>>,>>>,>>9.99" NO-UNDO.
def var curr-t-fc  as dec extent 6 format "->,>>>,>>>,>>9.99" NO-UNDO.
def var onacc      as DEC NO-UNDO.
def var s          as INT NO-UNDO.
DEF VAR cPO        AS CHAR FORMAT "x(20)" NO-UNDO.
def var ag         as dec format "->>>,>>>,>>9.99" NO-UNDO.
def var amt        like ag NO-UNDO.
def var paid-amt   like ag NO-UNDO.
def var c1         as dec format "->,>>>,>>>,>>9.99" NO-UNDO.
DEF VAR c1-pri     as dec format "->,>>>,>>>,>>9.99" NO-UNDO.
DEF VAR c1-fc      as dec format "->,>>>,>>>,>>9.99" NO-UNDO. 
def var m1         as char format "x(20)" NO-UNDO.
def var m2         as char format "x(20)" NO-UNDO.
def var m3         as char format "x(20)" NO-UNDO.
def var t1         as dec format "->,>>>,>>>,>>9.99" NO-UNDO.
DEF VAR t1-pri     as dec format "->,>>>,>>>,>>9.99" NO-UNDO.
DEF VAR t1-fc      as dec format "->,>>>,>>>,>>9.99" NO-UNDO.
def var save_id    as RECID NO-UNDO.
def var unapp like cust-t NO-UNDO.
def var first-unapp as log init YES NO-UNDO.
def var tmp-var as char format "x(20)" NO-UNDO.
def var v-disc-type as char format "x(4)" NO-UNDO.
def var v-sman as char format "x(24)" NO-UNDO.
def var v-int as int no-undo.
def var v-dec as dec extent 4 no-undo.
DEF VAR ll-valid-cust AS LOG NO-UNDO.
DEF VAR ll-mult-curr AS LOG NO-UNDO.
DEF VAR lv-page-break AS CHAR NO-UNDO.
DEF VAR lv-f-bot-hdr AS CHAR FORMAT "x(12)" NO-UNDO.
DEF VAR v-neg-text AS CHAR NO-UNDO.
DEF VAR v-tr-dscr AS CHAR NO-UNDO.
DEF VAR v-check-date AS DATE NO-UNDO.
DEF VAR v-gltrans-desc AS CHAR FORMAT "X(60)" NO-UNDO.
DEF VAR cPoNo LIKE ar-inv.po-no NO-UNDO.
DEF VAR cJobStr AS CHAR FORMAT "x(9)" NO-UNDO.
DEF TEMP-TABLE tt-cust NO-UNDO FIELD curr-code LIKE cust.curr-code
                               FIELD sorter    LIKE cust.cust-no
                               FIELD row-id    AS   ROWID
                               INDEX tt-cust curr-code sorter.

DEF TEMP-TABLE tt-inv NO-UNDO  FIELD sorter    LIKE ar-inv.inv-no
                               FIELD inv-no    LIKE ar-inv.inv-no
                               FIELD row-id    AS   ROWID
                               INDEX tt-inv sorter inv-no.

&SCOPED-DEFINE for-each-arinv                      ~
    FOR EACH ar-inv                                ~
        FIELDS(company posted cust-no inv-date     ~
               terms x-no due-date net gross       ~
               freight tax-amt inv-no)             ~
        NO-LOCK                                    ~
        WHERE ar-inv.company     EQ cust.company   ~
          AND ar-inv.posted      EQ YES            ~
          AND ar-inv.cust-no     EQ cust.cust-no   ~
          AND ar-inv.inv-date    LE v-date         ~
          AND ar-inv.terms       NE "CASH"         

&SCOPED-DEFINE for-each-arcsh                            ~
    FOR EACH ar-cash                                     ~
        FIELDS(company cust-no posted check-date c-no check-no)   ~
        NO-LOCK                                          ~
        WHERE ar-cash.company     EQ cust.company        ~
          AND ar-cash.cust-no     EQ cust.cust-no        ~
          AND (ar-cash.check-date LE v-date OR           ~
               ar-cash.check-date EQ ?)                  ~
          AND ar-cash.posted      EQ YES                 ~
        USE-INDEX ar-cash,                               ~
                                                         ~
        EACH ar-cashl                                    ~
        FIELDS(check-no c-no posted inv-no company       ~
               cust-no memo amt-disc amt-paid on-account rec_key) ~
        NO-LOCK                                          ~
        WHERE ar-cashl.c-no       EQ ar-cash.c-no        ~
          AND ar-cashl.posted     EQ YES                 ~
        USE-INDEX c-no:                                  ~
                                                         ~
      IF ar-cashl.inv-no NE 0 THEN DO:                   ~
        FIND FIRST ar-inv NO-LOCK                        ~
            WHERE ar-inv.company     EQ cust.company     ~
              AND ar-inv.inv-no      EQ ar-cashl.inv-no  ~
              AND ar-inv.inv-date    GT v-date           ~
            USE-INDEX inv-no NO-ERROR.                   ~
        IF NOT AVAIL ar-inv THEN NEXT.                   ~
      END.                                               ~
      IF ar-cashl.amt-paid GT 0 THEN DO:                 ~
      FIND FIRST reftable WHERE                          ~
           reftable.reftable EQ "ARCASHLVDDATE" AND      ~
           reftable.rec_key EQ ar-cashl.rec_key          ~
           USE-INDEX rec_key                             ~
           NO-LOCK NO-ERROR.                             ~
      IF AVAIL reftable THEN                             ~
         v-check-date = DATE(reftable.CODE).             ~
      ELSE                                               ~
      DO:                                                ~
         v-gltrans-desc = "VOID " + cust.cust-no + " " + ~
                          STRING(ar-cash.check-no,"9999999999") + ~
                         " Inv# " + STRING(ar-cashl.inv-no). ~
         FIND FIRST gltrans WHERE ~
              gltrans.company EQ cust.company AND ~
              gltrans.jrnl EQ "CASHRVD" AND ~
              gltrans.tr-dscr EQ v-gltrans-desc ~
              NO-LOCK NO-ERROR. ~
         IF AVAIL gltrans THEN ~
            v-check-date = gltrans.tr-date. ~
         ELSE ~
            v-check-date = ar-cash.check-date. ~
      END. ~
      END. ~
      ELSE v-check-date = ar-cash.check-date. ~
      IF v-check-date NE ? AND v-check-date GT v-date THEN ~
         NEXT.

&SCOPED-DEFINE valid-factored                                       ~
    IF NOT v-include-factored AND                                   ~
       CAN-FIND(FIRST tt-factored                                   ~
                  WHERE tt-factored.x-no EQ ar-inv.x-no) THEN       ~
        NEXT.   

FORM HEADER SKIP(1)
     lv-page-break FORMAT "x(200)"
WITH PAGE-TOP FRAME r-top-1 STREAM-IO WIDTH 200 NO-BOX.

{custom/formtext.i NEW}
DEF VAR lv-text AS cha NO-UNDO.
DEF VAR v-Inv-note AS cha FORM "x(80)" EXTENT 8 NO-UNDO.
DEF VAR v-Collection-note AS cha FORM "x(80)" EXTENT 8 NO-UNDO.

FORMAT HEADER
  SKIP(1)
  "Customer/Contact/SalesRep/Terms/Recent Payment Trend" SKIP
  v-chk-day " Type   Inv.#   Inv Date" SPACE(10)
  "Amount                Current           "
  v-days[1] SPACE(13) v-days[2] SPACE(13) v-days[3] SPACE(0) "+" SKIP  
  FILL("_",132) FORMAT "x(131)"
WITH PAGE-TOP FRAME r-top-2 STREAM-IO WIDTH 200 NO-BOX.

/* Store list of all factored items */
DEF TEMP-TABLE tt-factored
  FIELD company LIKE reftable.company
  FIELD i-no    LIKE itemfg.i-no
  FIELD x-no    LIKE ar-invl.x-no
  INDEX i1 i-no
  INDEX i2 x-no.
  
FOR EACH reftable WHERE reftable.reftable EQ "FACTORED"        
                    AND reftable.code2    EQ "YES"
                  NO-LOCK:
  /* Note: code2 index exists on reftable */
  FIND FIRST tt-factored WHERE tt-factored.i-no EQ reftable.CODE
    NO-LOCK NO-ERROR.
  IF NOT AVAIL tt-factored THEN DO:
    FOR EACH ar-invl WHERE ar-invl.company EQ cocode
      AND ar-invl.i-no EQ reftable.CODE
      NO-LOCK:
      CREATE tt-factored.
      ASSIGN tt-factored.company = reftable.company
             tt-factored.i-no    = reftable.CODE.
    END.
  END.
END.

/* Start processing */
 FOR EACH company WHERE
       company.company GE b-comp AND
       company.company LE e-comp
       NO-LOCK,    
    EACH cust 
      FIELDS(company cust-no sman curr-code name area-code
             phone terms fax cr-lim contact addr city state zip)
      NO-LOCK
      WHERE cust.company EQ company.company
        AND cust.cust-no GE v-s-cust
        AND cust.cust-no LE v-e-cust
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq cust.cust-no
        AND ttCustList.log-fld no-lock) else true)
        AND cust.sman    GE v-s-sman
        AND cust.sman    LE v-e-sman
        AND cust.terms    GE v-s-terms
        AND cust.terms    LE v-e-terms
        AND (cust.ACTIVE NE "I" OR v-inactive-custs)
        AND ((cust.curr-code GE v-s-curr    AND
              cust.curr-code LE v-e-curr)       OR
             (cust.curr-code EQ ""          AND
              company.curr-code GE v-s-curr AND
              company.curr-code LE v-e-curr)):
     
    STATUS DEFAULT "Checking Customer: " + TRIM(cust.cust-no).
    PROCESS EVENTS.
    
    ll-valid-cust = NO.

    IF NOT ll-valid-cust THEN
    {&for-each-arinv}:
      {&valid-factored}

      ll-valid-cust = YES.

      LEAVE.
    END.

    IF NOT ll-valid-cust THEN
    {&for-each-arcsh}

      ll-valid-cust = YES.

      LEAVE.
    END.

    IF ll-valid-cust THEN DO:
      CREATE tt-cust.
      ASSIGN
       tt-cust.curr-code = IF cust.curr-code EQ "" THEN company.curr-code
                                                   ELSE cust.curr-code
       tt-cust.sorter    = {&sort-by}
       tt-cust.row-id    = ROWID(cust).

      IF tt-cust.curr-code NE company.curr-code THEN ll-mult-curr = YES.
    END.
  END.

  FOR EACH tt-cust,
      FIRST cust 
      FIELDS(company cust-no sman curr-code NAME area-code
             phone terms fax cr-lim contact addr city state zip)
      NO-LOCK
      WHERE ROWID(cust) EQ tt-cust.row-id
      BREAK BY tt-cust.curr-code
            BY tt-cust.sorter:

    STATUS DEFAULT "Printing Currency/" + TRIM(v-sort) + ": " +
                   TRIM(tt-cust.curr-code) + "/" + TRIM(tt-cust.sorter).
    PROCESS EVENTS.
    
    IF FIRST-OF(tt-cust.curr-code) THEN DO:
      lv-page-break = "Currency: " + TRIM(tt-cust.curr-code).

      IF FIRST(tt-cust.curr-code) THEN DO:
        IF ll-mult-curr THEN VIEW FRAME r-top-1.
        /*VIEW FRAME r-top-2.*/
      END.

      IF ll-mult-curr OR FIRST(tt-cust.curr-code) THEN PAGE.
    END.

    IF FIRST-OF(tt-cust.sorter)     AND
       NOT FIRST(tt-cust.curr-code) AND
       "{&sort-by}" EQ "cust.sman"  THEN PAGE.

    FIND FIRST sman NO-LOCK
        WHERE sman.company eq cust.company
          AND sman.sman    eq cust.sman
        NO-ERROR.
    v-sman = cust.sman + "-" + (IF AVAIL sman THEN sman.sname
                                ELSE "Slsmn not on file").

    v-first-cust = yes.

    EMPTY TEMP-TABLE tt-inv.

    IF v-inc OR v-date NE TODAY THEN
    {&for-each-arinv}:
      {&valid-factored}

      CREATE tt-inv.
      ASSIGN
       tt-inv.sorter = INT(ar-inv.{&sort-by2})
       tt-inv.inv-no = ar-inv.inv-no
       tt-inv.row-id = ROWID(ar-inv).
    END.

    ELSE DO:
      {&for-each-arinv}
            AND ar-inv.due LT 0
          USE-INDEX posted-due:
        {&valid-factored}

        CREATE tt-inv.
        ASSIGN
         tt-inv.sorter = INT(ar-inv.{&sort-by2})
         tt-inv.inv-no = ar-inv.inv-no
         tt-inv.row-id = ROWID(ar-inv).
      END.

      {&for-each-arinv}
            AND ar-inv.due GT 0
          USE-INDEX posted-due:
        {&valid-factored}

        CREATE tt-inv.
        ASSIGN
         tt-inv.sorter = INT(ar-inv.{&sort-by2})
         tt-inv.inv-no = ar-inv.inv-no
         tt-inv.row-id = ROWID(ar-inv).
      END.
    END.

    FOR EACH tt-inv,
        FIRST ar-inv WHERE ROWID(ar-inv) EQ tt-inv.row-id NO-LOCK
        BY tt-inv.sorter
        BY tt-inv.inv-no:

/* Inserted because AR stores gross wrong */
      if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt then
        amt = ar-inv.net.
      else
        amt = ar-inv.gross.

      IF amt EQ ? THEN amt = 0.

      /* if fuel surcharge should not be aged, get it out of 'amt' */
      IF NOT v-include-fuel THEN FOR EACH ar-invl NO-LOCK 
         WHERE ar-invl.x-no EQ ar-inv.x-no
           AND can-find(first itemfg where itemfg.company eq ar-invl.company
                                       and itemfg.i-no    eq ar-invl.i-no
                                       and itemfg.procat  eq "FS"):
        ASSIGN amt = amt - ar-invl.amt.
      END.

      cPoNo = "". cJobStr = "".
      FOR EACH ar-invl NO-LOCK 
         WHERE ar-invl.x-no EQ ar-inv.x-no:
          IF ar-invl.po-no GT "" THEN
             ASSIGN cPoNo   = ar-invl.po-no.
          IF ar-invl.job-no GT "" THEN
              cJobStr = ar-invl.job-no + "-" + STRING(ar-invl.job-no2, "99").
      END.


      assign
       ag     = amt
       d      = v-date - ar-inv.{&date}
       ni     = ni + 1
       v-type = IF ar-inv.terms EQ "FCHG" THEN "FC" ELSE "IN".

      for each ar-cashl
          where ar-cashl.company  eq ar-inv.company
            and ar-cashl.posted   eq yes
            and ar-cashl.cust-no  eq ar-inv.cust-no
            and ar-cashl.inv-no   eq ar-inv.inv-no
           USE-INDEX inv-no no-lock,
          FIRST ar-cash
          where ar-cash.c-no       eq ar-cashl.c-no
            and ar-cash.check-date le v-date
          use-index c-no no-lock
          BY ar-cashl.rec_key:

        IF ar-cashl.amt-paid GT 0 THEN
        DO:
           FIND FIRST reftable WHERE                        
                reftable.reftable EQ "ARCASHLVDDATE" AND      
                reftable.rec_key EQ ar-cashl.rec_key          
                USE-INDEX rec_key
                NO-LOCK NO-ERROR.                             
           IF AVAIL reftable THEN                             
              v-check-date = DATE(reftable.CODE).             
           ELSE                                               
           DO:                                                
              v-gltrans-desc = "VOID " + cust.cust-no + " " + 
                               STRING(ar-cash.check-no,"9999999999") +
                              " Inv# " + STRING(ar-cashl.inv-no).
              FIND FIRST gltrans WHERE 
                   gltrans.company EQ cust.company AND
                   gltrans.jrnl EQ "CASHRVD" AND
                   gltrans.tr-dscr EQ v-gltrans-desc
                   NO-LOCK NO-ERROR.
              IF AVAIL gltrans THEN
                 v-check-date = gltrans.tr-date.
              ELSE
                 v-check-date = ar-cash.check-date.
           END.
        END.
        ELSE
           v-check-date = ar-cash.check-date.

        IF v-check-date NE ? AND v-check-date GT v-date THEN NEXT.

        if ar-cashl.memo then

          if ar-cashl.amt-disc ne 0 and ar-cashl.amt-paid eq 0 then
            ag = ag - ar-cashl.amt-disc.
          else 
          if ar-cashl.amt-paid + ar-cashl.amt-disc gt 0 then
            ag = ag + (ar-cashl.amt-paid + ar-cashl.amt-disc).
          else
            ag = ag + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc))).
        else
          ag = ag + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
      end.

     if ag ne 0                                                     or
        (v-inc and
         ar-inv.inv-date ge v-s-dat and ar-inv.inv-date le v-e-dat) then do:
       if v-first-cust then do:
         assign paid-amt = 0  m3 = ""  ni = 0.
         if cust.area-code ne "" then
            m3 = string(cust.area-code,"(999) ").

         m3 = m3 + string(cust.phone,"999-9999").

         find first terms where terms.company = cust.company and
                                terms.t-code = cust.terms no-lock no-error.

         /* If input trend days entered, then do the trend days calculation. */
         IF  v-trend-days > 0 THEN
             RUN get-trend-days (INPUT v-trend-days,
                                 OUTPUT v-current-trend-days).

         if det-rpt = 1 THEN DO:
         
           /*display cust.cust-no
                   space(2)
                   cust.name
                   space(2)
                   cust.area-code                           format "(xxx)"
                   cust.phone                               format "xxx-xxxx"
                   "  Fax:"
                   substr(cust.fax,1,3)                     format "(xxx)"
                   substr(cust.fax,4,7)                     format "xxx-xxxx"
                   "  Credit Limit:"
                   string(cust.cr-lim,">,>>>,>>>,>>9.99")   format "x(17)"
                   skip
                   cust.contact
                   v-sman
                   space(3)
                   terms.dscr when avail terms
                   "ADTP:" cust.avg-pay
                   "TD:" v-current-trend-days WHEN v-trend-days > 0
               with no-labels no-box frame a1 stream-io width 200.

             IF sPrtCollectionNote THEN RUN Display-CollectionNote.*/

         END. /* if det-rpt = 1 */
        
         v-first-cust = no.
       end.

       if d ge v-days[3] then do:
         /*if det-rpt = 1 then
           display d at 4 format "-9999" when v-days-old 
                   space(7) v-type space(5) ar-inv.inv-no
                   space(2) ar-inv.inv-date format "99/99/99"
                   amt  to 54 ag to 131
                   with frame a no-labels no-box stream-io width 200.*/

         v-int = 4.
       end.
       
       else
       if d ge v-days[2] then do:
         /*if det-rpt = 1 then
           display d at 4 format "-9999" when v-days-old 
                   space(7) v-type space(5) ar-inv.inv-no
                   space(2) ar-inv.inv-date format "99/99/99"
                   amt to 54  ag to 112
                   with frame b no-labels no-box stream-io width 200.*/

         v-int = 3.
       end.
       
       else
       if d ge v-days[1] then do:
         /*if det-rpt = 1 then
           display d at 4 format "-9999" when v-days-old 
                   space(7) v-type space(5) ar-inv.inv-no
                   space(2) ar-inv.inv-date format "99/99/99"
                   amt to 54 ag to 94
                   with frame c no-labels no-box stream-io width 200.*/

         v-int = 2.
       end.
       
       else do:
        /* if det-rpt = 1 then
           display d at 4 format "-9999" when v-days-old 
                   space(7) v-type space(5) ar-inv.inv-no
                   space(2) ar-inv.inv-date format "99/99/99"
                   amt to 54 ag to 77
                   with frame d no-labels no-box stream-io width 200.*/

         v-int = 1.
       END.

       /* Task 06201206 */
      /* IF (v-print-cust-po OR v-print-job) AND 
            (cPoNo NE "" OR cJobStr NE "") THEN DO:
           IF v-print-cust-po OR v-print-job THEN
               PUT "" AT 23. 
          IF v-print-cust-po AND cPoNo  GT "" THEN do:
               PUT "Customer PO# " cPoNO FORMAT "x(20)" .
               if v-export THEN DO:
                   IF det-rpt = 2 THEN DO:
                       EXPORT STREAM s-temp DELIMITER ","
                           ""
                           trim(string("Customer PO#"))                 /*Task# 02071402*/
                           TRIM(STRING(cPoNo)) .
                   END.
               END.
           END.
           IF v-print-job AND cJobStr GT "" AND trim(cJobStr) NE "-00" THEN 
               PUT " Job# " FORMAT "x(6)" cJobStr   .
           IF v-print-cust-po OR v-print-job THEN 
               PUT SKIP.
       END.*/

       ASSIGN
        cust-t[v-int] = cust-t[v-int] + ag
        v-dec         = 0
        v-dec[v-int]  = ag.

       IF v-sep-fc THEN
       DO:
          IF v-type NE "FC" THEN
             cust-t-pri[v-int] = cust-t-pri[v-int] + ag.
          ELSE
             cust-t-fc[v-int] = cust-t-fc[v-int] + ag.
       END.
       
       /*if v-export then
         run export-data ("", d, v-type, string(ar-inv.inv-no,">>>>>>>>>>"),
                          ar-inv.inv-date, amt,
                          v-dec[1], v-dec[2], v-dec[3], v-dec[4]).*/
      if det-rpt = 1 THEN DO:
         ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".
              
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                     WHEN "cust"      THEN cVarValue = string(cust.cust-no,"x(8)")  .
                     WHEN "cust-name" THEN cVarValue = string(cust.NAME,"x(30)")  .
                     WHEN "cont"      THEN cVarValue = string(cust.contact,"x(15)") .
                     WHEN "sman"      THEN cVarValue = STRING(v-sman,"x(25)") .
                     WHEN "term"      THEN cVarValue = if avail terms then string(terms.dscr,"x(15)") else ""  .
                     WHEN "add1"      THEN cVarValue = STRING(cust.addr[1],"x(25)").
                     WHEN "add2"      THEN cVarValue = STRING(cust.addr[2],"x(25)").
                     WHEN "city"      THEN cVarValue = STRING(cust.city,"x(10)") .
                     WHEN "stat"      THEN cVarValue = STRING(cust.state,"x(5)") .
                     WHEN "zip"       THEN cVarValue = STRING(cust.zip,"x(10)")  .
                     WHEN "cre-lim"   THEN cVarValue = string(cust.cr-lim,">>>>>>>>9.99") .
                     WHEN "phone"     THEN cVarValue = trim(string(cust.area-code,"(xxx)") + string(cust.phone,"xxx-xxxx")) .
                     WHEN "fax"       THEN cVarValue = trim(string(substr(cust.fax,1,3),"(xxx)") + string(substr(cust.fax,4,7),"xxx-xxxx")).
                     WHEN "chk-memo"  THEN cVarValue = STRING("0").
                     WHEN "day-old"   THEN cVarValue = STRING(d,"->>>>>>>"). /*8*/
                     WHEN "type"      THEN cVarValue = string(v-type,"x(4)").
                     WHEN "inv"       THEN cVarValue = string(ar-inv.inv-no,">>>>>>>>") .
                     WHEN "inv-date"  THEN cVarValue = STRING(ar-inv.inv-date,"99/99/99") .
                     WHEN "amount"    THEN cVarValue = STRING(amt,"->>>>>>>>9.99").
                     WHEN "current"   THEN cVarValue = STRING(v-dec[1],"->>>>>>>>9.99").
                     WHEN "adtp"      THEN cVarValue = STRING(cust.avg-pay,">>>9").
                     WHEN "td"        THEN cVarValue = STRING(v-current-trend-days,"->>9").
                     WHEN "per-1"     THEN cVarValue = STRING(v-dec[2],"->>>>>>>>9.99") .
                     WHEN "per-2"     THEN cVarValue = STRING(v-dec[3],"->>>>>>>>9.99").
                     WHEN "per-3"     THEN cVarValue = STRING(v-dec[4],"->>>>>>>>9.99") .
                     WHEN "cust-po"   THEN cVarValue = STRING(cPoNo,"x(15)") .
                     WHEN "job"       THEN cVarValue = STRING(cJobStr,"x(9)")  .
                     WHEN "inv-note"  THEN cVarValue = "".
                     WHEN "coll-note" THEN cVarValue = "".
                    
                END CASE.
                  
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                           FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
        
        PUT UNFORMATTED cDisplay SKIP.

        IF sPrtInvNote THEN RUN Display-InvNote.
        IF sPrtCollectionNote THEN RUN Display-CollectionNote.

        IF v-export THEN DO:
             PUT STREAM s-temp UNFORMATTED  
                   cExcelDisplay SKIP.
        END.
      END.  /* if det-rpt = 1 THEN */

      for each ar-cashl
          where ar-cashl.company  eq ar-inv.company
            and ar-cashl.posted   eq yes
            and ar-cashl.cust-no  eq ar-inv.cust-no
            and ar-cashl.inv-no   eq ar-inv.inv-no
          use-index inv-no no-lock,

          FIRST ar-cash
          where ar-cash.c-no       eq ar-cashl.c-no
            and ar-cash.check-date le v-date
          use-index c-no no-lock:

        IF ar-cashl.amt-paid GT 0 THEN
        DO:
           FIND FIRST reftable WHERE                        
                reftable.reftable EQ "ARCASHLVDDATE" AND      
                reftable.rec_key EQ ar-cashl.rec_key
                USE-INDEX rec_key
                NO-LOCK NO-ERROR.                             
           IF AVAIL reftable THEN                             
              v-check-date = DATE(reftable.CODE).             
           ELSE                                               
           DO:                                                
              v-gltrans-desc = "VOID " + cust.cust-no + " " + 
                               STRING(ar-cash.check-no,"9999999999") +
                              " Inv# " + STRING(ar-cashl.inv-no).
              FIND FIRST gltrans WHERE 
                   gltrans.company EQ cust.company AND
                   gltrans.jrnl EQ "CASHRVD" AND
                   gltrans.tr-dscr EQ v-gltrans-desc
                   NO-LOCK NO-ERROR.
              IF AVAIL gltrans THEN
                 v-check-date = gltrans.tr-date.
              ELSE
                 v-check-date = ar-cash.check-date.
           END.
        END.
        ELSE
           v-check-date = ar-cash.check-date.

        IF v-check-date NE ? AND v-check-date GT v-date THEN NEXT.

        if ar-cashl.memo then

           /* CTS CM/DM signs are reversed *****************************/
           /*if (ar-cashl.amt-paid + ar-cashl.amt-disc) lt 0 then
              assign v-type = "CM"
                     v-cr-db-amt = ar-cashl.amt-paid
                     v-disc-amt = ar-cashl.amt-disc.

           else*/
           if (ar-cashl.amt-paid + ar-cashl.amt-disc) gt 0 then
              assign v-type = "DM"
                     v-cr-db-amt = ar-cashl.amt-paid
                     v-disc-amt = ar-cashl.amt-disc.

           else
              assign v-type = "CM"
                     v-cr-db-amt = ar-cashl.amt-paid
                     v-disc-amt = - (ar-cashl.amt-disc).

        else
        DO:
           v-tr-dscr = "VOID " + cust.cust-no + " "
                     + STRING(ar-cash.check-no,"9999999999")
                     + " Inv# " + STRING(ar-cashl.inv-no).

           IF ar-cashl.amt-paid GT 0 AND
              (CAN-FIND(FIRST reftable WHERE
              reftable.reftable = "ARCASHLVDDATE" AND
              reftable.rec_key = ar-cashl.rec_key
              USE-INDEX rec_key) OR
              CAN-FIND(FIRST gltrans WHERE
              gltrans.company EQ cust.company AND
              gltrans.jrnl EQ "CASHRVD" AND
              gltrans.tr-dscr EQ v-tr-dscr)) THEN
              v-type = "VD".
           ELSE
              v-type = "PY".

           ASSIGN
              v-cr-db-amt = ar-cashl.amt-paid * -1
              v-disc-amt = ar-cashl.amt-disc * -1.

           /*IF v-type = "PY" AND v-cr-db-amt GT 0 THEN
              v-cr-db-amt = v-cr-db-amt * -1.
           ELSE*/
            /*IF v-type EQ "VD" AND v-cr-db-amt LT 0 THEN
              v-cr-db-amt = v-cr-db-amt * -1.*/
        END.

        if v-disc-amt ne 0 then do:

          v-disc-type = "DISC".

          if ar-cashl.memo then
            assign
             v-disc-type = "RETN"
             v-disc-amt  = - v-disc-amt.

          if det-rpt = 1 then do:
            if v-disc-type eq "DISC" then do:
             /* display ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                      v-type at 16
                      ar-cashl.inv-no at 23
                      ar-cash.check-date at 31 format "99/99/99"
                      v-cr-db-amt to 54 skip
                  with frame f-1 no-box no-labels stream-io width 200.
                  
              if v-export then
                run export-data (ar-cashl.check-no, 0, v-type,
                                 string(ar-cashl.inv-no,">>>>>>>>>>"),
                                 ar-cash.check-date, v-cr-db-amt, 0, 0, 0, 0).*/
                 ASSIGN cDisplay = ""
                     cTmpField = ""
                     cVarValue = ""
                     cExcelDisplay = ""
                     cExcelVarValue = "".

                 DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                     cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                     CASE cTmpField:             
                         WHEN "cust"      THEN cVarValue = string(cust.cust-no,"x(8)")  .
                         WHEN "cust-name" THEN cVarValue = string(cust.NAME,"x(30)")  .
                         WHEN "cont"      THEN cVarValue = string(cust.contact,"x(15)") .
                         WHEN "sman"      THEN cVarValue = STRING(v-sman,"x(25)") .
                         WHEN "term"      THEN cVarValue = if avail terms then string(terms.dscr,"x(15)") else ""  .
                         WHEN "add1"      THEN cVarValue = STRING(cust.addr[1],"x(25)").
                         WHEN "add2"      THEN cVarValue = STRING(cust.addr[2],"x(25)").
                         WHEN "city"      THEN cVarValue = STRING(cust.city,"x(10)") .
                         WHEN "stat"      THEN cVarValue = STRING(cust.state,"x(5)") .
                         WHEN "zip"       THEN cVarValue = STRING(cust.zip,"x(10)")  .
                         WHEN "cre-lim"   THEN cVarValue = string(cust.cr-lim,">>>>>>>>9.99") .
                         WHEN "phone"     THEN cVarValue = trim(string(cust.area-code,"(xxx)") +  string(cust.phone,"xxx-xxxx")) .
                         WHEN "fax"       THEN cVarValue = trim(string(substr(cust.fax,1,3),"(xxx)") + string(substr(cust.fax,4,7),"xxx-xxxx")).
                         WHEN "chk-memo"  THEN cVarValue = STRING(ar-cashl.check-no).
                         WHEN "day-old"   THEN cVarValue = STRING("").
                         WHEN "type"      THEN cVarValue = string(v-type,"x(4)").
                         WHEN "inv"       THEN cVarValue = string(ar-cashl.inv-no,">>>>>>>>") .
                         WHEN "inv-date"  THEN cVarValue = STRING(ar-cash.check-date,"99/99/99") .
                         WHEN "amount"    THEN cVarValue = STRING(v-cr-db-amt,"->>>>>>>>9.99").
                         WHEN "current"   THEN cVarValue = /*STRING(v-dec[1],"->>>>>>>>9.99")*/ "".
                         WHEN "adtp"      THEN cVarValue = STRING(cust.avg-pay,">>>9").
                         WHEN "td"        THEN cVarValue = STRING(v-current-trend-days,"->>9").
                         WHEN "per-1"     THEN cVarValue = /*STRING(v-dec[2],"->>>>>>>>9.99")"*/ "" .
                         WHEN "per-2"     THEN cVarValue = /*STRING(v-dec[3],"->>>>>>>>9.99")*/ "" .
                         WHEN "per-3"     THEN cVarValue = /*STRING(v-dec[4],"->>>>>>>>9.99")*/ "" .
                         WHEN "cust-po"   THEN cVarValue = STRING(cPoNo,"x(15)") .
                         WHEN "job"       THEN cVarValue = STRING(cJobStr,"x(10)")  .
                         WHEN "inv-note"  THEN cVarValue = "".
                         WHEN "coll-note" THEN cVarValue = "".
                     END CASE.

                     cExcelVarValue = cVarValue.
                     cDisplay = cDisplay + cVarValue +
                         FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                     cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF v-export THEN DO:
                PUT STREAM s-temp UNFORMATTED  
                    cExcelDisplay SKIP.
             END.
             IF sPrtInvNote THEN RUN Display-InvNote.
         end.
            /*IF det-rpt <> 3 THEN
            display ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                    v-disc-type at 16
                    ar-cashl.inv-no at 23
                    ar-cash.check-date at 31 format "99/99/99"
                    v-disc-amt to 54
                with frame f-50{&frame} no-box no-labels stream-io width 200.
                
            if v-export then
              run export-data (ar-cashl.check-no, 0, v-disc-type,
                               string(ar-cashl.inv-no,">>>>>>>>>>"),
                               ar-cash.check-date, v-disc-amt, 0, 0, 0, 0).*/
                    ASSIGN cDisplay = ""
                     cTmpField = ""
                     cVarValue = ""
                     cExcelDisplay = ""
                     cExcelVarValue = "".

                 DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                     cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                     CASE cTmpField:             
                         WHEN "cust"      THEN cVarValue = string(cust.cust-no,"x(8)")  .
                         WHEN "cust-name" THEN cVarValue = string(cust.NAME,"x(30)")  .
                         WHEN "cont"      THEN cVarValue = string(cust.contact,"x(15)") .
                         WHEN "sman"      THEN cVarValue = STRING(v-sman,"x(25)") .
                         WHEN "term"      THEN cVarValue = if avail terms then string(terms.dscr,"x(15)") else ""  .
                         WHEN "add1"      THEN cVarValue = STRING(cust.addr[1],"x(25)").
                         WHEN "add2"      THEN cVarValue = STRING(cust.addr[2],"x(25)").
                         WHEN "city"      THEN cVarValue = STRING(cust.city,"x(10)") .
                         WHEN "stat"      THEN cVarValue = STRING(cust.state,"x(5)") .
                         WHEN "zip"       THEN cVarValue = STRING(cust.zip,"x(10)")  .
                         WHEN "cre-lim"   THEN cVarValue = string(cust.cr-lim,">>>>>>>>9.99") .
                         WHEN "phone"     THEN cVarValue = trim(string(cust.area-code,"(xxx)") +  string(cust.phone,"xxx-xxxx")) .
                         WHEN "fax"       THEN cVarValue = trim(string(substr(cust.fax,1,3),"(xxx)") + string(substr(cust.fax,4,7),"xxx-xxxx")).
                         WHEN "chk-memo"  THEN cVarValue = STRING(ar-cashl.check-no).
                         WHEN "day-old"   THEN cVarValue = STRING("").
                         WHEN "type"      THEN cVarValue = string(v-disc-type,"x(4)").
                         WHEN "inv"       THEN cVarValue = string(ar-cashl.inv-no,">>>>>>>>") .
                         WHEN "inv-date"  THEN cVarValue = STRING(ar-cash.check-date,"99/99/99") .
                         WHEN "amount"    THEN cVarValue = STRING(v-disc-amt,"->>>>>>>>9.99").
                         WHEN "current"   THEN cVarValue = /*STRING(v-dec[1],"->>>>>>>>9.99")*/ "".
                         WHEN "adtp"      THEN cVarValue = STRING(cust.avg-pay,">>>9").
                         WHEN "td"        THEN cVarValue = STRING(v-current-trend-days,"->>9").
                         WHEN "per-1"     THEN cVarValue = /*STRING(v-dec[2],"->>>>>>>>9.99")"*/ "" .
                         WHEN "per-2"     THEN cVarValue = /*STRING(v-dec[3],"->>>>>>>>9.99")*/ "" .
                         WHEN "per-3"     THEN cVarValue = /*STRING(v-dec[4],"->>>>>>>>9.99")*/ "" .
                         WHEN "cust-po"   THEN cVarValue = STRING(cPoNo,"x(15)") .
                         WHEN "job"       THEN cVarValue = STRING(cJobStr,"x(10)")  .
                         WHEN "inv-note"  THEN cVarValue = "".
                         WHEN "coll-note" THEN cVarValue = "".
                     END CASE.

                     cExcelVarValue = cVarValue.
                     cDisplay = cDisplay + cVarValue +
                         FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                     cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF v-export THEN DO:
                PUT STREAM s-temp UNFORMATTED  
                    cExcelDisplay SKIP.
             END.
             IF sPrtInvNote THEN RUN Display-InvNote.
                    
          end.
        end.

        else
        if det-rpt = 1 then do:

           IF v-type EQ "VD" THEN
           DO:
              FIND FIRST reftable WHERE
                   reftable.reftable EQ "ARCASHLVDDATE" AND
                   reftable.rec_key EQ ar-cashl.rec_key
                   USE-INDEX rec_key
                   NO-LOCK NO-ERROR.

              IF AVAIL reftable THEN
                 v-check-date = DATE(reftable.CODE).
              ELSE
              DO:
                 v-gltrans-desc = "VOID " + cust.cust-no + " " +
                                  STRING(ar-cash.check-no,"9999999999") +
                                 " Inv# " + STRING(ar-cashl.inv-no).

                 FIND FIRST gltrans WHERE
                      gltrans.company EQ cust.company AND
                      gltrans.jrnl EQ "CASHRVD" AND
                      gltrans.tr-dscr EQ v-gltrans-desc
                      NO-LOCK NO-ERROR.
                
                 IF AVAIL gltrans THEN
                    v-check-date = gltrans.tr-date.
                 ELSE
                    v-check-date = ar-cash.check-date.
              END.
           END.
           ELSE
              v-check-date = ar-cash.check-date.
         /* IF det-rpt = 1 THEN
          display ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                  v-type at 16
                  ar-cashl.inv-no at 23
                  v-check-date @ ar-cash.check-date at 31 format "99/99/99"
                  v-cr-db-amt to 54
              with frame f-100 no-box no-labels stream-io width 200.
              
          if v-export AND det-rpt = 1 then
            run export-data (ar-cashl.check-no, 0, v-type,
                             string(ar-cashl.inv-no,">>>>>>>>>>"),
                             v-check-date, v-cr-db-amt, 0, 0, 0, 0).*/
              IF det-rpt = 1 THEN do:
                   ASSIGN cDisplay = ""
                     cTmpField = ""
                     cVarValue = ""
                     cExcelDisplay = ""
                     cExcelVarValue = "".

                 DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                     cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                     CASE cTmpField:             
                         WHEN "cust"      THEN cVarValue = string(cust.cust-no,"x(8)")  .
                         WHEN "cust-name" THEN cVarValue = string(cust.NAME,"x(30)")  .
                         WHEN "cont"      THEN cVarValue = string(cust.contact,"x(15)") .
                         WHEN "sman"      THEN cVarValue = STRING(v-sman,"x(25)") .
                         WHEN "term"      THEN cVarValue = if avail terms then string(terms.dscr,"x(15)") else ""  .
                         WHEN "add1"      THEN cVarValue = STRING(cust.addr[1],"x(25)").
                         WHEN "add2"      THEN cVarValue = STRING(cust.addr[2],"x(25)").
                         WHEN "city"      THEN cVarValue = STRING(cust.city,"x(10)") .
                         WHEN "stat"      THEN cVarValue = STRING(cust.state,"x(5)") .
                         WHEN "zip"       THEN cVarValue = STRING(cust.zip,"x(10)")  .
                         WHEN "cre-lim"   THEN cVarValue = string(cust.cr-lim,">>>>>>>>9.99") .
                         WHEN "phone"     THEN cVarValue = trim(string(cust.area-code,"(xxx)") +  string(cust.phone,"xxx-xxxx")) .
                         WHEN "fax"       THEN cVarValue = trim(string(substr(cust.fax,1,3),"(xxx)") + string(substr(cust.fax,4,7),"xxx-xxxx")).
                         WHEN "chk-memo"  THEN cVarValue = STRING(ar-cashl.check-no).
                         WHEN "day-old"   THEN cVarValue = STRING("").
                         WHEN "type"      THEN cVarValue = string(v-type,"x(4)").
                         WHEN "inv"       THEN cVarValue = string(ar-cashl.inv-no,">>>>>>>>") .
                         WHEN "inv-date"  THEN cVarValue = STRING(v-check-date,"99/99/99") .
                         WHEN "amount"    THEN cVarValue = STRING(v-cr-db-amt,"->>>>>>>>9.99").
                         WHEN "current"   THEN cVarValue = /*STRING(v-dec[1],"->>>>>>>>9.99")*/ "".
                         WHEN "adtp"      THEN cVarValue = STRING(cust.avg-pay,">>>9").
                         WHEN "td"        THEN cVarValue = STRING(v-current-trend-days,"->>9").
                         WHEN "per-1"     THEN cVarValue = /*STRING(v-dec[2],"->>>>>>>>9.99")"*/ "" .
                         WHEN "per-2"     THEN cVarValue = /*STRING(v-dec[3],"->>>>>>>>9.99")*/ "" .
                         WHEN "per-3"     THEN cVarValue = /*STRING(v-dec[4],"->>>>>>>>9.99")*/ "" .
                         WHEN "cust-po"   THEN cVarValue = STRING(cPoNo,"x(15)") .
                         WHEN "job"       THEN cVarValue = STRING(cJobStr,"x(10)")  .
                         WHEN "inv-note"  THEN cVarValue = "".
                         WHEN "coll-note" THEN cVarValue = "".
                     END CASE.

                     cExcelVarValue = cVarValue.
                     cDisplay = cDisplay + cVarValue +
                         FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                     cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF v-export THEN DO:
                PUT STREAM s-temp UNFORMATTED  
                    cExcelDisplay SKIP.
             END.
             IF sPrtInvNote THEN RUN Display-InvNote.

              END. /* det-prt = 1 */

        end.
      end. /* for each ar-cashl record */
     end.
    /* IF sPrtInvNote THEN RUN Display-InvNote.*/
     end. /* for each ar-inv record */

    assign unapp[1] = 0
           unapp[2] = 0
           unapp[3] = 0
           unapp[4] = 0.

    /* This loop finds all unapplied balances and totals by age */
    {&for-each-arcsh}

      if ar-cashl.memo then do:

           /* CTS CM/DM signs are reversed *****************************/
         if (ar-cashl.amt-paid + ar-cashl.amt-disc) gt 0 then
            assign v-type = "DM"
                   v-cr-db-amt = ar-cashl.amt-paid
                   v-disc-amt = ar-cashl.amt-disc.

         else
            assign v-type = "CM"
                   v-cr-db-amt = ar-cashl.amt-paid
                   v-disc-amt = ar-cashl.amt-disc.
      end.

      else
        assign v-cr-db-amt = ar-cashl.amt-paid * -1
               v-disc-amt = ar-cashl.amt-disc * -1.

      d = v-date - ar-cash.check-date.

      if d ge v-days[3] then
        unapp[4] = unapp[4] + v-cr-db-amt - v-disc-amt.
      else
      if d ge v-days[2] and d lt v-days[3] then
        unapp[3] = unapp[3] + v-cr-db-amt - v-disc-amt.
      else
      if d ge v-days[1] and d lt v-days[2] then
        unapp[2] = unapp[2] + v-cr-db-amt - v-disc-amt.
      else
      if d lt v-days[1] then
        unapp[1] = unapp[1] + v-cr-db-amt - v-disc-amt.

    end. /* for each ar-cashl record */

    first-unapp = yes.
    /* this loop displays all unapplied balances */
    {&for-each-arcsh}

      if v-first-cust then do:
        assign
           paid-amt = 0
           cust-t = 0
           m3 = ""
           ni = 0
           cust-t-pri = 0
           cust-t-fc = 0.

        if cust.area-code ne "" then
           m3 = string(cust.area-code,"(999) ").

        m3 = m3 + string(cust.phone,"999-9999").

        find first terms where terms.company = cust.company and
                               terms.t-code = cust.terms no-lock no-error.

       /* if det-rpt = 1 then
          display cust.cust-no
                  space(2)
                  cust.name
                  space(2)
                  cust.area-code                            format "(xxx)"
                  cust.phone                                format "xxx-xxxx"
                  "  Fax:"
                  substr(cust.fax,1,3)                      format "(xxx)"
                  substr(cust.fax,4,7)                      format "xxx-xxxx"
                  "  Credit Limit:"
                  string(cust.cr-lim,">,>>>,>>>,>>9.99")    format "x(17)"
                  skip
                  cust.contact
                  v-sman
                  space(3)
                  terms.dscr when avail terms
            /* stacey */
/*                   cust.avg-pay */
                  (v-trend-days - cust.avg-pay) WHEN v-trend-days > 0
              with no-labels no-box frame a2 stream-io width 200.
              
        if v-prt-add then run print-cust-add.*/
       
        assign v-first-cust = no.
      end.

      v-neg-text = "ON ACCT".

      if ar-cashl.memo eq true then do:
         if (ar-cashl.amt-paid + ar-cashl.amt-disc) gt 0 then
            assign v-type = "DM"
                   v-cr-db-amt = ar-cashl.amt-paid
                   v-disc-amt = ar-cashl.amt-disc.
         else
            assign v-type = "CM"
                   v-cr-db-amt = ar-cashl.amt-paid
                   v-disc-amt = ar-cashl.amt-disc.
      end.

      else
      DO:
         v-tr-dscr = "VOID " + cust.cust-no + " "
                   + STRING(ar-cash.check-no,"9999999999")
                   + " Inv# " + STRING(ar-cashl.inv-no).

         IF CAN-FIND(FIRST reftable WHERE
            reftable.reftable = "ARCASHLVDDATE" AND
            reftable.rec_key = ar-cashl.rec_key
            USE-INDEX rec_key) OR
            CAN-FIND(FIRST gltrans WHERE
            gltrans.company EQ cust.company AND
            gltrans.jrnl EQ "CASHRVD" AND
            gltrans.tr-dscr EQ v-tr-dscr) THEN
            DO:
              ASSIGN
                 v-type = "VD"
                 v-neg-text = "VOID".
              RELEASE reftable.
            END.
         ELSE
            v-type = "PY".

         ASSIGN
           v-cr-db-amt = ar-cashl.amt-paid * -1
           v-disc-amt = ar-cashl.amt-disc * -1.

         /*IF v-type = "PY" AND v-cr-db-amt GT 0 THEN
            v-cr-db-amt = v-cr-db-amt * -1.
         ELSE*/
          IF v-type EQ "VD" AND v-cr-db-amt LT 0 THEN
            v-cr-db-amt = v-cr-db-amt * -1.
      END.

      if first-unapp then do:
         IF v-type EQ "VD" THEN
         DO:
            FIND FIRST reftable WHERE
                 reftable.reftable EQ "ARCASHLVDDATE" AND
                 reftable.rec_key EQ ar-cashl.rec_key
                 USE-INDEX rec_key
                 NO-LOCK NO-ERROR.
           
            IF AVAIL reftable THEN
               v-check-date = DATE(reftable.CODE).
            ELSE
            DO:
               v-gltrans-desc = "VOID " + cust.cust-no + " " +
                                STRING(ar-cash.check-no,"9999999999") +
                                " Inv# " + STRING(ar-cashl.inv-no).

               FIND FIRST gltrans WHERE
                    gltrans.company EQ cust.company AND
                    gltrans.jrnl EQ "CASHRVD" AND
                    gltrans.tr-dscr EQ v-gltrans-desc
                    NO-LOCK NO-ERROR.
              
               IF AVAIL gltrans THEN
                  v-check-date = gltrans.tr-date.
               ELSE
                  v-check-date = ar-cash.check-date.
            END.
         END.
         ELSE
            v-check-date = ar-cash.check-date.

         /*if det-rpt = 1 then
         
           display skip(1)
                   ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                   v-type at 16
                   v-neg-text at 23
                   v-check-date @ ar-cash.check-date at 31 format "99/99/99"
                   (v-cr-db-amt + v-disc-amt)
                         format "->>>,>>>,>>9.99" to 54
                   unapp[1] when unapp[1] ne 0 to 77
                   unapp[2] when unapp[2] ne 0 to 94
                   unapp[3] when unapp[3] ne 0 to 112
                   unapp[4] when unapp[4] ne 0 to 131
               with frame ab no-labels no-box stream-io width 200.
         
               
         if v-export AND det-rpt = 1 then
           run export-data (ar-cashl.check-no, 0, v-type, v-neg-text,
                            v-check-date, v-cr-db-amt + v-disc-amt,
                            unapp[1], unapp[2], unapp[3], unapp[4]).*/
            if det-rpt = 1 THEN do:
             ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".
               
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                     WHEN "cust"      THEN cVarValue = string(cust.cust-no,"x(8)")  .
                     WHEN "cust-name" THEN cVarValue = string(cust.NAME,"x(30)")  .
                     WHEN "cont"      THEN cVarValue = string(cust.contact,"x(15)") .
                     WHEN "sman"      THEN cVarValue = STRING(v-sman,"x(25)") .
                     WHEN "term"      THEN cVarValue = if avail terms then string(terms.dscr,"x(15)") else ""  .
                     WHEN "add1"      THEN cVarValue = STRING(cust.addr[1],"x(25)").
                     WHEN "add2"      THEN cVarValue = STRING(cust.addr[2],"x(25)").
                     WHEN "city"      THEN cVarValue = STRING(cust.city,"x(10)") .
                     WHEN "stat"      THEN cVarValue = STRING(cust.state,"x(5)") .
                     WHEN "zip"       THEN cVarValue = STRING(cust.zip,"x(10)")  .
                     WHEN "cre-lim"   THEN cVarValue = string(cust.cr-lim,">>>>>>>>9.99") .
                     WHEN "phone"     THEN cVarValue = trim(string(cust.area-code,"(xxx)") +  string(cust.phone,"xxx-xxxx")) .
                     WHEN "fax"       THEN cVarValue = trim(string(substr(cust.fax,1,3),"(xxx)") + string(substr(cust.fax,4,7),"xxx-xxxx")).
                     WHEN "chk-memo"  THEN cVarValue = STRING(ar-cashl.check-no).
                     WHEN "day-old"   THEN cVarValue = STRING("").
                     WHEN "type"      THEN cVarValue = string(v-type,"x(4)").
                     WHEN "inv"       THEN cVarValue = string(v-neg-text) .
                     WHEN "inv-date"  THEN cVarValue = STRING(v-check-date,"99/99/99") .
                     WHEN "amount"    THEN cVarValue = STRING(v-cr-db-amt + v-disc-amt,"->>>>>>>>9.99").
                     WHEN "current"   THEN cVarValue = STRING(unapp[1],"->>>>>>>>9.99").
                     WHEN "adtp"      THEN cVarValue = STRING(cust.avg-pay,">>>9").
                     WHEN "td"        THEN cVarValue = STRING(v-current-trend-days,"->>9").
                     WHEN "per-1"     THEN cVarValue = STRING(unapp[2],"->>>>>>>>9.99") .
                     WHEN "per-2"     THEN cVarValue = STRING(unapp[3],"->>>>>>>>9.99").
                     WHEN "per-3"     THEN cVarValue = STRING(unapp[4],"->>>>>>>>9.99") .
                     WHEN "cust-po"   THEN cVarValue = STRING(cPoNo,"x(15)") .
                     WHEN "job"       THEN cVarValue = STRING(cJobStr,"x(10)")  .
                     WHEN "inv-note"  THEN cVarValue = "".
                     WHEN "coll-note" THEN cVarValue = "".
                    
                END CASE.
                  
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                           FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
        
        PUT UNFORMATTED cDisplay SKIP.
        IF v-export THEN DO:
             PUT STREAM s-temp UNFORMATTED  
                   cExcelDisplay SKIP.
        END.
            END. /*det-prt = 1 */

         assign
          cust-t[4] = cust-t[4] + unapp[4]
          cust-t[3] = cust-t[3] + unapp[3]
          cust-t[2] = cust-t[2] + unapp[2]
          cust-t[1] = cust-t[1] + unapp[1].

         IF v-sep-fc THEN
            ASSIGN
             cust-t-pri[4] = cust-t-pri[4] + unapp[4]
             cust-t-pri[3] = cust-t-pri[3] + unapp[3]
             cust-t-pri[2] = cust-t-pri[2] + unapp[2]
             cust-t-pri[1] = cust-t-pri[1] + unapp[1].
      end.

      if first-unapp then first-unapp = no.

      else do:

        IF v-type EQ "VD" THEN
        DO:
           FIND FIRST reftable WHERE
                reftable.reftable EQ "ARCASHLVDDATE" AND
                reftable.rec_key EQ ar-cashl.rec_key
                USE-INDEX rec_key
                NO-LOCK NO-ERROR.
          
           IF AVAIL reftable THEN
              v-check-date = DATE(reftable.CODE).
           ELSE
           DO:
              v-gltrans-desc = "VOID " + cust.cust-no + " " +
                               STRING(ar-cash.check-no,"9999999999") +
                               " Inv# " + STRING(ar-cashl.inv-no).

              FIND FIRST gltrans WHERE
                   gltrans.company EQ cust.company AND
                   gltrans.jrnl EQ "CASHRVD" AND
                   gltrans.tr-dscr EQ v-gltrans-desc
                   NO-LOCK NO-ERROR.
             
              IF AVAIL gltrans THEN
                 v-check-date = gltrans.tr-date.
              ELSE
                 v-check-date = ar-cash.check-date.
           END.
        END.
        ELSE
           v-check-date = ar-cash.check-date.

       /* if det-rpt = 1 then
          display ar-cashl.check-no at 4 format "x(10)" when not v-days-old 
                  v-type at 16
                  v-neg-text at 23
                  v-check-date @ ar-cash.check-date at 31 format "99/99/99"
                  (v-cr-db-amt + v-disc-amt)
                           format "->>>,>>>,>>9.99" to 54
              with frame f-2 no-box no-labels stream-io width 200.
        
              
        if v-export AND det-rpt = 1 then
          run export-data (ar-cashl.check-no, 0, v-type, v-neg-text,
                           v-check-date, v-cr-db-amt + v-disc-amt,
                           0, 0, 0, 0).*/
       if det-rpt = 1 THEN do:
            ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".
               
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                     WHEN "cust"      THEN cVarValue = string(cust.cust-no,"x(8)")  .
                     WHEN "cust-name" THEN cVarValue = string(cust.NAME,"x(30)")  .
                     WHEN "cont"      THEN cVarValue = string(cust.contact,"x(15)") .
                     WHEN "sman"      THEN cVarValue = STRING(v-sman,"x(25)") .
                     WHEN "term"      THEN cVarValue = if avail terms then string(terms.dscr,"x(15)") else ""  .
                     WHEN "add1"      THEN cVarValue = STRING(cust.addr[1],"x(25)").
                     WHEN "add2"      THEN cVarValue = STRING(cust.addr[2],"x(25)").
                     WHEN "city"      THEN cVarValue = STRING(cust.city,"x(10)") .
                     WHEN "stat"      THEN cVarValue = STRING(cust.state,"x(5)") .
                     WHEN "zip"       THEN cVarValue = STRING(cust.zip,"x(10)")  .
                     WHEN "cre-lim"   THEN cVarValue = string(cust.cr-lim,">>>>>>>>9.99") .
                     WHEN "phone"     THEN cVarValue = trim(string(cust.area-code,"(xxx)") + string(cust.phone,"xxx-xxxx")) .
                     WHEN "fax"       THEN cVarValue = trim(string(substr(cust.fax,1,3),"(xxx)") +  string(substr(cust.fax,4,7),"xxx-xxxx")).
                     WHEN "chk-memo"  THEN cVarValue = STRING(ar-cashl.check-no).
                     WHEN "day-old"   THEN cVarValue = STRING("").
                     WHEN "type"      THEN cVarValue = string(v-type,"x(4)").
                     WHEN "inv"       THEN cVarValue = string(v-neg-text) .
                     WHEN "inv-date"  THEN cVarValue = STRING(v-check-date,"99/99/99") .
                     WHEN "amount"    THEN cVarValue = STRING(v-cr-db-amt + v-disc-amt,"->>>>>>>>9.99").
                     WHEN "current"   THEN cVarValue = /*STRING(unapp[1],"->>>>>>>>9.99")*/ "".
                     WHEN "adtp"      THEN cVarValue = STRING(cust.avg-pay,">>>9").
                     WHEN "td"        THEN cVarValue = STRING(v-current-trend-days,"->>9").
                     WHEN "per-1"     THEN cVarValue = /*STRING(unapp[2],"->>>>>>>>9.99")*/ "" .
                     WHEN "per-2"     THEN cVarValue = /*STRING(unapp[3],"->>>>>>>>9.99")*/ "" .
                     WHEN "per-3"     THEN cVarValue = /*STRING(unapp[4],"->>>>>>>>9.99")*/ "" .
                     WHEN "cust-po"   THEN cVarValue = STRING(cPoNo,"x(15)") .
                     WHEN "job"       THEN cVarValue = STRING(cJobStr,"x(10)")  .
                     WHEN "inv-note"  THEN cVarValue = "".
                     WHEN "coll-note" THEN cVarValue = "".
                    
                END CASE.
                  
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                           FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
        
        PUT UNFORMATTED cDisplay SKIP.
        IF v-export THEN DO:
             PUT STREAM s-temp UNFORMATTED  
                   cExcelDisplay SKIP.
        END.
       END. /* det-prt  */
      end.
    end. /* for each ar-cashl record */

    c1 = cust-t[1] + cust-t[2] + cust-t[3] + cust-t[4].

    if (not v-first-cust) or c1 ne 0 then do:
      if det-rpt = 1 then do:

        /*display skip(1) "***** CUSTOMER TOTALS" at 4 c1 to 54 cust-t[1] to 77
                cust-t[2] to 94 cust-t[3] to 112 cust-t[4] to 131 skip(1)
            with frame a3 no-labels no-box no-attr-space stream-io width 200.*/
           RUN total-head("****** CUSTOMER TOTALS",c1,cust-t[1],cust-t[2],
                           cust-t[3],cust-t[4]).
        
        IF v-sep-fc THEN
        DO:
           ASSIGN
              c1-pri = cust-t-pri[1] + cust-t-pri[2] + cust-t-pri[3] + cust-t-pri[4]
              c1-fc  = cust-t-fc[1] + cust-t-fc[2] + cust-t-fc[3] + cust-t-fc[4].

           /*display skip(1) "***** PRINCIPAL AMOUNT" at 4 c1-pri to 54 cust-t-pri[1] to 77
                cust-t-pri[2] to 94 cust-t-pri[3] to 112 cust-t-pri[4] to 131 skip(1)
            with frame a4 no-labels no-box no-attr-space stream-io width 200.*/

           RUN total-head("***** PRINCIPAL AMOUNT",c1-pri,cust-t-pri[1],cust-t-pri[2],
                           cust-t-pri[3],cust-t-pri[4]).

           /*display skip(1) "***** FINANCE CHARGES" at 4 c1-fc to 54 cust-t-fc[1] to 77
                cust-t-fc[2] to 94 cust-t-fc[3] to 112 cust-t-fc[4] to 131 skip(1)
            with frame a5 no-labels no-box no-attr-space stream-io width 200.*/
           RUN total-head("****** FINANCE CHARGES",c1-fc,cust-t-fc[1],cust-t-fc[2],
                           cust-t-fc[3],cust-t-fc[4]).
        END.

        if not last-of(tt-cust.sorter) or "{&sort-by}" ne "cust.sman" then
          put skip(1).
      end.
      ELSE IF det-rpt = 2 THEN DO:

          RUN total-head("cust.cust-no",c1,cust-t[1],cust-t[2],
                           cust-t[3],cust-t[4]).

        /*display cust.cust-no space(2) cust.name + "  " + m3 format "x(50)" skip
                c1        to 54
                cust-t[1] to 77
                cust-t[2] to 94
                cust-t[3] to 112
                cust-t[4] to 131
                skip(1)
            with frame a3sum no-labels no-box no-attr-space stream-io width 200.*/
         if v-export THEN DO:
            /*IF NOT v-prt-add THEN
               EXPORT STREAM s-temp DELIMITER ","
                  trim(cust.cust-no) 
                  trim(cust.NAME)
                  m3                                         
                  c1                                      
                  cust-t[1]                                            
                  cust-t[2]
                  cust-t[3]
                  cust-t[4]
                  SKIP.
            ELSE 
               EXPORT STREAM s-temp DELIMITER ","
                  trim(cust.cust-no) 
                  trim(cust.NAME)
                  trim(cust.addr[1])                                      
                  trim(cust.addr[2])                                      
                  trim(cust.city)                                         
                  trim(cust.state)                                        
                  trim(cust.zip)                                                         
                  trim(string(cust.area-code,"(xxx)") + " " +
                       string(cust.phone,"xxx-xxxx"))                     
                  trim(string(substr(cust.fax,1,3),"(xxx)") + " " +
                       string(substr(cust.fax,4,7),"xxx-xxxx"))
                  c1                                      
                  cust-t[1]                                            
                  cust-t[2]
                  cust-t[3]
                  cust-t[4]
                  SKIP.*/ 
         END.
      END.
            
      do i = 1 to 4:
         ASSIGN
            sman-t[i] = sman-t[i] + cust-t[i]
            cust-t[i] = 0.

         IF v-sep-fc THEN
            ASSIGN
               sman-t-pri[i] = sman-t-pri[i] + cust-t-pri[i]
               sman-t-fc[i] = sman-t-fc[i] + cust-t-fc[i]
               cust-t-pri[i] = 0
               cust-t-fc[i] = 0.
      end.
    end.
    
    if last-of(tt-cust.sorter) then do:
      c1 = sman-t[1] + sman-t[2] + sman-t[3] + sman-t[4].
          
      if "{&sort-by}" eq "cust.sman" THEN DO:
        IF det-rpt <> 3 THEN
            RUN total-head("****** SALESREP TOTALS",c1,sman-t[1],sman-t[2],
                           sman-t[3],sman-t[4]).
        /*display v-sman                  at 4    format "x(33)"
                "TOTALS: " + v-sman                  @ v-sman
                "***** SALESREP TOTALS" when det-rpt = 1 @ v-sman
                c1                      to 54
                sman-t[1]               to 77
                sman-t[2]               to 94
                sman-t[3]               to 112
                sman-t[4]               to 131
                skip(2)
            with frame slsmn no-labels no-box no-attr-space stream-io width 200.*/
        IF v-export THEN DO:
           IF det-rpt = 2 /* was if det-rpt was if not dep-rpt */THEN DO:
              /*IF NOT v-prt-add THEN
                 EXPORT STREAM s-temp DELIMITER ","
                    " " 
                    "TOTALS: " + v-sman 
                    " "
                    c1                                      
                    sman-t[1]                                           
                    sman-t[2]
                    sman-t[3]
                    sman-t[4]
                    SKIP.
              ELSE
                 EXPORT STREAM s-temp DELIMITER ","
                    " " 
                    "TOTALS: " + v-sman 
                    " "                                      
                    " "                                      
                    " "                                         
                    " "                                        
                    " "                                                         
                    " "                      
                    " "
                    c1                                      
                    sman-t[1]                                           
                    sman-t[2]
                    sman-t[3]
                    sman-t[4]
                    SKIP.  */
           END.
        END.
      END.

      do i = 1 to 4:
        ASSIGN
           curr-t[i] = curr-t[i] + sman-t[i]
           sman-t[i] = 0
           curr-t-pri[i] = curr-t-pri[i] + sman-t-pri[i]
           curr-t-fc[i] = curr-t-fc[i] + sman-t-fc[i]
           sman-t-pri[i] = 0
           sman-t-fc[i] = 0.
      end.
    end.
    
    if last-of(tt-cust.curr-code) then do:
      IF ll-mult-curr THEN DO:
        c1 = curr-t[1] + curr-t[2] + curr-t[3] + curr-t[4].
        IF NOT det-rpt = 3 THEN
             RUN total-head("        CURRENCY TOTAL",c1,curr-t[1],curr-t[2],
                           curr-t[3],curr-t[4]).
        /*display fill("_",132) format "x(131)"
                "CURRENCY TOTAL"        at 12
                c1                      to 54
                curr-t[1]               to 77
                curr-t[2]               to 94
                curr-t[3]               to 112
                curr-t[4]               to 131
            with frame curr1 no-labels no-box no-attr-space stream-io width 200.*/

        /*display SPACE(11) "PERCENTAGE COMPOSITION"
                (IF c1 NE 0 THEN (curr-t[1] / c1) * 100 ELSE 0) to 77
                (IF c1 NE 0 THEN (curr-t[2] / c1) * 100 ELSE 0) to 94
                (IF c1 NE 0 THEN (curr-t[3] / c1) * 100 ELSE 0) to 112
                (IF c1 NE 0 THEN (curr-t[4] / c1) * 100 ELSE 0) to 131
            with frame curr2 STREAM-IO WIDTH 200 no-labels no-box no-attr-space.*/

        RUN total-head("PERCENTAGE COMPOSITION",0,(IF c1 NE 0 THEN (curr-t[1] / c1) * 100 ELSE 0),(IF c1 NE 0 THEN (curr-t[2] / c1) * 100 ELSE 0),
                           (IF c1 NE 0 THEN (curr-t[3] / c1) * 100 ELSE 0),(IF c1 NE 0 THEN (curr-t[4] / c1) * 100 ELSE 0)).

        IF v-export THEN DO:
           IF NOT det-rpt = 1 THEN DO:
               /*IF NOT v-prt-add THEN
                  EXPORT STREAM s-temp DELIMITER ","
                     trim(cust.cust-no) 
                     trim(cust.NAME)
                     m3                                         
                     c1                                      
                     cust-t[1]                                            
                     cust-t[2]
                     cust-t[3]
                     cust-t[4]
                     SKIP.
               ELSE
                  EXPORT STREAM s-temp DELIMITER ","
                     trim(cust.cust-no) 
                     trim(cust.NAME)
                     trim(cust.addr[1])                                      
                     trim(cust.addr[2])                                      
                     trim(cust.city)                                         
                     trim(cust.state)                                        
                     trim(cust.zip)                                                         
                     trim(string(cust.area-code,"(xxx)") + " " +
                          string(cust.phone,"xxx-xxxx"))                     
                     trim(string(substr(cust.fax,1,3),"(xxx)") + " " +
                          string(substr(cust.fax,4,7),"xxx-xxxx"))
                     c1                                      
                     cust-t[1]                                            
                     cust-t[2]
                     cust-t[3]
                     cust-t[4] 
                     SKIP. */ 
           END.
        END.
      END.

      do i = 1 to 4:
        ASSIGN
           grand-t[i] = grand-t[i] + curr-t[i]
           curr-t[i]  = 0.

        IF v-sep-fc THEN
           ASSIGN
              grand-t-pri[i] = grand-t-pri[i] + curr-t-pri[i]
              grand-t-fc[i] = grand-t-fc[i] + curr-t-fc[i]
              curr-t-pri[i] = 0
              curr-t-fc[i] = 0.
      end.
    end.
    
    m3 = "".
    if ni eq 1 then m3 = m2.
    ASSIGN
       v-cr-db-amt = 0
       v-disc-amt = 0.
  end.  /* for each cust record */

  IF ll-mult-curr THEN DO:
    HIDE FRAME r-top-1 NO-PAUSE.
    HIDE FRAME r-top-2 NO-PAUSE.
    PAGE.
  END.

  t1 = grand-t[1] + grand-t[2] + grand-t[3] + grand-t[4].

  /*display fill("_",132) WHEN det-rpt <> 3 format "x(131)"
    "GRAND TOTAL " AT 12  t1 to 54
    grand-t[1] to 77  format "->,>>>,>>>,>>9.99"
    grand-t[2] to 94 format "->,>>>,>>>,>>9.99"
    grand-t[3] to 112 format "->,>>>,>>>,>>9.99"
    grand-t[4] to 131 format "->,>>>,>>>,>>9.99"
    with frame grand1 no-box no-labels no-attr-space STREAM-IO WIDTH 200.*/
    
    RUN total-head("           GRAND TOTAL",t1,grand-t[1],grand-t[2],
                           grand-t[3],grand-t[4]).

    RUN total-head("PERCENTAGE COMPOSITION",0,(IF t1 NE 0 THEN (grand-t[1] / t1) * 100 ELSE 0),(IF t1 NE 0 THEN (grand-t[2] / t1) * 100 ELSE 0),
                           (IF t1 NE 0 THEN (grand-t[3] / t1) * 100 ELSE 0),(IF t1 NE 0 THEN (grand-t[4] / t1) * 100 ELSE 0)).

  /*display SPACE(11) "PERCENTAGE COMPOSITION"
    (IF t1 NE 0 THEN (grand-t[1] / t1) * 100 ELSE 0) to 77 format "->,>>>,>>>,>>9.99"
    (IF t1 NE 0 THEN (grand-t[2] / t1) * 100 ELSE 0) to 94 format "->,>>>,>>>,>>9.99"
    (IF t1 NE 0 THEN (grand-t[3] / t1) * 100 ELSE 0) to 112 format "->,>>>,>>>,>>9.99"
    (IF t1 NE 0 THEN (grand-t[4] / t1) * 100 ELSE 0) to 131 format "->,>>>,>>>,>>9.99"
    with frame grand2 STREAM-IO WIDTH 200 no-labels no-box no-attr-space.*/

    IF v-export THEN DO:
       IF NOT det-rpt = 1 THEN DO:
          IF NOT v-prt-add THEN DO:
             /*EXPORT STREAM s-temp DELIMITER ","
                " " 
                "GRAND TOTAL" 
                " "
                t1                                      
                grand-t[1]                                           
                grand-t[2]
                grand-t[3]
                grand-t[4]
                SKIP.
             EXPORT STREAM s-temp DELIMITER ","
                " " 
                "PERCENTAGE COMPOSITION" 
                " "
                " "                                      
                (IF t1 NE 0 THEN (grand-t[1] / t1) * 100 ELSE 0)                                           
                (IF t1 NE 0 THEN (grand-t[2] / t1) * 100 ELSE 0)
                (IF t1 NE 0 THEN (grand-t[3] / t1) * 100 ELSE 0)
                (IF t1 NE 0 THEN (grand-t[4] / t1) * 100 ELSE 0)
                SKIP.*/
          END.
          ELSE DO:
            /* EXPORT STREAM s-temp DELIMITER ","
                " " 
                "GRAND TOTAL" 
                " "                                      
                " "                                      
                " "                                         
                " "                                        
                " "                                                         
                " "                      
                " "
                t1                                      
                grand-t[1]                                           
                grand-t[2]
                grand-t[3]
                grand-t[4]
                SKIP.
             EXPORT STREAM s-temp DELIMITER ","
                " "
                "PERCENTAGE COMPOSITION" 
                " "                                      
                " "                                      
                " "                                         
                " "                                        
                " "                                                         
                " "                      
                " "
                " "                                      
                (IF t1 NE 0 THEN (grand-t[1] / t1) * 100 ELSE 0)                                           
                (IF t1 NE 0 THEN (grand-t[2] / t1) * 100 ELSE 0)
                (IF t1 NE 0 THEN (grand-t[3] / t1) * 100 ELSE 0)
                (IF t1 NE 0 THEN (grand-t[4] / t1) * 100 ELSE 0)
                SKIP.*/
          END.
       END.
    END.

  IF v-sep-fc THEN
  DO:
     ASSIGN
        t1-pri = grand-t-pri[1] + grand-t-pri[2] + grand-t-pri[3] + grand-t-pri[4]
        t1-fc =  grand-t-fc[1] + grand-t-fc[2] + grand-t-fc[3] + grand-t-fc[4].

     RUN total-head("      PRINCIPAL AMOUNT",t1-pri,grand-t-pri[1],grand-t-pri[2],
                           grand-t-pri[3],grand-t-pri[4]).

     /*DISPLAY 
       "PRINCIPAL AMOUNT " AT 12  t1-pri to 54
       grand-t-pri[1] to 77  format "->,>>>,>>>,>>9.99"
       grand-t-pri[2] to 94 format "->,>>>,>>>,>>9.99"
       grand-t-pri[3] to 112 format "->,>>>,>>>,>>9.99"
       grand-t-pri[4] to 131 format "->,>>>,>>>,>>9.99"
       with frame grand-pri no-box no-labels no-attr-space STREAM-IO WIDTH 200.*/

     RUN total-head("       FINANCE CHARGES",t1-fc,grand-t-fc[1],grand-t-fc[2],
                           grand-t-fc[3],grand-t-fc[4]).

     /*DISPLAY
       "FINANCE CHARGES " AT 12  t1-fc to 54
       grand-t-fc[1] to 77  format "->,>>>,>>>,>>9.99"
       grand-t-fc[2] to 94 format "->,>>>,>>>,>>9.99"
       grand-t-fc[3] to 112 format "->,>>>,>>>,>>9.99"
       grand-t-fc[4] to 131 format "->,>>>,>>>,>>9.99"
       with frame grand-fc no-box no-labels no-attr-space STREAM-IO WIDTH 200.*/
    IF v-export THEN DO:
       IF NOT det-rpt = 1 THEN DO:
          IF NOT v-prt-add THEN DO:
             /*EXPORT STREAM s-temp DELIMITER ","
                " " 
                "PRINCIPAL AMOUNT" 
                " "
                t1-pri                                      
                grand-t-pri[1]                                           
                grand-t-pri[2]
                grand-t-pri[3]
                grand-t-pri[4]
                SKIP.
             EXPORT STREAM s-temp DELIMITER ","
                " " 
                "FINANCE CHARGES" 
                " "
                t1-fc                                      
                grand-t-fc[1]                                           
                grand-t-fc[2]
                grand-t-fc[3]
                grand-t-fc[4]
                SKIP.*/
          END.
          ELSE DO:
             /*EXPORT STREAM s-temp DELIMITER ","
                " " 
                "PRINCIPAL AMOUNT" 
                " "                                      
                " "                                      
                " "                                         
                " "                                        
                " "                                                         
                " "                      
                " "
                t1-pri                                      
                grand-t-pri[1]                                           
                grand-t-pri[2]
                grand-t-pri[3]
                grand-t-pri[4]
                SKIP.
             EXPORT STREAM s-temp DELIMITER ","
                " "
                "FINANCE CHARGES" 
                " "                                      
                " "                                      
                " "                                         
                " "                                        
                " "                                                         
                " "                      
                " "
                t1-fc                                      
                grand-t-fc[1]                                           
                grand-t-fc[2]
                grand-t-fc[3]
                grand-t-fc[4]
                SKIP.*/
          END.
       END.
    END.
  END.


  STATUS DEFAULT "".

  return.
   /*-----------------------------------------------------------------------------*/
  procedure print-cust-add:
    IF det-rpt <> 3 THEN
    display cust.addr[1]                                                skip
            cust.addr[2]                                                skip
            trim(cust.city) + ", " +
            trim(cust.state) + "  " + trim(cust.zip) format "x(50)"
            
        with no-labels no-box frame cust-detail stream-io width 200.
  end.
   /*-----------------------------------------------------------------------------*/
  procedure export-data:
    def input parameter v-field-01 like ar-cashl.check-no no-undo.
    def input parameter v-field-02 like d                 no-undo.
    def input parameter v-field-03 like v-type            no-undo.
    def input parameter v-field-04 as   char              no-undo.
    def input parameter v-field-05 like ar-inv.inv-date   no-undo.
    def input parameter v-field-06 like amt               no-undo.
    def input parameter v-field-07 like ag                no-undo.
    def input parameter v-field-08 like ag                no-undo.
    def input parameter v-field-09 like ag                no-undo.
    def input parameter v-field-10 like ag                no-undo.
    DEF VAR v-delimiter AS cha NO-UNDO.       /* 9: tab 44: comma*/
    v-delimiter = "~t" /*CHR(9)*/ .
    /*put stream s-temp unformatted
        trim(cust.cust-no)                                      + v-delimiter +
        trim(cust.name)                                         + v-delimiter +
        trim(cust.contact)                                      + v-delimiter +
        trim(v-sman)                                            + v-delimiter +
        trim(if avail terms then terms.dscr else "")            + v-delimiter +
        trim(cust.addr[1])                                      + v-delimiter +
        trim(cust.addr[2])                                      + v-delimiter +
        trim(cust.city)                                         + v-delimiter +
        trim(cust.state)                                        + v-delimiter +
        trim(cust.zip)                                          + v-delimiter +
        trim(string(cust.cr-lim,">>>>>>>>9.99"))                + v-delimiter +
        trim(string(cust.area-code,"(xxx)") + " " +
             string(cust.phone,"xxx-xxxx"))                     + v-delimiter +
        trim(string(substr(cust.fax,1,3),"(xxx)") + " " +
             string(substr(cust.fax,4,7),"xxx-xxxx"))           + v-delimiter +
        trim(v-field-01)                                        + v-delimiter +
        trim(string(v-field-02,"->>>>"))                        + v-delimiter +
        trim(v-field-03)                                        + v-delimiter +
        trim(v-field-04)                                        + v-delimiter +
        trim(string(v-field-05,"99/99/9999"))                   + v-delimiter +
        trim(string(v-field-06,"->>>>>>>>9.99"))                + v-delimiter +
        trim(string(v-field-07,"->>>>>>>>9.99"))                + v-delimiter +
        trim(string(v-field-08,"->>>>>>>>9.99"))                + v-delimiter +
        trim(string(v-field-09,"->>>>>>>>9.99"))                + v-delimiter +
        trim(string(v-field-10,"->>>>>>>>9.99"))
        skip.
        */
    IF det-rpt = 1 THEN do:

    EXPORT STREAM s-temp DELIMITER ","
        trim(cust.cust-no)                                     
        trim(cust.name)                                         
        trim(cust.contact)                                      
        trim(v-sman)                                            
        trim(if avail terms then terms.dscr else "")            
        trim(cust.addr[1])                                      
        trim(cust.addr[2])                                      
        trim(cust.city)                                         
        trim(cust.state)                                        
        trim(cust.zip)                                          
        trim(string(cust.cr-lim,">>>>>>>>9.99"))                
        trim(string(cust.area-code,"(xxx)") + " " +
             string(cust.phone,"xxx-xxxx"))                     
        trim(string(substr(cust.fax,1,3),"(xxx)") + " " +
             string(substr(cust.fax,4,7),"xxx-xxxx"))           
        trim(v-field-01)                                        
        trim(string(v-field-02,"->>>>"))                        
        trim(v-field-03)                                        
        trim(v-field-04)                                        
        trim(string(v-field-05,"99/99/9999"))                   
        trim(string(v-field-06,"->>>>>>>>9.99"))                
        trim(string(v-field-07,"->>>>>>>>9.99"))  
        TRIM(STRING(cust.avg-pay,">>9"))                /*Task# 11151304*/
        TRIM(STRING(v-current-trend-days,"->>9"))       /*Task# 11151304*/ 
        trim(string(v-field-08,"->>>>>>>>9.99"))                
        trim(string(v-field-09,"->>>>>>>>9.99"))                
        trim(string(v-field-10,"->>>>>>>>9.99"))
        TRIM(STRING(IF cPoNo NE "" AND v-print-cust-po THEN cPoNo ELSE "")).    /*Task# 02071402*/

        ASSIGN cPoNo = "" . 
    END.
  end.
  
   /*-----------------------------------------------------------------------------*/
  PROCEDURE Display-InvNote:
    DEF VAR li AS INT NO-UNDO.

    ASSIGN lv-text = ""
           v-Inv-note = ""
           v-Collection-note = "".

    FOR EACH tt-formtext:
        DELETE tt-formtext.
    END.
    
       
    FOR EACH notes NO-LOCK WHERE notes.rec_key = ar-inv.rec_key
                                AND notes.note_type = "I" :
           lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
    END.

    DO li = 1 TO 8:
               CREATE tt-formtext.
               ASSIGN tt-line-no = li
                      tt-length  = 80.
    END.
    RUN custom/formtext.p (lv-text).
    i = 0.           
    FOR EACH tt-formtext:
           i = i + 1.
           IF  i <= 8 THEN v-inv-note[i] = tt-formtext.tt-text.      
    END.
    

    IF v-Inv-Note[1] <> "" THEN DO:
       PUT SKIP(1) "Invoice Note: " v-Inv-Note[1] SKIP.
       DO i = 2 TO 5:
          IF v-Inv-Note[i] > "" THEN 
            PUT v-Inv-Note[i] SKIP.
       END.
       PUT SKIP(1) .
    END.
  END.
   /*-----------------------------------------------------------------------------*/
  PROCEDURE Display-CollectionNote:
    DEF VAR li AS INT NO-UNDO.

    ASSIGN lv-text = ""
           v-Inv-note = ""
           v-Collection-note = "".

    FOR EACH tt-formtext:
        DELETE tt-formtext.
    END.
    
    FOR EACH notes NO-LOCK WHERE notes.rec_key = cust.rec_key
                                AND notes.note_type = "G"
                                AND notes.note_group = "Collection" :
        lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
    END.

    DO li = 1 TO 8:
               CREATE tt-formtext.
               ASSIGN tt-line-no = li
                      tt-length  = 80.
    END.
    RUN custom/formtext.p (lv-text).
    i = 0.           
    FOR EACH tt-formtext:
        i = i + 1.
        IF  i <= 8 THEN v-Collection-note[i] = tt-formtext.tt-text.      
    END.

    IF v-Collection-Note[1] <> "" THEN DO:
       PUT SKIP(1) "Collection Note: " v-Collection-Note[1] SKIP.
       DO i = 2 TO 5:
         IF v-Collection-Note[i] > "" THEN
           PUT v-Collection-Note[i] SKIP.
       END.
       PUT SKIP(1) .
    END.

  END PROCEDURE.
  /*-----------------------------------------------------------------------------*/
  PROCEDURE get-trend-days:
    DEFINE INPUT PARAMETER ip-trend-days AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER op-trend-days  AS INT NO-UNDO INIT 0.

    DEFINE BUFFER buf-ar-inv FOR ar-inv.

    DEF VAR v-days AS INT NO-UNDO.
    DEF VAR v-invs AS INT NO-UNDO.
    DEF VAR v-avg-days  LIKE cust.avg-pay NO-UNDO INIT 0.

    /* If zero trend days, then abort calculation. */
    IF ip-trend-days = 0 THEN RETURN.

    ASSIGN
        v-days = 0
        v-invs = 0.

    FOR each buf-ar-inv
        where buf-ar-inv.company  eq cust.company
        and buf-ar-inv.posted   eq yes
        and buf-ar-inv.cust-no  eq cust.cust-no
        and buf-ar-inv.due      le 0
        and buf-ar-inv.pay-date ge (today - ip-trend-days)
        USE-INDEX posted-due no-lock:

        ASSIGN v-days = v-days + (buf-ar-inv.pay-date - buf-ar-inv.inv-date)
               v-invs = v-invs + 1.

    END. /*  FOR each buf-ar-inv */
    
    ASSIGN v-avg-days = v-days / v-invs. 
  
    IF v-avg-days lt 1 or v-avg-days eq ? then v-avg-days = 1.
    ASSIGN op-trend-days = (cust.avg-pay - v-avg-days).


    RETURN.

  END PROCEDURE.

 PROCEDURE total-head:
     DEF INPUT PARAMETER vname AS CHAR .
     DEF INPUT PARAMETER amount AS DECIMAL.
     DEF INPUT PARAMETER vCURRENT AS DECIMAL.
     DEF INPUT PARAMETER per-day1 AS DECIMAL.
     DEF INPUT PARAMETER per-day2 AS DECIMAL.
     DEF INPUT PARAMETER per-day3 AS DECIMAL.

        ASSIGN cDisplay = ""
               cTmpField = ""
               cVarValue = ""
               cExcelDisplay = ""
               cExcelVarValue = "".
               
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
           cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                     WHEN "cust"      THEN cVarValue = ""  .
                     WHEN "cust-name" THEN cVarValue = ""  .
                     WHEN "cont"      THEN cVarValue = "" .
                     WHEN "sman"      THEN cVarValue = "" .
                     WHEN "term"      THEN cVarValue = ""  .
                     WHEN "add1"      THEN cVarValue = "".
                     WHEN "add2"      THEN cVarValue = "".
                     WHEN "city"      THEN cVarValue = "" .
                     WHEN "stat"      THEN cVarValue = "" .
                     WHEN "zip"       THEN cVarValue = ""  .
                     WHEN "cre-lim"   THEN cVarValue = "" .
                     WHEN "phone"     THEN cVarValue = "" .
                     WHEN "fax"       THEN cVarValue = "".
                     WHEN "chk-memo"  THEN cVarValue = "".
                     WHEN "day-old"   THEN cVarValue = "".
                     WHEN "type"      THEN cVarValue = "".
                     WHEN "inv"       THEN cVarValue = "" .
                     WHEN "inv-date"  THEN cVarValue = "" .
                     WHEN "amount"    THEN cVarValue = STRING(amount,"->>>>>>>>9.99").
                     WHEN "current"   THEN cVarValue = STRING(vCURRENT,"->>>>>>>>9.99").
                     WHEN "adtp"      THEN cVarValue = "".
                     WHEN "td"        THEN cVarValue = "".
                     WHEN "per-1"     THEN cVarValue = STRING(per-day1,"->>>>>>>>9.99") .
                     WHEN "per-2"     THEN cVarValue = STRING(per-day2,"->>>>>>>>9.99") .
                     WHEN "per-3"     THEN cVarValue = STRING(per-day3,"->>>>>>>>9.99")  .
                     WHEN "cust-po"   THEN cVarValue = "" .
                     WHEN "job"       THEN cVarValue = ""  .
                     WHEN "inv-note"  THEN cVarValue = "".
                     WHEN "coll-note" THEN cVarValue = "".
                    
                END CASE.
                  
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                           FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
        

        IF vname = "cust.cust-no" THEN do:
            PUT UNFORMATTED   cust.cust-no FORMAT "x(8)" space(1)  cust.name  FORMAT "x(25)"   substring(cDisplay,35,400) SKIP.
            IF v-export THEN DO:
                PUT STREAM s-temp UNFORMATTED   
                   cust.cust-no FORMAT "x(8)" space(1)  cust.name  FORMAT "x(25)" ','  substring(cExcelDisplay,4,400) SKIP(1).
            END.
        END.
        ELSE DO:
            PUT SKIP(1) str-line SKIP . 
            PUT UNFORMATTED  "          " vname  substring(cDisplay,33,400) SKIP.
            IF v-export THEN DO:
                PUT STREAM s-temp UNFORMATTED  
                 '                       ' vname  ','  substring(cExcelDisplay,4,400) SKIP(1).
            END.
        END.


        

 END PROCEDURE.

/* End ---------------------------------- Copr. 1997  Advanced Software, Inc. */


