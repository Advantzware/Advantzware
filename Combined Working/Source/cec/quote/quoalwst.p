/* ------------------------------------------- cec/quote/quoalwst.p 04/09 GDM */
/* print quotes in Pacific - Xprint format                                               */
/* -------------------------------------------------------------------------- */

{sys/inc/VAR.i shared}
{est/printquo.i}
{custom/notesDEF.i}

DEF SHARED BUFFER xquo FOR quotehd.

DEF BUFFER xqitm     FOR quoteitm.
DEF BUFFER xqqty     FOR quotEQty.
DEF BUFFER xqchg     FOR quotechg.
DEF BUFFER b-qi      FOR quoteitm.
DEF BUFFER x-qi      FOR quoteitm.
DEF BUFFER bf-report FOR report.
DEF BUFFER bf-eb     FOR eb.
DEF BUFFER bf-quo FOR quotehd.

DEF NEW SHARED VAR v-out1-id AS RECID NO-UNDO. 
DEF NEW SHARED VAR v-out2-id AS RECID NO-UNDO.

DEF VAR lv-FIRST-qRECID AS RECID NO-UNDO.

DEF VAR idx           AS INT         NO-UNDO.
DEF VAR idummy        AS INT         NO-UNDO.
DEF VAR numfit        AS INT         NO-UNDO.
DEF VAR cc            AS INT         NO-UNDO.
DEF VAR v-printline   AS INT INIT  0 NO-UNDO.
DEF VAR v-lines       AS INT         NO-UNDO.
DEF VAR v-rels        AS INT         NO-UNDO.
DEF VAR v-note-lines  AS INT         NO-UNDO.
DEF VAR v-note-length AS INT INIT 80 NO-UNDO.
DEF VAR li-cline      AS INT         NO-UNDO.
DEF VAR lv-pg-num     AS INT         NO-UNDO.
DEF VAR lv-tot-pg     AS INT INIT 1  NO-UNDO.
DEF VAR ln-cnt        AS INT         NO-UNDO.
DEF VAR lv-itm-ln-cnt AS INT         NO-UNDO.
DEF VAR intPageNum    AS INT         NO-UNDO.

DEF VAR k_frac       AS DEC INIT 6.25 NO-UNDO.
DEF VAR tot          AS DEC           NO-UNDO.
DEF VAR v-line-total AS DEC           NO-UNDO.
DEF VAR v-quo-total  AS DEC           NO-UNDO.
DEF VAR v-t-tax      as DEC EXTENT  3 NO-UNDO.
DEF VAR ld-metric    AS DEC INIT 1    NO-UNDO.
DEF VAR ld-wid       AS DEC           NO-UNDO.
DEF VAR ld-len       AS DEC           NO-UNDO.
DEF VAR ld-dep       AS DEC           NO-UNDO.

DEF VAR sold            AS CHAR EXTENT 5 FORMAT "x(30)"  NO-UNDO.
DEF VAR bill            AS CHAR EXTENT 5 FORMAT "x(30)"  NO-UNDO.
DEF VAR ship            AS CHAR EXTENT 5 FORMAT "x(30)"  NO-UNDO.
DEF VAR v-over-under    AS CHAR                          NO-UNDO.
DEF VAR v-board         AS CHAR                          NO-UNDO.
DEF VAR v-tel           AS CHAR FORMAT "x(30)"           NO-UNDO.
DEF VAR v-fax           AS CHAR FORMAT "x(30)"           NO-UNDO.
DEF VAR v-comp-add1     AS CHAR FORMAT "x(30)"           NO-UNDO.
DEF VAR v-comp-add2     AS CHAR FORMAT "x(30)"           NO-UNDO.
DEF VAR v-comp-add3     AS CHAR FORMAT "x(30)"           NO-UNDO.
DEF VAR v-comp-add4     AS CHAR FORMAT "x(30)"           NO-UNDO.
DEF VAR v-comp-add5     AS CHAR FORMAT "x(30)"           NO-UNDO.
DEF VAR v-bot-lab       AS CHAR extent 3 FORMAT "x(63)"  NO-UNDO.
DEF VAR style-dscr      AS CHAR FORMAT "x(30)"           NO-UNDO.
DEF VAR lv-est-no       AS CHAR                          NO-UNDO.
DEF VAR lv-comp-name    AS CHAR FORMAT "x(30)"           NO-UNDO.
DEF VAR lv-email        AS CHAR FORMAT "x(40)"           NO-UNDO.
DEF VAR lv-comp-color   AS CHAR                          NO-UNDO.
DEF VAR lv-other-color  AS CHAR INIT "BLACK"             NO-UNDO.
DEF VAR v-inst2         AS CHAR EXTENT 20                NO-UNDO.
DEF VAR v-dept-inst     AS CHAR EXTENT 20 FORMAT "x(80)" NO-UNDO.
DEF VAR lv-part-dscr1   AS CHAR FORMAT "x(30)"           NO-UNDO.
DEF VAR lv-fg#          AS CHAR FORMAT "x(15)"           NO-UNDO.
DEF VAR v-prep-prt-list AS CHAR                          NO-UNDO.
DEF VAR lv-format       AS CHAR INIT ">>>>>9.9<<<<"      NO-UNDO.
DEF VAR lv-part-dscr2   AS CHAR                          NO-UNDO. 
DEF VAR lv-i-coldscr    AS CHAR                          NO-UNDO. 
DEF VAR chrX            AS CHAR                          NO-UNDO. 
DEF VAR ls-image1       AS CHAR                          NO-UNDO.
DEF VAR ls-full-img1    AS CHAR FORMAT "x(200)"           NO-UNDO.

DEF VAR v-comp-name      LIKE company.name extent 4 NO-UNDO.
DEF VAR trim-size        LIKE quoteitm.size         NO-UNDO.
DEF VAR temp-trim-size   LIKE quoteitm.size         NO-UNDO.
DEF VAR v-FIRST-q-no     LIKE quotehd.q-no          NO-UNDO.
DEF VAR v-part           LIKE quoteitm.part-no      NO-UNDO.
DEF VAR v-contact        LIKE quotehd.contact       NO-UNDO.
DEF VAR lv-chg-amt       LIKE quotechg.amt          NO-UNDO.
DEF VAR lv-FIRST-qreckey LIKE quotehd.rec_key       NO-UNDO.

DEF VAR v-last          AS LOG INIT no NO-UNDO.
DEF VAR lv-display-comp AS LOG         NO-UNDO. 
DEF VAR v-prep-printed  AS LOG         NO-UNDO.
DEF VAR ll-prt-dscr2    AS LOG         NO-UNDO.
DEF VAR adder-print     AS LOG         NO-UNDO.
DEF VAR logSetPrinting  AS LOG         NO-UNDO.
DEF VAR logPrint        AS LOG         NO-UNDO.

DEF VAR v-quo-date AS DATE FORM "99/99/9999" NO-UNDO.


ASSIGN
  ls-image1 = "images\allwestl.jpg"
  FILE-INFO:FILE-NAME = ls-image1
  ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

{sys/inc/f16to32.i}
{cecrep/jobtick2.i "new shared"}

ASSIGN tmpstore = fill("-",130).

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "QUOPRINT" NO-ERROR.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld 
    THEN lv-display-comp = YES.
    ELSE lv-display-comp = NO.
        
FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "LOGOCOLR" NO-ERROR.
IF AVAIL sys-ctrl 
    THEN lv-comp-color = sys-ctrl.char-fld.
    ELSE lv-comp-color = "BLACK".

FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.

IF lv-display-comp THEN DO:
   FIND FIRST cust WHERE cust.company = cocode 
                     AND cust.active = "X" NO-LOCK NO-ERROR.
   IF AVAIL cust THEN
     ASSIGN v-comp-add1 = cust.addr[1]
            v-comp-add2 = cust.addr[2]
            v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
            v-comp-add4 = "Phone:  " + STRING(cust.area-code,"(999)") + STRING(cust.phone,"999-9999") 
            v-comp-add5 = "Fax     :  " + STRING(cust.fax,"(999)999-9999") 
            lv-email    = "Email:  " + cust.email 
            lv-comp-name = cust.NAME.
END.

FIND FIRST report WHERE report.term-id EQ v-term-id NO-LOCK NO-ERROR.

FIND FIRST xquo  WHERE RECID(xquo) EQ report.rec-id NO-LOCK NO-ERROR.
IF NOT AVAIL xquo THEN RETURN.

ASSIGN v-quo-date = xquo.quo-date.

FIND FIRST est WHERE est.company EQ xquo.company
                 AND est.est-no EQ xquo.est-no NO-LOCK NO-ERROR.
FIND FIRST sman
    WHERE sman.company EQ cocode
      AND sman.sman    EQ xquo.sman NO-LOCK NO-ERROR.

FIND FIRST carrier
    WHERE carrier.company EQ cocode
      AND carrier.carrier EQ xquo.carrier NO-LOCK NO-ERROR.

FIND FIRST terms
    WHERE terms.company EQ cocode
      AND terms.t-code  EQ xquo.terms NO-LOCK NO-ERROR.

FIND FIRST cust
    WHERE cust.company EQ xquo.company
      AND cust.cust-no EQ xquo.cust-no NO-LOCK NO-ERROR.
IF AVAIL cust THEN
    ASSIGN 
      v-over-under = TRIM(STRING(cust.over-pct,">>9%")) + "-" +
                     TRIM(STRING(cust.under-pct,">>9%")).

ASSIGN
    sold[5] = TRIM(STRING(xquo.sold-no))
    ship[5] = TRIM(STRING(xquo.ship-id))
    bill[5] = TRIM(STRING(xquo.cust-no)).

DO i = 1 to 4:
    ASSIGN 
        sold[i] = xquo.soldto[i]
        ship[i] = xquo.shipto[i]
        bill[i] = xquo.billto[i].
END.

IF (xquo.shipto[1] EQ xquo.soldto[1] AND
    xquo.shipto[2] EQ xquo.soldto[2] AND
    xquo.shipto[3] EQ xquo.soldto[3] AND
    xquo.shipto[4] EQ xquo.soldto[4]) 
  THEN
    ASSIGN
    ship[1] = "" ship[2] = ""
    ship[3] = "" ship[4] = ""
    ship[5] = "SAME".

ASSIGN
    v-FIRST-q-no = xquo.q-no
    v-line-total = 0
    v-printline = 0.

PUT "<Farial>". 

ln-cnt = 0.

IF (not ch-multi) THEN DO:

    /* get total page number */
    FOR EACH xqitm OF xquo NO-LOCK:
        ln-cnt = ln-cnt + 1.
    END.

    ln-cnt = ln-cnt * 5.

    FOR EACH xqitm OF xquo NO-LOCK,
        EACH xqqty OF xqitm NO-LOCK,
        EACH xqchg NO-LOCK 
         WHERE xqchg.company = xqqty.company
           AND xqchg.loc = xqqty.loc
           AND xqchg.q-no = xqqty.q-no
           AND xqchg.LINE = xqqty.LINE
           AND xqchg.qty = xqqty.qty 
         BREAK BY xqchg.charge:

         IF FIRST-of(xqchg.charge) THEN ln-cnt = ln-cnt + 1.
    END.


    FOR EACH xqchg OF xquo NO-LOCK
        WHERE xqchg.qty  EQ 0
          AND xqchg.line EQ 0
        BREAK BY xqchg.charge:

        IF FIRST(xqchg.charge) THEN ln-cnt = ln-cnt + 1.
    END.

    IF s-print-comp THEN DO :      /* Print components of a set */
        
        FIND FIRST est NO-LOCK 
            WHERE est.company EQ xquo.company
              AND est.est-no  EQ xquo.est-no NO-ERROR.
        IF AVAIL est AND 
           est.est-type EQ 6 AND est.form-qty GT 1 
          THEN
            FOR EACH ef NO-LOCK
              WHERE ef.company EQ est.company
                AND ef.est-no  EQ est.est-no,
              EACH eb OF ef NO-LOCK BREAK BY ef.form-no :
            
               ln-cnt = ln-cnt + 4.
            END. /* FOR EACH ef */
    END. /* IF s-print-comp */

    ASSIGN lv-tot-pg = lv-tot-pg + TRUNC( ln-cnt / 25,0) .
    
    /* get total page number */

    {cec/quote/quoalwst2.i}

    {cec/quote/quoalwst.i 1}

    ASSIGN v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].

    IF LINE-COUNTER > PAGE-SIZE - 22 THEN DO:      
        v-printline = 0.
        page.  
        {cec/quote/quoalwst2.i}
    END.

    RUN verbage.
/**
    PUT "<FArial><R58><C1><P12><B> Comments </B> <P9> " .
    
    ASSIGN
        v-tmp-lines = 0
        li-cline = 1.
    
    DO i = 1 to 5:
      IF xquo.comment[i] ne "" THEN DO: 
          PUT "<C1><R" STRING(58 + li-cline,">9") + "><C6>" xquo.comment[i]. 
        li-cline = li-cline + 1.
      END.
    END.
*/
    IF v-printline < 50 THEN PAGE.

    RELEASE est.
    IF v-prt-box THEN DO:
        FIND FIRST est NO-LOCK
            WHERE est.company EQ xquo.company
              AND est.loc     EQ xquo.loc
              AND est.est-no  EQ xquo.est-no NO-ERROR.
        IF AVAIL est THEN DO:
            PAGE.
            PUT "<FCouriar New>" SKIP.
            FIND FIRST xest WHERE RECID(xest) = RECID(est) NO-LOCK NO-ERROR.
            
            RUN cec/desprnt2.p (?,input-output v-lines, RECID(xest)).
        END.
    END. /* IF v-prt-box */

END. /* IF (not ch-multi */
ELSE DO:

    FOR EACH report 
       WHERE report.term-id EQ v-term-id,
     FIRST xquo NO-LOCK 
       WHERE RECID(xquo) EQ report.rec-id
       BREAK BY report.key-01
             BY report.key-02
             BY report.key-03 
      TRANSACTION:

        FIND FIRST est NO-LOCK
            WHERE est.company EQ xquo.company
            AND est.est-no  EQ xquo.est-no NO-ERROR.

        FIND FIRST sman NO-LOCK
            WHERE sman.company EQ cocode
              AND sman.sman    EQ xquo.sman NO-ERROR.

        FIND FIRST carrier NO-LOCK
            WHERE carrier.company EQ cocode
              AND carrier.carrier EQ xquo.carrier NO-ERROR.

        FIND FIRST terms NO-LOCK
            WHERE terms.company EQ cocode
              AND terms.t-code  EQ xquo.terms NO-ERROR.

        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ xquo.company
              AND cust.cust-no EQ xquo.cust-no NO-ERROR.
        IF AVAIL cust 
          THEN
           ASSIGN
            v-over-under = TRIM(STRING(cust.over-pct,">>9%")) + "-" +
                           TRIM(STRING(cust.under-pct,">>9%")).

        ASSIGN
            sold[5] = TRIM(STRING(xquo.sold-no))
            ship[5] = TRIM(STRING(xquo.ship-id))
            bill[5] = TRIM(STRING(xquo.cust-no)).

        DO i = 1 TO 4:
            assign
                sold[i] = xquo.soldto[i]
                ship[i] = xquo.shipto[i]
                bill[i] = xquo.billto[i].
        END.

        IF s-sep-page OR 
           FIRST(report.key-01) 
          THEN
           ASSIGN lv-FIRST-qreckey = xquo.rec_key
                  lv-FIRST-qRECID = RECID(xquo).

        IF s-sep-page OR FIRST-of(report.key-01) THEN DO:

            v-printline = 0.
            
            IF NOT FIRST(report.key-01) THEN PAGE.

            ASSIGN
                v-FIRST-q-no = xquo.q-no
                lv-tot-pg = 1
                ln-cnt = 0.
            
            /* get total page number */
            FOR EACH bf-report 
              WHERE bf-report.term-id EQ v-term-id
                AND bf-report.key-01 EQ report.key-01,
              FIRST bf-quo  WHERE RECID(bf-quo) EQ bf-report.rec-id,
              EACH xqitm OF bf-quo NO-LOCK
               BREAK BY bf-quo.q-no by xqitm.part-no: 

                ASSIGN lv-itm-ln-cnt = lv-itm-ln-cnt + 1.

                IF LAST-OF(bf-quo.q-no) THEN DO:
                    ASSIGN ln-cnt = ln-cnt + lv-itm-ln-cnt * 5.
                           lv-itm-ln-cnt = 0.              
                END. /* LAST-OF */
            END. /* FOR EACH bf-report */

            FOR EACH bf-report 
              WHERE bf-report.term-id EQ v-term-id
                AND bf-report.key-01 = report.key-01,
               FIRST bf-quo  WHERE RECID(bf-quo) EQ bf-report.rec-id,
              EACH xqitm OF bf-quo NO-LOCK,
              EACH xqqty OF xqitm NO-LOCK,
              EACH xqchg NO-LOCK
               WHERE xqchg.company = xqqty.company
                 AND xqchg.loc = xqqty.loc
                 AND xqchg.q-no = xqqty.q-no
                 AND xqchg.LINE = xqqty.LINE
                 AND xqchg.qty = xqqty.qty
                BREAK BY xqchg.qty BY xqchg.charge:

                IF FIRST(xqchg.charge) 
                  THEN ASSIGN ln-cnt = ln-cnt + 1.

                ln-cnt = ln-cnt + 1.

                IF LAST-OF(xqchg.qty) THEN LEAVE.
            END. /* FOR EACH bf-report */

            FOR EACH bf-report 
              WHERE bf-report.term-id EQ v-term-id
                AND bf-report.key-01 = report.key-01,
              FIRST bf-quo WHERE RECID(bf-quo) EQ bf-report.rec-id,
              EACH xqchg OF bf-quo NO-LOCK
                WHERE xqchg.qty  EQ 0 
                  AND xqchg.line EQ 0 
                BREAK BY xqchg.charge:

                IF FIRST(xqchg.charge) THEN ln-cnt = ln-cnt + 1.
            END. /* FOR EACH bf-report */


            IF s-print-comp THEN DO :   /* Print components of a set */
                
                FOR EACH bf-report 
                  WHERE bf-report.term-id EQ v-term-id
                      AND bf-report.key-01 = report.key-01 NO-LOCK,
                  FIRST bf-quo NO-LOCK WHERE RECID(bf-quo) EQ bf-report.rec-id,
                  FIRST est NO-LOCK
                    WHERE est.company EQ bf-quo.company
                      AND est.est-no  EQ bf-quo.est-no 
                      AND est.est-type EQ 6 
                      AND est.form-qty GT 1,
                  EACH ef NO-LOCK 
                    WHERE ef.company EQ est.company
                      AND ef.est-no  EQ est.est-no,
                  EACH eb OF ef NO-LOCK BREAK BY ef.form-no :

                    ASSIGN ln-cnt = ln-cnt + 4.
                END. /* FOR EACH bf-report  */
            END. /* IF s-print-comp */

            IF ch-inst THEN DO:

                FOR EACH bf-report 
                  WHERE bf-report.term-id EQ v-term-id
                    AND bf-report.key-01 = report.key-01,
                  FIRST bf-quo  WHERE RECID(bf-quo) EQ bf-report.rec-id,
                  FIRST est NO-LOCK
                    WHERE est.company = bf-quo.company
                      AND est.est-no = bf-quo.est-no.

                    ASSIGN v-inst2 = "".

                    {custom/notespr2.i job v-inst2 
                        EXTENT(v-inst2) 
                        "notes.rec_key = est.rec_key AND 
                        notes.note_code >= fdept AND notes.note_code <= tdept " }

                   ASSIGN ln-cnt = ln-cnt + 1.

                   DO idx = 1 TO EXTENT(v-inst2):
                       IF v-inst2[idx] NE '' THEN ln-cnt = ln-cnt + 1.
                   END. /* DO */
                END. /* FOR EACH bf-report  */
            END.  /* IF ch-inst */

            ASSIGN lv-tot-pg = lv-tot-pg + TRUNC( ln-cnt / 25,0) .
            
            /* get total page number */
            {cec/quote/quoalwst2.i}
        END. /* IF s-sep-page OR FIRST-of(report.key-01) */

        ASSIGN
            v-last = last-of(report.key-01)
            v-line-total = 0.


        {cec/quote/quoalwst.i 2}  
         ASSIGN v-quo-total = v-line-total + 
                              v-t-tax[1] + v-t-tax[2] + v-t-tax[3].

        IF LINE-COUNTER > PAGE-SIZE - 22 THEN DO:      
            v-printline = 0.
            PAGE.  
            {cec/quote/quoalwst2.i}
        END.

        IF (ch-multi AND (v-last OR s-sep-page)) THEN DO:

            RUN verBage.
/*            
            PUT "<FArial><R58><C1><P12><B> Comments </B> <P9> ".

            FIND bf-quo WHERE RECID(bf-quo) = lv-FIRST-qRECID NO-LOCK NO-ERROR.

            ASSIGN li-cline = 0.

            DO i = 1 TO 5:      
                if bf-quo.comment[i] ne "" THEN DO:
                    li-cline = li-cline + 1.
                    PUT 
                        "<C1><R" STRING(58 + li-cline,">9") + 
                        "><C6>" bf-quo.comment[i] .
                END. /*  */
            END. /*  */
*/            

            IF v-printline < 50 THEN DO:
                PAGE.
            END.

            IF v-prt-box THEN DO:
                FIND FIRST est NO-LOCK 
                    WHERE est.company EQ xquo.company 
                      AND est.loc     EQ xquo.loc 
                      AND est.est-no  EQ xquo.est-no NO-ERROR.
                IF AVAIL est THEN DO:
                    PAGE.
                    PUT "<FCouriar New>" SKIP.
                    FIND FIRST xest 
                        WHERE RECID(xest) = RECID(est) NO-LOCK NO-ERROR.
                    RUN cec/desprnt2.p (?,input-output v-lines,RECID(xest)).
                END. /* IF AVAIL est */
            END. /* IF v-prt-box */
        END.  /*ch-multi AND v-last */
    END. /* for each report */
END.  /* multi */

PROCEDURE printHeader:
  DEFINE INPUT PARAMETER ipPageOffSet AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opInitVAR AS INTEGER NO-UNDO.

  IF LINE-COUNTER > PAGE-SIZE - ipPageOffSet THEN DO:
    PAGE.
    {cec/quote/quoalwst2.i}
    opInitVAR = 0.
  END.
END PROCEDURE.

PROCEDURE verBage:

    PUT 
     "<FArial><R51.5><C1><P9>"
     "Cancellation cannot be made if order is in the process of manufacture, without assuming full liability."           SKIP
     "Customer agrees to pay for the following additional charges which may apply but may not be quoted above:"          SKIP
     " * appropriate sales taxes       * die, plate and/or film charges (printing/cutting)     * relevant freight fees"  SKIP
     " * kit charges for special inks  * control fee for exact quantities (when requested)"                              SKIP   
     .


    PUT "<|10><R56><C1><#5><FROM><R63><C80><RECT>" SKIP    
        "<C2><R56.5><#6><P12><B>CUSTOMER ACCEPTANCE: </B><P9>" SKIP
        "<=6><R58>"
        "Signature below by an authorized representative of Customer authorizes All West Container Company (and its designated affiliates)" SKIP
        "<=6><R59>"
        "to begin production of the item(s) listed above at the specified quantity and designation. Customer agrees to pay quoted prices"   SKIP
        "<=6><R60>"
        "(and any associated charges) within specified terms and agrees to the detail above."                                                SKIP
        "<=6><R61>"
        " _________________                     ______________________________________                              __________________________ " SKIP
        "<=6><R62>"
        "             Date                                         Authorized Representative of Customer                                                Purchase Order No."
        " " SKIP
        "           30 Tanforan Avenue, South San Francisco, CA 94080-6608" "    (650) 615-8970    Fax (650) 615-8974     www.allwestcontainer.com"
        .

    ASSIGN v-printline = v-printline + 12.
    

END PROCEDURE.
/* end ---------------------------------- copr. 2000  advanced software, inc. */
