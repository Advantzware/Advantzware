/* ------------------------------------------- cec/quote/quosmkct.p 05/18 GDM */
/* N-K = QUOPRINT - SIMKINS Xprint FORmat                                     */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xquo FOR quotehd.

DEF buffer xqitm     FOR quoteitm. 
DEF buffer xqqty     FOR quotEQty. 
DEF buffer xqchg     FOR quotechg. 
DEF buffer b-qi      FOR quoteitm. 
DEF buffer x-qi      FOR quoteitm.
DEF BUFFER bf-eb     FOR eb.
DEF BUFFER bf-report FOR report.
DEF BUFFER bf-quo    FOR quotehd.
{est/printquo.i}

DEF NEW SHARED VAR v-out1-id  AS RECID NO-UNDO.
DEF NEW SHARED VAR v-out2-id  AS RECID NO-UNDO.

DEF VAR idx            AS INT                        NO-UNDO.
DEF VAR idummy         AS INT                        NO-UNDO.
DEF VAR k_frac         AS DEC INIT 6.25              NO-UNDO.           
DEF VAR numfit         AS INT                        no-undo.
DEF VAR sold           AS CHAR extent 5 FORMAT "x(30)" no-undo.
DEF VAR bill           AS CHAR extent 5 FORMAT "x(30)" no-undo.
DEF VAR ship           AS CHAR extent 5 FORMAT "x(30)" no-undo.
DEF VAR v-bill         AS CHAR extent 5 FORMAT "x(30)" no-undo.
DEF VAR v-shp2         AS CHAR extent 5 FORMAT "x(30)" no-undo.
DEF VAR tot            AS DEC                        no-undo.                      
DEF VAR v-over-under   AS CHAR                       no-undo.
DEF VAR v-comp-name    LIKE company.name extent 4    NO-UNDO.         
DEF VAR trim-size      LIKE quoteitm.size            no-undo. 
DEF VAR temp-trim-size LIKE quoteitm.size            no-undo. 
DEF VAR cc             AS int                        no-undo.
DEF VAR v-printline    AS int initial 0              no-undo.
DEF VAR v-first-q-no   LIKE quotehd.q-no             no-undo.
DEF VAR v-lines        AS INT                        NO-UNDO.  
DEF VAR v-rels         AS int                        NO-UNDO.
DEF VAR v-part         LIKE quoteitm.part-no         no-undo.
DEF VAR v-board        AS CHAR                       no-undo.
DEF VAR v-last         AS LOG INITIAL NO             no-undo.
DEF VAR v-quo-date     AS DATE FORMAT "99/99/9999"   NO-UNDO.
DEF VAR v-contact      LIKE quotehd.contact          NO-UNDO.

DEF VAR v-tel           AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-fax           AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-comp-add1     AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-comp-add2     AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-comp-add3     AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-comp-add4     AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-comp-add5     AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-line-total    AS DEC                          NO-UNDO.
DEF VAR v-quo-total     AS DEC                          NO-UNDO.
DEF VAR v-t-tax         AS DEC  EXTENT 3                NO-UNDO.
DEF VAR v-bot-lab       AS CHAR FORMAT "x(63)" extent 3 NO-UNDO.
DEF VAR style-dscr      AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR lv-est-no       AS CHAR                         NO-UNDO.
DEF VAR lv-chg-amt      LIKE quotechg.amt               NO-UNDO.
DEF VAR lv-display-comp AS LOG                          NO-UNDO.
DEF VAR lv-comp-name    AS cha FORMAT "x(30)"           NO-UNDO.
DEF VAR lv-email        AS cha FORMAT "x(40)"           NO-UNDO.
DEF VAR v-note-lines    AS INT                          NO-UNDO.

DEF VAR lv-first-qreckey LIKE quotehd.rec_key NO-UNDO.
DEF VAR lv-first-qrecid  AS RECID             NO-UNDO.
DEF VAR lv-comp-color    AS CHAR              NO-UNDO.
DEF VAR lv-other-color   AS CHAR INIT "BLACK" NO-UNDO.

{custom/notesdef.i}

DEF VAR v-inst2         AS cha EXTENT 20                NO-UNDO.              
DEF VAR v-dept-inst     AS cha FORMAT "x(80)" EXTENT 20 NO-UNDO. 
DEF VAR v-note-length   AS INT INIT 80                  NO-UNDO.
DEF VAR li-cline        AS INT                          NO-UNDO.  
DEF VAR lv-pg-num       AS INT                          NO-UNDO.
DEF VAR lv-tot-pg       AS INT INIT 1                   NO-UNDO.
DEF VAR ln-cnt          AS INT                          NO-UNDO.
DEF VAR lv-itm-ln-cnt   AS INT                          NO-UNDO.
DEF VAR lv-part-dscr1   AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR lv-fg#          AS CHAR FORMAT "x(15)"          NO-UNDO.
DEF VAR v-prep-printed  AS LOG                          NO-UNDO.
DEF VAR v-prep-prt-list AS CHAR                         NO-UNDO.
DEF VAR ld-metric       AS DEC INIT 1                   NO-UNDO.
DEF VAR lv-FORmat       AS CHAR INIT ">>>>>9.9<<<<"     NO-UNDO.
DEF VAR ld-wid          AS DEC                          NO-UNDO. 
DEF VAR ld-len          AS DEC                          NO-UNDO.
DEF VAR ld-dep          AS DEC                          NO-UNDO.
DEF VAR lv-part-dscr2   AS CHAR                         NO-UNDO.
DEF VAR lv-i-coldscr    AS CHAR                         NO-UNDO.
DEF VAR ll-prt-dscr2    AS LOG                          NO-UNDO.
DEF VAR adder-print     AS LOG                          NO-UNDO.

DEF VAR logSetPrinting  AS LOG                          NO-UNDO.
DEF VAR chrX            AS CHAR                         NO-UNDO.
DEF VAR logPrint        AS LOG                          NO-UNDO.
DEF VAR intPageNum      AS INT                          NO-UNDO.
DEF VAR v-text1         AS CHAR FORMAT "x(170)" EXTENT 10 NO-UNDO.

{sys/inc/f16to32.i}
{cecrep/jobtick2.i "new shared"}

{custom/formtext.i NEW} 

ASSIGN tmpstore = FILL("-",130).

DEF VAR v-text          AS CHAR                         NO-UNDO.
DEF VAR v-licnt         AS INT                          NO-UNDO.
DEF VAR v-notes         AS CHAR FORMAT "x(80)" EXTENT 5 NO-UNDO.
DEF VAR note-count      AS INT                          NO-UNDO. 

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "QUOPRINT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld 
  THEN ASSIGN lv-display-comp = YES.
  ELSE ASSIGN lv-display-comp = NO.

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "LOGOCOLR" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl 
  THEN ASSIGN lv-comp-color = sys-ctrl.char-fld.
  ELSE ASSIGN lv-comp-color = "BLACK".

FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.

IF lv-display-comp THEN DO:
   FIND FIRST cust 
     WHERE cust.company EQ cocode 
       AND cust.active EQ "X" NO-LOCK NO-ERROR.
   IF AVAIL cust THEN
     ASSIGN v-comp-add1 = cust.addr[1]
            v-comp-add2 = cust.addr[2]
            v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
            v-comp-add4 = "Phone:  " + STRING(cust.area-code,"(999)") + 
                                       STRING(cust.phone,"999-9999") 
            v-comp-add5 = "Fax     :  " + STRING(cust.fax,"(999)999-9999") 
            lv-email    = "Email:  " + cust.email 
            lv-comp-name = cust.NAME.
END.

FIND FIRST report WHERE report.term-id EQ v-term-id NO-LOCK NO-ERROR.
FIND FIRST xquo  WHERE recid(xquo) EQ report.rec-id NO-LOCK NO-ERROR.
IF NOT AVAIL xquo THEN RETURN.

ASSIGN v-quo-date = xquo.quo-date.

FIND FIRST est 
   WHERE est.company EQ xquo.company
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
IF AVAIL cust 
  THEN ASSIGN v-over-under = TRIM(STRING(cust.over-pct,">>9%")) + "-" +
                             TRIM(STRING(cust.under-pct,">>9%")).

ASSIGN
  sold[5] = TRIM(STRING(xquo.sold-no))
  ship[5] = TRIM(STRING(xquo.ship-id))
  bill[5] = TRIM(STRING(xquo.cust-no)).

DO i = 1 TO 4:
  ASSIGN sold[i] = xquo.soldto[i]
         ship[i] = xquo.shipto[i]
         bill[i] = xquo.billto[i].
END.

IF (xquo.shipto[1] EQ xquo.soldto[1] AND
    xquo.shipto[2] EQ xquo.soldto[2] AND
    xquo.shipto[3] EQ xquo.soldto[3] AND
    xquo.shipto[4] EQ xquo.soldto[4]) 
  THEN
   ASSIGN
     ship[1] = "" ship[2] = ""  ship[3] = ""
     ship[4] = "" ship[5] = "SAME".

ASSIGN
  v-first-q-no = xquo.q-no
  v-line-total = 0
  v-printline = 0.

PUT "<Farial>". 

ASSIGN ln-cnt = 0.

IF (NOT ch-multi) THEN DO:

  /* get total page number */                                   
  FOR EACH xqitm OF xquo NO-LOCK:                               
      ASSIGN ln-cnt = ln-cnt + 1.                               
  END.                                                          
                                                                
  ASSIGN ln-cnt = ln-cnt * 5.                                   
                                                                
  FOR EACH xqitm OF xquo  NO-LOCK,                              
      EACH xqqty OF xqitm NO-LOCK,                              
      EACH xqchg          NO-LOCK                               
       WHERE xqchg.company EQ xqqty.company                     
        AND xqchg.loc      EQ xqqty.loc                         
        AND xqchg.q-no     EQ xqqty.q-no                        
        AND xqchg.LINE     EQ xqqty.LINE                        
        AND xqchg.qty      EQ xqqty.qty                         
        BREAK BY xqchg.charge:                                  
                                                                
      IF FIRST-OF(xqchg.charge) THEN ASSIGN ln-cnt = ln-cnt + 1.
  END.                                                          
                                                                
  FOR EACH xqchg OF xquo NO-LOCK                                
    WHERE xqchg.qty  EQ 0                                       
      AND xqchg.line EQ 0                                       
     BREAK BY xqchg.charge:                                     
                                                                
      IF FIRST(xqchg.charge) THEN ASSIGN ln-cnt = ln-cnt + 1.   
  END.                                                          
  
  IF s-print-comp THEN DO:                                                  
                                                                            
    FIND FIRST est                                                          
        WHERE est.company EQ xquo.company                                   
          AND est.est-no  EQ xquo.est-no NO-LOCK NO-ERROR.                  
    IF AVAIL est AND est.est-type EQ 6 AND est.FORm-qty GT 1                
      THEN                                                                  
       FOR EACH ef NO-LOCK                                                  
         WHERE ef.company EQ est.company                                    
           AND ef.est-no  EQ est.est-no,                                    
         EACH eb OF ef NO-LOCK BREAK BY ef.FORm-no :                        
         ASSIGN ln-cnt = ln-cnt + 4.                                        
       END.                                                                 
  END. /* IF s-print-comp */
                                                                            
  ASSIGN lv-tot-pg = lv-tot-pg + TRUNC( ln-cnt / 25,0).                     
                                                                            
  /* get total page number */                                               
  {cec/quote/quosmkt2.i}                                                    
                                                                            
  {cec/quote/quosmkct.i 1}                                                  
                                                                            
  ASSIGN v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3]. 
                                                                            
  IF LINE-COUNTER > PAGE-SIZE - 1 THEN DO:                                  
    v-printline = 0.                                                        
    PAGE.                                                                   
    {cec/quote/quosmkt2.i}                                                  
  END.                                                                      
                                                                            
  PUT "<FArial><R58><C1><P12><B> Comments </B> <P9> " .                     
                                                                            
  ASSIGN                                                                    
    v-tmp-lines = 0                                                         
    li-cline = 1.                                                           

  DO i = 1 TO 5:                                                         
    IF xquo.comment[i] NE "" THEN DO:                                    
                                                                         
       PUT "<C1><R" STRING(58 + li-cline,">9") + "><C6>" xquo.comment[i].
       ASSIGN li-cline = li-cline + 1.                                   
    END.                                                                 
  END.                                                                   
                                                                         
  ASSIGN v-printline = v-printline + 6.                                  
                                                                         
  IF v-printline < 50 THEN PAGE.                                         
                                                                         
  RELEASE est.                                                           
                                                                         
  IF v-prt-box THEN DO:                                                  
                                                                         
    FIND FIRST est                                                       
      WHERE est.company EQ xquo.company                                  
        AND est.loc     EQ xquo.loc                                      
        AND est.est-no  EQ xquo.est-no NO-LOCK NO-ERROR.                 
    IF AVAIL est THEN DO:                                                
      PAGE.                                                              
      PUT "<FCouriar New>" SKIP.                                         
      FIND FIRST xest WHERE RECID(xest) = RECID(est) NO-LOCK NO-ERROR.   
      RUN cec/desprnt2.p (?, input-output v-lines, recid(xest)).         
                                                                         
    END.
  END. 

  IF v-terms AND TRIM(v-termfile) NE "" THEN RUN get-terms.

END.
ELSE DO:

 FOR EACH report 
    WHERE report.term-id EQ v-term-id,
   FIRST xquo NO-LOCK
    WHERE recid(xquo) EQ report.rec-id
     BREAK BY report.key-01
           BY report.key-02
           BY report.key-03 
     TRANSACTION:

     FIND FIRST est
       WHERE est.company EQ xquo.company
         AND est.est-no  EQ xquo.est-no NO-LOCK NO-ERROR.

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
     IF AVAIL cust 
      THEN
       ASSIGN v-over-under = TRIM(STRING(cust.over-pct,">>9%")) + "-" +
                             TRIM(STRING(cust.under-pct,">>9%")).

     ASSIGN
       sold[5] = TRIM(STRING(xquo.sold-no))
       ship[5] = TRIM(STRING(xquo.ship-id))
       bill[5] = TRIM(STRING(xquo.cust-no)).

     DO i = 1 TO 4:
      ASSIGN
       sold[i] = xquo.soldto[i]
       ship[i] = xquo.shipto[i]
       bill[i] = xquo.billto[i].
     END.

     IF s-sep-page OR FIRST(report.key-01) 
      THEN ASSIGN lv-first-qreckey = xquo.rec_key
                  lv-first-qrecid  = RECID(xquo).

     IF s-sep-page OR FIRST-OF(report.key-01) THEN DO:      

       ASSIGN v-printline = 0.

       IF NOT FIRST(report.key-01) THEN PAGE.

       ASSIGN
          v-first-q-no = xquo.q-no
          lv-tot-pg = 1
          ln-cnt = 0.

       /* get total page number */
       FOR EACH bf-report 
          WHERE bf-report.term-id EQ v-term-id
           AND bf-report.key-01 EQ report.key-01,
         FIRST bf-quo  
          WHERE recid(bf-quo) EQ bf-report.rec-id,
           EACH xqitm OF bf-quo NO-LOCK
          BREAK BY bf-quo.q-no by xqitm.part-no: 

          ASSIGN lv-itm-ln-cnt = lv-itm-ln-cnt + 1.

          IF LAST-OF(bf-quo.q-no) THEN DO:
            ASSIGN ln-cnt = ln-cnt + lv-itm-ln-cnt * 5
                lv-itm-ln-cnt = 0.              
          END. /* IF LAST-OF(bf-quo.q-no) */
       END. /* EACH bf-report  */

       FOR EACH bf-report 
          WHERE bf-report.term-id EQ v-term-id
            AND bf-report.key-01 = report.key-01,
         FIRST bf-quo  
          WHERE recid(bf-quo) EQ bf-report.rec-id,
         EACH xqitm OF bf-quo NO-LOCK,
         EACH xqqty OF xqitm NO-LOCK,
         EACH xqchg 
          WHERE xqchg.company = xqqty.company
            AND xqchg.loc = xqqty.loc
            AND xqchg.q-no = xqqty.q-no
            AND xqchg.LINE = xqqty.LINE
            AND xqchg.qty = xqqty.qty NO-LOCK
           BREAK BY xqchg.qty
                 BY xqchg.charge:


            IF FIRST(xqchg.charge) 
              THEN ASSIGN ln-cnt = ln-cnt + 1.

            ASSIGN ln-cnt = ln-cnt + 1.
            
            IF LAST-OF(xqchg.qty) THEN LEAVE.
       END. /* EACH bf-report  */

       FOR EACH bf-report 
           WHERE bf-report.term-id EQ v-term-id
             AND bf-report.key-01 EQ report.key-01,
         FIRST bf-quo  
           WHERE RECID(bf-quo) EQ bf-report.rec-id,
          EACH xqchg OF bf-quo 
           WHERE xqchg.qty  EQ 0 
             AND xqchg.line EQ 0 NO-LOCK
            BREAK BY xqchg.charge:

            IF FIRST(xqchg.charge) THEN ln-cnt = ln-cnt + 1.
       END.

       IF s-print-comp THEN DO :      /* Print components of a set */
          FOR EACH bf-report NO-LOCK                       
            WHERE bf-report.term-id EQ v-term-id           
              AND bf-report.key-01 = report.key-01,        
           FIRST bf-quo  NO-LOCK                           
            WHERE recid(bf-quo) EQ bf-report.rec-id,       
           FIRST est NO-LOCK                               
            WHERE est.company EQ bf-quo.company            
              AND est.est-no  EQ bf-quo.est-no             
              AND est.est-type EQ 6 AND est.FORm-qty GT 1, 
           EACH ef NO-LOCK                                 
            WHERE ef.company EQ est.company                
              AND ef.est-no  EQ est.est-no,                
           EACH eb OF ef NO-LOCK BREAK BY ef.FORm-no :     
            ASSIGN ln-cnt = ln-cnt + 4.                    
          END.                                             
       END.

       IF ch-inst THEN DO:
         FOR EACH bf-report WHERE bf-report.term-id EQ v-term-id
                           AND bf-report.key-01 = report.key-01,
             first bf-quo  WHERE recid(bf-quo) EQ bf-report.rec-id,
             FIRST est WHERE est.company = bf-quo.company
                       AND est.est-no = bf-quo.est-no NO-LOCK.
             v-inst2 = "".
             {custom/notespr2.i job v-inst2 EXTENT(v-inst2) "notes.rec_key = est.rec_key and notes.note_code >= fdept and notes.note_code <= tdept " }
             ln-cnt = ln-cnt + 1.

             DO idx = 1 TO EXTENT(v-inst2):
               IF v-inst2[idx] NE '' THEN ln-cnt = ln-cnt + 1.
             END.

         END.
       END. /* IF ch-inst */



      lv-tot-pg = lv-tot-pg + TRUNC( ln-cnt / 25,0) .

      /* get total page number */
      {cec/quote/quosmkt2.i}
     END.

    ASSIGN
       v-last = last-of(report.key-01)
       v-line-total = 0.

    {cec/quote/quosmkct.i 2}  
    v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].

   IF LINE-COUNTER > PAGE-SIZE - 1 THEN DO:      
      v-printline = 0.
      page.  
      {cec/quote/quosmkt2.i}
   END.

   IF (ch-multi and (v-last OR s-sep-page)) then do:
      PUT "<FArial><R58><C1><P12><B> Comments </B> <P9> " .

     FIND bf-quo WHERE RECID(bf-quo) = lv-first-qrecid NO-LOCK NO-ERROR. 
     li-cline = 0.
     do i = 1 to 5:      
        if bf-quo.comment[i] ne "" then DO:
            li-cline = li-cline + 1.
            put "<C1><R" string(58 + li-cline,">9") + "><C6>" bf-quo.comment[i]  .            
        END.
            
     end.
     v-printline = v-printline + 6.
     IF v-printline < 50 THEN DO:
        page.
     END.

     if v-prt-box then DO:
        FIND FIRST est WHERE est.company EQ xquo.company
                   and est.loc     EQ xquo.loc
                   and est.est-no  EQ xquo.est-no
                   NO-LOCK NO-ERROR.
     if avail est then do:
       
       PAGE.
       PUT "<FCouriar New>" SKIP.
       FIND FIRST xest WHERE RECID(xest) = RECID(est) NO-LOCK NO-ERROR.
       run cec/desprnt2.p (?,
                       input-output v-lines,
                       recid(xest)).
     end.
     END.
   END.  /*ch-multi and v-last */
   
  end. /* FOR each report */

  IF v-terms AND TRIM(v-termfile) NE "" THEN RUN get-terms.
end.  /* multi */



PROCEDURE get-terms:

  DEF VAR v-char    AS CHAR NO-UNDO.
  DEF VAR v-linecnt AS INT  NO-UNDO.
    
  PAGE.

  INPUT FROM VALUE(TRIM(v-termfile)).
  REPEAT:
   IMPORT UNFORMATTED v-char. 

   IF TRIM(v-char) NE "" AND LENGTH(TRIM(v-char)) GT 145 THEN DO:
     FOR EACH tt-formtext: DELETE tt-formtext. END.

     DO v-licnt = 1 TO 10:

       CREATE tt-formtext.
       ASSIGN tt-line-no = v-licnt
              tt-length  = 145. 
     END.

     RUN custom/formtext.p (v-char).

     ASSIGN  i = 0 v-text1 = "" v-linecnt = 0.

     FOR EACH tt-formtext:
       ASSIGN i = i + 1.

       IF i <= 5 
        THEN ASSIGN v-text1[i] = tt-formtext.tt-text.

       IF v-text1[i] <> "" THEN v-linecnt = i.
     END.

     DO i = 1 TO v-linecnt:                   
      IF v-text1[i] NE "" 
        THEN  PUT  "<FTimes New (W1)><#11><P8>" 
                    v-text1[i] FORMAT "x(145)" SKIP.
     END.
   END.
   ELSE PUT "<FTimes New (W1)><#11><P8>"  v-char FORMAT "x(145)" SKIP. 
  
  END.
  INPUT CLOSE.
END.

PROCEDURE printHeader:
  DEFINE INPUT PARAMETER ipPageOffSet AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opInitVar AS INTEGER NO-UNDO.

  IF LINE-COUNTER > PAGE-SIZE - ipPageOffSet THEN DO:
    PAGE.
    {cec/quote/quosmkt2.i}
    opInitVar = 0.
  END.
END PROCEDURE.
/* end ---------------------------------- copr. 2000  advanced software, inc. */
