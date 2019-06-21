
/*------------------------------------------------------------------------
    File        : CorrEstimatePrint.p
    Purpose     :  Corrugated Estimate Print

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE TEMP-TABLE ttCorrEstimateList NO-UNDO
        FIELD vCorrEstimateFile AS CHAR.    
        
    DEFINE DATASET dsCorrEstimateList FOR ttCorrEstimateList .

    DEFINE INPUT PARAMETER prmUser       AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.

    DEFINE INPUT PARAMETER vFromDept     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vToDept       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vTbPrtBox    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vTbPrtNote   AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmEstimate   AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER prmFormNo   AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER prmBlankNo   AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER prmLine      AS INT NO-UNDO.


    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrEstimateList.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser   = ?   THEN ASSIGN prmUser = "".
    IF prmAction = ?  THEN ASSIGN prmAction = "".
    IF prmOut    = ?  THEN ASSIGN prmOut = "No".
    IF vFromDept = ?  THEN ASSIGN vFromDept = "".
    IF vToDept = ?  THEN ASSIGN vToDept = "".
    IF vTbPrtBox = ?  THEN ASSIGN vTbPrtBox = "".
    IF vTbPrtNote = ?  THEN ASSIGN vTbPrtNote = "". 
    IF prmEstimate = ?  THEN ASSIGN prmEstimate = "".
    IF prmFormNo = ?  THEN ASSIGN prmFormNo = 0.
    IF prmBlankNo = ?  THEN ASSIGN prmBlankNo = 0.
    IF prmLine    = ?  THEN ASSIGN prmLine    = 0.

    DEF VAR list-name AS CHAR NO-UNDO.
    DEF VAR init-dir  AS CHAR NO-UNDO.
    DEF VAR lv-pdf-file AS cha NO-UNDO.
    DEFINE VAR vPdfFile AS CHAR NO-UNDO.  
    DEFINE VAR vTxtFile AS CHAR NO-UNDO.
    DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no. 

    def var voverall as dec form ">>,>>>,>>9.99" no-undo.
    def var vtot-msf as dec form ">>>>9.99" no-undo.
     
    {jcrep/r-ticket.i "new shared"}
    {cecrep/jobtick.i "new shared"}

    {cec/print4.i "new shared" "new shared"}
    {cec/print42.i "new shared"}

    {sys/inc/var.i "new shared"}

   /* {custom/globdefs.i}*/

DEFINE NEW SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.

    DEF BUFFER probe-ref FOR reftable.
    DEF BUFFER b-probemk FOR reftable.

    def new shared var k_frac as dec init "6.25" no-undo.
    def new shared var day_str as cha form "x(10)" no-undo.
    def new shared var tim_str as cha form "x(8)" no-undo.
    def new shared var maxpage as int form ">9" no-undo.
    def new shared var tmp-dir as cha no-undo.
    def new shared var col-norm as cha init "White/Blue" no-undo. 
    def new shared var qty as int NO-UNDO.
    DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.
    def new shared var v-do-gsa like do-gsa no-undo.
    def new shared buffer xop for est-op.


    def new shared var v-qtty like qtty no-undo.
    def new shared var v-drop-rc as log no-undo.


    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEF VAR prmComp AS CHAR NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(100)":U      NO-UNDO.
    DEFINE VAR custcount AS CHAR NO-UNDO.    

    FIND FIRST usercomp WHERE
        usercomp.user_id = prmUser AND
        usercomp.loc = '' AND
        usercomp.company_default = YES
        NO-LOCK NO-ERROR.

    prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


    FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
    END.

    
    def var v as int no-undo.
    def var vn-out like ef.n-out-l init 1 no-undo.
    def var v-outf as dec no-undo.
    def var v-on-f as dec no-undo.
    def var v-on-l as dec no-undo.
    def var sh-tmp like sh-len no-undo.
    def var v-widp as log no-undo.
    def var v-brd-only like sys-ctrl.log-fld init no no-undo.
    def var v-brd-cost as dec no-undo.
    def var v-module as char format "x(60)" no-undo.
    def new shared var v-prep-mat like tprep-mat no-undo.  /* for probemk cost */
    def new shared var v-prep-lab like tprep-lab no-undo.
    def var v-bqty as int no-undo.
    def var v-gsa as log init no no-undo.
    def var ls-outfile as cha no-undo.
    def var ls-probetime as cha no-undo.  /* time display */
    DEF VAR v-tmp-int AS INT NO-UNDO.
    
    def new shared workfile w-form
        field form-no like ef.form-no
        field min-msf as   log init no.

    DEF TEMP-TABLE w-probeit LIKE probeit
        FIELD mat-cost   LIKE probe.mat-cost
        FIELD lab-cost   LIKE probe.lab-cost
        FIELD vo-cost    LIKE probe.vo-cost
        FIELD fo-cost    LIKE probe.fo-cost
        FIELD probe-date LIKE probe.probe-date.

    def TEMP-TABLE q-sort no-undo field qty as dec field rel as int.
    def TEMP-TABLE q-sort1 no-undo field qty as dec field rel as int.
    def TEMP-TABLE q-sort2 no-undo field qty as dec field rel as int.

    def new shared temp-table tt-qtty field qtty like qtty
                                  field rel like rels.

    DEF TEMP-TABLE tt-bqty NO-UNDO FIELD tt-bqty AS INT FIELD tt-brel AS INT.

    DEF TEMP-TABLE tt-probeit LIKE probeit
                          FIELD row-id AS ROWID.
   

    DEF VAR lv-override AS LOG NO-UNDO. /* probe override or new creatation */
    DEF VAR module AS cha FORM "x(60)" NO-UNDO.
    DEF VAR lv-changed AS CHAR NO-UNDO.
    DEF VAR lv-fullc AS CHAR NO-UNDO.
    DEF VAR lv-gprof AS CHAR NO-UNDO.
    DEF VAR lv-nprof AS CHAR NO-UNDO.
    DEF VAR lv-price AS CHAR NO-UNDO.
    DEF VAR lv-brd-% AS CHAR NO-UNDO.
    DEF VAR lv-brdcm AS CHAR NO-UNDO.
    DEF VAR lv-brdc$ AS CHAR NO-UNDO.
    DEF VAR lv-comm AS CHAR NO-UNDO.
    DEF VAR hold-value AS CHAR NO-UNDO.
    DEF VAR ll-use-margin AS LOG NO-UNDO.
    DEF VAR v-prt-note AS LOG NO-UNDO.
    DEF VAR v-prt-box AS LOG NO-UNDO.
    DEF VAR v-from-dept AS cha NO-UNDO.
    DEF VAR v-to-dept AS cha NO-UNDO.
    DEF VAR ll-no-valid AS LOG NO-UNDO.
    DEF VAR lv-valid-profit AS CHAR NO-UNDO
        INIT "market-price,gross-profit,net-profit".

    DEF NEW SHARED VAR lv-cebrowse-dir AS CHAR NO-UNDO.
    DEF VAR cerunc-dec AS DEC NO-UNDO.
    DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.
    

  DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
  DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
  DEFINE VARIABLE result AS LOGICAL NO-UNDO.
  DEF VAR v-print-fmt AS cha NO-UNDO.
  DEF VAR is-xprint-form AS LOG NO-UNDO.
  
  DEF VAR v-cinput AS cha FORM "x(255)" NO-UNDO.

  DEF VAR lv-dest AS int NO-UNDO.
  DEF VAR lv-font AS INT NO-UNDO.
  DEF VAR lv-ornt AS cha NO-UNDO.
  DEF VAR lv-lines AS INT NO-UNDO.
  DEF VAR ls-fax-file AS cha NO-UNDO.
  DEF VAR ret-code AS INT NO-UNDO.
  DEF VAR ls-mail-file2 AS cha NO-UNDO.
  DEF VAR lv-dir AS CHAR NO-UNDO.
  DEF VAR v-probe-fmt AS CHAR NO-UNDO.
  DEF VAR v-VERSION AS CHAR NO-UNDO.
  DEF VAR server-map-path AS CHAR NO-UNDO.

  {est/checkuse.i}


    {custom/xprint.i}

    assign
        cocode = "001"
        locode = "Main"
        tb_excel  = IF prmOut = "Yes" THEN TRUE ELSE FALSE 
        v-today    = TODAY   
       g_company   = cocode 
       g_loc       = locode .

       
  
        FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
        IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
        
       find first sys-ctrl WHERE  sys-ctrl.company eq cocode AND
            sys-ctrl.name    eq "CEBROWSE"   no-lock no-error.

       IF sys-ctrl.char-fld NE "" THEN
           ASSIGN server-map-path = sys-ctrl.char-fld .
            server-map-path = REPLACE(server-map-path,"/","\").

        IF sys-ctrl.char-fld NE "" THEN
            tmp-dir = sys-ctrl.char-fld.
        ELSE
            tmp-dir = "users\".

           IF INDEX(tmp-dir , 'P:' ,1) > 0 THEN ASSIGN 
                tmp-dir = REPLACE(tmp-dir ,'P:',"D:").

            IF LOOKUP(SUBSTRING(tmp-dir,LENGTH(tmp-dir)),"\,/") EQ 0 THEN
                tmp-dir = tmp-dir + "\".
            ASSIGN
                tmp-dir = REPLACE(tmp-dir,"/","\").
                lv-cebrowse-dir = tmp-dir.

            FIND FIRST users WHERE
                users.user_id EQ USERID("NOSWEAT")
                NO-LOCK NO-ERROR.

            IF AVAIL users AND users.user_program[2] NE "" THEN
                v-dir = users.user_program[2] + "\".
            ELSE
                v-dir = "c:\tmp\".

                

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
FIND FIRST loc WHERE loc.loc EQ locode NO-LOCK NO-ERROR.

module = IF AVAIL company THEN company.NAME ELSE cocode.
module = module + " - " + IF AVAILABLE loc THEN loc.dscr ELSE locode.

DO TRANSACTION:
  {sys/inc/cerun.i C}
  
  ASSIGN
   do-speed  = sys-ctrl.log-fld
   vmclean   = sys-ctrl.char-fld ne ""
   vsuthrlnd = lookup(sys-ctrl.char-fld,"Suthrlnd,Clevelnd,Brick") ne 0
   cerunc-dec = sys-ctrl.dec-fld.

  {sys/inc/cerun.i F}
  {sys/inc/cewhatif.i}
  {sys/inc/ceprint.i}
END.
  {sys/inc/ceprep.i} 
        
IF prmOut = "Yes" THEN DO:

    FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST ef where ef.company = prmComp AND ef.est-no = est.est-no AND ef.form-no = prmFormNo EXCLUSIVE-LOCK NO-ERROR.
    find FIRST eb where eb.company = prmComp AND eb.est-no = ef.est-no AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankNo EXCLUSIVE-LOCK NO-ERROR.
    
    find xest where recid(xest) = recid(est) NO-LOCK NO-ERROR.
    find xef where recid(xef) = recid(ef) NO-LOCK NO-ERROR.
    find xeb where recid(xeb) = recid(eb) NO-LOCK NO-ERROR.

    FOR EACH probe where probe.company = eb.company 
        AND probe.est-no = eb.est-no AND probe.LINE = prmLine   AND probe.probe-date ne ?  NO-LOCK:
        
        find FIRST probeit WHERE probeit.company EQ probe.company 
            AND probeit.est-no  EQ probe.est-no  
            AND probeit.line    EQ probe.LINE NO-LOCK NO-ERROR. 
        
       ASSIGN
            init-dir    = v-webrootpath   
            list-name = init-dir + 'CorrEstimatePrint' +  STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99")  + '.txt' . 

        ASSIGN
            vTxtFile   = 'CorrEstimatePrint' + STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99")  + '.txt' .

        RUN printProbe (YES).
        
        CREATE ttCorrEstimateList.
        ASSIGN ttCorrEstimateList.vCorrEstimateFile = vTxtFile.

     END.
   
END.


IF prmOut = "No" THEN DO:

    FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST ef where ef.company = prmComp AND ef.est-no = est.est-no AND ef.form-no = prmFormNo EXCLUSIVE-LOCK NO-ERROR.
    find FIRST eb where eb.company = prmComp AND eb.est-no = ef.est-no AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankNo EXCLUSIVE-LOCK NO-ERROR.
    
    find xest where recid(xest) = recid(est) NO-LOCK NO-ERROR.
    find xef where recid(xef) = recid(ef) NO-LOCK NO-ERROR.
    find xeb where recid(xeb) = recid(eb) NO-LOCK NO-ERROR.

      FOR EACH probe where probe.company = eb.company 
        AND probe.est-no = eb.est-no AND probe.LINE = prmLine   AND probe.probe-date ne ?  NO-LOCK:
        
        find FIRST probeit WHERE probeit.company EQ probe.company 
            AND probeit.est-no  EQ probe.est-no  
            AND probeit.line    EQ probe.LINE NO-LOCK NO-ERROR. 
        
      
        ASSIGN
            init-dir    = v-webrootpath   
            list-name = init-dir + 'CorrEstimatePrint' +  STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99")  + '.txt' . 
       
        RUN printProbe (NO).
        
         ASSIGN
            vTxtFile   = 'CorrEstimatePrint' + STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99")  + '.txt' .

        CREATE ttCorrEstimateList.
        ASSIGN ttCorrEstimateList.vCorrEstimateFile = vTxtFile.

     END.
   
END.



/* ------------------------------------------------------------------------------ */

PROCEDURE printProbe :
    DEFINE INPUT PARAMETER ipPrompt AS LOGICAL NO-UNDO.

  IF ipPrompt THEN DO:
    ASSIGN
    v-prt-note = IF  vTbPrtNote = "Yes" THEN TRUE ELSE FALSE
    v-prt-box =  IF  vTbPrtBox  = "Yes" THEN TRUE ELSE FALSE
    v-from-dept = vFromDept
    v-to-dept = vToDept
    lv-dest = 2
    lv-font = 15
    lv-ornt = 'P'
    lv-lines = 63
    .

    /* IF v-error THEN RETURN ERROR. */
  END. /* if ipprompt */
  ELSE
  ASSIGN
    is-xprint-form = YES
    v-prt-note = YES
    v-prt-box = YES
    v-from-dept = ''
    v-to-dept = 'zzzzz'
    lv-dest = 2
    lv-font = 15
    lv-ornt = 'P'
    lv-lines = 63.

 ASSIGN
     v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999"
     ls-outfile = lv-cebrowse-dir + trim(est.est-no) + ".p" + string(probe.line,v-probe-fmt).

  FIND CURRENT xest.
  IF xest.est-type LT 3 THEN RUN ce/probeu1.p (RECID(probe)).
  IF xest.est-type LT 7 AND xest.est-type GT 3 THEN RUN cec/probeu1.p (RECID(probe)) .
  FIND CURRENT xest NO-LOCK.

  v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

  RUN get-dir-proc(INPUT trim(est.est-no) + ".s" + string(probe.line,v-probe-fmt),
                   OUTPUT lv-dir).

  if opsys eq "unix" THEN
     unix silent cp  value(lv-dir + trim(est.est-no) + ".s" + string(probe.line,v-probe-fmt))
                     value(ls-outfile).
  ELSE DO:
    find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name eq "CEPrint" no-lock no-error.
    is-xprint-form = avail sys-ctrl and sys-ctrl.char-fld ne 'Text'.
    find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name eq "JOBCARDC" no-lock no-error.
    if avail sys-ctrl THEN ASSIGN v-print-fmt = sys-ctrl.char-fld.
    ELSE v-print-fmt = "".

     IF is-xprint-form THEN lv-lines = 63.

     
     OUTPUT TO VALUE(list-name) PAGE-SIZE VALUE(lv-lines). /* create .x file with page size */
     
      OS-COPY VALUE(server-map-path + "\" +  trim(est.est-no) + ".s" + string(probe.line,v-probe-fmt)) VALUE (lv-dir).
      
     input from value(lv-dir + trim(est.est-no) + ".s" + string(probe.line,v-probe-fmt)) NO-ECHO.
     repeat:
           v-cinput = "".
           import unformatted v-cinput.
           if v-cinput eq "" then put skip(1).
           else do:
               IF LOOKUP(SUBSTRING(v-cinput,1,2),"01,02,03,04,05,06,07,08,09,10,11,12") > 0 and
                  SUBSTRING(v-cinput,3,1) EQ "/" AND
                  SUBSTRING(v-cinput,6,1) EQ "/"    THEN PAGE. /*seperate page per form*/
               put unformatted v-cinput skip.
           END.
     end.

     IF NOT is-xprint-form AND v-prt-note THEN RUN print-notes.

     input close.
     OUTPUT CLOSE.
  END.
  
 
 /* IF is-xprint-form THEN RUN print-box-est (lv-dest,lv-font,lv-ornt).
  ELSE DO:
  

  END.*/
 /* OS-DELETE VALUE("d:\webapps\calcview.csv") .*/

END PROCEDURE.


/******************************************************************************************************/


PROCEDURE get-dir-proc :

   DEF INPUT PARAMETER ip-search AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER op-tmp-dir AS CHAR NO-UNDO.

   DEF VAR viDirCount AS INT NO-UNDO.
   DEF VAR char1 AS CHAR NO-UNDO.
   
      
           ASSIGN   op-tmp-dir = lv-cebrowse-dir.
          

      IF LOOKUP(SUBSTRING(op-tmp-dir,LENGTH(op-tmp-dir)),"\,/") EQ 0 THEN
         op-tmp-dir = op-tmp-dir + "\".

      op-tmp-dir = REPLACE(op-tmp-dir,"/","\").

      char1 = SEARCH(op-tmp-dir + ip-search)  .
      

      IF viDirCount EQ 3 OR SEARCH(op-tmp-dir + ip-search) NE ? THEN
         LEAVE.
  
   
END PROCEDURE.


/******************************************************************************************************/


PROCEDURE print-notes :

  {custom/notesdef.i}
  DEF VAR v-inst2 AS cha EXTENT 200 NO-UNDO.    
  DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 200 NO-UNDO.
  DEF VAR v-note-length AS INT INIT 80 NO-UNDO.
  DEF VAR lv-k AS INT NO-UNDO.
  
  /*determine number of lines needed*/
  ASSIGN v-tmp-lines = 0
       j = 0
       K = 0
       lv-got-return = 0.

  FOR EACH notes WHERE notes.rec_key = xest.rec_key and
      v-prt-note and notes.note_code >= v-from-dept and
      notes.note_code <= v-to-dept NO-LOCK:
    
    IF v-prev-note-rec <> ? AND
       v-prev-note-rec <> RECID(notes) THEN v-prev-extent = k.

    DO i = 1 TO LENGTH(notes.note_text) :        
       IF i - j >= lv-line-chars THEN ASSIGN j = i
                                             lv-got-return = lv-got-return + 1.
              
       v-tmp-lines = ( i - j ) / lv-line-chars.
       {SYS/INC/ROUNDUP.I v-tmp-lines}
       k = v-tmp-lines + lv-got-return +
       IF (v-prev-note-rec <> RECID(notes) AND v-prev-note-rec <> ?) THEN v-prev-extent ELSE 0.
    
       IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13) THEN                
       do:
          ASSIGN
             lv-got-return = lv-got-return + 1
             j = i.
       END.
    END.
    ASSIGN v-prev-note-rec = RECID(notes)
           j = 0
           lv-got-return = 0.
  END.

  lv-k = k.

  {custom/notespr2.i job v-inst2 lv-k "notes.rec_key = xest.rec_key and v-prt-note and notes.note_code >= v-from-dept and notes.note_code <= v-to-dept" }
  
  PUT SKIP(1)
      "Department Notes: " SKIP.

  IF lv-k GT EXTENT(v-dept-inst) THEN lv-k = EXTENT(v-dept-inst).

  DO i = 1 TO lv-k:
     v-dept-inst[i] = v-inst2[i].
     PUT v-dept-inst[i] AT 2 SKIP.
  END.

END PROCEDURE.

/******************************************************************************************************/


