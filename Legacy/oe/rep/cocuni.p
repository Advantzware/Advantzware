/*----------------------------------------------- oe/rep/cocuni.p */
/* Print Unipak COC (Certificate of Compliance)                   */
/*----------------------------------------------------------------*/

{sys/inc/var.i shared}

{oe/rep/oe-lad.i}

DEFINE VARIABLE v-bol-qty LIKE oe-boll.qty NO-UNDO.
DEFINE VARIABLE v-cust-addr3 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE viWorkSheetCount AS INTEGER NO-UNDO.

DEFINE SHARED VARIABLE LvOutputSelection AS CHARACTER NO-UNDO.
DEFINE VARIABLE CurActivePrinter AS CHARACTER NO-UNDO.
DEFINE VARIABLE AdobePrinter AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcTemplateFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE chExcelApplication AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE WshNetwork AS COM-HANDLE.
DEFINE VARIABLE chFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE chWorkBook AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE chWorksheet AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE CurrDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE CommandString AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-rel-date AS DATE INITIAL 12/31/2999 NO-UNDO.
DEFINE VARIABLE v-manuf-date AS DATE INITIAL 12/31/2999 NO-UNDO.
DEFINE VARIABLE v-type AS CHARACTER NO-UNDO.
DEFINE VARIABLE mypict AS COM-HANDLE.
DEFINE VARIABLE LvCtr AS INTEGER NO-UNDO.
DEFINE VARIABLE v-dir AS CHARACTER FORMAT "X(80)" NO-UNDO.
DEFINE VARIABLE v-total-cases LIKE oe-boll.cases NO-UNDO.
DEFINE VARIABLE v-dim AS CHARACTER NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE tt-filelist
       FIELD tt-FileCtr         AS INTEGER
       FIELD tt-FileName        AS CHARACTER
       INDEX filelist           IS PRIMARY 
             TT-FILECTR.

FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

IF AVAILABLE users AND users.user_program[2] NE "" THEN
   v-dir = users.user_program[2] + "\".
ELSE
   v-dir = "c:\tmp\".

RUN InitializeExcel.
RUN MainLoop.
RUN Cleanup.

PROCEDURE FillData:

{sa/sa-sls01.i}
v-total-cases = 0.
FOR EACH report NO-LOCK WHERE report.term-id EQ v-term-id,
    FIRST oe-bolh WHERE RECID(oe-bolh) EQ report.rec-id :

  FOR EACH oe-boll NO-LOCK
      WHERE oe-boll.company EQ cocode
        AND oe-boll.b-no    EQ oe-bolh.b-no :
    
    CREATE xreport.
    ASSIGN
     xreport.term-id = v-term-id
     xreport.key-01  = report.key-01
     xreport.key-02  = report.key-02
     xreport.key-03  = report.key-03
     xreport.key-04  = report.key-04
     xreport.key-06  = oe-boll.i-no
     xreport.rec-id  = RECID(oe-boll).
  END.

  DELETE report.
END.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id NO-LOCK,
    FIRST oe-bolh WHERE oe-bolh.b-no   EQ oe-boll.b-no NO-LOCK,
    FIRST cust    WHERE cust.cust-no   EQ oe-bolh.cust-no NO-LOCK

    BREAK BY report.key-01
          BY report.key-02
          BY report.key-03
          BY report.key-04
          BY report.key-06:

  IF LAST-OF(report.key-06) THEN
  DO:
     viWorkSheetCount = viWorkSheetCount + 1.
     IF viWorkSheetCount GT 1 THEN
        chWorkbook:WorkSheets(1):COPY(chExcelApplication:Sheets:ITEM(1)) NO-ERROR.
  END.
END.

ASSIGN
   viWorkSheetCount = 0
   FILE-INFO:FILE-NAME = "signature\" + USERID("nosweat") + ".jpg".

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id NO-LOCK,
    FIRST itemfg
    WHERE itemfg.company EQ cocode
      AND itemfg.i-no    EQ oe-boll.i-no
    NO-LOCK,
    FIRST oe-bolh WHERE oe-bolh.b-no   EQ oe-boll.b-no NO-LOCK,
    FIRST cust    WHERE cust.cust-no   EQ oe-bolh.cust-no NO-LOCK

    BREAK BY report.key-01
          BY report.key-02
          BY report.key-03
          BY report.key-04
          BY report.key-06 DESC:

  v-bol-qty = v-bol-qty + oe-boll.qty.
  v-total-cases = v-total-cases + oe-boll.cases.

  IF LAST-OF(report.key-06) THEN DO:
    
    viWorkSheetCount = viWorkSheetCount + 1.

    /* Go to the Active Sheet. */
    chWorkbook:WorkSheets(viWorkSheetCount):Activate NO-ERROR.

    FIND FIRST oe-ordl
        WHERE oe-ordl.company EQ cocode
          AND oe-ordl.ord-no  EQ oe-boll.ord-no
          AND oe-ordl.i-no    EQ oe-boll.i-no
          AND oe-ordl.line    EQ oe-boll.line
        NO-LOCK NO-ERROR.

    ASSIGN
      chWorkSheet = chExcelApplication:Sheets:ITEM(viWorkSheetCount)
      chWorkSheet:name = STRING(oe-bolh.bol-no) + "-" +
                         report.key-06
      v-cust-addr3 = cust.city + ", " +
                     cust.state + "  " +
                     cust.zip. 

    IF TRIM(v-cust-addr3) EQ "," THEN v-cust-addr3 = "".

    FOR EACH fg-rctd NO-LOCK WHERE
        fg-rctd.company EQ cocode AND
        fg-rctd.i-no EQ oe-ordl.i-no AND
        fg-rctd.job-no EQ oe-ordl.job-no AND
        fg-rctd.job-no2 EQ oe-ordl.job-no2
        USE-INDEX i-no
        BY fg-rctd.rct-date:

        v-manuf-date = fg-rctd.rct-date.
        LEAVE.
    END.

    IF v-manuf-date EQ 12/31/2999 THEN
       FOR EACH fg-rctd NO-LOCK WHERE
           fg-rctd.company EQ cocode AND
           fg-rctd.i-no EQ oe-ordl.i-no AND
           fg-rctd.po-no EQ STRING(oe-ordl.po-no-po)
           USE-INDEX i-no
           BY fg-rctd.rct-date:
           v-manuf-date = fg-rctd.rct-date.
           LEAVE.
       END.

    ASSIGN
      chWorkSheet:Range("k58"):VALUE = IF v-manuf-date NE 12/31/2999 THEN
                                          STRING(v-manuf-date) ELSE ""
      chWorkSheet:Range("AH6"):VALUE = TODAY
      chWorkSheet:Range("J16"):VALUE = cust.NAME
      chWorkSheet:Range("J17"):VALUE = cust.addr[1]
      chWorkSheet:Range("J18"):VALUE = cust.addr[2]
      chWorkSheet:Range("J19"):VALUE = v-cust-addr3
      chWorkSheet:Range("K25"):VALUE = oe-boll.po-no
      chWorkSheet:Range("K56"):VALUE = v-bol-qty. 

    IF AVAILABLE oe-ordl THEN
    DO:
       ASSIGN
         chWorkSheet:Range("K27"):VALUE = "'" + oe-ordl.part-no
         chWorkSheet:Range("K50"):VALUE = oe-ordl.ord-no.

       FIND FIRST eb NO-LOCK WHERE
            eb.company EQ cocode AND
            eb.est-no EQ oe-ordl.est-no AND
            eb.form-no EQ oe-ordl.form-no AND
            eb.blank-no EQ oe-ordl.blank-no
            NO-ERROR.

       IF AVAILABLE eb AND eb.part-dscr1 NE "" THEN
          chWorkSheet:Range("K29"):VALUE = eb.part-dscr1.
       ELSE
          chWorkSheet:Range("K29"):VALUE = oe-ordl.i-name.

       IF AVAILABLE eb AND eb.part-dscr1 NE "" THEN
          chWorkSheet:Range("K31"):VALUE = ITEMfg.part-dscr1.

       IF AVAILABLE eb AND eb.part-dscr1 NE "" THEN
          chWorkSheet:Range("K33"):VALUE = itemfg.part-dscr2.

       IF AVAILABLE eb AND eb.part-dscr1 NE "" THEN
          chWorkSheet:Range("K35"):VALUE = itemfg.part-dscr3.

       IF AVAILABLE eb AND eb.cad-no NE "" THEN
          chWorkSheet:Range("K37"):VALUE = eb.cad-no.
       ELSE
          chWorkSheet:Range("K37"):VALUE = itemfg.cad-no.
       
       IF AVAILABLE eb AND eb.die-no NE "" THEN
           chWorkSheet:Range("K42"):VALUE = eb.die-no.
       ELSE
           chWorkSheet:Range("K42"):VALUE = itemfg.die-no.
       
       v-dim = STRING(itemfg.l-score[50]) + "X" + STRING(itemfg.w-score[50]) + "X" + STRING(itemfg.d-score[50]).  
       chWorkSheet:Range("K44"):VALUE = v-dim.
       
       IF AVAILABLE eb THEN chWorkSheet:Range("K46"):VALUE = eb.i-coldscr. 
       
       FIND FIRST ef WHERE
            ef.company EQ cocode AND
            ef.est-no EQ oe-ordl.est-no AND
            ef.form-no EQ oe-ordl.form-no
            NO-LOCK NO-ERROR.
       IF AVAILABLE ef THEN chWorkSheet:Range("K48"):VALUE = ef.brd-dscr.
       
       chWorkSheet:Range("K54"):VALUE = v-total-cases.
       
       FOR EACH oe-rel NO-LOCK
           WHERE oe-rel.company   EQ oe-ordl.company
             AND oe-rel.ord-no    EQ oe-ordl.ord-no
             AND oe-rel.i-no      EQ oe-ordl.i-no
             AND oe-rel.line      EQ oe-ordl.line:
      
           RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).

           IF INDEX("A,B,P",v-type) > 0 THEN
           DO:
              IF oe-rel.rel-date LT v-rel-date THEN
                 v-rel-date = oe-rel.rel-date.
           END.
       END.

       IF v-rel-date NE 12/31/2999 THEN
          chWorkSheet:Range("K60"):VALUE =  v-rel-date.
    END.

    IF SEARCH(FILE-INFO:FULL-PATHNAME) NE ? THEN DO:
       mypict = chExcelApplication:Range("B61"):PARENT:Pictures:INSERT(FILE-INFO:FULL-PATHNAME).
       mypict:TOP = chExcelApplication:Range("B61"):TOP.
       mypict:LEFT = chExcelApplication:Range("B61"):LEFT.
       RELEASE OBJECT mypict.
    END.

    ASSIGN
       v-bol-qty = 0
       v-total-cases = 0
       v-rel-date = 12/31/2999
       v-manuf-date = 12/31/2999
       chExcelApplication:activeSheet:PageSetup:PrintArea = "$A$AM:$AM$62" .
  end.
end. /* for each report */   


chWorkbook:WorkSheets(1):Activate NO-ERROR.

OS-DELETE VALUE(v-dir + "cofc.xls").     
OS-DELETE VALUE(v-dir + "asi.pdf").
OS-DELETE VALUE(v-dir + "cofc.pdf").

IF LvOutputSelection = "PRINTER" THEN
DO:
   NO-RETURN-VALUE chWorkbook:PrintOut(,,,,,FALSE,).
   chWorkbook:CLOSE(NO) NO-ERROR.
END.
ELSE IF LvOutputSelection = "Email" THEN
DO:
   /*WshNetwork:SetDefaultPrinter(AdobePrinter).*/
   chExcelApplication:ActiveSheet:SaveAs(v-dir + "cofc.xls") NO-ERROR. 	   
   NO-RETURN-VALUE chWorkbook:PrintOut(,,,,,FALSE,). 
   chWorkbook:CLOSE(NO) NO-ERROR.   
   chExcelApplication:QUIT() NO-ERROR.
   PAUSE 3.
   OS-DELETE VALUE(v-dir + "cofc.xls").
   OS-RENAME VALUE(v-dir + "asi.pdf") VALUE(v-dir + "cofc.pdf").
   LvCtr = LvCtr + 1.
   CREATE tt-filelist.
   ASSIGN tt-FileCtr  = LvCtr
          tt-FileName = v-dir + "cofc.pdf".
END.

END PROCEDURE. /* FillData*/

PROCEDURE InitializeExcel:

   /* Capture the current active printer. */
  IF LvOutputSelection = "email" THEN
    ASSIGN 
      CurActivePrinter = SESSION:PRINTER-NAME
      AdobePrinter     = "PDFcamp Printer".
  
  vcTemplateFile   = "template\unipakcofc.xlt".

  /* Connect to the running Excel session. */
  CREATE "Excel.Application" chExcelApplication CONNECT NO-ERROR.

  /* If Excel is running close it. */
  IF VALID-HANDLE (chExcelApplication) THEN
  DO:
    chExcelApplication:QUIT()         NO-ERROR.
    RUN CleanUp.
  END.


  /* Network connection checks. */
  CREATE "WScript.Network" WshNetwork NO-ERROR.
  IF NOT(VALID-HANDLE(WshNetwork)) THEN
  DO :
    MESSAGE "Unable to Create Wscript.Network" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
  
  /* Switch Printer to PDFCamp Printer. */
  IF LvOutputSelection = "Email" THEN
     WshNetwork:SetDefaultPrinter(AdobePrinter).

  /* Start a new session of Excel. */
  /*if not (valid-handle (chExcelApplication)) THEN*/
    CREATE "Excel.Application" chExcelApplication NO-ERROR.
  
  /* Check if Excel got initialized. */
  IF NOT (VALID-HANDLE (chExcelApplication)) THEN
  DO :
    MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
  
  FILE-INFO:FILE-NAME = vcTemplateFile.

  /* Set the Excel Template to be used. */
  ASSIGN chFile = SEARCH (FILE-INFO:FULL-PATHNAME) NO-ERROR.

  IF SEARCH (chFile) = ? THEN DO:
    MESSAGE 'Template File: ' FILE-INFO:FULL-PATHNAME
            'cannot be found. Please verify that the file exists.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY 'CLOSE':U TO THIS-PROCEDURE.
  END.

  /* Make Excel visible. */
  ASSIGN
     chFile = FILE-INFO:FULL-PATHNAME
     chExcelApplication:VISIBLE = IF LvOutputSelection = "Email" OR 
                                     LvOutputSelection = "Printer" THEN  FALSE
                                  ELSE TRUE.
  
  /* Clear tt-FileList. */
  EMPTY TEMP-TABLE tt-filelist.

END PROCEDURE.

PROCEDURE MainLoop:

   /* Open our Excel Template. */  
  ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(chfile)  NO-ERROR.
  
  /* Do not display Excel error messages. */
  chExcelApplication:DisplayAlerts = FALSE  NO-ERROR.

  /* Disable screen updating so it will go faster */
  chExcelApplication:ScreenUpdating = FALSE.

  /* Go to the Active Sheet. */
  chWorkbook:WorkSheets(1):Activate NO-ERROR.
  chWorkSheet      = chExcelApplication:Sheets:ITEM(1).

  /*Fill in Data*/
  RUN FillData.

  /* enable screen updating */
  chExcelApplication:ScreenUpdating = TRUE.
END PROCEDURE.

PROCEDURE CleanUp:

    /* RELEASE OBJECTS */
  RELEASE OBJECT chWorkbook         NO-ERROR.
  RELEASE OBJECT chWorkSheet        NO-ERROR.

  /* Reset the Active Printer to the Original Printer. */
  IF CurActivePrinter NE '' THEN
    WshNetwork:SetDefaultPrinter(CurActivePrinter).

  /* For E-mail and Printer jobs, close Excel. */
  IF LvOutputSelection = "PRINTER" OR 
     LvOutputSelection = "EMAIL" THEN
    chExcelApplication:QUIT() NO-ERROR.
  
  /* Release created objects. */
  RELEASE OBJECT WshNetwork         NO-ERROR.
  RELEASE OBJECT chExcelApplication NO-ERROR.
END PROCEDURE.


