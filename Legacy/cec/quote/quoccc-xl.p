&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : oe/rep/cocccc.p
    Purpose     : CCC BOL Cert

    Syntax      :

    Description :

    Author(s)   :  BV
    Created     :  02/24/2014
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{sys/inc/var.i shared}


{est/printquo.i}

DEFINE SHARED VARIABLE LvOutputSelection    AS CHAR NO-UNDO.

DEFINE VARIABLE gcCurActivePrinter AS CHAR NO-UNDO.
/* DEFINE VARIABLE gcAdobePrinter AS CHAR NO-UNDO. */
DEFINE VARIABLE gchExcelApplication AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE gchWshNetwork AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE gchWorkBook  AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE gchWorksheet AS COM-HANDLE   NO-UNDO.

DEFINE VARIABLE gcTempDir AS CHARACTER FORMAT "X(80)" NO-UNDO.
DEFINE VARIABLE gcFile AS CHARACTER NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE tt-filelist
    FIELD tt-FileCtr         AS INT
    FIELD tt-FileName        AS CHAR
    INDEX filelist           IS PRIMARY TT-FILECTR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

IF AVAIL users AND users.user_program[2] NE "" THEN
   gcTempDir = users.user_program[2] + "\".
ELSE
   gcTempDir = "c:\tmp\".

RUN InitializeExcel.
RUN MainLoop.
RUN Cleanup.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AddBorders) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddBorders Procedure 
PROCEDURE AddBorders :
/*------------------------------------------------------------------------------
  Purpose:  Creates borders around range of Excel cells   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcRange AS CHARACTER NO-UNDO.

ASSIGN
    gchWorksheet:Range(ipcRange):Borders(5):LineStyle = -4142
    gchWorksheet:Range(ipcRange):Borders(6):LineStyle = -4142
    gchWorksheet:Range(ipcRange):Borders(7):LineStyle = 1
    gchWorksheet:Range(ipcRange):Borders(7):ColorIndex = 0
    gchWorksheet:Range(ipcRange):Borders(7):TintAndShade = 0
    gchWorksheet:Range(ipcRange):Borders(7):Weight = 2
    gchWorksheet:Range(ipcRange):Borders(8):LineStyle = 1
    gchWorksheet:Range(ipcRange):Borders(8):ColorIndex = 0
    gchWorksheet:Range(ipcRange):Borders(8):TintAndShade = 0
    gchWorksheet:Range(ipcRange):Borders(8):Weight = 2
    gchWorksheet:Range(ipcRange):Borders(9):LineStyle = 1
    gchWorksheet:Range(ipcRange):Borders(9):ColorIndex = 0
    gchWorksheet:Range(ipcRange):Borders(9):TintAndShade = 0
    gchWorksheet:Range(ipcRange):Borders(9):Weight = 2
    gchWorksheet:Range(ipcRange):Borders(10):LineStyle = 1
    gchWorksheet:Range(ipcRange):Borders(10):ColorIndex = 0
    gchWorksheet:Range(ipcRange):Borders(10):TintAndShade = 0
    gchWorksheet:Range(ipcRange):Borders(10):Weight = 2
    gchWorksheet:Range(ipcRange):Borders(11):LineStyle = 1
    gchWorksheet:Range(ipcRange):Borders(11):ColorIndex = 0
    gchWorksheet:Range(ipcRange):Borders(11):TintAndShade = 0
    gchWorksheet:Range(ipcRange):Borders(11):Weight = 2
    gchWorksheet:Range(ipcRange):Borders(12):LineStyle = 1
    gchWorksheet:Range(ipcRange):Borders(12):ColorIndex = 0
    gchWorksheet:Range(ipcRange):Borders(12):TintAndShade = 0
    gchWorksheet:Range(ipcRange):Borders(12):Weight = 2
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CleanUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CleanUp Procedure 
PROCEDURE CleanUp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* RELEASE OBJECTS */
  RELEASE OBJECT gchWorkbook         NO-ERROR.
  RELEASE OBJECT gchWorkSheet        NO-ERROR.

  /* Reset the Active Printer to the Original Printer. */
  if gcCurActivePrinter <> '' then
    gchWshNetwork:SetDefaultPrinter(gcCurActivePrinter).

  /* For E-mail and Printer jobs, close Excel. */
  IF LvOutputSelection = "PRINTER" OR 
     LvOutputSelection = "EMAIL" THEN
    gchExcelApplication:Quit() no-error.
  
  /* Release created objects. */
  RELEASE OBJECT gchWshNetwork         NO-ERROR.
  RELEASE OBJECT gchExcelApplication NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillData Procedure 
PROCEDURE FillData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE BUFFER bf-quotehd FOR quotehd.
DEFINE BUFFER bf-quoteitm FOR quoteitm.
DEFINE BUFFER bf-quoteqty FOR quoteqty.
DEFINE BUFFER bf-quotechg FOR quotechg.

DEFINE VARIABLE iLine  AS INTEGER     NO-UNDO.
DEFINE VARIABLE dUnitQty AS DECIMAL NO-UNDO.
DEFINE VARIABLE cFreight AS CHARACTER   NO-UNDO.

FIND FIRST report WHERE report.term-id EQ v-term-id NO-LOCK NO-ERROR.
FIND FIRST bf-quotehd  WHERE RECID(bf-quotehd) EQ report.rec-id NO-LOCK NO-ERROR.
IF NOT AVAIL bf-quotehd THEN RETURN.
FIND FIRST bf-quoteitm OF bf-quotehd NO-LOCK NO-ERROR.
                                
FIND FIRST est 
    WHERE est.company = bf-quotehd.company
      AND est.est-no EQ bf-quotehd.est-no 
    NO-LOCK NO-ERROR.
IF AVAIL est AND AVAIL bf-quoteitm THEN
    FIND FIRST eb 
        WHERE eb.company EQ est.company
          AND eb.est-no  EQ est.est-no
          AND eb.part-no EQ bf-quoteitm.part-no
          AND eb.form-no NE 0
        NO-LOCK NO-ERROR.
IF NOT AVAIL eb AND AVAIL bf-quoteitm AND bf-quoteitm.est-no <> "" THEN
    FIND FIRST eb
        WHERE eb.company EQ est.company
          AND eb.est-no  EQ est.est-no
          AND eb.form-no NE 0
        NO-LOCK NO-ERROR.
IF AVAIL eb THEN DO:
    FIND FIRST ef
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ eb.form-no
        NO-LOCK NO-ERROR.
    FIND FIRST fgcat
        WHERE fgcat.company EQ eb.company
          AND fgcat.procat EQ eb.procat
        NO-LOCK NO-ERROR.
    FIND FIRST prodl
        WHERE prodl.company EQ eb.company
          AND prodl.procat EQ eb.procat
        NO-LOCK NO-ERROR.
    FIND FIRST style
        WHERE style.company EQ eb.company
          AND style.style EQ eb.style
        NO-LOCK NO-ERROR.
    IF AVAIL ef THEN
        FIND FIRST ITEM
            WHERE ITEM.company EQ ef.company
              AND ITEM.i-no EQ ef.board
        NO-LOCK NO-ERROR.
END.
FIND FIRST sman
    WHERE sman.company EQ bf-quotehd.company
      AND sman.sman    EQ bf-quotehd.sman
      NO-LOCK NO-ERROR.

FIND FIRST carrier
    WHERE carrier.company EQ bf-quotehd.company
      AND carrier.carrier EQ bf-quotehd.carrier
    NO-LOCK NO-ERROR.
    
FIND FIRST terms
    WHERE terms.company EQ bf-quotehd.company
      AND terms.t-code  EQ bf-quotehd.terms
    NO-LOCK NO-ERROR.
    
FIND FIRST cust
    WHERE cust.company EQ bf-quotehd.company
      AND cust.cust-no EQ bf-quotehd.cust-no
    NO-LOCK NO-ERROR.

FIND FIRST shipto
    WHERE shipto.company EQ bf-quotehd.company
      AND shipto.cust-no EQ bf-quotehd.cust-no
      AND shipto.ship-id EQ bf-quotehd.ship-id
    NO-LOCK NO-ERROR.

FIND FIRST soldto
    WHERE soldto.company EQ bf-quotehd.company
      AND soldto.cust-no EQ bf-quotehd.cust-no
      AND soldto.sold-id EQ bf-quotehd.sold-id
    NO-LOCK NO-ERROR.
IF NOT AVAIL soldto THEN
    FIND FIRST soldto
        WHERE soldto.company EQ bf-quotehd.company
          AND soldto.cust-no EQ bf-quotehd.cust-no
          AND soldto.sold-id EQ bf-quotehd.cust-no
        NO-LOCK NO-ERROR.
CASE eb.chg-method:
    WHEN "P" THEN
        cFreight = "Pre-paid".
    WHEN "C" THEN
        cFreight = "Collect".
    WHEN "B" THEN
        cFreight = "Bill".
    WHEN "T" THEN
        cFreight = "Third Party".
END CASE.


/*Always overwriting Data sheet*/
gchWorkSheet = gchExcelApplication:Sheets:item(2).

/*write data to excel Data sheet*/
ASSIGN
    gchWorkSheet:Range("B1"):VALUE = STRING(bf-quotehd.q-no)
    gchWorkSheet:Range("B2"):VALUE = STRING(bf-quotehd.est-no)
    gchWorkSheet:Range("B3"):VALUE = IF AVAIL terms THEN terms.dscr ELSE ""
    gchWorkSheet:Range("B4"):VALUE = IF AVAIL cust THEN cust.fob-code ELSE ""
    gchWorkSheet:Range("B5"):VALUE = bf-quotehd.quo-date
    gchWorkSheet:Range("B6"):VALUE = IF AVAIL shipto THEN shipto.ship-name ELSE bf-quotehd.shipto[1]
    gchWorkSheet:Range("B7"):VALUE = IF AVAIL shipto THEN shipto.ship-addr[1] ELSE bf-quotehd.shipto[2]
    gchWorkSheet:Range("B8"):VALUE = IF AVAIL shipto THEN shipto.ship-addr[2] ELSE bf-quotehd.shipto[3]
    gchWorkSheet:Range("B9"):VALUE = IF AVAIL shipto THEN shipto.ship-city ELSE bf-quotehd.shipto[4]
    gchWorkSheet:Range("B10"):VALUE = IF AVAIL shipto THEN shipto.ship-state ELSE ""
    gchWorkSheet:Range("B11"):VALUE = IF AVAIL shipto THEN shipto.ship-zip ELSE ""
    gchWorkSheet:Range("B12"):VALUE = IF AVAIL soldto THEN soldto.sold-name ELSE bf-quotehd.soldto[1]
    gchWorkSheet:Range("B13"):VALUE = IF AVAIL soldto THEN soldto.sold-addr[1] ELSE bf-quotehd.soldto[2]
    gchWorkSheet:Range("B14"):VALUE = IF AVAIL soldto THEN soldto.sold-addr[2] ELSE bf-quotehd.soldto[3]
    gchWorkSheet:Range("B15"):VALUE = IF AVAIL soldto THEN soldto.sold-city ELSE bf-quotehd.soldto[4]
    gchWorkSheet:Range("B16"):VALUE = IF AVAIL soldto THEN soldto.sold-state ELSE ""
    gchWorkSheet:Range("B17"):VALUE = IF AVAIL soldto THEN soldto.sold-zip ELSE ""
    gchWorkSheet:Range("B18"):VALUE = IF AVAIL eb THEN eb.part-no ELSE ""
    gchWorkSheet:Range("B19"):VALUE = IF AVAIL eb THEN eb.part-dscr1 ELSE ""
    gchWorkSheet:Range("B20"):VALUE = IF AVAIL fgcat THEN fgcat.dscr ELSE ""
    gchWorkSheet:Range("B21"):VALUE = IF AVAIL ITEM THEN item.i-name ELSE ""
    gchWorkSheet:Range("B22"):VALUE = IF AVAIL style THEN style.dscr ELSE ""
    gchWorkSheet:Range("B23"):VALUE = IF AVAIL eb THEN STRING(eb.len) ELSE ""
    gchWorkSheet:Range("B24"):VALUE = IF AVAIL eb THEN STRING(eb.wid) ELSE ""
    gchWorkSheet:Range("B25"):VALUE = IF AVAIL eb THEN STRING(eb.dep) ELSE ""
    gchWorkSheet:Range("B26"):VALUE = IF AVAIL eb THEN eb.i-coldscr ELSE ""
    gchWorkSheet:Range("B27"):VALUE = cFreight
    gchWorkSheet:Range("B28"):VALUE = IF AVAIL sman THEN sman.sname ELSE ""
    gchWorkSheet:Range("B29"):VALUE = bf-quotehd.contact
    gchWorkSheet:Range("B30"):VALUE = bf-quotehd.comment
    gchWorkSheet:Range("B31"):VALUE = IF AVAIL prodl THEN prodl.prolin ELSE ""
    gchWorkSheet:Range("B32"):VALUE = IF AVAIL eb THEN STRING(eb.t-len) ELSE ""
    gchWorkSheet:Range("B33"):VALUE = IF AVAIL eb THEN STRING(eb.t-wid) ELSE ""
    .
                                                                    
iLine = 0.                                          
FOR EACH bf-quoteqty OF bf-quoteitm NO-LOCK:
    iLine = iLine + 1.
    RUN sys/ref/convquom.p("EA",
                       bf-quoteqty.uom ,
                       0,
                       IF AVAIL eb THEN eb.len ELSE 0,
                       IF AVAIL eb THEN eb.wid ELSE 0,
                       IF AVAIL eb THEN eb.dep ELSE 0,
                       bf-quoteqty.qty,
                       OUTPUT dUnitQty).
    ASSIGN 
        gchWorkSheet:Range("F" + STRING(iLine + 1)):VALUE = STRING(dUnitQty)
        gchWorkSheet:Range("G" + STRING(iLine + 1)):VALUE = bf-quoteqty.uom
        gchWorkSheet:Range("H" + STRING(iLine + 1)):VALUE = STRING(bf-quoteqty.price)
        .
    IF iLine GT 1 THEN DO:
            gchWorkSheet = gchExcelApplication:Sheets:item(1).
            RUN AddBorders(INPUT "D" + STRING(15 + iLine) + ":H" + STRING(15 + iLine)).
            gchWorkSheet = gchExcelApplication:Sheets:item(2).                               
        END.

END.
iLine = 0.                                          
FIND FIRST bf-quoteqty OF bf-quoteitm NO-LOCK NO-ERROR.
IF AVAIL bf-quoteqty THEN
    FOR EACH bf-quotechg OF bf-quotehd WHERE bf-quotechg.qty EQ bf-quoteqty.qty NO-LOCK:
        iLine = iLine + 1.
        FIND FIRST prep 
            WHERE prep.company EQ bf-quotechg.company
              AND prep.CODE EQ bf-quotechg.charge
            NO-LOCK NO-ERROR.

        ASSIGN 
            gchWorkSheet:Range("J" + STRING(iLine + 1)):VALUE = bf-quotechg.charge
            gchWorkSheet:Range("K" + STRING(iLine + 1)):VALUE = IF AVAIL prep THEN prep.dscr ELSE ""
            gchWorkSheet:Range("L" + STRING(iLine + 1)):VALUE = STRING(bf-quotechg.prep-qty)
            gchWorkSheet:Range("M" + STRING(iLine + 1)):VALUE = "EA"
            gchWorkSheet:Range("N" + STRING(iLine + 1)):VALUE = STRING(bf-quotechg.cost)
            .
        IF iLine GT 1 THEN DO:
            gchWorkSheet = gchExcelApplication:Sheets:item(1).
            RUN AddBorders(INPUT "B" + STRING(39 + iLine) + ":H" + STRING(39 + iLine)).
            gchWorkSheet = gchExcelApplication:Sheets:item(2).                               
        END.
    END.


/*Copy & PasteSpecial Values for a copy of the formula template sheet*/
/*         gchWorkbook:WorkSheets(iWorksheetCount + 2):UsedRange:COPY NO-ERROR.                                     */
/*         gchWorkbook:WorkSheets(iWorksheetCount + 2):UsedRange:PasteSpecial(-4163, -4142, False, False) NO-ERROR. */
gchWorkbook:WorkSheets(1):UsedRange:COPY NO-ERROR.
gchWorkbook:WorkSheets(1):UsedRange:PasteSpecial(-4163, -4142, False, False) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitializeExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeExcel Procedure 
PROCEDURE InitializeExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cTemplateFile AS CHARACTER NO-UNDO.


/* Capture the current active printer. */
IF LvOutputSelection = "email" THEN
    ASSIGN 
        gcCurActivePrinter = SESSION:PRINTER-NAME
/*         gcAdobePrinter     = "PDFcamp Printer" */
    .
  
cTemplateFile = "template\QuoteCCC.xlt".

/* Connect to the running Excel session. */
CREATE "Excel.Application" gchExcelApplication CONNECT NO-ERROR.

/* If Excel is running close it. */
IF NOT VALID-HANDLE (gchExcelApplication) THEN
    /* Start a new session of Excel. */
    CREATE "Excel.Application" gchExcelApplication NO-ERROR.

/* Network connection checks. */
CREATE "WScript.Network" gchWshNetwork NO-ERROR.
IF NOT(VALID-HANDLE(gchWshNetwork)) THEN
    DO:
        MESSAGE "Unable to Create Wscript.Network" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
  
/* /* Switch Printer to PDFCamp Printer. */             */
/* IF LvOutputSelection = "Email" THEN                  */
/*     gchWshNetwork:SetDefaultPrinter(gcAdobePrinter). */
 
/* Check if Excel got initialized. */
IF NOT (VALID-HANDLE (gchExcelApplication)) THEN
    DO:
        MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.

FILE-INFO:FILE-NAME = cTemplateFile.

/* Set the Excel Template to be used. */
ASSIGN gcFile = search (FILE-INFO:FULL-PATHNAME) no-error.
  
IF SEARCH (gcFile) = ? THEN DO:
    MESSAGE 'Template File: ' FILE-INFO:FULL-PATHNAME
        'cannot be found. Please verify that the file exists.'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* Make Excel visible. */
ASSIGN
    gcFile = FILE-INFO:FULL-PATHNAME
    gchExcelApplication:VISIBLE = 
        IF LvOutputSelection = "Email" OR LvOutputSelection = "Printer" THEN  
            FALSE
        ELSE 
            TRUE.

/* Clear ttFileList. */
EMPTY TEMP-TABLE tt-filelist.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MainLoop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MainLoop Procedure 
PROCEDURE MainLoop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* Open our Excel Template. */  
  ASSIGN gchWorkbook = gchExcelApplication:Workbooks:OPEN(gcFile)  NO-ERROR.
  
  /* Do not display Excel error messages. */
  gchExcelApplication:DisplayAlerts = FALSE  NO-ERROR.

  /* Disable screen updating so it will go faster */
  gchExcelApplication:ScreenUpdating = FALSE.

  /* Go to the Active Sheet. */
  gchWorkbook:WorkSheets(1):Activate NO-ERROR.
  gchWorkSheet = gchExcelApplication:Sheets:ITEM(1).

  /*Fill in Data*/
  RUN FillData.
  RUN OutputFiles.
  /* enable screen updating */
  gchExcelApplication:ScreenUpdating = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OutputFiles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OutputFiles Procedure 
PROCEDURE OutputFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iFileCtr AS INTEGER NO-UNDO.

/* gchWorkbook:WorkSheets(1):DELETE NO-ERROR. /*delete template sheet with formulas*/ */
gchWorkbook:WorkSheets(2):DELETE NO-ERROR. /*delete Data Sheet*/
gchWorkbook:WorkSheets(1):Activate NO-ERROR.

OS-DELETE VALUE(gcTempDir + "cofc.xls").     
OS-DELETE VALUE(gcTempDir + "asi.pdf").
OS-DELETE VALUE(gcTempDir + "cofc.pdf").

IF LvOutputSelection = "PRINTER" THEN DO:
   NO-RETURN-VALUE gchWorkbook:PrintOut(,,,,,FALSE,).
    gchWorkbook:CLOSE(NO) NO-ERROR.
END.
ELSE IF LvOutputSelection = "Email" THEN DO:
/*     gchWshNetwork:SetDefaultPrinter(gcAdobePrinter). */
    gchExcelApplication:ActiveSheet:SaveAs(gcTempDir + "cccquote.xls") /*NO-ERROR*/.

    NO-RETURN-VALUE gchWorkbook:ExportAsFixedFormat(0, gcTempDir + "quote.pdf").
/*     NO-RETURN-VALUE gchWorkbook:PrintOut(,,,,,TRUE,,gcTempDir + "asi.pdf"). */
 
    gchWorkbook:CLOSE(NO) NO-ERROR.
/*     gchExcelApplication:QUIT() NO-ERROR. */
    PAUSE 3.
    OS-DELETE VALUE(gcTempDir + "cccquote.xls").
    OS-RENAME VALUE(gcTempDir + "asi.pdf") VALUE(gcTempDir + "quote.pdf").
    iFileCtr = iFileCtr + 1.
    CREATE tt-filelist.
    ASSIGN 
        tt-FileCtr  = iFileCtr
        tt-FileName = gcTempDir + "quote.pdf".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

