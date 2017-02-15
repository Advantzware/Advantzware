&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : arrep\asiexlstmt.p
    
    Purpose     : A/R Statement
     
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/* Parameters */
DEFINE INPUT PARAMETER ip-stmt-date AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ip-msg AS CHAR NO-UNDO.

/* Includes */
 {sys/inc/var.i shared}

/* Temp-Tables */
def SHARED temp-table tt-inv no-undo
  field inv-date as date
  field sort-fld as char
  field trans-date as date
  field inv-no like ar-inv.inv-no
  field type as char format 'x(4)'
  field description as char format 'x(25)'
  field amount  as dec format '->>,>>>,>>>.99'
  FIELD inv-amt LIKE ar-inv.gross
  FIELD cust-no AS CHAR
  FIELD po-no LIKE ar-invl.po-no
  FIELD bol-no AS CHAR 
  FIELD old-day AS INT
  index tt-inv cust-no inv-date sort-fld trans-date.

DEF SHARED TEMP-TABLE tt-cust-excel NO-UNDO
    FIELD cust-no AS CHAR
    FIELD contact AS CHAR
    FIELD addr    AS CHAR EXTENT 5
    FIELD aged    AS DEC EXTENT 5
    INDEX excel cust-no ASC.

/* VARIABLE FOR EXCEL OUTPUT */
DEFINE SHARED VARIABLE LvOutputSelection    AS CHAR NO-UNDO.

/* Variables for excel Automation  */
DEFINE NEW SHARED VARIABLE chExcelApplication   AS COM-HANDLE   NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorkBook           AS COM-HANDLE   NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorksheet          AS COM-HANDLE   NO-UNDO.
DEFINE NEW SHARED VARIABLE chHyper              AS COM-HANDLE   NO-UNDO.
DEFINE            VARIABLE chFile               AS CHAR         NO-UNDO.
DEFINE            VARIABLE CurActivePrinter     AS CHAR         NO-UNDO.
DEFINE            VARIABLE AdobePrinter         AS CHAR         NO-UNDO.
define            variable CommandString        AS CHAR         NO-UNDO.
define            variable WshNetwork           as com-handle.
DEFINE            VARIABLE LvFirstTimePrint     AS LOGICAL      NO-UNDO   INIT no.
DEFINE            VARIABLE CurrDir              AS CHARACTER    NO-UNDO.
DEFINE VARIABLE LvCtr as int no-undo.
DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.

/* Build a Table to keep sequence of pdf files */
DEFINE new SHARED TEMP-TABLE tt-filelist
                       FIELD tt-FileCtr         AS INT
                       FIELD tt-FileName        AS CHAR
                       INDEX filelist           IS PRIMARY 
                             TT-FILECTR.

FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

IF AVAIL users AND users.user_program[2] NE "" THEN
   v-dir = users.user_program[2] + "\".
ELSE
   v-dir = "c:\tmp\".

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
         HEIGHT             = 22.81
         WIDTH              = 60.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
run InitializeExcel.
run MainLoop.
run Cleanup.

/*ESP - only way I could figure out to reset printer back to default*/
IF LvOutputSelection = "EMAIL" THEN
DO:
   CREATE "WScript.Network" WshNetwork.
   WshNetwork:SetDefaultPrinter(CurActivePrinter).
   RELEASE OBJECT wshnetwork.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CleanUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CleanUp Procedure 
PROCEDURE CleanUp :
/*------------------------------------------------------------------------------
  Purpose:    Clean up routine.
  Parameters: <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  /* RELEASE OBJECTS */
  RELEASE OBJECT chWorkbook         NO-ERROR.
  RELEASE OBJECT chWorkSheet        NO-ERROR.
  RELEASE OBJECT chHyper            NO-ERROR.

  /* Delete pre-existing PDF File. */
  os-delete VALUE (v-dir + "TempFile.pdf").

  RUN UTIL/CurrDir.p (output CurrDir).

  /* Set the PDF Merging Utility. */
  assign CommandString = CurrDir + "\util\pdftk ".
  
  /* Add the PDF Filenames to be merged to the command string.  */
  FOR EACH tt-filelist :
    assign CommandString = CommandString + " " + tt-FileName .
  END.
  
  /* Indicate the new filename of combined PDF File. */
  assign CommandString = CommandString + " cat output " + v-dir + "stmt.pdf".
  
  /* Merge the PDF Files. */
  os-command silent value(CommandString).
  
  /* For E-mail and Printer jobs, close Excel. */
  IF LvOutputSelection = "PRINTER" OR 
     LvOutputSelection = "EMAIL" THEN
    chExcelApplication:Quit() no-error.

  /* Release created objects. */
  RELEASE OBJECT WshNetwork         NO-ERROR.
  RELEASE OBJECT chExcelApplication NO-ERROR.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DisplayData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayData Procedure 
PROCEDURE DisplayData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR row-count AS INT NO-UNDO.
   DEF VAR v-balance AS DEC NO-UNDO.
   DEF VAR viWorkSheetCount AS INT NO-UNDO.

   /*Create a worksheet for each customer*/
    FOR EACH tt-cust-excel:
        viWorkSheetCount = viWorkSheetCount + 1.
        IF viWorkSheetCount GT 1 THEN
           chWorkbook:WorkSheets(1):COPY(chExcelApplication:Sheets:item(1)) NO-ERROR.
    END.

  viWorkSheetCount = 0.

  FOR EACH tt-cust-excel:

    ASSIGN
      viWorkSheetCount = viWorkSheetCount + 1
      row-count = 12
      v-balance = 0.

    /* Go to the Active Sheet. */
    chWorkbook:WorkSheets(viWorkSheetCount):Activate no-error.
    
    ASSIGN
      chWorkSheet      = chExcelApplication:Sheets:item(viWorkSheetCount)
      chWorkSheet:name = tt-cust-excel.cust-no
      chWorkSheet:Range("A4"):value = "Attn:" + tt-cust-excel.contact
      chWorkSheet:Range("F5"):VALUE = STRING(ip-stmt-date)
      chWorkSheet:Range("G5"):VALUE = tt-cust-excel.cust-no
      chWorkSheet:Range("A6"):VALUE = tt-cust-excel.addr[1]
      chWorkSheet:Range("A7"):VALUE = tt-cust-excel.addr[2]
      chWorkSheet:Range("A8"):VALUE = tt-cust-excel.addr[3]
      chWorkSheet:Range("A9"):VALUE = tt-cust-excel.addr[4]
      chWorkSheet:Range("A10"):VALUE = tt-cust-excel.addr[5].
   
    FOR EACH tt-inv WHERE
        tt-inv.cust-no EQ tt-cust-excel.cust-no
        break BY tt-inv.cust-no
              by tt-inv.inv-date
              by tt-inv.sort-fld
              by tt-inv.trans-date:
   
        ASSIGN
        row-count = row-count + 1
        chWorkSheet:Range("A" + STRING(row-count)):value = STRING(tt-inv.trans-date,"99/99/9999")
        chWorkSheet:Range("B" + STRING(row-count)):value = tt-inv.TYPE
        chWorkSheet:Range("C" + STRING(row-count)):value = IF tt-inv.inv-no GT 0 THEN STRING(tt-inv.inv-no) ELSE ""
        chWorkSheet:Range("D" + STRING(row-count)):value = SUBSTR(tt-inv.description,1,25)
        chWorkSheet:Range("F" + STRING(row-count)):value = STRING(tt-inv.amount)
        v-balance = v-balance + tt-inv.amount
        chWorkSheet:Range("G" + STRING(row-count)):value = STRING(v-balance).
    END.
   
    ASSIGN
    row-count = row-count + 2
    chWorkSheet:Range("A" + STRING(row-count) + ":D" +
                      STRING(row-count)):MergeCells = TRUE
   
    chWorkSheet:Range("A" + STRING(row-count)):FONT:NAME = "Arial"
    chWorkSheet:Range("A" + STRING(row-count)):FONT:SIZE = 10
    chWorkSheet:Range("A" + STRING(row-count)):value = ip-msg
    chWorkSheet:Range("G" + STRING(row-count)):value = STRING(v-balance)
    row-count = row-count + 2
    chWorkSheet:Range("B" + STRING(row-count) + ":C" +
                      STRING(row-count)):MergeCells = TRUE
    chWorkSheet:Range("D" + STRING(row-count) + ":E" +
                      STRING(row-count)):MergeCells = FALSE
    chWorkSheet:Range("A" + STRING(row-count)):value = "Aged"
   
    /*center*/
    chWorkSheet:Range("B" + STRING(row-count)):HorizontalAlignment = -4108
    chWorkSheet:Range("D" + STRING(row-count)):HorizontalAlignment = -4108
    chWorkSheet:Range("E" + STRING(row-count)):HorizontalAlignment = -4108
    chWorkSheet:Range("F" + STRING(row-count)):HorizontalAlignment = -4108
    chWorkSheet:Range("G" + STRING(row-count)):HorizontalAlignment = -4108
    chWorkSheet:Range("D" + STRING(row-count)):FONT:NAME = "Trebuchet MS"
    chWorkSheet:Range("D" + STRING(row-count)):FONT:SIZE = 8
    chWorkSheet:Range("E" + STRING(row-count)):FONT:NAME = "Trebuchet MS"
    chWorkSheet:Range("E" + STRING(row-count)):FONT:SIZE = 8
    chWorkSheet:Range("B" + STRING(row-count)):value = "Current"
    chWorksheet:Range("D" + STRING(row-count)):borders(10):lineStyle = 1
    chWorksheet:Range("D" + STRING(row-count)):borders(10):colorindex = 11
    chWorkSheet:Range("D" + STRING(row-count)):value = "30 Days"
    chWorkSheet:Range("E" + STRING(row-count)):value = "60 Days"
    chWorkSheet:Range("F" + STRING(row-count)):value = "90 Days"
    chWorkSheet:Range("G" + STRING(row-count)):value = ">90 Days"
    row-count = row-count + 1
    chWorkSheet:Range("C" + STRING(row-count)):FONT:NAME = "Trebuchet MS"
    chWorkSheet:Range("C" + STRING(row-count)):FONT:SIZE = 8
    chWorkSheet:Range("D" + STRING(row-count)):FONT:NAME = "Trebuchet MS"
    chWorkSheet:Range("D" + STRING(row-count)):FONT:SIZE = 8
    chWorkSheet:Range("E" + STRING(row-count)):FONT:NAME = "Trebuchet MS"
    chWorkSheet:Range("E" + STRING(row-count)):FONT:SIZE = 8
    chWorkSheet:Range("B" + STRING(row-count)):NumberFormat = chWorkSheet:Range("F13"):NumberFormat
    chWorkSheet:Range("D" + STRING(row-count)):NumberFormat = chWorkSheet:Range("F13"):NumberFormat
    chWorkSheet:Range("E" + STRING(row-count)):NumberFormat = chWorkSheet:Range("F13"):NumberFormat
    chWorkSheet:Range("B" + STRING(row-count) + ":C" +
                      STRING(row-count)):MergeCells = TRUE
    chWorkSheet:Range("D" + STRING(row-count) + ":E" +
                      STRING(row-count)):MergeCells = FALSE
    chWorksheet:Range("D" + STRING(row-count)):borders(10):lineStyle = 1
    chWorksheet:Range("D" + STRING(row-count)):borders(10):colorindex = 11
    chWorkSheet:Range("B" + STRING(row-count)):value = STRING(tt-cust-excel.aged[1])
    chWorkSheet:Range("D" + STRING(row-count)):value = STRING(tt-cust-excel.aged[2])
    chWorkSheet:Range("E" + STRING(row-count)):value = STRING(tt-cust-excel.aged[3])
    chWorkSheet:Range("F" + STRING(row-count)):value = STRING(tt-cust-excel.aged[4])
    chWorkSheet:Range("G" + STRING(row-count)):value = STRING(tt-cust-excel.aged[5])
    chExcelApplication:activeSheet:PageSetup:PrintArea = "$A$1:$G$" + STRING(row-count).
    
    IF NOT(LvOutputSelection = "PRINTER" OR 
           LvOutputSelection = "EMAIL") THEN
       chExcelApplication:ActiveSheet:Protect("advance4me").
      
  END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitializeExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeExcel Procedure 
PROCEDURE InitializeExcel :
/*------------------------------------------------------------------------------
  Purpose   :   Initializes Excel Environment
  Parameters:   None
  Notes     :   
------------------------------------------------------------------------------*/

  /* Capture the current active printer. */
  assign 
    CurActivePrinter = SESSION:PRINTER-NAME
    AdobePrinter     = "PDFcamp Printer".
  
  /* Connect to the running Excel session. */
  CREATE "Excel.Application" chExcelApplication.

  /* Network connection checks. */
  CREATE "WScript.Network" WshNetwork.

  IF NOT(VALID-HANDLE(WshNetwork)) THEN
  DO :
    MESSAGE "Unable to Create Wscript.Network" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
  
  IF LvOutputSelection = "Email" THEN
     WshNetwork:SetDefaultPrinter(AdobePrinter). 

  FILE-INFO:FILE-NAME = "template\Statement.xlt".

  /* Set the Excel Template to be used. */
  ASSIGN chFile = search (FILE-INFO:FULL-PATHNAME) no-error.
  
  if search (chFile) = ? then do:
    MESSAGE 'Template File: ' FILE-INFO:FULL-PATHNAME
            'cannot be found. Please verify that the file exists.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    apply 'CLOSE':U to this-procedure.
  end.

  /* Make Excel visible. */
  ASSIGN
     chFile = FILE-INFO:FULL-PATHNAME
     chExcelApplication:VISIBLE = TRUE.
  
  /* If we are going to E-Mail or Print, hide Excel. */
  IF LvOutputSelection = "Email"    or 
     LvOutputSelection = "Printer"  THEN
    chExcelApplication:VISIBLE = FALSE.
  
  /* Clear tt-FileList. */
  empty temp-table tt-filelist.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MainLoop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MainLoop Procedure 
PROCEDURE MainLoop :
/*------------------------------------------------------------------------------
  Purpose     : Main Loop for the Report Logic
  Parameters  : None
  Notes       :       
------------------------------------------------------------------------------*/

  /* Open our Excel Template. */  
  assign chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.
  
  /* Do not display Excel error messages. */
  chExcelApplication:DisplayAlerts = false  no-error.
  
  /* Disable screen updating so it will go faster */
  chExcelApplication:ScreenUpdating = False.

  /*Display Data*/
  run DisplayData.

  os-delete value(v-dir + "stmt.xls").     
  os-delete value(v-dir + "asi.pdf").
  os-delete value(v-dir + "stmt.pdf").

  IF LvOutputSelection = "PRINTER" THEN
  DO:
     NO-RETURN-VALUE chWorkbook:PrintOut(,,,,,False,).
         chWorkbook:Close(no) no-error. 
  END.
  ELSE IF LvOutputSelection = "Email" THEN
  DO:   
     chExcelApplication:ActiveSheet:SaveAs(v-dir + "stmt.xls") no-error.            
    /* NO-RETURN-VALUE chWorkbook:PrintOut(,,,,,False,).*/
     chWorkbook:Close(no) no-error.   
     chExcelApplication:Quit() no-error.

     pause 3.
     /*OS-DELETE VALUE(v-dir + "stmt.xls").
     OS-RENAME value(v-dir + "asi.pdf") value(v-dir + "stmt.pdf").*/
     ASSIGN LvCtr = LvCtr + 1.
     CREATE tt-filelist.
     ASSIGN tt-FileCtr  = LvCtr
            tt-FileName = v-dir + "stmt.xls".
  END.

  /* enable screen updating */
  chExcelApplication:ScreenUpdating = TRUE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

