&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : trkschxl.p 
    
    Purpose     : Generates Truck Plan Selection Using Excel Template

    Syntax      : run oerep/trkschxl.p (

    Description : Generates Truck Plan Selection Using Excel Template

    Author(s)   : Eric Panchenko
    
    Created     : April 4, 2007
    
    Notes       : 
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters */
def input param ip-multi-faxout   as log  no-undo. /* fax multiple recipents or single */
def input param ip-lines-per-page as int  no-undo.

/* Buffers */

/* Includes */
{sys/inc/var.i shared}

/* Temp-Tables */
{oerep/tt-truck-stop.i}

/* Variables */
def var viLoop           AS INT no-undo.
DEF VAR viWorkSheetCount AS INT NO-UNDO.
DEF VAR viRowCount       AS INT NO-UNDO.

/* VARIABLE FOR EXCEL OUTPUT */
DEFINE new SHARED VARIABLE LvOutputSelection    AS CHAR NO-UNDO.
DEFINE new SHARED VARIABLE CallingParameter     AS CHAR NO-UNDO.

/* Variables for excel Automation  */
DEFINE NEW SHARED VARIABLE chExcelApplication   AS COM-HANDLE   NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorkBook           AS COM-HANDLE   NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorksheet          AS COM-HANDLE   NO-UNDO.
DEFINE NEW SHARED VARIABLE chHyper              AS COM-HANDLE   NO-UNDO.
DEFINE            VARIABLE v-cell               AS CHARACTER    NO-UNDO.
DEFINE            VARIABLE t-dwg                AS CHAR         NO-UNDO.
DEFINE            VARIABLE t-name               AS CHARACTER    NO-UNDO   FORMAT "x(40)" .
DEFINE            VARIABLE t-fnd                AS LOGICAL      NO-UNDO   INIT "False"    .
DEFINE            VARIABLE t-seq                AS INTEGER      NO-UNDO.
DEFINE            VARIABLE inRowCount           AS INTEGER      NO-UNDO   INITIAL 1.
DEFINE            VARIABLE chFile               AS CHAR         NO-UNDO.
DEFINE            VARIABLE LvLineCnt            AS INT          NO-UNDO.
DEFINE            VARIABLE CurrDir              AS CHAR         NO-UNDO.
DEFINE            VARIABLE LvCtr                as int          no-undo.
DEFINE            VARIABLE CurActivePrinter     AS CHAR         NO-UNDO.
DEFINE            VARIABLE AdobePrinter         AS CHAR         NO-UNDO.
define            variable CommandString        AS CHAR         NO-UNDO.
define            variable WshNetwork           as com-handle.
DEFINE            VARIABLE LvFirstTimePrint     AS LOGICAL      NO-UNDO   INIT no.

/* Build a Table to keep sequence of pdf files */
DEFINE new SHARED TEMP-TABLE tt-filelist
                       FIELD tt-FileCtr         AS INT
                       FIELD tt-FileName        AS CHAR
                       INDEX filelist           IS PRIMARY 
                             TT-FILECTR.

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

run InitializeExcel.
run MainLoop.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-InitializeExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeExcel Procedure 
PROCEDURE InitializeExcel :
/*------------------------------------------------------------------------------
  Purpose:     Initializes Excel Environment
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Capture the current active printer. */
  IF LvOutputSelection = "email" THEN
    assign 
      CurActivePrinter = SESSION:PRINTER-NAME
      AdobePrinter     = "PDFcamp Printer".
  
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
  
  /* Initialize Excel. */
  CREATE "Excel.Application" chExcelApplication NO-ERROR.
  
  /* Check if Excel got initialized. */
  IF not (valid-handle (chExcelApplication)) THEN
  DO :
    MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
  
  /* Set our current directory. */
  RUN UTIL/CurrDir.p (output CurrDir).
  
  /* Set the Excel Template to be used. */
  ASSIGN chFile = CurrDir + "\Template\TruckPlan.xlt" no-error.
  
  /* Make Excel visible. */
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
  Purpose:      Main Loop for the Report Logic
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Open our Excel Template. */  
  assign chWorkbook = chExcelApplication:Workbooks:Open(chfile) no-error.
  
  /* Do not display Excel error messages. */
  chExcelApplication:DisplayAlerts = false  no-error.

  /* Disable screen updating so it will go faster */
  chExcelApplication:ScreenUpdating = False.

  /*Create a worksheet for each truck*/
  FOR EACH tt-report WHERE
      tt-report.carrier NE "" AND
      tt-report.truck-code NE ""
      BREAK BY tt-report.carrier
            BY tt-report.truck-code
            BY tt-report.load-no
            BY tt-report.ship-date
            BY tt-report.stop-no:

      IF FIRST-OF(tt-report.ship-date) THEN
         viWorkSheetCount = viWorkSheetCount + 1.
  END.

  /* Create a worksheet for each truck */
  DO viLoop = 2 TO viWorkSheetCount:
     chWorkbook:WorkSheets(1):COPY(chExcelApplication:Sheets:item(1)) NO-ERROR.
  END.

  viWorkSheetCount = 0.

  /*Go through each truck stop */
  FOR EACH tt-report WHERE
      tt-report.carrier NE "" AND
      tt-report.truck-code NE ""
      BREAK BY tt-report.carrier
            BY tt-report.truck-code
            BY tt-report.load-no
            BY tt-report.ship-date
            BY tt-report.stop-no:

      IF FIRST-OF(tt-report.ship-date) THEN
      DO:
        ASSIGN
          viWorkSheetCount = viWorkSheetCount + 1
          viRowCount       = 0
          chWorkSheet      = chExcelApplication:Sheets:item(viWorkSheetCount)
          chWorkSheet:name = tt-report.truck-code + "-" + tt-report.load-no
          NO-ERROR.

        /* Go to the Active Sheet. */
        chWorkbook:WorkSheets(viWorkSheetCount):Activate no-error.

        /*Populate Truck Code and Route*/
        RUN SetCellValue ("L1",tt-report.truck-code).
        RUN SetCellValue ("B3",tt-report.load-no).
        RUN SetCellValue ("N3",STRING(tt-report.ship-date)).
      END.

      viRowCount = viRowCount + 1.

      /* Populate cells */
      RUN SetCellValue ("A" + STRING(4 + viRowCount),STRING(tt-report.stop-no)).
      RUN SetCellValue ("B" + STRING(4 + viRowCount),tt-report.cust-name).
      RUN SetCellValue ("D" + STRING(4 + viRowCount),tt-report.ship-to-text).
      RUN SetCellValue ("E" + STRING(4 + viRowCount),tt-report.deliv-zone).
      RUN SetCellValue ("F" + STRING(4 + viRowCount),STRING(tt-report.order-no) +
                                                     "-" + STRING(tt-report.line-no)).
      RUN SetCellValue ("G" + STRING(4 + viRowCount),tt-report.item-no).


      IF tt-report.key-06 NE "P" THEN
         RUN SetCellValue ("H" + STRING(4 + viRowCount),STRING(tt-report.rel-no)).
      ELSE
         RUN SetCellValue ("J" + STRING(4 + viRowCount),STRING(tt-report.bol-no)).

      RUN SetCellValue ("I" + STRING(4 + viRowCount),STRING(tt-report.pallets)).
  END.

  /* Go to the first Sheet. */
  chWorkbook:WorkSheets(1):Activate no-error.

  /*
  /* Preview the Spreadsheet. */
  chWorkbook:PrintOut(1,999,1,yes,,no,no) no-error. */

  /* enable screen updating */
  chExcelApplication:ScreenUpdating = TRUE.

  /* Release created objects. */
  RELEASE OBJECT WshNetwork         NO-ERROR.
  RELEASE OBJECT chExcelApplication NO-ERROR.
  RELEASE OBJECT chHyper NO-ERROR.
  RELEASE OBJECT chWorkBook NO-ERROR.
  RELEASE OBJECT chWorkSheet NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetCellValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCellValue Procedure 
PROCEDURE SetCellValue :
/*------------------------------------------------------------------------------
  Purpose:    Positions to a cell and set its value.
  
  Parameters: icPosition - Cell Position
              icCellValue - Cell Value
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Parameters */
  def input param icPosition  as char no-undo.
  def input param icCellValue as char no-undo.

  /* Go to the Cell Position and Set its value. */
  chWorkSheet:Range(icPosition):value = icCellValue no-error.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

