&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
    File        : wDataQuery.w
    Purpose     : Actual databrowse for user query

    Author(s)   : Patrick Tingen
    Created     : 2019

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) = 0 &THEN
  DEFINE INPUT PARAMETER piQueryNr AS INTEGER NO-UNDO.
&ELSE
  DEFINE VARIABLE piQueryNr AS INTEGER NO-UNDO INITIAL 2.
&ENDIF

{queryLib.i}

DEFINE VARIABLE ghQueryProc AS HANDLE      NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnExcel btnText btnHtml btnCsv 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEscapedData C-Win 
FUNCTION getEscapedData RETURNS CHARACTER
  ( pcTarget AS CHARACTER
  , pcString AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCsv 
     LABEL "CSV" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnExcel 
     LABEL "Excel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnHtml 
     LABEL "HTML" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnText 
     LABEL "Text" 
     SIZE 15 BY 1.14.

DEFINE RECTANGLE rctData
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 785 BY 307
     BGCOLOR 17 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnExcel AT ROW 1.24 COL 4 WIDGET-ID 298
     btnText AT ROW 1.24 COL 20 WIDGET-ID 300
     btnHtml AT ROW 1.24 COL 36 WIDGET-ID 302
     btnCsv AT ROW 1.24 COL 52 WIDGET-ID 304
     rctData AT Y 33 X 0 WIDGET-ID 272
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 158.6 BY 16.57 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 16.67
         WIDTH              = 159.6
         MAX-HEIGHT         = 29.57
         MAX-WIDTH          = 226.2
         VIRTUAL-HEIGHT     = 29.57
         VIRTUAL-WIDTH      = 226.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR RECTANGLE rctData IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       rctData:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* <insert window title> */
DO:
  RUN resizeWindow IN ghQueryProc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCsv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCsv C-Win
ON CHOOSE OF btnCsv IN FRAME DEFAULT-FRAME /* CSV */
DO:
  DEFINE VARIABLE lOk       AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.
  
  /* Ask name, use query name as initial value for file name */
  FIND ttQuery.
  cFileName = ttQuery.queryName.

  SYSTEM-DIALOG GET-FILE cFilename
    FILTERS "CSV file (*.csv)" "*.csv",
            "Any File (*.*)" "*.*"
    INITIAL-FILTER 1 ASK-OVERWRITE USE-FILENAME CREATE-TEST-FILE DEFAULT-EXTENSION ".csv" SAVE-AS UPDATE lOk.

  IF NOT lOk THEN RETURN.
  
  SESSION:SET-WAIT-STATE('general').
  RUN saveAsCSV(cFilename).
  SESSION:SET-WAIT-STATE('').
  MESSAGE 'File saved as' cFilename VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  OS-COMMAND NO-WAIT START VALUE('explorer.exe "' + cFilename + '"').  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExcel C-Win
ON CHOOSE OF btnExcel IN FRAME DEFAULT-FRAME /* Excel */
DO:
  DEFINE VARIABLE lOk       AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.
  
  /* Ask name, use query name as initial value for file name */
  FIND ttQuery.
  cFileName = ttQuery.queryName.

  SYSTEM-DIALOG GET-FILE cFilename
    FILTERS "XLSX file (*.xlsx)" "*.xlsx",
            "Any File (*.*)" "*.*"
    INITIAL-FILTER 1 ASK-OVERWRITE USE-FILENAME CREATE-TEST-FILE DEFAULT-EXTENSION ".xlsx" SAVE-AS UPDATE lOk.

  IF NOT lOk THEN RETURN.
  
  SESSION:SET-WAIT-STATE('general').
  RUN saveAsCSV(cFilename).
  RUN saveAsExcel(cFilename).
  SESSION:SET-WAIT-STATE('').
  MESSAGE 'File saved as' cFilename VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  OS-COMMAND NO-WAIT START VALUE('explorer.exe "' + cFilename + '"').  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHtml
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHtml C-Win
ON CHOOSE OF btnHtml IN FRAME DEFAULT-FRAME /* HTML */
DO:
  DEFINE VARIABLE lOk       AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.
  
  /* Ask name, use query name as initial value for file name */
  FIND ttQuery.
  cFileName = ttQuery.queryName.

  SYSTEM-DIALOG GET-FILE cFilename
    FILTERS "HTML file (*.html)" "*.html",
            "Any File (*.*)" "*.*"
    INITIAL-FILTER 1 ASK-OVERWRITE USE-FILENAME CREATE-TEST-FILE DEFAULT-EXTENSION ".html" SAVE-AS UPDATE lOk.

  IF NOT lOk THEN RETURN.
  
  SESSION:SET-WAIT-STATE('general').
  RUN saveAsHTML(cFilename).
  SESSION:SET-WAIT-STATE('').
  MESSAGE 'File saved as' cFilename VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  OS-COMMAND NO-WAIT START VALUE('explorer.exe "' + cFilename + '"').    
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnText C-Win
ON CHOOSE OF btnText IN FRAME DEFAULT-FRAME /* Text */
DO:
  DEFINE VARIABLE lOk       AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.
  
  /* Ask name, use query name as initial value for file name */
  FIND ttQuery.
  cFileName = ttQuery.queryName.

  SYSTEM-DIALOG GET-FILE cFilename
    FILTERS "Text file (*.txt)" "*.txt",
            "Any File (*.*)" "*.*"
    INITIAL-FILTER 1 ASK-OVERWRITE USE-FILENAME CREATE-TEST-FILE DEFAULT-EXTENSION ".txt" SAVE-AS UPDATE lOk.

  IF NOT lOk THEN RETURN.
  
  SESSION:SET-WAIT-STATE('general').
  RUN saveAsTxt(cFilename).
  SESSION:SET-WAIT-STATE('').
  MESSAGE 'File saved as' '"' + cFilename + '"' VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  OS-COMMAND NO-WAIT START VALUE('explorer.exe "' + cFilename + '"').  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  RUN initObject. 
   
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE btnExcel btnText btnHtml btnCsv 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initObject C-Win 
PROCEDURE initObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iWidth  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iHeight AS INTEGER     NO-UNDO.
  DEFINE VARIABLE hFrame  AS HANDLE      NO-UNDO.
  
  DEFINE BUFFER bQuery FOR ttQuery.

  DO WITH FRAME {&FRAME-NAME}:
  
    /* The UIB allows screen size up to 320 chars but a normal monitor
     * is wider. To use all space, maximize first to max size.
     * Somehow this does not work if you set VIRTUAL-*-PIXELS attributes
     */
    iWidth  = {&WINDOW-NAME}:WIDTH-PIXELS.
    iHeight = {&WINDOW-NAME}:HEIGHT-PIXELS.
    {&WINDOW-NAME}:WIDTH-PIXELS  = SESSION:WIDTH-PIXELS.
    {&WINDOW-NAME}:HEIGHT-PIXELS = SESSION:HEIGHT-PIXELS.
    {&WINDOW-NAME}:WIDTH-PIXELS  = iWidth.
    {&WINDOW-NAME}:HEIGHT-PIXELS = iHeight.   
    
    RUN loadQuery(piQueryNr, OUTPUT DATASET dsQuery BY-REFERENCE).
    FIND bQuery NO-ERROR.
    IF NOT AVAILABLE bQuery THEN RETURN.
    {&window-name}:TITLE = bQuery.queryName.
    
    /* Run the results progam */
    RUN wQueryResults.w PERSISTENT SET ghQueryProc(INPUT THIS-PROCEDURE, OUTPUT hFrame).
      
    /* Bind our dataset to the screen */  
    RUN ScreenInit IN ghQueryProc(INPUT-OUTPUT DATASET dsQuery BIND).
    RUN ScreenShow IN ghQueryProc.

    hFrame:X = rctData:X.
    hFrame:Y = rctData:Y.
    hFrame:WIDTH-PIXELS  = rctData:WIDTH-PIXELS.
    hFrame:HEIGHT-PIXELS = rctData:HEIGHT-PIXELS.
    RUN resizeWindow IN ghQueryProc.
    
  END.

END PROCEDURE. /* initObject */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAsCSV C-Win 
PROCEDURE saveAsCSV :
/* Save query data as csv file
*/
  DEFINE INPUT PARAMETER pcFileName AS CHARACTER NO-UNDO.
  
  DEFINE BUFFER bField FOR ttField.
  DEFINE BUFFER bQuery FOR ttQuery.
  
  FIND bQuery.
  OUTPUT TO VALUE(pcFilename).
  
  /* Header */
  FOR EACH bField BREAK BY bField.queryNr:
    PUT UNFORMATTED bField.fieldLabel.
    IF NOT LAST-OF(bField.queryNr) THEN PUT ";".
  END.
  PUT SKIP.
  
  /* Data */
  bQuery.hQuery:GET-FIRST().
  DO WHILE NOT bQuery.hQuery:QUERY-OFF-END:
  
    FOR EACH bField BREAK BY bField.queryNr:
      PUT UNFORMATTED bField.hField:BUFFER-VALUE(bField.iExtent).
      IF NOT LAST-OF(bField.queryNr) THEN PUT ";".
    END.
    PUT SKIP.

    bQuery.hQuery:GET-NEXT().
  END.

  OUTPUT CLOSE.     

END PROCEDURE. /* saveAsCSV */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAsExcel C-Win 
PROCEDURE saveAsExcel :
/* save a CSV file as excel file
*/
  DEFINE INPUT PARAMETER pcCsvFile AS CHARACTER   NO-UNDO.
  
  DEFINE VARIABLE iField      AS INTEGER          NO-UNDO.
  DEFINE VARIABLE hQuery      AS HANDLE           NO-UNDO.
  DEFINE VARIABLE chExcel     AS COMPONENT-HANDLE NO-UNDO.
  DEFINE VARIABLE chWorkbook  AS COMPONENT-HANDLE NO-UNDO.
  DEFINE VARIABLE chQuery     AS COMPONENT-HANDLE NO-UNDO.
  DEFINE VARIABLE raw-array   AS RAW              NO-UNDO.
  DEFINE VARIABLE iNumColumns AS INTEGER          NO-UNDO.
  DEFINE VARIABLE cXlsFile    AS CHARACTER        NO-UNDO.

  DEFINE BUFFER bField FOR ttField.
  DEFINE BUFFER bQuery FOR ttQuery.

  SESSION:SET-WAIT-STATE('general').
  
  /* Start excel */
  CREATE "Excel.Application" chExcel NO-ERROR.
  chExcel:visible       = NO.
  chExcel:DisplayAlerts = NO.

  chWorkbook = chExcel:workbooks:add.

  /* Import the CSV */
  chQuery = chWorkbook:ActiveSheet:QueryTables:add("TEXT;" + pcCsvFile, chExcel:Range("$A$1")).
  ASSIGN
    chQuery:name                         = "data"
    chQuery:FieldNames                   = TRUE
    chQuery:RowNumbers                   = FALSE
    chQuery:FillAdjacentFormulas         = FALSE
    chQuery:PreserveFormatting           = TRUE
    chQuery:RefreshOnFileOpen            = FALSE
    chQuery:RefreshStyle                 = 1 /* xlInsertDeleteCells */
    chQuery:SavePassword                 = FALSE
    chQuery:SaveData                     = TRUE
    chQuery:AdjustColumnWidth            = TRUE
    chQuery:RefreshPeriod                = 0
    chQuery:TextFilePromptOnRefresh      = FALSE
    chQuery:TextFilePlatform             = 850
    chQuery:TextFileStartRow             = 1
    chQuery:TextFileParseType            = 1 /* xlDelimited */
    chQuery:TextFileTextQualifier        = -4142 /* xlTextQualifierNone */
    chQuery:TextFileConsecutiveDelimiter = FALSE
    chQuery:TextFileTabDelimiter         = FALSE
    chQuery:TextFileSemicolonDelimiter   = TRUE
    chQuery:TextFileCommaDelimiter       = FALSE
    chQuery:TextFileSpaceDelimiter       = FALSE
    chQuery:TextFileTrailingMinusNumbers = TRUE.

  /* Set the type of formatting of the columns, uses raw variable to pass array of types to Excel */
  FOR EACH bField:
    iField = iField + 1.
    
    IF bField.fieldLabel = "" THEN
    DO:
      PUT-BYTE(raw-array, iField) = 9. /* 9 = xlSkipColumn */
      NEXT.
    END.

    CASE bField.dataType:
      WHEN "character" THEN PUT-BYTE(raw-array, iField) = 2. /* 2 = xlTextFormat Excel */
      WHEN "date"      THEN PUT-BYTE(raw-array, iField) = 4. /* 4 = xlDMYFormat */
      OTHERWISE PUT-BYTE(raw-array, iField) = 1. /* 1 = xlGeneralFormat */
    END CASE.
  
    iNumColumns = iNumColumns + 1.
  END. /* for each bField */

  chQuery:TextFileColumnDataTypes = raw-array.
  chQuery:REFRESH().      

  /* First line bold */
  chExcel:range("A1", chExcel:cells(1, iNumColumns)):select().
  chExcel:selection:font:bold = TRUE.
  chExcel:selection:Interior:ColorIndex = 34.
  chExcel:selection:Interior:Pattern = 1.

  /* Autofilter */
  chExcel:selection:AutoFilter(,,).

  /* Adjust width of columns */
  chExcel:Cells:select().
  chExcel:selection:columns:AutoFit().
  chExcel:Range("A1"):select(). /* tira selecao */

  /* Save as XLSX */
  cXlsFile = REPLACE( pcCsvFile, ".csv", ".xlsx").
  chWorkbook:SaveAs(cXlsFile,51,"","",FALSE,FALSE,). /* 51 = xlOpenXMLWorkbook */

  /* Cleanup */
  FINALLY:
    chExcel:DisplayAlerts = YES.
    chExcel:quit().
    
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chQuery.
    RELEASE OBJECT chExcel.

    ASSIGN
      chWorkbook = ?
      chQuery    = ?
      chExcel    = ?.

    SESSION:SET-WAIT-STATE('').  
  END FINALLY.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAsHTML C-Win 
PROCEDURE saveAsHTML :
/* Save query data as html file
*/
  DEFINE INPUT PARAMETER pcFileName AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE iRowNr AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cData  AS CHARACTER NO-UNDO.
  
  DEFINE BUFFER bField FOR ttField.
  DEFINE BUFFER bQuery FOR ttQuery.
  
  FIND bQuery.
  SESSION:SET-WAIT-STATE('general').
  OUTPUT TO VALUE(pcFilename).
  
  /* Header */
  PUT UNFORMATTED
    '<html><head><title>' bQuery.queryName '</title></head>' SKIP '<body>' SKIP.
  
  /* Title */
  PUT UNFORMATTED '<h1>' bQuery.queryName '</h1>' SKIP.
  
  /* Labels */
  PUT UNFORMATTED '<table border="0"><tr bgcolor="KHAKI">' SKIP.
  FOR EACH bField BY bField.orderNr:
    PUT UNFORMATTED SUBSTITUTE('  <th>&1</th>', bField.fieldLabel) SKIP.    
  END.
  PUT '</tr>' SKIP.
  
  /* Data */
  bQuery.hQuery:GET-FIRST().
  DO WHILE NOT bQuery.hQuery:QUERY-OFF-END:
    iRowNr = iRowNr + 1.
  
    /* alternating colors per row */
    PUT UNFORMATTED SUBSTITUTE('<tr bgcolor="&1">', TRIM(STRING(iRowNr MOD 2 = 1,'WHITE/#f8f8f8'))) SKIP.
    FOR EACH bField BY bField.orderNr:
      cData = bField.hField:BUFFER-VALUE(bField.iExtent).
      cData = getEscapedData("HTML", cData).
      PUT UNFORMATTED SUBSTITUTE('  <td>&1</td>', cData) SKIP.
    END.
    PUT '</tr>' SKIP.

    bQuery.hQuery:GET-NEXT().
  END.
  
  PUT UNFORMATTED '</table></body></html>'.
  OUTPUT CLOSE.     
  
  FINALLY:
    SESSION:SET-WAIT-STATE('').
  END.
END PROCEDURE. /* saveAsHTML */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveAsTXT C-Win 
PROCEDURE saveAsTXT :
/* Save query data as txt file
*/
  DEFINE INPUT PARAMETER pcFileName AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE iWidth  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cFormat AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cLine   AS CHARACTER   NO-UNDO.
  
  DEFINE BUFFER bField FOR ttField.
  DEFINE BUFFER bQuery FOR ttQuery.
  
  FIND bQuery.
  OUTPUT TO VALUE(pcFilename).
  
  /* Header */
  FOR EACH bField BREAK BY bField.queryNr:
    iWidth = MAXIMUM(LENGTH(bField.fieldLabel), bField.hColumn:WIDTH-CHARS).
    cFormat = SUBSTITUTE('x(&1)', iWidth).
    PUT UNFORMATTED STRING(bField.fieldLabel, cFormat).
    cLine = cLine + FILL('-', iWidth).
    IF NOT LAST-OF(bField.queryNr) THEN DO:
      PUT " | ".
      cLine = cLine + ' . '.
    END.
  END.
  PUT UNFORMATTED SKIP cLine SKIP.
  
  /* Data */
  bQuery.hQuery:GET-FIRST().
  DO WHILE NOT bQuery.hQuery:QUERY-OFF-END:
  
    FOR EACH bField BREAK BY bField.queryNr:
      iWidth = MAXIMUM(LENGTH(bField.fieldLabel), bField.hColumn:WIDTH-CHARS).
      cFormat = SUBSTITUTE('x(&1)', iWidth).
      PUT UNFORMATTED STRING(bField.hField:STRING-VALUE(bField.iExtent), cFormat).
      IF NOT LAST-OF(bField.queryNr) THEN PUT " | ".
    END.
    PUT SKIP.

    bQuery.hQuery:GET-NEXT().
  END.

  OUTPUT CLOSE.     

END PROCEDURE. /* saveAsTXT */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEscapedData C-Win 
FUNCTION getEscapedData RETURNS CHARACTER
  ( pcTarget AS CHARACTER
  , pcString AS CHARACTER ) :
  /* Return html- or 4gl-safe string
  */
  DEFINE VARIABLE cOutput AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iTmp    AS INTEGER   NO-UNDO.

  /* Garbage in, garbage out  */
  cOutput = pcString.

  CASE pcTarget:
    WHEN "HTML" THEN
    DO:
      cOutput = REPLACE(cOutput,"<","&lt;").
      cOutput = REPLACE(cOutput,">","&gt;").
    END.

    WHEN "4GL" THEN
    DO:
      /* Replace single quotes because we are using them for 4GL separating too */
      cOutput = REPLACE(cOutput, "'", "~~'").

      /* Replace CHR's 1 till 13  */
      DO iTmp = 1 TO 13:
        cOutput = REPLACE(cOutput, CHR(iTmp), "' + chr(" + string(iTmp) + ") + '").
      END.
    END.
  END CASE.

  RETURN pcString.

END FUNCTION. /* getEscapedData */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
