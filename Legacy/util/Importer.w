&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE gcCompany AS CHARACTER NO-UNDO.
{custom/globdefs.i}
ASSIGN
    gcCompany = g_company.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-19 cbType fcFileName lHeaderRow ~
lLogOnly fiLogFile btnProcess btnCancel 
&Scoped-Define DISPLAYED-OBJECTS cbType fcFileName lHeaderRow lLogOnly ~
fiLogFile fcMessage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fConvertFileName C-Win 
FUNCTION fConvertFileName RETURNS CHARACTER
    ( INPUT ipcfileName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetDefaultImportFolder C-Win 
FUNCTION fGetDefaultImportFolder RETURNS CHARACTER
    (ipcCompany AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetLogFile C-Win 
FUNCTION fGetLogFile RETURNS CHARACTER
  ( ipcCompany AS CHARACTER  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fIsExcel C-Win 
FUNCTION fIsExcel RETURNS LOGICAL
    ( INPUT ipcfileName AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btnProcess 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE cbType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Import Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Accounts Receivable (AU1)","AR",
                     "Accounts Payable (VU1)","AP",
                     "Customers (AF1)","Cust",
                     "Vendors (VF1)","Vend",
                     "Finished Good (IF1)","FG",
                     "GL Accounts (GF2)","GL"
     DROP-DOWN-LIST
     SIZE 69 BY 1 NO-UNDO.

DEFINE VARIABLE fcFileName AS CHARACTER FORMAT "X(256)" 
     LABEL "Import File:" 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1.

DEFINE VARIABLE fcMessage AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 98 BY 1 NO-UNDO.

DEFINE VARIABLE fiLogFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "Log File" 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY 6.43.

DEFINE VARIABLE lHeaderRow AS LOGICAL INITIAL yes 
     LABEL "First Row is Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE lLogOnly AS LOGICAL INITIAL yes 
     LABEL "Generate Log Only" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     cbType AT ROW 2.43 COL 19 COLON-ALIGNED WIDGET-ID 6
     fcFileName AT ROW 3.86 COL 19 COLON-ALIGNED HELP
          "Enter file name to import order"
     lHeaderRow AT ROW 5.29 COL 21 WIDGET-ID 4
     lLogOnly AT ROW 5.29 COL 54 WIDGET-ID 12
     fiLogFile AT ROW 6.48 COL 19 COLON-ALIGNED WIDGET-ID 14
     btnProcess AT ROW 8.86 COL 28
     btnCancel AT ROW 8.86 COL 57
     fcMessage AT ROW 10.52 COL 3 NO-LABEL WIDGET-ID 2
     RECT-19 AT ROW 1.95 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 101.4 BY 11.29
         FONT 6.


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
         TITLE              = "Importer"
         HEIGHT             = 11.29
         WIDTH              = 105.6
         MAX-HEIGHT         = 26.62
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.62
         VIRTUAL-WIDTH      = 160
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         FONT               = 6
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
       fcFileName:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN fcMessage IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fcMessage:READ-ONLY IN FRAME FRAME-A        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Importer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Importer */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME FRAME-A /* Cancel */
DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProcess C-Win
ON CHOOSE OF btnProcess IN FRAME FRAME-A /* Start Process */
DO:
        DEFINE VARIABLE lProcess AS LOGICAL INIT NO NO-UNDO.
   
        
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.
        
        IF fcFileName <> "" AND SEARCH(fcFileName) = ? THEN 
        DO:
            MESSAGE "Import file not found "
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN NO-APPLY.
        END.

        
        IF fcFileName <> "" THEN
            MESSAGE "Are you ready to import orders from "  fcFileName "?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE lProcess.

        IF lProcess THEN RUN pRunProcess (fcFileName).
        APPLY "close" TO THIS-PROCEDURE.
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fcFileName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fcFileName C-Win
ON HELP OF fcFileName IN FRAME FRAME-A /* Import File: */
DO:
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lOK       AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cDefault  AS CHARACTER NO-UNDO.
        
        cDefault = fGetDefaultImportFolder(gcCompany).
        SYSTEM-DIALOG GET-FILE cFileName 
            TITLE "Select Image File to insert"
            FILTERS "Excel Comma delimited Files  (*.csv)" "*.csv",
            "Excel Files (*.xls,*.xlsx)" "*.xls,*.xlsx",
            "All Files    (*.*) " "*.*"
            INITIAL-DIR  cDefault
            MUST-EXIST
            USE-FILENAME
            UPDATE lOK.
      
        IF lOK THEN SELF:screen-value = cFileName.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

ON CLOSE OF THIS-PROCEDURE 
    RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
    /*    IF NOT llBatchMode THEN*/
    RUN enable_UI.


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
  DISPLAY cbType fcFileName lHeaderRow lLogOnly fiLogFile fcMessage 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-19 cbType fcFileName lHeaderRow lLogOnly fiLogFile btnProcess 
         btnCancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
  fiLogFile:SCREEN-VALUE = fGetLogFile(gcCompany).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pConvertXLStoCSV C-Win 
PROCEDURE pConvertXLStoCSV :
/*------------------------------------------------------------------------------
      Purpose:     Convert an excel file to a csv format.
      Parameters:  Input - Excel file name
                   Output - Csv file name
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcInputFile AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER ipcOutputFile AS CHAR NO-UNDO INIT "".

    DEF VAR chExcel     AS COM-HANDLE NO-UNDO.
    DEF VAR chWorkBook  AS COM-HANDLE NO-UNDO.
    DEF VAR chWorkSheet AS COM-HANDLE NO-UNDO.

    /* Start Excel */
    CREATE "Excel.Application" chExcel.
    ASSIGN 
        chExcel:Visible = FALSE.
  
  /* Open the file. */
  chExcel:Workbooks:Open(ipcInputFile,2,TRUE,,,,TRUE).

    /* Get the sheet. */
    ASSIGN 
        chWorkbook = chExcel:WorkBooks:Item(1).
    /*         chWorkSheet = chExcel:Sheets:Item(1).*/
  
  

    /* Convert the filename. */
    ASSIGN 
        ipcOutputFile = fConvertFileName(ipcInputFile). 

    /* Delete if already exists. */
    OS-COMMAND SILENT DEL VALUE(ipcOutputFile).
    /*  IF SEARCH(pcInFile) <> ? THEN
        OS-DELETE value(pcInfile) NO-ERROR. */

    /* Turn off alerts */
    chExcel:DisplayAlerts = FALSE.

  /* Save the new file in csv format. */
  chWorkBook:SaveAs(ipcOutputFile,6,,,,,,, TRUE).

    /* Close the workbook. */
    chWorkBook:Close().
  
    /* Release objects. */
    RELEASE OBJECT chWorkSheet NO-ERROR.
    RELEASE OBJECT chWorkBook NO-ERROR.

    /* Quit Excel */
    chExcel:QUIT.
    RELEASE OBJECT chExcel.

    RETURN.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pImport C-Win 
PROCEDURE pImport :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLogFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplLogOnly AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplHeader AS LOGICAL NO-UNDO.
    

    
    
    IF SEARCH(ipcFileName) NE ? THEN 
    DO:
         RUN util/import.p (gcCompany,
                           ipcFileName,
                           ipcLogFile,
                           iplLogOnly,
                           ipcType,
                           iplHeader).
    END.           
    ELSE 
        MESSAGE "File: " ipcFileName " not found." VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunProcess C-Win 
PROCEDURE pRunProcess :
/*------------------------------------------------------------------------------
          Purpose:     Main Process
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/  
    DEFINE INPUT PARAMETER ipcImportFileName  AS CHARACTER NO-UNDO.
    
    SESSION:SET-WAIT-STATE("general").   
    IF fIsExcel(ipcImportFileName) THEN 
        RUN pConvertXLStoCSV(ipcImportFileName, 
                             OUTPUT ipcImportFileName).

    RUN pImport (ipcImportFileName,
                 cbType,
                 lLogOnly,
                 lHeaderRow).

    SESSION:SET-WAIT-STATE(""). 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fConvertFileName C-Win 
FUNCTION fConvertFileName RETURNS CHARACTER
    ( INPUT ipcfileName AS CHAR ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cBaseFileName AS CHAR NO-UNDO.


    ASSIGN 
        cBaseFileName = TRIM(ENTRY(1,ipcfileName,".") + ".csv").

    RETURN cBaseFileName.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetDefaultImportFolder C-Win 
FUNCTION fGetDefaultImportFolder RETURNS CHARACTER
    (ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
         Purpose: Returns the character value for ImportFolder NK1 
         Notes: Default path when launching lookup on import file
        ------------------------------------------------------------------------------*/    
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO. 
    
    RUN sys\ref\nk1look.p (ipcCompany,
        'ImportFolder',
        'C',
        NO,
        NO,
        '',
        '', 
        OUTPUT cReturn,
        OUTPUT lFound).
    
    IF lFound THEN 
        RETURN cReturn.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetLogFile C-Win 
FUNCTION fGetLogFile RETURNS CHARACTER
  ( ipcCompany AS CHARACTER  ):
    /*------------------------------------------------------------------------------
         Purpose: Returns the character value for ImportFolder NK1 
         Notes: Default path when launching lookup on import file
        ------------------------------------------------------------------------------*/    
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO. 
    
    RUN sys\ref\nk1look.p (ipcCompany,
        'ImportLog',
        'C',
        NO,
        NO,
        '',
        '', 
        OUTPUT cReturn,
        OUTPUT lFound).
    
    IF lFound THEN DO:
        IF cReturn EQ "" THEN 
            cReturn = "C:\ImportLog.csv".
        RETURN cReturn.
    END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fIsExcel C-Win 
FUNCTION fIsExcel RETURNS LOGICAL
    ( INPUT ipcfileName AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cExt AS CHARACTER NO-UNDO INIT "".

    ASSIGN 
        cExt = TRIM(ENTRY(2,ipcfileName,".")).

    IF LOOKUP(cExt, "xls,xlsx") > 0 THEN 
        RETURN TRUE.
    ELSE
        RETURN FALSE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

