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
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.

{est\ttInputEst.i}

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-19 fcFileName lHeaderRow btn-process ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fcFileName lHeaderRow fcMessage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fConvertFileName C-Win
FUNCTION fConvertFileName RETURNS CHARACTER 
    (ipcFilename AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetDefaultImportFolder C-Win
FUNCTION fGetDefaultImportFolder RETURNS CHARACTER 
    ( ipcCompany AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetImportFormat C-Win
FUNCTION fGetImportFormat RETURNS CHARACTER 
    ( ipcCompany AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fIsExcel C-Win
FUNCTION fIsExcel RETURNS LOGICAL 
    (ipcFileName AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
    LABEL "Ca&ncel" 
    SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
    LABEL "&Start Process" 
    SIZE 18 BY 1.14.

DEFINE VARIABLE fcFileName AS CHARACTER FORMAT "X(256)" 
    LABEL "Import File:" 
    VIEW-AS FILL-IN 
    SIZE 69 BY 1.

DEFINE VARIABLE fcMessage  AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 99 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-19
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 98 BY 3.33.

DEFINE VARIABLE lHeaderRow AS LOGICAL INITIAL yes 
    LABEL "First Row is Header" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    fcFileName AT ROW 2.67 COL 18 COLON-ALIGNED HELP
    "Enter file name to import order"
    lHeaderRow AT ROW 4.1 COL 20 WIDGET-ID 4
    btn-process AT ROW 5.76 COL 27
    btn-cancel AT ROW 5.76 COL 56
    fcMessage AT ROW 7.67 COL 2 NO-LABEL WIDGET-ID 2
    RECT-19 AT ROW 1.95 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 101.4 BY 8.57
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
        TITLE              = "Import Estimate"
        HEIGHT             = 8.95
        WIDTH              = 102
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
    fcFileName:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN fcMessage IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
    fcMessage:READ-ONLY IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Import Estimate */
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
ON WINDOW-CLOSE OF C-Win /* Import Estimate */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
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
        
        cDefault = fGetDefaultImportFolder(ipcCompany).
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fcFileName C-Win
ON LEAVE OF fcFileName IN FRAME FRAME-A /* Import File: */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-to_ord-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
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
    EMPTY TEMP-TABLE ttInputEst.
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
    DISPLAY fcFileName lHeaderRow fcMessage 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-19 fcFileName lHeaderRow btn-process btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pConvertXLStoCSV C-Win
PROCEDURE pConvertXLStoCSV:
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
    
    DEFINE VARIABLE cImportFormat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFile         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLines        AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE cForm         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPartID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPartName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQtyPerSet    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLength       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWidth        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSkipImport   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMaterial     AS CHARACTER NO-UNDO.

    cImportFormat = fGetImportFormat(ipcCompany).
    cFile = ipcFileName.
    IF SEARCH(cFile) NE ? THEN 
    DO:
        INPUT FROM VALUE(cFile) NO-ECHO.
        REPEAT:
            CASE cImportFormat:
                WHEN "Protagon" THEN 
                    DO:
                        ASSIGN 
                            cForm = ""
                            cPartID = ""
                            cPartName = ""
                            cQtyPerSet = ""
                            cLength = ""
                            cWidth = ""
                            cSkipImport = ""
                            cMaterial = ""
                            .
                        IMPORT DELIMITER ","
                            cForm
                            cPartID
                            cPartName
                            cQtyPerSet
                            cLength
                            cWidth
                            cSkipImport
                            cMaterial
                            .
                       
                        IF NOT TRIM(cSkipImport) BEGINS "Y" AND cPartID NE "" THEN DO:           
                            iLines = iLines + 1.
                            IF iLines = 1 AND lHeaderRow THEN NEXT.
                            IF iLines EQ 100 THEN 
                            DO: 
                                MESSAGE "Import limit of " iLines " lines reached" VIEW-AS ALERT-BOX. 
                                RETURN. 
                            END.                         
                            CREATE ttInputEst.
               
                            ASSIGN 
                                ttInputEst.iFormNo        = INTEGER(cForm)
                                ttInputEst.cPartID        = cPartID
                                ttInputEst.cCadID         = cPartID
                                ttInputEst.cPartName      = SUBSTRING(REPLACE(cPartname,Chr(10),""),1,30)
                                ttInputEst.iQuantityYield = INTEGER(cQtyPerSet)
                                ttInputEst.dLength        = DECIMAL(TRIM(cLength,'"'))
                                ttInputEst.dWidth         = DECIMAL(TRIM(cWidth,'"'))
                                ttInputEst.cStyle         = "MISC"
                                ttInputEst.cBoard         = "NC"
                                .
                       END. /*not skip import*/
                    END. /*format Protagon*/
                OTHERWISE 
                RETURN.
            END CASE.
        END. /*input repeat*/
        INPUT CLOSE.
        
        IF lHeaderRow THEN iLines = iLines - 1.
        MESSAGE "Import Completed" SKIP
            iLines " forms have been imported to this estimate." 
            VIEW-AS ALERT-BOX.
    END.           


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
        RUN pConvertXLStoCSV(ipcImportFileName, OUTPUT ipcImportFileName).
    RUN pImport (ipcImportFileName).
    RUN pValidateImportedData. 
    SESSION:SET-WAIT-STATE(""). 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidateImportedData C-Win 
PROCEDURE pValidateImportedData :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/*FOR EACH ttInputEst:                           */
/*    MESSAGE                                    */
/*        "Form" ttInputEst.iFormNo SKIP         */
/*        "Part" ttInputEst.cPartID SKIP         */
/*        "Desc" ttInputEst.cPartDescription skip*/
/*        "Length" ttInputEst.dLength skip       */
/*        "Width" ttInputEst.dWidth              */
/*        VIEW-AS ALERT-BOX.                     */
/*END.                                           */

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
         Purpose: Returns the character value for CEImportFormFolder NK1 
         Notes: Default path when launching lookup on import file
        ------------------------------------------------------------------------------*/    
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO. 
    
    RUN sys\ref\nk1look.p (ipcCompany,
        'CEImportFormFolder',
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetImportFormat C-Win
FUNCTION fGetImportFormat RETURNS CHARACTER 
    ( ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
         Purpose: Returns the format for importing forms
         Notes:
        ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO. 
    
    RUN sys\ref\nk1look.p (ipcCompany,
        'CEImportForm',
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







