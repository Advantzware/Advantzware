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
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}    
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.
 
DEFINE STREAM sRpt.
DEFINE STREAM sCsv.
DEFINE VARIABLE v-process AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 fiReport btnBrowseFolder-2 fiCSV ~
btnBrowseFolder tgCsvExport btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiReport fiCSV tgCsvExport 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnConvertSlash C-Win
FUNCTION fnConvertSlash RETURNS CHARACTER 
  ( ipcPath AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnDefaultRptName C-Win 
FUNCTION fnDefaultRptName RETURNS CHARACTER
    (INPUT ipcType AS CHARACTER  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnGetFileName C-Win 
FUNCTION fnGetFileName RETURNS CHARACTER
    (INPUT ipcFullPath AS CHARACTER ) FORWARD.

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

DEFINE BUTTON btnBrowseFolder 
     LABEL "Select Log Folder" 
     SIZE 26 BY 1 TOOLTIP "Browse for Log Folder".

DEFINE BUTTON btnBrowseFolder-2 
     LABEL "Select Rpt Folder" 
     SIZE 26 BY 1 TOOLTIP "Browse for Log Folder".

DEFINE VARIABLE fiCSV AS CHARACTER FORMAT "X(256)":U 
     LABEL "CSV File" 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1 NO-UNDO.

DEFINE VARIABLE fiReport AS CHARACTER FORMAT "X(256)":U 
     LABEL "Report File" 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113 BY 9.52.

DEFINE VARIABLE tgCsvExport AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fiReport AT ROW 5.52 COL 15 COLON-ALIGNED WIDGET-ID 2
     btnBrowseFolder-2 AT ROW 5.52 COL 86 WIDGET-ID 24
     fiCSV AT ROW 7.19 COL 15 COLON-ALIGNED WIDGET-ID 4
     btnBrowseFolder AT ROW 7.19 COL 86 WIDGET-ID 22
     tgCsvExport AT ROW 7.33 COL 3.2 WIDGET-ID 26
     btn-process AT ROW 11.48 COL 20
     btn-cancel AT ROW 11.48 COL 51
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.95 COL 4
     RECT-17 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114 BY 13.1.


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
         TITLE              = "Duplicate Tag Locations Report"
         HEIGHT             = 13.19
         WIDTH              = 114.4
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 114.4
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 114.4
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Duplicate Tag Locations Report */
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
ON WINDOW-CLOSE OF C-Win /* Duplicate Tag Locations Report */
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
        v-process  = NO.

        MESSAGE "Are you sure you want to" TRIM(c-win:TITLE)
            "within the selected parameters?"       
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE v-process.

        IF v-process THEN RUN run-process.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBrowseFolder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBrowseFolder C-Win
ON CHOOSE OF btnBrowseFolder IN FRAME FRAME-A /* Select Log Folder */
DO:
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
        cFileName = fnGetFileName(fiCsv:SCREEN-VALUE).
        RUN pFolderBrowse(fiCsv:HANDLE, "").  
        fiCsv:SCREEN-VALUE = fiCsv:SCREEN-VALUE + "\" + cFileName.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBrowseFolder-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBrowseFolder-2 C-Win
ON CHOOSE OF btnBrowseFolder-2 IN FRAME FRAME-A /* Select Rpt Folder */
DO:
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
        cFileName = fnGetFileName(fiReport:SCREEN-VALUE).
        RUN pFolderBrowse(fiReport:HANDLE, "").  
        fiReport:SCREEN-VALUE = fiReport:SCREEN-VALUE + "\" + cFileName.
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

    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    RUN enable_UI.
    fiCSV:SCREEN-VALUE = "c:\tmp\" + fnDefaultRptName("CSV").
    fiReport:SCREEN-VALUE = "c:\tmp\" + fnDefaultRptName("Rpt").
    {methods/nowait.i}
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
  DISPLAY fiReport fiCSV tgCsvExport 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 fiReport btnBrowseFolder-2 fiCSV btnBrowseFolder tgCsvExport 
         btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFolderBrowse C-Win 
PROCEDURE pFolderBrowse :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphdFolderEntry AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFolder  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOK      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cDefault AS CHARACTER NO-UNDO.
        
    CASE ipcType:
        WHEN "Rpt" THEN
            DO:
                SYSTEM-DIALOG GET-DIR cFolder 
                    TITLE "Select Folder for report File"
                    UPDATE lOK.
            END.
        OTHERWISE
        DO:
            SYSTEM-DIALOG GET-DIR cFolder 
                TITLE "Select Folder for Log File"
                UPDATE lOK.
        END.  
    END CASE.
    IF lOK THEN iphdFolderEntry:SCREEN-VALUE = cFolder.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
SESSION:SET-WAIT-STATE("General").


    SESSION:SET-WAIT-STATE("").
    DEFINE VARIABLE iRecs AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotRecs AS INTEGER NO-UNDO.
    DEFINE VARIABLE iDups AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-fg-bin FOR fg-bin.
    
    DO WITH FRAME {&frame-name}:
        ASSIGN fiCsv fiReport tgCsvExport.
    END.
    ASSIGN fiReport = fnConvertSlash(fiReport)
           fiCsv    = fnConvertSlash(fiCsv)
           .
    OUTPUT stream sRpt to VALUE(fiReport).
    IF tgCsvExport THEN 
        OUTPUT stream sCsv to VALUE(fiCsv).
        
    PAUSE 0 BEFORE-HIDE.
    IF tgCsvExport THEN 
        EXPORT STREAM sCsv DELIMITER "," "Company" "Item" "Tag" "Loc1" "Bin1" "Qty1" "Loc2" "Bin2" "Qty2".
    FOR EACH fg-bin NO-LOCK 
         WHERE fg-bin.tag GT "" USE-INDEX tag.

        iRecs = iRecs + 1.
        IF iRecs GT 4999 THEN 
        DO:
            iTotRecs = iTotRecs + iRecs.
            iRecs = 0.
            STATUS DEFAULT STRING(iTotRecs).
        END.
  
        IF fg-bin.qty EQ 0 THEN NEXT.
  
        FIND FIRST bf-fg-bin NO-LOCK
            WHERE bf-fg-bin.company EQ fg-bin.company
              AND bf-fg-bin.tag     EQ fg-bin.tag
              AND bf-fg-bin.qty     GT 0
              AND  (   bf-fg-bin.loc     NE fg-bin.loc 
                    OR bf-fg-bin.loc-bin NE fg-bin.loc-bin)
            NO-ERROR.

        IF AVAILABLE bf-fg-bin THEN 
        DO:
            iDups = iDups + 1.
            DISPLAY STREAM sRpt 
                fg-bin.company COLUMN-LABEL "Company"
                fg-bin.i-no FORMAT "x(22)"
                fg-bin.tag  FORMAT "x(27)" COLUMN-LABEL "Tag"
                fg-bin.loc  COLUMN-LABEL "Loc1"
                fg-bin.loc-bin COLUMN-LABEL "Bin1"
                fg-bin.qty COLUMN-LABEL "Qty1" FORMAT "->>>>>>,>>9.9<<<<<"
                bf-fg-bin.loc  COLUMN-LABEL "Loc2"
                bf-fg-bin.loc-bin COLUMN-LABEL "Bin2"
                bf-fg-bin.qty COLUMN-LABEL "Qty2" FORMAT "->>>>>>,>>9.9<<<<<"
                WITH WIDTH 200 STREAM-IO. 
            IF tgCsvExport THEN
              EXPORT STREAM sCsv DELIMITER ","  
                    fg-bin.company       
                    fg-bin.i-no
                    fg-bin.tag
                    fg-bin.loc
                    fg-bin.loc-bin
                    fg-bin.qty
                    bf-fg-bin.loc
                    bf-fg-bin.loc-bin
                    bf-fg-bin.qty
                    .                    
        END.
    
    END.

    OUTPUT STREAM sRpt CLOSE.
    OUTPUT STREAM sCsv CLOSE.
    
    &IF DEFINED(FWD-VERSION) > 0 &THEN
        open-mime-resource "text/plain" string("file:///" + fiReport) false.
    &ELSE
        OS-COMMAND NO-WAIT NOTEPAD VALUE(fiReport).
    &ENDIF    
    
    MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.

    APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnConvertSlash C-Win
FUNCTION fnConvertSlash RETURNS CHARACTER 
  ( ipcPath AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
		DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
        IF OPSYS EQ "Win32" THEN 
           cResult = REPLACE(ipcPath, "/","\").
        ELSE 
           cResult = REPLACE(ipcPath, "\","/").
		RETURN cResult.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnDefaultRptName C-Win 
FUNCTION fnDefaultRptName RETURNS CHARACTER
    (INPUT ipcType AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFileName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDateAsChar AS CHARACTER NO-UNDO.
    cDateAsChar = STRING(DAY(TODAY),"99") + STRING(MONTH(TODAY),"99") + STRING(YEAR(TODAY), "9999"). 
        
    CASE ipcType:
        WHEN "Rpt" THEN 
            cFileName = "DuplicateTagLocations" + cDateAsChar + ".txt".
        WHEN "csv" THEN 
            cFileName = "DuplicateTagLocations" + cDateAsChar + ".csv".
    END CASE.
        
    RETURN cFileName.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnGetFileName C-Win 
FUNCTION fnGetFileName RETURNS CHARACTER
    (INPUT ipcFullPath AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPosition AS INTEGER   NO-UNDO.
    iPosition = R-INDEX(ipcFullPath, "\").
    IF iPosition EQ 0 THEN 
        iPosition = R-INDEX(ipcFullPath, "/").
    cFileName = ipcFullPath.
    IF iPosition GT 0 THEN 
        cFileName = SUBSTRING(ipcFullPath, iPosition + 1).
    RETURN cFileName.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

