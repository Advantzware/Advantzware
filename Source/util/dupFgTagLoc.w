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
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_i-no end_i-no begin_date ~
end_date tb_zero-qty t-receipt t-adj t-count t-ship t-ret t-trans tgIssue ~
fiReport btnBrowseFolder-2 fiCSV btnBrowseFolder tgCsvExport btn-process ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_i-no end_i-no begin_date end_date ~
tb_zero-qty v-trans-lbl t-receipt t-adj t-count t-ship t-ret t-trans ~
tgIssue fiReport fiCSV tgCsvExport 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Beginning Trans Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/2099 
     LABEL "Ending Trans Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiCSV AS CHARACTER FORMAT "X(256)":U 
     LABEL "CSV File" 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1 NO-UNDO.

DEFINE VARIABLE fiReport AS CHARACTER FORMAT "X(256)":U 
     LABEL "Report File" 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1 NO-UNDO.

DEFINE VARIABLE v-trans-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "Transaction Types" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .91
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113 BY 14.29.

DEFINE VARIABLE t-adj AS LOGICAL INITIAL no 
     LABEL "Adjustments" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-count AS LOGICAL INITIAL no 
     LABEL "Counts" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-receipt AS LOGICAL INITIAL no 
     LABEL "Receipts" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-ret AS LOGICAL INITIAL no 
     LABEL "Credit Returns" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-ship AS LOGICAL INITIAL no 
     LABEL "Shipments" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-trans AS LOGICAL INITIAL no 
     LABEL "Transfers" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE tb_zero-qty AS LOGICAL INITIAL no 
     LABEL "Include Zero Quantity?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE tgCsvExport AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE tgIssue AS LOGICAL INITIAL no 
     LABEL "Issue Farm Outs" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_i-no AT ROW 2.52 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number" WIDGET-ID 28
     end_i-no AT ROW 2.52 COL 80.8 COLON-ALIGNED HELP
          "Enter Ending Item Number" WIDGET-ID 30
     begin_date AT ROW 3.71 COL 27 COLON-ALIGNED WIDGET-ID 32
     end_date AT ROW 3.71 COL 80.8 COLON-ALIGNED WIDGET-ID 34
     tb_zero-qty AT ROW 5.33 COL 29 WIDGET-ID 36
     v-trans-lbl AT ROW 7 COL 29 NO-LABEL WIDGET-ID 50
     t-receipt AT ROW 7.95 COL 29 WIDGET-ID 40
     t-adj AT ROW 8.05 COL 53.6 WIDGET-ID 38
     t-count AT ROW 8.05 COL 77.6 WIDGET-ID 52
     t-ship AT ROW 8.81 COL 29 WIDGET-ID 44
     t-ret AT ROW 8.91 COL 53.6 WIDGET-ID 42
     t-trans AT ROW 9.67 COL 29 WIDGET-ID 46
     tgIssue AT ROW 9.71 COL 53.6 WIDGET-ID 48
     fiReport AT ROW 11.86 COL 15 COLON-ALIGNED WIDGET-ID 2
     btnBrowseFolder-2 AT ROW 11.86 COL 86 WIDGET-ID 24
     fiCSV AT ROW 13.52 COL 15 COLON-ALIGNED WIDGET-ID 4
     btnBrowseFolder AT ROW 13.52 COL 86 WIDGET-ID 22
     tgCsvExport AT ROW 13.67 COL 3.2 WIDGET-ID 26
     btn-process AT ROW 16.1 COL 20
     btn-cancel AT ROW 16.1 COL 51
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.57 COL 4
     RECT-17 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114 BY 17.24.


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
         TITLE              = "Duplicate FG Tag Locations Report"
         HEIGHT             = 17.48
         WIDTH              = 114.4
         MAX-HEIGHT         = 32.52
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 32.52
         VIRTUAL-WIDTH      = 273.2
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
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-adj:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-count:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-receipt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-ret:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-ship:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-trans:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_zero-qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tgIssue:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN v-trans-lbl IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       v-trans-lbl:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Duplicate FG Tag Locations Report */
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
ON WINDOW-CLOSE OF C-Win /* Duplicate FG Tag Locations Report */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Trans Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning FG Item# */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Trans Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending FG Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-adj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-adj C-Win
ON VALUE-CHANGED OF t-adj IN FRAME FRAME-A /* Adjustments */
DO:
      ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-count
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-count C-Win
ON VALUE-CHANGED OF t-count IN FRAME FRAME-A /* Counts */
DO:
      ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-receipt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-receipt C-Win
ON VALUE-CHANGED OF t-receipt IN FRAME FRAME-A /* Receipts */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-ship
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-ship C-Win
ON VALUE-CHANGED OF t-ship IN FRAME FRAME-A /* Shipments */
DO:
      ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-trans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-trans C-Win
ON VALUE-CHANGED OF t-trans IN FRAME FRAME-A /* Transfers */
DO:
      ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_zero-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zero-qty C-Win
ON VALUE-CHANGED OF tb_zero-qty IN FRAME FRAME-A /* Include Zero Quantity? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgIssue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgIssue C-Win
ON VALUE-CHANGED OF tgIssue IN FRAME FRAME-A /* Issue Farm Outs */
DO:
  ASSIGN {&self-name}.
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
    
    {custom/usrprint.i}
    
    fiCSV:SCREEN-VALUE = "c:\tmp\" + fnDefaultRptName("CSV").
    fiReport:SCREEN-VALUE = "c:\tmp\" + fnDefaultRptName("Rpt").
    tb_zero-qty:SCREEN-VALUE = "No" .
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
  DISPLAY begin_i-no end_i-no begin_date end_date tb_zero-qty v-trans-lbl 
          t-receipt t-adj t-count t-ship t-ret t-trans tgIssue fiReport fiCSV 
          tgCsvExport 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_i-no end_i-no begin_date end_date tb_zero-qty t-receipt 
         t-adj t-count t-ship t-ret t-trans tgIssue fiReport btnBrowseFolder-2 
         fiCSV btnBrowseFolder tgCsvExport btn-process btn-cancel 
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
    DEFINE VARIABLE iRecs    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotRecs AS INTEGER NO-UNDO.
    DEFINE VARIABLE iDups    AS INTEGER NO-UNDO.
    DEFINE VARIABLE cRitaCodeList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lDataFound AS LOGICAL NO-UNDO.    
    DEFINE VARIABLE cRitaCode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTransDate AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-fg-bin FOR fg-bin.
    
    DO WITH FRAME {&frame-name}:
        ASSIGN fiCsv fiReport tgCsvExport.
        ASSIGN {&DISPLAYED-OBJECTS}.
           .
       cRitaCodeList   = (IF t-receipt THEN "R," ELSE "") +                 
               (IF t-ship THEN "S," ELSE "") +
               (IF t-trans THEN "T," ELSE "") +
               (IF t-adj THEN "A," ELSE "") +
               (IF t-ret THEN "E," ELSE "") +
               (IF tgIssue THEN "F," ELSE "") +
               (IF t-count THEN "C," ELSE "") .
               
    END.
    IF LENGTH(cRitaCodeList) GT 0 AND
    SUBSTR(cRitaCodeList,LENGTH(cRitaCodeList),1) EQ "," THEN
    SUBSTR(cRitaCodeList,LENGTH(cRitaCodeList),1) = "".
    
    ASSIGN 
        fiReport = fnConvertSlash(fiReport)
        fiCsv    = fnConvertSlash(fiCsv)
        .
    OUTPUT stream sRpt to VALUE(fiReport).
    IF tgCsvExport THEN 
        OUTPUT stream sCsv to VALUE(fiCsv).
        
    PAUSE 0 BEFORE-HIDE.
    IF tgCsvExport THEN 
        EXPORT STREAM sCsv DELIMITER "," "Company" "FGItem" "Tag" "Loc" "Bin" "Qty" "Trans Date" "Trans Type".

    FOR EACH  fg-bin NO-LOCK 
        WHERE fg-bin.company EQ g_company
          AND fg-bin.tag     GT ""
          AND fg-bin.i-no    GE begin_i-no
          AND fg-bin.i-no    LE end_i-no
        USE-INDEX tag: 
        iRecs = iRecs + 1.
        IF iRecs GT 4999 THEN 
        DO:
            iTotRecs = iTotRecs + iRecs.
            iRecs = 0.
            STATUS DEFAULT STRING(iTotRecs).
        END.
        
         IF NOT tb_zero-qty AND fg-bin.qty EQ 0 THEN 
            NEXT.
        
        lDataFound = NO .
        cRitaCode = "".
        cTransDate = "" .
        FOR EACH fg-rcpth  FIELDS(trans-date rita-code)
        WHERE fg-rcpth.company   EQ fg-bin.company
          AND fg-rcpth.i-no      EQ fg-bin.i-no
          AND fg-rcpth.job-no    EQ fg-bin.job-no
          AND fg-rcpth.job-no2   EQ fg-bin.job-no2
          AND fg-rcpth.trans-date GE begin_date
          AND fg-rcpth.trans-date LE end_date
          AND LOOKUP(fg-rcpth.rita-code,cRitaCodeList) NE 0
         NO-LOCK, 
        EACH fg-rdtlh
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
          AND fg-rdtlh.loc       EQ fg-bin.loc    
          AND fg-rdtlh.loc-bin   EQ fg-bin.loc-bin
          AND fg-rdtlh.tag       EQ fg-bin.tag            
         NO-LOCK BY fg-rcpth.trans-date DESC  :
            lDataFound = YES .
            cTransDate = string(fg-rcpth.trans-date) .
            cRitaCode = fg-rcpth.rita-code .
            LEAVE.        
        END.
        IF cTransDate EQ ? THEN cTransDate = "".
        IF NOT lDataFound THEN NEXT.

        FOR EACH bf-fg-bin NO-LOCK
            WHERE bf-fg-bin.company   EQ fg-bin.company
              AND bf-fg-bin.tag       EQ fg-bin.tag
              AND ((bf-fg-bin.qty       NE 0 AND NOT tb_zero-qty) OR tb_zero-qty)
              AND  (bf-fg-bin.loc     NE fg-bin.loc OR 
                    bf-fg-bin.loc-bin NE fg-bin.loc-bin)
              BREAK BY bf-fg-bin.tag:
        
            iDups = iDups + 1.
            
            IF FIRST-OF(bf-fg-bin.tag) THEN
                DISPLAY STREAM sRpt  
                    fg-bin.company COLUMN-LABEL "Company"
                    fg-bin.i-no    COLUMN-LABEL "FGItem" FORMAT "x(22)"
                    fg-bin.tag     COLUMN-LABEL "Tag"    FORMAT "x(27)"
                    fg-bin.loc     COLUMN-LABEL "Loc" 
                    fg-bin.loc-bin COLUMN-LABEL "Bin" 
                    fg-bin.qty     COLUMN-LABEL "Qty"    FORMAT "->>>>>>,>>9.9<<<<<"
                    cTransDate    COLUMN-LABEL "Trans Date"    FORMAT "x(10)" 
                    cRitaCode    COLUMN-LABEL "Trans Type"    FORMAT "x(6)" 
                    WITH FRAME FirstLine WIDTH 200 STREAM-IO.
            
            DISPLAY STREAM sRpt 
                 bf-fg-bin.loc     NO-LABEL AT 60 
                 bf-fg-bin.loc-bin NO-LABEL  
                 bf-fg-bin.qty     NO-LABEL FORMAT "->>>>>>,>>9.9<<<<<"
                 WITH FRAME NextLine WIDTH 200 STREAM-IO NO-BOX.
              
            IF tgCsvExport THEN DO:
                IF FIRST-OF(bf-fg-bin.tag) THEN
                    EXPORT STREAM sCsv DELIMITER ","  
                           fg-bin.company       
                           fg-bin.i-no
                           fg-bin.tag
                           fg-bin.loc
                           fg-bin.loc-bin
                           fg-bin.qty
                           cTransDate
                           cRitaCode
                           SKIP
                            . 
                EXPORT STREAM sCsv DELIMITER "," 
                     "" /* Company */
                     "" /* Item no */
                     "" /* Tag */
                     bf-fg-bin.loc
                     bf-fg-bin.loc-bin
                     bf-fg-bin.qty SKIP
                     . 
            END.
                                   
        END.
    
    END.

    OUTPUT STREAM sRpt CLOSE.
    OUTPUT STREAM sCsv CLOSE.
    
    &IF DEFINED(FWD-VERSION) > 0 &THEN
        open-mime-resource "text/plain" string("file:///" + fiReport) false.
    &ELSE
        OS-COMMAND NO-WAIT NOTEPAD VALUE(fiReport).
    &ENDIF    
    
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    
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
            cFileName = "DuplicateFGTagLocations" + cDateAsChar + ".txt".
        WHEN "csv" THEN 
            cFileName = "DuplicateFGTagLocations" + cDateAsChar + ".csv".
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

