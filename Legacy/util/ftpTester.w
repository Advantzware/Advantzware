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

DEFINE VARIABLE v-process AS LOG           NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-18 fiTestNum fiFtpSite fiFormat ~
fiFolder fiFileSpec tgSilent tgExecute fiURL fiUser fiPassword fiMode fiDir ~
fiCmd fiSoftware fiScript fiBinary fiCommandFile edFullCmd btn-process ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fiTestNum fiFtpSite fiFormat fiFolder ~
fiFileSpec tgSilent tgExecute fiURL fiUser fiPassword fiMode fiDir fiCmd ~
fiSoftware fiScript fiBinary fiCommandFile edFullCmd 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE VARIABLE edFullCmd AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 86 BY 4.76 NO-UNDO.

DEFINE VARIABLE fiBinary AS CHARACTER FORMAT "X(256)":U 
     LABEL "Binary or ASCII" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiCmd AS CHARACTER FORMAT "X(256)":U 
     LABEL "Command" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiCommandFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "Command File" 
     VIEW-AS FILL-IN 
     SIZE 68 BY 1 NO-UNDO.

DEFINE VARIABLE fiDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Directory" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE fiFileSpec AS CHARACTER FORMAT "X(256)":U 
     LABEL "File Spec" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiFolder AS CHARACTER FORMAT "X(256)":U 
     LABEL "Folder" 
     VIEW-AS FILL-IN 
     SIZE 70 BY 1 NO-UNDO.

DEFINE VARIABLE fiFormat AS CHARACTER FORMAT "X(256)":U 
     LABEL "Format" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiFtpSite AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ftp Site" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiMode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mode" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiPassword AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiScript AS CHARACTER FORMAT "X(256)":U 
     LABEL "Script" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiSoftware AS CHARACTER FORMAT "X(256)":U 
     LABEL "Software" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiTestNum AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Test Num" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "1=show config,2=Create Script,3=WriteScript,4=Do Ftp" NO-UNDO.

DEFINE VARIABLE fiURL AS CHARACTER FORMAT "X(256)":U 
     LABEL "URL" 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1 NO-UNDO.

DEFINE VARIABLE fiUser AS CHARACTER FORMAT "X(256)":U 
     LABEL "User" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 6.43.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 13.81.

DEFINE VARIABLE tgExecute AS LOGICAL INITIAL no 
     LABEL "Execute Ftp?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tgSilent AS LOGICAL INITIAL no 
     LABEL "Silent?" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fiTestNum AT ROW 2.19 COL 12 COLON-ALIGNED WIDGET-ID 4
     fiFtpSite AT ROW 3.14 COL 36 COLON-ALIGNED WIDGET-ID 8
     fiFormat AT ROW 3.19 COL 12 COLON-ALIGNED WIDGET-ID 6
     fiFolder AT ROW 4.1 COL 12 COLON-ALIGNED WIDGET-ID 10
     fiFileSpec AT ROW 5.05 COL 12 COLON-ALIGNED WIDGET-ID 12
     tgSilent AT ROW 6.1 COL 14 WIDGET-ID 20
     tgExecute AT ROW 6.1 COL 29 WIDGET-ID 22
     fiURL AT ROW 8.86 COL 17 COLON-ALIGNED WIDGET-ID 26
     fiUser AT ROW 9.81 COL 17 COLON-ALIGNED WIDGET-ID 28
     fiPassword AT ROW 9.86 COL 52 COLON-ALIGNED WIDGET-ID 30
     fiMode AT ROW 10.76 COL 17 COLON-ALIGNED WIDGET-ID 32
     fiDir AT ROW 10.81 COL 52 COLON-ALIGNED WIDGET-ID 34
     fiCmd AT ROW 11.71 COL 17 COLON-ALIGNED WIDGET-ID 36
     fiSoftware AT ROW 11.76 COL 52 COLON-ALIGNED WIDGET-ID 38
     fiScript AT ROW 12.71 COL 17 COLON-ALIGNED WIDGET-ID 40
     fiBinary AT ROW 12.76 COL 52 COLON-ALIGNED WIDGET-ID 42
     fiCommandFile AT ROW 13.71 COL 17 COLON-ALIGNED WIDGET-ID 44
     edFullCmd AT ROW 15.52 COL 3.2 NO-LABEL WIDGET-ID 48
     btn-process AT ROW 22.67 COL 18
     btn-cancel AT ROW 22.67 COL 50
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1 COL 4
     RECT-17 AT ROW 1.24 COL 1.8
     RECT-18 AT ROW 8.38 COL 2 WIDGET-ID 24
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 91 BY 23.48.


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
         TITLE              = "Ftp Tester"
         HEIGHT             = 23.48
         WIDTH              = 91
         MAX-HEIGHT         = 23.48
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 23.48
         VIRTUAL-WIDTH      = 98.2
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
ON END-ERROR OF C-Win /* Ftp Tester */
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
ON WINDOW-CLOSE OF C-Win /* Ftp Tester */
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

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.

        MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) +
            " for the selected parameters?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

        IF v-process THEN RUN run-process.
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
  DISPLAY fiTestNum fiFtpSite fiFormat fiFolder fiFileSpec tgSilent tgExecute 
          fiURL fiUser fiPassword fiMode fiDir fiCmd fiSoftware fiScript 
          fiBinary fiCommandFile edFullCmd 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 RECT-18 fiTestNum fiFtpSite fiFormat fiFolder fiFileSpec 
         tgSilent tgExecute fiURL fiUser fiPassword fiMode fiDir fiCmd 
         fiSoftware fiScript fiBinary fiCommandFile edFullCmd btn-process 
         btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DEFINE VARIABLE ipcCompany   AS CHARACTER NO-UNDO INIT '001'.
    DEFINE VARIABLE ipctestNum   AS INTEGER   NO-UNDO INIT 4.
    DEFINE VARIABLE ipcFormat    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ipcFtpSite   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ipcFolder    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ipcFileSpec  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iplSilent    AS LOGICAL   NO-UNDO INIT NO.
    DEFINE VARIABLE iplExecute   AS LOGICAL   NO-UNDO INIT NO.


    DEFINE VARIABLE ftpURL       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpUser      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpPassword  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpMode      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpDir       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpGet       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpSoftware  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpScript    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ftpBinary    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCommandFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFullCmd     AS CHARACTER NO-UNDO.  
    
    DO WITH FRAME {&FRAME-NAME}: 
        ASSIGN
            fiTestNum
            fiFormat
            fiFtpSite
            fiFolder
            fiFileSpec
            tgSilent
            tgExecute
            .
    END.
    
    RUN util/testFtp.p
        (cocode,
        fiTestNum,
        fiFormat,
        fiFtpSite,
        fiFolder,
        fiFileSpec,
        tgSilent,
        tgExecute,
        OUTPUT fiUrl,
        OUTPUT fiUser,
        OUTPUT fiPassword,
        OUTPUT fiMode,
        OUTPUT fiDir,
        OUTPUT fiCmd,
        OUTPUT fiSoftware,
        OUTPUT fiScript,
        OUTPUT fiBinary,
        OUTPUT fiCommandFile,
        OUTPUT cFullCmd). 
      
    DISPLAY fiUrl
        fiUser
        fiPassword
        fiMode
        fiDir
        fiCmd
        fiSoftware
        fiScript
        fiBinary
        fiCommandFile
        WITH FRAME {&FRAME-NAME}
        .
      
    edFullCmd:screen-value = cFullCmd.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

