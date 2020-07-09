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
DEFINE VARIABLE cConnectString AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vhWebService AS HANDLE NO-UNDO.
DEFINE VARIABLE vhSalesSoap AS HANDLE NO-UNDO.
DEFINE VARIABLE parameters1 AS LONGCHAR NO-UNDO.
DEFINE VARIABLE fr-title  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCurrentVersion AS INTEGER NO-UNDO.
DEFINE VARIABLE iNewVersion AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS eInstructions currentVersion newVersion ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS eInstructions currentVersion newVersion 

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
     LABEL "Update" 
     SIZE 18 BY 1.14
     FONT 6.

DEFINE VARIABLE eInstructions AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 91 BY 4.52 NO-UNDO.

DEFINE VARIABLE currentVersion AS CHARACTER FORMAT "X(10)" 
     LABEL "Current Release Version" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1.

DEFINE VARIABLE newVersion AS CHARACTER FORMAT "X(10)" 
     LABEL "New Release Version" 
     VIEW-AS FILL-IN 
     SIZE 37.2 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     eInstructions AT ROW 1.48 COL 4 NO-LABEL NO-TAB-STOP 
     currentVersion AT ROW 6.71 COL 36 COLON-ALIGNED HELP
          "Current Version" NO-TAB-STOP 
     newVersion AT ROW 8.38 COL 36 COLON-ALIGNED HELP
          "New Version"
     btn-process AT ROW 11 COL 26
     btn-cancel AT ROW 11 COL 57
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.8 BY 12.14.


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
         TITLE              = "Update Version"
         HEIGHT             = 12.24
         WIDTH              = 96.8
         MAX-HEIGHT         = 26.62
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 26.62
         VIRTUAL-WIDTH      = 160
         RESIZE             = no
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

ASSIGN 
       currentVersion:READ-ONLY IN FRAME FRAME-A        = TRUE
       currentVersion:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       eInstructions:READ-ONLY IN FRAME FRAME-A        = TRUE.

ASSIGN 
       newVersion:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Update Version */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Update Version */
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
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Update */
DO:
    DEF VAR lProcess AS LOG INITIAL TRUE. 
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&DISPLAYED-OBJECTS}.
    END.
    
    IF newVersion:SCREEN-VALUE = "" THEN DO:
        MESSAGE 
            "You must enter a value for New Version."
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    
    IF newVersion:SCREEN-VALUE LT currentVersion:SCREEN-VALUE THEN DO:
        ASSIGN 
            lProcess = FALSE.
        MESSAGE 
            "You are trying to set the version LESS THAN the existing version.  Are you sure?"
            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE lProcess.
    END.
    ELSE DO:
        MESSAGE 
            "About to update the current released ASI version from" SKIP 
            currentVersion:SCREEN-VALUE + " to " + newVersion:SCREEN-VALUE SKIP 
            "Are you sure?"  
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE lProcess.
    END.

    IF lProcess THEN RUN pUpdateVersion.
    
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

    ASSIGN 
        eInstructions:SCREEN-VALUE = 
            "Use this function to update the currently published version of the 'ASI Latest Release'." + CHR(10) + 
            "This value is used when a customer is attempting to upgrade their system from any " +
            "existing release to the latest published version.".
            
    FIND FIRST sys-ctrl NO-LOCK WHERE
        sys-ctrl.company EQ "001" AND  
        sys-ctrl.name EQ "AsiHelpService"
        NO-ERROR.
    IF NOT AVAIL sys-ctrl THEN DO:
        MESSAGE 
            "Unable to locate the 'AsiHelpService' NK1." SKIP 
            "Please correct and try again."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    ELSE ASSIGN 
        cConnectString = sys-ctrl.char-fld.

    CREATE SERVER vhWebService.
    vhWebService:CONNECT(cConnectString) NO-ERROR.
    
    IF NOT vhWebService:CONNECTED() THEN DO:
        MESSAGE 
            "Unable to connect to the Help Service with string " SKIP 
            cConnectString SKIP 
            "Please correct and try again."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    RUN Service1Soap SET vhSalesSoap ON vhWebService .
    RUN HelpVersion IN vhSalesSoap( OUTPUT parameters1).

    DO WITH FRAME {&frame-name}:
        ASSIGN 
            currentVersion:SCREEN-VALUE = parameters1
            currentVersion = parameters1
            currentVersion:SENSITIVE = NO .
        APPLY "entry" TO newVersion.
    END.

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
  DISPLAY eInstructions currentVersion newVersion 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE eInstructions currentVersion newVersion btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateVersion C-Win 
PROCEDURE pUpdateVersion :
DO TRANSACTION WITH FRAME {&FRAME-NAME}:
        RUN Service1Soap SET vhSalesSoap ON vhWebService .
        RUN VersionUpdate  IN vhSalesSoap("",INPUT STRING(currentVersion:SCREEN-VALUE),INPUT STRING(newVersion:SCREEN-VALUE),  OUTPUT parameters1).
        ASSIGN 
            currentVersion:SCREEN-VALUE = newVersion:SCREEN-VALUE 
            newVersion:SCREEN-VALUE = ""
            currentVersion
            newVersion.
        MESSAGE
            "Latest release updated to " + newVersion:SCREEN-VALUE
            VIEW-AS ALERT-BOX.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

