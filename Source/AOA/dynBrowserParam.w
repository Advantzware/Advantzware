&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: AOA/dynBrowserParam.w

  Description: Dynamic Browser Parameter

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 4.7.2020

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures  
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&Scoped-define program-id dynBrowserParam.
&Scoped-define defaultUser _default

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipcPrgmName   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iprRowID      AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER iplParameters AS LOGICAL   NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cPoolName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrgmName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTitle       AS CHARACTER NO-UNDO.
DEFINE VARIABLE hAppSrvBin   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hJasper      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQueryBrowse AS HANDLE    NO-UNDO.
DEFINE VARIABLE lAdmin       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lSave        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
DEFINE VARIABLE queryStr     AS CHARACTER NO-UNDO.

cPrgmName = ipcPrgmName.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{AOA/tempTable/ttDynAction.i}

RUN AOA/appServer/aoaBin.p PERSISTENT SET hAppSrvBin.
SESSION:ADD-SUPER-PROCEDURE (hAppSrvBin).
RUN AOA/spJasper.p PERSISTENT SET hJasper.
SESSION:ADD-SUPER-PROCEDURE (hJasper).

{methods/lockWindowUpdate.i}

/* function fDateOptions */
{AOA/includes/fDateOptions.i}
/* function fDateOptionValue */
{AOA/includes/fDateOptionValue.i}

{AOA/includes/dynFuncs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME paramFrame

/* Custom List Definitions                                              */
/* UserIDObjects,List-2,List-3,List-4,List-5,List-6                     */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnOK 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.png":U NO-FOCUS FLAT-BUTTON
     LABEL "OK" 
     SIZE 8 BY 1.91 TOOLTIP "OK"
     BGCOLOR 8 .

DEFINE BUTTON btnUsers 
     IMAGE-UP FILE "Graphics/32x32/user-alert.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Users" 
     SIZE 8 BY 1.91 TOOLTIP "Link Dynamic Parameters to User(s)"
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-OK
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.29.

DEFINE RECTANGLE RECT-USERS
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME paramFrame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME FRAME-OK
     btnUsers AT ROW 1.24 COL 2 HELP
          "Link Dynamic Parameters to User(s)" WIDGET-ID 8
     btnOK AT ROW 1.24 COL 12 WIDGET-ID 2
     RECT-OK AT ROW 1 COL 11 WIDGET-ID 4
     RECT-USERS AT ROW 1 COL 1 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 138 ROW 1
         SIZE 20.8 BY 2.38
         FGCOLOR 1  WIDGET-ID 200.


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
         TITLE              = "Dynamic Browser Parameters"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 28.57
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 28.57
         VIRTUAL-WIDTH      = 160
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
IF NOT C-Win:LOAD-ICON("Graphics/32x32/jss_icon_32.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics/32x32/jss_icon_32.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-OK:FRAME = FRAME paramFrame:HANDLE.

/* SETTINGS FOR FRAME FRAME-OK
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-OK IN FRAME FRAME-OK
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-USERS IN FRAME FRAME-OK
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME paramFrame
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME paramFrame
/* Query rebuild information for FRAME paramFrame
     _Query            is NOT OPENED
*/  /* FRAME paramFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Dynamic Browser Parameters */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Dynamic Browser Parameters */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN pDeleteProcedure.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-OK
&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK C-Win
ON CHOOSE OF btnOK IN FRAME FRAME-OK /* OK */
DO:
    RUN pSaveDynParamValues ("Grid").
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUsers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUsers C-Win
ON CHOOSE OF btnUsers IN FRAME FRAME-OK /* Users */
DO:
    RUN pSaveDynParamValues ("Grid").
    RUN AOA/dynBrowserParamUsers.w (
        dynParamValue.subjectID,
        dynParamValue.prgmName,
        ROWID(dynParamValue)
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME paramFrame
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

{AOA/includes/dynProcs.i "dyn"}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  ASSIGN
    lAdmin             = DYNAMIC-FUNCTION("sfIsUserAdmin")
    RECT-USERS:HIDDEN  = NOT lAdmin
    btnUsers:HIDDEN    = NOT lAdmin
    btnUsers:SENSITIVE = lAdmin
    .
  RUN pCreateDynParameters (FRAME paramFrame:HANDLE, YES).
  IF iplParameters EQ NO THEN DO:
      APPLY "CHOOSE":U TO btnOK.
      RETURN NO-APPLY.
  END. /* if iplparameters */
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
  VIEW FRAME paramFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-paramFrame}
  ENABLE btnUsers btnOK 
      WITH FRAME FRAME-OK IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-OK}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteProcedure C-Win 
PROCEDURE pDeleteProcedure :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   IF VALID-HANDLE(hAppSrvBin) THEN
   DELETE PROCEDURE hAppSrvBin.
   IF VALID-HANDLE(hJasper) THEN
   DELETE PROCEDURE hJasper.
   IF VALID-HANDLE(hDynCalcField) THEN
   DELETE PROCEDURE hDynCalcField.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDynParamValue C-Win 
PROCEDURE pGetDynParamValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST dynParamValue NO-LOCK
         WHERE ROWID(dynParamValue) EQ iprRowID
         NO-ERROR.
    IF AVAILABLE dynParamValue THEN DO:
        FIND FIRST dynSubject NO-LOCK
             WHERE dynSubject.subjectID EQ dynParamValue.subjectID
             NO-ERROR.
        IF AVAILABLE dynSubject THEN
        {&WINDOW-NAME}:TITLE = dynSubject.subjectTitle + " "
                             + {&WINDOW-NAME}:TITLE
                             + " [Subject ID: "
                             + STRING(dynSubject.subjectID) + "]"
                             .
    END. /* if avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

