&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: dynVisibleSets.w

  Description: Show/Hide Parameter Sets

  Input Parameters: ROWID dynParamValue

  Output Parameters: Save logical

  Author: Ron Stark

  Created: 3.11.2019
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT  PARAMETER iprRowID AS ROWID   NO-UNDO.
DEFINE OUTPUT PARAMETER oplSave  AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE ttParamSet NO-UNDO
    FIELD sortOrder    AS INTEGER
    FIELD paramSetID LIKE dynParamSet.paramSetID
    FIELD setName    LIKE dynParamSet.setName
    FIELD setTitle   LIKE dynParamSet.setTitle
    FIELD isVisible    AS LOGICAL LABEL "Visible"
    FIELD rRowID       AS ROWID
        INDEX sortOrder IS PRIMARY sortOrder
        .
FIND FIRST dynParamValue NO-LOCK
     WHERE ROWID(dynParamValue) EQ iprRowID
     NO-ERROR.
IF NOT AVAILABLE dynParamValue THEN RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME ttParamSetBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttParamSet

/* Definitions for BROWSE ttParamSetBrowse                              */
&Scoped-define FIELDS-IN-QUERY-ttParamSetBrowse ttParamSet.isVisible ttParamSet.setName ttParamSet.setTitle   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttParamSetBrowse ttParamSet.isVisible   
&Scoped-define ENABLED-TABLES-IN-QUERY-ttParamSetBrowse ttParamSet
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-ttParamSetBrowse ttParamSet
&Scoped-define SELF-NAME ttParamSetBrowse
&Scoped-define QUERY-STRING-ttParamSetBrowse FOR EACH ttParamSet
&Scoped-define OPEN-QUERY-ttParamSetBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttParamSet.
&Scoped-define TABLES-IN-QUERY-ttParamSetBrowse ttParamSet
&Scoped-define FIRST-TABLE-IN-QUERY-ttParamSetBrowse ttParamSet


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-ttParamSetBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ttParamSetBrowse btnOK btnCancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U
     LABEL "Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel"
     BGCOLOR 8 .

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_check_disabled.ico":U
     LABEL "OK" 
     SIZE 8 BY 1.91 TOOLTIP "Save"
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 18 BY 2.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY ttParamSetBrowse FOR 
      ttParamSet SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE ttParamSetBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttParamSetBrowse Dialog-Frame _FREEFORM
  QUERY ttParamSetBrowse DISPLAY
      ttParamSet.isVisible VIEW-AS TOGGLE-BOX
ttParamSet.setName
ttParamSet.setTitle
ENABLE
ttParamSet.isVisible
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 62 BY 25.95
         TITLE "Parameter Sets".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     ttParamSetBrowse AT ROW 1 COL 2 WIDGET-ID 200
     btnOK AT ROW 27.43 COL 47 HELP
          "Save"
     btnCancel AT ROW 27.43 COL 55 HELP
          "Cancel"
     RECT-1 AT ROW 27.19 COL 46 WIDGET-ID 2
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Show/Hide Parameter Sets" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB ttParamSetBrowse RECT-1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       btnOK:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttParamSetBrowse
/* Query rebuild information for BROWSE ttParamSetBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttParamSet.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttParamSetBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Show/Hide Parameter Sets */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
    oplSave = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
    RUN pSave.
    oplSave = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ttParamSetBrowse
&Scoped-define SELF-NAME ttParamSetBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttParamSetBrowse Dialog-Frame
ON ROW-LEAVE OF ttParamSetBrowse IN FRAME Dialog-Frame /* Parameter Sets */
DO:
    btnOK:SENSITIVE = BROWSE ttParamSetBrowse:MODIFIED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

{methods/template/brwcustom.i}

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN pParameterSets.
  RUN enable_UI.
  btnOK:SENSITIVE = NO.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  ENABLE ttParamSetBrowse btnOK btnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pParameterSets Dialog-Frame 
PROCEDURE pParameterSets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE ttParamSet.
    FOR EACH dynValueParamSet NO-LOCK
        WHERE dynValueParamSet.subjectID    EQ dynParamValue.subjectID
          AND dynValueParamSet.user-id      EQ dynParamValue.user-id
          AND dynValueParamSet.prgmName     EQ dynParamValue.prgmName
          AND dynValueParamSet.paramValueID EQ dynParamValue.paramValueID
           BY dynValueParamSet.sortOrder
        :
        FIND FIRST dynParamSet NO-LOCK
             WHERE dynParamSet.paramSetID EQ dynValueParamSet.paramSetID
             NO-ERROR.
        IF NOT AVAILABLE dynParamSet THEN NEXT.
        CREATE ttParamSet.
        ASSIGN
            ttParamSet.sortOrder = dynValueParamSet.sortOrder
            ttParamSet.setName   = dynParamSet.setName
            ttParamSet.setTitle  = dynParamSet.setTitle
            ttParamSet.isVisible = dynValueParamSet.isVisible
            ttParamSet.rRowID    = ROWID(dynValueParamSet)
            .
    END. /* each dynvalueparamset */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSave Dialog-Frame 
PROCEDURE pSave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO TRANSACTION:
        FOR EACH ttParamSet:
            FIND FIRST dynValueParamSet EXCLUSIVE-LOCK
                 WHERE ROWID(dynValueParamSet) EQ ttParamSet.rRowID
                 NO-ERROR.
            IF AVAILABLE dynValueParamSet THEN
            dynValueParamSet.isVisible = ttParamSet.isVisible.
        END. /* each ttparamset */
    END. /* do trans */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

