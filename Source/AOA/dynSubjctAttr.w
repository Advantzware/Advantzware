&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: dynSubjctAttr.w

  Description: Dynamic External Form

  Input Parameters: Task Description, External Form, Record Limit

  Output Parameters: Task Description, External Form, Record Limit, OK

  Author: Ron Stark

  Created: 3.25.2019 (10.21.2020)
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT-OUTPUT PARAMETER iopcTaskDescrip  AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcExternalForm AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiRecordLimit  AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioplFavorite     AS LOGICAL   NO-UNDO.
DEFINE       OUTPUT PARAMETER oplOK            AS LOGICAL   NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES dynParamValue dynSubject

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH dynParamValue SHARE-LOCK, ~
      EACH dynSubject OF dynParamValue SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH dynParamValue SHARE-LOCK, ~
      EACH dynSubject OF dynParamValue SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame dynParamValue dynSubject
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame dynParamValue
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame dynSubject


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cTaskDescrip cExternalForm iRecordLimit ~
btnOK btnCancel lFavorite 
&Scoped-Define DISPLAYED-OBJECTS cTaskDescrip cExternalForm iRecordLimit ~
lFavorite 

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

DEFINE VARIABLE cExternalForm AS CHARACTER FORMAT "x(80)" 
     LABEL "External Form" 
     VIEW-AS FILL-IN 
     SIZE 82 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE cTaskDescrip AS CHARACTER FORMAT "X(80)":U 
     LABEL "Task Description" 
     VIEW-AS FILL-IN 
     SIZE 82 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE iRecordLimit AS INTEGER FORMAT ">>,>>>,>>9" INITIAL 0 
     LABEL "Record Limit" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 18 BY 2.38
     BGCOLOR 15 .

DEFINE VARIABLE lFavorite AS LOGICAL INITIAL no 
     LABEL "Favorite" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      dynParamValue, 
      dynSubject SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     cTaskDescrip AT ROW 1.24 COL 17 COLON-ALIGNED HELP
          "Enter Task Description" WIDGET-ID 8
     cExternalForm AT ROW 2.43 COL 17 COLON-ALIGNED HELP
          "Enter External Form" WIDGET-ID 4
     iRecordLimit AT ROW 4.81 COL 17 COLON-ALIGNED HELP
          "Enter Record Limit" WIDGET-ID 6
     btnOK AT ROW 5.05 COL 84 HELP
          "Save"
     btnCancel AT ROW 5.05 COL 92 HELP
          "Cancel"
     lFavorite AT ROW 6 COL 19 HELP
          "Toggle Favorite" WIDGET-ID 10
     "Note: When custom Jasper Report is desired, Enter location of the .jrxml form." VIEW-AS TEXT
          SIZE 82 BY 1 AT ROW 3.62 COL 19 WIDGET-ID 12
     RECT-1 AT ROW 4.81 COL 83 WIDGET-ID 2
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Task Attributes: Description, External Form, Record Limit, Favorite" WIDGET-ID 100.


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

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.dynParamValue,ASI.dynSubject OF ASI.dynParamValue"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Task Attributes: Description, External Form, Record Limit, Favorite */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
    oplOK = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
    ASSIGN
        cTaskDescrip
        cExternalForm
        iRecordLimit
        lFavorite
        iopcTaskDescrip  = cTaskDescrip
        iopcExternalForm = cExternalForm
        iopiRecordLimit  = iRecordLimit
        ioplFavorite     = lFavorite
        oplOK = YES
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  ASSIGN
      lFavorite     = ioplFavorite
      cTaskDescrip  = iopcTaskDescrip
      cExternalForm = iopcExternalForm
      iRecordLimit  = iopiRecordLimit
      .
  RUN enable_UI.
  cTaskDescrip:SENSITIVE = iopcTaskDescrip NE "User Default".
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

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  DISPLAY cTaskDescrip cExternalForm iRecordLimit lFavorite 
      WITH FRAME Dialog-Frame.
  ENABLE cTaskDescrip cExternalForm iRecordLimit btnOK btnCancel lFavorite 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

