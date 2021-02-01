&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: dynSubjctCopy.w

  Description: Dynamic Subject Copy child tables

  Input Parameters: <none>

  Output Parameters: Copy Table, Copy Where, Copy Column, Copy Param Set

  Author: Ron Stark

  Created: 2.21.2019
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE OUTPUT PARAMETER oplCopyTable    AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER oplCopyWhere    AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER oplCopyColumn   AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER oplCopyParamSet AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnOK lCopyAll lCopyTable lCopyWhere ~
lCopyColumn lCopyParamSet 
&Scoped-Define DISPLAYED-OBJECTS lCopyAll lCopyTable lCopyWhere lCopyColumn ~
lCopyParamSet 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCopyAll Dialog-Frame 
FUNCTION fCopyAll RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.png":U NO-FOCUS FLAT-BUTTON
     LABEL "OK" 
     SIZE 8 BY 1.91 TOOLTIP "OK"
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 9.8 BY 2.38.

DEFINE VARIABLE lCopyAll AS LOGICAL INITIAL yes 
     LABEL "Copy All Records" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE lCopyColumn AS LOGICAL INITIAL yes 
     LABEL "Copy Dynamic Column Records" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE lCopyParamSet AS LOGICAL INITIAL yes 
     LABEL "Copy Dynamic Parameter Set Records" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE lCopyTable AS LOGICAL INITIAL yes 
     LABEL "Copy Dynamic Table Records" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE lCopyWhere AS LOGICAL INITIAL yes 
     LABEL "Copy Dynamic Where Records" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnOK AT ROW 4.81 COL 50
     lCopyAll AT ROW 1.24 COL 3 WIDGET-ID 10
     lCopyTable AT ROW 2.43 COL 7 WIDGET-ID 2
     lCopyWhere AT ROW 3.62 COL 7 WIDGET-ID 4
     lCopyColumn AT ROW 4.81 COL 7 WIDGET-ID 6
     lCopyParamSet AT ROW 6 COL 7 WIDGET-ID 8
     RECT-1 AT ROW 4.57 COL 49 WIDGET-ID 12
     SPACE(0.20) SKIP(0.01)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1 
         TITLE "Dynamic Subject Copy" WIDGET-ID 100.


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

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Dynamic Subject Copy */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
    ASSIGN
        oplCopyTable    = lCopyTable
        oplCopyWhere    = lCopyWhere
        oplCopyColumn   = lCopyColumn
        oplCopyParamSet = lCopyParamSet
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lCopyAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lCopyAll Dialog-Frame
ON VALUE-CHANGED OF lCopyAll IN FRAME Dialog-Frame /* Copy All Records */
DO:
    ASSIGN
        {&SELF-NAME}
        lCopyTable    = {&SELF-NAME}
        lCopyWhere    = {&SELF-NAME}
        lCopyColumn   = {&SELF-NAME}
        lCopyParamSet = {&SELF-NAME}
        .
    DISPLAY lCopyTable lCopyWhere lCopyColumn lCopyParamSet
        WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lCopyColumn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lCopyColumn Dialog-Frame
ON VALUE-CHANGED OF lCopyColumn IN FRAME Dialog-Frame /* Copy Dynamic Column Records */
DO:
    ASSIGN {&SELF-NAME}.
    fCopyAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lCopyParamSet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lCopyParamSet Dialog-Frame
ON VALUE-CHANGED OF lCopyParamSet IN FRAME Dialog-Frame /* Copy Dynamic Parameter Set Records */
DO:
    ASSIGN {&SELF-NAME}.
    fCopyAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lCopyTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lCopyTable Dialog-Frame
ON VALUE-CHANGED OF lCopyTable IN FRAME Dialog-Frame /* Copy Dynamic Table Records */
DO:
    ASSIGN {&SELF-NAME}.
    fCopyAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lCopyWhere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lCopyWhere Dialog-Frame
ON VALUE-CHANGED OF lCopyWhere IN FRAME Dialog-Frame /* Copy Dynamic Where Records */
DO:
    ASSIGN {&SELF-NAME}.
    fCopyAll().
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
  RUN enable_UI.
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
  DISPLAY lCopyAll lCopyTable lCopyWhere lCopyColumn lCopyParamSet 
      WITH FRAME Dialog-Frame.
  ENABLE btnOK lCopyAll lCopyTable lCopyWhere lCopyColumn lCopyParamSet 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCopyAll Dialog-Frame 
FUNCTION fCopyAll RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            lCopyAll = lCopyTable AND lCopyWhere AND lCopyColumn AND lCopyParamSet
            lCopyAll:SCREEN-VALUE = STRING(lCopyAll)
            .
    END. /* with frame */

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

