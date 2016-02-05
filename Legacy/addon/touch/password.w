&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: touch/password.w

  Description: from SMART.W - Template for basic SmartObject

  Author: Ron Stark
  Created: 4.18.2000

------------------------------------------------------------------------*/
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

{touch/touchdef.i}

DEF VAR lv-password AS cha NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 keystroke Btn_Accept Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS keystroke 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Accept 
     LABEL "ACCEPT PASSWORD" 
     SIZE 40 BY 2.38 TOOLTIP "ACCEPT PASSWORD".

DEFINE BUTTON Btn_Cancel 
     LABEL "CANCEL" 
     SIZE 40 BY 2.38 TOOLTIP "CANCEL".

DEFINE VARIABLE keystroke AS CHARACTER FORMAT "X(256)":U 
     LABEL "PASSWORD" 
     VIEW-AS FILL-IN 
     SIZE 81 BY 1 TOOLTIP "PASSWORD"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 124 BY 12.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     keystroke AT ROW 9.81 COL 41 COLON-ALIGNED
     Btn_Accept AT ROW 11.24 COL 43
     Btn_Cancel AT ROW 11.24 COL 84
     "TYPE PASSWORD AND TOUCH 'ACCEPT PASSWORD' BUTTON" VIEW-AS TEXT
          SIZE 76 BY .62 AT ROW 8.86 COL 47
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 7 FGCOLOR 15 FONT 6
         DEFAULT-BUTTON Btn_Accept.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 12.95
         WIDTH              = 124.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Btn_Accept:PRIVATE-DATA IN FRAME F-Main     = 
                "ACCEPT PASSWORD".

ASSIGN 
       Btn_Cancel:PRIVATE-DATA IN FRAME F-Main     = 
                "CANCEL".

ASSIGN 
       keystroke:PRIVATE-DATA IN FRAME F-Main     = 
                "PASSWORD".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_Accept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Accept s-object
ON CHOOSE OF Btn_Accept IN FRAME F-Main /* ACCEPT PASSWORD */
DO:
  {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('employee_code',OUTPUT employee_code)"}
  FIND employee WHERE employee.company = company_code
                  AND employee.employee = employee_code
                NO-LOCK NO-ERROR.
  IF NOT AVAILABLE employee THEN
  RETURN.
  IF field_value NE employee.passwd THEN
  DO:
    MESSAGE 'INVALID PASSWORD - TRY AGAIN' VIEW-AS ALERT-BOX.
    ASSIGN
      field_value = ''
      h_field:SCREEN-VALUE = ''.
    APPLY "entry" TO keystroke.
    RETURN NO-APPLY.
  END.
  ASSIGN
    field_value = ''
    h_field:SCREEN-VALUE = ''.
      
  {methods/run_link.i "CONTAINER" "Change_Page" "(5)"}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel s-object
ON CHOOSE OF Btn_Cancel IN FRAME F-Main /* CANCEL */
DO:
  ASSIGN
    field_value = ''
    h_field:SCREEN-VALUE = ''.
  {methods/run_link.i "CONTAINER" "Change_Page" "(3)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME keystroke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL keystroke s-object
ON ANY-PRINTABLE OF keystroke IN FRAME F-Main /* PASSWORD */
DO:
    RUN Keyboard-Stroke  (keylabel(lastkey)). 
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE keyboard-stroke s-object 
PROCEDURE keyboard-stroke :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


DEFINE INPUT PARAMETER keystroke AS CHARACTER NO-UNDO.

IF keystroke = 'CAPS LOCK (ON)' THEN
caps_lock = YES.
ELSE
IF keystroke = 'CAPS LOCK (OFF)' THEN
caps_lock = NO.
ELSE
DO WITH FRAME {&FRAME-NAME}:
  IF NOT caps_lock THEN keystroke = LC(keystroke).
  
  CASE keystroke:
    WHEN 'ALPHA' THEN
    {methods/run_link.i "CONTAINER" "Display_Keyboard" "('alphabet.',THIS-PROCEDURE:HANDLE)"}
    WHEN 'BACKSPACE' THEN
    IF field_value NE '' THEN
    field_value = SUBSTR(field_value,1,LENGTH(field_value) - 1).
    WHEN 'CLEAR' THEN
    field_value = ''.
    WHEN 'DQ' THEN /* Double Quote */
    field_value = field_value + '"'.
    WHEN 'QWERTY' THEN
    {methods/run_link.i "CONTAINER" "Display_Keyboard" "('keyboard.',THIS-PROCEDURE:HANDLE)"}
    WHEN 'SPACE' THEN
    field_value = field_value + '`'.
    WHEN 'SORT' THEN
    {methods/run_link.i "CONTAINER" "Display_Keyboard" "('sortpad.',THIS-PROCEDURE:HANDLE)"}
    OTHERWISE
    field_value = field_value + keystroke.
  END CASE.
/*
  IF VALID-HANDLE(h_field) THEN h_field:SCREEN-VALUE = REPLACE(field_value,'`',' ').
*/ 
  lv-password = lv-password + keystroke.
  h_field:SCREEN-VALUE = h_field:SCREEN-VALUE + "*".
  h_field:CURSOR-OFFSET = h_field:CURSOR-OFFSET + 20.

END.
RETURN NO-APPLY.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Key_Stroke s-object 
PROCEDURE Key_Stroke :
/*------------------------------------------------------------------------------
  Purpose:     Apply keystroke to field with focus
  Parameters:  Input Keystroke
  Notes:       
------------------------------------------------------------------------------*/
  &Scoped-define KEYSTROKE PASSWORD

  {touch/keystrok.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize s-object 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      field_value = ''
      h_field = keystroke:HANDLE
      h_field:SCREEN-VALUE = ''.
  END.

  APPLY "entry" TO keystroke IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view s-object 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {touch/localview.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set_focus s-object 
PROCEDURE set_focus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY "entry" TO keystroke IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

