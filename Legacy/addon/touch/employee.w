&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: touch/employee.w

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
{custom/globdefs.i}

DO TRANSACTION:
   {sys/inc/tskey.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 SL Btn_One_Up Btn_Page_Up ~
Btn_One_Down Btn_Page_Down Btn_Select Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS SL keystroke 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     LABEL "CANCEL" 
     SIZE 40 BY 2.38 TOOLTIP "CANCEL".

DEFINE BUTTON Btn_One_Down 
     IMAGE-UP FILE "images\onedown":U
     LABEL "One Down" 
     SIZE 40 BY 2.38 TOOLTIP "DOWN".

DEFINE BUTTON Btn_One_Up 
     IMAGE-UP FILE "images\oneup":U
     LABEL "One Up" 
     SIZE 40 BY 2.38 TOOLTIP "UP".

DEFINE BUTTON Btn_Page_Down 
     IMAGE-UP FILE "images\pagedown":U
     LABEL "Page Down" 
     SIZE 40 BY 2.38 TOOLTIP "PAGE DOWN".

DEFINE BUTTON Btn_Page_Up 
     IMAGE-UP FILE "images\pageup":U
     LABEL "Page Up" 
     SIZE 40 BY 2.38 TOOLTIP "PAGE UP".

DEFINE BUTTON Btn_Select 
     LABEL "SELECT EMPLOYEE" 
     SIZE 40 BY 2.38 TOOLTIP "SELECT EMPLOYEE".

DEFINE VARIABLE keystroke AS CHARACTER FORMAT "X(256)":U 
     LABEL "KEY VALUE" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1 TOOLTIP "KEY VALUE"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 12.95.

DEFINE VARIABLE SL AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 40 BY 12.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     SL AT ROW 1.24 COL 2 NO-LABEL
     keystroke AT ROW 1.24 COL 67 COLON-ALIGNED
     Btn_One_Up AT ROW 3.62 COL 43
     Btn_Page_Up AT ROW 3.62 COL 84
     Btn_One_Down AT ROW 7.43 COL 43
     Btn_Page_Down AT ROW 7.43 COL 84
     Btn_Select AT ROW 11.24 COL 43
     Btn_Cancel AT ROW 11.24 COL 84
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 7 FGCOLOR 15 FONT 6.


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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Btn_Cancel:PRIVATE-DATA IN FRAME F-Main     = 
                "CANCEL".

ASSIGN 
       Btn_One_Down:PRIVATE-DATA IN FRAME F-Main     = 
                "images\onedown.bmp".

ASSIGN 
       Btn_One_Up:PRIVATE-DATA IN FRAME F-Main     = 
                "images\oneup.bmp".

ASSIGN 
       Btn_Page_Down:PRIVATE-DATA IN FRAME F-Main     = 
                "images\pagedown.bmp".

ASSIGN 
       Btn_Page_Up:PRIVATE-DATA IN FRAME F-Main     = 
                "images\pageup.bmp".

ASSIGN 
       Btn_Select:PRIVATE-DATA IN FRAME F-Main     = 
                "SELECT EMPLOYEE".

/* SETTINGS FOR FILL-IN keystroke IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       keystroke:PRIVATE-DATA IN FRAME F-Main     = 
                "KEY VALUE".

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

&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel s-object
ON CHOOSE OF Btn_Cancel IN FRAME F-Main /* CANCEL */
DO:
  {methods/run_link.i "CONTAINER" "Change_Page" "(2)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_One_Down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_One_Down s-object
ON CHOOSE OF Btn_One_Down IN FRAME F-Main /* One Down */
DO:
  item = item + 1.
  IF item GT SL:NUM-ITEMS THEN
  item = SL:NUM-ITEMS.
  SL:SCREEN-VALUE = SL:ENTRY(item).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_One_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_One_Up s-object
ON CHOOSE OF Btn_One_Up IN FRAME F-Main /* One Up */
DO:
  item = item - 1.
  IF item LT 1 THEN
  item = 1.
  SL:SCREEN-VALUE = SL:ENTRY(item).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Page_Down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Page_Down s-object
ON CHOOSE OF Btn_Page_Down IN FRAME F-Main /* Page Down */
DO:
  item = item + 19.
  IF item GT SL:NUM-ITEMS THEN
  item = SL:NUM-ITEMS.
  SL:SCREEN-VALUE = SL:ENTRY(item).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Page_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Page_Up s-object
ON CHOOSE OF Btn_Page_Up IN FRAME F-Main /* Page Up */
DO:
  item = item - 19.
  IF item LT 1 THEN
  item = 1.
  SL:SCREEN-VALUE = SL:ENTRY(item).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Select s-object
ON CHOOSE OF Btn_Select IN FRAME F-Main /* SELECT EMPLOYEE */
DO:
  ASSIGN
    idummy = INDEX(SL:SCREEN-VALUE,'(')
    employee_code = REPLACE(SUBSTR(SL:SCREEN-VALUE,idummy + 1),')','')
    employee_name = SUBSTR(SL:SCREEN-VALUE,1,idummy - 2).
  {methods/run_link.i "CONTAINER" "Set_Value" "('employee_code',employee_code)"}
  {methods/run_link.i "CONTAINER" "Set_Value" "('employee_name',employee_name)"}
  {methods/run_link.i "CONTAINER" "Change_Page" "(4)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME keystroke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL keystroke s-object
ON VALUE-CHANGED OF keystroke IN FRAME F-Main /* KEY VALUE */
DO:
   IF tskey-log = NO THEN
   DO:
      IF keystroke:SCREEN-VALUE NE "" THEN
         RUN key_stroke(INPUT SUBSTRING(keystroke:SCREEN-VALUE,LENGTH(keystroke:SCREEN-VALUE))).
      ELSE
         RUN key_stroke(INPUT "Clear").
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SL s-object
ON DEFAULT-ACTION OF SL IN FRAME F-Main
DO:
  APPLY 'CHOOSE' TO Btn_Select.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SL s-object
ON VALUE-CHANGED OF SL IN FRAME F-Main
DO:
  DO item = 1 TO SL:NUM-ITEMS:
    IF SL:IS-SELECTED(item) THEN
    LEAVE.
  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Active_Employees s-object 
PROCEDURE Get_Active_Employees :
/*------------------------------------------------------------------------------
  Purpose:     populate selection list with employee records
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
  itemlist = ''.
  
  /* ======== bad performance when many user hit this place at same time
  FOR EACH emplogin NO-LOCK WHERE emplogin.company = company_code
                              AND emplogin.machine GT ''
                              AND emplogin.end_time = 0
                              AND emplogin.total_time = 0:
    FIND employee OF emplogin NO-LOCK.
    itemlist = IF itemlist = '' THEN employee.last_name + ', ' + 
                                     employee.first_name + ' (' +
                                     employee.employee + ')'
               ELSE itemlist + '@' + employee.last_name + ', ' + 
                                     employee.first_name + ' (' +
                                     employee.employee + ')'.
  END.
  ===================================  */
  for each employee where employee.company = company_code no-lock:
      itemlist = IF itemlist = '' THEN employee.last_name + ', ' + 
                                     employee.first_name + ' (' +
                                     employee.employee + ')'
               ELSE itemlist + '@' + employee.last_name + ', ' + 
                                     employee.first_name + ' (' +
                                     employee.employee + ')'.
  end.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      SL:DELIMITER = '@'
      item = 1
      SL:LIST-ITEMS = itemlist
      SL:SCREEN-VALUE = SL:ENTRY(item)
      h_field = keystroke:HANDLE
      field_value = ''
      h_field = keystroke:HANDLE
      h_field:SCREEN-VALUE = ''.
  END.
  IF itemlist = '' THEN
  DO:
    MESSAGE 'NO EMPLOYEES AVAILABLE' VIEW-AS ALERT-BOX.
    {methods/run_link.i "CONTAINER" "Change_Page" "(2)"}
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Employees s-object 
PROCEDURE Get_Employees :
/*------------------------------------------------------------------------------
  Purpose:     populate selection list with employee records
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
  itemlist = ''.
  FOR EACH employee NO-LOCK WHERE employee.company = company_code:
    itemlist = IF itemlist = '' THEN employee.last_name + ', ' + 
                                     employee.first_name + ' (' +
                                     employee.employee + ')'
               ELSE itemlist + '@' + employee.last_name + ', ' + 
                                     employee.first_name + ' (' +
                                     employee.employee + ')'.
  END.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      SL:DELIMITER = '@'
      item = 1
      SL:LIST-ITEMS = itemlist
      SL:SCREEN-VALUE = SL:ENTRY(item)
      h_field = keystroke:HANDLE
      field_value = ''
      h_field:SCREEN-VALUE = ''.
  END.

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
  &Scoped-define KEYSTROKE EMPLOYEE

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
  IF tskey-log EQ NO THEN
     keystroke:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

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

