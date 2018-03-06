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

&SCOPED-DEFINE PageNo 3
{touch/touchdef.i}
{custom/globdefs.i}

DO TRANSACTION:
   {sys/inc/tskey.i}
END.

&Scoped-define BUTTON-INCLUDE EMPLOYEES

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 keystroke Btn_Button-1 Btn_Button-6 ~
Btn_Button-2 Btn_Button-7 Btn_Button-3 Btn_Button-8 Btn_Button-4 ~
Btn_Button-9 Btn_Button-5 Btn_Button-10 
&Scoped-Define DISPLAYED-OBJECTS keystroke 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Btn_Button-1 Btn_Button-6 Btn_Button-2 Btn_Button-7 ~
Btn_Button-3 Btn_Button-8 Btn_Button-4 Btn_Button-9 Btn_Button-5 ~
Btn_Button-10 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Button-1 
     LABEL "BUTTON 1" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-10 
     LABEL "BUTTON 10" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-2 
     LABEL "BUTTON 2" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-3 
     LABEL "BUTTON 3" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-4 
     LABEL "BUTTON 4" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-5 
     LABEL "BUTTON 5" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-6 
     LABEL "BUTTON 6" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-7 
     LABEL "BUTTON 7" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-8 
     LABEL "BUTTON 8" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Button-9 
     LABEL "BUTTON 9" 
     SIZE 40 BY 2.38.

DEFINE VARIABLE keystroke AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 40 BY .91 TOOLTIP "Password"
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 12.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     keystroke AT ROW 1.1 COL 41 COLON-ALIGNED WIDGET-ID 26
     Btn_Button-1 AT ROW 1.95 COL 2 WIDGET-ID 6
     Btn_Button-6 AT ROW 1.95 COL 43 WIDGET-ID 18
     Btn_Button-2 AT ROW 4.33 COL 2 WIDGET-ID 10
     Btn_Button-7 AT ROW 4.33 COL 43 WIDGET-ID 20
     Btn_Button-3 AT ROW 6.71 COL 2 WIDGET-ID 12
     Btn_Button-8 AT ROW 6.71 COL 43 WIDGET-ID 22
     Btn_Button-4 AT ROW 9.1 COL 2 WIDGET-ID 14
     Btn_Button-9 AT ROW 9.1 COL 43 WIDGET-ID 24
     Btn_Button-5 AT ROW 11.48 COL 2 WIDGET-ID 16
     Btn_Button-10 AT ROW 11.48 COL 43 WIDGET-ID 8
     "EMPLOYEES" VIEW-AS TEXT
          SIZE 13 BY .52 AT ROW 1.24 COL 3 WIDGET-ID 2
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FONT 6.


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

/* SETTINGS FOR BUTTON Btn_Button-1 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-10 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-2 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-3 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-4 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-5 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-6 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-7 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-8 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-9 IN FRAME F-Main
   1                                                                    */
ASSIGN 
       keystroke:PRIVATE-DATA IN FRAME F-Main     = 
                "Search".

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

&Scoped-define SELF-NAME Btn_Button-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-1 s-object
ON CHOOSE OF Btn_Button-1 IN FRAME F-Main /* BUTTON 1 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-10 s-object
ON CHOOSE OF Btn_Button-10 IN FRAME F-Main /* BUTTON 10 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-2 s-object
ON CHOOSE OF Btn_Button-2 IN FRAME F-Main /* BUTTON 2 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-3 s-object
ON CHOOSE OF Btn_Button-3 IN FRAME F-Main /* BUTTON 3 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-4 s-object
ON CHOOSE OF Btn_Button-4 IN FRAME F-Main /* BUTTON 4 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-5 s-object
ON CHOOSE OF Btn_Button-5 IN FRAME F-Main /* BUTTON 5 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-6 s-object
ON CHOOSE OF Btn_Button-6 IN FRAME F-Main /* BUTTON 6 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-7 s-object
ON CHOOSE OF Btn_Button-7 IN FRAME F-Main /* BUTTON 7 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-8 s-object
ON CHOOSE OF Btn_Button-8 IN FRAME F-Main /* BUTTON 8 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-9 s-object
ON CHOOSE OF Btn_Button-9 IN FRAME F-Main /* BUTTON 9 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME keystroke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL keystroke s-object
ON ANY-PRINTABLE OF keystroke IN FRAME F-Main /* Search */
DO:
    RUN Key_Stroke (KEYLABEL(LASTKEY)).    
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL keystroke s-object
ON RETURN OF keystroke IN FRAME F-Main /* Search */
DO:
    RUN pClick ("AcceptPassword").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL keystroke s-object
ON VALUE-CHANGED OF keystroke IN FRAME F-Main /* Search */
DO:
    field_value = SELF:SCREEN-VALUE.
    RUN Get_Employees.
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

{touch/pCreateINIObjects.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Button_Labels s-object 
PROCEDURE Button_Labels :
/*------------------------------------------------------------------------------
  Purpose:     place values on machine button labels
  Parameters:  Input current button item
  Notes:       
------------------------------------------------------------------------------*/
  {touch/btnlabel.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Get_Employees.

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
  ASSIGN
    itemlist = ''
    button_item = 1
    .
  DO WITH FRAME {&FRAME-NAME}:
    FOR EACH employee NO-LOCK
        WHERE employee.company EQ company_code
          AND employee.last_name + ' ' +
              employee.first_name MATCHES('*' + keystroke:SCREEN-VALUE + '*')
           BY employee.last_name
           BY employee.first_name
           BY employee.employee
        :
      itemlist = itemlist 
               + employee.last_name + ', '
               + employee.first_name + ' ('
               + employee.employee + ')'
               + '@'
               .
    END. /* each employee */
  END. /* with frame */
  itemlist = TRIM(itemlist,'@').
  RUN Button_Labels (INPUT-OUTPUT button_item).

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

  RUN Get_Employees.

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
  RUN pCreateINIObjects ("First,Last,PageUp,PageDown,HomeSmall,Back").

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  h_field = keystroke:HANDLE IN FRAME {&FRAME-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClick s-object 
PROCEDURE pClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcClick AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE idummy AS INTEGER NO-UNDO.
    
    CASE ipcClick:
        WHEN "HomeSmall" THEN DO:
            {methods/run_link.i "CONTAINER" "Change_Page" "(2)"}
        END.
        WHEN "PageUp" THEN DO:
            button_item = button_item - 20.
            RUN Button_Labels (INPUT-OUTPUT button_item).
        END.
        WHEN "PageDown" THEN DO:
            RUN Button_Labels (INPUT-OUTPUT button_item).
        END.
        WHEN "First" THEN DO:
            button_item = 1.
            RUN Button_Labels (INPUT-OUTPUT button_item).
        END.
        WHEN "Last" THEN DO:
            button_item = NUM-ENTRIES(itemlist,'@') + 1.
            RUN Button_Labels (INPUT-OUTPUT button_item).
        END.
        WHEN "Back" THEN DO:
            {methods/run_link.i "CONTAINER" "Change_Page" "(2)"}
        END.
    END CASE.

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

