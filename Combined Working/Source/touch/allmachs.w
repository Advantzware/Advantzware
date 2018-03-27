&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: touch/allmachs.w

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

&SCOPED-DEFINE PageNo 8
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
&Scoped-Define ENABLED-OBJECTS RECT-1 SL keystroke 
&Scoped-Define DISPLAYED-OBJECTS SL keystroke 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE keystroke AS CHARACTER FORMAT "X(256)":U 
     LABEL "Machine Filter" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1 TOOLTIP "Machine Filter"
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

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

ASSIGN 
       keystroke:PRIVATE-DATA IN FRAME F-Main     = 
                "Machine Filter".

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

&Scoped-define SELF-NAME keystroke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL keystroke s-object
ON VALUE-CHANGED OF keystroke IN FRAME F-Main /* Machine Filter */
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
    RUN pClick ("SelectMachine").
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

{touch/pCreateINIObjects.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Active_Machines s-object 
PROCEDURE Get_Active_Machines :
/*------------------------------------------------------------------------------
  Purpose:     populate selection list with machine records
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('employee_code',OUTPUT employee_code)"}
  itemlist = ''.
  FOR EACH emplogin NO-LOCK WHERE emplogin.company = company_code
                              AND emplogin.machine GT ''
                              AND emplogin.END_date = ?
                              AND emplogin.end_time = 0
                              AND emplogin.total_time = 0:
    /*IF INDEX(itemlist,emplogin.machine) NE 0 THEN*/
    IF LOOKUP(emplogin.machine,itemlist,"@") > 0 THEN
    NEXT.
    FIND FIRST mach WHERE mach.company = company_code
                      AND mach.m-code = emplogin.machine
                    NO-LOCK NO-ERROR.
    IF NOT AVAILABLE mach THEN
    NEXT.
    itemlist = IF itemlist = '' THEN CAPS(mach.m-code) + ' (' + LC(mach.m-dscr) + ')'
               ELSE itemlist + '@' + CAPS(mach.m-code) + ' (' + LC(mach.m-dscr) + ')'.
  END.
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
    MESSAGE 'NO MACHINES AVAILABLE' VIEW-AS ALERT-BOX.
    {methods/run_link.i "CONTAINER" "Change_Page" "(2)"}
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Machines s-object 
PROCEDURE Get_Machines :
/*------------------------------------------------------------------------------
  Purpose:     populate selection list with machine records
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/run_link.i "CONTAINER" "Get_Value" "('company_code',OUTPUT company_code)"}
  {methods/run_link.i "CONTAINER" "Get_Value" "('employee_code',OUTPUT employee_code)"}
  itemlist = ''.
  FOR EACH mach NO-LOCK WHERE mach.company = company_code:
    IF CAN-FIND(FIRST emplogin WHERE emplogin.company = company_code
                                 AND emplogin.employee = employee_code
                                 AND emplogin.machine = mach.m-code
                                 AND emplogin.END_date = ?
                                 AND emplogin.end_time = 0
                                 AND emplogin.total_time = 0) THEN
    NEXT.
    itemlist = IF itemlist = '' THEN CAPS(mach.m-code) + ' (' + LC(mach.m-dscr) + ')'
               ELSE itemlist + '@' + CAPS(mach.m-code) + ' (' + LC(mach.m-dscr) + ')'.
  END.
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
  &Scoped-define KEYSTROKE ALLMACHS

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
  RUN pCreateINIObjects ("Up,PageUp,Down,PageDown,SelectMachine,Home").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClick s-object 
PROCEDURE pClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcClick AS CHARACTER NO-UNDO.
    
    CASE ipcClick:
        WHEN "Up" THEN DO WITH FRAME {&FRAME-NAME}:
            item = item - 1.
            IF item LT 1 THEN
            item = 1.
            SL:SCREEN-VALUE = SL:ENTRY(item).
        END.
        WHEN "PageUp" THEN DO:
            item = item - 19.
            IF item LT 1 THEN
            item = 1.
            SL:SCREEN-VALUE = SL:ENTRY(item).
        END.
        WHEN "Down" THEN DO:
            item = item + 1.
            IF item GT SL:NUM-ITEMS THEN
            item = SL:NUM-ITEMS.
            SL:SCREEN-VALUE = SL:ENTRY(item).
        END.
        WHEN "PageDown" THEN DO:
            item = item + 19.
            IF item GT SL:NUM-ITEMS THEN
            item = SL:NUM-ITEMS.
            SL:SCREEN-VALUE = SL:ENTRY(item).
        END.
        WHEN "SelectMachine" THEN DO:
            ASSIGN
              idummy = INDEX(SL:SCREEN-VALUE,'(')
              machine_code = SUBSTR(SL:SCREEN-VALUE,1,idummy - 2)
              .
            {methods/run_link.i "CONTAINER" "Set_Value" "('machine_code',machine_code)"}
            {methods/run_link.i "CONTAINER" "Change_Page" "(6)"}
        END.
        WHEN "Home" THEN DO:
            {methods/run_link.i "CONTAINER" "Change_Page" "(3)"}
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

