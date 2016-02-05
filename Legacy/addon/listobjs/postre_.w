&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: listobjs/machtra_.w

  Description: from SMART.W - Template for basic SmartObject

  Author: 
  Created: 

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

{methods/defines/hndlset.i}
{custom/gcompany.i}

&Scoped-define SCOPDEFS post
&Scoped-define WHERE-STATEMENT machtran.company = selected-company

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartListObject

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES machtran
&Scoped-define FIRST-EXTERNAL-TABLE machtran


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR machtran.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 list-order begin_machine ~
end_machine begin_date end_date begin_shift end_shift post 
&Scoped-Define DISPLAYED-OBJECTS list-order selected-company begin_machine ~
end_machine begin_date end_date begin_shift end_shift post F1 F-3 F-5 F-6 ~
F-2 F-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define F1 F1 F-3 F-5 F-6 F-2 F-4 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_machine AS CHARACTER FORMAT "X(10)" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE begin_shift AS CHARACTER FORMAT "X":U 
     LABEL "Beginning Shift" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_machine AS CHARACTER FORMAT "X(10)" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE end_shift AS CHARACTER FORMAT "X":U 
     LABEL "Ending Shift" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-3 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-4 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-5 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-6 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE selected-company AS CHARACTER FORMAT "X(3)":U 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE list-order AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Default", 1,
"Description", 2
     SIZE 106 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 120 BY 1.67.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 120 BY 8.81.

DEFINE VARIABLE post AS LOGICAL INITIAL no 
     LABEL "RePost Data Collection Entries" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     list-order AT ROW 1.48 COL 14 HELP
          "Select List Order" NO-LABEL
     selected-company AT ROW 3.62 COL 40 COLON-ALIGNED
     begin_machine AT ROW 5.05 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     end_machine AT ROW 5.05 COL 76 COLON-ALIGNED HELP
          "Enter Ending Machine"
     begin_date AT ROW 6.48 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 6.48 COL 76 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_shift AT ROW 7.91 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Shift"
     end_shift AT ROW 7.91 COL 76 COLON-ALIGNED HELP
          "Enter Ending Shift"
     post AT ROW 9.81 COL 78 HELP
          "Post to ASI Database Indicator"
     F1 AT ROW 5.52 COL 58 NO-LABEL
     F-3 AT ROW 5.52 COL 94 NO-LABEL
     F-5 AT ROW 6.95 COL 58 NO-LABEL
     F-6 AT ROW 6.95 COL 94 NO-LABEL
     F-2 AT ROW 8.38 COL 46 NO-LABEL
     F-4 AT ROW 8.38 COL 82 NO-LABEL
     "Selection Parameters" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 2.91 COL 2
     RECT-1 AT ROW 1 COL 1
     RECT-2 AT ROW 3.14 COL 1
     "List Order:" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartListObject
   External Tables: EMPTRACK.machtran
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 10.95
         WIDTH              = 120.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN F-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-3 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN F-4 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-4:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-5 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-5:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-6 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-6:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       list-order:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

ASSIGN 
       post:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

/* SETTINGS FOR FILL-IN selected-company IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       selected-company:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}
{methods/enhance.i}
{methods/listobjs/listobjs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date s-object
ON HELP OF begin_date IN FRAME F-Main /* Beginning Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date s-object
ON HELP OF end_date IN FRAME F-Main /* Ending Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME list-order
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL list-order s-object
ON VALUE-CHANGED OF list-order IN FRAME F-Main
DO:
  RUN Get-Display-Values.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Create-List-Logic-Shell s-object 
PROCEDURE Create-List-Logic-Shell :
/*------------------------------------------------------------------------------
  Purpose:     Create List Logic Method
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lstlogic/lstshell.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Frame-Handle s-object 
PROCEDURE Frame-Handle :
/*------------------------------------------------------------------------------
  Purpose:     Called externally to get THIS-PROCEDURE's Frame handle
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/listobjs/framehdl.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-Show-Parameters s-object 
PROCEDURE Init-Show-Parameters :
/*------------------------------------------------------------------------------
  Purpose:     Supply Show Parameter setting to calling procedure.
  Parameters:  OUTPUT show parameters
  Notes:       
------------------------------------------------------------------------------*/
  {methods/listobjs/initshow.i}

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
  {methods/listobjs/listinit.i}

  {custom/getcmpny.i}
  FIND FIRST shifts NO-LOCK NO-ERROR.
  IF AVAILABLE shifts THEN
  begin_shift = shifts.shift.
  FIND LAST shifts NO-LOCK NO-ERROR.
  IF AVAILABLE shifts THEN
  end_shift = shifts.shift.
  ASSIGN
    selected-company = gcompany
    begin_date = TODAY
    end_date = TODAY.
  DISPLAY  
    selected-company
    begin_date
    end_date
    begin_shift
    end_shift
        WITH FRAME {&FRAME-NAME}.

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


