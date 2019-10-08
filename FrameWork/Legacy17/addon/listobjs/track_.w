&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: listobjs/track_.w

  Description: from SMART.W - Template for basic SmartObject

  Author: Ron Stark
  Created: 02/27/98

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

&Scoped-define FIRST-EXTERNAL-TABLE track

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 list-order RECT-2 m_prgmname ~
m_user_id begin_date end_date RECT-3 date-time program user-id m_purge 
&Scoped-Define DISPLAYED-OBJECTS list-order m_prgmname m_user_id begin_date ~
end_date date-time program user-id m_purge F-2 F-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define F1 F-2 F-1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/98 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/49 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE F-1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE m_prgmname AS CHARACTER FORMAT "X(12)":U 
     LABEL "Program Name" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE m_user_id AS CHARACTER FORMAT "X(12)":U 
     LABEL "User ID" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE date-time AS CHARACTER INITIAL "1" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "First", "1",
"Second", "2",
"Third", "3"
     SIZE 45 BY 1 TOOLTIP "Date and Time" NO-UNDO.

DEFINE VARIABLE list-order AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 106 BY 1 NO-UNDO.

DEFINE VARIABLE program AS CHARACTER INITIAL "2" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "First", "1",
"Second", "2",
"Third", "3"
     SIZE 45 BY 1 TOOLTIP "Program".

DEFINE VARIABLE user-id AS CHARACTER INITIAL "3" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "First", "1",
"Second", "2",
"Third", "3"
     SIZE 45 BY 1 TOOLTIP "User" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 120 BY 1.67.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 120 BY 8.81.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 66 BY 4.05.

DEFINE VARIABLE m_purge AS LOGICAL INITIAL no 
     LABEL "Purge Data" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     list-order AT ROW 1.48 COL 14 HELP
          "Select List Order" NO-LABEL
     m_prgmname AT ROW 3.62 COL 29 COLON-ALIGNED HELP
          "Enter Program Name or Leave BLANK for ALL"
     m_user_id AT ROW 4.81 COL 29 COLON-ALIGNED HELP
          "Enter User ID or Leave BLANK for ALL"
     begin_date AT ROW 6.24 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 6.24 COL 66 COLON-ALIGNED HELP
          "Enter Ending Date"
     date-time AT ROW 8.14 COL 51 HELP
          "Select Order for Data and Time" NO-LABEL
     program AT ROW 9.33 COL 51 HELP
          "Select Order for Programs" NO-LABEL
     user-id AT ROW 10.52 COL 51 HELP
          "Select Order for Users" NO-LABEL
     m_purge AT ROW 10.76 COL 99 HELP
          "Purge Data Indicator"
     F-2 AT ROW 3.62 COL 47 NO-LABEL
     F-1 AT ROW 4.81 COL 45 NO-LABEL
     RECT-1 AT ROW 1 COL 1
     "List Order:" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 1.48 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 2.91 COL 2
     RECT-2 AT ROW 3.14 COL 1
     "List Sort Order" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 7.43 COL 32
     RECT-3 AT ROW 7.67 COL 31
     "Date and Time:" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 8.14 COL 35
     "Program:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 9.33 COL 41
     "User:" VIEW-AS TEXT
          SIZE 5 BY 1 AT ROW 10.52 COL 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
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

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       date-time:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

/* SETTINGS FOR FILL-IN F-1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN F-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       list-order:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

ASSIGN 
       m_prgmname:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       m_purge:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       m_user_id:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       program:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       user-id:PRIVATE-DATA IN FRAME F-Main     = 
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

&Scoped-define SELF-NAME date-time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL date-time s-object
ON VALUE-CHANGED OF date-time IN FRAME F-Main
DO:
  {methods/listobjs/dispfld.i}
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


&Scoped-define SELF-NAME program
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL program s-object
ON VALUE-CHANGED OF program IN FRAME F-Main
DO:
  {methods/listobjs/dispfld.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME user-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL user-id s-object
ON VALUE-CHANGED OF user-id IN FRAME F-Main
DO:
  {methods/listobjs/dispfld.i}
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


