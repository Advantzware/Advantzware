&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: listobjs/cust_.w

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

&Scoped-define WHERE-STATEMENT cust.company = gcompany

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartListObject

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES eb
&Scoped-define FIRST-EXTERNAL-TABLE eb


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR eb.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-1 list-order rd-industry ~
begin_cust-no end_cust-no begin_eb_style end_eb_style begin_eb_flute ~
end_eb_flute begin_eb_test end_eb_test 
&Scoped-Define DISPLAYED-OBJECTS list-order rd-industry begin_cust-no ~
end_cust-no begin_eb_style end_eb_style begin_eb_flute end_eb_flute ~
begin_eb_test end_eb_test 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning  Customer" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE begin_eb_flute AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning  Flute" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE begin_eb_style AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning  Style" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE begin_eb_test AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Test" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE end_eb_flute AS CHARACTER FORMAT "X(8)" INITIAL "zzz" 
     LABEL "Ending Flute" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE end_eb_style AS CHARACTER FORMAT "X(8)" INITIAL "zzzz" 
     LABEL "Ending Style" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE end_eb_test AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzz" 
     LABEL "Ending Test" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE list-order AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Default", 1,
"Description", 2
     SIZE 106 BY 1 NO-UNDO.

DEFINE VARIABLE rd-industry AS CHARACTER INITIAL "B" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Corrugated", "C",
"Folding", "F",
"Both", "B"
     SIZE 58 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 120 BY 1.67.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 120 BY 8.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     list-order AT ROW 1.48 COL 14 HELP
          "Select List Order" NO-LABEL
     rd-industry AT ROW 3.38 COL 32 NO-LABEL
     begin_cust-no AT ROW 4.57 COL 35 COLON-ALIGNED HELP
          "Enter Beginning Customer"
     end_cust-no AT ROW 4.57 COL 75 COLON-ALIGNED HELP
          "Enter Ending Customer"
     begin_eb_style AT ROW 5.76 COL 35 COLON-ALIGNED HELP
          "Enter Beginning Customer"
     end_eb_style AT ROW 5.76 COL 75 COLON-ALIGNED HELP
          "Enter Ending Customer"
     begin_eb_flute AT ROW 6.95 COL 35 COLON-ALIGNED HELP
          "Enter Beginning Customer"
     end_eb_flute AT ROW 6.95 COL 75 COLON-ALIGNED HELP
          "Enter Ending Customer"
     begin_eb_test AT ROW 8.14 COL 35 COLON-ALIGNED HELP
          "Enter Beginning Customer"
     end_eb_test AT ROW 8.14 COL 75 COLON-ALIGNED HELP
          "Enter Ending Customer"
     RECT-2 AT ROW 3.14 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 2.91 COL 2
     RECT-1 AT ROW 1 COL 1
     "List Order:" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartListObject
   External Tables: ASI.eb
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
       begin_eb_flute:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       begin_eb_style:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       begin_eb_test:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       end_eb_flute:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       end_eb_style:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       end_eb_test:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       list-order:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

ASSIGN 
       rd-industry:PRIVATE-DATA IN FRAME F-Main     = 
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


