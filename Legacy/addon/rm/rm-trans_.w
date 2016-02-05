&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: listobjs/rm-ibtag_.w

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
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}
{sys/inc/varasgn.i}

{methods/defines/hndlset.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartListObject

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES item
&Scoped-define FIRST-EXTERNAL-TABLE item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR item.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-3 list-order begin_i-no end_i-no ~
Begin_procat End_procat begin_date end_date begin_whse end_whse TOGGLE-1 ~
TOGGLE-2 TOGGLE-3 TOGGLE-4 TOGGLE-5 
&Scoped-Define DISPLAYED-OBJECTS list-order begin_i-no end_i-no ~
Begin_procat End_procat begin_date end_date begin_whse end_whse TOGGLE-1 ~
TOGGLE-2 TOGGLE-3 TOGGLE-4 TOGGLE-5 F1 F-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define F1 F1 F-2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-F-Main 
       MENU-ITEM m_Inventory_by_Item_Name LABEL "Inventory by Item Name".


/* Definitions of the field level widgets                               */
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL ? 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(10)" 
     LABEL "From Item #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE Begin_procat AS CHARACTER FORMAT "X(10)":U 
     LABEL "From Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_whse AS CHARACTER FORMAT "X(10)":U 
     LABEL "From Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL ? 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(10)" 
     LABEL "To Item" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE End_procat AS CHARACTER FORMAT "X(10)":U 
     LABEL "To Category" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_whse AS CHARACTER FORMAT "X(10)":U INITIAL "ZZZZZZZZZZ" 
     LABEL "To Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE list-order AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Default", 1,
"Description", 2
     SIZE 106 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 120 BY 1.67.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 120 BY 9.52.

DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL yes 
     LABEL "Receipts" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .71
     FONT 6 NO-UNDO.

DEFINE VARIABLE TOGGLE-2 AS LOGICAL INITIAL yes 
     LABEL "Issue" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .71
     FONT 6 NO-UNDO.

DEFINE VARIABLE TOGGLE-3 AS LOGICAL INITIAL yes 
     LABEL "Transfer" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .71
     FONT 6 NO-UNDO.

DEFINE VARIABLE TOGGLE-4 AS LOGICAL INITIAL yes 
     LABEL "Adjustment" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .71
     FONT 6 NO-UNDO.

DEFINE VARIABLE TOGGLE-5 AS LOGICAL INITIAL yes 
     LABEL "Count" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .71
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     list-order AT ROW 1.48 COL 14 HELP
          "Select List Order" NO-LABEL
     begin_i-no AT ROW 3.86 COL 38 COLON-ALIGNED HELP
          "Enter Beginning <Name>"
     end_i-no AT ROW 3.86 COL 89 COLON-ALIGNED HELP
          "Enter Ending <Name>"
     Begin_procat AT ROW 5.05 COL 38 COLON-ALIGNED
     End_procat AT ROW 5.05 COL 89 COLON-ALIGNED
     begin_date AT ROW 6.24 COL 38 COLON-ALIGNED
     end_date AT ROW 6.24 COL 89 COLON-ALIGNED
     begin_whse AT ROW 7.43 COL 38 COLON-ALIGNED
     end_whse AT ROW 7.43 COL 89 COLON-ALIGNED
     TOGGLE-1 AT ROW 9.81 COL 31
     TOGGLE-2 AT ROW 9.81 COL 48
     TOGGLE-3 AT ROW 9.81 COL 61
     TOGGLE-4 AT ROW 9.81 COL 78
     TOGGLE-5 AT ROW 9.81 COL 98
     F1 AT ROW 3.86 COL 61 NO-LABEL
     F-2 AT ROW 3.86 COL 107 NO-LABEL
     "Transaction Type" VIEW-AS TEXT
          SIZE 22 BY .71 AT ROW 9.81 COL 5
          FONT 6
     RECT-1 AT ROW 1 COL 1
     RECT-3 AT ROW 2.67 COL 1
     "List Order:" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 1.48 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 2.91 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartListObject
   External Tables: ASI.item
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
         HEIGHT             = 11.48
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
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:POPUP-MENU       = MENU POPUP-MENU-F-Main:HANDLE.

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       Begin_procat:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       begin_whse:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       End_procat:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       end_whse:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

/* SETTINGS FOR FILL-IN F-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       list-order:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

ASSIGN 
       TOGGLE-1:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       TOGGLE-2:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       TOGGLE-3:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       TOGGLE-4:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       TOGGLE-5:PRIVATE-DATA IN FRAME F-Main     = 
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


&Scoped-define SELF-NAME TOGGLE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-1 s-object
ON VALUE-CHANGED OF TOGGLE-1 IN FRAME F-Main /* Receipts */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-2 s-object
ON VALUE-CHANGED OF TOGGLE-2 IN FRAME F-Main /* Issue */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-3 s-object
ON VALUE-CHANGED OF TOGGLE-3 IN FRAME F-Main /* Transfer */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-4 s-object
ON VALUE-CHANGED OF TOGGLE-4 IN FRAME F-Main /* Adjustment */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-5 s-object
ON VALUE-CHANGED OF TOGGLE-5 IN FRAME F-Main /* Count */
DO:
    assign {&self-name}.
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


