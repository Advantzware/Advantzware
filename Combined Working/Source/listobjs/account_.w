&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: listobjs/account_.w

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
DEFINE VARIABLE account-format AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE schema-account-format AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartListObject
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES account
&Scoped-define FIRST-EXTERNAL-TABLE account


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR account.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 list-order begin_actnum ~
end_actnum show-history 
&Scoped-Define DISPLAYED-OBJECTS list-order begin_actnum begin_account_dscr ~
end_account_dscr show-history F1 F-2 F-3 F-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define F1 F1 F-2 F-3 F-4 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE begin_account_dscr AS CHARACTER FORMAT "X(30)" 
     LABEL "Beginning Description" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.

DEFINE VARIABLE begin_actnum AS CHARACTER FORMAT "X(25)" 
     LABEL "Beginning  Account" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE end_account_dscr AS CHARACTER FORMAT "X(30)" 
     LABEL "Ending Description" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.

DEFINE VARIABLE end_actnum AS CHARACTER FORMAT "X(25)" 
     LABEL "Ending Account" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

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

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 120 BY 8.81.

DEFINE VARIABLE show-history AS LOGICAL INITIAL no 
     LABEL "Show History" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     list-order AT ROW 1.48 COL 14 HELP
          "Select List Order" NO-LABEL
     begin_actnum AT ROW 4.1 COL 45 COLON-ALIGNED HELP
          "Enter Beginning Account"
     begin_account_dscr AT ROW 5.29 COL 45 COLON-ALIGNED HELP
          "Enter Beginning Description"
     end_actnum AT ROW 7.19 COL 45 COLON-ALIGNED HELP
          "Enter Ending Account"
     end_account_dscr AT ROW 8.38 COL 45 COLON-ALIGNED HELP
          "Enter Ending Description"
     show-history AT ROW 10.29 COL 47 HELP
          "Select to Show History"
     F1 AT ROW 4.1 COL 79 NO-LABEL
     F-2 AT ROW 5.29 COL 91 NO-LABEL
     F-3 AT ROW 7.19 COL 79 NO-LABEL
     F-4 AT ROW 8.38 COL 91 NO-LABEL
     "List Order:" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 1.48 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 2.91 COL 2
     RECT-1 AT ROW 1 COL 1
     RECT-2 AT ROW 3.14 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartListObject
   External Tables: ASI.account
   Allow: Basic,DB-Fields
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
         HEIGHT             = 10.95
         WIDTH              = 120.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}
{methods/enhance.i}
{methods/listobjs/listobjs.i}

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

/* SETTINGS FOR FILL-IN begin_account_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN end_account_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN end_actnum IN FRAME F-Main
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN F-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN F-3 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN F-4 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */

ASSIGN 
       list-order:PRIVATE-DATA IN FRAME F-Main       = 
                "export".

ASSIGN 
       show-history:PRIVATE-DATA IN FRAME F-Main     = 
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME begin_actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_actnum s-object
ON LEAVE OF begin_actnum IN FRAME F-Main
DO:
   IF LASTKEY = -1 THEN RETURN.
   ASSIGN {&self-name}.

    FIND account 
        WHERE account.actnum = begin_actnum:SCREEN-VALUE 
          AND account.company = gcompany
        NO-LOCK NO-ERROR.
    IF AVAIL account THEN
      ASSIGN
        begin_account_dscr:SCREEN-VALUE = account.dscr.
    ELSE DO:
       MESSAGE "Invalid Account Number. Try Help." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_actnum s-object
ON LEAVE OF end_actnum IN FRAME F-Main
DO:
   IF LASTKEY = -1 THEN RETURN.
   ASSIGN {&self-name}.

    FIND FIRST account 
        WHERE account.actnum = end_actnum:SCREEN-VALUE 
          AND account.company = gcompany
        NO-LOCK NO-ERROR.
    IF AVAIL account THEN
    end_account_dscr:SCREEN-VALUE = account.dscr.
    ELSE DO:
       MESSAGE "Invalid Account Number. Try Help." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_actnum s-object
ON HELP OF begin_actnum IN FRAME F-Main /* Account Number */
DO:
  DEF VAR char-val AS CHAR NO-UNDO.

  RUN windows/l-acct.w (gcompany, "", begin_actnum:SCREEN-VALUE, OUTPUT char-val).
  IF char-val NE "" AND ENTRY(1,char-val) NE begin_actnum:SCREEN-VALUE THEN DO:
    begin_actnum:SCREEN-VALUE = ENTRY(1,char-val).
    RUN new-begin_acct.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_actnum s-object
ON HELP OF end_actnum IN FRAME F-Main /* Account Number */
DO:
  DEF VAR char-val AS CHAR NO-UNDO.

  RUN windows/l-acct.w (gcompany, "", end_actnum:SCREEN-VALUE, OUTPUT char-val).
  IF char-val NE "" AND ENTRY(1,char-val) NE end_actnum:SCREEN-VALUE THEN DO:
    end_actnum:SCREEN-VALUE = ENTRY(1,char-val).
    RUN new-end_acct.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-begin_acct s-object 
PROCEDURE new-begin_acct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND account
        WHERE account.company EQ gcompany
          AND account.actnum  EQ begin_actnum:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL account THEN
      ASSIGN
       begin_account_dscr:SCREEN-VALUE = account.dscr.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-begin_acct s-object 
PROCEDURE new-end_acct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND account
        WHERE account.company EQ gcompany
          AND account.actnum  EQ end_actnum:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL account THEN
      ASSIGN
       end_account_dscr:SCREEN-VALUE = account.dscr.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
