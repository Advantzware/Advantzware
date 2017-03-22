&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap-ctrl.w.w

  Description: G/L Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/format.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME ap-ctrl

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Update Btn_Close RECT-15 RECT-16 
&Scoped-Define DISPLAYED-FIELDS ap-ctrl.payables ap-ctrl.purchases ~
ap-ctrl.cash-act ap-ctrl.discount ap-ctrl.stax ap-ctrl.freight 
&Scoped-define DISPLAYED-TABLES ap-ctrl
&Scoped-define FIRST-DISPLAYED-TABLE ap-ctrl


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 ap-ctrl.payables ap-ctrl.purchases ap-ctrl.cash-act ~
ap-ctrl.discount ap-ctrl.stax ap-ctrl.freight 
&Scoped-define F1 F1 F-2 F-3 F-4 F-5 F-6 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Close 
     LABEL "&Close" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Update 
     LABEL "&Update" 
     SIZE 15 BY 1.14.

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

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 33 BY 1.67.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 51 BY 7.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME ap-ctrl
     ap-ctrl.payables AT ROW 1.24 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ap-ctrl.purchases AT ROW 2.43 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ap-ctrl.cash-act AT ROW 3.62 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ap-ctrl.discount AT ROW 4.81 COL 20 COLON-ALIGNED
          LABEL "Discount Taken"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ap-ctrl.stax AT ROW 6 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ap-ctrl.freight AT ROW 7.19 COL 20 COLON-ALIGNED FORMAT "x(25)"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     Btn_Update AT ROW 9.57 COL 11 HELP
          "Update/Save System Configurations"
     Btn_Close AT ROW 9.57 COL 27 HELP
          "Cancel Update or Close Window"
     F1 AT ROW 1.24 COL 49 NO-LABEL
     F-2 AT ROW 2.43 COL 49 NO-LABEL
     F-3 AT ROW 3.62 COL 49 NO-LABEL
     F-4 AT ROW 4.81 COL 49 NO-LABEL
     F-5 AT ROW 6 COL 49 NO-LABEL
     F-6 AT ROW 7.19 COL 49 NO-LABEL
     RECT-15 AT ROW 9.33 COL 10
     RECT-16 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 51.6 BY 10.81.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "A/P Control"
         HEIGHT             = 10.81
         WIDTH              = 51.6
         MAX-HEIGHT         = 10.81
         MAX-WIDTH          = 51.6
         VIRTUAL-HEIGHT     = 10.81
         VIRTUAL-WIDTH      = 51.6
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME ap-ctrl
                                                                        */
ASSIGN
       Btn_Close:PRIVATE-DATA IN FRAME ap-ctrl     = 
                "ribbon-button".


/* SETTINGS FOR FILL-IN ap-ctrl.cash-act IN FRAME ap-ctrl
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ap-ctrl.discount IN FRAME ap-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN F-2 IN FRAME ap-ctrl
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-2:HIDDEN IN FRAME ap-ctrl           = TRUE.

/* SETTINGS FOR FILL-IN F-3 IN FRAME ap-ctrl
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-3:HIDDEN IN FRAME ap-ctrl           = TRUE.

/* SETTINGS FOR FILL-IN F-4 IN FRAME ap-ctrl
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-4:HIDDEN IN FRAME ap-ctrl           = TRUE.

/* SETTINGS FOR FILL-IN F-5 IN FRAME ap-ctrl
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-5:HIDDEN IN FRAME ap-ctrl           = TRUE.

/* SETTINGS FOR FILL-IN F-6 IN FRAME ap-ctrl
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-6:HIDDEN IN FRAME ap-ctrl           = TRUE.

/* SETTINGS FOR FILL-IN F1 IN FRAME ap-ctrl
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F1:HIDDEN IN FRAME ap-ctrl           = TRUE.

/* SETTINGS FOR FILL-IN ap-ctrl.freight IN FRAME ap-ctrl
   NO-ENABLE 1 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN ap-ctrl.payables IN FRAME ap-ctrl
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ap-ctrl.purchases IN FRAME ap-ctrl
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ap-ctrl.stax IN FRAME ap-ctrl
   NO-ENABLE 1                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* A/P Control */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* A/P Control */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close C-Win
ON CHOOSE OF Btn_Close IN FRAME ap-ctrl /* Close */
DO:
  IF {&SELF-NAME}:LABEL = "&Close" THEN
  APPLY "CLOSE" TO THIS-PROCEDURE.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
    ASSIGN
      {&SELF-NAME}:LABEL = "&Close"
      Btn_Update:LABEL = "&Update".
    RUN enable_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Update C-Win
ON CHOOSE OF Btn_Update IN FRAME ap-ctrl /* Update */
DO:
  IF {&SELF-NAME}:LABEL = "&Update" THEN
  DO WITH FRAME {&FRAME-NAME}:
    ENABLE {&LIST-1}.
    DISPLAY {&F1}.
    ASSIGN
      {&SELF-NAME}:LABEL = "&Save"
      Btn_Close:LABEL = "&Cancel".
    APPLY "ENTRY" TO ap-ctrl.payables.
  END.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    /* VALIDATION */
    DEF VAR v-avail AS LOG NO-UNDO.

    {custom/validate/acct.i ap-ctrl.payables}
    {custom/validate/acct.i ap-ctrl.purchases}
    {custom/validate/acct.i ap-ctrl.cash-act}
    {custom/validate/acct.i ap-ctrl.discount}
    {custom/validate/acct.i ap-ctrl.stax}
    {custom/validate/acct.i ap-ctrl.freight}

    DISABLE {&LIST-1}.
    HIDE {&F1} NO-PAUSE.
    ASSIGN
      {&SELF-NAME}:LABEL = "&Update"
      Btn_Close:LABEL = "&Close".

    FIND CURRENT ap-ctrl EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN {&LIST-1}.
    FIND CURRENT ap-ctrl NO-LOCK NO-ERROR.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-ctrl.cash-act
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.cash-act C-Win
ON ENTRY OF ap-ctrl.cash-act IN FRAME ap-ctrl /* Cash Account */
DO:
  /*{custom/actentry.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.cash-act C-Win
ON LEAVE OF ap-ctrl.cash-act IN FRAME ap-ctrl /* Cash Account */
DO:
     DEF VAR v-avail AS LOG NO-UNDO.
  {custom/actleave.i}
  {custom/validate/acct.i ap-ctrl.cash-act}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-ctrl.discount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.discount C-Win
ON ENTRY OF ap-ctrl.discount IN FRAME ap-ctrl /* Discount Taken */
DO:
  /*{custom/actentry.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.discount C-Win
ON LEAVE OF ap-ctrl.discount IN FRAME ap-ctrl /* Discount Taken */
DO:
     DEF VAR v-avail AS LOG NO-UNDO.
  {custom/actleave.i}
      {custom/validate/acct.i ap-ctrl.discount}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-ctrl.freight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.freight C-Win
ON LEAVE OF ap-ctrl.freight IN FRAME ap-ctrl /* Freight Account */
DO:
     DEF VAR v-avail AS LOG NO-UNDO.
  {custom/actleave.i}
      {custom/validate/acct.i ap-ctrl.freight}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-ctrl.payables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.payables C-Win
ON ENTRY OF ap-ctrl.payables IN FRAME ap-ctrl /* Accounts Payable */
DO:
  /*{custom/actentry.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.payables C-Win
ON LEAVE OF ap-ctrl.payables IN FRAME ap-ctrl /* Accounts Payable */
DO:
  DEF VAR v-avail AS LOG NO-UNDO.
  {custom/actleave.i}
   {custom/validate/acct.i ap-ctrl.payables}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-ctrl.purchases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.purchases C-Win
ON ENTRY OF ap-ctrl.purchases IN FRAME ap-ctrl /* Purchases Account */
DO:
  /*{custom/actentry.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.purchases C-Win
ON LEAVE OF ap-ctrl.purchases IN FRAME ap-ctrl /* Purchases Account */
DO:
     DEF VAR v-avail AS LOG NO-UNDO.
  {custom/actleave.i}
  {custom/validate/acct.i ap-ctrl.purchases}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-ctrl.stax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.stax C-Win
ON ENTRY OF ap-ctrl.stax IN FRAME ap-ctrl /* Sales Tax */
DO:
  /*{custom/actentry.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-ctrl.stax C-Win
ON LEAVE OF ap-ctrl.stax IN FRAME ap-ctrl /* Sales Tax */
DO:
  /*{custom/actleave.i} */
    DEF VAR v-avail AS LOG NO-UNDO.
    {custom/validate/acct.i ap-ctrl.stax}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN.
  END.

  IF NOT CAN-FIND(FIRST ap-ctrl WHERE ap-ctrl.company EQ gcompany) THEN DO:
    CREATE ap-ctrl.
    ap-ctrl.company = gcompany.
  END.
  FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ gcompany NO-LOCK NO-ERROR.

  RUN enable_UI.
  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  IF AVAILABLE ap-ctrl THEN 
    DISPLAY ap-ctrl.payables ap-ctrl.purchases ap-ctrl.cash-act ap-ctrl.discount 
          ap-ctrl.stax ap-ctrl.freight 
      WITH FRAME ap-ctrl IN WINDOW C-Win.
  ENABLE Btn_Update Btn_Close RECT-15 RECT-16 
      WITH FRAME ap-ctrl IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-ap-ctrl}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

