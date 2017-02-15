&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oe-ctrl.w.w

  Description: Order Processing Control File

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-ctrl ar-ctrl

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME oe-ctrl.i-code ~
ar-ctrl.last-inv oe-ctrl.job-no-def-to-ord oe-ctrl.n-ord oe-ctrl.rng-ord[1] ~
oe-ctrl.rng-ord[2] oe-ctrl.n-bol oe-ctrl.rng-bol[1] oe-ctrl.rng-bol[2] ~
oe-ctrl.n-bar oe-ctrl.rng-bc[1] oe-ctrl.rng-bc[2] oe-ctrl.n-rec ~
oe-ctrl.rng-rec[1] oe-ctrl.rng-rec[2] oe-ctrl.prcom oe-ctrl.f-tax ~
oe-ctrl.prep-chrg oe-ctrl.prep-comm oe-ctrl.prep-tax oe-ctrl.use-ra-no ~
oe-ctrl.ship-from oe-ctrl.u-inv oe-ctrl.p-fact oe-ctrl.p-bol oe-ctrl.p-pick ~
oe-ctrl.p-ack oe-ctrl.p-sep oe-ctrl.pr-broker 
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH oe-ctrl SHARE-LOCK, ~
      EACH ar-ctrl OF oe-ctrl SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME oe-ctrl ar-ctrl
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME oe-ctrl


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-15 RECT-16 RECT-17 RECT-18 RECT-19 ~
Btn_Update Btn_Close 
&Scoped-Define DISPLAYED-FIELDS oe-ctrl.i-code ar-ctrl.last-inv ~
oe-ctrl.job-no-def-to-ord oe-ctrl.n-ord oe-ctrl.rng-ord[1] ~
oe-ctrl.rng-ord[2] oe-ctrl.n-bol oe-ctrl.rng-bol[1] oe-ctrl.rng-bol[2] ~
oe-ctrl.n-bar oe-ctrl.rng-bc[1] oe-ctrl.rng-bc[2] oe-ctrl.n-rec ~
oe-ctrl.rng-rec[1] oe-ctrl.rng-rec[2] oe-ctrl.prcom oe-ctrl.f-tax ~
oe-ctrl.prep-chrg oe-ctrl.prep-comm oe-ctrl.prep-tax oe-ctrl.use-ra-no ~
oe-ctrl.ship-from oe-ctrl.u-inv oe-ctrl.p-fact oe-ctrl.p-bol oe-ctrl.p-pick ~
oe-ctrl.p-ack oe-ctrl.p-sep oe-ctrl.pr-broker 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 oe-ctrl.i-code oe-ctrl.job-no-def-to-ord ~
oe-ctrl.n-ord oe-ctrl.rng-ord[1] oe-ctrl.rng-ord[2] oe-ctrl.n-bol ~
oe-ctrl.rng-bol[1] oe-ctrl.rng-bol[2] oe-ctrl.n-bar oe-ctrl.rng-bc[1] ~
oe-ctrl.rng-bc[2] oe-ctrl.n-rec oe-ctrl.rng-rec[1] oe-ctrl.rng-rec[2] ~
oe-ctrl.prcom oe-ctrl.f-tax oe-ctrl.prep-chrg oe-ctrl.prep-comm ~
oe-ctrl.prep-tax oe-ctrl.use-ra-no oe-ctrl.ship-from oe-ctrl.u-inv ~
oe-ctrl.p-fact oe-ctrl.p-bol oe-ctrl.p-pick oe-ctrl.p-ack oe-ctrl.p-sep ~
oe-ctrl.pr-broker 

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

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 33 BY 1.67.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 17.14.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46 BY 10.48.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 39 BY 3.33.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 39 BY 3.33.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      oe-ctrl, 
      ar-ctrl SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     oe-ctrl.i-code AT ROW 16 COL 24 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Stock", yes,
"Custom", no
          SIZE 22 BY .81
     ar-ctrl.last-inv AT ROW 1.24 COL 35 COLON-ALIGNED
          LABEL "Last Invoice Number"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 7 FGCOLOR 15 
     oe-ctrl.job-no-def-to-ord AT ROW 17 COL 24 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Order", yes,
"Estimate", no
          SIZE 23.2 BY .81
     oe-ctrl.n-ord AT ROW 2.43 COL 35 COLON-ALIGNED
          LABEL "Next Order Number"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.rng-ord[1] AT ROW 2.43 COL 57 COLON-ALIGNED
          LABEL "Range"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.rng-ord[2] AT ROW 2.43 COL 75 COLON-ALIGNED
          LABEL "To"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.n-bol AT ROW 3.62 COL 35 COLON-ALIGNED
          LABEL "Next Bill of Lading Number"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.rng-bol[1] AT ROW 3.62 COL 57 COLON-ALIGNED
          LABEL "Range"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.rng-bol[2] AT ROW 3.62 COL 75 COLON-ALIGNED
          LABEL "To"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.n-bar AT ROW 4.81 COL 35 COLON-ALIGNED
          LABEL "Next Bar Code Label Number"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.rng-bc[1] AT ROW 4.81 COL 57 COLON-ALIGNED
          LABEL "Range"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.rng-bc[2] AT ROW 4.81 COL 75 COLON-ALIGNED
          LABEL "To"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.n-rec AT ROW 6 COL 35 COLON-ALIGNED
          LABEL "Next Receiving Ticket Number"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.rng-rec[1] AT ROW 6 COL 57 COLON-ALIGNED
          LABEL "Range"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.rng-rec[2] AT ROW 6 COL 75 COLON-ALIGNED
          LABEL "To"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.prcom AT ROW 7.91 COL 4
          LABEL "Print Company Name on Invoices"
          VIEW-AS TOGGLE-BOX
          SIZE 36 BY .81
     oe-ctrl.f-tax AT ROW 8.86 COL 4
          LABEL "Charge Tax on Freight"
          VIEW-AS TOGGLE-BOX
          SIZE 25 BY .81
     oe-ctrl.prep-chrg AT ROW 9.81 COL 4
          LABEL "Charge Tax on Prep Chages"
          VIEW-AS TOGGLE-BOX
          SIZE 31 BY .81
     oe-ctrl.prep-comm AT ROW 10.76 COL 4
          LABEL "Pay Commissions on Prep Chages"
          VIEW-AS TOGGLE-BOX
          SIZE 37 BY .81
     oe-ctrl.prep-tax AT ROW 11.71 COL 4
          LABEL "Post Prep Charges to Sales Analysis"
          VIEW-AS TOGGLE-BOX
          SIZE 38 BY .81
     oe-ctrl.use-ra-no AT ROW 12.67 COL 4
          LABEL "Use Return Authorization Numbers"
          VIEW-AS TOGGLE-BOX
          SIZE 37 BY .81
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     oe-ctrl.ship-from AT ROW 13.62 COL 37 COLON-ALIGNED
          LABEL "Ship From"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
          BGCOLOR 15 
     oe-ctrl.u-inv AT ROW 14.81 COL 37 COLON-ALIGNED
          LABEL "Update Inventory When Posting"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
          BGCOLOR 15 
     oe-ctrl.p-fact AT ROW 7.91 COL 50
          LABEL "Print Factory Ticket"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     oe-ctrl.p-bol AT ROW 8.86 COL 50
          LABEL "Print Bill of Lading"
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .81
     oe-ctrl.p-pick AT ROW 9.81 COL 50
          LABEL "Print Release Ticket"
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .81
     oe-ctrl.p-ack AT ROW 12.67 COL 50
          LABEL "Print Totals on Acknowledgement"
          VIEW-AS TOGGLE-BOX
          SIZE 37 BY .81
     oe-ctrl.p-sep AT ROW 13.62 COL 50
          LABEL "Print Seperate Invoice per Release"
          VIEW-AS TOGGLE-BOX
          SIZE 37 BY .81
     oe-ctrl.pr-broker AT ROW 14.57 COL 50
          LABEL "Print Broker on Release / BOL"
          VIEW-AS TOGGLE-BOX
          SIZE 33 BY .81
     Btn_Update AT ROW 16.48 COL 56 HELP
          "Update/Save System Configurations"
     Btn_Close AT ROW 16.48 COL 72 HELP
          "Cancel Update or Close Window"
     RECT-15 AT ROW 16.24 COL 55
     RECT-16 AT ROW 1 COL 1
     RECT-17 AT ROW 7.43 COL 2
     RECT-18 AT ROW 7.43 COL 49
     RECT-19 AT ROW 12.19 COL 49
     "Company Control" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 7.19 COL 4
          FONT 6
     "Default Box Type:" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 16 COL 6
     "Default Job No:" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 16.95 COL 8
     "Credit Control" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 7.19 COL 51
          FONT 6
     "Print Operations" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 11.95 COL 51
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 88.2 BY 17.19.


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
         TITLE              = "Order Processing Control"
         HEIGHT             = 17.19
         WIDTH              = 88.2
         MAX-HEIGHT         = 17.19
         MAX-WIDTH          = 88.2
         VIRTUAL-HEIGHT     = 17.19
         VIRTUAL-WIDTH      = 88.2
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

IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   Custom                                                               */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.f-tax IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR RADIO-SET oe-ctrl.i-code IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR RADIO-SET oe-ctrl.job-no-def-to-ord IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ar-ctrl.last-inv IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-ctrl.n-bar IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.n-bol IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.n-ord IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.n-rec IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.p-ack IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.p-bol IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.p-fact IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.p-pick IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.p-sep IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.pr-broker IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.prcom IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.prep-chrg IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.prep-comm IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.prep-tax IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.rng-bc[1] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.rng-bc[2] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.rng-bol[1] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.rng-bol[2] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.rng-ord[1] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.rng-ord[2] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.rng-rec[1] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.rng-rec[2] IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.ship-from IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.u-inv IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.use-ra-no IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 EXP-LABEL                                                */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "ASI.oe-ctrl,ASI.ar-ctrl OF ASI.oe-ctrl"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME






/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Order Processing Control */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Order Processing Control */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close C-Win
ON CHOOSE OF Btn_Close IN FRAME DEFAULT-FRAME /* Close */
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
    {src/WinKit/triggerend.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Update C-Win
ON CHOOSE OF Btn_Update IN FRAME DEFAULT-FRAME /* Update */
DO:
  IF {&SELF-NAME}:LABEL = "&Update" THEN
  DO WITH FRAME {&FRAME-NAME}:
    ENABLE {&LIST-1}.
    ASSIGN
      {&SELF-NAME}:LABEL = "&Save"
      Btn_Close:LABEL = "&Cancel".
    APPLY "ENTRY" TO oe-ctrl.n-ord.
  END.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&LIST-1}.
    ASSIGN
      {&SELF-NAME}:LABEL = "&Update"
      Btn_Close:LABEL = "&Close".
    ASSIGN {&LIST-1}.
  END.
    {src/WinKit/triggerend.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i}
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FIND ar-ctrl WHERE ar-ctrl.company = gcompany NO-LOCK NO-ERROR.
  FIND oe-ctrl WHERE oe-ctrl.company = gcompany NO-LOCK NO-ERROR.
  RUN enable_UI.
  {methods/nowait.i}
    {Advantzware/WinKit/embedfinalize-nonadm.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  IF AVAILABLE ar-ctrl THEN 
    DISPLAY ar-ctrl.last-inv 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE oe-ctrl THEN 
    DISPLAY oe-ctrl.i-code oe-ctrl.job-no-def-to-ord oe-ctrl.n-ord 
          oe-ctrl.rng-ord[1] oe-ctrl.rng-ord[2] oe-ctrl.n-bol oe-ctrl.rng-bol[1] 
          oe-ctrl.rng-bol[2] oe-ctrl.n-bar oe-ctrl.rng-bc[1] oe-ctrl.rng-bc[2] 
          oe-ctrl.n-rec oe-ctrl.rng-rec[1] oe-ctrl.rng-rec[2] oe-ctrl.prcom 
          oe-ctrl.f-tax oe-ctrl.prep-chrg oe-ctrl.prep-comm oe-ctrl.prep-tax 
          oe-ctrl.use-ra-no oe-ctrl.ship-from oe-ctrl.u-inv oe-ctrl.p-fact 
          oe-ctrl.p-bol oe-ctrl.p-pick oe-ctrl.p-ack oe-ctrl.p-sep 
          oe-ctrl.pr-broker 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-15 RECT-16 RECT-17 RECT-18 RECT-19 Btn_Update Btn_Close 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


