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

DEF VAR giCurrOrd AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME oe-ctrl

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Close Btn_Update n-ord RECT-17 RECT-18 ~
RECT-19 
&Scoped-Define DISPLAYED-FIELDS oe-ctrl.i-code ar-ctrl.last-inv ~
oe-ctrl.job-no-def-to-ord oe-ctrl.rng-ord[1] oe-ctrl.rng-ord[2] ~
oe-ctrl.n-bol oe-ctrl.rng-bol[1] oe-ctrl.rng-bol[2] oe-ctrl.n-bar ~
oe-ctrl.rng-bc[1] oe-ctrl.rng-bc[2] oe-ctrl.n-rec oe-ctrl.rng-rec[1] ~
oe-ctrl.rng-rec[2] oe-ctrl.prcom oe-ctrl.f-tax oe-ctrl.prep-chrg ~
oe-ctrl.prep-comm oe-ctrl.prep-tax oe-ctrl.use-ra-no oe-ctrl.ship-from ~
oe-ctrl.u-inv oe-ctrl.p-fact oe-ctrl.p-bol oe-ctrl.p-pick oe-ctrl.p-ack ~
oe-ctrl.p-sep oe-ctrl.pr-broker 
&Scoped-define DISPLAYED-TABLES oe-ctrl ar-ctrl
&Scoped-define FIRST-DISPLAYED-TABLE oe-ctrl
&Scoped-define SECOND-DISPLAYED-TABLE ar-ctrl
&Scoped-Define DISPLAYED-OBJECTS tgCreateSSBol n-ord fNextRFIDNum 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 tgCreateSSBol oe-ctrl.i-code ~
oe-ctrl.job-no-def-to-ord n-ord oe-ctrl.rng-ord[1] oe-ctrl.rng-ord[2] ~
oe-ctrl.n-bol oe-ctrl.rng-bol[1] oe-ctrl.rng-bol[2] oe-ctrl.n-bar ~
oe-ctrl.rng-bc[1] oe-ctrl.rng-bc[2] oe-ctrl.n-rec oe-ctrl.rng-rec[1] ~
oe-ctrl.rng-rec[2] oe-ctrl.prcom oe-ctrl.f-tax oe-ctrl.prep-chrg ~
oe-ctrl.prep-comm oe-ctrl.prep-tax oe-ctrl.use-ra-no oe-ctrl.u-inv ~
oe-ctrl.p-fact oe-ctrl.p-bol oe-ctrl.p-pick oe-ctrl.p-ack oe-ctrl.p-sep ~
oe-ctrl.pr-broker fNextRFIDNum 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Close  NO-FOCUS FLAT-BUTTON
     LABEL "&Close" 
     SIZE 8 BY 1.91.

DEFINE BUTTON Btn_Update  NO-FOCUS FLAT-BUTTON
     LABEL "&Update" 
     SIZE 8 BY 1.91.

DEFINE VARIABLE fNextRFIDNum AS CHARACTER FORMAT "x(24)" 
     LABEL "Next RFID Tag Number" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1.

DEFINE VARIABLE n-ord LIKE oe-ctrl.n-ord
     LABEL "Next Order Number" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 12.38.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 5.48.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 5.48.

DEFINE VARIABLE tgCreateSSBol AS LOGICAL INITIAL no 
     LABEL "Create Sharp Shooter BOL?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME oe-ctrl
     Btn_Close AT ROW 22.19 COL 59 HELP
          "Cancel Update or Close Window"
     Btn_Update AT ROW 22.19 COL 51 HELP
          "Update/Save System Configurations"
     tgCreateSSBol AT ROW 13.86 COL 69.2 WIDGET-ID 8
     oe-ctrl.i-code AT ROW 19.62 COL 24 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Stock", yes,
"Custom", no
          SIZE 22 BY .81
     ar-ctrl.last-inv AT ROW 1.24 COL 32 COLON-ALIGNED
          LABEL "Last Invoice Number"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 7 FGCOLOR 15 
     oe-ctrl.job-no-def-to-ord AT ROW 20.86 COL 24 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Order", yes,
"Estimate", no
          SIZE 23.2 BY .81
     n-ord AT ROW 2.43 COL 32 COLON-ALIGNED HELP
          "Enter order number to be used for next order"
          LABEL "Next Order Number"
          BGCOLOR 15 
     oe-ctrl.rng-ord[1] AT ROW 2.43 COL 71 COLON-ALIGNED
          LABEL "Range"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.rng-ord[2] AT ROW 2.43 COL 89 COLON-ALIGNED
          LABEL "To"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.n-bol AT ROW 3.62 COL 32 COLON-ALIGNED
          LABEL "Next Bill of Lading Number"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.rng-bol[1] AT ROW 3.62 COL 71 COLON-ALIGNED
          LABEL "Range"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.rng-bol[2] AT ROW 3.62 COL 89 COLON-ALIGNED
          LABEL "To"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.n-bar AT ROW 4.81 COL 32 COLON-ALIGNED
          LABEL "Next Bar Code Label Number"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.rng-bc[1] AT ROW 4.81 COL 71 COLON-ALIGNED
          LABEL "Range"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.rng-bc[2] AT ROW 4.81 COL 89 COLON-ALIGNED
          LABEL "To"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.n-rec AT ROW 6 COL 32 COLON-ALIGNED
          LABEL "Next Receiving Ticket Number"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.rng-rec[1] AT ROW 6 COL 71 COLON-ALIGNED
          LABEL "Range"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.rng-rec[2] AT ROW 6 COL 89 COLON-ALIGNED
          LABEL "To"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     oe-ctrl.prcom AT ROW 10.29 COL 4
          LABEL "Print Company Name on Invoices"
          VIEW-AS TOGGLE-BOX
          SIZE 36 BY .81
     oe-ctrl.f-tax AT ROW 11.48 COL 4
          LABEL "Charge Tax on Freight"
          VIEW-AS TOGGLE-BOX
          SIZE 25 BY .81
     oe-ctrl.prep-chrg AT ROW 12.43 COL 4
          LABEL "Charge Tax on Prep Charges"
          VIEW-AS TOGGLE-BOX
          SIZE 37 BY .81
     oe-ctrl.prep-comm AT ROW 13.38 COL 4
          LABEL "Pay Commissions on Prep Charges"
          VIEW-AS TOGGLE-BOX
          SIZE 39 BY .81
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 108 BY 23.38.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME oe-ctrl
     oe-ctrl.prep-tax AT ROW 14.33 COL 4
          LABEL "Post Prep Charges to Sales Analysis"
          VIEW-AS TOGGLE-BOX
          SIZE 38 BY .81
     oe-ctrl.use-ra-no AT ROW 15.29 COL 4
          LABEL "Use Return Authorization Numbers"
          VIEW-AS TOGGLE-BOX
          SIZE 37 BY .81
     oe-ctrl.ship-from AT ROW 17 COL 34 COLON-ALIGNED
          LABEL "Ship From"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
          BGCOLOR 15 
     oe-ctrl.u-inv AT ROW 17.95 COL 34 COLON-ALIGNED
          LABEL "Update Inventory When Posting"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
          BGCOLOR 15 
     oe-ctrl.p-fact AT ROW 10.52 COL 69
          LABEL "Print Factory Ticket"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     oe-ctrl.p-bol AT ROW 11.71 COL 69
          LABEL "Print Bill of Lading"
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .81
     oe-ctrl.p-pick AT ROW 12.91 COL 69
          LABEL "Release/Print Release Ticket"
          VIEW-AS TOGGLE-BOX
          SIZE 34 BY .81
     oe-ctrl.p-ack AT ROW 17.43 COL 70
          LABEL "Print Totals on Acknowledgement"
          VIEW-AS TOGGLE-BOX
          SIZE 37 BY .81
     oe-ctrl.p-sep AT ROW 18.86 COL 70
          LABEL "Print Seperate Invoice per Release"
          VIEW-AS TOGGLE-BOX
          SIZE 37 BY .81
     oe-ctrl.pr-broker AT ROW 20.05 COL 70
          LABEL "Print Broker on Release / BOL"
          VIEW-AS TOGGLE-BOX
          SIZE 33 BY .81
     fNextRFIDNum AT ROW 7.19 COL 32 COLON-ALIGNED WIDGET-ID 6
     "Company Control" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 8.86 COL 4
          FGCOLOR 9 FONT 6
     "Default Box Type:" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 19.1 COL 6
     "Default Job No:" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 20.29 COL 8
     "Credit Control" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 9.1 COL 69
          FGCOLOR 9 FONT 6
     "Print Operations" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 15.76 COL 69
          FGCOLOR 9 FONT 6
     RECT-17 AT ROW 9.57 COL 2
     RECT-18 AT ROW 9.81 COL 68
     RECT-19 AT ROW 16.48 COL 69
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 108 BY 23.38.


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
         HEIGHT             = 23.38
         WIDTH              = 108
         MAX-HEIGHT         = 23.38
         MAX-WIDTH          = 120.8
         VIRTUAL-HEIGHT     = 23.38
         VIRTUAL-WIDTH      = 120.8
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME oe-ctrl
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.f-tax IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN fNextRFIDNum IN FRAME oe-ctrl
   NO-ENABLE 1                                                          */
/* SETTINGS FOR RADIO-SET oe-ctrl.i-code IN FRAME oe-ctrl
   NO-ENABLE 1                                                          */
/* SETTINGS FOR RADIO-SET oe-ctrl.job-no-def-to-ord IN FRAME oe-ctrl
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ar-ctrl.last-inv IN FRAME oe-ctrl
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-ctrl.n-bar IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.n-bol IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN n-ord IN FRAME oe-ctrl
   1 LIKE = asi.oe-ctrl. EXP-LABEL EXP-SIZE                             */
/* SETTINGS FOR FILL-IN oe-ctrl.n-rec IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.p-ack IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.p-bol IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.p-fact IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.p-pick IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.p-sep IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.pr-broker IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.prcom IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.prep-chrg IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.prep-comm IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.prep-tax IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.rng-bc[1] IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.rng-bc[2] IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.rng-bol[1] IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.rng-bol[2] IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.rng-ord[1] IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.rng-ord[2] IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.rng-rec[1] IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.rng-rec[2] IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ctrl.ship-from IN FRAME oe-ctrl
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX tgCreateSSBol IN FRAME oe-ctrl
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN oe-ctrl.u-inv IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.use-ra-no IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME oe-ctrl
/* Query rebuild information for FRAME oe-ctrl
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME oe-ctrl */
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
ON CHOOSE OF Btn_Close IN FRAME oe-ctrl /* Close */
DO:
  IF {&SELF-NAME}:LABEL = "&Close" THEN
  APPLY "CLOSE" TO THIS-PROCEDURE.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
   {methods/setButton.i Btn_Update "Update"}
   {methods/setButton.i Btn_Close "Close"}
    RUN enable_UI.
    RUN sys/ref/asicurseq.p (INPUT gcompany, INPUT "order_seq", OUTPUT giCurrOrd) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
    n-ord:SCREEN-VALUE = STRING(giCurrOrd  + 1, ">>>>>>").
  END.
    {Advantzware/WinKit/winkit-panel-triggerend.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Update C-Win
ON CHOOSE OF Btn_Update IN FRAME oe-ctrl /* Update */
DO:

  DEF VAR liNextOrder AS INT NO-UNDO.
  IF {&SELF-NAME}:LABEL = "&Update" THEN
  DO WITH FRAME {&FRAME-NAME}:
    tgCreateSSBol:SCREEN-VALUE = (IF oe-ctrl.spare-int-1 EQ 1 THEN "YES" ELSE "NO").
    ENABLE {&LIST-1}.
    RUN sys/ref/asicurseq.p (INPUT gcompany, INPUT "order_seq", OUTPUT giCurrOrd) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
    FIND CURRENT oe-ctrl EXCLUSIVE-LOCK.

    oe-ctrl.n-ord = giCurrOrd + 1.
    n-ord:SCREEN-VALUE = STRING(giCurrOrd  + 1, ">>>>>>").

    IF CAN-FIND(FIRST inv-head WHERE inv-head.company EQ gcompany AND
                inv-head.multi-invoice = no) THEN
      DISABLE oe-ctrl.u-inv.
           {methods/setButton.i Btn_Update "Save"}
           {methods/setButton.i Btn_Close "Cancel"}

    APPLY "ENTRY" TO n-ord.
    n-ord:SCREEN-VALUE = STRING(giCurrOrd  + 1, ">>>>>>").

  END.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:

   /* IF LENGTH(fNextRFIDZip:SCREEN-VALUE) <> 5 THEN DO:
       MESSAGE "RFID Zip code must have 5 digit character."
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN NO-APPLY.
    END.
    */
    IF LENGTH(fNextRFIDNum:SCREEN-VALUE) <> 24 THEN DO:
       MESSAGE "RFID Next Number uses 24 digit number."
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN NO-APPLY.
    END.
    FIND CURRENT ar-ctrl EXCLUSIVE-LOCK.
    FIND CURRENT oe-ctrl EXCLUSIVE-LOCK.
    ASSIGN {&LIST-1}.

    oe-ctrl.spare-char-1 = fNextRFIDNum.
    ASSIGN tgCreateSSBol.
    oe-ctrl.spare-int-1 = (IF tgCreateSSBol THEN 1 ELSE 0).
    DISABLE {&LIST-1}.
    DISABLE tgCreateSSBol.
   {methods/setButton.i Btn_Update "Update"}
   {methods/setButton.i Btn_Close "Close"}
    FIND CURRENT ar-ctrl NO-LOCK.

    FIND company WHERE company.company EQ gcompany NO-LOCK NO-ERROR.
    liNextOrder = INTEGER(n-ord:SCREEN-VALUE).
    /* Sequence holds the current value. They've entered the next value, so */
    /* subtract 1 to make it the correct current value */
     liNextOrder = liNextOrder - 1.
     DYNAMIC-CURRENT-VALUE("order_seq" + company.spare-char-1, "ASI") = liNextOrder.
     n-ord:SCREEN-VALUE = STRING(DYNAMIC-CURRENT-VALUE("order_seq" + company.spare-char-1, "ASI") + 1, ">>>>>>").
     FIND CURRENT oe-ctrl EXCLUSIVE-LOCK.
     oe-ctrl.n-ord = INTEGER(n-ord:SCREEN-VALUE).

     FIND CURRENT oe-ctrl NO-LOCK.
  END.
    {Advantzware/WinKit/winkit-panel-triggerend.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME n-ord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL n-ord C-Win
ON ENTRY OF n-ord IN FRAME oe-ctrl /* Next Order Number */
DO:
  n-ord:SCREEN-VALUE = STRING(current-value(order_seq), ">>>>>>").
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

  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN.
  END.


  DISABLE tgCreateSSBol.
  DO TRANSACTION:
    IF NOT CAN-FIND(FIRST ar-ctrl WHERE ar-ctrl.company EQ gcompany) THEN DO:
      CREATE ar-ctrl.
      ar-ctrl.company = gcompany.
    END.
    IF NOT CAN-FIND(FIRST oe-ctrl WHERE oe-ctrl.company EQ gcompany) THEN DO:
      CREATE oe-ctrl.
      oe-ctrl.company = gcompany.
    END.

    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ gcompany NO-ERROR.
    oe-ctrl.ship-from = YES.
  END.

  FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ gcompany NO-LOCK NO-ERROR.
  FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ gcompany NO-LOCK NO-ERROR.
  ENABLE tgCreateSSBol.
  tgCreateSSBol:SCREEN-VALUE = (IF oe-ctrl.spare-int-1 EQ 1 THEN "YES" ELSE "NO").
  DISABLE tgCreateSSBol.
  RUN enable_UI.

  ASSIGN fNextRFIDNum:SCREEN-VALUE = IF oe-ctrl.spare-char-1 = "" THEN "111110000000000000000000"
                                     ELSE oe-ctrl.spare-char-1
         tgCreateSSBol:SCREEN-VALUE = (IF oe-ctrl.spare-int-1 EQ 1 THEN "YES" ELSE "NO").
  RUN sys/ref/asicurseq.p (INPUT gcompany, INPUT "order_seq", OUTPUT giCurrOrd) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

  n-ord:SCREEN-VALUE = STRING(giCurrOrd  + 1, ">>>>>>").
  n-ord:SENSITIVE = NO.
   {methods/nowait.i}
   {methods/setButton.i Btn_Update "Update"}
   {methods/setButton.i Btn_Close "Close"}
   {Advantzware/WinKit/embedfinalize-nonadm.i}
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
  DISPLAY tgCreateSSBol n-ord fNextRFIDNum 
      WITH FRAME oe-ctrl IN WINDOW C-Win.
  IF AVAILABLE ar-ctrl THEN 
    DISPLAY ar-ctrl.last-inv 
      WITH FRAME oe-ctrl IN WINDOW C-Win.
  IF AVAILABLE oe-ctrl THEN 
    DISPLAY oe-ctrl.i-code oe-ctrl.job-no-def-to-ord oe-ctrl.rng-ord[1] 
          oe-ctrl.rng-ord[2] oe-ctrl.n-bol oe-ctrl.rng-bol[1] oe-ctrl.rng-bol[2] 
          oe-ctrl.n-bar oe-ctrl.rng-bc[1] oe-ctrl.rng-bc[2] oe-ctrl.n-rec 
          oe-ctrl.rng-rec[1] oe-ctrl.rng-rec[2] oe-ctrl.prcom oe-ctrl.f-tax 
          oe-ctrl.prep-chrg oe-ctrl.prep-comm oe-ctrl.prep-tax oe-ctrl.use-ra-no 
          oe-ctrl.ship-from oe-ctrl.u-inv oe-ctrl.p-fact oe-ctrl.p-bol 
          oe-ctrl.p-pick oe-ctrl.p-ack oe-ctrl.p-sep oe-ctrl.pr-broker 
      WITH FRAME oe-ctrl IN WINDOW C-Win.
  ENABLE Btn_Close Btn_Update n-ord RECT-17 RECT-18 RECT-19 
      WITH FRAME oe-ctrl IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-oe-ctrl}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

