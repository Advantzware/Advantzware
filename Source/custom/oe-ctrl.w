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
{sys/ref/sys-ctrl.i}

DEF VAR giCurrOrd AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME oe-ctrl
&Scoped-define BROWSE-NAME brHoldTests

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES sys-ctrl

/* Definitions for BROWSE brHoldTests                                   */
&Scoped-define FIELDS-IN-QUERY-brHoldTests sys-ctrl.name sys-ctrl.descrip ~
sys-ctrl.log-fld sys-ctrl.char-fld 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brHoldTests sys-ctrl.log-fld ~
sys-ctrl.char-fld 
&Scoped-define ENABLED-TABLES-IN-QUERY-brHoldTests sys-ctrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brHoldTests sys-ctrl
&Scoped-define QUERY-STRING-brHoldTests FOR EACH sys-ctrl ~
      WHERE sys-ctrl.company EQ gcompany AND sys-ctrl.module = "VAL" NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brHoldTests OPEN QUERY brHoldTests FOR EACH sys-ctrl ~
      WHERE sys-ctrl.company EQ gcompany AND sys-ctrl.module = "VAL" NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brHoldTests sys-ctrl
&Scoped-define FIRST-TABLE-IN-QUERY-brHoldTests sys-ctrl


/* Definitions for FRAME oe-ctrl                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-oe-ctrl ~
    ~{&OPEN-QUERY-brHoldTests}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS n-ord Btn_Update Btn_Close 
&Scoped-Define DISPLAYED-FIELDS ar-ctrl.last-inv oe-ctrl.n-bol ~
oe-ctrl.p-fact oe-ctrl.p-job oe-ctrl.p-bol oe-ctrl.p-pick oe-ctrl.p-sep 
&Scoped-define DISPLAYED-TABLES ar-ctrl oe-ctrl
&Scoped-define FIRST-DISPLAYED-TABLE ar-ctrl
&Scoped-define SECOND-DISPLAYED-TABLE oe-ctrl
&Scoped-Define DISPLAYED-OBJECTS fiOptions tgCreateSSBol n-ord fNextRFIDNum ~
fiHoldTests 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 tgCreateSSBol n-ord oe-ctrl.n-bol oe-ctrl.p-fact ~
oe-ctrl.p-job oe-ctrl.p-bol oe-ctrl.p-pick oe-ctrl.p-sep fNextRFIDNum 

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

DEFINE VARIABLE fiHoldTests AS CHARACTER FORMAT "X(256)":U INITIAL "Order Hold Tests:" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE fiOptions AS CHARACTER FORMAT "X(256)":U INITIAL "When order is on hold:" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE fNextRFIDNum AS CHARACTER FORMAT "x(24)" 
     LABEL "Next RFID Tag Number" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1.

DEFINE VARIABLE n-ord LIKE oe-ctrl.n-ord
     LABEL "Next Order Number" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tgCreateSSBol AS LOGICAL INITIAL no 
     LABEL "Allow Creating of BOL in Sharp Shooter" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brHoldTests FOR 
      sys-ctrl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brHoldTests
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brHoldTests C-Win _STRUCTURED
  QUERY brHoldTests NO-LOCK DISPLAY
      sys-ctrl.name FORMAT "x(16)":U WIDTH 16.2
      sys-ctrl.descrip FORMAT "x(40)":U WIDTH 45.2
      sys-ctrl.log-fld COLUMN-LABEL "Req'd" FORMAT "yes/no":U WIDTH 7.2
            VIEW-AS TOGGLE-BOX
      sys-ctrl.char-fld COLUMN-LABEL "Action" FORMAT "x(8)":U WIDTH 12.4
            VIEW-AS COMBO-BOX INNER-LINES 5
                      LIST-ITEMS "HOLD" 
                      DROP-DOWN-LIST 
  ENABLE
      sys-ctrl.log-fld
      sys-ctrl.char-fld
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 88 BY 10.71 ROW-HEIGHT-CHARS .81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME oe-ctrl
     brHoldTests AT ROW 14.1 COL 5
     fiOptions AT ROW 6.24 COL 3 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     tgCreateSSBol AT ROW 10.43 COL 13 WIDGET-ID 8
     ar-ctrl.last-inv AT ROW 1.24 COL 31 COLON-ALIGNED
          LABEL "Last Invoice Number" FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 7 FGCOLOR 15 
     n-ord AT ROW 2.43 COL 31 COLON-ALIGNED HELP
          "Enter order number to be used for next order"
          LABEL "Next Order Number"
          BGCOLOR 15 
     oe-ctrl.n-bol AT ROW 3.62 COL 31 COLON-ALIGNED
          LABEL "Next Bill of Lading Number" FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     oe-ctrl.p-fact AT ROW 7.43 COL 13
          LABEL "Allow Factory Ticket Printing"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     oe-ctrl.p-job AT ROW 7.43 COL 58
          LABEL "Allow Create Job"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     oe-ctrl.p-bol AT ROW 8.43 COL 13
          LABEL "Allow Bill of Lading Printing"
          VIEW-AS TOGGLE-BOX
          SIZE 35 BY .81
     oe-ctrl.p-pick AT ROW 9.43 COL 13
          LABEL "Allow Releases and Release Ticket Printing"
          VIEW-AS TOGGLE-BOX
          SIZE 45 BY .81
     oe-ctrl.p-sep AT ROW 11.48 COL 13
          LABEL "Print Separate Invoice per Release"
          VIEW-AS TOGGLE-BOX
          SIZE 43 BY .81
     Btn_Update AT ROW 25.29 COL 54 HELP
          "Update/Save System Configurations"
     Btn_Close AT ROW 25.29 COL 70 HELP
          "Cancel Update or Close Window"
     fNextRFIDNum AT ROW 4.76 COL 31 COLON-ALIGNED WIDGET-ID 6
     fiHoldTests AT ROW 12.91 COL 3 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.2 BY 25.67.


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
         HEIGHT             = 25.91
         WIDTH              = 94.8
         MAX-HEIGHT         = 25.91
         MAX-WIDTH          = 120.8
         VIRTUAL-HEIGHT     = 25.91
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
/* SETTINGS FOR FRAME oe-ctrl
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brHoldTests 1 oe-ctrl */
/* SETTINGS FOR BROWSE brHoldTests IN FRAME oe-ctrl
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Close:PRIVATE-DATA IN FRAME oe-ctrl     = 
                "ribbon-button".

ASSIGN 
       Btn_Update:PRIVATE-DATA IN FRAME oe-ctrl     = 
                "ribbon-button".

/* SETTINGS FOR FILL-IN fiHoldTests IN FRAME oe-ctrl
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiOptions IN FRAME oe-ctrl
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fNextRFIDNum IN FRAME oe-ctrl
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ar-ctrl.last-inv IN FRAME oe-ctrl
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN oe-ctrl.n-bol IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN n-ord IN FRAME oe-ctrl
   1 LIKE = asi.oe-ctrl. EXP-LABEL EXP-SIZE                             */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.p-bol IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.p-fact IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.p-job IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.p-pick IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ctrl.p-sep IN FRAME oe-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX tgCreateSSBol IN FRAME oe-ctrl
   NO-ENABLE 1                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brHoldTests
/* Query rebuild information for BROWSE brHoldTests
     _TblList          = "asi.sys-ctrl"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "sys-ctrl.company EQ gcompany AND sys-ctrl.module = ""VAL"""
     _FldNameList[1]   > asi.sys-ctrl.name
"sys-ctrl.name" ? "x(16)" "character" ? ? ? ? ? ? no ? no no "16.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.sys-ctrl.descrip
"sys-ctrl.descrip" ? ? "character" ? ? ? ? ? ? no ? no no "45.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.sys-ctrl.log-fld
"sys-ctrl.log-fld" "Req'd" ? "logical" ? ? ? ? ? ? yes ? no no "7.2" yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[4]   > asi.sys-ctrl.char-fld
"sys-ctrl.char-fld" "Action" ? "character" ? ? ? ? ? ? yes ? no no "12.4" yes no no "U" "" "" "DROP-DOWN-LIST" "," "HOLD" ? 5 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brHoldTests */
&ANALYZE-RESUME

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


&Scoped-define BROWSE-NAME brHoldTests
&Scoped-define SELF-NAME brHoldTests
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brHoldTests C-Win
ON ROW-ENTRY OF brHoldTests IN FRAME oe-ctrl
DO:
    DEF VAR cTestName AS CHAR NO-UNDO.
    DEF VAR iTestPos AS INT NO-UNDO.
    
    ASSIGN
        cTestName = sys-ctrl.name:SCREEN-VALUE IN BROWSE brHoldTests
        iTestPos = LOOKUP(cTestName,name-fld-list) 
        sys-ctrl.char-fld:LIST-ITEMS = str-init[iTestPos].
        
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
    ASSIGN
      {&SELF-NAME}:LABEL = "&Close"
      Btn_Update:LABEL = "&Update".
    RUN enable_UI.
    RUN sys/ref/asicurseq.p (INPUT gcompany, INPUT "order_seq", OUTPUT giCurrOrd) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
    n-ord:SCREEN-VALUE = STRING(giCurrOrd  + 1, ">>>>>>").
  END.
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
    ENABLE brHoldTests WITH FRAME {&frame-name}.
    RUN sys/ref/asicurseq.p (INPUT gcompany, INPUT "order_seq", OUTPUT giCurrOrd) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
    FIND CURRENT oe-ctrl EXCLUSIVE-LOCK.

    oe-ctrl.n-ord = giCurrOrd + 1.
    n-ord:SCREEN-VALUE = STRING(giCurrOrd  + 1, ">>>>>>").

    /*IF CAN-FIND(FIRST inv-head WHERE inv-head.company EQ gcompany AND
                inv-head.multi-invoice = no) THEN
      DISABLE oe-ctrl.u-inv.*/
    ASSIGN
      {&SELF-NAME}:LABEL = "&Save"
      Btn_Close:LABEL = "&Cancel".

    APPLY "ENTRY" TO n-ord.
    n-ord:SCREEN-VALUE = STRING(giCurrOrd  + 1, ">>>>>>").
    FIND CURRENT oe-ctrl NO-LOCK.
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
      /* MESSAGE "RFID Next Number uses 24 digit number."
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN NO-APPLY.*/
        fNextRFIDNum:SCREEN-VALUE = "00000000000001".
    END.
    FIND CURRENT ar-ctrl EXCLUSIVE-LOCK.
    FIND CURRENT oe-ctrl EXCLUSIVE-LOCK.
    ASSIGN {&LIST-1}.

    oe-ctrl.spare-char-1 = fNextRFIDNum.
    ASSIGN tgCreateSSBol.
    oe-ctrl.spare-int-1 = (IF tgCreateSSBol THEN 1 ELSE 0).
    oe-ctrl.u-inv = YES .  /* task 24948 */
    DISABLE {&LIST-1}.
    DISABLE tgCreateSSBol.
    DISABLE brHoldTests WITH FRAME {&frame-name}.
    ASSIGN
      {&SELF-NAME}:LABEL = "&Update"
      Btn_Close:LABEL = "&Close".
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
     FIND CURRENT ar-ctrl NO-LOCK.
  END.
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
 
  RUN enable_UI.
  DO WITH FRAME {&FRAME-NAME}:
  ASSIGN fNextRFIDNum:SCREEN-VALUE = IF oe-ctrl.spare-char-1 = "" THEN "111110000000000000000000"
                                     ELSE oe-ctrl.spare-char-1
         tgCreateSSBol:SCREEN-VALUE = (IF oe-ctrl.spare-int-1 EQ 1 THEN "YES" ELSE "NO").
  RUN sys/ref/asicurseq.p (INPUT gcompany, INPUT "order_seq", OUTPUT giCurrOrd) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

  n-ord:SCREEN-VALUE = STRING(giCurrOrd  + 1, ">>>>>>").
  n-ord:SENSITIVE = NO.
  END.
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
  DISPLAY fiOptions tgCreateSSBol n-ord fNextRFIDNum fiHoldTests 
      WITH FRAME oe-ctrl IN WINDOW C-Win.
  IF AVAILABLE ar-ctrl THEN 
    DISPLAY ar-ctrl.last-inv 
      WITH FRAME oe-ctrl IN WINDOW C-Win.
  IF AVAILABLE oe-ctrl THEN 
    DISPLAY oe-ctrl.n-bol oe-ctrl.p-fact oe-ctrl.p-job oe-ctrl.p-bol 
          oe-ctrl.p-pick oe-ctrl.p-sep 
      WITH FRAME oe-ctrl IN WINDOW C-Win.
  ENABLE n-ord Btn_Update Btn_Close 
      WITH FRAME oe-ctrl IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-oe-ctrl}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

