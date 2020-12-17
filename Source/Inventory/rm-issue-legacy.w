&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: inventory/rm-issue-legacy.w

  Description: Sharp Shooter Raw Material Issue

  Input Parameters:

  Output Parameters:
      <none>

  History: 
          
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
&SCOPED-DEFINE DEBUG TRUE

/* Parameters Definitions ---                                           */
DEFINE VARIABLE cCompany  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hdInventoryProcs        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdJobProcs              AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdJobDetails            AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdJobDetailsWin         AS HANDLE    NO-UNDO.
DEFINE VARIABLE lCreated                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobno2ListItems        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormnoListItems        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBlanknoListItems       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRMListItems            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormattedJobno         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSwitchJob              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lMoveToOnhand           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iTotTags                AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotOnHand              AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCount                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE cValidateJobno          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilterBy               AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAutoPost               AS LOGICAL   NO-UNDO.

{system/sysconst.i}
{Inventory/ttInventory.i "NEW SHARED"}
{methods/defines/sortByDefs.i}
{wip/keyboardDefs.i}
{custom/globdefs.i}
{sys/inc/var.i "NEW SHARED"}
{sys/inc/varasgn.i}
{methods/template/brwcustomdef.i}
ASSIGN
    cCompany  = cocode
    cLocation = locode
    cFilterBy = gcStatusStockReceived
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttBrowseInventory

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table ttBrowseInventory.quantity ttBrowseInventory.quantityOriginal fGetConcatLocationID() @ ttBrowseInventory.warehouseID ttBrowseInventory.tag fGetConcatJobID() @ ttBrowseInventory.jobID ttBrowseInventory.inventoryStatus   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH ttBrowseInventory     WHERE ttBrowseInventory.inventoryStatus EQ cFilterBy     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory     WHERE ttBrowseInventory.inventoryStatus EQ cFilterBy     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br-table ttBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ttBrowseInventory


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btExit btKeyboard-1 RECT-27 RECT-28 ~
rSelected btFirst btChange fiJobNo btLast cbJobNo2 btPost btNext ~
btJobLookup cbFormNo cbBlankNo cbRMItem btTotal btScanned btPrevious ~
btConsumed btnNumPad br-table 
&Scoped-Define DISPLAYED-OBJECTS fiJobNo cbJobNo2 cbFormNo cbBlankNo ~
cbRMItem fiUOM fiTag fiMessage fiTotalQty fiScannedQty fiConsumedQty 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetConcatJobID W-Win 
FUNCTION fGetConcatJobID RETURNS CHARACTER PRIVATE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetConcatLocationID W-Win 
FUNCTION fGetConcatLocationID RETURNS CHARACTER PRIVATE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btChange 
     LABEL "Change" 
     SIZE 24 BY 2.62
     FONT 37.

DEFINE BUTTON btConsumed 
     LABEL "Consumed: 0" 
     SIZE 27 BY 2 TOOLTIP "Filter Consumed Tags"
     FONT 36.

DEFINE BUTTON btExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS
     LABEL "" 
     SIZE 11 BY 2.62.

DEFINE BUTTON btFirst 
     IMAGE-UP FILE "Graphics/32x32/navigate_up2.ico":U NO-FOCUS
     LABEL "First" 
     SIZE 9.6 BY 2.29 TOOLTIP "First".

DEFINE BUTTON btJobDetails 
     IMAGE-UP FILE "Graphics/32x32/form.ico":U
     LABEL "" 
     SIZE 11 BY 2.62 TOOLTIP "View Current Job Details".

DEFINE BUTTON btJobLookup 
     IMAGE-UP FILE "Graphics/32x32/binocular2.ico":U
     LABEL "Job Lookup" 
     SIZE 8 BY 1.52 TOOLTIP "Job Lookup".

DEFINE BUTTON btKeyboard-1 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btKeyboard-2 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btLast 
     IMAGE-UP FILE "Graphics/32x32/navigate_down2.ico":U NO-FOCUS
     LABEL "Last" 
     SIZE 9.6 BY 2.29 TOOLTIP "Last".

DEFINE BUTTON btNext 
     IMAGE-UP FILE "Graphics/32x32/navigate_down.ico":U NO-FOCUS
     LABEL "Next" 
     SIZE 9.6 BY 2.29 TOOLTIP "Next".

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btPost 
     LABEL "Post" 
     SIZE 40 BY 2.62
     FONT 37.

DEFINE BUTTON btPrevious 
     IMAGE-UP FILE "Graphics/32x32/navigate_up.ico":U NO-FOCUS
     LABEL "Previous" 
     SIZE 9.6 BY 2.29 TOOLTIP "Previous".

DEFINE BUTTON btScanned 
     LABEL "Scanned: 0" 
     SIZE 27 BY 2 TOOLTIP "Filter Scanned Tags"
     FONT 36.

DEFINE BUTTON btTotal 
     LABEL "On Hand: 0" 
     SIZE 27 BY 2 TOOLTIP "Filter All Tags"
     FONT 36.

DEFINE VARIABLE cbBlankNo AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 10.2 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cbFormNo AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 10 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cbJobNo2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00" 
     DROP-DOWN-LIST
     SIZE 10.2 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cbRMItem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 38 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiConsumedQty AS CHARACTER FORMAT "X(256)":U INITIAL "Qty: 0" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     FONT 19 NO-UNDO.

DEFINE VARIABLE fiJobNo AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiMessage AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 101.4 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiScannedQty AS CHARACTER FORMAT "X(256)":U INITIAL "Qty: 0" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     FONT 19 NO-UNDO.

DEFINE VARIABLE fiTag AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 80 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE fiTotalQty AS CHARACTER FORMAT "X(256)":U INITIAL "Qty: 0" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     FONT 19.

DEFINE VARIABLE fiUOM AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.29
     FONT 36 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 202 BY 32.86.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 199 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 199 BY .1.

DEFINE RECTANGLE rSelected
     EDGE-PIXELS 0    
     SIZE 29 BY 2.38
     BGCOLOR 1 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      ttBrowseInventory SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table W-Win _FREEFORM
  QUERY br-table DISPLAY
      ttBrowseInventory.quantity WIDTH 25 COLUMN-LABEL "Qty On-hand"
      fGetConcatLocationID() @ ttBrowseInventory.warehouseID WIDTH 30 COLUMN-LABEL "Location" FORMAT "X(12)"
      ttBrowseInventory.tag WIDTH 70 COLUMN-LABEL "Tag #" FORMAT "X(30)"
      fGetConcatJobID() @ ttBrowseInventory.jobID WIDTH 25 COLUMN-LABEL "Job #" FORMAT "X(20)"
      ttBrowseInventory.inventoryStatus COLUMN-LABEL "Status" FORMAT "X(15)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 189 BY 21.91
         FONT 36 ROW-HEIGHT-CHARS 1.52 FIT-LAST-COLUMN TOOLTIP "Double click a tag to update status".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btExit AT ROW 1.95 COL 189.6 WIDGET-ID 84
     btKeyboard-1 AT ROW 1.91 COL 75 WIDGET-ID 136
     btKeyboard-2 AT ROW 8.1 COL 95.8 WIDGET-ID 142
     btFirst AT ROW 11.67 COL 192 WIDGET-ID 128
     btChange AT ROW 1.95 COL 2 WIDGET-ID 8
     fiJobNo AT ROW 1.95 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     btLast AT ROW 31.19 COL 192 WIDGET-ID 130
     cbJobNo2 AT ROW 1.95 COL 80.8 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     btPost AT ROW 1.95 COL 126.8 WIDGET-ID 38
     btJobDetails AT ROW 1.95 COL 172.6 WIDGET-ID 160
     btNext AT ROW 26.81 COL 192.2 WIDGET-ID 132
     btJobLookup AT ROW 3.43 COL 29.8 WIDGET-ID 156
     cbFormNo AT ROW 3.52 COL 54.6 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     cbBlankNo AT ROW 3.52 COL 80.8 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     cbRMItem AT ROW 5.29 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 152
     fiUOM AT ROW 5.29 COL 94 COLON-ALIGNED NO-LABEL WIDGET-ID 176
     btTotal AT ROW 8 COL 112.8 WIDGET-ID 162 NO-TAB-STOP 
     btScanned AT ROW 8 COL 141.4 WIDGET-ID 164
     btPrevious AT ROW 15.71 COL 192.2 WIDGET-ID 134
     btConsumed AT ROW 8 COL 170 WIDGET-ID 166
     fiTag AT ROW 8.19 COL 13.4 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     fiMessage AT ROW 9.95 COL 4.6 COLON-ALIGNED NO-LABEL WIDGET-ID 158
     fiTotalQty AT ROW 10.29 COL 110.8 COLON-ALIGNED NO-LABEL WIDGET-ID 170
     btnNumPad AT ROW 2.67 COL 98 WIDGET-ID 138
     fiScannedQty AT ROW 10.29 COL 139.4 COLON-ALIGNED NO-LABEL WIDGET-ID 174
     fiConsumedQty AT ROW 10.29 COL 168 COLON-ALIGNED NO-LABEL WIDGET-ID 172
     br-table AT ROW 11.71 COL 2 WIDGET-ID 200
     "UOM:" VIEW-AS TEXT
          SIZE 9 BY .95 AT ROW 5.48 COL 86.6 WIDGET-ID 178
          FONT 36
     "Blank #:" VIEW-AS TEXT
          SIZE 14 BY .95 AT ROW 3.71 COL 68 WIDGET-ID 58
          FONT 36
     "Form #:" VIEW-AS TEXT
          SIZE 14.6 BY .95 AT ROW 3.71 COL 42 WIDGET-ID 48
          FONT 36
     "RM Item:" VIEW-AS TEXT
          SIZE 14 BY .95 AT ROW 5.43 COL 28 WIDGET-ID 154
          FONT 36
     "Job #:" VIEW-AS TEXT
          SIZE 11 BY .95 AT ROW 2.14 COL 30 WIDGET-ID 12
          FONT 36
     "Tag:" VIEW-AS TEXT
          SIZE 8.2 BY 1.19 AT ROW 8.29 COL 6.8 WIDGET-ID 22
          FONT 36
     RECT-2 AT ROW 2.43 COL 97 WIDGET-ID 140
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 126
     RECT-27 AT ROW 6.95 COL 2.2 WIDGET-ID 18
     RECT-28 AT ROW 11.33 COL 2.2 WIDGET-ID 146
     rSelected AT ROW 7.81 COL 111.8 WIDGET-ID 168
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202 BY 32.86
         BGCOLOR 15  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Issue RM"
         HEIGHT             = 32.86
         WIDTH              = 202
         MAX-HEIGHT         = 36.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 36.57
         VIRTUAL-WIDTH      = 273.2
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB br-table fiConsumedQty F-Main */
ASSIGN 
       br-table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR BUTTON btJobDetails IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btJobLookup:AUTO-RESIZE IN FRAME F-Main      = TRUE
       btJobLookup:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btKeyboard-1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btKeyboard-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiConsumedQty IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiMessage IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiScannedQty IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTag IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTotalQty IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiUOM IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory
    WHERE ttBrowseInventory.inventoryStatus EQ cFilterBy
    ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Issue RM */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Issue RM */
DO:
    IF VALID-HANDLE(hdInventoryProcs) THEN
        DELETE OBJECT hdInventoryProcs.
        
    IF VALID-HANDLE(hdJobProcs) THEN
        DELETE OBJECT hdJobProcs.
        
    /* This ADM code must be left here in order for the SmartWindow
       and its descendents to terminate properly on exit. */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-table
&Scoped-define SELF-NAME br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table W-Win
ON DEFAULT-ACTION OF br-table IN FRAME F-Main
DO:
    IF AVAILABLE ttBrowseInventory THEN DO:
        fiTag:SCREEN-VALUE = ttBrowseInventory.tag.
        
        APPLY "LEAVE" TO fiTag.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table W-Win
ON START-SEARCH OF br-table IN FRAME F-Main
DO:
	{methods/template/sortindicator.i} 
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        IF VALID-HANDLE(hSaveLabel) THEN
        hSaveLabel:LABEL-BGCOLOR = ?.
        ASSIGN
            hColumnLabel = {&BROWSE-NAME}:CURRENT-COLUMN
            hColumnLabel:LABEL-BGCOLOR = 14
            hSaveLabel = hColumnLabel
            cSaveLabel = cColumnLabel
            .
        RUN pReopenBrowse.
    END.
	{methods/template/sortindicatorend.i} 
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btChange
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btChange W-Win
ON CHOOSE OF btChange IN FRAME F-Main /* Change */
DO:
    RUN pEnableJobEntry.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btConsumed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConsumed W-Win
ON CHOOSE OF btConsumed IN FRAME F-Main /* Consumed: 0 */
DO:
    RUN pHighlightSelection (
        INPUT gcStatusStockConsumed
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit W-Win
ON CHOOSE OF btExit IN FRAME F-Main
DO:
    IF VALID-HANDLE(hdJobDetailsWin) THEN
        APPLY "WINDOW-CLOSE" TO hdJobDetailsWin.

    IF VALID-HANDLE(hdJobDetails) THEN
        DELETE OBJECT hdInventoryProcs.
    
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    
    RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFirst W-Win
ON CHOOSE OF btFirst IN FRAME F-Main /* First */
DO:
    RUN pNavigate (
        SELF
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btJobDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btJobDetails W-Win
ON CHOOSE OF btJobDetails IN FRAME F-Main
DO:
    DO WITH FRAME {&FRAME-NAME}:
    END.

    IF NOT VALID-HANDLE(hdJobDetails) THEN DO:         
        RUN inventory/job-details.w PERSISTENT SET hdJobDetails.

        RUN dispatch IN hdJobDetails (
            INPUT 'initialize':U
            ) NO-ERROR.
        
        hdJobDetailsWin = hdJobDetails:CURRENT-WINDOW.
    END.
                                                 
    IF VALID-HANDLE(hdJobDetails) AND
        VALID-HANDLE(hdJobDetailsWin) THEN DO:        
        RUN pInit IN hdJobDetails (
            INPUT cCompany,
            INPUT cLocation,
            INPUT fijobno:SCREEN-VALUE,
            INPUT INTEGER(cbJobno2:SCREEN-VALUE),
            INPUT INTEGER(cbFormno:SCREEN-VALUE),
            INPUT INTEGER(cbBlankno:SCREEN-VALUE)
            ) NO-ERROR.            

        IF hdJobDetailsWin:WINDOW-STATE EQ 2 THEN ASSIGN 
            hdJobDetailsWin:WINDOW-STATE = 3.
        
        hdJobDetailsWin:MOVE-TO-TOP().
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btJobLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btJobLookup W-Win
ON CHOOSE OF btJobLookup IN FRAME F-Main /* Job Lookup */
DO:
    APPLY "HELP" TO fiJobno.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btKeyboard-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btKeyboard-1 W-Win
ON CHOOSE OF btKeyboard-1 IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiJobNo.
    RUN pKeyboard (
        fiJobNo:HANDLE, 
        "Qwerty"
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btKeyboard-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btKeyboard-2 W-Win
ON CHOOSE OF btKeyboard-2 IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO fiTag.
    RUN pKeyboard (
        fiTag:HANDLE, 
        "Qwerty"
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLast W-Win
ON CHOOSE OF btLast IN FRAME F-Main /* Last */
DO:
    RUN pNavigate (
        SELF
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNext W-Win
ON CHOOSE OF btNext IN FRAME F-Main /* Next */
DO:
    RUN pNavigate (
        SELF
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNumPad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNumPad W-Win
ON CHOOSE OF btnNumPad IN FRAME F-Main /* NumPad */
DO:
    ASSIGN
        lKeyboard = NOT lKeyboard
        RECT-2:BGCOLOR = IF lKeyboard THEN 10 ELSE 12
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPost W-Win
ON CHOOSE OF btPost IN FRAME F-Main /* Post */
DO:
    DEFINE VARIABLE lSuccess  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.
    
    FIND FIRST ttBrowseInventory 
         WHERE ttBrowseInventory.inventoryStatus EQ gcStatusStockScanned
         NO-ERROR.
    IF NOT AVAILABLE ttBrowseInventory THEN DO:
        MESSAGE "No records available to post"
            VIEW-AS ALERT-BOX ERROR.            
        RETURN NO-APPLY.
    END.
    
    RUN Inventory_PostRawMaterials IN hdInventoryProcs (
        INPUT  cocode,
        INPUT  TODAY,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).
    IF lSuccess THEN
        MESSAGE "Posting completed"
            VIEW-AS ALERT-BOX INFORMATION.
    ELSE
       MESSAGE cMessage
           VIEW-AS ALERT-BOX ERROR.
    
    RUN pRebuildBrowse (
        INPUT cCompany,
        INPUT cFormattedJobNo,
        INPUT INTEGER(cbJobNo2:SCREEN-VALUE),
        INPUT INTEGER(cbFormNo:SCREEN-VALUE),
        INPUT INTEGER(cbBlankNo:SCREEN-VALUE),
        INPUT cbRMItem:SCREEN-VALUE,
        INPUT FALSE
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrevious W-Win
ON CHOOSE OF btPrevious IN FRAME F-Main /* Previous */
DO:
    RUN pNavigate (
        SELF
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btScanned
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btScanned W-Win
ON CHOOSE OF btScanned IN FRAME F-Main /* Scanned: 0 */
DO:
    RUN pHighlightSelection (
        INPUT gcStatusStockScanned
        ).        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btTotal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btTotal W-Win
ON CHOOSE OF btTotal IN FRAME F-Main /* On Hand: 0 */
DO:
    RUN pHighlightSelection (
        INPUT gcStatusStockReceived
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbBlankNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbBlankNo W-Win
ON VALUE-CHANGED OF cbBlankNo IN FRAME F-Main
DO:
    RUN pOnValueChangedOfJobDetails.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbFormNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbFormNo W-Win
ON VALUE-CHANGED OF cbFormNo IN FRAME F-Main
DO:
    RUN pOnValueChangedOfJobDetails.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbJobNo2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbJobNo2 W-Win
ON VALUE-CHANGED OF cbJobNo2 IN FRAME F-Main
DO:
    RUN pOnValueChangedOfJobDetails.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbRMItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbRMItem W-Win
ON VALUE-CHANGED OF cbRMItem IN FRAME F-Main
DO:
    IF SELF:SCREEN-VALUE NE ? AND SELF:SCREEN-VALUE NE "" THEN
        RUN pDisableJobEntry.

    RUN pUpdateMessageText (
        INPUT "",     /* Message Text */
        INPUT FALSE,  /* Error */
        INPUT FALSE   /* Alert-box*/
        ).

    RUN pRebuildBrowse (
        INPUT cCompany,
        INPUT cFormattedJobNo,
        INPUT INTEGER(cbJobNo2:SCREEN-VALUE),
        INPUT INTEGER(cbFormNo:SCREEN-VALUE),
        INPUT INTEGER(cbBlankNo:SCREEN-VALUE),
        INPUT cbRMItem:SCREEN-VALUE,
        INPUT TRUE
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiJobNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiJobNo W-Win
ON ENTRY OF fiJobNo IN FRAME F-Main
DO:
    hFocusField = SELF.
    cValidateJobno = SELF:SCREEN-VALUE.
    IF lKeyboard THEN
        RUN pKeyboard (
            SELF, 
            "Qwerty"
            ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiJobNo W-Win
ON HELP OF fiJobNo IN FRAME F-Main
DO:
    DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.

    RUN system/openlookup.p (
        INPUT  cCompany, 
        INPUT  "job-no",  /* Job No lookup ID */
        INPUT  0,
        INPUT  "",
        INPUT  0,
        OUTPUT cFieldsValue, 
        OUTPUT cFoundValue, 
        OUTPUT recFoundRecID
        ).
    APPLY "LEAVE":U TO SELF.
    
    IF cFoundValue NE "" THEN DO:
        SELF:SCREEN-VALUE      = cFoundValue.
        APPLY "LEAVE" TO SELF.
    END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiJobNo W-Win
ON LEAVE OF fiJobNo IN FRAME F-Main
DO:
    DEFINE VARIABLE cJobNo     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo2    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormNo    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBlankNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lParse     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidJob  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iJobFormat AS INTEGER   NO-UNDO INITIAL 6.
        
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.
    
    IF cValidateJobno EQ SELF:SCREEN-VALUE THEN
        RETURN.

    ASSIGN 
        cJobno2ListItems       = ""
        cFormnoListItems       = ""
        cBlanknoListitems      = ""
        cMessage               = ""
        btJobDetails:SENSITIVE = FALSE
        .            

    RUN pUpdateMessageText (
        INPUT cMessage,    /* Message Text */
        INPUT FALSE,       /* Error */
        INPUT FALSE        /* Alert-box*/
        ).

    RUN JobParser IN hdJobProcs (
        INPUT  SELF:SCREEN-VALUE,
        OUTPUT cJobNo,
        OUTPUT cJobNo2,
        OUTPUT cFormNo,
        OUTPUT cBlankNo,
        OUTPUT lParse,
        OUTPUT cMessage
        ).

    IF cMessage NE "" THEN DO:
        RUN pUpdateMessageText (
            INPUT cMessage,    /* Message Text */
            INPUT TRUE,        /* Error */
            INPUT TRUE         /* Alert-box*/
            ).
        RETURN.
    END.
    
    cFormattedJobno = DYNAMIC-FUNCTION (
                      "fAddSpacesToString" IN hdJobProcs, SELF:SCREEN-VALUE, 6, TRUE
                      ).                                  

    IF lParse THEN
        ASSIGN
            fiJobNo:SCREEN-VALUE = cJobNo   
            cFormattedJobno      = DYNAMIC-FUNCTION (
                                       "fAddSpacesToString" IN hdJobProcs, cJobNo, 6, TRUE
                                   )
            .
            
    RUN GetSecondaryJobForJob IN hdJobProcs (
        INPUT cCompany,
        INPUT cFormattedJobno,
        INPUT-OUTPUT cJobno2ListItems
        ).
    
    DO iCount = 1 TO NUM-ENTRIES(cJobno2ListItems):
        RUN GetFormnoForJob IN hdJobProcs (
            INPUT cCompany,
            INPUT cFormattedJobno,
            INPUT INTEGER(ENTRY(iCount, cJobno2ListItems)),
            INPUT-OUTPUT cFormnoListItems
            ).
    
        RUN GetBlanknoForJob IN hdJobProcs (
            INPUT cCompany,
            INPUT cFormattedJobno,
            INPUT INTEGER(ENTRY(iCount, cJobno2ListItems)),
            INPUT-OUTPUT cBlanknoListItems
            ).
    END.

    IF cJobno2ListItems EQ "" THEN
        ASSIGN 
            cJobno2ListItems      = "00"
            cbJobno2:LIST-ITEMS   = cJobno2ListItems 
            cbJobno2:SCREEN-VALUE = "00"
            .
    ELSE
        cbJobNo2:LIST-ITEMS = cJobno2ListItems.
 
    IF cFormnoListItems EQ "" THEN
        ASSIGN
            cFormnoListItems      = "00"
            cbFormno:LIST-ITEMS   = cFormnoListItems 
            cbFormno:SCREEN-VALUE = "00"
            .
    ELSE
        cbFormNo:LIST-ITEMS = cFormnoListItems.

    IF cBlanknoListItems EQ "" THEN
        ASSIGN
            cBlanknoListItems       = "00"
            cbBlankNo:LIST-ITEMS   = cBlanknoListItems
            cbBlankNo:SCREEN-VALUE = "00"
            .
    ELSE
        cbBlankNo:LIST-ITEMS = cBlanknoListItems.

    IF lParse THEN
        IF (cJobNo2 NE "" AND INDEX(cJobno2ListItems,STRING(INTEGER(cJobNo2),"99")) LE 0) OR
           (cFormNo NE "" AND INDEX(cFormnoListItems,STRING(INTEGER(cFormNo),"99")) LE 0) OR
           (cBlankNo NE "" AND INDEX(cBlanknoListitems,STRING(INTEGER(cBlankNo),"99")) LE 0) THEN DO:
            MESSAGE "Invalid Job Scan, please scan a valid Job Number." 
                VIEW-AS ALERT-BOX ERROR.
            
            ASSIGN
                cFormattedJobNo        = ""
                cValidateJobno         = ""
                SELF:SCREEN-VALUE      = ""
                cbjobno2:LIST-ITEMS    = "00"
                cbformno:LIST-ITEMS    = "00"
                cbblankno:LIST-ITEMS   = "00"
                cbjobno2:SCREEN-VALUE  = "00"
                cbformno:SCREEN-VALUE  = "00"
                cbblankno:SCREEN-VALUE = "00"
                .           
            
            SESSION:SET-WAIT-STATE("").
            
            RETURN NO-APPLY.
        END.
        ELSE
            ASSIGN
                cbjobno2:SCREEN-VALUE  = IF cJobNo2 EQ "" THEN 
                                             ENTRY(1,cJobno2ListItems)
                                         ELSE
                                             STRING(INTEGER(cJobNo2),"99")
                cbformno:SCREEN-VALUE  = IF cFormNo EQ "" THEN
                                             ENTRY(1,cFormnoListItems)
                                         ELSE
                                             STRING(INTEGER(cFormNo),"99")
                cbblankno:SCREEN-VALUE = IF cBlankNo EQ "" THEN
                                             ENTRY(1,cBlanknoListItems)
                                         ELSE
                                             STRING(INTEGER(cBlankNo),"99")
                .
    ELSE
        ASSIGN 
            cbjobno2:SCREEN-VALUE  = ENTRY(1,cJobno2ListItems)
            cbformno:SCREEN-VALUE  = ENTRY(1,cFormnoListItems)
            cbblankno:SCREEN-VALUE = ENTRY(1,cBlanknoListItems)
            .

    RUN ValidateJob IN hdJobProcs (
        INPUT  cCompany,
        INPUT  cFormattedJobno,
        INPUT  "", /* Blank Machine Code */
        INPUT  INTEGER(cbJobNo2:SCREEN-VALUE),
        INPUT  INTEGER(cbFormNo:SCREEN-VALUE),
        INPUT  INTEGER(cbBlankNo:SCREEN-VALUE),
        OUTPUT lValidJob
        ).

    IF NOT lValidJob THEN
        ASSIGN 
            cbJobNo2:SCREEN-VALUE  = ENTRY(1,cJobno2ListItems)
            cbFormNo:SCREEN-VALUE  = ENTRY(1,cFormnoListItems)
            cbBlankNo:SCREEN-VALUE = ENTRY(1,cBlanknoListItems)
            .    

    RUN pUpdateRMItemList (
        INPUT cCompany,
        INPUT cFormattedJobno,
        INPUT INTEGER(cbJobNo2:SCREEN-VALUE),
        INPUT INTEGER(cbFormNo:SCREEN-VALUE),
        INPUT INTEGER(cbBlankNo:SCREEN-VALUE)
        ).
    
    RUN ValidateJobHdr IN hdJobProcs (
        INPUT  cCompany,
        INPUT  cFormattedJobno,
        INPUT  INTEGER(cbJobNo2:SCREEN-VALUE),
        OUTPUT lValidJob
        ) NO-ERROR.
    
    btJobDetails:SENSITIVE = lValidJob.

    RUN ValidateJob IN hdJobProcs (
        INPUT  cCompany,
        INPUT  cFormattedJobno,
        INPUT  "", /* Blank Machine Code */
        INPUT  INTEGER(cbJobNo2:SCREEN-VALUE),
        INPUT  INTEGER(cbFormNo:SCREEN-VALUE),
        INPUT  INTEGER(cbBlankNo:SCREEN-VALUE),
        OUTPUT lValidJob
        ).
    
    IF NOT lValidJob THEN DO:
        ASSIGN
            cMessage          = "Invalid Job entry '" + SELF:SCREEN-VALUE + "'"
            SELF:SCREEN-VALUE = ""
            cValidateJobno    = ""
            .
        RUN pUpdateMessageText (
            INPUT cMessage,    /* Message Text */
            INPUT TRUE,        /* Error */
            INPUT TRUE         /* Alert-box*/
            ).
        RETURN NO-APPLY.    
    END.
           
    APPLY "VALUE-CHANGED" TO cbRMItem.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON ENTRY OF fiTag IN FRAME F-Main
DO:    
    hFocusField = SELF.
    IF lKeyboard THEN
        RUN pKeyboard (
            SELF, 
            "Qwerty"
            ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTag W-Win
ON LEAVE OF fiTag IN FRAME F-Main
DO:
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.

    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
        
    RUN pTagScan (
        INPUT cCompany,
        INPUT SELF:SCREEN-VALUE
        ).
    
    SELF:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

{wip/pNavigate.i}
{wip/pKeyboard.i}
{methods/sortByProc.i "pByQuantity" "ttBrowseInventory.quantity"}
{methods/sortByProc.i "pByQuantityOriginal" "ttBrowseInventory.quantityOriginal"}
{methods/sortByProc.i "pByLocationID" "ttBrowseInventory.locationID"}
{methods/sortByProc.i "pByTag" "ttBrowseInventory.tag"}
{methods/sortByProc.i "pByJobID" "ttBrowseInventory.jobID"}
{methods/sortByProc.i "pByInventoryStatus" "ttBrowseInventory.inventoryStatus"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY fiJobNo cbJobNo2 cbFormNo cbBlankNo cbRMItem fiUOM fiTag fiMessage 
          fiTotalQty fiScannedQty fiConsumedQty 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE btExit btKeyboard-1 RECT-27 RECT-28 rSelected btFirst btChange fiJobNo 
         btLast cbJobNo2 btPost btNext btJobLookup cbFormNo cbBlankNo cbRMItem 
         btTotal btScanned btPrevious btConsumed btnNumPad br-table 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisableJobEntry W-Win 
PROCEDURE pDisableJobEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiJobNo:SENSITIVE       = FALSE
        cbJobNo2:SENSITIVE      = FALSE
        cbFormNo:SENSITIVE      = FALSE
        cbBlankNo:SENSITIVE     = FALSE
        cbRMItem:SENSITIVE      = FALSE
        btKeyboard-1:SENSITIVE  = FALSE
        btJobLookup:SENSITIVE   = FALSE        
        fiTag:SENSITIVE         = TRUE
        btKeyboard-2:SENSITIVE  = TRUE
        .        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEnableJobEntry W-Win 
PROCEDURE pEnableJobEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiJobNo:SENSITIVE       = TRUE
        cbJobNo2:SENSITIVE      = TRUE
        cbFormNo:SENSITIVE      = TRUE
        cbBlankNo:SENSITIVE     = TRUE
        cbRMItem:SENSITIVE      = TRUE
        btKeyboard-1:SENSITIVE  = TRUE
        btJobLookup:SENSITIVE   = TRUE
        fiTag:SENSITIVE         = FALSE
        btKeyboard-2:SENSITIVE  = FALSE
        .        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHighlightSelection W-Win 
PROCEDURE pHighlightSelection :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFilterType AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    cFilterBy = ipcFilterType.
    
    CASE ipcFilterType:
        WHEN gcStatusStockReceived THEN
            rSelected:COL = btTotal:COL - 1.
        WHEN gcStatusStockConsumed THEN
            rSelected:COL = btConsumed:COL - 1.
        WHEN gcStatusStockScanned THEN
            rSelected:COL = btScanned:COL - 1.    
    END.
    
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit W-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.
    
    FIND FIRST company NO-LOCK 
         WHERE company.company EQ cCompany NO-ERROR .
    IF AVAILABLE company THEN
        {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - {&awversion}" + " - " 
                             + STRING(company.name) + " - " + cLocation  .
    
    IF lAutoPost THEN
        btPost:HIDDEN = TRUE.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJobScan W-Win 
PROCEDURE pJobScan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER  ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipcJobno    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipiJobno2   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER  ipiFormno   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER  ipiBlankno  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER  ipcRMItem   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER  oplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER  opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lValidJob AS LOGICAL NO-UNDO.
    
    ASSIGN 
        cJobno2ListItems  = STRING(ipiJobno2,"99")
        cFormnoListItems  = STRING(ipiFormno,"99")
        cBlanknoListitems = STRING(ipiBlankno,"99")
        .            
    DO WITH FRAME {&FRAME-NAME}:
    END.
           
    ASSIGN 
        cbJobNo2:LIST-ITEMS    = cJobno2ListItems
        cbFormNo:LIST-ITEMS    = cFormnoListItems
        cbBlankNo:LIST-ITEMS   = cBlanknoListItems
        cbJobNo2:SCREEN-VALUE  = STRING(ipiJobno2,"99")
        cbFormNo:SCREEN-VALUE  = STRING(ipiFormno,"99")
        cbBlankNo:SCREEN-VALUE = STRING(ipiBlankno,"99")
        fiJobNo:SCREEN-VALUE   = ipcJobno
        cFormattedJobNo        = ipcJobno
        .
                          
    RUN ValidateJob IN hdJobProcs (
        INPUT ipcCompany,
        INPUT cFormattedJobno,
        INPUT "", /* Blank Machine code */
        INPUT INTEGER(cbJobNo2:SCREEN-VALUE),
        INPUT INTEGER(cbFormNo:SCREEN-VALUE),
        INPUT INTEGER(cbBlankNo:SCREEN-VALUE),
        OUTPUT lValidJob
        ).
                      
    IF NOT lValidJob THEN DO: 
        ASSIGN
            opcMessage = "Invalid Job Entry"
            oplSuccess = FALSE
            .
            
        RETURN.
    END.  

    RUN pUpdateRMItemList (
        INPUT ipcCompany,
        INPUT ipcJobno,
        INPUT INTEGER(cbJobNo2:SCREEN-VALUE),
        INPUT INTEGER(cbFormNo:SCREEN-VALUE),
        INPUT INTEGER(cbBlankNo:SCREEN-VALUE)
        ).
        
    IF cbRMItem:LOOKUP (ipcRMItem) EQ ? OR
       cbRMItem:LOOKUP (ipcRMItem) EQ 0 THEN DO:
        ASSIGN
            opcMessage = "Invalid Tag RM Item for the existing job"
            oplSuccess = FALSE.

        RUN pEnableJobEntry.
        
        RETURN.
    END. 
    ELSE DO:
        cbRMItem:SCREEN-VALUE = ipcRMItem.  
        APPLY "VALUE-CHANGED" TO cbRMItem.
    END.
        
    ASSIGN
        opcMessage = "Success"
        oplSuccess = TRUE
        .                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOnValueChangedOfJobDetails W-Win 
PROCEDURE pOnValueChangedOfJobDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lValidJob AS LOGICAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    RUN pUpdateMessageText (
        INPUT "",     /* Message Text */
        INPUT FALSE,  /* Error */
        INPUT FALSE   /* Alert-box*/
        ).

    RUN ValidateJob IN hdJobProcs (
        INPUT  cCompany,
        INPUT  cFormattedJobno,
        INPUT  "", /* Blank Machine Code */
        INPUT  INTEGER(cbJobNo2:SCREEN-VALUE),
        INPUT  INTEGER(cbFormNo:SCREEN-VALUE),
        INPUT  INTEGER(cbBlankNo:SCREEN-VALUE),
        OUTPUT lValidJob
        ).

    IF NOT lValidJob THEN DO: 
        cMessage = "Invalid Job Entry".
        RUN pUpdateMessageText (
            INPUT cMessage, /* Message Text */
            INPUT TRUE,     /* Error */
            INPUT TRUE      /* Alert-box*/
            ).        
    END.
    
    RUN pUpdateRMItemList (
        INPUT cCompany,
        INPUT cFormattedJobno,
        INPUT INTEGER(cbJobNo2:SCREEN-VALUE),
        INPUT INTEGER(cbFormNo:SCREEN-VALUE),
        INPUT INTEGER(cbBlankNo:SCREEN-VALUE)
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRebuildBrowse W-Win 
PROCEDURE pRebuildBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormno     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankno    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcRMItem     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplRebuild    AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE iScannedTags  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iConsumedTags AS INTEGER NO-UNDO.
    DEFINE VARIABLE iOnHandTags   AS INTEGER NO-UNDO.
    DEFINE VARIABLE dScannedQty   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dConsumedQty  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOnHandQty    AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE cItemName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRMItem   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cConsUOM  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError    AS LOGICAL   NO-UNDO.
    
    
    cRMItem = ipcRMItem.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF ipcRMItem EQ ? THEN
        ipcRMItem = "".

    IF iplRebuild THEN DO:        
        RUN Inventory_BuildRMBinForItem IN hdInventoryProcs (
            INPUT        ipcCompany,
            INPUT        "",
            INPUT        "",        
            INPUT-OUTPUT cRMItem,
            INPUT-OUTPUT cItemName,
            INPUT        ipcJobNo,
            INPUT        ipiJobNo2,
            INPUT        FALSE,  /* Include Zero qty bins */
            INPUT        TRUE,   /* Include empty tag bins */
            OUTPUT       cConsUOM,
            OUTPUT       lError,
            OUTPUT       cMessage
            ).

        RUN Inventory_BuildRMTransactions IN hdInventoryProcs (
            INPUT  ipcCompany,
            INPUT  ipcJobno,
            INPUT  "", /* Blank Machine Code */
            INPUT  ipiJobno2,
            INPUT  ipiFormno,
            INPUT  ipiBlankno,
            INPUT  ipcRMItem,
            INPUT  "I",  /* Issue Transactions */
            INPUT  FALSE /* Empty existing temp-table records */
            ).

        RUN Inventory_BuildRMHistory IN hdInventoryProcs (
            INPUT ipcCompany,
            INPUT ipcRMItem,
            INPUT "", /* Warehouse */
            INPUT "", /* Location */
            INPUT ipcJobNo,
            INPUT ipiJobNo2,
            INPUT "I",
            INPUT FALSE /* Empty existing temp-table records */
            ).                    
    END.
    
    ASSIGN
        iConsumedTags    = DYNAMIC-FUNCTION(
                              'fCalculateTagCountInTTbrowse' IN hdInventoryProcs,
                              gcStatusStockConsumed
                              )
        iScannedTags     = DYNAMIC-FUNCTION(
                              'fCalculateTagCountInTTbrowse' IN hdInventoryProcs,
                              gcStatusStockScanned
                              )
        iOnHandTags      = DYNAMIC-FUNCTION(
                              'fCalculateTagCountInTTbrowse' IN hdInventoryProcs,
                              gcStatusStockReceived
                              )
        dConsumedQty     = DYNAMIC-FUNCTION(
                              'fCalculateTagQuantityInTTbrowse' IN hdInventoryProcs,
                              gcStatusStockConsumed
                              )
        dScannedQty      = DYNAMIC-FUNCTION(
                              'fCalculateTagQuantityInTTbrowse' IN hdInventoryProcs,
                              gcStatusStockScanned
                              )
        dOnHandQty       = DYNAMIC-FUNCTION(
                              'fCalculateTagQuantityInTTbrowse' IN hdInventoryProcs,
                              gcStatusStockReceived
                              )
        btTotal:LABEL    = "On Hand: " + STRING(iOnHandTags)
        btScanned:LABEL  = "Scanned: " + STRING(iScannedTags)
        btConsumed:LABEL = "Consumed: " + STRING(iConsumedTags)
        .

    ASSIGN
        fiTotalQty:SCREEN-VALUE    = "Qty: " + STRING(dOnHandQty)
        fiScannedQty:SCREEN-VALUE  = "Qty: " + STRING(dScannedQty)
        fiConsumedQty:SCREEN-VALUE = "Qty: " + STRING(dConsumedQty)
        fiUOM:SCREEN-VALUE         = cConsUOM
        .
    
    {&OPEN-BROWSERS-IN-QUERY-F-Main}    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReOpenBrowse W-Win 
PROCEDURE pReOpenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CASE cColumnLabel:
        WHEN "quantity" THEN
            RUN pByQuantity.
        WHEN "quantityOriginal" THEN
            RUN pByQuantityOriginal.
        WHEN "locationID" THEN
            RUN pByLocationID.
        WHEN "tag" THEN
            RUN pByTag.
        WHEN "jobID" THEN
            RUN pByJobID.
        WHEN "inventoryStatus" THEN
            RUN pByInventoryStatus.
        OTHERWISE
            {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
    IF AVAILABLE ttBrowseInventory THEN
        APPLY "VALUE-CHANGED":U TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTagScan W-Win 
PROCEDURE pTagScan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTag           AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cJobNo      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRMItem     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFormNo     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBlankNo    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lValidInv   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSuccess    AS LOGICAL   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiTag:SCREEN-VALUE     = ipcTag
        fiMessage:SCREEN-VALUE = ""
        cMessage               = ""
        .

    RUN pGetInventoryStockDetails IN hdInventoryProcs (
        INPUT  ipcCompany,
        INPUT  ipcTag,
        OUTPUT lValidInv,
        OUTPUT cMessage,
        INPUT-OUTPUT TABLE ttInventoryStockDetails
        ).
  
    IF lValidInv THEN DO:
        FIND FIRST ttInventoryStockDetails
             WHERE ttInventoryStockDetails.tag EQ ipcTag
             NO-ERROR.
        IF AVAILABLE ttInventoryStockDetails THEN
            ASSIGN
                cJobNo   = ttInventoryStockDetails.jobID
                cRMItem  = ttInventoryStockDetails.rmItemID
                iJobNo2  = ttInventoryStockDetails.jobID2
                iFormNo  = ttInventoryStockDetails.formNo
                iBlankNo = ttInventoryStockDetails.blankNo
                .
  
        IF (fiJobno:SCREEN-VALUE   NE cJobNo OR
            cbJobno2:SCREEN-VALUE  NE STRING(iJobNo2,"99") OR
            cbFormno:SCREEN-VALUE  NE STRING(iFormNo,"99") OR
            cbBlankno:SCREEN-VALUE NE STRING(iBlankNo,"99")) AND
            cJobNo NE "" THEN DO:
            MESSAGE "Tag belongs to different Job context. Do you want to switch Job?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
                TITLE "Continue?" UPDATE lSwitchJob AS LOGICAL.
            IF lSwitchJob THEN DO:
                RUN pJobScan (
                    INPUT  ipcCompany,
                    INPUT  cJobNo,
                    INPUT  iJobNo2,
                    INPUT  iFormNo,
                    INPUT  iBlankNo,
                    INPUT  cRMItem,
                    OUTPUT lSuccess,
                    OUTPUT cMessage
                    ).
                
                IF NOT lSuccess THEN DO:
                    RUN pUpdateMessageText (
                        INPUT cMessage,    /* Message Text */
                        INPUT TRUE,        /* Error */
                        INPUT TRUE         /* Alert-box*/
                        ).
                    RETURN.
                END.                    
            END.
            ELSE
                RETURN.
        END.
  
        cFormattedJobno = cJobNo.
        
        IF AVAILABLE ttInventoryStockDetails THEN DO:  
            RUN Inventory_CreateRMIssueFromTag in hdInventoryProcs (
                INPUT  ipcCompany,
                INPUT  ttInventoryStockDetails.tag,
                INPUT  lAutoPost,
                OUTPUT lCreated,                    
                OUTPUT cMessage
                ).
            IF lCreated THEN
                cMessage = "Tag '" + ttInventoryStockDetails.tag + "' moved to '" + STRING(lAutoPost, "Consumed/Scanned") + "' status.".

            RUN pUpdateMessageText (
                INPUT cMessage,     /* Message Text */
                INPUT NOT lCreated, /* Error */
                INPUT FALSE         /* Alert-box*/
                ).
        END.
    END.
    ELSE DO:
        cMessage = "Invalid Tag".
        RUN pUpdateMessageText (
            cMessage,    /* Message Text */
            TRUE,        /* Error */
            FALSE        /* Alert-box*/
            ).
        RETURN.
    END.

    RUN pRebuildBrowse (
        INPUT ipcCompany,
        INPUT cFormattedJobNo,
        INPUT INTEGER(cbJobNo2:SCREEN-VALUE),
        INPUT INTEGER(cbFormNo:SCREEN-VALUE),
        INPUT INTEGER(cbBlankNo:SCREEN-VALUE),
        INPUT cbRMItem:SCREEN-VALUE,
        INPUT FALSE
        ).    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateMessageText W-Win 
PROCEDURE pUpdateMessageText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMessage  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplError    AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplAlertBox AS LOGICAL   NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.

    fiMessage:SCREEN-VALUE = "".

    IF iplAlertBox THEN DO:
        MESSAGE ipcMessage
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    ASSIGN
        fiMessage:SCREEN-VALUE = ipcMessage
        fiMessage:FGCOLOR      = 2   /* Green */
        .

    IF iplError THEN
        ASSIGN
            fiMessage:SCREEN-VALUE = "**" + ipcMessage
            fiMessage:FGCOLOR      = 12  /* Red */
            .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRMItemList W-Win 
PROCEDURE pUpdateRMItemList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormno       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankno      AS INTEGER   NO-UNDO.
        
    RUN GetRMItemsForJob IN hdJobProcs (
        INPUT  ipcCompany,
        INPUT  ipcJobNo,
        INPUT  ipiJobNo2,
        INPUT  ipiFormNo,
        INPUT  ipiBlankNo,
        OUTPUT cRMListItems
        ).
    
    cbRMItem:LIST-ITEMS IN FRAME {&FRAME-NAME} = cRMListitems.

    APPLY "VALUE-CHANGED" TO cbRMItem IN FRAME {&FRAME-NAME}.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ttBrowseInventory"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetConcatJobID W-Win 
FUNCTION fGetConcatJobID RETURNS CHARACTER PRIVATE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
    IF ttBrowseInventory.jobID EQ "" THEN
        RETURN "".
    ELSE
        RETURN ttBrowseInventory.jobID + "-" + STRING(ttBrowseInventory.jobID2, "99").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetConcatLocationID W-Win 
FUNCTION fGetConcatLocationID RETURNS CHARACTER PRIVATE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    RETURN ttBrowseInventory.warehouseID + " " 
           + FILL(" ", 5 - LENGTH(ttBrowseInventory.warehouseID)) 
           + ttBrowseInventory.locationID.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

