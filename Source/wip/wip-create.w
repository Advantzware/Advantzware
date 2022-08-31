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

  File: wip-create.w

  Description: Creates a Work In Process tag for an item

  Input Parameters:
    ipcCompany     :Company code
    ipcLocation    :Location code
    ipcJobno       :Primary Job number
    ipcMachine     :Machine code
    ipiJobno2      :Second Job number
    ipiFormno      :Form number of the Job
    ipiBlankno     :Blank number of the Job

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

/* Parameters Definitions ---                                           */
{sys/inc/var.i}

DEFINE VARIABLE cCompany  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hdInventoryProcs  AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdOutputProcs     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdJobProcs        AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdBrowseQuery     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdBrowseBuffer    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdNumericKeyBoard AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdJobDetails      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdJobDetailsWin   AS HANDLE    NO-UNDO.
DEFINE VARIABLE lCreated          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobno2ListItems  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormnoListItems  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBlanknoListItems AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMachineListItems AS CHARACTER NO-UNDO.
DEFINE VARIABLE cValidateJobno    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutputFileName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPathTemplate     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormattedJobno   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCopies           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotTags          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotOnHand        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCount            AS INTEGER   NO-UNDO.
DEFINE VARIABLE cFilterBy         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdWipTagProcs     AS HANDLE    NO-UNDO.

DEFINE VARIABLE gcPathDataFileDefault AS CHARACTER INITIAL "C:\BA\LABEL".

RUN spGetSessionParam("Company", OUTPUT cCompany).
RUN spGetSessionParam("Location", OUTPUT cLocation).

&SCOPED-DEFINE SORTBY-PHRASE BY ttBrowseInventory.lastTransTime DESCENDING

{Inventory/ttBrowseInventory.i}
{Inventory/ttInventory.i}
{Inventory/ttPrintInventoryStock.i}
{methods/defines/sortByDefs.i}
{wip/keyboardDefs.i}

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
&Scoped-define FIELDS-IN-QUERY-br-table ttBrowseInventory.quantity ttBrowseInventory.quantityOriginal ttBrowseInventory.locationID ttBrowseInventory.tag ttBrowseInventory.jobID ttBrowseInventory.inventoryStatus ttBrowseInventory.emptyColumn   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH ttBrowseInventory             WHERE (IF cFilterBy EQ "" THEN                        TRUE                    ELSE                        ttBrowseInventory.inventoryStatus EQ cFilterBy)  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory             WHERE (IF cFilterBy EQ "" THEN                        TRUE                    ELSE                        ttBrowseInventory.inventoryStatus EQ cFilterBy)  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br-table ttBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ttBrowseInventory


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rSelected ls-tag btnNumPad btnKeyboard ~
ls-jobno cb-jobno2 bt-exit cb-formno cb-blankno btnFirst cb-machine btnLast ~
bt-create btnNext btCreated btOH btnPrevious btAll btnNumPad-2 btnNumPad-1 ~
btnNumPad-3 ls-total-run-qty ls-qty-per-tag ls-num-tags br-table ~
btnExitText btnJobDetailText btnDeleteText btnAdjustQtyText btnPrintText 
&Scoped-Define DISPLAYED-OBJECTS ls-tag ls-jobno cb-jobno2 cb-formno ~
cb-blankno cb-machine fiRMItem fiSizeLabel fiSize fiUOMLabel fiUOM ~
ls-total-run-qty ls-qty-per-tag ls-num-tags btnExitText btnJobDetailText ~
btnDeleteText statusMessage btnAdjustQtyText btnPrintText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-adjust-qty 
     IMAGE-UP FILE "Graphics/32x32/gearwheels.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/gearwheels_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Adjust" 
     SIZE 8 BY 1.91 TOOLTIP "Adjust Quantity"
     FONT 35.

DEFINE BUTTON bt-create 
     LABEL "CREATE" 
     SIZE 22.2 BY 3 TOOLTIP "Create Tags"
     FONT 37.

DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Exit".

DEFINE BUTTON bt-print-selected 
     IMAGE-UP FILE "Graphics/32x32/print_new.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/printer_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Print Tags"
     FONT 37.

DEFINE BUTTON btAll 
     LABEL "ALL - 0" 
     SIZE 17.6 BY 2 TOOLTIP "Filter All Tags"
     FONT 37.

DEFINE BUTTON btCreated 
     LABEL "CR - 0" 
     SIZE 17.6 BY 2 TOOLTIP "Filter Created Tags"
     FONT 37.

DEFINE BUTTON btJobDetails 
     IMAGE-UP FILE "Graphics/32x32/UDF.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/UDF_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Job Details".

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "Graphics/32x32/garbage_can.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/garbage_can_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Delete".

DEFINE BUTTON btnFirst 
     IMAGE-UP FILE "Graphics/32x32/first.png":U NO-FOCUS FLAT-BUTTON
     LABEL "First" 
     SIZE 8 BY 1.91 TOOLTIP "First".

DEFINE BUTTON btnKeyboard 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnLast 
     IMAGE-UP FILE "Graphics/32x32/last.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Last" 
     SIZE 8 BY 1.91 TOOLTIP "Last".

DEFINE BUTTON btnNext 
     IMAGE-UP FILE "Graphics/32x32/next.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Next" 
     SIZE 8 BY 1.91 TOOLTIP "Next".

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btnNumPad-1 
     IMAGE-UP FILE "Graphics/24x24/numeric_keypad.gif":U
     LABEL "NumPad1" 
     SIZE 6.4 BY 1.52 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btnNumPad-2 
     IMAGE-UP FILE "Graphics/24x24/numeric_keypad.gif":U
     LABEL "NumPad1" 
     SIZE 6.4 BY 1.52 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btnNumPad-3 
     IMAGE-UP FILE "Graphics/24x24/numeric_keypad.gif":U
     LABEL "NumPad1" 
     SIZE 6.4 BY 1.52 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btnPrevious 
     IMAGE-UP FILE "Graphics/32x32/previous.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Prev" 
     SIZE 8 BY 1.91 TOOLTIP "Previous".

DEFINE BUTTON btOH 
     LABEL "OH - 0" 
     SIZE 17.6 BY 2 TOOLTIP "Filter On-Hand Tags"
     FONT 37.

DEFINE VARIABLE cb-blankno AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 9.8 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE cb-formno AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 9.8 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE cb-jobno2 AS INTEGER FORMAT "999":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000" 
     DROP-DOWN-LIST
     SIZE 12 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE cb-machine AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 37.4 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE btnAdjustQtyText AS CHARACTER FORMAT "X(256)":U INITIAL "ADJUST QTY" 
      VIEW-AS TEXT 
     SIZE 23 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnDeleteText AS CHARACTER FORMAT "X(256)":U INITIAL "DELETE" 
      VIEW-AS TEXT 
     SIZE 15 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnExitText AS CHARACTER FORMAT "X(256)":U INITIAL "EXIT" 
      VIEW-AS TEXT 
     SIZE 9 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnJobDetailText AS CHARACTER FORMAT "X(256)":U INITIAL "JOB DETAIL" 
      VIEW-AS TEXT 
     SIZE 21 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE btnPrintText AS CHARACTER FORMAT "X(256)":U INITIAL "PRINT" 
      VIEW-AS TEXT 
     SIZE 11 BY 1.43
     BGCOLOR 21  NO-UNDO.

DEFINE VARIABLE fiRMItem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1.43
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiSize AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 23.4 BY 1.43
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiSizeLabel AS CHARACTER FORMAT "X(256)":U INITIAL "SIZE:" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.43 NO-UNDO.

DEFINE VARIABLE fiUOM AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.43
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiUOMLabel AS CHARACTER FORMAT "X(256)":U INITIAL "UOM:" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.43 NO-UNDO.

DEFINE VARIABLE ls-jobno AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE ls-num-tags AS INTEGER FORMAT ">>>9":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE ls-qty-per-tag AS DECIMAL FORMAT ">,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE ls-tag AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 63.4 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE ls-total-run-qty AS DECIMAL FORMAT ">,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE statusMessage AS CHARACTER FORMAT "X(256)":U INITIAL "STATUS MESSAGE" 
      VIEW-AS TEXT 
     SIZE 116 BY 1.43 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .

DEFINE RECTANGLE rSelected
     EDGE-PIXELS 0    
     SIZE 19.6 BY 2.38
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
      ttBrowseInventory.quantity WIDTH 25 COLUMN-LABEL "Qty On-Hand" LABEL-BGCOLOR 14
    ttBrowseInventory.quantityOriginal WIDTH 25 COLUMN-LABEL "Qty Original" LABEL-BGCOLOR 14
    ttBrowseInventory.locationID WIDTH 25 COLUMN-LABEL "Location" FORMAT "X(12)" LABEL-BGCOLOR 14
    ttBrowseInventory.tag WIDTH 50 COLUMN-LABEL "Tag #" FORMAT "X(30)" LABEL-BGCOLOR 14
    ttBrowseInventory.jobID WIDTH 25 COLUMN-LABEL "Job #" FORMAT "X(20)" LABEL-BGCOLOR 14
    ttBrowseInventory.inventoryStatus COLUMN-LABEL "Status" FORMAT "X(15)" LABEL-BGCOLOR 14
    ttBrowseInventory.emptyColumn COLUMN-LABEL ""
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-SCROLLBAR-VERTICAL SIZE 193 BY 21.19
         BGCOLOR 15 FGCOLOR 0 FONT 36 ROW-HEIGHT-CHARS .95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btJobDetails AT ROW 2.43 COL 168 WIDGET-ID 154
     bt-adjust-qty AT ROW 31.95 COL 169 WIDGET-ID 110
     bt-print-selected AT ROW 31.95 COL 195 WIDGET-ID 114
     btnDelete AT ROW 31.95 COL 17 WIDGET-ID 116
     ls-tag AT ROW 2.67 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     btnNumPad AT ROW 4.29 COL 186 WIDGET-ID 120
     btnKeyboard AT ROW 4.57 COL 14 WIDGET-ID 132
     ls-jobno AT ROW 4.67 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cb-jobno2 AT ROW 4.67 COL 72.8 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     bt-exit AT ROW 1 COL 195 WIDGET-ID 84
     cb-formno AT ROW 4.67 COL 99.8 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     cb-blankno AT ROW 4.67 COL 127 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     btnFirst AT ROW 11.24 COL 195 WIDGET-ID 156
     cb-machine AT ROW 6.48 COL 24 COLON-ALIGNED NO-LABEL WIDGET-ID 94
     fiRMItem AT ROW 6.48 COL 62 COLON-ALIGNED NO-LABEL WIDGET-ID 144
     fiSizeLabel AT ROW 6.48 COL 119 COLON-ALIGNED NO-LABEL WIDGET-ID 146
     fiSize AT ROW 6.48 COL 127.6 COLON-ALIGNED NO-LABEL WIDGET-ID 148
     btnLast AT ROW 16.95 COL 195 WIDGET-ID 158
     fiUOMLabel AT ROW 6.48 COL 150 COLON-ALIGNED NO-LABEL WIDGET-ID 150
     fiUOM AT ROW 6.48 COL 160.6 COLON-ALIGNED NO-LABEL WIDGET-ID 152
     bt-create AT ROW 7.29 COL 178 WIDGET-ID 108
     btnNext AT ROW 15.05 COL 195 WIDGET-ID 160
     btCreated AT ROW 8.14 COL 121 WIDGET-ID 136
     btOH AT ROW 8.14 COL 140 WIDGET-ID 140
     btnPrevious AT ROW 13.14 COL 195 WIDGET-ID 162
     btAll AT ROW 8.14 COL 159 WIDGET-ID 138
     btnNumPad-2 AT ROW 8.62 COL 38.6 WIDGET-ID 126
     btnNumPad-1 AT ROW 8.62 COL 82.8 WIDGET-ID 124
     btnNumPad-3 AT ROW 8.62 COL 112.8 WIDGET-ID 128
     ls-total-run-qty AT ROW 8.71 COL 18 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     ls-qty-per-tag AT ROW 8.71 COL 62 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     ls-num-tags AT ROW 8.71 COL 100.2 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     br-table AT ROW 10.52 COL 2 WIDGET-ID 200
     btnExitText AT ROW 1.24 COL 186 NO-LABEL WIDGET-ID 24
     btnJobDetailText AT ROW 2.67 COL 145 COLON-ALIGNED NO-LABEL WIDGET-ID 164
     btnDeleteText AT ROW 32.19 COL 2 NO-LABEL WIDGET-ID 20
     statusMessage AT ROW 32.19 COL 28 NO-LABEL WIDGET-ID 28
     btnAdjustQtyText AT ROW 32.19 COL 144 COLON-ALIGNED NO-LABEL WIDGET-ID 134
     btnPrintText AT ROW 32.19 COL 182 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     "TAGS:" VIEW-AS TEXT
          SIZE 11 BY 1.33 AT ROW 8.71 COL 91 WIDGET-ID 104
     "RM or WIP TAG:" VIEW-AS TEXT
          SIZE 28 BY 1.33 AT ROW 2.67 COL 5 WIDGET-ID 86
     "QTY/TAG:" VIEW-AS TEXT
          SIZE 17.4 BY 1.33 AT ROW 8.81 COL 45.6 WIDGET-ID 96
     "FORM:" VIEW-AS TEXT
          SIZE 11.8 BY 1.33 AT ROW 4.81 COL 89 WIDGET-ID 48
     "BLANK:" VIEW-AS TEXT
          SIZE 14 BY 1.33 AT ROW 4.76 COL 114.2 WIDGET-ID 58
     "JOB:" VIEW-AS TEXT
          SIZE 9 BY 1.33 AT ROW 4.57 COL 24 WIDGET-ID 12
     "MACHINE/OP:" VIEW-AS TEXT
          SIZE 24 BY 1.33 AT ROW 6.48 COL 2 WIDGET-ID 92
     "RUN QTY:" VIEW-AS TEXT
          SIZE 18 BY 1.33 AT ROW 8.86 COL 2 WIDGET-ID 100
     "(OPTIONAL)" VIEW-AS TEXT
          SIZE 18.8 BY .95 AT ROW 2.86 COL 100.2 WIDGET-ID 90
          FONT 36
     RECT-2 AT ROW 4.1 COL 185 WIDGET-ID 130
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202.2 BY 32.91
         BGCOLOR 21 FGCOLOR 15 FONT 38 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     rSelected AT ROW 7.91 COL 158 WIDGET-ID 142
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202.2 BY 32.91
         BGCOLOR 21 FGCOLOR 15 FONT 38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Create WIP"
         HEIGHT             = 32.81
         WIDTH              = 202
         MAX-HEIGHT         = 32.91
         MAX-WIDTH          = 202.2
         VIRTUAL-HEIGHT     = 32.91
         VIRTUAL-WIDTH      = 202.2
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = yes
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
/* BROWSE-TAB br-table ls-num-tags F-Main */
ASSIGN 
       br-table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR BUTTON bt-adjust-qty IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-print-selected IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btJobDetails IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnDelete IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN btnDeleteText IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN btnExitText IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       btnKeyboard:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnNumPad-1:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnNumPad-2:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnNumPad-3:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiRMItem IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSize IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSizeLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiUOM IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiUOMLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN statusMessage IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory
            WHERE (IF cFilterBy EQ "" THEN
                       TRUE
                   ELSE
                       ttBrowseInventory.inventoryStatus EQ cFilterBy)
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Create WIP */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Create WIP */
DO:    
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
ON START-SEARCH OF br-table IN FRAME F-Main
DO:
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
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table W-Win
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO: 
    ASSIGN
        bt-adjust-qty:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
        btnDelete:SENSITIVE = AVAILABLE ttBrowseInventory AND
                              DYNAMIC-FUNCTION("fCanDeleteInventoryStock" IN hdInventoryProcs, 
                                               ttBrowseInventory.inventoryStockID)
        bt-adjust-qty:SENSITIVE = AVAILABLE ttBrowseInventory AND 
                                  (ttBrowseInventory.inventoryStatus EQ gcStatusStockInitial)
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-adjust-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-adjust-qty W-Win
ON CHOOSE OF bt-adjust-qty IN FRAME F-Main /* Adjust */
DO:
    DEFINE VARIABLE dTotalQuantity   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSubUnitCount    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSubUnitsPerUnit AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dPartialQuantity AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cAdjReasonCode   AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE lValueReturned   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dValue           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lContinue        AS LOGICAL   NO-UNDO.

    IF AVAILABLE ttBrowseInventory THEN DO:
        RUN inventory/adjustQuantity.w (
            INPUT  ttBrowseInventory.quantityOriginal,
            INPUT  ttBrowseInventory.quantityOfSubUnits,
            INPUT  1,
            INPUT  FALSE, /* Required Adj Reason  */
            INPUT  TRUE,  /* Display sub units */
            INPUT  TRUE,  /* Allow decimal units */
            OUTPUT dTotalQuantity,
            OUTPUT dSubUnitCount,
            OUTPUT dSubUnitsPerUnit,
            OUTPUT dPartialQuantity,
            OUTPUT cAdjReasonCode,
            OUTPUT lValueReturned,
            OUTPUT dValue
            ).

        IF lValueReturned THEN DO:
            IF ttBrowseInventory.quantityOriginal EQ dTotalQuantity THEN DO:
                RUN pStatusMessage ("ADJUSTED QUANTITY FOR TAG " + ttBrowseInventory.tag + " IS SAME AS EXISTING QUANTITY", 3).
                RETURN.
            END.
            
            RUN sharpshooter/messageDialog.w (
                "ADJUST QUANTITY OF TAG " + ttBrowseInventory.tag +
                " TO " + STRING(dTotalQuantity) + "?",
                YES,
                YES,
                NO,
                OUTPUT lContinue
                ).    
            IF lContinue THEN
                RUN pAdjustQuantity (
                    cCompany,
                    ttBrowseInventory.inventoryStockID,
                    dTotalQuantity - ttBrowseInventory.quantityOriginal,
                    ttBrowseInventory.quantityUOM
                    ).                     
        END.        
    END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-create
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-create W-Win
ON CHOOSE OF bt-create IN FRAME F-Main /* CREATE */
DO: 
    IF DECIMAL(ls-qty-per-tag:SCREEN-VALUE) EQ 0 THEN DO:
        RUN pStatusMessage ("QUANTITY PER TAG CANNOT BE 0.00", 3).
        APPLY "ENTRY" TO ls-qty-per-tag.
        RETURN.    
    END.
    
    IF DECIMAL(ls-total-run-qty:SCREEN-VALUE) LT DECIMAL(ls-qty-per-tag:SCREEN-VALUE) THEN DO:
        RUN pStatusMessage ("TOTAL RUN QTY " + STRING(ls-total-run-qty:SCREEN-VALUE) +
            " CANNOT BE LESS THAN QUANTITY PER TAG " + STRING(ls-qty-per-tag:SCREEN-VALUE), 3).
        APPLY "ENTRY" TO ls-total-run-qty.
        RETURN.
    END.
    
    RUN CreateTransactionInitializedFromJob IN hdInventoryProcs (
        cCompany,
        cFormattedJobno,
        cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},                         
        cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        "", /* Empty Item */
        DECIMAL(ls-total-run-qty:SCREEN-VALUE),
        DECIMAL(ls-qty-per-tag:SCREEN-VALUE),
        1,  /* Sub Units Per Unit */
        "EA", /* Quantity UOM */
        gcItemTypeWIP,
        OUTPUT lCreated, 
        OUTPUT cMessage
        ).
    
    IF NOT lCreated THEN
        RUN pStatusMessage (CAPS(cMessage), 3).
                       
    RUN rebuildTempTable(
        cCompany,
        cFormattedJobno,
        cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},                         
        cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        ).               
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit W-Win
ON CHOOSE OF bt-exit IN FRAME F-Main
DO:
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.

    IF VALID-HANDLE(hdInventoryProcs) THEN
        DELETE OBJECT hdInventoryProcs.

    IF VALID-HANDLE(hdOutputProcs) THEN
        DELETE OBJECT hdOutputProcs.

    IF VALID-HANDLE(hdJobProcs) THEN
        DELETE OBJECT hdJobProcs.
    
    IF VALID-HANDLE(hdWipTagProcs) THEN
        DELETE OBJECT hdWipTagProcs.

    IF VALID-HANDLE(hdJobDetailsWin) THEN
        APPLY "WINDOW-CLOSE" TO hdJobDetailsWin.

    IF VALID-HANDLE(hdJobDetails) THEN
        DELETE OBJECT hdInventoryProcs.

    APPLY "CLOSE":U TO THIS-PROCEDURE.
    
    RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-print-selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-print-selected W-Win
ON CHOOSE OF bt-print-selected IN FRAME F-Main
DO: 
    DEFINE VARIABLE lOptionSelected AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cOption         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lXprint         AS LOGICAL   NO-UNDO.
     
    lXprint =  IF cPathTemplate MATCHES "*.xpr*" THEN YES ELSE NO .
    
    RUN Inventory\PrintDialog.w (
        OUTPUT lOptionSelected,
        OUTPUT cOption
        ).
        
    IF NOT lOptionSelected THEN
        RETURN.

    EMPTY TEMP-TABLE ttPrintInventoryStock.
    
    /* Print All Option */
    IF cOption EQ "All" THEN DO:
        FOR EACH ttBrowseInventory:
            IF ttBrowseInventory.inventoryStatus EQ gcStatusStockInitial THEN
                RUN PostReceivedInventory IN hdInventoryProcs (
                    INPUT cCompany,
                    INPUT ttBrowseInventory.inventoryStockID
                    ).
    
            RUN CreatePrintInventory IN hdInventoryProcs (
                INPUT ttBrowseInventory.inventoryStockID,
                INPUT-OUTPUT TABLE ttPrintInventoryStock 
                ).
        END.
        IF lXprint THEN
        RUN pXprint.
        ELSE 
        RUN pPrintLabels.
    END.
    /* Print Selected Option */
    ELSE IF cOption EQ "Selected" THEN DO:
        IF AVAILABLE ttBrowseInventory THEN DO:
            IF ttBrowseInventory.inventoryStatus EQ gcStatusStockInitial THEN
                RUN PostReceivedInventory IN hdInventoryProcs (
                    INPUT cCompany,
                    INPUT ttBrowseInventory.inventoryStockID
                    ).
        

            RUN CreatePrintInventory IN hdInventoryProcs (
                INPUT ttBrowseInventory.inventoryStockID,
                INPUT-OUTPUT TABLE ttPrintInventoryStock
                ).
        
            IF lXprint THEN
            RUN pXprint.
            ELSE 
            RUN pPrintLabels.
            
        END.
    END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAll W-Win
ON CHOOSE OF btAll IN FRAME F-Main /* ALL - 0 */
DO:
    RUN pHighlightButton (
        INPUT ""   /* All */
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCreated
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCreated W-Win
ON CHOOSE OF btCreated IN FRAME F-Main /* CR - 0 */
DO:
    RUN pHighlightButton (
        INPUT gcStatusStockInitial
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
            INPUT cFormattedJobno,
            INPUT INTEGER(cb-jobno2:SCREEN-VALUE),
            INPUT INTEGER(cb-formno:SCREEN-VALUE),
            INPUT INTEGER(cb-blankno:SCREEN-VALUE)
            ) NO-ERROR.            

        IF hdJobDetailsWin:WINDOW-STATE EQ 2 THEN ASSIGN 
            hdJobDetailsWin:WINDOW-STATE = 3.
        
        hdJobDetailsWin:MOVE-TO-TOP().
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdjustQtyText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdjustQtyText W-Win
ON MOUSE-SELECT-CLICK OF btnAdjustQtyText IN FRAME F-Main
DO:
    IF bt-adjust-qty:SENSITIVE THEN
    APPLY "CHOOSE":U TO bt-adjust-qty.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete W-Win
ON CHOOSE OF btnDelete IN FRAME F-Main
DO:
    RUN pDelete.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeleteText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeleteText W-Win
ON MOUSE-SELECT-CLICK OF btnDeleteText IN FRAME F-Main
DO:
  APPLY "CHOOSE":U TO btnDelete.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExitText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExitText W-Win
ON MOUSE-SELECT-CLICK OF btnExitText IN FRAME F-Main
DO:
    APPLY "CHOOSE":U TO bt-Exit.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst W-Win
ON CHOOSE OF btnFirst IN FRAME F-Main /* First */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnJobDetailText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnJobDetailText W-Win
ON MOUSE-SELECT-CLICK OF btnJobDetailText IN FRAME F-Main
DO:
    IF btJobDetails:SENSITIVE THEN
    APPLY "CHOOSE":U TO btJobDetails.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyboard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyboard W-Win
ON CHOOSE OF btnKeyboard IN FRAME F-Main /* Keyboard */
DO:
    APPLY "ENTRY":U TO ls-jobno.
    RUN pKeyboard (ls-jobno:HANDLE, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast W-Win
ON CHOOSE OF btnLast IN FRAME F-Main /* Last */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext W-Win
ON CHOOSE OF btnNext IN FRAME F-Main /* Next */
DO:
    RUN pNavigate (SELF).
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


&Scoped-define SELF-NAME btnNumPad-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNumPad-1 W-Win
ON CHOOSE OF btnNumPad-1 IN FRAME F-Main /* NumPad1 */
DO:
    APPLY "ENTRY":U TO ls-qty-per-tag.
    RUN pKeyboard (ls-qty-per-tag:HANDLE, "Numeric").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNumPad-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNumPad-2 W-Win
ON CHOOSE OF btnNumPad-2 IN FRAME F-Main /* NumPad1 */
DO:
    APPLY "ENTRY":U TO ls-total-run-qty.
    RUN pKeyboard (ls-total-run-qty:HANDLE, "Numeric").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNumPad-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNumPad-3 W-Win
ON CHOOSE OF btnNumPad-3 IN FRAME F-Main /* NumPad1 */
DO:
    APPLY "ENTRY":U TO ls-num-tags.
    RUN pKeyboard (ls-num-tags:HANDLE, "Numeric").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrevious
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevious W-Win
ON CHOOSE OF btnPrevious IN FRAME F-Main /* Prev */
DO:
    RUN pNavigate (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrintText
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrintText W-Win
ON MOUSE-SELECT-CLICK OF btnPrintText IN FRAME F-Main
DO:
    IF bt-print-selected:SENSITIVE THEN
    APPLY "CHOOSE":U TO bt-print-selected.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOH
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOH W-Win
ON CHOOSE OF btOH IN FRAME F-Main /* OH - 0 */
DO:
    RUN pHighlightButton (
        INPUT gcStatusStockReceived
        ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-blankno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-blankno W-Win
ON VALUE-CHANGED OF cb-blankno IN FRAME F-Main
DO:
    RUN onValueChangedOfJobDetails.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-formno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-formno W-Win
ON VALUE-CHANGED OF cb-formno IN FRAME F-Main
DO:
    RUN onValueChangedOfJobDetails.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-jobno2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-jobno2 W-Win
ON VALUE-CHANGED OF cb-jobno2 IN FRAME F-Main
DO:
    RUN pUpdateMachineList (
        INPUT  cFormattedJobno,
        INPUT  cb-jobno2:SCREEN-VALUE,
        OUTPUT cMachineListItems
        ).
        
    RUN onValueChangedOfJobDetails.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-machine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-machine W-Win
ON VALUE-CHANGED OF cb-machine IN FRAME F-Main
DO:
    RUN onValueChangedOfJobDetails.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-jobno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-jobno W-Win
ON ENTRY OF ls-jobno IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
    RUN pKeyboard (SELF, "Qwerty").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-jobno W-Win
ON HELP OF ls-jobno IN FRAME F-Main
DO:
    DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.

    RUN system/openlookup.p (
        INPUT  cCompany, 
        INPUT  "job-no",        /* Lookup ID */
        INPUT  0,               /* Subject ID */
        INPUT  "",              /* User ID */
        INPUT  0,               /* Param Value ID */
        OUTPUT cFieldsValue, 
        OUTPUT cFoundValue, 
        OUTPUT recFoundRecID
        ).
        
    IF cFoundValue NE "" THEN DO:    
        SELF:SCREEN-VALUE = cFoundValue.
        
        APPLY "LEAVE":U TO SELF.
                    
        ASSIGN
            cb-jobno2:SCREEN-VALUE  = IF NUM-ENTRIES(cFieldsValue,"|") GE 9 AND
                                         INDEX(cb-jobno2:LIST-ITEMS, STRING(INTEGER(ENTRY(9,cFieldsValue,"|")),"999")) GT 0 THEN
                                          ENTRY(9,cFieldsValue,"|")
                                      ELSE
                                          ENTRY(1,cb-jobno2:LIST-ITEMS)
            cb-formno:SCREEN-VALUE  = IF NUM-ENTRIES(cFieldsValue,"|") GE 8 AND
                                         INDEX(cb-formno:LIST-ITEMS, STRING(INTEGER(ENTRY(8,cFieldsValue,"|")),"99")) GT 0 THEN
                                          ENTRY(8,cFieldsValue,"|")
                                      ELSE
                                          ENTRY(1,cb-formno:LIST-ITEMS)
            cb-blankno:SCREEN-VALUE = IF NUM-ENTRIES(cFieldsValue,"|") GE 10 AND
                                         INDEX(cb-blankno:LIST-ITEMS, STRING(INTEGER(ENTRY(10,cFieldsValue,"|")),"99")) GT 0 THEN
                                          ENTRY(10,cFieldsValue,"|")
                                      ELSE
                                          ENTRY(1,cb-blankno:LIST-ITEMS)
            NO-ERROR.
            
        APPLY "VALUE-CHANGED" to cb-jobno2.
        
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-jobno W-Win
ON LEAVE OF ls-jobno IN FRAME F-Main
DO:
    DEFINE VARIABLE cJobNo     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo2    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormNo    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBlankNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lParse     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidJob  AS LOGICAL   NO-UNDO.    
    
    IF VALID-HANDLE(hKeyboard) THEN
    DELETE OBJECT hKeyboard.
    
    IF cValidateJobno EQ ls-jobno:SCREEN-VALUE THEN
        RETURN.
    
    EMPTY TEMP-TABLE ttBrowseInventory.
    
    {&OPEN-BROWSERS-IN-QUERY-F-Main} 
    
    SESSION:SET-WAIT-STATE("GENERAL").     
    
    RUN disableCreate.

    ASSIGN 
        cJobno2ListItems       = ""
        cFormnoListItems       = ""
        cBlanknoListitems      = ""
        cMachineListItems      = ""
        cMessage               = ""
        fiRMItem:SCREEN-VALUE  = ""
        fiSize:SCREEN-VALUE    = ""
        fiUOM:SCREEN-VALUE     = ""        
        btAll:LABEL            = "All - 0"
        btCreated:LABEL        = "Cr - 0"
        btOH:LABEL             = "OH - 0"
        btJobDetails:SENSITIVE = FALSE
        .
        
    RUN JobParser IN hdJobProcs (
        SELF:SCREEN-VALUE,
        OUTPUT cJobNo,
        OUTPUT cJobNo2,
        OUTPUT cFormNo,
        OUTPUT cBlankNo,
        OUTPUT lParse,
        OUTPUT cMessage
        ).

    cFormattedJobno = DYNAMIC-FUNCTION (
                      "fAddSpacesToString" IN hdJobProcs, ls-jobno:SCREEN-VALUE, iJobLen, TRUE
                      ).                                  

    IF lParse THEN
        ASSIGN
            SELF:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', cJobNo))    
            cFormattedJobno = DYNAMIC-FUNCTION (
                              "fAddSpacesToString" IN hdJobProcs, cJobNo, iJobLen, TRUE
                              ).

    IF cMessage NE "" THEN DO:
        SESSION:SET-WAIT-STATE("").
        RUN pStatusMessage (CAPS(cMessage), 3).
        RETURN.
    END.
                    
    RUN updateComboBoxes.

    IF lParse THEN
        IF (cJobNo2 NE "" AND INDEX(cJobno2ListItems,STRING(INTEGER(cJobNo2),"999")) LE 0) OR
           (cFormNo NE "" AND INDEX(cFormnoListItems,STRING(INTEGER(cFormNo),"99")) LE 0) OR
           (cBlankNo NE "" AND INDEX(cBlanknoListitems,STRING(INTEGER(cBlankNo),"99")) LE 0) THEN DO:
            RUN pStatusMessage ("INVALID JOB SCAN, PLEASE SCAN A VALID JOB NUMBER.", 3). 
            
            ASSIGN
                cFormattedJobNo         = ""
                cValidateJobno          = ""
                SELF:SCREEN-VALUE       = ""
                cb-jobno2:LIST-ITEMS    = "000"
                cb-formno:LIST-ITEMS    = "00"
                cb-blankno:LIST-ITEMS   = "00"
                cb-machine:LIST-ITEMS   = ""
                cb-jobno2:SCREEN-VALUE  = "000"
                cb-formno:SCREEN-VALUE  = "00"
                cb-blankno:SCREEN-VALUE = "00"
                cb-machine:SCREEN-VALUE = ""
                .           
            
            SESSION:SET-WAIT-STATE("").
            
            RETURN NO-APPLY.
        END.
        ELSE
            ASSIGN
                cb-jobno2:SCREEN-VALUE  = IF cJobNo2 EQ "" THEN 
                                              ENTRY(1,cJobno2ListItems)
                                          ELSE
                                              STRING(INTEGER(cJobNo2),"999")
                cb-formno:SCREEN-VALUE  = IF cFormNo EQ "" THEN
                                              ENTRY(1,cFormnoListItems)
                                          ELSE
                                              STRING(INTEGER(cFormNo),"99")
                cb-blankno:SCREEN-VALUE = IF cBlankNo EQ "" THEN
                                              ENTRY(1,cBlanknoListItems)
                                          ELSE
                                              STRING(INTEGER(cBlankNo),"99")
                .
    ELSE
        ASSIGN 
            cb-jobno2:SCREEN-VALUE  = ENTRY(1,cJobno2ListItems)
            cb-formno:SCREEN-VALUE  = ENTRY(1,cFormnoListItems)
            cb-blankno:SCREEN-VALUE = ENTRY(1,cBlanknoListItems)
            .
            
    APPLY "VALUE-CHANGED" to cb-jobno2. 
          
    RUN ValidateJob IN hdJobProcs (
        INPUT cCompany,
        INPUT cFormattedJobno,
        INPUT cb-machine:SCREEN-VALUE,
        INPUT INTEGER(cb-jobno2:SCREEN-VALUE),
        INPUT INTEGER(cb-formno:SCREEN-VALUE),
        INPUT INTEGER(cb-blankno:SCREEN-VALUE),
        OUTPUT lValidJob
        ).
            
    IF lValidJob THEN
        RUN enableCreate.

    /* Additional validation to check if job still doesn't exist. 
       In this case machine code is passed empty to check if job is valid*/
    IF NOT lValidJob THEN
        RUN ValidateJob IN hdJobProcs (
            INPUT cCompany,
            INPUT cFormattedJobno,
            INPUT "", /* Blank Machine code */
            INPUT INTEGER(cb-jobno2:SCREEN-VALUE),
            INPUT INTEGER(cb-formno:SCREEN-VALUE),
            INPUT INTEGER(cb-blankno:SCREEN-VALUE),
            OUTPUT lValidJob
            ).
    
    IF NOT lValidJob THEN DO:
        RUN pStatusMessage ("INVALID JOB NUMBER " + SELF:SCREEN-VALUE + ", PLEASE ENTER A VALID JOB NUMBER.", 3). 
        
        ASSIGN
            SELF:SCREEN-VALUE = ""
            cValidateJobno    = ""
            .
        SESSION:SET-WAIT-STATE("").
        RETURN NO-APPLY.
    END.
    ELSE RUN pStatusMessage ("", 0).

    btJobDetails:SENSITIVE = TRUE.
                            
    cValidateJobno = ls-jobno:SCREEN-VALUE.    
                           
    RUN rebuildTempTable (
        INPUT cCompany,
        INPUT cFormattedJobno,
        INPUT cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},                         
        INPUT cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        ).    

    SESSION:SET-WAIT-STATE("").        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-num-tags
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-num-tags W-Win
ON ENTRY OF ls-num-tags IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
    RUN pKeyboard (SELF, "Numeric").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-num-tags W-Win
ON LEAVE OF ls-num-tags IN FRAME F-Main
DO:
    IF VALID-HANDLE(hKeyboard) THEN
    DELETE OBJECT hKeyboard.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-qty-per-tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-qty-per-tag W-Win
ON ENTRY OF ls-qty-per-tag IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
    RUN pKeyboard (SELF, "Numeric").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-qty-per-tag W-Win
ON LEAVE OF ls-qty-per-tag IN FRAME F-Main
DO: 
    IF VALID-HANDLE(hKeyboard) THEN
    DELETE OBJECT hKeyboard.
    
    IF DECIMAL(ls-total-run-qty:SCREEN-VALUE) EQ 0 THEN
        ls-total-run-qty:SCREEN-VALUE = ls-qty-per-tag:SCREEN-VALUE.
    
    IF DECIMAL(ls-qty-per-tag:SCREEN-VALUE) NE 0 AND
        DECIMAL(ls-total-run-qty:SCREEN-VALUE) NE 0 AND
        DECIMAL(ls-qty-per-tag:SCREEN-VALUE) LT DECIMAL(ls-total-run-qty:SCREEN-VALUE) THEN 
    DO:   
        ASSIGN
            ls-num-tags:SCREEN-VALUE = STRING(INTEGER(TRUNC(DECIMAL(ls-total-run-qty:SCREEN-VALUE) / DECIMAL(ls-qty-per-tag:SCREEN-VALUE),0)))
            bt-create:LABEL          = "Create All".
    END.
    ELSE
        ASSIGN
            ls-num-tags:SCREEN-VALUE = "1"
            bt-create:LABEL          = "Create".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-tag W-Win
ON LEAVE OF ls-tag IN FRAME F-Main
DO:
    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN.
    
    ASSIGN
        fiRMItem:SCREEN-VALUE = ""
        fiSize:SCREEN-VALUE   = ""
        fiUOM:SCREEN-VALUE    = ""
        btAll:LABEL           = "All - 0"
        btCreated:LABEL       = "Cr - 0"
        btOH:LABEL            = "OH - 0"        
        .
                        
    RUN tagScan(SELF:SCREEN-VALUE).  
        
    RUN rebuildTempTable(
        INPUT cCompany,
        INPUT ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},                         
        INPUT cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        ).
        
    IF SELF:SCREEN-VALUE EQ "" THEN
        RETURN NO-APPLY.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-total-run-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-total-run-qty W-Win
ON ENTRY OF ls-total-run-qty IN FRAME F-Main
DO:
    hFocusField = SELF.
    IF lKeyboard THEN
    RUN pKeyboard (SELF, "Numeric").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-total-run-qty W-Win
ON LEAVE OF ls-total-run-qty IN FRAME F-Main
DO:    
    IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.
    
    IF DECIMAL(ls-qty-per-tag:SCREEN-VALUE) NE 0 AND
        DECIMAL(ls-total-run-qty:SCREEN-VALUE) NE 0 AND
        DECIMAL(ls-qty-per-tag:SCREEN-VALUE) LT DECIMAL(ls-total-run-qty:SCREEN-VALUE) THEN 
    DO:    
        ASSIGN
            ls-num-tags:SCREEN-VALUE = STRING(INTEGER(TRUNC(DECIMAL(ls-total-run-qty:SCREEN-VALUE) / DECIMAL(ls-qty-per-tag:SCREEN-VALUE),0)))
            bt-create:LABEL          = "Create All".
    END.
    ELSE
        ASSIGN
            ls-num-tags:SCREEN-VALUE = "1"
            bt-create:LABEL          = "Create".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

{sharpshooter/pStatusMessage.i}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disableCreate W-Win 
PROCEDURE disableCreate :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    ASSIGN 
        ls-qty-per-tag:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "0"
        ls-total-run-qty:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
        ls-num-tags:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = "0"
        ls-qty-per-tag:SENSITIVE IN FRAME {&FRAME-NAME}      = FALSE
        ls-total-run-qty:SENSITIVE IN FRAME {&FRAME-NAME}    = FALSE
        ls-num-tags:SENSITIVE IN FRAME {&FRAME-NAME}         = FALSE
        bt-create:SENSITIVE IN FRAME {&FRAME-NAME}           = FALSE
        ls-qty-per-tag:BGCOLOR   = 8
        ls-total-run-qty:BGCOLOR = 8
        ls-num-tags:BGCOLOR      = 8
        btnNumPad-1:HIDDEN = YES
        btnNumPad-2:HIDDEN = YES
        btnNumPad-3:HIDDEN = YES
        .

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableCreate W-Win 
PROCEDURE enableCreate :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    ASSIGN 
        ls-qty-per-tag:SENSITIVE IN FRAME {&FRAME-NAME}   = TRUE
        ls-total-run-qty:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
        ls-num-tags:SENSITIVE IN FRAME {&FRAME-NAME}      = TRUE
        bt-create:SENSITIVE IN FRAME {&FRAME-NAME}        = TRUE
        ls-qty-per-tag:BGCOLOR   = 15
        ls-total-run-qty:BGCOLOR = 15
        ls-num-tags:BGCOLOR      = 15
        btnNumPad-1:HIDDEN = NO
        btnNumPad-2:HIDDEN = NO
        btnNumPad-3:HIDDEN = NO
        .

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
  DISPLAY ls-tag ls-jobno cb-jobno2 cb-formno cb-blankno cb-machine fiRMItem 
          fiSizeLabel fiSize fiUOMLabel fiUOM ls-total-run-qty ls-qty-per-tag 
          ls-num-tags btnExitText btnJobDetailText btnDeleteText statusMessage 
          btnAdjustQtyText btnPrintText 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE rSelected ls-tag btnNumPad btnKeyboard ls-jobno cb-jobno2 bt-exit 
         cb-formno cb-blankno btnFirst cb-machine btnLast bt-create btnNext 
         btCreated btOH btnPrevious btAll btnNumPad-2 btnNumPad-1 btnNumPad-3 
         ls-total-run-qty ls-qty-per-tag ls-num-tags br-table btnExitText 
         btnJobDetailText btnDeleteText btnAdjustQtyText btnPrintText 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init W-Win 
PROCEDURE init :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
    RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.
    RUN wip/wipTagProcs.p PERSISTENT SET hdWipTagProcs.
   
    FIND FIRST company NO-LOCK 
         WHERE company.company EQ cCompany
         NO-ERROR .
    IF AVAILABLE company THEN
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                         + " - " + DYNAMIC-FUNCTION("sfVersion") + " - " 
                         + STRING(company.name) + " - " + cLocation.

    ASSIGN 
        hdBrowseQuery  = {&BROWSE-NAME}:QUERY IN FRAME {&FRAME-NAME}
        hdBrowseBuffer = hdBrowseQuery:GET-BUFFER-HANDLE(1)
        .
    RUN pGetSettings(cCompany, OUTPUT cOutputFileName, OUTPUT cPathTemplate, OUTPUT iCopies).
    
    APPLY "ENTRY" TO ls-jobno IN FRAME {&FRAME-NAME}.

    RUN disableCreate.
    RUN pStatusMessage ("", 0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobScan W-Win 
PROCEDURE jobScan :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcJobno    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcMachine  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipiJobno2   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER  ipiFormno   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER  ipiBlankno  AS INTEGER   NO-UNDO.

    DEFINE VARIABLE lValidJob AS LOGICAL NO-UNDO.

    RUN ValidateJob IN hdJobProcs (
        INPUT ipcCompany,
        INPUT ipcJobno,
        INPUT ipcMachine,
        INPUT ipiJobno2,
        INPUT ipiFormno,
        INPUT ipiBlankno,
        OUTPUT lValidJob
        ).
        
    IF NOT lValidJob THEN 
    DO: 
        RUN pStatusMessage ("INVALID JOB SCAN", 3).
        APPLY "ENTRY" TO ls-jobno in FRAME {&FRAME-NAME}.
        RETURN ERROR.
    END.    

    ASSIGN
        ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', ipcJobno))
        cFormattedJobno = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', ipcJobno)).
    
    RUN updateComboBoxes.
       
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            cb-jobno2:LIST-ITEMS    = cJobno2ListItems
            cb-jobno2:SCREEN-VALUE  = STRING(ipiJobno2,"999")
            .
            
        APPLY "VALUE-CHANGED" to cb-jobno2.
        
        ASSIGN 
            cb-formno:LIST-ITEMS    = cFormnoListItems
            cb-blankno:LIST-ITEMS   = cBlanknoListItems
            cb-machine:LIST-ITEMS   = cMachineListItems
            cb-formno:SCREEN-VALUE  = STRING(ipiFormno,"99")
            cb-blankno:SCREEN-VALUE = STRING(ipiBlankno,"99")
            cb-machine:SCREEN-VALUE = ipcmachine
            .
    END.
    
    cValidateJobno = ls-jobno:SCREEN-VALUE.
                       
    RUN enableCreate.
    
    RUN rebuildTempTable (
        INPUT ipcCompany,
        INPUT cFormattedJobno,
        INPUT cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},                         
        INPUT cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        ).                   
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
    RUN pWinReSize.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN init.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE onValueChangedOfJobDetails W-Win 
PROCEDURE onValueChangedOfJobDetails :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lValidJob AS LOGICAL NO-UNDO.
    
    RUN disableCreate.
    
    RUN ValidateJob IN hdJobProcs (
        INPUT cCompany,
        INPUT cFormattedJobno,
        INPUT cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT INTEGER(cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
        INPUT INTEGER(cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
        INPUT INTEGER(cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
        OUTPUT lValidJob
        ).
    
    IF lValidJob THEN
        RUN enableCreate.

    RUN rebuildTempTable(
        INPUT cCompany,
        INPUT cFormattedJobno,
        INPUT cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},                        
        INPUT cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAdjustQuantity W-Win 
PROCEDURE pAdjustQuantity :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantity         AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM      AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lCreated AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.
                
    RUN CreateTransactionAdjustQuantity IN hdInventoryProcs (
        ipcCompany,
        ipcInventoryStockID,
        ipdQuantity,
        ipcQuantityUOM,
        TRUE, /* Post Transaction */
        OUTPUT lCreated,
        OUTPUT cMessage
        ).               
        
    RUN rebuildTempTable (
        INPUT ipcCompany,
        INPUT cFormattedJobno,
        INPUT cb-machine:SCREEN-VALUE,
        INPUT cb-jobno2:SCREEN-VALUE,
        INPUT cb-formno:SCREEN-VALUE,
        INPUT cb-blankno:SCREEN-VALUE
        ).     
        
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDelete W-Win 
PROCEDURE pDelete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lDelete AS LOGICAL NO-UNDO.

    IF AVAILABLE ttBrowseInventory THEN DO:
        RUN sharpshooter/messageDialog.w (
            "DELETE SELECTION?",
            YES,
            YES,
            NO,
            OUTPUT lDelete
            ).
        IF lDelete THEN DO:
            RUN DeleteInventoryStock IN hdInventoryProcs (ttBrowseInventory.inventoryStockID).
            BROWSE {&BROWSE-NAME}:DELETE-CURRENT-ROW().
        END. /* if ldelete */
        
    END. /* if avail */

    RUN rebuildTempTable(
        cCompany,
        cFormattedJobno,
        cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},                         
        cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings W-Win 
PROCEDURE pGetSettings PRIVATE :
/*------------------------------------------------------------------------------
     Purpose: Returns the key NK1 settings for printing FG Labels
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPathDataFile AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPathTemplate AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiCopies AS INTEGER NO-UNDO.

    DEFINE VARIABLE cReturn AS CHARACTER.
    DEFINE VARIABLE lFound  AS LOGICAL.

    RUN sys/ref/nk1look.p (ipcCompany, "WIPTAG", "C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound). 
    IF lFound THEN 
        opcPathTemplate = cReturn.

    RUN sys/ref/nk1look.p (ipcCompany, "WIPTAG", "I", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound). 
    IF lFound THEN 
        opiCopies = INTEGER(cReturn).
    IF opiCopies EQ 0 THEN opiCopies = 1.
    
    RUN GetBarDirFilePath IN hdOutputProcs (ipcCompany, "WIPTAG", OUTPUT opcPathDataFile).
    IF opcPathDataFile EQ "" THEN opcPathDataFile = gcPathDataFileDefault.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHighlightButton W-Win 
PROCEDURE pHighlightButton :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFilterType AS CHARACTER NO-UNDO.
    
    cFilterBy = ipcFilterType.
    
    CASE ipcFilterType:
        WHEN "" OR
        WHEN "All" THEN
            rSelected:COL IN FRAME {&FRAME-NAME} = btAll:COL - 1.
        WHEN gcStatusStockReceived THEN
            rSelected:COL IN FRAME {&FRAME-NAME} = btOH:COL - 1.
        WHEN gcStatusStockInitial THEN
            rSelected:COL IN FRAME {&FRAME-NAME} = btCreated:COL - 1.    
    END.
    
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintLabels W-Win 
PROCEDURE pPrintLabels PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    IF SEARCH(cPathTemplate) NE ? THEN 
    DO:
        RUN PrintLabelMatrixFile IN hdOutputProcs(cCompany, cPathTemplate, "WIPTAG").
        RUN sharpshooter/messageDialog.w (
            "PRINT CONFIRMATION" + CHR(10) + CHR(10) +
            "LABEL MATRIX PRINT PROCESS HAS BEEN STARTED." + CHR(10) +
            "THIS PROCESS WILL GENERATE PRINTER LABELS FOR YOUR SELECTED WIP TAGS" + CHR(10) + CHR(10) +
            "     DATA FILE: " + cOutputFileName + CHR(10) + 
            "     TEMPLATE FILE: " + cPathTemplate,
            NO,
            NO,
            YES,
            OUTPUT lSuccess
            ).
    END.
    ELSE  
        RUN sharpshooter/messageDialog.w (
            "INVALID TEMPLATE" + CHR(10) + CHR(10) +
            "DATA FILE HAS BEEN GENERATED BUT SYSTEM IS UNABLE TO LOCATE TEMPLATE FILE TO PRINT." + CHR(10) + CHR(10) +
            "     DATA FILE: " + cOutputFileName + CHR(10) +
            "     TEMPLATE FILE: " + cPathTemplate,
            NO,
            NO,
            YES,
            OUTPUT lSuccess
            ).

    RUN Output_TempTableToCSV IN hdOutputProcs ( 
        INPUT TEMP-TABLE ttPrintInventoryStock:HANDLE,
        INPUT cOutputFileName,
        INPUT TRUE,
        INPUT FALSE /* Auto increment File name */,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).
        
    RUN rebuildTempTable(
        INPUT cCompany,
        INPUT cFormattedJobno,
        INPUT cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-jobno2:SCREEN-VALUE  IN FRAME {&FRAME-NAME},
        INPUT cb-formno:SCREEN-VALUE  IN FRAME {&FRAME-NAME},
        INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        ).

    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME} IN FRAME  {&FRAME-NAME}.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse W-Win 
PROCEDURE pReopenBrowse :
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
        WHEN "stockIDAlias" THEN
        RUN pByStockIDAlias.
        WHEN "jobID" THEN
        RUN pByJobID.
        WHEN "inventoryStatus" THEN
        RUN pByInventoryStatus.
        OTHERWISE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
    IF AVAILABLE ttBrowseInventory THEN
    APPLY "VALUE-CHANGED":U TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
    {AOA/includes/pReopenBrowse.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateMachineList W-Win 
PROCEDURE pUpdateMachineList :
/*------------------------------------------------------------------------------
  Purpose:     Gets machine list for Job no and Job no2 combination
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcJobNo            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobNo2           AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMachineListItems AS CHARACTER NO-UNDO.
    
    RUN GetOperationsForJob IN hdJobProcs (
        cCompany,
        ipcJobNo,
        INT(ipcJobNo2),
        ?,
        ?,
        INPUT-OUTPUT opcMachineListItems 
        ).

    IF opcMachineListItems EQ "" THEN
        ASSIGN
            opcMachineListItems                            = ""
            cb-machine:LIST-ITEMS IN FRAME {&FRAME-NAME}   = opcMachineListItems
            cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    ELSE
        cb-machine:LIST-ITEMS IN FRAME {&FRAME-NAME} = opcMachineListItems.
    cb-machine:SCREEN-VALUE = ENTRY(1,opcMachineListItems).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateRMDetails W-Win 
PROCEDURE pUpdateRMDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER lValidTag AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cJobNo    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFormNo   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBlankNo  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cQtyUOM   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.
        
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN GetRMLoadTagDetails IN hdInventoryProcs (
        INPUT  cCompany,
        INPUT  ls-tag:SCREEN-VALUE,        
        OUTPUT cJobNo,   
        OUTPUT iJobNo2,  
        OUTPUT iFormNo,  
        OUTPUT iBlankNo, 
        OUTPUT cQtyUOM,
        OUTPUT cItemName,
        OUTPUT lValidTag,
        OUTPUT cMessage
        ).
    
    IF lValidTag THEN DO:
        ASSIGN
            ls-jobno:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', cJobNo))
            cFormattedJobno       = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', cJobNo))
            .
        
        RUN updateComboBoxes.
        
        ASSIGN 
            cValidateJobno          = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', cJobNo))
            cb-jobno2:SCREEN-VALUE  = STRING(iJobNo2,"999")
            .
            
        APPLY "VALUE-CHANGED" to cb-jobno2.
        
        ASSIGN
            cb-formno:SCREEN-VALUE  = STRING(iFormNo,"99")
            cb-blankno:SCREEN-VALUE = STRING(iBlankNo,"99")
            fiRMItem:SCREEN-VALUE   = cItemName
            fiSize:SCREEN-VALUE     = ""
            fiUOM:SCREEN-VALUE      = cQtyUOM
            NO-ERROR.    
    END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize W-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dCol    AS DECIMAL NO-UNDO.

    SESSION:SET-WAIT-STATE("General").
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            {&WINDOW-NAME}:ROW                 = 1
            {&WINDOW-NAME}:COL                 = 1
            {&WINDOW-NAME}:VIRTUAL-HEIGHT      = SESSION:HEIGHT - 1
            {&WINDOW-NAME}:VIRTUAL-WIDTH       = SESSION:WIDTH  - 1
            {&WINDOW-NAME}:HEIGHT              = {&WINDOW-NAME}:VIRTUAL-HEIGHT
            {&WINDOW-NAME}:WIDTH               = {&WINDOW-NAME}:VIRTUAL-WIDTH
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT         = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH          = {&WINDOW-NAME}:WIDTH
            statusMessage:ROW                  = {&WINDOW-NAME}:HEIGHT - .86
            dCol                               = {&WINDOW-NAME}:WIDTH  - 8
            bt-Exit:COL                        = dCol - 1
            btnFirst:COL                       = dCol - 1
            btnPrevious:COL                    = dCol - 1
            btnNext:COL                        = dCol - 1
            btnLast:COL                        = dCol - 1
            btnExitText:COL                    = dCol - 9
            btnPrintText:COL                   = dCol - btnPrintText:WIDTH - 1
            bt-print-selected:COL              = dCol - 1
            bt-print-selected:ROW              = {&WINDOW-NAME}:HEIGHT - 1.24
            btnPrintText:ROW                   = {&WINDOW-NAME}:HEIGHT - .86
            bt-adjust-qty:ROW                  = {&WINDOW-NAME}:HEIGHT - 1.24
            btnAdjustQtyText:ROW               = {&WINDOW-NAME}:HEIGHT - .86
            btnDelete:ROW                      = {&WINDOW-NAME}:HEIGHT - 1.24
            btnDeleteText:ROW                  = {&WINDOW-NAME}:HEIGHT - .86
            BROWSE {&BROWSE-NAME}:HEIGHT       = {&WINDOW-NAME}:HEIGHT - BROWSE {&BROWSE-NAME}:ROW - btnDelete:HEIGHT
            BROWSE {&BROWSE-NAME}:WIDTH        = {&WINDOW-NAME}:WIDTH  - btnFirst:WIDTH - 3
            .
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pXprint W-Win 
PROCEDURE pXprint PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
        
    RUN pPrintView IN hdWipTagProcs (INPUT TABLE ttPrintInventoryStock,                          
                                     INPUT cPathTemplate
                                     ).
    RUN rebuildTempTable(
        INPUT cCompany,
        INPUT cFormattedJobno,
        INPUT cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-jobno2:SCREEN-VALUE  IN FRAME {&FRAME-NAME},
        INPUT cb-formno:SCREEN-VALUE  IN FRAME {&FRAME-NAME},
        INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        ).

    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME} IN FRAME  {&FRAME-NAME}.                                        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME      

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rebuildTempTable W-Win 
PROCEDURE rebuildTempTable :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobno   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcMachine AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobno2  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormno  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankno AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE iAll     AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCreated AS INTEGER NO-UNDO.
    DEFINE VARIABLE iOnHand  AS INTEGER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    RUN RebuildWIPBrowseTT IN hdInventoryProcs (
        ipcCompany,
        ipcJobno,
        ipcMachine,
        ipiJobno2,
        ipiFormno,
        ipiBlankno,
        OUTPUT iTotTags,
        OUTPUT iTotOnHand,
        INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
        ).
            
    RUN Inventory_CalculateTagCountInTTbrowse IN hdInventoryProcs (
        INPUT  "",
        OUTPUT iAll,
        INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
        ).

    RUN Inventory_CalculateTagCountInTTbrowse IN hdInventoryProcs (
        INPUT  gcStatusStockInitial,
        OUTPUT iCreated,
        INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
        ).
                
    RUN Inventory_CalculateTagCountInTTbrowse IN hdInventoryProcs (
        INPUT  gcStatusStockReceived,
        OUTPUT iOnHand,
        INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
        ).

    ASSIGN
        btAll:LABEL     = "All - " + STRING(iAll)
        btCreated:LABEL = "Cr - " + STRING(iCreated)
        btOH:LABEL      = "OH - " + STRING(iOnHand)
        .

    {&OPEN-BROWSERS-IN-QUERY-F-Main}    
        
    IF AVAILABLE ttBrowseInventory THEN DO:
        ASSIGN
            bt-print-selected:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
            .        
        APPLY "VALUE-CHANGED" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
    END.    
    ELSE
        ASSIGN
            bt-adjust-qty:SENSITIVE IN FRAME {&FRAME-NAME}     = FALSE
            bt-print-selected:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
            .
   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus W-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
    APPLY "ENTRY" TO ls-jobno IN FRAME {&FRAME-NAME}.    

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tagScan W-Win 
PROCEDURE tagScan :
/*------------------------------------------------------------------------------
      Purpose: Tag scan     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTag   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cJobNo      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMachine    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFormNo     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBlankNo    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lValidInv   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidJob   AS LOGICAL   NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF ls-tag:SCREEN-VALUE EQ "" THEN
        RETURN.
        
    ASSIGN 
        ls-jobno:SCREEN-VALUE = ""
        cFormattedJobno       = ""
        cMessage              = ""
        .
    
    RUN disableCreate.
    
    RUN pGetInventoryStockJobDetails IN hdInventoryProcs (
        cCompany,
        ipcTag,
        OUTPUT cJobNo,
        OUTPUT iJobNo2,
        OUTPUT iFormNo,
        OUTPUT iBlankNo,
        OUTPUT cMachine,
        OUTPUT lValidInv,
        OUTPUT cMessage
        ).
            
    IF lValidInv THEN 
    DO:
        ASSIGN
            ls-jobno:SCREEN-VALUE = cJobNo
            cFormattedJobno       = cJobNo
            .
        
        RUN updateComboBoxes.
        
        ASSIGN 
            cValidateJobno          = cJobNo
            cb-jobno2:SCREEN-VALUE  = STRING(iJobNo2,"999")
            .
            
        APPLY "VALUE-CHANGED" to cb-jobno2.
        
        ASSIGN
            cb-formno:SCREEN-VALUE  = STRING(iFormNo,"99")
            cb-blankno:SCREEN-VALUE = STRING(iBlankNo,"99")
            cb-machine:SCREEN-VALUE = cMachine
            NO-ERROR.
        
        RUN enableCreate.         
    END.
    ELSE DO:
        RUN pUpdateRMDetails (
            OUTPUT lValidInv
            ).
        
        IF lValidInv THEN DO:
            RUN ValidateJob IN hdJobProcs (
                INPUT cCompany,
                INPUT cFormattedJobno,
                INPUT cb-machine:SCREEN-VALUE,
                INPUT INTEGER(cb-jobno2:SCREEN-VALUE),
                INPUT INTEGER(cb-formno:SCREEN-VALUE),
                INPUT INTEGER(cb-blankno:SCREEN-VALUE),
                OUTPUT lValidJob
                ).
            
            IF lValidJob THEN            
                RUN enableCreate.
        END.
    END.
    
    IF NOT lValidInv THEN DO:
        RUN pStatusMessage ("INVALID TAB " + ipcTag + ", PLEASE ENTER A VALID TAG", 3).

        ASSIGN 
            cValidateJobno          = ""
            ls-tag:SCREEN-VALUE     = ""
            cb-jobno2:LIST-ITEMS    = ""
            cb-formno:LIST-ITEMS    = ""
            cb-blankno:LIST-ITEMS   = ""
            cb-machine:LIST-ITEMS   = ""
            cb-jobno2:SCREEN-VALUE  = ""
            cb-formno:SCREEN-VALUE  = ""
            cb-blankno:SCREEN-VALUE = ""
            cb-machine:SCREEN-VALUE = ""
            NO-ERROR.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateComboBoxes W-Win 
PROCEDURE updateComboBoxes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/    
    ASSIGN
        cJobno2ListItems  = ""
        cFormnoListItems  = ""
        cBlanknoListItems = ""
        cMachineListItems = "".
        
    RUN GetSecondaryJobForJob IN hdJobProcs (
        cCompany,
        cFormattedJobno,
        INPUT-OUTPUT cJobno2ListItems
        ).
    
    DO iCount = 1 TO NUM-ENTRIES(cJobno2ListItems):
        RUN GetFormnoForJob IN hdJobProcs (
            cCompany,
            cFormattedJobno,
            INTEGER(ENTRY(iCount, cJobno2ListItems)),
            INPUT-OUTPUT cFormnoListItems
            ).
    
        RUN GetBlanknoForJob IN hdJobProcs (
            cCompany,
            cFormattedJobno,
            INTEGER(ENTRY(iCount, cJobno2ListItems)),
            INPUT-OUTPUT cBlanknoListItems
            ).

    END.
    
    IF cJobno2ListItems EQ "" THEN
        ASSIGN 
            cJobno2ListItems                              = "000"
            cb-jobno2:LIST-ITEMS IN FRAME {&FRAME-NAME}   = cJobno2ListItems 
            cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "000".
    ELSE
        cb-jobno2:LIST-ITEMS IN FRAME {&FRAME-NAME} = cJobno2ListItems.
        
    APPLY "VALUE-CHANGED" to cb-jobno2.
    
    IF cFormnoListItems EQ "" THEN
        ASSIGN
            cFormnoListItems                              = "00"
            cb-formno:LIST-ITEMS IN FRAME {&FRAME-NAME}   = cFormnoListItems 
            cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00".
    ELSE
        cb-formno:LIST-ITEMS IN FRAME {&FRAME-NAME} = cFormnoListItems.

    IF cBlanknoListItems EQ "" THEN
        ASSIGN
            cBlanknoListItems                              = "00"
            cb-blankno:LIST-ITEMS IN FRAME {&FRAME-NAME}   = cBlanknoListItems
            cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00".
    ELSE
        cb-blankno:LIST-ITEMS IN FRAME {&FRAME-NAME} = cBlanknoListItems.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

