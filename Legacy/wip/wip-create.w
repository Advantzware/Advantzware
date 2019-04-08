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
DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcJobno    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcMachine  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiJobno2   AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipiFormno   AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipiBlankno  AS INTEGER   NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hdInventoryProcs  AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdOutputProcs     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdBrowseQuery     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdBrowseBuffer    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdNumericKeyBoard AS HANDLE    NO-UNDO.
DEFINE VARIABLE lCreated          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobno2ListItems  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormnoListItems  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBlanknoListItems AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMachineListItems AS CHARACTER NO-UNDO.
DEFINE VARIABLE cValidateJobno    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutputFileName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPathTemplate     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCopies           AS INTEGER   NO-UNDO.

DEFINE VARIABLE gcPathDataFileDefault AS CHARACTER INITIAL "C:\BA\LABEL".

&SCOPED-DEFINE sysCtrlName "BARDIR"

{system/sysconst.i}
{Inventory/ttInventory.i "NEW SHARED"}
{methods/defines/sortByDefs.i}
{wip/keyboardDefs.i}

DEFINE TEMP-TABLE ttBrowseInventory
    LIKE ttInventoryStockLoadtagWIP.

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
&Scoped-define FIELDS-IN-QUERY-br-table ttBrowseInventory.quantity ttBrowseInventory.quantityOriginal ttBrowseInventory.locationID ttBrowseInventory.stockIDAlias ttBrowseInventory.jobID ttBrowseInventory.inventoryStatus   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH ttBrowseInventory BY ttBrowseInventory.lastTransTime DESCENDING  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory BY ttBrowseInventory.lastTransTime DESCENDING  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br-table ttBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ttBrowseInventory


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-exit btnFirst btnKeyboard RECT-26 btnLast ~
btnNext ls-jobno cb-jobno2 btnNumPad cb-formno cb-blankno btnNumPad-1 ~
cb-machine btnNumPad-2 bt-create ls-qty-per-tag ls-total-run-qty ~
btnNumPad-3 ls-num-tags btnPrevious br-table 
&Scoped-Define DISPLAYED-OBJECTS ls-tag ls-jobno cb-jobno2 cb-formno ~
cb-blankno cb-machine ls-qty-per-tag ls-total-run-qty ls-num-tags 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-adjust-qty 
     LABEL "Adjust Quantity" 
     SIZE 54 BY 3
     FONT 37.

DEFINE BUTTON bt-create 
     LABEL "Create" 
     SIZE 27 BY 3
     FONT 37.

DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29.

DEFINE BUTTON bt-print-all 
     LABEL "Print and Receive All" 
     SIZE 54 BY 3
     FONT 37.

DEFINE BUTTON bt-print-selected 
     LABEL "Print and Receive Selected" 
     SIZE 54 BY 3
     FONT 37.

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_cross_disabled.ico":U NO-FOCUS
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Delete".

DEFINE BUTTON btnFirst 
     IMAGE-UP FILE "Graphics/32x32/navigate_up2.ico":U NO-FOCUS
     LABEL "First" 
     SIZE 9.6 BY 2.29 TOOLTIP "First".

DEFINE BUTTON btnKeyboard 
     IMAGE-UP FILE "Graphics/24x24/keyboard.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Keyboard" 
     SIZE 6.4 BY 1.52 TOOLTIP "Keyboard".

DEFINE BUTTON btnLast 
     IMAGE-UP FILE "Graphics/32x32/navigate_down2.ico":U NO-FOCUS
     LABEL "Last" 
     SIZE 9.6 BY 2.29 TOOLTIP "Last".

DEFINE BUTTON btnNext 
     IMAGE-UP FILE "Graphics/32x32/navigate_down.ico":U NO-FOCUS
     LABEL "Next" 
     SIZE 9.6 BY 2.29 TOOLTIP "Next".

DEFINE BUTTON btnNumPad 
     IMAGE-UP FILE "Graphics/32x32/numeric_keypad.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad" 
     SIZE 8 BY 1.91 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btnNumPad-1 
     IMAGE-UP FILE "Graphics/24x24/numeric_keypad.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad1" 
     SIZE 6.4 BY 1.52 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btnNumPad-2 
     IMAGE-UP FILE "Graphics/24x24/numeric_keypad.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad1" 
     SIZE 6.4 BY 1.52 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btnNumPad-3 
     IMAGE-UP FILE "Graphics/24x24/numeric_keypad.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "NumPad1" 
     SIZE 6.4 BY 1.52 TOOLTIP "Numeric Keypad".

DEFINE BUTTON btnPrevious 
     IMAGE-UP FILE "Graphics/32x32/navigate_up.ico":U NO-FOCUS
     LABEL "Previous" 
     SIZE 9.6 BY 2.29 TOOLTIP "Previous".

DEFINE VARIABLE cb-blankno AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 9.8 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cb-formno AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 9.8 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cb-jobno2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00" 
     DROP-DOWN-LIST
     SIZE 9.8 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cb-machine AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 37.4 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE ls-jobno AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE ls-num-tags AS INTEGER FORMAT ">9":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE ls-qty-per-tag AS DECIMAL FORMAT ">,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 29.6 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE ls-tag AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 63.4 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE ls-total-run-qty AS DECIMAL FORMAT ">,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 29.6 BY 1.38
     FONT 37 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 202 BY 32.86.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 199 BY .1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      ttBrowseInventory SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table W-Win _FREEFORM
  QUERY br-table DISPLAY
      ttBrowseInventory.quantity WIDTH 25 COLUMN-LABEL "Qty On-Hand"
    ttBrowseInventory.quantityOriginal WIDTH 25 COLUMN-LABEL "Qty Original"
    ttBrowseInventory.locationID WIDTH 25 COLUMN-LABEL "Location" FORMAT "X(12)"
    ttBrowseInventory.stockIDAlias WIDTH 50 COLUMN-LABEL "Tag #" FORMAT "X(30)"
    ttBrowseInventory.jobID WIDTH 20 COLUMN-LABEL "Job #" FORMAT "X(20)"
    ttBrowseInventory.inventoryStatus COLUMN-LABEL "Status" FORMAT "X(15)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 189 BY 19.29
         FONT 36 ROW-HEIGHT-CHARS .95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     bt-exit AT ROW 1.24 COL 192 WIDGET-ID 84
     btnDelete AT ROW 18.86 COL 192 WIDGET-ID 116
     btnFirst AT ROW 10.52 COL 191.8 WIDGET-ID 44
     btnKeyboard AT ROW 3.62 COL 15 WIDGET-ID 132
     btnLast AT ROW 27.52 COL 191.8 WIDGET-ID 46
     btnNext AT ROW 23.14 COL 192 WIDGET-ID 42
     ls-tag AT ROW 1.71 COL 33 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     ls-jobno AT ROW 3.71 COL 33 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cb-jobno2 AT ROW 3.71 COL 73.8 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     btnNumPad AT ROW 2.43 COL 163 WIDGET-ID 120
     cb-formno AT ROW 3.71 COL 100.8 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     cb-blankno AT ROW 3.71 COL 128 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     btnNumPad-1 AT ROW 8.14 COL 64 WIDGET-ID 124
     cb-machine AT ROW 6.24 COL 32.6 COLON-ALIGNED NO-LABEL WIDGET-ID 94
     btnNumPad-2 AT ROW 8.14 COL 127 WIDGET-ID 126
     bt-create AT ROW 6.67 COL 174.2 WIDGET-ID 108
     ls-qty-per-tag AT ROW 8.14 COL 32.4 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     ls-total-run-qty AT ROW 8.14 COL 95.4 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     btnNumPad-3 AT ROW 8.14 COL 166 WIDGET-ID 128
     ls-num-tags AT ROW 8.14 COL 145.8 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     btnPrevious AT ROW 14.57 COL 192 WIDGET-ID 40
     br-table AT ROW 10.52 COL 2 WIDGET-ID 200
     bt-adjust-qty AT ROW 30.52 COL 2 WIDGET-ID 110
     bt-print-selected AT ROW 30.52 COL 70 WIDGET-ID 114
     bt-print-all AT ROW 30.52 COL 137 WIDGET-ID 112
     "Quantity Per Tag:" VIEW-AS TEXT
          SIZE 29.6 BY 1.33 AT ROW 8.24 COL 4 WIDGET-ID 96
          FONT 36
     "# Tags:" VIEW-AS TEXT
          SIZE 13.8 BY 1.33 AT ROW 8.14 COL 133 WIDGET-ID 104
          FONT 36
     "RM or WIP Tag:" VIEW-AS TEXT
          SIZE 27 BY 1.33 AT ROW 1.71 COL 7 WIDGET-ID 86
          FONT 36
     "Job #:" VIEW-AS TEXT
          SIZE 11 BY 1.33 AT ROW 3.62 COL 23 WIDGET-ID 12
          FONT 36
     "Blank #:" VIEW-AS TEXT
          SIZE 14 BY 1.33 AT ROW 3.81 COL 115.2 WIDGET-ID 58
          FONT 36
     "Machine/Op:" VIEW-AS TEXT
          SIZE 21.6 BY 1.33 AT ROW 6.38 COL 12 WIDGET-ID 92
          FONT 36
     "Total Run Qty:" VIEW-AS TEXT
          SIZE 24 BY 1.33 AT ROW 8.24 COL 72 WIDGET-ID 100
          FONT 36
     "(optional)" VIEW-AS TEXT
          SIZE 16.8 BY .95 AT ROW 1.91 COL 101.2 WIDGET-ID 90
          FONT 36
     "Form #:" VIEW-AS TEXT
          SIZE 13.8 BY 1.33 AT ROW 3.86 COL 88 WIDGET-ID 48
          FONT 36
     RECT-26 AT ROW 5.62 COL 2.2 WIDGET-ID 18
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 118
     RECT-2 AT ROW 2.19 COL 162 WIDGET-ID 130
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202.2 BY 32.91
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


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
         HEIGHT             = 32.86
         WIDTH              = 202
         MAX-HEIGHT         = 32.91
         MAX-WIDTH          = 202.2
         VIRTUAL-HEIGHT     = 32.91
         VIRTUAL-WIDTH      = 202.2
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
/* BROWSE-TAB br-table btnPrevious F-Main */
ASSIGN 
       br-table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR BUTTON bt-adjust-qty IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-print-all IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-print-selected IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnDelete IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btnKeyboard:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnNumPad-1:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnNumPad-2:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnNumPad-3:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN ls-tag IN FRAME F-Main
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
BY ttBrowseInventory.lastTransTime DESCENDING
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
    IF VALID-HANDLE(hdInventoryProcs) THEN
        DELETE OBJECT hdInventoryProcs.

    IF VALID-HANDLE(hdOutputProcs) THEN
        DELETE OBJECT hdOutputProcs.
    
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
        bt-print-selected:LABEL = "Print and Receive Selected"
        btnDelete:SENSITIVE = AVAILABLE ttBrowseInventory AND
                              DYNAMIC-FUNCTION("fCanDeleteInventoryStock" IN hdInventoryProcs, ttBrowseInventory.inventoryStockID)
                              .
    IF {&BROWSE-NAME}:NUM-SELECTED-ROWS EQ 1 THEN DO:
        {&BROWSE-NAME}:FETCH-SELECTED-ROW(1).        
        IF hdBrowseBuffer:AVAILABLE THEN DO:
            FIND FIRST inventoryTransaction NO-LOCK
                 WHERE inventoryTransaction.inventoryStockID EQ hdBrowseBuffer:BUFFER-FIELD("inventoryStockID"):BUFFER-VALUE
                 NO-ERROR.
            IF AVAILABLE inventoryTransaction THEN DO:
                /*
                IF inventoryTransaction.transactionStatus EQ gcStatusTransactionInitial THEN
                bt-adjust-qty:SENSITIVE = TRUE.
                ELSE
                */
                IF inventoryTransaction.transactionStatus EQ gcStatusTransactionPosted THEN
                bt-print-selected:LABEL = "Re-Print Selected".
            END.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-create
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-create W-Win
ON CHOOSE OF bt-create IN FRAME F-Main /* Create */
DO: 
    IF DECIMAL(ls-qty-per-tag:SCREEN-VALUE) EQ 0 THEN DO:
        MESSAGE "Quantity Per Tag cannot be 0.00"
            VIEW-AS ALERT-BOX ERROR.               
        APPLY "ENTRY" TO ls-qty-per-tag.
        RETURN.    
    END.
    
    IF DECIMAL(ls-total-run-qty:SCREEN-VALUE) LT DECIMAL(ls-qty-per-tag:SCREEN-VALUE) THEN DO:
        MESSAGE "Total Run Qty " + STRING(ls-total-run-qty:SCREEN-VALUE) +
            " cannot be less than Quantity Per tag " + STRING(ls-qty-per-tag:SCREEN-VALUE)
            VIEW-AS ALERT-BOX ERROR.               
        APPLY "ENTRY" TO ls-total-run-qty.
        RETURN.
    END.

    FIND FIRST job-mch NO-LOCK
         WHERE job-mch.company  EQ ipcCompany
           AND job-mch.job-no   EQ ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME}
           AND job-mch.job-no2  EQ INT(cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME})
           AND job-mch.m-code   EQ cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME}
           AND job-mch.frm      EQ INT(cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME})
           AND job-mch.blank-no EQ INT(cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME})
         NO-ERROR.
    IF NOT AVAILABLE job-mch THEN
        RETURN.

    FIND FIRST job-mat NO-LOCK  
         WHERE job-mat.company  EQ job-mch.company
           AND job-mat.job-no   EQ job-mch.job-no
           AND job-mat.job-no2 EQ job-mch.job-no2
           AND job-mat.frm     EQ job-mch.frm
         NO-ERROR.  
    IF NOT AVAILABLE job-mat THEN
        RETURN.
  
    RUN CreatePreLoadtagsFromInputsWIP IN hdInventoryProcs (
        ROWID(job-mch),
        ROWID(job-mat), 
        DECIMAL(ls-total-run-qty:SCREEN-VALUE),
        DECIMAL(ls-qty-per-tag:SCREEN-VALUE),
        1,
        "EA",
        OUTPUT lCreated,
        OUTPUT cMessage
        ).

    RUN CreateInventoryLoadtagsFromPreLoadtags IN hdInventoryProcs.

    FOR EACH ttInventoryStockLoadtag:
        ASSIGN 
            lCreated = NO
            cMessage = "". 
        RUN CreateInventoryStockFromLoadtag IN hdInventoryProcs (
            ttInventoryStockLoadtag.inventoryStockID,
            YES,
            NO,
            OUTPUT lCreated,
            OUTPUT cMessage
            ).
    END.   

    RUN rebuildTempTable(
        ipcCompany,
        ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},                         
        cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}).               
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit W-Win
ON CHOOSE OF bt-exit IN FRAME F-Main
DO:
        IF VALID-HANDLE(hKeyboard) THEN
        DELETE OBJECT hKeyboard.
        APPLY "CLOSE":U TO THIS-PROCEDURE.    
        RETURN.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-print-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-print-all W-Win
ON CHOOSE OF bt-print-all IN FRAME F-Main /* Print and Receive All */
DO: 
        EMPTY TEMP-TABLE ttPrintInventoryStock.
    
        FOR EACH ttBrowseInventory
            WHERE ttBrowseInventory.inventoryStatus = gcStatusStockInitial:
            RUN PostReceivedInventory IN hdInventoryProcs (INPUT ipcCompany,
                INPUT ttBrowseInventory.inventoryStockID).

            RUN CreatePrintInventory IN hdInventoryProcs (INPUT ttBrowseInventory.inventoryStockID).
        END.
    
        RUN pPrintLabels.
        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-print-selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-print-selected W-Win
ON CHOOSE OF bt-print-selected IN FRAME F-Main /* Print and Receive Selected */
DO:
    DEFINE VARIABLE cInventoryStatus  LIKE inventoryStock.inventoryStatus NO-UNDO.
    DEFINE VARIABLE cInventoryStockID LIKE inventoryTransaction.inventoryStockID NO-UNDO.

    EMPTY TEMP-TABLE ttPrintInventoryStock.

    IF hdBrowseBuffer:AVAILABLE THEN DO:
        ASSIGN 
            cInventoryStatus  = hdBrowseBuffer:BUFFER-FIELD("inventoryStatus"):BUFFER-VALUE
            cInventoryStockID = hdBrowseBuffer:BUFFER-FIELD("inventoryStockID"):BUFFER-VALUE.
        
        IF cInventoryStatus = gcStatusStockInitial THEN
            RUN PostReceivedInventory IN hdInventoryProcs (INPUT ipcCompany,
                INPUT cInventoryStockID).
    
        RUN CreatePrintInventory in hdInventoryProcs (INPUT cInventoryStockID).
    
        RUN pPrintLabels.
        
    END.    
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


&Scoped-define SELF-NAME btnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst W-Win
ON CHOOSE OF btnFirst IN FRAME F-Main /* First */
DO:
    RUN pNavigate (SELF).
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
ON CHOOSE OF btnPrevious IN FRAME F-Main /* Previous */
DO:
    RUN pNavigate (SELF).
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
    DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.

    RUN system/openlookup.p (
        ipcCompany,
        "job-no",
        3,
        "",
        0,
        OUTPUT cFieldsValue,
        OUTPUT cFoundValue,
        OUTPUT recFoundRecID
        ).
    IF recFoundRecID NE ? THEN DO:
        SELF:SCREEN-VALUE = cFoundValue.
        APPLY "LEAVE":U TO SELF.
        idx = LOOKUP("job-no2",cFieldsValue,"|").
        IF idx NE 0 THEN
        cb-jobno2:SCREEN-VALUE  = ENTRY(idx + 1,cFieldsValue,"|") NO-ERROR.
        idx = LOOKUP("frm",cFieldsValue,"|").
        IF idx NE 0 THEN
        cb-formno:SCREEN-VALUE  = ENTRY(idx + 1,cFieldsValue,"|") NO-ERROR.
        idx = LOOKUP("blank-no",cFieldsValue,"|").
        IF idx NE 0 THEN
        cb-blankno:SCREEN-VALUE = ENTRY(idx + 1,cFieldsValue,"|") NO-ERROR.
    END. /* if ne ? */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-jobno W-Win
ON LEAVE OF ls-jobno IN FRAME F-Main
DO:
    DEFINE VARIABLE riJobMch AS ROWID     NO-UNDO.
    DEFINE VARIABLE cJobNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo2  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormNo  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBlankNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lParse   AS LOGICAL   NO-UNDO.
 
    IF VALID-HANDLE(hKeyboard) THEN
    DELETE OBJECT hKeyboard.
    
    IF cValidateJobno = ls-jobno:SCREEN-VALUE THEN
        RETURN.
    
    RUN disableCreate.

    ASSIGN 
        cJobno2ListItems  = ""
        cFormnoListItems  = ""
        cBlanknoListitems = ""
        cMachineListItems = ""
        .
    IF ls-jobno:SCREEN-VALUE = "" THEN
        RETURN.
    
    IF INDEX(SELF:SCREEN-VALUE,"-") NE 0 THEN DO:
        ASSIGN
            cJobNo   = ENTRY(1,SELF:SCREEN-VALUE,"-")
            cJobNo   = FILL(" ",6 - LENGTH(cJobNo))+ cJobNo
            cJobNo2  = ENTRY(2,SELF:SCREEN-VALUE,"-")
            cJobNo2  = ENTRY(1,cJobNo2,".")
            cFormNo  = ENTRY(2,SELF:SCREEN-VALUE,".")
            cBlankNo = ENTRY(3,SELF:SCREEN-VALUE,".")
            SELF:SCREEN-VALUE = cJobNo
            lParse   = CAN-FIND(FIRST job-mch
                                WHERE job-mch.company  EQ ipcCompany
                                  AND job-mch.job-no   EQ cJobNo
                                  AND job-mch.job-no2  EQ INT(cJobNo2)
                                  AND job-mch.frm      EQ INT(cFormNo)
                                  AND job-mch.blank-no EQ INT(cBlankNo))
                                  .
        IF NOT lParse THEN DO:
            MESSAGE
                "Invalid Job Number"
            VIEW-AS ALERT-BOX ERROR.
            SELF:SCREEN-VALUE = "".
            APPLY "ENTRY":U TO SELF.
            RETURN NO-APPLY.
        END. /* if not lparse */
    END. /* if index */
    
    RUN updateComboBoxes.

    ASSIGN 
        cb-jobno2:SCREEN-VALUE  = ENTRY(1,cJobno2ListItems)
        cb-formno:SCREEN-VALUE  = ENTRY(1,cFormnoListItems)
        cb-blankno:SCREEN-VALUE = ENTRY(1,cBlanknoListItems)
        cb-machine:SCREEN-VALUE = ENTRY(1,cMachineListItems)
        .           
    RUN getJobDetails(INPUT ipcCompany,
        INPUT ls-jobno:SCREEN-VALUE,
        INPUT cb-machine:SCREEN-VALUE,
        INPUT cb-jobno2:SCREEN-VALUE,
        INPUT cb-formno:SCREEN-VALUE,
        INPUT cb-blankno:SCREEN-VALUE,
        OUTPUT riJobMch).
        
    FIND FIRST job-mch WHERE ROWID(job-mch) = riJobMch NO-ERROR.
    IF AVAILABLE job-mch THEN DO:
        ASSIGN 
            cb-jobno2:SCREEN-VALUE  = STRING(job-mch.job-no2)
            cb-formno:SCREEN-VALUE  = STRING(job-mch.frm)
            cb-blankno:SCREEN-VALUE = STRING(job-mch.blank-no)
            cb-machine:SCREEN-VALUE = STRING(job-mch.m-code).                                                                       
    
        RUN enableCreate.               
    END.

    ASSIGN
        cValidateJobno = ls-jobno:SCREEN-VALUE.
               
    RUN rebuildTempTable(INPUT ipcCompany,
        INPUT ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},                         
        INPUT cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
    
    IF lParse THEN
    ASSIGN
        cb-jobno2:SCREEN-VALUE  = cJobNo2
        cb-formno:SCREEN-VALUE  = cFormNo
        cb-blankno:SCREEN-VALUE = cBlankNo
        .
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
    
    IF DECIMAL(ls-total-run-qty:SCREEN-VALUE) = 0 THEN
        ASSIGN 
            ls-total-run-qty:SCREEN-VALUE = ls-qty-per-tag:SCREEN-VALUE.
    
    IF DECIMAL(ls-qty-per-tag:SCREEN-VALUE) <> 0 AND
        DECIMAL(ls-total-run-qty:SCREEN-VALUE) <> 0 AND
        DECIMAL(ls-qty-per-tag:SCREEN-VALUE) <= DECIMAL(ls-total-run-qty:SCREEN-VALUE) THEN 
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
ON ENTRY OF ls-tag IN FRAME F-Main
DO: 
    /*     IF NOT VALID-HANDLE(hdNumericKeyBoard) THEN */
    /*         RUN touch\numeric.w PERSISTENT SET hdNumericKeyBoard (THIS-PROCEDURE:HANDLE,THIS-PROCEDURE). */
    /*    */
    /*     field_value = ls-tag:SCREEN-VALUE. */
    /*     h_field = ls-tag:HANDLE. */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-tag W-Win
ON LEAVE OF ls-tag IN FRAME F-Main
DO:
        RUN tagScan(SELF:SCREEN-VALUE).  
    
        RUN rebuildTempTable(INPUT ipcCompany,
            INPUT ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
            INPUT cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},                         
            INPUT cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
            INPUT cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
            INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}).                   
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
    
    IF DECIMAL(ls-qty-per-tag:SCREEN-VALUE) <> 0 AND
        DECIMAL(ls-total-run-qty:SCREEN-VALUE) <> 0 AND
        DECIMAL(ls-qty-per-tag:SCREEN-VALUE) <= DECIMAL(ls-total-run-qty:SCREEN-VALUE) THEN 
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

{wip/pNavigate.i}
{wip/pKeyboard.i}
{methods/sortByProc.i "pByQuantity" "ttBrowseInventory.quantity"}
{methods/sortByProc.i "pByQuantityOriginal" "ttBrowseInventory.quantityOriginal"}
{methods/sortByProc.i "pByLocationID" "ttBrowseInventory.locationID"}
{methods/sortByProc.i "pByStockIDAlias" "ttBrowseInventory.stockIDAlias"}
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
        ls-qty-per-tag:BGCOLOR   = ?
        ls-total-run-qty:BGCOLOR = ?
        ls-num-tags:BGCOLOR      = ?
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
  DISPLAY ls-tag ls-jobno cb-jobno2 cb-formno cb-blankno cb-machine 
          ls-qty-per-tag ls-total-run-qty ls-num-tags 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE bt-exit btnFirst btnKeyboard RECT-26 btnLast btnNext ls-jobno 
         cb-jobno2 btnNumPad cb-formno cb-blankno btnNumPad-1 cb-machine 
         btnNumPad-2 bt-create ls-qty-per-tag ls-total-run-qty btnNumPad-3 
         ls-num-tags btnPrevious br-table 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getJobDetails W-Win 
PROCEDURE getJobDetails :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMachine AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormno  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankno AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opriJobMch AS ROWID     NO-UNDO.
    
    DEFINE BUFFER bf-job-mch FOR job-mch.
    
    FIND FIRST bf-job-mch NO-LOCK
        WHERE bf-job-mch.company  EQ ipcCompany
          AND bf-job-mch.job-no   EQ ipcJobno
          AND bf-job-mch.m-code   EQ ipcMachine
          AND bf-job-mch.job-no2  EQ ipiJobno2
          AND bf-job-mch.frm      EQ ipiFormno
          AND bf-job-mch.blank-no EQ ipiBlankno
        NO-ERROR.
    IF AVAILABLE bf-job-mch THEN
        ASSIGN 
            opriJobMch = ROWID(bf-job-mch).
        
    RELEASE bf-job-mch.

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

    FIND FIRST company NO-LOCK 
         WHERE company.company EQ ipcCompany
         NO-ERROR .
    IF AVAILABLE company THEN
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                         + " - {&awversion}" + " - " 
                         + STRING(company.name) + " - " + ipcLocation.

    ASSIGN 
        hdBrowseQuery  = {&BROWSE-NAME}:QUERY IN FRAME {&FRAME-NAME}
        hdBrowseBuffer = hdBrowseQuery:GET-BUFFER-HANDLE(1)
        .
    RUN pGetSettings(ipcCompany, OUTPUT cOutputFileName, OUTPUT cPathTemplate, OUTPUT iCopies).
    
    APPLY "ENTRY" TO ls-jobno IN FRAME {&FRAME-NAME}.

    RUN disableCreate.

    IF ipcJobNo NE "" THEN 
        RUN jobScan(INPUT ipcCompany,
            INPUT ipcJobno,
            INPUT ipcMachine,
            INPUT ipiJobno2,
            INPUT ipiFormno,
            INPUT ipiBlankno).

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

    DEFINE VARIABLE riJobMch AS ROWID NO-UNDO.

    RUN getJobDetails(INPUT ipcCompany,
        INPUT ipcJobno,
        INPUT ipcMachine,
        INPUT ipiJobno2,
        INPUT ipiFormno,
        INPUT ipiBlankno,
        OUTPUT riJobMch).
                      
    FIND FIRST job-mch 
        WHERE ROWID(job-mch) = riJobMch NO-ERROR.
    
    IF NOT AVAILABLE job-mch THEN 
    DO: 
        MESSAGE "Invalid Job scan" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO ls-jobno in FRAME {&FRAME-NAME}.
        RETURN ERROR.
    END.    

    ASSIGN
        ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ipcJobno.
    
    RUN updateComboBoxes.
       
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            cb-jobno2:LIST-ITEMS    = cJobno2ListItems
            cb-formno:LIST-ITEMS    = cFormnoListItems
            cb-blankno:LIST-ITEMS   = cBlanknoListItems
            cb-machine:LIST-ITEMS   = cMachineListItems
            cb-jobno2:SCREEN-VALUE  = STRING(ipiJobno2,"99")
            cb-formno:SCREEN-VALUE  = STRING(ipiFormno,"99")
            cb-blankno:SCREEN-VALUE = STRING(ipiBlankno,"99")
            cb-machine:SCREEN-VALUE = ipcmachine.
    END.
    
    ASSIGN 
        cValidateJobno = ls-jobno:SCREEN-VALUE.
                       
    RUN enableCreate.
    
    RUN rebuildTempTable(INPUT ipcCompany,
        INPUT ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},                         
        INPUT cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}).                   
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
    DEFINE VARIABLE riJobMch AS ROWID NO-UNDO.
    
    RUN disableCreate.
    
    RUN getJobDetails(INPUT ipcCompany,
        INPUT ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        OUTPUT riJobMch).
    
    FIND FIRST job-mch 
        WHERE ROWID(job-mch) = riJobMch NO-ERROR.
              
    IF AVAILABLE job-mch THEN
        RUN enableCreate.

    RUN rebuildTempTable(INPUT ipcCompany,
        INPUT ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},                        
        INPUT cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
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
    IF AVAILABLE ttBrowseInventory THEN DO:
        MESSAGE
            "Delete Selection?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE lDelete AS LOGICAL.
        IF lDelete THEN DO:
            RUN DeleteInventoryStock IN hdInventoryProcs (ttBrowseInventory.inventoryStockID).
            BROWSE {&BROWSE-NAME}:DELETE-CURRENT-ROW().
        END. /* if ldelete */
        
    END. /* if avail */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintLabels W-Win 
PROCEDURE pPrintLabels PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    IF SEARCH(cPathTemplate) NE ? THEN 
    DO:
        RUN PrintLabelMatrixFile IN hdOutputProcs(ipcCompany, cPathTemplate, "WIPTAG").
        MESSAGE "Label Matrix print process has been started." SKIP
            "This process will generate printed labels for your selected WIP Tags" SKIP(2)
            "     Data file: " cOutputFileName SKIP 
            "     Template file: " cPathTemplate
            VIEW-AS ALERT-BOX TITLE "Print Confirmation".
    END.
    ELSE  
        MESSAGE "Data file has been generated but system is unable to locate template file to print." SKIP (2)
            "     Data file: " cOutputFileName SKIP
            "     Template file: " cPathTemplate
            VIEW-AS ALERT-BOX TITLE "Invalid Template".
    
    RUN TempTableToCSV IN hdOutputProcs( INPUT TEMP-TABLE ttPrintInventoryStock:HANDLE,
        INPUT cOutputFileName,
        INPUT TRUE).
    
    
    RUN rebuildTempTable(INPUT ipcCompany,
        INPUT ls-jobno:SCREEN-VALUE   IN FRAME {&FRAME-NAME},
        INPUT cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT cb-jobno2:SCREEN-VALUE  IN FRAME {&FRAME-NAME},
        INPUT cb-formno:SCREEN-VALUE  IN FRAME {&FRAME-NAME},
        INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

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

    EMPTY TEMP-TABLE ttBrowseInventory.
    
    FOR EACH inventoryStock NO-LOCK
        WHERE inventoryStock.company   EQ ipcCompany
          AND inventoryStock.jobID     EQ ipcJobno
          AND inventoryStock.jobID2    EQ ipiJobno2   
          AND inventoryStock.formNo    EQ ipiFormno   
          AND inventoryStock.blankNo   EQ ipiBlankno
          AND inventoryStock.machineID EQ ipcMachine
        :
        CREATE ttBrowseInventory.
        BUFFER-COPY inventoryStock EXCEPT inventoryStock.locationID TO ttBrowseInventory.
        ttBrowseinventory.locationID = inventoryStock.warehouseID + " " + inventoryStock.locationID.
    END.
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    
    hdBrowseQuery:GET-FIRST().
    IF hdBrowseBuffer:AVAILABLE THEN DO:
        ASSIGN
            bt-print-all:SENSITIVE IN FRAME {&FRAME-NAME}      = TRUE
            bt-print-selected:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
            .        
        APPLY "VALUE-CHANGED" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
    END.    
    ELSE
        ASSIGN
            bt-print-all:SENSITIVE IN FRAME {&FRAME-NAME}      = FALSE
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
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTag   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-inventoryStock FOR inventoryStock.
    
    IF ls-tag:SCREEN-VALUE in FRAME {&FRAME-NAME} = "" THEN
        RETURN.
        
    ASSIGN 
        ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    
    RUN disableCreate.
    
    FIND FIRST bf-inventoryStock NO-LOCK
        WHERE bf-inventoryStock.StockIDAlias = ipcTag NO-ERROR.

    IF AVAILABLE bf-inventoryStock THEN 
    DO:
        ASSIGN
            ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = bf-inventoryStock.jobID.
                          
        RUN updateComboBoxes.
                
        ASSIGN 
            cValidateJobno                                 = bf-inventoryStock.jobID
            cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(bf-inventoryStock.jobID2,"99")
            cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(bf-inventoryStock.formNo,"99")
            cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(bf-inventoryStock.blankNo,"99")
            cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME} = bf-inventoryStock.machineID
            ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = bf-inventoryStock.jobID.
        
        RUN enableCreate.         
    END.
        
    RELEASE bf-inventoryStock.
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
    DEFINE BUFFER buf-job-mch FOR job-mch.
    
    FOR EACH buf-job-mch NO-LOCK
        WHERE buf-job-mch.company = ipcCompany
          AND buf-job-mch.job-no  = ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        BY buf-job-mch.line
        :
        cJobno2ListItems  = IF cJobno2ListItems = "" THEN STRING(buf-job-mch.job-no2,"99")
        ELSE IF INDEX(cJobno2Listitems,STRING(buf-job-mch.job-no2,"99")) > 0 THEN cJobno2ListItems
        ELSE cJobno2ListItems + "," + STRING(buf-job-mch.job-no2,"99").
        cFormnoListItems  = IF cFormnoListItems = "" THEN STRING(buf-job-mch.frm,"99")
        ELSE IF INDEX(cFormnoListitems,STRING(buf-job-mch.frm,"99")) > 0 THEN cFormnoListItems
        ELSE cFormnoListItems + "," + STRING(buf-job-mch.frm,"99").
        cBlanknoListItems = IF cBlanknoListItems = "" THEN STRING(buf-job-mch.blank-no,"99")
        ELSE IF INDEX(cBlanknoListitems,STRING(buf-job-mch.blank-no,"99")) > 0 THEN cBlanknoListItems
        ELSE cBlanknoListItems + "," + STRING(buf-job-mch.blank-no,"99").
        cMachineListItems = IF cMachineListItems = "" THEN STRING(buf-job-mch.m-code)
        ELSE IF INDEX(cMachineListItems,STRING(buf-job-mch.m-code)) > 0 THEN cMachineListItems
        ELSE cMachineListItems + "," + STRING(buf-job-mch.m-code).           
    END.

    IF cJobno2ListItems = "" THEN
        ASSIGN 
            cJobno2ListItems                              = "00"
            cb-jobno2:LIST-ITEMS IN FRAME {&FRAME-NAME}   = cJobno2ListItems 
            cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00".
    ELSE
        cb-jobno2:LIST-ITEMS IN FRAME {&FRAME-NAME} = cJobno2ListItems.
 
    IF cFormnoListItems = "" THEN
        ASSIGN
            cFormnoListItems                              = "00"
            cb-formno:LIST-ITEMS IN FRAME {&FRAME-NAME}   = cFormnoListItems 
            cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00".
    ELSE
        cb-formno:LIST-ITEMS IN FRAME {&FRAME-NAME} = cFormnoListItems.

    IF cBlanknoListItems = "" THEN
        ASSIGN
            cBlanknoListItems                              = "00"
            cb-blankno:LIST-ITEMS IN FRAME {&FRAME-NAME}   = cBlanknoListItems
            cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00".
    ELSE
        cb-blankno:LIST-ITEMS IN FRAME {&FRAME-NAME} = cBlanknoListItems.

    IF cMachineListItems = "" THEN
        ASSIGN
            cMachineListItems                              = ""
            cb-machine:LIST-ITEMS IN FRAME {&FRAME-NAME}   = cMachineListItems
            cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    ELSE
        cb-machine:LIST-ITEMS IN FRAME {&FRAME-NAME} = cMachineListItems.
    
    RELEASE buf-job-mch.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

