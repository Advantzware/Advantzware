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
DEFINE VARIABLE hdInventoryProcs    AS         HANDLE    NO-UNDO.
DEFINE VARIABLE hdOutputProcs       AS         HANDLE    NO-UNDO.
DEFINE VARIABLE hdBrowseQuery       AS         HANDLE    NO-UNDO.
DEFINE VARIABLE hdBrowseBuffer      AS         HANDLE    NO-UNDO.
DEFINE VARIABLE hdNumericKeyBoard   AS         HANDLE    NO-UNDO.
DEFINE VARIABLE lCreated            AS         LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage            AS         CHARACTER NO-UNDO.
DEFINE VARIABLE cJobno2ListItems    AS         CHARACTER NO-UNDO.
DEFINE VARIABLE cFormnoListItems    AS         CHARACTER NO-UNDO.
DEFINE VARIABLE cBlanknoListItems   AS         CHARACTER NO-UNDO.
DEFINE VARIABLE cMachineListItems   AS         CHARACTER NO-UNDO.
DEFINE VARIABLE cValidateJobno      AS         CHARACTER NO-UNDO.
DEFINE VARIABLE cOutputFileName     AS         CHARACTER NO-UNDO.

&SCOPED-DEFINE sysCtrlName "BARDIR"

{Inventory/ttInventory.i "NEW SHARED"}
            
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
&Scoped-define QUERY-STRING-br-table FOR EACH ttBrowseInventory BY ttBrowseInventory.lastTransTime DESCENDING
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory BY ttBrowseInventory.lastTransTime DESCENDING.
&Scoped-define TABLES-IN-QUERY-br-table ttBrowseInventory
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ttBrowseInventory


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-exit RECT-26 ls-tag ls-jobno cb-jobno2 ~
cb-formno cb-blankno cb-machine bt-create ls-qty-per-tag ls-total-run-qty ~
ls-num-tags br-table bt-first bt-up bt-down bt-last 
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
     SIZE 50 BY 3
     FONT 37.

DEFINE BUTTON bt-create 
     LABEL "Create" 
     SIZE 27 BY 3
     FONT 37.

DEFINE BUTTON bt-down 
     IMAGE-UP FILE "Graphics/32x32/navigate_down.ico":U
     LABEL "" 
     SIZE 9.6 BY 2.29.

DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 9.6 BY 2.29.

DEFINE BUTTON bt-first 
     IMAGE-UP FILE "Graphics/32x32/navigate_up2.ico":U
     LABEL "" 
     SIZE 9.6 BY 2.29.

DEFINE BUTTON bt-last 
     IMAGE-UP FILE "Graphics/32x32/navigate_down2.ico":U
     LABEL "" 
     SIZE 9.6 BY 2.29.

DEFINE BUTTON bt-print-all 
     LABEL "Print and Receive All" 
     SIZE 50 BY 3
     FONT 37.

DEFINE BUTTON bt-print-selected 
     LABEL "Print and Receive Selected" 
     SIZE 50 BY 3
     FONT 37.

DEFINE BUTTON bt-up 
     IMAGE-UP FILE "Graphics/32x32/navigate_up.ico":U
     LABEL "" 
     SIZE 9.6 BY 2.29.

DEFINE VARIABLE cb-blankno AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 9 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cb-formno AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 9 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cb-jobno2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00" 
     DROP-DOWN-LIST
     SIZE 9 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE cb-machine AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 37.4 BY 1
     FONT 37 NO-UNDO.

DEFINE VARIABLE ls-jobno AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.38
     FONT 38 NO-UNDO.

DEFINE VARIABLE ls-num-tags AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE ls-qty-per-tag AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 29.6 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE ls-tag AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 63.4 BY 1.38
     FONT 37 NO-UNDO.

DEFINE VARIABLE ls-total-run-qty AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 29.6 BY 1.38
     FONT 37 NO-UNDO.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 202 BY .1.

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
         FONT 37 ROW-HEIGHT-CHARS .95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     bt-exit AT ROW 2.38 COL 191.8 WIDGET-ID 84
     ls-tag AT ROW 1.76 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     ls-jobno AT ROW 3.91 COL 20.2 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cb-jobno2 AT ROW 3.95 COL 61 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     cb-formno AT ROW 3.95 COL 86.6 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     cb-blankno AT ROW 3.95 COL 114.2 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     cb-machine AT ROW 5.91 COL 31.6 COLON-ALIGNED NO-LABEL WIDGET-ID 94
     bt-create AT ROW 6.67 COL 174.2 WIDGET-ID 108
     ls-qty-per-tag AT ROW 8.19 COL 39.4 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     ls-total-run-qty AT ROW 8.19 COL 97.6 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     ls-num-tags AT ROW 8.19 COL 143 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     br-table AT ROW 10.52 COL 2 WIDGET-ID 200
     bt-first AT ROW 10.52 COL 191.8 WIDGET-ID 44
     bt-up AT ROW 12.95 COL 191.8 WIDGET-ID 40
     bt-down AT ROW 25.1 COL 191.8 WIDGET-ID 42
     bt-last AT ROW 27.52 COL 191.8 WIDGET-ID 46
     bt-adjust-qty AT ROW 30.52 COL 2 WIDGET-ID 110
     bt-print-selected AT ROW 30.52 COL 71 WIDGET-ID 114
     bt-print-all AT ROW 30.52 COL 141 WIDGET-ID 112
     "Quantity Per Tag:" VIEW-AS TEXT
          SIZE 29.6 BY .95 AT ROW 8.29 COL 11 WIDGET-ID 96
          FONT 37
     "RM or WIP Tag:" VIEW-AS TEXT
          SIZE 27 BY .95 AT ROW 1.95 COL 11 WIDGET-ID 86
          FONT 37
     "Blank #:" VIEW-AS TEXT
          SIZE 14 BY .95 AT ROW 4.05 COL 101.4 WIDGET-ID 58
          FONT 37
     "Job #:" VIEW-AS TEXT
          SIZE 11 BY .95 AT ROW 4.14 COL 11 WIDGET-ID 12
          FONT 37
     "Form #:" VIEW-AS TEXT
          SIZE 12.6 BY .95 AT ROW 4.1 COL 74.2 WIDGET-ID 48
          FONT 37
     "(optional)" VIEW-AS TEXT
          SIZE 16.8 BY .95 AT ROW 1.95 COL 104.2 WIDGET-ID 90
          FONT 37
     "Total Run Qty:" VIEW-AS TEXT
          SIZE 24 BY .95 AT ROW 8.29 COL 74.2 WIDGET-ID 100
          FONT 37
     "Machine/Op:" VIEW-AS TEXT
          SIZE 21.6 BY .95 AT ROW 6.05 COL 11 WIDGET-ID 92
          FONT 37
     "# Tags:" VIEW-AS TEXT
          SIZE 13.8 BY .95 AT ROW 8.33 COL 131.2 WIDGET-ID 104
          FONT 37
     RECT-26 AT ROW 5.62 COL 2.2 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 204.8 BY 36.19
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
         TITLE              = "Create WIP"
         HEIGHT             = 32.76
         WIDTH              = 201.6
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
/* BROWSE-TAB br-table ls-num-tags F-Main */
/* SETTINGS FOR BUTTON bt-adjust-qty IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-print-all IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-print-selected IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttBrowseInventory BY ttBrowseInventory.lastTransTime DESCENDING.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Create WIP */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
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
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO: 
    ASSIGN
        bt-adjust-qty:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
        bt-print-selected:LABEL IN FRAME {&FRAME-NAME} = "Print and Receive Selected".

    IF {&BROWSE-NAME}:NUM-SELECTED-ROWS = 1 THEN DO:
        {&BROWSE-NAME}:FETCH-SELECTED-ROW(1).
        
        IF hdBrowseBuffer:AVAILABLE THEN DO:
            FIND FIRST inventoryTransaction NO-LOCK
                 WHERE inventoryTransaction.inventoryStockID = hdBrowseBuffer:BUFFER-FIELD("inventoryStockID"):BUFFER-VALUE
                 NO-ERROR.
            IF AVAILABLE inventoryTransaction THEN DO:
                IF inventoryTransaction.transactionStatus = gcStatusTransactionInitial THEN
                    ASSIGN
                        bt-adjust-qty:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
                ELSE IF inventoryTransaction.transactionStatus = gcStatusTransactionPosted THEN
                    ASSIGN
                        bt-print-selected:LABEL IN FRAME {&FRAME-NAME} = "Re-Print Selected".
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
    IF DECIMAL(ls-qty-per-tag:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Quantity Per Tag cannot be 0.00"
               VIEW-AS ALERT-BOX ERROR.               
       APPLY "ENTRY" TO ls-qty-per-tag.
       RETURN.    
    END.
        
    IF DECIMAL(ls-total-run-qty:SCREEN-VALUE) < DECIMAL(ls-qty-per-tag:SCREEN-VALUE) THEN DO:
       MESSAGE "Total Run Qty " + STRING(ls-total-run-qty:SCREEN-VALUE) +
               " cannot be less than Quantity Per tag " + STRING(ls-qty-per-tag:SCREEN-VALUE)
               VIEW-AS ALERT-BOX ERROR.               
       APPLY "ENTRY" TO ls-total-run-qty.
       RETURN.
    END.

    FIND FIRST job-mch NO-LOCK
         WHERE job-mch.company EQ ipcCompany
           AND job-mch.job-no EQ ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME}
           AND job-mch.job-no2 EQ INT(cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME})
           AND job-mch.m-code EQ cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME}
           AND job-mch.frm EQ INT(cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME})
           AND job-mch.blank-no EQ INT(cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME})
           NO-ERROR.

    IF NOT AVAILABLE job-mch THEN
        RETURN.

    FIND FIRST job-mat NO-LOCK  
         WHERE job-mat.company EQ job-mch.company
           AND job-mat.job-no EQ job-mch.job-no
           AND job-mat.job-no2 EQ job-mch.job-no2
           AND job-mat.frm EQ job-mch.frm
           NO-ERROR.
  
    IF NOT AVAILABLE job-mat THEN
        RETURN.
      
    RUN CreatePreLoadtagsFromInputsWIP IN hdInventoryProcs (ROWID(job-mch), ROWID(job-mat), 
        DECIMAL(ls-total-run-qty:SCREEN-VALUE), DECIMAL(ls-qty-per-tag:SCREEN-VALUE), 1, "EA", OUTPUT lCreated, OUTPUT cMessage).  

    RUN CreateInventoryLoadtagsFromPreLoadtags IN hdInventoryProcs.

    FOR EACH ttInventoryStockLoadtag:
        ASSIGN 
            lCreated = NO
            cMessage = "". 
        RUN CreateInventoryStockFromLoadtag IN hdInventoryProcs (ttInventoryStockLoadtag.inventoryStockID, YES, NO, OUTPUT lCreated, OUTPUT cMessage).
    END.   
    
    RUN rebuildTempTable(INPUT ipcCompany,
                         INPUT ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                         INPUT cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},                         
                         INPUT cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                         INPUT cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                         INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}).               
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit W-Win
ON CHOOSE OF bt-exit IN FRAME F-Main
DO:
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

    RUN rebuildTempTable(INPUT ipcCompany,
                         INPUT ls-jobno:SCREEN-VALUE   IN FRAME {&FRAME-NAME},
                         INPUT cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                         INPUT cb-jobno2:SCREEN-VALUE  IN FRAME {&FRAME-NAME},
                         INPUT cb-formno:SCREEN-VALUE  IN FRAME {&FRAME-NAME},
                         INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME} IN FRAME  {&FRAME-NAME}.
    
    RUN TempTableToCSV IN hdOutputProcs( INPUT TEMP-TABLE ttPrintInventoryStock:HANDLE,
                                         INPUT cOutputFileName,
                                         INPUT TRUE).    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-print-selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-print-selected W-Win
ON CHOOSE OF bt-print-selected IN FRAME F-Main /* Print and Receive Selected */
DO:
    DEFINE VARIABLE cInventoryStatus  LIKE inventoryStock.inventoryStatus        NO-UNDO.
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
        
        RUN rebuildTempTable(INPUT ipcCompany,
                             INPUT ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                             INPUT cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                             INPUT cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                             INPUT cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                             INPUT cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

        APPLY "VALUE-CHANGED" TO {&BROWSE-NAME} IN FRAME  {&FRAME-NAME}.
        
        RUN TempTableToCSV IN hdOutputProcs( INPUT TEMP-TABLE ttPrintInventoryStock:HANDLE,
                                             INPUT cOutputFileName,
                                             INPUT TRUE).
    END.    
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
ON LEAVE OF ls-jobno IN FRAME F-Main
DO:
    DEFINE VARIABLE riJobMch AS ROWID     NO-UNDO.
     
    IF cValidateJobno = ls-jobno:SCREEN-VALUE THEN
        RETURN.
        
    RUN disableCreate.
    
    ASSIGN 
        cJobno2ListItems  = ""
        cFormnoListItems  = ""
        cBlanknoListitems = ""
        cMachineListItems = "".
    
    IF ls-jobno:SCREEN-VALUE = "" THEN
        RETURN.
        
    RUN updateComboBoxes.

    ASSIGN 
        cb-jobno2:SCREEN-VALUE  = ENTRY(1,cJobno2ListItems)
        cb-formno:SCREEN-VALUE  = ENTRY(1,cFormnoListItems)
        cb-blankno:SCREEN-VALUE = ENTRY(1,cBlanknoListItems)
        cb-machine:SCREEN-VALUE = ENTRY(1,cMachineListItems).
               
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-qty-per-tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-qty-per-tag W-Win
ON LEAVE OF ls-qty-per-tag IN FRAME F-Main
DO: 
    IF DECIMAL(ls-total-run-qty:SCREEN-VALUE) = 0 THEN
        ASSIGN 
            ls-total-run-qty:SCREEN-VALUE = ls-qty-per-tag:SCREEN-VALUE.
        
    IF DECIMAL(ls-qty-per-tag:SCREEN-VALUE) <> 0 AND
       DECIMAL(ls-total-run-qty:SCREEN-VALUE) <> 0 AND
       DECIMAL(ls-qty-per-tag:SCREEN-VALUE) <= DECIMAL(ls-total-run-qty:SCREEN-VALUE) THEN DO:
        
        ASSIGN
            ls-num-tags:SCREEN-VALUE = STRING(INTEGER(TRUNC(DECIMAL(ls-total-run-qty:SCREEN-VALUE) / DECIMAL(ls-qty-per-tag:SCREEN-VALUE),0)))
            bt-create:LABEL = "Create All".
    END.
    ELSE
        ASSIGN
            ls-num-tags:SCREEN-VALUE = "1"
            bt-create:LABEL = "Create".
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
ON LEAVE OF ls-total-run-qty IN FRAME F-Main
DO:    
    IF DECIMAL(ls-qty-per-tag:SCREEN-VALUE) <> 0 AND
       DECIMAL(ls-total-run-qty:SCREEN-VALUE) <> 0 AND
       DECIMAL(ls-qty-per-tag:SCREEN-VALUE) <= DECIMAL(ls-total-run-qty:SCREEN-VALUE) THEN DO:
        
        ASSIGN
            ls-num-tags:SCREEN-VALUE = STRING(INTEGER(TRUNC(DECIMAL(ls-total-run-qty:SCREEN-VALUE) / DECIMAL(ls-qty-per-tag:SCREEN-VALUE),0)))
            bt-create:LABEL = "Create All".
    END.
    ELSE
        ASSIGN
            ls-num-tags:SCREEN-VALUE = "1"
            bt-create:LABEL = "Create".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

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
        ls-qty-per-tag:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
        ls-total-run-qty:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
        ls-num-tags:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
        ls-qty-per-tag:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
        ls-total-run-qty:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
        ls-num-tags:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
        bt-create:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.

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
        ls-qty-per-tag:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
        ls-total-run-qty:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
        ls-num-tags:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
        bt-create:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
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
  ENABLE bt-exit RECT-26 ls-tag ls-jobno cb-jobno2 cb-formno cb-blankno 
         cb-machine bt-create ls-qty-per-tag ls-total-run-qty ls-num-tags 
         br-table bt-first bt-up bt-down bt-last 
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
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMachine  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormno   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankno  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opriJobMch  AS ROWID     NO-UNDO.
    
    DEFINE BUFFER bf-job-mch FOR job-mch.
    
    FIND FIRST bf-job-mch NO-LOCK
         WHERE bf-job-mch.company  = ipcCompany
           AND bf-job-mch.job-no   = ipcJobno
           AND bf-job-mch.m-code   = ipcMachine
           AND bf-job-mch.job-no2  = ipiJobno2
           AND bf-job-mch.frm      = ipiFormno
           AND bf-job-mch.blank-no = ipiBlankno NO-ERROR.
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
         WHERE company.company EQ ipcCompany NO-ERROR .
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - {&awversion}" + " - " 
                         + string(company.name) + " - " + ipcLocation.

    ASSIGN 
        hdBrowseQuery  = {&BROWSE-NAME}:QUERY IN FRAME {&FRAME-NAME}
        hdBrowseBuffer = hdBrowseQuery:GET-BUFFER-HANDLE(1).

    FIND FIRST sys-ctrl NO-LOCK                       
         WHERE sys-ctrl.company EQ ipcCompany
           AND sys-ctrl.name EQ {&sysCtrlName} NO-ERROR.     
 
    ASSIGN 
        cOutputFileName = IF AVAIL sys-ctrl THEN
                              TRIM(sys-ctrl.descrip) + "\wiptag.txt" 
                          ELSE
                              "C:\BA\Label\wiptag.txt".
    
    APPLY "ENTRY" TO ls-jobno IN FRAME {&FRAME-NAME}.

    RUN disableCreate.

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
    
    IF NOT AVAILABLE job-mch THEN DO: 
        MESSAGE "Invalid Job scan" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO ls-jobno in FRAME {&FRAME-NAME}.
        RETURN ERROR.
    END.    

    ASSIGN
        ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = ipcJobno.
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rebuildTempTable W-Win 
PROCEDURE rebuildTempTable :
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

    EMPTY TEMP-TABLE ttBrowseInventory.
    
    FOR EACH inventoryStock
       WHERE inventoryStock.company   = ipcCompany
         AND inventoryStock.jobID     = ipcJobno
         AND inventoryStock.jobID2    = ipiJobno2   
         AND inventoryStock.formNo    = ipiFormno   
         AND inventoryStock.blankNo   = ipiBlankno
         AND inventoryStock.machineID = ipcMachine:
         CREATE ttBrowseInventory.
         BUFFER-COPY inventoryStock EXCEPT inventoryStock.locationID TO ttBrowseInventory.
         ttBrowseinventory.locationID = inventoryStock.warehouseID + " " + inventoryStock.locationID.
    END.
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    
    hdBrowseQuery:GET-FIRST().
    IF hdBrowseBuffer:AVAILABLE THEN DO:
        ASSIGN
            bt-print-all:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
            bt-print-selected:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
        
        APPLY "VALUE-CHANGED" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
    END.    
    ELSE
        ASSIGN
            bt-print-all:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
            bt-adjust-qty:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
            bt-print-selected:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.    
   
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
        ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "".
    
    RUN disableCreate.
    
    FIND FIRST bf-inventoryStock NO-LOCK
         WHERE bf-inventoryStock.StockIDAlias = ipcTag NO-ERROR.

    IF AVAILABLE bf-inventoryStock THEN DO:
        ASSIGN
            ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = bf-inventoryStock.jobID.
                          
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
         AND buf-job-mch.job-no  = ls-jobno:SCREEN-VALUE IN FRAME {&FRAME-NAME}:
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
            cJobno2ListItems = "00"
            cb-jobno2:LIST-ITEMS IN FRAME {&FRAME-NAME} = cJobno2ListItems 
            cb-jobno2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00".
    ELSE
        cb-jobno2:LIST-ITEMS IN FRAME {&FRAME-NAME} = cJobno2ListItems.
 
    IF cFormnoListItems = "" THEN
        ASSIGN
            cFormnoListItems = "00"
            cb-formno:LIST-ITEMS IN FRAME {&FRAME-NAME} = cFormnoListItems 
            cb-formno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00".
    ELSE
        cb-formno:LIST-ITEMS IN FRAME {&FRAME-NAME} = cFormnoListItems.

    IF cBlanknoListItems = "" THEN
        ASSIGN
            cBlanknoListItems = "00"
            cb-blankno:LIST-ITEMS IN FRAME {&FRAME-NAME} = cBlanknoListItems
            cb-blankno:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "00".
    ELSE
        cb-blankno:LIST-ITEMS IN FRAME {&FRAME-NAME} = cBlanknoListItems.

    IF cMachineListItems = "" THEN
        ASSIGN
            cMachineListItems = ""
            cb-machine:LIST-ITEMS IN FRAME {&FRAME-NAME} = cMachineListItems
            cb-machine:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    ELSE
        cb-machine:LIST-ITEMS IN FRAME {&FRAME-NAME} = cMachineListItems.
    
    RELEASE buf-job-mch.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

