&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------------
  testSmart4glTv.w   a long TIME ago  by slacroix   
  Purpose: 
  Parameters:  <none>
  Notes:       
  28-NOV-2007 sla: demo usage of new options 'fgcolor=', 'bgcolor=', 'font=', and 'tooltip='
  introduced by Simon Prinsloo and Dries Feys 
     => the choose triger of the "Small TV" button
------------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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

DEFINE VARIABLE itraceEventTime AS INTEGER    NO-UNDO.

DEF TEMP-TABLE osFile NO-UNDO
 FIELD cFileName AS CHAR
 FIELD cFullPath  AS CHAR
 FIELD cAttr AS CHAR
 INDEX af cAttr cFileName.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fContainer

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addToEdMsg wWin 
FUNCTION addToEdMsg RETURNS LOGICAL
  (pcTxt AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BtnPopupMenu 
       MENU-ITEM m_Add_Before   LABEL "Add Before"    
       MENU-ITEM m_Add_After    LABEL "Add After"     
       RULE
       MENU-ITEM m_RUN_OtherDropTargetWinw LABEL "RUN treeview/OtherDropTargetWin.w"
       RULE
       MENU-ITEM m_build_and_open_a_popup_with LABEL "Build and open a popup without right click".


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_pure4gltv AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bPopulateLargeTree 
     LABEL "Large TV (1000 nodes)" 
     SIZE 25 BY 1.14 TOOLTIP "Populate a TV with 1000 nodes.  To test drag and drop on TV itself".

DEFINE BUTTON btnClearEditor 
     LABEL "Clear editor" 
     SIZE 12 BY 1.14 TOOLTIP "To clear the monitoring editor widget at the bottom".

DEFINE BUTTON btnDisplay 
     LABEL "Display TV  (time in ms =>)" 
     SIZE 28 BY 1.14 TOOLTIP "Already called at end of populate.  Button to test time of dummy refresh".

DEFINE BUTTON btnDumpNodeTable 
     LABEL "Display dumpNodeTable" 
     SIZE 24 BY 1.14.

DEFINE BUTTON btnEmptyTv 
     LABEL "Empty tree" 
     SIZE 14 BY 1.14 TOOLTIP "To empty the treeview so one can populate another demo TV"
     FONT 6.

DEFINE BUTTON btnFocusInfo  NO-FOCUS
     LABEL "Info Focus" 
     SIZE 12 BY 1.14 TOOLTIP "Flat button to message info about the FOCUS system handle".

DEFINE BUTTON BtnPopulateMore 
     LABEL "Medium (vert scrolling)" 
     SIZE 25 BY 1.14 TOOLTIP "Populate a bigger TV, to test vertical scrolling".

DEFINE BUTTON btnPopulateSmall 
     LABEL "Small TV" 
     SIZE 25 BY 1.14 TOOLTIP "Populate a small demo TV.  Has some addOnExpand Nodes".

DEFINE BUTTON btnPopulateSrCustOrder 
     LABEL "Salesrep + custome + order + orderline" 
     SIZE 50 BY 1.14 TOOLTIP "A sample TV with data nodes from sports2000.  4 levels of nodes".

DEFINE BUTTON btnPopulateWide 
     LABEL "Wide tree" 
     SIZE 25 BY 1.14 TOOLTIP "Populate a wide TV to test both vertical and horizontal  scrolling".

DEFINE BUTTON BtnPopupMenu 
     LABEL "More test" 
     SIZE 12 BY 1.67 TOOLTIP "Right click to get popup menu with few more options to test".

DEFINE VARIABLE cMoveMode AS CHARACTER FORMAT "X(256)":U INITIAL "after" 
     LABEL "mode" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "after","before","parent" 
     DROP-DOWN-LIST
     SIZE 14 BY 1 TOOLTIP "Mode used to call moveNode"
     BGCOLOR 10  NO-UNDO.

DEFINE VARIABLE edMsg AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 119 BY 6.91
     FONT 0 NO-UNDO.

DEFINE VARIABLE cFileSystem AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\" 
     LABEL "Populate FileSystem TV" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 TOOLTIP "To populate file system directory tree from this root directory  (press return)" NO-UNDO.

DEFINE VARIABLE cGetNodeDetails AS CHARACTER FORMAT "X(256)":U INITIAL "n21" 
     LABEL "NodeDetails" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 TOOLTIP "Press RETURN"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cMoveNode AS CHARACTER FORMAT "X(256)":U INITIAL "n22" 
     LABEL "Move node" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Key of node to move    Press return to apply"
     BGCOLOR 10  NO-UNDO.

DEFINE VARIABLE cMoveTo AS CHARACTER FORMAT "X(256)":U INITIAL "n3" 
     LABEL "to" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 TOOLTIP "New Parent or Next-sibling or Prev-sibling (depends on chosen mode)"
     BGCOLOR 10  NO-UNDO.

DEFINE VARIABLE cSelectNode AS CHARACTER FORMAT "X(256)":U INITIAL "n31" 
     LABEL "Select node" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 TOOLTIP "Key of node ot select    Used to test selection that leads to scroll to node"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cSwapNode AS CHARACTER FORMAT "X(256)":U INITIAL "n1" 
     LABEL "SwapNode" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 TOOLTIP "To swap a node with another (enter a node key and press RETURN)"
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE cSwapWith AS CHARACTER FORMAT "X(256)":U INITIAL "n2" 
     LABEL "with" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "See the other yellow fill-in"
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE deleteNode AS CHARACTER FORMAT "X(256)":U INITIAL "n221" 
     LABEL "Delete NodeKey" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Press RETURN to delete the node (branch) of that key"
     BGCOLOR 12  NO-UNDO.

DEFINE VARIABLE etimeToDisplay AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 TOOLTIP "time in ms to execute tvRefresh() in pure4GlTv"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE giRowsToBatch AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 3 
     LABEL "RowsToBatch" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 TOOLTIP "rowsToBatch when adding nodes on the fly with the Mode node or AddOnExpand"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE sortChildren AS CHARACTER FORMAT "X(256)":U INITIAL "k10" 
     LABEL "Sort children of" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Sort the children of a given parent (enter the node key and press enter)"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE updKey AS CHARACTER FORMAT "X(256)":U INITIAL "n21" 
     LABEL "Key" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .95 TOOLTIP "Node key of node to update"
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE updLabIco AS CHARACTER FORMAT "X(256)":U INITIAL "long long very long label that takes a lot of space#tvpics/$.bmp" 
     LABEL "Lab#Ico" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .95 TOOLTIP "new label and icone file name (separated by # here, please see the code)"
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE updOptn AS CHARACTER FORMAT "X(256)":U INITIAL "private@hello world#refresh" 
     LABEL "Optn" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .95 TOOLTIP "# and @ are replaced by CHR (1) and CHR(2) before sending to updateNode"
     BGCOLOR 11  NO-UNDO.

DEFINE RECTANGLE rectUpd
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 2.86.

DEFINE VARIABLE lTraceEvents AS LOGICAL INITIAL yes 
     LABEL "Trace events" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fContainer
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.2 ROW 1
         SIZE 164.8 BY 15.29.

DEFINE FRAME fMain
     btnPopulateSmall AT ROW 1.24 COL 3
     BtnPopulateMore AT ROW 1.24 COL 28
     cSelectNode AT ROW 1.24 COL 66 COLON-ALIGNED
     sortChildren AT ROW 1.24 COL 104 COLON-ALIGNED
     btnFocusInfo AT ROW 7.91 COL 55
     btnPopulateWide AT ROW 2.43 COL 3
     bPopulateLargeTree AT ROW 2.43 COL 28
     giRowsToBatch AT ROW 2.43 COL 66 COLON-ALIGNED
     cSwapNode AT ROW 2.43 COL 85 COLON-ALIGNED
     cSwapWith AT ROW 2.43 COL 104 COLON-ALIGNED
     btnPopulateSrCustOrder AT ROW 3.62 COL 3
     cGetNodeDetails AT ROW 3.62 COL 66 COLON-ALIGNED
     deleteNode AT ROW 3.62 COL 104 COLON-ALIGNED
     cFileSystem AT ROW 4.81 COL 24 COLON-ALIGNED
     cMoveNode AT ROW 4.81 COL 66 COLON-ALIGNED
     cMoveTo AT ROW 4.81 COL 84 COLON-ALIGNED
     cMoveMode AT ROW 4.81 COL 104 COLON-ALIGNED
     btnEmptyTv AT ROW 6.24 COL 3
     btnDisplay AT ROW 6.24 COL 17
     BtnPopupMenu AT ROW 6.24 COL 55
     etimeToDisplay AT ROW 6.33 COL 43 COLON-ALIGNED NO-LABEL
     updKey AT ROW 6.71 COL 73 COLON-ALIGNED
     updLabIco AT ROW 6.71 COL 96 COLON-ALIGNED
     lTraceEvents AT ROW 7.91 COL 3
     btnClearEditor AT ROW 7.91 COL 19
     btnDumpNodeTable AT ROW 7.91 COL 31
     updOptn AT ROW 7.91 COL 73 COLON-ALIGNED
     edMsg AT ROW 9.33 COL 1 NO-LABEL
     " Update node" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 6 COL 86
     rectUpd AT ROW 6.24 COL 68
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 45.6 ROW 1
         SIZE 120 BY 15.24.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Test the pure4glTv Smart Object   (this window is resizable)"
         HEIGHT             = 15.29
         WIDTH              = 165.4
         MAX-HEIGHT         = 29.67
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 29.67
         VIRTUAL-WIDTH      = 256
         RESIZE             = yes
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME fMain:FRAME = FRAME fContainer:HANDLE.

/* SETTINGS FOR FRAME fContainer
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME fMain
                                                                        */
ASSIGN 
       BtnPopupMenu:POPUP-MENU IN FRAME fMain       = MENU POPUP-MENU-BtnPopupMenu:HANDLE.

ASSIGN 
       etimeToDisplay:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Test the pure4glTv Smart Object   (this window is resizable) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Test the pure4glTv Smart Object   (this window is resizable) */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-RESIZED OF wWin /* Test the pure4glTv Smart Object   (this window is resizable) */
DO:
  DEFINE VARIABLE iHorizontalGap  AS DEC    NO-UNDO.
  DEFINE VARIABLE iVerticalGap    AS DEC    NO-UNDO.
  DEFINE VARIABLE iTvWidth        AS DEC    NO-UNDO.
  DEFINE VARIABLE lresizeVertical AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lresizeHorizontal AS LOGICAL    NO-UNDO.
  
  
  {get resizeVertical lresizeVertical h_pure4glTv}.
  IF lresizeVertical = ? THEN lresizeVertical = YES.
  {get resizeHorizontal lresizeHorizontal h_pure4glTv}.
  IF lresizeHorizontal = ? THEN lresizeHorizontal = YES.
  
  iTvWidth = FRAME fMain:COL - 1.7.

  iVerticalGap = SELF:HEIGHT-CHAR - FRAME fContainer:HEIGHT-CHARS.
  iHorizontalGap = SELF:WIDTH-CHAR - FRAME fContainer:WIDTH-CHARS.
  
  IF iVerticalGap > 0 THEN ASSIGN
   FRAME fContainer:HEIGHT-CHARS = SELF:HEIGHT-CHAR
   FRAME fMain:HEIGHT-CHARS = SELF:HEIGHT-CHAR
   edMsg:HEIGHT-CHARS = SELF:HEIGHT-CHARS - 0.2 - edMsg:ROW + 1.
  
  IF iHorizontalGap > 0 THEN ASSIGN
   FRAME fContainer:WIDTH-CHARS = SELF:WIDTH-CHAR
   FRAME fMain:COL = FRAME fMain:COL + iHorizontalGap.
  
  RUN resizeObject IN h_pure4glTv
    (IF lresizeVertical THEN SELF:HEIGHT-CHARS - 0.2 ELSE ?
    ,IF lresizeHorizontal THEN iTVWidth + iHorizontalGap ELSE ?).

  IF iVerticalGap < 0 THEN ASSIGN
   edMsg:HEIGHT-CHARS = SELF:HEIGHT-CHARS - 0.2 - edMsg:ROW + 1
   FRAME fMain:HEIGHT-CHARS = SELF:HEIGHT-CHAR
   FRAME fContainer:HEIGHT-CHARS = SELF:HEIGHT-CHAR.

  IF iHorizontalGap < 0 THEN ASSIGN
   FRAME fMain:COL = FRAME fMain:COL + iHorizontalGap
   FRAME fContainer:WIDTH-CHARS = SELF:WIDTH-CHAR.


   /* no scrollbar when shrinking please */
   FRAME fContainer:VIRTUAL-HEIGHT-CHARS = SELF:HEIGHT-CHAR.
   FRAME fContainer:VIRTUAL-WIDTH-CHARS = SELF:WIDTH-CHAR.
   
   FRAME fMain:VIRTUAL-HEIGHT-CHARS = FRAME fMain:HEIGHT-CHARS.
   FRAME fMain:VIRTUAL-WIDTH-CHARS = FRAME fMain:WIDTH-CHARS.

   /* 21-MAR-2005: To fix a problem in 10.0B where the scroll thumb may 
    disappear because of the above call to get rid off native scrollbars
    in the container...
    This problem may go away in 10.0B02 */
   RUN hideObject IN h_pure4glTv.
   RUN viewObject IN h_pure4glTv.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME bPopulateLargeTree
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bPopulateLargeTree wWin
ON CHOOSE OF bPopulateLargeTree IN FRAME fMain /* Large TV (1000 nodes) */
DO:

DEFINE VARIABLE iEtime AS INTEGER    NO-UNDO.
iEtime = ETIME.


DEFINE VARIABLE n AS INTEGER    NO-UNDO.
DEFINE VARIABLE p AS CHAR       NO-UNDO.

DO ON ERROR UNDO, LEAVE:
  DO n = 0 TO 999:
      IF n < 10 THEN p = "".
      ELSE p = "k" + STRING(TRUNCATE(n / 10,0)).
      RUN addNode IN h_pure4gltv ("k" + STRING(n)
                                   ,p
                                   ,"k " + STRING(n)
                                   ,IF n < 10 THEN "treeview/tvpics/books05.bmp" ELSE IF n < 100 THEN "treeview/tvpics/book02.bmp" ELSE "treeview/tvpics/present1.bmp"
                                   ,"expanded") NO-ERROR.
  END.
END.

IF lTraceEvents THEN addToEdMsg("TV loaded in " + STRING(ETIME - iEtime) + " ms~n").
IF ERROR-STATUS:ERROR THEN MESSAGE RETURN-VALUE.
APPLY 'CHOOSE' TO btnDisplay.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearEditor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearEditor wWin
ON CHOOSE OF btnClearEditor IN FRAME fMain /* Clear editor */
DO:
    edMsg:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDisplay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDisplay wWin
ON CHOOSE OF btnDisplay IN FRAME fMain /* Display TV  (time in ms =>) */
DO:
  etimeToDisplay = ETIME.
  DYNAMIC-FUNCTION('tvRefresh':U IN h_pure4gltv).
  etimeToDisplay = ETIME - etimeToDisplay.
  DISPLAY etimeToDisplay WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDumpNodeTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDumpNodeTable wWin
ON CHOOSE OF btnDumpNodeTable IN FRAME fMain /* Display dumpNodeTable */
DO:
  RUN dumpNodeTable IN h_pure4gltv.
  edMsg:SCREEN-VALUE = "".
  edMsg:INSERT-FILE("dumpNodeTable.txt").
/*  edMsg:SCREEN-VALUE = REPLACE(edMsg:SCREEN-VALUE,"~n~n","~n").*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEmptyTv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEmptyTv wWin
ON CHOOSE OF btnEmptyTv IN FRAME fMain /* Empty tree */
DO:
  RUN emptyTree IN h_pure4gltv.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFocusInfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFocusInfo wWin
ON CHOOSE OF btnFocusInfo IN FRAME fMain /* Info Focus */
DO:
  MESSAGE FOCUS:TYPE SKIP
   FOCUS:NAME SKIP
   IF CAN-QUERY(FOCUS,"SCREEN-VALUE") THEN FOCUS:SCREEN-VALUE ELSE ""
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnPopulateMore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnPopulateMore wWin
ON CHOOSE OF BtnPopulateMore IN FRAME fMain /* Medium (vert scrolling) */
DO:

DEFINE VARIABLE iEtime AS INTEGER    NO-UNDO.
iEtime = ETIME.

DO ON ERROR UNDO, LEAVE:
  RUN addNode IN h_pure4gltv ("n1", "", "node 1","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n2", "", "node 2","","expanded") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n21", "n2", "node 21","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n22", "n2", "node 22","","expanded") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n221", "n22", "node 221","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n222", "n22", "node 222","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n3", "", "node 3","","expanded") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n31", "n3", "node 31","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n4", "", "node 4","","expanded") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n41", "n4", "node 41","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n42", "n4", "node 42","","expanded") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n421", "n42", "node 421","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n422", "n42", "node 422","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n43", "n4", "node 43","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n44", "n4", "node 44","","expanded") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n441", "n44", "node 441","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n442", "n44", "node 442","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n45", "n4", "node 45","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n46", "n4", "node 46","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n461", "n46", "node 461","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n462", "n46", "node 462","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n5", "", "node 5","","expanded") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n51", "n5", "node 51","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n52", "n5", "node 52","","expanded") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n521", "n52", "node 521","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n522", "n52", "node 522","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n53", "n5", "node 53","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n54", "n5", "node 54","","expanded") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n541", "n54", "node 541","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n542", "n54", "node 542","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n55", "n5", "node 55","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n56", "n5", "node 56","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n561", "n56", "node 561","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n562", "n56", "node 562","","") NO-ERROR.
END.

IF lTraceEvents THEN addToEdMsg("TV loaded in " + STRING(ETIME - iEtime) + " ms~n").
IF ERROR-STATUS:ERROR THEN MESSAGE RETURN-VALUE.
APPLY 'CHOOSE' TO btnDisplay.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPopulateSmall
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPopulateSmall wWin
ON CHOOSE OF btnPopulateSmall IN FRAME fMain /* Small TV */
DO:

DEFINE VARIABLE iEtime AS INTEGER    NO-UNDO.
iEtime = ETIME.

/* 28-NOV-2007 sla: show nice new feature implemented by Simon Prinsloo */
DEFINE VARIABLE cFgColor AS CHARACTER   NO-UNDO.
cFgColor = CHR(1) + "fgcolor=9" + CHR(1) + "tooltip=This node shows in blue with the new option 'fgColor=' and shows this very tooltip with the new option 'tooltip='".


DO ON ERROR UNDO, LEAVE:
  RUN addNode IN h_pure4gltv  ("n1"   ,""    ,"node 1 (drop on another window)"   ,""                    ,"")             NO-ERROR.
  RUN addNode IN h_pure4gltv  ("n2"   ,""    ,"node 2"   ,""                    ,"expanded" + cFgColor) NO-ERROR.
  RUN addNode IN h_pure4gltv  ("n21"  ,"n2"  ,"node 21"  ,""                    ,"" + cFgColor)             NO-ERROR.
  RUN addNode IN h_pure4gltv  ("n22"  ,"n2"  ,"node 22"  ,"treeview/tvpics/book02.bmp"  ,"expanded" + cFgColor) NO-ERROR.
  RUN addNode IN h_pure4gltv  ("n221" ,"n22" ,"221 dummy addOnExpand" ,"treeview/tvpics/book02.bmp"  ,"addOnExpand" + cFgColor)             NO-ERROR.
  RUN addNode IN h_pure4gltv  ("n222" ,"n22" ,"222 real addOnExpand","treeview/tvpics/book02.bmp"  ,"addOnExpand" + cFgColor)             NO-ERROR.
  RUN addNode IN h_pure4gltv  ("n3"   ,""    ,"node 3  dragSource option"   ,"treeview/tvpics/present1.bmp" ,"expanded" + CHR(1) + "dragSource") NO-ERROR.
  RUN addNode IN h_pure4gltv  ("n31"  ,"n3"  ,"node 31 nodragSource option"  ,"treeview/tvpics/$.bmp"        ,"noDragSource")             NO-ERROR.
  RUN addNode IN h_pure4gltv  ("n4"   ,""    ,"node 4  returns cancelDrag"   ,"treeview/tvpics/smile56.bmp"  ,"bgcolor=12" + CHR(1) + "font=6" + CHR(1) + "tooltip=This node uses the new options 'bgcolor=', 'font=', and 'tooltip='"         )   NO-ERROR.
  RUN addNode IN h_pure4gltv  ("n5"   ,""    ,"node 5  with unvalid picture file"   ,"treeview/tvpics/ThereIsNoSuchPicFile.bmp"  ,"")             NO-ERROR.
END.

IF lTraceEvents THEN addToEdMsg("TV loaded in " + STRING(ETIME - iEtime) + " ms~n").
IF ERROR-STATUS:ERROR THEN MESSAGE RETURN-VALUE.
APPLY 'CHOOSE' TO btnDisplay.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPopulateSmall wWin
ON CURSOR-DOWN OF btnPopulateSmall IN FRAME fMain /* Small TV */
DO:
  
  MESSAGE PROGRAM-NAME(1) "in" THIS-PROCEDURE:FILE-NAME SKIP
       
      VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Debug message".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPopulateSrCustOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPopulateSrCustOrder wWin
ON CHOOSE OF btnPopulateSrCustOrder IN FRAME fMain /* Salesrep + custome + order + orderline */
DO:

DEFINE VARIABLE iEtime AS INTEGER    NO-UNDO.
iEtime = ETIME.


DEFINE VARIABLE iSR AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCust AS INTEGER    NO-UNDO.
DEFINE VARIABLE iOrder AS INTEGER    NO-UNDO.
DEFINE VARIABLE iOrderline AS INTEGER    NO-UNDO.

DO ON ERROR UNDO, LEAVE:
FOR EACH salesrep NO-LOCK BY salesrep.rep-name:
    IF isr > 100 THEN LEAVE.  /* will handle 'More...' nodes later ...*/
    RUN addNode IN h_pure4gltv ("sr=" + salesrep.sales-rep
                               ,""
                               ,salesrep.rep-name
                               ,"treeview/tvpics/user.bmp"
                               ,IF CAN-FIND(FIRST customer OF salesrep) THEN "addOnExpand" ELSE "").
    iSR = iSR + 1.
END. /* FOR EACH slaeres rep*/

END.

IF lTraceEvents THEN addToEdMsg("TV loaded in " + STRING(ETIME - iEtime) + " ms~n").
IF ERROR-STATUS:ERROR THEN MESSAGE RETURN-VALUE.
APPLY 'CHOOSE' TO btnDisplay.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPopulateWide
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPopulateWide wWin
ON CHOOSE OF btnPopulateWide IN FRAME fMain /* Wide tree */
DO:

DEFINE VARIABLE iEtime AS INTEGER    NO-UNDO.
iEtime = ETIME.
  
  
DO ON ERROR UNDO, LEAVE:
  RUN addNode IN h_pure4gltv ("n1", "", "node 1","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n2", "", "node 2","","expanded") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n21", "n2", "node 21","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n22", "n2", "node 22","","expanded") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n221", "n22", "node 221","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n222", "n22", "node 222","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n3", "", "node 3","","expanded") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n31", "n3", "node 31","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n4", "", "node 4","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n41", "n4", "node 41","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n42", "n4", "node 42","","expanded") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n421", "n42", "node 421","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n422", "n42", "node 422","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n43", "n4", "node 43","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n44", "n4", "node 44","","expanded") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n441", "n44", "node 441","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n442", "n44", "node 442","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n45", "n4", "node 45","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n46", "n4", "node 46","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n461", "n46", "node 461","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n462", "n46", "node 462","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n5", "", "node 5","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n51", "n5", "node 51   this label larger","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n511", "n51", "node 511   this label is intentionaly very wide to force scrolling","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n52", "n5", "node 52","","expanded") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n53", "n5", "node 53   this label is intentionaly very wide to force scrolling","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n54", "n5", "node 54   this label is also wide ","","") NO-ERROR.
  RUN addNode IN h_pure4gltv ("n541", "n54", "node 541   this one too","","") NO-ERROR.
END.

IF lTraceEvents THEN addToEdMsg("TV loaded in " + STRING(ETIME - iEtime) + " ms~n").
IF ERROR-STATUS:ERROR THEN MESSAGE RETURN-VALUE.
APPLY 'CHOOSE' TO btnDisplay.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cFileSystem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFileSystem wWin
ON RETURN OF cFileSystem IN FRAME fMain /* Populate FileSystem TV */
DO:
  RUN loadDirectory("", cFileSystem).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFileSystem wWin
ON VALUE-CHANGED OF cFileSystem IN FRAME fMain /* Populate FileSystem TV */
DO:
  ASSIGN cFileSystem.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cGetNodeDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cGetNodeDetails wWin
ON RETURN OF cGetNodeDetails IN FRAME fMain /* NodeDetails */
DO:
    DEFINE VARIABLE hNodeBuffer AS HANDLE     NO-UNDO.
    
    RUN getNodeDetails IN h_pure4gltv
    ( INPUT SELF:SCREEN-VALUE /* CHARACTER */,
      OUTPUT hNodeBuffer /* HANDLE */).

    MESSAGE
    "id:"           hNodeBuffer:BUFFER-FIELD("id"):BUFFER-VALUE       SKIP
    "lab:"          hNodeBuffer:BUFFER-FIELD("lab"):BUFFER-VALUE      SKIP
    "ico:"          hNodeBuffer:BUFFER-FIELD("ico"):BUFFER-VALUE      SKIP
    "level:"        hNodeBuffer:BUFFER-FIELD("level"):BUFFER-VALUE    SKIP
    "par:"          hNodeBuffer:BUFFER-FIELD("par"):BUFFER-VALUE      SKIP
    "prev-sibling:" hNodeBuffer:BUFFER-FIELD("pre"):BUFFER-VALUE      SKIP
    "next-sibling:" hNodeBuffer:BUFFER-FIELD("nex"):BUFFER-VALUE      SKIP
    "expanded:"     hNodeBuffer:BUFFER-FIELD("expanded"):BUFFER-VALUE SKIP
    "optn:"         hNodeBuffer:BUFFER-FIELD("optn"):BUFFER-VALUE
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
    DELETE OBJECT hNodeBuffer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cMoveMode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cMoveMode wWin
ON RETURN OF cMoveMode IN FRAME fMain /* mode */
DO:
  APPLY 'RETURN' TO cMoveNode.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cMoveMode wWin
ON VALUE-CHANGED OF cMoveMode IN FRAME fMain /* mode */
DO:
    ASSIGN {&SELF-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cMoveNode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cMoveNode wWin
ON RETURN OF cMoveNode IN FRAME fMain /* Move node */
DO:
  RUN moveNode IN h_pure4gltv (cMoveNode, cMoveTo, cMoveMode , "refresh") NO-ERROR.
  IF ERROR-STATUS:ERROR THEN MESSAGE 
   "MoveNode failed with the following error message:" SKIP
     RETURN-VALUE
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cMoveNode wWin
ON VALUE-CHANGED OF cMoveNode IN FRAME fMain /* Move node */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cMoveTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cMoveTo wWin
ON RETURN OF cMoveTo IN FRAME fMain /* to */
DO:
  APPLY 'RETURN' TO cMoveNode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cMoveTo wWin
ON VALUE-CHANGED OF cMoveTo IN FRAME fMain /* to */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cSelectNode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSelectNode wWin
ON RETURN OF cSelectNode IN FRAME fMain /* Select node */
DO:
    DYNAMIC-FUNCTION('selectNode' IN h_pure4gltv , cSelectNode).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSelectNode wWin
ON VALUE-CHANGED OF cSelectNode IN FRAME fMain /* Select node */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cSwapNode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSwapNode wWin
ON RETURN OF cSwapNode IN FRAME fMain /* SwapNode */
DO:
  RUN SwapNodes IN h_pure4gltv (cSwapNode, cSwapWith, "refresh") NO-ERROR.
  IF ERROR-STATUS:ERROR THEN MESSAGE 
   "SwapNodes failed with the following error message:" SKIP
     RETURN-VALUE
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSwapNode wWin
ON VALUE-CHANGED OF cSwapNode IN FRAME fMain /* SwapNode */
DO:
  ASSIGN {&SELF-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cSwapWith
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSwapWith wWin
ON RETURN OF cSwapWith IN FRAME fMain /* with */
DO:
  APPLY 'RETURN' TO cSwapNode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSwapWith wWin
ON VALUE-CHANGED OF cSwapWith IN FRAME fMain /* with */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME deleteNode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL deleteNode wWin
ON RETURN OF deleteNode IN FRAME fMain /* Delete NodeKey */
DO:
  ASSIGN {&SELF-NAME}.
  RUN deleteNode IN h_pure4glTv ({&SELF-NAME}, "refresh") NO-ERROR.
  IF ERROR-STATUS:ERROR THEN 
   MESSAGE RETURN-VALUE
       VIEW-AS ALERT-BOX WARNING BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME giRowsToBatch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL giRowsToBatch wWin
ON VALUE-CHANGED OF giRowsToBatch IN FRAME fMain /* RowsToBatch */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lTraceEvents
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lTraceEvents wWin
ON VALUE-CHANGED OF lTraceEvents IN FRAME fMain /* Trace events */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Add_After
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Add_After wWin
ON CHOOSE OF MENU-ITEM m_Add_After /* Add After */
DO:
  RUN addNode IN h_pure4gltv  ("addedAfter"   ,"n22"    ,"addedAfter n22"   ,""                    ,"addMode=after" + CHR(1) + "refresh")             NO-ERROR.
  IF ERROR-STATUS:ERROR THEN MESSAGE RETURN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Add_Before
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Add_Before wWin
ON CHOOSE OF MENU-ITEM m_Add_Before /* Add Before */
DO:
  RUN addNode IN h_pure4gltv  ("addedBefore"   ,"n22"    ,"addedBefore n22"   ,""                    ,"addMode=before" + CHR(1) + "refresh")             NO-ERROR.
  IF ERROR-STATUS:ERROR THEN MESSAGE RETURN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_build_and_open_a_popup_with
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_build_and_open_a_popup_with wWin
ON CHOOSE OF MENU-ITEM m_build_and_open_a_popup_with /* Build and open a popup without right click */
DO:
  RUN buildAndOpenPopupMenu IN h_pure4gltv
       ( INPUT "n21"
        ,INPUT "Yo ho ho and a bottle of Rum,MenuPirateSong,RULE,,Isn't it cool?,MenuCool" ).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_RUN_OtherDropTargetWinw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_RUN_OtherDropTargetWinw wWin
ON CHOOSE OF MENU-ITEM m_RUN_OtherDropTargetWinw /* RUN OtherDropTargetWin.w */
DO:
  RUN OtherDropTargetWin.w PERSISTENT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sortChildren
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sortChildren wWin
ON RETURN OF sortChildren IN FRAME fMain /* Sort children of */
DO:
    RUN sortChildren IN h_pure4gltv ({&SELF-NAME} , "refresh").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sortChildren wWin
ON VALUE-CHANGED OF sortChildren IN FRAME fMain /* Sort children of */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME updKey
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL updKey wWin
ON RETURN OF updKey IN FRAME fMain /* Key */
DO:
    updOptn   = REPLACE(updoptn,"#", CHR(1)).
    updOptn   = REPLACE(updoptn,"@", CHR(2)).
    
    updLabIco = REPLACE(updLabIco,"#", CHR(1)).
    
    RUN updateNode IN h_pure4gltv  (updKey ,"lab,ico" ,updLabIco ,updOptn) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN MESSAGE RETURN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL updKey wWin
ON VALUE-CHANGED OF updKey IN FRAME fMain /* Key */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME updLabIco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL updLabIco wWin
ON RETURN OF updLabIco IN FRAME fMain /* Lab#Ico */
DO:
    APPLY 'RETURN' TO updkey.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL updLabIco wWin
ON VALUE-CHANGED OF updLabIco IN FRAME fMain /* Lab#Ico */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME updOptn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL updOptn wWin
ON RETURN OF updOptn IN FRAME fMain /* Optn */
DO:
    APPLY 'RETURN' TO updkey.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL updOptn wWin
ON VALUE-CHANGED OF updOptn IN FRAME fMain /* Optn */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fContainer
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}


/*   This was a bad idea to run a test instance persistently form protools/run
 so I could interact with it.  when running the window directly form the AppBuilder
 I was initializing the window and smartTV twice, resulting in 2 sets of widgets
 for the scrollbar....
  It is better to use runit.w from protools/run with persistent option.  This 
 last guy will take care of initializeing the window.
  
    Anyway, I have added a protection to not call initializeObject twice in the
     4GLTreeview object 
RUN initializeObject.
  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'treeview/pure4gltv.w':U ,
             INPUT  FRAME fContainer:HANDLE ,
             INPUT  'wineModeAutomaticwindowsSkinAutomaticpicCacheCoef1labCacheCoef1tvIterationHeight17TreeStyle3FocSelNodeBgColor1UnfSelNodeBgColor8tvnodeDefaultFont1FocSelNodeFgColor15UnfSelNodeFgColor0resizeVerticalyesresizeHorizontalyesDragSourceallautoSortnoMSkeyScrollForcePaintyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_pure4gltv ).
       RUN repositionObject IN h_pure4gltv ( 1.24 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_pure4gltv ( 15.00 , 44.00 ) NO-ERROR.

       /* Links to pure4glTv h_pure4gltv. */
       RUN addLink ( h_pure4gltv , 'tvNodeEvent':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_pure4gltv ,
             FRAME fMain:HANDLE , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  VIEW FRAME fContainer IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fContainer}
  DISPLAY cSelectNode sortChildren giRowsToBatch cSwapNode cSwapWith 
          cGetNodeDetails deleteNode cFileSystem cMoveNode cMoveTo cMoveMode 
          etimeToDisplay updKey updLabIco lTraceEvents updOptn edMsg 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE rectUpd btnPopulateSmall BtnPopulateMore cSelectNode sortChildren 
         btnFocusInfo btnPopulateWide bPopulateLargeTree giRowsToBatch 
         cSwapNode cSwapWith btnPopulateSrCustOrder cGetNodeDetails deleteNode 
         cFileSystem cMoveNode cMoveTo cMoveMode btnEmptyTv btnDisplay 
         BtnPopupMenu etimeToDisplay updKey updLabIco lTraceEvents 
         btnClearEditor btnDumpNodeTable updOptn edMsg 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  wwin:MAX-WIDTH-PIXELS = SESSION:WIDTH-PIXELS * 2 NO-ERROR.
  wWin:MIN-WIDTH-PIXELS = wWin:WIDTH-PIXELS - 100.
  btnEmptyTv:LOAD-MOUSE-POINTER("CROSS") IN FRAME fMain. /* to test if effective with drag and drop
                                                          => well, apparently, it is not */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadDirectory wWin 
PROCEDURE loadDirectory :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcParentKey AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cDir      AS CHARACTER  NO-UNDO.

DEFINE VARIABLE iEtime AS INTEGER    NO-UNDO.
iEtime = ETIME.


DEFINE VARIABLE cFileName   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFullPath   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAttr       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ico         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE optn        AS CHARACTER  NO-UNDO.


EMPTY TEMP-TABLE osFile. /* should be useless now */

/* First load the directory in a temp-table so we can sort it afterwards */
INPUT FROM OS-DIR(cDir).
REPEAT:
    IMPORT cfileName cFullPath cAttr.
    
    IF cFileName = ".."  THEN NEXT.
    IF cFileName = "."   THEN NEXT.
    
    CREATE osFile.
    ASSIGN
     osFile.cFileName = cFileName
     osFile.cFullPath = cFullPath
     osFile.cAttr     = cAttr.
END.
INPUT CLOSE.

/* now, load the nodes */
FOR EACH osFile
 BY osFile.cAttr /* the directories ("D") will come first */
 BY osFile.cFileName:
    IF osFile.cAttr = "D" THEN ASSIGN
     ico = "treeview/tvpics/fold"
     optn = "addOnExpand".
    ELSE ASSIGN
     ico = "treeview/tvpics/blankSheet"
     optn = "".
    
    RUN addNode IN h_pure4gltv  ("fileName=" + osFile.cFullPath
                                ,pcParentKey
                                ,osFile.cFileName
                                ,ico
                                ,optn) NO-ERROR.
    
    IF ERROR-STATUS:ERROR THEN DO:
        EMPTY TEMP-TABLE osFile.
        MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN ERROR.
    END.
END.

EMPTY TEMP-TABLE osFile.
IF lTraceEvents THEN addToEdMsg("File system TV loaded in " + STRING(ETIME - iEtime) + " ms~n").

APPLY 'CHOOSE' TO btnDisplay IN FRAME fMain.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE traceIt wWin 
PROCEDURE traceIt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pc  AS CHARACTER  NO-UNDO.
    
    addToEdMsg(pc + "~n").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tvNodeAddOnExpand wWin 
PROCEDURE tvNodeAddOnExpand :
/*------------------------------------------------------------------------------
  Purpose: example to use tvNodeEvent procedure with pure4GlTv
  Parameters: 
  Notes: I use this procedure for mulitple demo treeview
     Note that I rely on pcnodeKey to distinguish the different
     sample treeview
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcnodeKey AS CHARACTER  NO-UNDO.

DEFINE VARIABLE nCust     AS INTEGER    NO-UNDO.
DEFINE VARIABLE norder    AS INTEGER    NO-UNDO.

DEFINE VARIABLE cSalesrep AS CHARACTER  NO-UNDO.
DEFINE VARIABLE icustnum  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iorder    AS INTEGER    NO-UNDO.
DEFINE VARIABLE cFullPath AS CHARACTER  NO-UNDO.


/*========= For text treview sample, nodeKey beings 'n' ================
  for n221   I do not add a node, this one is actually a dummy
            addOnExpand node to show that pure4glTv does not panic
      
      n222   I add two nodes n2221 and n2222
      
      n2222  I add a node n22221*/
IF pcNodeKey = "n222" THEN DO:
    RUN addNode IN h_pure4gltv  ("n2221" ,"n222" ,"node 2221" ,"treeview/tvpics/books05.bmp"  ,"selected").
    RUN addNode IN h_pure4gltv  ("n2222" ,"n222" ,"node 2222" ,"treeview/tvpics/books05.bmp"  ,"addOnExpand").
/* do by picLeftMouseEvent now    DYNAMIC-FUNCTION('tvRefresh':U IN h_pure4gltv).*/
END.
IF pcNodeKey = "n2222" THEN DO:
    RUN addNode IN h_pure4gltv  ("n22221" ,"n2222" ,"node 22221" ,"treeview/tvpics/user.bmp"  ,"").
/* do by picLeftMouseEvent now    DYNAMIC-FUNCTION('tvRefresh':U IN h_pure4gltv).*/
END.



/*======== For data treeview example on salesrep customer order orderline: ========*/


/*-------------- add customers to salesrep --------------*/
IF pcNodeKey BEGINS "sr=" THEN DO:
    cSalesrep = ENTRY(2,pcNodeKey,"=").
    FOR EACH customer NO-LOCK WHERE customer.sales-rep = cSalesrep BY customer.name:
        nCust = nCust + 1.
        IF nCust > giRowsToBatch THEN DO:
            RUN addNode IN h_pure4gltv ("MoreCust=" + STRING(customer.cust-num)
                                       ,pcNodeKey
                                       ,"More..."
                                       ,""
                                       ,"").
            LEAVE.
        END.
        RUN addNode IN h_pure4gltv ("cust=" + STRING(customer.cust-num)
                                   ,pcNodeKey
                                   ,customer.name
                                   ,"treeview/tvpics/smile56.bmp"
                                   ,IF CAN-FIND(FIRST order OF customer) THEN "addOnExpand" ELSE "").
    END.  /* for each customer */
END. /* add customers to salesrep node */


/*-------------- add orders to customer --------------*/
IF pcNodeKey BEGINS "cust=" THEN DO:
    icustnum = INT(ENTRY(2,pcNodeKey,"=")).
    FOR EACH order NO-LOCK WHERE order.cust-num = icustnum BY order.order-num:
        norder = nOrder + 1.
        IF norder > giRowsToBatch THEN DO:
            RUN addNode IN h_pure4gltv ("MoreOrder=" + STRING(order.order-num)
                                       ,pcNodeKey
                                       ,"More..."
                                       ,""
                                       ,"").
            LEAVE.
        END.
        RUN addNode IN h_pure4gltv ("order=" + STRING(order.order-num)
                                   ,pcNodeKey
                                   ,STRING(order.order-num) + " (" + STRING(order.order-date) + ")"
                                   ,"treeview/tvpics/book02.bmp"
                                   ,IF CAN-FIND(FIRST order-line OF order) THEN "addOnExpand" ELSE "").
    END.  /* for each customer */
END. /* add customers to salesrep node */


/*-------------- add orderline to order --------------*/
IF pcNodeKey BEGINS "order=" THEN DO:
    iorder   = INT(ENTRY(2,pcNodeKey,"=")).
    FOR EACH order-line NO-LOCK WHERE order-line.order-num = iorder BY order-line.line-num:
        FIND sports.ITEM OF order-line NO-LOCK. 
        RUN addNode IN h_pure4gltv ("OL=" + STRING(iorder) + ";" + STRING(order-line.line-num)
                                   ,pcNodeKey
                                   ,STRING(order-line.line-num) + "  " + ITEM.item-name
                                   ,"treeview/tvpics/present1.bmp"
                                   ,"") NO-ERROR.
    END. /* FOR EACH orderline */
    IF ERROR-STATUS:ERROR THEN MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.


/* example with directory tree */
IF pcNodeKey BEGINS "fileName=" THEN DO:
    cFullPath = ENTRY(2,pcNodeKey,"=").
    RUN loadDirectory (pcNodeKey, cFullPath).
END.

 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tvNodeCreatePopup wWin 
PROCEDURE tvNodeCreatePopup :
/*------------------------------------------------------------------------------
  Purpose: example to use tvNodePopup procedure with pure4GlTv
  Parameters: 
  Notes: I use this procedure for mulitple demo treeview
     Note that I rely on pcnodeKey to distinguish the different
     sample treeview
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcnodeKey AS CHARACTER  NO-UNDO.

DEFINE VARIABLE nCust     AS INTEGER    NO-UNDO.
DEFINE VARIABLE norder    AS INTEGER    NO-UNDO.

DEFINE VARIABLE cSalesrep  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE icustnum   AS INTEGER    NO-UNDO.
DEFINE VARIABLE ccustname  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iorder     AS INTEGER    NO-UNDO.
DEFINE VARIABLE cparentKey AS CHARACTER  NO-UNDO.

/*========= For text treview sample, nodeKey beings 'n' ================*/
IF pcNodeKey BEGINS "n"
 AND NUM-ENTRIES(pcNodeKey, "=") = 1
 THEN RETURN "Add a child node,MenuAddChildNode,RULE,,Hello World,MenuHelloWorld".


/*======== For data treeview example on salesrep customer order orderline: ========*/
/* popup menu addSalesRep and addCustomer */
IF pcNodeKey BEGINS "sr="
 THEN RETURN "Add Salesrep,MenuAddSR,Add Customer,MenuAddCustomer".

/*-------------- Popup menu addOrder  --------------*/
IF pcNodeKey BEGINS "cust="
 THEN RETURN "Add order,MenuAddOrder".

/*-------------- Popup menu addOrderline --------------*/
IF pcNodeKey BEGINS "order="
 THEN RETURN "Add order line,MenuAddOrderLine".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tvNodeDropEnd wWin 
PROCEDURE tvNodeDropEnd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER pcEvent   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcnodeKey AS CHARACTER  NO-UNDO.

DEFINE VARIABLE mouseX   AS INTEGER    NO-UNDO.
DEFINE VARIABLE mouseY   AS INTEGER    NO-UNDO.
DEFINE VARIABLE cWidgets AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hWidget  AS HANDLE     NO-UNDO.
DEFINE VARIABLE icount   AS INTEGER    NO-UNDO.

mouseX = INT(ENTRY(2,pcEvent)) NO-ERROR.
mouseY = INT(ENTRY(3,pcEvent)) NO-ERROR.

/* Example with the large tree (1000 node) and drag-drop used to move
 a node somewhere else (drop in the treeview itself */
IF pcnodeKey BEGINS "k" THEN DO:
    DEFINE VARIABLE targetKe AS CHARACTER  NO-UNDO.
    targetKe = DYNAMIC-FUNCTION('getNodeLocatedAtXY' IN h_pure4GlTv, mouseX, mouseY).
    
    IF lTraceEvents THEN DO:
        addToEdMsg("Drop end fired in MouseX: " + STRING(mouseX)
         + "  mouseY: " + STRING(mouseY) + "   nodeKey: " + pcnodeKey
         + "~n         => This falls in the following widgets:" + cWidgets
         + "~n         => Detected Target Nodekey: " + targetKe + "~n").
    END.
    
    IF targetKe <> "" AND targetKe <> pcnodeKey THEN
     RUN moveNode IN h_pure4gltv (pcnodeKey, targetKe, "after", "refresh") NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN DYNAMIC-FUNCTION('selectNode' IN h_pure4gltv , pcnodeKey).
    ELSE MESSAGE "This node cannot be moved here!"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* other cases, the drop is done in this container */
ELSE DO:
    /* work out the name of drop target widgets from the handles passed in pcEvent*/
    DO iCount = 4 TO NUM-ENTRIES(pcEvent):
        hWidget = WIDGET-HANDLE(ENTRY(iCount,pcEvent)) NO-ERROR.
        cWidgets = cWidgets + " "
         + IF hWidget:NAME = ?
            THEN (IF CAN-QUERY(hWidget,"SCREEN-VALUE")
                   THEN "SCREEN-VALUE=" + hWidget:SCREEN-VALUE
                   ELSE "?")
            ELSE hWidget:NAME.
    END.
    
    /* if trace enable, then display info in the monitoring editor */
    IF lTraceEvents THEN DO:
        addToEdMsg("Drop end fired in at mouseX: " + STRING(mouseX)
         + "  mouseY: " + STRING(mouseY) + "   nodeKey: " + pcnodeKey + "~n").
        
        IF cWidgets = "" THEN addToEdMsg("         This (X,Y) does not falls in any widget~n").
        ELSE addToEdMsg("         This (X,Y) falls in the following widgets:" + cWidgets + "~n").
    END.
    
    /* at last insert the label of the dragged node into the drop target widget */
    hWidget = ?.
    DO iCount = 4 TO NUM-ENTRIES(pcEvent):
        hWidget = WIDGET-HANDLE(ENTRY(iCount,pcEvent)) NO-ERROR.
        IF NOT CAN-QUERY(hWidget, "SCREEN-VALUE") THEN NEXT.
        IF NOT hWidget:SENSITIVE THEN NEXT. /* otherwise, we give the ability to change a label :o */
    
        DEFINE VARIABLE hNodeBuffer AS HANDLE     NO-UNDO.
        RUN getNodeDetails IN h_pure4gltv
        ( INPUT  pcnodeKey /* CHARACTER */,
          OUTPUT hNodeBuffer /* HANDLE */).
        
        IF NOT VALID-HANDLE (hNodeBuffer) THEN LEAVE.
        
        CASE hWidget:TYPE:
          WHEN "EDITOR" THEN DO:
            hWidget:CURSOR-OFFSET = hWidget:LENGTH + 1.
            hWidget:INSERT-STRING(hNodeBuffer:BUFFER-FIELD("lab"):BUFFER-VALUE + "~n").
          END.
          WHEN "FILL-IN" THEN hWidget:SCREEN-VALUE = hWidget:SCREEN-VALUE + 
            hNodeBuffer:BUFFER-FIELD("lab"):BUFFER-VALUE NO-ERROR.
          OTHERWISE hWidget:SCREEN-VALUE = hNodeBuffer:BUFFER-FIELD("lab"):BUFFER-VALUE NO-ERROR.
        END CASE. /* CASE hWidget:TYPE: */
        APPLY 'VALUE-CHANGED'TO hWidget. /*very important if we want the change of the SCREEN-VALUE
                                          to result in the same as typing */

        DELETE OBJECT hNodeBuffer.
        
        LEAVE. /* one widget is enough ;) */
    END. /* DO iCount = 4 TO NUM-ENTRIES(cWidgets): */
END. /* other cases, the drop is done in this container */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tvnodeEvent wWin 
PROCEDURE tvnodeEvent :
/*------------------------------------------------------------------------------
  Purpose: example to use tvNodeEvent procedure with pure4GlTv
  Parameters: 
  Notes: I use this procedure for mulitple demo treeview
     Note that I rely on pcnodeKey to distinguish the different
     sample treeview
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcEvent   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcnodeKey AS CHARACTER  NO-UNDO.

DEFINE VARIABLE nCust     AS INTEGER    NO-UNDO.
DEFINE VARIABLE norder    AS INTEGER    NO-UNDO.

DEFINE VARIABLE cSalesrep AS CHARACTER  NO-UNDO.
DEFINE VARIABLE icustnum  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iorder AS INTEGER    NO-UNDO.


IF lTraceEvents THEN addToEdMsg(STRING(pcEvent,FILL("X",25)) + pcnodeKey + "~n").
 
CASE pcEvent:
  WHEN "addOnExpand" THEN RUN tvNodeaddOnExpand (pcnodeKey).
  WHEN "select"      THEN RUN tvNodeSelect (pcnodeKey).
  
  WHEN "rightClick"  THEN DO:
      RUN tvNodeCreatePopup (pcnodeKey) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN RETURN RETURN-VALUE.
      ELSE MESSAGE "tvNodeCreatePopup failed with the following message:" RETURN-VALUE
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  
  /* place to handle the Menu event */
  WHEN "MenuAddChildNode"
   OR WHEN "MenuAddSR"
   OR WHEN "MenuAddCustomer"
   OR WHEN "MenuAddOrder"
   OR WHEN "MenuAddOrderLine"
   THEN addToEdMsg("Menu item event fired: " + pcEvent + " for key " + pcnodeKey + "~n").
   
   WHEN "MenuHelloWorld" THEN MESSAGE "Hello World!" SKIP
      "Node key parent of the popup menu item:" + pcNodeKey
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
   
   WHEN "DragBegin" THEN DO:
       /* sample with Large tree (1000 nodes), the node keys are of type "k<n>" 
         by returning "yourself" the treview will be the drop target to move a node
         to another location in the tree */
       IF pcnodeKey BEGINS "k" THEN RETURN "dropOnYourself".
       
       /* see node n4 in small TV */
       IF pcnodeKey = "n4" THEN RETURN "cancelDrag".
       
       /* drop target frame is in another window */
       IF pcnodeKey = "n1" THEN DO:
           /* to test that, use PRO*Tools/run to run C:\BabouSoft\tv4gl\OtherDropTargetWin.w
             with the persistent option before running this test container */
           DEFINE VARIABLE hTargetFrame AS HANDLE     NO-UNDO.
           PUBLISH "getOtherWinTargetFrame" (OUTPUT hTargetFrame).
           IF VALID-HANDLE(hTargetFrame) THEN RETURN STRING(hTargetFrame).
       END.

       /* for the other samle, the target is this container */
       RETURN STRING(FRAME fMain:HANDLE).
   END.
   
   OTHERWISE IF pcEvent BEGINS "DropEnd," THEN RUN tvNodeDropEnd (pcEvent, pcNodeKey).
END CASE.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tvNodeSelect wWin 
PROCEDURE tvNodeSelect :
/*------------------------------------------------------------------------------
  Purpose: example to use tvNodeEvent procedure with pure4GlTv
  Parameters: 
  Notes: I use this procedure for mulitple demo treeview
     Note that I rely on pcnodeKey to distinguish the different
     sample treeview
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcnodeKey AS CHARACTER  NO-UNDO.

DEFINE VARIABLE nCust     AS INTEGER    NO-UNDO.
DEFINE VARIABLE norder    AS INTEGER    NO-UNDO.

DEFINE VARIABLE cSalesrep  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE icustnum   AS INTEGER    NO-UNDO.
DEFINE VARIABLE ccustname  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iorder     AS INTEGER    NO-UNDO.
DEFINE VARIABLE cparentKey AS CHARACTER  NO-UNDO.
DEFINE VARIABLE optn       AS CHARACTER  NO-UNDO.



/*========= For text treview sample, nodeKey beings 'n' ================*/


/*======== For data treeview example on salesrep customer order orderline: ========*/



/*-------------- add more customers to salesrep --------------*/
IF pcNodeKey BEGINS "MoreCust=" THEN DO:
    ASSIGN
     icustnum = INT(ENTRY(2,pcNodeKey,"="))
     cparentKey = DYNAMIC-FUNCTION('getNodeParentKey' IN h_pure4glTv, pcNodeKey)
     cSalesrep = ENTRY(2,cparentKey,"=").
    FIND customer NO-LOCK WHERE customer.cust-num = icustnum.
    cCustName = customer.name.
    
    FOR EACH customer NO-LOCK WHERE
     customer.sales-rep = cSalesrep
     AND customer.name >= cCustName
     BY customer.name:
        nCust = nCust + 1.
        IF nCust > giRowsToBatch THEN DO:
            RUN addNode IN h_pure4gltv ("MoreCust=" + STRING(customer.cust-num)
                                       ,cparentKey
                                       ,"More..."
                                       ,""
                                       ,"InViewPortIfPossible").
            LEAVE.
        END.
        optn = "InViewPortIfPossible".
        IF nCust = 1 THEN optn = optn + CHR(1) + "selected".
        IF CAN-FIND(FIRST order OF customer)
         THEN optn = optn + CHR(1) + "addOnExpand".
        
        RUN addNode IN h_pure4gltv ("cust=" + STRING(customer.cust-num)
                                   ,cParentKey
                                   ,customer.name
                                   ,"treeview/tvpics/smile56.bmp"
                                   ,optn).
    END.  /* for each customer */
    
    RUN deleteNode IN h_Pure4glTv (pcNodeKey, "refresh").
END. /* add more customers to salesrep node */


/*-------------- add more orders to customer --------------*/
IF pcNodeKey BEGINS "MoreOrder=" THEN DO:
    ASSIGN
     iorder = INT(ENTRY(2,pcNodeKey,"="))
     cparentKey = DYNAMIC-FUNCTION('getNodeParentKey' IN h_pure4glTv, pcNodeKey)
     icustnum = INT(ENTRY(2,cparentKey,"=")).
     
    FOR EACH order NO-LOCK WHERE
     order.cust-num = icustnum
     AND order.order-num >= iorder
     BY order.order-num:
        norder = nOrder + 1.
        IF norder > giRowsToBatch THEN DO:
            RUN addNode IN h_pure4gltv ("MoreOrder=" + STRING(order.order-num)
                                       ,cparentKey
                                       ,"More..."
                                       ,""
                                       ,"InViewPortIfPossible").
            LEAVE.
        END.
        optn = "InViewPortIfPossible".
        IF norder = 1 THEN optn = optn + CHR(1) + "selected".
        IF CAN-FIND(FIRST order-line OF order)
         THEN optn = optn + CHR(1) + "addOnExpand".
        
        RUN addNode IN h_pure4gltv ("order=" + STRING(order.order-num)
                                   ,cparentKey
                                   ,STRING(order.order-num) + " (" + STRING(order.order-date) + ")"
                                   ,"treeview/tvpics/book02.bmp"
                                   ,optn).
    END.  /* for each customer */
    RUN deleteNode IN h_Pure4glTv (pcNodeKey, "refresh").
END. /* add customers to salesrep node */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addToEdMsg wWin 
FUNCTION addToEdMsg RETURNS LOGICAL
  (pcTxt AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


  IF edMsg:LENGTH IN FRAME fMain > 31000 THEN edMsg:SCREEN-VALUE =
    SUBSTR(edMsg:SCREEN-VALUE,1000).
       
  edmsg:CURSOR-OFFSET = edmsg:LENGTH + 1.
  edMsg:INSERT-STRING(pcTxt).
  
  RETURN YES.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

