&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/vendorSelector.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Anjly

  Created:02nd May 2021

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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
{methods/defines/sortByDefs.i}
{system/VendorCostProcs.i}

DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcScope AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplIncludeBlankVendor AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipcQuantityUOM AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdDimLength AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdDimWidth AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdDimDepth AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipcDimUOM AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdBasisWeight AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipcBasisWeightUOM AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttVendItemCost. 
DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brVendItemCost

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttVendItemCost

/* Definitions for BROWSE brVendItemCost                                */
&Scoped-define FIELDS-IN-QUERY-brVendItemCost ttVendItemCost.vendorID ttVendItemCost.costPerVendorUOM ttVendItemCost.vendorUOM ttVendItemCost.costSetup ttVendItemCost.costSetup ttVendItemCost.costTotal ttVendItemCost.vendorItem ttVendItemCost.isValid ttVendItemCost.reasonNotValid // ttVendItemCost.note //   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brVendItemCost   
&Scoped-define SELF-NAME brVendItemCost
&Scoped-define QUERY-STRING-brVendItemCost FOR EACH ttVendItemCost ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-brVendItemCost OPEN QUERY {&SELF-NAME} FOR EACH ttVendItemCost ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-brVendItemCost ttVendItemCost
&Scoped-define FIRST-TABLE-IN-QUERY-brVendItemCost ttVendItemCost


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brVendItemCost}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-13 tbShowAll brVendItemCost bOk bCancel ~
fiItem fiHotKey fiTitle fiProgramName 
&Scoped-Define DISPLAYED-OBJECTS tbShowAll fiItem fiHotKey fiTitle lItem ~
lSize lAddress fiProgramName lQuantity lShow 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.29.

DEFINE BUTTON bOk 
     LABEL "Ok" 
     SIZE 15 BY 1.29.

DEFINE VARIABLE fiHotKey AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 23.6 BY 1.1
     BGCOLOR 23 FGCOLOR 0 FONT 22 NO-UNDO.

DEFINE VARIABLE fiItem AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 31.4 BY 1.1
     BGCOLOR 23 FGCOLOR 0 FONT 22 NO-UNDO.

DEFINE VARIABLE fiProgramName AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 86.8 BY 1.1
     BGCOLOR 23 FGCOLOR 0 FONT 22 NO-UNDO.

DEFINE VARIABLE fiTitle AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 58.6 BY 1.1
     BGCOLOR 23 FGCOLOR 0 FONT 22 NO-UNDO.

DEFINE VARIABLE lAddress AS CHARACTER FORMAT "X(256)":U INITIAL "Address:" 
      VIEW-AS TEXT 
     SIZE 10.6 BY .62
     BGCOLOR 23 FGCOLOR 24 FONT 6 NO-UNDO.

DEFINE VARIABLE lItem AS CHARACTER FORMAT "X(256)":U INITIAL "Item:" 
      VIEW-AS TEXT 
     SIZE 7 BY .62
     BGCOLOR 23 FGCOLOR 24 FONT 6 NO-UNDO.

DEFINE VARIABLE lQuantity AS CHARACTER FORMAT "X(256)":U INITIAL "Quantity Required:" 
      VIEW-AS TEXT 
     SIZE 23 BY .62
     BGCOLOR 23 FGCOLOR 24 FONT 6 NO-UNDO.

DEFINE VARIABLE lShow AS CHARACTER FORMAT "X(256)":U INITIAL "Show All" 
      VIEW-AS TEXT 
     SIZE 27.4 BY .62
     BGCOLOR 23 FGCOLOR 24 FONT 6 NO-UNDO.

DEFINE VARIABLE lSize AS CHARACTER FORMAT "X(256)":U INITIAL "Size:" 
      VIEW-AS TEXT 
     SIZE 6 BY .62
     BGCOLOR 23 FGCOLOR 24 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 147 BY 3.81
     BGCOLOR 23 FGCOLOR 24 .

DEFINE VARIABLE tbShowAll AS LOGICAL INITIAL NO 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81
     BGCOLOR 23  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brVendItemCost FOR 
      ttVendItemCost SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brVendItemCost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brVendItemCost C-Win _FREEFORM
  QUERY brVendItemCost NO-LOCK DISPLAY
      ttVendItemCost.vendorID       COLUMN-LABEL "Vendor ID"       
            LABEL-BGCOLOR 14
      ttVendItemCost.costPerVendorUOM    COLUMN-LABEL "Cost"    
             LABEL-BGCOLOR 14  
      ttVendItemCost.vendorUOM    COLUMN-LABEL "UOM"  
             LABEL-BGCOLOR 14   
      ttVendItemCost.costSetup    COLUMN-LABEL "Setup" 
             LABEL-BGCOLOR 14
      ttVendItemCost.costSetup   COLUMN-LABEL "Additional Cost" 
             LABEL-BGCOLOR 14
      ttVendItemCost.costTotal      COLUMN-LABEL "Total Cost"        
             LABEL-BGCOLOR 14
      ttVendItemCost.vendorItem COLUMN-LABEL "Vendor Item"   FORMAT "x(32)":U
             LABEL-BGCOLOR 14 
      ttVendItemCost.isValid      COLUMN-LABEL "Valid"        
             LABEL-BGCOLOR 14
      ttVendItemCost.reasonNotValid COLUMN-LABEL "Invalid Reason"   FORMAT "x(32)":U
             LABEL-BGCOLOR 14 
          //  ttVendItemCost.note COLUMN-LABEL "Note"   FORMAT "x(32)":U
           // WIDTH 24 LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 147.4 BY 15.19
         FONT 34 ROW-HEIGHT-CHARS .9 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     tbShowAll AT ROW 3.62 COL 116.2 WIDGET-ID 336
     brVendItemCost AT ROW 5.29 COL 148 RIGHT-ALIGNED WIDGET-ID 200
     bOk AT ROW 21.05 COL 51.2 WIDGET-ID 342
     bCancel AT ROW 21.05 COL 75.2 WIDGET-ID 344
     fiItem AT ROW 1.86 COL 11.6 NO-LABEL WIDGET-ID 66
     fiHotKey AT ROW 1.86 COL 48.4 COLON-ALIGNED NO-LABEL WIDGET-ID 330
     fiTitle AT ROW 1.86 COL 84.2 COLON-ALIGNED NO-LABEL WIDGET-ID 332
     lItem AT ROW 2.1 COL 4.4 NO-LABEL WIDGET-ID 64
     lSize AT ROW 2.1 COL 41.6 COLON-ALIGNED NO-LABEL WIDGET-ID 326
     lAddress AT ROW 2.1 COL 74.8 NO-LABEL WIDGET-ID 328
     fiProgramName AT ROW 3.43 COL 25.2 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     lQuantity AT ROW 3.67 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     lShow AT ROW 3.71 COL 119.8 NO-LABEL WIDGET-ID 338
     RECT-13 AT ROW 1.24 COL 2 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149.8 BY 21.81
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


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
         TITLE              = "Vendor Selector"
         HEIGHT             = 21.81
         WIDTH              = 149.8
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 199.8
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 199.8
         MAX-BUTTON         = NO
         RESIZE             = NO
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brVendItemCost tbShowAll DEFAULT-FRAME */
/* SETTINGS FOR BROWSE brVendItemCost IN FRAME DEFAULT-FRAME
   ALIGN-R                                                              */
ASSIGN 
       brVendItemCost:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       fiHotKey:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiItem IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       fiItem:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiProgramName:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiTitle:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lAddress IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lItem IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lQuantity IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lShow IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lSize IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brVendItemCost
/* Query rebuild information for BROWSE brVendItemCost
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttVendItemCost ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brVendItemCost */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Vendor Selector */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vendor Selector */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bCancel C-Win
ON CHOOSE OF bCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bOk C-Win
ON CHOOSE OF bOk IN FRAME DEFAULT-FRAME /* Ok */
DO:
    IF AVAILABLE ttVendItemCost THEN
    ttVendItemCost.isSelected = TRUE.
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brVendItemCost
&Scoped-define SELF-NAME brVendItemCost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brVendItemCost C-Win
ON START-SEARCH OF brVendItemCost IN FRAME DEFAULT-FRAME
DO:
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
        
        IF cColumnLabel EQ cSaveLabel THEN
            lAscending = NOT lAscending.
        IF VALID-HANDLE(hSaveLabel) THEN
            hSaveLabel:LABEL-BGCOLOR = ?.
    
        ASSIGN
            hColumnLabel               = {&BROWSE-NAME}:CURRENT-COLUMN
            hColumnLabel:LABEL-BGCOLOR = 14
            hSaveLabel                 = hColumnLabel
            cSaveLabel                 = cColumnLabel
            .
        RUN pReopenBrowse.
    END.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbShowAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbShowAll C-Win
ON VALUE-CHANGED OF tbShowAll IN FRAME DEFAULT-FRAME
DO:
    IF tbShowAll:CHECKED THEN 
    {&OPEN-QUERY-brVendItemCost}
    ELSE 
    OPEN QUERY brVendItemCost FOR EACH ttVendItemCost WHERE ttVendItemCost.isValid = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
{methods/template/brwcustom.i}
{sys/inc/f3helpw.i}
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
bOk:LOAD-IMAGE("Graphics/32x32/ok.png").
bCancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
           
    RUN BuildVendItemCosts(
     INPUT  ipcCompany,
     INPUT  ipcItemID,
     INPUT  ipcItemType,
     INPUT  ipcScope,
     INPUT  iplIncludeBlankVendor,
     INPUT  ipcEstimateNo,
     INPUT  ipiFormNo,
     INPUT  ipiBlankNo,
     INPUT  ipdQuantity,
     INPUT  ipcQuantityUOM,
     INPUT  ipdDimLength,
     INPUT  ipdDimWidth,
     INPUT  ipdDimDepth,
     INPUT  ipcDimUOM,
     INPUT  ipdBasisWeight,
     INPUT  ipcBasisWeightUOM,
     OUTPUT  TABLE  ttVendItemCost,
     OUTPUT  oplError,
     OUTPUT  opcMessage).
     
     
    RUN enable_UI.
    fiItem:screen-value IN FRAME {&frame-name} = ipcItemID.
    IF tbShowAll:CHECKED THEN 
        {&OPEN-QUERY-brVendItemCost}
    ELSE 
        OPEN QUERY brVendItemCost FOR EACH ttVendItemCost WHERE ttVendItemCost.isValid = TRUE.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

&Scoped-define sdBrowseName brVendItemCost
{methods/sortByProc.i "pByVendorID" "ttVendItemCost.vendorID"}
{methods/sortByProc.i "pByCostPerVendorUOM" "ttVendItemCost.costPerVendorUOM"}
{methods/sortByProc.i "pByCostSetup" "ttVendItemCost.costSetup"}
{methods/sortByProc.i "pBycostTotal" "ttVendItemCost.costTotal"}
{methods/sortByProc.i "pByVendorItem" "ttVendItemCost.vendorItem"}

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
  DISPLAY tbShowAll fiItem fiHotKey fiTitle lItem lSize lAddress fiProgramName 
          lQuantity lShow 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-13 tbShowAll brVendItemCost bOk bCancel fiItem fiHotKey fiTitle 
         fiProgramName 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReOpenBrowse C-Win 
PROCEDURE pReOpenBrowse :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    CASE cColumnLabel:
        WHEN "vendorID" THEN
            RUN pByVendorID.
        WHEN "costPerVendorUOM" THEN
            RUN pByCostPerVendorUOM.
        WHEN "costSetup" THEN
            RUN pByCostSetup.
        WHEN "costTotal" THEN
            RUN pBycostTotal.
        WHEN "vendorItem" THEN
            RUN pByVendorItem.       
        OTHERWISE
            {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

