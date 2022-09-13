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
//{methods/prgsecur.i}
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
DEFINE INPUT PARAMETER ipcAdderList      AS CHARACTER EXTENT 6 NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttVendItemCost. 
DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

DEFINE VARIABLE cAdderValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE iIndex      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cVendorCostMatrixUseEstimate AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-item FOR ITEM.

RUN spGetSettingByName ("VendorCostMatrixUseEstimate", OUTPUT cVendorCostMatrixUseEstimate).

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
&Scoped-define FIELDS-IN-QUERY-brVendItemCost ttVendItemCost.vendorID ttvendItemCost.estimateNo + (IF ttvendItemCost.formNo = 0 THEN '' ELSE ('-' + string(ttvendItemCost.formNo ) )) + (IF ttvendItemCost.blankNo = 0 THEN '' ELSE ('-' + string(ttvendItemCost.blankNo ) )) ttVendItemCost.costPerVendorUOM ttVendItemCost.vendorUOM ttVendItemCost.costSetup  ttVendItemCost.costSetup ttVendItemCost.costTotal ttVendItemCost.vendorItem ttVendItemCost.effectiveDate ttVendItemCost.expirationDate ttVendItemCost.isValid ttVendItemCost.reasonNotValid
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
fiItem fiTitle fiLen fiWid fiDep fiQuantity fiUOM fiAdders 
&Scoped-Define DISPLAYED-OBJECTS tbShowAll fiItem fiLen fiWid fiDep lItem ~
lSize x x-2 fiQuantity fiUOM lQuantity lShow fiAdders cAdders 

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

DEFINE VARIABLE cAdders AS CHARACTER FORMAT "X(256)":U INITIAL "Adders:" 
      VIEW-AS TEXT 
     SIZE 9 BY .62
     BGCOLOR 23 FGCOLOR 24 FONT 6 NO-UNDO.

DEFINE VARIABLE fiAdders AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 144 BY 1.1
     BGCOLOR 23 FGCOLOR 24 FONT 0 NO-UNDO.

DEFINE VARIABLE fiDep AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 5.6 BY .71
     BGCOLOR 23 FGCOLOR 0 FONT 22 NO-UNDO.

DEFINE VARIABLE fiItem AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 31.4 BY 1.1
     BGCOLOR 23 FGCOLOR 0 FONT 22 NO-UNDO.

DEFINE VARIABLE fiLen AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 5.6 BY .71
     BGCOLOR 23 FGCOLOR 0 FONT 22 NO-UNDO.

DEFINE VARIABLE fiQuantity AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 17.8 BY 1.1
     BGCOLOR 23 FGCOLOR 0 FONT 22 NO-UNDO.

DEFINE VARIABLE fiTitle AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 58.6 BY 1.1
     BGCOLOR 23 FGCOLOR 0 FONT 22 NO-UNDO.

DEFINE VARIABLE fiUOM AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 17.8 BY 1.1
     BGCOLOR 23 FGCOLOR 0 FONT 22 NO-UNDO.

DEFINE VARIABLE fiWid AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 5.6 BY .71
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
     SIZE 11.2 BY .62
     BGCOLOR 23 FGCOLOR 24 FONT 6 NO-UNDO.

DEFINE VARIABLE lSize AS CHARACTER FORMAT "X(256)":U INITIAL "Size:" 
      VIEW-AS TEXT 
     SIZE 6 BY .62
     BGCOLOR 23 FGCOLOR 24 FONT 6 NO-UNDO.

DEFINE VARIABLE x AS CHARACTER FORMAT "X(256)":U INITIAL "X" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .62
     BGCOLOR 23 FGCOLOR 24 FONT 6 NO-UNDO.

DEFINE VARIABLE x-2 AS CHARACTER FORMAT "X(256)":U INITIAL "X" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .62
     BGCOLOR 23 FGCOLOR 24 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 157 BY 5.48
     BGCOLOR 23 FGCOLOR 24 .

DEFINE VARIABLE tbShowAll AS LOGICAL INITIAL no 
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
      ttVendItemCost.vendorID           COLUMN-LABEL "Vendor ID"       
            LABEL-BGCOLOR 14    FORMAT "x(10)"
      ttvendItemCost.estimateNo +  
        (IF  ttvendItemCost.formNo  =  0 THEN '' ELSE  ('-' + string(ttvendItemCost.formNo ) )) +
        (IF  ttvendItemCost.blankNo  =  0 THEN '' ELSE  ('-' + string(ttvendItemCost.blankNo ) )) LABEL-BGCOLOR 14  COLUMN-LABEL "Estimate" 
      ttVendItemCost.costPerVendorUOM   COLUMN-LABEL "Cost"    
             LABEL-BGCOLOR 14   FORMAT "->>,>>9.99"  
      ttVendItemCost.vendorUOM    COLUMN-LABEL "UOM"  
             LABEL-BGCOLOR 14   FORMAT "x(5)"    
      ttVendItemCost.costSetup    COLUMN-LABEL "Setup" 
             LABEL-BGCOLOR 14   FORMAT "->>,>>9.99" 
     /* ttVendItemCost.costSetup   COLUMN-LABEL "Additional Cost" 
             LABEL-BGCOLOR 14*/
      ttVendItemCost.costTotal      COLUMN-LABEL "Total Cost"        
             LABEL-BGCOLOR 14   FORMAT "->,>>>,>>9.99" 
      ttVendItemCost.vendorItem COLUMN-LABEL "Vendor Item"   
             LABEL-BGCOLOR 14   FORMAT "x(15)"  
      ttVendItemCost.effectiveDate      COLUMN-LABEL "Effective"        
             LABEL-BGCOLOR 14   FORMAT "99/99/9999" 
      ttVendItemCost.expirationDate COLUMN-LABEL "Expiration"   
             LABEL-BGCOLOR 14   FORMAT "99/99/9999"  
      ttVendItemCost.isValid      COLUMN-LABEL "Valid"        
             LABEL-BGCOLOR 14   FORMAT "Yes/No" 
      ttVendItemCost.reasonNotValid COLUMN-LABEL "Invalid Reason"  
             LABEL-BGCOLOR 14   FORMAT "x(100)"  
          //  ttVendItemCost.note COLUMN-LABEL "Note"   FORMAT "x(32)":U
           // WIDTH 24 LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 158.4 BY 13.52
         FONT 34 ROW-HEIGHT-CHARS .9.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     tbShowAll AT ROW 3.62 COL 141 WIDGET-ID 336
     brVendItemCost AT ROW 6.95 COL 159 RIGHT-ALIGNED WIDGET-ID 200
     bOk AT ROW 21.05 COL 61.6 WIDGET-ID 342
     bCancel AT ROW 21.05 COL 85.6 WIDGET-ID 344
     fiItem AT ROW 1.86 COL 11.6 NO-LABEL WIDGET-ID 66
     fiTitle AT ROW 1.86 COL 84.2 COLON-ALIGNED NO-LABEL WIDGET-ID 332
     fiLen AT ROW 2.05 COL 47.6 COLON-ALIGNED NO-LABEL WIDGET-ID 348
     fiWid AT ROW 2.05 COL 56.8 COLON-ALIGNED NO-LABEL WIDGET-ID 346
     fiDep AT ROW 2.05 COL 65.6 COLON-ALIGNED NO-LABEL WIDGET-ID 350
     lItem AT ROW 2.1 COL 4.4 NO-LABEL WIDGET-ID 64
     lSize AT ROW 2.1 COL 41.6 COLON-ALIGNED NO-LABEL WIDGET-ID 326
     x AT ROW 2.1 COL 53.8 COLON-ALIGNED NO-LABEL WIDGET-ID 354
     x-2 AT ROW 2.1 COL 63 COLON-ALIGNED NO-LABEL WIDGET-ID 356
     lAddress AT ROW 2.1 COL 74.8 NO-LABEL WIDGET-ID 328
     fiQuantity AT ROW 3.38 COL 25.2 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     fiUOM AT ROW 3.38 COL 46.8 COLON-ALIGNED NO-LABEL WIDGET-ID 358
     lQuantity AT ROW 3.67 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     lShow AT ROW 3.71 COL 144.6 NO-LABEL WIDGET-ID 338
     fiAdders AT ROW 5.05 COL 12 COLON-ALIGNED NO-LABEL
     cAdders AT ROW 5.29 COL 2 COLON-ALIGNED NO-LABEL
     RECT-13 AT ROW 1.24 COL 2 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 161.2 BY 21.81
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
         WIDTH              = 161.2
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 199.8
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 199.8
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
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

/* SETTINGS FOR FILL-IN cAdders IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       cAdders:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       fiAdders:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
       fiAdders:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiDep:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiItem IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       fiItem:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiLen:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiQuantity:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiTitle IN FRAME DEFAULT-FRAME
   NO-DISPLAY                                                           */
ASSIGN 
       fiTitle:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
       fiTitle:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiUOM:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiWid:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lAddress IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       lAddress:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN lItem IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lQuantity IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lShow IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lSize IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN x IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN x-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

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
    ASSIGN 
        oplError = TRUE 
        opcMessage = "User cancelled the vendor selection.".
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bOk C-Win
ON CHOOSE OF bOk IN FRAME DEFAULT-FRAME /* Ok */
DO:
    IF AVAILABLE ttVendItemCost THEN
    DO:
        IF ttVendItemCost.IsValid = FALSE THEN DO:
            MESSAGE "This Vendor cost is invalid." SKIP
                "Reason: " + ttVendItemCost.Reason SKIP
                "Please select a valid Vendor Cost."        
                VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END. 
        ELSE IF PROGRAM-NAME(9) EQ "oe/ordfrest.p" 
        OR INDEX(PROGRAM-NAME(9),"oe/d-oeitem.w") NE 0 
        OR INDEX(PROGRAM-NAME(9),"panels/p-job.w") NE 0 
        OR INDEX(PROGRAM-NAME(9),"browsers/probe.w") NE 0 
        OR INDEX(PROGRAM-NAME(7),"oe/p-oehold.w") NE 0 
        THEN 
        DO:
            MESSAGE 
                "Creating PO for vendor: " + ttVendItemCost.vendorID + "." skip
                "Is this correct?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lCorrect AS LOG.
            IF NOT lCorrect THEN RETURN NO-APPLY.
            ASSIGN 
                oplError = FALSE  
                opcMessage = "".
            ttVendItemCost.isSelected = TRUE.
            APPLY 'CLOSE' TO THIS-PROCEDURE.
        END.
        ELSE DO:
            MESSAGE 
                PROGRAM-NAME(1) skip
                PROGRAM-NAME(2) skip
                PROGRAM-NAME(3) skip
                PROGRAM-NAME(4) skip
                PROGRAM-NAME(5) skip
                PROGRAM-NAME(6) skip
                PROGRAM-NAME(7) skip
                PROGRAM-NAME(8) skip
                PROGRAM-NAME(9) 
                VIEW-AS ALERT-BOX.
        END.        
                
    END.
    ELSE DO:
        MESSAGE 
            "No vendor was selected."
            VIEW-AS ALERT-BOX.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brVendItemCost
&Scoped-define SELF-NAME brVendItemCost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brVendItemCost C-Win
ON MOUSE-SELECT-CLICK OF brVendItemCost IN FRAME DEFAULT-FRAME
DO:
    APPLY 'value-changed' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brVendItemCost C-Win
ON MOUSE-SELECT-DBLCLICK OF brVendItemCost IN FRAME DEFAULT-FRAME
DO:
    APPLY 'value-changed' TO SELF.
    APPLY 'CHOOSE' TO bOK IN FRAME default-frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
    {&OPEN-QUERY-brVendItemCost}
    
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

    RUN BuildVendItemCostsWithAdders(
     INPUT  ipcCompany,
     INPUT  ipcItemID,
     INPUT  ipcItemType,
     INPUT  "All", //ipcScope,
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
     INPUT  ipcAdderList,
     OUTPUT  TABLE  ttVendItemCost,
     OUTPUT  oplError,
     OUTPUT  opcMessage).
     
     FOR EACH ttVendItemCost:
       ASSIGN ttVendItemCost.isSelected = NO.
     END.
     
    RUN enable_UI.

    ASSIGN
        fiItem:screen-value     = ipcItemID
        fiLen:SCREEN-VALUE      = STRING(ipdDimLength)
        fiWid:SCREEN-VALUE      = STRING(ipdDimWidth)
        fiDep:SCREEN-VALUE      = STRING(ipdDimDepth)
        fiQuantity:SCREEN-VALUE = STRING(ipdQuantity)
        fiUOM:SCREEN-VALUE      = ipcQuantityUOM
        .
 
    IF CAN-FIND(FIRST ttVendItemCost WHERE ttVendItemCost.isValid = TRUE) THEN 
    DO:
        DO iIndex = 1 TO EXTENT(ipcAdderList):
            IF ipcAdderList[iIndex] <> "" THEN
            DO:
                FOR FIRST bf-item NO-LOCK 
                    WHERE bf-item.company = ipcCompany
                      AND bf-item.i-no    = ipcAdderList[iIndex]: 
                          
                    IF cAdderValue = "" THEN 
                        cAdderValue = ipcAdderList[iIndex] + " - " + bf-item.i-name.
                    ELSE  
                        cAdderValue = cAdderValue + ", " + ipcAdderList[iIndex] + " - " + bf-item.i-name.
                END.
            END.
        END.
        IF cAdderValue <> "" THEN
            fiAdders:SCREEN-VALUE = cAdderValue.
    END.
    IF cAdderValue = "" THEN 
        cAdders:HIDDEN = TRUE.
        
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

&Scoped-define sdBrowseName brVendItemCost
{methods/sortByProc.i "pByVendorID" "ttVendItemCost.vendorID"}
{methods/sortByProc.i "pByCostPerVendorUOM" "ttVendItemCost.costPerVendorUOM"}
{methods/sortByProc.i "pByVendorUOM" "ttVendItemCost.vendorUOM"}
{methods/sortByProc.i "pByCostSetup" "ttVendItemCost.costSetup"}
{methods/sortByProc.i "pBycostTotal" "ttVendItemCost.costTotal"}
{methods/sortByProc.i "pByVendorItem" "ttVendItemCost.vendorItem"}
{methods/sortByProc.i "pByEffectiveDate" "ttVendItemCost.effectiveDate"}
{methods/sortByProc.i "pByExpirationDate" "ttVendItemCost.expirationDate"}
{methods/sortByProc.i "pByIsValid" "ttVendItemCost.isValid"}

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
  DISPLAY tbShowAll fiItem fiLen fiWid fiDep lItem lSize x x-2 fiQuantity fiUOM 
          lQuantity lShow fiAdders cAdders 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-13 tbShowAll brVendItemCost bOk bCancel fiItem fiTitle fiLen 
         fiWid fiDep fiQuantity fiUOM fiAdders 
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
        WHEN "vendorUOM" THEN
            RUN pByVendorUOM.
        WHEN "costSetup" THEN
            RUN pByCostSetup.
        WHEN "costTotal" THEN
            RUN pBycostTotal.
        WHEN "vendorItem" THEN
            RUN pByVendorItem.
        WHEN "effectiveDate" THEN
            RUN pByEffectiveDate.
        WHEN "expirationDate" THEN
            RUN pByExpirationDate.
        WHEN "isValid" THEN
            RUN pByIsValid.       
        OTHERWISE
            {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

