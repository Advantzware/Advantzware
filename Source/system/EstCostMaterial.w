&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/estCostMaterial.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Anjly

  Created:04th Jun 2021

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
DEFINE INPUT PARAMETER ipiEstCostHeaderID   AS INTEGER      NO-UNDO.
DEFINE OUTPUT PARAMETER oplVendorUpdated    AS LOGICAL      NO-UNDO. 
/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{methods/defines/sortByDefs.i}
{system/VendorCostProcs.i}

//DEFINE VARIABLE  ipiEstCostHeaderID AS INTEGER NO-UNDO.
ipiEstCostHeaderID = 7.

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
&Scoped-define INTERNAL-TABLES estCostMaterial

/* Definitions for BROWSE brVendItemCost                                */
&Scoped-define FIELDS-IN-QUERY-brVendItemCost estCostMaterial.formNo estCostMaterial.blankNo estCostMaterial.itemID estCostMaterial.vendorID estCostMaterial.costPerUOM estCostMaterial.costSetup   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brVendItemCost   
&Scoped-define SELF-NAME brVendItemCost
&Scoped-define QUERY-STRING-brVendItemCost FOR EACH estCostMaterial where estCostMaterial.estCostHeaderID = ipiEstCostHeaderID and estCostMaterial.isPrimarySubstrate ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-brVendItemCost OPEN QUERY {&SELF-NAME} FOR EACH estCostMaterial where estCostMaterial.estCostHeaderID = ipiEstCostHeaderID and estCostMaterial.isPrimarySubstrate ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-brVendItemCost estCostMaterial
&Scoped-define FIRST-TABLE-IN-QUERY-brVendItemCost estCostMaterial


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brVendItemCost}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brVendItemCost bOk 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bOk 
     LABEL "Select Vendor" 
     SIZE 15 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brVendItemCost FOR 
      estCostMaterial SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brVendItemCost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brVendItemCost C-Win _FREEFORM
  QUERY brVendItemCost NO-LOCK DISPLAY
      estCostMaterial.formNo    COLUMN-LABEL "Form"       
            LABEL-BGCOLOR 14
      estCostMaterial.blankNo COLUMN-LABEL "Blank"    
             LABEL-BGCOLOR 14  
      estCostMaterial.itemID COLUMN-LABEL "Item ID"  
             LABEL-BGCOLOR 14   
      estCostMaterial.vendorID COLUMN-LABEL "Best Vendor" 
             LABEL-BGCOLOR 14
      estCostMaterial.costPerUOM COLUMN-LABEL "Per UOM Cost" 
             LABEL-BGCOLOR 14
      estCostMaterial.costSetup COLUMN-LABEL "Setup Cost"        
             LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 147.4 BY 15.19
         FONT 34 ROW-HEIGHT-CHARS .9 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brVendItemCost AT ROW 1.29 COL 148 RIGHT-ALIGNED WIDGET-ID 200
     bOk AT ROW 17 COL 64 WIDGET-ID 342
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149.8 BY 17.71
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
         HEIGHT             = 17.71
         WIDTH              = 149.8
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
/* BROWSE-TAB brVendItemCost 1 DEFAULT-FRAME */
/* SETTINGS FOR BROWSE brVendItemCost IN FRAME DEFAULT-FRAME
   ALIGN-R                                                              */
ASSIGN 
       brVendItemCost:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brVendItemCost
/* Query rebuild information for BROWSE brVendItemCost
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH estCostMaterial where estCostMaterial.estCostHeaderID = ipiEstCostHeaderID and estCostMaterial.isPrimarySubstrate ~{&SORTBY-PHRASE}.
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


&Scoped-define SELF-NAME bOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bOk C-Win
ON CHOOSE OF bOk IN FRAME DEFAULT-FRAME /* Select Vendor */
DO:
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE gcScopeRMOverride  AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - RM Override".
    DEFINE VARIABLE gcScopeFGEstimated AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - FG Estimated". 
    IF AVAILABLE estCostMaterial THEN
    RUN system/vendorcostSelector.w(
     INPUT  estCostMaterial.company, //ipcCompany ,
     INPUT  estCostMaterial.itemID ,
     INPUT  IF isPurchasedFG THEN "FG" ELSE "RM", //ipcItemType ,
     INPUT  IF isPurchasedFG THEN gcScopeFGEstimated ELSE gcScopeRMOverride, //ipcScope ,
     INPUT  "Yes", //iplIncludeBlankVendor ,
     INPUT  estCostMaterial.estimateNo, //ipcEstimateNo,
     INPUT  estCostMaterial.formNo, //ipiFormNo,
     INPUT  estCostMaterial.blankNo, //ipiBlankNo,
     INPUT  0, //ipdQuantity ,
     INPUT  estCostMaterial.quantityUOM, //ipcQuantityUOM ,
     INPUT  estCostMaterial.dimLength, //ipdDimLength ,
     INPUT  estCostMaterial.dimWidth, //ipdDimWidth ,
     INPUT  estCostMaterial.dimDepth, //ipdDimDepth ,
     INPUT  estCostMaterial.dimUOM, //ipcDimUOM ,
     INPUT  estCostMaterial.basisWeight, //ipdBasisWeight ,
     INPUT  estCostMaterial.basisWeightUOM, //ipcBasisWeightUOM ,
     OUTPUT  TABLE ttVendItemCost,
     OUTPUT  lError ,
     OUTPUT  cMessage 
    ).
    
    FOR FIRST ttVendItemCost WHERE ttVendItemCost.isSelected :
        FIND CURRENT estCostMaterial EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE estCostMaterial THEN 
        DO:          
            estCostMaterial.vendorId = ttVendItemCost.vendorID.
            oplVendorUpdated = TRUE.
        END.
        FIND CURRENT estCostMaterial NO-LOCK NO-ERROR.  
    END.
    {&OPEN-QUERY-brVendItemCost}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brVendItemCost
&Scoped-define SELF-NAME brVendItemCost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brVendItemCost C-Win
ON START-SEARCH OF brVendItemCost IN FRAME DEFAULT-FRAME
DO:
   /* IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
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
    END.    */   
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

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
/*
&Scoped-define sdBrowseName brVendItemCost
{methods/sortByProc.i "pByVendorID" "ttVendItemCost.vendorID"}
{methods/sortByProc.i "pByCostPerVendorUOM" "ttVendItemCost.costPerVendorUOM"}
{methods/sortByProc.i "pByCostSetup" "ttVendItemCost.costSetup"}
{methods/sortByProc.i "pBycostTotal" "ttVendItemCost.costTotal"}
{methods/sortByProc.i "pByVendorItem" "ttVendItemCost.vendorItem"}*/

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
  ENABLE brVendItemCost bOk 
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

