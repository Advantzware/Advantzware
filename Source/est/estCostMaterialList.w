&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/estCostMaterialList.w

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
{est/ttEstCostHeaderToCalc.i}
{methods/defines/hndldefs.i}
{methods/defines/sortByDefs.i}
{system/VendorCostProcs.i}
{custom/globdefs.i}
{est\RecostBoardEst.i}
{est\ttEstimateCalc.i}


/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttEstCostHeaderToCalc. 
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttEstCostMaterial.
DEFINE INPUT PARAMETER TABLE FOR ttEstCostHeader.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
 

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brEstCostMaterial
&Scoped-define SORTBY-PHRASE BY ttEstCostHeader.quantityMaster BY ttEstCostMaterial.formNo

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttEstCostHeaderToCalc ttEstCostHeader ~
ttEstCostMaterial

/* Definitions for BROWSE brEstCostMaterial                             */
&Scoped-define FIELDS-IN-QUERY-brEstCostMaterial ttEstCostHeader.quantityMaster ttEstCostMaterial.formNo ttEstCostMaterial.blankNo ttEstCostMaterial.itemID ttEstCostMaterial.vendorID ttEstCostMaterial.quantityRequiredTotal ttEstCostMaterial.quantityUOM ttEstCostMaterial.costPerUOM ttEstCostMaterial.costUOM ttEstCostMaterial.costSetup ttEstCostMaterial.costTotal   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brEstCostMaterial   
&Scoped-define SELF-NAME brEstCostMaterial
&Scoped-define QUERY-STRING-brEstCostMaterial FOR EACH ttEstCostHeaderToCalc, ~
           FIRST ttEstCostHeader NO-LOCK     WHERE ttEstCostHeader.estCostHeaderID EQ ttEstCostHeaderToCalc.iEstCostHeaderID, ~
           EACH ttEstCostMaterial NO-LOCK     WHERE ttEstCostMaterial.estCostHeaderID EQ ttEstCostHeader.estCostHeaderID     AND (ttEstCostMaterial.isPrimarySubstrate OR ttEstCostMaterial.isPurchasedFG) ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-brEstCostMaterial OPEN QUERY {&SELF-NAME} FOR EACH ttEstCostHeaderToCalc, ~
           FIRST ttEstCostHeader NO-LOCK     WHERE ttEstCostHeader.estCostHeaderID EQ ttEstCostHeaderToCalc.iEstCostHeaderID, ~
           EACH ttEstCostMaterial NO-LOCK     WHERE ttEstCostMaterial.estCostHeaderID EQ ttEstCostHeader.estCostHeaderID     AND (ttEstCostMaterial.isPrimarySubstrate OR ttEstCostMaterial.isPurchasedFG) ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-brEstCostMaterial ttEstCostHeaderToCalc ~
ttEstCostHeader ttEstCostMaterial
&Scoped-define FIRST-TABLE-IN-QUERY-brEstCostMaterial ttEstCostHeaderToCalc
&Scoped-define SECOND-TABLE-IN-QUERY-brEstCostMaterial ttEstCostHeader
&Scoped-define THIRD-TABLE-IN-QUERY-brEstCostMaterial ttEstCostMaterial


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brEstCostMaterial}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-13 brEstCostMaterial bOk ~
btnCopyVendorToAll btnRecostboard bCancel fiEstimateNo 
&Scoped-Define DISPLAYED-OBJECTS fiEstimateNo estimate 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bCancel 
     LABEL "Calculate" 
     SIZE 16 BY 1.38.

DEFINE BUTTON bOk 
     LABEL "Choose Vendor" 
     SIZE 17.6 BY 1.38.

DEFINE BUTTON btnCopyVendorToAll 
     LABEL "Copy Vendor to All" 
     SIZE 20 BY 1.38.

DEFINE BUTTON btnRecostboard 
     LABEL "Recost Board" 
     SIZE 18.6 BY 1.38.

DEFINE VARIABLE estimate AS CHARACTER FORMAT "X(8)":U INITIAL "Estimate#:" 
      VIEW-AS TEXT 
     SIZE 12.6 BY .62
     BGCOLOR 23 FGCOLOR 24 FONT 6 NO-UNDO.

DEFINE VARIABLE fiEstimateNo AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 17.2 BY 1.1
     BGCOLOR 23 FGCOLOR 0 FONT 22 NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 77 BY 2.14
     BGCOLOR 23 FGCOLOR 24 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brEstCostMaterial FOR 
      ttEstCostHeaderToCalc, 
      ttEstCostHeader, 
      ttEstCostMaterial SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brEstCostMaterial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brEstCostMaterial C-Win _FREEFORM
  QUERY brEstCostMaterial NO-LOCK DISPLAY
      ttEstCostHeader.quantityMaster              COLUMN-LABEL "Master Qty"
            LABEL-BGCOLOR 14    FORMAT '>,>>>,>>9'
      ttEstCostMaterial.formNo                    COLUMN-LABEL "Form"       
            LABEL-BGCOLOR 14    FORMAT '>9'
      ttEstCostMaterial.blankNo                   COLUMN-LABEL "Blank"    
             LABEL-BGCOLOR 14   FORMAT ">9"
      ttEstCostMaterial.itemID                    COLUMN-LABEL "Item ID"  
             LABEL-BGCOLOR 14   FORMAT "x(20)" 
      ttEstCostMaterial.vendorID                  COLUMN-LABEL "Current Vendor" 
             LABEL-BGCOLOR 14   FORMAT "x(10)"
      ttEstCostMaterial.quantityRequiredTotal     COLUMN-LABEL "Quantity Required" 
             LABEL-BGCOLOR 14   FORMAT "->>,>>9.99"
      ttEstCostMaterial.quantityUOM COLUMN-LABEL "Quantity UOM" 
             LABEL-BGCOLOR 14   FORMAT "x(5)"  
      ttEstCostMaterial.costPerUOM                COLUMN-LABEL "Per UOM Cost" 
             LABEL-BGCOLOR 14   FORMAT "->>,>>9.99"
      ttEstCostMaterial.costUOM                   COLUMN-LABEL "Cost UOM"        
             LABEL-BGCOLOR 14   FORMAT "x(5)"          
      ttEstCostMaterial.costSetup                 COLUMN-LABEL "Setup Cost"        
             LABEL-BGCOLOR 14   FORMAT "->>,>>9.99"
      ttEstCostMaterial.costTotal                COLUMN-LABEL "Total Cost" 
             LABEL-BGCOLOR 14   FORMAT "->,>>>,>>9.99"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 169.8 BY 15.19
         FONT 34 ROW-HEIGHT-CHARS .9.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brEstCostMaterial AT ROW 3.67 COL 171 RIGHT-ALIGNED WIDGET-ID 200
     bOk AT ROW 19.19 COL 3 WIDGET-ID 342
     btnCopyVendorToAll AT ROW 19.19 COL 22.4 WIDGET-ID 358     
     btnRecostboard AT ROW 19.19 COL 44.2 WIDGET-ID 360
     bCancel AT ROW 19.19 COL 155.8 WIDGET-ID 356
     fiEstimateNo AT ROW 1.67 COL 17.8 NO-LABEL WIDGET-ID 66
     estimate AT ROW 1.91 COL 4.4 NO-LABEL WIDGET-ID 64
     RECT-13 AT ROW 1.24 COL 2 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 171.8 BY 19.76
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
         TITLE              = "Estimate cost material"
         HEIGHT             = 19.76
         WIDTH              = 171.8
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
/* BROWSE-TAB brEstCostMaterial RECT-13 DEFAULT-FRAME */
/* SETTINGS FOR BROWSE brEstCostMaterial IN FRAME DEFAULT-FRAME
   ALIGN-R                                                              */
ASSIGN 
       brEstCostMaterial:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR FILL-IN estimate IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiEstimateNo IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       fiEstimateNo:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brEstCostMaterial
/* Query rebuild information for BROWSE brEstCostMaterial
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
FOR EACH ttEstCostHeaderToCalc,
    FIRST ttEstCostHeader NO-LOCK
    WHERE ttEstCostHeader.estCostHeaderID EQ ttEstCostHeaderToCalc.iEstCostHeaderID,
    EACH ttEstCostMaterial NO-LOCK
    WHERE ttEstCostMaterial.estCostHeaderID EQ ttEstCostHeader.estCostHeaderID
    AND (ttEstCostMaterial.isPrimarySubstrate OR ttEstCostMaterial.isPurchasedFG) ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brEstCostMaterial */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Estimate cost material */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Estimate cost material */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bCancel C-Win
ON CHOOSE OF bCancel IN FRAME DEFAULT-FRAME /* Calculate */
DO:
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bOk C-Win
ON CHOOSE OF bOk IN FRAME DEFAULT-FRAME /* Choose Vendor */
DO:
    DEFINE BUFFER bf-ef              FOR ef.
    
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE gcScopeRMOverride  AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - RM Override".
    DEFINE VARIABLE gcScopeFGEstimated AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - FG Estimated".
    DEFINE VARIABLE cAdderList         AS CHARACTER EXTENT 6 NO-UNDO.
    DEFINE VARIABLE iCount             AS INTEGER   NO-UNDO. 
     
    IF AVAILABLE ttEstCostMaterial THEN
    DO: 
        cAdderList = "".
        FOR EACH bf-ef NO-LOCK
            WHERE bf-ef.company EQ ttEstCostMaterial.company
            AND bf-ef.est-no  EQ ttEstCostMaterial.estimateNo
            AND bf-ef.form-no EQ ttEstCostMaterial.formNo:
            DO iCount = 1 TO 6:
                IF bf-ef.adder[iCount] <> "" THEN 
                    cAdderList[iCount] = bf-ef.adder[iCount].
            END.
        END.
        RUN system/vendorcostSelector.w(
         INPUT  ttEstCostMaterial.company, //ipcCompany ,
         INPUT  ttEstCostMaterial.itemID ,
         INPUT  IF isPurchasedFG THEN "FG" ELSE "RM", //ipcItemType ,
         INPUT  IF isPurchasedFG THEN gcScopeFGEstimated ELSE gcScopeRMOverride, //ipcScope ,
         INPUT  "Yes", //iplIncludeBlankVendor ,
         INPUT  ttEstCostMaterial.estimateNo, //ipcEstimateNo,
         INPUT  ttEstCostMaterial.formNo, //ipiFormNo,
         INPUT  ttEstCostMaterial.blankNo, //ipiBlankNo,
         INPUT  ttEstCostMaterial.quantityRequiredTotal , //ipdQuantity ,
         INPUT  ttEstCostMaterial.quantityUOM, //ipcQuantityUOM ,
         INPUT  ttEstCostMaterial.dimLength, //ipdDimLength ,
         INPUT  ttEstCostMaterial.dimWidth, //ipdDimWidth ,
         INPUT  ttEstCostMaterial.dimDepth, //ipdDimDepth ,
         INPUT  ttEstCostMaterial.dimUOM, //ipcDimUOM ,
         INPUT  ttEstCostMaterial.basisWeight, //ipdBasisWeight ,
         INPUT  ttEstCostMaterial.basisWeightUOM, //ipcBasisWeightUOM ,
         INPUT  cAdderList,
         OUTPUT  TABLE ttVendItemCost,
         OUTPUT  lError ,
         OUTPUT  cMessage 
        ).
    END.
    
    FOR FIRST ttVendItemCost WHERE ttVendItemCost.isSelected :
        
        IF AVAILABLE ttEstCostMaterial THEN 
        DO:          
            ASSIGN 
                ttEstCostMaterial.vendorId   = ttVendItemCost.vendorID  
                ttEstCostMaterial.costUOM    = ttVendItemCost.vendorUOM 
                ttEstCostMaterial.costPerUOM = ttVendItemCost.costPerVendorUOM
                ttEstCostMaterial.costSetup  = ttVendItemCost.costSetup
                ttEstCostMaterial.costTotal  = ttVendItemCost.costTotal
                .
                
            IF AVAILABLE ttEstCostHeaderToCalc THEN 
                ttEstCostHeaderToCalc.lRecalcRequired = TRUE. 
        END.
         
    END.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brEstCostMaterial
&Scoped-define SELF-NAME brEstCostMaterial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brEstCostMaterial C-Win
ON START-SEARCH OF brEstCostMaterial IN FRAME DEFAULT-FRAME
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

&Scoped-define SELF-NAME btnRecostboard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRecostboard C-Win
ON CHOOSE OF btnRecostboard IN FRAME DEFAULT-FRAME /* Recost Board */
    DO: 
        RUN ipRecostBoard(INPUT YES).                
        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopyVendorToAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopyVendorToAll C-Win
ON CHOOSE OF btnCopyVendorToAll IN FRAME DEFAULT-FRAME /* Copy Vendor to All */
DO:
    DEFINE BUFFER bf-estCostMaterialForAll FOR estCostMaterial.
    DEFINE BUFFER bb-ttEstCostHeaderToCalc FOR ttEstCostHeaderToCalc.
    DEFINE BUFFER bb-estCostHeaderForAll   FOR esTCostHeader.
    DEFINE BUFFER bf-ttEstCostMaterial     FOR ttEstCostMaterial.
        
    DEFINE VARIABLE gcScopeRMOverride  AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - RM Override".
    DEFINE VARIABLE gcScopeFGEstimated AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - FG Estimated".
    DEFINE VARIABLE cAdderList         AS CHARACTER EXTENT 6 NO-UNDO.
    DEFINE VARIABLE oplError           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE opcMessage         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE chErrorList        AS CHARACTER NO-UNDO.
        
    IF AVAILABLE estCostMaterial THEN
    DO:                              
        FOR EACH bb-ttEstCostHeaderToCalc,
            FIRST bb-estCostHeaderForAll NO-LOCK
            WHERE bb-estCostHeaderForAll.estCostHeaderID   = bb-ttEstCostHeaderToCalc.iEstCostHeaderID,        
            EACH bf-estCostMaterialForAll EXCLUSIVE-LOCK
            WHERE bf-estCostMaterialForAll.estCostHeaderID = bb-estCostHeaderForAll.estCostHeaderID
            AND bf-estCostMaterialForAll.estimateNo        = bb-estCostHeaderForAll.estimateNo            
            AND (bf-estCostMaterialForAll.isPrimarySubstrate OR bf-estCostMaterialForAll.isPurchased)
            AND bf-estCostMaterialForAll.estCostMaterialID <> estCostMaterial.estCostMaterialID
            AND bf-estCostMaterialForAll.vendorId          <> estCostMaterial.vendorID:    
    
            RUN Estmate_GetAddersArray(INPUT bf-estCostMaterialForAll.Company, // Internal procedure in est/EstimateProcs.p
                INPUT bf-estCostMaterialForAll.estimateNo,
                INPUT bf-estCostMaterialForAll.formNo,
                OUTPUT cAdderList).
                
            EMPTY TEMP-TABLE ttVendItemCost.
                  
            RUN BuildVendItemCostsWithAdders(
                INPUT  bf-estCostMaterialForAll.Company,
                INPUT  bf-estCostMaterialForAll.ItemID,
                INPUT  IF isPurchasedFG THEN "FG" ELSE "RM",
                INPUT  IF isPurchasedFG THEN gcScopeFGEstimated ELSE gcScopeRMOverride,
                INPUT  "Yes",
                INPUT  bf-estCostMaterialForAll.estimateNo,
                INPUT  bf-estCostMaterialForAll.formNo,
                INPUT  bf-estCostMaterialForAll.blankNo,
                INPUT  bf-estCostMaterialForAll.quantityRequiredTotal,
                INPUT  bf-estCostMaterialForAll.quantityUOM,
                INPUT  bf-estCostMaterialForAll.dimLength,
                INPUT  bf-estCostMaterialForAll.dimWidth,
                INPUT  bf-estCostMaterialForAll.dimDepth,
                INPUT  bf-estCostMaterialForAll.dimUOM,
                INPUT  bf-estCostMaterialForAll.basisWeight,
                INPUT  bf-estCostMaterialForAll.basisWeightUOM,
                INPUT  cAdderList,
                OUTPUT  TABLE  ttVendItemCost,
                OUTPUT  oplError,
                OUTPUT  opcMessage).
                
            IF oplError THEN 
            DO:                    
                ASSIGN 
                    chErrorList = chErrorList + chr(10) + opcMessage.            
            END.
                
            FIND FIRST ttVendItemCost
                WHERE ttVendItemCost.vendorid = estCostMaterial.vendorID NO-ERROR.
                    
            IF NOT AVAILABLE ttVendItemCost THEN
            DO:
                ASSIGN 
                    chErrorList = chErrorList + chr(10) + "Unable to copy Vendor " + estCostMaterial.vendorID + 
                                  " to Item " + bf-estCostMaterialForAll.itemID + " - Form " + estCostMaterial.itemID.
            END.
                
            IF AVAILABLE ttVendItemCost AND ttVendItemCost.isValid = FALSE THEN 
            DO:
                ASSIGN 
                    chErrorList = chErrorList + chr(10) + ttVendItemCost.reasonNotValid.
            END.
        END.    
            
        ASSIGN 
            chErrorList = TRIM(chErrorList,CHR(10)).    
            
        IF chErrorList <> "" THEN
        DO:
            MESSAGE chErrorList
                VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        
        FOR EACH bb-ttEstCostHeaderToCalc,
            FIRST bb-estCostHeaderForAll NO-LOCK     
            WHERE bb-estCostHeaderForAll.estCostHeaderID EQ bb-ttEstCostHeaderToCalc.iEstCostHeaderID,
                        
            EACH bf-estCostMaterialForAll EXCLUSIVE-LOCK
            WHERE bf-estCostMaterialForAll.estCostHeaderID = bb-estCostHeaderForAll.estCostHeaderID
            AND bf-estCostMaterialForAll.estimateNo        = bb-estCostHeaderForAll.estimateNo            
			AND (bf-estCostMaterialForAll.isPrimarySubstrate OR bf-estCostMaterialForAll.isPurchased)
            AND bf-estCostMaterialForAll.estCostMaterialID <> estCostMaterial.estCostMaterialID
            AND bf-estCostMaterialForAll.vendorId          <> estCostMaterial.vendorID:
                  
            ASSIGN 
                bf-estCostMaterialForAll.vendorId = estCostMaterial.vendorID.
        END.                  
    END.
        
    IF AVAIL ttEstCostMaterial AND ttEstCostMaterial.vendorID NE ""  THEN
    FOR EACH bf-ttEstCostMaterial NO-LOCK     
        WHERE bf-ttEstCostMaterial.vendorId <> ttEstCostMaterial.vendorID:
                
          ASSIGN 
              bf-ttEstCostMaterial.vendorId   = ttEstCostMaterial.vendorID.
        
        RUN Estmate_GetAddersArray(INPUT bf-ttEstCostMaterial.Company, // Internal procedure in est/EstimateProcs.p
                INPUT bf-ttEstCostMaterial.estimateNo,
                INPUT bf-ttEstCostMaterial.formNo,
                OUTPUT cAdderList).   
                
         EMPTY TEMP-TABLE ttVendItemCost.
         
         RUN BuildVendItemCostsWithAdders(
                INPUT  bf-ttEstCostMaterial.Company,
                INPUT  bf-ttEstCostMaterial.ItemID,
                INPUT  IF isPurchasedFG THEN "FG" ELSE "RM",
                INPUT  IF isPurchasedFG THEN gcScopeFGEstimated ELSE gcScopeRMOverride,
                INPUT  "Yes",
                INPUT  bf-ttEstCostMaterial.estimateNo,
                INPUT  bf-ttEstCostMaterial.formNo,
                INPUT  bf-ttEstCostMaterial.blankNo,
                INPUT  bf-ttEstCostMaterial.quantityRequiredTotal,
                INPUT  bf-ttEstCostMaterial.quantityUOM,
                INPUT  bf-ttEstCostMaterial.dimLength,
                INPUT  bf-ttEstCostMaterial.dimWidth,
                INPUT  bf-ttEstCostMaterial.dimDepth,
                INPUT  bf-ttEstCostMaterial.dimUOM,
                INPUT  bf-ttEstCostMaterial.basisWeight,
                INPUT  bf-ttEstCostMaterial.basisWeightUOM,
                INPUT  cAdderList,
                OUTPUT  TABLE  ttVendItemCost, 
                OUTPUT  oplError,
                OUTPUT  opcMessage).
                 
         FIND FIRST ttVendItemCost NO-LOCK
              WHERE ttVendItemCost.vendorID EQ bf-ttEstCostMaterial.vendorId NO-ERROR.
         IF AVAILABLE ttVendItemCost THEN
         ASSIGN                 
              bf-ttEstCostMaterial.costUOM    = ttVendItemCost.vendorUOM 
              bf-ttEstCostMaterial.costPerUOM = ttVendItemCost.costPerVendorUOM
              bf-ttEstCostMaterial.costSetup  = ttVendItemCost.costSetup
              bf-ttEstCostMaterial.costTotal  = ttVendItemCost.costTotal .             
       
         FIND FIRST bb-ttEstCostHeaderToCalc NO-LOCK
            WHERE bb-ttEstCostHeaderToCalc.iEstCostHeaderID EQ  bf-ttEstCostMaterial.estCostHeaderID NO-ERROR.
       
         IF AVAILABLE bb-ttEstCostHeaderToCalc THEN 
                bb-ttEstCostHeaderToCalc.lRecalcRequired = TRUE.    
    END.      
             
    RELEASE bf-estCostMaterialForAll.
    RELEASE bf-ttEstCostMaterial.
    
        {&OPEN-QUERY-{&BROWSE-NAME}}     
      
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

bOk:LOAD-IMAGE("Graphics/32x32/choosevendor.png").
bCancel:LOAD-IMAGE("Graphics/32x32/calculate.png").
btnRecostboard:LOAD-IMAGE("Graphics/32x32/RecostBoard.png").

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
    FOR FIRST ttEstCostHeaderToCalc,
        FIRST ttEstCostHeader NO-LOCK 
            WHERE ttEstCostHeader.estCostHeaderID EQ ttEstCostHeaderToCalc.iEstCostHeaderID:
            fiEstimateNo:SCREEN-VALUE = ttEstCostHeader.estimateNo.
    END.
    
    RUN sys/ref/nk1look.p (g_company, "CEAutoRecostBoard", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    
    IF lFound = TRUE AND cReturn EQ "YES" THEN
        btnRecostboard:HIDDEN IN FRAME DEFAULT-FRAME = TRUE.
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.


&Scoped-define sdBrowseName brEstCostMaterial
{methods/sortByProc.i "pByQtyMaster" "ttEstCostHeader.quantityMaster"}
{methods/sortByProc.i "pByFormNo" "ttEstCostMaterial.formNo"}
{methods/sortByProc.i "pByBlankNo" "ttEstCostMaterial.blankNo"}
{methods/sortByProc.i "pByItemID" "ttEstCostMaterial.itemID"}
{methods/sortByProc.i "pByVendorID" "ttEstCostMaterial.vendorID"}
{methods/sortByProc.i "pByQuantityRequiredTotal" "ttEstCostMaterial.quantityRequiredTotal"}
{methods/sortByProc.i "pByQuantityRequiredTotalInCUOM" "ttEstCostMaterial.quantityRequiredTotalInCUOM"}
{methods/sortByProc.i "pByCostPerUOM" "ttEstCostMaterial.costPerUOM"}
{methods/sortByProc.i "pByCostUOM" "ttEstCostMaterial.costUOM"}
{methods/sortByProc.i "pByCostSetup" "ttEstCostMaterial.costSetup"}
{methods/sortByProc.i "pByCostTotal" "ttEstCostMaterial.costTotal"}

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
  DISPLAY fiEstimateNo estimate 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-13 brEstCostMaterial bOk btnCopyVendorToAll btnRecostboard
         bCancel fiEstimateNo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRecostBoard C-Win
PROCEDURE ipRecostBoard:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplMessage AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE hdRecostBoardEst AS HANDLE NO-UNDO.
    DEFINE VARIABLE chMessage        AS CHARACTER NO-UNDO.
        
    IF VALID-HANDLE(hdRecostBoardEst) = FALSE THEN
        RUN est\RecostBoardEst.p PERSISTENT SET hdRecostBoardEst.
    
    RUN RecostBoardEst_RecostBoard IN hdRecostBoardEst(INPUT TABLE ttEstCostHeaderToCalc,
        INPUT TABLE ttEstCostMaterial,
        INPUT TABLE ttEstCostHeader,
        OUTPUT chMessage,
        OUTPUT TABLE ttRecostBoardGroups).
                                              
    IF iplMessage AND chMessage <> "" THEN 
        MESSAGE chMessage
            VIEW-AS ALERT-BOX.
                
    IF chMessage = "" THEN
    DO:        
        IF iplMessage THEN RUN ShowCostUpdates. /*Present updates to user and get confirmation*/
        
        RUN RecostBoardEst_UpdateEstCostMaterial IN hdRecostBoardEst(INPUT NO,
                                                                     INPUT-OUTPUT TABLE ttEstCostMaterial BY-REFERENCE,
                                                                     INPUT TABLE ttRecostBoardGroups).  /*Update the EstCostMaterial costs with better costs*/
    END.
    
    {&OPEN-QUERY-{&BROWSE-NAME}}

    IF VALID-HANDLE(hdRecostBoardEst) THEN
        DELETE PROCEDURE hdRecostBoardEst.
        
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
        WHEN "quantityMaster" THEN
            RUN pByQtyMaster.
        WHEN "formNo" THEN
            RUN pByFormNo.
        WHEN "blankNo" THEN
            RUN pByBlankNo.
        WHEN "itemID" THEN
            RUN pByItemID.
        WHEN "vendorID" THEN
            RUN pByVendorID.
        WHEN "quantityRequiredTotal" THEN
            RUN pByQuantityRequiredTotal.
        WHEN "quantityRequiredTotalInCUOM" THEN
            RUN pByQuantityRequiredTotalInCUOM.
        WHEN "costPerUOM" THEN
            RUN pByCostPerUOM.
        WHEN "costUOM" THEN
            RUN pByCostUOM.
        WHEN "costSetup" THEN
            RUN pByCostSetup.
        WHEN "costTotal" THEN
            RUN pByCostTotal.       
        OTHERWISE
            {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowCostUpdates C-Win
PROCEDURE ShowCostUpdates:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE lUpdateCost   AS LOGICAL NO-UNDO.
DEFINE VARIABLE chMessageText AS CHARACTER NO-UNDO.


    FOR EACH ttRecostBoardGroups
        WHERE ttRecostBoardGroups.Multi
          AND ttRecostBoardGroups.UpdateCost:
          
        ASSIGN 
            chMessageText = "A reduced cost of " + STRING(ttRecostBoardGroups.NewCost) + " per " + ttRecostBoardGroups.NewCostUOM + " was found." + CHR(10) + 
            "Forms: " + ttRecostBoardGroups.FormIdList + CHR(10) +
            "Master quantity: " + String(ttRecostBoardGroups.quantityMaster) + CHR(10) +                                  
            "Item #: " + ttRecostBoardGroups.INo + " " + ttRecostBoardGroups.ItemName + CHR(10) +
            "Vendor: " + ttRecostBoardGroups.VendNo + CHR(10) +
            "Size: " + STRING(ttRecostBoardGroups.Wid) + " x " + STRING(ttRecostBoardGroups.Len) + CHR(10) +            
            "Scores: " + ttRecostBoardGroups.Scores + CHR(10) + 
            (IF ttRecostBoardGroups.Adders <> "" THEN "Adders: " + ttRecostBoardGroups.Adders + CHR(10) ELSE "")  + 
            "Total Combined Quantity: " + STRING(ttRecostBoardGroups.TotalQty) + " " + ttRecostBoardGroups.TotalQtyUom + CHR(10) +                                
            "Do you want to apply this cost to the order lines?".

        MESSAGE chMessageText
            VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE lUpdateCost.
        ttRecostBoardGroups.UpdateCost = lUpdateCost.

    END. /*each ttRecostBoardGroups*/
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

