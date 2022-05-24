&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/ItemToPurchase.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Varun Agrawal

  Created:26th April 2022

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

{methods/defines/sortByDefs.i}
{system/VendorCostProcs.i}
{PO/itemToPurchase.i}       

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttJobMaterial.
DEFINE OUTPUT PARAMETER oploUpdatePO AS LOGICAL NO-UNDO. 
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brItemToPurchase

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttJobMaterial

/* Definitions for BROWSE brItemToPurchase                              */
&Scoped-define FIELDS-IN-QUERY-brItemToPurchase ttJobMaterial.CreatePO ttJobMaterial.DropShipment ttJobMaterial.frm ttJobMaterial.blank-no ttJobMaterial.i-no ttJobMaterial.ItemName ttJobMaterial.ItemType ttJobMaterial.vendorID ttJobMaterial.POdate ttJobMaterial.PODuedate ttJobMaterial.qty ttJobMaterial.qty-uom ttJobMaterial.costPerUOM ttJobMaterial.costUOM ttJobMaterial.CostSetup ttJobMaterial.CostTotal ttJobMaterial.IsValid ttJobMaterial.InvalidReason   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brItemToPurchase ttJobMaterial.CreatePO ttJobMaterial.qty ttJobMaterial.POdate ttJobMaterial.PODueDate ttJobMaterial.DropShipment   
&Scoped-define ENABLED-TABLES-IN-QUERY-brItemToPurchase ttJobMaterial
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brItemToPurchase ttJobMaterial
&Scoped-define SELF-NAME brItemToPurchase
&Scoped-define QUERY-STRING-brItemToPurchase FOR EACH ttJobMaterial ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-brItemToPurchase OPEN QUERY {&SELF-NAME} FOR EACH ttJobMaterial WHERE ttJobMaterial.isValid = (IF tbShowAll:CHECKED in frame {&frame-name} THEN ttJobMaterial.isValid else TRUE) {&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-brItemToPurchase ttJobMaterial
&Scoped-define FIRST-TABLE-IN-QUERY-brItemToPurchase ttJobMaterial


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brItemToPurchase}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-13 tbShowAll rdPOChoice ~
brItemToPurchase btnChooseVendor btnOk 
&Scoped-Define DISPLAYED-OBJECTS tbShowAll rdPOChoice 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnChooseVendor 
     LABEL "Choose Vendor" 
     SIZE 17.6 BY 1.38.

DEFINE BUTTON btnOk 
     LABEL "Create PO" 
     SIZE 16 BY 1.38.

DEFINE VARIABLE rdPOChoice AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Combine onto one PO per Vendor", 1,
"Separate PO for each line", 2
     SIZE 39 BY 2
     BGCOLOR 23 FGCOLOR 24 FONT 22 NO-UNDO.

DEFINE VARIABLE tbShowAll AS LOGICAL INITIAL NO 
     LABEL "Show All" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81
     BGCOLOR 23  FGCOLOR 24 FONT 22 NO-UNDO.
     
DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 170 BY 2.52
     BGCOLOR 23 FGCOLOR 24.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brItemToPurchase FOR 
      ttJobMaterial SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brItemToPurchase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brItemToPurchase C-Win _FREEFORM
  QUERY brItemToPurchase NO-LOCK DISPLAY
      ttJobMaterial.CreatePO              COLUMN-LABEL "Purchase"
            LABEL-BGCOLOR 14    FORMAT 'Yes/No' VIEW-AS TOGGLE-BOX 
      ttJobMaterial.DropShipment          COLUMN-LABEL "Drop Shipment"
            LABEL-BGCOLOR 14    FORMAT 'Yes/No' VIEW-AS TOGGLE-BOX            
      ttJobMaterial.frm                    COLUMN-LABEL "Form"       
            LABEL-BGCOLOR 14    FORMAT '>9'
      ttJobMaterial.blank-no                   COLUMN-LABEL "Blank"    
             LABEL-BGCOLOR 14   FORMAT ">9"
      ttJobMaterial.i-no                    COLUMN-LABEL "Item ID"  
             LABEL-BGCOLOR 14   FORMAT "x(10)" 
      ttJobMaterial.ItemName                    COLUMN-LABEL "Item Description"  
             LABEL-BGCOLOR 14   FORMAT "x(20)"
      ttJobMaterial.ItemType                    COLUMN-LABEL "Item Type"  
             LABEL-BGCOLOR 14   FORMAT "x(8)"             
      ttJobMaterial.vendorID                  COLUMN-LABEL "Best Vendor" 
             LABEL-BGCOLOR 14   FORMAT "x(10)"
      ttJobMaterial.POdate                  COLUMN-LABEL "PO Date" 
             LABEL-BGCOLOR 14   FORMAT "99/99/99"        
      ttJobMaterial.PODuedate                  COLUMN-LABEL "PO Due Date" 
             LABEL-BGCOLOR 14   FORMAT "99/99/99"             
      ttJobMaterial.qty                COLUMN-LABEL "Quantity" 
             LABEL-BGCOLOR 14   FORMAT "->>,>>9.99"
      ttJobMaterial.qty-uom                   COLUMN-LABEL "Quantity UOM"        
             LABEL-BGCOLOR 14   FORMAT "x(8)"
      ttJobMaterial.costPerUOM                COLUMN-LABEL "Per UOM Cost" 
             LABEL-BGCOLOR 14   FORMAT "->>,>>9.99"
      ttJobMaterial.costUOM                   COLUMN-LABEL "Cost UOM"        
             LABEL-BGCOLOR 14   FORMAT "x(8)"     
      ttJobMaterial.CostSetup     COLUMN-LABEL "Cost SetUp" 
             LABEL-BGCOLOR 14   FORMAT "->>,>>9.99"
      ttJobMaterial.CostTotal       COLUMN-LABEL "Extended Total" 
             LABEL-BGCOLOR 14    FORMAT "->>,>>9.99"
      ttJobMaterial.IsValid         COLUMN-LABEL "Valid" 
             LABEL-BGCOLOR 14   FORMAT 'Yes/No'
      ttJobMaterial.InvalidReason COLUMN-LABEL "InvalidReason" 
             LABEL-BGCOLOR 14    FORMAT "x(60)"
             
      ENABLE ttJobMaterial.CreatePO ttJobMaterial.qty ttJobMaterial.POdate ttJobMaterial.PODueDate ttJobMaterial.DropShipment
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 169.8 BY 15.19
         FONT 34 ROW-HEIGHT-CHARS .9 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     tbShowAll AT ROW 1.52 COL 150
     rdPOChoice AT ROW 1.52 COL 3.6 NO-LABEL
     brItemToPurchase AT ROW 4.1 COL 171 RIGHT-ALIGNED WIDGET-ID 200
     btnChooseVendor AT ROW 19.48 COL 3 WIDGET-ID 342
     btnOk AT ROW 19.48 COL 155.8 WIDGET-ID 356
     RECT-13 AT ROW 1.24 COL 2 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 171.8 BY 20.1
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
         TITLE              = "Item To Purchase"
         HEIGHT             = 20.1
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
/* BROWSE-TAB brItemToPurchase rdPOChoice DEFAULT-FRAME */
/* SETTINGS FOR BROWSE brItemToPurchase IN FRAME DEFAULT-FRAME
   ALIGN-R                                                              */
ASSIGN 
       brItemToPurchase:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brItemToPurchase
/* Query rebuild information for BROWSE brItemToPurchase
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
FOR EACH ttJobMaterial ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brItemToPurchase */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Item To Purchase */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Item To Purchase */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brItemToPurchase
&Scoped-define SELF-NAME brItemToPurchase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brItemToPurchase C-Win
ON START-SEARCH OF brItemToPurchase IN FRAME DEFAULT-FRAME
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


&Scoped-define SELF-NAME btnChooseVendor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnChooseVendor C-Win
ON CHOOSE OF btnChooseVendor IN FRAME DEFAULT-FRAME /* Choose Vendor */
DO:    
    DEFINE BUFFER bf-ef              FOR ef.
    
    DEFINE VARIABLE lError             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE gcScopeRMOverride  AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - RM Override".
    DEFINE VARIABLE gcScopeFGEstimated AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - FG Estimated".
    DEFINE VARIABLE cAdderList         AS CHARACTER EXTENT 6 NO-UNDO.
    DEFINE VARIABLE iCount             AS INTEGER   NO-UNDO.     
     
    IF AVAILABLE ttJobMaterial THEN
    DO:
        
        FOR EACH bf-ef NO-LOCK
            WHERE bf-ef.company EQ ttJobMaterial.company
            AND bf-ef.est-no    EQ ttJobMaterial.est-no:
                
            DO iCount = 1 TO 6:
                IF bf-ef.adder[iCount] <> "" THEN 
                    cAdderList[iCount] = bf-ef.adder[iCount].
            END.
        END.
        
        
        RUN system/vendorcostSelector.w(
         INPUT  ttJobMaterial.company,    //ipcCompany ,
         INPUT  ttJobMaterial.i-no,
         INPUT  IF this-is-a-rm THEN "RM" ELSE "FG",   //ipcItemType ,
         INPUT  IF this-is-a-rm NE TRUE THEN gcScopeFGEstimated ELSE gcScopeRMOverride, //ipcScope ,
         INPUT  "No",                           //iplIncludeBlankVendor ,
         INPUT  ttJobMaterial.est-no,           //ipcEstimateNo,
         INPUT  ttJobMaterial.frm,              //ipifrm,
         INPUT  ttJobMaterial.blank-no,         //ipiblank-no,
         INPUT  ttJobMaterial.qty,              //ipdqty,
         INPUT  ttJobMaterial.qty-uom,          //ipcqty-uom ,
         INPUT  ttJobMaterial.Len,              //ipdDimLength,
         INPUT  ttJobMaterial.Wid,              //ipdDimWidth,
         INPUT  ttJobMaterial.Dep,              //ipdDimDepth,
         INPUT  ttJobMaterial.sc-UOM,           //ipcDimUOM ,
         INPUT  ttJobMaterial.basis-w,          //ipdBasisWeight ,
         INPUT  "",                             //ipcBasisWeightUOM ,
         INPUT  cAdderList,
         OUTPUT  TABLE ttVendItemCost,
         OUTPUT  lError ,
         OUTPUT  cMessage 
        ).
    END.
    
    FOR FIRST ttVendItemCost WHERE ttVendItemCost.isSelected:                  
        ASSIGN 
            ttJobMaterial.vendorId   = ttVendItemCost.vendorID               
            ttJobMaterial.costUOM    = ttVendItemCost.vendorUOM            
            ttJobMaterial.costPerUOM = ttVendItemCost.costPerVendorUOM
            ttJobMaterial.costSetup  = ttVendItemCost.costSetup
            ttJobMaterial.costTotal  = ttVendItemCost.costTotal.
          
    END.
    {&OPEN-QUERY-{&BROWSE-NAME}}      
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* Create PO */
DO:
    ASSIGN oploUpdatePO = IF rdPOChoice:screen-VALUE = "1" THEN TRUE ELSE FALSE.
        
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME tbShowAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbShowAll C-Win
ON VALUE-CHANGED OF tbShowAll IN FRAME DEFAULT-FRAME
DO:
    {&OPEN-QUERY-brItemToPurchase}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttJobMaterial.DropShipment
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttJobMaterial.DropShipment C-Win
ON VALUE-CHANGED OF ttJobMaterial.DropShipment /* Dropship finished goods */
DO:  
    DEFINE VARIABLE chValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE reShipTo     AS RECID     NO-UNDO.
    DEFINE VARIABLE chShipChoice AS CHARACTER NO-UNDO.       
    
    ASSIGN ttJobMaterial.DropShipment = LOGICAL(ttJobMaterial.DropShipment:SCREEN-VALUE IN BROWSE brItemToPurchase). 
    
    IF ttJobMaterial.DropShipment:SCREEN-VALUE IN BROWSE brItemToPurchase = "Yes" THEN 
    DO:        
        RUN ipaskDropShip(OUTPUT chShipChoice).
                 
        ASSIGN ttJobMaterial.ShipChoice = chShipChoice.
        
        IF ttJobMaterial.ShipChoice EQ "C" THEN 
        DO:
            RUN windows/l-shipt2.w (ttJobMaterial.company, ttJobMaterial.locode, ttJobMaterial.DropCustNo, ttJobMaterial.ShipId, OUTPUT chValue, OUTPUT reShipTo).
                    
            IF reShipTo <> ? THEN 
                ASSIGN ttJobMaterial.shiptoRecId = reShipTo.
            ELSE
                ASSIGN ttJobMaterial.DropShipment = No.        
        END. /* if chShipChoice eq "C" */
        ELSE
        DO:
            RUN windows/l-vendno.w (ttJobMaterial.company, "A", ttJobMaterial.DropCustNo, OUTPUT chValue).
                        
            IF chValue <> "" THEN 
                ASSIGN ttJobMaterial.ShipToVendId = chValue.
            ELSE
                ASSIGN ttJobMaterial.DropShipment = No.
        END. /* NOT chShipChoice eq "C" */
        {&OPEN-QUERY-brItemToPurchase}
    END.
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

btnChooseVendor:LOAD-IMAGE("Graphics/32x32/choosevendor.png").
//btnOk:LOAD-IMAGE("Graphics/32x32/calculate.png").
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

&Scoped-define sdBrowseName brItemToPurchase
{methods/sortByProc.i "pByCreatePO" "ttJobMaterial.CreatePO"}
{methods/sortByProc.i "pByDropShipment" "ttJobMaterial.DropShipment"}
{methods/sortByProc.i "pByfrm" "ttJobMaterial.frm"}
{methods/sortByProc.i "pByblank-no" "ttJobMaterial.blank-no"}
{methods/sortByProc.i "pByi-no" "ttJobMaterial.i-no"}
{methods/sortByProc.i "pByItemName" "ttJobMaterial.ItemName"}
{methods/sortByProc.i "pByItemType" "ttJobMaterial.ItemType"}
{methods/sortByProc.i "pByVendorID" "ttJobMaterial.VendorID"}
{methods/sortByProc.i "pByPODate" "ttJobMaterial.PODate"}
{methods/sortByProc.i "pByPODueDate" "ttJobMaterial.PODueDate"}
{methods/sortByProc.i "pByqty" "ttJobMaterial.qty"}
{methods/sortByProc.i "pByqty-uom" "ttJobMaterial.qty-uom"}
{methods/sortByProc.i "pByCostPerUOM" "ttJobMaterial.CostPerUOM"}
{methods/sortByProc.i "pByCostUOM" "ttJobMaterial.costUOM"}
{methods/sortByProc.i "pByCostSetup" "ttJobMaterial.costSetup"}
{methods/sortByProc.i "pByCostTotal" "ttJobMaterial.costTotal"}

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
  DISPLAY tbShowAll rdPOChoice 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-13 tbShowAll rdPOChoice brItemToPurchase btnChooseVendor btnOk 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipaskDropShip C-Win
PROCEDURE ipaskDropShip:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opchShipChoice AS CHARACTER NO-UNDO.    
    
    DEFINE VARIABLE ship-choice AS CHARACTER LABEL "  Ship To"
        VIEW-AS RADIO-SET HORIZONTAL
        RADIO-BUTTONS "Vendor", "Vendor",
        "Customer", "Customer"
        SIZE 28 BY 1 NO-UNDO.

    DEFINE BUTTON Btn_OK AUTO-GO 
        LABEL "OK" 
        SIZE 15 BY 1
        BGCOLOR 8.

    DEFINE FRAME f-drop
        ship-choice SKIP
        btn_ok AT ROW 2 COL 11
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER TITLE "Drop Ship PO"
        SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE.
          
    ON "value-changed" OF ship-choice
        DO:
            opchShipChoice = SUBSTR(ship-choice:SCREEN-VALUE,1,1).
        END.

    ON 'choose':U OF btn_ok
        DO:
            ASSIGN 
                opchShipChoice = SUBSTR(ship-choice:SCREEN-VALUE,1,1). 
            APPLY "go" TO FRAME f-drop.
        END.

    ENABLE ship-choice btn_ok WITH FRAME f-drop.
    APPLY "value-changed" TO ship-choice.
    APPLY "entry" TO ship-choice.
    WAIT-FOR GO OF FRAME f-drop.
            
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
        WHEN "CreatePO" THEN
            RUN pByCreatePO.
        WHEN "DropShipment" THEN
            RUN pByDropShipment.            
        WHEN "frm" THEN
            RUN pByfrm.
        WHEN "blank-no" THEN
            RUN pByblank-no.
        WHEN "i-no" THEN
            RUN pByi-no.
        WHEN "ItemName" THEN
            RUN pByItemName.
        WHEN "ItemType" THEN
            RUN pByItemType.
        WHEN "vendorID" THEN
            RUN pByVendorID.            
        WHEN "PODate" THEN
            RUN pByPODate.
        WHEN "PODueDate" THEN
            RUN pByPODueDate.
        WHEN "qty" THEN
            RUN pByqty.
        WHEN "qty-uom" THEN
            RUN pByqty-uom.
        WHEN "CostPerUOM" THEN
            RUN pByCostPerUOM.
        WHEN "CostUOM" THEN
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

