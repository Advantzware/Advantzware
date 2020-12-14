&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEstNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiFromNo AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
DEF INPUT-OUTPUT PARAM io-price AS DEC NO-UNDO.
DEF INPUT-OUTPUT PARAM io-UOM AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF TEMP-TABLE ttVICLevel /* LIKE vendItemCostLevel*/
    FIELD uptoQty AS INT LABEL "Up To Qty"
    FIELD price AS DEC LABEL "Price" 
    FIELD uom AS CHARACTER LABEL "UOM"
    FIELD levelRowid AS ROWID 
    FIELD vendItemCostLevelID LIKE vendItemCostLevel.vendItemCostLevelID.
    
DEF VAR ldPrice AS DEC NO-UNDO.

DEF BUFFER bf-ef for ef.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-VendorCost

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttVicLevel

/* Definitions for BROWSE br-VendorCost                                 */
&Scoped-define FIELDS-IN-QUERY-br-VendorCost ttVicLevel.UptoQty ttVicLevel.price ttVicLevel.UOM   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-VendorCost   
&Scoped-define SELF-NAME br-VendorCost
&Scoped-define QUERY-STRING-br-VendorCost FOR EACH ttVicLevel
&Scoped-define OPEN-QUERY-br-VendorCost OPEN QUERY {&SELF-NAME} FOR EACH ttVicLevel.
&Scoped-define TABLES-IN-QUERY-br-VendorCost ttVicLevel
&Scoped-define FIRST-TABLE-IN-QUERY-br-VendorCost ttVicLevel


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-VendorCost}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-26 br-VendorCost Btn_OK Btn_Cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76 BY 12.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-VendorCost FOR 
      ttVicLevel SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-VendorCost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-VendorCost D-Dialog _FREEFORM
  QUERY br-VendorCost DISPLAY
      ttVicLevel.UptoQty ttVicLevel.price ttVicLevel.uom ttVicLevel.vendItemCostLevelID
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 66 BY 11.19
         FONT 6 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     br-VendorCost AT ROW 1.48 COL 5
     Btn_OK AT ROW 14.1 COL 20
     Btn_Cancel AT ROW 14.1 COL 43
     RECT-26 AT ROW 1 COL 1
     SPACE(2.59) SKIP(2.52)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Vendor Item Cost Selection"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
/* BROWSE-TAB br-VendorCost RECT-26 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-VendorCost
/* Query rebuild information for BROWSE br-VendorCost
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttVicLevel
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-VendorCost */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Vendor Item Cost Selection */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
/*  DO WITH FRAME {&FRAME-NAME}:  */
/*    ASSIGN {&displayed-objects}.*/
/*  END.                          */

  ASSIGN io-price = ttviclevel.price
         io-UOM = ttvicLevel.uom.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-VendorCost
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */


DO:
    
/*  FIND bf-ef NO-LOCK WHERE rowid(ef) = ip-rowid NO-ERROR.*/
/*  IF NOT AVAILABLE bf-ef THEN RETURN.                    */
    
  RUN pBuildTable.
  RUN pOpenQuery.
  {src/adm/template/dialogmn.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  ENABLE RECT-26 br-VendorCost Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildTable D-Dialog 
PROCEDURE pBuildTable PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
/*    DEFINE INPUT PARAMETER cCompany AS CHARACTER NO-UNDO.         */
/*    DEFINE INPUT PARAMETER cItemID AS CHARACTER NO-UNDO.          */
/*    DEFINE INPUT PARAMETER cItemType AS CHARACTER NO-UNDO.        */
/*    DEFINE INPUT PARAMETER cScope AS CHARACTER NO-UNDO.           */
/*    DEFINE INPUT PARAMETER lIncludeBlankVendor AS LOGICAL NO-UNDO.*/
/*    DEFINE INPUT PARAMETER dQuantity AS DECIMAL NO-UNDO.          */
/*    DEFINE INPUT PARAMETER cQuantityUOM AS CHARACTER NO-UNDO.     */
/*    DEFINE INPUT PARAMETER dDimLength AS DECIMAL NO-UNDO.         */
/*    DEFINE INPUT PARAMETER dDimWidth AS DECIMAL NO-UNDO.          */
/*    DEFINE INPUT PARAMETER dDimDepth AS DECIMAL NO-UNDO.          */
/*    DEFINE INPUT PARAMETER cDimUOM AS CHARACTER NO-UNDO.          */
/*    DEFINE INPUT PARAMETER dBasisWeight AS DECIMAL NO-UNDO.       */
/*    DEFINE INPUT PARAMETER cBasisWeightUOM AS CHARACTER NO-UNDO.  */
    
    DEF VAR licount AS INT NO-UNDO.
    
    
    EMPTY TEMP-TABLE ttVicLevel .    
     
    FOR EACH vendItemCost NO-LOCK
        WHERE vendItemCost.company EQ ipcCompany
/*        AND vendItemCost.EstimateNo = ipcEstNo*/
        AND vendItemCost.itemID EQ ipcItemID
        AND vendItemCost.itemType EQ ipcItemType
/*        AND (bf-vendItemCost.effectiveDate LE TODAY OR lIncludeNonEffective)                                                                                   */
/*        AND (bf-vendItemCost.expirationDate GE TODAY OR bf-vendItemCost.expirationDate EQ ? OR bf-vendItemCost.expirationDate EQ 01/01/0001 OR lIncludeExpired)*/
/*        AND (lEstimatedOnly AND bf-vendItemCost.estimateNo NE "" OR NOT lEstimatedOnly)                                                                        */
/*        AND (lNotEstimatedOnly AND bf-vendItemCost.estimateNo EQ "" OR NOT lNotEstimatedOnly)                                                                  */
/*        AND (bf-vendItemCost.vendorID NE "" OR iplIncludeBlankVendor)                                                                                          */
        ,  
     each vendItemCostLevel NO-LOCK WHERE vendItemCostLevel.vendItemCostID EQ vendItemCost.VendItemCostID
/*          AND vendItemCostLevel.quantityFrom LE ipdQuantityTarget*/
/*          AND vendItemCostLevel.quantityTo GE ipdQuantityTarget  */
        
         BY vendItemCost.vendorID BY vendItemCostLevel.quantityTo :
     
       CREATE ttVicLevel.
       ASSIGN ttVicLevel.levelRowid = rowid(vendItemCostLevel)
              ttVicLevel.uptoQty = vendItemCostLevel.quantityTo
              ttVicLevel.uom = vendItemCost.vendorUOM
              ttVicLevel.price = vendItemCostLevel.costPerUOM
              ttVicLevel.vendItemCostLevelID = vendItemCostLevel.vendItemCostLevelID
              liCount = liCount + 1
              .
            
    END.
    MESSAGE "# of VIC level records: " liCount  "item: " ipcItemID
    VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOpenQuery D-Dialog 
PROCEDURE pOpenQuery :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*{&OPEN-QUERY-br-VendorCost}*/
    OPEN QUERY br-VendorCost FOR EACH ttVicLevel BY uptoQty.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ttVicLevel"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
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

