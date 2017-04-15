&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: r-commcr.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 3.19.2016

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

&SCOPED-DEFINE useCustList
{aoa/includes/aoaParamVars.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS svCompany svShowQOHOlderThanDays svAsOfDate ~
btnCalendar-1 svAsOfDateOption svCustList btnCustList svAllCustNo ~
svStartCustNo svEndCustNo svAllLoc svStartLoc svEndLoc svAllLocBin ~
svStartLocBin svEndLocBin svAllItemNo svStartItemNo svEndItemNo ~
svAllProdCategory svStartProdCategory svEndProdCategory svAllSalesRep ~
svStartSalesRep svEndSalesRep svSort svItemCode svPrintSetAndComponentsOnly ~
svIncludeZeroBalance svIncludeCustomerOwnerdWarehouse ~
svPrintSummaryByBinQty svOnlyCustomerOwnedWarehouse svIncludeInactiveItems ~
svPrintCost svDLMATOnly 
&Scoped-Define DISPLAYED-OBJECTS svCompany svShowQOHOlderThanDays ~
svAsOfDate svAsOfDateOption svCustList svAllCustNo svStartCustNo ~
startCustName svEndCustNo endCustName svAllLoc svStartLoc startLocName ~
svEndLoc endLocName svAllLocBin svStartLocBin svEndLocBin svAllItemNo ~
svStartItemNo startItemName svEndItemNo endItemName svAllProdCategory ~
svStartProdCategory startProdCategoryName svEndProdCategory ~
endProdCategoryName svAllSalesRep svStartSalesRep startSalesRepName ~
svEndSalesRep endSalesRepName svSort svItemCode svPrintSetAndComponentsOnly ~
svIncludeZeroBalance svIncludeCustomerOwnerdWarehouse ~
svPrintSummaryByBinQty svOnlyCustomerOwnedWarehouse svIncludeInactiveItems ~
svPrintCost svDLMATOnly 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .95.

DEFINE VARIABLE svAsOfDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE endCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE endItemName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE endLocName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE endProdCategoryName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE endSalesRepName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE startCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startItemName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE startLocName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startProdCategoryName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startSalesRepName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE svAsOfDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "As Of Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE svEndCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "End Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svEndItemNo AS CHARACTER FORMAT "X(15)" 
     LABEL "End Item" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svEndLoc AS CHARACTER FORMAT "X(5)" 
     LABEL "End Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svEndLocBin AS CHARACTER FORMAT "X(8)" 
     LABEL "End Bin" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svEndProdCategory AS CHARACTER FORMAT "X(5)" 
     LABEL "End Prod Category" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndSalesRep AS CHARACTER FORMAT "X(3)" 
     LABEL "End Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svShowQOHOlderThanDays AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Only Show QOH Older Than" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE svStartCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartItemNo AS CHARACTER FORMAT "X(15)" 
     LABEL "Start Item" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svStartLoc AS CHARACTER FORMAT "X(5)" 
     LABEL "Start Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartLocBin AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Bin" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartProdCategory AS CHARACTER FORMAT "X(5)" 
     LABEL "Start Prod Category" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartSalesRep AS CHARACTER FORMAT "X(3)" 
     LABEL "Start Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svItemCode AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Stock", "S",
"Custom", "C",
"All", "A"
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE svSort AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer", "Customer",
"FG Item", "FG Item",
"Part", "Part",
"Product Category", "Product Category",
"Whs/Bin", "Whs/Bin"
     SIZE 70 BY 1 NO-UNDO.

DEFINE VARIABLE svAllCustNo AS LOGICAL INITIAL yes 
     LABEL "All Customers" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE svAllItemNo AS LOGICAL INITIAL yes 
     LABEL "All Items" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE svAllLoc AS LOGICAL INITIAL yes 
     LABEL "All Warehouses" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .95 NO-UNDO.

DEFINE VARIABLE svAllLocBin AS LOGICAL INITIAL yes 
     LABEL "All Bins" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE svAllProdCategory AS LOGICAL INITIAL yes 
     LABEL "All Product Categories" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE svAllSalesRep AS LOGICAL INITIAL yes 
     LABEL "All Sales Reps" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE svCustList AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE svDLMATOnly AS LOGICAL INITIAL no 
     LABEL "If Yes - DL/MAT Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE svIncludeCustomerOwnerdWarehouse AS LOGICAL INITIAL no 
     LABEL "Include Customer Ownerd Warehouse?" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE svIncludeInactiveItems AS LOGICAL INITIAL no 
     LABEL "Include Inactive Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE svIncludeZeroBalance AS LOGICAL INITIAL no 
     LABEL "Include Zero Balance?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE svOnlyCustomerOwnedWarehouse AS LOGICAL INITIAL no 
     LABEL "Only Customer Owned Warehouse?" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE svPrintCost AS LOGICAL INITIAL no 
     LABEL "Print Cost?" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE svPrintSetAndComponentsOnly AS LOGICAL INITIAL no 
     LABEL "Print Set and Components Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE svPrintSummaryByBinQty AS LOGICAL INITIAL no 
     LABEL "Print Summary By Bin Qty?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 23 COLON-ALIGNED WIDGET-ID 60
     svShowQOHOlderThanDays AT ROW 1.24 COL 63 COLON-ALIGNED HELP
          "Enter Only Show QOH Older Than Days" WIDGET-ID 266
     svAsOfDate AT ROW 2.91 COL 23 COLON-ALIGNED HELP
          "Enter As Of Date" WIDGET-ID 72
     btnCalendar-1 AT ROW 2.91 COL 41 WIDGET-ID 76
     svAsOfDateOption AT ROW 2.91 COL 44 COLON-ALIGNED HELP
          "Select Start Date Option" NO-LABEL WIDGET-ID 74
     svCustList AT ROW 4.57 COL 25 WIDGET-ID 48
     btnCustList AT ROW 4.57 COL 55 WIDGET-ID 46
     svAllCustNo AT ROW 5.76 COL 25 HELP
          "All Customers?" WIDGET-ID 56
     svStartCustNo AT ROW 6.95 COL 23 COLON-ALIGNED HELP
          "Enter Start Customer" WIDGET-ID 2
     startCustName AT ROW 6.95 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     svEndCustNo AT ROW 8.14 COL 23 COLON-ALIGNED HELP
          "Enter End Customer" WIDGET-ID 6
     endCustName AT ROW 8.14 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     svAllLoc AT ROW 9.81 COL 25 HELP
          "All Warehouses?" WIDGET-ID 158
     svStartLoc AT ROW 11 COL 23 COLON-ALIGNED HELP
          "Enter State Warehouse" WIDGET-ID 162
     startLocName AT ROW 11 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 156
     svEndLoc AT ROW 12.19 COL 23 COLON-ALIGNED HELP
          "Enter End Warehouse" WIDGET-ID 160
     endLocName AT ROW 12.19 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 154
     svAllLocBin AT ROW 13.86 COL 25 HELP
          "All Bins?" WIDGET-ID 226
     svStartLocBin AT ROW 15.05 COL 23 COLON-ALIGNED HELP
          "Enter Start Bin" WIDGET-ID 230
     svEndLocBin AT ROW 16.24 COL 23 COLON-ALIGNED HELP
          "Enter End Bin" WIDGET-ID 228
     svAllItemNo AT ROW 17.91 COL 25 HELP
          "All Items?" WIDGET-ID 164
     svStartItemNo AT ROW 19.1 COL 23 COLON-ALIGNED HELP
          "Enter Start Item" WIDGET-ID 168
     startItemName AT ROW 19.1 COL 46 COLON-ALIGNED NO-LABEL WIDGET-ID 172
     svEndItemNo AT ROW 20.29 COL 23 COLON-ALIGNED HELP
          "Enter End Item" WIDGET-ID 166
     endItemName AT ROW 20.29 COL 46 COLON-ALIGNED NO-LABEL WIDGET-ID 170
     svAllProdCategory AT ROW 21.95 COL 25 HELP
          "All Sales Reps?" WIDGET-ID 202
     svStartProdCategory AT ROW 23.14 COL 23 COLON-ALIGNED HELP
          "Enter Start Product Category" WIDGET-ID 206
     startProdCategoryName AT ROW 23.14 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 210
     svEndProdCategory AT ROW 24.33 COL 23 COLON-ALIGNED HELP
          "Enter End Product Category" WIDGET-ID 204
     endProdCategoryName AT ROW 24.33 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 208
     svAllSalesRep AT ROW 26 COL 25 HELP
          "All Sales Reps?" WIDGET-ID 108
     svStartSalesRep AT ROW 27.19 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep#" WIDGET-ID 112
     startSalesRepName AT ROW 27.19 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     svEndSalesRep AT ROW 28.38 COL 23 COLON-ALIGNED HELP
          "Enter Ending Sales Rep" WIDGET-ID 110
     endSalesRepName AT ROW 28.38 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     svSort AT ROW 30.05 COL 25 HELP
          "Select Sort Option" NO-LABEL WIDGET-ID 84
     svItemCode AT ROW 31.24 COL 25 HELP
          "Select Sort Option" NO-LABEL WIDGET-ID 270
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.8 BY 37.38.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     svPrintSetAndComponentsOnly AT ROW 32.67 COL 25 HELP
          "Select to Print Set and Components Only" WIDGET-ID 88
     svIncludeZeroBalance AT ROW 32.67 COL 66 HELP
          "Select to Include Zero Balance" WIDGET-ID 256
     svIncludeCustomerOwnerdWarehouse AT ROW 33.86 COL 25 HELP
          "Select to Include Customer Ownerd Warehouse" WIDGET-ID 252
     svPrintSummaryByBinQty AT ROW 33.86 COL 66 HELP
          "Select to Print Summary By Bin Qty" WIDGET-ID 258
     svOnlyCustomerOwnedWarehouse AT ROW 35.05 COL 25 HELP
          "Select to Only Customer Owned Warehouse" WIDGET-ID 254
     svIncludeInactiveItems AT ROW 35.05 COL 66 HELP
          "Select to Include Inactive Items" WIDGET-ID 260
     svPrintCost AT ROW 36.24 COL 25 HELP
          "Select to Print Cost" WIDGET-ID 262
     svDLMATOnly AT ROW 36.24 COL 66 HELP
          "Select to If Yes - DL/MAT Only" WIDGET-ID 264
     "Days" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 1.24 COL 72 WIDGET-ID 268
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 30.05 COL 16 WIDGET-ID 90
     "Item Code:" VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 31.24 COL 14 WIDGET-ID 274
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.8 BY 37.38
         TITLE "Report Parameters".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW sObject ASSIGN
         HEIGHT             = 37.38
         WIDTH              = 94.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB sObject 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW sObject
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN endCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endItemName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endLocName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endProdCategoryName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endSalesRepName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startItemName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startLocName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startProdCategoryName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startSalesRepName IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       svCompany:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 sObject
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svAsOfDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList sObject
ON CHOOSE OF btnCustList IN FRAME F-Main /* Preview */
DO:
    RUN sys/ref/CustListManager.w (svCompany, "AR15").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllCustNo sObject
ON VALUE-CHANGED OF svAllCustNo IN FRAME F-Main /* All Customers */
DO:
    {aoa/includes/svAllValueChanged.i svStartCustNo svEndCustNo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllItemNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllItemNo sObject
ON VALUE-CHANGED OF svAllItemNo IN FRAME F-Main /* All Items */
DO:
    {aoa/includes/svAllValueChanged.i svStartItemNo svEndItemNo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllLoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllLoc sObject
ON VALUE-CHANGED OF svAllLoc IN FRAME F-Main /* All Warehouses */
DO:
    {aoa/includes/svAllValueChanged.i svStartLoc svEndLoc}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllLocBin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllLocBin sObject
ON VALUE-CHANGED OF svAllLocBin IN FRAME F-Main /* All Bins */
DO:
    {aoa/includes/svAllValueChanged.i svStartLocBin svEndLocBin}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllProdCategory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllProdCategory sObject
ON VALUE-CHANGED OF svAllProdCategory IN FRAME F-Main /* All Product Categories */
DO:
    {aoa/includes/svAllValueChanged.i svStartProdCategory svEndProdCategory}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllSalesRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllSalesRep sObject
ON VALUE-CHANGED OF svAllSalesRep IN FRAME F-Main /* All Sales Reps */
DO:
    {aoa/includes/svAllValueChanged.i svStartSalesRep svEndSalesRep}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAsOfDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAsOfDate sObject
ON HELP OF svAsOfDate IN FRAME F-Main /* As Of Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAsOfDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAsOfDateOption sObject
ON VALUE-CHANGED OF svAsOfDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svAsOfDate &btnCalendar=1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCompany sObject
ON ENTRY OF svCompany IN FRAME F-Main /* Company */
DO:
  APPLY "ENTRY":U TO svAsOfDate.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCustList sObject
ON VALUE-CHANGED OF svCustList IN FRAME F-Main /* Use Defined Customer List */
DO:
  ASSIGN {&SELF-NAME}
      svStartCustNo:READ-ONLY = {&SELF-NAME}
      svEndCustNo:READ-ONLY   = {&SELF-NAME}
      btnCustList:SENSITIVE   = {&SELF-NAME}
      .
  IF {&SELF-NAME} THEN
  ASSIGN svAllCustNo:SCREEN-VALUE = "no".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndCustNo sObject
ON LEAVE OF svEndCustNo IN FRAME F-Main /* End Customer */
DO:
    endCustName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndItemNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndItemNo sObject
ON LEAVE OF svEndItemNo IN FRAME F-Main /* End Item */
DO:
    endItemName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndLoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndLoc sObject
ON LEAVE OF svEndLoc IN FRAME F-Main /* End Warehouse */
DO:
    endLocName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndProdCategory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndProdCategory sObject
ON LEAVE OF svEndProdCategory IN FRAME F-Main /* End Prod Category */
DO:
    endProdCategoryName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndSalesRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndSalesRep sObject
ON LEAVE OF svEndSalesRep IN FRAME F-Main /* End Sales Rep */
DO:
    endSalesRepName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartCustNo sObject
ON LEAVE OF svStartCustNo IN FRAME F-Main /* Start Customer */
DO:
    startCustName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartItemNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartItemNo sObject
ON LEAVE OF svStartItemNo IN FRAME F-Main /* Start Item */
DO:
    startItemName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartLoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartLoc sObject
ON LEAVE OF svStartLoc IN FRAME F-Main /* Start Warehouse */
DO:
    startLocName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartProdCategory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartProdCategory sObject
ON LEAVE OF svStartProdCategory IN FRAME F-Main /* Start Prod Category */
DO:
    startProdCategoryName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartSalesRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartSalesRep sObject
ON LEAVE OF svStartSalesRep IN FRAME F-Main /* Start Sales Rep */
DO:
    startSalesRepName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK sObject 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI sObject  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInitialize sObject 
PROCEDURE pInitialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphContainer AS HANDLE NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            hContainer = iphContainer
            svCompany:SCREEN-VALUE = DYNAMIC-FUNCTION('fGetCompany' IN hContainer)
            svCompany
            .

        APPLY "VALUE-CHANGED":U TO svAsOfDateOption.
        
        APPLY "VALUE-CHANGED":U TO svAllCustNo.
        APPLY "LEAVE":U TO svStartCustNo.
        APPLY "LEAVE":U TO svEndCustNo.
        
        APPLY "VALUE-CHANGED":U TO svAllLoc.
        APPLY "LEAVE":U TO svStartLoc.
        APPLY "LEAVE":U TO svEndLoc.
        
        APPLY "VALUE-CHANGED":U TO svAllLocBin.
        APPLY "LEAVE":U TO svStartLocBin.
        APPLY "LEAVE":U TO svEndLocBin.
        
        APPLY "VALUE-CHANGED":U TO svAllItemNo.
        APPLY "LEAVE":U TO svStartItemNo.
        APPLY "LEAVE":U TO svEndItemNo.
        
        APPLY "VALUE-CHANGED":U TO svAllProdCategory.
        APPLY "LEAVE":U TO svStartProdCategory.
        APPLY "LEAVE":U TO svEndProdCategory.
        
        APPLY "VALUE-CHANGED":U TO svAllSalesRep.
        APPLY "LEAVE":U TO svStartSalesRep.
        APPLY "LEAVE":U TO svEndSalesRep.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPopulateOptions sObject 
PROCEDURE pPopulateOptions :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphContainer AS HANDLE NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        hContainer = iphContainer.
        
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svAsOfDateOption:HANDLE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

