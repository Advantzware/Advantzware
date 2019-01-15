&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: r-booked.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: 
  Created: 

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
&Scoped-Define ENABLED-OBJECTS svCompany svCustList btnCustList svAllCustNo ~
svStartCustNo svEndCustNo svAllSalesRep svStartSalesRep svEndSalesRep ~
svAllProdCategory svStartProdCategory svEndProdCategory svStartOrderDate ~
btnCalendar-1 svStartOrderDateOption svEndOrderDate btnCalendar-2 ~
svEndOrderDateOption svStartDueDate btnCalendar-3 svStartDueDateOption ~
svEndDueDate btnCalendar-4 svEndDueDateOption svIncludePrepMiscChg ~
svExcludeSetComponents svExcludeTransferReleasesOrders svPrintOrderUnderPct ~
svUnderValue svPrintOrderOverPct svOverValue 
&Scoped-Define DISPLAYED-OBJECTS svCompany svCustList svAllCustNo ~
svStartCustNo startCustName svEndCustNo endCustName svAllSalesRep ~
svStartSalesRep startSalesRepName svEndSalesRep endSalesRepName ~
svAllProdCategory svStartProdCategory startProdCategoryName ~
svEndProdCategory endProdCategoryName svStartOrderDate ~
svStartOrderDateOption svEndOrderDate svEndOrderDateOption svStartDueDate ~
svStartDueDateOption svEndDueDate svEndDueDateOption svIncludePrepMiscChg ~
svExcludeSetComponents svExcludeTransferReleasesOrders svPrintOrderUnderPct ~
svUnderValue svPrintOrderOverPct svOverValue 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 btnCalendar-2 btnCalendar-3 ~
btnCalendar-4 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-3 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-4 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .95.

DEFINE VARIABLE svEndDueDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svEndOrderDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartDueDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartOrderDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE endCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.

DEFINE VARIABLE endProdCategoryName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.

DEFINE VARIABLE endSalesRepName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.

DEFINE VARIABLE startCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.

DEFINE VARIABLE startProdCategoryName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.

DEFINE VARIABLE startSalesRepName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1.

DEFINE VARIABLE svCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE svEndCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "End Customer" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndDueDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Due Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndOrderDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Order Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndProdCategory AS CHARACTER FORMAT "X(5)" 
     LABEL "End Prod Category" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndSalesRep AS CHARACTER FORMAT "X(3)" 
     LABEL "End SalesRep#" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svOverValue AS INTEGER FORMAT ">9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4.4 BY 1.

DEFINE VARIABLE svStartCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Customer" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartDueDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Due Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartOrderDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Order Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartProdCategory AS CHARACTER FORMAT "X(5)" 
     LABEL "Start Prod Category" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartSalesRep AS CHARACTER FORMAT "X(3)" 
     LABEL "Start SalesRep#" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svUnderValue AS INTEGER FORMAT ">9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4.4 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 82 BY 5.43.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 82 BY 4.29.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 82 BY 4.29.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 69 BY 5.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 69 BY 9.29.

DEFINE VARIABLE svAllCustNo AS LOGICAL INITIAL yes 
     LABEL "All Customers" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE svAllProdCategory AS LOGICAL INITIAL yes 
     LABEL "All Product Categories" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE svAllSalesRep AS LOGICAL INITIAL yes 
     LABEL "All Sales Reps" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE svCustList AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE svExcludeSetComponents AS LOGICAL INITIAL no 
     LABEL "Exclude Set Components" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE svExcludeTransferReleasesOrders AS LOGICAL INITIAL no 
     LABEL "Exclude Transfer Releases/Orders" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE svIncludePrepMiscChg AS LOGICAL INITIAL no 
     LABEL "Include Prep / Misc Charge" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE svPrintOrderOverPct AS LOGICAL INITIAL no 
     LABEL "Print Order Over(%) +" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.2 BY 1 NO-UNDO.

DEFINE VARIABLE svPrintOrderUnderPct AS LOGICAL INITIAL no 
     LABEL "Print Order Under(%) -" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 153 COLON-ALIGNED WIDGET-ID 60
     svCustList AT ROW 3.1 COL 27 WIDGET-ID 48
     btnCustList AT ROW 3.1 COL 62 WIDGET-ID 46
     svAllCustNo AT ROW 4.29 COL 27 HELP
          "All Customers?" WIDGET-ID 56
     svStartCustNo AT ROW 5.43 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Customer" WIDGET-ID 2
     startCustName AT ROW 5.38 COL 41.4 COLON-ALIGNED HELP
          "Enter Beginning Customer Name" NO-LABEL WIDGET-ID 4
     svEndCustNo AT ROW 6.62 COL 25 COLON-ALIGNED HELP
          "Enter Ending Customer" WIDGET-ID 6
     endCustName AT ROW 6.57 COL 41.4 COLON-ALIGNED HELP
          "Enter Ending Customer Name" NO-LABEL WIDGET-ID 8
     svAllSalesRep AT ROW 9.33 COL 27 HELP
          "All Sales Reps?" WIDGET-ID 58
     svStartSalesRep AT ROW 10.43 COL 25 COLON-ALIGNED HELP
          "Enter Beginning SalesRep#" WIDGET-ID 22
     startSalesRepName AT ROW 10.43 COL 42 COLON-ALIGNED HELP
          "Enter Beginning Customer Name" NO-LABEL WIDGET-ID 18
     svEndSalesRep AT ROW 11.62 COL 25 COLON-ALIGNED HELP
          "Enter Ending SalesRep" WIDGET-ID 20
     endSalesRepName AT ROW 11.62 COL 42 COLON-ALIGNED HELP
          "Enter Ending Customer Name" NO-LABEL WIDGET-ID 16
     svAllProdCategory AT ROW 14.33 COL 27 HELP
          "All Sales Reps?" WIDGET-ID 112
     svStartProdCategory AT ROW 15.43 COL 25 COLON-ALIGNED HELP
          "Enter Start Product Category" WIDGET-ID 90
     startProdCategoryName AT ROW 15.43 COL 41 COLON-ALIGNED NO-LABEL WIDGET-ID 210
     svEndProdCategory AT ROW 16.62 COL 25 COLON-ALIGNED HELP
          "Enter End Product Category" WIDGET-ID 88
     endProdCategoryName AT ROW 16.62 COL 41 COLON-ALIGNED NO-LABEL WIDGET-ID 208
     svStartOrderDate AT ROW 3.14 COL 108 COLON-ALIGNED HELP
          "Enter Start Order Date" WIDGET-ID 84
     btnCalendar-1 AT ROW 3.14 COL 125 WIDGET-ID 76
     svStartOrderDateOption AT ROW 3.14 COL 128 COLON-ALIGNED HELP
          "Select Start Receipt Date Option" NO-LABEL WIDGET-ID 86
     svEndOrderDate AT ROW 4.33 COL 108 COLON-ALIGNED HELP
          "Enter End Order Date" WIDGET-ID 80
     btnCalendar-2 AT ROW 4.33 COL 125 WIDGET-ID 78
     svEndOrderDateOption AT ROW 4.33 COL 128 COLON-ALIGNED HELP
          "Select End Order Date Option" NO-LABEL WIDGET-ID 82
     svStartDueDate AT ROW 5.62 COL 108 COLON-ALIGNED HELP
          "Enter Start Order Date" WIDGET-ID 220
     btnCalendar-3 AT ROW 5.62 COL 125 WIDGET-ID 212
     svStartDueDateOption AT ROW 5.62 COL 128 COLON-ALIGNED HELP
          "Select Start Receipt Date Option" NO-LABEL WIDGET-ID 222
     svEndDueDate AT ROW 6.81 COL 108 COLON-ALIGNED HELP
          "Enter End Order Date" WIDGET-ID 216
     btnCalendar-4 AT ROW 6.81 COL 125 WIDGET-ID 214
     svEndDueDateOption AT ROW 6.81 COL 128 COLON-ALIGNED HELP
          "Select End Order Date Option" NO-LABEL WIDGET-ID 218
     svIncludePrepMiscChg AT ROW 9.33 COL 110 WIDGET-ID 42
     svExcludeSetComponents AT ROW 10.52 COL 110 WIDGET-ID 94
     svExcludeTransferReleasesOrders AT ROW 11.71 COL 110 WIDGET-ID 96
     svPrintOrderUnderPct AT ROW 13.14 COL 110 WIDGET-ID 98
     svUnderValue AT ROW 13.14 COL 134 COLON-ALIGNED HELP
          "Enter Beginning Prod Category#" NO-LABEL WIDGET-ID 104
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160.6 BY 18.81.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     svPrintOrderOverPct AT ROW 14.33 COL 110 WIDGET-ID 102
     svOverValue AT ROW 14.33 COL 134 COLON-ALIGNED HELP
          "Enter Beginning Prod Category#" NO-LABEL WIDGET-ID 106
     "Note: Profit Includes Estimate Markups and Commissions." VIEW-AS TEXT
          SIZE 54.6 BY .71 AT ROW 17.05 COL 96.4 WIDGET-ID 110
          FGCOLOR 1 
     "(Prep / Misc Charges will Display 'P' or 'M' for Product Code)" VIEW-AS TEXT
          SIZE 58 BY .95 AT ROW 16 COL 95 WIDGET-ID 108
     RECT-1 AT ROW 2.67 COL 4 WIDGET-ID 224
     RECT-2 AT ROW 8.86 COL 4 WIDGET-ID 226
     RECT-3 AT ROW 13.86 COL 4 WIDGET-ID 228
     RECT-4 AT ROW 2.67 COL 89 WIDGET-ID 230
     RECT-5 AT ROW 8.86 COL 89 WIDGET-ID 232
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160.6 BY 18.81
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
         HEIGHT             = 18.81
         WIDTH              = 160.6.
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
   NOT-VISIBLE FRAME-NAME Custom                                        */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-3 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-4 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN endCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endProdCategoryName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endSalesRepName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startCustName IN FRAME F-Main
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
  {methods/btnCalendar.i svStartOrderDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 sObject
ON CHOOSE OF btnCalendar-2 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndOrderDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-3 sObject
ON CHOOSE OF btnCalendar-3 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svStartDueDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-4 sObject
ON CHOOSE OF btnCalendar-4 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndDueDate}
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


&Scoped-define SELF-NAME svCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCompany sObject
ON ENTRY OF svCompany IN FRAME F-Main /* Company */
DO:
  APPLY "ENTRY":U TO svCustList.
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


&Scoped-define SELF-NAME svEndDueDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndDueDate sObject
ON HELP OF svEndDueDate IN FRAME F-Main /* End Due Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndDueDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndDueDateOption sObject
ON VALUE-CHANGED OF svEndDueDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svEndDueDate &btnCalendar=4}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndOrderDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndOrderDate sObject
ON HELP OF svEndOrderDate IN FRAME F-Main /* End Order Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndOrderDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndOrderDateOption sObject
ON VALUE-CHANGED OF svEndOrderDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svEndOrderDate &btnCalendar=2}
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
ON LEAVE OF svEndSalesRep IN FRAME F-Main /* End SalesRep# */
DO:
    endSalesRepName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svOverValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svOverValue sObject
ON LEAVE OF svOverValue IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
    
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


&Scoped-define SELF-NAME svStartDueDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartDueDate sObject
ON HELP OF svStartDueDate IN FRAME F-Main /* Start Due Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartDueDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartDueDateOption sObject
ON VALUE-CHANGED OF svStartDueDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svStartDueDate &btnCalendar=3}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartOrderDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartOrderDate sObject
ON HELP OF svStartOrderDate IN FRAME F-Main /* Start Order Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartOrderDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartOrderDateOption sObject
ON VALUE-CHANGED OF svStartOrderDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svStartOrderDate &btnCalendar=1}
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
ON LEAVE OF svStartSalesRep IN FRAME F-Main /* Start SalesRep# */
DO:
    startSalesRepName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svUnderValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svUnderValue sObject
ON LEAVE OF svUnderValue IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
    
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
        APPLY "VALUE-CHANGED":U TO svCustList.
        APPLY "VALUE-CHANGED":U TO svAllCustNo.
        APPLY "LEAVE":U TO svStartCustNo.
        APPLY "LEAVE":U TO svEndCustNo.
        
        APPLY "VALUE-CHANGED":U TO svStartOrderDateOption.
        APPLY "VALUE-CHANGED":U TO svEndOrderDateOption.
        APPLY "VALUE-CHANGED":U TO svStartDueDateOption.
        APPLY "VALUE-CHANGED":U TO svEndDueDateOption.

        APPLY "VALUE-CHANGED":U TO svAllSalesRep.
        APPLY "LEAVE":U TO svStartSalesRep.
        APPLY "LEAVE":U TO svEndSalesRep.
        
        APPLY "VALUE-CHANGED":U TO svAllProdCategory.
        APPLY "LEAVE":U TO svStartProdCategory.
        APPLY "LEAVE":U TO svEndProdCategory.
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
        ASSIGN
            hContainer = iphContainer
            .
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartOrderDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndOrderDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartDueDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndDueDateOption:HANDLE).
        
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

