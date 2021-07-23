&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: r-ordopn.w

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
&Scoped-Define ENABLED-OBJECTS svCompany svCustList btnCustList svAllCustNo ~
svStartCustNo svEndCustNo svAllUserID svStartUserID svEndUserID svAllItemNo ~
svStartItemNo svEndItemNo svSubRpt_PrintJobQtyDetails svDropOrderUnderrun ~
svIncludeZeroOrderBalanceItems svIncludeZeroQtyWIPItems ~
svIncludeZeroQtyActReleaseQty svIncludeJobsQOH svIncludeInactiveItems ~
svStartOrderDate btnCalendar-1 svStartOrderDateOption svEndOrderDate ~
btnCalendar-2 svEndOrderDateOption svStartDueDate btnCalendar-3 ~
svStartDueDateOption svEndDueDate btnCalendar-4 svEndDueDateOption ~
svAllSalesRep svStartSalesRep svEndSalesRep svAllPONumber svStartPONumber ~
svEndPONumber svAllJobNo svStartJobNo svStartJobNo2 svEndJobNo svEndJobNo2 ~
svAllCAD svStartCAD svEndCAD svPrimarySort svSecondarySort svJobStatus ~
svOrderStatus svWIPQty btnAddEmail svRecipients 
&Scoped-Define DISPLAYED-OBJECTS svCompany svCustList svAllCustNo ~
svStartCustNo startCustName svEndCustNo endCustName svAllUserID ~
svStartUserID startUserIDName svEndUserID endUserIDName svAllItemNo ~
svStartItemNo startItemName svEndItemNo endItemName ~
svSubRpt_PrintJobQtyDetails svDropOrderUnderrun ~
svIncludeZeroOrderBalanceItems svIncludeZeroQtyWIPItems ~
svIncludeZeroQtyActReleaseQty svIncludeJobsQOH svIncludeInactiveItems ~
svStartOrderDate svStartOrderDateOption svEndOrderDate svEndOrderDateOption ~
svStartDueDate svStartDueDateOption svEndDueDate svEndDueDateOption ~
svAllSalesRep svStartSalesRep startSalesRepName svEndSalesRep ~
endSalesRepName svAllPONumber svStartPONumber svEndPONumber svAllJobNo ~
svStartJobNo svStartJobNo2 svEndJobNo svEndJobNo2 svAllCAD svStartCAD ~
svEndCAD svPrimarySort svSecondarySort svJobStatus svOrderStatus svWIPQty ~
svRecipients 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 btnCalendar-2 btnCalendar-3 ~
btnCalendar-4 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddEmail 
     IMAGE-UP FILE "AOA/images/navigate_plus.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Email" 
     SIZE 4.4 BY 1.05 TOOLTIP "Add Recipents".

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

DEFINE VARIABLE svRecipients AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 70 BY 2.86
     BGCOLOR 15 .

DEFINE VARIABLE endCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE endItemName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE endSalesRepName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE endUserIDName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startItemName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE startSalesRepName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE startUserIDName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE svCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE svEndCAD AS CHARACTER FORMAT "X(8)" 
     LABEL "End CAD" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svEndCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "End Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svEndDueDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Due Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndItemNo AS CHARACTER FORMAT "X(15)" 
     LABEL "End Item" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svEndJobNo AS CHARACTER FORMAT "X(6)" 
     LABEL "End Job" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svEndJobNo2 AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE svEndOrderDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Order Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndPONumber AS CHARACTER FORMAT "X(8)" 
     LABEL "End PO Number" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svEndSalesRep AS CHARACTER FORMAT "X(3)" 
     LABEL "End Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svEndUserID AS CHARACTER FORMAT "X(8)" 
     LABEL "End User ID" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartCAD AS CHARACTER FORMAT "X(15)" 
     LABEL "Start CAD" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svStartCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartDueDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Due Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartItemNo AS CHARACTER FORMAT "X(15)" 
     LABEL "Start Item" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svStartJobNo AS CHARACTER FORMAT "X(6)" 
     LABEL "Start Job" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svStartJobNo2 AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE svStartOrderDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Order Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartPONumber AS CHARACTER FORMAT "X(15)" 
     LABEL "Start PO Number" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svStartSalesRep AS CHARACTER FORMAT "X(3)" 
     LABEL "Start Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svStartUserID AS CHARACTER FORMAT "X(8)" 
     LABEL "Start User ID" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svJobStatus AS CHARACTER INITIAL "All" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"All", "All"
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE svOrderStatus AS CHARACTER INITIAL "All" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"All", "All"
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE svPrimarySort AS CHARACTER INITIAL "Customer" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer", "Customer",
"Line Due Date", "Line Due Date",
"Release Due Date", "Release Due Date",
"Sales Rep", "Sales Rep"
     SIZE 81 BY 1 NO-UNDO.

DEFINE VARIABLE svSecondarySort AS CHARACTER INITIAL "PO" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "PO", "PO",
"Item", "Item",
"Cust Part", "Cust Part",
"FG Item Name", "FG Item Name",
"Order", "Order",
"Due Date", "Due Date",
"CAD", "CAD"
     SIZE 88 BY 1 NO-UNDO.

DEFINE VARIABLE svWIPQty AS CHARACTER INITIAL "Order" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Order Qty - OH - Ship", "Order",
"Job Qty - Rcpts", "Job"
     SIZE 44.4 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 80 BY 5.19.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 109 BY 5.24.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 83 BY 3.57.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 109 BY 5.24.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 43 BY 4.05.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 28 BY 4.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 80 BY 5.24.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 36 BY 4.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 80 BY 4.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 80 BY 4.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 109 BY 4.

DEFINE VARIABLE svAllCAD AS LOGICAL INITIAL yes 
     LABEL "All CAD" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .95 NO-UNDO.

DEFINE VARIABLE svAllCustNo AS LOGICAL INITIAL yes 
     LABEL "All Customers" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE svAllItemNo AS LOGICAL INITIAL yes 
     LABEL "All Items" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE svAllJobNo AS LOGICAL INITIAL yes 
     LABEL "All Jobs" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE svAllPONumber AS LOGICAL INITIAL yes 
     LABEL "All PO Numbers" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .95 NO-UNDO.

DEFINE VARIABLE svAllSalesRep AS LOGICAL INITIAL yes 
     LABEL "All Sales Reps" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE svAllUserID AS LOGICAL INITIAL yes 
     LABEL "All User IDs" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE svCustList AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE svDropOrderUnderrun AS LOGICAL INITIAL no 
     LABEL "Drop Order Underrun%" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE svIncludeInactiveItems AS LOGICAL INITIAL no 
     LABEL "Include Inactive Items" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svIncludeJobsQOH AS LOGICAL INITIAL no 
     LABEL "Include Jobs with QOH" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE svIncludeZeroOrderBalanceItems AS LOGICAL INITIAL yes 
     LABEL "Include 0 Order Balance Items" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE svIncludeZeroQtyActReleaseQty AS LOGICAL INITIAL yes 
     LABEL "Include 0 Qty / Act. Release Qty = 0" 
     VIEW-AS TOGGLE-BOX
     SIZE 39.2 BY 1 NO-UNDO.

DEFINE VARIABLE svIncludeZeroQtyWIPItems AS LOGICAL INITIAL yes 
     LABEL "Include 0 Qty WIP Items" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE svSubRpt_PrintJobQtyDetails AS LOGICAL INITIAL no 
     LABEL "Print Job Qty Details" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 185 COLON-ALIGNED WIDGET-ID 60
     svCustList AT ROW 2.91 COL 20 WIDGET-ID 48
     btnCustList AT ROW 2.91 COL 50 WIDGET-ID 46
     svAllCustNo AT ROW 4.1 COL 20 HELP
          "All Customers?" WIDGET-ID 56
     svStartCustNo AT ROW 5.29 COL 18 COLON-ALIGNED HELP
          "Enter Start Customer" WIDGET-ID 2
     startCustName AT ROW 5.29 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     svEndCustNo AT ROW 6.48 COL 18 COLON-ALIGNED HELP
          "Enter End Customer" WIDGET-ID 6
     endCustName AT ROW 6.48 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     svAllUserID AT ROW 8.38 COL 20 HELP
          "All User IDs?" WIDGET-ID 158
     svStartUserID AT ROW 9.57 COL 18 COLON-ALIGNED HELP
          "Enter Start User ID" WIDGET-ID 162
     startUserIDName AT ROW 9.57 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 156
     svEndUserID AT ROW 10.76 COL 18 COLON-ALIGNED HELP
          "Enter End User ID" WIDGET-ID 160
     endUserIDName AT ROW 10.76 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 154
     svAllItemNo AT ROW 12.67 COL 20 HELP
          "All Items?" WIDGET-ID 164
     svStartItemNo AT ROW 13.86 COL 18 COLON-ALIGNED HELP
          "Enter Start Item" WIDGET-ID 168
     startItemName AT ROW 13.86 COL 41 COLON-ALIGNED NO-LABEL WIDGET-ID 172
     svEndItemNo AT ROW 15.05 COL 18 COLON-ALIGNED HELP
          "Enter End Item" WIDGET-ID 166
     endItemName AT ROW 15.05 COL 41 COLON-ALIGNED NO-LABEL WIDGET-ID 170
     svSubRpt_PrintJobQtyDetails AT ROW 16.95 COL 3 HELP
          "Select to Print Job Qty Details" WIDGET-ID 88
     svDropOrderUnderrun AT ROW 18.14 COL 3 HELP
          "Select to Drop Order Underrun%" WIDGET-ID 234
     svIncludeZeroOrderBalanceItems AT ROW 19.33 COL 3 HELP
          "Select to Include 0 Order Balance Items" WIDGET-ID 224
     svIncludeZeroQtyWIPItems AT ROW 20.52 COL 3 HELP
          "Select to Include 0 Qty WIP Items" WIDGET-ID 226
     svIncludeZeroQtyActReleaseQty AT ROW 18.14 COL 37 HELP
          "Select to Include 0 Qty / Act. Release Qty = 0" WIDGET-ID 222
     svIncludeJobsQOH AT ROW 19.33 COL 37 HELP
          "Select to Include Jobs with QOH" WIDGET-ID 230
     svIncludeInactiveItems AT ROW 20.52 COL 37 HELP
          "Select to Include Inactive Items" WIDGET-ID 228
     svStartOrderDate AT ROW 2.91 COL 101 COLON-ALIGNED HELP
          "Enter Start Order Date" WIDGET-ID 122
     btnCalendar-1 AT ROW 2.91 COL 119 WIDGET-ID 114
     svStartOrderDateOption AT ROW 2.91 COL 122 COLON-ALIGNED HELP
          "Select Start Order Date Option" NO-LABEL WIDGET-ID 124
     svEndOrderDate AT ROW 4.1 COL 101 COLON-ALIGNED HELP
          "Enter End Order Date" WIDGET-ID 118
     btnCalendar-2 AT ROW 4.1 COL 119 WIDGET-ID 116
     svEndOrderDateOption AT ROW 4.1 COL 122 COLON-ALIGNED HELP
          "Select End Order Date Option" NO-LABEL WIDGET-ID 120
     svStartDueDate AT ROW 5.29 COL 101 COLON-ALIGNED HELP
          "Enter Start Due Date" WIDGET-ID 134
     btnCalendar-3 AT ROW 5.29 COL 119 WIDGET-ID 126
     svStartDueDateOption AT ROW 5.29 COL 122 COLON-ALIGNED HELP
          "Select Start Due Date Option" NO-LABEL WIDGET-ID 136
     svEndDueDate AT ROW 6.48 COL 101 COLON-ALIGNED HELP
          "Enter End Due Date" WIDGET-ID 130
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 192.2 BY 26.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     btnCalendar-4 AT ROW 6.48 COL 119 WIDGET-ID 128
     svEndDueDateOption AT ROW 6.48 COL 122 COLON-ALIGNED HELP
          "Select End Due Date Option" NO-LABEL WIDGET-ID 132
     svAllSalesRep AT ROW 8.38 COL 103 HELP
          "All Sales Reps?" WIDGET-ID 108
     svStartSalesRep AT ROW 9.57 COL 101 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep" WIDGET-ID 112
     startSalesRepName AT ROW 9.57 COL 110 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     svEndSalesRep AT ROW 10.76 COL 101 COLON-ALIGNED HELP
          "Enter Ending Sales Rep" WIDGET-ID 110
     endSalesRepName AT ROW 10.76 COL 110 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     svAllPONumber AT ROW 12.91 COL 103 HELP
          "All PO Numbers?" WIDGET-ID 142
     svStartPONumber AT ROW 13.86 COL 101 COLON-ALIGNED HELP
          "Enter Start PO Number" WIDGET-ID 146
     svEndPONumber AT ROW 15.05 COL 101 COLON-ALIGNED HELP
          "Enter End PO Number" WIDGET-ID 144
     svAllJobNo AT ROW 12.67 COL 139 HELP
          "All Jobs?" WIDGET-ID 174
     svStartJobNo AT ROW 13.86 COL 137 COLON-ALIGNED HELP
          "Enter Start Job" WIDGET-ID 178
     svStartJobNo2 AT ROW 13.86 COL 148 COLON-ALIGNED HELP
          "Enter Start Job Run" WIDGET-ID 180
     svEndJobNo AT ROW 15.05 COL 137 COLON-ALIGNED HELP
          "Enter End Job" WIDGET-ID 176
     svEndJobNo2 AT ROW 15.05 COL 148 COLON-ALIGNED HELP
          "Enter End Job Run" WIDGET-ID 182
     svAllCAD AT ROW 12.67 COL 169 HELP
          "All PO Numbers?" WIDGET-ID 148
     svStartCAD AT ROW 13.86 COL 167 COLON-ALIGNED HELP
          "Enter Start CAD" WIDGET-ID 152
     svEndCAD AT ROW 15.05 COL 167 COLON-ALIGNED HELP
          "Enter End CAD" WIDGET-ID 150
     svPrimarySort AT ROW 16.95 COL 103 HELP
          "Select Primary Sort Option" NO-LABEL WIDGET-ID 84
     svSecondarySort AT ROW 18.14 COL 103 HELP
          "Select Secondary Sort Option" NO-LABEL WIDGET-ID 202
     svJobStatus AT ROW 19.33 COL 103 HELP
          "Select Job Status" NO-LABEL WIDGET-ID 210
     svOrderStatus AT ROW 19.33 COL 162 HELP
          "Select Order Status" NO-LABEL WIDGET-ID 214
     svWIPQty AT ROW 20.52 COL 103 HELP
          "Select QIP Qty" NO-LABEL WIDGET-ID 218
     btnAddEmail AT ROW 24.33 COL 61 HELP
          "Add Recipents" WIDGET-ID 636
     svRecipients AT ROW 22.67 COL 67 NO-LABEL WIDGET-ID 600
     "Primary Sort By:" VIEW-AS TEXT
          SIZE 16 BY 1 AT ROW 16.95 COL 86 WIDGET-ID 90
     "Job Status:" VIEW-AS TEXT
          SIZE 11 BY 1 AT ROW 19.33 COL 91 WIDGET-ID 242
     "2ndary Sort By:" VIEW-AS TEXT
          SIZE 16 BY 1 AT ROW 18.14 COL 86 WIDGET-ID 208
     "WIP Qty:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 20.52 COL 93 WIDGET-ID 246
     "Email" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 22.67 COL 61 WIDGET-ID 640
     "Recipients:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 23.38 COL 56 WIDGET-ID 602
     "Order Status:" VIEW-AS TEXT
          SIZE 13 BY 1 AT ROW 19.33 COL 148 WIDGET-ID 244
     RECT-1 AT ROW 2.67 COL 2 WIDGET-ID 248
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 192.2 BY 26.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RECT-2 AT ROW 2.67 COL 83 WIDGET-ID 250
     RECT-3 AT ROW 12.43 COL 83 WIDGET-ID 252
     RECT-4 AT ROW 12.43 COL 127 WIDGET-ID 254
     RECT-5 AT ROW 16.71 COL 2 WIDGET-ID 256
     RECT-6 AT ROW 12.43 COL 156 WIDGET-ID 258
     RECT-7 AT ROW 12.43 COL 2 WIDGET-ID 260
     RECT-8 AT ROW 8.14 COL 2 WIDGET-ID 262
     RECT-9 AT ROW 8.14 COL 83 WIDGET-ID 264
     RECT-10 AT ROW 16.71 COL 83 WIDGET-ID 266
     RECT-11 AT ROW 22.19 COL 55 WIDGET-ID 638
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 192.2 BY 26
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
         HEIGHT             = 26
         WIDTH              = 192.2.
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
/* SETTINGS FOR FILL-IN endItemName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endSalesRepName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endUserIDName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-10 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-11 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-8 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-9 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startItemName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startSalesRepName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startUserIDName IN FRAME F-Main
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

&Scoped-define SELF-NAME btnAddEmail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddEmail sObject
ON CHOOSE OF btnAddEmail IN FRAME F-Main /* Email */
DO:
    DEFINE VARIABLE cRecipients AS CHARACTER NO-UNDO.
    
    cRecipients = svRecipients:SCREEN-VALUE.
    RUN AOA/Recipients.w (INPUT-OUTPUT cRecipients).
    svRecipients:SCREEN-VALUE = cRecipients.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define SELF-NAME svAllCAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllCAD sObject
ON VALUE-CHANGED OF svAllCAD IN FRAME F-Main /* All CAD */
DO:
    {aoa/includes/svAllValueChanged.i svStartCAD svEndCAD}
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


&Scoped-define SELF-NAME svAllJobNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllJobNo sObject
ON VALUE-CHANGED OF svAllJobNo IN FRAME F-Main /* All Jobs */
DO:
    {aoa/includes/svAllValueChanged.i svStartJobNo svEndJobNo}
    {aoa/includes/svAllValueChanged.i svStartJobNo2 svEndJobNo2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllPONumber
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllPONumber sObject
ON VALUE-CHANGED OF svAllPONumber IN FRAME F-Main /* All PO Numbers */
DO:
    {aoa/includes/svAllValueChanged.i svStartPONumber svEndPONumber}
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


&Scoped-define SELF-NAME svAllUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllUserID sObject
ON VALUE-CHANGED OF svAllUserID IN FRAME F-Main /* All User IDs */
DO:
    {aoa/includes/svAllValueChanged.i svStartUserID svEndUserID}
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


&Scoped-define SELF-NAME svDropOrderUnderrun
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svDropOrderUnderrun sObject
ON VALUE-CHANGED OF svDropOrderUnderrun IN FRAME F-Main /* Drop Order Underrun% */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME svEndItemNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndItemNo sObject
ON LEAVE OF svEndItemNo IN FRAME F-Main /* End Item */
DO:
    endItemName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
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


&Scoped-define SELF-NAME svEndSalesRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndSalesRep sObject
ON LEAVE OF svEndSalesRep IN FRAME F-Main /* End Sales Rep */
DO:
    endSalesRepName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndUserID sObject
ON LEAVE OF svEndUserID IN FRAME F-Main /* End User ID */
DO:
    endUserIDName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svIncludeInactiveItems
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svIncludeInactiveItems sObject
ON VALUE-CHANGED OF svIncludeInactiveItems IN FRAME F-Main /* Include Inactive Items */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svIncludeJobsQOH
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svIncludeJobsQOH sObject
ON VALUE-CHANGED OF svIncludeJobsQOH IN FRAME F-Main /* Include Jobs with QOH */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svIncludeZeroOrderBalanceItems
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svIncludeZeroOrderBalanceItems sObject
ON VALUE-CHANGED OF svIncludeZeroOrderBalanceItems IN FRAME F-Main /* Include 0 Order Balance Items */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svIncludeZeroQtyActReleaseQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svIncludeZeroQtyActReleaseQty sObject
ON VALUE-CHANGED OF svIncludeZeroQtyActReleaseQty IN FRAME F-Main /* Include 0 Qty / Act. Release Qty = 0 */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svIncludeZeroQtyWIPItems
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svIncludeZeroQtyWIPItems sObject
ON VALUE-CHANGED OF svIncludeZeroQtyWIPItems IN FRAME F-Main /* Include 0 Qty WIP Items */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svJobStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svJobStatus sObject
ON VALUE-CHANGED OF svJobStatus IN FRAME F-Main
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svOrderStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svOrderStatus sObject
ON VALUE-CHANGED OF svOrderStatus IN FRAME F-Main
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME svStartItemNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartItemNo sObject
ON LEAVE OF svStartItemNo IN FRAME F-Main /* Start Item */
DO:
    startItemName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
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


&Scoped-define SELF-NAME svStartSalesRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartSalesRep sObject
ON LEAVE OF svStartSalesRep IN FRAME F-Main /* Start Sales Rep */
DO:
    startSalesRepName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartUserID sObject
ON LEAVE OF svStartUserID IN FRAME F-Main /* Start User ID */
DO:
    startUserIDName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svWIPQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svWIPQty sObject
ON VALUE-CHANGED OF svWIPQty IN FRAME F-Main
DO:
  assign {&self-name}.
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

        APPLY "VALUE-CHANGED":U TO svStartOrderDateOption.
        APPLY "VALUE-CHANGED":U TO svEndOrderDateOption.
        
        APPLY "VALUE-CHANGED":U TO svStartDueDateOption.
        APPLY "VALUE-CHANGED":U TO svEndDueDateOption.
        
        APPLY "VALUE-CHANGED":U TO svAllSalesRep.
        APPLY "LEAVE":U TO svStartSalesRep.
        APPLY "LEAVE":U TO svEndSalesRep.
        
        APPLY "VALUE-CHANGED":U TO svAllCustNo.
        APPLY "LEAVE":U TO svStartCustNo.
        APPLY "LEAVE":U TO svEndCustNo.
        
        APPLY "VALUE-CHANGED":U TO svAllPONumber.
        
        APPLY "VALUE-CHANGED":U TO svAllCAD.
        
        APPLY "VALUE-CHANGED":U TO svAllUserID.
        APPLY "LEAVE":U TO svStartUserID.
        APPLY "LEAVE":U TO svEndUserID.
        
        APPLY "VALUE-CHANGED":U TO svAllItemNo.
        APPLY "LEAVE":U TO svStartItemNo.
        APPLY "LEAVE":U TO svEndItemNo.
        
        APPLY "VALUE-CHANGED":U TO svAllJobNo.
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
        
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartOrderDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndOrderDateOption:HANDLE).
        
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartDueDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndDueDateOption:HANDLE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
