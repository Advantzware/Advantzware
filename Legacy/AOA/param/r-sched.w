&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: r-sched.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 8.23.2017

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
&Scoped-Define ENABLED-OBJECTS svCompany svLocation svCustList btnCustList ~
svAllCustNo svStartCustNo svEndCustNo svAllOrderNo svStartOrderNo ~
svEndOrderNo svAllShipFrom svStartShipFrom svEndShipFrom svAllLoc ~
svStartLoc svEndLoc svAllSalesRep svStartSalesRep svEndSalesRep svAllCSR ~
svStartCSR svEndCSR svPrintOHQty svAllItemNo svStartItemNo svEndItemNo ~
svAllCarrier svStartCarrier svEndCarrier svAllProdCategory ~
svStartProdCategory svEndProdCategory svStartReleaseDate btnCalendar-1 ~
svStartReleaseDateOption svEndReleaseDate btnCalendar-2 ~
svEndReleaseDateOption svSubTotalByCustomerNo svOnlyNegativeAvailable ~
svOnlyNegOHRelQty svSubRpt_PrintScheduleStats svSort svScheduled svLate ~
svPastLastShipDate svActual svBackorder svBillOfLading svInvoiceUnposted ~
svCompleted svSubRpt_PrintSpecNotes svStartSpecNote svEndSpecNote ~
btnAddEmail svRecipients 
&Scoped-Define DISPLAYED-OBJECTS svCompany svLocation svCustList ~
svAllCustNo svStartCustNo startCustName svEndCustNo endCustName ~
svAllOrderNo svStartOrderNo svEndOrderNo svAllShipFrom svStartShipFrom ~
svEndShipFrom svAllLoc svStartLoc startLocName svEndLoc endLocName ~
svAllSalesRep svStartSalesRep startSalesRepName svEndSalesRep ~
endSalesRepName svAllCSR svStartCSR startCSRName svEndCSR endCSRName ~
svPrintOHQty svAllItemNo svStartItemNo startItemName svEndItemNo ~
endItemName svAllCarrier svStartCarrier startCarrierName svEndCarrier ~
endCarrierName svAllProdCategory svStartProdCategory startProdCategoryName ~
svEndProdCategory endProdCategoryName svStartReleaseDate ~
svStartReleaseDateOption svEndReleaseDate svEndReleaseDateOption ~
svSubTotalByCustomerNo svOnlyNegativeAvailable svOnlyNegOHRelQty ~
svSubRpt_PrintScheduleStats svSort svScheduled svLate svPastLastShipDate ~
svActual svBackorder svBillOfLading svInvoiceUnposted svCompleted ~
svSubRpt_PrintSpecNotes svStartSpecNote svEndSpecNote svRecipients 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 btnCalendar-2 

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

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .95.

DEFINE VARIABLE svEndReleaseDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartReleaseDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svRecipients AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 70 BY 2.86
     BGCOLOR 15 .

DEFINE VARIABLE endCarrierName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1.

DEFINE VARIABLE endCSRName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1.

DEFINE VARIABLE endCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1.

DEFINE VARIABLE endItemName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.

DEFINE VARIABLE endLocName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1.

DEFINE VARIABLE endProdCategoryName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE endSalesRepName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE startCarrierName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1.

DEFINE VARIABLE startCSRName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1.

DEFINE VARIABLE startCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1.

DEFINE VARIABLE startItemName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.

DEFINE VARIABLE startLocName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1.

DEFINE VARIABLE startProdCategoryName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE startSalesRepName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE svCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE svEndCarrier AS CHARACTER FORMAT "X(5)" 
     LABEL "End Carrier" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svEndCSR AS CHARACTER FORMAT "x(10)" INITIAL "0" 
     LABEL "End CSR" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE svEndCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "End Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svEndItemNo AS CHARACTER FORMAT "X(15)" 
     LABEL "End Item" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE svEndLoc AS CHARACTER FORMAT "X(5)" 
     LABEL "End Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svEndOrderNo AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "End" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svEndProdCategory AS CHARACTER FORMAT "X(5)" 
     LABEL "End Prod Cat" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndReleaseDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Rel Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndSalesRep AS CHARACTER FORMAT "X(3)" 
     LABEL "End Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svEndShipFrom AS CHARACTER FORMAT "X(5)" 
     LABEL "End From" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svEndSpecNote AS CHARACTER FORMAT "X(3)" 
     LABEL "End Spec" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svLocation AS CHARACTER FORMAT "X(5)" 
     LABEL "Location" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE svStartCarrier AS CHARACTER FORMAT "X(5)" 
     LABEL "Start Carrier" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svStartCSR AS CHARACTER FORMAT "x(10)" INITIAL "0" 
     LABEL "Start CSR" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE svStartCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartItemNo AS CHARACTER FORMAT "X(15)" 
     LABEL "Start Item" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE svStartLoc AS CHARACTER FORMAT "X(5)" 
     LABEL "Start Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartOrderNo AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Start Order" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svStartProdCategory AS CHARACTER FORMAT "X(5)" 
     LABEL "Start Prod Cat" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartReleaseDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Rel Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartSalesRep AS CHARACTER FORMAT "X(3)" 
     LABEL "Start Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svStartShipFrom AS CHARACTER FORMAT "X(5)" 
     LABEL "Start Ship From" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svStartSpecNote AS CHARACTER FORMAT "X(3)" 
     LABEL "Start Spec" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svPrintOHQty AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Qty OH = 0", "Qty OH = 0",
"Qty OH < Order Qty", "Qty OH < Order Qty",
"Qty OH > Order Qty", "Qty OH > Order Qty",
"All", "All"
     SIZE 23 BY 3.33 NO-UNDO.

DEFINE VARIABLE svSort AS CHARACTER INITIAL "Customer No" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Customer No", "Customer No",
"Release Date", "Release Date",
"Item No", "Item No",
"Item Name", "Item Name",
"Territory", "Territory",
"Carrier", "Carrier",
"Credit", "Credit"
     SIZE 17.4 BY 6.43 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 71 BY 5.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 33.8 BY 3.81.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 71 BY 4.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 35 BY 5.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 34 BY 6.91.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 29 BY 5.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 34 BY 10.71.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 83 BY 3.57.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 33 BY 4.05.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 65 BY 4.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 70.8 BY 4.05.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 71 BY 3.76.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 65 BY 4.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 65 BY 4.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 65 BY 3.76.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 37 BY 4.05.

DEFINE VARIABLE svActual AS LOGICAL INITIAL yes 
     LABEL "Actual" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE svAllCarrier AS LOGICAL INITIAL yes 
     LABEL "All" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE svAllCSR AS LOGICAL INITIAL yes 
     LABEL "All" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE svAllCustNo AS LOGICAL INITIAL yes 
     LABEL "All Customers" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE svAllItemNo AS LOGICAL INITIAL yes 
     LABEL "All" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE svAllLoc AS LOGICAL INITIAL yes 
     LABEL "All Warehouses" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .95 NO-UNDO.

DEFINE VARIABLE svAllOrderNo AS LOGICAL INITIAL yes 
     LABEL "All Orders" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .95 NO-UNDO.

DEFINE VARIABLE svAllProdCategory AS LOGICAL INITIAL yes 
     LABEL "All Product Categories" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE svAllSalesRep AS LOGICAL INITIAL yes 
     LABEL "All Sales Reps" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE svAllShipFrom AS LOGICAL INITIAL yes 
     LABEL "All Ship From" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE svBackorder AS LOGICAL INITIAL yes 
     LABEL "Backorder" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE svBillOfLading AS LOGICAL INITIAL no 
     LABEL "Bil lOf Lading" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE svCompleted AS LOGICAL INITIAL no 
     LABEL "Completed" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE svCustList AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE svInvoiceUnposted AS LOGICAL INITIAL no 
     LABEL "Invoice Unposted" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE svLate AS LOGICAL INITIAL yes 
     LABEL "Late" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE svOnlyNegativeAvailable AS LOGICAL INITIAL yes 
     LABEL "Only Neg Available?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE svOnlyNegOHRelQty AS LOGICAL INITIAL yes 
     LABEL "Only Neg OH RelQty?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .95 NO-UNDO.

DEFINE VARIABLE svPastLastShipDate AS LOGICAL INITIAL yes 
     LABEL "Past Last Ship Date" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE svScheduled AS LOGICAL INITIAL yes 
     LABEL "Scheduled" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE svSubRpt_PrintScheduleStats AS LOGICAL INITIAL no 
     LABEL "Print Schedule Stats?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE svSubRpt_PrintSpecNotes AS LOGICAL INITIAL no 
     LABEL "Print Spec Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95 NO-UNDO.

DEFINE VARIABLE svSubTotalByCustomerNo AS LOGICAL INITIAL no 
     LABEL "Sub By Customer No?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 146 COLON-ALIGNED WIDGET-ID 60
     svLocation AT ROW 1.24 COL 162 COLON-ALIGNED WIDGET-ID 232
     svCustList AT ROW 2.67 COL 21 WIDGET-ID 48
     btnCustList AT ROW 2.67 COL 62 WIDGET-ID 46
     svAllCustNo AT ROW 3.86 COL 21 HELP
          "All Customers?" WIDGET-ID 56
     svStartCustNo AT ROW 5.05 COL 19 COLON-ALIGNED HELP
          "Enter Start Customer" WIDGET-ID 2
     startCustName AT ROW 5.05 COL 35 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     svEndCustNo AT ROW 6.24 COL 19 COLON-ALIGNED HELP
          "Enter End Customer" WIDGET-ID 6
     endCustName AT ROW 6.24 COL 35 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     svAllOrderNo AT ROW 7.91 COL 21 HELP
          "All Orders?" WIDGET-ID 196
     svStartOrderNo AT ROW 9.1 COL 19 COLON-ALIGNED HELP
          "Enter Start Order" WIDGET-ID 200
     svEndOrderNo AT ROW 10.29 COL 19 COLON-ALIGNED HELP
          "Enter End Order" WIDGET-ID 198
     svAllShipFrom AT ROW 7.91 COL 38 HELP
          "All Ship From?" WIDGET-ID 334
     svStartShipFrom AT ROW 9.33 COL 62 COLON-ALIGNED HELP
          "Enter Start Ship From" WIDGET-ID 338
     svEndShipFrom AT ROW 10.52 COL 62 COLON-ALIGNED HELP
          "Enter End Ship From" WIDGET-ID 336
     svAllLoc AT ROW 12.19 COL 21 HELP
          "All Warehouses?" WIDGET-ID 262
     svStartLoc AT ROW 13.38 COL 19 COLON-ALIGNED HELP
          "Enter Start Warehouse" WIDGET-ID 270
     startLocName AT ROW 13.38 COL 35 COLON-ALIGNED NO-LABEL WIDGET-ID 258
     svEndLoc AT ROW 14.57 COL 19 COLON-ALIGNED HELP
          "Enter End Warehouse" WIDGET-ID 266
     endLocName AT ROW 14.57 COL 35 COLON-ALIGNED NO-LABEL WIDGET-ID 254
     svAllSalesRep AT ROW 16.48 COL 21 HELP
          "All Sales Reps?" WIDGET-ID 108
     svStartSalesRep AT ROW 17.43 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep#" WIDGET-ID 112
     startSalesRepName AT ROW 17.43 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     svEndSalesRep AT ROW 18.62 COL 19 COLON-ALIGNED HELP
          "Enter Ending Sales Rep" WIDGET-ID 110
     endSalesRepName AT ROW 18.62 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     svAllCSR AT ROW 20.52 COL 21 HELP
          "All Orders?"
     svStartCSR AT ROW 21.71 COL 19 COLON-ALIGNED HELP
          "Enter Start CSR"
     startCSRName AT ROW 21.71 COL 33 COLON-ALIGNED NO-LABEL
     svEndCSR AT ROW 22.91 COL 19 COLON-ALIGNED HELP
          "Enter End CSR"
     endCSRName AT ROW 22.91 COL 33 COLON-ALIGNED NO-LABEL
     svPrintOHQty AT ROW 3.86 COL 81 HELP
          "Select Sort Option" NO-LABEL WIDGET-ID 340
     svAllItemNo AT ROW 7.91 COL 89 HELP
          "All Items?" WIDGET-ID 164
     svStartItemNo AT ROW 9.1 COL 87 COLON-ALIGNED HELP
          "Enter Start Item" WIDGET-ID 168
     startItemName AT ROW 9.1 COL 108 COLON-ALIGNED NO-LABEL WIDGET-ID 172
     svEndItemNo AT ROW 10.29 COL 87 COLON-ALIGNED HELP
          "Enter End Item" WIDGET-ID 166
     endItemName AT ROW 10.29 COL 108 COLON-ALIGNED NO-LABEL WIDGET-ID 170
     svAllCarrier AT ROW 12.19 COL 90 HELP
          "All Carriers?" WIDGET-ID 328
     svStartCarrier AT ROW 13.38 COL 88 COLON-ALIGNED HELP
          "Enter Start Carrier" WIDGET-ID 332
     startCarrierName AT ROW 13.38 COL 97 COLON-ALIGNED NO-LABEL WIDGET-ID 326
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 174 BY 28.29.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     svEndCarrier AT ROW 14.57 COL 88 COLON-ALIGNED HELP
          "Enter End Carrier" WIDGET-ID 330
     endCarrierName AT ROW 14.57 COL 97 COLON-ALIGNED NO-LABEL WIDGET-ID 324
     svAllProdCategory AT ROW 16.48 COL 90 HELP
          "All Sales Reps?" WIDGET-ID 202
     svStartProdCategory AT ROW 17.43 COL 88 COLON-ALIGNED HELP
          "Enter Start Product Category" WIDGET-ID 206
     startProdCategoryName AT ROW 17.43 COL 104 COLON-ALIGNED NO-LABEL WIDGET-ID 210
     svEndProdCategory AT ROW 18.62 COL 88 COLON-ALIGNED HELP
          "Enter End Product Category" WIDGET-ID 204
     endProdCategoryName AT ROW 18.62 COL 104 COLON-ALIGNED NO-LABEL WIDGET-ID 208
     svStartReleaseDate AT ROW 21.71 COL 88 COLON-ALIGNED HELP
          "Enter Start Release Date" WIDGET-ID 72
     btnCalendar-1 AT ROW 21.71 COL 106 WIDGET-ID 76
     svStartReleaseDateOption AT ROW 21.71 COL 109 COLON-ALIGNED HELP
          "Select Start Date Option" NO-LABEL WIDGET-ID 74
     svEndReleaseDate AT ROW 22.91 COL 88 COLON-ALIGNED HELP
          "Enter End Release Date" WIDGET-ID 68
     btnCalendar-2 AT ROW 22.91 COL 106 WIDGET-ID 78
     svEndReleaseDateOption AT ROW 22.91 COL 109 COLON-ALIGNED HELP
          "Select End Date Option" NO-LABEL WIDGET-ID 70
     svSubTotalByCustomerNo AT ROW 2.67 COL 113 HELP
          "Select to Show Sub Total By Customer No" WIDGET-ID 88
     svOnlyNegativeAvailable AT ROW 3.86 COL 113 HELP
          "Select to Show Only Negative Available" WIDGET-ID 162
     svOnlyNegOHRelQty AT ROW 5.05 COL 113 HELP
          "Select to Show Only Neg OH RelQty" WIDGET-ID 156
     svSubRpt_PrintScheduleStats AT ROW 6.24 COL 113 HELP
          "Select to Show Print Schedule Stats" WIDGET-ID 346
     svSort AT ROW 2.67 COL 150 HELP
          "Select Sort Option" NO-LABEL WIDGET-ID 84
     svScheduled AT ROW 10.76 COL 150 HELP
          "Select to Show Scheduled" WIDGET-ID 356
     svLate AT ROW 11.95 COL 150 HELP
          "Select to Show Late" WIDGET-ID 358
     svPastLastShipDate AT ROW 13.14 COL 150 HELP
          "Select to Show Past Last Ship Date" WIDGET-ID 360
     svActual AT ROW 14.33 COL 150 HELP
          "Select to Show Actual" WIDGET-ID 362
     svBackorder AT ROW 15.52 COL 150 HELP
          "Select to Show Backorder" WIDGET-ID 364
     svBillOfLading AT ROW 16.71 COL 150 HELP
          "Select to Show Bil lOf Lading" WIDGET-ID 366
     svInvoiceUnposted AT ROW 17.91 COL 150 HELP
          "Select to Show Invoice Unposted" WIDGET-ID 368
     svCompleted AT ROW 19.1 COL 150 HELP
          "Select to Show Completed" WIDGET-ID 370
     svSubRpt_PrintSpecNotes AT ROW 20.76 COL 150 HELP
          "Select to Print Spec Notes" WIDGET-ID 348
     svStartSpecNote AT ROW 21.95 COL 163 COLON-ALIGNED HELP
          "Enter Start Spec Note" WIDGET-ID 352
     svEndSpecNote AT ROW 23.14 COL 163 COLON-ALIGNED HELP
          "Enter End Spec Note" WIDGET-ID 350
     btnAddEmail AT ROW 26.71 COL 52 HELP
          "Add Recipents" WIDGET-ID 636
     svRecipients AT ROW 25.05 COL 58 NO-LABEL WIDGET-ID 600
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 174 BY 28.29.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "Sort:" VIEW-AS TEXT
          SIZE 5 BY 1 AT ROW 2.67 COL 144 WIDGET-ID 90
     "Release Types:" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 9.81 COL 141 WIDGET-ID 354
     "Print OH Qty:" VIEW-AS TEXT
          SIZE 13 BY 1 AT ROW 2.67 COL 75 WIDGET-ID 136
     "Email" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 25.05 COL 52 WIDGET-ID 640
     "Recipients:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 25.76 COL 47 WIDGET-ID 602
     RECT-1 AT ROW 2.43 COL 2 WIDGET-ID 248
     RECT-2 AT ROW 7.67 COL 2 WIDGET-ID 372
     RECT-3 AT ROW 7.67 COL 74 WIDGET-ID 374
     RECT-4 AT ROW 11.95 COL 2 WIDGET-ID 376
     RECT-5 AT ROW 16.24 COL 2 WIDGET-ID 378
     RECT-6 AT ROW 20.29 COL 74 WIDGET-ID 380
     RECT-7 AT ROW 11.95 COL 74 WIDGET-ID 382
     RECT-8 AT ROW 16.24 COL 74 WIDGET-ID 384
     RECT-9 AT ROW 7.67 COL 36 WIDGET-ID 386
     RECT-10 AT ROW 20.52 COL 140 WIDGET-ID 388
     RECT-11 AT ROW 20.29 COL 2 WIDGET-ID 390
     RECT-12 AT ROW 2.43 COL 74 WIDGET-ID 392
     RECT-13 AT ROW 2.43 COL 140 WIDGET-ID 394
     RECT-14 AT ROW 2.43 COL 110 WIDGET-ID 396
     RECT-15 AT ROW 9.57 COL 140 WIDGET-ID 398
     RECT-16 AT ROW 24.57 COL 46 WIDGET-ID 638
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 174 BY 28.29
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
         HEIGHT             = 28.29
         WIDTH              = 174.
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
/* SETTINGS FOR FILL-IN endCarrierName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endCSRName IN FRAME F-Main
   NO-ENABLE                                                            */
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
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-10 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-11 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-12 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-13 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-14 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-15 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-16 IN FRAME F-Main
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
/* SETTINGS FOR FILL-IN startCarrierName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startCSRName IN FRAME F-Main
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

ASSIGN 
       svLocation:READ-ONLY IN FRAME F-Main        = TRUE.

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
    RUN AOA/aoaRecipients.w (INPUT-OUTPUT cRecipients).
    svRecipients:SCREEN-VALUE = cRecipients.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 sObject
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svStartReleaseDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 sObject
ON CHOOSE OF btnCalendar-2 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndReleaseDate}
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


&Scoped-define SELF-NAME svAllCarrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllCarrier sObject
ON VALUE-CHANGED OF svAllCarrier IN FRAME F-Main /* All */
DO:
    {aoa/includes/svAllValueChanged.i svStartCarrier svEndCarrier}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllCSR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllCSR sObject
ON VALUE-CHANGED OF svAllCSR IN FRAME F-Main /* All */
DO:
    {aoa/includes/svAllValueChanged.i svStartCSR svEndCSR}
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
ON VALUE-CHANGED OF svAllItemNo IN FRAME F-Main /* All */
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


&Scoped-define SELF-NAME svAllOrderNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllOrderNo sObject
ON VALUE-CHANGED OF svAllOrderNo IN FRAME F-Main /* All Orders */
DO:
    {aoa/includes/svAllValueChanged.i svStartOrderNo svEndOrderNo}
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


&Scoped-define SELF-NAME svAllShipFrom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllShipFrom sObject
ON VALUE-CHANGED OF svAllShipFrom IN FRAME F-Main /* All Ship From */
DO:
    {aoa/includes/svAllValueChanged.i svStartShipFrom svEndShipFrom}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCompany sObject
ON ENTRY OF svCompany IN FRAME F-Main /* Company */
DO:
  APPLY "ENTRY":U TO svAllCustNo.
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


&Scoped-define SELF-NAME svEndCarrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndCarrier sObject
ON LEAVE OF svEndCarrier IN FRAME F-Main /* End Carrier */
DO:
    endCarrierName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndCSR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndCSR sObject
ON LEAVE OF svEndCSR IN FRAME F-Main /* End CSR */
DO:
    endCSRName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
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
ON LEAVE OF svEndProdCategory IN FRAME F-Main /* End Prod Cat */
DO:
    endProdCategoryName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndReleaseDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndReleaseDate sObject
ON HELP OF svEndReleaseDate IN FRAME F-Main /* End Rel Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndReleaseDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndReleaseDateOption sObject
ON VALUE-CHANGED OF svEndReleaseDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svEndReleaseDate &btnCalendar=2}
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


&Scoped-define SELF-NAME svLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svLocation sObject
ON ENTRY OF svLocation IN FRAME F-Main /* Location */
DO:
  APPLY "ENTRY":U TO svAllCustNo.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svOnlyNegativeAvailable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svOnlyNegativeAvailable sObject
ON VALUE-CHANGED OF svOnlyNegativeAvailable IN FRAME F-Main /* Only Neg Available? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svOnlyNegOHRelQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svOnlyNegOHRelQty sObject
ON VALUE-CHANGED OF svOnlyNegOHRelQty IN FRAME F-Main /* Only Neg OH RelQty? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svSort sObject
ON VALUE-CHANGED OF svSort IN FRAME F-Main
DO:
    IF {&SELF-NAME}:SCREEN-VALUE NE "Customer No" THEN
    svSubTotalByCustomerNo:SCREEN-VALUE = "no". 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartCarrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartCarrier sObject
ON LEAVE OF svStartCarrier IN FRAME F-Main /* Start Carrier */
DO:
    startCarrierName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartCSR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartCSR sObject
ON LEAVE OF svStartCSR IN FRAME F-Main /* Start CSR */
DO:
    startCSRName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
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
ON LEAVE OF svStartProdCategory IN FRAME F-Main /* Start Prod Cat */
DO:
    startProdCategoryName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartReleaseDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartReleaseDate sObject
ON HELP OF svStartReleaseDate IN FRAME F-Main /* Start Rel Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartReleaseDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartReleaseDateOption sObject
ON VALUE-CHANGED OF svStartReleaseDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svStartReleaseDate &btnCalendar=1}
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


&Scoped-define SELF-NAME svSubRpt_PrintScheduleStats
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svSubRpt_PrintScheduleStats sObject
ON VALUE-CHANGED OF svSubRpt_PrintScheduleStats IN FRAME F-Main /* Print Schedule Stats? */
DO:
    IF {&SELF-NAME}:SCREEN-VALUE EQ "yes" THEN
    svSort:SCREEN-VALUE = "Customer No".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svSubRpt_PrintSpecNotes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svSubRpt_PrintSpecNotes sObject
ON VALUE-CHANGED OF svSubRpt_PrintSpecNotes IN FRAME F-Main /* Print Spec Notes? */
DO:
    IF {&SELF-NAME}:SCREEN-VALUE EQ "yes" THEN
    svSort:SCREEN-VALUE = "Customer No".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svSubTotalByCustomerNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svSubTotalByCustomerNo sObject
ON VALUE-CHANGED OF svSubTotalByCustomerNo IN FRAME F-Main /* Sub By Customer No? */
DO:
    IF {&SELF-NAME}:SCREEN-VALUE EQ "yes" THEN
    svSort:SCREEN-VALUE = "Customer No".
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
            svLocation:SCREEN-VALUE = DYNAMIC-FUNCTION('fGetLocation' IN hContainer)
            svLocation
            .

        APPLY "VALUE-CHANGED":U TO svStartReleaseDateOption.
        APPLY "VALUE-CHANGED":U TO svEndReleaseDateOption.
        
        APPLY "VALUE-CHANGED":U TO svAllSalesRep.
        APPLY "LEAVE":U TO svStartSalesRep.
        APPLY "LEAVE":U TO svEndSalesRep.
        
        APPLY "VALUE-CHANGED":U TO svAllCustNo.
        APPLY "LEAVE":U TO svStartCustNo.
        APPLY "LEAVE":U TO svEndCustNo.
        
        APPLY "VALUE-CHANGED":U TO svAllItemNo.
        APPLY "LEAVE":U TO svStartItemNo.
        APPLY "LEAVE":U TO svEndItemNo.
        
        APPLY "VALUE-CHANGED":U TO svAllOrderNo.

        APPLY "VALUE-CHANGED":U TO svAllProdCategory.
        APPLY "LEAVE":U TO svStartProdCategory.
        APPLY "LEAVE":U TO svEndProdCategory.
        
        APPLY "VALUE-CHANGED":U TO svAllLoc.
        APPLY "LEAVE":U TO svStartLoc.
        APPLY "LEAVE":U TO svEndLoc.
        
        APPLY "VALUE-CHANGED":U TO svAllCarrier.
        APPLY "LEAVE":U TO svStartCarrier.
        APPLY "LEAVE":U TO svEndCarrier.

        APPLY "VALUE-CHANGED":U TO svAllCSR.
        APPLY "LEAVE":U TO svStartCSR.
        APPLY "LEAVE":U TO svEndCSR.
        
        APPLY "VALUE-CHANGED":U TO svAllShipFrom.
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

        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartReleaseDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndReleaseDateOption:HANDLE).
        
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

