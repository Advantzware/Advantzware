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

DEFINE VARIABLE hContainer AS HANDLE NO-UNDO.

{sys/ref/CustList.i NEW}

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
svStartTranDate btnCalendar-1 svStartTranDateOption svStartCustNo ~
svEndTranDate btnCalendar-2 svEndTranDateOption svEndCustNo ~
svStartReceiptDate btnCalendar-3 svStartReceiptDateOption svAllMachine ~
svEndReceiptDate btnCalendar-4 svEndReceiptDateOption svStartMachine ~
svStartInvoiceDate btnCalendar-5 svStartInvoiceDateOption svEndMachine ~
svEndInvoiceDate btnCalendar-6 svEndInvoiceDateOption svAllSalesRep ~
svStartOrderDate btnCalendar-7 svStartOrderDateOption svStartSalesRep ~
svEndOrderDate btnCalendar-8 svEndOrderDateOption svEndSalesRep ~
svStartDueDate btnCalendar-9 svStartDueDateOption svAllPONumber svAllCAD ~
svEndDueDate btnCalendar-10 svEndDueDateOption svStartPONumber svStartCAD ~
svEndPONumber svEndCAD svStartShipDate btnCalendar-11 svStartShipDateOption ~
svEndShipDate btnCalendar-12 svEndShipDateOption svAllUserID svStartUserID ~
svSort svEndUserID svSubRpt_SubReportName svAllItemNo svStartItemNo ~
svEndItemNo svAllJobNo svAllOrderNo svStartJobNo svStartJobNo2 ~
svStartOrderNo svEndJobNo svEndJobNo2 svEndOrderNo svAllProdCategory ~
svStartProdCategory svEndProdCategory 
&Scoped-Define DISPLAYED-OBJECTS svCompany svCustList svAllCustNo ~
svStartTranDate svStartTranDateOption svStartCustNo startCustName ~
svEndTranDate svEndTranDateOption svEndCustNo endCustName ~
svStartReceiptDate svStartReceiptDateOption svAllMachine svEndReceiptDate ~
svEndReceiptDateOption svStartMachine startMachineDescription ~
svStartInvoiceDate svStartInvoiceDateOption svEndMachine ~
endMachineDescription svEndInvoiceDate svEndInvoiceDateOption svAllSalesRep ~
svStartOrderDate svStartOrderDateOption svStartSalesRep startSalesRepName ~
svEndOrderDate svEndOrderDateOption svEndSalesRep endSalesRepName ~
svStartDueDate svStartDueDateOption svAllPONumber svAllCAD svEndDueDate ~
svEndDueDateOption svStartPONumber svStartCAD svEndPONumber svEndCAD ~
svStartShipDate svStartShipDateOption svEndShipDate svEndShipDateOption ~
svAllUserID svStartUserID startUserIDName svSort svEndUserID endUserIDName ~
svSubRpt_SubReportName svAllItemNo svStartItemNo startItemName svEndItemNo ~
endItemName svAllJobNo svAllOrderNo svStartJobNo svStartJobNo2 ~
svStartOrderNo svEndJobNo svEndJobNo2 svEndOrderNo svAllProdCategory ~
svStartProdCategory startProdCategoryName svEndProdCategory ~
endProdCategoryName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 btnCalendar-2 btnCalendar-3 ~
btnCalendar-4 btnCalendar-5 btnCalendar-6 btnCalendar-7 btnCalendar-8 ~
btnCalendar-9 btnCalendar-10 btnCalendar-11 btnCalendar-12 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-10 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-11 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-12 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-3 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-4 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-5 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-6 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-7 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-8 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-9 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
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

DEFINE VARIABLE svEndInvoiceDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svEndOrderDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svEndReceiptDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svEndShipDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svEndTranDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartDueDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartInvoiceDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartOrderDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartReceiptDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartShipDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartTranDateOption AS CHARACTER FORMAT "X(256)":U 
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

DEFINE VARIABLE endMachineDescription AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE endProdCategoryName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.

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

DEFINE VARIABLE startMachineDescription AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startProdCategoryName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.

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

DEFINE VARIABLE svEndInvoiceDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndItemNo AS CHARACTER FORMAT "X(8)" 
     LABEL "End Item" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svEndJobNo AS CHARACTER FORMAT "X(6)" 
     LABEL "End Job" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svEndJobNo2 AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL ":" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE svEndMachine AS CHARACTER FORMAT "X(8)" 
     LABEL "End Machine" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svEndOrderDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Order Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndOrderNo AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "End Order" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svEndPONumber AS CHARACTER FORMAT "X(8)" 
     LABEL "End PO Number" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svEndProdCategory AS CHARACTER FORMAT "X(5)" 
     LABEL "End Prod Category" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndReceiptDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndSalesRep AS CHARACTER FORMAT "X(3)" 
     LABEL "End Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svEndShipDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Ship Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndTranDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

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

DEFINE VARIABLE svStartInvoiceDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Invoice Date" 
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
     LABEL ":" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE svStartMachine AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Machine" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartOrderDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Order Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartOrderNo AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Start Order" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svStartPONumber AS CHARACTER FORMAT "X(15)" 
     LABEL "Start PO Number" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svStartProdCategory AS CHARACTER FORMAT "X(5)" 
     LABEL "Start Prod Category" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartReceiptDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartSalesRep AS CHARACTER FORMAT "X(3)" 
     LABEL "Start Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svStartShipDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Ship Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartTranDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartUserID AS CHARACTER FORMAT "X(8)" 
     LABEL "Start User ID" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svSort AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Sort Option 1", "Sort Option 1",
"Sort Option 2", "Sort Option 2",
"Sort Option 3", "Sort Option 3"
     SIZE 17.4 BY 3.33 NO-UNDO.

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

DEFINE VARIABLE svAllMachine AS LOGICAL INITIAL yes 
     LABEL "All Machines" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE svAllOrderNo AS LOGICAL INITIAL yes 
     LABEL "All Orders" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .95 NO-UNDO.

DEFINE VARIABLE svAllPONumber AS LOGICAL INITIAL yes 
     LABEL "All PO Numbers" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .95 NO-UNDO.

DEFINE VARIABLE svAllProdCategory AS LOGICAL INITIAL yes 
     LABEL "All Product Categories" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .95 NO-UNDO.

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

DEFINE VARIABLE svSubRpt_SubReportName AS LOGICAL INITIAL no 
     LABEL "Show Sub Report" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 23 COLON-ALIGNED WIDGET-ID 60
     svCustList AT ROW 1.24 COL 91 WIDGET-ID 48
     btnCustList AT ROW 1.24 COL 121 WIDGET-ID 46
     svAllCustNo AT ROW 2.43 COL 91 HELP
          "All Customers?" WIDGET-ID 56
     svStartTranDate AT ROW 2.91 COL 23 COLON-ALIGNED HELP
          "Enter Start Transaction Date" WIDGET-ID 72
     btnCalendar-1 AT ROW 2.91 COL 41 WIDGET-ID 76
     svStartTranDateOption AT ROW 2.91 COL 44 COLON-ALIGNED HELP
          "Select Start Date Option" NO-LABEL WIDGET-ID 74
     svStartCustNo AT ROW 3.62 COL 89 COLON-ALIGNED HELP
          "Enter Start Customer" WIDGET-ID 2
     startCustName AT ROW 3.62 COL 105 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     svEndTranDate AT ROW 4.1 COL 23 COLON-ALIGNED HELP
          "Enter End Transaction Date" WIDGET-ID 68
     btnCalendar-2 AT ROW 4.1 COL 41 WIDGET-ID 78
     svEndTranDateOption AT ROW 4.1 COL 44 COLON-ALIGNED HELP
          "Select End Date Option" NO-LABEL WIDGET-ID 70
     svEndCustNo AT ROW 4.81 COL 89 COLON-ALIGNED HELP
          "Enter End Customer" WIDGET-ID 6
     endCustName AT ROW 4.81 COL 105 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     svStartReceiptDate AT ROW 5.76 COL 23 COLON-ALIGNED HELP
          "Enter Start Receipt Date" WIDGET-ID 100
     btnCalendar-3 AT ROW 5.76 COL 41.2 WIDGET-ID 92
     svStartReceiptDateOption AT ROW 5.76 COL 44.2 COLON-ALIGNED HELP
          "Select Start Receipt Date Option" NO-LABEL WIDGET-ID 102
     svAllMachine AT ROW 6.48 COL 91 HELP
          "All Macines?" WIDGET-ID 58
     svEndReceiptDate AT ROW 6.95 COL 23.2 COLON-ALIGNED HELP
          "Enter End Receipt Date" WIDGET-ID 96
     btnCalendar-4 AT ROW 6.95 COL 41.2 WIDGET-ID 94
     svEndReceiptDateOption AT ROW 6.95 COL 44.2 COLON-ALIGNED HELP
          "Select End Receipt Date Option" NO-LABEL WIDGET-ID 98
     svStartMachine AT ROW 7.67 COL 89 COLON-ALIGNED HELP
          "Enter Start Machine" WIDGET-ID 22
     startMachineDescription AT ROW 7.67 COL 105 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     svStartInvoiceDate AT ROW 8.62 COL 23 COLON-ALIGNED HELP
          "Enter Start Invoice Date" WIDGET-ID 26
     btnCalendar-5 AT ROW 8.62 COL 41 WIDGET-ID 80
     svStartInvoiceDateOption AT ROW 8.62 COL 44 COLON-ALIGNED HELP
          "Select Start Invoice Date Option" NO-LABEL WIDGET-ID 64
     svEndMachine AT ROW 8.86 COL 89 COLON-ALIGNED HELP
          "Enter End Machine" WIDGET-ID 20
     endMachineDescription AT ROW 8.86 COL 105 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     svEndInvoiceDate AT ROW 9.81 COL 23 COLON-ALIGNED HELP
          "Enter End Invoice Date" WIDGET-ID 24
     btnCalendar-6 AT ROW 9.81 COL 41 WIDGET-ID 82
     svEndInvoiceDateOption AT ROW 9.81 COL 44 COLON-ALIGNED HELP
          "Select End Invoice Date Option" NO-LABEL WIDGET-ID 66
     svAllSalesRep AT ROW 10.52 COL 91 HELP
          "All Sales Reps?" WIDGET-ID 108
     svStartOrderDate AT ROW 11.48 COL 23 COLON-ALIGNED HELP
          "Enter Start Order Date" WIDGET-ID 122
     btnCalendar-7 AT ROW 11.48 COL 41 WIDGET-ID 114
     svStartOrderDateOption AT ROW 11.48 COL 44 COLON-ALIGNED HELP
          "Select Start Order Date Option" NO-LABEL WIDGET-ID 124
     svStartSalesRep AT ROW 11.71 COL 89 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep#" WIDGET-ID 112
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 151.4 BY 34.33.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     startSalesRepName AT ROW 11.71 COL 98 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     svEndOrderDate AT ROW 12.67 COL 23 COLON-ALIGNED HELP
          "Enter End Order Date" WIDGET-ID 118
     btnCalendar-8 AT ROW 12.67 COL 41 WIDGET-ID 116
     svEndOrderDateOption AT ROW 12.67 COL 44 COLON-ALIGNED HELP
          "Select End Order Date Option" NO-LABEL WIDGET-ID 120
     svEndSalesRep AT ROW 12.91 COL 89 COLON-ALIGNED HELP
          "Enter Ending Sales Rep" WIDGET-ID 110
     endSalesRepName AT ROW 12.91 COL 98 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     svStartDueDate AT ROW 14.33 COL 23 COLON-ALIGNED HELP
          "Enter Start Due Date" WIDGET-ID 134
     btnCalendar-9 AT ROW 14.33 COL 41 WIDGET-ID 126
     svStartDueDateOption AT ROW 14.33 COL 44 COLON-ALIGNED HELP
          "Select Start Due Date Option" NO-LABEL WIDGET-ID 136
     svAllPONumber AT ROW 14.57 COL 91 HELP
          "All PO Numbers?" WIDGET-ID 142
     svAllCAD AT ROW 14.57 COL 130 HELP
          "All PO Numbers?" WIDGET-ID 148
     svEndDueDate AT ROW 15.52 COL 23 COLON-ALIGNED HELP
          "Enter End Due Date" WIDGET-ID 130
     btnCalendar-10 AT ROW 15.52 COL 41 WIDGET-ID 128
     svEndDueDateOption AT ROW 15.52 COL 44 COLON-ALIGNED HELP
          "Select End Due Date Option" NO-LABEL WIDGET-ID 132
     svStartPONumber AT ROW 15.76 COL 89 COLON-ALIGNED HELP
          "Enter Start PO Number" WIDGET-ID 146
     svStartCAD AT ROW 15.76 COL 128 COLON-ALIGNED HELP
          "Enter Start CAD" WIDGET-ID 152
     svEndPONumber AT ROW 16.95 COL 89 COLON-ALIGNED HELP
          "Enter End PO Number" WIDGET-ID 144
     svEndCAD AT ROW 16.95 COL 128 COLON-ALIGNED HELP
          "Enter End CAD" WIDGET-ID 150
     svStartShipDate AT ROW 17.19 COL 23 COLON-ALIGNED HELP
          "Enter Start Ship Date" WIDGET-ID 192
     btnCalendar-11 AT ROW 17.19 COL 41 WIDGET-ID 186
     svStartShipDateOption AT ROW 17.19 COL 44 COLON-ALIGNED HELP
          "Select Start Ship Date Option" NO-LABEL WIDGET-ID 194
     svEndShipDate AT ROW 18.38 COL 23 COLON-ALIGNED HELP
          "Enter End Ship Date" WIDGET-ID 188
     btnCalendar-12 AT ROW 18.38 COL 41 WIDGET-ID 184
     svEndShipDateOption AT ROW 18.38 COL 44 COLON-ALIGNED HELP
          "Select End Ship Date Option" NO-LABEL WIDGET-ID 190
     svAllUserID AT ROW 18.62 COL 91 HELP
          "All User IDs?" WIDGET-ID 158
     svStartUserID AT ROW 19.81 COL 89 COLON-ALIGNED HELP
          "Enter Start User ID" WIDGET-ID 162
     startUserIDName AT ROW 19.81 COL 105 COLON-ALIGNED NO-LABEL WIDGET-ID 156
     svSort AT ROW 20.05 COL 24.6 HELP
          "Select Sort Option" NO-LABEL WIDGET-ID 84
     svEndUserID AT ROW 21 COL 89 COLON-ALIGNED HELP
          "Enter End User ID" WIDGET-ID 160
     endUserIDName AT ROW 21 COL 105 COLON-ALIGNED NO-LABEL WIDGET-ID 154
     svSubRpt_SubReportName AT ROW 21.24 COL 46 HELP
          "Select to Show Sub Report" WIDGET-ID 88
     svAllItemNo AT ROW 22.67 COL 91 HELP
          "All Items?" WIDGET-ID 164
     svStartItemNo AT ROW 23.86 COL 89 COLON-ALIGNED HELP
          "Enter Start Item" WIDGET-ID 168
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 151.4 BY 34.33.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     startItemName AT ROW 23.86 COL 112 COLON-ALIGNED NO-LABEL WIDGET-ID 172
     svEndItemNo AT ROW 25.05 COL 89 COLON-ALIGNED HELP
          "Enter End Item" WIDGET-ID 166
     endItemName AT ROW 25.05 COL 112 COLON-ALIGNED NO-LABEL WIDGET-ID 170
     svAllJobNo AT ROW 26.71 COL 91 HELP
          "All Jobs?" WIDGET-ID 174
     svAllOrderNo AT ROW 26.71 COL 131 HELP
          "All Orders?" WIDGET-ID 196
     svStartJobNo AT ROW 27.91 COL 89 COLON-ALIGNED HELP
          "Enter Start Job" WIDGET-ID 178
     svStartJobNo2 AT ROW 27.91 COL 100 COLON-ALIGNED HELP
          "Enter Start Job Run" WIDGET-ID 180
     svStartOrderNo AT ROW 27.91 COL 129 COLON-ALIGNED HELP
          "Enter Start Order" WIDGET-ID 200
     svEndJobNo AT ROW 29.1 COL 89 COLON-ALIGNED HELP
          "Enter End Job" WIDGET-ID 176
     svEndJobNo2 AT ROW 29.1 COL 100 COLON-ALIGNED HELP
          "Enter End Job Run" WIDGET-ID 182
     svEndOrderNo AT ROW 29.1 COL 129 COLON-ALIGNED HELP
          "Enter End Order" WIDGET-ID 198
     svAllProdCategory AT ROW 30.76 COL 91 HELP
          "All Sales Reps?" WIDGET-ID 202
     svStartProdCategory AT ROW 31.95 COL 89 COLON-ALIGNED HELP
          "Enter Start Product Category" WIDGET-ID 206
     startProdCategoryName AT ROW 31.95 COL 106 COLON-ALIGNED NO-LABEL WIDGET-ID 210
     svEndProdCategory AT ROW 33.14 COL 89 COLON-ALIGNED HELP
          "Enter End Product Category" WIDGET-ID 204
     endProdCategoryName AT ROW 33.14 COL 106 COLON-ALIGNED NO-LABEL WIDGET-ID 208
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 20.05 COL 15 WIDGET-ID 90
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 151.4 BY 34.33
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
         HEIGHT             = 34.33
         WIDTH              = 151.4.
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
/* SETTINGS FOR BUTTON btnCalendar-10 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-11 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-12 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-3 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-4 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-5 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-6 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-7 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-8 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-9 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN endCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endItemName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endMachineDescription IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endProdCategoryName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endSalesRepName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endUserIDName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startItemName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startMachineDescription IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startProdCategoryName IN FRAME F-Main
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

&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 sObject
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svStartTranDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-10 sObject
ON CHOOSE OF btnCalendar-10 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndDueDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-11 sObject
ON CHOOSE OF btnCalendar-11 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svStartShipDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-12 sObject
ON CHOOSE OF btnCalendar-12 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndShipDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 sObject
ON CHOOSE OF btnCalendar-2 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndTranDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-3 sObject
ON CHOOSE OF btnCalendar-3 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svStartReceiptDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-4 sObject
ON CHOOSE OF btnCalendar-4 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndReceiptDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-5 sObject
ON CHOOSE OF btnCalendar-5 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svStartInvoiceDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-6 sObject
ON CHOOSE OF btnCalendar-6 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndInvoiceDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-7 sObject
ON CHOOSE OF btnCalendar-7 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svStartOrderDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-8 sObject
ON CHOOSE OF btnCalendar-8 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndOrderDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-9 sObject
ON CHOOSE OF btnCalendar-9 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svStartDueDate}
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
  ASSIGN {&SELF-NAME}.
  RUN pSetCADRange ({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllCustNo sObject
ON VALUE-CHANGED OF svAllCustNo IN FRAME F-Main /* All Customers */
DO:
  ASSIGN {&SELF-NAME}.
  RUN pSetCustRange ({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllItemNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllItemNo sObject
ON VALUE-CHANGED OF svAllItemNo IN FRAME F-Main /* All Items */
DO:
  ASSIGN {&SELF-NAME}.
  RUN pSetItemRange ({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllJobNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllJobNo sObject
ON VALUE-CHANGED OF svAllJobNo IN FRAME F-Main /* All Jobs */
DO:
  ASSIGN {&SELF-NAME}.
  RUN pSetJobRange ({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllMachine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllMachine sObject
ON VALUE-CHANGED OF svAllMachine IN FRAME F-Main /* All Machines */
DO:
  ASSIGN {&SELF-NAME}.
  RUN pSetMachineRange ({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllOrderNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllOrderNo sObject
ON VALUE-CHANGED OF svAllOrderNo IN FRAME F-Main /* All Orders */
DO:
  ASSIGN {&SELF-NAME}.
  RUN pSetOrderRange ({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllPONumber
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllPONumber sObject
ON VALUE-CHANGED OF svAllPONumber IN FRAME F-Main /* All PO Numbers */
DO:
  ASSIGN {&SELF-NAME}.
  RUN pSetPONumberRange ({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllProdCategory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllProdCategory sObject
ON VALUE-CHANGED OF svAllProdCategory IN FRAME F-Main /* All Product Categories */
DO:
  ASSIGN {&SELF-NAME}.
  RUN pSetProdCategoryRange ({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllSalesRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllSalesRep sObject
ON VALUE-CHANGED OF svAllSalesRep IN FRAME F-Main /* All Sales Reps */
DO:
  ASSIGN {&SELF-NAME}.
  RUN pSetSalesRepRange ({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllUserID sObject
ON VALUE-CHANGED OF svAllUserID IN FRAME F-Main /* All User IDs */
DO:
  ASSIGN {&SELF-NAME}.
  RUN pSetUserIDRange ({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCompany sObject
ON ENTRY OF svCompany IN FRAME F-Main /* Company */
DO:
  APPLY "ENTRY":U TO svStartTranDate.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCustList sObject
ON VALUE-CHANGED OF svCustList IN FRAME F-Main /* Use Defined Customer List */
DO:
  ASSIGN {&SELF-NAME}.
  RUN pUseCustList ({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndCustNo sObject
ON LEAVE OF svEndCustNo IN FRAME F-Main /* End Customer */
DO:
    ASSIGN {&SELF-NAME}.
    FIND FIRST cust NO-LOCK
         WHERE cust.company EQ DYNAMIC-FUNCTION('fGetCompany' IN hContainer)
           AND cust.cust-no EQ {&SELF-NAME}
         NO-ERROR.
    endCustName:SCREEN-VALUE = IF AVAILABLE cust THEN cust.name
                               ELSE "<Ending Range Value>".
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
  ASSIGN
      {&SELF-NAME}
      svEndDueDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      btnCalendar-10:SENSITIVE = {&SELF-NAME} EQ "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndInvoiceDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndInvoiceDate sObject
ON HELP OF svEndInvoiceDate IN FRAME F-Main /* End Invoice Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndInvoiceDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndInvoiceDateOption sObject
ON VALUE-CHANGED OF svEndInvoiceDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svEndInvoiceDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      btnCalendar-6:SENSITIVE = {&SELF-NAME} EQ "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndItemNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndItemNo sObject
ON LEAVE OF svEndItemNo IN FRAME F-Main /* End Item */
DO:
    ASSIGN {&SELF-NAME}.
    FIND FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ DYNAMIC-FUNCTION('fGetCompany' IN hContainer)
           AND itemfg.i-no EQ {&SELF-NAME}
         NO-ERROR.
    endItemName:SCREEN-VALUE = IF AVAILABLE itemfg THEN itemfg.i-dscr
                               ELSE "<Ending Range Value>".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndMachine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndMachine sObject
ON LEAVE OF svEndMachine IN FRAME F-Main /* End Machine */
DO:
    ASSIGN {&SELF-NAME}.
    FIND FIRST mach NO-LOCK
         WHERE mach.company EQ DYNAMIC-FUNCTION('fGetCompany' IN hContainer)
           AND mach.m-code  EQ {&SELF-NAME}
         NO-ERROR.
    endMachineDescription:SCREEN-VALUE = IF AVAILABLE mach THEN mach.m-dscr
                                         ELSE "<Ending Range Value>".
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
  ASSIGN
      {&SELF-NAME}
      svEndOrderDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      btnCalendar-8:SENSITIVE = {&SELF-NAME} EQ "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndProdCategory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndProdCategory sObject
ON LEAVE OF svEndProdCategory IN FRAME F-Main /* End Prod Category */
DO:
    ASSIGN {&SELF-NAME}.
    FIND FIRST procat NO-LOCK
         WHERE procat.company EQ DYNAMIC-FUNCTION('fGetCompany' IN hContainer)
           AND procat.procat EQ {&SELF-NAME}
         NO-ERROR.
    endProdCategoryName:SCREEN-VALUE = IF AVAILABLE procat THEN procat.dscr
                                       ELSE "<Ending Range Value>".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndReceiptDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndReceiptDate sObject
ON HELP OF svEndReceiptDate IN FRAME F-Main /* End Receipt Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndReceiptDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndReceiptDateOption sObject
ON VALUE-CHANGED OF svEndReceiptDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svEndReceiptDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      btnCalendar-2:SENSITIVE = {&SELF-NAME} EQ "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndSalesRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndSalesRep sObject
ON LEAVE OF svEndSalesRep IN FRAME F-Main /* End Sales Rep */
DO:
    ASSIGN {&SELF-NAME}.
    FIND FIRST sman NO-LOCK
         WHERE sman.company EQ DYNAMIC-FUNCTION('fGetCompany' IN hContainer)
           AND sman.sman EQ {&SELF-NAME}
         NO-ERROR.
    endSalesRepName:SCREEN-VALUE = IF AVAILABLE sman THEN sman.sname
                                   ELSE "<Ending Range Value>".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndShipDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndShipDate sObject
ON HELP OF svEndShipDate IN FRAME F-Main /* End Ship Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndShipDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndShipDateOption sObject
ON VALUE-CHANGED OF svEndShipDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svEndShipDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      btnCalendar-12:SENSITIVE = {&SELF-NAME} EQ "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndTranDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndTranDate sObject
ON HELP OF svEndTranDate IN FRAME F-Main /* End Transaction Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndTranDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndTranDateOption sObject
ON VALUE-CHANGED OF svEndTranDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svEndTranDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      btnCalendar-2:SENSITIVE = {&SELF-NAME} EQ "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndUserID sObject
ON LEAVE OF svEndUserID IN FRAME F-Main /* End User ID */
DO:
    ASSIGN {&SELF-NAME}.
    FIND FIRST users NO-LOCK
         WHERE users.user_id EQ {&SELF-NAME}
         NO-ERROR.
    endUserIDName:SCREEN-VALUE = IF AVAILABLE users THEN users.user_name
                                 ELSE "<Ending Range Value>".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartCustNo sObject
ON LEAVE OF svStartCustNo IN FRAME F-Main /* Start Customer */
DO:
    ASSIGN {&SELF-NAME}.
    FIND FIRST cust NO-LOCK
         WHERE cust.company EQ DYNAMIC-FUNCTION('fGetCompany' IN hContainer)
           AND cust.cust-no EQ {&SELF-NAME}
         NO-ERROR.
    startCustName:SCREEN-VALUE = IF AVAILABLE cust THEN cust.name
                                 ELSE "<Beginning Range Value>".
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
  ASSIGN
      {&SELF-NAME}
      svStartDueDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      btnCalendar-9:SENSITIVE = {&SELF-NAME} EQ "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartInvoiceDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartInvoiceDate sObject
ON HELP OF svStartInvoiceDate IN FRAME F-Main /* Start Invoice Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartInvoiceDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartInvoiceDateOption sObject
ON VALUE-CHANGED OF svStartInvoiceDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svStartInvoiceDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      btnCalendar-5:SENSITIVE = {&SELF-NAME} EQ "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartItemNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartItemNo sObject
ON LEAVE OF svStartItemNo IN FRAME F-Main /* Start Item */
DO:
    ASSIGN {&SELF-NAME}.
    FIND FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ DYNAMIC-FUNCTION('fGetCompany' IN hContainer)
           AND itemfg.i-no EQ {&SELF-NAME}
         NO-ERROR.
    startItemName:SCREEN-VALUE = IF AVAILABLE itemfg THEN itemfg.i-dscr
                                 ELSE "<Beginning Range Value>".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartMachine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartMachine sObject
ON LEAVE OF svStartMachine IN FRAME F-Main /* Start Machine */
DO:
    ASSIGN {&SELF-NAME}.
    FIND FIRST mach NO-LOCK
         WHERE mach.company EQ DYNAMIC-FUNCTION('fGetCompany' IN hContainer)
           AND mach.m-code  EQ {&SELF-NAME}
         NO-ERROR.
    startMachineDescription:SCREEN-VALUE = IF AVAILABLE mach THEN mach.m-dscr
                                           ELSE "<Beginning Range Value>".
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
  ASSIGN
      {&SELF-NAME}
      svStartOrderDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      btnCalendar-7:SENSITIVE = {&SELF-NAME} EQ "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartProdCategory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartProdCategory sObject
ON LEAVE OF svStartProdCategory IN FRAME F-Main /* Start Prod Category */
DO:
    ASSIGN {&SELF-NAME}.
    FIND FIRST procat NO-LOCK
         WHERE procat.company EQ DYNAMIC-FUNCTION('fGetCompany' IN hContainer)
           AND procat.procat EQ {&SELF-NAME}
         NO-ERROR.
    startProdCategoryName:SCREEN-VALUE = IF AVAILABLE procat THEN procat.dscr
                                         ELSE "<Beginning Range Value>".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartReceiptDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartReceiptDate sObject
ON HELP OF svStartReceiptDate IN FRAME F-Main /* Start Receipt Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartReceiptDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartReceiptDateOption sObject
ON VALUE-CHANGED OF svStartReceiptDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svStartReceiptDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      btnCalendar-1:SENSITIVE = {&SELF-NAME} EQ "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartSalesRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartSalesRep sObject
ON LEAVE OF svStartSalesRep IN FRAME F-Main /* Start Sales Rep# */
DO:
    ASSIGN {&SELF-NAME}.
    FIND FIRST sman NO-LOCK
         WHERE sman.company EQ DYNAMIC-FUNCTION('fGetCompany' IN hContainer)
           AND sman.sman EQ {&SELF-NAME}
         NO-ERROR.
    startSalesRepName:SCREEN-VALUE = IF AVAILABLE sman THEN sman.sname
                                     ELSE "<Beginning Range Value>".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartShipDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartShipDate sObject
ON HELP OF svStartShipDate IN FRAME F-Main /* Start Ship Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartShipDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartShipDateOption sObject
ON VALUE-CHANGED OF svStartShipDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svStartShipDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      btnCalendar-11:SENSITIVE = {&SELF-NAME} EQ "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartTranDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartTranDate sObject
ON HELP OF svStartTranDate IN FRAME F-Main /* Start Transaction Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartTranDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartTranDateOption sObject
ON VALUE-CHANGED OF svStartTranDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svStartTranDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      btnCalendar-1:SENSITIVE = {&SELF-NAME} EQ "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartUserID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartUserID sObject
ON LEAVE OF svStartUserID IN FRAME F-Main /* Start User ID */
DO:
    ASSIGN {&SELF-NAME}.
    FIND FIRST users NO-LOCK
         WHERE users.user_id EQ {&SELF-NAME}
         NO-ERROR.
    startUserIDName:SCREEN-VALUE = IF AVAILABLE users THEN users.user_name
                                   ELSE "<Beginning Range Value>".
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

        APPLY "VALUE-CHANGED":U TO svStartTranDateOption.
        APPLY "VALUE-CHANGED":U TO svEndTranDateOption.
        
        APPLY "VALUE-CHANGED":U TO svStartReceiptDateOption.
        APPLY "VALUE-CHANGED":U TO svEndReceiptDateOption.
        
        APPLY "VALUE-CHANGED":U TO svStartInvoiceDateOption.
        APPLY "VALUE-CHANGED":U TO svEndInvoiceDateOption.
        
        APPLY "VALUE-CHANGED":U TO svStartOrderDateOption.
        APPLY "VALUE-CHANGED":U TO svEndOrderDateOption.
        
        APPLY "VALUE-CHANGED":U TO svStartDueDateOption.
        APPLY "VALUE-CHANGED":U TO svEndDueDateOption.
        
        APPLY "VALUE-CHANGED":U TO svStartShipDateOption.
        APPLY "VALUE-CHANGED":U TO svEndShipDateOption.
        
        APPLY "VALUE-CHANGED":U TO svAllMachine.
        APPLY "LEAVE":U TO svStartMachine.
        APPLY "LEAVE":U TO svEndMachine.
        
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
        
        APPLY "VALUE-CHANGED":U TO svAllOrderNo.

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
        hContainer = iphContainer.
        
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartTranDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndTranDateOption:HANDLE).
        
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartReceiptDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndReceiptDateOption:HANDLE).
        
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartInvoiceDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndInvoiceDateOption:HANDLE).
        
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartOrderDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndOrderDateOption:HANDLE).
        
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartDueDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndDueDateOption:HANDLE).
        
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartShipDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndShipDateOption:HANDLE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetCADRange sObject 
PROCEDURE pSetCADRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          svStartCAD:READ-ONLY = iplChecked
          svEndCAD:READ-ONLY   = iplChecked
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetCustRange sObject 
PROCEDURE pSetCustRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          svStartCustNo:READ-ONLY = iplChecked
          svEndCustNo:READ-ONLY   = iplChecked
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetItemRange sObject 
PROCEDURE pSetItemRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          svStartItemNo:READ-ONLY = iplChecked
          svEndItemNo:READ-ONLY   = iplChecked
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetJobRange sObject 
PROCEDURE pSetJobRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          svStartJobNo:READ-ONLY  = iplChecked
          svStartJobNo2:READ-ONLY = iplChecked
          svEndJobNo:READ-ONLY    = iplChecked
          svEndJobNo2:READ-ONLY   = iplChecked
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetMachineRange sObject 
PROCEDURE pSetMachineRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          svStartMachine:READ-ONLY = iplChecked
          svEndMachine:READ-ONLY   = iplChecked
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetOrderRange sObject 
PROCEDURE pSetOrderRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          svStartOrderNo:READ-ONLY = iplChecked
          svEndOrderNo:READ-ONLY   = iplChecked
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetPONumberRange sObject 
PROCEDURE pSetPONumberRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          svStartPONumber:READ-ONLY = iplChecked
          svEndPONumber:READ-ONLY   = iplChecked
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetProdCategoryRange sObject 
PROCEDURE pSetProdCategoryRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          svStartProdCategory:READ-ONLY = iplChecked
          svEndProdCategory:READ-ONLY   = iplChecked
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetSalesRepRange sObject 
PROCEDURE pSetSalesRepRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          svStartSalesRep:READ-ONLY = iplChecked
          svEndSalesRep:READ-ONLY   = iplChecked
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetUserIDRange sObject 
PROCEDURE pSetUserIDRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          svStartUserID:READ-ONLY = iplChecked
          svEndUserID:READ-ONLY   = iplChecked
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUseCustList sObject 
PROCEDURE pUseCustList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          svStartCustNo:READ-ONLY = iplChecked
          svEndCustNo:READ-ONLY   = iplChecked
          btnCustList:SENSITIVE   = iplChecked
          .
      IF iplChecked THEN
      ASSIGN svAllCustNo:SCREEN-VALUE = "no".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

