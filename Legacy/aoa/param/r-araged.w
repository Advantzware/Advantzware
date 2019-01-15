&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: r-araged.w

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
&Scoped-Define ENABLED-OBJECTS svCompany svAllCompany svStartCompany ~
svEndCompany svCustList btnCustList svAllCustNo svStartCustNo svEndCustNo ~
svAllSalesRep svStartSalesRep svEndSalesRep svAllCurrency svStartCurrency ~
svEndCurrency svStartInvoiceDate btnCalendar-1 svStartInvoiceDateOption ~
svEndInvoiceDate btnCalendar-2 svEndInvoiceDateOption svInactiveCustomers ~
svRecenTrendDays svAllTerms svStartTerms svEndTerms svAsOfDate ~
btnCalendar-3 svAsOfDateOption svPeriodDays1 svPeriodDays2 svPeriodDays3 ~
svPeriodDays4 svIncludePaidInvoices svIncludeFuelSurchages ~
svIncludeFactoredFGItems svSeparateFinanceCharges svType svSort1 svSort2 
&Scoped-Define DISPLAYED-OBJECTS svCompany svAllCompany svStartCompany ~
startCompanyName svEndCompany endCompanyName svCustList svAllCustNo ~
svStartCustNo startCustName svEndCustNo endCustName svAllSalesRep ~
svStartSalesRep startSalesRepName svEndSalesRep endSalesRepName ~
svAllCurrency svStartCurrency startCurrencyName svEndCurrency ~
endCurrencyName svStartInvoiceDate svStartInvoiceDateOption ~
svEndInvoiceDate svEndInvoiceDateOption svInactiveCustomers ~
svRecenTrendDays svAllTerms svStartTerms startTermsName svEndTerms ~
endTermsName svAsOfDate svAsOfDateOption svPeriodDays1 svPeriodDays2 ~
svPeriodDays3 svPeriodDays4 svIncludePaidInvoices svIncludeFuelSurchages ~
svIncludeFactoredFGItems svSeparateFinanceCharges svType svSort1 svSort2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 btnCalendar-2 btnCalendar-3 

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

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .95.

DEFINE VARIABLE svAsOfDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svEndInvoiceDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartInvoiceDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE endCompanyName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE endCurrencyName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE endCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE endSalesRepName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE endTermsName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE startCompanyName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE startCurrencyName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE startCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startSalesRepName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE startTermsName AS CHARACTER FORMAT "X(30)" 
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

DEFINE VARIABLE svEndCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "End Company" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svEndCurrency AS CHARACTER FORMAT "X(3)" 
     LABEL "End Currency" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svEndCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "End Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svEndInvoiceDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndSalesRep AS CHARACTER FORMAT "X(3)" 
     LABEL "End Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svEndTerms AS CHARACTER FORMAT "X(3)" 
     LABEL "End Terms" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svPeriodDays1 AS INTEGER FORMAT ">,>>9":U INITIAL 9999 
     LABEL "Period Days 1" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE svPeriodDays2 AS INTEGER FORMAT ">,>>9":U INITIAL 9999 
     LABEL "2" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE svPeriodDays3 AS INTEGER FORMAT ">,>>9":U INITIAL 9999 
     LABEL "3" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE svPeriodDays4 AS INTEGER FORMAT ">,>>9":U INITIAL 9999 
     LABEL "4" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE svRecenTrendDays AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Days for Recent Trend" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE svStartCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "Start Company" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svStartCurrency AS CHARACTER FORMAT "X(3)" 
     LABEL "Start Currency" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svStartCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartInvoiceDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartSalesRep AS CHARACTER FORMAT "X(3)" 
     LABEL "Start Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svStartTerms AS CHARACTER FORMAT "X(3)" 
     LABEL "Start Terms" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svSort1 AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer No", "Customer No",
"Name", "Name",
"Sales Rep No", "Sales Rep No"
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE svSort2 AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Due Date", "Due Date",
"Invoice Date", "Invoice Date",
"Invoice No", "Invoice No"
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE svType AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Detail", "Detail"
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 82 BY 4.05.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 82 BY 5.24.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 82 BY 4.05.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 82 BY 4.05.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 85 BY 5.24.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 85 BY 4.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 85 BY 4.05.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 85 BY 4.05.

DEFINE VARIABLE svAllCompany AS LOGICAL INITIAL yes 
     LABEL "All Companies" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE svAllCurrency AS LOGICAL INITIAL yes 
     LABEL "All Currency" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE svAllCustNo AS LOGICAL INITIAL yes 
     LABEL "All Customers" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE svAllSalesRep AS LOGICAL INITIAL yes 
     LABEL "All Sales Reps" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE svAllTerms AS LOGICAL INITIAL yes 
     LABEL "All Terms" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE svCustList AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE svInactiveCustomers AS LOGICAL INITIAL no 
     LABEL "Inactive Customers" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE svIncludeFactoredFGItems AS LOGICAL INITIAL no 
     LABEL "Include Factored FG Items" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE svIncludeFuelSurchages AS LOGICAL INITIAL no 
     LABEL "Include Fuel Surchages" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE svIncludePaidInvoices AS LOGICAL INITIAL no 
     LABEL "Include Paid Invoices" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE svSeparateFinanceCharges AS LOGICAL INITIAL no 
     LABEL "Separate Finance Charges" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 163 COLON-ALIGNED WIDGET-ID 60
     svAllCompany AT ROW 2.91 COL 20 HELP
          "All Sales Reps?" WIDGET-ID 292
     svStartCompany AT ROW 4.1 COL 18 COLON-ALIGNED HELP
          "Enter Start Company" WIDGET-ID 296
     startCompanyName AT ROW 4.1 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 290
     svEndCompany AT ROW 5.29 COL 18 COLON-ALIGNED HELP
          "Enter End Company" WIDGET-ID 294
     endCompanyName AT ROW 5.29 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 288
     svCustList AT ROW 7.19 COL 20 WIDGET-ID 48
     btnCustList AT ROW 7.19 COL 50 WIDGET-ID 46
     svAllCustNo AT ROW 8.38 COL 20 HELP
          "All Customers?" WIDGET-ID 56
     svStartCustNo AT ROW 9.57 COL 18 COLON-ALIGNED HELP
          "Enter Start Customer" WIDGET-ID 2
     startCustName AT ROW 9.57 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     svEndCustNo AT ROW 10.76 COL 18 COLON-ALIGNED HELP
          "Enter End Customer" WIDGET-ID 6
     endCustName AT ROW 10.76 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     svAllSalesRep AT ROW 12.67 COL 20 HELP
          "All Sales Reps?" WIDGET-ID 108
     svStartSalesRep AT ROW 13.86 COL 18 COLON-ALIGNED HELP
          "Enter Start Sales Rep" WIDGET-ID 112
     startSalesRepName AT ROW 13.86 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     svEndSalesRep AT ROW 15.05 COL 18 COLON-ALIGNED HELP
          "Enter End Sales Rep" WIDGET-ID 110
     endSalesRepName AT ROW 15.05 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     svAllCurrency AT ROW 16.95 COL 20 HELP
          "All Currency?" WIDGET-ID 302
     svStartCurrency AT ROW 18.14 COL 18 COLON-ALIGNED HELP
          "Enter Start Currency" WIDGET-ID 306
     startCurrencyName AT ROW 18.14 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 300
     svEndCurrency AT ROW 19.33 COL 18 COLON-ALIGNED HELP
          "Enter End Currency" WIDGET-ID 304
     endCurrencyName AT ROW 19.33 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 298
     svStartInvoiceDate AT ROW 2.91 COL 105 COLON-ALIGNED HELP
          "Enter Start Invoice Date" WIDGET-ID 26
     btnCalendar-1 AT ROW 2.91 COL 123 WIDGET-ID 80
     svStartInvoiceDateOption AT ROW 2.91 COL 126 COLON-ALIGNED HELP
          "Select Start Invoice Date Option" NO-LABEL WIDGET-ID 64
     svEndInvoiceDate AT ROW 4.1 COL 105 COLON-ALIGNED HELP
          "Enter End Invoice Date" WIDGET-ID 24
     btnCalendar-2 AT ROW 4.1 COL 123 WIDGET-ID 82
     svEndInvoiceDateOption AT ROW 4.1 COL 126 COLON-ALIGNED HELP
          "Select End Invoice Date Option" NO-LABEL WIDGET-ID 66
     svInactiveCustomers AT ROW 5.33 COL 107 HELP
          "Select to Include Inactive Customers" WIDGET-ID 336
     svRecenTrendDays AT ROW 5.33 COL 161 COLON-ALIGNED HELP
          "Enter Days for Recent Trend" WIDGET-ID 344
     svAllTerms AT ROW 7.19 COL 107 HELP
          "All Terms?" WIDGET-ID 312
     svStartTerms AT ROW 8.38 COL 105 COLON-ALIGNED HELP
          "Enter Start Terms" WIDGET-ID 316
     startTermsName AT ROW 8.38 COL 114 COLON-ALIGNED NO-LABEL WIDGET-ID 310
     svEndTerms AT ROW 9.57 COL 105 COLON-ALIGNED HELP
          "Enter End Terms" WIDGET-ID 314
     endTermsName AT ROW 9.57 COL 114 COLON-ALIGNED NO-LABEL WIDGET-ID 308
     svAsOfDate AT ROW 10.76 COL 105 COLON-ALIGNED HELP
          "Enter As Of Date" WIDGET-ID 274
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 170.8 BY 21.14.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     btnCalendar-3 AT ROW 10.76 COL 123 WIDGET-ID 272
     svAsOfDateOption AT ROW 10.76 COL 126 COLON-ALIGNED HELP
          "Select Start Date Option" NO-LABEL WIDGET-ID 276
     svPeriodDays1 AT ROW 12.67 COL 104 COLON-ALIGNED HELP
          "Enter Days for Period 1" WIDGET-ID 338
     svPeriodDays2 AT ROW 12.67 COL 115 COLON-ALIGNED HELP
          "Enter Days for Period 2" WIDGET-ID 340
     svPeriodDays3 AT ROW 12.67 COL 126 COLON-ALIGNED HELP
          "Enter Days for Period 3" WIDGET-ID 342
     svPeriodDays4 AT ROW 12.67 COL 137 COLON-ALIGNED HELP
          "Enter Days for Period 4"
     svIncludePaidInvoices AT ROW 13.86 COL 106 HELP
          "Select to Include Paid Invoices" WIDGET-ID 88
     svIncludeFuelSurchages AT ROW 15.05 COL 106 HELP
          "Select to Include Fuel Surchages" WIDGET-ID 330
     svIncludeFactoredFGItems AT ROW 13.86 COL 133 HELP
          "Select to Include Factored FG Items" WIDGET-ID 332
     svSeparateFinanceCharges AT ROW 15.05 COL 133 HELP
          "Select to Separate Finance Charges" WIDGET-ID 334
     svType AT ROW 16.95 COL 107 HELP
          "Select Sort Option" NO-LABEL WIDGET-ID 324
     svSort1 AT ROW 18.14 COL 107 HELP
          "Select Sort Option" NO-LABEL WIDGET-ID 84
     svSort2 AT ROW 19.33 COL 107 HELP
          "Select Sort Option" NO-LABEL WIDGET-ID 320
     "Sort By 2:" VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 19.33 COL 96 WIDGET-ID 318
     "Type:" VIEW-AS TEXT
          SIZE 7 BY 1 AT ROW 16.95 COL 99 WIDGET-ID 328
     "Sort By 1:" VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 18.14 COL 96 WIDGET-ID 90
     RECT-1 AT ROW 2.67 COL 2 WIDGET-ID 346
     RECT-2 AT ROW 6.95 COL 2 WIDGET-ID 348
     RECT-3 AT ROW 12.43 COL 2 WIDGET-ID 350
     RECT-4 AT ROW 16.71 COL 2 WIDGET-ID 352
     RECT-5 AT ROW 6.95 COL 85 WIDGET-ID 354
     RECT-6 AT ROW 16.71 COL 85 WIDGET-ID 356
     RECT-7 AT ROW 12.43 COL 85 WIDGET-ID 358
     RECT-8 AT ROW 2.67 COL 85 WIDGET-ID 360
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 170.8 BY 21.14
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
         HEIGHT             = 21.14
         WIDTH              = 170.8.
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
/* SETTINGS FOR FILL-IN endCompanyName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endCurrencyName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endSalesRepName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endTermsName IN FRAME F-Main
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
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-8 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startCompanyName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startCurrencyName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startSalesRepName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startTermsName IN FRAME F-Main
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
  {methods/btnCalendar.i svStartInvoiceDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 sObject
ON CHOOSE OF btnCalendar-2 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndInvoiceDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-3 sObject
ON CHOOSE OF btnCalendar-3 IN FRAME F-Main
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


&Scoped-define SELF-NAME svAllCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllCompany sObject
ON VALUE-CHANGED OF svAllCompany IN FRAME F-Main /* All Companies */
DO:
    {aoa/includes/svAllValueChanged.i svStartCompany svEndCompany}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllCurrency
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllCurrency sObject
ON VALUE-CHANGED OF svAllCurrency IN FRAME F-Main /* All Currency */
DO:
    {aoa/includes/svAllValueChanged.i svStartCurrency svEndCurrency}
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


&Scoped-define SELF-NAME svAllSalesRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllSalesRep sObject
ON VALUE-CHANGED OF svAllSalesRep IN FRAME F-Main /* All Sales Reps */
DO:
    {aoa/includes/svAllValueChanged.i svStartSalesRep svEndSalesRep}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllTerms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllTerms sObject
ON VALUE-CHANGED OF svAllTerms IN FRAME F-Main /* All Terms */
DO:
    {aoa/includes/svAllValueChanged.i svStartTerms svEndTerms}
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
    {aoa/includes/tDateOption.i &dateObject=svAsOfDate &btnCalendar=3}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCompany sObject
ON ENTRY OF svCompany IN FRAME F-Main /* Company */
DO:
  APPLY "ENTRY":U TO svAllCompany.
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


&Scoped-define SELF-NAME svEndCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndCompany sObject
ON LEAVE OF svEndCompany IN FRAME F-Main /* End Company */
DO:
    endCompanyName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndCurrency
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndCurrency sObject
ON LEAVE OF svEndCurrency IN FRAME F-Main /* End Currency */
DO:
    endCurrencyName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
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
    {aoa/includes/tDateOption.i &dateObject=svEndInvoiceDate &btnCalendar=2}
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


&Scoped-define SELF-NAME svEndTerms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndTerms sObject
ON LEAVE OF svEndTerms IN FRAME F-Main /* End Terms */
DO:
    endTermsName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartCompany sObject
ON LEAVE OF svStartCompany IN FRAME F-Main /* Start Company */
DO:
    startCompanyName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartCurrency
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartCurrency sObject
ON LEAVE OF svStartCurrency IN FRAME F-Main /* Start Currency */
DO:
    startCurrencyName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
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
    {aoa/includes/tDateOption.i &dateObject=svStartInvoiceDate &btnCalendar=1}
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


&Scoped-define SELF-NAME svStartTerms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartTerms sObject
ON LEAVE OF svStartTerms IN FRAME F-Main /* Start Terms */
DO:
    startTermsName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
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

        APPLY "VALUE-CHANGED":U TO svAllCompany.
        APPLY "LEAVE":U TO svStartCompany.
        APPLY "LEAVE":U TO svEndCompany.
        
        APPLY "VALUE-CHANGED":U TO svAllCustNo.
        APPLY "LEAVE":U TO svStartCustNo.
        APPLY "LEAVE":U TO svEndCustNo.
        
        APPLY "VALUE-CHANGED":U TO svAllCurrency.
        APPLY "LEAVE":U TO svStartCurrency.
        APPLY "LEAVE":U TO svEndCurrency.
        
        APPLY "VALUE-CHANGED":U TO svAllTerms.
        APPLY "LEAVE":U TO svStartTerms.
        APPLY "LEAVE":U TO svEndTerms.
        
        APPLY "VALUE-CHANGED":U TO svAllSalesRep.
        APPLY "LEAVE":U TO svStartSalesRep.
        APPLY "LEAVE":U TO svEndSalesRep.
        
        APPLY "VALUE-CHANGED":U TO svAsOfDateOption.

        APPLY "VALUE-CHANGED":U TO svStartInvoiceDateOption.
        APPLY "VALUE-CHANGED":U TO svEndInvoiceDateOption.
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
        
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartInvoiceDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndInvoiceDateOption:HANDLE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

