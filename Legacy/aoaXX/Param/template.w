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
&Scoped-Define ENABLED-OBJECTS svCompany svStartTranDate btnCalendar-1 ~
svStartTranDateOption svEndTranDate btnCalendar-2 svEndTranDateOption ~
svStartReceiptDate btnCalendar-3 svStartReceiptDateOption svEndReceiptDate ~
btnCalendar-4 svEndReceiptDateOption svStartInvoiceDate btnCalendar-5 ~
svStartInvoiceDateOption svEndInvoiceDate btnCalendar-6 ~
svEndInvoiceDateOption svAllMachine svStartMachine svEndMachine ~
svAllSalesRep svStartSalesRep svEndSalesRep svCustList btnCustList ~
svAllCustNo svStartCustNo svEndCustNo svSort svSubRpt_SubReportName 
&Scoped-Define DISPLAYED-OBJECTS svCompany svStartTranDate ~
svStartTranDateOption svEndTranDate svEndTranDateOption svStartReceiptDate ~
svStartReceiptDateOption svEndReceiptDate svEndReceiptDateOption ~
svStartInvoiceDate svStartInvoiceDateOption svEndInvoiceDate ~
svEndInvoiceDateOption svAllMachine svStartMachine startMachineDescription ~
svEndMachine endMachineDescription svAllSalesRep svStartSalesRep ~
startSalesRepName svEndSalesRep endSalesRepName svCustList svAllCustNo ~
svStartCustNo startCustName svEndCustNo endCustName svSort ~
svSubRpt_SubReportName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 btnCalendar-2 btnCalendar-3 ~
btnCalendar-4 btnCalendar-5 btnCalendar-6 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
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

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .95.

DEFINE VARIABLE svEndInvoiceDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svEndReceiptDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svEndTranDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartInvoiceDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartReceiptDateOption AS CHARACTER FORMAT "X(256)":U 
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

DEFINE VARIABLE endMachineDescription AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1.

DEFINE VARIABLE endSalesRepName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE startCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startMachineDescription AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1.

DEFINE VARIABLE startSalesRepName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE svCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE svEndCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "End Customer" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndInvoiceDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndMachine AS CHARACTER FORMAT "X(8)" 
     LABEL "End Machine" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE svEndReceiptDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndSalesRep AS CHARACTER FORMAT "X(3)" 
     LABEL "End Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svEndTranDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Customer" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartInvoiceDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartMachine AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Machine" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE svStartReceiptDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartSalesRep AS CHARACTER FORMAT "X(3)" 
     LABEL "Start Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svStartTranDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svSort AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Sort Option 1", "Sort Option 1",
"Sort Option 2", "Sort Option 2",
"Sort Option 3", "Sort Option 3"
     SIZE 17.4 BY 3.33 NO-UNDO.

DEFINE VARIABLE svAllCustNo AS LOGICAL INITIAL yes 
     LABEL "All Customers" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE svAllMachine AS LOGICAL INITIAL yes 
     LABEL "All Machines" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE svAllSalesRep AS LOGICAL INITIAL yes 
     LABEL "All Sales Reps" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

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
     svStartTranDate AT ROW 2.67 COL 23 COLON-ALIGNED HELP
          "Enter Start Transaction Date" WIDGET-ID 72
     btnCalendar-1 AT ROW 2.67 COL 41 WIDGET-ID 76
     svStartTranDateOption AT ROW 2.67 COL 44 COLON-ALIGNED HELP
          "Select Start Date Option" NO-LABEL WIDGET-ID 74
     svEndTranDate AT ROW 3.86 COL 23 COLON-ALIGNED HELP
          "Enter End Transaction Date" WIDGET-ID 68
     btnCalendar-2 AT ROW 3.86 COL 41 WIDGET-ID 78
     svEndTranDateOption AT ROW 3.86 COL 44 COLON-ALIGNED HELP
          "Select End Date Option" NO-LABEL WIDGET-ID 70
     svStartReceiptDate AT ROW 5.52 COL 23 COLON-ALIGNED HELP
          "Enter Start Receipt Date" WIDGET-ID 100
     btnCalendar-3 AT ROW 5.52 COL 41.2 WIDGET-ID 92
     svStartReceiptDateOption AT ROW 5.52 COL 44.2 COLON-ALIGNED HELP
          "Select Start Receipt Date Option" NO-LABEL WIDGET-ID 102
     svEndReceiptDate AT ROW 6.71 COL 23.2 COLON-ALIGNED HELP
          "Enter End Receipt Date" WIDGET-ID 96
     btnCalendar-4 AT ROW 6.71 COL 41.2 WIDGET-ID 94
     svEndReceiptDateOption AT ROW 6.71 COL 44.2 COLON-ALIGNED HELP
          "Select End Receipt Date Option" NO-LABEL WIDGET-ID 98
     svStartInvoiceDate AT ROW 8.38 COL 23 COLON-ALIGNED HELP
          "Enter Start Invoice Date" WIDGET-ID 26
     btnCalendar-5 AT ROW 8.38 COL 41 WIDGET-ID 80
     svStartInvoiceDateOption AT ROW 8.38 COL 44 COLON-ALIGNED HELP
          "Select Start Invoice Date Option" NO-LABEL WIDGET-ID 64
     svEndInvoiceDate AT ROW 9.57 COL 23 COLON-ALIGNED HELP
          "Enter End Invoice Date" WIDGET-ID 24
     btnCalendar-6 AT ROW 9.57 COL 41 WIDGET-ID 82
     svEndInvoiceDateOption AT ROW 9.57 COL 44 COLON-ALIGNED HELP
          "Select End Invoice Date Option" NO-LABEL WIDGET-ID 66
     svAllMachine AT ROW 11.24 COL 25 HELP
          "All Macines?" WIDGET-ID 58
     svStartMachine AT ROW 12.43 COL 23 COLON-ALIGNED HELP
          "Enter Start Machine" WIDGET-ID 22
     startMachineDescription AT ROW 12.43 COL 37 COLON-ALIGNED HELP
          "Enter Beginning Customer Name" NO-LABEL WIDGET-ID 18
     svEndMachine AT ROW 13.62 COL 23 COLON-ALIGNED HELP
          "Enter End Machine" WIDGET-ID 20
     endMachineDescription AT ROW 13.62 COL 37 COLON-ALIGNED HELP
          "Enter Ending Customer Name" NO-LABEL WIDGET-ID 16
     svAllSalesRep AT ROW 15.52 COL 25 HELP
          "All Sales Reps?" WIDGET-ID 108
     svStartSalesRep AT ROW 16.71 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep#" WIDGET-ID 112
     startSalesRepName AT ROW 16.71 COL 32 COLON-ALIGNED HELP
          "Enter Beginning Customer Name" NO-LABEL WIDGET-ID 106
     svEndSalesRep AT ROW 17.91 COL 23 COLON-ALIGNED HELP
          "Enter Ending Sales Rep" WIDGET-ID 110
     endSalesRepName AT ROW 17.91 COL 32 COLON-ALIGNED HELP
          "Enter Ending Customer Name" NO-LABEL WIDGET-ID 104
     svCustList AT ROW 20.05 COL 25 WIDGET-ID 48
     btnCustList AT ROW 20.05 COL 55 WIDGET-ID 46
     svAllCustNo AT ROW 21.24 COL 25 HELP
          "All Customers?" WIDGET-ID 56
     svStartCustNo AT ROW 22.43 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Customer" WIDGET-ID 2
     startCustName AT ROW 22.43 COL 39 COLON-ALIGNED HELP
          "Enter Beginning Customer Name" NO-LABEL WIDGET-ID 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 85.8 BY 28.91.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     svEndCustNo AT ROW 23.62 COL 23 COLON-ALIGNED HELP
          "Enter Ending Customer" WIDGET-ID 6
     endCustName AT ROW 23.62 COL 39 COLON-ALIGNED HELP
          "Enter Ending Customer Name" NO-LABEL WIDGET-ID 8
     svSort AT ROW 25.52 COL 24.6 HELP
          "Select Sort Option" NO-LABEL WIDGET-ID 84
     svSubRpt_SubReportName AT ROW 26.71 COL 51 HELP
          "Select to Show Sub Report" WIDGET-ID 88
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 25.52 COL 15 WIDGET-ID 90
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 85.8 BY 28.91
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
         HEIGHT             = 28.91
         WIDTH              = 85.8.
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
/* SETTINGS FOR FILL-IN endCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endMachineDescription IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endSalesRepName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startMachineDescription IN FRAME F-Main
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
  {methods/btnCalendar.i svStartTranDate}
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
  ASSIGN {&SELF-NAME}.
  RUN pSetCustRange ({&SELF-NAME}).
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


&Scoped-define SELF-NAME svAllSalesRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllSalesRep sObject
ON VALUE-CHANGED OF svAllSalesRep IN FRAME F-Main /* All Sales Reps */
DO:
  ASSIGN {&SELF-NAME}.
  RUN pSetSalesRepRange ({&SELF-NAME}).
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
      btnCalendar-4:SENSITIVE = {&SELF-NAME} EQ "Fixed date"
      .
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
                                         ELSE "<Beginning Range Value>".
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
      btnCalendar-3:SENSITIVE = {&SELF-NAME} EQ "Fixed date"
      .
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
        
        APPLY "VALUE-CHANGED":U TO svAllMachine.
        APPLY "LEAVE":U TO svStartMachine.
        APPLY "LEAVE":U TO svEndMachine.
        
        APPLY "VALUE-CHANGED":U TO svAllSalesRep.
        APPLY "LEAVE":U TO svStartSalesRep.
        APPLY "LEAVE":U TO svEndSalesRep.
        
        APPLY "VALUE-CHANGED":U TO svAllCustNo.
        APPLY "LEAVE":U TO svStartCustNo.
        APPLY "LEAVE":U TO svEndCustNo.
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

