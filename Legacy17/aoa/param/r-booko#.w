&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: r-booko#.w

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
&Scoped-Define ENABLED-OBJECTS svCompany svAllOrderNo svStartOrderNo ~
svEndOrderNo svCustList btnCustList svAllCustNo svStartCustNo svEndCustNo ~
svAllItemNo svStartItemNo svEndItemNo svStartOrderDate btnCalendar-1 ~
svStartOrderDateOption svEndOrderDate btnCalendar-2 svEndOrderDateOption ~
svStartReceiptDate btnCalendar-3 svStartReceiptDateOption svEndReceiptDate ~
btnCalendar-4 svEndReceiptDateOption svUseReceiptDate svStartShipDate ~
btnCalendar-5 svStartShipDateOption svEndShipDate btnCalendar-6 ~
svEndShipDateOption svUseShipDate svPrintOrderedRemaining ~
svPrintMiscCharges svPrintContribution 
&Scoped-Define DISPLAYED-OBJECTS svCompany svAllOrderNo svStartOrderNo ~
svEndOrderNo svCustList svAllCustNo svStartCustNo startCustName svEndCustNo ~
endCustName svAllItemNo svStartItemNo startItemName svEndItemNo endItemName ~
svStartOrderDate svStartOrderDateOption svEndOrderDate svEndOrderDateOption ~
svStartReceiptDate svStartReceiptDateOption svEndReceiptDate ~
svEndReceiptDateOption svUseReceiptDate svStartShipDate ~
svStartShipDateOption svEndShipDate svEndShipDateOption svUseShipDate ~
svPrintOrderedRemaining svPrintMiscCharges svPrintContribution 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 btnCalendar-2 btnCalendar-3 ~
btnCalendar-4 btnCalendar-5 btnCalendar-6 

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

DEFINE BUTTON btnCalendar-5 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-6 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .95.

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

DEFINE VARIABLE endCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE endItemName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE startCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startItemName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

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

DEFINE VARIABLE svEndOrderDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Order Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndOrderNo AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "End Order" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svEndReceiptDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndShipDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Ship Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Customer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE svStartItemNo AS CHARACTER FORMAT "X(15)" 
     LABEL "Start Item" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svStartOrderDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Order Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartOrderNo AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Start Order" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svStartReceiptDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartShipDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Ship Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svPrintOrderedRemaining AS CHARACTER INITIAL "Ordered" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ordered", "Ordered",
"Remaining", "Remaining"
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE svAllCustNo AS LOGICAL INITIAL yes 
     LABEL "All Customers" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE svAllItemNo AS LOGICAL INITIAL yes 
     LABEL "All Items" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE svAllOrderNo AS LOGICAL INITIAL yes 
     LABEL "All Orders" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .95 NO-UNDO.

DEFINE VARIABLE svCustList AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE svPrintContribution AS LOGICAL INITIAL no 
     LABEL "Print Contribution?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95 NO-UNDO.

DEFINE VARIABLE svPrintMiscCharges AS LOGICAL INITIAL no 
     LABEL "Print Misc. Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.6 BY 1 NO-UNDO.

DEFINE VARIABLE svUseReceiptDate AS LOGICAL INITIAL no 
     LABEL "Use?" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE svUseShipDate AS LOGICAL INITIAL no 
     LABEL "Use?" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 19 COLON-ALIGNED WIDGET-ID 60
     svAllOrderNo AT ROW 2.91 COL 21 HELP
          "All Orders?" WIDGET-ID 202
     svStartOrderNo AT ROW 4.1 COL 19 COLON-ALIGNED HELP
          "Enter Start Order" WIDGET-ID 206
     svEndOrderNo AT ROW 5.29 COL 19 COLON-ALIGNED HELP
          "Enter End Order" WIDGET-ID 204
     svCustList AT ROW 6.95 COL 21 WIDGET-ID 48
     btnCustList AT ROW 6.95 COL 51 WIDGET-ID 46
     svAllCustNo AT ROW 8.14 COL 21 HELP
          "All Customers?" WIDGET-ID 56
     svStartCustNo AT ROW 9.33 COL 19 COLON-ALIGNED HELP
          "Enter Start Customer" WIDGET-ID 214
     startCustName AT ROW 9.33 COL 35 COLON-ALIGNED NO-LABEL WIDGET-ID 210
     svEndCustNo AT ROW 10.52 COL 19 COLON-ALIGNED HELP
          "Enter End Customer" WIDGET-ID 212
     endCustName AT ROW 10.52 COL 35 COLON-ALIGNED NO-LABEL WIDGET-ID 208
     svAllItemNo AT ROW 12.19 COL 21 HELP
          "All Items?" WIDGET-ID 164
     svStartItemNo AT ROW 13.38 COL 19 COLON-ALIGNED HELP
          "Enter Start Item" WIDGET-ID 168
     startItemName AT ROW 13.38 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 172
     svEndItemNo AT ROW 14.57 COL 19 COLON-ALIGNED HELP
          "Enter End Item" WIDGET-ID 166
     endItemName AT ROW 14.57 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 170
     svStartOrderDate AT ROW 16.24 COL 19 COLON-ALIGNED HELP
          "Enter Start Order Date" WIDGET-ID 122
     btnCalendar-1 AT ROW 16.24 COL 37 WIDGET-ID 114
     svStartOrderDateOption AT ROW 16.24 COL 40 COLON-ALIGNED HELP
          "Select Start Order Date Option" NO-LABEL WIDGET-ID 124
     svEndOrderDate AT ROW 17.43 COL 19 COLON-ALIGNED HELP
          "Enter End Order Date" WIDGET-ID 118
     btnCalendar-2 AT ROW 17.43 COL 37 WIDGET-ID 116
     svEndOrderDateOption AT ROW 17.43 COL 40 COLON-ALIGNED HELP
          "Select End Order Date Option" NO-LABEL WIDGET-ID 120
     svStartReceiptDate AT ROW 19.1 COL 19 COLON-ALIGNED HELP
          "Enter Start Receipt Date" WIDGET-ID 100
     btnCalendar-3 AT ROW 19.1 COL 37.2 WIDGET-ID 92
     svStartReceiptDateOption AT ROW 19.1 COL 40.2 COLON-ALIGNED HELP
          "Select Start Receipt Date Option" NO-LABEL WIDGET-ID 102
     svEndReceiptDate AT ROW 20.29 COL 19.2 COLON-ALIGNED HELP
          "Enter End Receipt Date" WIDGET-ID 96
     btnCalendar-4 AT ROW 20.29 COL 37.2 WIDGET-ID 94
     svEndReceiptDateOption AT ROW 20.29 COL 40.2 COLON-ALIGNED HELP
          "Select End Receipt Date Option" NO-LABEL WIDGET-ID 98
     svUseReceiptDate AT ROW 20.29 COL 68 HELP
          "Select to Use Receipt Date Range" WIDGET-ID 42
     svStartShipDate AT ROW 21.95 COL 19 COLON-ALIGNED HELP
          "Enter Start Ship Date" WIDGET-ID 192
     btnCalendar-5 AT ROW 21.95 COL 37 WIDGET-ID 186
     svStartShipDateOption AT ROW 21.95 COL 40 COLON-ALIGNED HELP
          "Select Start Ship Date Option" NO-LABEL WIDGET-ID 194
     svEndShipDate AT ROW 23.14 COL 19 COLON-ALIGNED HELP
          "Enter End Ship Date" WIDGET-ID 188
     btnCalendar-6 AT ROW 23.14 COL 37 WIDGET-ID 184
     svEndShipDateOption AT ROW 23.14 COL 40 COLON-ALIGNED HELP
          "Select End Ship Date Option" NO-LABEL WIDGET-ID 190
     svUseShipDate AT ROW 23.14 COL 68 HELP
          "Select to Use Ship Date Range" WIDGET-ID 44
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81.8 BY 28.48.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     svPrintOrderedRemaining AT ROW 24.81 COL 21 HELP
          "Print Qty Ordered or Remaining" NO-LABEL WIDGET-ID 216
     svPrintMiscCharges AT ROW 26.24 COL 21 HELP
          "Select to Use Misc. Charges" WIDGET-ID 40
     svPrintContribution AT ROW 27.43 COL 21 HELP
          "Select to Print Contribution" WIDGET-ID 222
     "Print Quantity:" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 24.57 COL 7 WIDGET-ID 220
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81.8 BY 28.48
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
         HEIGHT             = 28.48
         WIDTH              = 81.8.
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
/* SETTINGS FOR FILL-IN endItemName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startItemName IN FRAME F-Main
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
  {methods/btnCalendar.i svStartShipDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-6 sObject
ON CHOOSE OF btnCalendar-6 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndShipDate}
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


&Scoped-define SELF-NAME svAllOrderNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllOrderNo sObject
ON VALUE-CHANGED OF svAllOrderNo IN FRAME F-Main /* All Orders */
DO:
    {aoa/includes/svAllValueChanged.i svStartOrderNo svEndOrderNo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCompany sObject
ON ENTRY OF svCompany IN FRAME F-Main /* Company */
DO:
  APPLY "ENTRY":U TO svStartOrderNo.
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
    {aoa/includes/tDateOption.i &dateObject=svEndReceiptDate &btnCalendar=4}
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
    {aoa/includes/tDateOption.i &dateObject=svEndShipDate &btnCalendar=6}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svPrintContribution
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svPrintContribution sObject
ON VALUE-CHANGED OF svPrintContribution IN FRAME F-Main /* Print Contribution? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svPrintOrderedRemaining
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svPrintOrderedRemaining sObject
ON VALUE-CHANGED OF svPrintOrderedRemaining IN FRAME F-Main
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
    {aoa/includes/tDateOption.i &dateObject=svStartReceiptDate &btnCalendar=3}
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
    {aoa/includes/tDateOption.i &dateObject=svStartShipDate &btnCalendar=5}
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
        
        APPLY "VALUE-CHANGED":U TO svStartReceiptDateOption.
        APPLY "VALUE-CHANGED":U TO svEndReceiptDateOption.
        
        APPLY "VALUE-CHANGED":U TO svStartShipDateOption.
        APPLY "VALUE-CHANGED":U TO svEndShipDateOption.
        
        APPLY "VALUE-CHANGED":U TO svAllOrderNo.
        
        APPLY "VALUE-CHANGED":U TO svAllCustNo.
        APPLY "LEAVE":U TO svStartCustNo.
        APPLY "LEAVE":U TO svEndCustNo.
        
        APPLY "VALUE-CHANGED":U TO svAllItemNo.
        APPLY "LEAVE":U TO svStartItemNo.
        APPLY "LEAVE":U TO svEndItemNo.
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
            svCompany:SCREEN-VALUE = DYNAMIC-FUNCTION('fGetCompany' IN hContainer)
            svCompany
            .
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartReceiptDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndReceiptDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartOrderDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndOrderDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartShipDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndShipDateOption:HANDLE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

