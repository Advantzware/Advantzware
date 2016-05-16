&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: r-commcr.w

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
&Scoped-Define ENABLED-OBJECTS  svCompany svStartCustNo svEndCustNo ~
svStartOrdDate svStartOrdDateOption svEndOrdDate svEndOrdDateOption ~
svStartDueDate svStartDueDateOption svEndDueDate svEndDueDateOption svStartPo ~
svEndPo svStartJob svStartJob2 svEndJob svEndJob2 svStartItem svEndItem  ~
svStartCad svEndCad svStartUser svEndUser svStartRep svEndRep  svSortPer ~
svSortSec svPrintJobQtyDetails svIncOrdBalItem svSelJob svDropUnderrun ~
svIncQtyWIP svIncRelQty svSelOrd svJobsWQOH svSelWip svInActive-2 
&Scoped-Define DISPLAYED-OBJECTS RECT-1 svCompany svStartCustNo startCustName ~
svEndCustNo endCustName svStartOrdDate svStartOrdDateOption svEndOrdDate ~
svEndOrdDateOption svStartDueDate svStartDueDateOption svEndDueDate ~
svEndDueDateOption svStartPo svEndPo  svStartJob svStartJob2 svEndJob svEndJob2 ~
svStartItem svEndItem svStartCad svEndCad svStartUser svEndUser svEndRep ~
svStartRep svSortPer svSortSec svPrintJobQtyDetails svIncOrdBalItem ~
svSelJob svDropUnderrun svIncQtyWIP svIncRelQty svSelOrd svJobsWQOH ~
svSelWip svInActive-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE svEndDueDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svEndOrdDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartDueDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartOrdDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE endCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.

DEFINE VARIABLE startCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.

DEFINE VARIABLE svCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE svEndCad AS CHARACTER FORMAT "X(15)" 
     LABEL "End CAD#" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1.

DEFINE VARIABLE svEndCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "End Customer" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndDueDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Due Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndItem AS CHARACTER FORMAT "X(15)" 
     LABEL "End Item#" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1.

DEFINE VARIABLE svEndJob AS CHARACTER FORMAT "X(6)" 
     LABEL "End Job#" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndJob2 AS INTEGER FORMAT ">9" INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1.

DEFINE VARIABLE svEndOrdDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Order Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndPo AS CHARACTER FORMAT "X(15)" 
     LABEL "End Customer Po#" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1.

DEFINE VARIABLE svEndRep AS CHARACTER FORMAT "X(3)" 
     LABEL "End SalesRep#" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1.

DEFINE VARIABLE svEndUser AS CHARACTER FORMAT "X(8)" 
     LABEL "End UserID#" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1.

DEFINE VARIABLE svStartJob2 AS INTEGER FORMAT ">9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.8 BY 1.

DEFINE VARIABLE svStartCad AS CHARACTER FORMAT "X(15)" 
     LABEL "Start CAD#" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1.

DEFINE VARIABLE svStartCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Customer" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartDueDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Due Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartItem AS CHARACTER FORMAT "X(15)" 
     LABEL "Start Item#" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1.

DEFINE VARIABLE svStartJob AS CHARACTER FORMAT "X(6)" 
     LABEL "Start Job#" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartOrdDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Order Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartPo AS CHARACTER FORMAT "X(15)" 
     LABEL "Start Customer Po#" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1.

DEFINE VARIABLE svStartRep AS CHARACTER FORMAT "X(3)" 
     LABEL "Start SalesRep#" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1.

DEFINE VARIABLE svStartUser AS CHARACTER FORMAT "X(8)" 
     LABEL "Start UserID#" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1.

DEFINE VARIABLE svSelJob AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"All", "All"
     SIZE 26.6 BY .81 NO-UNDO.

DEFINE VARIABLE svSelOrd AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"All", "All"
     SIZE 26.6 BY .81 NO-UNDO.

DEFINE VARIABLE svSelWip AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Order Qty - OH - Ship", "1",
"Job Qty - Rcpts", "2"
     SIZE 46.6 BY .81 NO-UNDO.

DEFINE VARIABLE svSortPer AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer#", "Customer#",
"Line Due Date", "Due Date",
"Release Due Date", "Rel Date",
"Sales Rep", "Salesman"
     SIZE 75 BY 1.19 NO-UNDO.

DEFINE VARIABLE svSortSec AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "PO#", "PO#",
"Item", "Item",
"Cust Part#", "Cust Part#",
"FG Item Name", "FG Item Name",
"Order#", "Order#",
"Due Date", "Due Date",
"CAD#", "CAD#"
     SIZE 83.6 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 2.86.

DEFINE VARIABLE svDropUnderrun AS LOGICAL INITIAL no 
     LABEL "Drop Order Underrun%" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.2 BY 1 NO-UNDO.

DEFINE VARIABLE svInActive-2 AS LOGICAL INITIAL no 
     LABEL "Include Inactive Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE svIncOrdBalItem AS LOGICAL INITIAL no 
     LABEL "Include 0 Order Balance Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34.4 BY 1 NO-UNDO.

DEFINE VARIABLE svIncQtyWIP AS LOGICAL INITIAL no 
     LABEL "Include 0 Qty WIP Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE svIncRelQty AS LOGICAL INITIAL no 
     LABEL "Include 0 Qty/Act. Release Qty = 0" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE svJobsWQOH AS LOGICAL INITIAL no 
     LABEL "Include Jobs w/QOH?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE svPrintJobQtyDetails AS LOGICAL INITIAL no 
     LABEL "Print Job Qty Details?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.2 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.33 COL 23 COLON-ALIGNED WIDGET-ID 60
     svStartCustNo AT ROW 2.76 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Customer" WIDGET-ID 2
     startCustName AT ROW 2.76 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Customer Name" NO-LABEL WIDGET-ID 4
     svEndCustNo AT ROW 3.71 COL 23 COLON-ALIGNED HELP
          "Enter Ending Customer" WIDGET-ID 6
     endCustName AT ROW 3.71 COL 40 COLON-ALIGNED HELP
          "Enter Ending Customer Name" NO-LABEL WIDGET-ID 8
     svStartOrdDate AT ROW 4.67 COL 23 COLON-ALIGNED HELP
          "Enter Start Order Date" WIDGET-ID 84
     svStartOrdDateOption AT ROW 4.67 COL 41.2 COLON-ALIGNED HELP
          "Select Start Receipt Date Option" NO-LABEL WIDGET-ID 86
     svEndOrdDate AT ROW 5.62 COL 23 COLON-ALIGNED HELP
          "Enter End Order Date" WIDGET-ID 80
     svEndOrdDateOption AT ROW 5.62 COL 41.2 COLON-ALIGNED HELP
          "Select End Order Date Option" NO-LABEL WIDGET-ID 82
     svStartDueDate AT ROW 6.62 COL 23 COLON-ALIGNED HELP
          "Enter Start Due Date" WIDGET-ID 132
     svStartDueDateOption AT ROW 6.62 COL 41.2 COLON-ALIGNED HELP
          "Select Start Receipt Date Option" NO-LABEL WIDGET-ID 134
     svEndDueDate AT ROW 7.57 COL 23 COLON-ALIGNED HELP
          "Enter End Due Date" WIDGET-ID 128
     svEndDueDateOption AT ROW 7.57 COL 41.2 COLON-ALIGNED HELP
          "Select End Due Date Option" NO-LABEL WIDGET-ID 130
    svStartPo AT ROW 8.52 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Customer Po#" WIDGET-ID 22
     svEndPo AT ROW 8.43 COL 73 COLON-ALIGNED HELP
          "Enter Ending Customer Po" WIDGET-ID 20
     svStartJob AT ROW 9.48 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Job#" WIDGET-ID 90
     svStartJob2 AT ROW 9.52 COL 38.4 COLON-ALIGNED HELP
          "Enter Beginning Prod Category#" NO-LABEL WIDGET-ID 112
     svEndJob AT ROW 9.38 COL 73 COLON-ALIGNED HELP
          "Enter Ending Prod Category" WIDGET-ID 88
     svEndJob2 AT ROW 9.43 COL 88.6 COLON-ALIGNED HELP
          "Enter Beginning Job2#" NO-LABEL WIDGET-ID 114
     svStartItem AT ROW 10.48 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Item#" WIDGET-ID 118
     svEndItem AT ROW 10.38 COL 73 COLON-ALIGNED HELP
          "Enter Ending Item" WIDGET-ID 116
     svStartCad AT ROW 11.43 COL 23 COLON-ALIGNED HELP
          "Enter Beginning CAD#" WIDGET-ID 122
     svEndCad AT ROW 11.29 COL 73 COLON-ALIGNED HELP
          "Enter Ending CAD" WIDGET-ID 120
     svStartUser AT ROW 12.43 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Userid#" WIDGET-ID 138
     svEndUser AT ROW 12.33 COL 73 COLON-ALIGNED HELP
          "Enter Ending UserID" WIDGET-ID 136
     svStartRep AT ROW 13.43 COL 23 COLON-ALIGNED HELP
          "Enter BeginningSalesRep" WIDGET-ID 142
     svEndRep AT ROW 13.33 COL 73 COLON-ALIGNED HELP
          "Enter Ending SalesRep" WIDGET-ID 140
     svSortPer AT ROW 15.29 COL 28.4 NO-LABEL WIDGET-ID 144
     svSortSec AT ROW 16.33 COL 28.4 NO-LABEL WIDGET-ID 150
     svPrintJobQtyDetails AT ROW 18.14 COL 54.6 WIDGET-ID 92
     svIncOrdBalItem AT ROW 18.14 COL 79.6 WIDGET-ID 42
     svSelJob AT ROW 19.1 COL 23.4 NO-LABEL WIDGET-ID 172
     svDropUnderrun AT ROW 19.1 COL 54.6 WIDGET-ID 100
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 118.2 BY 24.81.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     svIncQtyWIP AT ROW 19.1 COL 79.6 WIDGET-ID 94
     svIncRelQty AT ROW 20.14 COL 79.6 WIDGET-ID 96
     svSelOrd AT ROW 20.24 COL 23.4 NO-LABEL WIDGET-ID 176
     svJobsWQOH AT ROW 21.24 COL 79.6 WIDGET-ID 98
     svSelWip AT ROW 21.38 COL 23.4 NO-LABEL WIDGET-ID 180
     svInActive-2 AT ROW 22.43 COL 79.6 WIDGET-ID 184
     "" VIEW-AS TEXT
          SIZE 67 BY .95 AT ROW 23.86 COL 12 WIDGET-ID 2
     " Sort By" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 14.71 COL 8.8
     "WIP Qty?" VIEW-AS TEXT
          SIZE 13.8 BY .62 AT ROW 21.48 COL 8.6 WIDGET-ID 170
     "Order Status?" VIEW-AS TEXT
          SIZE 14.8 BY .62 AT ROW 20.38 COL 8.6 WIDGET-ID 168
     "Job Status?" VIEW-AS TEXT
          SIZE 14.8 BY .62 AT ROW 19.19 COL 8.6 WIDGET-ID 166
     "Print Selections" VIEW-AS TEXT
          SIZE 16.8 BY .62 AT ROW 18.14 COL 7.8 WIDGET-ID 164
     "Secondary Sort?" VIEW-AS TEXT
          SIZE 17.2 BY .62 AT ROW 16.57 COL 10 WIDGET-ID 162
     "Primary Sort?" VIEW-AS TEXT
          SIZE 13.2 BY .62 AT ROW 15.52 COL 14 WIDGET-ID 160
     RECT-1 AT ROW 15.05 COL 7 WIDGET-ID 158
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 118.2 BY 24.81
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
         HEIGHT             = 24.81
         WIDTH              = 118.2.
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

/* SETTINGS FOR FILL-IN endCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startCustName IN FRAME F-Main
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

&Scoped-define SELF-NAME svCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCompany sObject
ON ENTRY OF svCompany IN FRAME F-Main /* Company */
DO:
  APPLY "ENTRY":U TO svStartCustNo.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndCad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndCad sObject
ON LEAVE OF svEndCad IN FRAME F-Main /* End CAD# */
DO:
    ASSIGN {&SELF-NAME}.
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


&Scoped-define SELF-NAME svEndDueDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndDueDateOption sObject
ON VALUE-CHANGED OF svEndDueDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svEndOrdDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndItem sObject
ON LEAVE OF svEndItem IN FRAME F-Main /* End Item# */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndJob sObject
ON LEAVE OF svEndJob IN FRAME F-Main /* End Job# */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndJob2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndJob2 sObject
ON LEAVE OF svEndJob2 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndOrdDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndOrdDateOption sObject
ON VALUE-CHANGED OF svEndOrdDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svEndOrdDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndPo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndPo sObject
ON LEAVE OF svEndPo IN FRAME F-Main /* End Customer Po# */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndRep sObject
ON LEAVE OF svEndRep IN FRAME F-Main /* End SalesRep# */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndUser sObject
ON LEAVE OF svEndUser IN FRAME F-Main /* End UserID# */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartJob2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartJob2 sObject
ON LEAVE OF svStartJob2 IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartCad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartCad sObject
ON LEAVE OF svStartCad IN FRAME F-Main /* Start CAD# */
DO:
    ASSIGN {&SELF-NAME}.
    
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


&Scoped-define SELF-NAME svStartDueDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartDueDateOption sObject
ON VALUE-CHANGED OF svStartDueDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svStartOrdDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartItem sObject
ON LEAVE OF svStartItem IN FRAME F-Main /* Start Item# */
DO:
    ASSIGN {&SELF-NAME}.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartJob sObject
ON LEAVE OF svStartJob IN FRAME F-Main /* Start Job# */
DO:
    ASSIGN {&SELF-NAME}.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartOrdDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartOrdDateOption sObject
ON VALUE-CHANGED OF svStartOrdDateOption IN FRAME F-Main
DO:
  ASSIGN
      {&SELF-NAME}
      svStartOrdDate:READ-ONLY = {&SELF-NAME} NE "Fixed date"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartPo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartPo sObject
ON LEAVE OF svStartPo IN FRAME F-Main /* Start Customer Po# */
DO:
    ASSIGN {&SELF-NAME}.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartRep sObject
ON LEAVE OF svStartRep IN FRAME F-Main /* Start SalesRep# */
DO:
    ASSIGN {&SELF-NAME}.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartUser sObject
ON LEAVE OF svStartUser IN FRAME F-Main /* Start UserID# */
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
        APPLY "VALUE-CHANGED":U TO svStartOrdDateOption.
        APPLY "VALUE-CHANGED":U TO svEndOrdDateOption.
        /*APPLY "VALUE-CHANGED":U TO svCustList.
        APPLY "VALUE-CHANGED":U TO svAllCustomers.
        APPLY "VALUE-CHANGED":U TO svAllSalesReps.*/
        APPLY "LEAVE":U TO svStartCustNo.
        APPLY "LEAVE":U TO svEndCustNo.
      /*  APPLY "LEAVE":U TO svStartOrd.
        APPLY "LEAVE":U TO svEndOrd.**/
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
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartOrdDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndOrdDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartDueDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndDueDateOption:HANDLE).
        
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

