&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME wOrdStatTest
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wOrdStatTest 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

{src/adm2/widgetprto.i}

{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}


DEFINE TEMP-TABLE ttCloseRecord
    FIELD RecordRowID AS ROWID
    FIELD OrderNumber LIKE oe-ord.ord-no
    FIELD ItemNumber LIKE oe-ordl.i-no
    FIELD CloseStatusCurrent LIKE oe-ord.stat
    FIELD CloseStatusNew LIKE oe-ord.stat
    FIELD Reason AS CHAR FORMAT "x(60)"
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME wOrdStatTest
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCloseRecord

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 ttCloseRecord.OrderNumber ttCloseRecord.ItemNumber ttCloseRecord.CloseStatusCurrent ttCloseRecord.CloseStatusNew ttCloseRecord.Reason   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH ttCloseRecord
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH ttCloseRecord.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 ttCloseRecord
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 ttCloseRecord


/* Definitions for DIALOG-BOX wOrdStatTest                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-wOrdStatTest ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cboLookupType company begin_order end_order ~
begin_cust end_cust tbIncludeClosed tbDiffOnly btnGo BROWSE-1 btnUpdate ~
btnDon 
&Scoped-Define DISPLAYED-OBJECTS cboLookupType company begin_order ~
end_order begin_cust end_cust tbIncludeClosed tbDiffOnly 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDon AUTO-GO 
     LABEL "Done" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnGo 
     LABEL "Show Results" 
     SIZE 46 BY 1.14.

DEFINE BUTTON btnUpdate 
     LABEL "Close Open Orders If New Status = C" 
     SIZE 46 BY 1.14.

DEFINE VARIABLE cboLookupType AS CHARACTER FORMAT "X(256)":U INITIAL "Order Lines" 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Order Headers","Order Lines" 
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_order AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE company AS CHARACTER FORMAT "X(6)":U INITIAL "001" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_order AS INTEGER FORMAT ">>>>>>":U INITIAL 999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tbDiffOnly AS LOGICAL INITIAL no 
     LABEL "Only Show Differences" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 NO-UNDO.

DEFINE VARIABLE tbIncludeClosed AS LOGICAL INITIAL no 
     LABEL "Include Closed Orders" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      ttCloseRecord SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 wOrdStatTest _FREEFORM
  QUERY BROWSE-1 DISPLAY
      ttCloseRecord.OrderNumber COLUMN-LABEL "Ord#" FORMAT ">>>>>>"
 ttCloseRecord.ItemNumber FORMAT "X(15)" COLUMN-LABEL "FG Item#"
 ttCloseRecord.CloseStatusCurrent FORMAT "X" COLUMN-LABEL "Old"
 ttCloseRecord.CloseStatusNew FORMAT "X" COLUMN-LABEL "New"
 ttCloseRecord.Reason FORMAT "x(160)" COLUMN-LABEL "Reason"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 137 BY 9.05 ROW-HEIGHT-CHARS .75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME wOrdStatTest
     cboLookupType AT ROW 1.38 COL 24 COLON-ALIGNED WIDGET-ID 2
     company AT ROW 2.52 COL 24 COLON-ALIGNED HELP
          "Enter Company" WIDGET-ID 36
     begin_order AT ROW 3.67 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Order Number" WIDGET-ID 22
     end_order AT ROW 3.67 COL 65 COLON-ALIGNED HELP
          "Enter Ending Order Number" WIDGET-ID 24
     begin_cust AT ROW 4.86 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 28
     end_cust AT ROW 4.86 COL 65 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 30
     tbIncludeClosed AT ROW 6.29 COL 26 WIDGET-ID 34
     tbDiffOnly AT ROW 7.24 COL 26 WIDGET-ID 26
     btnGo AT ROW 8.38 COL 47 WIDGET-ID 10
     BROWSE-1 AT ROW 9.81 COL 5 WIDGET-ID 200
     btnUpdate AT ROW 19.1 COL 47 WIDGET-ID 38
     btnDon AT ROW 19.33 COL 126
     SPACE(2.59) SKIP(0.38)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Order Status Tester"
         DEFAULT-BUTTON btnGo WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wOrdStatTest 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX wOrdStatTest
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-1 btnGo wOrdStatTest */
ASSIGN 
       FRAME wOrdStatTest:SCROLLABLE       = FALSE
       FRAME wOrdStatTest:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttCloseRecord.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX wOrdStatTest
/* Query rebuild information for DIALOG-BOX wOrdStatTest
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX wOrdStatTest */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wOrdStatTest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wOrdStatTest wOrdStatTest
ON WINDOW-CLOSE OF FRAME wOrdStatTest /* Order Status Tester */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust wOrdStatTest
ON LEAVE OF begin_cust IN FRAME wOrdStatTest /* Beginning Customer# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_order
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_order wOrdStatTest
ON LEAVE OF begin_order IN FRAME wOrdStatTest /* Beginning Order# */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGo wOrdStatTest
ON CHOOSE OF btnGo IN FRAME wOrdStatTest /* Show Results */
DO:
    ASSIGN cboLookupType company begin_order end_order begin_cust end_cust 
          tbIncludeClosed tbDiffOnly .
    
    CASE cboLookupType:
        WHEN "Order Headers" THEN
            RUN TestOrderHeader.
        WHEN "Order Lines" THEN
            RUN TestOrderLine.
        OTHERWISE
            RUN TestOrderLine.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdate wOrdStatTest
ON CHOOSE OF btnUpdate IN FRAME wOrdStatTest /* Close Open Orders If New Status = C */
DO:
    ASSIGN cboLookupType company begin_order end_order begin_cust end_cust 
          tbIncludeClosed tbDiffOnly .
    IF NOT CAN-FIND(FIRST ttCloseRecord) THEN
        MESSAGE "No orders listed.  Hit Show Results button first."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    CASE cboLookupType:
        WHEN "Order Headers" THEN
            RUN UpdateOrderHeader.
        WHEN "Order Lines" THEN
            RUN UpdateOrderLine.
        OTHERWISE
            RUN UpdateOrderLine.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cboLookupType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cboLookupType wOrdStatTest
ON VALUE-CHANGED OF cboLookupType IN FRAME wOrdStatTest /* Type */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL company wOrdStatTest
ON LEAVE OF company IN FRAME wOrdStatTest /* Company */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust wOrdStatTest
ON LEAVE OF end_cust IN FRAME wOrdStatTest /* Ending Customer# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_order
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_order wOrdStatTest
ON LEAVE OF end_order IN FRAME wOrdStatTest /* Ending Order# */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbDiffOnly
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbDiffOnly wOrdStatTest
ON VALUE-CHANGED OF tbDiffOnly IN FRAME wOrdStatTest /* Only Show Differences */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbIncludeClosed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbIncludeClosed wOrdStatTest
ON VALUE-CHANGED OF tbIncludeClosed IN FRAME wOrdStatTest /* Include Closed Orders */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wOrdStatTest 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wOrdStatTest  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wOrdStatTest  _DEFAULT-DISABLE
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
  HIDE FRAME wOrdStatTest.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wOrdStatTest  _DEFAULT-ENABLE
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
  DISPLAY cboLookupType company begin_order end_order begin_cust end_cust 
          tbIncludeClosed tbDiffOnly 
      WITH FRAME wOrdStatTest.
  ENABLE cboLookupType company begin_order end_order begin_cust end_cust 
         tbIncludeClosed tbDiffOnly btnGo BROWSE-1 btnUpdate btnDon 
      WITH FRAME wOrdStatTest.
  VIEW FRAME wOrdStatTest.
  {&OPEN-BROWSERS-IN-QUERY-wOrdStatTest}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TestOrderHeader wOrdStatTest 
PROCEDURE TestOrderHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE ttCloseRecord.

MESSAGE cocode SKIP
    begin_order SKIP
    END_order SKIP
    begin_cust SKIP
    END_cust
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH oe-ord
    WHERE oe-ord.company EQ company
      AND oe-ord.ord-no GE begin_order
      AND oe-ord.ord-no LE end_order
      AND oe-ord.cust-no GE begin_cust
      AND oe-ord.cust-no LE end_cust
      AND (tbIncludeClosed OR oe-ord.stat NE 'C')
    NO-LOCK:
    CREATE ttCloseRecord.
    RUN oe/CloseOrder.p(INPUT ROWID(oe-ord),
                        INPUT NO,
                        OUTPUT ttCloseRecord.CloseStatusNew,
                        OUTPUT ttCloseRecord.Reason).
    ASSIGN 
        ttCloseRecord.RecordRowID = ROWID(oe-ord)
        ttCloseRecord.OrderNumber = oe-ord.ord-no
        ttCloseRecord.CloseStatusCurrent = oe-ord.stat
            .  
    IF tbDiffOnly 
        AND ttCloseRecord.CloseStatusCurrent EQ ttCloseRecord.CloseStatusNew THEN
        DELETE ttCloseRecord.

END.
{&CLOSE-QUERY-BROWSE-1}   
{&OPEN-QUERY-BROWSE-1}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TestOrderLine wOrdStatTest 
PROCEDURE TestOrderLine :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE ttCloseRecord.

FOR EACH oe-ordl
    WHERE oe-ordl.company EQ company
      AND oe-ordl.ord-no GE begin_order
      AND oe-ordl.ord-no LE end_order
      AND oe-ordl.cust-no GE begin_cust
      AND oe-ordl.cust-no LE end_cust
      AND (tbIncludeClosed OR oe-ordl.stat NE 'C')
    NO-LOCK:
    CREATE ttCloseRecord.
    RUN oe/CloseOrder.p(INPUT ROWID(oe-ordl),
                        INPUT NO,
                        OUTPUT ttCloseRecord.CloseStatusNew,
                        OUTPUT ttCloseRecord.Reason).
    ASSIGN 
            ttCloseRecord.OrderNumber = oe-ordl.ord-no
            ttCloseRecord.ItemNumber = oe-ordl.i-no
            ttCloseRecord.CloseStatusCurrent = oe-ordl.stat
            .  
    IF tbDiffOnly 
        AND ttCloseRecord.CloseStatusCurrent EQ ttCloseRecord.CloseStatusNew THEN
        DELETE ttCloseRecord.

END.
{&CLOSE-QUERY-BROWSE-1}   
{&OPEN-QUERY-BROWSE-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateOrderHeader wOrdStatTest 
PROCEDURE UpdateOrderHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH ttCloseRecord
    NO-LOCK:
    FIND FIRST oe-ord WHERE ROWID(oe-ord) EQ ttCloseRecord.RecordRowID NO-LOCK NO-ERROR.
    IF AVAIL oe-ord 
        AND oe-ord.stat NE "C" 
        AND ttCloseRecord.CloseStatusCurrent EQ "C" THEN
        RUN oe/CloseOrder.p(INPUT ROWID(oe-ord),
                            INPUT YES,
                            OUTPUT ttCloseRecord.CloseStatusNew,
                            OUTPUT ttCloseRecord.Reason).
    
END.
{&CLOSE-QUERY-BROWSE-1}   
{&OPEN-QUERY-BROWSE-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateOrderLine wOrdStatTest 
PROCEDURE UpdateOrderLine :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH ttCloseRecord
    NO-LOCK:
    FIND FIRST oe-ordl WHERE ROWID(oe-ordl) EQ ttCloseRecord.RecordRowID NO-LOCK NO-ERROR.
    IF AVAIL oe-ordl 
        AND oe-ordl.stat NE "C" 
        AND ttCloseRecord.CloseStatusCurrent EQ "C" THEN
        RUN oe/CloseOrder.p(INPUT ROWID(oe-ordl),
                            INPUT YES,
                            OUTPUT ttCloseRecord.CloseStatusNew,
                            OUTPUT ttCloseRecord.Reason).
    
END.
{&CLOSE-QUERY-BROWSE-1}   
{&OPEN-QUERY-BROWSE-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

