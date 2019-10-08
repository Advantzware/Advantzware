&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def input param ip-company like oe-ord.company no-undo.
def input param ip-cust like oe-ord.cust-no no-undo.
def input param ip-item like oe-ordl.i-no no-undo.
def input param ip-uom like oe-ordl.pr-uom no-undo.

def output param op-char-val as cha no-undo.
def var v-term as cha no-undo.
def var v-term-2 as cha no-undo.

def temp-table tt-rpt field tt-recid as recid
                      field tt-cost as dec
                      field tt-price as dec
                      field tt-uom-price as dec
                      field tt-qty as dec
                      field tt-prof as dec
                      FIELD tt-selldate AS DATE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES report tt-rpt

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 report.key-04 tt-rpt.tt-cost tt-rpt.tt-price tt-rpt.tt-qty tt-rpt.tt-prof   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH report WHERE report.term-id eq v-term NO-LOCK, ~
             each tt-rpt where tt-rpt.tt-recid = recid(report) NO-LOCK BY tt-rpt.tt-selldate DESC
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH report WHERE report.term-id eq v-term NO-LOCK, ~
             each tt-rpt where tt-rpt.tt-recid = recid(report) NO-LOCK BY tt-rpt.tt-selldate DESC.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 report tt-rpt
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 report
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 tt-rpt


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      report, 
      tt-rpt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      report.key-04 label "SaleDate"
      tt-rpt.tt-cost  label "Cost" form ">>>.99<"
      tt-rpt.tt-price label "Sell" form ">>>,>>>.99<"
      tt-rpt.tt-qty   label "Qty" form ">>>>>>>>9"
      tt-rpt.tt-prof  label "Prof%" form "->>>.9<"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 87 BY 13.81
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     SPACE(0.00) SKIP(0.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Price History Information".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-1 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
 /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Price History Information */
DO:
  
   for each report where report.term-id eq v-term:
       delete report.
   end.


  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
   op-char-val = string(tt-rpt.tt-uom-price).
   apply "window-close" to frame {&frame-name}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  v-term = string(year(today),"9999") +
            string(month(today),"99")  +
            string(day(today),"99") +
            string(time,"99999").
            
  IF ip-uom EQ "" THEN ip-uom = "M".

  run reset-report. 

  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  ENABLE BROWSE-1 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-report Dialog-Frame 
PROCEDURE reset-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def var i as int no-undo.
 
def var a as int.


/*{sa/sa-sls02.i} */

for each oe-ordl where oe-ordl.company eq ip-company
                   and oe-ordl.i-no    eq ip-item
                   use-index item no-lock,
    first oe-ord where oe-ord.company eq oe-ordl.company
                   and oe-ord.cust-no eq ip-cust
                   and oe-ord.ord-no  eq oe-ordl.ord-no
             use-index cust no-lock:
  create report.
  assign report.term-id = v-term
         report.key-02  = string(year(oe-ord.ord-date),"9999") +
                    string(month(oe-ord.ord-date),"99")  +
                    string(day(oe-ord.ord-date),"99")
         report.key-03  = string(oe-ord.ord-no,"9999999999")
         report.key-04  = string(month(oe-ord.ord-date),"99")  + "/" +
                    string(day(oe-ord.ord-date),"99")    + "/" +
                    substr(string(year(oe-ord.ord-date),"9999"),3,2)
         report.rec-id  = recid(oe-ordl).

   create tt-rpt.   
   assign tt-rpt.tt-recid = recid(report)
          tt-rpt.tt-selldate = oe-ord.ord-date.
   
   tt-qty = oe-ordl.qty.
   tt-cost = oe-ordl.cost / 1000.
   if tt-cost eq ? then tt-cost = 0.
   if oe-ordl.pr-uom eq "EA" then tt-price = oe-ordl.price.
   else run sys/ref/convcuom.p(oe-ordl.pr-uom, "EA", 0, 0, 0, 0,
                               oe-ordl.price, output tt-price).
   if oe-ordl.pr-uom eq ip-uom then tt-uom-price = oe-ordl.price.
   else run sys/ref/convcuom.p(oe-ordl.pr-uom, ip-uom, 0, 0, 0, 0,
                               oe-ordl.price, output tt-uom-price).
   tt-prof = (1 - (tt-cost / tt-pric)) * 100.
   if tt-prof eq ? then tt-prof = 0.

end.

  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

