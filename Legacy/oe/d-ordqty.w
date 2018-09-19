&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: oe\d-ordqty.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{oe/tt-item-qty-price.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def input param ip-recid as recid no-undo.
def output parameter op-qty as int no-undo.
DEF OUTPUT PARAMETER op-price LIKE quoteqty.price NO-UNDO.
DEF OUTPUT PARAMETER op-uom AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER op-rels LIKE quoteqty.rels NO-UNDO.
DEF OUTPUT PARAMETER op-error AS LOG INIT YES NO-UNDO.
DEF OUTPUT PARAMETER TABLE FOR tt-item-qty-price.

DEF VAR lv-first-quote-date AS DATE NO-UNDO.
DEF VAR columnCounts AS INT NO-UNDO.
DEF VAR cellColumn AS HANDLE EXTENT 10 NO-UNDO.
DEF VAR idx AS INT NO-UNDO.
DEF VAR v-current-rowid AS ROWID.

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
&Scoped-define INTERNAL-TABLES tt-item-qty-price

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-item-qty-price.tt-selected tt-item-qty-price.q-no tt-item-qty-price.quote-date tt-item-qty-price.part-no tt-item-qty-price.qty tt-item-qty-price.rels tt-item-qty-price.price tt-item-qty-price.uom tt-item-qty-price.quote-user   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 tt-item-qty-price.tt-selected   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 tt-item-qty-price
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 tt-item-qty-price
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-item-qty-price BY tt-item-qty-price.q-no DESC BY tt-item-qty-price.qty
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-item-qty-price BY tt-item-qty-price.q-no DESC BY tt-item-qty-price.qty.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-item-qty-price
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-item-qty-price


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 Btn_OK scr-quote-no btn-select 
&Scoped-Define DISPLAYED-OBJECTS scr-quote-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-select 
     LABEL "Select Items from Quote" 
     SIZE 25.6 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE scr-quote-no AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     LABEL "Select All Items on Quote #" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-item-qty-price SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 DISPLAY
      tt-item-qty-price.tt-selected FORMAT "Yes/No" COLUMN-LABEL "Selected"
      tt-item-qty-price.q-no 
      tt-item-qty-price.quote-date
      tt-item-qty-price.part-no COLUMN-LABEL "Cust Part #"
      tt-item-qty-price.qty COLUMN-LABEL "Qty."
      tt-item-qty-price.rels COLUMN-LABEL "Releases"
      tt-item-qty-price.price
      tt-item-qty-price.uom
      tt-item-qty-price.quote-user
      ENABLE
          tt-item-qty-price.tt-selected
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 105 BY 11.19
         BGCOLOR 8  ROW-HEIGHT-CHARS .67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     Btn_OK AT ROW 12.67 COL 3
     scr-quote-no AT ROW 12.67 COL 44.2 COLON-ALIGNED WIDGET-ID 4
     btn-select AT ROW 12.67 COL 61 WIDGET-ID 6
     SPACE(19.40) SKIP(0.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Select Sell Price(s)"
         DEFAULT-BUTTON Btn_OK.


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
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-item-qty-price BY tt-item-qty-price.q-no DESC BY tt-item-qty-price.qty.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Select Sell Price(s) */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
   ASSIGN
      tt-item-qty-price.tt-selected = YES
      op-qty = tt-item-qty-price.qty
      op-price = tt-item-qty-price.price
      op-uom = tt-item-qty-price.uom
      op-rels = tt-item-qty-price.rels
      op-error = NO.

   RUN update-selected-proc(INPUT ROWID(tt-item-qty-price), INPUT tt-item-qty-price.LINE).

   apply "window-close" to frame {&frame-name}.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON ROW-DISPLAY OF BROWSE-1 IN FRAME Dialog-Frame
DO:
   DO idx = 1 TO columncounts:
     IF tt-item-qty-price.quote-date = lv-first-quote-date THEN
        cellColumn[idx]:BGCOLOR = 10.
      ELSE cellColumn[idx]:BGCOLOR = 12.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-select Dialog-Frame
ON CHOOSE OF btn-select IN FRAME Dialog-Frame /* Select Items from Quote */
DO:
   DO WITH FRAME {&FRAME-NAME}:

      ASSIGN scr-quote-no.

      IF NOT CAN-FIND(FIRST tt-item-qty-price WHERE
         tt-item-qty-price.q-no EQ scr-quote-no) THEN
         DO:
            MESSAGE "Quote # not found for this estimate."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            LEAVE.
         END.     

      IF AVAIL tt-item-qty-price THEN
         v-current-rowid = ROWID(tt-item-qty-price).
         
      FOR EACH tt-item-qty-price:

          IF tt-item-qty-price.q-no NE scr-quote-no THEN
             tt-item-qty-price.tt-selected = NO.
          ELSE
             tt-item-qty-price.tt-selected = YES.
      END.

      OPEN QUERY browse-1 FOR EACH tt-item-qty-price BY tt-item-qty-price.q-no DESC BY tt-item-qty-price.qty.
      REPOSITION browse-1 TO ROWID v-current-rowid.

   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   ASSIGN
      op-qty = tt-item-qty-price.qty
      op-price = tt-item-qty-price.price
      op-uom = tt-item-qty-price.uom
      op-rels = tt-item-qty-price.rels
      op-error = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-quote-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-quote-no Dialog-Frame
ON HELP OF scr-quote-no IN FRAME Dialog-Frame /* Select All Items on Quote # */
DO:
   DO WITH FRAME {&FRAME-NAME}:
   
      DEF VAR op-quote-no AS INT NO-UNDO.

      ASSIGN scr-quote-no.

      RUN windows\l-quoteord.w(INPUT TABLE tt-item-qty-price,
                               INPUT scr-quote-no,
                               OUTPUT op-quote-no).

      IF op-quote-no NE 0 THEN
         scr-quote-no:SCREEN-VALUE = STRING(op-quote-no).

   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

ON 'mouse-select-dblclick' OF tt-item-qty-price.tt-selected IN BROWSE {&BROWSE-NAME} DO:
  
   ASSIGN 
      tt-item-qty-price.tt-selected = 
          NOT LOGICAL(tt-item-qty-price.tt-selected:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      v-current-rowid = ROWID(tt-item-qty-price).
  
   IF tt-item-qty-price.tt-selected = YES THEN
      RUN update-selected-proc(INPUT ROWID(tt-item-qty-price), INPUT tt-item-qty-price.LINE).
  
   OPEN QUERY browse-1 FOR EACH tt-item-qty-price BY tt-item-qty-price.q-no DESC BY tt-item-qty-price.qty.
   REPOSITION browse-1 TO ROWID v-current-rowid.
  
   RETURN NO-APPLY.
END.

ON 'value-changed' OF tt-item-qty-price.tt-selected IN BROWSE {&BROWSE-NAME} DO:
  
   ASSIGN 
      tt-item-qty-price.tt-selected = 
          LOGICAL(tt-item-qty-price.tt-selected:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      v-current-rowid = ROWID(tt-item-qty-price).
  
   IF tt-item-qty-price.tt-selected = YES THEN
   DO:
      RUN update-selected-proc(INPUT ROWID(tt-item-qty-price), INPUT tt-item-qty-price.LINE).
      OPEN QUERY browse-1 FOR EACH tt-item-qty-price BY tt-item-qty-price.q-no DESC BY tt-item-qty-price.qty.
      REPOSITION browse-1 TO ROWID v-current-rowid.
   END.
  
   RETURN NO-APPLY.
END.


/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:

DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  run build-table.
  
  RUN enable_UI.
  

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table Dialog-Frame 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  find est-qty where recid(est-qty) = ip-recid no-lock.

  lv-first-quote-date = ?.
  FOR EACH quotehd NO-LOCK 
      WHERE quotehd.company EQ est-qty.company AND
      quotehd.est-no EQ est-qty.est-no AND 
      quotehd.quo-date LE TODAY AND
      quotehd.expireDate GE TODAY ,
  EACH quoteitm WHERE
      quoteitm.company = est-qty.company AND
      quoteitm.est-no = est-qty.est-no AND
      CAN-FIND(FIRST eb WHERE
      eb.company EQ est-qty.company AND
      eb.est-no EQ est-qty.est-no AND
      eb.part-no EQ quoteitm.part-no)
      NO-LOCK,
      EACH quoteqty WHERE
           quoteqty.company = quoteitm.company AND
           quoteqty.loc = quoteitm.loc AND
           quoteqty.q-no = quoteitm.q-no AND
           quoteqty.line = quoteitm.line
           NO-LOCK
           BY quoteitm.q-no DESC
           BY quoteitm.LINE
           BY quoteitm.qty:

      CREATE tt-item-qty-price.
      ASSIGN tt-item-qty-price.q-no = quoteitm.q-no
             tt-item-qty-price.LINE = quoteitm.LINE
             tt-item-qty-price.quote-date = quoteqty.quote-date
             tt-item-qty-price.part-no = quoteitm.part-no
             tt-item-qty-price.qty = quoteqty.qty
             tt-item-qty-price.price = quoteqty.price
             tt-item-qty-price.uom = quoteqty.uom
             tt-item-qty-price.rels = quoteqty.rels
             tt-item-qty-price.quote-user = quoteqty.quote-user.
     /* Ticket 15599 */
     /* FIND FIRST quotehd OF quoteitm WHERE quotehd.company EQ quoteitm.company NO-LOCK NO-ERROR .
      IF AVAIL quotehd THEN*/
          ASSIGN  tt-item-qty-price.quote-date = quotehd.quo-date .

      IF NOT CAN-FIND(FIRST tt-item-qty-price WHERE
         tt-item-qty-price.part-no EQ quoteitm.part-no AND
         tt-item-qty-price.tt-selected) THEN
         tt-item-qty-price.tt-selected = YES.    

      IF lv-first-quote-date = ? THEN lv-first-quote-date = tt-item-qty-price.quote-date.
  END.
    
  columncounts = {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}.
  DO idx = 1 TO columnCounts:
     cellColumn[idx] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(idx).
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY scr-quote-no 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-1 Btn_OK scr-quote-no btn-select 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-selected-proc Dialog-Frame 
PROCEDURE update-selected-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
   DEFINE INPUT PARAMETER ip-line AS INT NO-UNDO.

   DEF BUFFER b-tt-item-qty-price FOR tt-item-qty-price.

   FOR EACH b-tt-item-qty-price WHERE
       b-tt-item-qty-price.LINE EQ ip-line AND
       ROWID(b-tt-item-qty-price) NE ip-rowid:

       b-tt-item-qty-price.tt-selected = NO.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

