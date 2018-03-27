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
def input parameter ip-company like itemfg.company no-undo.
def input parameter ip-est-no like item.mat-type no-undo. 
def input parameter ip-cur-val as cha no-undo.
def output parameter op-char-val as cha no-undo. /* string i-code + i-name */

def var lv-first-time as log init yes no-undo.
DEF VAR lv-first-quote-date AS DATE NO-UNDO.
DEF VAR columnCounts AS INT NO-UNDO.
DEF VAR cellColumn AS HANDLE EXTENT 10 NO-UNDO.
DEF VAR idx AS INT NO-UNDO.

&SCOPED-DEFINE sortby-phrase BY quoteqty.quote-date DESC BY quoteqty.qty

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
&Scoped-define INTERNAL-TABLES quoteitm quoteqty

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 quoteitm.q-no quoteqty.quote-date ~
quoteqty.qty quoteqty.price quoteqty.uom quoteqty.quote-user 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH quoteitm WHERE ~{&KEY-PHRASE} ~
      AND quoteitm.company = ip-company ~
and quoteitm.est-no = ip-est-no NO-LOCK, ~
      EACH quoteqty WHERE quoteqty.company = quoteitm.company ~
and quoteqty.loc = quoteitm.loc ~
and quoteqty.q-no = quoteitm.q-no ~
  AND quoteqty.line = quoteitm.line NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH quoteitm WHERE ~{&KEY-PHRASE} ~
      AND quoteitm.company = ip-company ~
and quoteitm.est-no = ip-est-no NO-LOCK, ~
      EACH quoteqty WHERE quoteqty.company = quoteitm.company ~
and quoteqty.loc = quoteitm.loc ~
and quoteqty.q-no = quoteitm.q-no ~
  AND quoteqty.line = quoteitm.line NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 quoteitm quoteqty
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 quoteitm
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 quoteqty


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-color Dialog-Frame 
FUNCTION display-color RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      quoteitm, 
      quoteqty SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      quoteitm.q-no FORMAT ">>>>>>9":U
      quoteqty.quote-date FORMAT "99/99/9999":U
      quoteqty.qty FORMAT ">>>>>>>>>":U
      quoteqty.price FORMAT ">>>,>>9.99<<<":U
      quoteqty.uom FORMAT "x(3)":U
      quoteqty.quote-user FORMAT "X(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 65 BY 13.1
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     SPACE(0.59) SKIP(0.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Order Quantity Information".


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
     _TblList          = "ASI.quoteitm,ASI.quoteqty WHERE ASI.quoteitm ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ","
     _Where[1]         = "ASI.quoteitm.company = ip-company
and quoteitm.est-no = ip-est-no"
     _JoinCode[2]      = "asi.quoteqty.company = asi.quoteitm.company
and asi.quoteqty.loc = asi.quoteitm.loc
and ASI.quoteqty.q-no = ASI.quoteitm.q-no
  AND ASI.quoteqty.line = ASI.quoteitm.line"
     _FldNameList[1]   > ASI.quoteitm.q-no
"quoteitm.q-no" ? ">>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = ASI.quoteqty.quote-date
     _FldNameList[3]   > ASI.quoteqty.qty
"quoteqty.qty" ? ">>>>>>>>>" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.quoteqty.price
"quoteqty.price" ? ">>>,>>9.99<<<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = ASI.quoteqty.uom
     _FldNameList[6]   = ASI.quoteqty.quote-user
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Order Quantity Information */
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
   op-char-val = string(quoteqty.qty) + "," +
                 string(quoteqty.price) + "," +
                 string(quoteqty.uom)
                 .
   apply "window-close" to frame {&frame-name}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON ROW-DISPLAY OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  DO idx = 1 TO columncounts:
     IF quoteqty.quote-date = lv-first-quote-date THEN
        cellColumn[idx]:BGCOLOR = 10.
      ELSE cellColumn[idx]:BGCOLOR = 12.   
   END.
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
 
  columncounts = {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}.
  DO idx = 1 TO columnCounts:
    cellColumn[idx] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(idx).
  END.
  lv-first-quote-date = ?.
  FOR EACH ASI.quoteitm WHERE ASI.quoteitm.company = ip-company
              and quoteitm.est-no = ip-est-no NO-LOCK,
      EACH ASI.quoteqty WHERE ASI.quoteqty.q-no = ASI.quoteitm.q-no
            AND ASI.quoteqty.line = ASI.quoteitm.line NO-LOCK
       BY quoteqty.quote-date DESC :
       lv-first-quote-date = quoteqty.quote-date.
       LEAVE.
  END.
    
  RUN enable_UI.
/*  {custom/lookpos.i &lookup-file = "quoteqty" &lookup-field = "qty" &field-type = "string" } */
/* not working  
  find first quoteitm WHERE ASI.quoteitm.company = ip-company
                      and quoteitm.est-no = ip-est-no NO-LOCK no-error.
  if avail quoteitm then do:
     find first ASI.quoteqty WHERE ASI.quoteqty.q-no = ASI.quoteitm.q-no
                               AND ASI.quoteqty.line = ASI.quoteitm.line
                               and quoteqty.qty ge int(ip-cur-val)
                               use-index q-qty
                               NO-LOCK no-error.
     if avail quoteqty then reposition {&browse-name} to rowid rowid(quoteqty) no-error.
     message "avail" avail quoteqty quoteqty.qty error-status:error view-as alert-box.
     
  end.

   def var lv-rowid as rowid no-undo.
   FOR EACH ASI.quoteitm WHERE {&KEY-PHRASE}
                         AND ASI.quoteitm.company = ip-company
                         and quoteitm.est-no = ip-est-no NO-LOCK,
       EACH ASI.quoteqty WHERE ASI.quoteqty.q-no = ASI.quoteitm.q-no
                         AND ASI.quoteqty.line = ASI.quoteitm.line NO-LOCK
                         {&SORTBY-PHRASE}:
       lv-rowid = rowid(quoteqty).
       leave.
   end.
   if lv-rowid <> ? then  reposition {&browse-name} to rowid lv-rowid no-error.
*/
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-color Dialog-Frame 
FUNCTION display-color RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

