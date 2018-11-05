&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
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
def input parameter ip-company like itemfg.company no-undo.
def input parameter ip-cur-val as CHAR no-undo.
def output parameter op-rowid-list as CHAR no-undo. /* string i-code + i-name */

DEFINE TEMP-TABLE tt-bol LIKE oe-boll
   FIELD selected-flag AS LOG INIT FALSE.

/* Local Variable Definitions ---                                       */
def var lv-type-dscr as cha no-undo.
def var lv-first-time as log init yes no-undo.

&scoped-define SORTBY-1 BY STRING(tt-bol.bol-no)
&scoped-define SORTBY-2 BY tt-bol.i-no {&sortby-1}
&scoped-define fld-name-1 STRING(tt-bol.bol-no)
&scoped-define fld-name-2 tt-bol.i-no
&global-define IAMWHAT LOOKUP

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
{sys/inc/varasgn.i}

DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.
DEF VAR period_pos AS INTEGER NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("NOSWEAT") + "..".
ELSE
ASSIGN
  period_pos = INDEX(PROGRAM-NAME(1),".")
  v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

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
&Scoped-define INTERNAL-TABLES tt-bol

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-bol.bol-no tt-bol.i-no tt-bol.cust-no tt-bol.job-no tt-bol.job-no2 tt-bol.ord-no tt-bol.line tt-bol.po-no tt-bol.qty   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define OPEN-QUERY-BROWSE-1 IF rd-sort = 1 THEN DO:    OPEN QUERY {&SELF-NAME} FOR EACH tt-bol                                  BY tt-bol.bol-no. END. ELSE DO:     OPEN QUERY {&SELF-NAME} FOR EACH tt-bol                                   BY tt-bol.i-no. END.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-bol
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-bol


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 BROWSE-1 bt-cancel bt-ok rd-sort 
&Scoped-Define DISPLAYED-OBJECTS rd-sort 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 11 BY 1.14.

DEFINE BUTTON bt-ok 
     LABEL "&OK" 
     SIZE 11 BY 1.14.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "BOL ", 1,
"FG Item No", 2
     SIZE 53 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 140 BY 1.95.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-bol SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      tt-bol.bol-no COLUMN-LABEL "BOL" FORMAT ">>>>>>>9":U
      tt-bol.i-no FORMAT "x(15)":U
      tt-bol.cust-no FORMAT "x(8)":U WIDTH 12
      tt-bol.job-no FORMAT "x(6)":U WIDTH 14.2
      tt-bol.job-no2 COLUMN-LABEL "" FORMAT "99":U WIDTH 5.2
      tt-bol.ord-no FORMAT ">>>>>9":U WIDTH 10.2
      tt-bol.line FORMAT "99":U WIDTH 7.2
      tt-bol.po-no FORMAT "x(15)":U WIDTH 16.2
      tt-bol.qty FORMAT "->>,>>>,>>9":U WIDTH 25.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 140 BY 12.62
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 2
     bt-cancel AT ROW 13.91 COL 128.8
     bt-ok AT ROW 14 COL 116.2
     lv-search AT ROW 14.1 COL 86 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     rd-sort AT ROW 14.19 COL 14 NO-LABEL
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 14.38 COL 4
     RECT-1 AT ROW 13.81 COL 1
     SPACE(1.39) SKIP(0.33)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "BOL Information".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-1 RECT-1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN lv-search IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
IF rd-sort = 1 THEN DO:
   OPEN QUERY {&SELF-NAME} FOR EACH tt-bol
                                 BY tt-bol.bol-no.
END.
ELSE DO:
    OPEN QUERY {&SELF-NAME} FOR EACH tt-bol
                                  BY tt-bol.i-no.
END.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST OUTER"
     _Where[1]         = "ASI.oe-boll.company = ip-company
AND ASI.oe-boll.i-no BEGINS ip-cur-val
 AND ASI.oe-boll.opened = FALSE"
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* BOL Information */
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
/*    op-char-val = tt-bol.bol-no:screen-value in browse {&browse-name} + "," + */
/*                  tt-bol.i-no:screen-value in browse {&browse-name}           */
/*                  .                                                           */
/*    apply "window-close" to frame {&frame-name}.                              */
RUN set-select.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON MOUSE-EXTEND-CLICK OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  RUN set-select.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON MOUSE-MENU-CLICK OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  RUN set-select.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON MOUSE-MOVE-CLICK OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  RUN set-select.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON MOUSE-SELECT-CLICK OF BROWSE-1 IN FRAME Dialog-Frame
DO:
  RUN set-select.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
/*    RUN set-select. */

   RUN create-vend-whse-trans.
/*    op-char-val = tt-bol.bol-no:screen-value in browse {&browse-name} + "," + */
/*                  tt-bol.i-no:screen-value in browse {&browse-name}           */
                 .
   apply "window-close" to frame {&frame-name}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sort Dialog-Frame
ON VALUE-CHANGED OF rd-sort IN FRAME Dialog-Frame
DO:
    ASSIGN rd-sort.
    {&CLOSE-QUERY-{&BROWSE-NAME}}
    {&OPEN-QUERY-{&BROWSE-NAME}}
    RUN new-rd-sort.
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

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    ASSIGN rd-sort NO-ERROR.

    RUN enable_UI.
    RUN create-tt-bol.
    RUN new-rd-sort.

     {custom/lookpos4.i &lookup-file = "tt-bol" &lookup-field = "i-no"} 
  END.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tt-bol Dialog-Frame 
PROCEDURE create-tt-bol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-vend-whse-trans FOR vend-whse-trans.
DEF BUFFER b-vend-whse-item  FOR vend-whse-item.
DEF BUFFER b-oe-bolh         FOR oe-bolh.
DEF BUFFER b-oe-boll         FOR oe-boll.

FOR EACH b-oe-bolh NO-LOCK WHERE b-oe-bolh.company = ip-company
                             AND b-oe-bolh.posted  = YES
                       USE-INDEX b-no:
   FOR EACH b-oe-boll NO-LOCK WHERE b-oe-boll.company = b-oe-bolh.company
                                AND b-oe-boll.bol-no  = b-oe-bolh.bol-no:
      FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = b-oe-boll.company
                                    AND b-vend-whse-item.fg-item-no  = b-oe-boll.i-no NO-LOCK NO-ERROR.
      IF AVAILABLE b-vend-whse-item THEN DO:
         FIND FIRST tt-bol WHERE tt-bol.company = b-oe-boll.company
                             AND tt-bol.bol-no  = b-oe-boll.bol-no
                             AND tt-bol.i-no    = b-oe-boll.i-no NO-ERROR.
         IF NOT AVAILABLE(tt-bol) THEN DO:
            CREATE tt-bol.
            BUFFER-COPY b-oe-boll EXCEPT b-oe-boll.qty TO tt-bol NO-ERROR. 
         END.
         tt-bol.qty = tt-bol.qty + b-oe-boll.qty.
      END.
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-vend-whse-trans Dialog-Frame 
PROCEDURE create-vend-whse-trans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-r-no LIKE vend-whse-trans.r-no NO-UNDO.

DEF BUFFER b-vend-whse-trans FOR vend-whse-trans.
DEF BUFFER b-vend-whse-trans-hist FOR vend-whse-trans-hist.
DEF BUFFER b-vend-whse-item FOR vend-whse-item.
DEF BUFFER b-oe-ordl FOR oe-ordl.

FOR EACH tt-bol WHERE tt-bol.selected-flag = YES:
   v-r-no = 0.
   FIND LAST b-vend-whse-trans USE-INDEX r-no NO-LOCK NO-ERROR.
   IF AVAIL b-vend-whse-trans AND b-vend-whse-trans.r-no > v-r-no THEN v-r-no = b-vend-whse-trans.r-no.
   
   FIND LAST b-vend-whse-trans-hist USE-INDEX r-no NO-LOCK NO-ERROR.
   IF AVAIL b-vend-whse-trans-hist AND b-vend-whse-trans-hist.r-no GT v-r-no THEN v-r-no = b-vend-whse-trans-hist.r-no.
   
   
   DO WHILE TRUE:
      v-r-no = v-r-no + 1.
   
      FIND FIRST b-vend-whse-trans-hist WHERE b-vend-whse-trans-hist.r-no = v-r-no USE-INDEX r-no NO-LOCK NO-ERROR.
      IF AVAIL b-vend-whse-trans-hist THEN NEXT.
   
      FIND FIRST b-vend-whse-trans WHERE b-vend-whse-trans.r-no = v-r-no USE-INDEX r-no NO-LOCK NO-ERROR.
      IF AVAIL b-vend-whse-trans THEN NEXT.
   
      LEAVE.
   END.
   
   CREATE vend-whse-trans. 
   ASSIGN
      vend-whse-trans.company       = tt-bol.company
      vend-whse-trans.r-no          = v-r-no
      vend-whse-trans.trans-type    = "R"
      vend-whse-trans.trans-date    = tt-bol.bol-date
      vend-whse-trans.create-date   = TODAY
      vend-whse-trans.create-time   = TIME
      vend-whse-trans.create-userid = USERID("nosweat")
      vend-whse-trans.rec_key       = STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + STRING(TIME)
      vend-whse-trans.upd-date      = TODAY
      vend-whse-trans.upd-time      = TIME
      vend-whse-trans.upd-userid    = USERID("nosweat")
      vend-whse-trans.cust-no       = CAPS(tt-bol.cust-no)         
      vend-whse-trans.fg-item-no    = CAPS(tt-bol.i-no)
      vend-whse-trans.item-line-no  = tt-bol.line
      vend-whse-trans.item-po-no    = tt-bol.po-no
      vend-whse-trans.trans-qty     = tt-bol.qty
      vend-whse-trans.vend-bol-no   = tt-bol.bol-no 
      vend-whse-trans.vend-job-no   = tt-bol.job-no
      vend-whse-trans.vend-job-no2  = tt-bol.job-no2 
      vend-whse-trans.vend-ord-no   = tt-bol.ord-no.

   FIND FIRST b-oe-ordl WHERE b-oe-ordl.company  EQ tt-bol.company
                          AND b-oe-ordl.i-no     EQ tt-bol.i-no
                          AND b-oe-ordl.job-no   EQ tt-bol.job-no
                          AND b-oe-ordl.ord-no   EQ tt-bol.ord-no
                          AND b-oe-ordl.line     EQ tt-bol.LINE NO-LOCK NO-ERROR.
   IF AVAILABLE(b-oe-ordl) THEN
      vend-whse-trans.sell-price = b-oe-ordl.t-price.
   
   FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company = tt-bol.company
                                 AND b-vend-whse-item.fg-item = tt-bol.i-no NO-LOCK NO-ERROR.
   IF AVAILABLE(b-vend-whse-item) THEN DO:
   ASSIGN   
      vend-whse-trans.vendor-code        = CAPS(b-vend-whse-item.vendor-code)
      vend-whse-trans.vendor-dept-code   = CAPS(b-vend-whse-item.vendor-dept-code)
      vend-whse-trans.vendor-plant-code  = CAPS(b-vend-whse-item.vendor-plant-code)
      vend-whse-trans.cust-part-no       = CAPS(b-vend-whse-item.cust-part-no)
      vend-whse-trans.plant-tot-oh-qty   = b-vend-whse-item.plant-tot-oh-qty + vend-whse-trans.trans-qty.
   END.

   op-rowid-list = op-rowid-list + STRING(ROWID(vend-whse-trans)) + ",".
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
  DISPLAY rd-sort 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 BROWSE-1 bt-cancel bt-ok rd-sort 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-rd-sort Dialog-Frame 
PROCEDURE new-rd-sort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* redefined for lookup */
  &scoped-define IAMWHAT LOOKUP   
         
  DO WITH FRAME {&FRAME-NAME}: 
    assign rd-sort.
    case rd-sort:
        {srtord.i 1}
        {srtord2.i 2}
    end. 
  END.

  DO TRANSACTION:
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    APPLY 'entry' TO BROWSE {&browse-name}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-select Dialog-Frame 
PROCEDURE set-select :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-qty AS DECI NO-UNDO.
DEF BUFFER b-tt-bol FOR tt-bol.

DO WITH FRAME {&frame-name}:
   FOR EACH b-tt-bol:
      b-tt-bol.selected-flag = NO.
   END.
    
   IF {&browse-name}:NUM-SELECTED-ROWS GT 0 THEN DO:
      DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
         {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.
         IF AVAIL tt-bol THEN DO: 
            tt-bol.selected-flag = YES.
         END.
      END.
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

