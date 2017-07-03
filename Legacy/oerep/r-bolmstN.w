&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-prodmh.w

  Description: Production Highlights
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}
/*{salrep/dashprod.i NEW}*/


DEF TEMP-TABLE tt-oe-bolh FIELD tt-recid AS RECID
    FIELD bol-no LIKE oe-bolh.ord-no
    FIELD i-count AS INTEGER
    FIELD IS-SELECTED AS LOG COLUMN-LABEL "" VIEW-AS TOGGLE-BOX
    .

assign
 cocode = gcompany
 locode = gloc.

DEFINE VARIABLE list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOG NO-UNDO.
DEFINE VARIABLE lv-prt-bypass     AS LOG NO-UNDO.  /* bypass window's printer driver */
DEFINE  VARIABLE v-program       AS CHARACTER NO-UNDO.
DEF VAR vcBOLNums       AS CHAR NO-UNDO.
DEFINE VARIABLE lvFound AS LOG NO-UNDO.
DEFINE VARIABLE lv-pdf-file     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" NO-UNDO.
DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" NO-UNDO .
DEFINE NEW SHARED VARIABLE v-print-fmt  AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHARACTER NO-UNDO.
def NEW SHARED VAR v-term-id as char no-undo.
DEF VAR counter AS INT NO-UNDO.

{custom/xprint.i}

RUN sys/ref/nk1look.p (cocode, "BOLMaster", "C", NO, NO, "", "", 
                      OUTPUT v-print-fmt, OUTPUT lvFound).

DEFINE VARIABLE retcode AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.

 RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME browse-machine

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-oe-bolh oe-bolh

/* Definitions for BROWSE browse-machine                                */
&Scoped-define FIELDS-IN-QUERY-browse-machine tt-oe-bolh.IS-SELECTED oe-bolh.bol-no oe-bolh.cust-no tt-oe-bolh.i-count oe-bolh.bol-date   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browse-machine tt-oe-bolh.IS-SELECTED   
&Scoped-define ENABLED-TABLES-IN-QUERY-browse-machine tt-oe-bolh
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-browse-machine tt-oe-bolh
&Scoped-define SELF-NAME browse-machine
&Scoped-define QUERY-STRING-browse-machine FOR EACH tt-oe-bolh, ~
       EACH oe-bolh WHERE      recid(oe-bolh) EQ tt-oe-bolh.tt-recid AND      oe-bolh.company = cocode and      oe-bolh.posted EQ NO and      oe-bolh.ship-id = fi_ship-id:SCREEN-VALUE      NO-LOCK BY oe-bolh.bol-no
&Scoped-define OPEN-QUERY-browse-machine OPEN QUERY {&SELF-NAME} FOR EACH tt-oe-bolh, ~
       EACH oe-bolh WHERE      recid(oe-bolh) EQ tt-oe-bolh.tt-recid AND      oe-bolh.company = cocode and      oe-bolh.posted EQ NO and      oe-bolh.ship-id = fi_ship-id:SCREEN-VALUE      NO-LOCK BY oe-bolh.bol-no.
&Scoped-define TABLES-IN-QUERY-browse-machine tt-oe-bolh oe-bolh
&Scoped-define FIRST-TABLE-IN-QUERY-browse-machine tt-oe-bolh
&Scoped-define SECOND-TABLE-IN-QUERY-browse-machine oe-bolh


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-browse-machine}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-9 RECT-10 btn-show fi_ship-id ~
browse-machine rd-dest btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_ship-id rd-dest 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-show 
     LABEL "Show BOLs" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi_ship-id AS CHARACTER FORMAT "X(8)":U 
     LABEL "Create BOL to Ship To" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To Email", 3
     SIZE 20 BY 5.05 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 5.95.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71.6 BY 21.19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browse-machine FOR 
      tt-oe-bolh, 
      oe-bolh SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browse-machine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browse-machine C-Win _FREEFORM
  QUERY browse-machine NO-LOCK DISPLAY
      tt-oe-bolh.IS-SELECTED COLUMN-LABEL ''  VIEW-AS TOGGLE-BOX 
      oe-bolh.bol-no FORMAT ">>>>>>" COLUMN-LABEL "BOL No" width 10
      oe-bolh.cust-no FORMAT "X(8)" COLUMN-LABEL "Customer"
      tt-oe-bolh.i-count COLUMN-LABEL "Item" 
      oe-bolh.bol-date
      ENABLE tt-oe-bolh.IS-SELECTED
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 60.8 BY 8.48 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     btn-show AT ROW 2.67 COL 49.2 WIDGET-ID 2
     fi_ship-id AT ROW 2.81 COL 27 COLON-ALIGNED
     browse-machine AT ROW 4.67 COL 5.2
     rd-dest AT ROW 14.38 COL 11.4 NO-LABEL
     btn-ok AT ROW 20.24 COL 20.6
     btn-cancel AT ROW 20.24 COL 36
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     "Output Options" VIEW-AS TEXT
          SIZE 19 BY .71 AT ROW 13.62 COL 10 WIDGET-ID 6
     RECT-9 AT ROW 1 COL 1
     RECT-10 AT ROW 13.86 COL 6 WIDGET-ID 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72 BY 21.86.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Master BOL Creation"
         HEIGHT             = 21.86
         WIDTH              = 72
         MAX-HEIGHT         = 21.86
         MAX-WIDTH          = 72
         VIRTUAL-HEIGHT     = 21.86
         VIRTUAL-WIDTH      = 72
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB browse-machine fi_ship-id FRAME-A */
ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       fi_ship-id:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browse-machine
/* Query rebuild information for BROWSE browse-machine
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-oe-bolh, EACH oe-bolh WHERE
     recid(oe-bolh) EQ tt-oe-bolh.tt-recid AND
     oe-bolh.company = cocode and
     oe-bolh.posted EQ NO and
     oe-bolh.ship-id = fi_ship-id:SCREEN-VALUE
     NO-LOCK BY oe-bolh.bol-no.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE browse-machine */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Master BOL Creation */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Master BOL Creation */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  i = 0 .
  FOR EACH tt-oe-bolh WHERE tt-oe-bolh.IS-SELECTED:
      i = i + 1.
  END.

  IF i = 0  THEN DO:
   MESSAGE "Please Select Record ..."
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK .
      RETURN .
  END.


  CASE rd-dest:
      WHEN 1 THEN
          LvOutputSelection = "Printer".
      WHEN 2 THEN
          LvOutputSelection = "Screen". 
      WHEN 3 THEN
          LvOutputSelection = "Email".
     
  END CASE.

  RUN SetBolForm (v-print-fmt).

  RUN run-report("",NO).

  RUN GenerateReport("",NO).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-show C-Win
ON CHOOSE OF btn-show IN FRAME FRAME-A /* Show BOLs */
DO:

    DO WITH FRAME {&FRAME-NAME}:

    
         ASSIGN fi_ship-id.
        
         CLOSE QUERY browse-machine.
        RUN build-table. 
         OPEN QUERY browse-machine FOR EACH tt-oe-bolh, EACH oe-bolh WHERE 
              recid(oe-bolh) EQ tt-oe-bolh.tt-recid AND
              oe-bolh.company = cocode  AND
              oe-bolh.posted EQ NO AND 
              oe-bolh.ship-id = fi_ship-id
              NO-LOCK BY oe-bolh.bol-no.

       END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_ship-id C-Win
ON ENTRY OF fi_ship-id IN FRAME FRAME-A /* Create BOL to Ship To */
DO:
  SELF:MODIFIED = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_ship-id C-Win
ON HELP OF fi_ship-id IN FRAME FRAME-A /* Create BOL to Ship To */
DO:
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR rec-val AS RECID NO-UNDO.
   ASSIGN fi_ship-id .
   RUN windows/l-bolhsp.w (g_company,NO,fi_ship-id,OUTPUT char-val,OUTPUT rec-val).
   IF char-val <> "" THEN
      ASSIGN fi_ship-id:SCREEN-VALUE = ENTRY(3,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_ship-id C-Win
ON LEAVE OF fi_ship-id IN FRAME FRAME-A /* Create BOL to Ship To */
DO:
   DO WITH FRAME {&FRAME-NAME}:

      IF SELF:MODIFIED THEN
      DO:
        
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browse-machine
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:


/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.
   
  ASSIGN
    fi_ship-id = cocode.

  RUN enable_UI.
  
  {methods/nowait.i}
  {custom/usrprint.i}

  CLOSE QUERY browse-machine.
   RUN build-table. 
  DO WITH FRAME {&FRAME-NAME}:
         
  OPEN QUERY browse-machine FOR EACH tt-oe-bolh, EACH oe-bolh WHERE
       recid(oe-bolh) EQ tt-oe-bolh.tt-recid AND
       oe-bolh.company = cocode  AND
       oe-bolh.ship-id = fi_ship-id:SCREEN-VALUE AND 
       oe-bolh.posted EQ NO 
       NO-LOCK BY oe-bolh.bol-no.
  END.
 
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table C-Win 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    EMPTY TEMP-TABLE tt-oe-bolh .     
    FOR EACH oe-bolh WHERE
       oe-bolh.company = cocode  AND
       oe-bolh.ship-id = fi_ship-id:SCREEN-VALUE AND 
       oe-bolh.posted EQ NO BREAK BY oe-bolh.bol-no :
     
         FIND FIRST tt-oe-bolh WHERE tt-oe-bolh.tt-recid = RECID(oe-bolh)
              NO-ERROR.
         IF NOT AVAIL tt-oe-bolh THEN DO:
             CREATE tt-oe-bolh.
             ASSIGN tt-oe-bolh.tt-recid   = RECID(oe-bolh)
                    tt-oe-bolh.bol-no     = oe-bolh.bol-no  .
             i =  0.
             FOR EACH oe-boll NO-LOCK
                 where oe-boll.company eq oe-bolh.company
                   and oe-boll.b-no eq oe-bolh.b-no :
                 i = i + 1.
             END.
              tt-oe-bolh.i-count    = i .
         END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-work C-Win 
PROCEDURE build-work :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ic2ndKey  AS CHAR NO-UNDO.

  DEFINE VARIABLE reportKey10 AS LOGICAL NO-UNDO. /* 05291402 */

build-work:
 FOR EACH tt-oe-bolh WHERE tt-oe-bolh.IS-SELECTED:
  FOR EACH oe-bolh
     WHERE oe-bolh.company EQ cocode AND 
       recid(oe-bolh) EQ tt-oe-bolh.tt-recid AND
       oe-bolh.posted  EQ NO
       
       AND CAN-FIND (FIRST oe-boll
                     WHERE oe-boll.company EQ oe-bolh.company
                       AND oe-boll.b-no    EQ oe-bolh.b-no )
      USE-INDEX post:
  
    /*IF NOT oe-ctrl.p-bol THEN*/
    FOR EACH oe-boll
       WHERE oe-boll.company EQ oe-bolh.company
         AND oe-boll.bol-no  EQ oe-bolh.bol-no
         AND CAN-FIND(FIRST oe-ord
                      WHERE oe-ord.company EQ oe-boll.company
                        AND oe-ord.ord-no  EQ oe-boll.ord-no
                        AND oe-ord.stat    EQ "H")
        NO-LOCK:

      /*IF begin_bol# EQ END_bol# THEN
         MESSAGE "Order on BOL is on hold, and BOL will not print."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

      NEXT build-work.
    END.
  
 
    FIND FIRST sys-ctrl-shipto WHERE
         sys-ctrl-shipto.company      EQ oe-bolh.company AND
         sys-ctrl-shipto.name         EQ "BOLMaster" AND
         sys-ctrl-shipto.cust-vend    EQ YES AND
         sys-ctrl-shipto.cust-vend-no EQ oe-bolh.cust-no AND
         sys-ctrl-shipto.ship-id      EQ oe-bolh.ship-id
         NO-LOCK NO-ERROR.
    IF NOT AVAIL sys-ctrl-shipto THEN
        FIND FIRST sys-ctrl-shipto WHERE
         sys-ctrl-shipto.company      EQ oe-bolh.company AND
         sys-ctrl-shipto.name         EQ "BOLMaster" AND
         sys-ctrl-shipto.cust-vend    EQ YES AND
         sys-ctrl-shipto.cust-vend-no EQ oe-bolh.cust-no AND
         sys-ctrl-shipto.ship-id      EQ ''
         NO-LOCK NO-ERROR.
  

    ASSIGN
       reportKey10     = oe-bolh.printed /* 05291402 */
       oe-bolh.printed = YES
       vcBOLNums       = vcBOLNums + '-' + STRING (oe-bolh.bol-no)
       vcBOLNums       = LEFT-TRIM (vcBOLNums, '-').

    IF vcBOLNums MATCHES '*-*' THEN 
       vcBOLNums = RIGHT-TRIM (SUBSTRING (vcBOLNums, 1, INDEX (vcBOLNums,'-')), '-') + SUBSTRING (vcBOLNums, R-INDEX (vcBOLNums, '-')).

    IF NOT CAN-FIND(FIRST report WHERE
       report.term-id = v-term-id AND
       report.rec-id  = RECID(oe-bolh)) THEN
       DO:
        
          CREATE report.
          ASSIGN 
              report.term-id  = v-term-id
              report.key-01   = oe-bolh.cust-no
              report.key-02   = oe-bolh.ship-id
              report.rec-id   = RECID(oe-bolh)
              report.key-09   = STRING(oe-bolh.printed,"REVISED/ORIGINAL")
              report.key-10   = STRING(reportKey10) /* 05291402 */
              report.key-03   = IF AVAIL sys-ctrl-shipto AND  NOT sys-ctrl-shipto.log-fld THEN "C" /*commercial invoice only*/
                                ELSE IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN "B" /*commercial invoice and bol both*/
                                ELSE "N" /*BOL only*/ 
              report.key-04   = IF AVAIL sys-ctrl-shipto THEN sys-ctrl-shipto.char-fld ELSE "".
       END.
       
    status default 'Now Processing BOL: ' + string (oe-bolh.bol-no) + '....'.
    
  END.
 END.
  
 /* v-lines-per-page = 76.*/

  status default ''.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fi_ship-id rd-dest 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-9 RECT-10 btn-show fi_ship-id browse-machine rd-dest btn-ok 
         btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateReport C-Win 
PROCEDURE GenerateReport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.
   DEFINE VARIABLE v-trans-lbl AS CHARACTER NO-UNDO .
   
      CASE rd-dest:
         WHEN 1 THEN RUN output-to-printer(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
         WHEN 2 THEN RUN output-to-screen(INPUT ip-cust-no, INPUT ip-sys-ctrl-shipto).
         WHEN 3 THEN DO:
              IF is-xprint-form THEN DO:
               
                  RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                  {custom/asimail.i &TYPE = "Customer"
                                &begin_cust= v-trans-lbl
                                &END_cust= v-trans-lbl
                                &mail-subject="FRAME {&FRAME-NAME}:TITLE"
                                &mail-body="FRAME {&FRAME-NAME}:TITLE"
                                &mail-file=  lv-pdf-file }
              END.
              ELSE DO:
                  {custom/asimailr.i &TYPE = "Customer"
                                     &begin_cust= v-trans-lbl
                                     &END_cust= v-trans-lbl
                                     &mail-subject="FRAME {&FRAME-NAME}:TITLE"
                                     &mail-body="FRAME {&FRAME-NAME}:TITLE"
                                     &mail-file=list-name }
    
              END.
          END. 
      END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

  IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
/*  ELSE IF lv-prt-bypass THEN
     RUN custom/d-print.w (list-name).*/
  ELSE
     RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-screen C-Win 
PROCEDURE output-to-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

  IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE
     RUN custom/scr-rpt2.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt,lv-prt-bypass).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* --------------------------------------------- oe/rep/oe-lad.p      RM ---- */
/* print bill of ladings                                                      */
/* -------------------------------------------------------------------------- */
  DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-sys-ctrl-ship-to AS LOG NO-UNDO.

  {sys/form/r-top.i}

  {sys/inc/print1.i}
  
  {sys/inc/outprint.i value(76)}

  SESSION:SET-WAIT-STATE ("general").
   
  {sa/sa-sls01.i}
  
  v-term-id = v-term.
  
  run build-work ('').

  ASSIGN lv-pdf-file = init-dir + "\TransBol" + string(TIME) + ".pdf".


  IF IS-xprint-form THEN DO:
  
      CASE rd-dest:
          WHEN 1 THEN PUT "<PRINTER?>".
          WHEN 2 THEN do:
             IF NOT lBussFormModle THEN
               PUT "<PREVIEW><MODAL=NO>". 
             ELSE
               PUT "<PREVIEW>".        
          END.
          WHEN 3 THEN DO:
               PUT "<PDF-OUTPUT=" + lv-pdf-file + ">" FORM "x(180)".
          END.
      END CASE.
  END.

  RUN VALUE(v-program). 
  
   for each report where report.term-id eq v-term-id:
      delete report.
  end.
  
  OUTPUT CLOSE.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetBOLForm C-Win 
PROCEDURE SetBOLForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER icFormName AS CHARACTER NO-UNDO.
   
      CASE icFormName:
         WHEN "ShipTo" OR WHEN "Indiana" THEN
            ASSIGN 
               is-xprint-form = YES
               v-program      = "oe/rep/bolmindc.p".
        
         OTHERWISE
            ASSIGN
               is-xprint-form = YES
               v-program = "oe/rep/bolmindc.p".
     END CASE.
  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

