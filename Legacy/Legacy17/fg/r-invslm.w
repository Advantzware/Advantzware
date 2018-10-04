&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fg\r-invslm.w

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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_cust end_cust begin_po-no end_po-no ~
begin_slm end_slm tb_inc-zer tb_inc-cust rd_sort rd-dest lines-per-page ~
td-show-parm btn-ok btn-cancel RECT-6 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_po-no end_po-no ~
begin_slm end_slm lbl_inc-zer tb_inc-zer lbl_inc-cust tb_inc-cust lbl_sort ~
rd_sort rd-dest lines-per-page td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_po-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Customer PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slm AS CHARACTER FORMAT "XXX":U 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_po-no AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Customer PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slm AS CHARACTER FORMAT "XXX":U INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_inc-cust AS CHARACTER FORMAT "X(256)":U INITIAL "Include Customer Owned Warehouse?" 
     VIEW-AS FILL-IN 
     SIZE 38 BY .91 NO-UNDO.

DEFINE VARIABLE lbl_inc-zer AS CHARACTER FORMAT "X(256)":U INITIAL "Include Zero Quantity On Hand?" 
     VIEW-AS FILL-IN 
     SIZE 32 BY .91 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort by SalesRep By?" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .91 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 55 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer#", "Customer#",
"Item", "Item"
     SIZE 29 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 92 BY 5.

DEFINE VARIABLE tb_inc-cust AS LOGICAL INITIAL no 
     LABEL "Include Customer Owned Warehouse?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .91 NO-UNDO.

DEFINE VARIABLE tb_inc-zer AS LOGICAL INITIAL no 
     LABEL "Include Zero Quantity On Hand?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .91 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust AT ROW 4.1 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 4.1 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_po-no AT ROW 5.05 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer PO Number"
     end_po-no AT ROW 5.05 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer PO Number"
     begin_slm AT ROW 6 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slm AT ROW 6 COL 70 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     lbl_inc-zer AT ROW 7.67 COL 20 NO-LABEL
     tb_inc-zer AT ROW 7.67 COL 52
     lbl_inc-cust AT ROW 8.62 COL 12 COLON-ALIGNED NO-LABEL
     tb_inc-cust AT ROW 8.62 COL 54 RIGHT-ALIGNED
     lbl_sort AT ROW 9.57 COL 28 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 9.57 COL 52 NO-LABEL
     rd-dest AT ROW 12.67 COL 12 NO-LABEL
     lines-per-page AT ROW 13.38 COL 73 COLON-ALIGNED
     td-show-parm AT ROW 15.05 COL 59
     btn-ok AT ROW 17.91 COL 19
     btn-cancel AT ROW 17.91 COL 57
     RECT-6 AT ROW 11.71 COL 3
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.71 COL 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 21.57.


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
         TITLE              = "Finished Goods Inventory By Sales Rep"
         HEIGHT             = 21.81
         WIDTH              = 95.8
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
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

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_po-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_po-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_inc-cust IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_inc-cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "tb_cust-whse".

/* SETTINGS FOR FILL-IN lbl_inc-zer IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_inc-cust IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_inc-cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_inc-zer:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Finished Goods Inventory By SalesRep */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Finished Goods Inventory By SalesRep */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-no C-Win
ON LEAVE OF begin_po-no IN FRAME FRAME-A /* Beginning Customer PO# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slm C-Win
ON LEAVE OF begin_slm IN FRAME FRAME-A /* Beginning SalesRep# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   apply "close" to this-procedure.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  assign rd-dest.

  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case. 

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-no C-Win
ON LEAVE OF end_po-no IN FRAME FRAME-A /* Ending Customer PO# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slm C-Win
ON LEAVE OF end_slm IN FRAME FRAME-A /* Ending SalesRep# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inc-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inc-cust C-Win
ON VALUE-CHANGED OF tb_inc-cust IN FRAME FRAME-A /* Include Customer Owned Warehouse? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inc-zer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inc-zer C-Win
ON VALUE-CHANGED OF tb_inc-zer IN FRAME FRAME-A /* Include Zero Quantity On Hand? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:


  RUN enable_UI.

  {methods/nowait.i}
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images1.p */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY begin_cust end_cust begin_po-no end_po-no begin_slm end_slm 
          lbl_inc-zer tb_inc-zer lbl_inc-cust tb_inc-cust lbl_sort rd_sort 
          rd-dest lines-per-page td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_cust end_cust begin_po-no end_po-no begin_slm end_slm tb_inc-zer 
         tb_inc-cust rd_sort rd-dest lines-per-page td-show-parm btn-ok 
         btn-cancel RECT-6 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file C-Win 
PROCEDURE output-to-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE list-name
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
    /*     CREATE-TEST-FILE*/
         SAVE-AS
         USE-FILENAME

         UPDATE OKpressed.

     IF NOT OKpressed THEN  RETURN NO-APPLY.


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
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */

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
  run scr-rpt.w (list-name,c-win:title). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ fg/rep/fg-ssman.p 3/94 RM */
/*  finished goods inventory by salesrep report                               */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var v-ext as dec format "->>,>>>,>>9.99".
def var fcust as ch init " ".
def var tcust like fcust init "ZZZZZZZZZ".
def var fcustpo as ch init " ".
def var tcustpo like fcustpo init "ZZZZZZZZZ".
def var fsman like cust.sman init " ".
def var tsman like cust.sman init "ZZZ".
def var zbal as log format "Y/N".
def var v-sub-ext like v-ext.
def var v-cust-ext like v-ext.
def var tot-string as char format "x(5)" init "Total".
def var v-cost like itemfg.total-std-cost.
def var v-qty-onh as int format "->>>,>>>,>>9".
def var v-totcost as dec format "->>>,>>9.99".
def var v-frst as log.
def var v-sel-one as char format "x(30)".
def var v-sel-two as char format "x(33)".
def var v-sel-three as char format "x(20)".
def var v-custown as log format "Y/N" init "N".
def var v-cust-qty as int format "->>>,>>>,>>9".

def var sort-opt as char no-undo init "C" format "!".

format
  itemfg.i-no label "ITEM #"
  itemfg.i-name format "x(20)" label "DESCRIPTION"
  itemfg.cust-no label "CUST #"
  itemfg.cust-po-no column-label "CUSTOMER!  PO #"
  itemfg.cust-job-no column-label "CUSTOMER!  JOB #"
  itemfg.q-prod-ptd column-label "QUANTITY!PRODUCED" format "->>,>>>,>>9"
  itemfg.q-ship-ptd column-label "QUANTITY!SHIPPED" format "->>,>>>,>>9"
  v-qty-onh column-label "QUANTITY! ON HAND" format "->>>,>>>,>>9"
  v-totcost column-label "TOTAL!COST" format "->>,>>>,>>9.99"
  v-ext column-label "TOTAL!VALUE"  format "->>,>>>,>>9.99"
  with frame itemx no-box down STREAM-IO width 132.

find first fg-ctrl where fg-ctrl.company eq cocode.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 fcust        = begin_cust
 tcust        = end_cust
 fcustpo      = begin_po-no
 tcustpo      = END_po-no 
 fsman        = begin_slm
 tsman        = end_slm
 zbal         = tb_inc-zer
 v-custown    = tb_inc-cust
 sort-opt     = SUBSTR(rd_sort,1,1).

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

DISPLAY "" WITH FRAME r-top.

  for each itemfg
        no-lock
        use-index customer
        where itemfg.company eq cocode 
      AND itemfg.cust-no ge fcust 
      and itemfg.cust-no le tcust 
      AND itemfg.cust-po-no ge fcustpo 
      and itemfg.cust-po-no le tcustpo,
        each cust
        no-lock
        where cust.company eq itemfg.company
          and cust.sman ge fsman  and cust.sman le tsman and
        cust.cust-no
        eq itemfg.cust-no
        break
        by cust.sman
        by (if sort-opt eq "C" then itemfg.cust-no else itemfg.i-no)
        by (if sort-opt eq "C" then itemfg.i-no    else itemfg.cust-no):

      if first-of(cust.sman) then
      assign v-frst = yes.

      assign v-totcost = 0
        v-ext = 0
        v-cost = 0
        v-qty-onh = 0
        v-cust-qty = 0.

      for each fg-bin where fg-bin.company eq cocode and
          fg-bin.i-no eq itemfg.i-no no-lock use-index i-no
          break by fg-bin.i-no:
        if trim(fg-bin.cust-no) eq "" then
        assign v-qty-onh = v-qty-onh + fg-bin.qty.
        else
        assign v-cust-qty = v-cust-qty + fg-bin.qty.

        if v-custown or
          (not v-custown and fg-bin.loc ne "CUST" and trim(fg-bin.cust-no) eq "") then
        do:

          if caps(fg-ctrl.inv-meth) eq "L" then
          v-cost = itemfg.last-cost.
          else
          assign v-cost = fg-bin.std-tot-cost.

          /* Calculate Cost */
          if itemfg.pur-uom eq "CS" and itemfg.case-count ne 0 then
          assign v-totcost = v-totcost + (fg-bin.qty * v-cost)
            / itemfg.case-count.
          else
          find first uom where uom.uom eq itemfg.sell-uom and
            uom.mult ne 0 no-lock no-error.
          if avail uom then
          assign v-totcost = v-totcost + (fg-bin.qty * v-cost / uom.mult).
          else
          assign v-totcost = v-totcost + (fg-bin.qty * v-cost) / 1000.

          if itemfg.pur-uom eq "L" then
          assign v-totcost = v-cost.
        end.

        if last-of(fg-bin.i-no) then
        do:
          /** include customer owned inventory in qty on hand
          if v-custown = true. **/
          if v-custown then
          assign v-qty-onh = v-qty-onh + v-cust-qty.
          /* Calculate Selling Price */
          if itemfg.sell-uom eq "CS" and itemfg.case-count ne 0 then
          assign v-ext = (v-qty-onh * itemfg.sell-price)
            / itemfg.case-count.
          else
          find first uom where uom.uom eq itemfg.sell-uom and
            uom.mult ne 0 no-lock no-error.
          if avail uom then
          assign v-ext = (v-qty-onh * itemfg.sell-price / uom.mult).
          else
          assign v-ext = (v-qty-onh * itemfg.sell-price) / 1000.

          if itemfg.sell-uom eq "L" then
          assign v-ext = itemfg.sell-price.
          /**
          if v-ext < 0 then assign v-ext = 0.
          if v-totcost < 0 then assign v-totcost = 0.
          **/
          if (itemfg.q-onh ne 0 ) or (itemfg.q-onh eq 0 and zbal)  then
          do:
            if line-counter ge 56 then
            page.
            if v-frst then
            do:
              find first sman where sman.company eq cust.company and
                sman.sman eq cust.sman no-lock no-error.
              if avail sman then
              put "SALES REP: " cust.sman SPACE(2) sman.sname skip(1).
              else
              put "SALES REP: " cust.sman SPACE(2) "UNKNOWN" skip(1).
              assign v-frst = NO.
            end.
            display
              itemfg.i-no itemfg.i-name itemfg.cust-po-no itemfg.cust-job-no
              itemfg.q-prod-ptd itemfg.q-ship-ptd v-qty-onh itemfg.cust-no
              v-totcost v-ext
              with frame itemx.
            down with frame itemx.
            assign v-sub-ext = v-sub-ext + v-ext
              v-cust-ext = v-cust-ext + v-ext /* 9508 CAH */
              v-totcost = 0
              v-ext = 0
              v-qty-onh = 0.
          end.
        end. /* last-of fg-bin.i-no */
      end. /* each bin */
      if last-of
        (if sort-opt eq "C" then itemfg.cust-no else itemfg.i-no)
        and sort-opt eq "C"  and v-cust-ext ne 0
        then
      do: /* 9508 CAH */
        put "--------------" TO 132 skip
          "CUSTOMER TOTAL" TO 116 v-cust-ext TO 132 skip(1).
        assign v-cust-ext = 0.
      end.

      if last-of(cust.sman) and v-sub-ext ne 0 then
      do:
        put "--------------" TO 132 skip
          "SALES REP TOTAL" TO 116 v-sub-ext TO 132 skip(1).
        assign v-sub-ext = 0.
      end.
    end. /* for each itemfg and each cust */

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE show-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-frame-hdl as handle no-undo.
  def var lv-group-hdl as handle no-undo.
  def var lv-field-hdl as handle no-undo.
  def var lv-field2-hdl as handle no-undo.
  def var parm-fld-list as cha no-undo.
  def var parm-lbl-list as cha no-undo.
  def var i as int no-undo.
  def var lv-label as cha.

  lv-frame-hdl = frame {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:first-child.
  lv-field-hdl = lv-group-hdl:first-child .

  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," 
                     .
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     .
              lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name then do:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".
                  end.
                  lv-field2-hdl = lv-field2-hdl:next-sibling.                 
              end.       
           end.                 
        end.            
     lv-field-hdl = lv-field-hdl:next-sibling.   
  end.

  put space(28)
      "< Selection Parameters >"
      skip(1).

  do i = 1 to num-entries(parm-fld-list,","):
    if entry(i,parm-fld-list) ne "" or
       entry(i,parm-lbl-list) ne "" then do:

      lv-label = fill(" ",34 - length(trim(entry(i,parm-lbl-list)))) +
                 trim(entry(i,parm-lbl-list)) + ":".

      put lv-label format "x(35)" at 5
          space(1)
          trim(entry(i,parm-fld-list)) format "x(40)"
          skip.              
    end.
  end.

  put fill("-",80) format "x(80)" skip.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

