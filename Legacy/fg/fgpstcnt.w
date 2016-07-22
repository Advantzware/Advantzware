&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fg\fgpstcnt.w

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
  {oe/invwork.i new}
assign
 cocode = gcompany
 locode = gloc.

def new shared var v-trnum as int.

def TEMP-TABLE w-fg-rcpts NO-UNDO like fg-rcpts.
def TEMP-TABLE w-fg-rdtl  NO-UNDO like fg-rdtl.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-post-date tb_gl rd-dest lines-per-page ~
td-show-parm btn-ok btn-cancel RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS v-post-date lbl_gl tb_gl rd-dest ~
lines-per-page td-show-parm 

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

DEFINE VARIABLE lbl_gl AS CHARACTER FORMAT "X(256)":U INITIAL "Create GL Accounts?" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 55 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE v-post-date AS DATE FORMAT "99/99/9999":U 
     LABEL "Posting Date" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 92 BY 5.48.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 9.29.

DEFINE VARIABLE tb_gl AS LOGICAL INITIAL no 
     LABEL "Create GL Accounts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     v-post-date AT ROW 6.48 COL 36 COLON-ALIGNED
     lbl_gl AT ROW 8.38 COL 17 COLON-ALIGNED NO-LABEL
     tb_gl AT ROW 8.38 COL 43
     rd-dest AT ROW 12.43 COL 11 NO-LABEL
     lines-per-page AT ROW 13.14 COL 72 COLON-ALIGNED
     td-show-parm AT ROW 14.81 COL 58
     btn-ok AT ROW 18.38 COL 19
     btn-cancel AT ROW 18.38 COL 57
     RECT-6 AT ROW 11.48 COL 2
     RECT-7 AT ROW 1.48 COL 1
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.48 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "This Procedure Will Post All Finished Goods" VIEW-AS TEXT
          SIZE 50 BY 1.43 AT ROW 3.14 COL 19
          FONT 6
     "Physical Count Transactions." VIEW-AS TEXT
          SIZE 36 BY 1 AT ROW 4.33 COL 29
          FONT 6
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
         TITLE              = "FG Physcial Count Posting Report"
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* SETTINGS FOR FILL-IN lbl_gl IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_gl:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* FG Physcial Count Posting Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* FG Physcial Count Posting Report */
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
  assign rd-dest.
       
  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case. 

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


&Scoped-define SELF-NAME tb_gl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_gl C-Win
ON VALUE-CHANGED OF tb_gl IN FRAME FRAME-A /* Create GL Accounts? */
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


&Scoped-define SELF-NAME v-post-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-post-date C-Win
ON LEAVE OF v-post-date IN FRAME FRAME-A /* Posting Date */
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  ASSIGN
   v-post-date = TODAY.

  RUN enable_UI.
  
  {methods/nowait.i}
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
  DISPLAY v-post-date lbl_gl tb_gl rd-dest lines-per-page td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE v-post-date tb_gl rd-dest lines-per-page td-show-parm btn-ok 
         btn-cancel RECT-6 RECT-7 
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
/* --------------------------------------------------- fg/fg-cpost.p 10/94 rd */
/* Finished Goods - Cycle Count POST                                          */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def buffer b-fg-rcpts   for fg-rcpts.
def buffer b-fg-rdtl    for fg-rdtl.
def buffer b-itemfg     for itemfg.
def buffer b-fg-bin     for fg-bin.

def var v-post-date     as   date init today.
def var v-gl            as   log init no.

def var save_id         as   recid.
def var v-qty-onh       as   dec.
def var v-temp-cost     as   dec format "->>>>>9.99".
def var time_stamp      as   char.
def var v-cum-qty       as   dec format "->>>>>>9".
def var v-tot-value     as   dec format "->>>,>>>,>>9.99".
def var v-item-tot      as   dec format "->>>,>>>,>>9.99".
def var v-std-cost      as   dec format "->>>,>>9.99<<".
def var v-q-adj-ytd     as   int.
def var v-adj-qty       as   int.
def var v-dscr          like account.dscr.
def var v-disp-actnum   like account.actnum.
def var v-disp-amt      as   dec format ">>,>>>,>>9.99cr".
def var v-cost          like itemfg.std-tot-cost extent 4.
def var v-uom           like itemfg.prod-uom.
def var v-part-qty      as dec.


form fg-rcpts.trans-date     column-label "TRANS.!DATE"
     fg-rcpts.i-no           label "ITEM"
     fg-rcpts.i-name         format "x(20)" label "DESCRIPTION"
     fg-rcpts.job-no         label "   JOB" space(0) "-" space(0)
     fg-rcpts.job-no2        label "# " format "99"
     fg-rdtl.loc             label "WHSE"
     fg-rdtl.loc-bin         label "BIN"
     fg-rdtl.tag             label "TAG"
     fg-rdtl.t-qty           format "->>>>>>9" label "QUANTITY"
     fg-rcpts.pur-uom        label "UOM"
     fg-rdtl.std-cost        label "COST/UOM"
     v-tot-value             label "TOTAL COST"
     skip

    with frame itemx no-box down STREAM-IO width 132.

form v-disp-actnum label "G/L ACCOUNT NUMBER"
     v-dscr        label "DESCRIPTION"
     udate         label "DATE"   
     v-disp-amt    label "AMOUNT" skip

    with down STREAM-IO width 130 frame gldetail.

udate = v-post-date.

if v-gl then do:
  view frame dreg.
  pause 0.

  /*{sys/inc/period.i udate}*/
  
  hide frame dreg no-pause.
end.

ASSIGN
 v-post-date = v-post-date
 v-gl        = tb_gl.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display "" with frame r-top.

  for each fg-rcpts
      where fg-rcpts.company   eq cocode
        and fg-rcpts.rita-code eq "C"
      use-index i-no no-lock,

      first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq fg-rcpts.i-no
      no-lock,

      each fg-rdtl
      where fg-rdtl.r-no eq fg-rcpts.r-no
      no-lock

      break by fg-rcpts.i-no:

    if first-of(fg-rcpts.i-no) then do:
      put skip(1).
      v-cum-qty = 0.
    end.

    if itemfg.prod-uom eq "EA" then
      v-std-cost = fg-rdtl.std-cost.
    else
      run sys/ref/convcuom.p(itemfg.prod-uom, "EA", 0, 0, 0, 0,
                             fg-rdtl.std-cost, output v-std-cost).

    assign
     v-tot-value = v-std-cost * fg-rdtl.t-qty
     v-cum-qty   = v-cum-qty + fg-rdtl.t-qty.

    display fg-rcpts.trans-date when first-of(fg-rcpts.i-no)
            fg-rcpts.i-no       when first-of(fg-rcpts.i-no)
            fg-rcpts.i-name
            fg-rdtl.tag
            fg-rdtl.t-qty
            fg-rcpts.pur-uom
            fg-rdtl.loc
            fg-rdtl.loc-bin
            fg-rdtl.std-cost
            v-tot-value
            fg-rcpts.job-no
            fg-rcpts.job-no2
        with frame itemx.
    down with frame itemx.

    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq fg-rcpts.i-no
          and fg-bin.loc     eq fg-rdtl.loc
          and fg-bin.loc-bin eq fg-rdtl.loc-bin
          and fg-bin.tag     eq fg-rdtl.tag
          and fg-bin.job-no  eq fg-rcpts.job-no
          and fg-bin.job-no2 eq fg-rcpts.job-no2
        no-lock no-error.
    if not avail fg-bin and fg-rdtl.tag ne "" then
    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq fg-rcpts.i-no
          and fg-bin.loc     eq fg-rdtl.loc
          and fg-bin.loc-bin eq fg-rdtl.loc-bin
          and fg-bin.tag     eq ""
          and fg-bin.job-no  eq fg-rcpts.job-no
          and fg-bin.job-no2 eq fg-rcpts.job-no2
        no-lock no-error.
        
    if avail fg-bin then
      assign
       v-cost[1] = fg-bin.std-lab-cost
       v-cost[2] = fg-bin.std-fix-cost
       v-cost[3] = fg-bin.std-var-cost
       v-cost[4] = fg-bin.std-mat-cost
       v-uom     = fg-bin.pur-uom.
    else   
      assign
       v-cost[1] = itemfg.std-lab-cost
       v-cost[2] = itemfg.std-fix-cost
       v-cost[3] = itemfg.std-var-cost
       v-cost[4] = itemfg.std-mat-cost
       v-uom     = itemfg.prod-uom.
 
    v-adj-qty = (if avail fg-bin then fg-bin.qty else 0) - fg-rdtl.t-qty.

    /*Invoicing  - Post Invoicing Transactions - Job Costing*/
    run oe/invposty.p (0, itemfg.i-no, v-adj-qty, v-uom,
                       v-cost[1], v-cost[2], v-cost[3], v-cost[4]).

    v-item-tot = v-item-tot + v-tot-value.

    if last-of(fg-rcpts.i-no) then do:
      put "--------" to 88 "---------------" to 121 skip
          "Item Total"                       to 75
          v-cum-qty                          to 88
          v-item-tot                         to 121 skip.

      v-item-tot = 0.
    end.
  end. /* each fg-rcpts */

  if v-gl then
  for each work-job break by work-job.actnum:
  
    find first account
        where account.company eq cocode
          and account.actnum  eq work-job.actnum
        no-lock no-error.
        
    assign
     v-dscr        = if avail account then account.dscr
                     else "ACCOUNT NOT FOUND - " + work-job.actnum
     v-disp-actnum = work-job.actnum.

    if work-job.fg then
      v-disp-amt = - work-job.amt.
    else
      v-disp-amt = work-job.amt.

    display v-disp-actnum v-dscr udate v-disp-amt
        with frame gldetail.
    down with frame gldetail.
  end. /* each work-job */
postit:
  do transaction on error undo postit, leave postit:
  
    /*{sys/sho/s-top.v}
    {sys/msg/post.i post}*/

    for each fg-rcpts                              /* Unassembled Sets */
        where fg-rcpts.company   eq cocode
          and fg-rcpts.rita-code eq "C"
        no-lock,  

        first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq fg-rcpts.i-no
          and itemfg.isaset
          and itemfg.alloc
        no-lock,  

        each fg-set
        where fg-set.company eq cocode
          and fg-set.set-no  eq itemfg.i-no
        no-lock,

        first b-itemfg
        where b-itemfg.company eq cocode
          and b-itemfg.i-no    eq fg-set.part-no
        no-lock:

      {sys/inc/part-qty.i v-part-qty fg-set}

      x = 1.
      for each w-fg-rcpts by w-fg-rcpts.r-no desc:
        leave.
      end.
      if avail w-fg-rcpts then
        x = w-fg-rcpts.r-no + 1.

      find last b-fg-rcpts use-index r-no no-lock no-error.
      if avail b-fg-rcpts and b-fg-rcpts.r-no ge x then
        x = b-fg-rcpts.r-no + 1.

      find last fg-rcpth use-index r-no no-lock no-error.
      if avail fg-rcpth and fg-rcpth.r-no ge x then
        x = fg-rcpth.r-no + 1.

      create w-fg-rcpts.
      buffer-copy fg-rcpts to w-fg-rcpts
      assign
       w-fg-rcpts.i-no   = b-itemfg.i-no
       w-fg-rcpts.i-name = b-itemfg.i-name
       w-fg-rcpts.r-no   = x.
         
      for each fg-rdtl where fg-rdtl.r-no eq fg-rcpts.r-no:
        find first fg-bin
            where fg-bin.company eq cocode
              and fg-bin.i-no    eq itemfg.i-no
              and fg-bin.loc     eq fg-rdtl.loc
              and fg-bin.loc-bin eq fg-rdtl.loc-bin
              and fg-bin.tag     eq fg-rdtl.tag
              and fg-bin.job-no  eq fg-rcpts.job-no
              and fg-bin.job-no2 eq fg-rcpts.job-no2
            use-index co-ino no-error.
        v-adj-qty = (if avail fg-bin then fg-bin.qty else 0) * v-part-qty.
        
        find first fg-bin
            where fg-bin.company eq cocode
              and fg-bin.i-no    eq b-itemfg.i-no
              and fg-bin.loc     eq fg-rdtl.loc
              and fg-bin.loc-bin eq fg-rdtl.loc-bin
              and fg-bin.tag     eq fg-rdtl.tag
              and fg-bin.job-no  eq fg-rcpts.job-no
              and fg-bin.job-no2 eq fg-rcpts.job-no2
            use-index co-ino no-error.
        v-adj-qty = (if avail fg-bin then fg-bin.qty else 0) - v-adj-qty.
        
        if v-adj-qty lt 0 then v-adj-qty = 0.

        create w-fg-rdtl.
        buffer-copy fg-rdtl to w-fg-rdtl
        assign
         w-fg-rdtl.r-no     = w-fg-rcpts.r-no
         w-fg-rdtl.t-qty    = (fg-rdtl.t-qty * v-part-qty) + v-adj-qty
         w-fg-rdtl.std-cost = 0
         w-fg-rdtl.ext-cost = 0.
      end.
    end.
    
    /*{fg/fg-cpost.i w-}

    {fg/fg-cpost.i}*/

    if v-gl then do:
      /** GET next G/L TRANS. POSTING # **/
      /* gdm - 11050906 */
      REPEAT:
        FIND FIRST gl-ctrl EXCLUSIVE-LOCK
          WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
        IF AVAIL gl-ctrl THEN DO:
          ASSIGN v-trnum       = gl-ctrl.trnum + 1
                 gl-ctrl.trnum = v-trnum.
          FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.
          LEAVE.
        END. /* IF AVAIL gl-ctrl */
      END. /* REPEAT */
      /* gdm - 11050906 */

      for each work-job break by work-job.actnum:
        create gltrans.
        assign
         gltrans.company = cocode
         gltrans.actnum  = work-job.actnum
         gltrans.jrnl    = "OEINV"
         gltrans.tr-date = udate
         gltrans.period  = uperiod
         gltrans.trnum   = v-trnum.
    
        if work-job.fg then
          assign
           gltrans.tr-amt  = - work-job.amt
           gltrans.tr-dscr = "ORDER ENTRY INVOICE FG".
        else
          assign
           gltrans.tr-amt  = work-job.amt
           gltrans.tr-dscr = "ORDER ENTRY INVOICE COGS".
      end. /* each work-job */
    end.
  end. /* postit */

  /*leave inners.*/

hide all no-pause.

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

