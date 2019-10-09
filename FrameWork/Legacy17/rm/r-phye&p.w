&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-rmte&p.w

  Description: RM Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 05/23/2002

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
&IF DEFINED(UIB_IS_RUNNING) NE 0 &THEN
DEF VAR ip-post AS LOG INIT NO NO-UNDO.
&ELSE
DEF INPUT PARAMETER ip-post AS LOG NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */
DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEF VAR ll-valid AS LOG NO-UNDO.

def new shared var pox like po-ordl.po-no.

def var v-types as char format "x(10)" no-undo.
def var v-autoissue as log.
def var v-dunne as log init no.

def TEMP-TABLE w-rec-h NO-UNDO
    field rec-id    as   recid
    field rita-code like rm-rcpt.rita-code.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "AUTOISSU"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "AUTOISSU"
   sys-ctrl.descrip = "Automatically Issue RM Receipts to Jobs?".
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
v-autoissue = sys-ctrl.log-fld.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-F

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-from-job v-to-job v-post-date t-receipt ~
t-issue t-trans t-adj rd-dest lines-per-page td-show-parm Btn_OK Btn_Cancel ~
RECT-18 RECT-6 
&Scoped-Define DISPLAYED-OBJECTS v-from-job v-to-job v-post-date t-receipt ~
t-issue t-trans t-adj rd-dest lines-per-page td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 55 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 2 BY 1 NO-UNDO.

DEFINE VARIABLE v-from-job AS CHARACTER FORMAT "X(256)":U 
     LABEL "From Job#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE v-post-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Posting Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE v-to-job AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzz" 
     LABEL "To Job#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY 10.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY 5.24.

DEFINE VARIABLE t-adj AS LOGICAL INITIAL no 
     LABEL "Adjustments" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE t-issue AS LOGICAL INITIAL no 
     LABEL "Issues" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.

DEFINE VARIABLE t-receipt AS LOGICAL INITIAL no 
     LABEL "Receipts" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .86 NO-UNDO.

DEFINE VARIABLE t-trans AS LOGICAL INITIAL no 
     LABEL "Transfers" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-F
     v-from-job AT ROW 3.38 COL 17 COLON-ALIGNED
     v-to-job AT ROW 3.38 COL 49 COLON-ALIGNED
     v-post-date AT ROW 4.57 COL 17 COLON-ALIGNED
     tran-period AT ROW 5.05 COL 73 COLON-ALIGNED
     t-receipt AT ROW 6.71 COL 31
     t-issue AT ROW 7.67 COL 31
     t-trans AT ROW 8.62 COL 31
     t-adj AT ROW 9.57 COL 31
     rd-dest AT ROW 12.43 COL 12 NO-LABEL
     lines-per-page AT ROW 12.91 COL 65 COLON-ALIGNED
     td-show-parm AT ROW 14.57 COL 50
     Btn_OK AT ROW 17.91 COL 19
     Btn_Cancel AT ROW 17.91 COL 50
     RECT-18 AT ROW 1.24 COL 1
     RECT-6 AT ROW 11.48 COL 1
     "Transaction Types" VIEW-AS TEXT
          SIZE 21 BY .86 AT ROW 6.71 COL 10
     "Output Destination" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 11.71 COL 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 22 BY .95 AT ROW 1.48 COL 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.2 BY 19.24.


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
         TITLE              = "Raw Material Post"
         HEIGHT             = 19.24
         WIDTH              = 80.6
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
/* SETTINGS FOR FRAME FRAME-F
                                                                        */
ASSIGN 
       t-adj:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

ASSIGN 
       t-issue:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

ASSIGN 
       t-receipt:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

ASSIGN 
       t-trans:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-F
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tran-period:HIDDEN IN FRAME FRAME-F           = TRUE.

ASSIGN 
       v-from-job:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

ASSIGN 
       v-post-date:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

ASSIGN 
       v-to-job:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Raw Material Post */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Raw Material Post */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME FRAME-F /* Cancel */
DO:
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME FRAME-F /* OK */
DO: 
  DEF VAR lv-post AS LOG NO-UNDO.

  /*run check-date.
  if not ll-valid then return no-apply.*/

  assign
   rd-dest
   v-from-job = fill(" ",6 - length(trim(input v-from-job))) +
                trim(input v-from-job)
   v-to-job   = fill(" ",6 - length(trim(input v-to-job))) +
                trim(input v-to-job)
   v-types    = (IF t-receipt THEN "R" ELSE "") +
                (IF t-issue   THEN "I" ELSE "") +
                (IF t-trans   THEN "T" ELSE "") +
                (IF t-adj     THEN "A" ELSE "").

  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case.

  lv-post = NO.

  IF ip-post THEN DO:
    MESSAGE "Post RM Transactions?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-post.

    IF lv-post THEN do:
      RUN post-rm.

      lv-post = v-dunne.

      IF lv-post THEN MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.

      ELSE MESSAGE "Posting Incomplete..." VIEW-AS ALERT-BOX ERROR.
    END.
  END. 

  if (not lv-post) or index(v-types,"R") eq 0 then
  for each w-rec-h  where w-rec-h.rita-code eq "R",

      first rm-rctd where recid(rm-rctd) eq w-rec-h.rec-id:

    delete rm-rctd.
  end.

  if (not lv-post) or index(v-types,"I") eq 0 then
  for each w-rec-h  where w-rec-h.rita-code eq "I",

      first rm-rctd where recid(rm-rctd) eq w-rec-h.rec-id:

    delete rm-rctd.
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-F /* Lines Per Page */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-F
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-adj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-adj C-Win
ON VALUE-CHANGED OF t-adj IN FRAME FRAME-F /* Adjustments */
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-issue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-issue C-Win
ON VALUE-CHANGED OF t-issue IN FRAME FRAME-F /* Issues */
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-receipt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-receipt C-Win
ON VALUE-CHANGED OF t-receipt IN FRAME FRAME-F /* Receipts */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-trans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-trans C-Win
ON VALUE-CHANGED OF t-trans IN FRAME FRAME-F /* Transfers */
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-F /* Show Parameters? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-from-job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-from-job C-Win
ON LEAVE OF v-from-job IN FRAME FRAME-F /* From Job# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-post-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-post-date C-Win
ON LEAVE OF v-post-date IN FRAME FRAME-F /* Posting Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-to-job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-to-job C-Win
ON LEAVE OF v-to-job IN FRAME FRAME-F /* To Job# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */    
DEF VAR choice AS LOG NO-UNDO.

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

  for each rm-rctd
      where rm-rctd.company   eq cocode
        and rm-rctd.rita-code ne "C"
      break by rm-rctd.rita-code:
    if first-of(rm-rctd.rita-code) then v-types = v-types + rm-rctd.rita-code.
  end.

  if v-autoissue             and
     index(v-types,"R") gt 0 and
     index(v-types,"I") eq 0 then v-types = trim(v-types) + "I".

  assign
   v-post-date = TODAY
   t-receipt   = index(v-types,"R") gt 0
   t-issue     = index(v-types,"I") gt 0
   t-trans     = index(v-types,"T") gt 0
   t-adj       = index(v-types,"A") gt 0
   v-types     = ""
   c-win:TITLE = IF ip-post THEN "Raw Material Post"
                            ELSE "Raw Material Edit List".

  RUN enable_UI.

  /*RUN check-date.*/

  IF NOT ip-post THEN DISABLE v-post-date WITH FRAME {&FRAME-NAME}.

  {methods/nowait.i}

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date C-Win 
PROCEDURE check-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO with frame {&frame-name}:
    ll-valid = YES.

    find first period                   
        where period.company eq cocode
          and period.pst     le v-post-date
          and period.pend    ge v-post-date
        no-lock no-error.
    if avail period then tran-period:SCREEN-VALUE = string(period.pnum).

    else
    IF ip-post THEN DO:
      message "No Defined Period Exists for" v-post-date view-as alert-box error.
      ll-valid = NO.
    end.
  END.
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
  DISPLAY v-from-job v-to-job v-post-date t-receipt t-issue t-trans t-adj 
          rd-dest lines-per-page td-show-parm 
      WITH FRAME FRAME-F IN WINDOW C-Win.
  ENABLE v-from-job v-to-job v-post-date t-receipt t-issue t-trans t-adj 
         rd-dest lines-per-page td-show-parm Btn_OK Btn_Cancel RECT-18 RECT-6 
      WITH FRAME FRAME-F IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-F}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-rm C-Win 
PROCEDURE post-rm :
/* --------------------------------------------------- rm/rm-post.p 10/94 rd  */
/* raw materials inventory control receipt maintenance                        */
/* -------------------------------------------------------------------------- */

def buffer xrm-rctd     for rm-rctd.
def buffer xrm-bin      for rm-bin.
def buffer b-rm-rctd    for rm-rctd.
def buffer b-item       for item.
def buffer b-po-ordl    for po-ordl.
def buffer b-job-mat    for job-mat.

def var v-avg-cst   as log.
def var v-next_r-no like rm-rctd.r-no.
def var v_r-no like rm-rctd.r-no.
def var v-conv-qty as dec.
def var v-overrun-qty like po-ordl.ord-qty.
def var v-underrun-qty like po-ordl.ord-qty.
def var v-reduce-qty like po-ordl.ord-qty.
def var no-of-items as int no-undo.
def var ld-cvt-qty as dec no-undo.

def var v-r-qty     as   dec                    no-undo.
def var v-i-qty     as   dec                    no-undo.
def var v-t-qty     as   dec                    no-undo.
def var cost        as   dec                    no-undo.
def var out-qty     as   dec                    no-undo.
def var v-bwt       like item.basis-w           no-undo.
def var v-len       like item.s-len             no-undo.
def var v-wid       like item.s-wid             no-undo.
def var v-dep       like item.s-dep             no-undo.
def var v-recid     as   recid                  no-undo.

find first rm-ctrl where rm-ctrl.company eq cocode no-lock no-error.
v-avg-cst = rm-ctrl.avg-lst-cst.

    transblok:
    for each b-rm-rctd
        where b-rm-rctd.company                  eq cocode
          and index(v-types,b-rm-rctd.rita-code) gt 0
          and b-rm-rctd.rita-code                ne "C"
          and b-rm-rctd.rita-code                ne "ADDER"
          and b-rm-rctd.job-no                   ge v-from-job
          and b-rm-rctd.job-no                   le v-to-job
          and can-find(first item where item.company eq cocode
                                    and item.i-no    eq b-rm-rctd.i-no)
        no-lock

        break by b-rm-rctd.i-no
              by b-rm-rctd.r-no

        transaction:

      find rm-rctd where recid(rm-rctd) eq recid(b-rm-rctd) exclusive no-wait.
      if locked rm-rctd then next transblok.

      find first item
          where item.company eq cocode
            and item.i-no    eq rm-rctd.i-no
          use-index i-no exclusive-lock no-wait.
      if locked item then next transblok.

      ld-cvt-qty = rm-rctd.qty.

      if (item.i-code eq "E" and rm-rctd.rita-code eq "R") then do:
        {rm/rm-poupd.i 1}
        delete rm-rctd.
        NEXT transblok.
      end.

      if rm-rctd.rita-code eq "I" then
      for each xrm-rctd
          where xrm-rctd.company   eq cocode
            and xrm-rctd.i-no      eq rm-rctd.i-no
            and xrm-rctd.rita-code eq "R"
            and xrm-rctd.po-no     eq rm-rctd.po-no
            and xrm-rctd.r-no      lt rm-rctd.r-no
          no-lock:

        undo transblok, next transblok.
      end.

      find first job
          where job.company eq cocode
            and job.job-no  eq fill(" ",6 - length(trim(rm-rctd.job-no))) +
                               trim(rm-rctd.job-no)
            and job.job-no2 eq rm-rctd.job-no2
          no-error.

      /** Find Bin & if not avail then create it **/
      find first rm-bin
          where rm-bin.company eq rm-rctd.company
            and rm-bin.loc     eq rm-rctd.loc
            and rm-bin.i-no    eq rm-rctd.i-no
            and rm-bin.loc-bin eq rm-rctd.loc-bin
            and rm-bin.tag     eq rm-rctd.tag
          no-error.
      if not avail rm-bin then do:
        create rm-bin.
        assign
         rm-bin.company = rm-rctd.company
         rm-bin.loc     = rm-rctd.loc
         rm-bin.loc-bin = rm-rctd.loc-bin
         rm-bin.tag     = rm-rctd.tag
         rm-bin.i-no    = rm-rctd.i-no.
      end. /* not avail rm-bin */

      if rm-rctd.rita-code eq "R" then do:        /** RECEIPTS **/
        {rm/rm-post.i "rm-bin.qty" "rm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}

        assign
         rm-bin.qty     = rm-bin.qty + rm-rctd.qty
         item.last-cost = rm-rctd.cost
         item.q-onh     = item.q-onh + rm-rctd.qty.

        {rm/rm-poupd.i 2}

        item.q-avail = item.q-onh + item.q-ono - item.q-comm.
      end. /* R */

      else
      if rm-rctd.rita-code eq "I" then do:  /** ISSUES **/
        if avail job and job.job-no ne "" then do:
          run rm/mkjobmat.p (recid(rm-rctd),rm-rctd.company, output v-recid).

          find job-mat where recid(job-mat) eq v-recid no-error.

          if not avail job-mat then do:
            bell.
            message " Job Mat Record not found for "
                    string(job.job-no + "-" + string(job.job-no2,"99") +
                           "  " + rm-rctd.i-no)
                    VIEW-AS ALERT-BOX.
            undo transblok, next transblok.
          end.

          assign
           v-bwt = job-mat.basis-w
           v-len = job-mat.len
           v-wid = job-mat.wid
           v-dep = item.s-dep.

          if v-len eq 0 then v-len = item.s-len.

          if v-wid eq 0 then
            v-wid = if item.r-wid ne 0 then item.r-wid else item.s-wid.

          if v-bwt eq 0 then v-bwt = item.basis-w.

          if index("RL",job.stat) ne 0 then job.stat = "W".

          {rm/rmmatact.i}            /* Create Actual Material */

          run sys/ref/convquom.p(rm-rctd.pur-uom, job-mat.qty-uom,
                                 v-bwt, v-len, v-wid, v-dep,
                                 rm-rctd.qty, output out-qty).

          run sys/ref/convcuom.p(rm-rctd.pur-uom, job-mat.sc-uom,
                                 v-bwt, v-len, v-wid, v-dep,
                                 rm-rctd.cost, output cost).

          assign
           mat-act.qty-uom = job-mat.qty-uom
           mat-act.cost    = if mat-act.cost eq 0 then cost else mat-act.cost
           mat-act.qty     = mat-act.qty     + out-qty
           job-mat.qty-iss = job-mat.qty-iss + out-qty
           job-mat.qty-all = job-mat.qty-all - out-qty
           item.q-comm     = item.q-comm     - rm-rctd.qty.

          run sys/ref/convquom.p(rm-rctd.pur-uom, job-mat.sc-uom,
                                 v-bwt, v-len, v-wid, v-dep,
                                 rm-rctd.qty, output out-qty).

          mat-act.ext-cost = mat-act.ext-cost + (cost * out-qty).

          /* Don't relieve more than were allocated */
          if job-mat.qty-all lt 0 then
            run sys/ref/convquom.p(job-mat.qty-uom, rm-rctd.pur-uom,
                                   v-bwt, v-len, v-wid, v-dep,
                                   job-mat.qty-all, output out-qty).
          assign
           job-mat.qty-all = 0
           item.q-comm     = item.q-comm - out-qty.

          job-mat.all-flg = (job-mat.qty-all gt 0).
          if item.q-comm lt 0 then item.q-comm = 0.

          IF item.mat-type EQ "B" THEN RUN rm/rm-addcr.p (ROWID(rm-rctd)).
        end.

        find first rm-bin
            where rm-bin.company eq rm-rctd.company
              and rm-bin.loc     eq rm-rctd.loc
              and rm-bin.i-no    eq rm-rctd.i-no
              and rm-bin.loc-bin eq rm-rctd.loc-bin
              and rm-bin.tag     eq rm-rctd.tag
            no-error.

        assign
         rm-bin.qty     = rm-bin.qty - rm-rctd.qty
         item.q-onh     = item.q-onh - rm-rctd.qty
         item.qlast-iss = rm-rctd.qty
         item.dlast-iss = rm-rctd.rct-date
         item.q-ytd     = item.q-ytd + rm-rctd.qty
         item.q-ptd     = item.q-ptd + rm-rctd.qty
         item.u-ptd     = item.u-ptd + (rm-rctd.cost * rm-rctd.qty)
         item.u-ytd     = item.u-ytd + (rm-rctd.cost * rm-rctd.qty)
         item.q-avail   = item.q-onh + item.q-ono - item.q-comm.
      end.  /* I */

      else
      if rm-rctd.rita-code eq "A" then do:  /** ADJUSTMENTS **/
        if rm-rctd.cost ne 0 then do:
          {rm/rm-post.i "rm-bin.qty" "rm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}
        end.

        assign
         rm-bin.qty     = rm-bin.qty + rm-rctd.qty
         item.last-cost = if rm-rctd.cost ne 0 then rm-rctd.cost
                                               else item.last-cost
         item.q-onh     = item.q-onh + rm-rctd.qty
         item.q-avail   = item.q-onh + item.q-ono - item.q-comm.
      end. /* A */

      else
      if rm-rctd.rita-code eq "T" then do:  /** TRANSFERS **/
        assign
         rm-bin.qty   = rm-bin.qty - rm-rctd.qty
         rm-rctd.cost = rm-bin.cost.

        /* This code is to handel the Transfer to quantity to increase the BIN
           using a buffer record so current rm-bin record is not updated. */

        find first xrm-bin
             where xrm-bin.company eq rm-rctd.company
               and xrm-bin.loc     eq rm-rctd.loc2
               and xrm-bin.i-no    eq rm-rctd.i-no
               and xrm-bin.loc-bin eq rm-rctd.loc-bin2
               and xrm-bin.tag     eq rm-rctd.tag2
             no-error.
        if not avail xrm-bin then do:
          create xrm-bin.
          assign
           xrm-bin.company = rm-rctd.company
           xrm-bin.loc     = rm-rctd.loc2
           xrm-bin.loc-bin = rm-rctd.loc-bin2
           xrm-bin.tag     = rm-rctd.tag2
           xrm-bin.i-no    = rm-rctd.i-no.
        end.

        {rm/rm-post.i "rm-bin.qty" "rm-bin.cost" "rm-rctd.qty" "rm-rctd.cost"}

        xrm-bin.qty = xrm-bin.qty + rm-rctd.qty.
      end. /* T */

/*       /** Delete Bins With Zero Quantities. **/ */
/*       if rm-bin.qty = 0 then delete rm-bin.     */

      if last-of(b-rm-rctd.i-no) then             /* Calculate average cost */
      for each rm-bin
          where rm-bin.company eq rm-rctd.company
            and rm-bin.i-no    eq rm-rctd.i-no
          no-lock use-index i-no
          break by rm-bin.i-no:

        if first(rm-bin.i-no) then
          assign
           v-i-qty = 0
           cost    = 0.

        v-r-qty = rm-bin.qty.

        if v-r-qty lt 0 then v-r-qty = v-r-qty * -1.

        assign
         v-i-qty = v-i-qty + v-r-qty
         cost    = cost    + (v-r-qty * rm-bin.cost).

        if last(rm-bin.i-no) and v-i-qty ne 0 THEN item.avg-cost = cost / v-i-qty.
      end. /* each rm-bin */

      {rm/rm-rctd.i rm-rcpth rm-rdtlh rm-rctd} /* Create History Records */

      delete rm-rctd.
    end. /* for each rm-rctd */

    v-dunne = yes.
    for each rm-rctd
        where rm-rctd.company   eq cocode
          and rm-rctd.rita-code eq "ADDER"
          and rm-rctd.job-no    ge v-from-job
          and rm-rctd.job-no    le v-to-job     
        transaction:

      rm-rctd.rita-code = "I".
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ rm/rep/rm-post.p 10/93 cd */
/* raw materials posting - part 1 : printout                                  */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

def var v-type-prt as ch format "X(11)" init "".
def var v-ext-cost as de.
def var v-tot-qty as dec format "->>,>>>,>>9.99<<".
def var v-tot-cost as dec format "->,>>>>,>>9.99<<".
def var v-grd-qty as dec format "->>,>>>,>>9.99<<".
def var v-grd-cost as dec format "->,>>>>,>>9.99<<".
def var v-po-no like rm-rctd.po-no.
def var v-inkissue as log.
def var v-pr-tots as log format "Y/N".     
def var v-whse like rm-rctd.loc.
DEF VAR v-int AS INT NO-UNDO.

def buffer b-rm-rctd for rm-rctd.
DEF BUFFER b-item FOR ITEM.

form header "WHSE:" v-whse with frame r-top.

form rm-rctd.rct-date                   label "DATE"
     rm-rctd.i-no                       label "ITEM"
     rm-rctd.i-name format "x(14)"      label "DESCRIPTION"
     rm-rctd.po-no                      label "P.O.#"
     po-ord.vend-no                     label "VENDOR"
     rm-rctd.job-no                     label "Job #" space(0) "-" space(0)
     rm-rctd.job-no2                    label ""
     rm-rctd.rita-code                  label "T"
     rm-rctd.tag                        label "TAG#"
     rm-rctd.qty format "->>>>>9.99<<"  label "QUANTITY" 
     rm-rctd.loc-bin                    label "BIN"
     rm-rctd.pur-uom                    label "UOM"    
     rm-rctd.cost format "->>>>>9.99"   label "COST"
     v-ext-cost                         label "TOTAL COST"

    with frame itemx no-box down STREAM-IO width 132.

    if index(v-types,"R") gt 0 then
    auto-issue:
    for each rm-rctd
        where rm-rctd.company   eq cocode
          and rm-rctd.rita-code eq "R"
          and rm-rctd.job-no    ge v-from-job
          and rm-rctd.job-no    le v-to-job
          AND rm-rctd.job-no    ne ""
        no-lock,

        first item
        where item.company eq cocode
          and item.i-no    eq rm-rctd.i-no
        no-lock.

      release po-ordl.

      v-po-no = trim(rm-rctd.po-no).
      if v-po-no ne "" then do:
        do x = 1 to length(v-po-no):
          if substr(v-po-no,x,1) lt "0" or
             substr(v-po-no,x,1) gt "9" then next auto-issue.
        end.

        find first po-ordl
            where po-ordl.company   eq cocode
              and po-ordl.i-no      eq rm-rctd.i-no
              and po-ordl.po-no     eq int(v-po-no)
              and po-ordl.job-no    eq rm-rctd.job-no
              and po-ordl.job-no2   eq rm-rctd.job-no2
              and po-ordl.item-type eq yes
            use-index item-ordno no-lock no-error.
      end.

      if (item.i-code eq "E" and not avail po-ordl) or
          (item.i-code eq "R" and not v-autoissue)  then next auto-issue.

      v-int = 1.
     RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT v-int) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN
       MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

      create b-rm-rctd.
      buffer-copy rm-rctd to b-rm-rctd
      assign
       b-rm-rctd.r-no      = v-int
       b-rm-rctd.rita-code = "I".

      create w-rec-h.
      assign
       w-rec-h.rec-id    = recid(b-rm-rctd)
       w-rec-h.rita-code = "R".
    end.

    if index(v-types,"I") gt 0 then
    issue-adder-for-board:
    for each rm-rctd
        where rm-rctd.company   eq cocode
          and rm-rctd.rita-code eq "I"
          and rm-rctd.job-no    ge v-from-job
          and rm-rctd.job-no    le v-to-job
          AND rm-rctd.job-no    ne ""
        no-lock,

        first job
        where job.company eq cocode
          and job.job-no  eq rm-rctd.job-no
          and job.job-no2 eq rm-rctd.job-no2
        no-lock,

        first item
        where item.company eq cocode
          and item.i-no    eq rm-rctd.i-no
          and item.mat-type eq "B"
        no-lock:

      {rm/rm-addcr.i E b-rm-rctd b-rm-rctd b-}
        create w-rec-h.
        assign
         w-rec-h.rec-id    = recid(b-rm-rctd)
         w-rec-h.rita-code = "I".
      END.
    end.

  assign
   str-tit2 = c-win:TITLE
   {sys/inc/ctrtext.i str-tit2 112}.

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  if td-show-parm then run show-param.

    assign
     v-grd-qty = 0
     v-grd-cost = 0.

    for each rm-rctd 
        where rm-rctd.company   eq cocode
          and rm-rctd.job-no    ge v-from-job
          and rm-rctd.job-no    le v-to-job
          AND rm-rctd.r-no      eq rm-rctd.r-no
          and index(v-types,rm-rctd.rita-code) gt 0 
          and rm-rctd.rita-code ne "C"
        no-lock

        break by rm-rctd.loc                                             
              by rm-rctd.i-no 
              by rm-rctd.loc-bin                           
              by rm-rctd.tag

        with frame itemx:                                                   

      if first-of(rm-rctd.loc) then do:
        v-whse = rm-rctd.loc.

        if first(rm-rctd.loc) then DISPLAY WITH frame r-top.

        else put skip(3) "WHSE: " v-whse.
      end.

      v-ext-cost = rm-rctd.cost * rm-rctd.qty.

      if rm-rctd.po-no ne " " then                                         
      find first po-ord
          where po-ord.company eq cocode
            and po-ord.po-no   eq int(rm-rctd.po-no)
          no-error. 

      display rm-rctd.rct-date   when first-of(rm-rctd.i-no)
              rm-rctd.i-no       when first-of(rm-rctd.i-no)
              rm-rctd.i-name     when first-of(rm-rctd.i-no)
              rm-rctd.po-no
              po-ord.vend-no     when avail po-ord                     
              rm-rctd.job-no
              rm-rctd.job-no     when rm-rctd.job-no eq "" @ rm-rctd.job-no
              rm-rctd.job-no2
              rm-rctd.job-no2    when rm-rctd.job-no eq "" @ rm-rctd.job-no2
              rm-rctd.rita-code
              rm-rctd.tag
              rm-rctd.qty
              rm-rctd.loc-bin
              rm-rctd.pur-uom                                   
              rm-rctd.cost
              v-ext-cost.
      down.

      if rm-rctd.rita-code eq "T" then 
        put "To WHSE: " at 72
            rm-rctd.loc2 format "x(5)"
            space(1)
            rm-rctd.loc-bin2
            skip.               

      if rm-rctd.rita-code eq "R" or
         rm-rctd.rita-code eq "A" then
        assign
         v-tot-qty = v-tot-qty + rm-rctd.qty
         v-tot-cost = v-tot-cost + (rm-rctd.cost * rm-rctd.qty).

      if last-of(rm-rctd.i-no) then do:
        if v-pr-tots then
          put "-----------"                         to 85
              "----------"                          to 120 skip
              "Total for Receipts and Adjustments:" to 59
              v-tot-qty                             to 85
              v-tot-cost                            to 120 skip(1).

        assign
         v-grd-qty  = v-grd-qty + v-tot-qty
         v-grd-cost = v-grd-cost + v-tot-cost
         v-tot-qty  = 0
         v-tot-cost = 0.
      end.   
    end. /* each rm-rctd */

    if v-pr-tots then                                                           
      put "-----------"                               to 85
          "----------"                                to 120 skip
          "Grand Total for Receipts and Adjustments:" to 59
          v-grd-qty                                   to 85
          v-grd-cost                                  to 120 skip(1).

END PROCEDURE.

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
  def var lv-label as cha NO-UNDO.

  ASSIGN
  lv-frame-hdl = frame {&frame-name}:HANDLE
  lv-group-hdl = lv-frame-hdl:first-child
  lv-field-hdl = lv-group-hdl:first-child.

  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," .
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
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

