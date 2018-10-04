&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-bole&p.w

  Description: BOL Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 04/12/2002

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

def buffer xap-payl for ap-payl.    

def var time_stamp as character.
def var save_id as recid.
def var v-post as logical.
def var v-matching-record as logical.
def var v-trans-num as integer.
def var v-tot-amt-paid like ap-pay.check-amt.
def var v-tot-amt-disc like ap-payl.amt-disc.
def var loc-no like ap-pay.c-no.   
def var first-time as log. 
DEF VAR v-invalid AS LOG NO-UNDO. 
DEF VAR v-postable AS LOG NO-UNDO.  

def workfile xpayl field recnum as recid.

def workfile w-disb field w-actnum   like ap-payl.actnum
                    field w-amt-paid like ap-payl.amt-paid
                    field w-amt-disc like ap-payl.amt-disc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date rd-dest ~
lines-per-page td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period rd-dest ~
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

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 55 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 5.48.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.24.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 4.81 COL 32 COLON-ALIGNED
     tran-period AT ROW 4.81 COL 60 COLON-ALIGNED
     rd-dest AT ROW 12.43 COL 11 NO-LABEL
     lines-per-page AT ROW 12.91 COL 72 COLON-ALIGNED
     td-show-parm AT ROW 14.57 COL 58
     btn-ok AT ROW 18.14 COL 23
     btn-cancel AT ROW 18.14 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.71 COL 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     RECT-6 AT ROW 11.48 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 19.71.


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
         TITLE              = "BOL Edit List & Posting"
         HEIGHT             = 20.19
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
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       tran-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* BOL Edit List  Posting */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* BOL Edit List  Posting */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DEF VAR lv-post AS LOG NO-UNDO.


  assign rd-dest.

  run check-date.
  if v-invalid then return no-apply.

  run run-report.

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case.

  IF v-postable THEN DO:
    run list-gl.

    case rd-dest:
         when 1 then run output-to-printer.
         when 2 then run output-to-screen.
         when 3 then run output-to-file.
    end case.

    v-post = NO.

    MESSAGE "Do You Want To Post Voided Checks ?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-post.

    IF lv-post THEN do:
      RUN list-post-inv ("post").

      RUN post-gl.

      MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
    END.
  END.

  ELSE MESSAGE "No Invoices available for posting..." VIEW-AS ALERT-BOX ERROR.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Transaction Date */
DO:
  assign {&self-name}.

  if lastkey ne -1 then do:
    run check-date.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-period C-Win
ON LEAVE OF tran-period IN FRAME FRAME-A /* Period */
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

ASSIGN
  tran-date   = today.

  RUN init-proc.  
  RUN enable_UI.
  RUN check-date.

  do transaction:
   /* gdm - 11050906 */
   REPEAT:
    FIND FIRST gl-ctrl EXCLUSIVE-LOCK
      WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
    IF AVAIL gl-ctrl THEN DO:
      ASSIGN v-trans-num = gl-ctrl.trnum + 1.
             gl-ctrl.trnum = v-trans-num.
      FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.
      LEAVE.
    END. /* IF AVAIL gl-ctrl */
   END. /* REPEAT */
   /* gdm - 11050906 */
  END.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date C-Win 
PROCEDURE check-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO with frame {&frame-name}:
    v-invalid = no.

    find first period                   
        where period.company eq cocode
          and period.pst     le tran-date
          and period.pend    ge tran-date
        no-lock no-error.
    if avail period then tran-period:SCREEN-VALUE = string(period.pnum).

    else
    IF ip-post THEN DO:
      message "No Defined Period Exists for" tran-date view-as alert-box error.
      v-invalid = yes.
    end.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-nopost C-Win 
PROCEDURE create-nopost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


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
  DISPLAY tran-date tran-period rd-dest lines-per-page td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date rd-dest lines-per-page td-show-parm btn-ok 
         btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exception-rpt C-Win 
PROCEDURE exception-rpt :
/* -------------------------------------------------- oe/oe-bolp7.p 11/01 JLF */
/* BOL posting Exception Report                                               */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

FORM HEADER SKIP(1) WITH FRAME r-top.


  FIND first period                   
      where period.company eq gcompany
        and period.pst     le tran-date
        and period.pend    ge tran-date
      no-lock no-error.

  assign
   str-tit2 = "BOL - Insufficient Inventory Report"
   {sys/inc/ctrtext.i str-tit2 112}

   str-tit3 = "Period " + STRING(tran-period,"99") + " - " +
              IF AVAIL period THEN
                (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
   {sys/inc/ctrtext.i str-tit3 132}.

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  display with frame r-top.

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
/* ---------------------------------------------------- ap/apvdckrg.p 4/94 RM */
/* Void Accounts Payable Check Register - A/P Module                          */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}


form header
   "Check #" at 1
   "Date" at 17
   "Amount" at 33
   "Vendor" at 59 skip
   fill("_",132) format "x(130)" skip
   with frame f-top stream-io width 132 no-box no-labels no-underline.

form ap-pay.check-no at 1 format "zzzzzzz9"
     ap-pay.check-date at 17
     ap-pay.check-amt at 33 format "zzz,zz9.99"
     ap-pay.vend-no at 59 space(1) vend.name
     with frame report-lines stream-io width 132 no-box no-labels.

form skip(2) space(54) "(There are no voided checks)"
     with frame no-matching-record stream-io width 132 no-box no-labels.

form skip(2) space(58) "(End of the report)"
     with frame end-of-report stream-io width 132 no-box no-labels.
view frame voidckreg.

  assign
   str-tit2 = c-win:TITLE
   {sys/inc/ctrtext.i str-tit2 112}

   str-tit3 = "Period " + STRING(tran-period,"99") + " - " +
              IF AVAIL period THEN
                (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
   {sys/inc/ctrtext.i str-tit3 132}.




  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  if td-show-parm then run show-param.

  display with frame r-top.

 for each ap-pay where ap-pay.company = cocode and
                      ap-pay.cleared = ? and
                      ap-pay.reconciled = no
                      break by ap-pay.check-no:
    find first vend where vend.company = cocode and
                          vend.vend-no = ap-pay.vend-no no-lock no-error.
    display ap-pay.check-no
            ap-pay.check-date
            ap-pay.check-amt
            ap-pay.vend-no
            vend.name when avail vend
            with frame report-lines stream-io width 132 no-box no-labels.
    down with frame report-lines.
    assign v-matching-record = yes.
end. /* for each ap-pay record */

if v-matching-record = no then
   display with frame no-matching-record.
display with frame end-of-report.
hide frame f-top.
page.

 if v-matching-record = no then return.

hide frame verify-date.
v-post = no.


postit:
do transaction on error undo, leave:
   find first ap-ctrl where ap-ctrl.company = cocode no-lock no-error.

   for each ap-pay where ap-pay.company = cocode and
                         ap-pay.cleared = ? and
                         ap-pay.reconciled = no
                         on error undo postit, leave postit:

       assign ap-pay.cleared = yes
              ap-pay.reconciled = ?
              v-tot-amt-paid = v-tot-amt-paid + ap-pay.check-amt.

       find first bank where bank.company = cocode and
                             bank.bank-code = ap-pay.bank-code no-error.
       if available bank then
          bank.bal = bank.bal + ap-pay.check-amt.

       create ap-ledger.
         assign
         ap-ledger.company = ap-pay.company
         ap-ledger.vend-no = ap-pay.vend-no
         ap-ledger.refnum = "VOIDED CHECK" + string(ap-pay.check-no, "zzzzzzz9")
         ap-ledger.ref-date = today
         ap-ledger.tr-date = udate
         ap-ledger.trnum = v-trans-num
         ap-ledger.period = uperiod
         ap-ledger.amt = ap-ledger.amt - ap-pay.check-amt
         ap-ledger.actnum = bank.actnum.

       for each xpayl:
         delete xpayl.
       end.

       for each ap-payl where ap-payl.c-no = ap-pay.c-no:
           create xpayl.                    
           xpayl.recnum = recid(ap-payl).   

           find first ap-inv where ap-inv.company = cocode and
                                   ap-inv.vend-no = ap-pay.vend-no and
                                   ap-inv.inv-no = ap-payl.inv-no no-error.
           if available ap-inv then
           do:
              find vend where vend.company = cocode and
                              vend.vend-no = ap-inv.vend-no
                              exclusive-lock no-error.
              assign
              ap-inv.paid = ap-inv.paid - ap-payl.amt-paid
              ap-inv.disc-taken = ap-inv.disc-taken - ap-payl.amt-disc
              ap-inv.due = ap-inv.due + ap-payl.amt-paid + ap-payl.amt-disc.
              if available vend then
                 vend.acc-bal = vend.acc-bal + ap-payl.amt-paid +
                                       ap-payl.amt-disc.
           end. /* if avail ap-inv .. */

           v-tot-amt-disc = v-tot-amt-disc + ap-payl.amt-disc.

           if ap-payl.d-no ne 0 then do:
             create w-disb.
             assign
              w-actnum   = ap-payl.actnum
              w-amt-paid = ap-payl.amt-paid
              w-amt-disc = ap-payl.amt-disc.
           end.
       end. /* for each ap-payl record */

       for each xpayl,
           first ap-payl where recid(ap-payl) eq xpayl.recnum
           no-lock:

          find last xap-payl where xap-payl.c-no eq ap-payl.c-no
              use-index c-no no-lock no-error.
          x = if avail xap-payl then xap-payl.line else 0.

          create xap-payl.

          assign
           xap-payl.c-no       = ap-payl.c-no
           xap-payl.line       = x + 1
           xap-payl.actnum     = ap-payl.actnum
           xap-payl.amt-disc   = -(ap-payl.amt-disc)
           xap-payl.amt-due    = ap-payl.amt-due + ap-payl.amt-paid +
                                                   ap-payl.amt-disc
           xap-payl.amt-paid   = -(ap-payl.amt-paid)
           xap-payl.check-no   = ap-payl.check-no
           xap-payl.due-date   = ap-payl.due-date
           xap-payl.inv-no     = ap-payl.inv-no
           xap-payl.man-check  = ap-payl.man-check
           xap-payl.memo       = ap-payl.memo
           xap-payl.posted     = ap-payl.posted
           xap-payl.vend-no    = ap-payl.vend-no.
       end.  /* for each xpayl */
   end. /* for each ap-pay record */

   create gltrans.
   assign
    gltrans.company = cocode
    gltrans.actnum  = bank.actnum
    gltrans.jrnl    = "APVOIDCK"
    gltrans.tr-dscr = "AP VOIDED CHECK REGISTER"
    gltrans.tr-date = udate
    gltrans.tr-amt  = v-tot-amt-paid
    gltrans.period  = uperiod
    gltrans.trnum   = v-trans-num.

   if v-tot-amt-disc ne 0 then do:
     create gltrans.
     assign
      gltrans.company = cocode
      gltrans.actnum  = ap-ctrl.discount
      gltrans.jrnl    = "APVOIDCK"
      gltrans.tr-dscr = "AP VOIDED CHECK REGISTER"
      gltrans.tr-date = udate
      gltrans.tr-amt  = v-tot-amt-disc
      gltrans.period  = uperiod
      gltrans.trnum   = v-trans-num.
   end.

   for each w-disb break by w-actnum:
     assign
      v-tot-amt-paid = v-tot-amt-paid - w-amt-paid
      v-tot-amt-disc = v-tot-amt-disc - w-amt-disc.

     accumulate w-amt-paid (sub-total by w-actnum).
     accumulate w-amt-disc (sub-total by w-actnum).

     if last-of(w-actnum) then do:
       create gltrans.
       assign
        gltrans.company = cocode
        gltrans.actnum  = w-actnum
        gltrans.jrnl    = "APVOIDCK"
        gltrans.tr-dscr = "AP VOIDED CHECK REGISTER"
        gltrans.tr-date = udate
        gltrans.tr-amt  = ((accum sub-total by w-actnum w-amt-paid) +
                           (accum sub-total by w-actnum w-amt-disc)) * -1 
        gltrans.period  = uperiod
        gltrans.trnum   = v-trans-num.
     end.
   end.

   create gltrans.
   assign
    gltrans.company = cocode
    gltrans.actnum  = ap-ctrl.payables
    gltrans.jrnl    = "APVOIDCK"
    gltrans.tr-dscr = "AP VOIDED CHECK REGISTER"
    gltrans.tr-date = udate
    gltrans.tr-amt  = (v-tot-amt-paid + v-tot-amt-disc) * -1
    gltrans.period  = uperiod
    gltrans.trnum   = v-trans-num.
end. /* postit */

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

