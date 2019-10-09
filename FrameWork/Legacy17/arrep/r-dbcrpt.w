&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: Print Invoices

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

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
DEF VAR tmp-dir AS cha NO-UNDO.
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

/* gdm - 03120909 */
{custom/xprint.i}
{sys/inc/print1.i}

DEF VAR v-program AS CHAR NO-UNDO.
def NEW SHARED var v-lo-cust like cust.cust-no init "" NO-UNDO.
def NEW SHARED var v-hi-cust like cust.cust-no init "zzzzzzzz" NO-UNDO.
def NEW SHARED var v-lo-memo like ar-cash.check-no init 0 NO-UNDO.
def NEW SHARED var v-hi-memo like ar-cash.check-no init 99999999 NO-UNDO.
def NEW SHARED var v-reprint as   log NO-UNDO.

/* gdm - 04210922 */
DEF NEW SHARED VAR v-begdt LIKE ar-cash.check-date NO-UNDO.
DEF NEW SHARED VAR v-enddt LIKE ar-cash.check-date NO-UNDO.
DEF NEW SHARED VAR v-tbpst AS LOG                  NO-UNDO.

def var save_id as recid NO-UNDO.
def var time_stamp as ch NO-UNDO.
def var qfirst as l NO-UNDO.
def var g1 as dec format "->,>>>,>>9.99" NO-UNDO.
def var archk as dec format ">>>>>>99" NO-UNDO.
def var t1 as dec format "->,>>>,>>9.99" NO-UNDO.
def var g2 as dec format "->,>>>,>>9.99" NO-UNDO.
def var t3 as dec format "->,>>>,>>9.99" NO-UNDO.
def var v1 as dec format "->,>>>,>>9.99" NO-UNDO.
def var v2 as dec format "->,>>>,>>9.99" NO-UNDO.
def var t2 as dec format "->,>>>,>>9.99" NO-UNDO.
def var v-on-act-amt as dec format "->,>>>,>>9.99" NO-UNDO.
def var big_ul as char format "x(80)" no-undo.
def var big_ul2 as char format "x(80)" no-undo.
def var letterhead as char format "x(50)" extent 5 no-undo.

def var v-printlines as int init 0 no-undo.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.

DEF VAR v-ftp-done AS LOG NO-UNDO.
DEF VAR v-print-fmt AS cha NO-UNDO.
DEF NEW SHARED VAR v-term-id AS cha NO-UNDO.

DEFINE VARIABLE retcode AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.

 RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

/* gdm - 03120909 */
DEF VAR v-chrfld  AS CHAR NO-UNDO.

/* gdm 03120909 - dont why this is here, its also in MAIN BLOCK - 
redunduncy ? commented out
find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "INVPRINT"
      no-lock no-error.
v-print-fmt = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_cust end_cust begin_memo ~
end_memo begin_date end_date tb_reprint tb_posted tb_export rd-dest lv-ornt ~
lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_memo end_memo ~
begin_date end_date tb_reprint tb_posted tb_export rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_memo AS INTEGER FORMAT ">>>>>>>>>>" INITIAL 0 
     LABEL "Beginning Memo#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_memo AS INTEGER FORMAT ">>>>>>>>>9" INITIAL 99999999 
     LABEL "Ending Memo#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.81.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.05.

DEFINE VARIABLE tb_export AS LOGICAL INITIAL no 
     LABEL "Export/FTP  Memos?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_posted AS LOGICAL INITIAL no 
     LABEL "Reprint Posted Memos?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tb_reprint AS LOGICAL INITIAL no 
     LABEL "Reprint Memos?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust AT ROW 2.67 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 2.67 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_memo AT ROW 3.62 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Number"
     end_memo AT ROW 3.62 COL 70 COLON-ALIGNED HELP
          "Enter Ending Invoice Number"
     begin_date AT ROW 4.57 COL 27 COLON-ALIGNED HELP
          "Enter Beginning AR Memo Date"
     end_date AT ROW 4.57 COL 70 COLON-ALIGNED HELP
          "Enter Ending AR Memo Date"
     tb_reprint AT ROW 6 COL 36
     tb_posted AT ROW 7.19 COL 36
     tb_export AT ROW 8.38 COL 36
     rd-dest AT ROW 11.48 COL 7 NO-LABEL
     lv-ornt AT ROW 11.71 COL 31 NO-LABEL
     lines-per-page AT ROW 11.71 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 13.14 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 14.1 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17.19 COL 31
     btn-ok AT ROW 20.05 COL 24
     btn-cancel AT ROW 20.05 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 4
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.52 COL 5
     RECT-6 AT ROW 10.29 COL 1
     RECT-7 AT ROW 1 COL 1
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
         TITLE              = "Print Credit/Debit Memo"
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
   FRAME-NAME                                                           */
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
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_memo:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_memo:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_export:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_export:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_posted:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_posted:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_reprint:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Print Credit/Debit Memo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Print Credit/Debit Memo */
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


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_memo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_memo C-Win
ON LEAVE OF begin_memo IN FRAME FRAME-A /* Beginning Memo# */
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

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  ASSIGN {&DISPLAYED-OBJECTS}.

/* gdm - 03120909 */
  IF IS-xprint-form 
    THEN DO:
      RUN Set-Memo-Form. 
      RUN run-report. 
  END.
    ELSE RUN run-report-old.       

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_cust
                            &end_cust=end_cust
                            &fax-subject="Credit/Debit Memo"
                            &fax-body="Credit/Debit Memo"
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              ASSIGN lv-pdf-file = list-name.
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_cust
                             &end_cust=end_cust
                             &mail-subject="Credit/Debit Memo"
                             &mail-body="Credit/Debit Memo"
                             &mail-file=lv-pdf-file + ".pdf" }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= begin_cust
                                  &end_cust=end_cust
                                  &mail-subject="Credit/Debit Memo"
                                  &mail-body="Credit/Debit Memo"
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
  IF v-ftp-done THEN MESSAGE "File Export/FTP is completed." VIEW-AS ALERT-BOX INFORMATION.
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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
DO:
   assign {&self-name}.
   IF begin_memo = end_memo THEN DO:
     FIND FIRST ar-cash WHERE ar-cash.company = g_company
                         AND ar-cash.check-no = begin_memo NO-LOCK NO-ERROR.
     IF AVAIL ar-cash THEN ASSIGN begin_cust:SCREEN-VALUE = ar-cash.cust-no
                                 end_cust:SCREEN-VALUE = ar-cash.cust-no.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_memo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_memo C-Win
ON LEAVE OF end_memo IN FRAME FRAME-A /* Ending Memo# */
DO:
   assign {&self-name}.
   IF begin_memo = end_memo THEN DO:
     FIND FIRST ar-cash WHERE ar-cash.company = g_company
                         AND ar-cash.check-no = begin_memo NO-LOCK NO-ERROR.
     IF AVAIL ar-cash THEN ASSIGN begin_cust:SCREEN-VALUE = ar-cash.cust-no
                                 end_cust:SCREEN-VALUE = ar-cash.cust-no.
     /* task 01311304 */
     FIND FIRST reftable WHERE reftable.reftable EQ  "AR-CASH"
                            AND reftable.code     EQ 
                            STRING(ar-cash.c-no,"9999999999") NO-LOCK NO-ERROR.
     IF AVAIL ar-cash AND ar-cash.posted THEN DO:
       IF AVAIL reftable THEN
         ASSIGN
            tb_reprint:SCREEN-VALUE = "YES"
            tb_posted:SCREEN-VALUE  = "YES".
       ELSE
           ASSIGN
               tb_posted:SCREEN-VALUE  = "YES" 
               tb_reprint:SCREEN-VALUE = "NO".
     END.    
     ELSE do:
        IF AVAIL reftable THEN
         tb_reprint:SCREEN-VALUE = "YES".
        ELSE
            tb_reprint:SCREEN-VALUE = "NO" .
      tb_posted:SCREEN-VALUE  = "NO" . /* task 01311304 */
     END.
   END. 
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


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON LEAVE OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
   ASSIGN lv-font-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON LEAVE OF lv-ornt IN FRAME FRAME-A
DO:
  ASSIGN lv-ornt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON VALUE-CHANGED OF lv-ornt IN FRAME FRAME-A
DO:
  {custom/chgfont.i}
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


&Scoped-define SELF-NAME tb_export
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_export C-Win
ON VALUE-CHANGED OF tb_export IN FRAME FRAME-A /* Export/FTP  Memos? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_posted
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_posted C-Win
ON VALUE-CHANGED OF tb_posted IN FRAME FRAME-A /* Reprint Posted Memos? */
DO:
  assign {&self-name}.
   IF tb_posted THEN ASSIGN tb_export:SENSITIVE = YES
                           tb_reprint:SCREEN-VALUE = "YES".
  ELSE ASSIGN tb_export:SCREEN-VALUE = "NO"
              tb_export:SENSITIVE = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reprint C-Win
ON VALUE-CHANGED OF tb_reprint IN FRAME FRAME-A /* Reprint Memos? */
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

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.


  FIND FIRST sys-ctrl NO-LOCK
      WHERE sys-ctrl.company eq cocode AND sys-ctrl.name eq "ARMEMO" NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN 
  DO TRANSACTION:
     CREATE sys-ctrl.
     ASSIGN 
         sys-ctrl.company  = cocode
         sys-ctrl.name     = "ARMEMO"
         sys-ctrl.descrip  = "Credit/Debit Memo Form"
         sys-ctrl.char-fld = "".
  END. 

  ASSIGN v-chrfld = IF AVAIL sys-ctrl THEN TRIM(sys-ctrl.char-fld) ELSE "" 
         v-print-fmt  = sys-ctrl.char-fld.

  IF AVAIL sys-ctrl AND 
     (TRIM(sys-ctrl.char-fld) NE "" AND
      TRIM(sys-ctrl.char-fld) NE "HOP")
    THEN IS-xprint-form = TRUE.  

  do transaction:
    {sys/inc/inexport.i}
  end.

  lines-per-page = 58.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust.
     IF v-print-fmt = "frankstn" OR v-print-fmt = "Mirpkg" THEN 
       ASSIGN tb_export:HIDDEN = NO
              tb_export = NO
              tb_export:SCREEN-VALUE = "NO"
              tb_export:SENSITIVE = IF tb_posted:SCREEN-VALUE = "yes" THEN YES ELSE NO .
    ELSE ASSIGN tb_export = no
                tb_export:SCREEN-VALUE = "NO"
                tb_export:HIDDEN = YES.
    DISABLE lines-per-page.
    IF tb_posted:SCREEN-VALUE = 'yes' THEN tb_reprint:SCREEN-VALUE = "YES".
  END.

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
  DISPLAY begin_cust end_cust begin_memo end_memo begin_date end_date tb_reprint 
          tb_posted tb_export rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_cust end_cust begin_memo end_memo begin_date 
         end_date tb_reprint tb_posted tb_export rd-dest lv-ornt lv-font-no 
         td-show-parm btn-ok btn-cancel 
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
  {custom/out2file.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-port C-Win 
PROCEDURE output-to-port :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN custom/d-print.w (list-name).

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
 /*    DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 1, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */

  */
/* gdm - 03120909 */
IF is-xprint-form THEN DO:
    FILE-INFO:FILE-NAME = list-name.
    RUN printfile (FILE-INFO:FILE-NAME).
END.
ELSE 
/* gdm - 03120909 end */
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
/* gdm - 03120909 */
IF is-xprint-form THEN DO:
    FILE-INFO:FILE-NAME = list-name.
    RUN printfile (FILE-INFO:FILE-NAME).
END.
ELSE 
/* gdm - 03120909 end */
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ oe/rep/invoice.p  9/94 RM */
/* PRINT INVOICE - O/E MODULE                                                 */
/* -------------------------------------------------------------------------- */

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

{sa/sa-sls01.i}

v-term-id = v-term.

{sys/form/r-top3.f}



  /* gdm - 03120909 */
  IF IS-xprint-form THEN DO:
    CASE rd-dest:
        WHEN 1 THEN PUT  "<PRINTER?></PROGRESS>".
        WHEN 2 THEN do:
            IF NOT lBussFormModle THEN
              PUT "<PREVIEW><MODAL=NO></PROGRESS>".
            ELSE 
              PUT "<PREVIEW></PROGRESS>". 
        END.    


        WHEN  4 THEN do:
              ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
                  /*(IF is-xprint-form THEN ".xpr" ELSE ".txt").*/
              PUT UNFORMATTED "</PROGRESS><PRINTER?><EXPORT=" Ls-fax-file ",BW>".
        END.
        WHEN 5 THEN PUT "<PREVIEW><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".
    END CASE.
  END.


ASSIGN 
    v-lo-cust = begin_cust
    v-hi-cust = end_cust
    v-lo-memo = begin_memo
    v-hi-memo = end_memo
    v-reprint = tb_reprint
    v-begdt   = begin_date
    v-enddt   = end_date
    v-tbpst   = tb_posted.

if v-hi-cust eq "" then v-hi-cust = "zzzzzzzz".

SESSION:SET-WAIT-STATE("general").

/* gdm - 03120909 */
IF IS-xprint-form THEN RUN VALUE(v-program).

FOR EACH report WHERE report.term-id EQ v-term-id: 
  DELETE report.
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE("").

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-old C-Win 
PROCEDURE run-report-old :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{sys/inc/print1.i}
{sys/inc/outprint.i value(lines-per-page)}

 DEF VAR lv-desc LIKE ar-cashl.dscr NO-UNDO.

if td-show-parm then run show-param.

{sa/sa-sls01.i}
v-term-id = v-term.

{sys/form/r-top3.f}


format 
    lv-desc at 4
    ar-cashl.inv-no at 55
    ar-cashl.amt-paid to 78
   with frame crdb-lines no-box no-labels width 80 down.


assign
 time_stamp = string(time, "hh:mmam")
 tmpstore   = fill("_",125)
 big_ul     = fill("=",80)
 big_ul2    = fill("=",80).

ASSIGN v-lo-cust = begin_cust
       v-hi-cust = end_cust
       v-lo-memo = begin_memo
       v-hi-memo = end_memo
       v-reprint = tb_reprint.

find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

if avail oe-ctrl and oe-ctrl.prcom then do:

  assign
   letterhead[1] = ""
   letterhead[2] =  company.name
   letterhead[3] =  company.addr[1]
   letterhead[4] =  company.addr[2]
   letterhead[5] =  company.city + ", " + company.state + "  " + company.zip.

  do i = 5 to 2 by -1:
    if letterhead[i - 1] le " " and letterhead[i] ge "" then
      assign
       letterhead[i - 1] = letterhead[i]
       letterhead[i]     = "".
  end.
  do i = 1 to 5:
    {sys/inc/ctrtext.i letterhead[i] 50}.
  end.
end.

if v-hi-cust eq "" then v-hi-cust = "zzzzzzzz".

if v-chrfld eq "HOP" then do:
  run ar/rep/memohop.p.
  return.
end.
SESSION:SET-WAIT-STATE("general").


  for each ar-cash
      where ar-cash.company    eq cocode
        and ar-cash.posted     eq tb_posted
        and ar-cash.memo       eq yes
        and ar-cash.cust-no    ge v-lo-cust
        and ar-cash.cust-no    le v-hi-cust
        and ar-cash.check-no   ge v-lo-memo
        and ar-cash.check-no   le v-hi-memo
        and ar-cash.check-date ge begin_date
        and ar-cash.check-date le end_date
        and can-find(first ar-cashl where ar-cashl.c-no eq ar-cash.c-no)
        and ((v-reprint and
              can-find(first reftable
                       where reftable.reftable eq "AR-CASH"
                         and reftable.code     eq
                                    string(ar-cash.c-no,"9999999999")
                           use-index code)) or
             (not v-reprint and
              not can-find(first reftable
                           where reftable.reftable eq "AR-CASH"
                             and reftable.code     eq
                                        string(ar-cash.c-no,"9999999999")
                           use-index code)))
      use-index posted        
      break by ar-cash.cust-no
            by ar-cash.check-no with frame a1:

    FIND FIRST reftable WHERE
         reftable.reftable = "ARCASHHOLD" AND
         reftable.rec_key = ar-cash.rec_key
         USE-INDEX rec_key
         NO-LOCK NO-ERROR.

    /*skip on hold cms*/
    IF AVAIL reftable AND reftable.CODE EQ "H" THEN
    DO:
      RELEASE reftable.
      NEXT.
    END.

    release reftable no-error.

    IF can-find(FIRST ar-cashl WHERE ar-cashl.company = cocode and
                               ar-cashl.c-no = ar-cash.c-no AND
                               (ar-cashl.amt-paid + ar-cashl.amt-disc) < 0 )
    THEN DO:
      CREATE report.
      ASSIGN
          report.term-id = v-term-id
          report.key-01  = STRING(ar-cash.check-no,"9999999999")
          report.rec-id  = RECID(ar-cash).
    END.

    if first-of(ar-cash.cust-no) then do:
      v-printlines = 1.
      find first cust
          {ar/ar-custW.i}
            and cust.cust-no eq ar-cash.cust-no
          no-lock no-error.

      if first(ar-cash.cust-no) then
        format HEADER
               SKIP(1)
               letterhead[1] at 5 "  Date:" to 70
               ar-cash.check-date FORMAT "99/99/99" skip
               letterhead[2] at 5
               letterhead[3] at 5 "Memo #:" to 70
               ar-cash.check-no format ">>>>>>>9" skip
               letterhead[4] at 5
               letterhead[5] at 5
               skip (4)
               "Customer #:" at 11 ar-cash.cust-no
               cust.name              at 11
               cust.addr [1]          at 11
               cust.addr [2]          at 11
               cust.city              at 11 cust.state cust.zip
               skip(4)
               big_ul at 1
        "********************     CREDIT / DEBIT MEMO     ********************"
               at 6
               big_ul2 at 1 skip(1)
               "Description" at 4 "Invoice #" at 55 "Amount" to 78
        "---------------------------------------------" at 4 "---------" at 55
        "--------------" to 78
            with frame crdb-header no-box no-labels width 80 PAGE-TOP STREAM-IO.

    end.
    view frame crdb-header.
    if first-of(ar-cash.check-no) then PAGE.

    for each ar-cashl
       where ar-cashl.company eq cocode
         and ar-cashl.c-no    eq ar-cash.c-no
       break by ar-cashl.line
       with frame crdb-lines no-box no-labels width 80 down:

      ASSIGN
        v2 = v2 + ar-cashl.amt-paid - ar-cashl.amt-disc
        lv-desc = ar-cashl.dscr.

      IF tb_posted THEN DO:

        IF lv-desc BEGINS "Credit -" THEN
          lv-desc = SUBSTR(lv-desc,10).
        ELSE IF lv-desc BEGINS "Debit -" THEN
          lv-desc = SUBSTR(lv-desc,9).
      END.

      display lv-desc ar-cashl.inv-no
              (ar-cashl.amt-paid - ar-cashl.amt-disc) @ ar-cashl.amt-paid
          with frame crdb-lines STREAM-IO.
      down with fram crdb-lines.
      v-printlines = v-printlines + 1.
      if v-printlines gt 26 then do:
        put "** CONTINUED **" at 35.
        page.
        assign v-printlines = 0.
      end.      

    end. /* each ar-cashl */

    if last-of(ar-cash.check-no) then do:
      put skip(27 - v-printlines) "Memo Amount" to 78 skip v2 to 78 skip.
      assign
       g1 = g1 + v1
       g2 = g2 + v2
       v1 = 0
       v2 = 0.
    end.

    find first reftable
        where reftable.reftable eq "AR-CASH"
          and reftable.code     eq string(ar-cash.c-no,"9999999999")
        use-index code no-error.
    if not avail reftable then do:
      create reftable.
      assign
       reftable.reftable = "AR-CASH"
       reftable.code     = string(ar-cash.c-no,"9999999999").
    end.

    /* gdm 07010903 */
    ASSIGN ar-cash.ret-memo = YES.

  end. /* each ar-cash */

  g2 = 0.
  OUTPUT CLOSE.

v-ftp-done = NO.
IF tb_export AND inexport-log THEN DO:    
   DEF VAR v-exp-file AS cha NO-UNDO.
   v-exp-file = inexport-desc +  
                "MEMO_" + trim(v-print-fmt) + 
                    substr(string(year(today),"9999"),3,2) +
                    string(month(today),"99") +
                    string(day(today),"99") +
                    substr(string(time,"HH:MM:SS"),1,2) +
                    substr(string(time,"HH:MM:SS"),4,2) +
                    substr(string(time,"HH:MM:SS"),7,2) + ".dat".
   OUTPUT TO VALUE(v-exp-file).
   IF inexport-cha EQ "CIT" THEN DO:
      RUN ar/rep/expfmemo.p .
      OUTPUT CLOSE.
      OUTPUT TO VALUE(".\ar\ftpcmd2.txt").     /* ftp text file */
      PUT UNFORMATTED 
       "open cs.ftp.citonline.com" SKIP  /* ftp server ip address */
       "ftpa1526" SKIP  /* userid*/
       "none" SKIP  /* password */
       "put " v-exp-file " " '"' "$$ ID=EP003F BID='DI1526' PASSWORD=NARF" '"' SKIP         /* file to transfer */
       "quit" .
      OUTPUT CLOSE.
      OS-COMMAND SILENT value("ftp -v -i -s:.\oe\ftpcmd2.txt"). 
      v-ftp-done = YES.
   END.
END.

FOR EACH report WHERE report.term-id EQ v-term-id: 
  DELETE report.
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Memo-Form C-Win 
PROCEDURE Set-Memo-Form :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR icFormName AS CHAR NO-UNDO.

ASSIGN
    icFormName = v-chrfld.

CASE icFormName:

    WHEN "AllWest" THEN
     ASSIGN
        v-program = "ar/rep/AlWstMem.p"
        lines-per-page = 66
        is-xprint-form = YES.

    WHEN "PremierPkg" THEN
     ASSIGN
        v-program = "ar/rep/prpkgmem.p"
        lines-per-page = 66
        is-xprint-form = YES. 
    WHEN "SouleMed" THEN
     ASSIGN
        v-program = "ar/rep/soulmemo.p"
        lines-per-page = 66
        is-xprint-form = YES.
    WHEN "Soule" THEN
     ASSIGN
        v-program = "ar/rep/soulpkmem.p"
        lines-per-page = 66
        is-xprint-form = YES.
    OTHERWISE
     ASSIGN
        v-program      = "ar/rep/crdbmemo.p"
        is-xprint-form = YES
        lines-per-page = 66.

END CASE.


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

  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

