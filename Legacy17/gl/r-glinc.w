&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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
DEF VAR v-invalid AS LOG NO-UNDO.
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


def new shared var v-ptd as date format "99/99/9999" initial today no-undo.
def new shared var v-s-cos-no like account.actnum no-undo.
def new shared var v-e-cos-no like account.actnum no-undo.
def new shared var v-s-oper-no like account.actnum no-undo.
def new shared var v-e-oper-no like account.actnum no-undo.
def new shared var v-s-gen-no like account.actnum no-undo.
def new shared var v-e-gen-no like account.actnum no-undo.
def new shared var v-s-inc-no like account.actnum no-undo.
def new shared var v-e-inc-no like account.actnum no-undo.
def new shared var v-s-oth-no like account.actnum no-undo.
def new shared var v-e-oth-no like account.actnum no-undo.

def var ptd-sales as dec format "->>,>>>,>>9.99" no-undo.
def var ytd-sales as dec format "->>,>>>,>>9.99" no-undo.
def var ptd-cos as dec format "->>,>>>,>>9.99" no-undo.
def var ytd-cos as dec format "->>,>>>,>>9.99" no-undo.
def var ptd-oper as dec format "->>,>>>,>>9.99" no-undo.
def var ytd-oper as dec format "->>,>>>,>>9.99" no-undo.
def var ptd-gen as dec format "->>,>>>,>>9.99" no-undo.
def var ytd-gen as dec format "->>,>>>,>>9.99" no-undo.
def var ptd-inc as dec format "->>,>>>,>>9.99" no-undo.
def var ytd-inc as dec format "->>,>>>,>>9.99" no-undo.
def var ptd-oth as dec format "->>,>>>,>>9.99" no-undo.
def var ytd-oth as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ptd-sales as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ytd-sales as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ptd-cos as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ytd-cos as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ptd-oper as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ytd-oper as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ptd-gen as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ytd-gen as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ptd-inc as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ytd-inc as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ptd-oth as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ytd-oth as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ptd-gross as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ytd-gross as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ptd-exp as dec format "->>,>>>,>>9.99" no-undo.
def var tot-ytd-exp as dec format "->>,>>>,>>9.99" no-undo.
def var v-ptd-per as dec format "->>9.99" no-undo.
def var v-ytd-per as dec format "->>9.99" no-undo.
def var pre-close as logical initial no no-undo.
def var v-year like period.yr.
def var v-period like period.pnum.
def var per-loop as int no-undo.

def buffer xperiod for period.

def var save_id as recid no-undo.
def var time_stamp as ch.
time_stamp = string(time, "HH:MMam").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tran-date tb_pre beg_acct-1 end_acct-1 ~
beg_acct-2 end_acct-2 beg_acct-3 end_acct-3 beg_acct-4 end_acct-4 ~
beg_acct-5 end_acct-5 rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm btn-ok btn-cancel RECT-11 RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period lbl_pre tb_pre ~
beg_acct-1 end_acct-1 beg_acct-2 end_acct-2 beg_acct-3 end_acct-3 ~
beg_acct-4 end_acct-4 beg_acct-5 end_acct-5 rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE beg_acct-1 AS CHARACTER FORMAT "X(25)":U 
     LABEL "Cost of Sales" 
     VIEW-AS FILL-IN 
     SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE beg_acct-2 AS CHARACTER FORMAT "X(25)":U 
     LABEL "Operating Expense" 
     VIEW-AS FILL-IN 
     SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE beg_acct-3 AS CHARACTER FORMAT "X(25)":U 
     LABEL "General & Admin" 
     VIEW-AS FILL-IN 
     SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE beg_acct-4 AS CHARACTER FORMAT "X(25)":U 
     LABEL "Income Expense" 
     VIEW-AS FILL-IN 
     SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE beg_acct-5 AS CHARACTER FORMAT "X(25)":U 
     LABEL "Other Expense" 
     VIEW-AS FILL-IN 
     SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct-1 AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct-2 AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct-3 AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct-4 AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct-5 AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_pre AS CHARACTER FORMAT "X(256)":U INITIAL "Pre Close Period?" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

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
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 101 BY 6.91.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 102 BY 6.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 103 BY 13.33.

DEFINE VARIABLE tb_pre AS LOGICAL INITIAL yes 
     LABEL "Pre Close Period?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.67 COL 39 COLON-ALIGNED
     tran-period AT ROW 3.62 COL 39 COLON-ALIGNED
     lbl_pre AT ROW 4.81 COL 20 COLON-ALIGNED NO-LABEL
     tb_pre AT ROW 4.81 COL 41
     beg_acct-1 AT ROW 8.62 COL 20 COLON-ALIGNED
     end_acct-1 AT ROW 8.62 COL 58 COLON-ALIGNED NO-LABEL
     beg_acct-2 AT ROW 9.57 COL 20 COLON-ALIGNED
     end_acct-2 AT ROW 9.57 COL 58 COLON-ALIGNED NO-LABEL
     beg_acct-3 AT ROW 10.52 COL 20 COLON-ALIGNED
     end_acct-3 AT ROW 10.52 COL 58 COLON-ALIGNED NO-LABEL
     beg_acct-4 AT ROW 11.48 COL 20 COLON-ALIGNED
     end_acct-4 AT ROW 11.48 COL 58 COLON-ALIGNED NO-LABEL
     beg_acct-5 AT ROW 12.43 COL 20 COLON-ALIGNED
     end_acct-5 AT ROW 12.43 COL 58 COLON-ALIGNED NO-LABEL
     rd-dest AT ROW 16.24 COL 7 NO-LABEL
     lv-ornt AT ROW 16.48 COL 33 NO-LABEL
     lines-per-page AT ROW 16.48 COL 86 COLON-ALIGNED
     lv-font-no AT ROW 17.91 COL 36 COLON-ALIGNED
     lv-font-name AT ROW 18.86 COL 30 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 20.29 COL 7
     btn-ok AT ROW 22.43 COL 20
     btn-cancel AT ROW 22.43 COL 58
     RECT-11 AT ROW 6.95 COL 2
     RECT-6 AT ROW 14.81 COL 2
     RECT-7 AT ROW 1 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "  Account Ranges" VIEW-AS TEXT
          SIZE 21 BY .95 AT ROW 6.48 COL 48
          FGCOLOR 1 FONT 6
     "From Account#" VIEW-AS TEXT
          SIZE 17 BY .71 AT ROW 7.67 COL 31
     "To Account#" VIEW-AS TEXT
          SIZE 15 BY .71 AT ROW 7.67 COL 71
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.29 COL 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 104 BY 24.


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
         TITLE              = "GL Financial Statements"
         HEIGHT             = 24.24
         WIDTH              = 105
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


/* SETTINGS FOR FILL-IN lbl_pre IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_pre:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tran-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tran-period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* GL Financial Statements */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* GL Financial Statements */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON HELP OF FRAME FRAME-A
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR lk-recid AS RECID NO-UNDO.

    CASE FOCUS:NAME :
        WHEN "beg_acct-1"  OR WHEN "beg_acct-2" OR WHEN "beg_acct-3" OR WHEN "beg_acct-4" OR WHEN "beg_acct-5" OR
        WHEN "end_acct-1"  OR WHEN "end_acct-2" OR WHEN "end_acct-3" OR WHEN "end_acct-4" OR WHEN "end_acct-5"
        THEN DO:
             RUN windows/l-acct.w (g_company,"E",FOCUS:SCREEN-VALUE, OUTPUT char-val).
             IF char-val <> "" THEN DO:
                 ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                        .
             END.
        END.
    END CASE.

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
  assign rd-dest
         tran-date
         tran-period
         tb_pre
         beg_acct-1 END_acct-1
         beg_acct-2 END_acct-2
         beg_acct-3 END_acct-3
         beg_acct-4 END_acct-4
         beg_acct-5 END_acct-5
         .

  run check-date.
  if v-invalid then return no-apply.       

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


&Scoped-define SELF-NAME tb_pre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_pre C-Win
ON VALUE-CHANGED OF tb_pre IN FRAME FRAME-A /* Pre Close Period? */
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

 find first company where company.company = cocode no-lock no-error.  
  TRAN-date = TODAY.

  RUN init-proc.

  RUN enable_UI.
  RUN check-date.

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

    ELSE DO:
      message "No Defined Period Exists for" tran-date view-as alert-box error.
      v-invalid = yes.
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
  DISPLAY tran-date tran-period lbl_pre tb_pre beg_acct-1 end_acct-1 beg_acct-2 
          end_acct-2 beg_acct-3 end_acct-3 beg_acct-4 end_acct-4 beg_acct-5 
          end_acct-5 rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE tran-date tb_pre beg_acct-1 end_acct-1 beg_acct-2 end_acct-2 
         beg_acct-3 end_acct-3 beg_acct-4 end_acct-4 beg_acct-5 end_acct-5 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok 
         btn-cancel RECT-11 RECT-6 RECT-7 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc C-Win 
PROCEDURE init-proc :
/*------------------------------------------------------------------------------
  Purpose:     gl/gl-inc1.p
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN gl/gl-inc1.p.


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
/*     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
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
 */
 run CUSTOM\PRNTPROC.P (list-name,INT(LV-FONT-NO),LV-ORNT). /* open file-name, title */ 

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
  run scr-rpt.w (list-name,c-win:title,INT(LV-FONT-NO),LV-ORNT). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{sys/form/r-top3.f}

FORM HEADER
  skip(1)
  "PTD Post" to 40 "%Sales" to 50
  "YTD Post" to 65 "%Sales" to 75
  "========" to 40 "======" to 50
  "========" to 65 "======" to 75
  with frame head-columns no-box no-labels width 80 PAGE-TOP STREAM-IO.

FORM
  account.dscr at 1 format "x(25)"
  ptd-sales to 40
  v-ptd-per to 50
  ytd-sales to 65
  v-ytd-per to 75
  with frame line-item down no-box no-labels width 80 STREAM-IO.

  assign v-ptd = tran-date
         pre-close = tb_pre
         v-s-cos-no = beg_acct-1
         v-e-cos-no = END_acct-1
         v-s-oper-no = beg_acct-2
         v-e-oper-no = END_acct-2
         v-s-gen-no  = beg_acct-3
         v-e-gen-no = END_acct-3
         v-s-inc-no = beg_acct-4
         v-e-inc-no = END_acct-4
         v-s-oth-no = beg_acct-5
         v-e-oth-no = END_acct-5
         .

{sys/inc/print1.i}
 {sys/inc/outprint.i VALUE(lines-per-page)}

 IF td-show-parm THEN RUN show-param.
 SESSION:SET-WAIT-STATE("general").
 tot-ytd-sales = 0.
 tot-ptd-sales = 0.
 ptd-sales = 0.
 ytd-sales = 0.
 tot-ptd-cos = 0.
 tot-ytd-cos = 0.
 tot-ptd-gross = 0.
 tot-ytd-gross = 0.
 tot-ptd-gen = 0.
 tot-ytd-gen = 0.

  find first period where period.company = cocode and
                          period.pst <= v-ptd and
                          period.pend >= v-ptd no-lock no-error.
  if avail period then
    assign v-year = period.yr
           v-period = period.pnum.

   str-tit  = coname + " - " + loname.
   str-tit2 = "INCOME STATEMENT" .

   if pre-close then
     str-tit3 = "From " + string(period.pst) + "  Thru " + string(v-ptd).
   else
     str-tit3 = "From " + string(period.pst) + "  Thru " + string(period.pend).

   x = (56 - length(str-tit)) / 2.
   str-tit  = fill(" ",x) + str-tit .
   x = (57 - length(str-tit2)) / 2.
   str-tit2 = fill(" ",x) + str-tit2 .
   x = (78 - length(str-tit3)) / 2.
   str-tit3 = fill(" ",x) + str-tit3 .

  view frame r-top.
  view frame head-columns.

  find first period where period.company = cocode and
			  period.pst <= v-ptd and
			  period.pend >= v-ptd no-lock no-error.
    if avail period then
    assign v-year = period.yr
	   v-period = period.pnum.

   str-tit  = coname + " - " + loname.
   str-tit2 = "INCOME STATEMENT" .

   if pre-close then
     str-tit3 = "From " + string(period.pst) + "  Thru " + string(v-ptd).
   else
     str-tit3 = "From " + string(period.pst) + "  Thru " + string(period.pend).

   x = (56 - length(str-tit)) / 2.
   str-tit  = fill(" ",x) + str-tit .
   x = (57 - length(str-tit2)) / 2.
   str-tit2 = fill(" ",x) + str-tit2 .
   x = (78 - length(str-tit3)) / 2.
   str-tit3 = fill(" ",x) + str-tit3 .

  view frame r-top.
  view frame head-columns.

  /*  Sales Totals */
  for each account where account.company eq cocode and
			 account.type eq "R" use-index type no-lock:

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum
          AND glhist.period  EQ v-period
          AND glhist.tr-date GE period.pst
          AND glhist.tr-date LE period.pend:
      tot-ptd-sales = tot-ptd-sales + glhist.tr-amt.
    END.

    if pre-close then
    do:

      for each gltrans no-lock where gltrans.actnum eq account.actnum and
				     gltrans.company eq cocode:
	if gltrans.tr-date le v-ptd and
	   gltrans.tr-date >= period.pst and
	   gltrans.tr-date <= period.pend then
	do:
	  if gltrans.period <> period.pnum then
	    next.
	  assign tot-ptd-sales = tot-ptd-sales + gltrans.tr-amt.
	end.
      end.
    end.

    do per-loop = 1 to (v-period - 1):
      find first xperiod where xperiod.company = cocode and
			      xperiod.pnum = per-loop and
			      xperiod.yr = v-year
			      no-lock no-error.
      IF AVAIL xperiod THEN
	  FOR EACH glhist NO-LOCK
          WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ per-loop
		    AND glhist.tr-date GE xperiod.pst
	        AND glhist.tr-date LE xperiod.pend:
	    tot-ytd-sales = tot-ytd-sales + glhist.tr-amt.
	  END.
    end.

    assign tot-ytd-sales = tot-ytd-sales + account.cyr-open.

  end.  /* Sales Totals for each */

  assign tot-ytd-sales = tot-ytd-sales + tot-ptd-sales
	 tot-ptd-sales = - tot-ptd-sales
	 tot-ytd-sales = - tot-ytd-sales.

  put "========  Sales  ========" skip(1).

  /*  Sales Totals */
  for each account where account.company eq cocode and
			 account.type eq "R" no-lock
			 use-index type break by account.actnum:

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum
          AND glhist.period  EQ v-period
          AND glhist.tr-date GE period.pst
		  AND glhist.tr-date LE period.pend:
      ptd-sales = ptd-sales + glhist.tr-amt.
    END.


    if pre-close then
    do:
      for each gltrans no-lock where gltrans.actnum = account.actnum and
				     gltrans.company = cocode:
	if gltrans.tr-date le v-ptd and
	     gltrans.tr-date >= period.pst and
	     gltrans.tr-date <= period.pend then
	do:
	  if gltrans.period <> period.pnum then
	    next.
	  assign ptd-sales = ptd-sales + gltrans.tr-amt.
	end.
      end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
	    find first xperiod where xperiod.company = cocode and
		   		   xperiod.pnum = per-loop and
				   xperiod.yr = v-year
				  no-lock no-error.
	    IF AVAIL xperiod THEN
	    FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum
              AND glhist.period  EQ per-loop
		      AND glhist.tr-date GE xperiod.pst
	          AND glhist.tr-date LE xperiod.pend:
	      ytd-sales = ytd-sales + glhist.tr-amt.
	    END.
      END.

      assign v-ptd-per = ((- ptd-sales / tot-ptd-sales) * 100)
	     ytd-sales = ytd-sales + ptd-sales + account.cyr-open
	     v-ytd-per = ((- ytd-sales / tot-ytd-sales) * 100).
      display account.dscr (- ptd-sales) @ ptd-sales v-ptd-per
			   (- ytd-sales) @ ytd-sales v-ytd-per
	with frame line-item overlay down.
      down with frame line-item.
      assign ptd-sales = 0
	     ytd-sales = 0.
    end.
  end.  /* Sales Totals for each */

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Sales" at 1
      tot-ptd-sales to 40
      100.00 to 50
      tot-ytd-sales to 65
      100.00 to 75 skip(1).
  put "====  Cost of Sales  ====" skip(1).

  /*  Cost of Sales */
  for each account where account.company eq cocode and
			 account.actnum >= v-s-cos-no and
			 account.actnum <= v-e-cos-no
			 no-lock use-index account break by account.actnum:

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum
          AND glhist.period  EQ v-period
		  AND glhist.tr-date GE period.pst
		  AND glhist.tr-date LE period.pend:
      ptd-cos = ptd-cos + glhist.tr-amt.
    END.

    if pre-close then
    do:
	for each gltrans no-lock where gltrans.actnum = account.actnum and
				       gltrans.company = cocode:
	  if gltrans.tr-date le v-ptd and
	     gltrans.tr-date >= period.pst and
	     gltrans.tr-date <= period.pend then
	  do:
	    if gltrans.period <> period.pnum then
	      next.
	    assign ptd-cos = ptd-cos + gltrans.tr-amt.
	  end.
	end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
	    find first xperiod where xperiod.company = cocode and
		    	   xperiod.pnum = per-loop and
				   xperiod.yr = v-year
				  no-lock no-error.
	    IF AVAIL xperiod THEN
	    FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum
              AND glhist.period  EQ per-loop
		      AND glhist.tr-date GE xperiod.pst
		      AND glhist.tr-date LE xperiod.pend:
	      ytd-cos = ytd-cos + glhist.tr-amt.
	    END.
      END.

      assign v-ptd-per = ((ptd-cos / tot-ptd-sales) * 100)
	     ytd-cos = ytd-cos + ptd-cos + account.cyr-open
	     v-ytd-per = ((ytd-cos / tot-ytd-sales) * 100).
      display account.dscr ptd-cos @ ptd-sales v-ptd-per
			   ytd-cos @ ytd-sales v-ytd-per
	with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-cos = tot-ptd-cos + ptd-cos
	     tot-ytd-cos = tot-ytd-cos + ytd-cos
	     ptd-cos = 0
	     ytd-cos = 0.
    end.
  end.  /* Cost of Sales */

  assign tot-ptd-gross = tot-ptd-sales - tot-ptd-cos
	 tot-ytd-gross = tot-ytd-sales - tot-ytd-cos.

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Cost of Sales" at 1 tot-ptd-cos to 40
       ((tot-ptd-cos / tot-ptd-sales) * 100.00) to 50
      tot-ytd-cos to 65
       ((tot-ytd-cos / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Gross Margin" tot-ptd-gross to 40
      ((tot-ptd-gross / tot-ptd-sales) * 100) to 50
      tot-ytd-gross to 65
      ((tot-ytd-gross / tot-ytd-sales) * 100) to 75 skip(1).
  put "===  Operating Expenses  ===" skip(1).

  /*  Operating Expenses */
  for each account where account.company eq cocode and
			 account.actnum >= v-s-oper-no and
			 account.actnum <= v-e-oper-no
			 no-lock use-index account break by account.actnum:

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum
          AND glhist.period  EQ v-period
		  AND glhist.tr-date GE period.pst
		  AND glhist.tr-date LE period.pend:
      ptd-oper = ptd-oper + glhist.tr-amt.
    END.

    if pre-close then
    do:
	for each gltrans no-lock where gltrans.actnum = account.actnum and
				       gltrans.company = cocode:
	  if gltrans.tr-date le v-ptd and
	     gltrans.tr-date >= period.pst and
	     gltrans.tr-date <= period.pend then
	  do:
	    if gltrans.period <> period.pnum then
	      next.
	    assign ptd-oper = ptd-oper + gltrans.tr-amt.
	  end.
	end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
	    find first xperiod where xperiod.company = cocode and
				   xperiod.pnum = per-loop and
				   xperiod.yr = v-year
				  no-lock no-error.
	    IF AVAIL xperiod THEN
	    FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum
              AND glhist.period  EQ per-loop
			  AND glhist.tr-date GE xperiod.pst
			  AND glhist.tr-date LE xperiod.pend:
   	      ytd-oper = ytd-oper + glhist.tr-amt.
	    END.
      END.

      assign v-ptd-per = ((ptd-oper / tot-ptd-sales) * 100)
	     ytd-oper = ytd-oper + ptd-oper + account.cyr-open
	     v-ytd-per = ((ytd-oper / tot-ytd-sales) * 100).
      display account.dscr ptd-oper @ ptd-sales v-ptd-per
			   ytd-oper @ ytd-sales v-ytd-per
	with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-oper = tot-ptd-oper + ptd-oper
	     tot-ytd-oper = tot-ytd-oper + ytd-oper
	     ptd-oper = 0
	     ytd-oper = 0.
    end.
  end.  /* Operating Expenses */

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Operating Expenses" at 1 tot-ptd-oper to 40
       ((tot-ptd-oper / tot-ptd-sales) * 100.00) to 50
	tot-ytd-oper to 65
       ((tot-ytd-oper / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "General & Administrative" skip(1).

  /*  General/Admin Expenses */
  for each account where account.company eq cocode and
			 account.actnum >= v-s-gen-no and
			 account.actnum <= v-e-gen-no
			 no-lock use-index account break by account.actnum:

    find first period where period.company = cocode and
			    period.pst <= v-ptd and
			    period.pend >= v-ptd no-lock no-error.

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum 
          AND glhist.period  EQ v-period
		  AND glhist.tr-date GE period.pst
		  AND glhist.tr-date LE period.pend:
      ptd-gen = ptd-gen + glhist.tr-amt.
    END.

    if pre-close then
    do:
	for each gltrans no-lock where gltrans.actnum = account.actnum and
				       gltrans.company = cocode:
	  if gltrans.tr-date le v-ptd and
	     gltrans.tr-date >= period.pst and
	     gltrans.tr-date <= period.pend then
	  do:
	    if gltrans.period <> period.pnum then
	      next.
	    assign ptd-gen = ptd-gen + gltrans.tr-amt.
	  end.
	end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
	    find first xperiod where xperiod.company = cocode and
				  xperiod.pnum = per-loop and
				  xperiod.yr = v-year
				  no-lock no-error.
	    IF AVAIL xperiod THEN
	    FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum
              AND glhist.period  EQ per-loop
			  AND glhist.tr-date GE xperiod.pst
			  AND glhist.tr-date LE xperiod.pend:
	      ytd-gen = ytd-gen + glhist.tr-amt.
	    END.
      end.

      assign v-ptd-per = ((ptd-gen / tot-ptd-sales) * 100)
	     ytd-gen = ytd-gen + ptd-gen + account.cyr-open
	     v-ytd-per = ((ytd-gen / tot-ytd-sales) * 100).
      display account.dscr ptd-gen @ ptd-sales v-ptd-per
			   ytd-gen @ ytd-sales v-ytd-per
	with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-gen = tot-ptd-gen + ptd-gen
	     tot-ytd-gen = tot-ytd-gen + ytd-gen
	     ptd-gen = 0
	     ytd-gen = 0.
    end.
  end.  /* Gen & Admin Expenses */

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total General & Admin" at 1 tot-ptd-gen to 40
       ((tot-ptd-gen / tot-ptd-sales) * 100.00) to 50
	tot-ytd-gen to 65
       ((tot-ytd-gen / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "Income Tax Expenses" skip(1).

  /*  Income Tax Expenses */
  for each account where account.company eq cocode and
			 account.actnum >= v-s-inc-no and
			 account.actnum <= v-e-inc-no
			 no-lock use-index account break by account.actnum:

    find first period where period.company = cocode and
			    period.pst <= v-ptd and
			    period.pend >= v-ptd no-lock no-error.

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum
          AND glhist.period  EQ v-period
		  AND glhist.tr-date GE period.pst
		  AND glhist.tr-date LE period.pend:
      ptd-inc = ptd-inc + glhist.tr-amt.
    END.

    if pre-close then
    do:
      for each gltrans no-lock where gltrans.actnum = account.actnum and
				       gltrans.company = cocode:
	if gltrans.tr-date le v-ptd and
	   gltrans.tr-date >= period.pst and
	   gltrans.tr-date <= period.pend then
	do:
	  if gltrans.period <> period.pnum then
	    next.
	  assign ptd-inc = ptd-inc + gltrans.tr-amt.
	end.
      end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
	    FIND FIRST xperiod where xperiod.company = cocode and
				  xperiod.pnum = per-loop and
				  xperiod.yr = v-year
				  no-lock no-error.
	    IF AVAIL xperiod THEN
	    FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum
              AND glhist.period  EQ per-loop
			  AND glhist.tr-date GE xperiod.pst
			  AND glhist.tr-date LE xperiod.pend:
	      ytd-inc = ytd-inc + glhist.tr-amt.
	    END.
      END.

      assign v-ptd-per = ((ptd-inc / tot-ptd-sales) * 100)
	     ytd-inc = ytd-inc + ptd-inc + account.cyr-open
	     v-ytd-per = ((ytd-inc / tot-ytd-sales) * 100).
      display account.dscr ptd-inc @ ptd-sales v-ptd-per
			   ytd-inc @ ytd-sales v-ytd-per
	with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-inc = tot-ptd-inc + ptd-inc
	     tot-ytd-inc = tot-ytd-inc + ytd-inc
	     ptd-inc = 0
	     ytd-inc = 0.
    end.
  end.  /* Operating Expenses */

  assign tot-ptd-exp = tot-ptd-oper + tot-ptd-gen + tot-ptd-inc
	 tot-ytd-exp = tot-ytd-oper + tot-ytd-gen + tot-ytd-inc.

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Income Tax Expense" at 1 tot-ptd-inc to 40
       ((tot-ptd-inc / tot-ptd-sales) * 100.00) to 50
	tot-ytd-inc to 65
       ((tot-ytd-inc / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Operating Expenses" at 1
       tot-ptd-exp to 40
       ((tot-ptd-exp / tot-ptd-sales) * 100.00) to 50
       tot-ytd-exp to 65
       ((tot-ytd-exp / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "Net Income Before Taxes"
      (tot-ptd-gross - tot-ptd-exp) to 40 format "->>,>>>,>>9.99"
      (((tot-ptd-gross - tot-ptd-exp) / tot-ptd-sales) * 100.00) to 50
      (tot-ytd-gross - tot-ytd-exp) to 65 format "->>,>>>,>>9.99"
      (((tot-ytd-gross - tot-ytd-exp) / tot-ytd-sales) * 100.00) to 75 skip.
  put "==============" to 40 "=======" to 50
      "==============" to 65 "=======" to 75 skip(1).

  /*  Other Expenses */
  for each account where account.company eq cocode and
			 account.actnum >= v-s-oth-no and
			 account.actnum <= v-e-oth-no
			 no-lock use-index account break by account.actnum:

    find first period where period.company = cocode and
			    period.pst <= v-ptd and
			    period.pend >= v-ptd no-lock no-error.

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum
          AND glhist.period  EQ v-period 
		  AND glhist.tr-date GE period.pst
		  AND glhist.tr-date LE period.pend:
      ptd-oth = ptd-oth + glhist.tr-amt.
    END.

    if pre-close then
    do:
      for each gltrans no-lock where gltrans.actnum = account.actnum and
				       gltrans.company = cocode:
	if gltrans.tr-date le v-ptd and
	   gltrans.tr-date >= period.pst and
	   gltrans.tr-date <= period.pend then
	do:
	  if gltrans.period <> period.pnum then
	    next.
	  assign ptd-oth = ptd-oth + gltrans.tr-amt.
	end.
      end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
	    find first xperiod where xperiod.company = cocode and
				  xperiod.pnum = per-loop and
				  xperiod.yr = v-year
				  no-lock no-error.
	    IF AVAIL xperiod THEN
	    FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum
              AND glhist.period  EQ per-loop
			  AND glhist.tr-date GE xperiod.pst
			  AND glhist.tr-date LE xperiod.pend:
	      ytd-oth = ytd-oth + glhist.tr-amt.
	    END.
      END.

      assign v-ptd-per = ((ptd-oth / tot-ptd-sales) * 100)
	     ytd-oth = ytd-oth + ptd-oth + account.cyr-open
	     v-ytd-per = ((ytd-oth / tot-ytd-sales) * 100).
      display account.dscr ptd-oth @ ptd-sales v-ptd-per
			   ytd-oth @ ytd-sales v-ytd-per
	with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-oth = tot-ptd-oth + ptd-oth
	     tot-ytd-oth = tot-ytd-oth + ytd-oth
	     ptd-oth = 0
	     ytd-oth = 0.
    end.
  end.  /* Other Expenses */

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.

  put "Net Income After Taxes"
      ((tot-ptd-gross - tot-ptd-exp) - tot-ptd-oth) to 40 format "->>,>>>,>>9.99"
      ((((tot-ptd-gross - tot-ptd-exp) - tot-ptd-oth) / tot-ptd-sales) * 100.00) to 50
      ((tot-ytd-gross - tot-ytd-exp) - tot-ytd-oth) to 65 format "->>,>>>,>>9.99"
      ((((tot-ytd-gross - tot-ytd-exp) - tot-ytd-oth) / tot-ytd-sales) * 100.00) to 75 skip.
  put "==============" to 40 "=======" to 50
      "==============" to 65 "=======" to 75 skip. 

  SESSION:SET-WAIT-STATE("").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sdf C-Win 
PROCEDURE sdf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{sys/form/r-top3.f}

FORM HEADER
  skip(1)
  "PTD Post" to 40 "%Sales" to 50
  "YTD Post" to 65 "%Sales" to 75
  "========" to 40 "======" to 50
  "========" to 65 "======" to 75
  with frame head-columns no-box no-labels width 80 PAGE-TOP STREAM-IO.

FORM
  account.dscr at 1 format "x(25)"
  ptd-sales to 40
  v-ptd-per to 50
  ytd-sales to 65
  v-ytd-per to 75
  with frame line-item down no-box no-labels width 80 STREAM-IO.

  assign v-ptd = tran-date
         pre-close = tb_pre
             v-s-cos-no = beg_acct-1
         v-e-cos-no = END_acct-1
         v-s-oper-no = beg_acct-2
         v-e-oper-no = END_acct-2
             v-s-gen-no  = beg_acct-3
         v-e-gen-no = END_acct-3
             v-s-inc-no = beg_acct-4
         v-e-inc-no = END_acct-4
             v-s-oth-no = beg_acct-5
         v-e-oth-no = END_acct-5
         .

{sys/inc/print1.i}
 {sys/inc/outprint.i VALUE(lines-per-page)}

 IF td-show-parm THEN RUN show-param.
 SESSION:SET-WAIT-STATE("general").
 tot-ytd-sales = 0.
 tot-ptd-sales = 0.
 ptd-sales = 0.
 ytd-sales = 0.
 tot-ptd-cos = 0.
 tot-ytd-cos = 0.
 tot-ptd-gross = 0.
 tot-ytd-gross = 0.
 tot-ptd-gen = 0.
 tot-ytd-gen = 0.

  find first period where period.company = cocode and
                          period.pst <= v-ptd and
                          period.pend >= v-ptd no-lock no-error.
  if avail period then
    assign v-year = period.yr
           v-period = period.pnum.

   str-tit  = coname + " - " + loname.
   str-tit2 = "INCOME STATEMENT" .

   if pre-close then
     str-tit3 = "From " + string(period.pst) + "  Thru " + string(v-ptd).
   else
     str-tit3 = "From " + string(period.pst) + "  Thru " + string(period.pend).

   x = (56 - length(str-tit)) / 2.
   str-tit  = fill(" ",x) + str-tit .
   x = (57 - length(str-tit2)) / 2.
   str-tit2 = fill(" ",x) + str-tit2 .
   x = (78 - length(str-tit3)) / 2.
   str-tit3 = fill(" ",x) + str-tit3 .

  view frame r-top.
  view frame head-columns.

  /*  Sales Totals */
  for each account where account.company eq cocode and
                         account.type eq "R" use-index type no-lock:

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum
          AND glhist.period  EQ v-period
          AND glhist.tr-date GE period.pst
          AND glhist.tr-date LE period.pend:
      tot-ptd-sales = tot-ptd-sales + glhist.tr-amt.
    END.

    if pre-close then
    do:

      for each gltrans no-lock where gltrans.actnum eq account.actnum and
                                     gltrans.company eq cocode:
        if gltrans.tr-date le v-ptd and
           gltrans.tr-date >= period.pst and
           gltrans.tr-date <= period.pend then
        do:
          if gltrans.period <> period.pnum then
            next.
          assign tot-ptd-sales = tot-ptd-sales + gltrans.tr-amt.
        end.
      end.
    end.

    do per-loop = 1 to (v-period - 1):
      find first xperiod where xperiod.company = cocode and
                              xperiod.pnum = per-loop and
                              xperiod.yr = v-year
                              no-lock no-error.
      IF AVAIL xperiod THEN
      FOR EACH glhist NO-LOCK
          WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ per-loop
            AND glhist.tr-date GE xperiod.pst
            AND glhist.tr-date LE xperiod.pend:
          tot-ytd-sales = tot-ytd-sales + glhist.tr-amt.
      END.
    end.

    assign tot-ytd-sales = tot-ytd-sales + account.cyr-open.

  end.  /* Sales Totals for each */

  assign tot-ytd-sales = tot-ytd-sales + tot-ptd-sales
         tot-ptd-sales = - tot-ptd-sales
         tot-ytd-sales = - tot-ytd-sales.

  put "========  Sales  ========" skip(1).

  /*  Sales Totals */
  for each account where account.company eq cocode and
                         account.type eq "R" no-lock
                         use-index type break by account.actnum:

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum
          AND glhist.period  EQ v-period
          AND glhist.tr-date GE period.pst
          AND glhist.tr-date LE period.pend:
      ptd-sales = ptd-sales + glhist.tr-amt.
    END.

    if pre-close then
    do:
      for each gltrans no-lock where gltrans.actnum = account.actnum and
                                     gltrans.company = cocode:
        if gltrans.tr-date le v-ptd and
             gltrans.tr-date >= period.pst and
             gltrans.tr-date <= period.pend then
        do:
          if gltrans.period <> period.pnum then
            next.
          assign ptd-sales = ptd-sales + gltrans.tr-amt.
        end.
      end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
        find first xperiod where xperiod.company = cocode and
                                  xperiod.pnum = per-loop and
                                  xperiod.yr = v-year
                                  no-lock no-error.
        IF AVAIL xperiod THEN
        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum
              AND glhist.period  EQ per-loop
              AND glhist.tr-date GE xperiod.pst
              AND glhist.tr-date LE xperiod.pend:
          ytd-sales = ytd-sales + glhist.tr-amt.
        END.
      END.

      assign v-ptd-per = IF tot-ptd-sales <> 0 THEN ((- ptd-sales / tot-ptd-sales) * 100)
                         ELSE 0
             ytd-sales = ytd-sales + ptd-sales + account.cyr-open
             v-ytd-per = IF tot-ytd-sales <> 0 THEN ((- ytd-sales / tot-ytd-sales) * 100)
                         ELSE 0.

      display account.dscr (- ptd-sales) @ ptd-sales v-ptd-per
                           (- ytd-sales) @ ytd-sales v-ytd-per
        with frame line-item overlay DOWN STREAM-IO.     
      down with frame line-item.
      assign ptd-sales = 0
             ytd-sales = 0.
    end.
  end.  /* Sales Totals for each */

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Sales" at 1
      tot-ptd-sales to 40
      100.00 to 50
      tot-ytd-sales to 65
      100.00 to 75 skip(1).
  put "====  Cost of Sales  ====" skip(1).

  /*  Cost of Sales */
  for each account where account.company eq cocode and
                         account.actnum >= v-s-cos-no and
                         account.actnum <= v-e-cos-no
                         no-lock use-index account break by account.actnum:

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum
          AND glhist.period  EQ v-period
          AND glhist.tr-date GE period.pst
          AND glhist.tr-date LE period.pend:
      ptd-cos = ptd-cos + glhist.tr-amt.
    END.

    if pre-close then
    do:
        for each gltrans no-lock where gltrans.actnum = account.actnum and
                                       gltrans.company = cocode:
          if gltrans.tr-date le v-ptd and
             gltrans.tr-date >= period.pst and
             gltrans.tr-date <= period.pend then
          do:
            if gltrans.period <> period.pnum then
              next.
            assign ptd-cos = ptd-cos + gltrans.tr-amt.
          end.
        end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
        find first xperiod where xperiod.company = cocode and
                                  xperiod.pnum = per-loop and
                                  xperiod.yr = v-year
                                  no-lock no-error.
        IF AVAIL xperiod THEN
        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum
              AND glhist.period  EQ per-loop
              AND glhist.tr-date GE xperiod.pst
              AND glhist.tr-date LE xperiod.pend:
          ytd-cos = ytd-cos + glhist.tr-amt.
        END.
      end.

      assign v-ptd-per = ((ptd-cos / tot-ptd-sales) * 100)
             ytd-cos = ytd-cos + ptd-cos + account.cyr-open
             v-ytd-per = ((ytd-cos / tot-ytd-sales) * 100).
      IF v-ptd-per = ? THEN v-ptd-per = 0.
      IF v-ytd-per = ? THEN v-ytd-per = 0.
      display account.dscr ptd-cos @ ptd-sales v-ptd-per
                           ytd-cos @ ytd-sales v-ytd-per
        with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-cos = tot-ptd-cos + ptd-cos
             tot-ytd-cos = tot-ytd-cos + ytd-cos
             ptd-cos = 0
             ytd-cos = 0.
    end.
  end.  /* Cost of Sales */
  assign tot-ptd-gross = tot-ptd-sales - tot-ptd-cos
      tot-ytd-gross = tot-ytd-sales - tot-ytd-cos.

put "--------------" to 40 "-------" to 50
   "--------------" to 65 "-------" to 75 skip.
put "Total Cost of Sales" at 1 tot-ptd-cos to 40
    ((tot-ptd-cos / tot-ptd-sales) * 100.00) to 50
   tot-ytd-cos to 65
    ((tot-ytd-cos / tot-ytd-sales) * 100.00) to 75 skip(1).
put "--------------" to 40 "-------" to 50
   "--------------" to 65 "-------" to 75 skip.
put "Gross Margin" tot-ptd-gross to 40
   ((tot-ptd-gross / tot-ptd-sales) * 100) to 50
   tot-ytd-gross to 65
   ((tot-ytd-gross / tot-ytd-sales) * 100) to 75 skip(1).
put "===  Operating Expenses  ===" skip(1).

/*  Operating Expenses */
for each account where account.company eq cocode and
                      account.actnum >= v-s-oper-no and
                      account.actnum <= v-e-oper-no
                      no-lock use-index account break by account.actnum:

  FOR EACH glhist NO-LOCK
      WHERE glhist.company EQ account.company
        AND glhist.actnum  EQ account.actnum
        AND glhist.period  EQ v-period
        AND glhist.tr-date GE period.pst
        AND glhist.tr-date LE period.pend:
    ptd-oper = ptd-oper + glhist.tr-amt.
  END.

 if pre-close then
 do:
     for each gltrans no-lock where gltrans.actnum = account.actnum and
                                    gltrans.company = cocode:
       if gltrans.tr-date le v-ptd and
          gltrans.tr-date >= period.pst and
          gltrans.tr-date <= period.pend then
       do:
         if gltrans.period <> period.pnum then
           next.
         assign ptd-oper = ptd-oper + gltrans.tr-amt.
       end.
     end.
 end.

 if last-of(account.actnum) then
  do:

    do per-loop = 1 to (v-period - 1):
      find first xperiod where xperiod.company = cocode and
                                xperiod.pnum = per-loop and
                                xperiod.yr = v-year
                                no-lock no-error.
      IF AVAIL xperiod THEN
      FOR EACH glhist NO-LOCK
          WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ per-loop
            AND glhist.tr-date GE xperiod.pst
            AND glhist.tr-date LE xperiod.pend:
        ytd-gen = ytd-gen + glhist.tr-amt.
      END.
    end.

    assign v-ptd-per = ((ptd-gen / tot-ptd-sales) * 100)
           ytd-gen = ytd-gen + ptd-gen + account.cyr-open
           v-ytd-per = ((ytd-gen / tot-ytd-sales) * 100).
    IF v-ptd-per = ? THEN v-ptd-per = 0.
    IF v-ytd-per = ? THEN v-ytd-per = 0.
    display account.dscr ptd-gen @ ptd-sales v-ptd-per
                         ytd-gen @ ytd-sales v-ytd-per
      with frame line-item overlay down.
    down with frame line-item.
    assign tot-ptd-gen = tot-ptd-gen + ptd-gen
           tot-ytd-gen = tot-ytd-gen + ytd-gen
           ptd-gen = 0
           ytd-gen = 0.
  end.
end.  /* Gen & Admin Expenses */

put "--------------" to 40 "-------" to 50
    "--------------" to 65 "-------" to 75 skip.
put "Total General & Admin" at 1 tot-ptd-gen to 40
     ((tot-ptd-gen / tot-ptd-sales) * 100.00) to 50
      tot-ytd-gen to 65
     ((tot-ytd-gen / tot-ytd-sales) * 100.00) to 75 skip(1).
put "Income Tax Expenses" skip(1).

/*  Income Tax Expenses */
for each account where account.company eq cocode and
                       account.actnum >= v-s-inc-no and
                       account.actnum <= v-e-inc-no
                       no-lock use-index account break by account.actnum:

  find first period where period.company = cocode and
                          period.pst <= v-ptd and
                          period.pend >= v-ptd no-lock no-error.

  FOR EACH glhist NO-LOCK
      WHERE glhist.company EQ account.company
        AND glhist.actnum  EQ account.actnum
        AND glhist.period  EQ v-period
        AND glhist.tr-date GE period.pst
        AND glhist.tr-date LE period.pend:
    ptd-inc = ptd-inc + glhist.tr-amt.
  END.

  if pre-close then
  do:
    for each gltrans no-lock where gltrans.actnum = account.actnum and
                                     gltrans.company = cocode:
      if gltrans.tr-date le v-ptd and
         gltrans.tr-date >= period.pst and
         gltrans.tr-date <= period.pend then
      do:
        if gltrans.period <> period.pnum then
          next.
        assign ptd-inc = ptd-inc + gltrans.tr-amt.
      end.
    end.
  end.

  if last-of(account.actnum) then
  do:

    do per-loop = 1 to (v-period - 1):
      find first xperiod where xperiod.company = cocode and
                                xperiod.pnum = per-loop and
                                xperiod.yr = v-year
                                no-lock no-error.
      IF AVAIL xperiod THEN
      FOR EACH glhist NO-LOCK
          WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.period  EQ per-loop
            AND glhist.tr-date GE xperiod.pst
            AND glhist.tr-date LE xperiod.pend:
        ytd-inc = ytd-inc + glhist.tr-amt.
      END.
    end.

    assign v-ptd-per = ((ptd-inc / tot-ptd-sales) * 100)
           ytd-inc = ytd-inc + ptd-inc + account.cyr-open
           v-ytd-per = ((ytd-inc / tot-ytd-sales) * 100).
    IF v-ptd-per = ? THEN v-ptd-per = 0.
    IF v-ytd-per = ? THEN v-ytd-per = 0.
    display account.dscr ptd-inc @ ptd-sales v-ptd-per
                         ytd-inc @ ytd-sales v-ytd-per
      with frame line-item overlay down.
    down with frame line-item.
    assign tot-ptd-inc = tot-ptd-inc + ptd-inc
           tot-ytd-inc = tot-ytd-inc + ytd-inc
           ptd-inc = 0
           ytd-inc = 0.
  end.
end.  /* Operating Expenses */


  assign tot-ptd-exp = tot-ptd-oper + tot-ptd-gen + tot-ptd-inc
         tot-ytd-exp = tot-ytd-oper + tot-ytd-gen + tot-ytd-inc.

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Income Tax Expense" at 1 tot-ptd-inc to 40
       ((tot-ptd-inc / tot-ptd-sales) * 100.00) to 50
        tot-ytd-inc to 65
       ((tot-ytd-inc / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.
  put "Total Operating Expenses" at 1
       tot-ptd-exp to 40
       ((tot-ptd-exp / tot-ptd-sales) * 100.00) to 50
       tot-ytd-exp to 65
       ((tot-ytd-exp / tot-ytd-sales) * 100.00) to 75 skip(1).
  put "Net Income Before Taxes"
      (tot-ptd-gross - tot-ptd-exp) to 40 format "->>,>>>,>>9.99"
      (((tot-ptd-gross - tot-ptd-exp) / tot-ptd-sales) * 100.00) to 50
      (tot-ytd-gross - tot-ytd-exp) to 65 format "->>,>>>,>>9.99"
      (((tot-ytd-gross - tot-ytd-exp) / tot-ytd-sales) * 100.00) to 75 skip.
  put "==============" to 40 "=======" to 50
      "==============" to 65 "=======" to 75 skip(1).

  /*  Other Expenses */
  for each account where account.company eq cocode and
                         account.actnum >= v-s-oth-no and
                         account.actnum <= v-e-oth-no
                         no-lock use-index account break by account.actnum:

    find first period where period.company = cocode and
                            period.pst <= v-ptd and
                            period.pend >= v-ptd no-lock no-error.

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ account.company
          AND glhist.actnum  EQ account.actnum
          AND glhist.period  EQ v-period
          AND glhist.tr-date GE period.pst
          AND glhist.tr-date LE period.pend:
      ptd-oth = ptd-oth + glhist.tr-amt.
    END.

    if pre-close then
    do:
      for each gltrans no-lock where gltrans.actnum = account.actnum and
                                       gltrans.company = cocode:
        if gltrans.tr-date le v-ptd and
           gltrans.tr-date >= period.pst and
           gltrans.tr-date <= period.pend then
        do:
          if gltrans.period <> period.pnum then
            next.
          assign ptd-oth = ptd-oth + gltrans.tr-amt.
        end.
      end.
    end.

    if last-of(account.actnum) then
    do:

      do per-loop = 1 to (v-period - 1):
        find first xperiod where xperiod.company = cocode and
                                  xperiod.pnum = per-loop and
                                  xperiod.yr = v-year
                                  no-lock no-error.
        IF AVAIL xperiod THEN
        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum
              AND glhist.period  EQ per-loop
              AND glhist.tr-date GE xperiod.pst
              AND glhist.tr-date LE xperiod.pend:
          ytd-oth = ytd-oth + glhist.tr-amt.
        END.
      end.

      assign v-ptd-per = ((ptd-oth / tot-ptd-sales) * 100)
             ytd-oth = ytd-oth + ptd-oth + account.cyr-open
             v-ytd-per = ((ytd-oth / tot-ytd-sales) * 100).
      IF v-ptd-per = ? THEN v-ptd-per = 0.
      IF v-ytd-per = ? THEN v-ytd-per = 0.
      display account.dscr ptd-oth @ ptd-sales v-ptd-per
                           ytd-oth @ ytd-sales v-ytd-per
        with frame line-item overlay down.
      down with frame line-item.
      assign tot-ptd-oth = tot-ptd-oth + ptd-oth
             tot-ytd-oth = tot-ytd-oth + ytd-oth
             ptd-oth = 0
             ytd-oth = 0.
    end.
  end.  /* Other Expenses */

  put "--------------" to 40 "-------" to 50
      "--------------" to 65 "-------" to 75 skip.

  put "Net Income After Taxes"
      ((tot-ptd-gross - tot-ptd-exp) - tot-ptd-oth) to 40 format "->>,>>>,>>9.99"
      ((((tot-ptd-gross - tot-ptd-exp) - tot-ptd-oth) / tot-ptd-sales) * 100.00) to 50
      ((tot-ytd-gross - tot-ytd-exp) - tot-ytd-oth) to 65 format "->>,>>>,>>9.99"
      ((((tot-ytd-gross - tot-ytd-exp) - tot-ytd-oth) / tot-ytd-sales) * 100.00) to 75 skip.
  put "==============" to 40 "=======" to 50
      "==============" to 65 "=======" to 75 SKIP.

 SESSION:SET-WAIT-STATE("").

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
           ELSE IF lv-field-hdl:TYPE = "Fill-in" THEN 
               assign parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:help + "," 
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

