&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: gl\r-gljrn.w

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
DEF INPUT PARAM ip-post AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.
DEF VAR post-line-error AS LOGICAL NO-UNDO INITIAL NO.
DEF VAR v-jrnal-2 AS CHAR NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.

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


def var v-unline as char format "x(80)" init
  "--------------- ------------------------- ------- ----------- ---" NO-UNDO.
def var time_stamp as ch NO-UNDO.
DEF VAR v-invalid AS LOG NO-UNDO.


def var save_id as recid NO-UNDO.
def var tbal as dec format "->>,>>>,>>9.99" NO-UNDO.
def var deb  as dec label "DEBIT"  format "->>,>>>,>>9.99" NO-UNDO.
def var cred as dec label "CREDIT" format "->>,>>>,>>9.99" NO-UNDO.
DEF VAR lv-rev AS CHAR LABEL " " NO-UNDO.
def var tot-deb  as dec format "->>,>>>,>>9.99" NO-UNDO.
def var tot-cred like tot-deb NO-UNDO.
def var ad like tot-deb NO-UNDO.
def var ac like tot-deb NO-UNDO.
def var gc like tot-deb NO-UNDO.
def var gd like tot-deb NO-UNDO.
def var curjnl as char format "x(8)" NO-UNDO.
def var fjrnl as int label "  From Journal Number" NO-UNDO.
def var tjrnl as int label "  Thru Journal Number" NO-UNDO.
def var xtrnum as int NO-UNDO.
def var sort-by-acct as log NO-UNDO.
DEF VAR v-postable AS LOG NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEFINE VARIABLE glBalanceCheck AS LOGICAL     NO-UNDO.
DEFINE VARIABLE glOutOfBalance AS LOGICAL     NO-UNDO.

DEF BUFFER bgl-jrn  FOR gl-jrn.
DEF BUFFER bgl-jrnl FOR gl-jrnl.

DEF TEMP-TABLE tt-gl-jrn  LIKE gl-jrn.
DEF TEMP-TABLE tt-gl-jrnl LIKE gl-jrnl.

DEF STREAM s-temp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_j-no end_j-no begin_date ~
end_date rd_sort rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_j-no end_j-no begin_date end_date ~
lbl_sort rd_sort rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_j-no AS INTEGER FORMAT ">>>>>>>>" INITIAL 1 
     LABEL "Beginning Journal #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_j-no AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 
     LABEL "Ending Journal #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-gljrn.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort by?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

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

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Account#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Account#", "Account#",
"Sequence", "Sequence"
     SIZE 31 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 9.76.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 10.24.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_j-no AT ROW 3.86 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Journal Number"
     end_j-no AT ROW 3.86 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_date AT ROW 5.05 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Journal Date"
     end_date AT ROW 5.05 COL 69 COLON-ALIGNED HELP
          "Enter Ending Journal Date"
     lbl_sort AT ROW 6.95 COL 30 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 6.95 COL 42 NO-LABEL
     rd-dest AT ROW 12.86 COL 6 NO-LABEL
     lv-ornt AT ROW 12.86 COL 31 NO-LABEL
     lines-per-page AT ROW 12.86 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 15.43 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 16.57 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17.71 COL 31
     tb_excel AT ROW 18.67 COL 31 WIDGET-ID 2
     tb_runExcel AT ROW 18.67 COL 53 WIDGET-ID 4
     fi_file AT ROW 19.57 COL 29 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 6
     btn-ok AT ROW 21.57 COL 27
     btn-cancel AT ROW 21.57 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 11.67 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY 1 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 11.24 COL 1
     RECT-7 AT ROW 1.05 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 95.8 BY 22.91.


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
         TITLE              = "Journal Entries Edit/Posting Register"
         HEIGHT             = 22.95
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
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_j-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_j-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

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
ON END-ERROR OF C-Win /* Journal Entries Edit/Posting Register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Journal Entries Edit/Posting Register */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME begin_j-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_j-no C-Win
ON LEAVE OF begin_j-no IN FRAME FRAME-A /* Beginning Journal # */
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
  DEF VAR lv-post AS LOG NO-UNDO.


  RUN GetNK1GLJournalPost(INPUT cocode,
                          OUTPUT glBalanceCheck).
  RUN check-date.
  IF v-invalid THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  IF ip-post THEN DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        ASSIGN xtrnum        = gl-ctrl.trnum + 1
               gl-ctrl.trnum = xtrnum.
        FIND CURRENT gl-ctrl NO-LOCK.
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
  END.

  RUN run-report. 

  CASE rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= ''
                            &begin_cust= "begin_j-no"
                            &END_cust= "end_j-no"
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = ''
                             &begin_cust=''
                             &END_cust='' 
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust=''
                                  &END_cust='' 
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  END CASE.
  SESSION:SET-WAIT-STATE ("").

  IF ip-post THEN
    IF v-postable THEN DO: 
        lv-post = NO.
        IF glBalanceCheck AND glOutOfBalance THEN DO:
            MESSAGE "Journal is out of balance.  Cannot post."
                VIEW-AS ALERT-BOX.
        END.
        ELSE
            MESSAGE "Post Journals?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-post.

      IF lv-post THEN do:
        RUN post-gl.
        RUN copy-report-to-audit-dir.
        MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
        APPLY "close" TO THIS-PROCEDURE.
      END.
      ELSE RUN undo-trnum.
    END.

    ELSE DO:
      MESSAGE "No Journals available for posting..." VIEW-AS ALERT-BOX ERROR.
      RUN undo-trnum.
    END.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_j-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_j-no C-Win
ON LEAVE OF end_j-no IN FRAME FRAME-A /* Ending Journal # */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
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


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
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

  RUN enable_UI.

  {methods/nowait.i}


find first sys-ctrl where
    sys-ctrl.company eq cocode AND
    sys-ctrl.name    eq "AUDITDIR"
    no-lock no-error.

  if not avail sys-ctrl then DO TRANSACTION:
     create sys-ctrl.
     assign
        sys-ctrl.company = cocode
        sys-ctrl.name    = "AUDITDIR"
        sys-ctrl.descrip = "Audit Trails directory"
        sys-ctrl.char-fld = ".\AUDIT TRAILS".
  end.

  lv-audit-dir = sys-ctrl.char-fld.

  IF LOOKUP(SUBSTR(lv-audit-dir,LENGTH(lv-audit-dir),1),"/,\") > 0 THEN
     lv-audit-dir = SUBSTR(lv-audit-dir,1,LENGTH(lv-audit-dir) - 1).

  RELEASE sys-ctrl.

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    /*IF ip-post THEN DO:
      /*tran-date:SCREEN-VALUE = STRING(TODAY).
      APPLY "entry" TO tran-date.*/
    END.
    ELSE DO:
      tran-date:SENSITIVE = NO.
      APPLY "entry" TO begin_j-no.
    END.*/
    IF ip-post EQ ? THEN DO:
      c-win:TITLE = "Journal Entries Report".
      /*ASSIGN
       tran-date:VISIBLE   = NO
       tran-period:VISIBLE = NO.*/
    END.
  END.

  RUN check-date.

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

  /*IF ip-post NE ? THEN
  DO with frame {&frame-name}:
    v-invalid = no.

    find first period                   
        where period.company eq cocode
          and period.pst     le DATE(tran-date:SCREEN-VALUE)
          and period.pend    ge DATE(tran-date:SCREEN-VALUE)
        no-lock no-error.
    if avail period then do:
       IF NOT period.pstat THEN DO:
          MESSAGE "Period Already Closed. " VIEW-AS ALERT-BOX ERROR.
          v-invalid = YES.
       END.
        tran-period:SCREEN-VALUE = STRING(period.pnum).
    END.

    ELSE DO:
      message "No Defined Period Exists for" tran-date view-as alert-box error.
      v-invalid = yes.
    end.
  END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-report-to-audit-dir C-Win 
PROCEDURE copy-report-to-audit-dir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR targetfile AS CHAR FORMAT "X(50)" NO-UNDO.
  DEF VAR dirname1 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR dirname2 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR dirname3 AS CHAR FORMAT "X(20)" NO-UNDO.

  ASSIGN targetfile = lv-audit-dir + "\GL\GU2\Run#"
                    + STRING(xtrnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\GL"
         dirname3 = lv-audit-dir + "\GL\GU2".

  OS-COPY VALUE(list-name) VALUE (targetfile).

  IF SEARCH(targetfile) EQ ? THEN DO:
    OS-CREATE-DIR VALUE(dirname1).
    OS-CREATE-DIR VALUE(dirname2).
    OS-CREATE-DIR VALUE(dirname3).
    OS-COPY VALUE(list-name) VALUE (targetfile).
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
  DISPLAY begin_j-no end_j-no begin_date end_date lbl_sort rd_sort rd-dest 
          lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_j-no end_j-no begin_date end_date rd_sort rd-dest 
         lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel 
         fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetNK1GLJournalPost C-Win 
PROCEDURE GetNK1GLJournalPost :
/*------------------------------------------------------------------------------
  Purpose:     Get the value of the NK1 for Journal Posting
  Parameters:  input company, output logical
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER oplCheckBalance AS LOGICAL     NO-UNDO.

DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.

RUN sys/ref/nk1look.p (INPUT ipcCompany,
                       INPUT "GLJournalPost", 
                       INPUT "L", 
                       INPUT NO, 
                       INPUT NO, 
                       INPUT "", 
                       INPUT "", 
                       OUTPUT cReturn, 
                       OUTPUT lFound).
IF lFound THEN
    oplCheckBalance = LOGICAL(cReturn) EQ YES NO-ERROR.

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
 /*    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY.  */

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
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
 /*    RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */
     IF NOT RESULT THEN v-postable = NO.
   */  
run custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt). /* open file-name, title */ 

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
 run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-gl C-Win 
PROCEDURE post-gl :
FOR EACH tt-gl-jrn:
  DELETE tt-gl-jrn.
END.

FOR EACH tt-gl-jrnl:
  DELETE tt-gl-jrnl.
END.

DO TRANSACTION:
   FOR EACH gl-jrn
       WHERE gl-jrn.company EQ cocode
         AND gl-jrn.posted  EQ NO
         /*AND gl-jrn.period  EQ tran-period*/
         AND gl-jrn.recur   EQ NO
         AND gl-jrn.journal GE fjrnl
         AND gl-jrn.journal LE tjrnl
             AND gl-jrn.tr-date GE begin_date
         AND gl-jrn.tr-date LE end_date
       BREAK BY gl-jrn.journal:

      RELEASE tt-gl-jrn.
      IF gl-jrn.reverse THEN DO:
        CREATE tt-gl-jrn.
        BUFFER-COPY gl-jrn TO tt-gl-jrn.
      END.

      /* if gl-jrn.tr-amt ne 0 then next. */
      FOR EACH gl-jrnl OF gl-jrn:
        IF AVAIL tt-gl-jrn THEN DO:
          CREATE tt-gl-jrnl.
          BUFFER-COPY gl-jrnl TO tt-gl-jrnl.
        END.

       find first bank
           where bank.company eq gl-jrn.company
             and bank.actnum  eq gl-jrnl.actnum
           no-error.
        if not available bank then do:
         create gltrans.
         assign
            gltrans.jrnl    = "GENERAL" /* 9508 CAH was STRING(JRN#,"99999") */
            gltrans.tr-date = gl-jrn.tr-date
            gltrans.tr-dscr = gl-jrnl.dscr + "JRN#" + string(gl-jrn.journal,"9999999")
            gltrans.actnum  = gl-jrnl.actnum
            gltrans.company = cocode
            gltrans.tr-amt  = gl-jrnl.tr-amt
            gltrans.period  = gl-jrn.period
            gltrans.trnum   = xtrnum.
/*
            gltrans.trnum   = gl-jrn.journal. /* 9508 CAH was jrnl.line */
*/
        end.
        if available bank then do:
         create gltrans.
         assign
            gltrans.jrnl    = "GENERAL" /* 9508 CAH was STRING(JRN#,"99999") */
            gltrans.tr-date = gl-jrn.tr-date
            gltrans.tr-dscr = gl-jrnl.dscr + "JRN#" + string(gl-jrn.journal,"9999999")
            gltrans.actnum  = gl-jrnl.actnum
            gltrans.company = cocode
            gltrans.tr-amt  = gl-jrnl.tr-amt
            gltrans.period  = gl-jrn.period
            gltrans.trnum   = xtrnum.
/*
            gltrans.trnum   = gl-jrn.journal. /* 9508 CAH was jrnl.line */
*/
         assign bank.bal = bank.bal + gl-jrnl.tr-amt.
        end.
      end.

      gl-jrn.posted = true.
   end.

   FOR EACH tt-gl-jrn:
     CREATE gl-jrn.
     BUFFER-COPY tt-gl-jrn EXCEPT j-no journal rec_key TO gl-jrn
     ASSIGN
      gl-jrn.reverse      = NO
      gl-jrn.from-reverse = YES
      gl-jrn.orig-jnl     = tt-gl-jrn.j-no.

     FOR EACH period                   
         WHERE period.company EQ cocode
           AND period.pst     GT tt-gl-jrn.tr-date
         BY period.yr
         BY period.pst:
       ASSIGN
        gl-jrn.tr-date = period.pst
        gl-jrn.period  = period.pnum.
       LEAVE.
     END.

     FOR EACH tt-gl-jrnl OF tt-gl-jrn:
       CREATE gl-jrnl.
       BUFFER-COPY tt-gl-jrnl EXCEPT rec_key TO gl-jrnl
       ASSIGN
        gl-jrnl.j-no   = gl-jrn.j-no
        gl-jrnl.tr-amt = tt-gl-jrnl.tr-amt * -1.

       DELETE tt-gl-jrnl.
     END.

     DELETE tt-gl-jrn.
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
DEF VAR v-excel-hdr AS   CHAR.
{sys/inc/print1.i}

  {sys/inc/outprint.i VALUE(lines-per-page)}

  IF td-show-parm THEN RUN show-param.

    time_stamp = string(time, "HH:MMam").
    tmpstore   = fill("_",125).
    ASSIGN fjrnl = begin_j-no
           tjrnl = END_j-no
           sort-by-acct = rd_sort EQ "Account#"          
           .

SESSION:SET-WAIT-STATE ("general").

{sys/form/r-topw.f}

    str-tit  = coname + " - " + loname.
    str-tit2 = "JOURNAL ENTRIES" +
             IF ip-post NE ? THEN ("  -  EDIT REGISTER " + STRING(xtrnum))
                             ELSE "".
    str-tit3 = "Journal " + string(fjrnl) + " Through " + string(tjrnl) + "  " +
               "Trans Date " + string(begin_date) + " Through " + string(end_date).
    x = (112 - length(str-tit)) / 2.
    str-tit  = fill(" ",x) + str-tit.
    x = (114 - length(str-tit2)) / 2.
    str-tit2 = fill(" ",x) + str-tit2.
    x = (132 - length(str-tit3)) / 2.
    str-tit3 = fill(" ",x) + str-tit3.

    DISPLAY str-tit3 format "x(130)"  skip(1) with frame r-top STREAM-IO.

    VIEW FRAME r-top.

    /* excel title rtc 08/12/08 */
    ASSIGN
        v-excel-hdr =  "Journal,Description,Date,Account,Description,Debit,Credit" 
        tbal = 0. /* 9508 CAH */

    v-postable = NO.

    IF tb_excel THEN DO:
        OUTPUT STREAM s-temp TO VALUE(fi_file).
        PUT STREAM s-temp UNFORMATTED 
            STRING(TODAY) + str-tit     SKIP
            time_stamp    + str-tit2    SKIP
            str-tit3                    SKIP
            v-excel-hdr                 SKIP.
    END.

    IF sort-by-acct THEN DO:
        {gl/gl-jreg.i "by gl-jrnl.actnum" 1}
    END.
    ELSE DO:
        {gl/gl-jreg.i "by gl-jrn.j-no by gl-jrnl.line" 2}
    END.

    /* rtc 08/11/2008 */
    IF tb_excel THEN DO:
        OUTPUT STREAM s-temp close.
        IF tb_runExcel THEN
            OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undo-trnum C-Win 
PROCEDURE undo-trnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* gdm - 11050906 */
  REPEAT:
    FIND FIRST gl-ctrl EXCLUSIVE-LOCK
      WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
    IF AVAIL gl-ctrl THEN DO:  
      if gl-ctrl.trnum = xtrnum THEN gl-ctrl.trnum = xtrnum - 1.
      FIND CURRENT gl-ctrl NO-LOCK.
      RELEASE gl-ctrl.
      LEAVE.
    END. /* IF AVAIL gl-ctrl */
  END. /* REPEAT */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

