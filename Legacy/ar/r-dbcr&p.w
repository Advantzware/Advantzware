&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ar\r-dbcr&p.w

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
DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.
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

/*
if v-return then return.
*/
DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-postable AS LOG NO-UNDO.

DEFINE VARIABLE save_id AS RECID NO-UNDO.
DEFINE VARIABLE time_stamp AS ch NO-UNDO.
DEFINE VARIABLE qfirst AS l NO-UNDO.
DEFINE VARIABLE post AS LOGICAL FORMAT "Yes/No"
  LABEL "        Post to G/L & Customer files?        " INITIAL NO NO-UNDO.
DEFINE VARIABLE g1 AS DECIMAL format "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE archk AS DECIMAL FORMAT ">>>>>>>>" NO-UNDO.
DEFINE VARIABLE t1 AS DECIMAL format "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE g2 AS DECIMAL format "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE t3 AS DECIMAL format "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v1 AS DECIMAL format "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v2 AS DECIMAL format "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE t2 AS DECIMAL format "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-on-act-amt AS DECIMAL NO-UNDO.
DEFINE VARIABLE xtrnum AS INTEGER NO-UNDO.
DEFINE VARIABLE xar-acct AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcs-acct AS CHARACTER NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEFINE VARIABLE cCustStatCheck AS CHARACTER NO-UNDO .

DO TRANSACTION:
  {sys/inc/postdate.i}
  {sys/inc/inexport.i}
  {sys/inc/oecredit.i}
END.


DEF VAR v-ftp-done AS LOG NO-UNDO.
DEF NEW SHARED VAR v-term-id AS cha NO-UNDO.

find first sys-ctrl
        where sys-ctrl.company eq cocode
          and sys-ctrl.name    eq "INVPRINT"
        no-lock no-error.
v-print-fmt = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date begin_cust end_cust ~
begin_date end_date tb_export rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_cust end_cust ~
begin_date end_date tb_export rd-dest lv-ornt lines-per-page lv-font-no ~
lv-font-name td-show-parm 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Memo Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Memo Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

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
     LABEL "Post Date" 
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

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.62.

DEFINE VARIABLE tb_export AS LOGICAL INITIAL no 
     LABEL "Export/FTP  Memos?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 1.62 COL 44 COLON-ALIGNED
     tran-period AT ROW 2.86 COL 44 COLON-ALIGNED
     begin_cust AT ROW 4.14 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 2
     end_cust AT ROW 4.14 COL 66.4 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 6
     begin_date AT ROW 5.48 COL 27 COLON-ALIGNED HELP
          "Enter Beginning BOL Date" WIDGET-ID 4
     end_date AT ROW 5.48 COL 66.6 COLON-ALIGNED HELP
          "Enter Ending BOL Date" WIDGET-ID 8
     tb_export AT ROW 7 COL 38.8
     rd-dest AT ROW 9.86 COL 6 NO-LABEL
     lv-ornt AT ROW 9.86 COL 30 NO-LABEL
     lines-per-page AT ROW 9.86 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 11.76 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 12.71 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 15.33 COL 31
     btn-ok AT ROW 18.38 COL 23
     btn-cancel AT ROW 18.38 COL 58
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 8.91 COL 4
     RECT-6 AT ROW 8.67 COL 1.4
     RECT-7 AT ROW 1 COL 1.4
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
         TITLE              = "Credit / Debit Memo Posting Register"
         HEIGHT             = 20.19
         WIDTH              = 95.4
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
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_export:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_export:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

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
ON END-ERROR OF C-Win /* Credit / Debit Memo Posting Register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Credit / Debit Memo Posting Register */
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
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Memo Date */
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DEF VAR lv-post AS LOG NO-UNDO.

  run check-date.
  if v-invalid then return no-apply.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  SESSION:SET-WAIT-STATE ("general").

  DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        ASSIGN xtrnum        = gl-ctrl.trnum + 1
               gl-ctrl.trnum = xtrnum.
        RELEASE gl-ctrl. 
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
  END.

  IF v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "southpak"
       THEN is-xprint-form = YES.     
  ELSE is-xprint-form = NO.

  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= ''
                            &begin_cust= "tran-date"
                            &END_cust= "tran-date" 
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
  end case.

  SESSION:SET-WAIT-STATE("").

  IF v-postable THEN DO:

    lv-post = NO.

    MESSAGE "Post Invoices?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-post.

    IF lv-post THEN do:      
        RUN post-gl.
        RUN check-status .
        RUN copy-report-to-audit-dir.
      MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
    END.
    ELSE RUN undo-trnum.  
  END.

  ELSE do:
      MESSAGE "No Credit/Debit Memos available for posting..." VIEW-AS ALERT-BOX ERROR.
      RUN undo-trnum.
  END.
  IF v-ftp-done THEN MESSAGE "File Export/FTP is completed." VIEW-AS ALERT-BOX INFORMATION.
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
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Memo Date */
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


&Scoped-define SELF-NAME tb_export
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_export C-Win
ON VALUE-CHANGED OF tb_export IN FRAME FRAME-A /* Export/FTP  Memos? */
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
ON LEAVE OF tran-date IN FRAME FRAME-A /* Post Date */
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

  RUN init-proc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  RUN enable_UI.
  {methods/nowait.i}

  DO WITH FRAME {&frame-name}:
    {custom/usrprint.i}

    IF postdate-log THEN DO:
      ASSIGN
       tran-date:SCREEN-VALUE = STRING(TODAY)
       tran-date              = TODAY.
      RUN check-date.
    END.

    ELSE
      ASSIGN
       tran-date:SCREEN-VALUE   = ""
       tran-period:SCREEN-VALUE = "".

    APPLY "entry" TO tran-date.
  END.

  
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
    if avail period then do:
       IF NOT period.pstat THEN DO:
          MESSAGE "Period Already Closed. " VIEW-AS ALERT-BOX ERROR.
          v-invalid = YES.
       END.
        tran-period:SCREEN-VALUE = string(period.pnum).
    END.

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
  DISPLAY tran-date tran-period begin_cust end_cust begin_date end_date 
          tb_export rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date begin_cust end_cust begin_date end_date 
         tb_export rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc C-Win 
PROCEDURE init-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

do :
  find first ar-ctrl where ar-ctrl.company eq cocode NO-LOCK.
  assign
   xar-acct   = ar-ctrl.receivables
   xcs-acct = ar-ctrl.cash-act.
  release ar-ctrl.
  find first account
      where account.company eq cocode
        and account.actnum  eq xar-acct
      no-lock no-error.
  if not avail account or account.actnum = "" then do:
     message " Receivables Account is blank or is not on file for this Company."
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  end.
  find first account
      where account.company eq cocode
        and account.actnum  eq xcs-acct
      no-lock no-error.
  if not avail account or account.actnum = "" then do:    
    message " Casht Account is blank or is not on file for this Company." VIEW-AS ALERT-BOX ERROR.    
    RETURN ERROR.
  end.
end.


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
  /*   DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY. */

{custom/out2file.i}.     


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

     IF NOT RESULT THEN v-postable = NO.
  */
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

  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-gl C-Win 
PROCEDURE post-gl :
postit:
 DO TRANSACTION ON ERROR UNDO:
    g2 = 0.

    for each ar-cash
        where ar-cash.company   eq cocode
          and ar-cash.posted    eq no
          and ar-cash.memo      eq yes
          /* gdm - 07130905 */
          AND ar-cash.cust-no    GE begin_cust
          AND ar-cash.cust-no    LE end_cust
          AND ar-cash.check-date GE begin_date
          AND ar-cash.check-date LE end_date
        /* gdm - 07130905 end */
          and can-find(first ar-cashl where ar-cashl.c-no eq ar-cash.c-no)
        use-index posted on error undo postit, leave postit:

      IF ar-cash.printed NE YES THEN NEXT.
      IF ar-cash.stat EQ "H" THEN
        NEXT.



      {ar/ar-dreg.i}

    end.  /* for each */

    RELEASE ar-ledger.

    CREATE gltrans.
    ASSIGN
      gltrans.company = cocode
      gltrans.actnum  = xar-acct
      gltrans.jrnl    = "DBMEM"
      gltrans.tr-dscr = "CREDIT/DEBIT MEMO"
      gltrans.tr-date = tran-date
      gltrans.tr-amt  = + g2
      gltrans.period  = tran-period
      gltrans.trnum   = xtrnum.
    IF gltrans.tr-amt < 0 THEN
    gltrans.jrnl = "CRMEM".

    RELEASE gltrans.
  END. /* postit: transaction */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- ar/ar-creg.p 10/94 gb */
/* AR Cash  - Edit Register & Post Transactions                   */
/* -------------------------------------------------------------------------- */
{sa/sa-sls01.i}
v-term-id = v-term.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.
SESSION:SET-WAIT-STATE("general").
ASSIGN
  v-postable = NO
  g2 = 0.

{sys/form/r-top3w.f}

FORMAT HEADER
  "CUSTOMER NAME                                      MEMO #    DATE          "
  "AMOUNT" skip
  "INVOICE   G/L DISTRIBUTION" AT 70 SKIP FILL("_",132) FORMAT "x(132)"
  WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top PAGE-TOP WIDTH 132 STREAM-IO.

  ASSIGN
    time_stamp = STRING(TIME, "HH:MMam")
    tmpstore   = FILL("_",125)
    str-tit  = coname + " - " + loname
    str-tit2 = "CR/DB MEMOS -  POSTING REPORT  " + STRING(xtrnum)
    str-tit3 = "Period " + STRING(tran-period,"99") + " " + STRING(tran-date)
    x = (112 - LENGTH(str-tit)) / 2
    str-tit  = FILL(" ",x) + str-tit
    x = (114 - LENGTH(str-tit2)) / 2
    str-tit2 = FILL(" ",x) + str-tit2
    x = (132 - LENGTH(str-tit3)) / 2
    str-tit3 = FILL(" ",x) + str-tit3 .

  DISPLAY "" WITH FRAME r-top.
  DISPLAY "" WITH FRAME f-top.

  for each ar-cash
      where ar-cash.company   eq cocode
        and ar-cash.posted    eq no
        and ar-cash.memo      eq YES
      /* gdm - 07130905 */
        AND ar-cash.cust-no    GE begin_cust
        AND ar-cash.cust-no    LE end_cust
        AND ar-cash.check-date GE begin_date
        AND ar-cash.check-date LE end_date
      /* gdm - 07130905 end */
        and can-find(first ar-cashl where ar-cashl.c-no eq ar-cash.c-no)
        and ar-cash.printed EQ YES
      use-index posted

      break by ar-cash.cust-no
            by ar-cash.check-no
      with frame a1:

      IF LOOKUP(ar-cash.cust-no,cCustStatCheck) EQ 0 THEN
        ASSIGN cCustStatCheck = cCustStatCheck + ar-cash.cust-no + "," .


    IF ar-cash.stat EQ "H" THEN
       NEXT.


    IF can-find(FIRST ar-cashl WHERE ar-cashl.company = cocode and
                               ar-cashl.c-no = ar-cash.c-no AND
                               (ar-cashl.amt-paid + ar-cashl.amt-disc) < 0 )
    THEN DO:
       CREATE report.
       ASSIGN
        report.term-id = v-term-id
        report.key-01  = STRING(ar-cash.check-no,"9999999999")
        report.rec-id  = RECID(ar-cash).
        FIND FIRST cust {ar/ar-custW.i} AND cust.cust-no = ar-cash.cust-no NO-LOCK NO-ERROR.
        IF cust.factored THEN
            report.key-02 = "Factored".  /* for ar/rep/expcmemo.p task#  09200521*/
    END.
    v-postable = yes.
    IF FIRST-OF(ar-cash.cust-no) THEN DO:
      FIND FIRST cust {ar/ar-custW.i} AND cust.cust-no = ar-cash.cust-no
        NO-LOCK NO-ERROR.
      PUT cust.cust-no SPACE(1) cust.name.
    END.
    ELSE
    IF FIRST-OF(ar-cash.check-no) THEN
    PUT SKIP(1).
    PUT ar-cash.check-no  FORMAT ">>>>>>>9"  TO 57
      ar-cash.check-date AT 60
      ar-cash.check-amt  format "->,>>>,>>9.99" AT 72 skip.

    FOR EACH ar-cashl WHERE ar-cashl.company = cocode and
                            ar-cashl.c-no = ar-cash.c-no 
        NO-LOCK
        USE-INDEX c-no
        BREAK BY ar-cashl.line
        WITH FRAME a2 NO-BOX NO-LABELS WIDTH 132:

      v2 = v2 + ar-cashl.amt-paid - ar-cashl.amt-disc.
      PUT ar-cashl.inv-no AT 70 ar-cashl.actnum AT 80 SPACE(1)
          ar-cashl.amt-paid - ar-cashl.amt-disc format "->,>>>,>>9.99" TO 126 SKIP.
    END. /* each ar-cashl */
    IF LAST-OF(ar-cash.cust-no) THEN
    DO:
      DISPLAY  "*  CUSTOMER TOTALS" AT 88 v2 TO 126 " *" SKIP(1)
        WITH FRAME vtot NO-BOX NO-LABELS WIDTH 132 STREAM-IO.
      ASSIGN
         g1 = g1 + v1
         g2 = g2 + v2
         v1 = 0
         v2 = 0.
    END.
  END. /* each invoice */

  DISPLAY  "** GRAND TOTAL  "  AT 90  g2 TO 126 " **"
    WITH NO-LABELS NO-UNDERLINE WIDTH 132 FRAME GT.

  HIDE FRAME f-top.

  ASSIGN
     str-tit3 = "Period " + STRING(tran-period,"99") + " " + STRING(tran-date) + " - " +
                "Summary by Account"
     x = (132 - LENGTH(str-tit3)) / 2
     str-tit3 = FILL(" ",x) + str-tit3.
  PAGE.
  FORMAT HEADER
    "ACCOUNT                                  DATE   CUSTOMER   MEMO #"
    "LINE DESCRIPTION                QTY   UNIT PRICE           AMOUNT" SKIP
    FILL("_",132) FORMAT "x(132)"
    WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top2 PAGE-TOP WIDTH 132 STREAM-IO.

  DISPLAY "" WITH FRAME f-top2.

  for each ar-cashl
      where ar-cashl.company eq cocode
        and ar-cashl.posted  eq no
        and ar-cash.printed EQ YES
      use-index inv-no
      NO-LOCK
      break by ar-cashl.actnum
            by ar-cashl.check-no

      with width 132 no-labels:

    FIND ar-cash WHERE ar-cash.company = cocode and
                       ar-cash.c-no = ar-cashl.c-no and         
                 /* gdm - 07130905 */
                      ar-cash.cust-no    GE begin_cust
                  AND ar-cash.cust-no    LE end_cust
                  AND ar-cash.check-date GE begin_date
                  AND ar-cash.check-date LE end_date
                /* gdm - 07130905 end */
                  AND ar-cash.memo USE-INDEX c-no NO-LOCK NO-ERROR.
    IF AVAILABLE ar-cash THEN
    DO:

    IF ar-cash.stat EQ "H" THEN
       NEXT.


       FIND cust {ar/ar-custW.i} AND cust.cust-no = ar-cash.cust-no
           NO-LOCK NO-ERROR.
       IF FIRST-OF(ar-cashl.actnum) THEN
       DO:

         FIND FIRST account WHERE account.company = cocode AND
           account.actnum  = ar-cashl.actnum NO-LOCK NO-ERROR.
         IF AVAILABLE account THEN
            PUT ar-cashl.actnum + " - " + account.dscr FORMAT "x(39)" .
         ELSE
            PUT ar-cashl.actnum.
       END.
       archk = ar-cash.check-no.
       PUT
         ar-cash.check-date AT 41     SPACE(1)
         ar-cash.cust-no              SPACE(1)
         archk                        SPACE(1)
         ar-cashl.line FORMAT ">>>9"  SPACE(1)
         ar-cashl.dscr FORMAT "x(20)" SPACE(1)
         ar-cashl.amt-paid - ar-cashl.amt-disc format "->,>>>,>>9.99" TO 132
         SKIP.
       ACCUMULATE ar-cashl.amt-paid - ar-cashl.amt-disc
         (TOTAL BY ar-cashl.actnum).
       ACCUMULATE ar-cashl.amt-paid - ar-cashl.amt-disc (TOTAL).
       IF LAST-OF(ar-cashl.actnum) THEN
       DO:
         PUT "** TOTAL "  TO 116
           ACCUMULATE TOTAL BY ar-cashl.actnum
           ar-cashl.amt-paid - ar-cashl.amt-disc format "->,>>>,>>9.99" TO 132
           SKIP(1).
       END.
    END.
  END.
  PUT "***** TOTAL FOR ALL ACCOUNTS " TO 116
  ACCUMULATE TOTAL ar-cashl.amt-paid - ar-cashl.amt-disc
                format "->,>>>,>>9.99" TO 132.

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
   ELSE IF inexport-cha EQ "ContSrvc" THEN DO:
        OUTPUT TO VALUE(v-exp-file).
        RUN ar/rep/expcmemo.p .
        OUTPUT CLOSE.
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

  DO TRANSACTION: 
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        IF xtrnum EQ gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
        FIND CURRENT gl-ctrl NO-LOCK.
        RELEASE gl-ctrl.
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
  END.

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

  ASSIGN targetfile = lv-audit-dir + "\AR\AW4\Run#"
                    + STRING(xtrnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AR"
         dirname3 = lv-audit-dir + "\AR\AW4".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-status C-Win 
 PROCEDURE check-status :
 /*------------------------------------------------------------------------------
   Purpose:     
   Parameters:  <none>
   Notes:       
 ------------------------------------------------------------------------------*/
 
 DEFINE VARIABLE ld-ord-bal      LIKE cust.ord-bal NO-UNDO.
 DEFINE VARIABLE lRelHold AS LOGICAL NO-UNDO .
 FOR EACH cust EXCLUSIVE-LOCK
     WHERE cust.company EQ cocode
       AND LOOKUP(cust.cust-no,cCustStatCheck) NE 0
       AND cust.cust-no NE "" :
            
     FIND FIRST  terms NO-LOCK
         WHERE terms.company = cust.company
           AND terms.t-code  = cust.terms NO-ERROR.
     
     IF cust.cr-hold THEN do:
         ld-ord-bal      = cust.ord-bal.

         IF oecredit-cha EQ "" THEN
             RUN ar/updcust1.p (YES, BUFFER cust, OUTPUT ld-ord-bal).
         
         IF ld-ord-bal + cust.acc-bal LT cust.cr-lim THEN 
              ASSIGN lRelHold = YES .
         ELSE IF ld-ord-bal LT cust.ord-lim THEN
             ASSIGN lRelHold = YES . 
         ELSE lRelHold = NO .

              IF lRelHold THEN  DO:  
                  ASSIGN cust.cr-hold = NO .
                  
                  FOR EACH oe-ord EXCLUSIVE-LOCK
                     WHERE oe-ord.company             EQ cocode
                       AND oe-ord.cust-no             EQ cust.cust-no
                       AND oe-ord.stat EQ "H" :
                     ASSIGN
                         oe-ord.stat = "A" .
                  END.
              END. /* lRelHold*/

     END. /* cust.cr-hold */
 END. /* for cust */
 
 END PROCEDURE.
 
 /* _UIB-CODE-BLOCK-END */
 &ANALYZE-RESUME
