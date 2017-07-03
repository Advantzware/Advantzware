&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: glrep\r-curdet.w

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

def var v-invalid as log init no no-undo.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

/* gdm - 10010905 */
DEF STREAM str-exl.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date begin_acct end_acct ~
tb_detailed tb_activ rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_acct end_acct ~
tb_detailed tb_activ rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
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

DEFINE VARIABLE begin_acct AS CHARACTER FORMAT "X(25)":U 
     LABEL "Beginning Acct#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct AS CHARACTER FORMAT "X(25)":U INITIAL "zzzzzzzzzzzzzzzzzzzzzzzzz" 
     LABEL "Ending Acct#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-curdet.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

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
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.71.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.81.

DEFINE VARIABLE tb_activ AS LOGICAL INITIAL no 
     LABEL "Print Accounts with No Activity?" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no 
     LABEL "Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1 NO-UNDO.

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
     tran-date AT ROW 2.67 COL 46 COLON-ALIGNED
     tran-period AT ROW 3.86 COL 46 COLON-ALIGNED
     begin_acct AT ROW 5.76 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Account Number"
     end_acct AT ROW 5.76 COL 63 COLON-ALIGNED HELP
          "Enter Ending Account Number"
     tb_detailed AT ROW 7.43 COL 30
     tb_activ AT ROW 8.38 COL 30
     rd-dest AT ROW 11.48 COL 6 NO-LABEL
     lv-ornt AT ROW 12.19 COL 31 NO-LABEL
     lines-per-page AT ROW 12.19 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 13.62 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 14.57 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.38 COL 31
     tb_excel AT ROW 17.86 COL 31.6 WIDGET-ID 2
     tb_runExcel AT ROW 17.86 COL 53.2 WIDGET-ID 4
     fi_file AT ROW 18.95 COL 29.6 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 6
     btn-ok AT ROW 20.81 COL 19
     btn-cancel AT ROW 20.81 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.76 COL 3
          FGCOLOR 9 
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 9.81 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 21.38.


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
         TITLE              = "Current Month General Ledger Summary/Detail"
         HEIGHT             = 21.76
         WIDTH              = 96.2
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
       begin_acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_activ:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
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





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Current Month General Ledger Summary/Detail */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Current Month General Ledger Summary/Detail */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_acct C-Win
ON LEAVE OF begin_acct IN FRAME FRAME-A /* Beginning Acct# */
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
  RUN check-date.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  /* gdm - 10010905 */
  ASSIGN tb_excel
         tb_runExcel
         fi_file.
  /* gdm - 10010905 */

  RUN run-report.
  STATUS DEFAULT "Processing Complete". 
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= " "
                            &begin_cust= "begin_acct"
                            &END_cust= "begin_acct" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = " "
                             &begin_cust= "begin_acct"
                             &END_cust= "begin_acct"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = " "
                                  &begin_cust="begin_acct"
                                  &END_cust="begin_acct"
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  end case.
  SESSION:SET-WAIT-STATE ("").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_acct C-Win
ON LEAVE OF end_acct IN FRAME FRAME-A /* Ending Acct# */
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


&Scoped-define SELF-NAME tb_activ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_activ C-Win
ON VALUE-CHANGED OF tb_activ IN FRAME FRAME-A /* Print Accounts with No Activity? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
DO:
  assign {&self-name}.
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

  tran-date = today.

  RUN check-date.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO tran-date.
  END.

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
  v-invalid = no.

  find first period
      where period.company eq gcompany
        and period.pst     le tran-date
        and period.pend    ge tran-date
      no-lock no-error.
  if avail period then do:  
    tran-period = period.pnum.
    display tran-period with frame {&frame-name}.
  end.

  else do:
    message "No Defined Period Exists for" tran-date view-as alert-box error.
    v-invalid = yes.
  end.
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
  DISPLAY tran-date tran-period begin_acct end_acct tb_detailed tb_activ rd-dest 
          lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date begin_acct end_acct tb_detailed tb_activ 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel 
         tb_runExcel fi_file btn-ok btn-cancel 
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
/*     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* --------------------------------------------------- gl/gl-cmon.p 11/92 cd  */
/* g/l current period transaction report                                      */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def buffer xgltrans for gltrans.

def var tacct like gltrans.actnum  label "    To Account Number".
def var facct like gltrans.actnum  label "  From Account Number".
def var op as char format "!" init "S" label "  S)ummary or D)etail?".
def var inc as log init no label "  Print Accounts with NO Activity?".

def var suppress_zero as logical no-undo init true
    label "Suppress Zero Balances?".

def var tot-all  as   dec format "->>>,>>>,>>>,>>9.99".
def var tot-tx   like tot-all.
def var tot-act  like tot-all.
def var open-amt like tot-all.
def var net-inc  as   dec.
def var tmp-amt  like gltrans.tr-amt FORMAT "(>>>,>>>,>>9.99)".
def var tmp-dscr like gltrans.tr-dscr.
def var v-crdit  like tmp-amt extent 3.
def var v-debit  like v-crdit.
def var str-tit4 as   char no-undo.
def var str-tit5 as   char no-undo.
def var vyear like period.yr no-undo.

/* gdm - 10010905 */
DEF VAR v-excel-hdr  AS CHAR.
DEF VAR v-runbal LIKE open-amt NO-UNDO.

form account.actnum format "x(75)" open-amt to 132
    with frame r-cmon down stream-io width 200 no-labels no-box no-underline.

find first period
    where period.company eq gcompany
      and period.pst     le tran-date
      and period.pend    ge tran-date
    no-lock no-error.

assign
 str-tit2 = "Current Month General Ledger" + IF tb_detailed THEN " Detail " ELSE " Summary"
 {sys/inc/ctrtext.i str-tit2 112}

 str-tit3 = "Period " + string(tran-period,"99") + " - " +
            string(period.pst) + " to " + string(period.pend)
 {sys/inc/ctrtext.i str-tit3 132}

 str-tit4 = "Account Number             Journal  Reference                           Date             Debits        Credits               Balance"
 str-tit5 = "           Jrn#    Run #"

 facct = begin_acct
 tacct = end_acct
 op    = string(tb_detailed,"D/S")
 inc   = tb_activ.

find last period
    where period.company eq cocode
      and period.pst     le tran-date
      and period.pend    ge tran-date
      and period.pnum    eq tran-period
    no-lock.
vyear = period.yr.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

  display str-tit3      format "x(132)" skip(1)
          str-tit4      format "x(132)" skip
          str-tit5      format "x(132)" skip
          fill("-",132) format "x(132)" skip

      with frame r-top.

/* gdm - 10010905 */
IF tb_excel THEN DO:
  ASSIGN v-excel-hdr = "Account Number,Description,Jrn#,Run #,Journal,Reference,Date," +
                       "Debits,Credits,Balance".
  OUTPUT STREAM str-exl TO VALUE(TRIM(fi_file)).
  PUT STREAM str-exl UNFORMATTED v-excel-hdr SKIP.
END. 
/* gdm - 10010905 */

  for each account
      where account.company eq cocode
        and account.actnum  ge facct
        and account.actnum  le tacct
      no-lock
      break by account.actnum:

      {custom/statusMsg.i " 'Processing Account#  '  + account.actnum "}

    run gl/gl-open1.p (recid(account), vyear, tran-date, tran-period,
                       output open-amt).

    find first gltrans no-lock
        where gltrans.company eq cocode
          and gltrans.actnum  eq account.actnum
          and gltrans.period  eq tran-period
        no-error.

    if avail gltrans or inc then do:
      if line-counter gt page-size - 2 then page.
      display string(account.actnum) + "  " + account.dscr format "x(75)" @
              account.actnum open-amt with frame r-cmon.
      down with frame r-cmon.
      if not avail gltrans then put skip(1).
    end.

    /* gdm - 10010905 */
    ASSIGN v-runbal = open-amt.

    for each gltrans no-lock
        where gltrans.company eq cocode
          and gltrans.actnum  eq account.actnum
          and gltrans.period  eq tran-period
        break by gltrans.trnum
              by gltrans.tr-date
              by gltrans.jrnl:

      if gltrans.tr-amt ge 0 then
        assign
         v-debit[1] = v-debit[1] + gltrans.tr-amt
         v-crdit[1] = v-crdit[1] + 0.
      else
        assign
         v-crdit[1] = v-crdit[1] + gltrans.tr-amt
         v-debit[1] = v-debit[1] + 0.

      /* gdm - 10010905 */
      ASSIGN v-runbal = v-runbal + v-crdit[1] + v-debit[1].

      if last-of(gltrans.trnum) or op eq "D" then do:
        if line-counter gt page-size - 2 then page.

        ASSIGN
        tmp-dscr = if op eq "D" then gltrans.tr-dscr                   else
                   if gltrans.jrnl eq "CASHR"                          then
                     "CASH RECEIPTS"                                   else
                   if gltrans.jrnl eq "APCKR"                          then
                     "ACCOUNTS PAYABLE CHECK REGISTER"                 else
                   if gltrans.jrnl eq "GENERAL"                        then
                     "GENERAL"                                         else
                   if gltrans.jrnl eq "ARINV"                          then
                     "ACCOUNTS RECEIVABLE INVOICE"                     else
                   if gltrans.jrnl eq "MCSHREC"                        then
                     "MISC CASH RECEIPTS"                              else
                   if gltrans.jrnl eq "CDISB"                          then
                     "CASH DISBURSEMENT"                               else
                     ""

        tmp-dscr = if op eq "D" then gltrans.tr-dscr                   else
                   if tmp-dscr ne "" then tmp-dscr                     else
                   if gltrans.jrnl eq "APMEM"                          then
                     "ACCOUNTS PAYABLE MEMO"                           else
                   if gltrans.jrnl eq "CRMEM"                          then
                     "CREDIT MEMO"                                     else
                   if gltrans.jrnl eq "DBMEM"                          then
                     "DEBIT MEMO"                                      else
                   if gltrans.jrnl eq "ACPAY"                          then
                     "ACCOUNTS PAYABLE"                                else
                   if gltrans.jrnl eq "APVOIDCK"                       then
                     "ACCOUNTS PAYABLE VOID CHECK"                     else
                   if gltrans.jrnl eq "OEINV"                          then
                     "ORDER ENTRY INVOICE"                             else
                   if gltrans.jrnl eq "JCOST"                          then
                     "PRODUCTION JOB COSTING"                          else
                     ""

        tmp-dscr = if op eq "D" then gltrans.tr-dscr                   else
                   if tmp-dscr ne "" then tmp-dscr                     else
                   if gltrans.jrnl eq "ADJUST"                         then
                     "ADJUSTMENT"                                      else
                     "".

        /* gdm - 10010905 */
        IF tb_excel THEN DO:
          PUT STREAM str-exl UNFORMATTED 
              STRING(account.actnum)  ","  
              REPLACE(account.dscr, "," ," ")  ",,"               
              gltrans.trnum ",".

          IF op EQ "D" 
            THEN PUT STREAM str-exl UNFORMATTED  gltrans.jrnl ",".
            ELSE PUT STREAM str-exl UNFORMATTED  " " ",".

          PUT STREAM str-exl UNFORMATTED 
              REPLACE(TRIM(tmp-dscr),","," ") ","
              STRING(gltrans.tr-date,"99/99/9999") ",".

          IF v-debit[1] NE 0
            THEN PUT STREAM str-exl UNFORMATTED v-debit[1] ",".
            ELSE PUT STREAM str-exl UNFORMATTED " " ",".
          IF v-crdit[1] NE 0 
            THEN PUT STREAM str-exl UNFORMATTED v-crdit[1] ",".
            ELSE PUT STREAM str-exl UNFORMATTED " " ",".

          PUT STREAM str-exl UNFORMATTED v-runbal.


          PUT STREAM str-exl UNFORMATTED  SKIP.

        END.
        /* gdm - 10010905 */

        if op eq "D" and gltrans.jrnl eq "GENERAL" and
           length(tmp-dscr) ge 11 then
        do:   
          if op eq "D" and gltrans.jrnl eq "GENERAL" and
             substring(tmp-dscr,(length(tmp-dscr) - 10),4) eq "JRN#" then
          do:
            display space(11)
                    substring(tmp-dscr,(length(tmp-dscr) - 6),length(tmp-dscr))
                                    format "x(7)"
                    gltrans.trnum   format "9999999"
                    gltrans.jrnl    when op eq "D"
                    substring(tmp-dscr,1,length(tmp-dscr) - 11) format "x(35)"
                    gltrans.tr-date format "99/99/99"
                    v-debit[1]      when v-debit[1] ne 0
                    v-crdit[1]      when v-crdit[1] ne 0
                with no-box stream-io width 200 no-attr-space no-labels frame f1.
            down with frame f1.            
          end.
          else                          
          do:
            display space(19)
                    gltrans.trnum   format "9999999"
                    gltrans.jrnl    when op eq "D"
                    tmp-dscr        format "x(35)"
                    gltrans.tr-date format "99/99/99"
                    v-debit[1]      when v-debit[1] ne 0
                    v-crdit[1]      when v-crdit[1] ne 0
                with no-box stream-io width 200 no-attr-space no-labels frame f2.
            down with frame f2.
          end.
        end.
        else                          
        do:
          display space(19)
                  gltrans.trnum   format "9999999"
                  gltrans.jrnl    when op eq "D"
                  tmp-dscr        format "x(35)"
                  gltrans.tr-date format "99/99/99"
                  v-debit[1]      when v-debit[1] ne 0
                  v-crdit[1]      when v-crdit[1] ne 0
              with no-box stream-io width 200 no-attr-space no-labels frame f2.
          down with frame f2.
        end.
        assign v-debit[1] = 0
               v-crdit[1] = 0.

      end.

      if gltrans.tr-amt ge 0 then
        assign
          v-debit[2] = v-debit[2] + gltrans.tr-amt
          v-crdit[2] = v-crdit[2] + 0.
      else
        assign
          v-debit[2] = v-debit[2] + 0
          v-crdit[2] = v-crdit[2] + gltrans.tr-amt.
/*
      assign
       v-debit[2] = v-debit[2] + v-debit[1]
       v-crdit[2] = v-crdit[2] + v-crdit[1].
*/

      if last(gltrans.trnum) then do:
        PUT v-debit[2]                                   to 97 format "(>>>,>>>,>>9.99)"
            v-crdit[2]                                   to 114 format "(>>>,>>>,>>9.99)"
            v-debit[2] +
            v-crdit[2] +
            open-amt        format "->>,>>>,>>>,>>9.99" to 132 " *" skip
            skip(1).
        down.

        /* gdm - 10010905 */
        IF tb_excel THEN DO:
          PUT STREAM str-exl UNFORMATTED 
              .
        END.
        /* gdm - 10010905 end */

                /*djk*/
        tot-all = tot-all +  v-debit[2] + v-crdit[2] + open-amt.

        assign
         v-debit[3] = v-debit[3] + v-debit[2]
         v-crdit[3] = v-crdit[3] + v-crdit[2]
         v-debit[2] = 0
         v-crdit[2] = 0.

      end.
    end.

    if last(account.actnum) then
      display "TOTAL"    to 68
              v-debit[3] to 97  format "(>>>,>>>,>>>,>>9.99)"
              tot-all    to 132 skip
              v-crdit[3] to 114 format "(>>>,>>>,>>>,>>9.99)"
          with frame r-cmon3 no-labels no-box stream-io width 200.
  end. /* each account */

/* gdm - 10010905 */
IF tb_excel THEN DO:
   OUTPUT STREAM str-exl CLOSE.

   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(TRIM(fi_file))).
END.
/* gdm - 10010905 */

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

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

