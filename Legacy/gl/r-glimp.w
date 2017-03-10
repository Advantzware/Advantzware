&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: gl\r-glimp.w

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



DEF VAR v-invalid AS LOG NO-UNDO.
DEFINE STREAM s-input.

DEF var top-debug       AS logical NO-UNDO initial false.
DEF var time_stamp      AS ch.
DEF var save_id         AS RECID.


DEF var qtr-file-name   AS char NO-UNDO.
DEF var save_name       AS char NO-UNDO.

DEF var cmdline         AS char NO-UNDO.
DEF var v-amount        AS dec NO-UNDO.
DEF var v-acct          AS char NO-UNDO.

DEF var v-date          AS date NO-UNDO.
DEF var v-jsc           AS char NO-UNDO.
DEF var v-seq           AS char NO-UNDO.
DEF var v-source        AS char NO-UNDO.
DEF var v-line          AS int NO-UNDO.

DEF var tot_recs AS int LABEL "Records".
DEF var tot_amt  AS decimal FORMAT "->>>,>>>,>>>.99CR" LABEL "Amount".
DEF var xtrnum AS int.
def var v-tmp as char initial "" no-undo.
def var v-start as int initial 1 no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tran-date in-file-name btn-ok btn-cancel ~
RECT-7 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period v-payroll-system ~
in-file-name 

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

DEFINE VARIABLE in-file-name AS CHARACTER FORMAT "X(256)":U 
     LABEL "Import From File" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE v-payroll-system AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 10.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.91 COL 39 COLON-ALIGNED
     tran-period AT ROW 4.1 COL 39 COLON-ALIGNED
     v-payroll-system AT ROW 6.71 COL 5 COLON-ALIGNED NO-LABEL
     in-file-name AT ROW 6.71 COL 37 COLON-ALIGNED
     btn-ok AT ROW 18.38 COL 23
     btn-cancel AT ROW 18.38 COL 58
     RECT-7 AT ROW 1 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
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
         TITLE              = "Journal Entries Import"
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
                                                                        */
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
/* SETTINGS FOR FILL-IN v-payroll-system IN FRAME FRAME-A
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
ON END-ERROR OF C-Win /* Journal Entries Import */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Journal Entries Import */
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
  DEF VAR lv-post AS LOG NO-UNDO.

  run check-date.
  if v-invalid then return no-apply.

  IF SEARCH(in-file-name) eq ? THEN
  DO:
    MESSAGE "ERROR: Could Not Find " in-file-name "!" VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO in-file-name.
    RETURN NO-apply.
  END.

  run run-report.
  /*
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case.
  */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME in-file-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL in-file-name C-Win
ON LEAVE OF in-file-name IN FRAME FRAME-A /* Import From File */
DO:
  IF LASTKEY = -1 THEN RETURN.

  IF SEARCH(in-file-name) eq ? THEN
  DO:
    MESSAGE "ERROR: Could Not Find " in-file-name "!" VIEW-AS ALERT-BOX ERROR.
    RETURN NO-apply.
  END.
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:


  tran-date = TODAY.
  RUN init-proc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN  APPLY "close" TO THIS-PROCEDURE.


  RUN enable_UI.

  RUN check-date.


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
  DISPLAY tran-date tran-period v-payroll-system in-file-name 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE tran-date in-file-name btn-ok btn-cancel RECT-7 
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
FIND FIRST sys-ctrl
  WHERE sys-ctrl.company eq cocode
  AND sys-ctrl.name    eq "PAYVEND"
  NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN
DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
    sys-ctrl.company = cocode
    sys-ctrl.name    = "PAYVEND"
    sys-ctrl.descrip = "Payroll Vendor for check register import".
  MESSAGE "System control record NOT found.  Please enter the payroll vendor"
    UPDATE sys-ctrl.char-fld.
END.
v-payroll-system = sys-ctrl.char-fld.

IF v-payroll-system = "MAS90"
  OR v-payroll-system = "ASI" THEN
DO:
  ASSIGN
    save_id = ?
    in-file-name = "PRGL" + cocode + "1.TXT"
    .
END.
ELSE IF v-payroll-system = "WESTIND" THEN
DO:
  in-file-name = "gltrn-" + cocode + ".txt".
END.
ELSE DO:  
  MESSAGE "Unrecognized Payroll Vendor Type:" v-payroll-system VIEW-AS ALERT-BOX ERROR.  
  RETURN ERROR.
END.


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
     /*IF NOT RESULT THEN v-postable = NO.*/

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
/*  {sys/inc/print1.i}

  {sys/inc/outprint.i VALUE(lines-per-page)}

  IF td-show-parm THEN RUN show-param.
*/  
SESSION:SET-WAIT-STATE("general").
in-file-name = SEARCH(in-file-name).
OUTERs:
DO ON ERROR UNDO:
  IF v-payroll-system = "WESTIND" THEN
  DO:
    ASSIGN
      qtr-file-name   = in-file-name
      v-date          = tran-date
      v-jsc           = "PR"
      v-seq           = "000"
      v-source        = "PR".
  END.  /* WESTIND */
  ELSE
  DO:
    qtr-file-name = in-file-name + ".q".

    cmdline = "quoter -d , " + in-file-name + " >" + qtr-file-name.

    IF OPSYS = "unix" THEN
    UNIX silent value(cmdline).
    ELSE
    IF OPSYS = "msdos" OR OPSYS BEGINS "win" THEN
    DOS silent value(cmdline).

  END.  /* else do */

  INPUT STREAM s-input FROM VALUE(qtr-file-name) no-echo.

  DO TRANSACTION:
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        ASSIGN xtrnum = gl-ctrl.trnum + 1
               gl-ctrl.trnum = xtrnum.
        FIND CURRENT gl-ctrl NO-LOCK.
        RELEASE gl-ctrl.
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */

    CREATE gl-jrn.

    ASSIGN
      gl-jrn.company = cocode
      gl-jrn.j-no = xtrnum
      gl-jrn.journal = xtrnum
      gl-jrn.tr-date = tran-date
      gl-jrn.tr-amt = 0
      gl-jrn.tcred = 0
      gl-jrn.tdeb = 0
      gl-jrn.period = tran-period
      gl-jrn.posted = FALSE
      save_id = RECID(gl-jrn).

  END. /* transaction */

  IF KEYFUNCTION(LASTKEY) = "end-error" THEN
  DO:
    HIDE ALL.
    LEAVE.
  END.

  v-line = 0.

  REPEAT TRANSACTION
      ON endkey UNDO, LEAVE:

    ASSIGN
      v-amount = 0
      v-acct = "".

    IF v-payroll-system = "WESTIND"
      THEN
    DO:
      IMPORT STREAM s-input
        v-acct v-amount.
    END.
    ELSE
    IF v-payroll-system = "MAS90"
      OR v-payroll-system = "ASI"
      THEN
    DO:
      IMPORT STREAM s-input
        v-date v-jsc v-acct v-seq v-source v-amount.
      IF top-debug THEN
      DO:
        DISPLAY v-date v-jsc v-acct v-seq v-source v-amount
          WITH FRAME f-debug CENTER OVERLAY 1 COLUMN.
        PAUSE.
      END.

      /* Code to import correct account number from MAS90 */
      find first company where company.company eq cocode no-lock no-error.
      if avail company then
      do: 
        assign v-tmp = "".
        do i = 1 to company.acc-level:
          assign v-start = 1.
          if i ne 1 then
          do x = 1 to i - 1:
            v-start = v-start + company.acc-dig[x].
          end.
          assign v-tmp = v-tmp + substring(v-acct,v-start,company.acc-dig[i]).
          if company.acc-level gt 1 and i ne company.acc-level then
            v-tmp = v-tmp + "-".
        end.
      end.
      assign v-acct = v-tmp.

    END.

    IF v-amount <> 0 THEN
    DO:

      FIND account NO-LOCK WHERE
        account.company = cocode AND
        account.actnum = v-acct NO-ERROR.

      IF NOT AVAILABLE account THEN
      MESSAGE "WARNING: Account "
        v-acct " was not found! ".

      FIND gl-jrn WHERE RECID(gl-jrn) = save_id EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL gl-jrn THEN
      DO:
        BELL.
        MESSAGE "Could not locate header when creating detail" v-acct.
        PAUSE.
        RETURN.
      END.

      CREATE gl-jrnl.
      ASSIGN
        v-line = v-line + 1
        gl-jrnl.j-no = gl-jrn.j-no
        gl-jrnl.line = v-line
        gl-jrnl.actnum = v-acct
        gl-jrnl.dscr = "Payroll G/L Entry Import"
        gl-jrnl.tr-amt = v-amount
        gl-jrn.tr-amt = gl-jrn.tr-amt + gl-jrnl.tr-amt
        gl-jrn.tcred = gl-jrn.tcred +
        (IF gl-jrnl.tr-amt < 0 THEN gl-jrnl.tr-amt ELSE 0)
        gl-jrn.tdeb = gl-jrn.tdeb +
        (IF gl-jrnl.tr-amt > 0 THEN gl-jrnl.tr-amt ELSE 0)
        .

      ASSIGN
        tot_recs = tot_recs + 1
        tot_amt = tot_amt + v-amount.

    END.    /* got a nonzero amount */

  END. /* repeat transaction */

  DISPLAY
    tot_recs
    tot_amt
    WITH FRAME f-hashtot.

  if search("osdelete.r") <> ? then do:
  save_name = in-file-name + "-" + string(TODAY,"999999").

  HIDE MESSAGE NO-PAUSE.
  MESSAGE "Saving " in-file-name " as " save_name.
  IF SEARCH(save_name) <> ? THEN
  run osdelete.p (save_name).
  run oscopy.p (in-file-name, save_name).
  run osdelete.p (in-file-name).
  run osdelete.p (qtr-file-name).
  PAUSE 5.
  end.

  LEAVE outers.



END.



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
  /* gdm - 11050906 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

