&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-inve&p.w

  Description: Invoice Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 05/07/02

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
DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-postable AS LOG NO-UNDO.
DEF VAR tran-period AS INT NO-UNDO.
DEF VAR tran-date AS DATE INIT TODAY NO-UNDO.
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


DEFINE STREAM s-input.

DEF var top-debug       AS logical NO-UNDO.
DEF var time_stamp      AS ch.

DEF var in-file-name    AS char init "xport.txt" FORMAT "x(40)" NO-UNDO.
DEF var qtr-file-name   AS char NO-UNDO.

DEF var v-payroll-system AS char NO-UNDO LABEL "Payroll Type".
DEF var v-bank-code     LIKE ap-pay.bank-code NO-UNDO.
DEF var save_id         AS RECID.

/* variables for MAS/90 payroll check register interface */
DEF var v-check-number  AS char NO-UNDO.
DEF var v-emp-number    AS char NO-UNDO.
DEF var v-check-date    AS date NO-UNDO.       /* 9809 CAH was char */
DEF var v-check-amt     AS dec NO-UNDO.
DEF var v-check-name    AS char NO-UNDO.
DEF var v-check-type    AS char NO-UNDO.
DEF var v-check-period  AS char NO-UNDO.
DEF var v-check-increment AS int NO-UNDO.
DEF var char_date       AS char NO-UNDO.
DEF var last_bank-code  AS char NO-UNDO.
DEF var last_trandate   AS date NO-UNDO.
DEF var ws_cashacct     AS char NO-UNDO.
DEF var write_it        AS logical NO-UNDO.

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

FIND FIRST sys-ctrl
  WHERE sys-ctrl.company eq cocode
  AND sys-ctrl.name    eq "PAYBANK"
  NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN
DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
    sys-ctrl.company = cocode
    sys-ctrl.name    = "PAYBANK"
    sys-ctrl.descrip = "Payroll Bank Code for check register import".
  MESSAGE "System control record NOT found.  Please enter the Bank Code"
    UPDATE sys-ctrl.char-fld.
END.
v-bank-code = sys-ctrl.char-fld.

DEF var tot_recs AS int LABEL "Records".
DEF var tot_amt  AS decimal FORMAT "->>>,>>>,>>>.99CR" LABEL "Amount".

FORM
  tot_recs tot_amt
  WITH FRAME f-hashtot side-labels
  TITLE " Totals " row 15 CENTER OVERLAY.

time_stamp = string(TIME, "HH:MMam").
tmpstore   = FILL("_",125).

{sys/form/r-topw.f}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-payroll-sys lv-imp-file btn-ok btn-cancel ~
RECT-7 
&Scoped-Define DISPLAYED-OBJECTS lv-payroll-sys lv-imp-file 

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

DEFINE VARIABLE lv-imp-file AS CHARACTER FORMAT "X(40)":U 
     LABEL "Import from file" 
     VIEW-AS FILL-IN 
     SIZE 49 BY 1 NO-UNDO.

DEFINE VARIABLE lv-payroll-sys AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 8.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     lv-payroll-sys AT ROW 4.81 COL 5 COLON-ALIGNED NO-LABEL
     lv-imp-file AT ROW 4.81 COL 38 COLON-ALIGNED
     btn-ok AT ROW 12.43 COL 21
     btn-cancel AT ROW 12.43 COL 51
     RECT-7 AT ROW 1 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 2.19 COL 5
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
         TITLE              = "Check Register Import"
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
       lv-imp-file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       lv-payroll-sys:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Check Register Import */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Check Register Import */
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
/*
  assign rd-dest       
         lv-imp-file
         in-file-name = lv-imp-file
         .
  */
  IF SEARCH(in-file-name) eq ? THEN
  DO:
    MESSAGE "Could Not Find " in-file-name "!" VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.

  in-file-name = SEARCH(in-file-name).

  run check-date.
  if v-invalid then return no-apply.

  run run-report.

  /*
  case rd-dest:
         when 1 then run output-to-printer.
         when 2 then run output-to-screen.
         when 3 then run output-to-file.
  end case.

    */


    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */    

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

   IF v-payroll-system = "MAS90" OR v-payroll-system = "ASI" THEN
   DO:
       ASSIGN save_id = ?
              last_trandate = ?
              ws_cashacct = ""
              in-file-name = "PRCK" + cocode + "1.TXT"
              last_bank-code = v-bank-code.
              .
   END.
   ELSE IF v-payroll-system = "WESTIND" THEN
   DO:
        in-file-name = "apchk-" + cocode + ".txt".
   END.
   ELSE DO:
       MESSAGE "Unrecognized Payroll Vendor Type:" v-payroll-system VIEW-AS ALERT-BOX ERROR.  
       RETURN.
   END.

   FIND bank WHERE bank.company = cocode
               AND bank.bank-code = v-bank-code NO-LOCK NO-ERROR.
   IF NOT AVAIL bank THEN
   DO:
      MESSAGE "Bank" v-bank-code "must be defined in bank file" VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
   ASSIGN ws_cashacct = bank.actnum.


  ASSIGN lv-payroll-sys = v-payroll-system
         lv-imp-file = in-file-name.

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
    if avail period then tran-period = period.pnum.

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
  DISPLAY lv-payroll-sys lv-imp-file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE lv-payroll-sys lv-imp-file btn-ok btn-cancel RECT-7 
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

 /*    RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).  */ 

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
qtr-file-name = in-file-name + ".q".

  DEF var cmdline AS char NO-UNDO.
  cmdline = "quoter -d , " + in-file-name + " >" + qtr-file-name.

  IF OPSYS = "unix" THEN UNIX silent value(cmdline).
  ELSE IF OPSYS = "msdos" OR OPSYS BEGINS "win" THEN
  DOS silent value(cmdline).

  INPUT STREAM s-input FROM VALUE(qtr-file-name) no-echo.


  REPEAT TRANSACTION
      ON endkey UNDO, LEAVE:

    ASSIGN
      v-check-number = ""
      v-emp-number = ""
      v-check-date = ?
      v-check-amt = 0
      v-check-type = ""
      v-check-period = ""
      v-check-increment = 0
      .

    IF v-payroll-system = "WESTIND" THEN
    DO:
       IMPORT STREAM s-input
                  v-check-number
                  v-emp-number
                  v-check-date
                  v-check-amt
                  v-check-name
                  v-check-type
                  v-check-period.
       IF v-check-amt <> 0 THEN write_it = TRUE.
    END.
    ELSE IF v-payroll-system = "ASI" OR v-payroll-system = "MAS90" THEN
    DO:
       IMPORT STREAM s-input
                  v-bank-code
                  v-check-number
                  v-check-date
                  v-emp-number
                  v-check-increment
                  v-check-amt
                  .
       IF v-check-amt <> 0 THEN write_it = TRUE.
    END.    /* MAS90 payroll vendor type */

    IF v-bank-code <> last_bank-code THEN
    DO:
       FIND bank WHERE bank.company = cocode
                       AND bank.bank-code = v-bank-code NO-LOCK NO-ERROR.
       IF NOT AVAIL bank THEN DO:
              MESSAGE "Bank" v-bank-code "is undefined in Co#"
                      cocode "- Processing aborted (003)" VIEW-AS ALERT-BOX ERROR.      
              RETURN ERROR.
      END.
      ELSE ASSIGN ws_cashacct = bank.actnum
                      last_bank-code = v-bank-code.

      IF top-debug THEN
          RUN rc/debugmsg.p ("Got Bank: " + v-bank-code + " act " + ws_cashacct).
    END.    /* bank code change */

    IF write_it THEN
    DO:
       FIND ap-pay WHERE ap-pay.company = cocode
                         AND ap-pay.check-no = integer(v-check-number)
                         AND ap-pay.check-act = bank.actnum EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL ap-pay THEN
      DO:
         MESSAGE "Check" v-check-number
                     "already in Co" cocode "for Acct" bank.actnum VIEW-AS ALERT-BOX.
             write_it = FALSE.
             UNDO, NEXT.
      END.

      DEF buffer xap-pay FOR ap-pay.
      DO :
             FIND LAST xap-pay USE-INDEX c-no NO-LOCK NO-ERROR.
             x = IF AVAIL xap-pay THEN xap-pay.c-no ELSE 0.
      END.

      CREATE ap-pay.
      ASSIGN ap-pay.c-no = x + 1
                 ap-pay.company = cocode
                 ap-pay.check-no = integer(v-check-number)
                 ap-pay.vend-no = ""
                 ap-pay.man-check = TRUE
                 ap-pay.posted = TRUE
                 ap-pay.cleared = FALSE
                 ap-pay.period = uperiod
                 ap-pay.memo = FALSE
                 ap-pay.check-act = ws_cashacct
                 ap-pay.bank-code = v-bank-code
                 ap-pay.reconciled = FALSE
                 ap-pay.d-no = 0.

      IF NEW(ap-pay) OR (ap-pay.check-amt <> v-check-amt OR ap-pay.check-date <> v-check-date)
          THEN    DO:
              /* 9812 CAH: v7+ only: RUN xcalper.ip (INPUT v-check-date, OUTPUT ap-pay.period). */
              DEF var p_date AS date NO-UNDO.
              DEF var p_per AS int NO-UNDO.

              p_date = v-check-date.
              IF p_date = ? THEN p_date =  TODAY.

              FIND FIRST period WHERE period.company = cocode
                                  AND period.pst <= p_date
                                  AND period.pend >= p_date NO-LOCK NO-ERROR.
              IF NOT AVAIL period THEN p_per = month(p_date).
          ELSE p_per = period.pnum.

              ASSIGN ap-pay.check-amt = v-check-amt
                     ap-pay.check-date = v-check-date
                     ap-pay.period = p_per.

              IF top-debug THEN    RUN rc/debugrec.p ("", RECID(ap-pay)) "ap-pay".
      END.

      ASSIGN write_it = FALSE
                 tot_recs = tot_recs + 1
                 tot_amt  = tot_amt + ap-pay.check-amt.

    END.    /* write it true */

  END. /* repeat transaction */

  INPUT STREAM s-input close.

  DISPLAY
    tot_recs
    tot_amt
    WITH FRAME f-hashtot.
  PAUSE 5.

  DEF var save_name AS char NO-UNDO.

  if search("osdelete.r") <> ? then do:
     save_name = in-file-name + "-" + string(TODAY,"999999").
     MESSAGE "Saving " in-file-name " as " save_name VIEW-AS ALERT-BOX.
     IF SEARCH(save_name) <> ? THEN run osdelete.p (save_name).
     run oscopy.p (in-file-name, save_name).
     run osdelete.p (in-file-name).
     run osdelete.p (qtr-file-name). 
  end.



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

