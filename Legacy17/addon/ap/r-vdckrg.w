&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap\r-vdchrg.w

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
DEF VAR lv-audit-dir AS CHAR NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

DO TRANSACTION:
    {sys/inc/aplockbx.i}    
END.

assign
 cocode = gcompany
 locode = gloc.
def buffer xap-payl for ap-payl.    

def new shared var v-trnum as INT NO-UNDO.    
def var v-postable as log init NO NO-UNDO.
def var time_stamp as character NO-UNDO.

def TEMP-TABLE xpayl NO-UNDO field recnum as recid.

def TEMP-TABLE w-disb NO-UNDO field w-actnum   like ap-payl.actnum
                    field w-amt-paid like ap-payl.amt-paid
                    field w-amt-disc like ap-payl.amt-disc.

{sys/form/r-top3w.f}

time_stamp = string(time, "HH:MMam").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date lines-per-page ~
rd-dest lv-ornt lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period lines-per-page ~
rd-dest lv-ornt lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 104 BY 7.14.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 104 BY 9.52.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 3.86 COL 39 COLON-ALIGNED
     tran-period AT ROW 5.05 COL 39 COLON-ALIGNED
     lines-per-page AT ROW 11.95 COL 73 COLON-ALIGNED
     rd-dest AT ROW 12.19 COL 11 NO-LABEL
     lv-ornt AT ROW 13.38 COL 49 NO-LABEL
     lv-font-no AT ROW 15.05 COL 47 COLON-ALIGNED
     lv-font-name AT ROW 16 COL 39 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.95 COL 10
     btn-ok AT ROW 18.86 COL 23
     btn-cancel AT ROW 18.86 COL 58
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11 COL 6
     RECT-6 AT ROW 10.76 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 104.6 BY 19.86.


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
         TITLE              = "A/P Voided Check Register"
         HEIGHT             = 20.29
         WIDTH              = 105.2
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


/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
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
ON END-ERROR OF C-Win /* A/P Voided Check Register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* A/P Voided Check Register */
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
  DEF VAR lv-bank-file AS cha NO-UNDO.

  run check-date.

  assign rd-dest tran-period tran-date .

  if v-invalid then return no-apply.
  assign rd-dest tran-period tran-date .

  DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
   /* gdm - 11050906 */
   REPEAT:
     FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
     IF AVAIL gl-ctrl THEN DO:
       ASSIGN v-trnum       = gl-ctrl.trnum + 1
              gl-ctrl.trnum = v-trnum.
       RELEASE gl-ctrl.       
       LEAVE.
     END. /* IF AVAIL gl-ctrl */
   END. /* REPEAT */
   /* gdm - 11050906 */

  END.

  run run-report.

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case.

  IF v-postable THEN DO:    
    lv-post = NO.
    MESSAGE "Do you want to post voided checks?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-post.

    IF lv-post THEN do:  
      IF aplockbx-log THEN DO:
         RUN create-bank-file (OUTPUT lv-bank-file).
         MESSAGE "Check Register/Lock Box file is created into " 
              aplockbx-path + lv-bank-file
            VIEW-AS ALERT-BOX INFO.
      END.
      RUN post-gl.
      RUN copy-report-to-audit-dir.
      MESSAGE "Posting complete. " VIEW-AS ALERT-BOX.
    END.
    ELSE RUN undo-trnum.
  END.
  ELSE do:
      MESSAGE "No Void Checks available for posting..." VIEW-AS ALERT-BOX ERROR.
      RUN undo-trnum.
  END.
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
  ASSIGN lines-per-page = 68.
  DISP lines-per-page WITH FRAME {&FRAME-NAME}.
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
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.
  tran-date = TODAY.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-bank-file C-Win 
PROCEDURE create-bank-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER op-data-file AS cha NO-UNDO.

  DEF VAR targetfile AS CHAR FORMAT "X(50)" NO-UNDO.
  DEF VAR dirname1 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR v-account AS CHAR NO-UNDO.
  DEF VAR v-ref AS cha NO-UNDO.
  DEF VAR v-check-date AS DATE NO-UNDO.
  DEF VAR v-check-date-string AS cha NO-UNDO.
  DEF VAR v-total-amt AS DEC NO-UNDO.

  ASSIGN targetfile = aplockbx-path +
                     "CheckRegister" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") +
                      STRING(DAY(TODAY),"99") + STRING(TIME) + ".txt"
         dirname1 = aplockbx-path
         .

  IF SEARCH(dirname1) EQ ? THEN DO:
    OS-CREATE-DIR VALUE(dirname1).
  END.


  OUTPUT TO VALUE(targetfile).
  PUT UNFORMATTED "01021226C3648091" SKIP.
  v-total-amt = 0.

  for each ap-pay where ap-pay.company = cocode and
                        ap-pay.cleared = ? and
                        ap-pay.reconciled = NO NO-LOCK:

      FIND FIRST vend WHERE vend.company = ap-pay.company
                        AND vend.vend-no = ap-pay.vend-no NO-LOCK NO-ERROR.

      find first bank where bank.company = cocode and
                             bank.bank-code = ap-pay.bank-code NO-LOCK no-error.

      ASSIGN
      v-account = IF AVAIL bank THEN bank.bk-act ELSE ""
      v-ref = IF AVAIL vend THEN SUBSTRING(vend.name,1,12) ELSE ""
      v-check-date = ap-pay.check-date
      v-check-date-string = STRING(MONTH(v-check-date),"99") +
                            STRING(DAY(v-check-date),"99") + 
                            SUBstring(STRING(YEAR(v-check-date),"9999"),3,2)
      v-total-amt = v-total-amt + ap-pay.check-amt.
      PUT UNFORMATTED "V"
          ap-pay.check-no FORM "9999999999"
          v-account FORM "99999999999999"
          ap-pay.check-amt * 100 FORM "9999999999"
          v-ref FORM  "x(12)"
          v-check-date-string FORM "x(6)"
          SPACE(25)
          "38"  /*for void*/
          SKIP.

  END.
  PUT UNFORMATTED "T          "
      v-account FORM "99999999999999"
      v-total-amt * 100 FORM "9999999999"
      SKIP.

  OUTPUT CLOSE.
  op-data-file = TARGETfile.

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
  DISPLAY tran-date tran-period lines-per-page rd-dest lv-ornt lv-font-no 
          lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date lines-per-page rd-dest lv-ornt lv-font-no 
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

     DEF VAR lv-orint AS INT NO-UNDO.
     lv-orint = IF lv-ornt BEGINS "L" THEN 3 ELSE 1.

     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT lv-font-no, INPUT lv-orint, INPUT 0, INPUT 0, OUTPUT result).

     IF NOT RESULT THEN v-postable = NO.

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
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR v-bank-amt AS DEC NO-UNDO.
 def var v-tot-amt-paid like ap-pay.check-amt NO-UNDO.
 def var v-tot-amt-disc like ap-payl.amt-disc NO-UNDO.

  /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
 ASSIGN uperiod = tran-period
        udate = tran-date.

 EMPTY TEMP-TABLE w-disb.

postit:
do transaction on error undo, leave:
   find first ap-ctrl where ap-ctrl.company = cocode no-lock no-error.

   for each ap-pay where ap-pay.company = cocode and
                         ap-pay.cleared = ? and
                         ap-pay.reconciled = no
                         on error undo postit, leave postit
                         BREAK BY ap-pay.bank-code:

       assign ap-pay.cleared = NO
              ap-pay.reconciled = ?
              v-tot-amt-paid = v-tot-amt-paid + ap-pay.check-amt
              v-bank-amt = v-bank-amt + ap-pay.check-amt.

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
         ap-ledger.trnum = v-trnum
         ap-ledger.period = tran-period
         ap-ledger.amt = ap-ledger.amt - ap-pay.check-amt
         ap-ledger.actnum = bank.actnum.
       RELEASE ap-ledger.

       EMPTY TEMP-TABLE xpayl.

       for each ap-payl NO-LOCK where ap-payl.c-no = ap-pay.c-no:
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
           RELEASE ap-inv.
           RELEASE vend.

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
          RELEASE xap-payl.
       end.  /* for each xpayl */
       IF LAST-OF(ap-pay.bank-code) THEN DO:
          create gltrans.
          ASSIGN gltrans.company = cocode
                 gltrans.actnum  = bank.actnum
                 gltrans.jrnl    = "APVOIDCK"
                 gltrans.tr-dscr = "AP VOIDED CHECK REGISTER"
                 gltrans.tr-date = udate
                 gltrans.tr-amt  = v-bank-amt
                 gltrans.period  = tran-period
                 gltrans.trnum   = v-trnum
                 v-bank-amt = 0.
          RELEASE gltrans.
       END.

       RELEASE bank.
   end. /* for each ap-pay record */

   FIND CURRENT ap-pay NO-LOCK NO-ERROR.

   /* old way -> changed to post by bank account
   create gltrans.
   assign
    gltrans.company = cocode
    gltrans.actnum  = bank.actnum
    gltrans.jrnl    = "APVOIDCK"
    gltrans.tr-dscr = "AP VOIDED CHECK REGISTER"
    gltrans.tr-date = udate
    gltrans.tr-amt  = v-tot-amt-paid
    gltrans.period  = tran-period
    gltrans.trnum   = v-trnum.
   */
   if v-tot-amt-disc ne 0 then do:
     create gltrans.
     assign
      gltrans.company = cocode
      gltrans.actnum  = ap-ctrl.discount
      gltrans.jrnl    = "APVOIDCK"
      gltrans.tr-dscr = "AP VOIDED CHECK REGISTER"
      gltrans.tr-date = udate
      gltrans.tr-amt  = v-tot-amt-disc
      gltrans.period  = tran-period
      gltrans.trnum   = v-trnum.
     RELEASE gltrans.
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
        gltrans.period  = tran-period
        gltrans.trnum   = v-trnum.
       RELEASE gltrans.
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
    gltrans.period  = tran-period
    gltrans.trnum   = v-trnum.
   RELEASE gltrans.

end. /* postit */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- oe/invpost.p 10/94 gb */
/* Invoicing  - Edit Register & Post Invoicing Transactions                   */
/* -------------------------------------------------------------------------- */

  form HEADER
         "Check #" at 1
         "Date" at 17
         "Amount" at 33
         "Vendor" at 59 skip
         fill("_",132) format "x(130)" skip
         with STREAM-IO frame f-top width 132 no-box no-labels no-underline.

  ASSIGN
     time_stamp = string(time, "hh:mmam")
     tmpstore   = fill("_",125).             

  {sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

  SESSION:SET-WAIT-STATE("general").

  ASSIGN
   str-tit  = coname + " - " + loname                                    
   str-tit2 = "A/P VOIDED CHECK REGISTER " + STRING(v-trnum)
   str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date)
   x = (112 - length(str-tit)) / 2
   str-tit  = fill(" ",x) + str-tit
   x = (114 - length(str-tit2)) / 2
     str-tit2 = fill(" ",x) + str-tit2
   x = (132 - length(str-tit3)) / 2
   str-tit3 = fill(" ",x) + str-tit3 .

display "" with frame r-top.
display "" with frame f-top.

v-postable = NO.

for each ap-pay where ap-pay.company = cocode and
                      ap-pay.cleared = ? and
                      ap-pay.reconciled = no
                      NO-LOCK
                      break by ap-pay.check-no:
    find first vend where vend.company = cocode and
                          vend.vend-no = ap-pay.vend-no no-lock no-error.
    display ap-pay.check-no
            ap-pay.check-date
            ap-pay.check-amt
            ap-pay.vend-no
            vend.name when avail vend
            with frame report-lines width 132 no-box NO-LABELS STREAM-IO.
    down with frame report-lines.
    assign v-postable = YES.
end. /* for each ap-pay record */

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
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + ",".
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name THEN
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".

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
        IF v-trnum EQ gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
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

  ASSIGN targetfile = lv-audit-dir + "\AP\VC5\Run#"
                    + STRING(v-trnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AP"
         dirname3 = lv-audit-dir + "\AP\VC5".

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
