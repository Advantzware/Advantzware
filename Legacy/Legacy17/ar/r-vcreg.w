&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-vcreg.w

  Description: Voided Cash Receipts Register

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
DEF VAR xar-acct AS CHAR NO-UNDO.
DEF VAR xdis-acct AS CHAR NO-UNDO.
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
def buffer xar-cashl for ar-cashl.    

def new shared var v-trnum as INT NO-UNDO.    
def var v-postable as log init NO NO-UNDO.

def var v-post as logical NO-UNDO.

def TEMP-TABLE xcashl NO-UNDO field recnum as recid.

{sys/form/r-top3w.f}

form ar-cash.check-no at 1 format "zzzzzzz9"
     ar-cash.check-date at 17
     ar-cash.check-amt at 33 format "-ZZ,ZZZ,ZZ9.99"
     ar-cash.cust-no at 59 space(1) cust.name
     with frame report-lines width 132 no-box NO-LABELS STREAM-IO.

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
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11 COL 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
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
         TITLE              = "A/R Voided Cash Receipt Register"
         HEIGHT             = 20.29
         WIDTH              = 105.2
         MAX-HEIGHT         = 20.29
         MAX-WIDTH          = 105.2
         VIRTUAL-HEIGHT     = 20.29
         VIRTUAL-WIDTH      = 105.2
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
ON END-ERROR OF C-Win /* A/R Voided Cash Receipt Register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* A/R Voided Cash Receipt Register */
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
        FIND CURRENT gl-ctrl NO-LOCK.
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

    MESSAGE "Do you want to post voided cash receipts?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-post.

    IF lv-post THEN do:  
      RUN post-gl.
      RUN copy-report-to-audit-dir.
      MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
    END.
    ELSE RUN undo-trnum.
  END.

  ELSE do:
      MESSAGE "No Voided Cash Receipts available for posting..." VIEW-AS ALERT-BOX ERROR.
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

  RUN init-proc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  tran-date = TODAY.

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

  ASSIGN targetfile = lv-audit-dir + "\AR\AC8\Run#"
                    + STRING(v-trnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AR"
         dirname3 = lv-audit-dir + "\AR\AC8".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc C-Win 
PROCEDURE init-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST ar-ctrl WHERE ar-ctrl.company eq cocode NO-LOCK.
  ASSIGN
    xar-acct  = ar-ctrl.receivables
    xdis-acct = ar-ctrl.discount.

  find first account where account.company eq cocode
                       and account.actnum  eq xar-acct no-lock no-error.
  if not avail account or account.actnum eq "" then do:
    message " Receivables Account is blank or is not on file for this Company." VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  end.
  find first account where account.company eq cocode
                       and account.actnum  eq xdis-acct
      no-lock no-error.
  if not avail account or account.actnum eq "" then do:

    message " Discount Account is blank or is not on file for this Company." VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
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
     DEF VAR lv-orint AS INT NO-UNDO.
     lv-orint = IF lv-ornt BEGINS "L" THEN 3 ELSE 1.

     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT lv-font-no, INPUT lv-orint, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */



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
 /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
 DEF VAR t1 AS DEC NO-UNDO.
 DEF VAR lv-rowid AS ROWID NO-UNDO.

 postit:
 do transaction on error undo, leave:

   for each ar-cash EXCLUSIVE-LOCK where
       ar-cash.company = cocode and
       ar-cash.cleared = ? and
       ar-cash.posted = YES, 
       FIRST cust WHERE
             cust.company EQ cocode AND
             cust.cust-no EQ ar-cash.cust-no
        EXCLUSIVE-LOCK
        on error undo postit, leave postit:

       assign t1 = 0
              ar-cash.cleared = NO
              ar-cash.reconciled = ?.

       find first bank where
            bank.company = cocode and
            bank.bank-code = ar-cash.bank-code
            no-error.

       if AVAIL bank then
       DO:
          bank.bal = bank.bal - ar-cash.check-amt.
          RELEASE bank.
       END.

       EMPTY TEMP-TABLE xcashl.

       for each ar-cashl where ar-cashl.c-no = ar-cash.c-no
           EXCLUSIVE-LOCK:

           create xcashl.   

           xcashl.recnum = recid(ar-cashl).   

           IF ar-cashl.inv-no NE 0 AND ar-cashl.on-account EQ NO THEN DO:

              find first ar-inv where
                   ar-inv.company = cocode and
                   ar-inv.inv-no = ar-cashl.inv-no
                   EXCLUSIVE-LOCK no-error.

              if AVAIL ar-inv then
              do:
                 assign
                 ar-inv.paid = ar-inv.paid - ar-cashl.amt-paid - ar-cashl.amt-disc
                 ar-inv.disc-taken = ar-inv.disc-taken + ar-cashl.amt-disc
                 ar-inv.due = ar-inv.due + ar-cashl.amt-paid + ar-cashl.amt-disc.

                 RELEASE ar-inv.

              end. /* if avail ar-inv  */

           END. /*inv-no ne 0 AND ar-cashl.on-account EQ NO*/

           IF ar-cashl.inv-no EQ 0 AND ar-cashl.on-account EQ YES THEN
              cust.on-account = cust.on-account - ar-cashl.amt-paid.

           CREATE gltrans.
           ASSIGN
             t1 = t1 + ar-cashl.amt-paid + ar-cashl.amt-disc
             gltrans.company   = cocode
             gltrans.actnum    = ar-cashl.actnum
             gltrans.jrnl      = "CASHRVD"
             gltrans.tr-dscr   = "VOID " + cust.cust-no + " " +
                                 STRING(ar-cash.check-no,"9999999999") +
                                 " Inv# " + STRING(ar-cashl.inv-no)
             gltrans.tr-date   = tran-date
             gltrans.tr-amt    = -1 * ar-cashl.amt-paid
             gltrans.period    = tran-period
             gltrans.trnum     = v-trnum
             /*ar-cashl.amt-paid = -1 * ar-cashl.amt-paid*/ .

           RELEASE gltrans.

           IF ar-cashl.amt-disc NE 0 THEN DO:
             CREATE gltrans.
             ASSIGN
              gltrans.company = cocode
              gltrans.actnum  = xdis-acct
              gltrans.jrnl    = "CRDISVD"
              gltrans.tr-dscr = "VOID " + cust.cust-no + " " +
                                STRING(ar-cash.check-no,"9999999999") +
                                " Inv# " + STRING(ar-cashl.inv-no)
              gltrans.tr-date = tran-date
              gltrans.tr-amt  = -1 * ar-cashl.amt-disc
              gltrans.period  = tran-period
              gltrans.trnum   = v-trnum.

             RELEASE gltrans.

             CREATE ar-ledger.
             ASSIGN
              ar-ledger.company  = cocode
              ar-ledger.cust-no  = ar-cash.cust-no
              ar-ledger.amt      = -1 * ar-cashl.amt-disc
              ar-ledger.ref-num  = "DISC VD" +
                                   STRING(ar-cash.check-no,"9999999999") +
                                   "-" + STRING(ar-cashl.line,"9999999999")
              ar-ledger.ref-date = ar-cash.check-date
              ar-ledger.tr-date  = tran-date
              ar-ledger.tr-num   = v-trnum.

             RELEASE ar-ledger.
           END.
       end. /* for each ar-cashl record */

       cust.acc-bal = cust.acc-bal + t1.

       IF cust.acc-bal GE cust.hibal THEN
          ASSIGN
             cust.hibal      = cust.acc-bal
             cust.hibal-date = tran-date.

       IF t1 NE 0 THEN DO:
          FIND gltrans WHERE ROWID(gltrans) EQ lv-rowid NO-ERROR.
          IF NOT AVAIL gltrans THEN DO:
            CREATE gltrans.
            ASSIGN
             gltrans.company = cocode
             gltrans.actnum  = xar-acct
             gltrans.jrnl    = "CASHRVD"
             gltrans.tr-dscr = "CASH RECEIPTS VOID"
             gltrans.tr-date = tran-date
             gltrans.period  = tran-period
             gltrans.trnum   = v-trnum
             lv-rowid        = ROWID(gltrans).
          END.
          gltrans.tr-amt = gltrans.tr-amt + t1.

          RELEASE gltrans.
       END.

       create ar-ledger.
       assign
        ar-ledger.company = cocode
        ar-ledger.cust-no = ar-cash.cust-no
        ar-ledger.amt = -1 * ar-cash.check-amt
        ar-ledger.ref-num = "VOIDED CHK# " + STRING(ar-cash.check-no,"9999999999")
        ar-ledger.ref-date = ar-cash.check-date
        ar-ledger.tr-date = tran-date
        ar-ledger.tr-num = v-trnum.

       RELEASE ar-ledger.

       for each xcashl,
           first ar-cashl where recid(ar-cashl) eq xcashl.recnum
           no-lock:

          find last xar-cashl where xar-cashl.c-no eq ar-cashl.c-no
              use-index c-no no-lock no-error.
          x = if avail xar-cashl then xar-cashl.line else 0.

          create xar-cashl.

          BUFFER-COPY ar-cashl EXCEPT LINE amt-disc amt-due amt-paid rec_key
                   TO xar-cashl 
             assign
              xar-cashl.line       = x + 1
              xar-cashl.amt-due    = ar-cashl.amt-due
              xar-cashl.amt-disc   = -(ar-cashl.amt-disc)

              xar-cashl.amt-paid   = -(ar-cashl.amt-paid).

          CREATE reftable.
          ASSIGN
            reftable.reftable = "ARCASHLVDDATE"
            reftable.rec_key = xar-cashl.rec_key
            reftable.CODE = STRING(tran-date,"99/99/9999").
          RELEASE reftable.

          RELEASE xar-cashl.
       end.  /* for each xcashl */
   end. /* for each ar-cash record */

 end. /* postit */

 FIND CURRENT cust NO-LOCK NO-ERROR.
 FIND CURRENT ar-cashl NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
form HEADER
         "Check #" at 1
         "Date" at 17
         "Amount" at 33
         "Customer" at 59 skip
         fill("_",132) format "x(130)" skip
         with STREAM-IO frame f-top width 132 no-box no-labels no-underline.

  ASSIGN
    tmpstore   = fill("_",125).             

  {sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

  SESSION:SET-WAIT-STATE("general").

  ASSIGN
   str-tit  = coname + " - " + loname                                    
   str-tit2 = "A/R VOIDED CASH RECEIPT REGISTER " + STRING(v-trnum)
   str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date)
   x = (112 - length(str-tit)) / 2
   str-tit  = fill(" ",x) + str-tit
   x = (114 - length(str-tit2)) / 2
     str-tit2 = fill(" ",x) + str-tit2
   x = (132 - length(str-tit3)) / 2
   str-tit3 = fill(" ",x) + str-tit3
   v-postable = NO.

display "" with frame r-top.
display "" with frame f-top.


for each ar-cash FIELDS(company cleared posted check-no check-date check-amt
    cust-no) where
    ar-cash.company = cocode and
    ar-cash.cleared = ? and
    ar-cash.posted = YES
    NO-LOCK
    break by ar-cash.check-no:

    find first cust where
         cust.company = cocode and
         cust.cust-no = ar-cash.cust-no
         no-lock no-error.

    display ar-cash.check-no FORMAT "ZZZZZZZZZ9"
            ar-cash.check-date
            ar-cash.check-amt
            ar-cash.cust-no
            cust.name when avail cust
            with frame report-lines width 132 no-box NO-LABELS STREAM-IO.
    down with frame report-lines.

    v-postable = YES.
end. /* for each ar-cash record */

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
        IF v-trnum EQ gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
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

