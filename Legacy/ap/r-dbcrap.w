&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap\r-dbcrap.w

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

DEF VAR g1 as dec format "->>>,>>>,>>9.99".
DEF VAR g2 as dec format "->>>,>>>,>>9.99".
DEF VAR xtrnum as int.
DEF VAR xap-acct as char.
DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-postable AS LOG NO-UNDO.

DEF TEMP-TABLE tt-rm-bin NO-UNDO LIKE rm-bin.

DO TRANSACTION:
  {sys/inc/postdate.i}
  {sys/inc/apcrmemo.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date begin_date end_date ~
rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_date end_date ~
rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Memo Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 5.48.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.67 COL 39 COLON-ALIGNED
     tran-period AT ROW 3.86 COL 39 COLON-ALIGNED
     begin_date AT ROW 5.14 COL 26.6 COLON-ALIGNED HELP
          "Enter Beginning Memo Date" WIDGET-ID 2
     end_date AT ROW 5.14 COL 63.4 COLON-ALIGNED HELP
          "Enter Ending Memo Date" WIDGET-ID 4
     rd-dest AT ROW 7.67 COL 9 NO-LABEL
     lv-ornt AT ROW 7.91 COL 30 NO-LABEL
     lines-per-page AT ROW 7.91 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 9.33 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 10.29 COL 27 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 11.95 COL 9
     btn-ok AT ROW 16 COL 21
     btn-cancel AT ROW 16 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .95 AT ROW 6.71 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 6.48 COL 1
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
         TITLE              = "Debit/Credit Memo Edit/Post Register"
         HEIGHT             = 18.48
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
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

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
ON END-ERROR OF C-Win /* Debit/Credit Memo Edit/Post Register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Debit/Credit Memo Edit/Post Register */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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

  RUN check-date.
  IF v-invalid THEN RETURN NO-APPLY. 

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     rd-dest
     tran-date
     tran-period.
  END.

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

  run run-report.

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case.

  find first ap-ctrl where ap-ctrl.company eq cocode no-lock no-error.
  if not avail ap-ctrl then return.
  IF AVAIL ap-ctrl  AND ap-ctrl.payables EQ "" THEN DO:
      MESSAGE "No AP control record found. " VIEW-AS ALERT-BOX .
      RETURN.
  END.

  IF v-postable THEN DO:    

    lv-post = NO.

    MESSAGE "Post Debit/Credit Memos?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-post.

    IF lv-post THEN DO:
       FOR EACH ap-pay
          WHERE ap-pay.company EQ cocode
            AND ap-pay.memo    EQ YES
            AND ap-pay.posted  EQ NO
            AND ap-pay.check-date GE begin_date
            AND ap-pay.check-date LE end_date,
          FIRST vend
          WHERE vend.company EQ ap-pay.company
            AND vend.vend-no EQ ap-pay.vend-no,
          EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no

          break by ap-pay.vend-no by ap-payl.inv-no by ap-payl.line:

         FIND FIRST ap-inv
             WHERE ap-inv.company EQ cocode
               AND ap-inv.vend-no EQ vend.vend-no
               AND ap-inv.inv-no  EQ ap-payl.inv-no
             NO-ERROR.
         IF AVAIL ap-inv AND ap-inv.stat EQ "H" THEN
             MESSAGE "Invoice is on hold - Are you sure you want it paid?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-post.
    END.
    END.
 

    IF lv-post THEN do:

      RUN post-gl.
      RUN copy-report-to-audit-dir.
      MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.     
    END.
    ELSE RUN undo-trnum.  
  END.

  ELSE do:
      MESSAGE "No Debit/Credit Memos available for posting..." VIEW-AS ALERT-BOX ERROR.
      RUN undo-trnum.  
  END.
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

  RUN init-proc.

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
  DISPLAY tran-date tran-period begin_date end_date rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date begin_date end_date rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
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

 find first ap-ctrl where ap-ctrl.company = cocode NO-LOCK.
 xap-acct = ap-ctrl.payables.
 release ap-ctrl.

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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no), lv-ornt). /* open file-name, title */ 
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
   DEF VAR t1 as dec.
   DEF VAR ld-pct AS DEC NO-UNDO.
   DEF VAR ld-cst AS DEC NO-UNDO.
   DEF VAR ld-qty AS DEC NO-UNDO.
   DEF VAR v-r-qty AS DEC NO-UNDO.
   DEF VAR v-i-qty AS DEC NO-UNDO.
   DEF VAR v-t-qty AS DEC NO-UNDO.
   DEF VAR lv-uom AS CHAR NO-UNDO.

   DEF BUFFER b-rm-bin FOR rm-bin.


   SESSION:SET-WAIT-STATE("general").

   EMPTY TEMP-TABLE tt-rm-bin.

   postit:
   do transaction on error undo:

      FOR EACH ap-pay
          WHERE ap-pay.company EQ cocode
            AND ap-pay.memo    EQ YES
            AND ap-pay.posted  EQ NO
            AND ap-pay.check-date GE begin_date
            AND ap-pay.check-date LE end_date,
          FIRST vend
          WHERE vend.company EQ ap-pay.company
            AND vend.vend-no EQ ap-pay.vend-no,
          EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no

          break by ap-pay.vend-no by ap-payl.inv-no by ap-payl.line
          on error undo postit, leave postit:

         FIND FIRST ap-inv
             WHERE ap-inv.company EQ cocode
               AND ap-inv.vend-no EQ vend.vend-no
               AND ap-inv.inv-no  EQ ap-payl.inv-no
             NO-ERROR.
         IF AVAIL ap-inv THEN DO:
           ASSIGN
            ap-inv.paid = ap-inv.paid + ap-payl.amt-paid - ap-payl.amt-disc
            ap-inv.due  = ap-inv.net - ap-inv.paid - ap-inv.disc-taken.

           IF apcrmemo-log AND (ap-payl.amt-paid - ap-payl.amt-disc) GT 0 THEN DO:
             ld-cst = 0.

             FOR EACH rm-rdtlh NO-LOCK
                 WHERE rm-rdtlh.company      EQ ap-inv.company
                   AND rm-rdtlh.receiver-no BEGINS STRING(ap-inv.i-no,"9999999999")
                   AND rm-rdtlh.rita-code   EQ "R"
                 USE-INDEX receiver-no,
                 FIRST rm-rcpth NO-LOCK
                 WHERE rm-rcpth.r-no      EQ rm-rdtlh.r-no
                   AND rm-rcpth.rita-code EQ rm-rdtlh.rita-code:
                   ld-cst = ld-cst +
                           (rm-rdtlh.qty * rm-rdtlh.cost) + rm-rdtlh.frt-cost. 
             END.

             ld-pct = 1 - ((ap-payl.amt-paid - ap-payl.amt-disc) / ld-cst).
             IF ld-pct LT 0 THEN ld-pct = 0.
             IF ld-pct EQ ? THEN ld-pct = 1.

             IF ld-pct NE 1 THEN
                FOR EACH rm-rdtlh
                    WHERE rm-rdtlh.company      EQ ap-inv.company
                      AND rm-rdtlh.receiver-no BEGINS STRING(ap-inv.i-no,"9999999999")
                      AND rm-rdtlh.rita-code   EQ "R"
                    USE-INDEX receiver-no,
                    FIRST rm-rcpth NO-LOCK
                    WHERE rm-rcpth.r-no      EQ rm-rdtlh.r-no
                      AND rm-rcpth.rita-code EQ rm-rdtlh.rita-code:

                  ASSIGN
                   rm-rdtlh.cost     = rm-rdtlh.cost * ld-pct
                   rm-rdtlh.frt-cost = rm-rdtlh.frt-cost * ld-pct.

                  IF NOT CAN-FIND(FIRST tt-rm-bin
                                  WHERE tt-rm-bin.company EQ rm-rcpth.company
                                    AND tt-rm-bin.i-no    EQ rm-rcpth.i-no
                                    AND tt-rm-bin.loc     EQ rm-rdtlh.loc
                                    AND tt-rm-bin.loc-bin EQ rm-rdtlh.loc-bin
                                    AND tt-rm-bin.tag     EQ rm-rdtlh.tag) THEN DO:
                    CREATE tt-rm-bin.
                    ASSIGN
                     tt-rm-bin.company = rm-rcpth.company
                     tt-rm-bin.i-no    = rm-rcpth.i-no
                     tt-rm-bin.loc     = rm-rdtlh.loc
                     tt-rm-bin.loc-bin = rm-rdtlh.loc-bin
                     tt-rm-bin.tag     = rm-rdtlh.tag.
                  END.
                END.
           END.
         END.

         RELEASE ap-inv.

         create gltrans.
         assign
            t1 = t1 - ap-payl.amt-paid + ap-payl.amt-disc
            gltrans.company = cocode
            gltrans.actnum  = ap-payl.actnum
            gltrans.jrnl    = "APMEM"
            gltrans.tr-dscr = vend.name  + "  " + string(ap-pay.check-date)
            gltrans.tr-date = tran-date
            gltrans.tr-amt  = - (ap-payl.amt-paid - ap-payl.amt-disc)
            gltrans.period  = tran-period
            gltrans.trnum   = xtrnum
            ap-payl.posted  = true.

         find first bank where bank.company = cocode and
                               bank.actnum = ap-payl.actnum no-error.
         if avail bank then
           assign bank.bal = bank.bal + gltrans.tr-amt.

         RELEASE gltrans.

         if last-of(ap-payl.inv-no) then do:
            assign
               vend.purch[tran-period]   = vend.purch[tran-period]   + t1
               vend.purch[13]        = vend.purch[13]   + t1
               vend.acc-bal          = vend.acc-bal     + t1.
            if vend.acc-bal >= vend.hibal or vend.hibal = 0 THEN
               ASSIGN
               vend.hibal = vend.acc-bal
               vend.hibal-date = ap-pay.check-date.

            create ap-ledger.
            assign
               ap-ledger.company  = cocode
               ap-ledger.vend-no  = ap-pay.vend-no
               ap-ledger.amt      = ap-payl.amt-paid - ap-payl.amt-disc
               ap-ledger.refnum   = "MEMO#" + ap-payl.inv-no
               ap-ledger.ref-date = ap-pay.check-date
               ap-ledger.trnum    = xtrnum
               ap-ledger.tr-date  = tran-date
               t1 = 0.

            RELEASE ap-ledger.
         end.
         assign ap-pay.posted = true.
      end.
      create gltrans.
      assign
      gltrans.company = cocode
      gltrans.actnum  = xap-acct
      gltrans.jrnl    = "APMEM"
      gltrans.tr-dscr = "ACCOUNTS PAYABLE MEMO"
      gltrans.tr-date = tran-date
      gltrans.tr-amt  = g1 - g2   /* DAR  -  g2 - g1  */
      gltrans.period  = tran-period
      gltrans.trnum   = xtrnum.
      RELEASE gltrans.

      /* Recalc Rm Bins whose cost has been changed by APCRMEMO sys-ctrl param */
      FOR EACH tt-rm-bin,
          FIRST rm-bin
          WHERE rm-bin.company EQ tt-rm-bin.company
            AND rm-bin.i-no    EQ tt-rm-bin.i-no
            AND rm-bin.loc     EQ tt-rm-bin.loc
            AND rm-bin.loc-bin EQ tt-rm-bin.loc-bin
            AND rm-bin.tag     EQ tt-rm-bin.tag
          BREAK BY tt-rm-bin.company
                BY tt-rm-bin.i-no:

        ASSIGN
         rm-bin.qty  = 0
         rm-bin.cost = 0.

        IF TRIM(rm-bin.tag) NE "" THEN
        FOR EACH rm-rdtlh NO-LOCK
            WHERE rm-rdtlh.company EQ fg-bin.company
              AND rm-rdtlh.tag     EQ fg-bin.tag
              AND rm-rdtlh.loc     EQ fg-bin.loc
              AND rm-rdtlh.loc-bin EQ fg-bin.loc-bin
            USE-INDEX tag,
            EACH rm-rcpth NO-LOCK
            WHERE rm-rcpth.r-no      EQ rm-rdtlh.r-no
              AND rm-rcpth.rita-code EQ rm-rdtlh.rita-code
              AND rm-rcpth.i-no      EQ fg-bin.i-no
            USE-INDEX r-no

            BY rm-rcpth.trans-date
            BY rm-rcpth.rec_key
            BY rm-rdtlh.rec_key
            BY rm-rcpth.r-no:

          {rm/rm-mkbin.i}
        END. /* each rm-rcpth */

        ELSE
        FOR EACH rm-rcpth
            WHERE rm-rcpth.company EQ fg-bin.company
              AND rm-rcpth.i-no    EQ fg-bin.i-no
            NO-LOCK USE-INDEX i-no,
            EACH rm-rdtlh
            WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
              AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
              AND rm-rdtlh.loc       EQ fg-bin.loc
              AND rm-rdtlh.loc-bin   EQ fg-bin.loc-bin
              AND rm-rdtlh.tag       EQ fg-bin.tag
            USE-INDEX rm-rdtl no-lock

            BY rm-rcpth.trans-date
            BY rm-rcpth.rec_key
            BY rm-rdtlh.rec_key
            BY rm-rcpth.r-no:

          {rm/rm-mkbin.i}
        END. /* each rm-rcpth */

        IF LAST-OF(tt-rm-bin.i-no) THEN DO:
          FIND FIRST item NO-LOCK
              WHERE item.company EQ tt-rm-bin.company
                AND item.i-no    EQ tt-rm-bin.i-no
              NO-ERROR.

          IF AVAIL item THEN DO:
            ASSIGN
             ld-qty = 0
             ld-cst = 0.

            FOR EACH b-rm-bin FIELDS(qty cost) NO-LOCK
                WHERE b-rm-bin.company EQ item.company
                  AND b-rm-bin.i-no    EQ item.i-no
                  AND b-rm-bin.cost    NE ?:
              ASSIGN
               ld-cst = ld-cst + (b-rm-bin.cost *
                        (b-rm-bin.qty * IF b-rm-bin.qty LT 0 THEN -1 ELSE 1))
               ld-qty = ld-qty +
                        (b-rm-bin.qty * IF b-rm-bin.qty LT 0 THEN -1 ELSE 1).
            END.

            FIND CURRENT item.
            item.avg-cost = IF ld-qty EQ 0 THEN 0 ELSE (ld-cst / ld-qty).
            FIND CURRENT item NO-LOCK.
          END.
        END.
      END.
   end. /* postit: transaction */

   RELEASE vend.
   RELEASE ap-pay.
   RELEASE ap-payl.

   SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report PRIVATE :
{sys/form/r-top3w.f}

DEF VAR v1 as dec format "->>>,>>>,>>9.99".
DEF VAR v2 as dec format "->>>,>>>,>>9.99".
DEF VAR lcnt as int init 0. /* DAR */

form header
SKIP(1)
"VENDOR#  Name                          MEMO#  INVOICE#     INV.DATE      "
"     DEBIT           CREDIT     G/L DISTRIBUTION" skip fill("_",130) format "x(130)"
WITH frame r-top.

form
    ap-pay.check-no  at 35 FORMAT ">>>>>>>>>>" space(2)
    ap-payl.inv-no   space(1)
    ap-payl.due-date FORMAT "99/99/99" space(3)
    ap-payl.amt-disc format "->>,>>>,>>9.99" space(3)
    ap-payl.amt-paid space(5)
    ap-payl.actnum with frame dbcr-memo no-box no-labels STREAM-IO width 132.


SESSION:SET-WAIT-STATE("general").

v-postable = NO.

ASSIGN 
 str-tit2 = c-win:title + " " + string(xtrnum)
 {sys/inc/ctrtext.i str-tit2 112} 
 str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date)
 {sys/inc/ctrtext.i str-tit3 132}

 g1 = 0
 g2 = 0.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

DISPLAY "" WITH frame r-top.

   FOR EACH ap-pay
       WHERE ap-pay.company EQ cocode
         AND ap-pay.memo    EQ YES
         AND ap-pay.posted  EQ NO
         AND ap-pay.check-date GE begin_date
         AND ap-pay.check-date LE end_date,
       FIRST vend
       WHERE vend.company EQ ap-pay.company
         AND vend.vend-no EQ ap-pay.vend-no
       NO-LOCK,
       EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no NO-LOCK

       BREAK BY ap-pay.vend-no
             BY ap-pay.check-no
             BY ap-payl.inv-no
             BY ap-payl.line
       WITH FRAME dbcr-memo:

      if first-of(ap-pay.vend-no) then do:
         ap-pay.period = tran-period.
         put vend.vend-no space(1) vend.name. 
      end.
      else 
      if first-of(ap-payl.inv-no) then put skip(1).

      display
          ap-pay.check-no  when first-of(ap-pay.check-no)
          ap-payl.inv-no
          ap-payl.due-date
          ap-payl.amt-paid 
          ap-payl.amt-disc
          ap-payl.actnum.

      down.
      ASSIGN
      lcnt = lcnt + 1
      v1 = v1 + ap-payl.amt-paid
      v2 = v2 + ap-payl.amt-disc.

      if last-of(ap-pay.vend-no) then do:
         display  "*  VENDOR TOTALS" to 62 v2 to 84 v1 to 101 " *" skip(1)
         with frame vtot no-box no-labels width 132 STREAM-IO.
         ASSIGN
         g1 = g1 + v1
         g2 = g2 + v2
         v1 = 0
         v2 = 0.
      end.
      v-postable = YES.
   end. /* each memo */

   RELEASE ap-pay.

   display  "** GRAND TOTAL" to 62 g2 to 84 g1 to 101 " **" skip(1)
   with no-labels no-underline width 132 frame GT STREAM-IO.

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
  DEF VAR lv-frame-hdl as handle no-undo.
  DEF VAR lv-group-hdl as handle no-undo.
  DEF VAR lv-field-hdl as handle no-undo.
  DEF VAR lv-field2-hdl as handle no-undo.
  DEF VAR parm-fld-list as cha no-undo.
  DEF VAR parm-lbl-list as cha no-undo.
  DEF VAR i as int no-undo.
  DEF VAR lv-label as cha.

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
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," 
                     .
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     .
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
        IF xtrnum EQ gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
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

  ASSIGN targetfile = lv-audit-dir + "\AP\VW2\Run#"
                    + STRING(xtrnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AP"
         dirname3 = lv-audit-dir + "\AP\VW2".

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
