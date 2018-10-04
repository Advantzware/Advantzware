&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: glrep\r-distrb.w

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

DEF TEMP-TABLE tt-trans NO-UNDO
                        FIELD tt-recid AS RECID
                        FIELD TYPE AS cha
                        FIELD c-rate LIKE acctcost.c-rate
                        FIELD costacct LIKE acctcost.costacct
                        FIELD jrnl LIKE gltrans.jrnl
                        FIELD tr-amt LIKE gltrans.tr-amt
                        INDEX tt-trans IS PRIMARY TYPE tt-recid.

DEF VAR v-distribute AS LOG NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-trnum LIKE gl-ctrl.trnum NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date end_date ~
begin_accnt end_accnt tran-date rd_detsum rd-dest lines-per-page lv-ornt ~
lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_accnt end_accnt ~
tran-date tran-period rd_detsum rd-dest lines-per-page lv-ornt lv-font-no ~
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

DEFINE VARIABLE begin_accnt AS CHARACTER FORMAT "X(25)":U 
     LABEL "Beginning Acct#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Posting Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_accnt AS CHARACTER FORMAT "X(25)":U INITIAL "zzzzzzzzzzzzzzzzzzzzzzzzz" 
     LABEL "Ending Acct#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Posting Date" 
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
     SIZE 30 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd_detsum AS CHARACTER INITIAL "S" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Detail", "D",
"Summary", "S"
     SIZE 33 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.33.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_date AT ROW 2.19 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 2.19 COL 74 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_accnt AT ROW 4.1 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Account Number"
     end_accnt AT ROW 4.1 COL 64 COLON-ALIGNED HELP
          "Enter Ending Account Number"
     tran-date AT ROW 5.76 COL 38 COLON-ALIGNED
     tran-period AT ROW 6.95 COL 38 COLON-ALIGNED
     rd_detsum AT ROW 7.91 COL 40 NO-LABEL
     rd-dest AT ROW 11.24 COL 4 NO-LABEL
     lines-per-page AT ROW 11.48 COL 84 COLON-ALIGNED
     lv-ornt AT ROW 11.71 COL 30 NO-LABEL
     lv-font-no AT ROW 13.38 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 14.33 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.24 COL 31
     btn-ok AT ROW 18.86 COL 18
     btn-cancel AT ROW 18.86 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.29 COL 2
          FGCOLOR 9 
     "Report :" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 8.14 COL 30
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     RECT-6 AT ROW 9.81 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 21.1.


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
         TITLE              = "Generate Auto Distribution"
         HEIGHT             = 21.81
         WIDTH              = 96
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
       begin_accnt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_accnt:PRIVATE-DATA IN FRAME FRAME-A     = 
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





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Generate Auto Distribution */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Generate Auto Distribution */
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

   CASE FOCUS:NAME:
       WHEN "begin_accnt" OR WHEN "end_accnt" THEN DO:
            RUN windows/l-acct.w (g_company,"",FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME}, OUTPUT char-val).
            IF char-val <> "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
            RETURN NO-APPLY.

       END.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_accnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_accnt C-Win
ON LEAVE OF begin_accnt IN FRAME FRAME-A /* Beginning Acct# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Posting Date */
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
  IF v-invalid THEN RETURN NO-APPLY.

  ASSIGN {&DISPLAYED-OBJECTS}.

  RUN run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= " "
                            &begin_cust= "begin_accnt"
                            &END_cust= "begin_accnt" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = " "
                             &begin_cust= "begin_accnt"
                             &END_cust= "begin_accnt"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = " "
                                  &begin_cust="begin_accnt"
                                  &END_cust="begin_accnt"
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  end case.
  SESSION:SET-WAIT-STATE("").

  IF v-distribute  THEN do:
     MESSAGE "Are you sure you want to distribute to cost account?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.

     IF ll-ans THEN do:
        DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/

           REPEAT:
              FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ cocode EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
              IF AVAIL gl-ctrl THEN
              DO:
                 ASSIGN v-trnum       = gl-ctrl.trnum + 1
                        gl-ctrl.trnum = v-trnum.
                 LEAVE.
              END.
           END.
        END.

        FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.

        IF rd_detsum = "S" THEN RUN cost-distribute.
        ELSE RUN cost-distribute-det.
     END.
  END.
  v-distribute = NO.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_accnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_accnt C-Win
ON LEAVE OF end_accnt IN FRAME FRAME-A /* Ending Acct# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Posting Date */
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

  assign
   begin_date = date(month(today),1,year(today))
   end_date   = today
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

    FIND first period                   
        where period.company eq cocode
          and period.pst     ge begin_date
          and period.pend    le end_date
          AND NOT period.pstat
        no-lock no-error.
    if avail period THEN DO:
       MESSAGE "Period from " pst " to " pend " is Closed. " VIEW-AS ALERT-BOX ERROR.
       v-invalid = YES.
       RETURN.
    END.

    find first period                   
        where period.company eq cocode
          and period.pst     le tran-date
          and period.pend    ge tran-date
        no-lock no-error.
    if avail period THEN DO:
       IF NOT period.pstat /* closed */ THEN DO:
          MESSAGE "Period is Closed. " VIEW-AS ALERT-BOX ERROR.
          v-invalid = YES.
       END.
       ELSE tran-period:SCREEN-VALUE = string(period.pnum).
    END.
    ELSE DO:
      message "No Defined Period Exists for" tran-date view-as alert-box error.
      v-invalid = yes.
    end.



  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cost-distribute C-Win 
PROCEDURE cost-distribute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  SESSION:SET-WAIT-STATE("general").
  DEF BUFFER cost-trans FOR gltrans.
  DEF BUFFER cost-hist  FOR glhist.
  DEF VAR v-act-amt AS DEC NO-UNDO.

  FOR EACH tt-trans WHERE tt-trans.TYPE = "GLTRANS",
      EACH gltrans WHERE RECID(gltrans) = tt-trans.tt-recid
      /*EACH acctcost NO-LOCK WHERE acctcost.company = gltrans.company
                              AND acctcost.actnum = gltrans.actnum */
      BREAK BY /*gltrans.actnum*/ tt-trans.costacct:

      IF FIRST-OF(tt-trans.costacct) THEN v-act-amt = 0.

      v-act-amt = v-act-amt + tt-trans.tr-amt .
      IF LAST-OF(tt-trans.costacct) THEN DO:
         CREATE cost-trans.
         BUFFER-COPY gltrans TO cost-trans.
         ASSIGN cost-trans.tr-amt = v-act-amt /*tt-trans.tr-amt*/
             cost-trans.actnum = tt-trans.costacct
             cost-trans.tr-date = tran-date
             cost-trans.jrnl = "AUTODIST"
             cost-trans.period  = tran-period
             cost-trans.trnum   = v-trnum
             cost-trans.tr-dscr = "Auto Distribution"
             .
             /*gltrans.jrnl = "AUTODIST"*/ .
      END.

  END.


  FOR EACH tt-trans WHERE tt-trans.TYPE = "GLHIST",
      EACH glhist WHERE RECID(glhist) = tt-trans.tt-recid
      /*EACH acctcost NO-LOCK WHERE acctcost.company = glhist.company
                              AND acctcost.actnum = glhist.actnum */
           BREAK BY tt-trans.costacct:

      IF FIRST-OF(tt-trans.costacct) THEN v-act-amt = 0.

      v-act-amt = v-act-amt + tt-trans.tr-amt .

      IF LAST-OF( tt-trans.costacct) THEN DO:
         CREATE cost-hist.
         BUFFER-COPY glhist TO cost-hist.
         ASSIGN cost-hist.tr-amt = v-act-amt /*tt-trans.tr-amt */
                cost-hist.actnum = tt-trans.costacct
                cost-hist.tr-date = tran-date
                cost-hist.jrnl = "AUTODIST"
                cost-hist.period  = tran-period
                cost-hist.tr-num   = v-trnum
                cost-hist.tr-dscr = "Auto Distribution"
                .
      END.
      /*
      IF LAST-OF(glhist.actnum) THEN DO:
         CREATE cost-hist.
         BUFFER-COPY glhist TO cost-hist.
         ASSIGN cost-hist.tr-amt = glhist.tr-amt * (-1) 
                cost-hist.jrnl = "AUTODIST".
      END.
      */
      /*glhist.jrnl = "AUTODIST" */.
  END.
SESSION:SET-WAIT-STATE("").
MESSAGE "Auto Distribution is completed." VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cost-distribute-det C-Win 
PROCEDURE cost-distribute-det :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 SESSION:SET-WAIT-STATE("general").
  DEF BUFFER cost-trans FOR gltrans.
  DEF BUFFER cost-hist  FOR glhist.
  DEF VAR v-act-amt AS DEC NO-UNDO.

  FOR EACH tt-trans WHERE tt-trans.TYPE = "GLTRANS",
      EACH gltrans WHERE RECID(gltrans) = tt-trans.tt-recid
      /*EACH acctcost NO-LOCK WHERE acctcost.company = gltrans.company
                              AND acctcost.actnum = gltrans.actnum */
      BREAK BY gltrans.actnum:

      CREATE cost-trans.
      BUFFER-COPY gltrans TO cost-trans.
      ASSIGN cost-trans.tr-amt = tt-trans.tr-amt
             cost-trans.actnum = tt-trans.costacct
             cost-trans.tr-date = tran-date
             cost-trans.jrnl = "AUTODIST"
             cost-trans.period  = tran-period
             cost-trans.trnum   = v-trnum
             cost-trans.tr-dscr = "Auto Distribution"
             .
             /*gltrans.jrnl = "AUTODIST"*/ .

  END.


  FOR EACH tt-trans WHERE tt-trans.TYPE = "GLHIST",
      EACH glhist WHERE RECID(glhist) = tt-trans.tt-recid
      /*EACH acctcost NO-LOCK WHERE acctcost.company = glhist.company
                              AND acctcost.actnum = glhist.actnum */
           BREAK BY glhist.actnum:

       CREATE cost-hist.
       BUFFER-COPY glhist TO cost-hist.
       ASSIGN cost-hist.tr-amt = tt-trans.tr-amt 
                cost-hist.actnum = tt-trans.costacct
                cost-hist.tr-date = tran-date
                cost-hist.jrnl = "AUTODIST"
                cost-hist.period  = tran-period
                cost-hist.tr-num   = v-trnum
                cost-hist.tr-dscr = "Auto Distribution"
                .
      /*
      IF LAST-OF(glhist.actnum) THEN DO:
         CREATE cost-hist.
         BUFFER-COPY glhist TO cost-hist.
         ASSIGN cost-hist.tr-amt = glhist.tr-amt * (-1) 
                cost-hist.jrnl = "AUTODIST".
      END.
      */
      /*glhist.jrnl = "AUTODIST" */.
  END.
SESSION:SET-WAIT-STATE("").
MESSAGE "Auto Distribution is completed." VIEW-AS ALERT-BOX.

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
  DISPLAY begin_date end_date begin_accnt end_accnt tran-date tran-period 
          rd_detsum rd-dest lines-per-page lv-ornt lv-font-no lv-font-name 
          td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_date end_date begin_accnt end_accnt tran-date 
         rd_detsum rd-dest lines-per-page lv-ornt lv-font-no td-show-parm 
         btn-ok btn-cancel 
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
/***************************************************************************\
*****************************************************************************
**  Program: ap/rep/pjgl.p
**      
** Descript: CASH DISBURSEMENT / VOUCHER REGISTER BY GL ACCT
**
*****************************************************************************
\***************************************************************************/

{sys/form/r-topw.f}

DEF VAR lo_trandate AS DATE FORMAT "99/99/9999" NO-UNDO LABEL "From Date".
DEF VAR hi_trandate AS DATE FORMAT "99/99/9999" NO-UNDO LABEL "Thru Date".
DEF VAR DEBUG AS LOG NO-UNDO INITIAL TRUE.
DEF VAR ws_disc LIKE ap-payl.amt-disc COLUMN-LABEL "Discount" NO-UNDO.
DEF VAR ws_check-no LIKE ap-chk.check-no NO-UNDO FORMAT ">>>>>>>"
    COLUMN-LABEL "Check#".
DEF VAR ws_order-no LIKE oe-ord.ord-no NO-UNDO
    FORMAT ">>>>>>".
DEF VAR ws_jrnl LIKE gltrans.jrnl COLUMN-LABEL "Journal" NO-UNDO.
DEF VAR gl_jrnl_list AS CHAR NO-UNDO.
DEF VAR lo_actnum LIKE account.actnum LABEL "From GL Acct#" NO-UNDO.
DEF VAR hi_actnum LIKE account.actnum LABEL "Thru GL Acct#" NO-UNDO.
DEF VAR t-amt AS DEC NO-UNDO.
DEF VAR t-disc AS DEC NO-UNDO.
DEF VAR t-qty AS DEC NO-UNDO.
DEF VAR t-msf AS DEC NO-UNDO.
DEF VAR hdg_printed AS LOG NO-UNDO.
DEF VAR v-tot-amt AS DEC NO-UNDO.
DEF VAR v-tramt AS DEC NO-UNDO.

DEF BUFFER b-tt-trans FOR tt-trans.

FORM    ws_jrnl FORM "x(15)"
        ap-inv.vend-no    COLUMN-LABEL "Vendor"
        vend.name
        ap-inv.inv-date COLUMN-LABEL "Date"
        ap-inv.inv-no COLUMN-LABEL "Invoice#"
        ws_check-no
        ws_order-no
        ap-invl.qty
        /*ap-invl.amt-msf*/
        ws_disc
        ap-invl.amt
        WITH FRAME f-det width 144 DOWN STREAM-IO.


SESSION:SET-WAIT-STATE ("general").

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

 lo_actnum    = begin_accnt
 hi_actnum    = end_accnt
 lo_trandate  = begin_date
 hi_trandate  = end_date
 gl_jrnl_list = /*(if tb_cashr    then "!CASHR,"    else "") +
                (if tb_general  then "!GENERAL,,"  else "") +
                (if tb_mcshrec  then "!MCSHREC,"  else "") +
                (if tb_apmem    then "!APMEM,"    else "") +
                (if tb_acpay    then "!ACPAY,"    else "") +
                (if tb_ap-purch then "!AP-PURCH," else "") +
                (if tb_apckr    then "!APCHR,"    else "") +
                (if tb_arinv    then "!ARINV,"    else "") +
                (if tb_cdisb    then "!CDISB,"    else "") +
                (if tb_crmem    then "!CRMEM,"    else "") +
                (if tb_apvoidck then "!APVOIDCK," else "") +
                (if tb_oeinv    then "!OEINV,"    else "")*/
   "CASHR,GENERAL,MCSHREC,APMEM,ACPAY,AP-PURCH,APCHR,ARINV,CDISB,CRMEM,DBMEM,OEINV,APCKR,APVOIDCK,CRDIS".


{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

DISPLAY "" WITH FRAME r-top.

EMPTY TEMP-TABLE tt-trans.

v-distribute = NO.

  FOR EACH account NO-LOCK
      WHERE account.company EQ cocode
        AND account.actnum  GE lo_actnum
        AND account.actnum  LE hi_actnum:

    FOR EACH gltrans NO-LOCK
        WHERE gltrans.company EQ cocode
          AND gltrans.actnum  EQ account.actnum
          AND gltrans.tr-date GE lo_trandate
          AND gltrans.tr-date LE hi_trandate
          AND CAN-DO(gl_jrnl_list,gltrans.jrnl),

        EACH acctcost NO-LOCK
        WHERE acctcost.company EQ account.company
          AND acctcost.actnum  EQ account.actnum

        BREAK BY gltrans.actnum
             /* BY acctcost.costacct*/:

      IF FIRST-OF(gltrans.actnum) THEN v-tot-amt = 0.

      /* auto distribution to sub cost account */
      CREATE tt-trans.
      ASSIGN
       tt-trans.tt-recid = RECID(gltrans)
       tt-trans.type     = "GLTRANS"
       tt-trans.c-rate   = acctcost.c-rate
       tt-trans.tr-amt   = ROUND(gltrans.tr-amt * acctcost.c-rate / 100,2)
       tt-trans.costacct = acctcost.costacct
       tt-trans.jrnl     = gltrans.jrnl.

      v-tot-amt = v-tot-amt + tt-trans.tr-amt.

      IF LAST-OF(gltrans.actnum) THEN DO:
      /*  IF v-tot-amt NE gltrans.tr-amt THEN
          tt-trans.tr-amt = tt-trans.tr-amt + (gltrans.tr-amt - v-tot-amt).
      */
        CREATE tt-trans.
        ASSIGN
         tt-trans.tt-recid = RECID(gltrans)
         tt-trans.type     = "GLTRANS"
         tt-trans.c-rate   = 100
         tt-trans.tr-amt   = v-tot-amt * -1 /*gltrans.tr-amt * -1*/
         tt-trans.costacct = gltrans.actnum
         tt-trans.jrnl     = "Cost Distribution".           
      END.               
    END. /* gltrans */

    FOR EACH glhist NO-LOCK
        WHERE glhist.company EQ cocode
          AND glhist.actnum  EQ account.actnum
          AND glhist.tr-date GE lo_trandate
          AND glhist.tr-date LE hi_trandate
          AND CAN-DO(gl_jrnl_list,glhist.jrnl),

        EACH acctcost NO-LOCK
        WHERE acctcost.company EQ account.company
          AND acctcost.actnum  EQ account.actnum

        BREAK BY glhist.actnum
              BY acctcost.costacct:

      IF FIRST-OF(glhist.actnum) THEN v-tot-amt = 0.

      /* auto distribution to sub cost account */
      CREATE tt-trans.
      ASSIGN
       tt-trans.tt-recid = RECID(glhist)
       tt-trans.type     = "GLHIST"
       tt-trans.c-rate   = acctcost.c-rate
       tt-trans.tr-amt   = ROUND(glhist.tr-amt * acctcost.c-rate / 100,2)
       tt-trans.costacct = acctcost.costacct
       tt-trans.jrnl     = glhist.jrnl.

      v-tot-amt = v-tot-amt + tt-trans.tr-amt.

      IF LAST-OF(glhist.actnum) THEN DO:
       /* IF v-tot-amt NE glhist.tr-amt THEN
          tt-trans.tr-amt = tt-trans.tr-amt + (glhist.tr-amt - v-tot-amt).
       */
        CREATE tt-trans.
        ASSIGN
         tt-trans.tt-recid = RECID(glhist)
         tt-trans.type     = "GLHIST"
         tt-trans.c-rate   = 100
         tt-trans.tr-amt   = v-tot-amt * -1 /*glhist.tr-amt * -1*/
         tt-trans.costacct = glhist.actnum
         tt-trans.jrnl     = "Cost Distribution".           
      END.               
    END. /* glhist */
  END. /* account*/

/*======*/
  VIEW FRAME F-DET.
  DOWN 0 WITH FRAME F-DET.
  ASSIGN
    hdg_printed = FALSE
    t-amt = 0
    t-disc = 0
    t-msf = 0
    t-qty = 0
    ws_disc = 0
    ws_jrnl = ''
    ws_check-no = 0
    ws_order-no = 0
    .


  FOR EACH tt-trans,
      FIRST account WHERE account.company = cocode
                      AND account.actnum = tt-trans.costacct NO-LOCK
                      BREAK BY tt-trans.costacct:

    if line-counter >= (page-size - 2) then do:
        page.
        view frame f-det.
        down 0 with frame f-det.
    end.

    IF first-of(tt-trans.costacct) THEN DO:


         v-tot-amt = 0.  
    END.

    v-distribute = YES.

    IF NOT hdg_printed THEN
    DO:
      /*  PUT SKIP account.actnum ' - '
          account.dscr
          SKIP. */
        hdg_printed = TRUE.
    END.

    IF rd_detsum = "D" THEN do: /* detail*/
       DISPLAY account.actnum @ ws_jrnl 
                 account.dscr @ vend.NAME        
                 /*"Cost Distribute" @ vend.NAME   */            
                 tt-trans.tr-amt @ ap-invl.amt 
                 WITH FRAME f-det.
       DOWN WITH FRAME f-det.
    END.
      v-tramt = tt-trans.tr-amt.
      v-tot-amt = v-tot-amt + v-tramt.

      IF LAST-OF(tt-trans.costacct) THEN DO:
         IF rd_detsum = "D" THEN do: /* detail*/
            UNDERLINE ws_disc ap-invl.amt ap-invl.qty WITH FRAME f-det.
            DOWN WITH FRAME f-det.
         END.
         DISPLAY account.actnum @ ws_jrnl
                 "Account Total" WHEN rd_detsum = "D" @ ws_jrnl
                 account.dscr @ vend.NAME        
                 /*"Cost Distribute" @ vend.NAME   */            
                 v-tot-amt @ ap-invl.amt 
                 WITH FRAME f-det.
         DOWN WITH FRAME f-det.

         IF NOT last(tt-trans.costacct) AND rd_detsum = "D" THEN do: /* detail*/
            UNDERLINE ws_disc ap-invl.amt ap-invl.qty WITH FRAME f-det.
            DOWN WITH FRAME f-det.
         END.
        ASSIGN t-amt = t-amt + v-tot-amt . /*gltrans.tr-amt * (-1).*/
      END.

  END.

    UNDERLINE ws_disc ap-invl.amt ap-invl.qty WITH FRAME f-det.
    DOWN WITH FRAME f-det.
    DISP
      /* "* ACCOUNT TOTAL *" @ vend.name */
      t-disc @ ws_disc
      t-amt  @ ap-invl.amt
      t-qty  @ ap-invl.qty WITH FRAME f-det.
      /*t-msf  @ ap-invl.amt-msf.*/
     /* down 1. */

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

