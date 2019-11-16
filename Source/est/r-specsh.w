&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File              : r-specsh.w

  Description       : 

  Author            : JLF

  Created           : 04/23/02

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
DEFINE INPUT PARAMETER ipr-eb AS ROWID.

DEFINE STREAM st-email.
DEFINE STREAM ediBOL.

/* Variables */
DEFINE NEW SHARED VARIABLE v-term-id AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHARACTER NO-UNDO.

DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-EDIBOLPost-log AS LOG NO-UNDO.
DEFINE VARIABLE v-EDIBOLPost-char AS CHARACTER FORMAT "X(200)" NO-UNDO.
DEFINE VARIABLE v-lines-per-page AS INTEGER NO-UNDO INIT 60.
DEFINE VARIABLE v-print-fmt AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-headers AS LOG  NO-UNDO.
DEFINE VARIABLE v-print-coc AS LOG  NO-UNDO.
DEFINE VARIABLE v-check-qty AS LOG  NO-UNDO.
DEFINE VARIABLE v-program AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOG  NO-UNDO.
DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-pdf-file AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcMailMode AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcDefaultForm AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-def-coc-fmt AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-strips1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-strips2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-next-num AS INTEGER NO-UNDO.
DEFINE VARIABLE choice AS LOG NO-UNDO.
DEFINE VARIABLE retcode AS INTEGER NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.
DEFINE VARIABLE lv-prt-bypass AS LOG NO-UNDO.

/* Build a Table to keep sequence of pdf files */
DEFINE NEW SHARED TEMP-TABLE tt-filelist NO-UNDO
    FIELD tt-FileCtr    AS INTEGER
    FIELD tt-FileName   AS CHARACTER
    INDEX filelist      IS PRIMARY TT-FILECTR.

DEFINE TEMP-TABLE tt-email NO-UNDO
    FIELD tt-recid AS RECID
    FIELD bol-no LIKE oe-boll.bol-no
    FIELD ord-no LIKE oe-boll.ord-no
    FIELD i-no LIKE itemfg.i-no
    FIELD qty AS INTEGER
    FIELD cust-no AS cha
    INDEX tt-cust IS PRIMARY cust-no DESCENDING .

DEFINE TEMP-TABLE ediOutFile NO-UNDO
    FIELD custNo AS CHARACTER
    FIELD poNo AS CHARACTER
    FIELD poLine AS INTEGER
    FIELD partNo AS CHARACTER
    FIELD qty AS DECIMAL
    FIELD lotNo AS CHARACTER
    FIELD bolDate AS DATE
    FIELD relNo AS INTEGER
    FIELD carrier AS CHARACTER
    FIELD trailer AS CHARACTER
    FIELD bolNo AS INTEGER
    INDEX ediOutFile IS PRIMARY custNo bolNo carrier trailer.

/* Includes */
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}
{sys/inc/var.i new shared}
{custom/xprint.i}

v-term-id = TERMINAL.
cocode = gcompany.

FIND FIRST company NO-LOCK WHERE 
    company.company = cocode 
    NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
                        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                        OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-20 begin_cust end_cust ~
begin_est# end_est# fiSampleNum tgSampleSent fiDateRec fiNoCells fiDateDue ~
fiNoSamples rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
btn-cancel btn-ok 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_est# end_est# ~
fiSampleNum tgSampleSent fiDateRec fiNoCells fiDateDue fiNoSamples rd-dest ~
lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
  ( ipField AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

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

DEFINE VARIABLE begin_est# AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Beginning Est #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_est# AS CHARACTER FORMAT "X(256)":U INITIAL "99999999" 
     LABEL "Ending Est #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiDateDue AS DATE FORMAT "99/99/99":U 
     LABEL "Date Due" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiDateRec AS DATE FORMAT "99/99/99":U 
     LABEL "Date Receive" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiNoCells AS INTEGER FORMAT "->,>>>,>>>":U INITIAL 0 
     LABEL "No. Cells" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiNoSamples AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "# of Samples" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiSampleNum AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sample #" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 47.6 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

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
"To Email", 4,
"To Direct Port", 5
     SIZE 23 BY 5 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 8.33.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 7.14.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tgSampleSent AS LOGICAL INITIAL NO 
     LABEL "Sample Sent" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust AT ROW 2.43 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 2.43 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_est# AT ROW 4.1 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_est# AT ROW 4.1 COL 69 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     fiSampleNum AT ROW 5.52 COL 26 COLON-ALIGNED WIDGET-ID 2
     tgSampleSent AT ROW 5.52 COL 53 WIDGET-ID 12
     fiDateRec AT ROW 6.95 COL 26 COLON-ALIGNED WIDGET-ID 4
     fiNoCells AT ROW 6.95 COL 69 COLON-ALIGNED WIDGET-ID 16
     fiDateDue AT ROW 8.38 COL 26 COLON-ALIGNED WIDGET-ID 6
     fiNoSamples AT ROW 8.38 COL 69 COLON-ALIGNED WIDGET-ID 10
     rd-dest AT ROW 11.95 COL 6 NO-LABELS
     lv-ornt AT ROW 12.19 COL 35 NO-LABELS
     lines-per-page AT ROW 12.19 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 13.71 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 13.71 COL 40.4 COLON-ALIGNED NO-LABELS
     td-show-parm AT ROW 15.24 COL 35
     btn-cancel AT ROW 18.14 COL 64
     btn-ok AT ROW 18.19 COL 23
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.76 COL 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.1 COL 2
          BGCOLOR 2 
     RECT-6 AT ROW 10.52 COL 3
     RECT-20 AT ROW 1.95 COL 3 WIDGET-ID 14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 18.95.


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
         TITLE              = "Print Spec Sheet"
         HEIGHT             = 19.43
         WIDTH              = 95.8
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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
       begin_est#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_est#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lv-font-name:READ-ONLY IN FRAME FRAME-A        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Print Spec Sheet */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Print Spec Sheet */
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
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_est#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est# C-Win
ON LEAVE OF begin_est# IN FRAME FRAME-A /* Beginning Est # */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
    DEFINE VARIABLE retCode     AS INTEGER NO-UNDO.  
    DEFINE VARIABLE ll          AS LOG NO-UNDO.
    DEFINE VARIABLE v-format-str AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-exception AS LOG NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            {&displayed-objects}.
    END.
  
    ASSIGN 
        begin_est#  = FILL(" ",8 - LENGTH(TRIM(begin_est#))) + TRIM(begin_est#)
        end_est#    = FILL(" ",8 - LENGTH(TRIM(end_est#))) + TRIM(end_est#)
        v-format-str = "SPECSHT".

    CASE rd-dest:
        WHEN 1 THEN ASSIGN LvOutputSelection = "Printer".
        WHEN 2 THEN ASSIGN LvOutputSelection = "Screen". 
        WHEN 3 THEN ASSIGN LvOutputSelection = "File". 
        WHEN 4 THEN ASSIGN LvOutputSelection = "Fax". 
        WHEN 5 THEN ASSIGN LvOutputSelection = "Email".
        WHEN 6 THEN ASSIGN LvOutputSelection = "Port".
    END CASE.

    IF NOT rd-dest = 5 THEN DO:
        RUN run-report.
   END.
   ELSE /*rd-dest eq 5*/ DO:
   END.

   SESSION:SET-WAIT-STATE ("").

   EMPTY TEMP-TABLE tt-email.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_est#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est# C-Win
ON LEAVE OF end_est# IN FRAME FRAME-A /* Ending Est # */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

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

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
PROCEDURE mail EXTERNAL "xpMail.dll" :
      DEFINE INPUT PARAMETER mailTo AS CHARACTER.
      DEFINE INPUT PARAMETER mailsubject AS CHARACTER.
      DEFINE INPUT PARAMETER mailText AS CHARACTER.
      DEFINE INPUT PARAMETER mailFiles AS CHARACTER.
      DEFINE INPUT PARAMETER mailDialog AS LONG.
      DEFINE OUTPUT PARAMETER retCode AS LONG.
END.

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

    FIND FIRST company NO-LOCK WHERE 
        company.company EQ cocode 
        NO-ERROR.

    FIND FIRST oe-ctrl NO-LOCK WHERE 
        oe-ctrl.company EQ cocode 
        NO-ERROR.

    FIND FIRST users NO-LOCK WHERE
        users.user_id EQ USERID("NOSWEAT")
        NO-ERROR.
    IF AVAILABLE users AND users.user_program[2] NE "" THEN
        init-dir = users.user_program[2].
    ELSE
        init-dir = "c:\tmp".

    RUN getCESAMPLE.

    RUN SetSpecForm(INPUT v-print-fmt).
    ASSIGN 
        vcDefaultForm = v-print-fmt.


  RUN enable_UI.

    DO WITH FRAME {&FRAME-NAME}:
        FIND eb WHERE 
            ROWID(eb) = ipr-eb 
            NO-LOCK.
        IF AVAILABLE eb THEN FIND FIRST est NO-LOCK WHERE 
            est.est-no EQ eb.est-no AND 
            est.company = cocode
            NO-ERROR.
        IF AVAILABLE eb THEN DO:
            ASSIGN
                begin_cust:SCREEN-VALUE = eb.cust-no
                end_cust:SCREEN-VALUE = eb.cust-no
                begin_est#:SCREEN-VALUE = eb.est-no
                end_est#:SCREEN-VALUE = eb.est-no.

            IF v-print-fmt = "Premier" THEN DO:
                ASSIGN 
                    fiSampleNum:HIDDEN = YES
                    tgSampleSent:HIDDEN = YES
                    fiDateRec:HIDDEN = YES
                    fiNoCells:HIDDEN = YES
                    fiDateDue:HIDDEN = YES
                    fiNoSamples:HIDDEN = YES
                    rd-dest:SCREEN-VALUE = "2"
                    rd-dest:SENSITIVE = NO
                    lv-ornt:HIDDEN = YES
                    lines-per-page:HIDDEN = YES
                    lv-font-no:HIDDEN = YES
                    lv-font-name:HIDDEN = YES
                    td-show-parm:HIDDEN = YES.
            END.
            ELSE DO:
                IF est.sampleNum EQ 0 THEN DO TRANSACTION:
                    FIND CURRENT est EXCLUSIVE.
                    FIND FIRST sys-ctrl EXCLUSIVE WHERE 
                        sys-ctrl.company EQ cocode AND 
                        sys-ctrl.name    EQ "CESample"
                        NO-ERROR.      

                    IF AVAIL sys-ctrl THEN ASSIGN 
                        v-next-num = sys-ctrl.int-fld + 1
                        est.sampleNum = v-next-num
                        sys-ctrl.int-fld = v-next-num.
                    FIND CURRENT est NO-LOCK.
                    FIND CURRENT sys-ctrl NO-LOCK.
                END. 
                fiSampleNum:SCREEN-VALUE = STRING(v-next-num).
            END.

            IF AVAILABLE eb AND AVAILABLE est THEN DO:
                RUN calc-values (INPUT ROWID(est), ROWID(eb)).
                fiNoCells:SCREEN-VALUE = STRING((INTEGER(v-strips1) + 1) * (INTEGER(v-strips2) + 1)).
            END.
        END.
        APPLY "entry" TO begin_cust.

        lines-per-page:SCREEN-VALUE = STRING(v-lines-per-page).
        DISABLE lines-per-page.

        APPLY 'value-changed':u TO rd-dest.
    END.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ASIMail C-Win 
PROCEDURE ASIMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER icMailMode  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER icCustNo    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER icSubBody   AS CHARACTER NO-UNDO.
/*
  {custom/asimail2.i  &TYPE           = value (icMailMode)
                      &group-title    = 'r-bolprt.'
                      &begin_cust     = icCustNo
                      &END_cust       = icCustNo
                      &mail-subject   = icSubBody
                      &mail-body      = icSubBody
                      &mail-file      = lv-pdf-file}
  */                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-work C-Win 
PROCEDURE build-work :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ic2ndKey  AS CHARACTER NO-UNDO.
  
    build-work:
    FOR EACH eb NO-LOCK WHERE 
        eb.company EQ cocode AND 
        eb.est-no  GE begin_est# AND 
        eb.est-no  LE END_est# AND 
        eb.cust-no GE begin_cust AND 
        eb.cust-no LE end_cust AND 
        ((eb.form-no EQ 0 AND v-print-fmt NE "Premier") 
            OR (eb.form-no NE 0 AND v-print-fmt EQ "Premier")):

        IF NOT CAN-FIND(FIRST report WHERE
            report.term-id = v-term-id AND
            report.rec-id  = RECID(eb)) THEN DO:
            CREATE report.
            ASSIGN 
                report.term-id  = v-term-id
                report.key-01   = eb.cust-no
                report.key-02   = eb.ship-id
                report.rec-id   = RECID(eb)
                report.key-03   = fiSampleNum
                report.key-04   = STRING(tgSampleSent)
                report.key-05   = STRING(fiDateRec)
                report.key-06   = STRING(fiDateDue)
                report.key-07   = STRING(fiNoSamples)
                report.key-08   = STRING(fiNoCells).
        END.

        STATUS DEFAULT 'Now Processing Estimate: ' + string (eb.est-no) + '....'.
    END.

    ASSIGN 
        v-lines-per-page = lines-per-page.

    STATUS DEFAULT ''.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-values C-Win 
PROCEDURE calc-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-est-rowid AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ip-eb-rowid AS ROWID NO-UNDO.

    DEFINE VARIABLE v-int AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll-crt-itemfg AS LOG INIT NO NO-UNDO.
    DEFINE VARIABLE k_frac AS DECIMAL INIT 6.25 NO-UNDO. 
    DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.
    DEFINE VARIABLE v-count AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-part LIKE eb.part-no NO-UNDO.
    DEFINE VARIABLE v-slot1 AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-caliper1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-caliper2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE scr-style-1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE scr-end-cell-l1 AS INTEGER NO-UNDO.
    DEFINE VARIABLE scr-end-cell-l2 AS INTEGER NO-UNDO.
    DEFINE VARIABLE scr-in-cell-length AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-slot2 AS INTEGER NO-UNDO.
    DEFINE VARIABLE scr-style-2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE scr-end-cell-w1 AS INTEGER NO-UNDO.
    DEFINE VARIABLE scr-end-cell-w2 AS INTEGER NO-UNDO.
    DEFINE VARIABLE scr-in-cell-width AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-eb-len AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-eb-wid AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-eb-dep AS INTEGER NO-UNDO.
    DEFINE BUFFER b-eb1 FOR eb.
    DEFINE BUFFER b-eb2 FOR eb.
    DEFINE BUFFER bf-est FOR est.
    DEFINE BUFFER bf-set FOR eb.

    {sys/inc/f16to32.i}
    {cec/msfcalc.i}
    {sys/inc/setprint.i}

    FIND est NO-LOCK WHERE 
        ROWID(est) = ip-est-rowid 
        NO-ERROR.
    FIND eb NO-LOCK WHERE 
        ROWID(eb) = ip-eb-rowid 
        NO-ERROR.
    IF NOT AVAILABLE eb THEN
        RETURN.

    ASSIGN 
        v-part = eb.part-no.
    FIND FIRST bf-set NO-LOCK WHERE
        bf-set.company = eb.company AND
        bf-set.est-no = eb.est-no AND
        bf-set.form-no = 0
        NO-ERROR.

    FIND FIRST b-eb1 NO-LOCK WHERE 
        b-eb1.company EQ eb.company AND
        b-eb1.est-no  EQ eb.est-no AND
        b-eb1.form-no NE 0 AND
        b-eb1.blank-no NE 0
        USE-INDEX est-qty
        NO-ERROR.

    IF AVAILABLE b-eb1 THEN FIND FIRST b-eb2 NO-LOCK WHERE 
        b-eb2.company EQ eb.company AND
        b-eb2.est-no  EQ eb.est-no AND
        b-eb2.form-no NE 0 AND
        b-eb2.blank-no NE 0 AND
        ROWID(b-eb2) NE ROWID(b-eb1)
        USE-INDEX est-qty
        NO-ERROR.

    IF AVAILABLE b-eb1 THEN DO:
        FIND FIRST style NO-LOCK WHERE
            style.company EQ b-eb1.company AND
            style.style = b-eb1.style
            NO-ERROR.
        IF AVAIL(style) THEN ASSIGN 
            v-slot1 = style.dim-df.
   
        ASSIGN 
            iCnt = 0.
        FOR EACH ef NO-LOCK WHERE 
            ef.company = b-eb1.company AND 
            ef.est-no  = b-eb1.est-no AND 
            ef.eqty    = b-eb1.eqty AND 
            ef.form-no = b-eb1.form-no:

            FIND FIRST ITEM NO-LOCK WHERE 
                ITEM.company = ef.company AND 
                ITEM.i-no = ef.board 
                NO-ERROR.
            IF AVAILABLE ITEM THEN DO:
                ASSIGN 
                    iCnt = iCnt + 1.
                IF icnt = 1 THEN ASSIGN 
                    v-caliper1 = STRING(ITEM.cal).
                ELSE ASSIGN 
                    v-caliper2 = STRING(ITEM.cal).
            END.
        END.
   
        ASSIGN
            v-int = style.dim-df + 1
            scr-style-1 = b-eb1.style
            scr-end-cell-l1 = {sys/inc/k16.i b-eb1.k-len-array2[1]}
            scr-end-cell-l2 = {sys/inc/k16.i b-eb1.k-len-array2[v-int]}
            scr-in-cell-length = {sys/inc/k16.i b-eb1.k-len-array2[2]}
            v-strips1 = b-eb1.quantityPerSet.
    END.

    IF AVAILABLE b-eb2 THEN DO:
        FIND FIRST style NO-LOCK WHERE
            style.company EQ b-eb2.company AND
            style.style = b-eb2.style
            NO-ERROR.
        IF AVAIL(style) THEN ASSIGN 
            v-slot2 = style.dim-df.
        ASSIGN
            v-int = style.dim-df + 1
            scr-style-2 = b-eb2.style
            scr-end-cell-w1 = {sys/inc/k16.i b-eb2.k-len-array2[1]}
            scr-end-cell-w2 = {sys/inc/k16.i b-eb2.k-len-array2[v-int]}
            scr-in-cell-width = {sys/inc/k16.i b-eb2.k-len-array2[2]}
            v-strips2 = b-eb2.quantityPerSet
            iCnt = 0.
   
        FOR EACH ef NO-LOCK WHERE 
            ef.company = b-eb2.company AND 
            ef.est-no  = b-eb2.est-no AND 
            ef.eqty    = b-eb2.eqty AND 
            ef.form-no = b-eb2.form-no:

            FIND FIRST ITEM NO-LOCK WHERE 
                ITEM.company = ef.company AND 
                ITEM.i-no = ef.board
                NO-ERROR.
            IF AVAILABLE ITEM THEN DO:
                ASSIGN 
                    iCnt = iCnt + 1.
                IF icnt = 1 THEN ASSIGN 
                    v-caliper1 = STRING(ITEM.cal).
                ELSE ASSIGN 
                    v-caliper2 = STRING(ITEM.cal).
            END.
        END.
    END.

    ASSIGN  
        v-eb-len = {sys/inc/k16.i eb.len}
        v-eb-wid = {sys/inc/k16.i eb.wid}
        v-eb-dep = {sys/inc/k16.i eb.dep}.

    RELEASE itemfg.

    IF avail(bf-set) 
    AND bf-set.stock-no NE "" THEN FIND FIRST itemfg NO-LOCK WHERE
        itemfg.company EQ bf-set.company AND
        itemfg.i-no    EQ bf-set.stock-no
        NO-ERROR.

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
  DISPLAY begin_cust end_cust begin_est# end_est# fiSampleNum tgSampleSent 
          fiDateRec fiNoCells fiDateDue fiNoSamples rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-20 begin_cust end_cust begin_est# end_est# fiSampleNum 
         tgSampleSent fiDateRec fiNoCells fiDateDue fiNoSamples rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm btn-cancel btn-ok 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateMail C-Win 
PROCEDURE GenerateMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ic1stKey AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ic2ndKey AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER iiMode   AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER icType   AS CHARACTER NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetCESAMPLE C-Win 
PROCEDURE GetCESAMPLE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lRecFound AS LOG NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

    RUN sys/ref/nk1look.p (cocode, "CESAMPLE", "C", NO, NO, "", "", 
                          OUTPUT v-print-fmt, OUTPUT lRecFound).
    RUN sys/ref/nk1look.p (cocode, "CESAMPLE", "I", NO, NO, "", "", 
                          OUTPUT cReturn, OUTPUT lRecFound).
    IF lRecFound THEN ASSIGN 
        v-next-num = INT(cReturn).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* --------------------------------------------- oe/rep/oe-lad.p 3/94 RM ---- */
/* print bill of ladings                                                      */
/* -------------------------------------------------------------------------- */
    DEFINE VARIABLE ret-code AS INTEGER.
    
    {sys/form/r-top.i}
    {sys/inc/print1.i} 
    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN 
        RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    {sa/sa-sls01.i} 

    ASSIGN 
        v-term-id = v-term.
    RUN build-work ('').

    IF IS-xprint-form THEN DO:
        CASE rd-dest:
            WHEN 1 THEN PUT "<PRINTER?></PROGRESS>".
            WHEN 2 THEN DO:
                IF NOT lBussFormModle THEN
                    PUT "<PREVIEW><MODAL=NO>". 
                ELSE
                    PUT "<PREVIEW>".      
            END.  
            WHEN 3 THEN DO:
                /*  {custom/out2file.i} */
            END.
            WHEN 7 THEN DO:
                ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
                PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW></PROGRESS>".
            END.   
            WHEN 4 THEN DO:
                ASSIGN
                    lv-pdf-file = init-dir + "\" + "Est" + TRIM(est.est-no).
                PUT "<PREVIEW><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf></PROGRESS>" FORM "x(180)".
            END. 
            WHEN 5 THEN RUN custom/d-print.w (list-name).
        END CASE.
    END.

    RUN value(v-program).

    FOR EACH report WHERE report.term-id EQ v-term-id:
        DELETE report.
    END.

    OUTPUT CLOSE.
  
    FILE-INFO:FILE-NAME = list-name.

    IF is-xprint-form THEN CASE rd-dest:
        WHEN 1 THEN RUN printfile (FILE-INFO:FILE-NAME).
        WHEN 2 THEN RUN printfile (FILE-INFO:FILE-NAME).
        WHEN 3 THEN DO:
            {custom/out2file.i}
        END. 
        WHEN 4 THEN DO:
            RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
            RUN custom/xpmail.p ("CUSTOMER",lv-pdf-file + ".pdf","",
                                'Sample Spec Sheet',
                                'Sample Spec Sheet',OUTPUT ret-code).
        END.   
        WHEN 5 THEN RUN custom/d-print.w (list-name).
        WHEN 7 THEN DO:
           ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".txt". 
           OS-COPY VALUE(list-name) VALUE(ls-fax-file).
           RUN custom/asifax.p ("",ls-fax-file,"",
                               'Estimate',
                                 'Estimate',OUTPUT ret-code).
        END.      
    END CASE.

    IF rd-dest NE 4 THEN
        RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-mail C-Win 
PROCEDURE run-report-mail :
/* --------------------------------------------------------*/
    DEFINE INPUT PARAMETER icCustNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ic2ndKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iiMode   AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iLprinted AS LOG NO-UNDO.

    {sys/form/r-top.i}
    {sys/inc/print1.i}
    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN 
        RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    {sa/sa-sls01.i}

    ASSIGN 
        v-term-id = v-term.

    RUN build-work (ic2ndKey).

    STATUS DEFAULT 'Processing... Please wait.'.

    IF CAN-FIND (FIRST report WHERE report.term-id EQ v-term-id) THEN DO:
        IF IS-xprint-form THEN DO:      
            PUT "<PDF=DIRECT><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
        END.
    END.
    ELSE DO:
        MESSAGE 
            'No records to process. Job aborted.'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN.
    END.

    FOR EACH report WHERE report.term-id EQ v-term-id:
        DELETE report.
    END.

    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-mail-uni-xl C-Win 
PROCEDURE send-mail-uni-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icIdxKey   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icRecType  AS CHARACTER NO-UNDO.    

    DEFINE VARIABLE vcSubject   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vcMailBody  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vcErrorMsg  AS CHARACTER  NO-UNDO.

    IF SEARCH (lv-pdf-file) EQ ? THEN DO:
        MESSAGE 
            'Attachment File: ' lv-pdf-file ' is missing.'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN.
    END.

    ASSIGN  
        vcSubject   = "CofC for BOL: " 
        vcMailBody  = "Please review attached CofC for BOL #: " .

    RUN custom/xpmail2.p (INPUT   icRecType,
                          INPUT   'R-BOLPRT.',
                          INPUT   lv-pdf-file,
                          INPUT   icIdxKey,
                          INPUT   vcSubject,
                          INPUT   vcMailBody,
                          OUTPUT  vcErrorMsg).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMail-1 C-Win 
PROCEDURE SendMail-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icIdxKey   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icRecType  AS CHARACTER NO-UNDO.    

    DEFINE VARIABLE vcSubject   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vcMailBody  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vcErrorMsg  AS CHARACTER  NO-UNDO.

    ASSIGN  
        vcSubject   = "BOL: " +  '   ' + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
        vcSubject   = vcSubject
        vcMailBody  = "Please review attached Bill of Lading(s) for BOL #: " .

    RUN custom/xpmail2.p (INPUT   icRecType,
                          INPUT   'R-BOLPRT.',
                          INPUT   lv-pdf-file,
                          INPUT   icIdxKey,
                          INPUT   vcSubject,
                          INPUT   vcMailBody,
                          OUTPUT  vcErrorMsg).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMail-2 C-Win 
PROCEDURE SendMail-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icIdxKey   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icRecType  AS CHARACTER NO-UNDO.    

    DEFINE VARIABLE vcSubject   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vcMailBody  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vcErrorMsg  AS CHARACTER  NO-UNDO.

    IF SEARCH (list-name) NE ? THEN DO:
        IF NOT list-name MATCHES '*.txt' THEN DO:
            OS-RENAME VALUE (SEARCH (list-name)) VALUE (SEARCH (list-name) + '.txt').
            IF OS-ERROR NE 0 THEN DO:
                MESSAGE 
                    'Failed to rename your temp file.'  SKIP
                    'OS Error: ' OS-ERROR
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            END.
            ELSE ASSIGN 
                list-name = list-name + '.txt'.
        END.
    END.
    ELSE DO:
        MESSAGE 
            'Attachment File: ' list-name ' is missing.'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN.
    END.

    ASSIGN  
        vcSubject   = "BOL: " +  '   ' + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
        vcSubject   = vcSubject
        vcMailBody  = "Please review attached Bill of Lading(s) for BOL #: " .

    RUN custom/xpmail2.p (INPUT   icRecType,
                          INPUT   'R-BOLPRT.',
                          INPUT   list-name,
                          INPUT   icIdxKey,
                          INPUT   vcSubject,
                          INPUT   vcMailBody,
                          OUTPUT  vcErrorMsg).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetSpecForm C-Win 
PROCEDURE SetSpecForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icFormName AS CHARACTER NO-UNDO.

    CASE icFormName:
        WHEN "Xprint" THEN ASSIGN 
            is-xprint-form = YES
            v-program      = "oe/rep/cocxprnt.p".
        WHEN "Partitions" THEN ASSIGN
            is-xprint-form = YES
            v-program = "est/specmultm.p".
        WHEN "Premier" THEN ASSIGN
            is-xprint-form = NO
            v-program = "est/specprem.p".
        WHEN "" OR WHEN "Brick" THEN ASSIGN 
            is-xprint-form = NO
            v-program      = "oe/rep/cocbrick.p".
        OTHERWISE ASSIGN
            is-xprint-form = NO
            v-program = "oe/rep/cocuni.p".
    END CASE.
    
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
    DEFINE VARIABLE lv-frame-hdl AS HANDLE NO-UNDO.
    DEFINE VARIABLE lv-group-hdl AS HANDLE NO-UNDO.
    DEFINE VARIABLE lv-field-hdl AS HANDLE NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-label AS cha NO-UNDO.

    ASSIGN
        lv-frame-hdl = FRAME {&frame-name}:HANDLE
        lv-group-hdl = lv-frame-hdl:FIRST-CHILD
        lv-field-hdl = lv-group-hdl:FIRST-CHILD.

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0 THEN DO:
            IF lv-field-hdl:LABEL <> ? THEN ASSIGN 
                parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + ",".
           ELSE DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN ASSIGN 
                        parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                    ASSIGN 
                        lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
                END.       
            END.                 
        END.
        ASSIGN 
            lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
    END.

    PUT SPACE(28)
        "< Selection Parameters >"
        SKIP(1).

    DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
        IF ENTRY(i,parm-fld-list) NE "" 
        OR ENTRY(i,parm-lbl-list) NE "" THEN DO:
            ASSIGN 
                lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) + trim(ENTRY(i,parm-lbl-list)) + ":".

            PUT lv-label FORMAT "x(35)" AT 5
                SPACE(1)
                TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
                SKIP.              
        END.
    END.

    PUT FILL("-",80) FORMAT "x(80)" SKIP.
    PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
  ( ipField AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE invalidChars AS CHARACTER NO-UNDO INITIAL "~",#".
    DEFINE VARIABLE replaceChars AS CHARACTER NO-UNDO INITIAL "'',".
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DEFINE VARIABLE k AS INTEGER NO-UNDO.

    ASSIGN 
        k = NUM-ENTRIES(invalidChars).
    DO i = 1 TO k:
        ipField = REPLACE(ipField,ENTRY(i,invalidChars),ENTRY(i,replaceChars)).
    END.
    RETURN ipField.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

