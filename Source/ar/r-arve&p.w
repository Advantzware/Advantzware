&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ar\r-arve&p.w

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
DEFINE INPUT PARAMETER ip-post AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name    AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-comp-curr AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-audit-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE cChar-fld    AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}
{system/TaxProcs.i}

ASSIGN
    cocode = gcompany
    locode = gloc.

FIND FIRST company WHERE company.company = gcompany NO-LOCK NO-ERROR.
IF AVAILABLE company THEN lv-comp-curr = company.curr-code.

DEFINE NEW SHARED VARIABLE v-trnum AS INTEGER NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE wkdistrib NO-UNDO
    FIELD ACTNUM  LIKE account.actnum COLUMN-LABEL "Account"
    FIELD tr-dscr LIKE glhist.tr-dscr
    FIELD AMOUNT  AS DECIMAL FORMAT "->>>,>>>,>>>.99"
    FIELD debit   AS LOG 
    FIELD recs    AS INTEGER COLUMN-LABEL "Records" FORMAT ">>,>>>"
    FIELD tons    AS DECIMAL.

DEFINE            VARIABLE ws_debit        LIKE wkdistrib.amount COLUMN-LABEL "Debit" NO-UNDO.
DEFINE            VARIABLE ws_credit       LIKE wkdistrib.amount COLUMN-LABEL "Credit" NO-UNDO.
DEFINE            VARIABLE ws_net          LIKE wkdistrib.amount COLUMN-LABEL "Net" NO-UNDO.

DEFINE            VARIABLE posting         AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE            VARIABLE v-invalid       AS LOG       NO-UNDO.
DEFINE            VARIABLE v-postable      AS LOG       INIT NO NO-UNDO.
DEFINE            VARIABLE xar-acct        LIKE account.actnum NO-UNDO.
DEFINE            VARIABLE xar-stax        LIKE account.actnum NO-UNDO.
DEFINE            VARIABLE xar-freight     LIKE account.actnum NO-UNDO.
DEFINE            VARIABLE xar-cur-acct    LIKE account.actnum NO-UNDO.  /* currency ar account */
DEFINE            VARIABLE xar-pd-acct     LIKE account.actnum NO-UNDO.  /* currency ar account */
DEFINE            VARIABLE lorow           AS INTEGER   NO-UNDO INIT 21.
DEFINE            VARIABLE v-post-ok       AS LOG       NO-UNDO.
DEFINE            VARIABLE total-msf       LIKE ar-invl.amt-msf NO-UNDO.
DEFINE            VARIABLE v-s-inv-no      LIKE inv-head.inv-no INIT 0 NO-UNDO.
DEFINE            VARIABLE v-e-inv-no      LIKE v-s-inv-no INIT 99999999 NO-UNDO.
DEFINE            VARIABLE v-s-date        LIKE inv-head.inv-date FORMAT "99/99/9999"
    INIT 01/01/0001 NO-UNDO.
DEFINE            VARIABLE v-e-date        LIKE v-s-date INIT TODAY NO-UNDO.
DEFINE            VARIABLE export_opt      AS CHARACTER NO-UNDO INITIAL "ASI".
DEFINE            VARIABLE t-rec-written   AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-rec-written   AS INTEGER   NO-UNDO.
DEFINE NEW SHARED VARIABLE v-term-id       AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-print-fmt     AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-ftp-done      AS LOG       NO-UNDO.
DEFINE            VARIABLE v-sort          AS LOGICAL   INIT YES FORMAT "Y/N" NO-UNDO.
DEFINE            VARIABLE hdOutboundProcs AS HANDLE    NO-UNDO.
DEFINE            VARIABLE cFieldInProcess AS CHARACTER NO-UNDO.
DEFINE            VARIABLE cFieldPostType  AS CHARACTER NO-UNDO.
DEFINE            VARIABLE cFieldUserId    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE cFieldDateTime  AS CHARACTER NO-UNDO.
DEFINE            VARIABLE hdInvoiceProcs  AS HANDLE    NO-UNDO.

RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "INVPRINT"
    NO-LOCK NO-ERROR.
v-print-fmt = IF AVAILABLE sys-ctrl THEN sys-ctrl.char-fld ELSE "".

/*IF PROGRAM-NAME(2) MATCHES "*r-inve&p*" THEN
posting = TRUE.
ELSE
posting = FALSE.
*/
posting = IF ip-post THEN YES ELSE NO.

IF ip-post THEN ASSIGN cChar-fld = "AU4".
ELSE ASSIGN cChar-fld = "AU2".


DO TRANSACTION:
    {sys/inc/postdate.i}
    {sys/inc/inexport.i}
END.

DEFINE BUFFER bf-period FOR period.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date begin_inv end_inv ~
begin_date end_date tb_sort tb_ton tb_export rd-dest tbAutoClose btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_inv end_inv ~
begin_date end_date tb_sort tb_ton tb_export rd-dest tbAutoClose 

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
     SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 16 BY 1.29.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_inv AS INTEGER FORMAT ">>>>>>>>>" INITIAL 0 
     LABEL "Beginning Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_inv AS INTEGER FORMAT ">>>>>>>>>" INITIAL 999999999 
     LABEL "Ending Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

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
"To Screen", 2
     SIZE 17.2 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 4.33.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_export AS LOGICAL INITIAL no 
     LABEL "Export/FTP  Invoices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE tb_sort AS LOGICAL INITIAL yes 
     LABEL "Sort by Customer Name?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_ton AS LOGICAL INITIAL no 
     LABEL "Print $/Ton?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.43 COL 43 COLON-ALIGNED
     tran-period AT ROW 3.62 COL 43 COLON-ALIGNED
     begin_inv AT ROW 5.29 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Invoice Number"
     end_inv AT ROW 5.29 COL 72 COLON-ALIGNED HELP
          "Enter Ending Invoice Number"
     begin_date AT ROW 6.24 COL 30 COLON-ALIGNED HELP
          "Enter Beginning Invoice Date"
     end_date AT ROW 6.24 COL 72 COLON-ALIGNED HELP
          "Enter Ending Invoice Date"
     tb_sort AT ROW 7.91 COL 37
     tb_ton AT ROW 8.86 COL 37
     tb_export AT ROW 9.76 COL 37
     lv-font-no AT ROW 12.14 COL 37 COLON-ALIGNED
     lv-ornt AT ROW 12.19 COL 47 NO-LABEL
     lines-per-page AT ROW 12.19 COL 90 COLON-ALIGNED
     rd-dest AT ROW 12.29 COL 4.8 NO-LABEL
     lv-font-name AT ROW 13.24 COL 31.8 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 15.1 COL 31.4
     tbAutoClose AT ROW 16.38 COL 31.4 WIDGET-ID 64
     btn-ok AT ROW 17.29 COL 31.2
     btn-cancel AT ROW 17.29 COL 52.6
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.05 COL 4
     " Output Destination" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 11.48 COL 4
     RECT-6 AT ROW 11.91 COL 3
     RECT-7 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100.8 BY 18
         BGCOLOR 15 .


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
         TITLE              = "A/R Invoice Edit/Posting Register"
         HEIGHT             = 17.86
         WIDTH              = 97.6
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
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lines-per-page:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-name:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-no:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-ornt:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       tb_export:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_export:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_ton:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       td-show-parm:HIDDEN IN FRAME FRAME-A           = TRUE.

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
ON END-ERROR OF C-Win /* A/R Invoice Edit/Posting Register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* A/R Invoice Edit/Posting Register */
DO:
        RUN spCommon_CheckPostingProcess(INPUT "ar-ctrl", INPUT "postInProcess", INPUT "postType", INPUT "postUserID",
            INPUT "postStartDtTm", INPUT cocode, INPUT STRING("AU4-" + cocode), INPUT YES, 
            OUTPUT cFieldInProcess, OUTPUT cFieldPostType, OUTPUT cFieldUserId, OUTPUT cFieldDateTime).
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        IF VALID-HANDLE(hdOutboundProcs) THEN
            DELETE PROCEDURE hdOutboundProcs.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Invoice Date */
DO:
  assign {&self-name}.      
  IF cChar-fld EQ "AU4" THEN DO:
   {ar/checkPeriod.i begin_date tran-date:SCREEN-VALUE 1}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv C-Win
ON LEAVE OF begin_inv IN FRAME FRAME-A /* Beginning Invoice# */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
        RUN spCommon_CheckPostingProcess(INPUT "ar-ctrl", INPUT "postInProcess", INPUT "postType", INPUT "postUserID",
            INPUT "postStartDtTm", INPUT cocode, INPUT STRING("AU4-" + cocode), INPUT YES, 
            OUTPUT cFieldInProcess, OUTPUT cFieldPostType, OUTPUT cFieldUserId, OUTPUT cFieldDateTime).
                                        
        IF VALID-HANDLE(hdOutboundProcs) THEN
            DELETE PROCEDURE hdOutboundProcs.
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
        DEFINE VARIABLE lv-post AS LOG NO-UNDO.
  
  run check-date.
  if v-invalid then return no-apply.  
  
  
  if cChar-fld eq "AU4" then do:
    {ar/checkPeriod.i begin_date:SCREEN-VALUE tran-date:SCREEN-VALUE 1}
    {ar/checkPeriod.i end_date:SCREEN-VALUE tran-date:SCREEN-VALUE 1}  
  END. 
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  ASSIGN v-s-inv-no = begin_inv
         v-e-inv-no = END_inv
         v-s-date   = begin_date
         v-e-date   = END_date   
         v-sort     = tb_sort.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.

        ASSIGN 
            v-s-inv-no = begin_inv
            v-e-inv-no = END_inv
            v-s-date   = begin_date
            v-e-date   = END_date   
            v-sort     = tb_sort.

        v-postable = NO.

        DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
            /* gdm - 11050906 */
            REPEAT:
                FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                    WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
                IF AVAILABLE gl-ctrl THEN 
                DO:
                    ASSIGN 
                        v-trnum       = gl-ctrl.trnum + 1
                        gl-ctrl.trnum = v-trnum.
                    FIND CURRENT gl-ctrl NO-LOCK.
                    LEAVE.
                END. /* IF AVAIL gl-ctrl */
            END. /* REPEAT */
        /* gdm - 11050906 */
        END.

        RUN run-report.

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
        END CASE.

        IF v-postable AND ip-post THEN 
        DO:

            lv-post = NO.

            MESSAGE "Post Invoices?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE lv-post.

            IF lv-post THEN 
            DO:
          
                RUN post-inv.
                RUN copy-report-to-audit-dir.        
      
                MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.      
            END.
            ELSE RUN undo-trnum.
        END.

        ELSE do:
          RUN undo-trnum.         
           MESSAGE "No invoices found to print - only approved and printed invoices can be posted." VIEW-AS ALERT-BOX ERROR.
        END. 
        IF v-ftp-done THEN MESSAGE "File Export/FTP is completed." VIEW-AS ALERT-BOX INFORMATION.

        SESSION:SET-WAIT-STATE("").

        IF ip-post THEN 
        DO:
            RUN spCommon_CheckPostingProcess(INPUT "ar-ctrl", INPUT "postInProcess", INPUT "postType", INPUT "postUserID",
                INPUT "postStartDtTm", INPUT cocode, INPUT STRING("AU4-" + cocode), INPUT YES, 
                OUTPUT cFieldInProcess, OUTPUT cFieldPostType, OUTPUT cFieldUserId, OUTPUT cFieldDateTime).
        END.
        IF tbAutoClose:CHECKED THEN 
        DO:
            RUN spCommon_CheckPostingProcess(INPUT "ar-ctrl", INPUT "postInProcess", INPUT "postType", INPUT "postUserID",
                INPUT "postStartDtTm", INPUT cocode, INPUT STRING("AU4-" + cocode), INPUT YES, 
                OUTPUT cFieldInProcess, OUTPUT cFieldPostType, OUTPUT cFieldUserId, OUTPUT cFieldDateTime).   
                                            
            IF VALID-HANDLE(hdOutboundProcs) THEN
                DELETE PROCEDURE hdOutboundProcs.
            APPLY "close" TO THIS-PROCEDURE.
        END. /* IF tbAutoClose:CHECKED THEN */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Invoice Date */
DO:
  assign {&self-name}.    
  IF cChar-fld EQ "AU4" THEN DO:
      {ar/checkPeriod.i end_date tran-date:SCREEN-VALUE 1}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv C-Win
ON LEAVE OF end_inv IN FRAME FRAME-A /* Ending Invoice# */
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
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE        = ENTRY(1,char-val)
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
        ASSIGN 
            lines-per-page = 68.
        DISPLAY lines-per-page WITH FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_export
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_export C-Win
ON VALUE-CHANGED OF tb_export IN FRAME FRAME-A /* Export/FTP  Invoices? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sort C-Win
ON VALUE-CHANGED OF tb_sort IN FRAME FRAME-A /* Sort by Customer Name? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ton
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ton C-Win
ON VALUE-CHANGED OF tb_ton IN FRAME FRAME-A /* Print $/Ton? */
DO:
        IF {&self-name}:SCREEN-VALUE EQ "Yes" THEN
            lv-ornt:SCREEN-VALUE = "L".
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


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Post Date */
DO:
        ASSIGN {&self-name}.

        IF LASTKEY NE -1 THEN 
        DO:
            RUN check-date.
            IF v-invalid THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-period C-Win
ON LEAVE OF tran-period IN FRAME FRAME-A /* Period */
DO:
        ASSIGN {&self-name}.
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
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    RUN init-proc.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i cChar-fld }
  
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    RUN spCommon_CheckPostingProcess(INPUT "ar-ctrl", INPUT "postInProcess", INPUT "postType", INPUT "postUserID",
        INPUT "postStartDtTm", INPUT cocode, INPUT STRING("AU4-" + cocode), INPUT NO, 
        OUTPUT cFieldInProcess, OUTPUT cFieldPostType, OUTPUT cFieldUserId, OUTPUT cFieldDateTime).
    IF cFieldInProcess EQ "Yes" THEN
    DO:    
        MESSAGE "Another user " cFieldUserId " started posting from " cFieldPostType " at " cFieldDateTime " and this process does not " 
            "support multiple people posting at the same time. Please try again later." VIEW-AS ALERT-BOX INFORMATION.
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    IF NOT ip-post THEN
        ASSIGN tran-date:HIDDEN IN FRAME {&FRAME-NAME}   = YES
            tran-period:HIDDEN IN FRAME {&FRAME-NAME} = YES
            tran-date                                 = TODAY.

    DO WITH FRAME {&frame-name}:
        {custom/usrprint.i}

        ASSIGN
            end_date:SCREEN-VALUE = STRING(TODAY)
            end_date              = TODAY.

        IF postdate-log THEN 
        DO:
            ASSIGN
                tran-date:SCREEN-VALUE = STRING(TODAY)
                tran-date              = TODAY.
            RUN check-date.
        END.

        ELSE
            ASSIGN
                tran-date:SCREEN-VALUE   = ""
                tran-period:SCREEN-VALUE = "".

        IF v-print-fmt = "Frankstn" OR v-print-fmt = "MIRPKG" OR v-print-fmt = "ContSrvc"
            THEN tb_export:SENSITIVE = YES.
        ELSE ASSIGN tb_export              = NO
                tb_export:SCREEN-VALUE = "NO" 
                tb_export:SENSITIVE    = NO.

        APPLY "entry" TO tran-date.
    END.

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
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DO WITH FRAME {&frame-name}:
        v-invalid = NO.
    
        RUN GL_CheckModClosePeriod(INPUT cocode, INPUT DATE(tran-date), INPUT "AR", OUTPUT cMessage, OUTPUT lSuccess ) .  
        IF NOT lSuccess THEN 
        DO:
            MESSAGE cMessage VIEW-AS ALERT-BOX INFORMATION.
            v-invalid = YES.
        END.

        FIND FIRST period                   
            WHERE period.company EQ cocode
            AND period.pst     LE tran-date
            AND period.pend    GE tran-date
            NO-LOCK NO-ERROR.
        IF AVAILABLE period THEN tran-period:SCREEN-VALUE = STRING(period.pnum).
    
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-ar C-Win 
PROCEDURE clear-ar :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    FOR EACH ar-inv WHERE ar-inv.company  EQ cocode
        AND ar-inv.posted   EQ NO
        /*and ar-inv.printed  eq NO */ :

        IF NOT CAN-FIND(FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no) THEN
            DELETE ar-inv.
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
    DEFINE VARIABLE targetfile AS CHARACTER FORMAT "X(50)" NO-UNDO.
    DEFINE VARIABLE dirname1   AS CHARACTER FORMAT "X(20)" NO-UNDO.
    DEFINE VARIABLE dirname2   AS CHARACTER FORMAT "X(20)" NO-UNDO.
    DEFINE VARIABLE dirname3   AS CHARACTER FORMAT "X(20)" NO-UNDO.

    ASSIGN 
        targetfile = lv-audit-dir + "\AR\AU4\Run#"
                    + STRING(v-trnum) + ".txt"
        dirname1   = lv-audit-dir
        dirname2   = lv-audit-dir + "\AR"
        dirname3   = lv-audit-dir + "\AR\AU4".

    OS-COPY VALUE(list-name) VALUE (targetfile).

    IF SEARCH(targetfile) EQ ? THEN 
    DO:
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
  DISPLAY tran-date tran-period begin_inv end_inv begin_date end_date tb_sort 
          tb_ton tb_export rd-dest tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date begin_inv end_inv begin_date end_date tb_sort 
         tb_ton tb_export rd-dest tbAutoClose btn-ok btn-cancel 
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
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "AREXP"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = cocode
            sys-ctrl.name     = "AREXP"
            sys-ctrl.descrip  = "A/R Export option"
            sys-ctrl.char-fld = "ASI".
        MESSAGE "System control record NOT found.  Please enter A/R Export Option".
        UPDATE sys-ctrl.char-fld.
    END.
    export_opt = sys-ctrl.char-fld.

    RELEASE sys-ctrl.

    FIND FIRST sys-ctrl WHERE
        sys-ctrl.company EQ cocode AND
        sys-ctrl.name    EQ "AUDITDIR"
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE sys-ctrl THEN 
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = cocode
            sys-ctrl.name     = "AUDITDIR"
            sys-ctrl.descrip  = "Audit Trails directory"
            sys-ctrl.char-fld = ".\AUDIT TRAILS".
    END.

    lv-audit-dir = sys-ctrl.char-fld.

    IF LOOKUP(SUBSTR(lv-audit-dir,LENGTH(lv-audit-dir),1),"/,\") > 0 THEN
        lv-audit-dir = SUBSTR(lv-audit-dir,1,LENGTH(lv-audit-dir) - 1).

    RELEASE sys-ctrl.

    DO :
        FIND FIRST ar-ctrl WHERE ar-ctrl.company = cocode NO-LOCK.
        xar-acct = ar-ctrl.receivables.
        FIND FIRST account WHERE
            account.company = cocode AND
            account.actnum  = ar-ctrl.receivables NO-ERROR.
        IF NOT AVAILABLE account THEN
        DO:    
            MESSAGE "A/R Control Files has a null or invalid Receivables Account." VIEW-AS ALERT-BOX ERROR.    
        END.
        xar-freight = ar-ctrl.freight.
        FIND FIRST account WHERE
            account.company = cocode AND
            account.actnum  = ar-ctrl.freight NO-ERROR.
        IF NOT AVAILABLE account THEN
        DO:    
            MESSAGE "A/R Control Files has a null or invalid Freight Account." VIEW-AS ALERT-BOX ERROR.    
        END.
        xar-stax = ar-ctrl.stax.
        FIND FIRST account WHERE
            account.company = cocode AND
            account.actnum  = ar-ctrl.stax NO-ERROR.
        IF NOT AVAILABLE account THEN
        DO:    
            MESSAGE "A/R Control Files has a null or invalid Sales Tax Account." VIEW-AS ALERT-BOX ERROR.    
        END.
        RELEASE ar-ctrl.
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

    IF init-dir = "" THEN init-dir = "c:\temp" .
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
    RUN scr-rpt-d.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateInvoiceLineTax C-Win 
PROCEDURE pCreateInvoiceLineTax PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ar-invl      FOR ar-invl.
    DEFINE           BUFFER bf-InvoiceLineTax FOR InvoiceLineTax.
    
    RUN ar/InvoiceProcs.p PERSISTENT SET hdInvoiceProcs.
    
    RUN invoice_pCreateInvoiceLineTax IN hdInvoiceProcs (
        INPUT ipbf-ar-invl.rec_key,
        INPUT ROWID(ipbf-ar-invl),                                      
        INPUT TABLE ttTaxDetail BY-REFERENCE  
        ).
    DELETE OBJECT hdInvoiceProcs.
                                   
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
    post-2:
    DO TRANSACTION:
        IF NOT v-post-ok THEN LEAVE post-2.

        FOR EACH wkdistrib:
            RUN GL_SpCreateGLHist(cocode,
                wkdistrib.actnum,
                "ARINV",
                wkdistrib.tr-dscr,
                tran-date,
                wkdistrib.amount,
                v-trnum,
                tran-period,
                "A",
                tran-date,
                "Inv: " + SUBSTRING(wkdistrib.tr-dscr,INDEX(wkdistrib.tr-dscr,'#') + 1,45),
                "AR").

        END.    /* each wkdistrib */

    END. /* post-2 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-inv C-Win 
PROCEDURE post-inv :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    post-1:
    DO TRANSACTION ON ERROR UNDO WITH WIDTH 255:

        v-post-ok = YES.

        FOR EACH ar-inv
            WHERE ar-inv.company  EQ cocode
            AND ar-inv.posted   EQ NO
            AND ar-inv.printed  EQ YES
            AND ar-inv.inv-no   GE v-s-inv-no
            AND ar-inv.inv-no   LE v-e-inv-no
            AND ar-inv.inv-date GE v-s-date
            AND ar-inv.inv-date LE v-e-date
            AND CAN-FIND(FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no )
            ON ERROR UNDO post-1, LEAVE post-1:

            PUT SCREEN ROW lorow COLUMNS 70 STRING(ar-inv.inv-no,">>>>>>>9") .

            ASSIGN
                ar-inv.period     = tran-period
                ar-inv.posted     = YES
                ar-inv.prod-date  = tran-date /* Using prod-date as a posted-date #53205 */
                ar-inv.postedDate = tran-date
                ar-inv.runNumber  = v-trnum
                ar-inv.glYear     = YEAR(tran-date)
                .


            IF NOT ar-inv.EdiInvoice THEN
                RUN pRunAPIOutboundTrigger(BUFFER ar-inv).                        
           
            RUN ed/asi/o810hook.p (RECID(inv-head), NO, NO).          
      
            /* Create eddoc for invoice if required */
            FIND FIRST edmast NO-LOCK
                WHERE edmast.cust EQ ar-inv.cust-no
                NO-ERROR.
            IF AVAILABLE edmast THEN 
            DO:
                FIND FIRST eddoc NO-LOCK
                    WHERE eddoc.setid EQ '810'
                    AND eddoc.partner EQ edmast.partner
                    AND eddoc.docid = STRING(ar-inv.inv-no)
                    NO-ERROR.
                IF NOT AVAILABLE eddoc THEN 
                DO:
                    RUN ed/asi/o810hook.p (RECID(ar-inv), NO, NO).
                    FIND FIRST edcode NO-LOCK
                        WHERE edcode.partner EQ edmast.partner
                        NO-ERROR.
                    IF NOT AVAILABLE edcode THEN
                        FIND FIRST edcode NO-LOCK
                            WHERE edcode.partner EQ edmast.partnerGrp
                            NO-ERROR.
                    IF AVAILABLE edcode AND edcode.sendFileOnPrint THEN
                        RUN ed/asi/write810.p (INPUT cocode, INPUT ar-inv.inv-no).
                END. /* If eddoc not available */
            END. /* If edi 810 customer */
           
     
            FIND FIRST cust
                {sys/ref/custW.i}
                AND cust.cust-no EQ ar-inv.cust-no
        use-index cust exclusive-lock .
            ASSIGN
                cust.sales[tran-period]   = cust.sales[tran-period]   +
                             (ar-inv.net - ar-inv.tax-amt)
                cust.n-sales[tran-period] = cust.n-sales[tran-period] + 1
                /*     cust.sales[13]        = cust.sales[13]        +
                                             (ar-inv.net  - ar-inv.tax-amt)
                */
                cust.ytd-sales            = cust.ytd-sales        +
                             (ar-inv.net  - ar-inv.tax-amt)
                cust.n-sales[13]          = cust.n-sales[13]      + 1
                cust.acc-bal              = cust.acc-bal          + ar-inv.net.

            IF cust.acc-bal GE cust.hibal THEN
                ASSIGN
                    cust.hibal      = cust.acc-bal
                    cust.hibal-date = ar-inv.inv-date.

            FOR EACH ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no:

                ASSIGN
                    ar-invl.period  = tran-period
                    ar-invl.posted  = YES
                    ar-invl.inv-qty = ar-invl.qty
                    total-msf       = total-msf + ar-invl.amt-msf.

                /*cost entered from A-U-1*/
                IF ar-invl.t-cost EQ 0 AND ar-invl.cost NE 0 THEN
                DO:
                    IF ar-invl.dscr[1] EQ "M" OR ar-invl.dscr[1] EQ ""  THEN /*M*/
                        ar-invl.t-cost = ar-invl.cost * (ar-invl.inv-qty / 1000).
                    ELSE /*EA*/
                        ar-invl.t-cost = ar-invl.cost * ar-invl.inv-qty.
                END.
      
                RUN pCreateInvoiceLineTax(BUFFER ar-invl).
  
            END. /* for each ar-invl */

            ASSIGN
                cust.ptd-msf[tran-period] = cust.ptd-msf[tran-period] + total-msf
                cust.ytd-msf              = cust.ytd-msf          + total-msf.

            CREATE ar-ledger.
            ASSIGN
                ar-ledger.company  = cocode
                ar-ledger.cust-no  = ar-inv.cust-no
                ar-ledger.amt      = - ar-inv.net
                ar-ledger.ref-num  = "INV# " + string(ar-inv.inv-no)
                ar-ledger.ref-date = ar-inv.inv-date
                ar-ledger.tr-num   = v-trnum
                ar-ledger.tr-date  = tran-date.
            RELEASE ar-ledger.
    
            RUN pPostSalesTax (
                INPUT ROWID(ar-inv)
                ).
        END. /* for each ar-inv */

        FIND CURRENT cust NO-LOCK NO-ERROR.

        RUN post-gl.
    /*RUN clear-ar.*/   /* ticket 54620*/
    END. /* post-1 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPostSalesTax C-Win 
PROCEDURE pPostSalesTax PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriArInv AS ROWID NO-UNDO.
    
    DEFINE VARIABLE dInvoiceTotal    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvoiceSubTotal AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotalTax        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.

    RUN Tax_CalculateForArInv  (
        INPUT  ipriArInv,
        INPUT  locode,
        INPUT  "INVOICE", /*  Message Type "INVOICE" or "QUOTATION" */
        INPUT  TRUE,      /* Post To journal */
        INPUT  "GetTaxAmountFinal", /* Trigger ID */
        OUTPUT dTotalTax,
        OUTPUT dInvoiceTotal,
        OUTPUT dinvoiceSubTotal,
        OUTPUT lError,
        OUTPUT cMessage
        ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuoteSalesTax C-Win 
PROCEDURE pQuoteSalesTax PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriArInv AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE     FOR ttTaxDetail.
    
    DEFINE VARIABLE dInvoiceTotal    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvoiceSubTotal AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotalTax        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    
    RUN Tax_CalculateForArInvWithDetail (
        INPUT  ipriArInv,
        INPUT  locode,
        INPUT  "QUOTATION", /*  Message Type "INVOICE" or "QUOTATION" */
        INPUT  FALSE,       /* Post To journal */
        INPUT  "GetTaxAmount", /* Trigger ID */
        OUTPUT dTotalTax,
        OUTPUT dInvoiceTotal,
        OUTPUT dinvoiceSubTotal,
        OUTPUT TABLE ttTaxDetail,
        OUTPUT lError,
        OUTPUT cMessage
        ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunApiOutboundTrigger C-Win 
PROCEDURE pRunApiOutboundTrigger :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ar-inv FOR ar-inv.

    DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAPIID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTriggerID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrimaryID   AS CHARACTER NO-UNDO.
   

    IF AVAILABLE ipbf-ar-inv AND ipbf-ar-inv.gross NE 0 THEN 
    DO:   
        ASSIGN 
            cAPIID       = "SendInvoice"
            cTriggerID   = "PostInvoice"
            cPrimaryID   = STRING(ipbf-ar-inv.inv-no)
            cDescription = cAPIID + " triggered by " + cTriggerID + " from r-arve&p.w for Invoice: " + cPrimaryID
            . 
        RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs (
            INPUT  ipbf-ar-inv.company,           /* Company Code (Mandatory) */
            INPUT  locode,                        /* Location Code (Mandatory) */
            INPUT  cAPIID,                        /* API ID (Mandatory) */
            INPUT  ipbf-ar-inv.cust-no,           /* Scope ID */
            INPUT  "Customer",                    /* Scope Type */
            INPUT  cTriggerID,                    /* Trigger ID (Mandatory) */
            INPUT  "ar-inv",                      /* Comma separated list of table names for which data being sent (Mandatory) */
            INPUT  STRING(ROWID(ipbf-ar-inv)),    /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
            INPUT  cPrimaryID,                    /* Primary ID for which API is called for (Mandatory) */   
            INPUT  cDescription,                  /* Event's description (Optional) */
            OUTPUT lSuccess,                      /* Success/Failure flag */
            OUTPUT cMessage                       /* Status message */
            ) NO-ERROR.


        RUN Outbound_ResetContext IN hdOutboundProcs.
    END. /*avail ar-inv*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- oe/invpost.p 10/94 gb */
    /* Invoicing  - Edit Register & Post Invoicing Transactions                   */
    /* -------------------------------------------------------------------------- */

    FOR EACH wkdistrib:
        DELETE wkdistrib.
    END.

    FORM
        WITH FRAME f-distrib DOWN WIDTH 140 NO-BOX STREAM-IO.

    FORM
        WITH FRAME f-distrib-t DOWN WIDTH 140 NO-BOX STREAM-IO.

    DEFINE VARIABLE time_stamp AS ch      NO-UNDO.
    DEFINE VARIABLE g1         AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE g2         LIKE g1 NO-UNDO.
    DEFINE VARIABLE g3         LIKE g1 NO-UNDO.
    DEFINE VARIABLE g4         LIKE g1 NO-UNDO.
    DEFINE VARIABLE v1         LIKE g1 NO-UNDO.
    DEFINE VARIABLE v2         LIKE g1 NO-UNDO.
    DEFINE VARIABLE tot        LIKE g1 NO-UNDO.
    DEFINE VARIABLE ld-tons    AS DECIMAL EXTENT 3 NO-UNDO.
    DEFINE VARIABLE ld-pton    AS DECIMAL NO-UNDO.
    /*DEF VAR v-sort AS LOGICAL /*INIT YES*/ FORMAT "Y/N" NO-UNDO.*/
    DEFINE VARIABLE ld-gl-amt  AS DECIMAL NO-UNDO.

    DEFINE VARIABLE ws_debit   LIKE wkdistrib.amount COLUMN-LABEL "Debit" NO-UNDO.
    DEFINE VARIABLE ws_credit  LIKE wkdistrib.amount COLUMN-LABEL "Credit" NO-UNDO.
    DEFINE VARIABLE ws_net     LIKE wkdistrib.amount COLUMN-LABEL "Net" NO-UNDO.
    DEFINE VARIABLE num-recs   LIKE wkdistrib.recs NO-UNDO.
    DEFINE VARIABLE tot-debit  LIKE wkdistrib.amount NO-UNDO.
    DEFINE VARIABLE tot-credit LIKE wkdistrib.amount NO-UNDO.
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.

    {sys/form/r-top3w.f}

    DEFINE VARIABLE lv-head AS CHARACTER FORMAT 'X(78)' NO-UNDO.
    IF NOT posting THEN
        lv-head =
            "C U S T O M E R   I N V O I C E   E D I T   R E G I S T E R".
    ELSE
        lv-head =
            "I N V O I C E   P O S T I N G".
    /* {sys/inc/ctrtext.i head 80}. */


    FIND FIRST period                   
        WHERE period.company EQ gcompany
        AND period.pst     LE tran-date
        AND period.pend    GE tran-date
        NO-LOCK NO-ERROR.


    FORMAT HEADER
        "CUST.#   Name                          INVOICE# INV.DATE"
        "        AMOUNT  LINE DESCRIPTION" SKIP
        FILL("_",130) FORMAT "x(130)"
        WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top PAGE-TOP WIDTH 132 STREAM-IO.

    FORMAT HEADER
        "CUST.#   Name                          INVOICE# INV.DATE"
        "        AMOUNT  LINE DESCRIPTION                        "
        "             $/TON      TONS"
        SKIP FILL("_",142) FORMAT "x(142)"
        WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top-t PAGE-TOP WIDTH 142 STREAM-IO.

    ASSIGN
        v-sort     = (IF posting THEN YES ELSE v-sort) /* for posting */
        time_stamp = STRING(TIME, "HH:MMam")
        tmpstore   = FILL("_",125).

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.
    ASSIGN 
        v1 = 0
        v2 = 0
        g1 = 0
        g2 = 0
        g3 = 0
        g4 = 0
        .

    DO: /* REPEAT: 9508 cah */
        SESSION:SET-WAIT-STATE("general").  /*{sys/msg/rproc.i} */
        ASSIGN
            str-tit  = coname + " - " + loname
            str-tit2 = "CUSTOMER INVOICES  -  "
    + (IF posting THEN "POSTING REPORT" ELSE "EDIT REGISTER") + "  " + STRING(v-trnum)
            str-tit3 = STRING(tran-date) + " - Period " + STRING(tran-period,"99")
            {sys/inc/ctrtext.i str-tit 112}
            {sys/inc/ctrtext.i str-tit2 114}
            {sys/inc/ctrtext.i str-tit3 132}.


        DISPLAY "" WITH FRAME r-top.
        IF tb_ton THEN
            DISPLAY "" WITH FRAME f-top-t.
        ELSE
            DISPLAY "" WITH FRAME f-top.

        {sa/sa-sls01.i}
        v-term-id = v-term.

        FOR EACH cust {sys/ref/custW.i} NO-LOCK,
      EACH ar-inv
      WHERE ar-inv.company  EQ cocode
        AND ar-inv.posted   EQ NO
        AND (IF posting THEN ar-inv.printed EQ TRUE ELSE TRUE)
        AND ar-inv.cust-no  EQ cust.cust-no
        AND ar-inv.loc      NE "Zqw"
        AND ar-inv.inv-no   GE v-s-inv-no
        AND ar-inv.inv-no   LE v-e-inv-no
        AND ar-inv.inv-date GE v-s-date
        AND ar-inv.inv-date LE v-e-date
        AND CAN-FIND(FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no
                      /*AND ar-invl.amt NE 0*/ )
      NO-LOCK

      BREAK BY (IF v-sort THEN cust.name ELSE "")
            BY cust.cust-no
            BY ar-inv.inv-no
      WITH FRAME f-det:

        CREATE report.
        ASSIGN
            report.term-id = v-term-id
            report.key-01  = STRING(ar-inv.inv-no,"9999999999")
            report.rec-id  = RECID(ar-inv).  

        IF cust.factored THEN
            FOR EACH ar-invl NO-LOCK WHERE ar-invl.x-no = ar-inv.x-no:
                IF CAN-FIND(FIRST itemfg WHERE itemfg.company  EQ ar-inv.company
                    AND itemfg.i-no     EQ ar-invl.i-no
                    AND itemfg.factored = YES)
                    OR ar-invl.i-no = ""
                    THEN 
                DO:
                    report.key-02 = "Factored".  /* for oe/rep/expfrank.p task#  09200521*/
                    LEAVE.
                END.
            END.

        PUT SCREEN ROW lorow COLUMNS 70 STRING(ar-inv.inv-no,">>>>>>>9") .
        IF FIRST-OF(cust.cust-no) THEN PUT cust.cust-no SPACE(1) cust.name.


        PUT ar-inv.inv-no   TO 47 FORM ">>>>>>>9"
            ar-inv.inv-date AT 49 FORM "99/99/99"
            ar-inv.net      AT 58.

        ASSIGN
            v-postable = YES
            v2         = v2 + net
            v1         = v1 + ar-inv.disc-taken.

        RELEASE currency.
        IF lv-comp-curr NE "" AND lv-comp-curr NE ar-inv.curr-code[1] THEN
            FIND FIRST currency NO-LOCK
                WHERE currency.company     EQ ar-inv.company
                AND currency.c-code      EQ ar-inv.curr-code[1]
                AND currency.ar-ast-acct NE ""
                AND currency.ex-rate     GT 0
                NO-ERROR.        
        xar-acct = STRING(DYNAMIC-FUNCTION("GL_GetAccountAR", cust.company, cust.cust-no)).
        cDescription = "HEADER".
        {sys/inc/gldstsum.i xar-acct "ar-inv.gross" YES cDescription }

        IF AVAILABLE currency THEN 
        DO:
            ld-gl-amt = (ar-inv.gross * currency.ex-rate) - ar-inv.gross.
            {sys/inc/gldstsum.i currency.ar-ast-acct ld-gl-amt YES cDescription}
        END.

        FOR EACH ar-invl NO-LOCK WHERE ar-invl.x-no EQ ar-inv.x-no
            BREAK BY ar-invl.line
            WITH FRAME f-det NO-BOX NO-LABELS WIDTH 132:

            FIND FIRST itemfg
                WHERE itemfg.company EQ ar-inv.company
                AND itemfg.i-no    EQ ar-invl.i-no
                AND ar-invl.i-no   NE ""
                AND ar-invl.misc   EQ NO
                NO-LOCK NO-ERROR.

            ld-gl-amt = ar-invl.amt * -1 *
                (IF AVAILABLE currency THEN currency.ex-rate ELSE 1).

            cDescription = "LINE " + string(ar-invl.LINE).
            {sys/inc/gldstsum.i ar-invl.actnum ld-gl-amt NO cDescription }

            ASSIGN
                ld-tons[1] = IF ar-invl.t-weight NE 0 THEN ar-invl.t-weight
                    ELSE
                    IF AVAILABLE itemfg THEN (itemfg.weight-100 * ar-invl.inv-qty / 100)
                    ELSE 0
                ld-tons[1] = ld-tons[1] / 2000.

            IF ld-tons[1] EQ ? THEN ld-tons[1] = 0.

            ASSIGN
                wkdistrib.tons = ld-tons[1]
                ld-pton        = ar-invl.amt / ld-tons[1]
                ld-tons[2]     = ld-tons[2] + ld-tons[1].

            IF ld-pton EQ ? THEN ld-pton = 0.

            PUT ar-invl.line                      AT 75   FORMAT ">>9"
                ar-invl.i-name                    AT 79
                SPACE(1)
                ar-invl.amt.

            IF tb_ton THEN
                PUT ld-pton                         TO 132  FORMAT "->>>>9.99"
                    ld-tons[1]                      TO 142  FORMAT "->>>9.999".

            PUT SKIP.

            IF ar-invl.i-dscr NE "" THEN PUT ar-invl.i-dscr AT 79 SKIP.
        END. /* each ar-invl */

        IF ar-inv.freight NE 0 AND ar-inv.f-bill THEN 
        DO:
            PUT "FREIGHT" AT 79 SPACE(1)
                ar-inv.freight FORMAT "->>,>>>,>>9.99" AT 110 SKIP.

            ld-gl-amt = ar-inv.freight * -1 *
                (IF AVAILABLE currency THEN currency.ex-rate ELSE 1).

            cDescription = "FREIGHT".
            {sys/inc/gldstsum.i xar-freight ld-gl-amt NO cDescription}
        END.

        IF ar-inv.tax-amt NE 0 THEN 
        DO:
            RUN pQuoteSalesTax (
                INPUT  ROWID(ar-inv),
                OUTPUT TABLE ttTaxDetail
                ).

            PUT "TAX" AT 79 SPACE(1)
                ar-inv.tax-amt FORMAT "->>,>>>,>>9.99" AT 110 SKIP.

            DEFINE VARIABLE tot-tax     AS DECIMAL   NO-UNDO.
            DEFINE VARIABLE ws_taxacct  AS CHARACTER NO-UNDO.
            DEFINE VARIABLE v-jd-taxamt AS DECIMAL   NO-UNDO.

            tot-tax = ar-inv.tax-amt.

            FOR EACH ttTaxDetail
                BREAK BY ttTaxDetail.invoiceNo:
                FIND FIRST account NO-LOCK
                    WHERE account.company EQ cocode
                    AND account.actnum  EQ ttTaxDetail.taxCodeAccount
                    NO-ERROR.
                ASSIGN
                    ws_taxacct  = IF AVAILABLE account THEN 
                                  ttTaxDetail.taxCodeAccount 
                              ELSE 
                                  xar-stax
                    v-jd-taxamt = ttTaxDetail.taxCodeTaxAmount
                    tot-tax     = tot-tax - v-jd-taxamt
                    .

                ld-gl-amt = v-jd-taxamt * -1.
            
                IF AVAILABLE currency THEN 
                    ld-gl-amt = ld-gl-amt * currency.ex-rate.

                cDescription = "TAX".
                {sys/inc/gldstsum.i ws_taxacct ld-gl-amt NO cDescription}
            END.
        END.

        IF LAST-OF(cust.cust-no) THEN 
        DO:
            ld-pton = v2 / ld-tons[2].
            IF ld-pton EQ ? THEN ld-pton = 0.

            IF tb_ton THEN
                DISPLAY "*  CUSTOMER TOTALS"      TO 56
                    v2                        AT 58 " *"
                    ld-pton    WHEN tb_ton    TO 132      FORMAT "->>>>9.99"
                    ld-tons[2] WHEN tb_ton    TO 142      FORMAT "->>>9.999"
                    SKIP(1)

                    WITH FRAME vtot{&frame}-t NO-BOX NO-LABELS WIDTH 142 STREAM-IO.

            ELSE
                DISPLAY "*  CUSTOMER TOTALS"      TO 56
                    v2                        AT 58 " *"
                    SKIP(1)

                    WITH FRAME vtot{&frame} NO-BOX NO-LABELS WIDTH 132 STREAM-IO.

            ASSIGN
                g1         = g1 + v1
                g2         = g2 + v2
                ld-tons[3] = ld-tons[3] + ld-tons[2]
                v1         = 0
                v2         = 0
                ld-tons[2] = 0.
        END.

        ASSIGN
            g3 = g3 + ar-inv.tax-amt
            g4 = g4 + ar-inv.freight.
    END. /* each invoice */

    ld-pton = g2 / ld-tons[3].
    IF ld-pton EQ ? THEN ld-pton = 0.

    IF tb_ton THEN
        DISPLAY "** GRAND TOTAL  "        TO 54
            g2                        AT 58 " **"
            ld-pton    WHEN tb_ton    TO 132            FORMAT "->>>>9.99"
            ld-tons[3] WHEN tb_ton    TO 142            FORMAT "->>>9.999"

            WITH NO-BOX NO-LABELS NO-UNDERLINE WIDTH 142 FRAME gt-t STREAM-IO.

    ELSE
        DISPLAY "** GRAND TOTAL  "        TO 54
            g2                        AT 58 " **"

            WITH NO-BOX NO-LABELS NO-UNDERLINE WIDTH 132 FRAME gt STREAM-IO.

    HIDE FRAME f-top.
    HIDE FRAME f-top-t.

    str-tit3 = STRING(tran-date) + " - Period " + STRING(tran-period,"99") + " - " +
        "Summary by Account".
    {sys/inc/ctrtext.i str-tit3 132}.

    PAGE.

    DO WITH FRAME f-distrib:
        ASSIGN
            num-recs  = 0
            ws_net    = 0
            ws_debit  = 0
            ws_credit = 0
            ld-tons   = 0.

        /*
        FOR EACH wkdistrib BREAK BY wkdistrib.actnum:
          num-recs = num-recs + 1.
    
          IF (NOT wkdistrib.use-cur-act AND wkdistrib.actnum EQ xar-acct) OR
             (wkdistrib.use-cur-act AND wkdistrib.actnum EQ xar-cur-acct) OR
             (wkdistrib.use-cur-act AND wkdistrib.actnum EQ xar-pd-acct)  THEN
            ASSIGN
             ws_credit  = ws_credit + (-1 * wkdistrib.amount)
             tot-credit = tot-credit + ( -1 * wkdistrib.amount)
             ws_debit   = ws_debit + (-1 * wkdistrib.amount)
             tot-debit  = tot-debit + (-1 * wkdistrib.amount).
    
          ELSE
            ASSIGN
             ws_debit  = ws_debit + (-1 * wkdistrib.amount) 
             tot-debit = tot-debit + (-1 * wkdistrib.amount).
    
          IF LAST-OF(wkdistrib.actnum)  THEN DO:
             find account where account.company eq cocode
                  and account.actnum  eq wkdistrib.actnum
              no-lock no-error.
              display wkdistrib.actnum
                  account.dscr when avail account
                  num-recs
                  ws_debit when ws_debit ne 0
                  ws_credit when ws_credit ne 0
               with frame f-distrib.
              if NOT avail account then DO:
                  display "*NOT IN ACCOUNT FILE*" @ account.dscr
                  with frame f-distrib.         
              END.
              down 1 with frame f-distrib.
              ASSIGN num-recs = 0
                     ws_debit = 0
                     ws_credit = 0.
          END.
        end.
    
        if avail wkdistrib then do:
          IF wkdistrib.use-cur-act THEN 
              find account
              where account.company eq cocode
                and account.actnum  eq xar-cur-acct
              no-lock no-error.
          ELSE find account
              where account.company eq cocode
                and account.actnum  eq xar-acct
              no-lock no-error.
          display xar-acct @ wkdistrib.actnum
                  account.dscr when avail account
                  num-recs
                  ws_debit when ws_debit ne 0
                  ws_credit when ws_credit ne 0
               with frame f-distrib.
          if not avail account then DO:
                  display "*NOT IN ACCOUNT FILE*" @ account.dscr
               with frame f-distrib.
    
          END.
          down 1 with frame f-distrib.
        end.*/

        FOR EACH wkdistrib BREAK BY wkdistrib.actnum:
            IF FIRST-OF(wkdistrib.actnum) THEN
                ASSIGN
                    num-recs   = 0
                    ws_debit   = 0
                    ws_credit  = 0
                    ld-tons[2] = 0.

            num-recs = num-recs + 1.

            IF wkdistrib.debit THEN
                ASSIGN
                    ws_debit  = ws_debit + wkdistrib.amount
                    tot-debit = tot-debit + wkdistrib.amount.
            ELSE
                ASSIGN
                    ws_credit  = ws_credit + ( -1 * wkdistrib.amount)
                    tot-credit = tot-credit + ( -1 * wkdistrib.amount)
                    ld-tons[1] = wkdistrib.tons
                    ld-tons[2] = ld-tons[2] + ld-tons[1]
                    ld-tons[3] = ld-tons[3] + ld-tons[1].

            FIND FIRST account
                WHERE account.company EQ cocode
                AND account.actnum  EQ wkdistrib.actnum
                NO-LOCK NO-ERROR.
            IF LAST-OF(wkdistrib.actnum) THEN 
            DO:
                ld-pton = ws_credit / ld-tons[2].
                IF ld-pton EQ ? THEN ld-pton = 0.

                IF tb_ton THEN 
                DO WITH FRAME f-distrib-t:
                    DISPLAY wkdistrib.actnum
                        account.dscr            
                        WHEN AVAILABLE account
                        num-recs
                        ws_debit                
                        WHEN ws_debit NE 0
                        ws_credit               
                        WHEN ws_credit NE 0
                        ld-pton                 
                        WHEN ws_credit NE 0
                        COLUMN-LABEL "$/Ton"
                        FORMAT "->>>>9.99"
                        ld-tons[2]              
                        WHEN ws_credit NE 0
                        COLUMN-LABEL "Tons"
                        FORMAT "->>>9.999".

                    IF NOT AVAILABLE account THEN
                        DISPLAY "*NOT IN ACCOUNT FILE*" @ account.dscr.
                    DOWN 1.
                END.

                ELSE
                DO WITH FRAME f-distrib:
                    DISPLAY wkdistrib.actnum
                        account.dscr            
                        WHEN AVAILABLE account
                        num-recs
                        ws_debit                
                        WHEN ws_debit NE 0
                        ws_credit               
                        WHEN ws_credit NE 0.

                    IF NOT AVAILABLE account THEN
                        DISPLAY "*NOT IN ACCOUNT FILE*" @ account.dscr.
                    DOWN 1.
                END.
            END.
        END. /* for each */

        ASSIGN
            ws_net  = tot-debit - tot-credit
            ld-pton = tot-credit / ld-tons[3].

        IF ld-pton EQ ? THEN ld-pton = 0.

        IF tb_ton THEN 
        DO WITH FRAME f-distrib-t:
            UNDERLINE ws_debit ws_credit ld-pton ld-tons[2].
            DOWN 1.

            DISPLAY "Totals" @ account.dscr
                tot-debit @ ws_debit
                tot-credit @ ws_credit
                ld-pton
                ld-tons[3] @ ld-tons[2].
            DOWN 1.

            DISPLAY "Net" @ account.dscr.
            IF ws_net GT 0 THEN DISPLAY ws_net @ ws_debit.
            ELSE DISPLAY (-1 * ws_net) @ ws_credit.
            DOWN 1.
        END.

        ELSE
        DO WITH FRAME f-distrib:
            UNDERLINE ws_debit ws_credit.
            DOWN 1.

            DISPLAY "Totals" @ account.dscr
                tot-debit @ ws_debit
                tot-credit @ ws_credit.
            DOWN 1.

            DISPLAY "Net" @ account.dscr.
            IF ws_net GT 0 THEN DISPLAY ws_net @ ws_debit.
            ELSE DISPLAY (-1 * ws_net) @ ws_credit.
            DOWN 1.
        END.
    END.
END.

HIDE ALL NO-PAUSE.
OUTPUT CLOSE.

/*IF v-print-fmt <> "frankstn" AND v-print-fmt <> "Mirpkg" THEN  tb_export = NO.*/
v-ftp-done = NO.
IF tb_export AND inexport-log THEN 
DO:    
    DEFINE VARIABLE v-exp-file AS CHARACTER NO-UNDO.
    v-exp-file = inexport-desc +  
        "ARINV_" + trim(v-print-fmt) + 
        substr(STRING(YEAR(TODAY),"9999"),3,2) +
        string(MONTH(TODAY),"99") +
        string(DAY(TODAY),"99") +
        substr(STRING(TIME,"HH:MM:SS"),1,2) +
        substr(STRING(TIME,"HH:MM:SS"),4,2) +
        substr(STRING(TIME,"HH:MM:SS"),7,2) + ".dat".
    OUTPUT TO VALUE(v-exp-file).
    IF inexport-cha EQ "CIT" THEN 
    DO:
        RUN ar/rep/expfrank.p .
        OUTPUT CLOSE.
        OUTPUT TO VALUE(".\ar\ftpcmd2.txt").     /* ftp text file */
        PUT UNFORMATTED 
            /*"open cs.ftptst.citonline.com" SKIP  /* ftp test server ip address */   */
            "open cs.ftp.citonline.com" SKIP  /* ftp Production server ip address */
            "ftpa1526" SKIP  /* userid*/
            "none" SKIP  /* password */
            "put " v-exp-file " " '"' "$$ ID=EP003F BID='DI1526' PASSWORD=NARF" '"' SKIP         /* file to transfer */
            "quit" .
        OUTPUT CLOSE.
        /*      OS-COMMAND SILENT value("ftp -v -i -s:.\oe\ftpcmd2.txt"). */
        v-ftp-done = YES.
    END.
    ELSE IF inexport-cha EQ "ContSrvc" THEN 
        DO:
            OUTPUT TO VALUE(v-exp-file).
            RUN ar/rep/expconts.p .
            OUTPUT CLOSE.
        END.
END.

FOR EACH report WHERE report.term-id EQ v-term-id: 
    DELETE report.
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE    NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha.

    lv-frame-hdl = FRAME {&frame-name}:handle.
    lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
    lv-field-hdl = lv-group-hdl:FIRST-CHILD .

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
            THEN 
        DO:
            IF lv-field-hdl:LABEL <> ? THEN 
                ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                    .
            ELSE 
            DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    .
                lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN 
                    DO:
                        parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                    END.
                    lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
                END.       
            END.                 
        END.            
        lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
    END.

    PUT SPACE(28)
        "< Selection Parameters >"
        SKIP(1).

    DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
        IF ENTRY(i,parm-fld-list) NE "" OR
            entry(i,parm-lbl-list) NE "" THEN 
        DO:

            lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                trim(ENTRY(i,parm-lbl-list)) + ":".

            PUT lv-label FORMAT "x(35)" AT 5
                SPACE(1)
                TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
                SKIP.              
        END.
    END.

    PUT FILL("-",80) FORMAT "x(80)" SKIP.

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
        IF AVAILABLE gl-ctrl THEN 
        DO:
            IF v-trnum = gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
            RELEASE gl-ctrl.
            LEAVE.
        END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
/* gdm - 11050906 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

