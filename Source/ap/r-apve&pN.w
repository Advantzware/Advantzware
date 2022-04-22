&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap\r-apve&pN.w

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
DEFINE VARIABLE list-name    AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-comp-curr AS CHARACTER NO-UNDO.
DEFINE VARIABLE ll-secure    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cAPSecure    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAPSecure    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cAPPostingUserId      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAPPostingPassword    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAPPostingPeriodRules AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-chk FOR ap-chk.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
IF AVAILABLE company THEN lv-comp-curr = company.curr-code.

DEFINE NEW SHARED VARIABLE v-post         AS LOG       INIT NO NO-UNDO.
DEFINE NEW SHARED VARIABLE v-trnum        AS INTEGER   NO-UNDO.

DEFINE            VARIABLE v-unline       AS CHARACTER FORMAT "x(80)" INIT
    "--------------- ------------------------- ------- ----------- ---".
DEFINE            VARIABLE time_stamp     AS ch.

DEFINE            VARIABLE v-invalid      AS LOG       NO-UNDO.
DEFINE            VARIABLE v-invalid-inv  AS LOG       NO-UNDO.
DEFINE            VARIABLE v-frt-acct     LIKE ap-ctrl.freight NO-UNDO.
DEFINE            VARIABLE v-cash-acct    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE xap-acct       LIKE account.actnum NO-UNDO.
DEFINE            VARIABLE xap-stax       LIKE account.actnum NO-UNDO.
DEFINE            VARIABLE lv-frt-total   AS DECIMAL   NO-UNDO.  /* accum total */
DEFINE            VARIABLE v-postable     AS LOG       INIT NO NO-UNDO.
DEFINE            VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE is-xprint-form AS LOGICAL.
DEFINE            VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-post        AS LOG       NO-UNDO.
DEFINE            VARIABLE ll-warned      AS LOG       NO-UNDO.
DEFINE            VARIABLE v-bank-code    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-bank-acct   AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD actnum   LIKE account.actnum
    FIELD ex-rate  LIKE currency.ex-rate INIT 1
    FIELD curr-amt LIKE ap-inv.net.

DEFINE VARIABLE v-fgpostgl    AS LOG       NO-UNDO.
DEFINE VARIABLE lv-audit-dir  AS CHARACTER NO-UNDO.


DO TRANSACTION :
    {sys/inc/fgpostgl.i}
    v-fgpostgl = fgpostgl NE "None".
    {sys/inc/rmpostgl.i}
    {sys/inc/postdate.i}
    {sys/inc/apsecure.i}
    {sys/inc/apautocheck.i}
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.company EQ cocode AND
        sys-ctrl.name    EQ "AUDITDIR"
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
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
END.

DEFINE VARIABLE lRecFound           AS LOGICAL          NO-UNDO.
DEFINE VARIABLE lAPInvoiceLength    AS LOGICAL          NO-UNDO.
DEFINE VARIABLE cNK1Value           AS CHARACTER        NO-UNDO.
DEFINE VARIABLE lAccessClose        AS LOGICAL          NO-UNDO.
DEFINE VARIABLE cAccessList         AS CHARACTER        NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "APInvoiceLength", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cNK1Value, OUTPUT lRecFound).
IF lRecFound THEN
    lAPInvoiceLength = logical(cNK1Value) NO-ERROR.      

DEF TEMP-TABLE tt-ap-invl NO-UNDO
                          FIELD row-id AS ROWID
                          FIELD actnum LIKE account.actnum
                          FIELD unit-pr LIKE ap-invl.unit-pr
                          FIELD amt LIKE ap-invl.amt
                          INDEX row-id row-id.

DEF TEMP-TABLE tt-ap-tax  NO-UNDO
                          FIELD row-id AS ROWID
                          FIELD actnum LIKE account.actnum
                          FIELD amt LIKE ap-invl.amt
                          FIELD curr-amt LIKE ap-invl.amt
                          FIELD cDescription AS CHARACTER
                          INDEX row-id row-id.
                          
RUN methods/prgsecur.p
            (INPUT "APSecure",
             INPUT "ALL", /* based on run, create, update, delete or all */
             INPUT NO,    /* use the directory in addition to the program */
             INPUT NO,    /* Show a message if not authorized */
             INPUT NO,    /* Group overrides user security? */
             OUTPUT lAPSecure, /* Allowed? Yes/NO */
             OUTPUT lAccessClose, /* used in template/windows.i  */
             OUTPUT cAccessList). /* list 1's and 0's indicating yes or no to run, create, update, delete */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date begin_vend end_vend ~
begin_date end_date begin_user end_user tb_sort rd-dest btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_vend end_vend ~
begin_date end_date begin_user end_user lbl_sort tb_sort rd-dest 

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

DEFINE VARIABLE begin_user AS CHARACTER FORMAT "x(8)" 
     LABEL "Beginning User ID" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "x(8)" 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_user AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending User ID" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Print G/L Acount Description?" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

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
"To Email", 5
     SIZE 15 BY 4.62 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 5.19.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 9.05.

DEFINE VARIABLE tb_sort AS LOGICAL INITIAL no 
     LABEL "Print G/L Account Description?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.43 COL 44 COLON-ALIGNED
     tran-period AT ROW 3.62 COL 44 COLON-ALIGNED
     begin_vend AT ROW 4.81 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend AT ROW 4.81 COL 71 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     begin_date AT ROW 6 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Invoice Date"
     end_date AT ROW 6 COL 71 COLON-ALIGNED HELP
          "Enter Ending Invoice Date"
     begin_user AT ROW 7.19 COL 29 COLON-ALIGNED HELP
          "Enter Beginning User ID"
     end_user AT ROW 7.19 COL 71 COLON-ALIGNED HELP
          "Enter Ending User ID"
     lbl_sort AT ROW 9.1 COL 30 COLON-ALIGNED NO-LABEL
     tb_sort AT ROW 9.1 COL 62
     lines-per-page AT ROW 11.24 COL 87 COLON-ALIGNED
     lv-font-no AT ROW 11.33 COL 31 COLON-ALIGNED
     lv-ornt AT ROW 11.33 COL 43 NO-LABEL
     rd-dest AT ROW 11.38 COL 5 NO-LABEL
     lv-font-name AT ROW 12.62 COL 28.4 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 15 COL 28.6
     btn-ok AT ROW 16.76 COL 28.6
     btn-cancel AT ROW 16.76 COL 50.8
     " Output Destination" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 10.62 COL 4
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.1 COL 4
     RECT-6 AT ROW 11.05 COL 3
     RECT-7 AT ROW 1.57 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.8 BY 19
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Vendor Invoices Edit/Post Register"
         HEIGHT             = 17.33
         WIDTH              = 95
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
       begin_user:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       end_user:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
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
       tb_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Vendor Invoices Edit/Post Register */
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
ON WINDOW-CLOSE OF C-Win /* Vendor Invoices Edit/Post Register */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Invoice Date */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_user C-Win
ON LEAVE OF begin_user IN FRAME FRAME-A /* Beginning User ID */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor# */
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
  DEF VAR lv-post AS LOG NO-UNDO.
  v-postable = NO.
     
  RUN check-inv-date(begin_date:SCREEN-VALUE, cAPPostingPeriodRules).
  if v-invalid-inv then return no-apply. 
  RUN check-inv-date(end_date:SCREEN-VALUE, cAPPostingPeriodRules).
  if v-invalid-inv then return no-apply.
  
  RUN pCheckUser.
  if v-invalid-inv then return no-apply.
  
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.

        DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
            /* gdm - 11050906 */
            loop:
            REPEAT:
                FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                    WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
                IF AVAILABLE gl-ctrl THEN 
                DO:
                    ASSIGN 
                        v-trnum       = gl-ctrl.trnum + 1
                        gl-ctrl.trnum = v-trnum.
                    FIND CURRENT gl-ctrl NO-LOCK.
                    LEAVE loop.
                END. /* IF AVAIL gl-ctrl */
            END. /* REPEAT */
        /* gdm - 11050906 */
        END.

        RUN run-report. 

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &type= ''
                            &begin_cust="begin_date"
                            &END_cust="begin_date"
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = ''
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = ''
                                  &begin_cust=''
                                  &END_cust='' 
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

                    END. 
                END.
            WHEN 6 THEN RUN output-to-port.
        END CASE.

        FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ap-ctrl THEN RETURN.
        IF AVAILABLE ap-ctrl  AND ap-ctrl.payables EQ "" THEN 
        DO:
            MESSAGE "No AP control record found. " VIEW-AS ALERT-BOX .
            RETURN.
        END.

        IF v-postable THEN 
        DO:
            lv-post = NO.

            MESSAGE "Post Invoices?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE lv-post.

            IF lv-post THEN 
            DO:

                SESSION:SET-WAIT-STATE("general").
                RUN post-gl.
                RUN copy-report-to-audit-dir.
                RUN clear-ap.

                MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
                APPLY "close" TO THIS-PROCEDURE.
            END.
            ELSE RUN undo-trnum.
        END.

        ELSE 
        DO:
            MESSAGE "No Invoices available for posting..." VIEW-AS ALERT-BOX ERROR.
            RUN undo-trnum.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Invoice Date */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_user C-Win
ON LEAVE OF end_user IN FRAME FRAME-A /* Ending User ID */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
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
        DEFINE VARIABLE char-val AS cha NO-UNDO.

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


&Scoped-define SELF-NAME tb_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sort C-Win
ON VALUE-CHANGED OF tb_sort IN FRAME FRAME-A /* Print G/L Account Description? */
DO:
        ASSIGN {&self-name}.
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
        IF LASTKEY NE -1 THEN 
        DO:               
            RUN valid-date.
            IF v-invalid THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON VALUE-CHANGED OF tran-date IN FRAME FRAME-A /* Post Date */
DO:
        ll-warned = NO.
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
    {sys/inc/reportsConfigNK1.i "PU3" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    {methods/nowait.i}

    DO WITH FRAME {&frame-name}:
        {custom/usrprint.i}
        
        RUN spGetSettingByName ("APPostingPassword", OUTPUT cAPPostingPassword).
        RUN spGetSettingByName ("APPostingPeriodRules", OUTPUT cAPPostingPeriodRules).        
        RUN spGetSettingByName ("APPostingUserId", OUTPUT cAPPostingUserId).
        
        ASSIGN
             begin_user:SCREEN-VALUE = USERID("ASI")
             end_user:SCREEN-VALUE   = USERID("ASI").
        IF cAPPostingUserId EQ "No" THEN
           ASSIGN
               begin_user:SENSITIVE    = NO
               end_user:SENSITIVE      = NO .
               
        IF postdate-log THEN 
        DO:
            tran-date:SCREEN-VALUE = STRING(TODAY).            
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-inv-date C-Win 
PROCEDURE check-inv-date :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-period LIKE period.pnum NO-UNDO.
    DEFINE INPUT PARAMETER ip-date AS CHARACTER.
    DEFINE INPUT PARAMETER ipcAPPostingPeriodRules AS CHARACTER.
    DEFINE VARIABLE lv-msg  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE v-month AS CHARACTER . 
    ASSIGN  
        lv-msg        = ""
        v-invalid-inv = NO 
        v-month       = SUBSTRING(STRING(TODAY),1,2) .
                
    FIND FIRST period NO-LOCK                 
         WHERE period.company EQ cocode
           AND period.pst     LE date(ip-date)
           AND period.pend    GE date(ip-date)
           AND ((period.pnum  EQ INT(v-month) AND ipcAPPostingPeriodRules EQ "CurrentPeriodOnly")
           OR ipcAPPostingPeriodRules EQ "OpenPeriodOnly")
           AND ((period.yr     EQ YEAR(TODAY) AND ipcAPPostingPeriodRules EQ "CurrentPeriodOnly")
           OR ipcAPPostingPeriodRules EQ "OpenPeriodOnly")   
         NO-ERROR.          

    IF NOT AVAILABLE period THEN
    DO:
        lv-msg = "CAN NOT POST OUT OF PERIOD, ENTER SECURITY PASSWORD OR  ENTER TO RETURN".
    END.

    ELSE IF NOT period.pstat THEN
            lv-msg = "Period for " + TRIM(STRING(ip-date)) + " is already closed, enter security password or enter to return".
        ELSE IF period.subLedgerAP EQ "C" THEN
                lv-msg = "Payables sub ledger period for " + TRIM(STRING(ip-date)) + " is already closed, enter security password or enter to return".    
            ELSE
                lv-msg = "".

    IF lv-msg NE "" AND cAPPostingPassword NE "" THEN 
    DO:
        IF NOT ll-secure THEN 
        DO:
            MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
            RUN sys/ref/dCheckPasswd.w (cAPPostingPassword, OUTPUT ll-secure). 
            IF NOT ll-secure THEN v-invalid-inv = YES .
        END.
    END.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-ap C-Win 
PROCEDURE clear-ap :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    FOR EACH ap-inv
        WHERE ap-inv.company  EQ cocode
        AND ap-inv.posted   EQ NO
        AND ap-inv.user-id  EQ USERID("nosweat")
        /*
        and xap-inv.inv-date ge v-s-date
        and xap-inv.inv-date le v-e-date 
        and xap-inv.vend-no  ge v-s-vend
        and xap-inv.vend-no  le v-e-vend
        AND CAN-FIND(FIRST ap-invl where ap-invl.i-no eq xap-inv.i-no USE-INDEX i-no)
      use-index posted no-lock
      transaction:
      */   :

        IF NOT CAN-FIND(FIRST ap-invl WHERE ap-invl.i-no = ap-inv.i-no) THEN
            DELETE ap-inv.
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
        targetfile = lv-audit-dir + "\AP\VU3\Run#"
                    + STRING(v-trnum) + ".txt"
        dirname1   = lv-audit-dir
        dirname2   = lv-audit-dir + "\AP"
        dirname3   = lv-audit-dir + "\AP\VU3".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-manual-check-proc C-Win 
PROCEDURE create-manual-check-proc :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE X          AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-check-no AS INTEGER NO-UNDO.

    v-check-no = INTEGER(ap-inv.receiver-no).

    IF CAN-FIND(FIRST ap-pay WHERE
        ap-pay.company = cocode AND
        ap-pay.check-no = v-check-no AND
        (ap-pay.check-act = lv-bank-acct OR 
        ap-pay.bank-code = v-bank-code) AND
        ap-pay.posted) THEN
    DO:
        MESSAGE "Check Number " STRING(v-check-no) " has already been posted." SKIP
            "Cannot Create Manual Check."
            VIEW-AS ALERT-BOX ERROR.
        LEAVE.
    END.

    FOR EACH bf-chk NO-LOCK BY bf-chk.c-no DESCENDING:
        X = bf-chk.c-no.
        LEAVE.
    END.

    /* Code placed here will execute AFTER standard behavior.    */

    CREATE ap-chk.
    ASSIGN 
        ap-chk.bank-code  = v-bank-code
        ap-chk.check-no   = v-check-no
        ap-chk.man-check  = YES   
        ap-chk.check-date = ap-inv.inv-date
        ap-chk.c-no       = X + 1
        ap-chk.company    = cocode
        ap-chk.vend-no    = ap-inv.vend-no
        ap-chk.check-amt  = ap-inv.due.

    FIND FIRST bank WHERE
        bank.company = cocode AND
        bank.bank-code = ap-chk.bank-code
        NO-ERROR.

    IF AVAILABLE bank THEN 
    DO:
        IF ap-chk.check-no > bank.last-chk THEN
            bank.last-chk = ap-chk.check-no.

        ap-chk.check-act = bank.actnum.

        RELEASE bank.
    END.

    CREATE ap-sel.
    ASSIGN 
        ap-sel.company   = cocode
        ap-sel.vend-no   = ap-chk.vend-no
        ap-sel.check-no  = ap-chk.check-no
        ap-sel.bank-code = ap-chk.bank-code
        ap-sel.man-check = YES
        ap-sel.pre-date  = ap-chk.check-date
        ap-sel.actnum    = IF lv-bank-acct <> "" THEN lv-bank-acct
                         ELSE "NO Account"
        ap-sel.inv-no    = ap-inv.inv-no
        ap-sel.due-date  = ap-inv.due-date
        ap-sel.inv-bal   = ap-inv.due
        ap-sel.amt-paid  = ap-inv.due.

    IF ap-sel.pre-date - ap-inv.inv-date LE ap-inv.disc-days THEN
        ap-sel.disc-amt = ROUND(ap-inv.disc-% * ap-inv.net / 100,2).

    RELEASE ap-sel.
    RELEASE ap-chk.

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
  DISPLAY tran-date tran-period begin_vend end_vend begin_date end_date 
          begin_user end_user lbl_sort tb_sort rd-dest 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date begin_vend end_vend begin_date end_date 
         begin_user end_user tb_sort rd-dest btn-ok btn-cancel 
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

    DO:
        FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ap-ctrl THEN RETURN.
        ASSIGN 
            xap-acct    = ap-ctrl.payables
            xap-stax    = ap-ctrl.stax
            v-frt-acct  = ap-ctrl.freight
            v-cash-acct = ap-ctrl.cash-act.
        RELEASE ap-ctrl.

        FIND FIRST bank WHERE
            bank.company = g_company AND
            bank.actnum = v-cash-acct
            NO-LOCK NO-ERROR.

        IF AVAILABLE bank THEN
        DO:
            ASSIGN
                v-bank-code  = bank.bank-code
                lv-bank-acct = bank.actnum.
            RELEASE bank.
        END.
    END.

    END_date = TODAY.

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
        IF NOT RESULT THEN v-postable = NO.
      */
    RUN custom/prntproc.p (list-name,(lv-font-no),lv-ornt). /* open file-name, title */ 
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
    RUN scr-rpt-d.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
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
    DEFINE VARIABLE g2        AS DECIMAL.
    DEFINE VARIABLE t1        AS DECIMAL.
    DEFINE VARIABLE v-upd     AS LOGICAL.
    DEFINE VARIABLE v-po-no   LIKE fg-rcpth.po-no.
    DEFINE VARIABLE total-msf LIKE ap-invl.amt-msf.
    DEFINE VARIABLE v-qty     LIKE ap-invl.qty.
    DEFINE VARIABLE v-qty1    LIKE v-qty.
    DEFINE VARIABLE v-qty2    LIKE v-qty.
    DEFINE VARIABLE v-qty3    LIKE v-qty.
    DEFINE VARIABLE v-cost    LIKE fg-rdtlh.cost.
    DEFINE VARIABLE v-wid     AS DECIMAL.
    DEFINE VARIABLE v-len     AS DECIMAL.
    DEFINE VARIABLE v-dep     AS DECIMAL.
    DEFINE VARIABLE v-bwt     AS DECIMAL.
    DEFINE VARIABLE ll-rcpth  AS LOG.

    /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
    g2 = 0.
    FOR EACH tt-report
        WHERE CAN-FIND(FIRST ap-inv WHERE RECID(ap-inv) EQ tt-report.rec-id
        AND ap-inv.posted EQ NO)
        BREAK BY tt-report.actnum
        TRANSACTION:

        FIND FIRST ap-inv
            WHERE RECID(ap-inv) EQ tt-report.rec-id
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

        IF NOT AVAILABLE ap-inv THEN
            UNDO, NEXT.
    
        ASSIGN
            ap-inv.period     = tran-period
            ap-inv.postedDate = tran-date
            ap-inv.runNumber  = v-trnum
            ap-inv.glYear     = YEAR(tran-date)
            ap-inv.glPostdate = tran-date
            .
        total-msf     = 0
            .
    
        FIND FIRST vend
            WHERE vend.company EQ cocode
            AND vend.vend-no EQ ap-inv.vend-no
            USE-INDEX vend NO-LOCK.

        FOR EACH ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no,

            FIRST tt-ap-invl WHERE tt-ap-invl.row-id EQ rowid(ap-invl):  
      
            ASSIGN
                t1             = t1 + ap-invl.amt
                g2             = g2 + ap-invl.amt
                total-msf      = total-msf + ap-invl.amt-msf
                ap-invl.posted = YES.
       
FIND FIRST currency NO-LOCK
      WHERE currency.company     EQ ap-inv.company
        AND currency.c-code      EQ ap-inv.curr-code[1]
        AND currency.ar-ast-acct NE ""
        AND currency.ex-rate     GT 0
      NO-ERROR.
            
      RUN GL_SpCreateGLHist(cocode,
                tt-ap-invl.actnum,
                "ACPAY",
                vend.name  + "  " + string(ap-inv.inv-date),
                tran-date,
                tt-ap-invl.amt,
                v-trnum,
                tran-period,
                "A",
                tran-date,
                (IF AVAIL vend THEN "Vendor:" + STRING(vend.vend-no,"x(8)") ELSE "") + " Inv:" + STRING(ap-inv.inv-no,"99999999") + " PO:" + STRING(ap-invl.po-no,"999999") ,
                "AP").
      RUN GL_SpCreateGLHist(cocode,
                 xap-acct,
                 "ACPAY",
                 vend.name  + "  " + string(ap-inv.inv-date),
                 tran-date,
                 tt-ap-invl.amt * -1 / (IF AVAIL currency THEN currency.ex-rate ELSE 1),
                 v-trnum,
                 tran-period,
                 "A",
                 tran-date,
                 (IF AVAIL vend THEN "Vendor:" + STRING(vend.vend-no,"x(8)") ELSE "") + " Inv:" + STRING(ap-inv.inv-no,"99999999") + " PO:" + STRING(ap-invl.po-no,"999999") ,
                 "AP").                     

            FIND FIRST po-ordl
                WHERE po-ordl.company EQ cocode
                AND po-ordl.po-no   EQ (IF ap-invl.po-no EQ 0 THEN ap-inv.po-no
                ELSE ap-invl.po-no)
                AND po-ordl.line    EQ {ap/invlline.i -1}
                USE-INDEX po-no NO-ERROR.

            IF AVAILABLE po-ordl THEN 
            DO:
                FIND FIRST reftable
                    {ap/apreftbw.i po-ordl.po-no}
                    AND reftable.code2 EQ string(ap-invl.i-no,"9999999999")
                NO-LOCK NO-ERROR.
                IF NOT AVAILABLE reftable THEN 
                DO:
                    {ap/addreftb.i po-ordl.po-no}
                    RELEASE reftable.
                END.

                po-ordl.t-inv-qty = po-ordl.t-inv-qty + ap-invl.qty.

                RELEASE item.
                IF po-ordl.item-type THEN
                    FIND FIRST item
                        WHERE item.company EQ po-ordl.company
                        AND item.i-no    EQ po-ordl.i-no
                        NO-LOCK NO-ERROR.

                IF AVAILABLE item           AND
                    item.i-code EQ "R"   AND
           INDEX("MOXY789@",ITEM.mat-type) GT 0 AND
                    item.stocked EQ NO   THEN 
                DO:

                    ll-rcpth = NO.

                    FOR EACH rm-rcpth NO-LOCK
                        WHERE rm-rcpth.company   EQ po-ordl.company
                        AND rm-rcpth.po-no     EQ STRING(po-ordl.po-no)
                        AND rm-rcpth.po-line   EQ po-ordl.LINE
                        AND rm-rcpth.i-no      EQ po-ordl.i-no
                        AND rm-rcpth.rita-code EQ "R",
                        EACH rm-rdtlh NO-LOCK
                        WHERE rm-rdtlh.r-no    EQ rm-rcpth.r-no
                        AND rm-rdtlh.job-no  EQ po-ordl.job-no
                        AND rm-rdtlh.job-no2 EQ po-ordl.job-no2
                        AND rm-rdtlh.s-num   EQ po-ordl.s-num
                        BREAK BY rm-rcpth.company:

                        IF FIRST(rm-rcpth.company) THEN
                            ASSIGN
                                po-ordl.t-rec-qty = 0
                                ll-rcpth          = YES.

                        v-qty = rm-rdtlh.qty.

                        IF rm-rcpth.pur-uom NE po-ordl.cons-uom THEN
                            RUN sys/ref/convquom.p (rm-rcpth.pur-uom, po-ordl.cons-uom,
                                item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                v-qty, OUTPUT v-qty).

                        IF po-ordl.cons-uom EQ "EA" THEN 
                        DO:
                            {sys/inc/roundup.i v-qty}
                        END.

                        po-ordl.t-rec-qty = po-ordl.t-rec-qty + v-qty.
                    END.

                    IF NOT ll-rcpth THEN 
                    DO:
                        v-dep = item.s-dep.          
                        {po/pol-dims.i}

                        v-qty = ap-invl.qty.

                        IF po-ordl.pr-qty-uom NE po-ordl.cons-uom THEN
                            RUN sys/ref/convquom.p (po-ordl.pr-qty-uom, po-ordl.cons-uom,
                                v-bwt, v-len, v-wid, v-dep,
                                v-qty, OUTPUT v-qty).

                        po-ordl.t-rec-qty = po-ordl.t-rec-qty + v-qty.
                    END.

                    RUN rm/polclose.p (ROWID(po-ordl), ap-invl.qty, po-ordl.pr-qty-uom).
                END.

                RUN po/closechk.p (ROWID(po-ordl)).

                /* Ensure receipts = payables */
                IF NOT po-ordl.item-type AND v-fgpostgl THEN 
                DO:
                    RELEASE prod.
                    FIND FIRST itemfg
                        WHERE itemfg.company EQ cocode
                        AND itemfg.i-no    EQ po-ordl.i-no
                        NO-ERROR.

                    IF AVAILABLE itemfg THEN
                        FIND FIRST prodl
                            WHERE prodl.company EQ cocode
                            AND prodl.procat  EQ itemfg.procat
                            AND CAN-FIND(FIRST prod
                            WHERE prod.company EQ cocode
                            AND prod.prolin  EQ prodl.prolin)
                            NO-LOCK NO-ERROR.

                    IF AVAILABLE prodl THEN
                        FIND FIRST prod
                            WHERE prod.company EQ cocode
                            AND prod.prolin  EQ prodl.prolin
                            NO-LOCK NO-ERROR.

                    IF AVAILABLE itemfg THEN 
                    DO:
                        RUN sys/ref/convquom.p (po-ordl.pr-qty-uom, "EA", 0, 0, 0, 0,
                            ap-invl.qty, OUTPUT v-qty1).

                        ASSIGN
                            v-po-no = TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
                            v-qty   = 0
                            v-cost  = ap-invl.amt / (v-qty1 / 1000).

                        FOR EACH fg-rcpth
                            WHERE fg-rcpth.company   EQ cocode
                            AND fg-rcpth.i-no      EQ po-ordl.i-no
                            AND fg-rcpth.po-no     EQ v-po-no
                            AND fg-rcpth.rita-code EQ "R"
                            AND ((fg-rcpth.b-no    EQ ap-invl.i-no AND v-fgpostgl) OR
                            (fg-rcpth.b-no    EQ 0        AND NOT v-fgpostgl))
                            USE-INDEX item-po,

                            EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no

                            BREAK BY fg-rcpth.trans-date
                            BY fg-rdtlh.trans-time
                            BY fg-rcpth.r-no
                            BY RECID(fg-rdtlh):

                            ASSIGN
                                v-qty         = v-qty + fg-rdtlh.qty
                                fg-rdtlh.cost = v-cost
                                fg-rcpth.b-no = ap-invl.i-no.

                            IF LAST(fg-rcpth.trans-date) AND
                                v-qty NE v-qty1           THEN 
                            DO:

                                /*                find first fg-bin                           */
                                /*                    where fg-bin.company eq cocode          */
                                /*                      and fg-bin.i-no    eq fg-rcpth.i-no   */
                                /*                      and fg-bin.loc     eq fg-rdtlh.loc    */
                                /*                      and fg-bin.loc-bin eq fg-rdtlh.loc-bin*/
                                /*                      and fg-bin.tag     eq fg-rdtlh.tag    */
                                /*                      and fg-bin.job-no  eq fg-rcpth.job-no */
                                /*                      and fg-bin.job-no2 eq fg-rcpth.job-no2*/
                                /*                    no-error.                               */
                                /*                                                            */
                                /*                if not avail fg-bin then do:                */
                                /*                  create fg-bin.                            */
                                /*                  assign                                    */
                                /*                   fg-bin.company      = fg-rdtlh.company   */
                                /*                   fg-bin.job-no       = fg-rcpth.job-no    */
                                /*                   fg-bin.job-no2      = fg-rcpth.job-no2   */
                                /*                   fg-bin.loc          = fg-rdtlh.loc       */
                                /*                   fg-bin.loc-bin      = fg-rdtlh.loc-bin   */
                                /*                   fg-bin.tag          = fg-rdtlh.tag       */
                                /*                   fg-bin.i-no         = fg-rcpth.i-no      */
                                /*                   fg-bin.case-count   = itemfg.case-count  */
                                /*                   fg-bin.cases-unit   = 1                  */
                                /*                   fg-bin.aging-date   = fg-rcpth.trans-date*/
                                /*                   fg-bin.pur-uom      = "M"                */
                                /*                   fg-bin.std-tot-cost = fg-rdtlh.cost      */
                                /*                   fg-bin.std-mat-cost = fg-bin.std-tot-cost*/
                                /*                   fg-bin.std-lab-cost = 0                  */
                                /*                   fg-bin.std-var-cost = 0                  */
                                /*                   fg-bin.std-fix-cost = 0.                 */
                                /*                end.                                        */

                                ASSIGN
                                    v-qty1         = v-qty1 - v-qty
                                    fg-rdtlh.qty   = fg-rdtlh.qty + v-qty1
                                    fg-rdtlh.cases = trunc(fg-rdtlh.qty / fg-rdtlh.qty-case,0)
                                    /*                 fg-bin.qty     = fg-bin.qty + v-qty1*/
                                    itemfg.q-onh   = itemfg.q-onh + v-qty1.

                                RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT fg-rdtlh.loc).
                                FIND FIRST itemfg-loc 
                                    WHERE itemfg-loc.company EQ itemfg.company
                                    AND itemfg-loc.i-no    EQ itemfg.i-no
                                    AND itemfg-loc.loc     EQ fg-rdtlh.loc
                                    EXCLUSIVE-LOCK NO-ERROR.
                                IF AVAILABLE itemfg-loc THEN
                                    itemfg-loc.q-onh = itemfg-loc.q-onh + v-qty1.
                                FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
                            END.
                        END.

                        FOR EACH fg-rcpth
                            WHERE fg-rcpth.company   EQ cocode
                            AND fg-rcpth.i-no      EQ po-ordl.i-no
                            AND fg-rcpth.po-no     EQ v-po-no
                            AND fg-rcpth.rita-code EQ "R"
                            USE-INDEX item-po NO-LOCK,

                            EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no

                            BREAK BY fg-rcpth.job-no
                            BY fg-rcpth.job-no2
                            BY fg-rdtlh.loc
                            BY fg-rdtlh.loc-bin
                            BY fg-rdtlh.tag:

                            IF FIRST-OF(fg-rdtlh.tag) THEN
                                ASSIGN
                                    v-qty  = 0
                                    v-cost = 0.

                            ASSIGN
                                v-qty  = v-qty + fg-rdtlh.qty
                                v-cost = v-cost + (fg-rdtlh.qty / 1000 * fg-rdtlh.cost).

                        /*              if last-of(fg-rdtlh.tag) then do:                               */
                        /*                find first fg-bin                                             */
                        /*                    where fg-bin.company eq cocode                            */
                        /*                      and fg-bin.i-no    eq fg-rcpth.i-no                     */
                        /*                      and fg-bin.loc     eq fg-rdtlh.loc                      */
                        /*                      and fg-bin.loc-bin eq fg-rdtlh.loc-bin                  */
                        /*                      and fg-bin.tag     eq fg-rdtlh.tag                      */
                        /*                      and fg-bin.job-no  eq fg-rcpth.job-no                   */
                        /*                      and fg-bin.job-no2 eq fg-rcpth.job-no2                  */
                        /*                    no-error.                                                 */
                        /*                                                                              */
                        /*                if not avail fg-bin then do:                                  */
                        /*                  create fg-bin.                                              */
                        /*                  assign                                                      */
                        /*                   fg-bin.company      = fg-rdtlh.company                     */
                        /*                   fg-bin.job-no       = fg-rcpth.job-no                      */
                        /*                   fg-bin.job-no2      = fg-rcpth.job-no2                     */
                        /*                   fg-bin.loc          = fg-rdtlh.loc                         */
                        /*                   fg-bin.loc-bin      = fg-rdtlh.loc-bin                     */
                        /*                   fg-bin.tag          = fg-rdtlh.tag                         */
                        /*                   fg-bin.i-no         = fg-rcpth.i-no                        */
                        /*                   fg-bin.case-count   = itemfg.case-count                    */
                        /*                   fg-bin.cases-unit   = 1                                    */
                        /*                   fg-bin.aging-date   = fg-rcpth.trans-date                  */
                        /*                   fg-bin.pur-uom      = "M"                                  */
                        /*                   fg-bin.std-tot-cost = fg-rdtlh.cost                        */
                        /*                   fg-bin.std-mat-cost = fg-bin.std-tot-cost                  */
                        /*                   fg-bin.std-lab-cost = 0                                    */
                        /*                   fg-bin.std-var-cost = 0                                    */
                        /*                   fg-bin.std-fix-cost = 0.                                   */
                        /*                end.                                                          */
                        /*                                                                              */
                        /*                v-cost = v-cost / (v-qty / 1000).                             */
                        /*                                                                              */
                        /*                if fg-bin.pur-uom eq "M" then                                 */
                        /*                  fg-bin.std-tot-cost = v-cost.                               */
                        /*                else                                                          */
                        /*                  run sys/ref/convcuom.p ("M", fg-bin.pur-uom, 0, 0, 0, 0,    */
                        /*                                          v-cost, output fg-bin.std-tot-cost).*/
                        /*                                                                              */
                        /*                assign                                                        */
                        /*                 fg-bin.std-mat-cost = fg-bin.std-tot-cost                    */
                        /*                 fg-bin.std-lab-cost = 0                                      */
                        /*                 fg-bin.std-var-cost = 0                                      */
                        /*                 fg-bin.std-fix-cost = 0.                                     */
                        /*              end.                                                            */
                        END.
                    END.

                /* run fg/updfgcst.p (po-ordl.i-no).  (see #52404) */
                END.
            END.

            IF ap-invl.actnum NE "" THEN
                FIND FIRST bank
                    WHERE bank.company EQ cocode
                    AND bank.actnum  EQ ap-invl.actnum
                    EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE bank THEN bank.bal = bank.bal + ap-invl.amt.
            RELEASE bank.
        END.  /* each line */

        FIND FIRST vend
            WHERE vend.company EQ cocode
            AND vend.vend-no EQ ap-inv.vend-no
            USE-INDEX vend EXCLUSIVE-LOCK.

        ASSIGN
            vend.purch[tran-period]   = vend.purch[tran-period] + t1
            vend.n-purch[tran-period] = vend.n-purch[tran-period] + 1
            vend.purch[13]            = vend.purch[13] + t1
            vend.n-purch[13]          = vend.n-purch[13] + 1
            vend.ptd-msf[tran-period] = vend.ptd-msf[tran-period] + total-msf
            vend.ytd-msf              = vend.ytd-msf + total-msf
            vend.acc-bal              = vend.acc-bal + t1 + ap-inv.tax-amt.

        IF vend.acc-bal GE vend.hibal THEN
            ASSIGN
                vend.hibal      = vend.acc-bal
                vend.hibal-date = ap-inv.inv-date.

        FIND CURRENT vend NO-LOCK NO-ERROR.
        FIND CURRENT po-ordl NO-LOCK NO-ERROR.
        FIND CURRENT itemfg NO-LOCK NO-ERROR.
        /*    FIND CURRENT fg-bin NO-LOCK NO-ERROR.*/
        FIND CURRENT fg-rdtlh NO-LOCK NO-ERROR.
        FIND CURRENT fg-rcpth NO-LOCK NO-ERROR.

        CREATE ap-ledger.
        ASSIGN
            ap-ledger.company  = cocode
            ap-ledger.vend-no  = ap-inv.vend-no
            ap-ledger.amt      = ap-inv.net
            ap-ledger.refnum   = "INV# " + ap-inv.inv-no
            ap-ledger.ref-date = ap-inv.inv-date
            ap-ledger.trnum    = v-trnum
            ap-ledger.period   = tran-period
            ap-ledger.tr-date  = tran-date.

        RELEASE ap-ledger.

        ASSIGN
            t1            = 0
            ap-inv.posted = YES.

        IF apautocheck-log AND ap-inv.receiver-no NE "0" THEN
            RUN create-manual-check-proc.

        ACCUM ap-inv.net (TOTAL BY tt-report.actnum).

        ACCUM tt-report.curr-amt - (ap-inv.net + ap-inv.freight) (TOTAL BY tt-report.actnum).

        ACCUM ap-inv.freight * tt-report.ex-rate (TOTAL).

        IF LAST-OF(tt-report.actnum) AND
            tt-report.actnum NE ""    AND
            (ACCUM TOTAL BY tt-report.actnum tt-report.curr-amt - (ap-inv.net + ap-inv.freight))
            NE 0    THEN 
        DO:      
            RUN GL_SpCreateGLHist(cocode,
                tt-report.actnum,
                "ACPAY",
                "ACCOUNTS PAYABLE CURRENCY GAIN/LOSS",
                tran-date,
                (ACCUM TOTAL BY tt-report.actnum tt-report.curr-amt - (ap-inv.net + ap-inv.freight)) * -1,
                v-trnum,
                tran-period,
                "A",
                tran-date,
                (IF AVAIL vend THEN "Vendor:" + STRING(vend.vend-no,"x(8)") ELSE "") + " Inv:" + STRING(ap-inv.inv-no,"99999999"),
                "AP").    
        END.
    END. /* for each ap-inv */

    g2 = g2 + lv-frt-total.

    DO TRANSACTION:
        IF lv-frt-total NE 0 THEN 
        DO:
            
            RUN GL_SpCreateGLHist(cocode,
                v-frt-acct,
                "ACPAY",
                "ACCOUNTS PAYABLE FREIGHT",
                tran-date,
                (ACCUM TOTAL ap-inv.freight * tt-report.ex-rate),
                v-trnum,
                tran-period,
                "A",
                tran-date,
                (IF AVAIL vend THEN "Vendor:" + STRING(vend.vend-no,"x(8)") ELSE "") + " Inv:" + STRING(ap-inv.inv-no,"99999999"),
                "AP").
            RUN GL_SpCreateGLHist(cocode,
                xap-acct,
                "ACPAY",
                "ACCOUNTS PAYABLE FREIGHT",
                tran-date,
                -(ACCUM TOTAL ap-inv.freight * tt-report.ex-rate),
                v-trnum,
                tran-period,
                "A",
                tran-date,
                (IF AVAIL vend THEN "Vendor:" + STRING(vend.vend-no,"x(8)") ELSE "") + " Inv:" + STRING(ap-inv.inv-no,"99999999"),
                "AP").
        END.

        FOR EACH tt-ap-tax BREAK BY tt-ap-tax.actnum:
            ACCUM tt-ap-tax.curr-amt (TOTAL BY tt-ap-tax.actnum).

            g2 = g2 + tt-ap-tax.amt.

            IF LAST-OF(tt-ap-tax.actnum) THEN 
            DO:
                
                RUN GL_SpCreateGLHist(cocode,
                    tt-ap-tax.actnum,
                    "ACPAY",
                    "ACCOUNTS PAYABLE TAX",
                    tran-date,
                    (ACCUM TOTAL BY tt-ap-tax.actnum tt-ap-tax.curr-amt),
                    v-trnum,
                    tran-period,
                    "A",
                    tran-date,
                    tt-ap-tax.cDescription,
                    "AP"). 
                RUN GL_SpCreateGLHist(cocode,
                    xap-acct,
                    "ACPAY",
                    "ACCOUNTS PAYABLE TAX",
                    tran-date,
                    -(ACCUM TOTAL BY tt-ap-tax.actnum tt-ap-tax.curr-amt),
                    v-trnum,
                    tran-period,
                    "A",
                    tran-date,
                    tt-ap-tax.cDescription,
                    "AP").                
            END.
        END. 
          
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- ap/ap-inreg.p 10/94 gb */
    /* Invoicing  - Edit Register & Post Invoicing TRANSACTIONs                   */
    /* -------------------------------------------------------------------------- */
    DEFINE VARIABLE g1         AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE g2         LIKE g1 NO-UNDO.
    DEFINE VARIABLE t1         LIKE g1 NO-UNDO.
    DEFINE VARIABLE t2         LIKE g1 NO-UNDO.
    DEFINE VARIABLE t3         LIKE g1 NO-UNDO.
    DEFINE VARIABLE v1         LIKE g1 NO-UNDO.
    DEFINE VARIABLE v2         LIKE g1 NO-UNDO.

    DEFINE VARIABLE total-msf  LIKE ap-invl.amt-msf NO-UNDO.
    DEFINE VARIABLE v-s-date   LIKE inv-head.inv-date FORMAT "99/99/9999" INIT 01/01/0001 NO-UNDO.
    DEFINE VARIABLE v-e-date   LIKE v-s-date INIT TODAY NO-UNDO.
    DEFINE VARIABLE v-prt-dscr AS LOG     INIT NO NO-UNDO.
    DEFINE VARIABLE v-s-vend   LIKE vend.vend-no INITIAL "First" NO-UNDO.
    DEFINE VARIABLE v-e-vend   LIKE vend.vend-no INITIAL "Last" NO-UNDO.
    DEFINE BUFFER xap-inv FOR ap-inv.
    DEFINE VARIABLE v-loop        AS INTEGER INIT 1 NO-UNDO.
    DEFINE VARIABLE v-upd         AS LOG     NO-UNDO.
    DEFINE VARIABLE v-po-no       LIKE fg-rcpth.po-no NO-UNDO.
    DEFINE VARIABLE v-dscr        LIKE account.dscr.
    DEFINE VARIABLE v-disp-actnum LIKE account.actnum.
    DEFINE VARIABLE v-disp-amt    AS DECIMAL FORMAT ">>,>>>,>>9.99cr".
    DEFINE VARIABLE ld-gl-amt     AS DECIMAL NO-UNDO.

    {sys/form/r-top3w.f}
    time_stamp = STRING(TIME,"hh:mmam").

    FORM HEADER
        "VENDOR#  Name                              INVOICE #       INV.DATE    DUE DATE         AMOUNT " 
        "    G/L DISTRIBUTION" SKIP FILL("_",136) FORMAT "x(136)"
        WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top PAGE-TOP WIDTH 136 STREAM-IO.
    
    FORM HEADER
        "VENDOR#  Name                              INVOICE #               INV.DATE    DUE DATE         AMOUNT " 
        "    G/L DISTRIBUTION" SKIP FILL("_",144) FORMAT "x(145)"
        WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top3 PAGE-TOP WIDTH 145 STREAM-IO.

    FORM v-disp-actnum LABEL "G/L ACCOUNT NUMBER"
        v-dscr        LABEL "DESCRIPTION"
        udate         LABEL "DATE"   
        v-disp-amt    LABEL "AMOUNT" SKIP

        WITH DOWN STREAM-IO WIDTH 130 FRAME gldetail.


    SESSION:SET-WAIT-STATE ("general").

    tmpstore = FILL("_",125).

    ASSIGN 
        v-s-vend   = begin_vend
        v-e-vend   = END_vend
        v-s-date   = begin_date
        v-e-date   = END_date
        v-prt-dscr = tb_sort.

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    ASSIGN
        g1        = 0
        g2        = 0
        t1        = 0
        t2        = 0
        t3        = 0
        v1        = 0
        v2        = 0
        total-msf = 0.

    ASSIGN
        str-tit  = coname + " - " + loname
        str-tit2 = "VENDOR INVOICES  -  EDIT REGISTER " + string(v-trnum)
        str-tit3 = "Period " + string(tran-period,"99") +
            " - TRANSACTION Date Entered: " + string(tran-date)
        x        = (112 - length(str-tit)) / 2
        str-tit  = FILL(" ",x) + str-tit
        x        = (114 - length(str-tit2)) / 2
        str-tit2 = FILL(" ",x) + str-tit2
        x        = (132 - length(str-tit3)) / 2
        str-tit3 = FILL(" ",x) + str-tit3.

    EMPTY TEMP-TABLE tt-report.

    EMPTY TEMP-TABLE tt-ap-invl.

    EMPTY TEMP-TABLE tt-ap-tax.

    DISPLAY "" WITH FRAME r-top.

    IF lAPInvoiceLength THEN
        DISPLAY "" WITH FRAME f-top3.
    ELSE
        DISPLAY "" WITH FRAME f-top.

    {ap/r-apve&p.i}

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE  NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS cha     NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS cha     NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER NO-UNDO.
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
            IF gl-ctrl.trnum = v-trnum THEN gl-ctrl.trnum = v-trnum - 1.
            RELEASE gl-ctrl.
            LEAVE.
        END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
/* gdm - 11050906 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-date C-Win 
PROCEDURE valid-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR ll AS LOG NO-UNDO.
    DEFINE BUFFER bf-period FOR period.

    DO WITH FRAME {&FRAME-NAME}:
        IF NOT ll-warned THEN DO:
            ll = NO.

            FOR EACH period NO-LOCK
                WHERE period.company EQ cocode
                AND period.pst     LE TODAY
                AND period.pend    GE TODAY
                BY period.pst:

                IF /* iplCheckDate 
                AND */ (period.pst  GT DATE(tran-date:SCREEN-VALUE) OR
                    period.pend LT DATE(tran-date:SCREEN-VALUE)) THEN DO:
                    ll = YES.
                    MESSAGE TRIM(tran-date:LABEL) + " is not in current period, " +
                        "would you like to re-enter..."
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE ll.
                    IF ll THEN DO:
                        ASSIGN 
                            v-invalid = TRUE.
                        RETURN.
                    END.
                END.
            END.        
        
            FIND FIRST bf-period NO-LOCK
                WHERE bf-period.company EQ cocode
                AND bf-period.pst     LE DATE(tran-date:SCREEN-VALUE)
                AND bf-period.pend    GE DATE(tran-date:SCREEN-VALUE)
            NO-ERROR.
        
            IF AVAIL bf-period THEN
            ASSIGN
                begin_date:SCREEN-VALUE = STRING(bf-period.pst)
                end_date:SCREEN-VALUE = STRING(bf-period.pend).

            ll-warned = YES.
        END.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckUser C-Win 
PROCEDURE pCheckUser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-period LIKE period.pnum NO-UNDO.
    DEFINE VARIABLE lv-msg  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE v-month AS CHARACTER . 
    ASSIGN  
        lv-msg        = ""
        v-invalid-inv = NO 
       .
    DO WITH FRAME {&FRAME-NAME}:   
        IF cAPPostingUserId EQ "Yes" THEN
        DO:
            IF begin_user:SCREEN-VALUE NE USERID(LDBNAME(1)) OR end_user:SCREEN-VALUE NE USERID(LDBNAME(1)) THEN
            DO:
                lv-msg = "CAN NOT POST OUT OF USER, ENTER SECURITY PASSWORD OR  ENTER TO RETURN".
            END.             
        END.
    END.

    IF lv-msg NE "" AND cAPPostingPassword NE "" THEN 
    DO:
        IF NOT ll-secure THEN          
        DO:
            MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
            RUN sys/ref/dCheckPasswd.w (cAPPostingPassword, OUTPUT ll-secure). 
            IF NOT ll-secure THEN v-invalid-inv = YES .
        END.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

