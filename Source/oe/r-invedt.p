&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oe\r-invedt.p

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
DEFINE VARIABLE v-sman-found AS LOG       NO-UNDO.
DEFINE VARIABLE v-prg-name   AS CHARACTER INIT "r-invedt.r" NO-UNDO.
DEFINE VARIABLE cFileName    AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE TEMP-TABLE work-rel NO-UNDO
    FIELD company    LIKE oe-relh.company
    FIELD loc        LIKE oe-rell.loc
    FIELD r-no       LIKE oe-relh.r-no
    FIELD bol-no     LIKE oe-bolh.bol-no
    FIELD carrier    LIKE oe-relh.carrier
    FIELD cust-no    LIKE oe-relh.cust-no
    FIELD ord-no     LIKE oe-relh.ord-no
    FIELD po-no      LIKE oe-relh.po-no
    FIELD rel-date   AS CHARACTER FORMAT "99/99/99"
    FIELD ship-id    LIKE oe-relh.ship-id
    FIELD ship-i     LIKE oe-relh.ship-i
    FIELD i-no       LIKE oe-rell.i-no
    FIELD line       LIKE oe-rell.line
    FIELD qty        LIKE oe-rell.qty
    FIELD tot-qty    LIKE oe-rell.qty
    FIELD posted     LIKE oe-rell.posted
    FIELD printed    LIKE oe-relh.printed
    FIELD ship-addr  AS CHARACTER FORMAT "x(20)"
    FIELD ship-city  AS CHARACTER FORMAT "x(10)"
    FIELD ship-state AS CHARACTER FORMAT "x(2)"
    FIELD ship-zip   AS CHARACTER FORMAT "x(10)"
    FIELD completed  AS CHARACTER FORMAT "x(1)".

DEFINE TEMP-TABLE work-rel-copy NO-UNDO LIKE work-rel.

DEFINE VARIABLE is-xprint-form AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.

/* gdm - 10130810 */
DEFINE STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 tb_detailed tb_printed ~
tb_unprinted tb_cost rd_sort begin_date end_date begin_slsmn end_slsmn ~
rd-dest tb_batch fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_detailed tb_printed tb_unprinted ~
tb_cost lbl_sort rd_sort begin_date end_date begin_slsmn end_slsmn rd-dest ~
tb_batch fi_file tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_slsmn    AS CHARACTER FORMAT "X(5)":U 
    LABEL "From Sales Rep" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slsmn      AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
    LABEL "To Sales Rep" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)":U INITIAL "c:~\tmp~\InvoiceEdit.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_sort       AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
    VIEW-AS FILL-IN 
    SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 55 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=9 (13CPI)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "12" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt        AS CHARACTER INITIAL "L" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 16.4 BY 5 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "Customer" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Customer", "Customer",
    "BOL", "BOL"
    SIZE 24 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 7.86.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.52.

DEFINE VARIABLE tbAutoClose  AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_batch     AS LOGICAL INITIAL NO 
    LABEL "Run In Batch Mode?" 
    VIEW-AS TOGGLE-BOX
    SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cost      AS LOGICAL INITIAL YES 
    LABEL "Print Cost / Margin%?" 
    VIEW-AS TOGGLE-BOX
    SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE tb_detailed  AS LOGICAL INITIAL NO 
    LABEL "Detailed?" 
    VIEW-AS TOGGLE-BOX
    SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL INITIAL NO 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_printed   AS LOGICAL INITIAL YES 
    LABEL "Show Printed Invoices?" 
    VIEW-AS TOGGLE-BOX
    SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.8 BY .81 NO-UNDO.

DEFINE VARIABLE tb_unprinted AS LOGICAL INITIAL YES 
    LABEL "Show Unprinted Invoices?" 
    VIEW-AS TOGGLE-BOX
    SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tb_detailed AT ROW 1.91 COL 31
    tb_printed AT ROW 2.81 COL 31
    tb_unprinted AT ROW 3.67 COL 31
    tb_cost AT ROW 4.52 COL 31
    lbl_sort AT ROW 5.62 COL 28 COLON-ALIGNED NO-LABELS
    rd_sort AT ROW 5.62 COL 40 NO-LABELS
    begin_date AT ROW 6.81 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 6.81 COL 64.8 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_slsmn AT ROW 7.81 COL 28 COLON-ALIGNED HELP
    "Enter From Sales Rep" WIDGET-ID 8
    end_slsmn AT ROW 7.81 COL 64.8 COLON-ALIGNED HELP
    "Enter To Sales Rep" WIDGET-ID 10
    lv-font-no AT ROW 10 COL 33 COLON-ALIGNED
    lines-per-page AT ROW 10 COL 86.8 COLON-ALIGNED
    rd-dest AT ROW 10.05 COL 4.6 NO-LABELS
    lv-ornt AT ROW 10.05 COL 43.6 NO-LABELS
    lv-font-name AT ROW 11.05 COL 28.6 COLON-ALIGNED NO-LABELS
    tb_batch AT ROW 12.05 COL 28.4 WIDGET-ID 12
    tb_excel AT ROW 12.19 COL 71 WIDGET-ID 4
    td-show-parm AT ROW 12.95 COL 28.4
    fi_file AT ROW 13.86 COL 26.4 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 2
    tb_OpenCSV AT ROW 13.95 COL 77.8 WIDGET-ID 6
    tbAutoClose AT ROW 15.29 COL 28.2 WIDGET-ID 64
    btn-ok AT ROW 16.24 COL 28
    btn-cancel AT ROW 16.24 COL 51
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 9.33 COL 4
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.1 COL 4
    RECT-7 AT ROW 1.48 COL 3
    RECT-8 AT ROW 9.76 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96.4 BY 21.81
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
        TITLE              = "Invoice Edit Listing"
        HEIGHT             = 16.76
        WIDTH              = 95
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
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lines-per-page:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-font-name:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-font-no:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-ornt:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_cost:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_detailed:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_printed:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_unprinted:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Invoice Edit Listing */
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
ON WINDOW-CLOSE OF C-Win /* Invoice Edit Listing */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* From Sales Rep */
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
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
  
        IF rd-dest = 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.

        IF g_batch THEN tb_batch = YES.
        IF tb_batch THEN 
        DO:
            RUN run-batch.
            RETURN NO-APPLY.
        END.

        RUN run-report. 

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN 
                DO:
                    IF NOT tb_OpenCSV THEN 
                    DO:        
                        MESSAGE "CSV file have been created." SKIP(1)
                            "~"OK"~" to open CSV file?"
                            VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                            TITLE "" UPDATE lChoice AS LOGICAL.
                 
                        IF lChoice THEN
                        DO:
                            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                        END.
                    END.
                END. /* WHEN 3 THEN DO: */
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &begin_cust=rd_sort
                            &END_cust=lbl_sort
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END.
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail.i &TYPE = ''
                             &begin_cust= lbl_sort
                             &END_cust=rd_sort
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:

                        {custom/asimailr2.i &TYPE = "SalesRep"
                                 &group-title= v-prg-name
                                 &begin_cust= begin_slsmn
                                 &END_cust= end_slsmn
                                 &mail-subject=c-win:title
                                 &mail-body=c-win:title
                                 &mail-file=list-name }
                    END.
 
                END. 
            WHEN 6 THEN RUN output-to-port.
        END CASE.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* To Sales Rep */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* Name */
    DO:
        DEFINE VARIABLE ls-filename AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ll-ok       AS LOG       NO-UNDO.

        SYSTEM-DIALOG GET-FILE ls-filename 
            TITLE "Select File to Save "
            FILTERS "Excel Files    (*.csv)" "*.csv",
            "All Files    (*.*) " "*.*"
            INITIAL-DIR "c:\tmp"
            MUST-EXIST
            USE-FILENAME
            UPDATE ll-ok.

        IF ll-ok THEN SELF:SCREEN-VALUE = ls-filename.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Name */
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
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
        RUN pChangeDest.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cost C-Win
ON VALUE-CHANGED OF tb_cost IN FRAME FRAME-A /* Print Cost / Margin%? */
    DO:
        IF {&self-name}:SCREEN-VALUE EQ "Yes" THEN lv-ornt:SCREEN-VALUE = "L".
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_printed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_printed C-Win
ON VALUE-CHANGED OF tb_printed IN FRAME FRAME-A /* Show Printed Invoices? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_unprinted
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_unprinted C-Win
ON VALUE-CHANGED OF tb_unprinted IN FRAME FRAME-A /* Show Unprinted Invoices? */
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

    IF g_batch THEN tb_batch = YES.

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "OB2" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
  
    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        APPLY "entry" TO tb_detailed.
    END.
    RUN pChangeDest.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
    DISPLAY tb_detailed tb_printed tb_unprinted tb_cost lbl_sort rd_sort 
        begin_date end_date begin_slsmn end_slsmn rd-dest tb_batch fi_file 
        tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-7 RECT-8 tb_detailed tb_printed tb_unprinted tb_cost rd_sort 
        begin_date end_date begin_slsmn end_slsmn rd-dest tb_batch fi_file 
        tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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
    /*    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
             
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
    RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-batch C-Win 
PROCEDURE run-batch :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    {BATCH/runbatch.i "oe\s-invedt.r"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ------------------------------------------------- oe/rep/newinv.p  3/94 RM */
    /* Invoicing  - new Invoice tt-report                                            */
    /* -------------------------------------------------------------------------- */

    /*also modify oe\s-invedt.p*/

    {sys/form/r-topw2.f}

    DEFINE VARIABLE v-detail       AS LOG       FORMAT "Detail/Summary" INIT NO NO-UNDO.
    DEFINE VARIABLE v-sort         AS LOG       FORMAT "Customer/BOL" INIT YES NO-UNDO.

    DEFINE VARIABLE save_id        AS RECID     NO-UNDO.
    DEFINE VARIABLE time_stamp     AS ch        NO-UNDO.
    DEFINE VARIABLE qfirst         AS l         NO-UNDO.
    DEFINE VARIABLE v-trnum        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-ext-price    LIKE inv-line.t-price NO-UNDO.
    DEFINE VARIABLE v-tot-cas      AS DECIMAL   FORMAT "->>>9.9999" NO-UNDO.
    DEFINE VARIABLE v-cases        AS DECIMAL   FORMAT "->>>9.9999" NO-UNDO.
    DEFINE VARIABLE v-tot-pallets  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-tax-rate     AS DECIMAL   FORMAT ">,>>9.99<<<" NO-UNDO.
    DEFINE VARIABLE v-fr-tax       AS LOG       INIT NO NO-UNDO.
    DEFINE VARIABLE v-postable     AS LOG       INIT NO NO-UNDO.
    DEFINE VARIABLE v-tot-cost     LIKE inv-head.t-inv-cost NO-UNDO.     /* total cost invoiced */
    DEFINE VARIABLE v-tot-weight   LIKE inv-head.t-inv-weight NO-UNDO.  /* total weight shipped */
    DEFINE VARIABLE v-line-price   LIKE inv-line.price NO-UNDO.
    DEFINE VARIABLE v-line-cost    LIKE inv-line.t-price NO-UNDO.
    DEFINE VARIABLE v-line-freight LIKE inv-line.t-freight NO-UNDO.
    DEFINE VARIABLE v-set-qty      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-part-qty     AS DECIMAL   FORMAT "999.9999" NO-UNDO.
    DEFINE VARIABLE v-bol-cases    LIKE oe-boll.cases NO-UNDO.
    DEFINE VARIABLE v-line-tot     LIKE inv-line.t-price NO-UNDO.
    DEFINE VARIABLE v-misc-tot     LIKE inv-misc.amt NO-UNDO.
    DEFINE VARIABLE v-tmp          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-margin      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-total-p     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-total-c     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-dash        AS CHARACTER FORMAT "x" INIT "-" NO-UNDO.
    DEFINE VARIABLE dfreight       LIKE inv-head.t-inv-freight NO-UNDO.
    DEFINE VARIABLE cfreightCode   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemFgCat     LIKE itemfg.procat NO-UNDO.

    /* gdm - 10130810 */
    DEFINE VARIABLE v_misc-amt     AS CHARACTER NO-UNDO.

    DEFINE BUFFER xinv-line FOR inv-line.

    FORM HEADER
        " Customer"
        "Weight" TO 47 "Pallets" TO 58 "Cases" TO 65 "Freight Terms" TO 80 "Freight" TO 96
        "Tax" TO 106 "Misc" TO 120 "Items" TO 135 " Total" TO 160 SKIP
        FILL("=",160) FORMAT "x(160)"
        WITH FRAME r-top.

    FORM
        inv-head.cust-no lv-dash
        inv-head.cust-name FORMAT "x(25)"
        inv-head.t-inv-weight v-tot-pallets v-tot-cas FORMAT "->>>>9"
        cfreightCode TO 80 FORMAT "x(10)"
        dfreight TO 96 FORMAT "->,>>9.99"
        inv-head.t-inv-tax TO 106 FORMAT "->>,>>9.99"
        v-misc-tot TO 120 FORMAT "->>>,>>9.99"
        v-line-tot TO 135
        inv-head.t-inv-rev FORMAT "->>,>>>,>>9.99" TO 160
        WITH DOWN STREAM-IO WIDTH 180 NO-LABELS NO-BOX NO-UNDERLINE FRAME ord.

    FORM
        inv-head.cust-no lv-dash
        inv-head.cust-name FORMAT "x(25)"
        inv-head.t-inv-weight v-tot-pallets v-tot-cas FORMAT "->>>>9"
        cfreightCode TO 80 FORMAT "x(10)"
        dfreight TO 96 FORMAT "->,>>9.99"
        inv-head.t-inv-tax TO 106 FORMAT "->>,>>9.99"
        v-misc-tot TO 120 FORMAT "->>>,>>9.99"
        /*v-line-tot */
        ld-total-c FORMAT "->>,>>>,>>9.99" TO 135
        inv-head.t-inv-rev FORMAT "->>,>>>,>>9.99" TO 160
        WITH DOWN STREAM-IO WIDTH 180 NO-LABELS NO-BOX NO-UNDERLINE FRAME ord-c.

    FORM
        inv-line.ord-no AT 5 LABEL "Order#"
        oe-ordl.po-no LABEL "Order PO Number"
        inv-line.i-no LABEL "Item"
        inv-line.i-name FORMAT "x(20)" LABEL "Description"
        inv-line.qty FORMAT "->>,>>>,>>9" LABEL "Order"
        inv-line.inv-qty FORMAT "->>,>>>,>>9" COLUMN-LABEL "Quantities!Invoiced "
        inv-line.ship-qty FORMAT "->>,>>>,>>9" LABEL "Shipped"
        inv-line.price FORMAT "->>>,>>9.99" LABEL "Price"
        inv-line.pr-uom LABEL "UOM"
        inv-line.t-price FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Extended! Price" TO 140 SKIP
        WITH DOWN NO-BOX STREAM-IO WIDTH 140 FRAME ordl.

    FORM
        inv-line.ord-no AT 5 LABEL "Order#"
        oe-ordl.po-no LABEL "Order PO Number"
        inv-line.i-no LABEL "Item"
        inv-line.i-name FORMAT "x(20)" LABEL "Description"
        inv-line.qty FORMAT "->>,>>>,>>9" LABEL "Order"
        inv-line.inv-qty FORMAT "->>,>>>,>>9" COLUMN-LABEL "Quantities!Invoiced "
        inv-line.ship-qty FORMAT "->>,>>>,>>9" LABEL "Shipped"
        inv-line.price FORMAT "->>>,>>9.99" LABEL "Price"
        inv-line.pr-uom LABEL "UOM"
        v-line-cost FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Extended!  Cost" TO 132 
        inv-line.t-price FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Extended! Price"
        ld-margin FORMAT "->>,>>9.99" COLUMN-LABEL "!Margin%"
        SKIP
        WITH DOWN NO-BOX STREAM-IO WIDTH 180 FRAME ordl-c.

    FORM
        inv-misc.charge AT 10 LABEL "Charge"
        inv-misc.dscr LABEL "Description"
        inv-misc.po-no LABEL "Customer PO#" FORMAT "x(30)"
        inv-misc.amt FORMAT "->>,>>>,>>9.99" TO 132 LABEL "Price"
        SKIP
        WITH DOWN STREAM-IO WIDTH 132 NO-BOX FRAME ordm.

    FORM
        inv-misc.charge AT 10 LABEL "Charge"
        inv-misc.dscr LABEL "Description"
        inv-misc.po-no LABEL "Customer PO#" FORMAT "x(30)"
        inv-misc.cost FORMAT "->>,>>>,>>9.99" TO 132 LABEL "Cost"
        inv-misc.amt FORMAT "->>,>>>,>>9.99" LABEL "Price"
        ld-margin FORMAT "->>,>>9.99" COLUMN-LABEL "!Margin%"
        SKIP
        WITH DOWN STREAM-IO WIDTH 180 NO-BOX FRAME ordm-c.

    FORM
        work-rel.i-no AT 10 COLUMN-LABEL "RELEASE!Items"
        work-rel.po-no LABEL "PO Number"
        work-rel.loc LABEL "Location"
        work-rel.rel-date LABEL "Date"
        work-rel.bol-no LABEL "BOL#"
        work-rel.completed COLUMN-LABEL "P/C"
        work-rel.r-no   LABEL "REL#"
        work-rel.carrier LABEL "Carrier"
        work-rel.ship-id LABEL "Ship To"
        work-rel.qty   LABEL "Quantity" SKIP
        WITH DOWN NO-BOX STREAM-IO WIDTH 132 FRAME rel.

    
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}
    
        v-fr-tax = oe-ctrl.f-tax  /** if fREIGHT IS TAXABLE **/

        v-detail = tb_detailed
        v-sort   = rd_sort BEGINS "Cust".

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    /* gdm - 10130810 */
    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        PUT STREAM excel UNFORMATTED 
            'Customer,Weight,Pallets,Cases,Freight Terms,Freight,Tax,Misc,Items,Total,'.   

        IF v-detail THEN 
        DO:
        
            IF tb_cost THEN 
            DO:
                PUT STREAM excel UNFORMATTED 
                    'Order#,Order PO Number,Item,Description,FG Category,Order,Quantities Invoiced,Shipped,Price,UOM,Extended Cost,Extended Price, Margin%,'
                    + 'Charge,Description,Customer PO#,Cost,Price,Margin%,'.
            END.
            ELSE 
            DO:
                PUT STREAM excel UNFORMATTED 
                    'Order#,Order PO Number,Item,Description,FG Category,Order,Quantities Invoiced,Shipped,Price,UOM,Extended Price,'
                    + 'Charge,Description,Customer PO#,Price,'.
            END.

            PUT STREAM excel UNFORMATTED 
                'RELEASE Items,PO Number,Location,Date,BOL#,P/C,REL#,Carrier,Ship To,Quantity,CSR,Line Sales TaxGroup,Misc Item SalesTax Group,OrderHeader ShipTo State,Order Line No,Billing Note'
                SKIP.

        END.
        ELSE 
            PUT STREAM excel UNFORMATTED SKIP.

    END. 


    IF td-show-parm THEN RUN show-param.

    DISPLAY "" WITH FRAME r-top.

    SESSION:SET-WAIT-STATE ("general").

    {oe/r-invedt.i}

    /* gdm - 10130810 */
    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2002 Advanced Software, Inc. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pChangeDest C-Win 
PROCEDURE pChangeDest :
    /*------------------------------------------------------------------------------
         Purpose:    
         Parameters:  <none>
         Notes:      
        ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF rd-dest:SCREEN-VALUE EQ "3" THEN
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "Yes"
                fi_file:SENSITIVE       = YES
                tb_OpenCSV:SENSITIVE    = YES
                tb_excel                = YES
                .
        ELSE
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "NO"
                fi_file:SENSITIVE       = NO
                tb_OpenCSV:SENSITIVE    = NO
                tb_excel                = NO
                .
        ASSIGN 
            fi_file:SCREEN-VALUE = "c:\tmp\InvoiceEdit.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
