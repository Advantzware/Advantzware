&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

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
DEFINE VARIABLE list-name AS cha       NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

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

DEFINE VARIABLE v-program      AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-pdf-file    AS cha       NO-UNDO.

{custom/xprint.i}

DEFINE VARIABLE lv-prt-bypass AS LOG NO-UNDO.  /* bypass window's printer driver */

DEFINE TEMP-TABLE w-bol 
    FIELD rec-id AS RECID 
    INDEX rec-id rec-id.

DEFINE STREAM last-page.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_cust begin_ship v-bol-list ~
begin_bol#-1 begin_bol#-2 begin_bol#-3 begin_bol#-4 begin_bol#-5 ~
begin_bol#-6 begin_bol#-7 begin_bol#-8 begin_bol#-9 begin_bol#-10 ~
begin_bol#-11 begin_bol#-12 begin_bol#-13 begin_bol#-14 begin_bol#-15 ~
begin_bol#-16 begin_bol#-17 begin_bol#-18 begin_bol#-19 begin_bol#-20 ~
begin_bol#-21 begin_bol#-22 begin_bol#-23 begin_bol#-24 rd-dest btn-ok ~
btn-cancel tbAutoClose RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS begin_cust begin_ship v-bol-list ~
begin_bol#-1 begin_bol#-2 begin_bol#-3 begin_bol#-4 begin_bol#-5 ~
begin_bol#-6 begin_bol#-7 begin_bol#-8 begin_bol#-9 begin_bol#-10 ~
begin_bol#-11 begin_bol#-12 begin_bol#-13 begin_bol#-14 begin_bol#-15 ~
begin_bol#-16 begin_bol#-17 begin_bol#-18 begin_bol#-19 begin_bol#-20 ~
begin_bol#-21 begin_bol#-22 begin_bol#-23 begin_bol#-24 rd-dest tbAutoClose 

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

DEFINE VARIABLE v-bol-list     AS CHARACTER 
    VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
    SIZE 49 BY 6.91 NO-UNDO.

DEFINE VARIABLE begin_bol#-1   AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-10  AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-11  AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-12  AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-13  AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-14  AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-15  AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-16  AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-17  AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-18  AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95.

DEFINE VARIABLE begin_bol#-19  AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-2   AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-20  AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-21  AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-22  AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-23  AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-24  AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-3   AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-4   AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-5   AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-6   AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-7   AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-8   AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-9   AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_cust     AS CHARACTER FORMAT "X(8)" 
    LABEL "Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_ship     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Ship-To#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt        AS CHARACTER INITIAL "P" 
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
    "To File", 3
    SIZE 17.4 BY 4.76 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.29.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 13.05.

DEFINE VARIABLE tbAutoClose  AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_cust AT ROW 3.38 COL 21.2 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    begin_ship AT ROW 4.33 COL 21.2 COLON-ALIGNED HELP
    "Enter Beginning Ship-to#"
    v-bol-list AT ROW 6.95 COL 4.2 NO-LABELS
    begin_bol#-1 AT ROW 2.67 COL 53.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-2 AT ROW 2.67 COL 73.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-3 AT ROW 3.62 COL 53.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-4 AT ROW 3.62 COL 73.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-5 AT ROW 4.57 COL 53.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-6 AT ROW 4.57 COL 73.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-7 AT ROW 5.52 COL 53.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-8 AT ROW 5.52 COL 73.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-9 AT ROW 6.48 COL 53.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-10 AT ROW 6.48 COL 73.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-11 AT ROW 7.43 COL 53.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-12 AT ROW 7.43 COL 73.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-13 AT ROW 8.38 COL 53.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-14 AT ROW 8.38 COL 73.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-15 AT ROW 9.33 COL 53.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-16 AT ROW 9.33 COL 73.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-17 AT ROW 10.29 COL 53.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-18 AT ROW 10.29 COL 73.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-19 AT ROW 11.24 COL 53.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-20 AT ROW 11.24 COL 73.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-21 AT ROW 12.19 COL 53.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-22 AT ROW 12.19 COL 73.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-23 AT ROW 13.14 COL 53.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    begin_bol#-24 AT ROW 13.14 COL 73.2 COLON-ALIGNED HELP
    "Enter Beginning BOL Number" NO-LABELS
    rd-dest AT ROW 15.29 COL 4.6 NO-LABELS
    lv-ornt AT ROW 15.29 COL 44 NO-LABELS
    lines-per-page AT ROW 15.19 COL 87 COLON-ALIGNED
    lv-font-no AT ROW 15.29 COL 34 COLON-ALIGNED
    lv-font-name AT ROW 16.43 COL 29 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 19.1 COL 31.6
    btn-ok AT ROW 21.29 COL 31.2
    btn-cancel AT ROW 21.29 COL 54.4
    tbAutoClose AT ROW 20.38 COL 31.4 WIDGET-ID 64
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 94.6 BY 25.29
    BGCOLOR 15 .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 14.57 COL 4
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.1 COL 4
    "Beginning BOL#" VIEW-AS TEXT
    SIZE 17 BY .76 AT ROW 1.71 COL 55.2
    "Ending BOL#" VIEW-AS TEXT
    SIZE 17 BY .76 AT ROW 1.71 COL 75.2
    "BOL# Ranges -->" VIEW-AS TEXT
    SIZE 18 BY .76 AT ROW 1.71 COL 36.2
    " Enter BOL#s separated by comma" VIEW-AS TEXT
    SIZE 36 BY .62 AT ROW 6.24 COL 9.2
    BGCOLOR 8 
    RECT-6 AT ROW 15 COL 3
    RECT-7 AT ROW 1.52 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 94.6 BY 25.29
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
        TITLE              = "Truckload BOL Summary"
        HEIGHT             = 21.76
        WIDTH              = 94.8
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
   FRAME-NAME Custom                                                    */
ASSIGN 
    begin_bol#-1:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-10:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-11:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-12:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-13:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-14:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-15:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-16:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-17:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-18:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-19:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-20:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-21:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-22:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-23:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-24:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-3:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-4:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-5:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-6:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-7:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-8:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_bol#-9:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_ship:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

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

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    v-bol-list:RETURN-INSERTED IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Truckload BOL Summary */
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
ON WINDOW-CLOSE OF C-Win /* Truckload BOL Summary */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-1 C-Win
ON LEAVE OF begin_bol#-1 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-10 C-Win
ON LEAVE OF begin_bol#-10 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-11 C-Win
ON LEAVE OF begin_bol#-11 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-12 C-Win
ON LEAVE OF begin_bol#-12 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-13 C-Win
ON LEAVE OF begin_bol#-13 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-14 C-Win
ON LEAVE OF begin_bol#-14 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-15 C-Win
ON LEAVE OF begin_bol#-15 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-16 C-Win
ON LEAVE OF begin_bol#-16 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-17 C-Win
ON LEAVE OF begin_bol#-17 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-18 C-Win
ON LEAVE OF begin_bol#-18 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-19 C-Win
ON LEAVE OF begin_bol#-19 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-2 C-Win
ON LEAVE OF begin_bol#-2 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-20 C-Win
ON LEAVE OF begin_bol#-20 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-21 C-Win
ON LEAVE OF begin_bol#-21 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-22
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-22 C-Win
ON LEAVE OF begin_bol#-22 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-23 C-Win
ON LEAVE OF begin_bol#-23 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-24
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-24 C-Win
ON LEAVE OF begin_bol#-24 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-3 C-Win
ON LEAVE OF begin_bol#-3 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-4 C-Win
ON LEAVE OF begin_bol#-4 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-5 C-Win
ON LEAVE OF begin_bol#-5 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-6 C-Win
ON LEAVE OF begin_bol#-6 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-7 C-Win
ON LEAVE OF begin_bol#-7 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-8 C-Win
ON LEAVE OF begin_bol#-8 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-9 C-Win
ON LEAVE OF begin_bol#-9 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Customer# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-cust NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ship
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship C-Win
ON LEAVE OF begin_ship IN FRAME FRAME-A /* Ship-To# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-ship NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
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
        RUN valid-cust NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-ship NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.

        RUN run-report. 

        SESSION:SET-WAIT-STATE ("").

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &TYPE="Customer"
                            &begin_cust=begin_cust
                            &end_cust=begin_cust
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").                            
                        {custom/asimail.i &TYPE="Customer"
                             &begin_cust=begin_cust
                             &end_cust=begin_cust
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=lv-pdf-file + ".pdf" }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE="Customer"
                                  &begin_cust=begin_cust
                                  &end_cust=begin_cust
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
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        APPLY "entry" TO begin_cust.
    END.

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
    DISPLAY begin_cust begin_ship v-bol-list begin_bol#-1 begin_bol#-2 
        begin_bol#-3 begin_bol#-4 begin_bol#-5 begin_bol#-6 begin_bol#-7 
        begin_bol#-8 begin_bol#-9 begin_bol#-10 begin_bol#-11 begin_bol#-12 
        begin_bol#-13 begin_bol#-14 begin_bol#-15 begin_bol#-16 begin_bol#-17 
        begin_bol#-18 begin_bol#-19 begin_bol#-20 begin_bol#-21 begin_bol#-22 
        begin_bol#-23 begin_bol#-24 rd-dest tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE begin_cust begin_ship v-bol-list begin_bol#-1 begin_bol#-2 
        begin_bol#-3 begin_bol#-4 begin_bol#-5 begin_bol#-6 begin_bol#-7 
        begin_bol#-8 begin_bol#-9 begin_bol#-10 begin_bol#-11 begin_bol#-12 
        begin_bol#-13 begin_bol#-14 begin_bol#-15 begin_bol#-16 begin_bol#-17 
        begin_bol#-18 begin_bol#-19 begin_bol#-20 begin_bol#-21 begin_bol#-22 
        begin_bol#-23 begin_bol#-24 rd-dest btn-ok btn-cancel tbAutoClose 
        RECT-6 RECT-7 
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
    /*   DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
   
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
      */
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
    RUN custom/d-print.w(list-name).

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
    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE IF lv-prt-bypass THEN RUN custom/d-print.w (list-name).
        ELSE RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).

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
    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE /*run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ */
        RUN custom/scr-rpt2.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt,lv-prt-bypass).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* --------------------------------------------- oe/rep/oe-lad.p 3/94 RM ---- */
    /* print bill of ladings                                                      */
    /* -------------------------------------------------------------------------- */

    {sys/form/r-top.i}

    DEFINE VARIABLE v-cust     LIKE cust.cust-no.
    DEFINE VARIABLE v-ship     LIKE shipto.ship-id.
    DEFINE VARIABLE v-s-bol    AS INTEGER   FORMAT ">>>>>>>" INIT 0 EXTENT 24 NO-UNDO.
    DEFINE VARIABLE v-bol      LIKE oe-bolh.bol-no NO-UNDO.

    DEFINE VARIABLE v-carrier  LIKE carrier.dscr NO-UNDO.
    DEFINE VARIABLE v-to-ship  LIKE oe-boll.qty NO-UNDO.
    DEFINE VARIABLE v-tot-pkgs AS INTEGER   FORMAT ">>>>>9" NO-UNDO.
    DEFINE VARIABLE v-tot-pals LIKE v-tot-pkgs.
    DEFINE VARIABLE v-tot-wght LIKE v-tot-pkgs.
    DEFINE VARIABLE v-lines    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-page-tot AS INTEGER   INIT 0 NO-UNDO.
    DEFINE VARIABLE v-last     AS LOG       NO-UNDO.
    DEFINE VARIABLE v-bol-list AS CHARACTER FORMAT "x(75)".

    DEFINE VARIABLE li         AS INTEGER   NO-UNDO.

    FORM oe-boll.i-no   FORMAT "x(15)"      AT 9
        itemfg.i-name  FORMAT "x(30)"      AT 26
        v-to-ship      FORMAT ">>>,>>>"    TO 66
        "_______"                          AT 71 SKIP

        WITH FRAME ln-s DOWN NO-BOX NO-LABELS STREAM-IO WIDTH 90.


    ASSIGN
        v-cust      = begin_cust
        v-ship      = begin_ship
        v-s-bol[01] = begin_bol#-1
        v-s-bol[02] = begin_bol#-2
        v-s-bol[03] = begin_bol#-3
        v-s-bol[04] = begin_bol#-4
        v-s-bol[05] = begin_bol#-5
        v-s-bol[06] = begin_bol#-6
        v-s-bol[07] = begin_bol#-7
        v-s-bol[08] = begin_bol#-8
        v-s-bol[09] = begin_bol#-9
        v-s-bol[10] = begin_bol#-10
        v-s-bol[11] = begin_bol#-11
        v-s-bol[12] = begin_bol#-12
        v-s-bol[13] = begin_bol#-13
        v-s-bol[14] = begin_bol#-14
        v-s-bol[15] = begin_bol#-15
        v-s-bol[16] = begin_bol#-16
        v-s-bol[17] = begin_bol#-17
        v-s-bol[18] = begin_bol#-18
        v-s-bol[19] = begin_bol#-19
        v-s-bol[20] = begin_bol#-20
        v-s-bol[21] = begin_bol#-21
        v-s-bol[22] = begin_bol#-22
        v-s-bol[23] = begin_bol#-23
        v-s-bol[24] = begin_bol#-24.

    IF v-bol-list NE "" AND 
        (ASC(SUBSTR(v-bol-list,LENGTH(v-bol-list),1)) EQ 10 OR
        ASC(SUBSTR(v-bol-list,LENGTH(v-bol-list),1)) EQ 13)
        THEN v-bol-list = SUBSTR(v-bol-list,1,LENGTH(v-bol-list) - 1).

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    FOR EACH w-bol:
        DELETE w-bol.
    END.

    OUTPUT stream last-page to value(tmp-dir + "\bolsumm.txt") page-size 55.

    DO li = 1 TO 23 BY 2:
        FOR EACH oe-bolh
            WHERE oe-bolh.company EQ cocode
            AND oe-bolh.cust-no EQ v-cust
            AND oe-bolh.ship-id EQ v-ship
            AND oe-bolh.bol-no  GE v-s-bol[li]
            AND oe-bolh.bol-no  LE v-s-bol[li + 1]
            AND oe-bolh.printed EQ YES
            AND oe-bolh.posted  EQ NO
            USE-INDEX cust NO-LOCK:
            CREATE w-bol.
            w-bol.rec-id = RECID(oe-bolh).
        END.
    END.

    DO li = 1 TO NUM-ENTRIES(v-bol-list):
        v-bol = INT(ENTRY(li,v-bol-list)) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
            FOR EACH oe-bolh
                WHERE oe-bolh.company EQ cocode
                AND oe-bolh.cust-no EQ v-cust
                AND oe-bolh.ship-id EQ v-ship
                AND oe-bolh.bol-no  EQ v-bol
                AND oe-bolh.printed EQ YES
                AND oe-bolh.posted  EQ NO
                USE-INDEX cust NO-LOCK:
                CREATE w-bol.
                w-bol.rec-id = RECID(oe-bolh).
            END.
    END.

    i = 1.

    FOR EACH w-bol,

        FIRST oe-bolh
        WHERE RECID(oe-bolh) EQ w-bol.rec-id
        NO-LOCK

        BREAK BY w-bol.rec-id:

        IF LAST-OF(w-bol.rec-id) THEN 
        DO:
            IF oe-bolh.tot-pallets NE ? THEN
                v-tot-pals = v-tot-pals + oe-bolh.tot-pallets.
            IF oe-bolh.tot-wt NE ? THEN
                v-tot-wght = v-tot-wght + oe-bolh.tot-wt.
        END.

        ELSE DELETE w-bol.
    END.

    FIND FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ v-cust
        NO-LOCK NO-ERROR.

    FIND FIRST shipto
        WHERE shipto.company EQ cocode
        AND shipto.cust-no EQ v-cust
        AND shipto.ship-id EQ v-ship
        USE-INDEX ship-id NO-LOCK NO-ERROR.

    FORMAT HEADER
        SKIP(5)
        "SHIP TO:" AT 41 shipto.ship-id TO 70
        shipto.ship-name  AT 46
        shipto.ship-addr[1] AT 46
        shipto.ship-addr[2] AT 46
        shipto.ship-city AT 46
        shipto.ship-state
        shipto.ship-zip
        SKIP(1)
        "PAGE" AT 63 PAGE-NUMBER TO 69 FORMAT "99"
        "OF" AT 71 v-page-tot TO 75 FORMAT "99" SKIP
        FILL("-",80) FORMAT "x(80)" AT 1 SKIP
        "ITEM NUMBER" AT 9 "PRODUCT DESCRIPTION" AT 26
        "QUANTITY IN CARTONS" AT 59 SKIP
        "---------------" AT 9
        "------------------------------" AT 26
        "---------------------" AT 58 SKIP
        "TO SHIP" AT 59 "SHIPPED" AT 71 SKIP
        "----------" AT 58 "---------" AT 70 SKIP

        WITH FRAME head NO-BOX NO-LABELS PAGE-TOP STREAM-IO WIDTH 80.

    VIEW FRAME head.
    VIEW STREAM last-page FRAME head.  /* Print headers */

    PAGE STREAM last-page.

    {oe/rep/bolsumm.i "stream last-page"}

    v-page-tot = PAGE-NUMBER (last-page).

    ASSIGN
        v-lines    = 0
        v-to-ship  = 0
        v-tot-pkgs = 0.

    PAGE.

    {oe/rep/bolsumm.i}

    OUTPUT stream last-page close.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

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
    PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust C-Win 
PROCEDURE valid-cust :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ begin_cust:SCREEN-VALUE) THEN 
        DO:
            MESSAGE TRIM(begin_cust:LABEL) + " is invalid, try help..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO begin_cust.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ship C-Win 
PROCEDURE valid-ship :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST shipto
            WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ begin_cust:SCREEN-VALUE
            AND shipto.ship-id EQ begin_ship:SCREEN-VALUE) THEN 
        DO:
            MESSAGE TRIM(begin_ship:LABEL) + " is invalid, try help..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO begin_ship.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

