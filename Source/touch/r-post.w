&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-post.w

  Description: Touch Screen Posting

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Yoosun Kim 

  Created: 01/12/2000

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */     

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{sys/inc/var.i NEW SHARED}
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmp-dir   AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

/*{sys/inc/var.i new shared} */
/*DEFINE NEW SHARED VARIABLE cocode AS CHARACTER NO-UNDO.*/
/*DEFINE NEW SHARED VARIABLE locode AS CHARACTER NO-UNDO.*/

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE VARIABLE v-invalid  AS LOG NO-UNDO.
DEFINE VARIABLE v-download AS LOG INIT NO NO-UNDO.
DEFINE VARIABLE v-prior    AS LOG INIT NO NO-UNDO.

DEFINE BUFFER tmp-per FOR period.

DEFINE STREAM st-mach.
DEFINE STREAM st-emp.


DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.

&Scoped-define SCOPDEFS post
&SCOPED-DEFINE where-statement machtran.company = g_company


DEFINE TEMP-TABLE ttbl_pc-prdd NO-UNDO LIKE pc-prdd
    INDEX ttbl_pc-prdd IS PRIMARY
    company m-code op-date shift job frm blank-no.
DEFINE TEMP-TABLE ttbl_pc-prdh NO-UNDO LIKE pc-prdh
    INDEX ttbl_pc-prdh IS PRIMARY
    company m-code trans-date shift.
DEFINE TEMP-TABLE ttbl_rowid NO-UNDO
    FIELD pc-prdd_rowid AS ROWID
    FIELD total_time    AS INTEGER.
DEFINE VARIABLE machtotaltime    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE shiftpct         AS DECIMAL   NO-UNDO.
/*DEFINE VARIABLE i                AS INTEGER   NO-UNDO.*/
DEFINE VARIABLE waste-qty        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE run-qty          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE selected-company AS CHARACTER FORMAT "X(3)" LABEL "Company" NO-UNDO.

DEFINE VARIABLE lv-valid-to-post AS LOG       NO-UNDO.

/* mods for auto post to job cost */
DEFINE VARIABLE v-auto-bin       LIKE sys-ctrl.char-fld NO-UNDO.
DEFINE VARIABLE v-rm-fg          AS LOG       NO-UNDO.
DEFINE TEMP-TABLE tt-report LIKE report.

DEFINE TEMP-TABLE w-job 
    FIELD job LIKE job.job.

DEFINE STREAM excel.

{pc/pcprdd4u.i NEW}

{jc/jcgl-sh.i NEW}

DO TRANSACTION:
    {sys/inc/dcpostgl.i}
    {sys/inc/tspost.i}
    {sys/inc/tspostfg.i}
    {sys/inc/tssecure.i}  
END.

DEFINE VARIABLE v-autopost AS LOG NO-UNDO.
FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "AUTOPOST" NO-LOCK NO-ERROR.
v-autopost = IF AVAILABLE sys-ctrl THEN sys-ctrl.log-fld ELSE NO.

DEFINE BUFFER b-MachTran FOR MachTran.
DEFINE BUFFER b-MachEmp  FOR MachEmp.
DEFINE BUFFER b-EmpLogin FOR EmpLogin.

DEFINE VARIABLE logDelMachTran   AS LOGICAL   NO-UNDO.

DEFINE VARIABLE begin_job_number AS CHARACTER NO-UNDO.
DEFINE VARIABLE END_job_number   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lInvalid         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hdJobProcs       AS HANDLE    NO-UNDO. 
RUN jc/Jobprocs.p   PERSISTENT SET hdJobProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb_ShowNoCrewIsEntered lv-text-1 ~
begin_machine lv-text-2 end_machine begin_job-no begin_job-no2 end_job-no ~
end_job-no2 fi_BeginEmp fi_EndEmp begin_date end_date begin_shift end_shift ~
t-purge t-prt-rate tg_MachTrans tg_EmpMachTrans tg_blank-end-date rd-dest ~
tb_OpenCSV fi_file btn-ok btn-cancel tg_MachReltdTrans tbAutoClose RECT-6 ~
RECT-7 
&Scoped-Define DISPLAYED-OBJECTS tb_ShowNoCrewIsEntered lv-text-1 ~
begin_machine lv-text-2 end_machine begin_job-no begin_job-no2 end_job-no ~
end_job-no2 fi_BeginEmp fi_EndEmp begin_date end_date FILL-IN-1 begin_shift end_shift ~
t-purge t-prt-rate tg_MachTrans tg_EmpMachTrans tg_blank-end-date rd-dest ~
tb_OpenCSV fi_file tg_MachReltdTrans tbAutoClose 

/* Custom List Definitions                                              */
/* nonPurgeItems,purgeItems,List-3,List-4,List-5,F1                     */
&Scoped-define nonPurgeItems tb_ShowNoCrewIsEntered FILL-IN-1 post t-prt-rate rd-dest ~
lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm RECT-6 
&Scoped-define purgeItems lv-text-1 lv-text-2 fi_BeginEmp fi_EndEmp t-purge ~
tg_MachTrans tg_EmpMachTrans tg_blank-end-date tg_MachReltdTrans 

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

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no   AS CHARACTER FORMAT "X(9)" 
    LABEL "Beginning Job#" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1
    BGCOLOR 15 FONT 4.

DEFINE VARIABLE begin_job-no2  AS CHARACTER FORMAT "999":U INITIAL "000" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_machine  AS CHARACTER FORMAT "X(10)" 
    LABEL "Beginning Machine" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1.

DEFINE VARIABLE begin_shift    AS INTEGER   FORMAT ">>":U INITIAL 0 
    LABEL "Beginning Shift" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no     AS CHARACTER FORMAT "X(9)" 
    LABEL "Ending Job#" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1
    BGCOLOR 15 FONT 4.

DEFINE VARIABLE end_job-no2    AS CHARACTER FORMAT "999":U INITIAL "999" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_machine    AS CHARACTER FORMAT "X(10)" 
    LABEL "Ending Machine" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1.

DEFINE VARIABLE end_shift      AS INTEGER   FORMAT ">>":U INITIAL 0 
    LABEL "Ending Shift" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1      AS CHARACTER FORMAT "X(256)":U INITIAL " Output Destination" 
    VIEW-AS TEXT 
    SIZE 19 BY .62 NO-UNDO.

DEFINE VARIABLE fi_BeginEmp    AS CHARACTER FORMAT "X(20)":U 
    LABEL "Beginning Employee" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_EndEmp      AS CHARACTER FORMAT "X(20)":U 
    LABEL "Ending Employee" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\PostTouchScreen.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-text-1      AS CHARACTER FORMAT "X(256)":U INITIAL "(Files Located in TF3 and TF4)" 
    VIEW-AS TEXT 
    SIZE 30 BY .62 NO-UNDO.

DEFINE VARIABLE lv-text-2      AS CHARACTER FORMAT "X(256)":U INITIAL "(Files Located in TF4)" 
    VIEW-AS TEXT 
    SIZE 21 BY .62 NO-UNDO.

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
    "To CSV", 3
    SIZE 15 BY 4.57 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.24.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 12.33.

DEFINE VARIABLE post                   AS LOGICAL INITIAL NO 
    LABEL "Post Data Collection Entries" 
    VIEW-AS TOGGLE-BOX
    SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE t-prt-rate             AS LOGICAL INITIAL NO 
    LABEL "Print Labor Rates" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE t-purge                AS LOGICAL INITIAL NO 
    LABEL "Purge History" 
    VIEW-AS TOGGLE-BOX
    SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE tbAutoClose            AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel               AS LOGICAL INITIAL NO 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV             AS LOGICAL INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_ShowNoCrewIsEntered AS LOGICAL INITIAL NO 
    LABEL "Don't Show No Crew Is Entered Message" 
    VIEW-AS TOGGLE-BOX
    SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm           AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg_blank-end-date      AS LOGICAL INITIAL NO 
    LABEL "Purge Blank Machine End Date" 
    VIEW-AS TOGGLE-BOX
    SIZE 35 BY .91 NO-UNDO.

DEFINE VARIABLE tg_EmpMachTrans        AS LOGICAL INITIAL NO 
    LABEL "Posted Employee Machine Transactions" 
    VIEW-AS TOGGLE-BOX
    SIZE 42.2 BY .81 NO-UNDO.

DEFINE VARIABLE tg_MachReltdTrans      AS LOGICAL INITIAL NO 
    LABEL "Unposted Machine & Related Employee Transactions T-F-3." 
    VIEW-AS TOGGLE-BOX
    SIZE 62.2 BY .81 NO-UNDO.

DEFINE VARIABLE tg_MachTrans           AS LOGICAL INITIAL NO 
    LABEL "Posted Machine and Related Emp. Trans." 
    VIEW-AS TOGGLE-BOX
    SIZE 44.2 BY .91 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tb_ShowNoCrewIsEntered AT ROW 7.91 COL 14 HELP
    "Don't Show No Crew Is Entered Message" WIDGET-ID 14
    lv-text-1 AT ROW 9.81 COL 47 COLON-ALIGNED NO-LABELS
    begin_machine AT ROW 2.19 COL 25 COLON-ALIGNED HELP
    "Enter Beginning Machine"
    lv-text-2 AT ROW 11.48 COL 47 COLON-ALIGNED NO-LABELS
    end_machine AT ROW 2.19 COL 67.8 COLON-ALIGNED HELP
    "Enter Ending Machine"
    begin_job-no AT ROW 3.24 COL 25 COLON-ALIGNED HELP
    "Enter Beginning Job Number" WIDGET-ID 4
    begin_job-no2 AT ROW 3.24 COL 40 COLON-ALIGNED HELP
    "Enter Beginning Job Number" WIDGET-ID 6
    end_job-no AT ROW 3.24 COL 67.8 COLON-ALIGNED HELP
    "Enter Ending Job Number" WIDGET-ID 8
    end_job-no2 AT ROW 3.24 COL 83 COLON-ALIGNED HELP
    "Enter Ending Job Number" WIDGET-ID 10
    fi_BeginEmp AT ROW 6.76 COL 25 COLON-ALIGNED
    fi_EndEmp AT ROW 6.76 COL 67.8 COLON-ALIGNED
    begin_date AT ROW 4.48 COL 25 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 4.48 COL 67.8 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_shift AT ROW 5.57 COL 25 COLON-ALIGNED HELP
    "Enter Beginning Shift"
    end_shift AT ROW 5.57 COL 67.8 COLON-ALIGNED HELP
    "Enter Ending Shift"
    FILL-IN-1 AT ROW 14 COL 3 COLON-ALIGNED NO-LABELS
    post AT ROW 8.91 COL 14 HELP
    "Post to ASI Database Indicator"
    t-purge AT ROW 9.86 COL 14
    t-prt-rate AT ROW 10.81 COL 14
    tg_MachTrans AT ROW 8.91 COL 45.8
    tg_EmpMachTrans AT ROW 10.62 COL 45.8
    tg_blank-end-date AT ROW 11.71 COL 14 WIDGET-ID 2
    rd-dest AT ROW 14.76 COL 6 NO-LABELS
    lv-ornt AT ROW 14.52 COL 29 NO-LABELS
    lines-per-page AT ROW 14.52 COL 82 COLON-ALIGNED
    lv-font-no AT ROW 15.05 COL 32.4 COLON-ALIGNED
    lv-font-name AT ROW 16 COL 27 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 17.19 COL 28.6
    tb_excel AT ROW 19.57 COL 4
    tb_OpenCSV AT ROW 18.24 COL 92.4 RIGHT-ALIGNED
    fi_file AT ROW 18.14 COL 26.6 COLON-ALIGNED HELP
    "Enter File Name"
    btn-ok AT ROW 20.48 COL 28.6
    btn-cancel AT ROW 20.48 COL 48.6
    tg_MachReltdTrans AT ROW 12.71 COL 14 WIDGET-ID 12
    tbAutoClose AT ROW 19.62 COL 28.6 WIDGET-ID 58
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    RECT-6 AT ROW 14.33 COL 4
    RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 21.76
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
        TITLE              = "Touch Screen"
        HEIGHT             = 21.76
        WIDTH              = 96
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
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no:PRIVATE-DATA IN FRAME FRAME-A = "save".

ASSIGN 
    begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_shift:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no:PRIVATE-DATA IN FRAME FRAME-A = "save".

ASSIGN 
    end_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_shift:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi_BeginEmp IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    fi_BeginEmp:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN fi_EndEmp IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    fi_EndEmp:PRIVATE-DATA IN FRAME FRAME-A = "save".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
    lines-per-page:HIDDEN IN FRAME FRAME-A       = TRUE
    lines-per-page:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
    lv-font-name:HIDDEN IN FRAME FRAME-A       = TRUE
    lv-font-name:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
    lv-font-no:HIDDEN IN FRAME FRAME-A       = TRUE
    lv-font-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
    lv-ornt:HIDDEN IN FRAME FRAME-A       = TRUE
    lv-ornt:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lv-text-1 IN FRAME FRAME-A
   2                                                                    */
/* SETTINGS FOR FILL-IN lv-text-2 IN FRAME FRAME-A
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX post IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
    post:HIDDEN IN FRAME FRAME-A       = TRUE
    post:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR RADIO-SET rd-dest IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    rd-dest:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX t-prt-rate IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
    t-prt-rate:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX t-purge IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    t-purge:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_ShowNoCrewIsEntered IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A       = TRUE
    td-show-parm:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tg_blank-end-date IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    tg_blank-end-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tg_EmpMachTrans IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    tg_EmpMachTrans:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tg_MachReltdTrans IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    tg_MachReltdTrans:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tg_MachTrans IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
    tg_MachTrans:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Touch Screen */
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
ON WINDOW-CLOSE OF C-Win /* Touch Screen */
    DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE OBJECT hdJobProcs.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON HELP OF begin_date IN FRAME FRAME-A /* Beginning Date */
    DO:
        {methods/calendar.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON ENTRY OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
    DO:
    /*{&SELF-NAME}:FORMAT = 'XXXXXX-XX'.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_machine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_machine C-Win
ON HELP OF begin_machine IN FRAME FRAME-A /* Beginning Machine */
    DO:
        RUN lookups/mach.p.
        IF g_lookup-var NE ""                                AND
            TRIM(g_lookup-var) NE TRIM(begin_machine:SCREEN-VALUE) THEN 
        DO:
            begin_machine:SCREEN-VALUE = g_lookup-var.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_shift C-Win
ON HELP OF begin_shift IN FRAME FRAME-A /* Beginning Shift */
    DO:
        RUN lookups/shifts.p.
        IF g_lookup-var NE ""                                AND
            TRIM(g_lookup-var) NE TRIM(begin_shift:SCREEN-VALUE) THEN 
        DO:
            begin_shift:SCREEN-VALUE = g_lookup-var.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
    DO:
        DELETE OBJECT hdJobProcs.
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
    DO:
        ASSIGN {&displayed-objects}.
        ASSIGN
            begin_job-no     = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job-no)) 
            end_job-no       = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', end_job-no)) 
            begin_job_number = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', begin_job-no, begin_job-no2)) 
            end_job_number   = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', end_job-no, end_job-no2))  
            .
      
        IF rd-dest = 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.
        DEFINE BUFFER b-mach FOR mach.

  &IF DEFINED(tsPurge) NE 0 &THEN
        IF t-purge THEN 
        DO:

            IF tg_MachTrans THEN 
            DO:

                MESSAGE 
                    "All machine transactions will be purged for" 
                    "Machines - " begin_machine " to " end_machine SKIP 
                    "Period - " begin_date " and " end_date "." SKIP 
                    "Are you sure?" 
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.

                IF ll-ans THEN 
                DO:  
                    DISABLE TRIGGERS FOR LOAD OF machtran.
                    FOR EACH b-mach FIELDS(m-code) WHERE
                        b-mach.company = g_company AND
                        b-mach.m-code GE begin_machine AND
                        b-mach.m-code LE end_machine
                        NO-LOCK,
                        EACH b-machtran WHERE
                        b-machtran.company = g_company AND
                        b-machtran.posted = YES AND
                        b-machtran.machine EQ b-mach.m-code AND
                        ((begin_date LE b-machtran.start_date AND
                        b-machtran.start_date LE end_date AND
                        begin_date LE b-machtran.end_date AND
                        (b-machtran.end_date LE end_date) OR
                        (tg_blank-end-date AND b-machtran.END_date EQ ?)) OR
                        CAN-FIND(FIRST mach 
                        WHERE mach.company = b-machtran.company
                        AND mach.m-code = b-machtran.machine
                        AND mach.industry = "X"))
                        NO-LOCK:

                        logDelMachTran = TRUE.
                        FIND MachTran WHERE ROWID(MachTran) EQ ROWID(b-MachTran)
                            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                        IF AVAILABLE(MachTran) THEN
                            FOR EACH b-MachEmp 
                                WHERE b-MachEmp.table_rec_key EQ MachTran.rec_key
                                NO-LOCK:

                                FIND MachEmp WHERE ROWID(MachEmp) EQ ROWID(b-MachEmp)
                                    EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                                IF AVAILABLE(MachEmp) THEN
                                    DELETE machemp.
                                ELSE
                                    logDelMachTran = FALSE.
                            END. /* b-MachEmp */

                        IF logDelMachTran THEN
                            DELETE machtran.
                    END. /* each machtran*/

                    MESSAGE 
                        "Machine Transaction Purge is completed."
                        VIEW-AS ALERT-BOX INFORMATION.

                END. /* if ll-ans*/
            END.  /* tg_MachTrans */

            IF tg_MachReltdTrans THEN 
            DO:

                MESSAGE 
                    "Unposted Employee Machine Transactions will be purged for" SKIP
                    "Machines: " begin_machine " to " end_machine SKIP
                    "Employees:" fi_BeginEmp " to " fi_EndEmp SKIP
                    "Period: " begin_date " to " end_date "." SKIP
                    "Are you sure?" 
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans3 AS LOG.

                IF ll-ans3 THEN 
                DO:  
                    DISABLE TRIGGERS FOR LOAD OF machtran.
                    FOR EACH b-mach FIELDS(m-code) WHERE
                        b-mach.company = g_company AND
                        b-mach.m-code GE begin_machine AND
                        b-mach.m-code LE end_machine
                        NO-LOCK,
                        EACH b-machtran WHERE
                        b-machtran.company = g_company AND
                        b-machtran.posted = NO AND
                        b-machtran.machine EQ b-mach.m-code AND
                        ((begin_date LE b-machtran.start_date AND
                        b-machtran.start_date LE end_date AND
                        begin_date LE b-machtran.end_date AND
                        (b-machtran.end_date LE end_date) OR
                        (tg_blank-end-date AND b-machtran.END_date EQ ?)) OR
                        CAN-FIND(FIRST mach 
                        WHERE mach.company = b-machtran.company
                        AND mach.m-code = b-machtran.machine
                        AND mach.industry = "X"))
                        NO-LOCK:

                        logDelMachTran = TRUE.
                        FIND MachTran WHERE ROWID(MachTran) EQ ROWID(b-MachTran)
                            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                        IF AVAILABLE(MachTran) THEN
                            FOR EACH b-MachEmp 
                                WHERE b-MachEmp.table_rec_key EQ MachTran.rec_key
                                NO-LOCK:

                                FIND MachEmp WHERE ROWID(MachEmp) EQ ROWID(b-MachEmp)
                                    EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                                IF AVAILABLE(MachEmp) THEN
                                    DELETE machemp.
                                ELSE
                                    logDelMachTran = FALSE.
                            END. /* b-MachEmp */

                        IF logDelMachTran THEN
                            DELETE machtran.
                    END. /* each machtran*/

                    MESSAGE 
                        "Machine Transaction Purge is completed."
                        VIEW-AS ALERT-BOX INFORMATION.

                END. /* if ll-ans3*/
            END.  /* tg_MachReltdTrans */


            IF tg_EmpMachTrans THEN 
            DO:

                MESSAGE 
                    "Posted Employee Machine Transactions will be purged for" SKIP
                    "Machines: " begin_machine " to " end_machine SKIP
                    "Employees:" fi_BeginEmp " to " fi_EndEmp SKIP
                    "Period: " begin_date " to " end_date "." SKIP
                    "Are you sure?" 
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans2 AS LOG.

                IF ll-ans2 THEN 
                DO:  
                    FOR EACH employee FIELDS(employee) WHERE
                        employee.company EQ g_company AND
                        employee.employee GE fi_BeginEmp AND
                        employee.employee LE fi_EndEmp
                        NO-LOCK,
                        EACH b-MachEmp WHERE
                        b-MachEmp.employee EQ employee.employee AND
                        begin_date LE b-MachEmp.start_date AND
                        b-MachEmp.start_date LE end_date AND
                        begin_date LE b-MachEmp.end_date AND
                        b-MachEmp.end_date LE end_date AND
                        b-MachEmp.posted EQ YES
                        NO-LOCK:

                        FIND FIRST MachTran WHERE
                            MachTran.rec_key EQ b-MachEmp.table_rec_key AND
                            machtran.machine GE begin_machine AND 
                            machtran.machine LE end_machine
                            NO-LOCK NO-ERROR.

                        IF AVAILABLE(MachTran) THEN 
                        DO:

                            FIND MachEmp WHERE
                                ROWID(MachEmp) EQ ROWID(b-MachEmp)
                                EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

                            IF AVAILABLE(MachEmp) THEN
                                DELETE machemp.
                        END.
                    END. /* employee */

                    MESSAGE 
                        "Posted Machine Transaction Purge is completed."
                        VIEW-AS ALERT-BOX INFORMATION.
                END.
            END. /* tg_EmpMachTrans */

            RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

            RETURN.
        END. /* t-purge */
  &ELSE
        IF t-prt-rate AND lv-tssecure THEN 
        DO:
            DEFINE VARIABLE lv-passwd AS CHARACTER NO-UNDO.
            RUN custom/d-passwd.w (OUTPUT lv-passwd).
            IF lv-tssecure AND lv-tssecure-val <> lv-passwd THEN 
            DO:
                MESSAGE "Security Error! Invalid Password. " VIEW-AS ALERT-BOX ERROR.
                t-prt-rate = NO.
                DISPLAY t-prt-rate.
            END.
        END.

        /* 
         lv-valid-to-post
         this flag is updated in run-report.
         it is updated in post_.i
         */

        lv-valid-to-post = NO. 

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
                    {custom/asifax.i &type="TS Posting"
                            &begin_cust=begin_machine
                            &END_cust= begin_machine
                            &fax-subject="TS Posting"
                            &fax-body="TS Posting"
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = "TS Posting"
                             &begin_cust= begin_machine
                             &END_cust=begin_machine
                             &mail-subject="TS Posting"
                             &mail-body="TS Posting"
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "TS Posting"
                                  &begin_cust= begin_machine
                                  &END_cust=begin_machine
                                  &mail-subject=CURRENT-WINDOW:TITLE
                                  &mail-body=CURRENT-WINDOW:TITLE
                                  &mail-file=list-name }
                    END.
                END.
            WHEN 6 THEN RUN OUTPUT-to-port.

        END CASE. 

        IF lv-valid-to-post THEN 
        DO:
  
            RUN pCheckDate(INPUT DATE(TODAY)).
            IF lInvalid THEN RETURN NO-APPLY .     
   
            MESSAGE 
                "Are you ready to Post?" 
                VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans4 AS LOG.

            IF ll-ans4 THEN 
            DO:
                RUN do-post.
                MESSAGE "Post completed..." VIEW-AS ALERT-BOX.
            END.
        END.
        ELSE 
        DO:
            MESSAGE "No Transactions Valid to Post."
                VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        END.


  &ENDIF
  
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON HELP OF end_date IN FRAME FRAME-A /* Ending Date */
    DO:
        {methods/calendar.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON ENTRY OF end_job-no IN FRAME FRAME-A /* Ending Job# */
    DO:
    /*{&SELF-NAME}:FORMAT = 'XXXXXX-XX'.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_machine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_machine C-Win
ON HELP OF end_machine IN FRAME FRAME-A /* Ending Machine */
    DO:
        RUN lookups/mach.p.
        IF g_lookup-var NE ""                                AND
            TRIM(g_lookup-var) NE TRIM(end_machine:SCREEN-VALUE) THEN 
        DO:
            end_machine:SCREEN-VALUE = g_lookup-var.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_shift C-Win
ON HELP OF end_shift IN FRAME FRAME-A /* Ending Shift */
    DO:
        RUN lookups/shifts.p.
        IF g_lookup-var NE ""                                AND
            TRIM(g_lookup-var) NE TRIM(end_shift:SCREEN-VALUE) THEN 
        DO:
            end_shift:SCREEN-VALUE = g_lookup-var.
        END.
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


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
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


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_blank-end-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_blank-end-date C-Win
ON VALUE-CHANGED OF tg_blank-end-date IN FRAME FRAME-A /* Purge Blank Machine End Date */
    DO:

        IF tg_MachTrans:SCREEN-VALUE = "yes" THEN 
            t-purge:SCREEN-VALUE = "yes".


    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_EmpMachTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_EmpMachTrans C-Win
ON VALUE-CHANGED OF tg_EmpMachTrans IN FRAME FRAME-A /* Posted Employee Machine Transactions */
    DO:
        IF tg_EmpMachTrans:SCREEN-VALUE = "yes" THEN
            t-purge:SCREEN-VALUE = "yes".
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_MachReltdTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_MachReltdTrans C-Win
ON VALUE-CHANGED OF tg_MachReltdTrans IN FRAME FRAME-A /* Unposted Machine  Related Employee Transactions T-F-3. */
    DO:
    /*IF tg_EmpMachTrans:SCREEN-VALUE = "yes" THEN
      t-purge:SCREEN-VALUE = "yes". */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_MachTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_MachTrans C-Win
ON VALUE-CHANGED OF tg_MachTrans IN FRAME FRAME-A /* Posted Machine and Related Emp. Trans. */
    DO:

        IF tg_MachTrans:SCREEN-VALUE = "yes" THEN 
            t-purge:SCREEN-VALUE = "yes".


    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
/*{sys/inc/f3helpw.i} */
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
    {sys/inc/reportsConfigNK1.i "IL6" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

  &IF DEFINED(tsPurge) EQ 0 &THEN
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + ' Posting'.
    HIDE {&purgeItems} NO-PAUSE.
  &ELSE
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + ' Purge History'.
    HIDE {&nonPurgeItems} NO-PAUSE.
  &ENDIF

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        APPLY "entry" TO begin_machine.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-post C-Win 
PROCEDURE do-post :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-mach FOR mach.

    FOR EACH tt-report:
        DELETE tt-report.
    END.

    FOR EACH b-mach FIELDS(m-code) WHERE
        b-mach.company = g_company AND
        b-mach.m-code GE begin_machine AND
        b-mach.m-code LE end_machine
        NO-LOCK,
        EACH machtran
        WHERE machtran.company = g_company AND
        machtran.machine EQ b-mach.m-code AND
        machtran.posted EQ NO
        AND FILL(" ", iJobLen - length(TRIM(machtran.job_number))) + trim(machtran.job_number) GE begin_job-no
        AND FILL(" ", iJobLen - length(TRIM(machtran.job_number))) + trim(machtran.job_number) LE end_job-no
        AND machtran.job_sub GE int(begin_job-no2)
        AND machtran.job_sub LE int(end_job-no2):



        /* don't post to ASI only set posted flag to yes 
           07/30/01  YSK                                  */
        /*find first mach where mach.company = machtran.company and
                              mach.m-code = machtran.machine and
                              mach.industry = "X" 
                              no-lock no-error. */   

        /*     IF NOT(((machtran.end_date GE begin_date AND */
        /*             machtran.end_date LE end_date) or    */
        /*             can-find (first mach where           */
        /*              mach.company = machtran.company and */
        /*              mach.m-code = machtran.machine and  */
        /*              mach.industry = "X" ))) THEN        */
        /*         NEXT.                                    */

        IF NOT(((machtran.end_date GE begin_date AND
            machtran.end_date LE end_date) AND
            (machtran.shift GE TRIM(STRING(begin_shift,">>")) AND
            machtran.shift LE TRIM(STRING(end_shift,">>"))) ) OR
            CAN-FIND (FIRST mach WHERE
            mach.company = machtran.company AND
            mach.m-code = machtran.machine AND
            mach.industry = "X")) OR 
            machtran.end_date = ? THEN
            NEXT.

        /* gdm - 04150912 
        FIND FIRST emplogin NO-LOCK
           WHERE emplogin.company    EQ g_company
             AND emplogin.machine    EQ machtran.machine
             AND emplogin.start_date GE machtran.start_date
             AND emplogin.end_date   LE machtran.end_date 
             AND emplogin.start_time GE machtran.start_time
             AND emplogin.end_time   LT machtran.end_time NO-ERROR.
    
        IF NOT AVAIL emplogin THEN NEXT.
    
         gdm - 04150912 end */

        FOR EACH machemp 
            WHERE machemp.table_rec_key = machtran.rec_key
            AND machemp.shift GE TRIM(STRING(begin_shift,">>"))
            AND machemp.shift LE TRIM(STRING(end_shift,">>"))
            AND machemp.end_date >= begin_date 
            AND machemp.end_date <= end_date
            AND machemp.posted = NO
            USE-INDEX pi-machemp:
            machemp.posted = YES.         
        END.

        machtran.posted = YES.
    END.

    FOR EACH ttbl_pc-prdh EXCLUSIVE-LOCK:
        CREATE  pc-prdh.
        BUFFER-COPY ttbl_pc-prdh TO pc-prdh.
    END.

    FOR EACH ttbl_pc-prdd EXCLUSIVE-LOCK:
        CREATE  pc-prdd.
        BUFFER-COPY ttbl_pc-prdd TO pc-prdd.
        IF v-tspost THEN 
        DO:
            CREATE tt-report.
            tt-report.rec-id = RECID(pc-prdd).
        END.
    END.

    IF v-tspost THEN RUN post-wip.

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
    DISPLAY tb_ShowNoCrewIsEntered lv-text-1 begin_machine lv-text-2 end_machine 
        begin_job-no begin_job-no2 end_job-no end_job-no2 fi_BeginEmp 
        fi_EndEmp begin_date end_date FILL-IN-1 begin_shift end_shift t-purge t-prt-rate 
        tg_MachTrans tg_EmpMachTrans tg_blank-end-date rd-dest tb_OpenCSV 
        fi_file tg_MachReltdTrans tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE tb_ShowNoCrewIsEntered lv-text-1 begin_machine lv-text-2 end_machine 
        begin_job-no begin_job-no2 end_job-no end_job-no2 fi_BeginEmp 
        fi_EndEmp begin_date end_date begin_shift end_shift t-purge t-prt-rate 
        tg_MachTrans tg_EmpMachTrans tg_blank-end-date rd-dest tb_OpenCSV 
        fi_file btn-ok btn-cancel tg_MachReltdTrans tbAutoClose RECT-6 RECT-7 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gl-from-work C-Win 
PROCEDURE gl-from-work :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-run AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ip-trnum AS INTEGER NO-UNDO.

    DEFINE VARIABLE credits AS DECIMAL INIT 0 NO-UNDO.
    DEFINE VARIABLE debits  AS DECIMAL INIT 0 NO-UNDO. 


    FIND FIRST period
        WHERE period.company EQ cocode
        AND period.pst     LE TODAY
        AND period.pend    GE TODAY
        NO-LOCK.

    FOR EACH work-gl 
        WHERE (ip-run EQ 1 AND work-gl.job-no NE "")
        OR (ip-run EQ 2 AND work-gl.job-no EQ "")
        BREAK BY work-gl.actnum:

        ASSIGN
            debits  = debits  + work-gl.debits
            credits = credits + work-gl.credits.

        IF LAST-OF(work-gl.actnum) THEN 
        DO:
      
            RUN GL_SpCreateGLHist(cocode,
                work-gl.actnum,
                "JCOST",
                "Production Job Costing",
                TODAY,
                debits - credits,
                ip-trnum,
                period.pnum,
                "A",
                TODAY,
                (IF work-gl.job-no NE "" THEN "Job:" + work-gl.job-no + "-" + STRING(work-gl.job-no2,"999") ELSE ""),
                "FG"). 
            ASSIGN
                debits  = 0
                credits = 0.
        END.
    END.

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
    ASSIGN 
        begin_date    = DATE(1,1,YEAR(TODAY))
        END_date      = TODAY
        begin_machine = ""
        END_machine   = "zzzzzzzz"
        fi_BeginEmp   = "" 
        fi_EndEmp     = "zzzzzzzzzzzz"
        begin_shift   = 0
        END_shift     = 99.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    RUN custom/prntproc.p (list-name,INT(lv-font-no), lv-ornt).

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
    RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckDate C-Win 
PROCEDURE pCheckDate :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdtDate AS DATE NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
  
    lInvalid = NO.
    
    RUN GL_CheckModClosePeriod(INPUT cocode, INPUT DATE(ipdtDate), INPUT "FG", OUTPUT cMessage, OUTPUT lSuccess ) .  
    IF NOT lSuccess THEN 
    DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX INFORMATION.
        lInvalid = YES.
    END.      
    
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-wip C-Win 
PROCEDURE post-wip :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    /* --------------------------------------------------- pc/pc-post.p  8/94 gb  */
    /* Production Control - Posting Entry                                         */
    /* -------------------------------------------------------------------------- */

    DEFINE VARIABLE v-loc      LIKE fg-bin.loc NO-UNDO.
    DEFINE VARIABLE v-loc-bin  LIKE fg-bin.loc-bin NO-UNDO.
    DEFINE VARIABLE v-up-hs    LIKE eb.num-up NO-UNDO.
    DEFINE VARIABLE v-up       LIKE eb.num-up NO-UNDO.
    DEFINE VARIABLE v-out      LIKE est-op.n-out NO-UNDO.
    DEFINE VARIABLE v-on       LIKE eb.num-up NO-UNDO.
    DEFINE VARIABLE v-est-type LIKE est.est-type NO-UNDO.
    DEFINE VARIABLE v-trnum    LIKE gl-ctrl.trnum NO-UNDO.
    DEFINE VARIABLE X          AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-eb     FOR eb.
    DEFINE BUFFER bf-itemfg FOR itemfg.

    FOR EACH tt-report NO-LOCK,
        FIRST pc-prdd WHERE RECID(pc-prdd) EQ tt-report.rec-id,          
        FIRST mach
        {sys/ref/machW.i}
      AND mach.m-code EQ pc-prdd.m-code
        NO-LOCK,

        FIRST job
        WHERE job.company EQ cocode
        AND job.job     EQ pc-prdd.job
        AND job.job-no  EQ pc-prdd.job-no
        AND job.job-no2 EQ pc-prdd.job-no2             
        BREAK BY pc-prdd.m-code

        TRANSACTION:

    FIND FIRST w-job WHERE w-job.job EQ job.job NO-ERROR.
    IF NOT AVAILABLE w-job THEN CREATE w-job.
    w-job.job = job.job.

    ASSIGN
        v-up  = 1
        v-out = 1
        v-on  = 1.

    FIND FIRST est
        WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
        NO-LOCK NO-ERROR.
    v-est-type = IF AVAILABLE est THEN est.est-type ELSE 1.
    IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

    FOR EACH mach-part WHERE
        mach-part.company EQ mach.company AND
        mach-part.m-code EQ mach.m-code
        EXCLUSIVE-LOCK:

        mach-part.total-impressions-run = mach-part.total-impressions-run
            + pc-prdd.qty + pc-prdd.waste.

        FIND FIRST reftable WHERE
            reftable.reftable EQ "MACHPARTHOURS" AND
            reftable.company  EQ mach-part.company AND
            reftable.loc      EQ mach-part.m-code AND
            reftable.code     EQ mach-part.rm-part-code
            EXCLUSIVE-LOCK NO-ERROR.

        IF NOT AVAILABLE reftable THEN 
        DO:
            CREATE reftable.
            ASSIGN
                reftable.reftable = "MACHPARTHOURS"
                reftable.company  = mach-part.company
                reftable.loc      = mach-part.m-code
                reftable.code     = mach-part.rm-part-code. 
        END.

        reftable.val[1] = reftable.val[1]
            + pc-prdd.hours.

        RELEASE reftable.
    END.

    IF mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR
        mach.dept[3] EQ "PR" OR mach.dept[4] EQ "PR" THEN
        RUN update-plate-die (ROWID(pc-prdd), "P", v-est-type).

    IF mach.dept[1] EQ "DC" OR mach.dept[2] EQ "DC" OR
        mach.dept[3] EQ "DC" OR mach.dept[4] EQ "DC" THEN
        RUN update-plate-die (ROWID(pc-prdd), "D", v-est-type).

    IF INDEX("AP",mach.p-type) GT 0 THEN
        ASSIGN
            v-on  = 1
            v-up  = 1
            v-out = 1.

    ELSE
        IF AVAILABLE est THEN 
        DO:
            RUN sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, OUTPUT v-up).

            FIND FIRST ef
                WHERE ef.company EQ est.company
                AND ef.est-no  EQ est.est-no
                AND ef.form-no EQ pc-prdd.frm
                NO-LOCK NO-ERROR.

            IF AVAILABLE ef THEN 
            DO:
                RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).
                v-on = v-up * v-on.
            END.

            FIND FIRST est-op
                WHERE est-op.company EQ est.company
                AND est-op.est-no  EQ est.est-no
                AND est-op.s-num   EQ pc-prdd.frm
                AND (est-op.b-num  EQ pc-prdd.blank-no OR
                pc-prdd.blank-no EQ 0)
                AND est-op.m-code  EQ pc-prdd.m-code
                AND est-op.op-pass EQ pc-prdd.pass
                AND est-op.dept    EQ pc-prdd.dept
                AND est-op.line    LT 500
                NO-LOCK NO-ERROR.

            IF ((AVAILABLE est-op) AND est-op.op-sb)           OR
                ((NOT AVAILABLE est-op) AND mach.p-type NE "B") THEN 
            DO:

                IF AVAILABLE est-op THEN
                    RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-out).

                ELSE v-out = 1.

                v-up = v-up * v-out.
            END.

            ELSE v-up = 1.

            v-on = v-on / v-up.
        END.

    v-up-hs = 1.

    IF pc-prdd.dept EQ "HS" AND
        AVAILABLE est            AND
        mach.therm           AND
        mach.p-type EQ "S"   THEN
        RUN sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, OUTPUT v-up-hs).

    {pc/pcmchact.i}

    IF pc-prdd.complete THEN 
    DO:
        RUN pc/pcprdd4u.p (ROWID(pc-prdd)).

        FOR EACH tt-job-hdr,
            FIRST itemfg
            WHERE itemfg.company    EQ cocode
            AND itemfg.i-no       EQ tt-job-hdr.i-no
            AND itemfg.case-count GT 0
            NO-LOCK:

            x = 1.
            FOR EACH fg-rctd NO-LOCK BY fg-rctd.r-no DESCENDING:
                LEAVE.
            END.
            IF AVAILABLE fg-rctd THEN x = fg-rctd.r-no.

            FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
            IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT x THEN x = fg-rcpth.r-no.

            CREATE fg-rctd.
            ASSIGN
                fg-rctd.r-no       = X + 1
                fg-rctd.rct-date   = pc-prdd.op-date
                fg-rctd.trans-time = pc-prdd.op-time
                fg-rctd.company    = cocode
                fg-rctd.rita-code  = "R"
                fg-rctd.i-name     = itemfg.i-name
                fg-rctd.i-no       = tt-job-hdr.i-no
                fg-rctd.job-no     = pc-prdd.job-no
                fg-rctd.job-no2    = pc-prdd.job-no2.

            ASSIGN
                v-up  = 1
                v-out = 1.

            IF AVAILABLE est AND index("APB",mach.p-type) LE 0 THEN 
            DO:
                RUN sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, OUTPUT v-up).

                FIND FIRST est-op
                    WHERE est-op.company EQ est.company
                    AND est-op.est-no  EQ est.est-no
                    AND est-op.s-num   EQ pc-prdd.frm
                    AND (est-op.b-num  EQ pc-prdd.blank-no OR
                    pc-prdd.blank-no EQ 0)
                    AND est-op.m-code  EQ pc-prdd.m-code
                    AND est-op.op-pass EQ pc-prdd.pass
                    AND est-op.dept    EQ pc-prdd.dept
                    AND est-op.line    LT 500
                    NO-LOCK NO-ERROR.
                IF AVAILABLE est-op AND est-op.n-out NE 0 THEN v-out = est-op.n-out.
            END.

            ASSIGN
                fg-rctd.b-num      = pc-prdd.blank-no
                fg-rctd.s-num      = pc-prdd.frm
                fg-rctd.t-qty      = pc-prdd.qty / v-up-hs * v-out * v-up
                fg-rctd.pur-uom    = itemfg.prod-uom
                fg-rctd.cost-uom   = itemfg.prod-uom
                fg-rctd.std-cost   = tt-job-hdr.std-tot-cost
                fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
                fg-rctd.qty-case   = itemfg.case-count
                fg-rctd.partial    = fg-rctd.t-qty MODULO itemfg.case-count
                fg-rctd.cases      = trunc(fg-rctd.t-qty / itemfg.case-count,0)
                fg-rctd.cases-unit = 1.

            IF fg-rctd.t-qty LE 0 THEN fg-rctd.cases = 0.

            RELEASE fg-bin.

            FIND FIRST reftable NO-LOCK
                WHERE reftable.reftable EQ "pc/pcprddu3.p"
                AND reftable.company  EQ pc-prdd.company
                AND reftable.code     EQ /*pc-prdd.rec_key*/ STRING(RECID(pc-prdd))
                NO-ERROR.

            IF AVAILABLE reftable THEN 
            DO:
                ASSIGN
                    fg-rctd.cases      = reftable.val[1]
                    fg-rctd.qty-case   = reftable.val[2]
                    fg-rctd.cases-unit = reftable.val[3]
                    fg-rctd.partial    = fg-rctd.t-qty - (fg-rctd.cases * fg-rctd.qty-case).

                FIND FIRST fg-bin 
                    WHERE fg-bin.rec_key EQ reftable.code2 /*RECID(fg-bin) EQ INT(reftable.code2)*/ 
                    NO-LOCK NO-ERROR.
            END.

            IF AVAILABLE fg-bin THEN
                ASSIGN
                    v-loc       = fg-bin.loc
                    v-loc-bin   = fg-bin.loc-bin
                    fg-rctd.tag = fg-bin.tag.

            ELSE
                IF v-auto-bin EQ "ShipTo" THEN 
                DO:
                    /*get estimate blank file from finished goods item file*/
                    FIND FIRST eb
                        WHERE eb.company  EQ cocode
                        AND eb.est-no   EQ itemfg.est-no
                        AND eb.stock-no EQ itemfg.i-no
                        USE-INDEX est-no NO-LOCK NO-ERROR.

                    IF AVAILABLE eb THEN 
                    DO:
                        /*get customer file from estimate blank file*/
                        FIND FIRST cust
                            WHERE cust.company EQ cocode
                            AND cust.cust-no EQ eb.cust-no
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE cust THEN 
                        DO:              
                            FIND FIRST shipto
                                WHERE shipto.company = cocode
                                AND shipto.cust-no = cust.cust-no 
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE shipto THEN 
                            DO:
                                FIND FIRST fg-bin
                                    WHERE fg-bin.company EQ cocode
                                    AND fg-bin.loc     EQ shipto.loc
                                    AND fg-bin.loc-bin EQ shipto.loc-bin
                                    AND fg-bin.i-no    EQ ""
                                    NO-LOCK NO-ERROR.
                                IF AVAILABLE fg-bin THEN 
                                    ASSIGN
                                        v-loc     = shipto.loc
                                        v-loc-bin = shipto.loc-bin.
                            END.

                            IF v-loc EQ "" AND v-loc-bin EQ "" THEN 
                            DO:
                                FIND FIRST fg-bin
                                    WHERE fg-bin.company EQ cocode
                                    AND fg-bin.loc     EQ itemfg.def-loc
                                    AND fg-bin.loc-bin EQ itemfg.def-loc-bin
                                    AND fg-bin.i-no    EQ ""
                                    NO-LOCK NO-ERROR.
                                IF AVAILABLE fg-bin THEN 
                                    ASSIGN 
                                        v-loc     = itemfg.def-loc
                                        v-loc-bin = itemfg.def-loc-bin.
                            END. /*if avail shipto*/
                        END. /*if avail cust*/
                    END. /*if avail eb*/
                END. /*if system default is shipto*/
                /*else if "FGFILE" then get from finished goods file*/
                ELSE 
                DO:
                    FIND FIRST fg-bin
                        WHERE fg-bin.company EQ cocode
                        AND fg-bin.loc     EQ itemfg.def-loc
                        AND fg-bin.loc-bin EQ itemfg.def-loc-bin
                        AND fg-bin.i-no    EQ ""
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE fg-bin THEN 
                        ASSIGN
                            v-loc     = itemfg.def-loc
                            v-loc-bin = itemfg.def-loc-bin.
                END. /*else FGFILE*/

            /*if bin and warehouse are blank, goto cust "X" shipto file*/
            IF v-loc EQ "" AND v-loc-bin EQ "" THEN 
            DO:
                FIND FIRST cust
                    WHERE cust.company EQ cocode
                    AND cust.active  EQ "X"
                    NO-LOCK NO-ERROR.

                IF AVAILABLE cust THEN 
                DO:
                    FIND FIRST shipto
                        WHERE shipto.company EQ cocode
                        AND shipto.cust-no EQ cust.cust-no  
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE shipto THEN 
                    DO:
                        FIND FIRST fg-bin
                            WHERE fg-bin.company EQ cocode
                            AND fg-bin.loc     EQ shipto.loc
                            AND fg-bin.loc-bin EQ shipto.loc-bin
                            AND fg-bin.i-no    EQ ""
                            NO-LOCK NO-ERROR.
                        ASSIGN
                            v-loc     = shipto.loc
                            v-loc-bin = shipto.loc-bin.
                    END.                                  
                END.
            END.

            ASSIGN
                fg-rctd.loc     = v-loc
                fg-rctd.loc-bin = v-loc-bin.

            FIND FIRST fg-bin
                WHERE fg-bin.company EQ fg-rctd.company
                AND fg-bin.i-no    EQ fg-rctd.i-no
                AND fg-bin.job-no  EQ pc-prdd.job-no
                AND fg-bin.job-no2 EQ pc-prdd.job-no2
                AND fg-bin.loc     EQ fg-rctd.loc
                AND fg-bin.loc-bin EQ fg-rctd.loc-bin
                AND fg-bin.tag     EQ fg-rctd.tag
                NO-LOCK NO-ERROR.

            IF AVAILABLE fg-bin THEN fg-rctd.cases-unit = fg-bin.cases-unit.
            RUN fg/comprcpt.p (ROWID(fg-rctd)).                         

        END. /* for each tt-job-hdr*/

    END. /* for each pc-prdd.completed*/
    DELETE pc-prdd.
END. /* for each tt-report, pc-prdd */

IF dcpostgl-log THEN 
DO TRANSACTION:
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
            RUN gl-from-work (1, v-trnum).
            RUN gl-from-work (2, v-trnum).
            LEAVE.
        END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
/* gdm - 11050906 */

END.

FOR EACH w-job,
    FIRST job NO-LOCK WHERE job.company EQ cocode AND job.job EQ w-job.job:
    RUN jc/job-cls2.p (RECID(job)).
END.

RUN job_CloseJob_DCPost IN hdJobProcs(INPUT cocode, INPUT TABLE w-job).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /*==== Report main body procedure ================================
    ==================================================================*/
    DEFINE VARIABLE ld-total-time AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ld-emp-time   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ld-mr-time    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ld-run-time   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li-job-cnt    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li-mr-cnt     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ld-mr-avg     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ld-run-avg    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ld-tot-avg    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-tmp-mr     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-tmp-run    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-tmp-tot    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ls-tot-mr     AS cha       NO-UNDO.
    DEFINE VARIABLE ls-tot-run    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ls-tot-tot    AS cha       NO-UNDO.
    DEFINE VARIABLE ld-waste%     AS DECIMAL   FORM ">,>>9.99" NO-UNDO.
    DEFINE VARIABLE lv-rqty       LIKE machtran.run_qty NO-UNDO.
    DEFINE VARIABLE lv-wqty       LIKE machtran.waste_qty NO-UNDO.
    DEFINE VARIABLE v-sch-mach    AS LOG       NO-UNDO.

    DEFINE VARIABLE v-rate-total  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE excelheader   AS CHARACTER NO-UNDO.

    SESSION:SET-WAIT-STATE("general").

    {sys/form/r-top3w.f}

    FORM HEADER SKIP(1) WITH FRAME r-top.

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        str-tit3 = "TS Posting "
        {sys/inc/ctrtext.i str-tit3 132}.



    {sys/inc/print1.i}
    {sys/inc/outprint.i value(lines-per-page)}

    FOR EACH ttbl_pc-prdh:    
        DELETE ttbl_pc-prdh.
    END.

    FOR EACH ttbl_pc-prdd:
        DELETE ttbl_pc-prdd.
    END.

    IF td-show-parm THEN RUN show-param.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        ASSIGN 
            excelheader = "Mach,Description,DP,Date,Sh,Job No,Form,Blank,"
                       + "Item,Description,Code,Hours,Crew,Start,Stop,Run Qty,Waste,C,EmpID,Rate".
        PUT STREAM excel UNFORMATTED excelheader SKIP.
    END.

    VIEW FRAME r-top.
    selected-company = g_company.

    {methods/lstlogic/custom/post_.i}

    OUTPUT close.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.

        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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
                    parm-lbl-list = parm-lbl-list + ","
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-plate-die C-Win 
PROCEDURE update-plate-die :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid    AS   ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ip-upd-type AS   CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-est-type LIKE est.est-type NO-UNDO.

    DEFINE BUFFER b-pc-prdd FOR pc-prdd.
    DEFINE BUFFER bf-job    FOR job.

    FIND b-pc-prdd WHERE ROWID(b-pc-prdd) EQ ip-rowid NO-LOCK NO-ERROR.

    IF AVAILABLE b-pc-prdd THEN
        FOR FIRST bf-job
            WHERE bf-job.company EQ b-pc-prdd.company
            AND bf-job.job     EQ b-pc-prdd.job
            AND bf-job.job-no  EQ b-pc-prdd.job-no
            AND bf-job.job-no2 EQ b-pc-prdd.job-no2
            NO-LOCK,
            FIRST job-hdr
            WHERE job-hdr.company   EQ bf-job.company
            AND job-hdr.job       EQ bf-job.job
            AND job-hdr.job-no    EQ bf-job.job-no
            AND job-hdr.job-no2   EQ bf-job.job-no2
            AND (job-hdr.frm      EQ b-pc-prdd.frm OR
            ip-est-type      EQ 2)
            NO-LOCK:

            FIND FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ job-hdr.i-no
                NO-LOCK NO-ERROR.

            IF ip-est-type EQ 2 AND job.est-no NE "" AND
                AVAILABLE itemfg AND itemfg.isaset        THEN
                FOR EACH eb
                    WHERE eb.company EQ cocode
                    AND eb.est-no  EQ bf-job.est-no
                    AND eb.form-no EQ b-pc-prdd.frm
                    NO-LOCK,
                    FIRST itemfg
                    WHERE itemfg.company EQ cocode
                    AND itemfg.i-no    EQ eb.stock-no
                    NO-LOCK:
                    LEAVE.
                END.

            IF AVAILABLE itemfg THEN 
            DO:

                IF ip-upd-type EQ "P" AND itemfg.plate-no NE "" THEN
                    FIND FIRST prep
                        WHERE prep.company EQ cocode
                        AND prep.code    EQ itemfg.plate-no
                        NO-ERROR.

                ELSE
                    IF ip-upd-type EQ "D" AND itemfg.die-no NE "" THEN
                        FIND FIRST prep
                            WHERE prep.company EQ cocode
                            AND prep.code    EQ itemfg.die-no
                            NO-ERROR.

                IF AVAILABLE prep THEN 
                DO:
                    ASSIGN 
                        prep.no-of-impressions = prep.no-of-impressions +
                                          b-pc-prdd.qty + b-pc-prdd.waste
                        prep.last-date         = b-pc-prdd.op-date
                        prep.last-job-no       = b-pc-prdd.job-no
                        prep.last-job-no2      = b-pc-prdd.job-no2
                        .
                    RELEASE prep.
                END.
            END.
        END.

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
                .
        ELSE
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "NO"
                fi_file:SENSITIVE       = NO
                tb_OpenCSV:SENSITIVE    = NO      
                .
        ASSIGN 
            fi_file:SCREEN-VALUE = "c:\tmp\PostTouchScreen.csv".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

