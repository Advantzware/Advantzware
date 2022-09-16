&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: pc\r-wippst.p

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
DEFINE TEMP-TABLE ttJob NO-UNDO
    FIELD company  AS CHARACTER
    FIELD jobID    AS CHARACTER
    FIELD jobID2   AS INTEGER
    FIELD location AS CHARACTER
    FIELD jobRowID AS ROWID
    .

/* Parameters Definitions ---                                           */
DEFINE VARIABLE ip-post   AS LOG       INIT YES NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

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

DEFINE VARIABLE ll-ok-to-post AS LOG     NO-UNDO.

DEFINE VARIABLE v-autopost    AS LOG     NO-UNDO.
DEFINE VARIABLE v-auto-bin    LIKE sys-ctrl.char-fld NO-UNDO.
DEFINE VARIABLE v-rm-fg       AS LOG     NO-UNDO.
DEFINE VARIABLE lInvalid      AS LOGICAL NO-UNDO.
DEFINE VARIABLE hdJobProcs    AS HANDLE  NO-UNDO.

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.

DEFINE TEMP-TABLE w-job NO-UNDO 
    FIELD job LIKE job.job.

{pc/pcprdd4u.i NEW}

{jc/jcgl-sh.i NEW}

DEFINE BUFFER bf-prdd FOR pc-prdd.

DO TRANSACTION:
    {sys/inc/dcpostgl.i}
    {sys/inc/tspost.i}
    {sys/inc/tspostfg.i}
    {sys/inc/fgrecpt.i}
END.

DEFINE STREAM excel.

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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date end_date begin_mach ~
end_mach begin_job-no begin_job-no2 end_job-no end_job-no2 begin_shift ~
end_shift begin_loc end_loc tb_tot-hrs tb_pg-brk rd-dest fi_file tb_OpenCSV ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_mach end_mach ~
begin_job-no begin_job-no2 end_job-no end_job-no2 begin_shift end_shift ~
begin_loc end_loc tb_tot-hrs tb_pg-brk rd-dest fi_file tb_OpenCSV ~
tbAutoClose 

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
    SIZE 20.4 BY .95 NO-UNDO.

DEFINE VARIABLE begin_job-no   AS CHARACTER FORMAT "X(9)":U 
    LABEL "Beginning Job#" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2  AS CHARACTER FORMAT "-999":U INITIAL "000" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_loc      AS CHARACTER FORMAT "X(6)" 
    LABEL "Beginning Warehouse" 
    VIEW-AS FILL-IN 
    SIZE 20.4 BY 1.

DEFINE VARIABLE begin_mach     AS CHARACTER FORMAT "X(5)" 
    LABEL "Beginning Machine#" 
    VIEW-AS FILL-IN 
    SIZE 20.4 BY 1.

DEFINE VARIABLE begin_shift    AS INTEGER   FORMAT ">>" INITIAL 1 
    LABEL "Beginning Shift" 
    VIEW-AS FILL-IN 
    SIZE 20.4 BY 1.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 20.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no     AS CHARACTER FORMAT "X(9)":U INITIAL "zzzzzzzzz" 
    LABEL "Ending Job#" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2    AS CHARACTER FORMAT "-999":U INITIAL "999" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_loc        AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
    LABEL "Ending Warehouse" 
    VIEW-AS FILL-IN 
    SIZE 20.4 BY 1.

DEFINE VARIABLE end_mach       AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
    LABEL "Ending Machine#" 
    VIEW-AS FILL-IN 
    SIZE 20.4 BY 1.

DEFINE VARIABLE end_shift      AS INTEGER   FORMAT ">>" INITIAL 99 
    LABEL "Ending Shift" 
    VIEW-AS FILL-IN 
    SIZE 20.4 BY 1.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\TransferWIPtoJobCost.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 51 BY 1.

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
    "To File", 3,
    "To CSV", 4
    SIZE 14 BY 5.24 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.91.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 9.71.

DEFINE VARIABLE tbAutoClose  AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_pg-brk    AS LOGICAL INITIAL NO 
    LABEL "Print Department Page Break?" 
    VIEW-AS TOGGLE-BOX
    SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE tb_tot-hrs   AS LOGICAL INITIAL YES 
    LABEL "Print Total Hours?" 
    VIEW-AS TOGGLE-BOX
    SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_date AT ROW 2.67 COL 26.8 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 2.67 COL 68 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_mach AT ROW 3.62 COL 26.8 COLON-ALIGNED HELP
    "Enter Beginning Machine Number"
    end_mach AT ROW 3.62 COL 68 COLON-ALIGNED HELP
    "Enter Ending Machine Number"
    begin_job-no AT ROW 4.57 COL 26.8 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    begin_job-no2 AT ROW 4.57 COL 41.8 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    end_job-no AT ROW 4.57 COL 68 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    end_job-no2 AT ROW 4.57 COL 83 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    begin_shift AT ROW 5.52 COL 26.8 COLON-ALIGNED HELP
    "Enter Beginning Machine Number"
    end_shift AT ROW 5.52 COL 68 COLON-ALIGNED HELP
    "Enter Beginning Machine Number"
    begin_loc AT ROW 6.48 COL 26.8 COLON-ALIGNED HELP
    "Enter Beginning Machine Number" WIDGET-ID 8
    end_loc AT ROW 6.48 COL 68 COLON-ALIGNED HELP
    "Enter Ending Machine Number" WIDGET-ID 10
    tb_tot-hrs AT ROW 8.48 COL 28.8
    tb_pg-brk AT ROW 9.67 COL 62.8 RIGHT-ALIGNED
    lv-ornt AT ROW 12.1 COL 31 NO-LABELS
    lines-per-page AT ROW 12.1 COL 84 COLON-ALIGNED
    rd-dest AT ROW 12.17 COL 5.4 NO-LABELS
    lv-font-no AT ROW 12.91 COL 35 COLON-ALIGNED
    tb_excel AT ROW 13.14 COL 74 RIGHT-ALIGNED WIDGET-ID 4
    lv-font-name AT ROW 13.86 COL 29 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 15.29 COL 26.6
    fi_file AT ROW 16.24 COL 24.6 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 2
    tb_OpenCSV AT ROW 16.33 COL 92.4 RIGHT-ALIGNED WIDGET-ID 6
    tbAutoClose AT ROW 17.86 COL 26.6 WIDGET-ID 64
    btn-ok AT ROW 18.76 COL 26.6
    btn-cancel AT ROW 18.76 COL 48.6
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 11.52 COL 5
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    RECT-6 AT ROW 11.86 COL 4
    RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 20
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
        TITLE              = "Production Control Edit List"
        HEIGHT             = 20
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
   FRAME-NAME                                                           */
ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_loc:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_mach:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_shift:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_loc:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_mach:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_shift:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_pg-brk IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_pg-brk:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_tot-hrs:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Production Control Edit List */
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
ON WINDOW-CLOSE OF C-Win /* Production Control Edit List */
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
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON HELP OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-pcprdd.w (cocode,begin_job-no:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN begin_job-no:SCREEN-VALUE  = ENTRY(1,char-val)
                begin_job-no2:SCREEN-VALUE = ENTRY(2,char-val)
                end_job-no:SCREEN-VALUE    = ENTRY(1,char-val)
                end_job-no2:SCREEN-VALUE   = ENTRY(2,char-val).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME begin_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_loc C-Win
ON LEAVE OF begin_loc IN FRAME FRAME-A /* Beginning Warehouse */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_shift C-Win
ON LEAVE OF begin_shift IN FRAME FRAME-A /* Beginning Shift */
    DO:
        ASSIGN {&self-name}.
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
        DEFINE VARIABLE lv-post AS LOG NO-UNDO.
        ASSIGN  
            begin_date  
            end_date      
            begin_mach    
            end_mach      
            begin_job-no  
            begin_job-no2 
            end_job-no    
            end_job-no2   
            begin_shift   
            end_shift     
            tb_tot-hrs    
            tb_pg-brk     
            lv-ornt       
            lines-per-page
            rd-dest       
            lv-font-no    
            lv-font-name  
            td-show-parm   
            tb_excel      
            tb_OpenCSV   
            fi_file
            .       

        IF rd-dest = 4 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.

        FIND FIRST sys-ctrl
            WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.name    EQ "AUTOPOST"
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE sys-ctrl THEN 
        DO TRANSACTION:
            CREATE sys-ctrl.
            ASSIGN
                sys-ctrl.company = cocode
                sys-ctrl.name    = "AUTOPOST"
                sys-ctrl.descrip = "Autopost to Finished Goods Receipts?".
            MESSAGE sys-ctrl.descrip
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE sys-ctrl.log-fld.
        END.
        ASSIGN
            v-autopost = sys-ctrl.log-fld
            v-auto-bin = sys-ctrl.char-fld.

        FIND FIRST sys-ctrl
            WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.name    EQ "RM=FG"
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE sys-ctrl THEN 
        DO TRANSACTION:
            CREATE sys-ctrl.
            ASSIGN
                sys-ctrl.company = cocode
                sys-ctrl.name    = "RM=FG"
                sys-ctrl.descrip = "Validate RM issues = FG Produced Plus Waste?"
                sys-ctrl.log-fld = NO.
            MESSAGE sys-ctrl.descrip
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE sys-ctrl.log-fld.
        END.
        v-rm-fg = sys-ctrl.log-fld.

        ll-ok-to-post = NO.

        FOR EACH tt-report:
            DELETE tt-report.
        END.

        FOR EACH work-gl:
            DELETE work-gl.
        END.
       
        RUN run-report. 

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
        END CASE.

        IF ip-post THEN 
        DO:
            RUN pCheckDate.
            IF lInvalid THEN RETURN NO-APPLY.
        
            IF ll-ok-to-post THEN 
            DO:
                lv-post = NO.

                MESSAGE "Post WIP?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE lv-post.

                IF lv-post THEN 
                DO:
                    RUN post-wip.

                    MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
                END.
            END.

            ELSE MESSAGE "No WIP available for posting..." VIEW-AS ALERT-BOX ERROR.
        END.  /* IF ip-post THEN */
        IF rd-dest = 4 THEN
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
            END.  /* IF NOT tb_OpenCSV THEN  */
        END.  /* IF rd-dest = 4 THEN */
        
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


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON HELP OF end_job-no IN FRAME FRAME-A /* Ending Job# */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-pcprdd.w (cocode,end_job-no:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN end_job-no:SCREEN-VALUE  = ENTRY(1,char-val)
                end_job-no2:SCREEN-VALUE = ENTRY(2,char-val).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME end_loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_loc C-Win
ON LEAVE OF end_loc IN FRAME FRAME-A /* Ending Warehouse */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_shift C-Win
ON LEAVE OF end_shift IN FRAME FRAME-A /* Ending Shift */
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


&Scoped-define SELF-NAME tb_pg-brk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_pg-brk C-Win
ON VALUE-CHANGED OF tb_pg-brk IN FRAME FRAME-A /* Print Department Page Break? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_tot-hrs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tot-hrs C-Win
ON VALUE-CHANGED OF tb_tot-hrs IN FRAME FRAME-A /* Print Total Hours? */
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
    
    ASSIGN
        begin_date  = TODAY
        end_date    = TODAY
        c-win:TITLE = IF ip-post THEN "Transfer WIP to Job Cost"
                             ELSE "WIP Edit List".

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "DT" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
  
    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
APPLY "entry" TO begin_date.
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
    DISPLAY begin_date end_date begin_mach end_mach begin_job-no begin_job-no2 
        end_job-no end_job-no2 begin_shift end_shift begin_loc end_loc 
        tb_tot-hrs tb_pg-brk rd-dest fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_date end_date begin_mach end_mach begin_job-no 
        begin_job-no2 end_job-no end_job-no2 begin_shift end_shift begin_loc 
        end_loc tb_tot-hrs tb_pg-brk rd-dest fi_file tb_OpenCSV tbAutoClose 
        btn-ok btn-cancel 
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
                (IF work-gl.job-no NE "" THEN "Job:" + STRING(work-gl.job-no) + "-" + STRING(work-gl.job-no2,"999") ELSE ""),
                "WIP"). 
            ASSIGN
                debits  = 0
                credits = 0.
        END.
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
    RUN scr-rpt-d.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCallOutboundAPI C-Win 
PROCEDURE pCallOutboundAPI PRIVATE :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdOutboundProcs AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lSuccess        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
    
    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.
    
    FOR EACH ttJob:
        RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs (
            INPUT  ttJob.company,              /* Company Code (Mandatory) */
            INPUT  ttJob.location,             /* Location Code (Mandatory) */
            INPUT  "SendJobAMS",               /* API ID (Mandatory) */
            INPUT  "",                         /* Scope ID */
            INPUT  "",                         /* Scope Type */
            INPUT  "PostDataCollection",       /* Trigger ID (Mandatory) */
            INPUT  "job",                      /* Comma separated list of table names for which data being sent (Mandatory) */
            INPUT  STRING(ttJob.jobRowID),     /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
            INPUT  ttJob.jobID + "-" + STRING(ttJob.jobID2),                 /* Primary ID for which API is called for (Mandatory) */   
            INPUT  "Triggered from pc/r-wippst.p",               /* Event's description (Optional) */
            OUTPUT lSuccess,                   /* Success/Failure flag */
            OUTPUT cMessage                    /* Status message */
            ).           
    END.
    
    FINALLY:
        DELETE PROCEDURE hdOutboundProcs.       
    END FINALLY.
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
        IF rd-dest:SCREEN-VALUE EQ "4" THEN
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
            fi_file:SCREEN-VALUE = "c:\tmp\TransferWIPtoJobCost.csv".   
    END.

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
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DO WITH FRAME {&frame-name}:
        lInvalid = NO.
    
        RUN GL_CheckModClosePeriod(INPUT cocode, INPUT DATE(TODAY), INPUT "WIP", OUTPUT cMessage, OUTPUT lSuccess ) .  
        IF NOT lSuccess THEN 
        DO:
            MESSAGE cMessage VIEW-AS ALERT-BOX INFORMATION.
            lInvalid = YES.
        END.       
    
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-wip C-Win 
PROCEDURE post-wip :
    /* --------------------------------------------------- pc/pc-post.p  8/94 gb  */
    /* Production Control - Posting Entry                                         */
    /* Modified By   : Aj  06/25/2008 Added code to post the data when complete   */
    /*                       flag is NO and job code category is RUN.             */
    /* -------------------------------------------------------------------------- */

    DEFINE VARIABLE v-loc      LIKE fg-bin.loc NO-UNDO.
    DEFINE VARIABLE v-loc-bin  LIKE fg-bin.loc-bin NO-UNDO.
    DEFINE VARIABLE v-up-hs    LIKE eb.num-up NO-UNDO.
    DEFINE VARIABLE v-up       LIKE eb.num-up NO-UNDO.
    DEFINE VARIABLE v-out      LIKE est-op.n-out NO-UNDO.
    DEFINE VARIABLE v-on       LIKE eb.num-up NO-UNDO.
    DEFINE VARIABLE v-est-type LIKE est.est-type NO-UNDO.
    DEFINE VARIABLE v-trnum    LIKE gl-ctrl.trnum NO-UNDO.

    DEFINE BUFFER b-mach FOR mach.

    EMPTY TEMP-TABLE ttJob.
    
    FOR EACH tt-report NO-LOCK,

        FIRST pc-prdd WHERE RECID(pc-prdd) EQ tt-report.rec-id,
        FIRST mach
        WHERE mach.company EQ cocode
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

        ASSIGN
            v-up  = 1
            v-out = 1
            v-on  = 1.

        IF AVAILABLE est AND INDEX("AP",mach.p-type) LE 0 THEN 
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
        IF pc-prdd.qty NE 0 AND (pc-prdd.complete OR  
            (CAN-FIND(FIRST job-code WHERE job-code.CODE = pc-prdd.CODE AND job-code.cat = "RUN")
            AND pc-prdd.COMPLETE = NO )) THEN 
        DO:

            RUN pc/pcprdd4u.p (ROWID(pc-prdd)).

            FOR EACH tt-job-hdr,

                FIRST itemfg
                WHERE itemfg.company    EQ cocode
                AND itemfg.i-no       EQ tt-job-hdr.i-no
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
                 
                /*if avail est and index("APB",mach.p-type) le 0 then do:
                  run sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, output v-up).
                           
                  find first est-op
                      where est-op.company eq est.company
                        and est-op.est-no  eq est.est-no
                        and est-op.s-num   eq pc-prdd.frm
                        and (est-op.b-num  eq pc-prdd.blank-no or
                             pc-prdd.blank-no eq 0)
                        and est-op.m-code  eq pc-prdd.m-code
                        and est-op.op-pass eq pc-prdd.pass
                        and est-op.dept    eq pc-prdd.dept
                        and est-op.line    lt 500
                      no-lock no-error.
                  if avail est-op and est-op.n-out ne 0 then v-out = est-op.n-out.
                end.*/
      
                IF AVAILABLE est AND INDEX("AP",mach.p-type) LE 0 THEN 
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
                        AND (est-op.b-num  EQ pc-prdd.blank-no OR pc-prdd.blank-no EQ 0)
                        AND est-op.m-code  EQ pc-prdd.m-code
                        AND est-op.op-pass EQ pc-prdd.pass
                        AND est-op.dept    EQ pc-prdd.dept
                        AND est-op.line    LT 500
                        NO-LOCK NO-ERROR.

                    IF ((AVAILABLE est-op) AND est-op.op-sb)           OR
                        ((NOT AVAILABLE est-op) AND mach.p-type NE "B") THEN 
                    DO:

                        IF AVAILABLE est-op THEN RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-out).
                        ELSE v-out = 1.
                        v-up = v-up * v-out.
                    END.
                    ELSE v-up = 1.

                    v-on = v-on / v-up.
                END.
                 
                ASSIGN
                    fg-rctd.b-num      = pc-prdd.blank-no
                    fg-rctd.s-num      = pc-prdd.frm
                    fg-rctd.t-qty      = pc-prdd.qty / v-up-hs * v-out * v-up
                    fg-rctd.pur-uom    = itemfg.prod-uom
                    fg-rctd.cost-uom   = itemfg.prod-uom
                    fg-rctd.std-cost   = tt-job-hdr.std-tot-cost
                    fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
                    fg-rctd.qty-case   = IF itemfg.case-count EQ 0 THEN 1 ELSE itemfg.case-count
                    fg-rctd.cases      = TRUNC(fg-rctd.t-qty / fg-rctd.qty-case,0)
                    fg-rctd.partial    = fg-rctd.t-qty - (fg-rctd.cases * fg-rctd.qty-case)
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
                        fg-rctd.loc     = fg-bin.loc
                        fg-rctd.loc-bin = fg-bin.loc-bin
                        fg-rctd.tag     = fg-bin.tag.
                
                ELSE 
                DO:
                    RUN fg/autopost.p (ROWID(itemfg), fg-rctd.job-no, fg-rctd.job-no2,
                        OUTPUT v-loc, OUTPUT v-loc-bin).

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

                    IF AVAILABLE fg-bin THEN
                        ASSIGN
                            fg-rctd.qty-case   = fg-bin.case-count
                            fg-rctd.cases-unit = fg-bin.cases-unit
                            fg-rctd.cases      = TRUNC(fg-rctd.t-qty / fg-rctd.qty-case,0).
                END.

                fg-rctd.partial = fg-rctd.t-qty - (fg-rctd.cases * fg-rctd.qty-case).

                RUN fg/comprcpt.p (ROWID(fg-rctd)).
            END.
        END. /* autopost*/

        /*
        /* task 11170511 */
        IF pc-prdd.code EQ "RUN"        AND
           fgrecpt-char NE "TSPARTS"    AND
           tspostfg-int EQ 1            AND
           INDEX("AP",mach.p-type) LE 0 THEN
        FOR EACH job-mch
            WHERE job-mch.company EQ cocode
              AND job-mch.job     EQ pc-prdd.job
              AND job-mch.job-no  EQ pc-prdd.job-no
              AND job-mch.job-no2 EQ pc-prdd.job-no2
              AND job-mch.frm     EQ pc-prdd.frm
            USE-INDEX line-idx NO-LOCK,
            FIRST b-mach
            WHERE b-mach.company EQ cocode
              AND b-mach.loc     EQ locode
              AND b-mach.m-code  EQ job-mch.m-code
              AND INDEX("AP",b-mach.p-type) GT 0
            NO-LOCK
            BY job-mch.line DESC:
      
          IF job-mch.m-code EQ pc-prdd.m-code THEN RUN proc-form-cmplt.
      
          LEAVE.
        END.  /* run */
        /* end of mods task 11170511*/
        */
        /* check if job exists for pc-prdd */
        IF CAN-FIND(FIRST job
            WHERE job.company EQ pc-prdd.company
            AND job.job     EQ pc-prdd.job
            AND job.job-no  EQ pc-prdd.job-no
            AND job.job-no2 EQ pc-prdd.job-no2) THEN
            RUN pTransferNotes (BUFFER pc-prdd).

        FIND FIRST ttJob
            WHERE ttJob.company EQ job.company
            AND ttJob.jobID   EQ job.job-no
            AND ttJob.jobID2  EQ job.job-no2
            NO-ERROR.
        IF NOT AVAILABLE ttJob THEN 
        DO:
            CREATE ttJob.
            ASSIGN
                ttJob.company  = job.company
                ttJob.jobID    = job.job-no
                ttJob.jobID2   = job.job-no2
                ttJob.location = job.loc
                ttJob.jobRowID = ROWID(job)
                .
        END.
        
        DELETE pc-prdd.
    END. /* for each pc-prdd */

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
        FIRST job
        WHERE job.company EQ cocode
        AND job.job     EQ w-job.job
        NO-LOCK:
        RUN jc/job-cls2.p (RECID(job)).
    END.
    
    RUN job_CloseJob_DCPost IN hdJobProcs(INPUT cocode, INPUT TABLE w-job).    
    
    RUN pCallOutboundAPI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-form-cmplt C-Win 
PROCEDURE proc-form-cmplt :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    /* from pc/pcprdd3u.p pcprdd4u.p */
    DEFINE VARIABLE v-est-type  LIKE est.est-type NO-UNDO.
    DEFINE VARIABLE v-loc       LIKE fg-bin.loc NO-UNDO.
    DEFINE VARIABLE v-loc-bin   LIKE fg-bin.loc-bin NO-UNDO.
    DEFINE VARIABLE v-qty       AS INTEGER NO-UNDO.
    DEFINE VARIABLE choice      AS LOG     NO-UNDO.
    DEFINE VARIABLE v-assembled AS LOG     NO-UNDO.
    DEFINE VARIABLE v-runqty    AS INTEGER NO-UNDO.
    DEFINE VARIABLE X           AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-up        AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-out       AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-up-hs     LIKE eb.num-up NO-UNDO.
    DEFINE VARIABLE v-on        LIKE eb.num-up NO-UNDO.
    DEFINE VARIABLE h_updbin    AS HANDLE  NO-UNDO.

    DEFINE BUFFER b-reftable FOR reftable.

    FIND FIRST job WHERE job.company EQ pc-prdd.company
        AND job.job-no  EQ pc-prdd.job-no
        AND job.job-no2 EQ pc-prdd.job-no2
        USE-INDEX job-no NO-LOCK NO-ERROR.

    FIND FIRST est
        WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
        NO-LOCK NO-ERROR.
    v-est-type = IF AVAILABLE est THEN est.est-type ELSE 1.

    IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

    v-assembled = NO.

    /* IF v-assembled THEN do for both assembled or unassembled */
    FOR EACH reftable
        WHERE reftable.reftable EQ "jc/jc-calc.p"
        AND reftable.company  EQ job-mch.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ STRING(job-mch.job,"999999999")
        AND reftable.val[12]  EQ job-mch.frm
        AND (reftable.val[13] EQ job-mch.blank-no OR
        job-mch.blank-no EQ 0),
        EACH job-hdr
        WHERE job-hdr.company   EQ cocode
        AND job-hdr.job-no    EQ job-mch.job-no
        AND job-hdr.job-no2   EQ job-mch.job-no2
        AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
        AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ),

        FIRST itemfg
        WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no    EQ reftable.code2 NO-LOCK:

        /*IF itemfg.isaset AND itemfg.alloc NE YES THEN DO:
          ASSIGN
           v-set  = itemfg.i-no
           v-qty  = pc-prdd.qty.
                
          RUN fg/checkset.p (RECID(itemfg), ?, INPUT-OUTPUT v-qty).
              
          IF v-qty LT pc-prdd.qty THEN DO:
            choice = NO.
            MESSAGE "Insufficient components for AUTOPOST, process anyway?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE choice.
            IF NOT choice THEN RETURN ERROR.
          END.
        END.*/    

        RUN fg/autopost.p (ROWID(itemfg), job-hdr.job-no, job-hdr.job-no2,
            OUTPUT v-loc, OUTPUT v-loc-bin).

        FIND FIRST fg-bin
            WHERE fg-bin.company EQ itemfg.company
            AND fg-bin.i-no    EQ itemfg.i-no
            AND fg-bin.loc     EQ v-loc
            AND fg-bin.loc-bin EQ v-loc-bin  
            AND fg-bin.tag     EQ ""
            AND fg-bin.job-no  EQ job-hdr.job-no
            AND fg-bin.job-no2 EQ job-hdr.job-no2
            NO-ERROR.
        IF NOT AVAILABLE fg-bin THEN 
        DO:
            CREATE fg-bin.
            ASSIGN
                fg-bin.company      = itemfg.company
                fg-bin.loc          = v-loc
                fg-bin.loc-bin      = v-loc-bin
                fg-bin.i-no         = reftable.code2
                fg-bin.tag          = ""
                fg-bin.job-no       = job-hdr.job-no
                fg-bin.job-no2      = job-hdr.job-no2
                fg-bin.std-mat-cost = reftable.val[2]
                fg-bin.std-lab-cost = reftable.val[1]
                fg-bin.std-fix-cost = reftable.val[4]
                fg-bin.std-var-cost = reftable.val[3]
                fg-bin.std-tot-cost = reftable.val[5]
                fg-bin.last-cost    = job-hdr.std-tot-cost
                fg-bin.unit-count   = itemfg.case-count.
        END.
      
        IF fg-bin.cases-unit   LE 0 THEN fg-bin.cases-unit   = 1.
        IF fg-bin.units-pallet LE 0 THEN fg-bin.units-pallet = 1.
    /*    
        FIND FIRST b-reftable
            WHERE b-reftable.reftable EQ "ts/jobdata.p"
              AND b-reftable.company  EQ cocode
              AND b-reftable.code     EQ STRING(RECID(job-hdr))
            EXCLUSIVE NO-ERROR.
        IF AVAIL b-reftable THEN DELETE b-reftable.
        CREATE b-reftable.
        ASSIGN
         b-reftable.reftable = "ts/jobdata.p"
         b-reftable.company  = cocode
         b-reftable.code     = STRING(RECID(job-hdr))
         b-reftable.code2    = STRING(RECID(fg-bin)).
        
        v-runqty = 0. 
        FOR EACH bf-prdd WHERE bf-prdd.company = pc-prdd.company 
                           AND bf-prdd.m-code = pc-prdd.m-code
                           AND bf-prdd.job-no = pc-prdd.job-no
                           AND bf-prdd.job-no2 = pc-prdd.job-no2
                           AND bf-prdd.FRM = pc-prdd.frm
                           AND bf-prdd.blank-no = pc-prdd.blank-no
                           AND bf-prdd.pass = pc-prdd.pass
                           NO-LOCK:
            v-runqty = v-runqty + bf-prdd.qty.
        END.                                      /*employee_code*/
        RUN addon/touch/d-updbin.w  (ROWID(fg-bin), v-runqty,'',cocode). /* pc-prdd.qty*/
    */    
    END.  /* v-assembled */
    /* === NEED more code later
      ELSE DO:     /* for unassembled sets
           THIS CODE WILL POST BOTH COMPONENTS AND SETS ON EVERY FORM, WHICH IS A BUG. 
           ADDITIONAL CODE MUST BE WRITTEN TO ONLY POST ON LAST OPERATION OF LAST FORM 
                   */     
            {addon/touch/jobbin.i}
      END.
    ===*/

    /*=========== create fg receipt : from pc/r-wippst.w */
    /*FOR EACH bf-machtran WHERE bf-machtran.company = cocode AND
                                        bf-machtran.machine = machine_code AND
                                        bf-machtran.job_number = job_number AND
                                        bf-machtran.job_sub = INTEGER(job_sub) AND
                                        bf-machtran.form_number = INTEGER(form_number) AND
                                        bf-machtran.blank_number = INTEGER(blank_number) AND
                                        bf-machtran.pass_sequence = INTEGER(pass_sequence) NO-LOCK:
                                        */
    FIND FIRST job WHERE job.company EQ cocode
        AND job.job-no  EQ pc-prdd.job-no
        AND job.job-no2 EQ pc-prdd.job-no2
        USE-INDEX job-no NO-ERROR.
    ASSIGN 
        v-up  = 1
        v-out = 1
        v-on  = 1.

    FIND FIRST est WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
        NO-LOCK NO-ERROR.
    v-est-type = IF AVAILABLE est THEN est.est-type ELSE 1.
    IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

    FIND FIRST mach WHERE mach.company = job-mch.company
        AND mach.m-code = job-mch.m-code NO-LOCK NO-ERROR.

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
        RUN update-plate-die ("P", v-est-type).

    IF mach.dept[1] EQ "DC" OR mach.dept[2] EQ "DC" OR
        mach.dept[3] EQ "DC" OR mach.dept[4] EQ "DC" THEN
        RUN update-plate-die ("D", v-est-type).

    IF AVAILABLE est AND INDEX("AP",mach.p-type) LE 0 THEN 
    DO:
        RUN sys/inc/numup.p (est.company, est.est-no, job-mch.frm, OUTPUT v-up).

        FIND FIRST ef
            WHERE ef.company EQ est.company
            AND ef.est-no  EQ est.est-no
            AND ef.form-no EQ job-mch.frm
            NO-LOCK NO-ERROR.

        IF AVAILABLE ef THEN 
        DO:
            RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).
            v-on = v-up * v-on.
        END.
                      
        FIND FIRST est-op
            WHERE est-op.company EQ est.company
            AND est-op.est-no  EQ est.est-no
            AND est-op.s-num   EQ job-mch.frm
            AND (est-op.b-num  EQ job-mch.blank-no OR job-mch.blank-no EQ 0)
            AND est-op.m-code  EQ job-mch.m-code
            AND est-op.op-pass EQ job-mch.pass
            AND est-op.dept    EQ job-mch.dept
            AND est-op.line    LT 500
            NO-LOCK NO-ERROR.

        IF ((AVAILABLE est-op) AND est-op.op-sb)           OR
            ((NOT AVAILABLE est-op) AND mach.p-type NE "B") THEN 
        DO:

            IF AVAILABLE est-op THEN RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-out).
            ELSE v-out = 1.
            v-up = v-up * v-out.
        END.
        ELSE v-up = 1.

        v-on = v-on / v-up.
    END.
           
    v-up-hs = 1.

    IF job-mch.dept EQ "HS" AND
        AVAILABLE est            AND
        mach.therm           AND
        mach.p-type EQ "S"   THEN
        RUN sys/inc/numup.p (est.company, est.est-no, job-mch.frm, OUTPUT v-up-hs).

    /* Don't create wip
       {touch/pcmchact.i}  /* from {pc/pcmchact.i}  mch-act creatation */
    */
 
    /* IF v-assembled THEN */
    IF pc-prdd.qty > 0 OR v-runqty > 0 THEN
        FOR EACH reftable
            WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job-mch.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job-mch.job,"999999999")
            AND reftable.val[12]  EQ job-mch.frm
            AND (reftable.val[13] EQ job-mch.blank-no OR
            job-mch.blank-no EQ 0),
            EACH job-hdr
            WHERE job-hdr.company   EQ cocode
            AND job-hdr.job-no    EQ job-mch.job-no
            AND job-hdr.job-no2   EQ job-mch.job-no2
            AND (job-hdr.frm      EQ job-mch.frm OR v-est-type <> 4)
            AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 OR v-est-type <> 4) ,
            FIRST itemfg
            WHERE itemfg.company    EQ cocode
            AND itemfg.i-no       EQ reftable.code2
            AND itemfg.case-count GT 0 NO-LOCK:

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
                fg-rctd.rct-date   = TODAY /*c-prdd.op-date*/
                fg-rctd.trans-time = TIME
                fg-rctd.company    = job-hdr.company
                fg-rctd.rita-code  = "R"
                fg-rctd.i-name     = itemfg.i-name
                fg-rctd.i-no       = reftable.code2
                fg-rctd.job-no     = job-hdr.job-no
                fg-rctd.job-no2    = job-hdr.job-no2.
                 
            ASSIGN
                v-up  = 1
                v-out = 1.
      
            IF AVAILABLE est AND INDEX("APB",mach.p-type) LE 0 THEN 
            DO:
                RUN sys/inc/numup.p (est.company, est.est-no, job-mch.frm, OUTPUT v-up).
                 
                FIND FIRST est-op
                    WHERE est-op.company EQ est.company
                    AND est-op.est-no  EQ est.est-no
                    AND est-op.s-num   EQ job-hdr.frm
                    AND (est-op.b-num  EQ job-hdr.blank-no OR
                    job-hdr.blank-no EQ 0)
                    AND est-op.m-code  EQ job-mch.m-code
                    AND est-op.op-pass EQ job-mch.pass
                    AND est-op.dept    EQ job-mch.dept
                    AND est-op.line    LT 500
                    NO-LOCK NO-ERROR.
                IF AVAILABLE est-op AND est-op.n-out NE 0 THEN v-out = est-op.n-out.
            END.

            ASSIGN
                fg-rctd.b-num      = reftable.val[13]
                fg-rctd.s-num      = reftable.val[12]
                fg-rctd.t-qty      = (IF pc-prdd.qty = 0 THEN v-runqty ELSE pc-prdd.qty)
                / v-up-hs * v-out * v-up  /*v-runqty*/
                fg-rctd.pur-uom    = itemfg.prod-uom
                fg-rctd.cost-uom   = itemfg.prod-uom
                fg-rctd.std-cost   = reftable.val[5]
                fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
                fg-rctd.qty-case   = itemfg.case-count
                fg-rctd.partial    = fg-rctd.t-qty MODULO itemfg.case-count
                fg-rctd.cases      = trunc(fg-rctd.t-qty / itemfg.case-count,0)
                fg-rctd.cases-unit = 1.

            IF fg-rctd.t-qty LE 0 THEN fg-rctd.cases = 0.

            RELEASE fg-bin.
      
            FIND FIRST b-reftable
                WHERE b-reftable.reftable EQ "ts/jobdata.p"
                AND b-reftable.company  EQ cocode
                AND b-reftable.code     EQ job-hdr.rec_key
                NO-LOCK NO-ERROR.

            IF AVAILABLE b-reftable THEN 
                FIND FIRST fg-bin WHERE fg-bin.rec_key EQ b-reftable.code2 NO-LOCK NO-ERROR.
      
            IF AVAILABLE fg-bin THEN
                ASSIGN
                    v-loc       = fg-bin.loc
                    v-loc-bin   = fg-bin.loc-bin
                    fg-rctd.tag = fg-bin.tag.
                
            ELSE
                RUN fg/autopost.p (ROWID(itemfg), fg-rctd.job-no, fg-rctd.job-no2,
                    OUTPUT v-loc, OUTPUT v-loc-bin).

            ASSIGN
                fg-rctd.loc     = v-loc
                fg-rctd.loc-bin = v-loc-bin.

            FIND FIRST fg-bin
                WHERE fg-bin.company EQ fg-rctd.company
                AND fg-bin.i-no    EQ fg-rctd.i-no
                AND fg-bin.job-no  EQ job-hdr.job-no
                AND fg-bin.job-no2 EQ job-hdr.job-no2
                AND fg-bin.loc     EQ fg-rctd.loc
                AND fg-bin.loc-bin EQ fg-rctd.loc-bin
                AND fg-bin.tag     EQ fg-rctd.tag
                NO-LOCK NO-ERROR.

            IF AVAILABLE fg-bin AND fg-bin.cases-unit <> 0 THEN fg-rctd.cases-unit = fg-bin.cases-unit.

            RUN fg/comprcpt.p (ROWID(fg-rctd)).
        END. /* v-assembled */
    /*====
        ELSE DO:
           FOR EACH job-hdr WHERE job-hdr.company   EQ cocode
              AND job-hdr.job-no    EQ job-mch.job-no
              AND job-hdr.job-no2   EQ job-mch.job-no2         
              AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
              AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ) :
    
              FIND first itemfg where itemfg.company    eq cocode
                            and itemfg.i-no       eq job-hdr.i-no
                            and itemfg.case-count gt 0
                            NO-LOCK NO-ERROR.
              IF NOT AVAIL itemfg THEN NEXT.
              {addon/touch/jobrcpt.i}
    ===    END.
      END.  /* for each job-hdr */
    ===    */

    /* end of fg receipt creation */
 
    RELEASE fg-rctd.
    RELEASE job.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-set-cmplt C-Win 
PROCEDURE proc-set-cmplt :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    /* from pc/pcprdd3u.p pcprdd4u.p */
    DEFINE VARIABLE v-est-type  LIKE est.est-type NO-UNDO.
    DEFINE VARIABLE v-loc       LIKE fg-bin.loc NO-UNDO.
    DEFINE VARIABLE v-loc-bin   LIKE fg-bin.loc-bin NO-UNDO.
    DEFINE VARIABLE v-qty       AS INTEGER NO-UNDO.
    DEFINE VARIABLE choice      AS LOG     NO-UNDO.
    DEFINE VARIABLE v-assembled AS LOG     NO-UNDO.
    DEFINE VARIABLE v-runqty    AS INTEGER NO-UNDO.
    DEFINE VARIABLE X           AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-up        AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-out       AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-up-hs     LIKE eb.num-up NO-UNDO.
    DEFINE VARIABLE v-on        LIKE eb.num-up NO-UNDO.
    DEFINE VARIABLE h_updbin    AS HANDLE  NO-UNDO.
   
    DEFINE BUFFER b-reftable FOR reftable.

    FIND FIRST job WHERE job.company EQ cocode
        AND job.job-no  EQ pc-prdd.job-no
        AND job.job-no2 EQ pc-prdd.job-no2
        USE-INDEX job-no NO-LOCK NO-ERROR.

    FIND FIRST est
        WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
        NO-LOCK NO-ERROR.
    v-est-type = IF AVAILABLE est THEN est.est-type ELSE 1.

    IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

    v-assembled = NO.

    /* IF v-assembled THEN do for both assembled or unassembled */
    /* FOR EACH reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job-mch.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job-mch.job,"999999999")
            AND reftable.val[12]  EQ job-mch.frm
            AND (reftable.val[13] EQ job-mch.blank-no OR
                 job-mch.blank-no EQ 0),
        */         

    FOR EACH job-hdr
        WHERE job-hdr.company   EQ cocode
        AND job-hdr.job-no    EQ job-mch.job-no
        AND job-hdr.job-no2   EQ job-mch.job-no2
        AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
        AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ),

        FIRST itemfg
        WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no    EQ job-hdr.i-no NO-LOCK:

        /*IF itemfg.isaset AND itemfg.alloc NE YES THEN DO:
          ASSIGN
           v-set  = itemfg.i-no
           v-qty  = pc-prdd.qty.
                
          RUN fg/checkset.p (RECID(itemfg), ?, INPUT-OUTPUT v-qty).
              
          IF v-qty LT pc-prdd.qty THEN DO:
            choice = NO.
            MESSAGE "Insufficient components for AUTOPOST, process anyway?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE choice.
            IF NOT choice THEN RETURN ERROR.
          END.
        END.*/    

        RUN fg/autopost.p (ROWID(itemfg), job-hdr.job-no, job-hdr.job-no2,
            OUTPUT v-loc, OUTPUT v-loc-bin).

        FIND FIRST fg-bin
            WHERE fg-bin.company EQ itemfg.company
            AND fg-bin.i-no    EQ itemfg.i-no
            AND fg-bin.loc     EQ v-loc
            AND fg-bin.loc-bin EQ v-loc-bin  
            AND fg-bin.tag     EQ ""
            AND fg-bin.job-no  EQ job-hdr.job-no
            AND fg-bin.job-no2 EQ job-hdr.job-no2
            NO-ERROR.
        IF NOT AVAILABLE fg-bin THEN 
        DO:
            CREATE fg-bin.
            ASSIGN
                fg-bin.company      = itemfg.company
                fg-bin.loc          = v-loc
                fg-bin.loc-bin      = v-loc-bin
                fg-bin.i-no         = job-hdr.i-no 
                fg-bin.tag          = ""
                fg-bin.job-no       = job-hdr.job-no
                fg-bin.job-no2      = job-hdr.job-no2
                fg-bin.std-mat-cost = job-hdr.std-mat-cost
                fg-bin.std-lab-cost = job-hdr.std-lab-cost
                fg-bin.std-fix-cost = job-hdr.std-fix-cost
                fg-bin.std-var-cost = job-hdr.std-var-cost
                fg-bin.std-tot-cost = job-hdr.std-tot-cost
                fg-bin.last-cost    = job-hdr.std-tot-cost
                fg-bin.unit-count   = itemfg.case-count.
        END.
      
        IF fg-bin.cases-unit   LE 0 THEN fg-bin.cases-unit   = 1.
        IF fg-bin.units-pallet LE 0 THEN fg-bin.units-pallet = 1.
    
    /*  FIND FIRST b-reftable
          WHERE b-reftable.reftable EQ "ts/jobdata.p"
            AND b-reftable.company  EQ cocode
            AND b-reftable.code     EQ STRING(RECID(job-hdr))
          EXCLUSIVE NO-ERROR.
      IF AVAIL b-reftable THEN DELETE b-reftable.
      CREATE b-reftable.
      ASSIGN
       b-reftable.reftable = "ts/jobdata.p"
       b-reftable.company  = cocode
       b-reftable.code     = STRING(RECID(job-hdr))
       b-reftable.code2    = STRING(RECID(fg-bin)).
      
      v-runqty = 0. 
      FOR EACH bf-prdd WHERE bf-prdd.company = pc-prdd.company 
                         AND bf-prdd.m-code = pc-prdd.m-code
                         AND bf-prdd.job-no = pc-prdd.job-no
                         AND bf-prdd.job-no2 = pc-prdd.job-no2
                         AND bf-prdd.FRM = pc-prdd.frm
                         AND bf-prdd.blank-no = pc-prdd.blank-no
                         AND bf-prdd.pass = pc-prdd.pass
                         NO-LOCK:
          v-runqty = v-runqty + bf-prdd.qty.
      END.                                      /*employee_code*/
      RUN addon/touch/d-updbin.w  (ROWID(fg-bin), v-runqty,'',cocode). /* pc-prdd.qty*/
  */    
    END.  /* v-assembled */

    /* === NEED more code later
      ELSE DO:     /* for unassembled sets
           THIS CODE WILL POST BOTH COMPONENTS AND SETS ON EVERY FORM, WHICH IS A BUG. 
           ADDITIONAL CODE MUST BE WRITTEN TO ONLY POST ON LAST OPERATION OF LAST FORM 
                   */     
            {addon/touch/jobbin.i}
      END.
    ===*/

    /*=========== create fg receipt : from pc/r-wippst.w */
    /*FOR EACH bf-machtran WHERE bf-machtran.company = cocode AND
                                        bf-machtran.machine = machine_code AND
                                        bf-machtran.job_number = job_number AND
                                        bf-machtran.job_sub = INTEGER(job_sub) AND
                                        bf-machtran.form_number = INTEGER(form_number) AND
                                        bf-machtran.blank_number = INTEGER(blank_number) AND
                                        bf-machtran.pass_sequence = INTEGER(pass_sequence) NO-LOCK:
                                        */
    FIND FIRST job WHERE job.company EQ cocode
        AND job.job-no  EQ pc-prdd.job-no
        AND job.job-no2 EQ pc-prdd.job-no2
        USE-INDEX job-no NO-ERROR.
    ASSIGN 
        v-up  = 1
        v-out = 1
        v-on  = 1.

    FIND FIRST est WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
        NO-LOCK NO-ERROR.
    v-est-type = IF AVAILABLE est THEN est.est-type ELSE 1.
    IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

    FIND FIRST mach WHERE mach.company = job-mch.company
        AND mach.m-code = job-mch.m-code NO-LOCK NO-ERROR.

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
        RUN update-plate-die ("P", v-est-type).

    IF mach.dept[1] EQ "DC" OR mach.dept[2] EQ "DC" OR
        mach.dept[3] EQ "DC" OR mach.dept[4] EQ "DC" THEN
        RUN update-plate-die ("D", v-est-type).

    IF AVAILABLE est AND INDEX("AP",mach.p-type) LE 0 THEN 
    DO:
        RUN sys/inc/numup.p (est.company, est.est-no, job-mch.frm, OUTPUT v-up).

        FIND FIRST ef
            WHERE ef.company EQ est.company
            AND ef.est-no  EQ est.est-no
            AND ef.form-no EQ job-mch.frm
            NO-LOCK NO-ERROR.

        IF AVAILABLE ef THEN
            v-on = v-up *
                (IF ef.n-out   EQ 0 THEN 1 ELSE ef.n-out) *
                (IF ef.n-out-l EQ 0 THEN 1 ELSE ef.n-out-l) *
                (IF ef.n-out-d EQ 0 THEN 1 ELSE ef.n-out-d).
                      
        FIND FIRST est-op
            WHERE est-op.company EQ est.company
            AND est-op.est-no  EQ est.est-no
            AND est-op.s-num   EQ job-mch.frm
            AND (est-op.b-num  EQ job-mch.blank-no OR job-mch.blank-no EQ 0)
            AND est-op.m-code  EQ job-mch.m-code
            AND est-op.op-pass EQ job-mch.pass
            AND est-op.dept    EQ job-mch.dept
            AND est-op.line    LT 500
            NO-LOCK NO-ERROR.

        IF ((AVAILABLE est-op) AND est-op.op-sb)           OR
            ((NOT AVAILABLE est-op) AND mach.p-type NE "B") THEN 
        DO:

            IF AVAILABLE est-op THEN RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-out).
            ELSE v-out = 1.
            v-up = v-up * v-out.
        END.
        ELSE v-up = 1.

        v-on = v-on / v-up.
    END.
           
    v-up-hs = 1.

    IF job-mch.dept EQ "HS" AND
        AVAILABLE est            AND
        mach.therm           AND
        mach.p-type EQ "S"   THEN
        RUN sys/inc/numup.p (est.company, est.est-no, job-mch.frm, OUTPUT v-up-hs).

    /* Don't create wip
       {touch/pcmchact.i}  /* from {pc/pcmchact.i}  mch-act creatation */
    */

    /* IF v-assembled THEN */
    IF pc-prdd.qty > 0 OR v-runqty > 0 THEN
        /*FOR EACH reftable
            WHERE reftable.reftable EQ "jc/jc-calc.p"
              AND reftable.company  EQ job-mch.company
              AND reftable.loc      EQ ""
              AND reftable.code     EQ STRING(job-mch.job,"999999999")
              AND reftable.val[12]  EQ job-mch.frm
              AND (reftable.val[13] EQ job-mch.blank-no OR
                   job-mch.blank-no EQ 0), */
        FOR EACH job-hdr
            WHERE job-hdr.company   EQ cocode
            AND job-hdr.job-no    EQ job-mch.job-no
            AND job-hdr.job-no2   EQ job-mch.job-no2
            AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
            AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ) ,
            FIRST itemfg
            WHERE itemfg.company    EQ cocode
            AND itemfg.i-no       EQ job-hdr.i-no
            AND itemfg.case-count GT 0 NO-LOCK:

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
                fg-rctd.rct-date   = TODAY /*c-prdd.op-date*/
                fg-rctd.trans-time = TIME
                fg-rctd.company    = job-hdr.company
                fg-rctd.rita-code  = "R"
                fg-rctd.i-name     = itemfg.i-name
                fg-rctd.i-no       = job-hdr.i-no
                fg-rctd.job-no     = job-hdr.job-no
                fg-rctd.job-no2    = job-hdr.job-no2.
                 
            ASSIGN
                v-up  = 1
                v-out = 1.
      
            IF AVAILABLE est AND index("AB",mach.p-type) LE 0 THEN 
            DO:
                RUN sys/inc/numup.p (est.company, est.est-no, job-mch.frm, OUTPUT v-up).
                 
                FIND FIRST est-op
                    WHERE est-op.company EQ est.company
                    AND est-op.est-no  EQ est.est-no
                    AND est-op.s-num   EQ job-hdr.frm
                    AND (est-op.b-num  EQ job-hdr.blank-no OR
                    job-hdr.blank-no EQ 0)
                    AND est-op.m-code  EQ job-mch.m-code
                    AND est-op.op-pass EQ job-mch.pass
                    AND est-op.dept    EQ job-mch.dept
                    AND est-op.line    LT 500
                    NO-LOCK NO-ERROR.
                IF AVAILABLE est-op AND est-op.n-out NE 0 THEN v-out = est-op.n-out.
            END.

            ASSIGN
                fg-rctd.b-num      = job-mch.blank-no
                fg-rctd.s-num      = job-mch.frm
                fg-rctd.t-qty      = (IF pc-prdd.qty = 0 THEN v-runqty ELSE pc-prdd.qty) 
                / v-up-hs * v-out * v-up  /*v-runqty*/
                fg-rctd.pur-uom    = itemfg.prod-uom
                fg-rctd.cost-uom   = itemfg.prod-uom
                fg-rctd.std-cost   = job-hdr.std-tot-cost
                fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
                fg-rctd.qty-case   = itemfg.case-count
                fg-rctd.partial    = fg-rctd.t-qty MODULO itemfg.case-count
                fg-rctd.cases      = trunc(fg-rctd.t-qty / itemfg.case-count,0)
                fg-rctd.cases-unit = 1.

            IF fg-rctd.t-qty LE 0 THEN fg-rctd.cases = 0.

            RELEASE fg-bin.
      
            FIND FIRST b-reftable
                WHERE b-reftable.reftable EQ "ts/jobdata.p"
                AND b-reftable.company  EQ cocode
                AND b-reftable.code     EQ job-hdr.rec_key
                NO-LOCK NO-ERROR.

            IF AVAILABLE b-reftable THEN 
                FIND FIRST fg-bin WHERE fg-bin.rec_key EQ b-reftable.code2 NO-LOCK NO-ERROR.
      
            IF AVAILABLE fg-bin THEN
                ASSIGN
                    v-loc       = fg-bin.loc
                    v-loc-bin   = fg-bin.loc-bin
                    fg-rctd.tag = fg-bin.tag.
                
            ELSE
                RUN fg/autopost.p (ROWID(itemfg), fg-rctd.job-no, fg-rctd.job-no2,
                    OUTPUT v-loc, OUTPUT v-loc-bin).

            ASSIGN
                fg-rctd.loc     = v-loc
                fg-rctd.loc-bin = v-loc-bin.

            FIND FIRST fg-bin
                WHERE fg-bin.company EQ fg-rctd.company
                AND fg-bin.i-no    EQ fg-rctd.i-no
                AND fg-bin.job-no  EQ job-hdr.job-no
                AND fg-bin.job-no2 EQ job-hdr.job-no2
                AND fg-bin.loc     EQ fg-rctd.loc
                AND fg-bin.loc-bin EQ fg-rctd.loc-bin
                AND fg-bin.tag     EQ fg-rctd.tag
                NO-LOCK NO-ERROR.

            IF AVAILABLE fg-bin AND fg-bin.cases-unit <> 0 THEN fg-rctd.cases-unit = fg-bin.cases-unit.

            RUN fg/comprcpt.p (ROWID(fg-rctd)).
        END. /* v-assembled */
    /*====
        ELSE DO:
           FOR EACH job-hdr WHERE job-hdr.company   EQ cocode
              AND job-hdr.job-no    EQ job-mch.job-no
              AND job-hdr.job-no2   EQ job-mch.job-no2         
              AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
              AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ) :
    
              FIND first itemfg where itemfg.company    eq cocode
                            and itemfg.i-no       eq job-hdr.i-no
                            and itemfg.case-count gt 0
                            NO-LOCK NO-ERROR.
              IF NOT AVAIL itemfg THEN NEXT.
              {addon/touch/jobrcpt.i}
    ===    END.
      END.  /* for each job-hdr */
    ===    */

    /* end of fg receipt creation */
 
    RELEASE fg-rctd.
    RELEASE job.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTransferNotes C-Win 
PROCEDURE pTransferNotes :
    /*------------------------------------------------------------------------------
     Purpose: if notes exist in pc-prdh and/or pc-prdd then transfer
     Notes:   them to the associated job.
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER bPCPrdd FOR pc-prdd.

    DEFINE           BUFFER bJob    FOR job.

    FIND FIRST bJob NO-LOCK
        WHERE bJob.company EQ bPCPrdd.company
        AND bJob.job     EQ bPCPrdd.job
        AND bJob.job-no  EQ bPCPrdd.job-no
        AND bJob.job-no2 EQ bPCPrdd.job-no2
        NO-ERROR.
    IF AVAILABLE bJob THEN 
    DO:
        FIND FIRST pc-prdh NO-LOCK
            WHERE pc-prdh.company    EQ bPCPrdd.company
            AND pc-prdh.m-code     EQ bPCPrdd.m-code
            AND pc-prdh.trans-date EQ bPCPrdd.op-date
            AND pc-prdh.shift      EQ bPCPrdd.shift
            NO-ERROR.
        IF AVAILABLE pc-prdh THEN
            RUN pUpdateNotesRecKey (pc-prdh.rec_key, bJob.rec_key).
        RUN pUpdateNotesRecKey (bPCPrdd.rec_key, bJob.rec_key).
    END. /* avail job */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateNotesRecKey C-Win 
PROCEDURE pUpdateNotesRecKey :
    /*------------------------------------------------------------------------------
     Purpose: set the notes rec_key value from pc-prdh and/or pc-prdd rec_key
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPCPrdRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobRecKey   AS CHARACTER NO-UNDO.

    IF CAN-FIND(FIRST notes
        WHERE notes.rec_key EQ ipcPCPrdRecKey) THEN
    DO TRANSACTION:
        FIND FIRST notes EXCLUSIVE-LOCK
            WHERE notes.rec_key EQ ipcPCPrdRecKey
            NO-ERROR.
        IF AVAILABLE notes THEN
            notes.rec_key = ipcJobRecKey.
        RELEASE notes.
    END. /* do trans */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ----------------------------------------------- pc/rep/mch-edit.p 8/94 gb  */
/* Production Control -transactions edit list                                 */
/* -------------------------------------------------------------------------- */

    {sys/form/r-topw.f}

    DEFINE VARIABLE v-date           LIKE pc-prdd.op-date EXTENT 2 FORMAT "99/99/9999" INIT TODAY NO-UNDO.
    DEFINE VARIABLE v-job-no         LIKE job.job-no EXTENT 2 INIT ["","zzzzzzzzz"] NO-UNDO.
    DEFINE VARIABLE v-job-no2        LIKE job.job-no2 EXTENT 2 FORMAT "999" INIT [0,999] NO-UNDO.
    DEFINE VARIABLE v-m-code         LIKE mach.m-code EXTENT 2 INIT ["","zzzzzz"] NO-UNDO. 
    DEFINE VARIABLE cLoc             AS CHARACTER EXTENT 2 INIT ["","zzzzz"] NO-UNDO. 
    DEFINE VARIABLE v-shift          LIKE pc-prdd.shift EXTENT 2 NO-UNDO.

    DEFINE VARIABLE doe              AS LOG       INIT TRUE.
    DEFINE VARIABLE dor              AS LOG       INIT TRUE.
    DEFINE VARIABLE detail           AS LOG       INIT FALSE.
    DEFINE VARIABLE v-value          AS DECIMAL   FORMAT "->>,>>>,>>9.99".
    DEFINE VARIABLE v-toth           AS LOG       INIT TRUE.
    DEFINE VARIABLE tothour          LIKE pc-prdd.hours.
    DEFINE VARIABLE totqty           LIKE pc-prdd.qty.
    DEFINE VARIABLE totwst           LIKE pc-prdd.waste.
    DEFINE VARIABLE uline            AS CHARACTER FORMAT "x(44)".
    DEFINE VARIABLE totchar          AS CHARACTER FORMAT "x(15)" INIT "Machine Totals:".
    DEFINE VARIABLE v-start          AS CHARACTER FORMAT "x(5)".
    DEFINE VARIABLE v-stopp          AS CHARACTER FORMAT "x(5)".
    DEFINE VARIABLE v-comp           AS CHARACTER FORMAT "x".
    DEFINE VARIABLE v-dept-paging    AS LOG       INIT NO.
    DEFINE VARIABLE v-recid          AS RECID.
    DEFINE VARIABLE v-tot-rm         LIKE mat-act.qty NO-UNDO.
    DEFINE VARIABLE v-tot-fg         LIKE mch-act.qty NO-UNDO.     
    DEFINE VARIABLE v-up-hs          LIKE eb.num-up NO-UNDO.
    DEFINE VARIABLE v-up             LIKE eb.num-up NO-UNDO.
    DEFINE VARIABLE v-out            LIKE est-op.n-out NO-UNDO.
    DEFINE VARIABLE v-on             LIKE eb.num-up NO-UNDO.
    DEFINE VARIABLE v-est-type       LIKE est.est-type NO-UNDO.
    DEFINE VARIABLE ll-one-item      AS LOG       NO-UNDO.
    DEFINE VARIABLE ld               AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE vmr-crusiz       LIKE mach.mr-crusiz.
    DEFINE VARIABLE vrun-crusiz      LIKE mach.run-crusiz.
    DEFINE VARIABLE v-dscr           LIKE account.dscr.
    DEFINE VARIABLE v-disp-actnum    LIKE account.actnum.
    DEFINE VARIABLE v-disp-amt       AS DECIMAL   FORMAT ">>,>>>,>>9.99cr".
    DEFINE VARIABLE v-disp-job       LIKE work-gl.job-no.
    DEFINE VARIABLE excelheader      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCurrentTitle    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCurrentMessage  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuppressMessage AS LOGICAL   NO-UNDO.

    FORM
        pc-prdd.m-code COLUMN-LABEL "MACH"
        mach.m-dscr COLUMN-LABEL "DESCRIPT" FORMAT "x(10)"
        mach.dept[1] COLUMN-LABEL "DP"
        pc-prdd.op-date COLUMN-LABEL "DATE" FORMAT "99/99/99"
        SPACE(0)
        pc-prdd.shift COLUMN-LABEL "SH" FORMAT ">>"
        pc-prdd.job-no COLUMN-LABEL "  JOB #" SPACE(0) "-" SPACE(0)
        pc-prdd.job-no2 NO-LABELS FORMAT "999"
        pc-prdd.frm COLUMN-LABEL " S" SPACE(0) "/" SPACE(0)
        pc-prdd.blank-no COLUMN-LABEL "/B"
        pc-prdd.pass COLUMN-LABEL "P"
        job-hdr.i-no COLUMN-LABEL "ITEM #"
        itemfg.i-name COLUMN-LABEL "ITEM DESCRIPTION" FORMAT "x(15)" 
        pc-prdd.code COLUMN-LABEL "CODE" 
        pc-prdd.hours COLUMN-LABEL "HOURS "
        v-start COLUMN-LABEL "START"
        v-stopp COLUMN-LABEL " STOP"
        pc-prdd.crew COLUMN-LABEL "CR" FORMAT ">9"
        pc-prdd.qty FORMAT "->>>>>>>9" COLUMN-LABEL "RUN QTY"
        pc-prdd.waste FORMAT "->>>>9" COLUMN-LABEL "WASTE"
        v-comp COLUMN-LABEL "C"
        WITH FRAME mch-edit NO-BOX DOWN STREAM-IO WIDTH 142.

    FORM v-disp-actnum  LABEL "G/L ACCOUNT NUMBER"
        v-dscr         LABEL "DESCRIPTION"
        v-disp-job     LABEL "Job#" FORMAT "x(13)"
        v-dscr         LABEL "DESCRIPTION"
        udate          LABEL "DATE"   
        v-disp-amt     LABEL "AMOUNT" SKIP

        WITH DOWN STREAM-IO WIDTH 136 FRAME gldetail.


    ASSIGN
        str-tit2      = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}
        v-m-code[1]   = begin_mach
        v-m-code[2]   = end_mach
        v-date[1]     = begin_date
        v-date[2]     = end_date
        v-job-no[1]   = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', begin_job-no, begin_job-no2)) 
        v-job-no[2]   = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', end_job-no, end_job-no2)) 
        v-shift[1]    = begin_shift
        v-shift[2]    = end_shift
        v-toth        = tb_tot-hrs
        v-dept-paging = tb_pg-brk
        cLoc[1]       = begin_loc
        cLoc[2]       = end_loc.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF rd-dest = 4 THEN 
    DO:                                    
        OUTPUT STREAM excel TO VALUE(cFileName).                        /*Task# 02101407*/                  
        excelheader = "MACH,DESCRIPT,DP,DATE,SH,JOB #,"
            + "F,B,P,ITEM #,ITEM DESCRIPTION,CODE,"
            + "HOURS,START,STOP,CR,RUN QTY,WASTE,C".
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.

    DISPLAY "" WITH FRAME r-top.

    FOR EACH pc-prdd
        WHERE pc-prdd.company EQ cocode
        AND pc-prdd.m-code  GE v-m-code[1]
        AND pc-prdd.m-code  LE v-m-code[2]
        AND pc-prdd.op-date GE v-date[1]
        AND pc-prdd.op-date LE v-date[2]
        AND pc-prdd.shift   GE v-shift[1]
        AND pc-prdd.shift   LE v-shift[2]         
        AND FILL(" ", iJobLen - length(TRIM(pc-prdd.job-no))) +
        trim(pc-prdd.job-no) + string(int(pc-prdd.job-no2),"999")
        GE v-job-no[1]
        AND FILL(" ", iJobLen - length(TRIM(pc-prdd.job-no))) +
        trim(pc-prdd.job-no) + string(int(pc-prdd.job-no2),"999")
        LE v-job-no[2]   
        AND ((pc-prdd.stopp - pc-prdd.start
        NE 0) OR
        (pc-prdd.qty   NE 0) OR
        (pc-prdd.waste NE 0))
        NO-LOCK,
          
        FIRST mach
        WHERE mach.company EQ cocode
        AND mach.m-code EQ pc-prdd.m-code
        AND mach.loc  GE cLoc[1]
        AND mach.loc  LE cLoc[2]
        NO-LOCK,

        FIRST job
        WHERE job.company EQ cocode
        AND job.job     EQ pc-prdd.job
        AND job.job-no  EQ pc-prdd.job-no
        AND job.job-no2 EQ pc-prdd.job-no2
        NO-LOCK:

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

        IF v-rm-fg THEN 
        DO:      /* validate rm > fg produced + waste */
            ASSIGN
                v-tot-rm = 0
                v-tot-fg = (pc-prdd.qty + pc-prdd.waste) / v-up-hs.

            FOR EACH mch-act
                WHERE mch-act.company  EQ cocode
                AND mch-act.job      EQ job.job
                AND mch-act.job-no   EQ job.job-no
                AND mch-act.job-no2  EQ job.job-no2
                AND mch-act.frm      EQ pc-prdd.frm
                AND mch-act.blank-no EQ pc-prdd.blank-no
                AND mch-act.pass     EQ pc-prdd.pass
                AND mch-act.m-code   EQ pc-prdd.m-code
                USE-INDEX job NO-LOCK:
                v-tot-fg = v-tot-fg + (mch-act.qty + mch-act.waste).
            END.

            v-tot-fg = v-tot-fg / v-on.

            RELEASE job-mat.
            FOR EACH job-mat
                WHERE job-mat.company EQ cocode
                AND job-mat.job     EQ job.job
                AND job-mat.job-no  EQ job.job-no
                AND job-mat.job-no2 EQ job.job-no2
                AND job-mat.frm     EQ pc-prdd.frm
                NO-LOCK,
                FIRST item
                WHERE item.company    EQ cocode
                AND item.i-no       EQ job-mat.i-no
                AND item.mat-type   EQ "B"
                AND item.i-code     EQ "R"
                NO-LOCK:
                LEAVE.
            END.

            IF AVAILABLE job-mat THEN 
            DO:
                FOR EACH mat-act
                    WHERE mat-act.company EQ cocode
                    AND mat-act.job     EQ job.job
                    AND mat-act.job-no  EQ job.job-no
                    AND mat-act.job-no2 EQ job.job-no2
                    AND mat-act.i-no    EQ job-mat.i-no
                    AND mat-act.s-num   EQ job-mat.frm
                    AND mat-act.b-num   EQ job-mat.blank-no
                    USE-INDEX job NO-LOCK:
                    v-tot-rm = v-tot-rm + mat-act.qty.
                END.

                IF v-tot-fg GT v-tot-rm THEN 
                DO:
                    RUN displayMessage ("6").
                    NEXT.
                END.
            END.
        END.
        
        CREATE tt-report.
        tt-report.rec-id = RECID(pc-prdd).
  
    END.
 
    IF v-dept-paging THEN 
    DO:
    {pc/rep/mch-edit2.i pc-prdd.dept "by pc-prdd.m-code"}
    END.

    ELSE 
    DO:
    {pc/rep/mch-edit2.i pc-prdd.m-code}
    END.

    IF dcpostgl-log THEN
        FOR EACH work-gl BY work-gl.actnum BY work-gl.job-no BY work-gl.job-no2:
            FIND FIRST account
                WHERE account.company EQ cocode
                AND account.actnum  EQ work-gl.actnum
                NO-LOCK NO-ERROR.
        
            ASSIGN
                v-dscr        = IF AVAILABLE account THEN account.dscr
                   ELSE "ACCOUNT NOT FOUND - " + work-gl.actnum
                v-disp-actnum = work-gl.actnum
                v-disp-job    = TRIM(work-gl.job-no) + "-" + STRING(work-gl.job-no2,"999")
                v-disp-amt    = work-gl.debits - work-gl.credits.

            DISPLAY v-disp-actnum v-dscr v-disp-job udate v-disp-amt
                WITH FRAME gldetail.
            DOWN WITH FRAME gldetail.
        END. /* each work-job */

    IF rd-dest = 4 THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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
                        /* gdm - 10230906 */
                        prep.last-order        = job-hdr.ord-no
                        prep.last-job-no       = job-hdr.job-no
                        prep.last-est-no       = job-hdr.est-no
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

