&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File: pcrep\r-prodep.w
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

{pcrep/tt-srt.i}

DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_dept end_dept begin_mach ~
end_mach begin_Shift end_shift begin_date end_date begin_time ~
begin_time_mins end_time end_time_mins tb_show cStartCodeNo cEndCodeNo ~
rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_dept end_dept begin_mach end_mach ~
begin_Shift end_shift begin_date end_date begin_time begin_time_mins ~
end_time end_time_mins tb_show cStartCodeNo cEndCodeNo rd-dest fi_file ~
tb_OpenCSV tbAutoClose 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_dept AS CHARACTER FORMAT "X(4)" 
     LABEL "Beginning Department" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_Shift AS INTEGER FORMAT ">9" INITIAL 1 
     LABEL "Beginning Shift" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_time AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Beginning Time (24 HR)" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .95 NO-UNDO.

DEFINE VARIABLE begin_time_mins AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .95 NO-UNDO.

DEFINE VARIABLE cEndCodeNo AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Charge Code" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE cStartCodeNo AS CHARACTER FORMAT "X(5)":U 
     LABEL "Notes - Beginning Charge Code" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_dept AS CHARACTER FORMAT "X(4)" INITIAL "zzzz" 
     LABEL "Ending Department" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_shift AS INTEGER FORMAT ">9" INITIAL 3 
     LABEL "Ending shift" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_time AS INTEGER FORMAT "99":U INITIAL 23 
     LABEL "Ending Time (24 HR)" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .95 NO-UNDO.

DEFINE VARIABLE end_time_mins AS INTEGER FORMAT "99":U INITIAL 59 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .95 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\MachineEfficiencyReport.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE 
    SIZE 51 BY 1.

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

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Email", 5,
"To CSV", 3
     SIZE 12.8 BY 3.33 NO-UNDO.

DEFINE VARIABLE rd_TonMsfQty AS CHARACTER INITIAL "QM" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tons,MSF", "TM",
"Qty,MSF", "QM",
"Qty,Tons", "QT"
     SIZE 56.6 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.43.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 10.67.

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

DEFINE VARIABLE tb_show      AS LOGICAL INITIAL YES 
     LABEL "Show Job detail For Selected Period?" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_dept AT ROW 2.67 COL 28.8 COLON-ALIGNED HELP
          "Enter Beginning Department"
     end_dept AT ROW 2.67 COL 70.4 COLON-ALIGNED HELP
          "Enter Ending Department"
     begin_mach AT ROW 3.62 COL 28.8 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     end_mach AT ROW 3.62 COL 70.4 COLON-ALIGNED HELP
          "Enter Ending Machine"
     begin_Shift AT ROW 4.57 COL 28.8 COLON-ALIGNED HELP
          "Enter Beginning Shift"
     end_shift AT ROW 4.57 COL 70.4 COLON-ALIGNED HELP
          "Enter Ending Shift"
     begin_date AT ROW 5.52 COL 28.8 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 5.52 COL 70.4 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_time AT ROW 6.43 COL 28.8 COLON-ALIGNED HELP
          "Enter Beginning Time (Hour)" WIDGET-ID 14
     begin_time_mins AT ROW 6.43 COL 35.8 COLON-ALIGNED HELP
          "Enter Beginning Time (Mins)" WIDGET-ID 18
     end_time AT ROW 6.48 COL 70.4 COLON-ALIGNED HELP
          "Enter Ending Time (Hour)" WIDGET-ID 20
     end_time_mins AT ROW 6.48 COL 77.4 COLON-ALIGNED HELP
          "Enter Ending Time (Mins)" WIDGET-ID 22
     tb_show AT ROW 7.91 COL 30.8
     rd_TonMsfQty AT ROW 9.33 COL 30.8 NO-LABEL WIDGET-ID 8
     cStartCodeNo AT ROW 10.76 COL 38.4 COLON-ALIGNED HELP
          "Enter Beginning Charge Code" WIDGET-ID 24
     cEndCodeNo AT ROW 10.76 COL 70.4 COLON-ALIGNED HELP
          "Enter Ending Charge Code" WIDGET-ID 26
     lv-font-no AT ROW 12.81 COL 34 COLON-ALIGNED
     lv-ornt AT ROW 12.91 COL 30 NO-LABEL
     lines-per-page AT ROW 12.91 COL 83 COLON-ALIGNED
     rd-dest AT ROW 13.33 COL 5.2 NO-LABEL
     lv-font-name AT ROW 13.33 COL 28 COLON-ALIGNED NO-LABEL
     tb_excel AT ROW 13.67 COL 74 RIGHT-ALIGNED
     td-show-parm AT ROW 14.38 COL 25.6
     fi_file AT ROW 15.33 COL 23.6 COLON-ALIGNED HELP
          "Enter File Name"
     tb_OpenCSV AT ROW 15.43 COL 92.4 RIGHT-ALIGNED
     tbAutoClose AT ROW 17.33 COL 25.6 WIDGET-ID 64
     btn-ok AT ROW 18.29 COL 25.6
     btn-cancel AT ROW 18.29 COL 48.8
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21.2 BY .71 AT ROW 1.14 COL 5
     " Output Destination" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 12.29 COL 5
     RECT-6 AT ROW 12.67 COL 4
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
         TITLE              = "New Machine efficiency report(Excel)"
         HEIGHT             = 20
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
       begin_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_Shift:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_time:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_time_mins:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       cEndCodeNo:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       cStartCodeNo:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_shift:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_time:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_time_mins:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
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

/* SETTINGS FOR RADIO-SET rd_TonMsfQty IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rd_TonMsfQty:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
       tb_excel:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_show:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       td-show-parm:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* New Machine efficiency report(Excel) */
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
ON WINDOW-CLOSE OF C-Win /* New Machine efficiency report(Excel) */
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


&Scoped-define SELF-NAME begin_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_dept C-Win
ON LEAVE OF begin_dept IN FRAME FRAME-A /* Beginning Department */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_Shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_Shift C-Win
ON LEAVE OF begin_Shift IN FRAME FRAME-A /* Beginning Shift */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_time C-Win
ON LEAVE OF begin_time IN FRAME FRAME-A /* Beginning Time (24 HR) */
DO:
        ASSIGN {&self-name}.
        IF begin_time LT 0 OR begin_time GT 24 THEN 
        DO: 
            MESSAGE "Invalid Hours" VIEW-AS ALERT-BOX.
            SELF:SET-SELECTION (1,3).    
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_time_mins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_time_mins C-Win
ON LEAVE OF begin_time_mins IN FRAME FRAME-A
DO:
        ASSIGN {&self-name}.
        IF begin_time_mins LT 0 OR begin_time_mins GT 60 THEN 
        DO: 
            MESSAGE "Invalid Minutes" VIEW-AS ALERT-BOX.
            SELF:SET-SELECTION (1,3).    
            RETURN NO-APPLY.
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
  
        RUN run-report.

        CASE rd-dest:
            /*when 1 then run output-to-printer.
            when 2 then run output-to-screen.
            */
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
                    ELSE DO:
                        OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                    END.
                END. /* WHEN 3 THEN DO: */
            WHEN 4 THEN 
                DO:

                    {custom/asifax.i &type= ''
                            &begin_cust=begin_mach
                            &END_cust= begin_mach
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    {custom/asimailr.i &TYPE = ''
                             &begin_cust= begin_mach
                             &END_cust=begin_mach
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                END.
            WHEN 6 THEN RUN OUTPUT-to-port.

        END CASE.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
        SESSION:SET-WAIT-STATE (""). 
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


&Scoped-define SELF-NAME end_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_dept C-Win
ON LEAVE OF end_dept IN FRAME FRAME-A /* Ending Department */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_shift C-Win
ON LEAVE OF end_shift IN FRAME FRAME-A /* Ending shift */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_time C-Win
ON LEAVE OF end_time IN FRAME FRAME-A /* Ending Time (24 HR) */
DO:
        ASSIGN {&self-name}.
        IF end_time LT 0 OR end_time GT 24 THEN 
        DO: 
            MESSAGE "Invalid Hours" VIEW-AS ALERT-BOX.
            SELF:SET-SELECTION (1,3).    
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_time_mins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_time_mins C-Win
ON LEAVE OF end_time_mins IN FRAME FRAME-A
DO:
        ASSIGN {&self-name}.
        IF end_time_mins LT 0 OR end_time_mins GT 60 THEN 
        DO: 
            MESSAGE "Invalid Minutes" VIEW-AS ALERT-BOX.
            SELF:SET-SELECTION (1,3).    
            RETURN NO-APPLY.
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


&Scoped-define SELF-NAME tb_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show C-Win
ON VALUE-CHANGED OF tb_show IN FRAME FRAME-A /* Show Job detail For Selected Period? */
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
        begin_date = DATE(MONTH(TODAY), 1, YEAR(TODAY))
        end_date   = TODAY.

    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "DE1" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
APPLY "entry" TO Begin_dept.
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
  DISPLAY begin_dept end_dept begin_mach end_mach begin_Shift end_shift 
          begin_date end_date begin_time begin_time_mins end_time end_time_mins 
          tb_show cStartCodeNo cEndCodeNo rd-dest fi_file tb_OpenCSV tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_dept end_dept begin_mach end_mach begin_Shift 
         end_shift begin_date end_date begin_time begin_time_mins end_time 
         end_time_mins tb_show cStartCodeNo cEndCodeNo rd-dest fi_file 
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
    RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
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
            fi_file:SCREEN-VALUE = "c:\tmp\MachineEfficiencyReport.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pro-rate-mr C-Win 
PROCEDURE pro-rate-mr :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-mch-act FOR mch-act.
    DEFINE BUFFER b-job-cod FOR job-code.


    FOR EACH b-mch-act NO-LOCK
        WHERE b-mch-act.company  EQ mch-act.company
        AND b-mch-act.job      EQ mch-act.job
        AND b-mch-act.job-no   EQ mch-act.job-no
        AND b-mch-act.job-no2  EQ mch-act.job-no2
        AND b-mch-act.m-code   EQ mch-act.m-code
        AND b-mch-act.dept     EQ mch-act.dept
        AND b-mch-act.pass     EQ mch-act.pass
        AND b-mch-act.frm      EQ mch-act.frm
        AND b-mch-act.blank-no EQ mch-act.blank-no,
        FIRST b-job-cod NO-LOCK
        WHERE b-job-cod.code EQ b-mch-act.code:

        IF b-job-cod.cat EQ "RUN" THEN
            tt-srt.tot-run-hours = tt-srt.tot-run-hours + b-mch-act.hours.
        ELSE
            IF b-job-cod.cat EQ "MR" THEN
                tt-srt.tot-mr-hours  = tt-srt.tot-mr-hours + b-mch-act.hours.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------  pc/rep/mch-dpt.p 8/94 gb */
/* Production by Department Report                                            */
/* -------------------------------------------------------------------------- */
    {sys/form/r-topw.f}

    DEFINE BUFFER b-mch-act FOR mch-act.

    DEFINE VARIABLE v-date         AS DATE      EXTENT 2 FORMAT "99/99/9999" NO-UNDO.
    DEFINE VARIABLE v-time         AS INTEGER   EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-dept         AS ch        FORMAT "x(4)" EXTENT 2 INITIAL ["","zzzz"].
    DEFINE VARIABLE v-mach         AS ch        FORMAT "x(6)" EXTENT 2 INITIAL ["","zzzzzz"].
    DEFINE VARIABLE v-shift        LIKE mch-act.shift FORMAT ">>" EXTENT 2 INITIAL ["1", "99"].
    DEFINE VARIABLE v-show         AS LOGICAL   FORMAT "Y/N" INIT YES NO-UNDO.
    DEFINE VARIABLE v-show1        AS LOGICAL   FORMAT "Y/N" INIT YES NO-UNDO.
    DEFINE VARIABLE mch-mr-std     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE mch-run-std    AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE mch-mr-act     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE mch-run-act    AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE mch-dt-act     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE mch-qty-prod   AS DECIMAL   FORMAT ">,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE mch-qty-expect AS DECIMAL   FORMAT ">,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE dpt-mr-std     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE dpt-run-std    AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE dpt-mr-act     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE dpt-run-act    AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE dpt-dt-act     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE dpt-qty-prod   AS DECIMAL   FORMAT ">,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE dpt-qty-expect AS DECIMAL   FORMAT ">,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE shf-mr-std     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE shf-run-std    AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE shf-mr-act     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE shf-run-act    AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE shf-dt-act     AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE shf-qty-prod   AS DECIMAL   FORMAT ">,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE shf-qty-expect AS DECIMAL   FORMAT ">,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE shf-jobs       AS INTEGER   FORMAT ">,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE tot-jobs       AS INTEGER   FORMAT ">,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE tot-eff        AS DECIMAL   FORMAT ">>>9.9-" NO-UNDO.
    DEFINE VARIABLE dt-eff         AS DECIMAL   FORMAT ">>>9.9-" NO-UNDO.
    DEFINE VARIABLE tot-std-hrs    AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE tot-act-hrs    AS DECIMAL   FORMAT ">>>>9.9" NO-UNDO.
    DEFINE VARIABLE a              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-tot-uni-jobs AS LOG       NO-UNDO.
    DEFINE VARIABLE hdr-tit        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdr-tit2       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdr-tit3       AS CHARACTER NO-UNDO.

    DEFINE VARIABLE excelheader1   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE excelheader2   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE excelheader3   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE mch-qty-ton    AS DECIMAL   FORMAT ">>,>>9.99" NO-UNDO.
    DEFINE VARIABLE shf-qty-ton    AS DECIMAL   FORMAT ">>,>>9.99" NO-UNDO.
    DEFINE VARIABLE dpt-qty-ton    AS DECIMAL   FORMAT ">>,>>9.99" NO-UNDO.
    DEFINE VARIABLE mch-qty-msf    AS DECIMAL   FORMAT ">>,>>9.99" NO-UNDO.
    DEFINE VARIABLE shf-qty-msf    AS DECIMAL   FORMAT ">>,>>9.99" NO-UNDO.
    DEFINE VARIABLE dpt-qty-msf    AS DECIMAL   FORMAT ">>,>>9.99" NO-UNDO.

    FORM HEADER
        hdr-tit FORMAT "x(145)" SKIP
        hdr-tit2 FORMAT "x(145)" SKIP
        hdr-tit3 FORMAT "x(145)"

        WITH FRAME r-top.


    SESSION:SET-WAIT-STATE ("general").

    ASSIGN
        str-tit2   = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-dept[1]  = begin_dept
        v-dept[2]  = end_dept
        v-mach[1]  = begin_mach
        v-mach[2]  = end_mach
        v-shift[1] = begin_shift
        v-shift[2] = end_shift
        v-date[1]  = begin_date
        v-date[2]  = end_date
        v-show     = tb_show
        v-time[1]  = begin_time * 3600 + begin_time_mins * 60
        v-time[2]  = end_time * 3600 + end_time_mins * 60 + 59
        /*v-show1     = tb_show1
        v-tot-uni-jobs = tb_tot-job*/

        /*
        hdr-tit =  "MACH                  <---MAKE READY HOURS-->  " +
                   "<------RUN HOURS------>  <----MR & RUN HOURS--->  " +
                   "<--D/T HOURS->   ACTUAL" +
                   /*IF tb_tonmsf THEN "    ACTUAL"
                   ELSE */ "    EXPECTED"             
       
        hdr-tit2 = "CODE     JOB #  SHIFT  STNDRD  ACTUAL  EFF %   " +
                   " STNDRD  ACTUAL  EFF %    STNDRD  ACTUAL  EFF %   " +
                   " ACTUAL  EFF %   " + 
                   IF tb_tonmsf AND rd_tonmsfQty = "tm" THEN "  TONS    MSF"
                   ELSE IF tb_tonmsf AND rd_tonmsfqty = "qm" THEN "QUANTITY  MSF"
                   ELSE IF tb_tonmsf AND rd_tonmsfqty = "qt" THEN "QUANTITY  TONS"
                   ELSE "QUANTITY  QUANTITY" 
       */
        hdr-tit3   = FILL("-", 132).


    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    /*if td-show-parm then run show-param. */

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        excelheader1 = "Machine Efficiency,,,,Date," + STRING(begin_date) + "," + STRING(END_date) +
            ",Shift," + STRING(v-shift[1]) + "," + STRING(v-shift[2]).
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader1,',','","') '"' SKIP(1).

        /*excelheader2 = "Date,Sq Feet,,,,,Sq Feet,Sheets,Finished,FG,FG,Sq. Ft.,Sq Ft,waste % ,Set up,Total,Avg Pcs,,Setup ,Run".
        excelheader3 = "Job#,Blank,Start SE,End SE,Start Run,End Run,Received,Received,Quantity,Received,Deviation,Produced,Waste,per line,Time,Run time,Per Hr,,Efficiency,Efficiency".   
        */
        excelheader2 = ",,MR Start,MR End,Run Start,Run End,,Sq Feet,Setup,Run,Sq Feet,Sheets,Produced,Finished,FG,FG,Sq. Ft.,Sq Ft,waste % ,Avg Pcs,Setup,Run".
        excelheader3 = "Machine,Date,Time,Time,Time,Time,Job#,Blank,Hours,Hours,Received,Received,Quantity,Quantity,Received,Deviation,Produced,Waste,per line,Per Hr,Efficiency,Efficiency,Shift#,Item Code,Item Name,Linear Ft/Hr,Notes,Entry Note".
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader2,',','","') '"' SKIP
            '"' REPLACE(excelheader3,',','","') '"' SKIP.
        .
    /*
    excelheader2 = 
    excelheader3 = "Mach Code,Job#,-,Shift,MR STND,MR ACT,MR EFF%," + 
                  "Run Hrs Stnd,Run Hrs ACT,Run Hrs EFF%," +
                  "MR/Run Hrs Stnd,MR/Run Hrs ACT,MR/Run Hrs EFF%," +
                  "D/T HRS ACT,D/T HRS EFF%," +                 
                  IF tb_tonmsf AND rd_tonmsfqty = "tm" THEN "ACT QTY,ACT TONS," 
                  ELSE IF tb_tonmsf AND rd_tonmsfqty = "qm" THEN "ACT QTY,ACT MSF,"
                  ELSE IF tb_tonmsf AND rd_tonmsfqty = "qt" THEN "ACT QTY,ACT TONS,"
                  ELSE "ACT QTY,EXPECTED QTY,".
    PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' skip.
    */
    END. 

    DISPLAY "" WITH FRAME r-top.

    {pcrep/r-mcheff.i}

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
    END.

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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE    NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-label      AS CHARACTER NO-UNDO.

    ASSIGN
        lv-frame-hdl = FRAME {&frame-name}:HANDLE
        lv-group-hdl = lv-frame-hdl:FIRST-CHILD
        lv-field-hdl = lv-group-hdl:FIRST-CHILD.

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

