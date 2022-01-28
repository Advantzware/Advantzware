&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: jcrep\JobSumReport.w

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


DEFINE VARIABLE cCompany   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLoc       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cocode     AS CHARACTER NO-UNDO.
DEFINE VARIABLE locode     AS CHARACTER NO-UNDO.
DEFINE VARIABLE i          AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-prgmname AS CHARACTER INIT "JobSumReport" NO-UNDO.

RUN spGetSessionParam("Company", OUTPUT cCompany).
RUN spGetSessionParam("Location", OUTPUT cLoc).

cocode =  cCompany.
locode =  cLoc.

{jc\ttJobReport.i}

DEFINE VARIABLE ll-secure AS LOG NO-UNDO.

DEFINE STREAM excel.
DEFINE VARIABLE fi_file    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName2 AS CHARACTER NO-UNDO.

ASSIGN
    fi_file = "c:\tmp\r-jobsumN2.csv".


DEFINE VARIABLE v-header-1         AS CHARACTER INIT "FG Item,FG Item Name,Selling Price,Job Quantity,Produced,On Hand" NO-UNDO .
DEFINE VARIABLE v-header-2         AS CHARACTER INIT "F/B,Department,Std/Act,Machine,Run Qty,Setup Hours,Run Hours,Speed,Cost,Setup Waste,Run Waste,Downtime Code,Downtime Hrs" NO-UNDO .
DEFINE VARIABLE v-header-3         AS CHARACTER INIT "F/B ,Material,Quantity,Quantity Variance,Material Cost,Material Cost Variance," NO-UNDO .
DEFINE VARIABLE v-header-4         AS CHARACTER INIT " Department, Run Qty,Run Qty Var, Setup Hours,Setup Hours Var, Run Hours,Run Hours Var, Speed,Speed Var, Cost,Cost Var, Setup Waste,Setup Waste Var, Run Waste,Run Waste Var, DownTime Code, DownTime Hrs" NO-UNDO .
DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cColumnInit        AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.

DEFINE VARIABLE fg-str-tit         AS cha       FORM "x(170)" NO-UNDO.
DEFINE VARIABLE fg-str-tit2        AS cha       FORM "x(170)" NO-UNDO.
DEFINE VARIABLE fg-str-tit3        AS cha       FORM "x(170)" NO-UNDO.
DEFINE VARIABLE fg-str-line        AS cha       FORM "x(170)" NO-UNDO.
DEFINE VARIABLE mach-str-tit       AS cha       FORM "x(170)" NO-UNDO.
DEFINE VARIABLE mach-str-tit2      AS cha       FORM "x(170)" NO-UNDO.
DEFINE VARIABLE mach-str-tit3      AS cha       FORM "x(170)" NO-UNDO.
DEFINE VARIABLE mach-str-line      AS cha       FORM "x(170)" NO-UNDO.
DEFINE VARIABLE item-str-tit       AS cha       FORM "x(170)" NO-UNDO.
DEFINE VARIABLE item-str-tit2      AS cha       FORM "x(170)" NO-UNDO.
DEFINE VARIABLE item-str-tit3      AS cha       FORM "x(170)" NO-UNDO.
DEFINE VARIABLE item-str-line      AS cha       FORM "x(170)" NO-UNDO.
DEFINE VARIABLE misc-str-tit       AS cha       FORM "x(220)" NO-UNDO.
DEFINE VARIABLE misc-str-tit2      AS cha       FORM "x(220)" NO-UNDO.
DEFINE VARIABLE misc-str-tit3      AS cha       FORM "x(220)" NO-UNDO.
DEFINE VARIABLE misc-str-line      AS cha       FORM "x(220)" NO-UNDO.
DEFINE VARIABLE excelheader        AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader1       AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader2       AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader3       AS CHARACTER NO-UNDO.
DEFINE VARIABLE excelheader4       AS CHARACTER NO-UNDO.

ASSIGN 
    cTextListToSelect  = "FG Item,FG Item Name,Selling Price,Job Quantity,Produced,On Hand," + 
                           "---MACHINE HEADER------,F/B,Department,Std/Act,Machine,Run Qty,Setup Hours,Run Hours,Speed,Cost,Setup Waste,Run Waste,Downtime Code,Downtime Hrs," +
                           "---ITEM HEADER---,F/B ,Material,Quantity,Quantity Variance,Material Cost,Material Cost Variance," +
                           "---DEPARTMENT HEADER---, Department, Run Qty,Run Qty Var, Setup Hours,Setup Hours Var, Run Hours,Run Hours Var, Speed,Speed Var, Cost,Cost Var, Setup Waste,Setup Waste Var, Run Waste,Run Waste Var, DownTime Code, DownTime Hrs"
    cFieldListToSelect = "fg-item,fgitem-name,sel-price,job-qty,produced,on-hand," +
                            "machheader,form,dept,std-act,machine,run-qty,setup-hrs,run-hrs,speed,cost,setup-waste,run-waste,downtime-code,downtime-hrs," +
                            "itemheader,form-blank,material,mat-qty,mat-qty-var,mat-cost,mat-cost-var," +
                            "deptheader,department,dep-run-qty,dep-run-qty-var,dept-setup-hrs,dept-setup-hrs-var,dept-run-hrs,dept-run-hrs-var,dept-speed,dept-speed-var,dept-cost,dept-cost-var,dept-setup-waste,dept-setup-waste-var,dept-run-waste,dept-run-waste-var,dept-downtime,dept-downtime-hrs"

    cFieldLength       = "15,30,14,12,12,12," +
                      "15,5,10,10,10,12,11,10,11,15,12,11,13,12," +
                      "15,5,15,12,17,15,22," +
                      "15,11,12,11,12,15,10,13,10,10,14,9,12,15,10,13,14,13"   

    cFieldType         = "c,c,i,i,i,i," + "c,c,c,c,c,i,i,i,i,i,i,i,c,i," + "c,c,c,i,i,i,i," + "c,c,i,i,i,i,i,i,i,i,i,i,i,i,i,i,c,i"    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "FG Item,FG Item Name,Selling Price,Job Quantity,Produced,On Hand," + 
                           "---MACHINE HEADER------,F/B,Department,Std/Act,Machine,Run Qty,Setup Hours,Run Hours,Speed,Cost,Setup Waste,Run Waste,Downtime Code,Downtime Hrs," +
                           "---ITEM HEADER---,F/B ,Material,Quantity,Quantity Variance,Material Cost,Material Cost Variance," +
                           "---DEPARTMENT HEADER---, Department, Run Qty,Run Qty Var, Setup Hours,Setup Hours Var, Run Hours,Run Hours Var, Speed,Speed Var, Cost,Cost Var, Setup Waste,Setup Waste Var, Run Waste,Run Waste Var, DownTime Code, DownTime Hrs" 
    .


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_job-no begin_job-no2 ~
end_job-no end_job-no2 rd-dest fi_file2 tb_OpenCSV tbAutoClose btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_job-no begin_job-no2 end_job-no ~
end_job-no2 rd-dest fi_file2 tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */



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

DEFINE VARIABLE begin_job-no   AS CHARACTER FORMAT "X(6)":U 
    LABEL "Beginning Job#" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2  AS CHARACTER FORMAT "-99":U INITIAL "00" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no     AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
    LABEL "Ending Job#" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2    AS CHARACTER FORMAT "-99":U INITIAL "99" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file2       AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-jobsumN.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 43 BY 1.

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
    "To Email", 5,
    "To CSV", 3
    SIZE 16 BY 4.24 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.14.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 4.95.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV  AS LOGICAL INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_job-no AT ROW 3.62 COL 29.2 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    begin_job-no2 AT ROW 3.62 COL 41.2 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    end_job-no AT ROW 3.62 COL 64.2 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    end_job-no2 AT ROW 3.62 COL 76.2 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    lv-ornt AT ROW 7.24 COL 30 NO-LABELS
    lines-per-page AT ROW 7.43 COL 83 COLON-ALIGNED
    rd-dest AT ROW 7.52 COL 6 NO-LABELS
    lv-font-no AT ROW 8.33 COL 33 COLON-ALIGNED
    lv-font-name AT ROW 9.33 COL 27 COLON-ALIGNED NO-LABELS
    fi_file2 AT ROW 10.76 COL 29 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 10.86 COL 91 RIGHT-ALIGNED
    tbAutoClose AT ROW 12.48 COL 31.2 WIDGET-ID 30
    btn-ok AT ROW 13.62 COL 31
    btn-cancel AT ROW 13.62 COL 51
    " Output Destination" VIEW-AS TEXT
    SIZE 18.8 BY .62 AT ROW 6.71 COL 5
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    RECT-6 AT ROW 7.1 COL 4
    RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 15.19
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
        TITLE              = "Job Summary Analysis Report New"
        HEIGHT             = 15.29
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
    begin_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Job Summary Analysis Report */
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
ON WINDOW-CLOSE OF C-Win /* Job Summary Analysis Report */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
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
             
        IF rd-dest EQ 3 THEN
        DO:
            ASSIGN 
                fi_file2 = SUBSTRING(fi_file2,1,INDEX(fi_file2,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file2,OUTPUT cFileName2) .
            fi_file2:SCREEN-VALUE =  cFileName2.
        END.  

        RUN GetSelectionList.
        
        RUN run-report. 
        STATUS DEFAULT "Processing Complete. ". 

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN 
                DO:
                    IF NOT tb_OpenCSV THEN 
                    DO:        
                        MESSAGE "CSV file have been created." SKIP(1)
                            "~"OK"~"Want to open CSV file?"
                            VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                            TITLE "" UPDATE lChoice AS LOGICAL.
                 
                        IF lChoice THEN
                        DO:
                            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName2)).
                        END.
                    END.
                END. /* WHEN 3 THEN DO: */
            WHEN 5 THEN
                DO:
                    DEFINE VARIABLE lv-tmp AS CHARACTER INIT "-0" NO-UNDO.

                    {custom/asimailr.i &TYPE="Customer"
                             &begin_cust=lv-tmp
                             &END_cust=lv-tmp
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                END.
        END CASE. 

        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
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


&Scoped-define SELF-NAME fi_file2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file2 C-Win
ON LEAVE OF fi_file2 IN FRAME FRAME-A /* Name */
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
        RUN pChangeDest .
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
      
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {methods/nowait.i}
  
    DO WITH FRAME {&FRAME-NAME}:
        //{custom/usrprint.i}
    
        APPLY "entry" TO begin_job-no.
    END.
    RUN pChangeDest .
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
    DISPLAY begin_job-no begin_job-no2 end_job-no end_job-no2 rd-dest fi_file2 
        tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_job-no begin_job-no2 end_job-no end_job-no2 
        rd-dest fi_file2 tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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
                fi_file2:SENSITIVE      = YES
                tb_OpenCSV:SENSITIVE    = YES      
                .
        ELSE
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "NO"
                fi_file2:SENSITIVE      = NO
                tb_OpenCSV:SENSITIVE    = NO      
                .
        ASSIGN 
            fi_file2:SCREEN-VALUE = "C:\tmp\JobSumReport.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ----------------------------------------------- jc/rep/job-sum.p 08/94 JLF */
    /* Job Summary Report                                                         */
    /* -------------------------------------------------------------------------- */        
    {sys/form/r-top3w.f} 
    DEFINE VARIABLE cDisplay       AS cha    NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS cha    NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHA    NO-UNDO.
    DEFINE VARIABLE cVarValue      AS cha    NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS cha    NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS cha    NO-UNDO.
    DEFINE VARIABLE cFieldName     AS cha    NO-UNDO. 
    
    ASSIGN 
        fg-str-tit    = ""
        fg-str-tit2   = ""
        fg-str-tit3   = ""
        fg-str-line   = ""
        mach-str-tit  = ""
        mach-str-tit2 = ""
        mach-str-tit3 = ""
        mach-str-line = ""
        item-str-tit  = ""
        item-str-tit2 = ""
        item-str-tit3 = ""
        item-str-line = ""
        misc-str-tit  = ""
        misc-str-tit2 = ""
        misc-str-tit3 = ""
        misc-str-line = ""
        excelheader   = ""
        excelheader1  = ""
        excelheader2  = ""
        excelheader3  = ""
        excelheader4  = "".
        
    cSelectedList = cTextListToDefault.
    
    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}.
        
    DEFINE VARIABLE cslist AS cha NO-UNDO.
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        IF LOOKUP(ttRptSelected.TextList, v-header-1) <> 0    THEN 
        DO:

            IF ttRptSelected.TextList = "SELLING PRICE/M" THEN 
                ASSIGN fg-str-tit  = fg-str-tit +   "   SELLING" + " "
                    fg-str-tit2 = fg-str-tit2 + "   PRICE/M" + " "
                    fg-str-tit3 = fg-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .         
            ELSE 
            DO:

                IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
                    THEN ASSIGN fg-str-tit2 = fg-str-tit2 + ttRptSelected.TextList + " "
                        fg-str-tit  = fg-str-tit + FILL(" ",ttRptSelected.FieldLength) + " " 
                        fg-str-tit3 = fg-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " "
                        .      
                ELSE 
                    ASSIGN fg-str-tit2 = fg-str-tit2 + 
                         (IF ttRptSelected.HeadingFromLeft THEN
                             ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
                             ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                        fg-str-tit  = fg-str-tit + FILL(" ",ttRptSelected.FieldLength) + " "
                        fg-str-tit3 = fg-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " "
                        .        
            END.

            excelheader1 = excelHeader1 + ttRptSelected.TextList + "," .  
            cSlist = cSlist + ttRptSelected.FieldList + ",".

            IF LOOKUP(ttRptSelected.TextList, "SELLING PRICE/M,ORDERED,POSTED,FINISHED,ALLOWED,ACTUAL SPOILAGE,ACT SPL%,ESTIMATE SPOILAGE,EST SPL%,OVER-RUN %") <> 0    THEN
                ASSIGN
                    fg-str-line = fg-str-line + FILL("-",ttRptSelected.FieldLength) + " " .
            ELSE
                fg-str-line = fg-str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
        END.

        IF LOOKUP(ttRptSelected.TextList, v-header-2) <> 0    THEN 
        DO:

            IF ttRptSelected.TextList = "MACH CODE" THEN 
                ASSIGN mach-str-tit  = mach-str-tit +   "   MACH" + " "
                    mach-str-tit2 = mach-str-tit2 +  "   CODE" + " "
                    mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .
            ELSE 
            DO:

                IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
                    THEN ASSIGN mach-str-tit2 = mach-str-tit2 + ttRptSelected.TextList + " "
                        mach-str-tit  = mach-str-tit + FILL(" ",ttRptSelected.FieldLength) + " "
                        mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " "
                        .        
                ELSE 
                    ASSIGN mach-str-tit2 = mach-str-tit2 + 
                         (IF ttRptSelected.HeadingFromLeft THEN
                             ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
                             ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                        mach-str-tit  = mach-str-tit + FILL(" ",ttRptSelected.FieldLength) + " "
                        mach-str-tit3 = mach-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " "
                        .   
            END.

            excelheader2 = excelHeader2 + ttRptSelected.TextList + "," .  
            cSlist = cSlist + ttRptSelected.FieldList + ",".

            IF LOOKUP(ttRptSelected.TextList, "Run Qty,Setup Hours,Run Hours,Speed,Cost,Setup Waste,Run Waste") <> 0    THEN
                ASSIGN
                    mach-str-line = mach-str-line + FILL("-",ttRptSelected.FieldLength) + " " .
            ELSE
                mach-str-line = mach-str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
        END.

        IF LOOKUP(ttRptSelected.TextList, v-header-3) <> 0    THEN 
        DO:

            IF ttRptSelected.TextList = "ITEM EST QUANTITY" THEN 
                ASSIGN item-str-tit  = item-str-tit +  "      EST" + " "
                    item-str-tit2 = item-str-tit2 + " QUANTITY" + " "
                    item-str-tit3 = item-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " " .

            ELSE 
            DO:  

                IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
                    THEN ASSIGN item-str-tit2 = item-str-tit2 + ttRptSelected.TextList + " "
                        item-str-tit  = item-str-tit + FILL(" ",ttRptSelected.FieldLength) + " "
                        item-str-tit3 = item-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " "
                        .        
                ELSE 
                    ASSIGN item-str-tit2 = item-str-tit2 + 
                         (IF ttRptSelected.HeadingFromLeft THEN
                             ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
                             ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                        item-str-tit  = item-str-tit + FILL(" ",ttRptSelected.FieldLength) + " "
                        item-str-tit3 = item-str-tit3 + FILL("-",ttRptSelected.FieldLength) + " "
                        .      
            END.

            excelheader3 = excelHeader3 + ttRptSelected.TextList + "," .  
            cSlist = cSlist + ttRptSelected.FieldList + ",".

            IF LOOKUP(ttRptSelected.TextList, "Quantity,Material Cost") <> 0    THEN
                ASSIGN
                    item-str-line = item-str-line + FILL("-",ttRptSelected.FieldLength) + " " .
            ELSE
                item-str-line = item-str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
        END.

        IF LOOKUP(ttRptSelected.TextList, v-header-4) <> 0    THEN 
        DO:

            IF ttRptSelected.TextList = "MISC EST COST" THEN 
                ASSIGN misc-str-tit  = misc-str-tit +  "  EST COST" + " "
                    misc-str-tit3 = misc-str-tit3 + "          " + " "
                    misc-str-tit2 = misc-str-tit2 + FILL("-",ttRptSelected.FieldLength) + " " .         
            ELSE 
            DO:

                IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
                    THEN ASSIGN misc-str-tit  = misc-str-tit + ttRptSelected.TextList + " "
                        misc-str-tit2 = misc-str-tit2 + FILL("-",ttRptSelected.FieldLength) + " "
                        .        
                ELSE 
                    ASSIGN misc-str-tit  = misc-str-tit + 
                     (IF ttRptSelected.HeadingFromLeft THEN
                         ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
                         ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                        misc-str-tit2 = misc-str-tit2 + FILL("-",ttRptSelected.FieldLength) + " "
                        .        
            END.

            excelheader4 = excelHeader4 + ttRptSelected.TextList + "," .  
            cSlist = cSlist + ttRptSelected.FieldList + ",".

            IF LOOKUP(ttRptSelected.TextList, "MISC EST COST,MISC ACT COST,MISC VARI,MISC VAR%") <> 0    THEN
                ASSIGN
                    misc-str-line = misc-str-line + FILL("-",ttRptSelected.FieldLength) + " " .
            ELSE
                misc-str-line = misc-str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
        END.

    END.

    IF rd-dest EQ 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName2).
    END.


    SESSION:SET-WAIT-STATE ("general").
    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}   

    DISPLAY "" WITH FRAME r-top.    
    
    EMPTY TEMP-TABLE ttJob.
    EMPTY TEMP-TABLE ttDepartment.
    EMPTY TEMP-TABLE ttOperation.
    EMPTY TEMP-TABLE ttMaterial.
    EMPTY TEMP-TABLE ttItem.    
    
    
    DO WITH FRAME {&FRAME-NAME}:
        RUN jc/jobSumReport.p(cocode, begin_job-no:SCREEN-VALUE, STRING(begin_job-no2:SCREEN-VALUE,"99"), end_job-no:SCREEN-VALUE, STRING(begin_job-no2,"99"),
            OUTPUT table ttJob,
            OUTPUT table ttDepartment,
            OUTPUT table ttOperation,
            OUTPUT table ttMaterial,
            OUTPUT table ttItem
            ).
    END. 
    
    RUN pPrintData.
         
    IF rd-dest EQ 3 THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName2)).
    END.
      
    //RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintData C-Win 
PROCEDURE pPrintData :
    /*------------------------------------------------------------------------------
     Purpose:    
     Parameters:  <none>
     Notes:      
    ------------------------------------------------------------------------------*/
    
    FOR EACH ttJob NO-LOCK        
        BREAK BY ttJob.cJobNo
        BY ttJob.iJobNo2: 

        IF FIRST-OF(ttJob.iJobNo2) THEN
        DO:             
            PUT "Job Number: "
                TRIM(ttJob.cJobNo) + "-" + string(ttJob.iJobNo2,"99") +
                "     Closing Date: " +
                (IF ttJob.dtCloseDate EQ ? THEN "        " ELSE
                STRING(ttJob.dtCloseDate,"99/99/99"))  FORMAT "X(36)" SKIP
                "Customer:   "
                ttJob.cCustName FORMAT "X(36)" .  

            IF rd-dest EQ 3 THEN
                PUT STREAM excel UNFORMATTED                       
                    '"' "Job Number: "                '",'           
                    '"' STRING(TRIM(ttJob.cJobNo) + "-" + string(ttJob.iJobNo2,"99"))               '",'
                    '"' "Closing Date: "                '",' 
                    '"' (IF ttJob.dtCloseDate EQ ? THEN " " ELSE STRING(ttJob.dtCloseDate,"99/99/9999"))               '",'  
                    SKIP
                    '"' "Customer:   "               '",'  
                    '"' ttJob.cCustName      '",'
                    SKIP(1) .  
                    
            RUN pPrintFGItem. 
            
            RUN pPrintOperation.

            RUN pPrintDepartment.
            
            RUN pPrintMaterial.
                        
            PUT SKIP(1)
                " Total Standard Machine Cost:" dTotStdMachineCost   FORMAT "->>>,>>>,>>9.99"
                "   Total Actual Machine Cost:"  dTotActMachineCost FORMAT "->>>,>>>,>>9.99"
                SKIP 
                "Total Standard Material Cost:"  dTotStdMaterialCost  FORMAT "->>>,>>>,>>9.99"
                "  Total Actual Material Cost:"  dTotActMaterialCost FORMAT "->>>,>>>,>>9.99"
                SKIP
                "         Total Standard Cost:" dTotStdCost FORMAT "->>>,>>>,>>9.99"
                "           Total Actual Cost:" dTotActCost FORMAT "->>>,>>>,>>9.99" SKIP(1).

            IF rd-dest EQ 3 THEN
                PUT STREAM excel UNFORMATTED SKIP(1)
                    '"' "Total Standard Machine Cost:" '",,,'  
                    '"' STRING(dTotStdMachineCost)      '",'
                    '"' "Total Actual Machine Cost:"  '",,,'  
                    '"' STRING(dTotActMachineCost) '",' SKIP

                    '"' "Total Standard Material Cost:" '",,,'  
                    '"' STRING(dTotStdMaterialCost)      '",'
                    '"' "Total Actual Material Cost:"  '",,,'  
                    '"' STRING(dTotActMaterialCost) '",' SKIP

                    '"' "Total Standard Cost:" '",,,'  
                    '"' STRING(dTotStdCost)      '",'
                    '"' "Total Actual Cost:"  '",,,'  
                    '"' STRING(dTotActCost) '",' SKIP(2)
                    .                                  
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintFGItem C-Win 
PROCEDURE pPrintFGItem :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF rd-dest EQ 3 THEN 
    DO:
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader1,',','","') '"' SKIP.
    END. 

    PUT SKIP(1)
        fg-str-tit SKIP
        fg-str-tit2 SKIP
        fg-str-tit3  SKIP.

    FOR EACH ttItem NO-LOCK
        WHERE ttItem.cJobNo EQ ttJob.cJobNo
        AND ttItem.iJobNo2 EQ ttJob.iJobNo2: 

        PUT ttItem.cFGItem  FORMAT "x(15)" SPACE(1)
            ttItem.cFGName  FORMAT "x(30)" SPACE(1)
            ttItem.cSellingPrice FORMAT "->>,>>>,>>9.99<<<<" SPACE(1)
            ttItem.dJobQty  FORMAT "->>>,>>>,>>9" SPACE(1)
            ttItem.dProduced FORMAT "->>>,>>>,>>9"  SPACE(1)
            ttItem.dOnHand FORMAT "->>>,>>>,>>9" SKIP.

        IF rd-dest EQ 3 THEN
            PUT STREAM excel UNFORMATTED                       
                '"' ttItem.cFGItem                    '",'           
                '"' ttItem.cFGName                    '",'
                '"' STRING(ttItem.cSellingPrice)      '",' 
                '"' STRING(ttItem.dJobQty)            '",'                          
                '"' STRING(ttItem.dProduced)          '",'  
                '"' STRING(ttItem.dOnHand)            '",'
                SKIP .                       
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintOperation C-Win 
PROCEDURE pPrintOperation :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF rd-dest EQ 3 THEN 
    DO:
        PUT STREAM excel UNFORMATTED SKIP(1)
            '"' REPLACE(excelheader2,',','","') '"' SKIP.
    END.
    PUT SKIP(1)
        mach-str-tit SKIP
        mach-str-tit2 SKIP
        mach-str-tit3  SKIP.

    FOR EACH ttOperation NO-LOCK
        WHERE ttOperation.cJobNo EQ ttJob.cJobNo
        AND ttOperation.iJobNo2 EQ ttJob.iJobNo2
        BREAK BY ttOperation.iJobNo2:

        PUT STRING(ttOperation.iFormNo) + "/" +  string(ttOperation.iBlankNo)  FORMAT "x(5)"  SPACE(1)
            ttOperation.cDept  FORMAT "x(10)"  SPACE(1)
            ttOperation.cStdAct FORMAT "x(10)"  SPACE(1)
            ttOperation.cMachine FORMAT "x(10)"    SPACE(1)
            ttOperation.dRunQty FORMAT "->>>,>>>,>>9"   SPACE(1)
            ttOperation.dSetupHrs FORMAT "->>>,>>9.99"  SPACE(1)
            ttOperation.dRunHrs FORMAT "->>,>>9.99"  SPACE(1)
            ttOperation.dSpeed FORMAT "->>,>>>,>>9"  SPACE(1)
            ttOperation.dCost   FORMAT "->>>,>>>,>>9.99"  SPACE(1)
            ttOperation.dSetupWaste FORMAT "->>>,>>>,>>9"  SPACE(1)
            ttOperation.dRunWaste  FORMAT "->>,>>>,>>9" SPACE(1)
            ttOperation.cDownTimeCode FORMAT "x(13)" SPACE(1)
            ttOperation.dDownTimeHrs  FORMAT "->>>,>>9.99" SPACE(1)
            SKIP.

        IF rd-dest EQ 3 THEN
            PUT STREAM excel UNFORMATTED                       
                '"' STRING(ttOperation.iFormNo,"99") + "|" +  string(ttOperation.iBlankNo,"99")                      '",'           
                '"' ttOperation.cDept                    '",'
                '"' STRING(ttOperation.cStdAct)          '",' 
                '"' STRING(ttOperation.cMachine)         '",'                          
                '"' STRING(ttOperation.dRunQty)          '",'  
                '"' STRING(ttOperation.dSetupHrs)        '",'
                '"' STRING(ttOperation.dRunHrs)          '",'
                '"' STRING(ttOperation.dSpeed)           '",'
                '"' STRING(ttOperation.dCost)            '",'
                '"' STRING(ttOperation.dSetupWaste)      '",'
                '"' STRING(ttOperation.dRunWaste)        '",'
                '"' STRING(ttOperation.cDownTimeCode)    '",'
                '"' STRING(ttOperation.dDownTimeHrs)     '",'
                SKIP .   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintDepartment C-Win 
PROCEDURE pPrintDepartment :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF rd-dest EQ 3 THEN 
    DO:
        PUT STREAM excel UNFORMATTED  SKIP(1)
            '"' REPLACE(excelheader4,',','","') '"' SKIP.
    END.
            
    PUT SKIP(2)
        misc-str-tit SKIP
        misc-str-tit2 SKIP
        .

    FOR EACH ttDepartment NO-LOCK
        WHERE ttDepartment.cJobNo EQ ttJob.cJobNo
        AND ttDepartment.iJobNo2 EQ ttJob.iJobNo2
        AND ttDepartment.cDept NE ""
        BREAK BY ttDepartment.iJobNo2:
                           
        PUT ttDepartment.cDept  FORMAT "x(11)"  SPACE(1)
            ttDepartment.dRunQty  FORMAT "->>>,>>>,>>9"  SPACE(1)
            ttDepartment.dRunQtyVar  FORMAT "->>,>>>,>>9"    SPACE(1)
            ttDepartment.dSetupHrs FORMAT "->>>,>>>,>>9"   SPACE(1)
            ttDepartment.dSetupHrsVar FORMAT "->>>,>>>,>>9.99"  SPACE(1)
            ttDepartment.dRunHrs FORMAT "->>,>>9.99"  SPACE(1)
            ttDepartment.dRunHrsVar FORMAT "->>>>,>>>,>>9"  SPACE(1)
            ttDepartment.dSpeed   FORMAT "->,>>>,>>9"  SPACE(1)
            ttDepartment.dSpeedVar FORMAT "->,>>>,>>9"  SPACE(1)
            ttDepartment.dCost  FORMAT "->>,>>>,>>9.99" SPACE(1)
            ttDepartment.dCostVar FORMAT "->,>>9.99" SPACE(1)
            ttDepartment.dSetupWaste  FORMAT "->>>,>>>,>>9" SPACE(1)                      
            ttDepartment.dSetupWasteVar FORMAT "->>>,>>>,>>9.99" SPACE(1)
            ttDepartment.dRunWaste  FORMAT "->,>>>,>>9" SPACE(1)
            ttDepartment.dRunWasteVar FORMAT "->,>>>,>>9.99" SPACE(1)
            ttDepartment.cDownTimeCode  FORMAT "x(14)" SPACE(1)
            ttDepartment.dDownTimeHrs FORMAT "->,>>>,>>9.99" SPACE(1)                    
            SKIP.

        IF rd-dest EQ 3 THEN
            PUT STREAM excel UNFORMATTED                       
                '"' ttDepartment.cDept                       '",'           
                '"' STRING(ttDepartment.dRunQty)             '",'
                '"' STRING(ttDepartment.dRunQtyVar)          '",'                        
                '"' STRING(ttDepartment.dSetupHrs)           '",'                          
                '"' STRING(ttDepartment.dSetupHrsVar)        '",'                         
                '"' STRING(ttDepartment.dRunHrs)             '",'
                '"' STRING(ttDepartment.dRunHrsVar)          '",'                                                                         
                '"' STRING(ttDepartment.dSpeed)              '",'
                '"' STRING(ttDepartment.dSpeedVar)           '",'                          
                '"' STRING(ttDepartment.dCost)               '",'
                '"' STRING(ttDepartment.dCostVar)            '",'                           
                '"' STRING(ttDepartment.dSetupWaste)         '",'
                '"' STRING(ttDepartment.dSetupWasteVar)      '",'
                '"' STRING(ttDepartment.dRunWaste)           '",'
                '"' STRING(ttDepartment.dRunWasteVar)        '",'                          
                '"' STRING(ttOperation.cDownTimeCode)        '",'
                '"' STRING(ttOperation.dDownTimeHrs)         '",'
                SKIP .   
    END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintMaterial C-Win 
PROCEDURE pPrintMaterial :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF rd-dest EQ 3 THEN 
    DO:
        PUT STREAM excel UNFORMATTED  SKIP(1)
            '"' REPLACE(excelheader3,',','","') '"' SKIP.
    END.
    PUT SKIP(1)
        item-str-tit SKIP
        item-str-tit2 SKIP
        item-str-tit3  SKIP.

    FOR EACH ttMaterial NO-LOCK
        WHERE ttMaterial.cJobNo EQ ttJob.cJobNo
        AND ttMaterial.iJobNo2 EQ ttJob.iJobNo2:

        PUT STRING(ttMaterial.iFormNo) + "|" +  string(ttMaterial.iBlankNo)  FORMAT "x(5)"  SPACE(1)
            ttMaterial.cMaterial FORMAT "x(15)"  SPACE(1)                    
            ttMaterial.dQtyStd  FORMAT "->>>,>>>,>>9" SPACE(1)
            ttMaterial.dQtyAct  FORMAT "->>>,>>>,>>9" SPACE(1)
            ttMaterial.dQtyVar  FORMAT "->,>>>,>>>,>>9.99" SPACE(1)
            ttMaterial.dCostStd  FORMAT "->>>,>>>,>>9.99"  SPACE(1)
            ttMaterial.dCostAct  FORMAT "->>>,>>>,>>9.99"  SPACE(1)
            ttMaterial.dCostVar  FORMAT "->>>,>>>,>>9.99"  SPACE(1)
            SKIP.

        IF rd-dest EQ 3 THEN
            PUT STREAM excel UNFORMATTED  
                '"' STRING(ttMaterial.iFormNo) + "/" +  string(ttMaterial.iBlankNo)              '",'
                '"' STRING(ttMaterial.cMaterial)              '",'           
                '"' STRING(ttMaterial.dQtyStd)                       '",'
                '"' STRING(ttMaterial.dQtyAct)                       '",'
                '"' STRING(ttMaterial.dQtyVar)            '",' 
                '"' STRING(ttMaterial.dCostStd )            '",'
                '"' STRING(ttMaterial.dCostAct )            '",'
                '"' STRING(ttMaterial.dCostVar )            '",' SKIP.                     
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList C-Win 
PROCEDURE GetSelectionList :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTmpList AS cha NO-UNDO.

    EMPTY TEMP-TABLE ttRptSelected.
    cTmpList = cTextListToSelect.
    iColumnLength = 0.

    DO i = 1 TO NUM-ENTRIES(cTextListToSelect) /* IN FRAME {&FRAME-NAME}*/ :


        CREATE ttRptSelected.
        ASSIGN 
            ttRptSelected.TextList        = ENTRY(i,cTmpList)
            ttRptSelected.FieldList       = ENTRY(i,cFieldListToSelect) //ttRptList.FieldList
            ttRptSelected.FieldLength     = int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
            ttRptSelected.DisplayOrder    = i
            ttRptSelected.HeadingFromLeft = IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
            iColumnLength                 = iColumnLength + ttRptSelected.FieldLength + 1.
        .        

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ************************  Function Implementations ***************** */



