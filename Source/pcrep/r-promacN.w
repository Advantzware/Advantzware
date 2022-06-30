&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: pcrep\r-promac.w

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
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE tt-mch-srt NO-UNDO LIKE mch-srt 
    FIELD i-no       LIKE mch-srt.job-no
    FIELD start-time AS INTEGER
    FIELD start-date AS DATE 
    FIELD start-date-sort AS DATE.

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

DEFINE VARIABLE v-print-fmt    AS CHARACTER.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.

DEFINE STREAM excel.

DEFINE STREAM excel.
DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

ASSIGN 
    cTextListToSelect  = "M Code,FG Item,Job #,Cust#,MR Std,MR Acl,MR Eff%,RUN Std,RUN Acl," +
                           "RUN Eff%,MR&RUN Std,MR&RUN Acl,MR&RUN Eff%,D/T Acl,D/T Eff%,Acl Qty,Exptd Qty,MR-C,RUNC," +
                           "Total Machine Hours,Total Labor Hours,Pieces per Hour,MSF,MSF per Hour,Number On," +
                           "Kicks per Hour,Pieces per Man Hour,MR Waste,Run Waste,Total Waste,% Waste,Date,User ID,Shift" 
    cFieldListToSelect = "m-cod,ino,job,cust-no,mr-stn,mr-acl,mr-eff,run-stnd,run-acl," +
                            "run-eff,mr&-stnd,mr&-acl,mr&-eff,dt-acl,dt-eff,acl-qty,exp-qty,mr-comp,run-comp," +
                            "ttl-mch-hrs,ttl-lbr-hrs,pic-per-hrs,msf,msf-per-hrs,nbr-on," +
                            "kik-per-hrs,pic-per-man-hrs,mr-wst,run-wst,ttl-wst,%wst,date,user-id,shift"
    cFieldLength       = "6,15,13,8,8,8,8,8,8," + "8,10,10,11,8,8,11,11,4,4," + "19,17,15,9,12,10," + "14,19,9,9,11,9,10,10,5"
    cFieldType         = "c,c,c,c,i,i,i,i,i," + "i,i,i,i,i,i,i,i,c,c," + "i,i,i,i,i,i," + "i,i,i,i,i,i,c,c,i"
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "M Code,FG Item,Job #,MR Std,MR Acl,MR Eff%,RUN Std,RUN Acl," +
                           "RUN Eff%,MR&RUN Std,MR&RUN Acl,MR&RUN Eff%,D/T Acl,D/T Eff%,Acl Qty,Exptd Qty"    .
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
end_mach begin_date end_date begin_user-id end_user-id tb_sel-per tb_donotcarry sl_avail Btn_Def ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest ~
tb_OpenCSV fi_file tbAutoClose btn-ok btn-cancel rd-job-timedt
&Scoped-Define DISPLAYED-OBJECTS begin_dept end_dept begin_mach end_mach ~
begin_date end_date begin_user-id end_user-id tb_sel-per tb_donotcarry sl_avail sl_selected rd-dest ~
tb_OpenCSV fi_file tbAutoClose rd-job-timedt

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
    ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE BUTTON Btn_Add 
    LABEL "&Add >>" 
    SIZE 16 BY 1.1.

DEFINE BUTTON Btn_Def 
    LABEL "&Default" 
    SIZE 16 BY 1.1.

DEFINE BUTTON btn_down 
    LABEL "Move Down" 
    SIZE 16 BY 1.1.

DEFINE BUTTON Btn_Remove 
    LABEL "<< &Remove" 
    SIZE 16 BY 1.1.

DEFINE BUTTON btn_Up 
    LABEL "Move Up" 
    SIZE 16 BY 1.1.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_dept     AS CHARACTER FORMAT "X(4)" 
    LABEL "Beginning Department" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_mach     AS CHARACTER FORMAT "X(6)" 
    LABEL "Beginning Machine" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_user-id  AS CHARACTER FORMAT "X(10)" 
    LABEL "Beginning Userid" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_dept       AS CHARACTER FORMAT "X(4)" INITIAL "zzzz" 
    LABEL "Ending Department" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_mach       AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
    LABEL "Ending Machine" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_user-id    AS CHARACTER FORMAT "X(10)" INITIAL "zzzzzzzzzz" 
    LABEL "Ending Userid" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\ProductivityByMachine.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_print      AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd-job-timedt  AS CHARACTER INITIAL "1" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Job Number", "1",
    "Job Date and Time", "2"
    SIZE 40 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 17 BY 4.52 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.05.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 8.81.

DEFINE VARIABLE sl_avail      AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected   AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_donotcarry AS LOGICAL   INITIAL NO 
    LABEL "Zero Standard MR from Previous Date?" 
    VIEW-AS TOGGLE-BOX
    SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE tbAutoClose   AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.     

DEFINE VARIABLE tb_excel      AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 FGCOLOR 15 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV    AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.8 BY .81 NO-UNDO.

DEFINE VARIABLE tb_sel-per    AS LOGICAL   INITIAL NO 
    LABEL "Summarize?" 
    VIEW-AS TOGGLE-BOX
    SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm  AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 23 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_dept AT ROW 2.52 COL 25 COLON-ALIGNED HELP
    "Enter Beginning Department"
    end_dept AT ROW 2.52 COL 68 COLON-ALIGNED HELP
    "Enter Ending Department"
    begin_mach AT ROW 3.48 COL 25 COLON-ALIGNED HELP
    "Enter Beginning Machine"
    end_mach AT ROW 3.48 COL 68 COLON-ALIGNED HELP
    "Enter Ending Machine"
    begin_date AT ROW 4.43 COL 25 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 4.43 COL 68 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_user-id AT ROW 5.35 COL 25 COLON-ALIGNED HELP
    "Enter Beginning User ID"
    end_user-id AT ROW 5.35 COL 68 COLON-ALIGNED HELP
    "Enter Ending User ID"
    tb_sel-per AT ROW 6.54 COL 30
    tb_donotcarry AT ROW 7.50 COL 30 WIDGET-ID 2
    rd-job-timedt AT ROW 8.50 COL 30 NO-LABELS
    sl_avail AT ROW 11.29 COL 3 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 11.29 COL 40.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 11.29 COL 60.6 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 12.29 COL 40.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 13.29 COL 40.2 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 14.33 COL 40.2 WIDGET-ID 40
    btn_down AT ROW 15.33 COL 40.2 WIDGET-ID 42
    rd-dest AT ROW 17.14 COL 4.6 NO-LABELS
    lv-ornt AT ROW 17.19 COL 50 NO-LABELS
    lines-per-page AT ROW 17.19 COL 87 COLON-ALIGNED
    lv-font-no AT ROW 17.19 COL 41 COLON-ALIGNED
    lv-font-name AT ROW 18.38 COL 29 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 19.71 COL 27
    tb_excel AT ROW 19.57 COL 91 RIGHT-ALIGNED
    tb_OpenCSV AT ROW 20.62 COL 91.4 RIGHT-ALIGNED
    fi_file AT ROW 20.57 COL 25 COLON-ALIGNED HELP
    "Enter File Name"
    tbAutoClose AT ROW 21.95 COL 27 WIDGET-ID 64     
    btn-ok AT ROW 22.86 COL 26.8
    btn-cancel AT ROW 22.86 COL 53
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 10.57 COL 60.2 WIDGET-ID 44
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 16.52 COL 4.2
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 5
    BGCOLOR 15 
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 10.57 COL 3 WIDGET-ID 38
    RECT-6 AT ROW 16.91 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95 BY 27.91
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
        TITLE              = "Productivity By Machine (D-R-4)"
        HEIGHT             = 23.38
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
    begin_dept:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_mach:PRIVATE-DATA IN FRAME FRAME-A = "parm".
ASSIGN 
    begin_user-id:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_dept:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_mach:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_user-id:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */

ASSIGN 
    tb_donotcarry:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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

ASSIGN 
    tb_sel-per:PRIVATE-DATA IN FRAME FRAME-A = "parm".
                
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
ON END-ERROR OF C-Win /* Productivity By Machine */
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
ON WINDOW-CLOSE OF C-Win /* Productivity By Machine */
    DO:
        /* This event will close the window and terminate the procedure.  */ 
        IF VALID-HANDLE(hdOutputProcs) THEN  
            DELETE PROCEDURE hdOutputProcs.
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


&Scoped-define SELF-NAME begin_user-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_user-id C-Win
ON LEAVE OF begin_user-id IN FRAME FRAME-A /* Beginning Department */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_user-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_user-id C-Win
ON LEAVE OF end_user-id IN FRAME FRAME-A /* Beginning Department */
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
            tb_excel = YES.
        END.
        ELSE  tb_excel = NO.

        RUN GetSelectionList.
        RUN run-report. 
        STATUS DEFAULT "Processing Complete".
        SESSION:SET-WAIT-STATE ("").

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
                    END. /* IF NOT tb_OpenCSV THEN */
                    ELSE DO:
                        OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                    END. /* ELSE DO: */
                END. /* WHEN 3 THEN DO: */
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &type= ''
                            &begin_cust=begin_mach
                            &END_cust= begin_mach
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = ''
                             &begin_cust= begin_mach
                             &END_cust=begin_mach
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = ''
                                  &begin_cust= begin_mach
                                  &END_cust=begin_mach
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }
                    END.
                END.
            WHEN 6 THEN RUN OUTPUT-to-port.

        END CASE.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.

    /*
    DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
      IF sl_avail:IS-SELECTED(i) AND
        (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR sl_selected:NUM-ITEMS = 0) THEN
      /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
          cSelectedList = cSelectedList +
                          entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
    END.
    cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
    sl_selected:LIST-ITEM-PAIRS = cSelectedList.
    sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def C-Win
ON CHOOSE OF Btn_Def IN FRAME FRAME-A /* Default */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        RUN DisplaySelectionDefault.  /* task 04041406 */ 
        RUN DisplaySelectionList2 .
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down C-Win
ON CHOOSE OF btn_down IN FRAME FRAME-A /* Move Down */
    DO:
        RUN Move-Field ("Down").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove C-Win
ON CHOOSE OF Btn_Remove IN FRAME FRAME-A /* << Remove */
    DO:
        /* DO i = sl_selected:NUM-ITEMS TO 1 BY -1 WITH FRAME {&FRAME-NAME}:
           IF sl_selected:IS-SELECTED(i) THEN
           ldummy = sl_selected:DELETE(i).
         END
         */
        APPLY "DEFAULT-ACTION" TO sl_selected  .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
    DO:
        RUN Move-Field ("Up").
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
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
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

&Scoped-define SELF-NAME rd-job-timedt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-job-timedt C-Win
ON LEAVE OF rd-job-timedt IN FRAME FRAME-A
    DO:
        ASSIGN rd-job-timedt.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-job-timedt C-Win
ON VALUE-CHANGED OF rd-job-timedt IN FRAME FRAME-A
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

&Scoped-define SELF-NAME tb_donotcarry
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_donotcarry C-Win
ON VALUE-CHANGED OF tb_donotcarry IN FRAME FRAME-A /* Zero Standard MR from Previous Date? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail C-Win
ON DEFAULT-ACTION OF sl_avail IN FRAME FRAME-A
    DO:
  
        IF (NOT CAN-DO(sl_selected:LIST-ITEMs,{&SELF-NAME}:SCREEN-VALUE) OR
            sl_selected:NUM-ITEMS = 0)
            THEN ASSIGN ldummy = sl_selected:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
                ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
                /* sl_selected:SCREEN-VALUE = sl_selected:ENTRY(sl_selected:NUM-ITEMS) */
                .

  
    /* for pairs
        DEF VAR cSelectedList AS CHARACTER NO-UNDO.
        cSelectedList = sl_Selected:LIST-ITEM-PAIRS.
        DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
        IF sl_avail:IS-SELECTED(i) AND
          (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR
             sl_selected:NUM-ITEMS = 0) THEN
        /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
            cSelectedList = cSelectedList +
                            entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
        MESSAGE i sl_avail:IS-SELECTED(i) NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i))
            sl_selected:NUM-ITEMS
            SKIP cSelectedList
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
      cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
      sl_selected:LIST-ITEM-PAIRS = cSelectedList.
      sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
      */

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected C-Win
ON DEFAULT-ACTION OF sl_selected IN FRAME FRAME-A
    DO:
        DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
            IF {&SELF-NAME}:IS-SELECTED(i) THEN 
            DO:
                ASSIGN 
                    ldummy = sl_Avail:add-last({&SELF-NAME}:SCREEN-VALUE)
                    ldummy = /*{&SELF-NAME}:DELETE(i)*/
                       {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
                    .
            END.           
        END.
        IF {&SELF-NAME}:NUM-ITEMS NE 0 THEN
            ASSIGN
                {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}:ENTRY(1)
                .
    

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
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Auto Run Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sel-per
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sel-per C-Win
ON VALUE-CHANGED OF tb_sel-per IN FRAME FRAME-A /* Show Job detail For Selected Period? */
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

    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "DR4" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
  
    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_dept.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault C-Win 
PROCEDURE DisplaySelectionDefault :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
  
    DO iCount = 1 TO NUM-ENTRIES(cTextListToDefault):

        cListContents = cListContents +                   
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToDefault)   .
    END.            
    sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList C-Win 
PROCEDURE DisplaySelectionList :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.

    IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN 
    DO:
     
        RETURN.
    END.

    EMPTY TEMP-TABLE ttRptList.

    DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

        cListContents = cListContents +
            /* (IF cListContents = "" THEN ""  ELSE ",") +
             ENTRY(iCount,cTextListToSelect) + "," +
             ENTRY(1,cFieldListToSelect)
             paris */
                     
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
            .
    END.
  
    /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */
  
    sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 C-Win 
PROCEDURE DisplaySelectionList2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
  
    IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN 
    DO:
        RETURN.
    END.
        
    EMPTY TEMP-TABLE ttRptList.

    DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

        cListContents = cListContents +
            /* (IF cListContents = "" THEN ""  ELSE ",") +
             ENTRY(iCount,cTextListToSelect) + "," +
             ENTRY(1,cFieldListToSelect)
             paris */
                     
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
            .
    END.
  
    /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */
  
    sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

    DO iCount = 1 TO sl_selected:NUM-ITEMS:
        ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
    END.

    {sys/ref/SelColCorrect.i}

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
    DISPLAY begin_dept end_dept begin_mach end_mach begin_date end_date begin_user-id end_user-id  
        tb_sel-per tb_donotcarry sl_avail sl_selected rd-dest tb_OpenCSV 
        fi_file tbAutoClose rd-job-timedt
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_dept end_dept begin_mach end_mach begin_date 
        end_date begin_user-id end_user-id tb_sel-per tb_donotcarry sl_avail Btn_Def  
        sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest tb_OpenCSV fi_file 
        tbAutoClose btn-ok btn-cancel rd-job-timedt
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
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
    DEFINE VARIABLE cTmpList AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE ttRptSelected.
    cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    iColumnLength = 0.

    DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
        FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.     
  
        CREATE ttRptSelected.
        ASSIGN 
            ttRptSelected.TextList        = ENTRY(i,cTmpList)
            ttRptSelected.FieldList       = ttRptList.FieldList
            ttRptSelected.FieldLength     = int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
            ttRptSelected.DisplayOrder    = i
            ttRptSelected.HeadingFromLeft = IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
            iColumnLength                 = iColumnLength + ttRptSelected.FieldLength + 1.
        .        
           
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field C-Win 
PROCEDURE Move-Field :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER move AS CHARACTER NO-UNDO.

    DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME}
        WITH FRAME {&FRAME-NAME}:
        IF sl_selected:IS-SELECTED(i) THEN
        DO:
            IF move = "Down" AND i NE sl_selected:NUM-ITEMS THEN
                ASSIGN
                    ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
                    ldummy                   = sl_selected:DELETE(i)
                    sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
                    .
            ELSE
                IF move = "Up" AND i NE 1 THEN
                    ASSIGN
                        ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i - 1)
                        ldummy                   = sl_selected:DELETE(i + 1)
                        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i - 1)
                        .
            LEAVE.
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
             
         IF NOT OKpressed THEN  RETURN NO-APPLY.  */
     
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ------------------------------------------------ pc/rep/mch-prd.p 8/94 gb */
    /* Production by Department/Machine Report                               */
    /* -------------------------------------------------------------------------- */
    /*{sys/form/r-topw.f}*/

    DEFINE BUFFER b-mch-srt  FOR mch-srt.
    DEFINE BUFFER bf-mch-act FOR mch-act .
    
    DEFINE VARIABLE v-date         AS DATE      EXTENT 2 FORMAT "99/99/9999" NO-UNDO.
    DEFINE VARIABLE v-dept         AS ch        FORMAT 'x(4)' EXTENT 2 INIT [" ","zzzz"].
    DEFINE VARIABLE v-mach         LIKE mach.m-code EXTENT 2 INIT [" ","zzzzzz"].
    DEFINE VARIABLE v-show         AS LOGICAL   FORMAT "Y/N" INIT YES NO-UNDO.
    DEFINE VARIABLE v-prt-job      AS LOGICAL   FORMAT "Job/FG" INIT YES NO-UNDO.
    DEFINE VARIABLE v-prt-both     AS LOGICAL   FORMAT "Both/FG" INIT NO NO-UNDO.

    DEFINE VARIABLE job-mr-std     AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE job-run-std    AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE job-mr-act     AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE job-run-act    AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE job-dt-act     AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE job-qty-prod   AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE job-qty-expect AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE mch-mr-std     AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE mch-run-std    AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE mch-mr-act     AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE mch-run-act    AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE mch-dt-act     AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE mch-qty-prod   AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE mch-qty-expect AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE dpt-mr-std     AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE dpt-run-std    AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE dpt-mr-act     AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE dpt-run-act    AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE dpt-dt-act     AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE dpt-qty-prod   AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE dpt-qty-expect AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE mr-eff         AS DECIMAL   FORMAT "->>>>9.9" NO-UNDO.
    DEFINE VARIABLE run-eff        AS DECIMAL   FORMAT "->>>>9.9" NO-UNDO.
    DEFINE VARIABLE tot-eff        AS DECIMAL   FORMAT "->>>>9.9" NO-UNDO.
    DEFINE VARIABLE dt-eff         AS DECIMAL   FORMAT "->>>>9.9" NO-UNDO.
    DEFINE VARIABLE tot-std-hrs    AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE tot-act-hrs    AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE a              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE b              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdr-tit        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdr-tit2       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdr-tit3       AS CHARACTER NO-UNDO.

    DEFINE VARIABLE v-mrcomp       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-runcomp      AS CHARACTER NO-UNDO .
    DEFINE VARIABLE v-mrwaste      AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-runwaste     AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dt-date        AS DATE      NO-UNDO .
    DEFINE VARIABLE cUserId        AS CHARACTER NO-UNDO .
    DEFINE VARIABLE iShift         AS INTEGER  NO-UNDO .

    DEFINE VARIABLE cDisplay       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str-tit4       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line       AS cha       FORM "x(350)" NO-UNDO.
    {sys/form/r-top5DL3.f} 
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE v-calmsf       AS DECIMAL   FORMAT ">>>>>>.99" NO-UNDO .
    DEFINE VARIABLE v-num-up       AS INTEGER   FORMAT ">>>,>>9" NO-UNDO .
    DEFINE VARIABLE v-act-lab-cost AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-pic-per-hrs  AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-msf-per-hrs  AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-kik-per-hrs  AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-wst          AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-per-man-hrs  AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE v-cust-no      AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cFileName      LIKE fi_file NO-UNDO .

    RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

    /*FORM HEADER
         hdr-tit format "x(142)" skip
         hdr-tit2 format "x(142)" skip
         hdr-tit3 format "x(142)"
    
        WITH FRAME r-top WIDTH 180.*/


    SESSION:SET-WAIT-STATE ("general").
    /*ASSIGN
        v-prt-job   = rd_print EQ "Job#"
        v-prt-both  = rd_print EQ "Both"  . */

    /*IF tb_excel THEN DO:
       IF v-prt-both THEN DO:
       OUTPUT STREAM excel TO VALUE(cFileName).
       EXPORT STREAM excel DELIMITER ","
           "Mach. Code"
           "FG Item"
           "Job #"
           "MR Stndrd"
           "MR Actual"
           "MR Eff%"
           "RUN Stndrd"
           "RUN Actual"
           "RUN Eff%"
           "MR&RUN Stndrd"
           "MR&RUN Actual"
           "MR&RUN Eff%"
           "D/T Actual"
           "D/T Eff%"
           "Actual Qty"
           "Expected Qty"
           SKIP.
       END.
    
       IF NOT v-prt-both THEN DO:
        IF v-prt-job THEN do:
        OUTPUT STREAM excel TO VALUE(cFileName).
        EXPORT STREAM excel DELIMITER ","
           "Mach. Code"
           "Job #"
           "MR Stndrd"
           "MR Actual"
           "MR Eff%"
           "RUN Stndrd"
           "RUN Actual"
           "RUN Eff%"
           "MR&RUN Stndrd"
           "MR&RUN Actual"
           "MR&RUN Eff%"
           "D/T Actual"
           "D/T Eff%"
           "Actual Qty"
           "Expected Qty"
           SKIP.
        END.
        ELSE do:
        OUTPUT STREAM excel TO VALUE(cFileName).
        EXPORT STREAM excel DELIMITER ","
           "Mach. Code"
           "FG Item"
           "MR Stndrd"
           "MR Actual"
           "MR Eff%"
           "RUN Stndrd"
           "RUN Actual"
           "RUN Eff%"
           "MR&RUN Stndrd"
           "MR&RUN Actual"
           "MR&RUN Eff%"
           "D/T Actual"
           "D/T Eff%"
           "Actual Qty"
           "Expected Qty"
           SKIP.
        END.
       END.
    
    END. */
    ASSIGN
        str-tit2  = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-dept[1] = begin_dept
        v-dept[2] = end_dept
        v-mach[1] = begin_mach
        v-mach[2] = end_mach
        v-date[1] = begin_date
        v-date[2] = end_date
        v-show    = NOT tb_sel-per .
 
    /*hdr-tit  = "MACH                    <---MAKE READY HOURS--->  " +
               "<------RUN HOURS------->  <----MR & RUN HOURS---->  " +
               "<--D/T HOURS-->       ACTUAL    EXPECTED"
    hdr-tit2 = "CODE   " +
               (if v-prt-job then "    JOB #      " else "FG ITEM#       ") +
               "   STNDRD  ACTUAL   EFF %   " +
               " STNDRD  ACTUAL   EFF %    STNDRD  ACTUAL   EFF %   " +
               " ACTUAL   EFF %     QUANTITY    QUANTITY"
    hdr-tit3 = fill("-", 142). */

    DEFINE VARIABLE cslist AS CHARACTER NO-UNDO.
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        IF LOOKUP(ttRptSelected.TextList, "MR Std,MR Acl,MR Eff%,RUN Std,RUN Acl,RUN Eff%,MR&RUN Std,MR&RUN Acl,MR&RUN Eff%,D/T Acl,D/T Eff%,Acl Qty,Exptd Qty" ) <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
     
        IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
            THEN ASSIGN str-tit4    = str-tit4 + ttRptSelected.TextList + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + "," .        
        ELSE 
            ASSIGN str-tit4    = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + ","
                .        
        cSlist = cSlist + ttRptSelected.FieldList + ",".
         
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}
                               
    IF td-show-parm THEN RUN show-param.

    DISPLAY "" WITH FRAME r-top.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.


    FOR EACH mch-srt:
        DELETE mch-srt.
    END.
    FOR EACH tt-mch-srt:
        DELETE tt-mch-srt.
    END.
  
    FOR EACH mch-act WHERE
        mch-act.company EQ cocode AND
        mch-act.m-code  GE v-mach[1] AND
        mch-act.m-code  LE v-mach[2] AND
        mch-act.op-date GE v-date[1] AND
        mch-act.op-date LE v-date[2] AND
        mch-act.dept    GE v-dept[1] AND
        mch-act.dept    LE v-dept[2] AND
        mch-act.USER-ID GE begin_user-id AND
        mch-act.USER-ID LE end_user-id  
        USE-INDEX dly-idx
        NO-LOCK,
        FIRST mach WHERE
        mach.company EQ cocode AND
        mach.loc     EQ locode AND
        mach.m-code  EQ mch-act.m-code
        NO-LOCK:
        {custom/statusMsg.i " 'Processing Machine#  '  + mch-act.m-code "}             
        FIND FIRST mch-srt WHERE
            mch-srt.dept EQ mch-act.dept AND
            mch-srt.m-code EQ mch-act.m-code AND
            mch-srt.job-no EQ mch-act.job-no AND
            mch-srt.job-no2 EQ mch-act.job-no2 AND
            mch-srt.frm EQ mch-act.frm AND
            (mch-srt.blank-no EQ mch-act.blank-no OR
            mach.p-type NE "B" OR
            mch-act.blank-no EQ 0) AND
            mch-srt.pass EQ mch-act.pass AND 
            mch-srt.op-date EQ mch-act.op-date
            NO-ERROR.
        IF NOT AVAILABLE mch-srt THEN
        DO:              
            CREATE mch-srt.
            ASSIGN 
                mch-srt.dept     = mch-act.dept
                mch-srt.m-code   = mch-act.m-code
                mch-srt.job      = mch-act.job
                mch-srt.job-no   = mch-act.job-no
                mch-srt.job-no2  = mch-act.job-no2
                mch-srt.frm      = mch-act.frm
                mch-srt.blank-no = IF mach.p-type EQ "B"    AND
                                     mch-act.blank-no EQ 0 THEN 1
                                                           ELSE mch-act.blank-no
                mch-srt.pass     = mch-act.pass
                mch-srt.op-date = mch-act.op-date.
        
        END.
        FIND job-code WHERE job-code.code EQ mch-act.code
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-code THEN NEXT.
        IF job-code.cat EQ "RUN" THEN
        DO:
            mch-srt.run-act-hr = mch-srt.run-act-hr + mch-act.hours.
            mch-srt.qty-prod   = mch-srt.qty-prod +
                IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty.
        END.
        ELSE IF job-code.cat EQ "MR" THEN
                mch-srt.mr-act-hr  = mch-srt.mr-act-hr + mch-act.hours.
            ELSE
                mch-srt.act-dt-hr  = mch-srt.act-dt-hr + mch-act.hours.
    /*   if job-code.cat eq "RUN" then
               do:
               v-runwaste = v-runwaste + mch-act.waste .
           end.
           else if job-code.cat eq "MR" then
               v-mrwaste = v-mrwaste + mch-act.waste . */
    END.
    
    ASSIGN
        v-pic-per-hrs = 0
        v-msf-per-hrs = 0
        v-kik-per-hrs = 0
        v-wst         = 0
        v-per-man-hrs = 0.
            
    FOR EACH mch-srt,

        FIRST job WHERE   /* CTS added join */
        job.company EQ cocode AND
        job.job-no  EQ mch-srt.job-no AND
        job.job-no2 EQ mch-srt.job-no2 NO-LOCK,
        FIRST mach FIELDS(p-type) WHERE
        mach.company EQ cocode AND
        mach.loc     EQ locode AND
        mach.m-code  EQ mch-srt.m-code
        NO-LOCK:

        {custom/statusMsg.i " 'Processing Machine#  '  + mch-srt.m-code "}
     
    

        FIND FIRST job-mch WHERE job-mch.company  = cocode AND
            job-mch.job      EQ mch-srt.job AND
            job-mch.job-no  EQ mch-srt.job-no AND
            job-mch.job-no2 EQ mch-srt.job-no2 AND
            job-mch.frm      = mch-srt.frm AND
            (job-mch.blank-no = mch-srt.blank-no OR
            mch-srt.blank-no = 0) AND
            job-mch.m-code   = mch-srt.m-code AND
            job-mch.pass     = mch-srt.pass
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ cocode AND
                job-mch.job      EQ mch-srt.job AND
                job-mch.job-no  EQ mch-srt.job-no AND
                job-mch.job-no2 EQ mch-srt.job-no2 AND
                job-mch.frm      EQ mch-srt.frm AND
                (job-mch.blank-no = mch-srt.blank-no OR
                mch-srt.blank-no = 0) AND
                job-mch.m-code   EQ mch-srt.m-code
                NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ cocode AND
                job-mch.job     EQ mch-srt.job AND
                job-mch.job-no  EQ mch-srt.job-no AND
                job-mch.job-no2 EQ mch-srt.job-no2 AND
                job-mch.frm     EQ mch-srt.frm AND
                job-mch.m-code  EQ mch-srt.m-code AND
                job-mch.speed   NE 0
                NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ cocode AND
                job-mch.job     EQ mch-srt.job AND
                job-mch.job-no  EQ mch-srt.job-no AND
                job-mch.job-no2 EQ mch-srt.job-no2 AND
                job-mch.frm     EQ mch-srt.frm AND
                job-mch.m-code  EQ mch-srt.m-code
                NO-LOCK NO-ERROR.
        IF AVAILABLE job-mch THEN
        DO:
            IF mch-srt.qty-prod NE 0 THEN
            DO:
                IF CAN-FIND(FIRST mach WHERE
                    mach.company EQ cocode AND
                    mach.loc     EQ locode AND
                    mach.m-code  EQ job-mch.m-code AND
                    mach.therm   EQ YES AND
                    (mach.p-type EQ "R" OR mach.dept[1] EQ "LM")) THEN
                    FOR EACH job-mat FIELDS(i-no len) WHERE
                        job-mat.company EQ cocode AND
                        job-mat.job = mch-srt.job AND
                        job-mat.frm EQ mch-srt.frm AND
                        job-mat.frm GT 0 AND
                        job-mat.len GT 0
                        NO-LOCK,
                        FIRST ITEM FIELDS(mat-type) WHERE
                        item.company EQ cocode AND
                        item.i-no EQ job-mat.i-no
                        NO-LOCK
         
                        BREAK BY job-mat.frm
                        BY item.mat-type
                        BY job-mat.j-no
                        BY job-mat.rec_key:
         
                        mch-srt.run-std-hr = (mch-srt.qty-prod * job-mat.len / 12) / job-mch.speed.
                        LEAVE.
                    END.
                ELSE
                    ASSIGN mch-srt.run-std-hr = mch-srt.qty-prod / job-mch.speed.
            END.
            ELSE
                ASSIGN mch-srt.run-std-hr = job-mch.run-hr.

            IF NOT(tb_donotcarry AND
                CAN-FIND(FIRST mch-act WHERE
                mch-act.company EQ job-mch.company AND
                mch-act.dept EQ mch-srt.dept AND
                mch-act.m-code EQ mch-srt.m-code AND
                mch-act.job EQ mch-srt.job AND 
                mch-act.job-no EQ mch-srt.job-no AND
                mch-act.job-no2 EQ mch-srt.job-no2 AND
                mch-act.frm EQ mch-srt.frm AND
                (mch-srt.blank-no = IF mach.p-type EQ "B"    AND
                mch-act.blank-no EQ 0 THEN 1
            ELSE mch-act.blank-no) AND
                mch-act.pass EQ mch-srt.pass AND
                mch-act.op-date LT begin_date)) THEN
                mch-srt.mr-std-hr  = job-mch.mr-hr.

            mch-srt.qty-expect = IF job-mch.speed NE 0 THEN
                (IF mch-srt.run-act-hr NE 0
                THEN mch-srt.run-act-hr
                ELSE mch-srt.run-std-hr) * job-mch.speed
                ELSE job-mch.run-qty.
        END.
    
     

        {sys/inc/roundup.i mch-srt.qty-expect}

        IF mch-srt.run-std-hr EQ ? THEN mch-srt.run-std-hr = 0.
                                    
        a = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', mch-srt.job-no, mch-srt.job-no2)) .
     
        /*  if not v-prt-job AND NOT v-prt-both then
          for each job-hdr
              where job-hdr.company eq cocode
                and job-hdr.job     eq mch-srt.job
                and job-hdr.job-no  eq mch-srt.job-no
                and job-hdr.job-no2 eq mch-srt.job-no2
              no-lock
                    
              by job-hdr.blank-no desc
              by job-hdr.frm      desc:
                    
            a = job-hdr.i-no.
                  
            if job-hdr.frm       eq mch-srt.frm           and
               (job-hdr.blank-no eq mch-srt.blank-no or
                job-hdr.blank-no eq 0)                    then leave.
          end. */
    
        /*  IF v-prt-both THEN */
     
        FOR EACH job-hdr
            WHERE job-hdr.company EQ cocode
            AND job-hdr.job     EQ mch-srt.job
            AND job-hdr.job-no  EQ mch-srt.job-no
            AND job-hdr.job-no2 EQ mch-srt.job-no2
            NO-LOCK
               
            BY job-hdr.blank-no DESCENDING
            BY job-hdr.frm      DESCENDING:
              
            b = job-hdr.i-no.
            IF job-hdr.frm       EQ mch-srt.frm           AND
                (job-hdr.blank-no EQ mch-srt.blank-no OR
                job-hdr.blank-no EQ 0)                    THEN LEAVE.
        END.

        mch-srt.job-no = a.
        CREATE tt-mch-srt .
        BUFFER-COPY mch-srt TO tt-mch-srt .
        ASSIGN
            tt-mch-srt.i-no = b
            tt-mch-srt.start-date = mch-srt.op-date
            .  
        IF rd-job-timedt EQ "2" THEN 
        DO:  
            IF AVAILABLE job-mch THEN
                ASSIGN
                    tt-mch-srt.start-date-sort = job-mch.start-date
                    tt-mch-srt.start-time = job-mch.start-time . 
        END. 
    END.
  
    PUT SKIP.
     
    ASSIGN 
        v-mrcomp       = "" 
        v-runcomp      = "" 
        v-calmsf       = 0
        v-num-up       = 0 
        v-act-lab-cost = 0 . 

    FOR EACH tt-mch-srt USE-INDEX dept-idx
        BREAK BY tt-mch-srt.dept
        BY tt-mch-srt.m-code
        BY tt-mch-srt.start-date
        BY tt-mch-srt.start-date-sort
        BY tt-mch-srt.start-time
        BY tt-mch-srt.job-no :

        {custom/statusMsg.i " 'Processing Machine#  '  + tt-mch-srt.m-code "}

        FIND FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ tt-mch-srt.i-no
            NO-LOCK NO-ERROR.

        v-calmsf = /*v-calmsf
                                     +*/ (tt-mch-srt.qty-prod *
            (IF AVAILABLE itemfg THEN itemfg.t-sqft
            ELSE 1) / 1000).

        FIND FIRST job WHERE 
            job.company EQ cocode AND
            job.job-no  EQ SUBSTRING(tt-mch-srt.job-no,1,iJobLen) AND
            job.job-no2 EQ tt-mch-srt.job-no2 NO-LOCK NO-ERROR .
        ASSIGN 
            v-cust-no = "" .
        IF AVAILABLE job THEN 
        DO:
             
            FIND FIRST eb WHERE eb.company EQ cocode
                AND eb.est-no EQ job.est-no
                AND eb.stock-no EQ tt-mch-srt.i-no NO-LOCK NO-ERROR .
            IF AVAILABLE eb THEN
                ASSIGN v-num-up = eb.num-up .
            ELSE 
                v-num-up = 0 .
         
            FOR EACH misc-act FIELDS(cost)
                WHERE misc-act.company EQ cocode
                AND misc-act.job     EQ job.job
                NO-LOCK:
                     
            /*v-act-lab-cost = v-act-lab-cost + misc-act.cost.*/
         
            END.
            FIND FIRST job-hdr
                WHERE job-hdr.company EQ cocode
                AND job-hdr.i-no     EQ tt-mch-srt.i-no
                AND job-hdr.job-no  EQ job.job-no
                AND job-hdr.job-no2 EQ job.job-no2 NO-LOCK NO-ERROR .
            IF AVAILABLE job-hdr THEN
                v-cust-no = job-hdr.cust-no  .

        END.

        FIND FIRST job-mch WHERE job-mch.company  = cocode  AND
            job-mch.job      EQ tt-mch-srt.job  AND 
            job-mch.job-no   EQ SUBSTRING(tt-mch-srt.job-no,1,iJobLen)  AND
            job-mch.job-no2  EQ tt-mch-srt.job-no2  AND 
            job-mch.frm      = tt-mch-srt.frm AND 
            (job-mch.blank-no = tt-mch-srt.blank-no OR
            mch-srt.blank-no = 0) AND              
            job-mch.m-code   = tt-mch-srt.m-code AND 
            job-mch.pass     = tt-mch-srt.pass 
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ cocode AND
                job-mch.job      EQ tt-mch-srt.job AND
                job-mch.job-no   EQ SUBSTRING(tt-mch-srt.job-no,1,iJobLen) AND
                job-mch.job-no2  EQ tt-mch-srt.job-no2 AND 
                job-mch.frm      EQ tt-mch-srt.frm AND
                (job-mch.blank-no = tt-mch-srt.blank-no OR
                mch-srt.blank-no = 0) AND
                job-mch.m-code   EQ tt-mch-srt.m-code
                NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ cocode AND
                job-mch.job     EQ tt-mch-srt.job AND
                job-mch.job-no  EQ SUBSTRING(tt-mch-srt.job-no,1,iJobLen) AND
                job-mch.job-no2 EQ tt-mch-srt.job-no2 AND 
                job-mch.frm     EQ tt-mch-srt.frm AND
                job-mch.m-code  EQ tt-mch-srt.m-code AND
                job-mch.speed   NE 0
                NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ cocode AND
                job-mch.job     EQ tt-mch-srt.job AND 
                job-mch.job-no  EQ SUBSTRING(tt-mch-srt.job-no,1,iJobLen)  AND
                job-mch.job-no2 EQ tt-mch-srt.job-no2 AND  
                job-mch.frm     EQ tt-mch-srt.frm AND
                job-mch.m-code  EQ tt-mch-srt.m-code        
                NO-LOCK NO-ERROR.  
        IF AVAILABLE job-mch THEN
        DO:
            ASSIGN                                   
                v-mrcomp  = STRING(job-mch.mr-complete)
                v-runcomp = STRING(job-mch.run-complete) .
        
        END.

        ASSIGN 
            v-runwaste     = 0 
            v-mrwaste      = 0 
            v-act-lab-cost = 0
            dt-date        = ? 
            cUserId        = ""
            iShift         = 0.
            
        FOR EACH bf-mch-act WHERE
            bf-mch-act.company EQ cocode AND
            bf-mch-act.dept EQ tt-mch-srt.dept AND
            bf-mch-act.m-code EQ tt-mch-srt.m-code AND
            bf-mch-act.op-date GE v-date[1] AND
            bf-mch-act.op-date LE v-date[2] AND
            bf-mch-act.job EQ tt-mch-srt.job AND 
            bf-mch-act.job-no EQ SUBSTRING(tt-mch-srt.job-no,1,iJobLen) AND
            bf-mch-act.job-no2 EQ tt-mch-srt.job-no2 AND
            bf-mch-act.frm EQ tt-mch-srt.frm AND
            (bf-mch-act.blank-no = tt-mch-srt.blank-no  OR
            mach.p-type NE "B" OR
            bf-mch-act.blank-no EQ 0) AND
            bf-mch-act.pass EQ tt-mch-srt.pass AND 
            bf-mch-act.op-date EQ tt-mch-srt.start-date  NO-LOCK:

            FIND job-code WHERE job-code.code EQ bf-mch-act.CODE NO-LOCK NO-ERROR.

            IF NOT AVAILABLE job-code THEN NEXT.
            IF job-code.cat EQ "RUN" THEN
            DO:
                v-runwaste = v-runwaste + bf-mch-act.waste .
            END.
            ELSE IF job-code.cat EQ "MR" THEN
                    v-mrwaste = v-mrwaste + bf-mch-act.waste .

            v-act-lab-cost = v-act-lab-cost + (bf-mch-act.hours * bf-mch-act.crew) .
            dt-date = bf-mch-act.op-date .
            cUserId = IF AVAILABLE bf-mch-act THEN  bf-mch-act.USER-ID ELSE "".
            iShift  = IF AVAILABLE bf-mch-act THEN  bf-mch-act.shift ELSE 0.

        END. /* FOR EACH bf-mch-act W */

        ASSIGN 
            job-mr-std     = job-mr-std + tt-mch-srt.mr-std-hr
            job-run-std    = job-run-std + tt-mch-srt.run-std-hr
            job-mr-act     = job-mr-act + tt-mch-srt.mr-act-hr
            job-run-act    = job-run-act + tt-mch-srt.run-act-hr
            job-dt-act     = job-dt-act + tt-mch-srt.act-dt-hr
            job-qty-prod   = job-qty-prod + tt-mch-srt.qty-prod
            job-qty-expect = job-qty-expect + tt-mch-srt.qty-expect.

        IF LAST-OF(tt-mch-srt.job-no) THEN 
        DO:
            IF job-run-act EQ 0 THEN
                job-run-std = 0.
            IF v-show THEN 
            DO:
                mr-eff  = (job-mr-std  / job-mr-act)  * 100.00.
                IF mr-eff EQ ? THEN mr-eff = 0.
                run-eff = (job-run-std / job-run-act) * 100.00.
                IF run-eff EQ ? THEN run-eff = 0.
                ASSIGN
                    tot-std-hrs = job-mr-std + job-run-std
                    tot-act-hrs = job-mr-act + job-run-act
                    tot-eff     = (tot-std-hrs / tot-act-hrs) * 100.00.
                IF tot-eff EQ ? THEN tot-eff = 0.
                dt-eff = (job-dt-act / tot-act-hrs) * 100.00.
                IF dt-eff EQ ? THEN dt-eff = 0.
          
                IF v-num-up = 0 THEN ASSIGN v-num-up = 1 .
                ASSIGN
                    v-pic-per-hrs = (job-qty-prod / (job-mr-act + job-run-act + job-dt-act))
                    v-msf-per-hrs = (v-calmsf / (job-mr-act + job-run-act + job-dt-act))
                    v-kik-per-hrs = ( /*job-qty-prod*/ v-pic-per-hrs  / v-num-up)
                    v-wst         = ((v-mrwaste + v-runwaste) / ( job-qty-prod)) * 100
                    v-per-man-hrs = (job-qty-prod / v-act-lab-cost) .
               

                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".
                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "m-cod"      THEN 
                            cVarValue = STRING(tt-mch-srt.m-code) .
                        WHEN "ino"        THEN 
                            cVarValue = STRING(tt-mch-srt.i-no) .  
                        WHEN "job"        THEN 
                            cVarValue = STRING(tt-mch-srt.job-no) .
                        WHEN "cust-no"    THEN 
                            cVarValue = IF v-cust-no <> ? THEN STRING(v-cust-no) ELSE "" .
                        WHEN "mr-stn"     THEN 
                            cVarValue = STRING(job-mr-std,"->>>9.9") .  
                        WHEN "mr-acl"     THEN 
                            cVarValue = STRING(job-mr-act,"->>>9.9") .       
                        WHEN "mr-eff"     THEN 
                            cVarValue = STRING(mr-eff,"->>>>9.9") .           
                        WHEN "run-stnd"   THEN 
                            cVarValue = STRING(job-run-std,"->>>9.9") .      
                        WHEN "run-acl"    THEN 
                            cVarValue = STRING(job-run-act,"->>>9.9") .      
                        WHEN "run-eff"    THEN 
                            cVarValue = STRING(run-eff,"->>>>9.9") .          
                        WHEN "mr&-stnd"   THEN 
                            cVarValue = STRING(tot-std-hrs,"->>>9.9") .      
                        WHEN "mr&-acl"    THEN 
                            cVarValue = STRING(tot-act-hrs,"->>>9.9") .      
                        WHEN "mr&-eff"    THEN 
                            cVarValue = STRING(tot-eff,"->>>>9.9") .          
                        WHEN "dt-acl"     THEN 
                            cVarValue = STRING(job-dt-act,"->>>9.9") .       
                        WHEN "dt-eff"     THEN 
                            cVarValue = STRING(dt-eff,"->>>>9.9") .           
                        WHEN "acl-qty"    THEN 
                            cVarValue = STRING(job-qty-prod,"->>,>>>,>>9") .     
                        WHEN "exp-qty"    THEN 
                            cVarValue = STRING(job-qty-expect,"->>,>>>,>>9") .   
                        WHEN "mr-comp"    THEN 
                            cVarValue = STRING(v-mrcomp) .     
                        WHEN "run-comp"    THEN 
                            cVarValue = STRING(v-runcomp) .   

                        WHEN "ttl-mch-hrs"       THEN 
                            cVarValue =  STRING((job-mr-act + job-run-act + job-dt-act),"->>,>>>,>>>,>>9.99") .
                        WHEN "ttl-lbr-hrs"       THEN 
                            cVarValue =  STRING(v-act-lab-cost,"->,>>>,>>>,>>9.99") .
                        WHEN "pic-per-hrs"       THEN 
                            cVarValue = IF v-pic-per-hrs NE ? THEN STRING(v-pic-per-hrs,"->>>,>>>,>>9.99") ELSE "".
                        WHEN "msf"               THEN 
                            cVarValue = STRING(v-calmsf,"->>>>9.99") .      
                        WHEN "msf-per-hrs"       THEN 
                            cVarValue = IF v-msf-per-hrs NE ? THEN STRING(v-msf-per-hrs,">,>>>,>>9.99") ELSE "".
                        WHEN "nbr-on"            THEN 
                            cVarValue = STRING(v-num-up,">>,>>>,>>9")     .
                        WHEN "kik-per-hrs "      THEN 
                            cVarValue =  IF v-kik-per-hrs NE ? THEN STRING(v-kik-per-hrs,"->>,>>>,>>9.99") ELSE "".
                        WHEN "pic-per-man-hrs"   THEN 
                            cVarValue = IF v-per-man-hrs NE ? THEN STRING(v-per-man-hrs,"->>,>>>,>>>,>>9.99") ELSE "" .      
                        WHEN "mr-wst"            THEN 
                            cVarValue = STRING(v-mrwaste,"->,>>9.99") .   
                        WHEN "run-wst"           THEN 
                            cVarValue = STRING(v-runwaste,"->,>>9.99") .   
                        WHEN "ttl-wst"           THEN 
                            cVarValue =  STRING(v-mrwaste + v-runwaste,"->,>>9.99") .
                        WHEN "%wst"              THEN 
                            cVarValue =  IF v-wst NE ? THEN STRING(v-wst,"->,>>9.99") ELSE "".
                        WHEN "date"              THEN 
                            cVarValue =  IF tt-mch-srt.start-date NE ? THEN STRING(tt-mch-srt.start-date) ELSE "".
                        WHEN "user-id"           THEN 
                            cVarValue = IF cUserId NE "" THEN STRING(cUserId,"x(10)") ELSE "" .
                        WHEN "shift"             THEN 
                            cVarValue = IF iShift NE 0 THEN STRING(iShift,">9") ELSE "" .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
          
                PUT UNFORMATTED cDisplay SKIP.
                IF tb_excel THEN 
                DO:
                    PUT STREAM excel UNFORMATTED  
                        cExcelDisplay SKIP.
                END.
            END.

            IF first-of(tt-mch-srt.m-code) THEN
            ASSIGN
              mch-mr-std     = mch-mr-std + job-mr-std
              dpt-mr-std     = dpt-mr-std + job-mr-std.
              
            ASSIGN    
                mch-run-std    = mch-run-std + job-run-std
                mch-mr-act     = mch-mr-act + job-mr-act
                mch-run-act    = mch-run-act + job-run-act
                mch-dt-act     = mch-dt-act + job-dt-act
                mch-qty-prod   = mch-qty-prod + job-qty-prod
                mch-qty-expect = mch-qty-expect + job-qty-expect                  
                dpt-run-std    = dpt-run-std + job-run-std
                dpt-mr-act     = dpt-mr-act + job-mr-act
                dpt-run-act    = dpt-run-act + job-run-act
                dpt-dt-act     = dpt-dt-act + job-dt-act
                dpt-qty-prod   = dpt-qty-prod + job-qty-prod
                dpt-qty-expect = dpt-qty-expect + job-qty-expect

                job-mr-std     = 0
                job-mr-act     = 0
                job-run-std    = 0
                job-run-act    = 0
                job-dt-act     = 0
                job-qty-prod   = 0
                job-qty-expect = 0.
        END.

        IF LAST-OF(tt-mch-srt.m-code) THEN
        DO:
            FIND mach WHERE mach.company EQ cocode AND
                mach.loc     EQ locode AND
                mach.m-code  EQ tt-mch-srt.m-code
                NO-LOCK NO-ERROR.
            {pc/rep/mchprdhr.i "mch"}

         

            PUT SKIP str-line SKIP .
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "m-cod"      THEN 
                        cVarValue = "" .
                    WHEN "ino"        THEN 
                        cVarValue = "" . 
                    WHEN "job"        THEN 
                        cVarValue = "" .
                    WHEN "mr-stn"     THEN 
                        cVarValue = STRING(mch-mr-std,"->>>9.9") .       
                    WHEN "mr-acl"     THEN 
                        cVarValue = STRING(mch-mr-act,"->>>9.9") .       
                    WHEN "mr-eff"     THEN 
                        cVarValue = STRING(mr-eff,"->>>>9.9") .           
                    WHEN "run-stnd"   THEN 
                        cVarValue = STRING(mch-run-std,"->>>9.9") .      
                    WHEN "run-acl"    THEN 
                        cVarValue = STRING(mch-run-act,"->>>9.9") .      
                    WHEN "run-eff"    THEN 
                        cVarValue = STRING(run-eff,"->>>>9.9") .          
                    WHEN "mr&-stnd"   THEN 
                        cVarValue = STRING(tot-std-hrs,"->>>9.9") .      
                    WHEN "mr&-acl"    THEN 
                        cVarValue = STRING(tot-act-hrs,"->>>9.9") .      
                    WHEN "mr&-eff"    THEN 
                        cVarValue = STRING(tot-eff,"->>>>9.9") .          
                    WHEN "dt-acl"     THEN 
                        cVarValue = STRING(mch-dt-act,"->>>9.9") .       
                    WHEN "dt-eff"     THEN 
                        cVarValue = STRING(dt-eff,"->>>>9.9") .           
                    WHEN "acl-qty"    THEN 
                        cVarValue = STRING(mch-qty-prod,"->>,>>>,>>9") .     
                    WHEN "exp-qty"    THEN 
                        cVarValue = STRING(mch-qty-expect,"->>,>>>,>>9") . 
                    WHEN "mr-comp"    THEN 
                        cVarValue = "" .     
                    WHEN "run-comp"    THEN 
                        cVarValue = "" .
                    WHEN "ttl-mch-hrs"       THEN 
                        cVarValue =  "" .
                    WHEN "ttl-lbr-hrs"       THEN 
                        cVarValue =  "" .
                    WHEN "pic-per-hrs"       THEN 
                        cVarValue = "" .
                    WHEN "msf"               THEN 
                        cVarValue = "" .
                    WHEN "msf-per-hrs"       THEN 
                        cVarValue = "" .
                    WHEN "nbr-on"            THEN 
                        cVarValue = "" .
                    WHEN "kik-per-hrs "      THEN 
                        cVarValue =  "" .
                    WHEN "pic-per-man-hrs"   THEN 
                        cVarValue = "" .
                    WHEN "mr-wst"            THEN 
                        cVarValue = "" .
                    WHEN "run-wst"           THEN 
                        cVarValue = "" .
                    WHEN "ttl-wst"           THEN 
                        cVarValue =  "" .
                    WHEN "%wst"              THEN 
                        cVarValue =  "" .
                    WHEN "date"              THEN 
                        cVarValue =  "" .
                    WHEN "user-id"           THEN 
                        cVarValue = "" .
                    WHEN "shift"             THEN 
                        cVarValue = "" .
                         
                END CASE.
                      
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED "* " + (IF AVAILABLE mach THEN STRING(mach.m-dscr,"x(10)") ELSE "") + substring(cDisplay,13,350) SKIP(1).
            IF tb_excel THEN 
            DO:         
                PUT STREAM excel UNFORMATTED  
                    (IF AVAILABLE mach THEN DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, STRING(mach.m-dscr,"x(14)"))  ELSE "") +  " - Totals" +
                    SUBSTRING(cExcelDisplay,3,250) SKIP.
            END.

            ASSIGN 
                mch-mr-std     = 0
                mch-mr-act     = 0
                mch-run-std    = 0
                mch-run-act    = 0
                mch-dt-act     = 0
                mch-qty-prod   = 0
                mch-qty-expect = 0.
        END.

        IF LAST-OF(tt-mch-srt.dept) THEN 
        DO:
            FIND dept WHERE dept.company EQ cocode AND
                dept.cod     EQ tt-mch-srt.dept
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE dept THEN
                FIND dept WHERE dept.company EQ "" AND
                    dept.cod     EQ tt-mch-srt.dept
                    NO-LOCK NO-ERROR.
            {pc/rep/mchprdhr.i "dpt"}

            PUT SKIP str-line SKIP .
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "m-cod"      THEN 
                        cVarValue = "" .
                    WHEN "ino"        THEN 
                        cVarValue = "" . 
                    WHEN "job"        THEN 
                        cVarValue = "" .
                    WHEN "mr-stn"     THEN 
                        cVarValue = STRING(dpt-mr-std,"->>>9.9") .       
                    WHEN "mr-acl"     THEN 
                        cVarValue = STRING(dpt-mr-act,"->>>9.9") .       
                    WHEN "mr-eff"     THEN 
                        cVarValue = STRING(mr-eff,"->>>>9.9") .           
                    WHEN "run-stnd"   THEN 
                        cVarValue = STRING(dpt-run-std,"->>>9.9") .      
                    WHEN "run-acl"    THEN 
                        cVarValue = STRING(dpt-run-act,"->>>9.9") .      
                    WHEN "run-eff"    THEN 
                        cVarValue = STRING(run-eff,"->>>>9.9") .          
                    WHEN "mr&-stnd"   THEN 
                        cVarValue = STRING(tot-std-hrs,"->>>9.9") .      
                    WHEN "mr&-acl"    THEN 
                        cVarValue = STRING(tot-act-hrs,"->>>9.9") .      
                    WHEN "mr&-eff"    THEN 
                        cVarValue = STRING(tot-eff,"->>>>9.9") .          
                    WHEN "dt-acl"     THEN 
                        cVarValue = STRING(dpt-dt-act,"->>>9.9") .       
                    WHEN "dt-eff"     THEN 
                        cVarValue = STRING(dt-eff,"->>>>9.9") .           
                    WHEN "acl-qty"    THEN 
                        cVarValue = STRING(dpt-qty-prod,"->>,>>>,>>9") .     
                    WHEN "exp-qty"    THEN 
                        cVarValue = STRING(dpt-qty-expect,"->>,>>>,>>9") .
                    WHEN "mr-comp"    THEN 
                        cVarValue = "" .     
                    WHEN "run-comp"    THEN 
                        cVarValue = "" .
                    WHEN "ttl-mch-hrs"       THEN 
                        cVarValue =  "" .
                    WHEN "ttl-lbr-hrs"       THEN 
                        cVarValue =  "" .
                    WHEN "pic-per-hrs"       THEN 
                        cVarValue = "" .
                    WHEN "msf"               THEN 
                        cVarValue = "" .
                    WHEN "msf-per-hrs"       THEN 
                        cVarValue = "" .
                    WHEN "nbr-on"            THEN 
                        cVarValue = "" .
                    WHEN "kik-per-hrs "      THEN 
                        cVarValue =  "" .
                    WHEN "pic-per-man-hrs"   THEN 
                        cVarValue = "" .
                    WHEN "mr-wst"            THEN 
                        cVarValue = "" .
                    WHEN "run-wst"           THEN 
                        cVarValue = "" .
                    WHEN "ttl-wst"           THEN 
                        cVarValue =  "" .
                    WHEN "%wst"              THEN 
                        cVarValue =  "" .
                    WHEN "date"              THEN 
                        cVarValue =  "" .
                    WHEN "user-id"           THEN 
                        cVarValue = "" .
                    WHEN "shift"           THEN 
                        cVarValue = "" .
                         
                END CASE.
                      
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED "** " + (IF AVAILABLE dept THEN STRING(dept.dscr,"x(10)") ELSE "") + substring(cDisplay,14,350) SKIP.
            IF tb_excel THEN 
            DO:  
                PUT STREAM excel UNFORMATTED 
                    (IF AVAILABLE dept THEN STRING(dept.dscr,"x(15)") ELSE "") +  " - Totals" +
                    SUBSTRING(cExcelDisplay,3,250) SKIP(1).
            END.

            ASSIGN 
                dpt-mr-std     = 0
                dpt-mr-act     = 0
                dpt-run-std    = 0
                dpt-run-act    = 0
                dpt-dt-act     = 0
                dpt-qty-prod   = 0
                dpt-qty-expect = 0.
            PAGE.
        END.
    END. /* each item */

    FIND CURRENT mch-srt NO-LOCK NO-ERROR.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    OUTPUT STREAM excel CLOSE.

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
            fi_file:SCREEN-VALUE = "c:\tmp\ProductivityByMachine.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
    ( hipField AS HANDLE ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    /*RETURN string(hField:BUFFER-VALUE, hField:FORMAT) */
    RETURN STRING(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



