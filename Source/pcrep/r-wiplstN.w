
&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: pcrep\r-wiplst.w

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

DEFINE TEMP-TABLE work-edit NO-UNDO
    FIELD job     LIKE job-mat.job
    FIELD job-no  LIKE job-mat.job-no
    FIELD job-no2 LIKE job-mat.job-no2.

DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cColumnInit        AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

ASSIGN 
    cTextListToSelect  = "Trans Type,Trans Date,Job No.,F,B,Item Number,"
                                            + "Description,Qty Posted,Wst Qty,Mch Hrs,"
                                            + "Mach Code,Job Code,C"
    cFieldListToSelect = "trns-typ,trns-dt,job-no,frm,blnk,i-no," +
                                        "dscr,qty-pstd,wst-qty,mch-hrs," +
                                        "mch-cd,job-cd,vc"
    cFieldLength       = "10,10,13,1,1,15," + "30,11,7,7," + "11,11,1" 
    cFieldType         = "c,c,c,c,c,c," + "c,i,i,i," + "c,c,c"
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Trans Type,Trans Date,Job No.,F,B,Item Number,"
                                            + "Description,Qty Posted,Wst Qty,Mch Hrs,"
                                            + "Mach Code,Job Code,C"  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_job-no begin_job-no2 ~
end_job-no end_job-no2 rd-dest tb_OpenCSV tbAutoClose fi_file ~
btn-ok btn-cancel sl_avail Btn_Def sl_selected ~
Btn_Add Btn_Remove btn_Up btn_down
&Scoped-Define DISPLAYED-OBJECTS begin_job-no begin_job-no2 end_job-no ~
end_job-no2 rd-dest tb_OpenCSV tbAutoClose fi_file sl_avail sl_selected

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

DEFINE VARIABLE begin_job-no   AS CHARACTER FORMAT "X(9)":U 
    LABEL "Beginning Job#" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2  AS CHARACTER FORMAT "-999":U INITIAL "000" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no     AS CHARACTER FORMAT "X(9)":U INITIAL "zzzzzzzzz" 
    LABEL "Ending Job#" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2    AS CHARACTER FORMAT "-999":U INITIAL "999" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\WIPPosting.csv" 
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
    "To CSV", 3
    SIZE 16 BY 4.6 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.3.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 3.55.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tbAutoClose  AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_job-no AT ROW 2.71 COL 23 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    begin_job-no2 AT ROW 2.71 COL 38 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    end_job-no AT ROW 2.71 COL 62 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    end_job-no2 AT ROW 2.71 COL 77 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    sl_avail AT ROW 5.95 COL 4 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 5.95 COL 40.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 5.95 COL 60.6 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 6.95 COL 40.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 7.95 COL 40.8 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 9 COL 40.8 WIDGET-ID 40
    btn_down AT ROW 10 COL 40.8 WIDGET-ID 42
    rd-dest AT ROW 12.35 COL 6 NO-LABELS
    lv-ornt AT ROW 12.95 COL 31 NO-LABELS
    lines-per-page AT ROW 12.95 COL 84 COLON-ALIGNED
    lv-font-no AT ROW 15.1 COL 35 COLON-ALIGNED
    lv-font-name AT ROW 15.91 COL 29 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 14.8 COL 28.6
    tb_excel AT ROW 18.19 COL 51 RIGHT-ALIGNED
    tb_OpenCSV AT ROW 15.8 COL 92.4 RIGHT-ALIGNED
    tbAutoClose AT ROW 17.2 COL 28.6 WIDGET-ID 78
    fi_file AT ROW 15.7 COL 26.6 COLON-ALIGNED HELP
    "Enter File Name"
    btn-ok AT ROW 18.2 COL 28.6
    btn-cancel AT ROW 18.2 COL 48.6
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 5.24 COL 60 WIDGET-ID 44
    " Output Destination" VIEW-AS TEXT
    SIZE 18.9 BY .62 AT ROW 11.52 COL 5
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 5.24 COL 4.1 WIDGET-ID 38
    RECT-6 AT ROW 11.88 COL 4
    RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 19.27
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
        TITLE              = "W.I.P. Posting Edit List"
        HEIGHT             = 19.51
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
                                                                        */
ASSIGN 
    begin_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".
                
/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".
                
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
ON END-ERROR OF C-Win /* W.I.P. Posting Edit List */
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
ON WINDOW-CLOSE OF C-Win /* W.I.P. Posting Edit List */
    DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
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
        DELETE PROCEDURE hdOutputProcs.
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
        ASSIGN rd-dest.
        IF rd-dest EQ 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.     
        RUN GetSelectionList.
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

    RUN DisplaySelectionList.
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    Btn_Def:load-image("Graphics/32x32/default.png").
    Btn_Add:load-image("Graphics/32x32/additem.png").
    Btn_Remove:load-image("Graphics/32x32/remove.png").
    btn_Up:load-image("Graphics/32x32/moveup.png").
    btn_down:load-image("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {methods/nowait.i}
    {sys/inc/reportsConfigNK1.i "DR8" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    DO WITH FRAME {&FRAME-NAME}:

        {custom/usrprint.i}
RUN DisplaySelectionList2.
IF end_job-no:SCREEN-VALUE = "" THEN
    end_job-no:SCREEN-VALUE = "zzzzzzzzz".
IF end_job-no2:SCREEN-VALUE = "" THEN
    end_job-no2:SCREEN-VALUE = "999".
APPLY "entry" TO begin_job-no .

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
    DISPLAY begin_job-no begin_job-no2 end_job-no end_job-no2 rd-dest 
        tb_OpenCSV tbAutoClose fi_file sl_avail sl_selected
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_job-no begin_job-no2 end_job-no end_job-no2 
        rd-dest tbAutoClose tb_OpenCSV fi_file btn-ok btn-cancel 
        sl_avail Btn_Def sl_selected Btn_Add 
        Btn_Remove btn_Up btn_down
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
/*      DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.      */
/*                                                         */
/*      if init-dir = "" then init-dir = "c:\temp" .       */
/*      SYSTEM-DIALOG GET-FILE list-name                   */
/*          TITLE      "Enter Listing Name to SAVE AS ..." */
/*          FILTERS    "Listing Files (*.rpt)" "*.rpt",    */
/*                     "All Files (*.*)" "*.*"             */
/*          INITIAL-DIR init-dir                           */
/*          ASK-OVERWRITE                                  */
/*          SAVE-AS                                        */
/*          USE-FILENAME                                   */
/*                                                         */
/*          UPDATE OKpressed.                              */
/*                                                         */
/*      IF NOT OKpressed THEN  RETURN NO-APPLY.            */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-job-tot-excel C-Win 
PROCEDURE print-job-tot-excel :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-v-brd-job AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-v-mch-job AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-v-wst-job AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-v-hrs-job AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-v-fg-job AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-v-oth-job AS DECIMAL NO-UNDO.
   
    DEFINE VARIABLE viCnt AS INTEGER NO-UNDO.

    PUT STREAM excel UNFORMATTED SKIP(1).

    DO viCnt = 1 TO 5:
        PUT STREAM excel UNFORMATTED
            '"' "" '",'.
    END.
          
    PUT STREAM excel UNFORMATTED
        '"' ("JOB TOTALS - " + STRING(work-edit.job-no) + "-" +
        STRING(work-edit.job-no2))                         '",'
        '"' "BOARD TOTALS:"                                     '",'
        '"' ip-v-brd-job                                        '",'
        SKIP.

    DO viCnt = 1 TO 5:
        PUT STREAM excel UNFORMATTED
            '"' "" '",'.
    END.

    PUT STREAM excel UNFORMATTED
        '"' ""                         '",'
        '"' "MACHINE TOTALS:"          '",'
        '"' ip-v-mch-job               '",'
        '"' ip-v-wst-job               '",'
        '"' ip-v-hrs-job               '",'
        SKIP.

    DO viCnt = 1 TO 5:
        PUT STREAM excel UNFORMATTED
            '"' "" '",'.
    END.

    PUT STREAM excel UNFORMATTED
        '"' ""                         '",'
        '"' "FINISHED GOODS TOTALS:"   '",'
        '"' ip-v-fg-job                '",'
        SKIP.

    DO viCnt = 1 TO 5:
        PUT STREAM excel UNFORMATTED
            '"' "" '",'.
    END.

    PUT STREAM excel UNFORMATTED
        '"' ""                         '",'
        '"' "OTHER MATERIALS TOTALS:"  '",'
        '"' ip-v-oth-job             '",'
        SKIP(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-rep-tot-excel C-Win 
PROCEDURE print-rep-tot-excel :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-v-brd-job AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-v-mch-job AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-v-wst-job AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-v-hrs-job AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-v-fg-job AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-v-oth-job AS DECIMAL NO-UNDO.
   
    DEFINE VARIABLE viCnt AS INTEGER NO-UNDO.

    PUT STREAM excel UNFORMATTED SKIP(1).

    DO viCnt = 1 TO 5:
        PUT STREAM excel UNFORMATTED
            '"' "" '",'.
    END.
          
    PUT STREAM excel UNFORMATTED
        '"' "REPORT TOTALS - "     '",'
        '"' "BOARD TOTALS:"        '",'
        '"' ip-v-brd-job           '",'
        SKIP.

    DO viCnt = 1 TO 5:
        PUT STREAM excel UNFORMATTED
            '"' "" '",'.
    END.

    PUT STREAM excel UNFORMATTED
        '"' ""                         '",'
        '"' "MACHINE TOTALS:"          '",'
        '"' ip-v-mch-job               '",'
        '"' ip-v-wst-job               '",'
        '"' ip-v-hrs-job               '",'
        SKIP.

    DO viCnt = 1 TO 5:
        PUT STREAM excel UNFORMATTED
            '"' "" '",'.
    END.

    PUT STREAM excel UNFORMATTED
        '"' ""                         '",'
        '"' "FINISHED GOODS TOTALS:"   '",'
        '"' ip-v-fg-job                '",'
        SKIP.

    DO viCnt = 1 TO 5:
        PUT STREAM excel UNFORMATTED
            '"' "" '",'.
    END.

    PUT STREAM excel UNFORMATTED
        '"' ""                         '",'
        '"' "OTHER MATERIALS TOTALS:"  '",'
        '"' ip-v-oth-job             '",'
        SKIP(1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /*------------------------------------------------- pc/rep/wip-edit.p 8/94 gb */
    /* WIP Edit Listing Report                                                    */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE v-job-no       LIKE job.job-no EXTENT 2 INITIAL [" ", "ZZZZZZZZZ"] NO-UNDO.
    DEFINE VARIABLE v-job-no2      LIKE job.job-no2 EXTENT 2 INITIAL [000, 999] NO-UNDO.
    DEFINE VARIABLE v-date         AS DATE      EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-brd-job      AS INTEGER   FORMAT ">>>>>>>>>9" NO-UNDO.
    DEFINE VARIABLE v-brd-tot      AS INTEGER   FORMAT ">>>>>>>>>9" NO-UNDO.
    DEFINE VARIABLE v-oth-job      AS INTEGER   FORMAT ">>>>>>>>>9" NO-UNDO.
    DEFINE VARIABLE v-oth-tot      AS INTEGER   FORMAT ">>>>>>>>>9" NO-UNDO.
    DEFINE VARIABLE v-mch-job      AS INTEGER   FORMAT ">>>>>>>>>9" NO-UNDO.
    DEFINE VARIABLE v-mch-tot      AS INTEGER   FORMAT ">>>>>>>>>9" NO-UNDO.
    DEFINE VARIABLE v-fg-job       AS INTEGER   FORMAT ">>>>>>>>>9" NO-UNDO.
    DEFINE VARIABLE v-fg-tot       AS INTEGER   FORMAT ">>>>>>>>>9" NO-UNDO.
    DEFINE VARIABLE v-hrs-job      AS DECIMAL   FORMAT ">>>>>9.99" NO-UNDO.
    DEFINE VARIABLE v-hrs-tot      AS DECIMAL   FORMAT ">>>>>9.99" NO-UNDO.
    DEFINE VARIABLE v-wst-job      AS INTEGER   FORMAT ">>>>>>>9" NO-UNDO.
    DEFINE VARIABLE v-wst-tot      AS INTEGER   FORMAT ">>>>>>>9" NO-UNDO.

    DEFINE VARIABLE hdr-tit        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdr-tit2       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdr-tit3       AS CHARACTER NO-UNDO.

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
    DEFINE VARIABLE str-line       AS cha       FORM "x(300)" NO-UNDO.

    DEFINE VARIABLE cCustomerName  AS cha       FORM "x(25)" NO-UNDO.
    DEFINE VARIABLE cPrepDscr      AS cha       FORM "x(25)" NO-UNDO.
    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.

    /*FORM HEADER
         hdr-tit format "x(132)" skip
         hdr-tit2 format "x(132)" skip
         hdr-tit3 format "x(132)"
    
        WITH FRAME r-top.
    
    form item.procat
         mat-act.mat-date FORMAT "99/99/99"
         work-edit.job-no space(0) "-" space(0)
         work-edit.job-no2 format "99"
         mat-act.s-num space(0) "/" space(0)
         mat-act.b-num
         mat-act.i-no
         item.i-name
         mat-act.qty format "->>>>>>>>>9"
         with frame edit-mat down no-attr-space no-box no-labels STREAM-IO width 132.
    
    form item.procat
         mch-act.op-date FORMAT "99/99/99"
         work-edit.job-no space(0) "-" space(0)
         work-edit.job-no2 format "99"
         mch-act.frm format ">9" space(0) "/" space(0)
         mch-act.blank-no
         mch-act.i-no /*"               "*/
         mach.m-dscr "         "
         mch-act.qty format "->>>>>>>9.99"
         mch-act.waste format ">>>>>>>9"
         mch-act.hours format ">>>>>9.99"
         mch-act.m-code
         mch-act.code
         mch-act.complete
         with frame edit-mch down no-attr-space no-box no-labels STREAM-IO width 132.
    
    form item.procat
         fg-act.fg-date  FORMAT "99/99/99"
         work-edit.job-no space(0) "-" space(0)
         work-edit.job-no2 format "99"
         fg-act.s-num space(0) "/" space(0)
         fg-act.b-num
         fg-act.i-no
         fg-act.i-name
         fg-act.qty format "->>>>>>>9.99"
         with frame edit-fg down no-attr-space no-box no-labels STREAM-IO width 132.
    */
    ASSIGN
        str-tit2    = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-job-no[1] = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', begin_job-no, begin_job-no2)) 
        v-job-no[2] = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', end_job-no, end_job-no2)) 
        /*
        v-job-no[1]    = begin_job-no
        v-job-no[2]    = end_job-no
        v-job-no2[1]   = begin_rel
        v-job-no2[2]   = end_rel
          
              hdr-tit = "TRANS  TRANS      JOB                                      " +
                    "                     QUANTITY     WASTE      MACH MACH   JOB     "
              hdr-tit2 = "TYPE    DATE      NUMBER  S/ B ITEM NUMBER     DESCRIPTION " +
                    "                       POSTED       QTY     HOURS CODE   CODE  C "
              hdr-tit3 = fill("-", 131)*/.


    DEFINE VARIABLE cslist AS CHARACTER NO-UNDO.
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

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

        IF LOOKUP(ttRptSelected.TextList, "Qty Posted,Wst Qty,Mch Hrs") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        /* excelheader = "TRANS TYPE,TRANS DATE,JOB NUMBER,S,B,ITEM NUMBER,"
                     + "DESCRIPTION,QUANTITY POSTED,WASTE QTY,MACH HOURS,MACH CODE,"
                     + "JOB CODE,C". */
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.

    DISPLAY "" WITH FRAME r-top.
    EMPTY TEMP-TABLE work-edit.

    FOR EACH mch-act WHERE mch-act.company = cocode AND
        mch-act.job-no2 ge int(begin_job-no2) AND
        mch-act.job-no2 le int(end_job-no2) AND
        mch-act.opn = YES
        USE-INDEX opn-idx
        NO-LOCK:
        IF STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', mch-act.job-no, mch-act.job-no2))  < v-job-no[1] OR 
           STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', mch-act.job-no, mch-act.job-no2))  > v-job-no[2] THEN NEXT.
                 
        FIND FIRST work-edit WHERE work-edit.job = mch-act.job NO-ERROR.

        IF NOT AVAILABLE work-edit THEN
        DO:
            CREATE work-edit.
            ASSIGN 
                work-edit.job-no  = mch-act.job-no
                work-edit.job     = mch-act.job
                work-edit.job-no2 = mch-act.job-no2.
        END.
    END.
    FOR EACH mat-act WHERE mat-act.company = cocode AND
        mat-act.job-no2 ge int(begin_job-no2) AND
        mat-act.job-no2 le int(end_job-no2) AND
        mat-act.opn = YES
        USE-INDEX opn-idx
        NO-LOCK:

        IF STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', mat-act.job-no, mat-act.job-no2)) < v-job-no[1] OR 
            STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', mat-act.job-no, mat-act.job-no2)) > v-job-no[2] THEN NEXT.
           
        FIND FIRST work-edit WHERE work-edit.job = mat-act.job NO-ERROR.

        IF NOT AVAILABLE work-edit THEN
        DO:
            CREATE work-edit.
            ASSIGN 
                work-edit.job-no  = mat-act.job-no
                work-edit.job     = mat-act.job
                work-edit.job-no2 = mat-act.job-no2.
        END.
    END.
    FOR EACH fg-act WHERE fg-act.company = cocode AND
        fg-act.job-no2 ge int(begin_job-no2) AND
        fg-act.job-no2 le int(end_job-no2) AND
        fg-act.opn = YES
        USE-INDEX opn-idx
        NO-LOCK:

        IF STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', fg-act.job-no, fg-act.job-no2)) < v-job-no[1] OR 
          STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', fg-act.job-no, fg-act.job-no2)) > v-job-no[2] THEN NEXT.
          
        FIND FIRST work-edit WHERE work-edit.job = fg-act.job NO-ERROR.

        IF NOT AVAILABLE work-edit THEN
        DO:
            CREATE work-edit.
            ASSIGN 
                work-edit.job-no  = fg-act.job-no
                work-edit.job     = fg-act.job
                work-edit.job-no2 = fg-act.job-no2.
        END.
    END.

    FOR EACH misc-act WHERE misc-act.company = cocode AND
        misc-act.job-no2 ge int(begin_job-no2) AND
        misc-act.job-no2 le int(end_job-no2) AND
        misc-act.opn = YES
        USE-INDEX opn-idx
        NO-LOCK:

        IF STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', misc-act.job-no, misc-act.job-no2))  < v-job-no[1] OR 
           STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', misc-act.job-no, misc-act.job-no2)) > v-job-no[2] THEN NEXT.
             
        FIND FIRST work-edit WHERE work-edit.job = misc-act.job NO-ERROR.

        IF NOT AVAILABLE work-edit THEN
        DO:
            CREATE work-edit.
            ASSIGN 
                work-edit.job-no  = misc-act.job-no
                work-edit.job     = misc-act.job
                work-edit.job-no2 = misc-act.job-no2.
        END.
    END.

    
    FOR EACH work-edit BREAK BY work-edit.job-no
        BY work-edit.job-no2:
 
        ASSIGN 
            v-brd-job = 0
            v-mch-job = 0
            v-fg-job  = 0
            v-oth-job = 0
            v-wst-job = 0
            v-hrs-job = 0.

        FOR EACH mat-act WHERE mat-act.company = cocode AND
            mat-act.job = work-edit.job
            USE-INDEX job
            NO-LOCK:
            IF NOT mat-act.opn THEN NEXT.

            FIND item WHERE item.company = cocode AND
                item.i-no    = mat-act.i-no
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE item THEN NEXT.

            /*  display item.procat
                      mat-act.mat-date
                      work-edit.job-no
                      work-edit.job-no2
                      mat-act.s-num
                      mat-act.b-num
                      mat-act.i-no
                      item.i-name
                      mat-act.qty
                      with frame edit-mat.
              down with frame edit-mat.
  
              IF tb_excel THEN
                 PUT STREAM excel UNFORMATTED
                   '"' item.procat                                  '",'
                   '"' (IF mat-act.mat-date NE ? THEN
                           STRING(mat-act.mat-date) ELSE "")        '",'
                   '"' (STRING(work-edit.job-no) + "-" +
                        STRING(work-edit.job-no2))                  '",'
                   '"' mat-act.s-num                                '",'
                   '"' mat-act.b-num                                '",'
                   '"' mat-act.i-no                                 '",'
                   '"' item.i-name                                  '",'
                   '"' STRING(mat-act.qty,"->>>>>>>>>9")            '",'
                   SKIP.*/

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "" .
                       
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "trns-typ"         THEN 
                        cVarValue =  STRING(item.procat) .
                    WHEN "trns-dt"      THEN 
                        cVarValue =  (IF mat-act.mat-date NE ? THEN STRING(mat-act.mat-date) ELSE "")  .
                    WHEN "job-no"           THEN 
                        cVarValue =  (STRING(work-edit.job-no) + "-" + STRING(work-edit.job-no2,"999")) .
                    WHEN "frm"              THEN 
                        cVarValue =  STRING(mat-act.s-num) .
                    WHEN "blnk"             THEN 
                        cVarValue =  STRING(mat-act.b-num) .
                    WHEN "i-no"             THEN 
                        cVarValue =  STRING(mat-act.i-no) .
                    WHEN "dscr"             THEN 
                        cVarValue =  STRING(item.i-name) .
                    WHEN "qty-pstd"         THEN 
                        cVarValue =  STRING(mat-act.qty,"->>>>>>>>>9")  .
                    WHEN "wst-qty"          THEN 
                        cVarValue =  "".
                    WHEN "mch-hrs"          THEN 
                        cVarValue =  "".
                    WHEN "mch-cd"           THEN 
                        cVarValue =  "".
                    WHEN "job-cd"           THEN 
                        cVarValue =  "".       
                    WHEN "vc"               THEN 
                        cVarValue =  "".
                    
                END CASE.  
                                 
                IF  cTmpField = "trns-dt" THEN
                     cExcelVarValue = IF mat-act.mat-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",mat-act.mat-date) ELSE "".
                ELSE cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, cExcelVarValue)) + ",".            
            END.
                       
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
            END.

            IF item.mat-type = "B" THEN
                ASSIGN v-brd-job = v-brd-job + mat-act.qty
                    v-brd-tot = v-brd-tot + mat-act.qty.
            ELSE
                ASSIGN v-oth-job = v-oth-job + mat-act.qty
                    v-oth-tot = v-oth-tot + mat-act.qty.
        END.

        FOR EACH mch-act WHERE mch-act.company = cocode AND
            mch-act.job = work-edit.job
            USE-INDEX job
            NO-LOCK:
            IF NOT mch-act.opn THEN NEXT.

            FIND mach WHERE mach.company = cocode AND
                mach.loc     = locode AND
                mach.m-code  = mch-act.m-code
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE mach THEN NEXT.

            /* display "HRS" @ item.procat
                     mch-act.op-date
                     work-edit.job-no
                     work-edit.job-no2
                     mch-act.frm
                     mch-act.blank-no
                     mch-act.i-no
                     mach.m-dscr
                     mch-act.qty
                     mch-act.waste
                     mch-act.hours
                     mch-act.m-code
                     mch-act.code
                     mch-act.complete
                     with frame edit-mch.
             down with frame edit-mch.
 
             IF tb_excel THEN
                PUT STREAM excel UNFORMATTED
                  '"' "HRS"                                   '",'
                  '"' (IF mch-act.op-date NE ? THEN
                          STRING(mch-act.op-date) ELSE "")    '",'
                  '"' (STRING(work-edit.job-no) + "-" +
                       STRING(work-edit.job-no2))             '",'
                  '"' mch-act.frm                             '",'
                  '"' mch-act.blank-no                        '",'
                  '"' mch-act.i-no                            '",'
                  '"' mach.m-dscr                             '",'
                  '"' STRING(mch-act.qty,"->>>>>>>9.99")      '",'
                  '"' STRING(mch-act.waste,">>>>>>>9")        '",'
                  '"' STRING(mch-act.hours,">>>>>9.99")       '",'
                  '"' mch-act.m-code                          '",'
                  '"' mch-act.CODE                            '",'
                  '"' mch-act.COMPLETE                        '",'
                  SKIP. */
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "" .
                       
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "trns-typ"         THEN 
                        cVarValue =  STRING("HRS") .
                    WHEN "trns-dt"      THEN 
                        cVarValue =  (IF mch-act.op-date NE ? THEN STRING(mch-act.op-date) ELSE "")  .
                    WHEN "job-no"           THEN 
                        cVarValue =  (STRING(work-edit.job-no) + "-" + STRING(work-edit.job-no2,"999")) .
                    WHEN "frm"              THEN 
                        cVarValue =  STRING(mch-act.frm) .
                    WHEN "blnk"             THEN 
                        cVarValue =  STRING(mch-act.blank-no) .
                    WHEN "i-no"             THEN 
                        cVarValue =  STRING(mch-act.i-no) .
                    WHEN "dscr"             THEN 
                        cVarValue =  STRING(mach.m-dscr) .
                    WHEN "qty-pstd"         THEN 
                        cVarValue =  STRING(mch-act.qty,"->>>>>>>>>9")  .
                    WHEN "wst-qty"          THEN 
                        cVarValue =  STRING(mch-act.waste,">>>>>>>9") .   
                    WHEN "mch-hrs"          THEN 
                        cVarValue =  STRING(mch-act.hours,">>>>>9.99")  . 
                    WHEN "mch-cd"           THEN 
                        cVarValue =  mch-act.m-code        .              
                    WHEN "job-cd"           THEN 
                        cVarValue =  STRING(mch-act.CODE)  .                      
                    WHEN "vc"               THEN 
                        cVarValue =  STRING(mch-act.COMPLETE)  .                  
                    
                END CASE.  
                                 
                IF  cTmpField = "trns-dt" THEN
                     cExcelVarValue = IF mch-act.op-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",mch-act.op-date) ELSE "".
                ELSE cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, cExcelVarValue)) + ",".            
            END.
                       
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
            END.

            ASSIGN 
                v-mch-job = v-mch-job + mch-act.qty
                v-wst-job = v-wst-job + mch-act.waste
                v-hrs-job = v-hrs-job + mch-act.hours
                v-mch-tot = v-mch-tot + mch-act.qty
                v-wst-tot = v-wst-tot + mch-act.waste
                v-hrs-tot = v-hrs-tot + mch-act.hours.
        END.
        FOR EACH fg-act WHERE fg-act.company = cocode AND
            fg-act.job = work-edit.job
            USE-INDEX job-idx
            NO-LOCK:
            IF NOT fg-act.opn THEN NEXT.

            /*  display "F.G." @ item.procat
                      fg-act.fg-date
                      work-edit.job-no
                      work-edit.job-no2
                      fg-act.s-num
                      fg-act.b-num
                      fg-act.i-no
                      fg-act.i-name
                      fg-act.qty
                      with frame edit-fg.
              down with frame edit-fg.
  
              IF tb_excel THEN
                 PUT STREAM excel UNFORMATTED
                   '"' "F.G."                                  '",'
                   '"' (IF fg-act.fg-date NE ? THEN
                           STRING(fg-act.fg-date) ELSE "")     '",'
                   '"' (STRING(work-edit.job-no) + "-" +
                        STRING(work-edit.job-no2))             '",'
                   '"' fg-act.s-num                            '",'
                   '"' fg-act.b-num                            '",'
                   '"' fg-act.i-no                             '",'
                   '"' fg-act.i-name                           '",'
                   '"' STRING(fg-act.qty,"->>>>>>>9.99")       '",'
                   SKIP. */

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "" .
                       
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "trns-typ"         THEN 
                        cVarValue =  STRING("F.G.") .
                    WHEN "trns-dt"      THEN 
                        cVarValue =  (IF fg-act.fg-date NE ? THEN STRING(fg-act.fg-date) ELSE "")  .
                    WHEN "job-no"           THEN 
                        cVarValue =  (STRING(work-edit.job-no) + "-" + STRING(work-edit.job-no2,"999")) .
                    WHEN "frm"              THEN 
                        cVarValue =  STRING(fg-act.s-num) .
                    WHEN "blnk"             THEN 
                        cVarValue =  STRING(fg-act.b-num) .
                    WHEN "i-no"             THEN 
                        cVarValue =  STRING(fg-act.i-no) .
                    WHEN "dscr"             THEN 
                        cVarValue =  STRING(fg-act.i-name) .
                    WHEN "qty-pstd"         THEN 
                        cVarValue =  STRING(fg-act.qty,"->>>>>>>>>9")  .
                    WHEN "wst-qty"          THEN 
                        cVarValue =  "" .   
                    WHEN "mch-hrs"          THEN 
                        cVarValue =  ""  . 
                    WHEN "mch-cd"           THEN 
                        cVarValue =  ""       .              
                    WHEN "job-cd"           THEN 
                        cVarValue =  ""  .                      
                    WHEN "vc"               THEN 
                        cVarValue =  ""  .                  
                    
                END CASE.  
                                 
                IF  cTmpField = "trns-dt" THEN
                     cExcelVarValue = IF fg-act.fg-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",fg-act.fg-date) ELSE "".
                ELSE cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, cExcelVarValue)) + ",".            
            END.
                       
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
            END.

            ASSIGN 
                v-fg-job = v-fg-job + fg-act.qty
                v-fg-tot = v-fg-tot + fg-act.qty.
        END.

        FOR EACH misc-act WHERE misc-act.company = cocode AND
            misc-act.job = work-edit.job
            USE-INDEX misc-idx
            NO-LOCK:
            IF NOT misc-act.opn THEN NEXT.

            IF misc-act.ml THEN
            DO:
                /*display "MSC-M" @ item.procat
                        misc-act.misc-date @ mat-act.mat-date
                        work-edit.job-no @ mat-act.job-no
                        work-edit.job-no2 @ mat-act.job-no2
                        misc-act.frm @ mat-act.s-num
                        misc-act.blank-no @ mat-act.b-num
                        misc-act.i-no @ mat-act.i-no
                        misc-act.dscr @ item.i-dscr
                        misc-act.cost @ mat-act.qty
                        with frame edit-mat.
                down with frame edit-mat.
 
                IF tb_excel THEN
                   PUT STREAM excel UNFORMATTED
                       '"' "MSC-M"                                 '",'
                       '"' (IF misc-act.misc-date NE ? THEN
                               STRING(misc-act.misc-date) ELSE "") '",'
                       '"' (STRING(work-edit.job-no) + "-" +
                            STRING(work-edit.job-no2))             '",'
                       '"' misc-act.frm                            '",'
                       '"' misc-act.blank-no                       '",'
                       '"' misc-act.i-no                           '",'
                       '"' misc-act.dscr                           '",'
                       '"' STRING(misc-act.cost,"->>,>>9.99")      '",'
                       SKIP. */
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "" .
                       
                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "trns-typ"         THEN 
                            cVarValue =  STRING("MSC-M") .
                        WHEN "trns-dt"      THEN 
                            cVarValue =  (IF misc-act.misc-date NE ? THEN STRING(misc-act.misc-date) ELSE "")  .
                        WHEN "job-no"           THEN 
                            cVarValue =  (STRING(work-edit.job-no) + "-" + STRING(work-edit.job-no2,"999")) .
                        WHEN "frm"              THEN 
                            cVarValue =  STRING(misc-act.frm) .
                        WHEN "blnk"             THEN 
                            cVarValue =  STRING(misc-act.blank-no) .
                        WHEN "i-no"             THEN 
                            cVarValue =  STRING(misc-act.i-no) .
                        WHEN "dscr"             THEN 
                            cVarValue =  STRING(misc-act.dscr) .
                        WHEN "qty-pstd"         THEN 
                            cVarValue =  STRING(misc-act.cost,"->>,>>9.99")   .
                        WHEN "wst-qty"          THEN 
                            cVarValue =  "" .   
                        WHEN "mch-hrs"          THEN 
                            cVarValue =  ""  . 
                        WHEN "mch-cd"           THEN 
                            cVarValue =  ""       .              
                        WHEN "job-cd"           THEN 
                            cVarValue =  ""  .                      
                        WHEN "vc"               THEN 
                            cVarValue =  ""  .                  
                    
                    END CASE.  
                                 
                    IF  cTmpField = "trns-dt" THEN
                         cExcelVarValue = IF misc-act.misc-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",misc-act.misc-date) ELSE "".
                    ELSE cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, cExcelVarValue)) + ",".            
                END.
                       
                PUT UNFORMATTED cDisplay SKIP.
                IF tb_excel THEN 
                DO:
                    PUT STREAM excel UNFORMATTED  
                        cExcelDisplay SKIP.
                END.
            END.
            ELSE
            DO:
                /*display "MSC-H" @ item.procat
                        misc-act.misc-date @ mch-act.op-date
                        misc-act.job-no @ work-edit.job-no
                        misc-act.job-no2 @ work-edit.job-no2
                        misc-act.frm @ mch-act.frm
                        misc-act.blank-no @ mch-act.blank-no
                        misc-act.i-no @ mat-act.i-no
                        misc-act.dscr @ mach.m-dscr
                        misc-act.cost @ mch-act.qty
                        mch-act.waste when available mch-act
                        mch-act.hours when available mch-act
                        misc-act.m-code @ mch-act.m-code
                        mch-act.code when available mch-act
                        mch-act.complete when available mch-act
                        with frame edit-mch.
                down with frame edit-mch.
 
                IF tb_excel THEN
                   PUT STREAM excel UNFORMATTED
                       '"' "MSC-H"                                 '",'
                       '"' (IF misc-act.misc-date NE ? THEN
                               STRING(misc-act.misc-date) ELSE "") '",'
                       '"' (STRING(work-edit.job-no) + "-" +
                            STRING(work-edit.job-no2))             '",'
                       '"' misc-act.frm                            '",'
                       '"' misc-act.blank-no                       '",'
                       '"' misc-act.i-no                               '",'
                       '"' misc-act.dscr                           '",'
                       '"' STRING(misc-act.cost,"->>,>>9.99")      '",'
                       '"' (IF AVAIL mch-act THEN
                               STRING(mch-act.waste) ELSE "")      '",'
                       '"' (IF AVAIL mch-act THEN
                               STRING(mch-act.hours) ELSE "")      '",'
                       '"' misc-act.m-code
                       '"' (IF AVAIL mch-act THEN mch-act.CODE
                            ELSE "")                               '",'
                       '"' (IF AVAIL mch-act THEN
                               STRING(mch-act.COMPLETE) ELSE "")   '",'
                       SKIP.*/
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "" .
                       
                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "trns-typ"         THEN 
                            cVarValue =  STRING("MSC-H") .
                        WHEN "trns-dt"      THEN 
                            cVarValue =  (IF misc-act.misc-date NE ? THEN STRING(misc-act.misc-date) ELSE "")  .
                        WHEN "job-no"           THEN 
                            cVarValue =  (STRING(work-edit.job-no) + "-" + STRING(work-edit.job-no2,"999")) .
                        WHEN "frm"              THEN 
                            cVarValue =  STRING(misc-act.frm) .
                        WHEN "blnk"             THEN 
                            cVarValue =  STRING(misc-act.blank-no) .
                        WHEN "i-no"             THEN 
                            cVarValue =  STRING(misc-act.i-no) .
                        WHEN "dscr"             THEN 
                            cVarValue =  STRING(misc-act.dscr) .
                        WHEN "qty-pstd"         THEN 
                            cVarValue =  STRING(misc-act.cost,"->>,>>9.99")   .
                        WHEN "wst-qty"          THEN 
                            cVarValue =  (IF AVAILABLE mch-act THEN STRING(mch-act.waste) ELSE "") .   
                        WHEN "mch-hrs"          THEN 
                            cVarValue =  (IF AVAILABLE mch-act THEN STRING(mch-act.hours) ELSE "")  . 
                        WHEN "mch-cd"           THEN 
                            cVarValue =  misc-act.m-code       .              
                        WHEN "job-cd"           THEN 
                            cVarValue =  (IF AVAILABLE mch-act THEN mch-act.CODE ELSE "")  .                      
                        WHEN "vc"               THEN 
                            cVarValue =  (IF AVAILABLE mch-act THEN STRING(mch-act.COMPLETE) ELSE "")  .                  
                    
                    END CASE.  
                                 
                    IF  cTmpField = "trns-dt" THEN
                         cExcelVarValue = IF misc-act.misc-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",misc-act.misc-date) ELSE "".
                    ELSE cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, cExcelVarValue)) + ",".            
                END.
                       
                PUT UNFORMATTED cDisplay SKIP.
                IF tb_excel THEN 
                DO:
                    PUT STREAM excel UNFORMATTED  
                        cExcelDisplay SKIP.
                END.
            END.
        END.

        /*   put skip(1) "JOB TOTALS - " at 20 work-edit.job-no
               space(0) "-" space(0) work-edit.job-no2 format "99"
               "         BOARD TOTALS: " at 56 v-brd-job skip
               "       MACHINE TOTALS: " at 56 v-mch-job " " v-wst-job " "
                                               v-hrs-job skip
               "FINISHED GOODS TOTALS: " at 56 v-fg-job skip
               "OTHER MATERIAL TOTALS: " at 56 v-oth-job skip(2).
  
           IF tb_excel THEN
              RUN print-job-tot-excel(INPUT v-brd-job, INPUT v-mch-job,
                                      INPUT v-wst-job, INPUT v-hrs-job, 
                                      INPUT v-fg-job, INPUT v-oth-job). */
        PUT SKIP str-line SKIP .
        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "" .
                       
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "trns-typ"         THEN 
                    cVarValue =  "" .
                WHEN "trns-dt"      THEN 
                    cVarValue =  "" .
                WHEN "job-no"           THEN 
                    cVarValue =  "" .
                WHEN "frm"              THEN 
                    cVarValue =  "" .
                WHEN "blnk"             THEN 
                    cVarValue =  "" .
                WHEN "i-no"             THEN 
                    cVarValue =  "" .
                WHEN "dscr"             THEN 
                    cVarValue =  "" .
                WHEN "qty-pstd"         THEN 
                    cVarValue =  STRING(v-brd-job,">>>>>>9.99-")  .
                WHEN "wst-qty"          THEN 
                    cVarValue =  "" .
                WHEN "mch-hrs"          THEN 
                    cVarValue =  "".
                WHEN "mch-cd"           THEN 
                    cVarValue =  "" .
                WHEN "job-cd"           THEN 
                    cVarValue =  "" .
                WHEN "vc"               THEN 
                    cVarValue =  "" .
                    
            END CASE.  
                                 
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.

        PUT UNFORMATTED "JOB TOTALS - " + work-edit.job-no + "-" + string(work-edit.job-no2,"999") + "         BOARD TOTALS: "
            SUBSTRING(cDisplay,44,300) SKIP.
        IF tb_excel THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                " JOB TOTALS    " work-edit.job-no + "-" + string(work-edit.job-no2,"999") + "         BOARD TOTALS: " SUBSTRING(cExcelDisplay,3,300) SKIP.
        END. 

        PUT SKIP str-line SKIP .
        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "" .
                       
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "trns-typ"         THEN 
                    cVarValue =  "" .
                WHEN "trns-dt"      THEN 
                    cVarValue =  "" .
                WHEN "job-no"           THEN 
                    cVarValue =  "" .
                WHEN "frm"              THEN 
                    cVarValue =  "" .
                WHEN "blnk"             THEN 
                    cVarValue =  "" .
                WHEN "i-no"             THEN 
                    cVarValue =  "" .
                WHEN "dscr"             THEN 
                    cVarValue =  "" .
                WHEN "qty-pstd"         THEN 
                    cVarValue =  STRING(v-mch-job,">>>>>>9.99-")  .
                WHEN "wst-qty"          THEN 
                    cVarValue =  STRING(v-wst-job,">>>>9-").
                WHEN "mch-hrs"          THEN 
                    cVarValue =  STRING(v-hrs-job,">>9.99-").
                WHEN "mch-cd"           THEN 
                    cVarValue =  "" .
                WHEN "job-cd"           THEN 
                    cVarValue =  "" .
                WHEN "vc"               THEN 
                    cVarValue =  "" .
                    
            END CASE.  
                                 
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.

        PUT UNFORMATTED 
            "MACHINE TOTALS : " SUBSTRING(cDisplay,17,300) SKIP.
        IF tb_excel THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                " MACHINE TOTALS " + substring(cExcelDisplay,3,300) SKIP.
        END.
        PUT SKIP str-line SKIP .
        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "" .
                       
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "trns-typ"         THEN 
                    cVarValue =  "" .
                WHEN "trns-dt"      THEN 
                    cVarValue =  "" .
                WHEN "job-no"           THEN 
                    cVarValue =  "" .
                WHEN "frm"              THEN 
                    cVarValue =  "" .
                WHEN "blnk"             THEN 
                    cVarValue =  "" .
                WHEN "i-no"             THEN 
                    cVarValue =  "" .
                WHEN "dscr"             THEN 
                    cVarValue =  "" .
                WHEN "qty-pstd"         THEN 
                    cVarValue =  STRING(v-fg-job,">>>>>>9.99-")  .
                WHEN "wst-qty"          THEN 
                    cVarValue =  "" .
                WHEN "mch-hrs"          THEN 
                    cVarValue = "" .
                WHEN "mch-cd"           THEN 
                    cVarValue =  "" .
                WHEN "job-cd"           THEN 
                    cVarValue =  "" .
                WHEN "vc"               THEN 
                    cVarValue =  "" .
                    
            END CASE.  
                                 
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.

        PUT UNFORMATTED 
            "FINISHED GOODS TOTALS : " SUBSTRING(cDisplay,24,300) SKIP.
        IF tb_excel THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                " FINISHED GOODS TOTALS " + substring(cExcelDisplay,3,300) SKIP.
        END.

        PUT SKIP str-line SKIP .
        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "" .
                       
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "trns-typ"         THEN 
                    cVarValue =  "" .
                WHEN "trns-dt"      THEN 
                    cVarValue =  "" .
                WHEN "job-no"           THEN 
                    cVarValue =  "" .
                WHEN "frm"              THEN 
                    cVarValue =  "" .
                WHEN "blnk"             THEN 
                    cVarValue =  "" .
                WHEN "i-no"             THEN 
                    cVarValue =  "" .
                WHEN "dscr"             THEN 
                    cVarValue =  "" .
                WHEN "qty-pstd"         THEN 
                    cVarValue =  STRING(v-oth-job,">>>>>>9.99-")  .
                WHEN "wst-qty"          THEN 
                    cVarValue =  "" .
                WHEN "mch-hrs"          THEN 
                    cVarValue =  "" .
                WHEN "mch-cd"           THEN 
                    cVarValue =  "" .
                WHEN "job-cd"           THEN 
                    cVarValue =  "" .
                WHEN "vc"               THEN 
                    cVarValue =  "" .
                    
            END CASE.  
                                 
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.

        PUT UNFORMATTED 
            "OTHER MATERIAL TOTALS : " SUBSTRING(cDisplay,24,300) SKIP(2).
        IF tb_excel THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                " OTHER MATERIAL TOTALS " + substring(cExcelDisplay,3,300) SKIP.
        END.

    END.
    /*  put skip(1) "REPORT TOTALS" at 20
             "         BOARD TOTALS: " at 56 v-brd-tot skip
             "       MACHINE TOTALS: " at 56 v-mch-tot " " v-wst-tot " "
                                             v-hrs-tot skip
             "FINISHED GOODS TOTALS: " at 56 v-fg-tot skip
             "OTHER MATERIAL TOTALS: " at 56 v-oth-tot skip.

      IF tb_excel THEN
         RUN print-rep-tot-excel(INPUT v-brd-tot, INPUT v-mch-tot,
                                 INPUT v-wst-tot, INPUT v-hrs-tot, 
                                 INPUT v-fg-tot, INPUT v-oth-tot). */

    PUT SKIP str-line SKIP .
    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "" .
                       
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        CASE cTmpField:             
            WHEN "trns-typ"         THEN 
                cVarValue =  "" .
            WHEN "trns-dt"      THEN 
                cVarValue =  "" .
            WHEN "job-no"           THEN 
                cVarValue =  "" .
            WHEN "frm"              THEN 
                cVarValue =  "" .
            WHEN "blnk"             THEN 
                cVarValue =  "" .
            WHEN "i-no"             THEN 
                cVarValue =  "" .
            WHEN "dscr"             THEN 
                cVarValue =  "" .
            WHEN "qty-pstd"         THEN 
                cVarValue =  STRING(v-brd-tot,">>>>>>9.99-")  .
            WHEN "wst-qty"          THEN 
                cVarValue =  "" .
            WHEN "mch-hrs"          THEN 
                cVarValue =  "".
            WHEN "mch-cd"           THEN 
                cVarValue =  "" .
            WHEN "job-cd"           THEN 
                cVarValue =  "" .
            WHEN "vc"               THEN 
                cVarValue =  "" .
                    
        END CASE.  
                                 
        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.

    PUT UNFORMATTED 
        "REPORT TOTALS  "  "             BOARD TOTALS: "
        SUBSTRING(cDisplay,43,300) SKIP(1).
    IF tb_excel THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            " REPORT TOTALS    "   "         BOARD TOTALS: " SUBSTRING(cExcelDisplay,3,300) SKIP.
    END.

    PUT SKIP str-line SKIP .
    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "" .
                       
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        CASE cTmpField:             
            WHEN "trns-typ"         THEN 
                cVarValue =  "" .
            WHEN "trns-dt"      THEN 
                cVarValue =  "" .
            WHEN "job-no"           THEN 
                cVarValue =  "" .
            WHEN "frm"              THEN 
                cVarValue =  "" .
            WHEN "blnk"             THEN 
                cVarValue =  "" .
            WHEN "i-no"             THEN 
                cVarValue =  "" .
            WHEN "dscr"             THEN 
                cVarValue =  "" .
            WHEN "qty-pstd"         THEN 
                cVarValue =  STRING(v-mch-tot,">>>>>>9.99-")  .
            WHEN "wst-qty"          THEN 
                cVarValue =  STRING(v-wst-tot,">>>>9-").
            WHEN "mch-hrs"          THEN 
                cVarValue =  STRING(v-hrs-tot,">>9.99-").
            WHEN "mch-cd"           THEN 
                cVarValue =  "" .
            WHEN "job-cd"           THEN 
                cVarValue =  "" .
            WHEN "vc"               THEN 
                cVarValue =  "" .
                    
        END CASE.  
                                 
        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.

    PUT UNFORMATTED 
        "MACHINE TOTALS : " SUBSTRING(cDisplay,17,300) SKIP.
    IF tb_excel THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            " MACHINE TOTALS " + substring(cExcelDisplay,3,300) SKIP.
    END.

    PUT SKIP str-line SKIP .
    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "" .
                       
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        CASE cTmpField:             
            WHEN "trns-typ"         THEN 
                cVarValue =  "" .
            WHEN "trns-dt"      THEN 
                cVarValue =  "" .
            WHEN "job-no"           THEN 
                cVarValue =  "" .
            WHEN "frm"              THEN 
                cVarValue =  "" .
            WHEN "blnk"             THEN 
                cVarValue =  "" .
            WHEN "i-no"             THEN 
                cVarValue =  "" .
            WHEN "dscr"             THEN 
                cVarValue =  "" .
            WHEN "qty-pstd"         THEN 
                cVarValue =  STRING(v-fg-tot,">>>>>>9.99-")  .
            WHEN "wst-qty"          THEN 
                cVarValue =  "" .
            WHEN "mch-hrs"          THEN 
                cVarValue = "" .
            WHEN "mch-cd"           THEN 
                cVarValue =  "" .
            WHEN "job-cd"           THEN 
                cVarValue =  "" .
            WHEN "vc"               THEN 
                cVarValue =  "" .
                    
        END CASE.  
                                 
        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.

    PUT UNFORMATTED 
        "FINISHED GOODS TOTALS : " SUBSTRING(cDisplay,24,300) SKIP.
    IF tb_excel THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            " FINISHED GOODS TOTALS " + substring(cExcelDisplay,3,300) SKIP.
    END.

    PUT SKIP str-line SKIP .
    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "" .
                       
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        CASE cTmpField:             
            WHEN "trns-typ"         THEN 
                cVarValue =  "" .
            WHEN "trns-dt"      THEN 
                cVarValue =  "" .
            WHEN "job-no"           THEN 
                cVarValue =  "" .
            WHEN "frm"              THEN 
                cVarValue =  "" .
            WHEN "blnk"             THEN 
                cVarValue =  "" .
            WHEN "i-no"             THEN 
                cVarValue =  "" .
            WHEN "dscr"             THEN 
                cVarValue =  "" .
            WHEN "qty-pstd"         THEN 
                cVarValue =  STRING(v-oth-tot,">>>>>>9.99-")  .
            WHEN "wst-qty"          THEN 
                cVarValue =  "" .
            WHEN "mch-hrs"          THEN 
                cVarValue =  "" .
            WHEN "mch-cd"           THEN 
                cVarValue =  "" .
            WHEN "job-cd"           THEN 
                cVarValue =  "" .
            WHEN "vc"               THEN 
                cVarValue =  "" .
                    
        END CASE.  
                                 
        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.

    PUT UNFORMATTED 
        "OTHER MATERIAL TOTALS : " SUBSTRING(cDisplay,24,300) SKIP(1).
    IF tb_excel THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            " OTHER MATERIAL TOTALS " + substring(cExcelDisplay,3,300) SKIP.
    END.

    /***************************************************** CTS Enh 052495-01
    put control v-end-compress.   /** reset printer pitch **/
    output close.
    ****** Call sys/inc/close.i so Smart Screen print works correctly ******/

    /* end ---------------------------------- copr. 2001 Advanced Software, Inc. */


    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
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
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + ",".
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
            fi_file:SCREEN-VALUE = "c:\tmp\WIPPosting.csv".   
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

