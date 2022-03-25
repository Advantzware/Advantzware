&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd-exlexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd-exlexp 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER piMchFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER piMchTo AS CHARACTER NO-UNDO.
/*DEFINE INPUT PARAMETER pcVendFrom LIKE po-ord.vend-no NO-UNDO.
DEFINE INPUT PARAMETER pcVendTo   LIKE po-ord.vend-no NO-UNDO.
DEFINE INPUT PARAMETER pcItemFrom LIKE po-ordl.i-no NO-UNDO.
DEFINE INPUT PARAMETER pcItemTo   LIKE po-ordl.i-no NO-UNDO.
DEFINE INPUT PARAMETER pcVendItemFrom LIKE po-ordl.vend-i-no NO-UNDO.
DEFINE INPUT PARAMETER pcVendItemTo   LIKE po-ordl.vend-i-no NO-UNDO.
DEFINE INPUT PARAMETER pcJobFrom LIKE po-ordl.job-no NO-UNDO.
DEFINE INPUT PARAMETER pcJobTo   LIKE po-ordl.job-no NO-UNDO.
DEFINE INPUT PARAMETER piJob2From LIKE po-ordl.job-no2 NO-UNDO.
DEFINE INPUT PARAMETER piJob2To   LIKE po-ordl.job-no2 NO-UNDO.
DEFINE INPUT PARAMETER pdDateFrom  LIKE po-ordl.due-date NO-UNDO.
DEFINE INPUT PARAMETER pdDateTo    LIKE po-ordl.due-date NO-UNDO.
DEFINE INPUT PARAMETER piOpenClosed AS INT NO-UNDO. */


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

{methods/defines/globdefs.i}

DEFINE BUFFER b-prgrms FOR prgrms.
DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.

{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/


IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
    v-prgmname = USERID("NOSWEAT") + "..".
ELSE
    ASSIGN
        /*   period_pos = INDEX(PROGRAM-NAME(1),".")                                             */
        /*   v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 10) + 1) */
        v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1).
v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
ASSIGN 
    cTextListToSelect  = "Machine,Job,Sub,Form,Blk,Pass,Charge,Start Date,Log In,End Date,Shift,Log Out,Run,Total"
    cFieldListToSelect = "machtran.machine,machtran.job_number,machtran.job_sub,machtran.form_number,machtran.blank_number,machtran.pass_sequence,machtran.charge_code,machtran.start_date,strt-time,machtran.end_date,machtran.shift,end-time,machtran.run_qty,ttl-time"
    .

{sys/inc/ttRptSel.i}

ASSIGN 
    cTextListToDefault = "Machine,Job,Sub,Form,Blk,Pass,Charge,Start Date,Log In,End Date,Shift,Log Out,Run,Total" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-exlexp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_date end_date begin_mch end_mch ~
begin_job begin_job2 end_job end_job2 sl_avail Btn_Def Btn_Add sl_selected ~
Btn_Remove btn_Up btn_down tb_OpenCSV fi_file btn-ok btn-cancel RECT-6 ~
RECT-7 RECT-8 tbAutoClose 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_mch end_mch ~
begin_job begin_job2 end_job end_job2 sl_avail sl_selected tb_OpenCSV ~
fi_file tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-exlexp 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-exlexp 
FUNCTION assignParam RETURNS CHARACTER
    ( ipc-param AS CHARACTER , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-exlexp 
FUNCTION buildHeader RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue rd-exlexp 
FUNCTION getValue RETURNS CHARACTER
    ( BUFFER ipb-machtran FOR machtran, 
    ipc-field AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-machtran rd-exlexp 
FUNCTION getValue-machtran RETURNS CHARACTER
    ( BUFFER ipb-buffer FOR machtran, ipc-field AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE BUTTON Btn_Add 
    LABEL "&Add >>" 
    SIZE 16 BY 1.

DEFINE BUTTON Btn_Def 
    LABEL "&Default" 
    SIZE 16 BY 1.

DEFINE BUTTON btn_down 
    LABEL "Move Down" 
    SIZE 16 BY 1.

DEFINE BUTTON Btn_Remove 
    LABEL "<< &Remove" 
    SIZE 16 BY 1.

DEFINE BUTTON btn_Up 
    LABEL "Move Up" 
    SIZE 16 BY 1.

DEFINE VARIABLE begin_date AS DATE      FORMAT "99/99/9999" INITIAL 01/01/1901 
    LABEL "From Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_job  AS CHARACTER FORMAT "X(9)" 
    LABEL "From Job #" 
    VIEW-AS FILL-IN 
    SIZE 13.2 BY 1.

DEFINE VARIABLE begin_job2 AS INTEGER   FORMAT "999":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 5.5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mch  AS CHARACTER FORMAT "X(6)" 
    LABEL "From Machine" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date   AS DATE      FORMAT "99/99/9999" INITIAL 12/31/2099 
    LABEL "To Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_job    AS CHARACTER FORMAT "X(9)" INITIAL "zzzzzzzzz" 
    LABEL "To Job #" 
    VIEW-AS FILL-IN 
    SIZE 13.2 BY 1.

DEFINE VARIABLE end_job2   AS INTEGER   FORMAT ">>9":U INITIAL 999 
    VIEW-AS FILL-IN 
    SIZE 5.5 BY 1 NO-UNDO.

DEFINE VARIABLE end_mch    AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "To Machine" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file    AS CHARACTER FORMAT "X(48)" INITIAL "c:~\tmp~\ExpMachineTransactions.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 54 BY 1.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 96 BY 6.67.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 96 BY 5.43.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 96 BY 2.71.

DEFINE VARIABLE sl_avail    AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31 BY 5.1 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31 BY 5.1 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel    AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV  AS LOGICAL   INITIAL YES 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-exlexp
    begin_date AT ROW 2.71 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Due Date" WIDGET-ID 112
    end_date AT ROW 2.67 COL 69 COLON-ALIGNED HELP
    "Enter Ending Due Date" WIDGET-ID 114
    begin_mch AT ROW 3.81 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Vendor Item Number" WIDGET-ID 104
    end_mch AT ROW 3.81 COL 69 COLON-ALIGNED HELP
    "Enter Ending Vendor Item Number" WIDGET-ID 106
    begin_job AT ROW 4.95 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Job Number" WIDGET-ID 108
    begin_job2 AT ROW 4.95 COL 39.2 COLON-ALIGNED HELP
    "Enter Beginning Job Number" NO-LABELS WIDGET-ID 116
    end_job AT ROW 4.95 COL 69 COLON-ALIGNED HELP
    "Enter Ending Job Number" WIDGET-ID 110
    end_job2 AT ROW 4.95 COL 82 COLON-ALIGNED HELP
    "Enter Ending Job Number" NO-LABELS WIDGET-ID 118
    sl_avail AT ROW 8.71 COL 7 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 8.76 COL 43.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    Btn_Add AT ROW 9.71 COL 43.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 130
    sl_selected AT ROW 8.71 COL 65 NO-LABELS WIDGET-ID 28
    Btn_Remove AT ROW 10.67 COL 43.2 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 134
    btn_Up AT ROW 11.62 COL 43.2 WIDGET-ID 136
    btn_down AT ROW 12.57 COL 43.2 WIDGET-ID 132
    tb_excel AT ROW 17.19 COL 4 WIDGET-ID 32
    tb_OpenCSV AT ROW 15.38 COL 90.2 RIGHT-ALIGNED WIDGET-ID 34
    fi_file AT ROW 15.29 COL 19.4 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 22
    btn-ok AT ROW 18.14 COL 32.4 WIDGET-ID 14
    btn-cancel AT ROW 18.14 COL 52.4 WIDGET-ID 12
    tbAutoClose AT ROW 17.19 COL 43.8 WIDGET-ID 58
    "Available Columns" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 8 COL 14 WIDGET-ID 140
    "Selected Columns" VIEW-AS TEXT
    SIZE 18.6 BY .62 AT ROW 8 COL 70.6 WIDGET-ID 138
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5 WIDGET-ID 36
    " Export Selection" VIEW-AS TEXT
    SIZE 17 BY .62 AT ROW 7.24 COL 5 WIDGET-ID 86
    RECT-6 AT ROW 7.52 COL 4 WIDGET-ID 30
    RECT-7 AT ROW 1.52 COL 4 WIDGET-ID 38
    RECT-8 AT ROW 14.33 COL 4 WIDGET-ID 84
    SPACE(2.99) SKIP(3.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    BGCOLOR 15 
    TITLE "Export to Excel" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX rd-exlexp
   FRAME-NAME Custom                                                    */
ASSIGN 
    FRAME rd-exlexp:SCROLLABLE = FALSE
    FRAME rd-exlexp:HIDDEN     = TRUE.

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME rd-exlexp = "parm".

ASSIGN 
    begin_job:PRIVATE-DATA IN FRAME rd-exlexp = "parm".

ASSIGN 
    begin_mch:PRIVATE-DATA IN FRAME rd-exlexp = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME rd-exlexp = "parm".

ASSIGN 
    end_job:PRIVATE-DATA IN FRAME rd-exlexp = "parm".

ASSIGN 
    end_mch:PRIVATE-DATA IN FRAME rd-exlexp = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME rd-exlexp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-exlexp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_excel:HIDDEN IN FRAME rd-exlexp       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME rd-exlexp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME rd-exlexp
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME rd-exlexp = "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-exlexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-exlexp rd-exlexp
ON HELP OF FRAME rd-exlexp /* Export to Excel */
    DO:
        DEFINE VARIABLE lw-focus   AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE ls-cur-val AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE char-val   AS CHARACTER     NO-UNDO.

        lw-focus = FOCUS.
    
        CASE lw-focus:NAME :

            WHEN "begin_vend-no" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-vendno.w (cocode, "", ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* vend-no*/
            WHEN "end_vend-no" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-vendno.w (cocode, "", ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* vend-no*/
            WHEN "begin_item" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-itemfg.w (cocode, "", ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* itemfg */
            WHEN "end_item" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-itemfg.w (cocode, "", ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* itemfg*/
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-exlexp rd-exlexp
ON WINDOW-CLOSE OF FRAME rd-exlexp /* Export to Excel */
    DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date rd-exlexp
ON LEAVE OF begin_date IN FRAME rd-exlexp /* From Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job rd-exlexp
ON LEAVE OF begin_job IN FRAME rd-exlexp /* From Job # */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mch rd-exlexp
ON LEAVE OF begin_mch IN FRAME rd-exlexp /* From Machine */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-exlexp
ON CHOOSE OF btn-cancel IN FRAME rd-exlexp /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd-exlexp
ON CHOOSE OF btn-ok IN FRAME rd-exlexp /* OK */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
  
        ASSIGN 
            fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
        RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
        fi_file:SCREEN-VALUE =  cFileName.
  
        RUN GetSelectionList.  
        RUN run-report.
  
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
        END.  /* IF NOT tb_OpenCSV THEN */
  
        IF tbAutoClose:CHECKED THEN 
            APPLY "END-ERROR":U TO SELF.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-exlexp
ON CHOOSE OF Btn_Add IN FRAME rd-exlexp /* Add >> */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-exlexp
ON CHOOSE OF Btn_Def IN FRAME rd-exlexp /* Default */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        RUN DisplaySelectionDefault.  /* task 04041406 */ 
        RUN DisplaySelectionList2 .
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd-exlexp
ON CHOOSE OF btn_down IN FRAME rd-exlexp /* Move Down */
    DO:
        RUN Move-Field ("Down").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd-exlexp
ON CHOOSE OF Btn_Remove IN FRAME rd-exlexp /* << Remove */
    DO:
        APPLY "DEFAULT-ACTION" TO sl_selected  .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd-exlexp
ON CHOOSE OF btn_Up IN FRAME rd-exlexp /* Move Up */
    DO:
        RUN Move-Field ("Up").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date rd-exlexp
ON LEAVE OF end_date IN FRAME rd-exlexp /* To Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job rd-exlexp
ON LEAVE OF end_job IN FRAME rd-exlexp /* To Job # */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mch rd-exlexp
ON LEAVE OF end_mch IN FRAME rd-exlexp /* To Machine */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME  


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-exlexp
ON HELP OF fi_file IN FRAME rd-exlexp /* Name */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-exlexp
ON LEAVE OF fi_file IN FRAME rd-exlexp /* Name */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd-exlexp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd-exlexp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-exlexp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-exlexp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-exlexp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-exlexp /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV rd-exlexp
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME rd-exlexp /* Open CSV? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-exlexp 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {methods/nowait.i}
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
RUN DisplaySelectionList2.
RUN Set-Sort-Data.

APPLY "entry" TO begin_date.
fi_file:SCREEN-VALUE = "c:\tmp\ExpMachineTransactions.csv".
END.
WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd-exlexp  _DEFAULT-DISABLE
PROCEDURE disable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     DISABLE the User Interface
      Parameters:  <none>
      Notes:       Here we clean-up the user-interface by deleting
                   dynamic widgets we have created and/or hide 
                   frames.  This procedure is usually called when
                   we are ready to "clean-up" after running.
    ------------------------------------------------------------------------------*/
    /* Hide all frames. */
    HIDE FRAME rd-exlexp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd-exlexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-exlexp 
PROCEDURE DisplaySelectionList :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.

    /*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
    /*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-exlexp 
PROCEDURE DisplaySelectionList2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.

    /*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
    /*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd-exlexp  _DEFAULT-ENABLE
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
    DISPLAY begin_date end_date begin_mch end_mch begin_job begin_job2 end_job 
        end_job2 sl_avail sl_selected tb_OpenCSV fi_file tbAutoClose 
        WITH FRAME rd-exlexp.
    ENABLE begin_date end_date begin_mch end_mch begin_job begin_job2 end_job 
        end_job2 sl_avail Btn_Def Btn_Add sl_selected Btn_Remove btn_Up 
        btn_down tb_OpenCSV fi_file btn-ok btn-cancel RECT-6 RECT-7 RECT-8 
        tbAutoClose 
        WITH FRAME rd-exlexp.
    VIEW FRAME rd-exlexp.
    {&OPEN-BROWSERS-IN-QUERY-rd-exlexp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd-exlexp 
PROCEDURE GetSelectionList :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTmpList AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE ttRptSelected.
    cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME} :
        FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.  
        CREATE ttRptSelected.
        ASSIGN 
            ttRptSelected.TextList  = ENTRY(i,cTmpList)
            ttRptSelected.FieldList = ttRptList.FieldList
            /* ttRptSelected.FieldLength */
            .   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd-exlexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-exlexp 
PROCEDURE run-report :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-excelheader        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-adder              AS CHARACTER NO-UNDO.
    DEFINE BUFFER xjob-mat FOR job-mat.
    DEFINE BUFFER xitem    FOR item.

    v-excelheader = buildHeader().
    SESSION:SET-WAIT-STATE ("general").

    IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).
    IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.


    FOR EACH machtran WHERE machtran.company = cocode
        AND machtran.posted = NO
        AND machtran.start_date GE begin_date 
        AND machtran.start_date LE end_date
        AND machtran.machine GE begin_mch
        AND machtran.machine LE END_mch
        AND machtran.job_number GE begin_job
        AND machtran.job_number LE END_job
        AND machtran.job_sub GE begin_job2
        AND machtran.job_sub LE  end_job2 NO-LOCK BY machtran.machine :

        v-excel-detail-lines = "".
   
        FOR EACH ttRptSelected:

            IF LOOKUP(ttRptSelected.FieldList,"strt-time,end-time,ttl-time") EQ 0 THEN 
            DO:
                v-excel-detail-lines = v-excel-detail-lines + 
                    appendXLLine(getValue(BUFFER machtran,ttRptSelected.FieldList)).
            END.
            ELSE 
            DO:
                CASE ttRptSelected.FieldList:                                                                                       
             
                    WHEN "strt-time" THEN 
                        DO:
                            v-excel-detail-lines = v-excel-detail-lines + appendXLLine(TRIM(STRING(machtran.start_time,'HH:MM am'))).                          
                        END.
                    WHEN "end-time" THEN 
                        DO:
                            IF machtran.end_time = 0 AND machtran.end_date EQ ? THEN
                                v-excel-detail-lines = v-excel-detail-lines + "," .
                            ELSE
                                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(TRIM(STRING(machtran.end_time,'HH:MM am'))).                          
                        END.
                    WHEN "ttl-time" THEN 
                        DO:
                            IF machtran.total_time = 0 AND machtran.end_date EQ ? THEN
                                v-excel-detail-lines = v-excel-detail-lines + "," .
                            ELSE
                                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(TRIM(STRING(machtran.total_time,'HH:MM'))).                          
                        END.
             
                END CASE. 
            END.
        END.

        PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.
    END.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd-exlexp 
PROCEDURE Set-Sort-Data :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:

        /* If a customer number was entered, find first and last matching customers. */
        ASSIGN 
            begin_mch:SCREEN-VALUE = STRING(piMchFrom)
            end_mch:SCREEN-VALUE   = STRING(piMchTo) .

    /*  ASSIGN 
          begin_po:SCREEN-VALUE = string(piPOFrom)
          begin_job:SCREEN-VALUE = pcJobFrom
          begin_job2:SCREEN-VALUE = string(piJob2From)
          end_job2:SCREEN-VALUE = string(99)
          end_date:SCREEN-VALUE = string(12/31/2099)
          rd_open-closed:SCREEN-VALUE = STRING(piOpenClosed).
          
          IF pdDateFrom NE ? THEN 
              begin_date:SCREEN-VALUE = string(pdDateFrom).
          ELSE
              begin_date:SCREEN-VALUE = string(01/01/1901).
          IF pcJobFrom NE "" THEN 
              end_job:SCREEN-VALUE   = pcJobTo.
          ELSE
              end_job:SCREEN-VALUE   = "ZZZZZZZZZZZZ".
          IF piPOFrom NE 0 THEN 
              end_po:SCREEN-VALUE   = string(piPOTo).
          ELSE
              end_po:SCREEN-VALUE   = string(99999999).
      ASSIGN 
          begin_vend-no:SCREEN-VALUE = assignParam(pcVendFrom,NO)
          end_vend-no:SCREEN-VALUE   = assignParam(pcVendTo,YES)
          begin_item:SCREEN-VALUE = assignParam(pcItemFrom,NO)
          end_item:SCREEN-VALUE   = assignParam(pcItemTo,YES)
          begin_vend-i-no:SCREEN-VALUE = assignParam(pcVendItemFrom,NO)
          end_vend-i-no:SCREEN-VALUE   = assignParam(pcVendItemTo,YES). */
    END.
    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-exlexp 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Adds a value to a csv line
        Notes:  Protects agains commans and quotes.
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-line AS CHARACTER NO-UNDO.

    ipc-append = REPLACE(ipc-append, '"', '').
    ipc-append = REPLACE(ipc-append, ',', ' ').
    lc-line = lc-line + '"' + ipc-append + '",'.
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-exlexp 
FUNCTION assignParam RETURNS CHARACTER
    ( ipc-param AS CHARACTER , ipl-end AS LOG) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-return AS CHARACTER.

    IF ipl-end THEN
        lc-return = ipc-param + "ZZZZZZZZZZZZZZZ".
    ELSE
        lc-return = ipc-param.

    RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd-exlexp 
FUNCTION buildHeader RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-header AS CHARACTER NO-UNDO.

    FOR EACH ttRptSelected:
        lc-header = lc-header + appendXLLine(ttRptSelected.TextList).
    END.
    /*     lc-header = lc-header + appendXLLine ("PO #").      */
    /*     lc-header = lc-header + appendXLLine ("Vendor #").  */
    /*     lc-header = lc-header + appendXLLine ("Due Date").  */
    /*     lc-header = lc-header + appendXLLine ("Ship ID").   */
    /*     lc-header = lc-header + appendXLLine ("Ship Name"). */
    /*     lc-header = lc-header + appendXLLine ("Job #").     */
    /*     lc-header = lc-header + appendXLLine ("Item #").    */
    /*     lc-header = lc-header + appendXLLine ("Item Name"). */

  
    RETURN lc-header.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue rd-exlexp 
FUNCTION getValue RETURNS CHARACTER
    ( BUFFER ipb-machtran FOR machtran, 
    ipc-field AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Take a buffer and field name as string and return the value
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-return AS CHARACTER FORMAT "x(100)" NO-UNDO.
    DEFINE VARIABLE li-period AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lc-table  AS CHARACTER NO-UNDO.

    li-period = INDEX(ipc-field,".").
    IF li-period > 0 THEN 
    DO:
        lc-table = SUBSTRING(ipc-field, 1, li-period - 1).
        ipc-field = SUBSTRING(ipc-field, li-period + 1,  LENGTH(ipc-field) - li-period).
        CASE lc-table:
            WHEN "machtran" THEN
                lc-return = getValue-machtran(BUFFER ipb-machtran, ipc-field).
            OTHERWISE
            lc-return = "".
        END CASE.
    END.
    ELSE
        lc-return = getValue-machtran(BUFFER ipb-machtran, ipc-field).
    RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-machtran rd-exlexp 
FUNCTION getValue-machtran RETURNS CHARACTER
    ( BUFFER ipb-buffer FOR machtran, ipc-field AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Take a buffer and field name as string and return the value
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE h-field   AS HANDLE.
    DEFINE VARIABLE li-extent AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lc-return AS CHARACTER FORMAT "x(100)" NO-UNDO.
    DEFINE BUFFER lb-oe-ord  FOR oe-ord.
    DEFINE BUFFER lb-job-hdr FOR job-hdr.

    CASE ipc-field :
        WHEN "dfuncFGFromJob"  THEN 
            DO:
            /* IF ipb-buffer.job-no NE "" THEN DO:
                 FIND FIRST lb-job-hdr WHERE lb-job-hdr.company = ipb-buffer.company
                     AND lb-job-hdr.job-no = ipb-buffer.job-no
                     AND lb-job-hdr.job-no2 = ipb-buffer.job-no2 NO-LOCK NO-ERROR.
                 IF AVAIL lb-job-hdr THEN
                     lc-return = lb-job-hdr.i-no.
             END.*/
            END.
        WHEN "dfuncCustFromOrder"  THEN 
            DO:
            /*   IF ipb-buffer.ord-no > 0 THEN DO:
                   FIND FIRST lb-oe-ord WHERE lb-oe-ord.company = ipb-buffer.company
                       AND lb-oe-ord.ord-no = ipb-buffer.ord-no NO-LOCK NO-ERROR.
                   IF AVAIL lb-oe-ord THEN
                       lc-return = lb-oe-ord.cust-no.
               END.*/
            END.
        OTHERWISE 
        DO:
            IF INDEX(ipc-field,"[") > 0 THEN 
            DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-buffer:BUFFER-FIELD(ipc-field).
            IF h-field:EXTENT = 0 THEN
                lc-return = STRING(h-field:BUFFER-VALUE /*, h-field:FORMAT*/ ).
            ELSE
                lc-return = STRING(h-field:BUFFER-VALUE(li-extent) /*, h-field:FORMAT*/ ).
        END.
    END CASE.
    IF lc-return EQ ? THEN lc-return = "".
    RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

