&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd-fgnq-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd-fgnq-exp 
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
DEFINE INPUT PARAMETER lcSearch   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER lcsearchby AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

{custom/gperiod.i}
{custom/persist.i}

{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}          */

/*{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}*/
DEFINE {&NEW} SHARED VARIABLE g_batch       AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE g_batch-rowid AS ROWID   NO-UNDO.
DEFINE               VARIABLE v-prgmname    LIKE prgrms.prgmname NO-UNDO.
{sys/inc/var.i new shared}
v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1).
v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

ASSIGN
    cocode = g_company
    /*locode = gloc*/ .


DEFINE TEMP-TABLE tt-arinq NO-UNDO
    FIELD ref-num AS CHARACTER FORM "x(15)" LABEL "Ck/Cr/Dr/Po#"
    FIELD inv-no  LIKE ar-inv.inv-no LABEL "Invoice#"
    FIELD tr-date AS DATE      FORM "99/99/9999" LABEL "Date"
    FIELD tr-dscr LIKE glhist.tr-dscr LABEL "Description"
    FIELD tr-damt LIKE glhist.tr-amt LABEL "Debits"
    FIELD tr-camt LIKE glhist.tr-amt LABEL "Credits"
    FIELD ageapp  AS CHARACTER FORM "x(5)" LABEL "Age App"
    FIELD tr-from AS CHARACTER LABEL "Inquiry From"
    FIELD balance AS DECIMAL   FORM "->>>,>>>,>>9.99" LABEL "Balance"
    FIELD applied AS LOG
    FIELD seq     AS INTEGER
    FIELD printed LIKE ar-inv.printed
    FIELD posted  LIKE ar-inv.posted
    INDEX seq                seq
    INDEX applied IS PRIMARY applied seq
    INDEX ref-num            ref-num seq
    INDEX inv-no             inv-no  seq
    INDEX tr-date            tr-date seq
    INDEX tr-dscr            tr-dscr seq
    INDEX tr-damt            tr-damt seq
    INDEX tr-camt            tr-camt seq
    INDEX ageapp             ageapp  seq
    INDEX tr-from            tr-from seq
    INDEX balance            balance seq.

DEFINE            VARIABLE lv-first       AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-sort-by     AS CHARACTER NO-UNDO.
DEFINE            VARIABLE ll-sort-asc    AS LOG       NO-UNDO.
DEFINE            VARIABLE v-format       AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE uperiod        AS INTEGER   NO-UNDO.  /* for gl-open.p */
DEFINE            VARIABLE v-gltrans-desc AS CHARACTER NO-UNDO.

&SCOPED-DEFINE SORTBY-ASC ASCENDING
&SCOPED-DEFINE SORTBY-DES DESCENDING

DEFINE            VARIABLE lv-save-char   AS CHARACTER INIT "" NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "ARINQ"
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN 
DO ON ERROR UNDO, RETRY TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "ARINQ"
        sys-ctrl.descrip  = "AR Customer Activity Inquiry format"
        sys-ctrl.char-fld = "ASI".
    MESSAGE "Enter" sys-ctrl.descrip " (ASI/Fibre)"
        UPDATE sys-ctrl.char-fld.
    IF LOOKUP(sys-ctrl.char-fld,"ASI,Fibre") EQ 0 THEN UNDO, RETRY.
END.
v-format = sys-ctrl.char-fld.


DEFINE STREAM excel.


DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.

ASSIGN 
    cTextListToSelect  = "Ck/Cr/Dr/Po#,Invoice#,Date,Description,Age App,Debits,Credits,Balance"

    cFieldListToSelect = "ck-cr,inv,date,dscr,ag-ap,deb,cre,bal" .
{sys/inc/ttRptSel.i}

ASSIGN 
    cTextListToDefault = "Ck/Cr/Dr/Po#,Invoice#,Date,Description,Age App,Debits,Credits,Balance".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-fgnq-exp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_cust end_cust ~
begin_chk end_chk begin_inv end_inv tb_open sl_avail sl_selected Btn_Def ~
Btn_Add Btn_Remove btn_Up btn_down fi_file tb_OpenCSV tbAutoClose btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_chk end_chk ~
begin_inv end_inv tb_open sl_avail sl_selected fi_file tb_OpenCSV ~
tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-fgnq-exp 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-fgnq-exp 
FUNCTION assignParam RETURNS CHARACTER
    ( ipc-param AS CHARACTER , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-fgnq-exp 
FUNCTION buildHeader RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-itemfg rd-fgnq-exp 
FUNCTION getValue-itemfg RETURNS CHARACTER
    ( BUFFER ipb-itemfg FOR cust, ipc-field AS CHARACTER )  FORWARD.

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

DEFINE VARIABLE begin_chk  AS INT64     FORMAT ">>>>>>>>>>>>":U INITIAL 0 
    LABEL "From Check#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "x(10)" 
    LABEL "From Customer#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1.

DEFINE VARIABLE begin_inv  AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    LABEL "From Invoice#" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1.

DEFINE VARIABLE end_chk    AS INT64     FORMAT ">>>>>>>>>>>>":U INITIAL 2147483647 
    LABEL "To Check#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE end_cust   AS CHARACTER FORMAT "X(10)" INITIAL "zzzzzzzzzzz" 
    LABEL "To Customer#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE end_inv    AS INTEGER   FORMAT ">>>>>>>>" INITIAL 99999999 
    LABEL "To Invoice#" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE fi_file    AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\CustomerExport.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 52 BY 1.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 7.86.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 6.38.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 2.48.

DEFINE VARIABLE sl_avail    AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel    AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_open     AS LOGICAL   INITIAL YES 
    LABEL "Open Invoices Only?" 
    VIEW-AS TOGGLE-BOX
    SIZE 27.8 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV  AS LOGICAL   INITIAL YES 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-fgnq-exp
    begin_cust AT ROW 2.38 COL 25.4 COLON-ALIGNED HELP
    "Enter Beginning Customer#" WIDGET-ID 142
    end_cust AT ROW 2.48 COL 67 COLON-ALIGNED HELP
    "Enter Ending Customer#" WIDGET-ID 144
    begin_chk AT ROW 3.76 COL 25.4 COLON-ALIGNED HELP
    "Enter Beginning Vendor#" WIDGET-ID 146
    end_chk AT ROW 3.86 COL 67 COLON-ALIGNED HELP
    "Enter Ending Vendor#" WIDGET-ID 148
    begin_inv AT ROW 5.24 COL 25.4 COLON-ALIGNED HELP
    "Enter Beginning Vendor#" WIDGET-ID 150
    end_inv AT ROW 5.33 COL 67 COLON-ALIGNED HELP
    "Enter Ending Vendor#" WIDGET-ID 152
    tb_open AT ROW 6.67 COL 65.4 RIGHT-ALIGNED WIDGET-ID 154
    sl_avail AT ROW 9.43 COL 7 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 9.43 COL 62.6 NO-LABELS WIDGET-ID 28
    Btn_Def AT ROW 9.48 COL 42 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    Btn_Add AT ROW 10.67 COL 42 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 130
    Btn_Remove AT ROW 11.86 COL 42 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 134
    btn_Up AT ROW 13.05 COL 42 WIDGET-ID 136
    btn_down AT ROW 14.24 COL 42 WIDGET-ID 132
    fi_file AT ROW 17.1 COL 18.8 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 22
    tb_OpenCSV AT ROW 17.19 COL 87.8 RIGHT-ALIGNED WIDGET-ID 34
    tbAutoClose AT ROW 19 COL 43 WIDGET-ID 60
    tb_excel AT ROW 19.1 COL 4 WIDGET-ID 32
    btn-ok AT ROW 20.05 COL 31 WIDGET-ID 14
    btn-cancel AT ROW 20.05 COL 51 WIDGET-ID 12
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5 WIDGET-ID 36
    " Export Selection" VIEW-AS TEXT
    SIZE 17 BY .62 AT ROW 8.05 COL 5 WIDGET-ID 86
    "Selected Columns" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 8.71 COL 69.4 WIDGET-ID 138
    "Available Columns" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 8.71 COL 13.4 WIDGET-ID 140
    RECT-6 AT ROW 8.38 COL 4 WIDGET-ID 30
    RECT-7 AT ROW 1.52 COL 4 WIDGET-ID 38
    RECT-8 AT ROW 16.48 COL 4 WIDGET-ID 84
    SPACE(2.99) SKIP(2.98)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    BGCOLOR 15 
    TITLE "Export Customer to Excel" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX rd-fgnq-exp
   FRAME-NAME                                                           */
ASSIGN 
    FRAME rd-fgnq-exp:SCROLLABLE = FALSE
    FRAME rd-fgnq-exp:HIDDEN     = TRUE.

ASSIGN 
    begin_chk:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

ASSIGN 
    begin_cust:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

ASSIGN 
    begin_inv:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

ASSIGN 
    end_chk:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

ASSIGN 
    end_cust:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

ASSIGN 
    end_inv:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-fgnq-exp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_excel:HIDDEN IN FRAME rd-fgnq-exp       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_open IN FRAME rd-fgnq-exp
   ALIGN-R                                                              */
ASSIGN 
    tb_open:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME rd-fgnq-exp
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME rd-fgnq-exp = "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-fgnq-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgnq-exp rd-fgnq-exp
ON HELP OF FRAME rd-fgnq-exp /* Export Customer to Excel */
    DO:
        DEFINE VARIABLE lw-focus   AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE ls-cur-val AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE char-val   AS CHARACTER     NO-UNDO.

        lw-focus = FOCUS.

        CASE lw-focus:NAME :
        /* when "begin_i-no" then do:
             ls-cur-val = lw-focus:screen-value.
             RUN windows/l-itemfj.w (cocode, ls-cur-val, output char-val).
             if char-val <> "" then do:
                lw-focus:screen-value =  ENTRY(1,char-val).
             end.
             return no-apply.
         end.  /* itemfg */
         when "end_i-no" then do:
             ls-cur-val = lw-focus:screen-value.
             run windows/l-itemfj.w (cocode, ls-cur-val, output char-val).
             if char-val <> "" then do:
                lw-focus:screen-value =  ENTRY(1,char-val).
             end.
             return no-apply.
         end.  /* itemfg*/*/

        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgnq-exp rd-fgnq-exp
ON WINDOW-CLOSE OF FRAME rd-fgnq-exp /* Export Customer to Excel */
    DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_chk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_chk rd-fgnq-exp
ON LEAVE OF begin_chk IN FRAME rd-fgnq-exp /* From Check# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust rd-fgnq-exp
ON LEAVE OF begin_cust IN FRAME rd-fgnq-exp /* From Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv rd-fgnq-exp
ON LEAVE OF begin_inv IN FRAME rd-fgnq-exp /* From Invoice# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-fgnq-exp
ON CHOOSE OF btn-cancel IN FRAME rd-fgnq-exp /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd-fgnq-exp
ON CHOOSE OF btn-ok IN FRAME rd-fgnq-exp /* OK */
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
        END.  /* IF NOT tb_OpenCSV THEN  */
        ELSE DO:
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY "END-ERROR":U TO SELF.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-fgnq-exp
ON CHOOSE OF Btn_Add IN FRAME rd-fgnq-exp /* Add >> */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-fgnq-exp
ON CHOOSE OF Btn_Def IN FRAME rd-fgnq-exp /* Default */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        RUN DisplaySelectionDefault.  /* task 04041406 */ 
        RUN DisplaySelectionList2 .
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd-fgnq-exp
ON CHOOSE OF btn_down IN FRAME rd-fgnq-exp /* Move Down */
    DO:
        RUN Move-Field ("Down").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd-fgnq-exp
ON CHOOSE OF Btn_Remove IN FRAME rd-fgnq-exp /* << Remove */
    DO:
        APPLY "DEFAULT-ACTION" TO sl_selected  .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd-fgnq-exp
ON CHOOSE OF btn_Up IN FRAME rd-fgnq-exp /* Move Up */
    DO:
        RUN Move-Field ("Up").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_chk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_chk rd-fgnq-exp
ON LEAVE OF end_chk IN FRAME rd-fgnq-exp /* To Check# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust rd-fgnq-exp
ON LEAVE OF end_cust IN FRAME rd-fgnq-exp /* To Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv rd-fgnq-exp
ON LEAVE OF end_inv IN FRAME rd-fgnq-exp /* To Invoice# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgnq-exp
ON HELP OF fi_file IN FRAME rd-fgnq-exp /* Name */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgnq-exp
ON LEAVE OF fi_file IN FRAME rd-fgnq-exp /* Name */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd-fgnq-exp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd-fgnq-exp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-fgnq-exp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-fgnq-exp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-fgnq-exp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-fgnq-exp /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_open
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_open rd-fgnq-exp
ON VALUE-CHANGED OF tb_open IN FRAME rd-fgnq-exp /* Open Invoices Only? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV rd-fgnq-exp
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME rd-fgnq-exp /* Open CSV? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-fgnq-exp 


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

end_chk:SCREEN-VALUE = "2147483647" .  /* Default value as given in AQ2 */
fi_file:SCREEN-VALUE = "c:\tmp\CustomerExport.csv". 

APPLY "entry" TO begin_cust.
END. 
WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tempfile rd-fgnq-exp 
PROCEDURE create-tempfile :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE xsum        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-open      AS LOG       FORMAT "Y/N" INIT "N".
    DEFINE VARIABLE t-check-no  AS CHARACTER FORMAT "x(8)".
    DEFINE VARIABLE x-check-no  AS CHARACTER FORMAT "x(10)".
    DEFINE VARIABLE t-credits   AS DECIMAL   FORMAT ">,>>>,>>>.99" COLUMN-LABEL "Credits".
    DEFINE VARIABLE t-debits    AS DECIMAL   FORMAT ">,>>>,>>>.99" COLUMN-LABEL "Debits".
    DEFINE VARIABLE t-balance   AS DECIMAL   FORMAT ">>,>>>,>>>.99" COLUMN-LABEL "Balance".
    DEFINE VARIABLE v-tot-due   AS DECIMAL   FORMAT "->,>>>,>>9.99".
    DEFINE VARIABLE v-pay-stat1 AS LOG       FORMAT "Y/N".
    DEFINE VARIABLE v-pay-stat2 AS CHARACTER FORMAT "x(4)".
    DEFINE VARIABLE li-seq      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-cust-no  LIKE ar-cashl.cust-no NO-UNDO.
    DEFINE VARIABLE ll-valid    AS LOG       NO-UNDO.
    DEFINE VARIABLE li-fchk     LIKE ar-cash.check-no NO-UNDO.
    DEFINE VARIABLE li-tchk     LIKE ar-cash.check-no NO-UNDO. 

    DEFINE VARIABLE num-day-old AS DECIMAL   FORMAT ">>>9" INIT 9999.
    DEFINE VARIABLE vp-custno   AS CHARACTER INIT "" NO-UNDO.
    DEFINE VARIABLE fi_fchk     AS INT64     FORMAT ">>>>>>>>>>>>":U INITIAL 0 NO-UNDO.
    DEFINE VARIABLE fi_tchk     AS INT64     FORMAT ">>>>>>>>>>>>":U INITIAL 2147483647 NO-UNDO .

    SESSION:SET-WAIT-STATE ("general").

    FOR EACH tt-arinq:
        DELETE tt-arinq.
    END.

    ASSIGN
        v-open  = tb_open
        li-seq  = 0
        xsum    = 0
        li-fchk = begin_chk
        li-tchk = begin_chk
        fi_fchk = li-fchk 
        fi_tchk = li-tchk    .

    IF NOT CAN-FIND(FIRST ar-cash
        WHERE ar-cash.company  EQ cocode
        AND ar-cash.memo     EQ YES
        AND ar-cash.check-no LT li-fchk) AND
        NOT CAN-FIND(FIRST ar-cash
        WHERE ar-cash.company  EQ cocode
        AND ar-cash.memo     EQ NO
        AND ar-cash.check-no LT li-fchk) THEN li-fchk = 0.

    IF NOT CAN-FIND(FIRST ar-cash
        WHERE ar-cash.company  EQ cocode
        AND ar-cash.memo     EQ YES
        AND ar-cash.check-no GT li-fchk) AND
        NOT CAN-FIND(FIRST ar-cash
        WHERE ar-cash.company  EQ cocode
        AND ar-cash.memo     EQ NO
        AND ar-cash.check-no GT li-fchk) THEN li-fchk = 2147483647.

    FOR EACH ar-inv NO-LOCK
        WHERE ar-inv.company  EQ cocode
        AND ar-inv.cust-no  GE begin_cust /*or fi_cust eq "")*/
        AND ar-inv.cust-no  LE end_cust 
        AND ar-inv.inv-no   GE INT(begin_inv)
        AND ar-inv.inv-no   LE INT(end_inv)
        AND ar-inv.posted   EQ YES
        AND ar-inv.terms    NE "CASH"
        AND ar-inv.inv-date GE (TODAY - num-day-old)      
        USE-INDEX ar-inv
        BY ar-inv.inv-date
        BY ar-inv.inv-no:
        FIND FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ ar-inv.cust-no 
            NO-LOCK NO-ERROR.
    
        IF v-format EQ "ASI" THEN 
        DO:
            {ar/ar-iactxl.i 1}
        END.    
        ELSE 
        DO:
            {ar/ar-iactxl.i 2}
        END.
    END. /* for each ar-inv record */
  
    /* display unapplied cr/db memos/payments */
    lv-cust-no = begin_cust.

    DO WHILE TRUE:
        FOR EACH ar-cashl NO-LOCK
            WHERE ar-cashl.company EQ cocode
            AND ar-cashl.posted  EQ YES
            AND ar-cashl.cust-no GE begin_cust
            AND ar-cashl.cust-no  LE end_cust /*or fi_cust eq "")*/
            AND ar-cashl.inv-no  EQ 0,      
            FIRST ar-cash NO-LOCK
            WHERE ar-cash.c-no     EQ ar-cashl.c-no
            AND ar-cash.check-no GE INT64(begin_chk)
            AND ar-cash.check-no LE end_chk
            BY ar-cash.check-date
            BY ar-cash.c-no:
       
            DO WITH FRAME {&frame-name}:
                FIND cust
                    WHERE cust.company EQ cocode
                    AND cust.cust-no EQ ar-cashl.cust-no
                    NO-LOCK NO-ERROR.
            END.
      
            IF v-format EQ "ASI" THEN 
            DO:
                {ar/ar-iact2.i 1}
            END.    
            ELSE 
            DO:
                {ar/ar-iact2.i 2}
            END.  
        END. /* for each ar-cash record */

        RELEASE ar-cashl.

        IF begin_cust EQ "" THEN
            FIND FIRST ar-cashl
                WHERE ar-cashl.company EQ cocode
                AND ar-cashl.posted  EQ YES
                AND ar-cashl.company GT lv-cust-no
                NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ar-cashl THEN LEAVE.

        lv-cust-no = ar-cashl.cust-no.
    END.
    /*APPLY 'VALUE-CHANGED':U TO FRAME {&FRAME-NAME}.*/

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd-fgnq-exp  _DEFAULT-DISABLE
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
    HIDE FRAME rd-fgnq-exp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd-fgnq-exp  _DEFAULT-ENABLE
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
    DISPLAY begin_cust end_cust begin_chk end_chk begin_inv end_inv tb_open 
        sl_avail sl_selected fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME rd-fgnq-exp.
    ENABLE RECT-6 RECT-7 RECT-8 begin_cust end_cust begin_chk end_chk begin_inv 
        end_inv tb_open sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up 
        btn_down fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
        WITH FRAME rd-fgnq-exp.
    VIEW FRAME rd-fgnq-exp.
    {&OPEN-BROWSERS-IN-QUERY-rd-fgnq-exp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-fgnq-exp 
PROCEDURE run-report :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-excelheader        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.
    DEFINE BUFFER b-cust FOR cust.
    DEFINE VARIABLE li-pallets AS INTEGER NO-UNDO.
    DEFINE VARIABLE op-qty-pal AS INTEGER NO-UNDO.

    v-excelheader = buildHeader().
    SESSION:SET-WAIT-STATE ("general").

    IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).
    IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

    v-excel-detail-lines = "".
    op-qty-pal = 0 .
    li-pallets = 0 .

    RUN create-tempfile.

    FOR EACH tt-arinq NO-LOCK
        BY tt-arinq.seq :

        v-excel-detail-lines = "".

        FOR EACH ttRptSelected:
            /* IF LOOKUP(ttRptSelected.TextList, "Contact,Title,Email") <> 0    THEN do: */
            IF ttRptSelected.TextList = "Ck/Cr/Dr/Po#" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(tt-arinq.ref-num)).
            IF ttRptSelected.TextList = "Invoice#" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(tt-arinq.inv-no)).
            IF ttRptSelected.TextList = "Date" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(tt-arinq.tr-date)).
            IF ttRptSelected.TextList = "Description" THEN 
            DO:
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(tt-arinq.tr-dscr)).
            END.
            IF ttRptSelected.TextList = "Age App" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(tt-arinq.ageapp)).
            IF ttRptSelected.TextList = "Debits" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(tt-arinq.tr-damt)).
            IF ttRptSelected.TextList = "Credits" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(tt-arinq.tr-camt)).
            IF ttRptSelected.TextList = "Balance" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(tt-arinq.balance)).
              
              
              
        /*  END.

          ELSE do:
              v-excel-detail-lines = v-excel-detail-lines + 
                  appendXLLine(getValue-itemfg(BUFFER b-cust,ttRptSelected.FieldList)).
          END.  */
        END.  /* each ttrptse */

        PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.

    END. /* for each phone */

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd-fgnq-exp 
PROCEDURE Set-Sort-Data :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:

    /* If a customer number was entered, find first and last matching customers. */
    /*   IF begin_i-no:SCREEN-VALUE EQ "" THEN DO:
           FIND FIRST fg-rcpth WHERE fg-rcpth.company EQ cocode NO-LOCK NO-ERROR.
           begin_i-no:SCREEN-VALUE = fg-rcpth.i-no.
           FIND LAST fg-rcpth WHERE fg-rcpth.company EQ cocode NO-LOCK NO-ERROR.
           end_i-no:SCREEN-VALUE   = fg-rcpth.i-no .
       END. */
    /*  IF begin_title-cod:SCREEN-VALUE EQ "" THEN DO:
          FIND FIRST phone WHERE phone.company EQ cocode NO-LOCK NO-ERROR.
          begin_title-cod:SCREEN-VALUE = phone.titlcode .
          FIND LAST phone WHERE phone.company EQ cocode NO-LOCK NO-ERROR.
          end_title-cod:SCREEN-VALUE   = phone.titlcode .
      END. */

    /*  IF lcSearch NE "" THEN 
          begin_cust-type:SCREEN-VALUE = lcSearch.
      IF lcsearchby NE "" THEN 
          end_cust-type:SCREEN-VALUE = lcsearchby.*/

    END.

    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-fgnq-exp 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Adds a value to a csv line
        Notes:  Protects agains commans and quotes.
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-line AS CHARACTER NO-UNDO.
    
    lc-line = quoter(ipc-append) + ",".
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-itemfg rd-fgnq-exp 
FUNCTION getValue-itemfg RETURNS CHARACTER
    ( BUFFER ipb-itemfg FOR cust, ipc-field AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
    {custom/getperd.i} 
    DEFINE VARIABLE h-field     AS HANDLE.
    DEFINE VARIABLE li-extent   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lc-return   AS CHARACTER FORMAT "x(100)" NO-UNDO.
    DEFINE VARIABLE ptd-profit1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ytd-profit1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lyr-profit1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ptd-sales1  AS DECIMAL   NO-UNDO.

    CASE ipc-field :
        WHEN "ptd-profit" THEN 
            DO:
                lc-return = STRING(ipb-itemfg.sales[gperiod] - ipb-itemfg.cost[1]) .
            END.
        
        OTHERWISE 
        DO:
            IF INDEX(ipc-field,"[") > 0 THEN 
            DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-itemfg:BUFFER-FIELD(ipc-field).
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

