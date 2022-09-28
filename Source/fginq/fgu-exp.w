&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd_fgu-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd_fgu-exp 
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

{custom/gperiod.i}
{custom/persist.i}

{methods/defines/hndldefs.i}

DEFINE {&NEW} SHARED VARIABLE g_batch       AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE g_batch-rowid AS ROWID   NO-UNDO.
DEFINE               VARIABLE v-prgmname    LIKE prgrms.prgmname NO-UNDO.
{sys/inc/var.i new shared}

v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1).
v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

DEFINE VARIABLE v-bal AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 NO-UNDO.
DEFINE VARIABLE v-net AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 NO-UNDO.
DEFINE VARIABLE v-onh AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 NO-UNDO. 
DEFINE VARIABLE v-ono AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 NO-UNDO.
DEFINE VARIABLE v-opo AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 NO-UNDO.

DEFINE TEMP-TABLE tt-report LIKE report
    FIELD required AS INTEGER
    FIELD variance AS INTEGER.

ASSIGN
    cocode = g_company
    /*locode = gloc*/ .
{sa/sa-sls01.i}
DEFINE STREAM excel.


DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.

ASSIGN 
    cTextListToSelect  = "FG Item,Release Date,Required,Variance,Ordered Qty,On Hand Qty," +
                            "Net Qty,Bal to Ship,Open PO's,Release"
    cFieldListToSelect = "item,rel-date,req,var,order,qty-hand," +
                            "net-qty,bal-ship,open-po,release".
{sys/inc/ttRptSel.i}

ASSIGN 
    cTextListToDefault = "FG Item,Release Date,Required,Variance,Ordered Qty,On Hand Qty," +
                                "Net Qty,Bal to Ship,Open PO's,Release" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd_fgu-exp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_i-no begin_date ~
sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up btn_down fi_file ~
tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_i-no begin_date sl_avail sl_selected ~
fi_file tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd_fgu-exp 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd_fgu-exp 
FUNCTION assignParam RETURNS CHARACTER
    ( ipc-param AS CHARACTER , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd_fgu-exp 
FUNCTION buildHeader RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-itemfg rd_fgu-exp 
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

DEFINE VARIABLE begin_date AS DATE      FORMAT "99/99/9999" 
    LABEL "From Ship Date" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "x(15)" 
    LABEL "From FG Item" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1.

DEFINE VARIABLE fi_file    AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\ScheduledShip-vs-Qoh.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 52 BY 1.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 7.86.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 5.67.

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

DEFINE VARIABLE tb_OpenCSV  AS LOGICAL   INITIAL YES 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd_fgu-exp
    begin_i-no AT ROW 4.24 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Customer Number" WIDGET-ID 146
    begin_date AT ROW 4.24 COL 71 COLON-ALIGNED HELP
    "Enter Ending Customer" WIDGET-ID 148
    sl_avail AT ROW 8.71 COL 7 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 8.71 COL 62.6 NO-LABELS WIDGET-ID 28
    Btn_Def AT ROW 8.76 COL 42.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    Btn_Add AT ROW 9.91 COL 42.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 130
    Btn_Remove AT ROW 11.1 COL 42.2 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 134
    btn_Up AT ROW 12.29 COL 42.2 WIDGET-ID 136
    btn_down AT ROW 13.48 COL 42.2 WIDGET-ID 132
    fi_file AT ROW 16.38 COL 18.8 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 22
    tb_OpenCSV AT ROW 16.48 COL 87.6 RIGHT-ALIGNED WIDGET-ID 34
    tbAutoClose AT ROW 18.29 COL 42 WIDGET-ID 60
    tb_excel AT ROW 18.38 COL 4 WIDGET-ID 32
    btn-ok AT ROW 19.1 COL 32.6 WIDGET-ID 14
    btn-cancel AT ROW 19.1 COL 52.6 WIDGET-ID 12
    "Selected Columns" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 8 COL 69.4 WIDGET-ID 138
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5 WIDGET-ID 36
    " Export Selection" VIEW-AS TEXT
    SIZE 17 BY .62 AT ROW 7.33 COL 5 WIDGET-ID 86
    "Available Columns" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 8 COL 13.4 WIDGET-ID 140
    RECT-6 AT ROW 7.67 COL 4 WIDGET-ID 30
    RECT-7 AT ROW 1.52 COL 4 WIDGET-ID 38
    RECT-8 AT ROW 15.76 COL 4 WIDGET-ID 84
    SPACE(2.99) SKIP(2.85)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    BGCOLOR 15 
    TITLE "Export Sched vs Qoh to Excel" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX rd_fgu-exp
   FRAME-NAME                                                           */
ASSIGN 
    FRAME rd_fgu-exp:SCROLLABLE = FALSE
    FRAME rd_fgu-exp:HIDDEN     = TRUE.

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME rd_fgu-exp = "parm".

ASSIGN 
    begin_i-no:PRIVATE-DATA IN FRAME rd_fgu-exp = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME rd_fgu-exp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd_fgu-exp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_excel:HIDDEN IN FRAME rd_fgu-exp       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME rd_fgu-exp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME rd_fgu-exp
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME rd_fgu-exp = "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd_fgu-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_fgu-exp rd_fgu-exp
ON HELP OF FRAME rd_fgu-exp /* Export Sched vs Qoh to Excel */
    DO:
        DEFINE VARIABLE lw-focus   AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE ls-cur-val AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE char-val   AS CHARACTER     NO-UNDO.

        lw-focus = FOCUS.

        CASE lw-focus:NAME :

            WHEN "begin_cust-type" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-cust.w (cocode, ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* itemfg */
            WHEN "end_cust-type" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-cust.w (cocode, ls-cur-val, OUTPUT char-val).
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_fgu-exp rd_fgu-exp
ON WINDOW-CLOSE OF FRAME rd_fgu-exp /* Export Sched vs Qoh to Excel */
    DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date rd_fgu-exp
ON LEAVE OF begin_date IN FRAME rd_fgu-exp /* From Ship Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no rd_fgu-exp
ON LEAVE OF begin_i-no IN FRAME rd_fgu-exp /* From FG Item */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd_fgu-exp
ON CHOOSE OF btn-cancel IN FRAME rd_fgu-exp /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd_fgu-exp
ON CHOOSE OF btn-ok IN FRAME rd_fgu-exp /* OK */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
  
        ASSIGN 
            fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
        RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
        fi_file:SCREEN-VALUE =  cFileName.
  
        RUN GetSelectionList.

        ASSIGN
            v-bal = 0
            v-net = 0
            v-onh = 0
            v-ono = 0
            v-opo = 0 .
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
        END.
        ELSE DO:
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
        END.

        IF tbAutoClose:CHECKED THEN 
            APPLY "END-ERROR":U TO SELF.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd_fgu-exp
ON CHOOSE OF Btn_Add IN FRAME rd_fgu-exp /* Add >> */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd_fgu-exp
ON CHOOSE OF Btn_Def IN FRAME rd_fgu-exp /* Default */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        RUN DisplaySelectionDefault.  /* task 04041406 */ 
        RUN DisplaySelectionList2 .
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd_fgu-exp
ON CHOOSE OF btn_down IN FRAME rd_fgu-exp /* Move Down */
    DO:
        RUN Move-Field ("Down").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd_fgu-exp
ON CHOOSE OF Btn_Remove IN FRAME rd_fgu-exp /* << Remove */
    DO:
        APPLY "DEFAULT-ACTION" TO sl_selected  .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd_fgu-exp
ON CHOOSE OF btn_Up IN FRAME rd_fgu-exp /* Move Up */
    DO:
        RUN Move-Field ("Up").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd_fgu-exp
ON HELP OF fi_file IN FRAME rd_fgu-exp /* Name */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd_fgu-exp
ON LEAVE OF fi_file IN FRAME rd_fgu-exp /* Name */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd_fgu-exp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd_fgu-exp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd_fgu-exp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd_fgu-exp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd_fgu-exp
ON VALUE-CHANGED OF tb_excel IN FRAME rd_fgu-exp /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV rd_fgu-exp
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME rd_fgu-exp /* Open CSV? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd_fgu-exp 


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



APPLY "entry" TO begin_i-no.
fi_file:SCREEN-VALUE = "c:\tmp\ScheduledShip-vs-Qoh.csv".
    
END.
WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tempfile rd_fgu-exp 
PROCEDURE create-tempfile :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-rel     LIKE v-ono NO-UNDO.
    DEFINE VARIABLE v-var     LIKE v-ono NO-UNDO.
    DEFINE VARIABLE v-hld-qty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-loc     LIKE oe-boll.loc NO-UNDO.

    FOR EACH tt-report:
        DELETE tt-report.
    END.
  
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ trim(begin_i-no)
        NO-LOCK NO-ERROR.
    /*  fi_name = itemfg.i-name.*/

    v-onh = 0.
    IF AVAILABLE itemfg THEN
        FOR EACH fg-bin
            WHERE fg-bin.company EQ cocode
            AND fg-bin.i-no    EQ itemfg.i-no
            NO-LOCK:
            v-onh = v-onh + fg-bin.qty.
        END.
    v-net = v-onh.

    ASSIGN
        v-rel = 0
        v-ono = 0
        v-bal = 0.

    FOR EACH oe-ordl
        WHERE oe-ordl.company EQ cocode
        AND oe-ordl.i-no    EQ itemfg.i-no
        USE-INDEX item NO-LOCK,

        FIRST oe-ord OF oe-ordl
        WHERE oe-ord.stat     NE "D"
        AND oe-ord.stat     NE "C"
        AND oe-ord.stat     NE "Z"
        NO-LOCK,

        EACH oe-rel
        WHERE oe-rel.company  EQ cocode
        AND oe-rel.ord-no   EQ oe-ord.ord-no
        AND oe-rel.i-no     EQ oe-ordl.i-no
        AND oe-rel.line     EQ oe-ordl.line
        NO-LOCK

        BREAK BY oe-rel.rel-date:

        ASSIGN
            v-bal = v-bal + oe-rel.qty
            v-rel = v-rel + oe-rel.qty
            v-ono = v-ono + oe-rel.qty
            v-net = v-net - oe-rel.qty.

        RELEASE oe-bolh.

        IF oe-rel.link-no NE 0 THEN
            FOR EACH oe-rell
                WHERE oe-rell.company EQ oe-rel.company
                AND oe-rell.ord-no  EQ oe-rel.ord-no
                AND oe-rell.r-no    EQ oe-rel.link-no
                AND oe-rell.i-no    EQ oe-rel.i-no
                AND oe-rell.line    EQ oe-rel.line
                NO-LOCK,
                FIRST oe-relh
                WHERE oe-relh.r-no   EQ oe-rell.r-no
                AND oe-relh.posted EQ YES
                NO-LOCK,
                EACH oe-boll
                WHERE oe-boll.company  EQ oe-rell.company
                AND oe-boll.ord-no   EQ oe-rell.ord-no
                AND oe-boll.line     EQ oe-rell.line
                AND oe-boll.i-no     EQ oe-rell.i-no
                AND oe-boll.r-no     EQ oe-rell.r-no
                AND oe-boll.rel-no   EQ oe-rell.rel-no
                AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
                NO-LOCK,
                FIRST oe-bolh
                WHERE oe-bolh.b-no   EQ oe-boll.b-no
                AND oe-bolh.posted EQ YES
                NO-LOCK:
                LEAVE.
            END.

        IF AVAILABLE oe-bolh THEN
            ASSIGN
                v-bal = v-bal - oe-rell.qty
                v-rel = v-rel - oe-rell.qty
                v-net = v-net + oe-rell.qty.
  
        IF LAST-OF(oe-rel.rel-date)  AND
            oe-rel.rel-date GE begin_date AND
            v-rel GT 0               THEN 
        DO:
            CREATE tt-report.
            ASSIGN
                tt-report.term-id  = v-term
                tt-report.key-01   = STRING(YEAR(oe-rel.rel-date),"9999") +
                            string(MONTH(oe-rel.rel-date),"99")  +
                            string(DAY(oe-rel.rel-date),"99")
                tt-report.key-02   = STRING(MONTH(oe-rel.rel-date),"99")  + "/" +
                            string(DAY(oe-rel.rel-date),"99")    + "/" +
                            string(YEAR(oe-rel.rel-date),"9999")
                tt-report.key-03   = STRING(v-rel,"-9999999999")
                tt-report.key-04   = STRING(v-net,"-9999999999")
                tt-report.required = v-rel
                tt-report.variance = v-net
                tt-report.key-05   = STRING(oe-rel.i-no)
                tt-report.key-06   = IF AVAILABLE oe-relh THEN STRING(oe-relh.release#) ELSE "0" .
            v-rel = 0.
      
        END.
    END.

    v-opo = 0.
    FOR EACH po-ordl
        WHERE po-ordl.company   EQ itemfg.company
        AND po-ordl.i-no      EQ itemfg.i-no
        AND po-ordl.item-type EQ NO
        AND lookup(po-ordl.stat,"O,P,U") GT 0
        AND po-ordl.t-rec-qty LT po-ordl.cons-qty
        NO-LOCK,

        FIRST po-ord WHERE
        po-ord.company EQ po-ordl.company AND
        po-ord.po-no   EQ po-ordl.po-no AND
        lookup(po-ord.stat,"N,O,R,U") GT 0
        AND po-ord.po-date                LE begin_date
        NO-LOCK:

        IF po-ordl.cons-uom EQ "EA" THEN
            v-hld-qty = po-ordl.cons-qty.
        ELSE
            RUN sys/ref/convquom.p(po-ordl.cons-uom, "EA", 0, 0, 0, 0,
                po-ordl.cons-qty, OUTPUT v-hld-qty).

        IF v-hld-qty - po-ordl.t-rec-qty GT 0 THEN
            v-opo = v-opo + (v-hld-qty - po-ordl.t-rec-qty).
    END.

    v-net = v-net + v-opo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd_fgu-exp  _DEFAULT-DISABLE
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
    HIDE FRAME rd_fgu-exp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd_fgu-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd_fgu-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd_fgu-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd_fgu-exp  _DEFAULT-ENABLE
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
    DISPLAY begin_i-no begin_date sl_avail sl_selected fi_file tb_OpenCSV 
        tbAutoClose 
        WITH FRAME rd_fgu-exp.
    ENABLE RECT-6 RECT-7 RECT-8 begin_i-no begin_date sl_avail sl_selected 
        Btn_Def Btn_Add Btn_Remove btn_Up btn_down fi_file tb_OpenCSV 
        tbAutoClose btn-ok btn-cancel 
        WITH FRAME rd_fgu-exp.
    VIEW FRAME rd_fgu-exp.
    {&OPEN-BROWSERS-IN-QUERY-rd_fgu-exp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd_fgu-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd_fgu-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd_fgu-exp 
PROCEDURE run-report :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-excelheader        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.
    DEFINE BUFFER b-cust FOR cust.

    v-excelheader = buildHeader().
    SESSION:SET-WAIT-STATE ("general").

    IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).
    IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

    RUN create-tempfile .

    v-excel-detail-lines = "".

    FOR EACH tt-report  NO-LOCK:
        v-excel-detail-lines = "".
        FOR EACH ttRptSelected:
            /* IF LOOKUP(ttRptSelected.TextList, "Contact,Title,Email") <> 0    THEN do: */
            IF ttRptSelected.FieldList = "item" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(tt-report.key-05).
            IF ttRptSelected.FieldList = "rel-date" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(tt-report.key-02).
            IF ttRptSelected.FieldList = "req" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(tt-report.required)).
            IF ttRptSelected.FieldList = "var" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(tt-report.variance)).

            IF ttRptSelected.FieldList = "order" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(v-ono)).
            IF ttRptSelected.FieldList = "qty-hand" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(v-onh)).
            IF ttRptSelected.FieldList = "net-qty" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(v-net)).
            IF ttRptSelected.FieldList = "bal-ship" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(v-bal)).
            IF ttRptSelected.FieldList = "open-po" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(v-opo)).
            IF ttRptSelected.FieldList = "release" THEN
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(tt-report.key-06)).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd_fgu-exp 
PROCEDURE Set-Sort-Data :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF lcSearch <> "" THEN
            ASSIGN
                begin_i-no:SCREEN-VALUE = lcSearch .
        IF lcsearchby <> "" OR lcsearchby NE ? THEN
            begin_date:SCREEN-VALUE = STRING(lcsearchby) .
    END.

    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd_fgu-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd_fgu-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd_fgu-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-itemfg rd_fgu-exp 
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

