&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
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

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
    cocode = gcompany
    locode = gloc.

def    var      v-process   as log    no-undo.
DEFINE VARIABLE hdCostProcs AS HANDLE.
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
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_i-no end_i-no fiStartDate ~
fiEndDate tb_FallBack tb_Receipts tb_0 tb_inactive tb_pro-only ~
tb_recalcCosts tb_excel tb_runExcel fi_file btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_i-no end_i-no fiStartDate fiEndDate ~
tb_FallBack tb_Receipts tb_0 tb_inactive tb_pro-only tb_recalcCosts ~
tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndDate AS DATE FORMAT "99/99/9999":U INITIAL 12/31/2099 
     LABEL "End Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiStartDate AS DATE FORMAT "99/99/9999":U INITIAL ? 
     LABEL "Start Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\fxtrncst.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 16.19.

DEFINE VARIABLE tb_0 AS LOGICAL INITIAL yes 
     LABEL "Fix Cost for ~"0~" in Cost and ~"?~" in Cost only" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_FallBack AS LOGICAL INITIAL yes 
     LABEL "Use FG Item Costs if PO, Job, or Receipt Not Found" 
     VIEW-AS TOGGLE-BOX
     SIZE 55 BY .81 NO-UNDO.

DEFINE VARIABLE tb_inactive AS LOGICAL INITIAL no 
     LABEL "Include Inactive Items" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.

DEFINE VARIABLE tb_pro-only AS LOGICAL INITIAL no 
     LABEL "Report on proposed changes only" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.

DEFINE VARIABLE tb_recalcCosts AS LOGICAL INITIAL yes 
     LABEL "Run Recalc Costs for each FG Item" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.

DEFINE VARIABLE tb_Receipts AS LOGICAL INITIAL no 
     LABEL "Include Receipts without Job or PO" 
     VIEW-AS TOGGLE-BOX
     SIZE 46 BY .81 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL yes 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_i-no AT ROW 5.95 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_i-no AT ROW 7.05 COL 28 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     fiStartDate AT ROW 8.24 COL 28 COLON-ALIGNED WIDGET-ID 18
     fiEndDate AT ROW 9.43 COL 28 COLON-ALIGNED WIDGET-ID 20
     tb_FallBack AT ROW 11.24 COL 29 WIDGET-ID 4
     tb_Receipts AT ROW 12.19 COL 29 WIDGET-ID 6
     tb_0 AT ROW 13.14 COL 29
     tb_inactive AT ROW 14.1 COL 29 WIDGET-ID 2
     tb_pro-only AT ROW 15.1 COL 29 WIDGET-ID 14
     tb_recalcCosts AT ROW 16.1 COL 29 WIDGET-ID 16
     tb_excel AT ROW 17.19 COL 41.6 WIDGET-ID 10
     tb_runExcel AT ROW 17.19 COL 83.6 RIGHT-ALIGNED WIDGET-ID 12
     fi_file AT ROW 18.1 COL 39.6 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 8
     btn-process AT ROW 21.24 COL 21
     btn-cancel AT ROW 21.24 COL 53
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 21.86.

DEFINE FRAME FRAME-B
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 3
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 79 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.81
         BGCOLOR 11 .


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
         TITLE              = "Fix FG Hist Cost"
         HEIGHT             = 21.86
         WIDTH              = 91.2
         MAX-HEIGHT         = 21.86
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 21.86
         VIRTUAL-WIDTH      = 98.2
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Fix FG Hist Cost */
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
ON WINDOW-CLOSE OF C-Win /* Fix FG Hist Cost */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
        apply "close" to this-procedure.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
        v-process  = NO.


        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.

        IF NOT tb_pro-only THEN 
        do:
            MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE) +
                " for the selected parameters?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.
        END.
        ELSE v-process = YES .

        IF v-process THEN RUN run-process.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
        assign {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
        assign {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
        assign {&self-name}.
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

    RUN system\CostProcs.p PERSISTENT SET hdCostProcs.
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    RUN enable_UI.

    {methods/nowait.i}
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
  DISPLAY begin_i-no end_i-no fiStartDate fiEndDate tb_FallBack tb_Receipts tb_0 
          tb_inactive tb_pro-only tb_recalcCosts tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_i-no end_i-no fiStartDate fiEndDate tb_FallBack 
         tb_Receipts tb_0 tb_inactive tb_pro-only tb_recalcCosts tb_excel 
         tb_runExcel fi_file btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateHistoryRecord C-Win 
PROCEDURE pUpdateHistoryRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriFGRdtlh AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipriFGRcpth AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUOM      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCost     AS DECIMAL   NO-UNDO EXTENT 6.
    DEFINE INPUT  PARAMETER ipcSource   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcProdUOM  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER bf-fg-rdtlh FOR fg-rdtlh.
    DEFINE BUFFER bf-fg-rctd  FOR fg-rctd.
    
    FIND bf-fg-rcpth EXCLUSIVE 
         WHERE ROWID(bf-fg-rcpth) EQ ipriFGRcpth
         NO-ERROR.
    FIND bf-fg-rdtlh EXCLUSIVE 
        WHERE ROWID(bf-fg-rdtlh) EQ ipriFGRdtlh
        NO-ERROR.
    
    IF NOT AVAILABLE bf-fg-rdtlh OR NOT AVAILABLE bf-fg-rcpth THEN
        RETURN.
            
    ASSIGN 
        bf-fg-rdtlh.spare-dec-1  = bf-fg-rdtlh.cost /*store old cost before NY12*/
        bf-fg-rdtlh.spare-char-2 = bf-fg-rcpth.pur-uom /*store old cost uom before NY12*/
        bf-fg-rdtlh.cost         = ipdCost[5]
        bf-fg-rcpth.pur-uom      = ipcUOM
        bf-fg-rdtlh.std-tot-cost = ipdCost[5]
        bf-fg-rdtlh.std-fix-cost = ipdCost[4]
        bf-fg-rdtlh.std-var-cost = ipdCost[3]
        bf-fg-rdtlh.std-mat-cost = ipdCost[2]
        bf-fg-rdtlh.std-lab-cost = ipdCost[1]
        bf-fg-rdtlh.spare-char-1 = ipcSource  /*Store cost source*/
        .

    FIND FIRST bf-fg-rctd EXCLUSIVE 
         WHERE bf-fg-rctd.r-no EQ bf-fg-rcpth.r-no 
         USE-INDEX fg-rctd NO-ERROR.
    IF AVAIL bf-fg-rctd THEN  
        ASSIGN 
            bf-fg-rctd.std-cost = ipdCost[5]
            bf-fg-rctd.cost-uom = ipcProdUOM
            bf-fg-rctd.ext-cost = bf-fg-rctd.std-cost * (bf-fg-rctd.t-qty / IF bf-fg-rctd.cost-uom EQ "M" THEN 1000 ELSE 1)
            .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DEF    VAR      lv-po-no       AS INT       NO-UNDO.
    DEF    VAR      lv-cost        LIKE fg-rdtlh.cost NO-UNDO EXTENT 6.
    DEF    VAR      lv-uom         LIKE fg-rcpth.pur-uom NO-UNDO.
    DEF    VAR      v-len          LIKE po-ordl.s-len NO-UNDO.
    DEF    VAR      v-wid          LIKE po-ordl.s-len NO-UNDO.
    DEF    VAR      v-dep          LIKE po-ordl.s-len NO-UNDO. 
    DEF    VAR      v-bwt          LIKE po-ordl.s-len NO-UNDO.
    DEF    VAR      li             AS INT       NO-UNDO.
    DEFINE VARIABLE iPOLine        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cSource        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSourceFound   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iCountItem     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCountTotal    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCountCompared AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE iCountChanged  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTime          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE excelheader    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCheckLockBin  AS LOGICAL   NO-UNDO.

    DEF    BUFFER b-fg-bin    FOR fg-bin.
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER bf-fg-rdtlh FOR fg-rdtlh.
    DEF BUFFER bf-fg-rctd FOR fg-rctd.
DISABLE TRIGGERS FOR LOAD OF fg-bin.    
    IF tb_excel THEN 
    do:
        OUTPUT STREAM excel TO VALUE(fi_file).

        excelheader = "FG Item,Rita Code,Job#,Job2,Po#,Cost,Cost Uom,Total Std Cost,Std Fix OH Cost,Std Var OH Cost,Std Mat'l Cost,Std Labor Cost,Group,Old Cost,Old Cost Uom".
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.


    SESSION:SET-WAIT-STATE("General").

    STATUS DEFAULT "Searching...".

    iTime = TIME.
    FOR EACH itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    GE begin_i-no
        AND itemfg.i-no    LE end_i-no
        AND (itemfg.stat EQ 'A' OR tb_inactive) :
  
        iCountItem = iCountItem + 1.
        STATUS DEFAULT "Processing FG Item#: " + TRIM(itemfg.i-no).

        FOR EACH fg-rcpth NO-LOCK 
            WHERE fg-rcpth.company EQ itemfg.company
            AND fg-rcpth.i-no    EQ itemfg.i-no
            AND (fg-rcpth.trans-date GE fiStartDate OR fiStartDate EQ ?)
            AND (fg-rcpth.trans-date LE fiEndDate   OR fiEndDate EQ ?)
            USE-INDEX i-no,
            EACH fg-rdtlh NO-LOCK 
            WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
            AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
            AND (fg-rdtlh.cost EQ 0                           OR
            fg-rdtlh.cost EQ ?                           OR
            NOT tb_0)
            USE-INDEX rm-rdtl

            BREAK BY INT(fg-rcpth.rita-code NE "R")
            BY INT(fg-rcpth.rita-code NE "C")
            BY fg-rcpth.trans-date
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no
            BY fg-rdtlh.rec_key:
            ASSIGN 
                iCountTotal = iCountTotal + 1
                lv-cost[1]  = 0
                lv-cost[2]  = 0
                lv-cost[3]  = 0
                lv-cost[4]  = 0
                lv-cost[5]  = 0
                lv-cost[6]  = 0
                .  
                
                FIND FIRST fg-bin NO-LOCK
                     WHERE fg-bin.company EQ cocode
                     AND fg-bin.i-no EQ fg-rcpth.i-no                     
                     AND fg-bin.tag EQ fg-rdtlh.tag 
                     AND fg-bin.qty GT 0
                     NO-ERROR  .
                     
                lCheckLockBin = IF AVAIL fg-bin AND fg-bin.ship-default THEN TRUE ELSE FALSE .     
      
            /*Only "fix" if the transaction has a job or po or if it is not a receipt*/
            /*In other words, if a receipt has no po or a job, leave it alone*/   
            IF NOT lCheckLockBin AND (fg-rcpth.rita-code NE "R" OR fg-rcpth.job-no NE "" OR fg-rcpth.po-no NE "" OR tb_Receipts) THEN 
            DO:   
                iCountCompared = iCountCompared + 1.
                RUN GetCostForFGItemHist IN hdCostProcs (fg-rcpth.company, fg-rcpth.i-no, fg-rcpth.job-no, fg-rcpth.job-no2, fg-rcpth.po-no, fg-rcpth.po-line, fg-rdtlh.tag, fg-rcpth.rita-code,
                    OUTPUT lv-cost[1], OUTPUT lv-cost[4], OUTPUT lv-cost[3], OUTPUT lv-cost[2], OUTPUT lv-cost[5], OUTPUT lv-cost[6], OUTPUT lv-uom, OUTPUT cSource, OUTPUT lSourceFound).
        
                RUN ClearTagsForGroup(
                    INPUT fg-rcpth.rec_key,
                    INPUT "Cost"
                    ).
                    
                RUN AddTagInfoForGroup(
                    INPUT fg-rcpth.rec_key,
                    INPUT "fg-rcpth",
                    INPUT cSource,
                    INPUT "",
                    INPUT "Cost"
                    ). /*From TagProcs Super Proc*/
                    
                IF lv-cost[5] EQ ? THEN lv-cost[5] = 0.
    
                IF (lv-cost[5] NE 0 OR fg-rcpth.po-no NE "") AND (lSourceFound OR tb_FallBack) THEN DO:
                    IF NOT tb_pro-only THEN DO: /*if cost was found from PO or Job, lSourceFound = YES, otherwise, fall back cost of IF1 cost*/
                        iCountChanged = iCountChanged + 1.
                        RUN pUpdateHistoryRecord(INPUT ROWID(fg-rdtlh),INPUT ROWID(fg-rcpth), lv-uom, lv-cost, cSource, itemfg.prod-uom).
                    END.
                    
                    IF tb_excel THEN DO:
                        EXPORT STREAM excel DELIMITER "," 
                            fg-rcpth.i-no
                            fg-rcpth.rita-code
                            fg-rcpth.job-no 
                            fg-rcpth.job-no2
                            fg-rcpth.po-no
                            lv-cost[5]
                            lv-uom
                            lv-cost[5]
                            lv-cost[4]
                            lv-cost[3]
                            lv-cost[2]
                            lv-cost[1] 
                            cSource 
                            ( IF tb_pro-only THEN fg-rdtlh.cost ELSE fg-rdtlh.spare-dec-1)
                            ( IF tb_pro-only THEN fg-rcpth.pur-uom ELSE fg-rdtlh.spare-char-2) .
                    END. /* Excel Report */
                END.
            END. /*History record eligible for processing*/
        END. /*Each History record*/
        STATUS DEFAULT "Recalculating cost for FG Item#: " + TRIM(itemfg.i-no).
        IF tb_RecalcCosts THEN 
            RUN fg/updfgcst.p (INPUT itemfg.i-no, INPUT YES).
    END. /*Each itemfg*/

    STATUS DEFAULT "".

    SESSION:SET-WAIT-STATE("").

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.

        IF tb_runExcel THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(fi_file)).
    END.

    IF NOT tb_pro-only THEN 
    do:
        MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." SKIP(2) 
            "Items Processed: " iCountItem SKIP 
            "History Records Processed: " iCountTotal SKIP 
            "History Records Qualified to Fix: " iCountCompared SKIP 
            "History Records Changed: " iCountChanged SKIP 
            "Total Time (seconds): " TIME - iTime
            VIEW-AS ALERT-BOX.
    
        APPLY "close" TO THIS-PROCEDURE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

