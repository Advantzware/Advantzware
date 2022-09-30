&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd-fgexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd-fgexp 
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
DEFINE INPUT PARAMETER lcAccForm AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER lcAccTo   AS CHARACTER NO-UNDO.


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

DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.

ASSIGN 
    cTextListToSelect  = "Account No,Acc Description,Current Year Open Balance,Last Year Open Balance,Type,Current YEAR(1),Current YEAR(2),Current YEAR(3),Current YEAR(4),Current YEAR(5),Current YEAR(6)," +
                            "Current YEAR(7),Current YEAR(8),Current YEAR(9),Current YEAR(10),Current YEAR(11),Current YEAR(12),Current YEAR(13),LAST YEAR(1),LAST YEAR(2),LAST YEAR(3),LAST YEAR(4)," +
                            "LAST YEAR(5),LAST YEAR(6),LAST YEAR(7),LAST YEAR(8),LAST YEAR(9),LAST YEAR(10),LAST YEAR(11),LAST YEAR(12),LAST YEAR(13),CURRENT YEAR Budget(1),CURRENT YEAR Budget(2),CURRENT YEAR Budget(3)," +
                            "CURRENT YEAR Budget(4),CURRENT YEAR Budget(5),CURRENT YEAR Budget(6),CURRENT YEAR Budget(7),CURRENT YEAR Budget(8),CURRENT YEAR Budget(9),CURRENT YEAR Budget(10),CURRENT YEAR Budget(11),CURRENT YEAR Budget(12)," +
                            "CURRENT YEAR Budget(13),LAST YEAR Budget(1),LAST YEAR Budget(1),LAST YEAR Budget(1),LAST YEAR Budget(1),LAST YEAR Budget(1),LAST YEAR Budget(1),LAST YEAR Budget(1),LAST YEAR Budget(1),LAST YEAR Budget(1),LAST YEAR Budget(1)," +
                            "LAST YEAR Budget(1),LAST YEAR Budget(1),LAST YEAR Budget(1),Terms Discount,Inactive,Sales,Commission"

    cFieldListToSelect = "actnum,dscr,cyr-open,lyr-open,vTYPE,cyr[1],cyr[2]," +
                        "cyr[3],cyr[4],cyr[5],cyr[6],cyr[7],cyr[8],cyr[9],cyr[10]," +
                        "cyr[11],cyr[12],cyr[13],lyr[1],lyr[2],lyr[3],lyr[4],lyr[5]," +
                        "lyr[6],lyr[7],lyr[8],lyr[9],lyr[10],lyr[11],lyr[12],lyr[13],bud[1],"  +
                        "bud[2],bud[3],bud[4],bud[5],bud[6],bud[7],bud[8],bud[9],bud[10],bud[11]," +
                        "bud[12],bud[13],ly-bud[1],ly-bud[2],ly-bud[3],ly-bud[4],ly-bud[5],ly-bud[6],ly-bud[7],ly-bud[8]," +
                        "ly-bud[9],ly-bud[10],ly-bud[11],ly-bud[12],ly-bud[13],term-disc,inactive,salesReport,commReport"  .
{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Account No,Acc Description,Type,Inactive,Current Year Open Balance,Last Year Open Balance,Sales,Commission"  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-fgexp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_acc end_acc ~
begin_type end_type sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up ~
btn_down fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_acc end_acc begin_type end_type ~
sl_avail sl_selected fi_file tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-fgexp 
FUNCTION appendXLLine RETURNS CHARACTER
    ( ipc-append AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-fgexp 
FUNCTION assignParam RETURNS CHARACTER
    ( ipc-param AS CHARACTER , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-fgexp 
FUNCTION buildHeader RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-account rd-fgexp 
FUNCTION getValue-account RETURNS CHARACTER
    ( BUFFER ipbf-account FOR account, ipc-field AS CHARACTER )  FORWARD.

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

DEFINE VARIABLE begin_acc  AS CHARACTER FORMAT "x(15)" 
    LABEL "From G/L Account" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1.

DEFINE VARIABLE begin_type AS CHARACTER FORMAT "x(1)" 
    LABEL "From G/L Type" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1.

DEFINE VARIABLE end_acc    AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzzzzzz" 
    LABEL "To G/L Account" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE end_type   AS CHARACTER FORMAT "X(1)" INITIAL "z" 
    LABEL "To G/L Type" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE fi_file    AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\AccountExport.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 7.67.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 6.33.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 2.48.

DEFINE VARIABLE sl_avail    AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 6.14 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 6.14 NO-UNDO.

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
    SIZE 15.2 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-fgexp
    begin_acc AT ROW 3.67 COL 23.6 COLON-ALIGNED HELP
    "Enter Beginning Account Number" WIDGET-ID 142
    end_acc AT ROW 3.67 COL 66.6 COLON-ALIGNED HELP
    "Enter Ending Account #" WIDGET-ID 144
    begin_type AT ROW 4.67 COL 23.6 COLON-ALIGNED HELP
    "Enter Beginning Type Number" WIDGET-ID 142
    end_type AT ROW 4.67 COL 66.6 COLON-ALIGNED HELP
    "Enter Ending Type #" WIDGET-ID 144
    sl_avail AT ROW 9.43 COL 4.4 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 9.43 COL 40.4 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 9.43 COL 59.4 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 10.62 COL 40.4 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 130
    Btn_Remove AT ROW 11.81 COL 40.4 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 134
    btn_Up AT ROW 13 COL 40.4 WIDGET-ID 136
    btn_down AT ROW 14.19 COL 40.4 WIDGET-ID 132
    tb_excel AT ROW 16.76 COL 36 WIDGET-ID 32
    fi_file AT ROW 17.24 COL 18.4 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 22
    tb_OpenCSV AT ROW 17.33 COL 84 RIGHT-ALIGNED WIDGET-ID 34
    tbAutoClose AT ROW 19.29 COL 29 WIDGET-ID 64
    btn-ok AT ROW 20.33 COL 28.8 WIDGET-ID 14
    btn-cancel AT ROW 20.33 COL 50.4 WIDGET-ID 12
    "Selected Columns" VIEW-AS TEXT
    SIZE 21.6 BY .62 AT ROW 8.71 COL 59.8 WIDGET-ID 138
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.1 COL 4 WIDGET-ID 36
    " Export Selection" VIEW-AS TEXT
    SIZE 17 BY .62 AT ROW 7.91 COL 4 WIDGET-ID 86
    "Available Columns" VIEW-AS TEXT
    SIZE 20.6 BY .62 AT ROW 8.71 COL 4.8 WIDGET-ID 140
    RECT-6 AT ROW 8.33 COL 3 WIDGET-ID 30
    RECT-7 AT ROW 1.57 COL 3 WIDGET-ID 38
    RECT-8 AT ROW 16.48 COL 3 WIDGET-ID 84
    SPACE(1.99) SKIP(2.98)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    BGCOLOR 15 
    TITLE "Export Account to Excel" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX rd-fgexp
   FRAME-NAME                                                           */
ASSIGN 
    FRAME rd-fgexp:SCROLLABLE = FALSE
    FRAME rd-fgexp:HIDDEN     = TRUE.

ASSIGN 
    begin_acc:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

ASSIGN 
    begin_type:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

ASSIGN 
    end_acc:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

ASSIGN 
    end_type:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-fgexp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_excel:HIDDEN IN FRAME rd-fgexp       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME rd-fgexp
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME rd-fgexp = "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-fgexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgexp rd-fgexp
ON HELP OF FRAME rd-fgexp /* Export Account to Excel */
    DO:
        DEFINE VARIABLE lw-focus   AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE ls-cur-val AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE char-val   AS CHARACTER     NO-UNDO.

        lw-focus = FOCUS.

        CASE lw-focus:NAME :

            WHEN "begin_acc" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-acct.w (cocode,"", ls-cur-val, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* account */
            WHEN "end_acc" THEN 
                DO:
                    ls-cur-val = lw-focus:SCREEN-VALUE.
                    RUN windows/l-acct.w (cocode,"", "", OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO:
                        lw-focus:SCREEN-VALUE =  ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.
                END.  /* account */
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgexp rd-fgexp
ON WINDOW-CLOSE OF FRAME rd-fgexp /* Export Account to Excel */
    DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_acc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_acc rd-fgexp
ON LEAVE OF begin_acc IN FRAME rd-fgexp /* From G/L Account */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_type rd-fgexp
ON LEAVE OF begin_type IN FRAME rd-fgexp /* From G/L Type */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-fgexp
ON CHOOSE OF btn-cancel IN FRAME rd-fgexp /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd-fgexp
ON CHOOSE OF btn-ok IN FRAME rd-fgexp /* OK */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-fgexp
ON CHOOSE OF Btn_Add IN FRAME rd-fgexp /* Add >> */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-fgexp
ON CHOOSE OF Btn_Def IN FRAME rd-fgexp /* Default */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

        RUN DisplaySelectionDefault.  /* task 04041406 */ 
        RUN DisplaySelectionList2 .
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd-fgexp
ON CHOOSE OF btn_down IN FRAME rd-fgexp /* Move Down */
    DO:
        RUN Move-Field ("Down").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd-fgexp
ON CHOOSE OF Btn_Remove IN FRAME rd-fgexp /* << Remove */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd-fgexp
ON CHOOSE OF btn_Up IN FRAME rd-fgexp /* Move Up */
    DO:
        RUN Move-Field ("Up").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_acc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_acc rd-fgexp
ON LEAVE OF end_acc IN FRAME rd-fgexp /* To G/L Account */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_type rd-fgexp
ON LEAVE OF end_type IN FRAME rd-fgexp /* To G/L Type */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgexp
ON HELP OF fi_file IN FRAME rd-fgexp /* Name */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgexp
ON LEAVE OF fi_file IN FRAME rd-fgexp /* Name */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd-fgexp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd-fgexp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-fgexp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-fgexp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-fgexp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-fgexp /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV rd-fgexp
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME rd-fgexp /* Open CSV? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-fgexp 


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

        APPLY "entry" TO begin_acc.
        fi_file:SCREEN-VALUE = "c:\tmp\AccountExport.csv".
    END.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd-fgexp  _DEFAULT-DISABLE
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
    HIDE FRAME rd-fgexp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd-fgexp 
PROCEDURE DisplaySelectionDefault :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.
  
    DO iCount = 1 TO NUM-ENTRIES(cTextListToDefault):

        cListContents = cListContents +                   
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToDefault)   .
    END.            
    sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd-fgexp  _DEFAULT-ENABLE
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
    DISPLAY begin_acc end_acc begin_type end_type sl_avail sl_selected fi_file 
        tb_OpenCSV tbAutoClose 
        WITH FRAME rd-fgexp.
    ENABLE RECT-6 RECT-7 RECT-8 begin_acc end_acc begin_type end_type sl_avail 
        Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down fi_file 
        tb_OpenCSV tbAutoClose btn-ok btn-cancel 
        WITH FRAME rd-fgexp.
    VIEW FRAME rd-fgexp.
    {&OPEN-BROWSERS-IN-QUERY-rd-fgexp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-fgexp 
PROCEDURE run-report :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-excelheader        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-excel-detail-lines AS CHARACTER NO-UNDO.
    DEFINE VARIABLE d-period$            AS DECIMAL   NO-UNDO EXTENT 20.
    DEFINE VARIABLE cVarValue            AS CHARACTER NO-UNDO .
    DEFINE BUFFER b-account FOR account.




    v-excelheader = buildHeader().
    SESSION:SET-WAIT-STATE ("general").

    IF tb_excel THEN OUTPUT STREAM excel TO VALUE(cFileName).
    IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

    FOR EACH b-account WHERE b-account.company = cocode
        AND b-account.actnum GE begin_acc
        AND b-account.actnum LE end_acc
        AND b-account.TYPE GE begin_type
        AND b-account.TYPE LE end_type
        NO-LOCK:

        DO li = 1 TO 13:
            d-period$[li] = b-account.cyr[li].
        END.

        FOR EACH period
            WHERE period.company EQ b-account.company
            AND period.pstat   EQ YES
            NO-LOCK,

            EACH glhist
            WHERE glhist.company EQ b-account.company
            AND glhist.actnum  EQ b-account.actnum
            AND glhist.period  EQ period.pnum
            AND glhist.tr-date GE period.pst
            AND glhist.tr-date LE period.pend          
            NO-LOCK:

            d-period$[period.pnum] = d-period$[period.pnum] + glhist.tr-amt.
        END.


        v-excel-detail-lines = "".

        FOR EACH ttRptSelected:

            IF LOOKUP(ttRptSelected.FieldList, "cyr[1],cyr[2],cyr[3],cyr[4],cyr[5],cyr[6],cyr[7],cyr[8],cyr[9],cyr[10],cyr[11],cyr[12],cyr[13]") <> 0 THEN 
            DO:
                CASE ttRptSelected.FieldList:             
                    WHEN "cyr[1]"    THEN 
                        cVarValue = STRING(d-period$[1]) .
                    WHEN "cyr[2]"    THEN 
                        cVarValue = STRING(d-period$[2]).
                    WHEN "cyr[3]"    THEN 
                        cVarValue = STRING(d-period$[3]).
                    WHEN "cyr[4]"    THEN 
                        cVarValue = STRING(d-period$[4]) .
                    WHEN "cyr[5]"    THEN 
                        cVarValue = STRING(d-period$[5]) .
                    WHEN "cyr[6]"    THEN 
                        cVarValue = STRING(d-period$[6]) .
                    WHEN "cyr[7]"    THEN 
                        cVarValue = STRING(d-period$[7]) .
                    WHEN "cyr[8]"    THEN 
                        cVarValue = STRING(d-period$[8]) .
                    WHEN "cyr[9]"    THEN 
                        cVarValue = STRING(d-period$[9]) .
                    WHEN "cyr[10]"   THEN 
                        cVarValue = STRING(d-period$[10]) .
                    WHEN "cyr[11]"   THEN 
                        cVarValue = STRING(d-period$[11]) .
                    WHEN "cyr[12]"   THEN 
                        cVarValue = STRING(d-period$[12]) .
                    WHEN "cyr[13]"   THEN 
                        cVarValue = STRING(d-period$[13]) .

                END CASE.
                    
                v-excel-detail-lines = v-excel-detail-lines + appendXLLine(cVarValue).
            END.
            ELSE 
            DO:
                v-excel-detail-lines = v-excel-detail-lines + 
                    appendXLLine(getValue-account(BUFFER b-account,ttRptSelected.FieldList)).
            END.


        END.

        PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.
    END.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd-fgexp 
PROCEDURE Set-Sort-Data :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:

        /* If a customer number was entered, find first and last matching customers. */
        IF lcAccForm <> "" THEN
            ASSIGN 
                begin_acc:SCREEN-VALUE = assignParam(lcAccForm,NO)
                end_acc:SCREEN-VALUE   = assignParam(lcAccTo,NO)
                .

    END.

    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd-fgexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-account rd-fgexp 
FUNCTION getValue-account RETURNS CHARACTER
    ( BUFFER ipbf-account FOR account, ipc-field AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  Take a buffer and field name as string and return the value
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE h-field   AS HANDLE.
    DEFINE VARIABLE li-extent AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lc-return AS CHARACTER FORMAT "x(100)" NO-UNDO.

    CASE ipc-field :
        WHEN "vTYPE"  THEN 
            DO:
                IF ipbf-account.TYPE EQ "A" THEN 
                    lc-return = "Asset".
                ELSE IF ipbf-account.TYPE EQ "C" THEN 
                        lc-return = "Capital".
                    ELSE IF ipbf-account.TYPE EQ "E" THEN 
                            lc-return = "Expense".
                        ELSE IF ipbf-account.TYPE EQ "L" THEN 
                                lc-return = "Liability".
                            ELSE IF ipbf-account.TYPE EQ "R" THEN 
                                    lc-return = "Revenue".
                                ELSE
                                    lc-return = "Title".
            END.
        WHEN "term-disc"  THEN 
            DO:
                IF ipbf-account.terms-discount EQ YES THEN
                    lc-return = "Yes".
                ELSE
                    lc-return = "No".
            END.
        WHEN "dfuncTotMSFPTD"  THEN 
            DO:
            /*IF g_period NE 0 THEN lc-return = STRING(ipbf-account.ptd-msf[g_period]).*/
            END.
        OTHERWISE 
        DO:
            IF INDEX(ipc-field,"[") > 0 THEN 
            DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipbf-account:BUFFER-FIELD(ipc-field).
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

