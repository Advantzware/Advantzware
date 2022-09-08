&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: rmrep\r-tonmsf.w

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

DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.
DEFINE STREAM s-excel.

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
DEFINE BUFFER b-itemfg FOR itemfg .

ASSIGN 
    cTextListToSelect  = "TRANS TYPE,ITEM,DESCRIPTION,DATE,P.O.#,VENDOR#,JOB#," +
                           "QUANTITY,MSF,TONS,COST,VALUE" 
    cFieldListToSelect = "trn-typ,i-no,dscr,dat,po,vend,job," +
                            "qty,msf,ton,cst,val"
    cFieldLength       = "10,10,15,10,9,8,13," + "12,13,13,10,17" 
    cFieldType         = "c,c,c,c,c,c,c," + "i,i,i,i,i"  
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "TRANS TYPE,ITEM,DESCRIPTION,DATE,P.O.#,VENDOR#,JOB#," +
                           "QUANTITY,MSF,TONS,COST,VALUE" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_rm-no end_rm-no ~
begin_procat end_procat begin_date end_date begin_vend end_vend tb_receipts ~
tb_adjustments tb_issues tb_counts tb_transfers sl_avail Btn_Def ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_rm-no end_rm-no begin_procat ~
end_procat begin_date end_date begin_vend end_vend tb_receipts ~
tb_adjustments tb_issues tb_counts tb_transfers sl_avail sl_selected ~
rd-dest fi_file tb_OpenCSV tbAutoClose 

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
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat   AS CHARACTER FORMAT "X(5)":U 
    LABEL "Beginning Category" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no    AS CHARACTER FORMAT "X(10)":U 
    LABEL "Beginning Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Vendor" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat     AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
    LABEL "Ending Category" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no      AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
    LABEL "Ending Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend       AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Vendor" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-tonmsf.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 43 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 1 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 17.2 BY 4.38 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.95.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 9.

DEFINE VARIABLE sl_avail       AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected    AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tbAutoClose    AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_adjustments AS LOGICAL   INITIAL YES 
    LABEL "Adjustments?" 
    VIEW-AS TOGGLE-BOX
    SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE tb_counts      AS LOGICAL   INITIAL YES 
    LABEL "Cycle Counts?" 
    VIEW-AS TOGGLE-BOX
    SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE tb_issues      AS LOGICAL   INITIAL YES 
    LABEL "Issues?" 
    VIEW-AS TOGGLE-BOX
    SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE tb_receipts    AS LOGICAL   INITIAL YES 
    LABEL "Receipts?" 
    VIEW-AS TOGGLE-BOX
    SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV     AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_transfers   AS LOGICAL   INITIAL YES 
    LABEL "Transfers?" 
    VIEW-AS TOGGLE-BOX
    SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm   AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_rm-no AT ROW 2.19 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Item Number"
    end_rm-no AT ROW 2.19 COL 69 COLON-ALIGNED HELP
    "Enter Ending Item number"
    begin_procat AT ROW 3.14 COL 28 COLON-ALIGNED HELP
    "Enter Begining Category"
    end_procat AT ROW 3.14 COL 69 COLON-ALIGNED HELP
    "Enter Ending Category"
    begin_date AT ROW 4.1 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 4.1 COL 69 COLON-ALIGNED HELP
    "Enter ending Date"
    begin_vend AT ROW 5.05 COL 28 COLON-ALIGNED HELP
    "Enter Beginng Vendor"
    end_vend AT ROW 5.05 COL 69 COLON-ALIGNED HELP
    "Enter Endng Vendor"
    tb_receipts AT ROW 7.43 COL 29.6
    tb_adjustments AT ROW 7.43 COL 52.4
    tb_issues AT ROW 8.38 COL 29.6
    tb_counts AT ROW 8.38 COL 52.4
    tb_transfers AT ROW 9.33 COL 29.6
    sl_avail AT ROW 11.48 COL 4 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 11.48 COL 40.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 11.48 COL 60.6 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 12.48 COL 40.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 13.48 COL 40.8 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 14.52 COL 40.8 WIDGET-ID 40
    btn_down AT ROW 15.57 COL 40.8 WIDGET-ID 42
    lv-ornt AT ROW 17.67 COL 27 NO-LABELS
    lines-per-page AT ROW 17.67 COL 80 COLON-ALIGNED
    rd-dest AT ROW 18.29 COL 5.8 NO-LABELS
    td-show-parm AT ROW 18.71 COL 45.4
    lv-font-no AT ROW 18.76 COL 31.2 COLON-ALIGNED
    lv-font-name AT ROW 19.71 COL 25.2 COLON-ALIGNED NO-LABELS
    fi_file AT ROW 21.71 COL 25 COLON-ALIGNED HELP
    "Enter File Name" WIDGET-ID 6
    tb_OpenCSV AT ROW 21.81 COL 71 WIDGET-ID 4
    tbAutoClose AT ROW 23.62 COL 27 WIDGET-ID 58
    btn-ok AT ROW 24.81 COL 27
    btn-cancel AT ROW 24.81 COL 46
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 10.71 COL 4.4 WIDGET-ID 38
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 10.71 COL 60.2 WIDGET-ID 44
    "Transaction Types" VIEW-AS TEXT
    SIZE 20 BY 1.19 AT ROW 6.29 COL 12
    FGCOLOR 9 
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 17.1 COL 5
    RECT-6 AT ROW 17.43 COL 4
    RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 25.81
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
        TITLE              = "RM Transaction by TON/MSF"
        HEIGHT             = 25.81
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
    begin_procat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_procat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_rm-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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

ASSIGN 
    tb_adjustments:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_counts:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_issues:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_receipts:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_transfers:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* RM Transaction by TON/MSF */
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
ON WINDOW-CLOSE OF C-Win /* RM Transaction by TON/MSF */
    DO:
        /* This event will close the window and terminate the procedure.  */
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


&Scoped-define SELF-NAME begin_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_procat C-Win
ON LEAVE OF begin_procat IN FRAME FRAME-A /* Beginning Category */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no C-Win
ON LEAVE OF begin_rm-no IN FRAME FRAME-A /* Beginning Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor */
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
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.

        IF rd-dest EQ 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.
  
        SESSION:SET-WAIT-STATE("general").
        RUN GetSelectionList.
        RUN run-report. 

        STATUS DEFAULT "Processing Complete". 
        SESSION:SET-WAIT-STATE("").

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
                            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                        END.
                    END.
                END. /* WHEN 3 THEN DO: */
            WHEN 4 THEN 
                DO:
           /*run output-to-fax.*/
                    {custom/asifax.i &type= "Vendor "
                            &begin_cust= "begin_vend"
                            &END_cust= "begin_vend" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = "Vendor "
                             &begin_cust= "begin_vend"
                             &END_cust= "begin_vend"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Vendor "
                                  &begin_cust="begin_vend"
                                  &END_cust="begin_vend"
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

                    END.
                END. 
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


&Scoped-define SELF-NAME end_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_procat C-Win
ON LEAVE OF end_procat IN FRAME FRAME-A /* Ending Category */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-no C-Win
ON LEAVE OF end_rm-no IN FRAME FRAME-A /* Ending Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
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


&Scoped-define SELF-NAME tb_adjustments
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_adjustments C-Win
ON VALUE-CHANGED OF tb_adjustments IN FRAME FRAME-A /* Adjustments? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_counts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_counts C-Win
ON VALUE-CHANGED OF tb_counts IN FRAME FRAME-A /* Cycle Counts? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_issues
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_issues C-Win
ON VALUE-CHANGED OF tb_issues IN FRAME FRAME-A /* Issues? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_receipts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_receipts C-Win
ON VALUE-CHANGED OF tb_receipts IN FRAME FRAME-A /* Receipts? */
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


&Scoped-define SELF-NAME tb_transfers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_transfers C-Win
ON VALUE-CHANGED OF tb_transfers IN FRAME FRAME-A /* Transfers? */
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
DEFINE VARIABLE v-mat-list AS CHARACTER NO-UNDO.
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

    end_date = TODAY.

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
    {sys/inc/reportsConfigNK1.i "MR!" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_rm-no.
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
    DISPLAY begin_rm-no end_rm-no begin_procat end_procat begin_date end_date 
        begin_vend end_vend tb_receipts tb_adjustments tb_issues tb_counts 
        tb_transfers sl_avail sl_selected rd-dest fi_file tb_OpenCSV 
        tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_rm-no end_rm-no begin_procat end_procat begin_date 
        end_date begin_vend end_vend tb_receipts tb_adjustments tb_issues 
        tb_counts tb_transfers sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
        btn_Up btn_down rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok 
        btn-cancel 
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
 /*    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY. */

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
    /* ---------------------------------------------- rm/rep/rm-trans.p 07/98 JLF */
    /* raw materials - transactions edit list                                     */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE v-fvend        LIKE item.vend-no NO-UNDO.
    DEFINE VARIABLE v-tvend        LIKE v-fvend INIT "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE v-fitem        LIKE rm-rcpth.i-no NO-UNDO.
    DEFINE VARIABLE v-titem        LIKE v-fitem INIT "zzzzzzzzzz" NO-UNDO.
    DEFINE VARIABLE v-fpcat        LIKE item.procat NO-UNDO.
    DEFINE VARIABLE v-tpcat        LIKE v-fpcat INIT "zzzzz" NO-UNDO.
    DEFINE VARIABLE v-fdate        AS DATE      FORMAT "99/99/99" INIT 01/01/0001 NO-UNDO.
    DEFINE VARIABLE v-tdate        LIKE v-fdate INIT TODAY NO-UNDO.
    DEFINE VARIABLE v-type         AS CHARACTER FORMAT "x(5)" INIT "RITAC" NO-UNDO.
    DEFINE VARIABLE v-code         LIKE rm-rcpth.rita-code NO-UNDO.

    DEFINE VARIABLE v-value        AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-job-no       AS CHARACTER FORMAT "x(9)" NO-UNDO.
    DEFINE VARIABLE v-qty          LIKE rm-rdtlh.qty EXTENT 3 NO-UNDO.
    DEFINE VARIABLE v-val          LIKE v-value EXTENT 3 NO-UNDO.
    DEFINE VARIABLE v-first        AS LOG       EXTENT 3 NO-UNDO.
    DEFINE VARIABLE v-msf          AS DECIMAL   EXTENT 4 NO-UNDO.
    DEFINE VARIABLE v-ton          AS DECIMAL   EXTENT 4 NO-UNDO.
    DEFINE VARIABLE v-wid          LIKE po-ordl.s-wid NO-UNDO.
    DEFINE VARIABLE v-len          LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-dep          LIKE item.s-dep NO-UNDO.
    DEFINE VARIABLE v-bwt          LIKE item.basis-w NO-UNDO.

    DEFINE VARIABLE v-i-no         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-i-name       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-trans-date   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-vend-no      AS CHARACTER NO-UNDO.

    DEFINE VARIABLE v-export       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE v-excel-hdr    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-exp-name     AS CHARACTER FORMAT "x(40)" INIT "c:\tmp\r-tonsmsf.csv" NO-UNDO.

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

    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.

    {custom/statusMsg.i "'Processing...'"} 

    FORM HEADER ""

        WITH FRAME r-top STREAM-IO WIDTH 332 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.

    FORM rm-rcpth.i-no LABEL "ITEM"
        rm-rcpth.i-name FORMAT "x(14)" LABEL "DESCRIPTION"
        rm-rcpth.trans-date LABEL "DATE" FORM "99/99/99"
        rm-rcpth.po-no LABEL "P.O.#"
        po-ord.vend-no LABEL "Vendor#"
        v-job-no LABEL "   Job #"
        rm-rdtlh.qty FORMAT "->>>>>>>9.99<<" LABEL "QUANTITY"
        v-msf[4] FORMAT "->>>>>>9.9999" LABEL "MSF"
        v-ton[4] FORMAT "->>>>>>9.9999" LABEL "TONS"
        rm-rdtlh.cost FORMAT "->>>>>9.99<<<<" LABEL "COST"
        SPACE(0)
        v-value LABEL "VALUE"  
        SKIP

        WITH FRAME itemx NO-BOX DOWN STREAM-IO WIDTH 432.

    ASSIGN
        str-tit2   = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}
        v-fitem    = begin_rm-no
        v-titem    = end_rm-no
        v-fpcat    = begin_procat
        v-tpcat    = end_procat
        v-fdate    = begin_date
        v-tdate    = end_date
        v-fvend    = begin_vend
        v-tvend    = end_vend
        v-type     = (IF tb_receipts    THEN "R" ELSE "") +
             (IF tb_issues      THEN "I" ELSE "") +
             (IF tb_transfers   THEN "T" ELSE "") +
             (IF tb_adjustments THEN "A" ELSE "") +
             (IF tb_counts      THEN "C" ELSE "")
        v-exp-name = cFileName.


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

        IF LOOKUP(ttRptSelected.TextList, "QUANTITY,MSF,TONS,VALUE") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    /*IF rd-dest EQ 3 THEN
       OUTPUT STREAM s-excel TO VALUE(v-exp-name).*/


    IF rd-dest EQ 3 THEN 
    DO:
        OUTPUT STREAM s-excel TO VALUE(TRIM(cFileName)).
        PUT STREAM s-excel UNFORMATTED excelheader SKIP.
    END. 

    SESSION:SET-WAIT-STATE ("general").

    EMPTY TEMP-TABLE tt-report.

    /*IF rd-dest EQ 3 THEN 
       PUT STREAM s-excel UNFORMATTED 
          'TRANS TYPE,ITEM,DESCRIPTION,DATE,P.O.#,VENDOR#,JOB#,QUANTITY,MSF,TONS,COST,VALUE'                 
          SKIP.   */

    FOR EACH rm-rcpth WHERE rm-rcpth.company    EQ cocode
        AND rm-rcpth.i-no       GE v-fitem
        AND rm-rcpth.i-no       LE v-titem
        AND rm-rcpth.trans-date GE v-fdate
        AND rm-rcpth.trans-date LE v-tdate
        AND index(CAPS(v-type),rm-rcpth.rita-code) GT 0
        AND (CAN-FIND(FIRST po-ord
        WHERE po-ord.company EQ cocode
        AND po-ord.po-no   EQ int(rm-rcpth.po-no)
        AND po-ord.vend-no GE v-fvend
        AND po-ord.vend-no LE v-tvend) OR
        ("" GE v-fvend AND "" LE v-tvend))
        USE-INDEX i-no NO-LOCK,

        FIRST ITEM WHERE item.company EQ cocode
        AND item.i-no    EQ rm-rcpth.i-no
        AND item.procat  GE v-fpcat
        AND item.procat  LE v-tpcat NO-LOCK,

        EACH rm-rdtlh WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
        AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code NO-LOCK
        BREAK BY rm-rcpth.rita-code
        BY rm-rcpth.i-no
        BY rm-rcpth.trans-date

        TRANSACTION:

        {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

        v-code = rm-rcpth.rita-code.

        IF FIRST-OF(rm-rcpth.rita-code) THEN 
        DO:
            IF FIRST(rm-rcpth.rita-code) THEN VIEW FRAME r-top.

            PAGE.
        END.

        IF FIRST-OF(rm-rcpth.trans-date) THEN v-first[1] = YES.
        IF FIRST-OF(rm-rcpth.i-no)       THEN v-first[2] = YES.

        ASSIGN
            v-job-no = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', rm-rdtlh.job-no, rm-rdtlh.job-no2)) 
            v-value  = rm-rdtlh.cost * rm-rdtlh.qty
            v-bwt    = item.basis-w
            v-wid    = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
            v-len    = IF item.r-wid EQ 0 THEN item.s-len ELSE 12
            v-dep    = item.s-dep.

        RELEASE po-ordl.
        RELEASE po-ord.

        IF rm-rcpth.po-no NE "" THEN
            FIND FIRST po-ordl
                WHERE po-ordl.company EQ rm-rcpth.company
                AND po-ordl.po-no   EQ int(rm-rcpth.po-no)
                AND po-ordl.i-no    EQ rm-rcpth.i-no
                AND po-ordl.s-num   EQ rm-rdtlh.s-num
                AND po-ordl.b-num   EQ rm-rdtlh.b-num
                NO-LOCK NO-ERROR.
        IF rm-rcpth.po-no NE "" AND NOT AVAILABLE po-ordl THEN 
            FIND FIRST po-ordl
                WHERE po-ordl.company EQ rm-rcpth.company
                AND po-ordl.po-no   EQ int(rm-rcpth.po-no)
                AND po-ordl.i-no    EQ rm-rcpth.i-no
                AND po-ordl.s-num   EQ rm-rdtlh.s-num
                NO-LOCK NO-ERROR.

        IF AVAILABLE po-ordl THEN 
        DO:

            FIND FIRST po-ord WHERE
                po-ord.company EQ po-ordl.company AND
                po-ord.po-no   EQ po-ordl.po-no
                NO-LOCK NO-ERROR.

            ASSIGN
                v-wid = po-ordl.s-wid
                v-len = po-ordl.s-len.
        END.
        ELSE IF ITEM.i-code EQ "E" THEN
            DO:
                FIND FIRST job-mat WHERE
                    job-mat.company EQ rm-rcpth.company AND
                    job-mat.job-no EQ rm-rcpth.job-no AND
                    job-mat.job-no2 EQ rm-rcpth.job-no2 AND
                    job-mat.i-no EQ rm-rcpth.i-no AND
                    job-mat.frm EQ rm-rdtlh.s-num
                    NO-LOCK NO-ERROR.

                IF AVAILABLE job-mat THEN
                    ASSIGN
                        v-wid = job-mat.wid
                        v-len = job-mat.len.
            END.

        IF rm-rcpth.pur-uom EQ "MSF" THEN
            v-msf[4] = rm-rdtlh.qty.
        ELSE
            RUN sys/ref/convquom.p (rm-rcpth.pur-uom, "MSF",
                v-bwt, v-len, v-wid, v-dep,
                rm-rdtlh.qty, OUTPUT v-msf[4]).

        IF rm-rcpth.pur-uom EQ "TON" THEN
            v-ton[4] = rm-rdtlh.qty.
        ELSE
            RUN sys/ref/convquom.p (rm-rcpth.pur-uom, "TON",
                v-bwt, v-len, v-wid, v-dep,
                rm-rdtlh.qty, OUTPUT v-ton[4]).

        IF trim(v-job-no) BEGINS "-" THEN v-job-no = "".

        /*display "" @ rm-rcpth.i-no
                rm-rcpth.i-no       when first-of(rm-rcpth.i-no)
                "" @ rm-rcpth.i-name
                rm-rcpth.i-name     when first-of(rm-rcpth.i-no)
                "" @ rm-rcpth.trans-date
                rm-rcpth.trans-date when first-of(rm-rcpth.trans-date)
                rm-rcpth.po-no
                po-ord.vend-no      when avail po-ord
                v-job-no
                rm-rdtlh.qty
                v-msf[4]
                v-ton[4]
                rm-rdtlh.cost
                v-value
            with frame itemx.
        down with frame itemx. */

        /* IF rd-dest EQ 3 THEN DO: */
        IF FIRST-OF(rm-rcpth.i-no) THEN
            ASSIGN
                v-i-no   = rm-rcpth.i-no
                v-i-name = rm-rcpth.i-name.   
        ELSE
            ASSIGN
                v-i-no   = ""
                v-i-name = "".

        IF FIRST-OF(rm-rcpth.trans-date) THEN
            v-trans-date = STRING(rm-rcpth.trans-date).
        ELSE
            v-trans-date = "".

        IF AVAILABLE(po-ord) THEN
            v-vend-no = po-ord.vend-no.
        ELSE
            v-vend-no = "".

        /* PUT STREAM s-excel UNFORMATTED 
            '"' v-code           '",'
            '"' v-i-no           '",'
            '"' v-i-name         '",'
            '"' v-trans-date     '",'
            '"' rm-rcpth.po-no   '",'
            '"' v-vend-no        '",' 
            '"' v-job-no         '",'
            '"' rm-rdtlh.qty     '",'
            '"' v-msf[4]         '",'
            '"' v-ton[4]         '",'
            '"' rm-rdtlh.cost    '",'
            '"' v-value          '"'
            SKIP.
      END.  */

        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "trn-typ"  THEN 
                    cVarValue = STRING(v-code) .
                WHEN "i-no"     THEN 
                    cVarValue = STRING(v-i-no,"x(10)") .
                WHEN "dscr"     THEN 
                    cVarValue = STRING(v-i-name,"x(15)") .
                WHEN "dat"      THEN 
                    cVarValue = DYNAMIC-FUNCTION("sfFormat_Date", DATE(v-trans-date) ) .
                WHEN "po"       THEN 
                    cVarValue = STRING(rm-rcpth.po-no,"x(9)")  .
                WHEN "vend"     THEN 
                    cVarValue = STRING(v-vend-no).
                WHEN "job"      THEN 
                    cVarValue = STRING(v-job-no).
                WHEN "qty"      THEN 
                    cVarValue = STRING(rm-rdtlh.qty,"->>>>>>>9.99<<") .
                WHEN "msf"      THEN 
                    cVarValue = STRING(v-msf[4],"->>>>>>9.9999") .
                WHEN "ton"      THEN 
                    cVarValue = STRING(v-ton[4],"->>>>>>9.9999") .
                WHEN "cst"      THEN 
                    cVarValue = STRING(rm-rdtlh.cost,"->>>>>9.99<<<<") .
                WHEN "val"      THEN 
                    cVarValue = STRING(v-value,"->,>>>,>>>,>>9.99")    .
            END CASE.
            IF cTmpField = "i-no" THEN
                cExcelVarValue = rm-rcpth.i-no.
            ELSE IF  cTmpField = "dscr" THEN
                    cExcelVarValue = rm-rcpth.i-name.
                ELSE IF  cTmpField = "dat" THEN
                        cExcelVarValue = IF rm-rcpth.trans-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",rm-rcpth.trans-date) ELSE "".
                    ELSE
                        cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,cExcelVarValue)) + ",".            
        END.

        PUT UNFORMATTED cDisplay SKIP.
        IF rd-dest EQ 3 THEN 
        DO:
            PUT STREAM s-excel UNFORMATTED  
                cExcelDisplay SKIP.
        END.

        ASSIGN
            v-qty[1] = v-qty[1] + rm-rdtlh.qty
            v-val[1] = v-val[1] + v-value
            v-msf[1] = v-msf[1] + v-msf[4]
            v-ton[1] = v-ton[1] + v-ton[4].

        IF LAST-OF(rm-rcpth.trans-date) THEN 
        DO:
            IF NOT v-first[1] THEN 
            DO:
                /*underline rm-rdtlh.qty
                          v-msf[4]
                          v-ton[4]
                          v-value
                    with frame itemx.
   
                display " DATE TOTALS" @ rm-rcpth.i-name
                        v-qty[1]       @ rm-rdtlh.qty
                        v-msf[1]       @ v-msf[4]
                        v-ton[1]       @ v-ton[4]
                        v-val[1]       @ v-value
                    with frame itemx.*/
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
                        WHEN "trn-typ"  THEN 
                            cVarValue = "" .
                        WHEN "i-no"     THEN 
                            cVarValue = "" .
                        WHEN "dscr"     THEN 
                            cVarValue = "" .
                        WHEN "dat"      THEN 
                            cVarValue = "" .
                        WHEN "po"       THEN 
                            cVarValue = "" .
                        WHEN "vend"     THEN 
                            cVarValue = "" .
                        WHEN "job"      THEN 
                            cVarValue = "" .
                        WHEN "qty"      THEN 
                            cVarValue = STRING(v-qty[1],"->>>>>>>9.99<<") .
                        WHEN "msf"      THEN 
                            cVarValue = STRING(v-msf[1],"->>>>>>9.9999") .
                        WHEN "ton"      THEN 
                            cVarValue = STRING(v-ton[1],"->>>>>>9.9999") .
                        WHEN "cst"      THEN 
                            cVarValue = "" .
                        WHEN "val"      THEN 
                            cVarValue = STRING(v-val[1],"->,>>>,>>>,>>9.99")    .
                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.

                PUT UNFORMATTED  
                    "       DATE TOTALS" SUBSTRING(cDisplay,19,300) SKIP.
            END.

            IF NOT LAST-OF(rm-rcpth.i-no) THEN PUT SKIP(1).

            ASSIGN
                v-qty[2] = v-qty[2] + v-qty[1]
                v-val[2] = v-val[2] + v-val[1]
                v-msf[2] = v-msf[2] + v-msf[1]
                v-ton[2] = v-ton[2] + v-ton[1]

                v-qty[1] = 0
                v-val[1] = 0
                v-msf[1] = 0
                v-ton[1] = 0.
        END.

        IF LAST-OF(rm-rcpth.i-no) THEN 
        DO:
            IF NOT v-first[2] THEN 
            DO:
                /*underline rm-rdtlh.qty
                          v-msf[4]
                          v-ton[4]
                          v-value
                    with frame itemx.*/

                /* display " ITEM TOTALS" @ rm-rcpth.i-name
                         v-qty[2]       @ rm-rdtlh.qty
                         v-msf[2]       @ v-msf[4]
                         v-ton[2]       @ v-ton[4]
                         v-val[2]       @ v-value
                     with frame itemx.*/
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
                        WHEN "trn-typ"  THEN 
                            cVarValue = "" .
                        WHEN "i-no"     THEN 
                            cVarValue = "" .
                        WHEN "dscr"     THEN 
                            cVarValue = "" .
                        WHEN "dat"      THEN 
                            cVarValue = "" .
                        WHEN "po"       THEN 
                            cVarValue = "" .
                        WHEN "vend"     THEN 
                            cVarValue = "" .
                        WHEN "job"      THEN 
                            cVarValue = "" .
                        WHEN "qty"      THEN 
                            cVarValue = STRING(v-qty[2],"->>>>>>>9.99<<") .
                        WHEN "msf"      THEN 
                            cVarValue = STRING(v-msf[2],"->>>>>>9.9999") .
                        WHEN "ton"      THEN 
                            cVarValue = STRING(v-ton[2],"->>>>>>9.9999") .
                        WHEN "cst"      THEN 
                            cVarValue = "" .
                        WHEN "val"      THEN 
                            cVarValue = STRING(v-val[2],"->,>>>,>>>,>>9.99")    .
                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.

                PUT UNFORMATTED  
                    "       ITEM TOTALS" SUBSTRING(cDisplay,19,300) SKIP(1).
            END.

            PUT SKIP(2).

            ASSIGN
                v-qty[3] = v-qty[3] + v-qty[2]
                v-val[3] = v-val[3] + v-val[2]
                v-msf[3] = v-msf[3] + v-msf[2]
                v-ton[3] = v-ton[3] + v-ton[2]

                v-qty[2] = 0
                v-val[2] = 0
                v-msf[2] = 0
                v-ton[2] = 0.
        END.

        v-first[1] = NO.
        IF LAST-OF(rm-rcpth.trans-date) THEN v-first[2] = NO.

        IF LAST-OF(rm-rcpth.rita-code) THEN 
        DO:
            /* underline rm-rdtlh.qty
                       v-msf[4]
                       v-ton[4]
                       v-value
                 with frame itemx.
  
             display " TYPE TOTALS" @ rm-rcpth.i-name
                     v-qty[3]       @ rm-rdtlh.qty
                     v-msf[3]       @ v-msf[4]
                     v-ton[3]       @ v-ton[4]      
                     v-val[3]       @ v-value
                   with frame itemx.*/

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
                    WHEN "trn-typ"  THEN 
                        cVarValue = "" .
                    WHEN "i-no"     THEN 
                        cVarValue = "" .
                    WHEN "dscr"     THEN 
                        cVarValue = "" .
                    WHEN "dat"      THEN 
                        cVarValue = "" .
                    WHEN "po"       THEN 
                        cVarValue = "" .
                    WHEN "vend"     THEN 
                        cVarValue = "" .
                    WHEN "job"      THEN 
                        cVarValue = "" .
                    WHEN "qty"      THEN 
                        cVarValue = STRING(v-qty[3],"->>>>>>>9.99<<") .
                    WHEN "msf"      THEN 
                        cVarValue = STRING(v-msf[3],"->>>>>>9.9999") .
                    WHEN "ton"      THEN 
                        cVarValue = STRING(v-ton[3],"->>>>>>9.9999") .
                    WHEN "cst"      THEN 
                        cVarValue = "" .
                    WHEN "val"      THEN 
                        cVarValue = STRING(v-val[3],"->,>>>,>>>,>>9.99")    .
                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED  
                "       TYPE TOTALS" SUBSTRING(cDisplay,19,300) SKIP(1).

            ASSIGN
                v-qty[3] = 0
                v-val[3] = 0
                v-msf[3] = 0
                v-ton[3] = 0.
        END.

        IF v-code NE "T" THEN 
        DO:
            FIND FIRST costtype
                WHERE costtype.company   EQ cocode
                AND costtype.loc       EQ rm-rdtlh.loc
                AND costtype.cost-type EQ item.cost-type
                NO-LOCK NO-ERROR.

            IF v-code EQ "R" THEN
            DO:
                CREATE tt-report.
                ASSIGN
                    tt-report.term-id = ""
                    tt-report.key-01  = IF AVAILABLE costtype THEN costtype.inv-asset
                                   ELSE "Cost Type not found"
                    tt-report.key-02  = STRING(v-value,"->>,>>>,>>9.99").
            END.

            ELSE
            DO:
                CREATE tt-report.
                ASSIGN
                    tt-report.term-id = ""
                    tt-report.key-01  = IF AVAILABLE costtype THEN costtype.cons-exp
                                   ELSE "Cost Type not found"
                    tt-report.key-02  = STRING(v-value,"->>,>>>,>>9.99").    
            END.
        END.
    END.

    HIDE FRAME r-top2.

    v-value = 0.
    PUT SKIP(2) .
    FOR EACH tt-report WHERE tt-report.term-id EQ "",
        FIRST account
        WHERE account.company EQ cocode
        AND account.actnum  EQ tt-report.key-01
        NO-LOCK
        BREAK BY tt-report.key-01
        TRANSACTION:

        IF FIRST(tt-report.key-01) THEN PAGE.

        HIDE FRAME r-top.
        v-value = v-value + dec(tt-report.key-02).

        IF LAST-OF(tt-report.key-01) THEN 
        DO:
            DISPLAY account.actnum
                account.dscr
                v-value  LABEL "Amount" (TOTAL)      FORMAT "->>,>>>,>>9.99"
                WITH STREAM-IO WIDTH 132.

            v-value = 0.
        END.

        DELETE tt-report.
    END.

    IF rd-dest EQ 3 THEN 
    DO:
        OUTPUT STREAM s-excel close.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(v-exp-name)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE (""). 

    OUTPUT CLOSE.   
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
                .
        ELSE
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "NO"
                fi_file:SENSITIVE       = NO
                tb_OpenCSV:SENSITIVE    = NO      
                .
        ASSIGN 
            fi_file:SCREEN-VALUE = "c:\tmp\r-rmroll.csv".   
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

