&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: porep\r-shpapr.w

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
DEFINE VARIABLE list-name AS cha       NO-UNDO.
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

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD mach-vend LIKE vend.vend-no.

DEFINE TEMP-TABLE wk-sh-ord NO-UNDO
    FIELD due-date LIKE po-ordl.due-date
    FIELD rec-id   AS RECID.

DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS cha       NO-UNDO.
DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG     NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS cha     NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS cha     NO-UNDO.
DEFINE VARIABLE cFieldLength       AS cha     NO-UNDO.
DEFINE VARIABLE cFieldType         AS cha     NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS cha       NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO .

ASSIGN 
    cTextListToSelect  = "Job#,PO#,Qty Ord,Receivd,Customer Name,Due,Units,Sht W," +
                           "Sht L,Machine,FGItem#,RMItem#,MSF Rem,Ship?,Vendor"

    cFieldListToSelect = "job,po,qty-ord,rec,cust,due,unit,sht-w," +
                            "sht-l,mach,fgitem,rmitem,msf,ship,vend"
    cFieldLength       = "13,8,9,9,22,10,6,8," + "8,10,15,10,10,5,15"
    cFieldType         = "c,i,c,c,c,c,c,i," + "i,c,c,c,i,c,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Job#,PO#,Qty Ord,Receivd,Customer Name,Due,Units,Sht W," +
                           "Sht L,Machine,FGItem#,RMItem#,MSF Rem,Ship?" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_vend end_vend ~
begin_due-date end_due-date rd_break rd_sort sl_avail Btn_Def sl_selected ~
Btn_Add Btn_Remove btn_Up btn_down rd-dest td-show-parm fi_file tb_OpenCSV ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_vend end_vend begin_due-date ~
end_due-date rd_break rd_sort sl_avail sl_selected rd-dest td-show-parm ~
fi_file tb_OpenCSV tbAutoClose lbl_break lbl_sort 

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
    SIZE 15 BY 1.29.

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

DEFINE VARIABLE begin_due-date AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Due Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_vend     AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_due-date   AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Due Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend       AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-shpapr.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE
    SIZE 43 BY 1
    FGCOLOR 0 .

DEFINE VARIABLE lbl_break      AS CHARACTER FORMAT "X(256)":U INITIAL "Page Break?" 
    VIEW-AS TEXT 
    SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE lbl_sort       AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
    VIEW-AS TEXT 
    SIZE 7 BY .62 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 52 BY 1 NO-UNDO.

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
    SIZE 16 BY 3.81 NO-UNDO.

DEFINE VARIABLE rd_break       AS CHARACTER INITIAL "Vendor#" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Vendor#", "Vendor#",
    "None", "None"
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "Job#" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Job#", "Job#",
    "Customer#", "Customer#",
    "Machine", "Machine"
    SIZE 43 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.33.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.95.

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
    SIZE 14 BY .81
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_vend AT ROW 2.43 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number"
    end_vend AT ROW 2.43 COL 69 COLON-ALIGNED HELP
    "Enter Ending Vendor Number"
    begin_due-date AT ROW 3.57 COL 26 COLON-ALIGNED
    end_due-date AT ROW 3.57 COL 69 COLON-ALIGNED HELP
    "Enter Ending Due Date"
    rd_break AT ROW 5.19 COL 43 NO-LABELS
    rd_sort AT ROW 6.24 COL 43 NO-LABELS
    sl_avail AT ROW 8.62 COL 3 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 8.62 COL 39 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 8.62 COL 60.4 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 9.62 COL 39 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 10.62 COL 39 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 11.67 COL 39 WIDGET-ID 40
    btn_down AT ROW 12.67 COL 39 WIDGET-ID 42
    rd-dest AT ROW 15.57 COL 6 NO-LABELS
    lv-ornt AT ROW 15.71 COL 31 NO-LABELS
    lv-font-name AT ROW 15.76 COL 29 COLON-ALIGNED NO-LABELS
    lines-per-page AT ROW 15.76 COL 54 COLON-ALIGNED
    tb_excel AT ROW 16 COL 50 RIGHT-ALIGNED
    lv-font-no AT ROW 16 COL 36 COLON-ALIGNED
    td-show-parm AT ROW 17.43 COL 29
    fi_file AT ROW 18.33 COL 28 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 18.43 COL 86.4 RIGHT-ALIGNED
    tbAutoClose AT ROW 20.86 COL 31.2 WIDGET-ID 16
    btn-ok AT ROW 21.67 COL 30.6
    btn-cancel AT ROW 21.67 COL 51.4
    lbl_break AT ROW 5.19 COL 26 COLON-ALIGNED NO-LABELS
    lbl_sort AT ROW 6.24 COL 33 COLON-ALIGNED NO-LABELS
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 7.91 COL 12.2 WIDGET-ID 38
    "Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 14.43 COL 4.2
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.33 COL 4.4
    BGCOLOR 15 
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 7.91 COL 59.8 WIDGET-ID 44
    RECT-6 AT ROW 14.71 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.6 ROW 1.24
    SIZE 94.4 BY 24.1
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
        TITLE              = "Shipment Approval Report"
        HEIGHT             = 22.38
        WIDTH              = 95.4
        MAX-HEIGHT         = 33.29
        MAX-WIDTH          = 204.8
        VIRTUAL-HEIGHT     = 33.29
        VIRTUAL-WIDTH      = 204.8
        RESIZE             = YES
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = 15
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
    begin_due-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_due-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_break IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_break:PRIVATE-DATA IN FRAME FRAME-A = "rd_break".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort".

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
    rd_break:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Shipment Approval Report */
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
ON WINDOW-CLOSE OF C-Win /* Shipment Approval Report */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_due-date C-Win
ON LEAVE OF begin_due-date IN FRAME FRAME-A /* Beginning Due Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor# */
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
            fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.
        RUN GetSelectionList.
        RUN run-report. 
        STATUS DEFAULT "Processing Complete".

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
                    {custom/asifax.i &begin_cust=begin_vend
                            &END_cust=END_vend
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END.
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail.i &TYPE = "Vendor"
                             &begin_cust= begin_vend
                             &END_cust=end_vend
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust= begin_vend
                                  &END_cust=end_vend
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

                    END.

                END. 
            WHEN 6 THEN RUN output-to-port.
        END CASE. 
        SESSION:SET-WAIT-STATE ("").
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
    DO:
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

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
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

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


&Scoped-define SELF-NAME end_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_due-date C-Win
ON LEAVE OF end_due-date IN FRAME FRAME-A /* Ending Due Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Name */
    DO:
    // assign {&self-name}.
        fi_file = ''.
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
        RUN pChangeDest.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_break
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_break C-Win
ON VALUE-CHANGED OF rd_break IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
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
        DEF VAR cSelectedList AS cha NO-UNDO.
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

    ASSIGN
        begin_due-date = DATE(1,1,YEAR(TODAY))
        end_due-date   = TODAY.
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
    {sys/inc/reportsConfigNK1.i "PR8" }
    ASSIGN
        td-show-parm:sensitive = lShowParameters
        td-show-parm:hidden    = NOT lShowParameters
        td-show-parm:visible   = lShowParameters
        .
    
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_vend.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList C-Win 
PROCEDURE DisplaySelectionList :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.

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
    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.
  
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
    DISPLAY begin_vend end_vend begin_due-date end_due-date rd_break rd_sort 
        sl_avail sl_selected rd-dest td-show-parm fi_file tb_OpenCSV 
        tbAutoClose lbl_break lbl_sort 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_vend end_vend begin_due-date end_due-date rd_break 
        rd_sort sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up 
        btn_down rd-dest td-show-parm fi_file tb_OpenCSV tbAutoClose btn-ok 
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
    DEFINE VARIABLE cTmpList AS cha NO-UNDO.

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
    {custom/out2file.i}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-grand-total C-Win 
PROCEDURE print-grand-total :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER v-char-ord-qty AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER v-msf-rem AS DECIMAL NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:

        PUT STREAM excel UNFORMATTED
            '"' "Grand Totals"                      '",'.

        IF rd_break EQ "Vendor#" THEN
            PUT STREAM excel UNFORMATTED
                '"' "" '",'.

        PUT STREAM excel UNFORMATTED
            '"' ""                                    '",'
            '"' v-char-ord-qty                        '",'
            '"' ""                                    '",'
            '"' ""                                    '",'
            '"' ""                                    '",'
            '"' ""                                    '",'
            '"' ""                                    '",'
            '"' ""                                    '",'
            '"' ""                                    '",'
            '"' ""                                    '",'
            '"' ""                                    '",'
            '"' STRING(v-msf-rem,"->,>>9.99")         '",'
            '"' ""                                    '",'
            SKIP.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ------------------------------------------------ po/rep/sh-apr.p 6/00 djk  */
    /* Sheets On Order Report                                                     */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-topw2.f}*/
    {sys/form/r-top.i}

    {sys/inc/ctrtext.i str-tit 112}.

    FORM  HEADER
        SKIP(1)
        day_str
        str-tit  FORMAT "x(112)"
        "Page" AT 123
        PAGE-NUMBER FORMAT ">>9"
        SKIP
        tim_str
        str-tit2 FORMAT "x(112)"   "{1}" AT 123
        SKIP(1)

        WITH FRAME r-top ROW 1 COLUMN 1 STREAM-IO WIDTH 210
        NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.


    DEFINE VARIABLE v-msf-rem       AS DECIMAL   FORMAT "->,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-cst-rem       AS DECIMAL   FORMAT ">>>,>>9" NO-UNDO.
    DEFINE VARIABLE v-s-vend        LIKE vend.vend-no INIT "" NO-UNDO.
    DEFINE VARIABLE v-e-vend        LIKE vend.vend-no INIT "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE v-s-date        LIKE po-ord.po-date FORMAT "99/99/9999" NO-UNDO INIT 01/01/2000.
    DEFINE VARIABLE v-e-date        LIKE po-ord.po-date FORMAT "99/99/9999" INIT TODAY NO-UNDO.
    DEFINE VARIABLE v-name          AS LOG       NO-UNDO INIT YES.
    DEFINE VARIABLE v-cust-name     LIKE oe-ord.cust-name.
    DEFINE VARIABLE v-wid           LIKE po-ordl.s-wid.
    DEFINE VARIABLE v-len           LIKE po-ordl.s-len.
    DEFINE VARIABLE v-dep           LIKE item.s-dep.
    DEFINE VARIABLE v-fgitem        AS LOGICAL   INIT YES.
    DEFINE VARIABLE v-pmach         AS LOGICAL   INIT YES.
    DEFINE VARIABLE v-preld         AS LOGICAL   INIT YES.
    DEFINE VARIABLE v-sortby        AS LOGICAL   FORMAT "J/C" INIT YES.
    DEFINE VARIABLE v-bwt           LIKE item.basis-w.
    DEFINE VARIABLE v-raw           LIKE po-ordl.i-no.
    DEFINE VARIABLE v-fg            LIKE po-ordl.i-no.

    DEFINE VARIABLE v-cust-vend     AS CHARACTER FORMAT "x(26)" INIT "--------- VENDOR ---------".

    DEFINE VARIABLE v-job-no        AS CHARACTER FORMAT "x(9)".

    DEFINE VARIABLE tot-cons-qty    LIKE po-ordl.cons-qty NO-UNDO.
    DEFINE VARIABLE tot-rec-qty     AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE tot-msf-rem     AS DECIMAL   FORMAT "->>>,>>>,>>>,>>>,>>9.99" EXTENT 2 NO-UNDO.
    DEFINE VARIABLE tot-qty-ord     AS DECIMAL   FORMAT "->>>,>>>,>>>,>>>,>>9.99" EXTENT 2 NO-UNDO.

    DEFINE VARIABLE tot-msf-rem-str AS CHARACTER FORMAT "x(60)" .

    DEFINE VARIABLE v-trcv          AS CHARACTER FORMAT "x(7)".    /* format "->>,>>9.9" */ 
    DEFINE VARIABLE v-char-ord-qty  AS CHARACTER FORMAT "x(8)".
    DEFINE VARIABLE v-rel-date      LIKE oe-rel.rel-date.
    DEFINE VARIABLE v-mach          LIKE mach.m-code.
    DEFINE VARIABLE v-vend          LIKE po-ord.vend-no.
    DEFINE VARIABLE v-stat          AS CHARACTER.
    DEFINE VARIABLE lv-label        AS CHARACTER EXTENT 2 NO-UNDO. 
    DEFINE VARIABLE ll-sub          AS LOG       NO-UNDO.
    DEFINE VARIABLE lv-under        AS CHARACTER FORMAT "x(5)" INIT "_____" EXTENT 2 NO-UNDO.

    DEFINE VARIABLE cDisplay        AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelDisplay   AS cha       NO-UNDO.
    DEFINE VARIABLE hField          AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField       AS CHA       NO-UNDO.
    DEFINE VARIABLE cVarValue       AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelVarValue  AS cha       NO-UNDO.
    DEFINE VARIABLE cSelectedList   AS cha       NO-UNDO.
    DEFINE VARIABLE cFieldName      AS cha       NO-UNDO.
    DEFINE VARIABLE str-tit4        AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5        AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line        AS cha       FORM "x(200)" NO-UNDO.

    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
//DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

//RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

    FORM HEADER
        lv-label[1]                            FORMAT "x(20)" 
        SKIP(1)
        str-tit4
        SKIP
        str-tit5

        WITH FRAME r-top.

    FORM v-job-no                   AT 1     
        po-ordl.po-no              TO 16
        v-char-ord-qty             TO 25
        v-trcv                     TO 33
        v-cust-name                AT 35  FORMAT "x(20)"
        tt-report.key-07           AT 56
        lv-under[1]                AT 65
        po-ordl.s-wid              TO 77  FORMAT "->>9.99"
        po-ordl.s-len              TO 84  FORMAT "->>9.99"
        tt-report.mach-vend        AT 86
        v-fg                       AT 95  FORMAT "x(15)"       
        v-raw                      AT 110 FORMAT "x(10)"
        v-msf-rem                  TO 128
        lv-under[2]                AT 130

        WITH DOWN STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-UNDERLINE FRAME sh-ord.


    SESSION:SET-WAIT-STATE ("general").

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-s-vend = begin_vend
        v-e-vend = end_vend
        v-s-date = begin_due-date
        v-e-date = end_due-date
        /*v-name       = tb_cust
        v-fgitem     = tb_fg-itm
        v-pmach      = tb_mach-opr OR rd_sort EQ "Machine"
        v-preld      = tb_rel-date*/
        v-sortby = rd_sort EQ "Job#".

    ASSIGN 
        tot-cons-qty = 0
        tot-rec-qty  = 0
        tot-msf-rem  = 0
        lv-label[2]  = IF v-pmach OR rd_break NE "None" THEN "Machine"
                      ELSE "Vendor#".

    DEFINE VARIABLE cslist AS cha NO-UNDO.
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

        IF LOOKUP(ttRptSelected.TextList, "Qty Ord,MSF Rem") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).

        /*IF rd_break EQ "Vendor#" THEN
           excelheader = "Vendor,".
      
        excelheader = excelheader    
                    + "Job#,PO#,Qty Ord,Receivd,Customer Name,"
                    + "Due,Units,Sht W,Sht L," + lv-label[2] + ",FGItem#,RMItem#,"
                    + "MSF Rem,Ship?".*/
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.

    VIEW FRAME r-top.

    EMPTY TEMP-TABLE tt-report.

    FOR EACH po-ord
        WHERE po-ord.company EQ cocode
        AND index("CXF",po-ord.stat) EQ 0
        AND po-ord.vend-no GE v-s-vend
        AND po-ord.vend-no LE v-e-vend
        NO-LOCK:

        FOR EACH po-ordl WHERE
            po-ordl.company EQ po-ord.company AND
            po-ordl.po-no   EQ po-ord.po-no AND
            po-ordl.item-type
            AND index("CXF",po-ordl.stat) EQ 0
            AND po-ordl.due-date GE v-s-date
            AND po-ordl.due-date LE v-e-date
            NO-LOCK:

            {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}

            RELEASE job-hdr.
            RELEASE oe-ord.
            RELEASE oe-ordl.

            ASSIGN
                v-job-no    = TRIM(po-ordl.job-no) + "-" + string(po-ordl.job-no2,"999")
                v-raw       = ""
                v-fg        = ""
                v-cust-name = "".

            IF TRIM(v-job-no) EQ "-00" THEN v-job-no = "".

            IF po-ordl.item-type THEN 
            DO:
                v-raw = po-ordl.i-no.

                FIND FIRST job-hdr
                    WHERE job-hdr.company EQ cocode
                    AND job-hdr.job-no  EQ po-ordl.job-no
                    AND job-hdr.job-no2 EQ po-ordl.job-no2
                    NO-LOCK NO-ERROR.

                IF AVAILABLE job-hdr THEN 
                DO:
                    v-fg = job-hdr.i-no.

                    FIND FIRST oe-ordl
                        WHERE oe-ordl.company EQ cocode
                        AND oe-ordl.ord-no  EQ job-hdr.ord-no
                        AND oe-ordl.i-no    EQ v-fg
                        AND oe-ordl.job-no  EQ po-ordl.job-no
                        AND oe-ordl.job-no2 EQ po-ordl.job-no2
                        NO-LOCK NO-ERROR.
                END.
            END.

            ELSE 
            DO:
                v-fg = po-ordl.i-no.

                IF po-ordl.ord-no NE 0 THEN
                    FIND FIRST oe-ordl
                        WHERE oe-ordl.company EQ cocode
                        AND oe-ordl.ord-no  EQ po-ordl.ord-no
                        AND oe-ordl.i-no    EQ v-fg
                        NO-LOCK NO-ERROR.
            END.

            v-rel-date = ?.

            IF AVAILABLE oe-ordl THEN 
            DO:
                FIND FIRST oe-ord
                    WHERE oe-ord.company EQ cocode
                    AND oe-ord.ord-no  EQ oe-ordl.ord-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE oe-ord THEN v-cust-name = oe-ord.cust-name.

                FOR EACH oe-rell
                    WHERE oe-rell.company  EQ cocode
                    AND oe-rell.ord-no   EQ oe-ordl.ord-no
                    AND oe-rell.i-no     EQ oe-ordl.i-no
                    AND oe-rell.line     EQ oe-ordl.line
                    NO-LOCK,
                    FIRST oe-relh
                    WHERE oe-relh.r-no    EQ oe-rell.r-no
                    AND oe-relh.deleted EQ NO
                    NO-LOCK
                    BY oe-relh.rel-date:

                    LEAVE.
                END.

                IF AVAILABLE oe-relh THEN v-rel-date = oe-relh.rel-date.

                ELSE
                    FOR EACH oe-rel
                        WHERE oe-rel.company EQ cocode
                        AND oe-rel.ord-no  EQ oe-ordl.ord-no
                        AND oe-rel.i-no    EQ oe-ordl.i-no
                        AND oe-rel.line    EQ oe-ordl.line
                        NO-LOCK:

                        {oe/rel-stat.i v-stat}

                        IF INDEX("ILS",v-stat) NE 0                           AND
                            (oe-rel.rel-date LT v-rel-date OR v-rel-date EQ ?) THEN
                            v-rel-date = oe-rel.rel-date.
                    END.
            END.

            v-mach = "".

            FOR FIRST job
                WHERE job.company EQ po-ordl.company
                AND job.job-no  EQ po-ordl.job-no
                AND job.job-no2 EQ po-ordl.job-no2
                NO-LOCK,

                FIRST job-mch
                WHERE job-mch.company EQ job.company
                AND job-mch.job     EQ job.job
                AND job-mch.frm     EQ po-ordl.s-num
                AND (job-mch.dept   NE "DM" AND
                job-mch.dept   NE "PM")
                USE-INDEX line-idx NO-LOCK:

                v-mach = job-mch.m-code.
            END.

            CREATE tt-report.
            ASSIGN
                tt-report.key-01    = IF rd_break EQ "Vendor#" THEN po-ord.vend-no
                             ELSE
                             IF rd_sort EQ "Machine" THEN v-mach ELSE ""
                tt-report.key-02    = IF rd_sort EQ "Job#"      THEN v-job-no
                             ELSE
                             IF rd_sort EQ "Customer#" THEN v-cust-name
                             ELSE
                             IF rd_break EQ "Vendor#" THEN v-mach ELSE ""
                tt-report.key-03    = v-job-no
                tt-report.key-04    = v-fg
                tt-report.key-05    = v-raw
                tt-report.key-06    = v-cust-name
                tt-report.key-07    = IF v-rel-date EQ ? THEN ""
                                             ELSE STRING(v-rel-date,"99/99/99")
                tt-report.rec-id    = RECID(po-ordl)
                tt-report.mach-vend = IF v-pmach THEN v-mach
                             ELSE
                             IF rd_break EQ "None" THEN po-ord.vend-no ELSE "".
        END.
    END.

    IF v-name THEN v-cust-vend = "-------- CUSTOMER --------".

    VIEW FRAME r-top.

    FOR EACH tt-report,
        EACH po-ordl WHERE RECID(po-ordl) EQ tt-report.rec-id NO-LOCK,
        FIRST po-ord WHERE
        po-ord.company EQ po-ordl.company AND
        po-ord.po-no   EQ po-ordl.po-no NO-LOCK
        BREAK BY tt-report.key-01
        BY tt-report.key-02:

        {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}

        IF rd_break EQ "Vendor#" AND first-of(tt-report.key-01) THEN 
        DO:
            lv-label[1] = TRIM(rd_break) + ": " + tt-report.key-01.
            PAGE.
        END.

        RELEASE item.
        RELEASE itemfg.

        IF po-ordl.item-type THEN 
        DO:
            FIND FIRST item
                WHERE item.company EQ cocode
                AND item.i-no    EQ po-ordl.i-no
                NO-LOCK NO-ERROR.
        END.

        ELSE 
        DO:
            FIND FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ po-ordl.i-no
                NO-LOCK NO-ERROR.
        END.

        ASSIGN
            v-len = po-ordl.s-len
            v-wid = po-ordl.s-wid
            v-dep = IF AVAILABLE item THEN item.s-dep ELSE 0
            v-bwt = 0.

        IF (v-len EQ 0 OR v-wid EQ 0 OR v-bwt EQ 0) THEN 
        DO:
            FIND FIRST job
                WHERE job.company EQ cocode
                AND job.job-no  EQ po-ordl.job-no
                AND job.job-no2 EQ po-ordl.job-no2
                NO-LOCK NO-ERROR.

            IF AVAILABLE job THEN 
            DO:
                FIND FIRST job-mat
                    WHERE job-mat.company EQ cocode
                    AND job-mat.job     EQ job.job
                    AND job-mat.i-no    EQ po-ordl.i-no
                    NO-LOCK NO-ERROR.

                IF AVAILABLE job-mat THEN
                    ASSIGN
                        v-len = IF v-len EQ 0 THEN job-mat.len     ELSE v-len
                        v-wid = IF v-wid EQ 0 THEN job-mat.wid     ELSE v-wid
                        v-bwt = IF v-bwt EQ 0 THEN job-mat.basis-w ELSE v-bwt.
            END.

            IF AVAILABLE item THEN 
            DO:
                IF po-ordl.item-type = YES THEN 
                    IF v-len EQ 0 THEN v-len = item.s-len.

                IF po-ordl.item-type = YES THEN 
                    IF v-wid EQ 0 THEN v-wid = IF item.r-wid NE 0 THEN item.r-wid
                        ELSE item.s-wid.

                IF po-ordl.item-type = YES THEN 
                    IF v-bwt EQ 0 THEN v-bwt = item.basis-w.
            END.
        END.

        IF po-ordl.cons-uom EQ "MSF" THEN
            v-msf-rem = po-ordl.cons-qty - po-ordl.t-rec-qty.

        ELSE 
        DO:
            RUN sys/ref/convquom.p(po-ordl.cons-uom, "MSF",
                v-bwt, v-len, v-wid, v-dep,
                (po-ordl.cons-qty - po-ordl.t-rec-qty),
                OUTPUT v-msf-rem).
        END.

        RUN sys/ref/convcuom.p(po-ordl.cons-uom, "MSF",
            v-bwt, v-len, v-wid, v-dep,
            po-ordl.cons-cost, OUTPUT v-cst-rem).

        ASSIGN
            tot-cons-qty   = tot-cons-qty + po-ordl.cons-qty
            tot-rec-qty    = tot-rec-qty + t-rec-qty
            tot-qty-ord[1] = tot-qty-ord[1] + po-ordl.ord-qty
            tot-msf-rem[1] = tot-msf-rem[1] + v-msf-rem.

        IF v-msf-rem GE 0 OR v-cst-rem GE 0 THEN 
        DO:
            ASSIGN
                v-job-no    = tt-report.key-03
                v-fg        = tt-report.key-04
                v-raw       = tt-report.key-05
                v-cust-name = tt-report.key-06.

            IF (v-cust-name EQ "") OR (v-name EQ NO) THEN v-cust-name = FILL("_",20).

            IF po-ordl.ord-qty - trunc(po-ordl.ord-qty,0) NE 0 AND
                po-ordl.ord-qty LT 100000                       THEN
                v-char-ord-qty = STRING(po-ordl.ord-qty,">>,>>9.9<<<").
            ELSE
                v-char-ord-qty = STRING(po-ordl.ord-qty,">>>>,>>9").

            IF po-ordl.t-rec-qty NE 0 THEN
                IF po-ordl.t-rec-qty - trunc(po-ordl.t-rec-qty,0) NE 0 AND
                    po-ordl.t-rec-qty LT 10000                          THEN
                    v-trcv = STRING(po-ordl.t-rec-qty,">,>>9.9<<<").
                ELSE
                    v-trcv = STRING(po-ordl.t-rec-qty,">>>,>>9").
            ELSE
                v-trcv = FILL("_",7).

            /*  display v-job-no                              
                      po-ordl.po-no
                      v-char-ord-qty                      
                      v-trcv
                      v-cust-name                         
                      tt-report.key-07
                        "________" when not v-preld @ tt-report.key-07
                      lv-under[1]
                      po-ordl.s-wid                       
                      po-ordl.s-len
                      tt-report.mach-vend
                        "______" when not v-pmach AND rd_break NE "None" @ tt-report.mach-vend
                      v-fg when v-fgitem 
                      v-raw
                      v-msf-rem
                      lv-under[2]
                  with frame sh-ord.
              down with frame sh-ord.*/

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "job"    THEN 
                        cVarValue = STRING(v-job-no,"x(13)") .
                    WHEN "po"   THEN 
                        cVarValue = STRING(po-ordl.po-no,">>>>>>>>").
                    WHEN "qty-ord"   THEN 
                        cVarValue = STRING(v-char-ord-qty,"x(9)").
                    WHEN "rec"  THEN 
                        cVarValue = STRING(v-trcv) .
                    WHEN "cust"   THEN 
                        cVarValue = STRING(v-cust-name,"x(22)") .
                    WHEN "due"  THEN 
                        cVarValue = STRING(tt-report.key-07) .
                    WHEN "unit"   THEN 
                        cVarValue = STRING(lv-under[1]) .
                    WHEN "sht-w"  THEN 
                        cVarValue = STRING(po-ordl.s-wid,"->>>9.99") .
                    WHEN "sht-l"   THEN 
                        cVarValue = STRING(po-ordl.s-len,"->>>9.99").
                    WHEN "mach"  THEN 
                        cVarValue = STRING(tt-report.mach-vend,"x(10)") .
                    WHEN "fgitem"   THEN 
                        cVarValue = STRING(v-fg,"x(15)") .
                    WHEN "rmitem"  THEN 
                        cVarValue = STRING(v-raw,"x(10)") .
                    WHEN "msf"   THEN 
                        cVarValue = STRING(v-msf-rem,">>>>>>9.99") .
                    WHEN "ship"  THEN 
                        cVarValue = STRING(lv-under[2]) .
                    WHEN "vend"  THEN 
                        cVarValue = STRING(po-ordl.vend-no,"x(15)") .

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest = 3 THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
            END.

            ll-sub = YES.
        END.

        IF LAST-OF(tt-report.key-01) THEN 
        DO:
            IF (rd_break EQ "Vendor#" OR rd_sort EQ "Machine") AND ll-sub THEN 
            DO:
                IF tot-qty-ord[1] - TRUNC(tot-qty-ord[1],0) NE 0 AND
                    tot-qty-ord[1] LT 100000                       THEN
                    v-char-ord-qty = STRING(tot-qty-ord[1],">>,>>9.9<<<").
                ELSE
                    v-char-ord-qty = STRING(tot-qty-ord[1],">>>>,>>9").

                v-msf-rem = tot-msf-rem[1].

                /* UNDERLINE v-char-ord-qty                      
                           v-msf-rem                          
                     WITH FRAME sh-ord.
         
                 DISPLAY FILL(" ",9 - LENGTH(TRIM(rd_break))) +
                         TRIM(IF rd_break EQ "Vendor#" THEN rd_break ELSE "Machine")
                                        @ v-job-no
                         "Totals"       @ po-ordl.po-no
                         v-char-ord-qty                      
                         v-msf-rem                            
                     WITH FRAME sh-ord.
                 DOWN WITH FRAME sh-ord.*/
                PUT str-line SKIP .
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "job"    THEN 
                            cVarValue = "" .
                        WHEN "po"   THEN 
                            cVarValue = "".
                        WHEN "qty-ord"   THEN 
                            cVarValue = STRING(v-char-ord-qty,"x(9)").
                        WHEN "rec"  THEN 
                            cVarValue = "" .
                        WHEN "cust"   THEN 
                            cVarValue = "" .
                        WHEN "due"  THEN 
                            cVarValue = "" .
                        WHEN "unit"   THEN 
                            cVarValue = "" .
                        WHEN "sht-w"  THEN 
                            cVarValue = "".
                        WHEN "sht-l"   THEN 
                            cVarValue = "".
                        WHEN "mach"  THEN 
                            cVarValue = "" .
                        WHEN "fgitem"   THEN 
                            cVarValue = "" .
                        WHEN "rmitem"  THEN 
                            cVarValue = "" .
                        WHEN "msf"   THEN 
                            cVarValue = STRING(v-msf-rem,">>>>>>9.99") .
                        WHEN "ship"  THEN 
                            cVarValue = "" .
                        WHEN "vend"  THEN 
                            cVarValue = "" .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.

                PUT UNFORMATTED 
                    "   "
                    TRIM(IF rd_break EQ "Vendor#" THEN rd_break ELSE "Machine") + "   Totals" + SUBSTRING(cDisplay,20,300) SKIP.
                IF rd-dest = 3 THEN 
                DO:
                    PUT STREAM excel UNFORMATTED FILL(" ",9 - LENGTH(TRIM(rd_break))) +
                        TRIM(IF rd_break EQ "Vendor#" THEN rd_break ELSE "Machine") ' Totals ,'  SUBSTRING(cExcelDisplay,4,300) SKIP.
                END.

                IF rd_break EQ "None" THEN PUT SKIP(2).

            END.

            ASSIGN
                tot-msf-rem[2] = tot-msf-rem[2] + tot-msf-rem[1]
                tot-qty-ord[2] = tot-qty-ord[2] + tot-qty-ord[1]

                tot-qty-ord[1] = 0
                tot-msf-rem[1] = 0
                ll-sub         = NO.
        END.

        IF LAST(tt-report.key-01) THEN 
        DO:
            IF tot-qty-ord[2] - TRUNC(tot-qty-ord[2],0) NE 0 AND
                tot-qty-ord[2] LT 100000                       THEN
                v-char-ord-qty = STRING(tot-qty-ord[2],">>,>>9.9<<<").
            ELSE
                v-char-ord-qty = STRING(tot-qty-ord[2],">>>>,>>9").

            v-msf-rem = tot-msf-rem[2].

            /*UNDERLINE v-char-ord-qty                      
                      v-msf-rem                          
                WITH FRAME sh-ord.
      
            UNDERLINE v-char-ord-qty                      
                      v-msf-rem                          
                WITH FRAME sh-ord.
      
            DISPLAY "    Grand" @ v-job-no
                    "Totals"    @ po-ordl.po-no
                    v-char-ord-qty                      
                    v-msf-rem                            
                WITH FRAME sh-ord.
            DOWN WITH FRAME sh-ord.*/
            PUT str-line SKIP .
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "job"    THEN 
                        cVarValue = "" .
                    WHEN "po"   THEN 
                        cVarValue = "".
                    WHEN "qty-ord"   THEN 
                        cVarValue = STRING(v-char-ord-qty,"x(9)").
                    WHEN "rec"  THEN 
                        cVarValue = "" .
                    WHEN "cust"   THEN 
                        cVarValue = "" .
                    WHEN "due"  THEN 
                        cVarValue = "" .
                    WHEN "unit"   THEN 
                        cVarValue = "" .
                    WHEN "sht-w"  THEN 
                        cVarValue = "".
                    WHEN "sht-l"   THEN 
                        cVarValue = "".
                    WHEN "mach"  THEN 
                        cVarValue = "" .
                    WHEN "fgitem"   THEN 
                        cVarValue = "" .
                    WHEN "rmitem"  THEN 
                        cVarValue = "" .
                    WHEN "msf"   THEN 
                        cVarValue = STRING(v-msf-rem,">>>>>>9.99") .
                    WHEN "ship"  THEN 
                        cVarValue = "" .
                    WHEN "vend"  THEN 
                        cVarValue = "" .

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED "      Grand Totals " + SUBSTRING(cDisplay,20,300) SKIP.
            IF rd-dest = 3 THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    '  Grand Totals ,' 
                    SUBSTRING(cExcelDisplay,4,300) SKIP.
            END.

        /*IF tb_excel THEN
           RUN print-grand-total(v-char-ord-qty,
                                 v-msf-rem).*/
        END.

        DELETE tt-report.
    END.

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE  NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS cha     NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS cha     NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha     NO-UNDO.

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
        ASSIGN rd-dest.
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
            fi_file:SCREEN-VALUE = "c:\tmp\r-shpapr.csv".
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

