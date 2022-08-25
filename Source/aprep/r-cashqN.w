&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: aprep\r-cashrq.w

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

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.
DEFINE VARIABLE is-xprint-form AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-pdf-file    AS CHARACTER NO-UNDO.
DEFINE STREAM excel.


DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.

DEFINE VARIABLE lRecFound          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lAPInvoiceLength   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cNK1Value          AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

RUN sys/ref/nk1look.p (INPUT cocode, "APInvoiceLength", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cNK1Value, OUTPUT lRecFound).
IF lRecFound THEN
    lAPInvoiceLength = LOGICAL(cNK1Value) NO-ERROR.


ASSIGN 
    cTextListToSelect  = "Vendor,Vendor Name,Invoice#,Inv Date,Due Date,1Gross,1Disc,2Gross,2Disc," +
                            "3Gross,3Disc,Old Gross,Old Disc,Total Gross,Total Disc,Company,Terms,Days Old,CC/ACH"
    cFieldListToSelect = "ap-inv.vend-no,vend-name,ap-inv.inv-no,ap-inv.inv-date,due-date,inv-t1,inv-d1,inv-t2,inv-d2," +
                            "inv-t3,inv-d3,inv-t4,inv-d4,ws_gross,ws_disc-avail,ap-inv.company,terms,dy-old,cc-ach"
    cFieldType         = "c,c,c,c,c,i,i,i,i," + "i,i,i,i,i,i,c,c,c,c"
    .
IF lAPInvoiceLength THEN
    ASSIGN cFieldLength = "8,30,20,10,10,11,8,11,8," +
                            "11,8,11,8,13,10,7,5,8,6".
ELSE
    ASSIGN cFieldLength = "8,30,12,10,10,11,8,11,8," +
                            "11,8,11,8,13,10,7,5,8,6".
                            
ASSIGN 
    cTextListToDefault = "Vendor,Vendor Name,Invoice#,Inv Date,Due Date,1Gross,1Disc,2Gross,2Disc," +
                             "3Gross,3Disc,Old Gross,Old Disc,Total Gross,Total Disc,Company".
{sys/inc/ttRptSel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_company end_company ~
begin_date-1 tb_disc-date begin_date-2 begin_date-3 rd_sort2 Btn_Def ~
sl_avail sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file ~
tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_company end_company begin_date-1 ~
tb_disc-date begin_date-2 begin_date-3 lbl_sort2 rd_sort2 sl_avail ~
sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
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

DEFINE VARIABLE begin_company  AS CHARACTER FORMAT "X(3)":U 
    LABEL "Beginning Company#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date-1   AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Date 1" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date-2   AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Date 2" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date-3   AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Date 3" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_company    AS CHARACTER FORMAT "X(3)":U INITIAL "zzz" 
    LABEL "Ending Company#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\APCashRequirements.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_sort2      AS CHARACTER FORMAT "X(256)":U INITIAL "Sort by?" 
    VIEW-AS FILL-IN 
    SIZE 8 BY 1 NO-UNDO.

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
    SIZE 16.4 BY 5.05 NO-UNDO.

DEFINE VARIABLE rd_sort2       AS CHARACTER INITIAL "Vendor" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Vendor", "Vendor",
    "Vend Name", "Vend Name",
    "Invoice#", "Invoice#",
    "Inv Date", "Inv Date"
    SIZE 57 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.52.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 8.33.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 34 BY 4.67 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 34 BY 4.67 NO-UNDO.

DEFINE VARIABLE tbAutoClose  AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_disc-date AS LOGICAL   INITIAL NO 
    LABEL "Print Discount Date?" 
    VIEW-AS TOGGLE-BOX
    SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_company AT ROW 3.33 COL 28.2 COLON-ALIGNED
    end_company AT ROW 3.33 COL 68.4 COLON-ALIGNED
    begin_date-1 AT ROW 4.76 COL 28.2 COLON-ALIGNED HELP
    "Enter First Date"
    tb_disc-date AT ROW 4.76 COL 55.4
    begin_date-2 AT ROW 5.95 COL 28.2 COLON-ALIGNED HELP
    "Enter the Second Date"
    begin_date-3 AT ROW 7.14 COL 28.2 COLON-ALIGNED HELP
    "Enter the Third Date"
    lbl_sort2 AT ROW 8.57 COL 19 COLON-ALIGNED NO-LABELS WIDGET-ID 58
    rd_sort2 AT ROW 8.57 COL 30 NO-LABELS WIDGET-ID 60
    Btn_Def AT ROW 10.71 COL 40.4 HELP
    "Default Selected Table to Tables to Audit" WIDGET-ID 56
    sl_avail AT ROW 10.76 COL 3 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 10.76 COL 59.8 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 11.67 COL 40.4 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 12.62 COL 40.4 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 13.57 COL 40.4 WIDGET-ID 40
    btn_down AT ROW 14.52 COL 40.4 WIDGET-ID 42
    lines-per-page AT ROW 16.14 COL 87 COLON-ALIGNED
    lv-font-no AT ROW 16.24 COL 39 COLON-ALIGNED
    lv-ornt AT ROW 16.24 COL 48 NO-LABELS
    rd-dest AT ROW 16.29 COL 4.6 NO-LABELS
    lv-font-name AT ROW 17.19 COL 29 COLON-ALIGNED NO-LABELS
    tb_excel AT ROW 18.38 COL 92 RIGHT-ALIGNED
    td-show-parm AT ROW 19.33 COL 28.2
    fi_file AT ROW 20.24 COL 26.2 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 20.33 COL 92.6 RIGHT-ALIGNED
    tbAutoClose AT ROW 21.48 COL 28.2 WIDGET-ID 78
    btn-ok AT ROW 22.43 COL 28
    btn-cancel AT ROW 22.43 COL 55
    "Cash Needed On:":U VIEW-AS TEXT
    SIZE 24 BY 1 AT ROW 2.1 COL 36.2
    FGCOLOR 9 FONT 5
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 4
    "Available Columns" VIEW-AS TEXT
    SIZE 22 BY .62 AT ROW 10.1 COL 3 WIDGET-ID 38
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 10.1 COL 60.6 WIDGET-ID 44
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 15.52 COL 4
    RECT-6 AT ROW 15.95 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 26.52
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
        TITLE              = "AP Cash Requirements Report"
        HEIGHT             = 22.81
        WIDTH              = 94.8
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
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_sort2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort2:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort2".

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
    rd_sort2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_disc-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* AP Cash Requirements Report */
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
ON WINDOW-CLOSE OF C-Win /* AP Cash Requirements Report */
    DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date-1 C-Win
ON LEAVE OF begin_date-1 IN FRAME FRAME-A /* Date 1 */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date-2 C-Win
ON LEAVE OF begin_date-2 IN FRAME FRAME-A /* Date 2 */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date-3 C-Win
ON LEAVE OF begin_date-3 IN FRAME FRAME-A /* Date 3 */
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
  
        IF rd-dest = 3 THEN
        DO:
            ASSIGN 
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
                            "~"OK"~" to open CSV file?"
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
                    {custom/asifax.i &begin_cust=begin_date-1
                            &END_cust=begin_date-1
                            &fax-subject="Customer List"
                            &fax-body="Customer List"
                            &fax-file=list-name }
                END.
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject="Customer List"
                             &mail-body="Customer List"
                             &mail-file=lv-pdf-file + ".pdf" }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= ''
                                  &END_cust=''
                                  &mail-subject="Customer List"
                                  &mail-body="Customer List"
                                  &mail-file=list-name }
                    END.

                END. 
            WHEN 6 THEN RUN output-to-port.
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

        RUN DisplaySelectionDefault.  /* task 04141407 */ 
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

&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* Name */
    DO:
        DEFINE VARIABLE ls-filename AS cha NO-UNDO.
        DEFINE VARIABLE ll-ok       AS LOG NO-UNDO.

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


&Scoped-define SELF-NAME rd_sort2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort2 C-Win
ON VALUE-CHANGED OF rd_sort2 IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_disc-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_disc-date C-Win
ON VALUE-CHANGED OF tb_disc-date IN FRAME FRAME-A /* Print Discount Date? */
    DO:
        ASSIGN {&self-name}.
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
        begin_date-1 = TODAY
        begin_date-2 = begin_date-1 + 7
        begin_date-3 = begin_date-2 + 7.
    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "VR2" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_date-1.
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
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
            .    
    END.

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
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect) .
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
    DISPLAY begin_company end_company begin_date-1 tb_disc-date begin_date-2 
        begin_date-3 lbl_sort2 rd_sort2 sl_avail sl_selected rd-dest fi_file 
        tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_company end_company begin_date-1 tb_disc-date 
        begin_date-2 begin_date-3 rd_sort2 Btn_Def sl_avail sl_selected 
        Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV 
        tbAutoClose btn-ok btn-cancel 
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
    RUN custom/dprint.w (list-name).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* --------------------------------------------------- ap/ap-flow.p 12/92 cd  */
    /*                                                                            */
    /* a/p - cash requirements report                                             */
    /*                                                                            */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-topw.f}*/
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
    DEFINE BUFFER bap-inv FOR ap-inv .

    {sys/form/r-top5L3.f} 

    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE ws_gross      LIKE ap-inv.net NO-UNDO.
    DEFINE VARIABLE b-comp        AS CHARACTER FORMAT "x(3)" NO-UNDO.
    DEFINE VARIABLE e-comp        AS CHARACTER FORMAT "x(3)" NO-UNDO.
    DEFINE VARIABLE d1            AS DATE      EXTENT 3 FORMAT "99/99/9999" NO-UNDO.
    DEFINE VARIABLE ni            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vend-t        AS DECIMAL   EXTENT 4 FORMAT "->>>>>.99" NO-UNDO.
    DEFINE VARIABLE vend-d        AS DECIMAL   EXTENT 4 FORMAT "->>>>.99" NO-UNDO.
    DEFINE VARIABLE inv-t         AS DECIMAL   FORMAT "->>>>>>9.99" EXTENT 4 NO-UNDO.
    DEFINE VARIABLE inv-d         LIKE vend-d NO-UNDO.
    DEFINE VARIABLE grand-t       LIKE vend-t NO-UNDO.
    DEFINE VARIABLE grand-d       LIKE vend-d NO-UNDO.
    DEFINE VARIABLE s             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ag            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE amt           LIKE ag NO-UNDO.
    DEFINE VARIABLE t1            AS DECIMAL   FORMAT "$->>>,>>>.99" NO-UNDO.
    DEFINE VARIABLE c1            AS DECIMAL   FORMAT "$->>>,>>>.99" NO-UNDO.
    DEFINE VARIABLE m1            AS CHARACTER FORMAT "x(20)" NO-UNDO.
    DEFINE VARIABLE m2            AS CHARACTER FORMAT "x(20)" NO-UNDO.
    DEFINE VARIABLE m3            AS CHARACTER FORMAT "x(20)" NO-UNDO.
    DEFINE VARIABLE first-time    AS LOG       INIT YES NO-UNDO.
    DEFINE VARIABLE ws_disc-avail AS DECIMAL   NO-UNDO COLUMN-LABEL "Disc"
        FORMAT '->>>>.99'.
    DEFINE VARIABLE v-sort        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-disc        AS LOG       INIT NO NO-UNDO.
    DEFINE VARIABLE v-disc-date   AS DATE      NO-UNDO.
    DEFINE VARIABLE v-company     AS LOG       INIT NO NO-UNDO.
    DEFINE VARIABLE excelheader   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE excelheader-2 AS CHARACTER NO-UNDO.


    /*form header
         d1[1] to 50 d1[2] to 72 d1[3] to 94 "Beyond" to 114 "Total" to 137 skip
         "Invoice#     Inv Date/Due Date       Gross     Disc       Gross    "
         "Disc       Gross     Disc       Gross     Disc         Gross     Disc company"
         skip
         fill("_",145) format "x(145)"
    
        with page-top frame b-top stream-io width 145 no-box.
    
    form ap-inv.inv-no
         ap-inv.inv-date format "99/99/99"
         ap-inv.due-date format "99/99/99"
         inv-t[1]
         inv-d[1] format "->>>>.99" 
         inv-t[2]
         inv-d[2] format "->>>>.99" 
         inv-t[3]
         inv-d[3] format "->>>>.99" 
         inv-t[4]
         inv-d[4] format "->>>>.99" 
         ws_gross
         ws_disc-avail
         ap-inv.company FORMAT "x(8)"
    
        with stream-io width 150 frame b no-labels down no-box.*/

    /*form header
         d1[1] to 50 d1[2] to 72 d1[3] to 94 "Beyond" to 114 "Total" to 137 skip
         "Invoice#     Inv Date/Due Date       Gross     Disc       Gross    "
         "Disc       Gross     Disc       Gross     Disc         Gross     Disc"
         skip
         fill("_",137) format "x(137)"
    
        with page-top frame f-top stream-io width 137 no-box.*/

    FORM ap-inv.inv-no
        ap-inv.inv-date FORMAT "99/99/99"
        ap-inv.due-date FORMAT "99/99/99"
        inv-t[1]
        inv-d[1] FORMAT "->>>>.99" 
        inv-t[2]
        inv-d[2] FORMAT "->>>>.99" 
        inv-t[3]
        inv-d[3] FORMAT "->>>>.99" 
        inv-t[4]
        inv-d[4] FORMAT "->>>>.99" 
        ws_gross
        ws_disc-avail

        WITH STREAM-IO WIDTH 137 FRAME a NO-LABELS DOWN NO-BOX.

    EMPTY TEMP-TABLE tt-report.

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        b-comp   = begin_company
        e-comp   = end_company
        d1[1]    = begin_date-1
        d1[2]    = begin_date-2
        d1[3]    = begin_date-3
        v-sort   = rd_sort2
        v-disc   = tb_disc-date
        . 
    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    /*IF tb_excel THEN DO:
      OUTPUT STREAM excel TO VALUE(cFileName).
      IF NOT v-company THEN DO:
      excelheader = "Vendor#,Vendor Name,Invoice#,Inv Date,Due Date,"
                  + STRING(d1[1]) + " Gross," + STRING(d1[1]) + " Disc,"
                  + STRING(d1[2]) + " Gross," + STRING(d1[2]) + " Disc,"
                  + STRING(d1[3]) + " Gross," + STRING(d1[3]) + " Disc,"
                  + "Beyond Gross,Beyond Disc,Gross,Total Disc".
      PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
      END.
      ELSE DO:
           excelheader = "Vendor#,Vendor Name,Invoice#,Inv Date,Due Date,"
                  + STRING(d1[1]) + " Gross," + STRING(d1[1]) + " Disc,"
                  + STRING(d1[2]) + " Gross," + STRING(d1[2]) + " Disc,"
                  + STRING(d1[3]) + " Gross," + STRING(d1[3]) + " Disc,"
                  + "Beyond Gross,Beyond Disc,Gross,Total Disc,Company".
      PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
      END.
    END.*/

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

        IF LOOKUP(ttRptSelected.TextList, "1Gross") <> 0    THEN 
            ASSIGN
                str-tit3      = str-tit3 + string(d1[1]) + "    " 
                excelheader-2 = excelheader-2 + string(d1[1]) + "," .
        ELSE IF LOOKUP(ttRptSelected.TextList, "2Gross") <> 0    THEN 
                ASSIGN
                    str-tit3      = str-tit3 + string(d1[2]) + "    "
                    excelheader-2 = excelheader-2 + string(d1[2]) + ",".
            ELSE IF LOOKUP(ttRptSelected.TextList, "3Gross") <> 0    THEN 
                    ASSIGN
                        str-tit3      = str-tit3 + string(d1[3]) + "    "
                        excelheader-2 = excelheader-2 + string(d1[3]) + "," .
                ELSE
                    ASSIGN
                        str-tit3      = str-tit3 + FILL(" ",ttRptSelected.FieldLength) + " "
                        excelheader-2 = excelheader-2 + " ,"  .


    /*lCountedDateSelected = IF lCountedDateSelected = NO AND ttRptSelected.FieldList = "v-counted-date" THEN YES ELSE NO.
    */    
    END.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        /* excelheader = "Name,Inv Number,FG Item,Prod.Category,Qty shipped,Total MSF," +
                       "$/MSF,Sales Amt,Full Cost,Profit". */
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader-2,',','","') '"' SKIP.
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.

    END. 

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    FOR EACH company WHERE
        company.company GE b-comp AND
        company.company LE e-comp
        NO-LOCK,
        EACH ap-inv      
        WHERE ap-inv.company EQ company.company
        AND ap-inv.posted  EQ YES
        AND ap-inv.due     NE 0
        NO-LOCK,

        FIRST vend
        WHERE vend.company EQ ap-inv.company 
        AND vend.vend-no EQ ap-inv.vend-no
        NO-LOCK:
        {custom/statusMsg.i " 'Processing Invoice#  '  + string(ap-inv.inv-no) "}
        CREATE tt-report.
        ASSIGN
            tt-report.key-01 = IF v-sort EQ "Vendor" THEN vend.vend-no
                         ELSE IF v-sort EQ "Vend Name" THEN vend.name
                         ELSE IF v-sort EQ "Invoice#" THEN ap-inv.inv-no
                         ELSE IF v-sort EQ "Inv Date" THEN STRING((YEAR(ap-inv.inv-date) * 10000) +
                                                       (MONTH(ap-inv.inv-date) * 100)  +
                                                       DAY(ap-inv.inv-date))
                         ELSE ""
            tt-report.key-02 = vend.vend-no
            tt-report.key-03 = ap-inv.inv-no
            tt-report.rec-id = RECID(ap-inv).
    END.
    DISPLAY "" WITH FRAME r-top.
    /*IF v-company THEN
        display "" with frame b-top.
    ELSE
        display "" with frame f-top.*/

    FOR EACH tt-report,
        FIRST ap-inv WHERE RECID(ap-inv) EQ tt-report.rec-id NO-LOCK

        BREAK BY tt-report.key-01
        BY tt-report.key-02
        BY tt-report.key-03:

        {custom/statusMsg.i " 'Processing Invoice#  '  + string(ap-inv.inv-no) "}

        FIND FIRST terms WHERE terms.t-code EQ ap-inv.terms NO-LOCK NO-ERROR.

        FIND FIRST vend
            WHERE vend.company EQ ap-inv.company
            AND vend.vend-no EQ ap-inv.vend-no
            NO-LOCK NO-ERROR.

        /*if first-of (tt-report.key-02) then do:
          put ap-inv.vend-no.
          if avail vend then put vend.name.
          put skip.
        end.*/

        ASSIGN
            ws_gross      = ap-inv.due
            ws_disc-avail = IF ap-inv.net NE 0 THEN
                       (ap-inv.net * (ap-inv.disc-% / 100) - ap-inv.disc-taken)
                     ELSE 0.

        DO i = 1 TO 4:
            ASSIGN
                inv-t[i] = 0
                inv-d[i] = 0.
        END.

        IF ap-inv.due-date GT d1[3] THEN
            ASSIGN
                vend-t[4] = vend-t[4] + ws_gross
                inv-t[4]  = ws_gross.

        ELSE
            IF ap-inv.due-date GT d1[2] THEN
                ASSIGN
                    vend-t[3] = vend-t[3] + ws_gross
                    inv-t[3]  = ws_gross.

            ELSE
                IF ap-inv.due-date GT d1[1] THEN
                    ASSIGN
                        vend-t[2] = vend-t[2] + ws_gross
                        inv-t[2]  = ws_gross.

                ELSE
                    ASSIGN
                        vend-t[1] = vend-t[1] + ws_gross
                        inv-t[1]  = ws_gross.

        v-disc-date = IF AVAILABLE terms THEN
            (ap-inv.inv-date + terms.disc-days) ELSE ap-inv.due-date.

        IF v-disc-date GT d1[3] THEN
            ASSIGN
                vend-d[4] = vend-d[4] + ws_disc-avail
                inv-d[4]  = ws_disc-avail.

        ELSE
            IF v-disc-date GT d1[2] THEN
                ASSIGN
                    vend-d[3] = vend-d[3] + ws_disc-avail
                    inv-d[3]  = ws_disc-avail.

            ELSE
                IF v-disc-date GT d1[1] THEN
                    ASSIGN
                        vend-d[2] = vend-d[2] + ws_disc-avail
                        inv-d[2]  = ws_disc-avail.

                ELSE
                    ASSIGN
                        vend-d[1] = vend-d[1] + ws_disc-avail
                        inv-d[1]  = ws_disc-avail.
        /*IF NOT v-company THEN do:
          display ap-inv.inv-no
                  ap-inv.inv-date
                  ap-inv.due-date
                    ap-inv.inv-date + ap-inv.disc-days
                      when ws_disc-avail ne 0 and v-disc @ ap-inv.due-date
                  inv-t[1] when inv-t[1] ne 0
                  inv-d[1] when inv-d[1] ne 0
                  inv-t[2] when inv-t[2] ne 0
                  inv-d[2] when inv-d[2] ne 0
                  inv-t[3] when inv-t[3] ne 0
                  inv-d[3] when inv-d[3] ne 0
                  inv-t[4] when inv-t[4] ne 0
                  inv-d[4] when inv-d[4] ne 0
                  ws_gross
                  ws_disc-avail
      
              with frame a.
          down with frame a.
        END. 
         ELSE do:
          display ap-inv.inv-no
                  ap-inv.inv-date
                  ap-inv.due-date
                    ap-inv.inv-date + ap-inv.disc-days
                      when ws_disc-avail ne 0 and v-disc @ ap-inv.due-date
                  inv-t[1] when inv-t[1] ne 0
                  inv-d[1] when inv-d[1] ne 0
                  inv-t[2] when inv-t[2] ne 0
                  inv-d[2] when inv-d[2] ne 0
                  inv-t[3] when inv-t[3] ne 0
                  inv-d[3] when inv-d[3] ne 0
                  inv-t[4] when inv-t[4] ne 0
                  inv-d[4] when inv-d[4] ne 0
                  ws_gross
                  ws_disc-avail
                  ap-inv.company
      
              with frame b.
          down with frame b.
         END.*/

        /*IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
              '"' IF first-of(tt-report.key-02) THEN
                     ap-inv.vend-no ELSE ""                                    '",'
              '"' IF FIRST-OF(tt-report.key-02) AND    
                     avail vend then vend.NAME ELSE ""                         '",'
              '"' ap-inv.inv-no                                                '",'
              '"' IF ap-inv.inv-date NE ? THEN
                     STRING(ap-inv.inv-date) ELSE ""                           '",'
              '"' IF ws_disc-avail ne 0 and v-disc THEN
                     STRING(ap-inv.inv-date + ap-inv.disc-days)               
                  ELSE IF ap-inv.due-date NE ? THEN
                       STRING(ap-inv.due-date) ELSE ""                         '",'
              '"' IF inv-t[1] ne 0 THEN STRING(inv-t[1],"->>>>>>9.99") ELSE "" '",'
              '"' IF inv-d[1] ne 0 THEN STRING(inv-d[1],"->>>>.99") ELSE ""    '",'
              '"' IF inv-t[2] ne 0 THEN STRING(inv-t[2],"->>>>>>9.99") ELSE "" '",'
              '"' IF inv-d[2] ne 0 THEN STRING(inv-d[2],"->>>>.99") ELSE ""    '",'
              '"' IF inv-t[3] ne 0 THEN STRING(inv-t[3],"->>>>>>9.99") ELSE "" '",'
              '"' IF inv-d[3] ne 0 THEN STRING(inv-d[3],"->>>>.99") ELSE ""    '",'
              '"' IF inv-t[4] ne 0 THEN STRING(inv-t[4],"->>>>>>9.99") ELSE "" '",'
              '"' IF inv-d[4] ne 0 THEN STRING(inv-d[4],"->>>>.99") ELSE ""    '",'
              '"' STRING(ws_gross,"->,>>>,>>9.99")                             '",'
              '"' STRING(ws_disc-avail,'->>>>.99')                             '",'
              '"' IF v-company THEN STRING(ap-inv.company) ELSE ""             '",'
              SKIP.*/



        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".

        BUFFER bap-inv:FIND-BY-ROWID(ROWID(ap-inv), NO-LOCK) .
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            IF INDEX(cTmpField,".") > 0 THEN 
            DO:
                cFieldName = cTmpField.
                cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                hField = BUFFER bap-inv:BUFFER-FIELD(cTmpField).
                IF hField <> ? THEN 
                DO:                 
                    cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                    cDisplay = cDisplay + 
                        IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldType) = "C" THEN
                        (cTmpField + FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)))
                        ELSE IF LENGTH(cTmpField) <  int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) THEN
                        (FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) - LENGTH(cTmpField)) + cTmpField) + " "
                        ELSE cTmpField.
                    cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,GetFieldValue(hField))) + ",".   

                END.
                ELSE 
                DO:
                    cTmpField = SUBSTRING(cFieldName,1,int( ENTRY( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + quoter(" ") + ",".
                END.
            END.
            ELSE 
            DO: 
                CASE cTmpField:                                           
                    WHEN "vend-name" THEN 
                        cVarValue = IF AVAILABLE vend THEN STRING(vend.name,"x(30)") ELSE "" .
                    WHEN "due-date" THEN 
                        cVarValue = IF ws_disc-avail NE 0 AND v-disc THEN STRING(ap-inv.inv-date + ap-inv.disc-days)  ELSE IF ap-inv.due-date NE ? THEN STRING(ap-inv.due-date) ELSE "" .
                    WHEN "inv-t1" THEN 
                        cVarValue = IF inv-t[1] NE 0 THEN  STRING(inv-t[1],"->>>>>>9.99") ELSE ""  .
                    WHEN "inv-d1" THEN 
                        cVarValue = IF inv-d[1] NE 0 THEN  STRING(inv-d[1],"->>>>.99") ELSE "" .  
                    WHEN "inv-t2" THEN 
                        cVarValue = IF inv-t[2] NE 0 THEN  STRING(inv-t[2],"->>>>>>9.99") ELSE "". 
                    WHEN "inv-d2" THEN 
                        cVarValue = IF inv-d[2] NE 0 THEN  STRING(inv-d[2],"->>>>.99") ELSE ""    .
                    WHEN "inv-t3" THEN 
                        cVarValue = IF inv-t[3] NE 0 THEN  STRING(inv-t[3],"->>>>>>9.99") ELSE ""  .
                    WHEN "inv-d3" THEN 
                        cVarValue = IF inv-d[3] NE 0 THEN  STRING(inv-d[3],"->>>>.99") ELSE "" .   
                    WHEN "inv-t4" THEN 
                        cVarValue = IF inv-t[4] NE 0 THEN  STRING(inv-t[4],"->>>>>>9.99") ELSE "" .
                    WHEN "inv-d4" THEN 
                        cVarValue = IF inv-d[4] NE 0 THEN  STRING(inv-d[4],"->>>>.99") ELSE "" .    
                    WHEN "ws_gross" THEN 
                        cVarValue      = STRING(ws_gross,"->,>>>,>>9.99")  .
                    WHEN "ws_disc-avail" THEN 
                        cVarValue = STRING(ws_disc-avail,'->>>>>>.99')  .

                    WHEN "terms" THEN 
                        cVarValue = IF AVAILABLE vend THEN vend.terms ELSE "" . 
                    WHEN "dy-old" THEN 
                        cVarValue =  STRING(TODAY - ap-inv.inv-date ,">>>>>>>9") .
                    WHEN "cc-ach" THEN 
                        cVarValue = IF AVAILABLE vend AND vend.payment-type EQ "ACH" THEN "Yes" ELSE "No". 

                END CASE.
                cExcelVarValue = DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,cVarValue).  
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
            END.
        END.
        PUT UNFORMATTED cDisplay SKIP.
        IF tb_excel THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                cExcelDisplay SKIP.
        END.

        IF LAST-OF(tt-report.key-02) THEN 
        DO:
            /* IF NOT v-company THEN do:
             display "       *" @ ap-inv.due-date
                   vend-t[1]  @ inv-t[1]
                   vend-d[1]  @ inv-d[1]
                   vend-t[2]  @ inv-t[2]
                   vend-d[2]  @ inv-d[2]
                   vend-t[3]  @ inv-t[3]
                   vend-d[3]  @ inv-d[3]
                   vend-t[4]  @ inv-t[4]
                   vend-d[4]  @ inv-d[4]
                   vend-t[1] + vend-t[2] + vend-t[3] + vend-t[4] @ ws_gross
                   vend-d[1] + vend-d[2] + vend-d[3] + vend-d[4] @ ws_disc-avail
     
               with frame a.
           down 2 with frame a.
             END.
           ELSE do:
           display "       *" @ ap-inv.due-date
                   vend-t[1]  @ inv-t[1]
                   vend-d[1]  @ inv-d[1]
                   vend-t[2]  @ inv-t[2]
                   vend-d[2]  @ inv-d[2]
                   vend-t[3]  @ inv-t[3]
                   vend-d[3]  @ inv-d[3]
                   vend-t[4]  @ inv-t[4]
                   vend-d[4]  @ inv-d[4]
                   vend-t[1] + vend-t[2] + vend-t[3] + vend-t[4] @ ws_gross
                   vend-d[1] + vend-d[2] + vend-d[3] + vend-d[4] @ ws_disc-avail
     
               with frame b.
           down 2 with frame b.
           END.*/

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            BUFFER bap-inv:FIND-BY-ROWID(ROWID(ap-inv), NO-LOCK) .
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

                CASE cTmpField:                                          
                    WHEN "ap-inv.vend-no" THEN 
                        cVarValue =  "*" .
                    WHEN "vend-name" THEN 
                        cVarValue =  "" .
                    WHEN "ap-inv.inv-no" THEN 
                        cVarValue =  "" .
                    WHEN "ap-inv.inv-date" THEN 
                        cVarValue =  "" .
                    WHEN "due-date" THEN 
                        cVarValue =  "" .
                    WHEN "ap-inv.company" THEN 
                        cVarValue =  "" .
                    WHEN "inv-t1" THEN 
                        cVarValue =  STRING(vend-t[1],"->>>>>>9.99")   .
                    WHEN "inv-d1" THEN 
                        cVarValue =  STRING(vend-d[1],"->>>>.99")  .  
                    WHEN "inv-t2" THEN 
                        cVarValue =  STRING(vend-t[2],"->>>>>>9.99") . 
                    WHEN "inv-d2" THEN 
                        cVarValue =  STRING(vend-d[2],"->>>>.99")     .
                    WHEN "inv-t3" THEN 
                        cVarValue =  STRING(vend-t[3],"->>>>>>9.99")   .
                    WHEN "inv-d3" THEN 
                        cVarValue =  STRING(vend-d[3],"->>>>.99")  .   
                    WHEN "inv-t4" THEN 
                        cVarValue =  STRING(vend-t[4],"->>>>>>9.99")  .
                    WHEN "inv-d4" THEN 
                        cVarValue =  STRING(vend-d[4],"->>>>.99") .    
                    WHEN "ws_gross" THEN 
                        cVarValue      = STRING(vend-t[1] + vend-t[2] + vend-t[3] + vend-t[4],"->,>>>,>>9.99")  .
                    WHEN "ws_disc-avail" THEN 
                        cVarValue = STRING(vend-d[1] + vend-d[2] + vend-d[3] + vend-d[4],'->>>>>>.99')  .

                    WHEN "terms" THEN 
                        cVarValue =  "" . 
                    WHEN "dy-old" THEN 
                        cVarValue = "" . 
                    WHEN "cc-ach" THEN 
                        cVarValue = "" . 

                END CASE.
                cExcelVarValue = DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,cVarValue).  
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 

            END.
            PUT  UNFORMATTED 
                "    *" SUBSTRING(cDisplay,6,400) SKIP(1).
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP(1).
            END.

            DO i = 1 TO 4:
                ASSIGN
                    grand-t[i] = grand-t[i] + vend-t[i]
                    grand-d[i] = grand-d[i] + vend-d[i]

                    vend-t[i]  = 0
                    vend-d[i]  = 0.
            END.
        END.  /* last-of loop */

        IF LAST(tt-report.key-02) THEN 
        DO:
            /*IF NOT v-company THEN DO:
            down 1 with frame a.
      
            display "      **" @ ap-inv.due-date
                    grand-t[1] @ inv-t[1]
                    grand-d[1] @ inv-d[1]
                    grand-t[2] @ inv-t[2]
                    grand-d[2] @ inv-d[2]
                    grand-t[3] @ inv-t[3]
                    grand-d[3] @ inv-d[3]
                    grand-t[4] @ inv-t[4]
                    grand-d[4] @ inv-d[4]
                    grand-t[1] + grand-t[2] + grand-t[3] + grand-t[4] @ ws_gross
                    grand-d[1] + grand-d[2] + grand-d[3] + grand-d[4] @ ws_disc-avail
      
                with frame a.
            END.
            ELSE do:
            down 1 with frame b.
      
            display "      **" @ ap-inv.due-date
                    grand-t[1] @ inv-t[1]
                    grand-d[1] @ inv-d[1]
                    grand-t[2] @ inv-t[2]
                    grand-d[2] @ inv-d[2]
                    grand-t[3] @ inv-t[3]
                    grand-d[3] @ inv-d[3]
                    grand-t[4] @ inv-t[4]
                    grand-d[4] @ inv-d[4]
                    grand-t[1] + grand-t[2] + grand-t[3] + grand-t[4] @ ws_gross
                    grand-d[1] + grand-d[2] + grand-d[3] + grand-d[4] @ ws_disc-avail
      
                with frame b.
            END.*/

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            BUFFER bap-inv:FIND-BY-ROWID(ROWID(ap-inv), NO-LOCK) .
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

                CASE cTmpField:                                          
                    WHEN "ap-inv.vend-no" THEN 
                        cVarValue =  "" .
                    WHEN "vend-name" THEN 
                        cVarValue =  "" .
                    WHEN "ap-inv.inv-no" THEN 
                        cVarValue =  "" .
                    WHEN "ap-inv.inv-date" THEN 
                        cVarValue =  "" .
                    WHEN "due-date" THEN 
                        cVarValue =  "" .
                    WHEN "ap-inv.company" THEN 
                        cVarValue =  "" .
                    WHEN "inv-t1" THEN 
                        cVarValue =  STRING(grand-t[1],"->>>>>>9.99")   .
                    WHEN "inv-d1" THEN 
                        cVarValue =  STRING(grand-d[1],"->>>>.99")  .  
                    WHEN "inv-t2" THEN 
                        cVarValue =  STRING(grand-t[2],"->>>>>>9.99"). 
                    WHEN "inv-d2" THEN 
                        cVarValue =  STRING(grand-d[2],"->>>>.99")    .
                    WHEN "inv-t3" THEN 
                        cVarValue =  STRING(grand-t[3],"->>>>>>9.99")   .
                    WHEN "inv-d3" THEN 
                        cVarValue =  STRING(grand-d[3],"->>>>.99")  .   
                    WHEN "inv-t4" THEN 
                        cVarValue =  STRING(grand-t[4],"->>>>>>9.99")  .
                    WHEN "inv-d4" THEN 
                        cVarValue =  STRING(grand-d[4],"->>>>.99") .    
                    WHEN "ws_gross" THEN 
                        cVarValue      = STRING(grand-t[1] + grand-t[2] + grand-t[3] + grand-t[4],"->,>>>,>>9.99")  .
                    WHEN "ws_disc-avail" THEN 
                        cVarValue = STRING(grand-d[1] + grand-d[2] + grand-d[3] + grand-d[4],'->>>>>>.99')  .

                    WHEN "terms" THEN 
                        cVarValue =  "" . 
                    WHEN "dy-old" THEN 
                        cVarValue = "" . 
                    WHEN "cc-ach" THEN 
                        cVarValue =  "" . 

                END CASE.
                cExcelVarValue = DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,cVarValue).  
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 

            END.
            PUT  UNFORMATTED 
                "    **" SUBSTRING(cDisplay,7,400) SKIP(1).
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
            END.



        END.
    END. /* for each */

    IF tb_excel THEN 
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
            fi_file:SCREEN-VALUE = "c:\tmp\APCashRequirements.csv".    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
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

