&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-saprod.w

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

DEFINE VARIABLE v-tot-msf AS DECIMAL FORMAT "->,>>>,>>9.99".
DEFINE VARIABLE v-sq-ft   LIKE itemfg.t-sqft FORMAT "->>>,>>9.999".

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.

DEFINE TEMP-TABLE w-data NO-UNDO
    FIELD w-procat   LIKE fgcat.procat
    FIELD w-sqft     LIKE v-sq-ft
    FIELD w-amt      LIKE ar-inv.gross
    FIELD w-msf      LIKE v-tot-msf
    FIELD w-ptd-sqft LIKE v-sq-ft
    FIELD w-ptd-amt  LIKE ar-inv.gross
    FIELD w-ptd-msf  LIKE v-tot-msf
    FIELD w-ytd-sqft LIKE v-sq-ft
    FIELD w-ytd-amt  LIKE ar-inv.gross
    FIELD w-ytd-msf  LIKE v-tot-msf.

DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.

DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.

ASSIGN 
    cTextListToSelect  = "Catgy,Dly Sq Ft/M,Dly Amount,Dly $/MSF,"
              + "PTD Sq Ft/M,PTD Amount,PTD $/MSF,YTD Sq Ft/M,YTD Amount,YTD $/MSF"
    cFieldListToSelect = "cat,dly-sf,dly-amt,dly-msf," +
                            "ptd-sf,ptd-amt,ptd-msf,ytd-sf,ytd-amt,ytd-msf"
    cFieldLength       = "5,12,13,13," + "12,13,13,12,13,13" 
    cFieldType         = "c,i,i,i," + "i,i,i,i,i,i"
    .



{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Catgy,Dly Sq Ft/M,Dly Amount,Dly $/MSF,"
              + "PTD Sq Ft/M,PTD Amount,PTD $/MSF,YTD Sq Ft/M,YTD Amount,YTD $/MSF"     .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 inv-date tb_fin-chg ~
tb_misc-sal sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up btn_down ~
rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS inv-date tb_fin-chg tb_misc-sal sl_avail ~
sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 

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

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\SalesByProduct.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE inv-date       AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Invoice Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

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
    SIZE 16 BY 4.86 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.71.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 4.48.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.95 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.95 NO-UNDO.

DEFINE VARIABLE tbAutoClose  AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_fin-chg   AS LOGICAL   INITIAL NO 
    LABEL "Include Finance Charges?" 
    VIEW-AS TOGGLE-BOX
    SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE tb_misc-sal  AS LOGICAL   INITIAL NO 
    LABEL "Break Out Misc Sales?" 
    VIEW-AS TOGGLE-BOX
    SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.5 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    inv-date AT ROW 2.24 COL 45 COLON-ALIGNED
    tb_fin-chg AT ROW 3.81 COL 64 RIGHT-ALIGNED
    tb_misc-sal AT ROW 4.76 COL 34
    sl_avail AT ROW 6.81 COL 4 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 6.81 COL 60.6 NO-LABELS WIDGET-ID 28
    Btn_Def AT ROW 6.91 COL 40.6 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    Btn_Add AT ROW 8.05 COL 40.6 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 9.24 COL 40.6 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 10.43 COL 40.6 WIDGET-ID 40
    btn_down AT ROW 11.62 COL 40.6 WIDGET-ID 42
    lv-ornt AT ROW 13.62 COL 29 NO-LABELS
    lines-per-page AT ROW 13.62 COL 82 COLON-ALIGNED
    rd-dest AT ROW 13.91 COL 6 NO-LABELS
    lv-font-no AT ROW 14.24 COL 32 COLON-ALIGNED
    tb_excel AT ROW 14.57 COL 74 RIGHT-ALIGNED
    lv-font-name AT ROW 15.19 COL 26 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 16.71 COL 28.6
    fi_file AT ROW 17.67 COL 26.6 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 17.76 COL 92.3 RIGHT-ALIGNED
    tbAutoClose AT ROW 19.1 COL 28.6 WIDGET-ID 78
    btn-ok AT ROW 20.05 COL 28.6
    btn-cancel AT ROW 20.05 COL 48.6
    "Available Columns" VIEW-AS TEXT
    SIZE 20 BY .62 AT ROW 6.05 COL 4.6 WIDGET-ID 38
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 6.1 COL 60.4 WIDGET-ID 44
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 13.05 COL 5
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    RECT-6 AT ROW 13.38 COL 4
    RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 21.1
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
        TITLE              = "Sales Analysis - Sales By Product"
        HEIGHT             = 21.1
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
   FRAME-NAME                                                           */
ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    inv-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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

/* SETTINGS FOR TOGGLE-BOX tb_fin-chg IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_fin-chg:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_misc-sal:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Sales Analysis - Sales By Product */
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
ON WINDOW-CLOSE OF C-Win /* Sales Analysis - Sales By Product */
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
                            OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
                        END.
                    END.
                END. /* WHEN 3 THEN DO: */
            WHEN 4 THEN 
                DO:
           /*run output-to-fax.*/
                    {custom/asifax.i &type= ''
                            &begin_cust= inv-date
                            &END_cust= inv-date
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = ''
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = ''
                                  &begin_cust=''
                                  &END_cust=''
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }
                    END.
                END.
        END CASE. 

        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.  
        SESSION:SET-WAIT-STATE("").
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


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* Name */
DO:
   DEF VAR ls-filename AS CHARACTER NO-UNDO.
   DEF VAR ll-ok AS LOG NO-UNDO.

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


&Scoped-define SELF-NAME inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-date C-Win
ON LEAVE OF inv-date IN FRAME FRAME-A /* Invoice Date */
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


&Scoped-define SELF-NAME tb_fin-chg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fin-chg C-Win
ON VALUE-CHANGED OF tb_fin-chg IN FRAME FRAME-A /* Include Finance Charges? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_misc-sal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_misc-sal C-Win
ON VALUE-CHANGED OF tb_misc-sal IN FRAME FRAME-A /* Break Out Misc Sales? */
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
    {sys/inc/reportsConfigNK1.i "HF" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
RUN DisplaySelectionList2.
ASSIGN
    inv-date:SCREEN-VALUE = STRING(TODAY).
APPLY "entry" TO inv-date.
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
    DISPLAY inv-date tb_fin-chg tb_misc-sal sl_avail sl_selected rd-dest fi_file 
        tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 inv-date tb_fin-chg tb_misc-sal sl_avail sl_selected 
        Btn_Def Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /***************************************************************************\
    *****************************************************************************
    **  Program: /u2/fold/all/dev/asi/sa/sa-
    **       By: David Reimer, Updated by Chris Heins 9507
    ** Descript: Print summary of sales by product code.
    **
    *****************************************************************************
    \***************************************************************************/

    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE fdate           AS DATE      FORMAT "99/99/9999" NO-UNDO.
    DEFINE VARIABLE tdate           AS DATE      FORMAT "99/99/9999" NO-UNDO.
    DEFINE VARIABLE v-ytd           AS LOG       NO-UNDO.
    DEFINE VARIABLE v-inc-fc        AS LOG       INIT NO NO-UNDO.
    DEFINE VARIABLE v-misc          AS LOG       INIT NO NO-UNDO.

    DEFINE VARIABLE v-mtot-sqft     LIKE v-sq-ft NO-UNDO.
    DEFINE VARIABLE v-mtot-amt      LIKE ar-inv.gross NO-UNDO.
    DEFINE VARIABLE v-mtot-msf      LIKE v-tot-msf NO-UNDO.
    DEFINE VARIABLE v-mtot-ptd-sqft LIKE v-mtot-sqft NO-UNDO.
    DEFINE VARIABLE v-mtot-ptd-amt  LIKE v-mtot-amt NO-UNDO.
    DEFINE VARIABLE v-mtot-ptd-msf  LIKE v-mtot-msf NO-UNDO.
    DEFINE VARIABLE v-mtot-ytd-sqft LIKE v-mtot-sqft NO-UNDO.
    DEFINE VARIABLE v-mtot-ytd-amt  LIKE v-mtot-amt NO-UNDO.
    DEFINE VARIABLE v-mtot-ytd-msf  LIKE v-mtot-msf NO-UNDO.

    DEFINE VARIABLE v-gtot-sqft     LIKE v-sq-ft NO-UNDO.
    DEFINE VARIABLE v-gtot-amt      LIKE ar-inv.gross NO-UNDO.
    DEFINE VARIABLE v-gtot-msf      LIKE v-tot-msf NO-UNDO.
    DEFINE VARIABLE v-gtot-ptd-sqft LIKE v-gtot-sqft NO-UNDO.
    DEFINE VARIABLE v-gtot-ptd-amt  LIKE v-gtot-amt NO-UNDO.
    DEFINE VARIABLE v-gtot-ptd-msf  LIKE v-gtot-msf NO-UNDO.
    DEFINE VARIABLE v-gtot-ytd-sqft LIKE v-gtot-sqft NO-UNDO.
    DEFINE VARIABLE v-gtot-ytd-amt  LIKE v-gtot-amt NO-UNDO.
    DEFINE VARIABLE v-gtot-ytd-msf  LIKE v-gtot-msf NO-UNDO.

    DEFINE VARIABLE v-grtot-amt     LIKE v-gtot-amt NO-UNDO.
    DEFINE VARIABLE v-grtot-ptd-amt LIKE v-gtot-amt NO-UNDO.
    DEFINE VARIABLE v-grtot-ytd-amt LIKE v-gtot-amt NO-UNDO.

    DEFINE VARIABLE v-procat        LIKE fgcat.procat NO-UNDO.
    DEFINE VARIABLE v-sqft          LIKE itemfg.t-sqft NO-UNDO.
    DEFINE VARIABLE v-amt           LIKE ar-inv.gross NO-UNDO.
    DEFINE VARIABLE dPtdAmt         LIKE ar-inv.gross NO-UNDO.
    DEFINE VARIABLE dfriAmt         LIKE ar-inv.gross NO-UNDO.
    DEFINE VARIABLE v-msf           LIKE v-tot-msf NO-UNDO.
    DEFINE VARIABLE v-ytd-first     AS DATE      NO-UNDO.
    DEFINE VARIABLE v-ytd-last      AS DATE      NO-UNDO.
    DEFINE VARIABLE v-ptd-first     AS DATE      NO-UNDO.
    DEFINE VARIABLE v-ptd-last      AS DATE      NO-UNDO.
    DEFINE VARIABLE v-period        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-year          AS INTEGER   FORMAT "9999" NO-UNDO.
    DEFINE VARIABLE v-qty           LIKE ar-invl.ship-qty FORMAT "->>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-fac           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE eps             AS DECIMAL   NO-UNDO INIT .001.
    DEFINE VARIABLE abs-sqft        AS DECIMAL   NO-UNDO FORMAT "->>>>>>>>>.99999999".

    DEFINE VARIABLE v-ytd-hdr       AS CHARACTER FORMAT "x(40)" INIT
        "--------------Year to Date--------------"
        NO-UNDO.

    DEFINE VARIABLE v-hdr1          AS CHARACTER FORMAT "x(10)" INIT "Category  " NO-UNDO.
    DEFINE VARIABLE v-hdr1-1        AS CHARACTER EXTENT 3 FORMAT "x(12)" INIT "     Sq Ft/M" NO-UNDO.
    DEFINE VARIABLE v-hdr1-2        AS CHARACTER EXTENT 3 FORMAT "x(13)" INIT "       Amount" NO-UNDO.
    DEFINE VARIABLE v-hdr1-3        AS CHARACTER EXTENT 3 FORMAT "x(13)" INIT "        $/MSF" NO-UNDO.
    DEFINE VARIABLE v-hdr2          AS CHARACTER FORMAT "x(10)" INIT "----------" NO-UNDO.
    DEFINE VARIABLE v-hdr2-1        AS CHARACTER EXTENT 3 FORMAT "x(12)" INIT "------------" NO-UNDO.
    DEFINE VARIABLE v-hdr2-2        AS CHARACTER EXTENT 3 FORMAT "x(13)" INIT "-------------" NO-UNDO.
    DEFINE VARIABLE v-hdr2-3        AS CHARACTER EXTENT 3 FORMAT "x(13)" INIT "-------------" NO-UNDO.

    DEFINE VARIABLE cDisplay        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelDisplay   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField          AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelVarValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedList   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str-tit4        AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5        AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line        AS cha       FORM "x(350)" NO-UNDO.
    DEFINE VARIABLE dTax            AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dFreight        AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dTax-ptd        AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dFreight-ptd    AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dTax-ytd        AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dFreight-ytd    AS DECIMAL   NO-UNDO.

    {sys/form/r-top5DL3.f}
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.

    FORM
        w-procat      FORMAT "x(10)"
        w-sqft
        w-amt
        w-msf
        w-ptd-sqft
        w-ptd-amt
        w-ptd-msf
        w-ytd-sqft
        w-ytd-amt
        w-ytd-msf


        "-----------------Daily------------------"
        "-------------Period to Date-------------"
        v-ytd-hdr 
        v-hdr1
        v-hdr1-1[1]
        v-hdr1-2[1]
        v-hdr1-3[1]
        v-hdr1-1[2]
        v-hdr1-2[2]
        v-hdr1-3[2]
        v-hdr1-1[3]
        v-hdr1-2[3]
        v-hdr1-3[3] 
        v-hdr2
        v-hdr2-1[1]
        v-hdr2-2[1]
        v-hdr2-3[1]
        v-hdr2-1[2]
        v-hdr2-2[2]
        v-hdr2-3[2]
        v-hdr2-1[3]
        v-hdr2-2[3]
        v-hdr2-3[3] 

        WITH FRAME itemx NO-BOX NO-UNDERLINE NO-LABELS DOWN STREAM-IO WIDTH 144. 


    SESSION:SET-WAIT-STATE ("general").

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        tdate    = inv-date
        /*v-ytd        = tb_ytd*/
        v-inc-fc = tb_fin-chg
        v-misc   = tb_misc-sal.

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

        IF LOOKUP(ttRptSelected.TextList, "Dly Sq Ft/M,Dly Amount,Dly $/MSF,PTD Sq Ft/M,PTD Amount,PTD $/MSF,YTD Sq Ft/M,YTD Amount,YTD $/MSF") <> 0    THEN
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
        /* excelheader = "Category,Daily Sq Ft/M,Daily Amount,Daily $/MSF,"
                     + "PTD Sq Ft/M,PTD Amount,PTD $/MSF,YTD Sq Ft/M,YTD Amount,"
                     + "YTD $/MSF". */
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.

    DISPLAY "" WITH FRAME r-top.

    FIND FIRST period
        WHERE period.company EQ cocode
        AND period.pst     LE tdate
        AND period.pend    GE tdate
        NO-LOCK.
    ASSIGN
        v-period    = period.pnum
        v-year      = period.yr
        v-ptd-first = period.pst
        v-ptd-last  = period.pend.

    FIND FIRST period
        WHERE period.company EQ cocode
        AND period.yr      EQ v-year
        NO-LOCK.

    ASSIGN
        v-ytd-first = v-ptd-first
        v-ytd-last  = tdate.

    FOR EACH cust WHERE cust.company EQ cocode NO-LOCK:
        FOR EACH ar-inv
            WHERE ar-inv.company  EQ cocode
            AND ar-inv.posted   EQ YES
            AND ar-inv.cust-no  EQ cust.cust-no
            AND ar-inv.inv-date GE v-ytd-first
            AND ar-inv.inv-date LE v-ytd-last
            AND (ar-inv.type    NE "FC" OR v-inc-fc)
            NO-LOCK,

            EACH ar-invl
            WHERE ar-invl.x-no EQ ar-inv.x-no
            AND (ar-invl.billable OR NOT ar-invl.misc)
            NO-LOCK BREAK BY ar-inv.inv-no :

            CREATE tt-report.

            ASSIGN
                tt-report.term-id = ""
                tt-report.rec-id  = RECID(ar-invl)
                tt-report.key-01  = "MISC"
                tt-report.key-10  = "ar-invl".
        
            IF FIRST-OF(ar-inv.inv-no) THEN 
            DO:
                ASSIGN 
                    dTax-ytd     = dTax-ytd + ar-inv.tax-amt 
                    dFreight-ytd = dFreight-ytd + ar-inv.freight .

                IF ar-inv.inv-date GE v-ptd-first AND
                    ar-inv.inv-date LE v-ptd-last  THEN
                    ASSIGN
                        dTax-ptd     = dTax-ptd + ar-inv.tax-amt 
                        dFreight-ptd = dFreight-ptd + ar-inv.freight.

                IF tdate EQ ar-inv.inv-date THEN
                    ASSIGN dTax     = dTax + ar-inv.tax-amt 
                        dFreight = dFreight + ar-inv.freight .
            END.

            IF NOT ar-invl.misc THEN 
            DO:

                RELEASE itemfg.

                IF ar-invl.i-no NE "" THEN
                    FIND FIRST itemfg
                        WHERE itemfg.company EQ cocode
                        AND itemfg.i-no    EQ ar-invl.i-no
                        NO-LOCK NO-ERROR.

                IF AVAILABLE itemfg THEN tt-report.key-01 = itemfg.procat.

                ELSE 
                DO:
                    FIND FIRST fgcat
                        WHERE fgcat.company EQ cocode
                        AND fgcat.glacc   EQ ar-invl.actnum
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE fgcat THEN tt-report.key-01 = fgcat.procat.
                END.
            END.

            tt-report.key-02 = IF v-misc AND tt-report.key-01 EQ "MISC"
                THEN ar-invl.actnum ELSE tt-report.key-01.
        END.

        FOR EACH ar-cash
            WHERE ar-cash.company    EQ cocode
            AND ar-cash.cust-no    EQ cust.cust-no
            AND ar-cash.check-date GE v-ytd-first
            AND ar-cash.check-date LE v-ytd-last
            AND ar-cash.posted     EQ YES
            NO-LOCK,

            EACH ar-cashl
            WHERE ar-cashl.c-no    EQ ar-cash.c-no
            AND ar-cashl.posted  EQ YES
            AND ar-cashl.memo    EQ YES
            AND CAN-FIND(FIRST account
            WHERE account.company EQ ar-cashl.company
            AND account.actnum  EQ ar-cashl.actnum
            AND account.type    EQ "R")
            NO-LOCK:

            CREATE tt-report.

            ASSIGN
                tt-report.term-id = ""
                tt-report.key-01  = "MEMO"
                tt-report.key-02  = ar-cashl.actnum
                tt-report.key-10  = "ar-cashl"
                tt-report.rec-id  = RECID(ar-cashl).

            RELEASE itemfg.

            RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

            IF AVAILABLE reftable                      OR
                ar-cashl.dscr MATCHES "*OE RETURN*" THEN 
            DO:


                RELEASE itemfg.

                IF AVAILABLE oe-retl AND oe-retl.i-no NE "" THEN
                    FIND FIRST itemfg
                        WHERE itemfg.company EQ cocode
                        AND itemfg.i-no    EQ oe-retl.i-no
                        NO-LOCK NO-ERROR.

                tt-report.key-01 = IF AVAILABLE itemfg THEN itemfg.procat ELSE "MISC".
            END.

            tt-report.key-02 = IF v-misc AND tt-report.key-01 EQ "MISC"
                THEN ar-cashl.actnum ELSE tt-report.key-01.
        END.
    END.

    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
        AND tt-report.key-01  NE "MISC"
        AND tt-report.key-01  NE "MEMO"

        {sa/sa-sprdN.i g}

    ASSIGN
        v-gtot-msf     = (IF v-gtot-sqft NE 0 THEN v-gtot-amt / v-gtot-sqft
                                           ELSE 0)
        v-gtot-ptd-msf = (IF v-gtot-ptd-sqft NE 0 THEN
                         v-gtot-ptd-amt / v-gtot-ptd-sqft ELSE 0)
        v-gtot-ytd-msf = (IF v-gtot-ytd-sqft NE 0 THEN
                         v-gtot-ytd-amt / v-gtot-ytd-sqft ELSE 0).

  
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
            WHEN "cat"      THEN 
                cVarValue = "" .
            WHEN "dly-sf"   THEN 
                cVarValue = STRING(v-gtot-sqft,"->>>,>>9.999").       
            WHEN "dly-amt"  THEN 
                cVarValue = STRING(v-gtot-amt,"->,>>>,>>9.99") .
            WHEN "dly-msf"  THEN 
                cVarValue = STRING(v-gtot-msf,"->,>>>,>>9.99") .     
            WHEN "ptd-sf"   THEN 
                cVarValue = STRING(v-gtot-ptd-sqft,"->>>,>>9.999") .
            WHEN "ptd-amt"  THEN 
                cVarValue = STRING(v-gtot-ptd-amt,"->,>>>,>>9.99") .
            WHEN "ptd-msf"  THEN 
                cVarValue = STRING(v-gtot-ptd-msf,"->,>>>,>>9.99") .
            WHEN "ytd-sf"   THEN 
                cVarValue = STRING(v-gtot-ytd-sqft,"->>>,>>9.999") .
            WHEN "ytd-amt"  THEN 
                cVarValue = STRING(v-gtot-ytd-amt,"->,>>>,>>9.99") .  
            WHEN "ytd-msf"  THEN 
                cVarValue = STRING(v-gtot-ytd-msf,"->,>>>,>>9.99") .

        END CASE.

        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.

    PUT UNFORMATTED 
        "  SALES " SUBSTRING(cDisplay,9,350) SKIP.
    IF tb_excel THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            "  SALES " + substring(cExcelDisplay,3,300) SKIP.
    END.

    FIND FIRST tt-report
        WHERE tt-report.term-id EQ ""
        AND tt-report.key-01  EQ "MISC"
        NO-LOCK NO-ERROR.

    IF AVAILABLE tt-report THEN 
    DO:
     

        FOR EACH tt-report
            WHERE tt-report.term-id EQ ""
            AND tt-report.key-01  EQ "MISC"

            {sa/sa-sprdN.i m}

        IF v-misc THEN 
        DO:
      
            ASSIGN
                v-gtot-msf     = (IF v-gtot-sqft NE 0 THEN v-gtot-amt / v-gtot-sqft
                                               ELSE 0)
                v-gtot-ptd-msf = (IF v-gtot-ptd-sqft NE 0 THEN
                             v-gtot-ptd-amt / v-gtot-ptd-sqft ELSE 0)
                v-gtot-ytd-msf = (IF v-gtot-ytd-sqft NE 0 THEN
                             v-gtot-ytd-amt / v-gtot-ytd-sqft ELSE 0).

            PUT SKIP(1).

     
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
                    WHEN "cat"      THEN 
                        cVarValue = "" .
                    WHEN "dly-sf"   THEN 
                        cVarValue = STRING(v-mtot-sqft,"->>>,>>9.999")     .       
                    WHEN "dly-amt"  THEN 
                        cVarValue = STRING(v-mtot-amt,"->,>>>,>>9.99")     .
                    WHEN "dly-msf"  THEN 
                        cVarValue = STRING(v-mtot-msf,"->,>>>,>>9.99")     .      
                    WHEN "ptd-sf"   THEN 
                        cVarValue = STRING(v-mtot-ptd-sqft,"->>>,>>9.999") .
                    WHEN "ptd-amt"  THEN 
                        cVarValue = STRING(v-mtot-ptd-amt,"->,>>>,>>9.99") .
                    WHEN "ptd-msf"  THEN 
                        cVarValue = STRING(v-mtot-ptd-msf,"->,>>>,>>9.99") .
                    WHEN "ytd-sf"   THEN 
                        cVarValue = STRING(v-mtot-ytd-sqft,"->>>,>>9.999") .
                    WHEN "ytd-amt"  THEN 
                        cVarValue = STRING(v-mtot-ytd-amt,"->,>>>,>>9.99") .  
                    WHEN "ytd-msf"  THEN 
                        cVarValue = STRING(v-mtot-ytd-msf,"->,>>>,>>9.99") .

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED 
                "  MISC " SUBSTRING(cDisplay,8,350) SKIP.
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    "  MISC " + substring(cExcelDisplay,3,300) SKIP.
            END.
        
        END.
        ASSIGN
            v-gtot-sqft     = v-gtot-sqft     + v-mtot-sqft
            v-gtot-amt      = v-gtot-amt      + v-mtot-amt
            v-gtot-ptd-sqft = v-gtot-ptd-sqft + v-mtot-ptd-sqft
            v-gtot-ptd-amt  = v-gtot-ptd-amt  + v-mtot-ptd-amt
            v-gtot-ytd-sqft = v-gtot-ytd-sqft + v-mtot-ytd-sqft
            v-gtot-ytd-amt  = v-gtot-ytd-amt  + v-mtot-ytd-amt.
      
    END.



    FIND FIRST tt-report
        WHERE tt-report.term-id EQ ""
        AND tt-report.key-01  EQ "MEMO"
        NO-LOCK NO-ERROR.

    IF AVAILABLE tt-report THEN 
    DO:
    /* down 1 with frame itemx.
     underline w-sqft
               w-amt
               w-msf
               w-ptd-sqft
               w-ptd-amt
               w-ptd-msf
         with frame itemx.

     if v-ytd then
       underline w-ytd-sqft w-ytd-amt w-ytd-msf with frame itemx.

     clear frame itemx no-pause. */
    END.

    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
        AND tt-report.key-01  EQ "MEMO"

    {sa/sa-sprdN.i g}

    /* Print totals the second time */
    /*put skip(1).*/
    /* underline w-sqft
               w-amt
               w-msf
               w-ptd-sqft
               w-ptd-amt
               w-ptd-msf
         with frame itemx.
 
     if v-ytd then
       underline w-ytd-sqft w-ytd-amt w-ytd-msf with frame itemx. */

    ASSIGN 
        v-gtot-msf     = (IF v-gtot-sqft NE 0 THEN
      v-gtot-amt / v-gtot-sqft
      ELSE 0)
        v-gtot-ptd-msf = (IF v-gtot-ptd-sqft NE 0 THEN
      v-gtot-ptd-amt / v-gtot-ptd-sqft
      ELSE 0)
        v-gtot-ytd-msf = (IF v-gtot-ytd-sqft NE 0 THEN
      v-gtot-ytd-amt / v-gtot-ytd-sqft
      ELSE 0).

    /* display "  TOTAL"                   @ w-procat
             v-gtot-sqft                 @ w-sqft
             v-gtot-amt                  @ w-amt
             v-gtot-msf                  @ w-msf
             v-gtot-ptd-sqft             @ w-ptd-sqft
             v-gtot-ptd-amt              @ w-ptd-amt
             v-gtot-ptd-msf              @ w-ptd-msf
             v-gtot-ytd-sqft when v-ytd  @ w-ytd-sqft
             v-gtot-ytd-amt  when v-ytd  @ w-ytd-amt
             v-gtot-ytd-msf  when v-ytd  @ w-ytd-msf
         with frame itemx.
 
     IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            SKIP(1)
            '"' "  TOTAL"                                 '",'
            '"' STRING(v-gtot-sqft,"->>>,>>9.999")                    '",'
            '"' STRING(v-gtot-amt,"->,>>>,>>9.99") '",'
            '"' STRING(v-gtot-msf,"->,>>>,>>9.99")                        '",'
            '"' STRING(v-gtot-ptd-sqft,"->>>,>>9.999")                    '",'
            '"' STRING(v-gtot-ptd-amt,"->,>>>,>>9.99") '",'
            '"' STRING(v-gtot-ptd-msf,"->,>>>,>>9.99")                        '",'
            '"' IF v-ytd THEN STRING(v-gtot-ytd-sqft,"->>>,>>9.999") 
                ELSE "" '",'
            '"' IF v-ytd THEN STRING(v-gtot-ytd-amt,"->,>>>,>>9.99")
                ELSE "" '",'
            '"' IF v-ytd THEN STRING(v-gtot-ytd-msf,"->,>>>,>>9.99")
                ELSE "" '",'. */

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
            WHEN "cat"      THEN 
                cVarValue = "" .
            WHEN "dly-sf"   THEN 
                cVarValue = STRING(v-gtot-sqft,"->>>,>>9.999")     .       
            WHEN "dly-amt"  THEN 
                cVarValue = STRING(v-gtot-amt,"->,>>>,>>9.99")  .
            WHEN "dly-msf"  THEN 
                cVarValue = STRING(v-gtot-msf,"->,>>>,>>9.99")     .      
            WHEN "ptd-sf"   THEN 
                cVarValue = STRING(v-gtot-ptd-sqft,"->>>,>>9.999") .
            WHEN "ptd-amt"  THEN 
                cVarValue = STRING(v-gtot-ptd-amt,"->,>>>,>>9.99") .
            WHEN "ptd-msf"  THEN 
                cVarValue = STRING(v-gtot-ptd-msf,"->,>>>,>>9.99") .
            WHEN "ytd-sf"   THEN 
                cVarValue = STRING(v-gtot-ytd-sqft,"->>>,>>9.999") .
            WHEN "ytd-amt"  THEN 
                cVarValue = STRING(v-gtot-ytd-amt,"->,>>>,>>9.99") .  
            WHEN "ytd-msf"  THEN 
                cVarValue = STRING(v-gtot-ytd-msf,"->,>>>,>>9.99") .

        END CASE.

        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.

    PUT UNFORMATTED 
        "  TOTAL " SUBSTRING(cDisplay,9,350) SKIP.
    IF tb_excel THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            "  TOTAL " + substring(cExcelDisplay,3,300) SKIP.
    END.

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
            WHEN "cat"      THEN 
                cVarValue = "" .
            WHEN "dly-sf"   THEN 
                cVarValue = ""     .       
            WHEN "dly-amt"  THEN 
                cVarValue =  STRING(dTax,"->,>>>,>>9.99")    .
            WHEN "dly-msf"  THEN 
                cVarValue = ""     .      
            WHEN "ptd-sf"   THEN 
                cVarValue = "" .
            WHEN "ptd-amt"  THEN 
                cVarValue = STRING(dTax-ptd,"->,>>>,>>9.99") .
            WHEN "ptd-msf"  THEN 
                cVarValue = "" .
            WHEN "ytd-sf"   THEN 
                cVarValue = "" .
            WHEN "ytd-amt"  THEN 
                cVarValue = STRING(dTax-ytd,"->,>>>,>>9.99") .  
            WHEN "ytd-msf"  THEN 
                cVarValue = "" .

        END CASE.

        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.
      
    PUT UNFORMATTED 
        "  Tax  " SUBSTRING(cDisplay,8,350) SKIP.
    IF tb_excel THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            "  Tax " + substring(cExcelDisplay,3,300) SKIP.
    END.
         
    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "".
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        CASE cTmpField:
            WHEN "cat"      THEN 
                cVarValue = "" .
            WHEN "dly-sf"   THEN 
                cVarValue = ""     .       
            WHEN "dly-amt"  THEN 
                cVarValue =  STRING(dFreight,"->,>>>,>>9.99")    .
            WHEN "dly-msf"  THEN 
                cVarValue = ""     .      
            WHEN "ptd-sf"   THEN 
                cVarValue = "" .
            WHEN "ptd-amt"  THEN 
                cVarValue = STRING(dFreight-ptd,"->,>>>,>>9.99") .
            WHEN "ptd-msf"  THEN 
                cVarValue = "" .
            WHEN "ytd-sf"   THEN 
                cVarValue = "" .
            WHEN "ytd-amt"  THEN 
                cVarValue = STRING(dFreight-ytd,"->,>>>,>>9.99") .  
            WHEN "ytd-msf"  THEN 
                cVarValue = "" .

        END CASE.

        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.
      
    PUT UNFORMATTED 
        "Freight" SUBSTRING(cDisplay,8,350) SKIP.
    IF tb_excel THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            "Freight" + substring(cExcelDisplay,3,300) SKIP.
    END.
          
    ASSIGN
        v-grtot-amt     = v-grtot-amt + v-gtot-amt + dTax + dFreight
        v-grtot-ptd-amt = v-grtot-ptd-amt + v-gtot-ptd-amt + dTax-ptd + dFreight-ptd
        v-grtot-ytd-amt = v-grtot-ytd-amt + v-gtot-ytd-amt + dTax-ytd + dFreight-ytd.    

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
            WHEN "cat"      THEN 
                cVarValue = "" .
            WHEN "dly-sf"   THEN 
                cVarValue = ""     .       
            WHEN "dly-amt"  THEN 
                cVarValue =  STRING(v-grtot-amt,"->,>>>,>>9.99")    .
            WHEN "dly-msf"  THEN 
                cVarValue = ""     .      
            WHEN "ptd-sf"   THEN 
                cVarValue = "" .
            WHEN "ptd-amt"  THEN 
                cVarValue = STRING(v-grtot-ptd-amt,"->,>>>,>>9.99") .
            WHEN "ptd-msf"  THEN 
                cVarValue = "" .
            WHEN "ytd-sf"   THEN 
                cVarValue = "" .
            WHEN "ytd-amt"  THEN 
                cVarValue = STRING(v-grtot-ytd-amt,"->,>>>,>>9.99") .  
            WHEN "ytd-msf"  THEN 
                cVarValue = "" .
        END CASE.

        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.

    PUT UNFORMATTED 
        "  GRAND TOTAL" SUBSTRING(cDisplay,14,350) SKIP.
    IF tb_excel THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            "  GRAND TOTAL " + substring(cExcelDisplay,3,300) SKIP.
    END.

        

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
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                    .
            ELSE 
            DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN
                        parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".

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
            fi_file:SCREEN-VALUE = "c:\tmp\SalesByProduct.csv".   
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

