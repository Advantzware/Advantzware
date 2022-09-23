&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: porep\r-pofghs.w

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

DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE STREAM excel.
DEFINE VARIABLE excelcol           AS INTEGER   NO-UNDO.

DEFINE VARIABLE ldummy             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cColumnInit        AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .


ASSIGN 
    cTextListToSelect  = "PO #,Vendor,Item #,Item Description,Control #,Packed,Qty Ord,Trans Date," +
                           "Trans Qty,Amount,UOM,Balance Due"
                         
    cFieldListToSelect = "po,vend,item,desc,cont,pack,qty-ord,tran-date," +
                            "tran-qty,amt,uom,bal-due"

    cFieldLength       = "6,8,15,25,15,10,13,10," + "11,10,8,11"
    cFieldType         = "i,c,c,c,c,c,i,c," + "i,i,c,i"  
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "PO #,Vendor,Item #,Item Description,Control #,Packed,Qty Ord,Trans Date," +
                           "Trans Qty,Amount,UOM,Balance Due" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_po-no end_po-no ~
begin_po-date end_po-date begin_vend end_vend begin_po-i-no end_po-i-no ~
begin_ctrl-no end_ctrl-no begin_buyer end_buyer rd_sort select-mat rd_show ~
tb_fg tb_rm rd-dest tb_OpenCSV tbAutoClose fi_file btn-ok btn-cancel ~
btn_SelectColumns
&Scoped-Define DISPLAYED-OBJECTS begin_po-no end_po-no begin_po-date ~
end_po-date begin_vend end_vend begin_po-i-no end_po-i-no begin_ctrl-no ~
end_ctrl-no begin_buyer end_buyer lbl_sort rd_sort select-mat lbl_show ~
rd_show tb_fg mat-types tb_rm rd-dest tb_OpenCSV tbAutoClose fi_file 

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


DEFINE BUTTON btn_SelectColumns 
    LABEL "Select Columns" 
    SIZE 40 BY 1.48.


DEFINE VARIABLE begin_buyer    AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Buyer" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ctrl-no  AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Vendor Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-date  AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning PO Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-i-no  AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning PO Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-no    AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "Beginning PO#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_vend     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_buyer      AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Buyer" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ctrl-no    AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Vendor Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-date    AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending PO Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-i-no    AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending PO Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-no      AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 999999 
    LABEL "Ending PO#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend       AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-pofghs.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE
    SIZE 43 BY 1.

DEFINE VARIABLE lbl_show       AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort       AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
    VIEW-AS FILL-IN 
    SIZE 10 BY 1 NO-UNDO.

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

DEFINE VARIABLE mat-types      AS CHARACTER FORMAT "X(256)":U 
    LABEL "Material Types" 
    VIEW-AS FILL-IN 
    SIZE 1 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt        AS CHARACTER INITIAL "L" 
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
    SIZE 14 BY 4.5 NO-UNDO.

DEFINE VARIABLE rd_show        AS CHARACTER INITIAL "Open" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Open", "Open",
    "Closed", "Closed",
    "All PO's", "All PO's"
    SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "Vendor" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Vendor", "Vendor",
    "Item", "Item",
    "Control#", "Control#"
    SIZE 32 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.35.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 13.75.

DEFINE VARIABLE select-mat   AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 28 BY 5.71 NO-UNDO.

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

DEFINE VARIABLE tb_fg        AS LOGICAL   INITIAL YES 
    LABEL "Print FGs?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tb_rm        AS LOGICAL   INITIAL YES 
    LABEL "Print RMs?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY 1 NO-UNDO.

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
    begin_po-no AT ROW 2.19 COL 29 COLON-ALIGNED HELP
    "Enter Beginning PO Number"
    end_po-no AT ROW 2.19 COL 70 COLON-ALIGNED HELP
    "Enter Ending PO Number"
    begin_po-date AT ROW 3.14 COL 29 COLON-ALIGNED HELP
    "Enter Beginning PO Date"
    end_po-date AT ROW 3.14 COL 70 COLON-ALIGNED HELP
    "Enter Ending PO Date"
    begin_vend AT ROW 4.1 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number"
    end_vend AT ROW 4.1 COL 70 COLON-ALIGNED HELP
    "Enter Ending Vendor Number"
    begin_po-i-no AT ROW 5.05 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Item Number"
    end_po-i-no AT ROW 5.05 COL 70 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    begin_ctrl-no AT ROW 6 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Control Number"
    end_ctrl-no AT ROW 6 COL 70 COLON-ALIGNED HELP
    "Enter Ending Control Number"
    begin_buyer AT ROW 6.95 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Buyer" WIDGET-ID 2
    end_buyer AT ROW 6.95 COL 70 COLON-ALIGNED HELP
    "Enter Ending Buyer" WIDGET-ID 4
    sl_avail AT ROW 14.81 COL 3 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 15.19 COL 15.8 NO-LABELS WIDGET-ID 28
    lbl_sort AT ROW 8.62 COL 4 COLON-ALIGNED NO-LABELS
    rd_sort AT ROW 8.62 COL 15 NO-LABELS
    select-mat AT ROW 9.1 COL 61 NO-LABELS
    btn_SelectColumns AT ROW 13.50 COL 12 WIDGET-ID 10
    lbl_show AT ROW 9.57 COL 5 COLON-ALIGNED NO-LABELS
    rd_show AT ROW 9.57 COL 15 NO-LABELS
    tb_fg AT ROW 10.52 COL 15
    mat-types AT ROW 11 COL 56 COLON-ALIGNED
    tb_rm AT ROW 11.48 COL 15
    rd-dest AT ROW 16.48 COL 7 NO-LABELS
    lv-ornt AT ROW 16.48 COL 31 NO-LABELS
    lines-per-page AT ROW 16.48 COL 84 COLON-ALIGNED
    lv-font-no AT ROW 18.62 COL 35 COLON-ALIGNED
    lv-font-name AT ROW 19.57 COL 29 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 20.76 COL 31
    tb_OpenCSV AT ROW 20 COL 86 RIGHT-ALIGNED
    tbAutoClose AT ROW 21.8 COL 26.8 LEFT-ALIGNED
    fi_file AT ROW 20 COL 25 COLON-ALIGNED HELP
    "Enter File Name"
    btn-ok AT ROW 22.8 COL 26.5
    btn-cancel AT ROW 22.8 COL 46
    " Output Destination" VIEW-AS TEXT
    SIZE 18.9 BY .62 AT ROW 15.64 COL 5
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    BGCOLOR 15 
    "Select/Deselect RM Types" VIEW-AS TEXT
    SIZE 31 BY 1 AT ROW 8.14 COL 59
    FONT 6
    RECT-6 AT ROW 16 COL 4
    RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 23.74
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
        TITLE              = "PO RM/FG History by Vendor"
        HEIGHT             = 23.79
        WIDTH              = 96.2
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
    begin_buyer:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_ctrl-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_po-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_po-i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_po-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_buyer:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_ctrl-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_po-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_po-i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_po-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_show IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_show:PRIVATE-DATA IN FRAME FRAME-A = "rd_show".

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
    mat-types:HIDDEN IN FRAME FRAME-A       = TRUE
    mat-types:PRIVATE-DATA IN FRAME FRAME-A = "parm".


ASSIGN 
    sl_avail:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR SELECTION-LIST sl_selected IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    sl_selected:HIDDEN IN FRAME FRAME-A = TRUE.


ASSIGN 
    rd_show:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    select-mat:AUTO-RESIZE IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR TOGGLE-BOX tbAutoClose IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tbAutoClose:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_fg:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_rm:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* PO RM/FG History by Vendor */
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
ON WINDOW-CLOSE OF C-Win /* PO RM/FG History by Vendor */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_buyer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_buyer C-Win
ON LEAVE OF begin_buyer IN FRAME FRAME-A /* Beginning Buyer */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ctrl-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ctrl-no C-Win
ON LEAVE OF begin_ctrl-no IN FRAME FRAME-A /* Beginning Vendor Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-date C-Win
ON LEAVE OF begin_po-date IN FRAME FRAME-A /* Beginning PO Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-i-no C-Win
ON LEAVE OF begin_po-i-no IN FRAME FRAME-A /* Beginning PO Item# */
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
                        {custom/asimailr.i &TYPE = "vendor"
                                  &begin_cust= begin_vend
                                  &END_cust=end_vend
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


&Scoped-define SELF-NAME btn_SelectColumns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_SelectColumns C-Win
ON CHOOSE OF btn_SelectColumns IN FRAME FRAME-A /* Select Columns */
    DO:
        DEFINE VARIABLE cTextSelected AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cTextListed   AS CHARACTER NO-UNDO.

        RUN displaySelectionList2.

        ASSIGN 
            cTextSelected = sl_selected:LIST-ITEMS
            cTextListed   = sl_avail:LIST-ITEMS.
 
        IF NOT cColumnInit THEN RUN custom/d-rptsel.w (INPUT-OUTPUT cTextListed, INPUT-OUTPUT cTextSelected, INPUT-OUTPUT cTextListToDefault, INPUT-OUTPUT cTextListToSelect).

        ASSIGN 
            sl_selected:LIST-ITEMS = cTextSelected
            sl_avail:LIST-ITEMS    = cTextListed.
 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_buyer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_buyer C-Win
ON LEAVE OF end_buyer IN FRAME FRAME-A /* Ending Buyer */
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
        {custom/chgfont2.i "12" }
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
        RUN pChangeDest .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_show C-Win
ON VALUE-CHANGED OF rd_show IN FRAME FRAME-A
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
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.
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


&Scoped-define SELF-NAME tbAutoClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAutoClose C-Win
ON VALUE-CHANGED OF tbAutoClose IN FRAME FRAME-A /* Auto Close */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fg C-Win
ON VALUE-CHANGED OF tb_fg IN FRAME FRAME-A /* Print FGs? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rm C-Win
ON VALUE-CHANGED OF tb_rm IN FRAME FRAME-A /* Print RMs? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Auto Run Excel? */
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
DEFINE VARIABLE v-mat-list AS CHARACTER NO-UNDO.
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
        begin_po-date = DATE(1,1,YEAR(TODAY))
        end_po-date   = TODAY.
    RUN DisplaySelectionList.
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    btn_SelectColumns:LOAD-IMAGE("Graphics/32x32/selectColumns.png").
    RUN enable_UI.
  
    FOR EACH mat:
        v-mat-list = v-mat-list + STRING(mat.mat,"x(5)") + " " + mat.dscr + ",".
    END.
    IF SUBSTR(v-mat-list,LENGTH(TRIM(v-mat-list)),1) EQ "," THEN
        SUBSTR(v-mat-list,LENGTH(TRIM(v-mat-list)),1) = "".
  
    select-mat:LIST-ITEMS = v-mat-list.
        
    DO i = 1 TO select-mat:NUM-ITEMS:
        IF TRIM(SUBSTR(select-mat:ENTRY(i),1,5)) EQ "B" THEN 
        DO:
            select-mat:SCREEN-VALUE = ENTRY(i,v-mat-list).
            LEAVE.
        END.
    END.
  
    {methods/nowait.i}
    {sys/inc/reportsConfigNK1.i "PR3" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_po-no.
    END.
    cColumnInit   = NO .
    RUN pChangeDest .

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-report-recs C-Win 
PROCEDURE build-report-recs :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipv-s-pono LIKE po-ord.po-no FORMAT ">>>>>>>>" NO-UNDO.
    DEFINE INPUT PARAMETER ipv-e-pono LIKE ipv-s-pono INIT 999999 NO-UNDO.
    DEFINE INPUT PARAMETER ipv-s-date LIKE po-ord.po-date FORMAT "99/99/9999" INIT "01/01/0001" NO-UNDO.
    DEFINE INPUT PARAMETER ipv-e-date LIKE ipv-s-date INIT TODAY NO-UNDO.
    DEFINE INPUT PARAMETER ipv-s-vend LIKE po-ord.vend-no NO-UNDO.
    DEFINE INPUT PARAMETER ipv-e-vend LIKE ipv-s-vend INIT "zzzzzzzz" NO-UNDO.
    DEFINE INPUT PARAMETER ipv-s-item LIKE po-ordl.i-no NO-UNDO.
    DEFINE INPUT PARAMETER ipv-e-item LIKE ipv-s-item INIT "zzzzzzzzzzzzzzz" NO-UNDO.
    DEFINE INPUT PARAMETER ipv-s-vitm LIKE po-ordl.vend-i-no NO-UNDO.
    DEFINE INPUT PARAMETER ipv-e-vitm LIKE ipv-s-vitm INIT "zzzzzzzzzzzzzzz" NO-UNDO.
    DEFINE INPUT PARAMETER ipv-s-buyer LIKE po-ord.buyer NO-UNDO.
    DEFINE INPUT PARAMETER ipv-e-buyer LIKE ipv-s-buyer INIT "zzzzzzzzzzzzzzz" NO-UNDO.

    DEFINE INPUT PARAMETER ipv-stat   AS   CHARACTER FORMAT "!" INIT "A" NO-UNDO.
    DEFINE INPUT PARAMETER ipv-type   AS   CHARACTER FORMAT "!" INIT "B" NO-UNDO.
    DEFINE INPUT PARAMETER ipv-sort   AS   CHARACTER FORMAT "!" INIT "V" NO-UNDO.
    DEFINE INPUT PARAMETER ipv-term   AS   CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipv-mattype-list AS   CHARACTER FORMAT "x(36)" NO-UNDO.

    DEFINE BUFFER xjob-mat FOR job-mat.
    FOR EACH po-ord
        WHERE po-ord.company EQ cocode
        AND po-ord.po-no   GE ipv-s-pono
        AND po-ord.po-no   LE ipv-e-pono
        AND po-ord.po-date GE ipv-s-date
        AND po-ord.po-date LE ipv-e-date
        AND po-ord.vend-no GE ipv-s-vend
        AND po-ord.vend-no LE ipv-e-vend
        AND po-ord.buyer   GE ipv-s-buyer
        AND po-ord.buyer   LE ipv-e-buyer
        AND ((po-ord.stat EQ "C" AND ipv-stat EQ "C") OR
        (po-ord.stat NE "C" AND ipv-stat EQ "O") OR ipv-stat EQ "A")
        NO-LOCK,
          
        EACH po-ordl
        WHERE po-ordl.company   EQ po-ord.company
        AND po-ordl.po-no     EQ po-ord.po-no
        AND po-ordl.i-no      GE ipv-s-item
        AND po-ordl.i-no      LE ipv-e-item
        AND po-ordl.vend-i-no GE ipv-s-vitm
        AND po-ordl.vend-i-no LE ipv-e-vitm
        AND ((po-ordl.stat EQ "C" AND ipv-stat EQ "C") OR
        (po-ordl.stat NE "C" AND ipv-stat EQ "O") OR ipv-stat EQ "A")
        AND ((po-ordl.item-type EQ YES AND ipv-type EQ "R") OR
        (po-ordl.item-type EQ NO  AND ipv-type EQ "F") OR ipv-type EQ "B")
        NO-LOCK:
      
        RELEASE item.
        RELEASE job-hdr.
        RELEASE job-mat.
      
        FIND FIRST item
            WHERE item.company EQ cocode
            AND item.i-no    EQ po-ordl.i-no
            NO-LOCK NO-ERROR.
      
        IF po-ordl.item-type AND NOT AVAILABLE item THEN NEXT.
      
        IF INDEX(ipv-mattype-list,"A") NE 0 AND
            po-ordl.job-no NE ""           AND 
            AVAILABLE item                     AND
            item.mat-type EQ "B"           THEN
            FIND FIRST job-hdr
                WHERE job-hdr.company EQ cocode
                AND job-hdr.job-no  EQ po-ordl.job-no
                AND job-hdr.job-no2 EQ po-ordl.job-no2
                NO-LOCK NO-ERROR.
       
        IF AVAILABLE job-hdr THEN
            FIND FIRST job-mat
                WHERE job-mat.company  EQ cocode
                AND job-mat.job-no   EQ job-hdr.job-no
                AND job-mat.job-no2  EQ job-hdr.job-no2
                AND job-mat.job      EQ job-hdr.job
                AND job-mat.rm-i-no  EQ po-ordl.i-no
                AND job-mat.frm      EQ po-ordl.s-num
                NO-LOCK NO-ERROR. 
      
        IF AVAILABLE job-mat THEN
            FOR EACH xjob-mat
                WHERE xjob-mat.company  EQ cocode
                AND xjob-mat.job      EQ job-mat.job
                AND xjob-mat.frm      EQ job-mat.frm
                AND xjob-mat.job-no   EQ job-mat.job-no
                AND xjob-mat.job-no2  EQ job-mat.job-no2
                AND CAN-FIND(FIRST item WHERE item.company  EQ cocode
                AND item.i-no     EQ xjob-mat.i-no
                AND item.mat-type EQ "A")
                NO-LOCK:
        
                CREATE report.
                ASSIGN
                    report.term-id = ipv-term
                    report.key-01  = STRING(int(po-ordl.item-type),"9")
                    report.key-02  = IF ipv-sort EQ "V" THEN po-ord.vend-no ELSE ""
                    report.key-03  = IF ipv-sort NE "C" THEN xjob-mat.i-no   ELSE ""
                    report.key-04  = ""
                    report.key-05  = xjob-mat.i-no
                    report.key-06  = po-ord.vend-no
                    report.key-07  = xjob-mat.qty-uom
                    report.rec-id  = RECID(po-ordl).
            END.
      
        IF AVAILABLE item AND index(ipv-mattype-list,item.mat-type) EQ 0 THEN NEXT.

        CREATE report.
        ASSIGN
            report.term-id = ipv-term
            report.key-01  = STRING(int(po-ordl.item-type),"9")
            report.key-02  = IF ipv-sort EQ "V" THEN po-ord.vend-no ELSE ""
            report.key-03  = IF ipv-sort NE "C" THEN po-ordl.i-no   ELSE ""
            report.key-04  = po-ordl.vend-i-no
            report.key-05  = po-ordl.i-no
            report.key-06  = po-ord.vend-no
            report.rec-id  = RECID(po-ordl).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
    DISPLAY begin_po-no end_po-no begin_po-date end_po-date begin_vend end_vend 
        begin_po-i-no end_po-i-no begin_ctrl-no end_ctrl-no begin_buyer 
        end_buyer lbl_sort rd_sort select-mat lbl_show rd_show tb_fg mat-types 
        tb_rm rd-dest tb_OpenCSV tbAutoClose fi_file 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_po-no end_po-no begin_po-date end_po-date 
        begin_vend end_vend begin_po-i-no end_po-i-no begin_ctrl-no 
        end_ctrl-no begin_buyer end_buyer rd_sort select-mat rd_show tb_fg 
        tb_rm rd-dest tb_OpenCSV tbAutoClose fi_file btn-ok btn-cancel 
        btn_SelectColumns
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel-1-proc C-Win 
PROCEDURE excel-1-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-date AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ip-dec-1 AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-dec-2 AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-uom AS CHARACTER NO-UNDO.

    PUT STREAM excel UNFORMATTED
        '"' (IF ip-date NE ? THEN STRING(ip-date)
        ELSE "")                             '",'
        '"' STRING(ip-dec-1,"->>,>>>,>>9.9<<<<<")       '",'
        '"' STRING(ip-dec-2,"->,>>>,>>9.99")      '",'
        '"' ip-uom                               '",'.
    excelcol = 11.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel-2-proc C-Win 
PROCEDURE excel-2-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-balance AS DECIMAL NO-UNDO.

    DEFINE VARIABLE viIndex AS INTEGER NO-UNDO.

    DO viIndex = excelcol TO 10:
        PUT STREAM excel UNFORMATTED
            '"' "" '",'.
    END.

    PUT STREAM excel UNFORMATTED
        '"' STRING(ip-balance,"->,>>>,>>9.9<<<<<") '",' SKIP.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel-3-proc C-Win 
PROCEDURE excel-3-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-text AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-dec1 AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-dec2 AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-dec3 AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-dec4 AS DECIMAL NO-UNDO.
  
    PUT STREAM excel UNFORMATTED
        SKIP
        '"' ""                                    '",' 
        '"' ""                                    '",'
        '"' ""                                    '",'
        '"' ip-text                               '",'
        '"' ""                                    '",'
        '"' ""                                    '",'
        '"' STRING(ip-dec1,"->>,>>>,>>9.9<<<<<")  '",'
        '"' ""                                    '",'
        '"' STRING(ip-dec2,"->,>>>,>>9.9<<<<<")    '",'
        '"' STRING(ip-dec4,"->,>>>,>>9.99")        '",'
        '"' ""                                    '",'
        '"' STRING(ip-dec3,"->>,>>>,>>9.9<<<<<")  '",'
        SKIP(1).

    IF ip-text EQ "Vendor Totals" THEN
        PUT STREAM excel UNFORMATTED SKIP.

    excelcol = 12.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel-skip-proc C-Win 
PROCEDURE excel-skip-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE i AS INTEGER NO-UNDO.

    PUT STREAM excel UNFORMATTED SKIP.
    DO i = 1 TO 7:
        PUT STREAM excel UNFORMATTED
            '"' "" '",'.
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
            
        IF NOT OKpressed THEN  RETURN NO-APPLY.*/
     
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
    /* ------------------------------------------------ po/rep/sh-pur.p 05/98 JLF */
    /* PO FG/RM History                                                           */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-topw.f}*/

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
        str-tit2 FORMAT "X(112)"   "{1}" AT 123
        SKIP(1)
      
        WITH FRAME r-top ROW 1 COLUMN 1 STREAM-IO WIDTH 250
        NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.
  
    DEFINE BUFFER xjob-mat FOR job-mat.

    DEFINE VARIABLE v-foot-rem     LIKE ap-invl.amt-msf NO-UNDO.
    DEFINE VARIABLE v-msf-cal      AS LOG       NO-UNDO.

    DEFINE VARIABLE v-s-pono       LIKE po-ord.po-no FORMAT ">>>>>>>>" NO-UNDO.
    DEFINE VARIABLE v-e-pono       LIKE v-s-pono INIT 999999 NO-UNDO.
    DEFINE VARIABLE v-s-date       LIKE po-ord.po-date FORMAT "99/99/9999" INIT "01/01/0001" NO-UNDO.
    DEFINE VARIABLE v-e-date       LIKE v-s-date INIT TODAY NO-UNDO.
    DEFINE VARIABLE v-s-vend       LIKE po-ord.vend-no NO-UNDO.
    DEFINE VARIABLE v-e-vend       LIKE v-s-vend INIT "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE v-s-item       LIKE po-ordl.i-no NO-UNDO.
    DEFINE VARIABLE v-e-item       LIKE v-s-item INIT "zzzzzzzzzzzzzzz" NO-UNDO.
    DEFINE VARIABLE v-s-vitm       LIKE po-ordl.vend-i-no NO-UNDO.
    DEFINE VARIABLE v-e-vitm       LIKE v-s-vitm INIT "zzzzzzzzzzzzzzz" NO-UNDO.
    DEFINE VARIABLE v-s-buyer      LIKE po-ord.buyer NO-UNDO.
    DEFINE VARIABLE v-e-buyer      LIKE v-s-buyer INIT "zzzzzzzzzzzzzzz" NO-UNDO.

    DEFINE VARIABLE v-stat         AS CHARACTER FORMAT "!" INIT "A" NO-UNDO.
    DEFINE VARIABLE v-type         AS CHARACTER FORMAT "!" INIT "B" NO-UNDO.
    DEFINE VARIABLE v-sort         AS CHARACTER FORMAT "!" INIT "V" NO-UNDO.

    DEFINE VARIABLE v-mattype-list AS CHARACTER FORMAT "x(36)" NO-UNDO.
    DEFINE VARIABLE v-mat-dscr     AS CHARACTER FORMAT "x(20)" EXTENT 21 NO-UNDO.

    DEFINE VARIABLE v-balance      LIKE rm-rdtlh.qty NO-UNDO.
    DEFINE VARIABLE v-cons-qty     LIKE po-ordl.cons-qty NO-UNDO.
    DEFINE VARIABLE bf-v-cons-qty  LIKE po-ordl.cons-qty NO-UNDO.
    DEFINE VARIABLE v-trans-date   LIKE rm-rcpth.trans-date NO-UNDO.
    DEFINE VARIABLE v-trans-qty    LIKE rm-rdtlh.qty NO-UNDO.

    DEFINE VARIABLE v-first        LIKE report.key-02 EXTENT 4 NO-UNDO.
    DEFINE VARIABLE v-ord          LIKE rm-rdtlh.qty EXTENT 4 NO-UNDO.
    DEFINE VARIABLE v-qty          LIKE rm-rdtlh.qty EXTENT 4 NO-UNDO.
    DEFINE VARIABLE v-bal          LIKE rm-rdtlh.qty EXTENT 4 NO-UNDO.
    DEFINE VARIABLE v-amt          AS DECIMAL   EXTENT 4 NO-UNDO.
    DEFINE VARIABLE v-amount       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-uom          AS CHARACTER NO-UNDO.

    DEFINE VARIABLE str-tit6       LIKE str-tit3.
    DEFINE VARIABLE v-item-type    AS CHARACTER FORMAT "x(14)" INIT "Finished Goods".
    DEFINE VARIABLE excel-skip     AS LOG       NO-UNDO.
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

    /*{sys/form/r-top5DL3.f} */
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
    DEFINE BUFFER b-rm-rcpth FOR rm-rcpth .
    DEFINE BUFFER b-fg-rcpth FOR fg-rcpth .
    DEFINE BUFFER b-rm-rdtlh FOR rm-rdtlh .
    DEFINE BUFFER b-fg-rdtlh FOR fg-rdtlh .

    DEFINE VARIABLE v-first-rec AS RECID NO-UNDO .

    FORM HEADER str-tit6 FORMAT "x(130)" SKIP
        v-item-type SKIP(1)
        str-tit4 SKIP
        str-tit5 SKIP

        WITH FRAME r-top.

    FORM po-ord.po-no          COLUMN-LABEL "PO #"
        po-ord.vend-no        COLUMN-LABEL "Vendor"
        report.key-05         COLUMN-LABEL "Item #"
        FORMAT "x(15)"
        item.i-name           COLUMN-LABEL "Item Description"
        FORMAT "x(23)"
        po-ordl.vend-i-no     COLUMN-LABEL "Control #"
        itemfg.prod-notes     COLUMN-LABEL "Packed"
        FORMAT "x(10)"
        v-cons-qty            COLUMN-LABEL "Qty Ord"
        rm-rcpth.trans-date   COLUMN-LABEL "Trans Date"
        rm-rdtlh.qty          COLUMN-LABEL "Trans Qty"
        v-amount COLUMN-LABEL "Amount"
        v-uom    COLUMN-LABEL "UOM"
        v-balance             COLUMN-LABEL "Balance Due"     
        WITH FRAME main NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 165 /*131*/ .
  
    {ce/msfcalc.i} 

    ASSIGN
        str-tit2  = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}
   
        v-s-pono  = begin_po-no
        v-e-pono  = END_po-no
        v-s-date  = begin_po-date
        v-e-date  = end_po-date
        v-s-vend  = begin_vend
        v-e-vend  = end_vend
        v-s-item  = begin_po-i-no
        v-e-item  = end_po-i-no
        v-s-vitm  = begin_ctrl-no
        v-e-vitm  = END_ctrl-no
        v-s-buyer = begin_buyer
        v-e-buyer = end_buyer
        v-sort    = SUBSTR(rd_sort,1,1)
        v-stat    = SUBSTR(rd_show,1,1)
        v-type    = IF tb_fg THEN
                 IF tb_rm THEN "B" ELSE "F"
               ELSE
                 IF tb_rm THEN "R" ELSE "".

    DO WITH FRAME {&frame-name}:          
        DO i = 1 TO select-mat:num-items:
            IF select-mat:is-selected(i) THEN
                v-mattype-list = v-mattype-list + trim(substr(select-mat:entry(i),1,5)) + ",".
        END.
  
        IF LENGTH(TRIM(v-mattype-list)) NE 0 AND substr(v-mattype-list,LENGTH(TRIM(v-mattype-list)),1) EQ "," THEN
            substr(v-mattype-list,LENGTH(TRIM(v-mattype-list)),1) = "".
    
        mat-types = v-mattype-list.
  
        DO i = 1 TO LENGTH(mat-types):
            IF substr(mat-types,i,1) EQ "," THEN substr(mat-types,i,1) = " ".
        END.
  
        DISPLAY mat-types.
    END.

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

        IF LOOKUP(ttRptSelected.TextList, "Qty Ord,Trans Qty,Amount,Balance Due") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    {sa/sa-sls01.i}
 
    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        /*excelheader = "PO #,Vendor,Item #,Item Description,Control #,Packed,"
                    + "Qty Ord,Trans Date,Trans Qty,Amount,UOM,Balance Due".*/
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").
  
    DISPLAY WITH FRAME r-top.

    {custom/statusMsg.i " 'Processing...'"}

    RUN build-report-recs (
        v-s-pono, 
        v-e-pono, 
        v-s-date, 
        v-e-date, 
        v-s-vend, 
        v-e-vend, 
        v-s-item, 
        v-e-item,
        v-s-vitm, 
        v-e-vitm, 
        v-s-buyer,
        v-e-buyer,
        v-stat, 
        v-type, 
        v-sort,     
        v-term,
        v-mattype-list).

    FOR EACH report
        WHERE report.term-id EQ v-term,

        FIRST po-ordl
        WHERE RECID(po-ordl) EQ report.rec-id
        NO-LOCK,

        FIRST po-ord WHERE
        po-ord.company EQ po-ordl.company AND
        po-ord.po-no   EQ po-ordl.po-no NO-LOCK
        BREAK BY report.key-01
        BY report.key-02
        BY report.key-03
        BY report.key-04

        TRANSACTION:

        {custom/statusMsg.i " 'Processing PO#  '  + string(po-ord.po-no) "}

        IF FIRST-OF(report.key-02) THEN v-first[2] = report.key-02.
        IF FIRST-OF(report.key-03) THEN v-first[3] = report.key-03.
        IF FIRST-OF(report.key-04) THEN v-first[4] = report.key-04.

        RELEASE item.
        RELEASE itemfg.

        IF FIRST-OF(report.key-01) THEN 
        DO:
            IF report.key-01 EQ "1" THEN v-item-type = "Raw Materials".

            IF FIRST(report.key-01) THEN DISPLAY WITH FRAME r-top.
            ELSE PAGE.

            ASSIGN
                v-ord = 0
                v-qty = 0
                v-amt = 0   .
        END.

        v-cons-qty = po-ordl.cons-qty.
      
        IF po-ordl.item-type THEN 
        DO:
            FIND FIRST item
                WHERE item.company EQ cocode
                AND item.i-no    EQ report.key-05
                NO-LOCK NO-ERROR.
            
            IF po-ordl.i-no NE report.key-05 AND
                report.key-07 NE ""           THEN 
            DO:
           
                FIND FIRST item
                    WHERE item.company EQ cocode
                    AND item.i-no    EQ po-ordl.i-no
                    NO-LOCK NO-ERROR. 
           
                IF po-ordl.cons-uom NE "EA" THEN
                    RUN sys/ref/convquom.p (po-ordl.cons-uom, "EA", item.basis-w,
                        po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                        v-cons-qty, OUTPUT v-cons-qty).
        
                FIND FIRST item
                    WHERE item.company EQ cocode
                    AND item.i-no    EQ report.key-05
                    NO-LOCK NO-ERROR.
            
                IF "EA" NE report.key-07 THEN
                    RUN sys/ref/convquom.p ("EA", report.key-07, item.basis-w,
                        po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                        v-cons-qty, OUTPUT v-cons-qty).
            END.
        END.
 
        ELSE 
        DO:
            FIND FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ po-ordl.i-no
                NO-LOCK NO-ERROR.
            IF po-ordl.cons-uom NE "EA" THEN
                RUN sys/ref/convquom.p(po-ordl.cons-uom, "EA", 0, 0, 0, 0, 
                    v-cons-qty, OUTPUT v-cons-qty).
        END.      

        ASSIGN
            v-ord[4]  = v-ord[4] + v-cons-qty
            v-balance = v-cons-qty.

        ASSIGN 
            v-trans-date = ? 
            v-trans-qty  = 0 .
      
        IF po-ordl.item-type THEN
            FIND FIRST b-rm-rcpth
                WHERE b-rm-rcpth.company    EQ cocode
                AND b-rm-rcpth.po-no      EQ trim(STRING(po-ordl.po-no,">>>>>9"))
                AND b-rm-rcpth.i-no       EQ po-ordl.i-no            
                AND (b-rm-rcpth.rita-code EQ "R" OR b-rm-rcpth.rita-code EQ "A")
                NO-LOCK NO-ERROR. 
        FIND FIRST b-rm-rdtlh
            WHERE b-rm-rdtlh.r-no EQ b-rm-rcpth.r-no  
            AND b-rm-rdtlh.job-no  EQ po-ordl.job-no
            AND b-rm-rdtlh.job-no2 EQ po-ordl.job-no2
            AND b-rm-rdtlh.s-num   EQ po-ordl.s-num
            NO-LOCK NO-ERROR.
     
        IF AVAILABLE b-rm-rcpth THEN 
        DO:
            bf-v-cons-qty = b-rm-rdtlh.qty.
        
            IF po-ordl.i-no NE report.key-05 AND
                report.key-07 NE ""           THEN 
            DO:
           
                FIND FIRST item
                    WHERE item.company EQ cocode
                    AND item.i-no    EQ po-ordl.i-no
                    NO-LOCK NO-ERROR. 
           
                IF po-ordl.cons-uom NE "EA" THEN
                    RUN sys/ref/convquom.p (po-ordl.cons-uom, "EA", item.basis-w,
                        po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                        bf-v-cons-qty, OUTPUT bf-v-cons-qty).
        
                FIND FIRST item
                    WHERE item.company EQ cocode
                    AND item.i-no    EQ report.key-05
                    NO-LOCK NO-ERROR.
            
                IF "EA" NE report.key-07 THEN
                    RUN sys/ref/convquom.p ("EA", report.key-07, item.basis-w,
                        po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                        bf-v-cons-qty, OUTPUT bf-v-cons-qty).
            END.
            ASSIGN 
                v-amount = bf-v-cons-qty *  b-rm-rdtlh.cost
                v-uom    = b-rm-rcpth.pur-uom.

            v-trans-date = b-rm-rcpth.trans-date .
            v-trans-qty  = b-rm-rdtlh.qty .

            ASSIGN 
                v-qty[4]  = v-qty[4] + bf-v-cons-qty
                v-balance = v-balance - bf-v-cons-qty
                v-amt[4]  = v-amt[4] + v-amount.
            ASSIGN 
                v-first-rec = RECID(b-rm-rdtlh) .

        END. /* avail buffer */

        ELSE 
        DO:
            FIND FIRST b-fg-rcpth
                WHERE b-fg-rcpth.company    EQ cocode
                AND b-fg-rcpth.i-no       EQ report.key-05
                AND b-fg-rcpth.po-no      EQ trim(STRING(po-ordl.po-no,">>>>>9"))
                AND b-fg-rcpth.job-no     EQ po-ordl.job-no
                AND b-fg-rcpth.job-no2    EQ po-ordl.job-no2
                AND (b-fg-rcpth.rita-code EQ "R" OR
                b-fg-rcpth.rita-code EQ "A")
                NO-LOCK NO-ERROR.

            FIND FIRST b-fg-rdtlh
                WHERE b-fg-rdtlh.r-no EQ b-fg-rcpth.r-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE b-fg-rcpth THEN 
            DO:
                ASSIGN
                    v-qty[4]  = v-qty[4] + b-fg-rdtlh.qty
                    v-balance = v-balance - b-fg-rdtlh.qty
                    v-amount  = b-fg-rdtlh.qty * b-fg-rdtlh.cost /
                               (IF b-fg-rcpth.pur-uom EQ "M" THEN 1000 ELSE 1)
                    v-amt[4]  = v-amt[4] + v-amount
                    v-uom     = "M".

                ASSIGN 
                    v-first-rec  = RECID(b-fg-rdtlh) 
                    v-trans-date = b-fg-rcpth.trans-date 
                    v-trans-qty  = b-fg-rdtlh.qty  .
            END.

        END.     /* not avail buffer b-rm-rdtlh */

        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".
     
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "po"    THEN 
                    cVarValue = STRING(po-ord.po-no,">>>>>>>>") .
                WHEN "vend"   THEN 
                    cVarValue = STRING(po-ord.vend-no,"x(8)").
                WHEN "item"   THEN 
                    cVarValue = STRING(report.key-05,"x(15)").
                WHEN "desc"  THEN 
                    cVarValue = IF AVAILABLE ITEM THEN STRING(item.i-name,"x(25)") ELSE IF AVAILABLE itemfg THEN STRING(itemfg.i-name,"x(25)") ELSE "".
                WHEN "cont"   THEN 
                    cVarValue = STRING(po-ordl.vend-i-no,"x(15)") .
                WHEN "pack"  THEN 
                    cVarValue = IF AVAILABLE itemfg THEN STRING(itemfg.prod-notes,"x(10)") ELSE "" .
                WHEN "qty-ord"   THEN 
                    cVarValue = STRING(v-cons-qty,"->,>>>,>>9.99") .
                WHEN "tran-date"  THEN 
                    cVarValue = IF v-trans-date NE ? THEN STRING(v-trans-date,"99/99/9999") ELSE "" .
                WHEN "tran-qty"  THEN 
                    cVarValue = STRING(v-trans-qty,"->>>,>>9.99") .
                WHEN "amt"   THEN 
                    cVarValue = STRING(v-amount,"->>,>>9.99") .
                WHEN "uom"  THEN 
                    cVarValue = STRING(v-uom,"x(8)") .
                WHEN "bal-due"  THEN 
                    cVarValue = STRING(v-balance,"->>>,>>9.99") .
                         
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

        /*display po-ord.po-no
                po-ord.vend-no
                report.key-05
                item.i-name when avail item
                itemfg.i-name when avail itemfg @ item.i-name
                po-ordl.vend-i-no
                itemfg.prod-notes when avail itemfg
                v-cons-qty
            with frame main.*/
        excel-skip = NO.

        IF po-ordl.item-type THEN
            FOR EACH rm-rcpth
                WHERE rm-rcpth.company    EQ cocode
                AND rm-rcpth.po-no      EQ trim(STRING(po-ordl.po-no,">>>>>9"))
                AND rm-rcpth.i-no       EQ po-ordl.i-no            
                /*and rm-rcpth.job-no     eq po-ordl.job-no
                and rm-rcpth.job-no2    eq po-ordl.job-no2*/
                AND (((rm-rcpth.rita-code EQ "R" OR rm-rcpth.rita-code EQ "A") /*and
                  item.i-code eq "R") or
                 (rm-rcpth.rita-code eq "I" and item.i-code eq "E"*/))
                NO-LOCK,
                EACH rm-rdtlh
                WHERE rm-rdtlh.r-no EQ rm-rcpth.r-no  
                AND rm-rdtlh.job-no  EQ po-ordl.job-no
                AND rm-rdtlh.job-no2 EQ po-ordl.job-no2
                AND rm-rdtlh.s-num   EQ po-ordl.s-num
                AND RECID(rm-rdtlh) NE v-first-rec 
                NO-LOCK

                BREAK BY rm-rcpth.trans-date BY rm-rcpth.r-no:

                IF NOT FIRST(rm-rcpth.trans-date) THEN 
                DO:
                    v-first[4] = "".
                /*down with frame main.
                clear frame main.*/
                END.
        
                v-cons-qty = rm-rdtlh.qty.
        
                IF po-ordl.i-no NE report.key-05 AND
                    report.key-07 NE ""           THEN 
                DO:
           
                    FIND FIRST item
                        WHERE item.company EQ cocode
                        AND item.i-no    EQ po-ordl.i-no
                        NO-LOCK NO-ERROR. 
           
                    IF po-ordl.cons-uom NE "EA" THEN
                        RUN sys/ref/convquom.p (po-ordl.cons-uom, "EA", item.basis-w,
                            po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                            v-cons-qty, OUTPUT v-cons-qty).
        
                    FIND FIRST item
                        WHERE item.company EQ cocode
                        AND item.i-no    EQ report.key-05
                        NO-LOCK NO-ERROR.
            
                    IF "EA" NE report.key-07 THEN
                        RUN sys/ref/convquom.p ("EA", report.key-07, item.basis-w,
                            po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                            v-cons-qty, OUTPUT v-cons-qty).
                END.
                ASSIGN 
                    v-amount = v-cons-qty *  rm-rdtlh.cost
                    v-uom    = rm-rcpth.pur-uom.

                ASSIGN 
                    v-qty[4]  = v-qty[4] + v-cons-qty
                    v-balance = v-balance - v-cons-qty
                    v-amt[4]  = v-amt[4] + v-amount.

                /*display rm-rcpth.trans-date
                        v-cons-qty @ rm-rdtlh.qty
                        v-amount v-uom
                    with frame main.*/
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "po"    THEN 
                            cVarValue = "" .
                        WHEN "vend"   THEN 
                            cVarValue = "".
                        WHEN "item"   THEN 
                            cVarValue = "".
                        WHEN "desc"  THEN 
                            cVarValue =  "" .
                        WHEN "cont"   THEN 
                            cVarValue = "" .
                        WHEN "pack"  THEN 
                            cVarValue =  "" .
                        WHEN "qty-ord"   THEN 
                            cVarValue = "" .
                        WHEN "tran-date"  THEN 
                            cVarValue = STRING(rm-rcpth.trans-date,"99/99/9999") .
                        WHEN "tran-qty"  THEN 
                            cVarValue = STRING(v-cons-qty,"->>>,>>9.99") .
                        WHEN "amt"   THEN 
                            cVarValue = STRING(v-amount,"->>,>>9.99") .
                        WHEN "uom"  THEN 
                            cVarValue = STRING(v-uom,"x(8)") .
                        WHEN "bal-due"  THEN 
                            cVarValue = /*STRING(v-balance,"->>>,>>9.99")*/ "" .
                         
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
       
            END.

        ELSE
            FOR EACH fg-rcpth
                WHERE fg-rcpth.company    EQ cocode
                AND fg-rcpth.i-no       EQ report.key-05
                AND fg-rcpth.po-no      EQ trim(STRING(po-ordl.po-no,">>>>>9"))
                AND fg-rcpth.job-no     EQ po-ordl.job-no
                AND fg-rcpth.job-no2    EQ po-ordl.job-no2
                AND (fg-rcpth.rita-code EQ "R" OR
                fg-rcpth.rita-code EQ "A")
                NO-LOCK,

                EACH fg-rdtlh
                WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                AND RECID(fg-rdtlh) NE v-first-rec
                NO-LOCK

                BREAK BY fg-rcpth.trans-date BY fg-rdtlh.trans-time BY fg-rcpth.r-no:

                IF NOT FIRST(fg-rcpth.trans-date) THEN 
                DO:
                    v-first[4] = "".
                /*down with frame main.
                clear frame main.*/
                END.

                ASSIGN
                    v-qty[4]  = v-qty[4] + fg-rdtlh.qty
                    v-balance = v-balance - fg-rdtlh.qty
                    v-amount  = fg-rdtlh.qty * fg-rdtlh.cost /
                     (IF fg-rcpth.pur-uom EQ "M" THEN 1000 ELSE 1)
                    v-amt[4]  = v-amt[4] + v-amount
                    v-uom     = "M".
         
                /*display fg-rcpth.trans-date @ rm-rcpth.trans-date
                        fg-rdtlh.qty        @ rm-rdtlh.qty
                        v-amount v-uom
                    with frame main.*/
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "po"    THEN 
                            cVarValue = "" .
                        WHEN "vend"   THEN 
                            cVarValue = "".
                        WHEN "item"   THEN 
                            cVarValue = "".
                        WHEN "desc"  THEN 
                            cVarValue =  "" .
                        WHEN "cont"   THEN 
                            cVarValue = "" .
                        WHEN "pack"  THEN 
                            cVarValue =  "" .
                        WHEN "qty-ord"   THEN 
                            cVarValue = "" .
                        WHEN "tran-date"  THEN 
                            cVarValue = STRING(fg-rcpth.trans-date,"99/99/9999") .
                        WHEN "tran-qty"  THEN 
                            cVarValue = STRING(fg-rdtlh.qty,"->>>,>>9.99") .
                        WHEN "amt"   THEN 
                            cVarValue = STRING(v-amount,"->>,>>9.99") .
                        WHEN "uom"  THEN 
                            cVarValue = STRING(v-uom,"x(8)") .
                        WHEN "bal-due"  THEN 
                            cVarValue = /*STRING(v-balance,"->>>,>>9.99")*/ "" .
                         
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

        
            END.
      
        /* display v-balance with frame main.
   
         IF rd-dest = 3 THEN
           RUN excel-2-proc(INPUT v-balance).
   
         down with frame main.
         clear frame main.*/

        v-bal[4] = v-bal[4] + v-balance.
      
        IF LAST-OF(report.key-04) THEN 
        DO:
            PUT SKIP(1).

            IF report.key-04 NE ""         AND
                report.key-04 NE v-first[4] THEN 
            DO:

                /* display "Control # Totals" @ item.i-name
                         v-ord[4]           @ v-cons-qty
                         v-qty[4]           @ rm-rdtlh.qty
                         v-bal[4]           @ v-balance
                         v-amt[4]           @ v-amount
                     with frame main.
                 down with frame main.*/

                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "po"    THEN 
                            cVarValue = "" .
                        WHEN "vend"   THEN 
                            cVarValue = "".
                        WHEN "item"   THEN 
                            cVarValue = "".
                        WHEN "desc"  THEN 
                            cVarValue =  "" .
                        WHEN "cont"   THEN 
                            cVarValue = "" .
                        WHEN "pack"  THEN 
                            cVarValue =  "" .
                        WHEN "qty-ord"   THEN 
                            cVarValue = STRING(v-ord[4],"->,>>>,>>9.99") .
                        WHEN "tran-date"  THEN 
                            cVarValue = "" .
                        WHEN "tran-qty"  THEN 
                            cVarValue = STRING(v-qty[4],"->>>,>>9.99") .
                        WHEN "amt"   THEN 
                            cVarValue = STRING(v-amt[4],"->>,>>9.99") .
                        WHEN "uom"  THEN 
                            cVarValue = "" .
                        WHEN "bal-due"  THEN 
                            cVarValue = STRING(v-bal[4],"->>>,>>9.99")  .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
                PUT str-line SKIP .
                PUT UNFORMATTED 
                    "    Control # Totals "  SUBSTRING(cDisplay,22,300) SKIP.
                IF rd-dest = 3 THEN 
                DO:
                    PUT STREAM excel UNFORMATTED 
                        'Control # Totals ,' 
                        SUBSTRING(cExcelDisplay,4,300) SKIP.
                END.

                PUT SKIP(1).

            /*IF rd-dest = 3 THEN
               RUN excel-3-proc(INPUT "Control # Totals",
                                INPUT v-ord[4], INPUT v-qty[4],
                                INPUT v-bal[4], INPUT v-amt[4]).
  
            if not last-of(report.key-03) then put skip(1).*/
            END.

            ASSIGN
                v-ord[3] = v-ord[3] + v-ord[4]
                v-qty[3] = v-qty[3] + v-qty[4]
                v-bal[3] = v-bal[3] + v-bal[4]
                v-amt[3] = v-amt[3] + v-amt[4]
                v-ord[4] = 0
                v-qty[4] = 0
                v-bal[4] = 0
                v-amt[4] = 0.
        END.

        IF LAST-OF(report.key-03) THEN 
        DO:
            IF report.key-03 NE ""         AND
                report.key-03 NE v-first[3] THEN 
            DO:

                /* display "Item Totals" @ item.i-name
                         v-ord[3]      @ v-cons-qty
                         v-qty[3]      @ rm-rdtlh.qty
                         v-bal[3]      @ v-balance
                         v-amt[3]      @ v-amount
                     with frame main.
                 down with frame main.*/

                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "po"    THEN 
                            cVarValue = "" .
                        WHEN "vend"   THEN 
                            cVarValue = "".
                        WHEN "item"   THEN 
                            cVarValue = "".
                        WHEN "desc"  THEN 
                            cVarValue =  "" .
                        WHEN "cont"   THEN 
                            cVarValue = "" .
                        WHEN "pack"  THEN 
                            cVarValue =  "" .
                        WHEN "qty-ord"   THEN 
                            cVarValue = STRING(v-ord[3],"->,>>>,>>9.99") .
                        WHEN "tran-date"  THEN 
                            cVarValue = "" .
                        WHEN "tran-qty"  THEN 
                            cVarValue = STRING(v-qty[3],"->>>,>>9.99") .
                        WHEN "amt"   THEN 
                            cVarValue = STRING(v-amt[3],"->>,>>9.99") .
                        WHEN "uom"  THEN 
                            cVarValue = "" .
                        WHEN "bal-due"  THEN 
                            cVarValue = STRING(v-bal[3],"->>>,>>9.99")  .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
                PUT str-line SKIP .
                PUT UNFORMATTED 
                    "    Item # Totals "  SUBSTRING(cDisplay,19,300) SKIP.
                IF rd-dest = 3 THEN 
                DO:
                    PUT STREAM excel UNFORMATTED 
                        'Item # Totals ,' 
                        SUBSTRING(cExcelDisplay,4,300) SKIP.
                END.

                PUT SKIP(1).

                /*IF rd-dest = 3 THEN
                   RUN excel-3-proc(INPUT "Item Totals",
                                    INPUT v-ord[3], INPUT v-qty[3],
                                    INPUT v-bal[3], INPUT v-amt[3]).*/

                IF NOT LAST-OF(report.key-02) THEN PUT SKIP(1).
            END.

            ASSIGN
                v-ord[2] = v-ord[2] + v-ord[3]
                v-qty[2] = v-qty[2] + v-qty[3]
                v-bal[2] = v-bal[2] + v-bal[3]
                v-amt[2] = v-amt[2] + v-amt[3]
                v-ord[3] = 0
                v-qty[3] = 0
                v-bal[3] = 0
                v-amt[3] = 0.
        END.

        IF LAST-OF(report.key-02) THEN 
        DO:
            IF report.key-02 NE ""         AND
                report.key-02 NE v-first[2] THEN 
            DO:

                /*display "Vendor Totals" @ item.i-name
                        v-ord[2]        @ v-cons-qty
                        v-qty[2]        @ rm-rdtlh.qty
                        v-bal[2]        @ v-balance
                        v-amt[2]        @ v-amount
                    with frame main.
                down with frame main.*/

                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "po"    THEN 
                            cVarValue = "" .
                        WHEN "vend"   THEN 
                            cVarValue = "".
                        WHEN "item"   THEN 
                            cVarValue = "".
                        WHEN "desc"  THEN 
                            cVarValue =  "" .
                        WHEN "cont"   THEN 
                            cVarValue = "" .
                        WHEN "pack"  THEN 
                            cVarValue =  "" .
                        WHEN "qty-ord"   THEN 
                            cVarValue = STRING(v-ord[2],"->,>>>,>>9.99") .
                        WHEN "tran-date"  THEN 
                            cVarValue = "" .
                        WHEN "tran-qty"  THEN 
                            cVarValue = STRING(v-qty[2],"->>>,>>9.99") .
                        WHEN "amt"   THEN 
                            cVarValue = STRING(v-amt[2],"->>,>>9.99") .
                        WHEN "uom"  THEN 
                            cVarValue = "" .
                        WHEN "bal-due"  THEN 
                            cVarValue = STRING(v-bal[2],"->>>,>>9.99")  .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
                PUT str-line SKIP .
                PUT UNFORMATTED 
                    "    Vender # Totals "  SUBSTRING(cDisplay,21,300) SKIP.
                IF rd-dest = 3 THEN 
                DO:
                    PUT STREAM excel UNFORMATTED 
                        'Vender # Totals ,' 
                        SUBSTRING(cExcelDisplay,4,300) SKIP.
                END.

                PUT SKIP(2).

            /*IF rd-dest = 3 THEN
               RUN excel-3-proc(INPUT "Vendor Totals",
                                INPUT v-ord[2], INPUT v-qty[2],
                                INPUT v-bal[2], INPUT v-amt[2]).*/
            END.

            ASSIGN
                v-ord[2] = 0
                v-qty[2] = 0
                v-bal[2] = 0
                v-amt[2] = 0   .
        END.

        v-first = "".

        DELETE report.
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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE    NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-label      AS CHARACTER.
  
    lv-frame-hdl = FRAME {&frame-name}:HANDLE.
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
            fi_file:SCREEN-VALUE = "c:\tmp\r-pofghs.csv".
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
