&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: rmrep\r-mathst.w

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

DEFINE VARIABLE jmctr      AS INTEGER.
DEFINE VARIABLE dayctr     AS INTEGER.
DEFINE VARIABLE itemctr    AS INTEGER.
DEFINE VARIABLE v-sqft     AS DECIMAL.
DEFINE VARIABLE v-tot-sqft AS DECIMAL.

{ce/msfcalc.i}

DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.

DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG     NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS cha     NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS cha     NO-UNDO.
DEFINE VARIABLE cFieldLength       AS cha     NO-UNDO.
DEFINE VARIABLE cFieldType         AS cha     NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS cha       NO-UNDO.
DEFINE VARIABLE cColumnInit        AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO .
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.


ASSIGN 
    cTextListToSelect  = "Date,RM Item#,PO#,Vendor,Job#,Quantity,Total MSF," +
                           "Stocked Qty,Cons. UOM" 
    cFieldListToSelect = "date,rm-item,po,vend,job,qty,msf," +
                            "stk-qty,uom"
    cFieldLength       = "10,16,9,8,13,18,12," + "18,9"
    cFieldType         = "c,c,i,c,c,i,i," + "i,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Date,RM Item#,PO#,Vendor,Job#,Quantity,Total MSF," +
                           "Stocked Qty,Cons. UOM" 
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date end_date ~
begin_rm-no end_rm-no begin_vend end_vend select-mat btn_SelectColumns ~
rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_rm-no end_rm-no ~
begin_vend end_vend select-mat rd-dest fi_file tb_OpenCSV tbAutoClose 

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
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.29.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.29.

DEFINE BUTTON btn_SelectColumns 
     LABEL "Select Columns" 
     SIZE 43 BY 1.19.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Due Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Due Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-mathst.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 45 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE mat-types AS CHARACTER FORMAT "X(256)":U 
     LABEL "Material Types" 
     VIEW-AS FILL-IN 
     SIZE 1 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To Email", 5,
"To CSV", 3
     SIZE 15 BY 4.91 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 5.48.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 13.52.

DEFINE VARIABLE select-mat AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 44 BY 5.71 NO-UNDO.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV AS LOGICAL INITIAL no 
     LABEL "Open CSV?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_date AT ROW 2.67 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 2.67 COL 69 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     begin_rm-no AT ROW 3.62 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_rm-no AT ROW 3.62 COL 69 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_vend AT ROW 4.57 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend AT ROW 4.57 COL 69 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     select-mat AT ROW 6.95 COL 28 HELP
          "Enter description of this Material Type." NO-LABEL
     mat-types AT ROW 11.48 COL 17 COLON-ALIGNED
     btn_SelectColumns AT ROW 13.1 COL 28.4 WIDGET-ID 10
     lv-ornt AT ROW 15.52 COL 56 NO-LABEL
     sl_avail AT ROW 16 COL 3 NO-LABEL WIDGET-ID 26
     rd-dest AT ROW 16 COL 5 NO-LABEL
     lines-per-page AT ROW 16.14 COL 85 COLON-ALIGNED
     lv-font-name AT ROW 16.24 COL 32 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17.43 COL 55
     lv-font-no AT ROW 17.43 COL 84 COLON-ALIGNED
     sl_selected AT ROW 17.67 COL 15 NO-LABEL WIDGET-ID 28 NO-TAB-STOP 
     fi_file AT ROW 19.67 COL 27 COLON-ALIGNED HELP
          "Enter File Name"
     tb_OpenCSV AT ROW 19.76 COL 92.4 RIGHT-ALIGNED
     tbAutoClose AT ROW 21.24 COL 29.2 WIDGET-ID 16
     btn-ok AT ROW 22.43 COL 28.8
     btn-cancel AT ROW 22.43 COL 53.4
     "Select/Deselect Material Types" VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 6.24 COL 31
          FONT 6
     " Output Destination" VIEW-AS TEXT
          SIZE 19 BY .71 AT ROW 15.1 COL 4.4
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.1 COL 4.8
          BGCOLOR 15 
     RECT-6 AT ROW 15.52 COL 3
     RECT-7 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1
         SIZE 95.6 BY 24.52
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
         TITLE              = "Material Receipts"
         HEIGHT             = 23.33
         WIDTH              = 96.2
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
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
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lines-per-page:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-name:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-no:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-ornt:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN mat-types IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       mat-types:HIDDEN IN FRAME FRAME-A           = TRUE
       mat-types:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR SELECTION-LIST sl_avail IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       sl_avail:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR SELECTION-LIST sl_selected IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       sl_selected:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       td-show-parm:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Material Receipts */
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
ON WINDOW-CLOSE OF C-Win /* Material Receipts */
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
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Due Date */
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
        DELETE PROCEDURE hdOutputProcs.
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:

        DEFINE VARIABLE v-valid AS LOG NO-UNDO.

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
        RUN run-report(OUTPUT v-valid).

        STATUS DEFAULT "Processing Complete". 
        SESSION:SET-WAIT-STATE("").

        IF v-valid THEN
            CASE rd-dest:
                WHEN 1 THEN RUN output-to-printer.
                WHEN 2 THEN RUN output-to-screen.
                WHEN 3 THEN 
                    DO:
                        IF NOT tb_OpenCSV THEN 
                        DO:        
                            MESSAGE  "CSV file have been created." SKIP(1)
                                "~"OK~" to open CSV file?"
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
                        {custom/asifax.i &type="Vendor"
                            &begin_cust=begin_vend
                            &END_cust= begin_vend
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                    END. 
                WHEN 5 THEN 
                    DO:
                        IF is-xprint-form THEN 
                        DO:
                            {custom/asimail.i &TYPE = "Vendor"
                             &begin_cust= begin_vend
                             &END_cust=begin_vend
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                        END.
                        ELSE 
                        DO:
                            {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust= begin_vend
                                  &END_cust=begin_vend
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }
                        END.
                    END.
                WHEN 6 THEN RUN OUTPUT-to-port.
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
        DEFINE VARIABLE cTextSelected AS cha NO-UNDO.
        DEFINE VARIABLE cTextListed   AS cha NO-UNDO.

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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Due Date */
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


&Scoped-define SELF-NAME mat-types
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mat-types C-Win
ON LEAVE OF mat-types IN FRAME FRAME-A /* Material Types */
DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME select-mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL select-mat C-Win
ON VALUE-CHANGED OF select-mat IN FRAME FRAME-A
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

    ASSIGN
        begin_date = DATE(1,1,YEAR(TODAY))
        end_date   = TODAY.
    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
   
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "MR7" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    FOR EACH mat:
        v-mat-list = v-mat-list + string(mat.mat,"x(5)") + " " + mat.dscr + ",".
    END.
    IF substr(v-mat-list,LENGTH(TRIM(v-mat-list)),1) EQ "," THEN
        substr(v-mat-list,LENGTH(TRIM(v-mat-list)),1) = "".

    select-mat:list-items = v-mat-list.

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_date.
    END.
    cColumnInit   = NO .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-items C-Win 
PROCEDURE do-items :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    IF item.i-code EQ "E" THEN 
    DO:

        /*If no form number then see how many job materials 
          there are.  If there is only one, then use that one
          to calculate the MSF.  Else, if there is more than
          one, we don't know which job-mat to use, so we 
          cant't calculate the MSF.  If there are no job-mat 
          recs, then obviously we can't calculate the MSF*/

        IF rm-rdtlh.s-num EQ 0 THEN 
        DO:
            jmctr = 0.
            FOR EACH job-mat NO-LOCK 
                WHERE job-mat.company EQ item.company 
                AND job-mat.rm-i-no EQ item.i-no
                AND job-mat.job-no  EQ rm-rcpth.job-no
                AND job-mat.job-no2 EQ rm-rcpth.job-no2:
                jmctr = jmctr + 1.
            END.

            IF jmctr EQ 1 THEN 
            DO:
                FIND FIRST job-mat 
                    WHERE job-mat.company EQ item.company 
                    AND job-mat.rm-i-no EQ item.i-no
                    AND job-mat.job-no  EQ rm-rcpth.job-no
                    AND job-mat.job-no2 EQ rm-rcpth.job-no2
                    NO-LOCK NO-ERROR.

                IF AVAILABLE job-mat THEN 
                DO:
                    IF job-mat.len GT 0 AND job-mat.wid GT 0 THEN 
                    DO:
                        v-sqft = (ROUND((((IF v-corr THEN (job-mat.len * job-mat.wid) * .007
                        ELSE (job-mat.len * job-mat.wid) / 144) * 
                            rm-rdtlh.qty) / 1000),2)).
                        v-tot-sqft = v-tot-sqft + v-sqft.
                    END.
                END.  /*avail job-mat*/
            END.  /*jmctr eq 1*/
        END.   /*snum eq 0*/
    END. /*"E"*/

    ELSE 
    DO:
        FOR EACH job-mat NO-LOCK 
            WHERE job-mat.company EQ item.company 
            AND job-mat.rm-i-no EQ item.i-no
            AND job-mat.job-no  EQ rm-rcpth.job-no
            AND job-mat.job-no2 EQ rm-rcpth.job-no2
            AND job-mat.frm     EQ rm-rdtlh.s-num:

            IF job-mat.len GT 0 AND job-mat.wid GT 0 THEN 
            DO:
                v-sqft = (ROUND((((IF v-corr THEN (job-mat.len * job-mat.wid) * .007
                ELSE (job-mat.len * job-mat.wid) / 144) *
                    rm-rdtlh.qty) / 1000),1)).
                v-tot-sqft = v-tot-sqft + v-sqft.
            END.    
        END. /*for each*/
    END.

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
  DISPLAY begin_date end_date begin_rm-no end_rm-no begin_vend end_vend 
          select-mat rd-dest fi_file tb_OpenCSV tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_date end_date begin_rm-no end_rm-no begin_vend 
         end_vend select-mat btn_SelectColumns rd-dest fi_file tb_OpenCSV 
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
            fi_file:SCREEN-VALUE = "c:\tmp\r-mathst.csv".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------- rm/rep/rm-rhist.p 06/00 DJK */
    /* raw materials - transactions edit list                                     */
    /* -------------------------------------------------------------------------- */

    DEFINE OUTPUT PARAMETER op-valid AS LOG INIT TRUE NO-UNDO.

    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE v-fitem             LIKE rm-rcpth.i-no.
    DEFINE VARIABLE v-titem             LIKE v-fitem INIT "zzzzzzzzzz".
    DEFINE VARIABLE v-fpcat             LIKE item.procat.
    DEFINE VARIABLE v-tpcat             LIKE v-fpcat INIT "zzzzz".
    DEFINE VARIABLE v-fdate             AS DATE      FORMAT "99/99/9999".
    DEFINE VARIABLE v-tdate             LIKE v-fdate INIT TODAY.
    DEFINE VARIABLE v-floc              LIKE rm-rcpth.loc.
    DEFINE VARIABLE v-tloc              LIKE v-floc INITIAL "zzzzz".
    DEFINE VARIABLE v-type              AS CHARACTER FORMAT "x(5)" INIT "RITAC".
    DEFINE VARIABLE v-code              LIKE rm-rcpth.rita-code.
    DEFINE VARIABLE v-job-no            AS CHARACTER FORMAT "x(13)".
    DEFINE VARIABLE v-fvend             LIKE item.vend-no.
    DEFINE VARIABLE v-tvend             LIKE item.vend-no INIT "zzzzzzzzzz".
    DEFINE VARIABLE v-qty               AS DECIMAL.         
    DEFINE VARIABLE v-msf               AS DECIMAL.
    DEFINE VARIABLE v-tot-msf-item      AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
    DEFINE VARIABLE v-tot-msf-day       AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
    DEFINE VARIABLE v-grand-tot-msf     AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
    DEFINE VARIABLE v-mtype             AS CHARACTER FORMAT "x(47)".

    DEFINE VARIABLE v-tot-qty           LIKE rm-rdtlh.qty FORMAT "->>>,>>>,>>9.99".
    DEFINE VARIABLE v-grand-tot-qty     LIKE rm-rdtlh.qty FORMAT "->>>,>>>,>>9.99".
    DEFINE VARIABLE v-len               LIKE po-ordl.s-len.
    DEFINE VARIABLE v-wid               LIKE po-ordl.s-wid.
    DEFINE VARIABLE v-stocked-qty       AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
    DEFINE VARIABLE v-tot-stk-qty-item  AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
    DEFINE VARIABLE v-tot-stk-qty-day   AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
    DEFINE VARIABLE v-grand-tot-stk-qty AS DECIMAL   FORMAT "->>>,>>>,>>9.99".

    DEFINE VARIABLE cDisplay            AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelDisplay       AS cha       NO-UNDO.
    DEFINE VARIABLE hField              AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField           AS CHA       NO-UNDO.
    DEFINE VARIABLE cVarValue           AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelVarValue      AS cha       NO-UNDO.
    DEFINE VARIABLE cSelectedList       AS cha       NO-UNDO.
    DEFINE VARIABLE cFieldName          AS cha       NO-UNDO.
    DEFINE VARIABLE str-tit4            AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5            AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line            AS cha       FORM "x(300)" NO-UNDO.

    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.  

    {custom/statusMsg.i "'Processing...'"} 

    FORM 
        rm-rcpth.trans-date                    LABEL "Date"      /*8*/
        rm-rcpth.i-no FORMAT "X(10)"           LABEL "RM Item#"  /*10*/
        rm-rcpth.po-no                         LABEL "PO#"       /*9*/
        po-ord.vend-no                         LABEL "Vendor"    /*8*/
        v-job-no                               LABEL "Job#"      /*9*/
        rm-rdtlh.qty FORMAT "->>>,>>>,>>9.99"  LABEL "Quantity"  /*15*/
        v-msf FORMAT  "->>,>>9.9999"           LABEL "Total MSF" /*15*/
        v-stocked-qty                          LABEL "Stocked Qty"
        item.cons-uom                          LABEL "Cons. UOM"
        WITH FRAME itemx DOWN NO-BOX STREAM-IO WIDTH 132.


    {sa/sa-sls01.i}

    FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-fdate  = begin_date
        v-tdate  = end_date
        v-fitem  = begin_rm-no
        v-titem  = end_rm-no
        v-fvend  = begin_vend
        v-tvend  = end_vend
        v-mtype  = "".

    DO WITH FRAME {&frame-name}:          
        DO i = 1 TO select-mat:num-items:
            IF select-mat:is-selected(i) THEN
                v-mtype = v-mtype + trim(substr(select-mat:entry(i),1,5)) + ",".
        END.

        IF LENGTH(TRIM(v-mtype)) EQ 0 THEN
        DO:
            MESSAGE "No Material Type Selected."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            op-valid = FALSE.
            LEAVE.
        END.

        IF substr(v-mtype,LENGTH(TRIM(v-mtype)),1) EQ "," THEN
            substr(v-mtype,LENGTH(TRIM(v-mtype)),1) = "".

        mat-types = v-mtype.

        DO i = 1 TO LENGTH(mat-types):
            IF substr(mat-types,i,1) EQ "," THEN substr(mat-types,i,1) = " ".
        END.

        DISPLAY mat-types.
    END.


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


        IF LOOKUP(ttRptSelected.TextList, "Quantity,Total MSF,Stocked Qty") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    IF rd-dest EQ 3 THEN 
    DO:

        OUTPUT STREAM excel TO VALUE(cFileName).
        /*excelheader = "Date,RM Item#,PO#,Vendor,Job#,Quantity,Total MSF,Stocked Qty,Cons. UOM".                        */
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.      
    END.

    SESSION:SET-WAIT-STATE ("general").

    DISPLAY "" WITH FRAME r-top.

    ASSIGN
        dayctr  = 0
        itemctr = 0.

    FOR EACH rm-rcpth
        WHERE rm-rcpth.company    EQ cocode
        AND rm-rcpth.trans-date GE v-fdate
        AND rm-rcpth.trans-date LE v-tdate
        AND rm-rcpth.i-no       GE v-fitem
        AND rm-rcpth.i-no       LE v-titem
        AND rm-rcpth.rita-code  EQ "R"
        USE-INDEX i-no NO-LOCK,

        EACH rm-rdtlh
        WHERE rm-rdtlh.r-no       EQ rm-rcpth.r-no
        AND rm-rdtlh.rita-code  EQ rm-rcpth.rita-code
        NO-LOCK,

        FIRST item
        WHERE item.company EQ cocode
        AND item.i-no    EQ rm-rcpth.i-no
        AND lookup(item.mat-type,v-mtype) GT 0
        NO-LOCK

        BREAK BY rm-rcpth.trans-date
        BY rm-rcpth.i-no:

        {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

        FIND FIRST po-ord
            WHERE po-ord.company EQ cocode
            AND po-ord.po-no   EQ int(rm-rcpth.po-no)
            NO-LOCK NO-ERROR.

        IF (AVAILABLE po-ord AND
            po-ord.vend-no GE v-fvend AND
            po-ord.vend-no LE  v-tvend)   OR
            (NOT AVAILABLE po-ord)             THEN 
        DO:

            ASSIGN
                dayctr        = dayctr + 1
                itemctr       = itemctr + 1
                v-job-no      = ""
                v-len         = item.s-len
                v-wid         = IF item.r-wid GT 0 THEN item.r-wid ELSE item.s-wid
                v-stocked-qty = 0.

            IF TRIM(rm-rdtlh.job-no) NE "" THEN 
                v-job-no = TRIM(rm-rdtlh.job-no) + "-" + string(rm-rdtlh.job-no2,"999").

            /* run do-items. */

            RELEASE po-ordl.

            IF AVAILABLE po-ord THEN
                FOR EACH po-ordl
                    WHERE po-ordl.company EQ cocode
                    AND po-ordl.po-no   EQ po-ord.po-no
                    AND po-ordl.i-no    EQ rm-rcpth.i-no
                    AND po-ordl.job-no  EQ rm-rcpth.job-no
                    AND po-ordl.job-no2 EQ rm-rcpth.job-no2
                    NO-LOCK
                    BY po-ordl.s-num DESCENDING:

                    ASSIGN
                        v-len = po-ordl.s-len
                        v-wid = po-ordl.s-wid.

                    IF po-ordl.s-num EQ rm-rdtlh.s-num THEN LEAVE.
                END.

            IF NOT AVAILABLE po-ordl AND rm-rcpth.job-no NE "" THEN
                FOR EACH job-mat
                    WHERE job-mat.company EQ cocode
                    AND job-mat.rm-i-no EQ item.i-no
                    AND job-mat.job-no  EQ rm-rcpth.job-no
                    AND job-mat.job-no2 EQ rm-rcpth.job-no2
                    NO-LOCK
                    BY job-mat.frm DESCENDING:

                    ASSIGN
                        v-len = job-mat.len
                        v-wid = job-mat.wid.

                    IF job-mat.frm EQ rm-rdtlh.s-num THEN LEAVE.  
                END.

            IF v-len EQ 0 THEN v-len = 12.
            IF v-wid EQ 0 THEN v-wid = 12.

            v-qty = rm-rdtlh.qty.

            IF rm-rcpth.pur-uom NE "EA" THEN
                RUN sys/ref/convquom.p (rm-rcpth.pur-uom, "EA",
                    item.basis-w, v-len, v-wid, item.s-dep,
                    v-qty, OUTPUT v-qty).

            {sys/inc/roundup.i v-qty}

            v-msf = (IF v-corr THEN (v-len * v-wid * .007)
            ELSE (v-len * v-wid / 144)) * v-qty / 1000.

            /* calculate stocked qty */

            CASE item.cons-uom:
                WHEN "LB" THEN 
                    v-stocked-qty = (v-msf * ITEM.basis-w). 
                WHEN "TON" THEN
                    v-stocked-qty = (v-msf * ITEM.basis-w) / 2000.
                WHEN "LF" THEN
                    v-stocked-qty = v-qty.
                WHEN "MSF" THEN
                    v-stocked-qty = v-msf * ITEM.basis-w.
            END CASE.

            ASSIGN
                v-tot-msf-item      = v-tot-msf-item + v-msf
                v-tot-msf-day       = v-tot-msf-day + v-msf
                v-grand-tot-msf     = v-grand-tot-msf + v-msf
                v-tot-qty           = v-tot-qty + v-qty
                v-tot-stk-qty-item  = v-tot-stk-qty-item + v-stocked-qty    
                v-tot-stk-qty-day   = v-tot-stk-qty-day + v-stocked-qty   
                v-grand-tot-stk-qty = v-grand-tot-stk-qty + v-stocked-qty.


            /* display rm-rcpth.trans-date when first-of(rm-rcpth.trans-date)
                     rm-rcpth.i-no  
                     rm-rcpth.po-no 
                     po-ord.vend-no      when avail po-ordl
                     v-job-no  
                     v-qty  @ rm-rdtlh.qty 
                     v-msf
                     v-stocked-qty
                     item.cons-uom
                 with frame itemx.
             down with frame itemx.*/

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "date"    THEN 
                        cVarValue = STRING(rm-rcpth.trans-date,"99/99/9999").
                    WHEN "rm-item"   THEN 
                        cVarValue =  STRING(rm-rcpth.i-no,"x(16)").
                    WHEN "po"   THEN 
                        cVarValue = STRING(rm-rcpth.po-no,"x(9)").
                    WHEN "vend"  THEN 
                        cVarValue = IF AVAILABLE po-ordl THEN STRING(po-ord.vend-no,"x(8)") ELSE "" .
                    WHEN "job"   THEN 
                        cVarValue = STRING(v-job-no,"x(13)") .
                    WHEN "qty"  THEN 
                        cVarValue = STRING(v-qty,"->>,>>>,>>>,>>9.99") .
                    WHEN "msf"   THEN 
                        cVarValue =  STRING(v-msf,"->>>>,>>9.99<<") .
                    WHEN "stk-qty"  THEN 
                        cVarValue =  STRING(v-stocked-qty,"->>,>>>,>>>,>>9.99")  .
                    WHEN "uom"  THEN 
                        cVarValue =  STRING(item.cons-uom,"x(9)")  .

                END CASE.

                IF  cTmpField = "date" THEN
                     cExcelVarValue = IF rm-rcpth.trans-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",rm-rcpth.trans-date) ELSE "".
                ELSE cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,cExcelVarValue)) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest EQ 3 THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
            END.

        END.

        IF LAST-OF(rm-rcpth.i-no) THEN 
        DO:
            IF itemctr GT 0 THEN 
            DO:
                PUT str-line SKIP .
                /*display "--------------------" @ rm-rdtlh.qty 
                        "------------" @ v-msf 
                        "--------------------" @ v-stocked-qty
                   with frame itemx.
                down with frame itemx.
        
                display v-tot-qty       @ rm-rdtlh.qty
                        v-tot-msf-item  @ v-msf
                        v-tot-stk-qty-item @ v-stocked-qty
                    with frame itemx.
                down with frame itemx.*/
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "date"    THEN 
                            cVarValue = "".
                        WHEN "rm-item"   THEN 
                            cVarValue =  "".
                        WHEN "po"   THEN 
                            cVarValue = "" .
                        WHEN "vend"  THEN 
                            cVarValue = "" .
                        WHEN "job"   THEN 
                            cVarValue = "" .
                        WHEN "qty"  THEN 
                            cVarValue = STRING(v-tot-qty,"->>,>>>,>>>,>>9.99") .
                        WHEN "msf"   THEN 
                            cVarValue =  STRING(v-tot-msf-item,"->>>>,>>9.99<<") .
                        WHEN "stk-qty"  THEN 
                            cVarValue =  STRING(v-tot-stk-qty-item,"->>,>>>,>>>,>>9.99")  .
                        WHEN "uom"  THEN 
                            cVarValue =  ""  .

                    END CASE.
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.

                PUT UNFORMATTED cDisplay SKIP(1).
                IF rd-dest EQ 3 THEN 
                DO:
                    PUT STREAM excel UNFORMATTED  
                        cExcelDisplay SKIP.
                END.


            END.    /* last of rm-rcpth.i-no */                

            ASSIGN
                v-tot-msf-item     = 0
                v-tot-stk-qty-item = 0
                itemctr            = 0
                v-grand-tot-qty    = v-grand-tot-qty + v-tot-qty
                v-tot-qty          = 0.
        END.

        IF LAST-OF(rm-rcpth.trans-date) THEN 
        DO:
            IF dayctr GT 0 THEN 
            DO:
                /*display space(10) " Totals For Day:" @ rm-rcpth.i-no
                        v-tot-msf-day         @ v-msf
                        v-tot-stk-qty-day     @ v-stocked-qty
                        skip(1)
                    with frame itemx.
                down with frame itemx.*/

                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "date"    THEN 
                            cVarValue = "".
                        WHEN "rm-item"   THEN 
                            cVarValue =  "".
                        WHEN "po"   THEN 
                            cVarValue = "" .
                        WHEN "vend"  THEN 
                            cVarValue = "" .
                        WHEN "job"   THEN 
                            cVarValue = "" .
                        WHEN "qty"  THEN 
                            cVarValue = "" .
                        WHEN "msf"   THEN 
                            cVarValue =  STRING(v-tot-msf-day,"->>>>,>>9.99<<") .
                        WHEN "stk-qty"  THEN 
                            cVarValue =  STRING(v-tot-stk-qty-day,"->>,>>>,>>>,>>9.99")  .
                        WHEN "uom"  THEN 
                            cVarValue =  ""  .

                    END CASE.
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.

                PUT UNFORMATTED  
                    "        Totals For Day:" SUBSTRING(cDisplay,24,300) SKIP(1).

            END. /* last-of(rm-rcpth.trans-date) */

            ASSIGN
                v-tot-msf-day     = 0
                v-tot-stk-qty-day = 0
                dayctr            = 0.
        END.

        IF LAST(rm-rcpth.trans-date) THEN 
        DO:
            /*display "Grand Totals:" @ rm-rcpth.i-no
                    v-grand-tot-qty @ rm-rdtlh.qty
                    v-grand-tot-msf @ v-msf
                    v-grand-tot-stk-qty @ v-stocked-qty
                with frame itemx.                            
            down with frame itemx.*/
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "date"    THEN 
                        cVarValue = "".
                    WHEN "rm-item"   THEN 
                        cVarValue =  "".
                    WHEN "po"   THEN 
                        cVarValue = "" .
                    WHEN "vend"  THEN 
                        cVarValue = "" .
                    WHEN "job"   THEN 
                        cVarValue = "" .
                    WHEN "qty"  THEN 
                        cVarValue = STRING(v-grand-tot-qty,"->>,>>>,>>>,>>9.99") .
                    WHEN "msf"   THEN 
                        cVarValue =  STRING(v-grand-tot-msf,"->>>>,>>9.99<<") .
                    WHEN "stk-qty"  THEN 
                        cVarValue =  STRING(v-grand-tot-stk-qty,"->>,>>>,>>>,>>9.99")  .
                    WHEN "uom"  THEN 
                        cVarValue =  ""  .

                END CASE.
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED  
                "        Grand Totals::" SUBSTRING(cDisplay,23,300) SKIP(1).

        END.  /* last trans date */
    END.  /*main for each*/

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    IF rd-dest EQ 3 THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

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

    PUT SPACE(15)
        "< Selection Parameters >"
        SKIP(1).

    DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
        IF ENTRY(i,parm-fld-list) NE "" OR
            entry(i,parm-lbl-list) NE "" THEN 
        DO:

            lv-label = FILL(" ",24 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                trim(ENTRY(i,parm-lbl-list)) + ":".

            PUT lv-label FORMAT "x(25)" AT 5
                SPACE(1)
                TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(50)"
                SKIP.              
        END.
    END.

    PUT FILL("-",80) FORMAT "x(80)" SKIP.

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

