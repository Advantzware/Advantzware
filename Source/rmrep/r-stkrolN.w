&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: rmrep\r-stkrol.w

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
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO .


ASSIGN 
    cTextListToSelect  = "RM Item#,Description,Tag#,Lineal Feet,Totals Rolls,Roll Weight," +
                           "Roll Width,Wt/MSF,Vendor Tag #"

    cFieldListToSelect = "rm-item,desc,tag,lin-ft,tot-rol,rol-wt," + 
                            "rol-wid,wt-msf,vend-tag"

    cFieldLength       = "10,25,20,12,12,11," + "10,8,20"
    cFieldType         = "c,c,c,i,i,i," + "i,i,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "RM Item#,Description,Tag#,Lineal Feet,Totals Rolls" 
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_rm-no end_rm-no ~
begin_procat end_procat begin_whs end_whs tg-paper tg-board sl_avail ~
Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file ~
tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_rm-no end_rm-no begin_procat ~
end_procat begin_whs end_whs tg-paper tg-board sl_avail sl_selected rd-dest ~
fi_file tb_OpenCSV tbAutoClose 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_whs AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_whs AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-stkrol.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 43 BY 1.

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
     SIZE 17 BY 4.52 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 5.48.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 6.24.

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
     SIZE 16 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg-board AS LOGICAL INITIAL no 
     LABEL "Board" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY 1 NO-UNDO.

DEFINE VARIABLE tg-paper AS LOGICAL INITIAL yes 
     LABEL "Paper" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_rm-no AT ROW 2.43 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_rm-no AT ROW 2.43 COL 69 COLON-ALIGNED HELP
          "Enter Ending Item number"
     begin_procat AT ROW 3.38 COL 28 COLON-ALIGNED HELP
          "Enter Begining Category"
     end_procat AT ROW 3.38 COL 69 COLON-ALIGNED HELP
          "Enter Ending Category"
     begin_whs AT ROW 4.33 COL 28 COLON-ALIGNED HELP
          "Enter Beginng Warehouse"
     end_whs AT ROW 4.33 COL 69 COLON-ALIGNED HELP
          "Enter Endng Warehouse"
     begin_date AT ROW 5.29 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 5.29 COL 69 COLON-ALIGNED HELP
          "Enter ending Date"
     tg-paper AT ROW 6.52 COL 45
     tg-board AT ROW 6.52 COL 57
     sl_avail AT ROW 8.62 COL 4 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 8.62 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 8.62 COL 59.4 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 9.62 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 10.62 COL 40 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 11.67 COL 40 WIDGET-ID 40
     btn_down AT ROW 12.67 COL 40 WIDGET-ID 42
     lv-ornt AT ROW 14.33 COL 43 NO-LABEL
     lv-font-no AT ROW 14.33 COL 77 COLON-ALIGNED
     rd-dest AT ROW 14.81 COL 5 NO-LABEL
     td-show-parm AT ROW 15.05 COL 35
     lines-per-page AT ROW 15.14 COL 84 COLON-ALIGNED
     lv-font-name AT ROW 16 COL 28 COLON-ALIGNED NO-LABEL
     fi_file AT ROW 18.29 COL 27 COLON-ALIGNED HELP
          "Enter File Name"
     tb_OpenCSV AT ROW 18.33 COL 88 RIGHT-ALIGNED
     tbAutoClose AT ROW 20 COL 29.2 WIDGET-ID 16
     btn-ok AT ROW 21.05 COL 28.8
     btn-cancel AT ROW 21.05 COL 57
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 7.91 COL 4.6 WIDGET-ID 38
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 7.91 COL 59.4 WIDGET-ID 44
     " Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.05 COL 5.6
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.1 COL 5.2
          BGCOLOR 15 
     "Material Type:" VIEW-AS TEXT
          SIZE 17 BY 1 AT ROW 6.52 COL 27
          FONT 6
     RECT-6 AT ROW 14.33 COL 4
     RECT-7 AT ROW 1.48 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 94.4 BY 23.62
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
         TITLE              = "Roll Stock Lot/Tag Report"
         HEIGHT             = 21.76
         WIDTH              = 94.8
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
/* SETTINGS FOR FILL-IN begin_date IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       begin_date:HIDDEN IN FRAME FRAME-A           = TRUE
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

/* SETTINGS FOR FILL-IN end_date IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       end_date:HIDDEN IN FRAME FRAME-A           = TRUE
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
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

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       td-show-parm:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       tg-board:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tg-paper:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Roll Stock Lot/Tag Report */
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
ON WINDOW-CLOSE OF C-Win /* Roll Stock Lot/Tag Report */
DO:
        /* This event will close the window and terminate the procedure.  */
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
ON HELP OF begin_procat IN FRAME FRAME-A /* Beginning Category */
DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.
        RUN windows/l-rmcat.w (cocode,SELF:SCREEN-VALUE,OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN SELF:SCREEN-VALUE = ENTRY(1,char-val).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define SELF-NAME begin_whs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_whs C-Win
ON LEAVE OF begin_whs IN FRAME FRAME-A /* Beginning Warehouse */
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
        STATUS DEFAULT "Processing Complete". 
        SESSION:SET-WAIT-STATE ("").

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
                    {custom/asifax.i &type= ''
                            &begin_cust= "begin_procat"
                            &END_cust= "begin_procat" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = ''
                             &begin_cust= "begin_procat"
                             &END_cust= "begin_procat"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = ''
                                  &begin_cust="begin_procat"
                                  &END_cust="begin_procat"
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
ON HELP OF end_procat IN FRAME FRAME-A /* Ending Category */
DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.
        RUN windows/l-rmcat.w (cocode,SELF:SCREEN-VALUE,OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN SELF:SCREEN-VALUE = ENTRY(1,char-val).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define SELF-NAME end_whs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_whs C-Win
ON LEAVE OF end_whs IN FRAME FRAME-A /* Ending Warehouse */
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
    {sys/inc/reportsConfigNK1.i "MR9" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-msf C-Win 
PROCEDURE calc-msf :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-msf-qty AS DECIMAL NO-UNDO.

    DEFINE VARIABLE v-len       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-wid       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-dep       LIKE po-ordl.s-len NO-UNDO. 
    DEFINE VARIABLE v-bwt       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE lv-out-qty  LIKE rm-rctd.qty NO-UNDO.
    DEFINE VARIABLE lv-out-cost LIKE rm-rctd.cost NO-UNDO.
    DEFINE VARIABLE lv-qty-uom  LIKE rm-rctd.pur-uom NO-UNDO.
    DEFINE VARIABLE lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.

    IF AVAILABLE item THEN v-dep = item.s-dep.      

    FIND FIRST po-ordl WHERE po-ordl.company = rm-rcpth.company
        AND po-ordl.po-no = integer(rm-rctd.po-no)
        AND po-ordl.i-no  = rm-rcpth.i-no
        AND po-ordl.job-no = rm-rdtlh.job-no
        AND po-ordl.job-no2 = rm-rdtlh.job-no2
        AND po-ordl.item-type = YES 
        AND po-ordl.s-num = rm-rdtlh.s-num
        NO-LOCK NO-ERROR.
    /*if not avail po-ordl then return.  */

    IF AVAILABLE po-ordl THEN 
    DO:
        ASSIGN  
            v-len       = po-ordl.s-len
            v-wid       = po-ordl.s-wid
            v-bwt       = 0
            lv-qty-uom  = po-ordl.cons-uom
            lv-cost-uom = po-ordl.cons-uom.
        {rm/pol-dims.i}
    END.
    ELSE 
    DO:
        FIND FIRST job WHERE job.company EQ cocode
            AND job.job-no  EQ rm-rdtlh.job-no
            AND job.job-no2 EQ rm-rdtlh.job-no2
            NO-LOCK NO-ERROR.
        IF AVAILABLE job THEN 
        DO :
            FIND FIRST job-mat WHERE job-mat.company EQ cocode
                AND job-mat.job     EQ job.job
                AND job-mat.i-no    EQ rm-rcpth.i-no
                AND job-mat.frm     EQ rm-rdtlh.s-num
                NO-LOCK NO-ERROR.
            IF AVAILABLE job-mat THEN ASSIGN v-len = job-mat.len
                    v-wid = job-mat.wid
                    v-bwt = job-mat.basis-w
                    .
        END.
        IF v-len EQ 0 THEN v-len = IF AVAILABLE item THEN item.s-len ELSE 0.
        IF v-wid EQ 0 THEN v-wid = IF AVAILABLE item AND item.r-wid NE 0 THEN item.r-wid ELSE IF AVAILABLE item THEN item.s-wid ELSE 0.
        IF v-bwt EQ 0 THEN v-bwt = IF AVAILABLE item THEN item.basis-w ELSE 0.

    END.

    /* convert qty    pr-qty-uom or po-ordl.pr-uom cons-uom*/
    /* run rm/convquom.p(rm-rctd.pur-uom,
                       po-ordl.cons-uom,
                            v-bwt,
                            v-len,
                            input v-wid,
                            input v-dep,
                            input rm-rctd.qty,
                            output lv-out-qty).
   
     /* convert cost pr-uom*/
     run rm/convcuom.p(rm-rctd.cost-uom, po-ordl.cons-uom,
                       v-bwt, v-len, v-wid, v-dep,
                                  rm-rctd.cost, output lv-out-cost).
     */
    RUN rm/convquom.p("LF",
        "MSF",
        v-bwt,
        v-len,
        INPUT v-wid,
        INPUT v-dep,
        rm-rdtlh.qty,
        OUTPUT lv-out-qty).
    op-msf-qty = lv-out-qty.

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
  DISPLAY begin_rm-no end_rm-no begin_procat end_procat begin_whs end_whs 
          tg-paper tg-board sl_avail sl_selected rd-dest fi_file tb_OpenCSV 
          tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_rm-no end_rm-no begin_procat end_procat begin_whs 
         end_whs tg-paper tg-board sl_avail Btn_Def sl_selected Btn_Add 
         Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV tbAutoClose 
         btn-ok btn-cancel 
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
    /*      DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.      */
    /*                                                         */
    /*      if init-dir = "" then init-dir = "c:\temp" .       */
    /*      SYSTEM-DIALOG GET-FILE list-name                   */
    /*          TITLE      "Enter Listing Name to SAVE AS ..." */
    /*          FILTERS    "Listing Files (*.rpt)" "*.rpt",    */
    /*                     "All Files (*.*)" "*.*"             */
    /*          INITIAL-DIR init-dir                           */
    /*          ASK-OVERWRITE                                  */
    /*     /*     CREATE-TEST-FILE*/                           */
    /*          SAVE-AS                                        */
    /*          USE-FILENAME                                   */
    /*                                                         */
    /*          UPDATE OKpressed.                              */
    /*                                                         */
    /*      IF NOT OKpressed THEN  RETURN NO-APPLY.            */

    {custom/out2file.i}

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
            fi_file:SCREEN-VALUE = "c:\tmp\r-stkrol.csv".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------- rm/rep/rm-trans.p 07/98 JLF */
    /* raw materials - transactions edit list                                     */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE v-fitem        LIKE rm-rcpth.i-no.
    DEFINE VARIABLE v-titem        LIKE v-fitem INIT "zzzzzzzzzz".
    DEFINE VARIABLE v-fpcat        LIKE item.procat.
    DEFINE VARIABLE v-tpcat        LIKE v-fpcat INIT "zzzzz".
    DEFINE VARIABLE v-fdate        AS DATE      FORMAT "99/99/9999" INIT 01/01/0001.
    DEFINE VARIABLE v-tdate        LIKE v-fdate INIT TODAY.
    DEFINE VARIABLE v-floc         LIKE rm-rcpth.loc.
    DEFINE VARIABLE v-tloc         LIKE v-floc INITIAL "zzzzz".
    DEFINE VARIABLE v-type         AS CHARACTER FORMAT "x(5)" INIT "RITAC".
    DEFINE VARIABLE v-code         LIKE rm-rcpth.rita-code.

    DEFINE VARIABLE v-value        AS DECIMAL   FORMAT "->>,>>>,>>9.99".
    DEFINE VARIABLE v-job-no       AS CHARACTER FORMAT "x(9)".
    DEFINE VARIABLE v-qty          LIKE rm-rdtlh.qty EXTENT 3.
    DEFINE VARIABLE v-val          LIKE v-value EXTENT 3.
    DEFINE VARIABLE v-roll-qty     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-roll-tot     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-first        AS LOG       EXTENT 3.
    DEFINE VARIABLE v-mattype      AS cha       NO-UNDO.
    DEFINE VARIABLE v-msf-qty      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dLinerFeet     AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dWeight        AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE cVendorTag     AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cDisplay       AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS cha       NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHA       NO-UNDO.
    DEFINE VARIABLE cVarValue      AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS cha       NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS cha       NO-UNDO.
    DEFINE VARIABLE cFieldName     AS cha       NO-UNDO.
    DEFINE VARIABLE str-tit4       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line       AS cha       FORM "x(300)" NO-UNDO.

    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.


    FORM rm-bin.i-no LABEL "ITEM"
        ITEM.i-name FORMAT "x(14)" LABEL "DESCRIPTION"
        /* rm-rcpth.po-no label "P.O.#"
         rm-rcpth.rita-code label "TY"  
         v-job-no label "   Job #"  */
        rm-bin.tag LABEL "TAG#"
        rm-bin.qty FORMAT "->>>>>9.99<<" LABEL "QUANTITY"
        /*v-msf-qty LABEL "MSF"
         ITEM.basis-w LABEL "Weight"
         rm-rdtlh.loc label "WHSE"
         rm-rdtlh.loc-bin label "BIN"
         rm-rdtlh.cost format "->>>>>9.99<<<<" label "COST" 
         space(0)                                             
         v-value label "VALUE"                                */
        v-roll-qty LABEL "ROLLS"
        SKIP
        WITH FRAME itemx NO-BOX DOWN STREAM-IO WIDTH 132.

    {sa/sa-sls01.i}


    ASSIGN
        str-tit2  = c-win:TITLE 
        {sys/inc/ctrtext.i str-tit2 112}

        v-fitem   = begin_rm-no
        v-titem   = end_rm-no
        v-fpcat   = begin_procat
        v-tpcat   = end_procat
        v-fdate   = begin_date
        v-tdate   = end_date
        v-floc    = begin_whs
        v-tloc    = end_whs
        v-type    = "R,I,T,A,C"
        v-mattype = "" .

    /*excelheader = "RM Item#,Description,Tag#,Lineal Feet,Totals Rolls".*/

    IF tg-paper AND tg-board THEN v-mattype = "B,P".
    ELSE IF tg-paper THEN v-mattype = "P".
        ELSE IF tg-board THEN v-mattype = "B".

    IF rd-dest EQ 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
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

        IF LOOKUP(ttRptSelected.TextList, "RM Item#,Description,Lineal Feet,Totals Rolls") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE("general").

    DISPLAY "" WITH FRAME r-top.

    IF rd-dest EQ 3 THEN PUT STREAM excel UNFORMATTED excelheader SKIP.

    FOR EACH rm-bin WHERE rm-bin.company = cocode
        AND rm-bin.i-no GE v-fitem
        AND rm-bin.i-no LE v-titem
        NO-LOCK,
        FIRST item
        WHERE item.company EQ cocode
        AND item.i-no    EQ rm-bin.i-no
        AND item.procat                      GE v-fpcat
        AND item.procat                      LE v-tpcat
        AND lookup(ITEM.mat-type,v-mattype) > 0 NO-LOCK
        BREAK /*by item.procat*/  BY rm-bin.i-no BY rm-bin.tag :

        {custom/statusMsg.i "'Processing Item # ' + string(item.i-no)"} 
     
        IF rm-bin.tag <> "" THEN ASSIGN v-roll-qty = v-roll-qty + 1.
        v-roll-tot = v-roll-tot + 1.

        dLinerFeet = rm-bin.qty . 
        IF item.cons-uom NE "LF" THEN
            RUN rm/convquom.p(item.cons-uom, "LF",
                ITEM.basis-w, ITEM.s-len,(IF item.r-wid NE 0 THEN item.r-wid ELSE item.s-wid), ITEM.s-dep,
                dLinerFeet, OUTPUT dLinerFeet).
        dWeight = rm-bin.qty . 
        IF item.cons-uom NE "LB" THEN
            RUN rm/convquom.p(item.cons-uom, "LB",
                ITEM.basis-w, ITEM.s-len, (IF item.r-wid NE 0 THEN item.r-wid ELSE item.s-wid), ITEM.s-dep,
                dWeight, OUTPUT dWeight).

        ASSIGN 
            cVendorTag = "" .
        FIND FIRST loadtag NO-LOCK
            WHERE loadtag.company EQ cocode
            AND loadtag.item-type EQ YES
            AND loadtag.tag-no EQ rm-bin.tag            
            NO-ERROR.
        IF AVAILABLE loadtag THEN
            cVendorTag = loadtag.misc-char[1] .

        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "rm-item"    THEN 
                    cVarValue = IF FIRST-OF(rm-bin.i-no) THEN STRING(rm-bin.i-no,"x(10)") ELSE "" .
                WHEN "desc"   THEN 
                    cVarValue = IF FIRST-OF(rm-bin.i-no) THEN STRING(item.i-name,"x(25)") ELSE "".
                WHEN "tag"   THEN 
                    cVarValue = STRING(rm-bin.tag,"x(20)").
                WHEN "lin-ft"  THEN 
                    cVarValue = STRING(dLinerFeet,"->>>>>>>9.99<<") .
                WHEN "tot-rol"   THEN 
                    cVarValue = IF LAST-OF(rm-bin.i-no) THEN STRING(v-roll-qty,"->>>,>>>,>>9") ELSE "" .
                WHEN "rol-wt"   THEN 
                    cVarValue = STRING(dWeight,"->>,>>9.99").
                WHEN "rol-wid"  THEN 
                    cVarValue = IF item.r-wid NE 0 THEN STRING(ITEM.r-wid,"->>,>>9.99") ELSE STRING(ITEM.s-wid,"->>,>>9.99") .
                WHEN "wt-msf"   THEN 
                    cVarValue = STRING(ITEM.basis-w,">>>>9.99")  .
                WHEN "vend-tag" THEN 
                    cVarValue = STRING(cVendorTag,"x(20)").

            END CASE.

            cExcelVarValue = IF cTmpField EQ "rm-item" THEN STRING(rm-bin.i-no,"x(10)") ELSE IF cTmpField = "desc" THEN STRING(item.i-name,"x(25)") ELSE cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.

        PUT UNFORMATTED cDisplay SKIP.
        IF rd-dest EQ 3 THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                cExcelDisplay SKIP.
        END.

        IF LAST-OF(rm-bin.i-no)
            THEN 
        DO:

            PUT str-line SKIP (1).
            v-roll-qty = 0.
        END.

        IF LAST(rm-bin.i-no) THEN 
        DO:
        
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "rm-item"    THEN 
                        cVarValue = "" .
                    WHEN "desc"   THEN 
                        cVarValue = "".
                    WHEN "tag"   THEN 
                        cVarValue = "".
                    WHEN "lin-ft"  THEN 
                        cVarValue = "" /*STRING(rm-bin.qty,"->>>>>9.99<<")*/ .
                    WHEN "tot-rol"   THEN 
                        cVarValue = STRING(v-roll-tot,"->>,>>>,>>9") .
                    WHEN "rol-wt"   THEN 
                        cVarValue = "".
                    WHEN "rol-wid"  THEN 
                        cVarValue = "" .
                    WHEN "wt-msf"   THEN 
                        cVarValue = ""  .
                    WHEN "vend-tag" THEN 
                        cVarValue = "" .

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED 
                "       GRAND TOTALS: " SUBSTRING(cDisplay,21,300) SKIP.
            IF rd-dest EQ 3 THEN 
            DO:
                PUT STREAM excel UNFORMATTED 
                    ' GRAND TOTALS  ,'   
                    SUBSTRING(cExcelDisplay,4,300) SKIP.
            END.
        END.

    END. /* for each */

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

    IF rd-dest EQ 3 THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.  

/* end ---------------------------------- copr. 1998  advanced software, inc. */

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

