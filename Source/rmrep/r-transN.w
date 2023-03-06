&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: rmrep\r-transN.w

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

DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.

DEFINE STREAM excel.


DEFINE VARIABLE ll-secure          AS LOG       NO-UNDO.

DEFINE VARIABLE excel-header-var-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE excel-header-var-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE excel-header-var-3 AS CHARACTER NO-UNDO.


DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS cha       NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS cha       NO-UNDO.
DEFINE VARIABLE cFieldLength       AS cha       NO-UNDO.
DEFINE VARIABLE cFieldType         AS cha       NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE BUFFER b-job-hdr FOR job-hdr .
DEFINE VARIABLE cTextListToDefault AS cha       NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO .
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.


ASSIGN 
    cTextListToSelect  = "DATE,ITEM,DESCRIPTION,P.O.#,TY,Job #,TAG#,REC QTY,WHSE,BIN,WHSETO,BIN TO,COST,VALUE," +
                           "PO QTY,DUE DATE,VENDOR,OVER/UNDER%,FORM,CUST,FG ITEM#,ITEM DESC,Over%,Under%,TONS,QUANTITY,REASON,REASON CODE,REASON DESCRIPTION," +
                           "SHEET SIZE,VENDOR TAG #"
    cFieldListToSelect = "trans-date,i-no,i-name,po-no,rita-code,v-job-no,tag,qty,loc,loc-bin,loc2,loc-bin2,cost,v-value," +
                                "poqty,due,vend,per,form,cust,fgitem,itemdesc,ovrpct,undpct,tons,qty,Reason,Reason-cd,Reason-dscr," +
                                "sheet-size,vend-tag"
    cFieldLength       = "8,10,30,8,2,13,20,10,5,8,6,8,10,10," + "12,8,20,12,4,25,15,30,7,7,12,10,30,11,25," + "25,26"
    cFieldType         = "c,c,c,c,c,c,c,i,c,c,c,c,i,i," + "i,c,c,c,i,c,c,c,i,i,i,i,c,c,c," + "c,c"
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "DATE,ITEM,DESCRIPTION,P.O.#,TY,Job #,CUST,FG ITEM#,ITEM DESC," +
                           "TAG#,QUANTITY,WHSE,BIN,WHSETO,BIN TO,COST,VALUE" .

{sys/inc/oereordr.i}

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
begin_procat end_procat begin_date end_date begin_whs end_whs begin_job-no ~
begin_job-no2 end_job-no end_job-no2 select-mat tb_sort tb_receipts ~
tb_issues tb_subtot tb_transfers tb_adjustments tb_counts tb_issue-detail ~
sl_selected sl_avail Btn_Def Btn_Add Btn_Remove btn_Up btn_down rd-dest ~
fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_rm-no end_rm-no begin_procat ~
end_procat begin_date end_date begin_whs end_whs begin_job-no begin_job-no2 ~
end_job-no end_job-no2 select-mat tb_sort tb_receipts tb_issues tb_subtot ~
tb_transfers tb_adjustments tb_counts tb_issue-detail sl_selected sl_avail ~
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
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(9)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-999":U INITIAL "000"      
     VIEW-AS FILL-IN 
     SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_whs AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(9)":U INITIAL "zzzzzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-999":U INITIAL "999"      
     VIEW-AS FILL-IN 
     SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_whs AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-trh.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 43 BY 1
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

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To Email", 5,
"To CSV", 3
     SIZE 17.2 BY 5.29 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 6.43.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 18.57.

DEFINE VARIABLE select-mat AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 44 BY 5 NO-UNDO.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5.71 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5.71 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_adjustments AS LOGICAL INITIAL yes 
     LABEL "Adjustments?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE tb_counts AS LOGICAL INITIAL yes 
     LABEL "Cycle Counts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE tb_issue-detail AS LOGICAL INITIAL yes 
     LABEL "Print FG Item Detail in Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .95 NO-UNDO.

DEFINE VARIABLE tb_issues AS LOGICAL INITIAL yes 
     LABEL "Issues?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.4 BY .95 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV AS LOGICAL INITIAL no 
     LABEL "Open CSV?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_receipts AS LOGICAL INITIAL yes 
     LABEL "Receipts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .95 NO-UNDO.

DEFINE VARIABLE tb_sort AS LOGICAL INITIAL no 
     LABEL "Sort by PO#/Item#" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_subtot AS LOGICAL INITIAL no 
     LABEL "SubTotal PO#/Item#" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_transfers AS LOGICAL INITIAL yes 
     LABEL "Transfers?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.4 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_rm-no AT ROW 2.19 COL 27.6 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_rm-no AT ROW 2.19 COL 71.8 COLON-ALIGNED HELP
          "Enter Ending Item number"
     begin_procat AT ROW 3.14 COL 27.6 COLON-ALIGNED HELP
          "Enter Begining Category"
     end_procat AT ROW 3.14 COL 71.8 COLON-ALIGNED HELP
          "Enter Ending Category"
     begin_date AT ROW 4.1 COL 27.6 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 4.1 COL 71.8 COLON-ALIGNED HELP
          "Enter ending Date"
     begin_whs AT ROW 5.05 COL 27.6 COLON-ALIGNED HELP
          "Enter Beginng Warehouse"
     end_whs AT ROW 5.05 COL 71.8 COLON-ALIGNED HELP
          "Enter Endng Warehouse"
     begin_job-no AT ROW 6 COL 27.6 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 6 COL 39.6 COLON-ALIGNED NO-LABEL HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 6 COL 71.8 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 6 COL 83.8 COLON-ALIGNED NO-LABEL HELP
          "Enter Ending Job Number"
     mat-types AT ROW 7 COL 17.4 COLON-ALIGNED
     select-mat AT ROW 8.14 COL 6.4 HELP
          "Enter description of this Material Type." NO-LABEL
     tb_sort AT ROW 8.14 COL 69 WIDGET-ID 44
     tb_receipts AT ROW 8.19 COL 52.4
     tb_issues AT ROW 9.14 COL 52.4
     tb_subtot AT ROW 9.14 COL 69 WIDGET-ID 46
     tb_transfers AT ROW 10.1 COL 52.4
     tb_adjustments AT ROW 11.05 COL 52.4
     tb_counts AT ROW 12 COL 52.4
     tb_issue-detail AT ROW 13.1 COL 6.6 WIDGET-ID 2
     sl_selected AT ROW 14 COL 61.6 NO-LABEL WIDGET-ID 28
     sl_avail AT ROW 14.05 COL 6.4 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 14.1 COL 41.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 15.24 COL 41.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 16.38 COL 41.4 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 17.52 COL 41.4 WIDGET-ID 40
     btn_down AT ROW 18.71 COL 41.4 WIDGET-ID 42
     lines-per-page AT ROW 20.76 COL 86.8 COLON-ALIGNED
     rd-dest AT ROW 21 COL 5.8 NO-LABEL
     lv-ornt AT ROW 21 COL 42.8 NO-LABEL
     lv-font-no AT ROW 21.95 COL 83.8 COLON-ALIGNED
     lv-font-name AT ROW 22.43 COL 30.8 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 23.62 COL 47.8
     fi_file AT ROW 25.29 COL 28.8 COLON-ALIGNED HELP
          "Enter File Name"
     tb_OpenCSV AT ROW 25.29 COL 89.8 RIGHT-ALIGNED
     tbAutoClose AT ROW 27.05 COL 31 WIDGET-ID 16
     btn-ok AT ROW 27.95 COL 30.8
     btn-cancel AT ROW 27.95 COL 53.8
     "Select/Deselect Material Types" VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 7.24 COL 7.2
          FONT 6
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.1 COL 6.6
          BGCOLOR 15 
     " Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 20.14 COL 5.8
     RECT-6 AT ROW 20.57 COL 3.8
     RECT-7 AT ROW 1.48 COL 3.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100.4 BY 30.19
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
         TITLE              = "Transaction History"
         HEIGHT             = 28.57
         WIDTH              = 96.6
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = 15
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
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
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

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
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

/* SETTINGS FOR FILL-IN mat-types IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       mat-types:HIDDEN IN FRAME FRAME-A           = TRUE
       mat-types:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".
 
/* SETTINGS FOR FILL-IN begin_job-no2 IN FRAME FRAME-A
   NO-LABEL                                                             */
/* SETTINGS FOR FILL-IN end_job-no2 IN FRAME FRAME-A
   NO-LABEL                                                             */   

ASSIGN 
       tb_adjustments:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_counts:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_issue-detail:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_issue-detail:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_issues:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_receipts:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_subtot:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_transfers:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Transaction History */
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
ON WINDOW-CLOSE OF C-Win /* Transaction History */
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


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
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
                        MESSAGE  "CSV file have been created." SKIP(1)
                            "~"OK~" to open CSV file?"
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


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
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
        IF SELF:SCREEN-VALUE BEGINS "L" THEN 
            ASSIGN lv-font-no     = "12"
                lines-per-page = 55
                lv-font-name   = "Courier New SIZE=8(15CPI)".

        ELSE    ASSIGN lv-font-no     = "11"
                lines-per-page = 99
                lv-font-name   = "Courier New Size=7 (17 cpi for 132 CLMN REPORT)".

        DISPLAY lv-font-no lines-per-page lv-font-name WITH FRAME {&FRAME-NAME}.
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


&Scoped-define SELF-NAME tb_issue-detail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_issue-detail C-Win
ON VALUE-CHANGED OF tb_issue-detail IN FRAME FRAME-A /* Print FG Item Detail in Excel? */
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


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
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


&Scoped-define SELF-NAME tb_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sort C-Win
ON VALUE-CHANGED OF tb_sort IN FRAME FRAME-A /* Sort by PO#/Item# */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_subtot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_subtot C-Win
ON VALUE-CHANGED OF tb_subtot IN FRAME FRAME-A /* SubTotal PO#/Item# */
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
    {sys/inc/reportsConfigNK1.i "MR5" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    FOR EACH mat FIELDS(mat dscr) NO-LOCK:
        v-mat-list = v-mat-list + string(mat.mat,"x(5)") + " " + mat.dscr + ",".
    END.
    IF substr(v-mat-list,LENGTH(TRIM(v-mat-list)),1) EQ "," THEN
        substr(v-mat-list,LENGTH(TRIM(v-mat-list)),1) = "".

    select-mat:list-items = v-mat-list.

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        tb_issue-detail:HIDDEN IN FRAME FRAME-A           = TRUE .
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
  DISPLAY begin_rm-no end_rm-no begin_procat end_procat begin_date end_date 
          begin_whs end_whs begin_job-no begin_job-no2 end_job-no end_job-no2 
          select-mat tb_sort tb_receipts tb_issues tb_subtot tb_transfers 
          tb_adjustments tb_counts tb_issue-detail sl_selected sl_avail rd-dest 
          fi_file tb_OpenCSV tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_rm-no end_rm-no begin_procat end_procat begin_date 
         end_date begin_whs end_whs begin_job-no begin_job-no2 end_job-no 
         end_job-no2 select-mat tb_sort tb_receipts tb_issues tb_subtot 
         tb_transfers tb_adjustments tb_counts tb_issue-detail sl_selected 
         sl_avail Btn_Def Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file 
         tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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
            fi_file:SCREEN-VALUE = "c:\tmp\r-trh.csv".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*{sys/form/r-topw.f}*/

    DEFINE VARIABLE v-fitem      LIKE rm-rcpth.i-no NO-UNDO.
    DEFINE VARIABLE v-titem      LIKE v-fitem INIT "zzzzzzzzzz" NO-UNDO.
    DEFINE VARIABLE v-fpcat      LIKE item.procat NO-UNDO.
    DEFINE VARIABLE v-tpcat      LIKE v-fpcat INIT "zzzzz" NO-UNDO.
    DEFINE VARIABLE v-fdate      AS DATE      FORMAT "99/99/9999" INIT 01/01/0001 NO-UNDO.
    DEFINE VARIABLE v-tdate      LIKE v-fdate INIT TODAY NO-UNDO.
    DEFINE VARIABLE v-floc       LIKE rm-rcpth.loc NO-UNDO.
    DEFINE VARIABLE v-tloc       LIKE v-floc INITIAL "zzzzz" NO-UNDO.
    DEFINE VARIABLE v-fjob       LIKE job.job-no NO-UNDO.
    DEFINE VARIABLE v-tjob       LIKE v-fjob INIT "zzzzzzzzz" NO-UNDO.
    DEFINE VARIABLE v-fjob2      LIKE job.job-no2 FORMAT "999" NO-UNDO.
    DEFINE VARIABLE v-tjob2      LIKE v-fjob2 INIT 999 NO-UNDO.
    DEFINE VARIABLE v-mtype      AS CHARACTER FORMAT "x(47)" NO-UNDO.
    DEFINE VARIABLE v-code       LIKE rm-rcpth.rita-code NO-UNDO.

    DEFINE VARIABLE v-value      AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-job-no     AS CHARACTER FORMAT "x(13)" NO-UNDO.
    DEFINE VARIABLE v-qty        LIKE rm-rdtlh.qty EXTENT 3 NO-UNDO.
    DEFINE VARIABLE v-t-ton      AS DECIMAL   EXTENT 3 NO-UNDO.
    DEFINE VARIABLE v-val        LIKE v-value EXTENT 3 NO-UNDO.
    DEFINE VARIABLE v-cost       LIKE v-value EXTENT 3 NO-UNDO.
    DEFINE VARIABLE v-poqty      LIKE v-value EXTENT 3 NO-UNDO.
    DEFINE VARIABLE v-first      AS LOG       EXTENT 3 NO-UNDO.
    DEFINE VARIABLE v-fjob1      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-tjob1      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-type       AS CHARACTER FORMAT "x(5)" NO-UNDO.
    DEFINE VARIABLE v-job1sub    AS INTEGER   NO-UNDO.

    DEFINE VARIABLE v-ton        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-wid        LIKE po-ordl.s-wid NO-UNDO.
    DEFINE VARIABLE v-len        LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-dep        LIKE item.s-dep NO-UNDO.
    DEFINE VARIABLE v-bwt        LIKE item.basis-w NO-UNDO.

    DEFINE VARIABLE cCust        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cINo         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIName       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVender      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ld-poqty     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-porqty    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-rqty      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-under-per AS DECIMAL   NO-UNDO .

    DEFINE BUFFER bf-itemfg  FOR itemfg.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-cust    FOR cust.

    DEFINE VARIABLE str-tit4       AS cha    FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5       AS cha    FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line       AS cha    FORM "x(300)" NO-UNDO.
    DEFINE VARIABLE cDisplay       AS cha    NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS cha    NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHA    NO-UNDO.
    DEFINE VARIABLE cVarValue      AS cha    NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS cha    NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS cha    NO-UNDO.
    DEFINE VARIABLE cFieldName     AS cha    NO-UNDO.
    {sys/form/r-top5DL.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReason     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShtSize    AS CHARACTER FORMAT "x(30)" NO-UNDO.
    DEFINE VARIABLE dShtWid     AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dShtLen     AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE cVendorTag  AS CHARACTER NO-UNDO .
 

    {custom/statusMsg.i "'Processing...'"} 

    /*form rm-rcpth.trans-date format "99/99/99" label "DATE"
         rm-rcpth.i-no label "ITEM"
         rm-rcpth.i-name format "x(14)" label "DESCRIPTION"
         rm-rcpth.po-no label "P.O.#"
         rm-rcpth.rita-code label "TY"
         v-job-no label "   Job #"
         rm-rdtlh.tag label "TAG#" FORMAT 'X(20)'
         rm-rdtlh.qty format "->>>>>9.99<<" label "QUANTITY"
         rm-rdtlh.loc label "WHSE"
         rm-rdtlh.loc-bin label "BIN"
         rm-rdtlh.loc2 column-label "WHSE! TO"
         rm-rdtlh.loc-bin2 column-label "BIN! TO"
         rm-rdtlh.cost format "->>>>>9.99<<<<" label "COST"
         space(0)
         v-value label "VALUE"
         skip
        with frame itemx no-box down stream-io width 150.
    
    form rm-rcpth.trans-date format "99/99/99" label "DATE"
         rm-rcpth.i-no label "ITEM"
         rm-rcpth.i-name format "x(14)" label "DESCRIPTION"
         rm-rcpth.po-no label "P.O.#"
         rm-rcpth.rita-code label "TY"
         v-job-no label "   Job #"
         rm-rdtlh.tag label "TAG#" FORMAT 'X(20)'
         rm-rdtlh.qty format "->>>>>9.99<<" label "QUANTITY"
         rm-rdtlh.loc label "WHSE"
         rm-rdtlh.loc-bin label "BIN"
         rm-rdtlh.loc2 column-label "WHSE! TO"
         rm-rdtlh.loc-bin2 column-label "BIN! TO"
         rm-rdtlh.cost format "->>>>>9.99<<<<" label "COST"
         space(0)
         v-value label "VALUE"
         skip
        with frame itemy no-box down stream-io width 150.*/


    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 150}

        v-fitem  = begin_rm-no
        v-titem  = end_rm-no
        v-fpcat  = begin_procat
        v-tpcat  = end_procat
        v-fdate  = begin_date
        v-tdate  = end_date
        v-floc   = begin_whs
        v-tloc   = end_whs
        v-type   = (IF tb_receipts    THEN "R" ELSE "") +
           (IF tb_issues      THEN "I" ELSE "") +
           (IF tb_transfers   THEN "T" ELSE "") +
           (IF tb_adjustments THEN "A" ELSE "") +
           (IF tb_counts      THEN "C" ELSE "")
        v-fjob   = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', begin_job-no, begin_job-no2)) 
        v-tjob   = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', end_job-no, end_job-no2)) .

    DO WITH FRAME {&frame-name}:          
        DO i = 1 TO select-mat:num-items:
            IF select-mat:is-selected(i) THEN
                v-mtype = v-mtype + trim(substr(select-mat:entry(i),1,5)) + ",".
        END.

        IF LENGTH(TRIM(v-mtype)) NE 0 AND
            substr(v-mtype,LENGTH(TRIM(v-mtype)),1) EQ "," THEN
            substr(v-mtype,LENGTH(TRIM(v-mtype)),1) = "".

        mat-types = v-mtype.

        DO i = 1 TO LENGTH(mat-types):
            IF substr(mat-types,i,1) EQ "," THEN substr(mat-types,i,1) = " ".
        END.

        DISPLAY mat-types.

        mat-types:HIDDEN = YES.
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

        IF LOOKUP(ttRptSelected.TextList, "REC QTY,COST,VALUE,PO QTY,TONS,QUANTITY") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " .
    END.

    IF rd-dest EQ 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.


    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    EMPTY TEMP-TABLE tt-report.

    /*IF rd-dest EQ 3 THEN DO:
       OUTPUT STREAM excel TO VALUE(cFileName).
       IF tb_issue-detail THEN 
         EXPORT STREAM excel DELIMITER "," 
              "DATE"
              "ITEM"
              "DESCRIPTION"
              "P.O.#"
              "TY"
              "JOB #"
              "CUSTOMER"
              "FG ITEM #"
              "FG ITEM DESCRIPTION"
              "TAG#"
              "QUANTITY"
              "WHSE"
              "BIN"
              "WHSE TO"
              "BIN TO"
              "COST"
              "VALUE"
              SKIP.
       ELSE
           EXPORT STREAM excel DELIMITER "," 
              "DATE"
              "ITEM"
              "DESCRIPTION"
              "P.O.#"
              "TY"
              "JOB #"
              "TAG#"
              "QUANTITY"
              "WHSE"
              "BIN"
              "WHSE TO"
              "BIN TO"
              "COST"
              "VALUE"
              SKIP.
 
    END.*/

    DISPLAY "" WITH FRAME r-top.

    ASSIGN
        v-type    = CAPS(v-type)
        v-fjob1   = substr(v-fjob,1,iJobLen)
        v-tjob1   = substr(v-tjob,1,iJobLen)
        v-job1sub = int(end_job-no2).

    IF tb_sort OR tb_subtot THEN 
    DO:
        FOR EACH rm-rcpth
            WHERE rm-rcpth.company    EQ cocode
            AND rm-rcpth.i-no       GE v-fitem
            AND rm-rcpth.i-no       LE v-titem
            AND rm-rcpth.trans-date GE v-fdate
            AND rm-rcpth.trans-date LE v-tdate
            AND index(v-type,rm-rcpth.rita-code) GT 0
            NO-LOCK,
            EACH rm-rdtlh
            WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
            AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
            AND rm-rdtlh.loc       GE v-floc
            AND rm-rdtlh.loc       LE v-tloc
            AND rm-rdtlh.job-no    GE v-fjob1 
            AND rm-rdtlh.job-no    LE v-tjob1
            AND FILL(" ", iJobLen - length(TRIM(rm-rdtlh.job-no))) +
            trim(rm-rdtlh.job-no) + string(rm-rdtlh.job-no2,"999")
            GE v-fjob
            AND FILL(" ", iJobLen - length(TRIM(rm-rdtlh.job-no))) +
            trim(rm-rdtlh.job-no) + string(rm-rdtlh.job-no2,"999")
            LE v-tjob
            NO-LOCK,
            FIRST item
            WHERE item.company EQ cocode
            AND item.i-no    EQ rm-rcpth.i-no
            AND item.procat  GE v-fpcat
            AND item.procat  LE v-tpcat
            AND index(v-mtype,item.mat-type) GT 0
            NO-LOCK

            BREAK BY rm-rcpth.i-no
            BY rm-rcpth.po-no:

            {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

            {rmrep\r-trans2.i}
        END.

    END.
    ELSE 
    DO:

        IF begin_job-no EQ end_job-no AND
            begin_job-no2 EQ end_job-no2 THEN
            FOR EACH rm-rdtlh WHERE
                rm-rdtlh.company EQ cocode AND
                rm-rdtlh.job-no  EQ v-fjob1 AND 
                rm-rdtlh.job-no2 EQ v-job1sub AND
          index(v-type,rm-rdtlh.rita-code) GT 0 AND
                rm-rdtlh.loc       GE v-floc AND
                rm-rdtlh.loc       LE v-tloc
                NO-LOCK,
                EACH rm-rcpth WHERE
                rm-rcpth.r-no EQ rm-rdtlh.r-no AND
                rm-rcpth.i-no GE v-fitem AND
                rm-rcpth.i-no LE v-titem AND
                rm-rcpth.trans-date GE v-fdate AND
                rm-rcpth.trans-date LE v-tdate
                NO-LOCK,
                FIRST ITEM WHERE
                item.company EQ cocode AND
                item.i-no    EQ rm-rcpth.i-no AND
                item.procat  GE v-fpcat AND
                item.procat  LE v-tpcat AND
                index(v-mtype,item.mat-type) GT 0
                NO-LOCK

                BREAK BY rm-rcpth.trans-date
                BY rm-rcpth.rita-code
                BY rm-rcpth.i-no
                BY rm-rcpth.po-no:

                {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

                {rmrep\r-transN.i}
            END.
        ELSE IF begin_job-no EQ end_job-no THEN
                FOR EACH rm-rdtlh WHERE
                    rm-rdtlh.company EQ cocode AND
                    rm-rdtlh.job-no  EQ v-fjob1 AND 
                    FILL(" ", iJobLen - length(TRIM(rm-rdtlh.job-no))) +
                    trim(rm-rdtlh.job-no) + string(rm-rdtlh.job-no2,"999")
                    GE v-fjob AND
                    FILL(" ", iJobLen - length(TRIM(rm-rdtlh.job-no))) +
                    trim(rm-rdtlh.job-no) + string(rm-rdtlh.job-no2,"999")
                    LE v-tjob AND
          index(v-type,rm-rdtlh.rita-code) GT 0 AND
                    rm-rdtlh.loc       GE v-floc AND
                    rm-rdtlh.loc       LE v-tloc
                    NO-LOCK,
                    EACH rm-rcpth WHERE
                    rm-rcpth.r-no EQ rm-rdtlh.r-no AND
                    rm-rcpth.i-no GE v-fitem AND
                    rm-rcpth.i-no LE v-titem AND
                    rm-rcpth.trans-date GE v-fdate AND
                    rm-rcpth.trans-date LE v-tdate
                    NO-LOCK,
                    FIRST ITEM WHERE
                    item.company EQ cocode AND
                    item.i-no    EQ rm-rcpth.i-no AND
                    item.procat  GE v-fpcat AND
                    item.procat  LE v-tpcat AND
                index(v-mtype,item.mat-type) GT 0
                    NO-LOCK

                    BREAK BY rm-rcpth.trans-date
                    BY rm-rcpth.rita-code
                    BY rm-rcpth.i-no
                    BY rm-rcpth.po-no:

                    {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

                    {rmrep\r-transN.i}
                END.
            ELSE IF begin_rm-no EQ end_rm-no THEN
                    FOR EACH rm-rcpth
                        WHERE rm-rcpth.company    EQ cocode
                        AND rm-rcpth.i-no       EQ v-fitem
                        AND rm-rcpth.trans-date GE v-fdate
                        AND rm-rcpth.trans-date LE v-tdate
                        AND index(v-type,rm-rcpth.rita-code) GT 0
                        NO-LOCK,
                        EACH rm-rdtlh
                        WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
                        AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
                        AND rm-rdtlh.loc       GE v-floc
                        AND rm-rdtlh.loc       LE v-tloc
                        AND rm-rdtlh.job-no    GE v-fjob1 
                        AND rm-rdtlh.job-no    LE v-tjob1
                        AND FILL(" ", iJobLen - length(TRIM(rm-rdtlh.job-no))) +
                        trim(rm-rdtlh.job-no) + string(rm-rdtlh.job-no2,"999")
                        GE v-fjob
                        AND FILL(" ", iJobLen - length(TRIM(rm-rdtlh.job-no))) +
                        trim(rm-rdtlh.job-no) + string(rm-rdtlh.job-no2,"999")
                        LE v-tjob
                        NO-LOCK,
                        FIRST item
                        WHERE item.company EQ cocode
                        AND item.i-no    EQ rm-rcpth.i-no
                        AND item.procat  GE v-fpcat
                        AND item.procat  LE v-tpcat
                        AND index(v-mtype,item.mat-type) GT 0
                        NO-LOCK

                        BREAK BY rm-rcpth.trans-date
                        BY rm-rcpth.rita-code
                        BY rm-rcpth.i-no
                        BY rm-rcpth.po-no:

                        {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

                        {rmrep\r-transN.i}
                    END.
                ELSE
                    FOR EACH rm-rcpth
                        WHERE rm-rcpth.company    EQ cocode
                        AND rm-rcpth.i-no       GE v-fitem
                        AND rm-rcpth.i-no       LE v-titem
                        AND rm-rcpth.trans-date GE v-fdate
                        AND rm-rcpth.trans-date LE v-tdate
                        AND index(v-type,rm-rcpth.rita-code) GT 0
                        NO-LOCK,
                        EACH rm-rdtlh
                        WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
                        AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
                        AND rm-rdtlh.loc       GE v-floc
                        AND rm-rdtlh.loc       LE v-tloc
                        AND rm-rdtlh.job-no    GE v-fjob1 
                        AND rm-rdtlh.job-no    LE v-tjob1
                        AND FILL(" ", iJobLen - length(TRIM(rm-rdtlh.job-no))) +
                        trim(rm-rdtlh.job-no) + string(rm-rdtlh.job-no2,"999")
                        GE v-fjob
                        AND FILL(" ", iJobLen - length(TRIM(rm-rdtlh.job-no))) +
                        trim(rm-rdtlh.job-no) + string(rm-rdtlh.job-no2,"999")
                        LE v-tjob
                        NO-LOCK,
                        FIRST item
                        WHERE item.company EQ cocode
                        AND item.i-no    EQ rm-rcpth.i-no
                        AND item.procat  GE v-fpcat
                        AND item.procat  LE v-tpcat
                        AND index(v-mtype,item.mat-type) GT 0
                        NO-LOCK

                        BREAK BY rm-rcpth.trans-date
                        BY rm-rcpth.rita-code
                        BY rm-rcpth.i-no
                        BY rm-rcpth.po-no:

                        {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

                        {rmrep\r-transN.i}
                    END.
    END.


    v-value = 0.

    FOR EACH tt-report WHERE tt-report.term-id EQ "",
        FIRST account
        WHERE account.company EQ cocode
        AND account.actnum  EQ tt-report.key-01
        NO-LOCK
        BREAK BY tt-report.key-01
        TRANSACTION:

        IF FIRST(tt-report.key-01) THEN PAGE.

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
        OUTPUT STREAM excel CLOSE.
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

