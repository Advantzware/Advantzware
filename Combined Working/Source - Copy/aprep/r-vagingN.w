&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&SCOPED-DEFINE WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: aprep\r-vaging.w

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
DEFINE VARIABLE list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i NEW SHARED}

ASSIGN 
 cocode = gcompany
 locode = gloc.



DEFINE VARIABLE lv-default-comp AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-count AS INTEGER NO-UNDO INITIAL 0.

FOR EACH usercomp NO-LOCK  WHERE usercomp.USER_id EQ USERID("nosweat") AND  usercomp.loc = "" :
    v-count = v-count + 1 .
END.
FIND FIRST usercomp NO-LOCK WHERE usercomp.USER_id EQ USERID("nosweat") AND
                                  usercomp.company_default NO-ERROR.
ASSIGN     
lv-default-comp = IF AVAILABLE usercomp THEN usercomp.company ELSE "001".



FIND FIRST company NO-LOCK WHERE company.company EQ cocode NO-ERROR.

DEFINE NEW SHARED VARIABLE v-s-vend LIKE vend.vend-no.
DEFINE NEW SHARED VARIABLE v-e-vend LIKE vend.vend-no.
DEFINE NEW SHARED VARIABLE v-idate AS LOGICAL FORMAT "Invoice/Posting" INITIAL yes.
DEFINE NEW SHARED VARIABLE v-sort AS LOGICAL FORMAT "Code/Name"       INITIAL yes.
DEFINE NEW SHARED VARIABLE v-dtl AS LOGICAL FORMAT "Detail/Summary"  INITIAL yes.
DEFINE NEW SHARED VARIABLE v-s-type AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-e-type AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-print-fmt AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-days AS INTEGER NO-UNDO EXTENT 4.
DEFINE VARIABLE v-refnum AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-check-date AS DATE NO-UNDO.
DEFINE VARIABLE v-terms LIKE terms.dscr NO-UNDO.

DEF TEMP-TABLE tt-vend NO-UNDO
    FIELD curr-code LIKE vend.curr-code
    FIELD sorter    LIKE vend.vend-no
    FIELD row-id    AS   ROWID
    INDEX tt-vend curr-code sorter.

DEF STREAM excel.

DEF TEMP-TABLE w-sort NO-UNDO FIELD w-int AS INTEGER.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE cTextListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength AS INTEGER NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.


ASSIGN cTextListToSelect = "CURRENCY,VENDOR#,VENDOR NAME,PHONE,TYPE,TERMS,INVOICE#,DATE,DUE DATE,AMOUNT,#DAYS" 

       cFieldListToSelect = "curr,vend,vend-name,phone,type,term,inv,date,due-date,amt,day" 

       cFieldLength = "8,10,30,15,6,17,12,8,8,17,6" 
       cFieldType = "c,c,c,c,c,c,c,c,c,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "CURRENCY,VENDOR#,VENDOR NAME,PHONE,TYPE,TERMS,INVOICE#,DATE,AMOUNT,#DAYS"  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&SCOPED-DEFINE PROCEDURE-TYPE WINDOW
&SCOPED-DEFINE DB-AWARE NO

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&SCOPED-DEFINE FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&SCOPED-DEFINE ENABLED-OBJECTS RECT-6 RECT-7 begin_comp end_comp begin_vend ~
end_vend begin_curr end_curr begin_type end_type tb_elect tb_non-elect ~
as_of_date rd_date period-days-1 period-days-2 period-days-3 period-days-4 ~
rd_sort tb_detailed sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up ~
btn_down lv-ornt lines-per-page rd-dest lv-font-no td-show-parm tb_excel ~
tb_runExcel fi_file btn-ok btn-cancel 
&SCOPED-DEFINE DISPLAYED-OBJECTS begin_comp end_comp begin_vend end_vend ~
begin_curr end_curr begin_type end_type tb_elect tb_non-elect as_of_date ~
lbl_date rd_date period-days-1 period-days-2 period-days-3 period-days-4 ~
lbl_sort rd_sort tb_detailed sl_avail sl_selected lv-ornt lines-per-page ~
rd-dest lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

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
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

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

DEFINE VARIABLE as_of_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "As of" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_comp AS CHARACTER FORMAT "X(3)":U 
     LABEL "Beginning Company#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_curr AS CHARACTER FORMAT "X(3)":U 
     LABEL "Beginning Currency" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_type AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor Type" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_comp AS CHARACTER FORMAT "X(3)":U INITIAL "zzz" 
     LABEL "Ending Company#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_curr AS CHARACTER FORMAT "X(3)":U INITIAL "zzz" 
     LABEL "Ending Currency" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_type AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor Type" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-vaging.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_date AS CHARACTER FORMAT "X(256)":U INITIAL "Which Date?" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(10)":U INITIAL "Sort By?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE period-days-1 AS INTEGER FORMAT ">,>>9":U INITIAL 9999 
     LABEL "Period Days 1" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE period-days-2 AS INTEGER FORMAT ">,>>9":U INITIAL 9999 
     LABEL "2" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE period-days-3 AS INTEGER FORMAT ">,>>9":U INITIAL 9999 
     LABEL "3" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE period-days-4 AS INTEGER FORMAT ">,>>9":U INITIAL 9999 
     LABEL "4" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

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
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_date AS CHARACTER INITIAL "Invoice" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Invoice", "Invoice",
"Posting", "Posting"
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Code" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Vendor Code", "Code",
"Vendor Name", "Name"
     SIZE 37 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 8.48.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.95.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_non-elect AS LOGICAL INITIAL no 
     LABEL "Non-Electronic" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE tb_elect AS LOGICAL INITIAL NO  
     LABEL "Electronic" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .95 NO-UNDO.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL NO  
     LABEL "Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL YES    
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL NO  
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO  
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_comp AT ROW 2.52 COLUMN 23 COLON-ALIGNED
     end_comp AT ROW 2.52 COLUMN 63 COLON-ALIGNED
     begin_vend AT ROW 3.48 COLUMN 23 COLON-ALIGNED HELP
          "Enter Beginning Vendor"
     end_vend AT ROW 3.48 COLUMN 63 COLON-ALIGNED HELP
          "Enter Ending Vendor"
     begin_curr AT ROW 4.43 COLUMN 23 COLON-ALIGNED HELP
          "Enter Beginning Currency Code"
     end_curr AT ROW 4.43 COLUMN 63 COLON-ALIGNED HELP
          "Enter Ending Currency Code"
     begin_type AT ROW 5.38 COLUMN 23 COLON-ALIGNED HELP
          "Enter Beginning Currency Code" WIDGET-ID 10
     end_type AT ROW 5.38 COLUMN 63 COLON-ALIGNED HELP
          "Enter Beginning Currency Code" WIDGET-ID 12
     tb_elect AT ROW 6.71 COLUMN 25 WIDGET-ID 14
     tb_non-elect AT ROW 6.71 COLUMN 52.4 WIDGET-ID 16
     as_of_date AT ROW 7.91 COLUMN 23 COLON-ALIGNED HELP
          "Enter As od date"
     lbl_date AT ROW 7.91 COLUMN 47 COLON-ALIGNED NO-LABEL
     rd_date AT ROW 7.91 COLUMN 64 NO-LABEL
     period-days-1 AT ROW 9.24 COLUMN 23 COLON-ALIGNED WIDGET-ID 2
     period-days-2 AT ROW 9.24 COLUMN 35 COLON-ALIGNED WIDGET-ID 4
     period-days-3 AT ROW 9.24 COLUMN 48 COLON-ALIGNED WIDGET-ID 6
     period-days-4 AT ROW 9.24 COLUMN 61 COLON-ALIGNED WIDGET-ID 8
     lbl_sort AT ROW 10.81 COLUMN 23 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 10.81 COLUMN 35 NO-LABEL
     tb_detailed AT ROW 10.86 COLUMN 74.8
     sl_avail AT ROW 12.81 COLUMN 4.8 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 12.81 COLUMN 40.8 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 12.81 COLUMN 60.2 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 13.81 COLUMN 40.8 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 14.81 COLUMN 40.8 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 15.86 COLUMN 40.8 WIDGET-ID 40
     btn_down AT ROW 16.86 COLUMN 40.8 WIDGET-ID 42
     lv-ornt AT ROW 19.38 COLUMN 31 NO-LABEL
     lines-per-page AT ROW 19.38 COLUMN 84 COLON-ALIGNED
     rd-dest AT ROW 19.43 COLUMN 6 NO-LABEL
     lv-font-no AT ROW 21.29 COLUMN 37 COLON-ALIGNED
     lv-font-name AT ROW 22.52 COLUMN 31 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 23.71 COLUMN 33
     tb_excel AT ROW 25.05 COLUMN 53 RIGHT-ALIGNED
     tb_runExcel AT ROW 25.05 COLUMN 74 RIGHT-ALIGNED
     fi_file AT ROW 25.86 COLUMN 31 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 27.57 COLUMN 19
     btn-cancel AT ROW 27.57 COLUMN 58
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 12.1 COLUMN 60.2 WIDGET-ID 44
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COLUMN 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 18.43 COLUMN 4
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 12.1 COLUMN 5.6 WIDGET-ID 38
     RECT-6 AT ROW 18.86 COLUMN 2
     RECT-7 AT ROW 1 COLUMN 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COLUMN 1.6 ROW 1.24
         SIZE 95 BY 28.1.


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
         TITLE              = "Vendor Aging"
         HEIGHT             = 28.33
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       as_of_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_comp:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_curr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_comp:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_curr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_date IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_date".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       period-days-1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       period-days-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       period-days-3:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       period-days-4:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_non-elect:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_elect:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&SCOPED-DEFINE SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Vendor Aging */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vendor Aging */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME as_of_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL as_of_date C-Win
ON LEAVE OF as_of_date IN FRAME FRAME-A /* As of */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_curr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_curr C-Win
ON LEAVE OF begin_curr IN FRAME FRAME-A /* Beginning Currency */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_type C-Win
ON LEAVE OF begin_type IN FRAME FRAME-A /* Beginning Vendor Type */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor# */
DO:  
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
  RUN GetSelectionList.
  RUN run-report. 
  STATUS DEFAULT "Processing Complete".
  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &TYPE= ''
                            &begin_cust= "begin_vend"
                            &END_cust= "begin_vend" 
                            &fax-subject=c-win:TITLE 
                            &fax-body=c-win:TITLE 
                            &fax-file=list-name }
       END. 
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Vendor"
                             &begin_cust=begin_vend
                             &END_cust=end_vend
                             &mail-subject=c-win:TITLE 
                             &mail-body=c-win:TITLE 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust=begin_vend
                                  &END_cust=end_vend
                                  &mail-subject=c-win:TITLE 
                                  &mail-body=c-win:TITLE 
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  END CASE.
  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME Btn_Add
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


&SCOPED-DEFINE SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def C-Win
ON CHOOSE OF Btn_Def IN FRAME FRAME-A /* Default */
DO:
  DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

  RUN DisplaySelectionDefault.  /* task 04041406 */ 
  RUN DisplaySelectionList2 .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down C-Win
ON CHOOSE OF btn_down IN FRAME FRAME-A /* Move Down */
DO:
  RUN Move-Field ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME Btn_Remove
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


&SCOPED-DEFINE SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
DO:
  RUN Move-Field ("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_curr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_curr C-Win
ON LEAVE OF end_curr IN FRAME FRAME-A /* Ending Currency */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_type C-Win
ON LEAVE OF end_type IN FRAME FRAME-A /* Ending Vendor Type */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val NE "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
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


&SCOPED-DEFINE SELF-NAME lv-ornt
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


&SCOPED-DEFINE SELF-NAME period-days-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period-days-1 C-Win
ON LEAVE OF period-days-1 IN FRAME FRAME-A /* Period Days 1 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME period-days-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period-days-2 C-Win
ON LEAVE OF period-days-2 IN FRAME FRAME-A /* 2 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME period-days-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period-days-3 C-Win
ON LEAVE OF period-days-3 IN FRAME FRAME-A /* 3 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME period-days-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period-days-4 C-Win
ON LEAVE OF period-days-4 IN FRAME FRAME-A /* 4 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME sl_avail
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


&SCOPED-DEFINE SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected C-Win
ON DEFAULT-ACTION OF sl_selected IN FRAME FRAME-A
DO:
   DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
    IF {&SELF-NAME}:IS-SELECTED(i) THEN DO:
       ASSIGN ldummy = sl_Avail:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
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


&SCOPED-DEFINE SELF-NAME tb_non-elect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_non-elect C-Win
ON VALUE-CHANGED OF tb_non-elect IN FRAME FRAME-A /* Exclude ACH Vendors */
DO:
  ASSIGN {&self-name}.
  IF NOT tb_elect AND NOT tb_non-elect THEN 
      ASSIGN 
        tb_elect = YES
        tb_elect:SCREEN-VALUE = "Yes".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME tb_elect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_elect C-Win
ON VALUE-CHANGED OF tb_elect IN FRAME FRAME-A /* ACH Vendors Only */
DO:
  ASSIGN {&self-name}.
  IF NOT tb_elect AND NOT tb_non-elect THEN 
      ASSIGN 
        tb_non-elect = YES
        tb_non-elect:SCREEN-VALUE = "Yes".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME td-show-parm
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
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  ASSIGN period-days-1 = 30
         period-days-2 = 60
         period-days-3 = 90
         period-days-4 = 120
            .
  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
     RUN DisplaySelectionList2.
    as_of_date:SCREEN-VALUE = STRING(TODAY).
    APPLY "entry" TO begin_vend.
    IF v-count LE 1 THEN
        ASSIGN
          begin_comp:SENSITIVE = NO
          end_comp:SENSITIVE   = NO 
          begin_comp:SCREEN-VALUE = lv-default-comp
          begin_comp = lv-default-comp
          end_comp:SCREEN-VALUE = lv-default-comp
          end_comp = lv-default-comp.

  END.  

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
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

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
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) NE NUM-ENTRIES(cFieldListToSelect) THEN DO:

     RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    /* (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect) + "," +
                     ENTRY(1,cFieldListToSelect)
                     paris */

                    (IF cListContents EQ "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
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
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
  DEFINE VARIABLE cTmpList AS CHARACTER NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) NE NUM-ENTRIES(cFieldListToSelect) THEN DO:
    RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    /* (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect) + "," +
                     ENTRY(1,cFieldListToSelect)
                     paris */

                    (IF cListContents EQ "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .
  END.

 /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */

  sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
  END.

  cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

   DO iCount = 1 TO sl_selected:NUM-ITEMS:
       IF LOOKUP(ENTRY(iCount,cTmpList), cTextListToSelect) EQ 0 THEN
        ldummy = sl_selected:DELETE(ENTRY(iCount,cTmpList)).
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
  DISPLAY begin_comp end_comp begin_vend end_vend begin_curr end_curr begin_type 
          end_type tb_elect tb_non-elect as_of_date lbl_date rd_date 
          period-days-1 period-days-2 period-days-3 period-days-4 lbl_sort 
          rd_sort tb_detailed sl_avail sl_selected lv-ornt lines-per-page 
          rd-dest lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel 
          fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_comp end_comp begin_vend end_vend begin_curr 
         end_curr begin_type end_type tb_elect tb_non-elect as_of_date 
         rd_date period-days-1 period-days-2 period-days-3 period-days-4 
         rd_sort tb_detailed sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
         btn_Up btn_down lv-ornt lines-per-page rd-dest lv-font-no td-show-parm 
         tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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
    FIND FIRST ttRptList NO-LOCK WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-ERROR.     

    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
           ttRptSelected.DisplayOrder = i
           ttRptSelected.HeadingFromLeft = IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
           iColumnLength = iColumnLength + ttRptSelected.FieldLength + 1.
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
        ldummy = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
        ldummy = sl_selected:DELETE(i)
        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
        .
      ELSE
      IF move = "Up" AND i NE 1 THEN
      ASSIGN
        ldummy = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i - 1)
        ldummy = sl_selected:DELETE(i + 1)
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
RUN custom/prntproc.p (list-name,INTEGER(lv-font-no),lv-ornt).   
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
  run scr-rpt.w (list-name,c-win:TITLE,INTEGER(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- ap/ap-aged.p  4/94 RM */
/* Vendor Aging Report Program - A/P Module                                   */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-top3w.f}*/

DEFINE VARIABLE v-date LIKE ap-inv.inv-date NO-UNDO.
DEFINE VARIABLE d AS INTEGER LABEL "Days" NO-UNDO.
DEFINE VARIABLE ni AS INTEGER NO-UNDO.
DEFINE VARIABLE cust-t AS DECIMAL FORMAT "->>>,>>>,>>9.99" EXTENT 5 NO-UNDO.
DEFINE VARIABLE curr-t AS DECIMAL FORMAT "$->>>,>>>,>>9.99" EXTENT 6 NO-UNDO.
DEFINE VARIABLE grand-t AS DECIMAL FORMAT "$->>>,>>>,>>9.99" EXTENT 6 NO-UNDO.
DEFINE VARIABLE s AS INTEGER NO-UNDO.
DEFINE VARIABLE v-amt AS DECIMAL FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE ag LIKE v-amt FORMAT "->>>,>>>,>>9.99" EXTENT 5 NO-UNDO.
DEFINE VARIABLE t1 AS DECIMAL FORMAT "$->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE t2 AS DECIMAL FORMAT "$->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE t3 AS DECIMAL FORMAT "$->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE m1 AS CHARACTER FORMAT "x(20)" NO-UNDO.
DEFINE VARIABLE m2 AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE m3 AS CHARACTER FORMAT "(999) 999-9999" NO-UNDO .
DEFINE VARIABLE first-time AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE time_stamp AS CHARACTER NO-UNDO.
DEFINE VARIABLE op AS CHARACTER FORMAT "x" INITIAL "D" NO-UNDO.
DEFINE VARIABLE ll-mult-curr AS LOGICAL NO-UNDO.
DEFINE VARIABLE lv-page-break AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-f-bot-hdr AS CHARACTER FORMAT "x(12)" NO-UNDO.

DEFINE VARIABLE cDisplay AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExcelDisplay AS CHARACTER NO-UNDO.
DEFINE VARIABLE hField AS HANDLE NO-UNDO.
DEFINE VARIABLE cTmpField AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVarValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
DEFINE VARIABLE str-tit4 AS CHARACTER FORM "x(200)" NO-UNDO.
DEFINE VARIABLE str-tit5 AS CHARACTER FORM "x(200)" NO-UNDO.
DEFINE VARIABLE str-line AS CHARACTER FORM "x(300)" NO-UNDO.

{sys/form/r-top5L3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPaymentList AS CHARACTER NO-UNDO.
DEFINE BUFFER xap-ledger FOR ap-ledger.

FORM HEADER SKIP(1)
     lv-page-break FORMAT "x(200)"
WITH PAGE-TOP FRAME r-top-1 STREAM-IO WIDTH 200 NO-BOX.

FORM HEADER SKIP(1)
     "VENDOR#  NAME  TERMS" SKIP
     "PHONE-   TYPE" skip
     "  INVOICE#      DATE                  AMOUNT #DAYS             0-" + STRING(period-days-1) + FILL(" ",11) +   
                                                                           STRING(period-days-1 + 1) + "-" + STRING(period-days-2) + FILL(" ",11) +
                                                                           STRING(period-days-2 + 1) + "-" + STRING(period-days-3) + FILL(" ",11) +
                                                                           STRING(period-days-3 + 1) + "-" + STRING(period-days-4) + FILL(" ",9) +
                                                                           STRING(period-days-4 + 1) + "+"  + FILL(" ",10) FORMAT "x(131)"
     SKIP FILL("_",131) FORMAT "x(131)"
WITH PAGE-TOP FRAME r-top-2 STREAM-IO WIDTH 200 NO-BOX.

FORM HEADER
     SPACE(10)
     lv-f-bot-hdr
     "          0 -"  STRING(period-days-1) + FILL(" ",10) +   
                      STRING(period-days-1 + 1) + " - " + STRING(period-days-2) + FILL(" ",10) +
                      STRING(period-days-2 + 1) + " - " + STRING(period-days-3) + FILL(" ",10) +
                      STRING(period-days-3 + 1) + " - " + STRING(period-days-4) + FILL(" ",10) +
                      STRING(period-days-4 + 1) + "+"  + FILL(" ",10) +
     "Total Payables" FORM "x(131)"
     SKIP SPACE(10) FILL("_",131) FORMAT "x(120)"
WITH FRAME f-bot DOWN STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-UNDERLINE.


SESSION:SET-WAIT-STATE ("general").

ASSIGN 
 str-tit2 = c-win:TITLE 
 {sys/inc/ctrtext.i str-tit2 112}

 v-s-vend  = begin_vend
 v-e-vend  = end_vend
 v-s-type  = begin_type
 v-e-type  = end_type
 v-idate   = rd_date EQ "Invoice"
 v-sort    = rd_sort EQ "Name"
 v-dtl     = tb_detailed
 v-date    = as_of_date
 v-days[1] = period-days-1
 v-days[2] = period-days-2
 v-days[3] = period-days-3
 v-days[4] = period-days-4

 str-tit3 = "Company From: " + STRING(begin_comp) + " To: " + STRING(end_comp) + "      " + "As of Date: " + STRING(v-date)
 {sys/inc/ctrtext.i str-tit3 132}.
DEFINE VARIABLE cslist AS CHARACTER NO-UNDO.
 FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

   IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
   THEN ASSIGN str-tit4 = str-tit4 + ttRptSelected.TextList + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + "," .        
   ELSE 
   ASSIGN str-tit4 = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
          str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
          excelheader = excelHeader + ttRptSelected.TextList + ","
          .        
          cSlist = cSlist + ttRptSelected.FieldList + ",".

        IF LOOKUP(ttRptSelected.TextList, "AMOUNT") NE 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.


     ASSIGN
     str-tit4 = str-tit4 +  "          0 -"  + STRING(period-days-1,">>>9") + FILL(" ",8) +   
                      STRING(period-days-1 + 1) + " - " + STRING(period-days-2,">>>9") + FILL(" ",8) +
                      STRING(period-days-2 + 1) + " - " + STRING(period-days-3,">>>9") + FILL(" ",8) +
                      STRING(period-days-3 + 1) + " - " + STRING(period-days-4,">>>9") + FILL(" ",8) +
                      STRING(period-days-4 + 1) + "+"  + FILL(" ",12)  
    str-tit5 = str-tit5 + "-----------------" + " " + "-----------------" + " " + "-----------------" + " " +
                       "-----------------" + " " + "-----------------" + " "   
     str-line = str-line + "-----------------" + " " + "-----------------" + " " + "-----------------" + " " +
                       "-----------------" + " " + "-----------------" + " " .

DO WITH FRAME {&FRAME-NAME}:
 EMPTY TEMP-TABLE w-sort.

  DO li = 1 TO 4:
    CREATE w-sort.
    w-int = v-days[li].
  END.
  li = 0.
  FOR EACH w-sort BY w-int:
    ASSIGN
       li = li + 1
       v-days[li] = w-int.
    IF i GT 3 THEN LEAVE.
  END.
  ASSIGN 
   period-days-1:SCREEN-VALUE = STRING(v-days[1])
   period-days-1
   period-days-2:SCREEN-VALUE = STRING(v-days[2])
   period-days-2
   period-days-3:SCREEN-VALUE = STRING(v-days[3])
   period-days-3
   period-days-4:SCREEN-VALUE = STRING(v-days[4])
   period-days-4.
END.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

VIEW FRAME r-top.

  ASSIGN grand-t = 0.

  EMPTY TEMP-TABLE tt-vend.

  IF tb_elect AND tb_non-elect THEN DO:
     cPaymentList = "".
  END.
  ELSE IF tb_elect THEN DO:
      FOR EACH payment-type NO-LOCK WHERE payment-type.company = cocode 
                                   AND NOT payment-type.paperCheck: 
       cPaymentList = cPaymentList + payment-type.type + ",".                            
      END.
  END.
  ELSE IF tb_non-elect THEN DO:
      FOR EACH payment-type NO-LOCK WHERE payment-type.company = cocode 
                                AND payment-type.paperCheck: 
          cPaymentList = cPaymentList + payment-type.type + ",".                            
      END.
  END.
  

  FOR EACH company NO-LOCK WHERE
       company.company GE begin_comp AND
       company.company LE end_comp,

    EACH vend NO-LOCK
      WHERE vend.company EQ company.company
        AND vend.vend-no GE v-s-vend
        AND vend.vend-no LE v-e-vend
        AND vend.TYPE    GE v-s-type
        AND vend.TYPE    LE v-e-type
        AND ((vend.curr-code GE begin_curr    AND
              vend.curr-code LE end_curr)           OR
             (vend.curr-code EQ ""            AND
              company.curr-code GE begin_curr AND
              company.curr-code LE end_curr))
       AND ( LOOKUP(vend.payment-type,cPaymentList) NE 0 OR  cPaymentList EQ ""):
  
      {custom/statusMsg.i " 'Processing Vendor#  '  + STRING(vend.vend-no) "}

    FOR EACH ap-inv NO-LOCK
        WHERE ap-inv.company   EQ company.company
          AND ap-inv.vend-no   EQ vend.vend-no
          AND ap-inv.posted    EQ YES
          AND (ap-inv.inv-date LE as_of_date OR NOT v-idate)
        USE-INDEX ap-inv ,

        FIRST ap-ledger NO-LOCK
        WHERE ap-ledger.company  EQ company.company
          AND ap-ledger.vend-no  EQ ap-inv.vend-no
          AND ap-ledger.ref-date EQ ap-inv.inv-date
          AND ap-ledger.refnum   EQ ("INV# " + ap-inv.inv-no)
          AND (ap-ledger.tr-date LE as_of_date OR v-idate)
        USE-INDEX ap-ledger :

      CREATE tt-vend.
      ASSIGN
       tt-vend.curr-code = IF vend.curr-code EQ "" THEN company.curr-code
                                                   ELSE vend.curr-code
       tt-vend.sorter    = IF v-sort THEN vend.NAME ELSE vend.vend-no
       tt-vend.row-id    = ROWID(vend).

      IF tt-vend.curr-code NE company.curr-code THEN ll-mult-curr = YES.

      LEAVE.
    END.
  END.

  IF tb_excel THEN DO:
     OUTPUT STREAM excel TO VALUE(fi_file).

     excelheader = excelheader
                /* + "VENDOR#,VENDOR NAME,PHONE,TYPE,TERMS,INVOICE#,DATE,AMOUNT,#DAYS,"*/
                 + "0-" + STRING(period-days-1) + "," + STRING(period-days-1 + 1) + "-" 
                        + STRING(period-days-2) + "," + STRING(period-days-2 + 1) + "-" 
                        + STRING(period-days-3) + "," + STRING(period-days-3 + 1) + "-" 
                        + STRING(period-days-4) + "," + STRING(period-days-4 + 1) + "+" 
                        + " ".

     PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
  END.

  FOR EACH tt-vend,
      FIRST vend NO-LOCK WHERE ROWID(vend) EQ tt-vend.row-id 
      BREAK BY tt-vend.curr-code
            BY tt-vend.sorter:
     {custom/statusMsg.i " 'Processing Vendor#  '  + string(vend.vend-no) "}
    IF FIRST-OF(tt-vend.curr-code) THEN DO:
      lv-page-break = "Currency: " + TRIM(tt-vend.curr-code).

      IF ll-mult-curr OR FIRST(tt-vend.curr-code) THEN PAGE.
    END.

    {ap/ap-agedN.i}

    IF LAST-OF(tt-vend.curr-code) THEN DO:
       IF ll-mult-curr THEN DO:
          PUT SKIP(2).

          ASSIGN
           lv-f-bot-hdr = " CURR TOTALS"
           curr-t[6]    = 0.

          DO i = 1 TO 5:
            curr-t[6] = curr-t[6] + curr-t[i].
          END.

          VIEW FRAME f-bot.
          DOWN.

          DISPLAY SPACE(10) lv-f-bot-hdr
                  curr-t[1]
                  curr-t[2]
                  curr-t[3]
                  curr-t[4]
                  curr-t[5]
                  curr-t[6]
                  SKIP

              WITH FRAME bot1 NO-BOX NO-LABELS NO-ATTR-SPACE STREAM-IO WIDTH 200.

          DISPLAY "PERCENTAGE COMPOSITION" SPACE(2)
                  (curr-t[1] / t2) * 100 FORMAT "->>>>>>>>>>9.99%"
                  (curr-t[2] / t2) * 100 FORMAT "->>>>>>>>>>9.99%"
                  (curr-t[3] / t2) * 100 FORMAT "->>>>>>>>>>9.99%"
                  (curr-t[4] / t2) * 100 FORMAT "->>>>>>>>>>9.99%"
                  (curr-t[5] / t2) * 100 FORMAT "->>>>>>>>>>9.99%"

              WITH FRAME bot2 STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-ATTR-SPACE.

          IF tb_excel THEN
          DO:
            PUT STREAM excel UNFORMATTED
                SKIP(1).

            IF ll-mult-curr THEN
               PUT STREAM excel UNFORMATTED
                   '"' "" '",'.

                 excelheader = "," + "," +   "0-" + STRING(period-days-1) + "," + STRING(period-days-1 + 1) + "-" 
                        + STRING(period-days-2) + "," + STRING(period-days-2 + 1) + "-" 
                        + STRING(period-days-3) + "," + STRING(period-days-3 + 1) + "-" 
                        + STRING(period-days-4) + "," + STRING(period-days-4 + 1) + "+" 
                        + ",Total Payables,".

                 PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.

            PUT STREAM excel UNFORMATTED

                '"' ""                                  '",'
                '"' "CURR TOTALS"                       '",'
                '"' ""                                  '",'
                '"' STRING(curr-t[1],"$->>>,>>>,>>9.99") '",'
                '"' STRING(curr-t[2],"$->>>,>>>,>>9.99") '",'
                '"' STRING(curr-t[3],"$->>>,>>>,>>9.99") '",'
                '"' STRING(curr-t[4],"$->>>,>>>,>>9.99") '",'
                '"' STRING(curr-t[5],"$->>>,>>>,>>9.99") '",'
                '"' STRING(curr-t[6],"$->>>,>>>,>>9.99") '",'
                SKIP.

            IF ll-mult-curr THEN
               PUT STREAM excel UNFORMATTED
                   '"' "" '",'.

            PUT STREAM excel UNFORMATTED

                '"' "PERCENTAGE COMPOSITION"            '",'
                '"' ""                                  '",'
                '"' STRING((curr-t[1] / t2) * 100,"->>>>>>>>>>9.99%") '",'
                '"' STRING((curr-t[2] / t2) * 100,"->>>>>>>>>>9.99%") '",'
                '"' STRING((curr-t[3] / t2) * 100,"->>>>>>>>>>9.99%") '",'
                '"' STRING((curr-t[4] / t2) * 100,"->>>>>>>>>>9.99%") '",'
                '"' STRING((curr-t[5] / t2) * 100,"->>>>>>>>>>9.99%") '",'
                SKIP(1).
          END.
       END.

       DO i = 1 TO 5:
         grand-t[i] = grand-t[i] + curr-t[i].
       END.
       ASSIGN
        curr-t = 0
        t3     = t3 + t2
        t2     = 0.
    END.
  END.

  IF ll-mult-curr THEN DO:
    HIDE FRAME r-top-1 NO-PAUSE.
    HIDE FRAME r-top-2 NO-PAUSE.
    PAGE.
  END.

  PUT SKIP(2).

  ASSIGN
   lv-f-bot-hdr = "GRAND TOTALS"
   grand-t[6]   = 0.

  DO i = 1 TO 5:
     grand-t[6] = grand-t[6] + grand-t[i].
  END.

  VIEW FRAME f-bot.
  DOWN.

  DISPLAY SPACE(10) lv-f-bot-hdr
          grand-t[1]
          grand-t[2]
          grand-t[3]
          grand-t[4]
          grand-t[5]
          grand-t[6]
          skip

      WITH FRAME bot3 NO-BOX NO-LABELS NO-ATTR-SPACE STREAM-IO WIDTH 200.

  DISPLAY "PERCENTAGE COMPOSITION" SPACE(2)
          (grand-t[1] / t3) * 100 FORMAT "->>>>>>>>>>9.99%"
          (grand-t[2] / t3) * 100 FORMAT "->>>>>>>>>>9.99%"
          (grand-t[3] / t3) * 100 FORMAT "->>>>>>>>>>9.99%"
          (grand-t[4] / t3) * 100 FORMAT "->>>>>>>>>>9.99%"
          (grand-t[5] / t3) * 100 FORMAT "->>>>>>>>>>9.99%"

      WITH FRAME b4 STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-ATTR-SPACE.

  IF tb_excel THEN
     DO:
       PUT STREAM excel UNFORMATTED
           SKIP(1).

       /*IF ll-mult-curr THEN
          PUT STREAM excel UNFORMATTED
              '"' "" '",'.*/

       PUT STREAM excel UNFORMATTED

           '"' ""                                  '",' 
           '"' "GRAND TOTALS"                      '",'
           '"' ""                                  '",'
           '"' STRING(grand-t[1],"$->>>,>>>,>>9.99") '",'
           '"' STRING(grand-t[2],"$->>>,>>>,>>9.99") '",'
           '"' STRING(grand-t[3],"$->>>,>>>,>>9.99") '",'
           '"' STRING(grand-t[4],"$->>>,>>>,>>9.99") '",'
           '"' STRING(grand-t[5],"$->>>,>>>,>>9.99") '",'
           '"' STRING(grand-t[6],"$->>>,>>>,>>9.99") '",'
           SKIP.

       PUT STREAM excel UNFORMATTED
           SKIP(1).

       /*IF ll-mult-curr THEN
          PUT STREAM excel UNFORMATTED
              '"' "" '",'.*/

       PUT STREAM excel UNFORMATTED

           '"' ""                                  '",'
           '"' "PERCENTAGE COMPOSITION"            '",'
           '"' ""                                  '",'
           '"' STRING((grand-t[1] / t3) * 100,"->>>>>>>>>>9.99%") '",'
           '"' STRING((grand-t[2] / t3) * 100,"->>>>>>>>>>9.99%") '",'
           '"' STRING((grand-t[3] / t3) * 100,"->>>>>>>>>>9.99%") '",'
           '"' STRING((grand-t[4] / t3) * 100,"->>>>>>>>>>9.99%") '",'
           '"' STRING((grand-t[5] / t3) * 100,"->>>>>>>>>>9.99%") '",'
           SKIP.
     END.

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
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
  DEFINE VARIABLE lv-frame-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-group-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-field-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-field2-hdl AS HANDLE NO-UNDO.
  DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
  DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-label AS CHARACTER.

  lv-frame-hdl = FRAME {&FRAME-NAME}:HANDLE.
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
  lv-field-hdl = lv-group-hdl:FIRST-CHILD .

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") GT 0
        THEN DO:
           IF lv-field-hdl:LABEL NE ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                     .
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     .
              lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                  IF lv-field2-hdl:private-data = lv-field-hdl:NAME THEN DO:
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
       ENTRY(i,parm-lbl-list) NE "" THEN DO:

      lv-label = FILL(" ",34 - LENGTH(TRIM(ENTRY(i,parm-lbl-list)))) +
                 TRIM(ENTRY(i,parm-lbl-list)) + ":".

      PUT lv-label FORMAT "x(35)" AT 5
          SPACE(1)
          TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
          SKIP.              
    END.
  END.

  PUT FILL("-",80) FORMAT "x(80)" SKIP.

  PAGE.

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

