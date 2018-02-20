&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: jcrep\r-brdreN.w

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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEF VAR is-xprint-form AS LOGICAL NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "JOB #,CUSTOMER,RM ITEM#,RM QTY RECD,FG ITEM#,FG QTY INVCD," + 
                           "DIFFERENCE,FG QTY RECD,DIFFERENCE2,WASTE%,BOARD COST,FG MSF,RM MSF,INVOICE DATE,BASIS WEIGHT,CATEGORY"
       cFieldListToSelect = "job,cust,rm-itm,rm-qty,fg-itm,fg-qty-in," +
                            "diff,fg-qty-re,diff2,wast,brd-cst,t-msf,t-msf-rm,inv-date,basis-wht,cat"
       cFieldLength = "9,25,15,12,15,12," + "12,12,12,11,10,11,11,12,12,8"
       cFieldType = "c,c,c,i,c,i," + "i,i,i,i,i,i,i,c,c,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "JOB #,CUSTOMER,RM ITEM#,RM QTY RECD,FG ITEM#,FG QTY INVCD," + 
                           "DIFFERENCE,FG QTY RECD,DIFFERENCE2,WASTE%" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rd_jstat rd_ostat begin_rm-no ~
end_rm-no begin_procat end_procat begin_inv-date end_inv-date begin_job-no ~
begin_job-no2 end_job-no end_job-no2 sl_avail Btn_Def sl_selected Btn_Add ~
Btn_Remove btn_Up btn_down rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lbl_jstat rd_jstat lbl_ostat rd_ostat ~
begin_rm-no end_rm-no begin_procat end_procat begin_inv-date end_inv-date ~
begin_job-no begin_job-no2 end_job-no end_job-no2 sl_avail sl_selected ~
rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm ~
tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_inv-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_inv-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-boarec.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_jstat AS CHARACTER FORMAT "X(256)":U INITIAL "Job Status?" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_ostat AS CHARACTER FORMAT "X(256)":U INITIAL "Order Status?" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .95 NO-UNDO.

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

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_jstat AS CHARACTER INITIAL "All" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"All", "All"
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE rd_ostat AS CHARACTER INITIAL "All" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"All", "All"
     SIZE 29 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 9.19.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 9.05.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     lbl_jstat AT ROW 2.14 COL 26 COLON-ALIGNED NO-LABEL
     rd_jstat AT ROW 2.14 COL 41 NO-LABEL
     lbl_ostat AT ROW 3.43 COL 24 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     rd_ostat AT ROW 3.43 COL 41 NO-LABEL WIDGET-ID 60
     begin_rm-no AT ROW 4.67 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_rm-no AT ROW 4.67 COL 69 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_procat AT ROW 5.86 COL 27 COLON-ALIGNED HELP
          "Enter Begining Category"
     end_procat AT ROW 5.86 COL 69 COLON-ALIGNED HELP
          "Enter Ending Category"
     begin_inv-date AT ROW 7.05 COL 27 COLON-ALIGNED
     end_inv-date AT ROW 7.05 COL 69 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     begin_job-no AT ROW 8.24 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 8.24 COL 39 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 8.24 COL 69 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 8.24 COL 81 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     sl_avail AT ROW 10.81 COL 3.6 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 10.81 COL 39.8 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 10.81 COL 59 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 11.81 COL 39.8 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 12.81 COL 39.8 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 13.86 COL 39.8 WIDGET-ID 40
     btn_down AT ROW 14.86 COL 39.8 WIDGET-ID 42
     rd-dest AT ROW 17.29 COL 4 NO-LABEL
     lv-ornt AT ROW 17.29 COL 31 NO-LABEL
     lines-per-page AT ROW 17.29 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 19.05 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 20 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 21.19 COL 30
     tb_excel AT ROW 22.81 COL 49.8 RIGHT-ALIGNED
     tb_runExcel AT ROW 22.81 COL 71 RIGHT-ALIGNED
     fi_file AT ROW 23.76 COL 27.8 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 25.43 COL 25
     btn-cancel AT ROW 25.43 COL 56
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 10.1 COL 4.4 WIDGET-ID 38
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 10.05 COL 58.6 WIDGET-ID 44
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 16.33 COL 3
     RECT-6 AT ROW 16 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 93.4 BY 25.81.


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
         TITLE              = "Board Reconcilation"
         HEIGHT             = 26.1
         WIDTH              = 94.6
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
       begin_inv-date:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_inv-date:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_jstat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_jstat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_jstat".

/* SETTINGS FOR FILL-IN lbl_ostat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_ostat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_ostat".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_jstat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_ostat:PRIVATE-DATA IN FRAME FRAME-A     = 
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

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Board Reconcilation */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Board Reconcilation */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv-date C-Win
ON LEAVE OF begin_inv-date IN FRAME FRAME-A /* Beginning Invoice Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_procat C-Win
ON LEAVE OF begin_procat IN FRAME FRAME-A /* Beginning Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no C-Win
ON LEAVE OF begin_rm-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   apply "close" to this-procedure.
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
  RUN GetSelectionList.
  run run-report. 
  STATUS DEFAULT "Processing Complete".

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_job-no
                            &END_cust=END_job-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = ''
                             &begin_cust= begin_job-no
                             &END_cust=end_job-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust= begin_job-no
                                  &END_cust=end_job-no
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

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
  DEF VAR cSelectedList AS cha NO-UNDO.

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


&Scoped-define SELF-NAME end_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv-date C-Win
ON LEAVE OF end_inv-date IN FRAME FRAME-A /* Ending Invoice Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_procat C-Win
ON LEAVE OF end_procat IN FRAME FRAME-A /* Ending Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-no C-Win
ON LEAVE OF end_rm-no IN FRAME FRAME-A /* Ending Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
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
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_jstat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_jstat C-Win
ON VALUE-CHANGED OF rd_jstat IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_ostat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_ostat C-Win
ON VALUE-CHANGED OF rd_ostat IN FRAME FRAME-A
DO:
  assign {&self-name}.
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
    IF {&SELF-NAME}:IS-SELECTED(i) THEN DO:
       ASSIGN ldummy = sl_Avail:add-last({&SELF-NAME}:SCREEN-VALUE)
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
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    assign {&self-name}.
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

  ASSIGN
   begin_inv-date = DATE(01,01,YEAR(TODAY))
   END_inv-date   = DATE(12,31,9999).

  RUN DisplaySelectionList. 
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_rm-no.
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
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

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

  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:

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
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.
  DEF VAR cTmpList AS cha NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:
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
       IF LOOKUP(ENTRY(iCount,cTmpList), cTextListToSelect) = 0 THEN
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
  DISPLAY lbl_jstat rd_jstat lbl_ostat rd_ostat begin_rm-no end_rm-no 
          begin_procat end_procat begin_inv-date end_inv-date begin_job-no 
          begin_job-no2 end_job-no end_job-no2 sl_avail sl_selected rd-dest 
          lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 rd_jstat rd_ostat begin_rm-no end_rm-no begin_procat 
         end_procat begin_inv-date end_inv-date begin_job-no begin_job-no2 
         end_job-no end_job-no2 sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
         btn_Up btn_down rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
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
 DEF VAR cTmpList AS cha NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
 iColumnLength = 0.

 DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.     

    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
           ttRptSelected.DisplayOrder = i
           ttRptSelected.HeadingFromLeft = IF entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
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
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ----------------------------------------------- jc/rep/jc-mrec.p 07/98 JLF */
/* Job Material Reconciliation Report                                         */
/* -------------------------------------------------------------------------- */
/*{sys/form/r-topw.f}*/

def buffer b-jh for job-hdr.

def var v-fdate as   date format "99/99/9999" init 01/01/0001 NO-UNDO.
def var v-tdate like v-fdate                  init 12/31/9999 NO-UNDO.
def var v-fjob  like job.job-no NO-UNDO.
def var v-tjob  like v-fjob                   init "zzzzzz" NO-UNDO.
def var v-fjob2 like job.job-no2 NO-UNDO.
def var v-tjob2 like v-fjob2                  init 99 NO-UNDO.
def var v-stat  as   char format "!"          init "O" NO-UNDO.

def var v-up     like eb.num-up NO-UNDO.
def var v-on     like v-up NO-UNDO.

def var v-job     as   CHAR NO-UNDO.
def var v-frst    as   LOG NO-UNDO.
def var v-qty     as   DEC NO-UNDO.
def var v-pct     as   DEC NO-UNDO.
def var v-rm-qty  as   dec format "->>>,>>>,>>9" extent 4 NO-UNDO.
def var v-in-qty  like v-rm-qty NO-UNDO.
def var v-fg-qty  like v-rm-qty NO-UNDO.
def var v-diff    like v-rm-qty extent 2 NO-UNDO.
def var v-waste   AS DECIMAL  format "->>>,>>9.99" NO-UNDO.
DEF VAR v-msf     AS DECIMAL NO-UNDO.
DEF VAR v-msf-rm  AS DECIMAL NO-UNDO.
DEF VAR vl-inv-qty AS DEC NO-UNDO.
DEF VAR vl-tsq AS DEC NO-UNDO.
DEF VAR brd-cst   AS DEC NO-UNDO.
def var v-cost as dec no-undo.
def var v-cst-qty as dec no-undo.
DEF VAR v-brdcst AS DEC NO-UNDO.
DEFINE VARIABLE iSetMult AS INTEGER     NO-UNDO.

DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF VAR str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-line AS cha FORM "x(300)" NO-UNDO.
DEF VAR dInvdt AS DATE NO-UNDO.
DEFINE VARIABLE cOrdStat  AS   CHARACTER FORMAT "!"          INITIAL "O" NO-UNDO.


{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF VAR excelheader AS CHAR NO-UNDO.


DEF VAR cslist AS cha NO-UNDO.
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

        IF LOOKUP(ttRptSelected.TextList, "rm-qty,fg-qty-in,diff,fg-qty-re,diff2,wast") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

  v-stat    = SUBSTR(rd_jstat,1,1)
  cOrdStat    = SUBSTR(rd_ostat,1,1)

  v-fdate   = begin_inv-date
  v-tdate   = END_inv-date

  v-fjob    = fill(" ",6 - length(trim(begin_job-no))) +
              trim(begin_job-no) + string(int(begin_job-no2),"99")
  v-tjob    = fill(" ",6 - length(trim(end_job-no)))   +
              trim(end_job-no)   + string(int(end_job-no2),"99").


{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

for each tt-report where tt-report.term-id eq "":
  delete tt-report.
end.
/*
IF tb_excel THEN do:
  OUTPUT STREAM excel TO VALUE(fi_file).
  EXPORT STREAM excel DELIMITER "," 
      " "
      " "
      " "
      "RM QTY"
      " "
      "FG QTY"
      " "
      "FG QTY"
      " "
      " "
      SKIP.  
  EXPORT STREAM excel DELIMITER ","       
      "JOB #"
      "CUSTOMER"
      "RM ITEM#"
      "RECEIVED"
      "FG ITEM#"
      "INVOICED"
      "DIFFERENCE"
      "RECEIVED"
      "DIFFERENCE"
      "Waste%"
      SKIP.
END.
*/
display with frame r-top.

ASSIGN 
    vl-inv-qty = 0
    v-msf = 0 
    v-msf-rm = 0 
    vl-tsq = 0 .

    for each ar-inv
        where ar-inv.company  eq cocode
          and ar-inv.inv-date ge v-fdate
          and ar-inv.inv-date le v-tdate
          and ar-inv.posted   eq yes
        use-index inv-date no-lock,

        each ar-invl
        where ar-invl.x-no    eq ar-inv.x-no
          and ar-invl.job-no  ge substr(v-fjob,1,6)
          and ar-invl.job-no  le substr(v-tjob,1,6)
          and fill(" ",6 - length(trim(ar-invl.job-no))) +
              trim(ar-invl.job-no) + string(ar-invl.job-no2,"99")
                          ge v-fjob
          and fill(" ",6 - length(trim(ar-invl.job-no))) +
              trim(ar-invl.job-no) + string(ar-invl.job-no2,"99")
                          le v-tjob
          and ar-invl.inv-qty ne 0
        no-lock,

        first job
        where job.company eq cocode
          and job.job-no  eq ar-invl.job-no
          and job.job-no2 eq ar-invl.job-no2
          and (v-stat     eq "A"                    or
               (v-stat    eq "O" and job.opened)    or
               (v-stat    eq "C" and NOT job.opened))
        use-index job-no no-lock,

        first job-hdr
        where job-hdr.company eq cocode
          and job-hdr.job     eq job.job
          and job-hdr.job-no  eq job.job-no
          and job-hdr.job-no2 eq job.job-no2
          and job-hdr.i-no    eq ar-invl.i-no
        no-lock:

        {custom/statusMsg.i " 'Processing Job#  '  + job.job-no "}
          FIND FIRST oe-ordl NO-LOCK
               WHERE oe-ordl.company EQ cocode
                 AND oe-ordl.ord-no EQ job-hdr.ord-no 
                 AND oe-ordl.i-no EQ job-hdr.i-no NO-ERROR.
         IF AVAILABLE oe-ordl THEN DO:
             IF cOrdStat EQ "C" AND oe-ordl.stat NE "C" THEN NEXT.
             ELSE IF cOrdStat EQ "O" AND oe-ordl.stat EQ "C" THEN NEXT.
         END.

      v-job = fill(" ",6 - length(trim(job.job-no))) +
              trim(job.job-no) + "-" + string(job.job-no2,"99").
        ASSIGN dInvdt = ar-inv.inv-date.
        

      find first tt-report
          where tt-report.term-id EQ ""
            and tt-report.key-01  EQ v-job
            and tt-report.key-02  EQ ar-invl.i-no
          no-lock no-error.

      if not avail tt-report then do:
        find first est where est.company EQ job.company
                         AND est.est-no  EQ job.est-no
                       no-lock no-error.

        create tt-report.
        assign
         tt-report.term-id = ""
         tt-report.key-01  = v-job
         tt-report.key-02  = ar-invl.i-no
         tt-report.key-03  = if avail est                             and
                            (est.est-type eq 2 or est.est-type eq 6) then
                            "SET" else ""
         tt-report.rec-id  = recid(job-hdr).
         RELEASE tt-report.
      end.
    end.

    for each tt-report where tt-report.term-id eq "",

        first job-hdr where recid(job-hdr) eq tt-report.rec-id no-lock,

        first job
        where job.company eq job-hdr.company
          and job.job     eq job-hdr.job
          and job.job-no  eq job-hdr.job-no
          and job.job-no2 eq job-hdr.job-no2
        no-lock,

        each job-mat
        where job-mat.company eq cocode
          and job-mat.job     eq job-hdr.job
          and (job-mat.frm    eq job-hdr.frm or
               tt-report.key-03  eq "SET")
          AND job-mat.i-no    GE begin_rm-no
          AND job-mat.i-no    LE end_rm-no
        no-lock,

        first item
        where item.company  eq cocode
          and item.i-no     eq job-mat.i-no
          and item.mat-type eq "B"
          AND item.procat   GE begin_procat
          AND item.procat   LE end_procat
        no-lock

        break by item.industry
              by tt-report.key-01
              by tt-report.key-02
              by job-mat.frm:

        {custom/statusMsg.i " 'Processing Job#  '  + job.job-no "}

      if first-of(tt-report.key-01) then v-frst = yes.

      find first cust
          where cust.company eq cocode
            and cust.cust-no eq job-hdr.cust-no
          no-lock no-error.

      ASSIGN v-brdcst = 0 .
      for each mat-act
          where mat-act.company eq cocode
            and mat-act.job     eq job-mat.job
            and mat-act.s-num   eq job-mat.frm
            and mat-act.b-num   eq job-mat.blank-no
            and mat-act.i-no    eq job-mat.i-no
          USE-INDEX job
          no-lock:

          run sys/ref/convquom.p(job-mat.qty-uom, "EA", job-mat.basis-w,
                                 job-mat.len, job-mat.wid, item.s-dep,
                                 mat-act.qty, output v-qty).

          v-rm-qty[1] = v-rm-qty[1] + v-qty.

          v-cost       = 0.
          v-cst-qty    = 0.
          for each rm-rcpth
              where rm-rcpth.company   eq cocode
                and rm-rcpth.job-no    eq mat-act.job-no
                and rm-rcpth.job-no2   eq mat-act.job-no2
                and rm-rcpth.i-no      eq mat-act.i-no
                and rm-rcpth.rita-code eq "I"
              no-lock,

              each rm-rdtlh
              where rm-rdtlh.r-no eq rm-rcpth.r-no
              no-lock:

            assign
             v-cst-qty  = v-cst-qty  + rm-rdtlh.qty
             v-cost = v-cost + (rm-rdtlh.qty * rm-rdtlh.cost).
          end.
          v-cost = v-cost / v-cst-qty.

          IF v-cost = 0 THEN ASSIGN v-cost = mat-act.cost.

          IF job-mat.sc-uom NE mat-act.qty-uom THEN
           if item.r-wid eq 0 then do:
             run sys/ref/convcuom.p(job-mat.sc-uom,
                                    mat-act.qty-uom,
                                    (if job-mat.basis-w   ne 0 then job-mat.basis-w
                                     else item.basis-w),
                                    (if job-mat.len       ne 0 then job-mat.len
                                     else item.s-len),
                                    (if job-mat.wid       ne 0 then job-mat.wid
                                     else item.s-wid),
                                    item.s-dep, 
                                    mat-act.cost,
                                    output v-cost).
           end.

           else do:
             run sys/ref/convcuom.p(job-mat.sc-uom,
                                    mat-act.qty-uom,
                                    (if job-mat.basis-w   ne 0 then job-mat.basis-w
                                     else item.basis-w),
                                    job-mat.len,
                                    (if job-mat.wid       ne 0 then job-mat.wid
                                     else item.r-wid),
                                    item.s-dep, 
                                    mat-act.cost,
                                    output v-cost).
           end.

         IF mat-act.ext-cost EQ 0 OR mat-act.ext-cost EQ ? THEN
           v-brdcst = v-brdcst + (mat-act.qty * v-cost * 1).
         ELSE
           v-brdcst = v-brdcst + (mat-act.ext-cost * 1).

      end.
      v-msf-rm = ((v-rm-qty[1] * job-mat.len * job-mat.wid) / 144000) .
      IF v-rm-qty[1] EQ 0 THEN      /* get sheets from slitter */
        RUN sys/inc/slitshts.p (ROWID(job), job-mat.frm, OUTPUT v-rm-qty[1]).

      assign
       v-pct = 1
       v-up  = 1
       v-on  = 1.

      find FIRST est where est.company EQ job-hdr.company 
                 AND est.est-no  EQ job-hdr.est-no
               no-lock no-error.

      if avail est then do:
        run sys/inc/numup.p (est.company, est.est-no, job-mat.frm, output v-up).

        find first ef
            where ef.company   EQ est.company
              AND ef.est-no    EQ est.est-no
              and ef.form-no eq job-mat.frm
            no-lock no-error.

        IF AVAIL ef THEN DO:
          RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).
          v-on = v-up * v-on.

          find first eb
              where eb.company         eq ef.company
                AND eb.est-no          EQ ef.est-no
                and eb.form-no         eq ef.form-no
                and eb.blank-no        ne 0
              no-lock no-error.
        end.

        if est.est-type eq 3 then do:
          v-qty = 0.

          for each b-jh
              where b-jh.job     eq job-hdr.job
                and b-jh.job-no  eq job-hdr.job-no
                and b-jh.job-no2 eq job-hdr.job-no2
              no-lock:

            v-qty = v-qty + b-jh.qty.
          end.

          v-pct = job-hdr.qty / v-qty.
        end.

        else
        if est.est-type eq 4 or est.est-type eq 8 then
           v-pct = job-hdr.sq-in / 100.
      end.

      v-rm-qty[1] = round(v-rm-qty[1] * v-pct * v-on,0).

      IF first-of(job-mat.frm) THEN
      DO:
         iSetMult = 1.
         IF tt-report.key-03 EQ "SET" AND AVAIL eb THEN DO:
            FIND FIRST fg-set
                WHERE fg-set.company EQ cocode
                  AND fg-set.set-no EQ job-hdr.i-no
                  AND fg-set.part-no EQ eb.stock-no
                NO-LOCK NO-ERROR.
            IF AVAIL fg-set THEN iSetMult = fg-set.part-qty.
/*             {ce/set-qty.i v-qty eb} */
         END.

         for each ar-inv
             where ar-inv.company eq cocode
               and ar-inv.ord-no  eq job-hdr.ord-no
               and ar-inv.posted  eq yes
             use-index ord-no no-lock,

             each ar-invl
             where ar-invl.x-no    eq ar-inv.x-no
               and ar-invl.i-no    eq job-hdr.i-no
               and ar-invl.job-no  eq job-hdr.job-no
               and ar-invl.job-no2 eq job-hdr.job-no2
             no-lock:

           v-qty = ar-invl.inv-qty.
           ASSIGN dInvdt = ar-inv.inv-date.

           v-in-qty[1] = v-in-qty[1] + v-qty.
         end.

         for each fg-act
             where fg-act.company eq cocode
               and fg-act.job     eq job-hdr.job
               and fg-act.job-no  eq job-hdr.job-no
               and fg-act.job-no2 eq job-hdr.job-no2
               and fg-act.i-no    eq job-hdr.i-no
             no-lock:
           v-fg-qty[1] = v-fg-qty[1] + fg-act.qty.
         end.
      END.
       IF iSetMult GT 1 THEN
           ASSIGN 
            v-in-qty[1] = v-in-qty[1] * iSetMult
            v-fg-qty[1] = v-fg-qty[1] * iSetMult
            .
       IF v-in-qty[1] = ? THEN ASSIGN v-in-qty[1] = 0 .
       IF v-fg-qty[1] = ? THEN ASSIGN v-fg-qty[1] = 0 .
       IF v-rm-qty[1] = ? THEN ASSIGN v-rm-qty[1] = 0 .

      assign
       v-diff[1] = v-rm-qty[1] - v-in-qty[1]
       v-diff[2] = v-rm-qty[1] - v-fg-qty[1]
       v-waste = ROUND((v-diff[2] / v-rm-qty[1] * 100),2)  .
       IF v-waste = ? THEN ASSIGN v-waste = 0.
       IF v-diff[1] = ? THEN ASSIGN v-diff[1] = 0.
       IF v-diff[2] = ? THEN ASSIGN v-diff[2] = 0.

       FIND FIRST itemfg WHERE itemfg.company EQ cocode
           AND itemfg.i-no EQ (IF tt-report.key-03 eq "SET" and avail eb THEN eb.stock-no 
                                ELSE job-hdr.i-no) NO-LOCK NO-ERROR.
       IF AVAIL itemfg THEN
           vl-tsq = itemfg.t-sqft .

/*        FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company */
/*            AND oe-ordl.i-no EQ job-hdr.i-no                                */
/*            AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.                  */
/*                                                                            */
/*        IF AVAILABLE oe-ordl THEN         */
/*            vl-inv-qty = oe-ordl.inv-qty. */

        v-msf = ((v-in-qty[1] * vl-tsq) / 1000) .


 /*    display tt-report.key-01    column-label "    JOB #"      format "x(9)"
                                  when v-frst
              cust.name           column-label "CUSTOMER"       format "x(25)"
                                  when avail cust and v-frst
              job-mat.i-no        column-label "RM ITEM#"
              v-rm-qty[1]         column-label "  RM QTY!RECEIVED"
              job-hdr.i-no        column-label "FG ITEM#"
              eb.stock-no         when tt-report.key-03 eq "SET" and avail eb
                                  @ job-hdr.i-no
              v-in-qty[1]         column-label "  FG QTY!INVOICED"
              v-diff[1]           column-label "DIFFERENCE"
              v-fg-qty[1]         column-label "  FG QTY!RECEIVED"
              v-diff[2]           column-label "DIFFERENCE"
              v-waste             column-label "Waste%"

            with frame det STREAM-IO width 150 no-box down.

IF tb_excel THEN
  EXPORT STREAM excel DELIMITER "," 
      (IF v-frst THEN tt-report.key-01 ELSE " ")
      (IF AVAILABLE cust AND v-frst = TRUE THEN cust.name ELSE " ")
      job-mat.i-no 
      v-rm-qty[1]      
      (IF tt-report.key-03 eq "SET" and avail eb THEN eb.stock-no ELSE job-hdr.i-no)
      v-in-qty[1]
      v-diff[1] 
      v-fg-qty[1]
      v-diff[2]
      v-waste
      SKIP. */
             ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".

    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                 WHEN "job"       THEN cVarValue = IF v-frst THEN string(tt-report.key-01,"x(9)") ELSE " " .
                 WHEN "cust"      THEN cVarValue = IF AVAILABLE cust AND v-frst = TRUE THEN string(cust.name,"x(25)") ELSE " ".
                 WHEN "rm-itm"    THEN cVarValue = STRING(job-mat.i-no).
                 WHEN "rm-qty"    THEN cVarValue = STRING(v-rm-qty[1],"->>>,>>>,>>9") .
                 WHEN "fg-itm"    THEN cVarValue = IF tt-report.key-03 eq "SET" and avail eb THEN string(eb.stock-no) ELSE string(job-hdr.i-no) .
                 WHEN "fg-qty-in" THEN cVarValue = STRING(v-in-qty[1],"->>>,>>>,>>9")  .
                 WHEN "diff"      THEN cVarValue = STRING(v-diff[1],"->>>,>>>,>>9").
                 WHEN "fg-qty-re" THEN cVarValue = STRING(v-fg-qty[1],"->>>,>>>,>>9").
                 WHEN "diff2"     THEN cVarValue = string(v-diff[2],"->>>,>>>,>>9")  .
                 WHEN "wast"      THEN cVarValue = string(v-waste,"->>>,>>9.99") .
                 WHEN "brd-cst"   THEN cVarValue = string(v-brdcst,">>>>>>9.99") . 
                 WHEN "t-msf"     THEN cVarValue = string(v-msf,"->>>,>>9.99") .
                 WHEN "t-msf-rm"  THEN cVarValue = string(v-msf-rm,"->>>,>>9.99") .
                 WHEN "inv-date"  THEN cVarValue = IF dInvdt <> ? THEN STRING(dInvdt,"99/99/9999") ELSE ""      .
                 WHEN "basis-wht"    THEN cVarValue = IF AVAIL ITEM AND item.basis-w NE 0 THEN STRING(ITEM.basis-w,">>9.99") ELSE "".
                 WHEN "cat"    THEN cVarValue = IF AVAIL ITEM THEN STRING(ITEM.procat,"x(5)") ELSE "".



            END CASE.

            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.

    PUT UNFORMATTED cDisplay SKIP.
    IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
     END.


      assign
       v-rm-qty[2] = v-rm-qty[2] + v-rm-qty[1]
       v-in-qty[2] = v-in-qty[2] + v-in-qty[1]
       v-fg-qty[2] = v-fg-qty[2] + v-fg-qty[1]

       v-rm-qty[1] = 0
       v-in-qty[1] = 0
       v-fg-qty[1] = 0
       .

      if last-of(tt-report.key-01) then do:

         IF tb_excel THEN
            EXPORT STREAM excel DELIMITER "," 
            " "
            " "
            " " 
            " "      
            " "
            " "
            " " 
            " "
            " " 
            SKIP.

        if not v-frst then do:
          put skip(1).

          assign
           v-diff[1] = v-rm-qty[2] - v-in-qty[2]
           v-diff[2] = v-rm-qty[2] - v-fg-qty[2]

           v-waste = ROUND((v-diff[2] / v-rm-qty[2] * 100),2)  .
            IF v-waste = ? THEN ASSIGN v-waste = 0.


          clear frame det no-pause.

        /*  display ""             @ tt-report.key-01
                  "Job Totals"   @ cust.name
                  v-rm-qty[2]    @ v-rm-qty[1]
                  v-in-qty[2]    @ v-in-qty[1]
                  v-diff[1]
                  v-fg-qty[2]    @ v-fg-qty[1]
                  v-diff[2]
                  v-waste

              with frame det STREAM-IO width 150 no-box down.
          down with frame det. */

        ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".

              DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                 cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                      CASE cTmpField:             
                           WHEN "job"       THEN cVarValue = " " .
                           WHEN "cust"      THEN cVarValue = " ".
                           WHEN "rm-itm"    THEN cVarValue = "".
                           WHEN "rm-qty"    THEN cVarValue = STRING(v-rm-qty[2],"->>>,>>>,>>9") .
                           WHEN "fg-itm"    THEN cVarValue = "".
                           WHEN "fg-qty-in" THEN cVarValue = STRING(v-in-qty[2],"->>>,>>>,>>9")  .
                           WHEN "diff"      THEN cVarValue = STRING(v-diff[1],"->>>,>>>,>>9").
                           WHEN "fg-qty-re" THEN cVarValue = STRING(v-fg-qty[2],"->>>,>>>,>>9").
                           WHEN "diff2"     THEN cVarValue = string(v-diff[2],"->>>,>>>,>>9")  .
                           WHEN "wast"      THEN cVarValue = string(v-waste,"->>>,>>9.99") .
                           WHEN "brd-cst"   THEN cVarValue = "" . 
                           WHEN "t-msf"     THEN cVarValue = "" .
                           WHEN "t-msf-rm"  THEN cVarValue = "" .
                           WHEN "inv-date"  THEN cVarValue = "" .
                           WHEN "basis-wht"    THEN cVarValue = "".
                           WHEN "cat"    THEN cVarValue = "".

                      END CASE.

                      cExcelVarValue = cVarValue.
                      cDisplay = cDisplay + cVarValue +
                                 FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                      cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
              END.

              PUT UNFORMATTED "   Job Totals" substring(cDisplay,14,300) SKIP.

          put skip(1).
        end.

        put skip(1).

        assign
         v-rm-qty[3] = v-rm-qty[3] + v-rm-qty[2]
         v-in-qty[3] = v-in-qty[3] + v-in-qty[2]
         v-fg-qty[3] = v-fg-qty[3] + v-fg-qty[2]

         v-rm-qty[2] = 0
         v-in-qty[2] = 0
         v-fg-qty[2] = 0 .
      end.

      v-frst = no.

      if last-of(item.industry) then do:
        put skip(1).

        assign
         v-diff[1] = v-rm-qty[3] - v-in-qty[3]
         v-diff[2] = v-rm-qty[3] - v-fg-qty[3]

         v-waste = ROUND((v-diff[2] / v-rm-qty[3] * 100),2)  .
            IF v-waste = ? THEN ASSIGN v-waste = 0.


        clear frame det no-pause.

      /*  display ""             @ tt-report.key-01
                (if item.industry eq "1" then "Folding" else "Corrugated") +
                     " Totals" @ cust.name
                v-rm-qty[3]    @ v-rm-qty[1]
                v-in-qty[3]    @ v-in-qty[1]
                v-diff[1]
                v-fg-qty[3]    @ v-fg-qty[1]
                v-diff[2]
                v-waste

            with frame det STREAM-IO width 150 no-box down.
        down with frame det. */
                     ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".

              DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                 cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                      CASE cTmpField:             
                           WHEN "job"       THEN cVarValue = " " .
                           WHEN "cust"      THEN cVarValue = " ".
                           WHEN "rm-itm"    THEN cVarValue = "".
                           WHEN "rm-qty"    THEN cVarValue = STRING(v-rm-qty[3],"->>>,>>>,>>9") .
                           WHEN "fg-itm"    THEN cVarValue = "".
                           WHEN "fg-qty-in" THEN cVarValue = STRING(v-in-qty[3],"->>>,>>>,>>9")  .
                           WHEN "diff"      THEN cVarValue = STRING(v-diff[1],"->>>,>>>,>>9").
                           WHEN "fg-qty-re" THEN cVarValue = STRING(v-fg-qty[3],"->>>,>>>,>>9").
                           WHEN "diff2"     THEN cVarValue = string(v-diff[2],"->>>,>>>,>>9")  .
                           WHEN "wast"      THEN cVarValue = string(v-waste,"->>>,>>9.99") .
                           WHEN "brd-cst"   THEN cVarValue = "" . 
                           WHEN "t-msf"     THEN cVarValue = "" .
                           WHEN "t-msf-rm"  THEN cVarValue = "" .
                           WHEN "inv-date"  THEN cVarValue = "" .
                           WHEN "basis-wht"    THEN cVarValue = "".
                           WHEN "cat"    THEN cVarValue = "".

                      END CASE.

                      cExcelVarValue = cVarValue.
                      cDisplay = cDisplay + cVarValue +
                                 FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                      cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
              END.

              if item.industry eq "1" then
                  PUT UNFORMATTED "   Folding Totals" substring(cDisplay,18,300) SKIP.
              else 
                  PUT UNFORMATTED "   Corrugated Totals" substring(cDisplay,21,300) SKIP.

        PAGE.

        assign
         v-rm-qty[4] = v-rm-qty[4] + v-rm-qty[3]
         v-in-qty[4] = v-in-qty[4] + v-in-qty[3]
         v-fg-qty[4] = v-fg-qty[4] + v-fg-qty[3]

         v-rm-qty[3] = 0
         v-in-qty[3] = 0
         v-fg-qty[3] = 0 .
      end.

      if LAST(item.industry) then do:
        put skip(1).

        assign
         v-diff[1] = v-rm-qty[4] - v-in-qty[4]
         v-diff[2] = v-rm-qty[4] - v-fg-qty[4]

         v-waste = ROUND((v-diff[2] / v-rm-qty[4] * 100),2)  .
          IF v-waste = ? THEN ASSIGN v-waste = 0.

        clear frame det no-pause.

      /*  display ""             @ tt-report.key-01
                "Grand Totals" @ cust.name
                v-rm-qty[4]    @ v-rm-qty[1]
                v-in-qty[4]    @ v-in-qty[1]
                v-diff[1]
                v-fg-qty[4]    @ v-fg-qty[1]
                v-diff[2]
                v-waste

            with frame det STREAM-IO width 150 no-box down.
        down with frame det. */

                 ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".

              DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                 cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                      CASE cTmpField:             
                           WHEN "job"       THEN cVarValue = " " .
                           WHEN "cust"      THEN cVarValue = " ".
                           WHEN "rm-itm"    THEN cVarValue = "".
                           WHEN "rm-qty"    THEN cVarValue = STRING(v-rm-qty[4],"->>>,>>>,>>9") .
                           WHEN "fg-itm"    THEN cVarValue = "".
                           WHEN "fg-qty-in" THEN cVarValue = STRING(v-in-qty[4],"->>>,>>>,>>9")  .
                           WHEN "diff"      THEN cVarValue = STRING(v-diff[1],"->>>,>>>,>>9").
                           WHEN "fg-qty-re" THEN cVarValue = STRING(v-fg-qty[4],"->>>,>>>,>>9").
                           WHEN "diff2"     THEN cVarValue = string(v-diff[2],"->>>,>>>,>>9")  .
                           WHEN "wast"      THEN cVarValue = string(v-waste,"->>>,>>9.99") .
                           WHEN "brd-cst"   THEN cVarValue = "" . 
                           WHEN "t-msf"     THEN cVarValue = "" .
                           WHEN "t-msf-rm"  THEN cVarValue = "" .
                           WHEN "inv-date"  THEN cVarValue = "" .
                           WHEN "basis-wht"    THEN cVarValue = "".
                           WHEN "cat"    THEN cVarValue = "".

                      END CASE.

                      cExcelVarValue = cVarValue.
                      cDisplay = cDisplay + cVarValue +
                                 FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                      cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
              END.

              PUT UNFORMATTED "    Grand Totals"  substring(cDisplay,17,300) SKIP.
      end.
    end.    

IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
     IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
 END.


RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE show-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-frame-hdl as handle no-undo.
  def var lv-group-hdl as handle no-undo.
  def var lv-field-hdl as handle no-undo.
  def var lv-field2-hdl as handle no-undo.
  def var parm-fld-list as cha no-undo.
  def var parm-lbl-list as cha no-undo.
  def var i as int no-undo.
  def var lv-label as cha.

  lv-frame-hdl = frame {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:first-child.
  lv-field-hdl = lv-group-hdl:first-child .

  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," 
                     .
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     .
              lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name then do:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".
                  end.
                  lv-field2-hdl = lv-field2-hdl:next-sibling.                 
              end.       
           end.                 
        end.            
     lv-field-hdl = lv-field-hdl:next-sibling.   
  end.

  put space(28)
      "< Selection Parameters >"
      skip(1).

  do i = 1 to num-entries(parm-fld-list,","):
    if entry(i,parm-fld-list) ne "" or
       entry(i,parm-lbl-list) ne "" then do:

      lv-label = fill(" ",34 - length(trim(entry(i,parm-lbl-list)))) +
                 trim(entry(i,parm-lbl-list)) + ":".

      put lv-label format "x(35)" at 5
          space(1)
          trim(entry(i,parm-fld-list)) format "x(40)"
          skip.              
    end.
  end.

  put fill("-",80) format "x(80)" skip.

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
  RETURN string(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

