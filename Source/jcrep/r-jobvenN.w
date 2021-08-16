&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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
DEF STREAM excel.

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

{jc/rep/job-sum.i new}


DEFINE VARIABLE ldummy              AS LOGICAL          NO-UNDO.
DEFINE VARIABLE cTextListToSelect   AS CHARACTER        NO-UNDO.
DEFINE VARIABLE cFieldListToSelect  AS CHARACTER        NO-UNDO.
DEFINE VARIABLE cFieldLength        AS CHARACTER        NO-UNDO.
DEFINE VARIABLE cFieldType          AS CHARACTER        NO-UNDO.
DEFINE VARIABLE iColumnLength       AS INTEGER          NO-UNDO.
DEFINE VARIABLE cTextListToDefault  AS CHARACTER        NO-UNDO.
DEFINE VARIABLE cFileName           AS CHARACTER        NO-UNDO.

DEFINE VARIABLE lRecFound           AS LOGICAL          NO-UNDO.
DEFINE VARIABLE lAPInvoiceLength    AS LOGICAL          NO-UNDO.
DEFINE VARIABLE cNK1Value           AS CHARACTER        NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "APInvoiceLength", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cNK1Value, OUTPUT lRecFound).
IF lRecFound THEN
    lAPInvoiceLength = logical(cNK1Value) NO-ERROR.

ASSIGN cTextListToSelect  = "CUST #,CUST NAME,JOB #,FG ITEM,QUANTITY,PRICE,UOM,VENDOR NAME,INVOICE," +
                            "INV QTY,REC QTY,P & P QTY PRODUCED,DIFF,VEND $ W/O SETUP,$ PER SHEET"
       cFieldListToSelect = "cust,cust-name,job,fgitem,qty,price,uom,vend-name,inv," +
                            "inv-qty,rec-qty,qty-prod,diff,vend-setup,per-sheet"
       cFieldType         = "c,c,c,c,i,i,c,c,c," + "i,i,i,i,i,i" 
    .
IF lAPInvoiceLength THEN
      ASSIGN cFieldLength = "8,30,9,15,8,10,3,30,20," + "10,10,8,6,10,5".
ELSE
      ASSIGN cFieldLength = "8,30,9,15,8,10,3,30,12," + "10,10,8,6,10,5".
      
      
{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "CUST #,CUST NAME,JOB #,FG ITEM,QUANTITY,PRICE,UOM,VENDOR NAME,INVOICE," +
                             "INV QTY,REC QTY,P & P QTY PRODUCED,DIFF,VEND $ W/O SETUP,$ PER SHEET" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rd_jstat begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_cust end_cust begin_date ~
end_date sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down ~
rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lbl_jstat rd_jstat begin_job-no ~
begin_job-no2 end_job-no end_job-no2 begin_cust end_cust begin_date ~
end_date sl_avail sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 

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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
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

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-jobven.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 43 BY 1.

DEFINE VARIABLE lbl_jstat AS CHARACTER FORMAT "X(256)":U INITIAL "Job Status?" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .95 NO-UNDO.

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
     SIZE 17 BY 4.76 NO-UNDO.

DEFINE VARIABLE rd_jstat AS CHARACTER INITIAL "All" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"All", "All"
     SIZE 29 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 6.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 6.14.

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
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     lbl_jstat AT ROW 2.43 COL 28 COLON-ALIGNED NO-LABEL
     rd_jstat AT ROW 2.43 COL 43 NO-LABEL
     begin_job-no AT ROW 4.1 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 4.1 COL 36 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 4.1 COL 67 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 4.1 COL 79 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_cust AT ROW 5.05 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Customer"
     end_cust AT ROW 5.05 COL 67 COLON-ALIGNED HELP
          "Enter Ending Customer"
     begin_date AT ROW 6 COL 24 COLON-ALIGNED
     end_date AT ROW 6 COL 67 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     sl_avail AT ROW 8.86 COL 3 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 8.86 COL 40.6 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 8.86 COL 60.8 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 9.86 COL 40.6 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 10.86 COL 40.6 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 11.91 COL 40.6 WIDGET-ID 40
     btn_down AT ROW 12.91 COL 40.6 WIDGET-ID 42
     lv-ornt AT ROW 15.29 COL 32 NO-LABEL
     lines-per-page AT ROW 15.29 COL 85 COLON-ALIGNED
     rd-dest AT ROW 15.67 COL 6 NO-LABEL
     td-show-parm AT ROW 16.52 COL 49
     lv-font-no AT ROW 16.57 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 17.52 COL 29 COLON-ALIGNED NO-LABEL
     fi_file AT ROW 19.33 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     tb_OpenCSV AT ROW 19.43 COL 90 RIGHT-ALIGNED
     tbAutoClose AT ROW 21.43 COL 31.2 WIDGET-ID 16
     btn-ok AT ROW 22.57 COL 31
     btn-cancel AT ROW 22.57 COL 51
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 8.14 COL 4.2 WIDGET-ID 38
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 8.14 COL 60.2 WIDGET-ID 44
     " Output Destination" VIEW-AS TEXT
          SIZE 18.8 BY .62 AT ROW 14.71 COL 5
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21.2 BY .71 AT ROW 1.14 COL 5
     RECT-6 AT ROW 15 COL 4
     RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96 BY 23.81
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
         TITLE              = "Job/Vendor Analysis Report"
         HEIGHT             = 23.86
         WIDTH              = 96
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
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

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
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_jstat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_jstat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_jstat".

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

ASSIGN 
       rd_jstat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

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
ON END-ERROR OF C-Win /* Job/Vendor Analysis Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Job/Vendor Analysis Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
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
  
  IF rd-dest EQ 3 THEN
  DO:
    fi_file:SCREEN-VALUE = "c:\tmp\r-jobven.csv".
    ASSIGN fi_file.
    RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
    fi_file:SCREEN-VALUE =  cFileName.
  END.
  
  RUN GetSelectionList. 
  run run-report. 

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN DO:
           IF NOT tb_OpenCSV THEN DO:        
                  MESSAGE "CSV file have been created." SKIP(1)
                           "~"OK"~"Want to open CSV file?"
                  VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                  TITLE "" UPDATE lChoice AS LOGICAL.
                 
                  IF lChoice THEN
                  DO:
                     OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
                  END.
              END.
           END. /* WHEN 3 THEN DO: */
       WHEN 4 THEN DO:
          {custom/asimailr.i &TYPE = ''
                             &begin_cust= begin_job-no
                             &END_cust=end_job-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
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


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
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


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Name */
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
  ASSIGN {&self-name}.
  RUN pChangeDest .
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


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
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
      begin_date  = DATE (1,1,YEAR(TODAY))
      END_date    = DATE (12,31,year(TODAY)).
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
  {sys/inc/reportsConfigNK1.i "JR9" }
  ASSIGN
    td-show-parm:SENSITIVE = lShowParameters
    td-show-parm:HIDDEN = NOT lShowParameters
    td-show-parm:VISIBLE = lShowParameters
    .

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_job-no.
  END.
  RUN pChangeDest .
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
  DISPLAY lbl_jstat rd_jstat begin_job-no begin_job-no2 end_job-no end_job-no2 
          begin_cust end_cust begin_date end_date sl_avail sl_selected rd-dest 
          fi_file tb_OpenCSV tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 rd_jstat begin_job-no begin_job-no2 end_job-no 
         end_job-no2 begin_cust end_cust begin_date end_date sl_avail Btn_Def 
         sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file 
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
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ----------------------------------------------- jc/rep/jobvend.p 12/98 JLF */
/* Job/Vendor Analysis                                                        */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-top3w.f}*/

def buffer b-jh for job-hdr.

def var v-stat  as   char format "!"          init "O".
def var v-fjob  like job.job-no.
def var v-tjob  like v-fjob                   init "zzzzzz".
def var v-fjob2 like job.job-no2.
def var v-tjob2 like v-fjob2                  init 99.
def var v-fcust like job-hdr.cust-no          init "".
def var v-tcust like v-fcust                  init "zzzzzzzz".
def var v-fdate as   date format "99/99/9999" init 01/01/0001.
def var v-tdate like v-fdate                  init 12/31/9999.

def var v-up     like eb.num-up.
def var v-on     like v-up.

def var v-job     as   char.
def var v-frst    as   log.
def var v-qty     as   dec.
def var v-mat-qty like rm-rdtlh.qty format ">>>>>>>9".
def var v-inv-qty like rm-rdtlh.qty.
def var v-fg-qty  as   dec.
def var v-cost    as   dec.
def var v-diff    like rm-rdtlh.qty.


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

{sys/form/r-top5L3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

  v-stat        = SUBSTR(rd_jstat,1,1)

  v-fjob        = fill(" ",6 - length(trim(begin_job-no))) +
                  trim(begin_job-no) + string(int(begin_job-no2),"99")
  v-tjob        = fill(" ",6 - length(trim(end_job-no)))   +
                  trim(end_job-no)   + string(int(end_job-no2),"99")  

  v-fcust       = begin_cust
  v-tcust       = END_cust
  v-fdate       = begin_date
  v-tdate       = END_date. 


DEF VAR cslist AS cha NO-UNDO.
 FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

    IF ttRptSelected.TextList = "P & P QTY PRODUCED" THEN ASSIGN
        str-tit3 = str-tit3 + " P&P QTY" + " "
        str-tit4 = str-tit4 + "PRODUCED" + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + "," .
    ELSE IF ttRptSelected.TextList = "VEND $ W/O SETUP" THEN ASSIGN
        str-tit3 = str-tit3 + "    VEND $" + " "
        str-tit4 = str-tit4 + " W/O SETUP" + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + "," .
    ELSE IF ttRptSelected.TextList = "$ PER SHEET" THEN ASSIGN
        str-tit3 = str-tit3 + "$ PER" + " "
        str-tit4 = str-tit4 + "SHEET" + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + "," .
    ELSE DO:


   IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
   THEN ASSIGN str-tit4 = str-tit4 + ttRptSelected.TextList + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               str-tit3 = str-tit3 + FILL(" ",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + "," .        
   ELSE 
   ASSIGN str-tit4 = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
          str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
          str-tit3 = str-tit3 + FILL(" ",ttRptSelected.FieldLength) + " "
          excelheader = excelHeader + ttRptSelected.TextList + ","
          .        

    END.
          cSlist = cSlist + ttRptSelected.FieldList + ",".

        IF LOOKUP(ttRptSelected.TextList, "Daily Sq Ft/M,Amount1,PTD Sq Ft/M,Amount2,YTD Sq Ft/M,Amount3") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF rd-dest EQ 3 THEN DO:
  OUTPUT STREAM excel TO VALUE(cFileName).
  /*excelheader = "CUST #,CUST NAME,JOB #,FG ITEM,QUANTITY,PRICE,UOM,VENDOR NAME,INVOICE,"
              + "INV QTY,REC QTY,P & P QTY PRODUCED,DIFF,VEND $ W/O SETUP,$ PER SHEET".*/
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

display "" with frame r-top.

 {sa/sa-sls01.i}

    for each job-hdr
        where job-hdr.company eq cocode
          and job-hdr.cust-no ge v-fcust
          and job-hdr.cust-no le v-tcust
          and job-hdr.job-no  ge substr(v-fjob,1,6)
          and job-hdr.job-no  le substr(v-tjob,1,6)
          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) + string(job-hdr.job-no2,"99")
                          ge v-fjob
          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) + string(job-hdr.job-no2,"99")
                          le v-tjob
        use-index cust-idx no-lock,

        first job
        where job.company eq cocode
          and job.job     eq job-hdr.job
          and job.job-no  eq job-hdr.job-no
          and job.job-no2 eq job-hdr.job-no2
          and (v-stat     eq "A"                    or
               (v-stat    eq "O" and job.opened)    or
               (v-stat    eq "C" and NOT job.opened))
        use-index job no-lock,

        first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq job-hdr.i-no
        no-lock:

      release oe-ordl.
      if job-hdr.ord-no ne 0 then
      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq job-hdr.ord-no
            and oe-ordl.i-no    eq job-hdr.i-no
          no-lock no-error.

      if avail oe-ordl and
         (oe-ordl.req-date lt v-fdate or
          oe-ordl.req-date gt v-tdate) then next.

      else
      if job.start-date + 60 lt v-fdate or
         job.start-date + 60 gt v-tdate then next.

      v-job = fill(" ",6 - length(trim(job.job-no))) +
              trim(job.job-no) + "-" + string(job.job-no2,"99").

      create report.
      assign
       report.term-id = v-term
       report.key-01  = "1"
       report.key-02  = job-hdr.cust-no
       report.key-03  = v-job
       report.key-04  = job-hdr.i-no
       report.rec-id  = recid(job-hdr).
    end.

    for each report
        where report.term-id eq v-term
          and report.key-01  eq "1",

        first job-hdr where recid(job-hdr) eq report.rec-id no-lock,

        first job
        where job.company eq cocode
          and job.job     eq job-hdr.job
          and job.job-no  eq job-hdr.job-no
          and job.job-no2 eq job-hdr.job-no2
        no-lock

        break by report.key-02
              by report.key-03
              by report.key-04:

      if first-of(report.key-02) then do:
        if first(report.key-02) then
        DO:
           put skip.
           IF rd-dest EQ 3 THEN
              PUT STREAM excel UNFORMATTED SKIP(1).
        END.
        find first cust
            where cust.company eq cocode
              and cust.cust-no eq job-hdr.cust-no
            no-lock no-error.
      /*  put skip(1) "Customer:" space(1) job-hdr.cust-no.

        if avail cust then put space(1) cust.name.
        put skip(1).*/
      end.

      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq job-hdr.ord-no
            and oe-ordl.i-no    eq job-hdr.i-no
          no-lock no-error.

      /*display report.key-03         to 9    format "x(9)"
              job-hdr.i-no          at 11
              job-hdr.qty           to 34   format ">>>>>>>9"
              oe-ordl.price         to 45   format ">>>>>>9.99<<<<"
                                    when avail oe-ordl
              oe-ordl.pr-uom        at 47
                                    when avail oe-ordl

          with frame det STREAM-IO width 132 no-box no-labels down.

      IF rd-dest EQ 3 THEN
         PUT STREAM excel UNFORMATTED
            SKIP
            '"' IF first-of(report.key-02) THEN
                   job-hdr.cust-no ELSE ""                '",'
            '"' IF FIRST-OF(report.key-02) AND
                   AVAIL cust THEN cust.NAME ELSE ""      '",'
            '"' report.key-03                             '",'
            '"' job-hdr.i-no                              '",'
            '"' STRING(job-hdr.qty,">>>>>>>9")            '",'
            '"' IF AVAIL oe-ordl THEN
                   STRING(oe-ordl.price,">>>>>>9.99<<<<")
                ELSE ""                                   '",'
            '"' IF AVAIL oe-ordl THEN oe-ordl.pr-uom
                ELSE ""                                   '",'.*/

       ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = string(job-hdr.cust-no,"x(8)") .
                         WHEN "cust-name"   THEN cVarValue = IF AVAIL cust THEN string(cust.NAME,"x(30)") ELSE "".
                         WHEN "job"   THEN cVarValue = STRING(report.key-03,"x(9)").
                         WHEN "fgitem"  THEN cVarValue = STRING(job-hdr.i-no,"x(15)") .
                         WHEN "qty"   THEN cVarValue = STRING(job-hdr.qty,">>>>>>>9") .
                         WHEN "price"  THEN cVarValue = IF AVAIL oe-ordl THEN STRING(oe-ordl.price,">>>>>>9.99<<<<") ELSE "".
                         WHEN "uom"   THEN cVarValue = IF AVAIL oe-ordl THEN STRING(oe-ordl.pr-uom,"x(3)") ELSE "" .
                         WHEN "vend-name"  THEN cVarValue = "" .
                         WHEN "inv"    THEN cVarValue = "" .
                         WHEN "inv-qty"   THEN cVarValue = "".
                         WHEN "rec-qty"   THEN cVarValue = "".
                         WHEN "qty-prod"  THEN cVarValue = "" .
                         WHEN "diff"   THEN cVarValue = "" .
                         WHEN "vend-setup"  THEN cVarValue = "" .
                         WHEN "per-sheet"   THEN cVarValue = ""  .


                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest EQ 3 THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

      v-on = 1.

      find est where est.company EQ job-hdr.company
               AND   est.est-no  EQ job-hdr.est-no
               no-lock no-error.

      if avail est then do:
        run sys/inc/numup.p (est.company, est.est-no, job-hdr.frm, output v-on).

        find first ef
            where ef.company   EQ est.company
              AND ef.est-no    EQ ef.est-no
              and ef.form-no eq job-hdr.frm
            no-lock no-error.

        IF AVAIL ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).
      end.

      v-fg-qty = 0.

      RUN fg/GetProductionQty.p (INPUT job-hdr.company,
                INPUT job-hdr.job-no,
                INPUT job-hdr.job-no2,
                INPUT job-hdr.i-no,
                INPUT NO,
                OUTPUT v-fg-qty).

      {sys/inc/roundup.i v-fg-qty}

      assign
       v-mat-qty = 0
       v-cost    = 0.

      for each rm-rdtlh
          where rm-rdtlh.company   eq cocode
            and rm-rdtlh.job-no    eq job-hdr.job-no
            and rm-rdtlh.job-no2   eq job-hdr.job-no2
            and rm-rdtlh.rita-code eq "I"
          use-index job no-lock,

          first rm-rcpth
          where rm-rcpth.r-no      eq rm-rdtlh.r-no
            and rm-rcpth.po-no     ne ""
            and rm-rcpth.rita-code eq "I"
          no-lock,

          first item
          where item.company  eq cocode
            and item.i-no     eq rm-rcpth.i-no
            and item.mat-type eq "B"
          no-lock

          break by rm-rcpth.po-no:

        v-mat-qty = v-mat-qty + rm-rdtlh.qty.

        if last-of(rm-rcpth.po-no) then do:
          do i = 1 to length(trim(rm-rcpth.po-no)):
            if substr(rm-rcpth.po-no,i,1) lt "0" or
               substr(rm-rcpth.po-no,i,1) gt "9" then do:
              i = 0.
              leave.
            end.
          end.

          release po-ord.
          release po-ordl.
          if i ne 0 then
          find first po-ord
              where po-ord.company eq cocode
                and po-ord.po-no   eq int(rm-rcpth.po-no)
              no-lock no-error.

          if avail po-ord then
          find first po-ordl
              where po-ordl.company eq cocode
                and po-ordl.po-no   eq po-ord.po-no
                and po-ordl.i-no    eq rm-rcpth.i-no
                and po-ordl.job-no  eq rm-rdtlh.job-no
                and po-ordl.job-no2 eq rm-rdtlh.job-no2
              use-index po-no no-lock no-error.

          if avail po-ordl then do:
            if item.cons-uom ne "EA" then
              run sys/ref/convquom.p(item.cons-uom, "EA", item.basis-w,
                                     po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                     v-mat-qty, output v-mat-qty).

            for each ap-inv
                where ap-inv.company eq cocode
                  and ap-inv.vend-no eq po-ord.vend-no
                  and ap-inv.po-no   eq po-ordl.po-no
                  and ap-inv.posted  eq yes
                use-index ap-inv no-lock,

                each ap-invl
                where ap-invl.i-no eq ap-inv.i-no
                  and ap-invl.line eq po-ordl.line
                no-lock:

              v-cost = v-cost + ap-invl.amt.

              create xreport.
              assign
               xreport.term-id = v-term
               xreport.key-01  = "2"
               xreport.key-02  = po-ord.vend-no
               xreport.key-03  = ap-inv.inv-no
               xreport.key-04  = po-ordl.i-no
               xreport.rec-id  = recid(ap-invl).
            end.

            for each ap-inv
                where ap-inv.company eq cocode
                  and ap-inv.vend-no eq po-ord.vend-no
                  and ap-inv.po-no   eq 0
                  and ap-inv.posted  eq yes
                use-index ap-inv no-lock,

                each ap-invl
                where ap-invl.i-no       eq ap-inv.i-no
                  and ap-invl.po-no      eq po-ordl.po-no
                  and {ap/invlline.i -1} eq po-ordl.line
                no-lock:

              v-cost = v-cost + ap-invl.amt.

              create xreport.
              assign
               xreport.term-id = v-term
               xreport.key-01  = "2"
               xreport.key-02  = po-ord.vend-no
               xreport.key-03  = ap-inv.inv-no
               xreport.key-04  = po-ordl.i-no
               xreport.rec-id  = recid(ap-invl).
            end.
          end.
        end.
      end.

      find first xreport
          where xreport.term-id eq v-term
            and xreport.key-01  eq "2"
          no-lock no-error.
      if not avail report then do:
        create xreport.
        assign
         xreport.term-id = v-term
         xreport.key-01  = "2".
      end.

      v-inv-qty = 0.

      for each xreport
          where xreport.term-id eq v-term
            and xreport.key-01  eq "2",

          first job-mat
          where job-mat.company eq cocode
            and job-mat.job     eq job-hdr.job
            and (job-mat.frm    eq job-hdr.frm or
                 (avail est and (est.est-type eq 2 or est.est-type eq 6)))
            and job-mat.i-no    eq xreport.key-04
          no-lock

          break by xreport.key-02
                by xreport.key-03:

        find ap-invl where recid(ap-invl) eq xreport.rec-id no-lock no-error.
        if avail ap-invl then do:
          find first ap-inv where ap-inv.i-no eq ap-invl.i-no no-lock.
          find first vend
              where vend.company eq cocode
                and vend.vend-no eq ap-inv.vend-no
              no-lock no-error.

          if ap-invl.cons-uom eq "EA" then
            v-qty = ap-invl.qty.
          else
            run sys/ref/convquom.p(ap-invl.cons-uom, "EA", job-mat.basis-w,
                                   job-mat.len, job-mat.wid, job-mat.dep,
                                   ap-invl.qty, output v-qty).

          /*display vend.name             at 51   format "x(15)"
                                        when avail vend
                                         and first-of(xreport.key-02)
                  ap-inv.inv-no         at 67
                  v-qty                 to 87   format ">>>>>>>9"

              with frame det.*/

          IF rd-dest EQ 3 THEN
             PUT STREAM excel UNFORMATTED
                 '"' IF first-of(xreport.key-02) AND
                        AVAIL vend THEN vend.NAME   
                        ELSE ""                      '",'
                 '"' ap-inv.inv-no                   '",'
                 '"' ( IF last(xreport.key-02) AND
                          not first(xreport.key-02) THEN
                          STRING(v-inv-qty,">>>>>>>9")
                       ELSE 
                          STRING(v-qty,">>>>>>>9")) '",'.

          v-inv-qty = v-inv-qty + v-qty.
        end.

        if last(xreport.key-02) then do:
          if not first(xreport.key-02) then do:
            /*down with frame det.
            display v-inv-qty @ v-qty with frame det.*/
          end.

          /*display v-mat-qty                     to 96
                  v-fg-qty                      to 105  format ">>>>>>>9"
                  v-mat-qty - (v-fg-qty / v-on) to 114  format "->>>>>>9"
                  v-cost                        to 125  format ">>>>>>9.99"
                  v-cost / (v-fg-qty / v-on)    to 131  format ">9.99"
                                                when v-fg-qty gt 0
                                                 and v-cost gt 0
              with frame det.*/

          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = /*string(job-hdr.cust-no,"x(8)")*/ "" .
                         WHEN "cust-name"   THEN cVarValue = /*IF AVAIL cust THEN string(cust.NAME,"x(30)") ELSE*/ "".
                         WHEN "job"   THEN cVarValue = /*STRING(job-hdr.job-no,"x(9)")*/ "" .
                         WHEN "fgitem"  THEN cVarValue = /*STRING(job-hdr.i-no,"x(15)")*/ "" .
                         WHEN "qty"   THEN cVarValue = /*STRING(job-hdr.qty,">>>>>>>9")*/ "" .
                         WHEN "price"  THEN cVarValue = /*IF AVAIL oe-ordl THEN STRING(oe-ordl.price,">>>>>>9.99<<<<") ELSE*/ "".
                         WHEN "uom"   THEN cVarValue = /*IF AVAIL oe-ordl THEN STRING(oe-ordl.pr-uom,"x(3)") ELSE*/ "" .
                         WHEN "vend-name"  THEN cVarValue = STRING(vend.name) .
                         WHEN "inv"    THEN cVarValue = IF lAPInvoiceLength THEN STRING(ap-inv.inv-no,"x(20)") ELSE STRING(ap-inv.inv-no,"x(12)").
                         WHEN "inv-qty"   THEN cVarValue = STRING(v-inv-qty,"->>>>>>>>9").
                         WHEN "rec-qty"   THEN cVarValue = STRING(v-mat-qty,"->>>>>>>>9") .
                         WHEN "qty-prod"  THEN cVarValue = STRING(v-fg-qty,">>>>>>>9") .
                         WHEN "diff"   THEN cVarValue = string(v-mat-qty - (v-fg-qty / v-on),">>>>>>") .
                         WHEN "vend-setup"  THEN cVarValue = STRING(v-cost,">>>>>>9.99") .
                         WHEN "per-sheet"   THEN cVarValue = STRING(v-cost / (v-fg-qty / v-on),">>9.9")  .


                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest EQ 3 THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

          IF rd-dest EQ 3 THEN
             PUT STREAM excel UNFORMATTED
                 '"' STRING(v-mat-qty,">>>>>>>9")                     '",'
                 '"' STRING(v-fg-qty,">>>>>>>9")                      '",'
                 '"' STRING(v-mat-qty - (v-fg-qty / v-on),"->>>>>>9") '",'
                 '"' STRING(v-cost,">>>>>>9.99")                      '",'
                 '"' IF v-fg-qty GT 0 AND v-cost GT 0 THEN
                        STRING(v-cost / (v-fg-qty / v-on),">9.99")
                     ELSE ""                                          '",'.
        end.

        down with frame det.

        delete xreport.
      end.


      if last-of(report.key-02) then
      DO:
         put skip(1).

         IF rd-dest EQ 3 THEN
            PUT STREAM excel UNFORMATTED SKIP(1).
      END.

      delete report.
    end.

IF rd-dest EQ 3 THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_OpenCSV THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
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
       fi_file:SENSITIVE = YES
       tb_OpenCSV:SENSITIVE = YES      
      .
     ELSE
       ASSIGN
       tb_OpenCSV:SCREEN-VALUE = "NO"
       fi_file:SENSITIVE = NO
       tb_OpenCSV:SENSITIVE = NO      
      .
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
  RETURN string(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

