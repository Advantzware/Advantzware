&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: jcrep\r-wbyjob.w

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
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

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

 def temp-table work-mch NO-UNDO
   field job like job.job
   field job-no like job.job-no
   field job-no2 like job.job-no2
   field frm like job-mch.frm
   field blank-no like job-mch.blank-no
   field m-code like job-mch.m-code
   field dept like job-mch.dept
   field pass like job-mch.pass
   field run-qty as dec format ">>>>>>>>>>9"
   field wst-qty as dec
   field mr-qty as dec
   field mr-waste as dec.

def temp-table work-job NO-UNDO
   field job     like job.job
   field job-no  like job.job-no
   field job-no2 like job.job-no2.

DEF STREAM excel.


DEFINE VARIABLE ldummy              AS LOGICAL NO-UNDO.
DEFINE VARIABLE cTextListToSelect   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType          AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength       AS INTEGER NO-UNDO.
DEFINE VARIABLE cTextListToDefault  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName           AS CHARACTER NO-UNDO.


ASSIGN cTextListToSelect =  "JOB #,STATUS,F,B,MACH CODE,DESCRIPTION,RUN QUANTITY,WASTE QUANTITY,"
                                            + "MR STD PERCENT,MR ACT PERCENT,MR VARIANCE,RUN STD PERCENT,"
                                            + "RUN ACT PERCENT,RUN VARIANCE,OVER STD PERCENT,OVER ACT PERCENT,"
                                            + "OVER VARIANCE"
       cFieldListToSelect = "job,stat,form,blank,mach,desc,run-qty,waste-qty," +
                                        "mr-std-per,mr-act-per,mr-var,run-std-per,"  +
                                        "run-act-per,run-ver,over-std-per,over-act-per,"     +
                                        "over-ver"
       cFieldLength = "13,6,2,2,9,30,11,9," + "9,9,9,9," + "9,9,9,9,9"
       cFieldType = "c,c,i,i,c,c,i,i," + "i,i,i,i," + "i,i,i,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "JOB #,STATUS,F,B,MACH CODE,DESCRIPTION,RUN QUANTITY,WASTE QUANTITY,"
                                            + "MR STD PERCENT,MR ACT PERCENT,MR VARIANCE,RUN STD PERCENT,"
                                            + "RUN ACT PERCENT,RUN VARIANCE,OVER STD PERCENT,OVER ACT PERCENT,"
                                            + "OVER VARIANCE" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_job-no begin_job-no2 ~
end_job-no end_job-no2 begin_date end_date begin_dept end_dept begin_mach ~
end_mach rd_qty sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up ~
btn_down rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_job-no begin_job-no2 end_job-no ~
end_job-no2 begin_date end_date begin_dept end_dept begin_mach end_mach ~
lbl_qty rd_qty sl_avail sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose  

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
     SIZE 16 BY 1.

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
     SIZE 20.4 BY .95 NO-UNDO.

DEFINE VARIABLE begin_dept AS CHARACTER FORMAT "X(4)" 
     LABEL "Beginning Department" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(9)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-999":U INITIAL "000" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_dept AS CHARACTER FORMAT "X(4)" INITIAL "zzzz" 
     LABEL "Ending Department" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(9)":U INITIAL "zzzzzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-999":U INITIAL "999" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-wbyjob.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 43 BY 1.

DEFINE VARIABLE lbl_qty AS CHARACTER FORMAT "X(256)":U INITIAL "Show Quantity in?" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .95 NO-UNDO.

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
     SIZE 18 BY 4.76 NO-UNDO.

DEFINE VARIABLE rd_qty AS CHARACTER INITIAL "Sheets" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Sheets", "Sheets",
"Blanks", "Blanks"
     SIZE 25 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 6.19.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 7.57.

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

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_job-no AT ROW 2.95 COL 27.4 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 2.95 COL 42.3 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 2.95 COL 69.8 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 2.95 COL 84.7 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     begin_date AT ROW 3.91 COL 27.4 COLON-ALIGNED
     end_date AT ROW 3.91 COL 69.8 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     begin_dept AT ROW 4.86 COL 27.4 COLON-ALIGNED HELP
          "Enter Beginning Department"
     end_dept AT ROW 4.86 COL 69.8 COLON-ALIGNED HELP
          "Enter Ending Department"
     begin_mach AT ROW 5.81 COL 27.4 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     end_mach AT ROW 5.81 COL 69.8 COLON-ALIGNED HELP
          "Enter Ending Machine"
     lbl_qty AT ROW 7.52 COL 20 COLON-ALIGNED NO-LABEL
     rd_qty AT ROW 7.52 COL 43 NO-LABEL
     sl_avail AT ROW 10.14 COL 4 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 10.14 COL 40.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 10.14 COL 60.8 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 11.14 COL 40.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 12.14 COL 40.4 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 13.19 COL 40.4 WIDGET-ID 40
     btn_down AT ROW 14.19 COL 40.4 WIDGET-ID 42
     lv-ornt AT ROW 16.24 COL 31 NO-LABEL
     lines-per-page AT ROW 16.24 COL 84 COLON-ALIGNED
     rd-dest AT ROW 16.71 COL 6 NO-LABEL
     lv-font-no AT ROW 17.67 COL 34 COLON-ALIGNED
     td-show-parm AT ROW 17.67 COL 50
     lv-font-name AT ROW 18.62 COL 28 COLON-ALIGNED NO-LABEL
     fi_file AT ROW 20.38 COL 28.8 COLON-ALIGNED HELP
          "Enter File Name"
     tb_OpenCSV AT ROW 20.43 COL 90 RIGHT-ALIGNED
     tbAutoClose AT ROW 22.52 COL 31.2 WIDGET-ID 16
     btn-ok AT ROW 23.86 COL 31
     btn-cancel AT ROW 23.86 COL 51
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 9.43 COL 4.4 WIDGET-ID 38
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 15 
     " Output Destination" VIEW-AS TEXT
          SIZE 18.8 BY .62 AT ROW 15.67 COL 5
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 9.43 COL 60.2 WIDGET-ID 44
     RECT-6 AT ROW 16 COL 4
     RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96 BY 24.91
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
         TITLE              = "Waste Analysis By Job Report"
         HEIGHT             = 24.95
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
       begin_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       end_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_qty IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_qty:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_qty".

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
       rd_qty:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Waste Analysis By Job Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Waste Analysis By Job Report */
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
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_dept C-Win
ON LEAVE OF begin_dept IN FRAME FRAME-A /* Beginning Department */
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


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine */
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
    ASSIGN fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
    RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
    fi_file:SCREEN-VALUE =  cFileName.
  END. 
  
  RUN GetSelectionList.    
  RUN run-report. 

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
                     OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                  END.
              END.
              ELSE DO:
                  OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
              END.
           END. /* WHEN 3 THEN DO: */
       WHEN 5 THEN
       DO:
          DEF VAR lv-tmp AS CHAR INIT "-0" NO-UNDO.

          {custom/asimailr.i &TYPE="Customer"
                             &begin_cust=lv-tmp
                             &END_cust=lv-tmp
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
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_dept C-Win
ON LEAVE OF end_dept IN FRAME FRAME-A /* Ending Department */
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


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine */
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


&Scoped-define SELF-NAME rd_qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_qty C-Win
ON VALUE-CHANGED OF rd_qty IN FRAME FRAME-A
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
   begin_date = DATE (01,01,YEAR(TODAY))
   END_date   = TODAY.
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
  {sys/inc/reportsConfigNK1.i "JR4" }
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
  DISPLAY begin_job-no begin_job-no2 end_job-no end_job-no2 begin_date end_date 
          begin_dept end_dept begin_mach end_mach lbl_qty rd_qty sl_avail 
          sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_job-no begin_job-no2 end_job-no end_job-no2 
         begin_date end_date begin_dept end_dept begin_mach end_mach rd_qty 
         sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down 
         rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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
/* ------------------------------------------------ jc/rep/wip-aud.p  8/94 gb */
/* WIP Job Audit Listing Report                                               */
/* ---------------------------------------------------------------------------*/

/*{sys/form/r-topw.f}*/

def var v-job as char format "x(9)" extent 2 init ["","zzzzzzzzz"] no-undo.
def var v-job2 as int format "999" extent 2 init [000,999] no-undo.
def var v-date as date extent 2 format "99/99/9999" no-undo.
def var v-mach like mch-act.m-code extent 2 init ["","zzzzzz"] no-undo.
def var v-dept like mch-act.dept extent 2 init ["","zz"] no-undo.
def var v-shts as log format "Sheets/Blanks" init yes no-undo.

def var v-up-hs     like eb.num-up.
def var v-up        like eb.num-up.
def var v-out       like est-op.n-out.
def var v-on        like eb.num-up.
def var v-run-qty   as   dec.
def var v-wst-qty   as   dec.
def var v-mr-qty    as   dec.
def var v-mr-waste  as   dec.

def var mr-std-pct as dec format ">>>9.99" no-undo.
def var mr-act-pct as dec format ">>>9.99" no-undo.
def var mr-var-pct as dec format "->>>9.99" no-undo.
def var run-std-pct as dec format ">>>9.99" no-undo.
def var run-act-pct as dec format ">>>9.99" no-undo.
def var run-var-pct as dec format "->>>9.99" no-undo.
def var ovr-std-pct as dec format ">>>9.99" no-undo.
def var ovr-act-pct as dec format ">>>9.99" no-undo.
def var ovr-var-pct as dec format "->>>9.99" no-undo.
def var j-mr-std-pct as dec format ">>>9.99" no-undo.
def var j-mr-act-pct as dec format ">>>9.99" no-undo.
def var j-mr-var-pct as dec format "->>>9.99" no-undo.
def var j-run-std-pct as dec format ">>>9.99" no-undo.
def var j-run-act-pct as dec format ">>>9.99" no-undo.
def var j-run-var-pct as dec format "->>>9.99" no-undo.
def var j-ovr-std-pct as dec format ">>>9.99" no-undo.
def var j-ovr-act-pct as dec format ">>>9.99" no-undo.
def var j-ovr-var-pct as dec format "->>>9.99" no-undo.
def var v-dscr like dept.dscr no-undo.

def var hdr-tit as char no-undo.
def var hdr-tit2 as char no-undo.
def var hdr-tit3 as char no-undo.
DEF VAR viLoop AS INT NO-UNDO.

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
DEF VAR str-line AS cha FORM "x(200)" NO-UNDO.

{sys/form/r-top5L3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

/*FORM HEADER
    /* hdr-tit format "x(132)" skip
     hdr-tit2 format "x(132)" skip
     hdr-tit3 format "x(132)"*/
   SKIP(1) str-tit3 SKIP 
           str-tit4 SKIP 
           str-tit5 SKIP 
    WITH FRAME r-top.*/

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

  v-date[1]  = begin_date
  v-date[2]  = end_date
  v-dept[1]  = begin_dept
  v-dept[2]  = end_dept
  v-mach[1]  = begin_mach
  v-mach[2]  = end_mach
  v-shts     = rd_qty BEGINS "Sheets"

  v-job[1]   = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', begin_job-no, begin_job-no2))                   
  v-job[2]   = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', end_job-no, end_job-no2)) . 

 assign hdr-tit = "       " +
                 "MACH                                RUN    WASTE  MR STD " +
                 " MR ACT       MR RUN STD RUN ACT      RUN OVER STD OVER ACT"
                 + "     OVER"
      hdr-tit2 = "  F/ B " +
                 "CODE   DESCRIPTION             QUANTITY QUANTITY PERCENT " +
                 "PERCENT VARIANCE PERCENT PERCENT VARIANCE  PERCENT  PERCENT"
                 + " VARIANCE"
      hdr-tit3 = fill("-", 132).


DEF VAR cslist AS cha NO-UNDO.
 FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

   /*IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
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
          cSlist = cSlist + ttRptSelected.FieldList + ",".*/
     IF ttRptSelected.TextList = "RUN QUANTITY"  THEN
         ASSIGN
         str-tit3  = str-tit3 + "        RUN" + " "
         str-tit4 = str-tit4 +  "   QUANTITY" + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "WASTE QUANTITY"  THEN ASSIGN
         str-tit3  = str-tit3 + "    WASTE" + " "
         str-tit4 = str-tit4 +  " QUANTITY" + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "MR STD PERCENT"  THEN ASSIGN
         str-tit3  = str-tit3 + "   MR STD" + " "
         str-tit4 = str-tit4 +  "  PERCENT" + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "MR ACT PERCENT"  THEN ASSIGN
         str-tit3  = str-tit3 + "   MR ACT" + " "
         str-tit4 = str-tit4 +  "  PERCENT" + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "MR VARIANCE"  THEN ASSIGN
         str-tit3  = str-tit3 + "      MR " + " "
         str-tit4 = str-tit4 +  " VARIANCE" + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "RUN STD PERCENT"  THEN ASSIGN
         str-tit3  = str-tit3 + "  RUN STD" + " "
         str-tit4 = str-tit4 +  "  PERCENT" + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "RUN ACT PERCENT"  THEN ASSIGN
         str-tit3  = str-tit3 + "  RUN ACT" + " "
         str-tit4 = str-tit4 +  "  PERCENT" + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "RUN VARIANCE"  THEN ASSIGN
         str-tit3  = str-tit3 + "      RUN" + " "
         str-tit4 = str-tit4 +  " VARIANCE" + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "OVER STD PERCENT"  THEN ASSIGN
         str-tit3  = str-tit3 + " OVER STD" + " "
         str-tit4 = str-tit4 +  "  PERCENT" + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "OVER ACT PERCENT"  THEN ASSIGN
         str-tit3  = str-tit3 + " OVER ACT" + " "
         str-tit4 = str-tit4 +  "  PERCENT" + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

     ELSE IF  ttRptSelected.TextList = "OVER VARIANCE"  THEN ASSIGN
         str-tit3  = str-tit3 + "     OVER" + " "
         str-tit4 = str-tit4 +  " VARIANCE" + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + ","  .

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

        IF LOOKUP(ttRptSelected.TextList, "MR STD PERCENT,MR ACT PERCENT,MR VARIANCE,RUN STD PERCENT,RUN ACT PERCENT,RUN VARIANCE,OVER STD PERCENT,OVER ACT PERCENT,OVER VARIANCE") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

IF rd-dest EQ 3 THEN DO:
  OUTPUT STREAM excel TO VALUE(cFileName).
  /*excelheader = "JOB #,STATUS,F,B,MACH CODE,DESCRIPTION,RUN QUANTITY,WASTE QUANTITY,"
              + "MR STD PERCENT,MR ACT PERCENT,MR VARIANCE,RUN STD PERCENT,"
              + "RUN ACT PERCENT,RUN VARIANCE,OVER STD PERCENT,OVER ACT PERCENT,"
              + "OVER VARIANCE".*/
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

SESSION:SET-WAIT-STATE ("general").

EMPTY TEMP-TABLE work-mch.
EMPTY TEMP-TABLE work-job.

for each mch-act
      where mch-act.company                                 eq cocode
        and mch-act.op-date                                 ge v-date[1]
        and mch-act.op-date                                 le v-date[2]
        and FILL(" ", iJobLen - length(trim(mch-act.job-no))) +
            trim(mch-act.job-no) + string(mch-act.job-no2,"999") ge v-job[1]
        and FILL(" ", iJobLen - length(trim(mch-act.job-no))) +
            trim(mch-act.job-no) + string(mch-act.job-no2,"999") le v-job[2]
        AND mch-act.job-no2                                 GE int(begin_job-no2)
        AND mch-act.job-no2                                 LE int(end_job-no2)    
        and mch-act.dept                                    ge v-dept[1]
        and mch-act.dept                                    le v-dept[2]
        and mch-act.m-code                                  ge v-mach[1]
        and mch-act.m-code                                  le v-mach[2]
      use-index dte-idx no-lock,

      first mach
      where mach.company eq cocode
        and mach.m-code  eq mch-act.m-code
      no-lock,

      first job
      where job.company eq cocode
        and job.job     eq mch-act.job
        and job.job-no  eq mch-act.job-no
        and job.job-no2 eq mch-act.job-no2
        and job.stat    ne "P"
      no-lock:

    find first work-mch
        where work-mch.job      eq mch-act.job
          and work-mch.frm      eq mch-act.frm
          and work-mch.blank-no eq mch-act.blank-no
          and work-mch.m-code   eq mch-act.m-code
          and work-mch.pass     eq mch-act.pass
        no-error.
    if not avail work-mch then do:
      create work-mch.
      assign
       work-mch.job      = mch-act.job
       work-mch.job-no   = mch-act.job-no
       work-mch.job-no2  = mch-act.job-no2
       work-mch.frm      = mch-act.frm
       work-mch.blank-no = mch-act.blank-no
       work-mch.m-code   = mch-act.m-code
       work-mch.dept     = mch-act.dept
       work-mch.pass     = mch-act.pass.
    end.

    assign
     v-run-qty  = 0
     v-wst-qty  = 0
     v-mr-qty   = 0
     v-mr-waste = 0.

    find job-code where job-code.code eq mch-act.code no-lock.
    if job-code.cat eq "RUN" then
      assign
       v-run-qty  = mch-act.qty
       v-wst-qty  = mch-act.waste.
    else
    if job-code.cat eq "MR" then
      assign
       v-mr-qty   = mch-act.qty
       v-mr-waste = mch-act.waste.

    assign
     v-up  = 1
     v-out = 1
     v-on  = 1.

    find est where est.company EQ job.company
               AND est.est-no  EQ job.est-no
             no-lock no-error.

    if avail est and INDEX("AP",mach.p-type) LE 0 then do:
      run sys/inc/numup.p (est.company, est.est-no, mch-act.frm, output v-up).

      find first ef
          where ef.company   EQ est.company
            AND ef.est-no    EQ est.est-no
            and ef.form-no   EQ mch-act.frm
          no-lock no-error.

      IF AVAIL ef THEN DO:
        RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).
        v-on = v-up * v-on.
      END.

      find first est-op
          where est-op.company eq est.company
            AND est-op.est-no  EQ est.est-no
            and est-op.s-num   eq mch-act.frm
            and est-op.b-num   eq mch-act.blank-no
            and est-op.m-code  eq mch-act.m-code
            and est-op.op-pass eq mch-act.pass
            and est-op.dept    eq mch-act.dept
            and est-op.line    lt 500
          no-lock no-error.

      if ((avail est-op) and est-op.op-sb)           or
         ((not avail est-op) and mach.p-type ne "B") then do:

        if avail est-op then
          run sys/inc/numout.p (recid(est-op), output v-out).

        else v-out = 1.

        v-up = v-up * v-out.
      end.

      else v-up = 1.

      v-on = v-on / v-up.
    end.

    if v-shts then
      assign
       v-run-qty  = v-run-qty  / v-on
       v-wst-qty  = v-wst-qty  / v-on
       v-mr-qty   = v-mr-qty   / v-on
       v-mr-waste = v-mr-waste / v-on.
    else
    if mach.p-type ne "B" then
      assign
       v-on       = v-on       * v-up
       v-run-qty  = v-run-qty  * v-on
       v-wst-qty  = v-wst-qty  * v-on
       v-mr-qty   = v-mr-qty   * v-on
       v-mr-waste = v-mr-waste * v-on.

    assign
     work-mch.run-qty  = work-mch.run-qty  + v-run-qty
     work-mch.wst-qty  = work-mch.wst-qty  + v-wst-qty
     work-mch.mr-qty   = work-mch.mr-qty   + v-mr-qty
     work-mch.mr-waste = work-mch.mr-waste + v-mr-waste.

    find first work-job
        where work-job.job-no  eq mch-act.job-no
          and work-job.job-no2 eq mch-act.job-no2
        no-error.
    if not avail work-job then do:
      create work-job.
      assign
       work-job.job     = mch-act.job
       work-job.job-no  = mch-act.job-no
       work-job.job-no2 = mch-act.job-no2.
    end.
  end.

  {sys/inc/print1.i}

 {sys/inc/outprint.i value(lines-per-page)}

 if td-show-parm then run show-param.

 display "" with frame r-top.

 for each work-job,
     first job
     where job.company  eq cocode

      and job.job      eq work-job.job
       and job.job-no   eq work-job.job-no
       and job.job-no2  eq work-job.job-no2
     no-lock

      break by work-job.job-no
            by work-job.job-no2:

    /*put "Job Number: " job.job-no "-" job.job-no2
        "   Status: " job.stat skip(1).*/

    for each job-mch
        where job-mch.company eq cocode        
          and job-mch.job     eq job.job 
        use-index seq-idx no-lock,

        first work-mch
        where work-mch.job      eq job.job
          and work-mch.frm      eq job-mch.frm
          and work-mch.blank-no eq job-mch.blank-no
          and work-mch.m-code   eq job-mch.m-code
        no-lock,

        first mach
        where mach.company eq cocode
          and mach.m-code  eq job-mch.m-code
        no-lock:

      assign
       mr-std-pct  = (job-mch.mr-waste / job-mch.run-qty) * 100.00
       run-std-pct = ((job-mch.run-qty * (job-mch.wst-prct * .01)) /
                      job-mch.run-qty) * 100.00
       mr-act-pct  = ((work-mch.mr-qty + work-mch.mr-waste) /
                      work-mch.run-qty) * 100.00
       run-act-pct = (work-mch.wst-qty / work-mch.run-qty) * 100.00.

      if mr-act-pct  eq ? then mr-act-pct  = 0.
      if mr-std-pct  eq ? then mr-std-pct  = 0.
      if run-act-pct eq ? then run-act-pct = 0.
      if run-std-pct eq ? then run-std-pct = 0.

      assign
       mr-var-pct  = mr-std-pct  - mr-act-pct
       run-var-pct = run-std-pct - run-act-pct
       ovr-std-pct = run-std-pct + mr-std-pct
       ovr-act-pct = run-act-pct + mr-act-pct
       ovr-var-pct = ovr-std-pct - ovr-act-pct.

      /*display job-mch.frm space(0) "-" space(0)
              job-mch.blank-no
              job-mch.m-code
              mach.m-dscr
              work-mch.run-qty when avail work-mch
              (work-mch.mr-waste + work-mch.mr-qty + work-mch.wst-qty)
                                        format ">>>>>>>9" when avail work-mch
              mr-std-pct
              mr-act-pct
              mr-var-pct
              run-std-pct
              run-act-pct
              run-var-pct
              space(2)
              ovr-std-pct
              space(2)
              ovr-act-pct
              ovr-var-pct

          with frame det STREAM-IO width 132 no-box down no-labels.

      down with frame det.*/

      ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".


            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "job"    THEN cVarValue = string(job.job-no + "-" + string(job.job-no2,"999"),"x(13)") .
                         WHEN "stat"   THEN cVarValue = string(job.stat).
                         WHEN "form"   THEN cVarValue = STRING(job-mch.frm,">>").
                         WHEN "blank"  THEN cVarValue = STRING(job-mch.blank-no,">>") .
                         WHEN "mach"   THEN cVarValue = STRING(job-mch.m-code) .
                         WHEN "desc"  THEN cVarValue = STRING(mach.m-dscr,"x(30)") .
                         WHEN "run-qty"   THEN cVarValue = IF AVAIL work-mch THEN STRING(work-mch.run-qty,"->>>>>>>>>9") ELSE "".
                         WHEN "waste-qty"  THEN cVarValue = IF AVAIL work-mch THEN STRING(work-mch.mr-waste + work-mch.mr-qty + work-mch.wst-qty,"->>>>>>>9") ELSE "" .

                         WHEN "mr-std-per"    THEN cVarValue = STRING(mr-std-pct,"->>>>9.99") .  
                         WHEN "mr-act-per"    THEN cVarValue = STRING(mr-act-pct,"->>>>9.99") . 
                         WHEN "mr-var"        THEN cVarValue = STRING(mr-var-pct,"->>>>9.99")  .
                         WHEN "run-std-per"   THEN cVarValue = STRING(run-std-pct,"->>>>9.99") .
                         WHEN "run-act-per"   THEN cVarValue = STRING(run-act-pct,"->>>>9.99") .
                         WHEN "run-ver"       THEN cVarValue = STRING(run-var-pct,"->>>>9.99") .
                         WHEN "over-std-per"  THEN cVarValue = STRING(ovr-std-pct,"->>>>9.99") .
                         WHEN "over-act-per"  THEN cVarValue = STRING(ovr-act-pct,"->>>>9.99") .
                         WHEN "over-ver"      THEN cVarValue = STRING(ovr-var-pct,"->>>>9.99") .

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

      assign
       j-mr-std-pct = j-mr-std-pct + mr-std-pct.
       j-mr-act-pct = j-mr-act-pct + mr-act-pct.
       j-mr-var-pct = j-mr-var-pct + mr-var-pct.
       j-run-std-pct = j-run-std-pct + run-std-pct.
       j-run-act-pct = j-run-act-pct + run-act-pct.
       j-run-var-pct = j-run-var-pct + run-var-pct.
       j-ovr-std-pct = j-ovr-std-pct + ovr-std-pct.
       j-ovr-act-pct = j-ovr-act-pct + ovr-act-pct.
       j-ovr-var-pct = j-ovr-var-pct + ovr-var-pct.
    end.


    PUT str-line SKIP .

       /* j-mr-std-pct at 57 " "
        j-mr-act-pct " "
        j-mr-var-pct " "
        j-run-std-pct " "
        j-run-act-pct " "
        j-run-var-pct "  "
        j-ovr-std-pct "  "
        j-ovr-act-pct " "
        j-ovr-var-pct skip
        fill("-", 132) format "x(132)" skip(2).*/

     ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".


            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "job"    THEN cVarValue = string("") .
                         WHEN "stat"   THEN cVarValue = "".
                         WHEN "form"   THEN cVarValue = "".
                         WHEN "blank"  THEN cVarValue = "" .
                         WHEN "mach"   THEN cVarValue = "" .
                         WHEN "desc"  THEN cVarValue = "" .
                         WHEN "run-qty"   THEN cVarValue =  "".
                         WHEN "waste-qty"  THEN cVarValue = "" .

                         WHEN "mr-std-per"    THEN cVarValue = STRING(j-mr-std-pct,"->>>>9.99") .  
                         WHEN "mr-act-per"    THEN cVarValue = STRING(j-mr-act-pct,"->>>>9.99") . 
                         WHEN "mr-var"        THEN cVarValue = STRING(j-mr-var-pct,"->>>>9.99")  .
                         WHEN "run-std-per"   THEN cVarValue = STRING(j-run-std-pct,"->>>>9.99") .
                         WHEN "run-act-per"   THEN cVarValue = STRING(j-run-act-pct,"->>>>9.99") .
                         WHEN "run-ver"       THEN cVarValue = STRING(j-run-var-pct,"->>>>9.99") .
                         WHEN "over-std-per"  THEN cVarValue = STRING(j-ovr-std-pct,"->>>>9.99") .
                         WHEN "over-act-per"  THEN cVarValue = STRING(j-ovr-act-pct,"->>>>9.99") .
                         WHEN "over-ver"      THEN cVarValue = STRING(j-ovr-var-pct,"->>>>9.99") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED "               Job Totals" + SUBSTRING(cDisplay,26,250) SKIP(2).
            IF rd-dest EQ 3 THEN DO:
                 PUT STREAM excel UNFORMATTED  'Job Totals,'
                       substring(cExcelDisplay,4,250) SKIP(2).
             END.



    assign
     j-mr-std-pct  = 0
     j-mr-act-pct  = 0
     j-mr-var-pct  = 0
     j-run-std-pct = 0
     j-run-act-pct = 0
     j-run-var-pct = 0
     j-ovr-std-pct = 0
     j-ovr-act-pct = 0
     j-ovr-var-pct = 0.
  end. /* each item */

IF rd-dest EQ 3 THEN DO:
  OUTPUT STREAM excel CLOSE.
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
    ASSIGN fi_file:SCREEN-VALUE = "c:\tmp\r-wbyjob.csv".    
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

