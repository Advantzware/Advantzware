&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-post.w

  Description: Touch Screen Posting

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Yoosun Kim 

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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

/*{sys/inc/var.i new shared} */
def new shared var cocode as cha no-undo.
def new shared var locode as cha no-undo.

assign
 cocode = gcompany
 locode = gloc.

DEF VAR v-invalid AS LOG NO-UNDO.
def var v-download as log init no no-undo.
def var v-prior as log init no no-undo.

def buffer tmp-per for period.

def stream st-mach.
def stream st-emp.


DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

&Scoped-define SCOPDEFS post
&SCOPED-DEFINE where-statement machtran.company = g_company


DEFINE TEMP-TABLE ttbl_pc-prdd NO-UNDO LIKE pc-prdd
       INDEX ttbl_pc-prdd IS PRIMARY
             company m-code op-date shift job frm blank-no.
DEFINE TEMP-TABLE ttbl_pc-prdh NO-UNDO LIKE pc-prdh
       INDEX ttbl_pc-prdh IS PRIMARY
             company m-code trans-date shift.
DEFINE TEMP-TABLE ttbl_rowid NO-UNDO
  FIELD pc-prdd_rowid AS ROWID
  FIELD total_time AS INTEGER.
DEFINE VARIABLE machtotaltime AS DECIMAL NO-UNDO.
DEFINE VARIABLE shiftpct AS DECIMAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE waste-qty AS DECIMAL NO-UNDO.
DEFINE VARIABLE run-qty AS DECIMAL NO-UNDO.
DEFINE VARIABLE selected-company AS CHARACTER FORMAT "X(3)" LABEL "Company" NO-UNDO.

DEF VAR lv-valid-to-post AS LOG NO-UNDO.

/* mods for auto post to job cost */
def var v-auto-bin  like sys-ctrl.char-fld no-undo.
def var v-rm-fg     as   log NO-UNDO.
DEF TEMP-TABLE tt-report LIKE report.

def TEMP-TABLE w-job field job like job.job.

DEFINE STREAM excel.

{pc/pcprdd4u.i NEW}

{jc/jcgl-sh.i NEW}

DO TRANSACTION:
  {sys/inc/dcpostgl.i}
  {sys/inc/tspost.i}
  {sys/inc/tspostfg.i}
  {sys/inc/tssecure.i}  
END.

DEF VAR v-autopost AS LOG NO-UNDO.
FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                        AND sys-ctrl.name    EQ "AUTOPOST" NO-LOCK NO-ERROR.
v-autopost = IF AVAIL sys-ctrl THEN sys-ctrl.log-fld ELSE NO.

DEFINE BUFFER b-MachTran FOR MachTran.
DEFINE BUFFER b-MachEmp  FOR MachEmp.
DEFINE BUFFER b-EmpLogin FOR EmpLogin.

DEFINE VARIABLE logDelMachTran AS LOGICAL    NO-UNDO.

DEF VAR begin_job_number AS cha NO-UNDO.
DEF VAR END_job_number AS cha NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-text-1 begin_machine lv-text-2 ~
end_machine fi_BeginEmp fi_EndEmp begin_date end_date begin_shift end_shift ~
t-purge t-prt-rate tg_MachTrans tg_EmpMachTrans tg_blank-end-date rd-dest ~
lv-ornt lines-per-page lv-font-no td-show-parm fi_file btn-ok btn-cancel ~
begin_job-no begin_job-no2 end_job-no end_job-no2 tg_MachReltdTrans ~
tb_excel tb_runExcel RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS lv-text-1 begin_machine lv-text-2 ~
end_machine fi_BeginEmp fi_EndEmp begin_date end_date FILL-IN-1 begin_shift ~
end_shift t-purge t-prt-rate tg_MachTrans tg_EmpMachTrans tg_blank-end-date ~
rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm fi_file ~
begin_job-no begin_job-no2 end_job-no end_job-no2 tg_MachReltdTrans ~
tb_excel tb_runExcel 

/* Custom List Definitions                                              */
/* nonPurgeItems,purgeItems,List-3,List-4,List-5,F1                     */
&Scoped-define nonPurgeItems FILL-IN-1 post t-prt-rate rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm RECT-6 
&Scoped-define purgeItems lv-text-1 lv-text-2 fi_BeginEmp fi_EndEmp t-purge ~
tg_MachTrans tg_EmpMachTrans tg_blank-end-date tg_MachReltdTrans 

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)" 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_machine AS CHARACTER FORMAT "X(10)" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_shift AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Beginning Shift" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_machine AS CHARACTER FORMAT "X(10)" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_shift AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Ending Shift" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Output Destination" 
      VIEW-AS TEXT 
     SIZE 19 BY .62 NO-UNDO.

DEFINE VARIABLE fi_BeginEmp AS CHARACTER FORMAT "X(20)":U 
     LABEL "Beginning Employee" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_EndEmp AS CHARACTER FORMAT "X(20)":U 
     LABEL "Ending Employee" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-postch.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1
     FGCOLOR 9 .

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

DEFINE VARIABLE lv-text-1 AS CHARACTER FORMAT "X(256)":U INITIAL "(Files Located in TF3 and TF4)" 
      VIEW-AS TEXT 
     SIZE 30 BY .62 NO-UNDO.

DEFINE VARIABLE lv-text-2 AS CHARACTER FORMAT "X(256)":U INITIAL "(Files Located in TF4)" 
      VIEW-AS TEXT 
     SIZE 21 BY .62 NO-UNDO.

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

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 12.62.

DEFINE VARIABLE post AS LOGICAL INITIAL no 
     LABEL "Post Data Collection Entries" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE t-prt-rate AS LOGICAL INITIAL no 
     LABEL "Print Labor Rates" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE t-purge AS LOGICAL INITIAL no 
     LABEL "Purge History" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg_blank-end-date AS LOGICAL INITIAL no 
     LABEL "Purge Blank Machine End Date" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .91 NO-UNDO.

DEFINE VARIABLE tg_EmpMachTrans AS LOGICAL INITIAL no 
     LABEL "Posted Employee Machine Transactions" 
     VIEW-AS TOGGLE-BOX
     SIZE 42.2 BY .81 NO-UNDO.

DEFINE VARIABLE tg_MachReltdTrans AS LOGICAL INITIAL no 
     LABEL "Unposted Machine & Related Employee Transactions T-F-3." 
     VIEW-AS TOGGLE-BOX
     SIZE 62.2 BY .81 NO-UNDO.

DEFINE VARIABLE tg_MachTrans AS LOGICAL INITIAL no 
     LABEL "Posted Machine and Related Emp. Trans." 
     VIEW-AS TOGGLE-BOX
     SIZE 44.2 BY .91 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     lv-text-1 AT ROW 9.81 COL 45 COLON-ALIGNED NO-LABEL
     begin_machine AT ROW 2.52 COL 22.8 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     lv-text-2 AT ROW 11.48 COL 45 COLON-ALIGNED NO-LABEL
     end_machine AT ROW 2.52 COL 65.6 COLON-ALIGNED HELP
          "Enter Ending Machine"
     begin_job-no AT ROW 3.57 COL 22.8 COLON-ALIGNED HELP
          "Enter Beginning Job Number" WIDGET-ID 4
     begin_job-no2 AT ROW 3.57 COL 37.8 COLON-ALIGNED HELP
          "Enter Beginning Job Number" WIDGET-ID 6
     end_job-no AT ROW 3.57 COL 65.6 COLON-ALIGNED HELP
          "Enter Ending Job Number" WIDGET-ID 8
     end_job-no2 AT ROW 3.57 COL 80.8 COLON-ALIGNED HELP
          "Enter Ending Job Number" WIDGET-ID 10
     fi_BeginEmp AT ROW 7.1 COL 22.8 COLON-ALIGNED
     fi_EndEmp AT ROW 7.1 COL 65.6 COLON-ALIGNED
     begin_date AT ROW 4.81 COL 22.8 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 4.81 COL 65.6 COLON-ALIGNED HELP
          "Enter Ending Date"
     FILL-IN-1 AT ROW 14.05 COL 1 COLON-ALIGNED NO-LABEL
     begin_shift AT ROW 5.91 COL 22.8 COLON-ALIGNED HELP
          "Enter Beginning Shift"
     end_shift AT ROW 5.91 COL 65.6 COLON-ALIGNED HELP
          "Enter Ending Shift"
     post AT ROW 8.91 COL 12 HELP
          "Post to ASI Database Indicator"
     t-purge AT ROW 9.86 COL 12
     t-prt-rate AT ROW 10.81 COL 12
     tg_MachTrans AT ROW 8.91 COL 43.8
     tg_EmpMachTrans AT ROW 10.62 COL 43.8
     tg_blank-end-date AT ROW 11.71 COL 12 WIDGET-ID 2
     rd-dest AT ROW 15 COL 6 NO-LABEL
     lv-ornt AT ROW 15 COL 29 NO-LABEL
     lines-per-page AT ROW 15 COL 82 COLON-ALIGNED
     lv-font-no AT ROW 16.67 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 17.62 COL 27.6 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.57 COL 52.8
     tb_excel AT ROW 19.57 COL 46.4
     tb_runExcel AT ROW 19.57 COL 89.2 RIGHT-ALIGNED
     fi_file AT ROW 20.62 COL 44.2 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 22.67 COL 19
     btn-cancel AT ROW 22.67 COL 57
     tg_MachReltdTrans AT ROW 12.71 COL 12 WIDGET-ID 12
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.48 COL 3
          BGCOLOR 2 
     RECT-6 AT ROW 13.38 COL 1.8
     RECT-7 AT ROW 1.24 COL 1.8
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.8 BY 23.38.


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
         TITLE              = "Touch Screen"
         HEIGHT             = 23.57
         WIDTH              = 95.4
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

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME Custom                                                    */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_shift:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       end_job-no2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_shift:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi_BeginEmp IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       fi_BeginEmp:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN fi_EndEmp IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       fi_EndEmp:PRIVATE-DATA IN FRAME FRAME-A     = 
                "save".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       lines-per-page:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
ASSIGN 
       lv-font-name:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       lv-font-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       lv-ornt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-text-1 IN FRAME FRAME-A
   2                                                                    */
/* SETTINGS FOR FILL-IN lv-text-2 IN FRAME FRAME-A
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX post IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
       post:HIDDEN IN FRAME FRAME-A           = TRUE
       post:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR RADIO-SET rd-dest IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       rd-dest:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME FRAME-A
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX t-prt-rate IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       t-prt-rate:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX t-purge IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       t-purge:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       td-show-parm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tg_blank-end-date IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       tg_blank-end-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tg_EmpMachTrans IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       tg_EmpMachTrans:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tg_MachReltdTrans IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       tg_MachReltdTrans:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tg_MachTrans IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       tg_MachTrans:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Touch Screen */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Touch Screen */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON HELP OF begin_date IN FRAME FRAME-A /* Beginning Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON ENTRY OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
DO:
  /*{&SELF-NAME}:FORMAT = 'XXXXXX-XX'.*/
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  ASSIGN {&displayed-objects}.
  ASSIGN
      begin_job-no = IF length(begin_job-no) < 6 THEN fill(" ",6 - length(trim(begin_job-no))) + trim(begin_job-no)
                     ELSE begin_job-no
      end_job-no = IF length(end_job-no) < 6 THEN fill(" ",6 - length(trim(end_job-no))) + trim(end_job-no)
                     ELSE end_job-no
      begin_job_number = /*fill(" ",6 - length(trim(begin_job-no))) +
                          trim(begin_job-no) + string(int(begin_job-no2),"99")*/
                         begin_job-no + "-" + string(int(begin_job-no2),"99")
      end_job_number  = /*fill(" ",6 - length(trim(end_job-no)))   +
                         trim(end_job-no)   + string(int(end_job-no2),"99")*/
                         end_job-no + "-" + string(int(end_job-no2),"99")
      .
  DEF BUFFER b-mach FOR mach.

  &IF DEFINED(tsPurge) NE 0 &THEN
  IF t-purge THEN DO:

      IF tg_MachTrans THEN DO:

         MESSAGE 
             "All machine transactions will be purged for" 
             "Machines - " begin_machine " to " end_machine SKIP 
             "Period - " begin_date " and " end_date "." SKIP 
             "Are you sure?" 
            view-as alert-box question button yes-no update ll-ans as log.

          IF ll-ans THEN DO:  
             DISABLE TRIGGERS FOR LOAD OF machtran.
             FOR EACH b-mach FIELDS(m-code) WHERE
                 b-mach.company = g_company AND
                 b-mach.m-code GE begin_machine AND
                 b-mach.m-code LE end_machine
                 NO-LOCK,
                 EACH b-machtran WHERE
                      b-machtran.company = g_company AND
                      b-machtran.posted = YES AND
                      b-machtran.machine EQ b-mach.m-code AND
                      ((begin_date LE b-machtran.start_date AND
                      b-machtran.start_date LE end_date AND
                      begin_date LE b-machtran.end_date AND
                      (b-machtran.end_date LE end_date) OR
                       (tg_blank-end-date AND b-machtran.END_date EQ ?)) OR
                      CAN-FIND(FIRST mach 
                               WHERE mach.company = b-machtran.company
                                AND mach.m-code = b-machtran.machine
                                AND mach.industry = "X"))
                      NO-LOCK:

                      logDelMachTran = TRUE.
                      FIND MachTran WHERE ROWID(MachTran) EQ ROWID(b-MachTran)
                                    EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                      IF AVAILABLE(MachTran) THEN
                         FOR EACH b-MachEmp 
                            WHERE b-MachEmp.table_rec_key EQ MachTran.rec_key
                             NO-LOCK:

                             FIND MachEmp WHERE ROWID(MachEmp) EQ ROWID(b-MachEmp)
                                          EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                             IF AVAILABLE(MachEmp) THEN
                               DELETE machemp.
                             ELSE
                               logDelMachTran = FALSE.
                         END. /* b-MachEmp */

                      IF logDelMachTran THEN
                        DELETE machtran.
             END. /* each machtran*/

             MESSAGE 
                 "Machine Transaction Purge is completed."
                VIEW-AS ALERT-BOX INFORMATION.

          END. /* if ll-ans*/
      END.  /* tg_MachTrans */

       IF tg_MachReltdTrans THEN DO:

         MESSAGE 
             "Unposted Employee Machine Transactions will be purged for" SKIP
             "Machines: " begin_machine " to " end_machine SKIP
             "Employees:" fi_BeginEmp " to " fi_EndEmp SKIP
             "Period: " begin_date " to " end_date "." SKIP
             "Are you sure?" 
            view-as alert-box question button yes-no update ll-ans3 as log.

          IF ll-ans3 THEN DO:  
             DISABLE TRIGGERS FOR LOAD OF machtran.
             FOR EACH b-mach FIELDS(m-code) WHERE
                 b-mach.company = g_company AND
                 b-mach.m-code GE begin_machine AND
                 b-mach.m-code LE end_machine
                 NO-LOCK,
                 EACH b-machtran WHERE
                      b-machtran.company = g_company AND
                      b-machtran.posted = NO AND
                      b-machtran.machine EQ b-mach.m-code AND
                      ((begin_date LE b-machtran.start_date AND
                      b-machtran.start_date LE end_date AND
                      begin_date LE b-machtran.end_date AND
                      (b-machtran.end_date LE end_date) OR
                       (tg_blank-end-date AND b-machtran.END_date EQ ?)) OR
                      CAN-FIND(FIRST mach 
                               WHERE mach.company = b-machtran.company
                                AND mach.m-code = b-machtran.machine
                                AND mach.industry = "X"))
                      NO-LOCK:

                      logDelMachTran = TRUE.
                      FIND MachTran WHERE ROWID(MachTran) EQ ROWID(b-MachTran)
                                    EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                      IF AVAILABLE(MachTran) THEN
                         FOR EACH b-MachEmp 
                            WHERE b-MachEmp.table_rec_key EQ MachTran.rec_key
                             NO-LOCK:

                             FIND MachEmp WHERE ROWID(MachEmp) EQ ROWID(b-MachEmp)
                                          EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                             IF AVAILABLE(MachEmp) THEN
                               DELETE machemp.
                             ELSE
                               logDelMachTran = FALSE.
                         END. /* b-MachEmp */

                      IF logDelMachTran THEN
                        DELETE machtran.
             END. /* each machtran*/

             MESSAGE 
                 "Machine Transaction Purge is completed."
                VIEW-AS ALERT-BOX INFORMATION.

          END. /* if ll-ans3*/
      END.  /* tg_MachReltdTrans */


      IF tg_EmpMachTrans THEN DO:

          MESSAGE 
             "Posted Employee Machine Transactions will be purged for" SKIP
             "Machines: " begin_machine " to " end_machine SKIP
             "Employees:" fi_BeginEmp " to " fi_EndEmp SKIP
             "Period: " begin_date " to " end_date "." SKIP
             "Are you sure?" 
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans2 AS LOG.

          IF ll-ans2 THEN DO:  
             FOR EACH employee fields(employee) WHERE
                 employee.company EQ g_company AND
                 employee.employee GE fi_BeginEmp AND
                 employee.employee LE fi_EndEmp
                 NO-LOCK,
                 EACH b-MachEmp WHERE
                      b-MachEmp.employee EQ employee.employee AND
                      begin_date LE b-MachEmp.start_date AND
                      b-MachEmp.start_date LE end_date AND
                      begin_date LE b-MachEmp.end_date AND
                      b-MachEmp.end_date LE end_date AND
                      b-MachEmp.posted EQ YES
                      NO-LOCK:

                      FIND FIRST MachTran WHERE
                           MachTran.rec_key EQ b-MachEmp.table_rec_key AND
                           machtran.machine GE begin_machine AND 
                           machtran.machine LE end_machine
                           NO-LOCK NO-ERROR.

                      IF AVAILABLE(MachTran) THEN DO:

                         FIND MachEmp WHERE
                              ROWID(MachEmp) EQ ROWID(b-MachEmp)
                              EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

                         IF AVAILABLE(MachEmp) THEN
                            DELETE machemp.
                      END.
             END. /* employee */

             MESSAGE 
                 "Posted Machine Transaction Purge is completed."
                VIEW-AS ALERT-BOX INFORMATION.
          END.
      END. /* tg_EmpMachTrans */

      RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

      RETURN.
  END. /* t-purge */
  &ELSE
  IF t-prt-rate AND lv-tssecure THEN DO:
     DEF VAR lv-passwd AS cha NO-UNDO.
     RUN custom/d-passwd.w (OUTPUT lv-passwd).
     IF lv-tssecure AND lv-tssecure-val <> lv-passwd THEN DO:
        MESSAGE "Security Error! Invalid Password. " VIEW-AS ALERT-BOX ERROR.
        t-prt-rate = NO.
        DISPLAY t-prt-rate.
     END.
  END.

 /* 
  lv-valid-to-post
  this flag is updated in run-report.
  it is updated in post_.i
  */

  lv-valid-to-post = NO. 

  RUN run-report.

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type="TS Posting"
                            &begin_cust=begin_machine
                            &END_cust= begin_machine
                            &fax-subject="TS Posting"
                            &fax-body="TS Posting"
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "TS Posting"
                             &begin_cust= begin_machine
                             &END_cust=begin_machine
                             &mail-subject="TS Posting"
                             &mail-body="TS Posting"
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "TS Posting"
                                  &begin_cust= begin_machine
                                  &END_cust=begin_machine
                                  &mail-subject=CURRENT-WINDOW:TITLE
                                  &mail-body=CURRENT-WINDOW:TITLE
                                  &mail-file=list-name }
     END.
       END.
       WHEN 6 THEN RUN OUTPUT-to-port.

  end case. 

  IF lv-valid-to-post THEN DO:
      MESSAGE 
          "Are you ready to Post?" 
        VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans4 AS LOG.

      IF ll-ans4 THEN DO:
         RUN do-post.
         MESSAGE "Post completed..." VIEW-AS ALERT-BOX.
      END.
  END.
  ELSE DO:
      MESSAGE "No Transactions Valid to Post."
          VIEW-AS ALERT-BOX WARNING BUTTONS OK.
  END.


  &ENDIF
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON HELP OF end_date IN FRAME FRAME-A /* Ending Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON ENTRY OF end_job-no IN FRAME FRAME-A /* Ending Job# */
DO:
  /*{&SELF-NAME}:FORMAT = 'XXXXXX-XX'.*/
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


&Scoped-define SELF-NAME tg_blank-end-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_blank-end-date C-Win
ON VALUE-CHANGED OF tg_blank-end-date IN FRAME FRAME-A /* Purge Blank Machine End Date */
DO:

IF tg_MachTrans:SCREEN-VALUE = "yes" THEN 
  t-purge:SCREEN-VALUE = "yes".


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_EmpMachTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_EmpMachTrans C-Win
ON VALUE-CHANGED OF tg_EmpMachTrans IN FRAME FRAME-A /* Posted Employee Machine Transactions */
DO:
   IF tg_EmpMachTrans:SCREEN-VALUE = "yes" THEN
     t-purge:SCREEN-VALUE = "yes".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_MachReltdTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_MachReltdTrans C-Win
ON VALUE-CHANGED OF tg_MachReltdTrans IN FRAME FRAME-A /* Unposted Machine  Related Employee Transactions T-F-3. */
DO:
   /*IF tg_EmpMachTrans:SCREEN-VALUE = "yes" THEN
     t-purge:SCREEN-VALUE = "yes". */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_MachTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_MachTrans C-Win
ON VALUE-CHANGED OF tg_MachTrans IN FRAME FRAME-A /* Posted Machine and Related Emp. Trans. */
DO:

IF tg_MachTrans:SCREEN-VALUE = "yes" THEN 
  t-purge:SCREEN-VALUE = "yes".


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
/*{sys/inc/f3helpw.i} */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

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

  RUN init-proc.
  RUN enable_UI.

  &IF DEFINED(tsPurge) EQ 0 &THEN
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + ' Posting'.
    HIDE {&purgeItems} NO-PAUSE.
  &ELSE
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + ' Purge History'.
    HIDE {&nonPurgeItems} NO-PAUSE.
  &ENDIF

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
     {custom/usrprint.i}
     APPLY "entry" TO begin_machine.
  END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-post C-Win 
PROCEDURE do-post :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-mach FOR mach.

FOR EACH tt-report:
    DELETE tt-report.
END.

FOR EACH b-mach fields(m-code) WHERE
    b-mach.company = g_company AND
    b-mach.m-code GE begin_machine AND
    b-mach.m-code LE end_machine
    NO-LOCK,
    EACH machtran
    WHERE machtran.company = g_company AND
          machtran.machine EQ b-mach.m-code AND
          machtran.posted EQ NO
      AND machtran.job_number GE begin_job-no
      AND machtran.job_number LE end_job-no
      AND machtran.job_sub GE int(begin_job-no2)
      AND machtran.job_sub LE int(end_job-no2):



    /* don't post to ASI only set posted flag to yes 
       07/30/01  YSK                                  */
    /*find first mach where mach.company = machtran.company and
                          mach.m-code = machtran.machine and
                          mach.industry = "X" 
                          no-lock no-error. */   

/*     IF NOT(((machtran.end_date GE begin_date AND */
/*             machtran.end_date LE end_date) or    */
/*             can-find (first mach where           */
/*              mach.company = machtran.company and */
/*              mach.m-code = machtran.machine and  */
/*              mach.industry = "X" ))) THEN        */
/*         NEXT.                                    */

       IF NOT(((machtran.end_date GE begin_date AND
            machtran.end_date LE end_date) AND
            (machtran.shift GE TRIM(STRING(begin_shift,">>")) AND
             machtran.shift LE TRIM(STRING(end_shift,">>"))) ) or
            can-find (first mach where
            mach.company = machtran.company AND
            mach.m-code = machtran.machine and
            mach.industry = "X")) OR 
           machtran.end_date = ? THEN
       NEXT.

    /* gdm - 04150912 
    FIND FIRST emplogin NO-LOCK
       WHERE emplogin.company    EQ g_company
         AND emplogin.machine    EQ machtran.machine
         AND emplogin.start_date GE machtran.start_date
         AND emplogin.end_date   LE machtran.end_date 
         AND emplogin.start_time GE machtran.start_time
         AND emplogin.end_time   LT machtran.end_time NO-ERROR.

    IF NOT AVAIL emplogin THEN NEXT.

     gdm - 04150912 end */

    FOR EACH machemp 
          WHERE machemp.table_rec_key = machtran.rec_key
          AND machemp.shift GE TRIM(STRING(begin_shift,">>"))
          AND machemp.shift LE TRIM(STRING(end_shift,">>"))
          and machemp.end_date >= begin_date 
          and machemp.end_date <= end_date
          AND machemp.posted = NO
          USE-INDEX pi-machemp:
        machemp.posted = YES.         
    END.

    machtran.posted = yes.
END.

FOR EACH ttbl_pc-prdh EXCLUSIVE-LOCK:
  CREATE  pc-prdh.
  BUFFER-COPY ttbl_pc-prdh TO pc-prdh.
END.

FOR EACH ttbl_pc-prdd EXCLUSIVE-LOCK:
  CREATE  pc-prdd.
  BUFFER-COPY ttbl_pc-prdd TO pc-prdd.
  IF v-tspost THEN DO:
     CREATE tt-report.
     tt-report.rec-id = RECID(pc-prdd).
  END.
END.

IF v-tspost THEN RUN post-wip.

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
  DISPLAY lv-text-1 begin_machine lv-text-2 end_machine fi_BeginEmp fi_EndEmp 
          begin_date end_date FILL-IN-1 begin_shift end_shift t-purge t-prt-rate 
          tg_MachTrans tg_EmpMachTrans tg_blank-end-date rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm fi_file 
          begin_job-no begin_job-no2 end_job-no end_job-no2 tg_MachReltdTrans 
          tb_excel tb_runExcel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE lv-text-1 begin_machine lv-text-2 end_machine fi_BeginEmp fi_EndEmp 
         begin_date end_date begin_shift end_shift t-purge t-prt-rate 
         tg_MachTrans tg_EmpMachTrans tg_blank-end-date rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm fi_file btn-ok btn-cancel 
         begin_job-no begin_job-no2 end_job-no end_job-no2 tg_MachReltdTrans 
         tb_excel tb_runExcel RECT-6 RECT-7 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gl-from-work C-Win 
PROCEDURE gl-from-work :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-run AS INT NO-UNDO.
  DEF INPUT PARAM ip-trnum AS INT NO-UNDO.

  def var credits as dec init 0 no-undo.
  def var debits as dec init 0 no-undo. 


  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE TODAY
        AND period.pend    GE TODAY
      NO-LOCK.

  for each work-gl 
      where (ip-run eq 1 and work-gl.job-no ne "")
         or (ip-run eq 2 and work-gl.job-no eq "")
      break by work-gl.actnum:

    assign
     debits  = debits  + work-gl.debits
     credits = credits + work-gl.credits.

    if last-of(work-gl.actnum) then do:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-gl.actnum
       gltrans.jrnl    = "JCOST"
       gltrans.period  = period.pnum
       gltrans.tr-amt  = debits - credits
       gltrans.tr-date = TODAY
       gltrans.tr-dscr = "Production Job Costing"
       gltrans.trnum   = ip-trnum.

      assign
       debits  = 0
       credits = 0.
    end.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc C-Win 
PROCEDURE init-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN begin_date = DATE(1,1,YEAR(TODAY))
         END_date = TODAY
         begin_machine = ""
         END_machine = "zzzzzzzz"
         fi_BeginEmp = "" 
         fi_EndEmp   = "zzzzzzzzzzzz"
         begin_shift = 0
         END_shift = 99.
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
   RUN custom/prntproc.p (list-name,INT(lv-font-no), lv-ornt).

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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-wip C-Win 
PROCEDURE post-wip :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* --------------------------------------------------- pc/pc-post.p  8/94 gb  */
/* Production Control - Posting Entry                                         */
/* -------------------------------------------------------------------------- */

def var v-loc       like fg-bin.loc NO-UNDO.
def var v-loc-bin   like fg-bin.loc-bin NO-UNDO.
def var v-up-hs     like eb.num-up NO-UNDO.
def var v-up        like eb.num-up NO-UNDO.
def var v-out       like est-op.n-out NO-UNDO.
def var v-on        like eb.num-up NO-UNDO.
def var v-est-type  like est.est-type NO-UNDO.
def var v-trnum like gl-ctrl.trnum no-undo.
DEF VAR X AS INT NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF BUFFER bf-itemfg FOR itemfg.

FOR EACH tt-report NO-LOCK,
    FIRST pc-prdd WHERE RECID(pc-prdd) EQ tt-report.rec-id,          
    first mach
    {sys/ref/machW.i}
      and mach.m-code eq pc-prdd.m-code
    no-lock,

    first job
    where job.company eq cocode
      and job.job     eq pc-prdd.job
      and job.job-no  eq pc-prdd.job-no
      and job.job-no2 eq pc-prdd.job-no2             
    break by pc-prdd.m-code

    TRANSACTION:

  find first w-job where w-job.job eq job.job no-error.
  if not avail w-job then create w-job.
  w-job.job = job.job.

  assign
   v-up  = 1
   v-out = 1
   v-on  = 1.

  FIND FIRST est
      WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
      NO-LOCK NO-ERROR.
  v-est-type = IF AVAIL est THEN est.est-type ELSE 1.
  IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

  FOR EACH mach-part WHERE
      mach-part.company EQ mach.company AND
      mach-part.m-code EQ mach.m-code
      EXCLUSIVE-LOCK:

      mach-part.total-impressions-run = mach-part.total-impressions-run
                                        + pc-prdd.qty + pc-prdd.waste.

      FIND FIRST reftable WHERE
           reftable.reftable EQ "MACHPARTHOURS" AND
           reftable.company  EQ mach-part.company AND
           reftable.loc      EQ mach-part.m-code AND
           reftable.code     EQ mach-part.rm-part-code
           EXCLUSIVE-LOCK NO-ERROR.

      IF NOT AVAIL reftable THEN DO:
         CREATE reftable.
         ASSIGN
           reftable.reftable = "MACHPARTHOURS"
           reftable.company  = mach-part.company
           reftable.loc      = mach-part.m-code
           reftable.code     = mach-part.rm-part-code. 
      END.

      reftable.val[1] = reftable.val[1]
                      + pc-prdd.hours.

      RELEASE reftable.
  END.

  IF mach.dept[1] EQ "PR" OR mach.dept[2] EQ "PR" OR
     mach.dept[3] EQ "PR" OR mach.dept[4] EQ "PR" THEN
    RUN update-plate-die (ROWID(pc-prdd), "P", v-est-type).

  IF mach.dept[1] EQ "DC" OR mach.dept[2] EQ "DC" OR
     mach.dept[3] EQ "DC" OR mach.dept[4] EQ "DC" THEN
    RUN update-plate-die (ROWID(pc-prdd), "D", v-est-type).

  IF INDEX("AP",mach.p-type) GT 0 THEN
    ASSIGN
     v-on  = 1
     v-up  = 1
     v-out = 1.

  ELSE
  if avail est then do:
    run sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, output v-up).

    find first ef
        where ef.company eq est.company
          and ef.est-no  eq est.est-no
          and ef.form-no eq pc-prdd.frm
        no-lock no-error.

    IF AVAIL ef THEN DO:
      RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).
      v-on = v-up * v-on.
    END.

    find first est-op
        where est-op.company eq est.company
          and est-op.est-no  eq est.est-no
          and est-op.s-num   eq pc-prdd.frm
          and (est-op.b-num  eq pc-prdd.blank-no or
               pc-prdd.blank-no eq 0)
          and est-op.m-code  eq pc-prdd.m-code
          and est-op.op-pass eq pc-prdd.pass
          and est-op.dept    eq pc-prdd.dept
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

  v-up-hs = 1.

  if pc-prdd.dept eq "HS" and
     avail est            and
     mach.therm           and
     mach.p-type eq "S"   then
    run sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, output v-up-hs).

  {pc/pcmchact.i}

  if pc-prdd.complete then do:
    RUN pc/pcprdd4u.p (ROWID(pc-prdd)).

    for each tt-job-hdr,
        first itemfg
        where itemfg.company    eq cocode
          and itemfg.i-no       eq tt-job-hdr.i-no
          and itemfg.case-count gt 0
        no-lock:

      x = 1.
      FOR EACH fg-rctd no-lock BY fg-rctd.r-no DESC:
        LEAVE.
      END.
      if avail fg-rctd then x = fg-rctd.r-no.

      find last fg-rcpth use-index r-no no-lock no-error.
      if avail fg-rcpth and fg-rcpth.r-no GT x then x = fg-rcpth.r-no.

      create fg-rctd.
      assign
       fg-rctd.r-no       = X + 1
       fg-rctd.rct-date   = pc-prdd.op-date
       fg-rctd.trans-time = pc-prdd.op-time
       fg-rctd.company    = cocode
       fg-rctd.rita-code  = "R"
       fg-rctd.i-name     = itemfg.i-name
       fg-rctd.i-no       = tt-job-hdr.i-no
       fg-rctd.job-no     = pc-prdd.job-no
       fg-rctd.job-no2    = pc-prdd.job-no2.

      assign
       v-up  = 1
       v-out = 1.

      if avail est and index("APB",mach.p-type) le 0 then do:
        run sys/inc/numup.p (est.company, est.est-no, pc-prdd.frm, output v-up).

        find first est-op
            where est-op.company eq est.company
              and est-op.est-no  eq est.est-no
              and est-op.s-num   eq pc-prdd.frm
              and (est-op.b-num  eq pc-prdd.blank-no or
                   pc-prdd.blank-no eq 0)
              and est-op.m-code  eq pc-prdd.m-code
              and est-op.op-pass eq pc-prdd.pass
              and est-op.dept    eq pc-prdd.dept
              and est-op.line    lt 500
            no-lock no-error.
        if avail est-op and est-op.n-out ne 0 then v-out = est-op.n-out.
      end.

      ASSIGN
       fg-rctd.b-num      = pc-prdd.blank-no
       fg-rctd.s-num      = pc-prdd.frm
       fg-rctd.t-qty      = pc-prdd.qty / v-up-hs * v-out * v-up
       fg-rctd.pur-uom    = itemfg.prod-uom
       fg-rctd.cost-uom   = itemfg.prod-uom
       fg-rctd.std-cost   = tt-job-hdr.std-tot-cost
       fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
       fg-rctd.qty-case   = itemfg.case-count
       fg-rctd.partial    = fg-rctd.t-qty modulo itemfg.case-count
       fg-rctd.cases      = trunc(fg-rctd.t-qty / itemfg.case-count,0)
       fg-rctd.cases-unit = 1.

      if fg-rctd.t-qty le 0 then fg-rctd.cases = 0.

      release fg-bin.

      FIND FIRST reftable NO-LOCK
          WHERE reftable.reftable EQ "pc/pcprddu3.p"
            AND reftable.company  EQ pc-prdd.company
            AND reftable.code     EQ /*pc-prdd.rec_key*/ STRING(RECID(pc-prdd))
          NO-ERROR.

      IF AVAIL reftable THEN DO:
        ASSIGN
         fg-rctd.cases      = reftable.val[1]
         fg-rctd.qty-case   = reftable.val[2]
         fg-rctd.cases-unit = reftable.val[3]
         fg-rctd.partial    = fg-rctd.t-qty - (fg-rctd.cases * fg-rctd.qty-case).

        FIND FIRST fg-bin 
            WHERE fg-bin.rec_key EQ reftable.code2 /*RECID(fg-bin) EQ INT(reftable.code2)*/ 
            NO-LOCK NO-ERROR.
      END.

      IF AVAIL fg-bin THEN
        ASSIGN
         v-loc       = fg-bin.loc
         v-loc-bin   = fg-bin.loc-bin
         fg-rctd.tag = fg-bin.tag.

      else
      if v-auto-bin eq "ShipTo" then do:
        /*get estimate blank file from finished goods item file*/
        find first eb
            where eb.company  eq cocode
              and eb.est-no   eq itemfg.est-no
              and eb.stock-no eq itemfg.i-no
            use-index est-no no-lock no-error.

        if avail eb then do:
          /*get customer file from estimate blank file*/
          find first cust
              where cust.company eq cocode
                and cust.cust-no eq eb.cust-no
              no-lock no-error.
          if avail cust then do:              
            find first shipto
                where shipto.company = cocode
                  and shipto.cust-no = cust.cust-no 
                no-lock no-error.
            if avail shipto then do:
              find first fg-bin
                  where fg-bin.company eq cocode
                    and fg-bin.loc     eq shipto.loc
                    and fg-bin.loc-bin eq shipto.loc-bin
                    and fg-bin.i-no    eq ""
                  no-lock no-error.
              if avail fg-bin then 
                ASSIGN
                 v-loc     = shipto.loc
                 v-loc-bin = shipto.loc-bin.
            end.

            if v-loc eq "" and v-loc-bin eq "" then do:
              find first fg-bin
                  where fg-bin.company eq cocode
                    and fg-bin.loc     eq itemfg.def-loc
                    and fg-bin.loc-bin eq itemfg.def-loc-bin
                    and fg-bin.i-no    eq ""
                  no-lock no-error.
              if avail fg-bin then 
                assign 
                 v-loc     = itemfg.def-loc
                 v-loc-bin = itemfg.def-loc-bin.
            end. /*if avail shipto*/
          end. /*if avail cust*/
        end. /*if avail eb*/
      end. /*if system default is shipto*/
      /*else if "FGFILE" then get from finished goods file*/
      else do:
        find first fg-bin
            where fg-bin.company eq cocode
              and fg-bin.loc     eq itemfg.def-loc
              and fg-bin.loc-bin eq itemfg.def-loc-bin
              and fg-bin.i-no    eq ""
            no-lock no-error.
        if avail fg-bin then 
          ASSIGN
           v-loc     = itemfg.def-loc
           v-loc-bin = itemfg.def-loc-bin.
      end. /*else FGFILE*/

      /*if bin and warehouse are blank, goto cust "X" shipto file*/
      if v-loc eq "" and v-loc-bin eq "" then do:
        find first cust
            where cust.company eq cocode
              and cust.active  eq "X"
            no-lock no-error.

        if avail cust then do:
          find first shipto
              where shipto.company eq cocode
                and shipto.cust-no eq cust.cust-no  
              no-lock no-error.
          if avail shipto then do:
            find first fg-bin
                where fg-bin.company eq cocode
                  and fg-bin.loc     eq shipto.loc
                  and fg-bin.loc-bin eq shipto.loc-bin
                  and fg-bin.i-no    eq ""
                no-lock no-error.
             ASSIGN
              v-loc     = shipto.loc
              v-loc-bin = shipto.loc-bin.
          end.                                  
        end.
      end.

      ASSIGN
       fg-rctd.loc     = v-loc
       fg-rctd.loc-bin = v-loc-bin.

      FIND FIRST fg-bin
          WHERE fg-bin.company EQ fg-rctd.company
            AND fg-bin.i-no    EQ fg-rctd.i-no
            AND fg-bin.job-no  EQ pc-prdd.job-no
            AND fg-bin.job-no2 EQ pc-prdd.job-no2
            AND fg-bin.loc     EQ fg-rctd.loc
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin
            AND fg-bin.tag     EQ fg-rctd.tag
          NO-LOCK NO-ERROR.

      IF AVAIL fg-bin THEN fg-rctd.cases-unit = fg-bin.cases-unit.
      RUN fg/comprcpt.p (ROWID(fg-rctd)).                         

    end. /* for each tt-job-hdr*/

  end. /* for each pc-prdd.completed*/
  delete pc-prdd.
end. /* for each tt-report, pc-prdd */

IF dcpostgl-log THEN DO TRANSACTION:
  /* gdm - 11050906 */
  REPEAT:
    FIND FIRST gl-ctrl EXCLUSIVE-LOCK
      WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
    IF AVAIL gl-ctrl THEN DO:
      ASSIGN v-trnum       = gl-ctrl.trnum + 1
             gl-ctrl.trnum = v-trnum.
      FIND CURRENT gl-ctrl NO-LOCK.
      RUN gl-from-work (1, v-trnum).
      RUN gl-from-work (2, v-trnum).
      LEAVE.
    END. /* IF AVAIL gl-ctrl */
  END. /* REPEAT */
  /* gdm - 11050906 */

END.

for each w-job,
    first job no-lock where job.company eq cocode and job.job eq w-job.job:
  run jc/job-cls2.p (recid(job)).
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*==== Report main body procedure ================================
==================================================================*/
DEF VAR ld-total-time AS INT NO-UNDO.
DEF VAR ld-emp-time AS INT NO-UNDO.
DEF VAR ld-mr-time AS INT NO-UNDO.
DEF VAR ld-run-time AS INT NO-UNDO.
DEF VAR li-job-cnt AS INT NO-UNDO.
DEF VAR li-mr-cnt AS INT NO-UNDO.
DEF VAR ld-mr-avg AS int NO-UNDO.
DEF VAR ld-run-avg AS int NO-UNDO.
DEF VAR ld-tot-avg AS int NO-UNDO.
def var lv-tmp-mr as int no-undo.
def var lv-tmp-run as int no-undo.
def var lv-tmp-tot as int no-undo.
def var ls-tot-mr as cha  no-undo.
def var ls-tot-run as cha no-undo.
def var ls-tot-tot as cha  no-undo.
def var ld-waste% as dec form ">,>>9.99"       no-undo.
def var lv-rqty like machtran.run_qty no-undo.
def var lv-wqty like machtran.waste_qty no-undo.
DEF VAR v-sch-mach AS LOG NO-UNDO.

DEF VAR v-rate-total AS DEC NO-UNDO.
DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.

SESSION:SET-WAIT-STATE("general").

{sys/form/r-top3w.f}

FORM HEADER SKIP(1) WITH FRAME r-top.

assign
   str-tit2 = c-win:TITLE
   {sys/inc/ctrtext.i str-tit2 112}

   str-tit3 = "TS Posting "
   {sys/inc/ctrtext.i str-tit3 132}.



{sys/inc/print1.i}
{sys/inc/outprint.i value(lines-per-page)}

FOR EACH ttbl_pc-prdh:    
    DELETE ttbl_pc-prdh.
END.

FOR EACH ttbl_pc-prdd:
    DELETE ttbl_pc-prdd.
END.

if td-show-parm then run show-param.

IF tb_excel THEN DO:
    OUTPUT STREAM excel TO VALUE(fi_file).
    ASSIGN excelheader = "Mach,Description,DP,Date,Sh,Job No,Form,Blank,"
                       + "Item,Description,Code,Hours,Crew,Start,Stop,Run Qty,Waste,C,EmpID,Rate".
    PUT STREAM excel UNFORMATTED excelheader SKIP.
END.

VIEW FRAME r-top.
selected-company = g_company.

{methods/lstlogic/custom/post_.i}

output close.

IF tb_excel THEN 
DO:
  OUTPUT STREAM excel CLOSE.

  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE("").

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
              assign 
                 parm-lbl-list = parm-lbl-list + ","
                 parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-plate-die C-Win 
PROCEDURE update-plate-die :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid    AS   ROWID NO-UNDO.
  DEF INPUT PARAM ip-upd-type AS   CHAR NO-UNDO.
  DEF INPUT PARAM ip-est-type LIKE est.est-type NO-UNDO.

  DEF BUFFER b-pc-prdd FOR pc-prdd.
  DEF BUFFER bf-job FOR job.

  FIND b-pc-prdd WHERE ROWID(b-pc-prdd) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-pc-prdd THEN
     FOR FIRST bf-job
        WHERE bf-job.company eq b-pc-prdd.company
          AND bf-job.job     eq b-pc-prdd.job
          AND bf-job.job-no  eq b-pc-prdd.job-no
          AND bf-job.job-no2 eq b-pc-prdd.job-no2
        NO-LOCK,
        FIRST job-hdr
        WHERE job-hdr.company   EQ bf-job.company
          AND job-hdr.job       EQ bf-job.job
          AND job-hdr.job-no    EQ bf-job.job-no
          AND job-hdr.job-no2   EQ bf-job.job-no2
          AND (job-hdr.frm      EQ b-pc-prdd.frm OR
               ip-est-type      EQ 2)
        NO-LOCK:

      FIND FIRST itemfg
          WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ job-hdr.i-no
          NO-LOCK NO-ERROR.

      IF ip-est-type EQ 2 AND job.est-no NE "" AND
         AVAIL itemfg AND itemfg.isaset        THEN
      FOR EACH eb
          WHERE eb.company EQ cocode
            AND eb.est-no  EQ bf-job.est-no
            AND eb.form-no EQ b-pc-prdd.frm
          NO-LOCK,
          FIRST itemfg
          WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ eb.stock-no
          NO-LOCK:
        LEAVE.
      END.

      IF AVAIL itemfg THEN DO:

        IF ip-upd-type EQ "P" AND itemfg.plate-no NE "" THEN
        FIND FIRST prep
            WHERE prep.company EQ cocode
              AND prep.code    EQ itemfg.plate-no
            NO-ERROR.

        ELSE
        IF ip-upd-type EQ "D" AND itemfg.die-no NE "" THEN
        FIND FIRST prep
            WHERE prep.company EQ cocode
              AND prep.code    EQ itemfg.die-no
            NO-ERROR.

        IF AVAIL prep THEN DO:
          ASSIGN prep.no-of-impressions = prep.no-of-impressions +
                                          b-pc-prdd.qty + b-pc-prdd.waste
                  prep.last-date        = b-pc-prdd.op-date.

          FIND FIRST reftable WHERE
               reftable.reftable EQ "PREPLASTJOB" AND
               reftable.company  EQ prep.company AND
               reftable.loc      EQ prep.loc AND
               reftable.code     EQ prep.CODE
               NO-ERROR.

          IF NOT AVAIL reftable THEN DO:
            CREATE reftable.
            ASSIGN
              reftable.reftable = "PREPLASTJOB"
              reftable.company  = prep.company
              reftable.loc      = prep.loc
              reftable.code     = prep.CODE. 
          END.

          ASSIGN
            reftable.code2    = b-pc-prdd.job-no
            reftable.val[1]   = b-pc-prdd.job-no2.

          RELEASE reftable.
          RELEASE prep.
        END.
      END.
     END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

