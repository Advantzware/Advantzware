&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:               jcrep/r-schtck.w

  Description:        Schedule Card Boards Selector

  Input Parameters:   <none>

  Output Parameters:  <none>

  Author:             Dennis G. Dizon

  Created:            Mar 30, 2007

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/*  Create an unnamed pool to store all the widgets created 
    by this procedure. This is a good default which assures
    that this procedure's triggers and internal procedures 
    will execute in this procedure's storage, and that proper
    cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Shared Variables */

/* Local Variable Definitions ---                                       */
DEF VAR list-name   AS CHAR NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR lv-pdf-file AS CHAR NO-UNDO.
DEF VAR init-dir    AS CHAR NO-UNDO.
DEF VAR tmp-dir     AS CHAR NO-UNDO.
DEF VAR fjob-no     AS CHAR NO-UNDO.
DEF VAR fjob-no2    AS INT  NO-UNDO.
DEF VAR tjob-no     AS CHAR NO-UNDO.
DEF VAR tjob-no2    AS INT  NO-UNDO.
DEF VAR v-colcnt    AS INT  NO-UNDO.

/* Includes */
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

DEF STREAM excel.

DEF TEMP-TABLE tt-wiptag LIKE wiptag
    FIELDS t-sht-wid LIKE reftable.val[1]
    FIELDS t-sht-len LIKE reftable.val[2]
    FIELDS t-sht-wid-len AS CHAR FORMAT "x(20)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_tag# end_tag# ~
begin_whs end_whs begin_bin end_bin begin_rmItem end_rmItem begin_FGItem ~
end_FGItem begin_job1 begin_job2 end_job1 end_job2 tgl-tag tgl-rmbin ~
tgl-RMwhs tgl-WIPwhs tgl-WIPBin tgl-job tgl-RMItem tgl-RMName tgl-FGItem ~
tgl-FGName tgl-TagQty tgl-RMTag# tgl-ShWL tgl-SubTotShSz rd-dest tb_excel ~
tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_tag# end_tag# begin_whs end_whs ~
begin_bin end_bin begin_rmItem end_rmItem begin_FGItem end_FGItem ~
begin_job1 begin_job2 end_job1 end_job2 tgl-tag tgl-rmbin tgl-RMwhs ~
tgl-WIPwhs tgl-WIPBin tgl-job tgl-RMItem tgl-RMName tgl-FGItem tgl-FGName ~
tgl-TagQty tgl-RMTag# tgl-ShWL tgl-SubTotShSz rd-dest tb_excel tb_runExcel ~
fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 begin_job2 end_job2 td-show-parm 

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

DEFINE VARIABLE begin_bin AS CHARACTER FORMAT "X(5)":U 
     LABEL "From Bin" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE begin_FGItem AS CHARACTER FORMAT "X(15)":U 
     LABEL "From FG Item" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job1 AS CHARACTER FORMAT "x(6)" 
     LABEL "From Job#" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1
     BGCOLOR 15 FGCOLOR 15 .

DEFINE VARIABLE begin_job2 AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE begin_rmItem AS CHARACTER FORMAT "X(15)":U 
     LABEL "From RM Item" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE begin_tag# AS CHARACTER FORMAT "X(20)":U 
     LABEL "From Tag #" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE begin_whs AS CHARACTER FORMAT "X(8)":U 
     LABEL "From Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE end_bin AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzzzzz" 
     LABEL "To Bin" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE end_FGItem AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To FG Item" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE end_job1 AS CHARACTER FORMAT "x(6)" INITIAL "zzzzzz" 
     LABEL "To Job#" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1
     BGCOLOR 15 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE end_job2 AS INTEGER FORMAT ">9" INITIAL 99 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE end_rmItem AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To RM Item" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE end_tag# AS CHARACTER FORMAT "X(20)":U INITIAL "zzzzzzzzzzzzzzzzzzzz" 
     LABEL "ToTag #" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE end_whs AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzz" 
     LABEL "To Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-wipmt.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier Size=5 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 40 BY 1.19 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 24.4 BY 3.43 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96.6 BY 5.71.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96.6 BY 8.62.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96.6 BY 4.29.

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

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tgl-FGItem AS LOGICAL INITIAL no 
     LABEL "FG Item #" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tgl-FGName AS LOGICAL INITIAL no 
     LABEL "FG Name" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE tgl-job AS LOGICAL INITIAL no 
     LABEL "Job #" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tgl-rmbin AS LOGICAL INITIAL no 
     LABEL "RM Bin" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tgl-RMItem AS LOGICAL INITIAL no 
     LABEL "RM Item #" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY 1.05 NO-UNDO.

DEFINE VARIABLE tgl-RMName AS LOGICAL INITIAL no 
     LABEL "RM Name" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tgl-RMTag# AS LOGICAL INITIAL no 
     LABEL "RM Tag #" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE tgl-RMwhs AS LOGICAL INITIAL no 
     LABEL "RM Whs" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tgl-ShWL AS LOGICAL INITIAL no 
     LABEL "Sheet Width and Length" 
     VIEW-AS TOGGLE-BOX
     SIZE 27.8 BY 1.05 NO-UNDO.

DEFINE VARIABLE tgl-SubTotShSz AS LOGICAL INITIAL no 
     LABEL "SubTot by RM Item & Sheet Size" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1.05 NO-UNDO.

DEFINE VARIABLE tgl-tag AS LOGICAL INITIAL no 
     LABEL "Tag #" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tgl-TagQty AS LOGICAL INITIAL no 
     LABEL "Tag Qty" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE tgl-WIPBin AS LOGICAL INITIAL no 
     LABEL "WIP Bin" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tgl-WIPwhs AS LOGICAL INITIAL no 
     LABEL "WIP Whs" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_tag# AT ROW 2.33 COL 11.8 COLON-ALIGNED WIDGET-ID 2
     end_tag# AT ROW 2.33 COL 58.2 COLON-ALIGNED WIDGET-ID 18
     begin_whs AT ROW 3.48 COL 16.4 COLON-ALIGNED WIDGET-ID 6
     end_whs AT ROW 3.48 COL 58.2 COLON-ALIGNED WIDGET-ID 20
     begin_bin AT ROW 4.67 COL 16.4 COLON-ALIGNED WIDGET-ID 8
     end_bin AT ROW 4.67 COL 58.2 COLON-ALIGNED WIDGET-ID 22
     begin_rmItem AT ROW 5.86 COL 16.4 COLON-ALIGNED WIDGET-ID 10
     end_rmItem AT ROW 5.86 COL 58.2 COLON-ALIGNED WIDGET-ID 24
     begin_FGItem AT ROW 7.05 COL 16.4 COLON-ALIGNED WIDGET-ID 12
     end_FGItem AT ROW 7.05 COL 58.2 COLON-ALIGNED WIDGET-ID 26
     begin_job1 AT ROW 8.24 COL 16.4 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job2 AT ROW 8.24 COL 43 COLON-ALIGNED HELP
          "Enter Beginning Run#"
     end_job1 AT ROW 8.24 COL 58.2 COLON-ALIGNED
     end_job2 AT ROW 8.24 COL 85.6 COLON-ALIGNED HELP
          "Enter Ending Run#"
     tgl-tag AT ROW 10.52 COL 3.2 WIDGET-ID 30
     tgl-rmbin AT ROW 10.52 COL 18.8 WIDGET-ID 32
     tgl-RMwhs AT ROW 10.52 COL 34 WIDGET-ID 34
     tgl-WIPwhs AT ROW 10.52 COL 49.4 WIDGET-ID 36
     tgl-WIPBin AT ROW 10.52 COL 65.6 WIDGET-ID 38
     tgl-job AT ROW 10.52 COL 81 WIDGET-ID 40
     tgl-RMItem AT ROW 11.43 COL 3.2 WIDGET-ID 42
     tgl-RMName AT ROW 11.43 COL 18.8 WIDGET-ID 44
     tgl-FGItem AT ROW 11.43 COL 34 WIDGET-ID 46
     tgl-FGName AT ROW 11.43 COL 49.4 WIDGET-ID 48
     tgl-TagQty AT ROW 11.43 COL 65.6 WIDGET-ID 50
     tgl-RMTag# AT ROW 11.43 COL 81 WIDGET-ID 52
     tgl-ShWL AT ROW 12.52 COL 3.2 WIDGET-ID 74
     tgl-SubTotShSz AT ROW 12.52 COL 34 WIDGET-ID 76
     lv-ornt AT ROW 13.86 COL 32 NO-LABEL
     td-show-parm AT ROW 14.1 COL 73
     rd-dest AT ROW 14.95 COL 5 NO-LABEL WIDGET-ID 58
     tb_excel AT ROW 15.24 COL 51 WIDGET-ID 68
     tb_runExcel AT ROW 15.24 COL 93 RIGHT-ALIGNED WIDGET-ID 70
     fi_file AT ROW 16.19 COL 49 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 66
     lines-per-page AT ROW 16.95 COL 46.8 COLON-ALIGNED
     lv-font-no AT ROW 16.95 COL 67 COLON-ALIGNED
     lv-font-name AT ROW 18.05 COL 31 COLON-ALIGNED NO-LABEL
     btn-ok AT ROW 20.05 COL 26
     btn-cancel AT ROW 20.05 COL 60.2
     "Output Destination" VIEW-AS TEXT
          SIZE 20 BY .95 AT ROW 14 COL 2
     "Print Options :" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 9.67 COL 2 WIDGET-ID 56
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.1 COL 1.4 WIDGET-ID 72
     RECT-6 AT ROW 13.86 COL 1
     RECT-7 AT ROW 1 COL 1
     RECT-8 AT ROW 9.57 COL 1 WIDGET-ID 28
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97 BY 20.57.


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
         TITLE              = "WIP Maintenance Report"
         HEIGHT             = 20.71
         WIDTH              = 97
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FILL-IN begin_job2 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       begin_job2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN end_job2 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       end_job2:PRIVATE-DATA IN FRAME FRAME-A     = 
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

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
       td-show-parm:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* WIP Maintenance Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* WIP Maintenance Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON RETURN OF FRAME FRAME-A
ANYWHERE
DO:
/*  
   IF SELF:TYPE <> "Button" THEN  do:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
   ELSE do:
       APPLY "choose" TO self.
       RETURN NO-APPLY.
   END.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bin C-Win
ON LEAVE OF begin_bin IN FRAME FRAME-A /* From Bin */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_FGItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_FGItem C-Win
ON LEAVE OF begin_FGItem IN FRAME FRAME-A /* From FG Item */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job1 C-Win
ON LEAVE OF begin_job1 IN FRAME FRAME-A /* From Job# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job2 C-Win
ON LEAVE OF begin_job2 IN FRAME FRAME-A /* - */
DO:
  IF {&self-name}:MODIFIED THEN RUN new-job-no.
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rmItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rmItem C-Win
ON LEAVE OF begin_rmItem IN FRAME FRAME-A /* From RM Item */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_tag#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_tag# C-Win
ON LEAVE OF begin_tag# IN FRAME FRAME-A /* From Tag # */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_whs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_whs C-Win
ON LEAVE OF begin_whs IN FRAME FRAME-A /* From Warehouse */
DO:
  ASSIGN {&self-name}.
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
  DEF VAR hold-title AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  IF NOT tgl-tag    AND 
     NOT tgl-rmbin  AND 
     NOT tgl-RMwhs  AND 
     NOT tgl-WIPwhs AND 
     NOT tgl-WIPBin AND 
     NOT tgl-job    AND 
     NOT tgl-RMItem AND 
     NOT tgl-RMName AND 
     NOT tgl-FGItem AND 
     NOT tgl-FGName AND 
     NOT tgl-TagQty AND 
     NOT tgl-RMTag# AND
     NOT tgl-ShWL    
    THEN DO:

        MESSAGE 
           "Please select at least one print option field to print."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.

  END.

  RUN count-item.

  RUN set-job-vars.

  RUN run-report.


  SESSION:SET-WAIT-STATE("general").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=END_RMItem
                            &END_cust=END_RMItem
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:title 
                            &fax-file=list-name }
       END.
       when 5 then do:
           {custom/asimailr.i &TYPE = ''
                                  &begin_cust=END_RMItem
                                  &END_cust=END_RMItem
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
                                  &mail-file=list-name }           

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
   SESSION:SET-WAIT-STATE("").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bin C-Win
ON LEAVE OF end_bin IN FRAME FRAME-A /* To Bin */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_FGItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_FGItem C-Win
ON LEAVE OF end_FGItem IN FRAME FRAME-A /* To FG Item */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job1 C-Win
ON LEAVE OF end_job1 IN FRAME FRAME-A /* To Job# */
DO:
  ASSIGN {&self-name}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job2 C-Win
ON LEAVE OF end_job2 IN FRAME FRAME-A /* - */
DO:
  IF {&self-name}:MODIFIED THEN RUN new-job-no.
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rmItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rmItem C-Win
ON LEAVE OF end_rmItem IN FRAME FRAME-A /* To RM Item */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_tag#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_tag# C-Win
ON LEAVE OF end_tag# IN FRAME FRAME-A /* ToTag # */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_whs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_whs C-Win
ON LEAVE OF end_whs IN FRAME FRAME-A /* To Warehouse */
DO:
  ASSIGN {&self-name}.
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
  lines-per-page = IF SELF:SCREEN-VALUE = "L" THEN 48 ELSE 99.
  DISP lines-per-page WITH FRAME {&FRAME-NAME}.
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


&Scoped-define SELF-NAME tgl-SubTotShSz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgl-SubTotShSz C-Win
ON VALUE-CHANGED OF tgl-SubTotShSz IN FRAME FRAME-A /* SubTot by RM Item  Sheet Size */
DO:
  ASSIGN {&SELF-NAME}.

  IF {&SELF-NAME} THEN ASSIGN 
                              tgl-ShWL   = YES
                              tgl-TagQty = YES
                              tgl-RMItem = YES
                              tgl-ShWL:SCREEN-VALUE   = "YES"
                              tgl-TagQty:SCREEN-VALUE = "YES"
                              tgl-RMItem:SCREEN-VALUE = "YES".         
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

  RUN enable_UI.

  {methods/nowait.i}

  /* uncommet this part if the greenbar is needed
  DO WITH FRAME {&frame-name}:
    {custom/usrprint.i}
  END.*/

  APPLY "entry" TO begin_tag# IN FRAME {&FRAME-NAME}.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE count-item C-Win 
PROCEDURE count-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-cnt AS INT NO-UNDO.
DEF VAR v-itmprnt AS CHAR NO-UNDO.

IF tgl-tag    THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-tag,'   .
IF tgl-rmbin  THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-rmbin,' .
IF tgl-RMwhs  THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-RMwhs,' .
IF tgl-WIPwhs THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-WIPwhs,'.
IF tgl-WIPBin THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-WIPBin,'.
IF tgl-job    THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-job,'   .
IF tgl-RMItem THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-RMItem,'.
IF tgl-RMName THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-RMName,'.
IF tgl-FGItem THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-FGItem,'.
IF tgl-FGName THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-FGName,'.
IF tgl-TagQty THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-TagQty,'.
IF tgl-RMTag# THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-RMTag#,'.

IF NUM-ENTRIES(v-itmprnt) GT 6 
  THEN
    ASSIGN lv-ornt    = "L"
           lv-font-no = "10".

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
  DISPLAY begin_tag# end_tag# begin_whs end_whs begin_bin end_bin begin_rmItem 
          end_rmItem begin_FGItem end_FGItem begin_job1 begin_job2 end_job1 
          end_job2 tgl-tag tgl-rmbin tgl-RMwhs tgl-WIPwhs tgl-WIPBin tgl-job 
          tgl-RMItem tgl-RMName tgl-FGItem tgl-FGName tgl-TagQty tgl-RMTag# 
          tgl-ShWL tgl-SubTotShSz rd-dest tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 RECT-8 begin_tag# end_tag# begin_whs end_whs begin_bin 
         end_bin begin_rmItem end_rmItem begin_FGItem end_FGItem begin_job1 
         begin_job2 end_job1 end_job2 tgl-tag tgl-rmbin tgl-RMwhs tgl-WIPwhs 
         tgl-WIPBin tgl-job tgl-RMItem tgl-RMName tgl-FGItem tgl-FGName 
         tgl-TagQty tgl-RMTag# tgl-ShWL tgl-SubTotShSz rd-dest tb_excel 
         tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-fax C-Win 
PROCEDURE output-to-fax :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*run output-to-fax.*/
DO WITH FRAME {&FRAME-NAME}:
    {custom/asifax.i &begin_cust=begin_job1
        &END_cust=END_job1
        &fax-subject=c-win:title
        &fax-body=c-win:title
        &fax-file=list-name }
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-mail C-Win 
PROCEDURE output-to-mail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

    {custom/asimailr.i &TYPE = ''
            &begin_cust= begin_job1
            &END_cust=end_job1
            &mail-subject=c-win:title
            &mail-body=c-win:title
            &mail-file=list-name }


END.

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

/* open file-name, title */ 

/* RUN scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). */
RUN scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{sys/form/r-topw.f}

DEF VAR v-xlshead AS CHAR NO-UNDO.

DEF VAR v-page-brk AS CHAR FORMAT "x(132)" NO-UNDO.

DEF VAR v-tag-no        LIKE wiptag.tag-no        NO-UNDO.
DEF VAR v-rm-bin        LIKE wiptag.rm-bin        NO-UNDO.
DEF VAR v-rm-whs        LIKE wiptag.rm-whs        NO-UNDO.
DEF VAR v-wip-warehouse LIKE wiptag.wip-warehouse NO-UNDO.
DEF VAR v-wip-rm-bin    LIKE wiptag.wip-rm-bin    NO-UNDO.
DEF VAR v-job-no        LIKE wiptag.job-no        NO-UNDO.
DEF VAR v-job-no2       LIKE wiptag.job-no2       NO-UNDO.
DEF VAR v-rm-i-no       LIKE wiptag.rm-i-no       NO-UNDO.
DEF VAR v-rm-i-name     LIKE wiptag.rm-i-name     NO-UNDO.
DEF VAR v-fg-i-no       LIKE wiptag.fg-i-no       NO-UNDO.
DEF VAR v-fg-i-name     LIKE wiptag.fg-i-name     NO-UNDO.
DEF VAR v-pallet-count  LIKE wiptag.pallet-count  NO-UNDO.
DEF VAR v-rm-tag-no     LIKE wiptag.rm-tag-no     NO-UNDO.
DEF VAR v-sht-wid-len   AS CHAR FORMAT "x(18)"    NO-UNDO.


DEF VAR v-subtot-count  LIKE wiptag.pallet-count  NO-UNDO.

FORM HEADER 
  SKIP(1)
    v-page-brk
  SKIP(1)
 WITH FRAME r-top2 NO-BOX PAGE-TOP STREAM-IO WIDTH 185.

ASSIGN str-tit2 = c-win:TITLE 
       {sys/inc/ctrtext.i str-tit2 142} .

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

IF tb_excel THEN DO:

  OUTPUT STREAM excel TO VALUE(fi_file).

  IF tgl-tag  
    THEN ASSIGN v-xlshead = v-xlshead + "Tag #,".
  IF tgl-rmbin
    THEN ASSIGN v-xlshead = v-xlshead + "RM Bin,".
  IF tgl-RMwhs
    THEN ASSIGN v-xlshead = v-xlshead + "RM Whs,".
  IF tgl-WIPwhs
    THEN ASSIGN v-xlshead = v-xlshead + "WIP Whs,".
  IF tgl-WIPBin
    THEN ASSIGN v-xlshead = v-xlshead + "WIP Bin,".
  IF tgl-job
    THEN ASSIGN v-xlshead = v-xlshead + "Job #,,".
  IF tgl-RMItem
    THEN ASSIGN v-xlshead = v-xlshead + "RM Item #,".
  IF tgl-RMName
    THEN ASSIGN v-xlshead = v-xlshead + "RM Name,".
  IF tgl-FGItem
    THEN ASSIGN v-xlshead = v-xlshead + "FG Item #,".
  IF tgl-FGName
    THEN ASSIGN v-xlshead = v-xlshead + "FG Name,".
  IF tgl-TagQty
    THEN ASSIGN v-xlshead = v-xlshead + "Tag Qty,".
  IF tgl-RMTag#
    THEN ASSIGN v-xlshead = v-xlshead + "RM Tag #,".
  IF tgl-ShWL
    THEN ASSIGN v-xlshead = v-xlshead + "Sheet Width,Sheet Length," .

  IF v-xlshead NE "" 
    THEN PUT STREAM excel UNFORMATTED v-xlshead SKIP.

END.

display "" with frame r-top.

IF tgl-tag    THEN PUT UNFORMATTED "  Tag #              ".    
IF tgl-rmbin  THEN PUT UNFORMATTED "RM Bin   ".   
IF tgl-RMwhs  THEN PUT UNFORMATTED "RM Whs ".   
IF tgl-WIPwhs THEN PUT UNFORMATTED "WIP Whs ".  
IF tgl-WIPBin THEN PUT UNFORMATTED "WIP Bin  ".  
IF tgl-job    THEN PUT UNFORMATTED "Job #        ".    
IF tgl-RMItem THEN PUT UNFORMATTED "RM Item #       ".
IF tgl-RMName THEN PUT UNFORMATTED "  RM Name                      ".  
IF tgl-FGItem THEN PUT UNFORMATTED " FG Item #      ".
IF tgl-FGName THEN PUT UNFORMATTED "  FG Name                      ".  
IF tgl-TagQty THEN PUT UNFORMATTED "Tag Qty    ".  
IF tgl-RMTag# THEN PUT UNFORMATTED "  RM Tag #        ". 
IF tgl-ShWL   THEN PUT UNFORMATTED "Sheet Wid  Sheet Len". 

PUT SKIP.

IF tgl-tag    THEN PUT UNFORMATTED "-------------------- ".
IF tgl-rmbin  THEN PUT UNFORMATTED "-------- ".   
IF tgl-RMwhs  THEN PUT UNFORMATTED "------ ".                                     
IF tgl-WIPwhs THEN PUT UNFORMATTED "------- ".                                     
IF tgl-WIPBin THEN PUT UNFORMATTED "-------- ".  
IF tgl-job    THEN PUT UNFORMATTED "------ -- ".    
IF tgl-RMItem THEN PUT UNFORMATTED "--------------- ".                                   
IF tgl-RMName THEN PUT UNFORMATTED "------------------------------ ".                                   
IF tgl-FGItem THEN PUT UNFORMATTED "--------------- ".
IF tgl-FGName THEN PUT UNFORMATTED "------------------------------ ".                                     
IF tgl-TagQty THEN PUT UNFORMATTED "---------- ".  
IF tgl-RMTag# THEN PUT UNFORMATTED "-------------------- ".
IF tgl-ShWL   THEN PUT UNFORMATTED "---------- ---------".

PUT SKIP.

EMPTY TEMP-TABLE tt-wiptag.

FOR EACH wiptag NO-LOCK
  WHERE wiptag.company EQ cocode 
    AND wiptag.tag-no  GE begin_tag# 
    AND wiptag.tag-no  LE end_tag# 
    AND wiptag.rm-whs  GE begin_whs
    AND wiptag.rm-whs  LE end_whs
    AND wiptag.rm-bin  GE begin_bin 
    AND wiptag.rm-bin  LE end_bin 
    AND wiptag.rm-i-no GE begin_rmItem 
    AND wiptag.rm-i-no LE end_rmItem 
    AND wiptag.fg-i-no GE begin_FGItem 
    AND wiptag.fg-i-no LE end_FGItem       
    AND wiptag.job-no  GE SUBSTR(fjob-no,1,6)
    AND wiptag.job-no  LE SUBSTR(tjob-no,1,6)
    AND FILL(" ",6 - LENGTH(TRIM(wiptag.job-no))) +
             TRIM(wiptag.job-no) +
             STRING(wiptag.job-no2,"99")  GE fjob-no
    AND FILL(" ",6 - LENGTH(TRIM(wiptag.job-no))) +
             TRIM(wiptag.job-no) +
             STRING(wiptag.job-no2,"99")  LE tjob-no
    :

    FIND FIRST reftable NO-LOCK
      WHERE reftable.reftable = "WIPLEN" 
        AND reftable.company = wiptag.company 
        AND reftable.CODE = wiptag.tag-no USE-INDEX CODE NO-ERROR.

    CREATE tt-wiptag.
    BUFFER-COPY wiptag TO tt-wiptag NO-ERROR.

    IF AVAIL reftable 
      THEN ASSIGN tt-wiptag.t-sht-wid     = reftable.val[1]
                  tt-wiptag.t-sht-len     = reftable.val[2]
                  tt-wiptag.t-sht-wid-len = STRING(tt-wiptag.t-sht-wid,">>9.9999")
                                            + "   " +
                                            STRING(tt-wiptag.t-sht-len,">>9.9999").
      ELSE ASSIGN tt-wiptag.t-sht-wid     = 0
                  tt-wiptag.t-sht-len     = 0
                  tt-wiptag.t-sht-wid-len = STRING(tt-wiptag.t-sht-wid,">>9.9999")
                                            + "   " +
                                            STRING(tt-wiptag.t-sht-len,">>9.9999").
END.


FOR EACH tt-wiptag NO-LOCK 
   BREAK BY tt-wiptag.rm-i-no
         BY tt-wiptag.t-sht-wid-len:

   ASSIGN  
       v-tag-no        = tt-wiptag.tag-no        
       v-rm-bin        = tt-wiptag.rm-bin        
       v-rm-whs        = tt-wiptag.rm-whs        
       v-wip-warehouse = tt-wiptag.wip-warehouse 
       v-wip-rm-bin    = tt-wiptag.wip-rm-bin    
       v-job-no        = tt-wiptag.job-no        
       v-job-no2       = tt-wiptag.job-no2       
       v-rm-i-no       = tt-wiptag.rm-i-no       
       v-rm-i-name     = tt-wiptag.rm-i-name     
       v-fg-i-no       = tt-wiptag.fg-i-no       
       v-fg-i-name     = tt-wiptag.fg-i-name     
       v-pallet-count  = tt-wiptag.pallet-count  
       v-rm-tag-no     = tt-wiptag.rm-tag-no
       v-sht-wid-len   = tt-wiptag.t-sht-wid-len.

    ASSIGN v-subtot-count  =  v-subtot-count + tt-wiptag.pallet-count.

    IF tgl-tag    THEN PUT UNFORMATTED v-tag-no FORMAT "x(21)".

    IF tgl-rmbin  THEN PUT UNFORMATTED v-rm-bin FORMAT "x(9)". 

    IF tgl-RMwhs  THEN PUT UNFORMATTED v-rm-whs FORMAT "x(7)".      

    IF tgl-WIPwhs THEN PUT UNFORMATTED v-wip-warehouse FORMAT "x(8)".

    IF tgl-WIPBin THEN PUT UNFORMATTED v-wip-rm-bin FORMAT "x(9)".  

    IF tgl-job    THEN PUT UNFORMATTED v-job-no + " " + 
                                       STRING(v-job-no2,"99") FORMAT "x(10)".

    IF tgl-RMItem THEN PUT UNFORMATTED v-rm-i-no FORMAT "x(16)".

    IF tgl-RMName THEN PUT UNFORMATTED v-rm-i-name FORMAT "x(31)".

    IF tgl-FGItem THEN PUT UNFORMATTED v-fg-i-no FORMAT "x(16)".

    IF tgl-FGName THEN PUT UNFORMATTED v-fg-i-name FORMAT "x(31)".   

    IF tgl-TagQty THEN PUT UNFORMATTED STRING(v-pallet-count,"->,>>>,>>9") " ".

    IF tgl-RMTag# THEN PUT UNFORMATTED v-rm-tag-no FORMAT "x(20)".

    IF tgl-ShWL   THEN PUT UNFORMATTED v-sht-wid-len FORMAT "x(20)".

    PUT SKIP.

    IF tgl-SubTotShSz AND 
       tgl-TagQty     AND
       tgl-ShWL       AND 
       tgl-RMItem
      THEN DO:

        IF LAST-OF(tt-wiptag.t-sht-wid-len) 
          THEN DO:

           PUT UNFORMATTED 
               SKIP(1) SPACE(40) 

/*                "Sheet Width And Length Total Qty:    " v-subtot-count. */
               "Sub Tot Qty of Sheets :  " 
               v-subtot-count.
           PUT UNFORMATTED  SKIP(2).

           ASSIGN v-subtot-count = 0.
        END.            

    END.


    IF tb_excel THEN DO:

      IF tgl-tag    THEN PUT STREAM excel UNFORMATTED '"' v-tag-no        '",'. 
      IF tgl-rmbin  THEN PUT STREAM excel UNFORMATTED '"' v-rm-bin        '",'. 
      IF tgl-RMwhs  THEN PUT STREAM excel UNFORMATTED '"' v-rm-whs        '",'. 
      IF tgl-WIPwhs THEN PUT STREAM excel UNFORMATTED '"' v-wip-warehouse '",'. 
      IF tgl-WIPBin THEN PUT STREAM excel UNFORMATTED '"' v-wip-rm-bin    '",'. 
      IF tgl-job    THEN PUT STREAM excel UNFORMATTED '"' v-job-no        '",' 
                                                      '"' v-job-no2       '",'. 
      IF tgl-RMItem THEN PUT STREAM excel UNFORMATTED '"' v-rm-i-no       '",'. 
      IF tgl-RMName THEN PUT STREAM excel UNFORMATTED '"' v-rm-i-name     '",'. 
      IF tgl-FGItem THEN PUT STREAM excel UNFORMATTED '"' v-fg-i-no       '",'. 
      IF tgl-FGName THEN PUT STREAM excel UNFORMATTED '"' v-fg-i-name     '",'. 
      IF tgl-TagQty THEN PUT STREAM excel UNFORMATTED '"' v-pallet-count  '",'. 
      IF tgl-RMTag# THEN PUT STREAM excel UNFORMATTED '"' v-rm-tag-no     '",'. 
      IF tgl-ShWL   THEN PUT STREAM excel UNFORMATTED '"' tt-wiptag.t-sht-wid '",'. 
      IF tgl-ShWL   THEN PUT STREAM excel UNFORMATTED '"' tt-wiptag.t-sht-len '",'. 

      PUT STREAM excel UNFORMATTED SKIP.

    END.

END. /* for each wiptag */          

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-job-vars C-Win 
PROCEDURE set-job-vars :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fjob-no   = FILL(" ",6 - LENGTH(TRIM(begin_job1:SCREEN-VALUE))) +
                 TRIM(begin_job1:SCREEN-VALUE)
     tjob-no   = FILL(" ",6 - LENGTH(TRIM(end_job1:SCREEN-VALUE))) +
                 trim(end_job1:SCREEN-VALUE)
     fjob-no2  = INT(begin_job2:SCREEN-VALUE)
     tjob-no2  = INT(end_job2:SCREEN-VALUE)
     fjob-no   = FILL(" ",6 - LENGTH(TRIM(fjob-no))) + TRIM(fjob-no) +
                 STRING(fjob-no2,"99")
     tjob-no   = FILL(" ",6 - LENGTH(TRIM(tjob-no))) + TRIM(tjob-no) +
                 STRING(tjob-no2,"99").
  END.


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

