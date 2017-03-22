&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:               jcrep/r-wiptag.w

  Description:        WIP TAG Report

  Input Parameters:   <none>

  Output Parameters:  <none>

  Author:             Stacey Brooks

  Created:            Jan 2012

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
/*DEF VAR tmp-dir     AS CHAR NO-UNDO.*/
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

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF VAR str-tit4 AS cha FORM "x(190)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(190)" NO-UNDO.
DEF VAR str-line AS cha FORM "x(190)" NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.



ASSIGN cTextListToSelect = "Job #,FG Item #,FG Name,RM Tag #,Sht Wid,Sht Len,Mach,Dept,Tag Qty"
       cFieldListToSelect = "job,ino,iname,rmtg,sh-wid,sh-len,mach,dept,tag-qty"
       cFieldLength = "10,15,30,20,10,10,5,15,14"
       cFieldType = "c,c,c,c,i,i,c,c,i"
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Job #,FG Item #,FG Name,RM Tag #,Sht Wid,Sht Len,Mach,Dept,Tag Qty" .

DEF TEMP-TABLE tt-wiptag LIKE wiptag
    FIELD seq LIKE dept.seq
    FIELD dept-code LIKE dept.code
    FIELD dept-name LIKE dept.dscr
    FIELDS t-sht-wid LIKE reftable.val[1]
    FIELDS t-sht-len LIKE reftable.val[2]
    FIELD m-code    LIKE mach.m-code
    FIELD produced-qty LIKE wiptag-mch.produced-qty
    INDEX item-seq IS PRIMARY tag-no seq ASCENDING.
/* INDEX index-name [ IS [ UNIQUE ] [ PRIMARY ] ]    */
/* { index-field [ ASCENDING | DESCENDING ] } . . .  */

/*     FIELDS t-sht-wid LIKE reftable.val[1]         */
/*     FIELDS t-sht-len LIKE reftable.val[2]         */
/*     FIELDS t-sht-wid-len AS CHAR FORMAT "x(20)".  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_tag# end_tag# ~
begin_i-no end_i-no begin_job1 begin_job2 end_job1 end_job2 begin_cust ~
end_cust rsReport rd-dest tb_excel tb_runExcel fi_file btn-ok btn-cancel ~
sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down
&Scoped-Define DISPLAYED-OBJECTS begin_tag# end_tag# begin_i-no ~
end_i-no begin_job1 begin_job2 end_job1 end_job2 begin_cust end_cust ~
rsReport rd-dest tb_excel tb_runExcel fi_file sl_avail sl_selected

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 td-show-parm 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
  ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Detail C-Win 
FUNCTION Detail RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Summary C-Win 
FUNCTION Summary RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "From Cust#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "From FG Item" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job1 AS CHARACTER FORMAT "X(6)":U 
     LABEL "From Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job2 AS CHARACTER FORMAT "99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_tag# AS CHARACTER FORMAT "X(20)":U 
     LABEL "From Tag #" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Cust#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To FG Item" 
     VIEW-AS FILL-IN 
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE end_job1 AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
     LABEL "To Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job2 AS CHARACTER FORMAT "99":U INITIAL "99" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_tag# AS CHARACTER FORMAT "X(20)":U INITIAL "zzzzzzzzzzzzzzzzzzzz" 
     LABEL "ToTag #" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-wiptag.csv" 
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
"To File", 3,
"To Email", 5
     SIZE 23 BY 5.24 NO-UNDO.

DEFINE VARIABLE rsReport AS CHARACTER INITIAL "Detail" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Detail", "Detail",
"Summary", "Summary",
"Test", "Test"
     SIZE 37 BY .71 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 107 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 107 BY 8.57.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_tag# AT ROW 2.91 COL 17 COLON-ALIGNED WIDGET-ID 2
     end_tag# AT ROW 2.91 COL 65 COLON-ALIGNED WIDGET-ID 18
     begin_i-no AT ROW 4.1 COL 17 COLON-ALIGNED WIDGET-ID 12
     end_i-no AT ROW 4.1 COL 65 COLON-ALIGNED WIDGET-ID 26
     begin_job1 AT ROW 5.29 COL 17 COLON-ALIGNED HELP
          "Enter Beginning Job Number" WIDGET-ID 86
     begin_job2 AT ROW 5.29 COL 29.2 COLON-ALIGNED HELP
          "Enter Beginning Job Number" WIDGET-ID 88
     end_job1 AT ROW 5.29 COL 65 COLON-ALIGNED HELP
          "Enter Ending Job Number" WIDGET-ID 90
     end_job2 AT ROW 5.29 COL 77 COLON-ALIGNED HELP
          "Enter Ending Job Number" WIDGET-ID 92
     begin_cust AT ROW 6.48 COL 17 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 78
     end_cust AT ROW 6.48 COL 65 COLON-ALIGNED HELP
          "Enter Ending Customer Number" WIDGET-ID 80
     rsReport AT ROW 8.38 COL 34 NO-LABEL WIDGET-ID 82
     sl_avail AT ROW 10.57 COL 8 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 10.57 COL 46 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 10.57 COL 66.6 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 11.57 COL 46 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 12.57 COL 46 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 13.62 COL 46 WIDGET-ID 40
     btn_down AT ROW 14.62 COL 46 WIDGET-ID 42
     lv-ornt AT ROW 17.33 COL 34 NO-LABEL
     rd-dest AT ROW 17.81 COL 4 NO-LABEL WIDGET-ID 96
     td-show-parm AT ROW 19.24 COL 51
     tb_excel AT ROW 20.33 COL 51 WIDGET-ID 68
     tb_runExcel AT ROW 20.33 COL 93 RIGHT-ALIGNED WIDGET-ID 70
     fi_file AT ROW 21.29 COL 49 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 66
     lines-per-page AT ROW 22.48 COL 46.8 COLON-ALIGNED
     lv-font-no AT ROW 22.48 COL 67 COLON-ALIGNED
     lv-font-name AT ROW 23.62 COL 31 COLON-ALIGNED NO-LABEL
     btn-ok AT ROW 25.91 COL 26
     btn-cancel AT ROW 25.91 COL 60.2
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 9.86 COL 8.6 WIDGET-ID 38
     "Output Destination" VIEW-AS TEXT
          SIZE 20 BY .95 AT ROW 16.38 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.1 COL 1.4 WIDGET-ID 72
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 9.86 COL 66.6 WIDGET-ID 44
     RECT-6 AT ROW 15.91 COL 2
     RECT-7 AT ROW 1 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 108.8 BY 26.33.


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
         TITLE              = "WIP TAG Report"
         HEIGHT             = 26.33
         WIDTH              = 108.8
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


ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_job1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

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
ON END-ERROR OF C-Win /* WIP TAG Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* WIP TAG Report */
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


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* From Cust# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* From FG Item */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job1 C-Win
ON LEAVE OF begin_job1 IN FRAME FRAME-A /* From Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job2 C-Win
ON LEAVE OF begin_job2 IN FRAME FRAME-A
DO:
  assign {&self-name}.
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

/*   IF NOT tgl-tag    AND                                               */
/*      NOT tgl-rmbin  AND                                               */
/*      NOT tgl-RMwhs  AND                                               */
/*      NOT tgl-WIPwhs AND                                               */
/*      NOT tgl-WIPBin AND                                               */
/*      NOT tgl-job    AND                                               */
/*      NOT tgl-RMItem AND                                               */
/*      NOT tgl-RMName AND                                               */
/*      NOT tgl-FGItem AND                                               */
/*      NOT tgl-FGName AND                                               */
/*      NOT tgl-TagQty AND                                               */
/*      NOT tgl-RMTag# AND                                               */
/*      NOT tgl-ShWL                                                     */
/*     THEN DO:                                                          */
/*                                                                       */
/*         MESSAGE                                                       */
/*            "Please select at least one print option field to print."  */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                          */
/*                                                                       */
/*         RETURN NO-APPLY.                                              */
/*                                                                       */
/*   END.                                                                */

  RUN GetSelectionList.

  RUN count-item.

  RUN set-job-vars.

  RUN run-report.
  STATUS DEFAULT "Processing Complete".

  SESSION:SET-WAIT-STATE("general").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           {custom/asimailr.i &TYPE = ''
                                  &begin_cust=end_i-no
                                  &END_cust=end_i-no
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
                                  &mail-file=list-name }           
       END.

  end case. 

   SESSION:SET-WAIT-STATE("").

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
ON LEAVE OF end_cust IN FRAME FRAME-A /* To Cust# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* To FG Item */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job1 C-Win
ON LEAVE OF end_job1 IN FRAME FRAME-A /* To Job# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job2 C-Win
ON LEAVE OF end_job2 IN FRAME FRAME-A
DO:
  assign {&self-name}.
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

&Scoped-define SELF-NAME begin_tag# 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_tag#  C-Win
ON HELP OF begin_tag#  IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR help-recid AS RECID NO-UNDO.

     run windows/l-wptag1.w (cocode, focus:screen-value, output char-val, OUTPUT help-recid).
         if char-val <> "" then do:
            assign
             begin_tag#:screen-value  = ENTRY(1,char-val).
         END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job2  C-Win
ON HELP OF begin_job2  IN FRAME FRAME-A /* Font */
DO: 

END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job2  C-Win
ON HELP OF end_job2  IN FRAME FRAME-A /* Font */
DO:

END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_tag#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_tag# C-Win
ON HELP OF end_tag# IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR help-recid AS RECID NO-UNDO.

     run windows/l-wptag1.w (cocode, end_tag#:screen-value, output char-val, OUTPUT help-recid).
         if char-val <> "" then do:
            assign
             end_tag#:screen-value  = ENTRY(1,char-val).
         END.
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

  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_tag#.
  END.


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

/* IF tgl-tag    THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-tag,'   .  */
/* IF tgl-rmbin  THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-rmbin,' .  */
/* IF tgl-RMwhs  THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-RMwhs,' .  */
/* IF tgl-WIPwhs THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-WIPwhs,'.  */
/* IF tgl-WIPBin THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-WIPBin,'.  */
/* IF tgl-job    THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-job,'   .  */
/* IF tgl-RMItem THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-RMItem,'.  */
/* IF tgl-RMName THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-RMName,'.  */
/* IF tgl-FGItem THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-FGItem,'.  */
/* IF tgl-FGName THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-FGName,'.  */
/* IF tgl-TagQty THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-TagQty,'.  */
/* IF tgl-RMTag# THEN ASSIGN v-itmprnt = v-itmprnt + 'tgl-RMTag#,'.  */

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
  DISPLAY begin_tag# end_tag# begin_i-no end_i-no begin_job1 begin_job2 
          end_job1 end_job2 begin_cust end_cust rsReport rd-dest tb_excel 
          tb_runExcel fi_file sl_avail sl_selected
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_tag# end_tag# begin_i-no end_i-no begin_job1 
         begin_job2 end_job1 end_job2 begin_cust end_cust rsReport rd-dest 
         tb_excel tb_runExcel fi_file btn-ok btn-cancel sl_avail Btn_Def sl_selected Btn_Add
         Btn_Remove btn_Up btn_down
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE report-detail C-Win 
PROCEDURE report-detail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH tt-wiptag NO-LOCK
   BREAK BY tt-wiptag.job-no
         BY tt-wiptag.fg-i-no
         BY tt-wiptag.rm-tag-no
         BY tt-wiptag.seq:
         {custom/statusMsg.i " 'Processing Tag#  '  + tt-wiptag.tag-no "}
    /* On first-of new job, print job number. */
/*   IF FIRST-OF(tt-wiptag.job-no) THEN
        PUT UNFORMATTED tt-wiptag.job-no + "-" + STRING(tt-wiptag.job-no2,"99") FORMAT "x(10)" AT 1.


    /* On first of new item, print item. */
    IF FIRST-OF(tt-wiptag.fg-i-no) THEN DO:
        PUT UNFORMATTED     tt-wiptag.fg-i-no FORMAT "x(16)" AT 12.
/*         PUT UNFORMATTED tt-wiptag.fg-i-name FORMAT "x(30)" AT 30.  */
    END.

    /* On first of new tag, print tag. */
    IF FIRST-OF(tt-wiptag.rm-tag-no) THEN DO:
/*         PUT UNFORMATTED tt-wiptag.tag-no FORMAT "x(21)" AT 61.  */
        PUT UNFORMATTED tt-wiptag.rm-tag-no FORMAT "x(21)" AT 30.
        PUT UNFORMATTED tt-wiptag.t-sht-wid FORMAT ">>9.9999" AT 52.
        PUT UNFORMATTED tt-wiptag.t-sht-len FORMAT ">>9.9999" AT 65.
    END.

/*     ASSIGN v-qty = v-qty + tt-wiptag.produced-qty.  */

    /* Detail will show each Machine and the Quantity of Each machine. */
    IF FIRST-OF(seq) THEN DO:
        PUT UNFORMATTED tt-wiptag.m-code FORMAT "x(6)" AT 80.
        PUT UNFORMATTED string(tt-wiptag.seq) + "-" + tt-wiptag.dept-name FORMAT "x(15)" AT 90.
        PUT UNFORMATTED tt-wiptag.produced-qty FORMAT "->>>,>>>,>>9.9<<<<<" AT 108.
    END.*/

  /*  IF LAST-OF(tt-wiptag.rm-tag-no) THEN PUT SKIP(2).
    ELSE
        PUT SKIP. 

    IF tb_excel THEN DO:
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.job-no + "-" + STRING(tt-wiptag.job-no2,"99")  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.fg-i-no  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.rm-tag-no  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.t-sht-wid  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.t-sht-len  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.m-code  '",'.
        PUT STREAM excel UNFORMATTED '"' string(tt-wiptag.seq) + "-" + tt-wiptag.dept-name  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.produced-qty  '",'.
        PUT STREAM excel UNFORMATTED SKIP.
    END. */


    ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "" .

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "job"         THEN cVarValue =  IF FIRST-OF(tt-wiptag.job-no) THEN STRING(TRIM(tt-wiptag.job-no) + "-" + STRING(tt-wiptag.job-no2,"99"))  ELSE "".
                         WHEN "ino"        THEN cVarValue =  IF FIRST-OF(tt-wiptag.fg-i-no) THEN string(tt-wiptag.fg-i-no,"x(16)") ELSE "" .
                         WHEN "iname"       THEN cVarValue =  IF FIRST-OF(tt-wiptag.fg-i-no) THEN string(tt-wiptag.fg-i-name,"x(30)") ELSE "".
                         WHEN "rmtg"        THEN cVarValue = IF FIRST-OF(tt-wiptag.rm-tag-no) THEN string(tt-wiptag.rm-tag-no,"x(21)") ELSE "" .
                         WHEN "sh-wid"      THEN cVarValue =  IF FIRST-OF(tt-wiptag.rm-tag-no) THEN string(tt-wiptag.t-sht-wid,">>9.9999") ELSE "".
                         WHEN "sh-len"      THEN cVarValue =  IF FIRST-OF(tt-wiptag.rm-tag-no) THEN string(tt-wiptag.t-sht-len,">>9.9999") ELSE "" .
                         WHEN "mach"        THEN cVarValue = IF FIRST-OF(seq) THEN string(tt-wiptag.m-code,"x(6)") ELSE "".
                         WHEN "dept"        THEN cVarValue =  IF FIRST-OF(seq) THEN string(string(tt-wiptag.seq) + "-" + tt-wiptag.dept-name,"x(15)")  ELSE "".
                         WHEN "tag-qty"     THEN cVarValue =  IF FIRST-OF(seq) THEN string(tt-wiptag.produced-qty,"->>>,>>>,>>9.9<<<<<") ELSE "".

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            IF FIRST-OF(seq) THEN do:
            PUT UNFORMATTED cDisplay  .
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.
            END.

IF LAST-OF(tt-wiptag.rm-tag-no) THEN PUT SKIP(2).
    ELSE
        PUT SKIP. 

END. /* FOR EACH tt-wiptag */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE report-summary C-Win 
PROCEDURE report-summary :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH tt-wiptag NO-LOCK
   BREAK BY tt-wiptag.job-no
         BY tt-wiptag.fg-i-no
         BY tt-wiptag.rm-tag-no
         BY tt-wiptag.seq:
         {custom/statusMsg.i " 'Processing Tag#  '  + tt-wiptag.tag-no "}
    /* On first-of new job, print job number. */
   /* IF FIRST-OF(tt-wiptag.job-no) THEN
        PUT UNFORMATTED tt-wiptag.job-no + "-" + STRING(tt-wiptag.job-no2,"99") FORMAT "x(10)" AT 1.

    /* On first of new item, print item. */
    IF FIRST-OF(tt-wiptag.fg-i-no) THEN DO:
        PUT UNFORMATTED tt-wiptag.fg-i-no FORMAT "x(16)" AT 12.
/*         PUT UNFORMATTED tt-wiptag.fg-i-name FORMAT "x(30)" AT 30.  */
    END.

    /* On first of new tag, print tag. */
    IF FIRST-OF(tt-wiptag.rm-tag-no) THEN DO:
        PUT UNFORMATTED tt-wiptag.rm-tag-no FORMAT "x(21)" AT 30.
        PUT UNFORMATTED tt-wiptag.t-sht-wid FORMAT ">>9.9999" AT 52.
        PUT UNFORMATTED tt-wiptag.t-sht-len FORMAT ">>9.9999" AT 65.
    END.


    /* Summary shows Last Machine Qty. */
    IF LAST-OF(tt-wiptag.rm-tag-no) THEN DO:
        PUT UNFORMATTED tt-wiptag.m-code FORMAT "x(6)" AT 80.
        PUT UNFORMATTED string(tt-wiptag.seq) + "-" + tt-wiptag.dept-name FORMAT "x(15)" AT 90.
        PUT UNFORMATTED tt-wiptag.produced-qty FORMAT "->>>,>>>,>>9.9<<<<<" AT 108.
    END.


    IF LAST-OF(tt-wiptag.fg-i-no) THEN PUT SKIP(2).
/*     ELSE           */
/*         PUT SKIP.  */


    IF tb_excel AND LAST-OF(tt-wiptag.rm-tag-no) THEN DO:
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.job-no + "-" + STRING(tt-wiptag.job-no2,"99")  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.fg-i-no  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.rm-tag-no  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.t-sht-wid  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.t-sht-len  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.m-code  '",'.
        PUT STREAM excel UNFORMATTED '"' string(tt-wiptag.seq) + "-" + tt-wiptag.dept-name  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.produced-qty  '",'.
        PUT STREAM excel UNFORMATTED SKIP.
    END. */

    ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "" .

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "job"         THEN cVarValue =  STRING(TRIM(tt-wiptag.job-no) + "-" + STRING(tt-wiptag.job-no2,"99")) .
                         WHEN "ino"        THEN cVarValue =  string(tt-wiptag.fg-i-no,"x(16)")  .
                         WHEN "iname"       THEN cVarValue =  string(tt-wiptag.fg-i-name,"x(30)") .
                         WHEN "rmtg"        THEN cVarValue =  string(tt-wiptag.rm-tag-no,"x(21)") .
                         WHEN "sh-wid"      THEN cVarValue =  string(tt-wiptag.t-sht-wid,">>9.9999")  .
                         WHEN "sh-len"      THEN cVarValue =   string(tt-wiptag.t-sht-len,">>9.9999") .
                         WHEN "mach"        THEN cVarValue =   string(tt-wiptag.m-code,"x(6)") .
                         WHEN "dept"        THEN cVarValue =   string(string(tt-wiptag.seq) + "-" + tt-wiptag.dept-name,"x(15)") .
                         WHEN "tag-qty"     THEN cVarValue =  string(tt-wiptag.produced-qty,"->>>,>>>,>>9.9<<<<<") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            IF LAST-OF(tt-wiptag.rm-tag-no) THEN do:
            PUT UNFORMATTED cDisplay SKIP .
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.
            END.

             IF LAST-OF(tt-wiptag.fg-i-no) THEN PUT SKIP(2).

END. /* FOR EACH tt-wiptag */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE report-test C-Win 
PROCEDURE report-test :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


FOR EACH tt-wiptag NO-LOCK
   BREAK BY tt-wiptag.job-no
         BY tt-wiptag.fg-i-no
         BY tt-wiptag.rm-tag-no
         BY tt-wiptag.m-code
         BY tt-wiptag.seq:
         {custom/statusMsg.i " 'Processing Tag#  '  + tt-wiptag.tag-no "}
    /* On first-of new job, print job number. */
  /*  IF FIRST-OF(tt-wiptag.job-no) THEN
        PUT UNFORMATTED tt-wiptag.job-no + "-" + STRING(tt-wiptag.job-no2,"99") FORMAT "x(10)" AT 1.

    /* On first of new item, print item. */
    IF FIRST-OF(tt-wiptag.fg-i-no) THEN DO:
        PUT UNFORMATTED tt-wiptag.fg-i-no FORMAT "x(16)" AT 12.
/*         PUT UNFORMATTED tt-wiptag.fg-i-name FORMAT "x(30)" AT 30.  */
    END.

    /* On first of new tag, print tag. */
    IF FIRST-OF(tt-wiptag.rm-tag-no) THEN DO:
        PUT UNFORMATTED tt-wiptag.rm-tag-no FORMAT "x(21)" AT 31.
        PUT UNFORMATTED tt-wiptag.t-sht-wid FORMAT ">>9.9999" AT 52.
        PUT UNFORMATTED tt-wiptag.t-sht-len FORMAT ">>9.9999" AT 65.
    END.

/*     ASSIGN v-qty = v-qty + tt-wiptag.produced-qty.  */

    /* Detail will show each Machine and the Quantity of Each machine. */
    PUT UNFORMATTED tt-wiptag.m-code FORMAT "x(6)" AT 80.
    PUT UNFORMATTED string(tt-wiptag.seq) + "-" + tt-wiptag.dept-name FORMAT "x(15)" AT 90.
    PUT UNFORMATTED tt-wiptag.produced-qty FORMAT "->>>,>>>,>>9.9<<<<<" AT 108.


    IF LAST-OF(tt-wiptag.rm-tag-no) THEN PUT SKIP(2).
    ELSE
        PUT SKIP. 

    IF tb_excel THEN DO:
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.job-no + "-" + STRING(tt-wiptag.job-no2,"99")  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.fg-i-no  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.rm-tag-no  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.t-sht-wid  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.t-sht-len  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.m-code  '",'.
        PUT STREAM excel UNFORMATTED '"' string(tt-wiptag.seq) + "-" + tt-wiptag.dept-name  '",'.
        PUT STREAM excel UNFORMATTED '"' tt-wiptag.produced-qty  '",'.
        PUT STREAM excel UNFORMATTED SKIP.
    END.*/

    ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "" .

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "job"         THEN cVarValue =  STRING(TRIM(tt-wiptag.job-no) + "-" + STRING(tt-wiptag.job-no2,"99")) .
                         WHEN "ino"        THEN cVarValue =  string(tt-wiptag.fg-i-no,"x(16)")  .
                         WHEN "iname"       THEN cVarValue =  string(tt-wiptag.fg-i-name,"x(30)") .
                         WHEN "rmtg"        THEN cVarValue = string(tt-wiptag.rm-tag-no,"x(21)") .
                         WHEN "sh-wid"      THEN cVarValue =  string(tt-wiptag.t-sht-wid,">>9.9999") .
                         WHEN "sh-len"      THEN cVarValue =  string(tt-wiptag.t-sht-len,">>9.9999") .
                         WHEN "mach"        THEN cVarValue =  string(tt-wiptag.m-code,"x(6)") .
                         WHEN "dept"        THEN cVarValue =  string(string(tt-wiptag.seq) + "-" + tt-wiptag.dept-name,"x(15)") .
                         WHEN "tag-qty"     THEN cVarValue =  string(tt-wiptag.produced-qty,"->>>,>>>,>>9.9<<<<<") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay .
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.

    IF LAST-OF(tt-wiptag.rm-tag-no) THEN PUT SKIP(2).
    ELSE
        PUT SKIP. 

END.


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
/*{sys/form/r-topw.f}*/
{sys/form/r-top5DL3.f}
DEF VAR v-xlshead AS CHAR NO-UNDO.
DEF VAR v-page-brk AS CHAR FORMAT "x(132)" NO-UNDO.
DEF VAR v-qty           AS DECIMAL                NO-UNDO INIT 0.
DEF VAR v-subtot-count  LIKE wiptag.pallet-count  NO-UNDO.
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

ASSIGN
    str-tit4 = "" 
    str-tit5  =  "" 
    str-line  = ""
    excelheader = "" .

/*FORM HEADER 
  SKIP(1)
    v-page-brk
  SKIP(1)
 WITH FRAME r-top2 NO-BOX PAGE-TOP STREAM-IO WIDTH 185. */

ASSIGN str-tit2 = c-win:TITLE 
       {sys/inc/ctrtext.i str-tit2 112} .

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

        IF LOOKUP(ttRptSelected.TextList, "") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

DISPLAY "" WITH FRAME r-top.

/* PUT UNFORMATTED "1234567890123456789012345678901234567890123456789012345678901234567890" +  */
/*                 "12345678901234567890123456789012345678901234567890".                       */
/*PUT SKIP.

PUT UNFORMATTED "Job #"        AT 1.
PUT UNFORMATTED "FG Item #"    AT 12. /* 15 */
/* PUT UNFORMATTED "FG Name"      AT 30. /* 30 */  */
PUT UNFORMATTED "RM Tag #"     AT 30. /* 20 */
PUT UNFORMATTED "Sht Wid"      AT 52.
PUT UNFORMATTED "Sht Len"      AT 65.
PUT UNFORMATTED "Mach"         AT 80.
PUT UNFORMATTED "Dept"         AT 90.
PUT UNFORMATTED "Tag Qty"      AT 113.

PUT SKIP.

PUT UNFORMATTED "------ --"                      AT 1.  /* job */
PUT UNFORMATTED "---------------"                AT 12 /* item */.
/* PUT UNFORMATTED "------------------------------" AT 30. /* item name */  */
PUT UNFORMATTED "--------------------"           AT 30. /* tag*/
PUT UNFORMATTED "----------"                     AT 52.
PUT UNFORMATTED "----------"                     AT 65.
PUT UNFORMATTED "-----"                          AT 80. /* Machine */
PUT UNFORMATTED "---------------"                AT 90 /* dept */.
PUT UNFORMATTED "----------"                     AT 113.  /* Tag Qty */

PUT SKIP. */

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
 /*SSIGN v-xlshead = v-xlshead + "Job,FG Item #,RM Tag #,Sht Wid,Sht Len,Machine,Dept,Tag Qty".*/
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.


EMPTY TEMP-TABLE tt-wiptag.

FOR EACH wiptag NO-LOCK
  WHERE wiptag.company EQ cocode 
    AND wiptag.tag-no  GE begin_tag# 
    AND wiptag.tag-no  LE end_tag# 
    AND wiptag.fg-i-no GE begin_i-no 
    AND wiptag.fg-i-no LE end_i-no  
    AND wiptag.cust-no GE begin_cust
    AND wiptag.cust-no LE END_cust
    AND wiptag.job-no  GE SUBSTR(fjob-no,1,6)
    AND wiptag.job-no  LE SUBSTR(tjob-no,1,6)
    AND FILL(" ",6 - LENGTH(TRIM(wiptag.job-no))) +
             TRIM(wiptag.job-no) +
             STRING(wiptag.job-no2,"99")  GE fjob-no
    AND FILL(" ",6 - LENGTH(TRIM(wiptag.job-no))) +
             TRIM(wiptag.job-no) +
             STRING(wiptag.job-no2,"99")  LE tjob-no:
             {custom/statusMsg.i " 'Processing Tag#  '  + wiptag.tag-no "}
    FIND FIRST reftable NO-LOCK
      WHERE reftable.reftable = "WIPLEN" 
        AND reftable.company = wiptag.company 
        AND reftable.CODE = wiptag.tag-no USE-INDEX CODE NO-ERROR.

        FOR EACH wiptag-mch OF wiptag NO-LOCK:

            FIND mach NO-LOCK WHERE 
                 mach.company = cocode AND 
                 mach.m-code = wiptag-mch.m-code NO-ERROR.

            /* Create a record for each dept of machine. */
            DO i = 1 TO 4:

                FIND dept NO-LOCK where
                     dept.code = mach.dept[i] NO-ERROR.

                IF AVAIL dept THEN DO:

                    CREATE tt-wiptag.
                    BUFFER-COPY wiptag TO tt-wiptag NO-ERROR.
                    ASSIGN tt-wiptag.dept-code    = mach.dept[i]
                           tt-wiptag.seq          = dept.fc   /* mach.d-seq (same value) dept.seq (is zero) */
                           tt-wiptag.dept-name    = dept.dscr
                           tt-wiptag.t-sht-wid    = IF AVAIL reftable THEN reftable.val[1] ELSE 0
                           tt-wiptag.t-sht-len    = IF AVAIL reftable THEN reftable.val[2] ELSE 0
                           tt-wiptag.m-code       = mach.m-code
                           tt-wiptag.produced-qty = wiptag-mch.produced-qty.
                END.
            END.
        END. /* FOR EACH wiptag-mch OF wiptag */
END. /* FOR EACH wiptag */

IF Detail() THEN RUN report-detail.
ELSE 
    IF Summary() THEN RUN report-summary.
ELSE 
    RUN report-test.


RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-rpt-save C-Win 
PROCEDURE run-rpt-save :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{sys/form/r-topw.f}

DEF VAR v-xlshead AS CHAR NO-UNDO.
DEF VAR v-page-brk AS CHAR FORMAT "x(132)" NO-UNDO.
DEF VAR v-qty           AS DECIMAL                NO-UNDO INIT 0.
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

/* IF tb_excel THEN DO:                                                   */
/*                                                                        */
/*   OUTPUT STREAM excel TO VALUE(fi_file).                               */
/*                                                                        */
/*   IF tgl-tag                                                           */
/*     THEN ASSIGN v-xlshead = v-xlshead + "Tag #,".                      */
/*   IF tgl-rmbin                                                         */
/*     THEN ASSIGN v-xlshead = v-xlshead + "RM Bin,".                     */
/*   IF tgl-RMwhs                                                         */
/*     THEN ASSIGN v-xlshead = v-xlshead + "RM Whs,".                     */
/*   IF tgl-WIPwhs                                                        */
/*     THEN ASSIGN v-xlshead = v-xlshead + "WIP Whs,".                    */
/*   IF tgl-WIPBin                                                        */
/*     THEN ASSIGN v-xlshead = v-xlshead + "WIP Bin,".                    */
/*   IF tgl-job                                                           */
/*     THEN ASSIGN v-xlshead = v-xlshead + "Job #,,".                     */
/*   IF tgl-RMItem                                                        */
/*     THEN ASSIGN v-xlshead = v-xlshead + "RM Item #,".                  */
/*   IF tgl-RMName                                                        */
/*     THEN ASSIGN v-xlshead = v-xlshead + "RM Name,".                    */
/*   IF tgl-FGItem                                                        */
/*     THEN ASSIGN v-xlshead = v-xlshead + "FG Item #,".                  */
/*   IF tgl-FGName                                                        */
/*     THEN ASSIGN v-xlshead = v-xlshead + "FG Name,".                    */
/*   IF tgl-TagQty                                                        */
/*     THEN ASSIGN v-xlshead = v-xlshead + "Tag Qty,".                    */
/*   IF tgl-RMTag#                                                        */
/*     THEN ASSIGN v-xlshead = v-xlshead + "RM Tag #,".                   */
/*   IF tgl-ShWL                                                          */
/*     THEN ASSIGN v-xlshead = v-xlshead + "Sheet Width,Sheet Length," .  */
/*                                                                        */
/*   IF v-xlshead NE ""                                                   */
/*     THEN PUT STREAM excel UNFORMATTED v-xlshead SKIP.                  */
/*                                                                        */
/* END.                                                                   */

display "" with frame r-top.

/*T UNFORMATTED "1234567890123456789012345678901234567890123456789012345678901234567890" + 
                "123456789012345678901234567890123456789012345678901234567890".
PUT SKIP. 

/* PUT UNFORMATTED "Job #"        AT 1.  */
PUT UNFORMATTED "FG Item #"    AT 12. /* 15 */
/* PUT UNFORMATTED "FG Name"      AT 30. /* 30 */  */
PUT UNFORMATTED "RM Tag #"     AT 61. /* 20 */
PUT UNFORMATTED "Sht Wid"      AT 82.
PUT UNFORMATTED "Sht Len"      AT 94.
PUT UNFORMATTED "Mach"         AT 112.
PUT UNFORMATTED "Dept"         AT 120.
PUT UNFORMATTED "Tag Qty"      AT 148.
/* IF tgl-RMTag# THEN PUT UNFORMATTED "  RM Tag #        ".  */

PUT SKIP.

PUT UNFORMATTED "------ --"                      AT 1.  /* job */
PUT UNFORMATTED "---------------"                AT 12 /* item */.
/* PUT UNFORMATTED "------------------------------" AT 30. /* item name */  */
PUT UNFORMATTED "--------------------"           AT 61. /* tag*/
PUT UNFORMATTED "----------"                     AT 82.
PUT UNFORMATTED "----------"                     AT 94.
PUT UNFORMATTED "-----"                          AT 112. /* Machine */
PUT UNFORMATTED "---------------"                AT 120 /* dept */.
PUT UNFORMATTED "----------"                     AT 148.  /* Tag Qty */
/* /* IF tgl-RMTag# THEN PUT UNFORMATTED "-------------------- ".  */  */

PUT SKIP. */

EMPTY TEMP-TABLE tt-wiptag.

FOR EACH wiptag NO-LOCK
  WHERE wiptag.company EQ cocode 
    AND wiptag.tag-no  GE begin_tag# 
    AND wiptag.tag-no  LE end_tag# 
    AND wiptag.fg-i-no GE begin_i-no 
    AND wiptag.fg-i-no LE end_i-no  
    AND wiptag.cust-no GE begin_cust
    AND wiptag.cust-no LE END_cust
    AND wiptag.job-no  GE SUBSTR(fjob-no,1,6)
    AND wiptag.job-no  LE SUBSTR(tjob-no,1,6)
    AND FILL(" ",6 - LENGTH(TRIM(wiptag.job-no))) +
             TRIM(wiptag.job-no) +
             STRING(wiptag.job-no2,"99")  GE fjob-no
    AND FILL(" ",6 - LENGTH(TRIM(wiptag.job-no))) +
             TRIM(wiptag.job-no) +
             STRING(wiptag.job-no2,"99")  LE tjob-no:
             {custom/statusMsg.i " 'Processing Tag#  '  + wiptag.tag-no "}
    FIND FIRST reftable NO-LOCK
      WHERE reftable.reftable = "WIPLEN" 
        AND reftable.company = wiptag.company 
        AND reftable.CODE = wiptag.tag-no USE-INDEX CODE NO-ERROR.

        FOR EACH wiptag-mch OF wiptag NO-LOCK:

            FIND mach NO-LOCK WHERE 
                 mach.company = cocode AND 
                 mach.m-code = wiptag-mch.m-code NO-ERROR.

            /* Create a record for each dept of machine. */
            DO i = 1 TO 4:

                FIND dept NO-LOCK where
                     dept.code = mach.dept[i] NO-ERROR.

                IF AVAIL dept THEN DO:

                    CREATE tt-wiptag.
                    BUFFER-COPY wiptag TO tt-wiptag NO-ERROR.
                    ASSIGN tt-wiptag.dept-code    = mach.dept[i]
                           tt-wiptag.seq          = dept.fc   /* mach.d-seq (same value) dept.seq (is zero) */
                           tt-wiptag.dept-name    = dept.dscr
                           tt-wiptag.t-sht-wid    = IF AVAIL reftable THEN reftable.val[1] ELSE 0
                           tt-wiptag.t-sht-len    = IF AVAIL reftable THEN reftable.val[2] ELSE 0
                           tt-wiptag.m-code       = mach.m-code
                           tt-wiptag.produced-qty = wiptag-mch.produced-qty.
                END.
            END.
        END. /* FOR EACH wiptag-mch OF wiptag */
END. /* FOR EACH wiptag */


FOR EACH tt-wiptag NO-LOCK
   BREAK BY tt-wiptag.job-no
         BY tt-wiptag.fg-i-no
         BY tt-wiptag.rm-tag-no
         BY tt-wiptag.m-code
         BY tt-wiptag.seq:
         {custom/statusMsg.i " 'Processing Tag#  '  + tt-wiptag.tag-no "}
    /* On first-of new job, print job number. */
    IF FIRST-OF(tt-wiptag.job-no) THEN
        PUT UNFORMATTED tt-wiptag.job-no + "-" + STRING(tt-wiptag.job-no2,"99") FORMAT "x(10)" AT 1.

    /* On first of new item, print item. */
    IF FIRST-OF(tt-wiptag.fg-i-no) THEN DO:
        PUT UNFORMATTED tt-wiptag.fg-i-no FORMAT "x(16)" AT 12.
/*         PUT UNFORMATTED tt-wiptag.fg-i-name FORMAT "x(30)" AT 30.  */
    END.

    /* On first of new tag, print tag. */
    IF FIRST-OF(tt-wiptag.rm-tag-no) THEN DO:
/*         PUT UNFORMATTED tt-wiptag.tag-no FORMAT "x(21)" AT 61.  */
        PUT UNFORMATTED tt-wiptag.rm-tag-no FORMAT "x(21)" AT 61.
        PUT UNFORMATTED tt-wiptag.t-sht-wid FORMAT ">>9.9999" AT 82.
        PUT UNFORMATTED tt-wiptag.t-sht-len FORMAT ">>9.9999" AT 94.
    END.

/*     ASSIGN v-qty = v-qty + tt-wiptag.produced-qty.  */

    /* Detail will show each Machine and the Quantity of Each machine. */
    IF Detail() THEN DO:
        PUT UNFORMATTED tt-wiptag.m-code FORMAT "x(6)" AT 112.
        PUT UNFORMATTED string(tt-wiptag.seq) + "-" + tt-wiptag.dept-name FORMAT "x(15)" AT 120.
        PUT UNFORMATTED tt-wiptag.produced-qty FORMAT "->>>,>>>,>>9.9<<<<<" AT 140.
    /*     PUT UNFORMATTED tt-wiptag.dept-code FORMAT "x(4)".  */
    END.

    /* Summary shows Last Machine Qty. */
    IF LAST-OF(tt-wiptag.rm-tag-no) AND Summary() THEN DO:
        PUT UNFORMATTED tt-wiptag.m-code FORMAT "x(6)" AT 112.
        PUT UNFORMATTED string(tt-wiptag.seq) + "-" + tt-wiptag.dept-name FORMAT "x(15)" AT 120.
        PUT UNFORMATTED tt-wiptag.produced-qty FORMAT "->>>,>>>,>>9.9<<<<<" AT 140.
    END.


    IF LAST-OF(tt-wiptag.rm-tag-no) THEN PUT SKIP(2).
    ELSE
        PUT SKIP. 
END.


/*     IF tgl-SubTotShSz AND  */
/*        tgl-TagQty     AND  */
/*        tgl-ShWL       AND  */
/*        tgl-RMItem          */
/*       THEN DO:             */

/*         IF LAST-OF(tt-wiptag.t-sht-wid-len)                                   */
/*           THEN DO:                                                            */
/*                                                                               */
/*            PUT UNFORMATTED                                                    */
/*                SKIP(1) SPACE(40)                                              */
/*                                                                               */
/* /*                "Sheet Width And Length Total Qty:    " v-subtot-count. */  */
/*                "Sub Tot Qty of Sheets :  "                                    */
/*                v-subtot-count.                                                */
/*            PUT UNFORMATTED  SKIP(2).                                          */
/*                                                                               */
/*            ASSIGN v-subtot-count = 0.                                         */
/*         END.                                                                  */

/*     END.  */


/*     IF tb_excel THEN DO:                                                             */
/*                                                                                      */
/*       IF tgl-tag    THEN PUT STREAM excel UNFORMATTED '"' v-tag-no        '",'.      */
/*       IF tgl-rmbin  THEN PUT STREAM excel UNFORMATTED '"' v-rm-bin        '",'.      */
/*       IF tgl-RMwhs  THEN PUT STREAM excel UNFORMATTED '"' v-rm-whs        '",'.      */
/*       IF tgl-WIPwhs THEN PUT STREAM excel UNFORMATTED '"' v-wip-warehouse '",'.      */
/*       IF tgl-WIPBin THEN PUT STREAM excel UNFORMATTED '"' v-wip-rm-bin    '",'.      */
/*       IF tgl-job    THEN PUT STREAM excel UNFORMATTED '"' v-job-no        '",'       */
/*                                                       '"' v-job-no2       '",'.      */
/*       IF tgl-RMItem THEN PUT STREAM excel UNFORMATTED '"' v-rm-i-no       '",'.      */
/*       IF tgl-RMName THEN PUT STREAM excel UNFORMATTED '"' v-rm-i-name     '",'.      */
/*       IF tgl-FGItem THEN PUT STREAM excel UNFORMATTED '"' v-fg-i-no       '",'.      */
/*       IF tgl-FGName THEN PUT STREAM excel UNFORMATTED '"' v-fg-i-name     '",'.      */
/*       IF tgl-TagQty THEN PUT STREAM excel UNFORMATTED '"' v-pallet-count  '",'.      */
/*       IF tgl-RMTag# THEN PUT STREAM excel UNFORMATTED '"' v-rm-tag-no     '",'.      */
/*       IF tgl-ShWL   THEN PUT STREAM excel UNFORMATTED '"' tt-wiptag.t-sht-wid '",'.  */
/*       IF tgl-ShWL   THEN PUT STREAM excel UNFORMATTED '"' tt-wiptag.t-sht-len '",'.  */
/*                                                                                      */
/*       PUT STREAM excel UNFORMATTED SKIP.                                             */
/*                                                                                      */
/*     END.                                                                             */

/* END. /* for each wiptag */  */

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Detail C-Win 
FUNCTION Detail RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN rsReport:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Detail".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Summary C-Win 
FUNCTION Summary RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   RETURN rsReport:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Summary".

END FUNCTION.

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

