&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: pcrep\r-promac.w

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
DEFINE TEMP-TABLE tt-mch-srt NO-UNDO LIKE mch-srt 
    FIELD i-no LIKE mch-srt.job-no
    FIELD start-time AS INT
    FIELD start-date AS DATE .

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

DEF VAR v-print-fmt AS CHARACTER.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEFINE STREAM excel.

DEF STREAM excel.
DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "M Code,FG Item,Job #,Cust#,MR Std,MR Acl,MR Eff%,RUN Std,RUN Acl," +
                           "RUN Eff%,MR&RUN Std,MR&RUN Acl,MR&RUN Eff%,D/T Acl,D/T Eff%,Acl Qty,Exptd Qty,MR-C,RUNC," +
                           "Total Machine Hours,Total Labor Hours,Pieces per Hour,MSF,MSF per Hour,Number On," +
                           "Kicks per Hour,Pieces per Man Hour,MR Waste,Run Waste,Total Waste,% Waste" 
       cFieldListToSelect = "m-cod,ino,job,cust-no,mr-stn,mr-acl,mr-eff,run-stnd,run-acl," +
                            "run-eff,mr&-stnd,mr&-acl,mr&-eff,dt-acl,dt-eff,acl-qty,exp-qty,mr-comp,run-comp," +
                            "ttl-mch-hrs,ttl-lbr-hrs,pic-per-hrs,msf,msf-per-hrs,nbr-on," +
                            "kik-per-hrs,pic-per-man-hrs,mr-wst,run-wst,ttl-wst,%wst"
       cFieldLength = "6,15,10,8,8,8,8,8,8," + "8,10,10,11,8,8,11,11,4,4," + "19,17,15,9,12,10," + "14,19,9,9,11,9"
       cFieldType = "c,c,c,c,i,i,i,i,i," + "i,i,i,i,i,i,i,i,c,c," + "i,i,i,i,i,i," + "i,i,i,i,i,i"
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "M Code,FG Item,Job #,MR Std,MR Acl,MR Eff%,RUN Std,RUN Acl," +
                           "RUN Eff%,MR&RUN Std,MR&RUN Acl,MR&RUN Eff%,D/T Acl,D/T Eff%,Acl Qty,Exptd Qty"    .
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_dept end_dept begin_mach ~
end_mach begin_date end_date tb_sel-per tb_donotcarry sl_avail Btn_Def ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel rd-job-timedt
&Scoped-Define DISPLAYED-OBJECTS begin_dept end_dept begin_mach end_mach ~
begin_date end_date tb_sel-per tb_donotcarry sl_avail sl_selected rd-dest ~
lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel ~
tb_runExcel fi_file rd-job-timedt

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_dept AS CHARACTER FORMAT "X(4)" 
     LABEL "Beginning Department" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)" 
     LABEL "Beginning Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_dept AS CHARACTER FORMAT "X(4)" INITIAL "zzzz" 
     LABEL "Ending Department" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
     LABEL "Ending Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-promac.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE lbl_print AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd-job-timedt AS CHARACTER INITIAL "1" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Job Number", "1",
"Job Date and Time", "2"
     SIZE 40 BY .95 NO-UNDO.

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
     SIZE 95 BY 8.81.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 7.62.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_donotcarry AS LOGICAL INITIAL no 
     LABEL "Zero Standard MR from Previous Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_sel-per AS LOGICAL INITIAL NO 
     LABEL "Summarize?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_dept AT ROW 2.52 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Department"
     end_dept AT ROW 2.52 COL 68 COLON-ALIGNED HELP
          "Enter Ending Department"
     begin_mach AT ROW 3.48 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Machine"
     end_mach AT ROW 3.48 COL 68 COLON-ALIGNED HELP
          "Enter Ending Machine"
     begin_date AT ROW 4.43 COL 25 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 4.43 COL 68 COLON-ALIGNED HELP
          "Enter Ending Date"
     tb_sel-per AT ROW 5.54 COL 30
     tb_donotcarry AT ROW 6.50 COL 30 WIDGET-ID 2
     rd-job-timedt AT ROW 7.50 COL 30 NO-LABEL
     sl_avail AT ROW 9.48 COL 5.2 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 9.48 COL 41.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 9.48 COL 60.6 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 10.48 COL 41.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 11.48 COL 41.2 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 12.52 COL 41.2 WIDGET-ID 40
     btn_down AT ROW 13.52 COL 41.2 WIDGET-ID 42
     rd-dest AT ROW 15.76 COL 6 NO-LABEL
     lv-ornt AT ROW 16.48 COL 31 NO-LABEL
     lines-per-page AT ROW 16.48 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 18.14 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 19.33 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 21.24 COL 28
     tb_excel AT ROW 21.24 COL 70.2 RIGHT-ALIGNED
     tb_runExcel AT ROW 21.24 COL 94 RIGHT-ALIGNED
     fi_file AT ROW 22.33 COL 48 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 24.19 COL 22
     btn-cancel AT ROW 24.19 COL 60
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 8.76 COL 60.6 WIDGET-ID 44
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.05 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 8.76 COL 6 WIDGET-ID 38
     RECT-6 AT ROW 14.81 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 96.8 BY 25.24.


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
         TITLE              = "Productivity By Machine (D-R-4)"
         HEIGHT             = 25.48
         WIDTH              = 98
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
   FRAME-NAME                                                           */
ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_mach:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */

ASSIGN 
       tb_donotcarry:PRIVATE-DATA IN FRAME FRAME-A     = 
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

ASSIGN 
       tb_sel-per:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Productivity By Machine */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Productivity By Machine */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
  SESSION:SET-WAIT-STATE ("").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= ''
                            &begin_cust=begin_mach
                            &END_cust= begin_mach
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = ''
                             &begin_cust= begin_mach
                             &END_cust=begin_mach
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust= begin_mach
                                  &END_cust=begin_mach
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }
     END.
       END.
       WHEN 6 THEN RUN OUTPUT-to-port.

  end case. 
     {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down C-Win
ON CHOOSE OF btn_down IN FRAME FRAME-A /* Move Down */
DO:
  RUN Move-Field ("Down").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
DO:
  RUN Move-Field ("Up").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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

&Scoped-define SELF-NAME rd-job-timedt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-job-timedt C-Win
ON LEAVE OF rd-job-timedt IN FRAME FRAME-A
DO:
  ASSIGN rd-job-timedt.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-job-timedt C-Win
ON VALUE-CHANGED OF rd-job-timedt IN FRAME FRAME-A
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

&Scoped-define SELF-NAME tb_donotcarry
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_donotcarry C-Win
ON VALUE-CHANGED OF tb_donotcarry IN FRAME FRAME-A /* Zero Standard MR from Previous Date? */
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


&Scoped-define SELF-NAME tb_sel-per
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sel-per C-Win
ON VALUE-CHANGED OF tb_sel-per IN FRAME FRAME-A /* Show Job detail For Selected Period? */
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


  assign
   begin_date = date(month(TODAY), 1, year(TODAY))
   end_date   = today.

  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_dept.
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
  DISPLAY begin_dept end_dept begin_mach end_mach begin_date end_date tb_sel-per 
          tb_donotcarry sl_avail sl_selected rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file rd-job-timedt
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_dept end_dept begin_mach end_mach begin_date 
         end_date tb_sel-per tb_donotcarry sl_avail Btn_Def sl_selected Btn_Add 
         Btn_Remove btn_Up btn_down rd-dest lv-ornt lines-per-page lv-font-no 
         td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel rd-job-timedt
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
/*     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY.  */

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
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ pc/rep/mch-prd.p 8/94 gb */
/* Production by Department/Machine Report                               */
/* -------------------------------------------------------------------------- */
/*{sys/form/r-topw.f}*/

DEF BUFFER b-mch-srt FOR mch-srt.
DEF BUFFER bf-mch-act FOR mch-act .

def var v-date as date extent 2 format "99/99/9999" no-undo.
def var v-dept as ch format 'x(4)' extent 2 init [" ","zzzz"].
def var v-mach like mach.m-code extent 2 init [" ","zzzzzz"].
def var v-show as logical format "Y/N" init yes no-undo.
def var v-prt-job as logical format "Job/FG" init yes no-undo.
def var v-prt-both as logical format "Both/FG" INIT NO  no-undo.

def var job-mr-std as dec format "->>>9.9" no-undo.
def var job-run-std as dec format "->>>9.9" no-undo.
def var job-mr-act as dec format "->>>9.9" no-undo.
def var job-run-act as dec format "->>>9.9" no-undo.
def var job-dt-act as dec format "->>>9.9" no-undo.
def var job-qty-prod as dec format "->>,>>>,>>9" no-undo.
def var job-qty-expect as dec format "->>,>>>,>>9" no-undo.
def var mch-mr-std as dec format "->>>9.9" no-undo.
def var mch-run-std as dec format "->>>9.9" no-undo.
def var mch-mr-act as dec format "->>>9.9" no-undo.
def var mch-run-act as dec format "->>>9.9" no-undo.
def var mch-dt-act as dec format "->>>9.9" no-undo.
def var mch-qty-prod as dec format "->>,>>>,>>9" no-undo.
def var mch-qty-expect as dec format "->>,>>>,>>9" no-undo.
def var dpt-mr-std as dec format "->>>9.9" no-undo.
def var dpt-run-std as dec format "->>>9.9" no-undo.
def var dpt-mr-act as dec format "->>>9.9" no-undo.
def var dpt-run-act as dec format "->>>9.9" no-undo.
def var dpt-dt-act as dec format "->>>9.9" no-undo.
def var dpt-qty-prod as dec format "->>,>>>,>>9" no-undo.
def var dpt-qty-expect as dec format "->>,>>>,>>9" no-undo.
def var mr-eff as dec format "->>>>9.9" no-undo.
def var run-eff as dec format "->>>>9.9" no-undo.
def var tot-eff as dec format "->>>>9.9" no-undo.
def var dt-eff as dec format "->>>>9.9" no-undo.
def var tot-std-hrs as dec format "->>>9.9" no-undo.
def var tot-act-hrs as dec format "->>>9.9" no-undo.
def var a as char no-undo.
def var b as char no-undo.
def var hdr-tit as char no-undo.
def var hdr-tit2 as char no-undo.
def var hdr-tit3 as char no-undo.

DEF VAR v-mrcomp AS CHAR NO-UNDO.
DEF VAR v-runcomp AS CHAR NO-UNDO .
DEF VAR v-mrwaste AS DEC NO-UNDO .
DEF VAR v-runwaste AS DEC NO-UNDO .

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
DEF VAR str-line AS cha FORM "x(350)" NO-UNDO.
{sys/form/r-top5DL3.f} 
DEF VAR excelheader AS CHAR NO-UNDO.
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF VAR v-calmsf as dec format ">>>>>>.99" NO-UNDO .
DEF VAR v-num-up AS INT FORMAT ">>>,>>9" NO-UNDO .
DEF VAR v-act-lab-cost AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-pic-per-hrs AS DEC NO-UNDO .
DEF VAR v-msf-per-hrs AS DEC NO-UNDO .
DEF VAR v-kik-per-hrs AS DEC NO-UNDO .
DEF VAR v-wst AS DEC NO-UNDO .
DEF VAR v-per-man-hrs AS DEC NO-UNDO .
DEF VAR v-cust-no AS CHAR NO-UNDO .

/*FORM HEADER
     hdr-tit format "x(142)" skip
     hdr-tit2 format "x(142)" skip
     hdr-tit3 format "x(142)"

    WITH FRAME r-top WIDTH 180.*/


SESSION:SET-WAIT-STATE ("general").
/*ASSIGN
    v-prt-job   = rd_print EQ "Job#"
    v-prt-both  = rd_print EQ "Both"  . */

/*IF tb_excel THEN DO:
   IF v-prt-both THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   EXPORT STREAM excel DELIMITER ","
       "Mach. Code"
       "FG Item"
       "Job #"
       "MR Stndrd"
       "MR Actual"
       "MR Eff%"
       "RUN Stndrd"
       "RUN Actual"
       "RUN Eff%"
       "MR&RUN Stndrd"
       "MR&RUN Actual"
       "MR&RUN Eff%"
       "D/T Actual"
       "D/T Eff%"
       "Actual Qty"
       "Expected Qty"
       SKIP.
   END.

   IF NOT v-prt-both THEN DO:
    IF v-prt-job THEN do:
    OUTPUT STREAM excel TO VALUE(fi_file).
    EXPORT STREAM excel DELIMITER ","
       "Mach. Code"
       "Job #"
       "MR Stndrd"
       "MR Actual"
       "MR Eff%"
       "RUN Stndrd"
       "RUN Actual"
       "RUN Eff%"
       "MR&RUN Stndrd"
       "MR&RUN Actual"
       "MR&RUN Eff%"
       "D/T Actual"
       "D/T Eff%"
       "Actual Qty"
       "Expected Qty"
       SKIP.
    END.
    ELSE do:
    OUTPUT STREAM excel TO VALUE(fi_file).
    EXPORT STREAM excel DELIMITER ","
       "Mach. Code"
       "FG Item"
       "MR Stndrd"
       "MR Actual"
       "MR Eff%"
       "RUN Stndrd"
       "RUN Actual"
       "RUN Eff%"
       "MR&RUN Stndrd"
       "MR&RUN Actual"
       "MR&RUN Eff%"
       "D/T Actual"
       "D/T Eff%"
       "Actual Qty"
       "Expected Qty"
       SKIP.
    END.
   END.

END. */
assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-dept[1]   = begin_dept
 v-dept[2]   = end_dept
 v-mach[1]   = begin_mach
 v-mach[2]   = end_mach
 v-date[1]   = begin_date
 v-date[2]   = end_date
 v-show      = NOT tb_sel-per .

 /*hdr-tit  = "MACH                    <---MAKE READY HOURS--->  " +
            "<------RUN HOURS------->  <----MR & RUN HOURS---->  " +
            "<--D/T HOURS-->       ACTUAL    EXPECTED"
 hdr-tit2 = "CODE   " +
            (if v-prt-job then "    JOB #      " else "FG ITEM#       ") +
            "   STNDRD  ACTUAL   EFF %   " +
            " STNDRD  ACTUAL   EFF %    STNDRD  ACTUAL   EFF %   " +
            " ACTUAL   EFF %     QUANTITY    QUANTITY"
 hdr-tit3 = fill("-", 142). */

 DEF VAR cslist AS cha NO-UNDO.
 FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

     IF LOOKUP(ttRptSelected.TextList, "MR Std,MR Acl,MR Eff%,RUN Std,RUN Acl,RUN Eff%,MR&RUN Std,MR&RUN Acl,MR&RUN Eff%,D/T Acl,D/T Eff%,Acl Qty,Exptd Qty" ) <> 0    THEN
      ASSIGN
      str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
     ELSE
      str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 

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

 END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display "" with frame r-top.

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.


  for each mch-srt:
      delete mch-srt.
  end.
  FOR EACH tt-mch-srt:
      DELETE tt-mch-srt.
  END.

  for each mch-act where
      mch-act.company eq cocode and
      mch-act.m-code  ge v-mach[1] and
      mch-act.m-code  le v-mach[2] and
      mch-act.op-date ge v-date[1] and
      mch-act.op-date le v-date[2] and
      mch-act.dept    ge v-dept[1] and
      mch-act.dept    le v-dept[2]
      use-index dly-idx
      no-lock,
      first mach where
            mach.company eq cocode and
            mach.loc     eq locode and
            mach.m-code  eq mch-act.m-code
            no-lock:
            {custom/statusMsg.i " 'Processing Machine#  '  + mch-act.m-code "}             
     find first mch-srt where
         mch-srt.dept eq mch-act.dept and
         mch-srt.m-code eq mch-act.m-code and
         mch-srt.job-no eq mch-act.job-no and
         mch-srt.job-no2 eq mch-act.job-no2 and
         mch-srt.frm eq mch-act.frm and
         (mch-srt.blank-no eq mch-act.blank-no or
          mach.p-type ne "B" OR
          mch-act.blank-no EQ 0) and
         mch-srt.pass eq mch-act.pass
         no-error.
     if not available mch-srt then
     do:
        create mch-srt.
        assign mch-srt.dept     = mch-act.dept
               mch-srt.m-code   = mch-act.m-code
               mch-srt.job      = mch-act.job
               mch-srt.job-no   = mch-act.job-no
               mch-srt.job-no2  = mch-act.job-no2
               mch-srt.frm      = mch-act.frm
               mch-srt.blank-no = IF mach.p-type EQ "B"    AND
                                     mch-act.blank-no EQ 0 THEN 1
                                                           ELSE mch-act.blank-no
               mch-srt.pass     = mch-act.pass.

     END.
     find job-code where job-code.code eq mch-act.code
                         no-lock no-error.
     if not available job-code then next.
     if job-code.cat eq "RUN" then
     do:
        mch-srt.run-act-hr = mch-srt.run-act-hr + mch-act.hours.
        mch-srt.qty-prod   = mch-srt.qty-prod +
                             IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty.
     end.
     else if job-code.cat eq "MR" then
        mch-srt.mr-act-hr  = mch-srt.mr-act-hr + mch-act.hours.
     else
        mch-srt.act-dt-hr  = mch-srt.act-dt-hr + mch-act.hours.
  /*   if job-code.cat eq "RUN" then
             do:
             v-runwaste = v-runwaste + mch-act.waste .
         end.
         else if job-code.cat eq "MR" then
             v-mrwaste = v-mrwaste + mch-act.waste . */
  end.

    ASSIGN
    v-pic-per-hrs = 0
    v-msf-per-hrs = 0
    v-kik-per-hrs = 0
    v-wst = 0
    v-per-man-hrs =  0.

  for each mch-srt,

    first job where   /* CTS added join */
      job.company eq cocode and
      job.job-no  eq mch-srt.job-no and
      job.job-no2 eq mch-srt.job-no2 no-lock,
      first mach FIELDS(p-type) where
            mach.company eq cocode and
            mach.loc     eq locode and
            mach.m-code  eq mch-srt.m-code
            no-lock:

      {custom/statusMsg.i " 'Processing Machine#  '  + mch-srt.m-code "}



     find first job-mch where job-mch.company  = cocode and
                              job-mch.job      eq mch-srt.job and
                              job-mch.job-no  eq mch-srt.job-no and
                              job-mch.job-no2 eq mch-srt.job-no2 and
                              job-mch.frm      = mch-srt.frm and
                              (job-mch.blank-no = mch-srt.blank-no or
                               mch-srt.blank-no = 0) and
                              job-mch.m-code   = mch-srt.m-code and
                              job-mch.pass     = mch-srt.pass
                              no-lock no-error.
     if not avail job-mch then
     find first job-mch where job-mch.company eq cocode and
                              job-mch.job      eq mch-srt.job and
                              job-mch.job-no  eq mch-srt.job-no and
                              job-mch.job-no2 eq mch-srt.job-no2 and
                              job-mch.frm      eq mch-srt.frm and
                              (job-mch.blank-no = mch-srt.blank-no or
                               mch-srt.blank-no = 0) and
                              job-mch.m-code   eq mch-srt.m-code
                              no-lock no-error.
     if not avail job-mch then
     find first job-mch where job-mch.company eq cocode and
                              job-mch.job     eq mch-srt.job and
                              job-mch.job-no  eq mch-srt.job-no and
                              job-mch.job-no2 eq mch-srt.job-no2 and
                              job-mch.frm     eq mch-srt.frm and
                              job-mch.m-code  eq mch-srt.m-code and
                              job-mch.speed   ne 0
                              no-lock no-error.
     if not avail job-mch then
     find first job-mch where job-mch.company eq cocode and
                              job-mch.job     eq mch-srt.job and
                              job-mch.job-no  eq mch-srt.job-no and
                              job-mch.job-no2 eq mch-srt.job-no2 and
                              job-mch.frm     eq mch-srt.frm and
                              job-mch.m-code  eq mch-srt.m-code
                              no-lock no-error.
     if available job-mch then
     DO:
        IF mch-srt.qty-prod NE 0 then
        DO:
           IF CAN-FIND(FIRST mach WHERE
              mach.company EQ cocode AND
              mach.loc     EQ locode AND
              mach.m-code  EQ job-mch.m-code AND
              mach.therm   EQ YES AND
              (mach.p-type EQ "R" OR mach.dept[1] EQ "LM")) THEN
              FOR EACH job-mat FIELDS(i-no len) WHERE
                  job-mat.company eq cocode AND
                  job-mat.job = mch-srt.job AND
                  job-mat.frm EQ mch-srt.frm AND
                  job-mat.frm GT 0 AND
                  job-mat.len GT 0
                  no-lock,
                  first ITEM FIELDS(mat-type) WHERE
                        item.company eq cocode AND
                        item.i-no eq job-mat.i-no
                        no-lock

                  BREAK BY job-mat.frm
                        BY item.mat-type
                        BY job-mat.j-no
                        BY job-mat.rec_key:

                  mch-srt.run-std-hr = (mch-srt.qty-prod * job-mat.len / 12) / job-mch.speed.
                  LEAVE.
              END.
           ELSE
              ASSIGN mch-srt.run-std-hr = mch-srt.qty-prod / job-mch.speed.
        END.
        ELSE
           ASSIGN mch-srt.run-std-hr = job-mch.run-hr.

        IF NOT(tb_donotcarry AND
           CAN-FIND(FIRST mch-act WHERE
           mch-act.company EQ job-mch.company AND
           mch-act.dept EQ mch-srt.dept AND
           mch-act.m-code EQ mch-srt.m-code AND
           mch-act.job EQ mch-srt.job AND 
           mch-act.job-no EQ mch-srt.job-no AND
           mch-act.job-no2 EQ mch-srt.job-no2 AND
           mch-act.frm EQ mch-srt.frm AND
           (mch-srt.blank-no = IF mach.p-type EQ "B"    AND
                                  mch-act.blank-no EQ 0 THEN 1
                               ELSE mch-act.blank-no) AND
            mch-act.pass EQ mch-srt.pass AND
            mch-act.op-date LT begin_date)) THEN
           mch-srt.mr-std-hr  = job-mch.mr-hr.

        mch-srt.qty-expect = IF job-mch.speed NE 0 THEN
                               (IF mch-srt.run-act-hr NE 0
                                THEN mch-srt.run-act-hr
                                ELSE mch-srt.run-std-hr) * job-mch.speed
                             ELSE job-mch.run-qty.
     END.



     {sys/inc/roundup.i mch-srt.qty-expect}

     IF mch-srt.run-std-hr EQ ? THEN mch-srt.run-std-hr = 0.

     a = fill(" ",6 - length(trim(mch-srt.job-no))) +
         trim(mch-srt.job-no) + "-" +
         string(mch-srt.job-no2,"99").


   /*  if not v-prt-job AND NOT v-prt-both then
     for each job-hdr
         where job-hdr.company eq cocode
           and job-hdr.job     eq mch-srt.job
           and job-hdr.job-no  eq mch-srt.job-no
           and job-hdr.job-no2 eq mch-srt.job-no2
         no-lock

         by job-hdr.blank-no desc
         by job-hdr.frm      desc:

       a = job-hdr.i-no.

       if job-hdr.frm       eq mch-srt.frm           and
          (job-hdr.blank-no eq mch-srt.blank-no or
           job-hdr.blank-no eq 0)                    then leave.
     end. */

   /*  IF v-prt-both THEN */

     for each job-hdr
         where job-hdr.company eq cocode
           and job-hdr.job     eq mch-srt.job
           and job-hdr.job-no  eq mch-srt.job-no
           and job-hdr.job-no2 eq mch-srt.job-no2
         no-lock

         by job-hdr.blank-no desc
         by job-hdr.frm      desc:

       b = job-hdr.i-no.
       if job-hdr.frm       eq mch-srt.frm           and
          (job-hdr.blank-no eq mch-srt.blank-no or
           job-hdr.blank-no eq 0)                    then leave.
     end.

       mch-srt.job-no = a.
       CREATE tt-mch-srt .
       BUFFER-COPY mch-srt TO tt-mch-srt .
       ASSIGN
        tt-mch-srt.i-no = b
/*         tt-mch-srt.job-no = FILL(" ",6 - LENGTH(TRIM(tt-mch-srt.job-no))) + TRIM(tt-mch-srt.job-no) */
           .
       IF rd-job-timedt EQ "2" THEN do:  
           IF AVAIL job-mch THEN
               ASSIGN
               tt-mch-srt.start-date = job-mch.start-date
               tt-mch-srt.start-time = job-mch.start-time . 
       END. 
  end.

    put skip.

    ASSIGN 
    v-mrcomp = "" 
    v-runcomp = "" 
    v-calmsf = 0
    v-num-up = 0 
    v-act-lab-cost = 0 . 

     for each tt-mch-srt use-index dept-idx
                      break by tt-mch-srt.dept
                            by tt-mch-srt.m-code
                            BY tt-mch-srt.start-date
                            BY tt-mch-srt.start-time
                            BY tt-mch-srt.job-no :

         {custom/statusMsg.i " 'Processing Machine#  '  + tt-mch-srt.m-code "}

         find first itemfg
                      where itemfg.company eq cocode
                        and itemfg.i-no    eq tt-mch-srt.i-no
                      no-lock no-error.

                 v-calmsf = /*v-calmsf
                                     +*/ (tt-mch-srt.qty-prod *
                                        (if avail itemfg then itemfg.t-sqft
                                         else 1) / 1000).

        FIND FIRST job where 
            job.company eq cocode and
            job.job-no  eq SUBSTRING(tt-mch-srt.job-no,1,6) and
            job.job-no2 eq tt-mch-srt.job-no2 NO-LOCK NO-ERROR .
         ASSIGN v-cust-no = "" .
         IF AVAIL job THEN do:

             FIND FIRST eb WHERE eb.company EQ cocode
             AND eb.est-no EQ job.est-no
             AND eb.stock-no EQ tt-mch-srt.i-no NO-LOCK NO-ERROR .
             IF AVAIL eb THEN
                 ASSIGN v-num-up = eb.num-up .
             ELSE 
                 v-num-up = 0 .

                 for each misc-act FIELDS(cost)
                     where misc-act.company eq cocode
                     and misc-act.job     eq job.job
                     no-lock:

                     /*v-act-lab-cost = v-act-lab-cost + misc-act.cost.*/

                 END.
                 FIND FIRST job-hdr
                     where job-hdr.company eq cocode
                     and job-hdr.i-no     eq tt-mch-srt.i-no
                     and job-hdr.job-no  eq job.job-no
                     and job-hdr.job-no2 eq job.job-no2 NO-LOCK NO-ERROR .
                 IF AVAIL job-hdr THEN
                     v-cust-no = job-hdr.cust-no  .

         END.

     find first job-mch where job-mch.company  = cocode  and
                              job-mch.job      eq tt-mch-srt.job  and 
                              job-mch.job-no  EQ SUBSTRING(tt-mch-srt.job-no,1,6)  and
                              job-mch.job-no2 eq tt-mch-srt.job-no2  and 
                              job-mch.frm      = tt-mch-srt.frm and 
                              (job-mch.blank-no = tt-mch-srt.blank-no or
                               mch-srt.blank-no = 0) and              
                              job-mch.m-code   = tt-mch-srt.m-code and 
                              job-mch.pass     = tt-mch-srt.pass 
                              no-lock no-error.
     if not avail job-mch then
     find first job-mch where job-mch.company eq cocode and
                              job-mch.job      eq tt-mch-srt.job and
                              job-mch.job-no  eq SUBSTRING(tt-mch-srt.job-no,1,6) and
                              job-mch.job-no2 eq tt-mch-srt.job-no2 and 
                              job-mch.frm      eq tt-mch-srt.frm and
                              (job-mch.blank-no = tt-mch-srt.blank-no or
                               mch-srt.blank-no = 0) and
                              job-mch.m-code   eq tt-mch-srt.m-code
                              no-lock no-error.
     if not avail job-mch then
     find first job-mch where job-mch.company eq cocode and
                              job-mch.job     eq tt-mch-srt.job and
                              job-mch.job-no  eq SUBSTRING(tt-mch-srt.job-no,1,6) and
                              job-mch.job-no2 eq tt-mch-srt.job-no2 and 
                              job-mch.frm     eq tt-mch-srt.frm and
                              job-mch.m-code  eq tt-mch-srt.m-code and
                              job-mch.speed   ne 0
                              no-lock no-error.
     if not avail job-mch then
     find first job-mch where job-mch.company eq cocode and
                              job-mch.job     eq tt-mch-srt.job and 
                              job-mch.job-no  eq SUBSTRING(tt-mch-srt.job-no,1,6)  and
                              job-mch.job-no2 eq tt-mch-srt.job-no2 and  
                              job-mch.frm     eq tt-mch-srt.frm and
                              job-mch.m-code  eq tt-mch-srt.m-code        
                              no-lock no-error.  
     if available job-mch then
     DO:
        ASSIGN                                   
            v-mrcomp = string(job-mch.mr-complete)
            v-runcomp = string(job-mch.run-complete) .

     END.

     ASSIGN v-runwaste = 0 
            v-mrwaste = 0 
            v-act-lab-cost = 0. 
     FOR EACH bf-mch-act WHERE
           bf-mch-act.company EQ cocode AND
           bf-mch-act.dept EQ tt-mch-srt.dept AND
           bf-mch-act.m-code EQ tt-mch-srt.m-code AND
           bf-mch-act.job EQ tt-mch-srt.job AND 
           bf-mch-act.job-no EQ SUBSTRING(tt-mch-srt.job-no,1,6) AND
           bf-mch-act.job-no2 EQ tt-mch-srt.job-no2 AND
           bf-mch-act.frm EQ tt-mch-srt.frm AND
           (bf-mch-act.blank-no = tt-mch-srt.blank-no  OR
                                               mach.p-type ne "B" OR
                                               bf-mch-act.blank-no EQ 0) AND
           bf-mch-act.pass eq tt-mch-srt.pass  NO-LOCK:

         find job-code where job-code.code eq bf-mch-act.CODE no-lock no-error.

         if not available job-code then next.
         if job-code.cat eq "RUN" then
             do:
             v-runwaste = v-runwaste + bf-mch-act.waste .
         end.
         else if job-code.cat eq "MR" then
             v-mrwaste = v-mrwaste + bf-mch-act.waste .

            v-act-lab-cost = v-act-lab-cost + (bf-mch-act.hours * bf-mch-act.crew) .

     END. /* FOR EACH bf-mch-act W */

       assign job-mr-std     = job-mr-std + tt-mch-srt.mr-std-hr
              job-run-std    = job-run-std + tt-mch-srt.run-std-hr
              job-mr-act     = job-mr-act + tt-mch-srt.mr-act-hr
              job-run-act    = job-run-act + tt-mch-srt.run-act-hr
              job-dt-act     = job-dt-act + tt-mch-srt.act-dt-hr
              job-qty-prod   = job-qty-prod + tt-mch-srt.qty-prod
              job-qty-expect = job-qty-expect + tt-mch-srt.qty-expect.

       IF LAST-OF(tt-mch-srt.job-no) THEN DO:
         IF job-run-act EQ 0 THEN
           job-run-std = 0.
         if v-show then do:
           mr-eff  = (job-mr-std  / job-mr-act)  * 100.00.
           if mr-eff eq ? then mr-eff = 0.
           run-eff = (job-run-std / job-run-act) * 100.00.
           if run-eff eq ? then run-eff = 0.
           ASSIGN
              tot-std-hrs = job-mr-std + job-run-std
              tot-act-hrs = job-mr-act + job-run-act
              tot-eff = (tot-std-hrs / tot-act-hrs) * 100.00.
           if tot-eff eq ? then tot-eff = 0.
           dt-eff = (job-dt-act / tot-act-hrs) * 100.00.
           if dt-eff eq ? then dt-eff = 0.

           IF v-num-up = 0 THEN ASSIGN v-num-up = 1 .
           ASSIGN
               v-pic-per-hrs = (job-qty-prod / (job-mr-act + job-run-act + job-dt-act))
               v-msf-per-hrs = (v-calmsf / (job-mr-act + job-run-act + job-dt-act))
               v-kik-per-hrs = ( /*job-qty-prod*/ v-pic-per-hrs  / v-num-up)
               v-wst = ((v-mrwaste + v-runwaste) / ( job-qty-prod)) * 100
               v-per-man-hrs = (job-qty-prod / v-act-lab-cost) .


            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "m-cod"      THEN cVarValue = string(tt-mch-srt.m-code) .
                         WHEN "ino"        THEN cVarValue = string(tt-mch-srt.i-no) .  
                         WHEN "job"        THEN cVarValue = string(tt-mch-srt.job-no) .
                         WHEN "cust-no"    THEN cVarValue = IF v-cust-no <> ? THEN string(v-cust-no) ELSE "" .
                         WHEN "mr-stn"     THEN cVarValue = string(job-mr-std,"->>>9.9") .  
                         WHEN "mr-acl"     THEN cVarValue = string(job-mr-act,"->>>9.9") .       
                         WHEN "mr-eff"     THEN cVarValue = string(mr-eff,"->>>>9.9") .           
                         WHEN "run-stnd"   THEN cVarValue = string(job-run-std,"->>>9.9") .      
                         WHEN "run-acl"    THEN cVarValue = string(job-run-act,"->>>9.9") .      
                         WHEN "run-eff"    THEN cVarValue = string(run-eff,"->>>>9.9") .          
                         WHEN "mr&-stnd"   THEN cVarValue = string(tot-std-hrs,"->>>9.9") .      
                         WHEN "mr&-acl"    THEN cVarValue = string(tot-act-hrs,"->>>9.9") .      
                         WHEN "mr&-eff"    THEN cVarValue = string(tot-eff,"->>>>9.9") .          
                         WHEN "dt-acl"     THEN cVarValue = string(job-dt-act,"->>>9.9") .       
                         WHEN "dt-eff"     THEN cVarValue = string(dt-eff,"->>>>9.9") .           
                         WHEN "acl-qty"    THEN cVarValue = string(job-qty-prod,"->>,>>>,>>9") .     
                         WHEN "exp-qty"    THEN cVarValue = string(job-qty-expect,"->>,>>>,>>9") .   
                         WHEN "mr-comp"    THEN cVarValue = string(v-mrcomp) .     
                         WHEN "run-comp"    THEN cVarValue = string(v-runcomp) .   

                         WHEN "ttl-mch-hrs"       THEN cVarValue =  STRING((job-mr-act + job-run-act + job-dt-act),"->>,>>>,>>>,>>9.99") .
                         WHEN "ttl-lbr-hrs"       THEN cVarValue =  STRING(v-act-lab-cost,"->,>>>,>>>,>>9.99") .
                         WHEN "pic-per-hrs"       THEN cVarValue = IF v-pic-per-hrs NE ? THEN STRING(v-pic-per-hrs,"->>>,>>>,>>9.99") ELSE "".
                         WHEN "msf"               THEN cVarValue = STRING(v-calmsf,"->>>>9.99") .      
                         WHEN "msf-per-hrs"       THEN cVarValue = IF v-msf-per-hrs NE ? THEN STRING(v-msf-per-hrs,">,>>>,>>9.99") ELSE "".
                         WHEN "nbr-on"            THEN cVarValue = STRING(v-num-up,">>,>>>,>>9")     .
                         WHEN "kik-per-hrs "      THEN cVarValue =  IF v-kik-per-hrs NE ? THEN STRING(v-kik-per-hrs,"->>,>>>,>>9.99") ELSE "".
                         WHEN "pic-per-man-hrs"   THEN cVarValue = IF v-per-man-hrs NE ? THEN STRING(v-per-man-hrs,"->>,>>>,>>>,>>9.99") ELSE "" .      
                         WHEN "mr-wst"            THEN cVarValue = STRING(v-mrwaste,"->,>>9.99") .   
                         WHEN "run-wst"           THEN cVarValue = STRING(v-runwaste,"->,>>9.99") .   
                         WHEN "ttl-wst"           THEN cVarValue =  STRING(v-mrwaste + v-runwaste,"->,>>9.99") .
                         WHEN "%wst"              THEN cVarValue =  IF v-wst NE ? THEN STRING(v-wst,"->,>>9.99") ELSE "".

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
         end.

         assign mch-mr-std     = mch-mr-std + job-mr-std
                mch-run-std    = mch-run-std + job-run-std
                mch-mr-act     = mch-mr-act + job-mr-act
                mch-run-act    = mch-run-act + job-run-act
                mch-dt-act     = mch-dt-act + job-dt-act
                mch-qty-prod   = mch-qty-prod + job-qty-prod
                mch-qty-expect = mch-qty-expect + job-qty-expect
                dpt-mr-std     = dpt-mr-std + job-mr-std
                dpt-run-std    = dpt-run-std + job-run-std
                dpt-mr-act     = dpt-mr-act + job-mr-act
                dpt-run-act    = dpt-run-act + job-run-act
                dpt-dt-act     = dpt-dt-act + job-dt-act
                dpt-qty-prod   = dpt-qty-prod + job-qty-prod
                dpt-qty-expect = dpt-qty-expect + job-qty-expect

                job-mr-std = 0
                job-mr-act = 0
                job-run-std = 0
                job-run-act = 0
                job-dt-act = 0
                job-qty-prod = 0
                job-qty-expect = 0.
       END.

        if last-of(tt-mch-srt.m-code) then
        do:
           find mach where mach.company eq cocode and
                           mach.loc     eq locode and
                           mach.m-code  eq tt-mch-srt.m-code
                           no-lock no-error.
               {pc/rep/mchprdhr.i "mch"}

          /* display fill("-", 136) format "x(136)" at 7 skip
                   "*" at 5
                   mach.m-dscr at 7 format "x(10)" when available mach
                   mch-mr-std  at 25
                   mch-mr-act
                   mr-eff
                   mch-run-std at 51
                   mch-run-act
                   run-eff
                   tot-std-hrs at 77
                   tot-act-hrs
                   tot-eff
                   mch-dt-act  at 103
                   dt-eff
                   mch-qty-prod
                   mch-qty-expect skip
                   fill("-", 136) format "x(136)" at 7

               with frame detm STREAM-IO width 180 no-labels no-box. */

            PUT SKIP str-line SKIP .
           ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "m-cod"      THEN cVarValue = "" .
                         WHEN "ino"        THEN cVarValue = "" . 
                         WHEN "job"        THEN cVarValue = "" .
                         WHEN "mr-stn"     THEN cVarValue = string(mch-mr-std,"->>>9.9") .       
                         WHEN "mr-acl"     THEN cVarValue = string(mch-mr-act,"->>>9.9") .       
                         WHEN "mr-eff"     THEN cVarValue = string(mr-eff,"->>>>9.9") .           
                         WHEN "run-stnd"   THEN cVarValue = string(mch-run-std,"->>>9.9") .      
                         WHEN "run-acl"    THEN cVarValue = string(mch-run-act,"->>>9.9") .      
                         WHEN "run-eff"    THEN cVarValue = string(run-eff,"->>>>9.9") .          
                         WHEN "mr&-stnd"   THEN cVarValue = string(tot-std-hrs,"->>>9.9") .      
                         WHEN "mr&-acl"    THEN cVarValue = string(tot-act-hrs,"->>>9.9") .      
                         WHEN "mr&-eff"    THEN cVarValue = string(tot-eff,"->>>>9.9") .          
                         WHEN "dt-acl"     THEN cVarValue = string(mch-dt-act,"->>>9.9") .       
                         WHEN "dt-eff"     THEN cVarValue = string(dt-eff,"->>>>9.9") .           
                         WHEN "acl-qty"    THEN cVarValue = string(mch-qty-prod,"->>,>>>,>>9") .     
                         WHEN "exp-qty"    THEN cVarValue = string(mch-qty-expect,"->>,>>>,>>9") . 
                         WHEN "mr-comp"    THEN cVarValue = "" .     
                         WHEN "run-comp"    THEN cVarValue = "" .
                         WHEN "ttl-mch-hrs"       THEN cVarValue =  "" .
                         WHEN "ttl-lbr-hrs"       THEN cVarValue =  "" .
                         WHEN "pic-per-hrs"       THEN cVarValue = "" .
                         WHEN "msf"               THEN cVarValue = "" .
                         WHEN "msf-per-hrs"       THEN cVarValue = "" .
                         WHEN "nbr-on"            THEN cVarValue = "" .
                         WHEN "kik-per-hrs "      THEN cVarValue =  "" .
                         WHEN "pic-per-man-hrs"   THEN cVarValue = "" .
                         WHEN "mr-wst"            THEN cVarValue = "" .
                         WHEN "run-wst"           THEN cVarValue = "" .
                         WHEN "ttl-wst"           THEN cVarValue =  "" .
                         WHEN "%wst"              THEN cVarValue =  "" .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED "* " + (IF AVAIL mach THEN string(mach.m-dscr,"x(10)") ELSE "") + substring(cDisplay,13,350) SKIP(1).

           assign mch-mr-std = 0
                  mch-mr-act = 0
                  mch-run-std = 0
                  mch-run-act = 0
                  mch-dt-act = 0
                  mch-qty-prod = 0
                  mch-qty-expect = 0.
        end.

        if last-of(tt-mch-srt.dept) then do:
           find dept where dept.company eq cocode and
                           dept.cod     eq tt-mch-srt.dept
                           no-lock no-error.
           if not available dept then
              find dept where dept.company eq "" and
                              dept.cod     eq tt-mch-srt.dept
                              no-lock no-error.
           {pc/rep/mchprdhr.i "dpt"}

          /* display "**" at 4 dept.dscr at 7 format "x(10)" when available dept
                   dpt-mr-std  at 25
                   dpt-mr-act
                   mr-eff
                   dpt-run-std at 51
                   dpt-run-act
                   run-eff
                   tot-std-hrs at 77
                   tot-act-hrs
                   tot-eff
                   dpt-dt-act  at 103
                   dt-eff
                   dpt-qty-prod
                   dpt-qty-expect skip
                   fill("=", 142) format "x(142)"

               with frame detd STREAM-IO width 180 no-labels no-box. */

           PUT SKIP str-line SKIP .
           ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
                      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "m-cod"      THEN cVarValue = "" .
                         WHEN "ino"        THEN cVarValue = "" . 
                         WHEN "job"        THEN cVarValue = "" .
                         WHEN "mr-stn"     THEN cVarValue = string(dpt-mr-std,"->>>9.9") .       
                         WHEN "mr-acl"     THEN cVarValue = string(dpt-mr-act,"->>>9.9") .       
                         WHEN "mr-eff"     THEN cVarValue = string(mr-eff,"->>>>9.9") .           
                         WHEN "run-stnd"   THEN cVarValue = string(dpt-run-std,"->>>9.9") .      
                         WHEN "run-acl"    THEN cVarValue = string(dpt-run-act,"->>>9.9") .      
                         WHEN "run-eff"    THEN cVarValue = string(run-eff,"->>>>9.9") .          
                         WHEN "mr&-stnd"   THEN cVarValue = string(tot-std-hrs,"->>>9.9") .      
                         WHEN "mr&-acl"    THEN cVarValue = string(tot-act-hrs,"->>>9.9") .      
                         WHEN "mr&-eff"    THEN cVarValue = string(tot-eff,"->>>>9.9") .          
                         WHEN "dt-acl"     THEN cVarValue = string(dpt-dt-act,"->>>9.9") .       
                         WHEN "dt-eff"     THEN cVarValue = string(dt-eff,"->>>>9.9") .           
                         WHEN "acl-qty"    THEN cVarValue = string(dpt-qty-prod,"->>,>>>,>>9") .     
                         WHEN "exp-qty"    THEN cVarValue = string(dpt-qty-expect,"->>,>>>,>>9") .
                         WHEN "mr-comp"    THEN cVarValue = "" .     
                         WHEN "run-comp"    THEN cVarValue = "" .
                        WHEN "ttl-mch-hrs"       THEN cVarValue =  "" .
                         WHEN "ttl-lbr-hrs"       THEN cVarValue =  "" .
                         WHEN "pic-per-hrs"       THEN cVarValue = "" .
                         WHEN "msf"               THEN cVarValue = "" .
                         WHEN "msf-per-hrs"       THEN cVarValue = "" .
                         WHEN "nbr-on"            THEN cVarValue = "" .
                         WHEN "kik-per-hrs "      THEN cVarValue =  "" .
                         WHEN "pic-per-man-hrs"   THEN cVarValue = "" .
                         WHEN "mr-wst"            THEN cVarValue = "" .
                         WHEN "run-wst"           THEN cVarValue = "" .
                         WHEN "ttl-wst"           THEN cVarValue =  "" .
                         WHEN "%wst"              THEN cVarValue =  "" .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED "** " + (IF AVAIL dept THEN string(dept.dscr,"x(10)") ELSE "") + substring(cDisplay,14,350) SKIP.

           assign dpt-mr-std = 0
                  dpt-mr-act = 0
                  dpt-run-std = 0
                  dpt-run-act = 0
                  dpt-dt-act = 0
                  dpt-qty-prod = 0
                  dpt-qty-expect = 0.
           page.
        end.
     end. /* each item */

FIND CURRENT mch-srt NO-LOCK NO-ERROR.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
     IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
 END.

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



