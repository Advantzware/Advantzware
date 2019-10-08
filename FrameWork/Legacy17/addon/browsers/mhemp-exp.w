&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd-exlexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd-exlexp 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER piEmpFrom AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER piEmpTo AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER piReckey AS CHAR NO-UNDO.
/*DEFINE INPUT PARAMETER pcVendFrom LIKE po-ord.vend-no NO-UNDO.
DEFINE INPUT PARAMETER pcVendTo   LIKE po-ord.vend-no NO-UNDO.
DEFINE INPUT PARAMETER pcItemFrom LIKE po-ordl.i-no NO-UNDO.
DEFINE INPUT PARAMETER pcItemTo   LIKE po-ordl.i-no NO-UNDO.
DEFINE INPUT PARAMETER pcVendItemFrom LIKE po-ordl.vend-i-no NO-UNDO.
DEFINE INPUT PARAMETER pcVendItemTo   LIKE po-ordl.vend-i-no NO-UNDO.
DEFINE INPUT PARAMETER pcJobFrom LIKE po-ordl.job-no NO-UNDO.
DEFINE INPUT PARAMETER pcJobTo   LIKE po-ordl.job-no NO-UNDO.
DEFINE INPUT PARAMETER piJob2From LIKE po-ordl.job-no2 NO-UNDO.
DEFINE INPUT PARAMETER piJob2To   LIKE po-ordl.job-no2 NO-UNDO.
DEFINE INPUT PARAMETER pdDateFrom  LIKE po-ordl.due-date NO-UNDO.
DEFINE INPUT PARAMETER pdDateTo    LIKE po-ordl.due-date NO-UNDO.
DEFINE INPUT PARAMETER piOpenClosed AS INT NO-UNDO. */


/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/globdefs.i}

DEFINE BUFFER b-prgrms FOR prgrms.
DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.

{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/


IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("NOSWEAT") + "..".
ELSE
ASSIGN
/*   period_pos = INDEX(PROGRAM-NAME(1),".")                                             */
/*   v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 10) + 1) */
  v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1).
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

DEFINE STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.
ASSIGN cTextListToSelect = "Emp ID,Name,Start Date,Started,End Date,Shift,Ended,Type,Posted,Total,Rate,Rate Usage"
       cFieldListToSelect = "machemp.employee,name,machemp.start_date,strd-dt,machemp.end_date,machemp.shift,ended,machemp.ratetype,machemp.posted,total,machemp.rate,rate_usage"
    .

{sys/inc/ttRptSel.i}

    ASSIGN cTextListToDefault  = "Emp ID,Name,Start Date,Started,End Date,Shift,Ended,Type,Posted,Total,Rate,Rate Usage" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-exlexp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_mch end_mch begin_job begin_job2 ~
end_job end_job2 begin_date end_date sl_avail Btn_Def Btn_Add sl_selected ~
Btn_Remove btn_Up btn_down tb_runExcel fi_file btn-ok btn-cancel RECT-6 ~
RECT-7 RECT-8 begin_emp end_emp 
&Scoped-Define DISPLAYED-OBJECTS begin_mch end_mch begin_job begin_job2 ~
end_job end_job2 begin_date end_date sl_avail sl_selected tb_excel ~
tb_runExcel fi_file begin_emp end_emp 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-exlexp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-exlexp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHAR , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-exlexp 
FUNCTION buildHeader RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue rd-exlexp 
FUNCTION getValue RETURNS CHARACTER
  ( BUFFER ipb-machemp FOR machemp, 
    BUFFER ipb-machtran FOR machtran, 
    ipc-field AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-machemp rd-exlexp 
FUNCTION getValue-machemp RETURNS CHARACTER
  ( BUFFER ipb-buffer FOR machemp, ipc-field AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-machtran rd-exlexp 
FUNCTION getValue-machtran RETURNS CHARACTER
  ( BUFFER ipb-buffer FOR machtran, ipc-field AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/1901 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_emp AS CHARACTER FORMAT "X(5)" 
     LABEL "From Emp#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_job AS CHARACTER FORMAT "X(6)" 
     LABEL "From Job #" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE begin_job2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mch AS CHARACTER FORMAT "X(6)" 
     LABEL "From Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/2099 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_emp AS CHARACTER FORMAT "X(5)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To Emp#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_job AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
     LABEL "To Job #" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE end_job2 AS INTEGER FORMAT ">>":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE end_mch AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "To Machine" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-po.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 6.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 6.67.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 2.71.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5.1 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5.1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL yes 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-exlexp
     begin_date AT ROW 3 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Due Date" WIDGET-ID 112
     end_date AT ROW 2.95 COL 69 COLON-ALIGNED HELP
          "Enter Ending Due Date" WIDGET-ID 114
     begin_mch AT ROW 4.1 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Vendor Item Number" WIDGET-ID 104
     end_mch AT ROW 4.1 COL 69 COLON-ALIGNED HELP
          "Enter Ending Vendor Item Number" WIDGET-ID 106
     begin_job AT ROW 5.24 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Job Number" WIDGET-ID 108
     begin_job2 AT ROW 5.24 COL 39.2 COLON-ALIGNED HELP
          "Enter Beginning Job Number" NO-LABEL WIDGET-ID 116
     end_job AT ROW 5.24 COL 69 COLON-ALIGNED HELP
          "Enter Ending Job Number" WIDGET-ID 110
     end_job2 AT ROW 5.24 COL 82 COLON-ALIGNED HELP
          "Enter Ending Job Number" NO-LABEL WIDGET-ID 118
    begin_emp AT ROW 6.38 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Vendor Item Number" WIDGET-ID 146
     end_emp AT ROW 6.38 COL 69 COLON-ALIGNED HELP
          "Enter Ending Vendor Item Number" WIDGET-ID 148
     sl_avail AT ROW 10 COL 9 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 10.24 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 11.19 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 130
     sl_selected AT ROW 10 COL 64 NO-LABEL WIDGET-ID 28
     Btn_Remove AT ROW 12.14 COL 44 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 134
     btn_Up AT ROW 13.1 COL 44 WIDGET-ID 136
     btn_down AT ROW 14.05 COL 44 WIDGET-ID 132
     tb_excel AT ROW 15.81 COL 36 WIDGET-ID 32
     tb_runExcel AT ROW 15.81 COL 78 RIGHT-ALIGNED WIDGET-ID 34
     fi_file AT ROW 16.76 COL 34 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     btn-ok AT ROW 18.67 COL 30 WIDGET-ID 14
     btn-cancel AT ROW 18.67 COL 60.2 WIDGET-ID 12
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 9.29 COL 10 WIDGET-ID 140
     "Selected Columns" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 9.29 COL 63.4 WIDGET-ID 138
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 36
          BGCOLOR 2 
     "Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 8.33 COL 3 WIDGET-ID 86
     RECT-6 AT ROW 8.57 COL 2 WIDGET-ID 30
     RECT-7 AT ROW 1.48 COL 2 WIDGET-ID 38
     RECT-8 AT ROW 15.33 COL 2 WIDGET-ID 84
     SPACE(0.59) SKIP(2.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Export to Excel" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX rd-exlexp
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME rd-exlexp:SCROLLABLE       = FALSE
       FRAME rd-exlexp:HIDDEN           = TRUE.

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME rd-exlexp     = 
                "parm".

ASSIGN 
       begin_emp:PRIVATE-DATA IN FRAME rd-exlexp     = 
                "parm".

ASSIGN 
       begin_job:PRIVATE-DATA IN FRAME rd-exlexp     = 
                "parm".

ASSIGN 
       begin_mch:PRIVATE-DATA IN FRAME rd-exlexp     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME rd-exlexp     = 
                "parm".

ASSIGN 
       end_emp:PRIVATE-DATA IN FRAME rd-exlexp     = 
                "parm".

ASSIGN 
       end_job:PRIVATE-DATA IN FRAME rd-exlexp     = 
                "parm".

ASSIGN 
       end_mch:PRIVATE-DATA IN FRAME rd-exlexp     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME rd-exlexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-exlexp
   NO-ENABLE                                                            */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME rd-exlexp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME rd-exlexp
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME rd-exlexp     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-exlexp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-exlexp rd-exlexp
ON HELP OF FRAME rd-exlexp /* Export POs to Excel */
DO:
    DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
    DEF VAR ls-cur-val AS CHAR NO-UNDO.
    DEF VAR char-val AS CHAR NO-UNDO.

    lw-focus = FOCUS.
    
    case lw-focus:name :

       when "begin_vend-no" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-vendno.w (cocode, "", ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* vend-no*/
       when "end_vend-no" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-vendno.w (cocode, "", ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* vend-no*/
       when "begin_item" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-itemfg.w (cocode, "", ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* itemfg */
       when "end_item" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-itemfg.w (cocode, "", ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* itemfg*/
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-exlexp rd-exlexp
ON WINDOW-CLOSE OF FRAME rd-exlexp /* Export POs to Excel */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date rd-exlexp
ON LEAVE OF begin_date IN FRAME rd-exlexp /* From Date */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_emp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_emp rd-exlexp
ON LEAVE OF begin_emp IN FRAME rd-exlexp /* From Emp# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job rd-exlexp
ON LEAVE OF begin_job IN FRAME rd-exlexp /* From Job # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mch rd-exlexp
ON LEAVE OF begin_mch IN FRAME rd-exlexp /* From Machine */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-exlexp
ON CHOOSE OF btn-cancel IN FRAME rd-exlexp /* Cancel */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd-exlexp
ON CHOOSE OF btn-ok IN FRAME rd-exlexp /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
  RUN GetSelectionList.  
  run run-report.

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-exlexp
ON CHOOSE OF Btn_Add IN FRAME rd-exlexp /* Add >> */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-exlexp
ON CHOOSE OF Btn_Def IN FRAME rd-exlexp /* Default */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

  RUN DisplaySelectionDefault.  /* task 04041406 */ 
  RUN DisplaySelectionList2 .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd-exlexp
ON CHOOSE OF btn_down IN FRAME rd-exlexp /* Move Down */
DO:
  RUN Move-Field ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd-exlexp
ON CHOOSE OF Btn_Remove IN FRAME rd-exlexp /* << Remove */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd-exlexp
ON CHOOSE OF btn_Up IN FRAME rd-exlexp /* Move Up */
DO:
  RUN Move-Field ("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date rd-exlexp
ON LEAVE OF end_date IN FRAME rd-exlexp /* To Date */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_emp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_emp rd-exlexp
ON LEAVE OF end_emp IN FRAME rd-exlexp /* To Emp# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job rd-exlexp
ON LEAVE OF end_job IN FRAME rd-exlexp /* To Job # */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mch rd-exlexp
ON LEAVE OF end_mch IN FRAME rd-exlexp /* To Machine */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-exlexp
ON LEAVE OF fi_file IN FRAME rd-exlexp /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd-exlexp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd-exlexp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-exlexp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-exlexp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-exlexp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-exlexp /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel rd-exlexp
ON VALUE-CHANGED OF tb_runExcel IN FRAME rd-exlexp /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-exlexp 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpw.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN DisplaySelectionList.
  RUN enable_UI.
   {methods/nowait.i}
   DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    RUN Set-Sort-Data.

    APPLY "entry" TO begin_date.
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd-exlexp  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME rd-exlexp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd-exlexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-exlexp 
PROCEDURE DisplaySelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

/*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-exlexp 
PROCEDURE DisplaySelectionList2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

/*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd-exlexp  _DEFAULT-ENABLE
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
  DISPLAY begin_mch end_mch begin_job begin_job2 end_job end_job2 begin_date 
          end_date sl_avail sl_selected tb_excel tb_runExcel fi_file begin_emp 
          end_emp 
      WITH FRAME rd-exlexp.
  ENABLE begin_mch end_mch begin_job begin_job2 end_job end_job2 begin_date 
         end_date sl_avail Btn_Def Btn_Add sl_selected Btn_Remove btn_Up 
         btn_down tb_runExcel fi_file btn-ok btn-cancel RECT-6 RECT-7 RECT-8 
         begin_emp end_emp 
      WITH FRAME rd-exlexp.
  VIEW FRAME rd-exlexp.
  {&OPEN-BROWSERS-IN-QUERY-rd-exlexp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd-exlexp 
PROCEDURE GetSelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR cTmpList AS cha NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

 DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME} :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.  
    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           /* ttRptSelected.FieldLength */
        .   
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd-exlexp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-exlexp 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-excelheader AS CHAR NO-UNDO.
DEF VAR v-excel-detail-lines AS CHAR NO-UNDO.
DEF VAR v-adder AS CHAR NO-UNDO.
DEF BUFFER xjob-mat FOR job-mat.
DEF BUFFER xitem    FOR item.

v-excelheader = buildHeader().
SESSION:SET-WAIT-STATE ("general").

IF tb_excel THEN OUTPUT STREAM excel TO VALUE(fi_file).
IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.


FOR EACH machemp WHERE TRUE 
    AND machemp.table_rec_key = piReckey
    AND trim(machemp.employee) GE TRIM(begin_emp)
    AND trim(machemp.employee) LE trim(end_emp) 
    AND machemp.start_date GE begin_date 
    AND machemp.start_date LE end_date NO-LOCK:

    v-excel-detail-lines = "".
   
    FOR EACH ttRptSelected:

        IF lookup(ttRptSelected.FieldList,"name,strd-dt,ended,total,rate_usage") EQ 0 THEN do:
        v-excel-detail-lines = v-excel-detail-lines + 
            appendXLLine(getValue(BUFFER machemp,BUFFER machtran,ttRptSelected.FieldList)).
        END.
        ELSE do:
         CASE ttRptSelected.FieldList:                                                                                       
             WHEN "name" THEN do:
                 FIND first employee WHERE employee.company = cocode
                     AND employee.employee = machemp.employee NO-LOCK NO-ERROR.

                 IF AVAIL employee THEN DO:
                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(employee.last_name + ',' +  employee.first_name)).                          
                 END.
                 ELSE v-excel-detail-lines = v-excel-detail-lines + "," .
             END.
             WHEN "strd-dt" THEN do:
                    v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(machemp.start_time,'HH:MM am')).                          
             END.
             WHEN "ended" THEN do:
                 IF machemp.end_time = 0 AND machemp.end_date EQ ? THEN
                     v-excel-detail-lines = v-excel-detail-lines + "," .
                 ELSE
                     v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(machemp.end_time,'HH:MM am')).                          
             END.
             WHEN "total" THEN do:
                 IF machemp.total_time = 0 AND machemp.end_date EQ ? THEN
                     v-excel-detail-lines = v-excel-detail-lines + "," .
                 ELSE
                     v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING(machemp.total_time,'HH:MM')).                          
             END.
             WHEN "rate_usage" THEN do:
                 IF machemp.rate_usage = YES THEN
                     v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING("Shift")).                          
                 ELSE
                     v-excel-detail-lines = v-excel-detail-lines + appendXLLine(STRING("Machine")). 
             END.
             
             
         END CASE. 
        END.
    END.

    PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.
END.

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd-exlexp 
PROCEDURE Set-Sort-Data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:

      /* If a customer number was entered, find first and last matching customers. */
    ASSIGN 
        begin_emp:SCREEN-VALUE = STRING(piEmpFrom)
        end_emp:SCREEN-VALUE   = STRING(piEmpTo) .

  /*  ASSIGN 
        begin_po:SCREEN-VALUE = string(piPOFrom)
        begin_job:SCREEN-VALUE = pcJobFrom
        begin_job2:SCREEN-VALUE = string(piJob2From)
        end_job2:SCREEN-VALUE = string(99)
        end_date:SCREEN-VALUE = string(12/31/2099)
        rd_open-closed:SCREEN-VALUE = STRING(piOpenClosed).
        
        IF pdDateFrom NE ? THEN 
            begin_date:SCREEN-VALUE = string(pdDateFrom).
        ELSE
            begin_date:SCREEN-VALUE = string(01/01/1901).
        IF pcJobFrom NE "" THEN 
            end_job:SCREEN-VALUE   = pcJobTo.
        ELSE
            end_job:SCREEN-VALUE   = "ZZZZZZZZZZZZ".
        IF piPOFrom NE 0 THEN 
            end_po:SCREEN-VALUE   = string(piPOTo).
        ELSE
            end_po:SCREEN-VALUE   = string(99999999).
    ASSIGN 
        begin_vend-no:SCREEN-VALUE = assignParam(pcVendFrom,NO)
        end_vend-no:SCREEN-VALUE   = assignParam(pcVendTo,YES)
        begin_item:SCREEN-VALUE = assignParam(pcItemFrom,NO)
        end_item:SCREEN-VALUE   = assignParam(pcItemTo,YES)
        begin_vend-i-no:SCREEN-VALUE = assignParam(pcVendItemFrom,NO)
        end_vend-i-no:SCREEN-VALUE   = assignParam(pcVendItemTo,YES). */
END.
RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-exlexp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Adds a value to a csv line
    Notes:  Protects agains commans and quotes.
------------------------------------------------------------------------------*/
    DEF VAR lc-line AS CHAR NO-UNDO.

    ipc-append = REPLACE(ipc-append, '"', '').
    ipc-append = REPLACE(ipc-append, ',', ' ').
    lc-line = lc-line + '"' + ipc-append + '",'.
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-exlexp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHAR , ipl-end AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR lc-return AS CHAR.

    IF ipl-end THEN
        lc-return = ipc-param + "ZZZZZZZZZZZZZZZ".
    ELSE
        lc-return = ipc-param.

  RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd-exlexp 
FUNCTION buildHeader RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR lc-header AS CHAR NO-UNDO.

FOR EACH ttRptSelected:
    lc-header = lc-header + appendXLLine(ttRptSelected.TextList).
END.
/*     lc-header = lc-header + appendXLLine ("PO #").      */
/*     lc-header = lc-header + appendXLLine ("Vendor #").  */
/*     lc-header = lc-header + appendXLLine ("Due Date").  */
/*     lc-header = lc-header + appendXLLine ("Ship ID").   */
/*     lc-header = lc-header + appendXLLine ("Ship Name"). */
/*     lc-header = lc-header + appendXLLine ("Job #").     */
/*     lc-header = lc-header + appendXLLine ("Item #").    */
/*     lc-header = lc-header + appendXLLine ("Item Name"). */

  
    RETURN lc-header.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue rd-exlexp 
FUNCTION getValue RETURNS CHARACTER
  ( BUFFER ipb-machemp FOR machemp, 
    BUFFER ipb-machtran FOR machtran, 
    ipc-field AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR lc-return AS CHAR FORMAT "x(100)" NO-UNDO.
    DEF VAR li-period AS INT NO-UNDO.
    DEF VAR lc-table AS CHAR NO-UNDO.

    li-period = INDEX(ipc-field,".").
    IF li-period > 0 THEN DO:
        lc-table = SUBSTRING(ipc-field, 1, li-period - 1).
        ipc-field = SUBSTRING(ipc-field, li-period + 1,  LENGTH(ipc-field) - li-period).
        CASE lc-table:
            WHEN "machemp" THEN
                lc-return = getValue-machemp(BUFFER ipb-machemp, ipc-field).
            WHEN "machtran" THEN
                lc-return = getValue-machtran(BUFFER ipb-machtran, ipc-field).
            OTHERWISE
                lc-return = "".
        END CASE.
    END.
    ELSE
        lc-return = getValue-machemp(BUFFER ipb-machemp, ipc-field).
    RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-machemp rd-exlexp 
FUNCTION getValue-machemp RETURNS CHARACTER
  ( BUFFER ipb-buffer FOR machemp, ipc-field AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR h-field AS HANDLE.
    DEF VAR li-extent AS INT NO-UNDO.
    DEF VAR lc-return AS CHAR FORMAT "x(100)" NO-UNDO.

    CASE ipc-field :
        WHEN "dfunc"  THEN DO:
        END.
        OTHERWISE DO:
            IF INDEX(ipc-field,"[") > 0 THEN DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-buffer:BUFFER-FIELD(ipc-field).
            IF h-field:EXTENT = 0 THEN
                lc-return = STRING(h-field:BUFFER-VALUE /*, h-field:FORMAT*/ ).
            ELSE
                lc-return = STRING(h-field:BUFFER-VALUE(li-extent) /*, h-field:FORMAT*/ ).
        END.
    END CASE.
    IF lc-return EQ ? THEN lc-return = "".
    RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-machtran rd-exlexp 
FUNCTION getValue-machtran RETURNS CHARACTER
  ( BUFFER ipb-buffer FOR machtran, ipc-field AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR h-field AS HANDLE.
    DEF VAR li-extent AS INT NO-UNDO.
    DEF VAR lc-return AS CHAR FORMAT "x(100)" NO-UNDO.
    DEF BUFFER lb-oe-ord FOR oe-ord.
    DEF BUFFER lb-job-hdr FOR job-hdr.

    CASE ipc-field :
        WHEN "dfuncFGFromJob"  THEN DO:
           /* IF ipb-buffer.job-no NE "" THEN DO:
                FIND FIRST lb-job-hdr WHERE lb-job-hdr.company = ipb-buffer.company
                    AND lb-job-hdr.job-no = ipb-buffer.job-no
                    AND lb-job-hdr.job-no2 = ipb-buffer.job-no2 NO-LOCK NO-ERROR.
                IF AVAIL lb-job-hdr THEN
                    lc-return = lb-job-hdr.i-no.
            END.*/
        END.
        WHEN "dfuncCustFromOrder"  THEN DO:
         /*   IF ipb-buffer.ord-no > 0 THEN DO:
                FIND FIRST lb-oe-ord WHERE lb-oe-ord.company = ipb-buffer.company
                    AND lb-oe-ord.ord-no = ipb-buffer.ord-no NO-LOCK NO-ERROR.
                IF AVAIL lb-oe-ord THEN
                    lc-return = lb-oe-ord.cust-no.
            END.*/
        END.
        OTHERWISE DO:
            IF INDEX(ipc-field,"[") > 0 THEN DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-buffer:BUFFER-FIELD(ipc-field).
            IF h-field:EXTENT = 0 THEN
                lc-return = STRING(h-field:BUFFER-VALUE /*, h-field:FORMAT*/ ).
            ELSE
                lc-return = STRING(h-field:BUFFER-VALUE(li-extent) /*, h-field:FORMAT*/ ).
        END.
    END CASE.
    IF lc-return EQ ? THEN lc-return = "".
    RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


