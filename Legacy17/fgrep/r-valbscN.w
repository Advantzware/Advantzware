&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fgrep\r-valbsc.w

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

DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

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

/*{sys/inc/custlistform.i ""IL13"" }*/
{sys/ref/CustList.i NEW}

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

def temp-table tt-report like report field qty as dec.
DEF NEW SHARED temp-table tt-file no-undo
                       field tt-sman like sman.sman
                       field tt-cust-no like cust.cust-no
                       field tt-i-no like itemfg.i-no .

DEF STREAM excel.
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.
DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "CUST#,FG ITEM#,CUSTOMER PART#,DESCRIPTION,CUST PO#,JOB#,ORDERED QTY,SHIPPED QTY," +
                           "ON-HAND QTY,RECEIPT DATE,SELL PRICE,TOTAL VALUE"
       cFieldListToSelect = "cust,fgitem,cust-part,desc,cust-po,job,ord-qty,ship-qty," +
                            "oh-hand-qty,rec-date,sell-pr,tot-val"
       cFieldLength = "8,15,15,30,15,10,12,12," + "12,12,12,15"
       cFieldType = "c,c,c,c,c,c,i,i," + "i,c,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "CUST#,FG ITEM#,CUSTOMER PART#,DESCRIPTION,CUST PO#,JOB#,ORDERED QTY,SHIPPED QTY," +
                           "ON-HAND QTY,RECEIPT DATE,SELL PRICE,TOTAL VALUE" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 as-of-date tb_cust-list ~
btnCustList begin_cust-no end_cust-no begin_slm end_slm tb_zero tb_whs ~
tb_detail sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down ~
rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS as-of-date tb_cust-list begin_cust-no ~
end_cust-no begin_slm end_slm tb_zero tb_whs tb_detail sl_avail sl_selected ~
rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm ~
tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

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

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

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

DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/01 
     LABEL "As of" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_slm AS CHARACTER FORMAT "XXX":U 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_slm AS CHARACTER FORMAT "XXX":U INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-valbsc.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
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
     SIZE 21 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.05.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .95 NO-UNDO.

DEFINE VARIABLE tb_detail AS LOGICAL INITIAL no 
     LABEL "Detail?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_whs AS LOGICAL INITIAL no 
     LABEL "Include Customer Owned Warehouse?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE tb_zero AS LOGICAL INITIAL no 
     LABEL "Include Zero Quantity Items?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     as-of-date AT ROW 2.24 COL 27 COLON-ALIGNED
     tb_cust-list AT ROW 3.33 COL 31.8 WIDGET-ID 6
     btnCustList AT ROW 3.33 COL 64.2 WIDGET-ID 8
     begin_cust-no AT ROW 4.29 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 4.29 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_slm AT ROW 5.24 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slm AT ROW 5.24 COL 70 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     tb_zero AT ROW 6.43 COL 30
     tb_whs AT ROW 7.48 COL 30
     tb_detail AT ROW 8.57 COL 30
     sl_avail AT ROW 10.91 COL 4.8 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 10.91 COL 40.8 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 10.91 COL 60.2 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 11.91 COL 40.8 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 12.91 COL 40.8 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 13.95 COL 40.8 WIDGET-ID 40
     btn_down AT ROW 14.95 COL 40.8 WIDGET-ID 42
     rd-dest AT ROW 17.19 COL 6 NO-LABEL
     lv-ornt AT ROW 17.43 COL 32 NO-LABEL
     lines-per-page AT ROW 17.43 COL 85 COLON-ALIGNED
     lv-font-no AT ROW 19.19 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 20.14 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 21.33 COL 31
     tb_excel AT ROW 22.05 COL 71 RIGHT-ALIGNED
     tb_runExcel AT ROW 22.05 COL 93 RIGHT-ALIGNED
     fi_file AT ROW 22.95 COL 49 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 24.95 COL 26
     btn-cancel AT ROW 24.95 COL 58
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 10.19 COL 5.6 WIDGET-ID 38
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 16.24 COL 2
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 10.19 COL 60.2 WIDGET-ID 44
     RECT-6 AT ROW 16.71 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 26.24.


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
         TITLE              = "Sales Value by Sales Rep by Customer"
         HEIGHT             = 26.48
         WIDTH              = 95.8
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       as-of-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_detail:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       tb_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_zero:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Sales Value by Sales Rep by Customer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sales Value by Sales Rep by Customer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME as-of-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL as-of-date C-Win
ON LEAVE OF as-of-date IN FRAME FRAME-A /* As of */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slm C-Win
ON LEAVE OF begin_slm IN FRAME FRAME-A /* Beginning Sales Rep# */
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
  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT tb_cust-list OR  NOT AVAIL ttCustList THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust-no,
                    INPUT END_cust-no).
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
           {custom/asifax.i &begin_cust=END_cust-no
                            &END_cust=END_cust-no
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:title 
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = ''
                             &begin_cust= END_cust-no
                             &END_cust=END_cust-no
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust=END_cust-no
                                  &END_cust=END_cust-no
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case.
  SESSION:SET-WAIT-STATE ("").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
DO:
  RUN CustList.

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


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON HELP OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val) .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slm C-Win
ON LEAVE OF end_slm IN FRAME FRAME-A /* Ending Sales Rep# */
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


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
DO:
  assign {&self-name}.
  EMPTY TEMP-TABLE ttCustList.
  RUN SetCustRange(INPUT tb_cust-list).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_detail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detail C-Win
ON VALUE-CHANGED OF tb_detail IN FRAME FRAME-A /* Detail? */
DO:
  IF {&self-name}:SCREEN-VALUE EQ "YES" THEN lv-ornt:SCREEN-VALUE = "L".
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


&Scoped-define SELF-NAME tb_whs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_whs C-Win
ON VALUE-CHANGED OF tb_whs IN FRAME FRAME-A /* Include Customer Owned Warehouse? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_zero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zero C-Win
ON VALUE-CHANGED OF tb_zero IN FRAME FRAME-A /* Include Zero Quantity Items? */
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

  as-of-date = TODAY. 
  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "IL13",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO as-of-date.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'IL13',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""IL13""}

  IF ou-log THEN DO:
      ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
        tb_cust-list = YES 
        .
      RUN SetCustRange(INPUT tb_cust-list).
  END.
  ELSE
      ASSIGN
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        .

  IF ou-log AND ou-cust-int = 0 THEN do:
       ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"
        tb_cust-list = NO
        .
      RUN SetCustRange(tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "YES").
   END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildCustList C-Win 
PROCEDURE BuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Builds the temp table of customers   
  Parameters:  Company Code, Customer list logical and/or customer range
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplList AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginCust AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEndCust AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-cust FOR cust.

DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.

IF iplList THEN DO:
    RUN sys/ref/CustList.p (INPUT ipcCompany,
                            INPUT 'IL13',
                            INPUT YES,
                            OUTPUT lActive).
END.
ELSE DO:
    FOR EACH bf-cust
        WHERE bf-cust.company EQ ipcCompany
          AND bf-cust.cust-no GE ipcBeginCust
          AND bf-cust.cust-no LE ipcEndCust
        NO-LOCK:
        CREATE ttCustList.
        ASSIGN 
            ttCustList.cust-no = bf-cust.cust-no
            ttCustList.log-fld = YES
        .
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustList C-Win 
PROCEDURE CustList :
/*------------------------------------------------------------------------------
  Purpose:  Display a UI of selected customers   
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    RUN sys/ref/CustListManager.w(INPUT cocode,
                                  INPUT 'IL13').


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
  DISPLAY as-of-date tb_cust-list begin_cust-no end_cust-no begin_slm end_slm 
          tb_zero tb_whs tb_detail sl_avail sl_selected rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 as-of-date tb_cust-list btnCustList begin_cust-no 
         end_cust-no begin_slm end_slm tb_zero tb_whs tb_detail sl_avail 
         Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel 
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
/* ----------------------------------------------- fg/rep/valbycs.p 12/02 JLF */
/*  finished goods sales value by salesrep by customer                        */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw3.f}

def var vdat        as   date init today format "99/99/9999".
def var fcus        like itemfg.cust-no.
def var tcus        like fcus init "zzzzzzzz".
def var fsls        like cust.sman.
def var tsls        like fsls init "zzz".
def var vzer        as   log format "Y/N" init no.
def var vwhs        like vzer.
DEF VAR cSlsRep     AS CHAR NO-UNDO .
def var v-sman      like sman.sman.
def var v-sname     like sman.sname.
def var v-frst      as   log no-undo.
def var v-print     as   log no-undo.
def var v-bin       as   log no-undo.
def var v-price     like itemfg.sell-price no-undo.
def var v-uom       like itemfg.sell-uom no-undo.
def var v-cas-cnt   like itemfg.case-count no-undo.
def var v-binqty    as   dec.
def var v-ord       as   dec extent 4.
def var v-shp       as   dec extent 4.
def var v-qoh       as   dec extent 4.
def var v-ext       as   dec extent 4.
def var v-date      as   date no-undo.
def var v-ord-qty   like oe-ordl.qty.
DEF VAR v-job       AS   CHAR FORMAT "x(9)" NO-UNDO.
DEF VAR li-inv-qty LIKE oe-ordl.inv-qty NO-UNDO.
DEF VAR li-ship-qty LIKE oe-ordl.ship-qty NO-UNDO.
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

/*{sys/form/r-top5DL3.f} */
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.   

form header
     skip(1)
     "Sales Rep:" v-sman v-sname
     skip(1)
     str-tit4 SKIP
     str-tit5 SKIP
     with frame r-top.

ASSIGN
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 vdat     = as-of-date
 fcus     = begin_cust-no
 tcus     = end_cust-no
 fsls     = begin_slm
 tsls     = end_slm
 vzer     = tb_zero
 vwhs     = tb_whs.

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

        IF LOOKUP(ttRptSelected.TextList, "ORDERED QTY,SHIPPED QTY,ON-HAND QTY,TOTAL VALUE") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.


{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE("general").

VIEW FRAME r-top.

/*IF tb_detail THEN VIEW FRAME r-top2.
             ELSE VIEW FRAME r-top1.*/

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

EMPTY TEMP-TABLE tt-file.
    FOR EACH ttCustList 
        WHERE ttCustList.log-fld
        NO-LOCK,
        EACH cust
        where cust.company eq cocode
          /*and cust.cust-no ge fcus*/
          and cust.cust-no EQ ttCustList.cust-no
          /*and cust.sman    ge fsls
          and cust.sman    le tsls*/
        no-lock,

        each itemfg
        where itemfg.company eq cocode
          and itemfg.cust-no eq cust.cust-no
        use-index customer no-lock 

        break by cust.sman
              by cust.cust-no:

        {custom/statusMsg.i " 'Processing Customer#/FG item  '  + cust.cust-no + '/' + itemfg.i-no "}


         RUN fg/fgSlsRep.p (INPUT itemfg.company,
                            INPUT itemfg.cust-no,
                            INPUT itemfg.part-no,
                            INPUT itemfg.i-no,
                            OUTPUT cSlsRep).
        CREATE tt-file.
          ASSIGN
            tt-file.tt-sman    = IF cSlsRep = "" THEN cust.sman ELSE cSlsRep 
            tt-file.tt-cust-no = cust.cust-no
            tt-file.tt-i-no    = itemfg.i-no  .
    END.

  for each tt-file WHERE
        tt-file.tt-sman GE fsls
    AND tt-file.tt-sman LE tsls
    ,    
    first cust
    where cust.company eq cocode
      and cust.cust-no eq tt-cust-no
    no-lock,    
    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq tt-i-no
    NO-LOCK    
    break by tt-sman
          by tt-cust-no :
       {custom/statusMsg.i " 'Processing Customer#/FG item  '  + cust.cust-no + '/' + itemfg.i-no "}

    if first-of(tt-sman) then do:
        find first sman
            where sman.company eq cust.company
              and sman.sman    eq tt-file.tt-sman
            no-lock no-error.
        assign
         v-sman  = tt-file.tt-sman
         v-sname = if avail sman then sman.sname else "Not on file".
        PAGE.
      end.

      if first-of(tt-cust-no) then
        assign
         v-frst  = yes
         v-print = no.

      v-bin = no.

      for each tt-report:
        delete tt-report.
      end.

      for each fg-bin
          where fg-bin.company eq cocode
            and fg-bin.i-no    eq itemfg.i-no
            and (vwhs or (fg-bin.loc ne "CUST" and fg-bin.cust-no eq ""))
          use-index i-no,

          each fg-rcpth
          where fg-rcpth.company      eq cocode
            and fg-rcpth.i-no         eq itemfg.i-no
            and fg-rcpth.job-no       eq fg-bin.job-no
            and fg-rcpth.job-no2      eq fg-bin.job-no2
            and fg-rcpth.trans-date   le vdat
          no-lock use-index tran,

          each fg-rdtlh
          where fg-rdtlh.r-no         eq fg-rcpth.r-no
            and fg-rdtlh.loc          eq fg-bin.loc
            and fg-rdtlh.loc-bin      eq fg-bin.loc-bin
            and fg-rdtlh.tag          eq fg-bin.tag
            and fg-rdtlh.rita-code    eq fg-rcpth.rita-code
          NO-LOCK USE-INDEX rm-rdtl

          break by fg-bin.job-no
                by fg-bin.job-no2
                by fg-bin.loc
                by fg-bin.loc-bin
                by fg-bin.tag
                by fg-rcpth.trans-date
                BY fg-rdtlh.trans-time
                by fg-rcpth.r-no:

        v-bin = yes.        

        if index("RATE",fg-rcpth.rita-code) ne 0 then
          v-binqty = v-binqty + fg-rdtlh.qty.

        else
        if fg-rcpth.rita-code eq "C" then
          v-binqty = fg-rdtlh.qty.

        else
        if fg-rcpth.rita-code eq "S" then
          v-binqty = v-binqty - fg-rdtlh.qty.

        if last-of(fg-bin.tag) then
          assign
           v-qoh[1] = v-qoh[1] + v-binqty
           v-binqty = 0.

        if last-of(fg-bin.job-no2) then do:
          find last oe-ordl
              where oe-ordl.company eq cocode
                and oe-ordl.i-no    eq fg-bin.i-no
                and oe-ordl.job-no  eq fg-bin.job-no
                and oe-ordl.job-no2 eq fg-bin.job-no2
              use-index item no-lock no-error.

          if avail oe-ordl then
          find first oe-ord
              where oe-ord.company eq cocode
                and oe-ord.ord-no  eq oe-ordl.ord-no
              no-lock.

          v-date = if avail oe-ordl then oe-ordl.req-date
                   else ?.

          create tt-report.
          assign
           tt-report.key-01 = if v-date eq ? then ""
                              else string(year(v-date),"9999") +
                                   string(month(v-date),"99")  +
                                   string(day(v-date),"99")
           tt-report.qty    = v-qoh[1]
           v-qoh[1]         = 0
           tt-report.rec-id = recid(fg-bin).
        end.
      end.

      for each tt-report,
          first fg-bin where recid(fg-bin) eq tt-report.rec-id no-lock

          by tt-report.key-01
          by fg-bin.job-no
          by fg-bin.job-no2:

        v-qoh[1] = tt-report.qty.

        assign
         v-ord-qty = 0
         v-price   = itemfg.sell-price
         v-uom     = itemfg.sell-uom
         v-cas-cnt = itemfg.case-count
         v-job     = FILL(" ",6 - LENGTH(TRIM(fg-bin.job-no))) +
                     TRIM(fg-bin.job-no) + "-" + STRING(fg-bin.job-no2,"99").

        IF TRIM(v-job) EQ "-00" THEN v-job = "".

        FOR EACH oe-ordl
            WHERE oe-ordl.company EQ cocode
              AND oe-ordl.i-no    EQ fg-bin.i-no
              AND oe-ordl.job-no  EQ fg-bin.job-no
              AND oe-ordl.job-no2 EQ fg-bin.job-no2
            USE-INDEX item NO-LOCK,
            FIRST oe-ord OF oe-ordl 
            WHERE oe-ord.ord-date LE vdat
            NO-LOCK
            BREAK BY oe-ordl.company
                  BY oe-ordl.req-date:

          RUN oe/ordlsqty.p (ROWID(oe-ordl),
                             OUTPUT li-inv-qty, OUTPUT li-ship-qty).

          ASSIGN
           v-ord-qty = oe-ordl.qty
           v-price   = oe-ordl.price
           v-uom     = oe-ordl.pr-uom
           v-cas-cnt = itemfg.case-count

           v-ord[1]  = v-ord[1] + oe-ordl.qty
           v-shp[1]  = v-shp[1] + li-ship-qty.
        END.

        if v-uom eq "L" and v-ord-qty ne 0 then
          v-ext[1] = v-price / v-ord-qty * v-qoh[1].

        else
        if v-uom eq "CS"  and
           v-cas-cnt ne 0 then
          v-ext[1] = (v-qoh[1] * v-price) / v-cas-cnt.

        else do:
          v-ext[1] = v-qoh[1] * v-price.
          find first uom
              where uom.uom  eq v-uom
                and uom.mult ne 0
              no-lock no-error.
          if avail uom then v-ext[1] = v-ext[1] / uom.mult.
        end.

        IF v-qoh[1] NE 0 OR vzer THEN
          IF tb_detail THEN
          FOR EACH oe-ordl
              WHERE oe-ordl.company EQ cocode
                AND oe-ordl.i-no    EQ fg-bin.i-no
                AND oe-ordl.job-no  EQ fg-bin.job-no
                AND oe-ordl.job-no2 EQ fg-bin.job-no2
              USE-INDEX item NO-LOCK,
              FIRST oe-ord OF oe-ordl 
              WHERE oe-ord.ord-date LE vdat
              NO-LOCK
              BREAK BY oe-ordl.company
                    BY oe-ordl.req-date:

            RUN oe/ordlsqty.p (ROWID(oe-ordl),
                               OUTPUT li-inv-qty, OUTPUT li-ship-qty).

            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = IF v-frst  THEN string(cust.cust-no,"x(8)") ELSE "".
                         WHEN "fgitem"   THEN cVarValue = IF FIRST(oe-ordl.company) THEN string(itemfg.i-no,"x(15)") ELSE "".
                         WHEN "cust-part"   THEN cVarValue = IF FIRST(oe-ordl.company) THEN STRING(oe-ordl.part-no,"x(15)") ELSE "".
                         WHEN "desc"  THEN cVarValue = IF FIRST(oe-ordl.company) THEN STRING(itemfg.i-name,"x(30)") ELSE "".
                         WHEN "cust-po"   THEN cVarValue = STRING(oe-ordl.po-no,"x(15)") .
                         WHEN "job"  THEN cVarValue = IF FIRST(oe-ordl.company) THEN STRING(v-job,"x(10)") ELSE "" .
                         WHEN "ord-qty"   THEN cVarValue = STRING(oe-ordl.qty,"->>>,>>>,>>9") .
                         WHEN "ship-qty"  THEN cVarValue = STRING(li-ship-qty,"->>>,>>>,>>9") .
                         WHEN "oh-hand-qty"   THEN cVarValue = IF LAST(oe-ordl.company) THEN STRING(v-qoh[1],"->>>,>>>,>>9") ELSE "" .
                         WHEN "rec-date"  THEN cVarValue = STRING(oe-ordl.req-date,"99/99/9999") .
                         WHEN "sell-pr"   THEN cVarValue = STRING(oe-ordl.price,"->>>>,>>9.99") .
                         WHEN "tot-val"  THEN cVarValue = IF LAST(oe-ordl.company) THEN STRING(v-ext[1],"->>>,>>>,>>9.99") ELSE "" .

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

           /* DISPLAY cust.cust-no        WHEN v-frst
                    itemfg.i-no         WHEN FIRST(oe-ordl.company)
                    oe-ordl.part-no     WHEN FIRST(oe-ordl.company)
                    itemfg.i-name       WHEN FIRST(oe-ordl.company)
                    oe-ordl.po-no
                    v-job               WHEN FIRST(oe-ordl.company)
                    oe-ordl.qty
                    li-ship-qty
                    v-qoh[1]            WHEN LAST(oe-ordl.company)
                    oe-ordl.req-date
                    oe-ordl.price
                    v-ext[1]            WHEN LAST(oe-ordl.company)
              WITH FRAME detail2.
            DOWN WITH FRAME detail2.
            */
          END.

          ELSE DO:

              ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = IF v-frst THEN string(cust.cust-no,"x(8)") ELSE "" .
                         WHEN "fgitem"   THEN cVarValue = string(itemfg.i-no,"x(15)").
                         WHEN "cust-part"   THEN cVarValue = /*STRING(oe-ordl.part-no,"x(15)")*/ "".
                         WHEN "desc"  THEN cVarValue = STRING(itemfg.i-name,"x(30)") .
                         WHEN "cust-po"   THEN cVarValue = /*STRING(oe-ordl.po-no,"x(15)")*/ "" .
                         WHEN "job"  THEN cVarValue = STRING(v-job,"x(10)")  .
                         WHEN "ord-qty"   THEN cVarValue = STRING(v-ord[1],"->>>,>>>,>>9") .
                         WHEN "ship-qty"  THEN cVarValue = STRING(v-shp[1],"->>>,>>>,>>9") .
                         WHEN "oh-hand-qty"   THEN cVarValue = STRING(v-qoh[1],"->>>,>>>,>>9") .
                         WHEN "rec-date"  THEN cVarValue = IF v-date NE ? THEN STRING(v-date,"99/99/9999") ELSE "" .
                         WHEN "sell-pr"   THEN cVarValue = STRING(v-price,"->>>>,>>9.99") .
                         WHEN "tot-val"  THEN cVarValue = STRING(v-ext[1],"->>>,>>>,>>9.99") .

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
           /* DISPLAY cust.cust-no        WHEN v-frst
                    itemfg.i-no
                    itemfg.i-name
                    v-job
                    v-ord[1]
                    v-shp[1]
                    v-qoh[1]
                    v-date
                    v-price
                    v-ext[1]
                WITH FRAME detail1.
            DOWN WITH FRAME detail1.
            */

          END.

        assign
         v-frst   = no
         v-ord[2] = v-ord[2] + v-ord[1]
         v-qoh[2] = v-qoh[2] + v-qoh[1]
         v-ext[2] = v-ext[2] + v-ext[1]
         v-ord[1] = 0
         v-shp[1] = 0
         v-qoh[1] = 0
         v-ext[1] = 0
         v-print  = yes.

        delete tt-report.
      end.

      if vzer and not v-bin then do:

            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue = IF v-frst THEN string(cust.cust-no,"x(8)") ELSE "" .
                         WHEN "fgitem"   THEN cVarValue = string(itemfg.i-no,"x(15)").
                         WHEN "cust-part"   THEN cVarValue = STRING(itemfg.part-no,"x(15)").
                         WHEN "desc"  THEN cVarValue = STRING(itemfg.i-name,"x(30)") .
                         WHEN "cust-po"   THEN cVarValue = /*STRING(oe-ordl.po-no,"x(15)")*/ "" .
                         WHEN "job"  THEN cVarValue = ""  .
                         WHEN "ord-qty"   THEN cVarValue = /*STRING(v-ord[1],">>>>,>>>,>>9")*/ string("           0") .
                         WHEN "ship-qty"  THEN cVarValue = /*STRING(v-shp[1],">>>>,>>>,>>9")*/ string("           0") .
                         WHEN "oh-hand-qty"   THEN cVarValue = /*STRING(v-qoh[1],">>>>,>>>,>>9")*/ string("           0") .
                         WHEN "rec-date"  THEN cVarValue = /*STRING(v-date,"99/99/9999")*/ "" .
                         WHEN "sell-pr"   THEN cVarValue = STRING(itemfg.sell-price,"->>>>,>>9.99") .
                         WHEN "tot-val"  THEN cVarValue = /*STRING(v-ext[1],"->,>>>,>>9.99")*/ string("              0") .

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
         /* DISPLAY cust.cust-no          WHEN v-frst
                  itemfg.i-no
                  itemfg.part-no        @ oe-ordl.part-no
                  itemfg.i-name
                  ""                    @ oe-ordl.po-no
                  0                     @ oe-ordl.qty
                  0                     @ li-ship-qty
                  0                     @ v-qoh[1]
                  itemfg.sell-price     @ oe-ordl.price
                  0                     @ v-ext[1]
              WITH FRAME detail2.
          DOWN WITH FRAME detail2.

         */


        assign
         v-frst  = no
         v-print = yes.
      end.

      if last-of(tt-cust-no) then do:
        IF v-print                 AND
           (v-qoh[2] NE 0 OR vzer) THEN DO:

            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue =   "" .
                         WHEN "fgitem"   THEN cVarValue = "".
                         WHEN "cust-part"   THEN cVarValue = "".
                         WHEN "desc"  THEN cVarValue = "" .
                         WHEN "cust-po"   THEN cVarValue =   "" .
                         WHEN "job"  THEN cVarValue = ""  .
                         WHEN "ord-qty"   THEN cVarValue = STRING(v-ord[2],"->>>,>>>,>>9") .
                         WHEN "ship-qty"  THEN cVarValue = STRING(v-shp[2],"->>>,>>>,>>9") .
                         WHEN "oh-hand-qty"   THEN cVarValue = STRING(v-qoh[2],"->>>,>>>,>>9") .
                         WHEN "rec-date"  THEN cVarValue =  "" .
                         WHEN "sell-pr"   THEN cVarValue = "" .
                         WHEN "tot-val"  THEN cVarValue = STRING(v-ext[2],"->>>,>>>,>>9.99") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
           PUT str-line SKIP .
            PUT UNFORMATTED "            Customer Total" substring(cDisplay,27,300) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                      'Customer Total ,' substring(cExcelDisplay,4,300) SKIP.
             END.


          PUT SKIP(1).
        END.

        assign
         v-ord[3] = v-ord[3] + v-ord[2]
         v-shp[3] = v-shp[3] + v-shp[2]
         v-qoh[3] = v-qoh[3] + v-qoh[2]
         v-ext[3] = v-ext[3] + v-ext[2]

         v-ord[2] = 0
         v-shp[2] = 0
         v-qoh[2] = 0
         v-ext[2] = 0.
      end.

      if last-of(tt-sman) then do:
        IF v-print                 AND
           (v-qoh[3] NE 0 OR vzer) THEN DO:

            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue =   "" .
                         WHEN "fgitem"   THEN cVarValue = "".
                         WHEN "cust-part"   THEN cVarValue = "".
                         WHEN "desc"  THEN cVarValue = "" .
                         WHEN "cust-po"   THEN cVarValue =   "" .
                         WHEN "job"  THEN cVarValue = ""  .
                         WHEN "ord-qty"   THEN cVarValue = STRING(v-ord[3],"->>>,>>>,>>9") .
                         WHEN "ship-qty"  THEN cVarValue = STRING(v-shp[3],"->>>,>>>,>>9") .
                         WHEN "oh-hand-qty"   THEN cVarValue = STRING(v-qoh[3],"->>>,>>>,>>9") .
                         WHEN "rec-date"  THEN cVarValue =  "" .
                         WHEN "sell-pr"   THEN cVarValue = "" .
                         WHEN "tot-val"  THEN cVarValue = STRING(v-ext[3],"->>>,>>>,>>9.99") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
           PUT str-line SKIP .
            PUT UNFORMATTED "            SalesRep Total" substring(cDisplay,27,300) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                      'SalesRep Total ,' substring(cExcelDisplay,4,300) SKIP.
             END.

          PUT SKIP(1).
        END.

        assign
         v-ord[4] = v-ord[4] + v-ord[3]
         v-shp[4] = v-shp[4] + v-shp[3]
         v-qoh[4] = v-qoh[4] + v-qoh[3]
         v-ext[4] = v-ext[4] + v-ext[3]

         v-ord[3] = 0
         v-shp[3] = 0
         v-qoh[3] = 0
         v-ext[3] = 0.
      end.

      IF LAST(tt-sman)         AND
         (v-qoh[4] NE 0 OR vzer) THEN DO:

          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "cust"    THEN cVarValue =   "" .
                         WHEN "fgitem"   THEN cVarValue = "".
                         WHEN "cust-part"   THEN cVarValue = "".
                         WHEN "desc"  THEN cVarValue = "" .
                         WHEN "cust-po"   THEN cVarValue =   "" .
                         WHEN "job"  THEN cVarValue = ""  .
                         WHEN "ord-qty"   THEN cVarValue = STRING(v-ord[4],"->>>,>>>,>>9") .
                         WHEN "ship-qty"  THEN cVarValue = STRING(v-shp[4],"->>>,>>>,>>9") .
                         WHEN "oh-hand-qty"   THEN cVarValue = STRING(v-qoh[4],"->>>,>>>,>>9") .
                         WHEN "rec-date"  THEN cVarValue =  "" .
                         WHEN "sell-pr"   THEN cVarValue = "" .
                         WHEN "tot-val"  THEN cVarValue = STRING(v-ext[4],"->>>,>>>,>>9.99") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
           PUT str-line SKIP .
            PUT UNFORMATTED "               Grand Total" substring(cDisplay,27,300) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                      'Grand Total ,' substring(cExcelDisplay,4,300) SKIP.
             END.

        /*IF tb_detail THEN
          PUT "--------------" TO 164 SKIP
              "   Grand Total" AT 126 v-ext[4] TO 164 FORMAT "->>,>>>,>>9.99".

        ELSE
          PUT "--------------" TO 132 SKIP
              "   Grand Total" AT 94  v-ext[4] TO 132 FORMAT "->>,>>>,>>9.99".*/

        PUT SKIP(1).
      END.
    END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2002 Advanced Software, Inc. */

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCustRange C-Win 
PROCEDURE SetCustRange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
        begin_cust-no:SENSITIVE = NOT iplChecked
        end_cust-no:SENSITIVE = NOT iplChecked
        begin_cust-no:VISIBLE = NOT iplChecked
        end_cust-no:VISIBLE = NOT iplChecked
        btnCustList:SENSITIVE = iplChecked
       .
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

  ASSIGN
  lv-frame-hdl = frame {&frame-name}:HANDLE
  lv-group-hdl = lv-frame-hdl:first-child
  lv-field-hdl = lv-group-hdl:first-child.

  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + ",".
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
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

