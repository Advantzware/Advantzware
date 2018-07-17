&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: cerep/r-prep.w

  Description: Prep code list

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

{sys/inc/var.i new shared}

ASSIGN
 cocode = gcompany
 locode = gloc.


DEFINE VARIABLE v-print-fmt AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
DEFINE VARIABLE security-flag AS LOG NO-UNDO.


/* gdm - 10130803 */
DEFINE VARIABLE v_exclhdr1 AS CHARACTER                NO-UNDO.
DEFINE VARIABLE v_exclhdr2 AS CHARACTER                NO-UNDO.
DEFINE VARIABLE v_custnum  AS CHARACTER FORMAT "x(35)" NO-UNDO.
DEFINE VARIABLE v_ML       AS CHARACTER INIT "M"       NO-UNDO.
DEFINE VARIABLE v_dfault   AS CHARACTER INIT "Y"       NO-UNDO.

/* gdm - 10130803 */
DEFINE STREAM excel.

DEFINE VARIABLE ldummy AS LOG NO-UNDO.
DEFINE VARIABLE cTextListToSelect AS cha NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS cha NO-UNDO.
DEFINE VARIABLE cFieldLength AS cha NO-UNDO.
DEFINE VARIABLE cFieldType AS cha NO-UNDO.
DEFINE VARIABLE iColumnLength AS INTEGER NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "Code,Desc.,Customer Name,Whse,Bin Loc,Dspsl Dt,Lst Usd Dt," +
                            "Markup,Cost,M/L,Amtz,M Type,Use w/Est,UOM,SIMON,C Type,Account No,Cad #,File #," +
                            "Customer #,Last Estimate,Last Job,Has Note,Box Style,Price," +
                            "Length,Width,Depth,Number Up,Die Width,Die Length,# of Impressions,Date Received," +  /*8*/
                            "FG Category,RM Category,RM Item #,Owner 1,Owner 1 %,Owner 2,Owner 2 %"  /*7*/
       cFieldListToSelect = "code,dscr,cust-name,ware,bin,dis-dt,lst-dt," +
                             "mrkup,cst,ml,amtz,m-typ,use-est,uom,simon,c-typ,act-no,cad-no,file-no," +
                             "cust,lst-est,lst-job,has-not,prep.box-style,prep.spare-dec-1," +
                             "prep.carton-l,prep.carton-w,prep.carton-d,prep.number-up,prep.die-w,prep.die-l,prep.no-of-impressions,received-date," +
                             "prep.fgcat,prep.procat,prep.i-no,owner1,owner%1,owner2,owner%2"
       cFieldLength = "15,30,30,5,8,10,10," + "6,10,3,6,6,9,3,5,6,30,15,15," + "10,13,10,8,10,8," + "10,10,10,10,10,10,15,13," + "11,11,15,25,9,25,9"
       cFieldType = "c,c,c,c,c,c,c," + "i,i,c,i,c,c,c,c,c,c,c,c," + "c,c,c,c,c,i," + "i,i,i,i,i,i,i,c," + "c,c,c,c,i,c,i"
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Code,Desc.,Customer Name,Whse,Bin Loc,Dspsl Dt,Lst Usd Dt," +
                            "Markup,Cost,M/L,Amtz,M Type,Use w/Est,UOM,SIMON,C Type,Account No,Cad #,File #".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 begin_prep end_prep ~
rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
tb_excel tb_runExcel fi_file btn-ok btn-cancel sl_avail Btn_Def sl_selected Btn_Add ~
 Btn_Remove btn_Up btn_down
&Scoped-Define DISPLAYED-OBJECTS begin_prep end_prep rd-dest ~
lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel ~
tb_runExcel fi_file sl_avail sl_selected

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
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

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

DEFINE VARIABLE begin_prep AS CHARACTER FORMAT "x(15)" 
     LABEL "Beginning  Prep Code" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE end_prep AS CHARACTER FORMAT "x(15)" INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Prep Code" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-prep.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

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

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 5.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.76.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_batch AS LOGICAL INITIAL NO 
     LABEL "Run In Batch Mode?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL NO 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_prep AT ROW 2.67 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Prep Code"
     end_prep AT ROW 4.1 COL 27 COLON-ALIGNED HELP
          "Enter Ending Prep Code"
     sl_avail AT ROW 6.86 COL 4.4 NO-LABELS WIDGET-ID 26
     Btn_Def AT ROW 6.86 COL 40.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 6.86 COL 59.8 NO-LABELS WIDGET-ID 28
     Btn_Add AT ROW 7.86 COL 40.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 8.86 COL 40.4 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 9.91 COL 40.4 WIDGET-ID 40
     btn_down AT ROW 10.91 COL 40.4 WIDGET-ID 42
     rd-dest AT ROW 13.14 COL 5 NO-LABELS
     lv-ornt AT ROW 13.86 COL 31 NO-LABELS
     lines-per-page AT ROW 13.86 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 15.76 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 16.71 COL 28 COLON-ALIGNED NO-LABELS
     td-show-parm AT ROW 18.62 COL 30
     tb_excel AT ROW 19.71 COL 67.4 RIGHT-ALIGNED WIDGET-ID 4
     tb_runExcel AT ROW 19.71 COL 91.4 RIGHT-ALIGNED WIDGET-ID 6
     tb_batch AT ROW 20.05 COL 6
     fi_file AT ROW 20.67 COL 45.4 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 2
     btn-ok AT ROW 22.29 COL 18
     btn-cancel AT ROW 22.29 COL 57
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 6.14 COL 5.2 WIDGET-ID 38
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.43 COL 2
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 6.14 COL 59.8 WIDGET-ID 44
     RECT-7 AT ROW 1 COL 1
     RECT-8 AT ROW 12.19 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 22.86.


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
         TITLE              = "Preparation Code List"
         HEIGHT             = 23.57
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
       begin_prep:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_prep:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_batch IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_batch:HIDDEN IN FRAME FRAME-A           = TRUE.

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
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Preparation Code List */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Preparation Code List */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_prep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_prep C-Win
ON LEAVE OF begin_prep IN FRAME FRAME-A /* Beginning  Prep Code */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

   /* gdm - 10130803 */
  /*ASSIGN
    v_exclhdr1 = "Code,Desc.,Customer Name,Warehouse,Bin Location,Disposal Date,Last Used Date,Markup,Cost,M/L,Amtz,M Type,Use w/ Est,UOM,SIMON,C Type,Account No,Cad #,File #"
    v_exclhdr2 = "Code,Description,Markup,Cost,M/L,Amtz,Mat'l Type,Use in all E,UOM,SIMON,Account No".

  IF tb_excel THEN DO:
      OUTPUT STREAM excel TO VALUE(fi_file).

      IF tb_cust-name 
        THEN
          PUT STREAM excel UNFORMATTED
            v_exclhdr1
           SKIP.
        ELSE
          PUT STREAM excel UNFORMATTED
            v_exclhdr2
           SKIP.

  END. */

  IF g_batch THEN tb_batch = YES.
  /*IF tb_batch THEN DO:
     RUN run-batch.
     RETURN NO-APPLY.
  END. */

  /*IF tb_cust-name THEN run run-report.
  ELSE RUN run-report-sum. */

  RUN GetSelectionList.
  RUN run-report. 

 CASE rd-dest:
      WHEN 1 THEN RUN output-to-printer.
      WHEN 2 THEN RUN output-to-screen.
      WHEN 3 THEN RUN output-to-file.
      WHEN 4 THEN DO:
             /*run output-to-fax.*/
           {custom/asifax.i &type= " "
                            &begin_cust= "begin_prep"
                            &END_cust= "begin_prep" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = " "
                             &begin_cust= "begin_prep"
                             &END_cust= "begin_prep"
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = " "
                                  &begin_cust="begin_prep"
                                  &END_cust="begin_prep"
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
DO:
  DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

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
  DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

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

&Scoped-define SELF-NAME end_prep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_prep C-Win
ON LEAVE OF end_prep IN FRAME FRAME-A /* Ending Prep Code */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEFINE VARIABLE char-val AS cha NO-UNDO.

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
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
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

  IF g_batch THEN tb_batch = YES.

  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
        RUN DisplaySelectionList2.
       APPLY "entry" TO begin_prep.
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
  DEFINE VARIABLE cListContents AS cha NO-UNDO.
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

  DEFINE VARIABLE cListContents AS cha NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

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
  DEFINE VARIABLE cListContents AS cha NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
  DEFINE VARIABLE cTmpList AS cha NO-UNDO.

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
  DISPLAY begin_prep end_prep rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file sl_avail sl_selected
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 RECT-8 begin_prep end_prep rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down
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
 DEFINE VARIABLE cTmpList AS cha NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
 iColumnLength = 0.

 DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.     

    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
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
  RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-batch C-Win 
PROCEDURE run-batch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {BATCH/runbatch.i "windows\r-booked.r"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/***************************************************************************\
*****************************************************************************
**  Program: /u2/fold/all/dev/asi/oe/rep
**       by: Christopher A. Heins, 07.14.95
** Descript: Salesman Performance daily, period and year to date.
**
*****************************************************************************
\***************************************************************************/
DEFINE VARIABLE ii LIKE i NO-UNDO.

/* gdm - 10130803 */
DEFINE VARIABLE v_exclhdr1 AS CHARACTER                NO-UNDO.
DEFINE VARIABLE v_exclhdr2 AS CHARACTER                NO-UNDO.
DEFINE VARIABLE v_custnum  AS CHARACTER FORMAT "x(35)" NO-UNDO.

/*{sys/form/r-topl.f}*/


DEFINE VARIABLE cDisplay AS cha NO-UNDO.
DEFINE VARIABLE cExcelDisplay AS cha NO-UNDO.
DEFINE VARIABLE hField AS HANDLE NO-UNDO.
DEFINE VARIABLE cTmpField AS CHA NO-UNDO.
DEFINE VARIABLE cVarValue AS cha NO-UNDO.
DEFINE VARIABLE cExcelVarValue AS cha NO-UNDO.
DEFINE VARIABLE cSelectedList AS cha NO-UNDO.
DEFINE VARIABLE cFieldName AS cha NO-UNDO.
DEFINE VARIABLE str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE str-tit5 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE str-line AS cha FORM "x(300)" NO-UNDO.
DEFINE VARIABLE v-lst-job    AS CHARACTER NO-UNDO.

DEFINE BUFFER bfprep FOR prep .

{sys/form/r-topsw.f}
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.

ASSIGN str-tit2 = c-win:TITLE
         {sys/inc/ctrtext.i str-tit2 112}.


DEFINE VARIABLE cslist AS cha NO-UNDO.
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

{sys/inc/outprint.i value(lines-per-page)}

IF td-show-parm THEN RUN show-param.

DISPLAY "" WITH FRAME r-top.
PUT  str-tit4 FORMAT "x(600)"
     SKIP
     str-tit5 FORMAT "x(600)"
     SKIP
    .

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

SESSION:SET-WAIT-STATE ("general").

v-lst-job   = "" .

FOR EACH prep WHERE prep.company = g_company
                 AND prep.CODE >= begin_prep
                 AND prep.CODE <= END_prep NO-LOCK BY prep.CODE:

   FIND FIRST notes WHERE notes.rec_key EQ prep.rec_key NO-LOCK NO-ERROR .


    ASSIGN v_ML     = IF prep.ml = TRUE THEN "M" ELSE "L"
           v_dfault = IF prep.dfault = TRUE THEN "Y" ELSE "N" .
           v-lst-job = TRIM(STRING(prep.last-job-no)  + "-" + string(prep.last-job-no2,"99"))    .
        

    ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            BUFFER bfprep:FIND-BY-ROWID(ROWID(prep), NO-LOCK) .
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                   IF INDEX(cTmpField,".") > 0 THEN DO:
                            cFieldName = cTmpField.
                            cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                            hField = BUFFER bfprep:BUFFER-FIELD(cTmpField).
                            IF hField <> ? THEN DO:                 
                                cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                                cDisplay = cDisplay + 
                                          IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldType) = "C" THEN
                                            (cTmpField + FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)))
                                          ELSE IF LENGTH(cTmpField) <  int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) THEN
                                            (FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) - LENGTH(cTmpField)) + cTmpField) + " "
                                          ELSE cTmpField.
                                cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".   

                            END.
                            ELSE DO:
                               cTmpField = SUBSTRING(cFieldName,1,int( ENTRY( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                               cDisplay = cDisplay + FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                               cExcelDisplay = cExcelDisplay + quoter(" ") + ",".
                            END.
                  END.
                  ELSE DO: 

                    CASE cTmpField:             
                         WHEN "code"                THEN cVarValue =  STRING(prep.CODE,"x(15)") .
                         WHEN "dscr"                  THEN cVarValue =  STRING(prep.dscr) .
                         WHEN "cust-name"        THEN cVarValue =  STRING(prep.cust-name) .
                         WHEN "ware"                 THEN cVarValue = STRING(prep.loc) .
                         WHEN "bin"                    THEN cVarValue = STRING(prep.loc-bin) .
                         WHEN "dis-dt"                 THEN cVarValue = IF prep.disposal-date NE ? THEN STRING(prep.disposal-date)  ELSE "" .
                         WHEN "lst-dt"                  THEN cVarValue = IF prep.last-date NE ? THEN STRING(prep.last-date)  ELSE "".
                         WHEN "mrkup"                THEN cVarValue = STRING(prep.mkup,">>9.99") .
                         WHEN "cst"                     THEN cVarValue = STRING(prep.cost,"->>,>>9.99") .
                         WHEN "ml"                      THEN cVarValue = STRING(v_ML) .
                         WHEN "amtz"                  THEN cVarValue =  STRING(prep.amtz,">>9.99") .
                         WHEN "m-typ"                 THEN cVarValue =  STRING(prep.mat-type) .
                         WHEN "use-est"              THEN cVarValue =  STRING(v_dfault) .
                         WHEN "uom"                   THEN cVarValue =  STRING(prep.uom) .
                         WHEN "simon"                 THEN cVarValue =  STRING(prep.simon) .
                         WHEN "c-typ"                  THEN cVarValue =  STRING(prep.cost-type) .
                         WHEN "act-no"                THEN cVarValue = STRING(prep.actnum) .
                         WHEN "cad-no"               THEN cVarValue =   prep.cadNo.
                         WHEN "file-no"                 THEN cVarValue =  prep.fileNo.
                         WHEN "cust"      THEN cVarValue = prep.cust-no   .
                         WHEN "lst-est"   THEN cVarValue = prep.last-est-no  .
                         WHEN "lst-job"   THEN cVarValue = IF v-lst-job NE "-00" AND v-lst-job NE "" THEN  v-lst-job ELSE "" .
                         WHEN "has-not"   THEN cVarValue = IF AVAILABLE notes THEN "Yes" ELSE "No" .    
                         WHEN "owner1"                THEN cVarValue =  STRING(prep.owner[1]) .
                         WHEN "owner%1"               THEN cVarValue =  STRING(prep.owner-%[1]) .
                         WHEN "owner2"                THEN cVarValue =  STRING(prep.owner[2]) .
                         WHEN "owner%2"               THEN cVarValue =  STRING(prep.owner-%[2]) .
                         WHEN "received-date"               THEN cVarValue = IF prep.received-date <> ?THEN  STRING(prep.received-date) ELSE "" .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".   
                  END.
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.  

   /* IF tb_cust-name THEN DO:

       DISPLAY prep.code FORMAT "x(15)"
               prep.dscr
               prep.cust-name 
               prep.loc       
               prep.loc-bin   LABEL "Bin"
               prep.disposal-date COLUMN-LABEL "Disposal!Date"
               prep.last-date    COLUMN-LABEL "Last Used!Date"  
               prep.mkup
               prep.cost
               prep.ml
               prep.amtz
               prep.mat-type COLUMN-LABEL 'M!Type'
               prep.dfault COLUMN-LABEL 'Use!w/ Est'
               prep.uom
               prep.simon
               prep.cost-type COLUMN-LABEL 'C!Type'
               prep.actnum 
               reftable.code  WHEN AVAIL reftable COLUMN-LABEL 'Cad #' FORMAT "x(15)"
               reftable.code2 WHEN AVAIL reftable COLUMN-LABEL 'File #' FORMAT "x(15)"              
           WITH STREAM-IO FRAME prep-det DOWN WIDTH 230 NO-BOX.

       /* gdm - 10130803 */
       IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
              '"' prep.code              '",'
              '"' prep.dscr          '",'    
              '"' prep.cust-name     '",'
              '"' prep.loc           '",'
              '"' prep.loc-bin       '",'
              '"' prep.disposal-date '",'
              '"' prep.last-date     '",'
              '"' prep.mkup          '",'    
              '"' prep.cost          '",'    
              '"' v_ML               '",'    
              '"' prep.amtz          '",'    
              '"' prep.mat-type      '",'
              '"' v_dfault           '",'
              '"' prep.uom           '",'    
              '"' prep.simon         '",'    
              '"' prep.cost-type     '",'
              '"' prep.actnum        '",'
              '"' IF AVAIL reftable THEN reftable.CODE ELSE ""      '",'
              '"' IF AVAIL reftable THEN reftable.code2 ELSE ""     '"'
             SKIP.



     /*ELSE DISPLAY prep.code
               prep.dscr
               prep.mkup
               prep.cost
               prep.ml
               prep.amtz
               prep.mat-type COLUMN-LABEL 'MType'
               prep.dfault COLUMN-LABEL 'Use w/ Est'
               prep.uom
               prep.simon
               prep.cost-type COLUMN-LABEL 'CType'
               prep.actnum WITH STREAM-IO FRAME prep DOWN WIDTH 132 NO-BOX.
*/
      END.          
      ELSE DO:

         /* gdm - 10130803*/
         IF tb_excel THEN DO:
             FIND FIRST account NO-LOCK
                 WHERE account.company = cocode 
                   AND account.actnum = prep.actnum NO-ERROR.

             ASSIGN
                 v_custnum = ""
                 v_custnum = prep.actnum + " " + account.dscr.            

             PUT STREAM excel UNFORMATTED
               '"' prep.code     '",' 
               '"' prep.dscr     '",' 
               '"' prep.mkup     '",' 
               '"' prep.cost     '",' 
               '"' v_ML          '",' 
               '"' prep.amtz     '",' 
               '"' prep.mat-type '",' 
               '"' prep.dfault   '",' 
               '"' prep.uom      '",' 
               '"' prep.simon    '",' 
               '"' v_custnum     '"' 
              SKIP.
         END. /* IF tb_excel */

         DO WITH FRAME prep 3 COLUMNS STREAM-IO:
             display skip(2).
             {ce/prep.v}.
             DOWN.
         END.

      END.    */
END.

IF tb_excel THEN DO:
    OUTPUT STREAM excel CLOSE.
    IF tb_runExcel THEN
        OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.


SESSION:SET-WAIT-STATE ("").

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-sum C-Win 
PROCEDURE run-report-sum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ii LIKE i NO-UNDO.

{sys/form/r-top.f}

ASSIGN str-tit2 = c-win:TITLE
         {sys/inc/ctrtext.i str-tit2 56}.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF td-show-parm THEN RUN show-param.

DISPLAY "" WITH FRAME r-top.

SESSION:SET-WAIT-STATE ("general").

FOR EACH prep WHERE prep.company = g_company
                 AND prep.CODE >= begin_prep
                 AND prep.CODE <= END_prep NO-LOCK BY prep.CODE:


    /* gdm - 10130803*/
    ASSIGN v_ML     = IF prep.ml = TRUE THEN "M" ELSE "L"
           v_dfault = IF prep.dfault = TRUE THEN "Y" ELSE "N".

    IF tb_excel THEN DO:
        FIND FIRST account NO-LOCK
            WHERE account.company = cocode 
              AND account.actnum = prep.actnum NO-ERROR.

        ASSIGN
            v_custnum = ""
            v_custnum = prep.actnum + " " + account.dscr.            

        PUT STREAM excel UNFORMATTED
           '"' prep.code     '",' 
           '"' prep.dscr     '",' 
           '"' prep.mkup     '",' 
           '"' prep.cost     '",' 
           '"' v_ML          '",' 
           '"' prep.amtz     '",' 
           '"' prep.mat-type '",' 
           '"' v_dfault      '",' 
           '"' prep.uom      '",' 
           '"' prep.simon    '",' 
           '"' v_custnum     '"' 
         SKIP.
    END. /* IF tb_excel */

     DO WITH FRAME prep 3 COLUMNS STREAM-IO:
          DISPLAY SKIP(2).
          {ce/prep.v}.
          DOWN.
     END.

END.

IF tb_excel THEN DO:
    OUTPUT STREAM excel CLOSE.
    IF tb_runExcel THEN
        OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.


SESSION:SET-WAIT-STATE ("").

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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
  DEFINE VARIABLE parm-fld-list AS cha NO-UNDO.
  DEFINE VARIABLE parm-lbl-list AS cha NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-label AS cha NO-UNDO.

  ASSIGN
  lv-frame-hdl = FRAME {&frame-name}:HANDLE
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD
  lv-field-hdl = lv-group-hdl:FIRST-CHILD.

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
        THEN DO:
           IF lv-field-hdl:LABEL <> ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + ",".
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                  IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN DO:
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
       entry(i,parm-lbl-list) NE "" THEN DO:

      lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                 trim(ENTRY(i,parm-lbl-list)) + ":".

      PUT lv-label FORMAT "x(35)" AT 5
          SPACE(1)
          TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
          SKIP.              
    END.
  END.

  PUT FILL("-",80) FORMAT "x(80)" SKIP.

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
