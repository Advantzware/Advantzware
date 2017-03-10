&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fgrep\r-unfgdt.w

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

/*{sys/inc/custlistform.i ""IL3"" }*/

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

DEF TEMP-TABLE tt-report LIKE report.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

DEF STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rd_ord-stat begin_ord-date ~
end_ord-date begin_ord-no end_ord-no tb_cust-list btnCustList begin_cust ~
end_cust begin_ship end_ship begin_i-no end_i-no begin_whse end_whse ~
rd_sort rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel ~
tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lbl_ord-stat rd_ord-stat begin_ord-date ~
end_ord-date begin_ord-no end_ord-no tb_cust-list begin_cust end_cust ~
begin_ship end_ship begin_i-no end_i-no begin_whse end_whse lbl_sort ~
rd_sort rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm ~
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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ship AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Ship-To#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_whse AS CHARACTER FORMAT "X(5)" 
     LABEL "Beginning Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ship AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Ship-To#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_whse AS CHARACTER FORMAT "X(5)" INITIAL "zzz" 
     LABEL "Ending Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-unfgdt.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_ord-stat AS CHARACTER FORMAT "X(256)":U INITIAL "Order Status?" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 93 
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

DEFINE VARIABLE rd_ord-stat AS CHARACTER INITIAL "Open" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"All", "All"
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer#", "Customer#",
"Item#", "Item#"
     SIZE 31 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 11.91.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .95 NO-UNDO.

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

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     lbl_ord-stat AT ROW 2.38 COL 26 COLON-ALIGNED NO-LABEL
     rd_ord-stat AT ROW 2.38 COL 43 NO-LABEL
     begin_ord-date AT ROW 3.81 COL 26 COLON-ALIGNED
     end_ord-date AT ROW 3.81 COL 69 COLON-ALIGNED HELP
          "Enter Ending Order Date"
     begin_ord-no AT ROW 4.76 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_ord-no AT ROW 4.76 COL 69 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     tb_cust-list AT ROW 6.05 COL 29.4 WIDGET-ID 6
     btnCustList AT ROW 6.1 COL 61.4 WIDGET-ID 8
     begin_cust AT ROW 7.24 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 7.24 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_ship AT ROW 8.19 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Ship-To Number"
     end_ship AT ROW 8.19 COL 69 COLON-ALIGNED HELP
          "Enter Ending Ship-To Number"
     begin_i-no AT ROW 9.14 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 9.14 COL 69 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_whse AT ROW 10.1 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Warehouse"
     end_whse AT ROW 10.1 COL 69 COLON-ALIGNED HELP
          "Enter Ending Warehouse"
     lbl_sort AT ROW 11.48 COL 26 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 11.48 COL 35 NO-LABEL
     rd-dest AT ROW 14.1 COL 5 NO-LABEL
     lv-ornt AT ROW 14.1 COL 32 NO-LABEL
     lines-per-page AT ROW 14.1 COL 85 COLON-ALIGNED
     lv-font-no AT ROW 15.52 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 16.48 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17.67 COL 31
     tb_excel AT ROW 18.62 COL 71 RIGHT-ALIGNED
     tb_runExcel AT ROW 18.62 COL 93 RIGHT-ALIGNED
     fi_file AT ROW 19.57 COL 49 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 21.43 COL 25
     btn-cancel AT ROW 21.43 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13.14 COL 2
     RECT-6 AT ROW 12.91 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 22.52.


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
         TITLE              = "Unshipped Finished Goods Detail"
         HEIGHT             = 22.76
         WIDTH              = 95
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
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ship:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_whse:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ship:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_whse:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_ord-stat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_ord-stat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_ord-stat".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_ord-stat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Unshipped Finished Goods Detail */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Unshipped Finished Goods Detail */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-date C-Win
ON LEAVE OF begin_ord-date IN FRAME FRAME-A /* Beginning Order Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* Beginning Order# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ship
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship C-Win
ON LEAVE OF begin_ship IN FRAME FRAME-A /* Beginning Ship-To# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ship
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship C-Win
ON HELP OF begin_ship IN FRAME FRAME-A /* Beginning Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-shipt4.w (cocode,"","",FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_whse C-Win
ON LEAVE OF begin_whse IN FRAME FRAME-A /* Beginning Warehouse */
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

  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT tb_cust-list OR  NOT AVAIL ttCustList THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust,
                    INPUT end_cust).
  END.

  run run-report. 
  STATUS DEFAULT "Processing Complete".

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=END_cust 
                            &END_cust=END_cust 
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:title 
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = ''
                             &begin_cust= END_cust 
                             &END_cust=END_cust 
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust=END_cust 
                                  &END_cust=END_cust 
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
DO:
  RUN CustList.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-date C-Win
ON LEAVE OF end_ord-date IN FRAME FRAME-A /* Ending Order Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* Ending Order# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ship
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ship C-Win
ON LEAVE OF end_ship IN FRAME FRAME-A /* Ending Ship-To# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ship
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ship C-Win
ON HELP OF end_ship IN FRAME FRAME-A /* Beginning Customer# */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-shipt4.w (cocode,"","",FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_whse C-Win
ON LEAVE OF end_whse IN FRAME FRAME-A /* Ending Warehouse */
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


&Scoped-define SELF-NAME rd_ord-stat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_ord-stat C-Win
ON VALUE-CHANGED OF rd_ord-stat IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
DO:
  assign {&self-name}.
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

    assign
      begin_ord-date = date(01,01,year(today))
      END_ord-date   = DATE(12,31,YEAR(TODAY)).     
  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "IL3",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO lbl_ord-stat.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'IL3',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""IL3""}

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
                            INPUT 'IL3',
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report C-Win 
PROCEDURE create-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var v-loc like oe-boll.loc.


  FOR each oe-ordl of oe-ord
      where oe-ordl.i-no    ge begin_i-no
        and oe-ordl.i-no    le end_i-no
      no-lock,

      each oe-rel
      where oe-rel.company eq cocode
        and oe-rel.ord-no  eq oe-ordl.ord-no
        and oe-rel.i-no    eq oe-ordl.i-no
        and oe-rel.line    eq oe-ordl.line
        and oe-rel.ship-id ge begin_ship
        and oe-rel.ship-id le end_ship
      no-lock,

      first shipto of oe-rel no-lock:

      {custom/statusMsg.i " 'Processing FG Item  '  + oe-ordl.i-no"}

    {fg/rep/fg-unsum.i}

    if v-loc ge begin_whse and v-loc le end_whse then do:
      create tt-report.
      assign
       tt-report.rec-id  = recid(oe-rel)
       tt-report.key-01  = if rd_sort EQ "Customer#" then oe-ord.cust-no
                                                     else oe-ordl.i-no
       tt-report.key-02  = if rd_sort EQ "Customer#" then oe-rel.ship-id
                                                     else oe-ord.cust-no
       tt-report.key-03  = if rd_sort EQ "Customer#" then oe-ordl.i-no
                                                     else oe-rel.ship-id
       tt-report.key-08  = oe-ord.cust-no
       tt-report.key-09  = oe-rel.ship-id.
    end.
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
                                  INPUT 'IL3').


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
  DISPLAY lbl_ord-stat rd_ord-stat begin_ord-date end_ord-date begin_ord-no 
          end_ord-no tb_cust-list begin_cust end_cust begin_ship end_ship 
          begin_i-no end_i-no begin_whse end_whse lbl_sort rd_sort rd-dest 
          lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 rd_ord-stat begin_ord-date end_ord-date begin_ord-no 
         end_ord-no tb_cust-list btnCustList begin_cust end_cust begin_ship 
         end_ship begin_i-no end_i-no begin_whse end_whse rd_sort rd-dest 
         lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel 
         fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
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

     IF NOT OKpressed THEN  RETURN NO-APPLY. */

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
/* ---------------------------------------------- fg/rep/fg-undet.p 05/98 JLF */
/* Detail of Unshipped Orders                                                 */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var v-ford         like oe-ord.ord-no format ">>>>>>".
def var v-tord         like v-ford                   init 999999.
def var v-fdate        as   date format "99/99/9999" init 01/01/0001.
def var v-tdate        like v-fdate                  init 12/31/9999.
def var v-fcust        like oe-ord.cust-no           init "".
def var v-tcust        like v-fcust                  init "zzzzzzzz".
def var v-fship        like oe-rel.ship-id           init "".
def var v-tship        like v-fship                  init "zzzzzzzz".
def var v-fitem        like itemfg.i-no              init "".
def var v-titem        like v-fitem                  init "zzzzzzzzzzzzzzz".
def var v-floc         like oe-rell.loc              init "".
def var v-tloc         like v-floc                   init "zzzzz".
def var v-stat         as   char format "!"          init "O".
def var v-cust-sort    as   log format "Cust/Item"   init yes.

def var v-ono          as   int format "->>>>>>>>9" extent 4.
def var v-shp          like v-ono.
def var v-bol          like v-ono.

def var v-bal          as   int format "->>>>>>>>9".

def var v-rel          as   log.
def var v-first        like tt-report.key-02 extent 3.
def var v-loc          like oe-boll.loc.

def var str-tit4       like str-tit3.
def var v-label1       as   char format "x(24)" extent 2.
def var v-label2       as   char format "x(21)" extent 2.
DEF VAR excelheader    AS CHAR NO-UNDO.

&Scoped-define where-phrase where oe-ord.company  eq cocode  ~
                              and oe-ord.ord-no   le v-tord  ~
                              and oe-ord.ord-date ge v-fdate ~
                              and oe-ord.ord-date le v-tdate ~
                              and oe-ord.cust-no  EQ ttCustList.cust-no ~
                              and oe-ord.stat     ne "D"

form header
     str-tit4 format "x(130)"
     skip(1)

     v-label1[1]
     v-label2[1]
     "               "
     "     Qty  "
     "        "
     "      "
     "     Qty  "
     "     Issued BOL    "
     /* "  Qty Not " skip */
     skip

     v-label1[2]
     v-label2[2]
     "Cust PO #      "
     "   Ordered"
     "Inv Date"
     " Inv #"
     "   Shipped"
     "  Number        Qty"
     /* " Scheduled" skip */
     " Available" skip

     "------------------------"
     "---------------------"
     "---------------"
     "----------"
     "--------"
     "------"
     "----------"
     "-------------------"
     "----------" SKIP

   with frame r-top.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-stat       = SUBSTR(rd_ord-stat,1,1) 
 v-fdate      = begin_ord-date
 v-tdate      = end_ord-date
 v-ford       = begin_ord-no
 v-tord       = end_ord-no
 v-fcust      = begin_cust
 v-tcust      = end_cust
 v-fship      = begin_ship
 v-tship      = end_ship
 v-fitem      = begin_i-no
 v-titem      = end_i-no
 v-floc       = begin_whse
 v-tloc       = end_whse
 v-cust-sort  = rd_sort EQ "Customer#"

 str-tit3 = (if v-stat eq "O" then "Open"   else
                 if v-stat eq "C" then "Closed" else "All") + " Orders" + "  " +
                "Order #:" + string(v-ford) + " - " + string(v-tord)    + "  " +
                "Order Date:" + string(v-fdate) + " - " +
                                string(v-tdate)                         + "  " +
                "Cust #:" + v-fcust + " - " + v-tcust
     x = (132 - length(str-tit3)) / 2
     str-tit3 = fill(" ",x) + str-tit3
     str-tit4 = "Ship To #:" + v-fship + " - " + v-tship                + "  " +
                "Item #:" + v-fitem + " - " + v-titem                   + "  " +
                "Whse:"   + v-floc  + " - " + v-tloc
     x = (132 - length(str-tit4)) / 2
     str-tit4 = fill(" ",x) + str-tit4.


    if v-cust-sort then
      assign
       v-label1[1] = "Customer                "
       v-label2[1] = "Ship To              "
       v-label1[2] = "Item Description        "
       v-label2[2] = "Item #               ".

    else
      assign
       v-label1[1] = "Item Description        "
       v-label2[1] = "Item #               "
       v-label1[2] = "Customer                "
       v-label2[2] = "Ship To              ".

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  ASSIGN
   excelheader = IF v-cust-sort THEN "Customer/Item Description,Ship To/Item #,"
                 ELSE "Item Description/Customer,Item #/Ship To,"
   excelheader = excelheader + "Cust PO #,Qty Ordered,"
               + "Inv Date,Inv #,Qty Shipped,Issued BOL Number,Issued BOL Qty,"
               + "Available".

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

SESSION:SET-WAIT-STATE("general").

DISPLAY WITH frame r-top.

FOR EACH tt-report:
  DELETE tt-report.
END.

    IF v-stat EQ "O" THEN
    FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
        EACH oe-ord
        {&where-phrase}
          AND oe-ord.opened EQ YES
        USE-INDEX opened NO-LOCK:

      RUN create-report. 
    END.

    ELSE
    IF v-stat EQ "C" THEN
    FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
        EACH oe-ord
        {&where-phrase}
          AND oe-ord.opened EQ NO
        USE-INDEX opened NO-LOCK:

      RUN create-report. 
    END.

    ELSE
    FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
      EACH oe-ord {&where-phrase} NO-LOCK:
      RUN create-report. 
    END.

    for each tt-report WHERE tt-report.term-id EQ "",

        first cust
        where cust.company eq cocode
          and cust.cust-no eq tt-report.key-08
        no-lock,

        first shipto
        where shipto.company eq cocode
          and shipto.cust-no eq tt-report.key-08
          and shipto.ship-id eq tt-report.key-09
        no-lock

        break by tt-report.key-01
              by tt-report.key-02
              by tt-report.key-03:

      if first-of(tt-report.key-01) then v-first[1] = tt-report.key-01.
      if first-of(tt-report.key-02) then v-first[2] = tt-report.key-02.
      if first-of(tt-report.key-03) then v-first[3] = tt-report.key-03.

      release oe-rell.

      find oe-rel where recid(oe-rel) eq tt-report.rec-id no-lock.

      find first oe-ordl of oe-rel no-lock.

      {custom/statusMsg.i " 'Processing FG Item/Cust#  '  + oe-ordl.i-no + '/' +  cust.cust-no "}

      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq oe-ordl.i-no
          no-lock no-error.

      find first oe-ord of oe-ordl no-lock.

      if v-cust-sort then do:
        if first-of(tt-report.key-01) or first-of(tt-report.key-02) then do:
          display cust.name               format "x(24)"
                                          when first-of(tt-report.key-01)
                  shipto.ship-name        format "x(21)"

              with frame detail no-labels no-box no-attr-space down STREAM-IO width 132.

          down with frame detail.
        end.

        if first-of(tt-report.key-03) then
          display itemfg.i-name when avail itemfg @ cust.name
                  oe-ordl.i-no                    @ shipto.ship-name

              with frame detail.
      end.

      else do:
        if first-of(tt-report.key-01) then do:
          display itemfg.i-name when avail itemfg @ cust.name
                  oe-ordl.i-no                    @ shipto.ship-name

              with frame detail.

          down with frame detail.
        end.

        if first-of(tt-report.key-02) or first-of(tt-report.key-03) then
          display cust.name               when first-of(tt-report.key-02)
                  shipto.ship-name

              with frame detail.
      end.

      {fg/rep/fg-unsum.i}

      assign
       v-ono[3] = v-ono[3] + oe-rel.qty
       v-bal    = oe-rel.qty.

      if avail ar-invl then
        assign
         v-shp[3] = v-shp[3] + ar-invl.ship-qty
         v-bal    = v-bal    - ar-invl.ship-qty.

      else
      if avail oe-rell        and
         avail oe-bolh        and
         (not oe-bolh.posted) then
        assign
         v-bol[3] = v-bol[3] + oe-rell.qty
         v-bal    = v-bal    - oe-rell.qty.

      if v-bal lt 0 then v-bal = 0.

      display oe-rel.po-no
              oe-rel.qty                format "->>>>>>>>9"
              ar-inv.inv-date           FORMAT "99/99/99"
                                        when avail ar-inv and avail ar-invl
              ar-inv.inv-no             when avail ar-inv and avail ar-invl
              ar-invl.ship-qty          format "->>>>>>>>9"
                                        when avail ar-invl
              oe-bolh.bol-no            when avail oe-bolh
                                         and (not oe-bolh.posted)
              oe-rell.qty               format "->>>>>>>>9"
                                        when avail oe-rell
                                         and avail oe-bolh
                                         and (not oe-bolh.posted)
              v-bal

          with frame detail.
      down with frame detail.

      IF tb_excel THEN DO:
        PUT STREAM excel UNFORMATTED
            '"' (IF v-cust-sort THEN cust.name
                 ELSE itemfg.i-name)                 '",'
            '"' (IF v-cust-sort THEN shipto.ship-name
                ELSE oe-ordl.i-no)                   '",'
            SKIP.

        PUT STREAM excel UNFORMATTED
            '"' (IF v-cust-sort THEN itemfg.i-name
                 ELSE cust.NAME)                           '",'
            '"' (IF v-cust-sort THEN oe-ordl.i-no 
                ELSE shipto.ship-name)                     '",'
            '"' oe-rel.po-no                               '",'
            '"' STRING(oe-rel.qty,"->>>>>>>>9")            '",'
            '"' (IF AVAIL ar-inv AND AVAIL ar-invl AND
                 ar-inv.inv-date <> ? THEN STRING(ar-inv.inv-date)
                 ELSE "")                                  '",'
            '"' (IF AVAIL ar-inv AND AVAIL ar-invl THEN
                 STRING(ar-inv.inv-no) ELSE "")            '",'
            '"' (IF avail ar-invl THEN STRING(ar-invl.ship-qty,"->>>>>>>>9")
                 ELSE "")                                  '",'
            '"' (IF avail oe-bolh and (not oe-bolh.posted) THEN
                 STRING(oe-bolh.bol-no) ELSE "")                   '",'
            '"' (IF avail oe-rell and avail oe-bolh AND
                 (not oe-bolh.posted) THEN STRING(oe-rell.qty,"->>>>>>>>9")
                 ELSE "")                                  '",'
            '"'  STRING(v-bal,"->>>>>>>>9")                '",'
            SKIP.
        END.

      if last-of(tt-report.key-03) then do:
        v-bal = v-ono[3] - v-shp[3] - v-bol[3].

        if v-bal lt 0 then v-bal = 0.

        clear frame detail no-pause.

        put skip(1).

        if tt-report.key-03 ne v-first[3] then do:

          display "         Item Totals:"     when v-cust-sort
                                              @ shipto.ship-name
                  "      Ship To Totals:"     when (not v-cust-sort)
                                              @ shipto.ship-name
                  v-ono[3]                    @ oe-rel.qty
                  v-shp[3]                    @ ar-invl.ship-qty
                  v-bol[3]                    @ oe-rell.qty
                  v-bal

              with frame detail.
          down with frame detail.

          put skip(1).
          if not last-of(tt-report.key-02) then put skip(1).
        end.

        assign
         v-ono[2] = v-ono[2] + v-ono[3]
         v-shp[2] = v-shp[2] + v-shp[3]
         v-bol[2] = v-bol[2] + v-bol[3]

         v-ono[3] = 0
         v-shp[3] = 0
         v-bol[3] = 0.
      end.

      if last-of(tt-report.key-02) then do:
        v-bal = v-ono[2] - v-shp[2] - v-bol[2].

        if v-bal lt 0 then v-bal = 0.

        clear frame detail no-pause.

        if tt-report.key-02 ne v-first[2] then do:

          display "      Ship To Totals:"     when v-cust-sort
                                              @ shipto.ship-name
                  "     Customer Totals:"     when (not v-cust-sort)
                                              @ shipto.ship-name
                  v-ono[2]                    @ oe-rel.qty
                  v-shp[2]                    @ ar-invl.ship-qty
                  v-bol[2]                    @ oe-rell.qty
                  v-bal

              with frame detail.
          down with frame detail.

          put skip(1).
          if not last-of(tt-report.key-01) then put skip(1).
        end.

        assign
         v-ono[1] = v-ono[1] + v-ono[2]
         v-shp[1] = v-shp[1] + v-shp[2]
         v-bol[1] = v-bol[1] + v-bol[2]

         v-ono[2] = 0
         v-shp[2] = 0
         v-bol[2] = 0.
      end.

      if last-of(tt-report.key-01) then do:
        v-bal = v-ono[1] - v-shp[1] - v-bol[1].

        if v-bal lt 0 then v-bal = 0.

        clear frame detail no-pause.

        if tt-report.key-01 ne v-first[1] then do:

          display "     Customer Totals:"     when v-cust-sort
                                              @ shipto.ship-name
                  "         Item Totals:"     when (not v-cust-sort)
                                              @ shipto.ship-name
                  v-ono[1]                    @ oe-rel.qty
                  v-shp[1]                    @ ar-invl.ship-qty
                  v-bol[1]                    @ oe-rell.qty
                  v-bal

              with frame detail.
          down with frame detail.

          put skip(2).
        end.

        assign
         v-ono[4] = v-ono[4] + v-ono[1]
         v-shp[4] = v-shp[4] + v-shp[1]
         v-bol[4] = v-bol[4] + v-bol[1]

         v-ono[1] = 0
         v-shp[1] = 0
         v-bol[1] = 0.
      end.

      if last(tt-report.key-01) then do:
        v-bal = v-ono[4] - v-shp[4] - v-bol[4].

        if v-bal lt 0 then v-bal = 0.

        clear frame detail no-pause.

        display "        Grand Totals:"  @ shipto.ship-name
                v-ono[4]                 @ oe-rel.qty
                v-shp[4]                 @ ar-invl.ship-qty
                v-bol[4]                 @ oe-rell.qty
                v-bal

            with frame detail.
        down with frame detail.
      end.

      v-first = "".
    end.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

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
        begin_cust:SENSITIVE = NOT iplChecked
        end_cust:SENSITIVE = NOT iplChecked
        begin_cust:VISIBLE = NOT iplChecked
        end_cust:VISIBLE = NOT iplChecked
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

