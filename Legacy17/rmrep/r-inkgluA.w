&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: rmrep\r-inkglu.w

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

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHARACTER NO-UNDO.

DEF VAR v-fitem      LIKE rm-rcpth.i-no.
DEF VAR v-titem      LIKE v-fitem                  INIT "zzzzzzzzzz".
DEF VAR v-fpcat      LIKE item.procat.
DEF VAR v-tpcat      LIKE v-fpcat                  INIT "zzzzz".
DEF VAR v-fdate      AS DATE FORMAT "99/99/9999"   INIT 01/01/0001.
DEF VAR v-tdate      LIKE v-fdate                  INIT today.
DEF VAR v-fjob       LIKE job.job-no.
DEF VAR v-tjob       LIKE v-fjob                   INIT "zzzzzz".
DEF VAR v-fjob2      LIKE job.job-no2 FORMAT "99".
DEF VAR v-tjob2      LIKE v-fjob2                  INIT 99.
DEF VAR v-mtype      AS CHAR FORMAT "x(47)".
DEF VAR v-export     AS LOG INIT NO FORMAT "Y/N".
DEF VAR v-exp-name   AS CHAR FORMAT "x(40)"        INIT "rmtrans3.csv".
DEF VAR v-fCat       LIKE itemfg.procat NO-UNDO.
DEF VAR v-tCat       LIKE itemfg.procat NO-UNDO.
DEF VAR v-job-no  AS CHAR FORMAT "x(9)".
DEF VAR v-rm-qty  AS DEC.
DEF VAR v-qty     AS DEC FORMAT "->,>>>,>>>,>>9.9<<<<" EXTENT 3.
DEF VAR v-m-code  LIKE mach.m-code FORMAT "x(6)".
DEF VAR v-board   LIKE item.i-no EXTENT 2.
DEF VAR v-brd-qty AS INT FORMAT "->,>>>,>>>,>>9" EXTENT 2.
DEF VAR v-first   AS LOG EXTENT 3.
DEF VAR v-board-msf  AS DECI NO-UNDO.
DEF VAR v-board-weight AS DECI NO-UNDO.
DEF VAR v-wax-coat-weight AS DECI NO-UNDO.
DEF VAR v-tot-board-weight AS DECI NO-UNDO.
DEF VAR v-tot-wax-coat-weight AS DECI NO-UNDO.
DEF VAR v-tot-job-hdr-qty AS DECI FORMAT "->,>>>,>>>,>>9" NO-UNDO.
DEF VAR v-grand-tot-board-weight AS DECI NO-UNDO.
DEF VAR v-grand-tot-wax-coat-weight AS DECI NO-UNDO.
DEF VAR v-grand-tot-job-hdr-qty AS DECI FORMAT "->,>>>,>>>,>>9" NO-UNDO.
DEF VAR v-tot-trans-qty AS DECI NO-UNDO.
DEF VAR v-grand-tot-trans-qty AS DECI NO-UNDO.
DEF VAR v-wid LIKE job-mat.wid NO-UNDO.
DEF VAR v-len LIKE job-mat.len NO-UNDO.
DEF VAR v-basis-w LIKE job-mat.basis-w NO-UNDO.

def stream s-temp.

DEF TEMP-TABLE tt-inks-glues
   FIELD trans-date LIKE rm-rcpth.trans-date 
   FIELD job-no     AS CHAR FORMAT "x(9)"
   FIELD qty        AS DEC FORMAT "->,>>>,>>>,>>9.9<<<<"
   FIELD m-code     LIKE mach.m-code FORMAT "x(6)"
   FIELD board      LIKE item.i-no 
   FIELD brd-qty    AS INT FORMAT "->,>>>,>>>,>>9"
   FIELD i-no       LIKE rm-rcpth.i-no
   FIELD procat     LIKE itemfg.procat.

DEF TEMP-TABLE tt-wax-coats
   FIELD trans-date        LIKE rm-rcpth.trans-date 
   FIELD job-no            AS CHAR FORMAT "x(9)"
   FIELD qty               AS DEC FORMAT "->,>>>,>>>,>>9"
   FIELD board             LIKE item.i-no 
   FIELD brd-qty           AS INT FORMAT "->,>>>,>>>,>>9"
   FIELD i-no              LIKE rm-rcpth.i-no
   FIELD board-msf         AS DECI
   FIELD board-weight      AS DECI
   FIELD shrink            LIKE ITEM.shrink
   FIELD wax-coat-weight   AS DECI
   FIELD procat            LIKE itemfg.procat.

FORM tt-wax-coats.trans-date LABEL "Issue Date"
     tt-wax-coats.job-no     LABEL "   Job #"
     tt-wax-coats.board      LABEL "Board"
     tt-wax-coats.brd-qty    LABEL "Sheets Issued"
     tt-wax-coats.i-no       LABEL "Wax/Coat"
     tt-wax-coats.board-msf  LABEL "Board MSF"
     tt-wax-coats.board-weight LABEL "Board Weight"
     tt-wax-coats.shrink       LABEL "Pickup%"
     tt-wax-coats.wax-coat-weight LABEL "Wax/Coat Weight"
     tt-wax-coats.procat LABEL "Product Category"
     tt-wax-coats.qty    LABEL "Produced"
     SKIP
    WITH FRAME item-x NO-BOX DOWN STREAM-IO WIDTH 180.

FORM    
     "------------"        AT 70
     "---------------"     AT 92
     "--------------"      AT 125
     SKIP
     "SUB TOTAL"           AT 37
     v-tot-board-weight    AT 72
     v-tot-wax-coat-weight AT 97
     v-tot-job-hdr-qty     AT 125
     SKIP(2)
    WITH FRAME item-b NO-BOX DOWN NO-LABEL NO-ATTR STREAM-IO WIDTH 180.

FORM    
     "------------"        AT 70
     "---------------"     AT 92
     "--------------"          AT 125
     SKIP
     "GRAND TOTAL"         AT 37
     v-grand-tot-board-weight    AT 72
     v-grand-tot-wax-coat-weight AT 97  
     v-grand-tot-job-hdr-qty     AT 125
    WITH FRAME item-c NO-BOX DOWN NO-LABEL NO-ATTR STREAM-IO WIDTH 180.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_rm-no end_rm-no ~
begin_procat end_procat begin_cat end_cat begin_date end_date begin_job-no ~
begin_job-no2 end_job-no end_job-no2 select-mat TG_sort-cat RS_material ~
rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_rm-no end_rm-no begin_procat ~
end_procat begin_cat end_cat begin_date end_date begin_job-no begin_job-no2 ~
end_job-no end_job-no2 select-mat TG_sort-cat RS_material rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel ~
fi_file 

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

DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning FG Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Beginning Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning RM Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending FG Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

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

DEFINE VARIABLE end_procat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending RM Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(256)" INITIAL "c:~\tmp~\r-inkglu.csv" 
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

DEFINE VARIABLE mat-types AS CHARACTER FORMAT "X(256)":U 
     LABEL "Material Types" 
     VIEW-AS FILL-IN 
     SIZE 1 BY 1 NO-UNDO.

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

DEFINE VARIABLE RS_material AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Ink/Glue", 1,
"Wax/Coat", 2
     SIZE 16 BY 1.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 11.91.

DEFINE VARIABLE select-mat AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 44 BY 3.57 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG_sort-cat AS LOGICAL INITIAL no 
     LABEL "Sort by FG Category" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_rm-no AT ROW 2.19 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_rm-no AT ROW 2.19 COL 67 COLON-ALIGNED HELP
          "Enter Ending Item number"
     begin_procat AT ROW 3.14 COL 26 COLON-ALIGNED HELP
          "Enter Begining Category"
     end_procat AT ROW 3.14 COL 67 COLON-ALIGNED HELP
          "Enter Ending Category"
     begin_cat AT ROW 4.1 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Category" WIDGET-ID 10
     end_cat AT ROW 4.1 COL 67 COLON-ALIGNED HELP
          "Enter Ending Category" WIDGET-ID 12
     begin_date AT ROW 5.14 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 5.14 COL 67 COLON-ALIGNED HELP
          "Enter ending Date"
     begin_job-no AT ROW 6.1 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 6.1 COL 38 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     end_job-no AT ROW 6.1 COL 67 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     end_job-no2 AT ROW 6.1 COL 79 COLON-ALIGNED HELP
          "Enter Ending Job Number"
     select-mat AT ROW 8.48 COL 20.6 HELP
          "Enter description of this Material Type." NO-LABEL
     TG_sort-cat AT ROW 8.48 COL 65.6 WIDGET-ID 6
     mat-types AT ROW 9.19 COL 16.6 COLON-ALIGNED
     RS_material AT ROW 9.67 COL 66 NO-LABEL WIDGET-ID 14
     rd-dest AT ROW 13.95 COL 4 NO-LABEL
     lv-ornt AT ROW 14.19 COL 30 NO-LABEL
     lines-per-page AT ROW 14.67 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 16.33 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 17.29 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 18.48 COL 30
     tb_excel AT ROW 19.43 COL 50 RIGHT-ALIGNED
     tb_runExcel AT ROW 19.43 COL 52.2
     fi_file AT ROW 20.43 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 22.05 COL 25
     btn-cancel AT ROW 22.05 COL 58
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Select/Deselect Material Types" VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 7.76 COL 23.6
          FONT 6
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 13 COL 2
     RECT-6 AT ROW 12.76 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 24.57.


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
         TITLE              = "Transaction History"
         HEIGHT             = 23.14
         WIDTH              = 96.4
         MAX-HEIGHT         = 45.05
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 45.05
         VIRTUAL-WIDTH      = 256
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
       begin_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       begin_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       end_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mat-types IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       mat-types:HIDDEN IN FRAME FRAME-A           = TRUE
       mat-types:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

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
ON END-ERROR OF C-Win /* Transaction History */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Transaction History */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cat C-Win
ON LEAVE OF begin_cat IN FRAME FRAME-A /* Beginning FG Category */
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


&Scoped-define SELF-NAME begin_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_procat C-Win
ON LEAVE OF begin_procat IN FRAME FRAME-A /* Beginning RM Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no C-Win
ON LEAVE OF begin_rm-no IN FRAME FRAME-A /* Beginning Item# */
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
  DEF VAR v-valid AS LOG NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  DO i = 1 to select-mat:NUM-ITEMS:
      IF select-mat:IS-SELECTED(i) THEN
      DO:
         v-valid = YES.
         LEAVE.
      END.
  END.

  IF v-valid THEN
  DO:
     SESSION:SET-WAIT-STATE("general").
     run run-report. 
     STATUS DEFAULT "Processing Complete". 
     SESSION:SET-WAIT-STATE("").

     case rd-dest:
          when 1 then run output-to-printer.
          when 2 then run output-to-screen.
          when 3 then run output-to-file.
          when 4 then do:
              /*run output-to-fax.*/
              {custom/asifax.i &type= ''
                               &begin_cust= "begin_procat"
                               &END_cust= "begin_procat" 
                               &fax-subject=c-win:title
                               &fax-body=c-win:title
                               &fax-file=list-name }
          END. 
          when 5 then do:
              IF is-xprint-form THEN DO:
                 {custom/asimail.i &TYPE = ''
                                &begin_cust= "begin_procat"
                                &END_cust= "begin_procat"
                                &mail-subject=c-win:title
                                &mail-body=c-win:title
                                &mail-file=list-name }
              END.
              ELSE DO:
                  {custom/asimailr.i &TYPE = ''
                                     &begin_cust="begin_procat"
                                     &END_cust="begin_procat"
                                     &mail-subject=c-win:title
                                     &mail-body=c-win:title
                                     &mail-file=list-name }
              END.
          END.
          WHEN 6 THEN RUN OUTPUT-to-port.
     END CASE.
  END.
  ELSE
     MESSAGE "No Material Type Selected."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cat C-Win
ON LEAVE OF end_cat IN FRAME FRAME-A /* Ending FG Category */
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


&Scoped-define SELF-NAME end_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_procat C-Win
ON LEAVE OF end_procat IN FRAME FRAME-A /* Ending RM Category */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-no C-Win
ON LEAVE OF end_rm-no IN FRAME FRAME-A /* Ending Item# */
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


&Scoped-define SELF-NAME mat-types
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mat-types C-Win
ON LEAVE OF mat-types IN FRAME FRAME-A /* Material Types */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME RS_material
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS_material C-Win
ON VALUE-CHANGED OF RS_material IN FRAME FRAME-A
DO:
   ASSIGN RS_material.
   RUN fill-select-box.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME select-mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL select-mat C-Win
ON VALUE-CHANGED OF select-mat IN FRAME FRAME-A
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
def var v-mat-list as char no-undo.
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

  end_date = today.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO begin_rm-no.
  END.

  RUN fill-select-box.

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
  DISPLAY begin_rm-no end_rm-no begin_procat end_procat begin_cat end_cat 
          begin_date end_date begin_job-no begin_job-no2 end_job-no end_job-no2 
          select-mat TG_sort-cat RS_material rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_rm-no end_rm-no begin_procat end_procat begin_cat 
         end_cat begin_date end_date begin_job-no begin_job-no2 end_job-no 
         end_job-no2 select-mat TG_sort-cat RS_material rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file 
         btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fill-select-box C-Win 
PROCEDURE fill-select-box :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   v-mat-list = "".
   ASSIGN RS_material.
   IF RS_material = 1 THEN
      FOR EACH mat WHERE INDEX("GI",mat.mat) GT 0 NO-LOCK:
        {custom/statusMsg.i "'Processing... '"} 
         v-mat-list = v-mat-list + STRING(mat.mat,"x(5)") + " " + mat.dscr + ",".
      END.
   ELSE
      FOR EACH mat NO-LOCK WHERE INDEX("VW",mat.mat) GT 0 BY mat.mat DESC:
       {custom/statusMsg.i "'Processing... '"} 
         v-mat-list = v-mat-list + STRING(mat.mat,"x(5)") + " " + mat.dscr + ",".
      END.

   IF SUBSTR(v-mat-list,LENGTH(TRIM(v-mat-list)),1) EQ "," THEN
      SUBSTR(v-mat-list,LENGTH(TRIM(v-mat-list)),1) = "".

   select-mat:LIST-ITEMS = v-mat-list.
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
 /*    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-coat-wax C-Win 
PROCEDURE print-coat-wax :
{sys/form/r-topw.f}

DEF BUFFER b-item FOR ITEM.

FOR EACH tt-wax-coats:
   DELETE tt-wax-coats.
END.



DEF VAR v-hdr       AS   CHAR INIT "Issue Date,Job#,Board,Sheets Issued,Wax/Coat,Board MSF,Board Weight,Pickup%,Wax/Coat Weight,Product Category,Produced" NO-UNDO.

ASSIGN
   str-tit2 = c-win:TITLE
   {sys/inc/ctrtext.i str-tit2 112}

   v-fitem    = begin_rm-no
   v-titem    = end_rm-no
   v-fpcat    = begin_procat
   v-tpcat    = end_procat
   v-fdate    = begin_date
   v-tdate    = end_date
   v-fjob     = FILL(" ",6 - LENGTH(TRIM(begin_job-no))) +
              TRIM(begin_job-no) + STRING(INT(begin_job-no2),"99")
   v-tjob     = FILL(" ",6 - LENGTH(TRIM(end_job-no)))   +
              TRIM(end_job-no)   + STRING(INT(end_job-no2),"99")
   v-export   = tb_excel
   v-exp-name = fi_file
   v-fCat     = begin_cat
   v-tCat     = END_cat
   v-mtype    = "".

DO WITH FRAME {&FRAME-NAME}:          
   DO i = 1 to select-mat:NUM-ITEMS:
      IF select-mat:IS-SELECTED(i) THEN
         v-mtype = v-mtype + TRIM(SUBSTR(select-mat:ENTRY(i),1,5)) + ",".
   END.

   IF SUBSTR(v-mtype,LENGTH(TRIM(v-mtype)),1) EQ "," THEN
      SUBSTR(v-mtype,LENGTH(TRIM(v-mtype)),1) = "".

   mat-types = v-mtype.
   DO i = 1 TO LENGTH(mat-types):
      IF SUBSTR(mat-types,i,1) EQ "," THEN 
         SUBSTR(mat-types,i,1) = " ".
   END.
   DISPLAY mat-types.
   mat-types:HIDDEN = YES.
END.

v-qty = 0.
{sys/inc/print1.i}
{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN 
   RUN show-param.

SESSION:SET-WAIT-STATE ("general").

DISPLAY "" WITH FRAME r-top.

IF v-export THEN DO:
   OUTPUT STREAM s-temp TO VALUE(v-exp-name).
   PUT STREAM s-temp UNFORMATTED v-hdr SKIP.
END.

FOR EACH rm-rcpth WHERE rm-rcpth.company    EQ cocode
                    AND rm-rcpth.i-no       GE v-fitem
                    AND rm-rcpth.i-no       LE v-titem
                    AND rm-rcpth.trans-date GE v-fdate
                    AND rm-rcpth.trans-date LE v-tdate
                    AND rm-rcpth.rita-code  EQ "I"
                    USE-INDEX i-no no-lock,
   EACH rm-rdtlh WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
                   AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
                   AND rm-rdtlh.job-no    GE substr(v-fjob,1,6)
                   AND rm-rdtlh.job-no    LE substr(v-tjob,1,6)
                   AND FILL(" ",6 - LENGTH(TRIM(rm-rdtlh.job-no))) +
                       TRIM(rm-rdtlh.job-no) + STRING(rm-rdtlh.job-no2,"99") GE v-fjob
                   AND FILL(" ",6 - LENGTH(TRIM(rm-rdtlh.job-no))) +
                       TRIM(rm-rdtlh.job-no) + STRING(rm-rdtlh.job-no2,"99") LE v-tjob NO-LOCK,
      FIRST item WHERE item.company EQ cocode
                   AND item.i-no    EQ rm-rcpth.i-no
                   AND item.procat  GE v-fpcat
                   AND item.procat  LE v-tpcat
                   AND INDEX(v-mtype,item.mat-type) GT 0
                   AND INDEX("VW",item.mat-type) GT 0 NO-LOCK
              BREAK BY rm-rcpth.trans-date
                    BY rm-rcpth.job-no
                    BY rm-rcpth.job-no2
                    BY item.mat-type
                    BY item.i-no
                    BY rm-rdtlh.s-num
                    BY rm-rdtlh.b-num transaction:

    {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

      IF FIRST-of(rm-rcpth.trans-date) THEN v-first[1] = yes.

      v-job-no = FILL(" ",6 - LENGTH(TRIM(rm-rdtlh.job-no))) +
                 TRIM(rm-rdtlh.job-no) + "-" + STRING(rm-rdtlh.job-no2,"99").

      IF v-job-no begins "-" THEN v-job-no = "".

      v-rm-qty = rm-rdtlh.qty.

      IF rm-rcpth.pur-uom ne "LB" THEN
         run sys/ref/convquom.p(rm-rcpth.pur-uom, "LB", 0, 0, 0, 0, v-rm-qty, output v-rm-qty).

      ASSIGN
         v-qty[1] = v-qty[1] + v-rm-qty
         v-qty[3] = v-qty[3] + v-rm-qty.

      IF LAST-OF(rm-rdtlh.b-num) THEN DO: 
         ASSIGN
            v-board   = ""
            v-brd-qty = 0
            v-m-code  = ""
            v-len = 0
            v-wid = 0
            v-basis-w = 0.

         FOR EACH job WHERE job.company EQ rm-rdtlh.company
                        AND job.job-no  EQ rm-rdtlh.job-no
                        AND job.job-no2 EQ rm-rdtlh.job-no2 NO-LOCK,
             EACH job-hdr NO-LOCK WHERE job-hdr.company = job.company
                                       AND job-hdr.job-no  = job.job-no
                                       AND job-hdr.job-no2 = job.job-no2
                                       AND (job-hdr.frm = rm-rdtlh.s-num OR rm-rdtlh.s-num EQ 0)
                                       and (job-hdr.blank-no eq rm-rdtlh.b-num OR rm-rdtlh.b-num EQ 0),
               FIRST itemfg WHERE itemfg.company = job-hdr.company
                              AND itemfg.i-no    = job-hdr.i-no
                              AND itemfg.procat  GE v-fCat
                              AND itemfg.procat  LE v-tCat NO-LOCK:

               FOR EACH job-mat WHERE job-mat.company EQ job.company
                                  AND job-mat.job     EQ job.job
                                  AND job-mat.job-no  EQ job.job-no
                                  AND job-mat.job-no2 EQ job.job-no2
                                  AND (job-mat.frm    EQ rm-rdtlh.s-num OR rm-rdtlh.s-num EQ 0)
                                  AND (job-mat.blank-no EQ rm-rdtlh.b-num OR rm-rdtlh.b-num EQ 0)
                            USE-INDEX seq-idx NO-LOCK,
                  FIRST b-item WHERE b-item.company  EQ job-mat.company
                                 AND b-item.i-no     EQ job-mat.i-no
                                 AND INDEX("1234BPR",b-item.mat-type) GT 0 NO-LOCK:

                   {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

                  IF v-board[1] EQ "" THEN 
                     v-board[1] = b-item.i-no.
                  ASSIGN
                     v-len = job-mat.len
                     v-wid = job-mat.wid
                     v-basis-w = job-mat.basis-w.


                  FOR EACH mat-act WHERE mat-act.company EQ cocode
                                     AND mat-act.job     EQ job-mat.job
                                     AND mat-act.job-no  EQ job-mat.job-no
                                     AND mat-act.job-no2 EQ job-mat.job-no2
                                     AND mat-act.s-num   EQ job-mat.frm
                                     AND mat-act.b-num   EQ job-mat.blank-no
                                     AND mat-act.i-no    EQ job-mat.i-no
                               USE-INDEX job NO-LOCK:

                     IF v-board[2] EQ "" THEN 
                        v-board[2] = b-item.i-no.

                     RUN sys/ref/convquom.p(job-mat.qty-uom, "EA", job-mat.basis-w,
                                            job-mat.len, job-mat.wid, b-item.s-dep,
                                            mat-act.qty, OUTPUT v-brd-qty[1]).

                     v-brd-qty[2] = v-brd-qty[2] + v-brd-qty[1].
                  END. 
               END. 

               FOR EACH mch-act WHERE mch-act.company EQ cocode
                                  AND mch-act.job-no  EQ job.job-no
                                  AND mch-act.job-no2 EQ job.job-no2
                                  AND (mch-act.frm    EQ rm-rdtlh.s-num OR rm-rdtlh.s-num EQ 0)
                                  AND (mat-act.b-num EQ rm-rdtlh.b-num OR rm-rdtlh.b-num EQ 0)  NO-LOCK,

                  FIRST mach WHERE mach.company EQ cocode
                               AND mach.loc     EQ locode 
                               AND mach.m-code  EQ mch-act.m-code
                               AND ((item.mat-type EQ "G" AND (mach.dept[1] EQ "GL" OR
                                                               mach.dept[2] EQ "GL" OR
                                                               mach.dept[3] EQ "GL" OR
                                                               mach.dept[4] EQ "GL")) OR
                                    (item.mat-type EQ "I" AND (mach.dept[1] EQ "PR" OR
                                                               mach.dept[2] EQ "PR" OR
                                                               mach.dept[3] EQ "PR" OR
                                                               mach.dept[4] EQ "PR"))) NO-LOCK
                          BREAK BY mch-act.frm      desc
                                BY mch-act.blank-no desc:

                   {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

                  IF (item.mat-type EQ "G"               AND
                      mch-act.frm      EQ rm-rdtlh.s-num AND
                      mch-act.blank-no EQ rm-rdtlh.b-num AND
                      mch-act.dept EQ "GL")                   OR
                     (item.mat-type EQ "I" AND
                      mch-act.dept EQ "PR")                   OR
                     last(mch-act.frm)                        THEN DO:

                     v-m-code = mach.m-code.
                     leave.
                  END.
               END. 

               IF v-m-code EQ "" THEN
                  FOR EACH job-mch WHERE job-mch.company EQ cocode
                                     AND job-mch.job     EQ job.job
                                     AND job-mch.job-no  EQ job.job-no
                                     AND job-mch.job-no2 EQ job.job-no2
                                     AND (job-mch.frm    EQ rm-rdtlh.s-num OR rm-rdtlh.s-num EQ 0)
                                     AND (job-mch.blank-no EQ rm-rdtlh.b-num OR rm-rdtlh.b-num EQ 0) NO-LOCK,
                     FIRST mach WHERE mach.company EQ cocode
                                  AND mach.loc     EQ locode 
                                  AND mach.m-code  EQ job-mch.m-code
                                  AND ((item.mat-type EQ "G" AND (mach.dept[1] EQ "GL" OR
                                                                  mach.dept[2] EQ "GL" OR
                                                                  mach.dept[3] EQ "GL" OR
                                                                  mach.dept[4] EQ "GL")) OR
                                       (item.mat-type EQ "I" AND (mach.dept[1] EQ "PR" OR
                                                                  mach.dept[2] EQ "PR" OR
                                                                  mach.dept[3] EQ "PR" OR
                                                                  mach.dept[4] EQ "PR"))) NO-LOCK

                             BREAK BY job-mch.frm      DESC
                                   BY job-mch.blank-no DESC:

                   {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

                     IF (item.mat-type    EQ "G"               AND
                         job-mch.frm      EQ rm-rdtlh.s-num    AND
                         job-mch.blank-no EQ rm-rdtlh.b-num    AND
                         job-mch.dept EQ "GL")                  OR
                        (item.mat-type EQ "I"                  AND
                         job-mch.dept EQ "PR")                  OR
                         LAST(job-mch.frm)                          THEN DO:

                        v-m-code = mach.m-code.
                        LEAVE.
                     END.
                  END. 

               IF v-board[2] EQ "" THEN 
                  v-board[2] = v-board[1].

               ASSIGN
                  v-board-msf = 0
                  v-board-weight = 0
                  v-wax-coat-weight = 0
                  v-board-msf = (v-brd-qty[2] * ((v-len * v-wid) / 144)) / 1000
                  v-board-weight = (v-board-msf * v-basis-w)
                  v-wax-coat-weight = v-board-weight * item.shrink.


               CREATE tt-wax-coats.
               ASSIGN
                  tt-wax-coats.trans-date        = rm-rcpth.trans-date
                  tt-wax-coats.job-no            = v-job-no
                  tt-wax-coats.qty               = job-hdr.qty
                  tt-wax-coats.board             = v-board[2] 
                  tt-wax-coats.brd-qty           = v-brd-qty[2]
                  tt-wax-coats.i-no              = rm-rcpth.i-no
                  tt-wax-coats.board-msf         = v-board-msf
                  tt-wax-coats.board-weight      = v-board-weight
                  tt-wax-coats.shrink            = ITEM.shrink 
                  tt-wax-coats.wax-coat-weight   = v-wax-coat-weight
                  tt-wax-coats.procat            = itemfg.procat.
         END.
      END.

END.
ASSIGN
   v-tot-board-weight =  0
   v-tot-wax-coat-weight = 0
   v-tot-job-hdr-qty     = 0
   v-grand-tot-board-weight = 0
   v-grand-tot-wax-coat-weight = 0
   v-grand-tot-job-hdr-qty = 0.

IF TG_sort-cat = NO THEN DO:
   FOR EACH tt-wax-coats BREAK BY tt-wax-coats.trans-date:

       {custom/statusMsg.i "'Processing Item # ' + string(tt-wax-coats.i-no)"} 

      DISPLAY 
         tt-wax-coats.trans-date  WHEN FIRST-OF(tt-wax-coats.trans-date)
         tt-wax-coats.job-no     
         tt-wax-coats.board      
         tt-wax-coats.brd-qty   
         tt-wax-coats.i-no       
         tt-wax-coats.board-msf   
         tt-wax-coats.board-weight 
         tt-wax-coats.shrink      
         tt-wax-coats.wax-coat-weight 
         tt-wax-coats.procat    
         tt-wax-coats.qty          
         WITH FRAME item-x.
      DOWN WITH FRAME item-x.

      ASSIGN 
         v-tot-board-weight =  v-tot-board-weight + tt-wax-coats.board-weight
         v-tot-wax-coat-weight = v-tot-wax-coat-weight + tt-wax-coats.wax-coat-weight
         v-tot-job-hdr-qty     = v-tot-job-hdr-qty + tt-wax-coats.qty
         v-grand-tot-board-weight = v-grand-tot-board-weight + tt-wax-coats.board-weight
         v-grand-tot-wax-coat-weight = v-grand-tot-wax-coat-weight + tt-wax-coats.wax-coat-weight
         v-grand-tot-job-hdr-qty = v-grand-tot-job-hdr-qty + tt-wax-coats.qty.

      IF LAST-OF(tt-wax-coats.trans-date) THEN DO:
         DISPLAY      
            v-tot-board-weight 
            v-tot-wax-coat-weight
            v-tot-job-hdr-qty
            SKIP
            WITH FRAME item-b NO-LABELS.
            ASSIGN 
               v-tot-board-weight =  0
               v-tot-wax-coat-weight = 0
               v-tot-job-hdr-qty     = 0.
      END.

      IF v-export THEN
         PUT STREAM s-temp UNFORMATTED
            TRIM(STRING(tt-wax-coats.trans-date,tt-wax-coats.trans-date:FORMAT))
                                                                  + "," +
            TRIM(tt-wax-coats.job-no)                            + "," +
            TRIM(tt-wax-coats.board)                             + "," +
            TRIM(STRING(tt-wax-coats.brd-qty,"->>>>>>>>>9"))     + "," +
            TRIM(tt-wax-coats.i-no)                              + "," +
            TRIM(STRING(tt-wax-coats.board-msf))                 + "," +
            TRIM(STRING(tt-wax-coats.board-weight))              + "," +
            TRIM(STRING(tt-wax-coats.shrink))                    + "," +
            TRIM(STRING(tt-wax-coats.wax-coat-weight))           + "," +
            TRIM(tt-wax-coats.procat)                            + "," +
            TRIM(STRING(tt-wax-coats.qty))                         
            SKIP.
   END.

   DISPLAY
      v-grand-tot-board-weight 
      v-grand-tot-wax-coat-weight
      v-grand-tot-job-hdr-qty
      SKIP
      WITH FRAME item-c NO-LABELS.
END.
ELSE DO:
   FOR EACH tt-wax-coats BREAK BY tt-wax-coats.procat
                               BY tt-wax-coats.trans-date:

       {custom/statusMsg.i "'Processing Item # ' + string(tt-wax-coats.i-no)"} 

      DISPLAY 
         tt-wax-coats.trans-date  WHEN FIRST-OF(tt-wax-coats.trans-date)
         tt-wax-coats.job-no     
         tt-wax-coats.board       
         tt-wax-coats.brd-qty    
         tt-wax-coats.i-no        
         tt-wax-coats.board-msf   
         tt-wax-coats.board-weight 
         tt-wax-coats.shrink      
         tt-wax-coats.wax-coat-weight  
         tt-wax-coats.procat       
         tt-wax-coats.qty            
         WITH FRAME item-x.
      DOWN WITH FRAME item-x.

      ASSIGN 
         v-tot-board-weight =  v-tot-board-weight + tt-wax-coats.board-weight
         v-tot-wax-coat-weight = v-tot-wax-coat-weight + tt-wax-coats.wax-coat-weight
         v-tot-job-hdr-qty     = v-tot-job-hdr-qty + tt-wax-coats.qty
         v-grand-tot-board-weight = v-grand-tot-board-weight + tt-wax-coats.board-weight
         v-grand-tot-wax-coat-weight = v-grand-tot-wax-coat-weight + tt-wax-coats.wax-coat-weight
         v-grand-tot-job-hdr-qty = v-grand-tot-job-hdr-qty + tt-wax-coats.qty.

      IF LAST-OF(tt-wax-coats.procat) THEN DO:
         DISPLAY      
            v-tot-board-weight 
            v-tot-wax-coat-weight
            v-tot-job-hdr-qty
            SKIP
            WITH FRAME item-b NO-LABELS.
         ASSIGN 
            v-tot-board-weight =  0
            v-tot-wax-coat-weight = 0
            v-tot-job-hdr-qty     = 0.
      END.
      IF v-export THEN
         PUT STREAM s-temp UNFORMATTED
            TRIM(STRING(tt-wax-coats.trans-date,tt-wax-coats.trans-date:FORMAT))
                                                                  + "," +
            TRIM(tt-wax-coats.job-no)                            + "," +
            TRIM(tt-wax-coats.board)                             + "," +
            TRIM(STRING(tt-wax-coats.brd-qty,"->>>>>>>>>9"))     + "," +
            TRIM(tt-wax-coats.i-no)                              + "," +
            TRIM(STRING(tt-wax-coats.board-msf))                 + "," +
            TRIM(STRING(tt-wax-coats.board-weight))              + "," +
            TRIM(STRING(tt-wax-coats.shrink))                    + "," +
            TRIM(STRING(tt-wax-coats.wax-coat-weight))           + "," +
            TRIM(tt-wax-coats.procat)                            + "," +
            TRIM(STRING(tt-wax-coats.qty))    
            SKIP.
   END.

   DISPLAY
      v-grand-tot-board-weight 
      v-grand-tot-wax-coat-weight
      v-grand-tot-job-hdr-qty
      SKIP
      WITH FRAME item-c NO-LABELS.
END.

IF v-export THEN DO:
   OUTPUT STREAM s-temp CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-inks-glues C-Win 
PROCEDURE print-inks-glues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{sys/form/r-topw.f}

DEF BUFFER b-item FOR ITEM.

    {custom/statusMsg.i "'Processing... '"} 

FORM tt-inks-glues.trans-date     LABEL "Issue Date"
     v-job-no               LABEL "   Job #"
     v-board[2]             LABEL "Board"
     v-brd-qty[2]           LABEL "Sheets Issued"
     rm-rcpth.i-no          LABEL "Ink/Glue"
     v-qty[1]               LABEL "Qty Issued/Lbs"
     v-m-code               LABEL "Machine"
     tt-inks-glues.procat   LABEL "FG Category"

     SKIP
    WITH FRAME itemx NO-BOX DOWN STREAM-IO WIDTH 132.

FORM    
     "----------------"    AT 59
     SKIP
     "SUB TOTAL"           AT 37
     v-tot-trans-qty       FORMAT "->,>>>,>>>,>>9.9<<<<" AT 59           
     SKIP(2)
    WITH FRAME itemb NO-BOX DOWN NO-LABEL NO-ATTR STREAM-IO WIDTH 132.

FORM    
     "----------------"    AT 59
     SKIP
     "GRAND TOTAL"         AT 37
     v-grand-tot-trans-qty       FORMAT "->,>>>,>>>,>>9.9<<<<" AT 59           
     SKIP(2)
    WITH FRAME itemc NO-BOX DOWN NO-LABEL NO-ATTR STREAM-IO WIDTH 132.

DEF VAR v-hdr       AS   CHAR INIT "Issue Date,Job#,Board,Sheets Issued,Ink/Glue,Qty Issued/Lbs,Machine,FG Category," NO-UNDO.

ASSIGN
   str-tit2 = c-win:TITLE
   {sys/inc/ctrtext.i str-tit2 112}

   v-fitem    = begin_rm-no
   v-titem    = end_rm-no
   v-fpcat    = begin_procat
   v-tpcat    = end_procat
   v-fdate    = begin_date
   v-tdate    = end_date
   v-fjob     = FILL(" ",6 - LENGTH(TRIM(begin_job-no))) +
              TRIM(begin_job-no) + STRING(INT(begin_job-no2),"99")
   v-tjob     = FILL(" ",6 - LENGTH(TRIM(end_job-no)))   +
              TRIM(end_job-no)   + STRING(INT(end_job-no2),"99")
   v-export   = tb_excel
   v-exp-name = fi_file
   v-fCat     = begin_cat
   v-tCat     = END_cat
   v-mtype    = "".

FOR EACH tt-inks-glues:
   DELETE tt-inks-glues.
END.
DO WITH FRAME {&FRAME-NAME}:          
   DO i = 1 to select-mat:NUM-ITEMS:
      IF select-mat:IS-SELECTED(i) THEN
         v-mtype = v-mtype + TRIM(SUBSTR(select-mat:ENTRY(i),1,5)) + ",".
   END.

   IF SUBSTR(v-mtype,LENGTH(TRIM(v-mtype)),1) EQ "," THEN
      SUBSTR(v-mtype,LENGTH(TRIM(v-mtype)),1) = "".

   mat-types = v-mtype.

   DO i = 1 TO LENGTH(mat-types):
      IF SUBSTR(mat-types,i,1) EQ "," THEN 
         SUBSTR(mat-types,i,1) = " ".
   END.

   DISPLAY mat-types.

   mat-types:HIDDEN = YES.
END.

v-qty = 0.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN 
   RUN show-param.

SESSION:SET-WAIT-STATE ("general").

DISPLAY "" WITH FRAME r-top.

IF v-export THEN DO:
   OUTPUT STREAM s-temp TO VALUE(v-exp-name).
   PUT STREAM s-temp UNFORMATTED v-hdr SKIP.
END.


FOR EACH rm-rcpth WHERE rm-rcpth.company    EQ cocode
                    AND rm-rcpth.i-no       GE v-fitem
                    AND rm-rcpth.i-no       LE v-titem
                    AND rm-rcpth.trans-date GE v-fdate
                    AND rm-rcpth.trans-date LE v-tdate
                    AND rm-rcpth.rita-code  EQ "I"
                    USE-INDEX i-no no-lock,
   EACH rm-rdtlh WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
                   AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
                   AND rm-rdtlh.job-no    GE substr(v-fjob,1,6)
                   AND rm-rdtlh.job-no    LE substr(v-tjob,1,6)
                   AND FILL(" ",6 - LENGTH(TRIM(rm-rdtlh.job-no))) +
                       TRIM(rm-rdtlh.job-no) + STRING(rm-rdtlh.job-no2,"99") GE v-fjob
                   AND FILL(" ",6 - LENGTH(TRIM(rm-rdtlh.job-no))) +
                       TRIM(rm-rdtlh.job-no) + STRING(rm-rdtlh.job-no2,"99") LE v-tjob NO-LOCK,
      FIRST item WHERE item.company EQ cocode
                   AND item.i-no    EQ rm-rcpth.i-no
                   AND item.procat  GE v-fpcat
                   AND item.procat  LE v-tpcat
                   AND INDEX(v-mtype,item.mat-type) GT 0
                   AND INDEX("GI",item.mat-type) GT 0 NO-LOCK
              BREAK BY rm-rcpth.trans-date
                    BY rm-rcpth.job-no
                    BY rm-rcpth.job-no2
                    BY item.mat-type
                    BY item.i-no
                    BY rm-rdtlh.s-num
                    BY rm-rdtlh.b-num transaction:

    {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

      IF FIRST-of(rm-rcpth.trans-date) THEN v-first[1] = yes.

      v-job-no = FILL(" ",6 - LENGTH(TRIM(rm-rdtlh.job-no))) +
                 TRIM(rm-rdtlh.job-no) + "-" + STRING(rm-rdtlh.job-no2,"99").

      IF v-job-no begins "-" THEN v-job-no = "".

      v-rm-qty = rm-rdtlh.qty.

      IF rm-rcpth.pur-uom ne "LB" THEN
         run sys/ref/convquom.p(rm-rcpth.pur-uom, "LB", 0, 0, 0, 0, v-rm-qty, output v-rm-qty).

      ASSIGN
         v-qty[1] = v-qty[1] + v-rm-qty
         v-qty[3] = v-qty[3] + v-rm-qty.

      IF LAST-OF(rm-rdtlh.b-num) THEN DO: 
         ASSIGN
            v-board   = ""
            v-brd-qty = 0
            v-m-code  = "".


         FOR EACH job WHERE job.company EQ rm-rdtlh.company
                        AND job.job-no  EQ rm-rdtlh.job-no
                        AND job.job-no2 EQ rm-rdtlh.job-no2 NO-LOCK,
             EACH job-hdr NO-LOCK WHERE job-hdr.company = job.company
                                       AND job-hdr.job-no  = job.job-no
                                       AND job-hdr.job-no2 = job.job-no2
                                       AND (job-hdr.frm = rm-rdtlh.s-num OR rm-rdtlh.s-num EQ 0)
                                       and (job-hdr.blank-no eq rm-rdtlh.b-num OR rm-rdtlh.b-num EQ 0),
               FIRST itemfg WHERE itemfg.company = job-hdr.company
                              AND itemfg.i-no    = job-hdr.i-no
                              AND itemfg.procat  GE v-fCat
                              AND itemfg.procat  LE v-tCat NO-LOCK:

               FOR EACH job-mat WHERE job-mat.company EQ job.company
                                  AND job-mat.job     EQ job.job
                                  AND job-mat.job-no  EQ job.job-no
                                  AND job-mat.job-no2 EQ job.job-no2
                                  AND (job-mat.frm    EQ rm-rdtlh.s-num OR rm-rdtlh.s-num EQ 0)
                                  AND (job-mat.blank-no EQ rm-rdtlh.b-num OR rm-rdtlh.b-num EQ 0)
                            USE-INDEX seq-idx NO-LOCK,
                  FIRST b-item WHERE b-item.company  EQ job-mat.company
                                 AND b-item.i-no     EQ job-mat.i-no
                                 AND INDEX("1234BPR",b-item.mat-type) GT 0 NO-LOCK:

                   {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

                  IF v-board[1] EQ "" THEN 
                     v-board[1] = b-item.i-no.

                  FOR EACH mat-act WHERE mat-act.company EQ cocode
                                     AND mat-act.job     EQ job-mat.job
                                     AND mat-act.job-no  EQ job-mat.job-no
                                     AND mat-act.job-no2 EQ job-mat.job-no2
                                     AND mat-act.s-num   EQ job-mat.frm
                                     AND mat-act.b-num   EQ job-mat.blank-no
                                     AND mat-act.i-no    EQ job-mat.i-no
                               USE-INDEX job NO-LOCK:

                     IF v-board[2] EQ "" THEN 
                        v-board[2] = b-item.i-no.

                     RUN sys/ref/convquom.p(job-mat.qty-uom, "EA", job-mat.basis-w,
                                            job-mat.len, job-mat.wid, b-item.s-dep,
                                            mat-act.qty, OUTPUT v-brd-qty[1]).

                     v-brd-qty[2] = v-brd-qty[2] + v-brd-qty[1].
                  END. /* FOR EACH mat-act */
               END. /* FOR EACH job-mat */

               FOR EACH mch-act WHERE mch-act.company EQ cocode
                                  AND mch-act.job-no  EQ job.job-no
                                  AND mch-act.job-no2 EQ job.job-no2
                                  AND (mch-act.frm    EQ rm-rdtlh.s-num OR rm-rdtlh.s-num EQ 0)
                                  AND (mat-act.b-num EQ rm-rdtlh.b-num OR rm-rdtlh.b-num EQ 0)  NO-LOCK,

                  FIRST mach WHERE mach.company EQ cocode
                               AND mach.loc     EQ locode 
                               AND mach.m-code  EQ mch-act.m-code
                               AND ((item.mat-type EQ "G" AND (mach.dept[1] EQ "GL" OR
                                                               mach.dept[2] EQ "GL" OR
                                                               mach.dept[3] EQ "GL" OR
                                                               mach.dept[4] EQ "GL")) OR
                                    (item.mat-type EQ "I" AND (mach.dept[1] EQ "PR" OR
                                                               mach.dept[2] EQ "PR" OR
                                                               mach.dept[3] EQ "PR" OR
                                                               mach.dept[4] EQ "PR"))) NO-LOCK
                          BREAK BY mch-act.frm      desc
                                BY mch-act.blank-no desc:

                   {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

                  IF (item.mat-type EQ "G"               AND
                      mch-act.frm      EQ rm-rdtlh.s-num AND
                      mch-act.blank-no EQ rm-rdtlh.b-num AND
                      mch-act.dept EQ "GL")                   OR
                     (item.mat-type EQ "I" AND
                      mch-act.dept EQ "PR")                   OR
                     last(mch-act.frm)                        THEN DO:

                     v-m-code = mach.m-code.
                     leave.
                  END.
               END. /* FOR EACH mch-act */

               IF v-m-code EQ "" THEN
                  FOR EACH job-mch WHERE job-mch.company EQ cocode
                                     AND job-mch.job     EQ job.job
                                     AND job-mch.job-no  EQ job.job-no
                                     AND job-mch.job-no2 EQ job.job-no2
                                     AND (job-mch.frm    EQ rm-rdtlh.s-num OR rm-rdtlh.s-num EQ 0)
                                     AND (job-mch.blank-no EQ rm-rdtlh.b-num OR rm-rdtlh.b-num EQ 0) NO-LOCK,
                     FIRST mach WHERE mach.company EQ cocode
                                  AND mach.loc     EQ locode 
                                  AND mach.m-code  EQ job-mch.m-code
                                  AND ((item.mat-type EQ "G" AND (mach.dept[1] EQ "GL" OR
                                                                  mach.dept[2] EQ "GL" OR
                                                                  mach.dept[3] EQ "GL" OR
                                                                  mach.dept[4] EQ "GL")) OR
                                       (item.mat-type EQ "I" AND (mach.dept[1] EQ "PR" OR
                                                                  mach.dept[2] EQ "PR" OR
                                                                  mach.dept[3] EQ "PR" OR
                                                                  mach.dept[4] EQ "PR"))) NO-LOCK

                             BREAK BY job-mch.frm      DESC
                                   BY job-mch.blank-no DESC:

                   {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

                     IF (item.mat-type    EQ "G"               AND
                         job-mch.frm      EQ rm-rdtlh.s-num    AND
                         job-mch.blank-no EQ rm-rdtlh.b-num    AND
                         job-mch.dept EQ "GL")                  OR
                        (item.mat-type EQ "I"                  AND
                         job-mch.dept EQ "PR")                  OR
                         LAST(job-mch.frm)                          THEN DO:

                        v-m-code = mach.m-code.
                        LEAVE.
                     END.
                  END. /* FOR EACH job-mch */

            IF v-board[2] EQ "" THEN 
               v-board[2] = v-board[1].
            CREATE tt-inks-glues.
            ASSIGN
               tt-inks-glues.trans-date = rm-rcpth.trans-date 
               tt-inks-glues.job-no     = v-job-no
               tt-inks-glues.qty        = v-qty[1]
               tt-inks-glues.m-code     = v-m-code
               tt-inks-glues.board      = v-board[2] 
               tt-inks-glues.brd-qty    = v-brd-qty[2]
               tt-inks-glues.i-no       = rm-rcpth.i-no
               tt-inks-glues.procat     = itemfg.procat.

/*             DISPLAY                                */
/*                rm-rcpth.trans-date WHEN v-first[1] */
/*                v-job-no                            */
/*                v-board[2]                          */
/*                v-brd-qty[2]                        */
/*                rm-rcpth.i-no                       */
/*                v-qty[1]                            */
/*                v-m-code                            */
/*                WITH FRAME itemx.                   */
/*             DOWN WITH FRAME itemx.                 */


            ASSIGN
               v-qty[2] = v-qty[2] + v-qty[1]
               v-qty[1] = 0
               v-first[1] = NO.

         END. /* FOR EACH job */

/*          IF LAST(rm-rcpth.trans-date) THEN DO: */
/*             UNDERLINE v-qty[1]                 */
/*             WITH FRAME itemx.                  */
/*                                                */
/*             DISPLAY                            */
/*                "GRAND TOTALS" @ rm-rcpth.i-no  */
/*                v-qty[3]       @ v-qty[1]       */
/*                WITH FRAME itemx.               */
/*          END.                                  */
      END. /* IF last-of(rm-rdtlh.b-num) */
END.

ASSIGN
   v-tot-trans-qty = 0
   v-grand-tot-trans-qty = 0.

IF TG_sort-cat = NO THEN DO:
   FOR EACH tt-inks-glues BREAK BY tt-inks-glues.trans-date:

       {custom/statusMsg.i "'Processing Item # ' + string(tt-inks-glues.i-no)"} 

      DISPLAY 
         tt-inks-glues.trans-date  WHEN FIRST-OF(tt-inks-glues.trans-date)
         tt-inks-glues.job-no      @ v-job-no
         tt-inks-glues.board       @ v-board[2]
         tt-inks-glues.brd-qty     @ v-brd-qty[2]
         tt-inks-glues.i-no        @ rm-rcpth.i-no
         tt-inks-glues.qty         @ v-qty[1]
         tt-inks-glues.m-code      @ v-m-code
         tt-inks-glues.procat      
         WITH FRAME itemx.
      DOWN WITH FRAME itemx.

      ASSIGN 
         v-tot-trans-qty       = v-tot-trans-qty + tt-inks-glues.qty
         v-grand-tot-trans-qty = v-grand-tot-trans-qty + tt-inks-glues.qty.

      IF LAST-OF(tt-inks-glues.trans-date) THEN DO:
         DISPLAY      
            v-tot-trans-qty     
            SKIP
            WITH FRAME itemb NO-LABELS.
         v-tot-trans-qty = 0.
      END.

      IF v-export THEN
         PUT STREAM s-temp UNFORMATTED
            TRIM(STRING(tt-inks-glues.trans-date,tt-inks-glues.trans-date:FORMAT))
                                                                  + "," +
            TRIM(tt-inks-glues.job-no)                            + "," +
            TRIM(tt-inks-glues.board)                             + "," +
            TRIM(STRING(tt-inks-glues.brd-qty,"->>>>>>>>>9"))     + "," +
            TRIM(tt-inks-glues.i-no)                              + "," +
            TRIM(STRING(tt-inks-glues.qty,"->>>>>>>>>9.9<<<<"))   + "," +
            TRIM(tt-inks-glues.m-code)                            + "," +
            TRIM(tt-inks-glues.procat)
            SKIP.
   END.

   DISPLAY      
      v-grand-tot-trans-qty    
      SKIP
      WITH FRAME itemc NO-LABELS.
END.
ELSE DO:
   FOR EACH tt-inks-glues BREAK BY tt-inks-glues.procat
                                BY tt-inks-glues.trans-date:

       {custom/statusMsg.i "'Processing Item # ' + string(tt-inks-glues.i-no)"} 

      DISPLAY 
         tt-inks-glues.trans-date  WHEN FIRST-OF(tt-inks-glues.trans-date)
         tt-inks-glues.job-no      @ v-job-no
         tt-inks-glues.board       @ v-board[2]
         tt-inks-glues.brd-qty     @ v-brd-qty[2]
         tt-inks-glues.i-no        @ rm-rcpth.i-no
         tt-inks-glues.qty         @ v-qty[1]
         tt-inks-glues.m-code      @ v-m-code
         tt-inks-glues.procat      WHEN FIRST-OF(tt-inks-glues.procat)
         WITH FRAME itemx.
      DOWN WITH FRAME itemx.

      ASSIGN 
         v-tot-trans-qty       = v-tot-trans-qty + tt-inks-glues.qty
         v-grand-tot-trans-qty = v-grand-tot-trans-qty + tt-inks-glues.qty.

      IF LAST-OF(tt-inks-glues.procat) THEN DO:
         DISPLAY      
            v-tot-trans-qty     
            SKIP
            WITH FRAME itemb NO-LABELS.
         v-tot-trans-qty = 0.
      END.
      IF v-export THEN
         PUT STREAM s-temp UNFORMATTED
            TRIM(STRING(tt-inks-glues.trans-date,tt-inks-glues.trans-date:FORMAT))
                                                                  + "," +
            TRIM(tt-inks-glues.job-no)                            + "," +
            TRIM(tt-inks-glues.board)                             + "," +
            TRIM(STRING(tt-inks-glues.brd-qty,"->>>>>>>>>9"))     + "," +
            TRIM(tt-inks-glues.i-no)                              + "," +
            TRIM(STRING(tt-inks-glues.qty,"->>>>>>>>>9.9<<<<"))   + "," +
            TRIM(tt-inks-glues.m-code)                            + "," +
            TRIM(tt-inks-glues.procat)
            SKIP.
   END.

   DISPLAY      
      v-grand-tot-trans-qty    
      SKIP
      WITH FRAME itemc NO-LABELS.
END.

IF v-export THEN DO:
   OUTPUT STREAM s-temp CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------- rm/rep/rmtrans3.p 10/03 JLF */
/* raw materials - Ink/Glue Consumption Report                                */
/* -------------------------------------------------------------------------- */

    {custom/statusMsg.i "'Processing... '"} 

IF RS_material = 1 THEN 
   RUN print-inks-glues.
ELSE
   RUN print-coat-wax.
/*
{sys/form/r-topw.f}

def var v-fitem like rm-rcpth.i-no.
def var v-titem like v-fitem                  init "zzzzzzzzzz".
def var v-fpcat like item.procat.
def var v-tpcat like v-fpcat                  init "zzzzz".
def var v-fdate as   date format "99/99/9999" init 01/01/0001.
def var v-tdate like v-fdate                  init today.
def var v-fjob  like job.job-no.
def var v-tjob  like v-fjob                   init "zzzzzz".
def var v-fjob2 like job.job-no2 format "99".
def var v-tjob2 like v-fjob2                  init 99.
def var v-mtype as   char format "x(47)".
def var v-export as log init no format "Y/N".
def var v-exp-name as char format "x(40)" initial "rmtrans3.csv".
DEF VAR v-fCat LIKE itemfg.procat NO-UNDO.
DEF VAR v-tCat LIKE itemfg.procat NO-UNDO.

def var v-job-no as char format "x(9)".
def var v-rm-qty as dec.
def var v-qty as dec format "->,>>>,>>>,>>9.9<<<<" extent 3.
def var v-m-code like mach.m-code format "x(6)".
def var v-board like item.i-no extent 2.
def var v-brd-qty as int format "->,>>>,>>>,>>9" extent 2.
def var v-first as log extent 3.

def buffer b-item for item.

form rm-rcpth.trans-date    label "Issue Date"
     v-job-no               label "   Job #"
     v-board[2]             label "Board"
     v-brd-qty[2]           label "Sheets Issued"
     rm-rcpth.i-no          label "Ink/Glue"
     v-qty[1]               label "Qty Issued/Lbs"
     v-m-code               label "Machine"
     skip
    with frame itemx no-box down stream-io width 132.

DEF VAR v-hdr       AS   CHAR INIT "Issue Date,Job#,Board,Sheets Issued,Ink/Glue,Qty Issued/Lbs,Machine," NO-UNDO.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-fitem    = begin_rm-no
 v-titem    = end_rm-no
 v-fpcat    = begin_procat
 v-tpcat    = end_procat
 v-fdate    = begin_date
 v-tdate    = end_date
 v-fjob     = fill(" ",6 - length(trim(begin_job-no))) +
              trim(begin_job-no) + string(int(begin_job-no2),"99")
 v-tjob     = fill(" ",6 - length(trim(end_job-no)))   +
              trim(end_job-no)   + string(int(end_job-no2),"99")
 v-export   = tb_excel
 v-exp-name = fi_file.

do with frame {&frame-name}:          
  do i = 1 to select-mat:num-items:
    if select-mat:is-selected(i) then
      v-mtype = v-mtype + trim(substr(select-mat:entry(i),1,5)) + ",".
  end.

  if substr(v-mtype,length(trim(v-mtype)),1) eq "," then
    substr(v-mtype,length(trim(v-mtype)),1) = "".

  mat-types = v-mtype.

  do i = 1 to length(mat-types):
    if substr(mat-types,i,1) eq "," then substr(mat-types,i,1) = " ".
  end.

  display mat-types.

  mat-types:HIDDEN = YES.
end.
v-qty = 0.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

display "" with frame r-top.

    if v-export then do:
      output stream s-temp to value(v-exp-name).

      put stream s-temp unformatted v-hdr skip.
    end.

    for each rm-rcpth
        where rm-rcpth.company    eq cocode
          and rm-rcpth.i-no       ge v-fitem
          and rm-rcpth.i-no       le v-titem
          and rm-rcpth.trans-date ge v-fdate
          and rm-rcpth.trans-date le v-tdate
          and rm-rcpth.rita-code  eq "I"
        use-index i-no no-lock,

        each rm-rdtlh
        where rm-rdtlh.r-no      eq rm-rcpth.r-no
          and rm-rdtlh.rita-code eq rm-rcpth.rita-code
          and rm-rdtlh.job-no    ge substr(v-fjob,1,6)
          and rm-rdtlh.job-no    le substr(v-tjob,1,6)
          and fill(" ",6 - length(trim(rm-rdtlh.job-no))) +
              trim(rm-rdtlh.job-no) + string(rm-rdtlh.job-no2,"99")
                                 ge v-fjob
          and fill(" ",6 - length(trim(rm-rdtlh.job-no))) +
              trim(rm-rdtlh.job-no) + string(rm-rdtlh.job-no2,"99")
                                 le v-tjob
        no-lock,

        first item
        where item.company eq cocode
          and item.i-no    eq rm-rcpth.i-no
          and item.procat  ge v-fpcat
          and item.procat  le v-tpcat
          and index(v-mtype,item.mat-type) gt 0
          and index("GIVW",item.mat-type) gt 0
        no-lock

        break by rm-rcpth.trans-date
              by rm-rcpth.job-no
              by rm-rcpth.job-no2
              by item.mat-type
              by item.i-no
              by rm-rdtlh.s-num
              by rm-rdtlh.b-num

        transaction:

      if first-of(rm-rcpth.trans-date) then v-first[1] = yes.

      v-job-no = fill(" ",6 - length(trim(rm-rdtlh.job-no))) +
                 trim(rm-rdtlh.job-no) + "-" + string(rm-rdtlh.job-no2,"99").

      if v-job-no begins "-" then v-job-no = "".

      v-rm-qty = rm-rdtlh.qty.

      if rm-rcpth.pur-uom ne "LB" then
        run sys/ref/convquom.p(rm-rcpth.pur-uom, "LB", 0, 0, 0, 0,
                               v-rm-qty, output v-rm-qty).

      v-qty[1] = v-qty[1] + v-rm-qty.
      v-qty[3] = v-qty[3] + v-rm-qty.
      if last-of(rm-rdtlh.b-num) then do: 
        assign
         v-board   = ""
         v-brd-qty = 0
         v-m-code  = "".

        for each job
            where job.company eq rm-rcpth.company
              and job.job-no  eq rm-rdtlh.job-no
              and job.job-no2 eq rm-rdtlh.job-no2
            no-lock:

           FOR EACH job-hdr NO-LOCK WHERE job-hdr.company = job.company 
                                      AND job-hdr.job-no  = job.job-no
                                      AND job-hdr.job-no2 = job.job-no2,
              FIRST itemfg WHERE itemfg.company = job-hdr.company
                             AND itemfg.i-no    = job-hdr.i-no
                             AND itemfg.procat  GE v-fCat
                             AND itemfg.procat  LE v-tCat NO-LOCK.



             for each job-mat
                 where job-mat.company eq job.company
                   and job-mat.job     eq job.job
                   and job-mat.job-no  eq job.job-no
                   and job-mat.job-no2 eq job.job-no2
                   and (job-mat.frm    eq rm-rdtlh.s-num or rm-rdtlh.s-num eq 0)
                 use-index seq-idx no-lock,

                 first b-item
                 where b-item.company  eq job-mat.company
                   and b-item.i-no     eq job-mat.i-no
                   and index("1234BPRWV",b-item.mat-type) gt 0
                 no-lock:

               if v-board[1] eq "" then v-board[1] = b-item.i-no.

               for each mat-act
                   where mat-act.company eq cocode
                     and mat-act.job     eq job-mat.job
                     and mat-act.job-no  eq job-mat.job-no
                     and mat-act.job-no2 eq job-mat.job-no2
                     and mat-act.s-num   eq job-mat.frm
                     and mat-act.b-num   eq job-mat.blank-no
                     and mat-act.i-no    eq job-mat.i-no
                   use-index job no-lock:

                 if v-board[2] eq "" then v-board[2] = b-item.i-no.

                 run sys/ref/convquom.p(job-mat.qty-uom, "EA", job-mat.basis-w,
                                        job-mat.len, job-mat.wid, b-item.s-dep,
                                        mat-act.qty, output v-brd-qty[1]).

                 v-brd-qty[2] = v-brd-qty[2] + v-brd-qty[1].
               end.
             end.

             for each mch-act
                 where mch-act.company eq cocode
                   and mch-act.job     eq job.job
                   and mch-act.job-no  eq job.job-no
                   and mch-act.job-no2 eq job.job-no2
                   and (mch-act.frm    eq rm-rdtlh.s-num or rm-rdtlh.s-num eq 0)
                 no-lock,

                 first mach
                 where mach.company eq cocode
                   and mach.loc     eq locode 
                   and mach.m-code  eq mch-act.m-code
                   and ((item.mat-type eq "G" and (mach.dept[1] eq "GL" or
                                                   mach.dept[2] eq "GL" or
                                                   mach.dept[3] eq "GL" or
                                                   mach.dept[4] eq "GL")) or
                        (item.mat-type eq "I" and (mach.dept[1] eq "PR" or
                                                   mach.dept[2] eq "PR" or
                                                   mach.dept[3] eq "PR" or
                                                   mach.dept[4] eq "PR")))
                 no-lock

                 break by mch-act.frm      desc
                       by mch-act.blank-no desc:

               if (item.mat-type eq "G"               and
                   mch-act.frm      eq rm-rdtlh.s-num and
                   mch-act.blank-no eq rm-rdtlh.b-num and
                   mch-act.dept eq "GL")                   or
                  (item.mat-type eq "I" and
                   mch-act.dept eq "PR")                   or
                  last(mch-act.frm)                        then do:

                 v-m-code = mach.m-code.
                 leave.
               end.
             end.

             if v-m-code eq "" then
             for each job-mch
                 where job-mch.company eq cocode
                   and job-mch.job     eq job.job
                   and job-mch.job-no  eq job.job-no
                   and job-mch.job-no2 eq job.job-no2
                   and (job-mch.frm    eq rm-rdtlh.s-num or rm-rdtlh.s-num eq 0)
                 no-lock,

                 first mach
                 where mach.company eq cocode
                   and mach.loc     eq locode 
                   and mach.m-code  eq job-mch.m-code
                   and ((item.mat-type eq "G" and (mach.dept[1] eq "GL" or
                                                   mach.dept[2] eq "GL" or
                                                   mach.dept[3] eq "GL" or
                                                   mach.dept[4] eq "GL")) or
                        (item.mat-type eq "I" and (mach.dept[1] eq "PR" or
                                                   mach.dept[2] eq "PR" or
                                                   mach.dept[3] eq "PR" or
                                                   mach.dept[4] eq "PR")))
                 no-lock

                 break by job-mch.frm      desc
                       by job-mch.blank-no desc:

               if (item.mat-type eq "G"               and
                   job-mch.frm      eq rm-rdtlh.s-num and
                   job-mch.blank-no eq rm-rdtlh.b-num and
                   job-mch.dept eq "GL")                   or
                  (item.mat-type eq "I" and
                   job-mch.dept eq "PR")                   or
                  last(job-mch.frm)                        then do:

                 v-m-code = mach.m-code.
                 leave.
               end.
             end.
           end.

           if v-board[2] eq "" then v-board[2] = v-board[1].

           display rm-rcpth.trans-date when v-first[1]
                   v-job-no
                   v-board[2]
                   v-brd-qty[2]
                   rm-rcpth.i-no
                   v-qty[1]
                   v-m-code
               with frame itemx.
           down with frame itemx.

           if v-export then 
             put stream s-temp unformatted
                 trim(string(rm-rcpth.trans-date,rm-rcpth.trans-date:format))
                                                                   + "," +
                 trim(v-job-no)                                    + "," +
                 trim(v-board[2])                                  + "," +
                 trim(string(v-brd-qty[2],"->>>>>>>>>9"))          + "," +
                 trim(rm-rcpth.i-no)                               + "," +
                 trim(string(v-qty[1],"->>>>>>>>>9.9<<<<"))        + "," +
                 trim(v-m-code)
                 skip.

           assign
            v-qty[2] = v-qty[2] + v-qty[1]

            v-qty[1] = 0

            v-first[1] = no.
         end.

         if last(rm-rcpth.trans-date) then do:
           underline v-qty[1]
               with frame itemx.

           display "GRAND TOTALS" @ rm-rcpth.i-no
                   v-qty[3]       @ v-qty[1]
                 with frame itemx.
         end.

    end.
   END.
    /* rtc 08/11/2008 */
    IF v-export THEN DO:
        OUTPUT STREAM s-temp close.
        IF tb_runExcel THEN
            OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
    END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

*/

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

