&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: aprep\r-vaging.w

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



DEF VAR lv-default-comp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO INIT 0.

FOR EACH usercomp WHERE usercomp.USER_id = USERID("nosweat") AND  usercomp.loc = "" NO-LOCK :
    v-count = v-count + 1 .
END.
FIND FIRST usercomp WHERE usercomp.USER_id = USERID("nosweat") AND
                                  usercomp.company_default NO-LOCK NO-ERROR.
ASSIGN     
lv-default-comp = IF AVAIL usercomp THEN usercomp.company ELSE "001".



FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.

def new shared var v-s-vend like vend.vend-no.
def new shared var v-e-vend like vend.vend-no.
def new shared var v-idate  as   log format "Invoice/Posting" init yes.
def new shared var v-sort   as   log format "Code/Name"       init yes.
def new shared var v-dtl    as   log format "Detail/Summary"  init yes.
def new shared var v-s-type AS CHAR NO-UNDO.
def new shared var v-e-type AS CHAR NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR v-days AS INTE NO-UNDO EXTENT 4.
DEF VAR v-refnum AS CHAR NO-UNDO.
DEF VAR v-check-date AS DATE NO-UNDO.
DEF VAR v-terms LIKE terms.dscr NO-UNDO.

DEF TEMP-TABLE tt-vend NO-UNDO
    FIELD curr-code LIKE vend.curr-code
    FIELD sorter    LIKE vend.vend-no
    FIELD row-id    AS   ROWID
    INDEX tt-vend curr-code sorter.

DEF STREAM excel.

DEF TEMP-TABLE w-sort NO-UNDO field w-int as int.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_comp end_comp begin_vend ~
end_vend begin_curr end_curr begin_type end_type tb_ACH-only tb_ACH-excl ~
as_of_date rd_date period-days-1 period-days-2 period-days-3 period-days-4 ~
rd_sort tb_detailed lines-per-page lv-ornt rd-dest lv-font-no td-show-parm ~
tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_comp end_comp begin_vend end_vend ~
begin_curr end_curr begin_type end_type tb_ACH-only tb_ACH-excl as_of_date ~
lbl_date rd_date period-days-1 period-days-2 period-days-3 period-days-4 ~
lbl_sort rd_sort tb_detailed lines-per-page lv-ornt rd-dest lv-font-no ~
lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE as_of_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "As of" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_comp AS CHARACTER FORMAT "X(3)":U 
     LABEL "Beginning Company#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_curr AS CHARACTER FORMAT "X(3)":U 
     LABEL "Beginning Currency" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_type AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor Type" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_comp AS CHARACTER FORMAT "X(3)":U INITIAL "zzz" 
     LABEL "Ending Company#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_curr AS CHARACTER FORMAT "X(3)":U INITIAL "zzz" 
     LABEL "Ending Currency" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_type AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor Type" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-vaging.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_date AS CHARACTER FORMAT "X(256)":U INITIAL "Which Date?" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(10)":U INITIAL "Sort By?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 144 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE period-days-1 AS INTEGER FORMAT ">,>>9":U INITIAL 9999 
     LABEL "Period Days 1" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE period-days-2 AS INTEGER FORMAT ">,>>9":U INITIAL 9999 
     LABEL "2" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE period-days-3 AS INTEGER FORMAT ">,>>9":U INITIAL 9999 
     LABEL "3" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE period-days-4 AS INTEGER FORMAT ">,>>9":U INITIAL 9999 
     LABEL "4" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd_date AS CHARACTER INITIAL "Invoice" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Invoice", "Invoice",
"Posting", "Posting"
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Code" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Vendor Code", "Code",
"Vendor Name", "Name"
     SIZE 37 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 9.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 12.62.

DEFINE VARIABLE tb_ACH-excl AS LOGICAL INITIAL no 
     LABEL "Exclude ACH Vendors" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE tb_ACH-only AS LOGICAL INITIAL no 
     LABEL "ACH Vendors Only" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .95 NO-UNDO.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no 
     LABEL "Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .95 NO-UNDO.

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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_comp AT ROW 2.52 COL 23 COLON-ALIGNED
     end_comp AT ROW 2.52 COL 63 COLON-ALIGNED
     begin_vend AT ROW 3.48 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Vendor"
     end_vend AT ROW 3.48 COL 63 COLON-ALIGNED HELP
          "Enter Ending Vendor"
     begin_curr AT ROW 4.43 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Currency Code"
     end_curr AT ROW 4.43 COL 63 COLON-ALIGNED HELP
          "Enter Ending Currency Code"
     begin_type AT ROW 5.38 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Currency Code" WIDGET-ID 10
     end_type AT ROW 5.38 COL 63 COLON-ALIGNED HELP
          "Enter Beginning Currency Code" WIDGET-ID 12
     tb_ACH-only AT ROW 6.71 COL 25 WIDGET-ID 14
     tb_ACH-excl AT ROW 6.71 COL 52.4 WIDGET-ID 16
     as_of_date AT ROW 7.91 COL 23 COLON-ALIGNED HELP
          "Enter As od date"
     lbl_date AT ROW 7.91 COL 47 COLON-ALIGNED NO-LABEL
     rd_date AT ROW 7.91 COL 64 NO-LABEL
     period-days-1 AT ROW 9.24 COL 23 COLON-ALIGNED WIDGET-ID 2
     period-days-2 AT ROW 9.24 COL 35 COLON-ALIGNED WIDGET-ID 4
     period-days-3 AT ROW 9.24 COL 48 COLON-ALIGNED WIDGET-ID 6
     period-days-4 AT ROW 9.24 COL 61 COLON-ALIGNED WIDGET-ID 8
     lbl_sort AT ROW 10.81 COL 23 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 10.81 COL 35 NO-LABEL
     tb_detailed AT ROW 12.1 COL 38
     lines-per-page AT ROW 14.52 COL 84 COLON-ALIGNED
     lv-ornt AT ROW 15.29 COL 31 NO-LABEL
     rd-dest AT ROW 15.33 COL 6 NO-LABEL
     lv-font-no AT ROW 17.19 COL 37 COLON-ALIGNED
     lv-font-name AT ROW 18.43 COL 31 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19.62 COL 33
     tb_excel AT ROW 20.95 COL 53 RIGHT-ALIGNED
     tb_runExcel AT ROW 20.95 COL 74 RIGHT-ALIGNED
     fi_file AT ROW 21.76 COL 31 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 23.48 COL 19
     btn-cancel AT ROW 23.48 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.38 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     RECT-6 AT ROW 13.71 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95 BY 24.48.


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
         TITLE              = "Vendor Aging"
         HEIGHT             = 24.71
         WIDTH              = 96
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
       as_of_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_comp:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_curr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_comp:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_curr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_date IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_date".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       period-days-1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       period-days-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       period-days-3:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       period-days-4:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "Parm".

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_ACH-excl:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_ACH-only:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Vendor Aging */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vendor Aging */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME as_of_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL as_of_date C-Win
ON LEAVE OF as_of_date IN FRAME FRAME-A /* As of */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_curr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_curr C-Win
ON LEAVE OF begin_curr IN FRAME FRAME-A /* Beginning Currency */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_type C-Win
ON LEAVE OF begin_type IN FRAME FRAME-A /* Beginning Vendor Type */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor# */
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

  run run-report. 
  STATUS DEFAULT "Processing Complete".
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &type= ''
                            &begin_cust= "begin_vend"
                            &END_cust= "begin_vend" 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Vendor"
                             &begin_cust=begin_vend
                             &END_cust=end_vend
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust=begin_vend
                                  &END_cust=end_vend
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN OUTPUT-to-port.
  end case.
  SESSION:SET-WAIT-STATE("").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_curr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_curr C-Win
ON LEAVE OF end_curr IN FRAME FRAME-A /* Ending Currency */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_type C-Win
ON LEAVE OF end_type IN FRAME FRAME-A /* Ending Vendor Type */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
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


&Scoped-define SELF-NAME period-days-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period-days-1 C-Win
ON LEAVE OF period-days-1 IN FRAME FRAME-A /* Period Days 1 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME period-days-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period-days-2 C-Win
ON LEAVE OF period-days-2 IN FRAME FRAME-A /* 2 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME period-days-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period-days-3 C-Win
ON LEAVE OF period-days-3 IN FRAME FRAME-A /* 3 */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME period-days-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period-days-4 C-Win
ON LEAVE OF period-days-4 IN FRAME FRAME-A /* 4 */
DO:
  ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ACH-excl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ACH-excl C-Win
ON VALUE-CHANGED OF tb_ACH-excl IN FRAME FRAME-A /* Exclude ACH Vendors */
DO:
  assign {&self-name}.
  IF tb_ACH-only AND tb_ACH-excl THEN 
      ASSIGN 
        tb_ACH-only = NO
        tb_ACH-only:SCREEN-VALUE = "NO".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ACH-only
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ACH-only C-Win
ON VALUE-CHANGED OF tb_ACH-only IN FRAME FRAME-A /* ACH Vendors Only */
DO:
  assign {&self-name}.
  IF tb_ACH-only AND tb_ACH-excl THEN 
      ASSIGN 
        tb_ACH-excl = NO
        tb_ACH-excl:SCREEN-VALUE = "NO".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
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

  ASSIGN period-days-1 = 30
         period-days-2 = 60
         period-days-3 = 90
         period-days-4 = 120
             .

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    as_of_date:SCREEN-VALUE = STRING(TODAY).
    APPLY "entry" TO begin_vend.

    IF v-count LE 1 THEN
        ASSIGN
          begin_comp:SENSITIVE = NO
          end_comp:SENSITIVE   = NO 
          begin_comp:SCREEN-VALUE = lv-default-comp
          begin_comp = lv-default-comp
          end_comp:SCREEN-VALUE = lv-default-comp
          end_comp = lv-default-comp.
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
  DISPLAY begin_comp end_comp begin_vend end_vend begin_curr end_curr begin_type 
          end_type tb_ACH-only tb_ACH-excl as_of_date lbl_date rd_date 
          period-days-1 period-days-2 period-days-3 period-days-4 lbl_sort 
          rd_sort tb_detailed lines-per-page lv-ornt rd-dest lv-font-no 
          lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_comp end_comp begin_vend end_vend begin_curr 
         end_curr begin_type end_type tb_ACH-only tb_ACH-excl as_of_date 
         rd_date period-days-1 period-days-2 period-days-3 period-days-4 
         rd_sort tb_detailed lines-per-page lv-ornt rd-dest lv-font-no 
         td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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
/* ---------------------------------------------------- ap/ap-aged.p  4/94 RM */
/* Vendor Aging Report Program - A/P Module                                   */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

def var v-date like ap-inv.inv-date NO-UNDO.
def var d as int label "Days" NO-UNDO.
def var ni as INT NO-UNDO.
def var cust-t as dec format "->>>,>>>,>>9.99" extent 5 NO-UNDO.
def var curr-t as dec format "$->>>,>>>,>>9.99" extent 6 NO-UNDO.
def var grand-t as dec format "$->>>,>>>,>>9.99" extent 6 NO-UNDO.
def var s as INT NO-UNDO.
def var v-amt as dec format "->,>>>,>>>,>>9.99" NO-UNDO.
def var ag like v-amt format "->>>,>>>,>>9.99" extent 5 NO-UNDO.
def var t1 as dec format "$->>,>>>,>>9.99" NO-UNDO.
def var t2 as dec format "$->>,>>>,>>9.99" NO-UNDO.
def var t3 as dec format "$->>,>>>,>>9.99" NO-UNDO.
def var m1 as char format "x(20)" NO-UNDO.
def var m2 as char format "x(15)" NO-UNDO.
def var m3 as char format "(999) 999-9999" NO-UNDO .
def var first-time as log init YES NO-UNDO.
def var time_stamp as CHAR NO-UNDO.
def var op as char format "x" init "D" NO-UNDO.
DEF VAR ll-mult-curr AS LOG NO-UNDO.
DEF VAR lv-page-break AS CHAR NO-UNDO.
DEF VAR lv-f-bot-hdr AS CHAR FORMAT "x(12)" NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.

def buffer xap-ledger for ap-ledger.

FORM HEADER SKIP(1)
     lv-page-break FORMAT "x(200)"
WITH PAGE-TOP FRAME r-top-1 STREAM-IO WIDTH 200 NO-BOX.

FORM HEADER SKIP(1)
     "VENDOR#  NAME  TERMS" SKIP
     "PHONE-   TYPE" skip
     "  INVOICE#      DATE                 AMOUNT #DAYS             0-" + string(period-days-1) + FILL(" ",11) +   
                                                                           string(period-days-1 + 1) + "-" + string(period-days-2) + FILL(" ",11) +
                                                                           string(period-days-2 + 1) + "-" + string(period-days-3) + FILL(" ",11) +
                                                                           string(period-days-3 + 1) + "-" + string(period-days-4) + FILL(" ",9) +
                                                                           string(period-days-4 + 1) + "+"  + FILL(" ",10) FORM "x(143)"
     SKIP FILL("_",143) FORMAT "x(143)"
WITH PAGE-TOP FRAME r-top-2 STREAM-IO WIDTH 200 NO-BOX.

FORM HEADER
     SPACE(10)
     lv-f-bot-hdr
     "          0 -"  string(period-days-1) + FILL(" ",10) +   
                      string(period-days-1 + 1) + " - " + string(period-days-2) + FILL(" ",10) +
                      string(period-days-2 + 1) + " - " + string(period-days-3) + FILL(" ",10) +
                      string(period-days-3 + 1) + " - " + string(period-days-4) + FILL(" ",10) +
                      string(period-days-4 + 1) + "+"  + FILL(" ",10) +
     "Total Payables" FORM "x(143)"
     SKIP SPACE(10) FILL("_",143) FORMAT "x(132)"
WITH FRAME f-bot DOWN STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-UNDERLINE.


SESSION:SET-WAIT-STATE ("general").

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-s-vend  = begin_vend
 v-e-vend  = end_vend
 v-s-type  = begin_type
 v-e-type  = end_type
 v-idate   = rd_date eq "Invoice"
 v-sort    = rd_sort eq "Name"
 v-dtl     = tb_detailed
 v-date    = as_of_date
 v-days[1] = period-days-1
 v-days[2] = period-days-2
 v-days[3] = period-days-3
 v-days[4] = period-days-4

 str-tit3 = "Company From: " + STRING(begin_comp) + " To: " + STRING(end_comp) + "      " + "As of Date: " + STRING(v-date)
 {sys/inc/ctrtext.i str-tit3 144}.


do with frame {&frame-name}:
 EMPTY TEMP-TABLE w-sort.

  do li = 1 to 4:
    create w-sort.
    w-int = v-days[li].
  end.
  li = 0.
  for each w-sort by w-int:
    ASSIGN
       li = li + 1
       v-days[li] = w-int.
    if i gt 3 then leave.
  end.
  assign
   period-days-1:screen-value = string(v-days[1])
   period-days-1
   period-days-2:screen-value = string(v-days[2])
   period-days-2
   period-days-3:screen-value = string(v-days[3])
   period-days-3
   period-days-4:screen-value = string(v-days[4])
   period-days-4.
end.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

VIEW FRAME r-top.

  ASSIGN grand-t = 0.

  EMPTY TEMP-TABLE tt-vend.

  FOR EACH company WHERE
       company.company GE begin_comp AND
       company.company LE end_comp
       NO-LOCK,

    EACH vend NO-LOCK
      WHERE vend.company EQ company.company
        AND vend.vend-no GE v-s-vend
        AND vend.vend-no LE v-e-vend
        AND vend.TYPE    GE v-s-type
        AND vend.TYPE    LE v-e-type
        AND ((vend.curr-code GE begin_curr    AND
              vend.curr-code LE end_curr)           OR
             (vend.curr-code EQ ""            AND
              company.curr-code GE begin_curr AND
              company.curr-code LE end_curr))
        AND ((vend.payment-type NE "ACH" AND tb_ACH-excl) OR NOT tb_ACH-excl)
        AND ((vend.payment-type EQ "ACH" AND tb_ACH-only) OR NOT tb_ACH-only):
       {custom/statusMsg.i " 'Processing Vendor#  '  + string(vend.vend-no) "}
    FOR EACH ap-inv
        WHERE ap-inv.company   EQ company.company
          AND ap-inv.vend-no   EQ vend.vend-no
          AND ap-inv.posted    EQ YES
          AND (ap-inv.inv-date LE as_of_date OR NOT v-idate)
        USE-INDEX ap-inv NO-LOCK,

        FIRST ap-ledger
        WHERE ap-ledger.company  EQ company.company
          AND ap-ledger.vend-no  EQ ap-inv.vend-no
          AND ap-ledger.ref-date EQ ap-inv.inv-date
          AND ap-ledger.refnum   EQ ("INV# " + ap-inv.inv-no)
          AND (ap-ledger.tr-date LE as_of_date OR v-idate)
        USE-INDEX ap-ledger NO-LOCK:

      CREATE tt-vend.
      ASSIGN
       tt-vend.curr-code = IF vend.curr-code EQ "" THEN company.curr-code
                                                   ELSE vend.curr-code
       tt-vend.sorter    = IF v-sort THEN vend.name ELSE vend.vend-no
       tt-vend.row-id    = ROWID(vend).

      IF tt-vend.curr-code NE company.curr-code THEN ll-mult-curr = YES.

      LEAVE.
    END.
  END.

  IF tb_excel THEN DO:
     OUTPUT STREAM excel TO VALUE(fi_file).

     IF ll-mult-curr THEN
        excelheader = excelheader + "CURRENCY,".

     excelheader = excelheader
                 + "VENDOR#,VENDOR NAME,PHONE,TYPE,TERMS,INVOICE#,DATE,AMOUNT,#DAYS,"
                 + "0-" + STRING(period-days-1) + "," + STRING(period-days-1 + 1) + "-" 
                        + STRING(period-days-2) + "," + STRING(period-days-2 + 1) + "-" 
                        + STRING(period-days-3) + "," + STRING(period-days-3 + 1) + "-" 
                        + STRING(period-days-4) + "," + STRING(period-days-4 + 1) + "+" 
                        + ",Total Payables,".

     PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
  END.

  FOR EACH tt-vend,
      FIRST vend WHERE ROWID(vend) EQ tt-vend.row-id NO-LOCK
      BREAK BY tt-vend.curr-code
            BY tt-vend.sorter:

      {custom/statusMsg.i " 'Processing Vendor#  '  + string(vend.vend-no) "}

    IF FIRST-OF(tt-vend.curr-code) THEN DO:
      lv-page-break = "Currency: " + TRIM(tt-vend.curr-code).

      IF FIRST(tt-vend.curr-code) THEN DO:
        IF ll-mult-curr THEN VIEW FRAME r-top-1.
        VIEW FRAME r-top-2.
      END.

      IF ll-mult-curr OR FIRST(tt-vend.curr-code) THEN PAGE.
    END.

    {ap/ap-aged.i}

    IF LAST-OF(tt-vend.curr-code) THEN DO:
       IF ll-mult-curr THEN DO:
          PUT SKIP(2).

          ASSIGN
           lv-f-bot-hdr = " CURR TOTALS"
           curr-t[6]    = 0.

          DO i = 1 TO 5:
            curr-t[6] = curr-t[6] + curr-t[i].
          END.

          VIEW FRAME f-bot.
          DOWN.

          DISPLAY SPACE(10) lv-f-bot-hdr
                  curr-t[1]
                  curr-t[2]
                  curr-t[3]
                  curr-t[4]
                  curr-t[5]
                  curr-t[6]
                  SKIP

              WITH FRAME bot1 NO-BOX NO-LABELS NO-ATTR-SPACE STREAM-IO WIDTH 200.

          DISPLAY "PERCENTAGE COMPOSITION" SPACE(2)
                  (curr-t[1] / t2) * 100 FORMAT "->>>>>>>>>>9.99%"
                  (curr-t[2] / t2) * 100 FORMAT "->>>>>>>>>>9.99%"
                  (curr-t[3] / t2) * 100 FORMAT "->>>>>>>>>>9.99%"
                  (curr-t[4] / t2) * 100 FORMAT "->>>>>>>>>>9.99%"
                  (curr-t[5] / t2) * 100 FORMAT "->>>>>>>>>>9.99%"

              WITH FRAME bot2 STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-ATTR-SPACE.

          IF tb_excel THEN
          DO:
            PUT STREAM excel UNFORMATTED
                SKIP(1).

            IF ll-mult-curr THEN
               PUT STREAM excel UNFORMATTED
                   '"' "" '",'.

            PUT STREAM excel UNFORMATTED
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' "CURR TOTALS"                       '",'
                '"' ""                                  '",'
                '"' STRING(curr-t[1],"$->>>,>>>,>>9.99") '",'
                '"' STRING(curr-t[2],"$->>>,>>>,>>9.99") '",'
                '"' STRING(curr-t[3],"$->>>,>>>,>>9.99") '",'
                '"' STRING(curr-t[4],"$->>>,>>>,>>9.99") '",'
                '"' STRING(curr-t[5],"$->>>,>>>,>>9.99") '",'
                '"' STRING(curr-t[6],"$->>>,>>>,>>9.99") '",'
                SKIP.

            IF ll-mult-curr THEN
               PUT STREAM excel UNFORMATTED
                   '"' "" '",'.

            PUT STREAM excel UNFORMATTED
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' "PERCENTAGE COMPOSITION"            '",'
                '"' ""                                  '",'
                '"' STRING((curr-t[1] / t2) * 100,"->>>>>>>>>>9.99%") '",'
                '"' STRING((curr-t[2] / t2) * 100,"->>>>>>>>>>9.99%") '",'
                '"' STRING((curr-t[3] / t2) * 100,"->>>>>>>>>>9.99%") '",'
                '"' STRING((curr-t[4] / t2) * 100,"->>>>>>>>>>9.99%") '",'
                '"' STRING((curr-t[5] / t2) * 100,"->>>>>>>>>>9.99%") '",'
                SKIP(1).
          END.
       END.

       DO i = 1 TO 5:
         grand-t[i] = grand-t[i] + curr-t[i].
       END.
       ASSIGN
        curr-t = 0
        t3     = t3 + t2
        t2     = 0.
    END.
  END.

  IF ll-mult-curr THEN DO:
    HIDE FRAME r-top-1 NO-PAUSE.
    HIDE FRAME r-top-2 NO-PAUSE.
    PAGE.
  END.

  PUT SKIP(2).

  ASSIGN
   lv-f-bot-hdr = "GRAND TOTALS"
   grand-t[6]   = 0.

  DO i = 1 TO 5:
     grand-t[6] = grand-t[6] + grand-t[i].
  END.

  VIEW FRAME f-bot.
  DOWN.

  DISPLAY SPACE(10) lv-f-bot-hdr
          grand-t[1]
          grand-t[2]
          grand-t[3]
          grand-t[4]
          grand-t[5]
          grand-t[6]
          skip

      WITH FRAME bot3 NO-BOX NO-LABELS NO-ATTR-SPACE STREAM-IO WIDTH 200.

  display "PERCENTAGE COMPOSITION" space(2)
          (grand-t[1] / t3) * 100 format "->>>>>>>>>>9.99%"
          (grand-t[2] / t3) * 100 format "->>>>>>>>>>9.99%"
          (grand-t[3] / t3) * 100 format "->>>>>>>>>>9.99%"
          (grand-t[4] / t3) * 100 format "->>>>>>>>>>9.99%"
          (grand-t[5] / t3) * 100 format "->>>>>>>>>>9.99%"

      with frame b4 stream-io width 200 no-labels no-box no-attr-space.

  IF tb_excel THEN
     DO:
       PUT STREAM excel UNFORMATTED
           SKIP(1).

       IF ll-mult-curr THEN
          PUT STREAM excel UNFORMATTED
              '"' "" '",'.

       PUT STREAM excel UNFORMATTED
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",' 
           '"' "GRAND TOTALS"                      '",'
           '"' ""                                  '",'
           '"' STRING(grand-t[1],"$->>>,>>>,>>9.99") '",'
           '"' STRING(grand-t[2],"$->>>,>>>,>>9.99") '",'
           '"' STRING(grand-t[3],"$->>>,>>>,>>9.99") '",'
           '"' STRING(grand-t[4],"$->>>,>>>,>>9.99") '",'
           '"' STRING(grand-t[5],"$->>>,>>>,>>9.99") '",'
           '"' STRING(grand-t[6],"$->>>,>>>,>>9.99") '",'
           SKIP.

       PUT STREAM excel UNFORMATTED
           SKIP(1).

       IF ll-mult-curr THEN
          PUT STREAM excel UNFORMATTED
              '"' "" '",'.

       PUT STREAM excel UNFORMATTED
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' "PERCENTAGE COMPOSITION"            '",'
           '"' ""                                  '",'
           '"' STRING((grand-t[1] / t3) * 100,"->>>>>>>>>>9.99%") '",'
           '"' STRING((grand-t[2] / t3) * 100,"->>>>>>>>>>9.99%") '",'
           '"' STRING((grand-t[3] / t3) * 100,"->>>>>>>>>>9.99%") '",'
           '"' STRING((grand-t[4] / t3) * 100,"->>>>>>>>>>9.99%") '",'
           '"' STRING((grand-t[5] / t3) * 100,"->>>>>>>>>>9.99%") '",'
           SKIP.
     END.

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
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

  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

