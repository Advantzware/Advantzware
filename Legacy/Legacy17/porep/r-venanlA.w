&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: porep\r-venanl.w

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

/* Working vars */
def var v-cost  as dec no-undo.
def var v-msf   as dec no-undo.
def var v-type  as char format "!" init "B".
def var v-mattype-list          as   char format "x(36)".
def var v-mat-dscr              as   char format "x(20)" extent 22.
def var b-date# like po-ord.po-date format "99/99/9999" init "01/01/0001".
def var p-date# like b-date# init today.
def var e-date# like b-date# init today.
def var ctr as int init 0.

def var v-wid       like po-ordl.s-wid.
def var v-len       like po-ordl.s-len.
def var v-dep       like item.s-dep.
def var v-bwt       like item.basis-w.
def var v-qty       as   dec.

DEF VAR v-print-fmt AS CHARACTER.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_due-date begin_vend-no ~
end_vend-no begin_prod-cat end_prod-cat begin_flute end_flute begin_cal ~
end_cal begin_acct end_acct tb_det rd_fg-rm select-mat rd_item-code lv-ornt ~
lines-per-page rd-dest lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_due-date begin_vend-no end_vend-no ~
begin_prod-cat end_prod-cat begin_flute end_flute begin_cal end_cal ~
begin_acct end_acct tb_det lbl_fg-rm rd_fg-rm select-mat lbl_fg-rm-2 ~
rd_item-code lv-ornt lines-per-page rd-dest lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

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

DEFINE VARIABLE begin_acct AS CHARACTER FORMAT "X(25)":U 
     LABEL "Beginning Acct#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cal AS DECIMAL FORMAT "9.99999":U INITIAL 0 
     LABEL "Beginning Caliper" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_due-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Due Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_flute AS CHARACTER FORMAT "X(15)" 
     LABEL "Beginning Flute" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE begin_prod-cat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Product Cat." 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct AS CHARACTER FORMAT "X(25)":U INITIAL "zzzzzzzzzzzzzzzzzzzzzzzzz" 
     LABEL "Ending Acct#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_cal AS DECIMAL FORMAT "9.99999":U INITIAL 9.99999 
     LABEL "Ending Caliper" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_flute AS CHARACTER FORMAT "X(15)" INITIAL "zzz" 
     LABEL "Ending Flute" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE end_prod-cat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Product Cat." 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend-no AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-venanl.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_fg-rm AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .91 NO-UNDO.

DEFINE VARIABLE lbl_fg-rm-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .91 NO-UNDO.

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
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_fg-rm AS CHARACTER INITIAL "Both" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "FG", "FG",
"RM", "RM",
"Both", "Both"
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE rd_item-code AS CHARACTER INITIAL "Both" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Real", "Real",
"Estimated", "Estimated",
"Both", "Both"
     SIZE 30 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 14.52.

DEFINE VARIABLE select-mat AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 40 BY 4.52 NO-UNDO.

DEFINE VARIABLE tb_det AS LOGICAL INITIAL yes 
     LABEL "Detail?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

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
     begin_due-date AT ROW 1.95 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Due Date"
     begin_vend-no AT ROW 3.38 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend-no AT ROW 3.38 COL 69 COLON-ALIGNED HELP
          "Enter Ending Vendor number"
     begin_prod-cat AT ROW 4.33 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Product Category"
     end_prod-cat AT ROW 4.33 COL 69 COLON-ALIGNED HELP
          "Enter Ending Product Category"
     begin_flute AT ROW 5.29 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Flute"
     end_flute AT ROW 5.29 COL 69 COLON-ALIGNED HELP
          "Enter Ending Flute"
     begin_cal AT ROW 6.24 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Caliper"
     end_cal AT ROW 6.24 COL 69 COLON-ALIGNED HELP
          "Enter Ending Caliper"
     begin_acct AT ROW 7.43 COL 20 COLON-ALIGNED HELP
          "Enter Beginning Account Number"
     end_acct AT ROW 7.43 COL 64 COLON-ALIGNED HELP
          "Enter Ending Account Number"
     tb_det AT ROW 9.29 COL 16
     lbl_fg-rm AT ROW 10.48 COL 6 COLON-ALIGNED NO-LABEL
     rd_fg-rm AT ROW 10.48 COL 16 NO-LABEL
     select-mat AT ROW 10.76 COL 50 HELP
          "Enter description of this Material Type." NO-LABEL
     lbl_fg-rm-2 AT ROW 11.71 COL 6 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     rd_item-code AT ROW 11.71 COL 16 NO-LABEL WIDGET-ID 4
     mat-types AT ROW 13.86 COL 44 COLON-ALIGNED
     lv-ornt AT ROW 15.95 COL 31 NO-LABEL
     lines-per-page AT ROW 15.95 COL 84 COLON-ALIGNED
     rd-dest AT ROW 16.48 COL 6 NO-LABEL
     lv-font-no AT ROW 17.14 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 18.1 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19.52 COL 31
     tb_excel AT ROW 20.76 COL 68.2 RIGHT-ALIGNED
     tb_runExcel AT ROW 20.76 COL 89.2 RIGHT-ALIGNED
     fi_file AT ROW 21.57 COL 46.2 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 23.62 COL 27
     btn-cancel AT ROW 23.62 COL 57
     "Select/Deselect Material Types" VIEW-AS TEXT
          SIZE 38 BY 1 AT ROW 9.81 COL 51
          FONT 6
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.76 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1 COL 5
     RECT-6 AT ROW 15.52 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.2 BY 24.14.


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
         TITLE              = "Vendor Analysis"
         HEIGHT             = 24.24
         WIDTH              = 95.6
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
       begin_acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cal:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_flute:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_prod-cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_acct:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cal:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_flute:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_prod-cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_fg-rm IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_fg-rm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_fg-rm".

/* SETTINGS FOR FILL-IN lbl_fg-rm-2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_fg-rm-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_fg-rm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mat-types IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       mat-types:HIDDEN IN FRAME FRAME-A           = TRUE
       mat-types:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_fg-rm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_item-code:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_det:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Vendor Analysis */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vendor Analysis */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_acct C-Win
ON LEAVE OF begin_acct IN FRAME FRAME-A /* Beginning Acct# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cal C-Win
ON LEAVE OF begin_cal IN FRAME FRAME-A /* Beginning Caliper */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_due-date C-Win
ON LEAVE OF begin_due-date IN FRAME FRAME-A /* Beginning Due Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_flute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_flute C-Win
ON LEAVE OF begin_flute IN FRAME FRAME-A /* Beginning Flute */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_prod-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_prod-cat C-Win
ON LEAVE OF begin_prod-cat IN FRAME FRAME-A /* Beginning Product Cat. */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-no C-Win
ON LEAVE OF begin_vend-no IN FRAME FRAME-A /* Beginning Vendor# */
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

run run-report(OUTPUT v-valid). 
STATUS DEFAULT "Processing Complete".
IF v-valid THEN
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_vend-no
                            &END_cust=END_vend-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "Vendor"
                             &begin_cust= begin_vend-no
                             &END_cust=end_vend-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust= begin_vend-no
                                  &END_cust=end_vend-no
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
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


&Scoped-define SELF-NAME end_acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_acct C-Win
ON LEAVE OF end_acct IN FRAME FRAME-A /* Ending Acct# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cal C-Win
ON LEAVE OF end_cal IN FRAME FRAME-A /* Ending Caliper */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_flute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_flute C-Win
ON LEAVE OF end_flute IN FRAME FRAME-A /* Ending Flute */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_prod-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_prod-cat C-Win
ON LEAVE OF end_prod-cat IN FRAME FRAME-A /* Ending Product Cat. */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend-no C-Win
ON LEAVE OF end_vend-no IN FRAME FRAME-A /* Ending Vendor# */
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


&Scoped-define SELF-NAME rd_fg-rm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_fg-rm C-Win
ON VALUE-CHANGED OF rd_fg-rm IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_item-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_item-code C-Win
ON VALUE-CHANGED OF rd_item-code IN FRAME FRAME-A
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME tb_det
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_det C-Win
ON VALUE-CHANGED OF tb_det IN FRAME FRAME-A /* Detail? */
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
DEF VAR v-mat-list AS CHAR NO-UNDO.
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
    begin_due-date = today.

  RUN enable_UI.

 for each mat:
    v-mat-list = v-mat-list + string(mat.mat,"x(5)") + " " + mat.dscr + ",".
  end.
  if substr(v-mat-list,length(trim(v-mat-list)),1) eq "," then
    substr(v-mat-list,length(trim(v-mat-list)),1) = "".

  select-mat:list-items = v-mat-list.

  {methods/nowait.i}

APPLY "entry" TO begin_due-date IN FRAME {&FRAME-NAME}.
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images1.p */
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
  DISPLAY begin_due-date begin_vend-no end_vend-no begin_prod-cat end_prod-cat 
          begin_flute end_flute begin_cal end_cal begin_acct end_acct tb_det 
          lbl_fg-rm rd_fg-rm select-mat lbl_fg-rm-2 rd_item-code lv-ornt 
          lines-per-page rd-dest lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_due-date begin_vend-no end_vend-no begin_prod-cat 
         end_prod-cat begin_flute end_flute begin_cal end_cal begin_acct 
         end_acct tb_det rd_fg-rm select-mat rd_item-code lv-ornt 
         lines-per-page rd-dest lv-font-no td-show-parm tb_excel tb_runExcel 
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

     IF NOT OKpressed THEN  RETURN NO-APPLY. */

{custom/out2file.i}.

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
RUN csutom/d-print.w (list-name).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
 /*    DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
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
/* ---------------------------------------------- rm/rep/rm-rhist.p 06/00 DJK */
/* raw materials - transactions edit list                                     */
/* -------------------------------------------------------------------------- */

DEFINE OUTPUT PARAMETER op-valid AS LOG INIT TRUE NO-UNDO.

{sys/form/r-topw.f}

def var b-vendor# like item.vend-no no-undo.
def var e-vendor# like item.vend-no no-undo init "zzzzzzzzzz".
def var b-itemcode# like item.i-code no-undo.
def var e-itemcode# like item.i-code no-undo init "zzzzzzzzzz".
def var b-category# like item.procat no-undo.
def var e-category# like item.procat no-undo init "zzzzzzzzzz".
def var b-flute# like item.flute no-undo.
def var e-flute# like item.flute no-undo init "zzzzzzzzzz".
def var b-caliper# like item.cal no-undo.
def var e-caliper# like item.cal no-undo init "9.99999".
def var starting-period# as int format "99" no-undo init 1.
def var starting-year# as int format "9999" no-undo.

/* Form vars */
def var vendor# like item.vend-no no-undo.
def var name# like vend.name no-undo.
def var item# as character no-undo.
def var mtd-msf# as dec format "->>>,>>>,>>9.99" extent 2 no-undo.
def var mtd-cost# as dec format "->>>,>>>,>>9.99" extent 2 no-undo.
def var ytd-msf# as dec format "->>>,>>>,>>9.99" extent 2 no-undo.
def var ytd-cost# as dec format "->>>,>>>,>>9.99" extent 2 no-undo.

def var current-date# as date format "99/99/9999" no-undo.
def var current-time# as character no-undo.
def var v-date like b-date#.

def var detail-flag# as logical no-undo format "Detail/Summary" init yes.
DEF VAR excelheader AS CHAR NO-UNDO.

FORM header
  SKIP(1)
  "Vendor:" at 1 vendor# name#
  fill("-",132) format "x(132)"
  "PTD Range:" p-date# space(0) "-" space(0) e-date#
  space(3)
  "YTD Range:" b-date# space(0) "-" space(0) v-date
  fill("-",132) format "x(132)"
with frame f-header down no-box STREAM-IO width 132 PAGE-TOP.

form
  item# column-label "Item or Vendor" format "x(25)"
  mtd-msf#[1] column-label "PTD!MSF"
  mtd-cost#[1] column-label "PTD!Cost"
  ytd-msf#[1] column-label "YTD!MSF"
  ytd-cost#[1] column-label "YTD!Cost"
with frame f-detail down no-box STREAM-IO width 132.


SESSION:SET-WAIT-STATE ("general").

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 p-date#       = begin_due-date
 b-vendor#     = begin_vend-no
 e-vendor#     = end_vend-no
/* b-itemcode#   = begin_i-code
 e-itemcode#   = end_i-code*/
 b-category#   = begin_prod-cat
 e-category#   = end_prod-cat
 b-flute#      = begin_flute
 e-flute#      = end_flute
 b-caliper#    = begin_cal
 e-caliper#    = end_cal
 detail-flag#  = tb_det
 v-type        = substr(rd_fg-rm,1,1)
 b-itemcode#   = SUBSTRING(rd_item-code,1,1)
 v-mattype-list = "".

do with frame {&frame-name}:          
  do i = 1 to select-mat:num-items:
    if select-mat:is-selected(i) then
      v-mattype-list = v-mattype-list + trim(substr(select-mat:entry(i),1,5)) + ",".
  end.

  if length(TRIM(v-mattype-list)) EQ 0 THEN
  DO:
     MESSAGE "No Material Type Selected."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     op-valid = FALSE.
     LEAVE.
  END.

  IF substr(v-mattype-list,length(trim(v-mattype-list)),1) eq "," then
    substr(v-mattype-list,length(trim(v-mattype-list)),1) = "".

  mat-types = v-mattype-list.
  do i = 1 to length(mat-types):
    if substr(mat-types,i,1) eq "," then substr(mat-types,i,1) = " ".
  end.
  display mat-types.
end.

DO WITH FRAME f-detail:
  item#:LABEL = IF detail-flag# THEN "Item#" ELSE "Vendor#".
END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Vendor#,".
  IF detail-flag# THEN
     excelheader = excelheader + "Item#,".
  excelheader = excelheader + "PTD MSF,PTD Cost,YTD MSF,YTD Cost".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

  ctr = 0.

  find first period
      where period.company eq cocode
        and period.pst     le p-date#
        and period.pend    ge p-date#
      no-lock.

  assign
   e-date#          = p-date#
   v-date           = e-date#
   p-date#          = period.pst
   starting-period# = period.pnum
   starting-year#   = period.yr.

  find first period
      where period.company eq cocode
        and period.yr      eq starting-year#
      no-lock.

  b-date# = period.pst.

  do with frame f-detail:
    if v-type eq "R" or v-type eq "B" then
    {po/rep/rmanalys.i "item" item.flute}

    if v-type eq "F" or v-type eq "B" then
    {po/rep/rmanalys.i "itemfg" b-flute#}
  end.        

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

