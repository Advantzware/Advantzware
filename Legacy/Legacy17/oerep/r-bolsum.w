&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

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

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.

{custom/xprint.i}

DEF VAR lv-prt-bypass AS LOG NO-UNDO.  /* bypass window's printer driver */

def TEMP-TABLE w-bol field rec-id as RECID INDEX rec-id rec-id.

def stream last-page.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_cust begin_ship v-bol-list ~
begin_bol#-1 begin_bol#-2 begin_bol#-3 begin_bol#-4 begin_bol#-5 ~
begin_bol#-6 begin_bol#-7 begin_bol#-8 begin_bol#-9 begin_bol#-10 ~
begin_bol#-11 begin_bol#-12 begin_bol#-13 begin_bol#-14 begin_bol#-15 ~
begin_bol#-16 begin_bol#-17 begin_bol#-18 begin_bol#-19 begin_bol#-20 ~
begin_bol#-21 begin_bol#-22 begin_bol#-23 begin_bol#-24 rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm btn-ok btn-cancel RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS begin_cust begin_ship v-bol-list ~
begin_bol#-1 begin_bol#-2 begin_bol#-3 begin_bol#-4 begin_bol#-5 ~
begin_bol#-6 begin_bol#-7 begin_bol#-8 begin_bol#-9 begin_bol#-10 ~
begin_bol#-11 begin_bol#-12 begin_bol#-13 begin_bol#-14 begin_bol#-15 ~
begin_bol#-16 begin_bol#-17 begin_bol#-18 begin_bol#-19 begin_bol#-20 ~
begin_bol#-21 begin_bol#-22 begin_bol#-23 begin_bol#-24 rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE v-bol-list AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 49 BY 6.91 NO-UNDO.

DEFINE VARIABLE begin_bol#-1 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-10 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-11 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-12 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-13 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-14 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-15 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-16 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-17 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-18 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95.

DEFINE VARIABLE begin_bol#-19 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-2 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-20 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-21 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-22 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-23 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-24 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-3 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-4 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-5 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-6 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-7 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-8 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_bol#-9 AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_ship AS CHARACTER FORMAT "X(8)":U 
     LABEL "Ship-To#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
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
     SIZE 23 BY 7.86 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 90 BY 9.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 90 BY 12.86.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust AT ROW 2.91 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     begin_ship AT ROW 3.86 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Ship-to#"
     v-bol-list AT ROW 6.48 COL 2 NO-LABEL
     begin_bol#-1 AT ROW 2.19 COL 51 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-2 AT ROW 2.19 COL 71 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-3 AT ROW 3.14 COL 51 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-4 AT ROW 3.14 COL 71 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-5 AT ROW 4.1 COL 51 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-6 AT ROW 4.1 COL 71 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-7 AT ROW 5.05 COL 51 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-8 AT ROW 5.05 COL 71 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-9 AT ROW 6 COL 51 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-10 AT ROW 6 COL 71 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-11 AT ROW 6.95 COL 51 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-12 AT ROW 6.95 COL 71 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-13 AT ROW 7.91 COL 51 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-14 AT ROW 7.91 COL 71 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-15 AT ROW 8.86 COL 51 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-16 AT ROW 8.86 COL 71 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-17 AT ROW 9.81 COL 51 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-18 AT ROW 9.81 COL 71 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-19 AT ROW 10.76 COL 51 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-20 AT ROW 10.76 COL 71 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-21 AT ROW 11.71 COL 51 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-22 AT ROW 11.71 COL 71 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-23 AT ROW 12.67 COL 51 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     begin_bol#-24 AT ROW 12.67 COL 71 COLON-ALIGNED HELP
          "Enter Beginning BOL Number" NO-LABEL
     rd-dest AT ROW 15.05 COL 3 NO-LABEL
     lv-ornt AT ROW 16.95 COL 28 NO-LABEL
     lines-per-page AT ROW 16.95 COL 81 COLON-ALIGNED
     lv-font-no AT ROW 18.38 COL 31 COLON-ALIGNED
     lv-font-name AT ROW 19.33 COL 25 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 21.71 COL 27
     btn-ok AT ROW 24.1 COL 20
     btn-cancel AT ROW 24.1 COL 61
     RECT-6 AT ROW 13.86 COL 1
     RECT-7 AT ROW 1 COL 1
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.1 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.4 BY 25.29.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-A
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     "Beginning BOL#" VIEW-AS TEXT
          SIZE 17 BY 1 AT ROW 1.24 COL 53
     "Ending BOL#" VIEW-AS TEXT
          SIZE 17 BY 1 AT ROW 1.24 COL 73
     "BOL# Ranges -->" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 1.24 COL 34
     " Enter BOL#s separated by comma" VIEW-AS TEXT
          SIZE 36 BY .62 AT ROW 5.76 COL 7
          BGCOLOR 14 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.4 BY 25.29.


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
         TITLE              = "Truckload BOL Summary"
         HEIGHT             = 25.29
         WIDTH              = 90.2
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
   Custom                                                               */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_bol#-1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-10:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-11:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-12:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-13:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-14:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-15:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-16:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-17:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-18:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-19:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-20:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-21:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-22:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-23:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-24:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-3:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-4:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-5:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-6:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-7:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-8:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_bol#-9:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ship:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       v-bol-list:RETURN-INSERTED IN FRAME FRAME-A  = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Truckload BOL Summary */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Truckload BOL Summary */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-1 C-Win
ON LEAVE OF begin_bol#-1 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-10 C-Win
ON LEAVE OF begin_bol#-10 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-11 C-Win
ON LEAVE OF begin_bol#-11 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-12 C-Win
ON LEAVE OF begin_bol#-12 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-13 C-Win
ON LEAVE OF begin_bol#-13 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-14 C-Win
ON LEAVE OF begin_bol#-14 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-15 C-Win
ON LEAVE OF begin_bol#-15 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-16 C-Win
ON LEAVE OF begin_bol#-16 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-17 C-Win
ON LEAVE OF begin_bol#-17 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-18 C-Win
ON LEAVE OF begin_bol#-18 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-19 C-Win
ON LEAVE OF begin_bol#-19 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-2 C-Win
ON LEAVE OF begin_bol#-2 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-20 C-Win
ON LEAVE OF begin_bol#-20 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-21 C-Win
ON LEAVE OF begin_bol#-21 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-22
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-22 C-Win
ON LEAVE OF begin_bol#-22 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-23 C-Win
ON LEAVE OF begin_bol#-23 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-24
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-24 C-Win
ON LEAVE OF begin_bol#-24 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-3 C-Win
ON LEAVE OF begin_bol#-3 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-4 C-Win
ON LEAVE OF begin_bol#-4 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-5 C-Win
ON LEAVE OF begin_bol#-5 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-6 C-Win
ON LEAVE OF begin_bol#-6 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-7 C-Win
ON LEAVE OF begin_bol#-7 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-8 C-Win
ON LEAVE OF begin_bol#-8 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol#-9 C-Win
ON LEAVE OF begin_bol#-9 IN FRAME FRAME-A
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Customer# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cust NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ship
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship C-Win
ON LEAVE OF begin_ship IN FRAME FRAME-A /* Ship-To# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-ship NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
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
  RUN valid-cust NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-ship NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  run run-report. 

  SESSION:SET-WAIT-STATE ("").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &TYPE="Customer"
                            &begin_cust=begin_cust
                            &end_cust=begin_cust
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").                            
              {custom/asimail.i &TYPE="Customer"
                             &begin_cust=begin_cust
                             &end_cust=begin_cust
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=lv-pdf-file + ".pdf" }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE="Customer"
                                  &begin_cust=begin_cust
                                  &end_cust=begin_cust
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
       END. 
       WHEN 6 THEN RUN output-to-port.
  end case. 
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO begin_cust.
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
  DISPLAY begin_cust begin_ship v-bol-list begin_bol#-1 begin_bol#-2 
          begin_bol#-3 begin_bol#-4 begin_bol#-5 begin_bol#-6 begin_bol#-7 
          begin_bol#-8 begin_bol#-9 begin_bol#-10 begin_bol#-11 begin_bol#-12 
          begin_bol#-13 begin_bol#-14 begin_bol#-15 begin_bol#-16 begin_bol#-17 
          begin_bol#-18 begin_bol#-19 begin_bol#-20 begin_bol#-21 begin_bol#-22 
          begin_bol#-23 begin_bol#-24 rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_cust begin_ship v-bol-list begin_bol#-1 begin_bol#-2 
         begin_bol#-3 begin_bol#-4 begin_bol#-5 begin_bol#-6 begin_bol#-7 
         begin_bol#-8 begin_bol#-9 begin_bol#-10 begin_bol#-11 begin_bol#-12 
         begin_bol#-13 begin_bol#-14 begin_bol#-15 begin_bol#-16 begin_bol#-17 
         begin_bol#-18 begin_bol#-19 begin_bol#-20 begin_bol#-21 begin_bol#-22 
         begin_bol#-23 begin_bol#-24 rd-dest lv-ornt lines-per-page lv-font-no 
         td-show-parm btn-ok btn-cancel RECT-6 RECT-7 
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
 /*   DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY.
   */
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
  RUN custom/d-print.w(list-name).

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
 IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE IF lv-prt-bypass THEN RUN custom/d-print.w (list-name).
  ELSE RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).

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
 IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE /*run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ */
       run custom/scr-rpt2.w (list-name,c-win:title,int(lv-font-no),lv-ornt,lv-prt-bypass).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* --------------------------------------------- oe/rep/oe-lad.p 3/94 RM ---- */
/* print bill of ladings                                                      */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.i}

def var v-cust   like cust.cust-no.
def var v-ship   like shipto.ship-id.
def var v-s-bol  as   int format ">>>>>>" init 0 extent 24 no-undo.
def var v-bol    like oe-bolh.bol-no no-undo.

def var v-carrier       like carrier.dscr no-undo.
def var v-to-ship       like oe-boll.qty no-undo.
def var v-tot-pkgs      as   int format ">>>>>9" no-undo.
def var v-tot-pals      like v-tot-pkgs.
def var v-tot-wght      like v-tot-pkgs.
def var v-lines         as   int no-undo.
def var v-page-tot      as   int init 0 no-undo.
def var v-last          as   log no-undo.
def var v-bol-list      as   char format "x(75)".

DEF VAR li AS INT NO-UNDO.

form oe-boll.i-no   format "x(15)"      at 9
     itemfg.i-name  format "x(30)"      at 26
     v-to-ship      format ">>>,>>>"    to 66
     "_______"                          at 71 skip

    with frame ln-s down no-box no-labels STREAM-IO width 90.


assign
 v-cust      = begin_cust
 v-ship      = begin_ship
 v-s-bol[01] = begin_bol#-1
 v-s-bol[02] = begin_bol#-2
 v-s-bol[03] = begin_bol#-3
 v-s-bol[04] = begin_bol#-4
 v-s-bol[05] = begin_bol#-5
 v-s-bol[06] = begin_bol#-6
 v-s-bol[07] = begin_bol#-7
 v-s-bol[08] = begin_bol#-8
 v-s-bol[09] = begin_bol#-9
 v-s-bol[10] = begin_bol#-10
 v-s-bol[11] = begin_bol#-11
 v-s-bol[12] = begin_bol#-12
 v-s-bol[13] = begin_bol#-13
 v-s-bol[14] = begin_bol#-14
 v-s-bol[15] = begin_bol#-15
 v-s-bol[16] = begin_bol#-16
 v-s-bol[17] = begin_bol#-17
 v-s-bol[18] = begin_bol#-18
 v-s-bol[19] = begin_bol#-19
 v-s-bol[20] = begin_bol#-20
 v-s-bol[21] = begin_bol#-21
 v-s-bol[22] = begin_bol#-22
 v-s-bol[23] = begin_bol#-23
 v-s-bol[24] = begin_bol#-24.

IF v-bol-list NE "" AND 
   (ASC(SUBSTR(v-bol-list,LENGTH(v-bol-list),1)) EQ 10 OR
    ASC(SUBSTR(v-bol-list,LENGTH(v-bol-list),1)) EQ 13)
THEN v-bol-list = SUBSTR(v-bol-list,1,LENGTH(v-bol-list) - 1).

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

  FOR EACH w-bol:
    DELETE w-bol.
  END.

  output stream last-page to value(tmp-dir + "\bolsumm.txt") page-size 55.

  do li = 1 to 23 by 2:
    for each oe-bolh
        where oe-bolh.company eq cocode
          and oe-bolh.cust-no eq v-cust
          and oe-bolh.ship-id eq v-ship
          and oe-bolh.bol-no  ge v-s-bol[li]
          and oe-bolh.bol-no  le v-s-bol[li + 1]
          and oe-bolh.printed eq yes
          and oe-bolh.posted  eq no
        use-index cust no-lock:
      create w-bol.
      w-bol.rec-id = recid(oe-bolh).
    end.
  end.

  DO li = 1 TO NUM-ENTRIES(v-bol-list):
    v-bol = INT(ENTRY(li,v-bol-list)) NO-ERROR.
    if NOT ERROR-STATUS:ERROR then
    for each oe-bolh
        where oe-bolh.company eq cocode
          and oe-bolh.cust-no eq v-cust
          and oe-bolh.ship-id eq v-ship
          and oe-bolh.bol-no  eq v-bol
          and oe-bolh.printed eq yes
          and oe-bolh.posted  eq no
        use-index cust no-lock:
      create w-bol.
      w-bol.rec-id = recid(oe-bolh).
    end.
  end.

  i = 1.

  for each w-bol,

      first oe-bolh
      where recid(oe-bolh) eq w-bol.rec-id
      no-lock

      break by w-bol.rec-id:

    if last-of(w-bol.rec-id) then do:
      if oe-bolh.tot-pallets ne ? then
        v-tot-pals = v-tot-pals + oe-bolh.tot-pallets.
      if oe-bolh.tot-wt ne ? then
        v-tot-wght = v-tot-wght + oe-bolh.tot-wt.
    end.

    else delete w-bol.
  end.

  find first cust
      where cust.company eq cocode
        and cust.cust-no eq v-cust
      no-lock no-error.

  find first shipto
      where shipto.company eq cocode
        and shipto.cust-no eq v-cust
        and shipto.ship-id eq v-ship
      use-index ship-id no-lock no-error.

  format header
         skip(5)
         "SHIP TO:" at 41 shipto.ship-id to 70
         shipto.ship-name  at 46
         shipto.ship-addr[1] at 46
         shipto.ship-addr[2] at 46
         shipto.ship-city at 46
         shipto.ship-state
         shipto.ship-zip
         skip(1)
         "PAGE" at 63 page-number to 69 format "99"
         "OF" at 71 v-page-tot to 75 format "99" skip
         fill("-",80) format "x(80)" at 1 skip
         "ITEM NUMBER" at 9 "PRODUCT DESCRIPTION" at 26
         "QUANTITY IN CARTONS" at 59 skip
         "---------------" at 9
         "------------------------------" at 26
         "---------------------" at 58 skip
         "TO SHIP" at 59 "SHIPPED" at 71 skip
         "----------" at 58 "---------" at 70 skip

      with frame head no-box no-labels page-top STREAM-IO width 80.

  view frame head.
  view stream last-page frame head.  /* Print headers */

  page stream last-page.

  {oe/rep/bolsumm.i "stream last-page"}

  v-page-tot = page-number (last-page).

  assign
   v-lines    = 0
   v-to-ship  = 0
   v-tot-pkgs = 0.

  page.

  {oe/rep/bolsumm.i}

  output stream last-page close.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust C-Win 
PROCEDURE valid-cust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST cust
                        WHERE cust.company EQ cocode
                          AND cust.cust-no EQ begin_cust:SCREEN-VALUE) THEN DO:
      MESSAGE TRIM(begin_cust:LABEL) + " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO begin_cust.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ship C-Win 
PROCEDURE valid-ship :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST shipto
                        WHERE shipto.company EQ cocode
                          AND shipto.cust-no EQ begin_cust:SCREEN-VALUE
                          AND shipto.ship-id EQ begin_ship:SCREEN-VALUE) THEN DO:
      MESSAGE TRIM(begin_ship:LABEL) + " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO begin_ship.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

