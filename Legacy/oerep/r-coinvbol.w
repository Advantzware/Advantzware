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
DEFINE INPUT PARAMETER ip-begin-bol AS INT NO-UNDO.
DEFINE INPUT PARAMETER ip-end-bol AS INT NO-UNDO.

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i} 

{sys/inc/var.i new shared}

def SHARED TEMP-TABLE w-comm-bol NO-UNDO field bol-no as INT INDEX bol-no bol-no.

assign
 cocode = gcompany
 locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-bol-list begin_bol#-1 begin_bol#-2 ~
begin_bol#-3 begin_bol#-4 begin_bol#-5 begin_bol#-6 begin_bol#-7 ~
begin_bol#-8 begin_bol#-9 begin_bol#-10 begin_bol#-11 begin_bol#-12 ~
begin_bol#-13 begin_bol#-14 begin_bol#-15 begin_bol#-16 begin_bol#-17 ~
begin_bol#-18 begin_bol#-19 begin_bol#-20 begin_bol#-21 begin_bol#-22 ~
begin_bol#-23 begin_bol#-24 btn-ok btn-cancel RECT-7 
&Scoped-Define DISPLAYED-OBJECTS v-bol-list begin_bol#-1 begin_bol#-2 ~
begin_bol#-3 begin_bol#-4 begin_bol#-5 begin_bol#-6 begin_bol#-7 ~
begin_bol#-8 begin_bol#-9 begin_bol#-10 begin_bol#-11 begin_bol#-12 ~
begin_bol#-13 begin_bol#-14 begin_bol#-15 begin_bol#-16 begin_bol#-17 ~
begin_bol#-18 begin_bol#-19 begin_bol#-20 begin_bol#-21 begin_bol#-22 ~
begin_bol#-23 begin_bol#-24 

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
     SIZE 49 BY 9.52 NO-UNDO.

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

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 90 BY 12.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     v-bol-list AT ROW 3.86 COL 2 NO-LABEL
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
     btn-ok AT ROW 14.1 COL 20
     btn-cancel AT ROW 14.1 COL 61
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
          SIZE 36 BY .62 AT ROW 2.76 COL 7
          BGCOLOR 14 
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.4 BY 15.29.


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
         TITLE              = "Select BOL Range"
         HEIGHT             = 15.29
         WIDTH              = 90.2
         MAX-HEIGHT         = 15.29
         MAX-WIDTH          = 90.4
         VIRTUAL-HEIGHT     = 15.29
         VIRTUAL-WIDTH      = 90.4
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
       v-bol-list:RETURN-INSERTED IN FRAME FRAME-A  = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Select BOL Range */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Select BOL Range */
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

  run select-bols.

  APPLY 'CLOSE':U TO THIS-PROCEDURE.
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

  ASSIGN
    begin_bol#-1 = ip-begin-bol
    begin_bol#-2 = ip-end-bol.

  RUN enable_UI.

  {methods/nowait.i}

  EMPTY TEMP-TABLE w-comm-bol.

  DO WITH FRAME {&FRAME-NAME}:

    APPLY "entry" TO begin_bol#-1.
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
  DISPLAY v-bol-list begin_bol#-1 begin_bol#-2 begin_bol#-3 begin_bol#-4 
          begin_bol#-5 begin_bol#-6 begin_bol#-7 begin_bol#-8 begin_bol#-9 
          begin_bol#-10 begin_bol#-11 begin_bol#-12 begin_bol#-13 begin_bol#-14 
          begin_bol#-15 begin_bol#-16 begin_bol#-17 begin_bol#-18 begin_bol#-19 
          begin_bol#-20 begin_bol#-21 begin_bol#-22 begin_bol#-23 begin_bol#-24 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE v-bol-list begin_bol#-1 begin_bol#-2 begin_bol#-3 begin_bol#-4 
         begin_bol#-5 begin_bol#-6 begin_bol#-7 begin_bol#-8 begin_bol#-9 
         begin_bol#-10 begin_bol#-11 begin_bol#-12 begin_bol#-13 begin_bol#-14 
         begin_bol#-15 begin_bol#-16 begin_bol#-17 begin_bol#-18 begin_bol#-19 
         begin_bol#-20 begin_bol#-21 begin_bol#-22 begin_bol#-23 begin_bol#-24 
         btn-ok btn-cancel RECT-7 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-bols C-Win 
PROCEDURE select-bols :
/* -------------------------------------------------- */
/* print bill of ladings                              */
/* -------------------------------------------------- */

DEF VAR li AS INT NO-UNDO.
DEF VAR v-s-bol AS INT EXTENT 24 NO-UNDO.
DEF VAR v-bol AS INT NO-UNDO.

assign
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
    ASC(SUBSTR(v-bol-list,LENGTH(v-bol-list),1)) EQ 13) THEN
   v-bol-list = SUBSTR(v-bol-list,1,LENGTH(v-bol-list) - 1).

  EMPTY TEMP-TABLE w-comm-bol.

  do li = 1 to 23 by 2:

    IF v-s-bol[li] NE 0 AND
       v-s-bol[li + 1] NE 0 THEN
       for each oe-bolh WHERE
           oe-bolh.company eq cocode AND
           oe-bolh.bol-no  ge v-s-bol[li] AND
           oe-bolh.bol-no  le v-s-bol[li + 1]
           no-lock:

           IF NOT CAN-FIND(FIRST w-comm-bol WHERE
              w-comm-bol.bol-no EQ oe-bolh.bol-no) THEN
              DO:
                create w-comm-bol.
                w-comm-bol.bol-no = oe-bolh.bol-no.
                RELEASE w-comm-bol.
              END.
       end.
  end.

  DO li = 1 TO NUM-ENTRIES(v-bol-list):
    v-bol = INT(ENTRY(li,v-bol-list)) NO-ERROR.
    if NOT ERROR-STATUS:ERROR then
    for each oe-bolh WHERE
        oe-bolh.company eq cocode AND
        oe-bolh.bol-no  eq v-bol
        no-lock:

        IF NOT CAN-FIND(FIRST w-comm-bol WHERE
           w-comm-bol.bol-no EQ oe-bolh.bol-no) THEN
           DO:
             create w-comm-bol.
             w-comm-bol.bol-no = oe-bolh.bol-no.
             RELEASE w-comm-bol.
           END.
    end.
  end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

