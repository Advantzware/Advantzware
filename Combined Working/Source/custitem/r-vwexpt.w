&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-rmte&p.w

  Description: RM Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 05/23/2002 

  Modified By : Aj 06/23/2008  Added  code to generate E-mails for 
                               receipts overrun and under run quantity.

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

/* Parameters Definitions --- */

/* Local Variable Definitions ---                                       */
DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

ASSIGN
   cocode = gcompany
   locode = gloc.

DEF VAR ll-valid AS LOG NO-UNDO.

DEF TEMP-TABLE tt-vend-whse-item LIKE vend-whse-item
   FIELD tt-row-id AS ROWID
   FIELD row-id    AS ROWID
   FIELD has-rec   AS LOG INIT NO
   FIELD seq-no    AS INT
   INDEX seq-no seq-no.

DEF TEMP-TABLE tt-email NO-UNDO
   FIELD cust-part-no LIKE vend-whse-trans.cust-part-no 
   FIELD fg-item-no   LIKE vend-whse-trans.fg-item-no 
   FIELD plant-tot-oh-qty LIKE vend-whse-trans.plant-tot-oh-qty 
   FIELD trans-date       LIKE vend-whse-trans.trans-date 
   FIELD trans-qty        LIKE vend-whse-trans.trans-qty 
   FIELD vendor-dept-code LIKE vend-whse-trans.vendor-dept-code 
   FIELD vendor-code      LIKE vend-whse-trans.vendor-code 
   FIELD vend-plant-code  LIKE vend-whse-trans.vendor-plant-code.

DEF VAR v-pr-tots AS LOG FORMAT "Y/N" NO-UNDO.
DEF {1} SHARED VAR v-print-fmt AS CHAR NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO. 

DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
DEFINE STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-F

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-18 RECT-6 RECT-19 RECT-20 ~
FI-beg-vend-code FI-end-vend-code FI-beg-vend-plant-code ~
FI-end-vend-plant-code FI-beg-fg-item-no FI-end-fg-item-no ~
FI-beg-cust-part-no FI-end-cust-part-no lv-ornt lines-per-page rd-dest ~
lv-font-no td-show-parm tb_excel tb_runExcel fi_file Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS FI-beg-vend-code FI-end-vend-code ~
FI-beg-vend-plant-code FI-end-vend-plant-code FI-beg-fg-item-no ~
FI-end-fg-item-no FI-beg-cust-part-no FI-end-cust-part-no lv-ornt ~
lines-per-page rd-dest lv-font-no td-show-parm lv-font-name tb_excel ~
tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-beg-cust-part-no AS CHARACTER FORMAT "X(12)":U 
     LABEL "Customers Part No" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-beg-fg-item-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Suppliers FG Item" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-beg-vend-code AS CHARACTER FORMAT "X(8)":U 
     LABEL "Vendor Code" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-beg-vend-plant-code AS CHARACTER FORMAT "X(8)":U 
     LABEL "Vendor Plant Code" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-end-cust-part-no AS CHARACTER FORMAT "X(12)":U INITIAL "zzzzzzzzzzzz" 
     LABEL "Customers Part No" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-end-fg-item-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Suppliers FG Item" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-end-vend-code AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Vendor Code" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-end-vend-plant-code AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Vendor Plant Code" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-vwitmp.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 55 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1 NO-UNDO.

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
"To Port Directly", 6
     SIZE 20 BY 4.52 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 12.14.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 5.91.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 5.91.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 8.81.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
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

DEFINE FRAME FRAME-F
     FI-beg-vend-code AT ROW 4.76 COL 24 COLON-ALIGNED WIDGET-ID 2
     FI-end-vend-code AT ROW 4.76 COL 65.6 COLON-ALIGNED WIDGET-ID 14
     FI-beg-vend-plant-code AT ROW 5.71 COL 24 COLON-ALIGNED WIDGET-ID 6
     FI-end-vend-plant-code AT ROW 5.71 COL 65.6 COLON-ALIGNED WIDGET-ID 16
     FI-beg-fg-item-no AT ROW 6.67 COL 24 COLON-ALIGNED WIDGET-ID 30
     FI-end-fg-item-no AT ROW 6.67 COL 65.6 COLON-ALIGNED WIDGET-ID 32
     FI-beg-cust-part-no AT ROW 7.71 COL 24 COLON-ALIGNED WIDGET-ID 34
     FI-end-cust-part-no AT ROW 7.71 COL 65.6 COLON-ALIGNED WIDGET-ID 36
     lv-ornt AT ROW 13.86 COL 34 NO-LABEL
     lines-per-page AT ROW 13.86 COL 85 COLON-ALIGNED
     rd-dest AT ROW 14.1 COL 6 NO-LABEL
     lv-font-no AT ROW 16.24 COL 34.2 COLON-ALIGNED
     td-show-parm AT ROW 16.38 COL 51
     lv-font-name AT ROW 17.33 COL 34 COLON-ALIGNED NO-LABEL
     tb_excel AT ROW 19.29 COL 31.4
     tb_runExcel AT ROW 19.29 COL 81 RIGHT-ALIGNED
     fi_file AT ROW 20.52 COL 29.4 COLON-ALIGNED HELP
          "Enter File Name"
     Btn_OK AT ROW 21.95 COL 20
     Btn_Cancel AT ROW 21.95 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 24 BY .95 AT ROW 1.48 COL 3
          BGCOLOR 3 
     "Ending:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 4 COL 48.2 WIDGET-ID 28
     "Output Destination" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 13.38 COL 2
     "Beginning:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 4.05 COL 5.2 WIDGET-ID 26
     RECT-18 AT ROW 1 COL 1
     RECT-6 AT ROW 13.05 COL 1.2
     RECT-19 AT ROW 4.38 COL 4.2 WIDGET-ID 22
     RECT-20 AT ROW 4.38 COL 46.8 WIDGET-ID 24
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92.8 BY 22.43.


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
         TITLE              = "Exceptions Report"
         HEIGHT             = 22.43
         WIDTH              = 92.8
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
/* SETTINGS FOR FRAME FRAME-F
   FRAME-NAME                                                           */
ASSIGN
       Btn_Cancel:PRIVATE-DATA IN FRAME FRAME-F     = 
                "ribbon-button".


ASSIGN
       Btn_OK:PRIVATE-DATA IN FRAME FRAME-F     = 
                "ribbon-button".


ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-F
   NO-ENABLE                                                            */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-F
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Exceptions Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Exceptions Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME FRAME-F /* Cancel */
DO:
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME FRAME-F /* OK */
DO: 
   DEF VAR lv-post AS LOG NO-UNDO.
   DEF VAR lv-r-no LIKE rm-rctd.r-no NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN {&DISPLAYED-OBJECTS}.    
   END.

   FOR EACH tt-vend-whse-item:
      DELETE tt-vend-whse-item.
   END.

   RUN run-report. 

   CASE rd-dest:
      WHEN 1 THEN RUN output-to-printer.
      WHEN 2 THEN RUN output-to-screen.
      WHEN 3 THEN RUN output-to-file.
/*        when 4 then do:                               */
/*            /*run output-to-fax.*/                    */
/*            {custom/asifax.i &type=''                 */
/*                             &begin_cust=''           */
/*                             &END_cust=''             */
/*                             &fax-subject=c-win:TITLE */
/*                             &fax-body=c-win:TITLE    */
/*                             &fax-file=list-name }    */
/*        END.                                          */
/*        when 5 then do:                                      */
/*            IF is-xprint-form THEN DO:                       */
/*               {custom/asimail.i &TYPE = ''                  */
/*                              &begin_cust= ''                */
/*                              &END_cust=''                   */
/*                              &mail-subject=c-win:TITLE      */
/*                              &mail-body=c-win:TITLE         */
/*                              &mail-file=list-name }         */
/*            END.                                             */
/*            ELSE DO:                                         */
/*                {custom/asimailr.i &TYPE = ''                */
/*                                   &begin_cust= ''           */
/*                                   &END_cust= ''             */
/*                                   &mail-subject=c-win:TITLE */
/*                                   &mail-body=c-win:TITLE    */
/*                                   &mail-file=list-name }    */

/*            END. */
/*        END.     */
      WHEN 6 THEN RUN output-to-port.
   END CASE.


   RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-beg-cust-part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-beg-cust-part-no C-Win
ON LEAVE OF FI-beg-cust-part-no IN FRAME FRAME-F /* Customers Part No */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-beg-fg-item-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-beg-fg-item-no C-Win
ON LEAVE OF FI-beg-fg-item-no IN FRAME FRAME-F /* Suppliers FG Item */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-beg-vend-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-beg-vend-code C-Win
ON LEAVE OF FI-beg-vend-code IN FRAME FRAME-F /* Vendor Code */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-beg-vend-plant-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-beg-vend-plant-code C-Win
ON LEAVE OF FI-beg-vend-plant-code IN FRAME FRAME-F /* Vendor Plant Code */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-end-cust-part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-end-cust-part-no C-Win
ON LEAVE OF FI-end-cust-part-no IN FRAME FRAME-F /* Customers Part No */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-end-fg-item-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-end-fg-item-no C-Win
ON LEAVE OF FI-end-fg-item-no IN FRAME FRAME-F /* Suppliers FG Item */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-end-vend-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-end-vend-code C-Win
ON LEAVE OF FI-end-vend-code IN FRAME FRAME-F /* Vendor Code */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-end-vend-plant-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-end-vend-plant-code C-Win
ON LEAVE OF FI-end-vend-plant-code IN FRAME FRAME-F /* Vendor Plant Code */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-F /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-F /* Lines Per Page */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-F /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON LEAVE OF lv-font-no IN FRAME FRAME-F /* Font */
DO:
   ASSIGN lv-font-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON LEAVE OF lv-ornt IN FRAME FRAME-F
DO:
  ASSIGN lv-ornt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON VALUE-CHANGED OF lv-ornt IN FRAME FRAME-F
DO:
  {custom/chgfont.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-F
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-F /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-F /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-F /* Show Parameters? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* DEF INPUT PARAM mailTo AS CHAR.            */
/*       DEF INPUT PARAM mailsubject AS CHAR. */
/*       DEF INPUT PARAM mailText AS CHAR.    */
/*       DEF INPUT PARAM mailFiles AS CHAR.   */
/*       DEF INPUT PARAM mailDialog AS LONG.  */
/*       DEF OUTPUT PARAM retCode AS LONG.    */
/* END.                                       */


/* ***************************  Main Block  *************************** */    
DEF VAR choice AS LOG NO-UNDO.
DEF VAR ll-auto AS LOG NO-UNDO.

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
{sys/inc/f3helpw.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

/*   security check need {methods/prgsecur.i} in definition section */

  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

   DO TRANSACTION:  
      {sys/inc/rmemails.i}
   END.

  ASSIGN
   c-win:TITLE = "Usage Post".

  RUN enable_UI.

/*   RUN check-date. */

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}


    APPLY "entry" TO FI-beg-vend-code.
  END.
  {methods/nowait.i}

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date C-Win 
PROCEDURE check-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
/*    ll-valid = YES.                                                         */
/*                                                                            */
/*    FIND FIRST period WHERE period.company = cocode                         */
/*                        AND period.pst     LE v-post-date                   */
/*                        AND period.pend    GE v-post-date NO-LOCK NO-ERROR. */
/*    IF AVAIL period THEN                                                    */
/*       tran-period:SCREEN-VALUE = STRING(period.pnum).                      */
/*    ELSE                                                                    */
/*       IF ip-post THEN DO:                                                  */
/*          MESSAGE "No Defined Period Exists for" v-post-date                */
/*             VIEW-AS ALERT-BOX ERROR.                                       */
/*          ll-valid = NO.                                                    */
/*       END.                                                                 */
END.
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
  DISPLAY FI-beg-vend-code FI-end-vend-code FI-beg-vend-plant-code 
          FI-end-vend-plant-code FI-beg-fg-item-no FI-end-fg-item-no 
          FI-beg-cust-part-no FI-end-cust-part-no lv-ornt lines-per-page rd-dest 
          lv-font-no td-show-parm lv-font-name tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-F IN WINDOW C-Win.
  ENABLE RECT-18 RECT-6 RECT-19 RECT-20 FI-beg-vend-code FI-end-vend-code 
         FI-beg-vend-plant-code FI-end-vend-plant-code FI-beg-fg-item-no 
         FI-end-fg-item-no FI-beg-cust-part-no FI-end-cust-part-no lv-ornt 
         lines-per-page rd-dest lv-font-no td-show-parm tb_excel tb_runExcel 
         fi_file Btn_OK Btn_Cancel 
      WITH FRAME FRAME-F IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-F}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ouput-to-port C-Win 
PROCEDURE ouput-to-port :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN custom/d-print.w (list-name).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

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
{sys/form/r-top3w.f}

DEF VAR v-head      AS CHAR FORMAT "x(280)" EXTENT 4 NO-UNDO.
DEF VAR v-per-rpt   AS LOG  FORMAT "PTD/YTD" INIT YES NO-UNDO.
DEF VAR v-date      AS DATE EXTENT 10 NO-UNDO.

DEF VAR v-length    AS CHAR NO-UNDO.
DEF VAR v-width     AS CHAR NO-UNDO.
DEF VAR v-depth     AS CHAR NO-UNDO.

DEF VAR v-dimensions    AS CHAR FORMAT "X(25)" NO-UNDO.
DEF VAR v-last-prod-dt  AS DATE NO-UNDO.
DEF VAR v-next-prod-dt  AS DATE NO-UNDO.
DEF VAR v-next-prod-qty AS DECI NO-UNDO.
DEF VAR v-caliper       LIKE ef.cal NO-UNDO.
DEF VAR v-last-ship-dt  AS DATE NO-UNDO.
DEF VAR v-no-of-colors  AS INT NO-UNDO.
DEF VAR v-cal           LIKE ef.cal NO-UNDO.
DEF VAR v-tot-inv       AS DECI NO-UNDO.
DEF VAR v-combined-mths AS DECI NO-UNDO.

DEF BUFFER b-itemfg  FOR itemfg.
DEF BUFFER b-eb      FOR eb.
DEF BUFFER b-est     FOR est.
DEF BUFFER b-ef      FOR ef.
DEF BUFFER b-job     FOR job.
DEF BUFFER b-job-hdr FOR job-hdr.
DEF BUFFER b-fg-rcpth FOR fg-rcpth.

FORMAT HEADER
   v-head[1] SKIP
   v-head[2] SKIP
   v-head[3] SKIP
   v-head[4]
WITH FRAME r-top WIDTH 280.

ASSIGN
   str-tit3 = (IF v-per-rpt THEN "P" ELSE "Y") +
              "TD (" + STRING(v-date[1]) + "-" + STRING(v-date[2]) +
              ")"

{sys/inc/ctrtext.i str-tit3 162}

v-head[1] = "".

ASSIGN                                                                                                                              
   v-head[2] = "         CUSTOMERS    DESCRIPTION/              SUPPLIERS                  TOT                          SUPPLIERS   CUSTOMERS  COMBINED     NEXT   NEXT PROD     LAST     EAU @     LAST      DATE    CASE  PIAP "
   v-head[3] = "PLANT ID PART NO      DIMENSIONS                PART NO         STYLE  CAL INK      PRICE/M        EAU  INVENTORY   INVENTORY    MTHS    PROD DATE  QUANTITY  PROD DATE LAST PROD SHIP DATE OBSOLETE  QTY   SCAN   "
   v-head[4] = FILL("-",208).

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

DISPLAY "" WITH FRAME r-top.

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   ASSIGN excelheader = "PLANT ID,CUSTOMERS PART NO,DESCRIPTION,SUPLLIERS PART NO,DIMENSIONS,STYLE,CAL,TOT INK,PRICE/M,EAU,SUPPLIERS INVENTORY,CUSTOMERS INVENTORY,COMBINED MTHS,NEXT PROD DATE,NEXT PROD QTY,LAST PROD DATE,EAU @ LAST PROD,LAST SHIP DATE,DATE OBSOLETE,CASE QTY,PIAP SCAN".
   PUT STREAM excel UNFORMATTED excelheader SKIP.
END.


FOR EACH vend-whse-item WHERE vend-whse-item.vendor-code >= FI-beg-vend-code
                          AND vend-whse-item.vendor-code <= FI-end-vend-code
                          AND vend-whse-item.vendor-plant-code >= FI-beg-vend-plant-code
                          AND vend-whse-item.vendor-plant-code <= FI-end-vend-plant-code
                          AND vend-whse-item.fg-item-no >= FI-beg-fg-item-no
                          AND vend-whse-item.fg-item-no <= FI-end-fg-item-no
                          AND vend-whse-item.cust-part-no >= FI-beg-cust-part-no
                          AND vend-whse-item.cust-part-no <= FI-end-cust-part-no
   BREAK BY vend-whse-item.vendor-code                                            
         BY vend-whse-item.vendor-plant-code:

      CREATE tt-vend-whse-item.
      BUFFER-COPY vend-whse-item TO tt-vend-whse-item
      ASSIGN
         tt-vend-whse-item.row-id     = ROWID(vend-whse-item)
         tt-vend-whse-item.has-rec    = YES
         tt-vend-whse-item.seq-no     = 1.
END.
FOR EACH tt-vend-whse-item
  BREAK BY tt-vend-whse-item.vendor-code                                            
        BY tt-vend-whse-item.vendor-plant-code:

   ASSIGN 
      v-dimensions   = ""
      v-last-prod-dt = ?
      v-caliper = 0
      v-next-prod-qty = 0
      v-next-prod-dt = ?
      v-last-ship-dt = ?
      v-no-of-colors = 0
      v-tot-inv = 0
      v-combined-mths = 0.   

   FIND FIRST b-itemfg WHERE b-itemfg.company = tt-vend-whse-item.company
                         AND b-itemfg.i-no    = tt-vend-whse-item.fg-item-no
                         AND b-itemfg.part-no = tt-vend-whse-item.cust-part-no NO-LOCK NO-ERROR.

   IF NOT AVAILABLE(b-itemfg) THEN 
      FIND FIRST b-itemfg WHERE b-itemfg.company = tt-vend-whse-item.company
                            AND b-itemfg.i-no    = tt-vend-whse-item.fg-item-no NO-LOCK NO-ERROR.

   IF tt-vend-whse-item.cust-part-no = "" THEN
      tt-vend-whse-item.cust-part-no = b-itemfg.part-no.

   RUN sys/inc/dec-frac.p (INPUT l-score[50],
                           INPUT 32,
                           OUTPUT v-length).
   RUN sys/inc/dec-frac.p (INPUT w-score[50],
                           INPUT 32,
                           OUTPUT v-width).
   RUN sys/inc/dec-frac.p (INPUT d-score[50],
                           INPUT 32,
                           OUTPUT v-depth).

   ASSIGN
      v-dimensions = TRIM(v-length) + " x " + TRIM(v-width) + " x " + TRIM(v-depth).

   /* start - get last and next production date/quantity */

   FOR EACH b-job-hdr NO-LOCK WHERE b-job-hdr.company = tt-vend-whse-item.company
                                AND b-job-hdr.i-no    = tt-vend-whse-item.fg-item-no
                                AND b-job-hdr.opened  = NO
                          USE-INDEX i-no,
      FIRST b-job NO-LOCK WHERE b-job.company = b-job-hdr.company
                            AND b-job.job     = b-job-hdr.job
                            AND b-job.job-no  = b-job-hdr.job-no
                            AND b-job.job-no2 = b-job-hdr.job-no2
                            AND b-job.due-date <> ?
                       BREAK BY b-job.due-date:
      IF FIRST-OF(b-job.due-date) THEN
         v-last-prod-dt = b-job.due-date.

      FIND FIRST b-est WHERE b-est.company = b-job-hdr.company
                         AND b-est.est-no  = b-job-hdr.est-no NO-LOCK NO-ERROR.
      FIND FIRST b-ef WHERE b-ef.company = b-est.company
                        AND b-ef.est-no  = b-est.est-no NO-LOCK NO-ERROR.
      FIND FIRST b-eb WHERE b-eb.company = b-ef.company 
                        AND b-eb.est-no  = b-ef.est-no
                        AND b-eb.form-no = b-ef.form-no NO-LOCK NO-ERROR.  

      IF b-job.due-date >= v-last-prod-dt THEN
         ASSIGN
            v-last-prod-dt = b-job.due-date
            v-caliper      = b-ef.cal
            v-no-of-colors = b-eb.i-col.
   END.

   FOR EACH b-job-hdr NO-LOCK WHERE b-job-hdr.company = tt-vend-whse-item.company
                                AND b-job-hdr.i-no    = tt-vend-whse-item.fg-item-no
                                AND b-job-hdr.opened  = YES
                          USE-INDEX i-no,
      FIRST b-job NO-LOCK WHERE b-job.company = b-job-hdr.company
                            AND b-job.job     = b-job-hdr.job
                            AND b-job.job-no  = b-job-hdr.job-no
                            AND b-job.job-no2 = b-job-hdr.job-no2
                            AND b-job.due-date <> ?
                       BREAK BY b-job.due-date
                             BY b-job.due-date:
      IF FIRST-OF(b-job.due-date) THEN
         v-next-prod-dt = b-job.due-date.


      IF b-job.due-date >= TODAY AND b-job.due-date <= v-next-prod-dt THEN DO:
         ASSIGN
            v-next-prod-dt = b-job.due-date
            v-next-prod-qty = b-job-hdr.qty.
      END.
      ELSE
         ASSIGN
            v-next-prod-dt = ?
            v-next-prod-qty = 0.
   END.

   FOR EACH b-fg-rcpth WHERE b-fg-rcpth.company  = tt-vend-whse-item.company 
                         AND b-fg-rcpth.i-no     = tt-vend-whse-item.fg-item-no 
                         AND b-fg-rcpth.rita-code = "S" NO-LOCK
                    BREAK BY b-fg-rcpth.rita-code 
                          BY b-fg-rcpth.trans-date DESC: 
      IF FIRST-OF(b-fg-rcpth.rita-code) THEN DO:
         v-last-ship-dt = b-fg-rcpth.trans-date.
         LEAVE.
      END.
   END.

   /* end - get last date and next production date/quantity */

   v-tot-inv = b-itemfg.q-onh + tt-vend-whse-item.plant-tot-oh-qty.
   IF v-tot-inv > 0 AND tt-vend-whse-item.est-annual-usage > 0 THEN
      v-combined-mths = ROUND((v-tot-inv / tt-vend-whse-item.est-annual-usage) * 12, 2).

   DISPLAY
      tt-vend-whse-item.vendor-plant-code FORMAT "X(8)"
      SPACE(1)
      tt-vend-whse-item.cust-part-no      FORMAT "X(12)"
      b-itemfg.i-name                     FORMAT "X(25)"
      tt-vend-whse-item.fg-item-no        FORMAT "X(15)"
      b-itemfg.style                      FORMAT "X(6)"
      b-ef.cal WHEN AVAILABLE b-ef        FORMAT ".999"
      v-no-of-colors                      FORMAT ">9"
      b-itemfg.sell-price                 FORMAT ">,>>>,>>9.99"
      tt-vend-whse-item.est-annual-usage  FORMAT "->,>>>,>>9"
      b-itemfg.q-onh                      FORMAT "->,>>>,>>9"
      tt-vend-whse-item.plant-tot-oh-qty  FORMAT "->,>>>,>>9"
      SPACE(2)
      v-combined-mths                     FORMAT ">>9.99"
      SPACE(6)
      v-next-prod-dt
      v-next-prod-qty                     FORMAT ">,>>>,>>9"
      SPACE(2)
      v-last-prod-dt
      SPACE(13) /* EAU @ last prod */
      v-last-ship-dt
      tt-vend-whse-item.obsolete-date
      b-itemfg.case-count                 FORMAT ">>>9"
      tt-vend-whse-item.piap-scan
      WITH FRAME a NO-BOX NO-LABELS STREAM-IO DOWN  WIDTH 280.

   DISPLAY
      SPACE(22)
      v-dimensions                        FORMAT "X(25)"
      WITH FRAME b NO-BOX NO-LABELS STREAM-IO DOWN  WIDTH 280.

   IF AVAILABLE(b-ef) THEN
      v-cal = b-ef.cal.
   ELSE
      v-cal = 0.

   IF tb_excel THEN
      PUT STREAM excel UNFORMATTED
         '"' tt-vend-whse-item.vendor-plant-code               '",'
         '"' REPLACE(tt-vend-whse-item.cust-part-no,'"', "")   '",'
         '"' REPLACE(b-itemfg.i-name,'"', "")                  '",'
         '"' REPLACE(tt-vend-whse-item.fg-item-no,'"', "")     '",'
         '"' v-dimensions                                      '",'
         '"' b-itemfg.style                                    '",'
         '"' v-cal                                             '",'
         '"' v-no-of-colors                                    '",'
         '"' b-itemfg.sell-price                               '",'
         '"' tt-vend-whse-item.est-annual-usage                '",'
         '"' b-itemfg.q-onh                                    '",'
         '"' tt-vend-whse-item.plant-tot-oh-qty                '",'
         '"' v-combined-mths                                   '",' 
         '"' v-next-prod-dt                                    '",'
         '"' v-next-prod-qty                                   '",'
         '"' v-last-prod-dt                                    '",'
         '"'                                                   '",' /* EAU @ last prod */
         '"' v-last-ship-dt                                    '",'
         '"' tt-vend-whse-item.obsolete-date                   '",'
         '"' b-itemfg.case-count                               '",'
         '"' tt-vend-whse-item.piap-scan                       '",'
         SKIP.
END.

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

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
   DEF VAR lv-frame-hdl    AS HANDLE NO-UNDO.
   DEF VAR lv-group-hdl    AS HANDLE NO-UNDO.
   DEF VAR lv-field-hdl    AS HANDLE NO-UNDO.
   DEF VAR lv-field2-hdl   AS HANDLE NO-UNDO.
   DEF VAR parm-fld-list   AS CHAR NO-UNDO.
   DEF VAR parm-lbl-list   AS CHAR NO-UNDO.
   DEF VAR i               AS INT NO-UNDO.
   DEF VAR lv-label        AS CHAR NO-UNDO.

   lv-frame-hdl = FRAME {&FRAME-NAME}:HANDLE.
   lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
   lv-field-hdl = lv-group-hdl:FIRST-CHILD.

   DO WHILE TRUE:
      IF NOT VALID-HANDLE(lv-field-hdl) THEN 
         LEAVE.
      IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0 THEN DO:
         IF lv-field-hdl:LABEL <> ? THEN 
            ASSIGN
               parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
               parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + ",".
         ELSE DO:  /* radio set */
            ASSIGN 
               parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
               lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
            REPEAT:
               IF NOT VALID-HANDLE(lv-field2-hdl) THEN 
                  LEAVE. 
               IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN
                  parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".

               lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
            END.
         END.
      END.
      lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
   END.

   PUT 
      SPACE(28)
      "< Selection Parameters >"
      SKIP(1).

   DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
      IF ENTRY(i,parm-fld-list) NE "" OR ENTRY(i,parm-lbl-list) NE "" THEN DO:
         lv-label = FILL(" ",34 - LENGTH(TRIM(ENTRY(i,parm-lbl-list)))) + TRIM(ENTRY(i,parm-lbl-list)) + ":".        
         PUT 
            lv-label FORMAT "x(35)" AT 5
            SPACE(1)
            TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
            SKIP.              
      END.
   END.
   PUT 
      FILL("-",80) FORMAT "x(80)" SKIP.

END PROCEDURE.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE send-rmemail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rmemail-file AS CHAR .

  DEF VAR retcode AS INT NO-UNDO.
  DEF VAR ls-to-list AS CHAR NO-UNDO.
  DEF VAR ls-to-cust AS CHAR NO-UNDO.
  DEF VAR lv-mailto AS CHAR NO-UNDO.
  DEF VAR lv-mailsubject AS CHAR NO-UNDO.
  DEF VAR lv-mailbody AS CHAR NO-UNDO.
  DEF VAR lv-mailattach AS CHAR NO-UNDO.
  DEF VAR v-overrun-found AS LOG NO-UNDO.
  DEF VAR v-underrun-found AS LOG NO-UNDO.

  DEF VAR v-first AS LOG INIT YES NO-UNDO.
  DEF VAR v-cust-rec-key AS CHAR NO-UNDO.

  FIND FIRST cust WHERE
       cust.company = g_company AND
       cust.active = "X"
       NO-LOCK NO-ERROR.

  IF AVAIL cust THEN
  DO:
     v-cust-rec-key = cust.rec_key.
     RELEASE cust.
  END.

/*    FIELD cust-part-no LIKE vend-whse-trans.cust-part-no           */
/*    FIELD fg-item-no   LIKE vend-whse-trans.fg-item-no             */
/*    FIELD plant-tot-oh-qty LIKE vend-whse-trans.plant-tot-oh-qty   */
/*    FIELD trans-date       LIKE vend-whse-trans.trans-date         */
/*    FIELD trans-qty        LIKE vend-whse-trans.trans-qty          */
/*    FIELD vendor-dept-code LIKE vend-whse-trans.vendor-dept-code   */
/*    FIELD vendor-code      LIKE vend-whse-trans.vendor-code        */
/*    FIELD vend-plant-code  LIKE vend-whse-trans.vendor-plant-code. */
/*   FOR EACH tt-email,                                                                                              */
/*       FIRST vend WHERE                                                                                            */
/*             vend.company = g_company AND                                                                          */
/*             vend.vend-no = tt-email.vend-no AND                                                                   */
/*             vend.active = "A"                                                                                     */
/*             NO-LOCK                                                                                               */
/*       BREAK BY tt-email.po-no:                                                                                    */
/*                                                                                                                   */
/*       IF FIRST-OF(tt-email.po-no) THEN                                                                            */
/*          ASSIGN                                                                                                   */
/*             lv-mailbody = ""                                                                                      */
/*             v-overrun-found = NO                                                                                  */
/*             v-underrun-found = NO.                                                                                */
/*                                                                                                                   */
/*       IF NOT v-overrun-found AND                                                                                  */
/*          tt-email.undovr = "O" THEN                                                                               */
/*          v-overrun-found = YES.                                                                                   */
/*                                                                                                                   */
/*       IF tt-email.undovr = "R" THEN                                                                               */
/*          lv-mailbody = lv-mailbody +  CHR(10) + "Raw Material Receipt From "                                      */
/*                      + "Vendor# "  + STRING (tt-email.vend-no) + "  "                                             */
/*                      + "PO# " + STRING (tt-email.po-no) + "  "                                                    */
/*                      + "Item# " + tt-email.item-no + "  "                                                         */
/*                      + "Item Name - "  + tt-email.item-name + "  "                                                */
/*                      + "The Purchse Order Quantity was " + TRIM(STRING(tt-email.po-qty,"-ZZ,ZZZ,ZZ9.99")) + " "   */
/*                      + tt-email.cons-uom + ". "                                                                   */
/*                      + "We just received " + trim(string(tt-email.recvd-qty,"-ZZ,ZZZ,ZZ9.99"))                    */
/*                      + " " + tt-email.cons-uom  + " out of a total receipt quantity of "                          */
/*                      + trim(string(tt-email.total-recvd-qty,"-ZZ,ZZZ,ZZ9.99")) + " " + tt-email.cons-uom          */
/*                      + ".".                                                                                       */
/*                                                                                                                   */
/*       ELSE IF tt-email.undovr = "U" THEN                                                                          */
/*          lv-mailbody = lv-mailbody + CHR(10) + "UNDERRUN WARNING - "                                              */
/*                      + "Raw Material Receipt From "                                                               */
/*                      + "Vendor# "  + STRING (tt-email.vend-no)  + "  "                                            */
/*                      + "PO# " + STRING (tt-email.po-no) + "  "                                                    */
/*                      + "For Item# "    + tt-email.item-no       + "  "                                            */
/*                      + "With Item Name "  + tt-email.item-name  + "  "                                            */
/*                      + "The Purchse Order Quantity of " + STRING(tt-email.po-qty,"-ZZ,ZZZ,ZZ9.99")                */
/*                      + " " + tt-email.cons-uom + " has Vendor Underrun % " + string(tt-email.underrun-pct) + ". " */
/*                      + "We just received " + string(tt-email.recvd-qty,"-ZZ,ZZZ,ZZ9.99")                          */
/*                      + " " + tt-email.cons-uom + " out of a total receipt quantity of "                           */
/*                      + trim(string(tt-email.total-recvd-qty,"-ZZ,ZZZ,ZZ9.99"))                                    */
/*                      + " " + tt-email.cons-uom + ".".                                                             */
/*                                                                                                                   */
/*       ELSE IF tt-email.undovr = "O" THEN                                                                          */
/*          lv-mailbody = lv-mailbody + CHR(10) + "OVERRUN WARNING - "                                               */
/*                      + "Raw Material Receipt From "                                                               */
/*                      + "Vendor# "  + STRING (tt-email.vend-no) + "  "                                             */
/*                      + "PO# " + STRING (tt-email.po-no) + "  "                                                    */
/*                      + "For Item# "    + tt-email.item-no      + "  "                                             */
/*                      + "With Item Name " + tt-email.item-name  + "  "                                             */
/*                      + "The Purchase Order Quantity of " + TRIM(STRING(tt-email.po-qty,"-ZZ,ZZZ,ZZ9.99"))         */
/*                      + " " + tt-email.cons-uom                                                                    */
/*                      + " has Maximum Vendor Overrun % " + string(tt-email.overrun-pct) + ". "                     */
/*                      + "We just received " + trim(string(tt-email.recvd-qty,"-ZZ,ZZZ,ZZ9.99"))                    */
/*                      + " " + tt-email.cons-uom + " out of a total receipt quantity of "                           */
/*                      + trim(string(tt-email.total-recvd-qty,"-ZZ,ZZZ,ZZ9.99")) + " " + tt-email.cons-uom          */
/*                      + ". "                                                                                       */
/*                      + "Allowed Qty " + trim(string(tt-email.allow-qty,"-ZZ,ZZZ,ZZ9.99"))                         */
/*                      + " " + tt-email.cons-uom + ". "                                                             */
/*                      + "QTY IS OVERRUN NOT AUTHORIZED.  "  + " "                                                  */
/*                      + "NEGATIVE RECEIPT SHOULD BE ISSUED FOR OVERRUN. ".                                         */
/*                                                                                                                   */
/*       IF LAST-OF(tt-email.po-no) THEN do:                                                                         */
/*                                                                                                                   */
/*           {custom/emailList.i &recKey=vend.rec_key &emailList=ls-to-list}                                         */
/*           {custom/emailList.i &recKey=v-cust-rec-key &emailList=ls-to-cust}                                       */
/*                                                                                                                   */
/*           IF ls-to-list + ls-to-cust NE '' THEN DO:                                                               */
/*                                                                                                                   */
/*             ASSIGN lv-mailbody = LEFT-TRIM(lv-mailbody)                                                           */
/*                    lv-mailto = "To:" + ls-to-list + "," + ls-to-cust                                              */
/*                    lv-mailsubject = "".                                                                           */
/*                                                                                                                   */
/*             IF v-overrun-found THEN                                                                               */
/*                lv-mailsubject = "OVERRUN WARNING ".                                                               */
/*             IF v-underrun-found THEN                                                                              */
/*                lv-mailsubject = "UNDERRUN WARNING ".                                                              */
/*                                                                                                                   */
/*             lv-mailsubject = lv-mailsubject + "Raw Goods Receipts have been posted".                              */
/*                                                                                                                   */
/*             RUN mail(lv-mailto,lv-mailsubject,lv-mailbody,"",                                                     */
/*                      INT(rmemail-dlg-box),OUTPUT retcode).                                                        */
/*           END.                                                                                                    */
/*       END. /* last-of(tt-email.vend-no) */                                                                        */
/*   END.                                                                                                            */
/*                                                                                                                   */
/*   EMPTY TEMP-TABLE tt-email.                                                                                      */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

