&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-quoprt.w

  Description: Quote Printing

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: JLF

  Created: 09/20/02

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
DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

{est/printquo.i NEW}

DEF TEMP-TABLE tt-quote FIELD row-id AS ROWID
                        FIELD tt-seq AS INT INIT 999999999
                        INDEX row-id row-id.

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF VAR v-tmp-lines-per-page AS INT NO-UNDO.
DEF VAR vlSkipRec AS LOG NO-UNDO.

{custom/xprint.i}

{methods/prgsecur.i}

/* Build a Table to keep sequence of pdf files */
DEFINE NEW SHARED TEMP-TABLE tt-filelist
    FIELD tt-FileCtr AS INT
    FIELD tt-FileName AS CHAR
INDEX filelist IS PRIMARY TT-FILECTR.

/* Output selection for the report */
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE CallingParameter AS CHAR NO-UNDO.

DEF BUFFER b1-cust FOR cust.
DEF BUFFER b-quotehd FOR quotehd.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-quo-list begin_cust end_cust begin_quo# ~
end_quo# tb_inst tb_notesSpanPage begin_dept end_dept rd_sort tb_note ~
tb_prt-box tb_boardDescription tb_comm tb_prt-comp tb_print-2nd-dscr ~
tb_prt-quoimage tb_BatchMail tb_HideDialog tb_page rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm btn-ok btn-cancel RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS v-quo-list begin_cust end_cust begin_quo# ~
end_quo# tb_inst tb_notesSpanPage begin_dept end_dept lbl_sort-3 rd_sort ~
tb_note tb_prt-box tb_boardDescription tb_comm tb_prt-comp ~
tb_print-2nd-dscr tb_prt-quoimage tb_BatchMail tb_HideDialog tb_page ~
rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE v-quo-list AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 82 BY 3.1 NO-UNDO.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_dept AS CHARACTER FORMAT "X(2)" 
     LABEL "Beginning Department" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_quo# AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Beginning Quote#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_dept AS CHARACTER FORMAT "X(2)" INITIAL "zz" 
     LABEL "Ending Department" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_quo# AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 
     LABEL "Ending Quote#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE lbl_sort-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Sort by?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

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

DEFINE VARIABLE v-group-title AS CHARACTER FORMAT "X(8)" INITIAL "EMAIL" 
     LABEL "Send to Title" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Quote#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Estimate#", "Estimate#",
"Cust Part#", "Cust Part#",
"Quote#", "Quote#",
"As Entered", "As Entered"
     SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE tb_boardDescription AS CHARACTER INITIAL "Est" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Estimate Board Description", "Est",
"Quote  Board Description", "Quote"
     SIZE 30 BY 1.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 106 BY 9.29.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 106 BY 13.1.

DEFINE VARIABLE tb_BatchMail AS LOGICAL INITIAL no 
     LABEL "&Batch E-Mail" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_comm AS LOGICAL INITIAL no 
     LABEL "Print Salesman Commission?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE tb_HideDialog AS LOGICAL INITIAL no 
     LABEL "&Hide Dialog-Box" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_inst AS LOGICAL INITIAL no 
     LABEL "Print Department Manufacturing Instructions?" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.

DEFINE VARIABLE tb_note AS LOGICAL INITIAL yes 
     LABEL "Print Notes per Item or Form?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE tb_notesSpanPage AS LOGICAL INITIAL no 
     LABEL "Instructions Span Multiple Pages" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE tb_page AS LOGICAL INITIAL no 
     LABEL "Separate Page Each Quote #" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-2nd-dscr AS LOGICAL INITIAL no 
     LABEL "Print 2nd Item Description Line?" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-box AS LOGICAL INITIAL no 
     LABEL "Print Box Design?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-comp AS LOGICAL INITIAL yes 
     LABEL "Print Components?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prt-quoimage AS LOGICAL INITIAL yes 
     LABEL "Print Quote Image?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     v-quo-list AT ROW 2.19 COL 14 NO-LABEL
     begin_cust AT ROW 5.52 COL 34 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 5.52 COL 77 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_quo# AT ROW 6.48 COL 34 COLON-ALIGNED HELP
          "Enter Beginning Quote Number"
     end_quo# AT ROW 6.48 COL 77 COLON-ALIGNED HELP
          "Enter Ending QuoteNumber"
     tb_inst AT ROW 7.91 COL 14
     tb_notesSpanPage AT ROW 7.91 COL 61
     begin_dept AT ROW 8.86 COL 38 COLON-ALIGNED HELP
          "Enter Beginning Department"
     end_dept AT ROW 8.86 COL 77 COLON-ALIGNED HELP
          "Enter Endng Department"
     lbl_sort-3 AT ROW 10.05 COL 15 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 10.05 COL 27 NO-LABEL
     tb_note AT ROW 11.19 COL 45 RIGHT-ALIGNED
     tb_prt-box AT ROW 11.24 COL 48
     tb_boardDescription AT ROW 11.29 COL 73 NO-LABEL
     tb_comm AT ROW 12.14 COL 45 RIGHT-ALIGNED
     tb_prt-comp AT ROW 12.19 COL 48
     tb_print-2nd-dscr AT ROW 13 COL 51 RIGHT-ALIGNED
     tb_prt-quoimage AT ROW 13.14 COL 74
     tb_BatchMail AT ROW 14.43 COL 30
     tb_HideDialog AT ROW 14.43 COL 50.6
     tb_page AT ROW 14.43 COL 71.6
     rd-dest AT ROW 15.52 COL 5 NO-LABEL
     lv-ornt AT ROW 15.52 COL 30 NO-LABEL
     lines-per-page AT ROW 15.52 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 16.95 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 17.91 COL 29 COLON-ALIGNED NO-LABEL
     v-group-title AT ROW 20.05 COL 41 COLON-ALIGNED HELP
          "Enter Email Title"
     td-show-parm AT ROW 22.43 COL 5
     btn-ok AT ROW 23.62 COL 29
     btn-cancel AT ROW 23.62 COL 63
     RECT-6 AT ROW 14.1 COL 1
     RECT-7 AT ROW 1 COL 1
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 14.33 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     " Enter Quotes separated by comma" VIEW-AS TEXT
          SIZE 36 BY .62 AT ROW 1.48 COL 35
          BGCOLOR 14 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 106.6 BY 24.1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Print Quotes"
         HEIGHT             = 24.52
         WIDTH              = 107.6
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
IF NOT C-Win:LOAD-ICON("images\progress":U) THEN
    MESSAGE "Unable to load icon: images\progress"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_quo#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_dept:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_quo#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort-3 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort-3:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_comm IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_comm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_HideDialog:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_inst:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_note IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_note:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_print-2nd-dscr IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_print-2nd-dscr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN v-group-title IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       v-group-title:HIDDEN IN FRAME FRAME-A           = TRUE
       v-group-title:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       v-quo-list:RETURN-INSERTED IN FRAME FRAME-A  = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Print Quotes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Print Quotes */
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


&Scoped-define SELF-NAME begin_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_dept C-Win
ON LEAVE OF begin_dept IN FRAME FRAME-A /* Beginning Department */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_quo#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_quo# C-Win
ON LEAVE OF begin_quo# IN FRAME FRAME-A /* Beginning Quote# */
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
    ASSIGN {&DISPLAYED-OBJECTS}.          
  END.

  ASSIGN
    init-dir = "c:\tmp\"
    lv-pdf-file = INIT-dir + (IF v-print-fmt EQ "Century" THEN "CBXQuote"
                              ELSE "QT") + STRING(begin_quo#).

  /* SKB - 1/24/07 - Changes for XL printing */

  IF tb_page:CHECKED AND (v-print-fmt EQ "CSC-EXCEL" OR
                          v-print-fmt EQ "TRILAKE-EXCEL") THEN
     DO:
        MESSAGE "'Separate Page Each Quote #' May not be Selected for Excel format."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN NO-APPLY.
     END.

  IF rd-dest = 1  THEN
      ASSIGN LvOutputSelection = "Printer".
  ELSE IF rd-dest = 2  THEN
      ASSIGN LvOutputSelection = "Screen". 
  ELSE IF rd-dest = 3  THEN
      ASSIGN LvOutputSelection = "File". 
  ELSE IF rd-dest = 4  THEN
      ASSIGN LvOutputSelection = "Fax". 
  ELSE IF rd-dest = 5  THEN
      ASSIGN LvOutputSelection = "Email".
  ELSE IF rd-dest = 6  THEN
      ASSIGN LvOutputSelection = "Port".

  IF rd-dest NE 5 THEN
     run run-report(INPUT NO, INPUT ""). 
 
  IF v-print-fmt <> "CSC-EXCEL" AND v-print-fmt <> "TRILAKE-EXCEL" THEN
  DO:
    case rd-dest:
      when 1 then run output-to-printer.
      when 2 then run output-to-screen.
      when 3 then run output-to-file.
      when 4 then do:
          /*run output-to-fax.*/
          {custom/asifax.i &begin_cust=begin_cust 
                           &END_cust=END_cust
                           &fax-subject="Quote"
                           &fax-body="Quote"
                           &fax-file=list-name }
      END.
      WHEN 6 THEN run output-to-port.
    end case. 
  END.

  IF rd-dest = 5 then do:

     IF tb_page:CHECKED THEN
     DO:
        MESSAGE "'Separate Page Each Quote #' not valid in Email Mode."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN NO-APPLY.
     END.

     IF NOT tb_BatchMail:CHECKED AND
        begin_cust <> end_cust THEN
        DO:
           MESSAGE "Beginning Customer and Ending Customer must be the same for E-Mail."
              VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO end_cust IN FRAME {&FRAME-NAME}.
           RETURN NO-APPLY.
        END.

     IF v-print-fmt = "CSC-EXCEL" OR v-print-fmt = "TRILAKE-EXCEL" THEN 
        ASSIGN lv-pdf-file = "c:\tmp\quote"
               is-xprint-form   = TRUE.
     
     RUN output-to-mail.

     RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    
     SESSION:SET-WAIT-STATE ("").
  END.

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


&Scoped-define SELF-NAME end_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_dept C-Win
ON LEAVE OF end_dept IN FRAME FRAME-A /* Ending Department */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_quo#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_quo# C-Win
ON LEAVE OF end_quo# IN FRAME FRAME-A /* Ending Quote# */
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
  /*
  IF {&self-name} = 5 THEN do:
     v-group-title:SENSITIVE = YES.
     APPLY "entry" TO v-group-title.
  END.
  ELSE v-group-title:SENSITIVE = NO.
  */
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


&Scoped-define SELF-NAME tb_BatchMail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_BatchMail C-Win
ON VALUE-CHANGED OF tb_BatchMail IN FRAME FRAME-A /* Batch E-Mail */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_boardDescription
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_boardDescription C-Win
ON VALUE-CHANGED OF tb_boardDescription IN FRAME FRAME-A
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_comm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_comm C-Win
ON VALUE-CHANGED OF tb_comm IN FRAME FRAME-A /* Print Salesman Commission? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_HideDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_HideDialog C-Win
ON VALUE-CHANGED OF tb_HideDialog IN FRAME FRAME-A /* Hide Dialog-Box */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inst C-Win
ON VALUE-CHANGED OF tb_inst IN FRAME FRAME-A /* Print Department Manufacturing Instructions? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_note
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_note C-Win
ON VALUE-CHANGED OF tb_note IN FRAME FRAME-A /* Print Notes per Item or Form? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_notesSpanPage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_notesSpanPage C-Win
ON VALUE-CHANGED OF tb_notesSpanPage IN FRAME FRAME-A /* Instructions Span Multiple Pages */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_print-2nd-dscr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_print-2nd-dscr C-Win
ON VALUE-CHANGED OF tb_print-2nd-dscr IN FRAME FRAME-A /* Print 2nd Item Description Line? */
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


&Scoped-define SELF-NAME v-group-title
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-group-title C-Win
ON HELP OF v-group-title IN FRAME FRAME-A /* Send to Title */
DO:
    DEF VAR v-title AS cha NO-UNDO.

    RUN windows/l-ttlcod.w (FOCUS:SCREEN-VALUE, OUTPUT v-title).
    IF v-title <> "" THEN SELF:SCREEN-VALUE = entry(1,v-title).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-group-title C-Win
ON LEAVE OF v-group-title IN FRAME FRAME-A /* Send to Title */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-quo-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-quo-list C-Win
ON VALUE-CHANGED OF v-quo-list IN FRAME FRAME-A
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

  FIND FIRST quotehd WHERE ROWID(quotehd) EQ ip-rowid NO-LOCK NO-ERROR.
  IF AVAIL quotehd THEN
  FIND FIRST est
      WHERE est.company EQ quotehd.company
        AND est.est-no  EQ quotehd.est-no
      NO-LOCK NO-ERROR.
  
  
  /*IF NOT AVAIL est THEN RETURN.*/

  ASSIGN
   begin_cust = quotehd.cust-no
   end_cust   = begin_cust
   begin_quo# = quotehd.q-no
   end_quo#   = begin_quo#.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "QUOPRINT"
      no-lock no-error.
  if not avail sys-ctrl then
  do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "QUOPRINT"
     sys-ctrl.descrip = "Print Quote Headers on Quote Form?".
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  end.
  ASSIGN
   v-print-fmt = sys-ctrl.char-fld
   CallingParameter = sys-ctrl.char-fld
   v-log       = sys-ctrl.log-fld.


  /*IF v-print-fmt EQ "10 Pitch" AND
     AVAIL est AND est.est-type LE 4 THEN
    ASSIGN
     v-program      = "ce/quote/prtquo10.p"
     lines-per-page = 56.
 
  ELSE*/
  IF INDEX("Pacific,Xprint,SouthPak,ABox,Midwest,century,Concepts,oracle,Harwell,PremierX,Elite,Unipak,Ottpkg,Frankstn,Mirpkg,APC,FibreX,PPI",v-print-fmt) > 0
       THEN is-xprint-form = YES.     
  ELSE is-xprint-form = NO.

  IF v-print-fmt EQ "HOP" THEN
    ASSIGN
     v-program      = "cec/quote/quohop.p"
     lines-per-page = 37.

  ELSE
  IF v-print-fmt EQ "LandScap" THEN
    ASSIGN
     v-program      = "ce/quote/landquo.p"
     lines-per-page = 56.

  ELSE
  IF v-print-fmt EQ "ContSrvc" OR
     v-print-fmt EQ "Triad"    THEN
    ASSIGN
     v-program      = "cec/quote/quocsc.p"
     lines-per-page = 56.

  ELSE
  IF v-print-fmt EQ "Rudd" THEN
    ASSIGN
     v-program      = "cec/quote/quorudd.p"
     lines-per-page = 66.

  ELSE
  IF v-print-fmt EQ "General" THEN
    ASSIGN
     v-program      = "cec/quote/quogener.p"
     lines-per-page = 56.

  ELSE
  IF v-print-fmt EQ "10 Pitch" /*AND AVAIL est AND est.est-type GT 4*/ THEN
    ASSIGN
     v-program      = "cec/quote/prtquo10.p"
     lines-per-page = 56.

  ELSE
  IF v-print-fmt EQ "Brick" THEN
    ASSIGN
     v-program      = "cec/quote/quobrick.p"
     lines-per-page = 38.

  ELSE
  IF v-print-fmt EQ "Fibre" THEN 
    ASSIGN
     v-program      = "cec/quote/quofibre.p"
     lines-per-page = 52.
    
  ELSE
  IF v-print-fmt EQ "Harwell" THEN
    ASSIGN
     v-program      = "cec/quote/quohawl.p"
     lines-per-page = 56.
  ELSE
  IF v-print-fmt EQ "Pacific" THEN
    ASSIGN
     v-program      = "cec/quote/quopacif.p"
     lines-per-page = 66.  
  ELSE
  IF v-print-fmt EQ "Abox" THEN
    ASSIGN
     v-program      = "cec/quote/quoabox.p"
     lines-per-page = 66.  

  ELSE IF v-print-fmt EQ "Xprint" THEN
    ASSIGN
     v-program      = "cec/quote/quoxprnt.p"
     lines-per-page = 66.  

  ELSE IF v-print-fmt EQ "FibreX" THEN
    ASSIGN
     v-program      = "cec/quote/quoxfib.p"
     lines-per-page = 66.  

  ELSE IF v-print-fmt EQ "Frankstn" OR v-print-fmt EQ "Mirpkg" THEN
  ASSIGN
   v-program      = "cec/quote/quofrank.p"
   lines-per-page = 66.  

  ELSE
  IF v-print-fmt EQ "Elite" THEN
    ASSIGN
     v-program      = "cec/quote/quoelite.p"
     lines-per-page = 66.  

  ELSE
  IF v-print-fmt EQ "premierX" THEN
    ASSIGN
     v-program      = "cec/quote/quoxprem.p"
     lines-per-page = 66.  

  ELSE IF v-print-fmt EQ "SouthPak" THEN
    ASSIGN
     v-program      = "cec/quote/quosthpk.p"
     lines-per-page = 66.  

  ELSE IF v-print-fmt EQ "APC" THEN
    ASSIGN
     v-program      = "cec/quote/quoxapc.p"
     lines-per-page = 66.  

  ELSE IF v-print-fmt EQ "Midwest" THEN
    ASSIGN
     v-program      = "cec/quote/quomwest.p"
     lines-per-page = 66.  

  ELSE IF v-print-fmt EQ "Century" THEN
    ASSIGN
     v-program      = "cec/quote/quocentx.p"
     lines-per-page = 66.  

  ELSE IF v-print-fmt EQ "Unipak" THEN
    ASSIGN
     v-program      = "cec/quote/quounipk.p"
     lines-per-page = 66.  

  ELSE IF v-print-fmt EQ "Oracle" THEN
    ASSIGN
     v-program      = "cec/quote/quooracl.p"
     lines-per-page = 66.  

  ELSE IF v-print-fmt EQ "OTTPkg" THEN
    ASSIGN
     v-program      = "cec/quote/quoottpk.p"
     lines-per-page = 66.

  ELSE IF v-print-fmt EQ "CSC-EXCEL" THEN
    ASSIGN
     v-program      = "cec/quote/quocsc-xl.p"
     lines-per-page = 66.

  ELSE IF v-print-fmt EQ "TRILAKE-EXCEL" THEN
    ASSIGN
     v-program      = "cec/quote/quotri-xl.p"
     lines-per-page = 66.

   ELSE IF v-print-fmt EQ "Concepts" THEN
    ASSIGN
     v-program      = "cec/quote/quocorc.p"
     lines-per-page = 66.

   ELSE IF v-print-fmt EQ "PPI" THEN
    ASSIGN
     v-program      = "cec/quote/quoppi.p"
     lines-per-page = 66.  

  ELSE
  IF AVAIL est AND est.est-type GT 4 THEN
    ASSIGN
     v-program      = "cec/quote/quoasi.p"
     lines-per-page = 56.

  ELSE
    ASSIGN
     v-program      = "ce/quote/quoasi.p"
     lines-per-page = IF v-log THEN 50 ELSE 56.
 
  v-tmp-lines-per-page = lines-per-page.
  RUN enable_UI.

  DO WITH FRAME {&frame-name}:
    {custom/usrprint.i} 
   /* ASSIGN rd-dest. screw all screen-values
    IF int(rd-dest:SCREEN-VALUE) = 5 THEN v-group-title:SENSITIVE = YES.
    ELSE v-group-title:SENSITIVE = NO. */

    ASSIGN begin_cust:SCREEN-VALUE = quotehd.cust-no
           end_cust:SCREEN-VALUE   = begin_cust
           begin_quo#:SCREEN-VALUE = string(quotehd.q-no)
           end_quo#:SCREEN-VALUE   = string(begin_quo#).

    /*IF lines-per-page = 0 THEN ASSIGN lines-per-page = v-tmp-lines-per-page
                                      lines-per-page:SCREEN-VALUE = STRING(v-tmp-lines-per-page).
    */
    lines-per-page:SCREEN-VALUE = STRING(v-tmp-lines-per-page).
    DISABLE lines-per-page.
    IF NOT AVAIL est OR est.est-type LE 4 THEN DISABLE tb_note tb_comm.
    IF NOT AVAIL est OR est.est-type LE 4 OR v-print-fmt NE "XPrint" THEN DO:
      ASSIGN
        tb_boardDescription:SCREEN-VALUE = 'Est'
        tb_boardDescription = 'Est'.
      HIDE tb_boardDescription NO-PAUSE.
    END.
    IF v-print-fmt NE "10 Pitch" THEN DISABLE tb_note.
    IF v-print-fmt NE "Brick" AND
       v-print-fmt NE "ASI" AND v-print-fmt NE "PACIFIC"
        THEN DISABLE tb_comm.
    
    IF is-xprint-form = NO THEN DISABLE tb_prt-box.
    IF v-print-fmt = "Century" THEN tb_prt-quoimage:HIDDEN = NO.
    ELSE tb_prt-quoimage:HIDDEN = YES.

    v-quo-list:SCREEN-VALUE = TRIM(v-quo-list:SCREEN-VALUE).

    APPLY "entry" TO v-quo-list.
  END.
 
  {methods/nowait.i}
  APPLY "entry" TO v-quo-list IN FRAME {&FRAME-NAME}.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BatchMail C-Win 
PROCEDURE BatchMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAM icBegCustNo  AS CHAR NO-UNDO.
  DEFINE INPUT PARAM icEndCustNo  AS CHAR NO-UNDO.

  FOR EACH b1-cust WHERE
      b1-cust.company = cocode AND
      b1-cust.cust-no GE icBegCustNo AND
      b1-cust.cust-no LE icEndCustNo
      NO-LOCK,
      FIRST b-quotehd WHERE
            b-quotehd.company EQ cocode AND
            b-quotehd.loc EQ locode AND
            b-quotehd.q-no GE begin_quo# AND
            b-quotehd.q-no LE end_quo# AND
            b-quotehd.cust-no EQ b1-cust.cust-no
            NO-LOCK:

      IF tb_BatchMail:CHECKED IN FRAME {&FRAME-NAME} THEN
      DO:
         vlSkipRec = YES.
         
         FOR EACH phone WHERE 
             phone.table_rec_key = b1-cust.rec_key
             NO-LOCK:
         
             IF CAN-FIND (FIRST emaildtl WHERE
                emaildtl.emailcod  BEGINS 'r-quoprt' AND
                emaildtl.table_rec_key  = phone.rec_key) THEN 
                DO:
                   vlSkipRec = NO.
                   LEAVE.
                END.
         END.
         
         IF vlSkipRec THEN NEXT.
      END.

      IF v-print-fmt <> "CSC-EXCEL" AND v-print-fmt <> "TRILAKE-EXCEL" THEN
         lv-pdf-file = "c:\tmp\" + (IF v-print-fmt EQ "Century" THEN "CBXQuote"
                       ELSE "QT") + STRING(b-quotehd.q-no).
      ELSE
         lv-pdf-file = "c:\tmp\quote".
      
      RUN run-report (INPUT yes, INPUT b1-cust.cust-no).
      
      STATUS DEFAULT 'Now processing CUST: ' + b1-cust.cust-no + '....'.
      
      RUN GenerateMail(INPUT b-quotehd.q-no).
      
      STATUS DEFAULT ''.
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
  DISPLAY v-quo-list begin_cust end_cust begin_quo# end_quo# tb_inst 
          tb_notesSpanPage begin_dept end_dept lbl_sort-3 rd_sort tb_note 
          tb_prt-box tb_boardDescription tb_comm tb_prt-comp tb_print-2nd-dscr 
          tb_prt-quoimage tb_BatchMail tb_HideDialog tb_page rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE v-quo-list begin_cust end_cust begin_quo# end_quo# tb_inst 
         tb_notesSpanPage begin_dept end_dept rd_sort tb_note tb_prt-box 
         tb_boardDescription tb_comm tb_prt-comp tb_print-2nd-dscr 
         tb_prt-quoimage tb_BatchMail tb_HideDialog tb_page rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm btn-ok btn-cancel RECT-6 RECT-7 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateMail C-Win 
PROCEDURE GenerateMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-quote-no AS INT NO-UNDO.

  IF is-xprint-form THEN DO WITH FRAME {&FRAME-NAME}:
      MESSAGE "list-name" list-name VIEW-AS ALERT-BOX.
    
      IF not(v-print-fmt EQ "CSC-EXCEL" OR
             v-print-fmt EQ "TRILAKE-EXCEL") THEN
          
         RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
    
      IF tb_HideDialog:CHECKED THEN RUN SendMail-1 (b1-cust.cust-no, 'Customer1', lv-pdf-file + ".pdf", ip-quote-no).
      ELSE RUN SendMail-1 (b1-cust.cust-no, 'Customer',  lv-pdf-file + ".pdf",ip-quote-no).
  END.

  ELSE DO:
      
    IF tb_HideDialog:CHECKED THEN RUN SendMail-1 (b1-cust.cust-no, 'Customer1', list-name, ip-quote-no).
    ELSE RUN SendMail-1 (b1-cust.cust-no, 'Customer',  list-name, ip-quote-no).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-fax C-Win 
PROCEDURE output-to-fax :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-mail C-Win 
PROCEDURE output-to-mail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      RUN BatchMail (begin_cust, IF NOT tb_BatchMail:CHECKED THEN begin_cust
                                 ELSE end_cust).
   END.
 
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

 IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
 END.
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
  ELSE run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
  
  /*ELSE run scr-rpt.w (list-name,c-win:title). /* open file-name, title */  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* -------------------------------------------- cec/quote/printquo.p 8/94 rd  */
/* print quotes                                                               */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER ip-mail-mode AS LOG NO-UNDO.
DEFINE INPUT PARAMETER icCustNo AS CHAR NO-UNDO.

{sys/form/r-top.i}

DEF VAR lv-quo-no LIKE quotehd.q-no NO-UNDO.
DEF VAR lv-loc LIKE quotehd.loc INIT "" NO-UNDO.
DEF VAR li AS INT NO-UNDO.

ASSIGN
 fquote   = begin_quo#
 tquote   = end_quo#
 v-prt-box = tb_prt-box
 s-sep-page = tb_page
 s-prt-quoimage = tb_prt-quoimage
 fcust = IF ip-mail-mode AND tb_BatchMail:CHECKED IN FRAME {&frame-name} THEN
            icCustNo 
         ELSE begin_cust
 tcust = IF ip-mail-mode AND tb_BatchMail:CHECKED IN FRAME {&frame-name} THEN
            icCustNo 
         ELSE end_cust.

ASSIGN
 ch-inst  = tb_inst
 fdept    = begin_dept
 tdept    = end_dept
 ch-note  = tb_note
 ch-sort  = SUBSTR(rd_sort,1,1)
 v-comm   = tb_comm
 ch-multi = fquote NE tquote
 s-print-comp = tb_prt-comp
 v-notesPageSpan = tb_notesSpanPage
 v-boardDescription = tb_boardDescription
 s-print-2nd-dscr = tb_print-2nd-dscr.
 
{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

FOR EACH tt-quote:
  DELETE tt-quote.
END.

DO WHILE TRUE:  /* because loc is in header */
  FIND FIRST quotehd
      WHERE quotehd.company EQ cocode
        AND quotehd.loc     GT lv-loc
      USE-INDEX q-no NO-LOCK NO-ERROR.

  IF NOT AVAIL quotehd THEN LEAVE.

  lv-loc = quotehd.loc.

  RELEASE quotehd.

  IF fcust EQ tcust AND fquote NE tquote THEN
  FOR EACH quotehd

      WHERE quotehd.cust-no GE fcust
        AND quotehd.cust-no LE tcust
        AND quotehd.company EQ cocode
        AND quotehd.loc     EQ lv-loc
        AND quotehd.q-no    GE fquote
        AND quotehd.q-no    LE tquote
      /*USE-INDEX cust2 */
      NO-LOCK:
    CREATE tt-quote.
    tt-quote.row-id = ROWID(quotehd).
  END.

  ELSE
  FOR EACH quotehd
      WHERE quotehd.company EQ cocode
        AND quotehd.loc     EQ lv-loc
        AND quotehd.cust-no GE fcust
        AND quotehd.cust-no LE tcust
        AND quotehd.q-no    GE fquote
        AND quotehd.q-no    LE tquote
      USE-INDEX q-no NO-LOCK:
    CREATE tt-quote.
    tt-quote.row-id = ROWID(quotehd).
  END.                                                

  DO li = 1 TO NUM-ENTRIES(v-quo-list):
    RELEASE quotehd.
    lv-quo-no = INT(ENTRY(li,v-quo-list)) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR AND lv-quo-no NE 0 THEN
    FIND FIRST quotehd
        WHERE quotehd.company EQ cocode
          AND quotehd.loc     EQ lv-loc  
          AND quotehd.q-no    EQ lv-quo-no
        USE-INDEX q-no NO-LOCK NO-ERROR.
  
    IF AVAIL quotehd THEN DO:
      CREATE tt-quote.
      ASSIGN
       tt-quote.row-id = ROWID(quotehd)
       tt-quote.tt-seq = li.
    END.
  END.
END.
FOR EACH tt-quote BREAK BY tt-quote.row-id BY tt-seq:   
  IF NOT FIRST-OF(tt-quote.row-id) THEN DELETE tt-quote.
END.

FOR EACH tt-quote BREAK BY tt-quote.row-id:
  ch-multi = NOT (FIRST(tt-quote.row-id) AND LAST(tt-quote.row-id)).
  LEAVE.
END.

IF NOT ch-multi THEN
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
   tb_note:SCREEN-VALUE = "YES"
   tb_note.
END.

FIND FIRST quotehd WHERE ROWID(quotehd) EQ ip-rowid NO-LOCK NO-ERROR.
IF AVAIL quotehd THEN
   FIND FIRST est WHERE est.company EQ quotehd.company
         AND est.est-no  EQ quotehd.est-no
         NO-LOCK NO-ERROR.

{sa/sa-sls01.i}

IF NOT ch-multi AND quotehd.est-no NE "" THEN
FOR EACH tt-quote,

    FIRST quotehd
    WHERE ROWID(quotehd) EQ tt-quote.row-id
      AND (CAN-FIND(FIRST est
                    WHERE est.company EQ quotehd.company
                      AND est.est-no  EQ quotehd.est-no) OR
           lookup(v-print-fmt,"Century,Unipak,PPI") > 0 )
    NO-LOCK,

    first quoteitm OF quotehd no-lock,

    first sman
    where sman.company eq quotehd.company
      and sman.sman    eq quotehd.sman
    no-lock,

    first carrier
    where carrier.company eq quotehd.company
      and carrier.carrier eq quotehd.carrier
    no-lock,

    first terms
    where terms.company eq quotehd.company
      and terms.t-code  eq quotehd.terms
    no-lock,

    first cust
    where cust.company eq quotehd.company
      and cust.cust-no eq quotehd.cust-no
    no-lock

    transaction:
  
  create report.
  assign
   report.term-id = v-term
   report.key-01  = quotehd.cust-no
   report.key-02  = if ch-sort eq "E" then quotehd.est-no              else
                    if ch-sort eq "C" then quoteitm.part-no            else
                    if ch-sort eq "A" then string(tt-seq,"9999999999") else ""
   report.key-03  = string(quotehd.q-no,"9999999999")
   report.rec-id  = recid(quotehd).
end.

ELSE    
FOR EACH tt-quote,
    FIRST quotehd WHERE ROWID(quotehd) EQ tt-quote.row-id NO-LOCK,
    first quoteitm OF quotehd no-lock,
    first sman
    where sman.company eq quotehd.company
      and sman.sman    eq quotehd.sman
    no-lock,

    first carrier
    where carrier.company eq quotehd.company
      and carrier.carrier eq quotehd.carrier
    no-lock,

    first terms
    where terms.company eq quotehd.company
      and terms.t-code  eq quotehd.terms
    no-lock,

    first cust
    where cust.company eq quotehd.company
      and cust.cust-no eq quotehd.cust-no
    NO-LOCK
    transaction:
    IF quotehd.est-no <> "" AND lookup(v-print-fmt,"Century,Unipak,PPI") <= 0 THEN DO:
       FIND first est where est.company eq quotehd.company
                   AND est.est-no  EQ quotehd.est-no nO-LOCK NO-ERROR.
       IF NOT AVAIL est THEN  NEXT.
    END.

  create report.
  assign
   report.term-id = v-term
   report.key-01  = quotehd.cust-no
   report.key-02  = if ch-sort eq "E" then quotehd.est-no              else
                    if ch-sort eq "C" then quoteitm.part-no            else
                    if ch-sort eq "A" then string(tt-seq,"9999999999") else ""
   report.key-03  = string(quotehd.q-no,"9999999999")
   report.rec-id  = recid(quotehd).
end.

ASSIGN
 v-term-id        = v-term
 v-lines-per-page = lines-per-page.
/*
IF is-xprint-form AND rd-dest = 2 THEN PUT "<PREVIEW>".
ELSE  IF is-xprint-form AND rd-dest = 1 THEN PUT "<PRINTER?>".
*/
/* Check for XL also */
IF v-print-fmt = "CSC-EXCEL" OR v-print-fmt = "TRILAKE-EXCEL" THEN.
ELSE
IF IS-xprint-form THEN DO:
    CASE rd-dest:
        WHEN 1 THEN PUT  "<PRINTER?></PROGRESS>".
        WHEN 2 THEN PUT "<PREVIEW></PROGRESS>".        
        WHEN  4 THEN do:
              ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
                  /*(IF is-xprint-form THEN ".xpr" ELSE ".txt").*/
              PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW></PROGRESS>".
        END.
        WHEN 5 THEN do:
            IF lookup(v-print-fmt,"century,unipak,ppi") > 0 THEN       
               PUT "<FORMAT=LETTER><PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(130)".
            ELSE PUT "<FORMAT=LETTER><PREVIEW><PDF-EXCLUDE=Arial,Courier New><PDF-LEFT=2mm><PDF-TOP=3mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(130)".
        END.
        
    END CASE.
    
END.

RUN value(v-program).

for each report where report.term-id eq v-term-id:
  delete report.
end.

ASSIGN
   tb_note:SCREEN-VALUE = "YES"
   rd_sort:SCREEN-VALUE = "Quote#"
   tb_note
   rd_sort.


IF NOT ip-mail-mode THEN
DO:
   RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
   SESSION:SET-WAIT-STATE ("").
END.

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMail-1 C-Win 
PROCEDURE SendMail-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAM icIdxKey   AS CHAR NO-UNDO.
  DEFINE INPUT PARAM icRecType  AS CHAR NO-UNDO.
  DEFINE INPUT PARAM icFileName AS CHAR NO-UNDO.
  DEFINE INPUT PARAM ip-quote-no AS INT NO-UNDO.

  DEFINE VARIABLE vcSubject   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcMailBody  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcErrorMsg  AS CHARACTER  NO-UNDO.

  ASSIGN  vcSubject   = "Quote # " + STRING(ip-quote-no) + " " + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
          vcMailBody  = "Please review attached quote(s)".
                      
  RUN custom/xpmail2.p   (input   icRecType,
                          input   'r-quoprt.',
                          input   icFileName,
                          input   icIdxKey,
                          input   vcSubject,
                          input   vcMailBody,
                          OUTPUT  vcErrorMsg).
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
  IF v-print-fmt <> "CSC-EXCEL" AND v-print-fmt <> "TRILAKE-EXCEL" THEN.
  ELSE DO :


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
  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

