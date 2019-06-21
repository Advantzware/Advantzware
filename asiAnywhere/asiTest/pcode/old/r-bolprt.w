&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File              : r-bolprt.w

  Description       : BOL Printing

  Input Parameters  : None

  Output Parameters : None

  Author            : JLF

  Created           : 04/23/02

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

/* Variables */
def var list-name         as char no-undo.
def var init-dir          as char no-undo.

/* Includes */
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}
{sys/inc/var.i new shared}
{oe/rep/oe-lad.i NEW}
{oe/oe-bolpi.i NEW}  
{oe/bolcheck.i NEW}  
{oe/closchk.i NEW}

assign
  cocode = gcompany
  locode = gloc.

/* Buffers */
DEF NEW SHARED BUFFER xoe-ord       FOR oe-ord.
DEFINE BUFFER         bf-oe-boll    FOR oe-boll.
DEFINE BUFFER         b1-cust       FOR cust.
DEFINE BUFFER         b1-oe-bolh    FOR oe-bolh.
DEFINE BUFFER         b1-oe-boll    FOR oe-boll.
DEFINE BUFFER         b1-shipto     FOR shipto.

DEF TEMP-TABLE tt-post NO-UNDO 
    FIELD row-id AS ROWID.

def var v-print-fmt     as char no-undo format 'x'.
def var v-headers       as log  no-undo.
def var v-print-coc     as log  no-undo.
def var v-check-qty     as log  no-undo.
DEF VAR v-program       AS CHAR NO-UNDO.
DEF VAR is-xprint-form  AS LOG  NO-UNDO.
DEF VAR ls-fax-file     AS CHAR NO-UNDO.
DEF VAR lv-pdf-file     AS CHAR NO-UNDO.
DEF VAR vcBOLNums       AS CHAR NO-UNDO.
DEF VAR vcMailMode      AS CHAR NO-UNDO.

{custom/xprint.i}

DEF VAR lv-prt-bypass     AS LOG NO-UNDO.  /* bypass window's printer driver */
DEF VAR lv-run-bol        AS char no-undo.
DEF VAR lv-run-commercial AS char no-undo.

/* Build a Table to keep sequence of pdf files */
DEF NEW SHARED TEMP-TABLE tt-filelist NO-UNDO
                    FIELD tt-FileCtr    AS INT
                    FIELD tt-FileName   AS CHAR
                    INDEX filelist      IS PRIMARY 
                          TT-FILECTR.

DEF TEMP-TABLE tt-ci-form NO-UNDO
    FIELD form-name AS CHAR
    FIELD total-pallets LIKE oe-bolh.tot-pallets
    INDEX tt-ci-form form-name ASC.

/* Output selection for the report */
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_cust end_cust begin_bol# ~
end_bol# begin_ord# end_ord# tb_reprint tb_pallet tb_posted ~
tb_print-component tb_print-shipnote rd_bolcert rd-dest tb_EMailAdvNotice ~
tb_MailBatchMode tb_ComInvoice lv-ornt lines-per-page lv-font-no ~
tb_post-bol td-show-parm btn-cancel btn-ok 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_bol# end_bol# ~
begin_ord# end_ord# tb_reprint tb_pallet tb_posted tb_print-component ~
tb_print-shipnote lbl_bolcert rd_bolcert rd-dest tb_EMailAdvNotice ~
tb_MailBatchMode tb_ComInvoice lv-ornt lines-per-page lv-font-no ~
lv-font-name tb_post-bol td-show-parm 

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

DEFINE VARIABLE begin_bol# AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Beginning BOL#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_ord# AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_bol# AS INTEGER FORMAT ">>>>>>>9" INITIAL 99999999 
     LABEL "Ending BOL#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_ord# AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_bolcert AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 47.6 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd_bolcert AS CHARACTER INITIAL "BOL" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "BOL", "BOL",
"Certificate of Compliance", "Certificate of Compliance"
     SIZE 39 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 92 BY 9.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 11.43.

DEFINE VARIABLE tb_ComInvoice AS LOGICAL INITIAL no 
     LABEL "Commercial Invoice (Excel)?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE tb_EMailAdvNotice AS LOGICAL INITIAL no 
     LABEL "E-Mail &Advanced Ship Notice?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tb_MailBatchMode AS LOGICAL INITIAL no 
     LABEL "Hide E-Mail Dialog Box" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .81 NO-UNDO.

DEFINE VARIABLE tb_pallet AS LOGICAL INITIAL no 
     LABEL "Print Number Of Pallets?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tb_post-bol AS LOGICAL INITIAL no 
     LABEL "Post BOL?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_posted AS LOGICAL INITIAL no 
     LABEL "Reprint Posted BOL?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-component AS LOGICAL INITIAL no 
     LABEL "Print Assembled Components?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-shipnote AS LOGICAL INITIAL no 
     LABEL "Print Ship Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE tb_reprint AS LOGICAL INITIAL no 
     LABEL "Reprint Bill Of Ladings?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust AT ROW 2.43 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 2.43 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_bol# AT ROW 3.38 COL 26 COLON-ALIGNED HELP
          "Enter Beginning BOL Number"
     end_bol# AT ROW 3.38 COL 69 COLON-ALIGNED HELP
          "Enter Ending BOL Number"
     begin_ord# AT ROW 4.33 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_ord# AT ROW 4.33 COL 69 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     tb_reprint AT ROW 5.76 COL 34
     tb_pallet AT ROW 6.71 COL 62 RIGHT-ALIGNED
     tb_posted AT ROW 7.67 COL 34
     tb_print-component AT ROW 8.62 COL 34
     tb_print-shipnote AT ROW 9.57 COL 34
     lbl_bolcert AT ROW 11 COL 23.4 COLON-ALIGNED NO-LABEL
     rd_bolcert AT ROW 11 COL 34 NO-LABEL
     rd-dest AT ROW 13.91 COL 5 NO-LABEL
     tb_EMailAdvNotice AT ROW 14.33 COL 67 RIGHT-ALIGNED
     tb_MailBatchMode AT ROW 15.29 COL 77 RIGHT-ALIGNED
     tb_ComInvoice AT ROW 16.43 COL 64 RIGHT-ALIGNED
     lv-ornt AT ROW 17.52 COL 34 NO-LABEL
     lines-per-page AT ROW 17.52 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 19.05 COL 32 COLON-ALIGNED
     lv-font-name AT ROW 19.05 COL 39.4 COLON-ALIGNED NO-LABEL
     tb_post-bol AT ROW 20.52 COL 69.8
     td-show-parm AT ROW 20.57 COL 34
     btn-cancel AT ROW 22.91 COL 61
     btn-ok AT ROW 22.95 COL 20
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.95 COL 5
     RECT-6 AT ROW 12.71 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 23.95.


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
         TITLE              = "Print Bills of Lading"
         HEIGHT             = 24.29
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("images/progress.ico":U) THEN
    MESSAGE "Unable to load icon: images/progress.ico"
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
       begin_bol#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_bol#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_bolcert IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_bolcert:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_bolcert".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lv-font-name:READ-ONLY IN FRAME FRAME-A        = TRUE.

ASSIGN 
       rd_bolcert:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_ComInvoice IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_ComInvoice:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_EMailAdvNotice IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_EMailAdvNotice:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_MailBatchMode IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_MailBatchMode:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_pallet IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_pallet:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_post-bol:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_posted:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_print-component:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_print-shipnote:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_reprint:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Print Bills of Lading */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Print Bills of Lading */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bol#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bol# C-Win
ON VALUE-CHANGED OF begin_bol# IN FRAME FRAME-A /* Beginning BOL# */
DO:
  RUN new-bol#.
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


&Scoped-define SELF-NAME begin_ord#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord# C-Win
ON LEAVE OF begin_ord# IN FRAME FRAME-A /* Beginning Order# */
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
  DEF VAR retCode     AS INT NO-UNDO.  
  DEF VAR ll          AS LOG NO-UNDO.
  
  /* Initilize temp-table */
  EMPTY TEMP-TABLE tt-filelist.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

/*   IF tb_posted AND rd-dest NE 5 THEN do:                     */
/*                                                              */
/*      ASSIGN tb_reprint = YES                                 */
/*             END_bol#:SCREEN-VALUE = begin_bol#:SCREEN-VALUE. */
/*      DISP tb_reprint.                                        */
/*   END.                                                       */
  
  ASSIGN 
    init-dir    = "C:\tmp\"
    lv-pdf-file = IF rd-dest = 5  THEN init-dir + "BOL"
                                  ELSE init-dir + "\BOL" + string(begin_bol#).
  
  IF      rd-dest = 1  THEN ASSIGN LvOutputSelection = "Printer".
  ELSE IF rd-dest = 2  THEN ASSIGN LvOutputSelection = "Screen". 
  ELSE IF rd-dest = 3  THEN ASSIGN LvOutputSelection = "File". 
  ELSE IF rd-dest = 4  THEN ASSIGN LvOutputSelection = "Fax". 
  ELSE IF rd-dest = 5  THEN ASSIGN LvOutputSelection = "Email".
  ELSE IF rd-dest = 6  THEN ASSIGN LvOutputSelection = "Port".
      
  IF NOT  rd-dest = 5  THEN RUN run-report. 

  SESSION:SET-WAIT-STATE ("").
  
  IF v-print-fmt <> "SouthPak-XL" THEN
  DO :
    case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then run output-to-fax.
       WHEN 6 THEN RUN output-to-port.
     end case.
  END.
  
  if rd-dest = 5 then RUN output-to-mail.

  IF lv-run-commercial = "YES" AND NOT IS-xprint-form THEN 
     RUN run-report-ci.

  IF tb_ComInvoice:CHECKED THEN
    RUN CommercialInvoice.

  EMPTY TEMP-TABLE tt-fg-bin.

  ll = tb_post-bol AND NOT tb_posted.

  IF ll AND oe-ctrl.u-inv AND v-check-qty THEN
   FOR EACH tt-post,
      FIRST oe-bolh NO-LOCK 
      WHERE ROWID(oe-bolh) EQ tt-post.row-id:

    RUN oe/bolcheck.p (ROWID(oe-bolh)).

    IF CAN-FIND (FIRST w-except 
                 WHERE w-except.bol-no EQ oe-bolh.bol-no) THEN DO:

      MESSAGE "BOL has insufficient inventory and cannot be posted..."
          VIEW-AS ALERT-BOX ERROR.
      ll = NO.
      LEAVE.
    END.
   END.

   IF ll THEN DO:
     ll = NO.
     MESSAGE "Post BOL?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE ll.
   END.

   IF ll THEN do:
     RUN post-bol.
     MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_bol#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bol# C-Win
ON VALUE-CHANGED OF end_bol# IN FRAME FRAME-A /* Ending BOL# */
DO:
  RUN new-bol#.
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


&Scoped-define SELF-NAME end_ord#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord# C-Win
ON LEAVE OF end_ord# IN FRAME FRAME-A /* Ending Order# */
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
    DEF VAR char-val AS char no-undo.

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
  if int (self:screen-value) = 5 then do:
    assign tb_MailBatchMode:sensitive  = true.
    apply 'value-changed':u to tb_MailBatchMode.
  end.
    
  ELSE DO:
  
    assign tb_MailBatchMode:sensitive  = false.

    APPLY 'VALUE-CHANGED':U TO tb_EmailAdvNotice.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_bolcert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_bolcert C-Win
ON VALUE-CHANGED OF rd_bolcert IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_EMailAdvNotice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_EMailAdvNotice C-Win
ON VALUE-CHANGED OF tb_EMailAdvNotice IN FRAME FRAME-A /* E-Mail Advanced Ship Notice? */
DO:

  IF NOT rd-dest:SCREEN-VALUE EQ '5' THEN DO:
  
    IF SELF:CHECKED THEN DO:
      ASSIGN tb_MailBatchMode:sensitive  = true.
      APPLY 'value-changed':u to tb_MailBatchMode.
    end.
      
    ELSE
      assign  tb_MailBatchMode:sensitive  = false.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_pallet C-Win
ON VALUE-CHANGED OF tb_pallet IN FRAME FRAME-A /* Print Number Of Pallets? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_post-bol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_post-bol C-Win
ON VALUE-CHANGED OF tb_post-bol IN FRAME FRAME-A /* Post BOL? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_posted
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_posted C-Win
ON VALUE-CHANGED OF tb_posted IN FRAME FRAME-A /* Reprint Posted BOL? */
DO:
  assign {&self-name}.
  IF tb_posted THEN do:
     ASSIGN tb_reprint = YES
            END_bol#:SCREEN-VALUE = begin_bol#:SCREEN-VALUE
          /*  END_ord#:SCREEN-VALUE = begin_ord#:SCREEN-VALUE
            END_cust:SCREEN-VALUE = begin_cust:SCREEN-VALUE*/.
     DISP tb_reprint     WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reprint C-Win
ON VALUE-CHANGED OF tb_reprint IN FRAME FRAME-A /* Reprint Bill Of Ladings? */
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
DEF VAR choice AS LOG NO-UNDO.

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

  find first company where company.company eq cocode no-lock no-error.

  find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

  find first sys-ctrl
       where sys-ctrl.company eq cocode
         and sys-ctrl.name    eq "BOLPRINT"
      no-lock no-error.

  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "BOLPRINT"
     sys-ctrl.descrip = "Print Bill of Lading Headers on Bill of Lading Form?".
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  end.
  v-print-hdgs = sys-ctrl.log-fld.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "BOLFMT"
      NO-LOCK no-error.
  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "BOLFMT"
     sys-ctrl.descrip  = "Bill of lading format"
     sys-ctrl.char-fld = "ASI".
    message "System control record not found. Update BOL Print format"
    update sys-ctrl.char-fld.
  end.
  assign
   v-print-fmt = sys-ctrl.char-fld
   v-headers   = sys-ctrl.log-fld.

  find first sys-ctrl
       where sys-ctrl.company eq cocode
         and sys-ctrl.name    eq "BOLCERT"
      no-lock no-error.
  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "BOLCERT"
     sys-ctrl.descrip  = "Print Certificate of Compliance forms?"
     sys-ctrl.log-fld  = no.
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  end.
  assign
   v-print-coc = sys-ctrl.log-fld
   v-coc-fmt   = sys-ctrl.char-fld. 

  find first sys-ctrl
       where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "BOLPOST"
      no-lock no-error.

  if not avail sys-ctrl then do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "BOLPOST"
     sys-ctrl.descrip = "Post BOL if BOL Qty > Bin Qty"
     choice           = yes.
   
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE choice.
  
    if not choice then sys-ctrl.char-fld eq "Bin>Qty".
  end.
  v-check-qty = sys-ctrl.char-fld eq "Bin>Qty".
  
  IF NOT CAN-FIND (FIRST sys-ctrl 
                   WHERE sys-ctrl.company = cocode
                   AND sys-ctrl.NAME    = 'CINVOICE') THEN
    DO TRANSACTION:
      CREATE sys-ctrl.
      ASSIGN sys-ctrl.company = cocode
             sys-ctrl.NAME    = 'CINVOICE'
             sys-ctrl.descrip = 'Commercial Invoice Forms'.
      RELEASE sys-ctrl.
   END.

  IF v-print-fmt EQ "1/2 page" THEN
    ASSIGN
     v-program      = "oe/rep/bolhalfp.p"
     lines-per-page = 44.
 
  ELSE
  IF v-print-fmt EQ "Royal" THEN
    ASSIGN
     v-program      = "oe/rep/bolroyal.p"
     lines-per-page = 56.

  ELSE
  IF v-print-fmt EQ "ContSrvc" THEN
    ASSIGN
     v-program      = "oe/rep/bol-csc.p"
     lines-per-page = 66.

  ELSE
  IF v-print-fmt EQ "HOP" THEN
    ASSIGN
     v-program      = "oe/rep/bolhop.p"
     lines-per-page = 39.

  ELSE
  IF v-print-fmt EQ "Superior" THEN
    ASSIGN
     v-program      = "oe/rep/bolsuper.p"
     lines-per-page = 55.

  ELSE
  IF v-print-fmt EQ "Premier" THEN
    ASSIGN
     v-program      = "oe/rep/bolprem.p"
     lines-per-page = 55.

  ELSE
  IF v-print-fmt EQ "PremierX" THEN
      ASSIGN
       v-program      = "oe/rep/bolpremx.p"
       is-xprint-form = YES
       lines-per-page = 80.

  ELSE
  IF v-print-fmt EQ "RFC" THEN
    ASSIGN
     v-program      = "oe/rep/bolrfc.p"
     lines-per-page = 55.

  ELSE
  IF v-print-fmt EQ "Sonoco" THEN
    ASSIGN
     v-program      = "oe/rep/bolsonoc.p"
     lines-per-page = 51.

  ELSE
  IF v-print-fmt EQ "Warren" THEN
    ASSIGN
     v-program      = "oe/rep/bolwarrn.p"
     lines-per-page = 59.

  ELSE
  IF v-print-fmt EQ "PAC 1/2" THEN
    ASSIGN
     v-program      = "oe/rep/bolpack.p"
     lines-per-page = 44.

  ELSE
  IF v-print-fmt EQ "Imperial" THEN
    ASSIGN
     v-program      = "oe/rep/bolimp.p"
     lines-per-page = 60.

  ELSE
  IF v-print-fmt EQ "P&P" THEN
    ASSIGN
     v-program      = "oe/rep/bolpnp.p"
     lines-per-page = 62.
  ELSE
  IF v-print-fmt EQ "TriLakes" THEN
    ASSIGN
     v-program      = "oe/rep/boltril.p"
     lines-per-page = 62.

  ELSE
  IF v-print-fmt EQ "Triad" THEN
    ASSIGN
     v-program      = "oe/rep/boltriad.p"
     lines-per-page = 62.

  ELSE
  IF v-print-fmt EQ "TriState" THEN
    ASSIGN
     v-program      = "oe/rep/boltrist.p"
     lines-per-page = 41.

  ELSE
  IF v-print-fmt EQ "BlueRidg" THEN
    ASSIGN
     v-program      = "oe/rep/bolbluer.p"
     lines-per-page = 65.

  ELSE
  IF v-print-fmt EQ "Danbury" THEN
    ASSIGN
     v-program      = "oe/rep/boldnbry.p"
     lines-per-page = 42.

  ELSE
  IF v-print-fmt EQ "Boxtech" THEN
    ASSIGN
     v-program      = "oe/rep/bolboxt.p"
     lines-per-page = 55
     lv-prt-bypass = YES .

  ELSE
  IF v-print-fmt EQ "Empire" THEN
    ASSIGN
     v-program      = "oe/rep/bolempir.p"
     lines-per-page = 54.

  ELSE
  IF v-print-fmt EQ "Herman" THEN
    ASSIGN
     v-program      = "oe/rep/bolhermn.p"
     lines-per-page = 54.

  ELSE
  IF v-print-fmt EQ "pacific" THEN
    ASSIGN
     v-program      = "oe/rep/bolpacif.p"
     is-xprint-form = YES
     lines-per-page = 80.

  ELSE
  IF v-print-fmt EQ "century" THEN
    ASSIGN
     v-program      = "oe/rep/bolcentx.p"
     is-xprint-form = YES
     lines-per-page = 64.

  ELSE
  IF v-print-fmt EQ "ccc" THEN
    ASSIGN
     v-program      = "oe/rep/bolccc.p"
     is-xprint-form = YES
     lines-per-page = 64.

  ELSE  
  IF v-print-fmt EQ "Xprint" THEN
      ASSIGN
       v-program      = "oe/rep/bolxprnt.p"
       is-xprint-form = YES
       lines-per-page = 66.

  ELSE
  IF v-print-fmt EQ "FibreCI" THEN
      ASSIGN
       v-program      = "oe/rep/bolfibci.p"
       is-xprint-form = YES
       lines-per-page = 66.

  ELSE
  IF v-print-fmt EQ "APC" THEN
    ASSIGN
     v-program      = "oe/rep/bolxapc.p"
     is-xprint-form = YES
     lines-per-page = 66.

  ELSE
  IF v-print-fmt EQ "P&PX" THEN
      ASSIGN
       v-program      = "oe/rep/bolpnpx.p"
       is-xprint-form = YES
       lines-per-page = 66.

  ELSE
  IF v-print-fmt EQ "Xprint2" THEN
      ASSIGN
       v-program      = "oe/rep/bolxprt2.p"
       is-xprint-form = YES
       lines-per-page = 66.

  ELSE
  IF v-print-fmt EQ "CSCIN" THEN
      ASSIGN
       v-program      = "oe/rep/bolcscin.p"
       is-xprint-form = YES
       lines-per-page = 66.

  ELSE
  IF v-print-fmt EQ "SouthPak" THEN
  DO:
    ASSIGN
       v-program      = "oe/rep/bolsouth.p"
       is-xprint-form = YES
       lines-per-page = 66.
  END.
  ELSE
  IF v-print-fmt EQ "SouthPak-XL" THEN
  DO:
    ASSIGN
       v-program      = "oe/rep/bolsouth-xl.p"
       is-xprint-form = YES
       lines-per-page = 66.
  END.
  ELSE
  IF v-print-fmt EQ "MWBox" THEN
      ASSIGN
       v-program      = "oe/rep/bolmwbox.p"
       is-xprint-form = YES
       lines-per-page = 66.

  ELSE
  IF v-print-fmt EQ "Hughes" THEN
      ASSIGN
       v-program      = "oe/rep/bolhughs.p"
       is-xprint-form = YES
       lines-per-page = 64.

  ELSE
  IF v-print-fmt EQ "Inland" THEN
      ASSIGN
       v-program      = "oe/rep/bolxinld.p"
       is-xprint-form = YES
       lines-per-page = 80.

  ELSE
  IF v-print-fmt EQ "concepts" THEN
      ASSIGN
       v-program      = "oe/rep/bolxcorc.p"
       is-xprint-form = YES
       lines-per-page = 80.
  
  ELSE
  IF v-print-fmt EQ "Brick" THEN
    IF v-headers THEN
      ASSIGN
       v-program      = "oe/rep/bolbrck1.p"
       lines-per-page = 64.
      
    ELSE
      ASSIGN
       v-program      = "oe/rep/bolbrick.p"
       lines-per-page = 60.
      
  ELSE
  IF v-print-fmt EQ "AllPkg" THEN
    ASSIGN
     v-program      = "oe/rep/bolallpk.p"
     lines-per-page = 60.

  ELSE
  IF v-print-fmt EQ "Fibre" THEN
    ASSIGN
     v-program      = "oe/rep/bolfibre.p"
     lines-per-page = 57.
    
  ELSE
  IF v-print-fmt EQ "MaxPak" THEN
    ASSIGN
     v-program      = "oe/rep/bolmaxpk.p"
     lines-per-page = 42.

  /*  
  ELSE
  IF v-print-fmt EQ "Oracle" THEN
    ASSIGN
     v-program      = "oe/rep/boloracl.p"
     lines-per-page = 62.
  */

  ELSE
  IF v-print-fmt EQ "Oracle" THEN
    ASSIGN
     v-program      = "oe/rep/bolora2.p"
     is-xprint-form = YES
     lines-per-page = 64.

  ELSE
  IF v-print-fmt EQ "Frankstn" OR v-print-fmt = "Mirpkg" THEN
    ASSIGN
     v-program      = "oe/rep/bolfrank.p"
     is-xprint-form = YES
     lines-per-page = 64.

  ELSE
  IF v-print-fmt EQ "PPI" THEN
    ASSIGN
     v-program      = "oe/rep/bolppi.p"
     is-xprint-form = YES
     lines-per-page = 64.

  ELSE
  IF v-print-fmt EQ "Indiana" THEN
    ASSIGN
     v-program      = "oe/rep/bolindc.p"
     is-xprint-form = YES
     lines-per-page = 64.

  ELSE
  IF v-print-fmt EQ "Ottpkg" THEN
    ASSIGN
     v-program      = "oe/rep/bolottpk.p"
     is-xprint-form = YES
     lines-per-page = 64.

  ELSE
  IF v-print-fmt EQ "ConsBox" THEN
    ASSIGN
     v-program      = "oe/rep/bolcnsbx.p"
     is-xprint-form = YES
     lines-per-page = 64.

  ELSE
  IF v-print-fmt EQ "Harwell" THEN
    ASSIGN
     v-program      = "oe/rep/bolharwl.p"
     lines-per-page = 62.

 /* old non xprint format   
  ELSE
  IF v-print-fmt EQ "Inland" THEN
    ASSIGN
     v-program      = "oe/rep/bolinlnd.p"
     lines-per-page = 56.
 */
  ELSE
  IF v-print-fmt EQ "Chillic" THEN
    ASSIGN 
     v-program      = "oe/rep/bolchill.p"
     lines-per-page = 60.

  ELSE
  IF v-print-fmt EQ "Midwest" THEN
    ASSIGN 
     v-program      = "oe/rep/bolmdwst.p"
     lines-per-page = 52.

  ELSE
  IF v-print-fmt EQ "Intrpack" THEN
    ASSIGN 
     v-program      = "oe/rep/bolinter.p"
     lines-per-page = 56.

  ELSE
  IF v-print-fmt EQ "Dayton" THEN
    ASSIGN
     v-program      = "oe/rep/boldaytn.p"
     lines-per-page = 57.

  ELSE
  IF v-print-fmt EQ "Elite" THEN
    ASSIGN
     v-program      = "oe/rep/bolelite.p"
     is-xprint-form = YES
     lines-per-page = 66.

  ELSE
  IF v-print-fmt EQ "Michcor" THEN
    ASSIGN
     v-program      = "oe/rep/bolmich.p"
     is-xprint-form = YES
     lines-per-page = 66.

  ELSE
  IF v-print-fmt EQ "Express" THEN
    ASSIGN
     v-program      = "oe/rep/bolexprs.p"
     lines-per-page = 62.

  ELSE
  IF v-print-fmt EQ "Hamilton" THEN
    ASSIGN
     v-program      = "oe/rep/bolhamil.p"
     lines-per-page = 44.

  ELSE
    ASSIGN
     v-print-mode   = "PROD"
     v-program      = "oe/rep/oe-lad" +
                      (if v-print-fmt eq "c" then "c" else "s") + ".p"
     lines-per-page = 62.

  v-lines-per-page = lines-per-page.

  RUN enable_UI.
  
  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:

    {custom/usrprint.i}
    
    APPLY "entry" TO begin_cust.

    lines-per-page:SCREEN-VALUE = STRING(v-lines-per-page).
    DISABLE lines-per-page.

    IF NOT PROGRAM-NAME(1) BEGINS "listobjs/oe-boll_." THEN
      ASSIGN
       tb_post-bol:SCREEN-VALUE = "no"
       tb_post-bol:HIDDEN       = YES.

    IF LOOKUP(v-print-fmt,"SouthPak,Xprint,Xprint2,Frankstn,Fibre,Ottpkg,Consbox,ContSrvc") LE 0 THEN DO:
      tb_print-component:SCREEN-VALUE = "no".
      DISABLE tb_print-component.
    END.
    IF v-print-fmt = "Xprint" THEN tb_print-shipnote:SENSITIVE = YES.
    ELSE tb_print-shipnote:SENSITIVE = NO.

    RUN new-bol#.

    tb_EMailAdvNotice:SENSITIVE = YES.
    APPLY 'value-changed':u TO rd-dest.
  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AdvancedNotice C-Win 
PROCEDURE AdvancedNotice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE BUFFER   b1-cust       FOR cust.
  DEFINE BUFFER   b1-oe-bolh    FOR oe-bolh.
  DEFINE BUFFER   b1-oe-boll    FOR oe-boll.
  DEFINE BUFFER   b1-in-house-cust FOR cust.
  DEFINE BUFFER   bl-phone      FOR phone.
  DEFINE BUFFER   bl-emaildtl   FOR emaildtl.

  DEFINE VARIABLE vcMailMode    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vlSkipRec     AS LOGICAL    NO-UNDO.

  assign
    v-s-cust            = begin_cust
    v-e-cust            = end_cust
    v-s-bol             = begin_bol#
    v-e-bol             = end_bol#
    v-s-ord             = begin_ord#
    v-e-ord             = end_ord#
    v-printed           = tb_reprint
    v-print-pal         = tb_pallet
    v-print-bol         = rd_bolcert EQ "BOL"
    v-print-components  = tb_print-component
    v-print-shipnotes   = tb_print-shipnote
    lv-run-bol          = ""
    lv-run-commercial   = "".

  FOR EACH b1-cust NO-LOCK
     WHERE b1-cust.company EQ cocode
       AND b1-cust.cust-no GE begin_cust
       AND b1-cust.cust-no LE end_cust,
     
      EACH b1-shipto NO-LOCK OF b1-cust,
    
     FIRST b1-oe-bolh NO-LOCK
     where b1-oe-bolh.company eq cocode
       and b1-oe-bolh.bol-no  ge v-s-bol
       and b1-oe-bolh.bol-no  le v-e-bol
       and b1-oe-bolh.cust-no EQ b1-cust.cust-no
       and b1-oe-bolh.printed eq YES
       and b1-oe-bolh.posted  eq tb_posted
       and can-find (FIRST b1-oe-boll
                     WHERE b1-oe-boll.company EQ b1-oe-bolh.company
                       AND b1-oe-boll.b-no    EQ b1-oe-bolh.b-no
                       AND b1-oe-boll.ord-no  GE v-s-ord
                       AND b1-oe-boll.ord-no  LE v-e-ord)
    USE-INDEX post
    BREAK BY b1-cust.cust-no
          BY b1-shipto.ship-id:

    IF FIRST-OF (b1-shipto.ship-id) THEN DO:

      STATUS DEFAULT 'Processing SHIPTO Contacts for: ' + b1-shipto.ship-id + '....'.

      ASSIGN
          vlSkipRec = YES
          vcBOLNums   = '' 
          lv-pdf-file = init-dir + '\BOL'
          vcMailMode  = if tb_MailBatchMode then 'ShipTo1'  /* Silent Mode */
                                            else 'ShipTo'.  /* Dialog Box */

      FOR EACH bl-phone WHERE
          bl-phone.table_rec_key EQ b1-shipto.rec_key
          NO-LOCK:

          IF CAN-FIND(FIRST bl-emaildtl WHERE
             bl-emaildtl.emailcod EQ 'r-bolprt.' AND
             bl-emaildtl.table_rec_key EQ bl-phone.rec_key) THEN
             DO:
                vlSkipRec = NO.
                LEAVE.
             END.
      END.

      IF NOT vlSkipRec THEN
         RUN GenerateMail (INPUT b1-cust.cust-no, 
                           INPUT b1-shipto.rec_key,
                           INPUT 2,
                           INPUT vcMailMode).
    END.
  END. /* each cust */

  /*in-house cust shiptos*/
  FOR EACH b1-cust NO-LOCK
     WHERE b1-cust.company EQ cocode
       AND b1-cust.cust-no GE begin_cust
       AND b1-cust.cust-no LE end_cust,
     FIRST b1-oe-bolh NO-LOCK
     where b1-oe-bolh.company eq cocode
       and b1-oe-bolh.bol-no  ge v-s-bol
       and b1-oe-bolh.bol-no  le v-e-bol
       and b1-oe-bolh.cust-no EQ b1-cust.cust-no
       and b1-oe-bolh.printed eq YES
       and b1-oe-bolh.posted  eq tb_posted
       and can-find (FIRST b1-oe-boll
                     WHERE b1-oe-boll.company EQ b1-oe-bolh.company
                       AND b1-oe-boll.b-no    EQ b1-oe-bolh.b-no
                       AND b1-oe-boll.ord-no  GE v-s-ord
                       AND b1-oe-boll.ord-no  LE v-e-ord)
      USE-INDEX post,
      EACH b1-in-house-cust WHERE
           b1-in-house-cust.company EQ cocode AND
           b1-in-house-cust.active  EQ "X"
           NO-LOCK,
      FIRST b1-shipto NO-LOCK WHERE
            b1-shipto.company EQ cocode AND
            b1-shipto.cust-no EQ b1-in-house-cust.cust-no AND
            b1-shipto.ship-id EQ b1-oe-bolh.ship-id
    BREAK BY b1-cust.cust-no
          BY b1-shipto.ship-id:

    IF FIRST-OF (b1-shipto.ship-id) THEN DO:

      STATUS DEFAULT 'Processing SHIPTO Contacts for: ' + b1-shipto.ship-id + '....'.

      ASSIGN
          vlSkipRec = YES
          vcBOLNums   = '' 
          lv-pdf-file = init-dir + '\BOL'
          vcMailMode  = if tb_MailBatchMode then 'ShipTo1'  /* Silent Mode */
                                            else 'ShipTo'.  /* Dialog Box */

      FOR EACH bl-phone WHERE
          bl-phone.table_rec_key EQ b1-shipto.rec_key
          NO-LOCK:

          IF CAN-FIND(FIRST bl-emaildtl WHERE
             bl-emaildtl.emailcod EQ 'r-bolprt.' AND
             bl-emaildtl.table_rec_key EQ bl-phone.rec_key) THEN
             DO:
                vlSkipRec = NO.
                LEAVE.
             END.
      END.

      IF NOT vlSkipRec THEN
         RUN GenerateMail (INPUT b1-cust.cust-no, 
                           INPUT b1-shipto.rec_key,
                           INPUT 2,
                           INPUT vcMailMode).
    END.
  END. /* each cust */

  if tb_MailBatchMode then
    MESSAGE "Your E-Mails have been sent in Silent Mode."  skip
            "Please verify transmission in your SENT folder."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  status default 'Enter data or ESC to end.'.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  
  SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ASIMail C-Win 
PROCEDURE ASIMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  def input param icMailMode  as char no-undo.
  def input param icCustNo    as char no-undo.
  def input param icSubBody   as char no-undo.

  {custom/asimail2.i  &TYPE           = value (icMailMode)
                      &group-title    = 'r-bolprt.'
                      &begin_cust     = icCustNo
                      &END_cust       = icCustNo
                      &mail-subject   = icSubBody
                      &mail-body      = icSubBody
                      &mail-file      = lv-pdf-file}
                      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-work C-Win 
PROCEDURE build-work :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAM ic2ndKey  AS CHAR NO-UNDO.

  EMPTY TEMP-TABLE tt-post.
  
  build-work:
  FOR EACH oe-bolh
     WHERE oe-bolh.company EQ cocode
       AND oe-bolh.bol-no  GE v-s-bol
       AND oe-bolh.bol-no  LE v-e-bol
       AND oe-bolh.cust-no GE v-s-cust
       AND oe-bolh.cust-no LE v-e-cust 
       AND oe-bolh.printed EQ v-printed
       AND oe-bolh.posted  EQ tb_posted
       AND CAN-FIND (FIRST oe-boll
                     WHERE oe-boll.company EQ oe-bolh.company
                       AND oe-boll.b-no    EQ oe-bolh.b-no
                       AND oe-boll.ord-no  GE v-s-ord
                       AND oe-boll.ord-no  LE v-e-ord)
      USE-INDEX post:
  
    IF NOT oe-ctrl.p-bol THEN
    FOR EACH oe-boll
       WHERE oe-boll.company EQ oe-bolh.company
         AND oe-boll.bol-no  EQ oe-bolh.bol-no
         AND CAN-FIND(FIRST oe-ord
                      WHERE oe-ord.company EQ oe-boll.company
                        AND oe-ord.ord-no  EQ oe-boll.ord-no
                        AND oe-ord.stat    EQ "H")
        NO-LOCK:
      NEXT build-work.
    END.
  
    /* update loadtag status - Bill of lading task#: 10190414 */
    IF NOT oe-bolh.printed THEN
    FOR EACH bf-oe-boll NO-LOCK
       WHERE bf-oe-boll.company EQ oe-bolh.company 
         AND bf-oe-boll.b-no    EQ oe-bolh.b-no
         AND bf-oe-boll.tag     NE "",
       FIRST loadtag
       WHERE loadtag.company   EQ bf-oe-boll.company
         AND loadtag.item-type EQ NO
         AND loadtag.tag-no    EQ bf-oe-boll.tag
   USE-INDEX tag:
      loadtag.sts = "Bill of Lading".
    END.
  
    IF ic2ndKey NE ? AND ic2ndKey NE '' THEN DO:
      FIND FIRST shipto NO-LOCK
           WHERE shipto.rec_key = ic2ndKey 
             AND shipto.ship-id = oe-bolh.ship-id NO-ERROR.
      IF NOT AVAIL shipto THEN NEXT build-work.
    END.

    FIND FIRST sys-ctrl-shipto WHERE
         sys-ctrl-shipto.company      EQ oe-bolh.company AND
         sys-ctrl-shipto.name         EQ "BOLFMT" AND
         sys-ctrl-shipto.cust-vend    EQ YES AND
         sys-ctrl-shipto.cust-vend-no EQ oe-bolh.cust-no AND
         sys-ctrl-shipto.ship-id      EQ oe-bolh.ship-id
         NO-LOCK NO-ERROR.

    CREATE report.
    
    ASSIGN 
        report.term-id  = v-term-id
        report.key-01   = oe-bolh.cust-no
        report.key-02   = oe-bolh.ship-id
        report.rec-id   = RECID(oe-bolh)
        report.key-09   = STRING(oe-bolh.printed,"REVISED/ORIGINAL")
        oe-bolh.printed = YES
        report.key-03   = IF AVAIL sys-ctrl-shipto AND  NOT sys-ctrl-shipto.log-fld THEN "C" /*commercial invoice only*/
                          ELSE IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN "B" /*commercial invoice and bol both*/
                          ELSE "N" /*BOL only*/ 
        report.key-04   = IF AVAIL sys-ctrl-shipto THEN sys-ctrl-shipto.char-fld ELSE ""
        vcBOLNums       = vcBOLNums + '-' + STRING (oe-bolh.bol-no)
        vcBOLNums       = LEFT-TRIM (vcBOLNums, '-').

    IF vcBOLNums MATCHES '*-*' THEN DO:
        vcBOLNums       = RIGHT-TRIM (SUBSTRING (vcBOLNums, 1, INDEX (vcBOLNums,'-')), '-') + SUBSTRING (vcBOLNums, R-INDEX (vcBOLNums, '-')).
    END.

    status default 'Now Processing BOL: ' + string (oe-bolh.bol-no) + '....'.
  
    IF lv-run-bol        = "" AND report.key-03 <> "C" THEN lv-run-bol = "YES" .
    IF lv-run-commercial = "" AND report.key-03 <> "N" THEN lv-run-commercial = "YES".
    
    CREATE tt-post.
    tt-post.row-id = ROWID(oe-bolh).      
  END.
  
  v-lines-per-page = lines-per-page.

  status default ''.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CommercialInvoice C-Win 
PROCEDURE CommercialInvoice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF BUFFER  bf-oe-boll          FOR oe-boll.
  DEF VAR     lv-run-bol          AS char NO-UNDO.
  DEF VAR     lv-run-commercial   AS char no-undo.
  DEF VAR     v-tmp-is-xprint     AS LOG  NO-UNDO.
  DEF VAR     viCount             AS INT  NO-UNDO.

  {sys/form/r-top.i}
  
  assign
    v-s-cust            = begin_cust
    v-e-cust            = end_cust
    v-s-bol             = begin_bol#
    v-e-bol             = end_bol#
    v-s-ord             = begin_ord#
    v-e-ord             = end_ord#
    v-printed           = tb_reprint
    v-print-pal         = tb_pallet
    v-print-bol         = rd_bolcert EQ "BOL"
    v-print-components  = tb_print-component
    v-print-shipnotes   = tb_print-shipnote
    lv-run-bol          = ""
    lv-run-commercial   = ""
    v-tmp-is-xprint     = IS-xprint-form
    is-xprint-form      = YES.
  
  IF NOT v-print-bol THEN DO:
  
     IF v-coc-fmt = "Xprint" THEN 
       ASSIGN is-xprint-form  = YES
              v-program       = "oe/rep/cocxprnt.p".
     ELSE 
       ASSIGN is-xprint-form  = NO
              v-program       = "oe/rep/cocbrick.p".
  END.
  
  {sys/inc/print1.i}
  
  {sys/inc/outprint.i value(lines-per-page)}
  
  if td-show-parm then run show-param.
  
  SESSION:SET-WAIT-STATE ("general").
  
  {sa/sa-sls01.i}
  
  v-term-id = v-term.

  EMPTY TEMP-TABLE tt-ci-form.

  FOR EACH b1-oe-bolh WHERE
      b1-oe-bolh.company eq cocode AND
      b1-oe-bolh.bol-no  ge v-s-bol AND
      b1-oe-bolh.bol-no  le v-e-bol AND
      b1-oe-bolh.printed eq v-printed AND
      b1-oe-bolh.posted  eq tb_posted AND
      can-find (FIRST b1-oe-boll WHERE
                      b1-oe-boll.company EQ b1-oe-bolh.company AND
                      b1-oe-boll.b-no    EQ b1-oe-bolh.b-no AND
                      b1-oe-boll.ord-no  GE v-s-ord AND
                      b1-oe-boll.ord-no  LE v-e-ord)
      NO-LOCK,
      FIRST sys-ctrl-shipto WHERE
            sys-ctrl-shipto.company      EQ cocode AND
            sys-ctrl-shipto.name         EQ "CINVOICE" AND
            sys-ctrl-shipto.cust-vend    EQ YES AND
            sys-ctrl-shipto.cust-vend-no EQ b1-oe-bolh.cust-no AND
            sys-ctrl-shipto.ship-id      EQ b1-oe-bolh.ship-id
            NO-LOCK,
      FIRST b1-cust WHERE
            b1-cust.company EQ cocode AND
            b1-cust.cust-no GE v-s-cust AND
            b1-cust.cust-no LE v-e-cust
            NO-LOCK:

      FIND FIRST tt-ci-form WHERE
           tt-ci-form.form-name = sys-ctrl-shipto.char-fld
           NO-ERROR.

      IF NOT AVAIL tt-ci-form THEN
      DO:
         CREATE tt-ci-form.
         ASSIGN tt-ci-form.form-name = sys-ctrl-shipto.char-fld.
      END.

      tt-ci-form.total-pallets = tt-ci-form.total-pallets
                               + b1-oe-bolh.tot-pallets.
  END.

  FOR EACH tt-ci-form:

      RUN oerep\d-fibreci.w (INPUT v-s-bol,
                             INPUT v-e-bol,
                             INPUT v-s-ord,
                             INPUT v-e-ord,
                             INPUT tt-ci-form.form-name,
                             INPUT v-printed,
                             INPUT tb_posted,
                             INPUT tt-ci-form.total-pallets,
                             INPUT v-s-cust,
                             INPUT v-e-cust).
  END.
  
  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  SESSION:SET-WAIT-STATE ("").

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
  DISPLAY begin_cust end_cust begin_bol# end_bol# begin_ord# end_ord# tb_reprint 
          tb_pallet tb_posted tb_print-component tb_print-shipnote lbl_bolcert 
          rd_bolcert rd-dest tb_EMailAdvNotice tb_MailBatchMode tb_ComInvoice 
          lv-ornt lines-per-page lv-font-no lv-font-name tb_post-bol 
          td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_cust end_cust begin_bol# end_bol# begin_ord# 
         end_ord# tb_reprint tb_pallet tb_posted tb_print-component 
         tb_print-shipnote rd_bolcert rd-dest tb_EMailAdvNotice 
         tb_MailBatchMode tb_ComInvoice lv-ornt lines-per-page lv-font-no 
         tb_post-bol td-show-parm btn-cancel btn-ok 
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
  
  DEFINE INPUT PARAM ic1stKey AS CHAR NO-UNDO.
  DEFINE INPUT PARAM ic2ndKey AS CHAR NO-UNDO.
  DEFINE INPUT PARAM iiMode   AS INTE NO-UNDO.
  DEFINE INPUT PARAM icType   AS CHAR NO-UNDO.

  /* XPrint */
  IF is-xprint-form THEN DO:
    
    RUN run-report-mail (INPUT ic1stKey,
                         INPUT ic2ndKey,
                         INPUT iiMode,
                         INPUT YES).

    IF NOT vcBOLNums GT '' THEN RETURN.

    IF v-print-fmt = "SouthPak-XL" THEN ASSIGN lv-pdf-file = "c:\tmp\bol".

    ELSE DO:    
      lv-pdf-file = lv-pdf-file + vcBOLNums + '.pdf'.
      IF list-name NE ? AND 
         list-name NE '' 
        THEN RUN printPDF (list-name,   "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").                            
        ELSE RUN printPDF (lv-pdf-file, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").                            
    END.
    
    CASE icType:

      WHEN 'Customer1':U  THEN RUN SendMail-1 (ic1stKey, 'Customer1').
      WHEN 'Customer':U   THEN RUN SendMail-1 (ic1stKey, 'Customer').
      WHEN 'ShipTo1':U    THEN RUN SendMail-1 (ic2ndKey, 'ShipTo1'). 
      WHEN 'ShipTo':U     THEN RUN SendMail-1 (ic2ndKey, 'ShipTo'). 

    END CASE.
  END.
  
  /* Not XPrint */
  ELSE DO:

    RUN run-report-mail (INPUT ic1stKey,
                         INPUT ic2ndKey,
                         INPUT iiMode,
                         INPUT YES).

    IF NOT vcBOLNums GT '' THEN RETURN.

    CASE icType:

      WHEN 'Customer1':U  THEN RUN SendMail-2 (ic1stKey, 'Customer1').
      WHEN 'Customer':U   THEN RUN SendMail-2 (ic1stKey, 'Customer').
      WHEN 'ShipTo1':U    THEN RUN SendMail-2 (ic2ndKey, 'ShipTo1'). 
      WHEN 'ShipTo':U     THEN RUN SendMail-2 (ic2ndKey, 'ShipTo'). 

    END CASE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-bol# C-Win 
PROCEDURE new-bol# :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF INT(begin_bol#:SCREEN-VALUE) NE 0                          AND
       INT(begin_bol#:SCREEN-VALUE) EQ INT(end_bol#:SCREEN-VALUE) THEN DO:
      FIND FIRST oe-bolh NO-LOCK
          WHERE oe-bolh.company EQ cocode
            AND oe-bolh.bol-no  EQ INT(begin_bol#:SCREEN-VALUE)
          NO-ERROR.
      IF AVAIL oe-bolh THEN
        ASSIGN
         tb_reprint:SCREEN-VALUE = STRING(oe-bolh.printed)
         tb_posted:SCREEN-VALUE  = STRING(oe-bolh.posted).
    END. 
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

  DO WITH FRAME {&FRAME-NAME}:
  
    {custom/asifax.i &type         = "Customer"
                     &begin_cust   = begin_cust 
                     &END_cust     = begin_cust 
                     &fax-subject  = "BOL"
                     &fax-body     = "BOL"
                     &fax-file     = list-name}
                     
    /* Intercept Advanced Ship Notice Job */
    IF tb_EMailAdvNotice:CHECKED IN FRAME {&FRAME-NAME} THEN
       RUN AdvancedNotice.
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

  /* Intercept Advanced Ship Notice Job */
  IF tb_EMailAdvNotice:CHECKED IN FRAME {&FRAME-NAME} THEN
    RUN AdvancedNotice.

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

  assign
    v-s-cust            = begin_cust
    v-e-cust            = end_cust
    v-s-bol             = begin_bol#
    v-e-bol             = end_bol#
    v-s-ord             = begin_ord#
    v-e-ord             = end_ord#
    v-printed           = tb_reprint
    v-print-pal         = tb_pallet
    v-print-bol         = rd_bolcert EQ "BOL"
    v-print-components  = tb_print-component
    v-print-shipnotes   = tb_print-shipnote
    lv-run-bol          = ""
    lv-run-commercial   = "".

  FOR EACH b1-cust NO-LOCK
     WHERE b1-cust.company EQ cocode
       AND b1-cust.cust-no GE begin_cust
       AND b1-cust.cust-no LE end_cust,
    
     FIRST b1-oe-bolh
     where b1-oe-bolh.company eq cocode
       and b1-oe-bolh.bol-no  ge v-s-bol
       and b1-oe-bolh.bol-no  le v-e-bol
       and b1-oe-bolh.cust-no EQ b1-cust.cust-no
       and b1-oe-bolh.printed eq v-printed
       and b1-oe-bolh.posted  eq tb_posted
       and can-find (FIRST b1-oe-boll
                     WHERE b1-oe-boll.company EQ b1-oe-bolh.company
                       AND b1-oe-boll.b-no    EQ b1-oe-bolh.b-no
                       AND b1-oe-boll.ord-no  GE v-s-ord
                       AND b1-oe-boll.ord-no  LE v-e-ord)
    USE-INDEX post:

    ASSIGN  
      vcBOLNums   = '' 
      lv-pdf-file = init-dir + 'BOL'
      vcMailMode  = if tb_MailBatchMode then 'Customer1'  /* Silent Mode */
                                        else 'Customer'.  /* Dialog Box */
    /* XPrint */
    IF is-xprint-form THEN DO:
      
      RUN run-report-mail (INPUT b1-cust.cust-no,
                           INPUT '',
                           INPUT 1,
                           INPUT v-printed).

      IF v-print-fmt = "SouthPak-XL" THEN ASSIGN lv-pdf-file = "c:\tmp\bol".

      ELSE DO:    
        lv-pdf-file = lv-pdf-file + vcBOLNums + '.pdf'.
        IF list-name NE ? AND
           list-name NE ''
          THEN RUN printPDF (list-name,   "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
          ELSE RUN printPDF (lv-pdf-file, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").  
      END.
      
      if vcMailMode = 'Customer1' then RUN SendMail-1 (b1-cust.cust-no, 'Customer1'). /* Silent Mode */
                                  else RUN SendMail-1 (b1-cust.cust-no, 'Customer').  /* Dialog Box */ 
    END.
    
    /* Not XPrint */
    ELSE DO:

      RUN run-report-mail (INPUT b1-cust.cust-no,
                           INPUT '',
                           INPUT 1,
                           INPUT v-printed).

      CASE vcMailMode:

        WHEN 'Customer1':U  THEN RUN SendMail-2 (b1-cust.cust-no, 'Customer1').
        WHEN 'Customer':U   THEN RUN SendMail-2 (b1-cust.cust-no, 'Customer').
      END CASE.

    END.
  END. /* each cust */

  status default 'Print Job completed'.

  IF tb_EMailAdvNotice:CHECKED IN FRAME {&FRAME-NAME} THEN
    RUN AdvancedNotice.

  if tb_MailBatchMode then
    MESSAGE "Your E-Mails have been sent in Silent Mode."  skip
            "Please verify transmission in your SENT folder."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  status default 'Enter data or ESC to end.'.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  
  SESSION:SET-WAIT-STATE ("").

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

  /* Intercept Advanced Ship Notice Job */
  IF tb_EMailAdvNotice:CHECKED IN FRAME {&FRAME-NAME} THEN
    RUN AdvancedNotice.
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
  IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE IF lv-prt-bypass THEN RUN custom/d-print.w (list-name).
  ELSE RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).

  /* Intercept Advanced Ship Notice Job */  
  IF tb_EMailAdvNotice:CHECKED IN FRAME {&FRAME-NAME} THEN 
    RUN AdvancedNotice.

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

  /* Intercept Advanced Ship Notice Job */
  IF tb_EMailAdvNotice:CHECKED IN FRAME {&FRAME-NAME} THEN
    RUN AdvancedNotice.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-bol C-Win 
PROCEDURE post-bol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {sa/sa-sls01.i}

  FOR EACH w-ord.
    DELETE w-ord.
  END.

  FOR EACH tt-post TRANSACTION:
    RELEASE oe-bolh.
    DO WHILE NOT AVAIL oe-bolh:
      FIND FIRST oe-bolh EXCLUSIVE WHERE ROWID(oe-bolh) EQ tt-post.row-id
          NO-WAIT NO-ERROR.

      IF AVAIL oe-bolh AND oe-bolh.posted EQ NO THEN
      FOR EACH oe-boll NO-LOCK WHERE oe-boll.b-no EQ oe-bolh.b-no:
        RUN oe/bol-pre-post.p (ROWID(oe-boll), v-term).
      END.
    END.
  END.

  RUN oe/oe-bolp3.p (v-term).

  /* close transfer order here */
  RUN oe/closchk.p (0).

  FOR EACH w-ord:
    RUN oe/close.p (w-ord.rec-id, YES).  
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* --------------------------------------------- oe/rep/oe-lad.p 3/94 RM ---- */
/* print bill of ladings                                                      */
/* -------------------------------------------------------------------------- */
    
  {sys/form/r-top.i}
  
  assign
    v-s-cust            = begin_cust
    v-e-cust            = end_cust
    v-s-bol             = begin_bol#
    v-e-bol             = end_bol#
    v-s-ord             = begin_ord#
    v-e-ord             = end_ord#
    v-printed           = tb_reprint
    v-print-pal         = tb_pallet
    v-print-bol         = rd_bolcert EQ "BOL"
    v-print-components  = tb_print-component
    v-print-shipnotes   = tb_print-shipnote
    lv-run-bol          = ""
    lv-run-commercial   = "".
  
  IF NOT v-print-bol THEN DO:
  
    IF v-coc-fmt = "Xprint" THEN
      ASSIGN 
        is-xprint-form = YES
        v-program      = "oe/rep/cocxprnt.p".
  
     ELSE 
       ASSIGN 
        is-xprint-form = NO
        v-program      = "oe/rep/cocbrick.p".
  END.
  
  {sys/inc/print1.i}
  
  {sys/inc/outprint.i value(lines-per-page)}
  
  if td-show-parm then run show-param.
  
  SESSION:SET-WAIT-STATE ("general").
  
  {sa/sa-sls01.i}
  
  v-term-id = v-term.

  run build-work ('').
  
  DO :
  
    /*
    IF rd-dest = 2 AND is-xprint-form THEN PUT "<PREVIEW>".   
    ELSE IF is-xprint-form AND rd-dest = 1 THEN PUT "<PRINTER?>".
    */
  
    IF IS-xprint-form THEN DO:
  
        CASE rd-dest:
            WHEN 1 THEN PUT "<PRINTER?>".
            WHEN 2 THEN PUT "<PREVIEW>".        
            WHEN 4 THEN do:
                  ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
                      /*(IF is-xprint-form THEN ".xpr" ELSE ".txt").*/
                  PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW>".
            END.
            WHEN 5 THEN do:
                IF v-print-fmt = "Century" THEN
                     PUT "<PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(100)".
                ELSE PUT "<PREVIEW><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(60)".
            END.
        END CASE.
    END.
  
    IF lv-run-bol = "YES" THEN DO:
        IF v-print-fmt = "1/2 Page" AND rd-dest = 6 THEN DO:
            PUT CONTROL CHR(27) CHR(67) CHR(44). 
            RUN value(v-program). 
            PUT CONTROL CHR(18).
        END.
        ELSE RUN value(v-program).
    END.
  
    IF lv-run-commercial = "YES" AND IS-xprint-form THEN DO:
       RUN oerep/runbolci.p.
    END.
  END.
  
  for each report where report.term-id eq v-term-id:
    delete report.
  end.
  
  OUTPUT CLOSE.
  
  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  
  SESSION:SET-WAIT-STATE ("").

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-ci C-Win 
PROCEDURE run-report-ci :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER  bf-oe-boll          FOR oe-boll.
DEF VAR     lv-run-bol          AS char NO-UNDO.
DEF VAR     lv-run-commercial   AS char no-undo.
DEF VAR     v-tmp-is-xprint     AS LOG  NO-UNDO.

{sys/form/r-top.i}

assign
  v-s-cust            = begin_cust
  v-e-cust            = end_cust
  v-s-bol             = begin_bol#
  v-e-bol             = end_bol#
  v-s-ord             = begin_ord#
  v-e-ord             = end_ord#
  v-printed           = tb_reprint
  v-print-pal         = tb_pallet
  v-print-bol         = rd_bolcert EQ "BOL"
  v-print-components  = tb_print-component
  v-print-shipnotes   = tb_print-shipnote
  lv-run-bol          = ""
  lv-run-commercial   = ""
  v-tmp-is-xprint     = IS-xprint-form
  is-xprint-form      = YES.

IF NOT v-print-bol THEN DO:

   IF v-coc-fmt = "Xprint" THEN 
     ASSIGN is-xprint-form  = YES
            v-program       = "oe/rep/cocxprnt.p".
   ELSE 
     ASSIGN is-xprint-form  = NO
            v-program       = "oe/rep/cocbrick.p".
END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

{sa/sa-sls01.i}

v-term-id = v-term.

build-work:
FOR EACH oe-bolh
   WHERE oe-bolh.company EQ cocode
     AND oe-bolh.bol-no  GE v-s-bol
     AND oe-bolh.bol-no  LE v-e-bol
     AND oe-bolh.cust-no GE v-s-cust
     AND oe-bolh.cust-no LE v-e-cust 
     AND oe-bolh.printed EQ v-printed
     AND oe-bolh.posted  EQ tb_posted
     AND CAN-FIND (FIRST oe-boll
                   WHERE oe-boll.company EQ oe-bolh.company
                     AND oe-boll.b-no    EQ oe-bolh.b-no
                     AND oe-boll.ord-no  GE v-s-ord
                     AND oe-boll.ord-no  LE v-e-ord)
    USE-INDEX post.

  IF NOT oe-ctrl.p-bol THEN
  FOR EACH oe-boll
     WHERE oe-boll.company EQ oe-bolh.company
       AND oe-boll.bol-no  EQ oe-bolh.bol-no
       AND CAN-FIND (FIRST oe-ord
                     WHERE oe-ord.company EQ oe-boll.company
                       AND oe-ord.ord-no  EQ oe-boll.ord-no
                       AND oe-ord.stat    EQ "H")
      NO-LOCK:
    NEXT build-work.
  END.

  /* update loadtag status - Bill of lading task#: 10190414 */
  IF NOT oe-bolh.printed THEN
  FOR EACH bf-oe-boll NO-LOCK
     WHERE bf-oe-boll.company EQ oe-bolh.company 
       AND bf-oe-boll.b-no    EQ oe-bolh.b-no
       AND bf-oe-boll.tag     NE "",
     FIRST loadtag
     WHERE loadtag.company    EQ bf-oe-boll.company
       AND loadtag.item-type  EQ NO
       AND loadtag.tag-no     EQ bf-oe-boll.tag USE-INDEX tag:

    loadtag.sts = "Bill of Lading".
  END.

  FIND FIRST sys-ctrl-shipto NO-LOCK
       WHERE sys-ctrl-shipto.company      EQ oe-bolh.company
         AND sys-ctrl-shipto.name         EQ "BOLFMT"
         AND sys-ctrl-shipto.cust-vend    EQ YES
         AND sys-ctrl-shipto.cust-vend-no EQ oe-bolh.cust-no
         AND sys-ctrl-shipto.ship-id      EQ oe-bolh.ship-id NO-ERROR.

  CREATE report.

  ASSIGN
   report.term-id  = v-term-id
   report.key-01   = oe-bolh.cust-no
   report.key-02   = oe-bolh.ship-id
   report.rec-id   = RECID(oe-bolh)
   report.key-09   = STRING(oe-bolh.printed,"REVISED/ORIGINAL")
   report.key-03   =      IF AVAIL sys-ctrl-shipto AND NOT sys-ctrl-shipto.log-fld  THEN "C" /*commercial invoice only*/
                     ELSE IF AVAIL sys-ctrl-shipto AND     sys-ctrl-shipto.log-fld  THEN "B" /*commercial invoice and bol both*/
                     ELSE                                                                "N" /*BOL only*/ 
   report.key-04   =      IF AVAIL sys-ctrl-shipto THEN    sys-ctrl-shipto.char-fld ELSE "".     
                            
  IF lv-run-bol        EQ "" AND report.key-03 <> "C" THEN lv-run-bol        = "YES" .
  IF lv-run-commercial EQ "" AND report.key-03 <> "N" THEN lv-run-commercial = "YES".
end.

v-lines-per-page = lines-per-page.
/*
IF rd-dest = 2 AND is-xprint-form THEN PUT "<PREVIEW>".   
ELSE IF is-xprint-form AND rd-dest = 1 THEN PUT "<PRINTER?>".
*/
/*IF IS-xprint-form THEN */  DO:
    CASE rd-dest:
        WHEN 1 THEN PUT  "<PRINTER?>".
        WHEN 2 THEN PUT "<PREVIEW>".        
        WHEN  4 THEN do:
              ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
                  /*(IF is-xprint-form THEN ".xpr" ELSE ".txt").*/
              PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW>".
        END.
        WHEN 5 THEN do:
            IF v-print-fmt = "Century" THEN
                 PUT "<PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(100)".
            ELSE PUT "<PREVIEW><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(60)".
        END.
    END CASE.
END.

IF lv-run-commercial = "YES" THEN DO:
   RUN oerep/runbolci.p.
END.
       

OUTPUT CLOSE.

DO WITH FRAME {&FRAME-NAME}:
  case rd-dest :
    when 1 then run output-to-printer.
    when 2 then run output-to-screen.
    when 3 then run output-to-file.
    when 4 then do:
       /*run output-to-fax.*/
       {custom/asifax.i       &type         = "Customer"
                              &begin_cust   = begin_cust 
                              &end_cust     = begin_cust
                              &fax-subject  = "BOL"
                              &fax-body     = "BOL"
                              &fax-file     = list-name }
    END. 
    when 5 then do:
       IF is-xprint-form THEN DO:
          RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").                            
          {custom/asimail2.i  &TYPE         = "Customer"
                              &group-title  = 'r-bolprt.' /* v-prgmname */
                              &begin_cust   = begin_cust
                              &end_cust     = end_cust
                              &mail-subject = "BOL"
                              &mail-body    = "BOL"
                              &mail-file    = lv-pdf-file + ".pdf" }
       END.
       ELSE DO:
           {custom/asimailr2.i &TYPE        = "Customer"
                              &group-title  = 'r-bolprt.' /* v-prgmname */
                              &begin_cust   = begin_cust
                              &end_cust     = end_cust
                              &mail-subject = current-window:title
                              &mail-body    = CURRENT-WINDOW:TITLE
                              &mail-file    = list-name }
    
       END.
    END. 
    WHEN 6 THEN RUN output-to-port.
  end case.
END.

for each report where report.term-id eq v-term-id:
  delete report.
end.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").
IS-xprint-form = v-tmp-is-xprint.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-mail C-Win 
PROCEDURE run-report-mail :
/* --------------------------------------------------------*/

  DEFINE INPUT PARAM icCustNo AS CHAR NO-UNDO.
  DEFINE INPUT PARAM ic2ndKey AS CHAR NO-UNDO.
  DEFINE INPUT PARAM iiMode   AS INTE NO-UNDO.
  DEFINE INPUT PARAM iLprinted AS LOG NO-UNDO.

  {sys/form/r-top.i}
  
  assign
    v-s-cust            = icCustNo
    v-e-cust            = icCustNo
    v-s-bol             = begin_bol#
    v-e-bol             = end_bol#
    v-s-ord             = begin_ord#
    v-e-ord             = end_ord#
    v-printed           = iLprinted
    v-print-pal         = tb_pallet
    v-print-bol         = rd_bolcert EQ "BOL"
    v-print-components  = tb_print-component
    v-print-shipnotes   = tb_print-shipnote
    lv-run-bol          = ""
    lv-run-commercial   = "".
  
  IF NOT v-print-bol THEN DO:
  
    IF v-coc-fmt = "Xprint" THEN
      ASSIGN 
        is-xprint-form = YES
        v-program      = "oe/rep/cocxprnt.p".
  
     ELSE 
       ASSIGN 
        is-xprint-form = NO
        v-program      = "oe/rep/cocbrick.p".
  END.
  
  {sys/inc/print1.i}
  
  {sys/inc/outprint.i value(lines-per-page)}
  
  if td-show-parm then run show-param.
  
  SESSION:SET-WAIT-STATE ("general").
  
  {sa/sa-sls01.i}
  
  v-term-id = v-term.
  
  run build-work (ic2ndKey).
  
  IF NOT vcBOLNums > '' THEN RETURN.

  status default 'Processing... Please wait.'.

  if can-find (first report where report.term-id eq v-term-id) then
  do:
  
    IF IS-xprint-form THEN DO:
      IF v-print-fmt = "Century" 
        THEN PUT "<PDF=DIRECT><PDF-EXCLUDE=MS Mincho><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + vcBOLNums + ".pdf>" FORM "x(100)".
        ELSE PUT "<PDF=DIRECT><PDF-OUTPUT=" + lv-pdf-file + vcBOLNums + ".pdf>" FORM "x(60)".
    END.

    IF lv-run-bol = "YES" THEN DO:

      IF v-print-fmt = "1/2 Page" AND rd-dest = 6 THEN DO:
          PUT CONTROL CHR(27) CHR(67) CHR(44). 
          RUN value(v-program). 
          PUT CONTROL CHR(18).
      END.

      ELSE RUN value(v-program).
    END.
  
    IF lv-run-commercial = "YES" AND 
       IS-xprint-form THEN DO:
       RUN oerep/runbolci.p.
    END.
  END.
  
  else do:
    MESSAGE 'No records to process. Job aborted.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    return.
  end.

  for each report 
     where report.term-id eq v-term-id:
    delete report.
  end.
  
  OUTPUT CLOSE.

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

  DEFINE VARIABLE vcSubject   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcMailBody  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcErrorMsg  AS CHARACTER  NO-UNDO.
  
  ASSIGN  vcSubject   = "BOL: " + vcBOLNums + '   ' + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
          vcSubject   = IF tb_reprint THEN '[REPRINT] ' + vcSubject ELSE vcSubject
          vcMailBody  = "Please review attached Bill of Lading(s) for BOL #: " + vcBOLNums.
                      
  RUN custom/xpmail2.p   (input   icRecType,
                          input   'R-BOLPRT.',
                          input   lv-pdf-file,
                          input   icIdxKey,
                          input   vcSubject,
                          input   vcMailBody,
                          OUTPUT  vcErrorMsg).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMail-2 C-Win 
PROCEDURE SendMail-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAM icIdxKey   AS CHAR NO-UNDO.
  DEFINE INPUT PARAM icRecType  AS CHAR NO-UNDO.    

  DEFINE VARIABLE vcSubject   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcMailBody  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcErrorMsg  AS CHARACTER  NO-UNDO.

  IF SEARCH (list-name) NE ? THEN DO:
    IF NOT list-name MATCHES '*.txt' THEN DO:
      OS-RENAME VALUE (SEARCH (list-name)) VALUE (SEARCH (list-name) + '.txt').
      IF OS-ERROR NE 0 THEN DO:
        MESSAGE 'Failed to rename your temp file.'  SKIP
                'OS Error: ' OS-ERROR
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
      ELSE list-name = list-name + '.txt'.
    END.
  END.

  ELSE DO:
    MESSAGE 'Attachment File: ' list-name ' is missing.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  ASSIGN  vcSubject   = "BOL: " + vcBOLNums + '   ' + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
          vcSubject   = IF tb_reprint THEN '[REPRINT] ' + vcSubject ELSE vcSubject
          vcMailBody  = "Please review attached Bill of Lading(s) for BOL #: " + vcBOLNums.
                      
  RUN custom/xpmail2.p   (input   icRecType,
                          input   'R-BOLPRT.',
                          input   list-name,
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
  def var parm-fld-list as char no-undo.
  def var parm-lbl-list as char no-undo.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE test C-Win 
PROCEDURE test :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.
PROCEDURE mail EXTERNAL "xpMail.dll":
    DEF INPUT PARAMETER mailto          as CHAR.
    DEF INPUT PARAMETER mailsubject             as CHAR.
    DEF INPUT PARAMETER mailText                as CHAR.
    DEF INPUT PARAMETER mailFiles               as CHAR.
    DEF INPUT PARAMETER mailDialog              as LONG.
    DEF OUTPUT PARAMETER retCode                as LONG.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

