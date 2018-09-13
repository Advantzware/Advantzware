&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File              : r-bolprt.w

  Description       : BOL Printing

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
DEF INPUT PARAMETER ipr-eb AS ROWID.
/* Variables */
def var list-name         as char no-undo.
def var init-dir          as char no-undo.
DEF VAR v-EDIBOLPost-log AS LOG NO-UNDO.
DEF VAR v-EDIBOLPost-char AS CHAR FORMAT "X(200)" NO-UNDO.
DEF VAR v-lines-per-page AS INT NO-UNDO INIT 60.
DEF NEW SHARED VAR v-term-id AS CHAR NO-UNDO.
v-term-id = TERMINAL.
/* Includes */
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}
{sys/inc/var.i new shared}
cocode = gcompany.
FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.

def var v-print-fmt     as char NO-UNDO.
def var v-headers       as log  no-undo.
def var v-print-coc     as log  no-undo.
def var v-check-qty     as log  no-undo.
DEF VAR v-program       AS CHAR NO-UNDO.
DEF VAR is-xprint-form  AS LOG  NO-UNDO.
DEF VAR ls-fax-file     AS CHAR NO-UNDO.
DEF VAR lv-pdf-file     AS CHAR NO-UNDO.
/* DEF VAR vcBOLNums       AS CHAR NO-UNDO. */
DEF VAR vcMailMode      AS CHAR NO-UNDO.
DEF VAR vcDefaultForm   AS CHAR NO-UNDO.
/* DEF VAR vcDefaultBOLX   AS CHAR NO-UNDO. */
DEF VAR v-def-coc-fmt   AS CHAR NO-UNDO.
DEF VAR v-strips1 AS DECIMAL NO-UNDO.
DEF VAR v-strips2 AS DECIMAL NO-UNDO.
DEF VAR v-next-num AS INT NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.

{custom/xprint.i}

DEFINE VARIABLE retcode AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.

 RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

DEF VAR lv-prt-bypass     AS LOG NO-UNDO.  /* bypass window's printer driver */

/* Build a Table to keep sequence of pdf files */
DEF NEW SHARED TEMP-TABLE tt-filelist NO-UNDO
                    FIELD tt-FileCtr    AS INT
                    FIELD tt-FileName   AS CHAR
                    INDEX filelist      IS PRIMARY 
                          TT-FILECTR.

/* Output selection for the report */
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-email NO-UNDO
      FIELD tt-recid AS RECID
      FIELD bol-no LIKE oe-boll.bol-no
      FIELD ord-no LIKE oe-boll.ord-no
      FIELD i-no LIKE itemfg.i-no
      FIELD qty AS INT
      FIELD cust-no AS cha
      INDEX tt-cust IS PRIMARY cust-no DESCENDING .

DEF STREAM st-email.

DEFINE STREAM ediBOL.

DEFINE TEMP-TABLE ediOutFile NO-UNDO
  FIELD custNo AS CHAR
  FIELD poNo AS CHAR
  FIELD poLine AS INT
  FIELD partNo AS CHAR
  FIELD qty AS DEC
  FIELD lotNo AS CHAR
  FIELD bolDate AS DATE
  FIELD relNo AS INT
  FIELD carrier AS CHAR
  FIELD trailer AS CHAR
  FIELD bolNo AS INT
    INDEX ediOutFile IS PRIMARY custNo bolNo carrier trailer.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-20 begin_cust end_cust ~
begin_est# end_est# fiSampleNum tgSampleSent fiDateRec fiNoCells fiDateDue ~
fiNoSamples rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
btn-cancel btn-ok 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_est# end_est# ~
fiSampleNum tgSampleSent fiDateRec fiNoCells fiDateDue fiNoSamples rd-dest ~
lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
  ( ipField AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_est# AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Beginning Est #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_est# AS CHARACTER FORMAT "X(256)":U INITIAL "99999999" 
     LABEL "Ending Est #" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiDateDue AS DATE FORMAT "99/99/99":U 
     LABEL "Date Due" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiDateRec AS DATE FORMAT "99/99/99":U 
     LABEL "Date Receive" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiNoCells AS INTEGER FORMAT "->,>>>,>>>":U INITIAL 0 
     LABEL "No. Cells" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiNoSamples AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "# of Samples" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiSampleNum AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sample #" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

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
"To Email", 4,
"To Direct Port", 5
     SIZE 23 BY 5 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 8.33.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 7.14.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE tgSampleSent AS LOGICAL INITIAL no 
     LABEL "Sample Sent" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust AT ROW 2.43 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 2.43 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_est# AT ROW 4.1 COL 26 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_est# AT ROW 4.1 COL 69 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     fiSampleNum AT ROW 5.52 COL 26 COLON-ALIGNED WIDGET-ID 2
     tgSampleSent AT ROW 5.52 COL 53 WIDGET-ID 12
     fiDateRec AT ROW 6.95 COL 26 COLON-ALIGNED WIDGET-ID 4
     fiNoCells AT ROW 6.95 COL 69 COLON-ALIGNED WIDGET-ID 16
     fiDateDue AT ROW 8.38 COL 26 COLON-ALIGNED WIDGET-ID 6
     fiNoSamples AT ROW 8.38 COL 69 COLON-ALIGNED WIDGET-ID 10
     rd-dest AT ROW 11.95 COL 6 NO-LABEL
     lv-ornt AT ROW 12.19 COL 35 NO-LABEL
     lines-per-page AT ROW 12.19 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 13.71 COL 33 COLON-ALIGNED
     lv-font-name AT ROW 13.71 COL 40.4 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 15.24 COL 35
     btn-cancel AT ROW 18.14 COL 64
     btn-ok AT ROW 18.19 COL 23
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.76 COL 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.1 COL 2
          BGCOLOR 2 
     RECT-6 AT ROW 10.52 COL 3
     RECT-20 AT ROW 1.95 COL 3 WIDGET-ID 14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 18.95.


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
         TITLE              = "Print Spec Sheet"
         HEIGHT             = 19.43
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
       begin_est#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_est#:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lv-font-name:READ-ONLY IN FRAME FRAME-A        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Print Spec Sheet */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Print Spec Sheet */
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


&Scoped-define SELF-NAME begin_est#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est# C-Win
ON LEAVE OF begin_est# IN FRAME FRAME-A /* Beginning Est # */
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
   DEF VAR v-format-str AS CHAR NO-UNDO.
   DEF VAR lv-exception AS LOG NO-UNDO.
   /* Initilize temp-table */

   DO WITH FRAME {&FRAME-NAME}:
     ASSIGN {&displayed-objects}.
   END.
  ASSIGN 
   begin_est#  = FILL(" ",8 - LENGTH(TRIM(begin_est#))) + TRIM(begin_est#)
   end_est#    = FILL(" ",8 - LENGTH(TRIM(end_est#))) + TRIM(end_est#).

   v-format-str = "SPECSHT".

   CASE rd-dest:
      WHEN 1 THEN ASSIGN LvOutputSelection = "Printer".
      WHEN 2 THEN ASSIGN LvOutputSelection = "Screen". 
      WHEN 3 THEN ASSIGN LvOutputSelection = "File". 
      WHEN 4 THEN ASSIGN LvOutputSelection = "Fax". 
      WHEN 5 THEN ASSIGN LvOutputSelection = "Email".
      WHEN 6 THEN ASSIGN LvOutputSelection = "Port".
   END CASE.

   IF NOT rd-dest = 5 THEN
   DO:
/*        /* find sysctrl */                    */
/*       IF AVAIL sys-ctrl THEN                 */
/*       DO:                                    */
/*          RUN SetSPECForm(sys-ctrl.char-fld). */
/*          v-print-fmt = sys-ctrl.char-fld.    */
/*       END.                                   */

/*       /* RUN SetVariables.*/            */
/*       FIND FIRST eb                     */
/*          WHERE eb.company EQ cocode     */
/*            AND eb.est-no  GE begin_est# */
/*            AND eb.est-no  LE END_est#   */
/*            AND eb.cust-no GE begin_cust */
/*            AND eb.cust-no LE end_cust   */
/* /*            AND eb.form-no EQ 0 */    */
/*          NO-LOCK NO-ERROR.              */
/*      IF AVAIL eb THEN DO:               */
        RUN run-report.

/*         FIND FIRST eb                     */
/*            WHERE eb.company EQ cocode     */
/*              AND eb.est-no  GE begin_est# */
/*              AND eb.est-no  LE END_est#   */
/*              AND eb.cust-no GE begin_cust */
/*              AND eb.cust-no LE end_cust   */
/* /*              AND eb.form-no EQ 0 */    */
/*            NO-LOCK NO-ERROR.              */
/*                                           */

/*         RUN GenerateReport(eb.cust-no,YES). */
/*                                                  */
/*       END.                                       */
/*       ELSE                                       */
/*           MESSAGE "Estimate record not found"    */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK. */
   END.
   ELSE /*rd-dest eq 5*/
   DO:

/*       IF CAN-FIND(FIRST sys-ctrl-shipto WHERE                                      */
/*          sys-ctrl-shipto.company = cocode AND                                      */
/*          sys-ctrl-shipto.NAME = v-format-str) THEN                                 */
/*          DO:                                                                       */
/*             IF CAN-FIND(FIRST b-oe-bolh WHERE                                      */
/*                b-oe-bolh.company eq cocode AND                                     */
/*                b-oe-bolh.bol-no  ge begin_bol# AND                                 */
/*                b-oe-bolh.bol-no  le end_bol# AND                                   */
/*                b-oe-bolh.cust-no GE begin_cust AND                                 */
/*                b-oe-bolh.cust-no LE end_cust AND                                   */
/*                b-oe-bolh.printed eq tb_reprint AND                                 */
/*                b-oe-bolh.posted  eq tb_posted AND                                  */
/*                can-find (FIRST b1-oe-boll                                          */
/*                          WHERE b1-oe-boll.company EQ b-oe-bolh.company AND         */
/*                                b1-oe-boll.b-no    EQ b-oe-bolh.b-no AND            */
/*                                b1-oe-boll.ord-no  GE begin_ord# AND                */
/*                                b1-oe-boll.ord-no  LE end_ord#)) THEN               */
/*                FOR EACH b-oe-bolh WHERE                                            */
/*                    b-oe-bolh.company eq cocode AND                                 */
/*                    b-oe-bolh.bol-no  ge begin_bol# AND                             */
/*                    b-oe-bolh.bol-no  le end_bol# AND                               */
/*                    b-oe-bolh.cust-no GE begin_cust AND                             */
/*                    b-oe-bolh.cust-no LE end_cust AND                               */
/*                    b-oe-bolh.printed eq tb_reprint AND                             */
/*                    b-oe-bolh.posted  eq tb_posted AND                              */
/*                    can-find (FIRST b1-oe-boll WHERE                                */
/*                                    b1-oe-boll.company EQ b-oe-bolh.company AND     */
/*                                    b1-oe-boll.b-no    EQ b-oe-bolh.b-no AND        */
/*                                    b1-oe-boll.ord-no  GE begin_ord# AND            */
/*                                    b1-oe-boll.ord-no  LE end_ord#)                 */
/*                    NO-LOCK                                                         */
/*                    USE-INDEX post                                                  */
/*                    BREAK BY b-oe-bolh.company                                      */
/*                          BY b-oe-bolh.cust-no:                                     */
/*                                                                                    */
/*                    IF FIRST-OF(b-oe-bolh.cust-no) THEN                             */
/*                    DO:                                                             */
/*                       FIND FIRST sys-ctrl-shipto WHERE                             */
/*                            sys-ctrl-shipto.company = cocode AND                    */
/*                            sys-ctrl-shipto.NAME = v-format-str AND                 */
/*                            sys-ctrl-shipto.cust-vend = YES AND                     */
/*                            sys-ctrl-shipto.cust-vend-no = b-oe-bolh.cust-no AND    */
/*                            sys-ctrl-shipto.ship-id = b-oe-bolh.ship-id AND         */
/*                            sys-ctrl-shipto.char-fld > ''                           */
/*                            NO-LOCK NO-ERROR.                                       */
/*                                                                                    */
/*                       IF NOT AVAIL sys-ctrl-shipto THEN                            */
/*                          FIND FIRST sys-ctrl-shipto WHERE                          */
/*                               sys-ctrl-shipto.company = cocode AND                 */
/*                               sys-ctrl-shipto.NAME = v-format-str AND              */
/*                               sys-ctrl-shipto.cust-vend = YES AND                  */
/*                               sys-ctrl-shipto.cust-vend-no = b-oe-bolh.cust-no AND */
/*                               sys-ctrl-shipto.char-fld > ''                        */
/*                               NO-LOCK NO-ERROR.                                    */
/*                                                                                    */
/*                       IF AVAIL sys-ctrl-shipto THEN                                */
/*                       DO:                                                          */
/*                          RUN SetBolForm(sys-ctrl-shipto.char-fld).                 */
/*                          v-print-fmt = sys-ctrl-shipto.char-fld.                   */
/*                       END.                                                         */
/*                       ELSE                                                         */
/*                       DO:                                                          */
/*                          IF rd_bolcert EQ "BOL" THEN                               */
/*                          DO:                                                       */
/*                             IF NOT tb_freight-bill THEN                            */
/*                             DO:                                                    */
/*                                RUN SetBolForm (vcDefaultForm).                     */
/*                                v-print-fmt = vcDefaultForm.                        */
/*                             END.                                                   */
/*                             ELSE                                                   */
/*                             DO:                                                    */
/*                                RUN SetBolForm (vcDefaultBOLX).                     */
/*                                v-print-fmt = vcDefaultBOLX.                        */
/*                             END.                                                   */
/*                          END.                                                      */
/*                          ELSE                                                      */
/*                          DO:                                                       */
/*                             RUN SetBolForm (v-def-coc-fmt).                        */
/*                             v-print-fmt = v-def-coc-fmt.                           */
/*                          END.                                                      */
/*                       END.                                                         */
/*                                                                                    */
/*                       RUN SetVariables.                                            */
/*                       RUN output-to-mail(INPUT b-oe-bolh.cust-no,YES).             */
/*                    END.                                                            */
/*                END. /*end for each*/                                               */
/*          END. /*sys-ctrl-shipto found*/                                            */
/*       ELSE /*no sys-ctrl-shipto found*/                                            */
/*       DO:                                                                          */
/*          IF rd_bolcert EQ "BOL" THEN                                               */
/*          DO:                                                                       */
/*             IF NOT tb_freight-bill THEN                                            */
/*                v-print-fmt = vcDefaultForm.                                        */
/*             ELSE                                                                   */
/*                v-print-fmt = vcDefaultBOLX.                                        */
/*          END.                                                                      */
/*          ELSE                                                                      */
/*             v-print-fmt = v-def-coc-fmt.                                           */
/*                                                                                    */
/*          RUN SetBOLForm(v-print-fmt).                                              */
/*          RUN SetVariables.                                                         */
/*          RUN output-to-mail(INPUT "",NO).                                          */
/*       END.                                                                         */
/*                                                                                    */
/*       if tb_MailBatchMode then                                                     */
/*          MESSAGE "Your E-Mails have been sent in Silent Mode."  skip               */
/*                  "Please verify transmission in your SENT folder."                 */
/*                  VIEW-AS ALERT-BOX INFO BUTTONS OK.                                */
/*                                                                                    */
   END.

   SESSION:SET-WAIT-STATE ("").

   EMPTY TEMP-TABLE tt-email.

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


&Scoped-define SELF-NAME end_est#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est# C-Win
ON LEAVE OF end_est# IN FRAME FRAME-A /* Ending Est # */
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
    /*
  if int (self:screen-value) = 5 then do:
    assign tb_MailBatchMode:sensitive  = true.
    apply 'value-changed':u to tb_MailBatchMode.
  end.

  ELSE DO:

    assign tb_MailBatchMode:sensitive  = false.

    APPLY 'VALUE-CHANGED':U TO tb_EmailAdvNotice.
  END.
  */
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
PROCEDURE mail EXTERNAL "xpMail.dll" :
      DEF INPUT PARAM mailTo AS CHAR.
      DEF INPUT PARAM mailsubject AS CHAR.
      DEF INPUT PARAM mailText AS CHAR.
      DEF INPUT PARAM mailFiles AS CHAR.
      DEF INPUT PARAM mailDialog AS LONG.
      DEF OUTPUT PARAM retCode AS LONG.
END.

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

  find first company where company.company eq cocode no-lock no-error.

  find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.


  FIND FIRST users WHERE
       users.user_id EQ USERID("NOSWEAT")
       NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     init-dir = users.user_program[2].
  ELSE
     init-dir = "c:\tmp".

  RUN getCESAMPLE.

  RUN SetSpecForm(INPUT v-print-fmt).
  vcDefaultForm = v-print-fmt.


  RUN enable_UI.

  /*{methods/nowait.i} */

  DO WITH FRAME {&FRAME-NAME}:

/*     {custom/usrprint.i} */
    FIND eb WHERE ROWID(eb) = ipr-eb NO-LOCK.
    IF AVAIL eb THEN
        FIND FIRST est WHERE est.est-no = eb.est-no 
                   AND est.company = cocode
                  EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL eb THEN DO:
        ASSIGN
          begin_cust:SCREEN-VALUE = eb.cust-no
          end_cust:SCREEN-VALUE = eb.cust-no
          begin_est#:SCREEN-VALUE = eb.est-no
          end_est#:SCREEN-VALUE = eb.est-no.

    IF v-print-fmt = "Premier" THEN DO:
        ASSIGN 
            fiSampleNum:HIDDEN = YES
            tgSampleSent:HIDDEN = YES
            fiDateRec:HIDDEN = YES
            fiNoCells:HIDDEN = YES
            fiDateDue:HIDDEN = YES
            fiNoSamples:HIDDEN = YES
            rd-dest:SCREEN-VALUE = "2"
            rd-dest:SENSITIVE = NO
            lv-ornt:HIDDEN = YES
            lines-per-page:HIDDEN = YES
            lv-font-no:HIDDEN = YES
            lv-font-name:HIDDEN = YES
            td-show-parm:HIDDEN = YES.

    END.
    ELSE DO:
          IF est.sampleNum EQ 0 THEN DO TRANSACTION:
            find first sys-ctrl WHERE sys-ctrl.company eq cocode 
                                  AND sys-ctrl.name    eq "CESample"
                                EXCLUSIVE-LOCK no-error.      

             v-next-num = sys-ctrl.int-fld + 1.
             sys-ctrl.int-fld = v-next-num.

              ASSIGN est.sampleNum = v-next-num.
              RELEASE sys-ctrl.
              FIND CURRENT est NO-LOCK NO-ERROR.         
          END.
          v-next-num = est.sampleNum.
          fiSampleNum:SCREEN-VALUE = STRING(v-next-num).

        END.

        IF AVAIL eb AND AVAIL est THEN DO:
            RUN calc-values (INPUT ROWID(est), ROWID(eb)).
            fiNoCells:SCREEN-VALUE = STRING((INTEGER(v-strips1) + 1) * (INTEGER(v-strips2) + 1)).
        END.
    END.
    APPLY "entry" TO begin_cust.

    lines-per-page:SCREEN-VALUE = STRING(v-lines-per-page).
    DISABLE lines-per-page.

    APPLY 'value-changed':u TO rd-dest.

  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
/*
  {custom/asimail2.i  &TYPE           = value (icMailMode)
                      &group-title    = 'r-bolprt.'
                      &begin_cust     = icCustNo
                      &END_cust       = icCustNo
                      &mail-subject   = icSubBody
                      &mail-body      = icSubBody
                      &mail-file      = lv-pdf-file}
  */                    
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
  build-work:
  FOR EACH eb
     WHERE eb.company EQ cocode
       AND eb.est-no  GE begin_est#
       AND eb.est-no  LE END_est# 
       AND eb.cust-no GE begin_cust
       AND eb.cust-no LE end_cust 
       AND ((eb.form-no EQ 0 AND v-print-fmt NE "Premier") 
            OR (eb.form-no NE 0 AND v-print-fmt EQ "Premier"))
  :

    IF NOT CAN-FIND(FIRST report WHERE
       report.term-id = v-term-id AND
       report.rec-id  = RECID(eb)) THEN
       DO:
          CREATE report.
          ASSIGN 
              report.term-id  = v-term-id
              report.key-01   = eb.cust-no
              report.key-02   = eb.ship-id
              report.rec-id   = RECID(eb)
              report.key-03   = fiSampleNum
              report.key-04   = STRING(tgSampleSent)
              report.key-05   = STRING(fiDateRec)
              report.key-06   = STRING(fiDateDue)
              report.key-07   = STRING(fiNoSamples)
              report.key-08   = STRING(fiNoCells)
               .
       END.

    status default 'Now Processing Estimate: ' + string (eb.est-no) + '....'.


  END.

  v-lines-per-page = lines-per-page.

  status default ''.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-values C-Win 
PROCEDURE calc-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


DEF INPUT PARAMETER ip-est-rowid AS ROWID NO-UNDO.
DEF INPUT PARAMETER ip-eb-rowid AS ROWID NO-UNDO.
DEF VAR v-int AS INT NO-UNDO.
DEF VAR ll-crt-itemfg AS LOG INIT NO NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO. 
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEF VAR iCnt AS INT NO-UNDO.
DEF VAR v-part LIKE eb.part-no NO-UNDO.
DEF VAR v-slot1 AS INT NO-UNDO.
DEF VAR v-caliper1 AS CHAR NO-UNDO.
DEF VAR v-caliper2 AS CHAR NO-UNDO.
DEF VAR scr-style-1 AS CHAR NO-UNDO.
DEF VAR scr-end-cell-l1 AS INT NO-UNDO.
DEF VAR scr-end-cell-l2 AS INT NO-UNDO.
DEF VAR scr-in-cell-length AS INT NO-UNDO.

DEF VAR v-slot2 AS INT NO-UNDO.
DEF VAR scr-style-2 AS CHAR NO-UNDO.
DEF VAR scr-end-cell-w1 AS INT NO-UNDO.
DEF VAR scr-end-cell-w2 AS INT NO-UNDO.
DEF VAR scr-in-cell-width AS INT NO-UNDO.

DEF VAR v-eb-len AS INT NO-UNDO.
DEF VAR v-eb-wid AS INT NO-UNDO.
DEF VAR v-eb-dep AS INT NO-UNDO.

DEF BUFFER b-eb1 FOR eb.
DEF BUFFER b-eb2 FOR eb.
DEF BUFFER bf-est FOR est.
DEF BUFFER bf-set FOR eb.


{sys/inc/f16to32.i}
{cec/msfcalc.i}
{sys/inc/setprint.i}

find est where rowid(est) = ip-est-rowid no-lock NO-ERROR.
FIND eb WHERE ROWID(eb) = ip-eb-rowid NO-LOCK NO-ERROR.
IF NOT AVAIL eb THEN
  RETURN.
v-part = eb.part-no.
FIND FIRST bf-set WHERE
     bf-set.company = eb.company AND
     bf-set.est-no = eb.est-no AND
     bf-set.form-no = 0
     NO-LOCK NO-ERROR.

FIND FIRST b-eb1 WHERE 
     b-eb1.company EQ eb.company AND
     b-eb1.est-no  EQ eb.est-no AND
     b-eb1.form-no NE 0 AND
     b-eb1.blank-no NE 0
     USE-INDEX est-qty
     NO-LOCK NO-ERROR.

IF AVAIL b-eb1 THEN
   FIND FIRST b-eb2 WHERE 
        b-eb2.company EQ eb.company AND
        b-eb2.est-no  EQ eb.est-no AND
        b-eb2.form-no NE 0 AND
        b-eb2.blank-no NE 0 AND
        ROWID(b-eb2) NE ROWID(b-eb1)
        USE-INDEX est-qty
        NO-LOCK NO-ERROR.

IF AVAIL b-eb1 THEN
DO:
   FIND FIRST style WHERE
        style.company EQ b-eb1.company AND
        style.style = b-eb1.style
        NO-LOCK.
   IF AVAIL(style) THEN
     v-slot1 = style.dim-df.
   iCnt = 0.
   FOR EACH ef WHERE ef.company = b-eb1.company
                 AND ef.est-no  = b-eb1.est-no
                 AND ef.eqty    = b-eb1.eqty
                 AND ef.form-no = b-eb1.form-no
               NO-LOCK.

     FIND FIRST ITEM WHERE ITEM.company = ef.company
                       AND ITEM.i-no = ef.board 
                     NO-LOCK NO-ERROR.
     IF AVAIL ITEM THEN DO:

       iCnt = iCnt + 1.
       IF icnt = 1 THEN
         v-caliper1 = STRING(ITEM.cal).
       ELSE
         v-caliper2 = string(ITEM.cal).
     END.

   END.
   ASSIGN
      v-int = style.dim-df + 1
      scr-style-1 = b-eb1.style
      scr-end-cell-l1 = {sys/inc/k16.i b-eb1.k-len-array2[1]}
      scr-end-cell-l2 = {sys/inc/k16.i b-eb1.k-len-array2[v-int]}
      scr-in-cell-length = {sys/inc/k16.i b-eb1.k-len-array2[2]}
       v-strips1 = b-eb1.quantityPerSet.
END.

IF AVAIL b-eb2 THEN
DO:
   FIND FIRST style WHERE
        style.company EQ b-eb2.company AND
        style.style = b-eb2.style
        NO-LOCK.
   IF AVAIL(style) THEN
     v-slot2 = style.dim-df.
   ASSIGN
      v-int = style.dim-df + 1
      scr-style-2 = b-eb2.style
      scr-end-cell-w1 = {sys/inc/k16.i b-eb2.k-len-array2[1]}
      scr-end-cell-w2 = {sys/inc/k16.i b-eb2.k-len-array2[v-int]}
      scr-in-cell-width = {sys/inc/k16.i b-eb2.k-len-array2[2]}
      v-strips2 = b-eb2.quantityPerSet       .
   iCnt = 0.
   FOR EACH ef WHERE ef.company = b-eb2.company
                 AND ef.est-no  = b-eb2.est-no
                 AND ef.eqty    = b-eb2.eqty
                 AND ef.form-no = b-eb2.form-no
               NO-LOCK.

     FIND FIRST ITEM WHERE ITEM.company = ef.company
                       AND ITEM.i-no = ef.board 
                     NO-LOCK NO-ERROR.
     IF AVAIL ITEM THEN DO:

       iCnt = iCnt + 1.
       IF icnt = 1 THEN
         v-caliper1 = STRING(ITEM.cal).
       ELSE
         v-caliper2 = string(ITEM.cal).
     END.

   END.

END.

ASSIGN
   v-eb-len = {sys/inc/k16.i eb.len}
   v-eb-wid = {sys/inc/k16.i eb.wid}
   v-eb-dep = {sys/inc/k16.i eb.dep}.

RELEASE itemfg.

IF avail(bf-set) AND bf-set.stock-no NE "" THEN
   FIND FIRST itemfg WHERE
        itemfg.company EQ bf-set.company AND
        itemfg.i-no    EQ bf-set.stock-no
        NO-LOCK NO-ERROR.



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
  DISPLAY begin_cust end_cust begin_est# end_est# fiSampleNum tgSampleSent 
          fiDateRec fiNoCells fiDateDue fiNoSamples rd-dest lv-ornt 
          lines-per-page lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-20 begin_cust end_cust begin_est# end_est# fiSampleNum 
         tgSampleSent fiDateRec fiNoCells fiDateDue fiNoSamples rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm btn-cancel btn-ok 
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
/*
  /* XPrint */
  IF is-xprint-form THEN DO:

    RUN run-report-mail (INPUT ic1stKey,
                         INPUT ic2ndKey,
                         INPUT iiMode,
                         INPUT YES).

  IF list-name NE ? AND list-name NE '' THEN
     RUN printPDF (list-name,   "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").                            
  ELSE
     RUN printPDF (lv-pdf-file, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").                            

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

    CASE icType:

      WHEN 'Customer1':U  THEN RUN SendMail-2 (ic1stKey, 'Customer1').
      WHEN 'Customer':U   THEN RUN SendMail-2 (ic1stKey, 'Customer').
      WHEN 'ShipTo1':U    THEN RUN SendMail-2 (ic2ndKey, 'ShipTo1'). 
      WHEN 'ShipTo':U     THEN RUN SendMail-2 (ic2ndKey, 'ShipTo'). 

    END CASE.
  END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetCESAMPLE C-Win 
PROCEDURE GetCESAMPLE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lRecFound AS LOG NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.
RUN sys/ref/nk1look.p (cocode, "CESAMPLE", "C", no, no, "", "", 
                          Output v-print-fmt, output lRecFound).
RUN sys/ref/nk1look.p (cocode, "CESAMPLE", "I", no, no, "", "", 
                          Output cReturn, output lRecFound).
IF lRecFound THEN
    v-next-num = INT(cReturn).



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* --------------------------------------------- oe/rep/oe-lad.p 3/94 RM ---- */
/* print bill of ladings                                                      */
/* -------------------------------------------------------------------------- */

  {sys/form/r-top.i}

  {sys/inc/print1.i} 

  {sys/inc/outprint.i value(lines-per-page)}

  if td-show-parm then run show-param.

  SESSION:SET-WAIT-STATE ("general").

  {sa/sa-sls01.i} 

  v-term-id = v-term.
  DEF VAR ret-code AS INT.
  run build-work ('').

  IF IS-xprint-form THEN DO:
  CASE rd-dest:
    WHEN 1 THEN PUT "<PRINTER?></PROGRESS>".
    WHEN 2 THEN do:
    IF NOT lBussFormModle THEN
        PUT "<PREVIEW><MODAL=NO>". 
    ELSE
        PUT "<PREVIEW>".      
    END.  
    WHEN 3 THEN DO:
     /*  {custom/out2file.i} */
    END.
    WHEN 7 THEN DO:
       ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
       PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW></PROGRESS>".
    END.   
    WHEN 4 THEN DO:
        ASSIGN
           /*init-dir = v-dir */
           lv-pdf-file = init-dir + "\" + "Est" + TRIM(est.est-no).
        PUT "<PREVIEW><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf></PROGRESS>" FORM "x(180)".
    END. 
    WHEN 5 THEN RUN custom/d-print.w (list-name).
  END CASE.

  END.

  RUN value(v-program).

  for each report where report.term-id eq v-term-id:
      delete report.
  end.

  OUTPUT CLOSE.
  FILE-INFO:FILE-NAME = list-name.

    IF is-xprint-form THEN
      CASE rd-dest:

        WHEN 1 THEN RUN printfile (FILE-INFO:FILE-NAME).
        WHEN 2 THEN RUN printfile (FILE-INFO:FILE-NAME).
        WHEN 3 THEN DO:
           {custom/out2file.i}
        END. 
        WHEN 4 THEN DO:

          RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

          run custom/xpmail.p ("CUSTOMER",lv-pdf-file + ".pdf","",
                            'Sample Spec Sheet',
                            'Sample Spec Sheet',OUTPUT ret-code).

        END.   
        WHEN 5 THEN RUN custom/d-print.w (list-name).
        WHEN 7 THEN DO:
           ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".txt". 
           OS-COPY VALUE(list-name) VALUE(ls-fax-file).
           run custom/asifax.p ("",ls-fax-file,"",
                             'Estimate',
                             'Estimate',OUTPUT ret-code).
        END.      
      END CASE.

  IF rd-dest NE 4 THEN
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  SESSION:SET-WAIT-STATE ("").

end procedure.

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

/*   assign                                                    */
/*     v-s-cust            = icCustNo                          */
/*     v-e-cust            = icCustNo                          */
/*     v-s-bol             = begin_bol#                        */
/*     v-e-bol             = end_bol#                          */
/*     v-s-ord             = begin_ord#                        */
/*     v-e-ord             = end_ord#                          */
/*     v-printed           = iLprinted                         */
/*     v-print-pal         = tb_pallet                         */
/*     v-print-bol         = rd_bolcert EQ "BOL"               */
/*     v-print-components  = tb_print-component                */
/*     v-print-shipnotes   = tb_print-shipnote                 */
/*     v-print-dept        = tb_print-dept                     */
/*     lv-run-bol          = ""                                */
/*     lv-run-commercial   = "".                               */
/*                                                             */
/*   IF fi_depts:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN       */
/*      ASSIGN                                                 */
/*         v-print-dept = LOGICAL(tb_print-dept:SCREEN-VALUE)  */
/*         v-depts = fi_depts:SCREEN-VALUE.                    */

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  if td-show-parm then run show-param.

  SESSION:SET-WAIT-STATE ("general").

  {sa/sa-sls01.i}

  v-term-id = v-term.

  run build-work (ic2ndKey).


  status default 'Processing... Please wait.'.

  if can-find (first report where report.term-id eq v-term-id) then
  do:

    IF IS-xprint-form THEN DO:      
        PUT "<PDF=DIRECT><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-mail-uni-xl C-Win 
PROCEDURE send-mail-uni-xl :
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

  IF SEARCH (lv-pdf-file) EQ ? THEN DO:
    MESSAGE 'Attachment File: ' lv-pdf-file ' is missing.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  ASSIGN  vcSubject   = "CofC for BOL: " 
          vcMailBody  = "Please review attached CofC for BOL #: " .

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

  ASSIGN  vcSubject   = "BOL: " +  '   ' + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
          vcSubject   = vcSubject
          vcMailBody  = "Please review attached Bill of Lading(s) for BOL #: " .

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

  ASSIGN  vcSubject   = "BOL: " +  '   ' + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
          vcSubject   = vcSubject
          vcMailBody  = "Please review attached Bill of Lading(s) for BOL #: " .

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetSpecForm C-Win 
PROCEDURE SetSpecForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER icFormName AS CHAR NO-UNDO.

   DO:
      CASE icFormName:
         WHEN "Xprint" THEN
            ASSIGN 
               is-xprint-form = YES
               v-program      = "oe/rep/cocxprnt.p".
         WHEN "Partitions" THEN
            ASSIGN
               is-xprint-form = YES
               v-program = "est/specmultm.p".
         WHEN "Premier" THEN
            ASSIGN
               is-xprint-form = NO
               v-program = "est/specprem.p".
         WHEN "" OR WHEN "Brick" THEN
            ASSIGN 
               is-xprint-form = NO
               v-program      = "oe/rep/cocbrick.p".

         OTHERWISE
            ASSIGN
               is-xprint-form = NO
               v-program = "oe/rep/cocuni.p".
     END CASE.
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
  def var parm-fld-list as char no-undo.
  def var parm-lbl-list as char no-undo.
  def var i as int no-undo.
  def var lv-label as cha NO-UNDO.

  ASSIGN
  lv-frame-hdl = frame {&frame-name}:HANDLE
  lv-group-hdl = lv-frame-hdl:first-child
  lv-field-hdl = lv-group-hdl:first-child.

  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + ",".
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
  ( ipField AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE invalidChars AS CHARACTER NO-UNDO INITIAL "~",#".
   DEFINE VARIABLE replaceChars AS CHARACTER NO-UNDO INITIAL "'',".
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE k AS INTEGER NO-UNDO.

   k = NUM-ENTRIES(invalidChars).
   DO i = 1 TO k:
     ipField = REPLACE(ipField,ENTRY(i,invalidChars),ENTRY(i,replaceChars)).
   END.
   RETURN ipField.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

