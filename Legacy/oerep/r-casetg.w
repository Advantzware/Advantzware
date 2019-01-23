&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-casetg.w

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
{custom/xprint.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

DEF VAR lines-per-page AS INT NO-UNDO.

DEF var save_id AS RECID.

DEF var time_stamp AS ch.
ASSIGN time_stamp = string(TIME, "hh:mmam").

DEF var v-ford-no AS int FORMAT ">>>>>>" no-undo.
def var v-orders as char format "x(78)" extent 10.
DEF var v-fitem AS char FORMAT "x(15)".
DEF var v-po-no-source AS char FORMAT "!" init "R".
def var v-stat as char format "!" init "O".

DEF var v-out AS char FORMAT "x(40)" NO-UNDO.
DEF var v-init-dir AS char FORMAT "x(40)" NO-UNDO.
DEF var v-job AS char FORMAT "x(9)" NO-UNDO.
DEF VAR v-frm-no AS CHAR FORMAT "x(3)" NO-UNDO.
DEF VAR v-blnk-no AS CHAR FORMAT "x(3)" NO-UNDO.
DEF var num-rec AS int init 0 NO-UNDO.
DEF var by-release AS log init NO NO-UNDO.

/* 9812 CAH: */
DEF var v-loadtag       AS char NO-UNDO initial "ASI".  /* sys ctrl option */
DEF var v-mult          AS int  NO-UNDO initial 0.  /* sys ctrl option */
DEF var v-count         AS int  NO-UNDO initial 0.

/* 9812 CAH: Variables for Intermec Support */
def var stx as char format 'x(01)' no-undo initial "~002".
def var etx as char format 'x(01)' no-undo initial "~003".
def var esc as char format 'x(01)' no-undo initial "~033".
def var etb as char format 'x(01)' no-undo initial "~027".
def var cr  as char format 'x(01)' no-undo initial "~015".
def var can as char format 'x(01)' no-undo initial "~030".
def var rs  as char format 'x(01)' no-undo initial "~036".
def var us  as char format 'x(01)' no-undo initial "~037".

def stream s-form.
def stream s-bar.

def var form_fid        as char no-undo initial "barcode.frm" FORMAT "X(40)".
def var form#           as int  no-undo format "9" initial 3.
def var char_units      as char no-undo.
def var copy_count      as int no-undo initial 2.
def var n               as int no-undo initial 0.
DEF VAR v-case-tag      AS LOG NO-UNDO.
DEF VAR v-auto-print    AS LOG NO-UNDO.

/* gdm - 11050806 */
DEF VAR v-casflg        AS LOG NO-UNDO.
DEF VAR v-lcnt          AS INT NO-UNDO.
DEFINE VARIABLE cBarCodeProgram AS CHARACTER NO-UNDO .

def TEMP-TABLE w-file NO-UNDO field w-key AS ROWID.
DEF TEMP-TABLE tt-tag NO-UNDO FIELD tt-recid AS RECID.

{oerep/r-loadtg.i NEW}

{fg/fullset.i NEW}

ASSIGN  
  tmpstore = FILL("_",50).

{sys/form/r-top3.f}

DEF VAR userLabelPath AS cha NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 fi_cas-lab begin_job begin_job2 ~
begin_ord-no begin_i-no begin_rel tb_per-unit tb_per-pallet tb_over ~
tb_print-comp scr-auto-print scr-freeze-label scr-label-file rd_print ~
begin_date end_date btn-cancel btn-ok 
&Scoped-Define DISPLAYED-OBJECTS fi_cas-lab begin_job begin_job2 ~
begin_ord-no begin_i-no begin_rel tb_per-unit tb_per-pallet tb_over ~
tb_print-comp scr-auto-print scr-freeze-label scr-label-file rd_print ~
begin_date end_date begin_filename lbl_po-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-2 begin_date end_date 
&Scoped-define List-3 tb_per-unit tb_per-pallet 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD blockAccess C-Win 
FUNCTION blockAccess RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
  (ipField AS CHARACTER)  FORWARD.

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_filename AS CHARACTER FORMAT "X(256)":U 
     LABEL "Text File/Path" 
     VIEW-AS FILL-IN 
     SIZE 71 BY 1 NO-UNDO.

DEFINE VARIABLE begin_form AS INTEGER FORMAT ">>>":U INITIAL 3 
     LABEL "Printer Form#" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job AS CHARACTER FORMAT "X(12)":U 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job2 AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_labels AS INTEGER FORMAT ">>>>":U INITIAL 2 
     LABEL "# of Labels/Pallet" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Order#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rel AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Release#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_cas-lab AS CHARACTER FORMAT "X(30)":U 
     LABEL "Scan Label" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_po-no AS CHARACTER FORMAT "X(256)":U INITIAL "Print PO from?" 
      VIEW-AS TEXT 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE scr-label-file AS CHARACTER FORMAT "X(256)":U 
     LABEL "Label Matrix Label File" 
     VIEW-AS FILL-IN 
     SIZE 63 BY 1 NO-UNDO.

DEFINE VARIABLE rd_comps AS CHARACTER INITIAL "B" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Assembled", "A",
"Unassembled", "U",
"Both", "B"
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE rd_order-sts AS CHARACTER INITIAL "O" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "O",
"Close", "C",
"All", "A"
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE rd_print AS CHARACTER INITIAL "R" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Header", "H",
"Line", "L",
"Release", "R"
     SIZE 35 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 104 BY 18.81.

DEFINE VARIABLE scr-auto-print AS LOGICAL INITIAL no 
     LABEL "Auto Print Label?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.8 BY 1 NO-UNDO.

DEFINE VARIABLE scr-freeze-label AS LOGICAL INITIAL no 
     LABEL "Freeze Label File Choice?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE tb_16ths AS LOGICAL INITIAL no 
     LABEL "Show LWD in 16ths?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_over AS LOGICAL INITIAL no 
     LABEL "Include Overrun Qty?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE tb_per-pallet AS LOGICAL INITIAL no 
     LABEL "Units/Pallet?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE tb_per-unit AS LOGICAL INITIAL no 
     LABEL "Unit Count?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tb_print-comp AS LOGICAL INITIAL no 
     LABEL "Print Components?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE tb_rel AS LOGICAL INITIAL no 
     LABEL "Print by Release?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     fi_cas-lab AT ROW 2.38 COL 40 COLON-ALIGNED
     begin_job AT ROW 3.24 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job2 AT ROW 3.33 COL 55 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_ord-no AT ROW 4.29 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     begin_i-no AT ROW 5.24 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     begin_rel AT ROW 6.19 COL 40 COLON-ALIGNED HELP
          "Enter Beginning Item Number" WIDGET-ID 6
     tb_per-unit AT ROW 7.29 COL 52
     tb_per-pallet AT ROW 7.29 COL 68
     tb_over AT ROW 8.29 COL 40
     tb_print-comp AT ROW 9.24 COL 40
     scr-auto-print AT ROW 10.19 COL 40
     scr-freeze-label AT ROW 10.19 COL 63.2
     scr-label-file AT ROW 11.33 COL 38 COLON-ALIGNED
     rd_print AT ROW 12.67 COL 55 NO-LABEL
     begin_date AT ROW 13.71 COL 54 COLON-ALIGNED HELP
          "Enter Beginning Release Date" WIDGET-ID 2
     end_date AT ROW 13.71 COL 79.6 COLON-ALIGNED HELP
          "Enter Ending Release Date" WIDGET-ID 4
     rd_order-sts AT ROW 15.19 COL 55 NO-LABEL
     begin_form AT ROW 16.38 COL 19 COLON-ALIGNED
     rd_comps AT ROW 16.38 COL 55 NO-LABEL
     begin_labels AT ROW 17.33 COL 19 COLON-ALIGNED
     tb_rel AT ROW 17.38 COL 37
     tb_16ths AT ROW 17.38 COL 76
     begin_filename AT ROW 18.52 COL 19 COLON-ALIGNED
     btn-cancel AT ROW 20.19 COL 66
     btn-ok AT ROW 20.29 COL 24
     lbl_po-no AT ROW 12.67 COL 38 COLON-ALIGNED HELP
          "Print Customer's PO Number from Header, Line item or Release" NO-LABEL
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 2
          BGCOLOR 2 
     "Update Estimate, Order and FG Item --->" VIEW-AS TEXT
          SIZE 39 BY 1 AT ROW 7.29 COL 12
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 105.2 BY 21.38.


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
         TITLE              = "Case Label Creation"
         HEIGHT             = 21.71
         WIDTH              = 104.8
         MAX-HEIGHT         = 31.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 31.29
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
/* SETTINGS FOR FILL-IN begin_date IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_filename IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       begin_filename:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_form IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       begin_form:HIDDEN IN FRAME FRAME-A           = TRUE
       begin_form:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_job2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_labels IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       begin_labels:HIDDEN IN FRAME FRAME-A           = TRUE
       begin_labels:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_rel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

/* SETTINGS FOR FILL-IN end_date IN FRAME FRAME-A
   2                                                                    */
ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_po-no IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_po-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_print".

/* SETTINGS FOR RADIO-SET rd_comps IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rd_comps:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET rd_order-sts IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rd_order-sts:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       rd_print:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_16ths IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_16ths:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_16ths:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_over:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_per-pallet IN FRAME FRAME-A
   3                                                                    */
ASSIGN 
       tb_per-pallet:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_per-unit IN FRAME FRAME-A
   3                                                                    */
ASSIGN 
       tb_per-unit:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_print-comp:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_rel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_rel:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_rel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Case Label Creation */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Case Label Creation */
DO:
   IF INDEX(program-name(4),"asiLogin") <> 0 THEN
       RUN system/userLogOut.p (NO, 0).
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON HELP OF FRAME FRAME-A
DO:
  DEF VAR lv-handle AS HANDLE NO-UNDO.
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR rec-val AS RECID NO-UNDO.


  ASSIGN
   begin_ord-no
   begin_job
   begin_job2
   begin_i-no .

  IF begin_job NE "" AND LENGTH(begin_job) LT 6 THEN
    begin_job = FILL(" ",6 - LENGTH(TRIM(begin_job))) + TRIM(begin_job).

  lv-handle = FOCUS:HANDLE.

  CASE FOCUS:NAME:
    WHEN "begin_ord-no" THEN DO:
      RUN windows/l-ordl.w (g_company, begin_ord-no:screen-value, output char-val, output rec-val). 
      FIND oe-ordl WHERE RECID(oe-ordl) EQ rec-val NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN do:
        ASSIGN
         begin_ord-no:SCREEN-VALUE = STRING(oe-ordl.ord-no)
         begin_job:SCREEN-VALUE    = oe-ordl.job-no
         begin_job2:SCREEN-VALUE   = STRING(oe-ordl.job-no2)
         begin_i-no:SCREEN-VALUE   = oe-ordl.i-no.
         RUN check-release .
      END.
    END.
    WHEN "begin_i-no" THEN DO:
      RUN windows/l-itemf3.w (g_company,begin_ord-no,begin_job,begin_job2,begin_i-no, OUTPUT char-val, OUTPUT rec-val).
      IF char-val <> "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
      RUN check-release .
    END.
    WHEN "begin_rel" THEN DO:
      RUN windows/l-ordrel.w (g_company,begin_ord-no:screen-value,begin_i-no:screen-value, OUTPUT char-val, OUTPUT rec-val).
      IF char-val <> "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
    END.
    OTHERWISE do:
      lv-handle = FOCUS:HANDLE.
      RUN applhelp.p.

      IF g_lookup-var NE "" THEN lv-handle:SCREEN-VALUE = g_lookup-var.
    END.  /* otherwise */
  END CASE.

  APPLY "entry" TO lv-handle.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_filename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_filename C-Win
ON HELP OF begin_filename IN FRAME FRAME-A /* Text File/Path */
DO:
   def var ls-filename as cha no-undo.
   def var ll-ok as log no-undo.

   system-dialog get-dir ls-filename 
                 title "Select Path to save"
                 initial-dir begin_filename
                 UPDATE ll-ok.

    IF ll-ok THEN self:screen-value = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_filename C-Win
ON LEAVE OF begin_filename IN FRAME FRAME-A /* Text File/Path */
DO:
  assign begin_filename.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_form
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_form C-Win
ON LEAVE OF begin_form IN FRAME FRAME-A /* Printer Form# */
DO:
  assign begin_form.

  begin_filename = "barcode" + string(begin_form) + ".frm".

  display begin_filename WITH FRAME FRAME-A IN WINDOW C-Win.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Item# */
DO:
   DEF VAR v-cust-no AS CHAR NO-UNDO.

   IF v-auto-print AND LOGICAL(scr-freeze-label:SCREEN-VALUE) EQ NO THEN
   DO:
      RELEASE oe-ord.

      FIND FIRST job-hdr WHERE
           job-hdr.company EQ cocode AND
           job-hdr.job-no EQ begin_job:SCREEN-VALUE AND
           job-hdr.job-no2 EQ INT(begin_job2:SCREEN-VALUE) AND
           begin_job:SCREEN-VALUE NE ""
           NO-LOCK NO-ERROR.

      IF AVAIL job-hdr THEN
         v-cust-no = job-hdr.cust-no.
      ELSE
      DO:
         FIND FIRST oe-ord WHERE
              oe-ord.company EQ cocode AND
              oe-ord.ord-no EQ INT(begin_ord-no:SCREEN-VALUE)
              NO-LOCK NO-ERROR.

         IF AVAIL oe-ord THEN
            v-cust-no = oe-ord.cust-no.
      END.

      IF v-cust-no NE "" THEN
         FIND FIRST cust-part WHERE
              cust-part.company = cocode AND
              cust-part.i-no    = begin_i-no:SCREEN-VALUE AND
              cust-part.cust-no = v-cust-no
              NO-LOCK NO-ERROR.

      IF AVAIL cust-part AND cust-part.labelCase NE "" THEN
         scr-label-file:SCREEN-VALUE = cust-part.labelCase.
      ELSE
      IF INT(begin_ord-no:SCREEN-VALUE) NE 0 THEN DO:

         FIND FIRST oe-rel WHERE
              oe-rel.company eq cocode AND
              oe-rel.i-no    eq begin_i-no:SCREEN-VALUE AND
              oe-rel.ord-no  eq INT(begin_ord-no:SCREEN-VALUE)
              NO-LOCK NO-ERROR.

         IF AVAIL oe-rel THEN 
            FIND FIRST shipto WHERE
                 shipto.company eq cocode AND
                 shipto.cust-no eq oe-rel.cust-no AND
                 shipto.ship-id eq oe-rel.ship-id
                 USE-INDEX ship-id NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST shipto WHERE
                 shipto.company eq cocode AND
                 shipto.cust-no eq v-cust-no AND
                 shipto.ship-id eq v-cust-no
                 USE-INDEX ship-id NO-LOCK NO-ERROR.

         IF AVAIL shipto THEN
         DO:
            IF AVAIL oe-rel THEN
               v-cust-no = oe-rel.cust-no.

            FIND FIRST sys-ctrl-shipto WHERE
                 sys-ctrl-shipto.company = cocode AND
                 sys-ctrl-shipto.NAME = "CASLABEL" AND
                 sys-ctrl-shipto.cust-vend = YES AND
                 sys-ctrl-shipto.cust-vend-no = v-cust-no AND
                 sys-ctrl-shipto.ship-id = shipto.ship-id AND
                 sys-ctrl-shipto.char-fld NE ''
                 NO-LOCK NO-ERROR.

            IF AVAIL sys-ctrl-shipto AND 
               TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
               scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.

            ELSE DO:
                FIND FIRST sys-ctrl-shipto NO-LOCK 
                    WHERE sys-ctrl-shipto.company      EQ cocode 
                      AND sys-ctrl-shipto.NAME         EQ "CASLABEL" 
                      AND sys-ctrl-shipto.cust-vend    EQ YES 
                      AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                      AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                IF AVAIL sys-ctrl-shipto AND 
                   TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                   scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.

                ELSE DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK 
                        WHERE sys-ctrl-shipto.company      EQ cocode 
                          AND sys-ctrl-shipto.NAME         EQ "CASLABEL" 
                          AND sys-ctrl-shipto.cust-vend    EQ YES
                          AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no NO-ERROR.
                    IF AVAIL sys-ctrl-shipto AND 
                        TRIM(sys-ctrl-shipto.char-fld) NE "" 
                      THEN 
                          scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
                      ELSE DO:
                          FIND FIRST sys-ctrl NO-LOCK 
                              WHERE sys-ctrl.company EQ cocode 
                               AND sys-ctrl.name    EQ "CASLABEL" NO-ERROR.
                          IF AVAIL sys-ctrl 
                            THEN
                                scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                            ELSE
                                scr-label-file:SCREEN-VALUE = "".
                      END.
                END.
            END.
         END.
         ELSE
         DO:
            FIND FIRST sys-ctrl-shipto NO-LOCK 
                WHERE sys-ctrl-shipto.company      EQ cocode 
                  AND sys-ctrl-shipto.NAME         EQ "CASLABEL" 
                  AND sys-ctrl-shipto.cust-vend    EQ YES 
                  AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                  AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
            IF AVAIL sys-ctrl-shipto AND 
               TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
               scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.

            ELSE DO:
               FIND FIRST sys-ctrl NO-LOCK 
                   WHERE sys-ctrl.company EQ cocode 
                    AND sys-ctrl.name    EQ "CASLABEL" NO-ERROR.
               IF AVAIL sys-ctrl THEN
                  scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
               ELSE
                  scr-label-file:SCREEN-VALUE = "".

            END.
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job C-Win
ON LEAVE OF begin_job IN FRAME FRAME-A /* Job# */
DO:
  /*ESP - Only fire on leave of job no 2*/
  IF LASTKEY NE -1 THEN DO:
    RUN get-jobord-info.
    
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job2 C-Win
ON LEAVE OF begin_job2 IN FRAME FRAME-A /* - */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN new-job NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      APPLY "entry" TO begin_job.
      RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* Order# */
DO:
   DEF VAR v-cust-no AS CHAR NO-UNDO.

   FIND FIRST oe-ordl WHERE oe-ordl.company = cocode
                        AND oe-ordl.ord-no = INT(begin_ord-no:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF AVAIL oe-ordl THEN begin_i-no:SCREEN-VALUE = oe-ordl.i-no.

   RUN check-release .

   IF v-auto-print AND LOGICAL(scr-freeze-label:SCREEN-VALUE) EQ NO THEN
   DO:
      IF INT(begin_ord-no:SCREEN-VALUE) NE 0 THEN DO:

          v-lcnt = 0.
          FOR EACH oe-rel NO-LOCK 
              WHERE oe-rel.company EQ cocode 
                AND oe-rel.ord-no  EQ INT(begin_ord-no:SCREEN-VALUE):
              v-lcnt = v-lcnt + 1.
              IF v-lcnt GT 1 THEN LEAVE.
          END.


          IF v-lcnt GT 1 AND 
             begin_i-no:SCREEN-VALUE EQ "" AND v-casflg THEN DO:

             MESSAGE 
                 "Item # can not be blank. Please enter an Item #."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

             APPLY "ENTRY" TO begin_i-no IN FRAME {&FRAME-NAME}.
             RETURN NO-APPLY.
          END.

         FIND FIRST oe-ord WHERE
              oe-ord.company EQ cocode AND
              oe-ord.ord-no EQ INT(begin_ord-no:SCREEN-VALUE)
              NO-LOCK NO-ERROR.

         v-cust-no = IF AVAIL oe-ord THEN oe-ord.cust-no ELSE "".

         IF AVAIL oe-ord THEN
            FIND FIRST cust-part WHERE
                 cust-part.company = cocode AND
                 cust-part.i-no    = begin_i-no:SCREEN-VALUE AND
                 cust-part.cust-no = oe-ord.cust-no
                 NO-LOCK NO-ERROR.

         IF AVAIL cust-part AND cust-part.labelCase NE "" THEN
            scr-label-file:SCREEN-VALUE = cust-part.labelCase.
         ELSE
         DO:
            IF begin_i-no:SCREEN-VALUE NE "" THEN
               FIND FIRST oe-rel WHERE
                    oe-rel.company eq cocode AND
                    oe-rel.i-no    eq begin_i-no:SCREEN-VALUE AND
                    oe-rel.ord-no  eq INT(begin_ord-no:SCREEN-VALUE)
                    NO-LOCK NO-ERROR.
            ELSE
               FIND FIRST oe-rel WHERE
                    oe-rel.company eq cocode AND
                    oe-rel.ord-no  eq INT(begin_ord-no:SCREEN-VALUE)
                    NO-LOCK NO-ERROR.

            IF AVAIL oe-rel THEN 
               FIND FIRST shipto WHERE
                    shipto.company eq cocode AND
                    shipto.cust-no eq oe-rel.cust-no AND
                    shipto.ship-id eq oe-rel.ship-id
                    USE-INDEX ship-id NO-LOCK NO-ERROR.
            ELSE
               FIND FIRST shipto WHERE
                    shipto.company eq cocode AND
                    shipto.cust-no eq v-cust-no AND
                    shipto.ship-id eq v-cust-no
                    USE-INDEX ship-id NO-LOCK NO-ERROR.

            IF AVAIL shipto THEN
            DO:
               if avail oe-rel then
                  v-cust-no = oe-rel.cust-no.

               FIND FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "CASLABEL" AND
                    sys-ctrl-shipto.cust-vend = YES AND
                    sys-ctrl-shipto.cust-vend-no = v-cust-no AND
                    sys-ctrl-shipto.ship-id = shipto.ship-id AND
                    sys-ctrl-shipto.char-fld NE ''
                    NO-LOCK NO-ERROR.

               IF AVAIL sys-ctrl-shipto AND 
                   TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                  scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.

               /* gdm - 11050806 */
               ELSE 
                   FIND FIRST sys-ctrl-shipto NO-LOCK  
                       WHERE sys-ctrl-shipto.company      EQ cocode 
                         AND sys-ctrl-shipto.NAME         EQ "CASLABEL" 
                         AND sys-ctrl-shipto.cust-vend    EQ YES 
                         AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                         AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.

                   IF AVAIL sys-ctrl-shipto AND 
                   TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                       scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.

                   IF scr-label-file:SCREEN-VALUE EQ "" THEN  DO:
                         FIND FIRST sys-ctrl NO-LOCK 
                             WHERE sys-ctrl.company EQ cocode 
                               AND sys-ctrl.name    EQ "CASLABEL" NO-ERROR.
                         IF AVAIL sys-ctrl THEN
                            scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                   END.
            END.
            ELSE
            DO:
               FIND FIRST sys-ctrl-shipto NO-LOCK  
                   WHERE sys-ctrl-shipto.company      EQ cocode 
                     AND sys-ctrl-shipto.NAME         EQ "CASLABEL" 
                     AND sys-ctrl-shipto.cust-vend    EQ YES 
                     AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                     AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.

               IF AVAIL sys-ctrl-shipto AND 
                  TRIM(sys-ctrl-shipto.char-fld) NE "" THEN
                  scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.

               IF scr-label-file:SCREEN-VALUE EQ "" THEN  DO:
                     FIND FIRST sys-ctrl NO-LOCK 
                         WHERE sys-ctrl.company EQ cocode 
                           AND sys-ctrl.name    EQ "CASLABEL" NO-ERROR.
                     IF AVAIL sys-ctrl THEN
                        scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
               END.
            END.

         END.         
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rel C-Win
ON LEAVE OF begin_rel IN FRAME FRAME-A /* Release# */
DO:
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   IF INDEX(program-name(4),"asiLogin") <> 0 THEN
       RUN system/userLogOut.p (NO, 0).
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO: 

    IF begin_i-no:SCREEN-VALUE EQ "" AND 
       (begin_job:SCREEN-VALUE NE "" OR begin_job:SCREEN-VALUE NE "" OR 
        begin_ord-no:SCREEN-VALUE NE "") AND 
       v-casflg 
       THEN DO:

        v-lcnt = 0.
        FOR EACH oe-rel NO-LOCK 
              WHERE oe-rel.company EQ cocode 
                AND oe-rel.ord-no  EQ INT(begin_ord-no:SCREEN-VALUE):
              v-lcnt = v-lcnt + 1.
              IF v-lcnt GT 1 THEN LEAVE.
        END.

        IF v-lcnt GT 1 THEN DO: 

            MESSAGE 
                "Item # can not be blank. Please enter an Item #."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.

            APPLY "ENTRY" TO begin_i-no IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
        END.

    END.

    RUN ok-button.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cas-lab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cas-lab C-Win
ON LEAVE OF fi_cas-lab IN FRAME FRAME-A /* Scan Label */
DO:
  DEF VAR v-cust-no AS CHAR NO-UNDO.

  IF SELF:SCREEN-VALUE NE "" AND SELF:MODIFIED THEN DO:      

    ASSIGN
      fi_cas-lab:SCREEN-VALUE   = TRIM(fi_cas-lab:SCREEN-VALUE)
      begin_ord-no:SCREEN-VALUE = SUBSTRING(fi_cas-lab:SCREEN-VALUE,16,6)
      begin_job:SCREEN-VALUE    = begin_ord-no:SCREEN-VALUE
      begin_job2:SCREEN-VALUE   = SUBSTRING(fi_cas-lab:SCREEN-VALUE,22,2)
      begin_i-no:SCREEN-VALUE   = SUBSTRING(fi_cas-lab:SCREEN-VALUE,1,15).

    IF v-auto-print AND LOGICAL(scr-freeze-label:SCREEN-VALUE) EQ NO THEN
    DO:
       FIND FIRST job-hdr WHERE
            job-hdr.company EQ cocode AND
            job-hdr.job-no EQ begin_job:SCREEN-VALUE AND
            job-hdr.job-no2 EQ INT(begin_job2:SCREEN-VALUE)
            NO-LOCK NO-ERROR.

       IF AVAIL job-hdr THEN
          v-cust-no = job-hdr.cust-no.
       ELSE
       DO:
           FIND FIRST oe-ord WHERE
                oe-ord.company EQ cocode AND
                oe-ord.ord-no EQ INT(begin_ord-no:SCREEN-VALUE)
                NO-LOCK NO-ERROR.

           IF AVAIL oe-ord THEN
              v-cust-no = oe-ord.cust-no.
       END.

       IF v-cust-no NE "" THEN
          FIND FIRST cust-part WHERE
               cust-part.company = cocode AND
               cust-part.i-no    = begin_i-no:SCREEN-VALUE AND
               cust-part.cust-no = v-cust-no
               NO-LOCK NO-ERROR.

       IF AVAIL cust-part AND cust-part.labelCase NE "" THEN
          scr-label-file:SCREEN-VALUE = cust-part.labelCase.
       ELSE
       IF INT(begin_ord-no:SCREEN-VALUE) NE 0 THEN DO:

          FIND FIRST oe-rel WHERE
               oe-rel.company eq cocode AND
               oe-rel.i-no    eq begin_i-no:SCREEN-VALUE AND
               oe-rel.ord-no  eq INT(begin_ord-no:SCREEN-VALUE)
               NO-LOCK NO-ERROR.

          IF AVAIL oe-rel THEN 
             FIND FIRST shipto WHERE
                  shipto.company eq cocode AND
                  shipto.cust-no eq oe-rel.cust-no AND
                  shipto.ship-id eq oe-rel.ship-id
                  USE-INDEX ship-id NO-LOCK NO-ERROR.
          ELSE
             FIND FIRST shipto WHERE
                  shipto.company eq cocode AND
                  shipto.cust-no eq v-cust-no AND
                  shipto.ship-id eq v-cust-no
                  USE-INDEX ship-id NO-LOCK NO-ERROR.

          IF AVAIL shipto THEN
          DO:
             if avail oe-rel then
                v-cust-no = oe-rel.cust-no.

             FIND FIRST sys-ctrl-shipto WHERE
                  sys-ctrl-shipto.company = cocode AND
                  sys-ctrl-shipto.NAME = "CASLABEL" AND
                  sys-ctrl-shipto.cust-vend = YES AND
                  sys-ctrl-shipto.cust-vend-no = v-cust-no AND
                  sys-ctrl-shipto.ship-id = shipto.ship-id AND
                  sys-ctrl-shipto.char-fld NE ''
                  NO-LOCK NO-ERROR.

             IF AVAIL sys-ctrl-shipto THEN
                scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.
          END.
          ELSE
          DO:
             FIND FIRST sys-ctrl-shipto NO-LOCK 
                 WHERE sys-ctrl-shipto.company      EQ cocode 
                   AND sys-ctrl-shipto.NAME         EQ "CASLABEL" 
                   AND sys-ctrl-shipto.cust-vend    EQ YES 
                   AND sys-ctrl-shipto.cust-vend-no EQ v-cust-no 
                   AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
             IF AVAIL sys-ctrl-shipto AND 
                TRIM(sys-ctrl-shipto.char-fld) NE "" THEN 
                scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.

             ELSE DO:
                FIND FIRST sys-ctrl NO-LOCK 
                    WHERE sys-ctrl.company EQ cocode 
                     AND sys-ctrl.name    EQ "CASLABEL" NO-ERROR.
                IF AVAIL sys-ctrl THEN
                   scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                ELSE
                   scr-label-file:SCREEN-VALUE = "".

             END.
          END.
       END.
    END.

    /* gdm - 01280911 */
    APPLY "LEAVE" TO begin_i-no IN FRAME {&FRAME-NAME}.

    RUN ok-button.
    IF RETURN-VALUE NE 'ERROR' THEN DO:
      SELF:SCREEN-VALUE = ''.
      APPLY 'ENTRY':U TO SELF.
      RETURN NO-APPLY.
    END.
    ELSE DO:
      APPLY 'ENTRY':U TO tb_per-unit.
      RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cas-lab C-Win
ON RETURN OF fi_cas-lab IN FRAME FRAME-A /* Scan Label */
DO:
  APPLY 'TAB' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_print C-Win
ON VALUE-CHANGED OF rd_print IN FRAME FRAME-A
DO:
   IF {&self-name}:SCREEN-VALUE EQ "R" THEN DO:
    ASSIGN
     begin_date:SCREEN-VALUE = string(DATE(1,1,YEAR(TODAY)),"99/99/9999")
     end_date:SCREEN-VALUE = string(DATE(12,31,YEAR(TODAY)),"99/99/9999")
     begin_date:SENSITIVE = YES
     end_date:SENSITIVE   = YES.
    APPLY "entry" TO begin_date.
  END.
  ELSE
    ASSIGN begin_date:SCREEN-VALUE = ""
           END_date:SCREEN-VALUE = ""
           begin_date:SENSITIVE = NO
           end_date:SENSITIVE   = NO.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-label-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-label-file C-Win
ON HELP OF scr-label-file IN FRAME FRAME-A /* Label Matrix Label File */
DO:
   DEF VAR chFile AS CHAR FORMAT "X(80)" NO-UNDO.
   DEF VAR ll-ok AS LOG NO-UNDO.

   /* gdm - 11050804 */
   DEF VAR v-path AS CHAR NO-UNDO.


   ASSIGN v-path = TRIM(scr-label-file:SCREEN-VALUE).

    IF TRIM(v-path) EQ "" THEN DO:
        FIND FIRST sys-ctrl NO-LOCK 
            WHERE sys-ctrl.company EQ cocode
              AND sys-ctrl.name EQ "CASLABEL" NO-ERROR.
        IF AVAIL sys-ctrl THEN
            ASSIGN v-path = TRIM(sys-ctrl.char-fld).

    END.
    RUN sys\ref\char-fld-help.w(INPUT cocode,
                                INPUT v-path,
                                OUTPUT chFile).


   /* gdm - 11050804 end

   DO WITH FRAME {&FRAME-NAME}:
      system-dialog get-file chFile 
                    title "Select Label Matrix Label File"
                    filters "Label Matrix (*.qdf) " "*.qdf"
                    initial-dir v_path
                    MUST-EXIST
                    USE-FILENAME
                    UPDATE ll-ok.

      IF ll-ok THEN
   */   
      ASSIGN scr-label-file:SCREEN-VALUE = chFile.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


PROCEDURE WinExec EXTERNAL "KERNEL32.DLL":
     DEFINE INPUT  PARAMETER ProgramName AS CHARACTER.
     DEFINE INPUT  PARAMETER VisualStyle AS LONG.
     DEFINE RETURN PARAMETER StatusCode  AS LONG.
END PROCEDURE.



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

  FIND FIRST company WHERE company.company EQ gcompany NO-LOCK.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "CEMENU"
      NO-LOCK NO-ERROR.
  ASSIGN
   tb_16ths  = AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "Corrware".

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company eq gcompany
        AND sys-ctrl.name    eq "LOADTAG"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = gcompany
     sys-ctrl.name     = "LOADTAG"
     sys-ctrl.descrip  = "Special Load tag print options, e.g. barcode printer"
     sys-ctrl.char-fld = "ASI".
    MESSAGE "System control record NOT found.  Please enter the load tag option"
            UPDATE sys-ctrl.char-fld.
  END.
  assign
   v-loadtag = sys-ctrl.char-fld
   v-mult    = sys-ctrl.int-fld.

  if v-loadtag eq "TRIAD" then begin_form = 4.
  if v-mult le 0 then v-mult = 1.

  DO TRANSACTION:
     {sys/inc/bardir.i}
     {sys/inc/casetag.i}
  END.
  ASSIGN userLabelPath = bardir-desc
         begin_filename = bardir-desc.
  IF casetag-int = 1 THEN DO:
       FIND FIRST users WHERE users.user_id EQ USERID("NOSWEAT") NO-LOCK NO-ERROR.
       IF AVAIL users AND users.user_program[3] NE "" THEN
           ASSIGN begin_filename:SCREEN-VALUE = users.user_program[3]
                  userLabelPath = users.USER_program[3].       
  END.

  RUN enable_UI.

/*  
  if v-loadtag ne "TRIAD" then
    DISABLE begin_form begin_labels begin_filename WITH FRAME FRAME-A.
*/
  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    begin_filename:SCREEN-VALUE = bardir-desc.
    IF casetag-int = 1 THEN DO:
       FIND FIRST users WHERE users.user_id EQ USERID("NOSWEAT") NO-LOCK NO-ERROR.
       IF AVAIL users AND users.user_program[3] NE "" THEN
          ASSIGN begin_filename:SCREEN-VALUE = users.user_program[3].           
    END.
    ASSIGN
     fi_cas-lab:SCREEN-VALUE = ""
     begin_ord-no:SCREEN-VALUE = ""
     begin_job:SCREEN-VALUE    = ""
     begin_job2:SCREEN-VALUE   = ""
     begin_i-no:SCREEN-VALUE   = ""
     begin_rel:SCREEN-VALUE    = ""
     begin_rel:SENSITIVE   = NO.

    FIND FIRST sys-ctrl WHERE
         sys-ctrl.company eq gcompany AND
         sys-ctrl.name    eq "CASLABEL"
         NO-LOCK NO-ERROR.

    IF NOT AVAIL sys-ctrl THEN
       DO TRANSACTION:
          CREATE sys-ctrl.
          ASSIGN
             sys-ctrl.company  = gcompany
             sys-ctrl.name     = "CASLABEL"
             sys-ctrl.descrip  = "Auto Print Case Label?".
       END.

    ASSIGN
       v-auto-print   = sys-ctrl.log-fld
       scr-auto-print:SCREEN-VALUE = STRING(sys-ctrl.log-fld)
       scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld
       v-casflg = asi.sys-ctrl.log-fld.

    IF rd_print:SCREEN-VALUE NE "R" THEN DISABLE begin_date end_date.
    ELSE DO:
         ASSIGN begin_date = DATE(1,1,YEAR(TODAY))
                END_date = DATE(12,31,YEAR(TODAY)).
         DISPLAY begin_date END_date.
    END.
    DISPLAY lbl_po-no.
    APPLY "entry" TO fi_cas-lab.

    IF begin_filename:SCREEN-VALUE = "" THEN
        begin_filename:SCREEN-VALUE = userLabelPath.
    IF blockAccess() THEN DO:
          ASSIGN {&List-3}.
          DISPLAY {&List-3}.
          DISABLE {&List-3}.  /*List-3 are the advanced options that should be disabled*/
    END.
  END.



  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AutoPrint C-Win 
PROCEDURE AutoPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cBarDir AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDB AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lUserSpecific AS LOGICAL     NO-UNDO.

IF scr-auto-print THEN DO:
    RUN sys/ref/GetBarDir.p (INPUT cocode,
                             INPUT "CaseLabel",
                             OUTPUT cBarDir,
                             OUTPUT cDB,
                             OUTPUT lUserSpecific).

    IF lUserSpecific THEN 
        RUN custom/lmprint.p (INPUT scr-label-file, 
                              INPUT cDB,
                              INPUT cBarDir).
    ELSE
        RUN custom/lmprint.p (INPUT scr-label-file,
                              INPUT "",
                              INPUT "").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dispJobInfo C-Win 
PROCEDURE dispJobInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------ ------------------------------------------------*/
DEF INPUT PARAMETER ipcCompany      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcJobNo    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiJobNo2   AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipiForm     AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipiBlank    AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER iplCheckBar AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER iplCheckBarBlank AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER oplCheckBar AS LOGICAL     NO-UNDO.

DEFINE BUFFER bf-job FOR job.
DEFINE BUFFER bf-job-hdr-2 FOR job-hdr.
DEFINE VARIABLE v-lncnt AS INT NO-UNDO.
DEFINE VARIABLE v-frstitem AS CHAR NO-UNDO.
DEFINE VARIABLE v-lastitem AS CHAR NO-UNDO.
DEFINE VARIABLE v-first-order AS INT NO-UNDO.
DEFINE VARIABLE v-last-order AS INT NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:

   FIND FIRST bf-job WHERE
        bf-job.company EQ cocode AND
        bf-job.job-no EQ ipcJobNo AND
        bf-job.job-no2 EQ ipiJobNo2
        NO-LOCK NO-ERROR.

   IF AVAIL bf-job THEN
   DO:
      FOR EACH bf-job-hdr-2 FIELDS(i-no frm blank-no) NO-LOCK
          WHERE bf-job-hdr-2.company EQ bf-job.company
            AND bf-job-hdr-2.job-no  EQ bf-job.job-no
            AND bf-job-hdr-2.job-no2 EQ bf-job.job-no2
            AND ( bf-job-hdr-2.frm EQ ipiForm OR NOT iplCheckBar )
            AND ( bf-job-hdr-2.blank-no EQ ipiBlank OR NOT iplCheckBarBlank )
           BREAK BY bf-job-hdr-2.i-no:

           v-lncnt = v-lncnt + 1.

           IF FIRST-OF(bf-job-hdr-2.i-no) THEN
              v-frstitem = bf-job-hdr-2.i-no.
           IF LAST-OF(bf-job-hdr-2.i-no) THEN
              v-lastitem = bf-job-hdr-2.i-no.
      END.

      FOR EACH bf-job-hdr-2 FIELDS(ord-no frm blank-no) NO-LOCK
          WHERE bf-job-hdr-2.company EQ bf-job.company
            AND bf-job-hdr-2.job-no  EQ bf-job.job-no
            AND bf-job-hdr-2.job-no2 EQ bf-job.job-no2
            AND ( bf-job-hdr-2.frm EQ ipiForm OR NOT iplCheckBar )
            AND ( bf-job-hdr-2.blank-no EQ ipiBlank OR NOT iplCheckBarBlank )
           BREAK BY bf-job-hdr-2.ord-no:

           IF FIRST-OF(bf-job-hdr-2.ord-no) THEN
              v-first-order = bf-job-hdr-2.ord-no.
           IF LAST-OF(bf-job-hdr-2.ord-no) THEN
              v-last-order = bf-job-hdr-2.ord-no.
      END.
  
  
      ASSIGN
         begin_ord-no:SCREEN-VALUE = STRING(v-first-order)
         begin_job:SCREEN-VALUE    = ipcJobNo         
         begin_job2:SCREEN-VALUE   = STRING(ipiJobNo2,"99")
         begin_i-no:SCREEN-VALUE   = v-lastitem.           

      APPLY "LEAVE" TO begin_i-no.
      IF v-lncnt EQ 1 THEN
          oplCheckBar = YES . 
      /*IF v-lncnt GT 1 THEN
         MESSAGE "There are multiple FG Items on this order." skip
                 "Please select an FG Item."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.*/ /* warning for malti fgitem*/
   END.
END.
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
  DISPLAY fi_cas-lab begin_job begin_job2 begin_ord-no begin_i-no begin_rel 
          tb_per-unit tb_per-pallet tb_over tb_print-comp scr-auto-print 
          scr-freeze-label scr-label-file rd_print begin_date end_date 
          begin_filename lbl_po-no 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-7 fi_cas-lab begin_job begin_job2 begin_ord-no begin_i-no 
         begin_rel tb_per-unit tb_per-pallet tb_over tb_print-comp 
         scr-auto-print scr-freeze-label scr-label-file rd_print begin_date 
         end_date btn-cancel btn-ok 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE from-job C-Win 
PROCEDURE from-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM vlWarning AS LOG NO-UNDO.

    FIND FIRST job
        WHERE ROWID(job) EQ ip-rowid
          AND (v-stat EQ "A"                      OR
               (v-stat EQ "C" AND NOT job.opened) OR
               (v-stat EQ "O" AND job.opened))
        NO-LOCK NO-ERROR.

    IF AVAIL job THEN
    FOR EACH job-hdr
        WHERE job-hdr.company EQ job.company
          AND job-hdr.job     EQ job.job
          AND job-hdr.job-no  EQ job.job-no
          AND job-hdr.job-no2 EQ job.job-no2

         /* AND job-hdr.i-no    GE v-fitem[1]
          AND job-hdr.i-no    LE v-fitem[2] */

         AND (job-hdr.i-no = v-fitem OR v-fitem = "")
         /*  AND job-hdr.ord-no  EQ 0

        USE-INDEX ord-no*/ NO-LOCK,
        FIRST cust
        WHERE cust.company eq cocode
          AND cust.cust-no eq job-hdr.cust-no
        NO-LOCK,
        FIRST itemfg
        WHERE itemfg.company eq cocode
          AND itemfg.i-no    eq job-hdr.i-no
        NO-LOCK:

          CREATE w-ord.

          ASSIGN
            w-ord.ord-no       = job-hdr.ord-no
            w-ord.job-no       = job-hdr.job-no
            w-ord.job-no2      = job-hdr.job-no2
            w-ord.cust-no      = cust.cust-no
            w-ord.cust-name    = cust.name
            w-ord.i-no         = job-hdr.i-no
            w-ord.cust-part-no = itemfg.part-no
            w-ord.ord-qty      = job-hdr.qty
            w-ord.i-name       = itemfg.i-name
            w-ord.part-dscr1   = itemfg.part-dscr1
            w-ord.part-dscr2   = itemfg.part-dscr2
            w-ord.part-dscr3   = itemfg.part-dscr3
            w-ord.upc-no       = itemfg.upc-no
            w-ord.due-date     = job.start-date
            w-ord.est-no       = job.est-no
            w-ord.form-no      = job-hdr.frm
            w-ord.upc-no       = itemfg.upc-no
            w-ord.box-len      = itemfg.l-score[50]
            w-ord.box-wid      = itemfg.w-score[50]
            w-ord.box-dep      = itemfg.d-score[50]
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = if cust.int-field[1] ne 0 then
                                   cust.int-field[1] else v-mult
            w-ord.prod-notes   = itemfg.prod-notes
            num-rec            = num-rec + 1.

          IF job-hdr.ord-no NE 0 THEN
          DO:
             FIND FIRST oe-ordl WHERE
                  oe-ordl.company EQ cocode AND
                  oe-ordl.ord-no  EQ job-hdr.ord-no AND
                  oe-ordl.i-no    EQ job-hdr.i-no
                  NO-LOCK NO-ERROR.

             IF AVAIL oe-ordl THEN
             DO:
               FIND FIRST oe-ord WHERE oe-ord.company EQ oe-ordl.company 
                                   AND oe-ord.ord-no EQ oe-ordl.ord-no NO-LOCK NO-ERROR.
/*                 w-ord.cust-po-no  = oe-ordl.po-no. */
               RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                                 OUTPUT w-ord.rel-date,
                                 OUTPUT w-ord.rel-lot#).

                w-ord.po-no        = oe-ordl.po-no-po.
                FIND FIRST oe-rel WHERE
                     oe-rel.company eq cocode AND
                     oe-rel.i-no    eq oe-ordl.i-no AND
                     oe-rel.ord-no  eq oe-ordl.ord-no
                     NO-LOCK NO-ERROR.

                FIND FIRST oe-relh NO-LOCK
                    WHERE oe-relh.company EQ cocode 
                    AND oe-relh.release# EQ begin_rel NO-ERROR .
                IF AVAIL oe-relh THEN
                    FIND FIRST oe-rell  NO-LOCK     
                    WHERE oe-rell.company   EQ oe-relh.company  
                    AND oe-rell.r-no      EQ oe-relh.r-no NO-ERROR.   

                IF AVAIL oe-rell THEN
                    w-ord.ord-qty      = oe-rell.qty .

           
                RELEASE oe-ordl.
                RELEASE oe-ord.
             END.
          END.
          ELSE
             vlWarning = YES.

          ASSIGN
           w-ord.cust-add1  = cust.addr[1]
           w-ord.cust-add2  = cust.addr[2]
           w-ord.cust-city  = cust.city
           w-ord.cust-state = cust.state
           w-ord.cust-zip   = cust.zip
           w-ord.cust-ctry  = cust.country
           w-ord.sold-name  = w-ord.cust-name
           w-ord.sold-add1  = w-ord.cust-add1
           w-ord.sold-add2  = w-ord.cust-add2
           w-ord.sold-city  = w-ord.cust-city
           w-ord.sold-state = w-ord.cust-state
           w-ord.sold-zip   = w-ord.cust-zip
           w-ord.sold-ctry  = w-ord.cust-ctry
           w-ord.ship-name  = w-ord.cust-name
           w-ord.ship-add1  = w-ord.cust-add1
           w-ord.ship-add2  = w-ord.cust-add2
           w-ord.ship-city  = w-ord.cust-city
           w-ord.ship-state = w-ord.cust-state
           w-ord.ship-zip   = w-ord.cust-zip
           w-ord.ship-ctry  = w-ord.cust-ctry.

          FOR EACH soldto
              WHERE soldto.company eq cocode
                AND soldto.cust-no eq job-hdr.cust-no
              USE-INDEX sold-id NO-LOCK
              BREAK BY soldto.sold-no:
            IF LAST(soldto.sold-no)              OR
               soldto.sold-id EQ job-hdr.cust-no THEN DO:
              ASSIGN
               w-ord.sold-name  = soldto.sold-name
               w-ord.sold-add1  = soldto.sold-add[1]
               w-ord.sold-add2  = soldto.sold-add[2]
               w-ord.sold-city  = soldto.sold-city
               w-ord.sold-state = soldto.sold-state
               w-ord.sold-zip   = soldto.sold-zip
               w-ord.sold-ctry  = soldto.country.
              LEAVE.
            END.
          END.

          IF AVAIL oe-rel THEN
          DO:
             FIND FIRST shipto WHERE
                  shipto.company eq cocode AND
                  shipto.cust-no eq job-hdr.cust-no AND
                  shipto.ship-id eq oe-rel.ship-id
                  NO-LOCK NO-ERROR.

             IF AVAIL shipto THEN
                ASSIGN
                   w-ord.ship-name  = shipto.ship-name
                   w-ord.ship-add1  = shipto.ship-add[1]
                   w-ord.ship-add2  = shipto.ship-add[2]
                   w-ord.ship-city  = shipto.ship-city
                   w-ord.ship-state = shipto.ship-state
                   w-ord.ship-zip   = shipto.ship-zip
                   w-ord.ship-ctry  = shipto.country.

             RELEASE oe-rel.
          END.
          ELSE
             FOR EACH shipto
                 WHERE shipto.company eq cocode
                   AND shipto.cust-no eq job-hdr.cust-no
                 USE-INDEX ship-id NO-LOCK
                 BREAK BY shipto.ship-no:
               IF LAST(shipto.ship-no)              OR
                  shipto.ship-id EQ job-hdr.cust-no THEN DO:
                 ASSIGN
                  w-ord.ship-name  = shipto.ship-name
                  w-ord.ship-add1  = shipto.ship-add[1]
                  w-ord.ship-add2  = shipto.ship-add[2]
                  w-ord.ship-city  = shipto.ship-city
                  w-ord.ship-state = shipto.ship-state
                  w-ord.ship-zip   = shipto.ship-zip
                  w-ord.ship-ctry  = shipto.country.
                 LEAVE.
               END.
             END.

          FIND FIRST est
              WHERE est.company eq job.company
                AND est.est-no  eq job.est-no
              NO-LOCK NO-ERROR.
          RELEASE eb.
          IF AVAIL est THEN DO:

              IF  est.est-type = 6 THEN
                  FIND FIRST eb
                      WHERE eb.company   EQ est.company
                        AND eb.est-no    EQ est.est-no
                        AND eb.form-no   EQ 0
                        /*AND (eb.blank-no EQ job-hdr.blank-no OR job-hdr.blank-no EQ 0) */
                      NO-LOCK NO-ERROR.
              ELSE
                  FIND FIRST eb
                      WHERE eb.company   EQ est.company
                        AND eb.est-no    EQ est.est-no
                        AND eb.form-no   EQ job-hdr.frm
                        AND (eb.blank-no EQ job-hdr.blank-no OR job-hdr.blank-no EQ 0)
                      NO-LOCK NO-ERROR.
          END.

          IF AVAIL eb THEN
            ASSIGN
             w-ord.flute      = eb.flute
             w-ord.test       = eb.test
             w-ord.pcs        = eb.cas-cnt
             w-ord.bundle     = eb.cas-pal
             w-ord.total-unit = w-ord.pcs * w-ord.bundle
             w-ord.partial    = 0 /* w-ord.ord-qty - w-ord.total-unit*/
             w-ord.cas-no     = eb.cas-no.

          /* Add .49 to round up and add 1 for extra tag   */
          w-ord.total-tags = ((w-ord.ord-qty / w-ord.total-unit) + .49) +  IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE from-ord C-Win 
PROCEDURE from-ord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEFINE VARIABLE iRno   AS INTEGER NO-UNDO .
  DEFINE VARIABLE iRelNo AS INTEGER NO-UNDO .

  DEF VAR ld-over AS DEC NO-UNDO.

    FIND FIRST oe-ord
        WHERE ROWID(oe-ord) EQ ip-rowid
          AND (v-stat EQ "A"                                    OR
               (v-stat EQ "C" AND INDEX("CZ",oe-ord.stat) GT 0) OR
               (v-stat EQ "O" AND INDEX("CZ",oe-ord.stat) EQ 0))
        NO-LOCK NO-ERROR.

    IF AVAIL oe-ord THEN
    FIND FIRST cust
        WHERE cust.company eq cocode
          AND cust.cust-no eq oe-ord.cust-no
        NO-LOCK NO-ERROR.

    IF AVAIL oe-ord THEN
    FIND FIRST soldto
        WHERE soldto.company EQ cocode
          AND soldto.cust-no EQ oe-ord.cust-no
          AND soldto.sold-id EQ oe-ord.sold-id
        NO-LOCK NO-ERROR.

    IF AVAIL cust THEN
    FOR EACH oe-ordl
        WHERE oe-ordl.company eq oe-ord.company
        /*AND oe-ordl.i-no    ge v-fitem[1]
        AND oe-ordl.i-no    le v-fitem[2] */
        AND (oe-ordl.i-no = v-fitem OR v-fitem = "")
        AND oe-ordl.ord-no  eq oe-ord.ord-no
        use-index ord-no NO-LOCK BREAK BY oe-ordl.i-no:

      ld-over = 1 + (IF tb_over THEN (oe-ordl.over-pct / 100) ELSE 0).

      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq oe-ordl.i-no
          no-lock no-error.

      IF oe-ordl.est-no NE "" THEN
      FIND FIRST eb
          WHERE eb.company  EQ oe-ordl.company
            AND eb.est-no   EQ oe-ordl.est-no
            AND eb.stock-no EQ oe-ordl.i-no
          NO-LOCK NO-ERROR.

      IF NOT by-release OR NOT AVAIL oe-ordl THEN DO:
        IF FIRST-OF(oe-ordl.i-no) THEN DO:
          CREATE w-ord.

          ASSIGN
            w-ord.ord-no       = oe-ord.ord-no
            w-ord.job-no       = oe-ordl.job-no
            w-ord.job-no2      = oe-ordl.job-no2
            w-ord.cust-no      = oe-ord.cust-no
            w-ord.cust-name    = oe-ord.cust-name
            w-ord.i-no         = oe-ordl.i-no
            w-ord.cust-part-no = oe-ordl.part-no
            w-ord.po-no        = oe-ordl.po-no-po
/*             w-ord.cust-po-no   = /*if v-po-no-source eq "L" THEN*/ oe-ordl.po-no */
/*                                  /*else oe-ord.po-no*/                           */
            w-ord.ord-qty      = oe-ordl.qty * ld-over
            w-ord.i-name       = oe-ordl.i-name
            w-ord.part-dscr1   = oe-ordl.part-dscr1
            w-ord.part-dscr2   = oe-ordl.part-dscr2
            w-ord.part-dscr3   = oe-ordl.part-dscr3
            w-ord.due-date     = if oe-ord.due-date ne ? then
                                   oe-ord.due-date
                                 else
                                 if oe-ordl.req-date ne ? then
                                   oe-ordl.req-date
                                 else today
            w-ord.est-no       = oe-ordl.est-no
            w-ord.form-no      = oe-ordl.form-no
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = if cust.int-field[1] ne 0 then
                                   cust.int-field[1] else v-mult
            w-ord.linenum      = oe-ordl.e-num
            num-rec            = num-rec + 1.

          ASSIGN
           w-ord.cust-add1  = oe-ord.addr[1]
           w-ord.cust-add2  = oe-ord.addr[2]
           w-ord.cust-city  = oe-ord.city
           w-ord.cust-state = oe-ord.state
           w-ord.cust-zip   = oe-ord.zip
           w-ord.cust-ctry  = cust.country
           w-ord.sold-name  = oe-ord.sold-name
           w-ord.sold-add1  = oe-ord.sold-addr[1]
           w-ord.sold-add2  = oe-ord.sold-addr[2]
           w-ord.sold-city  = oe-ord.sold-city
           w-ord.sold-state = oe-ord.sold-state
           w-ord.sold-zip   = oe-ord.sold-zip
           w-ord.sold-ctry  = IF AVAIL soldto THEN soldto.country ELSE w-ord.cust-ctry
           w-ord.ship-name  = w-ord.cust-name
           w-ord.ship-add1  = w-ord.cust-add1
           w-ord.ship-add2  = w-ord.cust-add2
           w-ord.ship-city  = w-ord.cust-city
           w-ord.ship-state = w-ord.cust-state
           w-ord.ship-zip   = w-ord.cust-zip
           w-ord.ship-ctry  = w-ord.cust-ctry.

         /* rtc */
         RUN get-rel-info (OUTPUT w-ord.cust-po-no,
                           OUTPUT w-ord.rel-date,
                           OUTPUT w-ord.rel-lot#). 

          IF AVAIL itemfg THEN
            ASSIGN
             w-ord.upc-no     = itemfg.upc-no
             w-ord.box-len    = itemfg.l-score[50]
             w-ord.box-wid    = itemfg.w-score[50]
             w-ord.box-dep    = itemfg.d-score[50]
             w-ord.prod-notes = itemfg.prod-notes.

          
          FIND FIRST oe-relh NO-LOCK
              WHERE oe-relh.company EQ cocode 
                AND oe-relh.release# EQ begin_rel NO-ERROR .
          IF AVAIL oe-relh THEN
              FIND FIRST oe-rell  NO-LOCK     
              WHERE oe-rell.company   EQ oe-relh.company  
              AND oe-rell.r-no      EQ oe-relh.r-no NO-ERROR.   
          
          IF AVAIL oe-rell THEN
              w-ord.ord-qty      = oe-rell.qty .
         
          FIND FIRST oe-rel WHERE oe-rel.company eq cocode
                            AND oe-rel.i-no    eq oe-ordl.i-no
                            AND oe-rel.ord-no  eq oe-ordl.ord-no
                        /*AND oe-rel.link-no ne 0*/  NO-LOCK NO-ERROR.
          IF AVAIL oe-rel THEN 
             FIND FIRST shipto WHERE shipto.company eq cocode
                      AND shipto.cust-no eq oe-ord.cust-no
                      AND shipto.ship-id eq oe-rel.ship-id
                      USE-INDEX ship-id NO-LOCK NO-ERROR.
          ELSE FIND FIRST shipto WHERE shipto.company eq cocode
                    AND shipto.cust-no eq oe-ord.cust-no
                    AND shipto.ship-id eq oe-ord.cust-no
                    USE-INDEX ship-id NO-LOCK NO-ERROR.
          IF AVAIL shipto THEN
          ASSIGN
            w-ord.ship-name  = shipto.ship-name
            w-ord.ship-add1  = shipto.ship-add[1]
            w-ord.ship-add2  = shipto.ship-add[2]
            w-ord.ship-city  = shipto.ship-city
            w-ord.ship-state = shipto.ship-state
            w-ord.ship-zip   = shipto.ship-zip
            w-ord.ship-ctry  = shipto.country.        
         
          IF NOT AVAIL eb AND AVAIL itemfg AND itemfg.est-no NE "" THEN
          FIND FIRST eb
              WHERE eb.company  EQ itemfg.company
                AND eb.est-no   EQ itemfg.est-no
                AND eb.stock-no EQ itemfg.i-no
              NO-LOCK NO-ERROR.

          IF AVAIL eb THEN
            ASSIGN
             w-ord.flute      = eb.flute
             w-ord.test       = eb.test
             w-ord.pcs        = eb.cas-cnt
             w-ord.bundle     = eb.cas-pal
             w-ord.total-unit = w-ord.pcs * w-ord.bundle
             w-ord.partial    = 0 /*w-ord.ord-qty - w-ord.total-unit*/
             w-ord.cas-no     = eb.cas-no.

          /* Add .49 to round up and add 1 for extra tag   */
          w-ord.total-tags = ((w-ord.ord-qty / w-ord.total-unit) + .49) +  (IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1).
        END.  /* first-of */
      END.  /* not by-release */

     ELSE 
      FOR EACH oe-rel
          WHERE oe-rel.company eq cocode
          AND oe-rel.i-no    eq oe-ordl.i-no
          AND oe-rel.ord-no  eq oe-ordl.ord-no
          AND oe-rel.link-no ne 0
          NO-LOCK:

        FIND FIRST shipto
            WHERE shipto.company EQ cocode
              AND shipto.cust-no EQ oe-ord.cust-no
              AND shipto.ship-id EQ oe-rel.ship-id
            NO-LOCK NO-ERROR.

        CREATE w-ord.
        ASSIGN
          w-ord.ord-no       = oe-ord.ord-no
          w-ord.job-no       = oe-ordl.job-no
          w-ord.job-no2      = oe-ordl.job-no2
          w-ord.cust-no      = oe-ord.cust-no
          w-ord.cust-name    = oe-ord.cust-name
          w-ord.i-no         = oe-ordl.i-no
          w-ord.cust-part-no = oe-ordl.part-no
          w-ord.po-no        = oe-ordl.po-no-po
          w-ord.cust-po-no   = IF v-po-no-source eq "L" THEN oe-ordl.po-no
                                                        ELSE
                               IF v-po-no-source eq "R" THEN oe-rel.po-no
                                                        ELSE oe-ord.po-no 
/*           w-ord.cust-po-no   = IF v-po-no-source eq "L" THEN oe-ordl.po-no */
/*           ELSE                                                             */
/*           IF v-po-no-source eq "R" THEN oe-rel.po-no                       */
/*           ELSE oe-ord.po-no                                                */
          w-ord.ord-qty      = oe-rel.qty * ld-over
          w-ord.ship-add1    = oe-rel.ship-add[1]
          w-ord.ship-add2    = oe-rel.ship-add[2]
          w-ord.ship-city    = oe-rel.ship-city
          w-ord.ship-state   = oe-rel.ship-state
          w-ord.ship-zip     = oe-rel.ship-zip
          w-ord.i-name       = oe-ordl.i-name
          w-ord.part-dscr1   = oe-ordl.part-dscr1
          w-ord.part-dscr2   = oe-ordl.part-dscr2
          w-ord.part-dscr3   = oe-ordl.part-dscr3
          w-ord.due-date     = 
            (if oe-ord.due-date <> ? 
             then oe-ord.due-date
             else if oe-ordl.req-date <> ?  /* 9901 CAH */
             then oe-ordl.req-date
             else today)
          w-ord.rel-date     = oe-rel.rel-date
          w-ord.est-no       = oe-ordl.est-no
          w-ord.form-no      = oe-ordl.form-no
          w-ord.vendor       = company.name
          w-ord.tare-wt      = 10
          w-ord.uom          = "EA"
          w-ord.mult         = if cust.int-field[1] ne 0 then
                                 cust.int-field[1] else v-mult
          w-ord.linenum      = oe-ordl.e-num
          num-rec            = num-rec + 1.

        ASSIGN
           w-ord.cust-add1  = oe-ord.addr[1]
           w-ord.cust-add2  = oe-ord.addr[2]
           w-ord.cust-city  = oe-ord.city
           w-ord.cust-state = oe-ord.state
           w-ord.cust-zip   = oe-ord.zip
           w-ord.cust-ctry  = cust.country
           w-ord.sold-name  = oe-ord.sold-name
           w-ord.sold-add1  = oe-ord.sold-addr[1]
           w-ord.sold-add2  = oe-ord.sold-addr[2]
           w-ord.sold-city  = oe-ord.sold-city
           w-ord.sold-state = oe-ord.sold-state
           w-ord.sold-zip   = oe-ord.sold-zip
           w-ord.sold-ctry  = IF AVAIL soldto THEN soldto.country ELSE w-ord.cust-ctry
           w-ord.ship-name  = IF AVAIL shipto THEN shipto.ship-name ELSE w-ord.cust-name
           w-ord.ship-ctry  = IF AVAIL shipto THEN shipto.country ELSE w-ord.cust-ctry.

        IF AVAIL itemfg THEN
          ASSIGN
           w-ord.upc-no   = itemfg.upc-no
           w-ord.box-len  = itemfg.l-score[50]
           w-ord.box-wid  = itemfg.w-score[50]
           w-ord.box-dep  = itemfg.d-score[50].

        IF NOT AVAIL eb AND AVAIL itemfg AND itemfg.est-no NE "" THEN
        FIND FIRST eb
            WHERE eb.company  EQ itemfg.company
              AND eb.est-no   EQ itemfg.est-no
              AND eb.stock-no EQ itemfg.i-no
            NO-LOCK NO-ERROR.

        IF AVAIL eb THEN
          ASSIGN
           w-ord.flute = eb.flute
           w-ord.test  = eb.test.

        ASSIGN
          w-ord.pcs        = oe-rel.qty-case
          w-ord.bundle     = oe-rel.cases
          w-ord.total-unit = w-ord.pcs * w-ord.bundle
          w-ord.partial    = 0 /*w-ord.ord-qty - w-ord.total-unit*/
          w-ord.cas-no     = eb.cas-no.

        /* Add .49 to round up and add 1 for extra tag   */
        ASSIGN w-ord.total-tags = ((w-ord.ord-qty / w-ord.total-unit) + .49) +  IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1.

        ASSIGN w-ord.rel-lot# = oe-rel.lot-no.
      END.
    END.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-jobord-info C-Win 
PROCEDURE get-jobord-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ll AS INTEGER INIT 1 NO-UNDO.
DEFINE VARIABLE lv-job-no AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-job-no2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-job AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-job2 AS INTEGER NO-UNDO.
DEFINE VARIABLE li AS INTEGER NO-UNDO.
DEFINE VARIABLE lcForm AS CHARACTER NO-UNDO.
DEFINE VARIABLE iForm AS CHARACTER NO-UNDO .
DEFINE VARIABLE iBlank-no AS CHARACTER NO-UNDO .
DEFINE VARIABLE lCheckForm AS LOGICAL INIT YES NO-UNDO .
DEFINE VARIABLE lCheckBlank AS LOGICAL INIT YES NO-UNDO .
DEFINE VARIABLE oplCheckForm AS LOGICAL INIT NO NO-UNDO .

DEFINE BUFFER bf-job FOR job.
DEFINE BUFFER bf-job-hdr-2 FOR job-hdr.


   v-job = ENTRY(1,begin_job:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

   DO li = 1 TO LENGTH(v-job):
      IF INDEX("/:-",SUBSTR(v-job,li,1)) GT 0 THEN
        ll = ll + 1.
         /*ELSE LEAVE.*/
      ELSE do:
         IF ll EQ 1 THEN lv-job-no = lv-job-no + SUBSTR(v-job,li,1).
         ELSE IF ll EQ 2 THEN lv-job-no2 = lv-job-no2 + SUBSTR(v-job,li,1).
         ELSE IF ll EQ 3 THEN iForm = iForm + SUBSTR(v-job,li,1) NO-ERROR .
         ELSE IF ll EQ 4 THEN iBlank-no = iBlank-no + SUBSTR(v-job,li,1) NO-ERROR .
      END.
   END.
   IF iForm EQ "" THEN
       lCheckForm = NO .
   IF iBlank-no EQ "" THEN
       lCheckBlank = NO .

   ASSIGN
      lv-job-no = FILL(" ",6 - LENGTH(TRIM(lv-job-no))) + lv-job-no
      v-job2 = INT(lv-job-no2).
   RUN dispJobInfo (INPUT cocode, INPUT lv-job-no, INPUT v-job2,INPUT iForm, INPUT iBlank-no, INPUT lCheckForm, INPUT lCheckBlank, OUTPUT oplCheckForm ).
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rel-info C-Win 
PROCEDURE get-rel-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-pono LIKE w-ord.cust-po-no NO-UNDO.
  DEF OUTPUT PARAM op-date LIKE w-ord.rel-date NO-UNDO.
  DEF OUTPUT PARAM op-lot# LIKE w-ord.rel-lot# NO-UNDO.


  RELEASE oe-rell.
  RELEASE oe-rel.

  IF v-po-no-source EQ "R" THEN DO:
    FOR EACH oe-rell NO-LOCK
        WHERE oe-rell.company  EQ oe-ordl.company
          AND oe-rell.ord-no   EQ oe-ordl.ord-no
          AND oe-rell.i-no     EQ oe-ordl.i-no
          AND oe-rell.line     EQ oe-ordl.line,

        FIRST oe-relh NO-LOCK
        WHERE oe-relh.r-no     EQ oe-rell.r-no
          AND oe-relh.posted   EQ NO
          AND oe-relh.rel-date GE begin_date
          AND oe-relh.rel-date LE end_date
        BY oe-relh.rel-date
        BY oe-relh.r-no:

      ASSIGN
       op-pono = oe-rell.po-no
       op-date = oe-relh.rel-date
       op-lot# = oe-rell.lot-no.
      LEAVE.
    END.

    IF AVAIL oe-rell THEN
    FIND FIRST oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no NO-LOCK NO-ERROR.

    ELSE
    FOR EACH oe-rel NO-LOCK
        WHERE oe-rel.company  EQ oe-ordl.company
          AND oe-rel.ord-no   EQ oe-ordl.ord-no
          AND oe-rel.i-no     EQ oe-ordl.i-no
          AND oe-rel.line     EQ oe-ordl.line
          /*AND oe-rel.rel-no   EQ 0 */  /* Ticket - 21750 */
          AND oe-rel.rel-date GE begin_date
          AND oe-rel.rel-date LE end_date
        BY oe-rel.rel-date
        BY oe-rel.r-no:

      ASSIGN
       op-pono = oe-rel.po-no
       op-date = oe-rel.rel-date
       op-lot# = oe-rel.lot-no.
      LEAVE.
    END.
  END.

  IF NOT AVAIL oe-rel THEN
  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company  EQ oe-ordl.company
        AND oe-rel.ord-no   EQ oe-ordl.ord-no
        AND oe-rel.i-no     EQ oe-ordl.i-no
        AND oe-rel.line     EQ oe-ordl.line
      BY oe-rel.rel-date
      BY oe-rel.r-no:

    ASSIGN
    op-pono = oe-rel.po-no
    op-date = oe-rel.rel-date
    op-lot# = oe-rel.lot-no.
    LEAVE.
  END.

  IF v-po-no-source NE "R"                    OR
     (NOT AVAIL oe-rel AND NOT AVAIL oe-rell) THEN
    op-pono = IF v-po-no-source EQ "L" THEN oe-ordl.po-no
                                       ELSE oe-ord.po-no.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-cas-lab C-Win 
PROCEDURE new-cas-lab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    IF fi_cas-lab:SCREEN-VALUE NE "" THEN DO:
      FIND loadtag
          WHERE loadtag.company     EQ cocode
            AND loadtag.tag-no      BEGINS TRIM(fi_cas-lab:SCREEN-VALUE)
            AND loadtag.item-type   EQ NO
            AND loadtag.is-case-tag EQ YES
          NO-LOCK NO-ERROR.
      IF AVAIL loadtag THEN DO:
        ASSIGN
          fi_cas-lab:SCREEN-VALUE   = loadtag.tag-no
          begin_ord-no:SCREEN-VALUE = STRING(loadtag.ord-no)
          begin_job:SCREEN-VALUE    = loadtag.job-no
          begin_job2:SCREEN-VALUE   = STRING(loadtag.job-no2)
          begin_i-no:SCREEN-VALUE   = loadtag.i-no.

        RUN ok-button.
      END.
      ELSE MESSAGE "Invalid Loadtag. Try Help." VIEW-AS ALERT-BOX ERROR.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job C-Win 
PROCEDURE new-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    RELEASE job.
    RELEASE job-hdr.

 IF begin_i-no:SCREEN-VALUE EQ "" THEN do:

    IF TRIM(begin_job:SCREEN-VALUE) NE "" THEN
    FIND FIRST job NO-LOCK
        WHERE job.company  EQ cocode
          AND job.job-no   EQ FILL(" ",6 - LENGTH(TRIM(begin_job:SCREEN-VALUE))) +
                              TRIM(begin_job:SCREEN-VALUE)
          AND job.job-no2 EQ INT(begin_job2:SCREEN-VALUE)
        NO-ERROR.

    IF AVAIL job THEN
    FIND FIRST job-hdr NO-LOCK
        WHERE job-hdr.company EQ job.company
          AND job-hdr.job     EQ job.job
          AND job-hdr.job-no  EQ job.job-no
          AND job-hdr.job-no2 EQ job.job-no2
        NO-ERROR.

    IF AVAIL job-hdr THEN DO:
       ASSIGN
          begin_ord-no:SCREEN-VALUE = STRING(job-hdr.ord-no)
          begin_i-no:SCREEN-VALUE   = job-hdr.i-no.

       IF v-auto-print AND LOGICAL(scr-freeze-label:SCREEN-VALUE) = NO THEN
       DO:
          FIND FIRST cust-part WHERE
               cust-part.company = cocode AND
               cust-part.i-no    = begin_i-no:SCREEN-VALUE AND
               cust-part.cust-no = job-hdr.cust-no
               NO-LOCK NO-ERROR.

          IF AVAIL cust-part AND cust-part.labelCase NE "" THEN
             scr-label-file:SCREEN-VALUE = cust-part.labelCase.
          ELSE
          DO:

             IF job-hdr.ord-no NE 0 THEN
                FIND FIRST oe-rel WHERE
                     oe-rel.company eq cocode AND
                     oe-rel.i-no    eq job-hdr.i-no AND
                     oe-rel.ord-no  eq job-hdr.ord-no
                     NO-LOCK NO-ERROR.

             IF AVAIL oe-rel THEN 
                FIND FIRST shipto WHERE
                     shipto.company eq cocode AND
                     shipto.cust-no eq job-hdr.cust-no AND
                     shipto.ship-id eq oe-rel.ship-id
                     USE-INDEX ship-id NO-LOCK NO-ERROR.
             ELSE
                FIND FIRST shipto WHERE
                     shipto.company eq cocode AND
                     shipto.cust-no eq job-hdr.cust-no AND
                     shipto.ship-id eq job-hdr.cust-no
                     USE-INDEX ship-id NO-LOCK NO-ERROR.

             IF AVAIL shipto THEN
             DO:

                FIND FIRST sys-ctrl-shipto WHERE
                     sys-ctrl-shipto.company = cocode AND
                     sys-ctrl-shipto.NAME = "CASLABEL" AND
                     sys-ctrl-shipto.cust-vend = YES AND
                     sys-ctrl-shipto.cust-vend-no = job-hdr.cust-no AND
                     sys-ctrl-shipto.ship-id = shipto.ship-id AND
                     sys-ctrl-shipto.char-fld NE ''
                     NO-LOCK NO-ERROR.

                IF AVAIL sys-ctrl-shipto AND 
                   TRIM(sys-ctrl-shipto.char-fld) NE "" 
                  THEN
                   scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.

                  /* gdm - 11050806 */
                  ELSE 
                   FIND FIRST sys-ctrl-shipto NO-LOCK 
                       WHERE sys-ctrl-shipto.company      EQ cocode 
                         AND sys-ctrl-shipto.NAME         EQ "CASLABEL" 
                         AND sys-ctrl-shipto.cust-vend    EQ YES 
                         AND sys-ctrl-shipto.cust-vend-no EQ oe-rel.cust-no 
                         AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.

                   IF AVAIL sys-ctrl-shipto 
                     THEN
                       scr-label-file:SCREEN-VALUE = sys-ctrl-shipto.char-fld.


                     IF scr-label-file:SCREEN-VALUE EQ "" THEN  DO:
                         FIND FIRST sys-ctrl NO-LOCK 
                             WHERE sys-ctrl.company EQ cocode 
                               AND sys-ctrl.name    EQ "CASLABEL" NO-ERROR.
                         IF AVAIL sys-ctrl THEN
                            scr-label-file:SCREEN-VALUE = sys-ctrl.char-fld.
                     END.

             END.
          END.
       END.       

       IF CAN-FIND(FIRST job-hdr
                   WHERE job-hdr.company EQ job.company
                     AND job-hdr.job     EQ job.job
                     AND job-hdr.job-no  EQ job.job-no
                     AND job-hdr.job-no2 EQ job.job-no2
                     AND job-hdr.i-no    NE begin_i-no:SCREEN-VALUE)
       THEN 
           ASSIGN 
             begin_i-no:SCREEN-VALUE     = ""
             scr-label-file:SCREEN-VALUE = ""
             v-lcnt = 2.
    END.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ok-button C-Win 
PROCEDURE ok-button :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF begin_filename:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" AND userLabelPath <> "" THEN        
        begin_filename:SCREEN-VALUE = userLabelPath.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
  ASSIGN
      cBarCodeProgram = IF scr-label-file MATCHES "*.xpr*" THEN "xprint" ELSE "" .
  IF scr-auto-print AND scr-label-file = "" THEN
  DO:
     MESSAGE "Label Matrix Label File cannot be blank."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY "ENTRY":U TO scr-label-file IN FRAME {&FRAME-NAME}.
     RETURN.
  END.

  FILE-INFO:FILE-NAME = begin_filename.
  if begin_filename <> "" AND FILE-INFO:FILE-type eq ? then do:
     message "Form file/path does not exist. Do you want to create it?" 
             view-as alert-box ERROR BUTTON YES-NO UPDATE v-ans AS LOG.
     IF v-ans THEN OS-CREATE-DIR VALUE(begin_filename).
     ELSE do:
         MESSAGE "Casetag file path is not valid. Can't create."
             VIEW-AS ALERT-BOX ERROR.
         APPLY 'entry' TO begin_filename.
         return error.
     END.
   end.

  v-out = begin_filename.
  v-init-dir = v-out.

  FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ gcompany
                        AND sys-ctrl.name    EQ "CASETAG" NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = gcompany
     sys-ctrl.name     = "CASETAG"
     sys-ctrl.descrip  = "Case Label Format?   Use Case Label as LoadTag?".
  END.
  v-case-tag = sys-ctrl.log-fld.
/* MESSAGE "Per Unit " tb_per-unit SKIP           */
/*         "Units/Pallet " tb_per-pallet SKIP     */
/*         "Include Overrun Qty " tb_over SKIP    */
/*         "Print Components " tb_print-comp SKIP */
/*         "Auto Print " scr-auto-print SKIP      */
/*         "Freeze " scr-freeze-label SKIP        */
/*         "LM File " scr-label-file SKIP         */
/*         "Print PO " rd_print SKIP              */
/*         "Begin Date " begin_date SKIP          */
/*         "End Date " end_date SKIP              */
/*         "Order Status " rd_order-sts SKIP      */
/*         "Sets " rd_comps                       */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.         */

  run run-report NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  MESSAGE "New Order?" VIEW-AS ALERT-BOX QUESTION
      BUTTON YES-NO UPDATE ll-ans AS LOG.
  IF ll-ans THEN DO:
     APPLY "CLOSE":U TO THIS-PROCEDURE.
     RETURN NO-APPLY.
  END.

  APPLY "entry" TO begin_job IN FRAME {&FRAME-NAME}.
  /*
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case. 
  */
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
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

    RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).
  */  
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
  run scr-rpt.w (list-name,c-win:title). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-barone C-Win 
PROCEDURE run-barone :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM ip-TagText AS cha NO-UNDO.

   DEFINE VARIABLE iReturnResult AS INTEGER NO-UNDO.
   DEFINE VARIABLE cProgramName AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

   cProgramName =  "c:\program files\bar-one 6 pro-plus\labels.exe ".
   cFileName    = "R:\ASI_GUI9\SOURCE\custom\century.lab".


  /* OS-COPY VALUE(ip-tagtext) VALUE("r:\asi_gui9\source\custom\"). */
   RUN WinExec (INPUT cProgramName + CHR(32) + cFileName , INPUT 1, OUTPUT
   iReturnResult).
/*
   IF iReturnResult >= 32 THEN
     MESSAGE "Application was Started" VIEW-AS ALERT-BOX.
   ELSE
     MESSAGE "Application Failed:" iReturnResult VIEW-AS ALERT-BOX.

*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-lmw C-Win 
PROCEDURE run-lmw :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM ip-TagText AS cha NO-UNDO.

   DEFINE VARIABLE iReturnResult AS INTEGER NO-UNDO.
   DEFINE VARIABLE cProgramName AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

/*   cProgramName =  "c:\program files\bar-one 6 pro-plus\labels.exe ".
*/
   cFileName    = "custom\interpack.qdf".
   FILE-INFO:FILE-NAME = cFileName.
   cFileName = FILE-INFO:FULL-PATHNAME.

   RUN custom/runlmw.p (OUTPUT cprogramname).

/*   OS-COPY VALUE(ip-tagtext) VALUE("c:\tmp\").*/

   RUN WinExec (INPUT cProgramName + CHR(32) + cFileName , INPUT 1, OUTPUT
   iReturnResult).
/*
   IF iReturnResult >= 32 THEN
     MESSAGE "Application was Started" VIEW-AS ALERT-BOX.
   ELSE
     MESSAGE "Application Failed:" iReturnResult VIEW-AS ALERT-BOX.

*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  meters:  <none>
  Notes:       
--------------------------*/
DEF VAR i AS INT NO-UNDO.
DEF VAR lv-ord-no LIKE oe-ord.ord-no NO-UNDO.
DEF VAR lv-job-no LIKE job.job-no NO-UNDO.
DEF VAR lv-job-no2 LIKE job.job-no NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.
DEF VAR lv-middlesex-job AS CHAR FORMAT "x(9)" NO-UNDO.
DEF VAR lv-middlesex-po AS CHAR FORMAT "x(9)" NO-UNDO.

DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-w-ord FOR w-ord.
DEF VAR lv-tag-no AS cha NO-UNDO.
DEF VAR lv-casetag-no AS cha NO-UNDO.
DEF VAR lv-how-many-tags AS INT NO-UNDO.
DEF VAR v-b-word-created AS LOG NO-UNDO.
DEF VAR op-warning AS LOG NO-UNDO.
DEF VAR var-display-warning AS LOG NO-UNDO.
DEFINE VARIABLE iPcs AS INTEGER NO-UNDO .

SESSION:SET-WAIT-STATE ("general").

ASSIGN
 v-ford-no   = begin_ord-no
 v-fitem     = begin_i-no
 by-release     = tb_rel
 v-po-no-source = rd_print
 form#          = begin_form
 copy_count     = begin_labels
 form_fid       = begin_filename
 v-stat         = rd_order-sts.

  FOR EACH w-ord:
    DELETE w-ord.
  END.

  FOR EACH w-file:
    DELETE w-file.
  END.

  EMPTY TEMP-TABLE tt-tag.

/*========
  DO i = 1 TO NUM-ENTRIES(v-ord-list).
    lv-ord-no = INT(ENTRY(i,v-ord-list)) NO-ERROR.

    IF NOT ERROR-STATUS:ERROR AND
       lv-ord-no NE 0         THEN RUN temp-ord (lv-ord-no).
  END.

  FOR EACH w-file: 
    RUN from-ord (w-key).
  END.
===*/

  IF TRIM(begin_job) EQ "" THEN
    FOR EACH oe-ord
        WHERE oe-ord.company EQ cocode
          AND oe-ord.ord-no  EQ v-ford-no
        NO-LOCK:
      RUN from-ord (ROWID(oe-ord)).
    END.

  /*=======
  DO i = 1 TO NUM-ENTRIES(v-job-list).
    ASSIGN
     ll = YES
     lv-job-no  = ""
     lv-job-no2 = "".

    DO li = 1 TO LENGTH(ENTRY(i,v-job-list)):
      IF INDEX("/:-",SUBSTR(ENTRY(i,v-job-list),li,1)) GT 0 THEN
        IF ll THEN ll = NO.
        ELSE LEAVE.
      ELSE
      IF ll THEN lv-job-no = lv-job-no + SUBSTR(ENTRY(i,v-job-list),li,1).
            ELSE lv-job-no2 = lv-job-no2 + SUBSTR(ENTRY(i,v-job-list),li,1).
    END.
    lv-job-no = FILL(" ",6 - LENGTH(TRIM(lv-job-no))) + TRIM(lv-job-no) +
                STRING(INT(lv-job-no2),"99") NO-ERROR.

    IF NOT ERROR-STATUS:ERROR AND
       lv-job-no NE ""        THEN RUN temp-job (lv-job-no).
  END.

  FOR EACH w-file: 
    RUN from-job (w-key).
  END.
  =======*/

  IF begin_job NE "" THEN
    FOR EACH job
        WHERE job.company EQ cocode
          AND job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(begin_job))) + TRIM(begin_job)
          /*AND job.job-no  LE FILL(" ",6 - LENGTH(TRIM(end_job)))   + TRIM(end_job) */
          AND FILL(" ",6 - LENGTH(TRIM(job.job-no))) + TRIM(job.job-no) + STRING(job.job-no2,"99")
                          EQ
              FILL(" ",6 - LENGTH(TRIM(begin_job))) + TRIM(begin_job) + STRING(begin_job2,"99")

        /*  AND FILL(" ",6 - LENGTH(TRIM(job.job-no))) + TRIM(job.job-no) + STRING(job.job-no2,"99")
                          LE
              FILL(" ",6 - LENGTH(TRIM(end_job))) + TRIM(end_job) + STRING(end_job2,"99") */
        NO-LOCK:

      RUN from-job (ROWID(job),OUTPUT op-warning).

      IF op-warning THEN
         var-display-warning = YES.
    END.

    IF var-display-warning THEN
       MESSAGE "Job does not contain an order number, hence data such as PO# will not print."
           VIEW-AS ALERT-BOX WARNING BUTTONS OK.

  FOR EACH w-ord,
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-ord.i-no
        AND itemfg.isaset  EQ YES
      NO-LOCK:

    IF tb_print-comp and
      ( rd_comps  EQ "B"                          OR
        (rd_comps EQ "A" AND itemfg.alloc NE YES) OR
        (rd_comps EQ "U" AND itemfg.alloc) )      THEN DO:

      RUN fg/fullset.p (ROWID(itemfg)).
      v-b-word-created = NO.

      FOR EACH tt-fg-set WHERE tt-fg-set.part-no <> w-ord.i-no ,
          FIRST b-itemfg
          WHERE b-itemfg.company EQ cocode
            AND b-itemfg.i-no    EQ tt-fg-set.part-no
          NO-LOCK:

        CREATE b-w-ord.
        BUFFER-COPY w-ord TO b-w-ord
        ASSIGN
         b-w-ord.i-no       = tt-fg-set.part-no
         b-w-ord.i-name     = b-itemfg.i-name
         b-w-ord.ord-qty    = w-ord.ord-qty * tt-fg-set.part-qty-dec
         b-w-ord.box-len    = b-itemfg.l-score[50]
         b-w-ord.box-wid    = b-itemfg.w-score[50]
         b-w-ord.box-dep    = b-itemfg.d-score[50]
         b-w-ord.prod-notes = b-itemfg.prod-notes.

        v-b-word-created = YES.
        FIND FIRST est
            WHERE est.company eq cocode
              AND est.est-no  eq w-ord.est-no
            NO-LOCK NO-ERROR.
        RELEASE eb.

        IF AVAIL est THEN DO:
          IF est.est-type = 6 AND tb_print-comp THEN DO:
              /* Ensure not picking up set header */
              FIND FIRST eb
                  WHERE eb.company   EQ cocode
                    AND eb.est-no    EQ w-ord.est-no
                    AND eb.form-no   GT 0
                    AND eb.part-no   EQ w-ord.cust-part-no
                  NO-LOCK NO-ERROR.
              IF NOT AVAIL eb THEN
              FIND FIRST eb
                  WHERE eb.company   EQ cocode
                    AND eb.est-no    EQ w-ord.est-no
                    AND eb.form-no   GT 0
                    AND eb.stock-no   EQ b-w-ord.i-no
                  NO-LOCK NO-ERROR.              
          END.
          ELSE
              FIND FIRST eb
                   WHERE eb.company   EQ cocode
                    AND eb.est-no    EQ w-ord.est-no
                    AND eb.form-no   EQ w-ord.form-no
                    AND eb.part-no   EQ w-ord.cust-part-no
                  NO-LOCK NO-ERROR.
        END.

        IF AVAIL eb THEN
          ASSIGN
           b-w-ord.flute      = eb.flute
           b-w-ord.test       = eb.test
           b-w-ord.pcs        = eb.cas-cnt
           b-w-ord.bundle     = eb.cas-pal
           b-w-ord.total-unit = b-w-ord.pcs * b-w-ord.bundle
           b-w-ord.cas-no     = eb.cas-no.

        b-w-ord.total-tags = ((b-w-ord.ord-qty / b-w-ord.total-unit) + .49)
                             + ( IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1) .

      END.

      IF v-b-word-created AND NOT tb_print-comp THEN DELETE w-ord.
    END.
  END.

  ASSIGN
   str-tit  = coname + " - " + loname
   str-tit2 = "DOWNLOAD LOADTAG DATA"
   x = (56 - length(str-tit)) / 2
   str-tit  = FILL(" ",x) + str-tit
   x = (56 - length(str-tit2)) / 2
   str-tit2 = FILL(" ",x) + str-tit2.

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  SESSION:SET-WAIT-STATE ("").

  RUN oerep/d-casetg.w.

  choice = NO.
  FIND FIRST w-ord  NO-ERROR.
  IF AVAIL w-ord THEN
     message "Are you ready to generate Case Label File? " 
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.

  IF NOT choice THEN RETURN ERROR.

  SESSION:SET-WAIT-STATE ("general").

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)} 

   VIEW FRAME r-top.
   VIEW FRAME top.

  IF v-out = "" THEN v-out = "c:~\ba~\label~\CaseLabel.txt".
  ELSE do:

     v-out = v-init-dir.

     IF SUBSTRING(v-out,LENGTH(v-out),1) = "/" OR
        SUBSTRING(v-out,LENGTH(v-out),1) = "\" THEN .
     ELSE v-out = v-out + "/".

     v-out = v-out + "CaseLabel.txt".
  END.

  IF v-loadtag = "TRIAD" THEN
  DO:
     if form_fid > "" then do:   /* download the form file into the printer ~*/
       input stream s-form from value(form_fid) no-echo.
       _form: do while true:
             readkey stream s-form.
         if lastkey < 0 then leave _form.
           put stream s-bar CONTROL chr(lastkey).
       end.
       input stream s-form close.
     end. 

     FOR EACH w-ord:
        v-job = w-ord.job-no + "-" + string(w-ord.job-no2,"99") .
        IF v-job BEGINS "-" or v-job = ? /* 9901 CAH */
             THEN v-job = string(W-ORD.ORD-NO).   /* 9812 CAH in case blank */

        find first itemfg where itemfg.company = cocode
                 and itemfg.i-no = w-ord.i-no no-lock no-error.

        IF w-ord.total-tags gt -1 THEN
        DO:
          DO i = 1 TO (w-ord.total-tags + 1):
            /* select the form */
            put stream s-bar control stx esc "E" string(form#) ",1" can etx.
            /* 9901 CAH: done above ... 
            /* clear the variable data fields */
            put stream s-bar control stx can etx.
            */

            char_units = (if i <= w-ord.total-tags 
            then string(w-ord.total-unit) else "    ").  /* 9902 was .bundle */

            /* 9901 CAH: was coming up as ? in file for no good reason ... */
            def var char_date as char format 'x(10)' no-undo.
            /* 09.26.2001 CAH: Per Triad, use manufacturing date ...
            char_date = string(w-ord.due-date,"99/99/9999").
            if char_date = ? then char_date = string(today).
            */
            char_date = string(today,"99/99/9999").

            /* 9901 CAH: Only room for 19 chars in the standard 48 pt font */
            if length(w-ord.ship-name) > 19
            then w-ord.ship-name = substring(w-ord.ship-name,1,19).

            /* 07/2001 CAH: Add finished goods item number to the label
                and the n of m label counts */
            def var vcFGItem as char no-undo.
            vcFGItem = 
                if avail itemfg then itemfg.i-no else w-ord.i-no.

           do n = copy_count to 1 by -1:

            /* send the variable data to the printer */
            put stream s-bar unformatted
                stx removeChars(w-ord.cust-po-no)    cr etx
                stx removeChars(w-ord.cust-po-no)    cr etx
                stx removeChars(w-ord.cust-part-no)  cr etx                    
                stx removeChars(w-ord.cust-part-no)  cr etx
                stx char_units          cr etx
                stx char_units          cr etx
                stx char_date           cr etx
                stx v-job               cr etx
                stx w-ord.ord-qty  FORM ">>>>>>>>9"      cr etx /* 9902 CAH was total-unit */
                stx string(i)                   cr etx  /* 08.20 was n */
                stx string(w-ord.total-tags + 1) cr etx  /* 08.20 was copy_count */
                stx removeChars(w-ord.ship-name)     cr etx
                stx removeChars(vcFGItem)            cr etx
                .

            /* issue the print command */    
            put stream s-bar control     
                stx rs "1" us "1" etb etx.
           end.

          end.   /* tag count loop */
        end.  /* non zero */
     END.    /* each w-ord */

     OUTPUT CLOSE.
  END.    /* TRIAD INTERMEC BARCODE PRINT ROUTINE */

  ELSE DO:
    IF cBarCodeProgram EQ "" THEN DO:
        OUTPUT TO VALUE(v-out).
        PUT UNFORMATTED
            "CUSTOMER,ORDNUMBER,JOBNUMBER,ITEM,CUSTPARTNO,INAME,IDSCR1,IDSCR2," +
            "IDSCR3,CUSTPONO,PCS,BUNDLE,TOTAL," +
            "BILLNAME,BILLADD1,BILLADD2,BILLCITY,BILLSTATE,BILLZIP,BILLCOUNTRY," +
            "SOLDNAME,SOLDADD1,SOLDADD2,SOLDCITY,SOLDSTATE,SOLDZIP,SOLDCOUNTRY," +
            "SHIPNAME,SHIPADD1,SHIPADD2,SHIPCITY,SHIPSTATE,SHIPZIP,SHIPCOUNTRY," +
            "DUEDATE,RELDATE,UPCNO,LENGTH,WIDTH,DEPTH,FLUTE,TEST,VENDOR,GROSSWGT," +
            "TAREWGT,NETWGT,SHEETWGT,UOM,MIDDLESEXJOBNUMBER,MIDDLESEXCUSTPONO," +
            "TAG#,PARTIAL,CASECODE,COLOR,CODE,CASEWGT,FG LOT#,RELLOT#,DRAWING#,POLINE#,PONO,FORM,BLANK,CURRENTDATE,CURRENTTIME".
        PUT SKIP.
    END.

    FOR EACH w-ord:
      IF tb_16ths THEN
        ASSIGN
         w-ord.box-len = ROUND((w-ord.box-len - TRUNC(w-ord.box-len,0)) / 6.25,2) +
                         TRUNC(w-ord.box-len,0)
         w-ord.box-wid = ROUND((w-ord.box-wid - TRUNC(w-ord.box-wid,0)) / 6.25,2) +
                         TRUNC(w-ord.box-wid,0)
         w-ord.box-dep = ROUND((w-ord.box-dep - TRUNC(w-ord.box-dep,0)) / 6.25,2) +
                         TRUNC(w-ord.box-dep,0).

      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq w-ord.i-no
          no-lock no-error.

      if avail itemfg then
        assign
         w-ord.net-wt   = itemfg.weight-100 * w-ord.total-unit / 100.
         w-ord.sheet-wt = itemfg.weight-100 / 100.


      w-ord.gross-wt = w-ord.net-wt + w-ord.tare-wt.

      v-job = w-ord.job-no + "-" + string(w-ord.job-no2,"99") .
      IF v-job BEGINS "-" THEN v-job = "".

      IF AVAIL w-ord THEN DO:
         FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
             AND job-hdr.job-no EQ w-ord.job-no
             AND job-hdr.job-no2 EQ w-ord.job-no2
             AND job-hdr.i-no EQ w-ord.i-no NO-LOCK NO-ERROR.
         IF AVAIL job-hdr THEN
             ASSIGN 
             v-frm-no = trim(string(job-hdr.frm))
             v-blnk-no = trim(string(job-hdr.blank-no)) .
      END. 

      ASSIGN
       lv-middlesex-po  = SUBSTR(TRIM(w-ord.job-no),1,6)
       lv-middlesex-job = IF lv-middlesex-job EQ "" THEN "" ELSE
                          "%MX" +
                          FILL("0",6 - LENGTH(TRIM(lv-middlesex-job))) +
                          TRIM(lv-middlesex-job)
       lv-middlesex-po  = SUBSTR(TRIM(w-ord.cust-po-no),1,6)
       lv-middlesex-po  = IF lv-middlesex-po EQ "" THEN "" ELSE
                          "BNJ" +
                          FILL("0",6 - LENGTH(TRIM(lv-middlesex-po))) +
                          TRIM(lv-middlesex-po).
      IF w-ord.total-tags gt 0 THEN
      DO:

        lv-how-many-tags =  /*IF v-loadtag = "SSLABEL" THEN w-ord.total-tags
                            ELSE (w-ord.total-tags - 1)*/
                            w-ord.total-tags.
        lv-tag-no = "".

        /*IF v-case-tag THEN do:*/
            IF length(trim(w-ord.job-no)) < 6 THEN
                    lv-casetag-no = FILL(" ", 6 - LENGTH(TRIM(w-ord.job-no)) ) + TRIM(w-ord.job-no) + STRING(w-ord.job-no2,"99").
            ELSE  lv-casetag-no = w-ord.job-no + STRING(w-ord.job-no2,"99").

            FIND FIRST loadtag
                WHERE loadtag.company     EQ cocode
                  AND loadtag.tag-no      EQ STRING(w-ord.i-no,"x(15)") +
                                             lv-casetag-no
                  AND loadtag.is-case-tag EQ YES
                  AND loadtag.item-type   EQ NO
                NO-ERROR.

            IF NOT AVAIL loadtag THEN CREATE loadtag.

            ASSIGN loadtag.company = cocode
                   loadtag.tag-no = STRING(w-ord.i-no,"x(15)") +
                                    lv-casetag-no
                   loadtag.item-type = no /*FGitem*/
                   /*loadtag.po-no = w-ord.po-no*/
                   loadtag.job-no = w-ord.job-no
                   loadtag.job-no2 = w-ord.job-no2
                   loadtag.ord-no = w-ord.ord-no
                   loadtag.i-no = caps(w-ord.i-no)
                   loadtag.i-name = w-ord.i-name
                   loadtag.qty = w-ord.ord-qty
                   loadtag.qty-case =   w-ord.pcs
                   loadtag.case-bundle =  w-ord.bundle
                   loadtag.pallet-count = 0 /*w-ord.total-unit*/ /*w-ord.pcs * w-ord.bundle*/
                   loadtag.loc = itemfg.def-loc
                   loadtag.loc-bin  = itemfg.def-loc-bin
                   loadtag.partial = w-ord.partial
                   loadtag.is-case-tag = YES 
                   loadtag.misc-char[2] = w-ord.lot
                   lv-tag-no = loadtag.tag-no.
        /*END.*/
        IF cBarCodeProgram EQ "" THEN
        DO i = 1 TO (lv-how-many-tags ):

           IF i EQ lv-how-many-tags AND w-ord.partial NE 0 THEN
               iPcs = w-ord.partial .
           ELSE iPcs = w-ord.pcs .

           PUT UNFORMATTED
            "~""  removeChars(w-ord.cust-name)  "~","
            w-ord.ord-no ","
            "~""  v-job  "~","
            "~""  removeChars(caps(w-ord.i-no))  FORM "x(15)" "~","
            "~""  removeChars(w-ord.cust-part-no) "~","
            "~""  removeChars(w-ord.i-name)  "~","
            "~""  removeChars(w-ord.part-dscr1)  "~","
            "~""  removeChars(w-ord.part-dscr2)  "~","
            "~""  removeChars(w-ord.part-dscr3)  "~","
            "~""  removeChars(w-ord.cust-po-no)  "~","
            iPcs ","
            w-ord.bundle ","
            w-ord.total-unit ","
            "~""  removeChars(w-ord.cust-name)  "~","
            "~""  removeChars(w-ord.cust-add1)  "~","
            "~""  removeChars(w-ord.cust-add2)  "~","
            "~""  removeChars(w-ord.cust-city)  "~","
            "~""  removeChars(w-ord.cust-state) "~","
            "~""  removeChars(w-ord.cust-zip)   "~","
            "~""  removeChars(w-ord.cust-ctry)  "~","
            "~""  removeChars(w-ord.sold-name)  "~","
            "~""  removeChars(w-ord.sold-add1)  "~","
            "~""  removeChars(w-ord.sold-add2)  "~","
            "~""  removeChars(w-ord.sold-city)  "~","
            "~""  removeChars(w-ord.sold-state) "~","
            "~""  removeChars(w-ord.sold-zip)   "~","
            "~""  removeChars(w-ord.sold-ctry)  "~","
            "~""  removeChars(w-ord.ship-name)  "~","
            "~""  removeChars(w-ord.ship-add1)  "~","
            "~""  removeChars(w-ord.ship-add2)  "~","
            "~""  removeChars(w-ord.ship-city)  "~","
            "~""  removeChars(w-ord.ship-state) "~","
            "~""  removeChars(w-ord.ship-zip)   "~","
            "~""  removeChars(w-ord.ship-ctry)  "~","
            "~""  w-ord.due-date  "~","
            "~""  w-ord.rel-date  "~","
            "~""  removeChars(w-ord.upc-no)  "~","
            "~""  w-ord.box-len  "~","
            "~""  w-ord.box-wid  "~","
            "~""  w-ord.box-dep  "~","
            "~""  removeChars(w-ord.flute)  "~","
            "~""  removeChars(w-ord.test)  "~","
            "~""  removeChars(w-ord.vendor)  "~","
            w-ord.gross-wt  ","
            w-ord.tare-wt  ","
            w-ord.net-wt  ","
            w-ord.sheet-wt  ","
            "~""  w-ord.uom  "~","
            "~""  lv-middlesex-job  "~","
            "~""  lv-middlesex-po  "~","              
            "~""  removeChars(lv-tag-no) /*FORM "x(23)"*/ "~"," 
            "~""  w-ord.partial "~","
            "~""  removeChars(w-ord.cas-no) "~","
            "~""  removeChars(w-ord.prod-notes) "~","
            "~""  removeChars(w-ord.l-code) "~","
            "~""  w-ord.case-wt FORMAT ">>>>9" "~","   
            "~""  removeChars(w-ord.lot#) "~","
            "~""  removeChars(w-ord.rel-lot#) "~","
            "~""  removeChars(w-ord.draw#) "~","
            w-ord.linenum ","
            w-ord.po-no ","
            "~""  v-frm-no  "~","
            "~""  v-blnk-no  "~"," 
            "~""  TODAY  "~","
            "~""  STRING(TIME,'hh:mm am')  "~","
               .
            put skip.
        end.
      end.
    end.
    output close.

    IF cBarCodeProgram EQ "" THEN
        RUN AutoPrint.

  end.    /* NOT TRIAD */

  RUN update-counts.
  IF cBarCodeProgram EQ "xprint" THEN do:
    {sys/inc/print1.i}

    {sys/inc/outprint.i value(85)}

    PUT "<PREVIEW>".  

    FOR EACH w-ord NO-LOCK BREAK 
                          BY w-ord.ord-no
                          BY w-ord.i-no :
       {oe/rep/case2xprnt.i}
       IF NOT LAST(w-ord.i-no) THEN PAGE .
    END.

     OUTPUT CLOSE.

     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.


SESSION:SET-WAIT-STATE ("").
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-create C-Win 
PROCEDURE temp-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  CREATE w-file.
  w-key = ip-rowid.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-job C-Win 
PROCEDURE temp-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-job-no LIKE job.job-no NO-UNDO.

  FOR EACH job
      WHERE job.company EQ cocode
        AND job.job-no  EQ SUBSTR(ip-job-no,1,6)
        AND job.job-no2 EQ INT(SUBSTR(ip-job-no,7,2))
      NO-LOCK:
    RUN temp-create (ROWID(job)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE temp-ord C-Win 
PROCEDURE temp-ord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-ord-no LIKE oe-ord.ord-no NO-UNDO.

  FOR EACH oe-ord
      WHERE oe-ord.company EQ cocode
        AND oe-ord.ord-no  EQ ip-ord-no
      NO-LOCK:
    RUN temp-create (ROWID(oe-ord)).

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-counts C-Win 
PROCEDURE update-counts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF tb_per-unit OR tb_per-pallet THEN
  FOR EACH w-ord,
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-ord.i-no:

     IF tb_per-unit THEN itemfg.case-count = w-ord.pcs.

     IF w-ord.ord-no NE 0 THEN
     FOR EACH oe-ordl
         WHERE oe-ordl.company EQ itemfg.company
           AND oe-ordl.ord-no  EQ w-ord.ord-no
           AND oe-ordl.i-no    EQ itemfg.i-no:
       IF tb_per-unit THEN oe-ordl.cas-cnt = w-ord.pcs.
     END.

     IF TRIM(w-ord.est-no) NE "" THEN
     FOR EACH eb
         WHERE eb.company  EQ itemfg.company
           AND eb.est-no   EQ w-ord.est-no
           AND eb.stock-no EQ itemfg.i-no:
       IF tb_per-unit THEN eb.cas-cnt = w-ord.pcs.
       IF tb_per-pallet THEN eb.cas-pal = w-ord.bundle.
       eb.tr-cnt = eb.cas-cnt * eb.cas-pal.
     END.
   END.
  RELEASE itemfg.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-release C-Win 
PROCEDURE check-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-stat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCheckRel AS LOGICAL NO-UNDO .
    IF AVAIL oe-ordl THEN
    FOR EACH oe-rel NO-LOCK
        WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
        USE-INDEX ord-item

        BREAK BY oe-rel.rel-no
              BY oe-rel.b-ord-no
              BY oe-rel.po-no
          TRANSACTION:

          IF LAST-OF(oe-rel.po-no) OR oe-rel.rel-no EQ 0 THEN DO:
              
              FIND FIRST oe-rell
                WHERE oe-rell.company  EQ oe-rel.company
                AND oe-rell.r-no     EQ oe-rel.link-no
                AND oe-rell.ord-no   EQ oe-rel.ord-no
                AND oe-rell.rel-no   EQ oe-rel.rel-no
                AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                AND oe-rell.i-no     EQ oe-rel.i-no
                AND oe-rell.line     EQ oe-rel.line
                AND oe-rell.po-no    EQ oe-rel.po-no
                AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
                USE-INDEX r-no NO-LOCK NO-ERROR.
            IF NOT AVAIL oe-rell THEN
                FIND FIRST oe-rell
                WHERE oe-rell.company  EQ oe-rel.company
                AND oe-rell.link-no  EQ oe-rel.r-no
                AND oe-rell.ord-no   EQ oe-rel.ord-no
                AND oe-rell.rel-no   EQ oe-rel.rel-no
                AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                AND oe-rell.i-no     EQ oe-rel.i-no
                AND oe-rell.line     EQ oe-rel.line
                AND oe-rell.po-no    EQ oe-rel.po-no
                AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
                NO-LOCK NO-ERROR.
            /* Needed because line was sometimes different between the two */
            IF NOT AVAIL oe-rell THEN
                FIND FIRST oe-rell
                WHERE oe-rell.company  EQ oe-rel.company
                AND oe-rell.ord-no   EQ oe-rel.ord-no              
                AND oe-rell.i-no     EQ oe-rel.i-no
                AND oe-rell.link-no  EQ oe-rel.r-no
                AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
                NO-LOCK NO-ERROR.

            IF AVAIL oe-rell THEN
                FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
                
 
            DO WITH FRAME {&FRAME-NAME}:
                IF AVAIL oe-relh AND oe-relh.release# NE 0 THEN do:
                    ASSIGN
                        begin_rel:SCREEN-VALUE   = STRING(oe-relh.release#)
                        begin_rel:SENSITIVE   = YES
                        lCheckRel = YES .
                    LEAVE .
                END.
            END.
          END.
    END.
    IF NOT lCheckRel THEN DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN
                begin_rel:SCREEN-VALUE   = ""
                begin_rel:SENSITIVE   = NO .
        END.
    END.
 

    END PROCEDURE.

    /* _UIB-CODE-BLOCK-END */
    &ANALYZE-RESUME


    /* ************************  Function Implementations ***************** */

    &ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION blockAccess C-Win 
    FUNCTION blockAccess RETURNS LOGICAL
      ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
DEF VAR llAllowed AS LOGICAL NO-UNDO.
DEF VAR llAccessClose AS LOG NO-UNDO.
DEF VAR lcAccessList AS CHARACTER NO-UNDO.

RUN methods/prgsecur.p
    (INPUT "OECsLbOp",
     INPUT "ALL", /* based on run, create, update, delete or all */
     INPUT NO,    /* use the directory in addition to the program */
     INPUT NO,    /* Show a message if not authorized */
     INPUT NO,    /* Group overrides user security? */
     OUTPUT llAllowed, /* Allowed? Yes/NO */
     OUTPUT llAccessClose, /* used in template/windows.i  */
     OUTPUT lcAccessList). /* list 1's and 0's indicating yes or no to run, create, update, delete */

RETURN NOT llAllowed.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
  (ipField AS CHARACTER) :
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

